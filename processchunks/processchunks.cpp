/*
 *  processchunks -- An application to process command files on multiple machines
 *
 *  Author: Sue Held
 *
 *  Copyright (C) 2010 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <processchunks.h>
#include <QTextStream>
#include <processhandler.h>
#include <parse_params.h>
#include "b3dutil.h"
#include <QDir>
#include <signal.h>
#include <QSet>

static const int arraySize = 2;
//Using a shorter sleep time then the processchunks script and not adjusting
//the sleep depending on the number of machines.
static const int sleepMillisec = 100;
static const int maxLocalByNum = 32;
static const int timeout = 30;
static const int numOptions = 13;

static char *commandName = "qtprocesschunks";
using namespace std;
/* Fallbacks from    ../manpages/autodoc2man 2 0 processchunks
 * cd manpages
 make autodoc2man
 */
static char
    *options[] = {
        ":r:B:Resume, retaining existing finished files (the default is to remove all log files and redo everything)",
        ":s:B:Run a single command file named root_name or root_name.com",
        ":g:B:Go process, without asking for confirmation after probing machines",
        ":n:I:Set the \"nice\" value to the given number (default 18, range 0-19)",
        ":w:FN:Full path to working directory on remote machines",
        ":d:I:Drop a machine after given number of failures in a row (default 5)",
        ":e:I:Quit after the given # of processing errors for a chunk (default 5)",
        ":c:FN:Check file \"name\" for commands P, Q, and D",
        ":q:I:Run on cluster queue with given maximum # of jobs at once",
        ":Q:CH:Machine name to use for the queue (default queue)",
        ":P:B:Output process ID", ":v:B:Print extra information",
        ":help:B:Print usage message" };
static char *queueNameDefault = "queue";
Processchunks *pcPointer;

int main(int argc, char **argv) {
  //Run processes
  Processchunks pc(argc, argv);
  pcPointer = &pc;
  pc.loadParams(argc, argv);
  pc.setup();
  if (!pc.askGo()) {
    return 0;
  }
  pc.runProcesses();
}

Processchunks::Processchunks(int &argc, char **argv) :
  QApplication(argc, argv) {
  //initialize member variables
  mOut = new QTextStream(stdout);
  mRemoteDir = NULL;
  mNice = 18;
  mDropCrit = 5;
  mMaxChunkErr = 5;
  mProcessFinishedCount = 0;
  mRetain = 0;
  mJustGo = 0;
  mReturnPid = 0;
  mSkipProbe = 0;
  mQueue = 0;
  mSingleFile = 0;
  mSshOpts = new QStringList();
  *mSshOpts << "-o PreferredAuthentications=publickey"
      << "-o StrictHostKeyChecking=no";
  mVersion = -1;
  mCpuList = NULL;
  mRootName = NULL;
  mQueueCom = NULL;
  mQueueName = queueNameDefault;
  mHostRoot = NULL;
  mCheckFile = NULL;
  mCopyLogIndex = -1;
  mRestarting = false;
  mMachineList = new QList<MachineHandler> ();
  //old codemNumSshOpts
  //mHandlers = new ProcessHandler[arraySize];
  //Handles need to know what process they are handling
  /* char *imodDir = getenv("IMOD_DIR");
   *mOut << "IMOD_DIR:" << imodDir << endl;
   int i = 0;
   mHandlers[i].init(this, imodDir, mOut, i);
   i++;
   mHandlers[i].init(this, imodDir, mOut, i);*/
}

Processchunks::~Processchunks() {
}

//Print usage statement
//Not implementing $IMOD_ALL_MACHINES since no one seems to have used it.
void processchunksUsageHeader(char *pname) {
  printf(
      "\nUsage: %s [Options] machine_list root_name\nWill process multiple command "
        "files on multiple processors or machines\nmachine_list is a list of "
        "available machines, separated by commas.\nList machines names multiple "
        "times to gain access to multiple CPUs on a machine.\nRoot_name is "
        "the base name of the command files, omitting -nnn.com\n\n", pname);
  imodUsageHeader(pname);
}

//Loads parameters, does error checking, returns zero if parameters are correct.
//Prints an error message to stdout and returns non-zero if the parameters are
//incorrect.
void Processchunks::loadParams(int &argc, char **argv) {
  int numOptArgs, numNonOptArgs;
  PipReadOrParseOptions(argc, argv, options, numOptions, commandName, 2, 2, 0,
      &numOptArgs, &numNonOptArgs, processchunksUsageHeader);
  PipGetBoolean("r", &mRetain);
  PipGetBoolean("s", &mSingleFile);
  PipGetBoolean("g", &mJustGo);
  PipGetInteger("n", &mNice);
  PipGetString("w", &mRemoteDir);
  PipGetInteger("d", &mDropCrit);
  PipGetInteger("e", &mMaxChunkErr);
  PipGetString("c", &mCheckFile);
  if (PipGetInteger("q", &mQueue)) {
    mSshExt = "ssh";
    mPidExt = "pid";
  }
  else {
    mSkipProbe = 1;
    mJustGo = 1;
    mSshExt = "job";
    mPidExt = "qid";
  }
  PipGetString("Q", &mQueueName);
  PipGetBoolean("P", &mReturnPid);
  if (mReturnPid) {
    mSkipProbe = 1;
    fprintf(stderr, "Shell PID: %d\n", imodGetpid());
    fflush(stderr);
  }
  PipGetNonOptionArg(0, &mCpuList);
  PipGetNonOptionArg(1, &mRootName);
  PipGetBoolean("v", &mVerbose);
  //Error check
  if (mRetain && mSingleFile) {
    exitError("You cannot use the retain option with a single command file");
  }
}

//Setup mSshOpts, mCpuArray, mComFileArray, mHostRoot, mRemoteDir.  Probe
//machines.
void Processchunks::setup() {
  int i;
  //Change mSshOpts if version is recent enough
  QProcess ssh(this);
  QString command("ssh");
  QStringList params("-V");
  ssh.start(command, params);
  if (ssh.waitForFinished()) {
    mVersion = -1;
    extractVersion(ssh.readAllStandardError());
    if (mVersion == -1) {
      extractVersion(ssh.readAllStandardOutput());
    }
    //Check if version if >= to ssh version 3.9
    if (mVersion >= 309) {
      mSshOpts->prepend("-o ConnectTimeout=5 ");
    }
  }
  if (mVerbose) {
    *mOut << "mSshOpts:" << endl;
    for (i = 0; i < mSshOpts->size(); i++) {
      *mOut << mSshOpts->at(i) << endl;
    }
  }

  //Setup cpuArray with the queue name or the values in mCpuList.
  //Not implementing $IMOD_ALL_MACHINES since no one seems to have used it.
  MachineHandler *machine;
  if (mQueue) {
    //For a queue, make a CPU list that is all the same name
    mQueueCom = mCpuList;
    machine = new MachineHandler(mQueueName, mQueue);
    mMachineList->append(*machine);
  }
  else {
    //Setup up machine names from mCpuList
    QString temp(mCpuList);
    QStringList cpuArray = temp.split(",", QString::SkipEmptyParts);
    //Now handling mixed up names (as in bear,bebop,bear) without extra probes.
    //Processes will now be run based on the first time a machine appears in
    //the list.
    //MachineSet temporarily keeps track of machines.
    QSet<QString> machineSet;
    for (i = 0; i < cpuArray.size(); i) {
      QString cpuMachine = cpuArray.at(i);
      if (!machineSet.contains(cpuMachine)) {
        machineSet.insert(cpuMachine);
        machine = new MachineHandler(cpuMachine);
        mMachineList->append(*machine);
      }
      else {
        //Increment the number of CPUs in an existing machine.
        machine = &mMachineList->last();
        if (*machine == cpuMachine) {
          machine->incrementNumCpus();
        }
        else {
          //Machine names are mixed up (as in bear,bebop,bear).
          for (int j = 0; j < mMachineList->size(); j++) {
            machine = &(*mMachineList)[j];
            if (*machine == cpuMachine) {
              machine->incrementNumCpus();
              break;
            }
          }
        }
      }
    }
  }
  int size = mMachineList->size();
  if (size < 1) {
    exitError("No machines specified");
  }
  //Translate a single number into a list of localhost entries
  if (size == 1) {
    bool ok;
    machine = &(*mMachineList)[0];
    long localByNum = machine->nameToLong(&ok);
    if (ok) {
      if (localByNum > maxLocalByNum) {
        exitError("You cannot run more than %d chunks on localhost by "
          "entering a number", maxLocalByNum);
      }
      machine->setValues("localhost", localByNum);
    }
  }

  if (mVerbose) {
    *mOut << "mMachineList:" << endl;
  }
  //Setup machines.  Do this only once after the number of CPUs is finalized.
  for (i = 0; i < mMachineList->size(); i++) {
    machine = &(*mMachineList)[i];
    machine->setup();
    if (mVerbose) {
      *mOut << i << ":" << machine->getName() << endl;
    }
  }
  if (mVerbose) {
    *mOut << endl;
  }

  //Sets up mProcessArray for single file or multi-file processing
  QStringList comFileArray;
  if (mVerbose) {
    *mOut << "current path:" << QDir::currentPath() << endl;
  }
  QDir dir(".");
  //Make the list for a single file
  if (mSingleFile) {
    QString rootName(mRootName);
    int extIndex = rootName.lastIndexOf(".");
    if (extIndex != -1) {
      rootName = rootName.mid(0, extIndex);
    }
    rootName.append(".com");
    if (!dir.exists(rootName)) {
      exitError("The single command file %s does not exist",
          rootName.toAscii().data());
    }
    comFileArray.append(rootName);
  }
  else {
    //Build up lists in order -nnn, -nnnn, -nnnnn*, which should work both for
    //lists that are all 5 digits or lists that are 3, 4, 5 digits
    //Put -start.com on front and -finish.com on end
    //Add start com file
    QString startComFile(mRootName);
    startComFile.append("-start.com");
    if (dir.exists(startComFile)) {
      comFileArray.append(startComFile);
    }
    //Add numeric com files
    dir.setSorting(QDir::Name);
    dir.setFilter(QDir::Files);
    //Add -nnn com files
    QStringList filters;
    //?- is a special character and gets a warning about -trigraphs
    buildFilters("-???.com", "-??\?-sync.com", filters);
    QStringList list = dir.entryList(filters);
    cleanupList("-\\D{3,3}(-sync){0,1}\\.com", list);
    comFileArray += list;
    //add -nnnn com files
    filters.clear();
    list.clear();
    buildFilters("-????.com", "-???\?-sync.com", filters);
    list = dir.entryList(filters);
    cleanupList("-\\D{4,4}(-sync){0,1}\\.com", list);
    comFileArray += list;
    //add -nnnnn com files
    filters.clear();
    list.clear();
    buildFilters("-?????.com", "-????\?-sync.com", filters);
    list = dir.entryList(filters);
    cleanupList("-\\D{5,5}(-sync){0,1}\\.com", list);
    comFileArray += list;
    //Add finish com file
    QString finishComFile(mRootName);
    finishComFile.append("-finish.com");
    if (dir.exists(finishComFile)) {
      comFileArray.append(finishComFile);
    }
  }
  if (mVerbose) {
    *mOut << "mComFileArray:" << endl;
    for (i = 0; i < comFileArray.size(); i++) {
      *mOut << i << ":" << comFileArray.at(i) << endl;
    }
    *mOut << endl;
  }
  if (comFileArray.isEmpty()) {
    exitError("There are no command files matching %s-nnn.com", mRootName);
  }
  //Build mProcessArray from comFileArray.
  //set up flag list and set up which chunk to copy the
  //log from, the first non-sync if any, otherwise just the first one
  mSizeProcessArray = comFileArray.size();
  mProcessArray = new ProcessHandler[mSizeProcessArray];
  for (i = 0; i < mSizeProcessArray; i++) {
    if (!mProcessArray[i].setup(mSingleFile, mVerbose, comFileArray.at(i))
        && mCopyLogIndex == -1) {
      //Setting mCopyLogIndex to the first non-sync log
      mCopyLogIndex = i;
    }
  }

  //Setup mHostRoot
  QProcess hostname(this);
  command.clear();
  command.append("hostname");
  hostname.start(command);
  if (hostname.waitForFinished()) {
    QString temp(hostname.readAllStandardOutput());
    int i = temp.indexOf(".");
    if (i != -1) {
      mHostRoot = new QString(temp.mid(0, i));
    }
    else {
      mHostRoot = new QString(temp);
    }
    if (mVerbose) {
      *mOut << "mHostRoot:" << *mHostRoot << endl;
    }
  }
  else {
    exitError("Unable to run the hostname command");
  }

  //Get current directory if the -w option was not used
  if (mRemoteDir == NULL) {
    QProcess pwd(this);
    command.clear();
    command.append("pwd");
    pwd.start(command);
    if (pwd.waitForFinished()) {
      QString temp(pwd.readAllStandardOutput());
      if (!temp.isEmpty()) {
        mRemoteDir = temp.toAscii().data();
      }
    }
    else {
      exitError("Unable to run the pwd command");
    }
  }

  //Probe machines by running the "w" command.  Drop machines that don't respond.
  //probe machines and get all the verifications unless etomo is running it
  if (!mSkipProbe || !mJustGo) {
    //Remove the old checkfile
    if (mCheckFile != NULL) {
      QDir dir(".");
      if (dir.exists(mCheckFile)) {
        dir.remove(mCheckFile);
      }
    }
    *mOut << "Probing machine connections and loads..." << endl;
    QProcess w(this);
    QString localCommand("w");
    QStringList localParams;
    QString remoteCommand("ssh");
    QStringList remoteParams("-x");
    for (i = 0; i < mSshOpts->size(); i++) {
      remoteParams.append(mSshOpts->at(i));
    }
    remoteParams << "dummy" << "hostname ; w";
    //Probing the machines and building a new cpu array from the ones that
    //respond.
    bool status = -1;
    i = 0;
    while (i < mMachineList->size()) {
      QString machName = (*mMachineList)[i].getName();
      if (machName == mHostRoot || machName == "localhost") {
        *mOut << machName << endl;
        status = runProcessAndOutputLines(w, localCommand, localParams, 1);
      }
      else {
        remoteParams.replace(mSshOpts->size() + 1, machName);
        status = runProcessAndOutputLines(w, remoteCommand, remoteParams, 2);
      }
      //status can also be set to 1 on the local machine if it times out.
      //No longer testing for 141 before no longer supporting SGI
      if (status != 0) {
        *mOut << "Dropping " << machName
            << " from list because it does not respond" << endl << endl;
        if (mVerbose) {
          *mOut << "status:" << status << endl;
        }
        //Drops failed machine from the machine list
        mMachineList->removeAt(i);
      }
      else {
        i++;
      }
    }
    if (mVerbose) {
      *mOut << "end probe machines" << endl;
    }
  }
}

//Handle ctrl-C
void handleSigInt(int signal) {
  if (signal != SIGINT) {
    return;
  }
  QTextStream out(stdout);
  out << "SIGINT detected" << endl;
  QApplication::quit();
}

//Setup mFlags.  Find first not-done log file.  Delete log files and
//miscellaneous files.  Listen for ctrl-C.  Run event loop.
void Processchunks::runProcesses() {/*
 QStringList params[2];
 //Start process 0
 int index = 0;
 params[index] << "-x" << "bear" << "bash" << "--login" << "-c"
 << "\"cd 'current data/UITests/dual-montage' && pwd && submfg tilta.com\"";
 mHandlers[index].setParams(params[index]);
 mHandlers[index].runProcess();
 //Start process 1
 index++;
 params[index] << "-x" << "bear" << "bash" << "--login" << "-c"
 << "\"cd 'current data/UITests/dual-montage' && pwd && submfg tiltb.com\"";
 mHandlers[index].setParams(params[index]);
 mHandlers[index].runProcess();
 //Error messages from inside the event loop must using QApplication functionality
 //Start loop and timer

 */

  int i;
  if (mRestarting) {
    //reset the flags and list of assignments and set up which chunk to copy the
    //log from, the first non-sync if any, otherwise just the first one
    for (i = 0; i < mSizeProcessArray; i++) {
      mProcessArray[i].reset();
    }
    //reset flags associated with the machines and CPUs
    MachineHandler *machine;
    for (i = 0; i < mMachineList->size(); i++) {
      machine = &(*mMachineList)[i];
      machine->reset();
    }
  }

  //Prescan logs for done ones to find first undone one, or back up unfinished
  int numDone = 0;
  int firstUndoneIndex = -1;
  for (i = 0; i < mSizeProcessArray; i++) {
    ProcessHandler *process = &mProcessArray[i];
    if (process->logExists()) {
      if (process->isChunkDone()) {
        //If it was done and we are resuming, set flag it is done, count
        if (mRetain) {
          process->setFlag(ProcessHandler::done);
          numDone++;
        }
      }
      else if (!mRetain) {
        //If it was not done and we are restarting, back up the old log
        process->backupLog();
      }
    }
    //If resuming and this is the first undone one, keep track of that
    if (mRetain && firstUndoneIndex == -1 && !process->flagEquals(
        ProcessHandler::done)) {
      firstUndoneIndex = i;
    }
  }
  if (firstUndoneIndex == -1) {
    firstUndoneIndex = 0;
  }

  //remove logs if not restarting
  if (!mRetain) {
    QProcess find(this);
    QString command("find");
    QStringList params;
    params << "." << "-maxdepth" << "1" << "-type" << "f" << "-name" << "dummy"
        << "-exec" << "rm" << "-f" << "{}";
    QString pattern;
    pattern.arg("%1-[0-9][0-9][0-9]*.log", mRootName);
    params.replace(6, pattern);
    find.start(command, params);
    find.waitForFinished();
    pattern.arg("%1-start.log", mRootName);
    params.replace(6, pattern);
    find.start(command, params);
    find.waitForFinished();
    pattern.arg("%1-finish.log", mRootName);
    params.replace(6, pattern);
    find.start(command, params);
    find.waitForFinished();
    //Clean up .ssh and .pid files to avoid confusion and .csh on general principle
    pattern.arg("%1-[0-9][0-9][0-9]*.%2", mRootName, mSshExt);
    params.replace(6, pattern);
    find.start(command, params);
    find.waitForFinished();
    pattern.arg("%1-[0-9][0-9][0-9]*.%2", mRootName, mPidExt);
    params.replace(6, pattern);
    find.start(command, params);
    find.waitForFinished();
    pattern.arg("%1-[0-9][0-9][0-9]*.csh", mRootName);
    params.replace(6, pattern);
    find.start(command, params);
    find.waitForFinished();
  }

  if (!mRestarting) {
    //allow users to send commands to process by type ctrl-C
    signal(SIGINT, handleSigInt);
  }

  startTimer(100);
  //Error messages from inside the event loop must using QApplication functionality
  PipDone();
  exec();
}

bool Processchunks::askGo() {
  if (mJustGo) {
    return true;
  }
  *mOut << "Enter Y to proceed with the current set of machines: ";
  mOut->flush();
  char answer;
  QTextStream in(stdin);
  in >> answer;
  if (answer == 'Y' || answer == 'y') {
    return true;
  }
  return false;
}

//Extracts the first two numbers of a numeric version.  Multiples the first
//number by 100 and adds it to the second number.  Places the result in
//mVersion.
void Processchunks::extractVersion(QString versionString) {
  if (mVerbose) {
    *mOut << "ssh sshOutput:" << versionString << endl;
  }
  QRegExp regExp("[0-9]+\\.[0-9]+");
  int i = regExp.indexIn(versionString, 0);
  int len = regExp.matchedLength();
  if (i != -1 && len != -1) {
    QString version = versionString.mid(i, len);
    if (version != NULL) {
      QStringList array = version.split(".", QString::SkipEmptyParts);
      if (!array.isEmpty()) {
        bool ok;
        mVersion = array.at(0).toLong(&ok) * 100;
        if (!ok) {
          mVersion = -1;
          return;
        }
        if (array.size() > 1) {
          mVersion += array.at(1).toLong(&ok);
          if (!ok) {
            mVersion = -1;
          }
        }
      }
    }
  }
  if (mVerbose) {
    *mOut << "ssh version:" << mVersion << endl;
  }
}

void Processchunks::buildFilters(char *reg, char *sync, QStringList &filters) {
  int i;
  QString filter1(mRootName);
  filter1.append(reg);
  filters.append(filter1);
  QString filter2(mRootName);
  filter2.append(sync);
  filters.append(filter2);
  if (mVerbose) {
    *mOut << "filters:" << endl;
    for (i = 0; i < filters.size(); i++) {
      *mOut << i << ":" << filters.at(i) << endl;
    }
    *mOut << endl;
  }
}

void Processchunks::cleanupList(char *remove, QStringList &list) {
  int i;
  //Remove files that don't have digits after rootname-
  QString regExp(mRootName);
  regExp.append(remove);
  while ((i = list.indexOf(QRegExp(regExp))) != -1) {
    list.removeAt(i);
  }
}

//Runs process, outputs first line, and returns the exit code
//lineNum - line number (from 1)
int Processchunks::runProcessAndOutputLines(QProcess &process,
    QString &command, QStringList &params, int numLines) {
  int i;
  if (mVerbose) {
    *mOut << command << " ";
    for (i = 0; i < params.size(); ++i) {
      *mOut << params.at(i) << " ";
    }
    *mOut << endl;
  }
  process.start(command, params);
  if (process.waitForFinished()) {
    QByteArray output = process.readAllStandardOutput();
    if (mVerbose) {
      QByteArray errArray = process.readAllStandardError();
      if (!errArray.isEmpty()) {
        *mOut << "stderr:" << errArray << endl;
      }
    }
    //Output first lines up to numLines
    int startIndex = 0;
    int endIndex = -1;
    int temp;
    for (i = 0; i < numLines; i++) {
      temp = endIndex;
      endIndex = output.indexOf('\n', endIndex + 1);
      startIndex = temp + 1;
      if (endIndex == -1) {
        //No more lines
        *mOut << output;
        break;
      }
      else {
        *mOut << output.mid(startIndex, endIndex + 1);
      }
    }
    return process.exitCode();
  }
  return 1;
}

void Processchunks::timerEvent(QTimerEvent *e) {
}

void Processchunks::msgProcessFinished() {
  mProcessFinishedCount++;
  if (mProcessFinishedCount >= arraySize) {
    quit();
  }
}
/*
 $Log$
 Revision 1.2  2010/06/23 20:17:27  sueh
 bug# 1364 Moved most setup functionality into one function.  Changed
 QTprocesschunks to Processchunks.

 Revision 1.1  2010/06/23 16:22:33  sueh
 bug# 1364 First checkin for QT version of processchunks.

 */
