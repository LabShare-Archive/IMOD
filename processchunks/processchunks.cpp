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
#include <QTimer>
#include <stdio.h>

static const int arraySize = 2;
//Using a shorter sleep time then the processchunks script and not adjusting
//the sleep depending on the number of machines.
static const int sleepMillisec = 100;
static const int maxLocalByNum = 32;
//converting old timeout counter to milliseconds
static const int runProcessTimeout = 30 * 2 * 1000;
static const int numOptions = 13;

static char *commandName = "processchunks";
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
        ":P:B:Output process ID",
        ":v:B:Print extra information.",
        ":V:CH:Syntax:  class,function,...,verbosity.  All elements are optional .  Which class and functions(s) should print extra information.  Verbosity (how much information to print): 1:normal, 2: a lot.  Can be limited to a class and function(s).  Case insensitive.  No effect if '-v' is not used.",
        ":help:B:Print usage message" };
static char *queueNameDefault = "queue";
Processchunks *processchunksInstance;

int main(int argc, char **argv) {
  //Run processes
  Processchunks pc(argc, argv);
  processchunksInstance = &pc;
  pc.loadParams(argc, argv);
  pc.setup();
  if (!pc.askGo()) {
    return 0;
  }
  pc.startLoop();
}

Processchunks::Processchunks(int &argc, char **argv) :
  QApplication(argc, argv) {
  //initialize member variables
  mOutStream = new QTextStream(stdout);
  mRemoteDir = NULL;
  mNice = 18;
  mDropCrit = 5;
  mMaxChunkErr = 5;
  mRetain = 0;
  mVerbose = 0;
  mJustGo = 0;
  mSkipProbe = false;
  mQueue = 0;
  mSingleFile = 0;
  mSshOpts.append("-o PreferredAuthentications=publickey");
  mSshOpts.append("-o StrictHostKeyChecking=no");
  mRootName = NULL;
  mQueueName = queueNameDefault;
  mCheckFile = NULL;
  mCopyLogIndex = -1;
  mAns = ' ';
  mTimerId = 0;
  mKillProcessMachineIndex = -1;
  mAllKillProcessesHaveStarted = false;
  mKill = false;
  mKillCounter = 0;
  mLsProcess = new QProcess(this);
  mInterrupt = false;
  mDecoratedClassName = typeid(*this).name();
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
  char *remoteDir = NULL;
  PipGetString("w", &remoteDir);
  if (remoteDir != NULL) {
    mRemoteDir = new QString(remoteDir);
  }
  PipGetInteger("d", &mDropCrit);
  PipGetInteger("e", &mMaxChunkErr);
  char *checkFile = NULL;
  PipGetString("c", &checkFile);
  if (checkFile != NULL) {
    mCheckFile = new QFile(checkFile);
  }
  PipGetBoolean("v", &mVerbose);
  if (mVerbose) {
    char *verboseClassFunctions = NULL;
    PipGetString("V", &verboseClassFunctions);
    if (verboseClassFunctions != NULL) {
      QString param(verboseClassFunctions);
      QStringList paramList = param.trimmed().split(",",
          QString::SkipEmptyParts);
      if (!paramList.isEmpty()) {
        bool ok;
        int temp = paramList.at(paramList.size() - 1).toInt(&ok);
        if (ok) {
          mVerbose = temp;
          paramList.removeAt(paramList.size() - 1);
        }
      }
      if (!paramList.isEmpty()) {
        mVerboseClass = paramList.at(0);
        paramList.removeAt(0);
      }
      if (!paramList.isEmpty()) {
        mVerboseFunctionList = paramList;
      }
    }
  }
  if (!PipGetInteger("q", &mQueue)) {
    mSkipProbe = true;
    mJustGo = 1;
  }
  PipGetString("Q", &mQueueName);
  int returnPid = 0;
  PipGetBoolean("P", &returnPid);
  if (returnPid) {
    mSkipProbe = true;
    fprintf(stderr, "Shell PID: %d\n", imodGetpid());
    fflush(stderr);
  }
  char *cpuList = NULL;
  PipGetNonOptionArg(0, &cpuList);
  if (cpuList != NULL) {
    mCpuList = cpuList;
  }
  PipGetNonOptionArg(1, &mRootName);
  //Error check
  if (mRetain && mSingleFile) {
    exitError("You cannot use the retain option with a single command file");
  }
}

//Setup mSshOpts, mCpuArray, mProcessArray, mHostRoot, mRemoteDir.  Probe
//machines.
void Processchunks::setup() {
  //Get current directory if the -w option was not used
  if (mRemoteDir == NULL) {
    mRemoteDir = new QString(mCurrentDir.absolutePath().toLatin1().data());
  }
  setupSshOpts();
  setupMachineList();
  setupHostRoot();
  setupEnvironment();
  setupProcessArray();
  probeMachines();
}

//Setup mFlags.  Find first not-done log file.  Delete log files and
//miscellaneous files.  Listen for ctrl-C.  Run event loop.
void Processchunks::startLoop() {
  int i;
  //Prescan logs for done ones to find first undone one, or back up unfinished
  mNumDone = 0;
  mFirstUndoneIndex = -1;
  for (i = 0; i < mSizeProcessArray; i++) {
    ProcessHandler *process = &mProcessArray[i];
    if (process->logFileExists(false)) {
      if (process->isChunkDone()) {
        //If it was done and we are resuming, set flag it is done, count
        if (mRetain) {
          process->setFlag(ProcessHandler::done);
          mNumDone++;
        }
      }
      else if (!mRetain) {
        //If it was not done and we are restarting, back up the old log
        process->backupLog();
      }
    }
    //If resuming and this is the first undone one, keep track of that
    if (mRetain && mFirstUndoneIndex == -1 && process->getFlag()
        != ProcessHandler::done) {
      mFirstUndoneIndex = i;
    }
  }
  if (mFirstUndoneIndex == -1) {
    mFirstUndoneIndex = 0;
  }

  //remove logs if not restarting
  if (!mRetain) {
    for (i = 0; i < mSizeProcessArray; i++) {
      mProcessArray[i].removeFiles();
    }
  }

  if (!mSingleFile || mSkipProbe) {
    *mOutStream << mNumDone << " OF " << mSizeProcessArray << " DONE SO FAR "
        << endl;
  }
  //Initialize variables needed by the timer event function
  mLastNumDone = 0;
  mPausing = false;
  mSyncing = 0;
  mAnyDone = false;
  mNextSyncIndex = mSizeProcessArray + 2 - 1;
  mHoldCrit = (mNumCpus + 1) / 2;
  //Error messages from inside the event loop must using QApplication functionality
  PipDone();
  startTimers();
  signal(SIGINT, SIG_IGN);
  exec();
}

void Processchunks::startTimers() {
  //Make sure there isn't already a timer going
  if (mTimerId != 0) {
    killTimer(mTimerId);
    mTimerId = 0;
  }
  //The timer event function should be called immediately and then put on a timer
  QTimer::singleShot(0, this, SLOT(timerEvent()));
  if (mQueue) {
    //Must look at files instead of stdout/err.  Prevent program from being a hog.
    mTimerId = startTimer((2 + mNumCpus / 100) * 1000);
  }
  else {
    mTimerId = startTimer(100);
  }
}

//Single shot timer slot
void Processchunks::timerEvent() {
  timerEvent(NULL);
}

void Processchunks::setInterrupt() {
  mInterrupt = true;
}

void Processchunks::timerEvent(QTimerEvent *timerEvent) {
  if (mInterrupt) {
    handleInterrupt();
  }
  if (mKill) {
    //Handle the kill timer.
    //This is a timeout if 15 seconds have gone by.  For a queue this means that
    //this is the 15th timerEvent triggered by the kill timer
    //For a non-queue this happens the first time the kill timer goes off.
    if (mQueue) {
      mKillCounter++;
      checkQueueProcessesDone(mKillCounter >= 15);
    }
    else {
      cleanupKillProcesses(!mQueue);
    }
    return;
  }
  //Handle the regular timer.
  int i, cpuIndex;
  bool endTimerEvent = false;
  while (!endTimerEvent) {
    if (mNumDone >= mSizeProcessArray) {
      cleanupAndExit();
    }
    if (readCheckFile()) {
      return;
    }
    //Count failures and assignments
    int assignTot = 0;
    int failTot = 0;
    int minFail = mDropCrit;
    int failCount = 0;
    int chunkErrTot = 0;
    MachineHandler *machine = NULL;
    int numCpus = 0;
    bool noChunks = false;
    for (i = 0; i < mMachineList.size(); i++) {
      machine = &(mMachineList)[i];
      failCount = machine->getFailureCount();
      if (failCount != 0) {
        failTot++;
      }
      if (failCount < minFail) {
        minFail = failCount;
      }
      if (machine->isChunkErred()) {
        chunkErrTot++;
      }
      numCpus = machine->getNumCpus();
      for (cpuIndex = 0; cpuIndex < numCpus; cpuIndex++) {
        if (machine->getAssignedProcIndex(cpuIndex) != -1) {
          assignTot++;
        }
      }
    }
    exitIfDropped(minFail, failTot, assignTot);

    //Loop ondummy CPUs, if they have an assignment check if it is done
    i = -1;
    bool loopDone = false;
    while (++i < mMachineList.size() && !loopDone) {
      machine = &(mMachineList)[i];
      numCpus = machine->getNumCpus();
      for (cpuIndex = 0; cpuIndex < numCpus; cpuIndex++) {
        int processIndex = machine->getAssignedProcIndex(cpuIndex);
        bool dropout = false;
        if (processIndex != -1) {
          QString dropMess;
          QString checkPid;
          QString errorMess;
          if (mProcessArray[processIndex].isComProcessDone()) {
            //Handle the comscript ran and finished
            // If the log is present and the process's finished signal has been caught
            if (mProcessArray[processIndex].isChunkDone()) {
              //If mSingleFile is true, set loopDone to end outer loop, and break
              //out of inner loop.
              loopDone = handleChunkDone(machine, cpuIndex, processIndex);
              if (loopDone) {
                break;
              }
            }
            else {
              //otherwise set flag to redo it
              dropout = true;
              if (!mProcessArray[processIndex].isLogFileEmpty()) {
                if (!handleLogFileError(errorMess, machine, cpuIndex,
                    processIndex)) {
                  return;
                }
              }
              else if (!mQueue) {
                //If log is zero length, check for something in .pid
                checkPid = mProcessArray[processIndex].getPid();
              }
            }
          }
          else {
            handleComProcessNotDone(dropout, dropMess, machine, processIndex);
          }
          //if failed, remove the assignment, mark chunk as to be done,
          //skip this machine on this round
          if (dropout) {
            handleDropOut(noChunks, dropMess, machine, cpuIndex, processIndex,
                errorMess);
          }
          //Clean up .job and .qid if no longer assigned
          if (machine->getAssignedProcIndex(cpuIndex) == -1 && mQueue) {
            mProcessArray[processIndex].removeProcessFiles();
          }
        }
        //Drop a machine if it has failed more than given number of times
        //Institute hold on any failed machine if no chunks are done and
        //machine failure count is above criterion
        int failCount = machine->getFailureCount();
        if (failCount >= mDropCrit || mPausing || (failCount && !mAnyDone
            && failTot >= mHoldCrit)) {
          if (isVerbose(mDecoratedClassName, __func__, 2)) {
            *mOutStream << machine->getName() << ":set dropout, failCount:"
                << failCount << ",failTot:" << failTot << endl;
          }
          dropout = true;
        }
        //If the current machine is unassigned, find next com to do and run it
        //Move current log out of way so non-existence of log can be sign of
        //nothing having started.  Skip if no chunks are available
        if (machine->getAssignedProcIndex(cpuIndex) == -1 && !dropout
            && !noChunks && mSyncing != 2) {
          processIndex = mFirstUndoneIndex;
          bool foundChunks = false;
          int undoneIndex = -1;
          while (processIndex < mSizeProcessArray
              && machine->getAssignedProcIndex(cpuIndex) == -1) {
            int runFlag;
            bool chunkOk;
            if (!checkChunk(runFlag, noChunks, undoneIndex, foundChunks,
                chunkOk, machine, processIndex, chunkErrTot)) {
              break;
            }
            if ((runFlag == ProcessHandler::sync || runFlag
                == ProcessHandler::notDone) && chunkOk) {
              runProcess(machine, cpuIndex, processIndex);
            }
            processIndex++;
          }
          //If no chunks were found in that loop set the nochunks flag
          if (!foundChunks) {
            noChunks = true;
          }
          if (undoneIndex > mFirstUndoneIndex) {
            mFirstUndoneIndex = undoneIndex;
          }
        }
      }
    }
    if (mNumDone > mLastNumDone) {
      *mOutStream << mNumDone << " OF " << mSizeProcessArray << " DONE SO FAR"
          << endl;
    }
    mLastNumDone = mNumDone;
    //If we have finished up to the sync file, then allow the loop to run it
    if (mNumDone - 1 >= mNextSyncIndex - 1) {
      QString endComName = QString("%1.com").arg(mRootName);
      if (mProcessArray[mNextSyncIndex].getComFileName() == endComName) {
        *mOutStream << "ALL DONE - going to run " << endComName
            << " to reassemble" << endl;
      }
      //Set syncing flag to 1 to get it started
      mSyncing = 1;
      mFirstUndoneIndex = mNextSyncIndex;
      mNextSyncIndex = mSizeProcessArray + 2 - 1;
      noChunks = false;
    }
    else {
      endTimerEvent = true;
    }
  }
  if (mSingleFile && mNumDone > 0) {
    cleanupAndExit();
  }
}

void Processchunks::cleanupAndExit(int exitCode) {
  if (mTimerId != 0) {
    killTimer(mTimerId);
    mTimerId = 0;
  }
  if (exitCode == 0) {
    //Etomo is looking for "to reassemble"
    QString endComName = QString("%1.com").arg(mRootName);
    if (!mCurrentDir.exists(endComName)) {
      *mOutStream << "ALL DONE - nothing to reassemble" << endl;
    }
    //Etomo is looking for this line too
    *mOutStream << "Finished reassembling" << endl;
  }
  if (mCheckFile != NULL) {
    mCheckFile->close();
    if (exitCode == 0 && mCurrentDir.exists(mCheckFile->fileName())) {
      mCurrentDir.remove(mCheckFile->fileName());
    }
  }
  *mOutStream << "exitCode:" << exitCode << endl;
  exit(exitCode);
}

void Processchunks::handleInterrupt() {
  mInterrupt = false;
  *mOutStream << mSizeProcessArray - mNumDone << " chunks are still undone"
      << endl;
  QString command;
  while (mAns != 'Q' && mAns != 'C' && mAns != 'P' && mAns != 'D') {
    mAns = ' ';
    command.clear();
    *mOutStream
        << "Enter Q to kill all jobs and quit, P to finish running jobs then exit,"
        << endl;
    *mOutStream << " D machine_list to kill jobs and drop given machines,"
        << endl;
    *mOutStream << " or C to continue waiting: " << endl;
    mOutStream->flush();
    QTextStream inStream(stdin);
    inStream >> command;
    command = command.trimmed().toUpper();
    mAns = command.at(0).toLatin1();
    if (mAns == 'D') {
      inStream >> command;
      command = command.trimmed();
      if (command.isEmpty()) {
        *mOutStream << endl << "Entry error: missing machine list" << endl;
        mAns = ' ';
      }
    }
  }
  if (mAns == 'D') {
    QStringList dropList = command.split(",", QString::SkipEmptyParts);
    killProcesses(&dropList);
  }
  else {
    killProcesses();
  }
}

//Handle mAns:  killing, pausing, dropping machines, and exiting as required
void Processchunks::killProcesses(QStringList *dropList) {
  if (dropList != NULL) {
    mDropList = *dropList;
  }
  mPausing = false;
  if (mAns == 'P') {
    mPausing = true;
  }
  if (mAns == 'C' || (mAns == 'P' && !mQueue)) {
    mDropList.clear();
    mAns = ' ';
    //Continue with timer loop
    return;
  }
  mKill = true;
  killTimer(mTimerId);
  //Slow down the timer for killing
  if (!mQueue) {
    //Start a time out for waiting for kill processes to finish
    mTimerId = startTimer(15 * 1000);
  }
  else {
    //for queue set timer to 1 sec intervals
    mTimerId = startTimer(1000);
  }
  killProcessOnNextMachines();
}

//Tells machines to send kill requests.  Stops when a machine has to give up
//control to the event loop.  If there are no more machines, starts
//cleaning up.
//This is called the first time by Processchunks::killProcesses.
//If that call can't get though all the machines because of a signal or event
//wait, it is called by MachineHandler::killNextProcess.
void Processchunks::killProcessOnNextMachines() {
  if (!mKill) {
    *mOutStream
        << "Warning: Processchunks::killProcessOnNextMachine called when mKill is false"
        << endl;
    return;
  }
  //Go to next machine and start killing its running processes.
  mKillProcessMachineIndex++;
  if (mKillProcessMachineIndex < mMachineList.size()) {
    while (mKillProcessMachineIndex < mMachineList.size()) {
      if (!mMachineList[mKillProcessMachineIndex].killProcesses()) {
        //Can't go on to the next machine because the current machine has to wait
        //for a signal or event.
        return;
      }
      mKillProcessMachineIndex++;
    }
  }
  //Clean up when all machines are completed
  if (mKillProcessMachineIndex >= mMachineList.size()
      && !mAllKillProcessesHaveStarted) {
    mAllKillProcessesHaveStarted = true;
  }
}

//Keeps track of the processes are being killed so that it will be able to
//wait for processes to be killed after all the kill commands have been sent out,
//and send a timeout message to the processes that haven't completed their kill
//command.
void Processchunks::msgKillProcessStarted(const int processIndex) {
  mProcessesWithUnfinishedKillRequest.append(processIndex);
}

//Removes the processIndex from the list of unfinished kill requests.  Calls
//clean up if all kill requests have been sent and the list of unfinshed kill
//requests is empty.
//
//When no kill request was sent, this call originates in
//ProcessHandler::continueKillProcess.
//
//When a non-queue kill request was sent, this call originates in the slot
//ProcessHandler::handleFinished.
//
//When a queue kill request was sent, this call originates in
//Processchunks::checkQueueProcessesDone (called by timerEvent).
void Processchunks::msgKillProcessDone(const int processIndex) {
  mProcessesWithUnfinishedKillRequest.removeOne(processIndex);
  if (mAllKillProcessesHaveStarted
      && mProcessesWithUnfinishedKillRequest.isEmpty()) {
    cleanupKillProcesses(false);
  }
}

//Removes process indexes of finished queued chunks from
//mProcessesWithUnfinishedKillRequest.
//Calls clean up function if every kill is done or timeout is true.
//Called by timerEvent.
void Processchunks::checkQueueProcessesDone(const bool timeout) {
  if (!mQueue) {
    *mOutStream
        << "Warning: Processchunks::checkQueueProcessesDone called when mQueue is false"
        << endl;
    return;
  }
  //updating mProcessesWithUnfinishedKillRequest is not done by
  //ProcessHandler in the case of a queue.
  int index = 0;
  //Remove indexes associated with finished queued chunks
  //The size of mProcessesWithUnfinishedKillRequest may decrease while this
  //loop is running.
  while (index < mProcessesWithUnfinishedKillRequest.size()) {
    int processIndex = mProcessesWithUnfinishedKillRequest.at(index);
    //The .qid file is deleted when a queued chunk finishes
    if (!mProcessArray[processIndex].qidFileExists()) {
      //This call will cause processIndex to be removed from
      //mProcessesWithUnfinishedKillRequest so don't increment index.
      mProcessArray[processIndex].cleanupKillProcess();
    }
    else {
      index++;
    }
  }
  //Move on to clean up if all kills have completed or kill processes has timed
  //out.
  if (timeout || mProcessesWithUnfinishedKillRequest.isEmpty()) {
    cleanupKillProcesses(timeout);
  }
}

//Cleans up process killing.  Must only be called when all kill requests have
//been sent.
//Also updates mProcessesWithUnfinishedKillRequest in the case of a queue.
//If timeout is false and mProcessesWithUnfinishedKillRequest is not empty, exits
//without cleaning up.
void Processchunks::cleanupKillProcesses(const bool timeout) {
  if (!mKill) {
    *mOutStream
        << "Warning: Processchunks::cleanupKillProcess called when mKill is false"
        << endl;
    return;
  }
  if (!mAllKillProcessesHaveStarted) {
    *mOutStream
        << "Warning: Processchunks::cleanupKillProcess called when mAllKillProcessesHaveStarted is false"
        << endl;
    return;
  }
  //Leave if still need to wait for process kill requests and kill timer hasn't
  //timed out yet.
  if (!timeout && !mProcessesWithUnfinishedKillRequest.isEmpty()) {
    return;
  }
  //clean up

  //Kill the timer to clean up.  It will go back on for D and P.
  if (mTimerId != 0) {
    killTimer(mTimerId);
    mTimerId = 0;
  }
  //Not all kill requests completed so send a timeout message to the processes
  int i;
  for (i = 0; i < mProcessesWithUnfinishedKillRequest.size(); i++) {
    mProcessArray[mProcessesWithUnfinishedKillRequest.at(i)].msgKillProcessTimeout();
  }
  //Handle error
  if (mAns == 'E') {
    if (!mSyncing) {
      *mOutStream << "ERROR: A CHUNK HAS FAILED " << mMaxChunkErr << " times"
          << endl;
    }
    else {
      *mOutStream << "ERROR: A START, FINISH, OR SYNC CHUNK HAS FAILED" << endl;
    }
    cleanupAndExit(4);
  }

  //Handle drop and pause by resuming processesing
  if (mAns == 'D' || mAns == 'P') {
    *mOutStream << "Resuming processing" << endl;
    mAns = ' ';
    //Reset kill values
    mKillCounter = 0;
    mDropList.clear();
    mKill = false;
    mAllKillProcessesHaveStarted = false;
    mKillProcessMachineIndex = -1;
    mProcessesWithUnfinishedKillRequest.clear();
    startTimers();
    return;
  }

  //If not returning to the timer loop, then exit the program.
  *mOutStream << endl
      << "When you rerun with a different set of machines, be sure to use"
      << endl << "the -r flag to retain the existing results" << endl;
  cleanupAndExit(2);
}

const bool Processchunks::askGo() {
  if (mJustGo) {
    return true;
  }
  *mOutStream << "Enter Y to proceed with the current set of machines: ";
  mOutStream->flush();
  char answer;
  QTextStream inStream(stdin);
  inStream >> answer;
  if (answer == 'Y' || answer == 'y') {
    return true;
  }
  return false;
}

//Change mSshOpts if version is recent enough
void Processchunks::setupSshOpts() {
  QProcess ssh(this);
  const QString command("ssh");
  const QStringList params("-V");
  ssh.start(command, params);
  if (ssh.waitForFinished()) {
    int version = extractVersion(ssh.readAllStandardError());
    if (version == -1) {
      version = extractVersion(ssh.readAllStandardOutput());
    }
    //Check if version if >= to ssh version 3.9
    if (version >= 309) {
      mSshOpts.prepend("-o ConnectTimeout=5 ");
    }
  }
  if (isVerbose(mDecoratedClassName, __func__)) {
    *mOutStream << "mSshOpts:" << mSshOpts.join(" ") << endl;
  }
}

//Setup mMachineList with the queue name or the values in mCpuList.
void Processchunks::setupMachineList() {
  int i;
  //Not implementing $IMOD_ALL_MACHINES since no one seems to have used it.
  MachineHandler *machine;
  if (mQueue) {
    //For a queue, make a CPU list that is all the same name
    machine = new MachineHandler(*this, mQueueName, mQueue);
    mMachineList.append(*machine);
    //Parse mCpuList into mQueueComand and mQueueParamList
    if (mCpuList.isEmpty()) {
      exitError("Queue command doesn't exist.");
    }
    mQueueParamList = mCpuList.split(QRegExp("\\s+"), QString::SkipEmptyParts);
    mQueueCommand = mQueueParamList.takeAt(0);
  }
  else {
    //Setup up machine names from mCpuList
    const QStringList cpuArray = mCpuList.split(",", QString::SkipEmptyParts);
    //Now handling mixed up names (as in bear,bebop,bear) without extra probes.
    //Processes will now be run based on the first time a machine appears in
    //the list.
    //MachineSet temporarily keeps track of machines.
    QSet<QString> machineSet;
    for (i = 0; i < cpuArray.size(); i++) {
      const QString cpuMachine = cpuArray.at(i);
      if (!machineSet.contains(cpuMachine)) {
        machineSet.insert(cpuMachine);
        machine = new MachineHandler(*this, cpuMachine);
        mMachineList.append(*machine);
      }
      else {
        //Increment the number of CPUs in an existing machine.
        machine = &mMachineList.last();
        if (*machine == cpuMachine) {
          machine->incrementNumCpus();
        }
        else {
          //Machine names are mixed up (as in bear,bebop,bear).
          for (int j = 0; j < mMachineList.size(); j++) {
            machine = &(mMachineList)[j];
            if (*machine == cpuMachine) {
              machine->incrementNumCpus();
              break;
            }
          }
        }
      }
    }
  }
  int size = mMachineList.size();
  if (size < 1) {
    exitError("No machines specified");
  }
  //Translate a single number into a list of localhost entries
  if (size == 1) {
    bool ok;
    machine = &(mMachineList)[0];
    const long localByNum = machine->nameToLong(&ok);
    if (ok) {
      if (localByNum > maxLocalByNum) {
        exitError("You cannot run more than %d chunks on localhost by "
          "entering a number", maxLocalByNum);
      }
      machine->setValues("localhost", localByNum);
    }
  }
  if (isVerbose(mDecoratedClassName, __func__)) {
    *mOutStream << "mMachineList:" << endl;
  }
  //Setup machines.  Do this only once after the number of CPUs is finalized.
  mNumCpus = 0;
  for (i = 0; i < mMachineList.size(); i++) {
    machine = &(mMachineList)[i];
    machine->setup();
    mNumCpus += machine->getNumCpus();
    if (isVerbose(mDecoratedClassName, __func__)) {
      *mOutStream << i << ":" << machine->getName() << endl;
    }
  }
  if (isVerbose(mDecoratedClassName, __func__)) {
    *mOutStream << endl;
  }
}

//Setup mHostRoot
void Processchunks::setupHostRoot() {
  QProcess hostname(this);
  const QString command("hostname");
  hostname.start(command);
  if (hostname.waitForFinished()) {
    const QString temp(hostname.readAllStandardOutput());
    const int i = temp.indexOf(".");
    if (i != -1) {
      mHostRoot = temp.mid(0, i);
    }
    else {
      mHostRoot = temp;
    }
    if (isVerbose(mDecoratedClassName, __func__)) {
      *mOutStream << "mHostRoot:" << mHostRoot << endl;
    }
  }
  else {
    exitError("Unable to run the hostname command");
  }
}

//Add $IMOD_DIR to the path
void Processchunks::setupEnvironment() {
  int i;
  mEnv = QProcess::systemEnvironment();
  QString pathReplace("PATH=");
  pathReplace.append(getenv("IMOD_DIR"));
#ifdef _WIN32
  pathReplace.append(";");
#else
  pathReplace.append(":");
#endif
  pathReplace.append("\\1");
  mEnv.replaceInStrings(QRegExp("^PATH=(.*)", Qt::CaseInsensitive), pathReplace);
  if (isVerbose(mDecoratedClassName, __func__)) {
    for (i = 0; i < mEnv.size(); ++i) {
      if (mEnv.at(i).contains(QRegExp("^PATH=(.*)", Qt::CaseInsensitive))) {
        *mOutStream << mEnv.at(i) << endl;
      }
    }
  }
}

//Sets up mProcessArray for single file or multi-file processing
void Processchunks::setupProcessArray() {
  int i;
  QStringList comFileArray;
  if (isVerbose(mDecoratedClassName, __func__)) {
    *mOutStream << "current path:" << QDir::currentPath() << endl;
  }
  //Make the list for a single file
  if (mSingleFile) {
    QString rootName(mRootName);
    const int extIndex = rootName.lastIndexOf(".");
    if (extIndex != -1) {
      rootName = rootName.mid(0, extIndex);
    }
    rootName.append(".com");
    if (!mCurrentDir.exists(rootName)) {
      exitError("The single command file %s does not exist",
          rootName.toLatin1().data());
    }
    comFileArray.append(rootName);
  }
  else {
    //Build up lists in order -nnn, -nnnn, -nnnnn*, which should work both for
    //lists that are all 5 digits or lists that are 3, 4, 5 digits
    //Put -start.com on front and -finish.com on end
    //Add start com file
    int numNumericCommandFiles = 0;
    QString startComFile(mRootName);
    startComFile.append("-start.com");
    if (mCurrentDir.exists(startComFile)) {
      comFileArray.append(startComFile);
    }
    //Add numeric com files
    mCurrentDir.setSorting(QDir::Name);
    mCurrentDir.setFilter(QDir::Files);
    //Add -nnn com files
    QStringList filters;
    //?- is a special character and gets a warning about -trigraphs
    buildFilters("-???.com", "-??\?-sync.com", filters);
    QStringList list = mCurrentDir.entryList(filters);
    cleanupList("-\\D{3,3}(-sync){0,1}\\.com", list);
    numNumericCommandFiles += list.size();
    comFileArray += list;
    //add -nnnn com files
    filters.clear();
    list.clear();
    buildFilters("-????.com", "-???\?-sync.com", filters);
    list = mCurrentDir.entryList(filters);
    cleanupList("-\\D{4,4}(-sync){0,1}\\.com", list);
    numNumericCommandFiles += list.size();
    comFileArray += list;
    //add -nnnnn com files
    filters.clear();
    list.clear();
    buildFilters("-?????.com", "-????\?-sync.com", filters);
    list = mCurrentDir.entryList(filters);
    cleanupList("-\\D{5,5}(-sync){0,1}\\.com", list);
    numNumericCommandFiles += list.size();
    comFileArray += list;
    //Add finish com file
    QString finishComFile(mRootName);
    finishComFile.append("-finish.com");
    if (mCurrentDir.exists(finishComFile)) {
      comFileArray.append(finishComFile);
    }
    if (numNumericCommandFiles == 0) {
      exitError("There are no command files matching %s-nnn.com", mRootName);
    }
  }
  if (isVerbose(mDecoratedClassName, __func__)) {
    *mOutStream << "comFileArray:" << endl;
    for (i = 0; i < comFileArray.size(); i++) {
      *mOutStream << i << ":" << comFileArray.at(i) << endl;
    }
    *mOutStream << endl;
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
    mProcessArray[i].setup(*this, comFileArray.at(i), i);
    if (mProcessArray[i].getFlag() != ProcessHandler::sync && mCopyLogIndex
        == -1) {
      //Setting mCopyLogIndex to the first non-sync log
      mCopyLogIndex = i;
    }
  }
  if (mCopyLogIndex == -1) {
    mCopyLogIndex = 0;
  }
}

//Probe machines by running the "w" command.  Drop machines that don't respond.
void Processchunks::probeMachines() {
  int i;
  //Remove the old checkfile
  if (mCheckFile != NULL) {
    if (mCheckFile->exists()) {
      mCheckFile->remove();
    }
  }
  //probe machines and get all the verifications unless etomo is running it
  if (!mSkipProbe || !mJustGo) {
    *mOutStream << "Probing machine connections and loads..." << endl;
    QProcess w(this);
    const QString localCommand("w");
    const QStringList localParams;
    const QString remoteCommand("ssh");
    QStringList remoteParams("-x");
    for (i = 0; i < mSshOpts.size(); i++) {
      remoteParams.append(mSshOpts.at(i));
    }
    remoteParams << "dummy" << "hostname ; w";
    //Probing the machines and building a new cpu array from the ones that
    //respond.
    bool status = -1;
    i = 0;
    if (isVerbose(mDecoratedClassName, __func__)) {
      *mOutStream << "mMachineList.size():" << mMachineList.size() << endl;
    }
    while (i < mMachineList.size()) {
      const QString machName = (mMachineList)[i].getName();
      if (machName == mHostRoot || machName == "localhost") {
        *mOutStream << machName << endl;
        status = runProcessAndOutputLines(w, localCommand, localParams, 1);
      }
      else {
        remoteParams.replace(mSshOpts.size() + 1, machName);
        status = runProcessAndOutputLines(w, remoteCommand, remoteParams, 2);
      }
      //status can also be set to 1 on the local machine if it times out.
      //No longer testing for 141 before no longer supporting SGI
      if (status != 0) {
        *mOutStream << "Dropping " << machName
            << " from list because it does not respond" << endl << endl;
        if (isVerbose(mDecoratedClassName, __func__)) {
          *mOutStream << "status:" << status << endl;
        }
        //Drops failed machine from the machine list
        mMachineList.removeAt(i);
      }
      else {
        i++;
      }
    }
    if (isVerbose(mDecoratedClassName, __func__)) {
      *mOutStream << "end probe machines" << endl;
    }
  }
}

//Look for commands in mCheckFile.  CheckFile is kept open so already processed
//commands are not read twice.
//Return true if a valid command is found in the check file
const bool Processchunks::readCheckFile() {
  //Handle mCheckFile
  if (mCheckFile != NULL) {
    if (mCheckFile->exists()) {
      if (!mCheckFile->isOpen()) {
        mCheckFile->open(QIODevice::ReadOnly);
      }
      if (mCheckFile->isReadable()) {
        QTextStream stream(mCheckFile);
        QString comLine = stream.readLine();
        while (!comLine.isNull()) {
          mAns = comLine.at(0).toLatin1();
          if (mAns == 'D' && comLine.size() > 1) {
            //machine name(s) are required
            QStringList dropList = comLine.mid(1).trimmed().split(",",
                QString::SkipEmptyParts);
            killProcesses(&dropList);
            return true;
          }
          else if (mAns == 'P') {
            killProcesses();
            return true;
          }
          else if (mAns == 'Q') {
            killProcesses();
            return true;
          }
          else {
            *mOutStream << "BAD COMMAND IGNORED: " << comLine << endl;
          }
          comLine = stream.readLine();
        }
      }
    }
  }
  return false;
}

//Stop if all have now been dropped out or all have failed and none done
void Processchunks::exitIfDropped(const int minFail, const int failTot,
    const int assignTot) {
  if (isVerbose(mDecoratedClassName, __func__, 2)) {
    *mOutStream << "minFail=" << minFail << ",failTot:" << failTot
        << ",assignTot:" << assignTot << ",mDropCrit:" << mDropCrit
        << ",mNumCpus:" << mNumCpus << ",mNumDone:" << mNumDone << ",mPausing:"
        << mPausing << ",mSyncing:" << mSyncing << ",mQueue:" << mQueue
        << ",mMachineList.size():" << mMachineList.size() << endl;
  }
  if (minFail >= mDropCrit) {
    *mOutStream << "ERROR: ALL MACHINES HAVE BEEN DROPPED DUE TO FAILURES"
        << endl;
    if (isVerbose(mDecoratedClassName, __func__)) {
      *mOutStream << "minFail:" << minFail << ",mDropCrit:" << mDropCrit
          << endl;
    }
    cleanupAndExit(1);
  }
  if (mPausing && assignTot == 0) {
    *mOutStream
        << "All previously running chunks are done - exiting as requested"
        << endl << "Rerun with -r to resume and retain existing results"
        << endl;
    cleanupAndExit(2);
  }
  if (assignTot == 0 && mNumDone == 0) {
    if (failTot == mNumCpus) {
      *mOutStream
          << "ERROR: NO CHUNKS HAVE WORKED AND EVERY MACHINE HAS FAILED"
          << endl;
      if (isVerbose(mDecoratedClassName, __func__)) {
        *mOutStream << "failTot:" << failTot << ",mNumCpus:" << mNumCpus
            << endl;
      }
      cleanupAndExit(1);
    }
    //Handle the case where the start.com cannot be run.
    else if (mSyncing) {
      if (!mQueue) {
        if (failTot == mMachineList.size()) {
          *mOutStream
              << "ERROR: NO CHUNKS HAVE WORKED AND EVERY MACHINE HAS FAILED"
              << endl;
          if (isVerbose(mDecoratedClassName, __func__)) {
            *mOutStream << "failTot:" << failTot << ",mMachineList.size():"
                << mMachineList.size() << endl;
          }
          cleanupAndExit(1);
        }
      }
      else if (failTot == 1 && minFail == mQueue) {
        *mOutStream
            << "ERROR: NO CHUNKS HAVE WORKED AND EVERY MACHINE HAS FAILED"
            << endl;
        if (isVerbose(mDecoratedClassName, __func__)) {
          *mOutStream << "failTot:" << failTot << ",mQueue:" << mQueue << endl;
        }
        cleanupAndExit(1);
      }
    }
  }
}

//Handle chunk done: deassign, get rid of chunk errors,
//When it is the first chunk done, issue drop messages
//copy the log for the first non-sync chunk
//Return true if all chunks are done
const bool Processchunks::handleChunkDone(MachineHandler *machine,
    const int cpuIndex, const int processIndex) {
  int i;
  //If it is DONE, then set flag to done and deassign
  //Exonerate the machine from chunk errors if this chunk
  //gave a previous chunk error
  mProcessArray[processIndex].setFlag(ProcessHandler::done);
  machine->setAssignedProcIndex(cpuIndex, -1);
  machine->setFailureCount(0);
  mSyncing = 0;
  if (mProcessArray[processIndex].getNumChunkErr() != 0) {
    machine->setChunkErred(false);
  }
  mNumDone++;
  *mOutStream << mProcessArray[processIndex].getComFileName()
      << " finished on " << machine->getName() << endl;
  mProcessArray[processIndex].printWarnings();
  if (mSingleFile) {
    if (!mSkipProbe) {
      cleanupAndExit();
    }
    return true;
  }
  //If this is the first one done, issue drop messages now
  //on ones that chunk errored and exceeded failure count
  if (!mAnyDone) {
    for (i = 0; i < mMachineList.size(); i++) {
      MachineHandler*m = &(mMachineList)[i];
      if (m->getFailureCount() >= mDropCrit && m->isChunkErred()) {
        *mOutStream << "Dropping " << m->getName() << endl;
      }
    }
  }
  mAnyDone = true;
  //copy the log for the first non-sync chunk
  if (processIndex == mCopyLogIndex) {
    QString rootLogName = QString("%1.log").arg(mRootName);
    *mOutStream << "rootLogName:" << rootLogName << endl;
    //Backup the root log if it exists
    imodBackupFile(rootLogName.toLatin1().data());
    QFile rootLog(rootLogName);
    if (!rootLog.open(QIODevice::WriteOnly)) {
      handleFileSystemBug();
      if (!rootLog.open(QIODevice::WriteOnly)) {
        *mOutStream << "Warning: Unable to write copied chunk log "
            << rootLogName << endl;
        return false;
      }
    }
    QTextStream writeStream(&rootLog);
    writeStream
        << "THIS FILE IS JUST THE LOG FOR ONE CHUNK AND WAS COPIED BY PROCESSCHUNKS FROM "
        << mProcessArray[processIndex].getLogFileName() << endl;
    const QByteArray log = mProcessArray[processIndex].readAllLogFile();
    if (!log.isEmpty()) {
      writeStream << log.data();
    }
    rootLog.close();
  }
  return false;
}

//Looks for and print an error message in log file.
//If the chunk has errored too many times, set mAns to E and kill jobs
//Return false the chunk has errored too many times
const bool Processchunks::handleLogFileError(QString &errorMess,
    MachineHandler *machine, const int cpuIndex, const int processIndex) {
  int numErr;
  mProcessArray[processIndex].getErrorMessageFromLog(errorMess);
  mProcessArray[processIndex].incrementNumChunkErr();
  numErr = mProcessArray[processIndex].getNumChunkErr();
  machine->setChunkErred(true);
  //Give up if the chunk errored too many  times: and
  //for a sync chunk that is twice or once if one machine
  if (numErr >= mMaxChunkErr || (mSyncing && (mMachineList.size() == 1
      || numErr >= 2))) {
    mProcessArray[processIndex].printTooManyErrorsMessage(numErr);
    if (!errorMess.isEmpty()) {
      *mOutStream << errorMess << endl;
    }
    mAns = 'E';
    //Unassign the CPU
    machine->setAssignedProcIndex(cpuIndex, -1);
    killProcesses();
    return false;
  }
  return true;
}

//Handle timeouts and missing qid files for queues
//Handle com never started and process ended - drop
//Handle ssh error - drop
//Handle com not started yet
//Handle log doen't exist and timeout - drop
void Processchunks::handleComProcessNotDone(bool &dropout, QString &dropMess,
    MachineHandler *machine, const int processIndex) {
  if (mQueue && !mProcessArray[processIndex].qidFileExists()) {
    //For a queue, the qid file should be there
    dropout = true;
    dropMess = "it failed to be submitted to queue";
  }
  else if (!mQueue) {
    //Either there is no log file or the .csh is still present:
    //check the ssh file and accumulate timeout
    //If the ssh file is non empty check for errors there
    if (mProcessArray[processIndex].getSshError(dropMess)) {
      //A cd or ssh error is very serious - stop using this machine.
      dropout = true;
      machine->setFailureCount(mDropCrit);
    }
    if (!dropout && mProcessArray[processIndex].isFinishedSignalReceived()) {
      dropout = true;
    }
    else if (!dropout) {
      //if log file doesn't exist, check the pid
      //and give up after timeout
      if (mProcessArray[processIndex].isStartProcessTimedOut(runProcessTimeout)) {
        dropout = true;
      }
    }
  }
}

//remove the assignment, mark chunk as to be done,
//skip this machine on this round
void Processchunks::handleDropOut(bool &noChunks, QString &dropMess,
    MachineHandler *machine, const int cpuIndex, const int processIndex,
    QString &errorMess) {
  *mOutStream << mProcessArray[processIndex].getComFileName() << " failed on "
      << machine->getName() << " - need to restart" << endl;
  if (!errorMess.isEmpty()) {
    *mOutStream << errorMess << endl;
  }
  mProcessArray[processIndex].setFlagNotDone();
  if (mSyncing) {
    mSyncing = 1;
  }
  machine->setAssignedProcIndex(cpuIndex, -1);
  noChunks = false;
  machine->incrementFailureCount();
  if (machine->getFailureCount() >= mDropCrit) {
    if (errorMess.isEmpty()) {
      mProcessArray[processIndex].getErrorMessageFromOutput(errorMess);
      if (!errorMess.isEmpty()) {
        *mOutStream << errorMess << endl;
      }
    }
    if (dropMess.isEmpty()) {
      dropMess = "it failed (with ";
      if (!machine->isChunkErred()) {
        dropMess.append("time out");
      }
      else {
        dropMess.append("chunk error");
      }
      dropMess.append(") %1 times in a row");
      dropMess = dropMess.arg(machine->getFailureCount());
    }
    if (!mAnyDone && machine->isChunkErred()) {
      *mOutStream << "Holding off on using ";
    }
    else {
      *mOutStream << "Dropping ";
    }
    *mOutStream << machine->getName() << " - " << dropMess << endl;
  }
}

//See if a process can be run by the current machine
//Return false when check fails and chunk should not be run
const bool Processchunks::checkChunk(int &runFlag, bool &noChunks,
    int &undoneIndex, bool &foundChunks, bool &chunkOk,
    MachineHandler *machine, const int processIndex, const int chunkErrTot) {
  runFlag = mProcessArray[processIndex].getFlag();
  //But if the next com is a sync, record number and break loop
  if (runFlag == ProcessHandler::sync && !mSyncing) {
    mNextSyncIndex = processIndex;
    if (!foundChunks) {
      noChunks = true;
    }
    return false;
  }
  if (undoneIndex == -1 && runFlag != ProcessHandler::done) {
    undoneIndex = processIndex;
  }
  //If any chunks found set that flag
  if (runFlag == ProcessHandler::sync || runFlag == ProcessHandler::notDone) {
    foundChunks = true;
  }
  //Skip a chunk if it has errored, if this machine has given chunk
  //error, and not all machines have done so
  chunkOk = true;
  if (runFlag == ProcessHandler::sync
      && mProcessArray[processIndex].getNumChunkErr() > 0
      && machine->isChunkErred() && chunkErrTot < mNumCpus) {
    chunkOk = false;
    if (mSyncing) {
      return false;
    }
  }
  return true;
}

//Build the .csh file and run the process
void Processchunks::runProcess(MachineHandler *machine, const int cpuIndex,
    const int processIndex) {
  machine->setAssignedProcIndex(cpuIndex, processIndex);
  mProcessArray[processIndex].setFlag(ProcessHandler::assigned);
  //don't forget to start the timer
  mProcessArray[processIndex].backupLog();
  mProcessArray[processIndex].removeProcessFiles();
  *mOutStream << "Running " << mProcessArray[processIndex].getComFileName()
      << " on " << machine->getName() << " ..." << endl;
  mProcessArray[processIndex].makeCshFile();
  //If running a sync, set the syncing flag to 2
  if (mSyncing) {
    mSyncing = 2;
  }
  mProcessArray[processIndex].runProcess(machine);
}

//Extracts the first two numbers of a numeric version.  Multiples the first
//number by 100 and adds it to the second number.  Places the result in
//mVersion.
const int Processchunks::extractVersion(const QString &versionString) {
  int iversion = -1;
  if (isVerbose(mDecoratedClassName, __func__)) {
    *mOutStream << "ssh sshOutput:" << versionString << endl;
  }
  const QRegExp regExp("[0-9]+\\.[0-9]+");
  const int i = regExp.indexIn(versionString, 0);
  const int len = regExp.matchedLength();
  if (i != -1 && len != -1) {
    QString version = versionString.mid(i, len);
    if (version != NULL) {
      const QStringList array = version.split(".", QString::SkipEmptyParts);
      if (!array.isEmpty()) {
        bool ok;
        iversion = array.at(0).toLong(&ok) * 100;
        if (!ok) {
          iversion = -1;
          return iversion;
        }
        if (array.size() > 1) {
          version += array.at(1).toLong(&ok);
          if (!ok) {
            version = -1;
          }
        }
      }
    }
  }
  if (isVerbose(mDecoratedClassName, __func__)) {
    *mOutStream << "ssh version:" << iversion << endl;
  }
  return iversion;
}

void Processchunks::buildFilters(const char *reg, const char *sync,
    QStringList &filters) {
  int i;
  QString filter1(mRootName);
  filter1.append(reg);
  filters.append(filter1);
  QString filter2(mRootName);
  filter2.append(sync);
  filters.append(filter2);
  if (isVerbose(mDecoratedClassName, __func__)) {
    *mOutStream << "filters:" << endl;
    for (i = 0; i < filters.size(); i++) {
      *mOutStream << i << ":" << filters.at(i) << endl;
    }
    *mOutStream << endl;
  }
}

void Processchunks::cleanupList(const char *remove, QStringList &list) {
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
const int Processchunks::runProcessAndOutputLines(QProcess &process,
    const QString &command, const QStringList &params, const int numLines) {
  int i;
  if (isVerbose(mDecoratedClassName, __func__)) {
    *mOutStream << "command:" << command << " " << params.join(" ") << endl;
  }
  process.start(command, params);
  if (process.waitForFinished()) {
    const QByteArray output = process.readAllStandardOutput();
    if (isVerbose(mDecoratedClassName, __func__)) {
      const QByteArray errArray = process.readAllStandardError();
      if (!errArray.isEmpty()) {
        *mOutStream << "stderr:" << errArray << endl;
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
        *mOutStream << output;
        break;
      }
      else {
        *mOutStream << output.mid(startIndex, endIndex - startIndex + 1);
      }
    }
    return process.exitCode();
  }
  return 1;
}

void Processchunks::handleFileSystemBug() {
  *mOutStream << "tag Z" << endl;
  mLsProcess->start("ls", mLsParamList);
  mLsProcess->waitForFinished(10000);
}

const QStringList &Processchunks::getEnv() {
  return mEnv;
}

const bool Processchunks::isQueue() {
  return mQueue;
}
const QString &Processchunks::getQueueCommand() {
  return mQueueCommand;
}

QDir &Processchunks::getCurrentDir() {
  return mCurrentDir;
}

const QStringList &Processchunks::getQueueParamList() {
  return mQueueParamList;
}

const bool Processchunks::isVerbose(const QString &verboseClass,
    const char *verboseFunction, const int verbosity) {
  if (!mVerbose) {
    return false;
  }
  if (verbosity > mVerbose) {
    return false;
  }
  if (mVerboseClass.isEmpty()) {
    return true;
  }
  if (!verboseClass.endsWith(mVerboseClass, Qt::CaseInsensitive)) {
    return false;
  }
  if (mVerboseFunctionList.isEmpty()) {
    return true;
  }
  if (!mVerboseFunctionList.contains(verboseFunction, Qt::CaseInsensitive)) {
    return false;
  }
  return true;
}

const char Processchunks::getAns() {
  return mAns;
}

QTextStream &Processchunks::getOutStream() {
  return *mOutStream;
}

const bool Processchunks::isSingleFile() {
  return mSingleFile;
}

const QString &Processchunks::getHostRoot() {
  return mHostRoot;
}

const QStringList &Processchunks::getSshOpts() {
  return mSshOpts;
}

const int Processchunks::getNice() {
  return mNice;
}

QStringList &Processchunks::getDropList() {
  return mDropList;
}

const int Processchunks::getDropCrit() {
  return mDropCrit;
}

ProcessHandler &Processchunks::getProcessHandler(const int processIndex) {
  return mProcessArray[processIndex];
}

const QString &Processchunks::getRemoteDir() {
  return *mRemoteDir;
}

/*
 $Log$
 Revision 1.9  2010/09/13 19:59:48  sueh
 bug# 1364 In probeMachines removing check file unconditionally.

 Revision 1.8  2010/09/10 06:11:10  sueh
 bug# 1364 Improved verbose functionality.  Fixed drop message bug - dropcrit was being reset.

 Revision 1.7  2010/09/04 00:08:44  sueh
 bug# 1364 Making sure that the timer doesn't go off.  Using join() to print
 QStringList.

 Revision 1.6  2010/09/01 00:53:11  sueh
 bug# 1364 Duplicate changes in processchunks (script) to fix the problem
 that nochunks can be set even when chunks have been found.  Also fix
 problems with syncing.  Get the last line of stdout and stderr when no log
 is available.

 Revision 1.5  2010/08/26 22:56:34  sueh
 bug# 1364 Got -r and -w working.  Fixed problems will killing when a
 computer is dead.  Other fixes.

 Revision 1.4  2010/08/20 21:37:14  sueh
 bug# 1364

 Revision 1.3  2010/06/27 17:30:08  sueh
 bug# 1364 Added MachineHandler.

 Revision 1.2  2010/06/23 20:17:27  sueh
 bug# 1364 Moved most setup functionality into one function.  Changed
 QTprocesschunks to Processchunks.

 Revision 1.1  2010/06/23 16:22:33  sueh
 bug# 1364 First checkin for QT version of processchunks.

 */
