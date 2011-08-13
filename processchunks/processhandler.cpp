/*
 *  Runs a process, receives and handles process signals, kills the process.
 *
 *  Author: Sue Held
 *
 *  Copyright (C) 2010,2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include "processchunks.h"

ProcessHandler::ProcessHandler() {
  mJobFile = NULL;
  mQidFile = NULL;
  mProcesschunks = NULL;
  mComFileJobIndex = -1;
  mMachine = NULL;
  mProcess = NULL;
  mKillProcess = new QProcess(this);
  mKillProcess->setProcessChannelMode(QProcess::ForwardedChannels);
  QObject::connect(mKillProcess, SIGNAL(finished(int, QProcess::ExitStatus)),
      SLOT(handleKillFinished(int, QProcess::ExitStatus)));
  mDecoratedClassName = typeid(*this).name();
  mJobFileTextStream = NULL;
  mLogFile = NULL;
  mQidFileTextStream = NULL;
  mValidJob = false;
  mKillStarted = false;
  mKillCounter = 0;
  mPidWaitCounter = 0;
  mIgnoreKill = true;
  resetFields();
}

void ProcessHandler::resetFields() {
  mLogFileExists = false;
  mErrorSignalReceived = false;
  mProcessError = -1;
  mFinishedSignalReceived = false;
  mStartTime.start();
  mStartingProcess = false;
  mKillFinishedSignalReceived = false;
  mKill = false;
  resetSignalValues();
  mPausing = 0;
  mStderr.clear();
  mPid.clear();
  mLocalKill = false;
}

ProcessHandler::~ProcessHandler() {
  delete mKillProcess;
  delete mProcess;
  delete mLogFile;
  delete mJobFile;
  delete mJobFileTextStream;
  delete mQidFile;
  delete mQidFileTextStream;
}

void ProcessHandler::initProcess() {
  if (mProcess != NULL) {
    disconnect(mProcess, SIGNAL(error(QProcess::ProcessError)), this,
        SLOT(handleError(QProcess::ProcessError)));
    disconnect(mProcess, SIGNAL(finished(int, QProcess::ExitStatus)), this,
        SLOT(handleFinished(int, QProcess::ExitStatus)));
    delete mProcess;
    mProcess = NULL;
  }
  mProcess = new QProcess(this);
  QObject::connect(mProcess, SIGNAL(error(QProcess::ProcessError)),
      SLOT(handleError(QProcess::ProcessError)));
  QObject::connect(mProcess, SIGNAL(finished(int, QProcess::ExitStatus)),
      SLOT(handleFinished(int, QProcess::ExitStatus)));
}

//Set mFlag to -1 for sync com files.
//Return true for non-sync files.
void ProcessHandler::setup(Processchunks &processchunks) {
  mProcesschunks = &processchunks;
  mEscapedRemoteDirPath = mProcesschunks->getRemoteDir();
  mEscapedRemoteDirPath.replace(QRegExp(" "), "\\ ");
  if (processchunks.isQueue()) {
    //Queue command
    //finishes after putting things into the queue
    //$queuecom -w "$curdir" -a R $comname:r
    mCommand = mProcesschunks->getQueueCommand();
  }
  else {
    //Local host command
    //csh -ef < $cshname >& $pidname ; \rm -f $cshname &
#ifndef _WIN32
    mCommand = "csh";
#else
    mCommand = "tcsh";
#endif
  }
  initProcess();
}

void ProcessHandler::setJob(const int jobIndex) {
  if (mValidJob) {
    mProcesschunks->getOutStream()
        << "ERROR: Unable is set job, process handler already contains a runnable job:"
        << mProcesschunks->getComFileJobs()->getComFileName(mComFileJobIndex)
        << ",mComFileJobIndex:" << mComFileJobIndex << endl;
    return;
  }
  resetFields();
  mValidJob = true;
  mComFileJobIndex = jobIndex;
  delete mLogFile;
  mLogFile = NULL;
  mLogFile = new QFile(mProcesschunks->getComFileJobs()->getLogFileName(jobIndex));
  mParamList.clear();
  if (mProcesschunks->isQueue()) {
    delete mJobFile;
    mJobFile = NULL;
    mJobFile = new QFile(mProcesschunks->getComFileJobs()->getJobFileName(jobIndex));
    delete mJobFileTextStream;
    mJobFileTextStream = NULL;
    mJobFileTextStream = new QTextStream(mJobFile);
    delete mQidFile;
    mQidFile = NULL;
    mQidFile = new QFile(mProcesschunks->getComFileJobs()->getQidFileName(jobIndex));
    delete mQidFileTextStream;
    mQidFileTextStream = NULL;
    mQidFileTextStream = new QTextStream(mQidFile);
    mParamList << mProcesschunks->getQueueParamList() << "-w" << mEscapedRemoteDirPath
        << "-a" << "R" << mProcesschunks->getComFileJobs()->getRoot(jobIndex);
  }
  else {
    mParamList << "-ef" << mProcesschunks->getComFileJobs()->getCshFileName(jobIndex);
  }
}

void ProcessHandler::setFlagNotDone(const bool singleFile) {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return;
  }
  mProcesschunks->getComFileJobs()->setFlagNotDone(mComFileJobIndex, singleFile);
}

void ProcessHandler::resetSignalValues() {
  mFinishedSignalReceived = false;
  mErrorSignalReceived = false;
  mExitCode = -1;
  mExitStatus = -1;
  mProcessError = -1;
}

int ProcessHandler::getFlag() {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return CHUNK_DONE;
  }
  return mProcesschunks->getComFileJobs()->getFlag(mComFileJobIndex);
}

bool ProcessHandler::logFileExists(const bool newlyCreatedFile) {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return false;
  }
  if (mLogFileExists) {
    return true;
  }
  if (!newlyCreatedFile) {
    //Don't set mLogFileExists when newlyCreateFile is false because ls may need
    //to be run (with handleFileSystemBug) later - the file may be backed up.

    return mProcesschunks->getCurrentDir().exists(mLogFile->fileName());
  }
  //Set mLogFileExists.  Run ls (with handleFileSystemBug) if necessary.
  mLogFileExists = mProcesschunks->getCurrentDir().exists(mLogFile->fileName());
  if (!mLogFileExists && newlyCreatedFile) {
    mProcesschunks->handleFileSystemBug();
    mLogFileExists = mProcesschunks->getCurrentDir().exists(mLogFile->fileName());
  }
  return mLogFileExists;
}

bool ProcessHandler::qidFileExists() {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return false;
  }
  if (mProcesschunks->isQueue()) {
    return mProcesschunks->getCurrentDir().exists(mQidFile->fileName());
  }
  return true;
}

//Looks for PID in either stderr (non-queue) or in .qid file (queue).
const QString ProcessHandler::getPid() {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return "";
  }
  if (!mProcesschunks->isQueue()) {
    readAllStandardError();
    QTextStream textStream(mStderr);
    getPid(textStream, true);
  }
  else {
    if (!mQidFile->open(QIODevice::ReadOnly)) {
      return mPid;
    }
    getPid(*mQidFileTextStream, true);
    mQidFile->close();
  }
  return mPid;
}

/**
 * Returns true if there is a pid in stderr.  Always returns fase if queue is
 * set.
 */
bool ProcessHandler::isPidInStderr() {
  if (mProcesschunks->isQueue()) {
    return false;
  }
  readAllStandardError();
  QTextStream textStream(mStderr);
  return getPid(textStream, false);
}

/**
 * return true if the pid is found in stream.  If save is true, save the pid
 * to mPid; in this case only check for the pid if mPid is empty.
 */
bool ProcessHandler::getPid(QTextStream &stream, const bool save) {
  if (save && !mPid.isEmpty()) {
    //Don't look for the PID more then once
    return true;
  }
  //Don't set the PID unless the line is conplete (includes an EOL).  Process
  //may not be finished when this function runs.
  QString output = stream.readAll();
  //Look for a PID entry with an EOL so the the complete PID is collected
  int index = output.lastIndexOf("PID:");
  if (index != -1) {
    int endIndex;
#ifdef _WIN32
    endIndex = output.indexOf("\r\n", index);
#else
    endIndex = output.indexOf('\n', index);
#endif
    if (endIndex != -1) {
      if (save) {
        mPid = output.mid(index + 4, endIndex - (index + 4));
      }
      return true;
    }
  }
  return false;
}

const QByteArray ProcessHandler::readAllLogFile() {
  QByteArray log;
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return log;
  }
  if (!mLogFile->open(QIODevice::ReadOnly)) {
    return log;
  }
  log = mLogFile->readAll();
  mLogFile->close();
  return log;
}

bool ProcessHandler::isLogFileEmpty() {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return true;
  }
  return mLogFile->size() == 0;
}

void ProcessHandler::readAllStandardError() {
  QByteArray err = mProcess->readAllStandardError();
  if (!err.isEmpty()) {
    mStderr.append(err);
    if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
      printf("%s\n", err.data());
    }
  }
}

//Looks for cd or ssh error in either stdout/stderr (non-queue) or
//.job file (queue).
//Returns true if found
bool ProcessHandler::getSshError(QString &dropMess) {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return false;
  }
  bool found = false;
  if (!mProcesschunks->isQueue()) {
    readAllStandardError();
    QTextStream textStream(mStderr);
    found = getSshError(dropMess, textStream);
  }
  else {
    if (!mProcesschunks->getCurrentDir().exists(mJobFile->fileName())) {
      return found;
    }
    if (mJobFile->size() == 0) {
      return found;
    }
    if (!mJobFile->open(QIODevice::ReadOnly)) {
      return found;
    }
    found = getSshError(dropMess, *mJobFileTextStream);
  }
  return found;
}

bool ProcessHandler::getSshError(QString &dropMess, QTextStream &stream) {
  //look for cd error & ssh error
  QString line = stream.readLine();
  while (!line.isNull()) {
    if (line.indexOf("cd: ") != -1) {
      dropMess = "it cannot cd to %1 (%2)";
      dropMess = dropMess.arg(mProcesschunks->getRemoteDir(), line);
      return true;
    }
    else if (line.indexOf("ssh: connect to host") != -1) {
      dropMess = "cannot connect (%1)";
      dropMess = dropMess.arg(line);
      return true;
    }
    line = stream.readLine();
  }
  return false;
}

//True when the .com file has been run and it has finished.  Returns true if
//the log exists and the finished signal has been received
bool ProcessHandler::isComProcessDone() {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return false;
  }
  if (mProcesschunks->isQueue()) {
    if (!cshFileExists() && logFileExists(true)) {
      mMachine = NULL;
      return true;
    }
    else {
      return false;
    }
  }
  bool done = mFinishedSignalReceived && logFileExists(true) && getFlag()
      != CHUNK_NOT_DONE;
  return done;
}

//Returns true if a last line of the log file starts with "CHUNK DONE"
bool ProcessHandler::isChunkDone() {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return false;
  }
  if (!mLogFile->open(QIODevice::ReadOnly)) {
    if (logFileExists(true) && !mLogFile->open(QIODevice::ReadOnly)) {
      mProcesschunks->getOutStream() << "Warning: Unable to open "
          << mLogFile->fileName() << endl;
    }
    return false;
  }
  int size = mLogFile->size();
  int sizeToCheck = 25;
  //Attempt to seek to the last line of the file (if the file is larger
  //then 25 characters).  If the seek fails, the whole file will have to be
  //looked at.
  if (size > sizeToCheck) {
    mLogFile->seek(size - sizeToCheck);
  }
  QByteArray lastPartOfFile = mLogFile->readAll();
  mLogFile->close();
  lastPartOfFile = lastPartOfFile.trimmed();
  bool done = lastPartOfFile.endsWith("CHUNK DONE");
  return done;
}

bool ProcessHandler::isPausing() {
  if (mPausing > 10) {
    return false;
  }
  mPausing++;
  return true;
}

//Reads the last 1000 characters of the file.  Returns all the text between
//with "ERROR:" and the end of the file.
void ProcessHandler::getErrorMessageFromLog(QString &errorMess) {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return;
  }
  if (!mLogFile->open(QIODevice::ReadOnly)) {
    return;
  }
  QTextStream stream(mLogFile);
  int size = mLogFile->size();
  if (size == 0) {
    return;
  }
  int sizeToCheck = 1000;
  if (size > sizeToCheck) {
    stream.seek(size - sizeToCheck);
  }
  QString line;
  do {
    line = stream.readLine();
    if (line.indexOf("ERROR:") != -1) {
      errorMess.append(line);
      errorMess.append(" ");
    }
  } while (!stream.atEnd());
  mLogFile->close();
  if (errorMess.isEmpty()) {
    errorMess.append("CHUNK ERROR: (last line) - ");
    errorMess.append(line);
  }
  else {
    errorMess.prepend("CHUNK ");
  }
}

//Reads the last lines of and stderr and appends then to errorMess..
void ProcessHandler::getErrorMessageFromOutput(QString &errorMess) {
  //Use the last lines of and stderr as the error message if the log
  //file is empty.
  char *eol;
#ifdef _WIN32
  eol = "\r\n";
#else
  eol = "\n";
#endif
  errorMess.append(eol);
  //stderr
  readAllStandardError();
  QByteArray output = mStderr.trimmed();
  if (!output.isEmpty()) {
    int lastLineIndex;
    lastLineIndex = output.lastIndexOf(eol);
    if (lastLineIndex != -1) {
      errorMess.append(output.mid(lastLineIndex));
      errorMess.append(eol);
    }
    else {
      errorMess.append(mStderr);
    }
  }
}

void ProcessHandler::printTooManyErrorsMessage(const int numErr) {
  mProcesschunks->getOutStream() << "ERROR: " << getComFileName()
      << " has given processing error " << numErr << " times - giving up" << endl;
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    mProcesschunks->getOutStream() << mDecoratedClassName << ":" << __func__
        << ":mExitCode:" << mExitCode << ",mExitStatus:" << mExitStatus << endl;
  }
}

void ProcessHandler::incrementNumChunkErr() {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return;
  }
  mProcesschunks->getComFileJobs()->incrementNumChunkErr(mComFileJobIndex);
}

void ProcessHandler::printWarnings(const QString &machineName) {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return;
  }
  if (!mLogFile->open(QIODevice::ReadOnly)) {
    return;
  }
  QString line = mLogFile->readLine();
  do {
    line = mLogFile->readLine();
    if (line.indexOf("WARNING:") != -1) {
      mProcesschunks->getOutStream() << line;
    }
    else if (line.indexOf("MESSAGE:") != -1) {
      mProcesschunks->getOutStream() << line.trimmed() << " - on " << machineName << endl;
    }
  } while (!mLogFile->atEnd());
  mLogFile->close();
}

bool ProcessHandler::cshFileExists() {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return false;
  }
  return mProcesschunks->getCurrentDir().exists(
      mProcesschunks->getComFileJobs()->getCshFileName(mComFileJobIndex));
}

int ProcessHandler::getNumChunkErr() {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return 0;
  }
  return mProcesschunks->getComFileJobs()->getNumChunkErr(mComFileJobIndex);
}

const QString ProcessHandler::getComFileName() {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return "";
  }
  return mProcesschunks->getComFileJobs()->getComFileName(mComFileJobIndex);
}

const QString ProcessHandler::getLogFileName() {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return "";
  }
  return mProcesschunks->getComFileJobs()->getLogFileName(mComFileJobIndex);
}

//Returns true if the process has started, but the log file hasn't been create,
//and the timeout (milliseconds) has been exceeded.
bool ProcessHandler::isStartProcessTimedOut(const int timeout) {
  //If a process isn't running then there is nothing to timeout
  //If a process is running and the log file has been created then the log file
  //was created before the timeout
  if (!mStartingProcess) {
    return false;
  }
  if (mStartTime.elapsed() <= timeout) {
    return false;
  }
  return !logFileExists(true);
}

void ProcessHandler::setFlag(const int flag) {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return;
  }
  mProcesschunks->getComFileJobs()->setFlag(mComFileJobIndex, flag);
}

void ProcessHandler::removeFiles() {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return;
  }
  mProcesschunks->getCurrentDir().remove(mLogFile->fileName());
  mProcesschunks->getCurrentDir().remove(
      mProcesschunks->getComFileJobs()->getCshFileName(mComFileJobIndex));
  removeProcessFiles();
}

void ProcessHandler::removeProcessFiles() {
  if (mProcesschunks->isQueue()) {
    mProcesschunks->getCurrentDir().remove(mJobFile->fileName());
    mProcesschunks->getCurrentDir().remove(mQidFile->fileName());
  }
  else {
    mStderr.clear();
    mPid.clear();
  }
}

QString ProcessHandler::getCshFile() {
  QString temp;
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return temp;
  }
  temp = mProcesschunks->getComFileJobs()->getCshFileName(mComFileJobIndex);
  return temp;
}

//Run the process.
void ProcessHandler::runProcess(MachineHandler &machine) {
  /*There is no way to undo the functionality preceding the running of a
   process.  The process MUST be run before exiting this function unless there
   is a serious error.*/
  //Don't run a job that has been reset.
  if (!mValidJob) {
    mProcesschunks ->getOutStream() << "ERROR: Job is not runnable,index="
        << mComFileJobIndex << endl;
    return;
  }
  mMachine = &machine;
  int i;
  //Build command if necessary
  QString *command = NULL;
  QStringList *paramList = NULL;
  if (!mProcesschunks->isQueue()) {
    //It is not working to run processes using csh on Windows.
    if (mMachine->getName() != mProcesschunks->getHostRoot() && mMachine->getName()
        != "localhost") {
      //Create remove command
      //Original command:
      //ssh -x $sshopts $machname bash --login -c \'"cd $curdir && (csh -ef < $cshname >& $pidname ; \rm -f $cshname)"\' >&! $sshname &
      command = new QString("ssh");
      //To run remote command: bash --login -c '"command"'
      //Escape spaces in the directory path
      //Escaping the single quote shouldn't be necessary because this is not
      //being run from a shell.
      QString param = QString("\"cd %1 && (csh -ef < %2 ; \\rm -f %3)\"").arg(
          mEscapedRemoteDirPath,
          mProcesschunks->getComFileJobs()->getCshFileName(mComFileJobIndex),
          mProcesschunks->getComFileJobs()->getCshFileName(mComFileJobIndex));
      paramList = new QStringList();
      paramList->append("-x");
      QStringList sshOpts = mProcesschunks->getSshOpts();
      for (i = 0; i < sshOpts.size(); i++) {
        paramList->append(sshOpts.at(i));
      }
      *paramList << mMachine->getName() << "bash" << "--login" << "-c" << param;
    }
    /*Hook stdin to a file to avoid excessive pipes - this is necessary to keep
     the pipes per process down to 4.  If stdout was no going to a file, the
     process pipe count would be 6.  In that case the total number of CPUs
     allowed (Processchunks::setupMachineList::numCpusLimit) would have to be
     reduced.*/
    mProcess->setStandardOutputFile(
        QString("%1.stdout").arg(
            mProcesschunks->getComFileJobs()->getCshFileName(mComFileJobIndex)),
        QProcess::Truncate);
    mProcess->setStandardInputFile(
        mProcesschunks->getComFileJobs()->getCshFileName(mComFileJobIndex));
  }
  //Run command
  resetSignalValues();
  if (command != NULL) {
    //Run on a remote machine
    mProcess->start(*command, *paramList);
    mProcess->closeWriteChannel();
    b3dMilliSleep(mProcesschunks->getMillisecSleep());
  }
  else {
    //Run on local machine or queue
    mProcess->start(mCommand, mParamList);
    mProcess->closeWriteChannel();
    b3dMilliSleep(mProcesschunks->getMillisecSleep());
    if (mProcesschunks->isQueue()) {
      mProcess->waitForFinished(2000);
    }
  }
  //Turn on running process boolean and record start time
  mStartingProcess = true;
  mStartTime.restart();
  delete command;
  delete paramList;
}

void ProcessHandler::startKill() {
  mKill = true;
  mIgnoreKill = !isJobValid();
}

/*
 Handles the kill signal when imodkillgroup isn't used.  If the is a queue and
 the kill request has already been sent, this process waits for it to finish.
 */
void ProcessHandler::killSignal() {
  if (mIgnoreKill || (mKillFinishedSignalReceived && mFinishedSignalReceived)) {
    return;
  }
  //For remote machines imodkillgroup can be used to kill all the processes
  if (!mKillStarted) {
    if (mProcesschunks->isQueue()) {
      mKillStarted = true;//This starts the 15-count timeout
      setJobNotDone();
      mProcesschunks->incrementKills();
      //Kill the process
      char ans = mProcesschunks->getAns();
      QString action(ans);
      if (ans != 'P') {
        action = "K";
        //Don't know if this waits until the kill is does
        //$queuecom -w "$curdir" -a $action $comlist[$ind]:r
        //The second to last parameter is the action letter
        mParamList.replace(mParamList.size() - 2, action);
        mKillProcess->start(mCommand, mParamList);
        mKillProcess->waitForFinished(1000);
        //Put mParamList back to its regular form
        mParamList.replace(mParamList.size() - 2, "R");
      }
    }
    else {
      //This must be a local job
      if (!isPidEmpty() || mPidWaitCounter > 15) {
        //PIDs are available
        mKillStarted = true;//This starts the 15-count timeout
        if (!isPidEmpty()) {
          mProcesschunks->getOutStream() << "Killing "
              << mProcesschunks->getComFileJobs()->getComFileName(mComFileJobIndex);
          if (mMachine == NULL) {
            mProcesschunks->getOutStream() << endl;
          }
          else {
            mProcesschunks->getOutStream() << " on " << mMachine->getName() << endl;
          }
          setJobNotDone();
          //Kill a local job.  Killing non-local jobs is not handled handled by
          //ProcessHandlers.
          killLocalProcessAndDescendents(mPid);
          mKillFinishedSignalReceived = true;
          mFinishedSignalReceived = true;
        }
        else {
          //Unable to get the PID.
          mProcesschunks->getOutStream() << "Unable to kill a processes on "
              << mMachine->getName() << endl;
          mKillFinishedSignalReceived = true;
          mProcess->kill();
          mFinishedSignalReceived = true;
        }
      }
      else {
        mPidWaitCounter++;
      }
    }
  }
  else {
    //Waiting for kill to finish
    mKillCounter++;
    if (mKillCounter > 15 && (!mKillFinishedSignalReceived || !mFinishedSignalReceived)) {
      if (mProcesschunks->isQueue()) {
        mKillProcess->kill();
        mProcesschunks->decrementKills();
      }
      mKillFinishedSignalReceived = true;
      mFinishedSignalReceived = true;
    }
  }
}

void ProcessHandler::resetKill() {
  mKillFinishedSignalReceived = false;
  mKillStarted = false;
  mKillCounter = 0;
  mPidWaitCounter = 0;
  mKill = false;
  mIgnoreKill = true;
}

bool ProcessHandler::isPidEmpty() {
  getPid();
  return mPid.isEmpty();
}

void ProcessHandler::setJobNotDone() {
  mProcesschunks->getComFileJobs()->setFlag(mComFileJobIndex, CHUNK_NOT_DONE);
}

void ProcessHandler::killQProcesses() {
  mProcess->kill();
  mKillProcess->kill();
}

//Sets signal variables.  For a non-queue removes the .csh file on the local
//machine.  If the process was killed, tell processchunks that its done.
void ProcessHandler::handleFinished(const int exitCode,
    const QProcess::ExitStatus exitStatus) {
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    printf("%s:%s:%d,exitStatus:%d\n", mDecoratedClassName.toLatin1().data(), __func__,
        exitCode, exitStatus);
  }
  if (mComFileJobIndex == -1) {
    printf("ERROR: Job index not set\n");
    return;
  }
  mFinishedSignalReceived = true;
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    printf(
        "%s:%s:%s\n",
        mDecoratedClassName.toLatin1().data(),
        __func__,
        mProcesschunks->getComFileJobs()->getComFileName(mComFileJobIndex).toLatin1().data());
    readAllStandardError();
  }
  mStartingProcess = false;
  mExitCode = exitCode;
  mExitStatus = exitStatus;
  //The queue request just submits the chunk to the queue, or submits a request
  //to kill the chunk to the queue.  Don't use it to figure out the state of the
  //chunk.
  if (!mProcesschunks->isQueue()) {
    if (mMachine->getName() == mProcesschunks->getHostRoot() || mMachine->getName()
        == "localhost") {
      mProcesschunks->getCurrentDir().remove(
          mProcesschunks->getComFileJobs()->getCshFileName(mComFileJobIndex));
    }
    mProcesschunks->getCurrentDir().remove(
        QString("%1.stdout").arg(
            mProcesschunks->getComFileJobs()->getCshFileName(mComFileJobIndex)));
    if (!mKill) {
      mMachine = NULL;
    }
  }
}

void ProcessHandler::handleKillFinished(const int exitCode,
    const QProcess::ExitStatus exitStatus) {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return;
  }
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    mProcesschunks->getOutStream() << mDecoratedClassName << ":" << __func__ << ":"
        << mProcesschunks->getComFileJobs()->getComFileName(mComFileJobIndex)
        << ",exitCode:" << exitCode << ",exitStatus:" << exitStatus << endl;
  }
  if (!mKillFinishedSignalReceived) {
    mProcesschunks->decrementKills();
    mKillFinishedSignalReceived = true;
  }
  if (mProcesschunks->isQueue()) {
    //The queue kill request is syncronous with the job.
    //exitCode == 0:  Kill is completed
    //exitCode == 100:  Process finished before it could be killed
    //exitCode == 101:  Unable to pause because the process had already started.
    if (exitCode != 0 && exitCode != 100 && exitCode != 101) {
      mProcesschunks->getOutStream() << "kill process exitCode:" << exitCode << endl;
      QByteArray byteArray = mKillProcess->readAllStandardError();
      if (!byteArray.isEmpty()) {
        mProcesschunks->getOutStream() << byteArray << endl;
      }
    }
  }
}

void ProcessHandler::handleError(const QProcess::ProcessError processError) {
  if (mLocalKill) {
    //KillLocalProcessAndDescendents was used - causes a return code of 1
    return;
  }
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return;
  }
  mErrorSignalReceived = true;
  mProcessError = processError;
  mProcesschunks->getOutStream() << mProcesschunks->getComFileJobs()->getComFileName(
      mComFileJobIndex) << ":process error:" << processError << ","
      << mProcess->errorString() << endl;
  mProcesschunks->getOutStream() << "exitCode:" << mProcess->exitCode() << ",exitStatus:"
      << mProcess->exitStatus() << ",state:" << mProcess->state() << endl;
}

//Stop and then kill the process with process ID pid and all of its descendents.
//Waits for kill commands to complete.
//Can't use imodkillgroup on the local host because we don't want to kill
//processchunks, and its part of the group.
void ProcessHandler::killLocalProcessAndDescendents(QString &pid) {
  if (mComFileJobIndex == -1) {
    mProcesschunks->getOutStream() << "ERROR: Job index not set" << endl;
    return;
  }
  //immediately stop the process
  stopProcess(pid);
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    mProcesschunks->getOutStream() << mDecoratedClassName << ":" << __func__
        << ":stopping " << pid << endl;
  }
  int i;
  QStringList pidList;
  pidList.append(pid.trimmed());
  int pidIndex = -1;
  int ppidIndex = -1;
  QProcess ps;
  QString command("ps");
  QStringList paramList;
  paramList.append("axl");
  //Run ps and stop descendent processes until there is nothing left to stop.
  bool foundNewChildPid = false;
  do {
    //Run ps axl
    ps.start(command, paramList);
    ps.waitForFinished(5000);
    if (!ps.exitCode()) {
      QTextStream stream(ps.readAllStandardOutput().trimmed());
      if (!stream.atEnd()) {
        //Find the pid and ppid columns if they haven't already been found.
        QString header;
        if (pidIndex == -1 || ppidIndex == -1) {
          header = stream.readLine().trimmed();
          if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
            mProcesschunks->getOutStream() << mDecoratedClassName << ":" << __func__
                << ":ps column header:" << endl << header << endl;
          }
          QStringList headerList = header.split(QRegExp("\\s+"), QString::SkipEmptyParts);
          for (i = 0; i < headerList.size(); i++) {
            if (headerList.at(i) == "PID") {
              pidIndex = i;
            }
            else if (headerList.at(i) == "PPID") {
              ppidIndex = i;
            }
            if (pidIndex != -1 && ppidIndex != -1) {
              break;
            }
          }
        }
        //collect child pids
        if (pidIndex != -1 && ppidIndex != -1) {
          foundNewChildPid = false;
          do {
            QString line = stream.readLine().trimmed();
            QStringList columns = line.split(QRegExp("\\s+"), QString::SkipEmptyParts);
            if (columns.size() < pidIndex || columns.size() < ppidIndex) {
              break;
            }
            //Look for child processes of parent processes that have already been
            //found.  Stop them and add them to the process ID list.  If
            //the process ids are in order, all the descendent pids should be
            //found during the first pass.
            if (pidList.contains(columns.at(ppidIndex))) {
              QString childPid = columns.at(pidIndex);
              if (!pidList.contains(childPid)) {
                stopProcess(childPid);
                foundNewChildPid = true;
                pidList.append(childPid);
                if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
                  mProcesschunks->getOutStream() << mDecoratedClassName << ":"
                      << __func__ << ":stopping:" << endl << line << endl;
                }
              }
            }
          } while (!stream.atEnd());
        }
        else {
          mProcesschunks->getOutStream()
              << "Warning: May not have been able to kill all processes descendent from "
              << mCommand << " " << mProcesschunks->getComFileJobs()->getCshFileName(
              mComFileJobIndex)
              << " on local machine.  Ps PID and PPID columns where not found in "
              << header << "(" << command << " " << paramList.join(" ") << ")." << endl;
          return;
        }
      }
      else {
        mProcesschunks->getOutStream()
            << "Warning: May not have been able to kill all processes descendent from "
            << mCommand << " " << mProcesschunks->getComFileJobs()->getCshFileName(
            mComFileJobIndex) << " on local machine.  Ps command return nothing" << "("
            << command << " " << paramList.join(" ") << ")." << endl;
      }
    }
    else {
      mProcesschunks->getOutStream()
          << "Warning: May not have been able to kill all processes descendent from "
          << mCommand << " " << mProcesschunks->getComFileJobs()->getCshFileName(
          mComFileJobIndex) << " on local machine.  Ps command failed" << "(" << command
          << " " << paramList.join(" ") << ")." << endl;
      return;
    }
  } while (foundNewChildPid);
  mLocalKill = true;
  //Kill everything in the process ID list.
  QProcess kill;
  QString killCommand("kill");
  pidList.prepend("-9");
  kill.execute(killCommand, pidList);
}

//Stop a single process.  Waits for process to complete.
void ProcessHandler::stopProcess(const QString &pid) {
  QProcess ps;
  QString command("kill");
  QStringList paramList;
  paramList.append("-19");
  paramList.append(pid);
  ps.execute(command, paramList);
}

/*
 $Log$
 Revision 1.53  2011/08/12 17:08:12  sueh
 Bug# 1527 In isComProcessDone do not return true for killed processes.

 Revision 1.52  2011/07/29 04:20:27  sueh
 Bug# 1492 In killSignal don't check for available pipes.  MaxKills is only set for non-queue.

 Revision 1.51  2011/07/22 23:07:32  sueh
 Bug# 1521 In printWarnings, use trimmed to strip eol, instead of index of and mid.

 Revision 1.50  2011/07/22 22:49:03  sueh
 Bug# 1521 In printWarnings, stripping the end-of-line character(s) from the MESSAGE-tagged line,
 and adding an endl after the machine name.

 Revision 1.49  2011/07/22 22:09:26  sueh
 Bug# 1521 In printWarnings, appending the machine name to the MESSAGE-tagged line.

 Revision 1.48  2011/07/22 20:59:04  sueh
 Bug# 1521 In printWarnings, printing MESSAGE:-tagged lines.

 Revision 1.47  2011/06/01 03:07:48  sueh
 Bug# 1491 In getErrorMessageFromLog added a space between error messages.

 Revision 1.46  2011/02/05 00:51:17  sueh
 bug# 1426 Preventing a lockup when the PID cannot be gotten and
 processchunks thinks that the process is running.

 Revision 1.45  2011/02/02 22:43:22  sueh
 bug# 1426 Added killQProcesses.

 Revision 1.44  2011/02/02 00:09:37  sueh
 bug# 1426 Removed unused variables and commented-out code.

 Revision 1.43  2011/02/01 22:38:49  sueh
 bug# 1426 Removing old method of killing.

 Revision 1.42  2011/02/01 20:20:52  sueh
 bug# 1426 Fixing killSignal comments.

 Revision 1.41  2011/02/01 01:29:12  sueh
 bug# 1426 In killLocalProcessAndDescendents kill all the collected PIDs at
 once.

 Revision 1.40  2011/01/31 20:02:31  sueh
 bug# 1426 Removed unused const static pidTag.

 Revision 1.39  2011/01/31 19:45:10  sueh
 bug# 1426 Counting kills instead of pipes.

 Revision 1.38  2011/01/27 03:52:10  sueh
 bug# 1426 Removes const from simple variable return values (int, char,
 bool, long) because they cause a warning in the intel compiler.  Moved the
 the kill message for queues to the machine handler so it will only print
 once.

 Revision 1.37  2011/01/26 06:50:11  sueh
 bug# 1426 Stop checking for the pid during the kill for queue.  Always
 clear the param list during setJob.

 Revision 1.36  2011/01/25 07:16:22  sueh
 bug# 1426 Setting mIgnoreKill in startKill() and checking it in killSignal().

 Revision 1.35  2011/01/24 18:46:59  sueh
 bug# 1426 Removed const from timerEvent(QtimerEvent) to avoid a
 compiler warning.

 Revision 1.34  2011/01/21 04:56:25  sueh
 bug# 1426 In setup, calling isQueue from the parameter.

 Revision 1.33  2011/01/21 00:20:24  sueh
 bug# 1426 Added isPidEmpty, killSignal, resetKill, setJobNotDone, startKill.

 Revision 1.32  2011/01/05 20:52:50  sueh
 bug# 1426 Instead of getting a ComFileJob instance, get an index and
 refer to the ComFileJobs instance in Processchunks.  Moved one-line
 functions to the header.  Added isJobValid.

 */
