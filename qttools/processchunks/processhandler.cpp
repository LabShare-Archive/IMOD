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
 */

#include "processchunks.h"
#include <qvector.h>

ProcessHandler::ProcessHandler() {
  mJobFile = NULL;
  mQidFile = NULL;
  mProcesschunks = NULL;
  mComFileJobIndex = -1;
  mGpuNumber = -1;
  mMachine = NULL;
  mProcess = NULL;
  mOutStream = NULL;
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
  mIgnoreKill = true;
  resetFields();
}

void ProcessHandler::resetFields() {
  mLogFileExists = false;
  mErrorSignalReceived = false;
  mProcessError = -1;
  mFinishedSignalReceived = false;
  mStartTime.start();
  mElapsedTime = -1;
  mLogLastModified = QDateTime::currentDateTime();
  mLastSizeCheckTime = mSizeChangedTime = mLogLastModified;
  mLastLogSize = 0;
  mStartingProcess = false;
  mKillFinishedSignalReceived = false;
  mKill = false;
  resetSignalValues();
  mPausing = 0;
  mStderr.clear();
  mPid.clear();
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
void ProcessHandler::setup(Processchunks &processchunks, int gpuNum) {
  mProcesschunks = &processchunks;
  mOutStream = mProcesschunks->getOutStreamPtr();
  mEscapedRemoteDirPath = mProcesschunks->getRemoteDir();
  mEscapedRemoteDirPath.replace(QRegExp(" "), "\\ ");
  mGpuNumber = gpuNum;
  if (processchunks.isQueue()) {
    //Queue command
    //finishes after putting things into the queue
    //$queuecom -w "$curdir" -a R $comname:r
    mCommand = mProcesschunks->getQueueCommand();
  }
  else {
    //Local host command
    //csh -ef < $cshname >& $pidname ; \rm -f $cshname &
    /* CSH -> PY 
#ifndef _WIN32
    mCommand = "csh";
#else
    mCommand = "tcsh";
    #endif */
    mCommand = "python";
  }
  initProcess();
}

void ProcessHandler::setJob(const int jobIndex) {
  if (mValidJob) {
    *mOutStream
        << "ERROR: Unable to set job, process handler already contains a runnable job:"
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
    /* mParamList << "-ef" << mProcesschunks->getComFileJobs()->getCshFileName(jobIndex);
       CSH -> PY */
    mParamList << "-u" << mProcesschunks->getComFileJobs()->getCshFileName(jobIndex);
  }
}

void ProcessHandler::setFlagNotDone(const bool singleFile) {
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
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
  mLogHasError = false;
}

int ProcessHandler::getFlag() {
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
    return CHUNK_DONE;
  }
  return mProcesschunks->getComFileJobs()->getFlag(mComFileJobIndex);
}

bool ProcessHandler::logFileExists(const bool newlyCreatedFile) {
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
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
    *mOutStream << "ERROR: Job index not set" << endl;
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
    *mOutStream << "ERROR: Job index not set" << endl;
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
    endIndex = output.indexOf('\n', index);
    if (endIndex != -1) {
      if (save) {
        mPid = output.mid(index + 4, endIndex - (index + 4)).trimmed();
      }
      return true;
    }
  }
  return false;
}

const QByteArray ProcessHandler::readAllLogFile() {
  QByteArray log;
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
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
    *mOutStream << "ERROR: Job index not set" << endl;
    return true;
  }
  return mLogFile->size() == 0;
}

// Test whether the log file is older than the given timeout valu
// Refreshs the stored last modified time and size only if it is older than the timeout 
// or if it hasn't been checked for 1/5 of the timeout interval
bool ProcessHandler::isLogFileOlderThan(const int timeoutSec) {
  if (timeoutSec <= 0)
    return false;
  QDateTime now = QDateTime::currentDateTime();
  bool modifiedOK = mLogLastModified.secsTo(now) < timeoutSec;
  if (modifiedOK && mLastSizeCheckTime.secsTo(now) < timeoutSec / 5)
    return false;
  QFileInfo fileInfo(*mLogFile);
  if (!fileInfo.exists())
    return false;
  int newSize = fileInfo.size();
  mLastSizeCheckTime = now;
  if (newSize > mLastLogSize) {
    mLastLogSize = newSize;
    mSizeChangedTime = now;
  }
  mLogLastModified = fileInfo.lastModified();
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    *mOutStream << mDecoratedClassName << ":" << __func__ << ": new mod " << 
      mLogLastModified.secsTo(now) << "  size time " << mSizeChangedTime.secsTo(now) <<
      endl;
  }
  return mLogLastModified.secsTo(now) >= timeoutSec &&
    mSizeChangedTime.secsTo(now) >= timeoutSec;
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
    *mOutStream << "ERROR: Job index not set" << endl;
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
    *mOutStream << "ERROR: Job index not set" << endl;
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
    *mOutStream << "ERROR: Job index not set" << endl;
    return false;
  }
  if (!mLogFile->open(QIODevice::ReadOnly)) {
    if (logFileExists(true) && !mLogFile->open(QIODevice::ReadOnly)) {
      *mOutStream << "Warning: Unable to open "
          << mLogFile->fileName() << endl;
    }
    return false;
  }
  int size = mLogFile->size();
  int sizeToCheck = 512;   // This was 25 before scanning for ERROR
  //Attempt to seek to the last line of the file (if the file is larger
  //then 512 characters).  If the seek fails, the whole file will have to be
  //looked at.
  if (size > sizeToCheck) {
    mLogFile->seek(size - sizeToCheck);
  }
  QByteArray lastPartOfFile = mLogFile->readAll();
  mLogFile->close();
  lastPartOfFile = lastPartOfFile.trimmed();
  bool done = lastPartOfFile.endsWith("CHUNK DONE");
  bool mLogHasError = lastPartOfFile.contains("ERROR:");
  return done;
}

// Pause occurs if process is done but CHUNK DONE not found, unless the exit code is
// nonzero and ERROR: was found in the log
// Start a timer on first call and do not return false until enough time is elapsed
bool ProcessHandler::isPausing() {
  if (mPausing) {
    return mPauseTime.elapsed() <= 1000;
  }
  else {
    if (mExitCode > 0 && mLogHasError)
      return false;
    mPausing++;
    mPauseTime.start();
  }
  return true;
}

//Reads the last 1000 characters of the file.  Returns all the text between
//with "ERROR:" and the end of the file.
void ProcessHandler::getErrorMessageFromLog(QString &errorMess) {
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
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
  bool errorFound = false;
  do {
    line = stream.readLine();
    if (errorFound || line.indexOf("ERROR:") != -1) {
    	errorFound = true;
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
  *mOutStream << "ERROR: " << getComFileName()
      << " has given processing error " << numErr << " times - giving up" << endl;
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    *mOutStream << mDecoratedClassName << ":" << __func__
        << ":mExitCode:" << mExitCode << ",mExitStatus:" << mExitStatus << endl;
  }
}

void ProcessHandler::incrementNumChunkErr() {
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
    return;
  }
  mProcesschunks->getComFileJobs()->incrementNumChunkErr(mComFileJobIndex);
}

void ProcessHandler::printWarnings(const QString &machineName) {
  QStringList warnList;
  QVector<int> numWarns;
  int i, lineInd, warnInd;
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
    return;
  }
  if (!mLogFile->open(QIODevice::ReadOnly)) {
    return;
  }
  QString line = mLogFile->readLine();
  do {
    line = mLogFile->readLine();
    if (line.indexOf("WARNING:") != -1) {

      // Keep track of matching warnings on the list
      line = line.trimmed();
      bool match = false;
      for (i = 0; i < warnList.size(); i++) {
        if (line == warnList[i]) {
          match = true;
          break;
        }

        // Look for a match up to the last space and replace final word with ...
        lineInd = line.lastIndexOf(' ');
        warnInd = warnList[i].lastIndexOf(' ');
        match = lineInd > 0 && lineInd == warnInd && 
          line.left(lineInd) ==  warnList[i].left(warnInd);
        if (match) {
          warnList[i] = warnList[i].left(warnInd) + " ...";
          break;
        }
      }
      if (match) {
        numWarns[i] = numWarns[i] + 1;
      } else {
        numWarns.append(1);
        warnList << line;
      }
    }
    else if (line.indexOf("MESSAGE:") != -1) {
      *mOutStream << line.trimmed() << " - on " << machineName << endl;
    }
    else if (line.indexOf("LOGFILE:") != -1) {
      // Assume files to be logged will not be duplicated by the caller.
      *mOutStream << line.trimmed() << endl;
    }
  } while (!mLogFile->atEnd());
  mLogFile->close();
  for (i = 0; i < warnList.size(); i++) {
    *mOutStream << warnList[i];
    if (numWarns[i] > 1)
      *mOutStream << " (" << numWarns[i] << " times)";
    *mOutStream << endl;
  }
}

bool ProcessHandler::cshFileExists() {
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
    return false;
  }
  return mProcesschunks->getCurrentDir().exists(
      mProcesschunks->getComFileJobs()->getCshFileName(mComFileJobIndex));
}

int ProcessHandler::getNumChunkErr() {
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
    return 0;
  }
  return mProcesschunks->getComFileJobs()->getNumChunkErr(mComFileJobIndex);
}

const QString ProcessHandler::getComFileName() {
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
    return "";
  }
  return mProcesschunks->getComFileJobs()->getComFileName(mComFileJobIndex);
}

const QString ProcessHandler::getLogFileName() {
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
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
    *mOutStream << "ERROR: Job index not set" << endl;
    return;
  }
  mProcesschunks->getComFileJobs()->setFlag(mComFileJobIndex, flag);
}

void ProcessHandler::removeFiles() {
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
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
    /* CSH -> PY  make sure there is no .csh file to confuse queuechunk */
    mProcesschunks->getCurrentDir().remove
      (mProcesschunks->getComFileJobs()->getRoot(mComFileJobIndex) + QString(".csh"));
  }
  else {
    mStderr.clear();
    mPid.clear();
  }
}

QString ProcessHandler::getCshFile() {
  QString temp;
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
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

      //Original command:
      //ssh -x $sshopts $machname bash --login -c \'"cd $curdir && (csh -ef < $cshname >& $pidname ; \rm -f $cshname)"\' >&! $sshname &
      command = new QString("ssh");
      //To run remote command: bash --login -c '"command"'
      //Escape spaces in the directory path
      //Escaping the single quote shouldn't be necessary because this is not
      //being run from a shell.
      // DNM Note: the rm prevents the ssh from passing on the exit status of the script
      // So now processchunks removes .csh for nonlocal jobs too
      /* CSH -> PY */
      QString param = QString("\"cd %1 && python -u < %2\"").arg(
          mEscapedRemoteDirPath,
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

// Save flage when initiating kill.  Single kill job should still be valid, but don't
// ignore it regardless
void ProcessHandler::startKill(bool killOne) {
  mKill = true;
  mIgnoreKill = !killOne && !isJobValid();
  mKillingOne = killOne;
}

/*
 Handles the kill signal when imodkillgroup isn't used.  If this a queue and
 the kill request has already been sent, this process waits for it to finish.
 Currently imodkillgroup is used for everything but queues
 */
void ProcessHandler::killSignal() {
  if (mIgnoreKill || (mKillFinishedSignalReceived && mFinishedSignalReceived) ||
      !mProcesschunks->isQueue()) {
    return;
  }
  if (!mKillStarted) {
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
    //Waiting for kill to finish
    mKillCounter++;
    if (mKillCounter > 15 && (!mKillFinishedSignalReceived || !mFinishedSignalReceived)) {
      mKillProcess->kill();
      mProcesschunks->decrementKills();
      mKillFinishedSignalReceived = true;
      mFinishedSignalReceived = true;
    }
  }
}

void ProcessHandler::resetKill() {
  mKillFinishedSignalReceived = false;
  mKillStarted = false;
  mKillCounter = 0;
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

//Sets signal variables.  For a non-queue removes the .csh file.
//If the process was killed, tell processchunks that its done.
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

  // record the actual elapsed time when the process ends
  if (mElapsedTime < 0)
    mElapsedTime = mStartTime.elapsed();
  //The queue request just submits the chunk to the queue, or submits a request
  //to kill the chunk to the queue.  Don't use it to figure out the state of the
  //chunk.
  if (!mProcesschunks->isQueue()) {
    mProcesschunks->getCurrentDir().remove(
          mProcesschunks->getComFileJobs()->getCshFileName(mComFileJobIndex));
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
    *mOutStream << "ERROR: Job index not set" << endl;
    return;
  }
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    *mOutStream << mDecoratedClassName << ":" << __func__ << ":"
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
      *mOutStream << "kill process exitCode:" << exitCode << endl;
      QByteArray byteArray = mKillProcess->readAllStandardError();
      if (!byteArray.isEmpty()) {
        *mOutStream << byteArray << endl;
      }
    }
  }
}

void ProcessHandler::handleError(const QProcess::ProcessError processError) {
  if (mKill) {
    // local tree killing was used - causes a return code of 1
    return;
  }
  if (mComFileJobIndex == -1) {
    *mOutStream << "ERROR: Job index not set" << endl;
    return;
  }
  mErrorSignalReceived = true;
  mProcessError = processError;
  *mOutStream << mProcesschunks->getComFileJobs()->getComFileName(
      mComFileJobIndex) << ":process error:" << processError << ","
      << mProcess->errorString() << endl;
  *mOutStream << "exitCode:" << mProcess->exitCode() << ",exitStatus:"
      << mProcess->exitStatus() << ",state:" << mProcess->state() << endl;
}

// 6/17/13: Removed killLocalProcessAndDescendents and StopProcess
