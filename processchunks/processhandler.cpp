/*
 * processhandler.cpp
 *
 *  Created on: Jun 3, 2010
 *      Author: sueh
 */
#include <processhandler.h>
#include <QTextStream>
#include <QDir>
#include <parse_params.h>
#include <QFile>
#include "b3dutil.h"

const static QString pidTag = "PID:";

ProcessHandler::ProcessHandler() {
  mJobFile = NULL;
  mQidFile = NULL;
  mLogFileExists = false;
  mErrorSignalReceived = false;
  mProcessError = -1;
  mStartedSignalReceived = false;
  mFinishedSignalReceived = false;
  mNumChunkErr = 0;
  mPidTimerId = 0;
  mProcesschunks = NULL;
  mMachine = NULL;
  mProcessIndex = -1;
  mProcess = NULL;
  mKillProcess = NULL;
  mStartTime.start();
  mStartingProcess = false;
  mRanContinueKillProcess = false;
  mVmstocsh = new QProcess(this);
  mKillProcess = new QProcess(this);
  mKillProcess->setProcessChannelMode(QProcess::ForwardedChannels);
  QObject::connect(mKillProcess, SIGNAL(finished(int, QProcess::ExitStatus)),
      SLOT(handleKillFinished(int, QProcess::ExitStatus)));
  mProcess = NULL;
  mKillFinishedSignalReceived = false;
  mKill = false;
  resetSignalValues();
  mDecoratedClassName = typeid(*this).name();
}

ProcessHandler::~ProcessHandler() {
}

void ProcessHandler::initProcess() {
  if (mProcess != NULL) {
    disconnect(mProcess, SIGNAL(error(QProcess::ProcessError)), this,
        SLOT(handleError(QProcess::ProcessError)));
    disconnect(mProcess, SIGNAL(started()), this, SLOT(handleStarted()));
    disconnect(mProcess, SIGNAL(finished(int, QProcess::ExitStatus)), this,
        SLOT(handleFinished(int, QProcess::ExitStatus)));
    disconnect(mProcess, SIGNAL(readyReadStandardError()), this,
        SLOT(handleReadyReadStandardError()));
    disconnect(mProcess, SIGNAL(readyReadStandardOutput()), this,
        SLOT(handleReadyReadStandardOutput()));
    disconnect(mKillProcess, SIGNAL(finished(int, QProcess::ExitStatus)), this,
        SLOT(handleKillFinished(int, QProcess::ExitStatus)));
    delete mProcess;
  }
  mProcess = new QProcess(this);
  QObject::connect(mProcess, SIGNAL(error(QProcess::ProcessError)),
      SLOT(handleError(QProcess::ProcessError)));
  QObject::connect(mProcess, SIGNAL(started()), SLOT(handleStarted()));
  QObject::connect(mProcess, SIGNAL(finished(int, QProcess::ExitStatus)),
      SLOT(handleFinished(int, QProcess::ExitStatus)));
  QObject::connect(mProcess, SIGNAL(readyReadStandardError()),
      SLOT(handleReadyReadStandardError()));
  QObject::connect(mProcess, SIGNAL(readyReadStandardOutput()),
      SLOT(handleReadyReadStandardOutput()));
  //Add $IMOD_DIR to the processes path
  mProcess->setEnvironment(mProcesschunks->getEnv());
}

//Set mFlag to -1 for sync com files.
//Return true for non-sync files.
void ProcessHandler::setup(Processchunks &processchunks,
    const QString &comFile, const int processIndex) {
  mProcesschunks = &processchunks;
  mComFileName = comFile;
  mProcessIndex = processIndex;
  mEscapedRemoteDirPath = mProcesschunks->getRemoteDir();
  mEscapedRemoteDirPath.replace(QRegExp(" "), "\\ ");
  setFlagNotDone();
  //Set mRoot
  int i = mComFileName.lastIndexOf(".");
  if (i != -1) {
    mRoot = mComFileName.mid(0, i);
  }
  else {
    mRoot = mComFileName;
  }
  QString fileName = QString("%1.log").arg(mRoot);
  mLogFile = new QFile(fileName);
  fileName.clear();
  fileName.append("%1.csh");
  fileName = fileName.arg(mRoot);
  mCshFile = new QFile(fileName);
  if (mProcesschunks->isQueue()) {
    fileName.clear();
    fileName.append("%1.job");
    fileName = fileName.arg(mRoot);
    mJobFile = new QFile(fileName);
    fileName.clear();
    fileName.append("%1.qid");
    fileName = fileName.arg(mRoot);
    mQidFile = new QFile(fileName);
    //Queue command
    //finishes after putting things into the queue
    //$queuecom -w "$curdir" -a R $comname:r
    mCommand = mProcesschunks->getQueueCommand();
    mParamList << mProcesschunks->getQueueParamList() << "-w"
        << mEscapedRemoteDirPath << "-a" << "R" << mRoot;
  }
  else {
    //Local host command
    //csh -ef < $cshname >& $pidname ; \rm -f $cshname &
    mCommand = "csh";
    mParamList << "-ef";
  }
  initProcess();
}

void ProcessHandler::setFlagNotDone() {
  if (mProcesschunks->isSingleFile() || mComFileName.endsWith("-start.com")
      || mComFileName.endsWith("-finish.com") || mComFileName.endsWith(
      "-sync.com")) {
    mFlag = sync;
  }
  else {
    mFlag = notDone;
  }
}

void ProcessHandler::resetSignalValues() {
  mFinishedSignalReceived = false;
  mErrorSignalReceived = false;
  mStartedSignalReceived = false;
  mExitCode = -1;
  mExitStatus = -1;
  mProcessError = -1;
}

const ProcessHandler::FlagType ProcessHandler::getFlag() {
  return mFlag;
}

const bool ProcessHandler::logFileExists(const bool newlyCreatedFile) {
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
    mLogFileExists = mProcesschunks->getCurrentDir().exists(
        mLogFile->fileName());
  }
  return mLogFileExists;
}

const bool ProcessHandler::qidFileExists() {
  if (mProcesschunks->isQueue()) {
    return mProcesschunks->getCurrentDir().exists(mQidFile->fileName());
  }
  return true;
}

//Looks for PID in either stdout/stderr (non-queue) or in .qid file (queue).
const QString &ProcessHandler::getPid() {
  if (!mProcesschunks->isQueue()) {
    readAllStandardError();
    if (!getPid(new QTextStream(mStderr))) {
      readAllStandardOutput();
      getPid(new QTextStream(mStdout));
    }
  }
  else {
    if (!mQidFile->open(QIODevice::ReadOnly)) {
      return mPid;
    }
    getPid(new QTextStream(mQidFile));
  }
  return mPid;
}

const bool ProcessHandler::getPid(QTextStream *stream) {
  if (!mPid.isEmpty()) {
    //Don't look for the PID more then once
    return true;
  }
  if (stream == NULL) {
    return false;
  }
  //Don't set the PID unless the line is conplete (includes an EOL).  Process
  //may not be finished when this function runs.
  QString output = stream->readAll();
  //Look for a PID entry with an EOL so the the complete PID is collected
  int index = output.lastIndexOf("PID:");
  if (index != -1) {
    int endIndex;
#ifdef _WIN32
    endIndex = output.indexOf('\r\n', index);
#else
    endIndex = output.indexOf('\n', index);
#endif
    if (endIndex != -1) {
      mPid = output.mid(index + 4, endIndex - (index + 4));
      return true;
    }
  }
  return false;
}

const QByteArray ProcessHandler::readAllLogFile() {
  QByteArray log;
  if (!mLogFile->open(QIODevice::ReadOnly)) {
    return log;
  }
  log = mLogFile->readAll();
  mLogFile->close();
  return log;
}

const bool ProcessHandler::isLogFileEmpty() {
  return mLogFile->size() == 0;
}

void ProcessHandler::readAllStandardError() {
  QByteArray err = mProcess->readAllStandardError();
  if (!err.isEmpty()) {
    mStderr.append(err);
    if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
      mProcesschunks->getOutStream() << err.data() << endl;
    }
  }
}

void ProcessHandler::readAllStandardOutput() {
  QByteArray out = mProcess->readAllStandardOutput();
  if (!out.isEmpty()) {
    mStdout.append(out);
    if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
      mProcesschunks->getOutStream() << out.data() << endl;
    }
  }
}

//Looks for cd or ssh error in either stdout/stderr (non-queue) or
//.job file (queue).
//Returns true if found
const bool ProcessHandler::getSshError(QString &dropMess) {
  bool found = false;
  if (!mProcesschunks->isQueue()) {
    readAllStandardError();
    found = getSshError(dropMess, new QTextStream(mStderr));
    if (!found) {
      readAllStandardOutput();
      found = getSshError(dropMess, new QTextStream(mStdout));
    }
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
    found = getSshError(dropMess, new QTextStream(mJobFile));
  }
  return found;
}

const bool ProcessHandler::getSshError(QString &dropMess, QTextStream *stream) {
  //look for cd error & ssh error
  QString line = stream->readLine();
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
    line = stream->readLine();
  }
  return false;
}

//True when the .com file has been run and it has finished.  Returns true if
//the log exists and the finished signal has been received
const bool ProcessHandler::isComProcessDone() {
  if (mProcesschunks->isQueue()) {
    if (!cshFileExists() && logFileExists(true)) {
      mMachine = NULL;
      if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
        mProcesschunks->getOutStream() << mComFileName
            << ":isComProcessDone returning true (mMachine set to NULL)"
            << endl;
      }
      return true;
    }
    else {
      return false;
    }
  }
  bool done = mFinishedSignalReceived && logFileExists(true);
  if (done && mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    mProcesschunks->getOutStream() << mComFileName
        << ":isComProcessDone returning " << done << endl;
  }
  return done;
}

const bool ProcessHandler::isFinishedSignalReceived() {
  return mFinishedSignalReceived;
}

//Returns true if a last line of the log file starts with "CHUNK DONE"
const bool ProcessHandler::isChunkDone() {
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
  if (done && mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    mProcesschunks->getOutStream() << mComFileName << ":isChunkDone returning "
        << done << ",lastPartOfFile:" << lastPartOfFile << endl;
  }
  return done;
}

//Reads the last 1000 characters of the file.  Returns all the text between
//with "ERROR:" and the end of the file.
void ProcessHandler::getErrorMessageFromLog(QString &errorMess) {
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

//Reads the last lines of stdout and stderr and appends then to errorMess..
void ProcessHandler::getErrorMessageFromOutput(QString &errorMess) {
  //Use the last lines of stdout and stderr as the error message if the log
  //file is empty.
  char eol;
#ifdef _WIN32
  eol = '\r\n';
#else
  eol = '\n';
#endif
  errorMess.append(eol);
  //stdout
  readAllStandardOutput();
  QByteArray output = mStdout.trimmed();
  int lastLineIndex;
  if (!output.isEmpty()) {
    lastLineIndex = output.lastIndexOf(eol);
    if (lastLineIndex != -1) {
      errorMess.append(output.mid(lastLineIndex));
    }
    else {
      errorMess.append(mStdout);
    }
  }
  //stderr
  readAllStandardError();
  output = mStderr.trimmed();
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
  mProcesschunks->getOutStream() << getComFileName()
      << " has given processing error " << numErr
      << " times - giving up:  mExitCode:" << mExitCode << "mExitStatus:"
      << mExitStatus << endl;
}

void ProcessHandler::incrementNumChunkErr() {
  mNumChunkErr++;
}

void ProcessHandler::printWarnings() {
  if (!mLogFile->open(QIODevice::ReadOnly)) {
    return;
  }
  QByteArray line = mLogFile->readLine();
  do {
    line = mLogFile->readLine();
    if (line.indexOf("WARNING:") != -1) {
      mProcesschunks->getOutStream() << line;
    }
  } while (!mLogFile->atEnd());
  mLogFile->close();
}

const bool ProcessHandler::cshFileExists() {
  return mProcesschunks->getCurrentDir().exists(mCshFile->fileName());
}

const int ProcessHandler::getNumChunkErr() {
  return mNumChunkErr;
}

const QString ProcessHandler::getComFileName() {
  return mComFileName;
}

const QString ProcessHandler::getLogFileName() {
  return mLogFile->fileName();
}

//Returns true if the process has started, but the log file hasn't been create,
//and the timeout (milliseconds) has been exceeded.
const bool ProcessHandler::isStartProcessTimedOut(const int timeout) {
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

void ProcessHandler::setFlag(const FlagType flag) {
  mFlag = flag;
}

void ProcessHandler::backupLog() {
  imodBackupFile(mLogFile->fileName().toLatin1().data());
}

void ProcessHandler::removeFiles() {
  mProcesschunks->getCurrentDir().remove(mLogFile->fileName());
  mProcesschunks->getCurrentDir().remove(mCshFile->fileName());
  removeProcessFiles();
}

void ProcessHandler::removeProcessFiles() {
  if (mProcesschunks->isQueue()) {
    mProcesschunks->getCurrentDir().remove(mJobFile->fileName());
    mProcesschunks->getCurrentDir().remove(mQidFile->fileName());
  }
  else {
    mStderr.clear();
    mStdout.clear();
    mPid.clear();
  }
}

//Return if csh file is made
void ProcessHandler::makeCshFile() {
  int i;
  mProcesschunks->getCurrentDir().remove(mCshFile->fileName());
  QTextStream writeStream(mCshFile);
  if (!mCshFile->open(QIODevice::WriteOnly)) {
    mProcesschunks->getOutStream() << "Warning: unable to open and create "
        << mCshFile->fileName() << endl;
    return;
  }
  if (!mProcesschunks->isQueue()) {
    writeStream << "nice +" << mProcesschunks->getNice() << endl;
  }
  //convert and add CHUNK DONE to all files
  mVmstocsh->setStandardInputFile(mComFileName);
  //This does not work:
  //mVmstocsh->setStandardOutputFile(mCshFile->fileName(), QIODevice::Append);
  QString command("vmstocsh");
  QStringList paramList;
  paramList.append(mLogFile->fileName());
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    mProcesschunks->getOutStream() << "running: " << command << " "
        << paramList.join(" ") << endl << "input file: " << mComFileName
        << endl;
  }
  mVmstocsh->start(command, paramList);
  if (!mVmstocsh->waitForFinished(15 * 1000)) {
    mProcesschunks->getOutStream() << "Warning: vmstocsh conversion of "
        << mComFileName << " did not finish " << endl;
  }
  else if (mVmstocsh->exitStatus() == QProcess::CrashExit) {
    mProcesschunks->getOutStream() << "Warning: vmstocsh conversion of "
        << mComFileName << " failed with exit code " << mVmstocsh->exitCode()
        << " " << mVmstocsh->readAllStandardError().data() << endl;
  }
  writeStream << mVmstocsh->readAllStandardOutput().data()
      << "echo CHUNK DONE >> " << mLogFile->fileName() << endl;

  mCshFile->close();
}

void ProcessHandler::runProcess(MachineHandler *machine) {
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__) && machine
      != NULL) {
    mProcesschunks->getOutStream() << "runProcess:machine:"
        << machine->getName() << endl;
  }
  mMachine = machine;
  int i;
  //Build command if necessary
  QString *command = NULL;
  QStringList *paramList = NULL;
  if (!mProcesschunks->isQueue()) {
    if (mMachine->getName() != mProcesschunks->getHostRoot()
        && mMachine->getName() != "localhost") {
      //Create remove command
      //Original command:
      //ssh -x $sshopts $machname bash --login -c \'"cd $curdir && (csh -ef < $cshname >& $pidname ; \rm -f $cshname)"\' >&! $sshname &
      command = new QString("ssh");
      //To run remote command: bash --login -c '"command"'
      //Escape spaces in the directory path
      //Escaping the single quote shouldn't be necessary because this is not
      //being run from a shell.
      QString param = QString("\"cd %1 && (csh -ef < %2 ; \\rm -f %3)\"").arg(
          mEscapedRemoteDirPath, mCshFile->fileName(), mCshFile->fileName());
      paramList = new QStringList();
      paramList->append("-x");
      QStringList sshOpts = mProcesschunks->getSshOpts();
      for (i = 0; i < sshOpts.size(); i++) {
        paramList->append(sshOpts.at(i));
      }
      *paramList << mMachine->getName() << "bash" << "--login" << "-c" << param;
    }
    else {
      //Use local command - which doesn't contain a remove command.
      mProcess->setStandardInputFile(mCshFile->fileName());
    }
  }
  //Run command
  resetSignalValues();
  if (command != NULL) {
    if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
      mProcesschunks->getOutStream() << "running remote command:" << endl
          << *command << " " << paramList->join(" ") << endl;
    }
    //Run on a remote machine
    mProcess->start(*command, *paramList);
    delete command;
    delete paramList;
  }
  else {
    if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
      mProcesschunks->getOutStream() << "running:" << endl << mCommand << " "
          << mParamList.join(" ") << endl;
    }
    //Run on local machine or queue
    mProcess->start(mCommand, mParamList);
    if (mProcesschunks->isQueue()) {
      mProcess->waitForFinished(2000);
    }
  }
  //Turn on running process boolean and record start time
  mStartingProcess = true;
  mStartTime.restart();
}

//Kill the process.  Returns false if it started the timer and exited instead of
//called continueKillProcess.
const bool ProcessHandler::killProcess() {
  if (mMachine == NULL) {
    //Nothing to do - no process is running
    return true;
  }
  mKill = true;
  //Tell processchunks that a process is being killed
  mProcesschunks->msgKillProcessStarted(mProcessIndex);
  mFlag = notDone;
  mRanContinueKillProcess = false;
  //Not doing the ls to handle the old RHEL5 bug - bug should be fixed
  if (mProcesschunks->isQueue() && mProcesschunks->getAns() != 'D') {
    continueKillProcess(false);
  }
  else if (!mProcesschunks->isQueue()) {
    //Make sure that the pid has been retrieved
    getPid();
    if (!mPid.isEmpty()) {
      continueKillProcess(false);
    }
    else {
      //if the PID isn't there yet, wait for it for at most 15 seconds
      mPidTimerId = startTimer(15 * 1000);
      return false;
    }
  }
  else {
    continueKillProcess(false);
  }
  return true;
}

void ProcessHandler::handleReadyReadStandardError() {
  if (mKill && mPidTimerId != 0) {
    //If killing, check for pid
    getPid();
    if (!mPid.isEmpty()) {
      continueKillProcess(true);
    }
  }
}

void ProcessHandler::handleReadyReadStandardOutput() {
  if (mKill && mPidTimerId != 0) {
    //If killing, check for pid
    getPid();
    if (!mPid.isEmpty()) {
      continueKillProcess(true);
    }
  }
}

//Pid timer event
void ProcessHandler::timerEvent(const QTimerEvent *timerEvent) {
  //timer should only go off once
  if (mPidTimerId != 0) {
    killTimer(mPidTimerId);
    mPidTimerId = 0;
  }
  continueKillProcess(true);
}

//If waiting for the PID (if necessary) continue killing the process
void ProcessHandler::continueKillProcess(const bool asynchronous) {
  if (!mKill) {
    mProcesschunks->getOutStream()
        << "Warning: ProcessHandler::continueKillProcess called when mKill is false"
        << endl;
    return;
  }
  //function should only be run once
  if (mRanContinueKillProcess) {
    mProcesschunks->getOutStream()
        << "Warning: ProcessHandler::continueKillProcess called when mRanContinueKillProcess is true"
        << endl;
    return;
  }
  mRanContinueKillProcess = true;
  if (mPidTimerId != 0) {
    killTimer(mPidTimerId);
    mPidTimerId = 0;
  }
  bool runningKillProcess = false;
  char ans = mProcesschunks->getAns();
  if (mProcesschunks->isQueue() && ans != 'D') {
    QString action(ans);
    if (ans != 'P') {
      action = "K";
    }
    //Don't know if this waits until the kill is does
    //$queuecom -w "$curdir" -a $action $comlist[$ind]:r
    //The second to last parameter is the action letter
    mParamList.replace(mParamList.size() - 2, action);
    if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
      mProcesschunks->getOutStream() << mCommand << " " << mParamList.join(" ")
          << endl;
    }
    mKillProcess->start(mCommand, mParamList);
    runningKillProcess = true;
    if (!mKillProcess->waitForFinished(1000) && mProcesschunks->isVerbose(
        mDecoratedClassName, __func__)) {
      mProcesschunks->getOutStream() << "did not finish:error:"
          << mKillProcess->error() << ",exitCode:" << mKillProcess->exitCode()
          << ",exitStatus:" << mKillProcess->exitStatus() << ",state:"
          << mKillProcess->state() << endl
          << mKillProcess->readAllStandardError() << endl
          << mKillProcess->readAllStandardOutput() << endl;
    }
    //Put mParamList back to its regular form
    mParamList.replace(mParamList.size() - 2, "R");
  }
  else if (!mProcesschunks->isQueue()) {
    //Make sure that the pid has been retrieved
    getPid();
    if (!mPid.isEmpty()) {
      //the pid exists - continue killing the process
      mProcesschunks->getOutStream() << "Killing " << mComFileName << " on "
          << mMachine->getName() << endl;
      QString command;
      QStringList paramList;
      if (mMachine->getName() == mProcesschunks->getHostRoot()
          || mMachine->getName() == "localHost") {
        //local job
        killLocalProcessAndDescendents(mPid);
      }
      else {
        //Kill a remote job in background
        //ssh -x $sshopts $machname bash --login -c \'"imodkillgroup $pid ; \rm -f $curdir/$pidname"\' &
        command = "ssh";
        QString param = QString("\"imodkillgroup %1\"").arg(mPid);
        paramList << "-x" << mProcesschunks->getSshOpts()
            << mMachine->getName() << "bash" << "--login" << "-c" << param;
      }
      mKillProcess->start(command, paramList);
      if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
        mProcesschunks->getOutStream() << "running " << command << " "
            << paramList.join(" ") << endl;
      }
      runningKillProcess = true;
    }
  }
  if (asynchronous) {
    //MachineHandler::killNextProcesses is not currently running because a wait
    //for a signal or event was done.  Run this function to get to the next
    //process.
    mMachine->killNextProcesses();
  }
  if (!runningKillProcess) {
    //No kill request to wait for - go straight to clean up.
    //This happens when a queue receives a drop command or when the PID was
    //never received from a non-queue process.
    cleanupKillProcess();
  }
}

//Sets signal variables.  For a non-queue removes the .csh file on the local
//machine.  If the process was killed, calls cleanupKillProcess.
void ProcessHandler::handleFinished(const int exitCode,
    const QProcess::ExitStatus exitStatus) {
  mFinishedSignalReceived = true;
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    mProcesschunks->getOutStream() << "finished" << endl;
    readAllStandardOutput();
    readAllStandardError();
  }
  mStartingProcess = false;
  mExitCode = exitCode;
  mExitStatus = exitStatus;
  //The queue request just submits the chunk to the queue, or submits a request
  //to kill the chunk to the queue.  Don't use it to figure out the state of the
  //chunk.
  if (!mProcesschunks->isQueue()) {
    if (mMachine->getName() == mProcesschunks->getHostRoot()
        || mMachine->getName() == "localhost") {
      mProcesschunks->getCurrentDir().remove(mCshFile->fileName());
    }
    if (mKill) {
      cleanupKillProcess();
    }
    mMachine = NULL;
    if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
      mProcesschunks->getOutStream() << "machine set to NULL" << endl;
    }
  }
}

void ProcessHandler::cleanupKillProcess() {
  if (!mKill) {
    mProcesschunks->getOutStream()
        << "Warning: ProcessHandler::cleanupKillProcess called when mKill is false"
        << endl;
    return;
  }
  //Tell processchunks that a process is killed
  mProcesschunks->msgKillProcessDone(mProcessIndex);
  mProcessIndex = -1;
  mRanContinueKillProcess = false;
  if (!mKillFinishedSignalReceived) {
    mKillProcess->kill();
  }
  mKillFinishedSignalReceived = false;
  mKill = false;
}

//Called if kill process did not finish before the Processchunks timeout
void ProcessHandler::msgKillProcessTimeout() {
  if (!mKill) {
    mProcesschunks->getOutStream()
        << "Warning: ProcessHandler::msgKillProcessTimeout called when mKill is false"
        << endl;
    return;
  }
  //A process did not end.  Disconnect the signals and create new processes
  //to prevent slots from being run because of old processes.
  mStartingProcess = false;
  initProcess();
  resetSignalValues();
  mProcesschunks->getOutStream() << "Failed to kill " << mComFileName << " on "
      << mMachine->getName() << " (no problem if machine is dead)" << endl;
  mProcessIndex = -1;
  if (!mKillFinishedSignalReceived) {
    mKillProcess->kill();
  }
  mKillFinishedSignalReceived = false;
  mKill = false;
}

void ProcessHandler::handleKillFinished(const int exitCode,
    const QProcess::ExitStatus exitStatus) {
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    mProcesschunks->getOutStream() << "handleKillFinished: " << mComFileName
        << ",exitCode:" << exitCode << ",exitStatus:" << exitStatus << endl;
  }
  mKillFinishedSignalReceived = true;
  if (exitCode) {
    if (exitCode == 100) {
      //Process finished before it could be killed
      cleanupKillProcess();
    }
    else if (exitCode == 101) {
      //Unable to pause because the process had already started.
      cleanupKillProcess();
    }
    else {
      mProcesschunks->getOutStream() << "kill process exitCode:" << exitCode
          << endl;
      QByteArray byteArray = mKillProcess->readAllStandardOutput();
      if (!byteArray.isEmpty()) {
        mProcesschunks->getOutStream() << byteArray << endl;
      }
      byteArray = mKillProcess->readAllStandardError();
      if (!byteArray.isEmpty()) {
        mProcesschunks->getOutStream() << byteArray << endl;
      }
    }
  }
  else {
    //kill is completed
    cleanupKillProcess();
  }
}

void ProcessHandler::handleError(const QProcess::ProcessError processError) {
  mErrorSignalReceived = true;
  mProcessError = processError;
}

void ProcessHandler::handleStarted() {
  mStartedSignalReceived = true;
}

//Stop and then kill the process with process ID pid and all of its descendents.
//Waits for kill commands to complete.
void ProcessHandler::killLocalProcessAndDescendents(QString &pid) {
  //immediately stop the process
  stopProcess(pid);
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    mProcesschunks->getOutStream() << "stopping " << pid << endl;
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
            mProcesschunks->getOutStream() << "ps column header:" << endl
                << header << endl;
          }
          QStringList headerList = header.split(QRegExp("\\s+"),
              QString::SkipEmptyParts);
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
            QStringList columns = line.split(QRegExp("\\s+"),
                QString::SkipEmptyParts);
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
                  mProcesschunks->getOutStream() << "stopping:" << endl << line
                      << endl;
                }
              }
            }
          } while (!stream.atEnd());
        }
        else {
          mProcesschunks->getOutStream()
              << "Warning: May not have been able to kill all processes descendent from "
              << mCommand << " " << mCshFile
              << " on local machine.  Ps PID and PPID columns where not found in "
              << header << "(" << command << " " << paramList.join(" ") << ")."
              << endl;
          return;
        }
      }
      else {
        mProcesschunks->getOutStream()
            << "Warning: May not have been able to kill all processes descendent from "
            << mCommand << " " << mCshFile
            << " on local machine.  Ps command return nothing" << "("
            << command << " " << paramList.join(" ") << ")." << endl;
      }
    }
    else {
      mProcesschunks->getOutStream()
          << "Warning: May not have been able to kill all processes descendent from "
          << mCommand << " " << mCshFile
          << " on local machine.  Ps command failed" << "(" << command << " "
          << paramList.join(" ") << ")." << endl;
      return;
    }
  } while (foundNewChildPid);
  //Kill everything in the process ID list.
  for (i = 0; i < pidList.size(); i++) {
    killProcess(pidList.at(i));
  }
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

//Kill a single process.  Waits for process to complete.
void ProcessHandler::killProcess(const QString &pid) {
  QProcess ps;
  QString command("kill");
  QStringList paramList;
  paramList.append("-9");
  paramList.append(pid);
  ps.execute(command, paramList);
}

