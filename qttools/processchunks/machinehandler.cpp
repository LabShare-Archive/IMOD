/*
 *  Description of a computer or a queue.
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

MachineHandler::MachineHandler() {
  mFailureCount = 0;
  mChunkErred = false;
  mKill = false;
  mKillFinishedSignalReceived = false;
  mIgnoreKill = true;
  mKillStarted = false;
  mPidsAvailable = false;
  mKillCounter = 0;
  mPidWaitCounter = 0;
  mDecoratedClassName = typeid(*this).name();
  mKillProcess = new QProcess(this);
  mDropped = false;
  mKillWarning = false;
  mKillProcess->setProcessChannelMode(QProcess::ForwardedChannels);
  QObject::connect(mKillProcess, SIGNAL(finished(int, QProcess::ExitStatus)),
      SLOT(handleFinished(int, QProcess::ExitStatus)));
  QObject::connect(mKillProcess, SIGNAL(error(QProcess::ProcessError)),
      SLOT(handleError(QProcess::ProcessError)));
}

MachineHandler::~MachineHandler() {
  delete[] mProcessHandlerArray;
  delete mKillProcess;
}

//Sets the arrays based on mNumCpus
//Call this once when mNumCpus will no long change
void MachineHandler::setup(Processchunks &processchunks, const QString &machineName,
                           const int numCpus, const std::vector<int> &gpuList,
                           const int baseIndex) {
  int i;
  bool gpuMode = processchunks.getGpuMode();
  mName = machineName;
  mNumCpus = numCpus;
  mProcesschunks = &processchunks;
  mProcessHandlerArray = new ProcessHandler[mNumCpus];
  for (i = 0; i < mNumCpus; i++) {
    mProcessHandlerArray[i].setup(processchunks, gpuMode ? gpuList[baseIndex + i] : -1);
  }
}

//Set mName and mNumCpus, and reset arrays based on mNumCpus
void MachineHandler::setValues(const char *machineName, const int numCpus) {
  mName = machineName;
  mNumCpus = numCpus;
}

//Setup done before starting to kill processes.
void MachineHandler::startKill() {
  mIgnoreKill = false;
  if (mDropped) {
    mIgnoreKill = true;
    return;
  }
  int i;
  if (mProcesschunks->getAns() == 'D') {
    //Ignore kill signal if it is a drop command and this machine is not on the list.
    if (mProcesschunks->getDropList().isEmpty()
        || !mProcesschunks->getDropList().contains(mName)) {
      mIgnoreKill = true;
    }
    else {
      //Turn on dropped if this machine is on the drop list.
      mDropped = true;
    }
  }
  if (mIgnoreKill) {
    return;
  }
  //If any of the jobs are valid, then turn off mIgnoreKill.  Run startKill
  //for the valid jobs.
  mIgnoreKill = true;
  for (i = 0; i < mNumCpus; i++) {
    if (mProcessHandlerArray[i].isJobValid()) {
      mProcessHandlerArray[i].startKill();
      mIgnoreKill = false;
    }
  }
  if (!mIgnoreKill && mProcesschunks->isQueue() && mProcesschunks->getAns() == 'Q') {
    mProcesschunks->getOutStream() << "Killing jobs on " << mName << endl;
  }
}

/*
 Handles the kill signal.  If this is a remote machine, it checks for its
 process PIDs and runs imodkillgroup.  If imodkillgroup has already been run,
 the machine handler waits for it to finish.  If this is the local machine or a
 queue, it passes the kill signal to the process.
 */
void MachineHandler::killSignal() {
  int i;
  if (mIgnoreKill || mKillFinishedSignalReceived) {
    return;
  }
  if (useImodkillgroup()) {
    if (!mKillStarted) {
      if (!mPidsAvailable) {
        //See if PIDs are available
        mPidsAvailable = true;
        for (i = 0; i < mNumCpus; i++) {
          if (mProcessHandlerArray[i].isJobValid()
              && mProcessHandlerArray[i].isPidEmpty()) {
            mPidsAvailable = false;
          }
        }
      }
      if (mPidsAvailable || mPidWaitCounter > 15) {
        //PIDs are available (or there was a timeout) and there are valid jobs - check for pipes
        if (mProcesschunks->resourcesAvailableForKill()) {
          mProcesschunks->getOutStream() << "Killing jobs on " << mName << endl;
          mKillStarted = true;//This starts the 15-count timeout
          //Get PIDs and run imodkillgroup
          //Kill a remote jobs in the background
          //ssh -x $sshopts $machname bash --login -c \'"imodkillgroup $pid ; \rm -f $curdir/$pidname"\' &
          QStringList paramList;
          bool pidFound = false;
#ifndef _WIN32
          QString command = "ssh";
          QString param = "\"imodkillgroup";
          for (i = 0; i < mNumCpus; i++) {
            if (mProcessHandlerArray[i].isJobValid()
                && !mProcessHandlerArray[i].isPidEmpty()) {
              pidFound = true;
              mProcessHandlerArray[i].setJobNotDone();
              param.append(" ");
              param.append(mProcessHandlerArray[i].getPid());
            }
          }
          param.append("\"");
          paramList << "-x" << mProcesschunks->getSshOpts() << mName << "bash"
              << "--login" << "-c" << param;
#else
          QString command = "imodkillgroup.cmd";
          for (i = 0; i < mNumCpus; i++) {
            if (mProcessHandlerArray[i].isJobValid()
                && !mProcessHandlerArray[i].isPidEmpty()) {
              pidFound = true;
              mProcessHandlerArray[i].setJobNotDone();
              paramList << mProcessHandlerArray[i].getPid();
            }
          }
#endif
          if (pidFound) {
            mProcesschunks->incrementKills();
            mKillProcess->start(command, paramList);
            b3dMilliSleep(mProcesschunks->getMillisecSleep());
          }
          else {
            //Only one attempt is made to kill the processes.  If no PIDs are
            //found, then no further attempt will be made because killStarted
            //has been turned on.
            mKillWarning = true;
            mProcesschunks->getOutStream() << "Unable to kill any processes on " << mName
                << endl;
            mKillFinishedSignalReceived = true;
          }
          paramList.clear();
        }
      }
      else {
        mPidWaitCounter++;
      }
    }
    else {
      //Waiting for kill to finish
      mKillCounter++;
      if (mKillCounter > 15 && !mKillFinishedSignalReceived) {
        mKillProcess->kill();
        mProcesschunks->decrementKills();
        mKillFinishedSignalReceived = true;
      }
    }
  }
  else {
    //Local machine (not Windows) or queue - pass kill signal to process handlers
    for (i = 0; i < mNumCpus; i++) {
      mProcessHandlerArray[i].killSignal();
    }
  }
}

void MachineHandler::handleFinished(const int exitCode,
    const QProcess::ExitStatus exitStatus) {
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    mProcesschunks->getOutStream() << mDecoratedClassName << ":" << __func__
        << ":exitCode:" << exitCode << ",exitStatus:" << exitStatus << endl;
  }
  if (!mKillFinishedSignalReceived) {
    mProcesschunks->decrementKills();
    mKillFinishedSignalReceived = true;
  }
}

void MachineHandler::handleError(const QProcess::ProcessError error) {
  if (error == QProcess::FailedToStart || error == QProcess::Crashed) {
    if (!mKillFinishedSignalReceived) {
      mProcesschunks->decrementKills();
      mKillFinishedSignalReceived = true;
    }
  }
}

bool MachineHandler::isKillFinished() {
  int i;
  if (mIgnoreKill) {
    return true;
  }
  if (useImodkillgroup()) {
    bool processesFinished = true;
    for (i = 0; i < mNumCpus; i++) {
      if (!mProcessHandlerArray[i].isFinishedSignalReceived()) {
        processesFinished = false;
      }
    }
    if (processesFinished) {
      return true;
    }
    return mKillFinishedSignalReceived;
  }
  else {
    for (i = 0; i < mNumCpus; i++) {
      if (!mProcessHandlerArray[i].isKillFinished()) {
        return false;
      }
    }
    return true;
  }
}

bool MachineHandler::useImodkillgroup() {
#ifndef _WIN32
  return mName != mProcesschunks->getHostRoot() && mName != "localhost"
      && !mProcesschunks->isQueue();
#else
  return !mProcesschunks->isQueue();
#endif
}

void MachineHandler::resetKill() {
  int i;
  if (!mKillWarning && !mIgnoreKill && useImodkillgroup() && !mPidsAvailable) {
    mProcesschunks->getOutStream() << "No processes are running on " << mName << endl;
  }
  mIgnoreKill = true;
  mKillFinishedSignalReceived = false;
  mKillStarted = false;
  mPidsAvailable = false;
  mKillCounter = 0;
  mPidWaitCounter = 0;
  mKillWarning = false;
  for (i = 0; i < mNumCpus; i++) {
    mProcessHandlerArray[i].resetKill();
  }
}

void MachineHandler::killQProcesses() {
  int i;
  for (i = 0; i < mNumCpus; i++) {
    mProcessHandlerArray[i].killQProcesses();
    mKillProcess->kill();
  }
}

//When a machine is dropped, its failures no longer count.
int MachineHandler::getFailureCount() {
  if (mDropped) {
    return 0;
  }
  else {
    return mFailureCount;
  }
}

/*
 $Log$
 Revision 1.31  2011/07/29 04:21:53  sueh
 Bug# 1492 Added a verbose print to startKill.

 Revision 1.30  2011/02/05 00:50:21  sueh
 bug# 1426 Preventing a lockup when the PID cannot be gotten and
 processchunks thinks that the process is running.

 Revision 1.29  2011/02/04 00:19:34  sueh
 *** empty log message ***

 Revision 1.28  2011/02/04 00:11:54  sueh
 bug# 1426 Added useImodkillgroup().

 Revision 1.27  2011/02/03 23:53:49  sueh
 bug# 1426 Moved checking for process done signal to isKillFinished.

 Revision 1.26  2011/02/03 23:39:15  sueh
 bug# 1426 In kill signal check if process done.

 Revision 1.25  2011/02/02 22:42:24  sueh
 bug# 1426 Added killQProcesses.

 Revision 1.24  2011/02/02 00:08:52  sueh
 bug# 1426 Removed unused variables and commented-out code.

 Revision 1.23  2011/02/01 23:27:56  sueh
 bug# 1426 Changed a bad format in the Windows code.

 Revision 1.22  2011/02/01 23:20:41  sueh
 bug# 1426 Added verbose information.  Allowing for Windows difference
 in isKillFinished.

 Revision 1.21  2011/02/01 23:08:13  sueh
 bug# 1426 Fixed windows code in handleFinished.

 Revision 1.20  2011/02/01 23:01:41  sueh
 bug# 1426 In windows imodkillgroup causes processchunks to behave as
 if its timer is dead, so restarting it.

 Revision 1.19  2011/02/01 22:39:13  sueh
 bug# 1426 Removing old method of killing.

 Revision 1.18  2011/02/01 21:49:45  mast
 In killSignal, really not use ssh stuff, set up PIDs in separate strings in
 paramList, and call by imodkillgroup.cmd for Windows

 Revision 1.17  2011/02/01 21:07:56  sueh
 bug# 1426 In killSignal remove ssh when using imodkillgroup for a local machine.

 Revision 1.16  2011/02/01 20:20:31  sueh
 bug# 1426 In killSignal using imodkillgroup to kill local processes in Windows.

 Revision 1.15  2011/01/31 19:45:00  sueh
 bug# 1426 Counting kills instead of pipes.

 Revision 1.14  2011/01/27 03:50:16  sueh
 bug# 1426 Removes const from simple variable return values (int, char,
 bool, long) because they cause a warning in the intel compiler.  Moved the
 the kill message for queues to the machine handler so it will only print
 once.

 Revision 1.13  2011/01/25 07:05:49  sueh
 bug# 1426 Added mDropped.

 Revision 1.12  2011/01/21 04:54:49  sueh
 bug# 1426 In setup, passing the parameter to ProcessHandler setup.

 Revision 1.11  2011/01/21 00:13:21  sueh
 bug# 1426 Added handleError, isKillFinished, killSignal, resetKill, startKill.

 Revision 1.10  2011/01/05 20:45:38  sueh
 bug# 1426 Moved ProcessHandler instances to MachineHandler.  Moved
 one-line functions to .h file.

 */
