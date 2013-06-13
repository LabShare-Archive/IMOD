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
  mSlowestTime = -1;
  mSlowTimeCount = 0;
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
void MachineHandler::startKill(const QString onePid) {
  mIgnoreKill = false;
  if (mDropped) {
    mIgnoreKill = true;
    return;
  }
  int i;

  // Record whether this is a single kill here
  mKillingOne = !onePid.isEmpty();
  mOnePidToKill = onePid;
  if (!mKillingOne && mProcesschunks->getAns() == 'D') {
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
  //If any of the jobs are valid, or if the single kill is found, then turn off 
  // mIgnoreKill.  Run startKill for the valid jobs, which just sets kill flags
  mIgnoreKill = true;
  for (i = 0; i < mNumCpus; i++) {
    if ((mProcessHandlerArray[i].isJobValid() && !mKillingOne) ||
        (mKillingOne && onePid == mProcessHandlerArray[i].getPid())) {
      mProcessHandlerArray[i].startKill(mKillingOne);
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
  QStringList pidList;
  if (mIgnoreKill || mKillFinishedSignalReceived) {
    return;
  }
  if (useImodkillgroup()) {
    if (!mKillStarted) {
      if (mKillingOne)
        mPidsAvailable = true;
      if (!mPidsAvailable) {
        //See if PIDs are available unless we were supplied with one
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
          mProcesschunks->getOutStream() << "Killing " << 
            (mKillingOne ? "one job" : "jobs") << " on " << mName << endl;
          mKillStarted = true;//This starts the 15-count timeout
          //Get PIDs and run imodkillgroup
          //Kll a remote jobs in the background
          //ssh -x $sshopts $machname bash --login -c \'"imodkillgroup $pid ; \rm -f $curdir/$pidname"\' &

          // Make a list of PIDs first
          QStringList paramList;
          bool pidFound = false;
          for (i = 0; i < mNumCpus; i++) {
            if ((mProcessHandlerArray[i].isJobValid() && !mKillingOne
                 && !mProcessHandlerArray[i].isPidEmpty()) ||
                (mKillingOne && mProcessHandlerArray[i].getPid() == mOnePidToKill)) {
              pidFound = true;
              mProcessHandlerArray[i].setJobNotDone();
              pidList << mProcessHandlerArray[i].getPid();

              // Invalidate the job now, this is needed for a hung job/single kill
              mProcessHandlerArray[i].invalidateJob();
            }
          }

          // Then make OS-dependent commands
#ifndef _WIN32
          QString command = "ssh";
          QString param = "\"imodkillgroup";
          for (i = 0; i < pidList.size(); i++) {
            param.append(" ");
            param.append(pidList[i]);
          }
          param.append("\"");
          paramList << "-x" << mProcesschunks->getSshOpts() << mName << "bash"
                    << "--login" << "-c" << param;
#else
          QString command = "imodkillgroup.cmd";
          for (i = 0; i < pidList.size(); i++)
            paramList << pidList[i];
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
    // This is the call that really makes it happen
    for (i = 0; i < mNumCpus; i++) {
      if (!mKillingOne || mProcessHandlerArray[i].getPid() == mOnePidToKill)
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
      if ((!mKillingOne || mProcessHandlerArray[i].getPid() == mOnePidToKill)
          && !mProcessHandlerArray[i].isFinishedSignalReceived()) {
        processesFinished = false;
      }
    }
    if (processesFinished) {
      return true;
    }

    // When killing a single job, it is not good enough for the group kill to be done, 
    // have to get the finish signal through before restarting job if possible
    return mKillFinishedSignalReceived && (!mKillingOne || mKillCounter > 15);
  }
  else {
    for (i = 0; i < mNumCpus; i++) {
      if ((!mKillingOne || mProcessHandlerArray[i].getPid() == mOnePidToKill)
          && !mProcessHandlerArray[i].isKillFinished()) {
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

// Set the current slowest time from a new time and increment the counter
// Reset to -1 and reset counter if -1 is supplied
void MachineHandler::setSlowestTime(const int newTime) {
  if (newTime < 0 || newTime > mSlowestTime)
    mSlowestTime = newTime;
  if (newTime < 0)
    mSlowTimeCount = 0;
  else
    mSlowTimeCount++;
}
