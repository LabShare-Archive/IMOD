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
  mKillCpuIndex = -1;
  mKill = false;
  mDrop = false;
  mKillFinishedSignalReceived = false;
  mIgnoreKill = true;
  mKillStarted = false;
  mPidsAvailable = false;
  mKillCounter = 0;
  mDecoratedClassName = typeid(*this).name();
  mKillProcess = new QProcess(this);
  mDropped = false;
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
    const int numCpus) {
  int i;
  mName = machineName;
  mNumCpus = numCpus;
  mProcesschunks = &processchunks;
  mProcessHandlerArray = new ProcessHandler[mNumCpus];
  for (i = 0; i < mNumCpus; i++) {
    mProcessHandlerArray[i].setup(processchunks);
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
  //For remote machines imodkillgroup can be used to kill all the processes on
  //a machine.
  if (mName != mProcesschunks->getHostRoot() && mName != "localhost"
      && !mProcesschunks->isQueue()) {
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
      if (mPidsAvailable) {
        //PIDs are available and there are valid jobs - check for pipes
        if (mProcesschunks->resourcesAvailableForKill()) {
          mProcesschunks->getOutStream() << "Killing jobs on " << mName << endl;
          mKillStarted = true;//This starts the 15-count timeout
          //Get PIDs and run imodkillgroup
          //Kill a remote jobs in the background
          //ssh -x $sshopts $machname bash --login -c \'"imodkillgroup $pid ; \rm -f $curdir/$pidname"\' &
          mProcesschunks->incrementKills();
          QString command = "ssh";
          QString param = "\"imodkillgroup";
          for (i = 0; i < mNumCpus; i++) {
            if (mProcessHandlerArray[i].isJobValid()) {
              mProcessHandlerArray[i].setJobNotDone();
              param.append(" ");
              param.append(mProcessHandlerArray[i].getPid());
            }
          }
          param.append("\"");
          QStringList paramList;
          paramList << "-x" << mProcesschunks->getSshOpts() << mName << "bash"
              << "--login" << "-c" << param;
          mKillProcess->start(command, paramList);
          b3dMilliSleep(mProcesschunks->getMillisecSleep());
          param.clear();
          paramList.clear();
        }
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
    //Local machine or queue - pass kill signal to process handlers
    for (i = 0; i < mNumCpus; i++) {
      mProcessHandlerArray[i].killSignal();
    }
  }
}

void MachineHandler::handleFinished(const int exitCode,
    const QProcess::ExitStatus exitStatus) {
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    mProcesschunks->getOutStream() << "exitCode:" << exitCode << ",exitStatus:"
        << exitStatus << endl;
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
  if (mName != mProcesschunks->getHostRoot() && mName != "localhost"
      && !mProcesschunks->isQueue()) {
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

void MachineHandler::resetKill() {
  int i;
  mIgnoreKill = true;
  mKillFinishedSignalReceived = false;
  mKillStarted = false;
  mPidsAvailable = false;
  mKillCounter = 0;
  for (i = 0; i < mNumCpus; i++) {
    mProcessHandlerArray[i].resetKill();
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

//Kill all running processes on this machine.  Returns false if one of the kill
//requests had to wait for a signal or event.
bool MachineHandler::killProcesses() {
  //set mKill boolean
  mKill = true;
  if (mProcesschunks->getAns() == 'D') {
    mKill = false;
    if (!mProcesschunks->getDropList().isEmpty()
        && mProcesschunks->getDropList().contains(mName)) {
      mKill = true;
      mDrop = true;
      mFailureCount = mProcesschunks->getDropCrit();
      mProcesschunks->getOutStream() << "Dropping " << mName << " as requested" << endl;
    }
  }
  if (!mKill) {
    return true;
  }
  return killNextProcess(false);
}

//Tells processes to send kill requests.  Stops when a process has to give up
//control to the event loop.  If there are no more processes, starts
//cleaning up.
//This is called the first time by Processchunks::killProcessOnNextMachine.
//If that call can't get though all the processes because of a signal or event
//wait, it returns false and is then called by ProcessHandler::killNextProcess.
bool MachineHandler::killNextProcess(const bool asynchronous) {
  if (!mKill) {
    mProcesschunks->getOutStream()
        << "Warning: MachineHandler::killNextProcess called when mKill is false" << endl;
    return false;
  }
  //Kill the process on each cpu
  mKillCpuIndex++;
  if (mKillCpuIndex < mNumCpus) {
    while (mKillCpuIndex < mNumCpus) {
      if (mProcessHandlerArray[mKillCpuIndex].getAssignedJobIndex() != -1) {
        if (mDrop) {
          mProcessHandlerArray[mKillCpuIndex].invalidateJob();
        }
        //Get the process handler using the assigned process index and to it to
        //kill its process.
        if (!mProcessHandlerArray[mKillCpuIndex].killProcess()) {
          return false;
        }
      }
      mKillCpuIndex++;
    }
  }
  if (mKillCpuIndex >= mNumCpus) {
    if (asynchronous) {
      //This machine is done - go on to the next machine
      mProcesschunks->killProcessOnNextMachine();
    }
  }
  return true;
}

void MachineHandler::cleanupKillProcess() {
  mKillCpuIndex = -1;
  mDrop = false;
  mKill = false;
}

/*
 $Log$
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
