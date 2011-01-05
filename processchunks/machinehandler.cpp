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

MachineHandler::MachineHandler(Processchunks &processchunks,
    const char *machineName, const int numCpus) {
  mName = machineName;
  mNumCpus = numCpus;
  mProcesschunks = &processchunks;
  init();
}

MachineHandler::MachineHandler(Processchunks &processchunks,
    const QString &machineName) {
  mName = machineName;
  mNumCpus = 1;
  mProcesschunks = &processchunks;
  init();
}

void MachineHandler::init() {
  mFailureCount = 0;
  mChunkErred = false;
  mKillCpuIndex = -1;
  mKill = false;
  mDecoratedClassName = typeid(*this).name();
  mProcessHandlerArray = NULL;
  mDrop = false;
}

MachineHandler::~MachineHandler() {
  delete[] mProcessHandlerArray;
}

MachineHandler &MachineHandler::operator=(const MachineHandler &otherInstance) {
  mProcesschunks = otherInstance.mProcesschunks;
  mName = otherInstance.mName;
  mNumCpus = otherInstance.mNumCpus;
  mFailureCount = otherInstance.mFailureCount;
  mChunkErred = otherInstance.mChunkErred;
  if (mProcessHandlerArray != NULL) {
    delete[] mProcessHandlerArray;
    mProcessHandlerArray = NULL;
  }
  mProcessHandlerArray = otherInstance.mProcessHandlerArray;
  mKillCpuIndex = otherInstance.mKillCpuIndex;
  mKill = otherInstance.mKill;
  return *this;
}

//Set mName and mNumCpus, and reset arrays based on mNumCpus
void MachineHandler::setValues(const char *machineName, const int numCpus) {
  mName = machineName;
  mNumCpus = numCpus;
}

//Sets the arrays based on mNumCpus
//Call this once when mNumCpus will no long change
void MachineHandler::setup() {
  if (mProcessHandlerArray != NULL) {
    delete[] mProcessHandlerArray;
    mProcessHandlerArray = NULL;
  }
  mProcessHandlerArray = new ProcessHandler[mNumCpus];
  int i;
  for (i = 0; i < mNumCpus; i++) {
    mProcessHandlerArray[i].setup(*mProcesschunks);
  }
}

//Kill all running processes on this machine.  Returns false if one of the kill
//requests had to wait for a signal or event.
const bool MachineHandler::killProcesses() {
  //set mKill boolean
  mKill = true;
  if (mProcesschunks->getAns() == 'D') {
    mKill = false;
    if (!mProcesschunks->getDropList().isEmpty()
        && mProcesschunks->getDropList().contains(mName)) {
      mKill = true;
      mDrop = true;
      mFailureCount = mProcesschunks->getDropCrit();
      mProcesschunks->getOutStream() << "Dropping " << mName << " as requested"
          << endl;
    }
  }
  if (!mKill) {
    return true;
  }
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    mProcesschunks->getOutStream() << mDecoratedClassName << ":" << __func__
        << ":mKill:" << mKill << ",mDrop:" << mDrop << endl;
  }
  return killNextProcess();
}

//Tells processes to send kill requests.  Stops when a process has to give up
//control to the event loop.  If there are no more processes, starts
//cleaning up.
//This is called the first time by Processchunks::killProcessOnNextMachine.
//If that call can't get though all the processes because of a signal or event
//wait, it returns false and is then called by ProcessHandler::killNextProcess.
const bool MachineHandler::killNextProcess() {
  if (!mKill) {
    mProcesschunks->getOutStream()
        << "Warning: MachineHandler::killNextProcess called when mKill is false"
        << endl;
    return false;
  }
  if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
    mProcesschunks->getOutStream() << mDecoratedClassName << ":" << __func__
        << ":A:mKillCpuIndex:" << mKillCpuIndex << ",mNumCpus:" << mNumCpus
        << ",mDrop:" << mDrop << endl;
  }
  //Kill the process on each cpu
  mKillCpuIndex++;
  if (mKillCpuIndex < mNumCpus) {
    while (mKillCpuIndex < mNumCpus) {
      if (mProcessHandlerArray[mKillCpuIndex].getAssignedJobIndex() != -1) {
        if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
          mProcesschunks->getOutStream() << mDecoratedClassName << ":"
              << __func__ << ":B:mKillCpuIndex:" << mKillCpuIndex << endl;
        }
        if (mDrop) {
          mProcessHandlerArray[mKillCpuIndex].invalidateJob();
        }
        //Get the process handler using the assigned process index and to it to
        //kill its process.
        if (!mProcessHandlerArray[mKillCpuIndex].killProcess()) {
          if (mProcesschunks->isVerbose(mDecoratedClassName, __func__)) {
            mProcesschunks->getOutStream() << mDecoratedClassName << ":"
                << __func__ << ":C:killProcess:false" << endl;
          }
          return false;
        }
      }
      mKillCpuIndex++;
    }
  }
  if (mKillCpuIndex >= mNumCpus) {
    //This machine is done - go on to the next machine
    mProcesschunks->killProcessOnNextMachine();
    cleanupKillProcess();
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
 */
