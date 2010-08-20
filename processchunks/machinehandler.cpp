/*
 * machinehandler.cpp
 *
 *  Created on: Jun 23, 2010
 *      Author: sueh
 */

#include <machinehandler.h>
#include <QString>
#include <QTextStream>

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
  mDrop = false;
  mKill = false;
}

MachineHandler::~MachineHandler() {
}

MachineHandler &MachineHandler::operator=(const MachineHandler &otherInstance) {
  mProcesschunks = otherInstance.mProcesschunks;
  mName = otherInstance.mName;
  mNumCpus = otherInstance.mNumCpus;
  mFailureCount = otherInstance.mFailureCount;
  mChunkErred = otherInstance.mChunkErred;
  mAssignedProcIndexList = otherInstance.mAssignedProcIndexList;
  mKillCpuIndex = otherInstance.mKillCpuIndex;
  mKill = otherInstance.mKill;
  return *this;
}

//Set mName and mNumCpus, and reset arrays based on mNumCpus
void MachineHandler::setValues(const char *machineName, const int numCpus) {
  mName = machineName;
  mNumCpus = numCpus;
}

//Increments mNumCpus.
void MachineHandler::incrementNumCpus() {
  mNumCpus++;
}

//Sets the arrays based on mNumCpus
//Call this once when mNumCpus will no long change
void MachineHandler::setup() {
  mAssignedProcIndexList = new int[mNumCpus];
  int i;
  for (i = 0; i < mNumCpus; i++) {
    mAssignedProcIndexList[i] = -1;
  }
}

const int MachineHandler::getNumCpus() {
  return mNumCpus;
}

const int MachineHandler::getAssignedProcIndex(const int index) {
  return mAssignedProcIndexList[index];
}

void MachineHandler::setAssignedProcIndex(const int index,
    const int assignedProcIndex) {
  mAssignedProcIndexList[index] = assignedProcIndex;
}

void MachineHandler::setChunkErred(const bool chunkErred) {
  mChunkErred = chunkErred;
}

void MachineHandler::incrementFailureCount() {
  mFailureCount++;
}

const int MachineHandler::getFailureCount() {
  return mFailureCount;
}

void MachineHandler::setFailureCount(const int failureCount) {
  mFailureCount = failureCount;
}

const bool MachineHandler::isChunkErred() {
  return mChunkErred;
}

const long MachineHandler::nameToLong(bool *ok) {
  return mName.toLong(ok);
}

const QString &MachineHandler::getName() {
  return mName;
}

//Compares mName to a QString
const bool MachineHandler::operator==(const QString &other) {
  return mName == other;
}

//Kill all running processes on this machine.  Returns false if one of the kill
//requests had to wait for a signal or event.
const bool MachineHandler::killProcesses() {
  mKill = true;
  //set mKill and mDrop booleans
  mDrop = false;
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
  return killNextProcesses();
}

//Tells processes to send kill requests.  Stops when a process has to give up
//control to the event loop.  If there are no more processes, starts
//cleaning up.
//This is called the first time by Processchunks::killProcessOnNextMachines.
//If that call can't get though all the processes because of a signal or event
//wait, it returns false and is then called by ProcessHandler::killNextProcess.
const bool MachineHandler::killNextProcesses() {
  if (!mKill) {
    mProcesschunks->getOutStream()
        << "Warning: MachineHandler::killNextProcess called when mKill is false"
        << endl;
    return false;
  }
  //Kill the process on each cpu
  mKillCpuIndex++;
  if (mKillCpuIndex < mNumCpus) {
    while (mKillCpuIndex < mNumCpus) {
      if (mAssignedProcIndexList[mKillCpuIndex] != -1) {
        int processIndex = mAssignedProcIndexList[mKillCpuIndex];
        if (mDrop) {
          mAssignedProcIndexList[mKillCpuIndex] = -1;
        }
        //Get the process handler using the assigned process index and to it to
        //kill its process.
        if (!mProcesschunks->getProcessHandler(processIndex).killProcess()) {
          return false;
        }
      }
      mKillCpuIndex++;
    }
  }
  if (mKillCpuIndex >= mNumCpus) {
    //This machine is done - go on to the next machine
    mProcesschunks->killProcessOnNextMachines();
    cleanupKillProcess();
  }
  return true;
}

void MachineHandler::cleanupKillProcess() {
  mDrop = false;
  mKillCpuIndex = -1;
  mKill = false;
}
