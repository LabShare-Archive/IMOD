/*
 * machinehandler.cpp
 *
 *  Created on: Jun 23, 2010
 *      Author: sueh
 */

#include <machinehandler.h>
#include <QString>
#include <QTime>

MachineHandler::MachineHandler(char *machineName, int numCpus) {
  mName = new QString(machineName);
  mNumCpus = numCpus;
}

MachineHandler::MachineHandler(QString &machineName) {
  mName = new QString(machineName);
  mNumCpus = 1;
}

MachineHandler::~MachineHandler() {
}

//Set mName and mNumCpus, and reset arrays based on mNumCpus
void MachineHandler::setValues(char *machineName, int numCpus) {
  mName = new QString(machineName);
  mNumCpus = numCpus;
}

//Increments mNumCpus.
void MachineHandler::incrementNumCpus() {
  mNumCpus++;
}

//Sets the arrays based on mNumCpus
//Call this once when mNumCpus will no long change
void MachineHandler::setup() {
  int i;
  mAssigned = new bool[mNumCpus];
  mStartTime = new QTime[mNumCpus];
  reset();
}

void MachineHandler::reset() {
  int i;
  for(i=0;i<mNumCpus;i++){
    mAssigned[i]=false;
  }
  mFailed = false;
  mChunkErred = false;
}

long MachineHandler::nameToLong(bool *ok) {
  return mName->toLong(ok);
}

char *MachineHandler::getName() {
  return mName->toAscii().data();
}

//Compares mName to a QString
bool MachineHandler::operator==(QString &other) {
  return *mName == other;
}

