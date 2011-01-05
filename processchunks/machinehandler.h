/*
 *  Description of a computer or a queue.
 *
 *  Manages processes for one or more CPUs.
 *
 *  Contains a list of ProcessHandlers based on the number of CPUs that was
 *  passed to the application.
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

#ifndef MACHINEHANDLER_H_
#define MACHINEHANDLER_H_

class MachineHandler {
public:
  MachineHandler(Processchunks &processchunks, const char *machineName,
      const int numCpus);
  MachineHandler(Processchunks &processchunks, const QString &machineName);
  ~MachineHandler();

  inline const long nameToLong(bool *ok) {
    return mName.toLong(ok);
  }
  ;
  void setValues(const char *machineName, const int numCpus);
  inline void incrementNumCpus() {
    mNumCpus++;
  }
  ;
  inline const QString &getName() {
    return mName;
  }
  ;
  void setup();
  inline const int getNumCpus() {
    return mNumCpus;
  }
  ;
  inline ProcessHandler *getProcessHandler(const int index) {
    return &mProcessHandlerArray[index];
  }
  ;
  inline const int getFailureCount() {
    return mFailureCount;
  }
  ;
  inline const bool isChunkErred() {
    return mChunkErred;
  }
  ;
  inline void setFailureCount(const int failureCount) {
    mFailureCount = failureCount;
  }
  ;
  inline void setChunkErred(const bool chunkErred) {
    mChunkErred = chunkErred;
  }
  ;
  const bool isTimedOut(const int index, const int timeoutMillisec);
  inline void incrementFailureCount() {
    mFailureCount++;
  }
  ;
  const bool killProcesses();
  void msgKillProcessTimeout();
  const bool killNextProcess();
  inline const bool isJobValid(const int index) {
    return mProcessHandlerArray[index].isJobValid();
  }
  ;

  MachineHandler &operator=(const MachineHandler &machineHandler);
  //Compares mName to a QString
  inline const bool operator==(const QString &other) {
    return mName == other;
  }
  ;

private:
  void init();
  void cleanupKillProcess();

  ProcessHandler *mProcessHandlerArray;
  QString mName, mDecoratedClassName;
  int mNumCpus, mFailureCount;
  bool mKill, mChunkErred;
  Processchunks *mProcesschunks;

  //killing processes
  bool mDrop;
  int mKillCpuIndex;
};

#endif /* MACHINEHANDLER_H_ */

/*
 $Log$
 */
