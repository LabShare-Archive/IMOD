/*
 *  Description of a computer or a queue.
 *
 *  Contains a list of ProcessHandlers based on the number of CPUs requested
 *  for the computer or queue.  Passes kill process requests to its
 *  ProcessHandlers.
 *
 *  Author: Sue Held
 *
 *  Copyright (C) 2010,2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#ifndef MACHINEHANDLER_H_
#define MACHINEHANDLER_H_

#include <processchunks.h>
#include <QProcess>

class MachineHandler: public QObject {
Q_OBJECT

public:
  MachineHandler();
  ~MachineHandler();

  void setup(Processchunks &processchunks, const QString &machineName, const int numCpus,
             const std::vector<int> &gpuList, const int baseIndex);
  inline long nameToLong(bool *ok) {
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
  inline int getNumCpus() {
    return mNumCpus;
  }
  ;
  inline int getSlowestTime() {
    return mSlowestTime;
  }
  ;
  inline int getSlowTimeCount() {
    return mSlowTimeCount;
  }
  ;
  void setSlowestTime(const int newTime);
  inline ProcessHandler *getProcessHandler(const int index) {
    return &mProcessHandlerArray[index];
  }
  ;
  int getFailureCount();
  inline bool isChunkErred() {
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
  bool isTimedOut(const int index, const int timeoutMillisec);
  inline void incrementFailureCount() {
    mFailureCount++;
  }
  ;
  void msgKillProcessTimeout();
  inline bool isJobValid(const int index) {
    return mProcessHandlerArray[index].isJobValid();
  }
  ;
  bool isKillNeeded();
  bool isKillSignal();
  void resetKill();
  void startKill(const QString onePid);
  void killSignal();
  bool isKillFinished();
  inline bool isDropped() {
    return mDropped;
  }
  ;
  void killQProcesses();

public slots:
  void handleFinished(const int exitCode, const QProcess::ExitStatus exitStatus);
  void handleError(const QProcess::ProcessError error);

private:
  void init();
  void setup();
  int remoteOrLocalKillType();

  ProcessHandler *mProcessHandlerArray;
  QString mName, mDecoratedClassName;
  int mNumCpus, mFailureCount, mSlowestTime, mSlowTimeCount;
  bool mKill, mChunkErred, mDropped;
  Processchunks *mProcesschunks;

  //killing processes
  bool mIgnoreKill, mKillFinishedSignalReceived, mKillStarted, mPidsAvailable;
  bool mKillWarning, mKillingOne;
  int mKillCounter, mPidWaitCounter;
  QString mOnePidToKill;
  QProcess *mKillProcess;
};

#endif /* MACHINEHANDLER_H_ */
