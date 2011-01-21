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
 *  Log at end of file
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

  void setup(Processchunks &processchunks, const QString &machineName, const int numCpus);
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
  const bool killNextProcess(const bool asynchronou);
  inline const bool isJobValid(const int index) {
    return mProcessHandlerArray[index].isJobValid();
  }
  ;

  //MachineHandler &operator=(const MachineHandler &machineHandler);
  //Compares mName to a QString
  //inline const bool operator==(const QString &other) {
  //  return mName == other;
  //}
  //;
  void cleanupKillProcess();
  const bool isKillNeeded();
  const bool isKillSignal();
  void resetKill();
  void startKill();
  void killSignal();
  const bool isKillFinished();

public slots:
  void handleFinished(const int exitCode, const QProcess::ExitStatus exitStatus);
  void handleError(const QProcess::ProcessError error);

private:
  void init();
  void setup();

  ProcessHandler *mProcessHandlerArray;
  QString mName, mDecoratedClassName;
  int mNumCpus, mFailureCount;
  bool mKill, mChunkErred;
  Processchunks *mProcesschunks;

  //killing processes
  bool mIgnoreKill, mDrop, mKillFinishedSignalReceived, mKillStarted, mPidsAvailable;
  int mKillCpuIndex, mKillCounter;
  QProcess *mKillProcess;
};

#endif /* MACHINEHANDLER_H_ */

/*
 $Log$
 Revision 1.8  2011/01/05 20:45:53  sueh
 bug# 1426 Moved ProcessHandler instances to MachineHandler.  Moved
 one-line functions to .h file.

 */
