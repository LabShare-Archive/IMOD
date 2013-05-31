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
  void startKill();
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
  bool useImodkillgroup();

  ProcessHandler *mProcessHandlerArray;
  QString mName, mDecoratedClassName;
  int mNumCpus, mFailureCount;
  bool mKill, mChunkErred, mDropped;
  Processchunks *mProcesschunks;

  //killing processes
  bool mIgnoreKill, mKillFinishedSignalReceived, mKillStarted, mPidsAvailable,mKillWarning;
  int mKillCounter,mPidWaitCounter;
  QProcess *mKillProcess;
};

#endif /* MACHINEHANDLER_H_ */

/*
 $Log$
 Revision 1.15  2011/02/04 00:12:04  sueh
 bug# 1426 Added useImodkillgroup().

 Revision 1.14  2011/02/02 22:42:33  sueh
 bug# 1426 Added killQProcesses.

 Revision 1.13  2011/02/02 00:09:03  sueh
 bug# 1426 Removed unused variables and commented-out code.

 Revision 1.12  2011/02/01 22:39:22  sueh
 bug# 1426 Removing old method of killing.

 Revision 1.11  2011/01/27 03:51:34  sueh
 bug# 1426 Removes const from simple variable return values (int, char,
 bool, long) because they cause a warning in the intel compiler.

 Revision 1.10  2011/01/25 07:06:01  sueh
 bug# 1426 Added mDropped.

 Revision 1.9  2011/01/21 00:13:32  sueh
 bug# 1426 Added handleError, isKillFinished, killSignal, resetKill, startKill.

 Revision 1.8  2011/01/05 20:45:53  sueh
 bug# 1426 Moved ProcessHandler instances to MachineHandler.  Moved
 one-line functions to .h file.

 */
