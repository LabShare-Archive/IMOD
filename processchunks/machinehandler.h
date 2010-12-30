/*
 * machinehandler.h
 *
 *  Created on: Jun 23, 2010
 *      Author: sueh
 */

#ifndef MACHINEHANDLER_H_
#define MACHINEHANDLER_H_

#include <QTime>
#include <QString>
#include <processchunks.h>

class QTextStream;
class Processchunks;
class ProcessHandler;

class MachineHandler {
public:
  MachineHandler(Processchunks &processchunks, const char *machineName,
      const int numCpus);
  MachineHandler(Processchunks &processchunks, const QString &machineName);
  ~MachineHandler();

  const long nameToLong(bool *ok);
  void setValues(const char *machineName, const int numCpus);
  void incrementNumCpus();
  const QString &getName();
  void setup();
  const int getNumCpus();
  ProcessHandler *getProcessHandler(const int index);
  const int getFailureCount();
  const bool isChunkErred();
  void setFailureCount(const int failureCount);
  void setChunkErred(const bool chunkErred);
  const bool isTimedOut(const int index, const int timeoutMillisec);
  void incrementFailureCount();
  const bool killProcesses();
  void msgKillProcessTimeout();
  const bool killNextProcess();
  const bool isJobValid(const int index);
  const int getAssignedJobIndex(const int index);

  MachineHandler &operator=(const MachineHandler &machineHandler);
  const bool operator==(const QString &other);

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
