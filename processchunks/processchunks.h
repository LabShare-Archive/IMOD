/*
 *  Main class for processchunks - a batch application that runs command
 *  file(s) on one or more machines or a single queue.
 *
 *  This application was derived directly from the processchunks script.  The
 *  algorithm for running processes is basically the same (process status is
 *  checked at intervals).  The kill process functionality is different from
 *  processchunks and is signal based - only using a timer for timeout
 *  situations.
 *
 *  Because there is so much class interdependency, this file includes the
 *  header files for all the classes in this application.  Include this file in
 *  all the .cpp files in this application.  This makes including the class's
 *  .h file unnecessary.
 *
 *  Contains the main timer for processchunks.  Assigns and manages chunks
 *  (.com file processes) running on multiple computers (or a queue).  Handles
 *  user requests via an interrupt mechanism (Esc+Enter) on non-Windows
 *  environments.  Also monitors a file (default name is processchunks.input)
 *  to get user requests.
 *
 *  Ignores Ctrl-C, which was the interrupt mechanism in the original
 *  processchunks script, because the Ctrl-C causes the QProcesses to stop -
 *  even when it is intercepted by the program.  Losing the QProcess's means
 *  that the application cannot receive signals from the actual .com processes,
 *  which are not effected by the Ctrl-C and are still running on other
 *  computers.
 *
 *  Contains a list of MachineHandlers based on the description of the
 *  computer(s) or the queue.
 *
 *  Contains a ComFileJobs instance based on the description of the chunk file
 *  names (a prefix for the .com files).
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

#ifndef PROCESSCHUNKS_H
#define PROCESSCHUNKS_H

#include <QCoreApplication>
#include <QDir>
//All processchunks application header files should be included here.
#include "comfilejobs.h"
#include "processhandler.h"
#include "machinehandler.h"

//Returns the name of the currently running function.
#ifndef __func__
#define __func__ __FUNCTION__
#endif

//Flag values
//was enum sync = -1, notDone, assigned, done
#define CHUNK_SYNC -1
#define CHUNK_NOT_DONE 0
#define CHUNK_ASSIGNED 1
#define CHUNK_DONE 2

class Processchunks: public QCoreApplication {
Q_OBJECT

public:
  Processchunks(int &argc, char **argv);
  ~Processchunks();

  void printOsInformation();
  void loadParams(int &argc, char **argv);
  void setup();
  bool askGo();
  void startLoop();
  void killProcessOnNextMachine();
  void msgKillProcessStarted(ProcessHandler *processHandler);
  void msgKillProcessDone(ProcessHandler *processHandler);
  void handleFileSystemBug();

  inline bool isQueue() {
    return mQueue;
  }
  ;
  inline const QString &getQueueCommand() {
    return mQueueCommand;
  }
  ;
  inline QDir &getCurrentDir() {
    return mCurrentDir;
  }
  ;
  inline const QStringList &getQueueParamList() {
    return mQueueParamList;
  }
  ;
  inline bool isVerbose(const QString &verboseClass, const QString verboseFunction,
      const int verbosity = 1) {
    return isVerbose(verboseClass, verboseFunction, verbosity, true);
  }
  ;
  inline QTextStream &getOutStream() {
    return *mOutStream;
  }
  ;
  inline bool isSingleFile() {
    return mSingleFile;
  }
  ;
  inline const QString &getHostRoot() {
    return mHostRoot;
  }
  ;
  inline const QStringList &getSshOpts() {
    return mSshOpts;
  }
  ;
  inline int getNice() {
    return mNice;
  }
  ;
  inline int getMillisecSleep() {
    return mMillisecSleep;
  }
  ;
  inline char getAns() {
    return mAns;
  }
  ;
  inline QStringList &getDropList() {
    return mDropList;
  }
  ;
  inline int getDropCrit() {
    return mDropCrit;
  }
  ;
  inline const QString &getRemoteDir() {
    return *mRemoteDir;
  }
  ;
  void makeCshFile(ProcessHandler *process);
  inline ComFileJobs *getComFileJobs() {
    return mComFileJobs;
  }
  ;
  void killSignal();
  inline bool resourcesAvailableForKill() {
    return mMaxKills > mNumKills;
  }
  ;
  inline void incrementKills() {
    mNumKills++;
  }
  inline void decrementKills() {
    mNumKills--;
  }
  ;

public slots:
  //Single shot timer slot
  inline void timerEvent() {
    timerEvent(NULL);
  }
  ;

protected:
  void timerEvent(QTimerEvent *e);

private:
  int extractVersion(const QString &versionString);
  void buildFilters(const char *reg, const char *sync, QStringList &filters);
  void cleanupList(const char *remove, QStringList &list);
  int runGenericProcess(QByteArray &output, QProcess &process, const QString &command,
      const QStringList &params, const int numLinesToPrint);
  void setupSshOpts();
  int * initMachineList(QStringList &machineNameList);
  void setupMachineList(QStringList &machineNameList, int *numCpusList);
  void setupHostRoot();
  void setupComFileJobs();
  void probeMachines(QStringList &machineNameList);
  bool readCheckFile();
  void exitIfDropped(const int minFail, const int failTot, const int assignTot);
  bool handleChunkDone(MachineHandler &machine, ProcessHandler *process,
      const int jobIndex);
  bool
      handleLogFileError(QString &errorMess, MachineHandler &machine,
          ProcessHandler *process);
  void handleComProcessNotDone(bool &dropout, QString &dropMess, MachineHandler &machine,
      ProcessHandler *process);
  void handleDropOut(bool &noChunks, QString &dropMess, MachineHandler &machine,
      ProcessHandler *process, QString &errorMess);
  bool checkChunk(int &runFlag, bool &noChunks, int &undone, bool &foundChunks,
      bool &chunkOk, MachineHandler &machine, const int jobIndex, const int chunkErrTot);
  void runProcess(MachineHandler &machine, ProcessHandler *process, const int jobIndex);
  int escapeEntered();
  void handleInterrupt();
  void cleanupAndExit(int exitCode = 0);
  void killProcessTimeout();
  void killProcesses(QStringList *dropList = NULL);
  void startTimers();
  void cleanupKillProcesses(const bool timeout);
  bool handleError(const QString *errorMess, MachineHandler &machine,
      ProcessHandler *process);
  bool isVerbose(const QString &verboseClass, const QString verboseFunction,
      const int verbosity, const bool print);

  int mSizeJobArray, mMachineListSize, mNumMachinesDropped;
  ComFileJobs *mComFileJobs;
  MachineHandler *mMachineList;
  QTextStream *mOutStream;

  //parameters
  int mRetain, mJustGo, mNice, mMillisecSleep, mDropCrit, mQueue, mSingleFile,
      mMaxChunkErr, mVerbose;
  bool mSkipProbe;
  char *mQueueName, *mRootName;
  QFile *mCheckFile;
  QString mCpuList, mVerboseClass, *mRemoteDir;//was curdir;
  QStringList mVerboseFunctionList;

  //setup
  int mCopyLogIndex, mNumCpus;
  QString mHostRoot, mQueueCommand, mDecoratedClassName;
  QStringList mSshOpts, mQueueParamList;
  QDir mCurrentDir;

  //loop
  int mNumDone, mLastNumDone, mHoldCrit, mTimerId, mFirstUndoneIndex, mNextSyncIndex,
      mSyncing;
  bool mPausing, mAnyDone;
  char mAns;

  //killing processes
  bool mKill, mAllKillProcessesHaveStarted;
  int mKillProcessMachineIndex, mKillCounter, mNumKills, mMaxKills;
  QList<ProcessHandler*> mProcessesWithUnfinishedKillRequest;
  QList<ProcessHandler*> mKilledProcesses;
  QStringList mDropList;

  //running processes
  QProcess *mLsProcess, *mVmstocsh;
  QStringList mLsParamList;
};

#endif /* PROCESSCHUNKS_H_ */

/*
 $Log$
 Revision 1.28  2011/01/27 22:54:30  sueh
 bug# 1426 Switching to QCoreApplication, which is for console applications.

 Revision 1.27  2011/01/27 03:51:55  sueh
 bug# 1426 Removes const from simple variable return values (int, char,
 bool, long) because they cause a warning in the intel compiler.  Moved the
 the kill message for queues to the machine handler so it will only print
 once.

 Revision 1.26  2011/01/25 07:15:22  sueh
 bug# 1426 Added mNumMachinesDropped.

 Revision 1.25  2011/01/21 00:18:12  sueh
 bug# 1426 Adding decrementPipes, incrementPipes, pipesAvailable,
 initMachineList, killSignal, setupComFileJobs.

 Revision 1.24  2011/01/05 20:50:22  sueh
 bug# 1426 Moved one-line functions to .h file.  Creating on instance of
 ComFileJobs instead of an array of ComFileJob instances.  Moved the array
 into ComFileJobs.  Fixed the includes.

 */
