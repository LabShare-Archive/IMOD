/*
 *  Main class for processchunks - an application to process command files on
 *  multiple machines.
 *
 *  Contains macros and all include files for the application.  Header files
 *  for the other processchunks classes should only be included in this class.
 *
 *  Contains the main timer for processchunks.  Assigns and manages chunks run-
 *  ning on multiple computers (or a queue).  Kills chunks.
 *
 *  Contains a list of MachineHandlers  based on the description of the
 *  computer(s) or a queue that was passed to the application.
 *
 *  Contains a ComFileJobs instance.
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

#include <QApplication>
#include <QDir>
//processchunks.h is the only place where all of the headers for the
//processchunks classes should be included.  Include processchunks.h instead of
//the class header in all the processchunks classes' .cpp files.
#include "processhandler.h"
#include "machinehandler.h"
#include "comfilejobs.h"

#ifndef __func__
#define __func__ __FUNCTION__
#endif

//Flag values
//was enum sync = -1, notDone, assigned, done
#define CHUNK_SYNC -1
#define CHUNK_NOT_DONE 0
#define CHUNK_ASSIGNED 1
#define CHUNK_DONE 2

class Processchunks: public QApplication {
Q_OBJECT

public:
  Processchunks(int &argc, char **argv);
  ~Processchunks();

  void printOsInformation();
  void loadParams(int &argc, char **argv);
  void setup();
  const bool askGo();
  void startLoop();
  void killProcessOnNextMachine();
  void msgKillProcessStarted(ProcessHandler *processHandler);
  void msgKillProcessDone(ProcessHandler *processHandler);
  void handleFileSystemBug();

  inline const bool isQueue() {
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
  inline const bool isVerbose(const QString &verboseClass,
      const QString verboseFunction, const int verbosity = 1) {
    return isVerbose(verboseClass, verboseFunction, verbosity, true);
  }
  ;
  inline QTextStream &getOutStream() {
    return *mOutStream;
  }
  ;
  inline const bool isSingleFile() {
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
  inline const int getNice() {
    return mNice;
  }
  ;
  inline const int getMillisecSleep() {
    return mMillisecSleep;
  }
  ;
  inline const char getAns() {
    return mAns;
  }
  ;
  inline QStringList &getDropList() {
    return mDropList;
  }
  ;
  inline const int getDropCrit() {
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

public slots:
  //Single shot timer slot
  inline void timerEvent() {
    timerEvent(NULL);
  }
  ;

protected:
  void timerEvent(QTimerEvent *e);

private:
  const int extractVersion(const QString &versionString);
  void buildFilters(const char *reg, const char *sync, QStringList &filters);
  void cleanupList(const char *remove, QStringList &list);
  const int runGenericProcess(QByteArray &output, QProcess &process,
      const QString &command, const QStringList &params,
      const int numLinesToPrint);
  void setupSshOpts();
  void setupMachineList();
  void setupHostRoot();
  void setupProcessArray();
  void probeMachines();
  const bool readCheckFile();
  void exitIfDropped(const int minFail, const int failTot, const int assignTot);
  const bool handleChunkDone(MachineHandler *machine, ProcessHandler *process,
      const int jobIndex);
  const bool
  handleLogFileError(QString &errorMess, MachineHandler *machine,
      ProcessHandler *process);
  void handleComProcessNotDone(bool &dropout, QString &dropMess,
      MachineHandler *machine, ProcessHandler *process);
  void handleDropOut(bool &noChunks, QString &dropMess,
      MachineHandler *machine, ProcessHandler *process, QString &errorMess);
  const bool checkChunk(int &runFlag, bool &noChunks, int &undone,
      bool &foundChunks, bool &chunkOk, MachineHandler *machine,
      const int jobIndex, const int chunkErrTot);
  void runProcess(MachineHandler *machine, ProcessHandler *process,
      const int jobIndex);
  int escapeEntered();
  void handleInterrupt();
  void cleanupAndExit(int exitCode = 0);
  void killProcessTimeout();
  void killProcesses(QStringList *dropList = NULL);
  void startTimers();
  void cleanupKillProcesses(const bool timeout);
  const bool handleError(const QString *errorMess, MachineHandler *machine,
      ProcessHandler *process);
  const bool isVerbose(const QString &verboseClass,
      const QString verboseFunction, const int verbosity, const bool print);

  int mSizeJobArray;
  ComFileJobs *mComFileJobs;
  QList<MachineHandler> mMachineList;
  QTextStream *mOutStream;

  //params
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
  int mNumDone, mLastNumDone, mHoldCrit, mTimerId, mFirstUndoneIndex,
      mNextSyncIndex, mSyncing;
  bool mPausing, mAnyDone;
  char mAns;

  //killing processes
  bool mKill, mAllKillProcessesHaveStarted;
  int mKillProcessMachineIndex, mKillCounter;
  QList<ProcessHandler*> mProcessesWithUnfinishedKillRequest;
  QStringList mDropList;

  //handling file system bug
  QProcess *mLsProcess, *mVmstocsh;
  QStringList mLsParamList;
};

#endif /* PROCESSCHUNKS_H_ */

/*
 $Log$
 */
