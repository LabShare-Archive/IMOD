#ifndef PROCESSCHUNKS_H
#define PROCESSCHUNKS_H

#include <QApplication>
#include <machinehandler.h>
#include <QList>
#include <QStringList>
#include <QDir>
#include <processhandler.h>

#ifndef __func__
#define __func__ __FUNCTION__
#endif

class ProcessHandler;
class QTextStream;
class QProcess;
class QFile;
class MachineHandler;

class Processchunks: public QApplication {
Q_OBJECT

public:
  Processchunks(int &argc, char **argv);
  ~Processchunks();

  void loadParams(int &argc, char **argv);
  void setup();
  const bool askGo();
  void startLoop();
  void handleInterrupt();
  void killProcessOnNextMachines();
  ProcessHandler &getProcessHandler(const int processIndex);
  void msgKillProcessStarted(const int processIndex);
  void msgKillProcessDone(const int processIndex);
  void handleFileSystemBug();
  void setInterrupt();

  const QStringList &getEnv();
  const bool isQueue();
  const QString &getQueueCommand();
  QDir &getCurrentDir();
  const QStringList &getQueueParamList();
  const bool isVerbose(const QString &verboseClass,const char *verboseFunction);
  QTextStream &getOutStream();
  const bool isSingleFile();
  const QString &getHostRoot();
  const QStringList &getSshOpts();
  const int getNice();
  const char getAns();
  QStringList &getDropList();
  const int getDropCrit();
  const QString &getRemoteDir();

public slots:
  void timerEvent();

protected:
  void timerEvent(QTimerEvent *e);

private:
  const int extractVersion(const QString &versionString);
  void buildFilters(const char *reg, const char *sync, QStringList &filters);
  void cleanupList(const char *remove, QStringList &list);
  const int runProcessAndOutputLines(QProcess &process, const QString &command,
      const QStringList &params, const int numLines);
  void setupSshOpts();
  void setupMachineList();
  void setupHostRoot();
  void setupEnvironment();
  void setupProcessArray();
  void probeMachines();
  const bool readCheckFile();
  void exitIfDropped(const int minFail, const int failTot, const int assignTot);
  const bool handleChunkDone(MachineHandler *machine, const int cpuIndex,
      const int processIndex);
  const bool
  handleLogFileError(QString &errorMess, MachineHandler *machine,
      const int cpuIndex, const int processIndex);
  void handleComProcessNotDone(bool &dropout, QString &dropMess,
      MachineHandler *machine, const int processIndex);
  void handleDropOut(bool &noChunks, QString &dropMess,
      MachineHandler *machine, const int cpuIndex, const int processIndex,
      QString &errorMess);
  const bool checkChunk(int &runFlag, bool &noChunks, int &undone,
      bool &foundChunks, bool &chunkOk, MachineHandler *machine,
      const int processIndex, const int chunkErrTot);
  void runProcess(MachineHandler *machine, const int cpuIndex,
      const int processIndex);
  void cleanupAndExit(int exitCode = 0);
  void killProcesses(QStringList *dropList = NULL);
  void startTimers();
  void cleanupKillProcesses(const bool timeout);
  void checkQueueProcessesDone(const bool timeout);

  int mSizeProcessArray;
  ProcessHandler *mProcessArray;
  QList<MachineHandler> mMachineList;
  QTextStream *mOutStream;

  //params
  int mRetain, mJustGo, mNice, mDropCrit, mQueue, mSingleFile, mMaxChunkErr,
      mVerbose;
  bool mSkipProbe;
  char *mQueueName, *mRootName;
  QFile *mCheckFile;
  const QString *mRemoteDir;//was curdir
  QString mCpuList,mVerboseClass;
  QStringList mVerboseFunctionList;

  //setup
  int mCopyLogIndex, mNumCpus;
  QString mHostRoot, mQueueCommand,mDecoratedClassName;
  QStringList mSshOpts, mQueueParamList, mEnv;
  QDir mCurrentDir;

  //loop
  int mNumDone, mLastNumDone, mHoldCrit, mTimerId, mFirstUndoneIndex,
      mNextSyncIndex, mSyncing;
  bool mPausing, mAnyDone, mInterrupt;
  char mAns;

  //killing processes
  bool mKill, mAllKillProcessesHaveStarted;
  int mKillProcessMachineIndex, mKillCounter;
  QList<int> mProcessesWithUnfinishedKillRequest;
  QStringList mDropList;

  //handling file system bug
  QProcess *mLsProcess;
  QStringList mLsParamList;
};

#endif
