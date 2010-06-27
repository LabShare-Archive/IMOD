#ifndef PROCESSCHUNKS_H
#define PROCESSCHUNKS_H

#include <QApplication>
#include <machinehandler.h>
#include <QList>

class ProcessHandler;
class QTextStream;
class QProcess;

class Processchunks: public QApplication {
Q_OBJECT

public:
  Processchunks(int &argc, char **argv);
  ~Processchunks();

  void loadParams(int &argc, char **argv);
  void setup();
  bool askGo();
  void runProcesses();
  void msgProcessFinished();

protected:
  void timerEvent(QTimerEvent *e);

private:
  void extractVersion(QString versionString);
  void buildFilters(char *reg, char *sync, QStringList &filters);
  void cleanupList(char *remove, QStringList &list);
  int runProcessAndOutputLines(QProcess &process, QString &command,
      QStringList &params, int numLines);

  ProcessHandler *mProcessArray;
  QList<MachineHandler> *mMachineList;
  QTextStream *mOut;
  int mSizeProcessArray, mProcessFinishedCount, mRetain,
      mSingleFile, mJustGo, mSkipProbe, mReturnPid, mNice, mDropCrit,
      mMaxChunkErr, mQueue, mVersion, mVerbose, mCopyLogIndex;
  char *mRemoteDir;//was curdir
  char *mCheckFile, *mQueueName, *mCpuList, *mRootName, *mQueueCom,
      *mSshExt, *mPidExt;
  QString *mHostRoot;
  QStringList *mSshOpts;
  bool mRestarting;
};

#endif
