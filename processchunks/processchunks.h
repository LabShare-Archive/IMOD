#ifndef QTPROCESSCHUNKS_H
#define QTPROCESSCHUNKS_H

#include <QApplication>

class ProcessHandler;
class QTextStream;
class QProcess;

class QTprocesschunks: public QApplication {
Q_OBJECT

public:
  QTprocesschunks(int &argc, char **argv);
  ~QTprocesschunks();

  void msgProcessFinished();
  void loadParams(int &argc, char **argv);
  void upgradeSshOptions();
  void setupMachineArray();
  void setupComFileArray();
  void setupHostRoot();
  void setupRemoteDir();
  void probeMachines();
  bool askGo();
  void listenForSigInt();
  void runProcesses();

protected:
  void timerEvent(QTimerEvent *e);

private:
  void extractVersion(QString versionString);
  void buildFilters(char *reg, char *sync, QStringList &filters);
  void cleanupList(char *remove, QStringList &list);
  int runProcessAndOutputLines(QProcess &process, QString &command,
      QStringList &params, int numLines);

  ProcessHandler *mHandlers;
  QTextStream *mOut;
  int mProcessFinishedCount, mRetain, mSingleFile, mJustGo, mSkipProbe,
      mReturnPid, mNice, mDropCrit, mMaxChunkErr, mQueue, mVersion, mVerbose;
  char *mRemoteDir;//was curdir
  char *mCheckFile, *mQueueName, *mMachineList, *mRootName, *mQueueCom,
      *mSshExt, *mPidExt;
  QString *mHostRoot;
  QStringList *mMachineArray, *mComFileArray, *mSshOpts;
};

#endif
