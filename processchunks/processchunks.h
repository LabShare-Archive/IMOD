#ifndef PROCESSCHUNKS_H
#define PROCESSCHUNKS_H

#include <QApplication>

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

  ProcessHandler *mHandlers;
  QTextStream *mOut;
  int mProcessFinishedCount, mRetain, mSingleFile, mJustGo, mSkipProbe,
      mReturnPid, mNice, mDropCrit, mMaxChunkErr, mQueue, mVersion, mVerbose,
      mFlags[],mNumChunkErr[];
  char *mRemoteDir;//was curdir
  char *mCheckFile, *mQueueName, *mMachineList, *mRootName, *mQueueCom,
      *mSshExt, *mPidExt;
  QString *mHostRoot;
  QStringList *mMachineArray, *mComFileArray, *mSshOpts;
};

#endif
