/*
 * processhandler.h - has slots to handle process signals and knows which process
 * it is running
 *
 *  Created on: Jun 3, 2010
 *      Author: sueh
 */

#ifndef PROCESSHANDLER_H_
#define PROCESSHANDLER_H_

#include <QProcess>
#include <processchunks.h>

class ProcessHandler: public QObject {
Q_OBJECT

public:
  ProcessHandler();
  ~ProcessHandler();

  enum flag {
    sync = -1, notDone, done = 2
  };

  bool setup(int verbose,int singleFile,QString comFile);
  void reset();
  bool logExists();
  bool isChunkDone();
  void setFlag(enum flag);
  void backupLog();
  bool flagEquals(enum flag);

  void
  init(Processchunks *parent, char *imodDir, QTextStream *out, int index);
  void setParams(QStringList params);
  void runProcess();
  bool waitForFinished(int timeout);


public slots:
  void handleError(QProcess::ProcessError error);
  void handleReadyReadStandardError();
  void handleStarted();
  void handleFinished(int exitCode, QProcess::ExitStatus exitStatus);



private:
  QString *mComFileName,*mLogFileName;
  bool mVerbose,mSingleFile;
  int mNumChunkErr;
  enum flag mFlag;

  Processchunks *mParent;
  QTextStream *mOut;
  QProcess *mProc;
  int mIndex;
  QStringList *mParams;
  bool mFinished;
  QString *mPid, *mStderr;
  char *mImodDir;
};

#endif /* PROCESSHANDLER_H_ */
