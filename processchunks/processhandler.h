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

  void
      init(Processchunks *parent, char *imodDir, QTextStream *out, int index);
  void setVerbose(int verbose);
  void setParams(QStringList params);
  void runProcess();
  bool waitForFinished(int timeout);

public slots:
  void handleError(QProcess::ProcessError error);
  void handleReadyReadStandardError();
  void handleStarted();
  void handleFinished(int exitCode, QProcess::ExitStatus exitStatus);

private:
  Processchunks *mParent;
  QTextStream *mOut;
  QProcess *mProc;
  int mIndex;
  QStringList *mParams;
  bool mFinished, mVerbose;
  QString *mPid, *mStderr;
  char *mImodDir;
};

#endif /* PROCESSHANDLER_H_ */
