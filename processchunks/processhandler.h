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
#include <machinehandler.h>
#include "comfilejob.h"
#include <QTextStream>

class Processchunks;
class MachineHandler;
class ComFileJob;

class ProcessHandler: public QObject {
Q_OBJECT

public:
  ProcessHandler();
  ~ProcessHandler();

  void setup(Processchunks &processchunks);
  const bool logFileExists(const bool newlyCreatedFile);
  const bool isChunkDone();
  void setFlag(const ComFileJob::FlagType flag);
  void removeFiles();
  void removeProcessFiles();
  const bool cshFileExists();
  const int getNumChunkErr();
  const QString getComFileName();
  void printWarnings();
  const QString getLogFileName();
  const QByteArray readAllLogFile();
  const bool isLogFileEmpty();
  const bool isStartProcessTimedOut(const int timeoutMillisec);
  void getErrorMessageFromLog(QString &errorMess);
  void incrementNumChunkErr();
  const bool isComProcessDone();
  void printTooManyErrorsMessage(const int numErr);
  const bool isJobFileEmpty();
  const bool getSshError(QString &dropMess);
  const bool qidFileExists();
  const QString getPid();
  void setFlagNotDone(const bool singleFile);
  void backupLog();
  const ComFileJob::FlagType getFlag();
  void runProcess(MachineHandler *machine);
  const bool killProcess();
  void continueKillProcess(const bool asynchronous);
  void msgKillProcessTimeout();
  const bool isFinishedSignalReceived();
  void getErrorMessageFromOutput(QString &errorMess);
  const bool isPidInStderr();
  void resetPausing();
  bool isPausing();
  QString getCshFile();
  void setJob(ComFileJob &comFileJob,const int jobIndex);
  void invalidateJob();
  const int getAssignedJobIndex();
  const bool isJobValid();

public slots:
  void handleError(const QProcess::ProcessError error);
  void handleStarted();
  void
  handleFinished(const int exitCode, const QProcess::ExitStatus exitStatus);
  void handleReadyReadStandardError();
  void handleKillFinished(const int exitCode,
      const QProcess::ExitStatus exitStatus);
  void cleanupKillProcess();

protected:
  void timerEvent(const QTimerEvent *e);

private:
  void initProcess();
  const bool getPid(QTextStream &stream, const bool save);
  const bool getSshError(QString &dropMess, QTextStream &stream);
  void resetSignalValues();
  void readAllStandardError();
  void killLocalProcessAndDescendents(QString &pid);
  void stopProcess(const QString &pid);
  void killProcess(const QString &pid);
  void resetFields();

  ComFileJob *mComFileJob;
  QFile *mLogFile, *mJobFile, *mQidFile;
  bool mLogFileExists;
  //On when process is run, off when finished, kill finished signal received,
  //or when the kill timeout is handled.
  bool mStartingProcess;
  int  mPausing;
  QByteArray mStderr;
  QTextStream  *mJobFileTextStream, *mQidFileTextStream;
  QString mPid, mEscapedRemoteDirPath, mDecoratedClassName, mCommand;//queue or local command
  QStringList mParamList;//list of queue or local params
  Processchunks *mProcesschunks;
  QProcess *mProcess;
  QTime mStartTime;

  //Kill process variables
  MachineHandler *mMachine;
  QProcess *mKillProcess;
  int mComFileJobIndex, mPidTimerId;
  bool mKill, mRanContinueKillProcess,mLocalKill;

  //Signal variables
  bool mErrorSignalReceived, mStartedSignalReceived, mFinishedSignalReceived,
      mKillFinishedSignalReceived;
  int mProcessError, mExitCode, mExitStatus;
};

#endif /* PROCESSHANDLER_H_ */
