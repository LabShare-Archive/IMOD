/*
 *  Runs a process, receives and handles process signals, kills the process.
 *
 *  Associated with a single .com file (chunk) using an index to an array
 *  in ComFileJobs.  Can be reset to change chunks.
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

#ifndef PROCESSHANDLER_H_
#define PROCESSHANDLER_H_

class Processchunks;
class MachineHandler;

#include <QFile>
#include <QTextStream>
#include <QProcess>
#include <QTime>
#include "b3dutil.h"

class ProcessHandler: public QObject {
Q_OBJECT

public:
  ProcessHandler();
  ~ProcessHandler();

  void setup(Processchunks &processchunks);
  const bool logFileExists(const bool newlyCreatedFile);
  const bool isChunkDone();
  void setFlag(const int flag);
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
  inline void backupLog() {
    imodBackupFile(mLogFile->fileName().toLatin1().data());
  }
  ;
  const int getFlag();
  void runProcess(MachineHandler *machine);
  const bool killProcess();
  void continueKillProcess(const bool asynchronous);
  void msgKillProcessTimeout();
  inline const bool isFinishedSignalReceived() {
    return mFinishedSignalReceived;
  }
  ;
  void getErrorMessageFromOutput(QString &errorMess);
  const bool isPidInStderr();
  inline void resetPausing() {
    mPausing = 0;
  }
  ;
  bool isPausing();
  QString getCshFile();
  void setJob(const int jobIndex);
  inline void invalidateJob() {
    mValidJob = false;
  }
  ;
  inline const int getAssignedJobIndex() {
    return mComFileJobIndex;
  }
  ;
  inline const bool isJobValid() {
    return mValidJob;
  }
  ;

public slots:
  void handleError(const QProcess::ProcessError error);
  inline void handleStarted() {
    mStartedSignalReceived = true;
  }
  ;
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

  QFile *mLogFile, *mJobFile, *mQidFile;
  bool mLogFileExists, mValidJob;
  //On when process is run, off when finished, kill finished signal received,
  //or when the kill timeout is handled.
  bool mStartingProcess;
  int mPausing;
  QByteArray mStderr;
  QTextStream *mJobFileTextStream, *mQidFileTextStream;
  QString mPid, mEscapedRemoteDirPath, mDecoratedClassName, mCommand;//queue or local command
  QStringList mParamList;//list of queue or local params
  Processchunks *mProcesschunks;
  QProcess *mProcess;
  QTime mStartTime;

  //Kill process variables
  MachineHandler *mMachine;
  QProcess *mKillProcess;
  int mComFileJobIndex, mPidTimerId;
  bool mKill, mRanContinueKillProcess, mLocalKill;

  //Signal variables
  bool mErrorSignalReceived, mStartedSignalReceived, mFinishedSignalReceived,
      mKillFinishedSignalReceived;
  int mProcessError, mExitCode, mExitStatus;
};

#endif /* PROCESSHANDLER_H_ */

/*
 $Log$
 */
