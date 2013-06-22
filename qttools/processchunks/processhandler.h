/*
 *  Runs a process, receives and handles process signals, kills the process.
 *
 *  Associated with one .com file (chunk) at a time, using an index to the
 *  array in ComFileJobs.
 *
 *  Author: Sue Held
 *
 *  Copyright (C) 2010,2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#ifndef PROCESSHANDLER_H_
#define PROCESSHANDLER_H_

class Processchunks;
class MachineHandler;

#include "b3dutil.h"
#include <QProcess>
#include <QTextStream>
#include <QTime>
#include <QDateTime>
#include <QFile>

#define CHUNK_PROCESS_PIPES 4
#define KILL_CHUNK_PROCESS_PIPES 6

class ProcessHandler: public QObject {
Q_OBJECT

public:
  ProcessHandler();
  ~ProcessHandler();

  void setup(Processchunks &processchunks, int gpuNum);
  bool logFileExists(const bool newlyCreatedFile);
  bool isChunkDone();
  void setFlag(const int flag);
  void removeFiles();
  void removeProcessFiles();
  bool cshFileExists();
  int getNumChunkErr();
  const QString getComFileName();
  void printWarnings(const QString &machineName);
  const QString getLogFileName();
  const QByteArray readAllLogFile();
  bool isLogFileEmpty();
  bool isStartProcessTimedOut(const int timeoutMillisec);
  bool isLogFileOlderThan(const int timeoutSec);
  void getErrorMessageFromLog(QString &errorMess);
  void incrementNumChunkErr();
  bool isComProcessDone();
  void printTooManyErrorsMessage(const int numErr);
  bool isJobFileEmpty();
  bool getSshError(QString &dropMess);
  bool qidFileExists();
  const QString getPid();
  void setFlagNotDone(const bool singleFile);
  inline void backupLog() {
    imodBackupFile(mLogFile->fileName().toLatin1().data());
  }
  ;
  int getFlag();
  void runProcess(MachineHandler &machine);
  inline bool isFinishedSignalReceived() {
    return mFinishedSignalReceived;
  }
  ;
  void getErrorMessageFromOutput(QString &errorMess);
  bool isPidInStderr();
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
  inline int getAssignedJobIndex() {
    return mComFileJobIndex;
  }
  ;
  inline int getGpuNumber() {
    return mGpuNumber;
  }
  ;
  inline int getElapsedTime() {
    return mElapsedTime < 0 ? mStartTime.elapsed() : mElapsedTime;
  }
  ;
  inline bool isJobValid() {
    return mValidJob;
  }
  ;
  void killSignal();
  bool isPidEmpty();
  inline bool isKillFinished() {
    return mIgnoreKill || (mKillFinishedSignalReceived && mFinishedSignalReceived);
  }
  ;
  void resetKill();
  void setJobNotDone();
  void startKill(bool killOne);
  void killQProcesses();

public slots:
  void handleError(const QProcess::ProcessError error);
  void
  handleFinished(const int exitCode, const QProcess::ExitStatus exitStatus);
  void handleKillFinished(const int exitCode, const QProcess::ExitStatus exitStatus);

private:
  void initProcess();
  bool getPid(QTextStream &stream, const bool save);
  bool getSshError(QString &dropMess, QTextStream &stream);
  void resetSignalValues();
  void readAllStandardError();
  void resetFields();

  QFile *mLogFile, *mJobFile, *mQidFile;
  bool mLogFileExists, mValidJob;
  //On when process is run, off when finished, kill finished signal received,
  //or when the kill timeout is handled.
  bool mStartingProcess;
  int mPausing, mComFileJobIndex;
  QTime mPauseTime;
  QByteArray mStderr;
  QTextStream *mJobFileTextStream, *mQidFileTextStream, *mOutStream;
  QString mPid, mEscapedRemoteDirPath, mDecoratedClassName, mCommand;//queue or local command
  QStringList mParamList;//list of queue or local params
  Processchunks *mProcesschunks;
  QProcess *mProcess;
  QTime mStartTime;
  int mElapsedTime;
  QDateTime mLogLastModified;
  QDateTime mLastSizeCheckTime;
  QDateTime mSizeChangedTime;
  int mLastLogSize;
  MachineHandler *mMachine;
  int mGpuNumber;

  //Kill process variables
  QProcess *mKillProcess;
  int mKillCounter;
  bool mKill, mKillStarted, mIgnoreKill, mKillingOne;

  //Signal variables
  bool mErrorSignalReceived, mFinishedSignalReceived, mKillFinishedSignalReceived;
  bool mLogHasError;
  int mProcessError, mExitCode, mExitStatus;
};

#endif /* PROCESSHANDLER_H_ */
