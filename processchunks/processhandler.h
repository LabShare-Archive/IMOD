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
 *  Log at end of file
 */

#ifndef PROCESSHANDLER_H_
#define PROCESSHANDLER_H_

class Processchunks;
class MachineHandler;

#include "b3dutil.h"
#include <QProcess>
#include <QTextStream>
#include <QTime>
#include <QFile>

#define CHUNK_PROCESS_PIPES 4
#define KILL_CHUNK_PROCESS_PIPES 6

class ProcessHandler: public QObject {
Q_OBJECT

public:
  ProcessHandler();
  ~ProcessHandler();

  void setup(Processchunks &processchunks);
  bool logFileExists(const bool newlyCreatedFile);
  bool isChunkDone();
  void setFlag(const int flag);
  void removeFiles();
  void removeProcessFiles();
  bool cshFileExists();
  int getNumChunkErr();
  const QString getComFileName();
  void printWarnings();
  const QString getLogFileName();
  const QByteArray readAllLogFile();
  bool isLogFileEmpty();
  bool isStartProcessTimedOut(const int timeoutMillisec);
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
  void startKill();
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
  void killLocalProcessAndDescendents(QString &pid);
  void stopProcess(const QString &pid);
  void resetFields();

  QFile *mLogFile, *mJobFile, *mQidFile;
  bool mLogFileExists, mValidJob;
  //On when process is run, off when finished, kill finished signal received,
  //or when the kill timeout is handled.
  bool mStartingProcess;
  int mPausing, mComFileJobIndex;
  QByteArray mStderr;
  QTextStream *mJobFileTextStream, *mQidFileTextStream;
  QString mPid, mEscapedRemoteDirPath, mDecoratedClassName, mCommand;//queue or local command
  QStringList mParamList;//list of queue or local params
  Processchunks *mProcesschunks;
  QProcess *mProcess;
  QTime mStartTime;
  MachineHandler *mMachine;

  //Kill process variables
  QProcess *mKillProcess;
  int mKillCounter;
  bool mKill, mLocalKill, mKillStarted, mIgnoreKill;

  //Signal variables
  bool mErrorSignalReceived, mFinishedSignalReceived, mKillFinishedSignalReceived;
  int mProcessError, mExitCode, mExitStatus;
};

#endif /* PROCESSHANDLER_H_ */

/*
 $Log$
 Revision 1.22  2011/02/02 00:09:49  sueh
 bug# 1426 Removed unused variables and commented-out code.

 Revision 1.21  2011/02/01 22:39:01  sueh
 bug# 1426 Removing old method of killing.

 Revision 1.20  2011/02/01 01:29:42  sueh
 bug# 1426 Removed unnecessary killProcess(QString).

 Revision 1.19  2011/01/27 03:52:25  sueh
 bug# 1426 Removes const from simple variable return values (int, char,
 bool, long) because they cause a warning in the intel compiler.

 Revision 1.18  2011/01/25 07:16:49  sueh
 bug# 1426 Added mIgnoreKill.

 Revision 1.17  2011/01/24 18:47:13  sueh
 bug# 1426 Removed const from timerEvent(QtimerEvent) to avoid a
 compiler warning.

 Revision 1.16  2011/01/21 00:20:35  sueh
 bug# 1426 Added isPidEmpty, killSignal, resetKill, setJobNotDone, startKill.

 Revision 1.15  2011/01/05 20:53:28  sueh
 bug# 1426 Instead of getting a ComFileJob instance, get an index and
 refer to the ComFileJobs instance in Processchunks.  Moved one-line
 functions to the header.  Added mValidJob.

 */
