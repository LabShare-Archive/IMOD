/*
 * processhandler.cpp
 *
 *  Created on: Jun 3, 2010
 *      Author: sueh
 */
#include <processhandler.h>
#include <QTextStream>

static QString pidTag = "PID:";

ProcessHandler::ProcessHandler() {
  mPid = NULL;
  mFinished = false;
  mProc = new QProcess(this);
  mStderr = new QString();
  mVerbose = 0;
  QObject::connect(mProc, SIGNAL(error(QProcess::ProcessError)),
      SLOT(handleError(QProcess::ProcessError)));
  QObject::connect(mProc, SIGNAL(readyReadStandardError()),
      SLOT(handleReadyReadStandardError()));
  QObject::connect(mProc, SIGNAL(started()), SLOT(handleStarted()));
  QObject::connect(mProc, SIGNAL(finished(int, QProcess::ExitStatus)),
      SLOT(handleFinished(int, QProcess::ExitStatus)));
}

ProcessHandler::~ProcessHandler() {
  delete mProc;
  delete mStderr;
  delete mPid;
}

void ProcessHandler::init(Processchunks *parent, char *imodDir,
    QTextStream *out, int index) {
  mParent = parent;
  mImodDir = imodDir;
  mOut = out;
  mIndex = index;
}

void ProcessHandler::setVerbose(int verbose) {
  mVerbose = verbose;
}

void ProcessHandler::setParams(QStringList params) {
  mParams = &params;
}

void ProcessHandler::runProcess() {
  int i;
  QString command("ssh");
  if (mVerbose) {
    *mOut << command << " ";
    for (i = 0; i < mParams->size(); ++i) {
      *mOut << mParams->at(i) << " ";
    }
    *mOut << endl;
  }
  QStringList env = QProcess::systemEnvironment();
  QString pathReplace("PATH=");
  pathReplace.append(mImodDir);
#ifdef _WIN32
  pathReplace.append(";");
#else
  pathReplace.append(":");
#endif
  pathReplace.append("\\1");
  env.replaceInStrings(QRegExp("^PATH=(.*)", Qt::CaseInsensitive), pathReplace);
  mProc->setEnvironment(env);
  if (mVerbose) {
    for (i = 0; i < env.size(); ++i) {
      if (env.at(i).contains(QRegExp("^PATH=(.*)", Qt::CaseInsensitive))) {
        *mOut << env.at(i) << endl;
      }
    }
  }
  mProc->start(command, *mParams);
}

void ProcessHandler::handleFinished(int exitCode,
    QProcess::ExitStatus exitStatus) {
  if (mVerbose) {
    *mOut << mIndex << ": " << "state:" << mProc->state() << ",exitCode:"
        << exitCode << ",exitStatus:" << exitStatus << endl;
    *mOut << mIndex << ": " << "stderr:" << *mStderr << endl;
    *mOut << mIndex << ": " << "stdout:" << mProc->readAllStandardOutput()
        << endl;
  }
  mParent->msgProcessFinished();
}

void ProcessHandler::handleError(QProcess::ProcessError error) {
  *mOut << mIndex << ": " << "state:" << mProc->state() << ",error:" << error
      << endl;
}

//Append stderr to mStderr.  Set mPid from the shell PID as soon as it is
//available.  Expecting the line to contain:
//Shell PID: #####eol
void ProcessHandler::handleReadyReadStandardError() {
  mStderr->append(mProc->readAllStandardError());
  if (mPid == NULL) {
    int pidTagIndex = mStderr->indexOf(pidTag);
    if (pidTagIndex != -1) {
      //Found PID tag, see if the PID itself is there.
      //Strip off everything before the PID
      QString temp = mStderr->mid(pidTagIndex + pidTag.length());
      if (temp.contains("\n") || temp.contains("\r\n")) {
        //Line completed so the complete pid should have been placed in stderr.
        //Strip off everything after the PID
        QString pid = temp.trimmed();
        if (!pid.isEmpty()) {
          QStringList array = pid.split(QRegExp("\\s+"),
              QString::SkipEmptyParts);
          if (array.size() > 0 && !array.at(0).isEmpty()) {
            mPid = new QString();
            mPid->append(pid.trimmed());
            if (mVerbose) {
              *mOut << mIndex << ": " << "mPid:" << *mPid << endl;
            }
          }
        }
      }
    }
  }
}
void ProcessHandler::handleStarted() {
  if (mVerbose) {
    *mOut << mIndex << ": " << "state:" << mProc->state() << endl;
  }
}
