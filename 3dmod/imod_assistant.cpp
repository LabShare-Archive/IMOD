/*  imod_assistant.cpp - Opens Qt Assistant for help pages
 *
 *  WARNING: EDIT ONLY THE ORIGINAL FILE IN THE 3dmod DIRECTORY
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdlib.h>
#include <qdir.h>
#include <qapplication.h>
#include <qtextstream.h>
#include <qmessagebox.h>
#include <qstringlist.h>
#include "imod_assistant.h"
#include "b3dutil.h"

/*!
 * Constructs the [ImodAssistant] object.  ^
 * {path} is a path to the help collection file ^
 * {qhcFile} is the name of a Qt help collection file,
 * where the file can be relative to the path or an absolute path ^
 * {messageTitle} should be a program name to have errors reported in a
 * message box, or NULL not to ^
 * {absolute} (default false) should be [true] if {path} is an absolute 
 * location or [false] if it is relative to the environment variable IMOD_DIR ^
 * {keepSideBar} (default false) should be true to avoid starting with the
 * argument to hide the sidebar ^
 * {prefix} is a prefix to place in front of the help page name; the default is
 * qthelp://bl3demc/IMOD  ^
 * If {prefAbsolute} is false (the default), the supplied prefix will be added to
 * qthelp://bl3demc/IMOD/ and if it is true, the prefix should be an absolute URL. ^
 * The object emits a signal to pass on the error message when there is a failure
 * running Assistant or a non-zero exit status: ^
 *   [void error(const QString &msg);]
 */
ImodAssistant::ImodAssistant(const char *path, const char *qhcFile, 
                             const char *messageTitle, bool absolute,
                             bool keepSideBar,  const char *prefix, bool prefAbsolute)
{
  mAssistant = NULL;
  mExiting = false;
  mAssumedIMOD = 0;
  mKeepSideBar = keepSideBar;
  if (messageTitle)
    mTitle = QString(messageTitle) + " Error";

  // Get IMOD_DIR or fallback if necessary
  mImodDir = IMOD_DIR_or_default(&mAssumedIMOD);
#ifdef _WIN32

  // Check for alternate locations for standalone IMOD/3dmod directories
  if (mAssumedIMOD && !QFile::exists(mImodDir)) {
    char *PF3dmod = "C:\\Program Files\\3dmod";
    char *PFIMOD = "C:\\Program Files\\IMOD";
    if (QFile::exists(PFIMOD))
      mImodDir = PFIMOD;
    else if (QFile::exists(PF3dmod))
      mImodDir = PF3dmod;
  }
#endif

  // Set up path to help files; either absolute or under IMOD_DIR
  if (absolute) {
    mPath = QString(path);
  } else {
    mPath = mImodDir + QDir::separator() + QString(path);
  }
  mQhc = qhcFile;
  if (mQhc == "IMOD.adp")
    mQhc = "IMOD.qhc";
  mQhc = mPath +  QDir::separator() + mQhc;
  mPrefix = "qthelp://bl3demc/IMOD/";
  if (prefix) {
    if (prefAbsolute)
      mPrefix = prefix;
    else
      mPrefix = mPrefix + prefix;
  }
}

/*!
 * Destructor closes and deletes the assistant client object if one was opened
 */
ImodAssistant::~ImodAssistant()
{
  mExiting = true;
  if (mAssistant) {
    mAssistant->terminate();
  } 
}

/*!
 * Shows a help file with file name given by {page}, which must be relative to the 
 * prefix given in the constructor ^
 * Returns 0 if assistant starts OK, and -1 if the qhc file is not
 * found.
 */
int ImodAssistant::showPage(const char *page)
{
  QString fullPath = mPrefix;
  QString fileOnly, assPath;
  bool sendTwice = false;
  int len;
  char sep = QDir::separator().toLatin1();
  if (!mPrefix.endsWith('/'))
    fullPath += '/';
  fullPath += page;

  if (!QFile::exists(mQhc)) {
    fileOnly = QString("Cannot find help collection file: ") + mQhc;
    if (mAssumedIMOD)
      fileOnly += QString("\nThis is probably because IMOD_DIR is not defined"
                          "\nand was assumed to be ") + mImodDir;
    if (!mTitle.isEmpty())
      QMessageBox::warning(0, mTitle, fileOnly, QMessageBox::Ok,
                           QMessageBox::NoButton, QMessageBox::NoButton);
    emit error(fileOnly);
    return -1;
  }

  // Get the assistant object the first time
  if (!mAssistant) {

    // Open the assistant in qtlib if one exists, otherwise take the one on
    // path.  Need to check for .app on Mac, and in /bin or IMOD_DIR itself on Windows
#ifdef _WIN32
    assPath = QDir::cleanPath(mImodDir + sep + "bin");
    if (!QFile::exists(assPath + sep + "assistant.exe")) 
      if (QFile::exists(mImodDir + sep + "assistant.exe"))
        assPath = mImodDir;
      else
        assPath = "";
#else
#ifdef Q_OS_MACX
    // On Mac, if it isn't here it's hopeless
    assPath = QDir::cleanPath(mImodDir + sep + 
                              "qtlib/Assistant.app/Contents/MacOS/Assistant");
#else
    assPath = QDir::cleanPath(mImodDir + sep + "qtlib");
    if (!QFile::exists(assPath + sep + "assistant"))
      assPath = "";
#endif
#endif
#ifndef Q_OS_MACX
    if (!assPath.isEmpty())
      assPath += '/';
    assPath += "assistant";
#endif
    mAssistant = new QProcess();
    connect(mAssistant, SIGNAL(finished(int, QProcess::ExitStatus)), this,
            SLOT(assistantExited(int, QProcess::ExitStatus )));
    QStringList args;
    args << "-collectionFile" << mQhc << "-enableRemoteControl" << "-showUrl" << fullPath;
    QString showhide = mKeepSideBar ? "-show" : "-hide";
    args << showhide << "contents" << showhide << "index" << showhide << "search";
    if (mKeepSideBar)
      args << "-activate" << "contents";
    else
      args << "-hide" << "bookmarks";
    mAssistant->start(assPath, args);
    /* printf("Started %s with args:", (const char *)(assPath.toLatin1()));
       for (int kk = 0; kk < args.size(); kk++)
       printf("  %s", (const char *)((args[kk]).toLatin1()));
       printf("\n"); */
    mAssistant->waitForStarted(3000);
    QApplication::processEvents();
    sendTwice = true;
  }

  // Want one level of the Table of contents; that entry was wrong for a long time
  if (mKeepSideBar)
#if QT_VERSION >= 0x040700
    fullPath +=  "; expandToc 1;";
#else
    fullPath +=  "; expandToc 0;";
#endif
  QTextStream str(mAssistant);
  str << "setSource " << fullPath << '\0' << endl;

  // On Mac, multiple sends were needed to keep from getting multiple tabs,
  // or about:blank or Qt Assistant help page.  With sending the page on
  // startup, it was better but still needed another send to get the Toc right
  // The time delay was needed on Win laptop
  if (sendTwice) {
    for (len = 0; len < 2; len++) {
      QApplication::processEvents();
      b3dMilliSleep(400);
      str << "setSource " << fullPath << '\0' << endl;
    }
  }
  return 0;
}

// Report errors when it exits
void ImodAssistant::assistantExited(int exitCode, QProcess::ExitStatus exitStatus)

{
  QString fullMsg;
  delete mAssistant;
  mAssistant = NULL;
  if (mExiting)
    return;
  if (exitStatus != QProcess::NormalExit)
    fullMsg = "Abnormal exit trying to run Qt Assistant";
  else if (exitCode)
    fullMsg.sprintf("Qt Assistant exited with an error (return code %d)", exitCode);
  else
    return;

  emit error(fullMsg);
  if (!mTitle.isEmpty())
    QMessageBox::warning(0, mTitle, fullMsg, QMessageBox::Ok,
                         QMessageBox::NoButton, QMessageBox::NoButton);
}
