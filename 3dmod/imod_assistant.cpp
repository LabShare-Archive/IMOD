/*  imod_assistant.cpp - Opens Qt Assistant for help pages
 *
 *  WARNING: EDIT ONLY THE ORIGINAL FILE IN THE imod DIRECTORY
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
#include <QtAssistant/QAssistantClient>
#include <qdir.h>
#include <qmessagebox.h>
#include <qstringlist.h>
#include "imod_assistant.h"
#include "b3dutil.h"

/*!
 * Constructs the [ImodAssistant] object.  ^
 * {path} is a path to the help files ^
 * {adpFile} is the name of An Assistant Document Profile file OR NULL for 
 * none, where the file can be relative to the path or an absolute path ^
 * {messageTitle} should be a program name to have errors reported in a
 * message box, or NULL not to ^
 * {absolute} (default false) should be [true] if {path} is an absolute 
 * location or [false] if it is relative to the environment variable IMOD_DIR ^
 * {keepSideBar} (default false) should eb true to avoid starting with the
 * argument to hide the sidebar ^
 * The object emits a signal to pass on the error signal from its assistant 
 * client:  [void error(const QString &msg);]
 */
ImodAssistant::ImodAssistant(const char *path, const char *adpFile, 
                             char *messageTitle, bool absolute,
                             bool keepSideBar)
{
  mAssistant = NULL;
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
  if (adpFile)
    mAdp = QString(adpFile);
}

/*!
 * Destructor closes and deletes the assistant client object if one was opened
 */
ImodAssistant::~ImodAssistant()
{
  if (mAssistant) {
    mAssistant->closeAssistant();
    delete mAssistant;
  }
}

/*!
 * Shows a help file with file name given by {page}, {absolute} (default
 * false) should be [true] if {page} gives an absolute path and [false] if
 * it gives a path relative to the path defined in the constructor. ^
 * Returns 0 if the page is found, 1 if not, and -1 if the adp file is not
 * found.
 */
int ImodAssistant::showPage(const char *page)
{
  QString fullPath;
  QString fileOnly, assPath;
  int len, retval = 0;
  char sep = QDir::separator().toLatin1();

  // Get the assistant object the first time
  if (!mAssistant) {

    // Open the assistant in qtlib if one exists, otherwise take the one on
    // path.  Need to check for .app on Mac, and in /bin or IMOD_DIR itself on Windows
#ifdef _WIN32
    assPath = QDir::cleanPath(mImodDir + sep + "bin");
    if (!QFile::exists(assPath + sep + "assistant_adp.exe")) 
      if (QFile::exists(mImodDir + sep + "assistant_adp.exe"))
        assPath = mImodDir;
      else
        assPath = "";
#else
    assPath = QDir::cleanPath(mImodDir + sep + "qtlib");
    if (!QFile::exists(assPath + sep + "assistant_adp") && 
        !QFile::exists(assPath + sep + "assistant_adp.app"))
      assPath = "";
#endif
    mAssistant = new QAssistantClient(assPath, this);
    connect(mAssistant, SIGNAL(error(const QString&)), this, 
            SLOT(assistantError(const QString&)));

    // Hide the side bar and define the adp file if any
#if QT_VERSION >= 0x030200
    QStringList args;
    if (!mKeepSideBar)
      args << "-hideSidebar";
    if (!mAdp.isEmpty()) {
      if (QDir::isRelativePath(mAdp))
        fileOnly = mPath + sep + mAdp;
      else
        fileOnly = mAdp;
      if (QFile::exists(fileOnly))
        args << "-profile" <<  fileOnly;
      else
        retval = -1;
    }
    if (!args.empty())
      mAssistant->setArguments(args);
#endif
  }

  // Get full path name and clean it; fix up call from plugin in standalone case
  if (QDir::isRelativePath(page))
    fullPath = mPath + sep + page;
  else if (QString(page).startsWith("/lib/imodplug"))
    fullPath = mImodDir + page;
  else
    fullPath = page;
  fullPath = QDir::cleanPath(fullPath);
  if (QDir::isRelativePath(fullPath))
    fullPath = QDir::currentPath() + sep + fullPath;
  
  // Get a name with any tags stripped off to check for file existence
  fileOnly = fullPath;
  len = fileOnly.indexOf('#');
  if (len >= 0)
    fileOnly = fileOnly.left(len);
  if (!QFile::exists(fileOnly)) {
    fileOnly = QString("Cannot find help file: ") + fileOnly;
    if (mAssumedIMOD)
      fileOnly += QString("\nThis is probably because IMOD_DIR is not defined"
                          "\nand was assumed to be ") + mImodDir;
    if (!mTitle.isEmpty())
      QMessageBox::warning(0, mTitle, fileOnly, QMessageBox::Ok,
                           QMessageBox::NoButton, QMessageBox::NoButton);
    retval = 1;
  } else {

    // Just show the page without opening assistant first
    mAssistant->showPage(fullPath);
  }
  return retval;
}

// Report errors.  Sadly, it will not report a bad page
void ImodAssistant::assistantError(const QString &msg)
{
  emit error(msg);
  if (mTitle.isEmpty())
    return;
  QString fullMsg = QString("Error opening Qt Assistant:\n") + msg;
  QMessageBox::warning(0, mTitle, fullMsg, QMessageBox::Ok,
                       QMessageBox::NoButton, QMessageBox::NoButton);
}
