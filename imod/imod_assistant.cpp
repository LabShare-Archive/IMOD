/*  imod_assistant.cpp - Opens Qt Assistant for help pages
 *
 *  WARNING: EDIT ONLY THE ORIGINAL FILE IN THE imod DIRECTORY
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

    $Date$

    $Revision$
    Log at end of file
*/

#include <stdlib.h>
#include <qassistantclient.h>
#include <qdir.h>
#include <qmessagebox.h>
#include <qstringlist.h>
#include "imod_assistant.h"

/*!
 * Constructs the [ImodAssistant] object.  ^
 * {path} is a path to the help files ^
 * {adpFile} is the name of Assistant Document Profile file in that
 * location or NULL for none ^
 * {messageTitle} should be a program name to have errors reported in a
 * message box, or NULL not to ^
 * {absolute} (default false) should be [true] if {path} is an absolute 
 * location or [false] if it is relative to the environment variable IMOD_DIR ^
 * The object emits a signal to pass on the error signal from its assistant 
 * client:  [void error(const QString &msg);]
 */
ImodAssistant::ImodAssistant(const char *path, const char *adpFile, 
                             char *messageTitle, bool absolute)
{
  mAssistant = NULL;
  if (messageTitle)
    mTitle = QString(messageTitle) + " Error";

  // Set up path to help files; either absolute or under IMOD_DIR
  if (absolute) {
    mPath = QString(path);
  } else {
    mPath = QString(getenv("IMOD_DIR")) + QDir::separator() + QString(path);
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
int ImodAssistant::showPage(const char *page, bool absolute)
{
  QString fullPath;
  QString fileOnly, assPath;
  int len, retval = 0;
  char sep = QDir::separator();

  // Get the assistant object the first time
  if (!mAssistant) {

    // Open the assistant in qtlib if one exists, otherwise take the one on
    // path.  Need to check for .app on Mac, and in /bin on Windows
    QString imodDir = QString(getenv("IMOD_DIR"));
#ifdef _WIN32
    assPath = QDir::cleanDirPath(imodDir + sep + "bin");
    if (!QFile::exists(assPath + sep + "assistant.exe")) 
      assPath = "";
#else
    assPath = QDir::cleanDirPath(imodDir + sep + "qtlib");
    if (!QFile::exists(assPath + sep + "assistant") && 
        !QFile::exists(assPath + sep + "assistant.app"))
      assPath = "";
#endif
    mAssistant = new QAssistantClient(assPath, this);
    connect(mAssistant, SIGNAL(error(const QString&)), this, 
            SLOT(assistantError(const QString&)));

    // Hide the side bar and define the adp file if any
#if QT_VERSION >= 0x030200
    QStringList args;
    args << "-hideSidebar";
    if (!mAdp.isEmpty()) {
      fileOnly = mPath + sep + mAdp;
      if (QFile::exists(fileOnly))
        args << "-profile" <<  fileOnly;
      else
        retval = -1;
    }
    mAssistant->setArguments(args);
#endif
  }

  // Get full path name and clean it
  if (absolute)
    fullPath = page;
  else
    fullPath = mPath + sep + page;
  fullPath = QDir::cleanDirPath(fullPath);
  if (QDir::isRelativePath(fullPath))
    fullPath = QDir::currentDirPath() + sep + fullPath;
  
  // Get a name with any tags stripped off to check for file existence
  fileOnly = fullPath;
  len = fileOnly.find('#');
  if (len >= 0)
    fileOnly = fileOnly.left(len);
  if (!QFile::exists(fileOnly)) {
    fileOnly = QString("Cannot find help file: ") + fileOnly;
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

/*
    $Log$
    Revision 1.7  2004/12/06 04:39:19  mast
    Made truly standalone, took out of library back into 3dmod

    Revision 1.2  2004/12/04 19:22:38  mast
    Converted path to absolute before call assistant, RH 9.0 seemed to need

    Revision 1.1  2004/12/04 02:07:13  mast
    Added to libdiaqt, added argument to control error reporting, fixed for
    Windows if assistant in IMOD_DIR/bin is not on path

    Revision 1.5  2004/11/24 18:30:02  mast
    Add the adp file if Qt version supports it

    Revision 1.4  2004/11/22 18:07:22  mast
    Changed to work on Mac and to get assistant object on first time instead
    of upon construction

    Revision 1.3  2004/11/22 04:30:50  mast
    ; on include

    Revision 1.2  2004/11/22 03:58:58  mast
    Got it working in Windows/Qt 3.3; added check for file existence

    Revision 1.1  2004/11/22 00:21:32  mast
    Addition to program

*/
