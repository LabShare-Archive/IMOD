/*
 *  imod_assistant.cpp - Opens Qt Assistant for help pages
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

    $Log$
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

#include <stdlib.h>
#include <qassistantclient.h>
#include <qdir.h>
#include <qstringlist.h>
#include "imod_assistant.h"
#include "dia_qtutils.h"
#include "imod.h"

ImodAssistant *ImodHelp;

ImodAssistant::ImodAssistant(const char *path, const char *adpFile, 
                             bool absolute)
{
  mAssistant = NULL;

  // Set up path to help files; either absolute or under IMOD_DIR
  if (absolute) {
    mPath = QString(path);
  } else {
    mPath = QString(getenv("IMOD_DIR")) + QDir::separator() + QString(path);
  }
  if (adpFile)
    mAdp = QString(adpFile);
}

ImodAssistant::~ImodAssistant()
{
  if (mAssistant) {
    mAssistant->closeAssistant();
    delete mAssistant;
  }
}

// Show page given an absolute or relative path
void ImodAssistant::showPage(const char *page, bool absolute)
{
  QString fullPath;
  QString fileOnly;
  int len;
  char sep = QDir::separator();

  // Get the assistant object the first time
  if (!mAssistant) {

    // Open the assistant in qtlib if one exists, otherwise take the one on
    // path.  Need to check for .app on Mac
    QString imodDir = QString(getenv("IMOD_DIR"));
    QString assPath = QDir::cleanDirPath(imodDir + sep + "qtlib");
    if (!QFile::exists(assPath + sep + "assistant") && 
        !QFile::exists(assPath + sep + "assistant.app"))
      assPath = "";
    mAssistant = new QAssistantClient(assPath, this);
    connect(mAssistant, SIGNAL(error(const QString&)), this, 
            SLOT(assistantError(const QString&)));

    // Hide the side bar and define the adp file if any
#if QT_VERSION >= 0x030200
    QStringList args;
    args << "-hideSidebar";
    if (!mAdp.isEmpty())
      args << "-profile" <<  mPath + QDir::separator() + mAdp;
    mAssistant->setArguments(args);
#endif
  }

  // Get full path name and clean it
  if (absolute)
    fullPath = page;
  else
    fullPath = mPath + QDir::separator() + page;
  fullPath = QDir::cleanDirPath(fullPath);
  
  // Get a name with any tags stripped off to check for file existence
  fileOnly = fullPath;
  len = fileOnly.find('#');
  if (len >= 0)
    fileOnly = fileOnly.left(len);
  if (!QFile::exists(fileOnly)) {
    fileOnly = QString("Cannot find help file: ") + fileOnly;
    dia_err((char *)fileOnly.latin1());
  } else {

    // Just show the page without opening assistant first
    mAssistant->showPage(fullPath);
  }
}

// Report errors.  Sadly, it will not report a bad page
void ImodAssistant::assistantError(const QString &msg)
{
  QString fullMsg = QString("Error opening Qt Assistant:\n") +
    msg;
  dia_err((char *)fullMsg.latin1());
}
