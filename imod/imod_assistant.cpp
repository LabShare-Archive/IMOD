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
    Revision 1.1  2004/11/22 00:21:32  mast
    Addition to program

*/

#include <stdlib.h>
#include <qassistantclient.h>
#include <qdir.h>
#include <qstringlist.h>
#include "imod_assistant.h"
#include "dia_qtutils.h";
#include "imod.h"

ImodAssistant *ImodHelp;

ImodAssistant::ImodAssistant(const char *path, bool absolute)
{
  QString imodDir = QString(getenv("IMOD_DIR"));
  char sep = QDir::separator();

  // Open the assistant in qtlib if one exists, otherwise take the one on path
  QString assPath = QDir::cleanDirPath(imodDir + sep + "qtlib");
  if (!QFile::exists(assPath + sep + "assistant"))
    assPath = "";
  mAssistant = new QAssistantClient(assPath, this);
  connect(mAssistant, SIGNAL(error(const QString&)), this, 
          SLOT(assistantError(const QString&)));

  // Set up path to help files; either absolute or under IMOD_DIR
  if (absolute) {
    mPath = QString(path);
  } else {
    mPath = imodDir + sep + QString(path);
  }

  // Hide the side bar but do not define an adp file
#if QT_VERSION >= 0x030200
  mAssistant->setArguments(QStringList("-hideSidebar"));
#endif

}

ImodAssistant::~ImodAssistant()
{
  mAssistant->closeAssistant();
  delete mAssistant;
}

// Show page given an absolute or relative path
void ImodAssistant::showPage(const char *page, bool absolute)
{
  QString fullPath;
  QString fileOnly;
  int len;

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
  QString fullMsg = QString("Error opening Qt Assistant or help page:\n") +
    msg;
  dia_err((char *)fullMsg.latin1());
}
