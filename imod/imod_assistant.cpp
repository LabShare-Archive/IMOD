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
*/

#include <stdlib.h>
#include <qassistantclient.h>
#include <qdir.h>
#include "imod_assistant.h"
#include "dia_qtutils.h";

ImodAssistant *ImodHelp;

ImodAssistant::ImodAssistant(const char *path, bool absolute)
{
  QString imodDir = QString(getenv("IMOD_DIR"));

  // Open the assistant in qtlib if one exists, otherwise take the one on path
  QString assPath = QDir::cleanDirPath(imodDir + "/qtlib");
  if (!QFile::exists(assPath + "/assistant"))
    assPath = "";
  mAssistant = new QAssistantClient(assPath, this);

  // Hide the sidebar if Qt supports it
#if QT_VERSION >= 0x030300
  mAssistant->setArguments("hideSidebar");
#endif

  // Set up path to help files; either absolute or under IMOD_DIR
  if (absolute) {
    mPath = QString(path);
  } else {
    mPath = imodDir + "/" + QString(path);
  }
  connect(mAssistant, SIGNAL(error(const QString&)), this, 
          SLOT(assistantError(const QString&)));
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
  if (absolute)
    fullPath = page;
  else
    fullPath = mPath + "/" + page;
  mAssistant->openAssistant();
  mAssistant->showPage(QDir::cleanDirPath(fullPath));
}

// Report errors.  Sadly, it will not report a bad page
void ImodAssistant::assistantError(const QString &msg)
{
  QString fullMsg = QString("Error opening Qt Assistant or help page:\n") +
    msg;
  dia_err((char *)fullMsg.latin1());
}
