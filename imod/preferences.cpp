/*  IMOD VERSION 3.0.4
 *
 *  preferences.cpp - Manage preferences for Imod, using Qt settings file
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DEMC") and the Regents of the University of Colorado.    *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

$Date$

$Revision$

$Log$
*/

#include <stdlib.h>
#include "preferences.h"
#include <qsettings.h>
#include <qtooltip.h>
#include <qtabdialog.h>
#include <qstylefactory.h>
#include <qapplication.h>
#include <qdir.h>
#include <qwidgetlist.h>
#include <qobjectlist.h>
#include "form_appearance.h"
#include "form_behavior.h"
#include "form_mouse.h"
#include "imod.h"
#include "imod_display.h"
#include "imod_workprocs.h"

ImodPreferences *ImodPrefs;

#define IMOD_NAME "/imod/"

ImodPreferences::ImodPreferences(char *cmdLineStyle)
{
  double szoomvals[MAXZOOMS] =
    { 0.1, 0.16, 0.25, 0.35, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0,
    10.0, 12.0, 16.0, 20.0};
  int i;
  bool readin;
  char *autoDir;
  QString str;
  ImodPrefStruct *prefs = &mCurrentPrefs;
  mTabDlg = NULL;
  mCurrentTab = 0;

  // Set the default values
  prefs->hotSliderKeyDflt = 0;
  prefs->hotSliderFlagDflt = HOT_SLIDER_KEYUP;
  prefs->mouseMappingDflt = 0;
  prefs->silentBeepDflt = false;
  prefs->tooltipsOnDflt = true;
  prefs->bwStepDflt = 3;
  prefs->iconifyImodvDlgDflt = 1;
  prefs->iconifyImodDlgDflt = 1;
  prefs->iconifyImageWinDflt = 0;
  prefs->minModPtSizeDflt = 4;
  prefs->minImPtSizeDflt = 4;
  prefs->autosaveIntervalDflt = 5;
  prefs->autosaveOnDflt = true;
  prefs->autosaveDirDflt = "";

  // Read the settings
  QSettings settings;
  settings.insertSearchPath( QSettings::Windows, "/BL3DEMC" );
  prefs->hotSliderKey = settings.readNumEntry(IMOD_NAME"hotSliderKey", 
					      prefs->hotSliderKeyDflt,
					      &prefs->hotSliderKeyChgd);
  prefs->hotSliderFlag = settings.readNumEntry(IMOD_NAME"hotSliderFlag",
					       prefs->hotSliderFlagDflt,
					       &prefs->hotSliderFlagChgd);
  prefs->mouseMapping = settings.readNumEntry(IMOD_NAME"mouseMapping",
					    prefs->mouseMappingDflt,
					    &prefs->mouseMappingChgd);
  prefs->silentBeep = settings.readBoolEntry(IMOD_NAME"silentBeep",
					    prefs->silentBeepDflt,
					    &prefs->silentBeepChgd);
  prefs->tooltipsOn = settings.readBoolEntry(IMOD_NAME"tooltipsOn",
					    prefs->tooltipsOnDflt,
					    &prefs->tooltipsOnChgd);
  QToolTip::setGloballyEnabled(prefs->tooltipsOn);

  prefs->bwStep = settings.readNumEntry(IMOD_NAME"bwStep",
					prefs->bwStepDflt,
					&prefs->bwStepChgd);
  prefs->iconifyImodvDlg = settings.readBoolEntry(IMOD_NAME"iconifyImodvDlg",
						 prefs->iconifyImodvDlgDflt,
						 &prefs->iconifyImodvDlgChgd);
  prefs->iconifyImodDlg = settings.readBoolEntry(IMOD_NAME"iconifyImodDlg",
						prefs->iconifyImodDlgDflt,
						&prefs->iconifyImodDlgChgd);
  prefs->iconifyImageWin = settings.readBoolEntry(IMOD_NAME"iconifyImageWin",
						 prefs->iconifyImageWinDflt,
						 &prefs->iconifyImageWinChgd);
  prefs->minModPtSize = settings.readNumEntry(IMOD_NAME"minModPtSize",
					prefs->minModPtSizeDflt,
					&prefs->minModPtSizeChgd);
  prefs->minImPtSize = settings.readNumEntry(IMOD_NAME"minImPtSize",
					prefs->minImPtSizeDflt,
					&prefs->minImPtSizeChgd);

  // Read each zoom with a separate key
  prefs->zoomsChgd = false;
  for (i = 0; i < MAXZOOMS; i++) {
    prefs->zoomsDflt[i] = szoomvals[i];
    str.sprintf(IMOD_NAME"zooms/%d", i);
    prefs->zooms[i] = settings.readDoubleEntry(str, szoomvals[i], &readin);
    if (readin)
      prefs->zoomsChgd = true;
  }

  prefs->autosaveInterval = settings.readNumEntry
    (IMOD_NAME"autosaveInterval", prefs->autosaveIntervalDflt,
     &prefs->autosaveIntervalChgd);
  prefs->autosaveDir = settings.readEntry(IMOD_NAME"autosaveDir",
					  prefs->autosaveDirDflt,
					  &prefs->autosaveDirChgd);
  prefs->autosaveOn = settings.readBoolEntry(IMOD_NAME"autosaveOn",
					    prefs->autosaveOnDflt,
					    &prefs->autosaveOnChgd);

  // If no autosave interval or state read in, look for environment entry
  if (!prefs->autosaveOnChgd && !prefs->autosaveIntervalChgd) {
    i = autosaveSec();
    if (!i)
      prefs->autosaveOn = false;
    else {
      if (i < 60)
	i = 60;
      prefs->autosaveInterval = i / 60;
    }
  }

  // If no autosave directory read in, look for environment entry here too
  if (!prefs->autosaveDirChgd)
    prefs->autosaveDir = autosaveDir();

  // Look for font; either set the font or get the current font
  str = settings.readEntry(IMOD_NAME"fontString");
  prefs->fontChgd = !str.isEmpty() && prefs->font.fromString(str);
  if (prefs->fontChgd)
    QApplication::setFont(prefs->font);
  else
    prefs->font = QApplication::font();
    
  // If user entered a valid style on the command line, save that style as the
  // current key and set changed flag to false
  if (cmdLineStyle &&  QStyleFactory::create(QString(cmdLineStyle)) != NULL) {
    prefs->styleChgd = false;
    prefs->styleKey = cmdLineStyle;

    // Otherwise, look for a key and use it; or set the key to windows
  } else {
    str = settings.readEntry(IMOD_NAME"styleKey");
    prefs->styleChgd = !str.isEmpty() && QStyleFactory::create(str) != NULL;
    if (prefs->styleChgd)
      prefs->styleKey = str;
    else
      prefs->styleKey = "windows";
    QApplication::setStyle(prefs->styleKey);
  }

}

void ImodPreferences::saveSettings()
{
  ImodPrefStruct *prefs = &mCurrentPrefs;
  QSettings settings;
  QString str;
  int i;

  settings.insertSearchPath( QSettings::Windows, "/BL3DEMC" );

  if (prefs->hotSliderKeyChgd)
    settings.writeEntry(IMOD_NAME"hotSliderKey", prefs->hotSliderKey);
  if (prefs->hotSliderFlagChgd)
    settings.writeEntry(IMOD_NAME"hotSliderFlag", prefs->hotSliderFlag);
  if (prefs->mouseMappingChgd)
    settings.writeEntry(IMOD_NAME"mouseMapping", prefs->mouseMapping);
  if (prefs->silentBeepChgd)
    settings.writeEntry(IMOD_NAME"silentBeep", prefs->silentBeep);
  if (prefs->tooltipsOnChgd)
    settings.writeEntry(IMOD_NAME"tooltipsOn", prefs->tooltipsOn);
  if (prefs->bwStepChgd)
    settings.writeEntry(IMOD_NAME"bwStep", prefs->bwStep);
  if (prefs->iconifyImodvDlgChgd)
    settings.writeEntry(IMOD_NAME"iconifyImodvDlg", prefs->iconifyImodvDlg);
  if (prefs->iconifyImodDlgChgd)
    settings.writeEntry(IMOD_NAME"iconifyImodDlg", prefs->iconifyImodDlg);
  if (prefs->iconifyImageWinChgd)
    settings.writeEntry(IMOD_NAME"iconifyImageWin", prefs->iconifyImageWin);
  if (prefs->minModPtSizeChgd)
    settings.writeEntry(IMOD_NAME"minModPtSize", prefs->minModPtSize);
  if (prefs->minImPtSizeChgd)
    settings.writeEntry(IMOD_NAME"minImPtSize", prefs->minImPtSize);

  if (prefs->zoomsChgd) {
    for (i = 0; i < MAXZOOMS; i++) {
      str.sprintf(IMOD_NAME"zooms/%d", i);
      settings.writeEntry(str, prefs->zooms[i]); 
    }
  }

  if (prefs->autosaveIntervalChgd)
    settings.writeEntry(IMOD_NAME"autosaveInterval", 
			    prefs->autosaveInterval);
  if (prefs->autosaveDirChgd)
    settings.writeEntry(IMOD_NAME"autosaveDir", prefs->autosaveDir);
  if (prefs->autosaveOnChgd)
    settings.writeEntry(IMOD_NAME"autosaveOn", prefs->autosaveOn);
  if (prefs->fontChgd) {
    str = prefs->font.toString();
    settings.writeEntry(IMOD_NAME"fontString", str);
  }
  if (prefs->styleChgd)
    settings.writeEntry(IMOD_NAME"styleKey", prefs->styleKey);
  
}

int hotSliderFlag()
{
  return ImodPrefs->hotSliderFlag();
}

int hotSliderKey() 
{
  int keys[3] = {Qt::Key_Control, Qt::Key_Shift, Qt::Key_Alt};
  return keys[ImodPrefs->hotSliderKey()];
}

// Determine code for actual mouse button given the logical button number
// (1, 2, or 3)
int ImodPreferences::actualButton(int logicalButton)
{
  int qtButtons[3] = {Qt::LeftButton, Qt::MidButton, Qt::RightButton};
  int mapping, index;

  mapping = mCurrentPrefs.mouseMapping;
  index = (mapping + (1 - 2 * (mapping / 3)) * (logicalButton - 1)) % 3;
  return qtButtons[index];
}

QString ImodPreferences::autosaveDir()
{
  char *envDir = getenv("IMOD_AUTOSAVE_DIR");
  
  // if the autosave dir was read in or changed, or if there is no
  // environment setting, use the autosave dir
  if (mCurrentPrefs.autosaveDirChgd || !envDir)
    return mCurrentPrefs.autosaveDir;

  // Otherwise convert and use the environment variable, convert to /
  QDir *curdir = new QDir();
  QString convDir = curdir->cleanDirPath(QString(envDir));
  delete curdir;
  return convDir;
}

// Return the autosave interval in seconds
int ImodPreferences::autosaveSec()
{
  int autosave_timeout;
  char *userto = getenv("IMOD_AUTOSAVE");

  // If anything has been read inor set by user, or if there is no
  // environment variable setting, use the current values
  if (mCurrentPrefs.autosaveOnChgd || mCurrentPrefs.autosaveIntervalChgd || 
      !userto) {
    if (mCurrentPrefs.autosaveOn)
      return 60 * mCurrentPrefs.autosaveInterval;
    return 0;
  }

  // Otherwise use the environment variable, have negative numbers specify
  // seconds rather than minutes
  autosave_timeout = atoi(userto);
  if (autosave_timeout < 0)
    return (-autosave_timeout);
  return (60 * autosave_timeout);
}

int ImodPreferences::minCurrentImPtSize()
{
  if (mTabDlg)
    return mDialogPrefs.minImPtSize;
  return mCurrentPrefs.minImPtSize;
}

int ImodPreferences::minCurrentModPtSize() 
{
  if (mTabDlg)
    return mDialogPrefs.minModPtSize;
  return mCurrentPrefs.minModPtSize;
}

void ImodPreferences::pointSizeChanged()
{
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ImodPreferences::editPrefs()
{
  mDialogPrefs = mCurrentPrefs;
  mTabDlg = new QTabDialog(NULL, NULL, true, Qt::WDestructiveClose);
  mTabDlg->setOKButton("Done");
  mTabDlg->setCancelButton("Cancel");
  mTabDlg->setDefaultButton("Defaults");
  mAppearForm = new AppearanceForm();
  mTabDlg->addTab(mAppearForm, "Appearance");
  mBehaveForm = new BehaviorForm();
  mTabDlg->addTab(mBehaveForm, "Behaviour");
  mMouseForm = new MouseForm();
  mTabDlg->addTab(mMouseForm, "Mouse");
  mTabDlg->setCaption("Imod: Set preferences");
  connect(mTabDlg, SIGNAL(applyButtonPressed()), this, SLOT(donePressed()));
  connect(mTabDlg, SIGNAL(defaultButtonPressed()), this, 
	  SLOT(defaultPressed()));
  connect(mTabDlg, SIGNAL(cancelButtonPressed()), this, 
	  SLOT(cancelPressed()));

  if (mCurrentTab == 1)
    mTabDlg->showPage(mBehaveForm);
  else if (mCurrentTab == 2)
    mTabDlg->showPage(mMouseForm);
  mTabDlg->show();
}

// When done is pressed, unload the properties, mark if any changed
void ImodPreferences::donePressed()
{
  ImodPrefStruct *newp = &mDialogPrefs;
  ImodPrefStruct *curp = &mCurrentPrefs;
  bool autosaveChanged = false;
  mBehaveForm->unload();

  if (newp->hotSliderKey != curp->hotSliderKey) {
    curp->hotSliderKey = newp->hotSliderKey;
    curp->hotSliderKeyChgd = true;
  }

  if (newp->hotSliderFlag != curp->hotSliderFlag) {
    curp->hotSliderFlag = newp->hotSliderFlag;
    curp->hotSliderFlagChgd = true;
  }

  if (newp->mouseMapping != curp->mouseMapping) {
    curp->mouseMapping = newp->mouseMapping;
    curp->mouseMappingChgd = true;
  }

  if (!equiv(newp->silentBeep, curp->silentBeep)) {
    curp->silentBeep = newp->silentBeep;
    curp->silentBeepChgd = true;
  }

  if (!equiv(newp->tooltipsOn, curp->tooltipsOn)) {
    curp->tooltipsOn = newp->tooltipsOn;
    curp->tooltipsOnChgd = true;
    QToolTip::setGloballyEnabled(curp->tooltipsOn);
  }


  if (newp->font != curp->font) {
    curp->font = newp->font;
    curp->fontChgd = true;
  }
  if (newp->styleKey.lower() != curp->styleKey.lower()) {
    curp->styleKey = newp->styleKey;
    curp->styleChgd = true;
  }

  if (newp->bwStep != curp->bwStep) {
    curp->bwStep = newp->bwStep;
    curp->bwStepChgd = true;
  }

  if (!equiv(newp->iconifyImodvDlg, curp->iconifyImodvDlg)) {
    curp->iconifyImodvDlg = newp->iconifyImodvDlg;
    curp->iconifyImodvDlgChgd = true;
  }
  if (!equiv(newp->iconifyImodDlg, curp->iconifyImodDlg)) {
    curp->iconifyImodDlg = newp->iconifyImodDlg;
    curp->iconifyImodDlgChgd = true;
  }
  if (!equiv(newp->iconifyImageWin, curp->iconifyImageWin)) {
    curp->iconifyImageWin = newp->iconifyImageWin;
    curp->iconifyImageWinChgd = true;
  }

  if (newp->minModPtSize != curp->minModPtSize) {
    curp->minModPtSize = newp->minModPtSize;
    curp->minModPtSizeChgd = true;
  }
  if (newp->minImPtSize != curp->minImPtSize) {
    curp->minImPtSize = newp->minImPtSize;
    curp->minImPtSizeChgd = true;
  }

  for (int i = 0; i < MAXZOOMS; i++) {
    if (newp->zooms[i] != curp->zooms[i]) {
      curp->zooms[i] = newp->zooms[i];
      curp->zoomsChgd = true;
    }
  }

  if (newp->autosaveInterval != curp->autosaveInterval) {
    curp->autosaveInterval = newp->autosaveInterval;
    curp->autosaveIntervalChgd = true;
    autosaveChanged = true;
  }
  if (!equiv(newp->autosaveOn, curp->autosaveOn)) {
    curp->autosaveOn = newp->autosaveOn;
    curp->autosaveOnChgd = true;
    autosaveChanged = true;
  }
  if (newp->autosaveDir != curp->autosaveDir) {
    curp->autosaveDir = newp->autosaveDir;
    curp->autosaveDirChgd = true;
    autosaveChanged = true;
  }

  if (autosaveChanged)
    imod_start_autosave(App->cvi);

  findCurrentTab();
  mTabDlg = NULL;
}

// When cancel is pressed, getthe current tab then go through common cancel
void ImodPreferences::cancelPressed()
{
  findCurrentTab();
  userCanceled();
}

// Whenever there is a cancel by Cancel, close, or escape, 
// need to check for font or style change and restore
void ImodPreferences::userCanceled()
{
  if (!mTabDlg)
    return;
  mTabDlg = NULL;

  mTimerID = startTimer(10);
}


void ImodPreferences::timerEvent(QTimerEvent *e)
{
  killTimer(mTimerID);
  if (mDialogPrefs.styleKey != mCurrentPrefs.styleKey)
    changeStyle(mCurrentPrefs.styleKey);
  if (mDialogPrefs.font != mCurrentPrefs.font)
    changeFont(mCurrentPrefs.font);
  QToolTip::setGloballyEnabled(mCurrentPrefs.tooltipsOn);
    
  pointSizeChanged();
}

// Restore all defaults and update the dialogs
void ImodPreferences::defaultPressed()
{
  ImodPrefStruct *prefs = &mDialogPrefs;
  prefs->hotSliderKey = prefs->hotSliderKeyDflt;    
  prefs->hotSliderFlag = prefs->hotSliderFlagDflt;
  prefs->mouseMapping = prefs->mouseMappingDflt;
  prefs->silentBeep = prefs->silentBeepDflt;
  prefs->tooltipsOn = prefs->tooltipsOnDflt;
  prefs->bwStep = prefs->bwStepDflt;
  prefs->iconifyImodvDlg = prefs->iconifyImodvDlgDflt;
  prefs->iconifyImodDlg = prefs->iconifyImodDlgDflt;
  prefs->iconifyImageWin = prefs->iconifyImageWinDflt;
  prefs->minModPtSize = prefs->minModPtSizeDflt;
  prefs->minImPtSize = prefs->minImPtSizeDflt;
  prefs->autosaveInterval = prefs->autosaveIntervalDflt;
  prefs->autosaveOn = prefs->autosaveOnDflt;
  prefs->autosaveDir = prefs->autosaveDirDflt;
  for (int i = 0; i < MAXZOOMS; i++)
    prefs->zooms[i] = prefs->zoomsDflt[i];
  mAppearForm->update();
  mBehaveForm->update();
  mMouseForm->update();
  QToolTip::setGloballyEnabled(prefs->tooltipsOn);
  pointSizeChanged();
}

// Determine the currently shown tab so it can be set next time
void ImodPreferences::findCurrentTab()
{
  QWidget *curPage = mTabDlg->currentPage();
  mCurrentTab = 0;
  if (curPage == (QWidget *)mBehaveForm)
    mCurrentTab = 1;
  if (curPage == (QWidget *)mMouseForm)
    mCurrentTab = 2;
}

void ImodPreferences::changeFont(QFont newFont)
{
  QApplication::setFont(newFont);

  // Get a list of toplevel widgets and iterate over it
  QWidgetList *topList = QApplication::topLevelWidgets();
  QWidgetListIt topListIt(*topList);
  while (topListIt.current()) {
    QWidget *widget = topListIt.current();
    ++topListIt;

    // get a list of children that are widgets and iterate over it
    QObjectList *objectList = widget->queryList("QWidget", 0, 0, true);
    QObjectListIt objectListIt(*objectList);
    while (objectListIt.current()) {
      QWidget *child = (QWidget *)objectListIt.current();
      ++objectListIt;
      child->setFont(newFont);
    }

    // Change font of toplevel last
    widget->setFont(newFont);
    delete objectList;
  }
  delete topList;
}

void ImodPreferences::changeStyle(QString newKey)
{
  QApplication::setStyle(newKey);
}
