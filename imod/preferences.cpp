/*  preferences.cpp - Manage preferences for Imod, using Qt settings file
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
/* Checklist for adding an item:
 * Add structure variable for item, default value, and boolean for change
 * Add function for returning current value
 * Set default value
 * Read the setting, including any checks on validity
 * Write the setting if changed in saveSettings()
 * In donePressed, record change in value
 * In defaultPressed, restore value in appropriate section
 * Update program state if needed in timerEvent or pointSizeChanged
 * In designer, build interface, load interface and unload it
 */

#include <stdlib.h>
#include "preferences.h"
#include <qsettings.h>
#include <qtooltip.h>
#include <qtabdialog.h>
#include <qstylefactory.h>
#include <qapplication.h>
#include <qdir.h>
#include <qimage.h>
#include <qwidgetlist.h>
#include <qobjectlist.h>
#include "form_appearance.h"
#include "form_behavior.h"
#include "form_mouse.h"
#include "imod.h"
#include "xzap.h"
#include "imod_info.h"
#include "imod_display.h"
#include "imod_moviecon.h"
#include "imodv_movie.h"
#include "imod_workprocs.h"
#include "control.h"

ImodPreferences *ImodPrefs;

#ifdef Q_OS_MACX
#define IMOD_NAME
#else
#define IMOD_NAME "/3dmod/"
#endif

#define MAX_STYLES 24

#ifdef EXCLUDE_STYLES
// These are the styles that crash in RH 7.3
static char *styleList[] = {"highcolor", "b3", "default", ""};
#else
// These are the styles that do not crash RH 7.3 and that exist there, because
// non-existent style makes it search and generate "already exists" messages
static char *styleList[] = {"Windows", "compact", 
                            "Platinum", "Motif", "MotifPlus", "SGI", 
                            "CDE", "marble", "System", "Systemalt", "riscos", 
                            "Light, 2nd revision", "Light, 3rd revision", 
#ifdef Q_OS_MACX
                            "Macintosh (Aqua)",
#endif
                            ""};
#endif
static int styleStatus[MAX_STYLES];

typedef struct generic_settings
{
  char *key;
  int numVals;
  double *values;
} GenericSettings;


/* CONSTRUCTOR TO READ PREFERENCES AND FUNCTION TO SAVE UPON EXIT */

ImodPreferences::ImodPreferences(char *cmdLineStyle)
{
  double szoomvals[MAXZOOMS] =
    { 0.1, 0.1667, 0.25, 0.3333, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0,
      8.0, 10.0, 12.0, 16.0, 20.0};
  int i, left, top, width, height;
  bool readin;
  QString str;
  QStringList strList;
  ImodPrefStruct *prefs = &mCurrentPrefs;
  QSettings *settings = getSettingsObject();
  mTabDlg = NULL;
  mCurrentTab = 0;
  mRecordedZapGeom = QRect(0, 0, 0, 0);
  mGenericList = ilistNew(sizeof(GenericSettings), 4);

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
  prefs->rememberGeomDflt = true;
  prefs->autoTargetMeanDflt = 150;
  prefs->autoTargetSDDflt = 40;
  prefs->namedIndex[0] = App->select; 
  prefs->namedIndex[1] = App->shadow; 
  prefs->namedIndex[2] = App->endpoint;
  prefs->namedIndex[3] = App->bgnpoint;
  prefs->namedIndex[4] = App->curpoint;
  prefs->namedIndex[5] = App->foreground;
  prefs->namedIndex[6] = App->background;
  prefs->namedIndex[7] = App->ghost;
  prefs->namedColorDflt[0] = qRgb(255, 255,   0);
  prefs->namedColorDflt[1] = qRgb(128, 128,   0);
  prefs->namedColorDflt[2] = qRgb(255,   0,   0);
  prefs->namedColorDflt[3] = qRgb(  0, 255,   0);
  prefs->namedColorDflt[4] = qRgb(255, 255, 128);
  prefs->namedColorDflt[5] = qRgb(255, 255, 128);
  prefs->namedColorDflt[6] = qRgb( 64,  64,  96);
  prefs->namedColorDflt[7] = qRgb( 16,  16,  16);  
  prefs->snapFormatDflt = "RGB";
  prefs->snapQualityDflt = 80;

  prefs->hotSliderKey = settings->readNumEntry(IMOD_NAME"hotSliderKey", 
                                              prefs->hotSliderKeyDflt,
                                              &prefs->hotSliderKeyChgd);
  prefs->hotSliderFlag = settings->readNumEntry(IMOD_NAME"hotSliderFlag",
                                               prefs->hotSliderFlagDflt,
                                               &prefs->hotSliderFlagChgd);
  prefs->mouseMapping = settings->readNumEntry(IMOD_NAME"mouseMapping",
                                            prefs->mouseMappingDflt,
                                            &prefs->mouseMappingChgd);
  prefs->silentBeep = settings->readBoolEntry(IMOD_NAME"silentBeep",
                                            prefs->silentBeepDflt,
                                            &prefs->silentBeepChgd);
  prefs->tooltipsOn = settings->readBoolEntry(IMOD_NAME"tooltipsOn",
                                            prefs->tooltipsOnDflt,
                                            &prefs->tooltipsOnChgd);
  QToolTip::setGloballyEnabled(prefs->tooltipsOn);

  prefs->bwStep = settings->readNumEntry(IMOD_NAME"bwStep",
                                        prefs->bwStepDflt,
                                        &prefs->bwStepChgd);
  prefs->iconifyImodvDlg = settings->readBoolEntry(IMOD_NAME"iconifyImodvDlg",
                                                 prefs->iconifyImodvDlgDflt,
                                                 &prefs->iconifyImodvDlgChgd);
  prefs->iconifyImodDlg = settings->readBoolEntry(IMOD_NAME"iconifyImodDlg",
                                                prefs->iconifyImodDlgDflt,
                                                &prefs->iconifyImodDlgChgd);
  prefs->iconifyImageWin = settings->readBoolEntry(IMOD_NAME"iconifyImageWin",
                                                 prefs->iconifyImageWinDflt,
                                                 &prefs->iconifyImageWinChgd);
  prefs->minModPtSize = settings->readNumEntry(IMOD_NAME"minModPtSize",
                                        prefs->minModPtSizeDflt,
                                        &prefs->minModPtSizeChgd);
  prefs->minImPtSize = settings->readNumEntry(IMOD_NAME"minImPtSize",
                                        prefs->minImPtSizeDflt,
                                        &prefs->minImPtSizeChgd);
  prefs->rememberGeom = settings->readBoolEntry(IMOD_NAME"rememberGeom",
                                                 prefs->rememberGeomDflt,
                                                 &prefs->rememberGeomChgd);
  mGeomLastSaved = settings->readNumEntry(IMOD_NAME"lastGeometrySaved", -1);
  prefs->autoTargetMean = settings->readNumEntry(IMOD_NAME"autoTargetMean",
                                        prefs->autoTargetMeanDflt,
                                        &prefs->autoTargetMeanChgd);
  prefs->autoTargetSD = settings->readNumEntry(IMOD_NAME"autoTargetSD",
                                        prefs->autoTargetSDDflt,
                                        &prefs->autoTargetSDChgd);
  prefs->snapFormat = settings->readEntry(IMOD_NAME"snapFormat",
                                          prefs->snapFormatDflt,
                                          &prefs->snapFormatChgd);
  prefs->snapQuality = settings->readNumEntry(IMOD_NAME"snapQuality",
                                        prefs->snapQualityDflt,
                                        &prefs->snapQualityChgd);

  // Make sure an output format is on the list; if not drop back to RGB
  strList = (snapFormatList()).grep(prefs->snapFormat);
  if (strList.isEmpty())
    prefs->snapFormat = "RGB";

  // Read each zoom with a separate key
  prefs->zoomsChgd = false;
  for (i = 0; i < MAXZOOMS; i++) {
    prefs->zoomsDflt[i] = szoomvals[i];
    str.sprintf(IMOD_NAME"zooms/%d", i);
    prefs->zooms[i] = settings->readDoubleEntry(str, szoomvals[i], &readin);
    if (readin)
      prefs->zoomsChgd = true;
  }

  // Read colors with separate keys
  for (i = 0; i < MAX_NAMED_COLORS; i++) {
    str.sprintf(IMOD_NAME"namedColors/%d", i);
    prefs->namedColor[i] = settings->readNumEntry
      (str, (int)prefs->namedColorDflt[i], &prefs->namedColorChgd[i]);
  }

  // Read the geometry information with separate keys
  for (i = 0; i < MAX_GEOMETRIES; i++) {
    mGeomImageXsize[i] = mGeomImageYsize[i] = 0;
    mGeomZapWin[i].setRect(0, 0, 0, 0);
    mGeomInfoWin[i].setRect(0, 0, 0, 0);
    str.sprintf(IMOD_NAME"geomImageSize/%d", i);
    str = settings->readEntry(str);
    if (!str.isEmpty())
      sscanf(str.latin1(), "%d,%d", &mGeomImageXsize[i], &mGeomImageYsize[i]);

    str.sprintf(IMOD_NAME"geomZapWindow/%d", i);
    str = settings->readEntry(str);
    if (!str.isEmpty()) {
      sscanf(str.latin1(), "%d,%d,%d,%d", &left, &top, &width, &height);
      mGeomZapWin[i].setRect(left, top, width, height);
    }

    str.sprintf(IMOD_NAME"geomInfoWindow/%d", i);
    str = settings->readEntry(str);
    if (!str.isEmpty()) {
      sscanf(str.latin1(), "%d,%d,%d,%d", &left, &top, &width, &height);
      mGeomInfoWin[i].setRect(left, top, width, height);
    }
  }

  prefs->autosaveInterval = settings->readNumEntry
    (IMOD_NAME"autosaveInterval", prefs->autosaveIntervalDflt,
     &prefs->autosaveIntervalChgd);
  prefs->autosaveDir = settings->readEntry(IMOD_NAME"autosaveDir",
                                          prefs->autosaveDirDflt,
                                          &prefs->autosaveDirChgd);
  prefs->autosaveOn = settings->readBoolEntry(IMOD_NAME"autosaveOn",
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
  str = settings->readEntry(IMOD_NAME"fontString");
  prefs->fontChgd = !str.isEmpty() && prefs->font.fromString(str);
  if (prefs->fontChgd)
    QApplication::setFont(prefs->font);
  else
    prefs->font = QApplication::font();
    
  // If user entered a valid style on the command line, save that style as the
  // current key and set changed flag to false
  // Just forbid highcolor due to crashes under RH 7.3
  str = cmdLineStyle ? cmdLineStyle : "";
  if (styleOK(str) && QStyleFactory::create(str) != NULL) {
    prefs->styleChgd = false;
    prefs->styleKey = str;

    // Otherwise, look for a key and use it; or set the key to windows
  } else {
    str = settings->readEntry(IMOD_NAME"styleKey");
    prefs->styleChgd = styleOK(str) &&  QStyleFactory::create(str) != NULL;
    if (prefs->styleChgd)
      prefs->styleKey = str;
    else
      prefs->styleKey = "windows";
    QApplication::setStyle(prefs->styleKey);
  }

  // Set status to 0; it will be 1 if OK after checking, -1 if not OK
  for (i = 0; i < MAX_STYLES; i++)
    styleStatus[i] = 0;

  delete settings;
}


bool ImodPreferences::styleOK(QString key)
{
  int i;
  QString str;
  if (key.isEmpty())
    return false;
#ifdef EXCLUDE_STYLES

  // If the list is of styles to exclude, return false if key is on the list
  for (i = 0; ; i++) {
    str = styleList[i];
    if (str.isEmpty())
      break;
    if (str.lower() == key.lower())
      return false;
  }
  return true;
#else

  // If the list is of styles to include, return true if on list and valid
  for (i = 0; ; i++) {
    str = styleList[i];
    if (str.isEmpty())
      break;
    if (str.lower() == key.lower() && QStyleFactory::create(str) != NULL)
      return true;
  }
  return false;
#endif
}

void ImodPreferences::saveSettings()
{
  ImodPrefStruct *prefs = &mCurrentPrefs;
  QString str, str2;
  int i, geomInd, firstEmpty;
  QSettings *settings = getSettingsObject();

  // Get current geometries of info and zap windows
  // first find where in the table this will go
  if (prefs->rememberGeom) {
    geomInd = -1;
    firstEmpty = MAX_GEOMETRIES - 1;
    for (i = 0; i < MAX_GEOMETRIES; i++) {
      /*  imodPrintStderr("i %d  xsize %d  ysize %d\n", i, mGeomImageXsize[i], 
          mGeomImageYsize[i]); */
      if (geomInd < 0 && App->cvi->xsize == mGeomImageXsize[i] &&
          App->cvi->ysize == mGeomImageYsize[i])
        geomInd = i;
      if (firstEmpty == MAX_GEOMETRIES - 1 && !mGeomImageXsize[i])
        firstEmpty = i;
    }

    /* imodPrintStderr("geomInd %d  firstEmpty %d xsize %d  ysize %d\n", 
       geomInd, firstEmpty, App->cvi->xsize, App->cvi->ysize); */

    // Move entries up if nothing in the table matches
    if (geomInd < 0) {
      for (i = firstEmpty; i > 0; i--) {
        mGeomImageXsize[i] = mGeomImageXsize[i - 1];
        mGeomImageYsize[i] = mGeomImageYsize[i - 1];
        mGeomZapWin[i] = mGeomZapWin[i - 1];
        mGeomInfoWin[i] = mGeomInfoWin[i - 1];
      }
      geomInd = 0;
      mGeomImageXsize[0] = App->cvi->xsize;
      mGeomImageYsize[0] = App->cvi->ysize;
    }

    // Get the current geometries and put in table
    mGeomInfoWin[geomInd] = ivwRestorableGeometry(ImodInfoWin);
    mGeomZapWin[geomInd] = mRecordedZapGeom;

    settings->writeEntry(IMOD_NAME"lastGeometrySaved", geomInd);

    for (i = 0; i < MAX_GEOMETRIES; i++) {
      if (mGeomImageXsize[i]) {
        str.sprintf(IMOD_NAME"geomImageSize/%d", i);
        str2.sprintf("%d,%d", mGeomImageXsize[i], mGeomImageYsize[i]);
        settings->writeEntry(str, str2);
        
        str.sprintf(IMOD_NAME"geomZapWindow/%d", i);
        str2.sprintf("%d,%d,%d,%d", mGeomZapWin[i].left(), 
                     mGeomZapWin[i].top(),
                     mGeomZapWin[i].width(), mGeomZapWin[i].height());
        settings->writeEntry(str, str2);
        
        str.sprintf(IMOD_NAME"geomInfoWindow/%d", i);
        str2.sprintf("%d,%d,%d,%d", mGeomInfoWin[i].left(), 
                     mGeomInfoWin[i].top(),
                     mGeomInfoWin[i].width(), mGeomInfoWin[i].height());
        settings->writeEntry(str, str2);
      }
    }
  }

  if (prefs->hotSliderKeyChgd)
    settings->writeEntry(IMOD_NAME"hotSliderKey", prefs->hotSliderKey);
  if (prefs->hotSliderFlagChgd)
    settings->writeEntry(IMOD_NAME"hotSliderFlag", prefs->hotSliderFlag);
  if (prefs->mouseMappingChgd)
    settings->writeEntry(IMOD_NAME"mouseMapping", prefs->mouseMapping);
  if (prefs->silentBeepChgd)
    settings->writeEntry(IMOD_NAME"silentBeep", prefs->silentBeep);
  if (prefs->tooltipsOnChgd)
    settings->writeEntry(IMOD_NAME"tooltipsOn", prefs->tooltipsOn);
  if (prefs->bwStepChgd)
    settings->writeEntry(IMOD_NAME"bwStep", prefs->bwStep);
  if (prefs->iconifyImodvDlgChgd)
    settings->writeEntry(IMOD_NAME"iconifyImodvDlg", prefs->iconifyImodvDlg);
  if (prefs->iconifyImodDlgChgd)
    settings->writeEntry(IMOD_NAME"iconifyImodDlg", prefs->iconifyImodDlg);
  if (prefs->iconifyImageWinChgd)
    settings->writeEntry(IMOD_NAME"iconifyImageWin", prefs->iconifyImageWin);
  if (prefs->minModPtSizeChgd)
    settings->writeEntry(IMOD_NAME"minModPtSize", prefs->minModPtSize);
  if (prefs->minImPtSizeChgd)
    settings->writeEntry(IMOD_NAME"minImPtSize", prefs->minImPtSize);
  if (prefs->rememberGeomChgd)
    settings->writeEntry(IMOD_NAME"rememberGeom", prefs->rememberGeom);
  if (prefs->autoTargetMeanChgd)
    settings->writeEntry(IMOD_NAME"autoTargetMean", prefs->autoTargetMean);
  if (prefs->autoTargetSDChgd)
    settings->writeEntry(IMOD_NAME"autoTargetSD", prefs->autoTargetSD);
  if (prefs->snapFormatChgd)
    settings->writeEntry(IMOD_NAME"snapFormat", prefs->snapFormat);
  if (prefs->snapQualityChgd)
    settings->writeEntry(IMOD_NAME"snapQuality", prefs->snapQuality);

  if (prefs->zoomsChgd) {
    for (i = 0; i < MAXZOOMS; i++) {
      str.sprintf(IMOD_NAME"zooms/%d", i);
      settings->writeEntry(str, prefs->zooms[i]); 
    }
  }

  for (i = 0; i < MAX_NAMED_COLORS; i++) {
    if (prefs->namedColorChgd[i]) {
      str.sprintf(IMOD_NAME"namedColors/%d", i);
      settings->writeEntry(str, (int)prefs->namedColor[i]);
    }
  }

  if (prefs->autosaveIntervalChgd)
    settings->writeEntry(IMOD_NAME"autosaveInterval", 
                            prefs->autosaveInterval);
  if (prefs->autosaveDirChgd)
    settings->writeEntry(IMOD_NAME"autosaveDir", prefs->autosaveDir);
  if (prefs->autosaveOnChgd)
    settings->writeEntry(IMOD_NAME"autosaveOn", prefs->autosaveOn);
  if (prefs->fontChgd) {
    str = prefs->font.toString();
    settings->writeEntry(IMOD_NAME"fontString", str);
  }
  if (prefs->styleChgd)
    settings->writeEntry(IMOD_NAME"styleKey", prefs->styleKey);

  // Write generic settings if any
  if (mGenericList) {
    GenericSettings *listSet = (GenericSettings *)ilistFirst(mGenericList);
    while (listSet) {
      for (i = 0; i < listSet->numVals; i++) {
        str.sprintf(IMOD_NAME"%s/%d", listSet->key, i);
        settings->writeEntry(str, listSet->values[i]);
      }
      listSet = (GenericSettings *)ilistNext(mGenericList);
    }
  }

  delete settings;
}

// Create the settings object and return pointer
QSettings *ImodPreferences::getSettingsObject()
{
  QSettings *settings = new QSettings();
  // The Mac format is not compatible with QT 3.0.5
  // Need to check if this will work on other systems without breaking old files.
  // Be sure to modify in saveSettings too
#ifdef Q_OS_MACX
  settings->setPath("", "3dmod", QSettings::User);
#else
  settings->insertSearchPath( QSettings::Windows, "/BL3DEMC" );
#endif
  return settings;
}



/* FUNCTIONS RELATED TO THE PREFERENCE SETTING DIALOG */

// Open the dialog to set preferences
void ImodPreferences::editPrefs()
{
  mDialogPrefs = mCurrentPrefs;
  mTabDlg = new QTabDialog(NULL, NULL, true, Qt::WDestructiveClose);
  mTabDlg->setOKButton("Done");
  mTabDlg->setCancelButton("Cancel");
  mTabDlg->setDefaultButton("Defaults for Tab");
  mAppearForm = new AppearanceForm();
  mTabDlg->addTab(mAppearForm, "Appearance");
  mBehaveForm = new BehaviorForm();
  mTabDlg->addTab(mBehaveForm, "Behavior");
  mMouseForm = new MouseForm();
  mTabDlg->addTab(mMouseForm, "Mouse");
  mTabDlg->setCaption("3dmod: Set preferences");
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

char **ImodPreferences::getStyleList()
{
  return &styleList[0];
}

int *ImodPreferences::getStyleStatus()
{
  return &styleStatus[0];
}

// When done is pressed, unload the properties, mark if any changed
void ImodPreferences::donePressed()
{
  ImodPrefStruct *newp = &mDialogPrefs;
  ImodPrefStruct oldPrefs = mCurrentPrefs;
  ImodPrefStruct *oldp = &oldPrefs;
  ImodPrefStruct *curp = &mCurrentPrefs;
  int i;
  bool autosaveChanged = false;
  mBehaveForm->unload();
  mAppearForm->unloadZoomValue();
  mCurrentPrefs = mDialogPrefs;

  curp->hotSliderKeyChgd |= newp->hotSliderKey != oldp->hotSliderKey;
  curp->hotSliderFlagChgd |= newp->hotSliderFlag != oldp->hotSliderFlag;
  curp->mouseMappingChgd |= newp->mouseMapping != oldp->mouseMapping;
  curp->silentBeepChgd |= !equiv(newp->silentBeep, oldp->silentBeep);
  if (!equiv(newp->tooltipsOn, oldp->tooltipsOn)) {
    curp->tooltipsOnChgd = true;
    QToolTip::setGloballyEnabled(curp->tooltipsOn);
  }

  curp->fontChgd |= newp->font != oldp->font;
  curp->styleChgd |= newp->styleKey.lower() != oldp->styleKey.lower();
  curp->bwStepChgd |= newp->bwStep != oldp->bwStep;
  curp->iconifyImodvDlgChgd |= !equiv(newp->iconifyImodvDlg, 
                                     oldp->iconifyImodvDlg);
  curp->iconifyImodDlgChgd |= !equiv(newp->iconifyImodDlg, 
                                    oldp->iconifyImodDlg);
  curp->iconifyImageWinChgd |= !equiv(newp->iconifyImageWin, 
                                     oldp->iconifyImageWin);
  curp->minModPtSizeChgd |= newp->minModPtSize != oldp->minModPtSize;
  curp->minImPtSizeChgd |= newp->minImPtSize != oldp->minImPtSize;
  curp->rememberGeomChgd |= !equiv(newp->rememberGeom, oldp->rememberGeom);
  curp->autoTargetMeanChgd |= newp->autoTargetMean != oldp->autoTargetMean;
  curp->autoTargetSDChgd |= newp->autoTargetSD != oldp->autoTargetSD;
  curp->snapFormatChgd |= newp->snapFormat != oldp->snapFormat;
  curp->snapQualityChgd |= newp->snapQuality != oldp->snapQuality;

  for (i = 0; i < MAXZOOMS; i++)
      curp->zoomsChgd |= newp->zooms[i] != oldp->zooms[i];

  for (i = 0; i < MAX_NAMED_COLORS; i++)
    curp->namedColorChgd[i] |= newp->namedColor[i] != oldp->namedColor[i];

  if (newp->autosaveInterval != oldp->autosaveInterval) {
    curp->autosaveIntervalChgd = true;
    autosaveChanged = true;
  }
  if (!equiv(newp->autosaveOn, oldp->autosaveOn)) {
    curp->autosaveOnChgd = true;
    autosaveChanged = true;
  }
  if (newp->autosaveDir != oldp->autosaveDir) {
    curp->autosaveDirChgd = true;
    autosaveChanged = true;
  }

  if (autosaveChanged)
    imod_start_autosave(App->cvi);

  findCurrentTab();
  mTabDlg = NULL;
  imcUpdateDialog();
  imodvMovieUpdate();
}

// When cancel is pressed, get the current tab then go through common cancel
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

// Restore defaults for the tab and update the dialogs
void ImodPreferences::defaultPressed()
{
  int i;
  ImodPrefStruct *prefs = &mDialogPrefs;
  findCurrentTab();
  
  switch(mCurrentTab) {
  case 0:
    prefs->minModPtSize = prefs->minModPtSizeDflt;
    prefs->minImPtSize = prefs->minImPtSizeDflt;
    for (i = 0; i < MAX_NAMED_COLORS; i++)
      prefs->namedColor[i] = prefs->namedColorDflt[i];
    prefs->autoTargetMean = prefs->autoTargetMeanDflt;
    prefs->autoTargetSD = prefs->autoTargetSDDflt;
    pointSizeChanged();
    for (i = 0; i < MAXZOOMS; i++)
      prefs->zooms[i] = prefs->zoomsDflt[i];
    mAppearForm->update();
    break;

  case 1: 
    prefs->silentBeep = prefs->silentBeepDflt;
    prefs->tooltipsOn = prefs->tooltipsOnDflt;
    prefs->bwStep = prefs->bwStepDflt;
    prefs->iconifyImodvDlg = prefs->iconifyImodvDlgDflt;
    prefs->iconifyImodDlg = prefs->iconifyImodDlgDflt;
    prefs->iconifyImageWin = prefs->iconifyImageWinDflt;
    prefs->rememberGeom = prefs->rememberGeomDflt;
    prefs->autosaveInterval = prefs->autosaveIntervalDflt;
    prefs->autosaveOn = prefs->autosaveOnDflt;
    prefs->autosaveDir = prefs->autosaveDirDflt;
    QToolTip::setGloballyEnabled(prefs->tooltipsOn);
    prefs->snapFormat = prefs->snapFormatDflt;
    prefs->snapQuality = prefs->snapQualityDflt;
    mBehaveForm->update();
    break;

  case 2: 
    prefs->hotSliderKey = prefs->hotSliderKeyDflt;    
    prefs->hotSliderFlag = prefs->hotSliderFlagDflt;
    prefs->mouseMapping = prefs->mouseMappingDflt;
    mMouseForm->update();
    break;
  }
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

// Change the font of all widgets by iteration
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

// Set the style - no iteration seems to be needed
void ImodPreferences::changeStyle(QString newKey)
{
  QApplication::setStyle(newKey);
  qApp->processEvents();

  // Lazy way to get the sizes right, just call the font setting pathway
  // Not very reliable, slightly better with the specific calls
  if (mTabDlg)
    changeFont(mDialogPrefs.font);
  else
    changeFont(mCurrentPrefs.font);
    //    changeFont(QApplication::font());
}

// Return a doctored list of snapshot formats, starting with RGB and leaving
// out binary formats
QStringList ImodPreferences::snapFormatList()
{
  QStringList retList = "RGB";
  QStringList formats = QImage::outputFormatList();
  for (int i = 0; i < formats.count(); i++)
    if (formats[i] != "PBM" && formats[i] != "XBM")
      retList << formats[i];
  return retList;
}


/* FUNCTIONS TO RETURN PREFERENCES SETTINGS TO THOSE WHO NEED THEM */

// Hot slider flag and key, accessed by global functions since they started
// that way
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

// The minimum sizes of current point markers
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

// A call to draw when the point size has changed
void ImodPreferences::pointSizeChanged()
{
  mapNamedColors();
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

// Return currently defined named color for the given index
QColor ImodPreferences::namedColor(int index)
{
  int i;
  for (i = 0; i < MAX_NAMED_COLORS; i++) {
    if (mCurrentPrefs.namedIndex[i] == index) {
      if (mTabDlg)
        return QColor(mDialogPrefs.namedColor[i]);
      return QColor(mCurrentPrefs.namedColor[i]);
    }
  }
  return QColor(0, 0, 0);
}

// Test the current style to see if it is Aqua, which needs rounding
bool ImodPreferences::getRoundedStyle()
{
  int index;
  if (mTabDlg)
    index = mDialogPrefs.styleKey.find(QString("aqua"), 0, false);
  else
    index = mCurrentPrefs.styleKey.find(QString("aqua"), 0, false);
  //imodPrintStderr("index = %d\n", index);
  return index >= 0;
}


// Set the geometry for the info window that matches current image or
// fall back to last one used
void ImodPreferences::setInfoGeometry()
{
  static int doneOnce = 0;
  int i, xx, yy;
  int indSave = -1;

  if (doneOnce || !mCurrentPrefs.rememberGeom)
    return;
  doneOnce++;

  for (i = 0; i < MAX_GEOMETRIES; i++)
    if (App->cvi->xsize == mGeomImageXsize[i] &&
        App->cvi->ysize == mGeomImageYsize[i]) {
      indSave = i;
      break;
    }
  if (indSave < 0 && mGeomLastSaved >= 0)
    indSave = mGeomLastSaved;
  if (indSave < 0 || !mGeomInfoWin[indSave].width() || 
      !mGeomInfoWin[indSave].height())
    return;

  // This is not good when going between systems due to font differences
  /* ImodInfoWin->resize(mGeomInfoWin[indSave].width(),
     mGeomInfoWin[indSave].height()); */
  // And we want to keep it on the screen in case last screen was bigger
  xx = mGeomInfoWin[indSave].x();
  yy = mGeomInfoWin[indSave].y();
  zapLimitWindowPos(ImodInfoWin->width(), ImodInfoWin->height(), xx, yy);
  ImodInfoWin->move(xx, yy);
}

// Return the geometry for the zap window that matches current image
QRect ImodPreferences::getZapGeometry()
{
  int i;
  if (mCurrentPrefs.rememberGeom) {
    for (i = 0; i < MAX_GEOMETRIES; i++)
      if (App->cvi->xsize == mGeomImageXsize[i] &&
          App->cvi->ysize == mGeomImageYsize[i])
        return (mGeomZapWin[i]);
  }
  return QRect(0, 0, 0, 0);
}

// Fetch the biggest zap window geometry before closing windows
void ImodPreferences::recordZapGeometry()
{
  mRecordedZapGeom = imodDialogManager.biggestGeometry(ZAP_WINDOW_TYPE);
}

void ImodPreferences::getAutoContrastTargets(int &mean, int &sd)
{
  mean = mCurrentPrefs.autoTargetMean;
  sd = mCurrentPrefs.autoTargetSD;
}


// Keep a list of settings to save under the given key
int ImodPreferences::saveGenericSettings(char *key, int numVals, double *values)
{
  GenericSettings genSet, *listSet;
  int i;

  if (!mGenericList)
    return 1;

  // First see if the key is already on list and just copy values if so
  listSet = (GenericSettings *)ilistFirst(mGenericList);
  while (listSet) {
    if (!strcmp(key, listSet->key)) {
      for (i = 0; i < listSet->numVals; i++)
        listSet->values[i] =  values[i];
      return 0;
    }
    listSet = (GenericSettings *)ilistNext(mGenericList);
  }

  // Copy the key and the values to this set
  genSet.key = strdup(key);
  genSet.numVals = numVals;
  genSet.values = (double *)malloc(numVals * sizeof(double));
  if (!genSet.key || !genSet.values)
    return 1;
  for (i = 0; i < numVals; i++)
        genSet.values[i] =  values[i];
  
  // Append to list and return 0 if OK
  ilistAppend(mGenericList, &genSet);
  return 0;
}

// Read the settings under the given key, return up to maxVals values
int ImodPreferences::getGenericSettings(char *key, double *values, int maxVals)
{
  QString str;
  QSettings *settings = getSettingsObject();
  bool readin;
  double val;
  int i;

  // Read each possible number and load into array if present, quit if not
  for (i = 0; i < maxVals; i++) {
    str.sprintf(IMOD_NAME"%s/%d", key, i);
    val = settings->readDoubleEntry(str, 0., &readin);
    if (!readin)
      break;
    values[i] = val;
  }
  delete settings;
  return i;
}


/*
$Log$
Revision 1.18  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 1.17  2004/11/02 20:18:05  mast
Added color settings, simplified dialog unload code, made default panel-only

Revision 1.16  2004/06/23 03:32:52  mast
Added ability to save generic settings from random callers

Revision 1.15  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 1.14  2004/05/19 15:41:36  mast
Changed to new setPath call on the Mac to get back to user's file with Qt 3.3

Revision 1.13  2003/09/25 21:09:19  mast
Make info window stay on the screen when it is positioned from settings

Revision 1.12  2003/09/24 17:39:52  mast
Moved log down

Revision 1.11  2003/09/24 17:38:53  mast
Switch to using restorable geometries, and setting info window position here

Revision 1.10  2003/09/24 00:48:55  mast
Switched from keeping track of geometry to keeping track of pos() and
size() when saving and restoring positions and sizes

Revision 1.9  2003/09/18 05:57:47  mast
Add autocontrast targets

Revision 1.8  2003/09/17 04:48:01  mast
Added ability to remember window geometries for Info and Zap windows

Revision 1.7  2003/09/15 21:05:18  mast
Changing zooms 0.35 -> 0.3333 and 0.16 -> 0.1667 for optimal drawing

Revision 1.6  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 1.5  2003/04/17 19:06:50  mast
various changes for Mac

Revision 1.4  2003/03/28 23:51:11  mast
changes for Mac problems

Revision 1.3  2003/03/26 23:06:32  mast
Only check status of a style once

Revision 1.2  2003/03/26 22:48:40  mast
Changed handling of style to prevent "already defined" messages

Revision 1.1  2003/03/24 17:55:19  mast
Initial implementation
*/
