/*  preferences.cpp - Manage preferences for Imod, using Qt settings file
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

/* Checklist for adding an item:
 * Add structure variable for item, default value, and boolean for change
 * Add function for returning current value
 * Set default value of the Dflt variable
 * Read the setting, including any checks on validity.  If not using READBOOL or READNUM,
 *    first set the Chgd variable from settings->contains(), and set the actual
 *    variable from the Dflt
 * Write the setting if changed in saveSettings()
 * In donePressed, record change in value
 * In defaultPressed, restore value in appropriate section
 * Update program state if needed in timerEvent or pointSizeChanged
 * In designer, add interface element
 * In the form_xxx.cpp, load interface and unload it
 *
 * Do not despair, just saving/restoring a program state variable just takes
 * one line in each place, see usewheelforsize, etc.
 * Hint: settings file on Linux is ~/.config/BL3DEMC/3dmod.conf
 */

#include <stdlib.h>
#include "preferences.h"
#include <qsettings.h>
#include <qtooltip.h>
#include <qtabwidget.h>
#include <qpushbutton.h>
#include <qstylefactory.h>
#include <qapplication.h>
#include <qdir.h>
#include <qdialogbuttonbox.h>
#include <qimage.h>
#include <qwidget.h>
#include <qobject.h>
//Added by qt3to4:
#include <QTimerEvent>
#include <QImageWriter>
#include "dia_qtutils.h"
#include "form_appearance.h"
#include "form_behavior.h"
#include "form_snapshot.h"
#include "form_mouse.h"
#include "imod.h"
#include "imodv.h"
#include "info_setup.h"
#include "cont_edit.h"
#include "info_cb.h"
#include "display.h"
#include "moviecon.h"
#include "mv_movie.h"
#include "workprocs.h"
#include "control.h"
#include "scalebar.h"

ImodPreferences *ImodPrefs;

#define MAX_STYLES 24

#ifdef EXCLUDE_STYLES
// This used to be a list of styles that crashed in RH7
static const char *styleList[] = {""};
#else
// It used to be done this way because 
// non-existent style made it search and generate "already exists" messages
static char *styleList[] = {"Windows",
                            "Motif", "CDE", "Plastique", "Cleanlooks",
#ifdef Q_OS_MACX
                            "Macintosh (Aqua)",
#endif
                            ""};
#endif
static int styleStatus[MAX_STYLES];

// Macros to reduce boilerplate!
// The # turns the argument into a string, the ## joins it to characters
#define READBOOL(a) prefs->a##Chgd = settings->contains(#a) ; prefs->a = settings->value(#a, prefs->a##Dflt).toBool();
#define READNUM(a) prefs->a##Chgd = settings->contains(#a) ; prefs->a = settings->value(#a, prefs->a##Dflt).toInt();
#define WRITE_IF_CHANGED(a) if (prefs->a##Chgd) settings->setValue(#a, prefs->a)

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
    {0.01, 0.02, 0.04, 0.07, 0.1, 0.1667, 0.25, 0.3333, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0,
     4.0, 5.0, 6.0, 8.0, 10.0, 12.0, 16.0, 20.0};
  int i, j, left, top, width, height, position, floatOn, subarea, readmax;
  bool readin;
  QString str;
  char *plugdir;
  QStringList strList;
  ScaleBar *scaleParm;
  ImodPrefStruct *prefs = &mCurrentPrefs;
  QSettings *settings = getSettingsObject();
  mTabDlg = NULL;
  mCurrentTab = 0;
  mTimerID = 0;
  mRecordedZapGeom = QRect(0, 0, 0, 0);
  mGenericList = ilistNew(sizeof(GenericSettings), 4);
  mMultiZgeom =  QRect(0, 0, 0, 0);
  mXyzApplyZscale = false;
  QCoreApplication::setOrganizationDomain("bio3d.colorado.edu");

  // Put plugin dir on the library path so image plugins can be found
  // Replace the default path so we don't get anything from our own Qt install,
  // Unless we are running an IMOD installation without an imageformats directory
  plugdir = getenv("IMOD_PLUGIN_DIR");
  if (plugdir)
    strList << plugdir;
  plugdir = getenv("IMOD_DIR");
  if (plugdir)
    strList << QString(plugdir) + QString("/lib/imodplug");

  // Add these for standalone 3dmod package
#ifdef _WIN32
  else
    strList << QString("C:/Program Files/IMOD/lib/imodplug") << 
      QString("C:/Program Files/3dmod/lib/imodplug");
#endif
  for (i = 0; i < strList.count(); i++)
    if (QFile::exists(strList[i] + "/imageformats"))
      break;
  if (strList.count() && i < strList.count())
    QApplication::setLibraryPaths(strList);

  // Set the default values
  prefs->hotSliderKeyDflt = 0;
  prefs->hotSliderFlagDflt = HOT_SLIDER_KEYUP;
  prefs->mouseMappingDflt = 0;
  prefs->modvSwapLeftMidDflt = false;
  prefs->silentBeepDflt = false;
  prefs->classicSlicerDflt = false;
  prefs->startInHQDflt = true;
  prefs->arrowsScrollZapDflt = false;
  //prefs->tooltipsOnDflt = true;
  prefs->startAtMidZDflt = true;
  prefs->autoConAtStartDflt = 1;
  prefs->attachToOnObjDflt = true;
  prefs->slicerNewSurfDflt = true;
  mNewObjPropsDflt.flags = 0;
  mNewObjPropsDflt.pdrawsize = 0;
  mNewObjPropsDflt.symbol = IOBJ_SYM_NONE;
  mNewObjPropsDflt.symsize = 3;
  mNewObjPropsDflt.linewidth2 = 1;
  mNewObjPropsDflt.symflags = 0;
  mNewObjPropsDflt.pointLimit = 0;
  mNewObjPropsDflt.fillTrans = 0;
  prefs->bwStepDflt = 3;
  prefs->pageStepDflt = 10;
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
  prefs->namedIndex[8] = App->arrow[0];
  prefs->namedIndex[9] = App->arrow[1];
  prefs->namedIndex[10] = App->arrow[2];
  prefs->namedIndex[11] = App->arrow[3];
  prefs->namedColorDflt[0] = qRgb(255, 255,   0);
  prefs->namedColorDflt[1] = qRgb(185, 185,   0);
  prefs->namedColorDflt[2] = qRgb(255,   0,   0);
  prefs->namedColorDflt[3] = qRgb(  0, 255,   0);
  prefs->namedColorDflt[4] = qRgb(255, 255, 128);
  prefs->namedColorDflt[5] = qRgb(255, 255, 128);
  prefs->namedColorDflt[6] = qRgb( 64,  64,  96);
  prefs->namedColorDflt[7] = qRgb( 16,  16,  16);  
  prefs->namedColorDflt[8] = qRgb(255,   0, 160);
  prefs->namedColorDflt[9] = qRgb(  0, 255,   0);
  prefs->namedColorDflt[10] = qRgb(  0, 170, 255);
  prefs->namedColorDflt[11] = qRgb(255, 170,   0);
#ifdef Q_OS_IRIX
  prefs->snapFormatDflt = "RGB";
#else
  prefs->snapFormatDflt = "JPEG";
#endif
  prefs->snapQualityDflt = 80;
  prefs->snapDPIDflt = 0;
  prefs->scaleSnapDPI = false;
  prefs->slicerPanKbDflt = 3000;
  prefs->speedupSliderDflt = false;
  prefs->loadUshortsDflt = false;
  prefs->isoHighThreshDflt = false;
  prefs->isoBoxLimitDflt = 320;
  prefs->isoBoxInitialDflt = 96;

  READNUM(hotSliderKey);
  READNUM(hotSliderFlag);
  READNUM(mouseMapping);
  READBOOL(modvSwapLeftMid);
  READBOOL(silentBeep);
  READBOOL(classicSlicer);
  mClassicWarned = settings->value("classicWarned").toBool();
  if (prefs->classicSlicer)
    mClassicWarned = true;
  READBOOL(startInHQ);
  READBOOL(arrowsScrollZap);
  //READBOOL(tooltipsOn);
  //QToolTip::setGloballyEnabled(prefs->tooltipsOn);
  READNUM(autoConAtStart);
  READBOOL(startAtMidZ);
  READBOOL(attachToOnObj);
  READBOOL(slicerNewSurf);

  READNUM(bwStep);
  READNUM(pageStep);
  READBOOL(iconifyImodvDlg);
  READBOOL(iconifyImodDlg);
  READBOOL(iconifyImageWin);
  READNUM(minModPtSize);
  READNUM(minImPtSize);
  READBOOL(rememberGeom);
  mGeomLastSaved = settings->value("lastGeometrySaved", -1).toInt();
  READNUM(autoTargetMean);
  READNUM(autoTargetSD);
  prefs->snapFormatChgd = settings->contains("snapFormat");
  prefs->snapFormat = settings->value("snapFormat",
                                      prefs->snapFormatDflt).toString();
  READNUM(snapQuality);
  READNUM(snapDPI);
  READBOOL(scaleSnapDPI);
  READNUM(slicerPanKb);
  READBOOL(speedupSlider);
  READBOOL(loadUshorts);
  READBOOL(isoHighThresh);
  READNUM(isoBoxInitial);
  READNUM(isoBoxLimit);

  // Make sure an output format is on the list; if not drop to PNG then RGB
  strList = (snapFormatList()).filter(prefs->snapFormat);
  if (strList.isEmpty())
    prefs->snapFormat = "PNG";
  strList = (snapFormatList()).filter(prefs->snapFormat);
  if (strList.isEmpty())
    prefs->snapFormat = "RGB";

  // Read each zoom with a separate key
  prefs->zoomsChgd = false;
  readmax = 0;
  for (i = 0; i < MAXZOOMS; i++) {
    prefs->zoomsDflt[i] = szoomvals[i];
    prefs->zooms[i] = szoomvals[i];
    str.sprintf("zooms/%d", i);
    if (settings->contains(str))
      readmax = i + 1;
  }
  prefs->zoomsChgd = readmax > 0;

  // Shift zooms up if they were not all present before (new zooms are added at bottom)
  for (i = 0; i < MAXZOOMS; i++) {
    j = i + MAXZOOMS - readmax;
    str.sprintf("zooms/%d", i);
    if (settings->contains(str))
      prefs->zooms[j] = settings->value(str, szoomvals[j]).toDouble();
  }

  // Read colors with separate keys
  for (i = 0; i < MAX_NAMED_COLORS; i++) {
    str.sprintf("namedColors/%d", i);
    prefs->namedColorChgd[i] = settings->contains(str);
    prefs->namedColor[i] = settings->value
      (str, (int)prefs->namedColorDflt[i]).toInt();
  }

  // Read the geometry information with separate keys
  readin = false;
  for (i = 0; i < MAX_GEOMETRIES; i++) {
    mGeomImageXsize[i] = mGeomImageYsize[i] = 0;
    mGeomZapWin[i].setRect(0, 0, 0, 0);
    mGeomInfoWin[i].setRect(0, 0, 0, 0);
    str.sprintf("geomImageSize/%d", i);
    if (settings->contains(str)) {
      str = settings->value(str).toString();
      sscanf(LATIN1(str), "%d,%d", &mGeomImageXsize[i], &mGeomImageYsize[i]);
      readin = true;
    }

    str.sprintf("geomZapWindow/%d", i);
    if (settings->contains(str)) {
      str = settings->value(str).toString();
      sscanf(LATIN1(str), "%d,%d,%d,%d", &left, &top, &width, &height);
      mGeomZapWin[i].setRect(left, top, width, height);
      readin = true;
    }

    str.sprintf("geomInfoWindow/%d", i);
    if (settings->contains(str)) {
      str = settings->value(str).toString();
      sscanf(LATIN1(str), "%d,%d,%d,%d", &left, &top, &width, &height);
      mGeomInfoWin[i].setRect(left, top, width, height);
      readin = true;
    }
  }

  // If no geometries were found, stop classic slicer warning
  if (!readin)
    mClassicWarned = true;

  // Get new object properties
  mNewObjProps = mNewObjPropsDflt;
  mNewObjPropsChgd= settings->contains("newObjectProps");
  if (mNewObjPropsChgd) {
    str = settings->value("newObjectProps").toString();
    sscanf(LATIN1(str), "%u,%d,%d,%d,%d,%d,%d,%d", &mNewObjProps.flags, 
           &mNewObjProps.pdrawsize, &mNewObjProps.symbol, &mNewObjProps.symsize,
           &mNewObjProps.linewidth2, &mNewObjProps.symflags, &mNewObjProps.pointLimit, 
           &mNewObjProps.fillTrans);
    B3DCLAMP(mNewObjProps.pdrawsize, 0, 999);
    B3DCLAMP(mNewObjProps.symbol, 0, IOBJ_SYM_LAST);
    B3DCLAMP(mNewObjProps.symsize, 0, 100);
    B3DCLAMP(mNewObjProps.linewidth2, 0, 10);
    B3DCLAMP(mNewObjProps.fillTrans, 0, 100);
  }

  // Get multi Z window geometry and params
  if (settings->contains("geomMultiZwindow")) {
    str = settings->value("geomMultiZwindow").toString();
    sscanf(LATIN1(str), "%d,%d,%d,%d", &left, &top, &width, &height);
    mMultiZgeom.setRect(left, top, width, height);
  }
  if (settings->contains("multiZparams")) {
    str = settings->value("multiZparams").toString();
    sscanf(LATIN1(str), "%d,%d,%d,%d,%d", &mMultiZnumX, &mMultiZnumY, 
           &mMultiZstep, &mMultiZdrawCen, &mMultiZdrawOthers);
  }

  // Get scale bar params
  if (settings->contains("scaleBarParams")) {
    str = settings->value("scaleBarParams").toString();
    scaleParm = scaleBarGetParams();
    sscanf(LATIN1(str), "%d,%d,%d,%d,%d,%d,%d,%d,%d", &left, &top, 
           &scaleParm->minLength, &scaleParm->thickness, &position,
           &scaleParm->indentX, &scaleParm->indentY, &width, 
           &scaleParm->customVal);
    scaleParm->draw = left != 0;
    scaleParm->white = top != 0;
    scaleParm->useCustom = width != 0;
    scaleParm->position = position % 4;
    scaleParm->vertical = (position & 4) != 0;
    scaleParm->colorRamp = (position & 8) != 0;
    scaleParm->invertRamp = (position & 16) != 0;
  }

  floatOn = settings->value("floatButton").toInt();
  subarea = settings->value("subareaButton").toInt();
  imodInfoSetFloatFlags(floatOn, subarea);
  i = settings->value("useWheelForSize").toInt();
  iceSetWheelForSize(i);
  mXyzApplyZscale = settings->value("xyzApplyZscale").toBool();
  if (settings->contains("montageSnapshots")) {
    str = settings->value("montageSnapshots").toString();
    sscanf(LATIN1(str), "%d,%d,%d,%d,%d", &left, &top, &i, &width, &position);
    imcSetSnapMontage(left != 0);
    imcSetMontageFactor(top);
    imcSetSnapWholeMont(i != 0);
    imcSetScaleSizes(width != 0);
    imcSetSizeScaling(position);
  }
  if (settings->contains("slicerMontSnapshots")) {
    str = settings->value("slicerMontSnapshots").toString();
    sscanf(LATIN1(str), "%d,%d,%d,%d", &left, &top, &width, &position);
    imcSetSlicerMontage(left != 0);
    imcSetSlicerMontFactor(top);
    imcSetScaleThicks(width != 0);
    imcSetThickScaling(position);
  }

  READNUM(autosaveInterval);
  prefs->autosaveDirChgd = settings->contains("autosaveDir");
  prefs->autosaveDir = settings->value("autosaveDir",
                                       prefs->autosaveDirDflt).toString();
  READBOOL(autosaveOn);

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
  str = settings->value("fontString").toString();
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
    str = settings->value("styleKey").toString();
    prefs->styleChgd = styleOK(str) &&  QStyleFactory::create(str) != NULL;
    if (prefs->styleChgd)
      prefs->styleKey = str;
    else {
#ifdef Q_OS_MACX
      prefs->styleKey = "Cleanlooks";
#else
      prefs->styleKey = "Windows";
#endif
    }
    QApplication::setStyle(prefs->styleKey);
  }

  // Set status to 0; it will be 1 if OK after checking, -1 if not OK
  for (i = 0; i < MAX_STYLES; i++)
    styleStatus[i] = 0;

  mCurrentTab = settings->value("currentPrefsTab").toInt();
  mCurrentTab = B3DMAX(0, B3DMIN(3, mCurrentTab));
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
    if (str.toLower() == key.toLower())
      return false;
  }
  return true;
#else

  // If the list is of styles to include, return true if on list and valid
  for (i = 0; ; i++) {
    str = styleList[i];
    if (str.isEmpty())
      break;
    if (str.toLower() == key.toLower() && QStyleFactory::create(str) != NULL)
      return true;
  }
  return false;
#endif
}

void ImodPreferences::saveSettings(int modvAlone)
{
  ImodPrefStruct *prefs = &mCurrentPrefs;
  QString str, str2;
  int i, geomInd, firstEmpty, floatOn, subarea;
  QSettings *settings = getSettingsObject();
  ScaleBar *scaleParm = scaleBarGetParams();

  // Get current geometries of info and zap windows
  // first find where in the table this will go
  if (prefs->rememberGeom && !modvAlone) {
    geomInd = -1;
    firstEmpty = MAX_GEOMETRIES - 1;
    for (i = 0; i < MAX_GEOMETRIES; i++) {
      /* imodPrintStderr("i %d  xsize %d  ysize %d\n", i, mGeomImageXsize[i], 
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

    settings->setValue("lastGeometrySaved", geomInd);

    for (i = 0; i < MAX_GEOMETRIES; i++) {
      if (mGeomImageXsize[i]) {
        str.sprintf("geomImageSize/%d", i);
        str2.sprintf("%d,%d", mGeomImageXsize[i], mGeomImageYsize[i]);
        settings->setValue(str, str2);
        
        str.sprintf("geomZapWindow/%d", i);
        str2.sprintf("%d,%d,%d,%d", mGeomZapWin[i].left(), 
                     mGeomZapWin[i].top(),
                     mGeomZapWin[i].width(), mGeomZapWin[i].height());
        settings->setValue(str, str2);
        
        str.sprintf("geomInfoWindow/%d", i);
        str2.sprintf("%d,%d,%d,%d", mGeomInfoWin[i].left(), 
                     mGeomInfoWin[i].top(),
                     mGeomInfoWin[i].width(), mGeomInfoWin[i].height());
        settings->setValue(str, str2);
      }
    }
  }

  if (mMultiZgeom.width()) {
    str.sprintf("%d,%d,%d,%d", mMultiZgeom.left(), mMultiZgeom.top(),
                mMultiZgeom.width(), mMultiZgeom.height());
    settings->setValue("geomMultiZwindow", str);
    str.sprintf("%d,%d,%d,%d,%d", mMultiZnumX, mMultiZnumY, mMultiZstep,
                mMultiZdrawCen, mMultiZdrawOthers);
    settings->setValue("multiZparams", str);
  }

  WRITE_IF_CHANGED(hotSliderKey);
  WRITE_IF_CHANGED(hotSliderFlag);
  WRITE_IF_CHANGED(mouseMapping);
  WRITE_IF_CHANGED(modvSwapLeftMid);
  WRITE_IF_CHANGED(silentBeep);
  WRITE_IF_CHANGED(classicSlicer);
  settings->setValue("classicWarned", mClassicWarned);
  WRITE_IF_CHANGED(startInHQ);
  WRITE_IF_CHANGED(arrowsScrollZap);
  //WRITE_IF_CHANGED(tooltipsOn);
  WRITE_IF_CHANGED(autoConAtStart);
  WRITE_IF_CHANGED(startAtMidZ);
  WRITE_IF_CHANGED(attachToOnObj);
  WRITE_IF_CHANGED(slicerNewSurf);
  WRITE_IF_CHANGED(bwStep);
  WRITE_IF_CHANGED(pageStep);
  WRITE_IF_CHANGED(iconifyImodvDlg);
  WRITE_IF_CHANGED(iconifyImodDlg);
  WRITE_IF_CHANGED(iconifyImageWin);
  WRITE_IF_CHANGED(minModPtSize);
  WRITE_IF_CHANGED(minImPtSize);
  WRITE_IF_CHANGED(rememberGeom);
  WRITE_IF_CHANGED(autoTargetMean);
  WRITE_IF_CHANGED(autoTargetSD);
  WRITE_IF_CHANGED(snapFormat);
  WRITE_IF_CHANGED(snapQuality);
  WRITE_IF_CHANGED(snapDPI);
  WRITE_IF_CHANGED(scaleSnapDPI);
  WRITE_IF_CHANGED(slicerPanKb);
  WRITE_IF_CHANGED(speedupSlider);
  WRITE_IF_CHANGED(loadUshorts);
  WRITE_IF_CHANGED(isoHighThresh);
  WRITE_IF_CHANGED(isoBoxLimit);
  WRITE_IF_CHANGED(isoBoxInitial);

  if (mNewObjPropsChgd) {
    str.sprintf("%u,%d,%d,%d,%d,%d,%d,%d,%d,%d", mNewObjProps.flags,
                mNewObjProps.pdrawsize, mNewObjProps.symbol, mNewObjProps.symsize,
                mNewObjProps.linewidth2, mNewObjProps.symflags, mNewObjProps.pointLimit,
                mNewObjProps.fillTrans,0,0);
    settings->setValue("newObjectProps", str);
  }

  if (prefs->zoomsChgd) {
    for (i = 0; i < MAXZOOMS; i++) {
      str.sprintf("zooms/%d", i);
      settings->setValue(str, prefs->zooms[i]); 
    }
  }

  for (i = 0; i < MAX_NAMED_COLORS; i++) {
    if (prefs->namedColorChgd[i]) {
      str.sprintf("namedColors/%d", i);
      settings->setValue(str, (int)prefs->namedColor[i]);
    }
  }

  str.sprintf("%d,%d,%d,%d,%d,%d,%d,%d,%d", scaleParm->draw ? 1 : 0, 
              scaleParm->white ? 1 : 0, scaleParm->minLength,
              scaleParm->thickness, 
              scaleParm->position + (scaleParm->vertical ? 4 : 0) +
              (scaleParm->colorRamp ? 8 : 0) + (scaleParm->invertRamp ? 16 : 0)
              ,scaleParm->indentX, scaleParm->indentY, 
              scaleParm->useCustom ? 1: 0, scaleParm->customVal);
  settings->setValue("scaleBarParams", str);

  imodInfoGetFloatFlags(floatOn, subarea);
  settings->setValue("floatButton", floatOn);
  settings->setValue("subareaButton", subarea);
  settings->setValue("useWheelForSize", iceGetWheelForSize());
  settings->setValue("xyzApplyZscale", mXyzApplyZscale);
  str.sprintf("%d,%d,%d,%d,%d", imcGetSnapMontage(false) ? 1 : 0, 
              imcGetMontageFactor(), imcGetSnapWholeMont() ? 1: 0,
              imcGetScaleSizes() ? 1 : 0, imcGetSizeScaling());
  settings->setValue("montageSnapshots", str);
  str.sprintf("%d,%d,%d,%d", imcGetSlicerMontage(false) ? 1 : 0, 
              imcGetSlicerMontFactor(), 
              imcGetScaleThicks() ? 1 : 0, imcGetThickScaling());
  settings->setValue("slicerMontSnapshots", str);

  WRITE_IF_CHANGED(autosaveInterval);
  WRITE_IF_CHANGED(autosaveDir);
  WRITE_IF_CHANGED(autosaveOn);
  if (prefs->fontChgd) {
    str = prefs->font.toString();
    settings->setValue("fontString", str);
  }
  if (prefs->styleChgd)
    settings->setValue("styleKey", prefs->styleKey);

  // Write generic settings if any
  if (mGenericList) {
    GenericSettings *listSet = (GenericSettings *)ilistFirst(mGenericList);
    while (listSet) {
      for (i = 0; i < listSet->numVals; i++) {
        str.sprintf("%s/%d", listSet->key, i);
        settings->setValue(str, listSet->values[i]);
      }
      listSet = (GenericSettings *)ilistNext(mGenericList);
    }
  }
  settings->setValue("currentPrefsTab", mCurrentTab);

  delete settings;
}

// Create the settings object and return pointer
QSettings *ImodPreferences::getSettingsObject()
{
  QSettings *settings = new QSettings("BL3DEMC", "3dmod");
  /*  // The Mac format is not compatible with QT 3.0.5
  // Need to check if this will work on other systems without breaking old files.
  // Be sure to modify in saveSettings too
#ifdef Q_OS_MACX
  settings->setPath("", "3dmod", QSettings::UserScope);
#else
  settings->insertSearchPath( QSettings::Windows, "/BL3DEMC" );
  #endif */
  return settings;
}



/* FUNCTIONS RELATED TO THE PREFERENCE SETTING DIALOG */

// Open the dialog to set preferences
void ImodPreferences::editPrefs()
{
  // Return if already open: Mac 3dmod menu allows double opening
  if (mTabDlg)
    return;
  mDialogPrefs = mCurrentPrefs;
  mTabDlg = new PrefsDialog(NULL);
  mTabDlg->mTabWidget->setCurrentIndex(mCurrentTab);
  mTabDlg->show();
}

const char **ImodPreferences::getStyleList()
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
  mTabDlg->mBehaveForm->unload();
  mTabDlg->mAppearForm->unload();
  mTabDlg->mSnapForm->unload();
  mCurrentPrefs = mDialogPrefs;

  curp->hotSliderKeyChgd |= newp->hotSliderKey != oldp->hotSliderKey;
  curp->hotSliderFlagChgd |= newp->hotSliderFlag != oldp->hotSliderFlag;
  curp->mouseMappingChgd |= newp->mouseMapping != oldp->mouseMapping;
  curp->modvSwapLeftMidChgd |= !equiv(newp->modvSwapLeftMid, oldp->modvSwapLeftMid);
  curp->silentBeepChgd |= !equiv(newp->silentBeep, oldp->silentBeep);
  curp->autoConAtStartChgd |= newp->autoConAtStart != oldp->autoConAtStart;
  curp->startAtMidZChgd |= !equiv(newp->startAtMidZ, oldp->startAtMidZ);
  curp->attachToOnObjChgd |= !equiv(newp->attachToOnObj, oldp->attachToOnObj);
  curp->slicerNewSurfChgd |= !equiv(newp->slicerNewSurf, oldp->slicerNewSurf);
  curp->classicSlicerChgd |= !equiv(newp->classicSlicer, oldp->classicSlicer);
  curp->startInHQChgd |= !equiv(newp->startInHQ, oldp->startInHQ);
  curp->arrowsScrollZapChgd |= !equiv(newp->arrowsScrollZap, oldp->arrowsScrollZap);
  /*if (!equiv(newp->tooltipsOn, oldp->tooltipsOn)) {
    curp->tooltipsOnChgd = true;
    //QToolTip::setGloballyEnabled(curp->tooltipsOn);
    } */

  curp->fontChgd |= newp->font != oldp->font;
  curp->styleChgd |= newp->styleKey.toLower() != oldp->styleKey.toLower();
  curp->bwStepChgd |= newp->bwStep != oldp->bwStep;
  curp->pageStepChgd |= newp->pageStep != oldp->pageStep;
  curp->iconifyImodvDlgChgd |= !equiv(newp->iconifyImodvDlg, oldp->iconifyImodvDlg);
  curp->iconifyImodDlgChgd |= !equiv(newp->iconifyImodDlg, oldp->iconifyImodDlg);
  curp->iconifyImageWinChgd |= !equiv(newp->iconifyImageWin, oldp->iconifyImageWin);
  curp->minModPtSizeChgd |= newp->minModPtSize != oldp->minModPtSize;
  curp->minImPtSizeChgd |= newp->minImPtSize != oldp->minImPtSize;
  curp->rememberGeomChgd |= !equiv(newp->rememberGeom, oldp->rememberGeom);
  curp->autoTargetMeanChgd |= newp->autoTargetMean != oldp->autoTargetMean;
  curp->autoTargetSDChgd |= newp->autoTargetSD != oldp->autoTargetSD;
  curp->snapFormatChgd |= newp->snapFormat != oldp->snapFormat;
  curp->snapQualityChgd |= newp->snapQuality != oldp->snapQuality;
  curp->snapDPIChgd |= newp->snapDPI != oldp->snapDPI;
  curp->scaleSnapDPIChgd |= !equiv(newp->scaleSnapDPI, oldp->scaleSnapDPI);
  curp->slicerPanKbChgd |= newp->slicerPanKb != oldp->slicerPanKb;
  curp->speedupSliderChgd |= !equiv(newp->speedupSlider, oldp->speedupSlider);
  curp->loadUshortsChgd |= !equiv(newp->loadUshorts, oldp->loadUshorts);
  curp->isoHighThreshChgd |= !equiv(newp->isoHighThresh, oldp->isoHighThresh);
  curp->isoBoxLimitChgd |= newp->isoBoxLimit != oldp->isoBoxLimit;
  curp->isoBoxInitialChgd |= newp->isoBoxInitial != oldp->isoBoxInitial;

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
  mTabDlg->close();
  mTabDlg = NULL;
  imcUpdateDialog();
  mvMovieUpdate();
}

// When cancel is pressed, get the current tab then go through common cancel
void ImodPreferences::cancelPressed()
{
  findCurrentTab();
  userCanceled();
}

// Whenever there is a cancel by Cancel, close, or escape, 
// need to check for font or style change and restore
// set the timerID non-zero before starting timer to reject re-entrant calls
void ImodPreferences::userCanceled()
{
  if (!mTabDlg || mTimerID)
    return;
  mTimerID = 1;
  mTabDlg->close();
  mTabDlg = NULL;

  mTimerID = startTimer(10);
}


void ImodPreferences::timerEvent(QTimerEvent *e)
{
  killTimer(mTimerID);
  mTimerID = 0;

  if (mDialogPrefs.styleKey != mCurrentPrefs.styleKey)
    changeStyle(mCurrentPrefs.styleKey);
  if (mDialogPrefs.font != mCurrentPrefs.font)
    changeFont(mCurrentPrefs.font);
  //QToolTip::setGloballyEnabled(mCurrentPrefs.tooltipsOn);
    
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
    prefs->slicerPanKb = prefs->slicerPanKbDflt;
    prefs->speedupSlider = prefs->speedupSliderDflt;
    prefs->isoHighThresh = prefs->isoHighThreshDflt;
    prefs->isoBoxLimit = prefs->isoBoxLimitDflt;
    prefs->isoBoxInitial = prefs->isoBoxInitialDflt;
    mTabDlg->mAppearForm->update();
    break;

  case 1:
    prefs->snapFormat = prefs->snapFormatDflt;
    prefs->snapQuality = prefs->snapQualityDflt;
    prefs->snapDPI = prefs->snapDPIDflt;
    prefs->scaleSnapDPI =  prefs->scaleSnapDPIDflt;
    mTabDlg->mSnapForm->update();
    break;

  case 2: 
    prefs->silentBeep = prefs->silentBeepDflt;
    //prefs->tooltipsOn = prefs->tooltipsOnDflt;
    prefs->autoConAtStart = prefs->autoConAtStartDflt;
    prefs->startAtMidZ = prefs->startAtMidZDflt;
    prefs->startInHQ = prefs->startInHQDflt;
    prefs->arrowsScrollZap = prefs->arrowsScrollZapDflt;
    prefs->loadUshorts = prefs->loadUshortsDflt;
    prefs->attachToOnObj = prefs->attachToOnObjDflt;
    prefs->slicerNewSurf = prefs->slicerNewSurfDflt;
    prefs->bwStep = prefs->bwStepDflt;
    prefs->pageStep = prefs->pageStepDflt;
    prefs->iconifyImodvDlg = prefs->iconifyImodvDlgDflt;
    prefs->iconifyImodDlg = prefs->iconifyImodDlgDflt;
    prefs->iconifyImageWin = prefs->iconifyImageWinDflt;
    prefs->rememberGeom = prefs->rememberGeomDflt;
    prefs->autosaveInterval = prefs->autosaveIntervalDflt;
    prefs->autosaveOn = prefs->autosaveOnDflt;
    prefs->autosaveDir = prefs->autosaveDirDflt;
    //QToolTip::setGloballyEnabled(prefs->tooltipsOn);
    mTabDlg->mBehaveForm->update();
    break;

  case 3: 
    prefs->hotSliderKey = prefs->hotSliderKeyDflt;    
    prefs->hotSliderFlag = prefs->hotSliderFlagDflt;
    prefs->mouseMapping = prefs->mouseMappingDflt;
    prefs->modvSwapLeftMid = prefs->modvSwapLeftMidDflt;
    mTabDlg->mMouseForm->update();
    break;
  }
}

// Determine the currently shown tab so it can be set next time
void ImodPreferences::findCurrentTab()
{
  mCurrentTab = mTabDlg->mTabWidget->currentIndex();
}

// Change the font of all widgets by iteration
void ImodPreferences::changeFont(QFont newFont)
{
  QApplication::setFont(newFont);

  // Get a list of toplevel widgets and iterate over it
  QWidgetList topList = QApplication::topLevelWidgets();

  for (int i = 0; i < topList.size(); i++) {
    QWidget *widget = topList.at(i);

    // get a list of children that are widgets and iterate over it
    //QObjectList objectList = widget->queryList("QWidget", 0, 0, true);
    QList<QWidget *> objectList = widget->findChildren<QWidget *>();
    for (int j = 0; j < objectList.size(); j++) {
      QWidget *child = objectList.at(j);
      child->setFont(newFont);
    }

    // Change font of toplevel last
    widget->setFont(newFont);
  }
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
// out binary formats and their TIF/TIFF and eliminating JPEG/JPG duplication
QStringList ImodPreferences::snapFormatList()
{
  QStringList retList;
  QString str;
  QList<QByteArray> formats = QImageWriter::supportedImageFormats();
  bool gotJPEG = false;
  for (int i = 0; i < formats.count(); i++) {
    str = formats[i];
    str = str.toUpper();
    if (Imod_debug)
      imodPrintStderr("%s  ", LATIN1(str));
    if (str == "JPG") 
      str = "JPEG";
    if (str != "PBM" && str != "XBM" && str != "TIF" && str != "TIFF" && 
        !(str == "JPEG" && gotJPEG))
      retList << str;
    if (str == "JPEG")
      gotJPEG = true;
  }
  retList << "RGB";
  if (Imod_debug)
    imodPuts(" ");
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

bool ImodPreferences::hotSliderActive(int ctrlPressed)
{
  return ((hotSliderFlag() == HOT_SLIDER_KEYDOWN && ctrlPressed) ||
          (hotSliderFlag() == HOT_SLIDER_KEYUP && !ctrlPressed));
}

// Return the second snap format: return PNG if the first one is not PNG, or
// JPEG if first one is PNG; but return empty if selected one doesn't exist
QString ImodPreferences::snapFormat2(QString *curFormat)
{
  QStringList formats = snapFormatList();
  QString format2 = "PNG";
  QString current = snapFormat();
  if (curFormat)
    current = *curFormat;
  if (current == "PNG")
    format2 = "JPEG";
  for (int i = 0; i < formats.count(); i++)
    if (formats[i] == format2)
      return format2;
  return QString("");
}

// Temporarily set the snap format to the second format if it exists
void ImodPreferences::set2ndSnapFormat()
{
  QString str;
  mSavedSnapFormat = mCurrentPrefs.snapFormat;
  str = snapFormat2();
  if (!str.isEmpty())
    mCurrentPrefs.snapFormat = str;
}

// Restore the saved snap format
void ImodPreferences::restoreSnapFormat()
{
  mCurrentPrefs.snapFormat = mSavedSnapFormat;
}

void ImodPreferences::setSnapQuality(int value)
{
  mCurrentPrefs.snapQuality = value;
  mCurrentPrefs.snapQualityChgd = true;
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

// Return code for actual button in model view
int ImodPreferences::actualModvButton(int logicalButton)
{
  int qtButtons[3] = {Qt::LeftButton, Qt::MidButton, Qt::RightButton};
  if (logicalButton < 3 && mCurrentPrefs.modvSwapLeftMid)
    logicalButton = 3 - logicalButton;
  return qtButtons[logicalButton - 1];
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
  QString convDir = curdir->cleanPath(QString(envDir));
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
    index = mDialogPrefs.styleKey.indexOf(QString("aqua"), 0, 
                                          Qt::CaseInsensitive);
  else
    index = mCurrentPrefs.styleKey.indexOf(QString("aqua"), 0,
                                           Qt::CaseInsensitive);
  //imodPrintStderr("index = %d\n", index);
  return index >= 0;
}

// Set the default new object properties from the current object
void ImodPreferences::setDefaultObjProps()
{
  Iobj *obj = imodObjectGet(App->cvi->imod);
  if (!obj)
    return;
  mNewObjProps.flags = obj->flags;
  mNewObjProps.pdrawsize = obj->pdrawsize;
  mNewObjProps.symbol = obj->symbol;
  mNewObjProps.symsize = obj->symsize;
  mNewObjProps.linewidth2 = obj->linewidth2;
  mNewObjProps.symflags = obj->symflags;
  mNewObjProps.pointLimit = obj->extra[IOBJ_EX_PNT_LIMIT];
  mNewObjProps.fillTrans = obj->extra[IOBJ_EX_2D_TRANS];
  mNewObjPropsChgd = true;
}

// restore default new object properties
void ImodPreferences::restoreDefaultObjProps()
{
  mNewObjProps = mNewObjPropsDflt;
  mNewObjPropsChgd = true;
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
  diaLimitWindowPos(ImodInfoWin->width(), ImodInfoWin->height(), xx, yy);
  imod_info_input();
  ImodInfoWin->doOrSetupMove(xx, yy);
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

// Save parameters from a multi-Z when when it closes, or at program end
void ImodPreferences::recordMultiZparams(QRect geom, int numx, int numy,
                                         int zstep, int drawCen, int drawOther)
{
  mMultiZgeom = geom;
  mMultiZnumX = numx;
  mMultiZnumY = numy;
  mMultiZstep = zstep;
  mMultiZdrawCen = drawCen;
  mMultiZdrawOthers = drawOther;
}

// Return the current multi Z params
QRect ImodPreferences::getMultiZparams(int &numx, int &numy, int &zstep, 
                                       int &drawCen, int &drawOther)
{
  if (!mMultiZgeom.width())
    return mMultiZgeom;
  numx = mMultiZnumX;
  numy = mMultiZnumY;
  zstep = mMultiZstep;
  drawCen = mMultiZdrawCen;
  drawOther = mMultiZdrawOthers;
  return mMultiZgeom;
}

void ImodPreferences::getAutoContrastTargets(int &mean, int &sd)
{
  mean = mCurrentPrefs.autoTargetMean;
  sd = mCurrentPrefs.autoTargetSD;
}


// Keep a list of settings to save under the given key
int ImodPreferences::saveGenericSettings(const char *key, int numVals, double *values)
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
int ImodPreferences::getGenericSettings(const char *key, double *values, int maxVals)
{
  QString str;
  QSettings *settings = getSettingsObject();
  bool readin;
  double val;
  int i;

  // Read each possible number and load into array if present, quit if not
  for (i = 0; i < maxVals; i++) {
    str.sprintf("%s/%d", key, i);
    readin = settings->contains(str);
    if (!readin)
      break;
    val = settings->value(str, 0.).toDouble();
    values[i] = val;
  }
  delete settings;
  return i;
}

// Find out if warning about classic slicer has happened and set it to true
bool ImodPreferences::classicWarned()
{
  bool temp = mClassicWarned;
  mClassicWarned = true;
  return temp;
}

PrefsDialog::PrefsDialog(QWidget *parent)
  : QDialog(parent)
{
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);

  // 2/19/11: experimented with QToolBox.  It is very annoying because you have to
  // move mouse to the header.  The right thing to do is to make a list box on the left
  // if more tans/panels are created
  mTabWidget = new QTabWidget();
  mAppearForm = new AppearanceForm();
  mTabWidget->addTab(mAppearForm, "Appearance/Limits");
  mSnapForm = new SnapshotForm();
  mTabWidget->addTab(mSnapForm, "Snapshots");
  mBehaveForm = new BehaviorForm();
  mTabWidget->addTab(mBehaveForm, "Behavior");
  mMouseForm = new MouseForm();
  mTabWidget->addTab(mMouseForm, "Mouse");
  setWindowTitle("3dmod: Set preferences");
  
  QVBoxLayout *mainLayout = new QVBoxLayout;
  QVBoxLayout *tabLayout = diaVBoxLayout(mainLayout);
  tabLayout->addWidget(mTabWidget);
  QHBoxLayout *butLayout = diaHBoxLayout(mainLayout);
  setLayout(mainLayout);

  QPushButton *button = new QPushButton("Done");
  butLayout->addWidget(button);
  connect(button, SIGNAL(clicked()), ImodPrefs, SLOT(donePressed()));
  
  button = new QPushButton("Defaults for Tab");
  butLayout->addWidget(button);
  connect(button, SIGNAL(clicked()), ImodPrefs, SLOT(defaultPressed())); 

  button = new QPushButton("Cancel");
  butLayout->addWidget(button);
  connect(button, SIGNAL(clicked()), ImodPrefs, SLOT(cancelPressed()));
}

void PrefsDialog::closeEvent ( QCloseEvent * e )
{
  ImodPrefs->userCanceled();
  e->accept();
}
