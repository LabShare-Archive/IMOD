/*   preferences.h  -  declarations for preferences.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef IMOD_PREFERENCES_H
#define IMOD_PREFERENCES_H

#include <qfont.h>
#include <qstring.h>
#include <qstringlist.h>
#include <qstyle.h>
#include <qdialog.h>
//Added by qt3to4:
#include <QTimerEvent>
#include <QCloseEvent>

class AppearanceForm;
class BehaviorForm;
class MouseForm;
class QSettings;
class QDialogButtonBox;
class QTabWidget;
class QAbstractButton;
class QPushButton;
typedef struct ilist_struct Ilist;

#ifndef HOTSLIDER_H
// Defines for the states: these are replicated in colorselector.cpp for now
#define HOT_SLIDER_KEYUP 0
#define HOT_SLIDER_KEYDOWN 1
#define NO_HOT_SLIDER 2
int hotSliderFlag();
int hotSliderKey();
#endif

#define EXCLUDE_STYLES
#define MAXZOOMS 18
#define MAX_GEOMETRIES 10
#define MAX_NAMED_COLORS 8

#define TRIPLET(a,b) a b; \
  a b##Dflt; \
  bool b##Chgd;

// Define this to use a list of styles to exclude rather than ones to include
//#define EXCLUDE_STYLES

// The structure of preferences.
// For each parameter, there is a default (Dflt) and a flag for whether
// the parameter has changed (Chgd)
typedef struct imod_pref_struct
{
  TRIPLET(int, hotSliderKey);        // Hot slider key (ctrl, Alt, Shift)
  TRIPLET(int, hotSliderFlag);       // Flag for whether active up or down
  TRIPLET(int, mouseMapping);          // Code for assignment of mouse keys
  TRIPLET(bool, modvSwapLeftMid);    // Swap left and middle in model view
  TRIPLET(bool, silentBeep);         // Silence the alarm in wprint
  //  TRIPLET(bool, tooltipsOn);         // Enable tool tips
  TRIPLET(bool, classicSlicer);       // Use classic slicer
  TRIPLET(bool, startAtMidZ);         // Go to middle Z at start
  TRIPLET(int, autoConAtStart);      // Do autocontrast at start
  TRIPLET(bool, attachToOnObj);      // Attach to ON objects only
  TRIPLET(bool, slicerNewSurf);      // Slicer make new surfaces automatically 
  QFont font;              // Font
  bool fontChgd;
  QString styleKey;        // Style
  bool styleChgd;
  TRIPLET(int, bwStep);              // Step size for F1-F8 keys
  TRIPLET(int, pageStep);            // Step size for paging with keypad /,*
  TRIPLET(bool, iconifyImodvDlg);    // Iconify imodv dialogs with imodv window
  TRIPLET(bool, iconifyImodDlg);    // Iconify imod dialogs with info window
  TRIPLET(bool, iconifyImageWin);   // Iconify image windows with info window
  TRIPLET(int, minModPtSize);       // Minimum size of current model point
  TRIPLET(int, minImPtSize);        // Minimum size of current image point
  double zooms[MAXZOOMS];    // Zooms
  double zoomsDflt[MAXZOOMS];
  bool zoomsChgd;
  TRIPLET(int, autosaveInterval);   // Interval to autosave at in minutes
  TRIPLET(bool, autosaveOn);        // Flag for doing autosaves
  TRIPLET(QString, autosaveDir);    // Location to save autosave file
  TRIPLET(bool, rememberGeom);     // Remember window size and locations
  TRIPLET(int, autoTargetMean);      // Target mean for autocontrast
  TRIPLET(int, autoTargetSD);        // Target SD
  int namedIndex[MAX_NAMED_COLORS];
  QRgb namedColor[MAX_NAMED_COLORS];
  QRgb namedColorDflt[MAX_NAMED_COLORS];
  bool namedColorChgd[MAX_NAMED_COLORS];
  TRIPLET(QString, snapFormat);     // Format for non-tif snapshot
  TRIPLET(int, snapQuality);        // Quality factor, controls compression
  TRIPLET(int, slicerPanKb);        // Maximum KB for slicer panning
  TRIPLET(bool, speedupSlider);     // Apply limit when using sliders too

} ImodPrefStruct;

class PrefsDialog : public QDialog
{
  Q_OBJECT
    
    public:
  PrefsDialog(QWidget *parent = 0);
  QTabWidget *mTabWidget;
  AppearanceForm *mAppearForm;
  BehaviorForm *mBehaveForm;
  MouseForm *mMouseForm;

 protected:
  void closeEvent ( QCloseEvent * e );

 private:
};

class ImodPreferences : public QObject
{
  Q_OBJECT

 public:
  ImodPreferences(char *cmdLineStyle);
  ~ImodPreferences() {};
  void saveSettings(int modvAlone);
  void editPrefs();
  double *getZooms() {return &mCurrentPrefs.zooms[0];};
  int getBwStep() {return mCurrentPrefs.bwStep;};
  int getPageStep() {return mCurrentPrefs.pageStep;};
  int iconifyImodvDlg() {return mCurrentPrefs.iconifyImodvDlg;};
  int iconifyImodDlg() {return mCurrentPrefs.iconifyImodDlg;};
  int iconifyImageWin() {return mCurrentPrefs.iconifyImageWin;};
  int hotSliderKey() {return mCurrentPrefs.hotSliderKey;};
  int hotSliderFlag() {return mCurrentPrefs.hotSliderFlag;};
  int autoConAtStart() {return mCurrentPrefs.autoConAtStart;};
  bool startAtMidZ() {return mCurrentPrefs.startAtMidZ;};
  bool hotSliderActive(int ctrlPressed);
  int minCurrentImPtSize();
  int minCurrentModPtSize();
  bool silentBeep() {return mCurrentPrefs.silentBeep;};
  bool classicSlicer() {return mCurrentPrefs.classicSlicer;};
  bool classicWarned();
  bool attachToOnObj() {return mCurrentPrefs.attachToOnObj;};
  bool slicerNewSurf() {return mCurrentPrefs.slicerNewSurf;};
  int actualButton(int logicalButton);
  int actualModvButton(int logicalButton);
  QString autosaveDir();
  int autosaveSec();
  ImodPrefStruct *getDialogPrefs() {return &mDialogPrefs;};
  void changeFont(QFont newFont);
  void changeStyle(QString newKey);
  void pointSizeChanged();
  bool equiv(bool b1, bool b2) {return ((b1 && b2) || (!b1 && !b2));};
  void findCurrentTab();
  void userCanceled();
  char **getStyleList();
  bool styleOK(QString key);
  int *getStyleStatus();
  void setInfoGeometry();
  QRect getZapGeometry();
  void getAutoContrastTargets(int &mean, int &sd);
  QColor namedColor(int index);
  int saveGenericSettings(char *key, int numVals, double *values);
  int getGenericSettings(char *key, double *values, int maxVals);
  QSettings *getSettingsObject();
  void recordZapGeometry();
  void recordMultiZparams(QRect geom, int numx, int numy, int zstep, 
                          int drawCen, int drawOther);
  QRect getMultiZparams(int &numx, int &numy, int &zstep, int &drawCen, 
                        int &drawOther);
  bool getRoundedStyle();
  QString snapFormat() {return mCurrentPrefs.snapFormat;};
  int snapQuality() {return mCurrentPrefs.snapQuality;};
  void setSnapQuality(int value);
  int slicerPanKb() {return mCurrentPrefs.slicerPanKb;};
  bool speedupSlider() {return mCurrentPrefs.speedupSlider;};
  QString snapFormat2();
  void set2ndSnapFormat();
  void restoreSnapFormat();
  QStringList snapFormatList();

  public slots:
    void donePressed();
  void cancelPressed();
  void defaultPressed();

 protected:
  void timerEvent(QTimerEvent *e);

 private:
  ImodPrefStruct mCurrentPrefs;
  ImodPrefStruct mDialogPrefs;
  PrefsDialog *mTabDlg;
  int mCurrentTab;
  int mTimerID;
  int mGeomImageXsize[MAX_GEOMETRIES];
  int mGeomImageYsize[MAX_GEOMETRIES];
  QRect mGeomInfoWin[MAX_GEOMETRIES];
  QRect mGeomZapWin[MAX_GEOMETRIES];
  QRect mRecordedZapGeom;
  int mGeomLastSaved;
  QRect mMultiZgeom;
  int mMultiZnumX, mMultiZnumY;
  int mMultiZstep;
  int mMultiZdrawCen, mMultiZdrawOthers;
  bool mClassicWarned;
  QString mSavedSnapFormat;
  Ilist *mGenericList;
};

extern ImodPreferences *ImodPrefs;


#endif // IMOD_PREFERENCES_H

/*
$Log$
Revision 1.23  2009/03/22 19:44:46  mast
Switched to taking all styles that exist

Revision 1.22  2009/02/26 20:03:32  mast
Add paging by big steps

Revision 1.21  2009/01/15 16:33:18  mast
Qt 4 port

Revision 1.20  2008/12/10 01:04:22  mast
Added function to set JPEG quality

Revision 1.19  2008/09/24 02:39:28  mast
Added option for attach function to look only at On objects

Revision 1.18  2008/05/27 05:42:19  mast
Various new preferences, added macro

Revision 1.17  2008/02/03 18:38:25  mast
Added option to swap left/middle in model view

Revision 1.16  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 1.15  2008/01/13 22:58:35  mast
Changes for multi-Z window

Revision 1.14  2007/11/13 19:14:08  mast
Added settings to control slicer speedup

Revision 1.13  2007/07/08 16:03:49  mast
Added hot slider active function

Revision 1.12  2007/05/31 16:25:33  mast
Added members for classic slicer

Revision 1.11  2006/10/05 15:41:32  mast
Provided for primary and second non-TIFF snapshot format

Revision 1.10  2004/11/29 19:25:21  mast
Changes to do QImage instead of RGB snapshots

Revision 1.9  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 1.8  2004/11/02 20:17:00  mast
Added color settings

Revision 1.7  2004/06/23 03:33:10  mast
New functions for generic settings

Revision 1.6  2003/09/24 17:35:54  mast
Change to routine that sets info window geometry directly

Revision 1.5  2003/09/18 05:57:29  mast
Add members for autocontrast targets

Revision 1.4  2003/09/17 04:47:24  mast
Added members for remembering window geometry

Revision 1.3  2003/03/26 23:06:42  mast
only check status of a style once

Revision 1.2  2003/03/26 22:49:09  mast
Change style handling to use list of styles

Revision 1.1  2003/03/24 17:56:59  mast
Initial creation

*/
