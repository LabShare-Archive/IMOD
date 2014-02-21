/*   preferences.h  -  declarations for preferences.cpp
 *
 *   Copyright (C) 1995-2011 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
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
#include "imod.h"

class AppearanceForm;
class BehaviorForm;
class MouseForm;
class SnapshotForm;
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
#define MAXZOOMS 22
#define MAX_GEOMETRIES 10
#define MAX_NAMED_COLORS 12

#define TRIPLET(a,b) a b; \
  a b##Dflt; \
  bool b##Chgd;

// Define this to use a list of styles to exclude rather than ones to include
//#define EXCLUDE_STYLES

// The structure for new (default) object properties
typedef struct new_obj_props
{
  unsigned int    flags;
  int     pdrawsize;
  int     symbol;
  int     symsize;
  int     linewidth2;
  int     symflags;
  int     pointLimit;
  int     fillTrans;
} NewObjectProps;

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
  TRIPLET(bool, startInHQ);          // Start windows in HQ mode
  TRIPLET(bool, arrowsScrollZap);    // Use arrows for Page Up/Down in Zap
  TRIPLET(bool, startAtMidZ);        // Go to middle Z at start
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
  TRIPLET(int, snapDPI);            // DPI value to set in snapshots
  TRIPLET(bool, scaleSnapDPI);      // Scale the DPI value up when montaging
  TRIPLET(int, slicerPanKb);        // Maximum KB for slicer panning
  TRIPLET(bool, speedupSlider);     // Apply limit when using sliders too
  TRIPLET(bool, loadUshorts);       // Load data as ushorts
  TRIPLET(bool, isoHighThresh);     // Set initial threshold above middle
  TRIPLET(int, isoBoxInitial);      // Initial box size
  TRIPLET(int, isoBoxLimit);        // Limit to box size
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
  SnapshotForm *mSnapForm;

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
  bool startInHQ() {return mCurrentPrefs.startInHQ;};
  bool arrowsScrollZap() {return mCurrentPrefs.arrowsScrollZap;};
  bool classicWarned();
  bool attachToOnObj() {return mCurrentPrefs.attachToOnObj;};
  bool slicerNewSurf() {return mCurrentPrefs.slicerNewSurf;};
  NewObjectProps *newObjectProps() {return &mNewObjProps;};
  void setDefaultObjProps();
  void restoreDefaultObjProps();
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
  const char **getStyleList();
  bool styleOK(QString key);
  int *getStyleStatus();
  void setInfoGeometry();
  QRect getZapGeometry();
  void getAutoContrastTargets(int &mean, int &sd);
  QColor namedColor(int index);
  int saveGenericSettings(const char *key, int numVals, double *values);
  int getGenericSettings(const char *key, double *values, int maxVals);
  QSettings *getSettingsObject();
  void recordZapGeometry();
  void recordMultiZparams(QRect geom, int numx, int numy, int zstep, 
                          int drawCen, int drawOther);
  QRect getMultiZparams(int &numx, int &numy, int &zstep, int &drawCen, 
                        int &drawOther);
  bool getRoundedStyle();
  QString snapFormat() {return mCurrentPrefs.snapFormat;};
  int snapQuality() {return mCurrentPrefs.snapQuality;};
  int snapDPI() {return mCurrentPrefs.snapDPI;};
  bool scaleSnapDPI() {return mCurrentPrefs.scaleSnapDPI;};
  void setSnapQuality(int value);
  int slicerPanKb() {return mCurrentPrefs.slicerPanKb;};
  bool speedupSlider() {return mCurrentPrefs.speedupSlider;};
  bool loadUshorts() {return mCurrentPrefs.loadUshorts;};
  bool isoHighThresh() {return mCurrentPrefs.isoHighThresh;};
  int isoBoxInitial() {return mCurrentPrefs.isoBoxInitial;};
  int isoBoxLimit() {return mCurrentPrefs.isoBoxLimit;};
  QString snapFormat2(QString *curFormat = NULL);
  void set2ndSnapFormat();
  void restoreSnapFormat();
  QStringList snapFormatList();
  getSetMember(bool, XyzApplyZscale);

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
  TRIPLET(NewObjectProps, mNewObjProps); // new object properties
  QRect mRecordedZapGeom;
  int mGeomLastSaved;
  QRect mMultiZgeom;
  int mMultiZnumX, mMultiZnumY;
  int mMultiZstep;
  int mMultiZdrawCen, mMultiZdrawOthers;
  bool mXyzApplyZscale;
  bool mClassicWarned;
  QString mSavedSnapFormat;
  Ilist *mGenericList;
};

extern ImodPreferences *ImodPrefs;


#endif // IMOD_PREFERENCES_H

