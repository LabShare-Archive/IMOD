/*   preferences.h  -  declarations for preferences.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Log at end of file
*/

#ifndef IMOD_PREFERENCES_H
#define IMOD_PREFERENCES_H

#include <qfont.h>
#include <qstring.h>
#include <qstringlist.h>
#include <qstyle.h>

class QTabDialog;
class AppearanceForm;
class BehaviorForm;
class MouseForm;
class QSettings;
typedef struct ilist_struct Ilist;

#ifndef HOTSLIDER_H
// Defines for the states: these are replicated in colorselector.cpp for now
#define HOT_SLIDER_KEYUP 0
#define HOT_SLIDER_KEYDOWN 1
#define NO_HOT_SLIDER 2
int hotSliderFlag();
int hotSliderKey();
#endif

#define MAXZOOMS 18
#define MAX_GEOMETRIES 10
#define MAX_NAMED_COLORS 8

// Define this to use a list of styles to exclude rather than ones to include
//#define EXCLUDE_STYLES

// The structure of preferences.
// For each parameter, there is a default (Dflt) and a flag for whether
// the parameter has changed (Chgd)
typedef struct imod_pref_struct
{
  int hotSliderKey;        // Hot slider key (ctrl, Alt, Shift)
  int hotSliderKeyDflt;    
  bool hotSliderKeyChgd;
  int hotSliderFlag;       // Flag for whether active up or down
  int hotSliderFlagDflt;
  bool hotSliderFlagChgd;
  int mouseMapping;          // Code for assignment of mouse keys
  int mouseMappingDflt;
  bool mouseMappingChgd;
  bool silentBeep;         // Silence the alarm in wprint
  bool silentBeepDflt;
  bool silentBeepChgd;
  bool tooltipsOn;         // Enable tool tips
  bool tooltipsOnDflt;
  bool tooltipsOnChgd;
  QFont font;              // Font
  bool fontChgd;
  QString styleKey;        // Style
  bool styleChgd;
  int bwStep;              // Step size for F1-F8 keys
  int bwStepDflt;
  bool bwStepChgd;
  bool iconifyImodvDlg;    // Iconify imodv dialogs with imodv window
  bool iconifyImodvDlgDflt;
  bool iconifyImodvDlgChgd;
  bool iconifyImodDlg;    // Iconify imod dialogs with info window
  bool iconifyImodDlgDflt;
  bool iconifyImodDlgChgd;
  bool iconifyImageWin;   // Iconify image windows with info window
  bool iconifyImageWinDflt;
  bool iconifyImageWinChgd;
  int minModPtSize;       // Minimum size of current model point
  int minModPtSizeDflt;
  bool minModPtSizeChgd;
  int minImPtSize;        // Minimum size of current image point
  int minImPtSizeDflt;
  bool minImPtSizeChgd;
  double zooms[MAXZOOMS];    // Zooms
  double zoomsDflt[MAXZOOMS];
  bool zoomsChgd;
  int autosaveInterval;   // Interval to autosave at in minutes
  int autosaveIntervalDflt;
  bool autosaveIntervalChgd;
  bool autosaveOn;        // Flag for doing autosaves
  bool autosaveOnDflt;
  bool autosaveOnChgd;
  QString autosaveDir;    // Location to save autosave file
  QString autosaveDirDflt;
  bool autosaveDirChgd;
  bool rememberGeom;     // Remember window size and locations
  bool rememberGeomDflt;
  bool rememberGeomChgd;
  int autoTargetMean;      // Target mean for autocontrast
  int autoTargetMeanDflt;
  bool autoTargetMeanChgd;
  int autoTargetSD;        // Target SD
  int autoTargetSDDflt;
  bool autoTargetSDChgd;
  int namedIndex[MAX_NAMED_COLORS];
  QRgb namedColor[MAX_NAMED_COLORS];
  QRgb namedColorDflt[MAX_NAMED_COLORS];
  bool namedColorChgd[MAX_NAMED_COLORS];
  QString snapFormat;     // Format for non-tif snapshot
  QString snapFormatDflt;
  bool snapFormatChgd;
  int snapQuality;        // Quality factor, controls compression
  int snapQualityDflt;
  bool snapQualityChgd;

} ImodPrefStruct;


class ImodPreferences : public QObject
{
  Q_OBJECT

 public:
  ImodPreferences(char *cmdLineStyle);
  ~ImodPreferences() {};
  void saveSettings();
  void editPrefs();
  double *getZooms() {return &mCurrentPrefs.zooms[0];};
  int getBwStep() {return mCurrentPrefs.bwStep;};
  int iconifyImodvDlg() {return mCurrentPrefs.iconifyImodvDlg;};
  int iconifyImodDlg() {return mCurrentPrefs.iconifyImodDlg;};
  int iconifyImageWin() {return mCurrentPrefs.iconifyImageWin;};
  int hotSliderKey() {return mCurrentPrefs.hotSliderKey;};
  int hotSliderFlag() {return mCurrentPrefs.hotSliderFlag;};
  int minCurrentImPtSize();
  int minCurrentModPtSize();
  bool silentBeep() {return mCurrentPrefs.silentBeep;};
  int actualButton(int logicalButton);
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
  bool getRoundedStyle();
  QString snapFormat() {return mCurrentPrefs.snapFormat;};
  int snapQuality() {return mCurrentPrefs.snapQuality;};
  QStringList snapFormatList();

  public slots:
    void donePressed();
  void defaultPressed();
  void cancelPressed();

 protected:
  void timerEvent(QTimerEvent *e);

 private:
  ImodPrefStruct mCurrentPrefs;
  ImodPrefStruct mDialogPrefs;
  QTabDialog *mTabDlg;
  AppearanceForm *mAppearForm;
  BehaviorForm *mBehaveForm;
  MouseForm *mMouseForm;
  int mCurrentTab;
  int mTimerID;
  int mGeomImageXsize[MAX_GEOMETRIES];
  int mGeomImageYsize[MAX_GEOMETRIES];
  QRect mGeomInfoWin[MAX_GEOMETRIES];
  QRect mGeomZapWin[MAX_GEOMETRIES];
  QRect mRecordedZapGeom;
  int mGeomLastSaved;
  Ilist *mGenericList;
};

extern ImodPreferences *ImodPrefs;


#endif // IMOD_PREFERENCES_H

/*
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
