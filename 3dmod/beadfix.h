/*   beadfix.h  -  header for beadfixer plugin/module
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 * $Id$
 */                                                                           

#ifndef BEADFIX_H
#define BEADFIX_H

#include "dialog_frame.h"
#include "imodel.h"
#include <qprocess.h>

// Changes to make internal module: include and declare class
#include "special_module.h"
//Added by qt3to4:
#include <QCloseEvent>
#include <QTimerEvent>
#include <QKeyEvent>

class BeadFixerModule : public SpecialModule
{
 public:
  BeadFixerModule();
};


class QPushButton;
class QCheckBox;
class QSpinBox;
class QDoubleSpinBox;
class QWidget;
class QLabel;
class QButtonGroup;
class MultiSlider;
class ToolEdit;

// A structure to hold all of the residual data that is read in, plus what
// local area it is in and whether it has been looked at in this run-through
typedef struct {
  int obj;
  int cont;
  int view;
  float xcen, ycen;
  float xres, yres;
  float sd;
  float weight;
  int lookedAt;
  int area;
} ResidPt;

// A structure to hold data about points that have been looked at
typedef struct {
  int obj;
  int cont;
  int view;
} LookedPt;

// A structure to hold data about areas
typedef struct {
  int areaX;
  int areaY;
  int firstPt;
  int numPts;
} AreaData;

class BeadFixer : public DialogFrame
{
  Q_OBJECT

 public:
  BeadFixer(QWidget *parent, const char *name = NULL);
  ~BeadFixer() {};
  int openFileByName(const char *filename);
  int reread();
  int insertPoint(float imx, float imy, bool keypad);
  int modifyPoint(float imx, float imy);
  int findCenter(float &imx, float &imy, int curz);
  int executeMessage(QStringList *strings, int *arg);
  void findGap(int idir);
  void modelUpdate();
  QCheckBox *overlayBox;
  int    mLastob;
  ToolEdit *skipEdit;
  int mIteratingMoveAll;

  public slots:
  void buttonPressed(int which);
  void nextGap() {findGap(1);};
  void prevGap() {findGap(-1);};
  void resetStart();
  void resetCurrent();
  void reattach();
  void openFile();
  void rereadFile() {reread();};
  void nextRes();
  void nextLocal();
  void backUp();
  void nextCont();
  void backUpCont();
  void delCont();
  void movePoint();
  void moveAllSlot();
  void moveAllAll();
  void undoMove();
  void clearList();
  void modeSelected(int value);
  void onceToggled(bool state);
  void seedToggled(bool state);
  void autoCenToggled(bool state);
  void lightToggled(bool state);
  void diameterChanged(int value);
  void overlayToggled(bool state);
  void overlayChanged(int value);
  void reverseToggled(bool state);
  void setOverlay(int doIt, int state);
  void keepOnTop(bool state);
  void threshChanged(int slider, int value, bool dragging); 
  void deleteBelow();
  void delAllSecToggled(bool state);
  void delAllObjToggled(bool state);
  void turnOffToggled(bool state);
  void ignoreToggled(bool state);
  void skipLowWgtToggled(bool state);
  void wgtThreshChanged(double value);
  void skipListEntered();
  void runAlign();
  void alignExited(int exitCode, QProcess::ExitStatus exitStatus);
  void setFontDependentWidths();
  void fixSize();

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void timerEvent(QTimerEvent *e);
  void mouseMoveEvent ( QMouseEvent * e );
  void changeEvent(QEvent *e);

 private:
  int foundgap(int obj, int cont, int ipt, int before);
  void setCurArea(int area);
  void makeUpDownArrow(int before);
  void showWidget(QWidget *widget, bool state);
  void newSkipList(QString list);
  bool inSkipList(int zval);
  void moveAll(bool globalOK, bool skipDisplay);
  void manageDoneLabel();
  void reportContRes();
  int moveToCont(int idir);
  void iterateMoveAll();
  void manageThreshWidgets(bool seedMode);

  int    mIfdidgap;
  int    mLastco, mLastpt, mLastbefore;
  int    mObjcont;                      /* Flag for new object/contour data */
  int    mObjlook, mContlook, mPtlook;  /* obj, cont, pt of current residual */
  int    mIndlook;                      /* Index in list of current residual */
  int    mCurmoved;                     /* flag whether it has been moved */
  int    mObjmoved, mContmoved, mPtmoved;  /* obj, cont, pt of moved point */
  int    mDidmove;                      /* flag that a point was moved */
  Ipoint mOldpt, mNewpt;                /* old and new positions */
  int    mLookonce;                     /* Flag for Examine once button */
  ResidPt *mResidList;                  /* List of all residuals read in */
  int    mNumResid;                     /* Number of point on list */ 
  int    mResidMax;                     /* Allocated size of list */ 
  int    mCurrentRes;                   /* Current residual index */ 
  LookedPt *mLookedList;                /* List of points examined */
  int    mNumLooked;                    /* Number of items on list */
  int    mLookedMax;                    /* Size allocated for list */
  int    mCurArea;                      /* Current local area index */
  int    mNumAreas;                     /* Number of areas */
  AreaData *mAreaList;                  /* Data about areas */
  int    mAreaMax;                      /* Size allocated */
  int    mBell;                         /* 1 to ring bell, -1 to suppress */ 
  bool   mMovingAll;                  /* Flag for moving all points in local */
  int    mNumAllMoved;                  /* Number moved in local area */
  double mLastContRes;                  /* Last contour residual shown */
  bool   mContResReported;              /* Flag that cont res stats reported */
  double mMaxContRes;
  double mContResSum, mContResSumsq;
  int    mNumContRes;
  int    mContForContRes;
  int    mObjForContRes;
  bool   mHasWeights;
  QButtonGroup *modeGroup;
  QPushButton *nextGapBut;
  QPushButton *prevGapBut;
  QPushButton *rereadBut;
  QPushButton *nextLocalBut;
  QPushButton *nextResBut;
  QPushButton *backUpBut;
  QPushButton *nextContBut;
  QPushButton *delContBut;
  QPushButton *backContBut;
  QPushButton *movePointBut;
  QPushButton *moveAllBut;
  QPushButton *moveAllAllBut;
  QPushButton *undoMoveBut;
  QPushButton *clearListBut;
  QPushButton *runAlignBut;
  QPushButton *openFileBut;
  QPushButton *resetStartBut;
  QPushButton *reattachBut;
  QPushButton *resetCurrentBut;
  QCheckBox *autoCenBox;
  QCheckBox *seedModeBox;
  QCheckBox *examineBox;
  QSpinBox *diameterSpin;
  QSpinBox *overlaySpin;
  QCheckBox *reverseBox;
  QWidget *topBox;
  QWidget *diameterHbox;
  QWidget *cenLightHbox;
  QWidget *overlayHbox;
  MultiSlider *threshSlider;
  QPushButton *deleteBelowBut;
  QCheckBox *delAllSecBut;
  QCheckBox *delAllObjBut;
  QCheckBox *turnOffBut;
  QCheckBox *ignoreSkipBut;
  QLabel *doneLabel;
  QLabel *wgtThreshLabel;
  QCheckBox *skipLowWgtBox;
  QDoubleSpinBox *wgtThreshSpin;
  QWidget *weightHbox;
  int mNumSkip;
  int *mSkipSecs;
  int mExtraObj;
  bool mStayOnTop;
  bool mRunningAlign;
  bool mShiftDown;
  int mTopTimerID;
  QProcess *mAlignProcess;
  int mPeakMin, mPeakMax;
  int mLastThresh;
  QString mLastLogError;
};

#endif
