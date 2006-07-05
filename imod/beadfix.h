/*   beadfix.h  -  header for beadfixer plugin/module
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

Log at end
*/

#ifndef BEADFIX_H
#define BEADFIX_H

#include "dialog_frame.h"
#include "imodel.h"

// Changes to make internal module: include and declare class
#include "special_module.h"

class BeadFixerModule : public SpecialModule
{
 public:
  BeadFixerModule();
};


class QPushButton;
class QCheckBox;
class QSpinBox;
class QHBox;
class QVButtonGroup;
class QProcess;

// A structure to hold all of the residual data that is read in, plus what
// local area it is in and whether it has been looked at in this run-through
typedef struct {
  int obj;
  int cont;
  int view;
  float xcen, ycen;
  float xres, yres;
  float sd;
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
  int insertPoint(float imx, float imy);
  int modifyPoint(float imx, float imy);
  int findCenter(float &imx, float &imy, int curz);
  int executeMessage(QStringList *strings, int *arg);
  void findGap(int idir);
  QCheckBox *overlayBox;

  public slots:
  void buttonPressed(int which);
  void nextGap() {findGap(1);};
  void prevGap() {findGap(-1);};
  void resetStart();
  void resetCurrent();
  void reattach();
  void upDownToggled(bool state);
  void openFile();
  void rereadFile() {reread();};
  void nextRes();
  void nextLocal();
  void backUp();
  void movePoint();
  void moveAll();
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
  void runAlign();
  void alignExited();
  void setFontDependentWidths();

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void timerEvent(QTimerEvent *e);
  void fontChange( const QFont & oldFont );

 private:
  int foundgap(int obj, int cont, int ipt, int before);
  void setCurArea(int area);
  void makeUpDownArrow(int before);
  void showWidget(QWidget *widget, bool state);

  int    mIfdidgap;
  int    mLastob, mLastco, mLastpt, mLastbefore;
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
  bool   mSeedMode;                     /* Seeding mode for inserting points */
  bool   mMovingAll;                  /* Flag for moving all points in local */
  int    mNumAllMoved;                  /* Number moved in local area */
  QVButtonGroup *modeGroup;
  QPushButton *nextGapBut;
  QPushButton *prevGapBut;
  QPushButton *rereadBut;
  QPushButton *nextLocalBut;
  QPushButton *nextResBut;
  QPushButton *backUpBut;
  QPushButton *movePointBut;
  QPushButton *moveAllBut;
  QPushButton *undoMoveBut;
  QPushButton *clearListBut;
  QPushButton *runAlignBut;
  QPushButton *openFileBut;
  QPushButton *resetStartBut;
  QPushButton *reattachBut;
  QPushButton *resetCurrentBut;
  QCheckBox *upDownArrowBox;
  QCheckBox *autoCenBox;
  QCheckBox *seedModeBox;
  QCheckBox *examineBox;
  QSpinBox *diameterSpin;
  QSpinBox *overlaySpin;
  QCheckBox *reverseBox;
  QHBox *topBox;
  QHBox *diameterHbox;
  QHBox *cenLightHbox;
  QHBox *overlayHbox;
  bool mStayOnTop;
  bool mRunningAlign;
  int mTopTimerID;
  QProcess *mAlignProcess;
};

#endif
/*
$Log$
Revision 1.16  2006/07/04 03:51:04  mast
Switched running align from a thread to a QProcess

Revision 1.15  2006/07/03 04:12:19  mast
Overlay mode, operating mode, new gap fizing functions

Revision 1.14  2006/03/01 18:17:50  mast
Added variables to keep track of moving all in local area

Revision 1.13  2006/02/13 05:14:50  mast
Added autocentering and seed mode

Revision 1.12  2005/04/12 18:58:00  mast
Added move all in local area

Revision 1.11  2005/02/19 01:29:50  mast
Removed function to clear extra object

Revision 1.10  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 1.9  2004/09/24 18:00:36  mast
Added message capability

Revision 1.8  2004/06/25 20:06:09  mast
Made plug variables be class members instead to eliminate plug->

Revision 1.7  2004/06/24 15:34:37  mast
Changes for read and analyze at once

Revision 1.6  2004/06/12 00:58:11  mast
Switched to reading in whole file at once

Revision 1.5  2004/05/28 18:57:42  mast
Enable fixer to run align if thread support exists

Revision 1.4  2004/05/07 22:14:26  mast
Defined a variable instead of QT_THREAD_SUPPORT for the the Run Align button

Revision 1.3  2004/05/03 19:17:20  mast
Added thread class to run tiltalign

Revision 1.2  2004/04/29 00:28:51  mast
Added button to keep window on top

Revision 1.1  2003/10/01 05:10:58  mast
Incorporation as internal module in 3dmod

*/
