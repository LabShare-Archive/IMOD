/*   beadfix.h  -  header for beadfixer plugin/module
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
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

#ifdef QT_THREAD_SUPPORT
#define FIXER_CAN_RUN_ALIGN
#endif

#ifdef FIXER_CAN_RUN_ALIGN
#include <qthread.h>

class AlignThread : public QThread
{
 public:
  AlignThread() {};
  ~AlignThread() {};

 protected:
  void run();
};
#endif


class QPushButton;
class QCheckBox;
class QHBox;

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

  public slots:
  void buttonPressed(int which);
  void nextGap();
  void openFile();
  void rereadFile() {reread();};
  void nextRes();
  void nextLocal();
  void backUp();
  void movePoint();
  void undoMove();
  void clearList();
  void onceToggled(bool state);
  void keepOnTop(bool state);
  void runAlign();
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
  QPushButton *rereadBut;
  QPushButton *nextLocalBut;
  QPushButton *nextResBut;
  QPushButton *backUpBut;
  QPushButton *movePointBut;
  QPushButton *undoMoveBut;
  QPushButton *clearListBut;
  QPushButton *runAlignBut;
  QPushButton *openFileBut;
  QHBox *topBox;
  bool mStayOnTop;
  bool mRunningAlign;
  int mTopTimerID;
#ifdef FIXER_CAN_RUN_ALIGN
  AlignThread *mTaThread;
#endif
};

#endif
