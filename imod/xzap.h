/*
 *  xzap.h -- Header file for ZaP Window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#ifndef XZAP_H
#define XZAP_H

#include "control.h"
//Added by qt3to4:
#include <QMouseEvent>
#include <QKeyEvent>
#include <QEvent>

class ZapWindow;
class ZapGL;
class QString;
class QKeyEvent;
class QEvent;
class QMouseEvent;

typedef struct b3d_ci_image B3dCIImage;

class ZapFuncs
{
 public:
  ZapFuncs(ImodView *vi, int wintype);
  ~ZapFuncs() {};
  void closing();
  void paint();
  void draw();
  void resize(int winx, int winy);
  void keyInput(QKeyEvent *e);
  void keyRelease(QKeyEvent *e);
  void mousePress(QMouseEvent *e);
  void mouseRelease(QMouseEvent *e);
  void mouseMove(QMouseEvent *e);
  void generalEvent(QEvent *e);
  void help();
  void enteredZoom(float newZoom);
  void enteredSection(int section);
  void stepZoom(int step);
  void stateToggled(int index, int state);
  QString printInfo(bool toInfoWindow = true);
  void stepTime(int step);
  int setupPanels();
  void toggleContourShift();
  void bandImageToMouse(int ifclip); 
  void flushImage();
  void getixy(int mx, int my, float *x, float *y, int *z);
  void montageSnapshot(int snaptype);
  void setCursor(int mode, bool setAnyway = false);
  void setMouseTracking();
  void shiftRubberband(float idx, float idy);
  void syncImage();
  void toggleRubberband(bool draw = true);
  bool getLowHighSection(int &low, int &high);

 private:
  int contInSelectArea(Iobj *obj, Icont *cont, Ipoint selmin, Ipoint selmax);
  void analyzeBandEdge(int ix, int iy);
  int checkPlugUseMouse(QMouseEvent *event, int but1, int but2, int but3);
  int b1Click(int x, int y, int controlDown);
  int b2Click(int x, int y, int controlDown);
  int b3Click(int x, int y, int controlDown);
  int b1Drag(int x, int y);
  int b2Drag(int x, int y, int controlDown);
  int b3Drag(int x, int y, int controlDown, int shiftDown);
  int delUnderCursor(int x, int y, Icont *cont);
  int dragSelectContsCrossed(int x, int y);
  void endContourShift();
  Icont *checkContourShift(int &pt, int &err);
  void setupContourShift();
  int startShiftingContour(int x, int y, int button, int ctrlDown);
  void shiftContour(int x, int y, int button, int shiftDown);
  int defaultXformCenter(float &xcen, float &ycen);
  int startMovieCheckSnap(int dir);
  void registerDragAdditions();
  int mouseXformMatrix(int x, int y, int type, float mat[2][2]);
  void markXformCenter(float ix, float iy);
  void drawGraphics();
  void fillOverlayRGB(unsigned char **lines, int nx, int ny, int chan,
                      unsigned char *image);
  void drawModel();
  void drawContour(int co, int ob);
  void drawCurrentPoint();
  void drawExtraObject();
  int  drawAuto();
  void drawGhost();
  void setGhostColor(float obr, float obg, float obb);
  void drawTools();
  int  xpos(float x);
  int  ypos(float x);
  int  pointVisable(Ipoint *pnt);
  void autoTranslate();
  void resizeToFit();
  void setAreaLimits();
  void setControlAndLimits();
  void bandMouseToImage(int ifclip);
  int getMontageShifts(int factor, int imStart, int border, int imSize, 
                       int winSize, int &transStart, int &transDelta, int &copyDelta,
                       int &fullSize);
  void panelIndexAndCoord(int size, int num, int gutter, int border, int &pos, 
                          int &panelInd);
  void setDrawCurrentOnly(int value);
  void translate(int x, int y);
  int bandMinimum();
  B3dCIImage *getNewCIImage(B3dCIImage *image);
  void allocateToPanels(int num, int winSize, int gutter, int &panelSize, int &border);

 public:
  ImodView    *mVi;  /* Pointer to view structure. */
  ZapWindow *mQtWindow;               /* Zap window widget. */
  ZapGL *mGfx;                  /* Image sub window.  */
  int    mWinx,      mWiny;      /* Image window size. */
  int mNumXpanels;    /* Number of panels in X and Y: # in X is flag for */
  int mNumYpanels;    /* multi Z window type */
  int mPanelZstep;    /* Step in Z between panels */
  int mDrawInCenter;  /* Flags to draw model in center or other panels */
  int mDrawInOthers;
  int mSection;
  int    mRubberband;    /* Rubber banding flag and image inside coordinates */
  float  mRbImageX0;
  float  mRbImageX1;
  float  mRbImageY0;
  float  mRbImageY1;
  int    mRbMouseX0;    /* mouse coords - always derived from image coords */
  int    mRbMouseX1;
  int    mRbMouseY0;
  int    mRbMouseY1;
  int    mBandChanged;    /* Flag that band changed, draw graph windows */
  /* Pointer to control structure. */
  int         mCtrl;
  int    mGinit;
  B3dCIImage **mImages;
  int mMovieSnapCount; /* Counter if this window is doing movie snapshots */
  int mPopup;
  int mRecordSubarea;  /* Record the subarea on the next draw */
  short mShowslice;   /* Flag that slice should be drawn during paint */
  int    mStartingBand;
  int   mToolMaxZ;
  int    mXtrans,    mYtrans,    mZtrans;
  float  mZoom;
  int mLock;
  float mScaleBarSize;  /* Size of scale bar that was last drawn */

 private:
  int    mXborder,   mYborder;   /* border around image window. */
  int    mXstart,    mYstart;
  int    mXdrawsize, mYdrawsize;
  int    mLmx,       mLmy;


  int    mHqgfx, mHide;
  int    mHqgfxsave;           /* Place to save hqgfx when dragging */
  int    mDrawCurrentOnly;

  int    mShiftingCont;   /* Flag for shifting contour */
  Ipoint mXformCenter;    /* Center defined by mouse */
  Ipoint mXformFixedPt;   /* Second fixed point defined by mouse */
  int    mCenterDefined;  /* Flag that center was defined with mouse */
  int    mCenterMarked;   /* Flag that center was displayed in extra object */
  int    mFixedPtDefined; /* Flag that second fixed point has been set */
  int    mShiftRegistered;  /* Flag that contour changes have been registered */
  int    mShiftObjNum;    /* Extra object number for center marker */
  int    mDragAddCount;   /* Number of points added and not registered for undo */
  Iindex mDragAddIndex;   /* Starting obj, cont, point for first such point*/
  int    mDragAddEnd;     /* Ending or last point not registered */

  bool   mDrewExtraCursor;  /* Flag that extra cursor was drawn */

  float  mXzoom;    /* Possibly slightly different X zoom */
  char   *mData;

  /* The graphic image buffer. */
  B3dCIImage *mImage;
  int mNumImages;

  int mSectionStep; /* auto step image after new model point. */
  int mTime;
  int mOverlay;     /* Flag for displaying overlay */
  int mKeepcentered;
  int mMousemode;
  int mLastShape;   /* Last shape for cursor */
  int   mToolSection;
  float mToolZoom;
  int   mToolTime;
  int mToolSizeX;
  int mToolSizeY;

  short mInsertmode;
  int mShowedSlice;   /* Flag that it was drawn last time */

  int mPanelXborder;  /* Border outside panels in X and Y */
  int mPanelYborder;  
  int mPanelGutter;   /* Gutter between panels */
  int mPanelXsize;    /* Size of panels in X and Y */
  int mPanelYsize;

  int mToolstart;

  /* Special, lock time */
  int    mTimeLock;
  int    mTwod;
};

void zapReportBiggestMultiZ();
ZapFuncs *getTopZapWindow(bool withBand, int type = ZAP_WINDOW_TYPE);
int getTopZapMouse(Ipoint *imagePt);
int  imod_zap_open(ImodView *vi, int wintype);
int zapSubsetLimits(ImodView *vi, int &ixStart, int &iyStart, int &nxUse, int &nyUse);
void zapReportRubberband();
void zapSetImageOrBandCenter(float imx, float imy, bool incremental);
int zapRubberbandCoords(float &rbX0, float &rbX1, float &rbY0, float &rbY1);
void zapPixelViewState(bool state);
void zapSetMouseTracking();


/*

$Log$
Revision 3.34  2011/02/04 03:52:55  mast
Add second fixed point

Revision 3.33  2009/06/05 15:43:04  mast
Stop passing mouse pressed to move move event

Revision 3.32  2009/01/15 16:33:18  mast
Qt 4 port

Revision 3.31  2008/08/01 15:37:55  mast
Moved draw routine to global so imodview can draw top zap

Revision 3.30  2008/02/06 16:33:48  sueh
bug# 1065 In zapPrintInfo made printing the info optional.

Revision 3.29  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 3.28  2008/01/14 19:48:23  mast
Added function to return mouse image coords to plugin

Revision 3.27  2008/01/13 22:58:35  mast
Changes for multi-Z window

Revision 3.26  2007/12/04 18:47:27  mast
Changes for moving functions to utilities module, and for mouse tracking
and cursor-like drawing

Revision 3.25  2007/08/13 16:04:50  mast
Changes for locator window

Revision 3.24  2007/05/31 16:23:10  mast
Changes for using hot toolbar

Revision 3.23  2007/03/29 04:55:50  mast
Fixed crash bug when closing window while focus is in edit/spinbox

Revision 3.22  2006/09/17 18:15:59  mast
Changes to provide mouse position to pixelview

Revision 3.21  2006/08/24 21:28:50  mast
Added flag that rubberband changed

Revision 3.20  2006/07/03 04:14:21  mast
Changes for beadfixer overlay mode

Revision 3.19  2006/04/01 23:43:14  mast
Added size output to toolbar

Revision 3.18  2006/03/01 19:13:06  mast
Moved window size/position routines from xzap to dia_qtutils

Revision 3.17  2005/09/12 14:23:43  mast
Added function to get rubber band coordinates

Revision 3.16  2005/03/08 02:28:46  mast
Added flag to set upon resize so that new subarea gets recorded

Revision 3.15  2005/02/19 01:31:05  mast
Added variables for center of rotation

Revision 3.14  2005/02/09 01:19:48  mast
Added flag for keeping track of changes started when shifted contours

Revision 3.13  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 3.12  2004/11/04 17:02:41  mast
Changes for switching to shifting contour as a mode that is turned on

Revision 3.11  2004/11/01 22:58:32  mast
New rubberband image variables

Revision 3.10  2004/05/05 17:33:46  mast
Added call to get rubberband coordinates

Revision 3.9  2003/09/25 21:10:34  mast
Removed unneeded starting geometry member

Revision 3.8  2003/09/18 00:44:13  mast
Added declaration for function to get subset boundaries

Revision 3.7  2003/09/17 04:43:51  mast
Add declarations for window size functions

Revision 3.6  2003/09/16 02:56:21  mast
Added an xzoom variable to keep track of actually displayed fractional zoom

Revision 3.5  2003/03/12 06:38:18  mast
Added time mismatch function

Revision 3.4  2003/03/03 22:13:38  mast
Added variable for starting the rubber band

Revision 3.3  2003/02/10 20:41:56  mast
Merge Qt source

Revision 3.2.2.8  2003/01/30 06:17:47  mast
Add ability to change range of Z slider on image flip

Revision 3.2.2.7  2003/01/30 00:47:35  mast
Cleanup with new timer logic

Revision 3.2.2.6  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 3.2.2.5  2003/01/04 03:42:20  mast
simplified closing logic

Revision 3.2.2.4  2002/12/14 05:23:42  mast
backing out the fancy subclass, adjusting for new visual detection

Revision 3.2.2.3  2002/12/13 06:06:30  mast
using new glmainwindow and mainglwidget classes

Revision 3.2.2.2  2002/12/10 16:57:34  mast
preventing multiple draws, implementing current contour draw while dragging

Revision 3.2.2.1  2002/12/09 17:50:17  mast
Initial changes to get Qt version

Revision 3.2  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.1  2002/09/13 21:04:57  mast
Added resizeSkipDraw to prevent redraws during resize

*/
#endif
