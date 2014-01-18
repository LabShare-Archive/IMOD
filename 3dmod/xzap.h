/*
 *  xzap.h -- Header file for ZaP Window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#ifndef XZAP_H
#define XZAP_H

#include "control.h"
#include <QMouseEvent>
#include <QKeyEvent>
#include <QEvent>
#include <vector>

class ZapWindow;
class ZapGL;
class QString;

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
  void getixy(int mx, int my, float &x, float &y, int &z);
  void montageSnapshot(int snaptype);
  int namedSnapshot(QString &fname, int format, bool checkConvert, bool fullArea);
  void setCursor(int mode, bool setAnyway = false);
  void setMouseTracking();
  void shiftRubberband(float idx, float idy);
  void syncImage(bool toImagePt);
  void toggleRubberband(bool draw = true);
  void toggleLasso(bool draw = true);
  void toggleArrow(bool draw = true);
  void clearArrows();
  void startAddedArrow();
  void setSnapshotLimits(int **limits, int *limarr);
  bool getLowHighSection(int &low, int &high);
  Icont *getLassoContour();
  getMember(int, TimeLock);
  B3dCIImage *zoomedDownImage(int subset, int &nxim, int &nyim, int &ixStart,
                              int &iyStart, int &nxUse, int &nyUse, int &uzXstart,
                              int &uzYstart, int &uzXuse, int &uzYuse);
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
  void limitContourShift(Icont *cont, float &ix, float &iy);
  int defaultXformCenter(float &xcen, float &ycen);
  int startMovieCheckSnap(int dir);
  void registerDragAdditions();
  void transformContour(Icont *cont, float mat[2][2], float ix, float iy, int button);
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
  void lockedPageUpOrDown(int shifted, int direction);

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
  bool mLassoOn;         // Lasso flag
  bool mDrawingLasso;    // And flag for initial drawing
  int mLassoObjNum;   // Extra object number for lasso
  bool mArrowOn;       // Arrow flag
  bool mDrawingArrow;  // And flag for initial drawing
  std::vector<Ipoint> mArrowTail;   // Image coordinates of head and tail (Z is 0)
  std::vector<Ipoint> mArrowHead;

 private:
  int    mXborder,   mYborder;   /* border around image window. */
  int    mXstart,    mYstart;    // Original starting positions in image drawn in window
  int    mXposStart, mYposStart; // Starting positions adjusted for pyramid offset
  int    mXlastStart, mYlastStart; // So tile cache draw can keep track of changes
  int    mXlastSize, mYlastSize;   // in the region drawn
  int    mLastStatus;            // And to keep track if this is a redraw with more data
  int    mXdrawsize, mYdrawsize;
  int    mLmx,       mLmy;
  Icont *mTessCont;

  int    mHqgfx, mHide;
  int    mHqgfxsave;           /* Place to save hqgfx when dragging */
  int    mDrawCurrentOnly;
  int    mLastHqDrawTime;

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
ZapFuncs *getTopZapWindow(bool withBand, bool withLasso = false, 
                          int type = ZAP_WINDOW_TYPE, int *index = NULL);
Icont *getTopZapLassoContour(bool aboveBand);
int getTopZapMouse(Ipoint *imagePt);
int  imod_zap_open(ImodView *vi, int wintype);
int zapSubsetLimits(ImodView *vi, int &ixStart, int &iyStart, int &nxUse, int &nyUse);
void zapReportRubberband();
void zapSetImageOrBandCenter(float imx, float imy, bool incremental);
int zapRubberbandCoords(float &rbX0, float &rbX1, float &rbY0, float &rbY1);
void zapPixelViewState(bool state);
void zapSetMouseTracking();
void zapSetNextOpenHQstate(int state);

#endif
