/*
 *  sslice.h -- Include file for slicer.c, slicer window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  No more Log
 */

#ifndef SSLICE_H
#define SSLICE_H

#define SLICE_ZSCALE_OFF    0
#define SLICE_ZSCALE_BEFORE 1
#define SLICE_ZSCALE_AFTER  2

class SlicerWindow;
class SlicerGL;
class SlicerCube;
class QKeyEvent;
class QMouseEvent;

typedef struct ViewInfo ImodView;
typedef struct b3d_ci_image B3dCIImage;
typedef struct imodel_matrix Imat;
typedef struct Mod_Point Ipoint;

class SlicerFuncs {

 public:
  SlicerFuncs(ImodView *vi);
  ~SlicerFuncs() {};
  void help();
  void stepZoom(int dir);
  void stepTime(int dir);
  void enteredZoom(float newZoom);
  void showSlice();
  void fillCache();
  void stateToggled(int index, int state);
  void angleChanged(int axis, int value, int dragging);
  void Zscale(int item);
  void imageThickness(int sno);
  void modelThickness(float mDepth);
  void resize(int mWinx, int mWiny);
  void cubeResize(int mWinx, int mWiny);
  void closing();
  void keyInput(QKeyEvent *event);
  void keyRelease(QKeyEvent *event);
  void mousePress(QMouseEvent *event);
  void mouseRelease(QMouseEvent *event);
  void mouseMove(QMouseEvent *event);
  void paint();
  void cubePaint();
  void transStep();
  float getZScaleBefore();
  void setForwardMatrix();
  int anglesFromContour();
  void checkMovieLimits();
  void setCurrentOrNewRow(bool newrow);
  void setAnglesFromRow();
  void externalDraw(ImodView *vi, int drawflag);
  void draw();
  int synchronizeSlicers(bool draw = false);
  void changeCenterIfLinked();
  void getSubsetLimits(int &ixStart, int &iyStart, int &nxUse, int &nyUse);
  int namedSnapshot(QString &fname, int format, bool checkConvert);
  void toggleArrow(bool drawWin = true);
  void rotateOnViewAxis(int deltaX, int deltaY, int deltaZ);
  void setCursor(int mode, bool setAnyway = false);

 private:
  void cubeDraw();
  void drawSelfAndLinked();
  int setxyz(int x, int y);
  int getxyz(int x, int y, float &xm, float &ym, float &zm);
  void attachPoint(int x, int y, int ctrlDown);
  void insertPoint(int x, int y, int ctrl);
  void modifyPoint(int x, int y, int ctrl);
  void updateImage();
  void drawThickControls();
  void drawModel();
  void drawCurrentPoint();
  void setAnglesFromPoints(Ipoint *p1, Ipoint *p2, int axis);
  double fixangle(double angle);
  void startMovieCheckSnap(int dir);
  void setMovieLimits(int axis);
  void findMovieAxis(int *xmovie, int *ymovie, int *zmovie, int dir, int *axis);
  void setInverseMatrix();
  int translateByRotatedVec(Ipoint *vec, bool outsideOK = false);
  void setClassicMode(int state);
  void getNormalToPlane(Ipoint *norm);
  void getWindowCoords(float xcur, float ycur, float zcur, int &xim, int &yim, int &zim);
  void setViewAxisRotation(float x, float y, float z);
  void getMontageShifts(int factor, float scaleFactor, int win, int &overlap,
                        float &transStart, float &transDelta, int &copyDelta,
                        int &fullSize);
  void montageSnapshot(int snaptype);
  void fillImageArray(int panning, int meanOnly, int rgbChannel);

 public:
  SlicerWindow *mQtWindow;
  ImodView *mVi;
  float mCx, mCy, mCz;  /* current x, y, z */
  float mTang[3];       /* transform angles */
  int mLocked;
  int mDrawModView;     /* Draw flag to draw model view on next redraw */
  bool mAlreadyDrew;    /* Flag that window was already drawn before draw_cb */
  int mTimeLock;        /* Time value if time is locked */
  bool mContinuous;     /* Flag for continuous mode in slicer angle update */
  bool mLinked;         /* Flag for linked to other slicers */
  int mClassic;
  int   mCtrl;
  float  mZoom;
  float mScaleBarSize;     /* Actual size of draw scale bar in model units */

 private:
  SlicerGL     *mGlw;
  SlicerCube   *mCube;
  int          mScalez;
  int          mMousemode;    /* value for keeping track of cursor */
  int          mShiftLock;    /* toggle button state for shift key or mouse */

  B3dCIImage   *mImage;
  unsigned char *mRedTemp;        /* Arrays to save channels for RGB */
  unsigned char *mGreenTemp;
  
  int    mWinx, mWiny;
  float  mLx, mLy, mLz;  /* last set point for x, y, z */
  float  mDepth;
  int    mLastangle;   /* Last angle slider that was used */
  
  /* coords for plane intersection */
  /* used for show slice location  */
  int mZx1, mZx2;
  int mZy1, mZy2;
  
  /* data for high res slicer */
  int   mHq;
  float mXsz, mYsz, mZsz;
  float mXo, mYo, mZo;

  float mLang[3];    /* last angles drawn */
  float mXstep[3];   /* change in x, y, z image for step in x' slicer */
  float mYstep[3];   /* change in x, y, z image for step in y' slicer */
  float mZstep[3];
  
  float mXzoom, mYzoom;
  int   mImageFilled;
  bool  mNoPixelZoom;   /* Flag that image data are to be drawn at 1:1 zoom */
  float mOneSliceMean, mOneSliceSD;   /* mean and SD of single center slice */
  bool  mScaleToMeanSD; /* Flag that multi-slice should be scaled to single */

  float mXshift, mYshift;   /* Shifts to apply to raster for low-res draw */
  float mZslast;           /* Value of zscale on last draw_cb */
  int mPending;            /* Flag that there are pending coords from hit */
  float mPendx, mPendy, mPendz;   /* pending coords */
  int mMovieSnapCount;     /* Counter for doing movie snapshots */
  float mOrigZmouse;       /* Original zxmouse when page up/down started */
  float mOrigAngles[3];    /* Original angles at that time */
  float mLastXmouse;       /* Last xmouse left by a page up/down */
  float mLastYmouse;
  float mLastZmouse;
  float mCumPageMoves;     /* Cumulative page up/down move since original */
  float mDrawnXmouse;      /* xmouse, etc when last drawn */
  float mDrawnYmouse;
  float mDrawnZmouse;
  int mIgnoreCurPtChg;     /* Flag to ignore a current point change in draw */
  bool mArrowOn;           // Arrow flag
  bool mDrawingArrow;      // And flag for initial drawing
  Ipoint mArrowTail;       // Image coordinates of head and tail
  Ipoint mArrowHead;
  float mArrowAngle[3];    // Angle at which arrow is valid
  int mLastShape;          // Last shape for cursor

  short mNslice;       /* Number of slices to draw */
  Imat  *mMat;
  int   mFftMode;      /* Flag to do FFT of slice */
  int   mToolTime;     /* Value of time when label sent to toolbar */
  bool  mNeedDraw;     /* Flag that window needs a draw regardless of flags*/
  int   mClosing;      /* Flag for window closing */
};     

void slicerCubicFillin(unsigned short *cidata, int winx, int winy, int izoom,
                       int ilimshort, int jlimshort, int minval, int maxval, int intData);
int sslice_open(ImodView *vi);
int slicerAnglesOpen();
void slicerAnglesClosing();
void slicerPixelViewState(bool state);
void slicerReportAngles();
SlicerFuncs *getTopSlicer();
int setTopSlicerAngles(float angles[3], Ipoint *center, bool draw);
int setTopSlicerFromModelView(Ipoint *rot);
int getTopSlicerTime(bool &continuous);
void slicerNewTime(bool refresh);
void slicerViewAxisStepChange(int delta);
int getSlicerThicknessScaling();
#endif
