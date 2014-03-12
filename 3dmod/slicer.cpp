/*
 *  slicer.cpp -- Open the slicer window; Slice 3-D data at any angle.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <math.h>
#include <limits.h>
#include <string.h>
#include <qcursor.h>
#include <qapplication.h>
#include <qobject.h>
#include <qdatetime.h>
#include <qcheckbox.h>
#include <qpushbutton.h>
//Added by qt3to4:
#include <QMouseEvent>
#include <QKeyEvent>

#include "slicer_classes.h"
#include "rotationtool.h"
#include "hottoolbar.h"
#include "imod.h"
#include "display.h"
#include "b3dgfx.h"
#include "sslice.h"
#include "imod_input.h"
#include "info_cb.h"
#include "info_setup.h"
#include "control.h"
#include "imodplug.h"
#include "dia_qtutils.h"
#include "xcramp.h"
#include "xcorr.h"
#include "imod_edit.h"
#include "pixelview.h"
#include "model_edit.h"
#include "moviecon.h"
#include "workprocs.h"
#include "preferences.h"
#include "form_slicerangle.h"
#include "mv_input.h"
#include "imodv.h"
#include "scalebar.h"
#include "cachefill.h"
#include "pyramidcache.h"

/* internal functions. */
static void slicerKey_cb(ImodView *vi, void *client, int released, QKeyEvent *e);
static void slicerDraw_cb(ImodView *vi, void *client, int drawflag);
static void slicerClose_cb(ImodView *vi, void *client, int junk);
static void notifySlicersOfAngDia(bool open);
static void setAngleToolbarState(SlicerWindow *slicer, bool open);

/* DNM: maximum angles for sliders */
static float sMaxAngle[3] = {90.0, 180.0, 180.0};
static int sShiftPressed = 0;
static int sHotControlPressed = 0;
static bool sPixelViewOpen = false;
static SlicerAngleForm *sSliceAngDia = NULL;
static int sSliderDragging = 0; 
static QTime sBut1downt;
static int sFirstMouseX, sFirstMouseY, sLastMouseX, sLastMouseY;
static int sMousePanning = 0;
static int sMouseRotating = 0;
static bool sMousePressed = false;
static float sViewAxisSteps[] = {0.1f, 0.3f, 1., 3., 10., 30., 90., 0.};
static int sViewAxisIndex = 2;
static bool sDoingMontage = false;
static int sScaleThick = 1;

/*
 * Open new slicer.
 */
int slicerOpen(ImodView *vi, int autoLink)
{
  SlicerFuncs *ss;

  ss = new SlicerFuncs(vi, autoLink);
  if (!ss) {
    wprint("Error opening slicer window.");
    return(-1);
  }
  return(0);
}

// Open or raise the slicer angle window
int slicerAnglesOpen()
{
  if (sSliceAngDia) {
    sSliceAngDia->raise();
    return 0;
  }
  sSliceAngDia = new SlicerAngleForm(imodDialogManager.parent(IMOD_DIALOG), 
                                    Qt::Window);
  if (!sSliceAngDia)
    return -1;
  imodDialogManager.add((QWidget *)sSliceAngDia, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)sSliceAngDia, IMOD_DIALOG);
  notifySlicersOfAngDia(true);
  return 0;
}

// The slicer angle window is closing
void slicerAnglesClosing()
{
  imodDialogManager.remove((QWidget *)sSliceAngDia);
  sSliceAngDia = NULL;
  notifySlicersOfAngDia(false);
}

// Update all slicers about state of the slicer angle window
static void notifySlicersOfAngDia(bool open)
{
  QObjectList objList;
  SlicerWindow *slicer;
  int i;

  imodDialogManager.windowList(&objList, -1, SLICER_WINDOW_TYPE);
  for (i = 0; i < objList.count(); i++) {
    slicer = (SlicerWindow *)objList.at(i);
    setAngleToolbarState(slicer, open);
  }
}

static void setAngleToolbarState(SlicerWindow *slicer, bool open)
{
  slicer->mAutoBox->setEnabled(open);
  slicer->mSetAngBut->setEnabled(open);
  slicer->mSaveAngBut->setEnabled(open);
  slicer->mNewRowBut->setEnabled(open);
  if (open) {
    slicer->showSaveAngleToolbar();
  } else {
    
    // Turn off continuous when closing
    diaSetChecked(slicer->mAutoBox, false);
    slicer->mFuncs->mContinuous = false;
    slicer->mSaveAngBar->hide();
  }
}
  
// The pixel view window has opened or closed, set mouse tracking for all
void slicerPixelViewState(bool state)
{
  sPixelViewOpen = state;
  QObjectList objList;
  SlicerWindow *slicer;
  int i;

  sPixelViewOpen = state;
  imodDialogManager.windowList(&objList, -1, SLICER_WINDOW_TYPE);

  for (i = 0; i < objList.count(); i++) {
    slicer = (SlicerWindow *)objList.at(i);
    slicer->mGLw->setMouseTracking(state);
  }
}

// Report the angles of the first slicer window
void slicerReportAngles()
{
  SlicerFuncs *ss = getTopSlicer();

  if (!ss) {
    imodPrintStderr("ERROR: No slicer windows open\n");
    return;
  }
  imodPrintStderr("Slicer angles: %.1f %.1f %.1f\n", ss->mTang[b3dX],
                  ss->mTang[b3dY], ss->mTang[b3dZ]);
}

// Find the slicer window that is nearest to the top of the control list; optionally
// looking for one with a rubberband starting or set.  The control list index is returned
// in *index if index is nonNULL.
SlicerFuncs *getTopSlicer(bool withBand, int *index)
{
  QObject *obj = imodDialogManager.getTopWindow(withBand, false, SLICER_WINDOW_TYPE,
                                                index);
  return obj ? ((SlicerWindow *)obj)->mFuncs : NULL;
}

// Called from slicer angle window to set the angles and position of the
// top slicer
int setTopSlicerAngles(float angles[3], Ipoint *center, bool draw)
{
  int drawflag = (draw && imodvLinkedToSlicer()) ? IMOD_DRAW_MOD : 0;
  SlicerFuncs *ss = getTopSlicer();
  if (!ss)
    return 1;
  for (int ixyz = 0; ixyz < 3; ixyz++) {
    ss->mTang[ixyz] = angles[ixyz];
    B3DCLAMP(ss->mTang[ixyz], -sMaxAngle[ixyz], sMaxAngle[ixyz]);
  }
  ss->mQtWindow->setAngles(ss->mTang);
  ss->mCx = B3DMAX(0., B3DMIN(center->x, ss->mVi->xsize - 1));
  ss->mCy = B3DMAX(0., B3DMIN(center->y, ss->mVi->ysize - 1));
  ss->mCz = B3DMAX(0., B3DMIN(center->z, ss->mVi->zsize - 1));
  if (!ss->mLocked) {
    ss->mVi->xmouse = ss->mCx;
    ss->mVi->ymouse = ss->mCy;
    ss->mVi->zmouse = ss->mCz;
    if (draw) {
      ss->synchronizeSlicers();
      imodDraw(ss->mVi, IMOD_DRAW_XYZ | IMOD_DRAW_SLICE | drawflag);
    }
  } else if (draw) {

    // This needs a guaranteed draw because the top slicer may not
    // be the top or active window
    ss->mDrawModView = drawflag;
    ss->draw();
    ss->mAlreadyDrew = true;
    ss->synchronizeSlicers();
    ss->showSlice();
  }
  return 0;
}

// Set zoom (used initially)
int setTopSlicerZoom(float zoom, bool draw)
{
  SlicerFuncs *ss = getTopSlicer();
  if (!ss || zoom < 0.005 || zoom > 200.)
    return 1;
  ss->setInitialZoom(zoom);
  if (draw)
    ss->draw();
  return 0;
}

int setTopSlicerFromModelView(Ipoint *rot)
{
  SlicerFuncs *ss = getTopSlicer();
  if (!ss)
    return 1;
  ss->setForwardMatrix();
  ss->mTang[b3dX] = rot->x;
  ss->mTang[b3dY] = rot->y;
  ss->mTang[b3dZ] = rot->z;
  ss->changeCenterIfLinked();
  ss->mQtWindow->setAngles(ss->mTang);
  ss->draw();
  ss->mAlreadyDrew = true;
  ss->synchronizeSlicers();
  imodDraw(ss->mVi, IMOD_DRAW_XYZ | IMOD_DRAW_SLICE | IMOD_DRAW_SKIPMODV);
  return 0;
}

// Return the angles and position of the top slicer, and the time, to the 
// slicer angle window (exported, declaration in imodview.h)
int getTopSlicerAngles(float angles[3], Ipoint *center, int &time)
{
  SlicerFuncs *ss = getTopSlicer();
  if (!ss)
    return 1;
  angles[0] = ss->mTang[b3dX];
  angles[1] = ss->mTang[b3dY];
  angles[2] = ss->mTang[b3dZ];
  center->x = ss->mCx;
  center->y = ss->mCy;
  center->z = ss->mCz;
  time = ivwWindowTime(ss->mVi, ss->mTimeLock);
  return 0;
}

// Just return the time of the top slicer window and the continuous state
int getTopSlicerTime(bool &continuous)
{
  SlicerFuncs *ss = getTopSlicer();
  if (!ss)
    return -1;
  continuous = ss->mContinuous;
  return ivwWindowTime(ss->mVi, ss->mTimeLock);
}

// Notify the slicer angle dialog of a new time or general need to refresh
void slicerNewTime(bool refresh)
{
  if (sSliceAngDia)
    sSliceAngDia->newTime(refresh);
  if (imodvLinkedToSlicer())
    imodv_draw();
}

int getSlicerThicknessScaling()
{
  return sScaleThick;
}

/*
 * Change the step size for view axis rotation, which is a static value
 */
void slicerViewAxisStepChange(int delta)
{
  QObjectList objList;
  SlicerWindow *slicer;
  int i = sViewAxisIndex;
  if (delta > 0 && sViewAxisSteps[sViewAxisIndex + 1])
    sViewAxisIndex++;
  else if (delta < 0) 
    sViewAxisIndex = B3DMAX(0, sViewAxisIndex - 1);
  if (i == sViewAxisIndex)
    return;

  imodDialogManager.windowList(&objList, -1, SLICER_WINDOW_TYPE);
  for (i = 0; i < objList.count(); i++) {
    slicer = (SlicerWindow *)objList.at(i);
    slicer->mRotationTool->setStepLabel(sViewAxisSteps[sViewAxisIndex]);
  }
}

/*
 * Open a separate slicer locked to every time if possible, with one slice toolbar 
 * floated, and arrange them in an array
 */
void setupLinkedSlicers(ImodView *vi)
{
  QObjectList objList;
  SlicerWindow *slicer;
  int i, deskWidth, deskHeight, newWidth, newHeight, firstLeft, firstTop, numInX,  numInY;
  int ix = 0, iy = 0;
  int xBorder = 16, yBorder = 40;
#ifdef Q_OS_MACX
  int topIndent = 24;
#else
  int topIndent =10;
#endif

  // First unlink any existing slicers
  imodDialogManager.windowList(&objList, -1, SLICER_WINDOW_TYPE);
  for (i = 0; i < objList.count(); i++) {
    slicer = (SlicerWindow *)objList.at(i);
    if (slicer->mFuncs->mLinked)
      diaSetChecked(slicer->mLinkBox, false);
    slicer->mFuncs->mLinked = false;
  }

  // Create the first slicer and determine its size and that of the toolbar
  if (slicerOpen(vi, 1))
    return;
  diaMaximumWindowSize(deskWidth, deskHeight);
  deskWidth -= 20;
  deskHeight -= 10 + topIndent;

  imodDialogManager.windowList(&objList, -1, SLICER_WINDOW_TYPE);
  slicer = (SlicerWindow *)objList.at(objList.count() - 1);
  imod_info_input();
  slicer->mFreeBar2->show();
  imod_info_input();

  // On Linux, the toolbar has the full size if we process events enough, but the 
  // window frame geometry might not be right yet
  QRect toolGeom = slicer->mFreeBar2->frameGeometry();
  QRect fullGeom = slicer->frameGeometry();
  QRect winGeom = ivwRestorableGeometry(slicer);
  newWidth = B3DMAX(winGeom.width(), toolGeom.width() + 20);
  newHeight = winGeom.height();
  imodTrace('s', "frame geom %d  %d  geom %d h %d  tool geom %d %d", 
            fullGeom.width(), fullGeom.height(),
            winGeom.width(), newHeight, toolGeom.width(), toolGeom.height());

  // If there appear to be some plausible borders on the window already, take those;
  // otherwise take the fallback borders
  if (fullGeom.width() - winGeom.width() > xBorder / 2 || 
      fullGeom.height() - newHeight > yBorder / 2) {
    xBorder = fullGeom.width() - winGeom.width();
    yBorder = fullGeom.height() - newHeight;
  }
  imodTrace('s', "border %d %d", xBorder, yBorder);

  // Find an arrangement where as many windows as possible fit on the screen, looking
  // first at having the toolbar above them all, then to the left of them all, and
  // dropping the window size by 10,10 each time
  while (newWidth > winGeom.width() / 2 && newHeight > winGeom.height() / 2) {
    firstLeft = 10;

    // Allow some space below toolbar because on gnome the size still doesn't have title 
    // bar included
    firstTop = topIndent + toolGeom.height() + 24;
    numInX = B3DMAX(1, deskWidth / (newWidth + xBorder));
    numInY = B3DMAX(1, (deskHeight - toolGeom.height()) / (newHeight + yBorder));
    if (numInX * numInY >= vi->numTimes)
      break;
    firstLeft = 10 + toolGeom.width();
    firstTop = topIndent;
    numInX = B3DMAX(1, (deskWidth - toolGeom.width()) / (newWidth + xBorder));
    numInY = B3DMAX(1, deskHeight / (newHeight + yBorder));
    if (numInX * numInY >= vi->numTimes)
      break;
    newWidth -= 10;
    newHeight -= 10;
  }
  
  // Open the windows and place them all
  slicer->mFreeBar2->move
    (10 + (firstLeft == 10 ? (deskWidth - toolGeom.width()) / 2 : 0),
     topIndent + (firstLeft == 10 ? 0 : (deskHeight - toolGeom.height()) / 2));
  ix = 0;
  iy = 0;
  for (i = 1; i <= B3DMIN(numInX * numInY, vi->numTimes); i++) {
    if (i > 1 && slicerOpen(vi, i))
      return;
    imodDialogManager.windowList(&objList, -1, SLICER_WINDOW_TYPE);
    slicer = (SlicerWindow *)objList.at(objList.count() - 1);
    slicer->resize(newWidth, newHeight);
    slicer->move(firstLeft + ix * (newWidth + xBorder), 
                 firstTop + iy * (newHeight + yBorder));
    ix++;
    if (ix == numInX) {
      iy++;
      ix = 0;
    }
  }

  // raise the info window at the end so the user can put it somewhere
  ImodInfoWin->raise();
}

/*
 *  The external draw command from the controller
 */
static void slicerDraw_cb(ImodView *vi, void *client, int drawflag)
{
  SlicerFuncs *ss = (SlicerFuncs *)client;
  if (ss)
    ss->externalDraw(vi, drawflag);
}

// A key passed on from elsewhere
// Do not pass on the hot slider key that would cause a grab
static void slicerKey_cb(ImodView *vi, void *client, int released, 
			 QKeyEvent *e)
{
  SlicerFuncs *ss = (SlicerFuncs *)client;
  if (e->key() == hotSliderKey())
    return;
  if (released)
    ss->keyRelease(e);
  else
    ss->keyInput(e);
}

/* The signal from the controller to close the window */
static void slicerClose_cb(ImodView *vi, void *client, int junk)
{
  SlicerFuncs *ss = (SlicerFuncs *)client;
  ss->mQtWindow->close();
}

//////////////////////////////////////////////////////////////////
// The SlicerFuncs class
//
SlicerFuncs::SlicerFuncs(ImodView *vi, int autoLink)
{
  int newZoom;
  QString str;
  mVi = vi;
  mRedTemp = NULL;

  /* DNM 5/16/02: if the current position is still in the lower left
     corner, move it to middle and draw other windows */
  /* 5/5/03: take away DRAW IMAGE flag */
  if (!vi->xmouse && !vi->ymouse) {
    vi->xmouse = vi->xsize / 2;
    vi->ymouse = vi->ysize / 2;
    imodDraw(vi, IMOD_DRAW_XYZ);
  }

  mClassic = ImodPrefs->classicSlicer();
  // After 6 1/2 years, we can get rid of the Welcome to the new mode

  mCx = vi->xmouse;
  mCy = vi->ymouse;
  mCz = vi->zmouse;
  mVi     = vi;
  mLocked = 0;
  mTimeLock = autoLink;
  mAutoLink = B3DMIN(2, autoLink);
  mLinked = autoLink > 0;
  mShiftLock = 0;
  mZoom   = 1.0;
  mLx     = vi->xmouse;
  mLy     = vi->ymouse;
  mLz     = vi->zmouse;
  mOrigAngles[0]  = -1000.;
  mHq     = 0;
  mTang[b3dX] = 0.0f;
  mTang[b3dY] = 0.0f;
  mTang[b3dZ] = 0.0f;
  mLang[b3dX] = 0.0f;
  mLang[b3dY] = 0.0f;
  mLang[b3dZ] = 0.0f;
  mScalez = 0;
  mDepth = 1.0;
  mImage = NULL;
  mRedTemp = NULL;
  mGreenTemp = NULL;
  mXstep[0]  = 1.0f; mXstep[1] = mXstep[2] = 0.0f;
  mYstep[1]  = 1.0f; mYstep[0] = mYstep[2] = 0.0f;
  mNslice = 1;
  mMat = imodMatNew(3);
  mLastangle = b3dX;
  mZslast = 1.0;
  mPending = 0;
  mImageFilled = 0;
  mLastShape = -1;
  mMovieSnapCount = 0;
  mFftMode = 0;
  mToolTime = 0;
  mContinuous = false;
  mClosing = 0;
  mIgnoreCurPtChg = 0;
  mAlreadyDrew = false;
  mNeedDraw = false;
  mDrawingArrow = false;
  mArrowOn = false;
  mRubberband = 0;
  mStartingBand = 0;
  mMoveBand = 0;
  mFirstDrag = 0;

  transStep();
  utilGetLongestTimeString(vi, &str);
  mQtWindow = new SlicerWindow(this, sMaxAngle,  str, App->rgba, App->doublebuffer,
                               App->qtEnableDepth, imodDialogManager.parent(IMOD_IMAGE));
  if (!mQtWindow)
    return;

  mGlw = mQtWindow->mGLw;
  mCube = mQtWindow->mCube;
  if (!App->rgba)
    mGlw->setColormap(*(App->qColormap));

  mQtWindow->setWindowTitle(imodCaption("3dmod Slicer"));
	
  mCtrl = ivwNewControl(vi, slicerDraw_cb, slicerClose_cb, slicerKey_cb, (void *)this);
  imodDialogManager.add((QWidget *)mQtWindow, IMOD_IMAGE, SLICER_WINDOW_TYPE, mCtrl);

  // Set up cursor
  setCursor(vi->imod->mousemode);

  // Initialize controls
  mQtWindow->setZoomText(mZoom);
  drawThickControls();
  mQtWindow->setAngles(mTang);
  updateViewAxisPos();

  // Include this to get toolbar sizes right
  imod_info_input();

  QSize toolSize1 = mQtWindow->mToolBar->sizeHint();
  QSize toolSize2;
  if (mAutoLink == 1 && mQtWindow->mFreeBar2) { 
    toolSize2 = mQtWindow->mFreeBar2->sizeHint();
    mQtWindow->mFreeBar2->setMaximumWidth(toolSize2.width() + 10);
  } else if (mQtWindow->mToolBar2) {
    toolSize2 = mQtWindow->mToolBar2->sizeHint();
    mQtWindow->mToolBar2->setMaximumWidth(toolSize2.width() + 10);
  }
  int newWidth = toolSize1.width() > toolSize2.width() ?
    toolSize1.width() : toolSize2.width();
  int newHeight = newWidth + toolSize1.height() + (mAutoLink ? 0 : toolSize2.height());
  mQtWindow->resize( newWidth, newHeight);

  // Adjust zoom to biggest one that fits image
  if (vi->xsize < newWidth && vi->ysize < newWidth) {
    mZoom = (2. * newWidth) / vi->xsize;
    while ((mZoom * vi->xsize > 1.06 * newWidth || 
           mZoom * vi->ysize > 1.06 * newWidth)) {
      newZoom = b3dStepPixelZoom(mZoom, -1);
      if (fabs(newZoom - mZoom) < 0.0001)
        break;
      mZoom = (float)newZoom;
    }
    setInitialZoom(mZoom);
  }
  
  setAngleToolbarState(mQtWindow, sSliceAngDia != NULL);

  adjustGeometryAndShow((QWidget *)mQtWindow, IMOD_IMAGE, false);
  mGlw->setMouseTracking(sPixelViewOpen);
}

// Set the zoom initially: switch to HQ if it is high enough
void SlicerFuncs::setInitialZoom(float zoom)
{
  mZoom = zoom;
  mQtWindow->setZoomText(mZoom);
  if (mZoom > 1.5) {
    mHq = 1;
    mQtWindow->setToggleState(0, 1);
  }
}

void SlicerFuncs::externalDraw(ImodView *vi, int drawflag)
{
  float usex, usey, usez, factor = 0.;
  int ignoreChg = mIgnoreCurPtChg;
  int *limits;
  int limarr[4];
  float zStep[3];
  getNormalToPlane(zStep);

  // Clear ignore flag before any possible returns
  // Skip out if already drawn
  mIgnoreCurPtChg = 0;
  if (mAlreadyDrew) {
    mAlreadyDrew = false;
    if (!(drawflag & IMOD_DRAW_COLORMAP))
      return;
  }
  
  if (drawflag & IMOD_DRAW_COLORMAP) {
    mGlw->setColormap(*(App->qColormap));
    return;
  }

  // Clamp the cx, cy, cz to the current data size in case of image flip
  mCx = B3DMIN(vi->xsize, B3DMAX(0, mCx));
  mCy = B3DMIN(vi->ysize, B3DMAX(0, mCy));
  mCz = B3DMIN(vi->zsize - 0.5, B3DMAX(0, mCz));

  /* Adjust the cursor if necessary */
  setCursor(vi->imod->mousemode);

  if (imodDebug('s'))
    imodPrintStderr("flags on draw %x \n", drawflag);

  /* DNM: use a value saved in structure in case more than one window */
  if (mZslast != mVi->imod->zscale){
    mZslast = mVi->imod->zscale;
    drawflag |= IMOD_DRAW_ACTIVE;
  }

  // Did this slicer start the current movie?
  if ((vi->xmovie || vi->ymovie || vi->zmovie) && 
      imcGetStarterID() == mCtrl) {

    // Then determine factor for moving in one step on dominant axis
    if (vi->xmovie && fabs((double)zStep[b3dX]) > 1.e-6)
      factor = (vi->xmouse - mCx) / zStep[b3dX];
    else if (vi->ymovie && fabs((double)zStep[b3dY]) > 1.e-6)
      factor = (vi->ymouse - mCy) / zStep[b3dY];
    else if (vi->zmovie && fabs((double)zStep[b3dZ]) > 1.e-6)
      factor = (vi->zmouse - mCz) / zStep[b3dZ];

    /*imodPrintStderr("%d %d %d factor %f mouse %.1f %.1f %.1f  "
            "cur %.1f %.1f %.1f\n", vi->xmovie, 
            vi->ymovie, vi->zmovie, factor, vi->xmouse, 
            vi->ymouse, vi->zmouse, mCx, mCy, mCz); */

    // Compute new position and bound it (may not be needed unless user
    // clicks a new position during movie)
    if (factor != 0.) {
      vi->xmouse = mCx + factor * zStep[b3dX];
      vi->ymouse = mCy + factor * zStep[b3dY];
      mCz += factor * zStep[b3dZ];
      if (mCz < 0.)
        mCz = 0.;
      if (mCz > vi->zsize - 1.)
        mCz = vi->zsize - 1.;
      vi->zmouse = (int)floor(mCz + 0.5);
      ivwBindMouse(vi);
      mCx = vi->xmouse;
      mCy = vi->ymouse;

      mPending = 0;
      mGlw->updateGL();

      // Get snapshots if there is a count for doing so
      if (imcGetSnapshot(vi) && mMovieSnapCount) {
        if (imcGetSlicerMontage(true)) {
          montageSnapshot(imcGetSnapshot(vi));
        } else {
          setSnapshotLimits(&limits, limarr);
          b3dKeySnapshot("slicer", imcGetSnapshot(vi) - 1, imcGetSnapshot(vi) % 2, 
                         limits);
        }
        mMovieSnapCount--;

        /* When count expires, stop movie */
        if(!mMovieSnapCount) {
          imodMovieXYZT(vi, 0, 0, 0, 0);
          b3dSetMovieSnapping(false);
        }
      }
      cubeDraw();
      return;
    }
  }

  // Simply draw if need flag is set 
  if (mNeedDraw) {
    if (imodDebug('s'))
      imodPrintStderr("NEED draw for ID %d\n", mCtrl);
    draw();
    return;
  }

  // Process a change in current point when not locked
  if ((drawflag & IMOD_DRAW_XYZ) && !mLocked) {

    if (mClassic) {
      /* DNM: if there is a pending set of values from a mouse hit,
         use them instead of the mouse values for this */
      if (mPending) {
        usex = mPendx;
        usey = mPendy;
        usez = mPendz;
      } else {
        usex = vi->xmouse;
        usey = vi->ymouse;
        usez = vi->zmouse;
      }
      if ((mLx != usex) ||
          (mLy != usey) ||
          (mLz != usez)) {
        mCx = usex;
        mCy = usey;
        mCz = usez;
        mPending = 0;
        if (imodDebug('s'))
          imodPrintStderr("XYZ draw at %f %f %f\n", mCx, mCy, mCz);
        draw();
        return;
      }
    } else {
      
      /* New mode: If our drawing position is the same as before, step cz by
         amount determined by change in zmouse; step cx and cy if there are
         single steps detected.  This will respond to arrow keys in other
         windows but not to clicking a new point. */
      if (mLx == mCx && mLy == mCy && mLz == mCz &&
          mLang[0] == mTang[0] && mLang[1] == mTang[1] &&
          mLang[2] == mTang[2] && !ignoreChg) {
        if (vi->xmouse == mDrawnXmouse && vi->ymouse == mDrawnYmouse && 
            vi->zmouse != mDrawnZmouse) {
          mCz += vi->zmouse - mDrawnZmouse;
          mCz = B3DMIN(vi->zsize - 0.5, B3DMAX(0, mCz));
        } else if (vi->xmouse == mDrawnXmouse && vi->ymouse != 
                   mDrawnYmouse && vi->zmouse == mDrawnZmouse &&
                   fabs((double)vi->ymouse - mDrawnYmouse) < 1.01) {
          mCy += vi->ymouse - mDrawnYmouse;
          mCy = B3DMIN(vi->ysize - 0.5, B3DMAX(0, mCy));
        } else if (vi->xmouse != mDrawnXmouse && vi->ymouse == 
                   mDrawnYmouse && vi->zmouse == mDrawnZmouse &&
                   fabs((double)vi->xmouse - mDrawnXmouse) < 1.01) {
          mCx += vi->xmouse - mDrawnXmouse;
          mCx = B3DMIN(vi->xsize - 0.5, B3DMAX(0, mCx));
        }
      }

      if (imodDebug('s'))
        imodPrintStderr("STEP draw at %f %f %f\n", mCx, mCy, mCz);
      draw();
      return;
    }
  }

  /* DNM 3/11/03: try moving only if not locked */
  if ((drawflag & (IMOD_DRAW_ACTIVE | IMOD_DRAW_IMAGE))){
    if (mPending && !mLocked && mClassic) {
      mCx = mPendx;
      mCy = mPendy;
      mCz = mPendz;
      mPending = 0;
    }
    if (imodDebug('s'))
      imodPrintStderr("ACTIVE draw at %f %f %f\n", mCx, mCy, mCz);
    draw();
    return;
  }
     
  // If we get here and there is a model flag, or still an XYZ flag in new
  // mode, then redraw the filled image
  if ((drawflag & IMOD_DRAW_MOD) || 
      (!mClassic && (drawflag & IMOD_DRAW_XYZ))){
    if (imodDebug('s'))
      imodPrintStderr("MOD draw at %f %f %f\n", mCx, mCy, mCz);
    updateImage();
  }
}


/*
 * Open up slicer help dialog.
 */
void SlicerFuncs::help()
{
  imodShowHelpPage("slicer.html#TOP");
}

float SlicerFuncs::viewAxisStepSize()
{
  return sViewAxisSteps[sViewAxisIndex];
}

/*
 * Toolbar zoom arrow callbacks.
 */
void SlicerFuncs::stepZoom(int dir)
{
  ivwControlPriority(mVi, mCtrl);
  mZoom = b3dStepPixelZoom(mZoom, dir);
  drawSelfAndLinked();
  mQtWindow->setZoomText(mZoom);
}

/* A new zoom value was entered in text box */
void SlicerFuncs::enteredZoom(float newZoom)
{
  if (mClosing)
    return;
  ivwControlPriority(mVi, mCtrl);
  mZoom = newZoom;
  if (mZoom <= 0.01) {
    mZoom = 0.01;
    mQtWindow->setZoomText(mZoom);
  }  
  drawSelfAndLinked();
}

/* A time step through toolbar button or hotkey */
void SlicerFuncs::stepTime(int step)
{
  ivwControlPriority(mVi, mCtrl);
  
  // if time locked, advance the time lock and draw this window
  if (mTimeLock){
    mTimeLock += step;
    if (mTimeLock <= 0)
      mTimeLock = 1;
    if (mTimeLock > ivwGetMaxTime(mVi))
      mTimeLock = ivwGetMaxTime(mVi);
    draw();

  } else {
    if (step > 0)
      inputNextTime(mVi);
    else
      inputPrevTime(mVi);
  }
}
 

/* 
 * Show the location of the slice in the XYZ and Zap windows by doing a general
 * draw after setting slice location.  This will draw this slicer if it is
 * the active or top window.
 */
void SlicerFuncs::showSlice()
{
  transStep();
  imodDraw(mVi, IMOD_DRAW_SLICE | mDrawModView);
  mDrawModView = 0;
}

/* 
 * Fill the cache around the current location
 */
void SlicerFuncs::fillCache()
{
  ivwControlPriority(mVi, mCtrl);
  imodCacheFill(mVi, 1);
}

/*
 * Toolbar Toggle buttons
 */
void SlicerFuncs::stateToggled(int index, int state)
{
  ivwControlPriority(mVi, mCtrl);
  switch (index) {
  case SLICER_TOGGLE_LOCK:

    /* toggle the lock button, redraw if unlocking */
    mLocked = state;
    if (!state) {
      mCx = mVi->xmouse;
      mCy = mVi->ymouse;
      mCz = mVi->zmouse;
      mPending = 0;
      draw();
    }
    break;

  case SLICER_TOGGLE_HIGHRES:

    /* toggle between fast rendering and highres rendering */
    mHq = state;
    draw();
    break;

  case SLICER_TOGGLE_CENTER:
    setClassicMode(state);
    break;

  case SLICER_TOGGLE_SHIFTLOCK:
    mShiftLock = state;
    break;

  case SLICER_TOGGLE_BAND:
    toggleRubberband();
    break;

  case SLICER_TOGGLE_ARROW:
    toggleArrow();
    break;

  case SLICER_TOGGLE_FFT:
    mFftMode = state;
    draw();
    break;

  case SLICER_TOGGLE_ZSCALE:
    mScalez = state;
    draw();
    break;

  case SLICER_TOGGLE_TIMELOCK:
    mTimeLock = state ? mVi->curTime : 0;
    if (!state)
      draw();
    break;
  }
}

/*
 * Toggle the arrow
 */
void SlicerFuncs::toggleArrow(bool drawWin)
{
  Ipoint zero = {0., 0., 0.};
  mArrowOn = !mArrowOn;
  mDrawingArrow = mArrowOn;
  mQtWindow->setToggleState(SLICER_TOGGLE_ARROW, mArrowOn ? 1 : 0);
  if (mArrowOn) {
    mArrowHead.push_back(zero);
    mArrowTail.push_back(zero);
  } else {
    mArrowHead.pop_back();
    mArrowTail.pop_back();
  }
  if (mArrowOn && mStartingBand)
    toggleRubberband(false);

  setCursor(mMousemode, true);
  if (drawWin)
    draw();
  setCursor(mMousemode, true);
}

/*
 * Clear out all arrows
 */
void SlicerFuncs::clearArrows()
{
  if (mClosing)
    return;
  if (mArrowOn)
    toggleArrow(false);
  mArrowHead.resize(0);
  mArrowTail.resize(0);
  draw();
}

/*
 * Start a new arrow, keeping an existing one
 */
void SlicerFuncs::startAddedArrow()
{
  int ind = mArrowHead.size();
  if (mDrawingArrow && !mArrowHead[ind].x && !mArrowHead[ind].y && !mArrowHead[ind].z && 
      !mArrowTail[ind].x && !mArrowTail[ind].y && !mArrowTail[ind].z)
    return;
  mArrowOn = false;
  toggleArrow(false);
}

// Toggle classic mode, set center if going to classic and not locked
void SlicerFuncs::setClassicMode(int state)
{
  mClassic = state;
  if (!mLocked && state) {
    mCx = mVi->xmouse;
    mCy = mVi->ymouse;
    mCz = mVi->zmouse;
  }
  mPending = 0;
  draw();
  synchronizeSlicers();
  
  imodDraw(mVi, IMOD_DRAW_XYZ);
}


/* 
 * Tilt angle controls.
 */
void SlicerFuncs::angleChanged(int axis, int value, int dragging)
{
  Ipoint vec;
  float zmove;

  ivwControlPriority(mVi, mCtrl);
  if (axis < 3) {

    // Normal angle slider
    setForwardMatrix();
    mTang[axis] = value * 0.1f;
    mLastangle = axis;
  } else {

    // view axis position slider
    getNormalToPlane(&vec);
    mCx += (value - mLastAxisPos) * vec.x;
    mCy += (value - mLastAxisPos) * vec.y;
    zmove = (value - mLastAxisPos) * vec.z;
    mCz += zmove;
    if (!mLocked)
      setZmouseForAxisMove(zmove);
  }
  sSliderDragging = dragging;
  changeCenterIfLinked();

  // Do complete redraw if not dragging or hot slider enabled
  if (!dragging || ImodPrefs->hotSliderActive(sHotControlPressed)) {
    mDrawModView = imodvLinkedToSlicer() ? IMOD_DRAW_MOD : 0;
    synchronizeSlicers();
    if (axis == 3 && !mLocked)
      imodDraw(mVi, IMOD_DRAW_XYZ | IMOD_DRAW_SLICE | mDrawModView);
    else
      showSlice();
    checkMovieLimits();
  } else {

    // Otherwise, just draw the cube
    transStep();
    cubeDraw();
  }
}

// Compute and update the view axis position slider setting and maximum
void SlicerFuncs::updateViewAxisPos()
{
  int numSteps[2], idir, ind, i;
  float vec[3], cx, cy, cz;
  getNormalToPlane(vec);

  // This could be done quicker but more messily
  for (ind = 0; ind < 2; ind++) {
    idir = 2 * ind - 1;
    for (i = 1; ;i++) {
      cx = mCx + idir * i * vec[0];
      cy = mCy + idir * i * vec[1];
      cz = mCz + idir * i * vec[2];
      
      // The test here matches translateByRotatedVec.  Note that setting angles from
      // slicer angle dialog clamps at [xyz]size - 1.
      if (cx < 0. || cx >= mVi->xsize || cy < 0. || cy >= mVi->ysize ||
          cz < 0. || cz >= mVi->zsize - 0.5) {
        numSteps[ind] = i - 1;
        break;
      }
    }
  }
  mLastAxisPos = 1 + numSteps[0];
  mQtWindow->setViewAxisPosition(1, mLastAxisPos + numSteps[1], mLastAxisPos);
}

/****************************************************************************/
/* Thickness controls. 
 *
 * Update thickness text widgets; get new values from user
 */
void SlicerFuncs::drawThickControls()
{
  mQtWindow->setModelThickness(mDepth);
  mQtWindow->setImageThickness(mNslice);
}

void SlicerFuncs::imageThickness(int sno)
{
  if (mClosing)
    return;
  ivwControlPriority(mVi, mCtrl);
  if (sno < 1)
    sno = 1;
  mNslice = sno;

  drawThickControls();
  drawSelfAndLinked();
}

void SlicerFuncs::modelThickness(float depth)
{
  if (mClosing)
    return;
  ivwControlPriority(mVi, mCtrl);
  if (fabs(mDepth - 0.1) < 0.01 && fabs(depth - 1.1) < 0.01)
    depth = 1.0;
  mDepth = depth;
  if (mDepth <= 0.0)
    mDepth = 0.1;

  drawThickControls();
  drawSelfAndLinked();
}

// The link button is toggled; if autolinked, manage the toolbar, etc  
void SlicerFuncs::setLinkedState(bool state)
{
  QObjectList objList;
  SlicerWindow *slicer;
  int i, numLeft = 0;

  mLinked = state;

  // If this is the master, find another slave and make it the master, move its toolbar
  // to current position
  if (!state && mAutoLink == 1) {
    imodDialogManager.windowList(&objList, -1, SLICER_WINDOW_TYPE);

    // Count number of linked windows left
    for (i = 0; i < objList.count(); i++) {
      slicer = (SlicerWindow *)objList.at(i);
      if (slicer->mFuncs->mAutoLink > 1)
        numLeft++;
    }

    // Then either make first one the new master, or unlink last one left
    for (i = 0; i < objList.count(); i++) {
      slicer = (SlicerWindow *)objList.at(i);
      if (slicer->mFuncs->mAutoLink > 1) {
        if (numLeft > 1) {
          slicer->manageAutoLink(1);
          slicer->mFuncs->mAutoLink = 1;
          if (slicer->mFreeBar2) {
            if (mQtWindow->mFreeBar2) {
              QRect pos = ivwRestorableGeometry(mQtWindow->mFreeBar2);
              slicer->mFreeBar2->move(pos.left(), pos.top());
            }
            slicer->mFreeBar2->show();
          }
          break;
        } else {
          slicer->manageAutoLink(0);
          slicer->mFuncs->mAutoLink = 0;
          diaSetChecked(slicer->mLinkBox, false);
        }
      }
    }
  }

  // In any case, get the toolbar shown and turn off the autolink of this window
  // Do not update a closing window; the toolbar is not recreated
  if (!state && mAutoLink) {
    mQtWindow->manageAutoLink(0);
    if (!mClosing) {
      drawThickControls();
      mQtWindow->setAngles(mTang);
      updateViewAxisPos();
    }
    mAutoLink = 0;
  }
}

/* The window is closing now.  Clean up, start by unlinking from autolink */
void SlicerFuncs::closing()
{
  mClosing = 1;
  if (mAutoLink > 0)
    setLinkedState(false);
  ivwRemoveControl(mVi, mCtrl);      
  imodDialogManager.remove((QWidget *)mQtWindow);
  b3dFreeCIImage(mImage);
  imodMatDelete(mMat);
}

// Sychronize angles of linked slicers, and positions for ones not locked,
// and draw them if the flag is set.  Return number that were linked
int SlicerFuncs::synchronizeSlicers(bool draw)
{
  QObjectList objList;
  SlicerFuncs *sso;
  int i, need, num = 0;
  if (!mLinked)
    return 0;
  imodDialogManager.windowList(&objList, -1, SLICER_WINDOW_TYPE);
  if (!objList.count())
    return 0;
  for (i = 0; i < objList.count(); i++) {
    sso = ((SlicerWindow *)objList.at(i))->mFuncs;

    // Modify if it is different and linked
    if (sso->mCtrl == mCtrl || !sso->mLinked)
      continue;

    // Keep track if anything changes requiring a draw
    need = 0;
    if (sso->mTang[b3dX] != mTang[b3dX] ||
        sso->mTang[b3dY] != mTang[b3dY] ||
        sso->mTang[b3dZ] != mTang[b3dZ]) {
      sso->mTang[b3dX] = mTang[b3dX];
      sso->mTang[b3dY] = mTang[b3dY];
      sso->mTang[b3dZ] = mTang[b3dZ];
      sso->mQtWindow->setAngles(sso->mTang);
      need = 1;
    }
    if (!mLocked && !sso->mLocked && 
        (sso->mCx != mCx || sso->mCy != mCy || sso->mCz != mCz)) {
      sso->mCx = mCx;
      sso->mCy = mCy;
      sso->mCz = mCz;
      need = 1; 
   }
    if (sso->mNslice != mNslice || sso->mDepth != mDepth) {
      sso->mNslice = mNslice;
      sso->mDepth = mDepth;
      sso->drawThickControls();
      need = 1;
    }
    if (sso->mZoom != mZoom) {
      sso->mZoom = mZoom;
      sso->mQtWindow->setZoomText(sso->mZoom);
      need = 1;
    }
    if (need) {
      sso->mNeedDraw = true;
      if (draw) {
        sso->draw();
        sso->mAlreadyDrew = true;
      }
      num++;
    }
  }
  return num;
}

// Return approximate limits of what could be displayed in the current window based on
// the zoom, center and window size
void SlicerFuncs::getSubsetLimits(int &ixStart, int &iyStart, int &nxUse, int &nyUse)
{
  int xend, yend;
  ixStart = (int)B3DMAX(0, mCx - 0.7 * mWinx / mZoom);
  xend = (int)B3DMIN(mVi->xsize, mCx + 0.7 * mWinx / mZoom);
  nxUse = xend - ixStart;
  iyStart = (int)B3DMAX(0, mCy - 0.7 * mWiny / mZoom);
  yend = (int)B3DMIN(mVi->ysize, mCy + 0.7 * mWiny / mZoom);
  nyUse = yend - iyStart;
}

// Take snapshot with the given name or automatically generated name and given format
int SlicerFuncs::namedSnapshot(QString &fname, int format, bool checkConvert, 
                               bool fullArea)
{
  int *limits = NULL;
  int limarr[4];
  mGlw->updateGL();
  if (!fullArea)
    setSnapshotLimits(&limits, limarr);
  return b3dNamedSnapshot(fname, "slicer", format, limits, checkConvert);
}

// Tell the slicer angle dialog to set current angles into current row, or
// into new row if necessary or if flag is set
void SlicerFuncs::setCurrentOrNewRow(bool newrow)
{
  ivwControlPriority(mVi, mCtrl);
  sSliceAngDia->setCurrentOrNewRow(ivwWindowTime(mVi, mTimeLock), newrow);
}

// Tell the slicer angle dialog to set the current row into the top slicer
void SlicerFuncs::setAnglesFromRow()
{
  ivwControlPriority(mVi, mCtrl);
  sSliceAngDia->setAnglesFromRow(ivwWindowTime(mVi, mTimeLock));
}

/* 
 * Modify the current angles by a rotation about the view axis
 */
void SlicerFuncs::setViewAxisRotation(float x, float y, float z)
{
  double alpha, beta, gamma;
  Imat *rmat = imodMatNew(3);
  Imat *pmat = imodMatNew(3);
  if (!rmat || !pmat)
    return;
  setForwardMatrix();
  imodvResolveRotation(rmat, x, y, z);
  imodMatMult(mMat, rmat, pmat);

  // 11/28/07: this now returns unique angles with |alpha| <= 90.
  imodMatGetNatAngles(pmat, &alpha, &beta, &gamma);

  mTang[b3dX] = alpha;
  mTang[b3dY] = beta;
  mTang[b3dZ] = gamma;
  changeCenterIfLinked();
  free(rmat);
  free(pmat);
}

/*
 * Change center of rotation if linked to model view
 */
void SlicerFuncs::changeCenterIfLinked()
{
  Imod *imod = mVi->imod;
  float zscale = imod->zscale ? imod->zscale : 1.f;
  Imat *omat, *pmat;
  Ipoint pnt, pnew;
  if (!imodvRotCenterLinked() || mLocked || mClassic)
    return;
  omat = imodMatNew(3);
  pmat = imodMatNew(3);
  if (!omat || !pmat)
    return;
  imodMatCopy(mMat, omat);
  setInverseMatrix();
  imodMatMult(omat, mMat, pmat);

  // Add center coordinates here because trans is negative of coordinate
  pnt.x = mCx + imod->view->trans.x;
  pnt.y = mCy + imod->view->trans.y;
  pnt.z = zscale * (mCz + imod->view->trans.z);
  //imodPrintStderr("Old %.1f %.1f %.1f   ", mCx, mCy, mCz);
  imodMatTransform3D(pmat, &pnt, &pnew);
  mCx = pnew.x - imod->view->trans.x;
  mCy = pnew.y - imod->view->trans.y;
  mCz = pnew.z / zscale - imod->view->trans.z;
  mCx = B3DMAX(0., B3DMIN(mCx, mVi->xsize - 1));
  mCy = B3DMAX(0., B3DMIN(mCy, mVi->ysize - 1));
  mCz = B3DMAX(0., B3DMIN(mCz, mVi->zsize - 1));
  mVi->zmouse = mCz;
  //imodPrintStderr("New %.1f %.1f %.1f\n", mCx, mCy, mCz);

  free(omat);
  free(pmat);
}


// Key press input
void SlicerFuncs::keyInput(QKeyEvent *event)
{
  int keysym = event->key();
  int shift = event->modifiers() & Qt::ShiftModifier;
  int ctrl = event->modifiers() & Qt::ControlModifier;
  int lang = mLastangle;
  int dodraw = 1;
  int handled = 1;
  int docheck = 0;
  int keypad = event->modifiers() & Qt::KeypadModifier;
  int *limits;
  int limarr[4];
  float unit, dist3d;
  QString str;
  Ipoint *p1, *p2, *pnt;
  int ob, co, pt, axis, start, end, ix, iy;
  Icont *cont;
  Ipoint scale, moupt, norm, vec = {0., 0., 0.};
  ImodView *vi = mVi;

  if (inputTestMetaKey(event))
    return;

  if (utilCloseKey(event)) {
    mQtWindow->close();
    return;
  }

  inputConvertNumLock(keysym, keypad);

  ivwControlPriority(vi, mCtrl);

  if (imodPlugHandleKey(vi, event)) 
    return;

  // These grabs may not work right if keys are passed from elsewhere
  if (keysym == hotSliderKey() || keysym == Qt::Key_Shift) {
    sHotControlPressed = keysym == hotSliderKey() ? 1 : 0;
    sShiftPressed = keysym == Qt::Key_Shift ? 1 : 0;
    mQtWindow->grabKeyboard();
    return;
  }

  switch(keysym){
          
  case Qt::Key_Equal:
  case Qt::Key_Plus:
    if (keypad || keysym == Qt::Key_Equal) {
      mZoom = b3dStepPixelZoom(mZoom, 1);
      mQtWindow->setZoomText(mZoom);
    } else {
      mNslice++;
      drawThickControls();
    }
    break;

  case Qt::Key_Underscore:
    mNslice--;
    if (mNslice < 1) 
      mNslice = 1;
    drawThickControls();
    break;

  case Qt::Key_Minus:
    mZoom = b3dStepPixelZoom(mZoom, -1);
    mQtWindow->setZoomText(mZoom);
    break;

  case Qt::Key_9:
    mDepth -= 1.0;
    if (mDepth < 1.0)
      mDepth  = 1.0;
    drawThickControls();
    break;

  case Qt::Key_0:
    mDepth += 1.0;
    drawThickControls();
    break;
          
  case Qt::Key_1:
  case Qt::Key_2:
    stepTime((keysym == Qt::Key_1) ? -1 : 1);
    break;

  case Qt::Key_At:
  case Qt::Key_Exclam:
    if (mTimeLock) {
      imcGetStartEnd(vi, 3, &start, &end);
      mTimeLock = (keysym == Qt::Key_At) ? end + 1 : start + 1;
    } else
      handled = 0;
    break;

  case Qt::Key_Q:
    ix = (mGlw->mapFromGlobal(QCursor::pos())).x();
    iy = (mGlw->mapFromGlobal(QCursor::pos())).y();
    getxyz(ix, iy, moupt.x, moupt.y, moupt.z);
    pnt = imodPointGet(vi->imod);
    norm.x = vi->xmouse;
    norm.y = vi->ymouse;
    norm.z = vi->zmouse;
    if (!pnt || mMousemode == IMOD_MMOVIE)
      pnt = &norm;
    scale.x = vi->imod->xscale * vi->xybin;
    scale.y = vi->imod->yscale * vi->xybin;
    scale.z = vi->imod->zscale * vi->zbin;
    dist3d  = imodPoint3DScaleDistance(pnt, &moupt, &scale);
    str.sprintf("From (%.1f, %.1f, %.1f) to (%.1f, %.1f, %.1f) = %.1f %spixels", 
                pnt->x+1., pnt->y+1., pnt->z+1., moupt.x+1., moupt.y+1., moupt.z+1., 
                dist3d, vi->zbin * vi->xybin > 1 ? "unbinned " : "");
    utilWprintMeasure(str, vi->imod, dist3d);

    break;


  case Qt::Key_S:
    if (shift || ctrl){
      if (imcGetSlicerMontage(true)) {
        montageSnapshot((ctrl ? 1 : 0) + (shift ? 2 : 0));
      } else {

        // Snapshots: need to update just the image window
        mGlw->updateGL();
        setSnapshotLimits(&limits, limarr);
        b3dKeySnapshot("slicer", shift, ctrl, limits);
      }
    }else
      inputSaveModel(vi);
    dodraw = 0;
    break;

  case Qt::Key_L:
    showSlice();
    dodraw = 0;
    break;

  case Qt::Key_K:
    setClassicMode(mClassic ? 0 : 1);
    mQtWindow->setToggleState(SLICER_TOGGLE_CENTER, mClassic);
    break;

  case Qt::Key_X:
  case Qt::Key_Y:
  case Qt::Key_Z:
    if (ctrl && (keysym == Qt::Key_Z || keysym == Qt::Key_Y)) {
      handled = 0;
    } else {
      cont = imodContourGet(vi->imod);
      imodGetIndex(vi->imod, &ob, &co, &pt);
      
      p2 = imodPointGet(vi->imod);
      if ((cont) && (p2) && (cont->psize > 1)){
        if (pt)
          p1 = &cont->pts[pt-1];
        else
          p1 = &cont->pts[pt+1];
        axis = b3dX;
        if (keysym == Qt::Key_Y)
          axis = b3dY;
        if (keysym == Qt::Key_Z)
          axis = b3dZ;
        if (shift){
          p2 = &cont->pts[cont->psize - 1];
          p1 = &cont->pts[0];
        }
        setAnglesFromPoints(p1, p2, axis);
        docheck = 1;
      } else
        dodraw = 0;
    }
    break;

  case Qt::Key_W:
    if (shift) {
      if (!anglesFromContour())
        docheck = 1;
      dodraw = 0;
    } else
      handled = 0;

    
  case Qt::Key_Comma:
  case Qt::Key_Less:
    dodraw = 0;
    if (keysym == Qt::Key_Comma && !mShiftLock)
      handled = 0;
    else
      slicerViewAxisStepChange(-1);
    break;

  case Qt::Key_Period:
  case Qt::Key_Greater:
    dodraw = 0;
    if (keysym == Qt::Key_Period && !mShiftLock)
      handled = 0;
    else
      slicerViewAxisStepChange(1);
    break;

    /* DNM: add these to adjust last angle, now that input is properly
       passed to the window */
  case Qt::Key_Up:
  case Qt::Key_Down:
  case Qt::Key_Right:
  case Qt::Key_Left:
  case Qt::Key_Insert:
  case Qt::Key_End:
  case Qt::Key_PageDown:
  case Qt::Key_PageUp:
    dodraw = 0;
    if ((keypad && !(shift || mShiftLock) && keysym == Qt::Key_PageUp) || 
        (keypad && (shift || mShiftLock) && keysym == Qt::Key_Insert) ||
        (!keypad && ((!mLocked && mClassic) || keysym == Qt::Key_End || 
                     keysym == Qt::Key_Insert || 
                     (shift && (keysym == Qt::Key_PageUp || 
                                keysym == Qt::Key_PageDown))))) {
      handled = 0;
      break;
    }

    // Non-keypad keys, only if locked or new mode, move without changing 
    // global point
    // and move in tilted coordinates so that modeling will be one pixel apart
    if (!keypad) {
      unit = mClassic ? -1. : +1.;
      if (keysym == Qt::Key_Down) 
        vec.y = unit;
      if (keysym == Qt::Key_Left)
        vec.x = unit;
      if (keysym == Qt::Key_Right)
        vec.x = -unit;
      if (keysym == Qt::Key_Up)
        vec.y = -unit;
      if (keysym == Qt::Key_PageUp)
        vec.z = 1.;
      if (keysym == Qt::Key_PageDown)
        vec.z = -1.;
      if (translateByRotatedVec(&vec)) {
        dodraw = 1;
        if (!mLocked && vec.z) {
          setZmouseForAxisMove(vec.z);
          synchronizeSlicers();
          imodDraw(vi, IMOD_DRAW_XYZ | IMOD_DRAW_SLICE);
          dodraw = 0;        
        }
      }
      break;
    }

    // Now handle keypad keys
    if (shift || mShiftLock) {
      unit = sViewAxisSteps[sViewAxisIndex];
      if (keysym == Qt::Key_Down) 
        setViewAxisRotation(-unit, 0., 0.);
      if (keysym == Qt::Key_Left)
        setViewAxisRotation(0., -unit, 0.);
      if (keysym == Qt::Key_Right)
        setViewAxisRotation(0., unit, 0.);
      if (keysym == Qt::Key_Up)
        setViewAxisRotation(unit, 0., 0.);
      if (keysym == Qt::Key_PageUp)
        setViewAxisRotation(0., 0., unit);
      if (keysym == Qt::Key_PageDown)
        setViewAxisRotation(0., 0., -unit);
    } else {
      setForwardMatrix();
      if (keysym == Qt::Key_Down) 
        mTang[lang] -= 0.5;
      if (keysym == Qt::Key_Left)
        mTang[lang] -= 0.1;
      if (keysym == Qt::Key_Right)
        mTang[lang] += 0.1;
      if (keysym == Qt::Key_Up)
        mTang[lang] += 0.5;
      if (keysym == Qt::Key_End)
        mTang[lang] -= 15.0;
      if (keysym == Qt::Key_PageDown)
        mTang[lang] += 15.0;
      if (keysym == Qt::Key_Insert)
        mTang[lang] = 0.0;

      B3DCLAMP(mTang[lang], -sMaxAngle[lang], sMaxAngle[lang]);
      changeCenterIfLinked();
    }

    mQtWindow->setAngles(mTang);
    mDrawModView = imodvLinkedToSlicer() ? IMOD_DRAW_MOD : 0;

    synchronizeSlicers();
    showSlice();
    docheck = 1;
    break;

  case Qt::Key_B:
    if (shift)
      toggleRubberband();
    else
      handled = 0;

  case Qt::Key_R:
    if (shift) {
      if (ctrl && mRubberband)
        resizeBandToWindow();
      else if (mRubberband)
        resizeToFit();
      dodraw = 0;
    } else
      handled = 0;

  default:
    handled = 0;
    break;
  }

  // If key not handled, call the default processor
  if (!handled) {
    inputQDefaultKeys(event, vi);
    return;
  }

  // If draw still needed, do it; then check movie limits if needed too
  if (dodraw)
    drawSelfAndLinked();
  if (docheck)
    checkMovieLimits();
}

// On any key release, clear the ctrl pressed flag and release the keyboard
void SlicerFuncs::keyRelease(QKeyEvent *event)
{
  bool needRelease = sHotControlPressed || sShiftPressed;
  if (event->key() == hotSliderKey())
    sHotControlPressed = 0;
  if (event->key() == Qt::Key_Shift)
    sShiftPressed = 0;
  if (needRelease && !sHotControlPressed && !sShiftPressed)
    mQtWindow->releaseKeyboard();
}

/*
 * When doing axis moves that affect the global Z position, keep track of cumulative
 * moves and adjust global Z mouse to an integer consistent with the precise Z
 */
void SlicerFuncs::setZmouseForAxisMove(float zmove)
{
  Ipoint norm;

  // For a move in Z, try to move current image point normal to the
  // plane.  First test if this move is a continuation of a sequence
  // and if so just increment the cumulative move; otherwise set this
  // up as the first move of a potential sequence
  if (mTang[0] == mOrigAngles[0] && 
      mTang[1] == mOrigAngles[1] && 
      mTang[2] == mOrigAngles[2] && 
      mVi->xmouse == mLastXmouse && 
      mVi->ymouse == mLastYmouse && 
      mVi->zmouse == mLastZmouse) {
    mCumPageMoves += zmove;
  } else {
    mOrigAngles[0] = mTang[0];
    mOrigAngles[1] = mTang[1];
    mOrigAngles[2] = mTang[2];
    mOrigZmouse = mVi->zmouse;
    mCumPageMoves = zmove;
  }

  getNormalToPlane(&norm);

  // Now move the zmouse by cumulative move from original position
  // and round z to integer (decided not to move in X/Y)
  mVi->zmouse = B3DNINT(mOrigZmouse + mCumPageMoves * norm.z);
  mLastXmouse = mVi->xmouse;
  mLastYmouse = mVi->ymouse;
  mLastZmouse = mVi->zmouse;
  ivwBindMouse(mVi);
}

// Do all the tasks for rotating on the view axis with the three delta values
void SlicerFuncs::rotateOnViewAxis(int deltaX, int deltaY, int deltaZ)
{
  float unit = sViewAxisSteps[sViewAxisIndex];
  setViewAxisRotation(deltaX * unit, deltaY * unit, deltaZ * unit);
  mQtWindow->setAngles(mTang);
  mDrawModView = imodvLinkedToSlicer() ? IMOD_DRAW_MOD : 0;
  synchronizeSlicers();
  showSlice();
  drawSelfAndLinked();
  checkMovieLimits();
}

// Process press of mouse buttons
void SlicerFuncs::mousePress(QMouseEvent *event)
{
  int shift = (event->modifiers() & Qt::ShiftModifier) + mShiftLock;
  int ctrl = (event->modifiers() & Qt::ControlModifier);
  int button1 = event->buttons() & ImodPrefs->actualButton(1) ? 1 : 0;
  int button2 = event->buttons() & ImodPrefs->actualButton(2) ? 1 : 0;
  int button3 = event->buttons() & ImodPrefs->actualButton(3) ? 1 : 0;
  sMousePressed = true;
  ivwControlPriority(mVi, mCtrl);

  utilRaiseIfNeeded(mQtWindow, event);
  setCursor(mMousemode, utilNeedToSetCursor());

  sLastMouseX = sFirstMouseX = event->x();
  sLastMouseY = sFirstMouseY = event->y();

  // First see if it is this is a hit to move the band
  if (event->button() == ImodPrefs->actualButton(2) && !button1 && !button3) {
    mMoveBand = 0;
    if (mRubberband) {
      bandImageToMouse();
      if (utilTestBandMove(sLastMouseX, sLastMouseY, mRbMouseX0, mRbMouseX1, mRbMouseY0,
                           mRbMouseY1)) {
        mMoveBand = 1;
        setCursor(mMousemode);
        setBandMouseStart();
        return;
      }
    }
  }

  if (button1) {
    if (mClassic || mStartingBand || mDrawingArrow) {
      attachPoint(event->x(), event->y(), ctrl);
    } else {
      sBut1downt.start();
      mFirstDrag = 1;
    } 
  }

  if (button2 && (ctrl || !shift))
    insertPoint(event->x(), event->y(), ctrl);
  
  if (button3 && (ctrl || !shift))
    modifyPoint(event->x(), event->y(), ctrl);
}

// Respond to mouse button up
void SlicerFuncs::mouseRelease(QMouseEvent *event)
{
  sMousePressed = false;
  ivwControlPriority(mVi, mCtrl);

  // For button 1 up, if not classic mode, either end panning or call attach
  if (event->button() == ImodPrefs->actualButton(1) && 
      (!mClassic || mDrawingArrow || mDragBand)) {
    if (mDragBand || mDrawingArrow) {
      mDragBand = 0;
      mDrawingArrow = false;
      setCursor(mMousemode);
    } else if (sMousePanning) {
      sMousePanning = 0;
      drawSelfAndLinked();
    } else
      attachPoint(event->x(), event->y(), event->modifiers() & Qt::ControlModifier);
    mFirstDrag = 0;
  }
  if (event->button() == ImodPrefs->actualButton(2) && mMoveBand) {
    mMoveBand = 0;
    setCursor(mMousemode);
  }
  if (sMouseRotating) {
    sMouseRotating = 0;
    drawSelfAndLinked();
  }
}

// Process a mouse move
void SlicerFuncs::mouseMove(QMouseEvent *event)
{
  static int button1, button2, button3, ex, ey, shift, processing = 0;
  int zmouse, ind;
  float xm, ym, zm, current, angleScale, trueLimits[2], dx = 0., dy = 0., drot = 0.;
  float axisStart, axisEnd;
  Ipoint cpt;
  Icont *cont;
  Imod *imod = mVi->imod;
  Ipoint scale = {1., 1., 1.};
  int cumdx, cumdy;
  int cumthresh = 6 * 6;
  bool addingPoints, checkNewPos;
  double transFac = mZoom < 4. ? 1. / mZoom : 0.25;
  int bandmin = b3dIMin(3, 4, (int)(mVi->xsize * mXzoom) + 2, 
                        (int)(mVi->ysize * mYzoom) + 2);
  Ipoint vec;
 
  // Record state of event and return if processing
  shift = (event->modifiers() & Qt::ShiftModifier) + mShiftLock;
  button1 = event->buttons() & ImodPrefs->actualButton(1);
  button2 = event->buttons() & ImodPrefs->actualButton(2);
  button3 = event->buttons() & ImodPrefs->actualButton(3);
  ex = event->x();
  ey = event->y();

  if (processing) {
    processing++;
    return;
  }

  if (sPixelViewOpen) {
    zmouse = getxyz(ex, ey, xm, ym, zm);
    pvNewMousePosition(mVi, xm, ym, zmouse);
  }

  // For anything but modeling points, eat any pending move events and use
  // latest position
  addingPoints = button2 && !shift && !mMoveBand &&
    (mLocked || !mClassic) && imod->mousemode == IMOD_MMODEL;
  if (!addingPoints) {
    processing = 1;
    imod_info_input();
    if (imodDebug('m') && processing > 1)
      imodPrintStderr("Flushed %d move events\n", processing - 1);
    processing = 0;
    
    // If this processed a release, then turn off the buttons
    if (!sMousePressed)
      button1 = button2 = button3 = 0;
  }

  // If mouse not pressed and rubber band is on, analyze for setting mouse for move/drag
  // Or, first time mouse moves on button 1, lock in the band drag position
  if (mRubberband && (!sMousePressed || (button1 && mFirstDrag))) {
    bandImageToMouse();
    utilAnalyzeBandEdge(ex, ey, mRbMouseX0, mRbMouseX1, mRbMouseY0, mRbMouseY1,
                        mDragBand, mDragging);
    setCursor(mMousemode);
    if (!sMousePressed)
      return;
    mFirstDrag = 0;
    setBandMouseStart();
  }

  ivwControlPriority(mVi, mCtrl);
  cumdx = ex - sFirstMouseX;
  cumdy = ey - sFirstMouseY;

  // Button 1 
  if (button1) {

    // when drawing arrow: update the arrowhead position
    if (mDrawingArrow) {
      ind = mArrowHead.size() - 1;
      getxyz(ex, ey, mArrowHead[ind].x, mArrowHead[ind].y, mArrowHead[ind].z);
      draw();

      // Button one when starting band, see if mouse has moved far enough to commit to
      // a direction and start dragging
    } else if (mStartingBand) {
      if (!utilIsBandCommitted(ex, ey, mWinx, mWiny, bandmin, mRbMouseX0, mRbMouseX1, 
                               mRbMouseY0, mRbMouseY1, mDragging))
        return;

      // Set flags for the band being on and being dragged
      setBandMouseStart();
      bandMouseToImage();
      mStartingBand = 0;
      mRubberband = 1;
      mGlw->setMouseTracking(true);
      mDragBand = 1;
      mBandChanged = 1;
      setCursor(mMousemode);
      draw();
      return;

      // Dragging the band edge or corner
    } else if (mRubberband && mDragBand) {
      checkNewPos = !findBandAxisRange(current, axisStart, axisEnd);
      saveBandForChecking();
      bandImageToMouse();
      if (mDragging[0]) {
        mRbMouseX0 = mRbStartX0 + cumdx;
        mRbMouseX0 = B3DMIN(mRbMouseX0, mRbMouseX1 - 2);
      }
      if (mDragging[1]) {
        mRbMouseX1 = mRbStartX1 + cumdx;
        mRbMouseX1 = B3DMAX(mRbMouseX1, mRbMouseX0 + 2);
      }
      if (mDragging[2]) {
        mRbMouseY0 = mRbStartY0 + cumdy;
        mRbMouseY0 = B3DMIN(mRbMouseY0, mRbMouseY1 - 2);
      }
      if (mDragging[3]) {
        mRbMouseY1 = mRbStartY1 + cumdy;
        mRbMouseY1 = B3DMAX(mRbMouseY1, mRbMouseY0 + 2);
      }
      bandMouseToImage();
      if (checkNewPos && findBandAxisRange(current, axisStart, axisEnd)) {
        restoreSavedBand();
        bandImageToMouse();
      } else {
        mBandChanged = 1;
        checkBandLowHighLimits(trueLimits);
      }
      draw();

      // Pan with button 1 if not classic mode
    } else if (!mClassic) {
      if (sMousePanning || sBut1downt.elapsed() > 250 || 
          cumdx * cumdx + cumdy * cumdy > cumthresh) {
        vec.x = (sLastMouseX - ex) * transFac;
        vec.y = (ey - sLastMouseY) * transFac;
        vec.z = 0.;
        sMousePanning = 1;
        if (translateByRotatedVec(&vec))
          drawSelfAndLinked();
      }
    }
  }

  // Button 2 in model mode, locked or new mode, add points
  if (addingPoints) {
    zmouse = setxyz(ex, ey);
    cpt.x = mVi->xmouse;
    cpt.y = mVi->ymouse;
    cpt.z = mVi->zmouse;
    cont = imodContourGet(imod);
    if (!cont || imod->cindex.point < 0)
      return;
    if (imodPoint3DScaleDistance
        (&(cont->pts[imod->cindex.point]), &cpt, &scale) > 
        scaleModelRes(imod->res, mZoom)) {
      mIgnoreCurPtChg = 1;
      inputInsertPoint(mVi);
      mVi->zmouse = zmouse;
    }
  }

  // Moving the band
  if (button2 && mMoveBand) {
    checkNewPos = !findBandAxisRange(current, axisStart, axisEnd);
    saveBandForChecking();
    bandImageToMouse();
    mRbMouseX0 = mRbStartX0 + cumdx;
    mRbMouseX1 = mRbStartX1 + cumdx;
    mRbMouseY0 = mRbStartY0 + cumdy;
    mRbMouseY1 = mRbStartY1 + cumdy;
    bandMouseToImage();
    if (checkNewPos && findBandAxisRange(current, axisStart, axisEnd)) {
      restoreSavedBand();
      bandImageToMouse();
    } else {
      mBandChanged = 1;
      checkBandLowHighLimits(trueLimits);
    }
    draw();
    
  } else if (shift && (button2 || button3)) {

  // Button 2 shifted, rotate around view axis in X/Y plane
    if (button2) {
      angleScale = 180. / (3.142 * 0.4 * B3DMIN(mWinx, mWiny));
      dy = (ex - sLastMouseX) * angleScale;
      dx = (ey - sLastMouseY) * angleScale;
    } else {

      // Button 3 shifted, rotate around Z view axis
      drot = utilMouseZaxisRotation(mWinx, ex, sLastMouseX,
                                    mWiny, ey, sLastMouseY);

    }
    if (dx <= -0.1 || dx >= 0.1 || dy <= -0.1 || dy >= 0.1 || 
        fabs(drot) >= 0.1) {
      setViewAxisRotation(dx, dy, drot);
      mQtWindow->setAngles(mTang);
      sMouseRotating = 1;
      if (imodDebug('s'))
        imodPuts("Mouse rotating");
      mDrawModView = imodvLinkedToSlicer() ? IMOD_DRAW_MOD : 0;
      synchronizeSlicers();
      showSlice();
    }
  }
  sLastMouseX = ex;
  sLastMouseY = ey;
}

// Process first mouse button - attach in model, set current point in movie
void SlicerFuncs::attachPoint(int x, int y, int ctrlDown)
{
  Ipoint pnt, norm;
  float delta;
  float distance;
  ImodView *vi = mVi;
  Imod *imod = vi->imod;
  Iindex index, indSave;
  float selsize = IMOD_SELSIZE / mZoom;
  int ind, drawflag = IMOD_DRAW_XYZ;

  vi->zmouse = setxyz(x, y);

  // Start the arrow; record tail position and angles
  if (mDrawingArrow) {
    ind = mArrowHead.size() - 1;
    getxyz(x, y, mArrowTail[ind].x, mArrowTail[ind].y, mArrowTail[ind].z);
    mArrowHead[ind] = mArrowTail[ind]; 
   for (ind = 0; ind < 3; ind++)
      mArrowAngle[ind] = mTang[ind];

    // If starting band, record position and angles
  } else if (mStartingBand) {
    mRbMouseX0 = x;
    mRbMouseY0 = y;
    for (ind = 0; ind < 3; ind++)
      mBandAngle[ind] = mTang[ind];
    
  } else if (imod->mousemode == IMOD_MMODEL) {
    pnt.x = vi->xmouse;
    pnt.y = vi->ymouse;
    pnt.z = vi->zmouse;
    setForwardMatrix();
    indSave = vi->imod->cindex;

    distance = imodAllObjNearest(vi, &index , &pnt, selsize, ivwWindowTime(vi, mTimeLock),
                                 mMat);
    if (distance >= 0.)
      imodSelectionNewCurPoint(vi, imod, indSave, ctrlDown);

    if (imod->cindex.point >= 0 && !mClassic) {
      
      // Move along direction of normal to plane until reaching a plane that
      // contains the current point; snap to curpt if this is out of bounds
      getNormalToPlane(&norm);
      delta = norm.x * (vi->xmouse - mCx) + 
        norm.y * (vi->ymouse - mCy) + norm.z * (vi->zmouse - mCz);
      if (imodDebug('s'))
        imodPrintStderr("norm %.4f %.4f %.4f  zm %f cz %f delta %.3f\n",
                        norm.x, norm.y, norm.z, vi->zmouse, mCz, delta);
      
      // Move to a new plane unconditionally so that modeling can be in
      // same plane
      mCx += delta * norm.x;
      mCy += delta * norm.y;
      mCz += delta * norm.z;
      
      // If this goes out of bounds, just snap to current point
      if (mCx < 0 || mCx >= mVi->xsize || mCy < 0 || 
          mCy >= mVi->ysize || mCz < 0 || 
          mCz >= mVi->zsize - 0.5) {
        mCx = mVi->xmouse;
        mCy = mVi->ymouse;
        mCz = mVi->zmouse;
      }
    }
    /* DNM 5/5/03: take out IMAGE flag, use RETHINK only if in model mode */
    drawflag |= IMOD_DRAW_RETHINK;
  }

  /* DNM: for select hits, do keep cz at an integral value */
  mPending = 0;
  if (!mClassic)
    mIgnoreCurPtChg = 1;
  imodDraw(mVi, drawflag);
}

// Second button, insert a point
void SlicerFuncs::insertPoint(int x, int y, int ctrl)
{
  int zmouse, pt;
  Icont *cont;
  Ipoint zvec = {0., 0., 1.};
  Ipoint zn;
  Imat *mat = mMat;
  ImodView *vi = mVi;
  float dist, mindist, maxdist, dplane, distsum;
  float flatCrit = 1.5f;
  float zcrit = 0.75f;
  bool newsurf, timeMismatch, notInPlane;
  Iobj *obj = imodObjectGet(vi->imod);
  if (vi->imod->mousemode == IMOD_MMODEL) {

    // Get the mouse position, get the contour
    zmouse = setxyz(x, y);
    cont = ivwGetOrMakeContour(vi, obj, mTimeLock);
    if (!cont)
      return;
    if (cont->psize > 0 && iobjPlanar(obj->flags)) {

      // For existing contour, get plane equation of current plane and
      // compute distance of each point from plane
      setInverseMatrix();
      imodMatTransform3D(mat, &zvec, &zn);
      dplane = vi->xmouse * zn.x + vi->ymouse * zn.y + vi->zmouse * zn.z;
      distsum = 0.;
      mindist = 1.e10;
      maxdist = -1.e10;
      for (pt = 0; pt < cont->psize; pt++) {
        dist = imodPointDot(&cont->pts[pt], &zn) - dplane;
        mindist = B3DMIN(dist, mindist);
        maxdist = B3DMAX(dist, maxdist);
        distsum += dist;
      }
      if (imodDebug('s'))
        imodPrintStderr("dplane %f  mindist  %f  maxdist %f  mean %f\n",
                        dplane, mindist, maxdist, distsum / cont->psize);
 
     // If the contour is not flat to current plane, start a new surface
      // If it is just too far away, start a new contour

      newsurf = maxdist - mindist > flatCrit && ImodPrefs->slicerNewSurf();
      timeMismatch = ivwTimeMismatch(vi, mTimeLock, obj, cont);
      notInPlane = fabs(distsum / cont->psize) > zcrit;
      imodTrace('s', "newsurf %d timeMismatch %d notInPlane %d", newsurf, timeMismatch, 
                notInPlane);
      if (newsurf || notInPlane || timeMismatch) {
        cont = utilAutoNewContour(vi, cont, notInPlane, timeMismatch, mTimeLock,
                                  newsurf ? INCOS_NEW_SURF : INCOS_NEW_CONT,
                                  "different slices", "angles");
        if (!cont)
          return;
      }
    }
    if (!mClassic)
      mIgnoreCurPtChg = 1;

    inputInsertPoint(vi);
    vi->zmouse = zmouse;
  } else if (ctrl)
    startMovieCheckSnap(1);
  else
    wprint("\aUse "CTRL_STRING"-middle button to start movie\n");
}

// Third button: modify a point or start/stop movie
void SlicerFuncs::modifyPoint(int x, int y, int ctrl)
{
  int zmouse;
  if (mVi->imod->mousemode == IMOD_MMODEL) {
    zmouse = setxyz(x, y);
    if (!mClassic)
      mIgnoreCurPtChg = 1;
    inputModifyPoint(mVi);
    mVi->zmouse = zmouse;
  } else if (ctrl)
    startMovieCheckSnap(-1);
}

// Sets the cursor for the given mode if necessary, or regardless if setAnyway is true
void SlicerFuncs::setCursor(int mode, bool setAnyway)
{
  bool needSpecial = (mRubberband && (mMoveBand || mDragBand)) || mStartingBand ||
    mDrawingArrow;
  bool needSizeAll = mStartingBand || mMoveBand || mDrawingArrow;
  bool needModel = false;
  //imodPrintStderr("sa %d special %d sizeall %d sb %d mb %d\n", setAnyway?1:0, needSpecial?1:0, needSizeAll?1:0, mStartingBand?1:0, mMoveBand?1:0);
  utilSetCursor(mode, setAnyway, needSpecial, needSizeAll, mDragging, needModel, 
                mMousemode, mLastShape, mGlw);
}

/*
 * MOVIE RELATED ROUTINES
 */

// Find dominant axis of this slicer window and set movie arguments to match
void SlicerFuncs::findMovieAxis(int &xmovie, int &ymovie, int &zmovie, int dir,
                                int &axis)
{
  Ipoint vec;
  xmovie = ymovie = zmovie = 0;
  getNormalToPlane(&vec);
  double xcomp = fabs((double)vec.x);
  double ycomp = fabs((double)vec.y);
  double zcomp = fabs((double)vec.z);

  if (xcomp >= ycomp && xcomp >= zcomp) {
    xmovie = dir;
    axis = 0;
  } else if (ycomp >= zcomp) {
    ymovie = dir;
    axis = 1;
  } else {
    zmovie = dir;
    axis = 2;
  }
}

/*
 * Returns the dominant axis, the normal to plane, and the absolute value of the normal
 * component on the main axis
 */
void SlicerFuncs::getNormalAndMainComponent(int &axis, Ipoint *norm, float &axisComp)
{
  int xmovie, ymovie, zmovie;
  findMovieAxis(xmovie, ymovie, zmovie, 1, axis);
  getNormalToPlane(norm);
  axisComp = xmovie * norm->x + ymovie * norm->y + zmovie * norm->z;
}

// Find the limits on the dominant axis that will keep the other two axis positions 
// within bounds; also returns current position on that axis in "current"
void SlicerFuncs::findAxisLimits(int axis, float &current, int &start, int &end)
{
  int i, size;
  float cx, cy, cz;
  float zStep[3];
  getNormalToPlane(zStep);
  start = -1;

  // Get the size and current value for the dominant axis
  if (axis == 0) {
    size = mVi->xsize;
    current = mCx;
  } else if (axis == 1) {
    size = mVi->ysize;
    current = mCy;
  } else {
    size = mVi->zsize;
    current = mCz;
  }

  // Loop on positions on dominant axis and find position on each axis
  for (i = 0; i < size; i++) {
    cx = mCx + (i - current) * zStep[0] / zStep[axis];
    cy = mCy + (i - current) * zStep[1] / zStep[axis];
    cz = mCz + (i - current) * zStep[2] / zStep[axis];
    if (cx >= 0. && cx <= mVi->xsize - 1 && 
        cy >= 0. && cy <= mVi->ysize - 1 &&
        cz >= 0. && cz <= mVi->zsize - 1) {

      // Set start on first legal position and end on any legal one
      end = i;
      if (start < 0)
        start = i;
    }
  }
}

// Set special movie limits on the dominant axis that will keep the other two
// axes within bounds
void SlicerFuncs::setMovieLimits(int axis)
{
  int end, start;
  float cur;
  findAxisLimits(axis, cur, start, end);
  imcSetSpecialLimits(axis, start, end);
}

// Check for movie and set new limits when angles have changed
void SlicerFuncs::checkMovieLimits()
{
  int xmovie, ymovie, zmovie;
  int axis;
  ImodView *vi = mVi;
  float zStep[3];
  getNormalToPlane(zStep);

  // If no movie or it was not started by this slicer, done
  if (!(vi->xmovie || vi->ymovie || vi->zmovie) || 
      imcGetStarterID() != mCtrl)
    return;

  // See if dominant axis has changed and restart movie
  findMovieAxis(xmovie, ymovie, zmovie, 1, axis);
  if ((vi->xmovie && axis != 0) || (vi->ymovie && axis != 1) || 
      (vi->zmovie && axis != 2)) {

    // If sign of new motion along old axis (xstep[old]/xstep[axis]) is 
    // opposite to sign of old motion (oldmovie), then need to restart
    // with negative direction.  Only one term of old sum counts.
    if ((vi->xmovie * zStep[b3dX] + vi->ymovie * zStep[b3dY] + 
         vi->zmovie * zStep[b3dZ]) / zStep[axis] < 0.)
      findMovieAxis(xmovie, ymovie, zmovie, -1, axis);
    imodMovieXYZT(vi, xmovie, ymovie, zmovie, 0);
  }

  // In any case, set new movie limits
  setMovieLimits(axis);
}

// Start a movie and set up for autosnapshotting
void SlicerFuncs::startMovieCheckSnap(int dir)
{
  int xmovie, ymovie, zmovie;
  int axis, start, end;
  ImodView *vi = mVi;

  // Find dominant axis and start/stop movie along that axis
  findMovieAxis(xmovie, ymovie, zmovie, dir, axis);
  imodMovieXYZT(vi, xmovie, ymovie, zmovie, 0);
  imcSetStarterID(mCtrl);

  // Just zero the snap count
  mMovieSnapCount = 0;
  b3dSetMovieSnapping(false);

  /* done if no movie, otherwise set the movie limits */
  if (!(vi->zmovie || vi->xmovie || vi->ymovie)) 
    return;

  // move the current point to center point
  vi->xmouse = mCx;
  vi->ymouse = mCy;
  vi->zmouse = mCz;
  setMovieLimits(axis);

  // now done if no snapshots are desired.
  if (!imcGetSnapshot(vi))
    return;
     
  /* Get start and end of loop, compute count */
  imcGetStartEnd(vi, axis, &start, &end);
  mMovieSnapCount = (end - start) / imcGetIncrement(vi, axis) + 1;
  if (mMovieSnapCount < 1)
    mMovieSnapCount = 1;

  /* double count for normal mode, leave as is for one-way */
  if (!imcGetLoopMode(vi))
    mMovieSnapCount *= 2;

  /* Set to start or end depending on which button was hit */
  if (!imcStartSnapHere(vi)) {
    if (axis == 0)
      vi->xmouse = dir > 0 ? start : end;
    else if (axis == 1)
      vi->ymouse = dir > 0 ? start : end;
    else
      vi->zmouse = dir > 0 ? start : end;
  }
  b3dSetMovieSnapping(true);

  /* draw - via imodDraw to get float and positioning done correctly */
  imodDraw(vi, IMOD_DRAW_XYZ);
}

/*
 * Take a snapshot by montaging at higher zoom
 */
void SlicerFuncs::montageSnapshot(int snaptype)
{
  int ix, iy, xFullSize, yFullSize,  xCopyDelta, yCopyDelta, hqSave;
  int xOverlap, yOverlap, numChunks;
  float xTransStart, yTransStart, xTransDelta, yTransDelta;
  int fromXoff, fromYoff, toXoff, toYoff, overhalf,xCopy,yCopy;
  float cxSave, cySave, czSave, scaleFactor;
  unsigned char *framePix, **fullPix, **linePtrs;
  double zoomSave;
  static int fileno = 0;
  Ipoint vec;
  int factor = imcGetSlicerMontFactor();
  ScaleBar *barReal = scaleBarGetParams();
  ScaleBar barSaved;

  // Save center and zoom 
  cxSave = mCx;
  cySave = mCy;
  czSave = mCz;
  zoomSave = mZoom;
  hqSave = mHq;
  vec.z = 0.;

  // Start out with an overlap big enough to avoid quadratic interpolated areas
  xOverlap = yOverlap = (int)ceil(3. * mZoom * factor);
  if (xOverlap > mWinx / 4 || yOverlap > mWiny / 4) {
    wprint("\aThe window is not big enough to take a montage with the "
           "required amount of overlap between frames.\n");
    return;
  }

  // Reduce the zoom so that the panels will cover the full area after overlap
  // is taken out, take the larger of factor for X and Y to avoid anything
  // outside the current window
  scaleFactor = factor - ((factor-1.) * xOverlap) / B3DMAX(mWinx, mWiny);

  // Get coordinate offsets and revised offsets and arrays
  getMontageShifts(factor, scaleFactor, mWinx, xOverlap, xTransStart, 
                   xTransDelta, xCopyDelta, xFullSize);
  getMontageShifts(factor, scaleFactor, mWiny, yOverlap, yTransStart,
                   yTransDelta, yCopyDelta, yFullSize);

  if (utilStartMontSnap(mWinx, mWiny, xFullSize, yFullSize,
                        scaleFactor, barSaved, numChunks, &framePix, &fullPix, 
                        &linePtrs)) {
    wprint("\aFailed to get memory for snapshot buffers.\n");
    return;
  }

  // On Quadro card (?), it is necessary to defer the autoswap for montaging
  // It's not clear why this isn't needed for other snapshots
  // NEED TO CHECK THIS!!!
  if (App->doublebuffer) {
    mGlw->setBufferSwapAuto(false);
    glReadBuffer(GL_BACK);
  }

  // Set up scaling
  if (imcGetScaleThicks()) {
    sScaleThick = imcGetThickScaling();
    if (sScaleThick == 1)
      sScaleThick = factor;
  }

  // Loop on frames, getting pixels and copying them
  mZoom *= scaleFactor;
  mHq = 1;
  sDoingMontage = true;
  for (iy = 0; iy < factor; iy++) {
    for (ix = 0; ix < factor; ix++) {

      // Set up for scale bar if it is the right corner
      utilMontSnapScaleBar(ix, iy, factor, mWinx - 4, mWiny - 4, 
                           mZoom, barSaved.draw);
      
      mCx = cxSave;
      mCy = cySave;
      mCz = czSave;
      vec.x = xTransStart + ix * xTransDelta;
      vec.y = yTransStart + iy * yTransDelta;
      translateByRotatedVec(&vec, true);
      mGlw->updateGL();

      // Print scale bar length if it was drawn
      if (mScaleBarSize > 0)
        imodPrintStderr("Scale bar for montage is %g %s\n", mScaleBarSize,
                        imodUnits(mVi->imod));

      glReadPixels(0, 0, mWinx, mWiny, GL_RGBA, GL_UNSIGNED_BYTE, 
                   framePix);
      glFlush();

      // Set up copy of middle pieces then adjust for first or last piece
      overhalf = xOverlap / 2;
      xCopy = mWinx - xOverlap;
      fromXoff = overhalf;
      toXoff = ix * xCopyDelta + overhalf;
      if (!ix) {
        xCopy += overhalf;
        toXoff -= overhalf;
        fromXoff -= overhalf;
      }
      if (ix == factor - 1)
        xCopy += xOverlap - overhalf;
      overhalf = yOverlap / 2;
      yCopy = mWiny - yOverlap;
      fromYoff = overhalf;
      toYoff = iy * yCopyDelta + overhalf;
      if (!iy) {
        yCopy += overhalf;
        toYoff -= overhalf;
        fromYoff -= overhalf;
      }
      if (iy == factor - 1)
        yCopy += yOverlap - overhalf;
      memLineCpy(linePtrs, framePix, xCopy, yCopy, 4,
                 toXoff, toYoff, mWinx, fromXoff, fromYoff);
      if (App->doublebuffer) 
        mGlw->swapBuffers();
    }
  }

  if (App->doublebuffer)
    mGlw->setBufferSwapAuto(true);

  // Reset the file number to zero unless doing movie, then get name and save
  if (!mMovieSnapCount)
    fileno = 0;

  // Save the image then restore display
  utilFinishMontSnap(linePtrs, xFullSize, yFullSize, snaptype - 1,
                     fileno, 3, scaleFactor, "slicer", 
                     "3dmod: Saving slicer");

  *barReal = barSaved;
  mHq = hqSave;
  mZoom = zoomSave;
  mCx = cxSave;
  mCy = cySave;
  mCz = czSave;
  sDoingMontage = false;
  sScaleThick = 1;
  draw();
  utilFreeMontSnapArrays(fullPix, numChunks, framePix, linePtrs);
}

// COmpute the shifts for one axis of the montage
void SlicerFuncs::getMontageShifts(int factor, float scaleFactor,
                             int win, int &overlap, float &transStart,
                             float &transDelta, int &copyDelta, int &fullSize)
{

  // Increase overlap so that the spacing between panels is a multiple of the
  // zoom factor, so that the computed grid points match between panels
  int izoom = B3DMAX(1, (int)(mZoom * scaleFactor));
  overlap += (win - overlap) % izoom;
  transDelta = (win - overlap) / (mZoom * scaleFactor);
  transStart = - transDelta * (factor - 1.) / 2.;
  copyDelta = win - overlap;
  fullSize = win * factor - (factor - 1) * overlap;
}

// Get the Z scale of the volume before slicing: it is the product of
// an implicit Z scale from the ratio of Z to XY binning, and actual z scale
// if zscale before is selected 
float SlicerFuncs::getZScaleBefore()
{
  float zs = (float)mVi->zbin / (float)mVi->xybin;
  if (mScalez == SLICE_ZSCALE_BEFORE && mVi->imod->zscale > 0)
    zs *= mVi->imod->zscale;
  return zs;
}

// Set the X, y, z coordinates based on the point in the window
int SlicerFuncs::setxyz(int x, int y)
{
  float xm, ym, zm;
  int zmouse;

  zmouse = getxyz(x, y, xm, ym, zm);

  /* Set up pending coordinates for next draw to use */
  mPendx = xm;
  mPendy = ym;
  mPendz = zm;
  mPending = 1;

  /* DNM: do this regardless of whether locked or not.  
     9/8/06: changed from setting zmouse to the integral value to
     setting it the real value but returning the integral value for the
     call to set after point insertion/modification. */
  mVi->xmouse = xm; 
  mVi->ymouse = ym; 
  mVi->zmouse = zm;
  //imodPrintStderr("setxyz %f %f %f\n", xm, ym, zm);
  return zmouse;
}

// Compute the x, y, z coordinates corresponding to a point in window, by default,
// clamp it to legal range for window coords
int SlicerFuncs::getxyz(int x, int y, float &xm, float &ym, float &zm, bool clamp)
{
  return getxyz((float)x, (float)y, xm, ym, zm, clamp);
}

int SlicerFuncs::getxyz(float x, float y, float &xm, float &ym, float &zm, bool clamp)
{
  float xoffset, yoffset;
  float zs;
  Ipoint delpt;

  /* DNM: the xzoom and yzoom are correct for all cases, and the zs
     needs to be applied only for the case of SCALE_BEFORE */

  zs = 1.0f / getZScaleBefore();
  //  if (mScalez == SLICE_ZSCALE_BEFORE && mVi->imod->zscale > 0)
  //  zs = 1.0 / mVi->imod->zscale;

  /* DNM: have to use integer arithmetic on window sizes, and account for
     y going from 0 to winy-1 when inverting it */

  xoffset = ((mWinx / 2) - x) / mXzoom;
  yoffset = ((mWiny / 2) - (mWiny - 1 - y)) / mYzoom;
     
  /* DNM: always take position relative to current display center,
     regardless of whether the window is locked (it was using [xyz]mouse */
  xm = mCx;
  ym = mCy;
  zm = mCz;

  delpt.x = (mXstep[b3dX] * xoffset) + (mYstep[b3dX] * yoffset);
  delpt.y = (mXstep[b3dY] * xoffset) + (mYstep[b3dY] * yoffset);
  delpt.z = (mXstep[b3dZ] * xoffset * zs) + (mYstep[b3dZ] * yoffset *zs);
  
  /*
  setForwardMatrix();
  imodMatTransform3D(mMat, &delpt, &rotpt);
  printf("angles %.2f %.2f rotpt %f %f %f\n", mTang[b3dX], mTang[b3dY],
         rotpt.x, rotpt.y, rotpt.z);
  */
  xm -= delpt.x;
  ym -= delpt.y;
  zm -= delpt.z;

  if (clamp) {
    B3DCLAMP(xm, 0, mVi->xsize - 1);
    B3DCLAMP(ym, 0, mVi->ysize - 1);
    B3DCLAMP(zm, 0, mVi->zsize - 1);
  }
  return(B3DNINT(zm));
}

// Get the window coordinates corresponding to the given image point 
// coordinates as integer values; zim should be 0 if the point is in the 
// current plane
void SlicerFuncs::getWindowCoords(float xcur, float ycur, float zcur, int &xim,
                                  int &yim, int &zim)
{
  float xwin, ywin, zwin;
  getWindowCoords(xcur, ycur, zcur, xwin, ywin, zwin);
  zim = B3DNINT(zwin);
  xim = B3DNINT(xwin);
  yim = B3DNINT(ywin);
}

void SlicerFuncs::getWindowCoords(float xcur, float ycur, float zcur, float &xim,
                                  float &yim, float &zim)
{
  Ipoint pnt, xn, yn, zn;
  Imat *mat = mMat;
  float xoffset, yoffset;
  float zs;
  zs = 1.0f / getZScaleBefore();

  setForwardMatrix();

  pnt.y = pnt.z = 0.0f;
  pnt.x = 1.0f;
  imodMatTransform3D(mat, &pnt, &xn);
  pnt.x = 0.0f;
  pnt.y = 1.0f;
  imodMatTransform3D(mat, &pnt, &yn);
  pnt.y = 0.0f;
  pnt.z = 1.0f;
  imodMatTransform3D(mat, &pnt, &zn);
  xcur -= mCx;
  ycur -= mCy;
  zcur = (zcur - mCz) / zs;
  xoffset = xn.x * xcur + yn.x * ycur + zn.x * zcur;
  yoffset = xn.y * xcur + yn.y * ycur + zn.y * zcur;
  zim = xn.z * xcur + yn.z * ycur + zn.z * zcur;
  xim = xoffset * mXzoom + mWinx / 2;
  yim = yoffset * mYzoom + mWiny / 2;
}

/* 
 * Convert rubber band mouse coordinates to 3-D image coordinates
 */
void SlicerFuncs::bandMouseToImage()
{
  float xim0, yim0, zim0, xwin0, ywin0, zwin0, xim1, yim1, zim1, xwin1, ywin1, zwin1;

  // Get the image coordinates of the clicked points in the current plane 
  getxyz(mRbMouseX0, mRbMouseY0, xim0, yim0, zim0, false);
  getxyz(mRbMouseX1, mRbMouseY1, xim1, yim1, zim1, false);

  // Get window coordinates of these points on a plane through volume center by switching
  // the current position to the center
  swapCenterXYZ(mVi->xsize / 2., mVi->ysize / 2., mVi->zsize / 2.);
  getWindowCoords(xim0, yim0, zim0, xwin0, ywin0, zwin0);
  getWindowCoords(xim1, yim1, zim1, xwin1, ywin1, zwin1);

  // Get the image coordinates of the points on the center plane
  getxyz(xwin0, mWiny - 1 - ywin0, mRbImageX0, mRbImageY0, mRbImageZ0, false); 
  getxyz(xwin1, mWiny - 1 - ywin1, mRbImageX1, mRbImageY1, mRbImageZ1, false);
  restoreCenterXYZ();
}

/* 
 * Convert rubber band 3-D image coordinates to mouse coordinates in the current plane
 */
void SlicerFuncs::bandImageToMouse()
{
  int zwin, ywin;
  getWindowCoords(mRbImageX0, mRbImageY0, mRbImageZ0, mRbMouseX0, ywin, zwin);
  mRbMouseY0 = mWiny - 1 - ywin;
  getWindowCoords(mRbImageX1, mRbImageY1, mRbImageZ1, mRbMouseX1, ywin, zwin);
  mRbMouseY1 = mWiny - 1 - ywin;
}

/*
 * Set the limits for a snapshot
 * Have to set both rubberband corners within legal limits then take difference
 */
void SlicerFuncs::setSnapshotLimits(int **limits, int *limarr)
{
  *limits = NULL;
  if (mRubberband) {
    *limits = limarr;
    bandImageToMouse();
    limarr[0] = mRbMouseX0 + 1;
    limarr[1] = mWiny - mRbMouseY1;
    B3DCLAMP(limarr[0], 0, mWinx - 2);
    B3DCLAMP(limarr[1], 0, mWiny - 2);
    limarr[2] = mRbMouseX1 - 1;
    limarr[3] = mWiny - mRbMouseY0 - 2;
    B3DCLAMP(limarr[2], limarr[0], mWinx - 1);
    B3DCLAMP(limarr[3], limarr[1], mWiny - 1);
    limarr[2] += 1 - limarr[0];
    limarr[3] += 1 - limarr[1];
  }
}

/*
 * Turn the rubberband on or off; toggle arrow if necessary
 */
void SlicerFuncs::toggleRubberband(bool drawWin)
{
  if (mRubberband || mStartingBand) {
    mRubberband = 0;
    mStartingBand = 0;
    mBandChanged = 1;
  } else {
    if (mDrawingArrow)
      toggleArrow(false);
    mStartingBand = 1;
    mLimitNoValue = b3dIMax(3, mVi->xsize, mVi->ysize, mVi->zsize);
    mBandLowHighLimits[0] = mBandLowHighLimits[1] = 2 * mLimitNoValue;
  }
  mGlw->setMouseTracking(sPixelViewOpen || mRubberband);
  mQtWindow->setToggleState(SLICER_TOGGLE_BAND, mRubberband + mStartingBand);
  mQtWindow->enableLowHighButtons(mRubberband + mStartingBand);
  setCursor(mMousemode, true);
  if (drawWin)
    draw();
  setCursor(mMousemode, true);
}

/*
 * Set one of the limits for extraction as a shift-independent position relative to the
 * central plane
 */
void SlicerFuncs::setBandLowHighLimit(int which)
{
  int axis;
  float trueLimits[2];
  float current, start, end, axisComp, delta;
  Ipoint norm;

  // If shift is pressed, then go to the limit instead of setting it
  getNormalAndMainComponent(axis, &norm, axisComp);
  if (sShiftPressed) {
    if (mBandLowHighLimits[which] > mLimitNoValue || 
        findBandAxisRange(current, start, end))
      return;
    ivwControlPriority(mVi, mCtrl);
    checkBandLowHighLimits(trueLimits);
    delta = (trueLimits[which] - current) / axisComp;
    mCx = (mRbImageX0 + mRbImageX1) / 2. + delta * norm.x;
    mCy = (mRbImageY0 + mRbImageY1) / 2. + delta * norm.y;
    mCz = (mRbImageZ0 + mRbImageZ1) / 2. + delta * norm.z;
    if (!mLocked) {
      mVi->zmouse = mCz;
      ivwBindMouse(mVi);
    }
    changeCenterIfLinked();
    synchronizeSlicers();
    showSlice();
    return;
  }

  // The canonical limit is the position along dominant axis of the current center 
  // relative to its position on the central plane
  mBandLowHighLimits[which] = currentMainAxisDistance(axis);
  checkBandLowHighLimits(trueLimits);
}

/*
 * Given the dominant axis, find position along this axis of the current point relative
 * to the projection of that point on the central plane
 */
float SlicerFuncs::currentMainAxisDistance(int axis)
{
  float xwin, ywin, zwin, xim, yim, zim;

  // Swap in the volume center and find position of current point relative to that plane
  swapCenterXYZ(mVi->xsize / 2., mVi->ysize / 2., mVi->zsize / 2.);
  getWindowCoords(mSaveCx, mSaveCy, mSaveCz, xwin, ywin, zwin);
  getxyz(xwin, mWiny - 1 - ywin, xim, yim, zim, false);
  restoreCenterXYZ();
  //imodPrintStderr("cz  %1.f  zim %.1f  limit %.1f\n",mCz,zim,mCz - zim);
  if (axis == 0)
    return mCx - xim;
  else if (axis == 1)
    return mCy - yim;
  else
    return mCz - zim;
}

/*
 * Check the extraction limits for the band; return 2 if the band cannot be extracted 
 * from at all at its current position, 1 if either limit is hasn't been set yet.
 * Return the true limits, which are relative to the current position of the band
 * along the axis
 */
int SlicerFuncs::checkBandLowHighLimits(float trueLimits[2])
{
  int ind;
  float current, start, end;;
  int invalid = 0;
  
  if (findBandAxisRange(current, start, end))
    return 2;
  for (ind = 0; ind < 2; ind++) {
    //imodPrintStderr("%d blhl %.1f\n", ind, mBandLowHighLimits[ind]);
    if (mBandLowHighLimits[ind] > b3dIMax(3, mVi->xsize, mVi->ysize, mVi->zsize)) {
      invalid = 1;
      mQtWindow->setLowHighValidity(ind, SLICER_LIMIT_INVALID);
    } else {
      trueLimits[ind] = current + mBandLowHighLimits[ind];
      //imodPrintStderr("%d true %.1f\n", ind, trueLimits[ind]);
      mQtWindow->setLowHighValidity(ind, 
                                    (trueLimits[ind] < start || trueLimits[ind] > end) ?
                                    SLICER_LIMIT_TRUNCATE : SLICER_LIMIT_VALID);
      B3DCLAMP(trueLimits[ind], start, end);
    }
  }
  return invalid;
}

/*
 * Find the range for movement of the rubber band along the axis, and its current 
 * canonical position.  These are all relative to the middle of the volume on the 
 * dominant axis.
 */
bool SlicerFuncs::findBandAxisRange(float &current, float &start, float &end)
{
  int xmovie, ymovie, zmovie, axis, iStart, iEnd;
  float delta;
  if (!mRubberband)
    return true;

  // Set the center to the middle of the rubberband area and find the limits along the 
  // axis
  swapCenterXYZ((mRbImageX0 + mRbImageX1) / 2., (mRbImageY0 + mRbImageY1) / 2.,
                (mRbImageZ0 + mRbImageZ1) / 2.);
  findMovieAxis(xmovie, ymovie, zmovie, 1, axis);
  findAxisLimits(axis, current, iStart, iEnd);
  delta = (xmovie * mVi->xsize + ymovie * mVi->ysize + zmovie * mVi->zsize) / 2.;
  start = iStart - delta;
  end = iEnd - delta;
  current -= delta;
  //imodPrintStderr("BAL %.1f, %.1f %.1f\n", current, start, end);
  restoreCenterXYZ();
  return iStart < 0;
}

/*
 * Compose a command for running rotatevol
 */
QString SlicerFuncs::rotateVolCommand()
{
  float trueLimits[2];
  int axis, xsize, ysize, zsize;
  float xcen, ycen, zcen, axisComp, lowDelta, highDelta, midDelta, current, start, end;
  float temp;
  int fx, fy, fz, llx, lly, llz, xpad, ypad, zpad;
  double alpha, beta, gamma;
  int bin = mVi->xybin;
  Ipoint norm;
  QString str = "";

  // Check all conditions
  int invalid = checkBandLowHighLimits(trueLimits);
  if (mStartingBand) {
    wprint("\aYou must draw a rubber band and set Lo/Hi limits or turn off the "
           "rubber band in the Slicer window.\n");
    return str;
  }
  if (invalid == 2) {
    wprint("\aThe rubber band in the Slicer is not in a valid position that overlaps "
           "the volume sufficiently.\n");
    return str;
  }
  if (invalid == 1) {
    wprint("\aYou must set both Li/Hi limits to extract with the rubber band in the "
           "Slicer.\n");
    return str;
  }
  if (getZScaleBefore() != 1.) {
    wprint("\aYou cannot extract from the slicer with unequal binning in X and Y "
           "or \"Z-Scale Before\" selected.\n");
    return str;
  }

  // The "current" value is offset of rubberband on central plane from middle of volume,
  // and the true limits are also relative to the middle of the volume
  // Their difference from current indicates how far they are from stored RB position
  // along the main axis, and to move that distance on the main axis as part of a
  // move along the normal, we need to divide by normal component on that axis
  findBandAxisRange(current, start, end);
  getNormalAndMainComponent(axis, &norm, axisComp);
  temp = (trueLimits[0] - current) / axisComp;
  highDelta = (trueLimits[1] - current) / axisComp;
  lowDelta = B3DMIN(temp, highDelta);
  highDelta = B3DMAX(temp, highDelta);
  zsize = bin * (B3DNINT(highDelta - lowDelta) + 1);
  midDelta = 0.5 * (lowDelta + highDelta);
  xcen = 0.5 * (mRbImageX0 + mRbImageX1) + midDelta * norm.x;
  ycen = 0.5 * (mRbImageY0 + mRbImageY1) + midDelta * norm.y;
  zcen = 0.5 * (mRbImageZ0 + mRbImageZ1) + midDelta * norm.z;
  if (ivwGetImagePadding(mVi, B3DNINT(ycen), B3DNINT(zcen), ivwWindowTime(mVi, mTimeLock),
                         llx, xpad, fx, lly, ypad, fy, llz, zpad, fz) < 0)
    return str;

  // Get the angles and center in case of no flip
  xcen = bin * (xcen + llx - xpad);
  alpha = mTang[b3dX];
  beta = mTang[b3dY];
  gamma = mTang[b3dZ];
  if (mVi->li->axis == 3) {
    ycen = bin * (ycen + lly - ypad);
    zcen = bin * (zcen + llz - zpad) + 0.5;
  } else {

    // In case of flip, swap the Y and Z centers appropriately and get new angles
    temp = bin * ((mVi->zsize - 1 - zcen) + lly - zpad) + 0.5;
    zcen = bin * (ycen + llz - ypad);
    ycen = temp;

    // Premultiply the rotation matrix by -90 X rotation
    Imat *rmat = imodMatNew(3);
    Imat *pmat = imodMatNew(3);
    if (!rmat || !pmat)
      return str;
    setForwardMatrix();
    imodMatRot(rmat, -90., b3dX);
    imodMatMult(rmat, mMat, pmat);
    imodMatGetNatAngles(pmat, &alpha, &beta, &gamma);
    free(rmat);
    free(pmat);
  }

  // Use size of band adjusted for bin and zoom as size of image
  bandImageToMouse();
  xsize = bin * b3dIMax(2, 2, B3DNINT((mRbMouseX1 + 1 - mRbMouseX0) / mXzoom));
  ysize = bin * b3dIMax(2, 2, B3DNINT((mRbMouseY1 + 1 - mRbMouseY0) / mYzoom));
  str.sprintf("-siz %d,%d,%d -cen %.2f,%.2f,%.2f -ang %.2f,%.2f,%.2f", xsize, ysize, 
              zsize, xcen, ycen, zcen, gamma, beta, alpha);
  return str;
}

/*
* Resize the image to fit the rubberband and center it on the band if possible
*/
void SlicerFuncs::resizeToFit()
{
  int width, height, neww, newh;
  int dx, dy, newdx, newdy, axis;
  float current, axisComp;
  Ipoint norm;

  width = mQtWindow->width();
  height = mQtWindow->height();
  QRect pos = ivwRestorableGeometry(mQtWindow);
  dx = pos.x();
  dy = pos.y();
  /* imodPrintStderr("dx %d dy %d\n", dx, dy); */

  // set size to size of band
  bandImageToMouse();
  neww = mRbMouseX1 -1 - mRbMouseX0 + width - mWinx;
  newh = mRbMouseY1 -1 - mRbMouseY0 + height - mWiny;

  // distance of current point from midplane (where RB is located) along the dominant
  // axis, divide by normal component on that axis to get the move to make along the
  // normal to move that distance on main axis, then move there from RB position
  getNormalAndMainComponent(axis, &norm, axisComp);
  current = currentMainAxisDistance(axis) / axisComp;
  mCx = 0.5 * (mRbImageX0 + mRbImageX1) + current * norm.x;
  mCy = 0.5 * (mRbImageY0 + mRbImageY1) + current * norm.y;
  mCz = 0.5 * (mRbImageZ0 + mRbImageZ1) + current * norm.z;
  B3DCLAMP(mCx, 0, mVi->xsize);
  B3DCLAMP(mCy, 0, mVi->ysize);
  B3DCLAMP(mCz, 0, mVi->zsize - 0.5);
  if (!mLocked)
    mVi->zmouse = mCz;
  toggleRubberband(false);
  
  diaLimitWindowSize(neww, newh);
  newdx = dx + width / 2 - neww / 2;
  newdy = dy + height / 2 - newh / 2;
  diaLimitWindowPos(neww, newh, newdx, newdy);

  /* imodPrintStderr("newdx %d newdy %d\n", newdx, newdy); */
  mQtWindow->resize(neww, newh);
  mQtWindow->move(newdx, newdy);
  changeCenterIfLinked();
  synchronizeSlicers();
  showSlice();
}

/*
 * Resize and center the region inside the rubber band to fill one dimension of the window
 */
void SlicerFuncs::resizeBandToWindow()
{
  int axis;
  float current, axisComp;
  Ipoint norm;

  // set size to size of band
  bandImageToMouse();
  mZoom *= B3DMIN(mWinx / (mRbMouseX1 - 1. - mRbMouseX0), 
                  mWinx / (mRbMouseY1 - 1. - mRbMouseY0));

  // distance of current point from midplane (where RB is located) along the dominant
  // axis, divide by normal component on that axis to get the move to make along the
  // normal to move that distance on main axis, then move there from RB position
  getNormalAndMainComponent(axis, &norm, axisComp);
  current = currentMainAxisDistance(axis) / axisComp;
  mCx = 0.5 * (mRbImageX0 + mRbImageX1) + current * norm.x;
  mCy = 0.5 * (mRbImageY0 + mRbImageY1) + current * norm.y;
  mCz = 0.5 * (mRbImageZ0 + mRbImageZ1) + current * norm.z;
  B3DCLAMP(mCx, 0, mVi->xsize);
  B3DCLAMP(mCy, 0, mVi->ysize);
  B3DCLAMP(mCz, 0, mVi->zsize - 0.5);
  if (!mLocked)
    mVi->zmouse = mCz;
  changeCenterIfLinked();
  synchronizeSlicers();
  showSlice();
}

#ifndef NDEBUG
int printmat(Imat *mat)
{
  int i;
  for(i = 0; i < 16; i+=4)
    wprint ("%5.2f %5.2f %5.2f %5.2f\n",
            mat->data[i], mat->data[i+1], 
            mat->data[i+2], mat->data[i+3]);
  return(0);
}
#endif

double SlicerFuncs::fixangle(double angle)
{
  double rpd = RADIANS_PER_DEGREE;
  if (angle <= -180. * rpd)
    angle += 360. * rpd;
  if (angle > 180. * rpd)
    angle -= 360. * rpd;
  return angle;
}

// Compose the forward transformation matrix with the current angles
void SlicerFuncs::setForwardMatrix()
{
  imodMatId(mMat);
  imodMatRot(mMat, (double)mTang[b3dZ], b3dZ);
  imodMatRot(mMat, (double)mTang[b3dY], b3dY);
  imodMatRot(mMat, (double)mTang[b3dX], b3dX); 
}

// Compose the inverse transformation matrix with the current angles
void SlicerFuncs::setInverseMatrix()
{
  imodMatId(mMat);
  imodMatRot(mMat, (double)(-mTang[b3dX]), b3dX); 
  imodMatRot(mMat, (double)(-mTang[b3dY]), b3dY);
  imodMatRot(mMat, (double)(-mTang[b3dZ]), b3dZ);
}

/* Translates the center position within or normal to plane, returns 1 if the
   position is changed.  The vector is in Z-scaled space; so after 
   back-rotating it is scaled down into original pixel space.  Lateral moves
   are correct for the panning motion in the window, but vertical moves need
   to be normalized */
 int SlicerFuncs::translateByRotatedVec(Ipoint *vec, 
                                  bool outsideOK)
{
  Ipoint vrot;
  setInverseMatrix();
  imodMatTransform3D(mMat, vec, &vrot);
  vrot.z /= getZScaleBefore();
  if (vec->z) {
    imodPointNormalize(&vrot);
    if (imodDebug('s'))
      imodPrintStderr("vec %.2f %.2f %.2f vrot %.4f %.4f %.4f\n", vec->x, 
                      vec->y, vec->z, vrot.x, vrot.y, vrot.z);
  }
  if ((mCx + vrot.x >= 0 && mCx + vrot.x < mVi->xsize && 
       mCy + vrot.y >= 0 && mCy + vrot.y < mVi->ysize && 
       mCz + vrot.z >= 0 && mCz + vrot.z < mVi->zsize - 0.5) || 
      outsideOK) {
    mCx += vrot.x;
    mCy += vrot.y;
    mCz += vrot.z;
    return 1;
  }
  return 0;
}

// Return the normal to the current plane.  In order for this to be a vector for movement
// on the view axis, DIVIDE the inverse transformation of Z vector by the Z scaling before
void SlicerFuncs::getNormalToPlane(Ipoint *norm)
{
  Ipoint vec = {0., 0., 1.};
  setInverseMatrix();
  imodMatTransform3D(mMat, &vec, norm);
  norm->z /= getZScaleBefore();
  imodPointNormalize(norm);
}

void SlicerFuncs::getNormalToPlane(float vec[3])
{
  Ipoint vrot;
  getNormalToPlane(&vrot);
  vec[0] = vrot.x;
  vec[1] = vrot.y;
  vec[2] = vrot.z;
}

/*
 * Set slicer angles to line up the two points along the given axis
 */
void SlicerFuncs::setAnglesFromPoints(Ipoint *p1, Ipoint *p2, int axis)
{
  Ipoint n;
  Ipoint a;
  double val;
  float smallVal = 1.e-4;
  double rpd = RADIANS_PER_DEGREE;
  n.x = p2->x - p1->x;
  n.y = p2->y - p1->y;
  n.z = p2->z - p1->z;
  n.z *= getZScaleBefore();
  //  if (mScalez == SLICE_ZSCALE_BEFORE)
  //  n.z *= mVi->imod->zscale;
  if (n.x == 0.0 && n.y == 0.0 && n.z == 0.0)
    return;
  imodPointNormalize(&n);

  if (axis == b3dX) {
    a.z = 0.0;
    if (n.x > smallVal || n.y > smallVal || n.x < -smallVal || n.y < -smallVal)
      a.z = -atan2((double)n.y, (double)n.x);
    val = a.z;
    val = n.x * cos(val) - n.y * sin(val);
    a.y = SlicerFuncs::fixangle(90. * rpd - atan2(val, (double)n.z));
    a.x = 0.0;
  } else if (axis == b3dY) {
    a.z = 0.0;
    if (n.x > smallVal || n.y > smallVal || n.x < -smallVal || n.y < -smallVal)
      a.z = SlicerFuncs::fixangle(90. * rpd - atan2((double)n.y, (double)n.x));
    val = a.z;
    val = n.x * sin(val) + n.y * cos(val);
    a.x = -atan2((double)n.z, val);
    a.y = 0.0;
  } else {
    /* This may be most logical in terms of general angles available */
    /*   a.y = 0.0;
         if (n.x > smallVal || n.z > smallVal || n.x < -smallVal || n.z < -smallVal)
         a.y = -atan2((double)n.x, (double)n.z);
         val = a.y;
         val = n.z * cos(val) - n.x * sin(val);
         a.x = SlicerFuncs::fixangle(90. * rpd - atan2(val, (double)n.y));
         a.z = 0.0; */

    /* But this is useful for doing Z rotation and then X, and for
       keeping all the angles under +/- 90 degrees. */
    a.z = 0.0;
    if (n.x > smallVal || n.y > smallVal || n.x < -smallVal || n.y < -smallVal) {
      if (n.y >= 0.0)
        a.z = atan2((double)n.x, (double)n.y);
      else
        a.z = -atan2((double)n.x, -(double)n.y);
    }
    val = a.z;
    val = n.x * sin(val) + n.y * cos(val);
    if (n.z >= 0.0)
      a.x = atan2(val, (double)n.z);
    else
      a.x = -atan2(val, -(double)n.z);
    a.y = 0.0;
  }

  mTang[b3dX] = a.x / rpd;
  mTang[b3dY] = a.y / rpd;
  mTang[b3dZ] = a.z / rpd;
  mQtWindow->setAngles(mTang);

  // Center on current point if not locked
  p2 = imodPointGet(mVi->imod);
  if (p2 && !mClassic && !mLocked) {
    mVi->xmouse = p2->x;
    mVi->ymouse = p2->y;
    mVi->zmouse = p2->z;
    ivwBindMouse(mVi);
    mCx = mVi->xmouse;
    mCy = mVi->ymouse;
    mCz = mVi->zmouse;
  }
}

/*
 * Find the angles that make current contour flat and set center to its center
 */
int SlicerFuncs::anglesFromContour()
{
  Icont *rcont;
  Icont *cont = imodContourGet(mVi->imod);
  Ipoint scale = {1., 1., 1.};
  Ipoint n;
  int pt;
  float dval;
  double rpd = RADIANS_PER_DEGREE;
  double beta, alpha;
  double xsum, ysum, zsum;

  scale.z = getZScaleBefore();
  if (!cont || !cont->psize)
    return 1;
  rcont = imodContourDup(cont);
  if (!rcont) {
    wprint("\aMemory error trying to copy contour.\n");
    return 1;
  }

  // Rotate the points in the contour by the current Z so that the X and Y
  // can be solved on top of the current Z
  imodMatId(mMat);
  imodMatRot(mMat, (double)(mTang[b3dZ]), b3dZ);
  xsum = ysum = zsum = 0.;
  for (pt = 0; pt < cont->psize; pt++) {
    imodMatTransform3D(mMat, &cont->pts[pt], &rcont->pts[pt]);
    xsum += cont->pts[pt].x;
    ysum += cont->pts[pt].y;
    zsum += cont->pts[pt].z;
  }

  if (imodContourFitPlane(rcont, &scale, &n, &dval, &alpha, &beta)) {
    wprint("\aContour too small or too straight to find plane.\n");
    imodContourDelete(rcont);
    return 1;
  }
  imodContourDelete(rcont);
  mTang[b3dX] = alpha / rpd;
  mTang[b3dY] = beta / rpd;
  /* printf("norm %f %f %f alpha %.2f  beta %.2f\n", 
     n.x, n.y, n.z, alpha/rpd, beta/rpd); */
  mQtWindow->setAngles(mTang);

  // Set the center point and mouse point using the floating Z value, do draw
  // If you set zmouse integer, cz needs to be reset AND displays with Z scale
  // alternate between between right and wrong when lock is off
  mCx = B3DMAX(0., B3DMIN(xsum / cont->psize, mVi->xsize - 1));
  mCy = B3DMAX(0., B3DMIN(ysum / cont->psize, mVi->ysize - 1));
  mCz = B3DMAX(0., B3DMIN(zsum / cont->psize, mVi->zsize - 1));
  mVi->xmouse = mCx;
  mVi->ymouse = mCy;
  mVi->zmouse = mCz;
  pt = imodvLinkedToSlicer() ? IMOD_DRAW_MOD : 0;
  synchronizeSlicers();
  imodDraw(mVi, IMOD_DRAW_XYZ | pt);

  return 0;
}


/* set up the step factors for a new slice angle. */
void SlicerFuncs::transStep()
{
  Ipoint pnt;
  Ipoint xn, yn, zn;
  Ipoint normal, xcut, ycut, zcut;
  float x,y,z;
  float isize, jsize, zs;
  Imat *mat = mMat;

  /* DNM: made the angles be negative to match angles applied to volume */
  
  setInverseMatrix();

  pnt.y = pnt.z = 0.0f;
  pnt.x = 1.0f;
  imodMatTransform3D(mat, &pnt, &xn);
  pnt.x = 0.0f;
  pnt.y = 1.0f;
  imodMatTransform3D(mat, &pnt, &yn);
  pnt.y = 0.0f;
  pnt.z = 1.0f;
  imodMatTransform3D(mat, &pnt, &zn);

  imodPointCross(&xn, &yn, &normal);
  pnt.x = 1.0f;
  pnt.y = pnt.z = 0.0f;
  imodPointCross(&normal, &pnt, &xcut);
  pnt.x = 0.0f;
  pnt.y = 1.0f;
  imodPointCross(&normal, &pnt, &ycut);
  pnt.y = 0.0f;
  pnt.z = 1.0f;
  imodPointCross(&normal, &pnt, &zcut);

  imodPointNormalize(&xcut);
  imodPointNormalize(&ycut);
  imodPointNormalize(&zcut);

  mXstep[b3dX] = xn.x;
  mXstep[b3dY] = xn.y;
  mXstep[b3dZ] = xn.z;
  mYstep[b3dX] = yn.x;
  mYstep[b3dY] = yn.y;
  mYstep[b3dZ] = yn.z;
  mZstep[b3dX] = zn.x;
  mZstep[b3dY] = zn.y;
  mZstep[b3dZ] = zn.z;
     
  /* Set cut points */
  mVi->slice.zx1 = mVi->slice.zx2 =
    mVi->slice.yx1 = mVi->slice.yx2 = (int)mCx;
  mVi->slice.zy1 = mVi->slice.zy2 =
    mVi->slice.xy1 = mVi->slice.xy2 = (int)mCy;
  mVi->slice.xz1 = mVi->slice.xz2 =
    mVi->slice.yz1 = mVi->slice.yz2 = (int)mCz;

  if ((zcut.x) || (zcut.y)){
    x = mCx;
    y = mCy;
    do{
      x += zcut.x;
      y += zcut.y;
    }while((x < mVi->xsize) && (x > 0) &&
           (y < mVi->ysize) && (y > 0));
    mVi->slice.zx1 = (int)(x - zcut.x);
    mVi->slice.zy1 = (int)(y - zcut.y);
    x = mCx;
    y = mCy;
    do{
      x -= zcut.x;
      y -= zcut.y;
    }while((x < mVi->xsize) && (x > 0) &&
           (y < mVi->ysize) && (y > 0));
    mVi->slice.zx2 = (int)(x + zcut.x);
    mVi->slice.zy2 = (int)(y + zcut.y);
  }else{
    mVi->slice.zx1 = mVi->slice.zy1 =
      mVi->slice.zx1 = mVi->slice.zx2 = 0;
  }

  /* set yx1, yz1  and yx2, yz2 */
  if ((ycut.x) || (ycut.z)){
    x = mCx;
    z = mCz;
    do{
      x += ycut.x;
      z += ycut.z;
    }while((x < mVi->xsize) && (x > 0) &&
           (z < mVi->zsize) && (z > 0));
    mVi->slice.yx1 = (int)(x - ycut.x);
    mVi->slice.yz1 = (int)(z - ycut.z);
    x = mCx;
    z = mCz;
    do{
      x -= ycut.x;
      z -= ycut.z;
    }while((x < mVi->xsize) && (x > 0) &&
           (z < mVi->zsize) && (z > 0));
    mVi->slice.yx2 = (int)(x + ycut.x);
    mVi->slice.yz2 = (int)(z + ycut.z);
  }else{
    mVi->slice.yx1 = mVi->slice.yz1 =
      mVi->slice.yx2 = mVi->slice.yz2 = 0;
  }

  /* set xy1, xz1  and xy2, xz2 */
  if ((xcut.y) || (xcut.z)){
    y = mCy;
    z = mCz;
    do{
      y += xcut.y;
      z += xcut.z;
    }while((y < mVi->ysize) && (y > 0) &&
           (z < mVi->zsize) && (z > 0));
    mVi->slice.xy1 = (int)(y - xcut.y);
    mVi->slice.xz1 = (int)(z - xcut.z);
    y = mCy;
    z = mCz;
    do{
      y -= xcut.y;
      z -= xcut.z;
    }while((y < mVi->ysize) && (y > 0) &&
           (z < mVi->zsize) && (z > 0));
    mVi->slice.xy2 = (int)(y + xcut.y);
    mVi->slice.xz2 = (int)(z + xcut.z);
  }else{
    mVi->slice.xy1 = mVi->slice.xz1 =
      mVi->slice.xy2 = mVi->slice.xz2 = 0;
  }


  /* set up starting image coord */
  mXo = mCx;
  mYo = mCy;
  mZo = mCz;

  isize = mWinx / mZoom;
  jsize = mWiny / mZoom;

  /* z stretch scale factor.   */
  zs = 1.0f / getZScaleBefore();
  /*  zs  = mVi->imod->zscale; 
  if (zs <= 0.0f)
    zs = 1.0f;
  zs = 1.0f / zs;
  if (mScalez != SLICE_ZSCALE_BEFORE)
  zs = 1.0f; */

  /* DNM: make these correct for HQ case at least */
  mXo -= (isize / 2) * mXstep[b3dX];
  mYo -= (isize / 2) * mXstep[b3dY];
  mZo -= (isize / 2) * mXstep[b3dZ] * zs;
  mZo -= (jsize / 2) * mYstep[b3dZ] * zs;
  mXo -= (jsize / 2) * mYstep[b3dX];
  mYo -= (jsize / 2) * mYstep[b3dY];

  return;
}



/* cubic convolution interpolation looks much better at high zoom : use it
 to fill in array created by shortcut */

void slicerCubicFillin(unsigned short *cidata, int winx, int winy, int izoom,
		       int ilimshort, int jlimshort, int minval, int maxval, int intData)
{
  int ifill, jfill, deli, delj, joffset, yoffset;
  int xn, xn2, xp, ynoffset, yn2offset, ypoffset;
  int i, j, xi, yi;
  float dysq, dycub, fyp, fy, fyn, fyn2;
  float dxsq, dxcub, fxp, fx, fxn, fxn2;
  float yp, y0, yn, yn2;
  float dx, dy, ival;
  float zoom = (float)izoom;
  int *idata = (int *)cidata;

  for (jfill = 0; jfill < izoom; jfill++) {
    dy = jfill / zoom;
    delj = -jfill;
    ifill = 0;
    dysq = dy * dy;
    dycub = dy * dysq;
    fyp = 2 * dysq - dycub - dy;
    fy = 1 + dycub - 2 * dysq;
    fyn = dy + dysq - dycub;
    fyn2 = dycub - dysq;
    if (jfill == 0)
      ifill = 1;
    for (; ifill < izoom; ifill++) {
      dx = ifill / zoom;
      deli = -ifill;
      dxsq = dx * dx;
      dxcub = dx * dxsq;
      fxp = 2 * dxsq - dxcub - dx;
      fx = 1 + dxcub - 2 * dxsq;
      fxn = dx + dxsq - dxcub;
      fxn2 = dxcub - dxsq;

      for (j = izoom + jfill; j < jlimshort; j += izoom) {
	joffset = j * winx;
	yi = j + delj;
	yoffset = yi * winx;
	ynoffset = (yi + izoom) * winx;
	yn2offset = (yi + 2 * izoom) * winx;
	ypoffset = (yi - izoom) * winx;

        if (intData) {
          for (i = izoom + ifill; i < ilimshort; i += izoom) {
            xi = i + deli;
            xn = xi + izoom;
            xn2 = xn + izoom;
            xp = xi - izoom;
            yp = fxp * idata[xp + ypoffset] + fx * idata[xi + ypoffset] +
              fxn * idata[xn + ypoffset] + fxn2 * idata[xn2 + ypoffset];
            y0 = fxp * idata[xp + yoffset] + fx * idata[xi + yoffset] +
              fxn * idata[xn + yoffset] + fxn2 * idata[xn2 + yoffset];
            yn = fxp * idata[xp + ynoffset] + fx * idata[xi + ynoffset] +
              fxn * idata[xn + ynoffset] + fxn2 * idata[xn2 + ynoffset];
            yn2 = fxp * idata[xp + yn2offset] + fx * idata[xi + yn2offset] +
              fxn * idata[xn + yn2offset] + fxn2 * idata[xn2 + yn2offset];
            ival = fyp * yp + fy * y0 + fyn * yn + fyn2 * yn2;
            ival = B3DMIN(maxval, ival);
            ival = B3DMAX(minval, ival);
            idata[i + joffset] = (int)(ival + 0.5f);
          }
        } else {
          for (i = izoom + ifill; i < ilimshort; i += izoom) {
            xi = i + deli;
            xn = xi + izoom;
            xn2 = xn + izoom;
            xp = xi - izoom;
            yp = fxp * cidata[xp + ypoffset] + fx * cidata[xi + ypoffset] +
              fxn * cidata[xn + ypoffset] + fxn2 * cidata[xn2 + ypoffset];
            y0 = fxp * cidata[xp + yoffset] + fx * cidata[xi + yoffset] +
              fxn * cidata[xn + yoffset] + fxn2 * cidata[xn2 + yoffset];
            yn = fxp * cidata[xp + ynoffset] + fx * cidata[xi + ynoffset] +
              fxn * cidata[xn + ynoffset] + fxn2 * cidata[xn2 + ynoffset];
            yn2 = fxp * cidata[xp + yn2offset] + fx * cidata[xi + yn2offset] +
              fxn * cidata[xn + yn2offset] + fxn2 * cidata[xn2 + yn2offset];
            ival = fyp * yp + fy * y0 + fyn * yn + fyn2 * yn2;
                         
            ival = B3DMIN(maxval, ival);
            ival = B3DMAX(minval, ival);
            cidata[i + joffset] = (unsigned short)(ival + 0.5f);
          }
        }
      }
    }
  }
}


/*
 * GfX resize Events:
 */

void SlicerFuncs::resize(int winx, int winy)
{
  mWinx = winx;
  mWiny = winy;
  b3dResizeViewportXY(winx, winy);

  /* DNM: send 12 rather than App->mDepth to guarantee shorts */
  mImage   = b3dGetNewCIImageSize(mImage, 12, winx, winy);

  // Allocate buffers for doing RGB 3 times and saving first two channels
  if (mVi->rgbStore) {
    B3DFREE(mRedTemp);
    B3DFREE(mGreenTemp);
    mRedTemp = B3DMALLOC(unsigned char, winx * winy);
    mGreenTemp = B3DMALLOC(unsigned char, winx * winy);
    if (!mRedTemp || !mGreenTemp || !mImage) {
      B3DFREE(mRedTemp);
      B3DFREE(mGreenTemp);
      mRedTemp = NULL;
      mGreenTemp = NULL;
      b3dFreeCIImage(mImage);
      mImage = NULL;
    }
  }
  if (!mImage)
    wprint("\aInsufficient memory to run this Slicer window.\n"
           "Try making it smaller or close it.\n");

  ivwControlPriority(mVi, mCtrl);
}

void SlicerFuncs::cubeResize(int winx, int winy)
{
  b3dResizeViewportXY(winx, winy);
}

/*
 * Internal draw function: draw the image then the cube
 */
void SlicerFuncs::draw()
{
  if (imodDebug('c'))
    imodPrintStderr("Slicer ID %d in sslice_draw\n", mCtrl);

  // Turn off arrow or rubberband now if angles don't match
  if ((mArrowHead.size() > 1 || 
       (mArrowOn && (mArrowHead[0].x || mArrowHead[0].y || mArrowHead[0].z))) &&
      (fabs(mArrowAngle[0] - mTang[0]) > 1.e-3 ||
       fabs(mArrowAngle[1] - mTang[1]) > 1.e-3 ||
       fabs(mArrowAngle[2] - mTang[2]) > 1.e-3))
    clearArrows();
  if ((mRubberband || (mStartingBand && sMousePressed)) &&
      (fabs(mBandAngle[0] - mTang[0]) > 1.e-3 ||
       fabs(mBandAngle[1] - mTang[1]) > 1.e-3 ||
       fabs(mBandAngle[2] - mTang[2]) > 1.e-3))
    toggleRubberband(false);
  mGlw->updateGL();
  cubeDraw();
  mNeedDraw = false;
}

// Simple draw with slicer-only changes: if there are linked slicers do a
// general draw that will draw only them; otherwise draw self
void SlicerFuncs::drawSelfAndLinked()
{
  if (synchronizeSlicers())
    imodDraw(mVi, 0);
  else
    draw();
}

/* Redraw image, assuming data array is filled */
void SlicerFuncs::updateImage()
{
  mImageFilled++;
  mGlw->updateGL();
  mImageFilled = 0;
  cubeDraw();
}

/*
 * The paint routine called by the GL widget
 */
void SlicerFuncs::paint()
{
  int i, iz, ival, sliceScaleThresh = 4;
  int ixStart, iyStart, nxUse, nyUse, xim1, xim2, yim1, yim2;
  int mousing = sMousePanning + sMouseRotating +
    (ImodPrefs->speedupSlider() ? sSliderDragging : 0);
  if (!mImage)
    return;

  b3dUInt32 *idata = (b3dUInt32 *)mImage->id1;
  unsigned char *bdata = (unsigned char *)idata;
  GLenum format = GL_COLOR_INDEX;
  GLenum type   = GL_UNSIGNED_SHORT;
  GLint unpack = b3dGetImageType(&type, &format);
  if (unpack == 1) {
    unpack = 2;
    format = GL_COLOR_INDEX;
    type   = GL_UNSIGNED_SHORT;
  }

  // Skip draw if minimized!
  if (mQtWindow->isMinimized())
    return;

  if (imodDebug('s'))
    imodPrintStderr("Paint, image filled %d\n", mImageFilled);

  if (mImageFilled <= 0) {

    // If filling array, first get this section loaded into a tile cache
    if (mVi->pyrCache) {
      getSubsetLimits(ixStart, iyStart, nxUse, nyUse);
      mVi->pyrCache->loadTilesContainingArea(mVi->pyrCache->getBaseIndex(), ixStart,
                                             iyStart, nxUse, nyUse, B3DNINT(mCz));
    }

    // Then assess mean and SD for scaling multiple slices
    // to single slice, then fill array for real and update angles
    if (!mousing && !mVi->colormapImage && mNslice > sliceScaleThresh && !mVi->rgbStore)
      fillImageArray(0, 1, 0);
    else if (!mousing)
      mScaleToMeanSD = false;
    fillImageArray(mousing, 0, 0);
    if (mVi->rgbStore) {
      for (i = 0; i < mWinx * mWiny; i++)
        mRedTemp[i] = (unsigned char)idata[i];
      fillImageArray(mousing, 0, 1);
      for (i = 0; i < mWinx * mWiny; i++)
        mGreenTemp[i] = (unsigned char)idata[i];
      fillImageArray(mousing, 0, 2);
      for (i = 0; i < mWinx * mWiny; i++) {
        ival = idata[i];
        *bdata++ = mRedTemp[i];
        *bdata++ = mGreenTemp[i];
        *bdata++ = (unsigned char)ival;
        *bdata++ = 0;
      }
    }

    // Keep track of mouse where it was drawn, update slicer angle window
    mDrawnXmouse = mVi->xmouse;
    mDrawnYmouse = mVi->ymouse;
    mDrawnZmouse = mVi->zmouse;
    if (sSliceAngDia && (getTopSlicer() == this))
      sSliceAngDia->topSlicerDrawing(mTang, mCx, mCy, mCz, ivwWindowTime(mVi, mTimeLock), 
                                     sSliderDragging + mousing, mContinuous);
  }

  b3dSetCurSize(mWinx, mWiny);

  glPixelStorei(GL_UNPACK_ALIGNMENT, unpack);

  /* Just clear the unused edges if there are shifts */
  if (mXshift || mYshift) {
    b3dColorIndex(App->background);
    b3dDrawFilledRectangle(0, 0, mWinx, (int)mYshift);
    b3dDrawFilledRectangle(0, 0, (int)mXshift, mWiny);
  }

  /* DNM: one-to-one image for fractional zoom as well as in hq case */
  if (mNoPixelZoom)
    glPixelZoom(1.0f, 1.0f);
  else
    /* DNM: don't make this xzoom, yzoom.  */
    glPixelZoom(mZoom, mZoom);

  glRasterPos2f(mXshift, mYshift);

  glDrawPixels(mWinx, mWiny, format, type, mImage->id1);
    
  drawModel();
  drawCurrentPoint();

  mScaleBarSize = scaleBarDraw(mWinx, mWiny, mZoom, 0);

  for (i = 0; i < mArrowHead.size(); i++) {
    getWindowCoords(mArrowTail[i].x, mArrowTail[i].y, mArrowTail[i].z, xim1, yim1, iz);
    getWindowCoords(mArrowHead[i].x, mArrowHead[i].y, mArrowHead[i].z, xim2, yim2, iz);
    if (fabs((double)xim1 - xim2) > 2. || fabs((double)yim1 - yim2) > 2.) {
      b3dColorIndex(App->arrow[i % 4]);
      b3dDrawArrow(xim1, yim1, xim2, yim2);
    }
  }

  if (mRubberband) {
    b3dLineWidth(1);
    b3dColorIndex(App->endpoint);
    bandImageToMouse();
    b3dDrawRectangle(mRbMouseX0, mWiny - 1 - mRbMouseY1, 
                     mRbMouseX1 - mRbMouseX0, 
                     mRbMouseY1 - mRbMouseY0);
  } 

  // Update toolbar for time
  if (mVi->numTimes) {
    int time = ivwWindowTime(mVi, mTimeLock);
    if (mToolTime != time){
      mToolTime = time;
      mQtWindow->setTimeLabel(time, QString(ivwGetTimeIndexLabel(mVi, time)));
    }
  }
}

/*
 * Draw the current point and current contour end points
 */
void SlicerFuncs::drawCurrentPoint()
{
  int xim, yim, zim;
  Ipoint norm;
  float delta;
  ImodView *vi = mVi;
  Iobj *obj = imodObjectGet(vi->imod);
  Icont *cont = imodContourGet(vi->imod);
  Ipoint *pnt = imodPointGet(vi->imod);
  int imPtSize, modPtSize, backupSize, curSize;

  // Draw cursors. 

  if (!mVi->drawcursor)
    return;

  utilCurrentPointSize(obj, &modPtSize, &backupSize, &imPtSize);
  b3dLineWidth(1);
  getNormalToPlane(&norm);

  // Draw crosshairs in center regardless; and draw current
  // point in movie mode non-classic.  Draw it dimmer if it is off plane
  b3dColorIndex(App->endpoint);
  if (!sDoingMontage)
    b3dDrawPlus((int)(mWinx * 0.5f), (int)(mWiny * 0.5f), 5);
  if (mMousemode == IMOD_MMOVIE || !pnt || !cont) {
    if (!mClassic) {
      delta = norm.x * (mVi->xmouse - mCx) + norm.y * 
        (mVi->ymouse - mCy) + norm.z * (mVi->zmouse - mCz);
      if (fabs((double)delta) < 0.5)
        b3dColorIndex(App->curpoint);
      else
        b3dColorIndex(App->shadow);
      getWindowCoords(mVi->xmouse, mVi->ymouse, mVi->zmouse,
                      xim, yim, zim);
      b3dDrawPlus(xim, yim, imPtSize);
    }
  } else {

    // Otherwise draw the current model point - change size if needed, draw
    // as shadow if off-time or out of plane
    b3dLineWidth(obj->linewidth2);
    curSize = modPtSize;
    if (cont->psize > 1 && 
        (pnt == cont->pts || pnt == cont->pts + cont->psize - 1))
      curSize = backupSize;
    delta = norm.x * (pnt->x - mCx) + norm.y * (pnt->y - mCy) + 
      norm.z * (pnt->z - mCz);
    if (fabs((double)delta) < 0.5 * mDepth  &&
        !ivwTimeMismatch(vi, mTimeLock, obj, cont))
      b3dColorIndex(App->curpoint);
    else
      b3dColorIndex(App->shadow);
    getWindowCoords(pnt->x, pnt->y, pnt->z, xim, yim, zim);
    b3dDrawCircle(xim, yim, curSize);
  }

  // Draw beginning and end points of current contour
  if (cont && !ivwTimeMismatch(vi, mTimeLock, obj, cont) && 
      cont->psize > 1) {
    b3dLineWidth(obj->linewidth2);
    pnt = cont->pts;
    delta = norm.x * (pnt->x - mCx) + norm.y * (pnt->y - mCy) +
      norm.z * (pnt->z - mCz);
    if (fabs((double)delta) < 0.5 * mDepth) {
      b3dColorIndex(App->bgnpoint);
      getWindowCoords(pnt->x, pnt->y, pnt->z, xim, yim, zim);
      b3dDrawCircle(xim, yim, modPtSize);
    }
    pnt = &cont->pts[cont->psize - 1];
    delta = norm.x * (pnt->x - mCx) + norm.y * (pnt->y - mCy) +
      norm.z * (pnt->z - mCz);
    if (fabs((double)delta) < 0.5 * mDepth) {
      b3dColorIndex(App->endpoint);
      getWindowCoords(pnt->x, pnt->y, pnt->z, xim, yim, zim);
      b3dDrawCircle(xim, yim, modPtSize);
    }
  }
  b3dLineWidth(1);
}

/* Model drawing routine */
void SlicerFuncs::drawModel()
{
  float depth = mDepth;

  depth = depth * mZoom;
  depth *= 0.5f;

  glMatrixMode(GL_PROJECTION);
     
  glLoadIdentity();

  glOrtho(0.0 , mWinx, 0.0, mWiny, depth, -depth);

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();


  glTranslatef(mWinx*0.5f, mWiny*0.5f, 0.0f);
  /*     if (mScalez == SLICE_ZSCALE_AFTER)
         glScalef(1.0f, 1.0f, mVi->imod->zscale); */
  glScalef(mXzoom, mYzoom, mZoom);

  /* DNM: took away minus signs because inverting sense of angles;
     also had to swap X and Y */
  glRotatef(mTang[b3dX], 1.0f, 0.0f, 0.0f);
  glRotatef(mTang[b3dY], 0.0f, 1.0f, 0.0f);
  glRotatef(mTang[b3dZ], 0.0f, 0.0f, 1.0f);
  
  glScalef(1.0f, 1.0f, getZScaleBefore());
  // if (mScalez == SLICE_ZSCALE_BEFORE)
  //  glScalef(1.0f, 1.0f, mVi->imod->zscale);

  glTranslatef(-mCx, -mCy, -mCz);

  imodDrawModel(mVi, mVi->imod, 0, getZScaleBefore());
  glPopMatrix();
  return;
}

/* cube drawing function call, also a good place to update the view axis slider */
void SlicerFuncs::cubeDraw()
{
  updateViewAxisPos();
  mQtWindow->mCube->updateGL();
}

/* The paint routine called by the cube GL widget */
void SlicerFuncs::cubePaint()
{
  double params[4];
  static float v[3], vx[3], vy[3];
  double x, y, z;
  double r;
  float zs  = 1;
  float zoom = 1.0/mZoom;
  int winx, winy;
  float xo, yo, zo;

  if (mClosing)
    return;

  mCube = mQtWindow->mCube;
  b3dSetCurSize(mCube->width(), mCube->height());

  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT);
     
  x = mVi->xsize;
  y = mVi->ysize;
  z = mVi->zsize;

  /* DNM: take window size * zoom as # of pixels displayed */
  xo = mXo;
  yo = mYo;
  zo = mZo;
  winx = (int)(mWinx * zoom);
  winy = (int)(mWiny * zoom);

  // Make r smaller to increase size of cube
  r = 0.7 * sqrt( (x * x) + (y * y) + (z * z));

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  /* DNM: was -r, r for near, far!  These ratios seem to work */
  gluPerspective(45.0, 1., .02*r, 10.*r);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  gluLookAt(x*0.5,  -r,  r*1.5,
            x*0.5, y*0.5, z*0.5, 
            0.0, 0.0, 1.0);


  /* draw the cube */
  glColor4ub(0x00, 0xff, 0xff, 0x00);
  glBegin(GL_LINE_LOOP);
  glVertex3f(0.0, 0.0, 0.0);
  glVertex3f(  x, 0.0, 0.0);
  glVertex3f(  x,   y, 0.0);
  glVertex3f(0.0,   y, 0.0);
  glEnd();

  glBegin(GL_LINE_LOOP);
  glVertex3f(0.0, 0.0, z);
  glVertex3f(  x, 0.0, z);
  glVertex3f(  x,   y, z);
  glVertex3f(0.0,   y, z);
  glEnd();

  glBegin(GL_LINES);
  glVertex3f(0.0, 0.0, 0.0);
  glVertex3f(0.0, 0.0,   z);
  glVertex3f(0.0,   y, 0.0);
  glVertex3f(0.0,   y,   z);
  glVertex3f(  x, 0.0, 0.0);
  glVertex3f(  x, 0.0,   z);
  glVertex3f(  x,   y, 0.0);
  glVertex3f(  x,   y,   z);
  glEnd();

  params[0] = 1.0; params[1] = 0.0; params[2] = 0.0; params[3] = 0.0;
  glClipPlane( GL_CLIP_PLANE0, params);
  params[0] = 0.0; params[1] = 1.0; params[2] = 0.0; params[3] = 0.0;
  glClipPlane( GL_CLIP_PLANE1, params);
  params[0] = 0.0; params[1] = 0.0; params[2] = 1.0; params[3] = 0.0;
  glClipPlane( GL_CLIP_PLANE2, params);
  params[0] = -1.0; params[1] = 0.0; params[2] = 0.0; params[3] = x+1;
  glClipPlane( GL_CLIP_PLANE3, params);
  params[0] = 0.0; params[1] = -1.0; params[2] = 0.0; params[3] = y+1;
  glClipPlane( GL_CLIP_PLANE4, params);
  params[0] = 0.0; params[1] = 0.0; params[2] = -1.0; params[3] = z+1;
  glClipPlane( GL_CLIP_PLANE5, params);
     
  vx[0] = xo + (mXstep[0] * winx);
  vx[1] = yo + (mXstep[1] * winx);
  vx[2] = zo + (mXstep[2] * winx * zs);

  vy[0] = xo + (mYstep[0] * winy);
  vy[1] = yo + (mYstep[1] * winy);
  vy[2] = zo + (mYstep[2] * winy * zs);

  v[0]  = xo + (mXstep[0] * winx) 
    + (mYstep[0] * winy);
  v[1]  = yo + (mXstep[1] * winx) 
    + (mYstep[1] * winy);
  v[2]  = zo + (mXstep[2] * winx * zs) 
    + (mYstep[2] * winy * zs);

  /*
  imodPrintStderr("xo,yo,zo %.0f %.0f %0.f\n", xo, yo, zo);
  imodPrintStderr("vx %.0f %.0f %0.f\n", vx[0], vx[1], vx[2]);
  imodPrintStderr("vy %.0f %.0f %0.f\n", vy[0], vy[1], vy[2]);
  imodPrintStderr("v %.0f %.0f %0.f\n", v[0], v[1], v[2]);
  */

  glBlendFunc(GL_SRC_ALPHA,  GL_ONE_MINUS_SRC_ALPHA); 
  glEnable(GL_BLEND); 

  glColor4ub(0xff, 0x00, 0x00, 0x7f);
  glEnable(GL_CLIP_PLANE0);
  glEnable(GL_CLIP_PLANE1);
  glEnable(GL_CLIP_PLANE2);
  glEnable(GL_CLIP_PLANE3);
  glEnable(GL_CLIP_PLANE4);
  glEnable(GL_CLIP_PLANE5);

  glBegin(GL_POLYGON);
  glVertex3f(xo, yo, zo);
  glVertex3fv(vx);
  glVertex3fv(v);
  glVertex3fv(vy);
  glEnd();

  glBlendFunc(GL_ONE,  GL_ZERO);
  glDisable(GL_BLEND);
  glDisable(GL_CLIP_PLANE0);
  glDisable(GL_CLIP_PLANE1);
  glDisable(GL_CLIP_PLANE2);
  glDisable(GL_CLIP_PLANE3);
  glDisable(GL_CLIP_PLANE4);
  glDisable(GL_CLIP_PLANE5);

  glFlush();

}
