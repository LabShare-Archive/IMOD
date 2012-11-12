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
#include "hottoolbar.h"
#include "imod.h"
#include "display.h"
#include "b3dgfx.h"
#include "sslice.h"
#include "imod_input.h"
#include "info_cb.h"
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


/* internal functions. */
static void slicerKey_cb(ImodView *vi, void *client, int released, QKeyEvent *e);
static void slicerDraw_cb(ImodView *vi, void *client, int drawflag);
static void slicerClose_cb(ImodView *vi, void *client, int junk);
static void notifySlicersOfAngDia(bool open);
static void setAngleToolbarState(SlicerWindow *slicer, bool open);

/* DNM: maximum angles for sliders */
static float maxAngle[3] = {90.0, 180.0, 180.0};
static int ctrlPressed = 0;
static bool pixelViewOpen = false;
static SlicerAngleForm *sliceAngDia = NULL;
static int sliderDragging = 0; 
static QTime but1downt;
static int firstmx, firstmy, lastmx, lastmy;
static int mousePanning = 0;
static int mouseRotating = 0;
static bool mousePressed = false;
static float viewAxisSteps[] = {0.1f, 0.3f, 1., 3., 10., 0.};
static int viewAxisIndex = 2;
static bool doingMontage = false;
static int scaleThick = 1;

/*
 * Open new slicer.
 */
int sslice_open(ImodView *vi)
{
  SlicerFuncs *ss;

  ss = new SlicerFuncs(vi);
  if (!ss) {
    delete ss;
    wprint("Error opening slicer window.");
    return(-1);
  }
  return(0);
}

// Open or raise the slicer angle window
int slicerAnglesOpen()
{
  if (sliceAngDia) {
    sliceAngDia->raise();
    return 0;
  }
  sliceAngDia = new SlicerAngleForm(imodDialogManager.parent(IMOD_DIALOG), 
                                    Qt::Window);
  if (!sliceAngDia)
    return -1;
  imodDialogManager.add((QWidget *)sliceAngDia, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)sliceAngDia, IMOD_DIALOG);
  notifySlicersOfAngDia(true);
  return 0;
}

// The slicer angle window is closing
void slicerAnglesClosing()
{
  imodDialogManager.remove((QWidget *)sliceAngDia);
  sliceAngDia = NULL;
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
  pixelViewOpen = state;
  QObjectList objList;
  SlicerWindow *slicer;
  int i;

  pixelViewOpen = state;
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

// Find the slicer window that is nearest to the top of the control list
SlicerFuncs *getTopSlicer()
{
  QObject *obj = imodDialogManager.getTopWindow(SLICER_WINDOW_TYPE);
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
  ss->mTang[b3dX] = angles[0];
  ss->mTang[b3dY] = angles[1];
  ss->mTang[b3dZ] = angles[2];
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
// slicer angle window
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
  time = ss->mTimeLock ? ss->mTimeLock : ss->mVi->ct;
  return 0;
}

// Just return the time of the top slicer window and the continuous state
int getTopSlicerTime(bool &continuous)
{
  SlicerFuncs *ss = getTopSlicer();
  if (!ss)
    return -1;
  continuous = ss->mContinuous;
  return ss->mTimeLock ? ss->mTimeLock : ss->mVi->ct;
}

// Notify the slicer angle dialog of a new time or general need to refresh
void slicerNewTime(bool refresh)
{
  if (sliceAngDia)
    sliceAngDia->newTime(refresh);
  if (imodvLinkedToSlicer())
    imodv_draw();
}

int getSlicerThicknessScaling()
{
  return scaleThick;
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
SlicerFuncs::SlicerFuncs(ImodView *vi)
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
  if (!mClassic && !ImodPrefs->classicWarned())
    dia_puts("Welcome to the new slicer mode, in which you can pan\n"
             "with the mouse but clicking does not recenter the image.\n"
             "You can switch to the classic mode with the centering\n"
             "button (two concentric squares) on the first toolbar.\n"
             "You can select classic mode as the default on the Behavior tab\n"
             "in the 3dmod Preferences dialog, accessed with Edit-Options.\n\n"
             "Also, the "CTRL_STRING" key is now needed to start or stop a "
             "movie with\nthe second or third mouse button.");

  mCx = vi->xmouse;
  mCy = vi->ymouse;
  mCz = vi->zmouse;
  mVi     = vi;
  mLocked = 0;
  mTimeLock = 0;
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
  mMousemode = vi->imod->mousemode;
  mMovieSnapCount = 0;
  mFftMode = 0;
  mToolTime = 0;
  mContinuous = false;
  mLinked = false;
  mClosing = 0;
  mIgnoreCurPtChg = 0;
  mAlreadyDrew = false;
  mNeedDraw = false;

  transStep();
  utilGetLongestTimeString(vi, &str);
  mQtWindow = new SlicerWindow(this, maxAngle,  str, App->rgba, App->doublebuffer,
                               App->qtEnableDepth, imodDialogManager.parent(IMOD_IMAGE));
  if (!mQtWindow)
    return;

  mGlw = mQtWindow->mGLw;
  mCube = mQtWindow->mCube;
  if (!App->rgba)
    mGlw->setColormap(*(App->qColormap));

  mQtWindow->setWindowTitle(imodCaption("3dmod Slicer"));
  mQtWindow->mToolBar->setWindowTitle(imodCaption("Slicer Toolbar 1"));
  mQtWindow->mToolBar2->setWindowTitle(imodCaption("Slicer Toolbar 2"));
  mQtWindow->mSaveAngBar->setWindowTitle(imodCaption("Slicer Toolbar 3"));
  if (mQtWindow->mTimeBar)
    mQtWindow->mTimeBar->setWindowTitle(imodCaption("Slicer Time Toolbar"));
	
  mCtrl = ivwNewControl(vi, slicerDraw_cb, slicerClose_cb, slicerKey_cb, (void *)this);
  imodDialogManager.add((QWidget *)mQtWindow, IMOD_IMAGE, SLICER_WINDOW_TYPE);

  // Set up cursor
  if (mMousemode == IMOD_MMODEL)
    mGlw->setCursor(*App->modelCursor);

  // Initialize controls
  mQtWindow->setZoomText(mZoom);
  drawThickControls();
  mQtWindow->setAngles(mTang);

  // Include this to get toolbar sizes right
  imod_info_input();

  QSize toolSize1 = mQtWindow->mToolBar->sizeHint();
  QSize toolSize2 = mQtWindow->mToolBar2->sizeHint();
  mQtWindow->mToolBar2->setMaximumWidth(toolSize2.width() + 10);
  int newWidth = toolSize1.width() > toolSize2.width() ?
    toolSize1.width() : toolSize2.width();
  int newHeight = newWidth + toolSize1.height() + toolSize2.height();
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
    mQtWindow->setZoomText(mZoom);
    if (mZoom > 1.5) {
      mHq = 1;
      mQtWindow->setToggleState(0, 1);
    }
  }
  
  setAngleToolbarState(mQtWindow, sliceAngDia != NULL);

  adjustGeometryAndShow((QWidget *)mQtWindow, IMOD_IMAGE, false);
  mGlw->setMouseTracking(pixelViewOpen);
}

void SlicerFuncs::externalDraw(ImodView *vi, int drawflag)
{
  float usex, usey, usez, factor = 0.;
  int ignoreChg = mIgnoreCurPtChg;

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
  if (vi->imod->mousemode != mMousemode) {
    mMousemode = vi->imod->mousemode;
    if (mMousemode == IMOD_MMODEL)
      mGlw->setCursor(*App->modelCursor);
    else
      mGlw->unsetCursor();
  }

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
    if (vi->xmovie && fabs((double)mZstep[b3dX]) > 1.e-6)
      factor = (vi->xmouse - mCx) / mZstep[b3dX];
    else if (vi->ymovie && fabs((double)mZstep[b3dY]) > 1.e-6)
      factor = (vi->ymouse - mCy) / mZstep[b3dY];
    else if (vi->zmovie && fabs((double)mZstep[b3dZ]) > 1.e-6)
      factor = (vi->zmouse - mCz) / mZstep[b3dZ];

    /*imodPrintStderr("%d %d %d factor %f mouse %.1f %.1f %.1f  "
            "cur %.1f %.1f %.1f\n", vi->xmovie, 
            vi->ymovie, vi->zmovie, factor, vi->xmouse, 
            vi->ymouse, vi->zmouse, mCx, mCy, mCz); */

    // Compute new position and bound it (may not be needed unless user
    // clicks a new position during movie)
    if (factor != 0.) {
      vi->xmouse = mCx + factor * mZstep[b3dX];
      vi->ymouse = mCy + factor * mZstep[b3dY];
      mCz += factor * mZstep[b3dZ];
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
        if (imcGetSlicerMontage(true))
          montageSnapshot(imcGetSnapshot(vi));
        else
          b3dKeySnapshot("slicer", imcGetSnapshot(vi) - 1, 
                         imcGetSnapshot(vi) % 2, NULL);
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

  case SLICER_TOGGLE_FFT:
    mFftMode = state;
    draw();
    break;

  case SLICER_TOGGLE_TIMELOCK:
    mTimeLock = state ? mVi->ct : 0;
    if (!state)
      draw();
    break;
  }
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

// Selection of a new zscaling option
void SlicerFuncs::Zscale(int item)
{
  ivwControlPriority(mVi, mCtrl);
  mScalez = item;
  draw();
}


/* 
 * Tilt angle controls.
 */
void SlicerFuncs::angleChanged(int axis, int value, 
			int dragging)
{
  ivwControlPriority(mVi, mCtrl);
  setForwardMatrix();
  mTang[axis] = value * 0.1f;
  mLastangle = axis;
  sliderDragging = dragging;
  changeCenterIfLinked();

  // Do complete redraw if not dragging or hot slider enabled
  if (!dragging || ImodPrefs->hotSliderActive(ctrlPressed)) {
    mDrawModView = imodvLinkedToSlicer() ? IMOD_DRAW_MOD : 0;
    synchronizeSlicers();
    showSlice();
    checkMovieLimits();
  } else {

    // Otherwise, just draw the cube
    transStep();
    cubeDraw();
  }
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
  draw();
}


/* The window is closing now.  Clean up */
void SlicerFuncs::closing()
{
  mClosing = 1;
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
    if (sso->mNslice != mNslice) {
      sso->mNslice = mNslice;
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

// Tell the slicer angle dialog to set current angles into current row, or
// into new row if necessary or if flag is set
void SlicerFuncs::setCurrentOrNewRow(bool newrow)
{
  ivwControlPriority(mVi, mCtrl);
  sliceAngDia->setCurrentOrNewRow(mTimeLock ? mTimeLock : mVi->ct,
                                  newrow);
}

// Tell the slicer angle dialog to set the current row into the top slicer
void SlicerFuncs::setAnglesFromRow()
{
  ivwControlPriority(mVi, mCtrl);
  sliceAngDia->setAnglesFromRow(mTimeLock ? mTimeLock : mVi->ct);
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
  float unit, dist3d;
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
  if (keysym == hotSliderKey()) {
    ctrlPressed = true;
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
    wprint("From (%.1f, %.1f, %.1f) to (%.1f, %.1f, %.1f) ="
           " %.1f %spixels, %g %s\n", pnt->x+1., pnt->y+1., pnt->z+1., moupt.x+1., 
           moupt.y+1., moupt.z+1., dist3d, vi->zbin * vi->xybin > 1 ? "unbinned " : "",
           dist3d * vi->imod->pixsize, imodUnits(vi->imod));

    break;


  case Qt::Key_S:
    if (shift || ctrl){
      if (imcGetSlicerMontage(true)) {
        montageSnapshot((ctrl ? 1 : 0) + (shift ? 2 : 0));
      } else {

        // Snapshots: need to update just the image window
        mGlw->updateGL();
        b3dKeySnapshot("slicer", shift, ctrl, NULL);
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
      viewAxisIndex = B3DMAX(0, viewAxisIndex - 1);
    break;

  case Qt::Key_Period:
  case Qt::Key_Greater:
    dodraw = 0;
    if (keysym == Qt::Key_Comma && !mShiftLock)
      handled = 0;
    else if (viewAxisSteps[viewAxisIndex + 1])
      viewAxisIndex++;
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
          
          // For a move in Z, try to move current image point normal to the
          // plane.  First test if this move is a continuation of a sequence
          // and if so just increment the cumulative move; otherwise set this
          // up as the first move of a potential sequence
          if (mTang[0] == mOrigAngles[0] && 
              mTang[1] == mOrigAngles[1] && 
              mTang[2] == mOrigAngles[2] && 
              vi->xmouse == mLastXmouse && 
              vi->ymouse == mLastYmouse && 
              vi->zmouse == mLastZmouse) {
            mCumPageMoves += vec.z;
          } else {
            mOrigAngles[0] = mTang[0];
            mOrigAngles[1] = mTang[1];
            mOrigAngles[2] = mTang[2];
            mOrigZmouse = vi->zmouse;
            mCumPageMoves = vec.z;
          }
          getNormalToPlane(&norm);

          // Now move the zmouse by cumulative move from original position
          // and round z to integer (decided not to move in X/Y)
          vi->zmouse = B3DNINT(mOrigZmouse + mCumPageMoves * norm.z);
          mLastXmouse = vi->xmouse;
          mLastYmouse = vi->ymouse;
          mLastZmouse = vi->zmouse;
          ivwBindMouse(vi);
          synchronizeSlicers();
          imodDraw(vi, IMOD_DRAW_XYZ | IMOD_DRAW_SLICE);
          dodraw = 0;        
        }
      }
      break;
    }

    // Now handle keypad keys
    if (shift || mShiftLock) {
      unit = viewAxisSteps[viewAxisIndex];
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

      if (mTang[lang] > maxAngle[lang])
        mTang[lang] = maxAngle[lang];
      if (mTang[lang] < -maxAngle[lang])
        mTang[lang] = -maxAngle[lang];
      changeCenterIfLinked();
    }

    mQtWindow->setAngles(mTang);
    mDrawModView = imodvLinkedToSlicer() ? IMOD_DRAW_MOD : 0;

    synchronizeSlicers();
    showSlice();
    docheck = 1;
    break;

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
  if (ctrlPressed)
    mQtWindow->releaseKeyboard();
  ctrlPressed = false;
}

// Process press of mouse buttons
void SlicerFuncs::mousePress(QMouseEvent *event)
{
  int shift = (event->modifiers() & Qt::ShiftModifier) + mShiftLock;
  int ctrl = (event->modifiers() & Qt::ControlModifier);
  mousePressed = true;
  ivwControlPriority(mVi, mCtrl);

  utilRaiseIfNeeded(mQtWindow, event);
  if (mMousemode == IMOD_MMODEL && utilNeedToSetCursor())
    mGlw->setCursor(*App->modelCursor);

  lastmx = firstmx = event->x();
  lastmy = firstmy = event->y();
  if (event->buttons() & ImodPrefs->actualButton(1)) {
    if (mClassic) {
      attachPoint(event->x(), event->y(), ctrl);
    } else {
      but1downt.start();
    }
  }

  if ((event->buttons() & ImodPrefs->actualButton(2)) && (ctrl || !shift))
    insertPoint(event->x(), event->y(), ctrl);
  
  if ((event->buttons() & ImodPrefs->actualButton(3)) && (ctrl || !shift))
    modifyPoint(event->x(), event->y(), ctrl);
}

// Respond to mouse button up
void SlicerFuncs::mouseRelease(QMouseEvent *event)
{
  mousePressed = false;
  ivwControlPriority(mVi, mCtrl);

  // For button 1 up, if not classic mode, either end panning or call attach
  if (event->button() == ImodPrefs->actualButton(1) && !mClassic) {
    if (mousePanning) {
      mousePanning = 0;
      drawSelfAndLinked();
    } else
      attachPoint(event->x(), event->y(), event->modifiers() & Qt::ControlModifier);
  }
  if (mouseRotating) {
    mouseRotating = 0;
    drawSelfAndLinked();
  }
}

// Process a mouse move
void SlicerFuncs::mouseMove(QMouseEvent *event)
{
  static int button1, button2, button3, ex, ey, shift, processing = 0;
  int zmouse;
  float xm, ym, zm, angleScale, dx = 0., dy = 0., drot = 0.;
  Ipoint cpt;
  Icont *cont;
  Imod *imod = mVi->imod;
  Ipoint scale = {1., 1., 1.};
  int cumdx, cumdy;
  int cumthresh = 6 * 6;
  bool addingPoints;
  double transFac = mZoom < 4. ? 1. / mZoom : 0.25;
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

  ivwControlPriority(mVi, mCtrl);
  if (pixelViewOpen) {
    zmouse = getxyz(ex, ey, xm, ym, zm);
    pvNewMousePosition(mVi, xm, ym, zmouse);
  }

  // For anything but modeling points, eat any pending move events and use
  // latest position
  addingPoints = button2 && !shift &&
    (mLocked || !mClassic) && imod->mousemode == IMOD_MMODEL;
  if (!addingPoints) {
    processing = 1;
    imod_info_input();
    if (imodDebug('m') && processing > 1)
      imodPrintStderr("Flushed %d move events\n", processing - 1);
    processing = 0;
    
    // If this processed a release, then turn off the buttons
    if (!mousePressed)
      button1 = button2 = button3 = 0;
  }

  // Pan with button 1 if not classic mode
  if (button1 && !mClassic) {
    cumdx = ex - firstmx;
    cumdy = ey - firstmy;
    if (mousePanning || but1downt.elapsed() > 250 || 
        cumdx * cumdx + cumdy * cumdy > cumthresh) {
      vec.x = (lastmx - ex) * transFac;
      vec.y = (ey - lastmy) * transFac;
      vec.z = 0.;
      mousePanning = 1;
      if (translateByRotatedVec(&vec))
        drawSelfAndLinked();
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

  if (shift && (button2 || button3)) {

  // Button 2 shifted, rotate around view axis in X/Y plane
    if (button2) {
      angleScale = 180. / (3.142 * 0.4 * B3DMIN(mWinx, mWiny));
      dy = (ex - lastmx) * angleScale;
      dx = (ey - lastmy) * angleScale;
    } else {

      // Button 3 shifted, rotate around Z view axis
      drot = utilMouseZaxisRotation(mWinx, ex, lastmx,
                                    mWiny, ey, lastmy);

    }
    if (dx <= -0.1 || dx >= 0.1 || dy <= -0.1 || dy >= 0.1 || 
        fabs(drot) >= 0.1) {
      setViewAxisRotation(dx, dy, drot);
      mQtWindow->setAngles(mTang);
      mouseRotating = 1;
      if (imodDebug('s'))
        imodPuts("Mouse rotating");
      mDrawModView = imodvLinkedToSlicer() ? IMOD_DRAW_MOD : 0;
      synchronizeSlicers();
      showSlice();
    }
  }
  lastmx = ex;
  lastmy = ey;
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
  int drawflag = IMOD_DRAW_XYZ;

  vi->zmouse = setxyz(x, y);
  if (imod->mousemode == IMOD_MMODEL) {
    pnt.x = vi->xmouse;
    pnt.y = vi->ymouse;
    pnt.z = vi->zmouse;
    setForwardMatrix();
    indSave = vi->imod->cindex;

    distance = imodAllObjNearest(vi, &index , &pnt, selsize, mMat);
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
  bool newsurf;
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
      if (newsurf || fabs(distsum / cont->psize) > zcrit){
        if (cont->psize == 1) {
          wprint("Started a new contour even though last "
                 "contour had only 1 pt.  ");
          if (iobjClose(obj->flags))
            wprint("\aUse open contours to model across different slices.\n");
          else
            wprint("\aTurn off \"Start new contour at new Z\" to model "
                   "across different slices.\n");
        }
        if (newsurf)
          inputNewSurface(vi);
         else
          inputNewContour(vi);
        cont = imodContourGet(vi->imod);
        if (!cont)
          return;
        if (newsurf)
          wprint("Started new surface # %d due to change in angles\n", 
                 cont->surf);
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

/*
 * MOVIE RELATED ROUTINES
 */

// Find dominant axis of this slicer window and set movie arguments to match
void SlicerFuncs::findMovieAxis(int *xmovie, int *ymovie, 
                                int *zmovie, int dir, int *axis)
{
  *xmovie = *ymovie = *zmovie = 0;
  double xcomp = fabs((double)mZstep[b3dX]);
  double ycomp = fabs((double)mZstep[b3dY]);
  double zcomp = fabs((double)mZstep[b3dZ]);

  if (xcomp >= ycomp && xcomp >= zcomp) {
    *xmovie = dir;
    *axis = 0;
  } else if (ycomp >= zcomp) {
    *ymovie = dir;
    *axis = 1;
  } else {
    *zmovie = dir;
    *axis = 2;
  }
}

// Set special movie limits on the dominant axis that will keep the other two
// axes within bounds
void SlicerFuncs::setMovieLimits(int axis)
{
  int i, size, end, start = -1;
  float cur, cx, cy, cz;

  // Get the size and current value for the dominant axis
  if (axis == 0) {
    size = mVi->xsize;
    cur = mCx;
  } else if (axis == 1) {
    size = mVi->ysize;
    cur = mCy;
  } else {
    size = mVi->zsize;
    cur = mCz;
  }

  // Loop on positions on dominant axis and find position on each axis
  for (i = 0; i < size; i++) {
    cx = mCx + (i - cur) * mZstep[0] / mZstep[axis];
    cy = mCy + (i - cur) * mZstep[1] / mZstep[axis];
    cz = mCz + (i - cur) * mZstep[2] / mZstep[axis];
    if (cx >= 0. && cx <= mVi->xsize - 1 && 
        cy >= 0. && cy <= mVi->ysize - 1 &&
        cz >= 0. && cz <= mVi->zsize - 1) {

      // Set start on first legal position and end on any legal one
      end = i;
      if (start < 0)
        start = i;
    }
  }

  imcSetSpecialLimits(axis, start, end);
}

// Check for movie and set new limits when angles have changed
void SlicerFuncs::checkMovieLimits()
{
  int xmovie, ymovie, zmovie;
  int axis;
  ImodView *vi = mVi;

  // If no movie or it was not started by this slicer, done
  if (!(vi->xmovie || vi->ymovie || vi->zmovie) || 
      imcGetStarterID() != mCtrl)
    return;

  // See if dominant axis has changed and restart movie
  findMovieAxis(&xmovie, &ymovie, &zmovie, 1, &axis);
  if ((vi->xmovie && axis != 0) || (vi->ymovie && axis != 1) || 
      (vi->zmovie && axis != 2)) {

    // If sign of new motion along old axis (xstep[old]/xstep[axis]) is 
    // opposite to sign of old motion (oldmovie), then need to restart
    // with negative direction.  Only one term of old sum counts.
    if ((vi->xmovie * mZstep[b3dX] + vi->ymovie * mZstep[b3dY] + 
         vi->zmovie * mZstep[b3dZ]) / mZstep[axis] < 0.)
      findMovieAxis(&xmovie, &ymovie, &zmovie, -1, &axis);
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
  findMovieAxis(&xmovie, &ymovie, &zmovie, dir, &axis);
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
    scaleThick = imcGetThickScaling();
    if (scaleThick == 1)
      scaleThick = factor;
  }

  // Loop on frames, getting pixels and copying them
  mZoom *= scaleFactor;
  mHq = 1;
  doingMontage = true;
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
  doingMontage = false;
  scaleThick = 1;
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

// Compute the x, y, z coordinates corresponding to a point in window
int SlicerFuncs::getxyz(int x, int y, float &xm, float &ym,
                         float &zm)
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

  xm = B3DMIN(mVi->xsize - 1, B3DMAX(0, xm));
  ym = B3DMIN(mVi->ysize - 1, B3DMAX(0, ym));
  zm = B3DMIN(mVi->zsize - 0.51, B3DMAX(-0.49, zm));

  return((int)floor(zm + 0.5));
}

// Get the window coordinates corresponding to the given image point 
// coordinates as integer values; zim should be 0 if the point is in the 
// current plane
void SlicerFuncs::getWindowCoords(float xcur, float ycur, 
                            float zcur, int &xim, int &yim, int &zim)
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
  zim = B3DNINT(xn.z * xcur + yn.z * ycur + zn.z * zcur);
  xim = B3DNINT(xoffset * mXzoom + mWinx / 2);
  yim = B3DNINT(yoffset * mYzoom + mWiny / 2);
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
   back-rotating it is sclaed downinto original pixel space.  Lateral moves
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

// Return the normal to the current plane.  Multiply the inverse transformation
// of a Z vector by the Z scaling before
void SlicerFuncs::getNormalToPlane(Ipoint *norm)
{
  Ipoint vec = {0., 0., 1.};
  setInverseMatrix();
  imodMatTransform3D(mMat, &vec, norm);
  norm->z *= getZScaleBefore();
  imodPointNormalize(norm);
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
  int i, ival, sliceScaleThresh = 4;
  int mousing = mousePanning + mouseRotating +
    (ImodPrefs->speedupSlider() ? sliderDragging : 0);
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

    // If filling array, first assess mean and SD for scaling multiple slices
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
    if (sliceAngDia && (getTopSlicer() == this))
      sliceAngDia->topSlicerDrawing(mTang, mCx, mCy, mCz, 
                                    mTimeLock ? mTimeLock : mVi->ct, 
                                    sliderDragging + mousing, mContinuous);
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

  // Update toolbar for time
  if (mVi->nt) {
    int time = mTimeLock ? mTimeLock : mVi->ct;
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
  if (!doingMontage)
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

/* cube drawing function call */
void SlicerFuncs::cubeDraw()
{
  mCube->updateGL();
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
