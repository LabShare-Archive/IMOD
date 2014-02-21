/*
 *  xzap.cpp -- The Zap Window.
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
#include <qcursor.h>
#include <qdatetime.h>
#include <qapplication.h>
#include <qclipboard.h>
#include <qpoint.h>
#include <qpainter.h>
#include <qpen.h>
#include <qtimer.h>
#include <qobject.h>
//Added by qt3to4:
#include <QWheelEvent>
#include <QMouseEvent>
#include <QKeyEvent>
#include <QEvent>
#include "zap_classes.h"
#include "hottoolbar.h"

#include "imod.h"
#include "mv_image.h"
#include "display.h"
#include "b3dgfx.h"
#include "xcramp.h"
#include "xzap.h"
#include "xxyz.h"
#include "pixelview.h"
#include "xgraph.h"
#include "locator.h"
#include "control.h"
#include "imodplug.h"
#include "info_setup.h"
#include "info_cb.h"
#include "imod_input.h"
#include "moviecon.h"
#include "autox.h"
#include "imod_edit.h"
#include "cont_edit.h"
#include "model_edit.h"
#include "workprocs.h"
#include "pyramidcache.h"
#include "dia_qtutils.h"
#include "preferences.h"
#include "undoredo.h"
#include "finegrain.h"
#include "scalebar.h"

static void zapDraw_cb(ImodView *vi, void *client, int drawflag);

static void zapClose_cb(ImodView *vi, void *client, int drawflag);
static void zapKey_cb(ImodView *vi, void *client, int released, QKeyEvent *e);

static int sDragRegisterSize = 10;
static int sHqDrawTimeCrit = 100;

/* DNM 1/19/01: add this to allow key to substitute for middle mouse button */
static int sInsertDown = 0;
static bool sPixelViewOpen = false;

static QTime sInsertTime;
static QTime sBut1downt;

static int sNumZapWindows = 0;
static int sSubStartX = 0;
static int sSubStartY = 0;
static int sSubEndX = 0;
static int sSubEndY = 0;

static int sFirstDrag = 0;
static int sMoveBandLasso = 0;
static int sDragBandLasso;
static int sDragging[4];
static int sFirstmx, sFirstmy;
static int sMaxMultiZarea = 0;
static int sScaleSizes = 1;
static bool sMousePressed = false;
static int sNextOpenHQstate = -1;

/*
 * Open the zap window
 */
int imod_zap_open(ImodView *vi, int wintype)
{
  ZapFuncs *zap;

  zap = new ZapFuncs(vi, wintype);
  if (!zap || !zap->mQtWindow) {
    delete zap;
    return (-1);
  }
  return 0;
}

/*
 * Look through all multiZ windows and report params of biggest one
 */
void zapReportBiggestMultiZ()
{
  QObjectList objList;
  QRect pos;
  ZapFuncs *zap;
  int i;
  if (!ImodPrefs)
    return;
  imodDialogManager.windowList(&objList, -1, MULTIZ_WINDOW_TYPE);

  for (i = 0; i < objList.count(); i++) {
    zap = ((ZapWindow *)objList.at(i))->mZap;
    if (sMaxMultiZarea < zap->mWinx * zap->mWiny) {
      sMaxMultiZarea = zap->mWinx * zap->mWiny;
      pos = ivwRestorableGeometry(zap->mQtWindow);
      ImodPrefs->recordMultiZparams(pos, zap->mNumXpanels, zap->mNumYpanels,
                                    zap->mPanelZstep, zap->mDrawInCenter,
                                    zap->mDrawInOthers);
    }
  }
}

/*
 * Find the first zap window of the given type (default regular zap), with a rubberband 
 * if flag is set, or with a lasso if that flag is set (default false) or with either one
 * if both flags are set.  Return the window list index in *index if index is nonNULL.
 */
ZapFuncs *getTopZapWindow(bool withBand, bool withLasso, int type, int *index)
{
  QObject *object = imodDialogManager.getTopWindow(withBand, withLasso, type, index);
  if (!object)
    return NULL;
  return ((ZapWindow *)object)->mZap;
}

/*
 * Return the lasso contour from the top with lasso; but if aboveBand is true,
 * return it only if is higher than any window with a rubberband.
 */
Icont *getTopZapLassoContour(bool aboveBand)
{
  ZapFuncs *zap = getTopZapWindow(aboveBand, true);

  if (!zap || !zap->mLassoOn || zap->mDrawingLasso)
    return NULL;
  return zap->getLassoContour();
}

/*
 * Report the rubberband coordinates of the first zap window with a band
 */
void zapReportRubberband()
{
  ZapFuncs *zap;
  int ixl, ixr, iyb, iyt, bin;
  int lowSection, highSection;

  zap = getTopZapWindow(true);
  if (!zap) {
    imodPrintStderr("ERROR: No Zap window has usable rubberband coordinates\n");
    return;
  }

  bin = zap->mVi->xybin;
  if (zap->mRubberband) {
    ixl = (int)floor(zap->mRbImageX0 + 0.5);
    ixr = (int)floor(zap->mRbImageX1 - 0.5);
    iyb = (int)floor(zap->mRbImageY0 + 0.5);
    iyt = (int)floor(zap->mRbImageY1 - 0.5);
  } else {

    // If band is just statring, report the full area
    ixl = 0;
    iyb = 0;
    ixr = zap->mVi->xsize - 1;
    iyt = zap->mVi->ysize - 1;
  }
  
  if (ixl < 0)
    ixl = 0;
  if (ixr >= zap->mVi->xsize)
    ixr = zap->mVi->xsize - 1;
  if (iyb < 0)
    iyb = 0;
  if (iyt >= zap->mVi->ysize)
    iyt = zap->mVi->ysize - 1;
  ixl *= bin;
  iyb *= bin;
  ixr = ixr * bin + bin - 1;
  iyt = iyt * bin + bin - 1;
  if (zap->getLowHighSection(lowSection, highSection)) {
    imodPrintStderr("Rubberband: %d %d %d %d %d %d\n", ixl + 1, iyb + 1,
                    ixr + 1, iyt + 1, zap->mVi->zbin * (lowSection - 1) + 1,
                     zap->mVi->zbin * highSection);
  }
  else {
    imodPrintStderr("Rubberband: %d %d %d %d\n", ixl + 1, iyb + 1, ixr + 1,
                    iyt + 1);
  }
}

/*
 * Return coordinates of first rubber band; return value 1 if any, 0 if none
 */
int zapRubberbandCoords(float &rbX0, float &rbX1, float &rbY0, float &rbY1)
{
  QObjectList objList;
  ZapFuncs *zap;
  int i;

  imodDialogManager.windowList(&objList, -1, ZAP_WINDOW_TYPE);

  for (i = 0; i < objList.count(); i++) {
    zap = ((ZapWindow *)objList.at(i))->mZap;
    if (zap->mRubberband) {
      rbX0 = zap->mRbImageX0;
      rbX1 = zap->mRbImageX1;
      rbY0 = zap->mRbImageY0;
      rbY1 = zap->mRbImageY1;
      return 1;
    }
  }
  return 0;
}

/*
 * Reposition image center or rubber band to the given absolute position,
 * or by the given increments if the flag is set
 */
void zapSetImageOrBandCenter(float imx, float imy, bool incremental)
{
  ZapFuncs *zap;
  zap = getTopZapWindow(false);
  if (!zap)
    return;
  if (zap->mRubberband) {

    // Rubberband: get desired shift if not incremental, try to do it
    if (!incremental) {
      imx -= (zap->mRbImageX1 + zap->mRbImageX0) / 2.;
      imy -= (zap->mRbImageY1 + zap->mRbImageY0) / 2.;
    }
    zap->shiftRubberband(imx, imy);

    // And center image on rubberband
    zap->mXtrans = B3DNINT(zap->mVi->xsize / 2. - 
                          (zap->mRbImageX1 + zap->mRbImageX0) / 2.);
    zap->mYtrans = B3DNINT(zap->mVi->ysize / 2. - 
                          (zap->mRbImageY1 + zap->mRbImageY0) / 2.);
    zap->mBandChanged = 1;
  } else {

    // Not rubberband: just adjust or set the translations, which will be
    // fixed when the draw is done
    if (incremental) {
      zap->mXtrans -= B3DNINT(imx);
      zap->mYtrans -= B3DNINT(imy);
    } else {
      zap->mXtrans = B3DNINT(zap->mVi->xsize / 2. - imx);
      zap->mYtrans = B3DNINT(zap->mVi->ysize / 2. - imy);
    }
  }
  zap->mRecordSubarea = 1;
  zap->draw();
}

/*
 * The pixel view window has opened or closed, set mouse tracking for all zaps
 */
void zapPixelViewState(bool state)
{
  sPixelViewOpen = state;
  zapSetMouseTracking();
}

/*
 * The state of externally requested tracking has changed somehow, set 
 * tracking for all zaps
 */
void zapSetMouseTracking()
{
  QObjectList objList;
  ZapFuncs *zap;
  int i;
  imodDialogManager.windowList(&objList, -1, ZAP_WINDOW_TYPE);

  for (i = 0; i < objList.count(); i++) {
    zap = ((ZapWindow *)objList.at(i))->mZap;
    zap->setMouseTracking();
  }
}

/* 
 * Return the image coordinates of the mouse in the top Zap
 */
int getTopZapMouse(Ipoint *imagePt)
{
  int mx, my, iz;
  ZapFuncs *zap = getTopZapWindow(false);
  if (!zap)
    return 1;
  mx = zap->mGfx->mapFromGlobal(QCursor::pos()).x();
  my = zap->mGfx->mapFromGlobal(QCursor::pos()).y();
  zap->getixy(mx, my, imagePt->x, imagePt->y, iz);
  imagePt->z = (float)iz;
  return 0;
}

/*
 * Return the subset limits from the active window
 */
int zapSubsetLimits(ViewInfo *vi, int &ixStart, int &iyStart, int &nxUse, int &nyUse)
{
  if (sNumZapWindows <= 0 || sSubStartX >= sSubEndX || sSubStartY >= sSubEndY ||
      sSubEndX >= vi->xsize || sSubEndY >= vi->ysize)
    return 1;
  ixStart = sSubStartX;
  nxUse = sSubEndX + 1 - sSubStartX;
  iyStart = sSubStartY;
  nyUse = sSubEndY + 1 - sSubStartY;
  return 0;
}

/* This sets a flag for the HQ state of the next opened window */
void zapSetNextOpenHQstate(int state)
{
  sNextOpenHQstate = state;
}

/*
 * This is the external draw command from the controller
 */
static void zapDraw_cb(ImodView *vi, void *client, int drawflag)
{
  ZapFuncs *zap = (ZapFuncs *)client;
  int *limits;
  int limarr[4];
  int snaptype = imcGetSnapshot(zap->mVi);

  if (imodDebug('z'))
    imodPrintStderr("Zap Draw  flags %x\n", drawflag);

  if (!zap)
    return;
  if ((!zap->mPopup) || (!zap->mGinit)) 
    return;
     
  zap->setCursor(vi->imod->mousemode);

  if (drawflag & IMOD_DRAW_COLORMAP) {
    zap->mGfx->setColormap(*(App->qColormap));
    return;
  }

  // If the rubberband is enabled and a flip is happening, turn off the
  // rubberband.
  if ((zap->mRubberband || zap->mStartingBand) && zap->mToolMaxZ != zap->mVi->zsize)
    zap->toggleRubberband(false);
  // drawTools();

  if (drawflag){
    if (drawflag & IMOD_DRAW_SLICE)
      zap->mShowslice = 1;

    /* DNM: skip this, it is covered by the zapdraw call below and the
       items that it sets are not needed by the flush or sync */
    /* b3dWinset(XtDisplay(zap->gfx), zap->gfx, (XID)zap->context); */

    if (drawflag & IMOD_DRAW_IMAGE){
      zap->flushImage();
    }
          
    if (!(drawflag & IMOD_DRAW_ACTIVE) && !(drawflag & IMOD_DRAW_NOSYNC))
      zap->syncImage(false);

    /* DNM 1/29/03: no more worry about multiple calls */
    zap->draw();

    /* DNM 3/8/01: add autosnapshot when movieing */
    // 3/8/07: make it take montages too
    if (snaptype && zap->mVi->zmovie && 
        zap->mMovieSnapCount && imcGetStarterID() == zap->mCtrl) {
      if (imcGetSnapMontage(true)) {
        zap->montageSnapshot(snaptype);
      } else {
        zap->setSnapshotLimits(&limits, limarr);
        b3dKeySnapshot((char *)(zap->mNumXpanels ? "multiz" : "zap"), 
                       snaptype - 1, snaptype % 2, limits);
      }
      zap->mMovieSnapCount--;
      /* When count expires, stop movie */
      if(!zap->mMovieSnapCount) {
        zap->mVi->zmovie = 0;
        b3dSetMovieSnapping(false);
      }
    }

    // If there is only one zap window, set flag to record the subarea
    if (imodDialogManager.windowCount(ZAP_WINDOW_TYPE) == 1 && 
        !zap->mNumXpanels)
      zap->mRecordSubarea = 1;
  }
}

/*
 * This receives the close signal back from the controller, tells the
 * window to close, and sets the closing flag
 */
static void zapClose_cb(ImodView *vi, void *client, int junk)
{
  ZapFuncs *zap = (ZapFuncs *)client;
  if (imodDebug('z'))
    imodPrintStderr("Sending zap window close.\n");
  zap->mPopup = 0;
  zap->mQtWindow->close();
}

/*
 * External key is passed on
 */
static void zapKey_cb(ImodView *vi, void *client, int released, QKeyEvent *e)
{
  ZapFuncs *zap = (ZapFuncs *)client;
  if ((e->modifiers() & Qt::KeypadModifier) && (e->key() == Qt::Key_Insert ||
                                    e->key() == Qt::Key_0))
    return;
  if (released)
    zap->keyRelease(e);
  else
    zap->keyInput(e);
}

/*****************************************************************************/
/* THE ZAP FUNCTION CLASS */

ZapFuncs::ZapFuncs(ImodView *vi, int wintype)
{
  QString str;
  QRect oldGeom;
  int needWinx, needWiny;
  int maxWinx;
  int maxWiny;
  int i, newWidth, newHeight, xleft, ytop, toolHeight;
  double newZoom;

  mVi     = vi;
  mCtrl   = 0;
  mXtrans = mYtrans = 0;
  mZtrans = 0;
  if (sNextOpenHQstate >= 0)
    mHqgfx = sNextOpenHQstate ? 1 : 0;
  else
    mHqgfx  = ImodPrefs->startInHQ() ? 1 : 0;
  sNextOpenHQstate = -1;
  mLastHqDrawTime = 0;
  mHide   = 0;
  mZoom   = 1.0;
  mData   = NULL;
  mImage  = NULL;
  mTessCont = NULL;
  mGinit  = 0;
  mLock   = 0;
  mKeepcentered = 0;
  mInsertmode = 0;
  mToolstart = 0;
  mShowslice = 0;
  mShowedSlice = 0;
  mTimeLock = 0;
  mToolSection = -1;
  mToolMaxZ = vi->zsize;
  mToolZoom = -1.0f;
  mToolTime = 0;
  mToolSizeX = 0;
  mToolSizeY = 0;
  mTwod = (!(vi->dim&4));
  mSectionStep = 0;
  mTime = 0;
  mOverlay = 0;
  mMousemode = 0;
  mLastShape = -1;
  mRubberband = 0;
  mStartingBand = 0;
  mShiftingCont = 0;
  mBandChanged = 0;
  mLassoOn = false;
  mDrawingLasso = false;
  mArrowOn = false;
  mDrawingArrow = false;
  mShiftRegistered = 0;
  mCenterMarked = 0;
  mXformFixedPt.x = 0.;
  mXformFixedPt.y = 0.;
  mMovieSnapCount = 0;
  mDrawCurrentOnly = 0;
  mDragAddCount = 0;
  mDrewExtraCursor = false;
  mNumXpanels = wintype ? 5 : 0;
  mNumYpanels = 1;
  mPanelZstep = 1;
  mDrawInCenter = 1;
  mDrawInOthers = 1;
  mPanelGutter = 8;
  mQtWindow = NULL;

  if (wintype) {
    mImages = (B3dCIImage **)malloc
      (MULTIZ_MAX_PANELS * MULTIZ_MAX_PANELS * sizeof(B3dCIImage *));
    mNumImages = 0;
    if (!mImages) {
      wprint("\aError getting memory for array of image pointers.\n");
      return;
    }
    for (i = 0; i < MULTIZ_MAX_PANELS * MULTIZ_MAX_PANELS; i++)
      mImages[i] = NULL;
    oldGeom = ImodPrefs->getMultiZparams(mNumXpanels, mNumYpanels,
                                         mPanelZstep, mDrawInCenter, 
                                         mDrawInOthers);
  }

  utilGetLongestTimeString(mVi, &str);
  mQtWindow = new ZapWindow(this, str, wintype != 0, App->rgba, 
                                App->doublebuffer, App->qtEnableDepth, 
                                imodDialogManager.parent(IMOD_IMAGE),
                                "zap window");
  if (!mQtWindow){
    free(mImages);
    wprint("\aError opening zap window.\n");
    return;
  }
  if (imodDebug('z'))
    imodPuts("Got a zap window");
  mQtWindow->setToggleState(ZAP_TOGGLE_RESOL, mHqgfx);

  mGfx = mQtWindow->mGLw;
  if (!App->rgba)
    mGfx->setColormap(*(App->qColormap));

  mQtWindow->setWindowTitle
    (imodCaption((char *)(wintype ? "3dmod Multi-Z Window" : 
                          "3dmod ZaP Window")));

  mQtWindow->mToolBar->setWindowTitle(imodCaption("ZaP Toolbar"));
  if (mQtWindow->mToolBar2)
    mQtWindow->mToolBar2->setWindowTitle(imodCaption("Time Toolbar"));
  if (mQtWindow->mPanelBar)
    mQtWindow->mPanelBar->setWindowTitle(imodCaption("Multi-Z Toolbar"));
  
  mCtrl = ivwNewControl(vi, zapDraw_cb, zapClose_cb, zapKey_cb, (void *)this);
  imodDialogManager.add((QWidget *)mQtWindow, IMOD_IMAGE, 
                        wintype ? MULTIZ_WINDOW_TYPE : ZAP_WINDOW_TYPE, mCtrl);

  if (!wintype) {
    diaMaximumWindowSize(maxWinx, maxWiny);
    mQtWindow->setSizeText(maxWinx, maxWiny);

    oldGeom = ImodPrefs->getZapGeometry();
  }

  /* 1/28/03: this call is needed to get the toolbar size hint right */
  imod_info_input();
  QSize toolSize = mQtWindow->mToolBar->sizeHint();
  QSize toolSize2(0, 0);
  QSize toolSize3(0, 0);
  if (mQtWindow->mToolBar2)
    toolSize2 = mQtWindow->mToolBar2->sizeHint();
  if (wintype)
    toolSize3 = mQtWindow->mPanelBar->sizeHint();
  toolHeight = toolSize.height();
  if (imodDebug('z'))
    imodPrintStderr("Toolsize %d %d win %d gfx %d\n", toolSize.width(), 
                    toolSize.height(), mQtWindow->height(), 
                    mGfx->height());
  if (!oldGeom.width()) {

    if (!wintype) {
      // If no old geometry, adjust zoom if necessary to fit image on screen

      for (i = 0; i < 2; i++) {
        mZoom = 1.;
        while ((mZoom * vi->xsize > 1.1 * maxWinx || 
                mZoom * vi->ysize > 1.1 * maxWiny - toolHeight)) {
          newZoom = b3dStepPixelZoom(mZoom, -1);
          if (fabs(newZoom - mZoom) < 0.0001)
            break;
          if (mVi->pyrCache && mVi->pyrCache->zoomRequiresBigLoad(newZoom, maxWinx,
                                                                  maxWiny - toolHeight))
            break;
          mZoom = (float)newZoom;
        }
      
        needWinx = (int)(mZoom * vi->xsize);
      
        // If Window is narrower than two toolbars, set up to stack the
        // toolbars and increase the tool height
        if (!i && needWinx < toolSize.width() + toolSize2.width()) {
          mQtWindow->insertToolBarBreak(mQtWindow->mToolBar2);
          toolHeight += toolSize2.height();
        } 
      }
      
      needWiny = (int)(mZoom * vi->ysize) + toolHeight;
      diaLimitWindowSize(needWinx, needWiny);

      // Make the width big enough for the toolbar, and add the difference
      // between the window and image widget heights to get height
      newWidth = toolSize.width() > needWinx ? toolSize.width() : needWinx;
      newHeight = needWiny;
    } else {

      // For multiZ, just make it big enough for two bars, then if
      // necessary insert break and make it taller for the panel bar
      newWidth = B3DMAX(640, toolSize.width() + toolSize2.width());
      newHeight = 170;
      if (newWidth < toolSize.width() + toolSize2.width() + toolSize3.width()){
        newHeight += toolSize3.height();
        mQtWindow->insertToolBarBreak(mQtWindow->mPanelBar);
      }
    }

    QRect pos = mQtWindow->geometry();
    xleft = pos.x();
    ytop = pos.y();

    diaLimitWindowPos(newWidth, newHeight, xleft, ytop);
    if (imodDebug('z'))
      imodPrintStderr("Sizes: zap %d %d, toolbar %d %d, GL %d %d: "
                      "resize %d %d\n", mQtWindow->width(), 
                      mQtWindow->height(), 
                      toolSize.width(), toolSize.height(), mGfx->width(), 
                      mGfx->height(), newWidth, newHeight);

  } else {

    // Existing geometry - better fit it to current screen
    xleft = oldGeom.x();
    ytop = oldGeom.y();
    newWidth = oldGeom.width();
    newHeight = oldGeom.height();
    diaLimitWindowSize(newWidth, newHeight);
    diaLimitWindowPos(newWidth, newHeight, xleft, ytop);

    // Adjust the tool height: see if time bar fits on line and insert break
    // if not and add to height
    int toolBase = toolSize.width();
    if (mQtWindow->mToolBar2) {
      if (newWidth < toolBase + toolSize2.width()) {
        mQtWindow->insertToolBarBreak(mQtWindow->mToolBar2);
        toolBase = toolSize2.width();
        toolHeight += toolSize2.height();
      } else
        toolBase += toolSize2.width();
    }

    // Then see if panel bar fits on line, insert break if not, add to height
    if (wintype && (newWidth < toolBase + toolSize3.width())) {
      mQtWindow->insertToolBarBreak(mQtWindow->mPanelBar);
      toolHeight += toolSize3.height();
    }

    if (!wintype) {

      needWinx = newWidth;
      needWiny = newHeight - toolHeight;
      
      // If images are too big, zoom down until they almost fit
      // If images are too small, start big and find first zoom that fits
      // Apply same overflow criterion so that reopened windows will behave
      // the same as when they were first opened
      if (vi->xsize < needWinx && vi->ysize < needWiny)
        mZoom = (2. * needWinx) / vi->xsize;
      
      while ((mZoom * vi->xsize > 1.1 * needWinx || 
              mZoom * vi->ysize > 1.1 * needWiny)) {
        newZoom = b3dStepPixelZoom(mZoom, -1);
        // This test goes into infinite loop in Windows - Intel, 6/22/04
        // if (newZoom == mZoom)
        if (fabs(newZoom - mZoom) < 0.0001)
          break;
        if (mVi->pyrCache && mVi->pyrCache->zoomRequiresBigLoad(newZoom, needWinx,
                                                                needWiny))
          break;
        mZoom = (float)newZoom;
      }
    }
  }

  // 9/23/03: changed setGeometry to resize/move and this allowed elimination
  // of the setting again on the first real draw
  mQtWindow->resize(newWidth, newHeight);
  mQtWindow->move(xleft, ytop);
  mXzoom = mZoom;
  mQtWindow->show();
  mPopup = 1;
  setMouseTracking();

  // 11/24/03: OS 10.3 needs to move after the show, so just put this in without
  // checking if it would work generally
#ifdef Q_OS_MACX
  mQtWindow->move(xleft, ytop);
#endif    

  if (imodDebug('z'))
    imodPuts("popup a zap dialog");

  /* DNM: set cursor after window created so it has model mode cursor if
     an existing window put us in model mode */
  setCursor(vi->imod->mousemode);
  sNumZapWindows++;
  sInsertTime.start();
}


void ZapFuncs::help()
{
  if (mNumXpanels)
    imodShowHelpPage("multizap.html#TOP");
  else
    imodShowHelpPage("zap.html#TOP");
}

/*
 * This receives a closing signal from the window 
 */
void ZapFuncs::closing()
{
  if (imodDebug('z'))
    imodPrintStderr("ZapClosing received.\n");

  // Do cleanup
  mPopup = 0;
  ivwRemoveControl(mVi, mCtrl);
  imodDialogManager.remove((QWidget *)mQtWindow);
  if (!mNumXpanels)
    sNumZapWindows--;

  // What for?  flush any events that might refer to this zap
  imod_info_input();     

  if (!mNumXpanels)
    b3dFreeCIImage(mImage);
  else {
    if (!sMaxMultiZarea) {
      QRect pos = ivwRestorableGeometry(mQtWindow);
      ImodPrefs->recordMultiZparams(pos, mNumXpanels, mNumYpanels, 
                                    mPanelZstep, mDrawInCenter, 
                                    mDrawInOthers);
    }
    for (int i = 0; i < mNumImages; i++)
      b3dFreeCIImage(mImages[i]);
    if (mImages)
      free(mImages);
  }
  // TODO: window class must delete after this returns
  //free(zap);
}

/*
 * Start or stop movie and check for whether to start a movie snapshot sequence
 */
int ZapFuncs::startMovieCheckSnap(int dir)
{
  int start, end;

  imodMovieXYZT(mVi, MOVIE_DEFAULT, MOVIE_DEFAULT, dir, MOVIE_DEFAULT);
  imcSetStarterID(mCtrl);

  mMovieSnapCount = 0;
  b3dSetMovieSnapping(false);

  /* done if no movie, or if no snapshots are desired.  */
  if (!mVi->zmovie || !imcGetSnapshot(mVi))
    return 0;
     
  /* Get start and end of loop, compute count */
  imcGetStartEnd(mVi, 2, &start, &end);
  mMovieSnapCount = (end - start) / imcGetIncrement(mVi, 2) + 1;
  if (mMovieSnapCount < 1)
    mMovieSnapCount = 1;

  /* double count for normal mode, leave as is for one-way */
  if (!imcGetLoopMode(mVi))
    mMovieSnapCount *= 2;

  /* Set to start or end depending on which button was hit */
  if (!imcStartSnapHere(mVi))
    mVi->zmouse = dir > 0 ? start : end;

  // Inform autosnapshot not to check file numbers from 0
  b3dSetMovieSnapping(true);

  /* draw - via imodDraw to get float done correctly */
  imodDraw(mVi, IMOD_DRAW_XYZ);
  return 1;
}

/*
 *  Sync the pan position to the current model point. 
 */
#define BORDER_FRAC  0.1
#define BORDER_MIN  50
#define BORDER_MIN_MULTIZ  20
#define BORDER_MAX  125

void ZapFuncs::syncImage(bool toImagePt)
{
  int syncborder, wposition, wsize, tripshift;
  int trytrans, trydraws, tryborder, trystart, borderMin;
  ImodView *vi = mVi;
  if ((!mLock) && 
      ((vi->imod->mousemode == IMOD_MMODEL && mVi->imod->cindex.point >= 0) || toImagePt
       || (mNumXpanels && mKeepcentered))) {
    borderMin = mNumXpanels ? BORDER_MIN_MULTIZ : BORDER_MIN;

    /* If the keepcentered flag is set, just do a shift to center */
    if (mKeepcentered)
      tripshift = 1;
    else {
      
      /* Otherwise, look at each axis independently.  First see if
         if the position is within the borders for shifting */
      tripshift = 0;
      wsize = mNumXpanels ? mPanelXsize : mWinx;
      wposition = xpos(vi->xmouse);
      syncborder = (int)(wsize * BORDER_FRAC);
      syncborder = B3DMIN(BORDER_MAX, B3DMAX(borderMin, syncborder));
      if (wposition < syncborder || wposition > wsize - syncborder) {
        
        /* If close to a border, do an image offset computation
           to see if the display would actually get moved if
           this axis were centered on point */
        trytrans = (int)((vi->xsize * 0.5f) - vi->xmouse + 0.5f);
        trydraws = mXdrawsize;
        tryborder = mXborder;
        trystart = mXstart;
        /* imodPrintStderr ("before %d %d %d %d\n", 
           trydraws, mXtrans, tryborder, trystart); */
        b3dSetImageOffset(wsize, vi->xsize, mZoom, trydraws,
                          trytrans, tryborder, trystart, 1);
        /* imodPrintStderr ("after %d %d %d %d\n", 
           trydraws, trytrans, tryborder, trystart); */
        /* Can't use xtrans for a test, need to use the other
           two values to see if change in display would occur */
        if (tryborder != mXborder || trystart != mXstart)
          tripshift += 1;
        
      }
      
      /* Same for Y axis */
      wsize = mNumXpanels ? mPanelYsize : mWiny;
      wposition = ypos(vi->ymouse);
      syncborder = (int)(wsize * BORDER_FRAC);
      syncborder = B3DMIN(BORDER_MAX, B3DMAX(borderMin, syncborder));
      if (wposition < syncborder || wposition > wsize - syncborder){
        trytrans = (int)((vi->ysize * 0.5f) - vi->ymouse + 0.5f);
        trydraws = mYdrawsize;
        tryborder = mYborder;
        trystart = mYstart;
        b3dSetImageOffset(wsize, vi->ysize, mZoom, trydraws,
                          trytrans, tryborder, trystart, 1);
        if (tryborder != mYborder || trystart != mYstart)
          tripshift += 2;
      }
    }
    
    if (tripshift) {
      /* imodPrintStderr("tripshift %d\n",tripshift); */
      mXtrans = (int)((vi->xsize * 0.5f) - vi->xmouse + 0.5f);
      mYtrans = (int)((vi->ysize * 0.5f) - vi->ymouse + 0.5f);
    }
  }
}

B3dCIImage *ZapFuncs::getNewCIImage(B3dCIImage *image)
{
  B3dCIImage *newim;
  if (mGinit)
    b3dFlushImage(image);

  newim =  b3dGetNewCIImage(image, App->depth);
  if (!newim) {
    wprint("\aInsufficient memory to run this Zap window.\n"
           "Try making it smaller or close it.\n");
    return NULL;
  }
    
  b3dBufferImage(newim);
  return newim;
}


/*
 * This receives the resize events which precede paint signals
 */
void ZapFuncs::resize(int winx, int winy)
{
  ivwControlPriority(mVi, mCtrl);

  if (imodDebug('z'))
    imodPrintStderr("RESIZE: ");

  if (imodDebug('z')) {
    imodPrintStderr("Size = %d x %d  heights win %d tool %d :", winx, winy,
                    mQtWindow->height(),mQtWindow->mToolBar->height());
    if (mGinit)
      imodPrintStderr("Old Size = %d x %d :", mWinx, mWiny);
  }

  mWinx = winx;
  mWiny = winy;
  b3dSetCurSize(winx, winy);
  b3dResizeViewportXY(winx, winy);

  if (mNumXpanels) {
    setupPanels();
  } else {
    
    mImage =  getNewCIImage(mImage);
    if (!mImage)
      return;
    mRecordSubarea = 1;
  }
  mGinit = 1;
  if (imodDebug('z'))
    imodPrintStderr("\n");
}

void ZapFuncs::allocateToPanels(int num, int winSize, int gutter, int &panelSize,
                                int &border)
{
  int imarea = B3DMAX(0, winSize - (num - 1) * gutter);
  panelSize = imarea / num;
  border = (imarea % num) / 2;
}

int ZapFuncs::setupPanels()
{
  int i, newnum = 0, retval = 0;
  int numtot = mNumXpanels * mNumYpanels;
  allocateToPanels(mNumXpanels, mWinx, mPanelGutter, 
                   mPanelXsize, mPanelXborder);
  allocateToPanels(mNumYpanels, mWiny, mPanelGutter, 
                   mPanelYsize, mPanelYborder);
  
  // If panels are too small, set them to 1 as a signal
  if (mPanelXsize < 4 ||  mPanelYsize < 4) {
    mPanelXsize = 1;
    mPanelYsize = 1;
    retval = 1;
  }

  // Set the minimum size regardless, so it won't get stuck at large value
  mGfx->setMinimumSize
    ((mNumXpanels - 1) *  (mPanelGutter + 5) + 5,
     (mNumYpanels - 1) *  (mPanelGutter + 5) + 5);

  // Allocate or resize the images, stop on a failure
  for (i = 0; i < numtot; i++) {
    mImages[i] = getNewCIImage(mImages[i]);
    if (!mImages[i]) {
      retval = 2;
      break;
    }
    newnum++;
  }
  
  // Clear out unused images
  for (i = newnum; i < mNumImages; i++) {
    b3dFreeCIImage(mImages[i]);
    mImages[i] = NULL;
  }
  mNumImages = newnum;
    
  return retval;
}

void ZapFuncs::flushImage()
{
  int ind;
  if (mNumXpanels) {
    for (ind = 0; ind < mNumImages; ind++)
      b3dFlushImage(mImages[ind]);
  } else
    b3dFlushImage(mImage);
}

/* 1/29/03: removed the resize and expose hack code and the report time code */

/*
 * This is the central drawing routine called from in the module or imodview
 */
void ZapFuncs::draw()
{
  mGfx->updateGL();
}

/*
 * This receives the paint events generated by the window manager
 */
void ZapFuncs::paint()
{
  int ob, ix, iy, ind, delInd, xborderSave, yborderSave, sectionSave;
  int panelX, panelY;
  QObjectList objList;
  QTime drawtime;

  drawtime.start();
  if (imodDebug('z'))
    imodPrintStderr("Paint:");

  b3dSetCurSize(mWinx, mWiny);

  if (mNumXpanels && mPanelXsize < 4)
    return;

  autoTranslate();
  mDrewExtraCursor = false;

  // If the current only flag is set, swap the displayed buffer into the
  // drawing buffer and just draw the current contour
  // Reset value drawing since it has not been set up for this object
  if (mDrawCurrentOnly > 0) {
    if (App->doublebuffer)
      mGfx->swapBuffers();
      ifgResetValueSetup();
    if (mDrawingLasso)
      drawExtraObject();
    else {
      ob = mVi->imod->cindex.object;
      imodSetObjectColor(ob); 
      b3dLineWidth(mVi->imod->obj[ob].linewidth2); 
      drawContour(mVi->imod->cindex.contour, ob);
    }
    mDrawCurrentOnly = -1;
    return;
  }
  
  if (mDrawCurrentOnly < 0)
    setDrawCurrentOnly(0);

  /* DNM 1/29/03: no more skipping of further drawing */
  drawGraphics();

  if (mNumXpanels) {

    // Multipanel draw: need to set borders and section for each panel
    xborderSave = mXborder;
    yborderSave = mYborder;
    sectionSave = mSection;
    for (ix = 0; ix < mNumXpanels; ix++) {
      for (iy = 0; iy < mNumYpanels; iy++) {
        ind = ix + iy * mNumXpanels;
        delInd = ind - (mNumXpanels * mNumYpanels - 1) / 2;
        mSection = sectionSave + mPanelZstep * delInd;
        if (mSection < 0 || mSection >= mVi->zsize)
          continue;
        if ((!delInd && !mDrawInCenter) || (delInd && !mDrawInOthers))
          continue;
        panelX = mPanelXborder + ix * (mPanelXsize + mPanelGutter);
        mXborder = xborderSave + panelX;
        panelY = mPanelYborder + iy * (mPanelYsize + mPanelGutter);
        mYborder = yborderSave + panelY;
        b3dSubareaViewport(panelX, panelY, mPanelXsize, mPanelYsize);
        drawModel();
        drawCurrentPoint();
        drawExtraObject();
      }
    }
    mXborder = xborderSave;
    mYborder = yborderSave;
    mSection = sectionSave;
    b3dResizeViewportXY(mWinx, mWiny);
    
  } else {

    // Normal draw
    drawModel();
    drawCurrentPoint();
    drawExtraObject();
    drawAuto();
    if (mRubberband) {
      b3dLineWidth(1);
      b3dColorIndex(App->endpoint);
      bandImageToMouse(0);
      b3dDrawRectangle(mRbMouseX0, mWiny - 1 - mRbMouseY1, 
                       mRbMouseX1 - mRbMouseX0, 
                       mRbMouseY1 - mRbMouseY0);
    } 

    for (ix = 0; ix < mArrowHead.size(); ix++) {
      if ((fabs((double)xpos(mArrowTail[ix].x) - xpos(mArrowHead[ix].x)) > 2. || 
           fabs((double)ypos(mArrowTail[ix].y) - ypos(mArrowHead[ix].y)) > 2.)) {
        b3dColorIndex(App->arrow[ix % 4]);
        b3dDrawArrow(xpos(mArrowTail[ix].x), ypos(mArrowTail[ix].y), 
                     xpos(mArrowHead[ix].x), ypos(mArrowHead[ix].y));
      }
    }
  }
  drawTools();
  mScaleBarSize = scaleBarDraw(mWinx, mWiny, mZoom, 0);

  // Update graph windows if rubber band changed (this should be done by 
  // control but that is not possible, it doesn't know types)
  if (mBandChanged) {
    imodDialogManager.windowList(&objList, -1, GRAPH_WINDOW_TYPE);
    for (ob = 0; ob < objList.count(); ob++)
      ((GraphWindow *)objList.at(ob))->xgraphDraw();
    mBandChanged = 0;
  }

  if (imodDebug('z'))
    imodPrintStderr("\n");
  if (mHqgfx)
    mLastHqDrawTime = drawtime.elapsed();
}


/*****************************************************************************/
/* zap tool bar functions called from interface class                        */

void ZapFuncs::stepZoom(int step)
{
  setControlAndLimits();
  mZoom = b3dStepPixelZoom(mZoom, step);
  draw();
}

void ZapFuncs::enteredZoom(float newZoom)
{
  if (!mPopup)
    return;
  setControlAndLimits();
  mZoom = newZoom;
  if (mZoom <= 0.001) {
    mZoom = 0.001;
    mToolZoom = newZoom;
  }
  draw();
  mQtWindow->setFocus();
}
 
void ZapFuncs::stateToggled(int index, int state)
{
  int time;
  setControlAndLimits();
  switch (index) {
  case ZAP_TOGGLE_RESOL:
    mHqgfx = state;
    draw();
    break;

  case ZAP_TOGGLE_ZLOCK:
    mLock = state ? 2 : 0;
    if (!mLock) {
      flushImage();
      syncImage(false);
      draw();
    }
    break;

  case ZAP_TOGGLE_CENTER:
    mKeepcentered = state;
    if (state) {
      flushImage();
      syncImage(true);
      draw();
    }
    break;

  case ZAP_TOGGLE_INSERT:
    mInsertmode = state;
    mVi->insertmode = mInsertmode;
    registerDragAdditions();
    break;

  case ZAP_TOGGLE_RUBBER:
    toggleRubberband();
    break;

  case ZAP_TOGGLE_LASSO:
    toggleLasso();
    break;

  case ZAP_TOGGLE_ARROW:
    toggleArrow();
    break;

  case ZAP_TOGGLE_TIMELOCK:
    ivwGetTime(mVi, &time);
    mTimeLock = state ? time : 0;
    if (!mTimeLock)
      draw();
    break;
  }
}

void ZapFuncs::enteredSection(int sec)
{
  if (!mPopup)
    return;
  setControlAndLimits();
  if (mLock != 2)
    mVi->zmouse = sec-1;
  mSection = sec-1;
  ivwBindMouse(mVi);
  imodDraw(mVi, IMOD_DRAW_XYZ);
  mQtWindow->setFocus();
}

void ZapFuncs::stepTime(int step)
{
  setControlAndLimits();
  
  // if time locked, advance the time lock and draw this window
  // Does this make sense?
  if (mTimeLock){
    mTimeLock += step;
    if (mTimeLock <= 0)
      mTimeLock = 1;
    if (mTimeLock > ivwGetMaxTime(mVi))
      mTimeLock = ivwGetMaxTime(mVi);
    draw();

  } else {
    imodMovieXYZT(mVi, MOVIE_DEFAULT, MOVIE_DEFAULT, MOVIE_DEFAULT, 0);
    if (step > 0)
      inputNextTime(mVi);
    else
      inputPrevTime(mVi);
  }
}

void ZapFuncs::autoTranslate()
{
  if (mLock == 2)
    return;

  mSection = (int)(mVi->zmouse + 0.5f);
     
  drawTools();

  if (mLock)
    return;

  return;
}

/* DNM: 2.40 deleted imod_zap_draw which was unused and confusing */


void ZapFuncs::translate(int x, int y)
{
  ImodView *vw = mVi;
  mXtrans += x;
  if (mXtrans > vw->xsize)
    mXtrans = vw->xsize;
  if (mXtrans < -vw->xsize)
    mXtrans = - vw->xsize;
  mYtrans += y;
  if (mYtrans > vw->ysize)
    mYtrans = vw->ysize;
  if (mYtrans < -vw->ysize)
    mYtrans = - vw->ysize;
  draw();
  return;
}


/* static QTime downtime; */

/*
 * Respond to a key press
 */
void ZapFuncs::keyInput(QKeyEvent *event)
{
  struct ViewInfo *vi = mVi;
  Imod *imod = vi->imod;
  int keysym = event->key();
  static int trans = 5;
  int *limits;
  int limarr[4];
  int rx, ix, iy, i, obst, obnd, ob, start, end;
  int keypad = event->modifiers() & Qt::KeypadModifier;
  int shifted = event->modifiers() & Qt::ShiftModifier;
  int ctrl = event->modifiers() & Qt::ControlModifier;
  int handled = 0;
  QString str;
  Iindex indadd;
  Iindex *indp;
  Ipoint selmin, selmax;
  Icont *lasso;
  float dist2d, cx, cy, dx, dy, refx, refy;
  Ipoint *curPnt;
  Iobj *obj;
  /* downtime.start(); */

  /*if (Imod_debug)
    imodPrintStderr("key %x, state %x\n", keysym, event->modifiers()); */
  if (inputTestMetaKey(event))
    return;

  if (utilCloseKey(event)) {
    // For cocoa/Qt 4.5.0, need to prevent enter/leave events
    mPopup = 0;
    mQtWindow->close();
    return;
  }

  inputConvertNumLock(keysym, keypad);

  setControlAndLimits();
  ivwControlActive(vi, 0);

  if (imodPlugHandleKey(vi, event)) 
    return;
  ivwControlActive(vi, 1);

  /* DNM: set global insertmode from this zap's mode to get it to work
     right with Delete key */
  vi->insertmode = mInsertmode;

  /*      imodPrintStderr("Zapo got %x keysym\n", keysym); */


  switch(keysym){

  case Qt::Key_Up:
  case Qt::Key_Down: 
  case Qt::Key_Right: 
  case Qt::Key_Left: 

    // If arrows scroll and it is not keypad, call the appropriate routine for up/down
    if (!keypad && (keysym == Qt::Key_Down || keysym == Qt::Key_Up) && 
        ImodPrefs->arrowsScrollZap()) {
      if (mLock == 2)
        lockedPageUpOrDown(shifted, keysym == Qt::Key_PageUp ? 1 : -1);
      else {
        ivwControlActive(vi, 0);
        inputPageUpOrDown(vi, shifted, keysym == Qt::Key_Up ? 1 : -1);
      }
      handled = 1;

      // Translate with keypad in movie mode or regular arrows in model mode
    } else if ((!keypad && imod->mousemode != IMOD_MMOVIE) ||
        (keypad && imod->mousemode == IMOD_MMOVIE)) {
      if (keysym == Qt::Key_Left)
        translate(-trans, 0);
      if (keysym == Qt::Key_Right)
        translate(trans, 0);
      if (keysym == Qt::Key_Down)
        translate(0, -trans);
      if (keysym == Qt::Key_Up)
        translate(0, trans);
      handled = 1;

      // Move point with keypad in model mode
    } else if (keypad && imod->mousemode != IMOD_MMOVIE) {
      inputKeyPointMove(vi, keysym);
      handled = 1;

    }
    break;

  case Qt::Key_PageUp:
  case Qt::Key_PageDown:
    // With keypad, translate in movie mode or move point in model mode
    if (keypad) {
      if (imod->mousemode == IMOD_MMOVIE)
        translate(trans, keysym == Qt::Key_PageDown ? -trans : trans);
      else
        inputKeyPointMove(vi, keysym);
      handled = 1;

      // with regular keys, handle specially if locked
    } else if (!keypad && mLock == 2){
      lockedPageUpOrDown(shifted, keysym == Qt::Key_PageUp ? 1 : -1);
      handled = 1;
    }
    break;
          
  case Qt::Key_1:
  case Qt::Key_2:
    if (mTimeLock) {
      stepTime((keysym == Qt::Key_1) ? -1 : 1);
      handled = 1;
    }
    break;

  case Qt::Key_At:
  case Qt::Key_Exclam:
    if (mTimeLock) {
      imcGetStartEnd(vi, 3, &start, &end);
      mTimeLock = (keysym == Qt::Key_At) ? end + 1 : start + 1;
      draw();
      handled = 1;
    }
    break;


  case Qt::Key_Home:
    if (keypad && imod->mousemode == IMOD_MMOVIE) {
      translate(-trans, trans);
      handled = 1;
    }
    break;

  case Qt::Key_End:
    if (keypad && imod->mousemode == IMOD_MMOVIE) {
      translate(-trans, -trans);
      handled = 1;
    }
    break;
          
  case Qt::Key_Minus:
    mZoom = b3dStepPixelZoom(mZoom, -1);
    draw();
    handled = 1;
    break;

  case Qt::Key_Plus:
  case Qt::Key_Equal:
    mZoom = b3dStepPixelZoom(mZoom, 1);
    draw();
    handled = 1;
    break;
          
    /* DNM: Keypad Insert key, alternative to middle mouse button */
  case Qt::Key_Insert:

    /* But skip out if in movie mode or already active */
    if (!keypad || imod->mousemode == IMOD_MMOVIE || sInsertDown || 
        mNumXpanels)
      break;

    // It wouldn't work going to a QPoint and accessing it, so do it in shot!
    ix = (mGfx->mapFromGlobal(QCursor::pos())).x();
    iy = (mGfx->mapFromGlobal(QCursor::pos())).y();

    // Set a flag, set continuous tracking, grab keyboard and mouse
    sInsertDown = 1;
    mGfx->setMouseTracking(true);
    mQtWindow->grabKeyboard();
    mGfx->grabMouse();

    /* Use time since last event to determine whether to treat like
       single click or drag */
    rx = sInsertTime.elapsed();
    sInsertTime.restart();
    /* imodPrintStderr(" %d %d %d\n ", rx, ix, iy); */
    if(rx > 250)
      b2Click(ix, iy, 0);
    else
      b2Drag(ix, iy, 0); 

    mLmx = ix;
    mLmy = iy;
    handled = 1;
    break;

    /* DNM 12/13/01: add next and smooth hotkeys to autox */
  case Qt::Key_A:
    if (mNumXpanels)
      break;
    if (ctrl) {

      // Select all contours in current object on section or in rubberband
      if (mRubberband) {
        selmin.x = mRbImageX0;
        selmax.x = mRbImageX1;
        selmin.y = mRbImageY0;
        selmax.y = mRbImageY1;
      } else {
        selmin.x = -vi->xsize;;
        selmax.x = 2 * vi->xsize;
        selmin.y = -vi->ysize;;
        selmax.y = 2 * vi->ysize;
      }
      selmin.z = mSection - 0.5;
      selmax.z = mSection + 0.5;
      lasso = NULL;
      if (mLassoOn && !mDrawingLasso)
        lasso = getLassoContour();

      // Look through selection list, remove any that do not fit constraints
      for (i = ilistSize(vi->selectionList) - 1; i >= 0; i--) {
        indp = (Iindex *)ilistItem(vi->selectionList, i);
        if (indp->object < imod->objsize) {
          obj = &imod->obj[indp->object];
          if (indp->contour < obj->contsize) {
            if ((lasso && imodContInsideCont(obj, &(obj->cont[indp->contour]), lasso,
                                             selmin.z, selmax.z)) || 
                (!lasso && imodContInSelectArea(obj, &(obj->cont[indp->contour]), selmin,
                                                selmax)))
              continue;
          }
        }
        ilistRemove(vi->selectionList, i);
      }

      obst = shifted ? 0 : imod->cindex.object;
      obnd = shifted ? imod->objsize - 1 : imod->cindex.object;
      
      if (obnd < 0)
        break;

      // Set up an index to add, look for contours inside
      // the bounding box, add them, make last one be current
      for (ob = obst; ob <= obnd; ob++) {
        obj = &imod->obj[ob];
        indadd.object = ob;
        indadd.point = -1;
        for (i = 0; i < obj->contsize; i++) {
          indadd.contour = i;
          if ((lasso && imodContInsideCont(obj, &(obj->cont[i]), lasso, selmin.z, 
                                           selmax.z)) ||
              (!lasso && imodContInSelectArea(obj, &(obj->cont[i]), selmin, selmax))) {
            imodSelectionListAdd(vi, indadd);
            imod->cindex = indadd;
          }
        }
      }
      imod_setxyzmouse();
      handled = 1;
      
    } else if (!shifted) {
      autox_next(vi->ax);
      handled = 1;
    } 
    break;

  case Qt::Key_U:
    if (shifted || mNumXpanels)
      break;
    autox_smooth(vi->ax);
    handled = 1;
    break;

  case Qt::Key_B:
    if (mNumXpanels || ctrl)
      break;
    if (shifted) { 
      toggleRubberband();
    } else
      autox_build(vi->ax);
    handled = 1;
    break;
          
  case Qt::Key_P:
    if (mNumXpanels)
      break;
    if (shifted) {
      toggleContourShift();
      handled = 1;
    }
    break;

  case Qt::Key_S:
    if (shifted || ctrl){
      mShowslice = mShowedSlice;

      // Take a montage snapshot if selected and no rubberband is on
      if (!mRubberband && imcGetSnapMontage(true) && !mNumXpanels) {
        montageSnapshot((ctrl ? 1 : 0) + (shifted ? 2 : 0));
        handled = 1;
        break;
      }
      draw();
      setSnapshotLimits(&limits, limarr);
      b3dKeySnapshot((char *)(mNumXpanels ? "multiz" : "zap"), shifted, 
                     ctrl, limits);
    }else
      inputSaveModel(vi);
    handled = 1;
    break;
          
  case Qt::Key_R:
    if (ctrl && shifted && !mNumXpanels && mRubberband) {
      mZoom = B3DMIN(mWinx / (mRbImageX1 + 1. - mRbImageX0), 
                     mWiny / (mRbImageY1 + 1. - mRbImageY0));
      mXtrans = (int)(-(mRbImageX1 + mRbImageX0 - mVi->xsize) / 2);
      mYtrans = (int)(-(mRbImageY1 + mRbImageY0 - mVi->ysize) / 2);
      draw();
      handled = 1;

    } else if (shifted && !mNumXpanels) {
      resizeToFit();
      handled = 1;
    }
    break;

  case Qt::Key_Z:
    if (shifted) {
      if (!mNumXpanels) { 
        if(mSectionStep) {
          mSectionStep = 0;
          wprint("Auto-section advance turned OFF\n");
        } else {
          mSectionStep = 1;
          wprint("\aAuto-section advance turned ON\n");
        }
      }
      handled = 1;
    }
    break;

  case Qt::Key_I:
    if (mNumXpanels)
      break;
    if (shifted)
      printInfo();
    else {
      stateToggled(ZAP_TOGGLE_INSERT, 1 - mInsertmode);
      mQtWindow->setToggleState(ZAP_TOGGLE_INSERT, mInsertmode);
      wprint("\aToggled modeling direction\n");
    }
    handled = 1;
    break;

  case Qt::Key_Q:
    ix = (mGfx->mapFromGlobal(QCursor::pos())).x();
    iy = (mGfx->mapFromGlobal(QCursor::pos())).y();
    getixy(ix, iy, cx, cy, i);
    refx = vi->xmouse;
    refy = vi->ymouse;
    curPnt = imodPointGet(imod);
    if (curPnt && imod->mousemode == IMOD_MMODEL) {
      refx = curPnt->x;
      refy = curPnt->y;
    }
    dx = cx - refx;
    dy = cy - refy;
    dist2d  = (float)(vi->xybin * sqrt(dx * dx + dy * dy));
    wprint("From (%.1f, %.1f) to (%.1f, %.1f) =\n", refx + 1., refy + 1., cx+1., cy+1.);
    str.sprintf("  %.1f %spixels", dist2d, vi->xybin > 1 ? "unbinned " : "");
    utilWprintMeasure(str, imod, dist2d);
    break;

    /*
      case Qt::Key_X:
      case Qt::Key_Y:
      {
      Dimension width, height, neww, newh;
      Position dx, dy;
      int delta = 1;
      XtVaGetValues(mDialog,
      XmNwidth, &width,
      XmNheight, &height,
      XmNx, &dx, XmNy, &dy,
      NULL);
      if (event->modifiers() & ShiftButton)
      delta = -1;
      if (keysym == Qt::Key_X)
      width += delta;
      else
      height += delta;
      imodPrintStderr ("%d x %d\n", width, height);
      XtConfigureWidget(mDialog, dx, dy, width, height, 0);
      }
    */

    /*
      case Qt::Key_X:
    wprint("Clipboard = %s\n", LATIN1(QApplication::clipboard()->text()));
    break;
    */

  default:
    break;

  }

  // If event not handled, pass up to default processor
  if (handled) {
    event->accept();    // redundant action - supposed to be the default
  } else {
    // What does this mean? It is needed to get images to sync right
    ivwControlActive(vi, 0);
    inputQDefaultKeys(event, vi);
  }
}

void ZapFuncs::lockedPageUpOrDown(int shifted, int direction)
{
  Iobj *obj;
  if (shifted) {
    obj = imodObjectGet(mVi->imod);
    mSection = utilNextSecWithCont(mVi, obj, mSection, direction);
  } else {
    mSection += direction;
  }
  B3DCLAMP(mSection, 0, mVi->zsize -1);
  draw();
}

/*
 * Key is raised: finish up various tasks
 */
void ZapFuncs::keyRelease(QKeyEvent *event)
{
  /*  imodPrintStderr ("%d down\n", downtime.elapsed()); */
  if (!sInsertDown || !(event->modifiers() & Qt::KeypadModifier) ||
      (event->key() != Qt::Key_Insert && event->key() != Qt::Key_0))
    return;
  sInsertDown = 0;
  registerDragAdditions();
  setMouseTracking();
  mQtWindow->releaseKeyboard();
  mGfx->releaseMouse();

  // Note that unless the user turns off autorepeat on the key, there is a
  // series of key press - release events and it does full draws
  if (mDrawCurrentOnly) {
    setDrawCurrentOnly(0);
    imodDraw(mVi, IMOD_DRAW_MOD | IMOD_DRAW_XYZ);
  }
}

// Pass on various events to plugins
void ZapFuncs::generalEvent(QEvent *e)
{
  int ix, iy, ifdraw, iz;
  float imx, imy;
  if (mNumXpanels || !mPopup || App->closing)
    return;
  ix = (mGfx->mapFromGlobal(QCursor::pos())).x();
  iy = (mGfx->mapFromGlobal(QCursor::pos())).y();
  getixy(ix, iy, imx, imy, iz);
  ifdraw = imodPlugHandleEvent(mVi, e, imx, imy);
  if (ifdraw & 2 || (mDrewExtraCursor && e->type() == QEvent::Leave))
    draw();
  if (ifdraw)
    return;
  if (e->type() == QEvent::Wheel && iceGetWheelForSize()) {
    utilWheelChangePointSize(mVi, mZoom, ((QWheelEvent *)e)->delta());
  }
}

/*
 * respond to a mouse press event
 */
void ZapFuncs::mousePress(QMouseEvent *event)
{
  int button1, button2, button3, ifdraw = 0, drew = 0;
  int ctrlDown = event->modifiers() & Qt::ControlModifier;
  int dxll, x, y;
  Icont *cont;
  Ipoint mpt;
  int rcrit = 10;   /* Criterion for moving the whole band */
  bool ebut1 = event->button() == ImodPrefs->actualButton(1);
  bool ebut2 = event->button() == ImodPrefs->actualButton(2);

  button1 = event->buttons() & ImodPrefs->actualButton(1) ? 1 : 0;
  button2 = event->buttons() & ImodPrefs->actualButton(2) ? 1 : 0;
  button3 = event->buttons() & ImodPrefs->actualButton(3) ? 1 : 0;
  x = event->x();
  y = event->y();
  sBut1downt.start();
  sFirstmx = x;
  sFirstmy = y;
  mLmx = x;
  mLmy = y;
  sMousePressed = true;
  utilRaiseIfNeeded(mQtWindow, event);

  if (imodDebug('m'))
    imodPrintStderr("click at %d %d   buttons %d %d %d\n", x, y, button1, 
                    button2, button3);

  // Check for starting a band move before offering to plugin
  if (ebut2 && !button1 && !button3 && !mShiftingCont) {
    sMoveBandLasso = 0;
    
    /* If rubber band is on and within criterion distance of any edge, set
       flag to move whole band and return */
    if (mRubberband) {
      bandImageToMouse(0);
      if (utilTestBandMove(x, y, mRbMouseX0, mRbMouseX1, mRbMouseY0, mRbMouseY1)) {
        sMoveBandLasso = 1;
        setCursor(mMousemode);
        return;
      }
    }
  }

  // Also check if lasso is within criterion distance, with button 1 or 2
  if ((ebut2 || ebut1) && !button3 && !mShiftingCont) {
    sMoveBandLasso = 0;
    if (mLassoOn && !mDrawingLasso) {
      getixy(x, y, mpt.x, mpt.y, dxll);
      mpt.z = dxll;
      cont = getLassoContour();
      if (cont && imodPointContDistance(cont, &mpt, 0, 0, &dxll) * mZoom < rcrit) {
        sMoveBandLasso = 1;
        setCursor(mMousemode);
        return;
      }
    }
  }  
  setCursor(mMousemode, utilNeedToSetCursor());

  // Now give the plugins a crack at it
  if (!mDrawingLasso && !mDrawingArrow)
    ifdraw = checkPlugUseMouse(event, button1, button2, button3);
  if (ifdraw & 1) {
    if (ifdraw & 2)
      draw();
    return;
  }

  // Check for regular actions
  if (ebut1 && !mDrawingLasso) {
    if (mShiftingCont)
      drew = startShiftingContour(sFirstmx, sFirstmy, 1, ctrlDown);
    else if (mStartingBand || mDrawingArrow)
      drew = b1Click(sFirstmx, sFirstmy, ctrlDown);
    else
      sFirstDrag = 1;
      
  } else if ((mDrawingLasso && (ebut1 || ebut2)) || (ebut2 && !button1 && !button3)) {
    if (mShiftingCont)
      drew = startShiftingContour(x, y, 2, ctrlDown);
    else
      drew = b2Click(x, y, ctrlDown);

  } else if (event->button() == ImodPrefs->actualButton(3) && !button1 && !button2) {
    if (mShiftingCont)
      drew = startShiftingContour(x, y, 3, ctrlDown);
    else
      drew = b3Click(x, y, ctrlDown);
  }
  if (ifdraw && !drew)
    draw();
}

/*
 * respond to mouse release event
 */
void ZapFuncs::mouseRelease(QMouseEvent *event)
{
  Iobj *obj;
  int imz, ind, button1, button2, button3, ifdraw = 0, drew = 0;
  float area, length;
  bool needDraw, releaseBand;
  float imx, imy;
  QString str;
  button1 = event->button() == ImodPrefs->actualButton(1) ? 1 : 0;
  button2 = event->button() == ImodPrefs->actualButton(2) ? 1 : 0;
  button3 = event->button() == ImodPrefs->actualButton(3) ? 1 : 0;
  sMousePressed = false;
  releaseBand = (((button2 && mRubberband) || ((button1 || button2) && mLassoOn)) && 
                 sMoveBandLasso) || (button1 && mDrawingArrow);

  if (imodDebug('m'))
    imodPrintStderr("release at %d %d   buttons %d %d %d\n", event->x(), 
                    event->y(), button1, button2, button3);
  if (mShiftRegistered) {
    mShiftRegistered = 0;
    mVi->undo->finishUnit();
  }
  needDraw = mDrewExtraCursor && !mGfx->extraCursorInWindow();

  if (!mDrawingLasso && !releaseBand)
    ifdraw = checkPlugUseMouse(event, button1, button2, button3);
  if (ifdraw & 1) {
    if ((ifdraw & 2) || needDraw)
      draw();

    // Defer the return so the band moving can be turned off, but then only 
    // check other things below if this flag is off
  }

  if (button1 && !(ifdraw & 1) && !mDrawingLasso && !releaseBand) {
    if (sDragBandLasso) {
      sDragBandLasso = 0;
      setCursor(mMousemode);
    }
    sFirstDrag = 0;

    if (imodDebug('m'))
      imodPrintStderr("Down time %d msec  %d\n", sBut1downt.elapsed(), mHqgfxsave);
    if (sBut1downt.elapsed() > 250) {
      if (mHqgfxsave || ifdraw)
        draw();
      mHqgfxsave  = 0;
      return;    //IS THIS RIGHT?
    }
    drew = b1Click(event->x(), event->y(),
               event->modifiers() & Qt::ControlModifier);
  }
 
  // Button 2 and band moving, release the band
  if (releaseBand) {
    sMoveBandLasso = 0;
    mDrawingArrow = false;
    setCursor(mMousemode);

    // Do a full draw for lasso move because isosurface may need updating
    if (mHqgfxsave || mLassoOn || mArrowOn) {
      if (mLassoOn)
        imodDraw(mVi, IMOD_DRAW_MOD);
      else        
        draw();
      drew = 1;
    }
    mHqgfxsave  = 0;

    // Button 2 and doing a drag draw - draw for real.
  } else if ((button2 || (button1 && mDrawingLasso)) && !mNumXpanels && !(ifdraw & 1) &&
              (mVi->imod->mousemode == IMOD_MMODEL || mDrawingLasso)) {
    if (imodDebug('z'))
      imodPrintStderr("Down time %d msec\n", sBut1downt.elapsed());

    if (mDrawingLasso) {
      obj = ivwGetAnExtraObject(mVi, mLassoObjNum);
      if (!obj || obj->cont->psize < 2) {
        toggleLasso();
      } else {
        mDrawingLasso = false;
        setOrClearFlags(&obj->cont->flags, ICONT_OPEN, 0);
        imodTrimContourLoops(obj->cont, 0);
        for (ind = 0; ind < 2; ind++) {
          length = mVi->xybin * imodContourLength(obj->cont, ind);
          str.sprintf("%s length %.1f %spixels", ind ? "Closed" : "Open", length, 
                      mVi->xybin > 1 ? "unbin " : "");
          utilWprintMeasure(str, mVi->imod, length);
        }
        area = mVi->xybin * mVi->xybin * imodContourArea(obj->cont);
        str.sprintf("Area %g %spixels^2", area, mVi->xybin > 1 ? "unbin " : "");
        utilWprintMeasure(str, mVi->imod, area, true);

        setMouseTracking();
        setCursor(mMousemode, true);
      }
    } else

      registerDragAdditions();

    // Fix the mouse position and update the other windows finally
    // Why call imod_info_setxyz again on release of single point add?
    getixy(event->x(), event->y(), imx, imy, imz);
    mVi->xmouse = imx;
    mVi->ymouse = imy;
    if (mDrawCurrentOnly) {
      setDrawCurrentOnly(0);
      imodDraw(mVi, IMOD_DRAW_MOD | IMOD_DRAW_XYZ);
      drew = 1;
    } else
      imod_info_setxyz();
  }

  // Now return if plugin said to
  if (ifdraw & 1)
    return;

  if (mCenterMarked && !mCenterDefined) {
    ivwClearAnExtraObject(mVi, mShiftObjNum);
    imodDraw(mVi, IMOD_DRAW_MOD);
    mCenterMarked = 0;
    drew = 1;
  }
  if ((ifdraw || needDraw) && !drew)
    draw();
  else
    setAreaLimits();
}

/*
 * Respond to a mouse move event (mouse down)
 */
void ZapFuncs::mouseMove(QMouseEvent *event)
{
  int imz;
  static int button1, button2, button3, ex, ey, processing = 0;
  static int ctrlDown, shiftDown;
  int cumdx, cumdy;
  bool movingBandLasso = ((mRubberband || mLassoOn) && sMoveBandLasso) || mDrawingArrow;
  int ifdraw = 0, drew = 0;
  int cumthresh = 6 * 6;
  int dragthresh = 10 * 10;
  float imx, imy;

  // Record state of event and then return if eating move events
  ctrlDown = event->modifiers() & Qt::ControlModifier;
  shiftDown = event->modifiers() & Qt::ShiftModifier;
  ex = event->x();
  ey = event->y();
  button1 = (event->buttons() & ImodPrefs->actualButton(1)) ? 1 : 0;
  button2 = (event->buttons() & ImodPrefs->actualButton(2)) ? 1 : 0;
  button3 = (event->buttons() & ImodPrefs->actualButton(3)) ? 1 : 0;
  if (processing) {
    processing++;
    return;
  }

  if (sPixelViewOpen) {
    getixy(ex, ey, imx, imy, imz);
    pvNewMousePosition(mVi, imx, imy, imz);
  }

  if (!movingBandLasso  && 
      (sMousePressed || sInsertDown || mVi->trackMouseForPlugs)) {
    ifdraw = checkPlugUseMouse(event, button1, button2, button3);
    if (ifdraw & 1) {
      if (ifdraw & 2)
        draw();
      return;
    }
  } else if (sMousePressed || sInsertDown)
    setControlAndLimits();

  if (!(sMousePressed || sInsertDown)) {
    if ((mRubberband || (mLassoOn && !mDrawingLasso)) && !mShiftingCont)
      analyzeBandEdge(ex, ey);
    if (ifdraw)
      draw();
    return;
  }

  // For first button or band moving, eat any pending move events and use 
  // latest position
  if ( (button1 && !mDrawingLasso && !button2 && !button3) || movingBandLasso){
    processing = 1;
    imod_info_input();
    if (imodDebug('m') && processing > 1)
      imodPrintStderr("Flushed %d move events\n", processing - 1);
    processing = 0;

    // If this processed a release, then turn off the buttons
    if (!sMousePressed)
      button1 = button2 = button3 = 0;
  }

  cumdx = ex - sFirstmx;
  cumdy = ey - sFirstmy;
  button2 = (button2 || sInsertDown) ? 1 : 0;
  if (imodDebug('m'))
    imodPrintStderr("move %d,%d  mb  %d|%d|%d  c %x s %x\n", ex, ey, button1,
                    button2, button3, ctrlDown, shiftDown);

  if (!movingBandLasso && (button1 && !mDrawingLasso) && !button2 && !button3){
    if (ctrlDown) {
      drew = dragSelectContsCrossed(ex, ey);
    } else {
      /* DNM: wait for a bit of time or until enough distance moved, but if we
         do not replace original lmx, lmy, there is a disconcerting lurch */
      if ((sBut1downt.elapsed()) > 250 || cumdx * cumdx + cumdy * cumdy >
          cumthresh)
        drew = b1Drag(ex, ey);
    }
  }

  // DNM 8/1/08: Reject small movements soon after the button press
  if ( (((mDrawingLasso || mDrawingArrow || (mLassoOn && sMoveBandLasso)) && 
         (button1 || button2)) || (!button1 && button2)) && !button3) {
    if ((sBut1downt.elapsed()) > 150 || cumdx * cumdx + cumdy * cumdy > 
        dragthresh)
      drew = b2Drag(ex, ey, ctrlDown);
  }
  
  if ( !button1 && !button2 && button3)
    drew = b3Drag(ex, ey, ctrlDown, shiftDown);
  
  mLmx = ex;
  mLmy = ey;
  if (ifdraw && !drew)
    draw();
}

/*
 * Test for whether a plugin takes care of the mouse event; take care of 
 * setting limits also
 */
int ZapFuncs::checkPlugUseMouse(QMouseEvent *event, int but1, 
                          int but2, int but3)
{
  float imx, imy;
  int ifdraw, imz;
  bool setControl = sMousePressed || sInsertDown || but1 || but2 || but3;
  if (setControl)
    setControlAndLimits();
  if (mNumXpanels)
    return 0;
  if (setControl)
    ivwControlActive(mVi, 0);
  getixy(event->x(), event->y(), imx, imy, imz);
  ifdraw = imodPlugHandleMouse(mVi, event, imx, imy, but1, but2, but3);
  if (ifdraw & 1) {
    if (ifdraw & 2) 
      draw();
  } else if (setControl)
    ivwControlActive(mVi, 1);
  return ifdraw;
}

/*
 * Analyze for whether mouse is close to a corner or an edge and set flags
 * for mouse to be set properly
 */
void ZapFuncs::analyzeBandEdge(int ix, int iy)
{
  int rubbercrit = 10;  /* Criterion distance for grabbing the band */
  int i, dxll;
  Icont *cont;
  Ipoint mpt;

  bandImageToMouse(0);    
  sDragBandLasso = 0;
  for (i = 0; i < 4; i++)
    sDragging[i] = 0;

  if (mLassoOn) {
    cont = getLassoContour();
    getixy(ix, iy, mpt.x, mpt.y, i);
    mpt.z = i;
    if (cont && imodPointContDistance(cont, &mpt, 0, 0, &dxll) * mZoom < rubbercrit)
      sDragBandLasso = 1;
  } else {

    utilAnalyzeBandEdge(ix, iy, mRbMouseX0, mRbMouseX1, mRbMouseY0, mRbMouseY1, 
                        sDragBandLasso, sDragging);
  }
  setCursor(mMousemode);
}

/*
 * Adjust minimum size of rubberband down in case image is tiny
 */
int ZapFuncs::bandMinimum()
{
  int bandmin = B3DMIN(4, (int)(mVi->xsize * mXzoom) + 2);
  bandmin = B3DMIN(bandmin, (int)(mVi->ysize * mZoom) + 2);
  return bandmin;
}

/*
 * First mouse button click: Attach to nearest point in model mode,
 * or just modify the current xmouse, ymouse values
 */
int ZapFuncs::b1Click(int x, int y, int controlDown)
{
  ImodView *vi   = mVi;
  Imod     *imod = vi->imod;
  Ipoint pnt;
  Iindex index, indSave;
  int iz;
  float distance;
  float ix, iy;
  float selsize = IMOD_SELSIZE / mZoom;

  getixy(x, y, ix, iy, iz);
  if (iz < 0)
    return 0;
     
  // If starting rubber band, just record these coordinates
  if (mStartingBand) {
    mRbMouseX0 = x;
    mRbMouseY0 = y;
    return 0;
  }

  // If drawing an arrow, start the coordinates
  if (mDrawingArrow) {
    iz = mArrowHead.size() - 1;
    mArrowHead[iz].x = mArrowTail[iz].x = ix;
    mArrowHead[iz].y = mArrowTail[iz].y = iy;
    return 0;
  }

  if (vi->ax && !mNumXpanels)
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      autox_fillmouse(vi, (int)ix, (int)iy);
      return 1;
    }
     
  // In either mode, do a default modification of zmouse or mSection
  vi->xmouse = ix;
  vi->ymouse = iy;
  if (mLock)
    mSection = iz;
  else
    vi->zmouse = (float)iz;

  if (vi->imod->mousemode == IMOD_MMODEL){
    pnt.x = ix;
    pnt.y = iy;
    pnt.z = iz;
    indSave = vi->imod->cindex;

    distance = imodAllObjNearest(vi, &index , &pnt, selsize, ivwWindowTime(vi,mTimeLock));

    // If point found, manage selection list and even toggle this contour off 
    // if appropriate
    if (distance >= 0.)
      imodSelectionNewCurPoint(vi, imod, indSave, controlDown);

    /* DNM: add the DRAW_XYZ flag to make it update info and Slicer */
    imodDraw(vi, IMOD_DRAW_RETHINK | IMOD_DRAW_XYZ);
    return 1;
  }

  imodDraw(vi, IMOD_DRAW_XYZ);
  return 1;
}

/* 
 * Second mouse button click:
 * In model mode, add a model point, creating a new contour if necessary 
 */
int ZapFuncs::b2Click(int x, int y, int controlDown)
{
  ImodView *vi = mVi;
  Iobj  *obj;
  Icont *cont;
  Ipoint point;
  int   pt;
  float ix, iy;
  float lastz;
  int iz, newSurf;
  bool timeMismatch, notInPlane;
  int time = ivwWindowTime(vi, mTimeLock);

  getixy(x, y, ix, iy, iz);

  if (vi->ax && !mNumXpanels){
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      /* DNM 2/1/01: need to call with int */
      autox_sethigh(vi, (int)ix, (int)iy);
      return 1;
    }
  }
     
  // 3/5/08: Moved band moving to mouse press routine so it would happen before
  // plugin action

  if (vi->imod->mousemode == IMOD_MMODEL || mDrawingLasso) {
    if (mNumXpanels)
      return 0;
    mDragAddCount = 0;

    if (mDrawingLasso) {
      obj = ivwGetAnExtraObject(vi, mLassoObjNum);
      if (!obj) {
        toggleLasso();
        return 0;
      }
      imodObjectSetColor(obj, 1., 0., 0.);
      obj->flags &= ~IMOD_OBJFLAG_SCAT;
      obj->pdrawsize = 0;
      obj->linewidth = 1;
      obj->extra[IOBJ_EX_LASSO_ID] = mCtrl;
      cont = imodContourNew();
      if (!cont) {
        toggleLasso();
        return 0;
      }
      imodObjectAddContour(obj, cont);
      free(cont);
      cont = obj->cont;
      cont->flags |= (ICONT_DRAW_ALLZ | ICONT_STIPPLED | ICONT_OPEN);
      
    } else {

      obj = imodObjectGet(vi->imod);
      if (!obj)
        return 0;

      // Get current contour; if there is none, start a new one
      // DNM 7/10/04: switch to calling routine; it now fixes time of empty cont
      cont = ivwGetOrMakeContour(vi, obj, mTimeLock);
      if (!cont)
        return 0;
    }

    point.x = ix;
    point.y = iy;
    point.z = mSection;
    if ((mTwod)&&(cont)&&(cont->psize)){
      point.z = cont->pts->z;
    }
    vi->xmouse = ix;
    vi->ymouse = iy;
    
    if (mDrawingLasso) {
      imodPointAppend(cont, &point);

    } else {

      // Get a new surface for planar modeling if the current surface is planar in X or Y
      newSurf = INCOS_NEW_CONT;
      if (iobjPlanar(obj->flags) && (!cont->psize || (cont->flags & ICONT_WILD)) &&
          cont->surf && (imodSurfaceIsPlanar(obj, cont->surf, time, X_SLICE_BOX) ||
                         imodSurfaceIsPlanar(obj, cont->surf, time, Y_SLICE_BOX)))
        newSurf = imodCheckSurfForNewCont(obj, cont, time, Z_SLICE_BOX);
                         
      /* If contours are closed and Z has changed, start a new contour */
      /* Also check for a change in time, if time data are being modeled  */
      /* and start new contour for any kind of contour */
      // DNM 7/10/04: just use first point instead of current point which is
      // not always defined
      if (cont->psize > 0) {
        timeMismatch = ivwTimeMismatch(vi, mTimeLock, obj, cont);
        notInPlane = iobjPlanar(obj->flags) && !(cont->flags & ICONT_WILD) &&
          B3DNINT(cont->pts->z) != (int)point.z;
        if (notInPlane || timeMismatch || newSurf != INCOS_NEW_CONT) {
          cont = utilAutoNewContour(vi, cont, notInPlane, timeMismatch, mTimeLock, 
                                    newSurf, "sections", "Z plane");
          if (!cont)
            return 0;
        }
      } else if (newSurf != INCOS_NEW_CONT) {
        utilAssignSurfToCont(vi, obj, cont, newSurf);
      }

      /* Now if times still don't match refuse the point */
      if (ivwTimeMismatch(vi, mTimeLock, obj, cont)) {
        wprint("\aContour time does not match current time.\n"
               "Set contour time to 0 to model across times.\n");
        vi->undo->finishUnit();
        return 0;
      }
    
      // DNM 11/17/04: Cleaned up adding point logic to set an insertion point
      // and just call InsertPoint with it
      // Set insertion point to next point and adjust it down if going backwards
      pt = vi->imod->cindex.point + 1;
      if (pt > 0 && cont->psize)
        lastz = cont->pts[pt - 1].z;
      else
        lastz = point.z;

      if (pt > 0 && mInsertmode)
        pt--;
    
      ivwRegisterInsertPoint(vi, cont, &point, pt);

      /* DNM: auto section advance is based on the direction of section change 
         between last and just-inserted points */
      if (mSectionStep && point.z != lastz) {
        if (point.z - lastz > 0.0)
          vi->zmouse += 1.0;
        else
          vi->zmouse -= 1.0;

        if (vi->zmouse < 0.0)
          vi->zmouse = 0;
        if (vi->zmouse > vi->zsize - 1)
          vi->zmouse = vi->zsize - 1;
      }     
    }
    imodDraw(vi, IMOD_DRAW_MOD | IMOD_DRAW_XYZ);
    /* DNM 5/22/03: sync all but the active window when single point added */
      //} else
      //imodDraw(vi, IMOD_DRAW_MOD | IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC);
      
    return 1;
  }
  return startMovieCheckSnap(1);
}

/*
 * Delete all points of current contour under the cursor 
 */     
int ZapFuncs::delUnderCursor(int x, int y, Icont *cont)
{
  float ix, iy;
  float crit = 8./ mZoom;
  float critsq, dsq;
  int i, iz;
  Ipoint *lpt;
  int deleted = 0;

  getixy(x, y, ix, iy, iz);
  critsq = crit * crit;
  for (i = 0; i < cont->psize  && cont->psize > 1; ) {
    lpt = &(cont->pts[i]);
    if (floor(lpt->z + 0.5) == mSection) {
      dsq = (lpt->x - ix) * (lpt->x - ix) +
        (lpt->y - iy) * (lpt->y - iy);
      if (dsq <= critsq) {
        mVi->undo->pointRemoval(i);
        imodPointDelete(cont, i);
        mVi->imod->cindex.point = 
          B3DMIN(cont->psize - 1, B3DMAX(i + mInsertmode - 1, 0));
        deleted = 1;
        continue;
      }
    }
    i++;
  }
  if (!deleted)
    return 0;
  mVi->undo->finishUnit();
  imodDraw(mVi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD );
  return 1;
}

/* 
 * Third mouse button click:
 * In model mode, modify current point; otherwise run movie 
 */
int ZapFuncs::b3Click(int x, int y, int controlDown)
{
  ImodView *vi = mVi;
  Icont *cont;
  Iobj *obj;
  int   pt, iz;
  float ix, iy;

  getixy(x, y, ix, iy, iz);

  if (vi->ax && !mNumXpanels) {
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      /* DNM 2/1/01: need to call with int */
      autox_setlow(vi, (int)ix, (int)iy);
      return 1;
    }
  }

  if (vi->imod->mousemode == IMOD_MMODEL) {
    if (mNumXpanels)
      return 0;
    cont = imodContourGet(vi->imod);
    pt   = vi->imod->cindex.point;
    if (!cont)
      return 0;
    if (pt < 0)
      return 0; 

    obj = imodObjectGet(vi->imod);
    if (ivwTimeMismatch(vi, mTimeLock, obj, cont))
      return 0;

    /* If the control key is down, delete points under the cursor */
    if (controlDown)
      return delUnderCursor(x, y, cont);

          
    if (!pointVisable(&(cont->pts[pt])))
      return 0;

    vi->undo->pointShift();
    cont->pts[pt].x = ix;
    cont->pts[pt].y = iy;
    vi->undo->finishUnit();

    vi->xmouse  = ix;
    vi->ymouse  = iy;

    imodDraw(vi, IMOD_DRAW_RETHINK);
    return 1;
  }
  return startMovieCheckSnap(-1);
}

/*
 * First mouse button drag
 */
int ZapFuncs::b1Drag(int x, int y)
{

  // For zooms less than one, move image along with mouse; for higher zooms,
  // Translate 1 image pixel per mouse pixel (accelerated)
  double transFac = mZoom < 1. ? 1. / mZoom : 1.;
  bool cancelHQ = mLastHqDrawTime > sHqDrawTimeCrit;
  int bandmin = bandMinimum();
  float dx, dy;

  if (mShiftingCont) {
    shiftContour(x, y, 1, 0);
    return 1;
  }

  // If we are still starting band, first find out if the mouse has moved far enough
  // to commit to a direction
  if (mStartingBand) {

    if (!utilIsBandCommitted(x, y, mWinx, mWiny, bandmin, mRbMouseX0, mRbMouseX1, 
                             mRbMouseY0, mRbMouseY1, sDragging))
      return 0;

    bandMouseToImage(0);

    // Does image coord need to be moved?  Do so and move mouse coords and mouse
    dx = dy = 0;
    if (mRbImageX0 < 0.)
      dx = -mRbImageX0;
    if (mRbImageX1 > mVi->xsize)
      dx = mVi->xsize - mRbImageX1; 
    if (mRbImageY0 < 0.)
      dy = -mRbImageY0;
    if (mRbImageY1 > mVi->ysize)
      dy = mVi->ysize - mRbImageY1; 
    if (dx || dy) {
      mRbImageX0 += dx;
      mRbImageX1 += dx;
      mRbImageY0 += dy;
      mRbImageY1 += dy;
      bandImageToMouse(1);
      x = sDragging[0] ? mRbMouseX0 : mRbMouseX1;
      y = sDragging[2] ? mRbMouseY0 : mRbMouseY1;
      QCursor::setPos(mGfx->mapToGlobal(QPoint(x, y)));
      mLmx = x;
      mLmy = y;
    }

    // Set flags for the band being on and being dragged
    mStartingBand = 0;
    mRubberband = 1;
    mGfx->setMouseTracking(true);
    sDragBandLasso = 1;
    mBandChanged = 1;
    setCursor(mMousemode);
    draw();
    return 1;  
      
  }

  // First time mouse moves, lock in the band drag position
  if (mRubberband && sFirstDrag)
    analyzeBandEdge(x, y);
  sFirstDrag = 0;
     
  if (mRubberband && sDragBandLasso) {

    /* Move the rubber band */
    if (sDragging[0]) {
      mRbImageX0 += (x - mLmx) / mXzoom;
      B3DCLAMP(mRbImageX0, 0, mRbImageX1 - 1);
    }
    if (sDragging[1]) {
      mRbImageX1 += (x - mLmx) / mXzoom;
      B3DCLAMP(mRbImageX1, mRbImageX0 + 1, mVi->xsize);
    }
    if (sDragging[3]) {
      mRbImageY0 += (mLmy - y) / mXzoom;
      B3DCLAMP(mRbImageY0, 0, mRbImageY1 - 1);
    }
    if (sDragging[2]) {
      mRbImageY1 += (mLmy - y) / mXzoom;
      B3DCLAMP(mRbImageY1, mRbImageY0 + 1, mVi->ysize);
    }
    mBandChanged = 1;
    setCursor(mMousemode, utilNeedToSetCursor());
  } else {
    /* Move the image */
    if (imodDebug('m'))
      imodPrintStderr("B1Drag: x,y %d,%d  lmx,y %d,%d  trans %d,%d\n",
                      x, y, mLmx,  mLmy,  mXtrans,  mYtrans);
    mXtrans += (int)floor(transFac * (x - mLmx) + 0.5);
    mYtrans -= (int)floor(transFac * (y - mLmy) + 0.5);
  }

  if (cancelHQ) {
    mHqgfxsave = mHqgfx;
    mHqgfx = 0;
  }
  draw();
  if (cancelHQ)
    mHqgfx = mHqgfxsave;
  return 1;
}


/*
 * Select contours in the current object crossed by a mouse move 
 */
int ZapFuncs::dragSelectContsCrossed(int x, int y)
{
  ImodView *vi = mVi;
  Imod *imod = vi->imod;
  int ob, co, pt, ptStart, lastPt, thisZ, lastZ, iz2, iz1, drew = 0;
  Icont *cont;
  Ipoint pnt1, pnt2;
  Iobj *obj = imodObjectGet(imod);

  // Skip for movie mode
  if (!obj || imod->mousemode == IMOD_MMOVIE)
    return 0;

  // Get image positions of starting and current mouse positions
  ob = imod->cindex.object;
  getixy(x, y, pnt2.x, pnt2.y, iz2);
  getixy(mLmx, mLmy, pnt1.x, pnt1.y, iz1);
  if (iz1 != iz2 || iz1 < 0)
    return 0;
  if (imodDebug('z'))
    imodPrintStderr("mouse segment %f,%f to %f,%f\n", pnt1.x, pnt1.y, pnt2.x,
                    pnt2.y);

  // Loop on contours
  // Skip single point, ones already selected, or non-wild with Z not matching
  for (ob = 0; ob < imod->objsize; ob++) {
    obj = &imod->obj[ob];
    // Skip for scattered objects
    if (iobjScat(obj->flags))
      continue;
    for (co = 0; co < obj->contsize; co++) {
      cont = &obj->cont[co];
      if (cont->psize < 2)
        continue;
      if (imodSelectionListQuery(vi, ob, co) > -2 || 
          (imod->cindex.contour == co && imod->cindex.object == ob))
        continue;
      if (!(cont->flags & ICONT_WILD) && B3DNINT(cont->pts->z) != iz1)
        continue;

      // Set up to loop on second point in segment, starting at first point
      // in contour for closed contour
      ptStart = iobjOpen(obj->flags) || (cont->flags & ICONT_OPEN) ? 1 : 0;
      lastZ = iz1;
      if (imodDebug('z'))
        imodPrintStderr("Examining contour %d\n", co);

      // Loop on points, look for segments on the section
      for (pt = ptStart; pt < cont->psize; pt++) {
        lastPt = pt ? pt - 1 : cont->psize - 1;
        thisZ = B3DNINT(cont->pts[pt].z);
        if (lastZ == iz1 && thisZ == iz1) {
          if (imodDebug('z'))
            imodPrintStderr("%f,%f to %f,%f\n", cont->pts[lastPt].x,
                            cont->pts[lastPt].y, cont->pts[pt].x,
                            cont->pts[pt].y);

          if (imodPointIntersect(&pnt1, &pnt2, &cont->pts[lastPt], 
                                 &cont->pts[pt])) {

            // Crosses.  Select this contour; add current contour if list empty
            if (!ilistSize(vi->selectionList) && imod->cindex.contour >= 0)
              imodSelectionListAdd(vi, imod->cindex);
            imod->cindex.object = ob;
            imod->cindex.contour = co;
            imod->cindex.point = pt;
            imodSelectionListAdd(vi, imod->cindex);
            imodDraw(mVi, IMOD_DRAW_RETHINK | IMOD_DRAW_XYZ);
            drew = 1;
            break;
          }
        }
        lastZ = thisZ;
      }
    }
  }
  return drew;
}

/*
 * Second mouse button drag
 */
int ZapFuncs::b2Drag(int x, int y, int controlDown)
{
  ImodView *vi = mVi;
  Iobj *obj;
  Icont *cont;
  Ipoint *lpt, cpt;
  float ix, iy, idx, idy;
  double dist;
  int pt, iz;
  bool cancelHQ = mLastHqDrawTime > sHqDrawTimeCrit;
     
  if (mNumXpanels)
    return 0;

  if (mShiftingCont) {
    shiftContour(x, y, 2, 0);
    return 1;
  }

  if (vi->ax){
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      getixy(x, y, ix, iy, iz);
      /* DNM 2/1/01: need to call with int */
      autox_sethigh(vi, (int)ix, (int)iy);
      return 1;
    }
  }

  if ((mRubberband || mLassoOn) && sMoveBandLasso) {
    /* Moving rubber band: get desired move and constrain it to keep
       band in the image */
    idx = (x - mLmx) / mXzoom;
    idy = (mLmy - y) / mZoom;
    if (mRubberband)
      shiftRubberband(idx, idy);
    else {
      cont = getLassoContour();
      if (cont) {
        limitContourShift(cont, idx, idy);
        for (pt = 0; pt < cont->psize; pt++) {
          cont->pts[pt].x += idx;
          cont->pts[pt].y += idy;
        }
      }
    }

    if (cancelHQ) {
      mHqgfxsave = mHqgfx;
      mHqgfx = 0;
    }
    draw();
    if (cancelHQ)
      mHqgfx = mHqgfxsave;
    if (mRubberband)
      mBandChanged = 1;
    return 1;
  }

  // Arrow: update the head coordinates
  getixy(x, y, ix, iy, iz);
  if (mDrawingArrow) {
    iz = mArrowHead.size() - 1;
    mArrowHead[iz].x = ix;
    mArrowHead[iz].y = iy;
    draw();
    return 1;
  }

  if (vi->imod->mousemode == IMOD_MMOVIE && !mDrawingLasso)
    return 0;

  if (vi->imod->cindex.point < 0  && !mDrawingLasso)
    return 0;

  cpt.x = ix;
  cpt.y = iy;
  cpt.z = mSection;
     
  if (mDrawingLasso) {
    obj = ivwGetAnExtraObject(vi, mLassoObjNum);
    if (!obj || !obj->cont) {
      toggleLasso();
      return 0;
    }
    cont = obj->cont;
    lpt = &cont->pts[cont->psize - 1];

  } else {

    obj = imodObjectGet(vi->imod);
    if (!obj)
      return 0;
    
    cont = imodContourGet(vi->imod);
    if (!cont)
      return 0;

    lpt = &(cont->pts[vi->imod->cindex.point]);
    if (mTwod)
      cpt.z = lpt->z;

    /* DNM 6/18/03: If Z or time has changed, treat it like a button click so
       new contour can be started */
    // DNM 6/30/04: change to start new for any kind of contour with time change
    // DNM 7/15/08: Start new contour if up to the point limit too
    if ((iobjPlanar(obj->flags) && !(cont->flags & ICONT_WILD) && 
         (int)floor(lpt->z + 0.5) != (int)cpt.z) ||
        ivwTimeMismatch(vi, mTimeLock, obj, cont) || 
        (obj->extra[IOBJ_EX_PNT_LIMIT] &&
         cont->psize >= obj->extra[IOBJ_EX_PNT_LIMIT])) {
      registerDragAdditions();
      return b2Click(x, y, 0);
    }

    if (ivwTimeMismatch(vi, mTimeLock, obj, cont))
      return 0;
  }

  dist = imodel_point_dist( lpt, &cpt);
  if ( dist > scaleModelRes(vi->imod->res, mZoom)){

    if (mDrawingLasso) {
      imodPointAppend(cont, &cpt);
      setDrawCurrentOnly(1);

    } else {

      // Set insertion index to next point, or to current if drawing backwards
      pt = vi->imod->cindex.point + 1;
      if (pt > 0 && mInsertmode)
        pt--;

      // Set flag for drawing current contour only if at end and going forward
      if (!mInsertmode && pt == cont->psize)
        setDrawCurrentOnly(1);

      // Register previous additions if the count is up or if the object or
      // contour has changed
      if (mDragAddCount >= sDragRegisterSize || 
          mDragAddIndex.object != vi->imod->cindex.object ||
          mDragAddIndex.contour != vi->imod->cindex.contour)
        registerDragAdditions();

      // Start keeping track of delayed registrations by opening a unit and
      // saving the indices.  If general store exists, start with whole data
      // change to save the store.
      // Otherwise if going backwards, need to increment registered first point
      if (!mDragAddCount) {
        if (ilistSize(cont->store))
          vi->undo->contourDataChg();
        else
          vi->undo->getOpenUnit();
        mDragAddIndex = vi->imod->cindex;
        mDragAddIndex.point = pt;
      } else if (mInsertmode)
        mDragAddIndex.point++;
    
      // Always save last point not registered and increment count
      mDragAddEnd = pt;
      mDragAddCount++;

      // Since we are not changing xyzmouse yet, drag draws can be done without
      // IMOD_DRAW_XYZ; this prevents some time-consuming things in other windows
      imodInsertPoint(vi->imod, &cpt, pt);
    }
    imodDraw(vi, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC);
    return 1;
  }
  return 0;
}

/*
 * Third mouse button drag
 */
int ZapFuncs::b3Drag(int x, int y, int controlDown, int shiftDown)
{
  ImodView *vi = mVi;
  Iobj *obj;
  Icont *cont;
  Ipoint *lpt;
  Ipoint pt;
  float ix, iy;
  int iz;

  if (mNumXpanels)
    return 0;

  if (mShiftingCont) {
    shiftContour(x, y, 3, shiftDown);
    return 1;
  }

  if (vi->ax){
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      getixy(x, y, ix, iy, iz);
      /* DNM 2/1/01: need to call with int */
      autox_setlow(vi, (int)ix, (int)iy);
      return 1;
    }
  }

  // if (!(maskr & Button3Mask))
  //  return;

  if (vi->imod->mousemode == IMOD_MMOVIE)
    return 0;
     
  if (vi->imod->cindex.point < 0)
    return 0;

  cont = imodContourGet(vi->imod);
  if (!cont)
    return 0;

  /* DNM 11/13/02: do not allow operation on scattered points */
  obj = imodObjectGet(vi->imod);
  if (iobjScat(obj->flags))
    return 0;

  if (ivwTimeMismatch(vi, mTimeLock, obj, cont))
    return 0;

  if (controlDown)
    return delUnderCursor(x, y, cont);

  if (vi->imod->cindex.point == (cont->psize - 1))
    return 0;

  /* DNM 11/13/02: need to test for both next and current points to prevent
     strange moves between sections */
  if (!pointVisable(&(cont->pts[vi->imod->cindex.point + 1])) ||
      !pointVisable(&(cont->pts[vi->imod->cindex.point])))
    return 0;

  lpt = &(cont->pts[vi->imod->cindex.point]);
  getixy(x, y, (pt.x), (pt.y), iz);
  pt.z = lpt->z;
  if (imodel_point_dist(lpt, &pt) > scaleModelRes(vi->imod->res, mZoom)){
    ++vi->imod->cindex.point;
    vi->undo->pointShift();
    lpt = &(cont->pts[vi->imod->cindex.point]);
    lpt->x = pt.x;
    lpt->y = pt.y;
    lpt->z = pt.z;
    vi->undo->finishUnit();
    imodDraw(vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD );
    return 1;
  }
  return 0;
}

/*
 * Register accumulated additions for undo
 */
void ZapFuncs::registerDragAdditions()
{
  Iindex *index = &mVi->imod->cindex;
  if (!mDragAddCount)
    return;
  mDragAddCount = 0;
 
  // If obj/cont don't match, forget it
 if (mDragAddIndex.object != index->object ||
      mDragAddIndex.contour != index->contour) {
    mVi->undo->flushUnit();
    return;
  }

 // Send out the additions
  mVi->undo->pointAddition(B3DMIN(mDragAddIndex.point, index->point),
                               B3DMAX(mDragAddIndex.point, index->point));
  mVi->undo->finishUnit();
}

/*
 * CONTOUR SHIFTING/TRANSFORMING
 */

/*
 * Single routine to toggle shift for contour move window to call
 */
void ZapFuncs::toggleContourShift()
{
  if (mShiftingCont)
    endContourShift();
  else
    setupContourShift();
}

/*
 * Turn off contour shifting and reset mouse
 */
void ZapFuncs::endContourShift()
{
  if (!mShiftingCont)
    return;
  mShiftingCont = 0;
  ivwFreeExtraObject(mVi, mShiftObjNum);
  if (mCenterMarked || mLassoOn)
    imodDraw(mVi, IMOD_DRAW_MOD);
  setCursor(mMousemode);
}

/*
 * Check whether contour shifting is OK and return current contour
 */
Icont *ZapFuncs::checkContourShift(int &pt, int &err)
{
  ImodView *vi = mVi;
  Iobj *obj = imodObjectGet(vi->imod);
  Icont *cont = imodContourGet(vi->imod);
  pt = vi->imod->cindex.point;
  
  err = 0;
  if (mLassoOn && !cont && !mDrawingLasso) {
    cont = getLassoContour();
    if (cont)
      pt = 0;
    else
      err = 1;
  } else {
    if (vi->imod->mousemode != IMOD_MMODEL || !obj || !cont || !cont->psize)
      err = 1;
    else if (iobjScat(obj->flags) || 
             (!iobjClose(obj->flags) && (cont->flags & ICONT_WILD)))
      err = -1;

    // If no current point, just use first
    if (pt < 0)
      pt = 0;
  }

  if (err)
    endContourShift();
  return cont;
}

/*
 * Initiate contour shifting                             
 */
void ZapFuncs::setupContourShift()
{
  int   pt, err;
  checkContourShift(pt, err);
  if (err < 0)
    wprint("\aYou cannot shift scattered point or non-planar open contours."
           "To shift this contour, temporarily make the object type be "
           "closed.\n");
  if (err)
    return;
  mShiftingCont = 1;
  if (mStartingBand)
    toggleRubberband();
  mCenterDefined = 0;
  mCenterMarked = 0;
  mFixedPtDefined = 0;
  mShiftObjNum = ivwGetFreeExtraObjectNumber(mVi);
  setCursor(mMousemode);
  if (mLassoOn)
    imodDraw(mVi, IMOD_DRAW_MOD);
}

// Keep track of the base position for contour shifting
static Ipoint sContShiftBase;

/*
 * Start the actual shift once the mouse goes down
 */
int ZapFuncs::startShiftingContour(int x, int y, int button,
                                 int ctrlDown)
{
  int   pt, err, iz;
  float ix, iy;
  Icont *cont = checkContourShift(pt, err);
  if (err)
    return 0;

  getixy(x, y, ix, iy, iz);

  // If button for marking center, save coordinates, set flag, show mark
  if (button == 2 && ctrlDown) {
    mXformCenter.x = ix;
    mXformCenter.y = iy;
    mCenterDefined = 1;
    markXformCenter(ix, iy);
    return 1;
  }

  // If button for 2nd fixed point, toggle it on or off
  if (button == 3 && ctrlDown) {
    if (mFixedPtDefined) {
      mFixedPtDefined = 0;
    } else {

      // Define center if it is not already defined
      if (!mCenterDefined && 
          defaultXformCenter(mXformCenter.x, mXformCenter.y)) {
        endContourShift();
        return 0;
      }
      mCenterDefined = 1;

      // Save coordinates and show both marks
      mXformFixedPt.x = ix;
      mXformFixedPt.y = iy;
      mFixedPtDefined = 1;
    }
    markXformCenter(mXformCenter.x, mXformCenter.y);
    return 1;
  }

  if (button == 1) {
    
    // Get base for shift as current point minus mouse position
    sContShiftBase.x = cont->pts[pt].x - ix;
    sContShiftBase.y = cont->pts[pt].y - iy;
  } else {

    // Use defined center if one was set
    if (mCenterDefined) {
      sContShiftBase.x = mXformCenter.x;
      sContShiftBase.y = mXformCenter.y;
    } else {

      // Otherwise get center for transforms as center of mass
      if (defaultXformCenter(sContShiftBase.x, sContShiftBase.y)) {
        endContourShift();
        return 0;
      }
    }
    markXformCenter(sContShiftBase.x, sContShiftBase.y);
    return 1;
  }
  return 0;
}

  // Get default center for transforms as center of mass
int ZapFuncs::defaultXformCenter(float &xcen, float &ycen)
{
  int   pt, err, co, ob, curco, curob;
  float area, areaSum;
  Ipoint cent, centSum;
  Imod *imod = mVi->imod;
  Iobj *obj;
  Icont *cont;

  // Loop on contours and analyze current or selected ones
  imodGetIndex(imod, &curob, &curco, &pt);
  if (curco < 0 && mLassoOn && !mDrawingLasso) {
    cont = getLassoContour();
    if (!cont)
      return 1;
    imodContourCenterOfMass(cont, &cent);
    xcen = cent.x;
    ycen = cent.y;
    return 0;
  }

  xcen = ycen = 0;
  centSum.x = centSum.y = 0;
  areaSum = 0;
  for (ob = 0; ob < imod->objsize; ob++) {
    obj = &imod->obj[ob];
    if (iobjScat(obj->flags))
      continue;
    for (co = 0; co < obj->contsize; co++) {
      
      if ((ob == curob && co == curco) || 
          imodSelectionListQuery(mVi, ob, co) > -2) {
        cont = &obj->cont[co];
        if (!iobjClose(obj->flags) && (cont->flags & ICONT_WILD))
          continue;
        
        // For each contour add centroid to straight sum, 
        // accumulate area-weighted sum also
        imodContourCenterOfMass(cont, &cent);
        area = imodContourArea(cont);
        areaSum += area;
        xcen += cent.x;
        ycen += cent.y;
        centSum.x += cent.x * area;
        centSum.y += cent.y * area;
        err++;
      }
    }
  }

  if (!err)
    return 1;

  // Use plain sum if area small or if only one contour, otherwise
  // use an area-weighted sum
  if (areaSum < 1. || err == 1) {
    xcen /= err;
    ycen /= err;
  } else if (areaSum >= 1. && err > 1) {
    xcen = centSum.x / areaSum;
    ycen = centSum.y / areaSum;
  }
  return 0;
}

/*
 * Shift or transform contour upon mouse move
 */
void ZapFuncs::shiftContour(int x, int y, int button, int shiftDown)
{
  int   pt, err, ob, co, curco, curob, iz;
  float ix, iy;
  float mat[2][2];
  Imod *imod = mVi->imod;
  Iobj *obj;
  Icont *cont = checkContourShift(pt, err);
  if (err)
    return;

  if (button == 1) {
    
    // Shift by change from original mouse pos minus change in current 
    // point position
    getixy(x, y, ix, iy, iz);
    ix += sContShiftBase.x - cont->pts[pt].x;
    iy += sContShiftBase.y - cont->pts[pt].y;
    limitContourShift(cont, ix, iy);
  } else {

    // Get transformation matrix if 2nd or 3rd button
    err = button + (button == 3 && shiftDown ? 1 : 0);
    if (mouseXformMatrix(x, y, err, mat))
      return;
  }

  imodGetIndex(imod, &curob, &curco, &pt);
  if (curco < 0 && mLassoOn && !mDrawingLasso) {
    transformContour(cont, mat, ix, iy, button);
    imodDraw(mVi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD );
    return;
  }

  // Loop on contours and act on current or selected ones
  for (ob = 0; ob < imod->objsize; ob++) {
    obj = &imod->obj[ob];
    if (iobjScat(obj->flags))
      continue;
    for (co = 0; co < obj->contsize; co++) {
      if ((ob == curob && co == curco) ||
          imodSelectionListQuery(mVi, ob, co) > -2) {

        cont = &obj->cont[co];
        if (!iobjClose(obj->flags) && (cont->flags & ICONT_WILD))
          continue;
        
        // Register changes first time only
        if (!mShiftRegistered)
          mVi->undo->contourDataChg(ob, co);
        transformContour(cont, mat, ix, iy, button);
      }
    }
  }

  mShiftRegistered = 1;
  imodDraw(mVi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD );
}

/*
 * Limit the shift to keep the contour intersecting with image
 */
void ZapFuncs::limitContourShift(Icont *cont, float &ix, float &iy)
{
  Ipoint pmin, pmax;
  imodContourGetBBox(cont, &pmin, &pmax);
  
  if (pmin.x + ix >= mVi->xsize - 1)
    ix = mVi->xsize - pmin.x - 1.;
  else if (pmax.x + ix <= 0)
    ix = -pmax.x + 1.;
  if (pmin.y + iy >= mVi->ysize - 1)
    iy = mVi->ysize - pmin.y - 1.;
  else if (pmax.y + iy <= 0)
    iy = -pmax.y + 1.;
}

/*
 * Transform one contour with shift of matrix
 */
void ZapFuncs::transformContour(Icont *cont, float mat[2][2], float ix, float iy, 
                                int button)
{
  int pt;
  if (button == 1) {
    
    // Shift points
    for (pt = 0; pt < cont->psize; pt++) {
      cont->pts[pt].x += ix;
      cont->pts[pt].y += iy;
    }
  } else {
          
    // Transform points
    for (pt = 0; pt < cont->psize; pt++) {
      ix = mat[0][0] * (cont->pts[pt].x - sContShiftBase.x) + mat[0][1] *
        (cont->pts[pt].y - sContShiftBase.y) + sContShiftBase.x;
      iy = mat[1][0] * (cont->pts[pt].x - sContShiftBase.x) + mat[1][1] *
        (cont->pts[pt].y - sContShiftBase.y) + sContShiftBase.y;
      cont->pts[pt].x = ix;
      cont->pts[pt].y = iy;
    }
  }
}

/*
 * Compute transform matrix from the mouse move
 */
int ZapFuncs::mouseXformMatrix(int x, int y, int type, 
                            float mat[2][2])
{
  float strThresh = 0.001;
  float rotThresh = 0.05;
  float delCrit = 20.;
  float fixPtCritSq = 1000.;
  double dxf, dyf, dxl, dyl, dxn, startang, endang, radst, radnd, delrad, drot, scale;
  double dyn, distsq, disxn, disyn, disxl, disyl, p2lsq, tmin;
  int xcen, ycen, xfix, yfix;

  xcen = xpos(sContShiftBase.x);
  ycen = ypos(sContShiftBase.y);

  // Compute starting/ending  angle and radii as in midas
  // l for last, n for new, f for fixed
  dxl = mLmx - xcen;
  dyl = mWiny - 1 - mLmy - ycen;
  if(dxl > -delCrit && dxl < delCrit && dyl > -delCrit && dyl < delCrit)
    return 1;
  radst = sqrt((dxl*dxl + dyl*dyl));
  startang = atan2(dyl, dxl) / RADIANS_PER_DEGREE;
  //imodPrintStderr("%d %d %f %f %f %f\n", xcen, ycen, dxl, dyl, radst, 
  //                startang);

  dxn = x - xcen;
  dyn = mWiny - 1 - y - ycen;
  if(dxn > -delCrit && dxn < delCrit && dyn > -delCrit && dyn < delCrit)
    return 1;
  radnd = sqrt((dxn*dxn + dyn*dyn));
  endang = atan2(dyn, dxn) / RADIANS_PER_DEGREE;
  //imodPrintStderr("%f %f %f %f\n", dxn, dyn, radnd, endang);

  drot = 0.;
  scale = 1.;
  delrad = 1.;
  if (type == 3 && mFixedPtDefined) {

    // Get fixed point in window coordinates and test if separation from center is enough
    // This code slavishly imitates midas but all the names are changed
    xfix = xpos(mXformFixedPt.x);
    yfix = ypos(mXformFixedPt.y);
    dxf = xfix - xcen;
    dyf = yfix - ycen;
    distsq = dxf * dxf + dyf + dyf;
    if (distsq < fixPtCritSq)
      return 1;

    // Then make sure each point is far enough away from line
    tmin = (dxn * dxf + dyn * dyf) / distsq;
    disxn = tmin * dxf - dxn;
    disyn = tmin * dyf - dyn;
    p2lsq = disxn * disxn + disyn * disyn;
    if (p2lsq < fixPtCritSq)
      return 1;

    tmin = (dxl * dxf + dyl * dyf) / distsq;
    disxl = tmin * dxf - dxl;
    disyl = tmin * dyf - dyl;
    p2lsq = disxl * disxl + disyl * disyl;
    if (p2lsq < fixPtCritSq)
      return 1;

    // If both points are on same side of line, do the change
    if (disxl * disxn + disyl * disyn <= 0.)
      return 1;
    distsq = dyl * dxf - dxl * dyf;
    if (fabs((double)distsq) < 1.e-5)
      return 1;
    mat[0][0] = (dxf * dyl - dxn * dyf) / distsq;
    mat[0][1] = (dxf * dxn - dxl * dxf) / distsq;
    mat[1][0] = (dyf * dyl - dyn * dyf) / distsq;
    mat[1][1] = (dxf * dyn - dxl * dyf) / distsq;
    return 0;
  }

  if (type == 2) {

    // Compute rotation from change in angle
    drot = endang - startang;
    if (drot < -360.)
      drot += 360.;
    if (drot > 360.)
      drot -= 360.;
    drot = rotThresh * floor(drot / rotThresh + 0.5);
    endang = 0.;
    if (!drot)
      return 1;
  } else {

    // Compute stretch from change in radius; set up as stretch or scale
    delrad = (radnd - radst) / radst;
    delrad = strThresh * floor(delrad / strThresh + 0.5);
    if (!delrad)
      return 1;
    delrad += 1.;
    if (type == 4) {
      scale = delrad;
      delrad = 1.;
      endang = 0.;
    }
  }

  // Compute matrix 
  rotmagstrToAmat(drot, scale, delrad, endang, &mat[0][0], &mat[0][1], &mat[1][0],
                  &mat[1][1]);
  //imodPrintStderr("%f %f %f %f %f %f %f %f\n", drot, scale, delrad, endang,
  //            mat[0][0], mat[0][1], mat[1][0], mat[1][1]);
  return 0;
}

/*
 * Mark the center of transformation with a star in extra object
 */
void ZapFuncs::markXformCenter(float ix, float iy)
{
  int i, which;
  float starEnd[6] = {0., 8., 7., 4., 7., -4.};
  Iobj *obj = ivwGetAnExtraObject(mVi, mShiftObjNum);
  Icont *cont;
  Ipoint tpt;
  Istore store;

  if (!obj)
    return;

  // Clear out the object and set to yellow non scattered
  ivwClearAnExtraObject(mVi, mShiftObjNum);
  imodObjectSetColor(obj, 1., 1., 0.);
  obj->flags &= ~IMOD_OBJFLAG_SCAT;
  obj->pdrawsize = 0;
  obj->linewidth = 1;
  tpt.z = mSection;

  // Add three contours for lines
  for (which = 0; which < (mFixedPtDefined ? 2 : 1); which++) {
    for (i = 0; i < 3; i++) {
      cont = imodContourNew();
      if (!cont)
        break;
      tpt.x = ix + starEnd[i * 2];
      tpt.y = iy + starEnd[i * 2 + 1];
      imodPointAppend(cont, &tpt);
      tpt.x = ix - starEnd[i * 2];
      tpt.y = iy - starEnd[i * 2 + 1];
      imodPointAppend(cont, &tpt);
      imodObjectAddContour(obj, cont);
      free(cont);
      if (which) {
        store.type = GEN_STORE_COLOR;
        store.flags = GEN_STORE_BYTE << 2;
        store.index.i = 3 + i;
        store.value.i = 0;
        store.value.b[0] = 255;
        istoreInsert(&obj->store, &store);
      }
    }
    ix = mXformFixedPt.x;
    iy = mXformFixedPt.y;
  }
  mCenterMarked = 1;
  imodDraw(mVi, IMOD_DRAW_MOD);
}


/********************************************************
 * conversion functions between image and window cords. */

/* DNM 9/15/03: use the possibly slightly different x zoom.  This might make
 a few tenths of a pixel difference */

/* return x pos in window for given image x cord. */
int ZapFuncs::xpos(float x)
{
  return( (int)(((x - mXposStart) * mXzoom) + mXborder));
}

/* return y pos in window for given image y cord. */
int ZapFuncs::ypos(float y)
{
  return((int)(((y - mYposStart) * mZoom) + mYborder));
}

/* returns image coords in x,y, z, given mouse coords mx, my */
void ZapFuncs::getixy(int mx, int my, float &x, float &y, int &z)
{
  int indx, indy, indp, indmid;

  // 10/31/04: winy - 1 maps to 0, not winy, so need a -1 here
  my = mWiny - 1 - my;

  if (!mNumXpanels) {
    z = mSection;
  } else {
    panelIndexAndCoord(mPanelXsize, mNumXpanels, mPanelGutter, 
                       mPanelXborder, mx, indx);
    panelIndexAndCoord(mPanelYsize, mNumYpanels, mPanelGutter, 
                       mPanelYborder, my, indy);
    if (indx < 0 || indy < 0) {
      z = -1;
    } else {
      indp = indx + indy * mNumXpanels;
      indmid = (mNumXpanels * mNumYpanels - 1) / 2;
      z = mSection + (indp - indmid) * mPanelZstep;
      if (z < 0 || z >= mVi->zsize)
        z = -1;
    }
  }
  x = ((float)(mx - mXborder) / mXzoom) + (float)mXposStart;
  y = ((float)(my - mYborder) / mZoom) + (float)mYposStart;
}

// Determine which panel a point is in for one direction and get the position
// within that panel
void ZapFuncs::panelIndexAndCoord(int size, int num, int gutter, int border, 
                                  int &pos, int &panelInd)
{
  int spacing = size + gutter;
  //imodPrintStderr("size %d, num %d, gutter %d, border %d, pos win %d",
  //            size, num, gutter, border, pos);
  panelInd = (pos - border) / spacing;
  pos -= border + panelInd * spacing;
  if (pos < 0 || pos >= size || panelInd < 0 || panelInd >= num)
    panelInd = -1;
  //imodPrintStderr(" panel %d ind %d\n", pos, panelInd);
}

/*
 * Convert image rubberband coordinates to outer window coordinates, with
 * optional clipping to window limits
 */
void ZapFuncs::bandImageToMouse(int ifclip)
{
  mRbMouseX0 = xpos(mRbImageX0) - 1;
  mRbMouseX1 = xpos(mRbImageX1);
  mRbMouseY0 = mWiny - 1 - ypos(mRbImageY1);
  mRbMouseY1 = mWiny - ypos(mRbImageY0);
  if (ifclip) {
    if (mRbMouseX0 < 0)
      mRbMouseX0 = 0;
    if (mRbMouseX1 >= mWinx)
      mRbMouseX1 = mWinx - 1;
    if (mRbMouseY0 < 0)
      mRbMouseY0 = 0;
    if (mRbMouseY1 >= mWiny)
      mRbMouseY1 = mWiny - 1;
  }
  /* imodPrintStderr("Image %.1f,%.1f : %.1f,%.1f  to mouse %d,%d : %d,%d\n", 
                  mRbImageX0, mRbImageY0, mRbImageX1, mRbImageY1,
                  mRbMouseX0, mRbMouseY0, mRbMouseX1, mRbMouseY1); */
}

/*
 * Convert rubberband window coordinates to inside image coordinates, with
 * optional clipping to image limits
 */
void ZapFuncs::bandMouseToImage(int ifclip)
{
  int iz;
  getixy(mRbMouseX0 + 1, mRbMouseY1 - 1, mRbImageX0, mRbImageY0, iz);
  getixy(mRbMouseX1, mRbMouseY0, mRbImageX1, mRbImageY1, iz);
  
  if (ifclip) {
    if (mRbImageX0 < 0)
      mRbImageX0 = 0;
    if (mRbImageX1 > mVi->xsize)
      mRbImageX1 = mVi->xsize;
    if (mRbImageY0 < 0)
      mRbImageY0 = 0;
    if (mRbImageY1 > mVi->ysize)
      mRbImageY1 = mVi->ysize;
  }
  /* imodPrintStderr("Mouse %d,%d : %d,%d  to image %.1f,%.1f : %.1f,%.1f\n", 
                  mRbMouseX0, mRbMouseY0, mRbMouseX1, mRbMouseY1,
                  mRbImageX0, mRbImageY0, mRbImageX1, mRbImageY1); */
}

/*
 * Sets the limits parameter to be used for snapshotting to null or to subarea limits
 * that are placed in the supplied limarr array
 */
void ZapFuncs::setSnapshotLimits(int **limits, int *limarr)
{
  *limits = NULL;
  if (mRubberband) {
    *limits = limarr;
    bandImageToMouse(1);
    limarr[0] = mRbMouseX0 + 1;
    limarr[1] = mWiny - mRbMouseY1;
    limarr[2] = mRbMouseX1 - 1 - mRbMouseX0;
    limarr[3] = mRbMouseY1 - 1 - mRbMouseY0;
  }
}

/*
 * Return the correct low and high section values from the zap parameter.
 * Returns true if the rubberband is in use and at least of the two values have
 * been set.
 */
bool ZapFuncs::getLowHighSection(int &lowSection, int &highSection)
{
  bool lowHighSectionSet = true;
  // If rubberband is not enabled, set both ints to the current section
  // (original functionality) and return false.  This is probably not necessary
  // because the sections values are reset when the rubberband is turned off.
  if (mRubberband + mStartingBand == 0) {
    lowHighSectionSet = false;
    lowSection = mSection + 1;
    highSection = mSection + 1;
  }
  else {
    QString low = mQtWindow->lowSection();
    QString high = mQtWindow->highSection();
    // If neither of the section values have been set, fall back to the original
    // functionality and return false.
    if (low.isEmpty() && high.isEmpty()) {
      lowHighSectionSet = false;
      lowSection = mSection + 1;
      highSection = mSection + 1;
    }
    else {
      lowSection = low.toInt();
      highSection = high.toInt();
      // If only one of the section values have been set, set the other one to
      // the max/min.
      if (low.isEmpty()) {
        lowSection = 1;
      } else if (high.isEmpty()) {
          highSection = mVi->zsize;
      }
    }
  }
  // LowSection cannot be bigger then highSection.
  if (lowSection > highSection) {
    int temp = lowSection;
    lowSection = highSection;
    highSection = temp;
  }
  return lowHighSectionSet;
}

/*
 * Prints window size and image coordinates in Info Window.  Returns the partial
 * trimvol command.  Actually printing to Info Window is optional.  The allows
 * this function to be reused for running trimvol.
 */
QString ZapFuncs::printInfo(bool toInfoWindow)
{
  float xl, xr, yb, yt;
  int ixl, ixr, iyb, iyt, iz;
  int fx, fy, fz, llx, lly, llz, xpad, ypad, zpad;
  int ixcen, iycen, ixofs, iyofs, imx, imy, itmp;
  int bin = mVi->xybin;
  bool ifpad, flipped = mVi->li->axis == 2;

  ivwControlPriority(mVi, mCtrl);
  ImodInfoWin->raise();
  if (mRubberband) {
    xl = mRbImageX0;
    yb = mRbImageY0;
    xr = mRbImageX1;
    yt = mRbImageY1;
  } else {
    getixy(0, -1, xl, yt, iz);
    getixy(mWinx, mWiny-1, xr, yb, iz);
  }
  ixl = (int)floor(xl + 0.5);
  ixr = (int)floor(xr - 0.5);
  iyb = (int)floor(yb + 0.5);
  iyt = (int)floor(yt - 0.5);
  ifpad = ixl < 0 || iyb < 0 || ixr >= mVi->xsize || iyt >= mVi->ysize;
  imx = bin * (ixr + 1 - ixl);
  imy = bin * (iyt + 1 - iyb);
  ixcen = bin * (ixr + 1 + ixl)/2;
  iycen = bin * (iyt + 1 + iyb)/2;
  ixofs = ixcen - (bin * mVi->xsize)/2;
  iyofs = iycen - (bin * mVi->ysize)/2;
  ixl = B3DMAX(0, ixl);
  iyb = B3DMAX(0, iyb);
  ixr = B3DMIN(ixr, mVi->xsize - 1);
  iyt = B3DMIN(iyt, mVi->ysize - 1);

  int lowSection, highSection;
  getLowHighSection(lowSection, highSection);
  if (lowSection < 1 || highSection < 1 || highSection > mVi->zsize) {
    wprint("ERROR: %s is out of range.\n", flipped ? "-y" : "-z");
    if (!toInfoWindow)
      return "";
  }

  // Get load offsets and padding
  if (ivwGetImagePadding(mVi, (iyb + iyt) / 2, (lowSection + highSection) / 2,
                         ivwWindowTime(mVi, mTimeLock), llx, xpad, fx, lly, ypad, fy,
                         llz, zpad, fz) < 0)
    llx = lly = llz = xpad = ypad = zpad = 0;

  // If flipped, adjust the section numbers and swap the load offsets, not the pads
  if (flipped) {
    itmp =  mVi->zsize + 1 - lowSection;
    lowSection = mVi->zsize + 1 - highSection;
    highSection = itmp;
    itmp = llz;
    llz = lly;
    lly = itmp;
  }

  // Adjust for load offsets/padding
  ixl += llx - xpad;
  ixr += llx - xpad;
  iyb += lly - ypad;
  iyt += lly - ypad;
  lowSection += llz - zpad;
  highSection += llz - zpad;

  // Adjust for binning
  ixl *= bin;
  iyb *= bin;
  ixr = ixr * bin + bin - 1;
  iyt = iyt * bin + bin - 1;
  lowSection = mVi->zbin * (lowSection - 1) + 1;
  highSection *= mVi->zbin;
  
  QString trimvol;
  trimvol.sprintf("  trimvol -x %d,%d %s %d,%d %s %d,%d", ixl + 1, ixr + 1, 
           flipped ? "-z" : "-y", iyb + 1, iyt + 1, flipped ? "-rx -y" : "-z",
           lowSection, highSection);

  if (toInfoWindow) {
    wprint("%senter (%d,%d); offset %d,%d\n", bin > 1 ? "Unbinned c" : "C",
           ixcen + 1, iycen + 1, ixofs, iyofs);
    wprint("%smage size: %d x %d;   To excise:\n", ifpad ? "Padded i" : "I",
           imx, imy);
    wprint("%s\n", LATIN1(trimvol));
  }
  return trimvol;
}

/*
 * Resize window to fit either whole image or part in rubber band 
 */
void ZapFuncs::resizeToFit()
{
  int width, height, neww, newh;
  int dx, dy, newdx, newdy;
  float xl, xr, yb, yt;
  width = mQtWindow->width();
  height = mQtWindow->height();
  QRect pos = ivwRestorableGeometry(mQtWindow);
  dx = pos.x();
  dy = pos.y();
  /* imodPrintStderr("dx %d dy %d\n", dx, dy); */
  if (mRubberband) {
    /* If rubberbanding, set size to size of band, and offset
       image by difference between band and window center */
    bandImageToMouse(0);
    xl = mRbImageX0;
    yb = mRbImageY0;
    xr = mRbImageX1;
    yt = mRbImageY1;
    neww = mRbMouseX1 -1 - mRbMouseX0 + width - mWinx;
    newh = mRbMouseY1 -1 - mRbMouseY0 + height - mWiny;
    mXtrans = (int)(-(xr + xl - mVi->xsize) / 2);
    mYtrans = (int)(-(yt + yb - mVi->ysize) / 2);

    // 3/6/05: turn off through common function to keep synchronized
    toggleRubberband(false);
  } else {
    /* Otherwise, make window the right size for the image */
    neww = (int)(mZoom * mVi->xsize + width - mWinx);
    newh = (int)(mZoom * mVi->ysize + height - mWiny);
  }

  diaLimitWindowSize(neww, newh);
  newdx = dx + width / 2 - neww / 2;
  newdy = dy + height / 2 - newh / 2;
  diaLimitWindowPos(neww, newh, newdx, newdy);

  if (imodDebug('z'))
    imodPrintStderr("configuring widget...");

  /* imodPrintStderr("newdx %d newdy %d\n", newdx, newdy); */
  mQtWindow->resize(neww, newh);
  mQtWindow->move(newdx, newdy);

  /* DNM 9/12/03: remove the ZAP_EXPOSE_HACK, and a second set geometry that
     was needed temporarily with Qt 3.2.1 on Mac */

  if (imodDebug('z'))
    imodPrintStderr("back\n");
}
     
/*
 * Set the control priority and set flag to record subarea and do float
 */
void ZapFuncs::setControlAndLimits()
{
  ivwControlPriority(mVi, mCtrl);
  if (!mNumXpanels)
    mRecordSubarea = 1;
}

/*
 * Record the limits of the image displayed in the window or in the rubber band
 */
void ZapFuncs::setAreaLimits()
{
  int iz, minArea = 16;
  float xl, xr, yb, yt, delta;
  if (mRubberband) {
    xl = mRbImageX0;
    yb = mRbImageY0;
    xr = mRbImageX1;
    yt = mRbImageY1;

    // Enforce a minimum size so 
    delta = (minArea - (xr - xl)) / 2.;
    if (delta > 0.) {
      xl -= delta;
      xr += delta;
    }
    delta = (minArea - (yt - yb)) / 2;
    if (delta > 0.) {
      yb -= delta;
      yt += delta;
    }
  } else {
    getixy(0, 0, xl, yt, iz);
    getixy(mWinx, mWiny, xr, yb, iz);
  }
  sSubStartX = B3DMAX((int)(xl + 0.5), 0);
  sSubEndX = B3DMIN((int)(xr - 0.5), mVi->xsize - 1);
  sSubStartY = B3DMAX((int)(yb + 0.5), 0);
  sSubEndY = B3DMIN((int)(yt - 0.5), mVi->ysize - 1);
  if (imodDebug('z'))
    imodPrintStderr("Set area %d %d %d %d\n", sSubStartX, sSubEndX, sSubStartY,
                    sSubEndY);
}     

/*
 * An external call for taking a snapshot of given format and given an optional name
 * that is returned to the called
 */
int ZapFuncs::namedSnapshot(QString &fname, int format, bool checkConvert, bool fullArea)
{
  int *limits = NULL;
  int limarr[4];
  mShowslice = mShowedSlice;
  draw();
  if (!fullArea)
    setSnapshotLimits(&limits, limarr);
  return b3dNamedSnapshot(fname, "zap", format, limits, checkConvert);
}

/*
 * Returns size and limits within rubberband of the zoomed down image being displayed,
 * and corresponding limits for the unzoomed stored image
 */
B3dCIImage *ZapFuncs::zoomedDownImage(int subset, int &nxim, int &nyim, int &ixStart,
                                      int &iyStart, int &nxUse, int &nyUse, int &uzXstart,
                                      int &uzYstart, int &uzXuse, int &uzYuse)
{
  int time, llX, leftXpad, rightXpad, llY, leftYpad, rightYpad, llZ, leftZpad, rightZpad;
  int uzXend, uzYend;
  float xl, xr, yb, yt;
  time = ivwWindowTime(mVi, mTimeLock);

  if (!mHqgfx || mZoom > b3dZoomDownCrit() || !App->rgba || !mImage || 
      mVi->cramp->falsecolor)
    return NULL;
  ixStart = 1;
  iyStart = 1;
  nxim = (int)(mXdrawsize * mZoom);
  nyim = (int)(mYdrawsize * mZoom);
  nxUse = nxim - 2;
  nyUse = nyim - 2;
  if (subset && mRubberband) {
    bandImageToMouse(1);
    ixStart = B3DMAX(1, mRbMouseX0 + 1 - mXborder);
    iyStart = B3DMAX(1, mWiny - mRbMouseY1 + 1 - mYborder);
    nxUse = B3DMIN(nxUse - ixStart, mRbMouseX1 - mRbMouseX0 - 1);
    nyUse = B3DMIN(nyUse - iyStart, mRbMouseY1 - mRbMouseY0 - 1);
  }

  // Get window limits of unzoomed image and limit by the possible rubber band subarea
  getixy(0, 0, xl, yt, uzXend);
  getixy(mWinx, mWiny, xr, yb, uzXend);
  uzXstart = B3DMAX((int)(xl + 0.5), 0);
  uzXend = B3DMIN((int)(xr - 0.5), mVi->xsize - 1);
  uzYstart = B3DMAX((int)(yb + 0.5), 0);
  uzYend = B3DMIN((int)(yt - 0.5), mVi->ysize - 1);
  if (subset) {
    uzXstart  = B3DMAX(uzXstart, sSubStartX);
    uzXend = B3DMIN(uzXend, sSubEndX);
    uzYstart = B3DMAX(uzYstart, sSubStartY);
    uzYend = B3DMIN(uzYend, sSubEndY);
  }
  uzXuse = uzXend + 1 - uzXstart;
  uzYuse = uzYend + 1 - uzYstart;

  // Get extent of padded region in image and use it to limit the subarea
  if (ivwGetImagePadding(mVi, -1, mSection, time, llX, leftXpad, rightXpad, llY, leftYpad,
                         rightYpad, llZ, leftZpad, rightZpad))
    return mImage;

  if (leftXpad || leftYpad || rightXpad || rightYpad) {
    imodInfoLimitSubarea(xpos(leftXpad) - mXborder, xpos(mVi->xsize - rightXpad) - 
                         mXborder, ypos(leftYpad) - mYborder, ypos(mVi->ysize - rightYpad)
                         - mYborder, ixStart, iyStart, nxUse, nyUse);
    imodInfoLimitSubarea(leftXpad, mVi->xsize - rightXpad, leftYpad, 
                         mVi->ysize - rightYpad, uzXstart, uzYstart, uzXuse, uzYuse);
  }
  return mImage;
}

/*
 * Toggle the rubber band
 */
void ZapFuncs::toggleRubberband(bool drawWin)
{
  if (mRubberband || mStartingBand) {
    mRubberband = 0;
    mStartingBand = 0;
    mBandChanged = 1;
    setControlAndLimits();
  } else {
    if (mLassoOn)
      toggleLasso(false);
    if (mDrawingArrow)
      toggleArrow(false);
    mStartingBand = 1;
    endContourShift();
    /* Eliminated old code for making initial band */
  }

  setMouseTracking();
  mQtWindow->setLowHighSectionState(mRubberband + mStartingBand);
 
  // 3/6/05: synchronize the toolbar button
  mQtWindow->setToggleState(ZAP_TOGGLE_RUBBER, 
                                mRubberband + mStartingBand);
  setCursor(mMousemode, utilNeedToSetCursor());
  if (drawWin)
    draw();

  // 4/6/09: This was needed before and after the draw for the Mac Qt 4.5.0
  setCursor(mMousemode, utilNeedToSetCursor());
}

/*
 * Shift the rubberband by a desired amount to the extent possible
 */
void ZapFuncs::shiftRubberband(float idx, float idy)
{
  if (mRbImageX0 + idx < 0)
    idx = -mRbImageX0;
  if (mRbImageX1 + idx > mVi->xsize)
    idx = mVi->xsize - mRbImageX1;
  if (mRbImageY0 + idy < 0)
    idy = -mRbImageY0;
  if (mRbImageY1 + idy > mVi->ysize)
    idy = mVi->ysize - mRbImageY1;
  mRbImageX0 += idx;
  mRbImageX1 += idx;
  mRbImageY0 += idy;
  mRbImageY1 += idy;
}

/*
 * Toggle the lasso contour
 */
void ZapFuncs::toggleLasso(bool drawWin)
{
  if (mLassoOn) {
    ivwFreeExtraObject(mVi, mLassoObjNum);
  } else {
    if (mRubberband || mStartingBand)
      toggleRubberband(false);
    if (mDrawingArrow)
      toggleArrow(false);
    endContourShift();
    mLassoObjNum = ivwGetFreeExtraObjectNumber(mVi);
  }
  mLassoOn = !mLassoOn;
  mDrawingLasso = mLassoOn;
  setMouseTracking();
  mQtWindow->setToggleState(ZAP_TOGGLE_LASSO, mLassoOn ? 1 : 0);

  // Set it to a modeling cursor
  setCursor(mMousemode, true);
  if (drawWin)
    draw();
  setCursor(mMousemode, true);
}

/*
 * Toggle the arrow: turn off lasso or rubberband if they are starting
 */
void ZapFuncs::toggleArrow(bool drawWin)
{
  Ipoint zero = {0., 0., 0.};
  if (!mArrowOn) {
    if (mStartingBand)
      toggleRubberband(false);
    if (mDrawingLasso)
      toggleLasso(false);
    endContourShift();
    mArrowHead.push_back(zero);
    mArrowTail.push_back(zero);
  } else {
    mArrowHead.pop_back();
    mArrowTail.pop_back();
  }
  mArrowOn = !mArrowOn;
  mDrawingArrow = mArrowOn;
  mQtWindow->setToggleState(ZAP_TOGGLE_ARROW, mArrowOn ? 1 : 0);

  setCursor(mMousemode, true);
  if (drawWin)
    draw();
  setCursor(mMousemode, true);
}

/*
 * Clear out all arrows
 */
void ZapFuncs::clearArrows()
{
  if (mArrowOn)
    toggleArrow(false);
  mArrowHead.resize(0);
  mArrowTail.resize(0);
  draw();
}

/*
 * Start a new arrow, keeping an existing one
 */
void ZapFuncs::startAddedArrow()
{
  int ind = mArrowHead.size();
  if (mDrawingArrow && !mArrowHead[ind].x && !mArrowTail[ind].x && 
      !mArrowHead[ind].y && !mArrowTail[ind].y)
    return;
  mArrowOn = false;
  toggleArrow(false);
}

/*
 * Just get the contour
 */
Icont *ZapFuncs::getLassoContour()
{
  Iobj *obj;
  if (!mLassoOn)
    return NULL;
  obj = ivwGetAnExtraObject(mVi, mLassoObjNum);
  return obj ? obj->cont : NULL;
}

/*
 * Set mouse tracking based on state of all governing flags
 */
void ZapFuncs::setMouseTracking()
{
  mGfx->setMouseTracking(sInsertDown != 0 || mRubberband || (mLassoOn && !mDrawingLasso)
                         || sPixelViewOpen || mVi->trackMouseForPlugs);
}

/*
 * Take a snapshot by montaging at higher zoom
 */
void ZapFuncs::montageSnapshot(int snaptype)
{
  int ix, iy, xFullSize, yFullSize, xTransStart, yTransStart, xTransDelta;
  int yTransDelta, xCopyDelta, yCopyDelta, xTransSave, yTransSave, hqSave;
  int showSlice, numChunks;
  int fromXoff, fromYoff, toXoff, toYoff, overhalf,xCopy,yCopy;
  unsigned char *framePix, **fullPix, **linePtrs;
  double zoomSave;
  static int fileno = 0;
  int factor = imcGetMontageFactor();
  float scalingFactor = factor;
  ScaleBar *barReal = scaleBarGetParams();
  ScaleBar barSaved;

  // Save translations and zoom 
  xTransSave = mXtrans;
  yTransSave = mYtrans;
  zoomSave = mZoom;
  hqSave = mHqgfx;

  if (imcGetSnapWholeMont()) {
    for (factor = 1; factor < 31; factor++) {
      if (factor * mWinx >= mVi->xsize && factor * mWiny >=
          mVi->ysize)
        break;
    }
    if (factor == 1) {
      wprint("\aImage already fits in window at zoom 1, no montage needed.\n");
      return;
    }
    if (factor == 31) {
      wprint("\aImage is too large for full montage snapshot in this "
             "window.\n");
      return;
    }
    mZoom = 1. / factor;
    scalingFactor = 1. / zoomSave;
    draw();
  }

  // Get coordinates and offsets and buffers
  setAreaLimits();
  if (getMontageShifts(factor, mXstart, mXborder,
                       mVi->xsize, mWinx,
                       xTransStart, xTransDelta, xCopyDelta, xFullSize) ||
      getMontageShifts(factor, mYstart, mYborder, 
                       mVi->ysize, mWiny,
                       yTransStart, yTransDelta, yCopyDelta, yFullSize)) {
    wprint("\aThere is too much border around image for montage snapshot.\n");
    return;
  }
  if (utilStartMontSnap(mWinx, mWiny, xFullSize, yFullSize,
                        scalingFactor, barSaved, numChunks, &framePix,
                        &fullPix, &linePtrs)) {
    wprint("\aFailed to get memory for snapshot buffers.\n");
    return;
  }

  // On Quadro card (?), it is necessary to defer the autoswap for montaging
  // It's not clear why this isn't needed for other snapshots
  if (App->doublebuffer) {
    mGfx->setBufferSwapAuto(false);
    glReadBuffer(GL_BACK);
  }

  // Set up scaling
  if (imcGetScaleSizes()) {
    sScaleSizes = imcGetSizeScaling();
    if (sScaleSizes == 1)
      sScaleSizes = B3DNINT(scalingFactor * B3DMIN(1., mZoom));
  }

  // Loop on frames, getting pixels and copying them
  mHqgfx = 1;
  mZoom *= factor;
  showSlice = mShowslice;
  for (iy = 0; iy < factor; iy++) {
    for (ix = 0; ix < factor; ix++) {

      // Set up for scale bar if it is the right corner
      utilMontSnapScaleBar(ix, iy, factor, mWinx - 4, mWiny - 4, 
                           mZoom, barSaved.draw);
      
      mXtrans = -(xTransStart + ix * xTransDelta);
      mYtrans = -(yTransStart + iy * yTransDelta);
      draw();
      mShowslice = showSlice;

      // Print scale bar length if it was drawn
      if (mScaleBarSize > 0)
        imodPrintStderr("Scale bar for montage is %g %s\n", mScaleBarSize,
                        imodUnits(mVi->imod));

      glReadPixels(0, 0, mWinx, mWiny, GL_RGBA, GL_UNSIGNED_BYTE, 
                   framePix);
      glFlush();

      // set up copy parameters for full copy, and adjust to skip the overlap
      // after the first piece unless doing panel with bar
      xCopy = mWinx;
      yCopy = mWiny;
      toXoff = ix * xCopyDelta;
      toYoff = iy * yCopyDelta;
      fromXoff = fromYoff = 0;
      if (ix && !(barReal->draw && ix == factor - 1)) {
        overhalf = mWinx - xCopyDelta - 2;
        if (overhalf > 2 && overhalf < xCopy) {
          fromXoff = overhalf;
          toXoff += overhalf;
          xCopy -= overhalf;
        }
      }
      if (iy && !(barReal->draw && iy == factor - 1)) {
        overhalf = mWiny - yCopyDelta - 2;
        if (overhalf > 2 && overhalf < yCopy) {
          fromYoff = overhalf;
          toYoff += overhalf;
          yCopy -= overhalf;
        }
      }

      memLineCpy(linePtrs, framePix, xCopy, yCopy, 4,
               toXoff, toYoff, mWinx, fromXoff, fromYoff);
      if (App->doublebuffer) 
        mGfx->swapBuffers();
    }
  }

  if (App->doublebuffer)
    mGfx->setBufferSwapAuto(true);

  // Reset the file number to zero unless doing movie, then get name and save
  if (!mMovieSnapCount)
    fileno = 0;

  // Save the image then restore display
  utilFinishMontSnap(linePtrs, xFullSize, yFullSize, snaptype - 1,
                     fileno, 3, (float)factor, "zap", "3dmod: Saving zap");

  *barReal = barSaved;
  mXtrans = xTransSave;
  mYtrans = yTransSave;
  mZoom = zoomSave;
  sScaleSizes = 1;
  mHqgfx = hqSave;
  draw();
  utilFreeMontSnapArrays(fullPix, numChunks, framePix, linePtrs);
}

/*
 * Compute shifts and increments for the montage snapshot
 */
int ZapFuncs::getMontageShifts(int factor, int imStart, 
                             int border, int imSize, int winSize,
                             int &transStart, int &transDelta, int &copyDelta,
                             int &fullSize)
{
  int inWin, overlap, imEnd, wofftmp, dstmp, dofftmp, trans;
  imEnd = B3DMIN(imStart + (int)((winSize - border)/ mZoom), imSize);
  inWin = (int)(winSize / (mZoom * factor));
  if (inWin >= imSize - factor)
    return 1;

  // Get trans and back it off to avoid window offset on left
  trans = -(imStart + (inWin - imSize) / 2);
  b3dSetImageOffset(winSize, imSize, mZoom * factor, dstmp, trans,
                    wofftmp, dofftmp, 0);
  if (wofftmp > 0)
    trans--;
  transStart = -trans;

  // Get overlap and delta, back off delta to avoid extra pixels on right
  overlap = B3DMAX(0, (factor * inWin + imStart - imEnd) / (factor - 1));
  transDelta = inWin - overlap;
  copyDelta = mZoom * factor * transDelta;
  fullSize = (factor - 1) * copyDelta + winSize;
  if (fullSize > (int)(mZoom * factor * (imSize - imStart))) {
    transDelta--;
    copyDelta = mZoom * factor * transDelta;
    fullSize = (factor - 1) * copyDelta + winSize;
  }

  if (imodDebug('z'))
    imodPrintStderr("im %d - %d  bord %d win %d  inwin %d overlap %d start %d"
                    " delta %d  copy %d  full %d\n", imStart, imEnd, border,
                    winSize, inWin, overlap, transStart, 
                    transDelta, copyDelta, fullSize);
  return 0;
}

/****************************************************************************/
/* drawing routines.                                                        */

/* Draws the image */
void ZapFuncs::drawGraphics()
{
  ImodView *vi = mVi;
  int bl, wh, ind, ix, iy, iz;
  int time, rgba = App->rgba;
  unsigned char **imageData;
  unsigned char *overImage;
  int overlay = 0;
  int otherSec = mSection + vi->overlaySec;
  double zoom;
  int xDrawsize, yDrawsize, xStart, yStart, tileScale = 1, status, imXsize, imYsize;
  float xOffset, yOffset;
  bool asyncLoad = !mVi->zmovie && imodDialogManager.windowCount(ZAP_WINDOW_TYPE) == 1 &&
    !mvImageDrawingZplanes() && !mNumXpanels && !mVi->loadingImage;

  zoom = mZoom;
  imXsize = vi->xsize;
  imYsize = vi->ysize;
  
  // DNM: this is not needed, 1/12/08
  //  ivwGetLocation(vi, &x, &y, &z);


  // DNM eliminated unused function 1/23/03
  // b3dSetCurPoint(x, y, mSection);

  b3dSetImageOffset(mNumXpanels ? mPanelXsize : mWinx, vi->xsize,
                    mZoom, mXdrawsize, mXtrans, 
                    mXborder, mXstart, 1);

  b3dSetImageOffset(mNumXpanels ? mPanelYsize : mWiny, vi->ysize,
                    mZoom, mYdrawsize, mYtrans, 
                    mYborder, mYstart, 1);
  mXposStart = mXstart;
  mYposStart = mYstart;

  /* Get the time to display and flush if time is different. */
  time = ivwWindowTime(vi, mTimeLock);
  if (time != mTime)
    flushImage();

  // For tile cache, go ahead and get the section area even if doing panels
  // in order to get all the drawing parameters modified once
  if (vi->pyrCache) {
    mGfx->cancelRedraw();
    imageData = vi->pyrCache->getSectionArea(mSection, mXstart, mYstart, mXdrawsize,
                                             mYdrawsize, mZoom, asyncLoad, xDrawsize, 
                                             yDrawsize, xOffset, yOffset, tileScale,
                                             status);
    xStart = 0;
    yStart = 0;
    zoom = mZoom * tileScale;
    imXsize = xDrawsize;
    imYsize = yDrawsize;
    mXposStart += xOffset;
    mYposStart += yOffset;
    if (mXlastStart != mXstart || mXlastSize != mXdrawsize || 
        mYlastStart != mYstart || mYlastSize != mYdrawsize || mLastStatus > 0)
      flushImage();
    mXlastStart = mXstart;
    mXlastSize = mXdrawsize; 
    mYlastStart = mYstart; 
    mYlastSize = mYdrawsize;
    mLastStatus = status;
    if (status > 0)
      mGfx->scheduleRedraw(300);   // 100 was way too often with debug version
  } else {
    xStart = mXstart;
    xDrawsize = mXdrawsize;
    yStart = mYstart;
    yDrawsize = mYdrawsize;
  }
  
  if (!mNumXpanels) {
    if (!vi->pyrCache)
      imageData = ivwGetZSectionTime(vi, mSection, time);

    // If flag set, record the subarea size, clear flag, and do call float to
    // set the color map if necessary.  If the black/white changes, flush image
    if (mRecordSubarea) {
      
      // Set the X zoom to zoom if it hasn't been set yet or is not close to
      // zoom
      if (!mXzoom || fabs(1./mXzoom - 1./mZoom) >= 0.002)
        mXzoom = mZoom;
      setAreaLimits();
      bl = vi->black;
      wh = vi->white;
      imod_info_bwfloat(vi, mSection, time);
      if (App->rgba && (bl != vi->black || wh != vi->white))
        b3dFlushImage(mImage);
    }
    
    b3dDrawBoxout(mXborder, mYborder, 
                  mXborder + (int)(xDrawsize * zoom),
                  mYborder + (int)(yDrawsize * zoom));
    
    // If overlay section is set and legal, get an image buffer and fill it
    // with the color overlay
    if (vi->overlaySec && App->rgba && !vi->rgbStore && otherSec >= 0 &&
        otherSec < vi->zsize && !vi->pyrCache) {
      overImage = (unsigned char *)malloc(3 * vi->xsize * vi->ysize);
      if (!overImage) {
        wprint("\aFailed to get memory for overlay image.\n");
      } else {
        overlay = vi->overlaySec;
        rgba = 3;
        if (vi->whichGreen) {
          fillOverlayRGB(imageData, vi->xsize, vi->ysize, 0, overImage);
          fillOverlayRGB(imageData, vi->xsize, vi->ysize, 2, overImage);
        } else {
          fillOverlayRGB(imageData, vi->xsize, vi->ysize, 1, overImage);
        }
        
        imageData = ivwGetZSectionTime(vi, otherSec, time);
        if (vi->whichGreen) {
          fillOverlayRGB(imageData, vi->xsize, vi->ysize, 1, overImage);
        } else {
          fillOverlayRGB(imageData, vi->xsize, vi->ysize, 0, overImage);
        fillOverlayRGB(imageData, vi->xsize, vi->ysize, 2, overImage);
        }
        imageData = ivwMakeLinePointers(vi, overImage, vi->xsize, vi->ysize, 
                                        MRC_MODE_RGB);
      }
    }
    if (overlay != mOverlay)
      b3dFlushImage(mImage);
    mOverlay = overlay;

    b3dDrawGreyScalePixelsHQ(imageData,
                             imXsize, imYsize,
                             xStart, yStart,
                             mXborder, mYborder,
                             xDrawsize, yDrawsize,
                             mImage,
                             vi->rampbase, 
                             zoom, zoom,
                             mHqgfx, mSection, rgba);
  } else {

    // For panels, clear whole window then draw each panel if it is at legal Z
    utilClearWindow(App->background);
    for (ix = 0; ix < mNumXpanels; ix++) {
      for (iy = 0; iy < mNumYpanels; iy++) {
        ind = ix + iy * mNumXpanels;
        bl = ind - (mNumXpanels * mNumYpanels - 1) / 2;
        iz = mSection + mPanelZstep * bl;
        if (iz < 0 || iz >= vi->zsize)
          continue;
        if (vi->pyrCache) {
          imageData = vi->pyrCache->getSectionArea(iz, mXstart, mYstart, mXdrawsize,
                                                   mYdrawsize, mZoom, asyncLoad,
                                                   xDrawsize, yDrawsize, xOffset, yOffset,
                                                   tileScale, status);
        } else {
          imageData = ivwGetZSectionTime(vi, iz, time);
        }
        bl =  mXborder + mPanelXborder + ix * 
          (mPanelXsize + mPanelGutter);
        wh =  mYborder + mPanelYborder + iy * 
          (mPanelYsize + mPanelGutter);
        b3dDrawGreyScalePixelsHQ(imageData,
                                 imXsize, imYsize,
                                 xStart, yStart, bl, wh,
                                 xDrawsize, yDrawsize,
                                 mImages[ind],
                                 vi->rampbase, 
                                 zoom, zoom,
                                 mHqgfx, iz, rgba);
      }
    }
  }

  /* DNM 9/15/03: Get the X zoom, which might be slightly different */
  // Then get the subarea limits again with more correct zoom value
  mXzoom = b3dGetCurXZoom() / tileScale;
  mTime = time;
  if (mRecordSubarea) {
    setAreaLimits();
    locatorScheduleDraw(vi);
    mRecordSubarea = 0;
  }
  if (overlay)
    free(overImage);
}

void ZapFuncs::fillOverlayRGB(unsigned char **lines, int nx, int ny, int chan,
                              unsigned char *image)
{
  b3dUInt16 **uslines = (b3dUInt16 **)lines;
  int i, j;
  
  image += chan;
  for (j = 0; j < ny; j++) {
    if (App->cvi->ushortStore) {
      for (i = 0; i < nx; i++) {
        *image = *(uslines[j] + i) / 256;
        image += 3;
      }
    } else {
      for (i = 0; i < nx; i++) {
        *image = *(lines[j] + i);
        image += 3;
      }
    }
  }
}

void ZapFuncs::drawModel()
{
  ImodView *vi = mVi;
  int ob, co, pt, maxPts = 0;
  int surf = -1;
  Icont *cont = imodContourGet(vi->imod);

  if (vi->imod->drawmode <= 0)
    return;

  drawGhost();

  if (cont)
    surf = cont->surf;

  for (ob = 0; ob < vi->imod->objsize; ob++){
    if (iobjOff(vi->imod->obj[ob].flags))
      continue;
    imodSetObjectColor(ob); 
    b3dLineWidth(sScaleSizes * vi->imod->obj[ob].linewidth2); 
    ifgSetupValueDrawing(&vi->imod->obj[ob], GEN_STORE_MINMAX1);

    // Set up for filled contour tesselator drawing
    if (vi->imod->obj[ob].extra[IOBJ_EX_2D_TRANS]) {
      setupFilledContTesselator();
      for (co = 0; co < vi->imod->obj[ob].contsize; co++)
        maxPts = B3DMAX(maxPts, vi->imod->obj[ob].cont[co].psize);
      mTessCont = imodContourNew();
      if (mTessCont) {
        mTessCont->pts = B3DMALLOC(Ipoint, maxPts);
        if (!mTessCont->pts) {
          imodContourDelete(mTessCont);
          mTessCont = NULL;
        } else {
          for (pt = 0; pt < maxPts; pt++)
            mTessCont->pts[pt].z = 0.;
        }
      }
    }

    for (co = 0; co < vi->imod->obj[ob].contsize; co++) {
      if (ob == vi->imod->cindex.object) {
        if (co == vi->imod->cindex.contour) {
          drawContour(co, ob);
          continue;
        }
        if (vi->ghostmode & IMOD_GHOST_SURFACE)
          if (surf >= 0)
            if (surf != vi->imod->obj[ob].cont[co].surf) {
              b3dColorIndex(App->ghost); 
              drawContour(co, ob);
              imodSetObjectColor(ob);
              continue;
            }
      }

      drawContour(co, ob); 
    }

    // Clean up the tesselator contour
    if (mTessCont) {
      mTessCont->psize = 1;
      imodContourDelete(mTessCont);
      mTessCont = NULL;
    }
  }
}

// A separate routine to draw the extra object(s) so that model - current 
// point - extra object drawing could happen in the right order
void ZapFuncs::drawExtraObject()
{
  ImodView *vi = mVi;
  Iobj *xobj;
  int co, ob, lassoCtrl, stippleSave;
  Icont *cont = imodContourGet(vi->imod);
 
  if (vi->imod->drawmode <= 0)
    return;
  for (ob = 0; ob < vi->numExtraObj; ob++) {
    xobj = ivwGetAnExtraObject(vi, ob);
    if (!xobj || !xobj->contsize)
      continue;
    if (iobjOff(xobj->flags) || (xobj->flags & IMOD_OBJFLAG_MODV_ONLY))
      continue;
    lassoCtrl = xobj->extra[IOBJ_EX_LASSO_ID];
    if (lassoCtrl && lassoCtrl != mCtrl)
      continue;
    if (lassoCtrl) {
      stippleSave = mVi->drawStipple;
      mVi->drawStipple = 1;
      if (mShiftingCont && !cont && !mDrawingLasso)
        xobj->linewidth2 = 2;
    }

    ifgResetValueSetup();

    // If there are contours in the extra object, set color and draw
    customGhostColor((int)(255. * xobj->red), (int)(255. * xobj->green), 
                     (int)(255. * xobj->blue));
    for (co = 0; co < xobj->contsize; co++)
      drawContour(co, -1 - ob);
    if (lassoCtrl) {
      mVi->drawStipple = stippleSave;
      xobj->linewidth2 = 1;
    }
  }
  resetGhostColor();
}

/*
 * Draw a contour, including contours in an extra object
 */
void ZapFuncs::drawContour(int co, int ob)
{
  ImodView *vi = mVi;
  float delz;
  Iobj  *obj;
  Icont *cont;
  Istore *stp;
  DrawProps contProps, ptProps;
  int pt, radius, lastX, lastY, thisX, thisY, st;
  float drawsize, zscale;
  int nextChange, stateFlags, changeFlags;
  int checkSymbol = 0;
  int handleFlags = HANDLE_LINE_COLOR | HANDLE_2DWIDTH;
  bool lastVisible, thisVisible, selected, drawAllZ, drawPntOffSec;
  bool stippleGaps, skipOutline = false;
  bool currentCont = (co == vi->imod->cindex.contour) &&
    (ob == vi->imod->cindex.object );

  if (ob >= 0)
    obj  = &(vi->imod->obj[ob]);
  else
    obj = ivwGetAnExtraObject(vi, -ob - 1);

  if (!obj)
    return;
  cont = &(obj->cont[co]);
  if ((!cont) || (!cont->psize))
    return;

  drawAllZ = (cont->flags & (ICONT_CURSOR_LIKE | ICONT_DRAW_ALLZ)) != 0;
  drawPntOffSec = !(obj->flags & IMOD_OBJFLAG_PNT_ON_SEC);
  if ((cont->flags & ICONT_MMODEL_ONLY) && vi->imod->mousemode != IMOD_MMODEL)
    return;

  if (cont->flags & ICONT_CURSOR_LIKE) {
    if (!mGfx->extraCursorInWindow() || mNumXpanels)
      return;
    mDrewExtraCursor = true;
  }

  if (ifgGetValueSetupState())
    handleFlags |= HANDLE_VALUE1;

  zscale = ((vi->imod->zscale ? vi->imod->zscale : 1.) * vi->zbin) / vi->xybin;

  /* check for contours that contain time data. */
  /* Don't draw them if the time isn't right. */
  /* DNM 6/7/01: but draw contours with time 0 regardless of time */
  if (ivwTimeMismatch(vi, mTimeLock, obj, cont))
    return;

  // get draw properties
  selected = imodSelectionListQuery(vi, ob, co) > -2;
  nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, &stateFlags,
                                   handleFlags, selected, sScaleSizes);
  if (contProps.gap)
    return;

  stippleGaps = !utilEnableStipple(vi, cont) && ifgStippleGaps();

  /* Open or closed contour */
  // Skip if not wild and not on section
  lastVisible = pointVisable(&(cont->pts[0])) || drawAllZ;
  if (!iobjScat(obj->flags) && ((cont->flags & ICONT_WILD) || lastVisible)) {

    // Draw fill first if there is a 2d trans setting
    if (obj->extra[IOBJ_EX_2D_TRANS] && mTessCont && !(cont->flags & ICONT_WILD)) {

      glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      glColor4f(contProps.red, contProps.green, contProps.blue, 
                1. - obj->extra[IOBJ_EX_2D_TRANS] / 100.);
      for (pt = 0; pt < cont->psize; pt++) {
        mTessCont->pts[pt].x = xpos(cont->pts[pt].x);
        mTessCont->pts[pt].y = ypos(cont->pts[pt].y);
      }
      mTessCont->psize = cont->psize;
      drawFilledPolygon(mTessCont);
      glDisable(GL_BLEND);
      glColor3f(contProps.red, contProps.green, contProps.blue);
      skipOutline = !selected && !(obj->flags & IMOD_OBJFLAG_POLY_CONT) && !currentCont;
    }

    if (!skipOutline && ((cont->flags & ICONT_WILD) || nextChange >= 0)) {
      if (!nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, &changeFlags, 
                                         handleFlags, selected, sScaleSizes);

      if (stippleGaps)
        glLineStipple(1, 0x0707);
      if (ptProps.gap && stippleGaps) {
        b3dStippleNextLine(true);
        ptProps.gap = 0;
      }

      // For wild contour, test every point and connect only pairs on section
      lastX = xpos(cont->pts[0].x);
      lastY = ypos(cont->pts[0].y);
      for (pt = 1; pt < cont->psize; pt++) {
        thisVisible = pointVisable(&(cont->pts[pt]));
        if (thisVisible) {
          thisX = xpos(cont->pts[pt].x);
          thisY = ypos(cont->pts[pt].y);
          if (lastVisible && !ptProps.gap)
            b3dDrawLine(lastX, lastY, thisX, thisY);
          lastX = thisX;
          lastY = thisY;
        }
        lastVisible = thisVisible;
        ptProps.gap = 0;
        if (pt == nextChange)
          nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                           &ptProps, &stateFlags,
                                           &changeFlags, handleFlags, 
                                           selected, sScaleSizes);
        if (ptProps.gap && stippleGaps) {
          b3dStippleNextLine(true);
          ptProps.gap = 0;
        }
      }

      // IF closed contour in closed object and not current, draw closure as
      // long as both points are visible
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN) && 
          !ptProps.gap && 
          !currentCont && lastVisible && pointVisable(cont->pts))
        b3dDrawLine(lastX, lastY, xpos(cont->pts->x),
                    ypos(cont->pts->y));

      if (stippleGaps)
        b3dStippleNextLine(false);
        
    } else if (!skipOutline) {

      // For non-wild contour with no changes, draw all points without testing
      b3dBeginLine();
      for (pt = 0; pt < cont->psize; pt++)
        b3dVertex2i(xpos(cont->pts[pt].x),
                    ypos(cont->pts[pt].y));

      // IF closed contour in closed object and not current, draw closure
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN) && !currentCont)
        b3dVertex2i(xpos(cont->pts->x), ypos(cont->pts->y));

      b3dEndLine();
    }
          
    checkSymbol = 1;
  }
     
  /* symbols */
  if (ilistSize(cont->store))
    nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                     &stateFlags, handleFlags, selected, 
                                     sScaleSizes);
  if ((iobjScat(obj->flags) || checkSymbol) && 
      (contProps.symtype != IOBJ_SYM_NONE || nextChange >= 0)) {
    for (pt = 0; pt < cont->psize; pt++) {
      ptProps.gap = 0;
      if (pt == nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, &changeFlags, 
                                         handleFlags, selected, sScaleSizes);

      if (ptProps.symtype != IOBJ_SYM_NONE && !(ptProps.gap && ptProps.valskip)
          && pointVisable(&(cont->pts[pt]))){
        utilDrawSymbol(xpos(cont->pts[pt].x),
                       ypos(cont->pts[pt].y),
                       ptProps.symtype,
                       ptProps.symsize * sScaleSizes,
                       ptProps.symflags);
      }
    }
  }

  /* Any contour with point sizes set */
  if (iobjScat(obj->flags) || cont->sizes || obj->pdrawsize) {
    if (ilistSize(cont->store))
      nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                       &stateFlags, handleFlags, selected, 
                                       sScaleSizes);
    for (pt = 0; pt < cont->psize; pt++){
      ptProps.gap = 0;
      if (pt == nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, &changeFlags, 
                                         handleFlags, selected, sScaleSizes);

      drawsize = imodPointGetSize(obj, cont, pt) / vi->xybin;
      if (drawsize > 0 && !(ptProps.gap && ptProps.valskip)) {
        if (pointVisable(&(cont->pts[pt]))){
          /* DNM: make the product cast to int, not drawsize */
          b3dDrawCircle(xpos(cont->pts[pt].x),
                        ypos(cont->pts[pt].y),
                        (int)(drawsize * mZoom));
          if (drawsize > 3 && drawPntOffSec)
            b3dDrawPlus(xpos(cont->pts[pt].x), 
                        ypos(cont->pts[pt].y), 3 * sScaleSizes);
        } else if (drawsize > 1 && drawPntOffSec) {
          /* DNM: fixed this at last, but let size round
             down so circles get smaller*/
          /* draw a smaller circ if further away. */
          delz = (cont->pts[pt].z - mSection) * zscale;
          if (delz < 0)
            delz = -delz;
          
          if (delz < drawsize - 0.01) {
            radius = (int)(sqrt((double)(drawsize * drawsize - delz * delz))
                           * mZoom);
            b3dDrawCircle(xpos(cont->pts[pt].x),
                          ypos(cont->pts[pt].y),
                          radius);
          }
        }
      }
    }
  }

  utilDisableStipple(vi, cont);

  // Draw end markers with assigned colors or arrowhead with object color
  if (obj->symflags & (IOBJ_SYMF_ENDS | IOBJ_SYMF_ARROW)) {
    if (cont->psize > 1 && pointVisable(&(cont->pts[cont->psize-1]))) {
      if (obj->symflags & IOBJ_SYMF_ARROW) {
        b3dDrawArrow(xpos(cont->pts[cont->psize-2].x), ypos(cont->pts[cont->psize-2].y), 
                     xpos(cont->pts[cont->psize-1].x), ypos(cont->pts[cont->psize-1].y), 
                     sScaleSizes * obj->symsize, obj->linewidth2, false);
      } else if (ob >= 0) {
        b3dColorIndex(App->endpoint);
        b3dDrawCross(xpos(cont->pts[cont->psize-1].x), ypos(cont->pts[cont->psize-1].y), 
                     sScaleSizes * obj->symsize/2);
      }
    }
    if (ob >= 0 && (obj->symflags & IOBJ_SYMF_ENDS) && pointVisable(cont->pts)) {
      b3dColorIndex(App->bgnpoint);
      b3dDrawCross(xpos(cont->pts->x), ypos(cont->pts->y),
                   sScaleSizes * obj->symsize/2);
    }
    imodSetObjectColor(ob);
  }

  // Draw connectors
  if (ifgShowConnections() && 
      istoreCountItems(cont->store, GEN_STORE_CONNECT, 0)) {
    b3dColorIndex(App->foreground);
    for (st = 0; st < ilistSize(cont->store); st++) {
      stp = istoreItem(cont->store, st);
      if (stp->type == GEN_STORE_CONNECT) {
        pt = stp->index.i;
        if (pt >= 0 && pt < cont->psize &&
            pointVisable(&cont->pts[pt]))
          b3dDrawSquare(xpos(cont->pts[pt].x),
                        ypos(cont->pts[pt].y), stp->value.i + 4);
      }
    }
    imodSetObjectColor(ob);
  }

  /* Removed drawing of size 3 circles at ends of current open contour if
     first two points visible or last point visible and next to last is not */

  if (selected)
    b3dLineWidth(sScaleSizes * obj->linewidth2); 
}

/*
 * Draw the current point marker and contour end markers
 */
void ZapFuncs::drawCurrentPoint()
{
  ImodView *vi = mVi;
  Iobj *obj = imodObjectGet(vi->imod);
  Icont *cont = imodContourGet(vi->imod);
  Ipoint *pnt = imodPointGet(vi->imod);
  int imPtSize, modPtSize, backupSize, curSize;
  int x,y, symbol = IOBJ_SYM_CIRCLE, flags = 0, openAdd = 0;

  if (!vi->drawcursor) return;

  utilCurrentPointSize(obj, &modPtSize, &backupSize, &imPtSize);

  // 11/11/04: Reset line width for slice lines or current image point,
  // set it below with object-specific thickness
  b3dLineWidth(sScaleSizes);

  if ((vi->imod->mousemode == IMOD_MMOVIE)||(!pnt)){
    x = xpos((float)((int)vi->xmouse + 0.5));
    y = ypos((float)((int)vi->ymouse + 0.5));
    b3dColorIndex(App->curpoint);
    b3dDrawPlus(x, y, imPtSize * sScaleSizes);
          
  }else{
    if ((cont) && (cont->psize) && (pnt)){

      b3dLineWidth(sScaleSizes * obj->linewidth2);
      curSize = modPtSize;
      if (cont->psize > 1 && 
          (pnt == cont->pts || pnt == cont->pts + cont->psize - 1))
        curSize = backupSize;
          
      /* DNM 6/17/01: display off-time features as if off-section */
      x = xpos(pnt->x);
      y = ypos(pnt->y);
      if (pointVisable(pnt) && 
	  !ivwTimeMismatch(vi, mTimeLock, obj, cont)) {
        b3dColorIndex(App->curpoint);
      }else{
        b3dColorIndex(App->shadow);
      }
      b3dDrawCircle(x, y, sScaleSizes * curSize);
    }
  }
     
  /* draw begin/end points for current contour */
  if (cont){
    if (ivwTimeMismatch(vi, mTimeLock, obj, cont))
      return;

    if (iobjClose(obj->flags) && (cont->flags & ICONT_OPEN)) {
      symbol = IOBJ_SYM_TRIANGLE;
      openAdd = 1;
    }
    b3dLineWidth(sScaleSizes * obj->linewidth2);
    if (cont->psize > 1){
      if (pointVisable(cont->pts)){
        b3dColorIndex(App->bgnpoint);
        utilDrawSymbol(xpos(cont->pts->x),
                       ypos(cont->pts->y), symbol, 
                       sScaleSizes * (modPtSize + openAdd), flags);
      }
      if (pointVisable(&(cont->pts[cont->psize - 1]))){
        b3dColorIndex(App->endpoint);
        utilDrawSymbol(xpos(cont->pts[cont->psize - 1].x),
                       ypos(cont->pts[cont->psize - 1].y), 
                       symbol, sScaleSizes * (modPtSize + openAdd), flags);
      }
    }
  }

  b3dLineWidth(sScaleSizes);
  mShowedSlice = mShowslice;
  if (mShowslice){
    b3dColorIndex(App->foreground);
    b3dDrawLine(xpos(vi->slice.zx1+0.5f),
                ypos(vi->slice.zy1+0.5f),
                xpos(vi->slice.zx2+0.5f), 
                ypos(vi->slice.zy2+0.5f));
    mShowslice = 0;
  }
  
  return;
}

/*
 * Draw ghost contours
 */ 
void ZapFuncs::drawGhost()
{
  int co, i, ob;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  Imod *mod = mVi->imod;
  DrawProps contProps, ptProps;
  int nextz, prevz, iz;
  bool drawprev, drawnext, stippleGaps = ifgStippleGaps();
  int pt, npt, lastX, lastY, thisX, thisY;
  int nextChange, stateFlags, changeFlags;
  int handleFlags;

  if (!mod)
    return;

  if ( !(mVi->ghostmode & IMOD_GHOST_SECTION))
    return;
     
  for (ob = 0; ob < mod->objsize; ob++) {
    if (ob != mod->cindex.object && !(mVi->ghostmode & IMOD_GHOST_ALLOBJ))
      continue;
    obj = &(mod->obj[ob]);

    /* DNM: don't do scattered points - point size works for that */
    if(iobjScat(obj->flags))
      continue;

    handleFlags = HANDLE_2DWIDTH;
    if (ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1))
      handleFlags |= HANDLE_VALUE1;

    /* DNM 6/16/01: need to be based on mSection, not zmouse */
    if (mVi->ghostdist) {
      nextz = mSection + mVi->ghostdist;
      prevz = mSection - mVi->ghostdist;
    } else {
      nextz = utilNextSecWithCont(mVi, obj, mSection, 1);
      prevz = utilNextSecWithCont(mVi, obj, mSection, -1);
    }
    drawprev = mVi->ghostmode & IMOD_GHOST_PREVSEC;
    drawnext = mVi->ghostmode & IMOD_GHOST_NEXTSEC;
     
    for (co = 0; co < obj->contsize; co++) {
      cont = &(obj->cont[co]);
      nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps,
                                       &stateFlags, handleFlags, 0,
                                       sScaleSizes);
      if (contProps.gap)
        continue;
      setGhostColor(contProps.red, contProps.green, contProps.blue);
      
      /* DNM: don't display wild contours, only coplanar ones */
      /* By popular demand, display ghosts from lower and upper sections */
      if (cont->pts && !(cont->flags & ICONT_WILD)) {
        iz = (int)floor(cont->pts->z + 0.5);
        if ((iz > mSection && drawprev && 
             ((mVi->ghostdist && iz <= nextz) || 
              (!mVi->ghostdist && iz == nextz))) ||
            (iz < mSection && drawnext && 
             ((mVi->ghostdist && iz >= prevz) ||
              (!mVi->ghostdist && iz == prevz)))) {

          if (nextChange < 0) {
            b3dBeginLine();
            for (i = 0; i < cont->psize; i++) {
              b3dVertex2i(xpos(cont->pts[i].x),
                          ypos(cont->pts[i].y));
            }
          
            /* DNM: connect back to start only if closed contour */
            if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN))
              b3dVertex2i(xpos(cont->pts->x),
                          ypos(cont->pts->y));
            b3dEndLine();
          } else {

            // If there are changes in contour, then draw only needed lines
            if (stippleGaps)
              glLineStipple(1, 0x0707);
            lastX = xpos(cont->pts[0].x);
            lastY = ypos(cont->pts[0].y);
            for (pt = 0; pt < cont->psize; pt++) {
              ptProps.gap = 0;
              if (pt == nextChange) {
                nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                                 &ptProps, &stateFlags,
                                                 &changeFlags, handleFlags, 0,
                                                 sScaleSizes);
                if (changeFlags & CHANGED_COLOR)
                  setGhostColor(ptProps.red, ptProps.green, 
                                   ptProps.blue);
                if (ptProps.gap && stippleGaps) {
                  b3dStippleNextLine(true);
                  ptProps.gap = 0;
                }
              }

              // Skip gap or last point if open
              npt = (pt + 1) % cont->psize;
              thisX = xpos(cont->pts[npt].x);
              thisY = ypos(cont->pts[npt].y);
              if ((pt < cont->psize - 1 || 
                   (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN)))
                  && !ptProps.gap)
                b3dDrawLine(lastX, lastY, thisX, thisY);
              lastX = thisX;
              lastY = thisY;
            }
            if (stippleGaps)
              b3dStippleNextLine(false);
          }
        }
      }
    }
  }
  resetGhostColor();
}

void ZapFuncs::setGhostColor(float obr, float obg, float obb)
{
  int red, green, blue, base;

  // Set base to 2 to make color get brighter instead of darker
  base = (mVi->ghostmode & IMOD_GHOST_LIGHTER) ? 2 : 0;
  red = (int)(((base + obr) * 255.0) / 3.0);
  green = (int)(((base + obg) * 255.0) / 3.0);
  blue = (int)(((base + obb) * 255.0) / 3.0);
    
  customGhostColor(red, green, blue); 
}

int ZapFuncs::drawAuto()
{
  ImodView *vi = mVi;
  unsigned int i, j;
  int x, y;
  int pixel;
  unsigned int xsize,ysize;
  int rectsize;

  xsize = vi->xsize;
  ysize = vi->ysize;

  if (!vi->ax)
    return(-1);

  if (!vi->ax->filled)
    return(-1);

  if (vi->ax->cz != mSection)
    return(-1);

  /* DNM 8/11/01: make rectangle size be nearest integer and not 0 */
  rectsize = mZoom < 1 ? 1 : (int)(mZoom + 0.5);
  for (j = 0; j < ysize; j++){
    y = ypos((float)j);
    for(i = 0; i < xsize; i++){
      x = xpos((float)i);
      /*DNM 2/1/01: pick a dark and light color to work in rgb mode */
      if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_BLACK){
        pixel = App->ghost;
        b3dColorIndex(pixel);
        b3dDrawFilledRectangle(x, y, rectsize, rectsize);
        continue;
      }
      if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_FLOOD){
        pixel = App->endpoint;
        b3dColorIndex(pixel);
        b3dDrawFilledRectangle(x, y, rectsize, rectsize);
        continue;
      }
               
      if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_WHITE){
        pixel = App->select;
        b3dColorIndex(pixel);
        b3dDrawFilledRectangle(x, y, rectsize, rectsize);
      }

    }
  }

  return(0);
}

  // send a new value of section, zoom, or time label if it has changed
void ZapFuncs::drawTools()
{
  int winx, winy;

  if (mToolMaxZ != mVi->zsize) {
    mToolMaxZ = mVi->zsize;
    mQtWindow->setMaxZ(mToolMaxZ);
  }

  // Workaround to Qt 4.5.0 cocoa bug, need to load these boxes 3 times
  if (mToolSection != mSection){
    if (mToolZoom <= -4. || mToolZoom > -0.9)
      mToolSection = mSection;
    mQtWindow->setSectionText(mSection + 1);
  }
     
  if (mToolZoom != mZoom){
    if (mToolZoom < 0.)
      mToolZoom -= 1.;
    if (mToolZoom <= -4. || mToolZoom > -0.9)
      mToolZoom = mZoom;
    mQtWindow->setZoomText(mZoom);
  }

  if (mRubberband) {
    bandImageToMouse(0);
    winx = mRbMouseX1 - 1 - mRbMouseX0;
    winy = mRbMouseY1 - 1 - mRbMouseY0;
  } else {
    winx = mWinx;
    winy = mWiny;
  }
  if (winx != mToolSizeX || winy != mToolSizeY) {
    mToolSizeX = winx;
    mToolSizeY = winy;
    mQtWindow->setSizeText(winx, winy);
  }

  if (mVi->numTimes) {
    int time = ivwWindowTime(mVi, mTimeLock);
    if (mToolTime != time){
      mToolTime = time;
      mQtWindow->setTimeLabel(time, QString(ivwGetTimeIndexLabel(mVi, time)));
    }
  }
}

// Set the cursor as appropriate for what is being drawn
void ZapFuncs::setCursor(int mode, bool setAnyway)
{
  bool needSpecial = ((mRubberband || mLassoOn) && (sMoveBandLasso || sDragBandLasso))
    || mStartingBand || mShiftingCont || mDrawingArrow;
  bool needSizeAll = mStartingBand || sMoveBandLasso || mShiftingCont || 
    (mLassoOn && sDragBandLasso) || mDrawingArrow;
  bool needModel = mDrawingLasso;
  utilSetCursor(mode, setAnyway, needSpecial, needSizeAll, sDragging, needModel, 
                mMousemode, mLastShape, mGfx);
}

int ZapFuncs::pointVisable(Ipoint *pnt)
{
  int cz;

  if (mTwod) return(1);

  /* DNM 11/30/02: replace +/- alternatives with standard nearest int code */
  cz = (int)floor(pnt->z + 0.5);
    
  if (cz == mSection)
    return(1);
    
  return(0);
}

void ZapFuncs::setDrawCurrentOnly(int value)
{
  mDrawCurrentOnly = value;
  imodInfoUpdateOnly(value);
}

