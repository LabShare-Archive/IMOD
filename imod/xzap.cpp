/*
 *  xzap.c -- The Zap Window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
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
#include "imod_display.h"
#include "b3dgfx.h"
#include "xcramp.h"
#include "xzap.h"
#include "pixelview.h"
#include "xgraph.h"
#include "locator.h"
#include "control.h"
#include "imodplug.h"
#include "imod_info.h"
#include "imod_info_cb.h"
#include "imod_input.h"
#include "imod_moviecon.h"
#include "autox.h"
#include "imod_edit.h"
#include "imod_cont_edit.h"
#include "imod_model_edit.h"
#include "imod_workprocs.h"
#include "dia_qtutils.h"
#include "preferences.h"
#include "undoredo.h"
#include "finegrain.h"
#include "scalebar.h"

static void zapDraw_cb(ImodView *vi, void *client, int drawflag);

static void zapClose_cb(ImodView *vi, void *client, int drawflag);
static void zapKey_cb(ImodView *vi, void *client, int released, QKeyEvent *e);

static int dragRegisterSize = 10;

/* DNM 1/19/01: add this to allow key to substitute for middle mouse button */
static int insertDown = 0;
static bool pixelViewOpen = false;

static QTime insertTime;
static QTime but1downt;

static int numZapWindows = 0;
static int subStartX = 0;
static int subStartY = 0;
static int subEndX = 0;
static int subEndY = 0;

static int firstdrag = 0;
static int moveband = 0;
static int dragband;
static int dragging[4];
static int firstmx, firstmy;
static int maxMultiZarea = 0;
static int scaleSizes = 1;
static bool mousePressed = false;

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
    if (maxMultiZarea < zap->mWinx * zap->mWiny) {
      maxMultiZarea = zap->mWinx * zap->mWiny;
      pos = ivwRestorableGeometry(zap->mQtWindow);
      ImodPrefs->recordMultiZparams(pos, zap->mNumXpanels, zap->mNumYpanels,
                                    zap->mPanelZstep, zap->mDrawInCenter,
                                    zap->mDrawInOthers);
    }
  }
}

/*
 * Find the first zap window, with a rubberband if flag is set
 */
ZapFuncs *getTopZapWindow(bool withBand, int type)
{
  QObjectList objList;
  ZapFuncs *zap;
  ImodControl *ctrlPtr;
  int ixl, ixr, iyb, iyt;
  int i, j, curSave, topOne;

  imodDialogManager.windowList(&objList, -1, type);
  if (!objList.count())
    return NULL;

  // Loop through the control list and find the first window that is a zap
  // with a viable rubberband if required.  
  // It is best to save and restore ctrlist current item
  topOne = -1;
  curSave = App->cvi->ctrlist->list->current;
  for (j = 0; topOne < 0 && j < ilistSize(App->cvi->ctrlist->list); j++) {
    ctrlPtr = (ImodControl *)ilistItem(App->cvi->ctrlist->list, j);
    for (i = 0; i < objList.count(); i++) {
      zap = ((ZapWindow *)objList.at(i))->mZap;
      if (ctrlPtr->id == zap->mCtrl && (!withBand || zap->mRubberband)) {
        if (zap->mRubberband) {
          ixl = (int)floor(zap->mRbImageX0 + 0.5);
          ixr = (int)floor(zap->mRbImageX1 - 0.5);
          iyb = (int)floor(zap->mRbImageY0 + 0.5);
          iyt = (int)floor(zap->mRbImageY1 - 0.5);
          if (ixr < 1 || iyt < 1 || ixl > zap->mVi->xsize - 2 || 
              iyb > zap->mVi->ysize - 2)
            continue;
        }
        topOne = i;
        break;
      }
    }
  }
  App->cvi->ctrlist->list->current = curSave;
  if (topOne && withBand)
    return NULL;
  if (topOne < 0)
    topOne = 0;
  return ((ZapWindow *)objList.at(topOne))->mZap;
}

/*
 * Report the rubberband coordinates of the first zap window with a band
 */
void zapReportRubberband()
{
  ZapFuncs *zap;
  int ixl, ixr, iyb, iyt, bin;


  zap = getTopZapWindow(true);
  if (!zap) {
    imodPrintStderr("ERROR: No Zap window has usable rubberband coordinates"
                    "\n");
    return;
  }

  bin = zap->mVi->xybin;
  ixl = (int)floor(zap->mRbImageX0 + 0.5);
  ixr = (int)floor(zap->mRbImageX1 - 0.5);
  iyb = (int)floor(zap->mRbImageY0 + 0.5);
  iyt = (int)floor(zap->mRbImageY1 - 0.5);
  
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
  int lowSection, highSection;
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
  pixelViewOpen = state;
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
  zap->getixy(mx, my, &imagePt->x, &imagePt->y, &iz);
  imagePt->z = (float)iz;
  return 0;
}

/*
 * Return the subset limits from the active window
 */
int zapSubsetLimits(ViewInfo *vi, int &ixStart, int &iyStart, int &nxUse, int &nyUse)
{
  if (numZapWindows <= 0 || subStartX >= subEndX || subStartY >= subEndY ||
      subEndX >= vi->xsize || subEndY >= vi->ysize)
    return 1;
  ixStart = subStartX;
  nxUse = subEndX + 1 - subStartX;
  iyStart = subStartY;
  nyUse = subEndY + 1 - subStartY;
  return 0;
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
    imodPrintStderr("Zap Draw\n");

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
      zap->syncImage();

    /* DNM 1/29/03: no more worry about multiple calls */
    zap->draw();

    /* DNM 3/8/01: add autosnapshot when movieing */
    // 3/8/07: make it take montages too
    if (snaptype && zap->mVi->zmovie && 
        zap->mMovieSnapCount && imcGetStarterID() == zap->mCtrl) {
      if (imcGetSnapMontage(true)) {
        zap->montageSnapshot(snaptype);
      } else {
        limits = NULL;
        if (zap->mRubberband) {
          limits = limarr;
          zap->bandImageToMouse(1);
          limarr[0] = zap->mRbMouseX0 + 1;
          limarr[1] = zap->mWiny - zap->mRbMouseY1;
          limarr[2] = zap->mRbMouseX1 - 1 - zap->mRbMouseX0;
          limarr[3] = zap->mRbMouseY1 - 1 - zap->mRbMouseY0;
        }
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
  mHqgfx  = ImodPrefs->startInHQ() ? 1 : 0;
  mHide   = 0;
  mZoom   = 1.0;
  mData   = NULL;
  mImage  = NULL;
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
                        wintype ? MULTIZ_WINDOW_TYPE : ZAP_WINDOW_TYPE);

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
        while (mZoom * vi->xsize > 1.1 * maxWinx || 
               mZoom * vi->ysize > 1.1 * maxWiny - toolHeight) {
          newZoom = b3dStepPixelZoom(mZoom, -1);
          if (fabs(newZoom - mZoom) < 0.0001)
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
  numZapWindows++;
  insertTime.start();
}


void ZapFuncs::help()
{
  if (mNumXpanels)
    imodShowHelpPage("multizap.html");
  else
    imodShowHelpPage("zap.html");
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
    numZapWindows--;

  // What for?  flush any events that might refer to this zap
  imod_info_input();     

  if (!mNumXpanels)
    b3dFreeCIImage(mImage);
  else {
    if (!maxMultiZarea) {
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

void ZapFuncs::syncImage()
{
  int syncborder, wposition, wsize, tripshift;
  int trytrans, trydraws, tryborder, trystart, borderMin;
  ImodView *vi = mVi;
  if ((!mLock) && 
      ((vi->imod->mousemode == IMOD_MMODEL && mVi->imod->cindex.point >= 0)
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
    ob = mVi->imod->cindex.object;
    imodSetObjectColor(ob); 
    b3dLineWidth(mVi->imod->obj[ob].linewidth2); 
    drawContour(mVi->imod->cindex.contour, ob);
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
  if (mZoom <= 0.001)
    mZoom = 0.001;
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
      syncImage();
      draw();
    }
    break;

  case ZAP_TOGGLE_CENTER:
    mKeepcentered = state;
    if (state) {
      flushImage();
      syncImage();
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
  Iindex indadd;
  Iindex *indp;
  Ipoint selmin, selmax;
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
    // Translate with keypad in movie mode or regular arrows in model mode
    if (!keypad && imod->mousemode != IMOD_MMOVIE ||
        keypad && imod->mousemode == IMOD_MMOVIE) {
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
      if (keysym == Qt::Key_Left)
        inputPointMove(vi, -1, 0, 0);
      if (keysym == Qt::Key_Right)
        inputPointMove(vi, 1, 0, 0);
      if (keysym == Qt::Key_Down)
        inputPointMove(vi, 0, -1, 0);
      if (keysym == Qt::Key_Up)
        inputPointMove(vi, 0, 1, 0);
      handled = 1;

    }
    break;

  case Qt::Key_PageUp:
  case Qt::Key_PageDown:
    // With keypad, translate in movie mode or move point in model mode
    if (keypad) {
      if (keysym == Qt::Key_PageDown) {
        if (imod->mousemode == IMOD_MMOVIE)
          translate(trans, -trans);
        else
          inputPointMove(vi, 0, 0, -1);
      } else {
        if (imod->mousemode == IMOD_MMOVIE)
          translate(trans, trans);
        else
          inputPointMove(vi, 0, 0, 1);
      }
      handled = 1;

      // with regular keys, handle specially if locked
    } else if (!keypad && mLock == 2){
      if (shifted) {
        obj = imodObjectGet(imod);
        mSection = utilNextSecWithCont(vi, obj, mSection, 
                                           keysym == Qt::Key_PageUp ? 1 : -1);
      } else {
        if (keysym == Qt::Key_PageDown)
          mSection--;
        else
          mSection++;
      }
      if (mSection < 0) mSection = 0;
      if (mSection >= vi->zsize) 
        mSection = vi->zsize -1;
      draw();
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
    if (!keypad || imod->mousemode == IMOD_MMOVIE || insertDown || 
        mNumXpanels)
      break;

    // It wouldn't work going to a QPoint and accessing it, so do it in shot!
    ix = (mGfx->mapFromGlobal(QCursor::pos())).x();
    iy = (mGfx->mapFromGlobal(QCursor::pos())).y();

    // Set a flag, set continuous tracking, grab keyboard and mouse
    insertDown = 1;
    mGfx->setMouseTracking(true);
    mQtWindow->grabKeyboard();
    mGfx->grabMouse();

    /* Use time since last event to determine whether to treat like
       single click or drag */
    rx = insertTime.elapsed();
    insertTime.restart();
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

      // Look through selection list, remove any that do not fit constraints
      for (i = ilistSize(vi->selectionList) - 1; i >= 0; i--) {
        indp = (Iindex *)ilistItem(vi->selectionList, i);
        if (indp->object < imod->objsize) {
          obj = &imod->obj[indp->object];
          if (indp->contour < obj->contsize) {
            if (contInSelectArea(obj, &(obj->cont[indp->contour]), selmin,
                                 selmax))
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
          if (contInSelectArea(obj, &(obj->cont[i]), selmin, selmax)) {
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
      limits = NULL;
      if (mRubberband) {
        limits = limarr;
        bandImageToMouse(1);
        limarr[0] = mRbMouseX0 + 1;
        limarr[1] = mWiny - mRbMouseY1;
        limarr[2] = mRbMouseX1 - 1 - mRbMouseX0;
        limarr[3] = mRbMouseY1 - 1 - mRbMouseY0;
      }
      b3dKeySnapshot((char *)(mNumXpanels ? "multiz" : "zap"), shifted, 
                     ctrl, limits);
    }else
      inputSaveModel(vi);
    handled = 1;
    break;
          
  case Qt::Key_R:
    if (shifted && !mNumXpanels) {
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

/*
 * Key is raised: finish up various tasks
 */
void ZapFuncs::keyRelease(QKeyEvent *event)
{
  /*  imodPrintStderr ("%d down\n", downtime.elapsed()); */
  if (!insertDown || !(event->modifiers() & Qt::KeypadModifier) ||
      (event->key() != Qt::Key_Insert && event->key() != Qt::Key_0))
    return;
  insertDown = 0;
  registerDragAdditions();
  setMouseTracking();
  mQtWindow->releaseKeyboard();
  mGfx->releaseMouse();

  // Note that unless the user truns off autorepeat on the key, there is a
  // series of key press - release events and it does full draws
  if (mDrawCurrentOnly) {
    setDrawCurrentOnly(0);
    imodDraw(mVi, IMOD_DRAW_MOD | IMOD_DRAW_XYZ);
  }
}

// Pass on various events to plugins
void ZapFuncs::generalEvent(QEvent *e)
{
  int ix, iy, ifdraw, iz, pt;
  float imx, imy;
  Iobj *obj;
  Icont *cont;
  float wheelScale = utilWheelToPointSizeScaling(mZoom);
  if (mNumXpanels || !mPopup || App->closing)
    return;
  ix = (mGfx->mapFromGlobal(QCursor::pos())).x();
  iy = (mGfx->mapFromGlobal(QCursor::pos())).y();
  getixy(ix, iy, &imx, &imy, &iz);
  ifdraw = imodPlugHandleEvent(mVi, e, imx, imy);
  if (ifdraw & 2 || (mDrewExtraCursor && e->type() == QEvent::Leave))
    draw();
  if (ifdraw)
    return;
  if (e->type() == QEvent::Wheel && iceGetWheelForSize()) {
    imodGetIndex(mVi->imod, &ix, &iy, &pt);
    obj = imodObjectGet(mVi->imod);
    cont = imodContourGet(mVi->imod);
    if (!cont || pt < 0)
      return;
    imx = imodPointGetSize(obj, cont, pt);
    if (!imx && (!cont->sizes || (cont->sizes && cont->sizes[pt] < 0)))
      return;
    imx += ((QWheelEvent *)e)->delta() * wheelScale;
    imx = B3DMAX(0., imx);
    mVi->undo->contourDataChg();
    imodPointSetSize(cont, pt, imx);
    mVi->undo->finishUnit();
    imodDraw(mVi, IMOD_DRAW_MOD);
  }
}

/*
 * respond to a mouse press event
 */
void ZapFuncs::mousePress(QMouseEvent *event)
{
  int button1, button2, button3, ifdraw = 0, drew = 0;
  int ctrlDown = event->modifiers() & Qt::ControlModifier;
  int dxll, dxur,dyll, dyur, x, y;
  int rcrit = 10;   /* Criterion for moving the whole band */

  button1 = event->buttons() & ImodPrefs->actualButton(1) ? 1 : 0;
  button2 = event->buttons() & ImodPrefs->actualButton(2) ? 1 : 0;
  button3 = event->buttons() & ImodPrefs->actualButton(3) ? 1 : 0;
  x = event->x();
  y = event->y();
  but1downt.start();
  firstmx = x;
  firstmy = y;
  mLmx = x;
  mLmy = y;
  mousePressed = true;
  utilRaiseIfNeeded(mQtWindow, event);

  if (imodDebug('m'))
    imodPrintStderr("click at %d %d   buttons %d %d %d\n", x, y, button1, 
                    button2, button3);

  // Check for starting a band move before offering to plugin
  if (event->button() == ImodPrefs->actualButton(2) && !button1 && !button3) {
    moveband = 0;
    
    /* If rubber band is on and within criterion distance of any edge, set
       flag to move whole band and return */
    if (mRubberband) {
      bandImageToMouse(0);
      dxll = x - mRbMouseX0;
      dxur = x - mRbMouseX1;
      dyll = y - mRbMouseY0;
      dyur = y - mRbMouseY1;
      if ((dyll > 0 && dyur < 0 && (dxll < rcrit && dxll > -rcrit ||
                                    dxur < rcrit && dxur > -rcrit)) ||
          (dxll > 0 && dxur < 0 && (dyll < rcrit && dyll > -rcrit ||
                                    dyur < rcrit && dyur > -rcrit))) {
        moveband = 1;
        setCursor(mMousemode);
        return;
      }
    }   
  }  
  setCursor(mMousemode, utilNeedToSetCursor());

  // Now give the plugins a crack at it
  ifdraw = checkPlugUseMouse(event, button1, button2, button3);
  if (ifdraw & 1) {
    if (ifdraw & 2)
      draw();
    return;
  }

  // Check for regular actions
  if (event->button() == ImodPrefs->actualButton(1)) {
    if (mShiftingCont)
      drew = startShiftingContour(firstmx, firstmy, 1, ctrlDown);
    else if (mStartingBand)
      drew = b1Click(firstmx, firstmy, ctrlDown);
    else
      firstdrag = 1;
      
  } else if (event->button() == ImodPrefs->actualButton(2) &&
	     !button1 && !button3) {
    if (mShiftingCont)
      drew = startShiftingContour(x, y, 2, ctrlDown);
    else
      drew = b2Click(x, y, ctrlDown);

  } else if (event->button() == ImodPrefs->actualButton(3) &&
	     !button1 && !button2) {
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
  int imz, button1, button2, button3, ifdraw = 0, drew = 0;
  bool needDraw;
  float imx, imy;
  button1 = event->button() == ImodPrefs->actualButton(1) ? 1 : 0;
  button2 = event->button() == ImodPrefs->actualButton(2) ? 1 : 0;
  button3 = event->button() == ImodPrefs->actualButton(3) ? 1 : 0;
  mousePressed = false;

  if (imodDebug('m'))
    imodPrintStderr("release at %d %d   buttons %d %d %d\n", event->x(), 
                    event->y(), button1, button2, button3);
  if (mShiftRegistered) {
    mShiftRegistered = 0;
    mVi->undo->finishUnit();
  }
  needDraw = mDrewExtraCursor && !mGfx->extraCursorInWindow();

  ifdraw = checkPlugUseMouse(event, button1, button2, button3);
  if (ifdraw & 1) {
    if ((ifdraw & 2) || needDraw)
      draw();

    // Defer the return so the band moving can be turned off, but then only 
    // check other things below if this flag is off
  }

  if (button1 && !(ifdraw & 1)) {
    if (dragband) {
      dragband = 0;
      setCursor(mMousemode);
    }
    firstdrag = 0;

    if (but1downt.elapsed() > 250) {
      if (mHqgfxsave || ifdraw)
        draw();
      mHqgfxsave  = 0;
      return;    //IS THIS RIGHT?
    }
    drew = b1Click(event->x(), event->y(),
               event->modifiers() & Qt::ControlModifier);
  }
 
  // Button 2 and band moving, release the band
  if (button2 && mRubberband && moveband) {
    moveband = 0;
    setCursor(mMousemode);
    if (mHqgfxsave) {
      draw();
      drew = 1;
    }
    mHqgfxsave  = 0;

    // Button 2 and doing a drag draw - draw for real.
  } else if (button2 && !mNumXpanels && !(ifdraw & 1) &&
             mVi->imod->mousemode == IMOD_MMODEL) {
    if (imodDebug('z'))
      imodPrintStderr("Down time %d msec\n", but1downt.elapsed());

    registerDragAdditions();

    // Fix the mouse position and update the other windows finally
    // Why call imod_info_setxyz again on release of single point add?
    getixy(event->x(), event->y(), &imx, &imy, &imz);
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

  if (pixelViewOpen) {
    getixy(ex, ey, &imx, &imy, &imz);
    pvNewMousePosition(mVi, imx, imy, imz);
  }

  if (!(mRubberband && moveband)  && 
      (mousePressed || insertDown || mVi->trackMouseForPlugs)) {
    ifdraw = checkPlugUseMouse(event, button1, button2, button3);
    if (ifdraw & 1) {
      if (ifdraw & 2)
        draw();
      return;
    }
  } else
    setControlAndLimits();

  if (!(mousePressed || insertDown)) {
    if (mRubberband)
      analyzeBandEdge(ex, ey);
    if (ifdraw)
      draw();
    return;
  }

  // For first button or band moving, eat any pending move events and use 
  // latest position
  if ( (button1 && !button2 && !button3) || (mRubberband && moveband)) {
    processing = 1;
    imod_info_input();
    if (imodDebug('m') && processing > 1)
      imodPrintStderr("Flushed %d move events\n", processing - 1);
    processing = 0;

    // If this processed a release, then turn off the buttons
    if (!mousePressed)
      button1 = button2 = button3 = 0;
  }

  cumdx = ex - firstmx;
  cumdy = ey - firstmy;
  button2 = (button2 || insertDown) ? 1 : 0;
  if (imodDebug('m'))
    imodPrintStderr("move %d,%d  mb  %d|%d|%d  c %x s %x\n", ex, ey, button1,
                    button2, button3, ctrlDown, shiftDown);

  if ( (button1) && (!button2) && (!button3)){
    if (ctrlDown) {
      drew = dragSelectContsCrossed(ex, ey);
    } else {
      /* DNM: wait for a bit of time or until enough distance moved, but if we
         do not replace original lmx, lmy, there is a disconcerting lurch */
      if ((but1downt.elapsed()) > 250 || cumdx * cumdx + cumdy * cumdy >
          cumthresh)
        drew = b1Drag(ex, ey);
    }
  }

  // DNM 8/1/08: Reject small movements soon after the button press
  if ( (!button1) && (button2) && (!button3)) {
    if ((but1downt.elapsed()) > 150 || cumdx * cumdx + cumdy * cumdy > 
        dragthresh)
      drew = b2Drag(ex, ey, ctrlDown);
  }
  
  if ( (!button1) && (!button2) && (button3))
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
  setControlAndLimits();
  if (mNumXpanels)
    return 0;
  ivwControlActive(mVi, 0);
  getixy(event->x(), event->y(), &imx, &imy, &imz);
  ifdraw = imodPlugHandleMouse(mVi, event, imx, imy, but1, but2, but3);
  if (ifdraw & 1) {
    if (ifdraw & 2) 
      draw();
  } else
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
  int i, dminsq, dist, distsq, dmin, dxll, dyll, dxur, dyur;
  int minedgex, minedgey;

  bandImageToMouse(0);    
  dminsq = rubbercrit * rubbercrit;
  dragband = 0;
  minedgex = -1;
  for (i = 0; i < 4; i++)
    dragging[i] = 0;
  dxll = ix - mRbMouseX0;
  dxur = ix - mRbMouseX1;
  dyll = iy - mRbMouseY0;
  dyur = iy - mRbMouseY1;

  /* Find distance from each corner, keep track of a min */
  distsq = dxll * dxll + dyll * dyll;
  if (distsq < dminsq) {
    dminsq = distsq;
    minedgex = 0;
    minedgey = 2;
  }
  distsq = dxur * dxur + dyll * dyll;
  if (distsq < dminsq) {
    dminsq = distsq;
    minedgex = 1;
    minedgey = 2;
  }
  distsq = dxll * dxll + dyur * dyur;
  if (distsq < dminsq) {
    dminsq = distsq;
    minedgex = 0;
    minedgey = 3;
  }
  distsq = dxur * dxur + dyur * dyur;
  if (distsq < dminsq) {
    dminsq = distsq;
    minedgex = 1;
    minedgey = 3;
  }

  /* If we are close to a corner, set up to drag the band */
  if (minedgex >= 0) {
    dragband = 1;
    dragging[minedgex] = 1;
    dragging[minedgey] = 1;
  } else {
    /* Otherwise look at each edge in turn */
    dmin = rubbercrit;
    dist = dxll > 0 ? dxll : -dxll;
    if (dyll > 0 && dyur < 0 && dist < dmin){
      dmin = dist;
      minedgex = 0;
    }
    dist = dxur > 0 ? dxur : -dxur;
    if (dyll > 0 && dyur < 0 && dist < dmin){
      dmin = dist;
      minedgex = 1;
    }
    dist = dyll > 0 ? dyll : -dyll;
    if (dxll > 0 && dxur < 0 && dist < dmin){
      dmin = dist;
      minedgex = 2;
    }
    dist = dyur > 0 ? dyur : -dyur;
    if (dxll > 0 && dxur < 0 && dist < dmin){
      dmin = dist;
      minedgex = 3;
    }
    if (minedgex < 0)
      dragband = 0;
    else {
      dragging[minedgex] = 1;
      dragband = 1;
    }
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
  int bandmin = bandMinimum();
  int iz;
  float distance;
  float ix, iy, dx, dy;
  float selsize = IMOD_SELSIZE / mZoom;

  getixy(x, y, &ix, &iy, &iz);
  if (iz < 0)
    return 0;
     
  // If starting rubber band, set upper left corner, set for moving lower left
  if (mStartingBand) {
    
    // Set up mouse coords then convert to image
    mRbMouseX1 = x + bandmin;
    if (mRbMouseX1 > mWinx - 1)
      mRbMouseX1 = mWinx - 1;
    mRbMouseX0 = mRbMouseX1 - bandmin;

    mRbMouseY0 = y - bandmin;
    if (y < bandmin)
      mRbMouseY0 = 0;
    mRbMouseY1 = mRbMouseY0 + bandmin;
    bandMouseToImage(0);

    // Does image coord need to be moved?  Do so and move mouse
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
      QCursor::setPos(mGfx->mapToGlobal(QPoint(mRbMouseX0, 
                                                   mRbMouseY0)));
      firstmx = mRbMouseX0;
      firstmy = mRbMouseY0;
    }

    mStartingBand = 0;
    mRubberband = 1;
    mGfx->setMouseTracking(true);
    dragband = 1;
    dragging[0] = 0;
    dragging[1] = 1;
    dragging[2] = 0;
    dragging[3] = 1;
    mBandChanged = 1;
    setCursor(mMousemode);
    draw();
    return 1;  
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

    distance = imodAllObjNearest(vi, &index , &pnt, selsize);

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
  int cz, pz, iz;

  getixy(x, y, &ix, &iy, &iz);

  if (vi->ax && !mNumXpanels){
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      /* DNM 2/1/01: need to call with int */
      autox_sethigh(vi, (int)ix, (int)iy);
      return 1;
    }
  }
     
  // 3/5/08: Moved band moving to mouse press routine so it would happen before
  // plugin action

  if (vi->imod->mousemode == IMOD_MMODEL) {
    if (mNumXpanels)
      return 0;
    mDragAddCount = 0;
    obj = imodObjectGet(vi->imod);
    if (!obj)
      return 0;

    // Get current contour; if there is none, start a new one
    // DNM 7/10/04: switch to calling routine; it now fixes time of empty cont
    cont = ivwGetOrMakeContour(vi, obj, mTimeLock);
    if (!cont)
      return 0;

    point.x = ix;
    point.y = iy;
    point.z = mSection;
    if ((mTwod)&&(cont)&&(cont->psize)){
      point.z = cont->pts->z;
    }
    vi->xmouse = ix;
    vi->ymouse = iy;

    /* If contours are closed and Z has changed, start a new contour */
    /* Also check for a change in time, if time data are being modeled  */
    /* and start new contour for any kind of contour */
    // DNM 7/10/04: just use first point instead of current point which is
    // not always defined
    if (cont->psize > 0) {
      cz = (int)floor(cont->pts->z + 0.5); 
      pz = (int)point.z;
      if ((iobjPlanar(obj->flags) && !(cont->flags & ICONT_WILD) && cz != pz) 
          || ivwTimeMismatch(vi, mTimeLock, obj, cont)) {
        if (cont->psize == 1) {
          wprint("Started a new contour even though last "
                 "contour had only 1 pt.  ");
          if (cz != pz && iobjClose(obj->flags))
            wprint("\aUse open contours to model across sections.\n");
          else if (cz != pz)
            wprint("\aTurn off \"Start new contour at new Z\" to model "
                   "across sections.\n");
          else
            wprint("\aSet contour time to 0 to model across times.\n");
        }
        
        vi->undo->contourAddition(obj->contsize);
        imodNewContour(vi->imod);
        cont = imodContourGet(vi->imod);
        if (!cont) {
          vi->undo->flushUnit();
          return 0;
        }
        ivwSetNewContourTime(vi, obj, cont);
      }
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

  getixy(x, y, &ix, &iy, &iz);
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

  getixy(x, y, &ix, &iy, &iz);

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

  if (mShiftingCont) {
    shiftContour(x, y, 1, 0);
    return 1;
  }

  // First time mouse moves, lock in the band drag position
  if (mRubberband && firstdrag)
    analyzeBandEdge(x, y);
  firstdrag = 0;
     
  if (mRubberband && dragband) {

    /* Move the rubber band */
    if (dragging[0]) {
      mRbImageX0 += (x - mLmx) / mXzoom;
      if (mRbImageX0 < 0)
        mRbImageX0 = 0;
      if (mRbImageX0 >= mRbImageX1)
        mRbImageX0 = mRbImageX1 - 1;
    }
    if (dragging[1]) {
      mRbImageX1 += (x - mLmx) / mXzoom;
      if (mRbImageX1 > mVi->xsize)
        mRbImageX1 = mVi->xsize;
      if (mRbImageX1 <= mRbImageX0)
        mRbImageX1 = mRbImageX0 + 1;
    }
    if (dragging[3]) {
      mRbImageY0 += (mLmy - y) / mXzoom;
      if (mRbImageY0 < 0)
        mRbImageY0 = 0;
      if (mRbImageY0 >= mRbImageY1)
        mRbImageY0 = mRbImageY1 - 1;
    }
    if (dragging[2]) {
      mRbImageY1 += (mLmy - y) / mXzoom;
      if (mRbImageY1 > mVi->ysize)
        mRbImageY1 = mVi->ysize;
      if (mRbImageY1 <= mRbImageY0)
        mRbImageY1 = mRbImageY0 + 1;
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

  mHqgfxsave = mHqgfx;
  mHqgfx = 0;
  draw();
  mHqgfx = mHqgfxsave;
  return 1;
}

/* Tests whether the contour is in the selection area.  Either it must be 
   entirely within the area, or it must be an open wild contour and have points
   on the current section that are all within the area */
int ZapFuncs::contInSelectArea(Iobj *obj, Icont *cont, Ipoint selmin, Ipoint selmax)
{
  Ipoint pmin, pmax;
  int pt, inRange = 0;
  Ipoint *pnt;

  imodContourGetBBox(cont, &pmin, &pmax);
  if (pmin.x >= selmin.x && pmax.x <= selmax.x &&
      pmin.y >= selmin.y && pmax.y <= selmax.y &&
      pmin.z >= selmin.z && pmax.z <= selmax.z)
    return 1;
  if (!(iobjOpen(obj->flags) && (cont->flags & ICONT_WILD)))
    return 0;

  // Wild open contour is no good if a point in the Z range is outside the
  // X/Y range
  for (pt = 0; pt < cont->psize; pt++) {
    pnt = &cont->pts[pt];
    if (pnt->z >= selmin.z && pnt->z <= selmax.z) {
      inRange = 1;
      if (pnt->x < selmin.x || pnt->x > selmax.x ||
          pnt->y < selmin.y || pnt->y > selmax.y)
        return 0;
    }
  }
  return inRange;
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
  getixy(x, y, &pnt2.x, &pnt2.y, &iz2);
  getixy(mLmx, mLmy, &pnt1.x, &pnt1.y, &iz1);
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
     
  if (mNumXpanels)
    return 0;

  if (mShiftingCont) {
    shiftContour(x, y, 2, 0);
    return 1;
  }

  if (vi->ax){
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      getixy(x, y, &ix, &iy, &iz);
      /* DNM 2/1/01: need to call with int */
      autox_sethigh(vi, (int)ix, (int)iy);
      return 1;
    }
  }

  if (mRubberband && moveband) {
    /* Moving rubber band: get desired move and constrain it to keep
       band in the image */
    idx = (x - mLmx) / mXzoom;
    idy = (mLmy - y) / mZoom;
    shiftRubberband(idx, idy);

    mHqgfxsave = mHqgfx;
    mHqgfx = 0;
    draw();
    mHqgfx = mHqgfxsave;
    mBandChanged = 1;
    return 1;
  }

  if (vi->imod->mousemode == IMOD_MMOVIE)
    return 0;

  if (vi->imod->cindex.point < 0)
    return 0;

  getixy(x, y, &ix, &iy, &iz);

  cpt.x = ix;
  cpt.y = iy;
  cpt.z = mSection;
     
  obj = imodObjectGet(vi->imod);
  if (!obj)
    return 0;

  cont = imodContourGet(vi->imod);
  if (!cont)
    return 0;

  lpt = &(cont->pts[vi->imod->cindex.point]);
  if (mTwod)
    cpt.z = lpt->z;

  dist = imodel_point_dist( lpt, &cpt);

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

  if ( dist > scaleModelRes(vi->imod->res, mZoom)){

    // Set insertion index to next point, or to current if drawing backwards
    pt = vi->imod->cindex.point + 1;
    if (pt > 0 && mInsertmode)
      pt--;

    // Set flag for drawing current contour only if at end and going forward
    if (!mInsertmode && pt == cont->psize)
      setDrawCurrentOnly(1);

    // Register previous additions if the count is up or if the object or
    // contour has changed
    if (mDragAddCount >= dragRegisterSize || 
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
      getixy(x, y, &ix, &iy, &iz);
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
  getixy(x, y, &(pt.x), &(pt.y), &iz);
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
  if (mCenterMarked)
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
  if (vi->imod->mousemode != IMOD_MMODEL || !obj || !cont || !cont->psize)
    err = 1;
  else if (iobjScat(obj->flags) || 
           (!iobjClose(obj->flags) && (cont->flags & ICONT_WILD)))
    err = -1;

  // If no current point, just use first
  if (pt < 0)
    pt = 0;

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
}

// Keep track of the base position for contour shifting
static Ipoint contShiftBase;

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

  getixy(x, y, &ix, &iy, &iz);

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
    contShiftBase.x = cont->pts[pt].x - ix;
    contShiftBase.y = cont->pts[pt].y - iy;
  } else {

    // Use defined center if one was set
    if (mCenterDefined) {
      contShiftBase.x = mXformCenter.x;
      contShiftBase.y = mXformCenter.y;
    } else {

      // Otherwise get center for transforms as center of mass
      if (defaultXformCenter(contShiftBase.x, contShiftBase.y)) {
        endContourShift();
        return 0;
      }
    }
    markXformCenter(contShiftBase.x, contShiftBase.y);
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
void ZapFuncs::shiftContour(int x, int y, int button, 
                         int shiftDown)
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
    getixy(x, y, &ix, &iy, &iz);
    ix += contShiftBase.x - cont->pts[pt].x;
    iy += contShiftBase.y - cont->pts[pt].y;
  } else {

    // Get transformation matrix if 2nd or 3rd button
    err = button + (button == 3 && shiftDown ? 1 : 0);
    if (mouseXformMatrix(x, y, err, mat))
      return;
  }

  imodGetIndex(imod, &curob, &curco, &pt);

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
        if (button == 1) {
          
          // Shift points
          for (pt = 0; pt < cont->psize; pt++) {
            cont->pts[pt].x += ix;
            cont->pts[pt].y += iy;
          }
        } else {
          
          // Transform points
          for (pt = 0; pt < cont->psize; pt++) {
            ix = mat[0][0] * (cont->pts[pt].x - contShiftBase.x) + mat[0][1] *
              (cont->pts[pt].y - contShiftBase.y) + contShiftBase.x;
            iy = mat[1][0] * (cont->pts[pt].x - contShiftBase.x) + mat[1][1] *
              (cont->pts[pt].y - contShiftBase.y) + contShiftBase.y;
            cont->pts[pt].x = ix;
            cont->pts[pt].y = iy;
          }
        }
      }
    }
  }

  mShiftRegistered = 1;
  imodDraw(mVi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD );
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

  xcen = xpos(contShiftBase.x);
  ycen = ypos(contShiftBase.y);

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
  return( (int)(((x - mXstart) * mXzoom) 
                + mXborder));
}

/* return y pos in window for given image y cord. */
int ZapFuncs::ypos(float y)
{
  return((int)(((y - mYstart) * mZoom)
               + mYborder));
}

/* returns image coords in x,y, z, given mouse coords mx, my */
void ZapFuncs::getixy(int mx, int my, float *x, float *y, 
                      int *z)
{
  int indx, indy, indp, indmid;

  // 10/31/04: winy - 1 maps to 0, not winy, so need a -1 here
  my = mWiny - 1 - my;

  if (!mNumXpanels) {
    *z = mSection;
  } else {
    panelIndexAndCoord(mPanelXsize, mNumXpanels, mPanelGutter, 
                       mPanelXborder, mx, indx);
    panelIndexAndCoord(mPanelYsize, mNumYpanels, mPanelGutter, 
                       mPanelYborder, my, indy);
    if (indx < 0 || indy < 0) {
      *z = -1;
    } else {
      indp = indx + indy * mNumXpanels;
      indmid = (mNumXpanels * mNumYpanels - 1) / 2;
      *z = mSection + (indp - indmid) * mPanelZstep;
      if (*z < 0 || *z >= mVi->zsize)
        *z = -1;
    }
  }
  *x = ((float)(mx - mXborder) / mXzoom) + (float)mXstart;
  *y = ((float)(my - mYborder) / mZoom) + (float)mYstart;
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
  getixy(mRbMouseX0 + 1, mRbMouseY1 - 1, &mRbImageX0, 
            &mRbImageY0, &iz);
  getixy(mRbMouseX1, mRbMouseY0, &mRbImageX1, 
            &mRbImageY1, &iz);
  
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
  int ixcen, iycen, ixofs, iyofs, imx, imy;
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
    getixy(0, -1, &xl, &yt, &iz);
    getixy(mWinx, mWiny-1, &xr, &yb, &iz);
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
  ixl *= bin;
  iyb *= bin;
  ixr = ixr * bin + bin - 1;
  iyt = iyt * bin + bin - 1;
  int lowSection, highSection;
  getLowHighSection(lowSection, highSection);
  if (lowSection < 1 || highSection < 1 || highSection > mVi->zsize) {
    wprint("ERROR: %s is out of range.\n", flipped ? "-y" : "-z");
    if (!toInfoWindow)
      return "";
  }
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
    getixy(0, 0, &xl, &yt, &iz);
    getixy(mWinx, mWiny, &xr, &yb, &iz);
  }
  subStartX = B3DMAX((int)(xl + 0.5), 0);
  subEndX = B3DMIN((int)(xr - 0.5), mVi->xsize - 1);
  subStartY = B3DMAX((int)(yb + 0.5), 0);
  subEndY = B3DMIN((int)(yt - 0.5), mVi->ysize - 1);
  if (imodDebug('z'))
    imodPrintStderr("Set area %d %d %d %d\n", subStartX, subEndX, subStartY,
                    subEndY);
}     

/*
 *
 */
B3dCIImage *ZapFuncs::zoomedDownImage(int subset, int &nxim, int &nyim, int &ixStart,
                                      int &iyStart, int &nxUse, int &nyUse)
{
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
    setMouseTracking();
  } else {
    mStartingBand = 1;
    mShiftingCont = 0;
    /* Eliminated old code for making initial band */
  }

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
 * Set mouse tracking based on state of all governing flags
 */
void ZapFuncs::setMouseTracking()
{
  mGfx->setMouseTracking(insertDown != 0 || mRubberband ||
                             pixelViewOpen || mVi->trackMouseForPlugs);
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
    scaleSizes = imcGetSizeScaling();
    if (scaleSizes == 1)
      scaleSizes = B3DNINT(scalingFactor * B3DMIN(1., mZoom));
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
  scaleSizes = 1;
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

  /* Get the time to display and flush if time is different. */
  if (mTimeLock)
    time = mTimeLock;
  else
    ivwGetTime(vi, &time);
  if (time != mTime)
    flushImage();

  if (!mNumXpanels) {
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
                  mXborder + (int)(mXdrawsize * mZoom),
                  mYborder + (int)(mYdrawsize * mZoom));
    
    // If overlay section is set and legal, get an image buffer and fill it
    // with the color overlay
    if (vi->overlaySec && App->rgba && !vi->rawImageStore && otherSec >= 0 &&
        otherSec < vi->zsize) {
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
                             vi->xsize, vi->ysize,
                             mXstart, mYstart,
                             mXborder, mYborder,
                             mXdrawsize, mYdrawsize,
                             mImage,
                             vi->rampbase, 
                             (double)mZoom, (double)mZoom,
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
        imageData = ivwGetZSectionTime(vi, iz, time);
        bl =  mXborder + mPanelXborder + ix * 
          (mPanelXsize + mPanelGutter);
        wh =  mYborder + mPanelYborder + iy * 
          (mPanelYsize + mPanelGutter);
        b3dDrawGreyScalePixelsHQ(imageData,
                                 vi->xsize, vi->ysize,
                                 mXstart, mYstart, bl, wh,
                                 mXdrawsize, mYdrawsize,
                                 mImages[ind],
                                 vi->rampbase, 
                                 mZoom, mZoom,
                                 mHqgfx, iz, rgba);
      }
    }
  }

  /* DNM 9/15/03: Get the X zoom, which might be slightly different */
  // Then get the subarea limits again with more correct zoom value
  mXzoom = b3dGetCurXZoom();
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
  unsigned int *cindex = App->cvi->cramp->ramp;
  unsigned char ramp[256];
  int i, j;
  
  // Extract the second byte from the integers of the color table
  for (i = 0; i < 256; i++)
    ramp[i] = (unsigned char)(cindex[i] >> 8);

  image += chan;
  for (j = 0; j < ny; j++) {
    for (i = 0; i < nx; i++) {
      *image = ramp[*(lines[j] + i)];
      image += 3;
    }
  }
}

void ZapFuncs::drawModel()
{
  ImodView *vi = mVi;
  int ob, co;
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
    b3dLineWidth(scaleSizes * vi->imod->obj[ob].linewidth2); 
    ifgSetupValueDrawing(&vi->imod->obj[ob], GEN_STORE_MINMAX1);

    for (co = 0; co < vi->imod->obj[ob].contsize; co++){
      if (ob == vi->imod->cindex.object){
        if (co == vi->imod->cindex.contour){
          drawContour(co, ob);
          continue;
        }
        if (vi->ghostmode & IMOD_GHOST_SURFACE)
          if (surf >= 0)
            if (surf != vi->imod->obj[ob].cont[co].surf){
              b3dColorIndex(App->ghost); 
              drawContour(co, ob);
              imodSetObjectColor(ob);
              continue;
            }
      }

      drawContour(co, ob); 
    }
  }
}

// A separate routine to draw the extra object(s) so that model - current 
// point - extra object drawing could happen in the right order
void ZapFuncs::drawExtraObject()
{
  ImodView *vi = mVi;
  Iobj *xobj;
  int co, ob;

  if (vi->imod->drawmode <= 0)
    return;
  for (ob = 0; ob < vi->numExtraObj; ob++) {
    xobj = ivwGetAnExtraObject(vi, ob);
    if (!xobj || !xobj->contsize)
      continue;
    if (iobjOff(xobj->flags) || (xobj->flags & IMOD_OBJFLAG_MODV_ONLY))
      continue;

    ifgResetValueSetup();

    // If there are contours in the extra object, set color and draw
    customGhostColor((int)(255. * xobj->red), (int)(255. * xobj->green), 
                     (int)(255. * xobj->blue));
    for (co = 0; co < xobj->contsize; co++)
      drawContour(co, -1 - ob);
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
  bool stippleGaps;
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
                                   handleFlags, selected, scaleSizes);
  if (contProps.gap)
    return;

  stippleGaps = !utilEnableStipple(vi, cont) && ifgStippleGaps();

  /* Open or closed contour */
  // Skip if not wild and not on section
  lastVisible = pointVisable(&(cont->pts[0])) || drawAllZ;
  if (!iobjScat(obj->flags) && ((cont->flags & ICONT_WILD) || lastVisible)) {
    if ((cont->flags & ICONT_WILD) || nextChange >= 0) {
      if (!nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, &changeFlags, 
                                         handleFlags, selected, scaleSizes);

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
                                           selected, scaleSizes);
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
        
    } else {

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
                                     scaleSizes);
  if ((iobjScat(obj->flags) || checkSymbol) && 
      (contProps.symtype != IOBJ_SYM_NONE || nextChange >= 0)) {
    for (pt = 0; pt < cont->psize; pt++) {
      ptProps.gap = 0;
      if (pt == nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, &changeFlags, 
                                         handleFlags, selected, scaleSizes);

      if (ptProps.symtype != IOBJ_SYM_NONE && !(ptProps.gap && ptProps.valskip)
          && pointVisable(&(cont->pts[pt]))){
        utilDrawSymbol(xpos(cont->pts[pt].x),
                       ypos(cont->pts[pt].y),
                       ptProps.symtype,
                       ptProps.symsize * scaleSizes,
                       ptProps.symflags);
      }
    }
  }

  /* Any contour with point sizes set */
  if (iobjScat(obj->flags) || cont->sizes || obj->pdrawsize) {
    if (ilistSize(cont->store))
      nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                       &stateFlags, handleFlags, selected, 
                                       scaleSizes);
    for (pt = 0; pt < cont->psize; pt++){
      ptProps.gap = 0;
      if (pt == nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, &changeFlags, 
                                         handleFlags, selected, scaleSizes);

      drawsize = imodPointGetSize(obj, cont, pt) / vi->xybin;
      if (drawsize > 0 && !(ptProps.gap && ptProps.valskip))
        if (pointVisable(&(cont->pts[pt]))){
          /* DNM: make the product cast to int, not drawsize */
          b3dDrawCircle(xpos(cont->pts[pt].x),
                        ypos(cont->pts[pt].y),
                        (int)(drawsize * mZoom));
          if (drawsize > 3 && drawPntOffSec)
            b3dDrawPlus(xpos(cont->pts[pt].x), 
                        ypos(cont->pts[pt].y), 3 * scaleSizes);
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

  utilDisableStipple(vi, cont);

  // Draw end markers
  if ((obj->symflags & IOBJ_SYMF_ENDS) && ob >= 0){
    if (pointVisable(&(cont->pts[cont->psize-1]))){
      b3dColorIndex(App->endpoint);
      b3dDrawCross(xpos(cont->pts[cont->psize-1].x),
                   ypos(cont->pts[cont->psize-1].y), 
                   scaleSizes * obj->symsize/2);
    }
    if (pointVisable(cont->pts)){
      b3dColorIndex(App->bgnpoint);
      b3dDrawCross(xpos(cont->pts->x),
                   ypos(cont->pts->y),
                   scaleSizes * obj->symsize/2);
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
    b3dLineWidth(scaleSizes * obj->linewidth2); 
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
  b3dLineWidth(scaleSizes);

  if ((vi->imod->mousemode == IMOD_MMOVIE)||(!pnt)){
    x = xpos((float)((int)vi->xmouse + 0.5));
    y = ypos((float)((int)vi->ymouse + 0.5));
    b3dColorIndex(App->curpoint);
    b3dDrawPlus(x, y, imPtSize * scaleSizes);
          
  }else{
    if ((cont) && (cont->psize) && (pnt)){

      b3dLineWidth(scaleSizes * obj->linewidth2);
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
      b3dDrawCircle(x, y, scaleSizes * curSize);
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
    b3dLineWidth(scaleSizes * obj->linewidth2);
    if (cont->psize > 1){
      if (pointVisable(cont->pts)){
        b3dColorIndex(App->bgnpoint);
        utilDrawSymbol(xpos(cont->pts->x),
                       ypos(cont->pts->y), symbol, 
                       scaleSizes * (modPtSize + openAdd), flags);
      }
      if (pointVisable(&(cont->pts[cont->psize - 1]))){
        b3dColorIndex(App->endpoint);
        utilDrawSymbol(xpos(cont->pts[cont->psize - 1].x),
                       ypos(cont->pts[cont->psize - 1].y), 
                       symbol, scaleSizes * (modPtSize + openAdd), flags);
      }
    }
  }

  b3dLineWidth(scaleSizes);
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
                                       scaleSizes);
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
                                                 scaleSizes);
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
  QString qstr;
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

  if (mVi->nt) {
    int time = mTimeLock ? mTimeLock : mVi->ct;
    if (mToolTime != time){
      mToolTime = time;
      qstr.sprintf(" (%3d)", time);
      qstr += ivwGetTimeIndexLabel(mVi, time);
      mQtWindow->setTimeLabel(qstr);
    }
  }
}

void ZapFuncs::setCursor(int mode, bool setAnyway)
{
  Qt::CursorShape shape;

  // Set up a special cursor for the rubber band
  if (mStartingBand || (mRubberband && (moveband || dragband)) ||
      mShiftingCont) {
    if (mStartingBand || moveband || mShiftingCont)
      shape = Qt::SizeAllCursor;
    else if (dragging[0] && dragging[2] || dragging[1] && dragging[3])
      shape = Qt::SizeFDiagCursor;
    else if (dragging[1] && dragging[2] || dragging[0] && dragging[3])
      shape = Qt::SizeBDiagCursor;
    else if (dragging[0] || dragging[1])
      shape = Qt::SizeHorCursor;
    else if (dragging[2] || dragging[3])
      shape = Qt::SizeVerCursor;
    if (shape != mLastShape || setAnyway) {

      // This one makes the cursor show up a little better on starting/MAC
      imod_info_input();
      mGfx->setCursor(QCursor(shape));
    }
    mLastShape = shape;
    return;
  }

  // Or restore cursor from rubber band or change cursor dur to mode change
  if (mMousemode != mode || mLastShape >= 0 || setAnyway) {
    if (mode == IMOD_MMODEL) {
      mGfx->setCursor(*App->modelCursor);
    } else {
      mGfx->unsetCursor();
    }
    mMousemode = mode;
    mLastShape = -1;
    imod_info_input();
  }
  return;
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

/*

$Log$
Revision 4.161  2011/02/07 16:12:39  mast
Convert zap structure to class, most functions to members

Revision 4.160  2011/02/04 03:53:18  mast
Added second fixed point to mouse stretching

Revision 4.159  2011/01/31 04:21:54  mast
Use new wheel scaling function and make scroll wheel size change undoable

Revision 4.158  2011/01/13 20:29:29  mast
stipple gaps; draw connector squares one pixel bigger

Revision 4.157  2010/12/18 05:44:30  mast
Use new common functions for montage snapshots

Revision 4.156  2010/12/15 06:14:41  mast
Changes for setting resolution in image snapshots

Revision 4.155  2010/12/08 05:34:16  mast
Fixed rubberband Z limits when there is Z binning for message and trimvol

Revision 4.154  2010/04/21 22:47:54  mast
Allowed full size piece for panel with scale bar in montage snapshot

Revision 4.153  2010/04/21 05:04:18  mast
Copy montage snapshot pieces from middle of overlap to minimize scalebar
 truncation; improve scaling with whole 1:1 montage

Revision 4.152  2010/04/01 02:41:48  mast
Called function to test for closing keys, or warning cleanup

Revision 4.151  2010/02/22 21:34:09  mast
Stop drawing points below threshold

Revision 4.150  2010/01/22 03:05:51  mast
Call new function to make scale bar work when doing montage snapshot

Revision 4.149  2009/11/11 19:28:46  mast
Changes for hot key to break contour

Revision 4.148  2009/09/02 18:41:02  mast
Fixed crash when adding with limited points per contour and empty contours

Revision 4.147  2009/06/05 15:44:02  mast
Keep track of mouse press/release and adjust button state after flushing
mouse moves so there is no move after a release

Revision 4.146  2009/04/22 03:43:44  mast
Store last mouse position on mouse click

Revision 4.145  2009/04/21 05:43:29  mast
debug version

Revision 4.144  2009/04/06 19:37:54  mast
Changes to preserve cursor state in Mac Qt 4.5

Revision 4.143  2009/03/30 18:27:09  mast
Check in the right file

Revision 4.141  2009/03/26 05:42:01  mast
New nearest section ghost mode

Revision 4.140  2009/03/22 21:18:49  mast
Keep general events from being processed whenever app or window is closing

Revision 4.139  2009/03/22 19:41:51  mast
Changes for cocoa/OS 10.5: fill toolbar text boxes 3 times, test if window
open before passing on general events

Revision 4.138  2009/03/14 00:43:33  mast
Made sure mouse move set limits regardless, fixed rubberband moving float

Revision 4.137  2009/03/10 04:34:33  mast
Draw triangles for open contour ends, draw squares at connectors

Revision 4.136  2009/03/05 00:59:16  mast
Flush mouse move events to get to most recent one when appropriate

Revision 4.135  2009/02/25 05:35:08  mast
Turn off dialog updates during continuous draw; add shift-Page commands

Revision 4.134  2009/01/15 16:33:18  mast
Qt 4 port

Revision 4.133  2008/12/17 00:10:06  mast
Switched trimvol statement from -yz to -rx

Revision 4.132  2008/12/08 17:27:42  mast
Fixed crash potential and background line problems with montage snapshots
Added scaling of sizes and thicknesses; added whole 1:1 snapshot

Revision 4.131  2008/11/28 06:39:45  mast
Don't draw extra ojects with odel view only flag set

Revision 4.130  2008/11/14 20:05:52  mast
Stopped drawing cross in center of sphere if on section only

Revision 4.129  2008/11/07 19:27:53  mast
Fixed setting of subarea at end of dragging rubberband

Revision 4.128  2008/09/24 02:40:03  mast
Call new attach function

Revision 4.127  2008/09/23 15:13:44  mast
Added mouse wheel scrolling of point size

Revision 4.126  2008/08/19 20:01:40  mast
Made it zoom with + as well as =

Revision 4.125  2008/08/01 19:24:01  mast
Added protection against small movements soon after button 2 press

Revision 4.124  2008/08/01 15:37:55  mast
Moved draw routine to global so imodview can draw top zap

Revision 4.123  2008/07/16 04:29:46  mast
Made drag drawing respect contour limit

Revision 4.122  2008/05/27 05:41:12  mast
Adapted to gray-scale snapshot, fixed current point change on mouse up

Revision 4.121  2008/05/23 04:31:44  mast
Changed to do nontiff montage snapshots

Revision 4.120  2008/05/02 20:40:50  mast
Do not draw extra object if its OFF flag is set

Revision 4.119  2008/04/04 21:22:19  mast
Free contour after adding to object, fix freeing of extra obj

Revision 4.118  2008/03/06 00:08:38  mast
Made rubber band moving happen before letting a plugin take a mouse event

Revision 4.117  2008/02/22 00:35:43  sueh
bug# 1076 In zapPrintInfo removed \n from the return string because it
messes up the trimvol process.

Revision 4.116  2008/02/06 16:33:06  sueh
bug# 1065, bug# 1076 Simplified getLowHighSection.  Made drawing optional in
zapToggleRubberband.  Turn off rubberband when flipping.  Send an error to the
info windows when the low and high sections are out of range.  Pass the trimvol
string back from zapPrintInfo.  In zapPrintInfo made printing the info optional.

Revision 4.115  2008/02/05 20:30:00  sueh
bug# 1065 In zapPrintInfo removed test call to zapReportRubberband.

Revision 4.114  2008/02/05 19:57:47  sueh
bug# 1065 Added getLowHighSection to get the low and high section numbers
from the ZaP windows.  Added low and high section numbers to the output from
zapPrintInfo and zapReportRubberband.  In zapToggleRubberband set the low
and high section toolbar field state in the ZaP window.

Revision 4.113  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 4.112  2008/01/20 17:40:36  mast
Oops, needed to multiply not divide by 255 for setting ghost color

Revision 4.111  2008/01/19 22:29:49  mast
Fixed call to set custom ghost color in extra object draw, although
not clear why it is there!

Revision 4.110  2008/01/14 19:48:22  mast
Added function to return mouse image coords to plugin

Revision 4.109  2008/01/13 23:19:22  mast
Fixed some compilation issues

Revision 4.108  2008/01/13 22:58:35  mast
Changes for multi-Z window

Revision 4.107  2007/12/04 18:46:13  mast
Moved functions to util, added cursor-like and other drawing capabilities and
expanded draw control when plugin uses mouse.

Revision 4.106  2007/11/27 17:53:32  mast
Add stipple display if enabled

Revision 4.105  2007/11/16 23:12:12  mast
Use new routines to set and restore ghost color

Revision 4.104  2007/11/10 04:07:10  mast
Changes for setting snapshot directory

Revision 4.103  2007/09/20 22:05:37  mast
Defined RADIANS_PER_DEGREE once

Revision 4.102  2007/08/13 16:04:50  mast
Changes for locator window

Revision 4.101  2007/07/19 22:29:19  mast
Added hot keys for jumping to set limits in time

Revision 4.100  2007/07/12 17:32:07  mast
Changed for new version of offset routine

Revision 4.99  2007/06/26 21:57:56  sueh
bug# 1021 Removed win_support.

Revision 4.98  2007/06/26 17:08:40  sueh
bug# 1021 Moved the bodies of shared functions setControlAndLimits,
zapDraw, and zapStepZoom to win_support.

Revision 4.97  2007/06/15 21:19:07  mast
Save and restore ctrlist current item when scanning list just to be safe

Revision 4.96  2007/06/08 04:45:15  mast
Implemented ability to constrain open contours to planes

Revision 4.95  2007/06/07 03:56:39  mast
Fix xyzmouse position when a drag draw finishes, and update info

Revision 4.94  2007/05/31 16:23:10  mast
Changes for using hot toolbar

Revision 4.93  2007/05/29 14:51:00  mast
Found most recent active zap for rubberband report, changed info output
to give trimvol info with size, offset info secondary

Revision 4.92  2007/05/06 03:26:09  mast
Added calls to b3dSetMovieSnapping

Revision 4.91  2007/03/29 04:55:49  mast
Fixed crash bug when closing window while focus is in edit/spinbox

Revision 4.90  2007/03/09 15:27:50  mast
Added ability to autosnapshot montages

Revision 4.89  2006/10/05 15:41:32  mast
Provided for primary and second non-TIFF snapshot format

Revision 4.88  2006/10/04 20:08:23  mast
Fixed window size being too small for toolbar due to new size text

Revision 4.87  2006/09/18 15:51:51  mast
Stopped time movie with a time step action

Revision 4.86  2006/09/17 18:15:59  mast
Changes to provide mouse position to pixelview

Revision 4.85  2006/09/06 23:11:29  mast
Fixed Ctrl-A with no rubberband and added Ctrl-Shift A for all objects

Revision 4.84  2006/08/31 23:27:45  mast
Changes for stored value display

Revision 4.83  2006/08/24 21:28:25  mast
Refresh graph windows when rubber band changes, fixed an initialization bug

Revision 4.82  2006/07/05 04:16:04  mast
Use flag to determine which color to give which image in overlay

Revision 4.81  2006/07/03 04:14:21  mast
Changes for beadfixer overlay mode

Revision 4.80  2006/06/09 20:25:39  mast
Added ability to display spheres on center section only

Revision 4.79  2006/04/01 23:43:14  mast
Added size output to toolbar

Revision 4.78  2006/03/01 19:13:06  mast
Moved window size/position routines from xzap to dia_qtutils

Revision 4.77  2006/02/13 05:13:04  mast
Call plugins with mouse events

Revision 4.76  2006/01/26 18:45:04  mast
Set up montage snapshot to defer swapping and read from back buffer

Revision 4.75  2006/01/25 23:11:41  mast
Fixed zap montage snapshot for quadro card and prevented negative overlaps

Revision 4.74  2005/09/15 14:31:52  mast
Added montage snapshot

Revision 4.73  2005/09/12 14:24:06  mast
Added function to get rubber band coordinates

Revision 4.72  2005/09/11 19:29:23  mast
Added fine-grained display properties to ghost display

Revision 4.71  2005/06/29 05:41:03  mast
Register changes properly on drag additions with fine grain storage

Revision 4.70  2005/06/26 19:38:10  mast
Added logic for fine-grained changes

Revision 4.69  2005/06/16 21:46:59  mast
Allowed Ctrl-A selection of open contours passing through selection area

Revision 4.68  2005/06/15 23:08:36  mast
Made 1 and 2 change locked time, fixed buffer flushing problem with locked
time, and allowed Ctrl A to select contours outside the image range

Revision 4.67  2005/04/12 14:47:39  mast
Changed handling of subset area recording so that it occurs right
within the draw, after area is determined, then bwfloat is called to
adjust contrast before the pixel draw is done.  This happens for
active window and also on external draw of single zap.

Revision 4.66  2005/03/30 02:39:15  mast
Fixed bugs in yesterdays additions

Revision 4.65  2005/03/29 00:59:11  mast
Made mouse change when moved over rubber band; moved time to 2nd toolbar

Revision 4.64  2005/03/20 19:55:37  mast
Eliminating duplicate functions

Revision 4.63  2005/03/08 02:30:21  mast
Made sure subarea is set properly after a resize and translations

Revision 4.62  2005/03/07 06:17:23  mast
Synchronized toolbar button properly when turning off rubberband

Revision 4.61  2005/02/24 22:36:00  mast
Implemented multiple-object selection for Ctrl-A and the drag-select,
and allowed contours from multiple objects to be shifted/transformed

Revision 4.60  2005/02/19 01:30:46  mast
Set center of rotation wih ctrl-button 2; allow shift/xform when no
current point is defined

Revision 4.59  2005/02/09 01:18:45  mast
Added ability to rotate/stretch/scale contours, made it work with multiple
contours

Revision 4.58  2004/12/22 15:21:15  mast
Fixed problems discovered with Visual C compiler

Revision 4.57  2004/12/02 21:41:59  mast
Fixed method for detecting segment crossing in new drag select

Revision 4.56  2004/12/01 18:19:14  mast
Added Ctrl-drag selection, converted help to html

Revision 4.55  2004/11/29 19:25:21  mast
Changes to do QImage instead of RGB snapshots

Revision 4.54  2004/11/21 05:50:34  mast
Switch from int to float for nearest point distance measurement

Revision 4.53  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 4.52  2004/11/12 01:21:55  mast
Made line thicknesses be 1 for current image point and band, and have
object's thickness for current contour markers

Revision 4.51  2004/11/04 17:02:41  mast
Changes for switching to shifting contour as a mode that is turned on

Revision 4.50  2004/11/02 20:22:35  mast
Added contour shift capability; switched to curpoint color for current point

Revision 4.49  2004/11/02 00:52:32  mast
Added multiple selection logic and image-based rubber band

Revision 4.48  2004/09/10 02:31:04  mast
replaced long with int

Revision 4.47  2004/08/31 01:27:15  mast
Changed info output to be in unbinned coordinates and used floor instead
of int to avoid rounding error of negative coordinates

Revision 4.46  2004/07/11 18:32:17  mast
Fixed bug in detecting when to start new contour after autocontouring.
Implemented light ghost option and all object ghost option.  Used new
functions for getting contour to add points to and setting new contour time.

Revision 4.45  2004/07/10 23:30:49  mast
FIxed subset area for autocontrast, made ghost have object line thickness

Revision 4.44  2004/06/23 04:46:28  mast
Fixed hang on opening window at 0.1 zoom in Windows

Revision 4.43  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.42  2004/05/31 02:16:15  mast
Changed to starting a new contour if time does not match for any kind of
object, not just closed contour

Revision 4.41  2004/05/07 22:10:45  mast
Added text to identify rubberband report

Revision 4.40  2004/05/05 17:34:51  mast
Added rubberband toolbutton and call to report coordinates from first\zap window with

Revision 4.39  2004/01/05 18:35:46  mast
Divide point sizes by xy binning for display

Revision 4.38  2003/12/18 22:47:27  mast
Fixed problem with float when starting movie snapshots, implemented ability
to start at current point, and made changes because of slicer movies

Revision 4.37  2003/11/25 01:15:02  mast
Move the window again after the show for Mac OS 10.3

Revision 4.36  2003/11/13 20:08:16  mast
made spillover factor in setting zoom to a remembered window size the
same as factor when no remembered size is available

Revision 4.35  2003/10/31 01:29:24  mast
Reset drawn area when rubber band is turned off

Revision 4.34  2003/10/30 06:28:44  mast
Specified that "a" hot key is lower case

Revision 4.33  2003/09/25 21:10:04  mast
Keep zap window on the screen when it is positioned from settings

Revision 4.32  2003/09/24 17:40:31  mast
Switch to restorable geometry call for resizing

Revision 4.31  2003/09/24 00:49:04  mast
Switched from keeping track of geometry to keeping track of pos() and
size() when saving and restoring positions and sizes

Revision 4.30  2003/09/18 00:47:20  mast
Added some functions for limiting window area to the desktop, with system-
dependent Y positions; got geometry setting stable by setting it after the
window is first displayed; changed mouse panning to move image along with
mouse instead of slower than mouse for fractional zooms; added ability to
keep track of window or rubberband limits of image for last active window.

Revision 4.28  2003/09/17 04:45:37  mast
Added ability to use window size from settings, made it zoom images
up or down as appropriate to fit window, and rationalized the window
size and position limiting logic

Revision 4.27  2003/09/16 02:58:39  mast
Changed to access image data using new line pointers and adjust for
slight changes in X zoom with fractional zooms

Revision 4.26  2003/09/13 04:30:31  mast
Switch to getting and setting geometry when resize to fit, to prevent the
window from moving when there is no size change.
Also add a hack to prevent bad drawing after resize on Mac with Qt 3.2.1

Revision 4.25  2003/08/08 16:40:51  mast
Fixed problem with current point display when model display turned off

Revision 4.24  2003/06/27 19:25:28  mast
Add ability to draw extra object that is not part of model

Revision 4.23  2003/06/18 05:54:33  mast
Start a new contour if section or time changes while dragging

Revision 4.22  2003/05/23 02:44:20  mast
Fix syncing to new point in other Zap windows

Revision 4.21  2003/05/04 18:37:14  mast
Fixing help

Revision 4.20  2003/05/03 00:16:59  mast
Fixed problem on connecting lines being drawn between non-contiguous points
for wild contours; drawing non-wild contours separately shuld also speed
them up.

Revision 4.19  2003/04/25 03:28:33  mast
Changes for name change to 3dmod

Revision 4.18  2003/04/18 20:16:39  mast
Rename meta test function

Revision 4.17  2003/04/18 20:08:55  mast
Reject the Ctrl (meta) key on the Mac

Revision 4.16  2003/04/17 19:06:50  mast
various changes for Mac

Revision 4.15  2003/04/14 15:38:10  mast
Fixing help error

Revision 4.14  2003/04/14 15:31:02  mast
fixing documentation

Revision 4.13  2003/03/25 23:01:35  mast
Take nearest int in checking for current point Z value when adding points

Revision 4.12  2003/03/24 17:56:46  mast
Register with dialogManager so it can be parked with info window

Revision 4.11  2003/03/13 01:20:08  mast
Convert numlock keypad keys so num lock can be on

Revision 4.10  2003/03/12 21:35:23  mast
Test if no CIImage is returned and give error message

Revision 4.9  2003/03/12 06:36:53  mast
Fixed problem of adding or modifying points at the wrong time

Revision 4.8  2003/03/04 05:38:48  mast
cleanup

Revision 4.7  2003/03/04 05:35:48  mast
Fix current point size bug

Revision 4.6  2003/03/03 22:43:43  mast
Added ability to display spheres for all points with size and eliminated
separate routine for drawing current contour.
Made rubber band work by initially defining it, and added cursor changes
when the rubber band is grabbed.
Implemented dynamic sizes for current and end point markers.
Added continuous mouse tracking while the Insert key is down, so it works
with or without keyboard repeat being set.
Improved tracking of initial movements with left mouse button.

Revision 4.5  2003/02/28 20:58:55  mast
adjusting geometry for Windows

Revision 4.4  2003/02/27 19:49:39  mast
Set geometry to stay on screen for Windows

Revision 4.3  2003/02/20 16:02:15  mast
Make current contour not display at wrong time

Revision 4.2  2003/02/14 01:12:47  mast
cleanup unused variables

Revision 4.1  2003/02/10 20:29:03  mast
autox.cpp

Revision 1.1.2.20  2003/02/04 19:08:29  mast
fix syncing to model point when changing contour or point via hotkey

Revision 1.1.2.19  2003/01/30 06:17:47  mast
Add ability to change range of Z slider on image flip

Revision 1.1.2.18  2003/01/30 00:46:27  mast
New timer logic and cleanup

Revision 1.1.2.17  2003/01/29 01:49:33  mast
changes for colormap mode, fix closing calls to ivwControl

Revision 1.1.2.16  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.15  2003/01/23 20:12:47  mast
implement variable ghost distance

Revision 1.1.2.14  2003/01/14 21:52:38  mast
include new movie controller include file

Revision 1.1.2.13  2003/01/13 01:15:43  mast
changes for Qt version of info window

Revision 1.1.2.12  2003/01/06 15:51:17  mast
Use imodcaption and viewport setting routines

Revision 1.1.2.11  2003/01/04 03:42:05  mast
simplified closing logic

Revision 1.1.2.10  2003/01/02 15:44:19  mast
accept key input from controller

Revision 1.1.2.9  2002/12/17 22:28:21  mast
cleanup of unused variables and SGI errors

Revision 1.1.2.8  2002/12/17 18:28:47  mast
Adding timer for second draw after resize

Revision 1.1.2.7  2002/12/14 05:23:42  mast
backing out the fancy subclass, adjusting for new visual detection

Revision 1.1.2.6  2002/12/13 07:09:19  mast
GLMainWindow needed different name for mouse event processors

Revision 1.1.2.5  2002/12/13 06:06:29  mast
using new glmainwindow and mainglwidget classes

Revision 1.1.2.4  2002/12/12 01:24:50  mast
Added Z slider

Revision 1.1.2.3  2002/12/10 16:57:34  mast
preventing multiple draws, implementing current contour draw while dragging

Revision 1.1.2.2  2002/12/09 23:23:49  mast
Plugged image memory leak

Revision 1.1.2.1  2002/12/09 17:50:33  mast
Qt version

Revision 3.9  2002/12/01 16:51:34  mast
Changes to eliminate warnings on SGI

Revision 3.8  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.7  2002/11/14 01:15:56  mast
Prevent 3rd mouse button drag from moving scattered points or points off
the section

Revision 3.6  2002/10/22 22:41:47  mast
Changed some debug messages for the expose timeouts

Revision 3.5  2002/09/13 21:03:58  mast
Changes to minimize crashes with Ti4600 when resizing window with R -
elimination of interfering draws, and postpone of draw after expose events

Revision 3.4  2002/07/28 22:58:42  mast
Made I pop Info window to front and added a button to toolbar to do this

Revision 3.3  2002/07/21 20:29:50  mast
changed number of columns for section number to 4

Revision 3.2  2002/01/28 16:53:59  mast
Added section number to call to b3dDrawGreyScalePixelsHQ

Revision 3.1  2001/12/17 18:52:40  mast
Added hotkeys to do smoothing and next section in autocontouring

*/
