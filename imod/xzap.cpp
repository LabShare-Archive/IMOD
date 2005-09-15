/*
 *  xzap.c -- The Zap Window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$
Log at end of file
*/

#include <math.h>
#include <qcursor.h>
#include <qdatetime.h>
#include <qapplication.h>
#include <qclipboard.h>
#include <qpoint.h>
#include <qtimer.h>
#include <qobjectlist.h>
#include "zap_classes.h"

#include "imod.h"
#include "imod_display.h"
#include "b3dgfx.h"
#include "xzap.h"
#include "control.h"
#include "imodplug.h"
#include "imod_info.h"
#include "imod_info_cb.h"
#include "imod_input.h"
#include "imod_moviecon.h"
#include "autox.h"
#include "imod_edit.h"
#include "imod_model_edit.h"
#include "imod_workprocs.h"
#include "dia_qtutils.h"
#include "preferences.h"
#include "undoredo.h"
#include "finegrain.h"

#define RADIANS_PER_DEGREE 0.0174532925

static void zapDraw_cb(ImodView *vi, void *client, int drawflag);
static void zapClose_cb(ImodView *vi, void *client, int drawflag);
static void zapKey_cb(ImodView *vi, void *client, int released, QKeyEvent *e);
static void zapDraw(ZapStruct *zap);
static void zapAnalyzeBandEdge(ZapStruct *zap, int ix, int iy);
static void zapButton1(struct zapwin *zap, int x, int y, int controlDown);
static void zapButton2(struct zapwin *zap, int x, int y, int controlDown);
static void zapButton3(struct zapwin *zap, int x, int y, int controlDown);
static void zapB1Drag(struct zapwin *zap, int x, int y);
static void zapB2Drag(struct zapwin *zap, int x, int y, int controlDown);
static void zapB3Drag(struct zapwin *zap, int x, int y, int controlDown, 
                      int shiftDown);
static void zapDelUnderCursor(ZapStruct *zap, int x, int y, Icont *cont);
static int contInSelectArea(Iobj *obj, Icont *cont, Ipoint selmin,
                            Ipoint selmax);
static void dragSelectContsCrossed(struct zapwin *zap, int x, int y);
static void endContourShift(ZapStruct *zap);
static Icont *checkContourShift(ZapStruct *zap, int &pt, int &err);
static void setupContourShift(ZapStruct *zap);
static void startShiftingContour(ZapStruct *zap, int x, int y, int button,
                                 int ctrlDown);
static void shiftContour(ZapStruct *zap, int x, int y, int button, 
                         int shiftDown);
static void startMovieCheckSnap(ZapStruct *zap, int dir);
static void registerDragAdditions(ZapStruct *zap);
static int mouseXformMatrix(ZapStruct *zap, int x, int y, int type, 
                            float mat[2][2]);
static void markXformCenter(ZapStruct *zap, float ix, float iy);

static void zapDrawGraphics(ZapStruct *zap);
static void zapDrawModel(ZapStruct *zap);
static void zapDrawContour(ZapStruct *zap, int co, int ob);
static void zapDrawCurrentPoint(ZapStruct *zap);
static void zapDrawExtraObject(ZapStruct *zap);
static int  zapDrawAuto(ZapStruct *zap);
static void zapDrawGhost(ZapStruct *zap);
static void zapSetGhostColor(ZapStruct *zap, float obr, float obg, float obb);
static void zapDrawTools(ZapStruct *zap);
static void zapSetCursor(ZapStruct *zap, int mode);
static int  zapXpos(ZapStruct *zap, float x);
static int  zapYpos(ZapStruct *zap, float x);
static void zapGetixy(ZapStruct *zap, int mx, int my, float *x, float *y);
static int  zapPointVisable(ZapStruct *zap, Ipoint *pnt);
static void zapAutoTranslate(ZapStruct *zap);
static void zapSyncImage(ZapStruct *win);
static void zapResizeToFit(ZapStruct *zap);
static void setAreaLimits(ZapStruct *zap);
static void setControlAndLimits(ZapStruct *zap);
static void zapToggleRubberband(ZapStruct *zap);
static void zapBandImageToMouse(ZapStruct *zap, int ifclip); 
static void zapBandMouseToImage(ZapStruct *zap, int ifclip);
static void montageSnapshot(ZapStruct *zap);
static void getMontageShifts(ZapStruct *zap, int factor, int imStart, 
                             int border, int imSize, int winSize,
                             int &transStart, int &transDelta, int &copyDelta,
                             int &fullSize);

static int dragRegisterSize = 10;

/* DNM 1/19/01: add this to allow key to substitute for middle mouse button */
static int insertDown = 0;

static QTime insertTime;
static QTime but1downt;

static int numZapWindows = 0;
static int subStartX = 0;
static int subStartY = 0;
static int subEndX = 0;
static int subEndY = 0;

void zapHelp()
{
  imodShowHelpPage("zap.html");
}

// This receives the close signal back from the controller, tells the
// window to close, and sets the closing flag
static void zapClose_cb(ImodView *vi, void *client, int junk)
{
  ZapStruct *zap = (ZapStruct *)client;
  if (imodDebug('z'))
    imodPrintStderr("Sending zap window close.\n");
  zap->qtWindow->close();
}

/* This receives a closing signal from the window */
void zapClosing(ZapStruct *zap)
{
  if (imodDebug('z'))
    imodPrintStderr("ZapClosing received.\n");

  // Do cleanup
  zap->popup = 0;
  ivwRemoveControl(zap->vi, zap->ctrl);
  imodDialogManager.remove((QWidget *)zap->qtWindow);
  numZapWindows--;

  // What for?  flush any events that might refer to this zap
  imod_info_input();     

  b3dFreeCIImage(zap->image);
  free(zap);
}


/* start or stop movie and check for whether to start a movie snapshot 
   sequence */
static void startMovieCheckSnap(ZapStruct *zap, int dir)
{
  int start, end;

  imodMovieXYZT(zap->vi, MOVIE_DEFAULT, MOVIE_DEFAULT, dir, MOVIE_DEFAULT);
  imcSetStarterID(zap->ctrl);

  zap->movieSnapCount = 0;

  /* done if no movie, or if no snapshots are desired.  */
  if (!zap->vi->zmovie || !imcGetSnapshot(zap->vi))
    return;
     
  /* Get start and end of loop, compute count */
  imcGetStartEnd(zap->vi, 2, &start, &end);
  zap->movieSnapCount = (end - start) / imcGetIncrement(zap->vi, 2) + 1;
  if (zap->movieSnapCount < 1)
    zap->movieSnapCount = 1;

  /* double count for normal mode, leave as is for one-way */
  if (!imcGetLoopMode(zap->vi))
    zap->movieSnapCount *= 2;

  /* Set to start or end depending on which button was hit */
  if (!imcStartSnapHere(zap->vi))
    zap->vi->zmouse = dir > 0 ? start : end;

  /* draw - via imodDraw to get float done correctly */
  imodDraw(zap->vi, IMOD_DRAW_XYZ);
}

// This is the external draw command from the controller
void zapDraw_cb(ImodView *vi, void *client, int drawflag)
{
  ZapStruct *zap = (ZapStruct *)client;
  int *limits;
  int limarr[4];

  if (imodDebug('z'))
    imodPrintStderr("Zap Draw\n");

  if (!zap)
    return;
  if ((!zap->popup) || (!zap->ginit)) 
    return;
     
  zapSetCursor(zap, vi->imod->mousemode);

  if (drawflag & IMOD_DRAW_COLORMAP) {
    zap->gfx->setColormap(*(App->qColormap));
    return;
  }

  // zapDrawTools(zap);

  if (drawflag){
    if (drawflag & IMOD_DRAW_SLICE)
      zap->showslice = 1;

    /* DNM: skip this, it is covered by the zapdraw call below and the
       items that it sets are not needed by the flush or sync */
    /* b3dWinset(XtDisplay(zap->gfx), zap->gfx, (XID)zap->context); */

    if (drawflag & IMOD_DRAW_IMAGE){
      b3dFlushImage(zap->image);
    }
          
    if (!(drawflag & IMOD_DRAW_ACTIVE) && !(drawflag & IMOD_DRAW_NOSYNC))
      zapSyncImage(zap);

    /* DNM 1/29/03: no more worry about multiple calls */
    zapDraw(zap);

    /* DNM 3/8/01: add autosnapshot when movieing */
    if (imcGetSnapshot(zap->vi) && zap->vi->zmovie && 
        zap->movieSnapCount && imcGetStarterID() == zap->ctrl) {
      limits = NULL;
      if (zap->rubberband) {
        limits = limarr;
        zapBandImageToMouse(zap, 1);
        limarr[0] = zap->rbMouseX0 + 1;
        limarr[1] = zap->winy - zap->rbMouseY1;
        limarr[2] = zap->rbMouseX1 - 1 - zap->rbMouseX0;
        limarr[3] = zap->rbMouseY1 - 1 - zap->rbMouseY0;
      }
      if (imcGetSnapshot(zap->vi) == 1)
        b3dAutoSnapshot("zap", SnapShot_TIF, limits);
      else
        b3dAutoSnapshot("zap", SnapShot_RGB, limits);
      zap->movieSnapCount--;
      /* When count expires, stop movie */
      if(!zap->movieSnapCount)
        zap->vi->zmovie = 0;
    }

    // If there is only one zap window, set flag to record the subarea
    if (imodDialogManager.windowCount(ZAP_WINDOW_TYPE) == 1)
      zap->recordSubarea = 1;
  }
  return;
}

/*
 *  Sync the pan position to the current model point. 
 */
#define BORDER_FRAC  0.1
#define BORDER_MIN  50
#define BORDER_MAX  125

static void zapSyncImage(ZapStruct *win)
{
  int syncborder, wposition, wsize, tripshift;
  int trytrans, trydraws, tryborder, trystart;
  ImodView *vi = win->vi;
  if ((!win->lock) && (vi->imod->mousemode == IMOD_MMODEL)){
    if (win->vi->imod->cindex.point >= 0){

      /* If the keepcentered flag is set, just do a shift to center */
      if (win->keepcentered)
        tripshift = 1;
      else {

        /* Otherwise, look at each axis independently.  First see if
           if the position is within the borders for shifting */
        tripshift = 0;
        wsize = win->winx;
        wposition = zapXpos(win, vi->xmouse);
        syncborder = (int)(wsize * BORDER_FRAC);
        if (syncborder < BORDER_MIN)
          syncborder = BORDER_MIN;
        if (syncborder > BORDER_MAX)
          syncborder = BORDER_MAX;
        if (wposition < syncborder || wposition > wsize - syncborder){

          /* If close to a border, do an image offset computation
             to see if the display would actually get moved if
             this axis were centered on point */
          trytrans = (int)((vi->xsize * 0.5f) - vi->xmouse + 0.5f);
          trydraws = win->xdrawsize;
          tryborder = win->xborder;
          trystart = win->xstart;
          /* imodPrintStderr ("before %d %d %d %d\n", 
             trydraws, win->xtrans, tryborder, trystart); */
          b3dSetImageOffset(wsize, vi->xsize, win->zoom, &trydraws,
                            &trytrans, &tryborder, &trystart);
          /* imodPrintStderr ("after %d %d %d %d\n", 
             trydraws, trytrans, tryborder, trystart); */
          /* Can't use xtrans for a test, need to use the other
             two values to see if change in display would occur */
          if (tryborder != win->xborder || trystart != win->xstart)
            tripshift += 1;

        }

        /* Same for Y axis */
        wsize = win->winy;
        wposition = zapYpos(win, vi->ymouse);
        syncborder = (int)(wsize * BORDER_FRAC);
        if (syncborder < BORDER_MIN)
          syncborder = BORDER_MIN;
        if (syncborder > BORDER_MAX)
          syncborder = BORDER_MAX;
        if (wposition < syncborder || wposition > wsize - syncborder){
          trytrans = (int)((vi->ysize * 0.5f) - vi->ymouse + 0.5f);
          trydraws = win->ydrawsize;
          tryborder = win->yborder;
          trystart = win->ystart;
          b3dSetImageOffset(wsize, vi->ysize, win->zoom, &trydraws,
                            &trytrans, &tryborder, &trystart);
          if (tryborder != win->yborder || trystart != win->ystart)
            tripshift += 2;
        }
      }

      if (tripshift) {
        /* imodPrintStderr("tripshift %d\n",tripshift); */
        win->xtrans = (int)((vi->xsize * 0.5f) - vi->xmouse + 0.5f);
        win->ytrans = (int)((vi->ysize * 0.5f) - vi->ymouse + 0.5f);
      }
    }
  }
}


// This receives the resize events which precede paint signals
void zapResize(ZapStruct *zap, int winx, int winy)
{
  ivwControlPriority(zap->vi, zap->ctrl);

  if (imodDebug('z'))
    imodPrintStderr("RESIZE: ");

  if (imodDebug('z')) {
    imodPrintStderr("Size = %d x %d :", winx, winy);
    if (zap->ginit)
      imodPrintStderr("Old Size = %d x %d :", zap->winx, zap->winy);
  }

  zap->winx = winx;
  zap->winy = winy;
  b3dSetCurSize(winx, winy);
 
  if (zap->ginit){

    // 11/11/04: rubber band now takes care of itself
    b3dFlushImage(zap->image);
  }

  b3dResizeViewportXY(winx, winy);

  zap->image =  b3dGetNewCIImage(zap->image, App->depth);
  if (!zap->image) {
    wprint("\aInsufficient memory to run this Zap window.\n"
            "Try making it smaller or close it.\n");
    return;
  }

  b3dBufferImage(zap->image);
  zap->ginit = 1;
  zap->recordSubarea = 1;

  if (imodDebug('z'))
    imodPrintStderr("\n");
  return;
}


/* 1/29/03: removed the resize and expose hack code and the report time code */

// This is the central drawing routine
static void zapDraw(ZapStruct *zap)
{
  zap->gfx->updateGL();
}

// This receives the paint events generated by the window manager
void zapPaint(ZapStruct *zap)
{
  int ob;
  if (imodDebug('z'))
    imodPrintStderr("Paint:");

  b3dSetCurSize(zap->winx, zap->winy);

  zapAutoTranslate(zap);

  // If the current only flag is set, swap the displayed buffer into the
  // drawing buffer and just draw the current contour
  if (zap->drawCurrentOnly > 0) {
    if (App->doublebuffer)
      zap->gfx->swapBuffers();
    ob = zap->vi->imod->cindex.object;
    imodSetObjectColor(ob); 
    b3dLineWidth(zap->vi->imod->obj[ob].linewidth2); 
    zapDrawContour(zap, zap->vi->imod->cindex.contour, ob);
    zap->drawCurrentOnly = -1;
    return;
  }
  
  zap->drawCurrentOnly = 0;

  /* DNM 1/29/03: no more skipping of further drawing */
  zapDrawGraphics(zap);

  zapDrawModel(zap);
  zapDrawCurrentPoint(zap);
  zapDrawExtraObject(zap);
  zapDrawAuto(zap);
  if (zap->rubberband) {
    b3dLineWidth(1);
    b3dColorIndex(App->endpoint);
    zapBandImageToMouse(zap, 0);
    b3dDrawRectangle(zap->rbMouseX0, zap->winy - 1 - zap->rbMouseY1, 
                     zap->rbMouseX1 - zap->rbMouseX0, 
                     zap->rbMouseY1 - zap->rbMouseY0);
  } 
  zapDrawTools(zap);

  if (imodDebug('z'))
    imodPrintStderr("\n");
}


/*****************************************************************************/
/* zap tool bar functions called from interface class                        */

void zapStepZoom(ZapStruct *zap, int step)
{
  setControlAndLimits(zap);
  zap->zoom = b3dStepPixelZoom(zap->zoom, step);
  zapDraw(zap);
}

void zapEnteredZoom(ZapStruct *zap, float newZoom)
{
  setControlAndLimits(zap);
  zap->zoom = newZoom;
  if (zap->zoom <= 0.01)
    zap->zoom = 0.01;
  zapDraw(zap);
  zap->qtWindow->setFocus();
}
 
void zapStateToggled(ZapStruct *zap, int index, int state)
{
  int time;
  setControlAndLimits(zap);
  switch (index) {
  case ZAP_TOGGLE_RESOL:
    zap->hqgfx = state;
    zapDraw(zap);
    break;

  case ZAP_TOGGLE_ZLOCK:
    zap->lock = state ? 2 : 0;
    if (!zap->lock) {
      b3dFlushImage(zap->image);
      zapSyncImage(zap);
      zapDraw(zap);
    }
    break;

  case ZAP_TOGGLE_CENTER:
    zap->keepcentered = state;
    if (state) {
      b3dFlushImage(zap->image);
      zapSyncImage(zap);
      zapDraw(zap);
    }
    break;

  case ZAP_TOGGLE_INSERT:
    zap->insertmode = state;
    zap->vi->insertmode = zap->insertmode;
    registerDragAdditions(zap);
    break;

  case ZAP_TOGGLE_RUBBER:
    zapToggleRubberband(zap);
    break;

  case ZAP_TOGGLE_TIMELOCK:
    ivwGetTime(zap->vi, &time);
    zap->timeLock = state ? time : 0;
    if (!zap->timeLock)
      zapDraw(zap);
    break;
  }
}




void zapEnteredSection(ZapStruct *zap, int sec)
{
  setControlAndLimits(zap);
  if (zap->lock != 2)
    zap->vi->zmouse = sec-1;
  zap->section = sec-1;
  ivwBindMouse(zap->vi);
  imodDraw(zap->vi, IMOD_DRAW_XYZ);
  zap->qtWindow->setFocus();
}

void zapStepTime(ZapStruct *zap, int step)
{
  setControlAndLimits(zap);
  
  // if time locked, advance the time lock and draw this window
  // Does this make sense?
  if (zap->timeLock){
    zap->timeLock += step;
    if (zap->timeLock <= 0)
      zap->timeLock = 1;
    if (zap->timeLock > ivwGetMaxTime(zap->vi))
      zap->timeLock = ivwGetMaxTime(zap->vi);
    zapDraw(zap);

  } else {
    if (step > 0)
      inputNextTime(zap->vi);
    else
      inputPrevTime(zap->vi);
  }
}


/*****************************************************************************/

int imod_zap_open(struct ViewInfo *vi)
{
  ZapStruct *zap;
  QString str;
  QRect oldGeom;
  int time, tmax, len, maxlen;
  int needWinx, needWiny;
  int maxWinx;
  int maxWiny;
  int newWidth, newHeight, xleft, ytop, toolHeight;
  double newZoom;

  zap = (ZapStruct *)malloc(sizeof(ZapStruct));
  if (!zap) return(-1);

  zap->vi     = vi;
  zap->ctrl   = 0;
  zap->xtrans = zap->ytrans = 0;
  zap->ztrans = 0;
  zap->hqgfx  = 0;
  zap->hide   = 0;
  zap->zoom   = 1.0;
  zap->data   = NULL;
  zap->image  = NULL;
  zap->ginit  = 0;
  zap->lock   = 0;
  zap->keepcentered = 0;
  zap->insertmode = 0;
  zap->toolstart = 0;
  zap->showslice = 0;
  zap->timeLock = 0;
  zap->toolSection = -1;
  zap->toolMaxZ = vi->zsize;
  zap->toolZoom = 0.0f;
  zap->toolTime = 0;
  zap->twod = (!(vi->dim&4));
  zap->sectionStep = 0;
  zap->time = 0;
  zap->mousemode = 0;
  zap->rubberband = 0;
  zap->startingBand = 0;
  zap->shiftingCont = 0;
  zap->shiftRegistered = 0;
  zap->movieSnapCount = 0;
  zap->drawCurrentOnly = 0;
  zap->dragAddCount = 0;

  /* Optional time section : find longest string and pass it in */
  if (vi->nt){
    str = " (999)";
    maxlen = -1;
    for (time = 1; time < zap->vi->nt; time++) {
      len = strlen(ivwGetTimeIndexLabel(zap->vi, time));
      if (len > maxlen) {
        maxlen = len;
        tmax = time;
      }
    }
    
    str += ivwGetTimeIndexLabel(zap->vi, tmax);
  }

  zap->qtWindow = new ZapWindow(zap, str, App->rgba, App->doublebuffer,
				App->qtEnableDepth, 
                                imodDialogManager.parent(IMOD_IMAGE),
                                "zap window");
  if (!zap->qtWindow){
    free(zap);
    wprint("Error opening zap window.");
    return(-1);
  }
  if (imodDebug('z'))
    imodPuts("Got a zap window");

  zap->gfx = zap->qtWindow->mGLw;
  if (!App->rgba)
    zap->gfx->setColormap(*(App->qColormap));

  zap->qtWindow->setCaption(imodCaption("3dmod ZaP Window"));

  zap->qtWindow->mToolBar->setLabel(imodCaption("ZaP Toolbar"));
  if (zap->qtWindow->mToolBar2)
    zap->qtWindow->mToolBar2->setLabel(imodCaption("Time Toolbar"));
  
  zap->ctrl = ivwNewControl(vi, zapDraw_cb, zapClose_cb, zapKey_cb,
                            (void *)zap);
  imodDialogManager.add((QWidget *)zap->qtWindow, IMOD_IMAGE, ZAP_WINDOW_TYPE);

  zapMaximumWindowSize(maxWinx, maxWiny);

  oldGeom = ImodPrefs->getZapGeometry();

  /* 1/28/03: this call is needed to get the toolbar size hint right */
  imod_info_input();
  QSize toolSize = zap->qtWindow->mToolBar->sizeHint();
  QSize toolSize2(0, 0);
  if (zap->qtWindow->mToolBar2)
    toolSize2 = zap->qtWindow->mToolBar2->sizeHint();
  toolHeight = zap->qtWindow->height() - zap->gfx->height();

  if (!oldGeom.width()) {

    // If no old geometry, adjust zoom if necessary to fit image on screen
    while (zap->zoom * vi->xsize > 1.1 * maxWinx || 
           zap->zoom * vi->ysize > 1.1 * maxWiny - toolHeight) {
      newZoom = b3dStepPixelZoom(zap->zoom, -1);
      if (fabs(newZoom - zap->zoom) < 0.0001)
        break;
      zap->zoom = (float)newZoom;
    }

    needWinx = (int)(zap->zoom * vi->xsize);

    // If Window is narrower than two toolbars, they will stack, so adjust
    // toolheight up if it is small; otherwise adjust it down if it is large
    if (needWinx < toolSize.width() + toolSize2.width()) {
      if (toolHeight < toolSize.height() + toolSize2.height())
        toolHeight += toolSize2.height();
    } else {
      if (toolHeight >= toolSize.height() + toolSize2.height()) 
        toolHeight -= toolSize2.height();
    }

    needWiny = (int)(zap->zoom * vi->ysize) + toolHeight;
    zapLimitWindowSize(needWinx, needWiny);

    // Make the width big enough for the toolbar, and add the difference
    // between the window and image widget heights to get height
    newWidth = toolSize.width() > needWinx ? toolSize.width() : needWinx;
    newHeight = needWiny;

    QRect pos = zap->qtWindow->geometry();
    xleft = pos.x();
    ytop = pos.y();

    zapLimitWindowPos(newWidth, newHeight, xleft, ytop);
    if (Imod_debug)
      imodPrintStderr("Sizes: zap %d %d, toolbar %d %d, GL %d %d: "
              "resize %d %d\n", zap->qtWindow->width(), 
              zap->qtWindow->height(), 
              toolSize.width(), toolSize.height(), zap->gfx->width(), 
              zap->gfx->height(), newWidth, newHeight);

  } else {

    // Existing geometry - better fit it to current screen
    xleft = oldGeom.x();
    ytop = oldGeom.y();
    newWidth = oldGeom.width();
    newHeight = oldGeom.height();
    zapLimitWindowSize(newWidth, newHeight);
    zapLimitWindowPos(newWidth, newHeight, xleft, ytop);

    // Adjust toolheight then adjust zoom either way to fit window
    needWinx = newWidth;
    if (needWinx < toolSize.width() + toolSize2.width()) {
      if (toolHeight < toolSize.height() + toolSize2.height())
        toolHeight += toolSize2.height();
    } else {
      if (toolHeight >= toolSize.height() + toolSize2.height()) 
        toolHeight -= toolSize2.height();
    }
    needWiny = newHeight - toolHeight;

    // If images are too big, zoom down until they almost fit
    // If images are too small, start big and find first zoom that fits
    // Apply same overflow criterion so that reopened windows will behave
    // the same as when they were first opened
    if (vi->xsize < needWinx && vi->ysize < needWiny)
      zap->zoom = (2. * needWinx) / vi->xsize;

    while ((zap->zoom * vi->xsize > 1.1 * needWinx || 
           zap->zoom * vi->ysize > 1.1 * needWiny)) {
      newZoom = b3dStepPixelZoom(zap->zoom, -1);
      // This test goes into infinite loop in Windows - Intel, 6/22/04
      // if (newZoom == zap->zoom)
      if (fabs(newZoom - zap->zoom) < 0.0001)
        break;
      zap->zoom = (float)newZoom;
    }

  }

  // 9/23/03: changed setGeometry to resize/move and this allowed elimination
  // of the setting again on the first real draw
  zap->qtWindow->resize(newWidth, newHeight);
  zap->qtWindow->move(xleft, ytop);
  zap->qtWindow->show();
  zap->popup = 1;

  // 11/24/03: OS 10.3 needs to move after the show, so just put this in without
  // checking if it would work generally
#ifdef Q_OS_MACX
  zap->qtWindow->move(xleft, ytop);
#endif    

  if (imodDebug('z'))
    imodPuts("popup a zap dialog");

  /* DNM: set cursor after window created so it has model mode cursor if
     an existing window put us in model mode */
  zapSetCursor(zap, vi->imod->mousemode);
  numZapWindows++;
  insertTime.start();
  return(0);
}

void zapGotoCurrentPoint(ZapStruct *zap)
{
  return;
}

static void zapAutoTranslate(ZapStruct *zap)
{
  if (zap->lock == 2)
    return;

  zap->section = (int)(zap->vi->zmouse + 0.5f);
     
  zapDrawTools(zap);

  if (zap->lock)
    return;

  return;
}

/* DNM: 2.40 deleted imod_zap_draw which was unused and confusing */


static void zapTranslate(ZapStruct *zap, int x, int y)
{
  ImodView *vw = zap->vi;
  zap->xtrans += x;
  if (zap->xtrans > vw->xsize)
    zap->xtrans = vw->xsize;
  if (zap->xtrans < -vw->xsize)
    zap->xtrans = - vw->xsize;
  zap->ytrans += y;
  if (zap->ytrans > vw->ysize)
    zap->ytrans = vw->ysize;
  if (zap->ytrans < -vw->ysize)
    zap->ytrans = - vw->ysize;
  zapDraw(zap);
  return;
}

static void zapKey_cb(ImodView *vi, void *client, int released, QKeyEvent *e)
{
  ZapStruct *zap = (ZapStruct *)client;
  if ((e->state() & Qt::Keypad) && (e->key() == Qt::Key_Insert ||
                                    e->key() == Qt::Key_0))
    return;
  if (released)
    zapKeyRelease(zap, e);
  else
    zapKeyInput(zap, e);
}


/* static QTime downtime; */

void zapKeyInput(ZapStruct *zap, QKeyEvent *event)
{
  struct ViewInfo *vi = zap->vi;
  int keysym = event->key();
  static int trans = 5;
  int *limits;
  int limarr[4];
  int rx, ix, iy, i;
  int keypad = event->state() & Qt::Keypad;
  int shifted = event->state() & Qt::ShiftButton;
  int handled = 0;
  Iindex indadd;
  Iindex *indp;
  Ipoint pmin, pmax, selmin, selmax;
  Iobj *obj;
  /* downtime.start(); */

  /*if (Imod_debug)
    imodPrintStderr("key %x, state %x\n", keysym, event->state()); */
  if (inputTestMetaKey(event))
    return;

  inputConvertNumLock(keysym, keypad);

  setControlAndLimits(zap);
  ivwControlActive(vi, 0);

  if (imodPlugHandleKey(vi, event)) 
    return;
  ivwControlActive(vi, 1);

  /* DNM: set global insertmode from this zap's mode to get it to work
     right with Delete key */
  vi->insertmode = zap->insertmode;

  /*      imodPrintStderr("Zapo got %x keysym\n", keysym); */


  switch(keysym){

  case Qt::Key_Up:
  case Qt::Key_Down: 
  case Qt::Key_Right: 
  case Qt::Key_Left: 
    // Translate with keypad in movie mode or regular arrows in model mode
    if (!keypad && vi->imod->mousemode != IMOD_MMOVIE ||
        keypad && vi->imod->mousemode == IMOD_MMOVIE) {
      if (keysym == Qt::Key_Left)
        zapTranslate(zap, -trans, 0);
      if (keysym == Qt::Key_Right)
        zapTranslate(zap, trans, 0);
      if (keysym == Qt::Key_Down)
        zapTranslate(zap, 0, -trans);
      if (keysym == Qt::Key_Up)
        zapTranslate(zap, 0, trans);
      handled = 1;

      // Move point with keypad in model mode
    } else if (keypad && vi->imod->mousemode != IMOD_MMOVIE) {
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

  case Qt::Key_Prior:
  case Qt::Key_Next:
    // With keypad, translate in movie mode or move point in model mode
    if (keypad) {
      if (keysym == Qt::Key_Next) {
        if (vi->imod->mousemode == IMOD_MMOVIE)
          zapTranslate(zap, trans, -trans);
        else
          inputPointMove(vi, 0, 0, -1);
      } else {
        if (vi->imod->mousemode == IMOD_MMOVIE)
          zapTranslate(zap, trans, trans);
        else
          inputPointMove(vi, 0, 0, 1);
      }
      handled = 1;

      // with regular keys, handle specially if locked
    } else if (!keypad && zap->lock == 2){
      if (keysym == Qt::Key_Next)
        zap->section--;
      else
        zap->section++;
      if (zap->section < 0) zap->section = 0;
      if (zap->section >= vi->zsize) 
        zap->section = vi->zsize -1;
      zapDraw(zap);
      handled = 1;
    }
    break;
          
  case Qt::Key_1:
  case Qt::Key_2:
    if (zap->timeLock) {
      zapStepTime(zap, (keysym == Qt::Key_1) ? -1 : 1);
      handled = 1;
    }
    break;

  case Qt::Key_Home:
    if (keypad && vi->imod->mousemode == IMOD_MMOVIE) {
      zapTranslate(zap, -trans, trans);
      handled = 1;
    }
    break;

  case Qt::Key_End:
    if (keypad && vi->imod->mousemode == IMOD_MMOVIE) {
      zapTranslate(zap, -trans, -trans);
      handled = 1;
    }
    break;
          
  case Qt::Key_Minus:
    zap->zoom = b3dStepPixelZoom(zap->zoom, -1);
    zapDraw(zap);
    handled = 1;
    break;

  case Qt::Key_Equal:
    zap->zoom = b3dStepPixelZoom(zap->zoom, 1);
    zapDraw(zap);
    handled = 1;
    break;
          
    /* DNM: Keypad Insert key, alternative to middle mouse button */
  case Qt::Key_Insert:

    /* But skip out if in movie mode or already active */
    if (!keypad || vi->imod->mousemode == IMOD_MMOVIE || insertDown)
      break;

    // It wouldn't work going to a QPoint and accessing it, so do it in shot!
    ix = (zap->gfx->mapFromGlobal(QCursor::pos())).x();
    iy = (zap->gfx->mapFromGlobal(QCursor::pos())).y();

    // Set a flag, set continuous tracking, grab keyboard and mouse
    insertDown = 1;
    zap->gfx->setMouseTracking(true);
    zap->qtWindow->grabKeyboard();
    zap->gfx->grabMouse();

    /* Use time since last event to determine whether to treat like
       single click or drag */
    rx = insertTime.elapsed();
    insertTime.restart();
    /* imodPrintStderr(" %d %d %d\n ", rx, ix, iy); */
    if(rx > 250)
      zapButton2(zap, ix, iy, 0);
    else
      zapB2Drag(zap, ix, iy, 0); 

    zap->lmx = ix;
    zap->lmy = iy;
    handled = 1;
    break;

    /* DNM 12/13/01: add next and smooth hotkeys to autox */
  case Qt::Key_A:
    if (event->state() & Qt::ControlButton) {

      // Select all contours in current object on section or in rubberband
      if (zap->rubberband) {
        selmin.x = zap->rbImageX0;
        selmax.x = zap->rbImageX1;
        selmin.y = zap->rbImageY0;
        selmax.y = zap->rbImageY1;
      } else {
        selmin.x = -vi->xsize;;
        selmax.x = 2 * vi->xsize;
        selmin.y = vi->ysize;;
        selmax.y = 2 * vi->ysize;
      }
      selmin.z = zap->section - 0.5;
      selmax.z = zap->section + 0.5;

      // Look through selection list, remove any that do not fit constraints
      for (i = ilistSize(vi->selectionList) - 1; i >= 0; i--) {
        indp = (Iindex *)ilistItem(vi->selectionList, i);
        if (indp->object < vi->imod->objsize) {
          obj = &vi->imod->obj[indp->object];
          if (indp->contour < obj->contsize) {
            if (contInSelectArea(obj, &(obj->cont[indp->contour]), selmin,
                                 selmax))
              continue;
          }
        }
        ilistRemove(vi->selectionList, i);
      }

      obj = imodObjectGet(vi->imod);
      if (!obj)
        break;

      // Set up an index to add, look for contours inside
      // the bounding box, add them, make last one be current
      indadd.object = vi->imod->cindex.object;
      indadd.point = -1;
      for (i = 0; i < obj->contsize; i++) {
        indadd.contour = i;
        if (contInSelectArea(obj, &(obj->cont[i]), selmin, selmax)) {
          imodSelectionListAdd(vi, indadd);
          vi->imod->cindex = indadd;
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
    autox_smooth(vi->ax);
    handled = 1;
    break;

  case Qt::Key_B:
    if (shifted) { 
      zapToggleRubberband(zap);
    } else
      autox_build(vi->ax);
    handled = 1;
    break;
          
  case Qt::Key_P:
    if (shifted) {
      if (zap->shiftingCont)
        endContourShift(zap);
      else
        setupContourShift(zap);
      handled = 1;
    }
    break;

  case Qt::Key_S:
    if (shifted || 
        (event->state() & Qt::ControlButton)){

      // Take a montage snapshot if selected and no rubberband is on
      if (!shifted && !zap->rubberband && imcGetMontageFactor() > 1) {
        montageSnapshot(zap);
        handled = 1;
        break;
      }
      zapDraw(zap);
      limits = NULL;
      if (zap->rubberband) {
        limits = limarr;
        zapBandImageToMouse(zap, 1);
        limarr[0] = zap->rbMouseX0 + 1;
        limarr[1] = zap->winy - zap->rbMouseY1;
        limarr[2] = zap->rbMouseX1 - 1 - zap->rbMouseX0;
        limarr[3] = zap->rbMouseY1 - 1 - zap->rbMouseY0;
      }
      if (shifted)
        b3dAutoSnapshot("zap", SnapShot_RGB, limits);
      else
        b3dAutoSnapshot("zap", SnapShot_TIF, limits);
    }else
      inputSaveModel(vi);
    handled = 1;
    break;
          
  case Qt::Key_Escape:
    zap->qtWindow->close();
    handled = 1;
    break;

  case Qt::Key_R:
    if (shifted) {
      zapResizeToFit(zap);
      handled = 1;
    }
    break;

  case Qt::Key_Z:
    if (shifted) { 
      if(zap->sectionStep) {
        zap->sectionStep = 0;
        wprint("Auto-section advance turned OFF\n");
      } else {
        zap->sectionStep = 1;
        wprint("\aAuto-section advance turned ON\n");
      }
      handled = 1;
    }
    break;

  case Qt::Key_I:
    if (shifted)
      zapPrintInfo(zap);
    else {
      zapStateToggled(zap, ZAP_TOGGLE_INSERT, 1 - zap->insertmode);
      zap->qtWindow->setToggleState(ZAP_TOGGLE_INSERT, zap->insertmode);
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
      XtVaGetValues(zap->dialog,
      XmNwidth, &width,
      XmNheight, &height,
      XmNx, &dx, XmNy, &dy,
      NULL);
      if (event->state() & ShiftButton)
      delta = -1;
      if (keysym == Qt::Key_X)
      width += delta;
      else
      height += delta;
      imodPrintStderr ("%d x %d\n", width, height);
      XtConfigureWidget(zap->dialog, dx, dy, width, height, 0);
      }
    */

    /*
      case Qt::Key_X:
    wprint("Clipboard = %s\n", QApplication::clipboard()->text().latin1());
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

void zapKeyRelease(ZapStruct *zap, QKeyEvent *event)
{
  /*  imodPrintStderr ("%d down\n", downtime.elapsed()); */
  if (!insertDown || !(event->state() & Qt::Keypad) ||
      (event->key() != Qt::Key_Insert && event->key() != Qt::Key_0))
    return;
  insertDown = 0;
  registerDragAdditions(zap);
  zap->gfx->setMouseTracking(zap->rubberband != 0);
  zap->qtWindow->releaseKeyboard();
  zap->gfx->releaseMouse();
  if (zap->drawCurrentOnly) {
    zap->drawCurrentOnly = 0;
    zapDraw(zap);
  }
}

static int firstdrag = 0;
static int moveband = 0;
static int dragband;
static int dragging[4];
static int firstmx, firstmy;

// respond to a mouse press event
void zapMousePress(ZapStruct *zap, QMouseEvent *event)
{
  int button1, button2, button3;
  int ctrlDown = event->state() & Qt::ControlButton;
  setControlAndLimits(zap);

  button1 = event->stateAfter() & ImodPrefs->actualButton(1) ? 1 : 0;
  button2 = event->stateAfter() & ImodPrefs->actualButton(2) ? 1 : 0;
  button3 = event->stateAfter() & ImodPrefs->actualButton(3) ? 1 : 0;

  /* imodPrintStderr("click at %d %d\n", event->x(), event->y()); */

  if (event->button() == ImodPrefs->actualButton(1)) {
    but1downt.start();
    firstmx = event->x();
    firstmy = event->y();
    if (zap->shiftingCont)
      startShiftingContour(zap, firstmx, firstmy, 1, ctrlDown);
    else if (zap->startingBand)
      zapButton1(zap, firstmx, firstmy, ctrlDown);
    else
      firstdrag = 1;
      
  } else if (event->button() == ImodPrefs->actualButton(2) &&
	     !button1 && !button3) {
    if (zap->shiftingCont)
      startShiftingContour(zap, event->x(), event->y(), 2, ctrlDown);
    else
      zapButton2(zap, event->x(), event->y(), ctrlDown);

  } else if (event->button() == ImodPrefs->actualButton(3) &&
	     !button1 && !button2) {
    if (zap->shiftingCont)
      startShiftingContour(zap, event->x(), event->y(), 3, ctrlDown);
    else
      zapButton3(zap, event->x(), event->y(), ctrlDown);
  }
  zap->lmx = event->x();
  zap->lmy = event->y();

}

// respond to mouse release event
void zapMouseRelease(ZapStruct *zap, QMouseEvent *event)
{
  setControlAndLimits(zap);
  if (zap->shiftRegistered) {
    zap->shiftRegistered = 0;
    zap->vi->undo->finishUnit();
  }

  if (event->button() == ImodPrefs->actualButton(1)){
    if (dragband) {
      dragband = 0;
      zapSetCursor(zap, zap->mousemode);
    }
    firstdrag = 0;

    if (but1downt.elapsed() > 250) {
      if (zap->hqgfxsave)
        zapDraw(zap);
      zap->hqgfxsave  = 0;
      return;    //IS THIS RIGHT?
    }
    zapButton1(zap, event->x(), event->y(),
               event->state() & Qt::ControlButton);
  }
 
  // Button 2 and band moving, release te band
  if ((event->button() == ImodPrefs->actualButton(2))
      && zap->rubberband && moveband) {
    moveband = 0;
    zapSetCursor(zap, zap->mousemode);
    if (zap->hqgfxsave)
      zapDraw(zap);
    zap->hqgfxsave  = 0;

    // Button 2 and doing a drag draw - draw for real.
  } else if ((event->button() == ImodPrefs->actualButton(2))) {
    registerDragAdditions(zap);

    if (zap->drawCurrentOnly) {
      zap->drawCurrentOnly = 0;
      zapDraw(zap);
    }
  }
  if (zap->centerMarked && !zap->centerDefined) {
    ivwClearExtraObject(zap->vi);
    imodDraw(zap->vi, IMOD_DRAW_MOD);
    zap->centerMarked = 0;
  }
}

// Respond to a mouse move event (mouse down)
void zapMouseMove(ZapStruct *zap, QMouseEvent *event, bool mousePressed)
{
  int button1, button2, button3;
  int cumdx, cumdy;
  int cumthresh = 6 * 6;
  int ctrlDown = event->state() & Qt::ControlButton;
  int shiftDown = event->state() & Qt::ShiftButton;

  if (!(mousePressed || insertDown)) {
    if (zap->rubberband)
      zapAnalyzeBandEdge(zap, event->x(), event->y());
    return;
  }

  setControlAndLimits(zap);
  
  button1 = event->state() & ImodPrefs->actualButton(1) ? 1 : 0;
  button2 = (event->state() & ImodPrefs->actualButton(2)) 
    || insertDown ? 1 : 0;
  button3 = event->state() & ImodPrefs->actualButton(3) ? 1 : 0;
  /*  imodPrintStderr("mb  %d|%d|%d\n", button1, button2, button3); */

  if ( (button1) && (!button2) && (!button3)){
    if (ctrlDown) {
      dragSelectContsCrossed(zap, event->x(), event->y());
    } else {
      /* DNM: wait for a bit of time or until enough distance moved, but if we
         do not replace original lmx, lmy, there is a disconcerting lurch */
      cumdx = event->x() - firstmx;
      cumdy = event->y() - firstmy;
      if ((but1downt.elapsed()) > 250 || cumdx * cumdx + cumdy * cumdy >
          cumthresh)
        zapB1Drag(zap, event->x(), event->y());
    }
  }

  if ( (!button1) && (button2) && (!button3))
    zapB2Drag(zap, event->x(), event->y(), ctrlDown);
  
  if ( (!button1) && (!button2) && (button3))
    zapB3Drag(zap, event->x(), event->y(), ctrlDown, shiftDown);
  
  zap->lmx = event->x();
  zap->lmy = event->y();
}

/*
 * Analyze for whether mouse is close to a corner or an edge and set flags
 * for mouse to be set properly
 */
static void zapAnalyzeBandEdge(ZapStruct *zap, int ix, int iy)
{
  int rubbercrit = 10;  /* Criterion distance for grabbing the band */
  int i, dminsq, dist, distsq, dmin, dxll, dyll, dxur, dyur;
  int minedgex, minedgey;

  zapBandImageToMouse(zap, 0);    
  dminsq = rubbercrit * rubbercrit;
  dragband = 0;
  minedgex = -1;
  for (i = 0; i < 4; i++)
    dragging[i] = 0;
  dxll = ix - zap->rbMouseX0;
  dxur = ix - zap->rbMouseX1;
  dyll = iy - zap->rbMouseY0;
  dyur = iy - zap->rbMouseY1;

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
  zapSetCursor(zap, zap->mousemode);
}

static int zapBandMinimum(ZapStruct *zap)
{
  // Adjust minimum size down in case image is tiny
  int bandmin = B3DMIN(4, (int)(zap->vi->xsize * zap->xzoom) + 2);
  bandmin = B3DMIN(bandmin, (int)(zap->vi->ysize * zap->zoom) + 2);
  return bandmin;
}

/* Attach to nearest point in model mode, or just modify the current 
   xmouse, ymouse values */

void zapButton1(ZapStruct *zap, int x, int y, int controlDown)
{
  ImodView *vi   = zap->vi;
  Imod     *imod = vi->imod;
  Ipoint pnt, *spnt;
  Iindex index, indSave;
  Iindex *indp;
  int bandmin = zapBandMinimum(zap);
  int i;
  float temp_distance;
  float distance = -1.;
  float ix, iy, dx, dy;
  float selsize = IMOD_SELSIZE / zap->zoom;

  zapGetixy(zap, x, y, &ix, &iy);
     
  // If starting rubber band, set upper left corner, set for moving lower left
  if (zap->startingBand) {
    
    // Set up mouse coords then convert to image
    zap->rbMouseX1 = x + bandmin;
    if (zap->rbMouseX1 > zap->winx - 1)
      zap->rbMouseX1 = zap->winx - 1;
    zap->rbMouseX0 = zap->rbMouseX1 - bandmin;

    zap->rbMouseY0 = y - bandmin;
    if (y < bandmin)
      zap->rbMouseY0 = 0;
    zap->rbMouseY1 = zap->rbMouseY0 + bandmin;
    zapBandMouseToImage(zap, 0);

    // Does image coord need to be moved?  Do so and move mouse
    dx = dy = 0;
    if (zap->rbImageX0 < 0.)
      dx = -zap->rbImageX0;
    if (zap->rbImageX1 > zap->vi->xsize)
      dx = zap->vi->xsize - zap->rbImageX1; 
    if (zap->rbImageY0 < 0.)
      dy = -zap->rbImageY0;
    if (zap->rbImageY1 > zap->vi->ysize)
      dy = zap->vi->ysize - zap->rbImageY1; 
    if (dx || dy) {
      zap->rbImageX0 += dx;
      zap->rbImageX1 += dx;
      zap->rbImageY0 += dy;
      zap->rbImageY1 += dy;
      zapBandImageToMouse(zap, 1);
      QCursor::setPos(zap->gfx->mapToGlobal(QPoint(zap->rbMouseX0, 
                                                   zap->rbMouseY0)));
      firstmx = zap->rbMouseX0;
      firstmy = zap->rbMouseY0;
    }

    zap->startingBand = 0;
    zap->rubberband = 1;
    zap->gfx->setMouseTracking(true);
    dragband = 1;
    dragging[0] = 0;
    dragging[1] = 1;
    dragging[2] = 0;
    dragging[3] = 1;
    zapSetCursor(zap, zap->mousemode);
    zapDraw(zap);
    return;  
  }

  if (vi->ax)
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      autox_fillmouse(vi, (int)ix, (int)iy);
      return;
    }
     
  if (vi->imod->mousemode == IMOD_MMODEL){
    pnt.x = ix;
    pnt.y = iy;
    pnt.z = zap->section;
    vi->xmouse = ix;
    vi->ymouse = iy;
    indSave = vi->imod->cindex;
    imod->cindex.contour = -1;
    imod->cindex.point = -1;

    for (i = 0; i < imod->objsize; i++){
      index.object = i;
      temp_distance = imod_obj_nearest
        (vi, &(imod->obj[i]), &index , &pnt, selsize);
      if (temp_distance < 0.)
        continue;
      if (distance < 0. || distance > temp_distance) {
        distance      = temp_distance;
        imod->cindex.object  = index.object;
        imod->cindex.contour = index.contour;
        imod->cindex.point   = index.point;
        spnt = imodPointGet(vi->imod);
        if (spnt){
          vi->xmouse = spnt->x;
          vi->ymouse = spnt->y;
        }
      }
    }

    if (distance >= 0.) {

      // If ctrl-select, then manage selection list
      if (controlDown) {

        // First add previous point if list is empty
        if (!ilistSize(vi->selectionList) && indSave.contour >= 0)
          imodSelectionListAdd(vi, indSave);

        // If point not on list, add it.  If point is on list, then remove
        // it and pop current point back to last item on list
        if (imodSelectionListQuery(vi, imod->cindex.object, 
                                   imod->cindex.contour) < -1)
          imodSelectionListAdd(vi, imod->cindex);
        else {
          imodSelectionListRemove(vi, imod->cindex.object, 
                                  imod->cindex.contour);
          if (ilistSize(vi->selectionList)) {
            indp = (Iindex *)ilistItem(vi->selectionList,
                                       ilistSize(vi->selectionList) - 1); 
            imod->cindex = *indp;
          }
        }
      } else

        // But if Ctrl not down, clear out the list
        imodSelectionListClear(vi);
    }

    /* DNM: add the DRAW_XYZ flag to make it update info and Slicer */
    imodDraw(vi, IMOD_DRAW_RETHINK | IMOD_DRAW_XYZ);
    return;
  }

  vi->xmouse = ix;
  vi->ymouse = iy;

  imodDraw(vi, IMOD_DRAW_XYZ);
}

/* In model mode, add a model point, creating a new contour if necessary */

void zapButton2(ZapStruct *zap, int x, int y, int controlDown)
{
  ImodView *vi = zap->vi;
  Iobj  *obj;
  Icont *cont;
  Ipoint point;
  int   pt;
  float ix, iy;
  float lastz;
  int rcrit = 10;   /* Criterion for moving the whole band */
  int dxll, dxur,dyll, dyur;
  int cz, pz;

  zapGetixy(zap, x, y, &ix, &iy);

  if (vi->ax){
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      /* DNM 2/1/01: need to call with int */
      autox_sethigh(vi, (int)ix, (int)iy);
      return;
    }
  }
     
  moveband = 0;
  /* If rubber band is on and within criterion distance of any edge, set
     flag to move whole band and return */
  if (zap->rubberband) {
    zapBandImageToMouse(zap, 0);
    dxll = x - zap->rbMouseX0;
    dxur = x - zap->rbMouseX1;
    dyll = y - zap->rbMouseY0;
    dyur = y - zap->rbMouseY1;
    if ((dyll > 0 && dyur < 0 && (dxll < rcrit && dxll > -rcrit ||
                                  dxur < rcrit && dxur > -rcrit)) ||
        (dxll > 0 && dxur < 0 && (dyll < rcrit && dyll > -rcrit ||
                                  dyur < rcrit && dyur > -rcrit))) {
      moveband = 1;
      zapSetCursor(zap, zap->mousemode);
      return;
    }
  }     

  if (vi->imod->mousemode == IMOD_MMODEL){
    zap->dragAddCount = 0;
    obj = imodObjectGet(vi->imod);
    if (!obj)
      return;

    // Get current contour; if there is none, start a new one
    // DNM 7/10/04: switch to calling routine; it now fixes time of empty cont
    cont = ivwGetOrMakeContour(vi, obj, zap->timeLock);
    if (!cont)
      return;
    point.x = ix;
    point.y = iy;
    point.z = zap->section;
    if ((zap->twod)&&(cont)&&(cont->psize)){
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
      if ((iobjClose(obj->flags) && !(cont->flags & ICONT_WILD) && cz != pz) ||
          ivwTimeMismatch(vi, zap->timeLock, obj, cont)) {
        if (cont->psize == 1) {
          wprint("Started a new contour even though last "
                 "contour had only 1 pt.  ");
          if (cz != pz)
            wprint("\aUse open contours to model across sections.\n");
          else
            wprint("\aSet contour time to 0 to model across times.\n");
        }
        
        vi->undo->contourAddition(obj->contsize);
        imodNewContour(vi->imod);
        cont = imodContourGet(vi->imod);
        if (!cont) {
          vi->undo->flushUnit();
          return;
        }
        ivwSetNewContourTime(vi, obj, cont);
      }
    }

    /* Now if times still don't match refuse the point */
    if (ivwTimeMismatch(vi, zap->timeLock, obj, cont)) {
      wprint("\aContour time does not match current time.\n"
             "Set contour time to 0 to model across times.\n");
      vi->undo->finishUnit();
      return;
    }
    
    // DNM 11/17/04: Cleaned up adding point logic to set an insertion point
    // and just call InsertPoint with it
    // Set insertion point to next point and adjust it down if going backwards
    pt = vi->imod->cindex.point + 1;
    if (pt > 0)
      lastz = cont->pts[pt - 1].z;
    else
      lastz = point.z;

    if (pt > 0 && zap->insertmode)
      pt--;
    
    ivwRegisterInsertPoint(vi, cont, &point, pt);

    /* DNM: auto section advance is based on the direction of section change 
       between last and just-inserted points */
    if (zap->sectionStep && point.z != lastz) {
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
      
    return;
  }
  startMovieCheckSnap(zap, 1);
}

/* Delete all points of current contour under the cursor */     
static void zapDelUnderCursor(ZapStruct *zap, int x, int y, Icont *cont)
{
  float ix, iy;
  float crit = 8./ zap->zoom;
  float critsq, dsq;
  int i;
  Ipoint *lpt;
  int deleted = 0;

  zapGetixy(zap, x, y, &ix, &iy);
  critsq = crit * crit;
  for (i = 0; i < cont->psize  && cont->psize > 1; ) {
    lpt = &(cont->pts[i]);
    if (floor(lpt->z + 0.5) == zap->section) {
      dsq = (lpt->x - ix) * (lpt->x - ix) +
        (lpt->y - iy) * (lpt->y - iy);
      if (dsq <= critsq) {
        zap->vi->undo->pointRemoval(i);
        imodPointDelete(cont, i);
        zap->vi->imod->cindex.point = 
          B3DMIN(cont->psize - 1, B3DMAX(i + zap->insertmode - 1, 0));
        deleted = 1;
        continue;
      }
    }
    i++;
  }
  if (!deleted)
    return;
  zap->vi->undo->finishUnit();
  imodDraw(zap->vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD );
}

/* In model mode, modify current point; otherwise run movie */

void zapButton3(ZapStruct *zap, int x, int y, int controlDown)
{
  ImodView *vi = zap->vi;
  Icont *cont;
  Iobj *obj;
  int   pt;
  float ix, iy;

  zapGetixy(zap, x, y, &ix, &iy);

  if (vi->ax){
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      /* DNM 2/1/01: need to call with int */
      autox_setlow(vi, (int)ix, (int)iy);
      return;
    }
  }

  if (vi->imod->mousemode == IMOD_MMODEL){
    cont = imodContourGet(vi->imod);
    pt   = vi->imod->cindex.point;
    if (!cont)
      return;
    if (pt < 0)
      return;

    obj = imodObjectGet(vi->imod);
    if (ivwTimeMismatch(vi, zap->timeLock, obj, cont))
      return;

    /* If the control key is down, delete points under the cursor */
    if (controlDown) {
      zapDelUnderCursor(zap, x, y, cont);
      return; 
    }

          
    if (!zapPointVisable(zap, &(cont->pts[pt])))
      return;

    vi->undo->pointShift();
    cont->pts[pt].x = ix;
    cont->pts[pt].y = iy;
    vi->undo->finishUnit();

    vi->xmouse  = ix;
    vi->ymouse  = iy;

    imodDraw(vi, IMOD_DRAW_RETHINK);
    return;
  }
  startMovieCheckSnap(zap, -1);
}

void zapB1Drag(ZapStruct *zap, int x, int y)
{

  // For zooms less than one, move image along with mouse; for higher zooms,
  // Translate 1 image pixel per mouse pixel (accelerated)
  double transFac = zap->zoom < 1. ? 1. / zap->zoom : 1.;

  if (zap->shiftingCont) {
    shiftContour(zap, x, y, 1, 0);
    return;
  }

  // First time mouse moves, lock in the band drag position
  if (zap->rubberband && firstdrag)
    zapAnalyzeBandEdge(zap, x, y);
  firstdrag = 0;
     
  if (zap->rubberband && dragband) {

    /* Move the rubber band */
    if (dragging[0]) {
      zap->rbImageX0 += (x - zap->lmx) / zap->xzoom;
      if (zap->rbImageX0 < 0)
        zap->rbImageX0 = 0;
      if (zap->rbImageX0 >= zap->rbImageX1)
        zap->rbImageX0 = zap->rbImageX1 - 1;
    }
    if (dragging[1]) {
      zap->rbImageX1 += (x - zap->lmx) / zap->xzoom;
      if (zap->rbImageX1 > zap->vi->xsize)
        zap->rbImageX1 = zap->vi->xsize;
      if (zap->rbImageX1 <= zap->rbImageX0)
        zap->rbImageX1 = zap->rbImageX0 + 1;
    }
    if (dragging[3]) {
      zap->rbImageY0 += (zap->lmy - y) / zap->xzoom;
      if (zap->rbImageY0 < 0)
        zap->rbImageY0 = 0;
      if (zap->rbImageY0 >= zap->rbImageY1)
        zap->rbImageY0 = zap->rbImageY1 - 1;
    }
    if (dragging[2]) {
      zap->rbImageY1 += (zap->lmy - y) / zap->xzoom;
      if (zap->rbImageY1 > zap->vi->ysize)
        zap->rbImageY1 = zap->vi->ysize;
      if (zap->rbImageY1 <= zap->rbImageY0)
        zap->rbImageY1 = zap->rbImageY0 + 1;
    }


  } else {
    /* Move the image */
    zap->xtrans += (int)floor(transFac * (x - zap->lmx) + 0.5);
    zap->ytrans -= (int)floor(transFac * (y - zap->lmy) + 0.5);
  }

  zap->hqgfxsave = zap->hqgfx;
  zap->hqgfx = 0;
  zapDraw(zap);
  zap->hqgfx = zap->hqgfxsave;
}

/* Tests whether the contour is in the selection area.  Either it must be 
   entirely within the area, or it must be an open wild contour and have points
   on the current section that are all within the area */
static int contInSelectArea(Iobj *obj, Icont *cont, Ipoint selmin, 
                            Ipoint selmax)
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


/* Select contours in the current object crossed by a mouse move */
static void dragSelectContsCrossed(struct zapwin *zap, int x, int y)
{
  ImodView *vi = zap->vi;
  Imod *imod = vi->imod;
  int ob, co, pt, ptStart, lastPt, thisZ, lastZ;
  Iindex index;
  Icont *cont;
  Ipoint pnt1, pnt2;
  Iobj *obj = imodObjectGet(imod);

  // Skip for movie mode
  if (!obj || imod->mousemode == IMOD_MMOVIE)
    return;

  // Get image positions of starting and current mouse positions
  ob = imod->cindex.object;
  zapGetixy(zap, x, y, &pnt2.x, &pnt2.y);
  zapGetixy(zap, zap->lmx, zap->lmy, &pnt1.x, &pnt1.y);
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
      if (!(cont->flags & ICONT_WILD) && 
          (int)floor(cont->pts->z + 0.5) != zap->section)
        continue;

      // Set up to loop on second point in segment, starting at first point
      // in contour for closed contour
      ptStart = iobjOpen(obj->flags) || (cont->flags & ICONT_OPEN) ? 1 : 0;
      lastZ = zap->section;
      if (imodDebug('z'))
        imodPrintStderr("Examining contour %d\n", co);

      // Loop on points, look for segments on the section
      for (pt = ptStart; pt < cont->psize; pt++) {
        lastPt = pt ? pt - 1 : cont->psize - 1;
        thisZ = (int)floor(cont->pts[pt].z + 0.5);
        if (lastZ == zap->section && thisZ == zap->section) {
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
            imodDraw(zap->vi, IMOD_DRAW_RETHINK | IMOD_DRAW_XYZ);
            break;
          }
        }
        lastZ = thisZ;
      }
    }
  }
}

void zapB2Drag(ZapStruct *zap, int x, int y, int controlDown)
{
  ImodView *vi = zap->vi;
  Iobj *obj;
  Icont *cont;
  Ipoint *lpt, cpt;
  float ix, iy, idx, idy;
  double dist;
  int pt;
     
  if (zap->shiftingCont) {
    shiftContour(zap, x, y, 2, 0);
    return;
  }

  if (vi->ax){
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      zapGetixy(zap, x, y, &ix, &iy);
      /* DNM 2/1/01: need to call with int */
      autox_sethigh(vi, (int)ix, (int)iy);
      return;
    }
  }

  if (zap->rubberband && moveband) {
    /* Moving rubber band: get desired move and constrain it to keep
       band in the image */
    idx = (x - zap->lmx) / zap->xzoom;
    idy = (zap->lmy - y) / zap->zoom;
    if (zap->rbImageX0 + idx < 0)
      idx = -zap->rbImageX0;
    if (zap->rbImageX1 + idx > zap->vi->xsize)
      idx = zap->vi->xsize - zap->rbImageX1;
    if (zap->rbImageY0 + idy < 0)
      idy = -zap->rbImageY0;
    if (zap->rbImageY1 + idy > zap->vi->ysize)
      idy = zap->vi->ysize - zap->rbImageY1;
    zap->rbImageX0 += idx;
    zap->rbImageX1 += idx;
    zap->rbImageY0 += idy;
    zap->rbImageY1 += idy;

    zap->hqgfxsave = zap->hqgfx;
    zap->hqgfx = 0;
    zapDraw(zap);
    zap->hqgfx = zap->hqgfxsave;
    return;
  }

  if (vi->imod->mousemode == IMOD_MMOVIE)
    return;

  if (vi->imod->cindex.point < 0)
    return;

  zapGetixy(zap, x, y, &ix, &iy);

  cpt.x = ix;
  cpt.y = iy;
  cpt.z = zap->section;
     
  obj = imodObjectGet(vi->imod);
  if (!obj)
    return;

  cont = imodContourGet(vi->imod);
  if (!cont)
    return;

  lpt = &(cont->pts[vi->imod->cindex.point]);
  if (zap->twod)
    cpt.z = lpt->z;

  dist = imodel_point_dist( lpt, &cpt);

  /* DNM 6/18/03: If Z or time has changed, treat it like a button click so
     new contour can be started */
  // DNM 6/30/04: change to start new for any kind of contour with time change
  if ((iobjClose(obj->flags) && !(cont->flags & ICONT_WILD) && 
      (int)floor(lpt->z + 0.5) != (int)cpt.z) ||
       ivwTimeMismatch(vi, zap->timeLock, obj, cont)) {
    registerDragAdditions(zap);
    zapButton2(zap, x, y, 0);
    return;
  }

  if (ivwTimeMismatch(vi, zap->timeLock, obj, cont))
    return;

  if ( dist > scaleModelRes(vi->imod->res, zap->zoom)){

    // Set insertion index to next point, or to current if drawing backwards
    pt = vi->imod->cindex.point + 1;
    if (pt > 0 && zap->insertmode)
      pt--;

    // Set flag for drawing current contour only if at end and going forward
    if (!zap->insertmode && pt == cont->psize)
      zap->drawCurrentOnly = 1;

    // Register previous additions if the count is up or if the object or
    // contour has changed
    if (zap->dragAddCount >= dragRegisterSize || 
        zap->dragAddIndex.object != vi->imod->cindex.object ||
        zap->dragAddIndex.contour != vi->imod->cindex.contour)
      registerDragAdditions(zap);

    // Start keeping track of delayed registrations by opening a unit and
    // saving the indices.  If general store exists, start with whole data
    // change to save the store.
    // Otherwise if going backwards, need to increment registered first point
    if (!zap->dragAddCount) {
      if (ilistSize(cont->store))
        vi->undo->contourDataChg();
      else
        vi->undo->getOpenUnit();
      zap->dragAddIndex = vi->imod->cindex;
      zap->dragAddIndex.point = pt;
    } else if (zap->insertmode)
      zap->dragAddIndex.point++;
    
    // Always save last point not registered and increment count
    zap->dragAddEnd = pt;
    zap->dragAddCount++;

    imodInsertPoint(vi->imod, &cpt, pt);
    imodDraw(vi, IMOD_DRAW_MOD | IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC);
  }
}

void zapB3Drag(ZapStruct *zap, int x, int y, int controlDown, int shiftDown)
{
  ImodView *vi = zap->vi;
  Iobj *obj;
  Icont *cont;
  Ipoint *lpt;
  Ipoint pt;
  float ix, iy;

  if (zap->shiftingCont) {
    shiftContour(zap, x, y, 3, shiftDown);
    return;
  }

  if (vi->ax){
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      zapGetixy(zap, x, y, &ix, &iy);
      /* DNM 2/1/01: need to call with int */
      autox_setlow(vi, (int)ix, (int)iy);
      return;
    }
  }

  // if (!(maskr & Button3Mask))
  //  return;

  if (vi->imod->mousemode == IMOD_MMOVIE)
    return;
     
  if (vi->imod->cindex.point < 0)
    return;

  cont = imodContourGet(vi->imod);
  if (!cont)
    return;

  /* DNM 11/13/02: do not allow operation on scattered points */
  obj = imodObjectGet(vi->imod);
  if (iobjScat(obj->flags))
    return;

  if (ivwTimeMismatch(vi, zap->timeLock, obj, cont))
    return;

  if (controlDown) {
    zapDelUnderCursor(zap, x, y, cont);
    return;
  }

  if (vi->imod->cindex.point == (cont->psize - 1))
    return;

  /* DNM 11/13/02: need to test for both next and current points to prevent
     strange moves between sections */
  if (!zapPointVisable(zap, &(cont->pts[vi->imod->cindex.point + 1])) ||
      !zapPointVisable(zap, &(cont->pts[vi->imod->cindex.point])))
    return;

  lpt = &(cont->pts[vi->imod->cindex.point]);
  zapGetixy(zap, x, y, &(pt.x), &(pt.y));
  pt.z = lpt->z;
  if (imodel_point_dist(lpt, &pt) > scaleModelRes(vi->imod->res, zap->zoom)){
    ++vi->imod->cindex.point;
    vi->undo->pointShift();
    lpt = &(cont->pts[vi->imod->cindex.point]);
    lpt->x = pt.x;
    lpt->y = pt.y;
    lpt->z = pt.z;
    vi->undo->finishUnit();
    imodDraw(vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD );
  }
  return;
}

// Register accumulated additions for undo
static void registerDragAdditions(ZapStruct *zap)
{
  Iindex *index = &zap->vi->imod->cindex;
  if (!zap->dragAddCount)
    return;
  zap->dragAddCount = 0;
 
  // If obj/cont don't match, forget it
 if (zap->dragAddIndex.object != index->object ||
      zap->dragAddIndex.contour != index->contour) {
    zap->vi->undo->flushUnit();
    return;
  }

 // Send out the additions
  zap->vi->undo->pointAddition(B3DMIN(zap->dragAddIndex.point, index->point),
                               B3DMAX(zap->dragAddIndex.point, index->point));
  zap->vi->undo->finishUnit();
}

/*
 * CONTOUR SHIFTING/TRANSFORMING
 */

// Turn off contour shifting and reset mouse
static void endContourShift(ZapStruct *zap)
{
  if (!zap->shiftingCont)
    return;
  zap->shiftingCont = 0;
  if (zap->centerMarked) {
    ivwClearExtraObject(zap->vi);
    imodDraw(zap->vi, IMOD_DRAW_MOD);
  }
  zapSetCursor(zap, zap->mousemode);
}

// Check whether contour shifting is OK and return current contour
static Icont *checkContourShift(ZapStruct *zap, int &pt, int &err)
{
  ImodView *vi = zap->vi;
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
    endContourShift(zap);
  return cont;
}

// Initiate contour shifting                             
static void setupContourShift(ZapStruct *zap)
{
  int   pt, err;
  checkContourShift(zap, pt, err);
  if (err < 0)
    wprint("\aYou cannot shift scattered point or non-planar open contours."
           "To shift this contour, temporarily make the object type be "
           "closed.\n");
  if (err)
    return;
  zap->shiftingCont = 1;
  if (zap->startingBand)
    zapToggleRubberband(zap);
  zap->centerDefined = 0;
  zap->centerMarked = 0;
  zapSetCursor(zap, zap->mousemode);
}

// Keep track of the base position for contour shifting
static Ipoint contShiftBase;

// Start the actual shift once the mouse goes down
static void startShiftingContour(ZapStruct *zap, int x, int y, int button,
                                 int ctrlDown)
{
  int   pt, err, co, ob, curco, curob;
  float ix, iy, area, areaSum;
  Ipoint cent, centSum;
  Imod *imod = zap->vi->imod;
  Iobj *obj;
  Icont *cont = checkContourShift(zap, pt, err);
  if (err)
    return;

  zapGetixy(zap, x, y, &ix, &iy);

  // If button for marking center, save coordinates, set flag, show mark
  if (button == 2 && ctrlDown) {
    zap->xformCenter.x = ix;
    zap->xformCenter.y = iy;
    zap->centerDefined = 1;
    markXformCenter(zap, ix, iy);
    return;
  }

  if (button == 1) {
    
    // Get base for shift as current point minus mouse position
    contShiftBase.x = cont->pts[pt].x - ix;
    contShiftBase.y = cont->pts[pt].y - iy;
  } else {

    // Use defined center if one was set
    if (zap->centerDefined) {
      contShiftBase.x = zap->xformCenter.x;
      contShiftBase.y = zap->xformCenter.y;
    } else {

      // Otherwise get center for transforms as center of mass
      // Loop on contours and analyze current or selected ones
      imodGetIndex(imod, &curob, &curco, &pt);
      contShiftBase.x = contShiftBase.y = 0;
      centSum.x = centSum.y = 0;
      areaSum = 0;
      for (ob = 0; ob < imod->objsize; ob++) {
        obj = &imod->obj[ob];
        if (iobjScat(obj->flags))
          continue;
        for (co = 0; co < obj->contsize; co++) {
          
          if ((ob == curob && co == curco) || 
              imodSelectionListQuery(zap->vi, ob, co) > -2) {
            cont = &obj->cont[co];
            if (!iobjClose(obj->flags) && (cont->flags & ICONT_WILD))
              continue;

            // For each contour add centroid to straight sum, 
            // accumulate area-weighted sum also
            imodContourCenterOfMass(cont, &cent);
            area = imodContourArea(cont);
            areaSum += area;
            contShiftBase.x += cent.x;
            contShiftBase.y += cent.y;
            centSum.x += cent.x * area;
            centSum.y += cent.y * area;
            err++;
          }
        }
      }

      if (!err) {
        endContourShift(zap);
        return;
      }

      // Use plain sum if area small or if only one contour, otherwise
      // use an area-weighted sum
      if (areaSum < 1. || err == 1) {
        contShiftBase.x /= err;
        contShiftBase.y /= err;
      } else if (areaSum >= 1. && err > 1) {
        contShiftBase.x = centSum.x / areaSum;
        contShiftBase.y = centSum.y / areaSum;
      }
    }
    markXformCenter(zap, contShiftBase.x, contShiftBase.y);
  }
}

// Shift or transform contour upon mouse move
static void shiftContour(ZapStruct *zap, int x, int y, int button, 
                         int shiftDown)
{
  int   pt, err, ob, co, curco, curob;
  float ix, iy;
  float mat[2][2];
  Imod *imod = zap->vi->imod;
  Iobj *obj;
  Icont *cont = checkContourShift(zap, pt, err);
  if (err)
    return;

  if (button == 1) {
    
    // Shift by change from original mouse pos minus change in current 
    // point position
    zapGetixy(zap, x, y, &ix, &iy);
    ix += contShiftBase.x - cont->pts[pt].x;
    iy += contShiftBase.y - cont->pts[pt].y;
  } else {

    // Get transformation matrix if 2nd or 3rd button
    err = button + (button == 3 && shiftDown ? 1 : 0);
    if (mouseXformMatrix(zap, x, y, err, mat))
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
          imodSelectionListQuery(zap->vi, ob, co) > -2) {

        cont = &obj->cont[co];
        if (!iobjClose(obj->flags) && (cont->flags & ICONT_WILD))
          continue;
        
        // Register changes first time only
        if (!zap->shiftRegistered)
          zap->vi->undo->contourDataChg(ob, co);
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

  zap->shiftRegistered = 1;
  imodDraw(zap->vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD );
}

// Compute transform matrix from the mouse move
static int mouseXformMatrix(ZapStruct *zap, int x, int y, int type, 
                            float mat[2][2])
{
  float strThresh = 0.001;
  float rotThresh = 0.05;
  float delCrit = 20.;
  double delx, dely, startang, endang, radst, radnd, delrad, drot, scale;
  double costh, sinth, cosphi, sinphi, cosphisq, sinphisq, f1, f2, f3;
  int xcen, ycen;

  xcen = zapXpos(zap, contShiftBase.x);
  ycen = zapYpos(zap, contShiftBase.y);

  // Compute starting/ending  angle and radii as in midas
  delx = zap->lmx - xcen;
  dely = zap->winy - 1 - zap->lmy - ycen;
  if(delx > -delCrit && delx < delCrit && dely > -delCrit && dely < delCrit)
    return 1;
  radst = sqrt((delx*delx + dely*dely));
  startang = atan2(dely, delx) / RADIANS_PER_DEGREE;
  //imodPrintStderr("%d %d %f %f %f %f\n", xcen, ycen, delx, dely, radst, 
  //                startang);

  delx = x - xcen;
  dely = zap->winy - 1 - y - ycen;
  if(delx > -delCrit && delx < delCrit && dely > -delCrit && dely < delCrit)
    return 1;
  radnd = sqrt((delx*delx + dely*dely));
  endang = atan2(dely, delx) / RADIANS_PER_DEGREE;
  //imodPrintStderr("%f %f %f %f\n", delx, dely, radnd, endang);

  drot = 0.;
  scale = 1.;
  delrad = 1.;
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

  // Compute matrix as in rotmagstr_to_amat
  costh = cos(drot * RADIANS_PER_DEGREE);
  sinth = sin(drot * RADIANS_PER_DEGREE);
  cosphi = cos(endang * RADIANS_PER_DEGREE);
  sinphi = sin(endang * RADIANS_PER_DEGREE);
  cosphisq = cosphi * cosphi;
  sinphisq = sinphi * sinphi;
  f1 = scale * (delrad * cosphisq + sinphisq);
  f2 = scale * (delrad - 1.) * cosphi * sinphi;
  f3 = scale * (delrad * sinphisq + cosphisq);
  mat[0][0] = (float)(f1 * costh - f2 * sinth);
  mat[0][1] = (float)(f2 * costh - f3 * sinth);
  mat[1][0] = (float)(f1 * sinth + f2 * costh);
  mat[1][1] = (float)(f2 * sinth + f3 * costh);
  //imodPrintStderr("%f %f %f %f %f %f %f %f\n", drot, scale, delrad, endang,
  //              mat[0][0], mat[0][1], mat[1][0], mat[1][1]);
  return 0;
}

// Mark the center of transformation with a star in extra object
static void markXformCenter(ZapStruct *zap, float ix, float iy)
{
  int i;
  float starEnd[6] = {0., 8., 7., 4., 7., -4.};
  Iobj *obj = zap->vi->extraObj;
  Icont *cont;
  Ipoint tpt;

  // Clear out the object and set to yellow non scattered
  ivwClearExtraObject(zap->vi);
  imodObjectSetColor(obj, 1., 1., 0.);
  obj->flags &= ~IMOD_OBJFLAG_SCAT;
  obj->pdrawsize = 0;
  obj->linewidth = 1;
  tpt.z = zap->section;

  // Add three contours for lines
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
  }
  zap->centerMarked = 1;
  imodDraw(zap->vi, IMOD_DRAW_MOD);
}


/********************************************************
 * conversion functions between image and window cords. */

/* DNM 9/15/03: use the possibly slightly different x zoom.  This might make
 a few tenths of a pixel difference */

/* return x pos in window for given image x cord. */
static int zapXpos(ZapStruct *zap, float x)
{
  return( (int)(((x - zap->xstart) * zap->xzoom) 
                + zap->xborder));
}

/* return y pos in window for given image y cord. */
static int zapYpos(ZapStruct *zap, float y)
{
  return((int)(((y - zap->ystart) * zap->zoom)
               + zap->yborder));
}

/* returns image cords in x,y, given mouse coords mx, my */
static void zapGetixy(ZapStruct *zap, int mx, int my, float *x, float *y)
{

  // 10/31/04: winy - 1 maps to 0, not winy, so need a -1 here
  my = zap->winy - 1 - my;
  *x = ((float)(mx - zap->xborder) / zap->xzoom)
    + (float)zap->xstart;
  *y = ((float)(my - zap->yborder) / zap->zoom)
    + (float)zap->ystart;
  return;
}

// Convert image rubberband coordinates to outer window coordinates, with
// optional clipping to window limits
static void zapBandImageToMouse(ZapStruct *zap, int ifclip)
{
  zap->rbMouseX0 = zapXpos(zap, zap->rbImageX0) - 1;
  zap->rbMouseX1 = zapXpos(zap, zap->rbImageX1);
  zap->rbMouseY0 = zap->winy - 1 - zapYpos(zap, zap->rbImageY1);
  zap->rbMouseY1 = zap->winy - zapYpos(zap, zap->rbImageY0);
  if (ifclip) {
    if (zap->rbMouseX0 < 0)
      zap->rbMouseX0 = 0;
    if (zap->rbMouseX1 >= zap->winx)
      zap->rbMouseX1 = zap->winx - 1;
    if (zap->rbMouseY0 < 0)
      zap->rbMouseY0 = 0;
    if (zap->rbMouseY1 >= zap->winy)
      zap->rbMouseY1 = zap->winy - 1;
  }
  /* imodPrintStderr("Image %.1f,%.1f : %.1f,%.1f  to mouse %d,%d : %d,%d\n", 
                  zap->rbImageX0, zap->rbImageY0, zap->rbImageX1, zap->rbImageY1,
                  zap->rbMouseX0, zap->rbMouseY0, zap->rbMouseX1, zap->rbMouseY1); */
}

// Convert rubberband window coordinates to inside image coordinates, with
// optional clipping to image limits
static void zapBandMouseToImage(ZapStruct *zap, int ifclip)
{
  zapGetixy(zap, zap->rbMouseX0 + 1, zap->rbMouseY1 - 1, &zap->rbImageX0, 
            &zap->rbImageY0);
  zapGetixy(zap, zap->rbMouseX1, zap->rbMouseY0, &zap->rbImageX1, 
            &zap->rbImageY1);
  
  if (ifclip) {
    if (zap->rbImageX0 < 0)
      zap->rbImageX0 = 0;
    if (zap->rbImageX1 > zap->vi->xsize)
      zap->rbImageX1 = zap->vi->xsize;
    if (zap->rbImageY0 < 0)
      zap->rbImageY0 = 0;
    if (zap->rbImageY1 > zap->vi->ysize)
      zap->rbImageY1 = zap->vi->ysize;
  }
  /* imodPrintStderr("Mouse %d,%d : %d,%d  to image %.1f,%.1f : %.1f,%.1f\n", 
                  zap->rbMouseX0, zap->rbMouseY0, zap->rbMouseX1, zap->rbMouseY1,
                  zap->rbImageX0, zap->rbImageY0, zap->rbImageX1, zap->rbImageY1); */
}


/* Prints window size and image coordinates in Info Window */
void zapPrintInfo(ZapStruct *zap)
{
  float xl, xr, yb, yt;
  int ixl, ixr, iyb, iyt;
  int ixcen, iycen, ixofs, iyofs, imx, imy;
  int bin = zap->vi->xybin;

  ivwControlPriority(zap->vi, zap->ctrl);
  ImodInfoWin->raise();
  if (zap->rubberband) {
    xl = zap->rbImageX0;
    yb = zap->rbImageY0;
    xr = zap->rbImageX1;
    yt = zap->rbImageY1;
  } else {
    zapGetixy(zap, 0, 0, &xl, &yt);
    zapGetixy(zap, zap->winx, zap->winy, &xr, &yb);
  }
  ixl = (int)floor(xl + 0.5);
  ixr = (int)floor(xr - 0.5);
  iyb = (int)floor(yb + 0.5);
  iyt = (int)floor(yt - 0.5);
  imx = ixr + 1 - ixl;
  imy = iyt + 1 - iyb;
  ixl *= bin;
  iyb *= bin;
  ixr = ixr * bin + bin - 1;
  iyt = iyt * bin + bin - 1;
  if (bin > 1)
    wprint("Unbinned: ");
  wprint("(%d,%d) to (%d,%d); ", ixl + 1, iyb + 1, ixr + 1, iyt + 1);
  ixcen = (ixr + 1 + ixl)/2;
  iycen = (iyt + 1 + iyb)/2;
  ixofs = ixcen - bin * zap->vi->xsize/2;
  iyofs = iycen - bin * zap->vi->ysize/2;
  wprint("Center (%d,%d)\n", ixcen + 1, iycen + 1);
  wprint("To excise: newstack -si %d,%d -of %d,%d\n", ixr + 1 - ixl, 
         iyt + 1 - iyb, ixofs, iyofs);
  if (zap->rubberband) 
    wprint("Rubberband: %d x %d; ", zap->rbMouseX1 - 1 - zap->rbMouseX0, 
           zap->rbMouseY1 - 1 - zap->rbMouseY0);
  else
    wprint("Window: %d x %d;   ", zap->winx, zap->winy);
  wprint("Image: %d x %d\n", imx, imy);
}

// Some routines for controlling window size and keeping the window on the
// screen.  The BORDERS are the total borders outside the window 
// excluding frame.  The TITLE_SPACE is the amount to allow for title bar
#define X_BORDERS 24
#define Y_BORDERS 60
#define TITLE_SPACE 28

// Get the maximum window size = desktop minus borders
void zapMaximumWindowSize(int &width, int &height)
{
  width = QApplication::desktop()->width() - X_BORDERS;
  height = QApplication::desktop()->height() - Y_BORDERS;
}

// Limit the window size to maximum size
void zapLimitWindowSize(int &width, int &height)
{
  int limh, limw;
  zapMaximumWindowSize(limw, limh);
  if (width > limw)
    width = limw;
  if (height > limh)
    height = limh;
}

// Limit window position in system-dependent way
void zapLimitWindowPos(int neww, int newh, int &newdx, int &newdy)
{
  int limw = QApplication::desktop()->width();
  int limh = QApplication::desktop()->height();

  // X11: spread margin equally top and bottom
  // Windows: put extra at bottom for task bar
  // Mac: put extra at top for menu
  int mintop = (Y_BORDERS - TITLE_SPACE) / 2;
#ifdef _WIN32
  mintop = 0;
#endif
#ifdef Q_OS_MACX
  mintop = Y_BORDERS - TITLE_SPACE - 10;
#endif
#ifdef SGI_GEOMETRY_HACK
  mintop = (Y_BORDERS + TITLE_SPACE) / 2;
#endif

  if (newdx < X_BORDERS / 2)
    newdx = X_BORDERS / 2;
  if (newdx + neww > limw - X_BORDERS / 2)
    newdx = limw - X_BORDERS / 2 - neww;

  if (newdy < mintop)
    newdy = mintop;
  if (newdy + newh > limh - (Y_BORDERS - mintop))
    newdy = limh - (Y_BORDERS - mintop) - newh;
}

/* Resize window to fit either whole image or part in rubber band */
static void zapResizeToFit(ZapStruct *zap)
{
  int width, height, neww, newh;
  int dx, dy, newdx, newdy;
  float xl, xr, yb, yt;
  width = zap->qtWindow->width();
  height = zap->qtWindow->height();
  QRect pos = ivwRestorableGeometry(zap->qtWindow);
  dx = pos.x();
  dy = pos.y();
  /* imodPrintStderr("dx %d dy %d\n", dx, dy); */
  if (zap->rubberband) {
    /* If rubberbanding, set size to size of band, and offset
       image by difference between band and window center */
    zapBandImageToMouse(zap, 0);
    xl = zap->rbImageX0;
    yb = zap->rbImageY0;
    xr = zap->rbImageX1;
    yt = zap->rbImageY1;
    neww = zap->rbMouseX1 -1 - zap->rbMouseX0 + width - zap->winx;
    newh = zap->rbMouseY1 -1 - zap->rbMouseY0 + height - zap->winy;
    zap->xtrans = (int)(-(xr + xl - zap->vi->xsize) / 2);
    zap->ytrans = (int)(-(yt + yb - zap->vi->ysize) / 2);

    // 3/6/05: turn off through common function to keep synchronized
    zapToggleRubberband(zap);
  } else {
    /* Otherwise, make window the right size for the image */
    neww = (int)(zap->zoom * zap->vi->xsize + width - zap->winx);
    newh = (int)(zap->zoom * zap->vi->ysize + height - zap->winy);
  }

  zapLimitWindowSize(neww, newh);
  newdx = dx + width / 2 - neww / 2;
  newdy = dy + height / 2 - newh / 2;
  zapLimitWindowPos(neww, newh, newdx, newdy);

  if (imodDebug('z'))
    imodPrintStderr("configuring widget...");

  /* imodPrintStderr("newdx %d newdy %d\n", newdx, newdy); */
  zap->qtWindow->resize(neww, newh);
  zap->qtWindow->move(newdx, newdy);

  /* DNM 9/12/03: remove the ZAP_EXPOSE_HACK, and a second set geometry that
     was needed temporarily with Qt 3.2.1 on Mac */

  if (imodDebug('z'))
    imodPrintStderr("back\n");
}
     
// Set the control priority and set flag to record subarea and do float
static void setControlAndLimits(ZapStruct *zap)
{
  ivwControlPriority(zap->vi, zap->ctrl);
  zap->recordSubarea = 1;
}

// Record the limits of the image displayed in the window or in the rubber band
static void setAreaLimits(ZapStruct *zap)
{
  int minArea = 16;
  float xl, xr, yb, yt, delta;
  if (zap->rubberband) {
    xl = zap->rbImageX0;
    yb = zap->rbImageY0;
    xr = zap->rbImageX1;
    yt = zap->rbImageY1;

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
    zapGetixy(zap, 0, 0, &xl, &yt);
    zapGetixy(zap, zap->winx, zap->winy, &xr, &yb);
  }
  subStartX = B3DMAX((int)(xl + 0.5), 0);
  subEndX = B3DMIN((int)(xr - 0.5), zap->vi->xsize - 1);
  subStartY = B3DMAX((int)(yb + 0.5), 0);
  subEndY = B3DMIN((int)(yt - 0.5), zap->vi->ysize - 1);
  if (imodDebug('z'))
    imodPrintStderr("Set area %d %d %d %d\n", subStartX, subEndX, subStartY,
                    subEndY);
}     

// Return the subset limits from the active window
int zapSubsetLimits(ViewInfo *vi, int &ixStart, int &iyStart, int &nxUse, 
                    int &nyUse)
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

// Toggle the rubber band
void zapToggleRubberband(ZapStruct *zap)
{
  if (zap->rubberband || zap->startingBand) {
    zap->rubberband = 0;
    zap->startingBand = 0;
    setControlAndLimits(zap);
    zap->gfx->setMouseTracking(insertDown != 0);
  } else {
    zap->startingBand = 1;
    zap->shiftingCont = 0;
    /* Eliminated old code for making initial band */
  }
 
  // 3/6/05: synchronize the toolbar button
  zap->qtWindow->setToggleState(ZAP_TOGGLE_RUBBER, 
                                zap->rubberband + zap->startingBand);
  zapSetCursor(zap, zap->mousemode);
  zapDraw(zap);
}

// Report the rubberband coordinates of the first zap window with a band
void zapReportRubberband()
{
  QObjectList objList;
  ZapStruct *zap;
  int i, bin;
  int ixl, ixr, iyb, iyt;

  imodDialogManager.windowList(&objList, -1, ZAP_WINDOW_TYPE);

  for (i = 0; i < objList.count(); i++) {
    zap = ((ZapWindow *)objList.at(i))->mZap;
    bin = zap->vi->xybin;
    if (zap->rubberband) {
      ixl = (int)floor(zap->rbImageX0 + 0.5);
      ixr = (int)floor(zap->rbImageX1 - 0.5);
      iyb = (int)floor(zap->rbImageY0 + 0.5);
      iyt = (int)floor(zap->rbImageY1 - 0.5);
      if (ixr < 1 || iyt < 1 || ixl > zap->vi->xsize - 2 || 
          iyb > zap->vi->ysize - 2)
        continue;

      if (ixl < 0)
        ixl = 0;
      if (ixr >= zap->vi->xsize)
        ixr = zap->vi->xsize - 1;
      if (iyb < 0)
        iyb = 0;
      if (iyt >= zap->vi->ysize)
        iyt = zap->vi->ysize - 1;
      ixl *= bin;
      iyb *= bin;
      ixr = ixr * bin + bin - 1;
      iyt = iyt * bin + bin - 1;
        
      imodPrintStderr("Rubberband: %d %d %d %d\n", ixl + 1, iyb + 1, ixr + 1,
              iyt + 1);
      return;
    }
  }
  imodPrintStderr("ERROR: No Zap window has usable rubberband coordinates\n");
}

// Return coordinates of first rubber band; return value 1 if any, 0 if none
int zapRubberbandCoords(float &rbX0, float &rbX1, float &rbY0, float &rbY1)
{
  QObjectList objList;
  ZapStruct *zap;
  int i;

  imodDialogManager.windowList(&objList, -1, ZAP_WINDOW_TYPE);

  for (i = 0; i < objList.count(); i++) {
    zap = ((ZapWindow *)objList.at(i))->mZap;
    if (zap->rubberband) {
      rbX0 = zap->rbImageX0;
      rbX1 = zap->rbImageX1;
      rbY0 = zap->rbImageY0;
      rbY1 = zap->rbImageY1;
      return 1;
    }
  }
  return 0;
}

// Take a snapshot by montaging at higher zoom
static void montageSnapshot(ZapStruct *zap)
{
  int ix, iy, xFullSize, yFullSize, xTransStart, yTransStart, xTransDelta;
  int yTransDelta, xCopyDelta, yCopyDelta, xTransSave, yTransSave;
  int limits[4];
  unsigned char *framePix, *fullPix, **linePtrs;
  double zoomSave;
  char fname[256];
  int fileno = 0;
  int factor = imcGetMontageFactor();

  // Get coordinates and offsets and buffers
  setAreaLimits(zap);
  getMontageShifts(zap, factor, zap->xstart, zap->xborder,
                   zap->vi->xsize, zap->winx,
                   xTransStart, xTransDelta, xCopyDelta, xFullSize);
  getMontageShifts(zap, factor, zap->ystart, zap->yborder, 
                   zap->vi->ysize, zap->winy,
                   yTransStart, yTransDelta, yCopyDelta, yFullSize);
  framePix = (unsigned char *)malloc(4 * zap->winx * zap->winy);
  fullPix = (unsigned char *)malloc(4 * xFullSize * yFullSize);
  linePtrs = (unsigned char **)malloc(yFullSize * sizeof(unsigned char *));
  if (!framePix || !fullPix || !linePtrs) {
    if (framePix)
      free(framePix);
    if (fullPix)
      free(fullPix);
    if (linePtrs)
      free(linePtrs);
    wprint("\aFailed to get memory for snapshot buffers.\n");
    return;
  }

  // Save translations and loop on frames, getting pixels and copying them
  xTransSave = zap->xtrans;
  yTransSave = zap->ytrans;
  zoomSave = zap->zoom;
  zap->zoom *= factor;
  for (iy = 0; iy < factor; iy++) {
    for (ix = 0; ix < factor; ix++) {
      zap->xtrans = -(xTransStart + ix * xTransDelta);
      zap->ytrans = -(yTransStart + iy * yTransDelta);
      zapDraw(zap);
      glReadPixels(0, 0, zap->winx, zap->winy, GL_RGBA, GL_UNSIGNED_BYTE, 
                   framePix);
      glFlush();
      memreccpy(fullPix, framePix, zap->winx, zap->winy, 4,
                xFullSize - zap->winx, ix * xCopyDelta, iy * yCopyDelta,
                0, 0, 0);
    }
  }

  // Save the image then restore display
  for (iy = 0; iy < yFullSize; iy++)
    linePtrs[iy] = fullPix + 4 * xFullSize * iy;
  limits[0] = limits[1] = 0;
  limits[2] = xFullSize;
  limits[3] = yFullSize;
  b3dGetSnapshotName(fname, "zap", SnapShot_TIF, 3, fileno);
  imodPrintStderr("3dmod: Saving zap montage to %s", fname);
  b3dSnapshot_TIF(fname, 4, limits, linePtrs);
  imodPuts("");

  zap->xtrans = xTransSave;
  zap->ytrans = yTransSave;
  zap->zoom = zoomSave;
  zapDraw(zap);
    
  free(framePix);
  free(fullPix);
  free(linePtrs);
}

// Compute shifts and increments for the montage snapshot
static void getMontageShifts(ZapStruct *zap, int factor, int imStart, 
                             int border, int imSize, int winSize,
                             int &transStart, int &transDelta, int &copyDelta,
                             int &fullSize)
{
  int inWin, overlap, imEnd;
  imEnd = B3DMIN(imStart + (int)((winSize - border)/ zap->zoom), imSize);
  inWin = (int)(winSize / (zap->zoom * factor));
  overlap = (factor * inWin + imStart - imEnd) / (factor - 1);
  transDelta = inWin - overlap;
  transStart = imStart + (inWin - imSize) / 2;
  copyDelta = zap->zoom * factor * transDelta;
  fullSize = (factor - 1) * copyDelta + winSize;
  if (imodDebug('z'))
    imodPrintStderr("im %d - %d  bord %d win %d  inwin %d overlap %d start %d delta %d  copy %d  "
                    "full %d\n", imStart, imEnd, border, winSize, inWin, overlap, transStart, 
                    transDelta, copyDelta, fullSize);
}

/****************************************************************************/
/* drawing routines.                                                        */

/* Draws the image */
static void zapDrawGraphics(ZapStruct *zap)
{
  ImodView *vi = zap->vi;
  int x, y, z;
  int time;
  unsigned char **imageData;

  ivwGetLocation(vi, &x, &y, &z);

  // DNM eliminated unused function 1/23/03
  // b3dSetCurPoint(x, y, zap->section);

  b3dSetImageOffset(zap->winx, vi->xsize, zap->zoom,
                    &zap->xdrawsize, &zap->xtrans, 
                    &zap->xborder, &zap->xstart);

  b3dSetImageOffset(zap->winy, vi->ysize, zap->zoom,
                    &zap->ydrawsize, &zap->ytrans, 
                    &zap->yborder, &zap->ystart);

  /* Get the time to display and flush if time is different. */
  if (zap->timeLock)
    time = zap->timeLock;
  else
    ivwGetTime(vi, &time);
  if (time != zap->time){
    b3dFlushImage(zap->image);
    zap->time = time;
  }
  imageData = ivwGetZSectionTime(vi, zap->section, time);

  // If flag set, record the subarea size, clear flag, and do call float to
  // set the color map if necessary.  If the black/white changes, flush image
  if (zap->recordSubarea) {
    setAreaLimits(zap);
    zap->recordSubarea = 0;
    x = vi->black;
    y = vi->white;
    imod_info_bwfloat(vi, zap->section, time);
    if (App->rgba && (x != vi->black || y != vi->white))
      b3dFlushImage(zap->image);
  }

  b3dDrawBoxout(zap->xborder, zap->yborder, 
		zap->xborder + (int)(zap->xdrawsize * zap->zoom),
		zap->yborder + (int)(zap->ydrawsize * zap->zoom));
  b3dDrawGreyScalePixelsHQ(imageData,
			   vi->xsize, vi->ysize,
			   zap->xstart, zap->ystart,
			   zap->xborder, zap->yborder,
			   zap->xdrawsize, zap->ydrawsize,
			   zap->image,
			   vi->rampbase, 
			   zap->zoom, zap->zoom,
			   zap->hqgfx, zap->section);

  /* DNM 9/15/03: Get the X zoom, which might be slightly different */
  zap->xzoom = b3dGetCurXZoom();
}

static void zapDrawModel(ZapStruct *zap)
{
  ImodView *vi = zap->vi;
  int ob, co;
  int surf = -1;
  Icont *cont = imodContourGet(vi->imod);

  if (vi->imod->drawmode <= 0)
    return;

  zapDrawGhost(zap);

  if (cont)
    surf = cont->surf;

  for (ob = 0; ob < vi->imod->objsize; ob++){
    if (iobjOff(vi->imod->obj[ob].flags))
      continue;
    imodSetObjectColor(ob); 
    b3dLineWidth(vi->imod->obj[ob].linewidth2); 

    for (co = 0; co < vi->imod->obj[ob].contsize; co++){
      if (ob == vi->imod->cindex.object){
        if (co == vi->imod->cindex.contour){
          zapDrawContour(zap, co, ob);
          continue;
        }
        if (vi->ghostmode & IMOD_GHOST_SURFACE)
          if (surf >= 0)
            if (surf != vi->imod->obj[ob].cont[co].surf){
              b3dColorIndex(App->ghost); 
              zapDrawContour(zap, co, ob);
              imodSetObjectColor(ob);
              continue;
            }
      }

      zapDrawContour(zap, co, ob); 
    }
  }
}

// A separate routine to draw the extra object so that model - current point -
// extra object drawing could happen in the right order
static void zapDrawExtraObject(ZapStruct *zap)
{
  ImodView *vi = zap->vi;
  Iobj *xobj = vi->extraObj;
  int co;

  if (vi->imod->drawmode <= 0 || !xobj->contsize)
    return;

  // If there are contours in the extra object, set color or red, and draw
  if (App->rgba)
    glColor3f(xobj->red, xobj->green, xobj->blue);
  else
    b3dColorIndex(App->endpoint);
  for (co = 0; co < xobj->contsize; co++)
    zapDrawContour(zap, co, -1);
}

void zapDrawSymbol(int mx, int my, 
                   unsigned char sym,
                   unsigned char size, 
                   unsigned char flags)
{
    
  switch (sym){
  case IOBJ_SYM_CIRCLE:
    if (flags  & IOBJ_SYMF_FILL)
      b3dDrawFilledCircle(mx, my, size);
    else
      b3dDrawCircle(mx, my, size);
    break;
  case IOBJ_SYM_SQUARE:
    if (flags  & IOBJ_SYMF_FILL)
      b3dDrawFilledSquare(mx, my, size);
    else
      b3dDrawSquare(mx, my, size);
    break;
  case IOBJ_SYM_TRIANGLE:
    if (flags  & IOBJ_SYMF_FILL)
      b3dDrawFilledTriangle(mx, my, size);
    else
      b3dDrawTriangle(mx, my, size);
    break;
  case IOBJ_SYM_STAR:
    break;
  case IOBJ_SYM_NONE:
    b3dDrawPoint(mx, my);
    break;

  default:
    return;

  }
  return;
}


static void zapDrawContour(ZapStruct *zap, int co, int ob)
{
  ImodView *vi = zap->vi;
  float delz;
  Iobj  *obj;
  Icont *cont;
  DrawProps contProps, ptProps;
  int pt, radius, lastX, lastY, thisX, thisY;
  float drawsize, zscale;
  int nextChange, stateFlags, changeFlags;
  int checkSymbol = 0;
  int handleFlags = HANDLE_LINE_COLOR | HANDLE_2DWIDTH;
  bool lastVisible, thisVisible, selected;
  bool currentCont = (co == vi->imod->cindex.contour) &&
    (ob == vi->imod->cindex.object );

  if (ob >= 0)
    obj  = &(vi->imod->obj[ob]);
  else
    obj = zap->vi->extraObj;
  cont = &(obj->cont[co]);

  if ((!cont) || (!cont->psize))
    return;


  zscale = ((vi->imod->zscale ? vi->imod->zscale : 1.) * vi->zbin) / vi->xybin;

  /* check for contours that contain time data. */
  /* Don't draw them if the time isn't right. */
  /* DNM 6/7/01: but draw contours with time 0 regardless of time */
  if (ivwTimeMismatch(vi, zap->timeLock, obj, cont))
    return;

  // get draw properties
  selected = imodSelectionListQuery(vi, ob, co) > -2;
  nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, &stateFlags,
                                   handleFlags, selected);
  if (contProps.gap)
    return;

  /* Open or closed contour */
  // Skip if not wild and not on section
  lastVisible = zapPointVisable(zap, &(cont->pts[0]));
  if (!iobjScat(obj->flags) && ((cont->flags & ICONT_WILD) || lastVisible)) {
    if ((cont->flags & ICONT_WILD) || nextChange >= 0) {
      if (!nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, &changeFlags, 
                                         handleFlags, selected);

      // For wild contour, test every point and connect only pairs on section
      lastX = zapXpos(zap, cont->pts[0].x);
      lastY = zapYpos(zap, cont->pts[0].y);
      for (pt = 1; pt < cont->psize; pt++) {
        thisVisible = zapPointVisable(zap, &(cont->pts[pt]));
        if (thisVisible) {
          thisX = zapXpos(zap, cont->pts[pt].x);
          thisY = zapYpos(zap, cont->pts[pt].y);
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
                                           selected);

      }

      // IF closed contour in closed object and not current, draw closure as
      // long as both points are visible
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN) && 
          !ptProps.gap && 
          !currentCont && lastVisible && zapPointVisable(zap, cont->pts))
        b3dDrawLine(lastX, lastY, zapXpos(zap, cont->pts->x),
                    zapYpos(zap, cont->pts->y));

    } else {

      // For non-wild contour with no changes, draw all points without testing
      b3dBeginLine();
      for (pt = 0; pt < cont->psize; pt++)
        b3dVertex2i(zapXpos(zap, cont->pts[pt].x),
                    zapYpos(zap, cont->pts[pt].y));

      // IF closed contour in closed object and not current, draw closure
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN) && !currentCont)
        b3dVertex2i(zapXpos(zap, cont->pts->x), zapYpos(zap, cont->pts->y));

      b3dEndLine();
    }
          
    checkSymbol = 1;
  }
     
  /* symbols */
  if (ilistSize(cont->store))
    nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                     &stateFlags, handleFlags, selected);
  if ((iobjScat(obj->flags) || checkSymbol) && 
      (contProps.symtype != IOBJ_SYM_NONE || nextChange >= 0)) {
    for (pt = 0; pt < cont->psize; pt++) {
      if (pt == nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, &changeFlags, 
                                         handleFlags, selected);

      if (ptProps.symtype != IOBJ_SYM_NONE && 
          zapPointVisable(zap, &(cont->pts[pt]))){
        zapDrawSymbol(zapXpos(zap, cont->pts[pt].x),
                      zapYpos(zap, cont->pts[pt].y),
                      ptProps.symtype,
                      ptProps.symsize,
                      ptProps.symflags);
      }
    }
  }

  /* Any contour with point sizes set */
  if (iobjScat(obj->flags) || cont->sizes || obj->pdrawsize) {
    if (ilistSize(cont->store))
      nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                       &stateFlags, handleFlags, selected);
    for (pt = 0; pt < cont->psize; pt++){
      if (pt == nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, &changeFlags, 
                                         handleFlags, selected);

      drawsize = imodPointGetSize(obj, cont, pt) / vi->xybin;
      if (drawsize > 0)
        if (zapPointVisable(zap, &(cont->pts[pt]))){
          /* DNM: make the product cast to int, not drawsize */
          b3dDrawCircle(zapXpos(zap, cont->pts[pt].x),
                        zapYpos(zap, cont->pts[pt].y),
                        (int)(drawsize * zap->zoom));
          if (drawsize > 3)
            b3dDrawPlus(zapXpos(zap, cont->pts[pt].x), 
                        zapYpos(zap, cont->pts[pt].y), 3);
        }else{
          if (drawsize > 1){
            /* DNM: fixed this at last, but let size round
               down so circles get smaller*/
            /* draw a smaller circ if further away. */
            delz = (cont->pts[pt].z - zap->section) * zscale;
            if (delz < 0)
              delz = -delz;
                        
            if (delz < drawsize - 0.01) {
              radius = (int)(sqrt((double)(drawsize * drawsize - delz * delz))
                             * zap->zoom);
              b3dDrawCircle(zapXpos(zap, cont->pts[pt].x),
                            zapYpos(zap, cont->pts[pt].y),
                            radius);
            }
          }
        }
    }
  }

  // Draw end markers
  if ((obj->symflags & IOBJ_SYMF_ENDS) && ob >= 0){
    if (zapPointVisable(zap, &(cont->pts[cont->psize-1]))){
      b3dColorIndex(App->endpoint);
      b3dDrawCross(zapXpos(zap, cont->pts[cont->psize-1].x),
                   zapYpos(zap, cont->pts[cont->psize-1].y), 
                   obj->symsize/2);
    }
    if (zapPointVisable(zap, cont->pts)){
      b3dColorIndex(App->bgnpoint);
      b3dDrawCross(zapXpos(zap, cont->pts->x),
                   zapYpos(zap, cont->pts->y),
                   obj->symsize/2);
    }
    imodSetObjectColor(ob);
  }

  /* Removed drawing of size 3 circles at ends of current open contour if
     first two points visible or last point visible and next to last is not */

  if (selected)
    b3dLineWidth(obj->linewidth2); 
}

/* Set the size of current and endpoint markers so that they do not conflict
   with symbol size or, if not symbols, with 3D sphere size */
void zapCurrentPointSize(Iobj *obj, int *modPtSize, int *backupSize,
                         int *imPtSize)
{
  // These two will be user preferences
  int minModSize = ImodPrefs->minCurrentModPtSize();
  int minImSize = ImodPrefs->minCurrentImPtSize();
  int symSize = 0;

  // Set sizes to minima
  *modPtSize = minModSize;
  *backupSize = minModSize + 2;
  *imPtSize = minImSize;
  if (!obj)
    return;

  // Determine an interfering symbol size
  if (obj->symbol != IOBJ_SYM_NONE && obj->symsize > 0)
    symSize = obj->symsize;
  if (!symSize && obj->pdrawsize > 0)
    symSize = obj->pdrawsize / App->cvi->xybin;

  // Make sure symbol and point sizes differ by at least 2
  if (symSize - *modPtSize < 2 && *modPtSize - symSize < 2)
    *modPtSize = symSize + 2;
  *backupSize = *modPtSize + 2;
  if (symSize - *backupSize < 2 && *backupSize - symSize < 2)
    *backupSize = symSize + 2;
  if (symSize - *imPtSize < 2 && *imPtSize - symSize < 2)
    *imPtSize = symSize + 2;
}

static void zapDrawCurrentPoint(ZapStruct *zap)
{
  ImodView *vi = zap->vi;
  Iobj *obj = imodObjectGet(vi->imod);
  Icont *cont = imodContourGet(vi->imod);
  Ipoint *pnt = imodPointGet(vi->imod);
  int imPtSize, modPtSize, backupSize, curSize;
  int x,y;

  if (!vi->drawcursor) return;

  zapCurrentPointSize(obj, &modPtSize, &backupSize, &imPtSize);

  // 11/11/04: Reset line width for slice lines or current image point,
  // set it below with object-specific thickness
  b3dLineWidth(1);

  if ((vi->imod->mousemode == IMOD_MMOVIE)||(!pnt)){
    x = zapXpos(zap, (float)((int)vi->xmouse + 0.5));
    y = zapYpos(zap, (float)((int)vi->ymouse + 0.5));
    b3dColorIndex(App->curpoint);
    b3dDrawPlus(x, y, imPtSize);
          
  }else{
    if ((cont) && (cont->psize) && (pnt)){

      b3dLineWidth(obj->linewidth2);
      curSize = modPtSize;
      if (cont->psize > 1 && 
          (pnt == cont->pts || pnt == cont->pts + cont->psize - 1))
        curSize = backupSize;
          
      /* DNM 6/17/01: display off-time features as if off-section */
      x = zapXpos(zap, pnt->x);
      y = zapYpos(zap, pnt->y);
      if (zapPointVisable(zap, pnt) && 
	  !ivwTimeMismatch(vi, zap->timeLock, obj, cont)) {
        b3dColorIndex(App->curpoint);
      }else{
        b3dColorIndex(App->shadow);
      }
      b3dDrawCircle(x, y, curSize);
    }
  }
     
  /* draw begin/end points for current contour */
  if (cont){
    if (ivwTimeMismatch(vi, zap->timeLock, obj, cont))
      return;

    b3dLineWidth(obj->linewidth2);
    if (cont->psize > 1){
      if (zapPointVisable(zap, cont->pts)){
        b3dColorIndex(App->bgnpoint);
        b3dDrawCircle(zapXpos(zap, cont->pts->x),
                      zapYpos(zap, cont->pts->y), modPtSize);
      }
      if (zapPointVisable(zap, &(cont->pts[cont->psize - 1]))){
        b3dColorIndex(App->endpoint);
        b3dDrawCircle(zapXpos(zap, cont->pts[cont->psize - 1].x),
                      zapYpos(zap, cont->pts[cont->psize - 1].y), 
                      modPtSize);
      }
    }
  }

  b3dLineWidth(1);
  if (zap->showslice){
    b3dColorIndex(App->foreground);
    b3dDrawLine(x, y,
                zapXpos(zap, vi->slice.zx1+0.5f),
                zapYpos(zap, vi->slice.zy1+0.5f));
    b3dDrawLine(x, y,
                zapXpos(zap, vi->slice.zx2+0.5f), 
                zapYpos(zap, vi->slice.zy2+0.5f));
    zap->showslice = 0;
  }
  
  return;
}

static void zapDrawGhost(ZapStruct *zap)
{
  int co, i, ob;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  Imod *mod = zap->vi->imod;
  DrawProps contProps, ptProps;
  int nextz, prevz, iz;
  int pt, npt, lastX, lastY, thisX, thisY;
  int nextChange, stateFlags, changeFlags;
  int handleFlags = HANDLE_2DWIDTH;

  if (!mod)
    return;

  if ( !(zap->vi->ghostmode & IMOD_GHOST_SECTION))
    return;
     
  for (ob = 0; ob < mod->objsize; ob++) {
    if (ob != mod->cindex.object && !(zap->vi->ghostmode & IMOD_GHOST_ALLOBJ))
      continue;
    obj = &(mod->obj[ob]);

    /* DNM: don't do scattered points - point size works for that */
    if(iobjScat(obj->flags))
      continue;

    /* DNM 6/16/01: need to be based on zap->section, not zmouse */
    nextz = zap->section + zap->vi->ghostdist;
    prevz = zap->section - zap->vi->ghostdist;
     
    for (co = 0; co < obj->contsize; co++) {
      cont = &(obj->cont[co]);
      nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                       &stateFlags, handleFlags, 0);
      if (contProps.gap)
        continue;
      zapSetGhostColor(zap, contProps.red, contProps.green, contProps.blue);
      
      /* DNM: don't display wild contours, only coplanar ones */
      /* By popular demand, display ghosts from lower and upper sections */
      if (cont->pts && !(cont->flags & ICONT_WILD)) {
        iz = (int)floor(cont->pts->z + 0.5);
        if ((iz > zap->section && iz <= nextz && 
             (zap->vi->ghostmode & IMOD_GHOST_PREVSEC)) ||
            (iz < zap->section && iz >= prevz && 
             (zap->vi->ghostmode & IMOD_GHOST_NEXTSEC))) {

          if (nextChange < 0) {
            b3dBeginLine();
            for (i = 0; i < cont->psize; i++) {
              b3dVertex2i(zapXpos(zap, cont->pts[i].x),
                          zapYpos(zap, cont->pts[i].y));
            }
          
            /* DNM: connect back to start only if closed contour */
            if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN))
              b3dVertex2i(zapXpos(zap, cont->pts->x),
                          zapYpos(zap, cont->pts->y));
            b3dEndLine();
          } else {

            // If there are changes in contour, then draw only needed lines
            lastX = zapXpos(zap, cont->pts[0].x);
            lastY = zapYpos(zap, cont->pts[0].y);
            for (pt = 0; pt < cont->psize; pt++) {
              ptProps.gap = 0;
              if (pt == nextChange) {
                nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                                 &ptProps, &stateFlags,
                                                 &changeFlags, handleFlags, 0);
                if (changeFlags & CHANGED_COLOR)
                  zapSetGhostColor(zap, ptProps.red, ptProps.green, 
                                   ptProps.blue);
              }

              // Skip gap or last point if open
              npt = (pt + 1) % cont->psize;
              thisX = zapXpos(zap, cont->pts[npt].x);
              thisY = zapYpos(zap, cont->pts[npt].y);
              if ((pt < cont->psize - 1 || 
                   (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN)))
                  && !ptProps.gap)
                b3dDrawLine(lastX, lastY, thisX, thisY);
              lastX = thisX;
              lastY = thisY;
            }
          }
        }
      }
    }
  }
  return;
}

static void zapSetGhostColor(ZapStruct *zap, float obr, float obg, float obb)
{
  int red, green, blue, base;

  // Set base to 2 to make color get brighter instead of darker
  base = (zap->vi->ghostmode & IMOD_GHOST_LIGHTER) ? 2 : 0;
  red = (int)(((base + obr) * 255.0) / 3.0);
  green = (int)(((base + obg) * 255.0) / 3.0);
  blue = (int)(((base + obb) * 255.0) / 3.0);
    
  mapcolor(App->ghost, red, green, blue); 
  b3dColorIndex(App->ghost);  
  
  /* DNM: if it's RGB, just have to set the color here */
  if (App->rgba)
    glColor3f(red/255., green/255., blue/255.);
}

static int zapDrawAuto(ZapStruct *zap)
{
  ImodView *vi = zap->vi;
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

  if (vi->ax->cz != zap->section)
    return(-1);

  /* DNM 8/11/01: make rectangle size be nearest integer and not 0 */
  rectsize = zap->zoom < 1 ? 1 : (int)(zap->zoom + 0.5);
  for (j = 0; j < ysize; j++){
    y = zapYpos(zap,(float)j);
    for(i = 0; i < xsize; i++){
      x = zapXpos(zap,(float)i);
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
static void zapDrawTools(ZapStruct *zap)
{
  QString qstr;

  if (zap->toolMaxZ != zap->vi->zsize) {
    zap->toolMaxZ = zap->vi->zsize;
    zap->qtWindow->setMaxZ(zap->toolMaxZ);
  }

  if (zap->toolSection != zap->section){
    zap->toolSection = zap->section;
    zap->qtWindow->setSectionText(zap->section + 1);
  }
     
  if (zap->toolZoom != zap->zoom){
    zap->toolZoom = zap->zoom;
    zap->qtWindow->setZoomText(zap->zoom);
  }

  if (zap->vi->nt) {
    int time = zap->timeLock ? zap->timeLock : zap->vi->ct;
    if (zap->toolTime != time){
      zap->toolTime = time;
      qstr.sprintf(" (%3d)", time);
      qstr += ivwGetTimeIndexLabel(zap->vi, time);
      zap->qtWindow->setTimeLabel(qstr);
    }
  }
}

static void zapSetCursor(ZapStruct *zap, int mode)
{
  static int lastShape = -1;
  int shape;

  // Set up a special cursor for the rubber band
  if (zap->startingBand || (zap->rubberband && (moveband || dragband)) ||
      zap->shiftingCont) {
    if (zap->startingBand || moveband || zap->shiftingCont)
      shape = Qt::SizeAllCursor;
    else if (dragging[0] && dragging[2] || dragging[1] && dragging[3])
      shape = Qt::SizeFDiagCursor;
    else if (dragging[1] && dragging[2] || dragging[0] && dragging[3])
      shape = Qt::SizeBDiagCursor;
    else if (dragging[0] || dragging[1])
      shape = Qt::SizeHorCursor;
    else if (dragging[2] || dragging[3])
      shape = Qt::SizeVerCursor;
    if (shape != lastShape)
      zap->gfx->setCursor(QCursor(shape));
    lastShape = shape;
    return;
  }

  // Or restore cursor from rubber band or change cursor dur to mode change
  if (zap->mousemode != mode || lastShape >= 0){
    if (mode == IMOD_MMODEL)
      zap->gfx->setCursor(*App->modelCursor);
    else
      zap->gfx->unsetCursor();
    zap->mousemode = mode;
    lastShape = -1;
    imod_info_input();
  }
  return;
}


static int zapPointVisable(ZapStruct *zap, Ipoint *pnt)
{
  int cz;

  if (zap->twod) return(1);

  /* DNM 11/30/02: replace +/- alternatives with standard nearest int code */
  cz = (int)floor(pnt->z + 0.5);
    
  if (cz == zap->section)
    return(1);
    
  return(0);
}


/*
$Log$
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
