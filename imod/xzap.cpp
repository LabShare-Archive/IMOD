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
static void zapAnalyzeBandEdge(ZapStruct *zap, int ix, int iy);
static int checkPlugUseMouse(ZapStruct *zap, QMouseEvent *event, int but1, 
                             int but2, int but3);
static int zapButton1(struct zapwin *zap, int x, int y, int controlDown);
static int zapButton2(struct zapwin *zap, int x, int y, int controlDown);
static int zapButton3(struct zapwin *zap, int x, int y, int controlDown);
static int zapB1Drag(struct zapwin *zap, int x, int y);
static int zapB2Drag(struct zapwin *zap, int x, int y, int controlDown);
static int zapB3Drag(struct zapwin *zap, int x, int y, int controlDown, 
                      int shiftDown);
static int zapDelUnderCursor(ZapStruct *zap, int x, int y, Icont *cont);
static int contInSelectArea(Iobj *obj, Icont *cont, Ipoint selmin,
                            Ipoint selmax);
static int dragSelectContsCrossed(struct zapwin *zap, int x, int y);
static void endContourShift(ZapStruct *zap);
static Icont *checkContourShift(ZapStruct *zap, int &pt, int &err);
static void setupContourShift(ZapStruct *zap);
static int startShiftingContour(ZapStruct *zap, int x, int y, int button,
                                 int ctrlDown);
static void shiftContour(ZapStruct *zap, int x, int y, int button, 
                         int shiftDown);
static int startMovieCheckSnap(ZapStruct *zap, int dir);
static void registerDragAdditions(ZapStruct *zap);
static int mouseXformMatrix(ZapStruct *zap, int x, int y, int type, 
                            float mat[2][2]);
static void markXformCenter(ZapStruct *zap, float ix, float iy);

static void zapDrawGraphics(ZapStruct *zap);
static void fillOverlayRGB(unsigned char **lines, int nx, int ny, int chan,
                           unsigned char *image);
static void zapDrawModel(ZapStruct *zap);
static void zapDrawContour(ZapStruct *zap, int co, int ob);
static void zapDrawCurrentPoint(ZapStruct *zap);
static void zapDrawExtraObject(ZapStruct *zap);
static int  zapDrawAuto(ZapStruct *zap);
static void zapDrawGhost(ZapStruct *zap);
static void zapSetGhostColor(ZapStruct *zap, float obr, float obg, float obb);
static void zapDrawTools(ZapStruct *zap);
static void zapSetCursor(ZapStruct *zap, int mode, bool setAnyway = false);
static int  zapXpos(ZapStruct *zap, float x);
static int  zapYpos(ZapStruct *zap, float x);
static void zapGetixy(ZapStruct *zap, int mx, int my, float *x, float *y, 
                      int *z);
static int  zapPointVisable(ZapStruct *zap, Ipoint *pnt);
static void zapAutoTranslate(ZapStruct *zap);
static void zapSyncImage(ZapStruct *win);
static void zapResizeToFit(ZapStruct *zap);
static void setAreaLimits(ZapStruct *zap);
static void setControlAndLimits(ZapStruct *zap);
static void zapToggleRubberband(ZapStruct *zap, bool draw = true);
static void zapBandImageToMouse(ZapStruct *zap, int ifclip); 
static void zapBandMouseToImage(ZapStruct *zap, int ifclip);
static void montageSnapshot(ZapStruct *zap, int snaptype);
static void shiftRubberband(ZapStruct *zap, float idx, float idy);
static int getMontageShifts(ZapStruct *zap, int factor, int imStart, 
                             int border, int imSize, int winSize,
                             int &transStart, int &transDelta, int &copyDelta,
                             int &fullSize);
static void setMouseTracking(ZapStruct *zap);
static void zapFlushImage(ZapStruct *zap);
static void panelIndexAndCoord(int size, int num, int gutter, int border, 
                               int &pos, int &panelInd);
static bool getLowHighSection(ZapStruct *zap, int &low, int &high);
static void setDrawCurrentOnly(ZapStruct *zap, int value);

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

void zapHelp(ZapStruct *zap)
{
  if (zap->numXpanels)
    imodShowHelpPage("multizap.html");
  else
    imodShowHelpPage("zap.html");
}

/*
 * This receives the close signal back from the controller, tells the
 * window to close, and sets the closing flag
 */
static void zapClose_cb(ImodView *vi, void *client, int junk)
{
  ZapStruct *zap = (ZapStruct *)client;
  if (imodDebug('z'))
    imodPrintStderr("Sending zap window close.\n");
  zap->popup = 0;
  zap->qtWindow->close();
}

/*
 * This receives a closing signal from the window 
 */
void zapClosing(ZapStruct *zap)
{
  if (imodDebug('z'))
    imodPrintStderr("ZapClosing received.\n");

  // Do cleanup
  zap->popup = 0;
  ivwRemoveControl(zap->vi, zap->ctrl);
  imodDialogManager.remove((QWidget *)zap->qtWindow);
  if (!zap->numXpanels)
    numZapWindows--;

  // What for?  flush any events that might refer to this zap
  imod_info_input();     

  if (!zap->numXpanels)
    b3dFreeCIImage(zap->image);
  else {
    if (!maxMultiZarea) {
      QRect pos = ivwRestorableGeometry(zap->qtWindow);
      ImodPrefs->recordMultiZparams(pos, zap->numXpanels, zap->numYpanels, 
                                    zap->panelZstep, zap->drawInCenter, 
                                    zap->drawInOthers);
    }
    for (int i = 0; i < zap->numImages; i++)
      b3dFreeCIImage(zap->images[i]);
    if (zap->images)
      free(zap->images);
  }
  free(zap);
}

/*
 * Start or stop movie and check for whether to start a movie snapshot sequence
 */
static int startMovieCheckSnap(ZapStruct *zap, int dir)
{
  int start, end;

  imodMovieXYZT(zap->vi, MOVIE_DEFAULT, MOVIE_DEFAULT, dir, MOVIE_DEFAULT);
  imcSetStarterID(zap->ctrl);

  zap->movieSnapCount = 0;
  b3dSetMovieSnapping(false);

  /* done if no movie, or if no snapshots are desired.  */
  if (!zap->vi->zmovie || !imcGetSnapshot(zap->vi))
    return 0;
     
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

  // Inform autosnapshot not to check file numbers from 0
  b3dSetMovieSnapping(true);

  /* draw - via imodDraw to get float done correctly */
  imodDraw(zap->vi, IMOD_DRAW_XYZ);
  return 1;
}

/*
 * This is the external draw command from the controller
 */
void zapDraw_cb(ImodView *vi, void *client, int drawflag)
{
  ZapStruct *zap = (ZapStruct *)client;
  int *limits;
  int limarr[4];
  int snaptype = imcGetSnapshot(zap->vi);

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

  // If the rubberband is enabled and a flip is happening, turn off the
  // rubberband.
  if ((zap->rubberband || zap->startingBand) && zap->toolMaxZ != zap->vi->zsize)
    zapToggleRubberband(zap, false);
  // zapDrawTools(zap);

  if (drawflag){
    if (drawflag & IMOD_DRAW_SLICE)
      zap->showslice = 1;

    /* DNM: skip this, it is covered by the zapdraw call below and the
       items that it sets are not needed by the flush or sync */
    /* b3dWinset(XtDisplay(zap->gfx), zap->gfx, (XID)zap->context); */

    if (drawflag & IMOD_DRAW_IMAGE){
      zapFlushImage(zap);
    }
          
    if (!(drawflag & IMOD_DRAW_ACTIVE) && !(drawflag & IMOD_DRAW_NOSYNC))
      zapSyncImage(zap);

    /* DNM 1/29/03: no more worry about multiple calls */
    zapDraw(zap);

    /* DNM 3/8/01: add autosnapshot when movieing */
    // 3/8/07: make it take montages too
    if (snaptype && zap->vi->zmovie && 
        zap->movieSnapCount && imcGetStarterID() == zap->ctrl) {
      if (imcGetSnapMontage(true)) {
        montageSnapshot(zap, snaptype);
      } else {
        limits = NULL;
        if (zap->rubberband) {
          limits = limarr;
          zapBandImageToMouse(zap, 1);
          limarr[0] = zap->rbMouseX0 + 1;
          limarr[1] = zap->winy - zap->rbMouseY1;
          limarr[2] = zap->rbMouseX1 - 1 - zap->rbMouseX0;
          limarr[3] = zap->rbMouseY1 - 1 - zap->rbMouseY0;
        }
        b3dKeySnapshot((char *)(zap->numXpanels ? "multiz" : "zap"), 
                       snaptype - 1, snaptype % 2, limits);
      }
      zap->movieSnapCount--;
      /* When count expires, stop movie */
      if(!zap->movieSnapCount) {
        zap->vi->zmovie = 0;
        b3dSetMovieSnapping(false);
      }
    }

    // If there is only one zap window, set flag to record the subarea
    if (imodDialogManager.windowCount(ZAP_WINDOW_TYPE) == 1 && 
        !zap->numXpanels)
      zap->recordSubarea = 1;
  }
  return;
}

/*
 *  Sync the pan position to the current model point. 
 */
#define BORDER_FRAC  0.1
#define BORDER_MIN  50
#define BORDER_MIN_MULTIZ  20
#define BORDER_MAX  125

static void zapSyncImage(ZapStruct *zap)
{
  int syncborder, wposition, wsize, tripshift;
  int trytrans, trydraws, tryborder, trystart, borderMin;
  ImodView *vi = zap->vi;
  if ((!zap->lock) && 
      ((vi->imod->mousemode == IMOD_MMODEL && zap->vi->imod->cindex.point >= 0)
       || (zap->numXpanels && zap->keepcentered))) {
    borderMin = zap->numXpanels ? BORDER_MIN_MULTIZ : BORDER_MIN;

    /* If the keepcentered flag is set, just do a shift to center */
    if (zap->keepcentered)
      tripshift = 1;
    else {
      
      /* Otherwise, look at each axis independently.  First see if
         if the position is within the borders for shifting */
      tripshift = 0;
      wsize = zap->numXpanels ? zap->panelXsize : zap->winx;
      wposition = zapXpos(zap, vi->xmouse);
      syncborder = (int)(wsize * BORDER_FRAC);
      syncborder = B3DMIN(BORDER_MAX, B3DMAX(borderMin, syncborder));
      if (wposition < syncborder || wposition > wsize - syncborder) {
        
        /* If close to a border, do an image offset computation
           to see if the display would actually get moved if
           this axis were centered on point */
        trytrans = (int)((vi->xsize * 0.5f) - vi->xmouse + 0.5f);
        trydraws = zap->xdrawsize;
        tryborder = zap->xborder;
        trystart = zap->xstart;
        /* imodPrintStderr ("before %d %d %d %d\n", 
           trydraws, zap->xtrans, tryborder, trystart); */
        b3dSetImageOffset(wsize, vi->xsize, zap->zoom, trydraws,
                          trytrans, tryborder, trystart, 1);
        /* imodPrintStderr ("after %d %d %d %d\n", 
           trydraws, trytrans, tryborder, trystart); */
        /* Can't use xtrans for a test, need to use the other
           two values to see if change in display would occur */
        if (tryborder != zap->xborder || trystart != zap->xstart)
          tripshift += 1;
        
      }
      
      /* Same for Y axis */
      wsize = zap->numXpanels ? zap->panelYsize : zap->winy;
      wposition = zapYpos(zap, vi->ymouse);
      syncborder = (int)(wsize * BORDER_FRAC);
      syncborder = B3DMIN(BORDER_MAX, B3DMAX(borderMin, syncborder));
      if (wposition < syncborder || wposition > wsize - syncborder){
        trytrans = (int)((vi->ysize * 0.5f) - vi->ymouse + 0.5f);
        trydraws = zap->ydrawsize;
        tryborder = zap->yborder;
        trystart = zap->ystart;
        b3dSetImageOffset(wsize, vi->ysize, zap->zoom, trydraws,
                          trytrans, tryborder, trystart, 1);
        if (tryborder != zap->yborder || trystart != zap->ystart)
          tripshift += 2;
      }
    }
    
    if (tripshift) {
      /* imodPrintStderr("tripshift %d\n",tripshift); */
      zap->xtrans = (int)((vi->xsize * 0.5f) - vi->xmouse + 0.5f);
      zap->ytrans = (int)((vi->ysize * 0.5f) - vi->ymouse + 0.5f);
    }
  }
}

static B3dCIImage *getNewCIImage(ZapStruct *zap, B3dCIImage *image)
{
  B3dCIImage *newim;
  if (zap->ginit)
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
void zapResize(ZapStruct *zap, int winx, int winy)
{
  ivwControlPriority(zap->vi, zap->ctrl);

  if (imodDebug('z'))
    imodPrintStderr("RESIZE: ");

  if (imodDebug('z')) {
    imodPrintStderr("Size = %d x %d  heights win %d tool %d :", winx, winy,
                    zap->qtWindow->height(),zap->qtWindow->mToolBar->height());
    if (zap->ginit)
      imodPrintStderr("Old Size = %d x %d :", zap->winx, zap->winy);
  }

  zap->winx = winx;
  zap->winy = winy;
  b3dSetCurSize(winx, winy);
  b3dResizeViewportXY(winx, winy);

  if (zap->numXpanels) {
    zapSetupPanels(zap);
  } else {
    
    zap->image =  getNewCIImage(zap, zap->image);
    if (!zap->image)
      return;
    zap->recordSubarea = 1;
  }
  zap->ginit = 1;
  if (imodDebug('z'))
    imodPrintStderr("\n");
}

static void allocateToPanels(int num, int winSize, int gutter, int &panelSize,
                             int &border)
{
  int imarea = B3DMAX(0, winSize - (num - 1) * gutter);
  panelSize = imarea / num;
  border = (imarea % num) / 2;
}

int zapSetupPanels(ZapStruct *zap)
{
  int i, newnum = 0, retval = 0;
  int numtot = zap->numXpanels * zap->numYpanels;
  allocateToPanels(zap->numXpanels, zap->winx, zap->panelGutter, 
                   zap->panelXsize, zap->panelXborder);
  allocateToPanels(zap->numYpanels, zap->winy, zap->panelGutter, 
                   zap->panelYsize, zap->panelYborder);
  
  // If panels are too small, set them to 1 as a signal
  if (zap->panelXsize < 4 ||  zap->panelYsize < 4) {
    zap->panelXsize = 1;
    zap->panelYsize = 1;
    retval = 1;
  }

  // Set the minimum size regardless, so it won't get stuck at large value
  zap->gfx->setMinimumSize
    ((zap->numXpanels - 1) *  (zap->panelGutter + 5) + 5,
     (zap->numYpanels - 1) *  (zap->panelGutter + 5) + 5);

  // Allocate or resize the images, stop on a failure
  for (i = 0; i < numtot; i++) {
    zap->images[i] = getNewCIImage(zap, zap->images[i]);
    if (!zap->images[i]) {
      retval = 2;
      break;
    }
    newnum++;
  }
  
  // Clear out unused images
  for (i = newnum; i < zap->numImages; i++) {
    b3dFreeCIImage(zap->images[i]);
    zap->images[i] = NULL;
  }
  zap->numImages = newnum;
    
  return retval;
}

static void zapFlushImage(ZapStruct *zap)
{
  int ind;
  if (zap->numXpanels) {
    for (ind = 0; ind < zap->numImages; ind++)
      b3dFlushImage(zap->images[ind]);
  } else
    b3dFlushImage(zap->image);
}

/* 1/29/03: removed the resize and expose hack code and the report time code */

/*
 * This is the central drawing routine called from in the module or imodview
 */
void zapDraw(ZapStruct *zap)
{
  zap->gfx->updateGL();
}

/*
 * This receives the paint events generated by the window manager
 */
void zapPaint(ZapStruct *zap)
{
  int ob, ix, iy, ind, delInd, xborderSave, yborderSave, sectionSave;
  int panelX, panelY;
  QObjectList objList;
  if (imodDebug('z'))
    imodPrintStderr("Paint:");

  b3dSetCurSize(zap->winx, zap->winy);

  if (zap->numXpanels && zap->panelXsize < 4)
    return;

  zapAutoTranslate(zap);
  zap->drewExtraCursor = false;

  // If the current only flag is set, swap the displayed buffer into the
  // drawing buffer and just draw the current contour
  // Reset value drawing since it has not been set up for this object
  if (zap->drawCurrentOnly > 0) {
    if (App->doublebuffer)
      zap->gfx->swapBuffers();
    ifgResetValueSetup();
    ob = zap->vi->imod->cindex.object;
    imodSetObjectColor(ob); 
    b3dLineWidth(zap->vi->imod->obj[ob].linewidth2); 
    zapDrawContour(zap, zap->vi->imod->cindex.contour, ob);
    zap->drawCurrentOnly = -1;
    return;
  }
  
  if (zap->drawCurrentOnly < 0)
    setDrawCurrentOnly(zap, 0);

  /* DNM 1/29/03: no more skipping of further drawing */
  zapDrawGraphics(zap);

  if (zap->numXpanels) {

    // Multipanel draw: need to set borders and section for each panel
    xborderSave = zap->xborder;
    yborderSave = zap->yborder;
    sectionSave = zap->section;
    for (ix = 0; ix < zap->numXpanels; ix++) {
      for (iy = 0; iy < zap->numYpanels; iy++) {
        ind = ix + iy * zap->numXpanels;
        delInd = ind - (zap->numXpanels * zap->numYpanels - 1) / 2;
        zap->section = sectionSave + zap->panelZstep * delInd;
        if (zap->section < 0 || zap->section >= zap->vi->zsize)
          continue;
        if ((!delInd && !zap->drawInCenter) || (delInd && !zap->drawInOthers))
          continue;
        panelX = zap->panelXborder + ix * (zap->panelXsize + zap->panelGutter);
        zap->xborder = xborderSave + panelX;
        panelY = zap->panelYborder + iy * (zap->panelYsize + zap->panelGutter);
        zap->yborder = yborderSave + panelY;
        b3dSubareaViewport(panelX, panelY, zap->panelXsize, zap->panelYsize);
        zapDrawModel(zap);
        zapDrawCurrentPoint(zap);
        zapDrawExtraObject(zap);
      }
    }
    zap->xborder = xborderSave;
    zap->yborder = yborderSave;
    zap->section = sectionSave;
    b3dResizeViewportXY(zap->winx, zap->winy);
    
  } else {

    // Normal draw
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
  }
  zapDrawTools(zap);
  zap->scaleBarSize = scaleBarDraw(zap->winx, zap->winy, zap->zoom, 0);

  // Update graph windows if rubber band changed (this should be done by 
  // control but that is not possible, it doesn't know types)
  if (zap->bandChanged) {
    imodDialogManager.windowList(&objList, -1, GRAPH_WINDOW_TYPE);
    for (ob = 0; ob < objList.count(); ob++)
      ((GraphWindow *)objList.at(ob))->xgraphDraw();
    zap->bandChanged = 0;
  }

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
  if (!zap->popup)
    return;
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
      zapFlushImage(zap);
      zapSyncImage(zap);
      zapDraw(zap);
    }
    break;

  case ZAP_TOGGLE_CENTER:
    zap->keepcentered = state;
    if (state) {
      zapFlushImage(zap);
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
  if (!zap->popup)
    return;
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
    imodMovieXYZT(zap->vi, MOVIE_DEFAULT, MOVIE_DEFAULT, MOVIE_DEFAULT, 0);
    if (step > 0)
      inputNextTime(zap->vi);
    else
      inputPrevTime(zap->vi);
  }
}

/*****************************************************************************/
/*
 * Open the zap window and initialize structure
 */
int imod_zap_open(struct ViewInfo *vi, int wintype)
{
  ZapStruct *zap;
  QString str;
  QRect oldGeom;
  int needWinx, needWiny;
  int maxWinx;
  int maxWiny;
  int i, newWidth, newHeight, xleft, ytop, toolHeight;
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
  zap->showedSlice = 0;
  zap->timeLock = 0;
  zap->toolSection = -1;
  zap->toolMaxZ = vi->zsize;
  zap->toolZoom = -1.0f;
  zap->toolTime = 0;
  zap->toolSizeX = 0;
  zap->toolSizeY = 0;
  zap->twod = (!(vi->dim&4));
  zap->sectionStep = 0;
  zap->time = 0;
  zap->overlay = 0;
  zap->mousemode = 0;
  zap->lastShape = -1;
  zap->rubberband = 0;
  zap->startingBand = 0;
  zap->shiftingCont = 0;
  zap->bandChanged = 0;
  zap->shiftRegistered = 0;
  zap->centerMarked = 0;
  zap->movieSnapCount = 0;
  zap->drawCurrentOnly = 0;
  zap->dragAddCount = 0;
  zap->drewExtraCursor = false;
  zap->numXpanels = wintype ? 5 : 0;
  zap->numYpanels = 1;
  zap->panelZstep = 1;
  zap->drawInCenter = 1;
  zap->drawInOthers = 1;
  zap->panelGutter = 8;

  if (wintype) {
    zap->images = (B3dCIImage **)malloc
      (MULTIZ_MAX_PANELS * MULTIZ_MAX_PANELS * sizeof(B3dCIImage *));
    zap->numImages = 0;
    if (!zap->images) {
      free(zap);
      wprint("\aError getting memory for array of image pointers.\n");
      return(-1);
    }
    for (i = 0; i < MULTIZ_MAX_PANELS * MULTIZ_MAX_PANELS; i++)
      zap->images[i] = NULL;
    oldGeom = ImodPrefs->getMultiZparams(zap->numXpanels, zap->numYpanels,
                                         zap->panelZstep, zap->drawInCenter, 
                                         zap->drawInOthers);
  }

  utilGetLongestTimeString(zap->vi, &str);
  zap->qtWindow = new ZapWindow(zap, str, wintype != 0, App->rgba, 
                                App->doublebuffer, App->qtEnableDepth, 
                                imodDialogManager.parent(IMOD_IMAGE),
                                "zap window");
  if (!zap->qtWindow){
    free(zap);
    wprint("\aError opening zap window.\n");
    return(-1);
  }
  if (imodDebug('z'))
    imodPuts("Got a zap window");

  zap->gfx = zap->qtWindow->mGLw;
  if (!App->rgba)
    zap->gfx->setColormap(*(App->qColormap));

  zap->qtWindow->setWindowTitle
    (imodCaption((char *)(wintype ? "3dmod Multi-Z Window" : 
                          "3dmod ZaP Window")));

  zap->qtWindow->mToolBar->setWindowTitle(imodCaption("ZaP Toolbar"));
  if (zap->qtWindow->mToolBar2)
    zap->qtWindow->mToolBar2->setWindowTitle(imodCaption("Time Toolbar"));
  if (zap->qtWindow->mPanelBar)
    zap->qtWindow->mPanelBar->setWindowTitle(imodCaption("Multi-Z Toolbar"));
  
  zap->ctrl = ivwNewControl(vi, zapDraw_cb, zapClose_cb, zapKey_cb,
                            (void *)zap);
  imodDialogManager.add((QWidget *)zap->qtWindow, IMOD_IMAGE, 
                        wintype ? MULTIZ_WINDOW_TYPE : ZAP_WINDOW_TYPE);

  if (!wintype) {
    diaMaximumWindowSize(maxWinx, maxWiny);
    zap->qtWindow->setSizeText(maxWinx, maxWiny);

    oldGeom = ImodPrefs->getZapGeometry();
  }

  /* 1/28/03: this call is needed to get the toolbar size hint right */
  imod_info_input();
  QSize toolSize = zap->qtWindow->mToolBar->sizeHint();
  QSize toolSize2(0, 0);
  QSize toolSize3(0, 0);
  if (zap->qtWindow->mToolBar2)
    toolSize2 = zap->qtWindow->mToolBar2->sizeHint();
  if (wintype)
    toolSize3 = zap->qtWindow->mPanelBar->sizeHint();
  toolHeight = toolSize.height();
  if (imodDebug('z'))
    imodPrintStderr("Toolsize %d %d win %d gfx %d\n", toolSize.width(), 
                    toolSize.height(), zap->qtWindow->height(), 
                    zap->gfx->height());
  if (!oldGeom.width()) {

    if (!wintype) {
      // If no old geometry, adjust zoom if necessary to fit image on screen

      for (i = 0; i < 2; i++) {
        zap->zoom = 1.;
        while (zap->zoom * vi->xsize > 1.1 * maxWinx || 
               zap->zoom * vi->ysize > 1.1 * maxWiny - toolHeight) {
          newZoom = b3dStepPixelZoom(zap->zoom, -1);
          if (fabs(newZoom - zap->zoom) < 0.0001)
            break;
          zap->zoom = (float)newZoom;
        }
      
        needWinx = (int)(zap->zoom * vi->xsize);
      
        // If Window is narrower than two toolbars, set up to stack the
        // toolbars and increase the tool height
        if (!i && needWinx < toolSize.width() + toolSize2.width()) {
          zap->qtWindow->insertToolBarBreak(zap->qtWindow->mToolBar2);
          toolHeight += toolSize2.height();
        } 
      }
      
      needWiny = (int)(zap->zoom * vi->ysize) + toolHeight;
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
        zap->qtWindow->insertToolBarBreak(zap->qtWindow->mPanelBar);
      }
    }

    QRect pos = zap->qtWindow->geometry();
    xleft = pos.x();
    ytop = pos.y();

    diaLimitWindowPos(newWidth, newHeight, xleft, ytop);
    if (imodDebug('z'))
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
    diaLimitWindowSize(newWidth, newHeight);
    diaLimitWindowPos(newWidth, newHeight, xleft, ytop);

    // Adjust the tool height: see if time bar fits on line and insert break
    // if not and add to height
    int toolBase = toolSize.width();
    if (zap->qtWindow->mToolBar2) {
      if (newWidth < toolBase + toolSize2.width()) {
        zap->qtWindow->insertToolBarBreak(zap->qtWindow->mToolBar2);
        toolBase = toolSize2.width();
        toolHeight += toolSize2.height();
      } else
        toolBase += toolSize2.width();
    }

    // Then see if panel bar fits on line, insert break if not, add to height
    if (wintype && (newWidth < toolBase + toolSize3.width())) {
      zap->qtWindow->insertToolBarBreak(zap->qtWindow->mPanelBar);
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
  }

  // 9/23/03: changed setGeometry to resize/move and this allowed elimination
  // of the setting again on the first real draw
  zap->qtWindow->resize(newWidth, newHeight);
  zap->qtWindow->move(xleft, ytop);
  zap->xzoom = zap->zoom;
  zap->qtWindow->show();
  zap->popup = 1;
  setMouseTracking(zap);

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

/*
 * External key is passed on
 */
static void zapKey_cb(ImodView *vi, void *client, int released, QKeyEvent *e)
{
  ZapStruct *zap = (ZapStruct *)client;
  if ((e->modifiers() & Qt::KeypadModifier) && (e->key() == Qt::Key_Insert ||
                                    e->key() == Qt::Key_0))
    return;
  if (released)
    zapKeyRelease(zap, e);
  else
    zapKeyInput(zap, e);
}


/* static QTime downtime; */

/*
 * Respond to a key press
 */
void zapKeyInput(ZapStruct *zap, QKeyEvent *event)
{
  struct ViewInfo *vi = zap->vi;
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
    if (!keypad && imod->mousemode != IMOD_MMOVIE ||
        keypad && imod->mousemode == IMOD_MMOVIE) {
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
          zapTranslate(zap, trans, -trans);
        else
          inputPointMove(vi, 0, 0, -1);
      } else {
        if (imod->mousemode == IMOD_MMOVIE)
          zapTranslate(zap, trans, trans);
        else
          inputPointMove(vi, 0, 0, 1);
      }
      handled = 1;

      // with regular keys, handle specially if locked
    } else if (!keypad && zap->lock == 2){
      if (shifted) {
        obj = imodObjectGet(imod);
        zap->section = utilNextSecWithCont(vi, obj, zap->section, 
                                           keysym == Qt::Key_PageUp ? 1 : -1);
      } else {
        if (keysym == Qt::Key_PageDown)
          zap->section--;
        else
          zap->section++;
      }
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

  case Qt::Key_At:
  case Qt::Key_Exclam:
    if (zap->timeLock) {
      imcGetStartEnd(vi, 3, &start, &end);
      zap->timeLock = (keysym == Qt::Key_At) ? end + 1 : start + 1;
      zapDraw(zap);
      handled = 1;
    }
    break;


  case Qt::Key_Home:
    if (keypad && imod->mousemode == IMOD_MMOVIE) {
      zapTranslate(zap, -trans, trans);
      handled = 1;
    }
    break;

  case Qt::Key_End:
    if (keypad && imod->mousemode == IMOD_MMOVIE) {
      zapTranslate(zap, -trans, -trans);
      handled = 1;
    }
    break;
          
  case Qt::Key_Minus:
    zap->zoom = b3dStepPixelZoom(zap->zoom, -1);
    zapDraw(zap);
    handled = 1;
    break;

  case Qt::Key_Plus:
  case Qt::Key_Equal:
    zap->zoom = b3dStepPixelZoom(zap->zoom, 1);
    zapDraw(zap);
    handled = 1;
    break;
          
    /* DNM: Keypad Insert key, alternative to middle mouse button */
  case Qt::Key_Insert:

    /* But skip out if in movie mode or already active */
    if (!keypad || imod->mousemode == IMOD_MMOVIE || insertDown || 
        zap->numXpanels)
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
    if (zap->numXpanels)
      break;
    if (ctrl) {

      // Select all contours in current object on section or in rubberband
      if (zap->rubberband) {
        selmin.x = zap->rbImageX0;
        selmax.x = zap->rbImageX1;
        selmin.y = zap->rbImageY0;
        selmax.y = zap->rbImageY1;
      } else {
        selmin.x = -vi->xsize;;
        selmax.x = 2 * vi->xsize;
        selmin.y = -vi->ysize;;
        selmax.y = 2 * vi->ysize;
      }
      selmin.z = zap->section - 0.5;
      selmax.z = zap->section + 0.5;

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
    if (shifted || zap->numXpanels)
      break;
    autox_smooth(vi->ax);
    handled = 1;
    break;

  case Qt::Key_B:
    if (zap->numXpanels || ctrl)
      break;
    if (shifted) { 
      zapToggleRubberband(zap);
    } else
      autox_build(vi->ax);
    handled = 1;
    break;
          
  case Qt::Key_P:
    if (zap->numXpanels)
      break;
    if (shifted) {
      zapToggleContourShift(zap);
      handled = 1;
    }
    break;

  case Qt::Key_S:
    if (shifted || ctrl){
      zap->showslice = zap->showedSlice;

      // Take a montage snapshot if selected and no rubberband is on
      if (!zap->rubberband && imcGetSnapMontage(true) && !zap->numXpanels) {
        montageSnapshot(zap, (ctrl ? 1 : 0) + (shifted ? 2 : 0));
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
      b3dKeySnapshot((char *)(zap->numXpanels ? "multiz" : "zap"), shifted, 
                     ctrl, limits);
    }else
      inputSaveModel(vi);
    handled = 1;
    break;
          
  case Qt::Key_Escape:
    // For cocoa/Qt 4.5.0, need to prevent enter/leave events
    zap->popup = 0;
    zap->qtWindow->close();
    handled = 1;
    break;

  case Qt::Key_R:
    if (shifted && !zap->numXpanels) {
      zapResizeToFit(zap);
      handled = 1;
    }
    break;

  case Qt::Key_Z:
    if (shifted) {
      if (!zap->numXpanels) { 
        if(zap->sectionStep) {
          zap->sectionStep = 0;
          wprint("Auto-section advance turned OFF\n");
        } else {
          zap->sectionStep = 1;
          wprint("\aAuto-section advance turned ON\n");
        }
      }
      handled = 1;
    }
    break;

  case Qt::Key_I:
    if (zap->numXpanels)
      break;
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
      if (event->modifiers() & ShiftButton)
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
void zapKeyRelease(ZapStruct *zap, QKeyEvent *event)
{
  /*  imodPrintStderr ("%d down\n", downtime.elapsed()); */
  if (!insertDown || !(event->modifiers() & Qt::KeypadModifier) ||
      (event->key() != Qt::Key_Insert && event->key() != Qt::Key_0))
    return;
  insertDown = 0;
  registerDragAdditions(zap);
  setMouseTracking(zap);
  zap->qtWindow->releaseKeyboard();
  zap->gfx->releaseMouse();

  // Note that unless the user truns off autorepeat on the key, there is a
  // series of key press - release events and it does full draws
  if (zap->drawCurrentOnly) {
    setDrawCurrentOnly(zap, 0);
    imodDraw(zap->vi, IMOD_DRAW_MOD | IMOD_DRAW_XYZ);
  }
}

// Pass on various events to plugins
void zapGeneralEvent(ZapStruct *zap, QEvent *e)
{
  int ix, iy, ifdraw, iz, pt;
  float imx, imy;
  Iobj *obj;
  Icont *cont;
  float wheelScale = 1./1200.f;
  if (zap->numXpanels || !zap->popup || App->closing)
    return;
  ix = (zap->gfx->mapFromGlobal(QCursor::pos())).x();
  iy = (zap->gfx->mapFromGlobal(QCursor::pos())).y();
  zapGetixy(zap, ix, iy, &imx, &imy, &iz);
  ifdraw = imodPlugHandleEvent(zap->vi, e, imx, imy);
  if (ifdraw & 2 || (zap->drewExtraCursor && e->type() == QEvent::Leave))
    zapDraw(zap);
  if (ifdraw)
    return;
  if (e->type() == QEvent::Wheel && iceGetWheelForSize()) {
    imodGetIndex(zap->vi->imod, &ix, &iy, &pt);
    obj = imodObjectGet(zap->vi->imod);
    cont = imodContourGet(zap->vi->imod);
    if (!cont || pt < 0)
      return;
    imx = imodPointGetSize(obj, cont, pt);
    if (!imx && (!cont->sizes || (cont->sizes && cont->sizes[pt] < 0)))
      return;
    if (zap->zoom < 4. && zap->zoom >= 2.)
      wheelScale *= 2.;
    else if (zap->zoom < 2. && zap->zoom > 1.)
      wheelScale *= 3.;
    else if (zap->zoom == 1.)
      wheelScale *= 4.;
    else if (zap->zoom < 1.)
      wheelScale *= 5.;
    imx += ((QWheelEvent *)e)->delta() * wheelScale;
    imx = B3DMAX(0., imx);
    imodPointSetSize(cont, pt, imx);
    imodDraw(zap->vi, IMOD_DRAW_MOD);
  }
}

/*
 * respond to a mouse press event
 */
void zapMousePress(ZapStruct *zap, QMouseEvent *event)
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
  zap->lmx = x;
  zap->lmy = y;
  mousePressed = true;
  utilRaiseIfNeeded(zap->qtWindow, event);

  if (imodDebug('m'))
    imodPrintStderr("click at %d %d   buttons %d %d %d\n", x, y, button1, 
                    button2, button3);

  // Check for starting a band move before offering to plugin
  if (event->button() == ImodPrefs->actualButton(2) && !button1 && !button3) {
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
  }  
  zapSetCursor(zap, zap->mousemode, utilNeedToSetCursor());

  // Now give the plugins a crack at it
  ifdraw = checkPlugUseMouse(zap, event, button1, button2, button3);
  if (ifdraw & 1) {
    if (ifdraw & 2)
      zapDraw(zap);
    return;
  }

  // Check for regular actions
  if (event->button() == ImodPrefs->actualButton(1)) {
    if (zap->shiftingCont)
      drew = startShiftingContour(zap, firstmx, firstmy, 1, ctrlDown);
    else if (zap->startingBand)
      drew = zapButton1(zap, firstmx, firstmy, ctrlDown);
    else
      firstdrag = 1;
      
  } else if (event->button() == ImodPrefs->actualButton(2) &&
	     !button1 && !button3) {
    if (zap->shiftingCont)
      drew = startShiftingContour(zap, x, y, 2, ctrlDown);
    else
      drew = zapButton2(zap, x, y, ctrlDown);

  } else if (event->button() == ImodPrefs->actualButton(3) &&
	     !button1 && !button2) {
    if (zap->shiftingCont)
      drew = startShiftingContour(zap, x, y, 3, ctrlDown);
    else
      drew = zapButton3(zap, x, y, ctrlDown);
  }
  if (ifdraw && !drew)
    zapDraw(zap);
}

/*
 * respond to mouse release event
 */
void zapMouseRelease(ZapStruct *zap, QMouseEvent *event)
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
  if (zap->shiftRegistered) {
    zap->shiftRegistered = 0;
    zap->vi->undo->finishUnit();
  }
  needDraw = zap->drewExtraCursor && !zap->gfx->extraCursorInWindow();

  ifdraw = checkPlugUseMouse(zap, event, button1, button2, button3);
  if (ifdraw & 1) {
    if ((ifdraw & 2) || needDraw)
      zapDraw(zap);

    // Defer the return so the band moving can be turned off, but then only 
    // check other things below if this flag is off
  }

  if (button1 && !(ifdraw & 1)) {
    if (dragband) {
      dragband = 0;
      zapSetCursor(zap, zap->mousemode);
    }
    firstdrag = 0;

    if (but1downt.elapsed() > 250) {
      if (zap->hqgfxsave || ifdraw)
        zapDraw(zap);
      zap->hqgfxsave  = 0;
      return;    //IS THIS RIGHT?
    }
    drew = zapButton1(zap, event->x(), event->y(),
               event->modifiers() & Qt::ControlModifier);
  }
 
  // Button 2 and band moving, release the band
  if (button2 && zap->rubberband && moveband) {
    moveband = 0;
    zapSetCursor(zap, zap->mousemode);
    if (zap->hqgfxsave) {
      zapDraw(zap);
      drew = 1;
    }
    zap->hqgfxsave  = 0;

    // Button 2 and doing a drag draw - draw for real.
  } else if (button2 && !zap->numXpanels && !(ifdraw & 1) &&
             zap->vi->imod->mousemode == IMOD_MMODEL) {
    if (imodDebug('z'))
      imodPrintStderr("Down time %d msec\n", but1downt.elapsed());

    registerDragAdditions(zap);

    // Fix the mouse position and update the other windows finally
    // Why call imod_info_setxyz again on release of single point add?
    zapGetixy(zap, event->x(), event->y(), &imx, &imy, &imz);
    zap->vi->xmouse = imx;
    zap->vi->ymouse = imy;
    if (zap->drawCurrentOnly) {
      setDrawCurrentOnly(zap, 0);
      imodDraw(zap->vi, IMOD_DRAW_MOD | IMOD_DRAW_XYZ);
      drew = 1;
    } else
      imod_info_setxyz();
  }

  // Now return if plugin said to
  if (ifdraw & 1)
    return;

  if (zap->centerMarked && !zap->centerDefined) {
    ivwClearAnExtraObject(zap->vi, zap->shiftObjNum);
    imodDraw(zap->vi, IMOD_DRAW_MOD);
    zap->centerMarked = 0;
    drew = 1;
  }
  if ((ifdraw || needDraw) && !drew)
    zapDraw(zap);
  else
    setAreaLimits(zap);
}

/*
 * Respond to a mouse move event (mouse down)
 */
void zapMouseMove(ZapStruct *zap, QMouseEvent *event)
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
    zapGetixy(zap, ex, ey, &imx, &imy, &imz);
    pvNewMousePosition(zap->vi, imx, imy, imz);
  }

  if (!(zap->rubberband && moveband)  && 
      (mousePressed || insertDown || zap->vi->trackMouseForPlugs)) {
    ifdraw = checkPlugUseMouse(zap, event, button1, button2, button3);
    if (ifdraw & 1) {
      if (ifdraw & 2)
        zapDraw(zap);
      return;
    }
  } else
    setControlAndLimits(zap);

  if (!(mousePressed || insertDown)) {
    if (zap->rubberband)
      zapAnalyzeBandEdge(zap, ex, ey);
    if (ifdraw)
      zapDraw(zap);
    return;
  }

  // For first button or band moving, eat any pending move events and use 
  // latest position
  if ( (button1 && !button2 && !button3) || (zap->rubberband && moveband)) {
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
      drew = dragSelectContsCrossed(zap, ex, ey);
    } else {
      /* DNM: wait for a bit of time or until enough distance moved, but if we
         do not replace original lmx, lmy, there is a disconcerting lurch */
      if ((but1downt.elapsed()) > 250 || cumdx * cumdx + cumdy * cumdy >
          cumthresh)
        drew = zapB1Drag(zap, ex, ey);
    }
  }

  // DNM 8/1/08: Reject small movements soon after the button press
  if ( (!button1) && (button2) && (!button3)) {
    if ((but1downt.elapsed()) > 150 || cumdx * cumdx + cumdy * cumdy > 
        dragthresh)
      drew = zapB2Drag(zap, ex, ey, ctrlDown);
  }
  
  if ( (!button1) && (!button2) && (button3))
    drew = zapB3Drag(zap, ex, ey, ctrlDown, shiftDown);
  
  zap->lmx = ex;
  zap->lmy = ey;
  if (ifdraw && !drew)
    zapDraw(zap);
}

/*
 * Test for whether a plugin takes care of the mouse event; take care of 
 * setting limits also
 */
static int checkPlugUseMouse(ZapStruct *zap, QMouseEvent *event, int but1, 
                          int but2, int but3)
{
  float imx, imy;
  int ifdraw, imz;
  setControlAndLimits(zap);
  if (zap->numXpanels)
    return 0;
  ivwControlActive(zap->vi, 0);
  zapGetixy(zap, event->x(), event->y(), &imx, &imy, &imz);
  ifdraw = imodPlugHandleMouse(zap->vi, event, imx, imy, but1, but2, but3);
  if (ifdraw & 1) {
    if (ifdraw & 2) 
      zapDraw(zap);
  } else
    ivwControlActive(zap->vi, 1);
  return ifdraw;
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

/*
 * Adjust minimum size of rubberband down in case image is tiny
 */
static int zapBandMinimum(ZapStruct *zap)
{
  int bandmin = B3DMIN(4, (int)(zap->vi->xsize * zap->xzoom) + 2);
  bandmin = B3DMIN(bandmin, (int)(zap->vi->ysize * zap->zoom) + 2);
  return bandmin;
}

/*
 * First mouse button click: Attach to nearest point in model mode,
 * or just modify the current xmouse, ymouse values
 */
int zapButton1(ZapStruct *zap, int x, int y, int controlDown)
{
  ImodView *vi   = zap->vi;
  Imod     *imod = vi->imod;
  Ipoint pnt;
  Iindex index, indSave;
  int bandmin = zapBandMinimum(zap);
  int iz;
  float distance;
  float ix, iy, dx, dy;
  float selsize = IMOD_SELSIZE / zap->zoom;

  zapGetixy(zap, x, y, &ix, &iy, &iz);
  if (iz < 0)
    return 0;
     
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
    zap->bandChanged = 1;
    zapSetCursor(zap, zap->mousemode);
    zapDraw(zap);
    return 1;  
  }

  if (vi->ax && !zap->numXpanels)
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      autox_fillmouse(vi, (int)ix, (int)iy);
      return 1;
    }
     
  // In either mode, do a default modification of zmouse or zap->section
  vi->xmouse = ix;
  vi->ymouse = iy;
  if (zap->lock)
    zap->section = iz;
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
int zapButton2(ZapStruct *zap, int x, int y, int controlDown)
{
  ImodView *vi = zap->vi;
  Iobj  *obj;
  Icont *cont;
  Ipoint point;
  int   pt;
  float ix, iy;
  float lastz;
  int cz, pz, iz;

  zapGetixy(zap, x, y, &ix, &iy, &iz);

  if (vi->ax && !zap->numXpanels){
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      /* DNM 2/1/01: need to call with int */
      autox_sethigh(vi, (int)ix, (int)iy);
      return 1;
    }
  }
     
  // 3/5/08: Moved band moving to mouse press routine so it would happen before
  // plugin action

  if (vi->imod->mousemode == IMOD_MMODEL) {
    if (zap->numXpanels)
      return 0;
    zap->dragAddCount = 0;
    obj = imodObjectGet(vi->imod);
    if (!obj)
      return 0;

    // Get current contour; if there is none, start a new one
    // DNM 7/10/04: switch to calling routine; it now fixes time of empty cont
    cont = ivwGetOrMakeContour(vi, obj, zap->timeLock);
    if (!cont)
      return 0;

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
      if ((iobjPlanar(obj->flags) && !(cont->flags & ICONT_WILD) && cz != pz) 
          || ivwTimeMismatch(vi, zap->timeLock, obj, cont)) {
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
    if (ivwTimeMismatch(vi, zap->timeLock, obj, cont)) {
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
      
    return 1;
  }
  return startMovieCheckSnap(zap, 1);
}

/*
 * Delete all points of current contour under the cursor 
 */     
static int zapDelUnderCursor(ZapStruct *zap, int x, int y, Icont *cont)
{
  float ix, iy;
  float crit = 8./ zap->zoom;
  float critsq, dsq;
  int i, iz;
  Ipoint *lpt;
  int deleted = 0;

  zapGetixy(zap, x, y, &ix, &iy, &iz);
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
    return 0;
  zap->vi->undo->finishUnit();
  imodDraw(zap->vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD );
  return 1;
}

/* 
 * Third mouse button click:
 * In model mode, modify current point; otherwise run movie 
 */
int zapButton3(ZapStruct *zap, int x, int y, int controlDown)
{
  ImodView *vi = zap->vi;
  Icont *cont;
  Iobj *obj;
  int   pt, iz;
  float ix, iy;

  zapGetixy(zap, x, y, &ix, &iy, &iz);

  if (vi->ax && !zap->numXpanels) {
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      /* DNM 2/1/01: need to call with int */
      autox_setlow(vi, (int)ix, (int)iy);
      return 1;
    }
  }

  if (vi->imod->mousemode == IMOD_MMODEL) {
    if (zap->numXpanels)
      return 0;
    cont = imodContourGet(vi->imod);
    pt   = vi->imod->cindex.point;
    if (!cont)
      return 0;
    if (pt < 0)
      return 0; 

    obj = imodObjectGet(vi->imod);
    if (ivwTimeMismatch(vi, zap->timeLock, obj, cont))
      return 0;

    /* If the control key is down, delete points under the cursor */
    if (controlDown)
      return zapDelUnderCursor(zap, x, y, cont);

          
    if (!zapPointVisable(zap, &(cont->pts[pt])))
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
  return startMovieCheckSnap(zap, -1);
}

/*
 * First mouse button drag
 */
int zapB1Drag(ZapStruct *zap, int x, int y)
{

  // For zooms less than one, move image along with mouse; for higher zooms,
  // Translate 1 image pixel per mouse pixel (accelerated)
  double transFac = zap->zoom < 1. ? 1. / zap->zoom : 1.;

  if (zap->shiftingCont) {
    shiftContour(zap, x, y, 1, 0);
    return 1;
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
    zap->bandChanged = 1;
    zapSetCursor(zap, zap->mousemode, utilNeedToSetCursor());
  } else {
    /* Move the image */
    if (imodDebug('m'))
      imodPrintStderr("B1Drag: x,y %d,%d  lmx,y %d,%d  trans %d,%d\n",
                      x, y, zap->lmx,  zap->lmy,  zap->xtrans,  zap->ytrans);
    zap->xtrans += (int)floor(transFac * (x - zap->lmx) + 0.5);
    zap->ytrans -= (int)floor(transFac * (y - zap->lmy) + 0.5);
  }

  zap->hqgfxsave = zap->hqgfx;
  zap->hqgfx = 0;
  zapDraw(zap);
  zap->hqgfx = zap->hqgfxsave;
  return 1;
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


/*
 * Select contours in the current object crossed by a mouse move 
 */
static int dragSelectContsCrossed(struct zapwin *zap, int x, int y)
{
  ImodView *vi = zap->vi;
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
  zapGetixy(zap, x, y, &pnt2.x, &pnt2.y, &iz2);
  zapGetixy(zap, zap->lmx, zap->lmy, &pnt1.x, &pnt1.y, &iz1);
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
            imodDraw(zap->vi, IMOD_DRAW_RETHINK | IMOD_DRAW_XYZ);
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
int zapB2Drag(ZapStruct *zap, int x, int y, int controlDown)
{
  ImodView *vi = zap->vi;
  Iobj *obj;
  Icont *cont;
  Ipoint *lpt, cpt;
  float ix, iy, idx, idy;
  double dist;
  int pt, iz;
     
  if (zap->numXpanels)
    return 0;

  if (zap->shiftingCont) {
    shiftContour(zap, x, y, 2, 0);
    return 1;
  }

  if (vi->ax){
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      zapGetixy(zap, x, y, &ix, &iy, &iz);
      /* DNM 2/1/01: need to call with int */
      autox_sethigh(vi, (int)ix, (int)iy);
      return 1;
    }
  }

  if (zap->rubberband && moveband) {
    /* Moving rubber band: get desired move and constrain it to keep
       band in the image */
    idx = (x - zap->lmx) / zap->xzoom;
    idy = (zap->lmy - y) / zap->zoom;
    shiftRubberband(zap, idx, idy);

    zap->hqgfxsave = zap->hqgfx;
    zap->hqgfx = 0;
    zapDraw(zap);
    zap->hqgfx = zap->hqgfxsave;
    zap->bandChanged = 1;
    return 1;
  }

  if (vi->imod->mousemode == IMOD_MMOVIE)
    return 0;

  if (vi->imod->cindex.point < 0)
    return 0;

  zapGetixy(zap, x, y, &ix, &iy, &iz);

  cpt.x = ix;
  cpt.y = iy;
  cpt.z = zap->section;
     
  obj = imodObjectGet(vi->imod);
  if (!obj)
    return 0;

  cont = imodContourGet(vi->imod);
  if (!cont)
    return 0;

  lpt = &(cont->pts[vi->imod->cindex.point]);
  if (zap->twod)
    cpt.z = lpt->z;

  dist = imodel_point_dist( lpt, &cpt);

  /* DNM 6/18/03: If Z or time has changed, treat it like a button click so
     new contour can be started */
  // DNM 6/30/04: change to start new for any kind of contour with time change
  // DNM 7/15/08: Start new contour if up to the point limit too
  if ((iobjPlanar(obj->flags) && !(cont->flags & ICONT_WILD) && 
      (int)floor(lpt->z + 0.5) != (int)cpt.z) ||
      ivwTimeMismatch(vi, zap->timeLock, obj, cont) || 
      (obj->extra[IOBJ_EX_PNT_LIMIT] &&
       cont->psize >= obj->extra[IOBJ_EX_PNT_LIMIT])) {
    registerDragAdditions(zap);
    return zapButton2(zap, x, y, 0);
  }

  if (ivwTimeMismatch(vi, zap->timeLock, obj, cont))
    return 0;

  if ( dist > scaleModelRes(vi->imod->res, zap->zoom)){

    // Set insertion index to next point, or to current if drawing backwards
    pt = vi->imod->cindex.point + 1;
    if (pt > 0 && zap->insertmode)
      pt--;

    // Set flag for drawing current contour only if at end and going forward
    if (!zap->insertmode && pt == cont->psize)
      setDrawCurrentOnly(zap, 1);

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
int zapB3Drag(ZapStruct *zap, int x, int y, int controlDown, int shiftDown)
{
  ImodView *vi = zap->vi;
  Iobj *obj;
  Icont *cont;
  Ipoint *lpt;
  Ipoint pt;
  float ix, iy;
  int iz;

  if (zap->numXpanels)
    return 0;

  if (zap->shiftingCont) {
    shiftContour(zap, x, y, 3, shiftDown);
    return 1;
  }

  if (vi->ax){
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
      zapGetixy(zap, x, y, &ix, &iy, &iz);
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

  if (ivwTimeMismatch(vi, zap->timeLock, obj, cont))
    return 0;

  if (controlDown)
    return zapDelUnderCursor(zap, x, y, cont);

  if (vi->imod->cindex.point == (cont->psize - 1))
    return 0;

  /* DNM 11/13/02: need to test for both next and current points to prevent
     strange moves between sections */
  if (!zapPointVisable(zap, &(cont->pts[vi->imod->cindex.point + 1])) ||
      !zapPointVisable(zap, &(cont->pts[vi->imod->cindex.point])))
    return 0;

  lpt = &(cont->pts[vi->imod->cindex.point]);
  zapGetixy(zap, x, y, &(pt.x), &(pt.y), &iz);
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
    return 1;
  }
  return 0;
}

/*
 * Register accumulated additions for undo
 */
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

/*
 * Single routine to toggle shift for contour move window to call
 */
void zapToggleContourShift(ZapStruct *zap)
{
  if (zap->shiftingCont)
    endContourShift(zap);
  else
    setupContourShift(zap);
}

/*
 * Turn off contour shifting and reset mouse
 */
static void endContourShift(ZapStruct *zap)
{
  if (!zap->shiftingCont)
    return;
  zap->shiftingCont = 0;
  ivwFreeExtraObject(zap->vi, zap->shiftObjNum);
  if (zap->centerMarked)
    imodDraw(zap->vi, IMOD_DRAW_MOD);
  zapSetCursor(zap, zap->mousemode);
}

/*
 * Check whether contour shifting is OK and return current contour
 */
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

/*
 * Initiate contour shifting                             
 */
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
  zap->shiftObjNum = ivwGetFreeExtraObjectNumber(zap->vi);
  zapSetCursor(zap, zap->mousemode);
}

// Keep track of the base position for contour shifting
static Ipoint contShiftBase;

/*
 * Start the actual shift once the mouse goes down
 */
static int startShiftingContour(ZapStruct *zap, int x, int y, int button,
                                 int ctrlDown)
{
  int   pt, err, co, ob, curco, curob, iz;
  float ix, iy, area, areaSum;
  Ipoint cent, centSum;
  Imod *imod = zap->vi->imod;
  Iobj *obj;
  Icont *cont = checkContourShift(zap, pt, err);
  if (err)
    return 0;

  zapGetixy(zap, x, y, &ix, &iy, &iz);

  // If button for marking center, save coordinates, set flag, show mark
  if (button == 2 && ctrlDown) {
    zap->xformCenter.x = ix;
    zap->xformCenter.y = iy;
    zap->centerDefined = 1;
    markXformCenter(zap, ix, iy);
    return 1;
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
        return 0;
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
    return 1;
  }
  return 0;
}

/*
 * Shift or transform contour upon mouse move
 */
static void shiftContour(ZapStruct *zap, int x, int y, int button, 
                         int shiftDown)
{
  int   pt, err, ob, co, curco, curob, iz;
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
    zapGetixy(zap, x, y, &ix, &iy, &iz);
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

/*
 * Compute transform matrix from the mouse move
 */
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

/*
 * Mark the center of transformation with a star in extra object
 */
static void markXformCenter(ZapStruct *zap, float ix, float iy)
{
  int i;
  float starEnd[6] = {0., 8., 7., 4., 7., -4.};
  Iobj *obj = ivwGetAnExtraObject(zap->vi, zap->shiftObjNum);
  Icont *cont;
  Ipoint tpt;

  if (!obj)
    return;

  // Clear out the object and set to yellow non scattered
  ivwClearAnExtraObject(zap->vi, zap->shiftObjNum);
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
    free(cont);
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

/* returns image coords in x,y, z, given mouse coords mx, my */
static void zapGetixy(ZapStruct *zap, int mx, int my, float *x, float *y, 
                      int *z)
{
  int indx, indy, indp, indmid;

  // 10/31/04: winy - 1 maps to 0, not winy, so need a -1 here
  my = zap->winy - 1 - my;

  if (!zap->numXpanels) {
    *z = zap->section;
  } else {
    panelIndexAndCoord(zap->panelXsize, zap->numXpanels, zap->panelGutter, 
                       zap->panelXborder, mx, indx);
    panelIndexAndCoord(zap->panelYsize, zap->numYpanels, zap->panelGutter, 
                       zap->panelYborder, my, indy);
    if (indx < 0 || indy < 0) {
      *z = -1;
    } else {
      indp = indx + indy * zap->numXpanels;
      indmid = (zap->numXpanels * zap->numYpanels - 1) / 2;
      *z = zap->section + (indp - indmid) * zap->panelZstep;
      if (*z < 0 || *z >= zap->vi->zsize)
        *z = -1;
    }
  }
  *x = ((float)(mx - zap->xborder) / zap->xzoom) + (float)zap->xstart;
  *y = ((float)(my - zap->yborder) / zap->zoom) + (float)zap->ystart;
}

// Determine which panel a point is in for one direction and get the position
// within that panel
static void panelIndexAndCoord(int size, int num, int gutter, int border, 
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

/*
 * Convert rubberband window coordinates to inside image coordinates, with
 * optional clipping to image limits
 */
static void zapBandMouseToImage(ZapStruct *zap, int ifclip)
{
  int iz;
  zapGetixy(zap, zap->rbMouseX0 + 1, zap->rbMouseY1 - 1, &zap->rbImageX0, 
            &zap->rbImageY0, &iz);
  zapGetixy(zap, zap->rbMouseX1, zap->rbMouseY0, &zap->rbImageX1, 
            &zap->rbImageY1, &iz);
  
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

/*
 * Return the correct low and high section values from the zap parameter.
 * Returns true if the rubberband is in use and at least of the two values have
 * been set.
 */
static bool getLowHighSection(ZapStruct *zap, int &lowSection, int &highSection)
{
  bool lowHighSectionSet = true;
  // If rubberband is not enabled, set both ints to the current section
  // (original functionality) and return false.  This is probably not necessary
  // because the sections values are reset when the rubberband is turned off.
  if (zap->rubberband + zap->startingBand == 0) {
    lowHighSectionSet = false;
    lowSection = zap->section + 1;
    highSection = zap->section + 1;
  }
  else {
    QString low = zap->qtWindow->lowSection();
    QString high = zap->qtWindow->highSection();
    // If neither of the section values have been set, fall back to the original
    // functionality and return false.
    if (low.isEmpty() && high.isEmpty()) {
      lowHighSectionSet = false;
      lowSection = zap->section + 1;
      highSection = zap->section + 1;
    }
    else {
      lowSection = low.toInt();
      highSection = high.toInt();
      // If only one of the section values have been set, set the other one to
      // the max/min.
      if (low.isEmpty()) {
        lowSection = 1;
      } else if (high.isEmpty()) {
          highSection = zap->vi->zsize;
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
QString zapPrintInfo(ZapStruct *zap, bool toInfoWindow)
{
  float xl, xr, yb, yt;
  int ixl, ixr, iyb, iyt, iz;
  int ixcen, iycen, ixofs, iyofs, imx, imy;
  int bin = zap->vi->xybin;
  bool ifpad, flipped = zap->vi->li->axis == 2;

  ivwControlPriority(zap->vi, zap->ctrl);
  ImodInfoWin->raise();
  if (zap->rubberband) {
    xl = zap->rbImageX0;
    yb = zap->rbImageY0;
    xr = zap->rbImageX1;
    yt = zap->rbImageY1;
  } else {
    zapGetixy(zap, 0, -1, &xl, &yt, &iz);
    zapGetixy(zap, zap->winx, zap->winy-1, &xr, &yb, &iz);
  }
  ixl = (int)floor(xl + 0.5);
  ixr = (int)floor(xr - 0.5);
  iyb = (int)floor(yb + 0.5);
  iyt = (int)floor(yt - 0.5);
  ifpad = ixl < 0 || iyb < 0 || ixr >= zap->vi->xsize || iyt >= zap->vi->ysize;
  imx = bin * (ixr + 1 - ixl);
  imy = bin * (iyt + 1 - iyb);
  ixcen = bin * (ixr + 1 + ixl)/2;
  iycen = bin * (iyt + 1 + iyb)/2;
  ixofs = ixcen - (bin * zap->vi->xsize)/2;
  iyofs = iycen - (bin * zap->vi->ysize)/2;
  ixl = B3DMAX(0, ixl);
  iyb = B3DMAX(0, iyb);
  ixr = B3DMIN(ixr, zap->vi->xsize - 1);
  iyt = B3DMIN(iyt, zap->vi->ysize - 1);
  ixl *= bin;
  iyb *= bin;
  ixr = ixr * bin + bin - 1;
  iyt = iyt * bin + bin - 1;
  int lowSection, highSection;
  getLowHighSection(zap, lowSection, highSection);
  if (lowSection < 1 || highSection < 1 || highSection > zap->vi->zsize) {
    wprint("ERROR: %s is out of range.\n", flipped ? "-y" : "-z");
    if (!toInfoWindow)
      return "";
  }
  
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
    zapToggleRubberband(zap, false);
  } else {
    /* Otherwise, make window the right size for the image */
    neww = (int)(zap->zoom * zap->vi->xsize + width - zap->winx);
    newh = (int)(zap->zoom * zap->vi->ysize + height - zap->winy);
  }

  diaLimitWindowSize(neww, newh);
  newdx = dx + width / 2 - neww / 2;
  newdy = dy + height / 2 - newh / 2;
  diaLimitWindowPos(neww, newh, newdx, newdy);

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
     
/*
 * Set the control priority and set flag to record subarea and do float
 */
static void setControlAndLimits(ZapStruct *zap)
{
  ivwControlPriority(zap->vi, zap->ctrl);
  if (!zap->numXpanels)
    zap->recordSubarea = 1;
}

/*
 * Record the limits of the image displayed in the window or in the rubber band
 */
static void setAreaLimits(ZapStruct *zap)
{
  int iz, minArea = 16;
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
    zapGetixy(zap, 0, 0, &xl, &yt, &iz);
    zapGetixy(zap, zap->winx, zap->winy, &xr, &yb, &iz);
  }
  subStartX = B3DMAX((int)(xl + 0.5), 0);
  subEndX = B3DMIN((int)(xr - 0.5), zap->vi->xsize - 1);
  subStartY = B3DMAX((int)(yb + 0.5), 0);
  subEndY = B3DMIN((int)(yt - 0.5), zap->vi->ysize - 1);
  if (imodDebug('z'))
    imodPrintStderr("Set area %d %d %d %d\n", subStartX, subEndX, subStartY,
                    subEndY);
}     

/*
 * Return the subset limits from the active window
 */
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

/*
 * Toggle the rubber band
 */
void zapToggleRubberband(ZapStruct *zap, bool draw)
{
  if (zap->rubberband || zap->startingBand) {
    zap->rubberband = 0;
    zap->startingBand = 0;
    zap->bandChanged = 1;
    setControlAndLimits(zap);
    setMouseTracking(zap);
  } else {
    zap->startingBand = 1;
    zap->shiftingCont = 0;
    /* Eliminated old code for making initial band */
  }

  zap->qtWindow->setLowHighSectionState(zap->rubberband + zap->startingBand);
 
  // 3/6/05: synchronize the toolbar button
  zap->qtWindow->setToggleState(ZAP_TOGGLE_RUBBER, 
                                zap->rubberband + zap->startingBand);
  zapSetCursor(zap, zap->mousemode, utilNeedToSetCursor());
  if (draw)
    zapDraw(zap);

  // 4/6/09: This was needed before and after the draw for the Mac Qt 4.5.0
  zapSetCursor(zap, zap->mousemode, utilNeedToSetCursor());
}

/*
 * Shift the rubberband by a desired amount to the extent possible
 */
static void shiftRubberband(ZapStruct *zap, float idx, float idy)
{
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
}

/*
 * Find the first zap window, with a rubberband if flag is set
 */
ZapStruct *getTopZapWindow(bool withBand, int type)
{
  QObjectList objList;
  ZapStruct *zap;
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
      if (ctrlPtr->id == zap->ctrl && (!withBand || zap->rubberband)) {
        if (zap->rubberband) {
          ixl = (int)floor(zap->rbImageX0 + 0.5);
          ixr = (int)floor(zap->rbImageX1 - 0.5);
          iyb = (int)floor(zap->rbImageY0 + 0.5);
          iyt = (int)floor(zap->rbImageY1 - 0.5);
          if (ixr < 1 || iyt < 1 || ixl > zap->vi->xsize - 2 || 
              iyb > zap->vi->ysize - 2)
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
  ZapStruct *zap;
  int ixl, ixr, iyb, iyt, bin;


  zap = getTopZapWindow(true);
  if (!zap) {
    imodPrintStderr("ERROR: No Zap window has usable rubberband coordinates"
                    "\n");
    return;
  }

  bin = zap->vi->xybin;
  ixl = (int)floor(zap->rbImageX0 + 0.5);
  ixr = (int)floor(zap->rbImageX1 - 0.5);
  iyb = (int)floor(zap->rbImageY0 + 0.5);
  iyt = (int)floor(zap->rbImageY1 - 0.5);
  
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
  int lowSection, highSection;
  if (getLowHighSection(zap, lowSection, highSection)) {
    imodPrintStderr("Rubberband: %d %d %d %d %d %d\n", ixl + 1, iyb + 1,
                    ixr + 1, iyt + 1, lowSection, highSection);
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

/*
 * Reposition image center or rubber band to the given absolute position,
 * or by the given increments if the flag is set
 */
void zapSetImageOrBandCenter(float imx, float imy, bool incremental)
{
  ZapStruct *zap;
  zap = getTopZapWindow(false);
  if (!zap)
    return;
  if (zap->rubberband) {

    // Rubberband: get desired shift if not incremental, try to do it
    if (!incremental) {
      imx -= (zap->rbImageX1 + zap->rbImageX0) / 2.;
      imy -= (zap->rbImageY1 + zap->rbImageY0) / 2.;
    }
    shiftRubberband(zap, imx, imy);

    // And center image on rubberband
    zap->xtrans = B3DNINT(zap->vi->xsize / 2. - 
                          (zap->rbImageX1 + zap->rbImageX0) / 2.);
    zap->ytrans = B3DNINT(zap->vi->ysize / 2. - 
                          (zap->rbImageY1 + zap->rbImageY0) / 2.);
    zap->bandChanged = 1;
  } else {

    // Not rubberband: just adjust or set the translations, which will be
    // fixed when the draw is done
    if (incremental) {
      zap->xtrans -= B3DNINT(imx);
      zap->ytrans -= B3DNINT(imy);
    } else {
      zap->xtrans = B3DNINT(zap->vi->xsize / 2. - imx);
      zap->ytrans = B3DNINT(zap->vi->ysize / 2. - imy);
    }
  }
  zap->recordSubarea = 1;
  zapDraw(zap);
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
  ZapStruct *zap;
  int i;
  imodDialogManager.windowList(&objList, -1, ZAP_WINDOW_TYPE);

  for (i = 0; i < objList.count(); i++) {
    zap = ((ZapWindow *)objList.at(i))->mZap;
    setMouseTracking(zap);
  }
}

/*
 * Set mouse tracking based on state of all governing flags
 */
static void setMouseTracking(ZapStruct *zap)
{
  zap->gfx->setMouseTracking(insertDown != 0 || zap->rubberband ||
                             pixelViewOpen || zap->vi->trackMouseForPlugs);
}

/* 
 * Return the image coordinates of the mouse in the top Zap
 */
int getTopZapMouse(Ipoint *imagePt)
{
  int mx, my, iz;
  ZapStruct *zap = getTopZapWindow(false);
  if (!zap)
    return 1;
  mx = zap->gfx->mapFromGlobal(QCursor::pos()).x();
  my = zap->gfx->mapFromGlobal(QCursor::pos()).y();
  zapGetixy(zap, mx, my, &imagePt->x, &imagePt->y, &iz);
  imagePt->z = (float)iz;
  return 0;
}

/*
 * Look through all multiZ windows and report params of biggest one
 */
void zapReportBiggestMultiZ()
{
  QObjectList objList;
  QRect pos;
  ZapStruct *zap;
  int i;
  if (!ImodPrefs)
    return;
  imodDialogManager.windowList(&objList, -1, MULTIZ_WINDOW_TYPE);

  for (i = 0; i < objList.count(); i++) {
    zap = ((ZapWindow *)objList.at(i))->mZap;
    if (maxMultiZarea < zap->winx * zap->winy) {
      maxMultiZarea = zap->winx * zap->winy;
      pos = ivwRestorableGeometry(zap->qtWindow);
      ImodPrefs->recordMultiZparams(pos, zap->numXpanels, zap->numYpanels,
                                    zap->panelZstep, zap->drawInCenter,
                                    zap->drawInOthers);
    }
  }
}

/*
 * Take a snapshot by montaging at higher zoom
 */
static void montageSnapshot(ZapStruct *zap, int snaptype)
{
  int ix, iy, xFullSize, yFullSize, xTransStart, yTransStart, xTransDelta;
  int yTransDelta, xCopyDelta, yCopyDelta, xTransSave, yTransSave, barpos;
  int showSlice, limits[4];
  unsigned char *framePix, *fullPix, **linePtrs;
  double zoomSave;
  QString fname, sname;
  static int fileno = 0;
  int factor = imcGetMontageFactor();
  ScaleBar *barReal = scaleBarGetParams();
  ScaleBar barSaved;

  // Save translations and zoom 
  xTransSave = zap->xtrans;
  yTransSave = zap->ytrans;
  zoomSave = zap->zoom;

  if (imcGetSnapWholeMont()) {
    for (factor = 1; factor < 31; factor++) {
      if (factor * zap->winx >= zap->vi->xsize && factor * zap->winy >=
          zap->vi->ysize)
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
    zap->zoom = 1. / factor;
    zapDraw(zap);
  }

  // Get coordinates and offsets and buffers
  setAreaLimits(zap);
  if (getMontageShifts(zap, factor, zap->xstart, zap->xborder,
                       zap->vi->xsize, zap->winx,
                       xTransStart, xTransDelta, xCopyDelta, xFullSize) ||
      getMontageShifts(zap, factor, zap->ystart, zap->yborder, 
                       zap->vi->ysize, zap->winy,
                       yTransStart, yTransDelta, yCopyDelta, yFullSize)) {
    wprint("\aThere is too much border around image for montage snapshot.\n");
    return;
  }
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

  // On Quadro card (?), it is necessary to defer the autoswap for montaging
  // It's not clear why this isn't needed for other snapshots
  if (App->doublebuffer) {
    zap->gfx->setBufferSwapAuto(false);
    glReadBuffer(GL_BACK);
  }

  // Save and modify scale bar directives
  barSaved = *barReal;
  barReal->minLength = B3DNINT(factor * barReal->minLength);
  barReal->thickness = B3DNINT(factor * barReal->thickness);
  barReal->indentX = B3DNINT(factor * barReal->indentX);
  barReal->indentY = B3DNINT(factor * barReal->indentY);
  barpos = barReal->position;

  // Set up scaling
  if (imcGetScaleSizes()) {
    scaleSizes = imcGetSizeScaling();
    if (scaleSizes == 1)
      scaleSizes = B3DNINT(factor * B3DMIN(1., zap->zoom));
  }

  // Loop on frames, getting pixels and copying them
  zap->zoom *= factor;
  showSlice = zap->showslice;
  for (iy = 0; iy < factor; iy++) {
    for (ix = 0; ix < factor; ix++) {

      // Set up for scale bar if it is the right corner
      barReal->draw = false;
      if ((((barpos == 0 || barpos == 3) && ix == factor - 1) ||
           ((barpos == 1 || barpos == 2) && !ix)) &&
          (((barpos == 2 || barpos == 3) && iy == factor - 1) ||
           ((barpos == 0 || barpos == 1) && !iy))) {
        barReal->draw = barSaved.draw;
        scaleBarTestAdjust(zap->winx, zap->winy, zap->zoom);
      }
      
      zap->xtrans = -(xTransStart + ix * xTransDelta);
      zap->ytrans = -(yTransStart + iy * yTransDelta);
      zapDraw(zap);
      zap->showslice = showSlice;

      // Print scale bar length if it was drawn
      if (zap->scaleBarSize > 0)
        imodPrintStderr("Scale bar for montage is %g %s\n", zap->scaleBarSize,
                        imodUnits(zap->vi->imod));

      glReadPixels(0, 0, zap->winx, zap->winy, GL_RGBA, GL_UNSIGNED_BYTE, 
                   framePix);
      glFlush();
      memreccpy(fullPix, framePix, zap->winx, zap->winy, 4,
                xFullSize - zap->winx, ix * xCopyDelta, iy * yCopyDelta,
                0, 0, 0);
      if (App->doublebuffer) 
        zap->gfx->swapBuffers();
    }
  }

  if (App->doublebuffer)
    zap->gfx->setBufferSwapAuto(true);

  // Save the image then restore display
  for (iy = 0; iy < yFullSize; iy++)
    linePtrs[iy] = fullPix + 4 * xFullSize * iy;
  limits[0] = limits[1] = 0;
  limits[2] = xFullSize;
  limits[3] = yFullSize;

  // Reset the file number to zero unless doing movie, then get name and save
  if (!zap->movieSnapCount)
    fileno = 0;
  if (snaptype > 2)
    ImodPrefs->set2ndSnapFormat();
  fname = b3dGetSnapshotName("zap", snaptype > 1 ? SnapShot_RGB : SnapShot_TIF,
                             3, fileno);
  sname = b3dShortSnapName(fname);
  imodPrintStderr("3dmod: Saving zap montage to %s", LATIN1(sname));
  if (snaptype == 1)
    b3dSnapshot_TIF(fname, 4, limits, linePtrs, true);
  else
    b3dSnapshot_NonTIF(fname, 4, limits, linePtrs);
  if (snaptype > 2)
    ImodPrefs->restoreSnapFormat();
  imodPuts("");

  *barReal = barSaved;
  zap->xtrans = xTransSave;
  zap->ytrans = yTransSave;
  zap->zoom = zoomSave;
  scaleSizes = 1;
  zapDraw(zap);
    
  free(framePix);
  free(fullPix);
  free(linePtrs);
}

/*
 * Compute shifts and increments for the montage snapshot
 */
static int getMontageShifts(ZapStruct *zap, int factor, int imStart, 
                             int border, int imSize, int winSize,
                             int &transStart, int &transDelta, int &copyDelta,
                             int &fullSize)
{
  int inWin, overlap, imEnd, wofftmp, dstmp, dofftmp, trans;
  imEnd = B3DMIN(imStart + (int)((winSize - border)/ zap->zoom), imSize);
  inWin = (int)(winSize / (zap->zoom * factor));
  if (inWin >= imSize - factor)
    return 1;

  // Get trans and back it off to avoid window offset on left
  trans = -(imStart + (inWin - imSize) / 2);
  b3dSetImageOffset(winSize, imSize, zap->zoom * factor, dstmp, trans,
                    wofftmp, dofftmp, 0);
  if (wofftmp > 0)
    trans--;
  transStart = -trans;

  // Get overlap and delta, back off delta to avoid extra pixels on right
  overlap = B3DMAX(0, (factor * inWin + imStart - imEnd) / (factor - 1));
  transDelta = inWin - overlap;
  copyDelta = zap->zoom * factor * transDelta;
  fullSize = (factor - 1) * copyDelta + winSize;
  if (fullSize > (int)(zap->zoom * factor * (imSize - imStart))) {
    transDelta--;
    copyDelta = zap->zoom * factor * transDelta;
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
static void zapDrawGraphics(ZapStruct *zap)
{
  ImodView *vi = zap->vi;
  int bl, wh, ind, ix, iy, iz;
  int time, rgba = App->rgba;
  unsigned char **imageData;
  unsigned char *overImage;
  int overlay = 0;
  int otherSec = zap->section + vi->overlaySec;

  // DNM: this is not needed, 1/12/08
  //  ivwGetLocation(vi, &x, &y, &z);


  // DNM eliminated unused function 1/23/03
  // b3dSetCurPoint(x, y, zap->section);

  b3dSetImageOffset(zap->numXpanels ? zap->panelXsize : zap->winx, vi->xsize,
                    zap->zoom, zap->xdrawsize, zap->xtrans, 
                    zap->xborder, zap->xstart, 1);

  b3dSetImageOffset(zap->numXpanels ? zap->panelYsize : zap->winy, vi->ysize,
                    zap->zoom, zap->ydrawsize, zap->ytrans, 
                    zap->yborder, zap->ystart, 1);

  /* Get the time to display and flush if time is different. */
  if (zap->timeLock)
    time = zap->timeLock;
  else
    ivwGetTime(vi, &time);
  if (time != zap->time)
    zapFlushImage(zap);

  if (!zap->numXpanels) {
    imageData = ivwGetZSectionTime(vi, zap->section, time);

    // If flag set, record the subarea size, clear flag, and do call float to
    // set the color map if necessary.  If the black/white changes, flush image
    if (zap->recordSubarea) {
      
      // Set the X zoom to zoom if it hasn't been set yet or is not close to
      // zoom
      if (!zap->xzoom || fabs(1./zap->xzoom - 1./zap->zoom) >= 0.002)
        zap->xzoom = zap->zoom;
      setAreaLimits(zap);
      bl = vi->black;
      wh = vi->white;
      imod_info_bwfloat(vi, zap->section, time);
      if (App->rgba && (bl != vi->black || wh != vi->white))
        b3dFlushImage(zap->image);
    }
    
    b3dDrawBoxout(zap->xborder, zap->yborder, 
                  zap->xborder + (int)(zap->xdrawsize * zap->zoom),
                  zap->yborder + (int)(zap->ydrawsize * zap->zoom));
    
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
    if (overlay != zap->overlay)
      b3dFlushImage(zap->image);
    zap->overlay = overlay;

    b3dDrawGreyScalePixelsHQ(imageData,
                             vi->xsize, vi->ysize,
                             zap->xstart, zap->ystart,
                             zap->xborder, zap->yborder,
                             zap->xdrawsize, zap->ydrawsize,
                             zap->image,
                             vi->rampbase, 
                             (double)zap->zoom, (double)zap->zoom,
                             zap->hqgfx, zap->section, rgba);
  } else {

    // For panels, clear whole window then draw each panel if it is at legal Z
    utilClearWindow(App->background);
    for (ix = 0; ix < zap->numXpanels; ix++) {
      for (iy = 0; iy < zap->numYpanels; iy++) {
        ind = ix + iy * zap->numXpanels;
        bl = ind - (zap->numXpanels * zap->numYpanels - 1) / 2;
        iz = zap->section + zap->panelZstep * bl;
        if (iz < 0 || iz >= vi->zsize)
          continue;
        imageData = ivwGetZSectionTime(vi, iz, time);
        bl =  zap->xborder + zap->panelXborder + ix * 
          (zap->panelXsize + zap->panelGutter);
        wh =  zap->yborder + zap->panelYborder + iy * 
          (zap->panelYsize + zap->panelGutter);
        b3dDrawGreyScalePixelsHQ(imageData,
                                 vi->xsize, vi->ysize,
                                 zap->xstart, zap->ystart, bl, wh,
                                 zap->xdrawsize, zap->ydrawsize,
                                 zap->images[ind],
                                 vi->rampbase, 
                                 zap->zoom, zap->zoom,
                                 zap->hqgfx, iz, rgba);
      }
    }
  }

  /* DNM 9/15/03: Get the X zoom, which might be slightly different */
  // Then get the subarea limits again with more correct zoom value
  zap->xzoom = b3dGetCurXZoom();
  zap->time = time;
  if (zap->recordSubarea) {
    setAreaLimits(zap);
    locatorScheduleDraw(vi);
    zap->recordSubarea = 0;
  }
  if (overlay)
    free(overImage);
}

static void fillOverlayRGB(unsigned char **lines, int nx, int ny, int chan,
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
    b3dLineWidth(scaleSizes * vi->imod->obj[ob].linewidth2); 
    ifgSetupValueDrawing(&vi->imod->obj[ob], GEN_STORE_MINMAX1);

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

// A separate routine to draw the extra object(s) so that model - current 
// point - extra object drawing could happen in the right order
static void zapDrawExtraObject(ZapStruct *zap)
{
  ImodView *vi = zap->vi;
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
      zapDrawContour(zap, co, -1 - ob);
  }
  resetGhostColor();
}

/*
 * Draw a contour, including contours in an extra object
 */
static void zapDrawContour(ZapStruct *zap, int co, int ob)
{
  ImodView *vi = zap->vi;
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
    if (!zap->gfx->extraCursorInWindow() || zap->numXpanels)
      return;
    zap->drewExtraCursor = true;
  }

  if (ifgGetValueSetupState())
    handleFlags |= HANDLE_VALUE1;

  zscale = ((vi->imod->zscale ? vi->imod->zscale : 1.) * vi->zbin) / vi->xybin;

  /* check for contours that contain time data. */
  /* Don't draw them if the time isn't right. */
  /* DNM 6/7/01: but draw contours with time 0 regardless of time */
  if (ivwTimeMismatch(vi, zap->timeLock, obj, cont))
    return;

  // get draw properties
  selected = imodSelectionListQuery(vi, ob, co) > -2;
  nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, &stateFlags,
                                   handleFlags, selected, scaleSizes);
  if (contProps.gap)
    return;

  utilEnableStipple(vi, cont);

  /* Open or closed contour */
  // Skip if not wild and not on section
  lastVisible = zapPointVisable(zap, &(cont->pts[0])) || drawAllZ;
  if (!iobjScat(obj->flags) && ((cont->flags & ICONT_WILD) || lastVisible)) {
    if ((cont->flags & ICONT_WILD) || nextChange >= 0) {
      if (!nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags, &changeFlags, 
                                         handleFlags, selected, scaleSizes);

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
                                           selected, scaleSizes);

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
          && zapPointVisable(zap, &(cont->pts[pt]))){
        utilDrawSymbol(zapXpos(zap, cont->pts[pt].x),
                       zapYpos(zap, cont->pts[pt].y),
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
        if (zapPointVisable(zap, &(cont->pts[pt]))){
          /* DNM: make the product cast to int, not drawsize */
          b3dDrawCircle(zapXpos(zap, cont->pts[pt].x),
                        zapYpos(zap, cont->pts[pt].y),
                        (int)(drawsize * zap->zoom));
          if (drawsize > 3 && drawPntOffSec)
            b3dDrawPlus(zapXpos(zap, cont->pts[pt].x), 
                        zapYpos(zap, cont->pts[pt].y), 3 * scaleSizes);
        } else if (drawsize > 1 && drawPntOffSec) {
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

  utilDisableStipple(vi, cont);

  // Draw end markers
  if ((obj->symflags & IOBJ_SYMF_ENDS) && ob >= 0){
    if (zapPointVisable(zap, &(cont->pts[cont->psize-1]))){
      b3dColorIndex(App->endpoint);
      b3dDrawCross(zapXpos(zap, cont->pts[cont->psize-1].x),
                   zapYpos(zap, cont->pts[cont->psize-1].y), 
                   scaleSizes * obj->symsize/2);
    }
    if (zapPointVisable(zap, cont->pts)){
      b3dColorIndex(App->bgnpoint);
      b3dDrawCross(zapXpos(zap, cont->pts->x),
                   zapYpos(zap, cont->pts->y),
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
            zapPointVisable(zap, &cont->pts[pt]))
          b3dDrawSquare(zapXpos(zap, cont->pts[pt].x),
                        zapYpos(zap, cont->pts[pt].y), stp->value.i + 3);
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
static void zapDrawCurrentPoint(ZapStruct *zap)
{
  ImodView *vi = zap->vi;
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
    x = zapXpos(zap, (float)((int)vi->xmouse + 0.5));
    y = zapYpos(zap, (float)((int)vi->ymouse + 0.5));
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
      x = zapXpos(zap, pnt->x);
      y = zapYpos(zap, pnt->y);
      if (zapPointVisable(zap, pnt) && 
	  !ivwTimeMismatch(vi, zap->timeLock, obj, cont)) {
        b3dColorIndex(App->curpoint);
      }else{
        b3dColorIndex(App->shadow);
      }
      b3dDrawCircle(x, y, scaleSizes * curSize);
    }
  }
     
  /* draw begin/end points for current contour */
  if (cont){
    if (ivwTimeMismatch(vi, zap->timeLock, obj, cont))
      return;

    if (iobjClose(obj->flags) && (cont->flags & ICONT_OPEN)) {
      symbol = IOBJ_SYM_TRIANGLE;
      openAdd = 1;
    }
    b3dLineWidth(scaleSizes * obj->linewidth2);
    if (cont->psize > 1){
      if (zapPointVisable(zap, cont->pts)){
        b3dColorIndex(App->bgnpoint);
        utilDrawSymbol(zapXpos(zap, cont->pts->x),
                       zapYpos(zap, cont->pts->y), symbol, 
                       scaleSizes * (modPtSize + openAdd), flags);
      }
      if (zapPointVisable(zap, &(cont->pts[cont->psize - 1]))){
        b3dColorIndex(App->endpoint);
        utilDrawSymbol(zapXpos(zap, cont->pts[cont->psize - 1].x),
                       zapYpos(zap, cont->pts[cont->psize - 1].y), 
                       symbol, scaleSizes * (modPtSize + openAdd), flags);
      }
    }
  }

  b3dLineWidth(scaleSizes);
  zap->showedSlice = zap->showslice;
  if (zap->showslice){
    b3dColorIndex(App->foreground);
    b3dDrawLine(zapXpos(zap, vi->slice.zx1+0.5f),
                zapYpos(zap, vi->slice.zy1+0.5f),
                zapXpos(zap, vi->slice.zx2+0.5f), 
                zapYpos(zap, vi->slice.zy2+0.5f));
    zap->showslice = 0;
  }
  
  return;
}

/*
 * Draw ghost contours
 */ 
static void zapDrawGhost(ZapStruct *zap)
{
  int co, i, ob;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  Imod *mod = zap->vi->imod;
  DrawProps contProps, ptProps;
  int nextz, prevz, iz;
  bool drawprev, drawnext;
  int pt, npt, lastX, lastY, thisX, thisY;
  int nextChange, stateFlags, changeFlags;
  int handleFlags;

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

    handleFlags = HANDLE_2DWIDTH;
    if (ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1))
      handleFlags |= HANDLE_VALUE1;

    /* DNM 6/16/01: need to be based on zap->section, not zmouse */
    if (zap->vi->ghostdist) {
      nextz = zap->section + zap->vi->ghostdist;
      prevz = zap->section - zap->vi->ghostdist;
    } else {
      nextz = utilNextSecWithCont(zap->vi, obj, zap->section, 1);
      prevz = utilNextSecWithCont(zap->vi, obj, zap->section, -1);
    }
    drawprev = zap->vi->ghostmode & IMOD_GHOST_PREVSEC;
    drawnext = zap->vi->ghostmode & IMOD_GHOST_NEXTSEC;
     
    for (co = 0; co < obj->contsize; co++) {
      cont = &(obj->cont[co]);
      nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps,
                                       &stateFlags, handleFlags, 0,
                                       scaleSizes);
      if (contProps.gap)
        continue;
      zapSetGhostColor(zap, contProps.red, contProps.green, contProps.blue);
      
      /* DNM: don't display wild contours, only coplanar ones */
      /* By popular demand, display ghosts from lower and upper sections */
      if (cont->pts && !(cont->flags & ICONT_WILD)) {
        iz = (int)floor(cont->pts->z + 0.5);
        if ((iz > zap->section && drawprev && 
             ((zap->vi->ghostdist && iz <= nextz) || 
              (!zap->vi->ghostdist && iz == nextz))) ||
            (iz < zap->section && drawnext && 
             ((zap->vi->ghostdist && iz >= prevz) ||
              (!zap->vi->ghostdist && iz == prevz)))) {

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
                                                 &changeFlags, handleFlags, 0,
                                                 scaleSizes);
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
  resetGhostColor();
}

static void zapSetGhostColor(ZapStruct *zap, float obr, float obg, float obb)
{
  int red, green, blue, base;

  // Set base to 2 to make color get brighter instead of darker
  base = (zap->vi->ghostmode & IMOD_GHOST_LIGHTER) ? 2 : 0;
  red = (int)(((base + obr) * 255.0) / 3.0);
  green = (int)(((base + obg) * 255.0) / 3.0);
  blue = (int)(((base + obb) * 255.0) / 3.0);
    
  customGhostColor(red, green, blue); 
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
  int winx, winy;

  if (zap->toolMaxZ != zap->vi->zsize) {
    zap->toolMaxZ = zap->vi->zsize;
    zap->qtWindow->setMaxZ(zap->toolMaxZ);
  }

  // Workaround to Qt 4.5.0 cocoa bug, need to load these boxes 3 times
  if (zap->toolSection != zap->section){
    if (zap->toolZoom <= -4. || zap->toolZoom > -0.9)
      zap->toolSection = zap->section;
    zap->qtWindow->setSectionText(zap->section + 1);
  }
     
  if (zap->toolZoom != zap->zoom){
    if (zap->toolZoom < 0.)
      zap->toolZoom -= 1.;
    if (zap->toolZoom <= -4. || zap->toolZoom > -0.9)
      zap->toolZoom = zap->zoom;
    zap->qtWindow->setZoomText(zap->zoom);
  }

  if (zap->rubberband) {
    zapBandImageToMouse(zap, 0);
    winx = zap->rbMouseX1 - 1 - zap->rbMouseX0;
    winy = zap->rbMouseY1 - 1 - zap->rbMouseY0;
  } else {
    winx = zap->winx;
    winy = zap->winy;
  }
  if (winx != zap->toolSizeX || winy != zap->toolSizeY) {
    zap->toolSizeX = winx;
    zap->toolSizeY = winy;
    zap->qtWindow->setSizeText(winx, winy);
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

static void zapSetCursor(ZapStruct *zap, int mode, bool setAnyway)
{
  Qt::CursorShape shape;

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
    if (shape != zap->lastShape || setAnyway) {

      // This one makes the cursor show up a little better on starting/MAC
      imod_info_input();
      zap->gfx->setCursor(QCursor(shape));
    }
    zap->lastShape = shape;
    return;
  }

  // Or restore cursor from rubber band or change cursor dur to mode change
  if (zap->mousemode != mode || zap->lastShape >= 0 || setAnyway) {
    if (mode == IMOD_MMODEL) {
      zap->gfx->setCursor(*App->modelCursor);
    } else {
      zap->gfx->unsetCursor();
    }
    zap->mousemode = mode;
    zap->lastShape = -1;
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

static void setDrawCurrentOnly(ZapStruct *zap, int value)
{
  zap->drawCurrentOnly = value;
  imodInfoUpdateOnly(value);
}

/*

$Log$
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
