/*  IMOD VERSION 2.50
 *
 *  xzap.c -- The Zap Window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/

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

static void zapDraw_cb(ImodView *vi, void *client, int drawflag);
static void zapClose_cb(ImodView *vi, void *client, int drawflag);
static void zapKey_cb(ImodView *vi, void *client, int released, QKeyEvent *e);
static void zapDraw(ZapStruct *zap);
static void zapButton1(struct zapwin *zap, int x, int y, int controlDown);
static void zapButton2(struct zapwin *zap, int x, int y);
static void zapButton3(struct zapwin *zap, int x, int y, int controlDown);
static void zapB1Drag(struct zapwin *zap, int x, int y);
static void zapB2Drag(struct zapwin *zap, int x, int y);
static void zapB3Drag(struct zapwin *zap, int x, int y, int controlDown);
static void startMovieCheckSnap(ZapStruct *zap, int dir);

static void zapDrawGraphics(ZapStruct *zap);
static void zapDrawModel(ZapStruct *zap);
static void zapDrawContour(ZapStruct *zap, int co, int ob);
static void zapDrawCurrentPoint(ZapStruct *zap, int undraw);
static void zapDrawExtraObject(ZapStruct *zap);
static int  zapDrawAuto(ZapStruct *zap);
static void zapDrawGhost(ZapStruct *zap);
static void zapDrawTools(ZapStruct *zap);
static void zapSetCursor(ZapStruct *zap, int mode);
static int  zapXpos(ZapStruct *zap, float x);
static int  zapYpos(ZapStruct *zap, float x);
static void zapGetixy(ZapStruct *zap, int mx, int my, float *x, float *y);
static int  zapPointVisable(ZapStruct *zap, Ipoint *pnt);
static void zapAutoTranslate(ZapStruct *zap);
static void zapSyncImage(ZapStruct *win);
static void zapResizeToFit(ZapStruct *zap);
static void setControlAndLimits(ZapStruct *zap);
static void zapToggleRubberband(ZapStruct *zap);
static void zapBandImageToMouse(ZapStruct *zap, int ifclip); 
static void zapBandMouseToImage(ZapStruct *zap, int ifclip);

/* DNM 1/19/01: add this to allow key to substitute for middle mouse button */
static int insertDown = 0;

static QTime insertTime;
static QTime but1downt;

static int numZapWindows = 0;
static int subStartX = 0;
static int subStartY = 0;
static int subEndX = 0;
static int subEndY = 0;

static int zapDebug = 0;

void zapHelp()
{
  dia_vasmsg
    ("3dmod Zap Help\n",
     "---------------------------------------------------------------\n",
     "Mouse Button Assignments\n\n",
     "\tDescriptions below refer to the first, second, and third mouse "
     "buttons.  By default, these correspond to left, middle, and right "
     "buttons of a 3-button mouse, but you can change these assignments in "
     "the 3dmod Preferences dialog, accessed via the Edit-Options menu entry."
     "\n\n"
     "The Tool Bar\n\n",
     "\tThe Up and Down Arrows step the zoom factor up or down.\n",
     "\tThe Zoom edit box shows the current zoom factor and allows ",
     "one to type in a custom zoom.\n",
     "\tThe checkerboard button toggles between fast rendering and ",
     "slower but higher quality interpolated image rendering.\n",
     "\tThe lock button can lock the Zap window at the current Z value.",
     "  When locked, the Z value of a Zap window can be changed with "
     "controls or hot keys in the window, but that will not change the current"
     " Z value of other windows in the program.\n",
     "\tThe centering button toggles between having the image always ",
     "centered ",
     "on the current model point, and recentering the image only when ",
     "the current point comes near an edge (the default).\n"
     "\tThe modeling direction button toggles between inserting new ",
     "points after the current point (when pointing up) or before ",
     "the current point (when pointing down).\n"
     "\tThe Z slider allows one to riffle through images or select a "
     "particular section to display.\n"
     "\tThe section edit box shows the current section and allows one ",
     "to go directly to a section by typing in a number.\n",
     "\tThe Info button (I) brings the Information Window to the "
     "front and prints information about window and image size (see the "
     "I Hot Key below).\n"
     "\tIf multiple image files have been loaded into 3dmod, three "
     "additional controls appear.  The Time Lock button will prevent "
     "changes in other windows from changing the time (image file) "
     "displayed in this Zap window.  The left and right arrows will "
     "step backward and forward in time.\n\n",
     "---------------------------------------------------------------\n",
     "\nHot Keys special to the Zap window\n\n"
     "\ti toggles the modeling direction.\n",
     "\tS or "CTRL_STRING"-S saves the Zap window, or the area inside the "
     "rubber band, into an RGB or TIFF file.\n"
     "\tZ toggles auto section advance on and off.  When this is on, ",
     "the section will change automatically after inserting a point if ",
     "there was a section change between that point and the previous ",
     "point.\n",
     "\tb builds a contour when AutoContour window is open.\n",
     "\ta advances to and fills next section when auto contouring.\n",
     "\tu smooths a filled area when auto contouring.\n",
     "\tB toggles the rubber band on and off.  The rubber band can be "
     "used to select an area, then snapshot the area, resize the window "
     "to that area, or find its coordinates.  After pressing B to turn on the "
     "rubber band, position the mouse at the desired upper left corner, click "
     "the first mouse button and hold it down while you move to the lower "
     "right corner.  After initially defining the band, you can adjust its "
     "size by placing the pointer near an edge or corner and "
     "dragging with the first mouse button.  The band can be moved as a "
     "unit by placing the pointer near an edge and dragging with the "
     "second mouse button.\n",
     "\tI prints information about the window and image size and "
     "the coordinates of the image in the window.  If the rubber band "
     "is on, the sizes and coordinates are relative to the rubber band "
     "rather than the window.  The image "
     "coordinates of the lower left and upper right corners, and of "
     "the center, are printed in the 3dmod info window.  There is also "
     "a fragment of a command line for extracting the image from the "
     "stack with \"newstack\".  This key also brings the Information "
     "Window to the front of the display.\n",
     "\tR resizes the window.  With the rubber band off, the window "
     "changes, "
     "if possible, to match the size of the entire image at the "
     "current zoom.  With the rubber band on, it changes to the size "
     "of the rubber band and the image is shifted to display the area "
     "previously in the rubber band.\n",
     "\t"CTRL_STRING"-A adds multiple contours on the current section to the "
     "contour selection list.  Selected contours will be shown with a "
     "distinguishable line thickness.  Only contours from the current object "
     "that are "
     "confined to the current section will be added.  With the rubber band "
     "off, all eligible contours on the section will be added; with the "
     "rubber band on, only contours completely within the rubber band will be "
     "selected.\n",
     "\tArrow keys and the keypad: In movie mode, the arrow keys and "
     "the PageUp and PageDown keys move the current viewing point (the "
     "small cross), while the keypad keys pan the image horizontally,"
     " vertically, and diagonally.  In model mode, the arrow keys pan "
     "the image, the numeric keypad arrows move the current model point "
     "laterally, and the numeric keypad PageUp and PageDown keys move "
     "the current model point in Z.\n"
     "\tIns on the keypad: In model mode, this key works the same as "
     "the second mouse button.  A single keystrike adds one point; "
     "holding the key down allows points to be added continuously.\n"
     "\tESC will close the Zap window.\n\n"
     "For other keys, see Help - Hot Keys in the 3dmod Info Window.\n\n",
     "---------------------------------------------------------------\n",
     "\nMouse button function in movie mode\n\n",
     "\tFirst Button Click: Select the current viewing point, marked by "
     "a small cross.\n",
     "\tFirst Button Drag: Pan the image if it is larger than the "
     "window, or adjust the size of the rubber band."
     "\n"
     "\tSecond Button: Start movie in forward direction, or stop movie."
     "\n"
     "\tSecond Button Drag: Move the rubber band.\n",
     "\tThird Button: Start movie in backward direction, or stop movie."
     "\n\n"
     "Mouse button function in model mode\n\n",
     "\tFirst Button Click: Make the nearest visible model point be the "
     "current model point.  If there is no point nearby, this detaches from "
     "the current point and contour and selects a current viewing point "
     "instead.\n",
     "\t"CTRL_STRING" - First Button Click: Selects the nearest visible point "
     "as the current point but also adds this contour to a list of selected "
     "contours.  Selected contours in this list will be "
     "displayed with a distinguishable line thickness.  If a contour is "
     "already selected, "CTRL_STRING"-clicking on it will deselect it.  The "
     "selection list is cleared by doing an ordinary first button selection "
     "of any model point, or by a variety of other operations, mostly ones "
     "that select a different contour from the current one.\n"
     "\tFirst Button Drag: Pan the image if it is larger than the "
     "window, or adjust the size of the rubber band."
     "\n"
     "\tSecond Button Click: Add one point to the current contour.\n"
     "\tSecond Button Drag: Continually add points to the current "
     "contour as the mouse is moved, or move the rubber band.\n",
     "\tThird Button Click: Modify the current model point to be at the "
     "selected position.\n",
     "\tThird Button Drag: Continually modify points as the mouse is "
     "moved.  This only works when the current model point is in the "
     "interior of the contour, not at its end.\n",
     "\t"CTRL_STRING" - Third Button Click: Delete any points under the "
     "cursor in the current contour.\n",
     "\t"CTRL_STRING" - Third Button Drag: Continually delete points under the"
     " cursor as the mouse is moved.  At the end, the current point is "
     "set before the last deletion (or after, if modeling direction is "
     "inverted.)\n",
     NULL);
  return;
}

// This receives the close signal back from the controller, tells the
// window to close, and sets the closing flag
static void zapClose_cb(ImodView *vi, void *client, int junk)
{
  ZapStruct *zap = (ZapStruct *)client;
  if (zapDebug)
    imodPrintStderr("Sending zap window close.\n");
  zap->qtWindow->close();
}

/* This receives a closing signal from the window */
void zapClosing(ZapStruct *zap)
{
  if (zapDebug)
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

  if (zapDebug)
    imodPrintStderr("Zap Draw\n");

  if (!zap) return;
  if ((!zap->popup) || (!zap->ginit)) return;
     
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

          /* If close to a border, do ani mage offset computation
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
   int bandmin = 4;

  ivwControlPriority(zap->vi, zap->ctrl);

  if (zapDebug)
    imodPrintStderr("RESIZE: ");

  if (zapDebug) {
    imodPrintStderr("Size = %d x %d :", winx, winy);
    if (zap->ginit)
      imodPrintStderr("Old Size = %d x %d :", zap->winx, zap->winy);
  }

  zap->winx = winx;
  zap->winy = winy;
  b3dSetCurSize(winx, winy);
 
  if (zap->ginit){

    /* Make sure the rubber band stays legal, but keep it same size
       if possible */
    /*    if (zap->rubberband) {
      if (zap->rbMouseX1 >= winx) {
        if (zap->rbMouseX1 + 1 - zap->rbMouseX0 > winx) {
          zap->rbMouseX1 = winx - 1;
          if (zap->rbMouseX0 > zap->rbMouseX1 - bandmin)
            zap->rbMouseX0 = zap->rbMouseX1 - bandmin;
        } else {
          zap->rbMouseX0 -= zap->rbMouseX1 + 1 - winx;
          zap->rbMouseX1 = winx - 1;
        }
      }
      if (zap->rbMouseY1 >= winy) {
        if (zap->rbMouseY1 + 1 - zap->rbMouseY0 > winy) {
          zap->rbMouseY1 = winy - 1;
          if (zap->rbMouseY0 > zap->rbMouseY1 - bandmin)
            zap->rbMouseY0 = zap->rbMouseY1 - bandmin;
        } else {
          zap->rbMouseY0 -= zap->rbMouseY1 + 1 - winy;
          zap->rbMouseY1 = winy - 1;
        }
      }
      } */

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

  if (zapDebug)
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
  if (zapDebug)
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
  zapDrawCurrentPoint(zap, 0);
  zapDrawExtraObject(zap);
  zapDrawAuto(zap);
  if (zap->rubberband) {
    b3dColorIndex(App->endpoint);
    zapBandImageToMouse(zap, 0);
    b3dDrawRectangle(zap->rbMouseX0, zap->winy - 1 - zap->rbMouseY1, 
                     zap->rbMouseX1 - zap->rbMouseX0, 
                     zap->rbMouseY1 - zap->rbMouseY0);
  } 
  zapDrawTools(zap);
  if (zapDebug)
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
  zap->movieSnapCount = 0;
  zap->drawCurrentOnly = 0;


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
  if (zapDebug)
    imodPuts("Got a zap window");

  zap->gfx = zap->qtWindow->mGLw;
  if (!App->rgba)
    zap->gfx->setColormap(*(App->qColormap));

  zap->qtWindow->setCaption(imodCaption("3dmod ZaP Window"));

  zap->qtWindow->mToolBar->setLabel(imodCaption("ZaP Toolbar"));
  
  zap->ctrl = ivwNewControl(vi, zapDraw_cb, zapClose_cb, zapKey_cb,
                            (void *)zap);
  imodDialogManager.add((QWidget *)zap->qtWindow, IMOD_IMAGE, ZAP_WINDOW_TYPE);

  zapMaximumWindowSize(maxWinx, maxWiny);

  oldGeom = ImodPrefs->getZapGeometry();

  /* 1/28/03: this call is needed to get the toolbar size hint right */
  imod_info_input();
  QSize toolSize = zap->qtWindow->mToolBar->sizeHint();
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

    needWinx = zap->zoom * vi->xsize;
    needWiny = zap->zoom * vi->ysize + toolHeight;
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

    // adjust zoom either way to fit window
    needWiny = newHeight - toolHeight;
    needWinx = newWidth;

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

  if (zapDebug)
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
  Ipoint pmin, pmax, selmin, selmax;
  Iobj *obj;
  /* downtime.start(); */

  if (Imod_debug)
    imodPrintStderr("key %x, state %x\n", keysym, event->state());
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
      zapButton2(zap, ix, iy);
    else
      zapB2Drag(zap, ix, iy); 

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
        selmin.x = 0.;
        selmax.x = vi->xsize;
        selmin.y = 0.;
        selmax.y = vi->ysize;
      }

      obj = imodObjectGet(vi->imod);
      if (!obj)
        break;

      // Clear the list, set up an index to add, look for contours inside
      // the bounding box, add them, make last one be current
      imodSelectionListClear(vi);
      indadd.object = vi->imod->cindex.object;
      indadd.point = -1;
      for (i = 0; i < obj->contsize; i++) {
        imodContourGetBBox(&(obj->cont[i]), &pmin, &pmax);
        indadd.contour = i;
        if (pmin.x >= selmin.x && pmax.x <= selmax.x &&
            pmin.y >= selmin.y && pmax.y <= selmax.y &&
            pmin.z >= zap->section - 0.5 && pmax.z <= zap->section + 0.5) {
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
          
  case Qt::Key_S:
    if (shifted || 
        (event->state() & Qt::ControlButton)){
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
    } else
      imod_zap_open(vi);
    handled = 1;
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
  if (handled)
    event->accept();    // redundant action - supposed to be the default
  else {
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
  zap->gfx->setMouseTracking(false);
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
  setControlAndLimits(zap);

  button1 = event->stateAfter() & ImodPrefs->actualButton(1) ? 1 : 0;
  button2 = event->stateAfter() & ImodPrefs->actualButton(2) ? 1 : 0;
  button3 = event->stateAfter() & ImodPrefs->actualButton(3) ? 1 : 0;

  /* imodPrintStderr("click at %d %d\n", event->x(), event->y()); */

  if (event->button() == ImodPrefs->actualButton(1)) {
      but1downt.start();
      firstmx = event->x();
      firstmy = event->y();
      if (zap->startingBand)
        zapButton1(zap, firstmx, firstmy, event->state() & Qt::ControlButton);
      else
        firstdrag = 1;
      
  } else if (event->button() == ImodPrefs->actualButton(2) &&
	     !button1 && !button3) {
      zapButton2(zap, event->x(), event->y());

  } else if (event->button() == ImodPrefs->actualButton(3) &&
	     !button1 && !button2) {
      zapButton3(zap, event->x(), event->y(), 
                 event->state() & Qt::ControlButton);
  }
  zap->lmx = event->x();
  zap->lmy = event->y();

}

// respond to mouse release event
void zapMouseRelease(ZapStruct *zap, QMouseEvent *event)
{
  setControlAndLimits(zap);
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
  } else if ((event->button() == ImodPrefs->actualButton(2))
	     && zap->drawCurrentOnly) {
    zap->drawCurrentOnly = 0;
    zapDraw(zap);
  }
}

// Respond to a mouse move event (mouse down)
void zapMouseMove(ZapStruct *zap, QMouseEvent *event, bool mousePressed)
{
  int button1, button2, button3;
  int cumdx, cumdy;
  int cumthresh = 6 * 6;

  if (!(mousePressed || insertDown))
    return;

  setControlAndLimits(zap);
  
  button1 = event->state() & ImodPrefs->actualButton(1) ? 1 : 0;
  button2 = (event->state() & ImodPrefs->actualButton(2)) 
    || insertDown ? 1 : 0;
  button3 = event->state() & ImodPrefs->actualButton(3) ? 1 : 0;
  /*  imodPrintStderr("mb  %d|%d|%d\n", button1, button2, button3); */

  if ( (button1) && (!button2) && (!button3)){
    /* DNM: wait for a bit of time or until enough distance moved, but if we
       do not replace original lmx, lmy, there is a disconcerting lurch */
    cumdx = event->x() - firstmx;
    cumdy = event->y() - firstmy;
    if ((but1downt.elapsed()) > 250 || cumdx * cumdx + cumdy * cumdy >
        cumthresh)
      zapB1Drag(zap, event->x(), event->y());
    }

    if ( (!button1) && (button2) && (!button3))
      zapB2Drag(zap, event->x(), event->y());

    if ( (!button1) && (!button2) && (button3))
      zapB3Drag(zap, event->x(), event->y(),
                event->state() & Qt::ControlButton);

    zap->lmx = event->x();
    zap->lmy = event->y();
}


static int zapBandMinimum(ZapStruct *zap)
{
  int bandmin = 4;
    // Adjust minimum size down in case image is tiny
    if (bandmin > zap->vi->xsize * zap->xzoom + 2)
      bandmin = zap->vi->xsize * zap->xzoom + 2;
    if (bandmin > zap->vi->ysize * zap->zoom + 2)
      bandmin = zap->vi->ysize * zap->zoom + 2;
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
  int i, temp_distance;
  int distance = -1;
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
      if (temp_distance == -1)
        continue;
      if (distance == -1 || distance > temp_distance){
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

    if (distance > -1) {

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

void zapButton2(ZapStruct *zap, int x, int y)
{
  ImodView *vi = zap->vi;
  Iobj  *obj;
  Icont *cont;
  Ipoint point;
  int   pt;
  float ix, iy;
  float lastz;
  int time;
  int rcrit = 10;   /* Criterion for moving the whole band */
  int dxll, dxur,dyll, dyur;
  int cz, pz;
  int curTime = zap->timeLock ? zap->timeLock : vi->ct;

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
    obj = imodObjectGet(vi->imod);
    if (!obj)
      return;

    // Get current contour; if there is none, start a new one
    // DNM 7/10/04: switch to calling routine
    cont = ivwGetOrMakeContour(vi, obj);
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

    /* If contour is empty and time doesn't match, 
       reassign it to the current time */
    if (zapTimeMismatch(vi, zap->timeLock, obj, cont) && !cont->psize)
      cont->type = curTime;
    
    /* If contours are closed and Z has changed, start a new contour */
    /* Also check for a change in time, if time data are being modeled  */
    /* and start new contour for any kind of contour */
    // DNM 7/10/04: just use first point instead of current point which is
    // not always defined
    if (cont->psize > 0) {
      cz = (int)floor(cont->pts->z + 0.5); 
      pz = (int)point.z;
      if ((iobjClose(obj->flags) && !(cont->flags & ICONT_WILD) && cz != pz) ||
          zapTimeMismatch(vi, zap->timeLock, obj, cont)) {
        if (cont->psize == 1) {
          wprint("Started a new contour even though last "
                 "contour had only 1 pt.  ");
          if (cz != pz)
            wprint("\aUse open contours to model across sections.\n");
          else
            wprint("\aSet contour time to 0 to model across times.\n");
        }
        imodNewContour(vi->imod);
        cont = imodContourGet(vi->imod);
        if (!cont)
          return;
        ivwSetNewContourTime(vi, obj, cont);
      }
    }

    /* Now if times still don't match refuse the point */
    if (zapTimeMismatch(vi, zap->timeLock, obj, cont)) {
      wprint("\aContour time does not match current time.\n"
             "Set contour time to 0 to model across times.\n");
      return;
    }
    
    pt = vi->imod->cindex.point;
    if (pt >= 0)
      lastz = cont->pts[pt].z;
    else
      lastz = point.z;

    /* Insert or add point depending on insertion mode and whether at end
       of contour */
    if ((cont->psize - 1) == pt){
      if (zap->insertmode && cont->psize)
        InsertPoint(vi->imod, &point, pt);
      else
        NewPoint(vi->imod, &point);
    }else{
      if (zap->insertmode)
        InsertPoint(vi->imod, &point, pt);
      else
        InsertPoint(vi->imod, &point, pt + 1);
    }

    /* DNM: auto section advance is based on 
       the direction of section change between last
       and just-inserted points */
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
        imodPointDelete(cont, i);
        zap->vi->imod->cindex.point = i + zap->insertmode - 1;
        if (zap->vi->imod->cindex.point < 0)
          zap->vi->imod->cindex.point = 0;
        if (zap->vi->imod->cindex.point >= cont->psize)
          zap->vi->imod->cindex.point = cont->psize - 1;
        deleted = 1;
        continue;
      }
    }
    i++;
  }
  if (!deleted)
    return;
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
    if (zapTimeMismatch(vi, zap->timeLock, obj, cont))
      return;

    /* If the control key is down, delete points under the cursor */
    if (controlDown) {
      zapDelUnderCursor(zap, x, y, cont);
      return; 
    }

          
    if (!zapPointVisable(zap, &(cont->pts[pt])))
      return;
    cont->pts[pt].x = ix;
    cont->pts[pt].y = iy;

    vi->xmouse  = ix;
    vi->ymouse  = iy;

    imodDraw(vi, IMOD_DRAW_RETHINK);
    return;
  }
  startMovieCheckSnap(zap, -1);
}

void zapB1Drag(ZapStruct *zap, int x, int y)
{
  int rubbercrit = 10;  /* Criterion distance for grabbing the band */
  int bandmin = zapBandMinimum(zap);
  int i, dminsq, dist, distsq, dmin, dxll, dyll, dxur, dyur;
  int minedgex, minedgey;

  // For zooms less than one, move image along with mouse; for higher zooms,
  // Translate 1 image pixel per mouse pixel (accelerated)
  double transFac = zap->zoom < 1. ? 1. / zap->zoom : 1.;

  if (zap->rubberband && firstdrag) {

    /* First time if rubberbanding, analyze for whether close to a
       corner or an edge */
    zapBandImageToMouse(zap, 0);    
    dminsq = rubbercrit * rubbercrit;
    minedgex = -1;
    for (i = 0; i < 4; i++)
      dragging[i] = 0;
    dxll = firstmx - zap->rbMouseX0;
    dxur = firstmx - zap->rbMouseX1;
    dyll = firstmy - zap->rbMouseY0;
    dyur = firstmy - zap->rbMouseY1;

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
    if (dragband)
      zapSetCursor(zap, zap->mousemode);
  }
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

void zapB2Drag(ZapStruct *zap, int x, int y)
{
  ImodView *vi = zap->vi;
  Iobj *obj;
  Icont *cont;
  Ipoint *lpt, cpt;
  float ix, iy, idx, idy;
  double dist;
  int pt;
  int dx, dy;
     
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
       zapTimeMismatch(vi, zap->timeLock, obj, cont)) {
    zapButton2(zap, x, y);
    return;
  }

  if (zapTimeMismatch(vi, zap->timeLock, obj, cont))
    return;

  if ( dist > scaleModelRes(vi->imod->res, zap->zoom)){
    pt = vi->imod->cindex.point;

    /* Insert or add point depending on insertion mode and whether at end
       of contour ; DNM made this work the same as single insert */
    if ((cont->psize - 1) == pt){
      if (zap->insertmode && cont->psize)
        InsertPoint(vi->imod, &cpt, pt);
      else {
        NewPoint(vi->imod, &cpt);

	// Set flag for drawing current contour only in this case
	zap->drawCurrentOnly = 1;
      }
    }else{
      if (zap->insertmode)
        InsertPoint(vi->imod, &cpt, pt);
      else
        InsertPoint(vi->imod, &cpt, pt + 1);
    }


    // TODO: figure out the right flags
    imodDraw(vi, IMOD_DRAW_MOD | IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC);
  }
}

void zapB3Drag(ZapStruct *zap, int x, int y, int controlDown)
{
  ImodView *vi = zap->vi;
  Iobj *obj;
  Icont *cont;
  Ipoint *lpt;
  Ipoint pt;
  float ix, iy;

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

  if (zapTimeMismatch(vi, zap->timeLock, obj, cont))
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
    lpt = &(cont->pts[vi->imod->cindex.point]);
    lpt->x = pt.x;
    lpt->y = pt.y;
    lpt->z = pt.z;
    imodDraw(vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD );
  }
  return;
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
    zap->rubberband = 0;
  } else {
    /* Otherwise, make window the right size for the image */
    neww = (int)(zap->zoom * zap->vi->xsize + width - zap->winx);
    newh = (int)(zap->zoom * zap->vi->ysize + height - zap->winy);
  }

  zapLimitWindowSize(neww, newh);
  newdx = dx + width / 2 - neww / 2;
  newdy = dy + height / 2 - newh / 2;
  zapLimitWindowPos(neww, newh, newdx, newdy);

  if (zapDebug)
    imodPrintStderr("configuring widget...");

  /* imodPrintStderr("newdx %d newdy %d\n", newdx, newdy); */
  zap->qtWindow->resize(neww, newh);
  zap->qtWindow->move(newdx, newdy);

  /* DNM 9/12/03: remove the ZAP_EXPOSE_HACK, and a second set geometry that
     was needed temporarily with Qt 3.2.1 on Mac */

  if (zapDebug)
    imodPrintStderr("back\n");
}
     
// Set the control priority and record the limits of the image displayed in
// the window or in the rubber band
static void setControlAndLimits(ZapStruct *zap)
{
  float xl, xr, yb, yt;
  ivwControlPriority(zap->vi, zap->ctrl);
  if (zap->rubberband) {
    xl = zap->rbImageX0;
    yb = zap->rbImageY0;
    xr = zap->rbImageX1;
    yt = zap->rbImageY1;
  } else {
    zapGetixy(zap, 0, 0, &xl, &yt);
    zapGetixy(zap, zap->winx, zap->winy, &xr, &yb);
  }
  subStartX = (int)(xl + 0.5);
  subEndX = (int)(xr - 0.5);
  subStartY = (int)(yb + 0.5);
  subEndY = (int)(yt - 0.5);
  if (subStartX < 0)
    subStartX = 0;
  if (subEndX >= zap->vi->xsize)
    subEndX = zap->vi->xsize - 1;
  if (subStartY < 0)
    subStartY = 0;
  if (subEndY >= zap->vi->ysize)
    subEndY = zap->vi->ysize - 1;
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
  } else {
    zap->startingBand = 1;
    /* Eliminated old code for making initial band */
  }
  zapSetCursor(zap, zap->mousemode);
  zapDraw(zap);
}

// Report the rubberband coordinates of the first zap window with a band
void zapReportRubberband()
{
  QObjectList objList;
  ZapStruct *zap;
  int i, bin;
  float xl, xr, yb, yt;
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

  if (zap->timeLock) {
    imageData = ivwGetZSectionTime(vi, zap->section, zap->timeLock);
    time = zap->timeLock;
  } else{
    /* flush if time is different. */
    ivwGetTime(vi, &time);
    if (time != zap->time){
      b3dFlushImage(zap->image);
      zap->time = time;
    }
    imageData = ivwGetZSection(vi, zap->section);
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
  Iobj *xobj = vi->extraObj;

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
  Ipoint *point;
  int pt, radius, lastX, lastY, thisX, thisY;
  float drawsize, zscale;
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

  /* check for contours that contian time data. */
  /* Don't draw them if the time isn't right. */
  /* DNM 6/7/01: but draw contours with time 0 regardless of time */
  if (zapTimeMismatch(vi, zap->timeLock, obj, cont))
    return;

  selected = imodSelectionListQuery(vi, ob, co) > -2;
  if (selected)
    b3dLineWidth(obj->linewidth2 < 3 ? 
                 obj->linewidth2 + 2 : obj->linewidth2 / 2);

  /* Open or closed contour */
  // Skip if not wild and not on section
  lastVisible = zapPointVisable(zap, &(cont->pts[0]));
  if (!iobjScat(obj->flags) && ((cont->flags & ICONT_WILD) || lastVisible)) {
    if (cont->flags & ICONT_WILD) {

      // For wild contour, test every point and connect only pairs on section
      lastX = zapXpos(zap, cont->pts[0].x);
      lastY = zapYpos(zap, cont->pts[0].y);
      for (pt = 1; pt < cont->psize; pt++) {
        thisVisible = zapPointVisable(zap, &(cont->pts[pt]));
        if (thisVisible) {
          thisX = zapXpos(zap, cont->pts[pt].x);
          thisY = zapYpos(zap, cont->pts[pt].y);
          if (lastVisible)
            b3dDrawLine(lastX, lastY, thisX, thisY);
          lastX = thisX;
          lastY = thisY;
        }
        lastVisible = thisVisible;
      }

      // IF closed contour in closed object and not current, draw closure as
      // long as both points are visible
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN) && 
          !currentCont && lastVisible && zapPointVisable(zap, cont->pts))
        b3dDrawLine(lastX, lastY, zapXpos(zap, cont->pts->x),
                    zapYpos(zap, cont->pts->y));

    } else {

      // For non-wild contour, draw all points without testing
      b3dBeginLine();
      for (pt = 0; pt < cont->psize; pt++)
        b3dVertex2i(zapXpos(zap, cont->pts[pt].x),
                    zapYpos(zap, cont->pts[pt].y));

      // IF closed contour in closed object and not current, draw closure
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN) && !currentCont)
        b3dVertex2i(zapXpos(zap, cont->pts->x), zapYpos(zap, cont->pts->y));

      b3dEndLine();
    }
          

    // Draw symbols
    if (obj->symbol != IOBJ_SYM_NONE)
      for (pt = 0; pt < cont->psize; pt++) {
        if (!zapPointVisable(zap, &(cont->pts[pt])))
          continue;
        zapDrawSymbol(zapXpos(zap, cont->pts[pt].x),
                      zapYpos(zap, cont->pts[pt].y),
                      obj->symbol,
                      obj->symsize,
                      obj->symflags);
      }
  }
     
  /* scattered contour - symbols */
  if (iobjScat(obj->flags) && obj->symbol != IOBJ_SYM_NONE){
    for (pt = 0; pt < cont->psize; pt++){
      if (zapPointVisable(zap, &(cont->pts[pt]))){
        zapDrawSymbol(zapXpos(zap, cont->pts[pt].x),
                      zapYpos(zap, cont->pts[pt].y),
                      obj->symbol,
                      obj->symsize,
                      obj->symflags);
      }
    }
  }

  /* Any contour with point sizes set */
  if (iobjScat(obj->flags) || cont->sizes || obj->pdrawsize) {
    for (pt = 0; pt < cont->psize; pt++){

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
  if (obj->symflags & IOBJ_SYMF_ENDS){
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

static void zapDrawCurrentPoint(ZapStruct *zap, int undraw)
{
  ImodView *vi = zap->vi;
  Iobj *obj = imodObjectGet(vi->imod);
  Icont *cont = imodContourGet(vi->imod);
  Ipoint *pnt = imodPointGet(vi->imod);
  int imPtSize, modPtSize, backupSize, curSize;
  int x,y;

  if (!vi->drawcursor) return;

  zapCurrentPointSize(obj, &modPtSize, &backupSize, &imPtSize);

  if ((vi->imod->mousemode == IMOD_MMOVIE)||(!pnt)){
    x = zapXpos(zap, (float)((int)vi->xmouse + 0.5));
    y = zapYpos(zap, (float)((int)vi->ymouse + 0.5));
    b3dColorIndex(App->foreground);
    b3dDrawPlus(x, y, imPtSize);
          
  }else{
    if ((cont) && (cont->psize) && (pnt)){

      curSize = modPtSize;
      if (cont->psize > 1 && 
          (pnt == cont->pts || pnt == cont->pts + cont->psize - 1))
        curSize = backupSize;
          
      /* DNM 6/17/01: display off-time features as if off-section */
      x = zapXpos(zap, pnt->x);
      y = zapYpos(zap, pnt->y);
      if (zapPointVisable(zap, pnt) && 
	  !zapTimeMismatch(vi, zap->timeLock, obj, cont)) {
        b3dColorIndex(App->foreground);
      }else{
        b3dColorIndex(App->shadow);
      }
      b3dDrawCircle(x, y, curSize);
    }
  }
     
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

  /* draw begin/end points for current contour */
  if (cont){
    if (zapTimeMismatch(vi, zap->timeLock, obj, cont))
      return;

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
  return;
}

static void zapDrawGhost(ZapStruct *zap)
{
  int co, i, base, ob;
  int red, green, blue;
  struct Mod_Object *obj;
  struct Mod_Contour *cont;
  Imod *mod = zap->vi->imod;
  int nextz, prevz, iz;

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

    // Set base to 2 to make color get brighter instead of darker
    base = (zap->vi->ghostmode & IMOD_GHOST_LIGHTER) ? 2 : 0;
    red = (int)(((base + obj->red) * 255.0) / 3.0);
    green = (int)(((base + obj->green) * 255.0) / 3.0);
    blue = (int)(((base + obj->blue) * 255.0) / 3.0);
    
    mapcolor(App->ghost, red, green, blue); 
    b3dColorIndex(App->ghost);  
    b3dLineWidth(obj->linewidth2);
    
    /* DNM: if it's RGB, just have to set the color here */
    if (App->rgba)
      glColor3f(red/255., green/255., blue/255.);

    /* DNM 6/16/01: need to be based on zap->section, not zmouse */
    nextz = zap->section + zap->vi->ghostdist;
    prevz = zap->section - zap->vi->ghostdist;
     
    for (co = 0; co < obj->contsize; co++) {
      cont = &(obj->cont[co]);
      
      /* DNM: don't display wild contours, only coplanar ones */
      /* By popular demand, display ghosts from lower and upper sections */
      if (cont->pts && !(cont->flags & ICONT_WILD)) {
        iz = (int)floor(cont->pts->z + 0.5);
        if ((iz > zap->section && iz <= nextz && 
             (zap->vi->ghostmode & IMOD_GHOST_PREVSEC)) ||
            (iz < zap->section && iz >= prevz && 
             (zap->vi->ghostmode & IMOD_GHOST_NEXTSEC))){
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
        }
      }
    }
  }
  return;
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
  if (zap->startingBand || (zap->rubberband && (moveband || dragband))) {
    if (zap->startingBand || moveband)
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

/* Return true if there are multiple images, contours have times in this 
   object, this contour has a non-zero time, and this time does not match
   current display time, which is either global time or timelock time */
bool zapTimeMismatch(ImodView *vi, int timelock, Iobj *obj, Icont *cont)
{
  int time = timelock ? timelock : vi->ct;
  return (vi->nt > 0 && iobjFlagTime(obj) && (cont->flags & ICONT_TYPEISTIME)
	  && cont->type && (time != cont->type));
}

/*
$Log$
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
