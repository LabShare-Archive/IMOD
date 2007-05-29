/*
 *  slicer.c -- Open the slicer window; Slice 3-D data at any angle.
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
#include <limits.h>
#include <string.h>
#include <qcursor.h>
#include <qapplication.h>
#include <qobjectlist.h>
#include <qdatetime.h>

#include "slicer_classes.h"
#include "imod.h"
#include "imod_display.h"
#include "b3dgfx.h"
#include "sslice.h"
#include "imod_input.h"
#include "imod_info_cb.h"
#include "control.h"
#include "imodplug.h"
#include "dia_qtutils.h"
#include "xcramp.h"
#include "xcorr.h"
#include "imod_edit.h"
#include "pixelview.h"
#include "imod_model_edit.h"
#include "imod_moviecon.h"
#include "imod_workprocs.h"
#include "preferences.h"
#include "form_slicerangle.h"
//#include "imodv_input.h"


/* internal functions. */
static void sslice_cube_draw(SlicerStruct *ss);
static void sslice_draw(SlicerStruct *ss);
static int sslice_setxyz(SlicerStruct *ss, int x, int y);
static int sslice_getxyz(SlicerStruct *ss, int x, int y, float &xm, float &ym,
                         float &zm);
static void slicer_attach_point(SlicerStruct *ss, int x, int y);
static void slicer_insert_point(SlicerStruct *ss, int x, int y);
static void slicer_modify_point(SlicerStruct *ss, int x, int y);
static void slicerUpdateImage(SlicerStruct *ss);
static void drawThickControls(SlicerStruct *ss);
static void sslice_draw_model(SlicerStruct *ss);
static void sliceSetAnglesFromPoints(SlicerStruct *ss,
                              Ipoint *p1, Ipoint *p2, int axis);
static void slicerKey_cb(ImodView *vi, void *client, int released, 
			 QKeyEvent *e);
static double fixangle(double angle);
static void slicerDraw_cb(ImodView *vi, void *client, int drawflag);
static void slicerClose_cb(ImodView *vi, void *client, int junk);
static int sslice_showslice(SlicerStruct *ss);
static void startMovieCheckSnap(SlicerStruct *ss, int dir);
static void setMovieLimits(SlicerStruct *ss, int axis);
static void findMovieAxis(SlicerStruct *ss, int *xmovie, int *ymovie, 
                          int *zmovie, int dir, int *axis);
static void setInverseMatrix(SlicerStruct *ss);
static int translateByRotatedVec(SlicerStruct *ss, Ipoint *vec);
static SlicerStruct *getTopSlicer();
static void getWindowCoords(SlicerStruct *ss, float xcur, float ycur, 
                            float zcur, int &xim, int &yim, int &zim);

/* DNM: maximum angles for sliders */
static float maxAngle[3] = {90.0, 180.0, 180.0};
static int ctrlPressed = 0;
static bool pixelViewOpen = false;
static SlicerAngleForm *sliceAngDia = NULL;
static int sliderDragging = 0; 
static QTime but1downt;
static int firstmx, firstmy, lastmx, lastmy;
static int mousePanning = 0;

/*
 * Open up slicer help dialog.
 */
void slicerHelp()
{
  imodShowHelpPage("slicer.html");
}


/*
 * Toolbar zoom arrow callbacks.
 */
void slicerStepZoom(SlicerStruct *ss, int dir)
{
  ivwControlPriority(ss->vi, ss->ctrl);
  ss->zoom = b3dStepPixelZoom(ss->zoom, dir);
  sslice_draw(ss);
  ss->qtWindow->setZoomText(ss->zoom);
}

/* A new zoom value was entered in text box */
void slicerEnteredZoom(SlicerStruct *ss, float newZoom)
{
  if (ss->closing)
    return;
  ivwControlPriority(ss->vi, ss->ctrl);
  ss->zoom = newZoom;
  if (ss->zoom <= 0.01) {
    ss->zoom = 0.01;
    ss->qtWindow->setZoomText(ss->zoom);
  }  
  sslice_draw(ss);
}
 

/* 
 * Show the location of the slice in the XYZ and Zap windows. 
 */
void slicerShowSlice(SlicerStruct *ss)
{
  slice_trans_step(ss);
  sslice_showslice(ss);
}


/*
 * Toolbar Toggle buttons
 */
void slicerStateToggled(SlicerStruct *ss, int index, int state)
{
  ivwControlPriority(ss->vi, ss->ctrl);
  switch (index) {
  case SLICER_TOGGLE_LOCK:

    /* toggle the lock button, redraw if unlocking */
    ss->locked = state;
    if (!state) {
      ss->cx = ss->vi->xmouse;
      ss->cy = ss->vi->ymouse;
      ss->cz = ss->vi->zmouse;
      ss->pending = 0;
      sslice_draw(ss);
    }
    break;

  case SLICER_TOGGLE_HIGHRES:

    /* toggle between fast rendering and highres rendering */
    ss->hq = state;
    sslice_draw(ss);
    break;

  case SLICER_TOGGLE_CENTER:

    // Toggle classic mode, set center if going to classic and not locked
    ss->classic = state;
    if (!ss->locked && state) {
      ss->cx = ss->vi->xmouse;
      ss->cy = ss->vi->ymouse;
      ss->cz = ss->vi->zmouse;
    }
    ss->pending = 0;
    sslice_draw(ss);
       
    imodDraw(ss->vi, IMOD_DRAW_XYZ);
    break;

  case SLICER_TOGGLE_FFT:
    ss->fftMode = state;
    sslice_draw(ss);
    break;

  case SLICER_TOGGLE_TIMELOCK:
    ss->timeLock = state ? ss->vi->ct : 0;
    if (!state)
      sslice_draw(ss);
    break;
  }
}

// Selection of a new zscaling option
void slicerZscale(SlicerStruct *ss, int item)
{
  ivwControlPriority(ss->vi, ss->ctrl);
  ss->scalez = item;
  sslice_draw(ss);
}


/* 
 * Tilt angle controls.
 */
void slicerAngleChanged(SlicerStruct *ss, int axis, int value, 
			int dragging)
{
  ivwControlPriority(ss->vi, ss->ctrl);
  ss->tang[axis] = value * 0.1f;
  ss->lastangle = axis;
  sliderDragging = dragging;

  // Do complete redraw if not dragging or hot slider enabled
  if (!dragging || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && ctrlPressed) ||
      (hotSliderFlag() == HOT_SLIDER_KEYUP && !ctrlPressed)) {
    sslice_draw(ss);
    sslice_showslice(ss);
    slicerCheckMovieLimits(ss);
  } else {

    // Otherwise, just draw the cube
    slice_trans_step(ss);
    sslice_cube_draw(ss);
  }
}

/****************************************************************************/
/* Thickness controls. 
 *
 * Update thickness text widgets; get new values from user
 */
static void drawThickControls(SlicerStruct *ss)
{
  ss->qtWindow->setModelThickness(ss->depth);
  ss->qtWindow->setImageThickness(ss->nslice);
}

void slicerImageThickness(SlicerStruct *ss, int sno)
{
  if (ss->closing)
    return;
  ivwControlPriority(ss->vi, ss->ctrl);
  if (sno < 1)
    sno = 1;
  ss->nslice = sno;

  drawThickControls(ss);
  sslice_draw(ss);
}

void slicerModelThickness(SlicerStruct *ss, float depth)
{
  if (ss->closing)
    return;
  ivwControlPriority(ss->vi, ss->ctrl);
  if (fabs(ss->depth - 0.1) < 0.01 && fabs(depth - 1.1) < 0.01)
    depth = 1.0;
  ss->depth = depth;
  if (ss->depth <= 0.0)
    ss->depth = 0.1;

  drawThickControls(ss);
  sslice_draw(ss);
}


/* The signal from the controller to close the window */
static void slicerClose_cb(ImodView *vi, void *client, int junk)
{
  SlicerStruct *ss = (SlicerStruct *)client;
  ss->qtWindow->close();
}

/* The window is closing now.  Clean up */
void slicerClosing(SlicerStruct *ss)
{
  ss->closing = 1;
  ivwRemoveControl(ss->vi, ss->ctrl);      
  imodDialogManager.remove((QWidget *)ss->qtWindow);
  b3dFreeCIImage(ss->image);
  imodMatDelete(ss->mat);
  free(ss);
}

/*
 * Open new slicer.
 */
int sslice_open(struct ViewInfo *vi)
{
  SlicerStruct *ss;
  int newZoom;

  ss = (SlicerStruct *)malloc(sizeof(SlicerStruct));
  if (!ss)
    return(-1);

  /* DNM 5/16/02: if the current position is still in the lower left
     corner, move it to middle and draw other windows */
  /* 5/5/03: take away DRAW IMAGE flag */
  if (!vi->xmouse && !vi->ymouse) {
    vi->xmouse = vi->xsize / 2;
    vi->ymouse = vi->ysize / 2;
    imodDraw(vi, IMOD_DRAW_XYZ);
  }

  ss->cx = vi->xmouse;
  ss->cy = vi->ymouse;
  ss->cz = vi->zmouse;
  ss->vi     = vi;
  ss->locked = 0;
  ss->timeLock = 0;
  ss->classic = 0;
  ss->zoom   = 1.0;
  ss->lx     = vi->xmouse;
  ss->ly     = vi->ymouse;
  ss->lz     = vi->zmouse;
  ss->origAngles[0]  = -1000.;
  ss->hq     = 0;
  ss->tang[b3dX] = 0.0f;
  ss->tang[b3dY] = 0.0f;
  ss->tang[b3dZ] = 0.0f;
  ss->lang[b3dX] = 0.0f;
  ss->lang[b3dY] = 0.0f;
  ss->lang[b3dZ] = 0.0f;
  ss->scalez = 0;
  ss->depth = 1.0;
  ss->image = NULL;
  ss->xstep[0]  = 1.0f; ss->xstep[1] = ss->xstep[2] = 0.0f;
  ss->ystep[1]  = 1.0f; ss->ystep[0] = ss->ystep[2] = 0.0f;
  ss->nslice = 1;
  ss->mat = imodMatNew(3);
  ss->lastangle = b3dX;
  ss->zslast = 1.0;
  ss->pending = 0;
  ss->imageFilled = 0;
  ss->mousemode = vi->imod->mousemode;
  ss->movieSnapCount = 0;
  ss->fftMode = 0;
  ss->closing = 0;

  slice_trans_step(ss);
  ss->qtWindow = new SlicerWindow(ss, maxAngle,  vi->nt > 1, App->rgba,
                                  App->doublebuffer, App->qtEnableDepth,
                                  imodDialogManager.parent(IMOD_IMAGE),
                                  "slicer window");
  if (!ss->qtWindow){
    free(ss);
    wprint("Error opening slicer window.");
    return(-1);
  }
  ss->glw = ss->qtWindow->mGLw;
  ss->cube = ss->qtWindow->mCube;
  if (!App->rgba)
    ss->glw->setColormap(*(App->qColormap));

  ss->qtWindow->setCaption(imodCaption("3dmod Slicer"));
  ss->qtWindow->mToolBar->setLabel(imodCaption("Slicer Toolbar 1"));
  ss->qtWindow->mToolBar2->setLabel(imodCaption("Slicer Toolbar 2"));
	
  ss->ctrl = ivwNewControl(vi, slicerDraw_cb, slicerClose_cb, slicerKey_cb,
                               (void *)ss);
  imodDialogManager.add((QWidget *)ss->qtWindow, IMOD_IMAGE, 
                        SLICER_WINDOW_TYPE);

  // Set up cursor
  if (ss->mousemode == IMOD_MMODEL)
    ss->glw->setCursor(*App->modelCursor);

  // Initialize controls
  ss->qtWindow->setZoomText(ss->zoom);
  drawThickControls(ss);
  ss->qtWindow->setAngles(ss->tang);

  // Include this to get toolbar sizes right
  imod_info_input();
  
  QSize toolSize1 = ss->qtWindow->mToolBar->sizeHint();
  QSize toolSize2 = ss->qtWindow->mToolBar2->sizeHint();
  int newWidth = toolSize1.width() > toolSize2.width() ?
    toolSize1.width() : toolSize2.width();
  int newHeight = newWidth + toolSize1.height() + toolSize2.height();
  ss->qtWindow->resize( newWidth, newHeight);

  // Adjust zoom to biggest one that fits image
  if (vi->xsize < newWidth && vi->ysize < newWidth) {
    ss->zoom = (2. * newWidth) / vi->xsize;
    while ((ss->zoom * vi->xsize > 1.06 * newWidth || 
           ss->zoom * vi->ysize > 1.06 * newWidth)) {
      newZoom = b3dStepPixelZoom(ss->zoom, -1);
      if (fabs(newZoom - ss->zoom) < 0.0001)
        break;
      ss->zoom = (float)newZoom;
    }
    ss->qtWindow->setZoomText(ss->zoom);
  }

  ss->qtWindow->show();
  ss->glw->setMouseTracking(pixelViewOpen);

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
                                    "slicer angles", Qt::WType_TopLevel | 
                                    Qt::WDestructiveClose);
  if (!sliceAngDia)
    return -1;
  imodDialogManager.add((QWidget *)sliceAngDia, IMOD_DIALOG);
  sliceAngDia->show();
  return 0;
}

// The slicer angle window is closing
void slicerAnglesClosing()
{
  imodDialogManager.remove((QWidget *)sliceAngDia);
  sliceAngDia = NULL;
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
  SlicerStruct *ss = getTopSlicer();

  if (!ss) {
    imodPrintStderr("ERROR: No slicer windows open\n");
    return;
  }
  imodPrintStderr("Slicer angles: %.1f %.1f %.1f\n", ss->tang[b3dX],
                  ss->tang[b3dY], ss->tang[b3dZ]);
}

// Find the slicer window that is nearest to the top of the control list
static SlicerStruct *getTopSlicer()
{
  QObjectList objList;
  SlicerStruct *ss;
  ImodControl *ctrlPtr;
  int i, j, topOne;

  imodDialogManager.windowList(&objList, -1, SLICER_WINDOW_TYPE);
  if (!objList.count())
    return NULL;

  // Look through the current control list, find first slicer with matching ID
  topOne = -1;
  for (j = 0; topOne < 0 && j < ilistSize(App->cvi->ctrlist->list); j++) {
    ctrlPtr = (ImodControl *)ilistItem(App->cvi->ctrlist->list, j);
    for (i = 0; i < objList.count(); i++) {
      ss = ((SlicerWindow *)objList.at(i))->mSlicer;
      if (ctrlPtr->id == ss->ctrl) {
        topOne = i;
        break;
      }
    }
  }
  if (topOne < 0)
    topOne = 0;
  return ((SlicerWindow *)objList.at(topOne))->mSlicer;
}

// Called from slicer angle window to set the angles and position of the
// top slicer
int setTopSlicerAngles(float angles[3], Ipoint *center, bool draw)
{
  SlicerStruct *ss = getTopSlicer();
  if (!ss)
    return 1;
  ss->tang[b3dX] = angles[0];
  ss->tang[b3dY] = angles[1];
  ss->tang[b3dZ] = angles[2];
  ss->qtWindow->setAngles(ss->tang);
  ss->cx = B3DMAX(0., B3DMIN(center->x, ss->vi->xsize - 1));
  ss->cy = B3DMAX(0., B3DMIN(center->y, ss->vi->ysize - 1));
  ss->cz = B3DMAX(0., B3DMIN(center->z, ss->vi->zsize - 1));
  if (!ss->locked) {
    ss->vi->xmouse = ss->cx;
    ss->vi->ymouse = ss->cy;
    ss->vi->zmouse = ss->cz;
    if (draw)
      imodDraw(ss->vi, IMOD_DRAW_XYZ | IMOD_DRAW_SLICE | IMOD_DRAW_ACTIVE);
  } else if (draw) {
    sslice_draw(ss);
    sslice_showslice(ss);
  }
  return 0;
}

// Return the angles and position of the top slicer, and the time, to the 
// slicer angle window
int getTopSlicerAngles(float angles[3], Ipoint *center, int &time)
{
  SlicerStruct *ss = getTopSlicer();
  if (!ss)
    return 1;
  angles[0] = ss->tang[b3dX];
  angles[1] = ss->tang[b3dY];
  angles[2] = ss->tang[b3dZ];
  center->x = ss->cx;
  center->y = ss->cy;
  center->z = ss->cz;
  time = ss->timeLock ? ss->timeLock : ss->vi->ct;
  return 0;
}

// Just return the time of the top slicer window
int getTopSlicerTime()
{
  SlicerStruct *ss = getTopSlicer();
  if (!ss)
    return -1;
  return ss->timeLock ? ss->timeLock : ss->vi->ct;
}

// Notify the slicer angle dialog of a new time or general need to refresh
void slicerNewTime(bool refresh)
{
  if (sliceAngDia)
    sliceAngDia->newTime(refresh);
}


/* Broadcast position of this slice, and let other windows
 * show where this slice intersects with their views.
 * DNM 1/6/03: removed extraneous code
 */
static int sslice_showslice(SlicerStruct *ss)
{
  imodDraw(ss->vi, IMOD_DRAW_SLICE);
  return(0);
}

// A key passed on from elsewhere
// Do not pass on the hot slider key that would cause a grab
static void slicerKey_cb(ImodView *vi, void *client, int released, 
			 QKeyEvent *e)
{
  SlicerStruct *slicer = (SlicerStruct *)client;
  if (e->key() == hotSliderKey())
    return;
  if (released)
    slicerKeyRelease(slicer, e);
  else
    slicerKeyInput(slicer, e);
}

// Key press input
void slicerKeyInput(SlicerStruct *ss, QKeyEvent *event)
{
  int keysym = event->key();
  int shift = event->state() & Qt::ShiftButton;
  int ctrl = event->state() & Qt::ControlButton;
  int lang = ss->lastangle;
  int dodraw = 1;
  int handled = 1;
  int docheck = 0;
  int keypad = event->state() & Qt::Keypad;
  float unit;
  Ipoint *p1, *p2;
  int ob, co, pt, axis;
  Icont *cont;
  Ipoint norm, vec = {0., 0., 0.};
  ImodView *vi = ss->vi;

  if (inputTestMetaKey(event))
    return;

  inputConvertNumLock(keysym, keypad);

  ivwControlPriority(vi, ss->ctrl);

  if (imodPlugHandleKey(vi, event)) 
    return;

  // These grabs may not work right if keys are passed from elsewhere
  if (keysym == hotSliderKey()) {
    ctrlPressed = true;
    ss->qtWindow->grabKeyboard();
    return;
  }

  switch(keysym){
          
  case Qt::Key_Plus:
    ss->nslice++;
    drawThickControls(ss);
    break;
    
  case Qt::Key_Equal:
    ss->zoom = b3dStepPixelZoom(ss->zoom, 1);
    ss->qtWindow->setZoomText(ss->zoom);
    break;

  case Qt::Key_Underscore:
    ss->nslice--;
    if (ss->nslice < 1) 
      ss->nslice = 1;
    drawThickControls(ss);
    break;

  case Qt::Key_Minus:
    ss->zoom = b3dStepPixelZoom(ss->zoom, -1);
    ss->qtWindow->setZoomText(ss->zoom);
    break;

  case Qt::Key_9:
    ss->depth -= 1.0;
    if (ss->depth < 1.0)
      ss->depth  = 1.0;
    drawThickControls(ss);
    break;

  case Qt::Key_0:
    ss->depth += 1.0;
    drawThickControls(ss);
    break;
          
  case Qt::Key_1:
  case Qt::Key_2:
    if (ss->timeLock) {
      if (keysym == Qt::Key_1)
        ss->timeLock = B3DMAX(ss->timeLock - 1, 1);
      else
        ss->timeLock = B3DMIN(ss->timeLock + 1, vi->nt);
    } else
      handled = 0;

  case Qt::Key_S:
    if (shift || ctrl){

      // Snapshots: need to update just the image window
      ss->glw->updateGL();
      b3dKeySnapshot("slicer", shift, ctrl, NULL);
    }else
      sslice_showslice(ss);
    dodraw = 0;
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
        sliceSetAnglesFromPoints(ss, p1, p2, axis);
        docheck = 1;
      } else
        dodraw = 0;
    }
    break;

  case Qt::Key_W:
    if (shift) {
      if (!slicerAnglesFromContour(ss))
        docheck = 1;
      dodraw = 0;
    } else
      handled = 0;

    /* DNM: add these to adjust last angle, now that input is properly
       passed to the window */
  case Qt::Key_Up:
  case Qt::Key_Down:
  case Qt::Key_Right:
  case Qt::Key_Left:
  case Qt::Key_Insert:
  case Qt::Key_End:
  case Qt::Key_Next:
  case Qt::Key_Prior:
    dodraw = 0;
    if ((keypad && keysym == Qt::Key_Prior) || 
        (!keypad && ((!ss->locked && ss->classic) || keysym == Qt::Key_End || 
                     keysym == Qt::Key_Insert))) {
      handled = 0;
      break;
    }

    // Non-keypad keys, only if locked or new mode, move without changing 
    // global point
    // and move in tilted coordinates so that modeling will be one pixel apart
    if (!keypad) {
      unit = ss->classic ? -1. : +1.;
      if (keysym == Qt::Key_Down) 
        vec.y = unit;
      if (keysym == Qt::Key_Left)
        vec.x = unit;
      if (keysym == Qt::Key_Right)
        vec.x = -unit;
      if (keysym == Qt::Key_Up)
        vec.y = -unit;
      if (keysym == Qt::Key_Prior)
        vec.z = 1.;
      if (keysym == Qt::Key_Next)
        vec.z = -1;
      if (translateByRotatedVec(ss, &vec)) {
        dodraw = 1;
        if (!ss->locked && vec.z) {
          
          // For a move in Z, try to move current image point normal to the
          // plane.  First test if this move is a continuation of a sequence
          // and if so just increment the cumulative move; otherwise set this
          // up as the first move of a potential sequence
          if (ss->tang[0] == ss->origAngles[0] && 
              ss->tang[1] == ss->origAngles[1] && 
              ss->tang[2] == ss->origAngles[2] && 
              vi->xmouse == ss->lastXYZmouse.x && 
              vi->ymouse == ss->lastXYZmouse.y && 
              vi->zmouse == ss->lastXYZmouse.z) {
            ss->cumPageMoves += vec.z;
          } else {
            ss->origAngles[0] = ss->tang[0];
            ss->origAngles[1] = ss->tang[1];
            ss->origAngles[2] = ss->tang[2];
            ss->origXYZmouse.x = vi->xmouse;
            ss->origXYZmouse.y = vi->ymouse;
            ss->origXYZmouse.z = vi->zmouse;
            ss->cumPageMoves = vec.z;
          }
          vec.z = 1.;
          imodMatTransform3D(ss->mat, &vec, &norm);

          // Now move the [xyz]mouse by cumulative move from original position
          // and round z to integer
          vi->xmouse = ss->origXYZmouse.x + ss->cumPageMoves * norm.x;
          vi->ymouse = ss->origXYZmouse.y + ss->cumPageMoves * norm.y;
          vi->zmouse = B3DNINT(ss->origXYZmouse.z + ss->cumPageMoves * norm.z);
          ss->lastXYZmouse.x = vi->xmouse;
          ss->lastXYZmouse.y = vi->ymouse;
          ss->lastXYZmouse.z = vi->zmouse;
          ivwBindMouse(vi);
          imodDraw(vi, IMOD_DRAW_XYZ);
          dodraw = 0;        
        }
      }
      break;
    }

    // Now handle keypad keys
    if (keysym == Qt::Key_Down) 
      ss->tang[lang] -= 0.5;
    if (keysym == Qt::Key_Left)
      ss->tang[lang] -= 0.1;
    if (keysym == Qt::Key_Right)
      ss->tang[lang] += 0.1;
    if (keysym == Qt::Key_Up)
      ss->tang[lang] += 0.5;
    if (keysym == Qt::Key_End)
      ss->tang[lang] -= 15.0;
    if (keysym == Qt::Key_Next)
      ss->tang[lang] += 15.0;
    if (keysym == Qt::Key_Insert)
      ss->tang[lang] = 0.0;
    if (ss->tang[lang] > maxAngle[lang])
      ss->tang[lang] = maxAngle[lang];
    if (ss->tang[lang] < -maxAngle[lang])
      ss->tang[lang] = -maxAngle[lang];

    ss->qtWindow->setAngles(ss->tang);

    sslice_draw(ss);
    sslice_showslice(ss);
    docheck = 1;
    break;

  case Qt::Key_Escape:
    ss->qtWindow->close();
    dodraw = 0;
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
    sslice_draw(ss);         
  if (docheck)
    slicerCheckMovieLimits(ss);
}

// On any key release, clear the ctrl pressed flag and release the keyboard
void slicerKeyRelease(SlicerStruct *ss, QKeyEvent *event)
{
  if (ctrlPressed)
    ss->qtWindow->releaseKeyboard();
  ctrlPressed = false;
}

// Process press of mouse buttons
void slicerMousePress(SlicerStruct *ss, QMouseEvent *event)
{
  ivwControlPriority(ss->vi, ss->ctrl);

  if (event->stateAfter() & ImodPrefs->actualButton(1)) {
    if (ss->classic) {
      slicer_attach_point(ss, event->x(), event->y());
    } else {
      but1downt.start();
      lastmx = firstmx = event->x();
      lastmy = firstmy = event->y();
    }
  }

  if (event->stateAfter() & ImodPrefs->actualButton(2))
    slicer_insert_point(ss, event->x(), event->y());
  
  if (event->stateAfter() & ImodPrefs->actualButton(3))
    slicer_modify_point(ss, event->x(), event->y());
}

// Respond to mouse button up
void slicerMouseRelease(SlicerStruct *ss, QMouseEvent *event)
{

  // For button 1 up, if not classic mode, either end panning or call attach
  if (event->button() == ImodPrefs->actualButton(1) && !ss->classic) {
    if (mousePanning) {
      mousePanning = 0;
      sslice_draw(ss);
    } else
      slicer_attach_point(ss, event->x(), event->y());
  }
}

// Process a mouse move
void slicerMouseMove(SlicerStruct *ss, QMouseEvent *event)
{
  int zmouse;
  float xm, ym, zm;
  Ipoint cpt;
  Icont *cont;
  Imod *imod = ss->vi->imod;
  Ipoint scale = {1., 1., 1.};
  int cumdx, cumdy;
  int cumthresh = 6 * 6;
  double transFac = ss->zoom < 4. ? 1. / ss->zoom : 0.25;
  Ipoint vec;
 
  if (pixelViewOpen) {
    zmouse = sslice_getxyz(ss, event->x(), event->y(), xm, ym, zm);
    pvNewMousePosition(ss->vi, xm, ym, zmouse);
  }

  // Pan with button 1 if not classic mode
  if ((event->state() & ImodPrefs->actualButton(1)) && !ss->classic) {
    cumdx = event->x() - firstmx;
    cumdy = event->y() - firstmy;
    if (mousePanning || but1downt.elapsed() > 250 || 
        cumdx * cumdx + cumdy * cumdy > cumthresh) {
      vec.x = (lastmx - event->x()) * transFac;
      vec.y = (event->y() - lastmy) * transFac;
      vec.z = 0.;
      mousePanning = 1;
      if (translateByRotatedVec(ss, &vec))
        sslice_draw(ss);
    }

    lastmx = event->x();
    lastmy = event->y();
  }

  // Button 2 in model mode, locked or new mode, add points
  if ((event->state() & ImodPrefs->actualButton(2)) && 
      (ss->locked || !ss->classic) && imod->mousemode == IMOD_MMODEL) {
    zmouse = sslice_setxyz(ss, event->x(), event->y());
    cpt.x = ss->vi->xmouse;
    cpt.y = ss->vi->ymouse;
    cpt.z = ss->vi->zmouse;
    cont = imodContourGet(imod);
    if (!cont || imod->cindex.point < 0)
      return;
    if (imodPoint3DScaleDistance
        (&(cont->pts[imod->cindex.point]), &cpt, &scale) > 
        scaleModelRes(imod->res, ss->zoom)) {
      inputInsertPoint(ss->vi);
      ss->vi->zmouse = zmouse;
    }
  }
}

// Process first mouse button - attach in model, set current point in movie
static void slicer_attach_point(SlicerStruct *ss, int x, int y)
{
  Ipoint pnt;
  Ipoint *spnt;
  int i;
  float temp_distance;
  float distance = -1.;
  ImodView *vi = ss->vi;
  Imod *imod = vi->imod;
  Iindex index;
  float selsize = IMOD_SELSIZE / ss->zoom;
  int drawflag = IMOD_DRAW_XYZ;

  vi->zmouse = sslice_setxyz(ss, x, y);
  if (imod->mousemode == IMOD_MMODEL) {
    pnt.x = vi->xmouse;
    pnt.y = vi->ymouse;
    pnt.z = vi->zmouse;
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
        spnt = imodPointGet(imod);
        if (spnt){
          vi->xmouse = spnt->x;
          vi->ymouse = spnt->y;
	  vi->zmouse = spnt->z;
        }
      }
    }
    /* DNM 5/5/03: take out IMAGE flag, use RETHINK only if in model mode */
    drawflag |= IMOD_DRAW_RETHINK;
  }

  /* DNM: for select hits, do keep cz at an integral value */
  ss->pending = 0;
  imodDraw(ss->vi, drawflag);
}

static void slicer_insert_point(SlicerStruct *ss, int x, int y)
{
  int zmouse;
  if (ss->vi->imod->mousemode == IMOD_MMODEL) {
    zmouse = sslice_setxyz(ss, x, y);
    inputInsertPoint(ss->vi);
    ss->vi->zmouse = zmouse;
  } else
    startMovieCheckSnap(ss, 1);
}

static void slicer_modify_point(SlicerStruct *ss, int x, int y)
{
  int zmouse;
  if (ss->vi->imod->mousemode == IMOD_MMODEL) {
    zmouse = sslice_setxyz(ss, x, y);
    inputModifyPoint(ss->vi);
    ss->vi->zmouse = zmouse;
  } else
    startMovieCheckSnap(ss, -1);
}

/*
 * MOVIE RELATED ROUTINES
 */

// Find dominant axis of this slicer window and set movie arguments to match
static void findMovieAxis(SlicerStruct *ss, int *xmovie, int *ymovie, 
                                int *zmovie, int dir, int *axis)
{
  *xmovie = *ymovie = *zmovie = 0;
  double xcomp = fabs((double)ss->zstep[b3dX]);
  double ycomp = fabs((double)ss->zstep[b3dY]);
  double zcomp = fabs((double)ss->zstep[b3dZ]);

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
static void setMovieLimits(SlicerStruct *ss, int axis)
{
  int i, size, end, start = -1;
  float cur, cx, cy, cz;

  // Get the size and current value for the dominant axis
  if (axis == 0) {
    size = ss->vi->xsize;
    cur = ss->cx;
  } else if (axis == 1) {
    size = ss->vi->ysize;
    cur = ss->cy;
  } else {
    size = ss->vi->zsize;
    cur = ss->cz;
  }

  // Loop on positions on dominant axis and find position on each axis
  for (i = 0; i < size; i++) {
    cx = ss->cx + (i - cur) * ss->zstep[0] / ss->zstep[axis];
    cy = ss->cy + (i - cur) * ss->zstep[1] / ss->zstep[axis];
    cz = ss->cz + (i - cur) * ss->zstep[2] / ss->zstep[axis];
    if (cx >= 0. && cx <= ss->vi->xsize - 1 && 
        cy >= 0. && cy <= ss->vi->ysize - 1 &&
        cz >= 0. && cz <= ss->vi->zsize - 1) {

      // Set start on first legal position and end on any legal one
      end = i;
      if (start < 0)
        start = i;
    }
  }

  imcSetSpecialLimits(axis, start, end);
}

// Check for movie and set new limits when angles have changed
void slicerCheckMovieLimits(SlicerStruct *ss)
{
  int xmovie, ymovie, zmovie;
  int axis;
  ImodView *vi = ss->vi;

  // If no movie or it was not started by this slicer, done
  if (!(vi->xmovie || vi->ymovie || vi->zmovie) || 
      imcGetStarterID() != ss->ctrl)
    return;

  // See if dominant axis has changed and restart movie
  findMovieAxis(ss, &xmovie, &ymovie, &zmovie, 1, &axis);
  if ((vi->xmovie && axis != 0) || (vi->ymovie && axis != 1) || 
      (vi->zmovie && axis != 2)) {

    // If sign of new motion along old axis (xstep[old]/xstep[axis]) is 
    // opposite to sign of old motion (oldmovie), then need to restart
    // with negative direction.  Only one term of old sum counts.
    if ((vi->xmovie * ss->zstep[b3dX] + vi->ymovie * ss->zstep[b3dY] + 
         vi->zmovie * ss->zstep[b3dZ]) / ss->zstep[axis] < 0.)
      findMovieAxis(ss, &xmovie, &ymovie, &zmovie, -1, &axis);
    imodMovieXYZT(vi, xmovie, ymovie, zmovie, 0);
  }

  // In any case, set new movie limits
  setMovieLimits(ss, axis);
}

// Start a movie and set up for autosnapshotting
static void startMovieCheckSnap(SlicerStruct *ss, int dir)
{
  int xmovie, ymovie, zmovie;
  int axis, start, end;
  ImodView *vi = ss->vi;

  // Find dominant axis and start/stop movie along that axis
  findMovieAxis(ss, &xmovie, &ymovie, &zmovie, dir, &axis);
  imodMovieXYZT(vi, xmovie, ymovie, zmovie, 0);
  imcSetStarterID(ss->ctrl);

  // Just zero the snap count
  ss->movieSnapCount = 0;

  /* done if no movie, otherwise set the movie limits */
  if (!(vi->zmovie || vi->xmovie || vi->ymovie)) 
    return;

  setMovieLimits(ss, axis);

  // now done if no snapshots are desired.
  if (!imcGetSnapshot(vi))
    return;
     
  /* Get start and end of loop, compute count */
  imcGetStartEnd(vi, axis, &start, &end);
  ss->movieSnapCount = (end - start) / imcGetIncrement(vi, axis) + 1;
  if (ss->movieSnapCount < 1)
    ss->movieSnapCount = 1;

  /* double count for normal mode, leave as is for one-way */
  if (!imcGetLoopMode(vi))
    ss->movieSnapCount *= 2;

  /* Set to start or end depending on which button was hit */
  if (!imcStartSnapHere(vi)) {
    if (axis == 0)
      vi->xmouse = dir > 0 ? start : end;
    else if (axis == 1)
      vi->ymouse = dir > 0 ? start : end;
    else
      vi->zmouse = dir > 0 ? start : end;
  }

  /* draw - via imodDraw to get float and positioning done correctly */
  imodDraw(vi, IMOD_DRAW_XYZ);
}

// Get the Z scale of the volume before slicing: it is the product of
// an implicit Z scael from the ration of Z to XY binning, and actual z scale
// if zscale before is selected 
float slicerGetZScaleBefore(SlicerStruct *ss)
{
  float zs = (float)ss->vi->zbin / (float)ss->vi->xybin;
  if (ss->scalez == SLICE_ZSCALE_BEFORE && ss->vi->imod->zscale > 0)
    zs *= ss->vi->imod->zscale;
  return zs;
}

// Set the X, y, z coordinates based on the point in the window
static int sslice_setxyz(SlicerStruct *ss, int x, int y)
{
  float xm, ym, zm;
  int zmouse;

  zmouse = sslice_getxyz(ss, x, y, xm, ym, zm);

  /* Set up pending coordinates for next draw to use */
  ss->pendx = xm;
  ss->pendy = ym;
  ss->pendz = zm;
  ss->pending = 1;

  /* DNM: do this regardless of whether locked or not.  
     9/8/06: changed from setting zmouse to the integral value to
     setting it the real value but returning the integral value for the
     call to set after point insertion/modification. */
  ss->vi->xmouse = xm; 
  ss->vi->ymouse = ym; 
  ss->vi->zmouse = zm;
  //imodPrintStderr("setxyz %f %f %f\n", xm, ym, zm);
  return zmouse;
}

// Compute the x, y, z coordinates corresponding to a point in window
static int sslice_getxyz(SlicerStruct *ss, int x, int y, float &xm, float &ym,
                         float &zm)
{
  float xoffset, yoffset;
  float zs;
  Ipoint delpt;

  /* DNM: the xzoom and yzoom are correct for all cases, and the zs
     needs to be applied only for the case of SCALE_BEFORE */

  zs = 1.0f / slicerGetZScaleBefore(ss);
  //  if (ss->scalez == SLICE_ZSCALE_BEFORE && ss->vi->imod->zscale > 0)
  //  zs = 1.0 / ss->vi->imod->zscale;

  /* DNM: have to use integer arithmetic on window sizes, and account for
     y going from 0 to winy-1 when inverting it */

  xoffset = ((ss->winx / 2) - x) / ss->xzoom;
  yoffset = ((ss->winy / 2) - (ss->winy - 1 - y)) / ss->yzoom;
     
  /* DNM: always take position relative to current display center,
     regardless of whether the window is locked (it was using [xyz]mouse */
  xm = ss->cx;
  ym = ss->cy;
  zm = ss->cz;

  delpt.x = (ss->xstep[b3dX] * xoffset) + (ss->ystep[b3dX] * yoffset);
  delpt.y = (ss->xstep[b3dY] * xoffset) + (ss->ystep[b3dY] * yoffset);
  delpt.z = (ss->xstep[b3dZ] * xoffset * zs) + (ss->ystep[b3dZ] * yoffset *zs);
  
  /*
  slicerSetForwardMatrix(ss);
  imodMatTransform3D(ss->mat, &delpt, &rotpt);
  printf("angles %.2f %.2f rotpt %f %f %f\n", ss->tang[b3dX], ss->tang[b3dY],
         rotpt.x, rotpt.y, rotpt.z);
  */
  xm -= delpt.x;
  ym -= delpt.y;
  zm -= delpt.z;

  xm = B3DMIN(ss->vi->xsize - 1, B3DMAX(0, xm));
  ym = B3DMIN(ss->vi->ysize - 1, B3DMAX(0, ym));
  zm = B3DMIN(ss->vi->zsize - 0.51, B3DMAX(-0.49, zm));

  return((int)floor(zm + 0.5));
}

// Get the window coordinates corresponding to the given image point 
// coordinates as integer values; zim should be 0 if the point is in the 
// current plane
static void getWindowCoords(SlicerStruct *ss, float xcur, float ycur, 
                            float zcur, int &xim, int &yim, int &zim)
{
  Ipoint pnt, xn, yn, zn, delpt;
  Imat *mat = ss->mat;
  float xoffset, yoffset;
  float zs;
  zs = 1.0f / slicerGetZScaleBefore(ss);

  slicerSetForwardMatrix(ss);

  pnt.y = pnt.z = 0.0f;
  pnt.x = 1.0f;
  imodMatTransform3D(mat, &pnt, &xn);
  pnt.x = 0.0f;
  pnt.y = 1.0f;
  imodMatTransform3D(mat, &pnt, &yn);
  pnt.y = 0.0f;
  pnt.z = 1.0f;
  imodMatTransform3D(mat, &pnt, &zn);
  xcur -= ss->cx;
  ycur -= ss->cy;
  zcur = (zcur - ss->cz) / zs;
  xoffset = xn.x * xcur + yn.x * ycur + zn.x * zcur;
  yoffset = xn.y * xcur + yn.y * ycur + zn.y * zcur;
  zim = B3DNINT(xn.z * xcur + yn.z * ycur + zn.z * zcur);
  xim = B3DNINT(xoffset * ss->xzoom + ss->winx / 2);
  yim = B3DNINT(yoffset * ss->yzoom + ss->winy / 2);
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

static double fixangle(double angle)
{
  double rpd = RADIANS_PER_DEGREE;
  if (angle <= -180. * rpd)
    angle += 360. * rpd;
  if (angle > 180. * rpd)
    angle -= 360. * rpd;
  return angle;
}

// Compose the forward transformation matrix with the current angles
void slicerSetForwardMatrix(SlicerStruct *ss)
{
  imodMatId(ss->mat);
  imodMatRot(ss->mat, (double)ss->tang[b3dZ], b3dZ);
  imodMatRot(ss->mat, (double)ss->tang[b3dY], b3dY);
  imodMatRot(ss->mat, (double)ss->tang[b3dX], b3dX); 
}

// Compose the inverse transformation matrix with the current angles
static void setInverseMatrix(SlicerStruct *ss)
{
  imodMatId(ss->mat);
  imodMatRot(ss->mat, (double)(-ss->tang[b3dX]), b3dX); 
  imodMatRot(ss->mat, (double)(-ss->tang[b3dY]), b3dY);
  imodMatRot(ss->mat, (double)(-ss->tang[b3dZ]), b3dZ);
}

// Translates the center position within or normal to plane, return 1 if the
// position is changed
static int translateByRotatedVec(SlicerStruct *ss, Ipoint *vec)
{
  Ipoint norm;
  setInverseMatrix(ss);
  imodMatTransform3D(ss->mat, vec, &norm);
  if (ss->cx + norm.x >= 0 && ss->cx + norm.x < ss->vi->xsize && 
      ss->cy + norm.y >= 0 && ss->cy + norm.y < ss->vi->ysize && 
      ss->cz + norm.z >= 0 && ss->cz + norm.z < ss->vi->zsize - 0.5) {
    ss->cx += norm.x;
    ss->cy += norm.y;
    ss->cz += norm.z;
    return 1;
  }
  return 0;
}

static void sliceSetAnglesFromPoints(SlicerStruct *ss,
                                     Ipoint *p1, Ipoint *p2, int axis)
{
  Ipoint n;
  Ipoint a;
  double val;
  float smallVal = 1.e-4;
  double rpd = RADIANS_PER_DEGREE;
  n.x = p2->x - p1->x;
  n.y = p2->y - p1->y;
  n.z = p2->z - p1->z;
  n.z *= slicerGetZScaleBefore(ss);
  //  if (ss->scalez == SLICE_ZSCALE_BEFORE)
  //  n.z *= ss->vi->imod->zscale;
  if (n.x == 0.0 && n.y == 0.0 && n.z == 0.0)
    return;
  imodPointNormalize(&n);

  if (axis == b3dX) {
    a.z = 0.0;
    if (n.x > smallVal || n.y > smallVal || n.x < -smallVal || n.y < -smallVal)
      a.z = -atan2((double)n.y, (double)n.x);
    val = a.z;
    val = n.x * cos(val) - n.y * sin(val);
    a.y = fixangle(90. * rpd - atan2(val, (double)n.z));
    a.x = 0.0;
  } else if (axis == b3dY) {
    a.z = 0.0;
    if (n.x > smallVal || n.y > smallVal || n.x < -smallVal || n.y < -smallVal)
      a.z = fixangle(90. * rpd - atan2((double)n.y, (double)n.x));
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
         a.x = fixangle(90. * rpd - atan2(val, (double)n.y));
         a.z = 0.0; */

    /* But this is useful for doing Z rotation and then X, and for
       keeping all the angles under +/- 90 degrees. */
    a.z = 0.0;
    if (n.x > smallVal || n.y > smallVal || n.x < -smallVal || n.y < -smallVal)
      if (n.y >= 0.0)
        a.z = atan2((double)n.x, (double)n.y);
      else
        a.z = -atan2((double)n.x, -(double)n.y);
    val = a.z;
    val = n.x * sin(val) + n.y * cos(val);
    if (n.z >= 0.0)
      a.x = atan2(val, (double)n.z);
    else
      a.x = -atan2(val, -(double)n.z);
    a.y = 0.0;
  }

  ss->tang[b3dX] = a.x / rpd;
  ss->tang[b3dY] = a.y / rpd;
  ss->tang[b3dZ] = a.z / rpd;
  ss->qtWindow->setAngles(ss->tang);
}

/*
 * Find the angles that make current contour flat and set center to its center
 */
int slicerAnglesFromContour(SlicerStruct *ss)
{
  Icont *rcont;
  Icont *cont = imodContourGet(ss->vi->imod);
  Ipoint scale = {1., 1., 1.};
  Ipoint n, a;
  int pt;
  float smallVal = 1.e-4;
  float dval;
  double rpd = RADIANS_PER_DEGREE;
  double beta, alpha, zrot;
  double xsum, ysum, zsum;

  scale.z = slicerGetZScaleBefore(ss);
  if (!cont || !cont->psize)
    return 1;
  rcont = imodContourDup(cont);
  if (!rcont) {
    wprint("\aMemory error trying to copy contour.\n");
    return 1;
  }

  // Rotate the points in the contour by the current Z so that the X and Y
  // can be solved on top of the current Z
  imodMatId(ss->mat);
  imodMatRot(ss->mat, (double)(ss->tang[b3dZ]), b3dZ);
  xsum = ysum = zsum = 0.;
  for (pt = 0; pt < cont->psize; pt++) {
    imodMatTransform3D(ss->mat, &cont->pts[pt], &rcont->pts[pt]);
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
  ss->tang[b3dX] = alpha / rpd;
  ss->tang[b3dY] = beta / rpd;
  /* printf("norm %f %f %f alpha %.2f  beta %.2f\n", 
     n.x, n.y, n.z, alpha/rpd, beta/rpd); */
  ss->qtWindow->setAngles(ss->tang);

  // Set the center point and mouse point using the floating Z value, do draw
  // If you set zmouse integer, cz needs to be reset AND displays with Z scale
  // alternate between between right and wrong when lock is off
  ss->cx = B3DMAX(0., B3DMIN(xsum / cont->psize, ss->vi->xsize - 1));
  ss->cy = B3DMAX(0., B3DMIN(ysum / cont->psize, ss->vi->ysize - 1));
  ss->cz = B3DMAX(0., B3DMIN(zsum / cont->psize, ss->vi->zsize - 1));
  ss->vi->xmouse = ss->cx;
  ss->vi->ymouse = ss->cy;
  ss->vi->zmouse = ss->cz;
  imodDraw(ss->vi, IMOD_DRAW_XYZ);

  return 0;
}


/* set up the step factors for a new slice angle. */
void slice_trans_step(SlicerStruct *ss)
{
  Ipoint pnt;
  Ipoint xn, yn, zn;
  Ipoint normal, xcut, ycut, zcut;
  float x,y,z;
  float isize, jsize, zs;
  Imat *mat = ss->mat;

  /* DNM: made the angles be negative to match angles applied to volume */
  
  setInverseMatrix(ss);

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

  ss->xstep[b3dX] = xn.x;
  ss->xstep[b3dY] = xn.y;
  ss->xstep[b3dZ] = xn.z;
  ss->ystep[b3dX] = yn.x;
  ss->ystep[b3dY] = yn.y;
  ss->ystep[b3dZ] = yn.z;
  ss->zstep[b3dX] = zn.x;
  ss->zstep[b3dY] = zn.y;
  ss->zstep[b3dZ] = zn.z;
     
  /* Set cut points */
  ss->vi->slice.zx1 = ss->vi->slice.zx2 =
    ss->vi->slice.yx1 = ss->vi->slice.yx2 = (int)ss->cx;
  ss->vi->slice.zy1 = ss->vi->slice.zy2 =
    ss->vi->slice.xy1 = ss->vi->slice.xy2 = (int)ss->cy;
  ss->vi->slice.xz1 = ss->vi->slice.xz2 =
    ss->vi->slice.yz1 = ss->vi->slice.yz2 = (int)ss->cz;

  if ((zcut.x) || (zcut.y)){
    x = ss->cx;
    y = ss->cy;
    do{
      x += zcut.x;
      y += zcut.y;
    }while((x < ss->vi->xsize) && (x > 0) &&
           (y < ss->vi->ysize) && (y > 0));
    ss->vi->slice.zx1 = (int)(x - zcut.x);
    ss->vi->slice.zy1 = (int)(y - zcut.y);
    x = ss->cx;
    y = ss->cy;
    do{
      x -= zcut.x;
      y -= zcut.y;
    }while((x < ss->vi->xsize) && (x > 0) &&
           (y < ss->vi->ysize) && (y > 0));
    ss->vi->slice.zx2 = (int)(x + zcut.x);
    ss->vi->slice.zy2 = (int)(y + zcut.y);
  }else{
    ss->vi->slice.zx1 = ss->vi->slice.zy1 =
      ss->vi->slice.zx1 = ss->vi->slice.zx2 = 0;
  }

  /* set yx1, yz1  and yx2, yz2 */
  if ((ycut.x) || (ycut.z)){
    x = ss->cx;
    z = ss->cz;
    do{
      x += ycut.x;
      z += ycut.z;
    }while((x < ss->vi->xsize) && (x > 0) &&
           (z < ss->vi->zsize) && (z > 0));
    ss->vi->slice.yx1 = (int)(x - ycut.x);
    ss->vi->slice.yz1 = (int)(z - ycut.z);
    x = ss->cx;
    z = ss->cz;
    do{
      x -= ycut.x;
      z -= ycut.z;
    }while((x < ss->vi->xsize) && (x > 0) &&
           (z < ss->vi->zsize) && (z > 0));
    ss->vi->slice.yx2 = (int)(x + ycut.x);
    ss->vi->slice.yz2 = (int)(z + ycut.z);
  }else{
    ss->vi->slice.yx1 = ss->vi->slice.yz1 =
      ss->vi->slice.yx2 = ss->vi->slice.yz2 = 0;
  }

  /* set xy1, xz1  and xy2, xz2 */
  if ((xcut.y) || (xcut.z)){
    y = ss->cy;
    z = ss->cz;
    do{
      y += xcut.y;
      z += xcut.z;
    }while((y < ss->vi->ysize) && (y > 0) &&
           (z < ss->vi->zsize) && (z > 0));
    ss->vi->slice.xy1 = (int)(y - xcut.y);
    ss->vi->slice.xz1 = (int)(z - xcut.z);
    y = ss->cy;
    z = ss->cz;
    do{
      y -= xcut.y;
      z -= xcut.z;
    }while((y < ss->vi->ysize) && (y > 0) &&
           (z < ss->vi->zsize) && (z > 0));
    ss->vi->slice.xy2 = (int)(y + xcut.y);
    ss->vi->slice.xz2 = (int)(z + xcut.z);
  }else{
    ss->vi->slice.xy1 = ss->vi->slice.xz1 =
      ss->vi->slice.xy2 = ss->vi->slice.xz2 = 0;
  }


  /* set up starting image coord */
  ss->xo = ss->cx;
  ss->yo = ss->cy;
  ss->zo = ss->cz;

  isize = ss->winx / ss->zoom;
  jsize = ss->winy / ss->zoom;

  /* z stretch scale factor.   */
  zs = 1.0f / slicerGetZScaleBefore(ss);
  /*  zs  = ss->vi->imod->zscale; 
  if (zs <= 0.0f)
    zs = 1.0f;
  zs = 1.0f / zs;
  if (ss->scalez != SLICE_ZSCALE_BEFORE)
  zs = 1.0f; */

  /* DNM: make these correct for HQ case at least */
  ss->xo -= (isize / 2) * ss->xstep[b3dX];
  ss->yo -= (isize / 2) * ss->xstep[b3dY];
  ss->zo -= (isize / 2) * ss->xstep[b3dZ] * zs;
  ss->zo -= (jsize / 2) * ss->ystep[b3dZ] * zs;
  ss->xo -= (jsize / 2) * ss->ystep[b3dX];
  ss->yo -= (jsize / 2) * ss->ystep[b3dY];

  return;
}



/* cubic convolution interpolation looks much better at high zoom : use it
 to fill in array created by shortcut */

void slicerCubicFillin(unsigned short *cidata, int winx, int winy, int izoom,
		       int ilimshort, int jlimshort, int minval, int maxval)
{
  int ifill, jfill, deli, delj, joffset, yoffset;
  int xn, xn2, xp, ynoffset, yn2offset, ypoffset;
  int i, j, xi, yi;
  float dysq, dycub, fyp, fy, fyn, fyn2;
  float dxsq, dxcub, fxp, fx, fxn, fxn2;
  float yp, y0, yn, yn2;
  float dx, dy, ival;
  float zoom = (float)izoom;
  unsigned short oldval;

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
                         
	  if (ival > maxval)
	    ival = maxval;
	  if (ival < minval)
	    ival = minval;
	  oldval = cidata[i + joffset];
	  cidata[i + joffset] = (unsigned short)(ival + 0.5f);
	}
      }
    }
  }
}


/*
 * GfX resize Events:
 */

void slicerResize(SlicerStruct *ss, int winx, int winy)
{
  ss->winx = winx;
  ss->winy = winy;
  b3dResizeViewportXY(winx, winy);

  /* DNM: send 12 rather than App->depth to guarantee shorts */
  ss->image   = b3dGetNewCIImageSize(ss->image, 12, winx, winy);
  if (!ss->image)
    wprint("\aInsufficient memory to run this Slicer window.\n"
           "Try making it smaller or close it.\n");
 
  ivwControlPriority(ss->vi, ss->ctrl);
}

void slicerCubeResize(SlicerStruct *ss, int winx, int winy)
{
  b3dResizeViewportXY(winx, winy);
}

/*
 *  The external draw command from the controller
 */
static void slicerDraw_cb(ImodView *vi, void *client, int drawflag)
{
  SlicerStruct *ss = (SlicerStruct *)client;
  float usex, usey, usez, delta, factor = 0.;
  Ipoint norm, vec = {0., 0., 1.};

  if (drawflag & IMOD_DRAW_COLORMAP) {
    ss->glw->setColormap(*(App->qColormap));
    return;
  }

  /* Adjust the cursor if necessary */
  if (vi->imod->mousemode != ss->mousemode) {
    ss->mousemode = vi->imod->mousemode;
    if (ss->mousemode == IMOD_MMODEL)
      ss->glw->setCursor(*App->modelCursor);
    else
      ss->glw->unsetCursor();
  }

  // imodPrintStderr("flags on draw %x \n", drawflag);

  /* DNM: use a value saved in structure in case more than one window */
  if (ss->zslast != ss->vi->imod->zscale){
    ss->zslast = ss->vi->imod->zscale;
    drawflag |= IMOD_DRAW_ACTIVE;
  }

  // Did this slicer start the current movie?
  if ((vi->xmovie || vi->ymovie || vi->zmovie) && 
      imcGetStarterID() == ss->ctrl) {

    // Then determine factor for moving in one step on dominant axis
    if (vi->xmovie && fabs((double)ss->zstep[b3dX]) > 1.e-6)
      factor = (ss->vi->xmouse - ss->cx) / ss->zstep[b3dX];
    else if (vi->ymovie && fabs((double)ss->zstep[b3dY]) > 1.e-6)
      factor = (ss->vi->ymouse - ss->cy) / ss->zstep[b3dY];
    else if (vi->zmovie && fabs((double)ss->zstep[b3dZ]) > 1.e-6)
      factor = (ss->vi->zmouse - ss->cz) / ss->zstep[b3dZ];

    /*imodPrintStderr("%d %d %d factor %f mouse %.1f %.1f %.1f  "
            "cur %.1f %.1f %.1f\n", vi->xmovie, 
            vi->ymovie, vi->zmovie, factor, vi->xmouse, 
            vi->ymouse, vi->zmouse, ss->cx, ss->cy, ss->cz); */

    // Compute new position and bound it (may not be needed unless user
    // clicks a new position during movie)
    if (factor != 0.) {
      vi->xmouse = ss->cx + factor * ss->zstep[b3dX];
      vi->ymouse = ss->cy + factor * ss->zstep[b3dY];
      ss->cz += factor * ss->zstep[b3dZ];
      if (ss->cz < 0.)
        ss->cz = 0.;
      if (ss->cz > vi->zsize - 1.)
        ss->cz = vi->zsize - 1.;
      vi->zmouse = (int)floor(ss->cz + 0.5);
      ivwBindMouse(vi);
      ss->cx = vi->xmouse;
      ss->cy = vi->ymouse;

      ss->pending = 0;
      ss->glw->updateGL();

      // Get snapshots if there is a count for doing so
      if (imcGetSnapshot(ss->vi) && ss->movieSnapCount) {
        b3dKeySnapshot("slicer", imcGetSnapshot(ss->vi) - 1, 
                       imcGetSnapshot(ss->vi) % 2, NULL);
        ss->movieSnapCount--;

        /* When count expires, stop movie */
        if(!ss->movieSnapCount)
          imodMovieXYZT(vi, 0, 0, 0, 0);
      }
      sslice_cube_draw(ss);
      return;
    }
  }

  if ((drawflag & IMOD_DRAW_XYZ) && !ss->locked) {

    if (ss->classic) {
      /* DNM: if there is a pending set of values from a mouse hit,
         use them instead of the mouse values for this */
      if (ss->pending) {
        usex = ss->pendx;
        usey = ss->pendy;
        usez = ss->pendz;
      } else {
        usex = ss->vi->xmouse;
        usey = ss->vi->ymouse;
        usez = ss->vi->zmouse;
      }
      if ((ss->lx != usex) ||
          (ss->ly != usey) ||
          (ss->lz != usez)) {
        ss->cx = usex;
        ss->cy = usey;
        ss->cz = usez;
        ss->pending = 0;
        //imodPrintStderr("XYZ draw at %f %f %f\n", ss->cx, ss->cy, ss->cz);
        sslice_draw(ss);
        return;
      }
    } else {
      
      // If our drawing position is the same as before, move to a plane
      // containing the current point
      if (ss->lx == ss->cx && ss->ly == ss->cy && ss->lz == ss->cz &&
          ss->lang[0] == ss->tang[0] && ss->lang[1] == ss->tang[1] &&
          ss->lang[2] == ss->tang[2]) {

        // Move along direction of normal to plane until reaching a plane that
        // contains the current point; snap to curpt if this is out of bounds
        setInverseMatrix(ss);
        imodMatTransform3D(ss->mat, &vec, &norm);
        delta = norm.x * (ss->vi->xmouse - ss->cx) + 
          norm.y * (ss->vi->ymouse - ss->cy) + 
          norm.z * (ss->vi->zmouse - ss->cz);
        if (imodDebug('s'))
          imodPrintStderr("norm %.4f %.4f %.4f  delta %.3f\n", norm.x, norm.y, 
                          norm.z, delta);
        
        // Only move to a new plane if the delta is bigger than 0.5
        if (fabs((double)delta) >= 0.5) {
          ss->cx += delta * norm.x;
          ss->cy += delta * norm.y;
          ss->cz += delta * norm.z;
          
          // If this goes out of bounds, just snap to current point
          if (ss->cx < 0 || ss->cx >= ss->vi->xsize || ss->cy < 0 || 
              ss->cy >= ss->vi->ysize || ss->cz < 0 || 
              ss->cz >= ss->vi->zsize) {
            ss->cx = ss->vi->xmouse;
            ss->cy = ss->vi->ymouse;
            ss->cz = ss->vi->zmouse;
          }
        }
      }
      sslice_draw(ss);
      return;
    }
  }

  /* DNM 3/11/03: try moving only if not locked */
  if ((drawflag & (IMOD_DRAW_ACTIVE | IMOD_DRAW_IMAGE))){
    if (ss->pending && !ss->locked && ss->classic) {
      ss->cx = ss->pendx;
      ss->cy = ss->pendy;
      ss->cz = ss->pendz;
      ss->pending = 0;
    }
    //imodPrintStderr("ACTIVE draw at %f %f %f\n", ss->cx, ss->cy, ss->cz);
    sslice_draw(ss);
    return;
  }
     
  // If we get here and there is a model flag, or still an XYZ flag in new
  // mode, then redraw the filled image
  if ((drawflag & IMOD_DRAW_MOD) || 
      (!ss->classic && (drawflag & IMOD_DRAW_XYZ))){
    //imodPrintStderr("MOD draw at %f %f %f\n", ss->cx, ss->cy, ss->cz);
    slicerUpdateImage(ss);
  }
}

/*
 * Internal draw function: draw the image then the cube
 */
static void sslice_draw(SlicerStruct *ss)
{
  ss->glw->updateGL();
  sslice_cube_draw(ss);
}

/* Redraw image, assuming data array is filled */
static void slicerUpdateImage(SlicerStruct *ss)
{
  ss->imageFilled = 1;
  ss->glw->updateGL();
  ss->imageFilled = 0;
  sslice_cube_draw(ss);
}

/*
 * The paint routine called by the GL widget
 */
void slicerPaint(SlicerStruct *ss)
{
  int xim, yim, zim;
  int sliceScaleThresh = 4;
  if (!ss->image)
    return;

  GLenum format = GL_COLOR_INDEX;
  GLenum type   = GL_UNSIGNED_SHORT;
  GLint unpack = b3dGetImageType(&type, &format);
  if (unpack == 1) {
    unpack = 2;
    format = GL_COLOR_INDEX;
    type   = GL_UNSIGNED_SHORT;
  }

  // Skip draw if minimized!
  if (ss->qtWindow->isMinimized())
    return;

  if (!ss->imageFilled) {

    // If filling array, first assess mean and SD for scaling multiple slices
    // to single slice, then fill array for real and update angles
    if (!mousePanning && !ss->vi->colormapImage && 
        ss->nslice > sliceScaleThresh)
      fillImageArray(ss, 0, 1);
    else if (!mousePanning)
      ss->scaleToMeanSD = false;
    fillImageArray(ss, mousePanning, 0);
    if (sliceAngDia && (getTopSlicer() == ss))
      sliceAngDia->topSlicerDrawing(ss->tang, ss->cx, ss->cy, ss->cz, 
                                    ss->timeLock ? ss->timeLock : ss->vi->ct, 
                                    sliderDragging + mousePanning);
  }

  b3dSetCurSize(ss->winx, ss->winy);

  glPixelStorei(GL_UNPACK_ALIGNMENT, unpack);

  /* Just clear the unused edges if there are shifts */
  if (ss->xshift || ss->yshift) {
    b3dColorIndex(App->background);
    b3dDrawFilledRectangle(0, 0, ss->winx, (int)ss->yshift);
    b3dDrawFilledRectangle(0, 0, (int)ss->xshift, ss->winy);
  }

  /* DNM: one-to-one image for fractional zoom as well as in hq case */
  if (ss->noPixelZoom)
    glPixelZoom(1.0f, 1.0f);
  else
    /* DNM: don't make this xzoom, yzoom.  */
    glPixelZoom(ss->zoom, ss->zoom);

  glRasterPos2f(ss->xshift, ss->yshift);

  glDrawPixels(ss->winx, ss->winy, format, type, ss->image->id1);
    
  /* Position of cursor. */
  if (ss->vi->drawcursor) {
    if (ss->classic) {
      b3dColorIndex(App->endpoint);
      b3dDrawPlus((int)(ss->winx * 0.5f), (int)(ss->winy * 0.5f), 5);
    } else if (ss->mousemode == IMOD_MMOVIE || ss->vi->imod->cindex.point < 0){
      b3dColorIndex(App->curpoint);
      getWindowCoords(ss, ss->vi->xmouse, ss->vi->ymouse, ss->vi->zmouse,
                      xim, yim, zim);
      b3dDrawPlus(xim, yim, ImodPrefs->minCurrentImPtSize());
    }
  }
  
  sslice_draw_model(ss);
}


/* Model drawing routine */
static void sslice_draw_model(SlicerStruct *ss)
{
  float depth = ss->depth;

  depth = depth * ss->zoom;
  depth *= 0.5f;

  glMatrixMode(GL_PROJECTION);
     
  glLoadIdentity();

  glOrtho(0.0 , ss->winx, 0.0, ss->winy, depth, -depth);

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();


  glTranslatef(ss->winx*0.5f, ss->winy*0.5f, 0.0f);
  /*     if (ss->scalez == SLICE_ZSCALE_AFTER)
         glScalef(1.0f, 1.0f, ss->vi->imod->zscale); */
  glScalef(ss->xzoom, ss->yzoom, ss->zoom);

  /* DNM: took away minus signs because inverting sense of angles;
     also had to swap X and Y */
  glRotatef(ss->tang[b3dX], 1.0f, 0.0f, 0.0f);
  glRotatef(ss->tang[b3dY], 0.0f, 1.0f, 0.0f);
  glRotatef(ss->tang[b3dZ], 0.0f, 0.0f, 1.0f);
  
  glScalef(1.0f, 1.0f, slicerGetZScaleBefore(ss));
  // if (ss->scalez == SLICE_ZSCALE_BEFORE)
  //  glScalef(1.0f, 1.0f, ss->vi->imod->zscale);

  glTranslatef(-ss->cx, -ss->cy, -ss->cz);

  imodDrawModel(ss->vi, ss->vi->imod);
  glPopMatrix();
  return;
}

/* cube drawing function call */
static void sslice_cube_draw(SlicerStruct *ss)
{
  ss->cube->updateGL();
}

/* The paint routine called by the cube GL widget */
void slicerCubePaint(SlicerStruct *ss)
{
  double params[4];
  static float v[3], vx[3], vy[3];
  double x, y, z;
  double r;
  float zs  = 1;
  float zoom = 1.0/ss->zoom;
  int winx, winy;
  float xo, yo, zo;

  b3dSetCurSize(ss->cube->width(), ss->cube->height());

  glClearColor(0, 0, 0, 0);
  glClear(GL_COLOR_BUFFER_BIT);
     
  x = ss->vi->xsize;
  y = ss->vi->ysize;
  z = ss->vi->zsize;

  /* DNM: take window size * zoom as # of pixels displayed */
  xo = ss->xo;
  yo = ss->yo;
  zo = ss->zo;
  winx = (int)(ss->winx * zoom);
  winy = (int)(ss->winy * zoom);

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
     
  vx[0] = xo + (ss->xstep[0] * winx);
  vx[1] = yo + (ss->xstep[1] * winx);
  vx[2] = zo + (ss->xstep[2] * winx * zs);

  vy[0] = xo + (ss->ystep[0] * winy);
  vy[1] = yo + (ss->ystep[1] * winy);
  vy[2] = zo + (ss->ystep[2] * winy * zs);

  v[0]  = xo + (ss->xstep[0] * winx) 
    + (ss->ystep[0] * winy);
  v[1]  = yo + (ss->xstep[1] * winx) 
    + (ss->ystep[1] * winy);
  v[2]  = zo + (ss->xstep[2] * winx * zs) 
    + (ss->ystep[2] * winy * zs);

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

/*
$Log$
Revision 4.41  2007/05/25 05:28:16  mast
Changes for addition of slicer angle storage

Revision 4.40  2007/03/29 04:55:49  mast
Fixed crash bug when closing window while focus is in edit/spinbox

Revision 4.39  2007/01/08 18:18:51  mast
Fixed bug: Z coordinate from mouse point was limited by X not Z size

Revision 4.38  2006/10/12 19:02:54  mast
Added toolbar button for W function

Revision 4.37  2006/10/11 04:07:05  mast
*** empty log message ***

Revision 4.36  2006/10/06 19:36:30  mast
Moved fillImageArry to slicer_classes for threading

Revision 4.35  2006/10/05 15:41:32  mast
Provided for primary and second non-TIFF snapshot format

Revision 4.34  2006/09/17 18:15:59  mast
Changes to provide mouse position to pixelview

Revision 4.33  2006/09/13 23:50:38  mast
Fixed a float in call that takes int

Revision 4.32  2006/09/13 05:36:41  mast
Fixed drag drawing when highly tilted by using 3D distance

Revision 4.31  2006/09/12 15:39:50  mast
Improved locked modeling and added hot key to realign to contour

Revision 4.30  2006/08/28 05:18:21  mast
Do not average multiple slices for colormapped images

Revision 4.29  2006/06/24 16:03:52  mast
Added arguments to call of FFT routine

Revision 4.28  2005/03/08 15:49:36  mast
Added FFT mode

Revision 4.27  2004/11/21 05:50:34  mast
Switch from int to float for nearest point distance measurement

Revision 4.26  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 4.25  2004/11/01 22:53:37  mast
Changed call to get nearest point

Revision 4.24  2004/08/12 17:05:17  mast
Added message to get slicer angles

Revision 4.23  2004/06/16 00:13:01  mast
Constrain keypad movements when locked

Revision 4.22  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.21  2003/12/18 22:50:25  mast
Implemented movieing and auot-snapshotting

Revision 4.20  2003/11/13 20:11:30  mast
Made locked window respond to Up, Down, and arrows by moving current
position without changing global position, consistent with locked zap

Revision 4.19  2003/09/16 02:11:48  mast
Changed to access image data using new line pointers

Revision 4.18  2003/05/05 20:09:34  mast
Fixed problem with positioning with first mouse button

Revision 4.17  2003/04/28 04:03:21  mast
Fix help text

Revision 4.16  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.15  2003/04/18 20:16:39  mast
Rename meta test function

Revision 4.14  2003/04/18 20:07:36  mast
Implement limit checks when filling data array to minimize the time spent
when the window is much larger than the image and speed up lowres
filling.  Also reject the Ctrl (meta) key on the Mac.

Revision 4.13  2003/04/17 20:11:56  mast
resolve merge conflict

Revision 4.12  2003/04/17 19:06:50  mast
various changes for Mac

Revision 4.11  2003/03/26 23:23:15  mast
switched from hotslider.h to preferences.h

Revision 4.10  2003/03/24 17:56:46  mast
Register with dialogManager so it can be parked with info window

Revision 4.9  2003/03/13 01:34:07  mast
Fixed initialization of distance variable in attach function

Revision 4.8  2003/03/13 01:20:08  mast
Convert numlock keypad keys so num lock can be on

Revision 4.7  2003/03/12 21:35:23  mast
Test if no CIImage is returned and give error message

Revision 4.6  2003/03/12 06:40:27  mast
Prevented adding or modifying points at the wrong time, made lock really
lock image for all mouse events so that one can model on a locked image,
added attach function.

Revision 4.5  2003/03/03 22:44:44  mast
Added +/- 15 degree hot keys.  Mode it display modeling cursor only
in model mode

Revision 4.4  2003/02/27 23:46:05  mast
Add keypad keys to move by +/-15 degrees.

Revision 4.3  2003/02/27 19:27:16  mast
Using new b3dX,Y,Z and adding argument to imodDrawModel call

Revision 4.2  2003/02/12 21:39:26  mast
Changed integer truncation method to speed up on some pentium 4's

Revision 4.1  2003/02/10 20:29:02  mast
autox.cpp

Revision 1.1.2.9  2003/01/30 01:01:08  mast
Simplify getting window the right size

Revision 1.1.2.8  2003/01/29 01:34:00  mast
implement colormaps

Revision 1.1.2.7  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.6  2003/01/23 20:13:33  mast
add include of imod_input

Revision 1.1.2.5  2003/01/13 01:15:43  mast
changes for Qt version of info window

Revision 1.1.2.4  2003/01/10 23:53:53  mast
moved declaration of cubicFillin to include file

Revision 1.1.2.3  2003/01/06 18:59:19  mast
cleanup and reorganization

Revision 1.1.2.2  2003/01/06 15:47:55  mast
Qt version

Revision 1.1.2.1  2003/01/02 15:45:09  mast
changes for new controller key callback

Revision 3.3  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.2  2002/09/05 16:03:41  mast
Choose visual explicitly for the cube, because GLw fails if there
is no single-buffer visual available

Revision 3.1  2002/05/20 15:29:39  mast
Made it move the current point to middle of image when slicer is opened if
it is still at 0,0.  Made it load cache for current point, which is needed
if no Zap window is open.

Revision 3.0  2001/11/29 18:10:49  rickg
*** empty log message ***

Revision 1.2  2001/11/23 05:32:30  mast
Activated faster HQ display method for zoom > 1 using cubic interpolation
- coded 2 years ago and left unused.  Also added workaround to Intel
compiler bug.

*/
