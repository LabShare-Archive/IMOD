/*  IMOD VERSION 2.50
 *
 *  slicer.c -- Open the slicer window; Slice 3-D data at any angle.
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

$Log$
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
#include <math.h>
#include <limits.h>
#include <string.h>
#include <qcursor.h>
#include <qbitmap.h>
#include <qapplication.h>

#include "slicer_classes.h"
#include "imod.h"
#include "imod_display.h"
#include "b3dgfx.h"
#include "sslice.h"
#include "imod_input.h"
#include "imod_info_cb.h"
#include "control.h"
#include "imodplug.h"
#include "hotslider.h"
#include "dia_qtutils.h"
#include "xcramp.h"

#include "qcursor.bits"
#include "qcursor_mask.bits"

/* internal functions. */
static void sslice_cube_draw(SlicerStruct *ss);
static void sslice_draw(SlicerStruct *ss);
static void sslice_setxyz(SlicerStruct *ss, int x, int y);
static void slice_trans_step(SlicerStruct *ss);
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
static void fillImageArray(SlicerStruct *ss);
static void slicerDraw_cb(ImodView *vi, void *client, int drawflag);
static void slicerClose_cb(ImodView *vi, void *client, int junk);
static int sslice_showslice(SlicerStruct *ss);

/* DNM: maximum angles for sliders */
static float maxAngle[3] = {90.0, 180.0, 180.0};
static int ctrlPressed = false;

/*
 * Open up slicer help dialog.
 */
void slicerHelp()
{
  dia_vasmsg
    ("Imod Slicer Help\n",
     "-------------------------------------------------------------\n",
     "\nSlicer has two dockable toolbars that may be floated as separate "
     "windows so that the image will fill the entire window area.  When they "
     "are floated, they will still pass hotkeys on to the slicer window\n",

     "\nThe First Toolbar\n",
     "-------------------\n",
     "\tThe Up and Down Arrows step the zoom factor up or down.\n",
     "\tThe text box shows the current zoom and allows  you to type in "
     "intermediate zoom factors.\n",
     "\tThe checkerboard button toggles between fast rendering and "
     "slower but higher quality image rendering.\n",
     "\tThe lock button will keep the image locked to the current "
     "position.\n",
     "\tThe show slice button will cause lines to be draw in the "
     "XYZ and ZaP windows to show the intersection of the current "
     "slice.\n",
     "The Z-Scale box lets you"
     "control whether the volume will be displayed with Z "
     "scaling.  If you choose \"Z-Scale Before\", then the volume will be "
     "scaled before it is rotated and sliced, and thus the rotation "
     "angles will not select the same slice as with no Z-scaling.  If "
     "you choose \"Z-Scale After\", the volume is scaled after being "
     "rotated, and the same slice is selected as with no Z-scaling.\n",

     "\nThe Second Toolbar\n",
     "-------------------\n",
     "\tOn the left are three sliders "
     "with which you can adjust the rotations about the "
     "X, Y, and Z axes.  The rotation angles are applied to the volume, "
     "not to the slicing plane, and they are applied in the order Z, Y, "
     "X; thus these same angles can be used directly in other programs "
     "that rotate the volume.  Clicking a slider with the left mouse "
     "button will change the angle by 1 degree; clicking with the "
     "middle button moves the slider immediately to the position of "
     "mouse.  If you drag the slider, the representation of the slice "
     "in the data volume will change continuously, but the image will "
     "not be updated until you release the slider, unless you press the "
     "Ctrl key to make the slider continuously active.\n",
     "\tIn the middle is a representation of the slice that is being cut "
     "from the data volume.\n"
     "\tOn the right are controls for adjusting the thickness of "
     "the image slice and the thickness of model that is shown on the slice.\n"
     "\nMouse Actions\n",
     "-----------------\n",
     "Left button: Pick a new current viewing point in the data volume\n"
     "Middle button: In model mode, insert a point after the current "
     "point\n"
     "Right button: In model mode, move the current point to the "
     "selected location\n\n",
     "Hot Keys\n",
     "-----------\n",
     "-/=\tDecrease/Increase zoom\n",
     "_/+\tDecrease/Increase displayed image thickness\n",
     "9/0\tDecrease/Increase displayed model thickness\n",
     "s\tShow slice in ZaP and XYZ windows\n",
     "S\tSnapshot to RGB file\n",
     "Ctrl-S\tSnapshot to TIFF file\n",
     "x/y/z\tAlign current and previous model points along X, Y or Z "
     "axis\n",
     "X/Y/Z\tAlign first and last points of contour along X, Y or Z "
     "axis\n\n",
     "Numeric Keypad:\n"
     "4/6\t(Left/Right) Decrease/Increase last adjusted angle by "
     "0.1 degree\n",
     "2/8\t(Down/Up) Decrease/Increase last adjusted angle by "
     "0.5 degree\n",
     "0\t(Insert) Set last adjusted angle to 0\n",
     NULL);
  return;
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
  if (index) {

    /* toggle the lock button, redraw if unlocking */
    ss->locked = state;
    if (!state) {
      ss->cx = ss->vi->xmouse;
      ss->cy = ss->vi->ymouse;
      ss->cz = ss->vi->zmouse;
      sslice_draw(ss);
    }
  } else {

    /* toggle between fast rendering and highres rendering */
    ss->hq = state;
    sslice_draw(ss);
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

  // Do complete redraw if not dragging or hot slider enabled
  if (!dragging || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && ctrlPressed) ||
      (hotSliderFlag() == HOT_SLIDER_KEYUP && !ctrlPressed)) {
    sslice_draw(ss);
    sslice_showslice(ss);
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
  ivwControlPriority(ss->vi, ss->ctrl);
  if (sno < 1)
    sno = 1;
  ss->nslice = sno;

  drawThickControls(ss);
  sslice_draw(ss);
}

void slicerModelThickness(SlicerStruct *ss, float depth)
{
  ivwControlPriority(ss->vi, ss->ctrl);
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
  ivwRemoveControl(ss->vi, ss->ctrl);      
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

  ss = (SlicerStruct *)malloc(sizeof(SlicerStruct));
  if (!ss)
    return(-1);

  /* DNM 5/16/02: if the current position is still in the lower left
     corner, move it to middle and draw other windows */
  if (!vi->xmouse && !vi->ymouse) {
    vi->xmouse = vi->xsize / 2;
    vi->ymouse = vi->ysize / 2;
    imodDraw(vi, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ);
  }

  ss->cx = vi->xmouse;
  ss->cy = vi->ymouse;
  ss->cz = vi->zmouse;
  ss->vi     = vi;
  ss->locked = 0;
  ss->zoom   = 1.0;
  ss->lx     = vi->xmouse;
  ss->ly     = vi->ymouse;
  ss->lz     = vi->zmouse;
  ss->hq     = 0;
  ss->tang[X] = 0.0f;
  ss->tang[Y] = 0.0f;
  ss->tang[Z] = 0.0f;
  ss->mapped = 0;
  ss->scalez = 0;
  ss->depth = 1.0;
  ss->image = NULL;
  ss->bcoord[0] = vi->xmouse;
  ss->bcoord[1] = vi->ymouse;
  ss->bcoord[2] = vi->zmouse;
  ss->xstep[0]  = 1.0f; ss->xstep[1] = ss->xstep[2] = 0.0f;
  ss->ystep[1]  = 1.0f; ss->ystep[0] = ss->ystep[2] = 0.0f;
  ss->nslice = 1;
  ss->mat = imodMatNew(3);
  ss->lastangle = X;
  ss->zslast = 1.0;
  ss->pending = 0;
  ss->imageFilled = 0;

  slice_trans_step(ss);
  ss->qtWindow = new SlicerWindow(ss, maxAngle, App->rgba, 
				      App->doublebuffer, App->qtEnableDepth,
				      NULL, "slicer window");
  if (!ss->qtWindow){
    free(ss);
    wprint("Error opening slicer window.");
    return(-1);
  }
  ss->glw = ss->qtWindow->mGLw;
  ss->cube = ss->qtWindow->mCube;
  if (!App->rgba)
    ss->glw->setColormap(*(App->qColormap));

  ss->qtWindow->setCaption(imodCaption("Imod Slicer"));
  ss->qtWindow->mToolBar->setLabel(imodCaption("Slicer Toolbar 1"));
  ss->qtWindow->mToolBar2->setLabel(imodCaption("Slicer Toolbar 2"));
	
  ss->ctrl = ivwNewControl(vi, slicerDraw_cb, slicerClose_cb, slicerKey_cb,
                               (void *)ss);

  // Set up cursor
  QBitmap bmCursor(qcursor_width, qcursor_height, qcursor_bits, true);
  QBitmap bmMask(qcursor_width, qcursor_height, qcursor_mask_bits, true);
  QCursor cursor(bmCursor, bmMask, qcursor_x_hot, qcursor_y_hot);
  ss->glw->setCursor(cursor);

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
  ss->qtWindow->resize( newWidth, newWidth);

  ss->qtWindow->show();

  return(0);
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
  Ipoint *p1, *p2;
  int ob, co, pt, axis;
  Icont *cont;

  ivwControlPriority(ss->vi, ss->ctrl);

  if (imodPlugHandleKey(ss->vi, event)) 
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
          
  case Qt::Key_S:
    if (shift || ctrl){

      // Snapshots: need to update just the image window
      ss->glw->updateGL();
      if (shift)
        b3dAutoSnapshot("slicer", SnapShot_RGB, NULL);
      else
        b3dAutoSnapshot("slicer", SnapShot_TIF, NULL);
    }else
      sslice_showslice(ss);
    dodraw = 0;
    break;

  case Qt::Key_X:
  case Qt::Key_Y:
  case Qt::Key_Z:
    cont = imodContourGet(ss->vi->imod);
    imodGetIndex(ss->vi->imod, &ob, &co, &pt);
    
    p2 = imodPointGet(ss->vi->imod);
    if ((cont) && (p2) && (cont->psize > 1)){
      if (pt)
	p1 = &cont->pts[pt-1];
      else
	p1 = &cont->pts[pt+1];
      axis = X;
      if (keysym == Qt::Key_Y)
	axis = Y;
      if (keysym == Qt::Key_Z)
	axis = Z;
      if (shift){
	p2 = &cont->pts[cont->psize - 1];
	p1 = &cont->pts[0];
      }
      sliceSetAnglesFromPoints(ss, p1, p2, axis);
    } else
      dodraw = 0;
    break;

    /* DNM: add these to adjust last angle, now that input is properly
       passed to the window */
  case Qt::Key_Up:
  case Qt::Key_Down:
  case Qt::Key_Right:
  case Qt::Key_Left:
  case Qt::Key_Insert:
    dodraw = 0;
    if (!(event->state() & Qt::Keypad)) {
      handled = 0;
      break;
    }
    if (keysym == Qt::Key_Down) 
      ss->tang[lang] -= 0.5;
    if (keysym == Qt::Key_Left) 
      ss->tang[lang] -= 0.1;
    if (keysym == Qt::Key_Right) 
      ss->tang[lang] += 0.1;
    if (keysym == Qt::Key_Up)
      ss->tang[lang] += 0.5;
    if (keysym == Qt::Key_Insert)
      ss->tang[lang] = 0.0;
    if (ss->tang[lang] > maxAngle[lang])
      ss->tang[lang] = maxAngle[lang];
    if (ss->tang[lang] < -maxAngle[lang])
      ss->tang[lang] = -maxAngle[lang];

    ss->qtWindow->setAngles(ss->tang);

    sslice_draw(ss);
    sslice_showslice(ss);
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
    inputQDefaultKeys(event, ss->vi);
    return;
  }

  // If draw still needed, do it
  if (dodraw)
    sslice_draw(ss);         
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

  if (event->stateAfter() & Qt::LeftButton){
    sslice_setxyz(ss, event->x(), event->y());
    /* DNM: for select hits, do keep cz at an integral value */
    ss->pending = 0;
    imodDraw(ss->vi, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ);
  }

  if (event->stateAfter() & Qt::MidButton)
      slicer_insert_point(ss, event->x(), event->y());

  if (event->stateAfter() & Qt::RightButton)
      slicer_modify_point(ss, event->x(), event->y());
}

static void slicer_insert_point(SlicerStruct *ss, int x, int y)
{
  sslice_setxyz(ss, x, y);
  inputInsertPoint(ss->vi);
  return;
}

static void slicer_modify_point(SlicerStruct *ss, int x, int y)
{
  sslice_setxyz(ss, x, y);
  inputModifyPoint(ss->vi);
  return;
}

static void sslice_setxyz(SlicerStruct *ss, int x, int y)
{
  float xoffset, yoffset;
  float xm, ym, zm;
  int zmouse;
  float zs;

  /* DNM: the xzoom and yzoom are correct for all cases, and the zs
     needs to be applied only for the case of SCALE_BEFORE */

  zs = 1.0f;
  if (ss->scalez == SLICE_ZSCALE_BEFORE && ss->vi->imod->zscale > 0)
    zs = 1.0 / ss->vi->imod->zscale;

  /* DNM: have to use integer arithmetic on window sizes, and account for
     y going from 0 to winy-1 when inverting it */

  xoffset = ((ss->winx / 2) - x) / ss->xzoom;
  yoffset = ((ss->winy / 2) - (ss->winy - 1 - y)) / ss->yzoom;
     
  /* DNM: always take position relative to current display center,
     regardless of whether the window is locked (it was using [xyz]mouse */
  xm = ss->cx;
  ym = ss->cy;
  zm = ss->cz;

  xm -= (ss->xstep[X] * xoffset) + (ss->ystep[X] * yoffset);
  ym -= (ss->xstep[Y] * xoffset) + (ss->ystep[Y] * yoffset);
     
  zm -= (ss->xstep[Z] * xoffset * zs) + (ss->ystep[Z] * yoffset * zs);

  if (xm < 0)
    xm = 0;
  if (xm >= ss->vi->xsize)
    xm = ss->vi->xsize - 1;
  if (ym < 0)
    ym = 0;
  if (ym >= ss->vi->ysize)
    ym = ss->vi->ysize - 1;
  if (zm < 0)
    zm = 0;
  if (zm >= ss->vi->zsize)
    zm = ss->vi->zsize - 1;

  zmouse = (int)(floor((double)(zm + 0.5f)));

  /* Set up pending coordinates for next draw to use */
  ss->pendx = xm;
  ss->pendy = ym;
  ss->pendz = zm;
  ss->pending = 1;

  /* DNM: do this regardless of whether locked or not.  Also, set zmouse
     to the integral value but keep pendz at a real value */
  ss->vi->xmouse = xm; 
  ss->vi->ymouse = ym; 
  ss->vi->zmouse = zmouse;
  return;
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


static void sliceSetAnglesFromPoints(SlicerStruct *ss,
                              Ipoint *p1, Ipoint *p2, int axis)
{
  Ipoint n;
  Ipoint a;
  double val;
  float small = 1.e-4;
  double rpd = RADIANS_PER_DEGREE;
  n.x = p2->x - p1->x;
  n.y = p2->y - p1->y;
  n.z = p2->z - p1->z;
  if (ss->scalez == SLICE_ZSCALE_BEFORE)
    n.z *= ss->vi->imod->zscale;
  if (n.x == 0.0 && n.y == 0.0 && n.z == 0.0)
    return;
  imodPointNormalize(&n);

  if (axis == X) {
    a.z = 0.0;
    if (n.x > small || n.y > small || n.x < -small || n.y < -small)
      a.z = -atan2((double)n.y, (double)n.x);
    val = a.z;
    val = n.x * cos(val) - n.y * sin(val);
    a.y = fixangle(90. * rpd - atan2(val, (double)n.z));
    a.x = 0.0;
  } else if (axis == Y) {
    a.z = 0.0;
    if (n.x > small || n.y > small || n.x < -small || n.y < -small)
      a.z = fixangle(90. * rpd - atan2((double)n.y, (double)n.x));
    val = a.z;
    val = n.x * sin(val) + n.y * cos(val);
    a.x = -atan2((double)n.z, val);
    a.y = 0.0;
  } else {
    /* This may be most logical in terms of general angles available */
    /*   a.y = 0.0;
         if (n.x > small || n.z > small || n.x < -small || n.z < -small)
         a.y = -atan2((double)n.x, (double)n.z);
         val = a.y;
         val = n.z * cos(val) - n.x * sin(val);
         a.x = fixangle(90. * rpd - atan2(val, (double)n.y));
         a.z = 0.0; */

    /* But this is useful for doing Z rotation and then X, and for
       keeping all the angles under +/- 90 degrees. */
    a.z = 0.0;
    if (n.x > small || n.y > small || n.x < -small || n.y < -small)
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

  ss->tang[X] = a.x / rpd;
  ss->tang[Y] = a.y / rpd;
  ss->tang[Z] = a.z / rpd;
  ss->qtWindow->setAngles(ss->tang);
}

/* set up the step factors for a new slice angle. */
static void slice_trans_step(SlicerStruct *ss)
{
  Ipoint pnt;
  Ipoint xn, yn, zn;
  Ipoint normal, xcut, ycut, zcut;
  float x,y,z;
  float isize, jsize, zs;
  Imat *mat = ss->mat;

  /* DNM: made the angles be negative to match angles applied to volume */
  
  imodMatId(mat);
  imodMatRot(mat, (double)(-ss->tang[X]), X); 
  imodMatRot(mat, (double)(-ss->tang[Y]), Y);
  imodMatRot(mat, (double)(-ss->tang[Z]), Z);

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

  ss->xstep[X] = xn.x;
  ss->xstep[Y] = xn.y;
  ss->xstep[Z] = xn.z;
  ss->ystep[X] = yn.x;
  ss->ystep[Y] = yn.y;
  ss->ystep[Z] = yn.z;
  ss->zstep[X] = zn.x;
  ss->zstep[Y] = zn.y;
  ss->zstep[Z] = zn.z;
     
  /* Set cut points */
  ss->vi->slice.zx1 = ss->vi->slice.zx2 =
    ss->vi->slice.yx1 = ss->vi->slice.yx2 = (int)ss->vi->xmouse;
  ss->vi->slice.zy1 = ss->vi->slice.zy2 =
    ss->vi->slice.xy1 = ss->vi->slice.xy2 = (int)ss->vi->ymouse;
  ss->vi->slice.xz1 = ss->vi->slice.xz2 =
    ss->vi->slice.yz1 = ss->vi->slice.yz2 = (int)ss->vi->zmouse;

  if ((zcut.x) || (zcut.y)){
    x = ss->vi->xmouse;
    y = ss->vi->ymouse;
    do{
      x += zcut.x;
      y += zcut.y;
    }while((x < ss->vi->xsize) && (x > 0) &&
           (y < ss->vi->ysize) && (y > 0));
    ss->vi->slice.zx1 = (int)(x - zcut.x);
    ss->vi->slice.zy1 = (int)(y - zcut.y);
    x = ss->vi->xmouse;
    y = ss->vi->ymouse;
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
    x = ss->vi->xmouse;
    z = ss->vi->zmouse;
    do{
      x += ycut.x;
      z += ycut.z;
    }while((x < ss->vi->xsize) && (x > 0) &&
           (z < ss->vi->zsize) && (z > 0));
    ss->vi->slice.yx1 = (int)(x - ycut.x);
    ss->vi->slice.yz1 = (int)(z - ycut.z);
    x = ss->vi->xmouse;
    z = ss->vi->zmouse;
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
    y = ss->vi->ymouse;
    z = ss->vi->zmouse;
    do{
      y += xcut.y;
      z += xcut.z;
    }while((y < ss->vi->ysize) && (y > 0) &&
           (z < ss->vi->zsize) && (z > 0));
    ss->vi->slice.xy1 = (int)(y - xcut.y);
    ss->vi->slice.xz1 = (int)(z - xcut.z);
    y = ss->vi->ymouse;
    z = ss->vi->zmouse;
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
  zs  = ss->vi->imod->zscale; 
  if (zs <= 0.0f)
    zs = 1.0f;
  zs = 1.0f / zs;
  if (ss->scalez != SLICE_ZSCALE_BEFORE)
    zs = 1.0f;

  /* DNM: make these correct for HQ case at least */
  ss->xo -= (isize / 2) * ss->xstep[X];
  ss->yo -= (isize / 2) * ss->xstep[Y];
  ss->zo -= (isize / 2) * ss->xstep[Z] * zs;
  ss->zo -= (jsize / 2) * ss->ystep[Z] * zs;
  ss->xo -= (jsize / 2) * ss->ystep[X];
  ss->yo -= (jsize / 2) * ss->ystep[Y];

  return;
}

/*
 * Main routines for filling the data array with interpolated values
 */
/* DNM: routines to replace ivwGetValue for speedy access */

static int (*best_GetValue)(int x, int y, int z);

static int imdataxsize;
static int *vmdataxsize;
static unsigned char **imdata;
static int vmnullvalue;

static int idata_GetValue(int x, int y, int z)
{
  return(imdata[z][x + (y * imdataxsize)]);
}

static int cache_GetValue(int x, int y, int z)
{
  if (!imdata[z])
    return(vmnullvalue);
  return(imdata[z][x + (y * vmdataxsize[z])]);
}

static int fake_GetValue(int x, int y, int z)
{
  return(0);
}

static void fillImageArray(SlicerStruct *ss)
{
  int i, j, k, isize, jsize, ksize;
  int xi, yi, zi;
  unsigned short rbase;
  float xo, yo, zo;  /* coords of the lower left zap origin. */
  float xzo, yzo,zzo;
  float xs = 1.0f, ys = 1.0f, zs = 1.0f;  /* scale factors.  */
  float zoffset;
  float xsx, ysx, zsx; /* steps for moving x in zap. */
  float xsy, ysy, zsy; /* steps for moving y in zap. */
  float xsz, ysz, zsz; /* steps for moving z in zap. */

  /* for 3-D quadratic interpolation */
  float dx, dy, dz;
  float x1, x2, y1, y2, z1, z2;
  float a, b, c, d, e, f;
  float ival;
  int pxi, nxi, pyi, nyi, pzi, nzi;
  float maxval = 255.0f, minval = 0.0f;
  float xzoom, yzoom, zzoom;
  float x, y, z; /* coords of pixel in 3-D image block. */
  unsigned char val, noDataVal = 0;
  float zoom = ss->zoom;
  int iz;
  float extrashift;
  int crossget, crosswant;
  Ipoint pnt, tpnt;
  Imat *mat = ss->mat;
  int izoom, shortcut, ilimshort, jlimshort, ishort;

  int cindex;
  unsigned int *cmap = App->cvi->cramp->ramp;
  unsigned short *cidata = ss->image->id1;
  unsigned int   *idata = (unsigned int  *)cidata;
  int pixsize  = b3dGetImageType(NULL, NULL);


  /* DNM 5/16/02: force a cache load of the current z slice at least */
  iz = (int)floor((double)(ss->cz + 0.5));
  ivwGetZSection(ss->vi, iz);

  /* Set up image pointer tables */
  imdata = (unsigned char **)
    malloc(sizeof(unsigned char *) * ss->vi->zsize);

  if (!imdata)
    return;

  vmnullvalue = (App->cvi->white + App->cvi->black) / 2;
  noDataVal = vmnullvalue;
  if (ss->vi->fakeImage) {
    best_GetValue = fake_GetValue;

  } else if (ss->vi->vmSize) {
    /* For cached data, get pointers to data that exist at this time */
    vmdataxsize = (int *)malloc(sizeof(int) * ss->vi->zsize);
    if (!vmdataxsize)
      return;
    best_GetValue = cache_GetValue;
    for (i = 0; i < ss->vi->zsize; i++)
      imdata[i] = NULL;
    for (i = 0; i < ss->vi->vmSize; i++) {
      iz = ss->vi->vmCache[i].cz;
      if (iz < ss->vi->zsize && iz >= 0 &&
          ss->vi->vmCache[i].ct == ss->vi->ct){
        imdata[iz] = ss->vi->vmCache[i].sec->data.b;
        vmdataxsize[iz] = ss->vi->vmCache[i].sec->xsize;
      }
    }

  } else {
    /* for loaded data, get pointers from ss->vi */
    best_GetValue = idata_GetValue;
    for (i = 0; i < ss->vi->zsize; i++)
      imdata[i] = ss->vi->idata[i];
    imdataxsize = ss->vi->xsize;
  }

  slice_trans_step(ss);
  rbase = ss->vi->rampbase;
  if (!App->rgba && App->depth == 8){
    minval = ss->vi->rampbase;
    maxval = minval + ss->vi->rampsize;
    noDataVal = (unsigned char)minval;
  }

  ksize = ss->nslice;
  zoffset = (float)(ksize - 1) * 0.5;

  xzoom = yzoom = zzoom = ss->zoom;
  xo = ss->cx;
  yo = ss->cy;
  zo = ss->cz;

  xsx = ss->xstep[X];
  ysx = ss->xstep[Y];
  zsx = ss->xstep[Z];
  xsy = ss->ystep[X];
  ysy = ss->ystep[Y];
  zsy = ss->ystep[Z];
  xsz = ss->zstep[X];
  ysz = ss->zstep[Y];
  zsz = ss->zstep[Z];

  if ((ss->scalez) && (ss->vi->imod->zscale > 0))
    zs  = 1.0f/ss->vi->imod->zscale;

		if (ss->scalez == SLICE_ZSCALE_AFTER){
    if (ss->vi->imod->zscale > 0)
      zs = ss->vi->imod->zscale;
    xzoom = zoom * sqrt((double)
                        ((xsx * xsx + ysx * ysx + zsx * zsx * zs * zs)/
                         (xsx * xsx + ysx * ysx + zsx * zsx)));
    yzoom = zoom * sqrt((double)
                        ((xsy * xsy + ysy * ysy + zsy * zsy * zs * zs)/
                         (xsy * xsy + ysy * ysy + zsy * zsy)));
    zzoom = zoom * sqrt((double)
                        ((xsz * xsz + ysz * ysz + zsz * zsz * zs * zs)/
                         (xsz * xsz + ysz * ysz + zsz * zsz)));
          
    xs = zoom / xzoom;
    ys = zoom / yzoom;
    zs = 1.0;
  }

  /* size of 2-D loop for i, j */
  /* DNM: don't use xzoom, yzoom; make pixels be zoom x zoom */
  isize = (int)(ss->winx / zoom + 0.9);
  jsize = (int)(ss->winy / zoom + 0.9);

  /* set up high res view workproc - for future development. */
  ss->xs = xs;
  ss->ys = ys;
  ss->yline = 0;

  if (ss->hq || (int)ss->zoom != ss->zoom){ 
    /* high quality image or fractional zoom */
    isize = ss->winx; /* calculate each pixel for zoom. */
    jsize = ss->winy;
    xsx /= xzoom;
    ysx /= xzoom;
    zsx /= xzoom / zs; 
    xsy /= yzoom;
    ysy /= yzoom;
    zsy /= yzoom / zs;
    xo -= (isize / 2) * xsx;
    yo -= (isize / 2) * ysx;
    zo -= (isize / 2) * zsx;
    xo -= (jsize / 2) * xsy;
    yo -= (jsize / 2) * ysy;
    zo -= (jsize / 2) * zsy;
    ss->xshift = 0;
    ss->yshift = 0;
  }else{
    xsx *= zoom / xzoom;
    ysx *= zoom / xzoom;
    zsx *= zs * zoom / xzoom;
    xsy *= zoom / yzoom;
    ysy *= zoom / yzoom;
    zsy *= zs * zoom / yzoom;

    /* Take fractional location of data point within a pixel and
       rotate in 3D to find location in display pixel */

    imodMatId(mat);
    imodMatRot(mat, (double)ss->tang[Z], Z);
    imodMatRot(mat, (double)ss->tang[Y], Y);
    imodMatRot(mat, (double)ss->tang[X], X); 

    pnt.x = ss->cx - (int)ss->cx;
    pnt.y = ss->cy - (int)ss->cy;
    pnt.z = ss->cz - (int)ss->cz;
    imodMatTransform3D(mat, &pnt, &tpnt);
          
    if (tpnt.x < 0.0)
      tpnt.x += 1.0;
    if (tpnt.y < 0.0)
      tpnt.y += 1.0;

    /* Compute where we want the crosshair to come out in the central
       pixel, and where it will fall with no raster offset, use 
       difference to set raster offset */

    crosswant = (int)(zoom * tpnt.x);  /* don't take nearest int here! */
    if (crosswant >= zoom)
      crosswant -= (int)zoom;
    crossget = (ss->winx / 2) % (int)zoom;
    ss->xshift = crossget - crosswant;
    if (ss->xshift < 0)
      ss->xshift += zoom;

    crosswant = (int)(zoom * tpnt.y);
    if (crosswant >= zoom)
      crosswant -= (int)zoom;
    crossget = (ss->winy / 2) % (int)zoom;
    ss->yshift = crossget - crosswant;
    if (ss->yshift < 0)
      ss->yshift += zoom;

    extrashift = 0.5;      /* Needed for proper sampling */
    if (zoom == 1.0)
      extrashift = 0.0;

    xo -= ((ss->winx / 2 - ss->xshift) / zoom - extrashift) * xsx;
    yo -= ((ss->winx / 2 - ss->xshift) / zoom - extrashift) * ysx;
    zo -= ((ss->winx / 2 - ss->xshift) / zoom - extrashift) * zsx;
    xo -= ((ss->winy / 2 - ss->yshift) / zoom - extrashift) * xsy;
    yo -= ((ss->winy / 2 - ss->yshift) / zoom - extrashift) * ysy;
    zo -= ((ss->winy / 2 - ss->yshift) / zoom - extrashift) * zsy;
  }

  /* steps per step in Z are independent of HQ versus regular */
  xsz *= zoom / zzoom;
  ysz *= zoom / zzoom;
  zsz *= zs * zoom / zzoom;

  /* Save values of starting position */
  ss->xo = xo;
  ss->yo = yo;
  ss->zo = zo;

  /* Adjust for multiple slices */
  xo -= zoffset * xsz;
  yo -= zoffset * ysz;
  zo -= zoffset * zsz;
  xzo = xo; yzo = yo; zzo = zo;

  shortcut = 0;
  izoom = (int) zoom;
  if ((ss->hq != 0) && (zoom == izoom) && (zoom > 1.0)) {
    shortcut = 1;
    ilimshort = izoom * ((isize - 1) / izoom - 1);
    jlimshort = izoom * ((jsize - 1) / izoom - 1);
  } else if (!izoom) {
    /* DNM 11/22/01: workaround to Intel compiler bug - it insists on
       doing j % izoom even when shortcut is 0 */ 
    izoom = 1;
  }

  /* DNM: don't need to clear array in advance */

  for(k = 0; k < ksize; k++){
    xo = xzo;
    yo = yzo;
    zo = zzo;
          
    /* (i,j) location in zap window data. */
    for(j = 0; j < jsize; j++){
      x = xo;
      y = yo;
      z = zo;
      cindex = j * ss->winx;
      for(i = 0; i < isize; i++){

        /* DNM & RJG 2/12/03: remove floor calls - they are dog-slow only
           Pentium 4 below 2.6 GHz... */
        xi = (int)x;
        yi = (int)y;
        zi = (int)(z + 0.5);
                    
        if ((xi >= 0) && (xi < ss->vi->xsize) &&
            (yi >= 0) && (yi < ss->vi->ysize) &&
            (z > -0.5) && (zi < ss->vi->zsize)){
          val = (*best_GetValue)(xi, yi, zi);
                         
          if (ss->hq){ /* do quadratic interpolation. */
            dx = x - xi - 0.5;
            dy = y - yi - 0.5;
            dz = z - zi;
                              
            pxi = xi - 1;
            nxi = xi + 1;
            pyi = yi - 1;
            nyi = yi + 1;
            pzi = zi - 1;
            nzi = zi + 1;
                              
            if (pxi < 0) pxi = 0;
            if (nxi >= ss->vi->xsize) nxi = xi;
            if (pyi < 0) pyi = 0;
            if (nyi >= ss->vi->ysize) nyi = yi;
            if (pzi < 0) pzi = 0;
            if (nzi >= ss->vi->zsize) nzi = zi;
                              
            x1 = (*best_GetValue)(pxi,  yi,  zi);
            x2 = (*best_GetValue)(nxi,  yi,  zi);
            y1 = (*best_GetValue)( xi, pyi,  zi);
            y2 = (*best_GetValue)( xi, nyi,  zi);
            z1 = (*best_GetValue)( xi,  yi, pzi);
            z2 = (*best_GetValue)( xi,  yi, nzi);
                              
            a = (x1 + x2) * 0.5f - (float)val;
            b = (y1 + y2) * 0.5f - (float)val;
            c = (z1 + z2) * 0.5f - (float)val;
            d = (x2 - x1) * 0.5f;
            e = (y2 - y1) * 0.5f;
            f = (z2 - z1) * 0.5f;
            ival = (a * dx * dx) + 
              (b * dy * dy) + 
              (c * dz * dz) +
              (d * dx) + (e * dy) + 
              (f * dz) + (float)val;
            if (ival > maxval)
              ival = maxval;
            if (ival < minval)
              ival = minval;
            val = (unsigned char)(ival + 0.5f);
                              
          }
        }
        else
          val = noDataVal;
                    
        if (k)
          cidata[i + cindex] += val;
        else
          cidata[i + cindex] = val;
                    
        x += xsx;
        y += ysx;
        z += zsx;

        if (shortcut != 0 && ((i >= izoom && j % izoom == 0) ||
                              (i >= izoom - 1 && j % izoom != 0))
            && i < ilimshort && j >= izoom && j < jlimshort) {
          ishort = izoom - 1;
          if (j % izoom)
            ishort = ilimshort - izoom;
          x += xsx * ishort;
          y += ysx * ishort;
          z += zsx * ishort;
          i += ishort;
        }
      }
      xo += xsy;
      yo += ysy;
      zo += zsy;

    }
    xzo += xsz;
    yzo += ysz;
    zzo += zsz;
  }

  if (shortcut) {

    /* DNM 1/9/03: deleted quadratic interpolation code, turned cubic code
       into a routine that can be used by tumbler */
    slicerCubicFillin(cidata, ss->winx, ss->winy, izoom, ilimshort, jlimshort,
		      minval * ss->nslice, maxval * ss->nslice);
  }

  cindex = ss->image->width * ss->image->height;
  k = ss->nslice;

  /* for 8-bit displays, range is less then 256 gray scales. */
  if (!App->rgba && App->depth == 8){
    int tval;
    int minval = ss->vi->rampbase;
    int maxval = minval + ss->vi->rampsize;
    if (k > 1)
      for (j = 0; j < jsize; j++)
        for(i = j * ss->winx; i < j * ss->winx + isize; i++){
          tval = cidata[i]/k;
          if (tval > maxval) tval = maxval;
          if (tval < minval) tval = minval;
          cidata[i] = tval;
        }
    else
      for (j = 0; j < jsize; j++)
        for(i = j * ss->winx; i < j * ss->winx + isize; i++){
          if (cidata[i] > maxval) cidata[i] = maxval;
          if (cidata[i] < minval) cidata[i] = minval;
        }

  }else{
    switch (pixsize){
    case 1:
      if (k > 1)
        for (j = 0; j < jsize; j++)
          for(i = j * ss->winx; i < j * ss->winx + isize; i++){
            cidata[i] = (cidata[i]/k);
          }
      break;
    case 2:
      if (k > 1)
        for (j = 0; j < jsize; j++)
          for(i = j * ss->winx; i < j * ss->winx + isize; i++){
            cidata[i] = (cidata[i]/k)+ rbase;
          }
      else
        for (j = 0; j < jsize; j++)
          for(i = j * ss->winx; i < j * ss->winx + isize; i++){
            cidata[i] = cidata[i]+ rbase;
          }
      break;
    case 4:
      if (k > 1)
        for (j = jsize - 1; j >= 0; j--)
          for(i = j * ss->winx + isize - 1; i >= j * ss->winx;
              i--){
            idata[i] = cmap[(cidata[i]/k)];
          }
      else
	for (j = jsize - 1; j >= 0; j--) {
	  for(i = j * ss->winx + isize - 1; i >= j * ss->winx; i--){
	    idata[i] = cmap[cidata[i]];
	  }
	}
    }
  }
  ss->xzoom = xzoom;
  ss->yzoom = yzoom;
  free(imdata);
  if (ss->vi->vmSize)
    free(vmdataxsize);
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
  float usex, usey, usez;

  if (drawflag & IMOD_DRAW_COLORMAP) {
    ss->glw->setColormap(*(App->qColormap));
    return;
  }

  /* DNM: use a value saved in structure in case more than one window */
  if (ss->zslast != ss->vi->imod->zscale){
    ss->zslast = ss->vi->imod->zscale;
    drawflag |= IMOD_DRAW_ACTIVE;
  }

  if (drawflag & IMOD_DRAW_XYZ){
    if (!ss->locked){
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
          (ss->lz != usez)){
        ss->cx = ss->lx = usex;
        ss->cy = ss->ly = usey;
        ss->cz = ss->lz = usez;
        ss->pending = 0;
        sslice_draw(ss);
        return;
      }
    }
  }

  if (drawflag & (IMOD_DRAW_ACTIVE | IMOD_DRAW_IMAGE)){
    if (ss->pending) {
      ss->cx = ss->lx = ss->pendx;
      ss->cy = ss->ly = ss->pendy;
      ss->cz = ss->lz = ss->pendz;
      ss->pending = 0;
    }
    sslice_draw(ss);
    return;
  }
     
  if (drawflag & IMOD_DRAW_MOD){
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

  if (!ss->imageFilled)
    fillImageArray(ss);

  b3dSetCurSize(ss->winx, ss->winy);

  glPixelStorei(GL_UNPACK_ALIGNMENT, unpack);

  /* Just clear the unused edges if there are shifts */
  if (ss->xshift || ss->yshift) {
    b3dColorIndex(App->background);
    b3dDrawFilledRectangle(0, 0, ss->winx, (int)ss->yshift);
    b3dDrawFilledRectangle(0, 0, (int)ss->xshift, ss->winy);
  }

  /* DNM: one-to-one image for fractional zoom as well as in hq case */
  if (ss->hq || ss->zoom != (int)ss->zoom)
    glPixelZoom(1.0f, 1.0f);
  else
    /* DNM: don't make this xzoom, yzoom.  */
    glPixelZoom(ss->zoom, ss->zoom);

  glRasterPos2f(ss->xshift, ss->yshift);

  glDrawPixels(ss->winx, ss->winy, format, type, ss->image->id1);
    
  /* Position of cursor. */
  b3dColorIndex(App->endpoint);
  if (ss->vi->drawcursor)
    b3dDrawPlus((int)(ss->winx * 0.5f),
                (int)(ss->winy * 0.5f), 5);

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
  glRotatef(ss->tang[X], 1.0f, 0.0f, 0.0f);
  glRotatef(ss->tang[Y], 0.0f, 1.0f, 0.0f);
  glRotatef(ss->tang[Z], 0.0f, 0.0f, 1.0f);
  if (ss->scalez == SLICE_ZSCALE_BEFORE)
    glScalef(1.0f, 1.0f, ss->vi->imod->zscale);

  glTranslatef(-ss->cx, -ss->cy, -ss->cz);

  imodDrawModel(ss->vi->imod);
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
  float zs  = ss->vi->imod->zscale;
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
  xo = ss->xo; yo = ss->yo, zo = ss->zo;
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
