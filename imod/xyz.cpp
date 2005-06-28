/*
 *  xyz.c -- Open the XYZ Window; View the X, Y and Z axis.
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
#include <qdatetime.h>
#include <qapplication.h>
#include <qcursor.h>

#include "imod.h"
#include "xxyz.h"
#include "imod_display.h"
#include "b3dgfx.h"
#include "xzap.h"
#include "control.h"
#include "imod_info_cb.h"
#include "imod_input.h"
#include "autox.h"
#include "imod_edit.h"
#include "imod_model_edit.h"
#include "imod_workprocs.h"
#include "imod_moviecon.h"
#include "preferences.h"
#include "undoredo.h"
#include "istore.h"
#include "finegrain.h"

#define XYZ_BSIZE 11
#define GRAB_LENGTH 7
#define GRAB_WIDTH 3

/*************************** internal functions ***************************/
static void xyzKey_cb(ImodView *vi, void *client, int released, QKeyEvent *e);
static void xyzClose_cb(ImodView *vi, void *client, int junk);
static void xyzDraw_cb(ImodView *vi, void *client, int drawflag);

/* The resident pointer to the structure */
static struct xxyzwin *XYZ = NULL;

static QTime but1downt;
static int xyzShowSlice = 0;

/* routine for opening or raising the window */
int xxyz_open(ImodView *vi)
{
  float newzoom;
  struct xxyzwin *xx;
  int deskWidth = QApplication::desktop()->width();
  int deskHeight = QApplication::desktop()->height();

  if (XYZ) {
    XYZ->dialog->raise();
    return(0);
  }

  xx = (struct xxyzwin *)malloc(sizeof(struct xxyzwin));
  if (!xx)
    return(-1);

  xx->xydata = xx->xzdata = xx->yzdata = NULL;

  /* DNM 1/19/02: need separate fdata for each side panel */
  xx->fdataxz  = (unsigned char *)malloc(vi->xsize * vi->zsize);
  xx->fdatayz  = (unsigned char *)malloc(vi->ysize * vi->zsize);

  xx->winx = vi->xsize + vi->zsize + (3 * XYZ_BSIZE);
  xx->winy = vi->ysize + vi->zsize + (3 * XYZ_BSIZE);
  xx->vi   = vi;
  xx->exposed = 0;

  xx->zoom = 1.0;
  xx->xtrans = 0;
  xx->ytrans = 0;
  xx->hq = 0;
  xx->project = 0;
  xx->mousemode = IMOD_MMOVIE;

  while (xx->winx > deskWidth - 20 ||
         xx->winy > deskHeight - 40) {
    newzoom = b3dStepPixelZoom(xx->zoom, -1);
    if (newzoom == xx->zoom)
      break;
    xx->zoom = newzoom;
    xx->winx = (int)((vi->xsize + vi->zsize) * xx->zoom + (3 * XYZ_BSIZE));
    xx->winy = (int)((vi->ysize + vi->zsize) * xx->zoom + (3 * XYZ_BSIZE));
  }
     
  xx->dialog = new XyzWindow(xx, App->rgba, App->doublebuffer, 
			     App->qtEnableDepth, 
                             imodDialogManager.parent(IMOD_IMAGE),
                             "xyz window");
  if ((!xx->dialog)||
      (!xx->fdataxz) || (!xx->fdatayz)) {
    wprint("Error:\n\tXYZ window can't open due to low memory\n");
    if(xx->fdataxz)
      free(xx->fdataxz);
    if(xx->fdatayz)
      free(xx->fdatayz);
    free(xx);
    return(-1);
  }
  
  xx->glw = xx->dialog->mGLw;
  if (!App->rgba)
    xx->glw->setColormap(*(App->qColormap));

  xx->dialog->setCaption(imodCaption("3dmod XYZ Window"));

  xx->lx = xx->ly = xx->lz = xx->lastCacheSum -1;
  XYZ  = xx;

  xx->ctrl = ivwNewControl(vi, xyzDraw_cb, xyzClose_cb, xyzKey_cb, 
			   (void *)xx);
  imodDialogManager.add((QWidget *)xx->dialog, IMOD_IMAGE);
  
  // This one we can resize before showing, since there is no toolbar size
  // that needs to get corrected
  QSize winSize = xx->dialog->sizeHint();
  QSize glSize = xx->glw->sizeHint();
  int newHeight = xx->winy + winSize.height() - 
    (glSize.height() > 0 ? glSize.height() : 0);
  xx->dialog->resize(xx->winx, newHeight);

  imod_info_input();
  xx->dialog->show();
  xx->dialog->SetCursor(vi->imod->mousemode);
  return(0);
}

/* The controller calls here to draw the window */
static void xyzDraw_cb(ImodView *vi, void *client, int drawflag)
{
  struct xxyzwin *xx = (struct xxyzwin *)client;

  if ((!vi) || (!xx) || (!drawflag)) return;
     
  if (drawflag & IMOD_DRAW_COLORMAP) {
    xx->glw->setColormap(*(App->qColormap));
    return;
  }

  xx->dialog->SetCursor(vi->imod->mousemode);

  if (drawflag) {
    if (drawflag & IMOD_DRAW_IMAGE) {

      /* This happens whens a flip occurs: get new image spaces */
      xx->lx = xx->ly = xx->lz = -1;
      b3dFlushImage(xx->xydata);
      b3dFlushImage(xx->xzdata);
      b3dFlushImage(xx->yzdata);
      if(xx->fdataxz)
        free(xx->fdataxz);
      if(xx->fdatayz)
        free(xx->fdatayz);
      xx->fdataxz  = (unsigned char *)malloc(vi->xsize * vi->zsize);
      xx->fdatayz  = (unsigned char *)malloc(vi->ysize * vi->zsize);
    }
    if (drawflag & IMOD_DRAW_SLICE)
      xyzShowSlice = 1;
    xx->dialog->Draw();
  }
  return;
}

// This receives the close signal back from the controller, and tells the
// window to close
static void xyzClose_cb(ImodView *vi, void *client, int junk)
{
  struct xxyzwin *xx = (struct xxyzwin *)client;
  xx->dialog->close();
}

// This receives a key from the controller
static void xyzKey_cb(ImodView *vi, void *client, int released, QKeyEvent *e)
{
  struct xxyzwin *xx = (struct xxyzwin *)client;
  if (!released)
    xx->dialog->keyPressPassedOn(e);
}


// Implementation of the window class
XyzWindow::XyzWindow(struct xxyzwin *xyz, bool rgba, bool doubleBuffer, 
		     bool enableDepth, QWidget * parent,
                     const char * name, WFlags f) 
  : QMainWindow(parent, name, f)
{
  mXyz = xyz;
  QGLFormat glFormat;
  glFormat.setRgba(rgba);
  glFormat.setDoubleBuffer(doubleBuffer);
  glFormat.setDepth(enableDepth);
  mGLw = new XyzGL(xyz, glFormat, this);
  
  // Set it as main widget, set focus
  setCentralWidget(mGLw);
  setFocusPolicy(QWidget::StrongFocus);
}

// Whan a close event comes in, tell control to remove window, clean up
//  and accept
void XyzWindow::closeEvent (QCloseEvent * e )
{
  struct xxyzwin *xx = mXyz;
  ivwRemoveControl(mXyz->vi, mXyz->ctrl);
  imodDialogManager.remove((QWidget *)xx->dialog);

  /* DNM 11/17/01: stop x and y movies when close window */
  imodMovieXYZT(xx->vi, 0, 0, MOVIE_DEFAULT, MOVIE_DEFAULT);

  if(xx->fdataxz)
    free(xx->fdataxz);
  if(xx->fdatayz)
    free(xx->fdatayz);

  b3dFreeCIImage(xx->xydata);
  b3dFreeCIImage(xx->xzdata);
  b3dFreeCIImage(xx->yzdata);

  free(xx);
  XYZ = NULL;

  e->accept();
}

void XyzWindow::SetCursor(int mode)
{
  if (mXyz->mousemode == mode)
    return;
  if (mode == IMOD_MMODEL)
      mGLw->setCursor(*App->modelCursor);
    else
      mGLw->unsetCursor();
  mXyz->mousemode = mode;
}


/* DNM 1/19/02: Add this function to get the CI Images whenever size has
   changed - now that we can do HQ graphics, they need to be the window size */
void XyzWindow::GetCIImages()
{
  struct xxyzwin *xx = mXyz;
  int xdim, ydim1, ydim2;

  xdim = xx->winx;
  ydim1 = ydim2 = xx->winy;
  xx->xydata = (B3dCIImage *)b3dGetNewCIImageSize
    (xx->xydata, App->depth, xdim, ydim1);

  xx->xzdata = (B3dCIImage *)b3dGetNewCIImageSize
    (xx->xzdata, App->depth, xdim, ydim2);

  xx->yzdata = (B3dCIImage *)b3dGetNewCIImageSize
    (xx->yzdata, App->depth, xdim, ydim1);

  if (!xx->xydata || !xx->xzdata || !xx->yzdata)
    wprint("\aInsufficient memory to run this Xyz window.\n"
            "Try making it smaller or close it.\n");

  return;
}


int XyzWindow::Getxyz(int x, int y, float *mx, float *my, int *mz)
{
  struct xxyzwin *xx = mXyz;
  int nx, ny, nz;
  /* DNM 1/23/02: turn this from float to int to keep calling expressions
     as ints */
  int b2 = XYZ_BSIZE;
  float scale;
  struct ViewInfo *vi = xx->vi;

  y = xx->winy - y;
  x -= XYZ_BSIZE + xx->xwoffset;
  y -= XYZ_BSIZE + xx->ywoffset;

  nx = (int)(vi->xsize * xx->zoom);
  ny = (int)(vi->ysize * xx->zoom);
  nz = (int)(vi->zsize * xx->zoom);

  scale = 1.0/xx->zoom;

  /* Click in main image, Z-Section */
  if (mouse_in_box(0, 0, nx, ny, x, y)) {
    *mx = x * scale;
    *my = y * scale;
    *mz = (int)vi->zmouse;
    ivwBindMouse(vi);
    return(3);
  }

  /* Click in top image, Y-Section */
  if (mouse_in_box(0, ny + b2,
                   nx,  ny + nz - 1 + b2, x, y)) {
    *mx = x * scale;
    *my = vi->ymouse;
    *mz = (int)((y - b2 - ny) * scale);
    ivwBindMouse(vi);
    return(2);
  }

  /* Click in right image */
  if (mouse_in_box(nx + b2, 0,
                   nx + b2 + nz - 1,
                   ny, x, y)) {
    *mx = vi->xmouse;
    *my = y * scale;
    *mz = (int)((x - b2 - nx) * scale);
    return(1);
  }

  /* Z-Section Gadget */
  if (mouse_in_box(nx + b2 - 1, ny + b2 - 1,
                   nx + b2 + nz + 1,
                   ny + b2 + nz + 1, x, y)) {
    *mx = vi->xmouse;
    *my = vi->ymouse;
    *mz = (int)(0.5 * ((y - b2 - ny) + (x - b2 - nx)) * scale);
    return(6);
  }
     
  /* Y-Section Gadget */
  if (mouse_in_box(-1, ny, nx + 1, ny + b2, x, y)) {
    *mx = x * scale;
    *my = vi->ymouse;
    *mz = (int)vi->zmouse;
    return(5);
  }
     
  /* X-Section Gadget */
  if (mouse_in_box(nx, -1, nx + b2, ny + 1, x, y)) {
    *mx = vi->xmouse;
    *my = y * scale;
    *mz = (int)vi->zmouse;
    return(4);
  }
     
  return(0);
}


void XyzWindow::B1Press(int x, int y)
{
  struct xxyzwin *xx = mXyz;
  float mx, my;
  int mz;
  ImodView *vi   = xx->vi;
  Imod     *imod = vi->imod;
  Ipoint pnt, *spnt;
  Iindex index;
  int i;
  float temp_distance;
  float distance = -1.;
  float selsize = IMOD_SELSIZE / xx->zoom;
  int box = Getxyz(x, y, &mx, &my, &mz);

  if (!box)
    return;

  if (xx->vi->ax) {
    if (xx->vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      autox_fillmouse(xx->vi, (int)mx, (int)my);
      return;
    }
  }

  /* DNM 1/23/02: Adopt code from Zap window, get nearest point if in the
     main display panel */
  if (xx->vi->imod->mousemode == IMOD_MMODEL && box == 3) {
    pnt.x = mx;
    pnt.y = my;
    pnt.z = mz;
    vi->xmouse = mx;
    vi->ymouse = my;
    vi->zmouse = mz;
    vi->imod->cindex.contour = -1;
    vi->imod->cindex.point = -1;

    for (i = 0; i < imod->objsize; i++) {
      index.object = i;
      temp_distance = imod_obj_nearest
        (vi, &(vi->imod->obj[i]), &index , &pnt, selsize);
      if (temp_distance < 0.)
        continue;
      if (distance < 0. || distance > temp_distance) {
        distance      = temp_distance;
        vi->imod->cindex.object  = index.object;
        vi->imod->cindex.contour = index.contour;
        vi->imod->cindex.point   = index.point;
        spnt = imodPointGet(vi->imod);
        if (spnt) {
          vi->xmouse = spnt->x;
          vi->ymouse = spnt->y;
        }
      }
    }
    ivwBindMouse(xx->vi);
    imodDraw(vi, IMOD_DRAW_RETHINK | IMOD_DRAW_XYZ);
    return;
  }

  xx->vi->xmouse = mx;
  xx->vi->ymouse = my;
  xx->vi->zmouse = mz;
  ivwBindMouse(xx->vi);
     
  /* DNM 1/23/02: make it update all windows */
  imodDraw(xx->vi, IMOD_DRAW_XYZ);
  return;
}

void XyzWindow::B2Press(int x, int y)
{
  struct xxyzwin *xx = mXyz;
  float mx, my;
  int mz;
  int movie;
  struct Mod_Object  *obj;
  struct Mod_Contour *cont;
  struct Mod_Point   point;
  int pt;

  movie = Getxyz(x, y, &mx, &my, &mz);
  if (!movie)
    return;

  if (xx->vi->ax) {
    if (xx->vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      autox_sethigh(xx->vi, (int)mx, (int)my);
      return;
    }
  }

  /* DNM 12/18/93: do not start movie in slider areas */
  if (xx->vi->imod->mousemode == IMOD_MMOVIE) {
    switch(movie) {
    case 1:
      imodMovieXYZT(xx->vi, 1,
                    MOVIE_DEFAULT, MOVIE_DEFAULT, MOVIE_DEFAULT);
      break;
    case 2:
      imodMovieXYZT(xx->vi, MOVIE_DEFAULT, 1,
                    MOVIE_DEFAULT, MOVIE_DEFAULT);
      break;
    case 3:
      imodMovieXYZT(xx->vi, MOVIE_DEFAULT, MOVIE_DEFAULT, 1,
                    MOVIE_DEFAULT);
      break;
    default:
      break;
    }
    if (movie > 0 && movie < 4)
      imcSetStarterID(xx->ctrl);
    return;
  }

  if (movie != 3)
    return;

  obj = imodObjectGet(xx->vi->imod);
  if (!obj)
    return;

  // DNM 7/10/04: switch to calling function for this
  // 11/17/04: have it take care of modifying time of empty contour
  cont = ivwGetOrMakeContour(xx->vi, obj, 0);
  if (!cont)
    return;

  /* Now if times still don't match refuse the point */
  if (ivwTimeMismatch(xx->vi, 0, obj, cont)) {
    wprint("\aContour time does not match current time.\n"
	   "Set contour time to 0 to model across times.\n");
    return;
  }


  /* DNM: don't make closed contours wild if they're not */
  if (cont->psize &&  iobjClose(obj->flags) && !(cont->flags & ICONT_WILD)
      && (int)floor(cont->pts[0].z + 0.5) != mz) {
    wprint("\aXYZ will not add a point on a different section to"
           " a co-planar closed contour.\n");
    return;
  }
  point.x = mx;
  point.y = my;
  point.z = mz;
  pt = xx->vi->imod->cindex.point + 1;

  ivwRegisterInsertPoint(xx->vi, cont, &point, pt);

  /* 11/71/04: deleted test to maintain wild flag, insert takes care of it */
  xx->vi->xmouse  = mx;
  xx->vi->ymouse  = my;

  /* DNM 1/23/02: make it update all windows */
  imodDraw(xx->vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
  return;
}

void XyzWindow::B3Press(int x, int y)
{
  struct xxyzwin *xx = mXyz;
  float mx, my;
  int mz;
  int movie;
  Icont *cont;
  Iobj *obj;
  int pt;

  movie = Getxyz(x, y, &mx, &my, &mz);
  if (!movie)
    return;

  if (xx->vi->ax) {
    if (xx->vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      autox_setlow(xx->vi, (int)mx, (int)my);
      return;
    }
  }
     
     
  if (xx->vi->imod->mousemode == IMOD_MMOVIE) {
    switch(movie) {
    case 1:
      imodMovieXYZT(xx->vi, -1, 
                    MOVIE_DEFAULT, MOVIE_DEFAULT, MOVIE_DEFAULT);
      break;
    case 2:
      imodMovieXYZT(xx->vi, MOVIE_DEFAULT, -1,
                    MOVIE_DEFAULT, MOVIE_DEFAULT);
      break;
    case 3:
      imodMovieXYZT(xx->vi, MOVIE_DEFAULT, MOVIE_DEFAULT, -1,
                    MOVIE_DEFAULT);
      break;
    default:
      break;
    }
    if (movie > 0 && movie < 4)
      imcSetStarterID(xx->ctrl);
    return;
  }

  if (movie != 3)
    return;

  
  obj = imodObjectGet(xx->vi->imod);
  cont = imodContourGet(xx->vi->imod);
  pt   = xx->vi->imod->cindex.point;
  if (!cont || !obj)
    return;
  if (pt < 0)
    return;
  if (!ivwPointVisible(xx->vi, &(cont->pts[pt])))
    return;

  if (ivwTimeMismatch(xx->vi, 0, obj, cont))
    return;

  xx->vi->undo->pointShift();
  cont->pts[pt].x = mx;
  cont->pts[pt].y = my;
  xx->vi->undo->finishUnit();

  xx->vi->xmouse  = mx;
  xx->vi->ymouse  = my;
  ivwBindMouse(xx->vi);

  /* DNM 1/23/02: make it update all windows */
  imodDraw(xx->vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
  return;
}

/* DNM 1/20/02: add statements to implement pan */
void XyzWindow::B1Drag(int x, int y)
{
  struct xxyzwin *xx = mXyz;
  int sx, sy;
  int nx, ny;
  int b2 = XYZ_BSIZE;
  float scale;
  struct ViewInfo *vi = xx->vi;
  int newVal;

  sy = xx->winy - y - 1;
  sx = x - (XYZ_BSIZE + xx->xwoffset);
  sy -= XYZ_BSIZE + xx->ywoffset;

  nx = (int)(vi->xsize * xx->zoom);
  ny = (int)(vi->ysize * xx->zoom);

  scale = 1.0/xx->zoom;

  switch (xx->whichbox) {
  case 0:
  case 1:
  case 2:
  case 3:
    xx->xtrans += (x - xx->lmx);
    xx->ytrans -= (y - xx->lmy);
    Draw();
    return;

  case 6:
    newVal = (int)(0.5 * ((sy - b2 - ny) + (sx - b2 - nx)) * scale);
    if (xx->vi->zmouse == newVal)
      return;
    xx->vi->zmouse = newVal;
    break;

  case 5:
    newVal = (int)(sx * scale);
    if (xx->vi->xmouse == newVal)
      return;
    xx->vi->xmouse = newVal; 
    break;

  case 4:
    newVal = (int)(sy * scale);
    if (xx->vi->ymouse == newVal)
      return;
    xx->vi->ymouse = newVal;
    break;

  }
  ivwBindMouse(xx->vi);
  imodDraw(xx->vi, IMOD_DRAW_XYZ);
  return;
}

void XyzWindow::B2Drag(int x, int y)
{
  struct xxyzwin *xx = mXyz;
  float mx, my;
  int mz, box;
  struct Mod_Object  *obj;
  struct Mod_Contour *cont;
  struct Mod_Point   point;
  double dist;
  int pt;

  if (xx->vi->ax) {
    if (xx->vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      box = Getxyz(x, y, &mx, &my, &mz);
      if (box != 3)
        return;
      autox_sethigh(xx->vi, (int)mx, (int)my);
      return;
    }
  }

  if (xx->vi->imod->mousemode != IMOD_MMODEL)
    return;

  box = Getxyz(x, y, &mx, &my, &mz);
  if (box != 3)
    return;

  obj = imodObjectGet(xx->vi->imod);
  if (!obj)
    return;
  cont = imodContourGet(xx->vi->imod);
  if (!cont)
    return;
  pt = xx->vi->imod->cindex.point;
  if (pt < 0)
    return;

  if (ivwTimeMismatch(xx->vi, 0, obj, cont))
    return;

  /* DNM: don't make closed contours wild if they're not */
  if (cont->psize &&  iobjClose(obj->flags) && !(cont->flags & ICONT_WILD)
      && cont->pts[0].z != mz)
    return;

  point.x = mx;
  point.y = my;
  point.z = mz;

  dist = imodel_point_dist(&point, &(cont->pts[pt]));
  if (dist < scaleModelRes(xx->vi->imod->res, xx->zoom))
    return;

  ivwRegisterInsertPoint(xx->vi, cont, &point, pt);

  xx->vi->xmouse  = mx;
  xx->vi->ymouse  = my;
  ivwBindMouse(xx->vi);

  /* DNM 1/23/02: make it update all windows */
  imodDraw(xx->vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
  return;
}

void XyzWindow::B3Drag(int x, int y)
{
  struct xxyzwin *xx = mXyz;
  float mx, my;
  int mz, box;
  struct Mod_Object  *obj;
  struct Mod_Contour *cont;
  struct Mod_Point   point;
  double dist;
  int pt;

  if (xx->vi->ax) {
    if (xx->vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      box = Getxyz(x, y, &mx, &my, &mz);
      if (box != 3)
        return;
      autox_setlow(xx->vi, (int)mx, (int)my);
      return;
    }
  }

  if (xx->vi->imod->mousemode != IMOD_MMODEL)
    return;

  box = Getxyz(x, y, &mx, &my, &mz);
  if (box != 3)
    return;

  obj = imodObjectGet(xx->vi->imod);
  if (!obj)
    return;
  cont = imodContourGet(xx->vi->imod);
  if (!cont)
    return;
  pt = xx->vi->imod->cindex.point;
  if (pt < 0)
    return;

  if (ivwTimeMismatch(xx->vi, 0, obj, cont))
    return;

  point.x = mx;
  point.y = my;
  point.z = mz;

  dist = imodel_point_dist(&point, &(cont->pts[pt]));
  if (dist < scaleModelRes(xx->vi->imod->res, xx->zoom))
    return;

  pt++;
  if (pt >= cont->psize)
    pt = cont->psize - 1;

  xx->vi->imod->cindex.point = pt;
  xx->vi->undo->pointShift();
  cont->pts[pt].x = mx;
  cont->pts[pt].y = my;
  cont->pts[pt].z = mz;
  xx->vi->undo->finishUnit();

  xx->vi->xmouse  = mx;
  xx->vi->ymouse  = my;
  ivwBindMouse(xx->vi);

  /* DNM 1/23/02: make it update all windows */
  imodDraw(xx->vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
  return;
}


void XyzWindow::SetSubimage(int absStart, int winSize, int imSize, 
			       float zoom, int *drawsize, int *woffset,
			       int *dataStart)
{
  *dataStart = 0;
  *woffset = absStart;

  /* If the absolute starting point is negative, then adjust the data start
     for this; throw away one more pixel and set a small window offset
     if necessary to keep pixels synchronized */
  if (absStart < 0) {
    *dataStart = (int)(-absStart / zoom);
    *woffset = 0;
    if (zoom * *dataStart < -absStart) {
      (*dataStart)++;
      *woffset = (int)(zoom * *dataStart + absStart);
    }
  }

  /* limit # of pixels to draw if it goes past end of window - this also
     takes care of case where image starts past end of window */
  *drawsize = imSize - *dataStart;
  if (*drawsize * zoom + *woffset > winSize)
    *drawsize = (int)((winSize - *woffset) / zoom);
}     

void XyzWindow::DrawImage()
{
  struct xxyzwin *win = mXyz;
  int x, y, z, i;
  int nx = win->vi->xsize;
  int ny = win->vi->ysize;
  int nz = win->vi->zsize;
  unsigned char **id;
  unsigned char *fdata;
  int cyi;
  int cx, cy, cz;
  int imdataxsize;
  unsigned char **imdata;
  int extraImSize;
  int dataOffset;
  int drawsize;
  int sx = (int)(nx * win->zoom);
  int sy = (int)(ny * win->zoom);
  int wx1, wx2, wy1, wy2;
  int xoffset1, xoffset2, yoffset1, yoffset2;
  int width1, height1, width2, height2, cacheSum, xslice, yslice;
  bool flipped;
          
  if (!win) return;

  if (!win->exposed) return;     /* DNM: avoid crashes if Zap is movieing*/

  /* DNM 3/5/01: changed to avoid very slow ivwGetValue when data are in
     cache; set up image pointer tables */
  // Keep track of a sum of Z values in the cache in order to detect 
  // Changes in available data that will require redisplay of XZ and YZ data
  // Load current Z section first to get it into cacheSum
  id = ivwGetCurrentZSection(win->vi);
  if (ivwSetupFastAccess(win->vi, &imdata, 0, &cacheSum))
    return;

  /* Just take the X size, do not allow for possibility of cached data 
     having different X sizes */
  imdataxsize = win->vi->xsize;

  if (win->vi->vmSize) {
    win->lx = win->ly = -1;
  }
          
  glClearIndex(App->background);
  /* DNM: need to set clear colors for rgb mode */
  if (App->rgba) {
    QColor qcol = ImodPrefs->namedColor(App->background);
    glClearColor(qcol.red()/255., qcol.green()/255. , qcol.blue()/255., 0.);
  }
     
  /* DNM 1/20/02: remove the XYZ_CLEAR_HACK */
     
  glClear(GL_COLOR_BUFFER_BIT);

  ivwGetLocation(win->vi, &cx, &cy, &cz);
  cyi = cy * nx;
  flipped = !win->vi->fakeImage && 
    (!win->vi->vmSize || win->vi->fullCacheFlipped) && win->vi->li->axis == 2;

  /* Pass the image offset routine the effective image size, including
     de-zoomed borders, and convert a data offset into a negative window
     offset */
  extraImSize = (int)floor((double)(3. * XYZ_BSIZE / win->zoom + 0.5));
  b3dSetImageOffset(win->winx, nx + nz + extraImSize, win->zoom, &drawsize,
                    &win->xtrans, &win->xwoffset, &dataOffset);
  if (dataOffset)
    win->xwoffset = -(int)floor((double)(dataOffset * win->zoom + 0.5));

  b3dSetImageOffset(win->winy, ny + nz + extraImSize, win->zoom, &drawsize,
                    &win->ytrans, &win->ywoffset, &dataOffset);
  if (dataOffset)
    win->ywoffset = -(int)floor((double)(dataOffset * win->zoom + 0.5));


  /* Now compute drawing parameters for each of the subareas */
  SetSubimage(win->xwoffset + XYZ_BSIZE, win->winx, nx, win->zoom,
	      &width1, &wx1, &xoffset1);
  SetSubimage(win->xwoffset + sx + 2 * XYZ_BSIZE, win->winx, nz, 
	      win->zoom, &width2, &wx2, &xoffset2);
  SetSubimage(win->ywoffset + XYZ_BSIZE, win->winy, ny, win->zoom,
	      &height1, &wy1, &yoffset1);
  SetSubimage(win->ywoffset + sy + 2 * XYZ_BSIZE, win->winy, nz, 
	      win->zoom, &height2, &wy2, &yoffset2);

  /* imodPrintStderr("width1 %d  height1 %d  width2 %d  height2 %d\n", width1,
     height1, width2, height2);
     imodPrintStderr("wx1 %d  xoffset1 %d  wy1 %d  yoffset1 %d\n", wx1,
     xoffset1, wy1, yoffset1); */
  if (width1 > 0 && height1 > 0) {
    win->lz = cz;
    b3dDrawGreyScalePixelsHQ(id, nx,ny, xoffset1, yoffset1, wx1, wy1,
                             width1, height1, win->xydata,
                             win->vi->rampbase, win->zoom, win->zoom, 
                             win->hq, cz);
  }

  // Send out a negative xslice or yslice if the data are being reloaded,
  // this is the best way to signal that they are new to the matching routine
  if (width2 > 0 && height1 > 0) {
    xslice = cx;
    fdata  = win->fdatayz;
    if (cx != win->lx || cacheSum != win->lastCacheSum) {
      xslice = -1 - cx;
      win->lx = cx;
      if (flipped && !win->vi->fakeImage) {
        for (y = 0; y < ny; y++)
          if (imdata[y]) {
            for (z = 0; z < nz; z++) 
              fdata[z + y * nz] = imdata[y][cx + (z * imdataxsize)];
          } else {
            for (z = 0; z < nz; z++) 
              fdata[z + y * nz] = 0;
          }
      } else {
        for(z = 0; z < nz; z++) {
          if (!win->vi->fakeImage && imdata[z]) {
            for (i = z, y = 0; y < ny; y++, i += nz)
              fdata[i] = imdata[z][cx + (y * imdataxsize)];
          } else {
            for (i= z, y = 0; y < ny; y++, i += nz)
              fdata[i] = 0;
          }
        }
      }
    }
    b3dDrawGreyScalePixelsHQ(ivwMakeLinePointers(win->vi, win->fdatayz, nz, ny,
                                                 MRC_MODE_BYTE), 
                             nz, ny, xoffset2, yoffset1,
                             wx2, wy1, width2, height1, win->yzdata,
                             win->vi->rampbase, win->zoom, win->zoom, 
                             win->hq, xslice);
  }

  if (width1 > 0 && height2 > 0) {
    yslice = cy;
    fdata  = win->fdataxz;
    if (cy != win->ly || cacheSum != win->lastCacheSum) {
      yslice = -1 - cy;
      win->ly = cy;
      for(i = 0,z = 0; z < nz; z++) {
        if (flipped && !win->vi->fakeImage && imdata[cy]) {
          for(x = 0; x < nx; x++, i++)
            fdata[i] = imdata[cy][x + (z * imdataxsize)];
        } else if (!flipped && !win->vi->fakeImage && imdata[z]) {
          for(x = 0; x < nx; x++, i++)
            fdata[i] = imdata[z][x + (cy * imdataxsize)];
        } else {
          for(x = 0; x < nx; x++, i++)
            fdata[i] = 0;
        }
      }
    }       
    b3dDrawGreyScalePixelsHQ(ivwMakeLinePointers(win->vi, win->fdataxz, nx, nz,
                                                 MRC_MODE_BYTE),
                             nx, nz, xoffset1, yoffset2,
                             wx1, wy2, width1, height2, win->xzdata,
                             win->vi->rampbase, win->zoom, win->zoom, 
                             win->hq, yslice);
  }
  win->lastCacheSum = cacheSum;

  return;
}

void XyzWindow::DrawCurrentLines()
{
  struct xxyzwin *xx = mXyz;
  int cx, cy, cz, cenx, ceny, xlim, ylim;
  float z = xx->zoom;
  int bx = XYZ_BSIZE + xx->xwoffset;
  int by = XYZ_BSIZE + xx->ywoffset;
  int nx = xx->vi->xsize;
  int ny = xx->vi->ysize;
  int nz = xx->vi->zsize;
  int xsize = (int)(nx * z + 0.5);
  int ysize = (int)(ny * z + 0.5);
  int zsize = (int)(nz * z + 0.5);
  int bx2 = bx + XYZ_BSIZE + xsize;
  int by2 = by + XYZ_BSIZE + ysize;

  /* DNM 1/23/02: Put the line in the middle now that one can drag it */
  int bpad = XYZ_BSIZE / 2;
  int hlen = GRAB_LENGTH / 2;
  int hwidth = GRAB_WIDTH / 2;


  b3dLineWidth(1);

  ivwGetLocation(xx->vi, &cx, &cy, &cz);
  b3dColorIndex(App->foreground);

  /* Draw Z location crossed lines and box around X/Y plane */
  cenx = (int)(bx2 + z * cz);
  ceny = (int)(by2 + z * cz);
  xlim = (int)(bx2 + z * nz);
  ylim = (int)(by2 + z * nz);
  b3dDrawLine(bx2, ceny, xlim, ceny);
  b3dDrawLine(cenx, by2, cenx, ylim);
  b3dDrawRectangle(bx - 1, by - 1, xsize + 1, ysize + 1);

  // Back off from edges and draw grab box
  if (cenx < bx2 +  hlen)
    cenx = bx2 +  hlen;
  if (cenx > xlim - hlen)
    cenx = xlim - hlen;
  if (ceny < by2 +  hlen)
    ceny = by2 +  hlen;
  if (ceny > ylim - hlen)
    ceny = ylim - hlen;
  b3dDrawFilledRectangle(cenx - hlen, ceny - hlen,
                         GRAB_LENGTH, GRAB_LENGTH);

  /* draw x location line and box around X/Z plane */
  b3dColorIndex(App->bgnpoint);
  cenx = (int)(bx + z * cx);
  ceny = (int)(by + z * ny + bpad);
  b3dDrawLine(cenx, ceny, xlim, ceny);
  b3dDrawRectangle(bx2 - 1, by - 1, zsize + 1, ysize + 1);

  if (cenx < bx +  hwidth)
    cenx = bx +  hwidth;
  if (cenx > bx2 - XYZ_BSIZE - hwidth)
    cenx = bx2 - XYZ_BSIZE - hwidth;
  b3dDrawFilledRectangle(cenx - hwidth, ceny - hlen,
                         GRAB_WIDTH, GRAB_LENGTH);

  /* draw y location line. */
  b3dColorIndex(App->endpoint);
  cenx = (int)(bx + z * nx + bpad);
  ceny = (int)(by + z * cy);
  b3dDrawLine(cenx, ylim, cenx, ceny);
  b3dDrawRectangle(bx - 1, by2 - 1, xsize + 1, zsize + 1);

  if (ceny < by +  hwidth)
    ceny = by +  hwidth;
  if (ceny > by2 - XYZ_BSIZE - hwidth)
    ceny = by2 - XYZ_BSIZE - hwidth;
  b3dDrawFilledRectangle(cenx - hlen, ceny - hwidth,
                         GRAB_LENGTH, GRAB_WIDTH);
  return;
}

void XyzWindow::DrawGhost()
{
  return;
}

/* DNM 1/20/02: add argument ob to be able to reset color properly */
/* DNM 5/5/03: replace calls to ivPointVisible with direct Z tests to speed
   things up, now that floor call is needed */
void XyzWindow::DrawContour(Iobj *obj, int ob, int co)
{
  struct xxyzwin *xx = mXyz;
  ImodView *vi = xx->vi;
  Icont *cont = &(obj->cont[co]);
  bool currentCont = (co == vi->imod->cindex.contour) &&
    (ob == vi->imod->cindex.object);
  
  Ipoint *point, *thisPt, *lastPt;
  DrawProps contProps, ptProps;
  int pt, next, radius;
  bool thisVis, lastVis;
  float drawsize, delz, zscale;
  int nextChange, stateFlags, changeFlags;
  int handleFlags = HANDLE_LINE_COLOR | HANDLE_2DWIDTH;
  float z = xx->zoom;
  int bx = XYZ_BSIZE + xx->xwoffset;
  int by = XYZ_BSIZE + xx->ywoffset;
  int bx2 = (int)(bx + XYZ_BSIZE + floor((double)(vi->xsize * z + 0.5)));
  int by2 = (int)(by + XYZ_BSIZE + floor((double)(vi->ysize * z + 0.5)));
  int currentZ = (int)floor(vi->zmouse + 0.5);
     
  if (!cont->psize)
    return;
     
  if (ivwTimeMismatch(vi, 0, obj, cont))
    return;

  zscale = ((vi->imod->zscale ? vi->imod->zscale : 1.) * vi->zbin) / vi->xybin;
  nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, &stateFlags,
                                   handleFlags, 0);
  if (contProps.gap)
    return;

  if (!iobjScat(obj->flags)) {

    /* Open or closed contours, if they are wild or there are any changes
       coming, then need to test every
       point and draw lines between points that are visible */
    lastVis = (int)floor(cont->pts->z + 0.5) == currentZ;
    if ((cont->flags & ICONT_WILD) || nextChange >= 0) {

      /* draw line if this and last point were visible or if this is current
         contour and project is set; draw symbol if visible and one is set */
      for (pt = 0; pt < cont->psize; pt++) {
        thisPt = &(cont->pts[pt]);
	thisVis = (int)floor(thisPt->z + 0.5) == currentZ;
        if (pt && ((lastVis && thisVis) || (currentCont && xx->project)) &&
            !ptProps.gap)
          b3dDrawLine((int)(z * thisPt->x + bx),
                      (int)(z * thisPt->y + by),
                      (int)(z * lastPt->x + bx),
                      (int)(z * lastPt->y + by));
        ptProps.gap = 0;
        if (nextChange == pt)
          nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                           &ptProps, &stateFlags, 
                                           &changeFlags, handleFlags, 0);
        if (thisVis && ptProps.symtype != IOBJ_SYM_NONE)
          zapDrawSymbol((int)(z * thisPt->x + bx), 
                        (int)(z * thisPt->y + by),
                        ptProps.symtype, ptProps.symsize, ptProps.symflags);
        
        lastVis = thisVis;
        lastPt = thisPt;
      }

      /* Close if all conditions are met */
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN) && !currentCont
          && lastVis && ((int)floor(cont->pts->z + 0.5) == currentZ) && 
          !ptProps.gap)
        b3dDrawLine((int)(z * cont->pts->x + bx),
                    (int)(z * cont->pts->y + by),
                    (int)(z * lastPt->x + bx),
                    (int)(z * lastPt->y + by));

    } else if (lastVis) {

      /* If the contour is not wild or fine grained and it is on this section,
         draw it completely, close if appropriate, and draw symbols in
         separate loop */
      b3dBeginLine();
      for (pt = 0; pt < cont->psize; pt++)
        b3dVertex2i((int)(z * cont->pts[pt].x + bx),  
                    (int)(z * cont->pts[pt].y + by));
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN) && !currentCont)
        b3dVertex2i((int)(bx + z * cont->pts->x), 
                    (int)(by + z * cont->pts->y));
      b3dEndLine();
            
      if (ptProps.symtype != IOBJ_SYM_NONE)
        for (pt = 0; pt < cont->psize; pt++)
          zapDrawSymbol((int)(z * cont->pts[pt].x + bx), 
                        (int)(z * cont->pts[pt].y + by),
                        ptProps.symtype, ptProps.symsize, ptProps.symflags);
    }

    /* Now draw symbols and points in X/Z and Y/Z views */
    if (ilistSize(cont->store))
      nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                       &stateFlags, handleFlags, 0);
    for (pt = 0; pt < cont->psize; pt++) {
      point = &(cont->pts[pt]);
      if (pt == nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags,
                                         &changeFlags, handleFlags, 0);
      next = (pt + 1) % cont->psize;
      thisVis = (int)(point->y + 0.5) == (int)vi->ymouse;
        
      /* Symbol if in plane */
      if (thisVis && ptProps.symtype != IOBJ_SYM_NONE)
        zapDrawSymbol((int)(z * point->x + bx), 
                      (int)(z * point->z + by2),
                      ptProps.symtype, ptProps.symsize, ptProps.symflags);

      /* connecting line if in plane or if projecting current cont, and not 
         last point unless closure is appropriate */
      if (((thisVis && (int)(cont->pts[next].y + 0.5) == (int)vi->ymouse) || 
           (currentCont && xx->project)) && !ptProps.gap && 
          (next || (!currentCont &&
                    (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN)))))
        b3dDrawLine((int)(z * point->x + bx),
                    (int)(z * point->z + by2),
                    (int)(z * cont->pts[next].x + bx),
                    (int)(z * cont->pts[next].z + by2));

      thisVis = (int)(point->x + 0.5) == (int)vi->xmouse;
      if (thisVis && ptProps.symtype != IOBJ_SYM_NONE)
        zapDrawSymbol((int)(z * point->z + bx2), 
                      (int)(z * point->y + by),
                      ptProps.symtype, ptProps.symsize, ptProps.symflags);
      
      if (((thisVis && (int)(cont->pts[next].x + 0.5) == (int)vi->ymouse) || 
           (currentCont && xx->project)) && !ptProps.gap &&
          (next || (!currentCont &&
                    (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN)))))
        b3dDrawLine((int)(z * point->z + bx2),
                    (int)(z * point->y + by),
                    (int)(z * cont->pts[next].z + bx2),
                    (int)(z * cont->pts[next].y + by));
    
      ptProps.gap = 0;
    }
  }
     
  /* scattered contour - symbols in all three planes */
  if (ilistSize(cont->store))
    nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                       &stateFlags, handleFlags, 0);
  if (iobjScat(obj->flags) && 
      (contProps.symtype != IOBJ_SYM_NONE || nextChange >= 0)) {
    for (pt = 0; pt < cont->psize; pt++) {
      point = &(cont->pts[pt]);
      if (pt == nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags,
                                         &changeFlags, handleFlags, 0);
      if (ptProps.symtype != IOBJ_SYM_NONE) {
        if ((int)floor(point->z + 0.5) == currentZ)
          zapDrawSymbol((int)(z * point->x + bx), 
                        (int)(z * point->y + by),
                        ptProps.symtype, ptProps.symsize, ptProps.symflags);

        if ((int)(point->y + 0.5) == (int)vi->ymouse)
          zapDrawSymbol((int)(z * point->x + bx), 
                        (int)(z * point->z + by2),
                        ptProps.symtype, ptProps.symsize, ptProps.symflags);
        
        if ((int)(point->x + 0.5) == (int)vi->xmouse)
          zapDrawSymbol((int)(z * point->z + bx2), 
                        (int)(z * point->y + by),
                        ptProps.symtype, ptProps.symsize, ptProps.symflags);
      }
    }
  }

  /* scattered contour or contour with points to be draw */
  if (iobjScat(obj->flags) || obj->pdrawsize || cont->sizes ) {
    if (ilistSize(cont->store))
      nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, 
                                       &stateFlags, handleFlags, 0);
    for (pt = 0; pt < cont->psize; pt++) {
      if (pt == nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                         &ptProps, &stateFlags,
                                         &changeFlags, handleFlags, 0);
      drawsize = imodPointGetSize(obj, cont, pt) / vi->xybin;
      if (drawsize > 0) {
        point = &(cont->pts[pt]);
        if ((int)floor(point->z + 0.5) == currentZ) {
          /* If there's a size, draw a circle and a plus for a
             circle that's big enough */
          b3dDrawCircle((int)(bx + z * point->x),
                        (int)(by + z * point->y), (int)(z * drawsize));
          if (drawsize > 3)
            b3dDrawPlus((int)(bx + z * point->x),
                        (int)(by + z * point->y), 3);
        } else {
          /* for off-section, compute size of circle and draw 
             that */
          if (drawsize > 1) {
            /* draw a smaller circ if further away. */
            delz = (point->z - vi->zmouse) * zscale;
            if (delz < 0)
              delz = -delz;
            
            if (delz < drawsize - 0.01) {
              radius = (int)(sqrt((double)(drawsize * drawsize - delz * delz))
                             * z);
              b3dDrawCircle((int)(bx + z * point->x),
                            (int)(by + z * point->y), radius);
            }
          }
        }
         
        /* Do the same tests for the XZ plane */
        delz = (point->y - vi->ymouse);
        if (delz < 0)
          delz = -delz;
        if (delz <= 0.5) {
          b3dDrawCircle((int)(bx + z * point->x),
                        (int)(by2 + z * point->z), (int)(z * drawsize));
          if (drawsize > 3)
            b3dDrawPlus((int)(bx + z * point->x),
                        (int)(by2 + z * point->z), 3);
        } else if (delz < drawsize - 0.01) {
          radius = (int)(sqrt((double)(drawsize * drawsize - delz * delz))
                         * z);
          b3dDrawCircle((int)(bx + z * point->x),
                        (int)(by2 + z * point->z), radius);
        }

        /* Do the same tests for the XZ plane */
        delz = (point->x - vi->xmouse);
        if (delz < 0)
          delz = -delz;
        if (delz <= 0.5) {
          b3dDrawCircle((int)(bx2 + z * point->z),
                        (int)(by + z * point->y), (int)(z * drawsize));
          if (drawsize > 3)
            b3dDrawPlus((int)(bx2 + z * point->z),
                        (int)(by + z * point->y), 3);
        } else if (delz < drawsize - 0.01) {
          radius = (int)(sqrt((double)(drawsize * drawsize - delz * delz))
                         * z);
          b3dDrawCircle((int)(bx2 + z * point->z),
                        (int)(by + z * point->y), radius);
        }
        
      }
    }
  }

  /* draw end markers if requested */
  b3dLineWidth(contProps.linewidth2);
  if (obj->symflags & IOBJ_SYMF_ENDS) {
    b3dColorIndex(App->bgnpoint);
    point = cont->pts;

    for (pt = 0; pt < 2; pt ++) {
      if ((int)floor(point->z + 0.5) == currentZ)
        b3dDrawCross((int)(bx + z * point->x),
                     (int)(by + z * point->y), obj->symsize/2);
      if ((int)(point->y + 0.5) == (int)vi->ymouse)
        b3dDrawCross((int)(bx + z * point->x),
                     (int)(by2 + z * point->z), obj->symsize/2);
      if ((int)(point->x + 0.5) == (int)vi->xmouse)
        b3dDrawCross((int)(bx2 + z * point->z),
                     (int)(by + z * point->y), obj->symsize/2);
          
      b3dColorIndex(App->endpoint);
      point = &(cont->pts[cont->psize-1]);
    }

    /* DNM 1/21/02: need to reset color this way, not wih b3dColorIndex*/
    imodSetObjectColor(ob);

  }
  return;
}


void XyzWindow::DrawModel()
{
  struct xxyzwin *xx = mXyz;
  Imod *imod = xx->vi->imod;
  Iobj *obj;
  int ob, co;

  if (imod->drawmode <= 0)
    return;
  if (xx->vi->ghostmode)
    DrawGhost();
     
  for(ob = 0; ob < imod->objsize; ob++) {
    imodSetObjectColor(ob);
    obj = &(imod->obj[ob]);
    b3dLineWidth(obj->linewidth2);
    for(co = 0; co < imod->obj[ob].contsize; co++)
      DrawContour(obj, ob, co);
  }

  return;
}

void XyzWindow::DrawCurrentPoint()
{
  struct xxyzwin *xx = mXyz;
  Iobj *obj = imodObjectGet(xx->vi->imod);
  Icont *cont = imodContourGet(xx->vi->imod);
  Ipoint *pnt = imodPointGet(xx->vi->imod);
  Ipoint *point;
  int psize;
  int cx, cy, cz, pt;
  float z = xx->zoom;
  int bx = XYZ_BSIZE + xx->xwoffset;
  int by = XYZ_BSIZE + xx->ywoffset;
  int bx2 = (int)(bx + XYZ_BSIZE + floor((double)(xx->vi->xsize * z + 0.5)));
  int by2 = (int)(by + XYZ_BSIZE + floor((double)(xx->vi->ysize * z + 0.5)));
  int imPtSize, modPtSize, backupSize;

  ivwGetLocation(xx->vi, &cx, &cy, &cz);

  if (!xx->vi->drawcursor)
    return;

  b3dLineWidth(obj->linewidth2);
  zapCurrentPointSize(obj, &modPtSize, &backupSize, &imPtSize);
  psize = modPtSize;
  if (cont && cont->psize > 1 && 
      (pnt == cont->pts || pnt == cont->pts + cont->psize - 1))
    psize = backupSize;

  /* Draw begin and end points of selected contour in model mode. */
  if (xx->vi->imod->mousemode == IMOD_MMODEL && cont && cont->psize > 1 &&
      !ivwTimeMismatch(xx->vi, 0, obj, cont)) {

    b3dColorIndex(App->bgnpoint);
    point = cont->pts;

    for (pt = 0; pt < 2; pt ++) {
      if ((int)(point->z + 0.5) == cz)
        b3dDrawCircle((int)(z * point->x+bx),
                      (int)(z * point->y+by), modPtSize);
      if ((int)(point->y + 0.5) == cy)
        b3dDrawCircle((int)(z * point->x+bx),
                      (int)(z * point->z+by2), modPtSize);
      if ((int)(point->x + 0.5) == cx)
        b3dDrawCircle((int)(z * point->z+bx2),
                      (int)(z * point->y+by), modPtSize);

      b3dColorIndex(App->endpoint);
      point = &(cont->pts[cont->psize-1]);
    }
  }
     
  /* Draw location of current point. */
  /* DNM 1/21/02: do it like zap window, draw only if in model mode,
     otherwise draw crosses at current mouse point */
  if (xx->vi->imod->mousemode == IMOD_MMODEL &&  pnt) {
          
    if ((int)floor(pnt->z + 0.5) == cz && 
        !ivwTimeMismatch(xx->vi, 0, obj, cont)) {
      b3dColorIndex(App->curpoint);
    }else{
      b3dColorIndex(App->shadow);
    }
    b3dDrawCircle((int)(z * pnt->x+bx), (int)(z * pnt->y+by), psize);
    b3dColorIndex(App->curpoint);
    b3dDrawPlus((int)(z*pnt->x+bx), (int)(z*cz + by2), psize);
    b3dDrawPlus((int)(z * cz + bx2), (int)(by+z*pnt->y), psize);
    return;
  }
  b3dColorIndex(App->curpoint);
  b3dDrawPlus((int)(z*cx+bx), (int)(z*cy+by), imPtSize);
  b3dDrawPlus((int)(z*cx+bx), (int)(z*cz+by2), imPtSize);
  b3dDrawPlus((int)(bx2+z*cz), (int)(by+z*cy), imPtSize);
     
  return;
}

void XyzWindow::DrawAuto()
{
#ifdef FIX_xyzDrawAuto_BUG
  struct xxyzwin *xx = mXyz;
  ImodView *vi = xx->vi;
  int i, j;
  float vert[2];
  unsigned short cdat;
  int x, y;
  unsigned int pixel;
     

  if (!vi->ax)
    return;
     
  if (!vi->ax->filled)
    return;
     
  if (vi->ax->cz != vi->zmouse)
    return;

  /* Buggy need to fix. */

  cdat = App->endpoint;

  for (j = 0; j < vi->ysize; j++) {
    y = j + XYZ_BSIZE;
    for(i = 0; i < vi->xsize; i++) {
      x = i + XYZ_BSIZE;
      if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_FLOOD) {
        pixel = App->endpoint;
        if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_BLACK) {
          pixel = vi->rampbase;
        }
      }else{
        if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_BLACK) {
          pixel = vi->rampbase;
        }
                    
        if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_WHITE) {
          pixel = vi->rampbase + vi->rampsize;
        }
      }
      b3dColorIndex(pixel);
      b3dDrawFilledRectangle(x, y, 1,1);
    }
  }
#endif
  return;
}


void XyzWindow::Draw()
{
  mGLw->updateGL();
}


// Key handler
void XyzWindow::keyPressEvent ( QKeyEvent * event )
{
  struct xxyzwin *xx = mXyz;
  struct ViewInfo *vi = xx->vi;

  int keysym = event->key();
  int state = event->state();

  if (inputTestMetaKey(event))
    return;
  
  // Start with this at 1: set to 0 if NOT handled
  int handled = 1;

  ivwControlPriority(xx->vi, xx->ctrl);

  switch(keysym) {

  case Qt::Key_Minus:
    xx->zoom = b3dStepPixelZoom(xx->zoom, -1);
    Draw();
    break;
             
  case Qt::Key_Equal:
    xx->zoom = b3dStepPixelZoom(xx->zoom, 1);
    Draw();
    break;

  case Qt::Key_S:
    if ((state & Qt::ShiftButton) || (state & Qt::ControlButton)) {

      // Need to draw the window now (didn't have to in X version)
      Draw();
      if (state & Qt::ShiftButton)
	b3dAutoSnapshot("xyz", SnapShot_RGB, NULL);
      else 
	b3dAutoSnapshot("xyz", SnapShot_TIF, NULL);
    } else
      handled = 0;
    break;

  case Qt::Key_R:
    if (!(event->state() & Qt::ControlButton)) {
      xx->hq = 1 - xx->hq;
      wprint("\aHigh-resolution mode %s\n", xx->hq ? "ON" : "OFF");
      Draw();
    } else
      handled = 0;
    break;

  case Qt::Key_P:
    if (!(state & Qt::ShiftButton)) {
      handled = 0;
      break;
    }
    xx->project = 1 - xx->project;
    wprint("\aCurrent contour projection into side views %s\n",
           xx->project ? "ON" : "OFF");
    Draw();
    break;

  case Qt::Key_Escape:
    close();
    break;

  default:
    handled = 0;
    break;
  }

  if (!handled)
    inputQDefaultKeys(event, vi);
}

XyzGL::XyzGL(struct xxyzwin *xyz, QGLFormat inFormat, XyzWindow * parent,
             const char * name)
  : QGLWidget(inFormat, parent, name)
{
  mMousePressed = false;
  mXyz = xyz;
  mWin = parent;
}


// The main drawing routine
/* DNM 1/21/02: eliminate OpenGL scaling of native coordinates, make all
   model drawing routines multiply coordinates by zoom */
/* DNM 1/28/02: moved uses of the elements of xx to after the test for zz */
void XyzGL::paintGL()
{
  struct xxyzwin *xx = mXyz;
  float z;
  int bx, by, bx2, by2;

  if (!xx)
    return;
  if (!xx->exposed) return;     /* DNM: avoid crashes if Zap is movieing*/

  b3dSetCurSize(xx->winx, xx->winy);
  z = xx->zoom;
  bx = XYZ_BSIZE + xx->xwoffset;
  by = XYZ_BSIZE + xx->ywoffset;
  bx2 = (int)(bx + XYZ_BSIZE + floor((double)(xx->vi->xsize * z + 0.5)));
  by2 = (int)(by + XYZ_BSIZE + floor((double)(xx->vi->ysize * z + 0.5)));


  mWin->DrawImage();

  mWin->DrawModel();
  mWin->DrawCurrentLines();
  mWin->DrawCurrentPoint();
  mWin->DrawAuto();

  if (xyzShowSlice) {
    b3dColorIndex(App->foreground);
          
    b3dDrawLine((int)(bx + (xx->vi->slice.zx1 * xx->zoom)), 
                (int)(by + (xx->vi->slice.zy1 * xx->zoom)),
                (int)(bx + (xx->vi->slice.zx2 * xx->zoom)), 
                (int)(by + (xx->vi->slice.zy2 * xx->zoom)));

    b3dDrawLine((int)(bx + (xx->vi->slice.yx1 * xx->zoom)),
                (int)(by2+ (xx->vi->slice.yz1 * xx->zoom)),
                (int)(bx + (xx->vi->slice.yx2 * xx->zoom)),
                (int)(by2+ (xx->vi->slice.yz2 * xx->zoom)));

    b3dDrawLine((int)(bx2+ (xx->vi->slice.xz1 * xx->zoom)),
                (int)(by + (xx->vi->slice.xy1 * xx->zoom)),
                (int)(bx2+ (xx->vi->slice.xz2 * xx->zoom)),
                (int)(by + (xx->vi->slice.xy2 * xx->zoom)));

    xyzShowSlice = 0;
  }
  glFlush();

}

// The routine that initializes or reinitializes upon resize
void XyzGL::resizeGL( int winx, int winy )
{
  struct xxyzwin *xx = mXyz;

  ivwControlPriority(xx->vi, xx->ctrl);

  xx->winx = winx;
  xx->winy = winy;
  b3dSetCurSize(winx, winy);

  b3dResizeViewportXY(winx, winy);
  
  mWin->GetCIImages();
  xx->exposed = 1;
}

// Handlers for mouse events
void XyzGL::mousePressEvent(QMouseEvent * event )
{
  int button1, button2, button3;
  float mx, my;
  int mz;
  mMousePressed = true;

  ivwControlPriority(mXyz->vi, mXyz->ctrl);
  
  button1 = event->stateAfter() & ImodPrefs->actualButton(1) ? 1 : 0;
  button2 = event->stateAfter() & ImodPrefs->actualButton(2) ? 1 : 0;
  button3 = event->stateAfter() & ImodPrefs->actualButton(3) ? 1 : 0;

  if (event->button() == ImodPrefs->actualButton(1) && !button2 && !button3) {
    but1downt.start();
    mXyz->whichbox = mWin->Getxyz(event->x(), event->y(), &mx, &my, &mz);

  } else if (event->button() == ImodPrefs->actualButton(2) &&
	     !button1 && !button3) {
    mWin->B2Press(event->x(), event->y());

  } else if (event->button() == ImodPrefs->actualButton(3) &&
	     !button1 && !button2) {
    mWin->B3Press(event->x(), event->y());
  }

  mXyz->lmx = event->x();
  mXyz->lmy = event->y();
}

void XyzGL::mouseReleaseEvent( QMouseEvent * event )
{
  mMousePressed = false;
  if (event->button() == ImodPrefs->actualButton(1)) {
      if (but1downt.elapsed() > 250)
        return;
      mWin->B1Press(event->x(), event->y());
  }
}

void XyzGL::mouseMoveEvent( QMouseEvent * event )
{
  int button1, button2, button3;
  if(!mMousePressed)
    return;

  ivwControlPriority(mXyz->vi, mXyz->ctrl);
  
  button1 = event->state() & ImodPrefs->actualButton(1) ? 1 : 0;
  button2 = event->state() & ImodPrefs->actualButton(2) ? 1 : 0;
  button3 = event->state() & ImodPrefs->actualButton(3) ? 1 : 0;

  if ( (button1) && (!button2) && (!button3) && but1downt.elapsed() > 250)
    mWin->B1Drag(event->x(), event->y());
  if ( (!button1) && (button2) && (!button3))
    mWin->B2Drag(event->x(), event->y());
  if ( (!button1) && (!button2) && (button3))
    mWin->B3Drag(event->x(), event->y());
  
  mXyz->lmx = event->x();
  mXyz->lmy = event->y();
}

/*
$Log$
Revision 4.25  2005/03/20 19:55:37  mast
Eliminating duplicate functions

Revision 4.24  2004/11/21 05:50:34  mast
Switch from int to float for nearest point distance measurement

Revision 4.23  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 4.22  2004/11/02 20:16:34  mast
Switched to using curpoint color for current point

Revision 4.21  2004/11/01 22:56:51  mast
Kept floating point positions, made res zoom-dependent

Revision 4.20  2004/09/10 02:31:04  mast
replaced long with int

Revision 4.19  2004/07/11 18:27:53  mast
Made it use new function for getting contour to add points

Revision 4.18  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.17  2004/05/03 19:19:10  mast
fixed bug in test for presence of data on one plane

Revision 4.16  2004/01/05 18:35:15  mast
Changes to deal with flipped cache properly and to scale point size by
binning

Revision 4.15  2003/12/18 22:46:26  mast
Register with movie controller when start movie

Revision 4.14  2003/09/16 02:11:30  mast
Changed to access image data using new line pointers

Revision 4.13  2003/05/06 02:18:45  mast
Fixed problem with displaying z = -1 on first section

Revision 4.12  2003/04/25 03:28:33  mast
Changes for name change to 3dmod

Revision 4.11  2003/04/18 20:16:39  mast
Rename meta test function

Revision 4.10  2003/04/18 20:06:28  mast
Reject the Ctrl (meta) key on the Mac

Revision 4.9  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.8  2003/03/25 23:01:43  mast
Take nearest int in checking for current point Z value when adding points

Revision 4.7  2003/03/24 17:56:46  mast
Register with dialogManager so it can be parked with info window

Revision 4.6  2003/03/13 01:14:46  mast
Pass Ctrl R on to default keys

Revision 4.5  2003/03/12 21:35:23  mast
Test if no CIImage is returned and give error message

Revision 4.4  2003/03/12 06:36:18  mast
Fixed problem of adding or modifying contours at the wrong times

Revision 4.3  2003/03/07 15:49:55  mast
Fixed bug in drawing curretn point when no current contour

Revision 4.2  2003/03/03 22:09:49  mast
Added grab bars to the sliders and color coded the sliders and boxes.
Added ability to display spheres for any objects with point sizes.
Made all points and connecting lines display in XZ and YZ windows, with
the projection of the current contour toggleable with P.
Implemented dynamic sizes for current point and end point markers.
Eliminated separate routine for drawing current contour.

Revision 4.1  2003/02/10 20:29:03  mast
autox.cpp

Revision 1.1.2.11  2003/01/29 01:34:23  mast
implement colormaps

Revision 1.1.2.10  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.9  2003/01/23 20:13:33  mast
add include of imod_input

Revision 1.1.2.8  2003/01/13 01:15:43  mast
changes for Qt version of info window

Revision 1.1.2.7  2003/01/06 15:50:47  mast
Use imodCaption and viewport xy routine

Revision 1.1.2.6  2003/01/03 16:46:18  mast
Simplified closing logic

Revision 1.1.2.5  2003/01/02 15:43:37  mast
accept key input from controlled; use a cache sum to detect if xz and yz
data need redrawing

Revision 1.1.2.4  2002/12/14 05:23:42  mast
backing out the fancy subclass, adjusting for new visual detection

Revision 1.1.2.3  2002/12/13 07:09:19  mast
GLMainWindow needed different name for mouse event processors

Revision 1.1.2.2  2002/12/13 06:06:29  mast
using new glmainwindow and mainglwidget classes

Revision 1.1.2.1  2002/12/12 02:41:10  mast
Qt version

Revision 3.6  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.5  2002/11/27 03:22:12  mast
Changed argumnet 3 of xyz_draw_cb from long to int to avoid warnings

Revision 3.4  2002/11/25 19:23:38  mast
Made it add itself to list of controls, and restructured the
structure for closing the window to accomodate that change.

Revision 3.3  2002/01/29 03:11:47  mast
Fixed bug in xxyz_draw from accessing elements of xx before xx existence
test

Revision 3.2  2002/01/28 16:58:52  mast
Major enhancements: Made it use the same image drawing code as
the Zap window so that it would not be slow with fractional zooms; added
ability to display in high-resolution mode and to take snapshots of the
window; added ability to pan the window with the mouse; made the model
display have fixed line widths and symbol sizes independent of zoom; made
attachment to the nearest model point work just like in the Zap window;
added ability to riffle through images by dragging the current point
indicators with the mouse.
Note that the use of the b3dDrawGreyScalePixelsHQ routine is incompatible
with the now-obsolete PIXEL_DRAW_HACK required with Nvidia 0.9.5 drivers.
Removed non OpenGL code for readability.

Revision 3.1  2001/12/17 18:51:49  mast
Removed call to autox_build

*/
