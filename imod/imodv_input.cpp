/*
 *  imodv_input.c -- Keyboard and mouse input handlers for imodv.
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

// Movie timer intervals in standalone and model view modes
#define STANDALONE_INTERVAL 1
#define MODELVIEW_INTERVAL 10

#include <stdlib.h>
#include <qdatetime.h>
#include <qtimer.h>
#include <qcursor.h>
#include <qapplication.h>
#include "imodv_window.h"
#include <math.h>
#include "imodv.h"
#include "imod.h"
#include "imod_edit.h"
#include "b3dgfx.h"
#include "imod_input.h"
#include "control.h"
#include "imodv_menu.h"
#include "imodv_gfx.h"
#include "imodv_input.h"
#include "imodv_control.h"
#include "imodv_light.h"
#include "imodv_stereo.h"
#include "imodv_depthcue.h"
#include "imodv_views.h"
#include "imodv_modeled.h"
#include "imodv_image.h"
#include "imodv_objed.h"
#include "imodv_movie.h"
#include "dia_qtutils.h"


static void imodv_light_move(ImodvApp *a);
static void imodv_translate(ImodvApp *a, int x, int y);
static void imodv_translated(ImodvApp *a, int x, int y, int z);
static void imodvSelect(ImodvApp *a);
static void imodv_translated(ImodvApp *a, int x, int y, int z);
static void imodv_compute_rotation(ImodvApp *a, float x, float y, float z);
static void imodv_rotate(ImodvApp *a, int throwFlag);
static int  imodvStepTime(ImodvApp *a, int tstep);
static void processHits (ImodvApp *a, GLint hits, GLuint buffer[]);
static void imodv_start_movie(ImodvApp *a);
static void registerClipPlaneChg(ImodvApp *a);

static int pickedObject = -1;
static int pickedContour = -1;

static unsigned int ctrlDown = 0;
static unsigned int shiftDown = 0;
static unsigned int leftDown = 0;
static unsigned int midDown = 0;
static unsigned int rightDown = 0;
static int firstMove = 0;

// Look up current position of pointer and report last state of buttons/keys
static unsigned int imodv_query_pointer(ImodvApp *a, int *wx, int *wy)
{
  unsigned int maskr = leftDown | midDown | rightDown | ctrlDown | shiftDown;
  *wx = (a->mainWin->mCurGLw->mapFromGlobal(QCursor::pos())).x();
  *wy = (a->mainWin->mCurGLw->mapFromGlobal(QCursor::pos())).y();
  return(maskr);
}


static int b2x = 0;
static int b2y = 0;

void imodvKeyPress(QKeyEvent *event)
{
  ImodvApp *a = Imodv;
  IclipPlanes *clips;
  int keysym = event->key();
  int tstep = 1;
  int newval, fastdraw, ip;
  float elapsed;
  int state = event->state();
  int keypad = event->state() & Qt::Keypad;
  int shifted = state & Qt::ShiftButton;
  int ctrl = state & Qt::ControlButton;
  QString qstr, qstr2;

  if (inputTestMetaKey(event))
    return;

  inputConvertNumLock(keysym, keypad);

  if (shifted)
    tstep = 10;

  if (!Imodv->imod) return;

  if (imodDebug('k'))
    imodPrintStderr("key %x\n", keysym);

  switch(keysym){

  case Qt::Key_B:
    if (shifted){

      /* DNM 12/1/02: call this so it can keep track of open/closed state */
      imodvMenuBgcolor(1);
    }else{
      imodv_setbuffer(a);
      imodvDraw(Imodv);
    }
    break;
               
    /* Kludge, add clip data to model/object later. */
  case Qt::Key_C: /* print the current clipping plane parameters */
    if (shifted){
      imodv_control(a, 1);
    }else{
      if (a->obj){
        clips = a->imod->editGlobalClip ? 
          &a->imod->view->clips : &a->obj->clips;
        ip = clips->plane;

        /* DNM 7/31/01 remove pixsize from D */
        qstr.sprintf("Current %s clip data = (A B C D) = %g %g %g %g.\n", 
                     a->imod->editGlobalClip ? "Global": "Object",
                     clips->normal[ip].x,
                     clips->normal[ip].y,
                     clips->normal[ip].z / a->imod->zscale,
                     ((clips->normal[ip].x * clips->point[ip].x) +
                      (clips->normal[ip].y * clips->point[ip].y) +
                      (clips->normal[ip].z * clips->point[ip].z)));
        imodPrintInfo(qstr.latin1());
      }
    }
    break;

  case Qt::Key_G: /* gooder (sic) */
    fastdraw = (a->imod->view->world & WORLD_QUALITY_BITS) >>
      WORLD_QUALITY_SHIFT;
    if (!(shifted))
      fastdraw++;
    else
      fastdraw--;
    if (fastdraw < 0)
      fastdraw = 0;
    if (fastdraw > 3)
      fastdraw = 3;
    imodPrintStderr("Sphere draw quality %d\n", fastdraw + 1);
    imodvRegisterModelChg();
    imodvFinishChgUnit();
    a->imod->view->world = (a->imod->view->world & ~WORLD_QUALITY_BITS) |
      (fastdraw << WORLD_QUALITY_SHIFT);
    imodvDraw(Imodv);
    imodvObjedNewView();
    break;

  case Qt::Key_Minus:
    imodv_zoomd(Imodv, 0.95238095);
    imodvDraw(Imodv);
    break;

  case Qt::Key_Underscore:
    imodv_zoomd(Imodv, 0.5);
    imodvDraw(Imodv);
    break;

  case Qt::Key_Equal:
    imodv_zoomd(Imodv, 1.05);
    imodvDraw(Imodv);
    break;

  case Qt::Key_Plus:
    imodv_zoomd(Imodv, 2.0);
    imodvDraw(Imodv);
    break;

  case Qt::Key_S:
    if (shifted)
      imodv_auto_snapshot(NULL, SnapShot_RGB);
    else if (ctrl)
      imodv_auto_snapshot(NULL, SnapShot_TIF);
    else
      imodvStereoToggle();
    break;

    /* '[' and ']' adjust stereo */
  case Qt::Key_BracketLeft:
    a->plax -= 0.5f;
    imodvStereoUpdate();
    imodvDraw(a);
    break;

  case Qt::Key_BracketRight:
    a->plax += 0.5f;
    imodvStereoUpdate();
    imodvDraw(a);
    break;

  case Qt::Key_L:
    if (shifted)
      imodvObjectListDialog(a, 1);
    else {
      a->plax *= -1.0f;
      imodvStereoUpdate();
      imodvDraw(a);
    }
    break;

  case Qt::Key_Comma:
    newval = (int)(a->md->arot / IMODV_ROTATION_FACTOR + 0.5);
    if (newval == a->md->arot)
      newval--;
    if (!newval)
      newval = 1;
    imodvControlSetArot(a, newval);
    imodvControlIncSpeed(-1);
    break;

  case Qt::Key_Period:
    newval = (int)(a->md->arot * IMODV_ROTATION_FACTOR + 0.5);
    if (newval == a->md->arot)
      newval++;
    imodvControlSetArot(a, newval);
    imodvControlIncSpeed(1);
    break;

  case Qt::Key_M:
    if (shifted){
      imodvModelEditDialog(Imodv, 1);
    }else{
      imodvMovieDialog(Imodv, 1);
    }
    break;

  case Qt::Key_I:
    if (shifted && imodvByteImagesExist())
      imodvImageEditDialog(Imodv, 1);
    break;

  case Qt::Key_V:
    if (shifted)
      imodvViewEditDialog(a, 1);
    break;

  case Qt::Key_1:
    imodvStepTime(a,-1);
    imodvDraw(a);
    break;
  case Qt::Key_2:
    imodvStepTime(a,1);
    imodvDraw(a);
    break;
  case Qt::Key_8:
    if (a->drawall)
      a->drawall = 0;
    else
      a->drawall = 3;
    imeSetViewData(a->drawall);
    imodvDraw(a);
    break;

  case Qt::Key_9:
    imodvSelectModel(a, a->cm - 1);
    break;
               
  case Qt::Key_0:
    imodvSelectModel(a, a->cm + 1);
    break;

  case Qt::Key_Next:
    if (keypad)
      imodv_rotate_model(a,0, 0, -a->md->arot);
    else
      imodv_translated(a, 0, 0, tstep);
    break;
  case Qt::Key_Prior:
    if (keypad)
      imodv_rotate_model(a,0, 0, a->md->arot);
    else
      imodv_translated(a, 0, 0, -tstep);
    break;
  case Qt::Key_Up:
    if (keypad)
      imodv_rotate_model(a,-a->md->arot, 0, 0);
    else
      imodv_translated(a, 0, -tstep, 0);
    break;
  case Qt::Key_Down:
    if (keypad)
      imodv_rotate_model(a,a->md->arot, 0, 0);
    else
      imodv_translated(a, 0, tstep, 0);
    break;
  case Qt::Key_Right:
    if (keypad)
      imodv_rotate_model(a,0, a->md->arot, 0);
    else
      imodv_translated(a, -tstep, 0, 0);
    break;
  case Qt::Key_Left:
    if (keypad)
      imodv_rotate_model(a,0, -a->md->arot, 0);
    else
      imodv_translated(a, tstep, 0, 0);
    break;
  case Qt::Key_5:
  case Qt::Key_Enter:
    if (!keypad)
      break;
    if (!a->movie){
      a->md->xrotm = a->md->yrotm = a->md->zrotm =0;
      a->movie = 1;
    }else{
      a->movie = 0;
      a->md->xrotm = a->md->yrotm = a->md->zrotm = 0;
    }
    break;
               
  case Qt::Key_O:
    if ((shifted)){
      objed(Imodv);
    } else if (ctrl){
      if (a->standalone && imodvLoadModel() < 0)
          dia_err("Error reading model file.  No model loaded.");
    } else {
      /* output info */
      qstr.sprintf("Zoom = %g\n", a->md->zoom);
      if (a->imod->view->world & VIEW_WORLD_ON){
        qstr += qstr2.sprintf("Transformation matrix:");
        for(tstep = 0; tstep < 16; tstep++){
          if (!(tstep%4))
            qstr += qstr2.sprintf("\n");
          qstr += qstr2.sprintf("%7.3f ", a->imod->view->mat[tstep]);
        }
        qstr += qstr2.sprintf("\n");
      }
      qstr += qstr2.sprintf("Trans (x,y,z) = (%g, %g, %g)\n",
             a->imod->view->trans.x,
             a->imod->view->trans.y,
             a->imod->view->trans.z);
      qstr += qstr2.sprintf("Rotate (x,y,z) = (%g, %g, %g)\n",
             a->imod->view->rot.x,
             a->imod->view->rot.y,
             a->imod->view->rot.z);
      if (a->movieFrames) {
        elapsed = (float)(a->movieCurrent - a->movieStart) / 1000.f;
        qstr += qstr2.sprintf("%d frames / %.3f sec = %.3f FPS\n", 
               a->movieFrames, elapsed, 
               a->movieFrames / elapsed);
      }
      imodPrintInfo(qstr.latin1());
    }
    break;

    // DNM 12/29/02: actually  implement this as an on-off for image view
  case Qt::Key_Z:
    if (a->standalone)
      break;
    if (ctrl) 
      inputUndoRedo(a->vi, 0);
    else {
      a->texMap = 1 - a->texMap;
      imodvImageUpdate(a);
      imodvDraw(a);
    }
    break;

  case Qt::Key_Y:
    if (!a->standalone && ctrl) 
      inputUndoRedo(a->vi, 1);
    break;

  case Qt::Key_R:
    imodvViewMenu(VVIEW_MENU_LOWRES);
    imodvMenuLowres(a->lowres);
    break;

  case Qt::Key_A:
    if (ctrl && a->standalone)
      imodvSaveModelAs();
    break;

  case Qt::Key_D:
    if (shifted && !a->standalone && pickedContour >= 0 &&
        a->imod->cindex.object == pickedObject && 
        a->imod->cindex.contour == pickedContour) {
      inputDeleteContour(a->vi);
      pickedContour = -1;
    }
    break;

  case Qt::Key_F1:
               
    break;

  case Qt::Key_Escape:
    a->mainWin->close();
    break;
  case Qt::Key_Q:
    a->mainWin->close();
    break;
    
    // Grabs seem not to be needed and avoiding them saves a lot of trouble
  case Qt::Key_Control:
    ctrlDown = Qt::ControlButton;
    //    a->mainWin->grabKeyboard();
    break;

  case Qt::Key_Shift:
    shiftDown = Qt::ShiftButton;
    //    a->mainWin->grabKeyboard();
    break;

  }
}

void imodvKeyRelease(QKeyEvent *event)
{
  if (event->key() == Qt::Key_Control) {
    ctrlDown = 0;
    if (!shiftDown)
      Imodv->mainWin->releaseKeyboard();
  }
  if (event->key() == Qt::Key_Shift) {
    shiftDown = 0;
    if (!ctrlDown)
      Imodv->mainWin->releaseKeyboard();
  }
}

void imodvMousePress(QMouseEvent *event)
{
  ImodvApp *a = Imodv;

  // Use state after in press and release to keep track of mouse state
  leftDown = event->stateAfter() & Qt::LeftButton;
  midDown = event->stateAfter() & Qt::MidButton;
  rightDown = event->stateAfter() & Qt::RightButton;

  // Set flag for any operation that needs to do something only on first move
  firstMove = 1;

  switch(event->button()){
  case Qt::LeftButton:
    leftDown = Qt::LeftButton;
    a->lmx = event->x();
    a->lmy = event->y();
    b2x = -10;
    b2y = -10;
    /* DNM: why draw here? */
    /* imodvDraw(a); */
    break;
  case Qt::MidButton:
    b2x = a->lmx = event->x();
    b2y = a->lmy = event->y();
    /* DNM: why draw here? */
    /* imodvDraw(a); */
    break;

  case Qt::RightButton:
    imodvSelect(a);
    break;
  }
}

void imodvMouseRelease(QMouseEvent *event)
{
  ImodvApp *a = Imodv;

  leftDown = event->stateAfter() & Qt::LeftButton;
  midDown = event->stateAfter() & Qt::MidButton;
  rightDown = event->stateAfter() & Qt::RightButton;
  if ((event->button() & Qt::MidButton) && 
      !(event->state() & Qt::ShiftButton))
    imodv_rotate(a, 1);
}


void imodvMouseMove(QMouseEvent *event)
{
  ImodvApp *a = Imodv;

  // Use state in mouse move to keep track of button down
  leftDown = event->state() & Qt::LeftButton;
  midDown = event->state() & Qt::MidButton;
  rightDown = event->state() & Qt::RightButton;
  if (leftDown){
    if (!(event->state() & Qt::ShiftButton))
      /*   DNM: disable this */
      /*           imodv_fog_move(a);
                   else */
      imodv_translate(a, event->x(), event->y());
  }
  if (midDown){
    if (event->state() & Qt::ShiftButton)
      imodv_light_move(a);
    else 
      imodv_rotate(a, 0);
  }
  a->lmx = event->x();
  a->lmy = event->y();
}

/*
 *  Move the light.
 */
static void imodv_light_move(ImodvApp *a)
{
  int mx, my; 
  unsigned int maskr = imodv_query_pointer(a,&mx,&my);

  if ((maskr & Qt::MidButton) && (maskr & Qt::ShiftButton)){
    if (firstMove) {
      imodvRegisterModelChg();
      imodvFinishChgUnit();
      firstMove = 0;
    }
    /*        a->lightx += 10 * (mx - a->lmx);
              a->lighty += 10 * (my - a->lmy);
              light_move(&(a->lightx), &(a->lighty));
    */
    light_moveby(10 * (mx - a->lmx),
                 10 * (my - a->lmy));
  }
  imodvDraw(a);
  return;
}


/* model coord transformation. */

static void imodv_translate(ImodvApp *a, int x, int y)
{
  int mx, my;
  unsigned int maskr = imodv_query_pointer(a,&mx,&my);
  int dx, dy;
     
  dx = -(mx - a->lmx);
  dy = my - a->lmy;

  imodv_translated(a, dx, dy, 0);
  return;
}


void imodv_zoomd(ImodvApp *a, double zoom)
{
  int m;
  if (!a->imod) return;

  if (a->crosset){
    for(m = 0; m < a->nm; m++)
      a->mod[m]->view->rad /= zoom;
  }else{
    a->imod->view->rad /= zoom;
  }

  return;
}

/* Register a clip plane change for model or object on the first move */
static void registerClipPlaneChg(ImodvApp *a)
{
  if (firstMove) {
    if (a->imod->editGlobalClip)
      imodvRegisterModelChg();
    else
      imodvRegisterObjectChg(a->ob);
    imodvFinishChgUnit();
    firstMove = 0;
  }
}

static void imodv_translated(ImodvApp *a, int x, int y, int z)
{
  int mx, my, ip;
  unsigned int maskr = imodv_query_pointer(a,&mx,&my);
  IclipPlanes *clips;
    
  Imod *imod;
  Imat *mat = a->mat;
  Ipoint ipt, opt, spt;
  int m, mstrt, mend;
  float scrnscale;

  if ((maskr & Qt::ControlButton) || !a->moveall) {
    mstrt = a->cm;
    mend = mstrt + 1;
  } else {
    mstrt = 0;
    mend = a->nm;
  }

  /* DNM: changed to compute shift properly for each model, to take account
     of actual scale to window, and to shift by mouse move amount */

  for (m = mstrt; m < mend; m++) {
    imod = a->mod[m];
    imodMatId(mat);
    imodMatRot(mat, -(double)imod->view->rot.x, b3dX);
    imodMatRot(mat, -(double)imod->view->rot.y, b3dY);
    imodMatRot(mat, -(double)imod->view->rot.z, b3dZ);

    scrnscale = 0.5 * (a->winx > a->winy ? a->winy : a->winx) / 
      imod->view->rad;
    
    spt.x = 1.0f/scrnscale;
    spt.y = 1.0f/scrnscale;
    spt.z = 1.0f/scrnscale * 1.0f/a->mod[a->cm]->zscale;
    imodMatScale(mat, &spt);
    
    ipt.x = x;
    ipt.y = y;
    ipt.z = z;
    imodMatTransform(mat, &ipt, &opt);
    
    opt.x *= (1.0/ imod->view->scale.x);
    opt.y *= (1.0/ imod->view->scale.y);
    opt.z *= (1.0/ imod->view->scale.z);
    
    if (maskr & Qt::ControlButton){
      if (a->obj){
        registerClipPlaneChg(a);
        clips = a->imod->editGlobalClip ? 
          &a->imod->view->clips : &a->obj->clips;
        ip = clips->plane;
        if (clips->flags & (1 << ip)){
          clips->point[ip].x += opt.x;
          clips->point[ip].y += opt.y;
          clips->point[ip].z += opt.z;
        }
      }
    }else{ 
      imod->view->trans.x -= opt.x;
      imod->view->trans.y -= opt.y;
      imod->view->trans.z -= opt.z;
    }
  }
    
  imodvDraw(a);
  return;
}
    

/* Rotate the model by the actual angles x, y and z.
 * The angles are in 0.1 degree increments.
 * DNM: made this work properly.
 */
void imodv_rotate_model(ImodvApp *a, int x, int y, int z)
{
  /* IF movieing, save the current increments as ones to movie on */
  if (a->movie){
      a->md->xrotm = x;
      a->md->yrotm = y;
      a->md->zrotm = z;
  }
  imodv_compute_rotation(a, (float)x, (float)y, (float)z);
  imodvDraw(a);
  return;
}

static void imodv_compute_rotation(ImodvApp *a, float x, float y, float z)
{
  int mx, my;
  unsigned int maskr = imodv_query_pointer(a,&mx,&my);
  int m, mstrt, mend, ip;
  Imat *mat = a->mat;
  Imat *mato, *matp;
  double alpha, beta, gamma, gamrad;
  Ipoint normal;
  Ipoint scalePoint;
  Imod *imod = a->imod;
  IclipPlanes *clips = imod->editGlobalClip ? 
    &imod->view->clips : &a->obj->clips;
  ip = clips->plane;

  /* IF movieing, start the movie if necessary */
  if (a->movie && !a->wpid) {
    a->throwFactor = 1.;
    imodv_start_movie(a);
    /*  return; */
  }

  mato = imodMatNew(3);
  matp = imodMatNew(3);

  /* Compute a matrix that resolves X and Y rotations into rotation
     about a single axis */
  gamrad = atan2((double)y, (double)x);
  gamma = gamrad / 0.017453293;
  alpha = 0.1 * (x * cos(-gamrad) - y * sin(-gamrad));

  imodMatId(mat);
  imodMatRot(mat, -gamma, b3dZ);
  imodMatRot(mat, alpha, b3dX);
  imodMatRot(mat, gamma + (double)(0.1 * z), b3dZ);

  if (!(maskr & Qt::ControlButton)){

    /* Regular rotation of one or all models */

    if (!a->moveall) {
      mstrt = a->cm;
      mend = mstrt + 1;
    } else {
      mstrt = 0;
      mend = a->nm;
    }

    for (m = mstrt; m < mend; m++) {
      imod = a->mod[m];

      /* Compute current rotation matrix */
      imodMatId(mato);
      imodMatRot(mato, (double)imod->view->rot.z, b3dZ);
      imodMatRot(mato, (double)imod->view->rot.y, b3dY);
      imodMatRot(mato, (double)imod->view->rot.x, b3dX);

      /* Multiply by the new rotation, then get back to 3 angles */
      imodMatMult(mato, mat, matp);
      imodMatGetNatAngles(matp, &alpha, &beta, &gamma);
      imod->view->rot.x = alpha;
      imod->view->rot.y = beta;
      imod->view->rot.z = gamma;
    }
          
  } else if (clips->flags & (1 << ip)) {

    /* Clipping plane rotation: apply to current model only */

    registerClipPlaneChg(a);

    /* Find the normal in scaled model coordinates by scaling
       each of the components appropriately */
    scalePoint.x = clips->normal[ip].x / imod->view->scale.x;
    scalePoint.y = clips->normal[ip].y / imod->view->scale.y;
    scalePoint.z = clips->normal[ip].z / 
      (imod->view->scale.z * imod->zscale);

    /* get current rotation transform into viewing space */
    imodMatId(mato);
    imodMatRot(mato, (double)imod->view->rot.z, b3dZ);
    imodMatRot(mato, (double)imod->view->rot.y, b3dY);
    imodMatRot(mato, (double)imod->view->rot.x, b3dX);

    /* Get product of that with screen-oriented rotation */
    imodMatMult(mato, mat, matp);
    imodMatTransform(matp, &scalePoint, &normal);

    /* Back-transform normal by inverse of current transform */

    imodMatId(mato);
    imodMatRot(mato, -(double)imod->view->rot.x, b3dX);
    imodMatRot(mato, -(double)imod->view->rot.y, b3dY);
    imodMatRot(mato, -(double)imod->view->rot.z, b3dZ);
    imodMatTransform(mato, &normal, &scalePoint);

    /* Rescale components to get back to unscaled model normal */
    clips->normal[ip].x = scalePoint.x * imod->view->scale.x;
    clips->normal[ip].y = scalePoint.y * imod->view->scale.y;
    clips->normal[ip].z = scalePoint.z * 
      (imod->view->scale.z * imod->zscale);
    imodPointNormalize(&(clips->normal[ip]));

  }

  imodMatDelete(mato);
  imodMatDelete(matp);
  return;
}


/* Rotate the model or clipping planes by the amount of cursor movement. */

#define MOUSE_TO_THROW 0.25f
#define MIN_SQUARE_TO_THROW 17
#define SAME_SPEED_DISTANCE 100.

static void imodv_rotate(ImodvApp *a, int throwFlag)
{
  int mx, my, idx, idy;
  unsigned int maskr = imodv_query_pointer(a,&mx,&my);
  float dx, dy, angleScale;

  /* If movie on and not a Control rotation, then check the throw flag */

  if (a->movie && !(maskr & Qt::ControlButton)) {
    if (throwFlag) { 

      /* If throwing at end of movement, then turn off movie if
         movement is too small, otherwise set rotations to the total
         movement since the button was pressed */

      dx = (mx - b2x);
      dy = (my - b2y);
      if (dx * dx + dy * dy < MIN_SQUARE_TO_THROW) {
        a->md->xrotm = a->md->yrotm = a->md->zrotm = 0;
        a->movie = 0;
        return;
      }

      idx = (int)floor((MOUSE_TO_THROW * dy + 0.5));
      idy = (int)floor((MOUSE_TO_THROW * dx + 0.5));
      if (!idx && !idy)
        a->movie = 0;
      a->md->xrotm = idx;
      a->md->yrotm = idy;
      a->md->zrotm = 0;

      a->throwFactor = (float)(sqrt((double)(dx * dx + dy * dy)) /
                               SAME_SPEED_DISTANCE);
      /* Start movie if it is not already going */
      if (a->movie && !a->wpid)
        imodv_start_movie(a);
    }
    return;
  }
       
  /* If the mouse button has been released, don't rotate. */
  if (!(maskr & Qt::MidButton))
    return; 

  /* Turn off movie for all rotation axis. DNM add movie flag too */
  a->md->xrotm = a->md->yrotm = a->md->zrotm = 0;
  a->movie = 0;

  /* Get the total x and y movement.  The scale factor will roll the surface
     of a sphere 0.8 times the size of window's smaller dimension at the same 
     rate as the mouse */
  angleScale = 1800. / (3.142 * 0.4 * (a->winx > a->winy ? a->winy : a->winx));
  dx = (mx - a->lmx);
  dy = (my - a->lmy);
  idx = (int)floor(angleScale * dy + 0.5);
  idy = (int)floor(angleScale * dx + 0.5);
  if ((!idx) && (!idy))
    return;
     
  imodv_rotate_model(a, idx, idy, 0);

  /* This is uneeded, since the rotate_model has a draw */
  /* imodvDraw(a); */
  return;
}

/* DNM 12/16/02: removed unused callback code */

/*****************************************************************************/
#define SELECT_BUFSIZE 4096
#define OBJCONTNAME(a,b) = ((a<<16)|(b))
/*#define HIT_DEBUG       */

static void processHits (ImodvApp *a, GLint hits, GLuint buffer[])
{
  unsigned int i, j;
  GLuint names, *ptr, *ptrstr;
  unsigned int z1, z2, zav, zmin;
  int tmo, tob, tco, tpt;
  int mo, ob, co, pt;

  if (!hits) return;
  /* If it overflowed, process what's there */
  if (hits == -1) hits = SELECT_BUFSIZE/3; 

  imodGetIndex(a->imod, &ob, &co, &pt);

#ifdef HIT_DEBUG
  imodPrintStderr ("hits = %d\n", hits);
#endif
  ptr = (GLuint *) buffer;
  ptrstr = ptr;
  pt = -1;

  for (i = 0; i < hits; i++) {    /* for each hit */
    if (ptr - ptrstr >= SELECT_BUFSIZE) break;
    names = *ptr; ptr++;
    if ((ptr - ptrstr) + names + 2 > SELECT_BUFSIZE) break;
          
    z1 = *ptr; ptr++;
    z2 = *ptr; ptr++;
    zav = z1/2 + z2/2;

#ifdef HIT_DEBUG
    imodPrintStderr (" # names = %d", names);
    imodPrintStderr (";  z1 = %u;", z1);
    imodPrintStderr (" z2 = %u; ", z2);
    imodPrintStderr ("   names are ");
#endif
                
    for (j = 0; j < names; j++) {   /*  for each name */
      switch(j){
      case 0:
        tmo = *ptr;
        break;
      case 1:
        tob = *ptr;
        break;
      case 2:
        tco = *ptr;
        break;
      case 3:
        tpt = *ptr;
        break;
      }



#ifdef HIT_DEBUG
      imodPrintStderr ("%d ", *ptr);
#endif
      ptr++;          
    }

    /* If it was a good hit (4 names) and its in front of any previous, take it */
    if ((names > 3 ) && ((i == 0) || (zav <= zmin))) { 
      zmin = zav;
      mo = tmo; ob = tob; co = tco; pt = tpt;
#ifdef HIT_DEBUG
      imodPrintStderr (" *");
#endif
    }
#ifdef HIT_DEBUG
    imodPrintStderr ("\n");
    imodPrintStderr ("   zav = %u; zmin = %u\n",zav, zmin);
#endif

  }

  if (pt == -1) return;
  a->cm = mo;
  a->imod = a->mod[mo];
  imodSetIndex(a->imod, ob, co, pt);     
  if (!a->standalone){
    imod_setxyzmouse();
    pickedContour = co;
    pickedObject = ob;
  }
}

static void imodvSelect(ImodvApp *a)
{
  static unsigned int buf[SELECT_BUFSIZE];
  GLint hits;
  int x, y;

  imodv_winset(a);
  imodv_query_pointer(a, &x, &y);
  glSelectBuffer(SELECT_BUFSIZE, buf);
  glRenderMode( GL_SELECT);

  a->xPick = x;
  a->yPick = a->winy - y;

  a->wPick = a->hPick = 10;
  a->doPick = 1;
     
  glInitNames();
     
  imodvDraw(a);

  a->doPick = 0;
  hits = glRenderMode( GL_RENDER );
  processHits(a, hits, buf);

}

static int imodvStepTime(ImodvApp *a, int tstep)
{
  Iobj *obj;
  int ob, co;

  if (!a->standalone){
    if (tstep > 0)
      inputNextTime(a->vi);
    if (tstep < 0)
      inputPrevTime(a->vi); 
    return 0;
  }

  for(;;){
    a->imod->ctime += tstep;
    if (a->imod->ctime < 0){ a->imod->ctime = 0; return 0; }
    if (a->imod->ctime > a->imod->tmax){
      a->imod->ctime = a->imod->tmax;
      return a->imod->tmax;
    }
        
    for(ob = 0 ; ob < a->imod->objsize; ob++){
      obj = &a->imod->obj[ob];
      if (!iobjTime(obj->flags)) continue;
      for(co = 0; co < obj->contsize; co++){
        if (obj->cont[co].type == a->imod->ctime)
          return(a->imod->ctime);
      }
    }
  }
}

/* DNM 2/27/03: replace unix times/clock with Qt time */
int imodv_sys_time(void)
{
  QTime curTime = QTime::currentTime();
  return ((((curTime.hour() * 60 + curTime.minute()) * 60) 
           + curTime.second()) * 1000 + curTime.msec());
}

void imodvInputRaise()
{
  if (Imodv->mainWin->isVisible())
    Imodv->mainWin->raise();
  imodvDialogManager.raise(IMODV_DIALOG);
#ifdef _WIN32
  Imodv->mainWin->setActiveWindow();
#endif
}


/**************imodv movie workproc **************************/

/* DNM 11/5/00: changed logic from using interlocked time-outs and workprocs
   to using just this workproc after starting the movie.
   DNM 5/21/01: eliminated old code */

static void imodv_start_movie(ImodvApp *a)
{
  int m;
  /* DNM: new workproc approach, start it here and go on */
  a->wpid = a->mainWin->mTimer->start
    (a->standalone ? STANDALONE_INTERVAL : MODELVIEW_INTERVAL, false);
  a->movieFrames = 0;
  a->movieStart = imodv_sys_time();
  for (m = 0; m < MAX_MOVIE_TIMES; m++)
    a->movieTimes[m] = a->movieStart;
}

void imodvMovieTimeout()
{
  ImodvApp *a = Imodv;
  int index, nframes;
  float rot, scale;
     
  if (a->wpid && !ImodvClosed && a->movie && 
      (a->md->xrotm || a->md->yrotm || a->md->zrotm)) {
    a->movieFrames++;
    a->movieCurrent = imodv_sys_time();
    index = a->movieFrames % MAX_MOVIE_TIMES;
    nframes = a->movieFrames < MAX_MOVIE_TIMES ? 
      a->movieFrames : MAX_MOVIE_TIMES;
    rot = (float)sqrt((double)(a->md->xrotm * a->md->xrotm + 
                               a->md->yrotm * a->md->yrotm + 
                               a->md->zrotm * a->md->zrotm));
    scale = a->movieSpeed * a->throwFactor * 
      (a->movieCurrent - a->movieTimes[index]) / (100. * nframes * rot);
    a->movieTimes[index] = a->movieCurrent;
    imodv_compute_rotation(a, scale * a->md->xrotm, scale * a->md->yrotm, 
                           scale * a->md->zrotm);
    imodvDraw(a);
  } else {
    a->wpid = 0;
    a->mainWin->mTimer->stop();
  }
}

/*
    $Log$
    Revision 4.16  2004/09/21 20:29:10  mast
    Changes to deal with new clipping plane structures

    Revision 4.15  2004/05/31 23:35:26  mast
    Switched to new standard error functions for all debug and user output

    Revision 4.14  2004/04/28 04:38:16  mast
    Added ability to delete current contour it it was picked

    Revision 4.13  2003/12/01 20:00:30  mast
    Fixed problem with movie not changing direction when a different keypad key
    is pressed

    Revision 4.12  2003/11/26 18:15:09  mast
    Disable image menu entry unless byte images exist

    Revision 4.11  2003/11/12 18:54:52  mast
    moved quit call out, added raise call

    Revision 4.10  2003/11/04 04:43:49  mast
    Implement new method for constant rotation speed

    Revision 4.9  2003/11/01 18:12:17  mast
    changed to put out virtually all error messages to a window

    Revision 4.8  2003/06/27 20:01:36  mast
    Changes to use world flag for quality instead of fastdraw flag

    Revision 4.7  2003/04/18 20:13:40  mast
    Reject Ctrl Key (Meta) on Mac

    Revision 4.6  2003/04/17 19:27:13  mast
    keypad workaround for Mac

    Revision 4.5  2003/03/13 01:20:08  mast
    Convert numlock keypad keys so num lock can be on

    Revision 4.4  2003/02/27 23:09:21  mast
    Change to use Qt time functions for timing values

    Revision 4.3  2003/02/27 17:27:51  mast
    Use new b3dX,Y,Z

    Revision 4.2  2003/02/21 22:19:00  mast
    Use new b3d types

    Revision 4.1  2003/02/10 20:29:01  mast
    autox.cpp

    Revision 1.1.2.16  2003/01/29 01:28:53  mast
    replace imodv_exit with direct window close calls

    Revision 1.1.2.15  2003/01/27 00:30:07  mast
    Pure Qt version and general cleanup

    Revision 1.1.2.14  2003/01/23 20:10:18  mast
    Add include of imod_input

    Revision 1.1.2.13  2003/01/18 01:13:24  mast
    remove X workproc stuff

    Revision 1.1.2.12  2003/01/13 07:21:38  mast
    Changes to use new dialog manager class

    Revision 1.1.2.11  2003/01/01 19:12:31  mast
    changes to start Qt application in standalone mode

    Revision 1.1.2.10  2003/01/01 05:46:29  mast
    changes for qt version of stereo

    Revision 1.1.2.9  2002/12/30 06:47:47  mast
    Implement Z key correctly and call new dialog closing function

    Revision 1.1.2.8  2002/12/27 01:24:54  mast
    Using new background color dialog

    Revision 1.1.2.7  2002/12/23 05:01:54  mast
    Do not process more events before quitting imodv

    Revision 1.1.2.6  2002/12/19 04:37:13  mast
    Cleanup of unused global variables and defines

    Revision 1.1.2.5  2002/12/18 04:15:14  mast
    new includes for imodv modules

    Revision 1.1.2.4  2002/12/17 22:28:21  mast
    cleanup of unused variables and SGI errors

    Revision 1.1.2.3  2002/12/17 21:38:49  mast
    include imodconfig so NO_SYS_TIMES can be acted on

    Revision 1.1.2.2  2002/12/17 17:44:59  mast
    Changes for Qt version

    Revision 1.1.2.1  2002/12/15 21:14:02  mast
    conversion to cpp

    Revision 3.2  2002/12/01 16:51:34  mast
    Changes to eliminate warnings on SGI

    Revision 3.1  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

*/
