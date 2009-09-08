/*
 *  imodv_input.c -- Keyboard and mouse input handlers for imodv.
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

// Movie timer intervals in standalone and model view modes
#define STANDALONE_INTERVAL 1
#define MODELVIEW_INTERVAL 10

#include <stdlib.h>
#include <qdatetime.h>
#include <qtimer.h>
#include <qcursor.h>
#include <qapplication.h>
//Added by qt3to4:
#include <QKeyEvent>
#include <QMouseEvent>
#include "imodv_window.h"
#include <math.h>
#include "imodv.h"
#include "imod.h"
#include "imod_edit.h"
#include "imod_info_cb.h"
#include "imod_display.h"
#include "b3dgfx.h"
#include "imod_input.h"
#include "preferences.h"
#include "control.h"
#include "imodv_menu.h"
#include "imodv_ogl.h"
#include "imodv_gfx.h"
#include "imodv_input.h"
#include "imodv_control.h"
#include "imodv_light.h"
#include "imodv_stereo.h"
#include "imodv_depthcue.h"
#include "imodv_views.h"
#include "imodv_modeled.h"
#include "imodv_image.h"
#include "imodv_isosurface.h"
#include "imodv_objed.h"
#include "imodv_listobj.h"
#include "imodv_movie.h"
#include "dia_qtutils.h"


static void imodv_light_move(ImodvApp *a, int mx, int my);
static void imodv_translate(ImodvApp *a, int x, int y);
static void imodv_translated(ImodvApp *a, int x, int y, int z);
static void imodvSelect(ImodvApp *a, int mx, int my, bool moving);
static void imodv_compute_rotation(ImodvApp *a, float x, float y, float z);
static void imodv_rotate(ImodvApp *a, int mx, int my, int throwFlag,
                         int rightWasDown);
static int  imodvStepTime(ImodvApp *a, int tstep);
static void processHits (ImodvApp *a, GLint hits, GLuint buffer[], 
                         bool moving);
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
  int state = event->modifiers();
  int keypad = event->modifiers() & Qt::KeypadModifier;
  int shifted = state & Qt::ShiftModifier;
  int ctrl = state & Qt::ControlModifier;
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
      imodvObjedMoveToAxis(2);
    }
    break;
               
    /* Kludge, add clip data to model/object later. */
  case Qt::Key_C: /* print the current clipping plane parameters */
    if (shifted){
      imodv_control(a, 1);
    } else if (ctrl) {
      imodvObjedToggleClip();
    } else {
      objedObject();
      if (a->obj){
        clips = a->imod->editGlobalClip ? 
          &a->imod->view->clips : &a->obj->clips;
        ip = clips->plane;

        // DNM 7/31/01 remove pixsize from D
        qstr.sprintf("Current %s clip data = (A B C D) = %g %g %g %g.\n", 
                     a->imod->editGlobalClip ? "Global": "Object",
                     clips->normal[ip].x,
                     clips->normal[ip].y,
                     clips->normal[ip].z / a->imod->zscale,
                     ((clips->normal[ip].x * clips->point[ip].x) +
                      (clips->normal[ip].y * clips->point[ip].y) +
                      (clips->normal[ip].z * clips->point[ip].z)));
        imodPrintInfo(LATIN1(qstr));
        // imodPrintStderr("%.2f %.2f %.2f\n", clips->point[ip].x, 
        //   clips->point[ip].y, clips->point[ip].z);
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
  case Qt::Key_Plus:
    if (keypad || keysym == Qt::Key_Equal)
      imodv_zoomd(Imodv, 1.05);
    else
      imodv_zoomd(Imodv, 2.0);
    imodvDraw(Imodv);
    break;

  case Qt::Key_S:
    if (shifted) {
      if (ctrl)
        ImodPrefs->set2ndSnapFormat();
      imodv_auto_snapshot(QString::null, SnapShot_RGB);
      if (ctrl)
        ImodPrefs->restoreSnapFormat();
    } else if (ctrl)
      imodv_auto_snapshot(QString::null, SnapShot_TIF);
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
    else
      imodvObjedMoveToAxis(9);
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

  case Qt::Key_U:
    if (shifted && imodvByteImagesExist())
      imodvIsosurfaceEditDialog(Imodv, 1);
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

  case Qt::Key_PageDown:
    if (keypad)
      imodv_rotate_model(a,0, 0, -a->md->arot);
    else
      imodv_translated(a, 0, 0, tstep);
    break;
  case Qt::Key_PageUp:
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
      imodPrintInfo(LATIN1(qstr));
    }
    break;

    // DNM 12/29/02: actually  implement this as an on-off for image view
  case Qt::Key_Z:
    if (ctrl) 
      inputUndoRedo(a->vi, 0);
    else if (!a->standalone) {
      a->texMap = 1 - a->texMap;
      imodvImageUpdate(a);
      imodvDraw(a);
    }
    break;

  case Qt::Key_Y:
    if (ctrl) 
      inputUndoRedo(a->vi, 1);
    break;

  case Qt::Key_R:
    if (shifted) {
      imodvViewMenu(VVIEW_MENU_LOWRES);
      imodvMenuLowres(a->lowres);
    } else
      imodvObjedMoveToAxis(11);
    break;

  case Qt::Key_P:
    if (!a->standalone)
      imodvViewMenu(VVIEW_MENU_CURPNT);
    break;

  case Qt::Key_T:
    imodvObjedMoveToAxis(0);
    break;

  case Qt::Key_F:
    imodvObjedMoveToAxis(1);
    break;

  case Qt::Key_K:
    imodvObjedMoveToAxis(10);
    break;

  case Qt::Key_A:

    // 8/29/06: Take accelerator for save as away so this will work
    if (ctrl) {
      imodvSelectVisibleConts(a, pickedObject, pickedContour);
      imodvDraw(a);
      if (!a->standalone)
        imod_setxyzmouse();
      /*imodPrintStderr("current %d %d  picked %d %d\n",
                      a->imod->cindex.object, 
                      a->imod->cindex.contour, pickedObject,
                      pickedContour); */
    } else if (!shifted && !ctrl) {
      a->plax *= -1.0f;
      imodvStereoUpdate();
      imodvDraw(a);
    }
    break;

  case Qt::Key_D:
    if (shifted && pickedContour >= 0 &&
        ((a->imod->cindex.object == pickedObject && 
          a->imod->cindex.contour == pickedContour) || 
         imodSelectionListQuery(a->vi, pickedObject, pickedContour) > -2)) {
      inputDeleteContour(a->vi);
      pickedContour = -1;
    } else if (!shifted) {
      imodv_setbuffer(a, 1 - a->db, -1);
      imodvDraw(Imodv);
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
    ctrlDown = Qt::ControlModifier;
    //    a->mainWin->grabKeyboard();
    break;

  case Qt::Key_Shift:
    shiftDown = Qt::ShiftModifier;
    if (midDown && !a->drawLight) {
      a->drawLight = 1;
      imodvDraw(a);
    };
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
    if (Imodv->drawLight) {
      Imodv->drawLight = 0;
      imodvDraw(Imodv);
    }
  }
}

// Mouse press: start keeping track of movements
void imodvMousePress(QMouseEvent *event)
{
  ImodvApp *a = Imodv;

  // Use state after in press and release to keep track of mouse state
  leftDown = event->buttons() & ImodPrefs->actualModvButton(1);
  midDown = event->buttons() & ImodPrefs->actualModvButton(2);
  rightDown = event->buttons() & ImodPrefs->actualModvButton(3);
  utilRaiseIfNeeded(a->mainWin, event);

  // Set flag for any operation that needs to do something only on first move
  firstMove = 1;

  if (event->button() == ImodPrefs->actualModvButton(1)) {
    leftDown = ImodPrefs->actualModvButton(1);
    a->lmx = event->x();
    a->lmy = event->y();
    b2x = -10;
    b2y = -10;
    /* DNM: why draw here? */
    /* imodvDraw(a); */

  } else if (event->button() == ImodPrefs->actualModvButton(2) ||
             (event->button() == ImodPrefs->actualModvButton(3) && 
              (event->modifiers() & Qt::ShiftModifier))) {
    b2x = a->lmx = event->x();
    b2y = a->lmy = event->y();
    if (event->button() == ImodPrefs->actualModvButton(2) && 
        (event->modifiers() & Qt::ShiftModifier)) {
      a->drawLight = 1;
      imodvDraw(a);
    }

  } else if (event->button() == ImodPrefs->actualModvButton(3)) {
    imodvSelect(a, event->x(), event->y(), false);
  }
}

// Mouse release: check for throwing
void imodvMouseRelease(QMouseEvent *event)
{
  ImodvApp *a = Imodv;
  int rightWasDown = event->button() & ImodPrefs->actualModvButton(3);

  leftDown = event->buttons() & ImodPrefs->actualModvButton(1);
  midDown = event->buttons() & ImodPrefs->actualModvButton(2);
  rightDown = event->buttons() & ImodPrefs->actualModvButton(3);
  if (((event->button() & ImodPrefs->actualModvButton(2)) && 
      !(event->modifiers() & Qt::ShiftModifier)) || 
      (rightWasDown && (event->modifiers() & Qt::ShiftModifier)))
    imodv_rotate(a, event->x(), event->y(), 1, rightWasDown);
  if (a->drawLight) {
    a->drawLight = 0;
    imodvDraw(a);
  }
}

// Mouse movement with button down
void imodvMouseMove(QMouseEvent *event)
{
  ImodvApp *a = Imodv;
  static int ex, ey, modifiers;
  static bool processing = false;
  ex = event->x();
  ey = event->y();

  // Use state in mouse move to keep track of button down
  leftDown = event->buttons() & ImodPrefs->actualModvButton(1);
  midDown = event->buttons() & ImodPrefs->actualModvButton(2);
  rightDown = event->buttons() & ImodPrefs->actualModvButton(3);
  modifiers = event->modifiers();
  if (imodDebug('m'))
    imodPrintStderr("Move ex,y %d %d ", ex, ey);

  // Return after recording values if processing events, or process events
  // to stay up to date
  if (processing)
    return;
  processing = true;
  imod_info_input();
  processing = false;

  if (leftDown){
    if (!(modifiers & Qt::ShiftModifier))
      /*   DNM: disable this */
      /*           imodv_fog_move(a);
                   else */
      imodv_translate(a, ex, ey);
  }
  if (midDown && (modifiers & Qt::ShiftModifier))
    imodv_light_move(a, ex, ey);
  else if (midDown || (rightDown && (modifiers & Qt::ShiftModifier)))
    imodv_rotate(a, ex, ey, 0, rightDown);
  else if (rightDown && (modifiers & Qt::ControlModifier))
    imodvSelect(a, ex, ey, true);
  a->lmx = ex;
  a->lmy = ey;
  if (imodDebug('m'))
    imodPuts(" ");
}

/*
 *  Move the light.
 */
static void imodv_light_move(ImodvApp *a, int mx, int my)
{
  int mxp, myp; 
  unsigned int maskr = imodv_query_pointer(a,&mxp,&myp);

  if ((maskr & ImodPrefs->actualModvButton(2)) && (maskr & Qt::ShiftModifier)) {
    if (firstMove) {
      imodvRegisterModelChg();
      imodvFinishChgUnit();
      firstMove = 0;
    }
    
    // 4/3/07: remove factor of 10 so sensitivity can be less
    light_moveby(a->imod->view, mx - a->lmx, my - a->lmy);
  }
  imodvDraw(a);
}


/* model coord transformation. */

static void imodv_translate(ImodvApp *a, int mx, int my)
{
  int dx, dy;
     
  dx = -(mx - a->lmx);
  dy = my - a->lmy;

  imodv_translated(a, dx, dy, 0);
}

// Change zoom by a factor
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
}

/* Register a clip plane change for model or object on the first move */
static void registerClipPlaneChg(ImodvApp *a)
{
  if (firstMove) {
    if (a->imod->editGlobalClip)
      imodvRegisterModelChg();
    else {
      objedObject();
      imodvRegisterObjectChg(a->ob);
    }
    imodvFinishChgUnit();
    firstMove = 0;
  }
}

/*
 * Translate model or clipping plane
 */
static void imodv_translated(ImodvApp *a, int x, int y, int z)
{
  int mx, my, ip, ipst, ipnd;
  unsigned int maskr = imodv_query_pointer(a,&mx,&my);
  IclipPlanes *clips;
    
  Imod *imod;
  Imat *mat = a->mat;
  Ipoint ipt, opt, spt;
  int m, mstrt, mend;
  float scrnscale;
  double alpha, beta;

  if ((maskr & Qt::ControlModifier) || !a->moveall) {
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
    
    if (maskr & Qt::ControlModifier){
      objedObject();
      if (a->obj){
        registerClipPlaneChg(a);
        clips = a->imod->editGlobalClip ? 
          &a->imod->view->clips : &a->obj->clips;
        ipst = ipnd = clips->plane;
        if (imod->view->world & WORLD_MOVE_ALL_CLIP) {
          ipst = 0;
          ipnd = clips->count - 1;
        }
        for (ip = ipst; ip <= ipnd; ip++) 
          if (clips->flags & (1 << ip)) {
            clips->point[ip].x += opt.x;
            clips->point[ip].y += opt.y;
            clips->point[ip].z += opt.z;
            clipCenterAndAngles(a, &clips->point[ip], &clips->normal[ip], 
                                &spt, alpha, beta);
            clips->point[ip].x = -spt.x;
            clips->point[ip].y = -spt.y;
            clips->point[ip].z = -spt.z;
          }
      }
    }else{ 
      imod->view->trans.x -= opt.x;
      imod->view->trans.y -= opt.y;
      imod->view->trans.z -= opt.z;
    }
  }
    
  imodvDraw(a);
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
}

static void imodv_compute_rotation(ImodvApp *a, float x, float y, float z)
{
  int mx, my;
  unsigned int maskr = imodv_query_pointer(a,&mx,&my);
  int m, mstrt, mend, ip, ipst, ipnd;
  double alpha, beta, gamma;
  Imat *mat = a->mat;
  Imat *mato, *matp;
  Ipoint normal;
  Ipoint scalePoint;
  Imod *imod = a->imod;
  IclipPlanes *clips;

  /* IF movieing, start the movie if necessary */
  if (a->movie && !a->wpid) {
    a->throwFactor = 1.;
    imodv_start_movie(a);
    /*  return; */
  }

  mato = imodMatNew(3);
  matp = imodMatNew(3);

  imodvResolveRotation(mat, 0.1f * x, 0.1f * y, 0.1f * z);

  if (!(maskr & Qt::ControlModifier)){

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
    imodvNewModelAngles(&imod->view->rot);
          
  } else {
    objedObject();
    if (imod->editGlobalClip || a->obj) {
      if (imod->editGlobalClip)
        clips = &imod->view->clips;
      else
        clips = &a->obj->clips;
      ipst = ipnd = clips->plane;
      if (imod->view->world & WORLD_MOVE_ALL_CLIP) {
        ipst = 0;
        ipnd = clips->count - 1;
      }
      for (ip = ipst; ip <= ipnd; ip++) {
        if (clips->flags & (1 << ip)) {
          
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
          
          // Reset the fixed point to point nearest center of field
          clipCenterAndAngles(a, &clips->point[ip], &clips->normal[ip], 
                              &normal, alpha, beta);
          clips->point[ip].x = -normal.x;
          clips->point[ip].y = -normal.y;
          clips->point[ip].z = -normal.z;
        }
      }
    }
  }

  imodMatDelete(mato);
  imodMatDelete(matp);
  return;
}

/* Compute a matrix that resolves X and Y rotations into rotation
   about a single axis if both are present, and/or rotates about Z */
void imodvResolveRotation(Imat *mat, float x, float y, float z)
{
  double alpha, gamma, gamrad;
  gamrad = atan2((double)y, (double)x);
  gamma = gamrad / 0.017453293;
  alpha = x * cos(-gamrad) - y * sin(-gamrad);

  imodMatId(mat);
  imodMatRot(mat, -gamma, b3dZ);
  imodMatRot(mat, alpha, b3dX);
  imodMatRot(mat, gamma + z, b3dZ);
}


/* Rotate the model or clipping planes by the amount of cursor movement. */

#define MOUSE_TO_THROW 0.25f
#define MIN_SQUARE_TO_THROW 17
#define SAME_SPEED_DISTANCE 100.

static void imodv_rotate(ImodvApp *a, int mx, int my, int throwFlag, 
                         int rightWasDown)
{
  int mxp, myp, idx = 0, idy = 0, idz = 0;
  unsigned int maskr = imodv_query_pointer(a,&mxp,&myp);
  float dx, dy, angleScale;

  /* If movie on and not a Control rotation, then check the throw flag */

  if (a->movie && !(maskr & Qt::ControlModifier)) {
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

      // Got to figure out which button it was!
      if (rightWasDown) {
        idz = B3DNINT( 10. * utilMouseZaxisRotation(a->winx, mx, b2x, 
                                                    a->winy, my, b2y));
      } else {
        idx = B3DNINT(MOUSE_TO_THROW * dy);
        idy = B3DNINT(MOUSE_TO_THROW * dx);
      }
      if (!idx && !idy && !idz)
        a->movie = 0;
      a->md->xrotm = idx;
      a->md->yrotm = idy;
      a->md->zrotm = idz;

      a->throwFactor = (float)(sqrt((double)(dx * dx + dy * dy)) /
                               SAME_SPEED_DISTANCE);
      /* Start movie if it is not already going */
      if (a->movie && !a->wpid)
        imodv_start_movie(a);
    }
    return;
  }
       
  /* If the mouse button has been released, don't rotate. */
  if (!(maskr & (ImodPrefs->actualModvButton(2) | 
                 ImodPrefs->actualModvButton(3))))
    return; 

  /* Turn off movie for all rotation axis. DNM add movie flag too */
  a->md->xrotm = a->md->yrotm = a->md->zrotm = 0;
  a->movie = 0;

  if (midDown) {

    /* Get the total x and y movement.  The scale factor will roll the surface
       of a sphere 0.8 times the size of window's smaller dimension at the 
       same rate as the mouse */
    dx = (mx - a->lmx);
    dy = (my - a->lmy);
    angleScale = 1800. / (3.142 * 0.4 * B3DMIN(a->winx, a->winy));
    idx = B3DNINT(angleScale * dy);
    idy = B3DNINT(angleScale * dx);
  } else {
    idz = B3DNINT( 10. * utilMouseZaxisRotation(a->winx, mx, a->lmx, 
                                                a->winy, my, a->lmy));
  }
  if ((!idx) && (!idy) && !idz)
    return;
     
  if (imodDebug('m'))
    imodPrintStderr("mx,y %d %d  lmx,y %d %d",  mx, my, a->lmx, a->lmy);
  imodv_rotate_model(a, idx, idy, idz);

  /* This is uneeded, since the rotate_model has a draw */
  /* imodvDraw(a); */
  return;
}

/*
 * Compute the point on a clipping plane that is closest to the center of the
 * of the window view center, and the angles of rotation for the normal
 */
void clipCenterAndAngles(ImodvApp *a, Ipoint *clipPoint, Ipoint *clipNormal, 
                         Ipoint *cen, double &alpha, double &beta)
{
  float smallVal = 1.e-4;
  double zrot;
  Iview *vw = a->imod->view;
  float tt;
  Ipoint normal, trans, point;
  float zscale = a->imod->zscale ? a->imod->zscale : 1.;

  normal = *clipNormal;
  normal.z /= zscale;
  imodPointNormalize(&normal);
  trans = vw->trans;
  trans.z *= zscale;
  point = *clipPoint;
  point.z *= zscale;
  alpha = 0.;
  beta = 0.;
  if (fabs((double)normal.x) > smallVal || fabs((double)normal.z) > smallVal)
    beta = -atan2((double)normal.x, (double)normal.z);
  zrot = normal.z * cos(beta) - normal.x * sin(beta);
  alpha = -(atan2(zrot, (double)normal.y) - 1.570796);

  // Get a center point: point on plane closest to the center of the display
  tt = imodPointDot(&normal, &trans) - imodPointDot(&normal, &point);
  cen->x = normal.x * tt - trans.x;
  cen->y = normal.y * tt - trans.y;
  cen->z = normal.z * tt - trans.z;
  /*imodPrintStderr("cen %.2f %.2f %.2f  d %.2f cen prod %.2f \n", cen->x,
                  cen->y, cen->z, -imodPointDot(&normal, &point),
                  imodPointDot(&normal, cen)); */
  cen->z /= zscale;
}


/* DNM 12/16/02: removed unused callback code */

/*****************************************************************************/
#define SELECT_BUFSIZE 40960

static void processHits (ImodvApp *a, GLint hits, GLuint buffer[], bool moving)
{
  unsigned int i, j;
  GLuint names, *ptr, *ptrstr;
  unsigned int z1, z2, zav, zmin;
  int tmo, tob, tco, tpt;
  int mo, ob, co, pt;
  Iindex indSave;
  Iobj *obj;

  if (!hits) 
    return;

  /* If it overflowed, process what's there */
  if (hits < 0) 
    hits = SELECT_BUFSIZE/3; 

  imodGetIndex(a->imod, &ob, &co, &pt);

  ptr = (GLuint *) buffer;
  ptrstr = ptr;
  pt = -1;

  for (i = 0; i < hits; i++) {    /* for each hit */
    if (ptr - ptrstr >= SELECT_BUFSIZE - 7) 
      break;
    names = *ptr; ptr++;
    if ((ptr - ptrstr) + names + 2 > SELECT_BUFSIZE) 
      break;
          
    z1 = *ptr; ptr++;
    z2 = *ptr; ptr++;
    zav = z1/2 + z2/2;

    if (imodDebug('p')) {
      imodPrintStderr (" # names = %d", names);
      imodPrintStderr (";  z1 = %u;", z1);
      imodPrintStderr (" z2 = %u; ", z2);
      imodPrintStderr ("   names are ");
    }
                
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



      if (imodDebug('p'))
        imodPrintStderr ("%d ", *ptr);
      ptr++;          
    }

    /* If it was a good hit (4 names) and its in front of any previous, take it */
    if ((names > 3 ) && ((i == 0) || (zav <= zmin))) { 
      zmin = zav;
      mo = tmo; ob = tob; co = tco; pt = tpt;
      if (imodDebug('p'))
      imodPrintStderr (" *");
    }
    if (imodDebug('p')) {
      imodPrintStderr ("\n");
      imodPrintStderr ("   zav = %u; zmin = %u\n",zav, zmin);
    }

  }

  if (imodDebug('p'))
      imodPrintStderr ("hits = %d\n", hits);

  if (co < 0 || pt == -1 || ob >= a->mod[mo]->objsize)
    return;

  // 11/29/08: call central select function if changing model
  if (a->cm != mo)
    imodvSelectModel(a, mo);

  if (ob >= 0)
    obj = &a->imod->obj[ob];
  else {
    obj = ivwGetAnExtraObject(a->vi, -1 - ob);
    if (!obj || a->standalone)
      return;
    
    // For an extra object with contours, just set mouse from point position
    if (obj->contsize) {
      a->vi->xmouse = obj->cont[co].pts[pt].x;
      a->vi->ymouse = obj->cont[co].pts[pt].y;
      a->vi->zmouse = obj->cont[co].pts[pt].z;
      ivwBindMouse(a->vi);
      if (imodDebug('p'))
        imodPrintStderr ("Extra object, point at %.1f %.1f %.1f\n", 
                         a->vi->xmouse, a->vi->ymouse, a->vi->zmouse);
      imodDraw(a->vi, IMOD_DRAW_XYZ);
      return;
    }
  }

  // Now if there are contours, process as an indexable point
  if (obj->contsize) {
    indSave = a->imod->cindex;
    imodSetIndex(a->imod, ob, co, pt);     
    if (!moving || imodSelectionListQuery(a->vi, ob, co) < -1)
      imodSelectionNewCurPoint(a->vi, a->imod, indSave, ctrlDown);
    if (!a->standalone)
      imod_setxyzmouse();
    else
      imodvDraw(a);
    pickedContour = a->imod->cindex.contour;
    pickedObject = a->imod->cindex.object;
    if (imodDebug('p'))
      imodPrintStderr("hit %d %d  current picked %d %d\n", ob, co, 
                      pickedObject, pickedContour);
  } else {

    // Otherwise look up position in mesh
    if (co >= obj->meshsize || pt >= obj->mesh[co].vsize || a->standalone)
      return;
    a->vi->xmouse = obj->mesh[co].vert[pt].x;
    a->vi->ymouse = obj->mesh[co].vert[pt].y;
    a->vi->zmouse = obj->mesh[co].vert[pt].z;
    ivwBindMouse(a->vi);
    imodDraw(a->vi, IMOD_DRAW_XYZ);
    if (imodDebug('p'))
      imodPrintStderr ("Contourless mesh, point at %.1f %.1f %.1f\n", 
                       a->vi->xmouse, a->vi->ymouse, a->vi->zmouse);
  }
}

// For select mode, set up for picking then call draw routine
static void imodvSelect(ImodvApp *a, int x, int y, bool moving)
{

  // 5/29/08: This was static, but why?  It stays in scope while needed.
  GLuint buf[SELECT_BUFSIZE];
  GLint hits;
  //QTime picktime;
  //picktime.start();

  imodv_winset(a);
  glSelectBuffer(SELECT_BUFSIZE, buf);

  // Defer entering selection mode until inside the paint routine and context
  // is already set.  This avoid context-setting errors on some systems
  a->xPick = x;
  a->yPick = a->winy - y;

  a->wPick = a->hPick = 10;
  a->doPick = 1;
     
  imodvDraw(a);

  a->doPick = 0;
  hits = glRenderMode( GL_RENDER );
  processHits(a, hits, buf, moving);
  //imodPrintStderr("Pick time %d\n", picktime.elapsed());
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
        if (obj->cont[co].time == a->imod->ctime)
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
  Imodv->mainWin->activateWindow();
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
  a->mainWin->mTimer->start 
    (a->standalone ? STANDALONE_INTERVAL : MODELVIEW_INTERVAL);
  a->wpid = a->mainWin->mTimer->timerId();
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
Revision 4.48  2009/03/30 18:26:20  mast
Call function to raise on mouse press if needed

Revision 4.47  2009/02/25 05:34:26  mast
Consume mouse move events to get to current one

Revision 4.46  2009/01/15 16:33:18  mast
Qt 4 port

Revision 4.45  2008/12/15 21:28:00  mast
Changes to call for switching buffering

Revision 4.44  2008/12/01 15:42:01  mast
Changes for undo/redo and selection in 3dmodv standalone

Revision 4.43  2008/11/28 06:44:00  mast
Added hot key to toggle current point

Revision 4.42  2008/08/19 20:01:20  mast
Made it treat keypad + like =

Revision 4.41  2008/06/10 05:54:57  mast
Changes for setting flag to draw light vector

Revision 4.40  2008/05/29 22:18:47  mast
Made it process hits from extra objects and contourless mesh properly

Revision 4.39  2008/05/28 05:57:58  mast
Prevent pick on extra object from doing bad things

Revision 4.38  2008/05/27 05:49:34  mast
Added hot keys for axis views, rearranged some keys

Revision 4.37  2008/05/22 15:42:57  mast
Changed for extra object editability

Revision 4.36  2008/04/29 18:11:51  xiongq
add isosurface dialog

Revision 4.35  2008/03/04 23:55:47  mast
Made point pick debug available on a key letter

Revision 4.34  2008/02/03 18:42:46  mast
Added parameterized mouse buttons and shift-right rotation

Revision 4.33  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 4.32  2008/01/21 17:47:40  mast
Added include for new listobj module

Revision 4.31  2007/11/30 06:51:50  mast
Changes for linking slicer to model view

Revision 4.30  2007/11/10 04:07:10  mast
Changes for setting snapshot directory

Revision 4.29  2007/09/23 15:16:00  mast
Cast args of atn2 to double

Revision 4.28  2007/09/20 22:06:55  mast
Changes for visualizing clipping plane

Revision 4.27  2007/08/08 03:05:02  mast
Fixed setting up of point picking to avoid context-setting errors

Revision 4.26  2007/07/08 16:53:19  mast
Added drag selection of contours crossed by mouse

Revision 4.25  2007/06/18 16:16:06  mast
Fixed stupid bug that ruined rotations

Revision 4.24  2007/06/13 15:19:21  mast
Made function for computing matrix from angle stpes

Revision 4.23  2007/04/06 22:21:04  mast
Chnaged scaling in call to move light

Revision 4.22  2006/10/05 15:41:32  mast
Provided for primary and second non-TIFF snapshot format

Revision 4.21  2006/09/12 15:47:19  mast
Handled contour member renames

Revision 4.20  2006/09/01 20:49:03  mast
Left a debugging statement in

Revision 4.19  2006/08/31 23:27:44  mast
Changes for stored value display

Revision 4.18  2005/10/21 23:58:41  mast
Fixed for gcc 4.0 on Mac

Revision 4.17  2004/11/21 06:07:49  mast
Changes for undo/redo

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
