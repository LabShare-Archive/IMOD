/*
 *  mv_input.cpp -- Keyboard and mouse input handlers for imodv.
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
#include "mv_window.h"
#include <math.h>
#include "imodv.h"
#include "imod.h"
#include "imod_edit.h"
#include "info_cb.h"
#include "display.h"
#include "b3dgfx.h"
#include "imod_input.h"
#include "preferences.h"
#include "control.h"
#include "undoredo.h"
#include "vertexbuffer.h"
#include "mv_menu.h"
#include "mv_ogl.h"
#include "mv_gfx.h"
#include "mv_input.h"
#include "mv_control.h"
#include "mv_light.h"
#include "mv_stereo.h"
#include "mv_depthcue.h"
#include "mv_views.h"
#include "mv_modeled.h"
#include "mv_image.h"
#include "isosurface.h"
#include "mv_objed.h"
#include "mv_listobj.h"
#include "mv_movie.h"
#include "dia_qtutils.h"


static void imodv_light_move(ImodvApp *a, int mx, int my);
static void imodvTranslateByDelta(ImodvApp *a, int x, int y, int z);
static void imodvSelect(ImodvApp *a, int mx, int my, bool moving, bool insert, 
                        bool curObj);
static void imodv_compute_rotation(ImodvApp *a, float x, float y, float z);
static void imodv_rotate(ImodvApp *a, int mx, int my, int throwFlag,
                         int rightWasDown);
static int  imodvStepTime(ImodvApp *a, int tstep);
static void processHits (ImodvApp *a, GLint hits, GLuint buffer[], 
                         bool moving, bool insert, bool curObj);
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
  Iobj *obj;
  int keysym = event->key();
  int tstep = 1;
  int fastdraw, ip;
  float elapsed;
  int state = event->modifiers();
  int keypad = event->modifiers() & Qt::KeypadModifier;
  int shifted = state & Qt::ShiftModifier;
  int ctrl = state & Qt::ControlModifier;
  QString qstr, qstr2;

  if (inputTestMetaKey(event))
    return;

  if (utilCloseKey(event) || keysym == Qt::Key_Q) {
    a->mainWin->close();
    return;
  }

  inputConvertNumLock(keysym, keypad);

  // Increase step size for shift, except not when moving a point
  if (shifted && !ctrl)
    tstep = 10;
  if (shifted && ctrl && (keysym == Qt::Key_PageDown || keysym == Qt::Key_PageUp)) {
    tstep = (int)(0.5 * B3DMIN(a->winx, a->winy) / a->imod->view->rad);
    tstep = B3DMAX(1, tstep);
  }

  if (!Imodv->imod)
    return;

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
    imodvControlChangeSteps(a, -1);
    break;

  case Qt::Key_Period:
    imodvControlChangeSteps(a, 1);
    break;

  case Qt::Key_M:
    if (shifted){
      imodvModelEditDialog(Imodv, 1);
    }else{
      mvMovieDialog(Imodv, 1);
    }
    break;

  case Qt::Key_N:
    if (shifted)
      mvMovieSequenceDialog(Imodv, 1);
    break;

  case Qt::Key_V:
    if (shifted && ctrl && a->vertBufOK >= 0) {
      a->vertBufOK = 1 - a->vertBufOK;
      imodPrintStderr("Vertex buffers %s\n", a->vertBufOK ? "ON" : "OFF");
      if (!a->vertBufOK) {
        for (int m = 0; m < a->numMods; m++)
          vbCleanupVBD(a->mod[m]);
      }
      imodvDraw(a);
    } else if (shifted)
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
    imodvSelectModel(a, a->curMod - 1);
    break;
               
  case Qt::Key_0:
    imodvSelectModel(a, a->curMod + 1);
    break;

  case Qt::Key_PageDown:
    if (keypad)
      imodv_rotate_model(a,0, 0, -a->deltaRot);
    else
      imodvTranslateByDelta(a, 0, 0, tstep);
    break;
  case Qt::Key_PageUp:
    if (keypad)
      imodv_rotate_model(a,0, 0, a->deltaRot);
    else
      imodvTranslateByDelta(a, 0, 0, -tstep);
    break;
  case Qt::Key_Up:
    if (keypad)
      imodv_rotate_model(a,-a->deltaRot, 0, 0);
    else
      imodvTranslateByDelta(a, 0, -tstep, 0);
    break;
  case Qt::Key_Down:
    if (keypad)
      imodv_rotate_model(a,a->deltaRot, 0, 0);
    else
      imodvTranslateByDelta(a, 0, tstep, 0);
    break;
  case Qt::Key_Right:
    if (keypad)
      imodv_rotate_model(a,0, a->deltaRot, 0);
    else
      imodvTranslateByDelta(a, -tstep, 0, 0);
    break;
  case Qt::Key_Left:
    if (keypad)
      imodv_rotate_model(a,0, -a->deltaRot, 0);
    else
      imodvTranslateByDelta(a, tstep, 0, 0);
    break;
  case Qt::Key_5:
  case Qt::Key_Enter:
  case Qt::Key_U:
    if (shifted && keysym == Qt::Key_U) {
      if (imodvByteImagesExist())
        imodvIsosurfaceEditDialog(Imodv, 1);
      break;
    }
    if (!keypad && keysym != Qt::Key_U)
      break;
    if (ctrl && keysym == Qt::Key_U) {
      imodv_rotate_model(a, 0, -a->deltaRot, 0);
      break;
    }
    imodvControlStart();
    break;
  case Qt::Key_7:
    imodv_rotate_model(a, -a->deltaRot, 0, 0);
    break;
  case Qt::Key_J:
    imodv_rotate_model(a, a->deltaRot, 0, 0);
    break;
  case Qt::Key_Y:
    if (ctrl) 
      inputUndoRedo(a->vi, 1);
    else
      imodv_rotate_model(a, 0, -a->deltaRot, 0);
    break;
  case Qt::Key_I:
    if (shifted) {
      if (imodvByteImagesExist())
        mvImageEditDialog(Imodv, 1);
    } else
      imodv_rotate_model(a, 0, a->deltaRot, 0);
    break;
  case Qt::Key_6:
    imodv_rotate_model(a, 0, 0, -a->deltaRot);
    break;
  case Qt::Key_H:
    imodv_rotate_model(a, 0, 0, a->deltaRot);
    break;

  case Qt::Key_Delete:
    obj = imodObjectGet(a->imod);
    if (!keypad && shifted && ctrl && !a->standalone && obj && imodPointGet(a->imod)) {
      if (!iobjScat(obj->flags)) {
        wprint("\aObject type must be scattered points to delete points in Model View\n");
        break;
      }
      if (a->imod->mousemode != IMOD_MMODEL) {
        wprint("\aYou must be in Model Mode to delete points in Model View\n");
        break;
      }
      inputDeletePoint(a->vi);
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
      mvImageUpdate(a);
      imodvStereoUpdate();
      imodvDraw(a);
    }
    break;

  case Qt::Key_R:
    if (ctrl) {
      imodvViewMenu(VVIEW_MENU_LOWRES);
      imodvMenuLowres(a->lowres);
    } else if (shifted)
      a->mainWin->openRotationTool(a);
    else
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

      // This routine removes VBD's of all objects cont's removed from
      inputDeleteContour(a->vi);
      pickedContour = -1;
    } else if (!shifted) {
      imodv_setbuffer(a, 1 - a->dblBuf, -1, -1);
      a->mainWin->setEnabledMenuItem(VVIEW_MENU_TRANSBKGD, a->dblBuf && !a->transBkgd &&
                                     (a->enableDepthDBal >= 0 ||
                                      a->enableDepthDBstAl >= 0));
      imodvDraw(Imodv);
    }
    break;

  case Qt::Key_F1:
               
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
  int shift = event->modifiers() & Qt::ShiftModifier;
  int ctrl = event->modifiers() & Qt::ControlModifier;

  // Use state after in press and release to keep track of mouse state
  leftDown = event->buttons() & ImodPrefs->actualModvButton(1);
  midDown = event->buttons() & ImodPrefs->actualModvButton(2);
  rightDown = event->buttons() & ImodPrefs->actualModvButton(3);
  utilRaiseIfNeeded(a->mainWin, event);

  // Set flag for any operation that needs to do something only on first move
  firstMove = 1;

  if (event->button() == ImodPrefs->actualModvButton(1)) {
    leftDown = ImodPrefs->actualModvButton(1);
    a->lastmx = event->x();
    a->lastmy = event->y();
    b2x = -10;
    b2y = -10;
    /* DNM: why draw here? */
    /* imodvDraw(a); */

  } else if (event->button() == ImodPrefs->actualModvButton(2) ||
             (event->button() == ImodPrefs->actualModvButton(3) && shift && !ctrl)) {
    b2x = a->lastmx = event->x();
    b2y = a->lastmy = event->y();
    if (event->button() == ImodPrefs->actualModvButton(2) && shift && !ctrl) {
      a->drawLight = 1;
      imodvDraw(a);
    }
    if (event->button() == ImodPrefs->actualModvButton(2) && shift && ctrl)
      imodvSelect(a, event->x(), event->y(), false, true, false);

  } else if (event->button() == ImodPrefs->actualModvButton(3)) {
    imodvSelect(a, event->x(), event->y(), false, false, (shift && ctrl));
  }
}

// Mouse release: check for throwing
void imodvMouseRelease(QMouseEvent *event)
{
  ImodvApp *a = Imodv;
  int rightWasDown = event->button() & ImodPrefs->actualModvButton(3);
  int shift = event->modifiers() & Qt::ShiftModifier;
  int ctrl = event->modifiers() & Qt::ControlModifier;

  leftDown = event->buttons() & ImodPrefs->actualModvButton(1);
  midDown = event->buttons() & ImodPrefs->actualModvButton(2);
  rightDown = event->buttons() & ImodPrefs->actualModvButton(3);
  if (((event->button() & ImodPrefs->actualModvButton(2)) && !shift && !ctrl) || 
      (rightWasDown && shift))
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
  static int ex, ey, shift, ctrl;
  static bool processing = false;
  ex = event->x();
  ey = event->y();
  shift = event->modifiers() & Qt::ShiftModifier;
  ctrl = event->modifiers() & Qt::ControlModifier;

  // Use state in mouse move to keep track of button down
  leftDown = event->buttons() & ImodPrefs->actualModvButton(1);
  midDown = event->buttons() & ImodPrefs->actualModvButton(2);
  rightDown = event->buttons() & ImodPrefs->actualModvButton(3);
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

    imodvTranslateByDelta(a, -(ex - a->lastmx), ey - a->lastmy, 0);
  }
  if (midDown && shift && !ctrl)
    imodv_light_move(a, ex, ey);
  else if ((midDown && !shift) || (rightDown && shift))
    imodv_rotate(a, ex, ey, 0, rightDown);
  else if (rightDown && ctrl)
    imodvSelect(a, ex, ey, true, false, false);
  a->lastmx = ex;
  a->lastmy = ey;
  if (imodDebug('m'))
    imodPuts(" ");
}

// A mouse wheel event either zooms or scales a scattered point size
void imodvScrollWheel(QWheelEvent *e)
{
  double power = -e->delta() / 120.;
  double zoom = pow(1.05, power);
  float scrnScale, size;
  int ob, co, pt;
  Imod *imod = Imodv->imod;
  if ((e->modifiers() & Qt::ShiftModifier) && (e->modifiers() & Qt::ControlModifier)) {
    imodGetIndex(Imodv->imod, &ob, &co, &pt);
    if (pt < 0 || !iobjScat(imod->obj[ob].flags))
      return;
    size = imodPointGetSize(&imod->obj[ob], &imod->obj[ob].cont[co], pt);
    scrnScale = 0.5 * B3DMIN(Imodv->winx, Imodv->winy) / Imodv->imod->view->rad;
    size += e->delta() * utilWheelToPointSizeScaling(scrnScale);
    size = B3DMAX(0., size);
    Imodv->vi->undo->contourDataChg();
    imodPointSetSize(&imod->obj[ob].cont[co], pt, size);
    imodvFinishChgUnit();
    imodvDrawImodImages(false);
    imodvDraw(Imodv);
  } else {
    imodv_zoomd(Imodv, zoom);
    imodvDraw(Imodv);
  }
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
    light_moveby(a->imod->view, mx - a->lastmx, my - a->lastmy);
  }
  imodvDraw(a);
}


// Change zoom by a factor
void imodv_zoomd(ImodvApp *a, double zoom)
{
  int m;
  if (!a->imod) return;

  if (a->crosset){
    for(m = 0; m < a->numMods; m++)
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
      imodvRegisterObjectChg(a->objNum);
    }
    imodvFinishChgUnit();
    firstMove = 0;
  }
}

/*
 * Translate model or clipping plane or current point
 */
static void imodvTranslateByDelta(ImodvApp *a, int x, int y, int z)
{
  int mx, my, ip, ipst, ipnd, ob, co, pt;
  unsigned int maskr = imodv_query_pointer(a,&mx,&my);
  bool ctrl = (maskr & Qt::ControlModifier) != 0;
  bool shift = (maskr & Qt::ShiftModifier) != 0;
  IclipPlanes *clips;
    
  Imod *imod;
  Imat *mat = a->mat;
  Ipoint ipt, opt, spt;
  int m, mstrt, mend;
  double alpha, beta;

  if (ctrl || !a->moveall) {
    mstrt = a->curMod;
    mend = mstrt + 1;
  } else {
    mstrt = 0;
    mend = a->numMods;
  }

  /* DNM: changed to compute shift properly for each model, to take account
     of actual scale to window, and to shift by mouse move amount */

  for (m = mstrt; m < mend; m++) {
    imod = a->mod[m];
    imodvRotScaleMatrix(a, mat, imod);
    
    ipt.x = x;
    ipt.y = y;
    ipt.z = z;
    imodMatTransform(mat, &ipt, &opt);
    
    opt.x *= (1.0/ imod->view->scale.x);
    opt.y *= (1.0/ imod->view->scale.y);
    opt.z *= (1.0/ imod->view->scale.z);
    
    if (ctrl && !shift) {
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
    } else if (ctrl && shift) {
      imodGetIndex(imod, &ob, &co, &pt);
      if (pt < 0 || !iobjScat(imod->obj[ob].flags))
        return;
      if (firstMove) {
        a->vi->undo->contourDataChg();
        imodvFinishChgUnit();
        firstMove = 0;
      }
      imod->obj[ob].cont[co].pts[pt].x -= opt.x;
      imod->obj[ob].cont[co].pts[pt].y -= opt.y;
      imod->obj[ob].cont[co].pts[pt].z -= opt.z;
      imodvDrawImodImages();
    } else { 
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
      a->xrotMovie = x;
      a->yrotMovie = y;
      a->zrotMovie = z;
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

  if (!(maskr & Qt::ControlModifier) || a->movie){

    /* Regular rotation of one or all models */

    if (!a->moveall) {
      mstrt = a->curMod;
      mend = mstrt + 1;
    } else {
      mstrt = 0;
      mend = a->numMods;
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

/* Compute the current rotation and scaling matrix to apply to a screen shift and get
   a model */
void imodvRotScaleMatrix(ImodvApp *a, Imat *mat, Imod *imod)
{
  float scrnscale;
  Ipoint spt;
  imodMatId(mat);
  imodMatRot(mat, -(double)imod->view->rot.x, b3dX);
  imodMatRot(mat, -(double)imod->view->rot.y, b3dY);
  imodMatRot(mat, -(double)imod->view->rot.z, b3dZ);

  scrnscale = 0.5 * B3DMIN(a->winx, a->winy) / imod->view->rad;
    
  spt.x = 1.0f/scrnscale;
  spt.y = 1.0f/scrnscale;
  spt.z = 1.0f/scrnscale * 1.0f/imod->zscale;
  if (imod->view->world & VIEW_WORLD_INVERT_Z)
      spt.z = -spt.z;
  imodMatScale(mat, &spt);
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
        a->xrotMovie = a->yrotMovie = a->zrotMovie = 0;
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
      a->xrotMovie = idx;
      a->yrotMovie = idy;
      a->zrotMovie = idz;

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
  a->xrotMovie = a->yrotMovie = a->zrotMovie = 0;
  a->movie = 0;

  if (midDown) {

    /* Get the total x and y movement.  The scale factor will roll the surface
       of a sphere 0.8 times the size of window's smaller dimension at the 
       same rate as the mouse */
    dx = (mx - a->lastmx);
    dy = (my - a->lastmy);
    angleScale = 1800. / (3.142 * 0.4 * B3DMIN(a->winx, a->winy));
    idx = B3DNINT(angleScale * dy);
    idy = B3DNINT(angleScale * dx);
  } else {
    idz = B3DNINT( 10. * utilMouseZaxisRotation(a->winx, mx, a->lastmx, 
                                                a->winy, my, a->lastmy));
  }
  if ((!idx) && (!idy) && !idz)
    return;
     
  if (imodDebug('m'))
    imodPrintStderr("mx,y %d %d  lmx,y %d %d",  mx, my, a->lastmx, a->lastmy);
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
// PICKING CODE
/* How picking works:
   1) Mouse hit occurs, it calls imodvSelect
   2) imodvSelect sets the pick flag and calls for a draw
   3) In the paint routine, it sets the render context to selection mode
   4) Model name is loaded by imodvDraw_models
   5) Object name is loaded by imodvDraw_model
   6) imodvDraw_object draws all spheres via imodvDraw_spheres, which always
   loads names for each point
   7) imodvDraw_object then calls imodvPick_Contours if there is picking
   8) imodvPick_Contours checks for mesh drawing with a POLYNORM2 and draws
   either all mesh triangles (normal mesh) or all vertex points (isosurface 
   mesh, assumed to be denser and not need triangles.  The "contour" name is
   the mesh index + contsize + 1; the "point" name is the vertex index.
   Otherwise it draws contours with contour and point indexes for names.
   9) imodvSelect restores the render mode and calls processHits 
   10) processHits finds the hit with the highest Z.  For an extra object with
   contours, it sets the current image point.  Otherwise there is an object,
   contour and point index that could be used to set a current model point.
   But there is a high contour index it then looks up the mesh vertex and 
   finds the corresponding contour point.  If this fails it looks for the 
   nearest contour point.  Then it sets the new current point, adding to a 
   selection list if appropriate.

*/
#define SELECT_BUFSIZE 40960

// For select mode, set up for picking then call draw routine
static void imodvSelect(ImodvApp *a, int x, int y, bool moving, bool insert, bool curObj)
{

  // 5/29/08: This was static, but why?  It stays in scope while needed.
  GLuint buf[SELECT_BUFSIZE];
  GLint hits;
  Iobj *obj = imodObjectGet(a->imod);
  //QTime picktime;
  //picktime.start();

  if (insert) {
    if (a->standalone)
      return;
    if (!iobjScat(obj->flags)) {
      wprint("\aObject type must be scattered points to add points in Model View\n");
      return;
    }
    if (a->imod->mousemode != IMOD_MMODEL) {
      wprint("\aYou must be in Model Mode to add points in Model View\n");
      return;
    }
  }      

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
  processHits(a, hits, buf, moving, insert, curObj);
  //imodPrintStderr("Pick time %d\n", picktime.elapsed());
}

// Analyze the hits from selection mode
static void processHits (ImodvApp *a, GLint hits, GLuint buffer[], bool moving, 
                         bool insert, bool curObj)
{
  unsigned int i, j;
  GLuint names, *ptr, *ptrstr;
  unsigned int z1, z2, zav, zmin;
  int tmo, tob, tco, tpt;
  int mo, ob, co, pt, minco, cosave, obsave, minpt;
  Iindex indSave;
  Ipoint pickpt;
  Ipoint *pts;
  Iobj *obj;
  float minsq, dsqr, dx, dy, dz;

  if (!hits) 
    return;

  /* If it overflowed, process what's there */
  if (hits < 0) 
    hits = SELECT_BUFSIZE/3; 

  imodGetIndex(a->imod, &ob, &co, &pt);
  obsave = ob;
  cosave = co;

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
    if (names > 3 && (pt == -1 || zav <= zmin) && (!curObj || tob == obsave)) { 
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
  if (a->curMod != mo)
    imodvSelectModel(a, mo);

  if (ob >= 0)
    obj = &a->imod->obj[ob];
  else {
    obj = ivwGetAnExtraObject(a->vi, -1 - ob);
    if (!obj || a->standalone)
      return;
    
    // For an extra object with contours, just set mouse from point position
    if (obj->contsize) {
      if (co < obj->contsize) {
        a->vi->xmouse = obj->cont[co].pts[pt].x;
        a->vi->ymouse = obj->cont[co].pts[pt].y;
        a->vi->zmouse = obj->cont[co].pts[pt].z;
        ivwBindMouse(a->vi);
        if (imodDebug('p'))
          imodPrintStderr ("Extra object, point at %.1f %.1f %.1f   inserting %d\n", 
                           a->vi->xmouse, a->vi->ymouse, a->vi->zmouse, insert?1:0);
        if (insert)
          inputInsertPoint(a->vi);
        else {

          // Detach from current model point in model mode so point shows up as cross
          if (a->imod->mousemode == IMOD_MMODEL)
            imodSetIndex(a->imod, obsave, cosave, -1);
          imodDraw(a->vi, IMOD_DRAW_XYZ);
        }
      }
      return;
    }
  }

  // If there is mesh drawing, get the position in the mesh
  indSave = a->imod->cindex;
  if (co >= obj->contsize) {
    co -= 1 + obj->contsize;
    
    if (!iobjMesh(obj->flags) || co < 0 || co >= obj->meshsize || 
        pt >= obj->mesh[co].vsize)
      return;
    pickpt = obj->mesh[co].vert[pt];
    if (!obj->contsize || insert) {
      if (a->standalone)
        return;
      a->vi->xmouse = pickpt.x;
      a->vi->ymouse = pickpt.y;
      a->vi->zmouse = pickpt.z;
      ivwBindMouse(a->vi);
      if (imodDebug('p'))
        imodPrintStderr ("Contourless mesh, point at %.1f %.1f %.1f  inserting %d\n", 
                         a->vi->xmouse, a->vi->ymouse, a->vi->zmouse, insert?1:0);
      if (insert)
        inputInsertPoint(a->vi);
      else {
        if (a->imod->mousemode == IMOD_MMODEL) {

          // for a real object, set the object index and update the info window
          if (ob >= 0) {
            imodSetIndex(a->imod, ob, -1, -1);
            tpt = ilistSize(a->vi->selectionList);
            if (!moving || imodSelectionListQuery(a->vi, ob, -1) < -1)
              imodSelectionNewCurPoint(a->vi, a->imod, indSave, ctrlDown);
            if (!a->standalone) {
              imod_setxyzmouse();
              if (ilistSize(a->vi->selectionList) > 1 && 
                  ilistSize(a->vi->selectionList) != tpt) {
                wprint("Selected objs:");
                for (tob = 0; tob < a->imod->objsize; tob++)
                  if (imodSelectionListQuery(a->vi, tob, -1) > -2)
                    wprint(" %d", tob + 1);
                wprint("\n");
              }
            }
          } else
            imodSetIndex(a->imod, obsave, cosave, -1);
        }
        imodDraw(a->vi, IMOD_DRAW_XYZ);
      }
      return;
    }

    // Search contours for the point, first an exact hit
    for (co = 0; co < obj->contsize; co++) {
      pts = obj->cont[co].pts;
      for (pt = 0; pt < obj->cont[co].psize; pt++)
        if (pts[pt].x == pickpt.x && pts[pt].y == pickpt.y && 
            pts[pt].z == pickpt.z)
          break;
      if (pt < obj->cont[co].psize)
        break;
    }
    if (co < obj->contsize) {
      if (imodDebug('p'))
        imodPrintStderr ("Mesh hit, exact match\n");
    } else {
      
      // Now look for closest point
      minsq = 1.e30;
      for (co = 0; co < obj->contsize; co++) {
        pts = obj->cont[co].pts;
        for (pt = 0; pt < obj->cont[co].psize; pt++) {
          dx = pts[pt].x - pickpt.x;
          dx *= dx;
          if (dx < minsq) {
            dy = pts[pt].y - pickpt.y;
            dy *= dy;
            if (dy < minsq) {
              dz = pts[pt].z - pickpt.z;
              dz *= dz;
              if (dz < minsq) {
                dsqr = dx + dy + dz;
                if (dsqr < minsq) {
                  minco = co;
                  minpt = pt;
                  minsq = dsqr;
                }
              }
            }
          }
        }
      }
      if (minsq > 1.e20)
        return;
      co = minco;
      pt = minpt;
      if (imodDebug('p'))
        imodPrintStderr ("Mesh hit, nearest point distance %.3f\n", 
                         sqrt((double)minsq));
    }
  } else
    pickpt = a->imod->obj[ob].cont[co].pts[pt];
  
  // If inserting is requesting, always insert a point no matter how it was gotten
  if (insert) {
    a->vi->xmouse = pickpt.x;
    a->vi->ymouse = pickpt.y;
    a->vi->zmouse = pickpt.z;
    ivwBindMouse(a->vi);
    if (imodDebug('p'))
      imodPrintStderr("Inserting  %.1f %.1f %.1f\n", a->vi->xmouse, a->vi->ymouse, 
                      a->vi->zmouse);
    inputInsertPoint(a->vi);
    return;
  }

  // Now process the indexable point whether from contour or mesh
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
      (a->xrotMovie || a->yrotMovie || a->zrotMovie)) {
    a->movieFrames++;
    a->movieCurrent = imodv_sys_time();
    index = a->movieFrames % MAX_MOVIE_TIMES;
    nframes = a->movieFrames < MAX_MOVIE_TIMES ? 
      a->movieFrames : MAX_MOVIE_TIMES;
    rot = (float)sqrt((double)(a->xrotMovie * a->xrotMovie + 
                               a->yrotMovie * a->yrotMovie + 
                               a->zrotMovie * a->zrotMovie));
    scale = a->movieSpeed * a->throwFactor * 
      (a->movieCurrent - a->movieTimes[index]) / (100. * nframes * rot);
    a->movieTimes[index] = a->movieCurrent;
    imodv_compute_rotation(a, scale * a->xrotMovie, scale * a->yrotMovie, 
                           scale * a->zrotMovie);
    imodvDraw(a);
  } else {
    a->wpid = 0;
    a->mainWin->mTimer->stop();
  }
}
