/*
 *  imod_input.cpp -- Handles general mouse/keyboard input.
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

#include <math.h>
#include <qapplication.h>
#include <qwidget.h>
#include <qevent.h>
#include <qnamespace.h>
//Added by qt3to4:
#include <QKeyEvent>
#include "dia_qtutils.h"
#include "xgraph.h"
#include "imod.h"
#include "imodplug.h"
#include "autox.h"
#include "display.h"
#include "workprocs.h"
#include "xcramp.h"
#include "xzap.h"
#include "imod_edit.h"
#include "info_setup.h"
#include "imod_input.h"
#include "imod_io.h"
#include "info_cb.h"
#include "cont_edit.h"
#include "cont_copy.h"
#include "imodv.h"
#include "mv_gfx.h"
#include "mv_objed.h"
#include "isosurface.h"
#include "mv_window.h"
#include "imod_input.h"
#include "control.h"
#include "sslice.h"
#include "finegrain.h"
#include "moviecon.h"
#include "model_edit.h"
#include "object_edit.h"
#include "preferences.h"
#include "undoredo.h"
#include "vertexbuffer.h"

void inputRaiseWindows()
{
  /* New way, allows type of windows to be raised in chosen order */
  if (!ImodvClosed && Imodv->mainWin->isVisible())
    Imodv->mainWin->raise();

  imodDialogManager.raise(IMOD_IMAGE);
  imodvDialogManager.raise(IMODV_DIALOG);
  imodDialogManager.raise(IMOD_DIALOG);
  if (ImodInfoWin->isVisible())
    ImodInfoWin->raise();
#ifdef _WIN32
  ImodInfoWin->activateWindow();
#endif
  
  /* The old way
  QWidgetList  *list = QApplication::topLevelWidgets();
  QWidgetListIt it( *list );  // iterate over the widgets
  QWidget * w;
  while ( (w=it.current()) != 0 ) {   // for each top level widget...
    ++it;
    if (w->isVisible() )
      w->raise();
  }
  delete list;                // delete the list, not the widgets
  */
}
     
/* old function still in use */
int mouse_in_box(int llx, int lly, int urx, int  ury, int mousex, int mousey)
{
     
  if ( (mousex >= llx ) && (mousey >= lly) && 
       (mousex <= urx ) && (mousey <= ury))
    return(1);
  else
    return(0);
}

/* DNM 3/11/03: This is used only by slicer.  Modified to behave like zap
   with respect to time matches */
void inputInsertPoint(ImodView *vi)
{
  Iobj *obj = imodObjectGet(vi->imod);
  Icont *cont;
  Ipoint point;
  int pt;

  /* create a new contour if there is no current contour */
  if (obj && vi->imod->mousemode == IMOD_MMODEL) {
    cont = ivwGetOrMakeContour(vi, obj, 0);
    if (!cont)
      return;

    /* Add point only if at current time */
    if (!ivwTimeMismatch(vi, 0, obj, cont)) {
      point.x = vi->xmouse;
      point.y = vi->ymouse;
      point.z = vi->zmouse;
      
      // Set insertion point to next point, or to current point if inserting
      // backwards
      pt = vi->imod->cindex.point + 1;
      if (pt > 0 && vi->insertmode)
        pt--;

      ivwRegisterInsertPoint(vi, cont, &point, pt);
    }
  }
  vi->undo->finishUnit();
  imodDraw(vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
}

/* 
 * Delete a single point, or whole contour
 */
void inputDeletePoint(ImodView *vi)
{
  Icont *cont = imodContourGet(vi->imod);
  Ipoint *pnt;

  // Must be in model mode, have contour and have a current point or be empty
  if (vi->imod->mousemode == IMOD_MMODEL && cont && 
      (!cont->psize || vi->imod->cindex.point >= 0)) {
    if (!cont->psize)
      vi->undo->contourRemoval();
    else
      vi->undo->pointRemoval();

    if (imodDeletePoint(vi->imod) < 0)
      vi->undo->flushUnit();
    else
      vi->undo->finishUnit();

    /* If inserting in reverse, advance to next point unless at start
       of contour */
    if (vi->insertmode && vi->imod->cindex.point > 0){
      cont = imodContourGet(vi->imod);
      if (cont){
        vi->imod->cindex.point++;
        if (vi->imod->cindex.point >= cont->psize)
          vi->imod->cindex.point = cont->psize - 1;
      }
    }

    // Set current point position to the new current point if any
    pnt = imodPointGet(vi->imod);
    if (pnt) {
      vi->xmouse = pnt->x;
      vi->ymouse = pnt->y;
      vi->zmouse = pnt->z;
    }
    imod_info_setxyz();
    imodDraw(vi, IMOD_DRAW_MOD | IMOD_DRAW_XYZ);
  }
  return;
}

void inputModifyPoint(ImodView *vi)
{
  Iobj *obj = imodObjectGet(vi->imod);
  Icont *cont = imodContourGet(vi->imod);
  Ipoint *point = imodPointGet(vi->imod);
     
  if (point && cont && obj && !ivwTimeMismatch(vi, 0, obj, cont) &&
      vi->imod->mousemode == IMOD_MMODEL) {

    
    /* DNM: if z value is changing, need to set contour's wild flag */
    if (point->z != vi->zmouse) {
      if (!(cont->flags & ICONT_WILD))
        vi->undo->contourPropChg();
      cont->flags |= ICONT_WILD;
    }
    vi->undo->pointShift();
    point->x = vi->xmouse;
    point->y = vi->ymouse;
    point->z = vi->zmouse;
    vi->undo->finishUnit();
  }
  imodDraw(vi, IMOD_DRAW_MOD);
}

/* DNM 7/29/03: make correct nearest int tests for zmouse to avoid going 
   outside legal limits */
void inputNextz(ImodView *vi, int step)
{
  if (B3DNINT(vi->zmouse) < (vi->zsize - 1)){
    vi->zmouse = B3DMIN(vi->zmouse + step, vi->zsize - 1);
    imodDraw(vi, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC);
  }
  return;
}

void inputPrevz(ImodView *vi, int step)
{
  if (B3DNINT(vi->zmouse) > 0){
    vi->zmouse = B3DMAX(vi->zmouse - step, 0);
    imodDraw(vi, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC);
  }
  return;
}

/* DNM 7/29/03: treat Y just like X is now treated */
void inputNexty(ImodView *vi)
{
  if (vi->ymouse < (vi->ysize - 1)){
    vi->ymouse += 1.0;
    ivwBindMouse(vi);
    imodDraw(vi, IMOD_DRAW_XYZ);
  }
  return;
}

void inputPrevy(ImodView *vi)
{
  if (vi->ymouse > 0){
    vi->ymouse -= 1.0;
    ivwBindMouse(vi);
    imodDraw(vi, IMOD_DRAW_XYZ);
  }
  return;
}

/* DNM 2/13/03: add binding to limit mouse to possible values */
void inputNextx(ImodView *vi)
{
  if (vi->xmouse < (vi->xsize - 1)){
    vi->xmouse += 1.0;
    ivwBindMouse(vi);
    imodDraw(vi, IMOD_DRAW_XYZ);
  }
  return;
}

void inputPrevx(ImodView *vi)
{
  if (vi->xmouse > 0){
    vi->xmouse -= 1.0;
    ivwBindMouse(vi);
    imodDraw(vi, IMOD_DRAW_XYZ);
  }
  return;
}

/* DNM 6/16/01: To toggle ghost mode, save current mode if it is on; if it is
   off, transfer the next and previous section flags from the saved mode */
void inputGhostmode(ImodView *vi)
{
  if (vi->ghostmode & IMOD_GHOST_SECTION) {
    vi->ghostlast = vi->ghostmode;
    vi->ghostmode = vi->ghostmode & ~IMOD_GHOST_SECTION;
  } else
    vi->ghostmode |= (vi->ghostlast & IMOD_GHOST_SECTION);

  imodDraw(vi, IMOD_DRAW_MOD);
  return;
}

// Make a new contour
void inputNewContour(ImodView *vi)
{
  inputNewContourOrSurface(vi, INCOS_NEW_CONT, 0);
}

// Make a new contour on a new surface number
void inputNewSurface(ImodView *vi)
{
  inputNewContourOrSurface(vi, INCOS_NEW_SURF, 0);
}

/*
 * Make a new contour, optionally assigning it to a given or new surface. Set surface = -2
 * for no surface change, surface = -1 for new surface, surface >=0 to assign it
 */
void inputNewContourOrSurface(ImodView *vi, int surface, int timeLock)
{
  Iobj *obj = imodObjectGet(vi->imod);
  Icont *cont;
  if (!obj || vi->imod->mousemode == IMOD_MMOVIE)
    return;

  // Register an object property change if there is a new surface
  if (surface == INCOS_NEW_SURF)
    vi->undo->objectPropChg();
  vi->undo->contourAddition(obj->contsize);

  imodNewContour(vi->imod);
  cont = imodContourGet(vi->imod);
  if (surface == INCOS_NEW_SURF)
    imodel_contour_newsurf(obj, cont);
  if (surface >= 0)
    cont->surf = surface;

  ivwSetNewContourTime(vi, obj, cont);
  if (timeLock && cont->time)
    cont->time = timeLock;
  vi->undo->finishUnit();
  if (imodSelectionListClear(vi))
    imod_setxyzmouse();
  imod_info_setocp();
}

// Unused 11/16/04
void inputContourDup(ImodView *vi)
{
  Icont *cont, *ocont;
  if (vi->imod->mousemode == IMOD_MMOVIE){
    wprint("\nMust be in model mode to edit model.\n");
    return;
  }
  cont = imodContourGet(vi->imod);
  if (!cont) return;
  ocont = imodContourDup(cont);
  imodNewContour(vi->imod);
  cont = imodContourGet(vi->imod);
  if (!cont) return;
  *cont = *ocont;
  free(ocont);
  imod_info_setocp();
}

void inputNextObject(ImodView *vi)
{
  imodNextObject(vi->imod);
  inputKeepContourAtSameTime(vi);

  /*  Drop tests for object in this and next function, setxyzmouse works OK 
      with no  object */
  imodSelectionListClear(vi);
  imod_setxyzmouse();
}

void inputPrevObject(ImodView *vi)
{
  imodPrevObject(vi->imod);
  inputKeepContourAtSameTime(vi);
  imodSelectionListClear(vi);
  imod_setxyzmouse();
}

// Finds first contour in object that matches the current time and sets it as
// current contour
void inputKeepContourAtSameTime(ImodView *vi)
{
  Iobj *obj;
  Icont *cont;
  int currentTime, co;
  int objIndex, contIndex, pointIndex;

  ivwGetTime(vi, &currentTime);

  obj  = imodObjectGet(vi->imod);
  if (!obj || !iobjFlagTime(obj)) 
    return;

  cont = imodContourGet(vi->imod);
  if (cont) {
    imodGetIndex(vi->imod, &objIndex, &contIndex, &pointIndex);

    if (cont->time != currentTime){
      for(co = 0; co < obj->contsize; co++){
	if (obj->cont[co].time ==  currentTime){
	  imodSetIndex (vi->imod, objIndex, co, pointIndex);
	  break;
        }
      }
    }
  }
}

// Goes to adjacent available surfave in the given direction
void inputAdjacentSurface(ImodView *vi, int direction)
{
  Iobj *obj;
  int co, newsurf, newcont, cursurf;

  /* if no object selected, forget it. */
  if (vi->imod->cindex.object < 0)
    return;

  /* Selected Object. */
  obj = &(vi->imod->obj[vi->imod->cindex.object]);

  /* If object has no contours, or none is selected, return. */
  if (!obj->contsize)
    return;

  if (vi->imod->cindex.contour < 0)
    return;

  cursurf = obj->cont[vi->imod->cindex.contour].surf;
     
  if (direction > 0) {
    /* Find the first contour with lowest surface # greater than the 
       current surface # */
    newsurf = obj->surfsize + 1;
    for (co = 0; co < obj->contsize; co++)
      if (obj->cont[co].surf > cursurf && 
          obj->cont[co].surf < newsurf) {
        newsurf = obj->cont[co].surf;
        newcont = co;
      }
    if (newsurf == obj->surfsize + 1)
      return;

  } else {
    /* Or find the first contour with highest surface # less than the 
       current surface # */
    newsurf = -1;
    for (co = 0; co < obj->contsize; co++)
      if (obj->cont[co].surf < cursurf && 
          obj->cont[co].surf > newsurf) {
        newsurf = obj->cont[co].surf;
        newcont = co;
      }
    if (newsurf == -1)
      return;
  }

  vi->imod->cindex.contour = newcont;

  /* if point index is too high or low, change it. */
  if (vi->imod->cindex.point >= obj->cont[newcont].psize)
    vi->imod->cindex.point = obj->cont[newcont].psize - 1;
  if (vi->imod->cindex.point < 0 && obj->cont[newcont].psize > 0)
    vi->imod->cindex.point = 0;

  imod_setxyzmouse();
  return;
}

// Tries to go to the target surface number
void inputGotoSurface(ImodView *vi, int target)
{
  Icont *cont;
  Iobj  *obj;
  int distmin = 1000000;
  int co, closest, dist;
      
  obj = imodObjectGet(vi->imod);

  if (!obj || !obj->contsize)
    return;

  /* if target is next or previous surface, use the AdjacentSurface call */
  // But update the window in case contour didn't change
  cont = imodContourGet(vi->imod);
  if (cont)
    if (cont->surf == target + 1 || cont->surf == target - 1) {
      inputAdjacentSurface(vi, target - cont->surf);
      imodContEditSurfShow();
      return;
    }
     
  /* find the first contour with the closest surface number */
  for (co = 0; co < obj->contsize; co++) {
    dist = obj->cont[co].surf - target;
    if (dist < 0)
      dist = -dist;
    if (dist < distmin) {
      distmin = dist;
      closest = co;
      if (!dist)
        break;
    }
  }
     
  vi->imod->cindex.contour = closest;

  /* if point index is too high or low, change it. */
  if (vi->imod->cindex.point >= obj->cont[closest].psize)
    vi->imod->cindex.point = obj->cont[closest].psize - 1;
  if (vi->imod->cindex.point < 0 && obj->cont[closest].psize > 0)
    vi->imod->cindex.point = 0;

  imod_setxyzmouse();
  imodContEditSurfShow();
}

// Changes to adjacent contour in the current surface in the given direction
void inputAdjacentContInSurf(ImodView *vi, int direction)
{
  Iobj *obj;
  int co, newcont, cursurf;

  /* if no object selected, forget it. */
  if (vi->imod->cindex.object < 0)
    return;

  /* Selected Object. */
  obj = &(vi->imod->obj[vi->imod->cindex.object]);

  /* If object has no contours, or none is selected, return. */
  if (!obj->contsize)
    return;

  if (vi->imod->cindex.contour < 0)
    return;

  cursurf = obj->cont[vi->imod->cindex.contour].surf;
  newcont = -1;

  for (co = vi->imod->cindex.contour + direction; 
       co >= 0 && co < obj->contsize; co += direction)
    if (obj->cont[co].surf == cursurf) {
      newcont = co;
      break;
    }
  if (newcont == -1)
    return;

  vi->imod->cindex.contour = newcont;

  /* if point index is too high or low, change it. */
  if (vi->imod->cindex.point >= obj->cont[newcont].psize)
    vi->imod->cindex.point = obj->cont[newcont].psize - 1;
  if (vi->imod->cindex.point < 0 && obj->cont[newcont].psize > 0)
    vi->imod->cindex.point = 0;

  imod_setxyzmouse();
  return;
}



void inputNextContour(ImodView *vi)
{
  Iindex indOld = vi->imod->cindex;
  imodNextContour(vi->imod);
  inputRestorePointIndex(vi, &indOld);
  imod_setxyzmouse();
}

void inputPrevContour(ImodView *vi)
{
  Iindex indOld = vi->imod->cindex;
  imodPrevContour(vi->imod);
  inputRestorePointIndex(vi, &indOld);
  imod_setxyzmouse();
}

/* If the current point index is -1 and the new contour has points, set
   to starting point.  Otherwise, try to match Z if contour is not planar,
   or match the fractional position in the contour */
void inputRestorePointIndex(ImodView *vi, Iindex *oldInd)
{
  Icont *cont = imodContourGet(vi->imod);
  Icont *oldCont;
  Iobj *obj;
  int pt, oldz, indz, delz, mindelz = 10000000;
  float dist, mindist = 1.e30;

  if (cont && cont->psize) {
    if (vi->imod->cindex.point == -1)
      vi->imod->cindex.point = 0;
    else if (oldInd && oldInd->point >= 0 && oldInd->object >= 0 && 
             oldInd->object < vi->imod->objsize)  {
      obj = &vi->imod->obj[oldInd->object];
      if (oldInd->contour >= 0 && oldInd->contour < obj->contsize &&
          oldInd->point < obj->cont[oldInd->contour].psize) {
      
        // Find the nearest point in X/Y on the closest Z plane 
        // in X/Y overall
        oldCont = &obj->cont[oldInd->contour];
        oldz = B3DNINT(oldCont->pts[oldInd->point].z);
        for (pt = 0; pt < cont->psize; pt++) {
          dist = imodPointDistance(&oldCont->pts[oldInd->point], &cont->pts[pt]);
          delz = B3DNINT(cont->pts[pt].z) - oldz;
          if (delz < 0)
            delz = -delz;
          if (delz < mindelz || (delz == mindelz && dist < mindist)) {
            mindelz = delz;
            mindist = dist;
            indz = pt;
          } 
        }
        vi->imod->cindex.point = indz;
      }
    }
  }
}

void inputNextPoint(ImodView *vi)
{
  imodNextPoint(vi->imod);
  imod_setxyzmouse();
  return;
}

/* DNM 3/29/01: make it go to first point if point undefined */
void inputPrevPoint(ImodView *vi)
{
  Icont *cont;
  if (imodPrevPoint(vi->imod) < 0) {
    cont = imodContourGet(vi->imod);
    if (cont)
      if (cont->psize)
        vi->imod->cindex.point = 0;
  }
  imod_setxyzmouse();
  return;
}

/* Set model index to a contour that exists
 * during the current time, if possible.
 */
void inputSetModelTime(ImodView *vi, int time)
{
  Iobj *obj = imodObjectGet(vi->imod);
  Icont *cont;
  Ipoint *point;
  int ob,co;

  int nco = -1; 
  int npt = -1;
     
  if (!obj) return;
  if (!iobjFlagTime(obj)) return;
     
  cont = imodContourGet(vi->imod);
  if (!cont) return;

  point = imodPointGet(vi->imod);
  if (!point){
    imodGetIndex(vi->imod,&ob,&nco,&npt);
    for(co = 0; co < obj->contsize; co++){
      if (obj->cont[co].time == time)
        nco = co;
    }
    imodSetIndex(vi->imod,ob,nco,npt);
  }else{

    imodGetIndex(vi->imod,&ob,&nco,&npt);
    for(co = 0; co < obj->contsize; co++){
      if (obj->cont[co].time == time)
        nco = co;
    }
    imodSetIndex(vi->imod,ob,nco,npt);
  }
}

void inputNextTime(ImodView *vi)
{
  int time, maxtime;

  maxtime = ivwGetTime(vi, &time);
  if (!maxtime) 
    return;
     
  time++;
  /*     inputSetModelTime(vi, time); */
  ivwSetTime(vi, time);
  slicerNewTime(false);
  imodDraw(vi, IMOD_DRAW_ALL);
}

void inputMovieTime(ImodView *vi, int val)
{
  int time;
  int maxtime = ivwGetTime(vi, &time);
  if (!maxtime)
    return;
  imodMovieXYZT(vi, MOVIE_DEFAULT, MOVIE_DEFAULT, MOVIE_DEFAULT, val);
}

void inputPrevTime(ImodView *vi)
{
  int time, maxtime;
     
  maxtime = ivwGetTime(vi, &time);
  if (!maxtime)
    return;
     
  time--;
  /*     inputSetModelTime(vi, time); */
  ivwSetTime(vi, time);
  slicerNewTime(false);
  imodDraw(vi, IMOD_DRAW_ALL);
}

void inputLimitingTime(ImodView *vi, int dir)
{
  int time, start, end;
  if (!ivwGetTime(vi, &time))
    return;
  imcGetStartEnd(vi, 3, &start, &end);
  if (dir > 0)
    ivwSetTime(vi, end + 1);
  else
    ivwSetTime(vi, start + 1);
  slicerNewTime(false);
  imodDraw(vi, IMOD_DRAW_ALL);
}

void inputFirstPoint(ImodView *vi)
{
  Icont *cont = imodContourGet(vi->imod);
  if (!cont)
    return;
  if (!cont->psize)
    return;
  vi->imod->cindex.point = 0;
  imod_setxyzmouse();
}

void inputLastPoint(ImodView *vi)
{
  Icont *cont = imodContourGet(vi->imod);
  if (!cont)
    return;
  if (!cont->psize)
    return;
     
  vi->imod->cindex.point = cont->psize - 1;
  imod_setxyzmouse();
}

void inputMoveObject(ImodView *vi)
{
  imodContEditMove();
  imod_setxyzmouse();
}

void inputDeleteContour(ImodView *vi)
{
  /* DNM 3/29/01: have it make the previous contour be the current contour,
     but keep current point undefined to keep image from popping to that
     contour */
  Imod *imod = vi->imod;
  Iobj *obj;
  int conew = imod->cindex.contour -1;
  int obnew = imod->cindex.object;
  int numDel, i, ob;
  QString qstr;
  static int lastDel = 0;

  if (!ilistSize(vi->selectionList)) {
    if (!imodContourGet(imod))
      return;
    vi->undo->contourRemoval();
    imodDelCurrentContour(imod);

  } else {

    // Multiple selection: first confirm if > 2 to delete
    numDel = ilistSize(vi->selectionList);
    if (numDel > 2 && lastDel < 2) {
      qstr.sprintf("Are you sure you want to delete these %d contours?", 
                   numDel);
      lastDel = dia_ask_forever(LATIN1(qstr));
      if (!lastDel)
        return;
    } else
      wprint("%d contours deleted.\n", numDel);

    // Loop through objects, set each as current object
    vi->undo->getOpenUnit();
    for (ob = 0; ob < imod->objsize; ob++) {
      obj = &imod->obj[ob];
      imodSetIndex(imod, ob, -1, -1);

      // Loop backwards through object removing selected contours
      for (i = obj->contsize - 1; i >= 0; i--) {
        if (imodSelectionListQuery(vi, ob, i) > -2) {
          vbCleanupVBD(obj);
          vi->undo->contourRemoval(ob, i);
          imodDeleteContour(imod, i);
          if (ob == obnew)
            conew = i - 1;
        }
      }
    }
  }
  vi->undo->finishUnit();
    
  obj = imodObjectGet(imod);
  if (conew < 0 && obj->contsize > 0)
    conew = 0;
  imodSetIndex(imod, obnew, conew, -1);
  imodSelectionListClear(vi);
  if (vi->modelViewVi)
    imodvDraw(Imodv);
  else
    imod_setxyzmouse();
}

/* Truncate contour at the current point */
void inputTruncateContour(ImodView *vi)
{
  Icont *cont = imodContourGet(vi->imod);
  if (!cont || vi->imod->cindex.point < 0)
    return;
  vi->undo->contourDataChg();
  cont->psize = vi->imod->cindex.point + 1;
  vi->undo->finishUnit();
  imod_setxyzmouse();
}

void inputToggleGap(ImodView *vi)
{
  Icont *cont = imodContourGet(vi->imod);
  int wasGap;
  if (!cont || vi->imod->cindex.point < 0)
    return;
  wasGap = istorePointIsGap(cont->store, vi->imod->cindex.point);
  if (!ifgToggleGap(vi, cont, vi->imod->cindex.point, wasGap == 0))
    wprint("%s gap at current point\n", wasGap ? "Removed" : "Added");
  imodDraw(vi, IMOD_DRAW_MOD);
  fineGrainUpdate();
}

void inputFindValue(ImodView *vi)
{
  float pixval;
  /*

  if ((!vi->fp) | (!vi->hdr)){
  wprint("Error: Find Value can't read file.\n");
  return;
  }

  */
  pixval = ivwGetFileValue(vi, (int)vi->xmouse, (int)vi->ymouse, 
                           (int)vi->zmouse);
  wprint("Pixel (%g, %g, %g) = %g\n", 
         vi->xmouse + 1, vi->ymouse + 1, vi->zmouse + 1, pixval);

  imodDraw(vi, IMOD_DRAW_XYZ);
  return;
}

void inputPointMove(ImodView *vi, int x, int y, int z)
{
  Iobj *obj;
  Icont *cont;
  Ipoint *pt;

  if (vi->imod->cindex.point == -1)
    return;
  cont = imodContourGet(vi->imod);
  if (!cont)
    return;

  vi->undo->pointShift();
  pt = &(cont->pts[vi->imod->cindex.point]);
  if (x){
    if (x > 0){
      pt->x += 1.0f;
      if (pt->x > vi->xsize - 1)
        pt->x = vi->xsize - 1;
    }else{
      pt->x -= 1.0f;
      if (pt->x < 0.0f)
        pt->x = 0.0f;
    }
          
  }
  if (y){
    if (y > 0){
      pt->y += 1.0f;
      if (pt->y > vi->ysize - 1)
        pt->y = vi->ysize - 1;
    }else{
      pt->y -= 1.0f;
      if (pt->y < 0.0f)
        pt->y = 0.0f;
    }
          
  }
  if (z){
    if (cont->psize > 1) {
      /* DNM: since z is changing, need to set contour's wild flag */
      obj = imodObjectGet(vi->imod);
      if (iobjPlanar(obj->flags) && !(cont->flags & ICONT_WILD))
        wprint("\aContour is no longer in one Z plane. "
               "With this contour, you will not get a new"
               " contour automatically when you change Z.\n");
      vi->undo->contourPropChg();
      cont->flags |= ICONT_WILD;
    }
    if (z > 0){
      pt->z += 1.0f;
      vi->zmouse = pt->z;
      if (pt->z > vi->zsize - 1){
        pt->z = vi->zsize - 1;
        vi->zmouse = vi->zsize - 1;
      }
    }else{
      pt->z -= 1.0f;
      vi->zmouse = pt->z;
      if (pt->z < 0.0f){
        pt->z = 0.0f;
        vi->zmouse = 0;
      }
    }
  }
  vi->undo->finishUnit();
  imodDraw(vi, IMOD_DRAW_RETHINK | IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ);
  return;
}

// Move a point in response to a keypad key
void inputKeyPointMove(ImodView *vi, int keysym)
{
  if (keysym == Qt::Key_Left)
    inputPointMove(vi, -1, 0, 0);
  else if (keysym == Qt::Key_Right)
    inputPointMove(vi, 1, 0, 0);
  else if (keysym == Qt::Key_Down)
    inputPointMove(vi, 0, -1, 0);
  else if (keysym == Qt::Key_Up)
    inputPointMove(vi, 0, 1, 0);
  else if (keysym == Qt::Key_PageDown)
    inputPointMove(vi, 0, 0, -1);
  else if (keysym == Qt::Key_PageUp)
    inputPointMove(vi, 0, 0, 1);
}

/* Find the maximum value within 5 pixels of the current location */
/* DNM 1/31/03 fixed problem with mx, my being used for max and loop limits */
void inputFindMaxValue(ImodView *vi)
{
  int mx, my;
  int  x,  y, maxx, maxy;
  float maxpixval, pixval;

  maxpixval = -1.e30;
  mx = (int)(vi->xmouse - 5);
  my = (int)(vi->ymouse - 5);
  for (x = mx; x <= mx + 10; x++)
    for (y = my ; y <= my + 10; y++){
      pixval = ivwGetFileValue(vi, x, y, (int)vi->zmouse);
      if (pixval > maxpixval){
        maxpixval = pixval;
        maxx = x; maxy = y;
      }
    }
  vi->xmouse = maxx;
  vi->ymouse = maxy;

  wprint("Pixel %g %g %g = %g\n", 
         vi->xmouse + 1, vi->ymouse + 1, vi->zmouse + 1, maxpixval);

  imodDraw(vi, IMOD_DRAW_XYZ);
}

// Create a new object
void inputNewObject(ImodView *vi)
{
  Iobj *obj;
  NewObjectProps *props = ImodPrefs->newObjectProps();
  vi->undo->objectAddition(vi->imod->objsize);

  // Get a new object and set the default properties
  imodNewObject(vi->imod);
  obj = imodObjectGet(vi->imod);
  obj->flags = props->flags;
  obj->pdrawsize = props->pdrawsize;
  obj->symbol = props->symbol;
  obj->symsize = props->symsize;
  obj->linewidth2 = props->linewidth2;
  obj->symflags = props->symflags;
  obj->extra[IOBJ_EX_PNT_LIMIT] = props->pointLimit;
  obj->extra[IOBJ_EX_2D_TRANS] = props->fillTrans;
  vi->undo->finishUnit();

  /* DNM: need to find pixel value for new object, but no longer allocate */

  if (!App->rgba && App->depth <= 8)
    obj->fgcolor = App->objbase - vi->imod->cindex.object;
  else if (!App->rgba)
    obj->fgcolor = App->objbase + vi->imod->cindex.object;
     
  /* DNM 5/16/02: if multiple image files, set time flag by default */
  if (vi->numTimes)
    obj->flags |= IMOD_OBJFLAG_TIME;

  if (imodSelectionListClear(vi))
    imod_setxyzmouse();
  imod_info_setocp();
  imod_cmap(vi->imod);
  imodDraw(vi, IMOD_DRAW_MOD);
}

void inputSaveModel(ImodView *vi)
{
  if (ImodForbidLevel || App->cvi->doingInitialLoad)
    return;

  imod_info_forbid();
  vi->imod->blacklevel = vi->black;
  vi->imod->whitelevel = vi->white;

  /* DNM: the first FlipModel is unnecessary in normal cases and actually 
     messes up the saved model when there's a fakeimage; the second maybe
     restored the in-memory model in that case */
  /*     ivwFlipModel(vi); */
  SaveModel(vi->imod);
  /*        show_status("Error Saving Model.");   DNM: it already has messages
            else
            show_status("Done saving model."); */
  /*     ivwFlipModel(vi); */

  imod_info_enable();
  imod_draw_window();
}

// Do an undo or a redo
void inputUndoRedo(ImodView *vi, bool redo)
{
  int err;
  if (redo)
    err = vi->undo->redo();
  else
    err = vi->undo->undo();
  if (err == UndoRedo::NoneAvailable)
    wprint("\aThere is no further %s information available.\n", 
           redo ? "redo" : "undo");
  if (err == UndoRedo::StateMismatch)
    wprint("\aThe model has changed in a way that prevents the %s.\n",
           redo ? "redo" : "undo");
  if (err == UndoRedo::MemoryError)
    wprint("\aError - insufficient memory for operation!\n");
  if (err == UndoRedo::NoBackupItem)
    wprint("\aError - a needed backup item was not found.\n");
}


void inputQDefaultKeys(QKeyEvent *event, ImodView *vi)
{
  int keysym = event->key();
  int keypad = event->modifiers() & Qt::KeypadModifier;
  int shifted = event->modifiers() & Qt::ShiftModifier;
  int ctrl = event->modifiers() & Qt::ControlModifier;
  int bwStep = ImodPrefs->getBwStep();
  int mean, sd;
  Iobj *obj;
  Icont *cont;

  if (inputTestMetaKey(event))
    return;
#ifdef Q_OS_MACX
  if (event->key() == Qt::Key_Q && (event->modifiers() & Qt::ControlModifier)){
    imod_quit();
    return;
  }
#endif

  inputConvertNumLock(keysym, keypad);
  if (vi->ushortStore)
    bwStep *= B3DNINT(B3DMAX(1., (vi->rangeHigh - vi->rangeLow) / 256.));

  // Set this to 0 when a case is NOT handling the key
  int handled = 1;

  switch(keysym){

    /* DNM: Make this go to midpoint of stack instead of Inserting 
       a point */
#ifdef Q_OS_MACX
  case Qt::Key_Help:
#endif
  case Qt::Key_Insert:
    if (!keypad) {
      vi->zmouse = vi->zsize/2;
      imodDraw(vi, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC);
    } else
      handled = 0;
    break;
  case Qt::Key_Backspace:
  case Qt::Key_Delete:
    if (!keypad)
      inputDeletePoint(vi);
    else
      handled = 0;
    break;
  case Qt::Key_Home:
    if (!keypad) {
      if (shifted)
        vi->zmouse = vi->zsize/2;
      else
        vi->zmouse = vi->zsize - 1;
      imodDraw(vi, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC);
    } else
      handled = 0;
    break;
  case Qt::Key_End:
    if (!keypad) {
      vi->zmouse = 0;
      imodDraw(vi, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC);
    } else
      handled = 0;
    break;

  case Qt::Key_J:
    if (shifted) 
      imodContEditJoin(vi);
    else
      handled = 0;
    break;

  case Qt::Key_PageUp:
  case Qt::Key_PageDown:
    if (!keypad)
      inputPageUpOrDown(vi, shifted, keysym == Qt::Key_PageUp ? 1 : -1);
    else
      handled = 0;
    break;

  case Qt::Key_Slash:
    if (keypad)
      inputPrevz(vi, ImodPrefs->getPageStep());
    else
      handled = 0;
    break;

  case Qt::Key_Asterisk:
    if (keypad)
      inputNextz(vi, ImodPrefs->getPageStep());
    else {
      if (vi->cramp->falsecolor < 2)
        xcramp_falsecolor(vi->cramp, 1 - vi->cramp->falsecolor);
      if (App->rgba)
      imodDraw(App->cvi, IMOD_DRAW_IMAGE);
    }
    break;

  case Qt::Key_Up:
    if (!keypad)
      inputNexty(vi);
    else
      handled = 0;
    break;
  case Qt::Key_Down: 
    if (!keypad)
      inputPrevy(vi);
    else
      handled = 0;
    break;
  case Qt::Key_Right: 
    if (!keypad)
      inputNextx(vi);
    else
      handled = 0;
    break;
  case Qt::Key_Left: 
    if (!keypad)
      inputPrevx(vi);
    else
      handled = 0;
    break;

  case Qt::Key_Backslash:
    if (!vi->doingInitialLoad)
      slicerOpen(vi, 0);
    break; 
          
  case Qt::Key_A:
    if (shifted && !vi->fakeImage && !vi->rgbStore) {
      ImodPrefs->getAutoContrastTargets(mean, sd);
      imodInfoAutoContrast(mean, sd);
    } else
      handled = 0;
    break;

  case Qt::Key_B:
    if (!ctrl || imodContEditBreak())
      handled = 0;
    break;

  case Qt::Key_C:
    if (shifted)
      inputNextContour(vi);
    else
      inputPrevContour(vi);
    break;
          
  case Qt::Key_D:
    if (shifted && !ctrl)
      inputDeleteContour(vi);
    else if (ctrl && !shifted)
      inputTruncateContour(vi);
    else if (ctrl && shifted)
      ImodInfoWin->editSurfaceSlot(ESURFACE_MENU_DELETE);
    else
      handled = 0;
    break;

  case Qt::Key_E: /* erase current contour and/or point */
    if (shifted)
      vi->imod->cindex.contour = -1;
    vi->imod->cindex.point = -1;
    imod_setxyzmouse();
    break;
          
  case Qt::Key_F:
    if (shifted)
      inputFindMaxValue(vi);
    else
      inputFindValue(vi);
    break;


  case Qt::Key_G:
    if (shifted) {
      /* DMN 2/25/01: do not open with fake image */
      if (!vi->rgbStore && !vi->fakeImage)
        xgraphOpen(vi);
    } else if (ctrl) {
      inputToggleGap(vi);
    } else  
      inputGhostmode(vi);
    break;

  case Qt::Key_H:
    if (shifted)
      imodPlugOpenByName("Grab with Note");
    else
      handled = 0;
    break;

  case Qt::Key_K:
    iccCopyContour();
    break;
    
  case Qt::Key_M:
    if (shifted)
      inputMoveObject(vi);
    else
      imod_set_mmode(IMOD_MM_TOGGLE);
    break;
          
  case Qt::Key_N:
    if (shifted)
      inputNewSurface(vi);
    else
      inputNewContour(vi);
    break;

  case Qt::Key_0:
    inputNewObject(vi);
    imod_info_setobjcolor();
    imodvObjedNewView();
    break;

  case Qt::Key_Exclam:
    inputMovieTime(vi, 0);
    inputLimitingTime(vi, -1);
    break;

  case Qt::Key_1:
    inputMovieTime(vi, 0);
    inputPrevTime(vi);
    break;

  case Qt::Key_At:
    inputMovieTime(vi, 0);
    inputLimitingTime(vi, 1);
    break;

  case Qt::Key_2:
    inputMovieTime(vi, 0);
    inputNextTime(vi);
    break;

  case Qt::Key_3:
    inputMovieTime(vi, 1);
    break;
  case Qt::Key_4:
    inputMovieTime(vi, -1);
    break;

  case Qt::Key_NumberSign:
    imodMovieXYZT(vi, MOVIE_DEFAULT, MOVIE_DEFAULT, 1, MOVIE_DEFAULT);
    break;
  case Qt::Key_Dollar:
    imodMovieXYZT(vi, MOVIE_DEFAULT, MOVIE_DEFAULT, -1, MOVIE_DEFAULT);
    break;

  case Qt::Key_5:
    inputAdjacentContInSurf(vi, -1);
    break;

  case Qt::Key_6:
    inputAdjacentContInSurf(vi, 1);
    break;

  case Qt::Key_7:
    inputAdjacentSurface(vi, -1);
    break;

  case Qt::Key_8:
    inputAdjacentSurface(vi, 1);
    break;

  case Qt::Key_O:
    if (shifted) {
      obj = imodObjectGet(vi->imod);
      cont = imodContourGet(vi->imod);
      if (cont) {
        if (iobjClose(obj->flags))
          iceClosedOpen((cont->flags & ICONT_OPEN) ? 0 : 1);
        else
          wprint("\aObject must be closed type to toggle between open and "
                 "closed contours\n");
      }
    } else
    inputPrevObject(vi);
    break;

  case Qt::Key_P:
    inputNextObject(vi);
    break;
          
    // Brackets and braces are often not available without modifiers on
    // European keyboards, so add more keys for these
  case Qt::Key_BraceLeft:
  case Qt::Key_ParenLeft:
    inputFirstPoint(vi);
    break;
  case Qt::Key_BracketLeft:
  case Qt::Key_Less:
    inputPrevPoint(vi);
    break;
          
  case Qt::Key_BraceRight:
  case Qt::Key_ParenRight:
    inputLastPoint(vi);
    break;
  case Qt::Key_BracketRight:
  case Qt::Key_Greater:
    inputNextPoint(vi);
    break;

  case Qt::Key_T:
    if (shifted){
      if (vi->drawcursor)
        vi->drawcursor = FALSE;
      else
        vi->drawcursor = TRUE;
    } else if (ctrl) {
      obj = imodObjectGet(vi->imod);
      if (!obj)
        break;
      mean = (obj->flags & IMOD_OBJFLAG_OFF) ? 1 : 0;
      ioew_draw(mean);
      break;
    } else {
      vi->imod->drawmode -= (2 * vi->imod->drawmode);
      imodModelEditUpdate();
    }
    imodDraw(vi, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC);
    break;
          
  case Qt::Key_U:
    if (shifted && !vi->fakeImage && !vi->rgbStore && 
        !vi->doingInitialLoad) {
      imodv_open();
      imodvIsosurfaceEditDialog(Imodv, 1);
    } else
      handled = 0;
    break;

  case Qt::Key_V:
    if (!vi->doingInitialLoad)
      imodv_open();
    break;

  case Qt::Key_Y:
    if (ctrl)
      inputUndoRedo(vi, true);
    else
      handled = 0;
    break;

  case Qt::Key_Z:
    if (ctrl)
      inputUndoRedo(vi, false);
    else if (!vi->doingInitialLoad)
      imod_zap_open(vi, 0);
    break;
          
  case Qt::Key_R:
    if (ctrl)
      inputRaiseWindows();
    else
      handled = 0;
    break;

  case Qt::Key_Comma: /* slower movie */
    imcSetMovierate(vi, vi->movierate + 1);
    /* vi->movierate++; */
    break;
  case Qt::Key_Period: /* faster movie */
    imcSetMovierate(vi, vi->movierate - 1);
    /*
      vi->movierate--;
      if (vi->movierate < 0)
      vi->movierate = 0;
    */
    break;
          
    /* DNM 3/14/01: took out the DRAW_GL versions for readability */
  case Qt::Key_F1:
    xcramp_level(vi->cramp, -bwStep, 0);
    xcramp_getlevels(vi->cramp, &(vi->black), &(vi->white));
    imod_info_setbw(vi->black, vi->white);
    break;
  case Qt::Key_F2:
    /* DNM: move in tandem if they're equal; xcramp_ramp takes care
       of it in the F3 case */
    if (vi->black >= vi->white)
      xcramp_level(vi->cramp, bwStep, bwStep);
    else
      xcramp_level(vi->cramp, bwStep, 0);
    xcramp_getlevels(vi->cramp, &(vi->black), &(vi->white));
    imod_info_setbw(vi->black, vi->white);
    break;
  case Qt::Key_F3:
    xcramp_level(vi->cramp, 0, -bwStep);
    xcramp_getlevels(vi->cramp, &(vi->black), &(vi->white));
    imod_info_setbw(vi->black, vi->white);
    break;
  case Qt::Key_F4:
    xcramp_level(vi->cramp, 0, bwStep);
    xcramp_getlevels(vi->cramp, &(vi->black), &(vi->white));
    imod_info_setbw(vi->black, vi->white);
    break;
  case Qt::Key_F5:
    xcramp_level(vi->cramp, -bwStep, bwStep);
    xcramp_getlevels(vi->cramp, &(vi->black), &(vi->white));
    imod_info_setbw(vi->black, vi->white);
    break;
  case Qt::Key_F6:
    /* DNM 3/14/01: don't move if they are equal */
    if (vi->black + bwStep > vi->white - bwStep)
      break;
    xcramp_level(vi->cramp, bwStep, -bwStep);
    xcramp_getlevels(vi->cramp, &(vi->black), &(vi->white));
    imod_info_setbw(vi->black, vi->white);
    break;

    /* DNm 2/27/03: it makes more sense to decrease then increase brightness */
  case Qt::Key_F7:
    xcramp_level(vi->cramp, bwStep, bwStep);
    xcramp_getlevels(vi->cramp, &(vi->black), &(vi->white));
    imod_info_setbw(vi->black, vi->white);
    break;
  case Qt::Key_F8:
    xcramp_level(vi->cramp, -bwStep, -bwStep);
    xcramp_getlevels(vi->cramp, &(vi->black), &(vi->white));
    imod_info_setbw(vi->black, vi->white);
    break;
  case Qt::Key_F9:
    xcrampSelectIndex(vi->cramp, 0);
    xcramp_ramp(vi->cramp);
    xcramp_getlevels(vi->cramp, &(vi->black), &(vi->white));
    autoxCrampSelected(vi);
    imod_info_setbw(vi->black, vi->white);

    break;
  case Qt::Key_F10:
    xcrampSelectIndex(vi->cramp, 
                      (vi->cramp->clevel + 1) % vi->cramp->noflevels);
    xcramp_ramp(vi->cramp);
    xcramp_getlevels(vi->cramp, &(vi->black), &(vi->white));
    autoxCrampSelected(vi);
    imod_info_setbw(vi->black, vi->white);
    break;
  case Qt::Key_F11:
  case Qt::Key_Ampersand:
    xcramp_reverse(vi->cramp, !(vi->cramp->reverse));
    if (App->rgba){
      imodDraw(App->cvi, IMOD_DRAW_IMAGE);
    }
    break;

    // Also do same for asterisk (duplicate code)
  case Qt::Key_F12:
    if (vi->cramp->falsecolor < 2)
      xcramp_falsecolor(vi->cramp, 1 - vi->cramp->falsecolor);
    if (App->rgba){
      imodDraw(App->cvi, IMOD_DRAW_IMAGE);
    }
    break;
    
  default:
    handled = 0;
    break;

  }

  if (handled)
    event->accept();
  else
    event->ignore();

}

void inputPageUpOrDown(ImodView *vi, int shifted, int direction)
{
  Iobj *obj;
  if (shifted) {
    obj = imodObjectGet(vi->imod);
    vi->zmouse = utilNextSecWithCont(vi, obj, B3DNINT(vi->zmouse), direction);
    imodDraw(vi, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC);
  } else if (direction > 0)
    inputNextz(vi);
  else
    inputPrevz(vi);
}

/* Convert numeric keypad keys that come through as numbers because NumLock is
   on to the named keys */
/* But also turn off keypad on Mac if they are arrow keys */
static int keypadKeys[10] = {Qt::Key_Delete, Qt::Key_Insert, Qt::Key_End, 
                             Qt::Key_Down, Qt::Key_PageDown, Qt::Key_Left,
                             Qt::Key_Right, Qt::Key_Home, Qt::Key_Up,
                             Qt::Key_PageUp};
static int numLockKeys[10] = {Qt::Key_Period, Qt::Key_0, Qt::Key_1, Qt::Key_2,
                              Qt::Key_3, Qt::Key_4,
                              Qt::Key_6, Qt::Key_7, Qt::Key_8, Qt::Key_9};
void inputConvertNumLock(int &keysym, int &keypad)
{
  if (!keypad)
    return;
  for (int i = 0; i < 10; i++)
    if (keysym == numLockKeys[i]) {
      keysym = keypadKeys[i];
      return;
    }
#ifdef Q_OS_MACX
  if (keysym == Qt::Key_Left || keysym == Qt::Key_Right || 
      keysym == Qt::Key_Up || keysym == Qt::Key_Down)
    keypad = 0;
#endif
  return;
}

/* For Mac, allow modules to discard keys if the Ctrl key is down */
bool inputTestMetaKey(QKeyEvent *event)
{
#ifdef Q_OS_MACX
  if (event->modifiers() & Qt::MetaModifier) {
    wprint("\aUse the Apple/Command key instead of Ctrl on the Mac!\n");
    event->ignore();
    return true;
  }
#endif
  return false;
}
