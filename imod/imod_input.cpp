/*  IMOD VERSION 2.50
 *
 *  imod_input.c -- Handels general mouse/keyboard input.
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
#include <qapplication.h>
#include <qwidgetlist.h>
#include <qevent.h>
#include <qnamespace.h>
#include "dia_qtutils.h"
#include "xgraph.h"
#include "imod.h"
#include "imod_display.h"
#include "imod_workprocs.h"
#include "xcramp.h"
#include "xzap.h"
#include "imod_edit.h"
#include "imod_info.h"
#include "imod_input.h"
#include "imod_io.h"
#include "imod_info_cb.h"
#include "imod_cont_edit.h"
#include "imodv.h"
#include "imodv_window.h"
#include "imod_input.h"
#include "control.h"
#include "sslice.h"
#include "imod_moviecon.h"
#include "imod_model_edit.h"
#include "imod_object_edit.h"
#include "preferences.h"
#include "undoredo.h"

static void newContourOrSurface(ImodView *vw, int surface);

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
  ImodInfoWin->setActiveWindow();
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
void inputInsertPoint(ImodView *vw)
{
  Iobj *obj = imodObjectGet(vw->imod);
  Icont *cont;
  Ipoint point;
  int pt;

  /* create a new contour if there is no current contour */
  if (obj && vw->imod->mousemode == IMOD_MMODEL) {
    cont = ivwGetOrMakeContour(vw, obj, 0);
    if (!cont)
      return;

    /* Add point only if at current time */
    if (!ivwTimeMismatch(vw, 0, obj, cont)) {
      point.x = vw->xmouse;
      point.y = vw->ymouse;
      point.z = vw->zmouse;
      
      // Set insertion point to next point, or to current point if inserting
      // backwards
      pt = vw->imod->cindex.point + 1;
      if (pt > 0 && vw->insertmode)
        pt--;

      ivwRegisterInsertPoint(vw, cont, &point, pt);
    }
  }
  vw->undo->finishUnit();
  imodDraw(vw, IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
}

void inputDeletePoint(ImodView *vw)
{
  Icont *cont;
  if (vw->imod->mousemode == IMOD_MMODEL){

    cont = imodContourGet(vw->imod);
    if (cont->psize == 1)
      vw->undo->contourRemoval();
    else
      vw->undo->pointRemoval();

    if (imodDeletePoint(vw->imod) < 0)
      vw->undo->flushUnit();
    else
      vw->undo->finishUnit();

    /* If inserting in reverse, advance to next point unless at start
       of contour */
    if (vw->insertmode && vw->imod->cindex.point > 0){
      cont = imodContourGet(vw->imod);
      if (cont){
        vw->imod->cindex.point++;
        if (vw->imod->cindex.point >= cont->psize)
          vw->imod->cindex.point = cont->psize - 1;
      }
    }
    imod_info_setxyz();
    imodDraw(vw, IMOD_DRAW_MOD | IMOD_DRAW_XYZ);
  }
  return;
}

void inputModifyPoint(ImodView *vw)
{
  Iobj *obj = imodObjectGet(vw->imod);
  Icont *cont = imodContourGet(vw->imod);
  Ipoint *point = imodPointGet(vw->imod);
     
  if (point && cont && obj && !ivwTimeMismatch(vw, 0, obj, cont) &&
      vw->imod->mousemode == IMOD_MMODEL) {

    
    /* DNM: if z value is changing, need to set contour's wild flag */
    if (point->z != vw->zmouse) {
      Icont *cont = imodContourGet(vw->imod);
      if (!(cont->flags & ICONT_WILD))
        vw->undo->contourPropChg();
      cont->flags |= ICONT_WILD;
    }
    vw->undo->pointShift();
    point->x = vw->xmouse;
    point->y = vw->ymouse;
    point->z = vw->zmouse;
    vw->undo->finishUnit();
  }
  imodDraw(vw, IMOD_DRAW_MOD);
}

/* DNM 7/29/03: make correct nearest int tests for zmouse to avoid going 
   outside legal limits */
void inputNextz(ImodView *vw)
{
  if ((int)(vw->zmouse + 0.5) < (vw->zsize - 1)){
    vw->zmouse += 1.0;
    imodDraw(vw, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC);
  }
  return;
}

void inputPrevz(ImodView *vw)
{
  if ((int)floor(vw->zmouse + 0.5) > 0){
    vw->zmouse -= 1.0;
    imodDraw(vw, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC);
  }
  return;
}

/* DNM 7/29/03: treat Y just like X is now treated */
void inputNexty(ImodView *vw)
{
  if (vw->ymouse < (vw->ysize - 1)){
    vw->ymouse += 1.0;
    ivwBindMouse(vw);
    imodDraw(vw, IMOD_DRAW_XYZ);
  }
  return;
}

void inputPrevy(ImodView *vw)
{
  if (vw->ymouse > 0){
    vw->ymouse -= 1.0;
    ivwBindMouse(vw);
    imodDraw(vw, IMOD_DRAW_XYZ);
  }
  return;
}

/* DNM 2/13/03: add binding to limit mouse to possible values */
void inputNextx(ImodView *vw)
{
  if (vw->xmouse < (vw->xsize - 1)){
    vw->xmouse += 1.0;
    ivwBindMouse(vw);
    imodDraw(vw, IMOD_DRAW_XYZ);
  }
  return;
}

void inputPrevx(ImodView *vw)
{
  if (vw->xmouse > 0){
    vw->xmouse -= 1.0;
    ivwBindMouse(vw);
    imodDraw(vw, IMOD_DRAW_XYZ);
  }
  return;
}

/* DNM 6/16/01: To toggle ghost mode, save current mode if it is on; if it is
   off, transfer the next and previous section flags from the saved mode */
void inputGhostmode(ImodView *vw)
{
  if (vw->ghostmode & IMOD_GHOST_SECTION) {
    vw->ghostlast = vw->ghostmode;
    vw->ghostmode = vw->ghostmode & ~IMOD_GHOST_SECTION;
  } else
    vw->ghostmode |= (vw->ghostlast & IMOD_GHOST_SECTION);

  imodDraw(vw, IMOD_DRAW_MOD);
  return;
}

// Make a new contour
void inputNewContour(ImodView *vw)
{
  newContourOrSurface(vw, 0);
}

// Make a new contour on a new surface number
void inputNewSurface(ImodView *vw)
{
  newContourOrSurface(vw, 1);
}

// Make a new contour, optionally assigning it to a new surface
static void newContourOrSurface(ImodView *vw, int surface)
{
  Iobj *obj = imodObjectGet(vw->imod);
  Icont *cont;
  if (!obj || vw->imod->mousemode == IMOD_MMOVIE)
    return;

  // Register an object property change if there is a new surface
  if (surface)
    vw->undo->objectPropChg();
  vw->undo->contourAddition(obj->contsize);

  imodNewContour(vw->imod);
  cont = imodContourGet(vw->imod);
  if (surface)
    imodel_contour_newsurf(obj, cont);

  ivwSetNewContourTime(vw, obj, cont);
  vw->undo->finishUnit();
  if (imodSelectionListClear(vw))
    imod_setxyzmouse();
  imod_info_setocp();
}

// Unused 11/16/04
void inputContourDup(ImodView *vw)
{
  Icont *cont, *ocont;
  if (vw->imod->mousemode == IMOD_MMOVIE){
    wprint("\nMust be in model mode to edit model.\n");
    return;
  }
  cont = imodContourGet(vw->imod);
  if (!cont) return;
  ocont = imodContourDup(cont);
  imodNewContour(vw->imod);
  cont = imodContourGet(vw->imod);
  if (!cont) return;
  *cont = *ocont;
  free(ocont);
  imod_info_setocp();
}

void inputNextObject(ImodView *vw)
{
  imodNextObject(vw->imod);
  inputKeepContourAtSameTime(vw);

  /*  Drop tests for object in this and next function, setxyzmouse works OK 
      with no  object */
  imodSelectionListClear(vw);
  imod_setxyzmouse();
}

void inputPrevObject(ImodView *vw)
{
  imodPrevObject(vw->imod);
  inputKeepContourAtSameTime(vw);
  imodSelectionListClear(vw);
  imod_setxyzmouse();
}

// Finds first contour in object that matches the current time and sets it as
// current contour
void inputKeepContourAtSameTime(ImodView *vw)
{
  Iobj *obj;
  Icont *cont;
  int currentTime, co;
  int objIndex, contIndex, pointIndex;

  ivwGetTime(vw, &currentTime);

  obj  = imodObjectGet(vw->imod);
  if (!obj || !iobjFlagTime(obj)) 
    return;

  cont = imodContourGet(vw->imod);
  if (cont) {
    imodGetIndex(vw->imod, &objIndex, &contIndex, &pointIndex);

    if (cont->type != currentTime){
      for(co = 0; co < obj->contsize; co++){
	if (obj->cont[co].type ==  currentTime){
	  imodSetIndex (vw->imod, objIndex, co, pointIndex);
	  break;
        }
      }
    }
  }
}

void inputAdjacentSurface(ImodView *vw, int direction)
{
  Iobj *obj;
  int co, newsurf, newcont, cursurf;

  /* if no object selected, forget it. */
  if (vw->imod->cindex.object < 0)
    return;

  /* Selected Object. */
  obj = &(vw->imod->obj[vw->imod->cindex.object]);

  /* If object has no contours, or none is selected, return. */
  if (!obj->contsize)
    return;

  if (vw->imod->cindex.contour < 0)
    return;

  cursurf = obj->cont[vw->imod->cindex.contour].surf;
     
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

  vw->imod->cindex.contour = newcont;

  /* if point index is too high or low, change it. */
  if (vw->imod->cindex.point >= obj->cont[newcont].psize)
    vw->imod->cindex.point = obj->cont[newcont].psize - 1;
  if (vw->imod->cindex.point < 0 && obj->cont[newcont].psize > 0)
    vw->imod->cindex.point = 0;

  imod_setxyzmouse();
  return;
}

void inputAdjacentContInSurf(ImodView *vw, int direction)
{
  Iobj *obj;
  int co, newcont, cursurf;

  /* if no object selected, forget it. */
  if (vw->imod->cindex.object < 0)
    return;

  /* Selected Object. */
  obj = &(vw->imod->obj[vw->imod->cindex.object]);

  /* If object has no contours, or none is selected, return. */
  if (!obj->contsize)
    return;

  if (vw->imod->cindex.contour < 0)
    return;

  cursurf = obj->cont[vw->imod->cindex.contour].surf;
  newcont = -1;

  for (co = vw->imod->cindex.contour + direction; 
       co >= 0 && co < obj->contsize; co += direction)
    if (obj->cont[co].surf == cursurf) {
      newcont = co;
      break;
    }
  if (newcont == -1)
    return;

  vw->imod->cindex.contour = newcont;

  /* if point index is too high or low, change it. */
  if (vw->imod->cindex.point >= obj->cont[newcont].psize)
    vw->imod->cindex.point = obj->cont[newcont].psize - 1;
  if (vw->imod->cindex.point < 0 && obj->cont[newcont].psize > 0)
    vw->imod->cindex.point = 0;

  imod_setxyzmouse();
  return;
}



void inputNextContour(ImodView *vw)
{
  NextContour(vw->imod);
  inputRestorePointIndex(vw);
  imod_setxyzmouse();
}

void inputPrevContour(ImodView *vw)
{
  PrevContour(vw->imod);
  inputRestorePointIndex(vw);
  imod_setxyzmouse();
}

/* If the current point index is -1 and the new contour has points, set
   to starting point */
void inputRestorePointIndex(ImodView *vw)
{
  Icont *cont;

  if (vw->imod->cindex.point == -1){
    cont = imodContourGet(vw->imod);
    if (cont)
      if (cont->psize)
        vw->imod->cindex.point = 0;
  }
}

void inputNextPoint(ImodView *vw)
{
  NextPoint(vw->imod);
  imod_setxyzmouse();
  return;
}

/* DNM 3/29/01: make it go to first point if point undefined */
void inputPrevPoint(ImodView *vw)
{
  Icont *cont;
  if (PrevPoint(vw->imod) < 0) {
    cont = imodContourGet(vw->imod);
    if (cont)
      if (cont->psize)
        vw->imod->cindex.point = 0;
  }
  imod_setxyzmouse();
  return;
}

/* Set model index to a contour that exists
 * during the current time, if possible.
 */
void inputSetModelTime(ImodView *vw, int time)
{
  Iobj *obj = imodObjectGet(vw->imod);
  Icont *cont;
  Ipoint *point;
  int ob,co;

  int nco = -1; 
  int npt = -1;
     
  if (!obj) return;
  if (!iobjFlagTime(obj)) return;
     
  cont = imodContourGet(vw->imod);
  if (!cont) return;

  point = imodPointGet(vw->imod);
  if (!point){
    imodGetIndex(vw->imod,&ob,&nco,&npt);
    for(co = 0; co < obj->contsize; co++){
      if (obj->cont[co].type == time)
        nco = co;
    }
    imodSetIndex(vw->imod,ob,nco,npt);
  }else{

    imodGetIndex(vw->imod,&ob,&nco,&npt);
    for(co = 0; co < obj->contsize; co++){
      if (obj->cont[co].type == time)
        nco = co;
    }
    imodSetIndex(vw->imod,ob,nco,npt);
  }
}

void inputNextTime(ImodView *vw)
{
  int time, maxtime;

  maxtime = ivwGetTime(vw, &time);
  if (!maxtime) 
    return;
     
  time++;
  /*     inputSetModelTime(vw, time); */
  ivwSetTime(vw, time);
  imodDraw(vw, IMOD_DRAW_ALL);
}

void inputMovieTime(ImodView *vw, int val)
{
  int time;
  int maxtime = ivwGetTime(vw, &time);
  if (!maxtime)
    return;
  imodMovieXYZT(vw, MOVIE_DEFAULT, MOVIE_DEFAULT, MOVIE_DEFAULT, val);
}

void inputPrevTime(ImodView *vw)
{
  int time, maxtime;
     
  maxtime = ivwGetTime(vw, &time);
  if (!maxtime)
    return;
     
  time--;
  /*     inputSetModelTime(vw, time); */
  ivwSetTime(vw, time);
  imodDraw(vw, IMOD_DRAW_ALL);
}

void inputFirstPoint(ImodView *vw)
{
  Icont *cont = imodContourGet(vw->imod);
  if (!cont)
    return;
  if (!cont->psize)
    return;
  vw->imod->cindex.point = 0;
  imod_setxyzmouse();
}

void inputLastPoint(ImodView *vw)
{
  Icont *cont = imodContourGet(vw->imod);
  if (!cont)
    return;
  if (!cont->psize)
    return;
     
  vw->imod->cindex.point = cont->psize - 1;
  imod_setxyzmouse();
}

void inputMoveObject(ImodView *vw)
{
  imodContEditMove();
  imod_setxyzmouse();
}

void inputDeleteContour(ImodView *vw)
{
  /* DNM 3/29/01: have it make the previous contour be the current contour,
     but keep current point undefined to keep image from popping to that
     contour */
  Iobj *obj = imodObjectGet(vw->imod);
  int conew = vw->imod->cindex.contour -1;
  int numDel, i;
  b3dUInt32 delFlag = 2876351;
  Iindex *index;
  QString qstr;

  if (!ilistSize(vw->selectionList)) {
    if (!imodContourGet(vw->imod))
      return;
    vw->undo->contourRemoval();
    DelContour(vw->imod, vw->imod->cindex.contour);

  } else {

    // Multiple selection: first count and confirm if > 2 to delete
    numDel = 0;
    for (i = 0; i < ilistSize(vw->selectionList); i++) {
      index = (Iindex *)ilistItem(vw->selectionList, i);
      if (index->object ==  vw->imod->cindex.object)
        numDel++;
    }
    qstr.sprintf("Are you sure you want to delete these %d contours?", numDel);
    if (numDel > 2 && !dia_ask((char *)qstr.latin1()))
      return;

    // Go through again and set flags for deletion
    for (i = 0; i < ilistSize(vw->selectionList); i++) {
      index = (Iindex *)ilistItem(vw->selectionList, i);
      if (index->object ==  vw->imod->cindex.object)
        obj->cont[index->contour].flags = delFlag;
    }

    // Loop backwards through object removing flagged contours
    for (i = obj->contsize - 1; i >= 0; i--) {
      if (obj->cont[i].flags == delFlag) {
        vw->undo->contourRemoval(vw->imod->cindex.object, i);
        DelContour(vw->imod, i);
        conew = i - 1;
      }
    }
  }
  vw->undo->finishUnit();
    
  if (conew < 0 && obj->contsize > 0)
    conew = 0;
  vw->imod->cindex.contour = conew;
  imodSelectionListClear(vw);
  imod_setxyzmouse();
}

/* Truncate contour at the current point */
void inputTruncateContour(ImodView *vw)
{
  Icont *cont = imodContourGet(vw->imod);
  if (!cont || vw->imod->cindex.point < 0)
    return;
  vw->undo->contourDataChg();
  cont->psize = vw->imod->cindex.point + 1;
  vw->undo->finishUnit();
  imod_setxyzmouse();
}

void inputFindValue(ImodView *vw)
{
  float pixval;
  /*

  if ((!vw->fp) | (!vw->hdr)){
  wprint("Error: Find Value can't read file.\n");
  return;
  }

  */
  pixval = ivwGetFileValue(vw, (int)vw->xmouse, (int)vw->ymouse, 
                           (int)vw->zmouse);
  wprint("Pixel (%g, %g, %g) = %g\n", 
         vw->xmouse + 1, vw->ymouse + 1, vw->zmouse + 1, pixval);

  imodDraw(vw, IMOD_DRAW_XYZ);
  return;
}

void inputPointMove(ImodView *vw, int x, int y, int z)
{
  Iobj *obj;
  Icont *cont;
  Ipoint *pt;

  if (vw->imod->cindex.point == -1)
    return;
  cont = imodContourGet(vw->imod);
  if (!cont)
    return;

  vw->undo->pointShift();
  pt = &(cont->pts[vw->imod->cindex.point]);
  if (x){
    if (x > 0){
      pt->x += 1.0f;
      if (pt->x > vw->xsize - 1)
        pt->x = vw->xsize - 1;
    }else{
      pt->x -= 1.0f;
      if (pt->x < 0.0f)
        pt->x = 0.0f;
    }
          
  }
  if (y){
    if (y > 0){
      pt->y += 1.0f;
      if (pt->y > vw->ysize - 1)
        pt->y = vw->ysize - 1;
    }else{
      pt->y -= 1.0f;
      if (pt->y < 0.0f)
        pt->y = 0.0f;
    }
          
  }
  if (z){
    if (cont->psize > 1) {
      /* DNM: since z is changing, need to set contour's wild flag */
      obj = imodObjectGet(vw->imod);
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_WILD))
        wprint("\aContour is no longer in one Z plane. "
               "With this contour, you will not get a new"
               " contour automatically when you change Z.");
      vw->undo->contourPropChg();
      cont->flags |= ICONT_WILD;
    }
    if (z > 0){
      pt->z += 1.0f;
      vw->zmouse = pt->z;
      if (pt->z > vw->zsize - 1){
        pt->z = vw->zsize - 1;
        vw->zmouse = vw->zsize - 1;
      }
    }else{
      pt->z -= 1.0f;
      vw->zmouse = pt->z;
      if (pt->z < 0.0f){
        pt->z = 0.0f;
        vw->zmouse = 0;
      }
    }
  }
  vw->undo->finishUnit();
  imodDraw(vw, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ);
  return;
}

/* Find the maximum value within 5 pixels of the current location */
/* DNM 1/31/03 fixed problem with mx, my being used for max and loop limits */
void inputFindMaxValue(ImodView *vw)
{
  int mx, my;
  int  x,  y, maxx, maxy;
  float maxpixval, pixval;

  maxpixval = -1.e30;
  mx = (int)(vw->xmouse - 5);
  my = (int)(vw->ymouse - 5);
  for (x = mx; x <= mx + 10; x++)
    for (y = my ; y <= my + 10; y++){
      pixval = ivwGetFileValue(vw, x, y, (int)vw->zmouse);
      if (pixval > maxpixval){
        maxpixval = pixval;
        maxx = x; maxy = y;
      }
    }
  vw->xmouse = maxx;
  vw->ymouse = maxy;

  wprint("Pixel %g %g %g = %g\n", 
         vw->xmouse + 1, vw->ymouse + 1, vw->zmouse + 1, maxpixval);

  imodDraw(vw, IMOD_DRAW_XYZ);
  return;
}

// Create a new object
void inputNewObject(ImodView *vw)
{
  Iobj *obj;
  vw->undo->objectAddition(vw->imod->objsize);
  imodNewObject(vw->imod); 
  vw->undo->finishUnit();

  obj = imodObjectGet(vw->imod);

  /* DNM: need to find pixel value for new object, but no longer allocate */

  if (!App->rgba && App->depth <= 8)
    obj->fgcolor = App->objbase - vw->imod->cindex.object;
  else if (!App->rgba)
    obj->fgcolor = App->objbase + vw->imod->cindex.object;
     
  /* DNM 5/16/02: if multiple image files, set time flag by default */
  if (vw->nt)
    obj->flags |= IMOD_OBJFLAG_TIME;

  if (imodSelectionListClear(vw))
    imod_setxyzmouse();
  imod_info_setocp();
  imod_cmap(vw->imod);
  return;
}

void inputSaveModel(ImodView *vw)
{
  if (ImodForbidLevel)
    return;

  imod_info_forbid();
  vw->imod->blacklevel = vw->black;
  vw->imod->whitelevel = vw->white;

  /* DNM: the first FlipModel is unnecessary in normal cases and actually 
     messes up the saved model when there's a fakeimage; the second maybe
     restored the in-memory model in that case */
  /*     ivwFlipModel(vw); */
  if (SaveModel(vw->imod));
  /*        show_status("Error Saving Model.");   DNM: it already has messages
            else
            show_status("Done saving model."); */
  /*     ivwFlipModel(vw); */

  imod_info_enable();
  imod_draw_window();
  return;
}

// Do an undo or a redo
void inputUndoRedo(ImodView *vw, bool redo)
{
  int err;
  if (redo)
    err = vw->undo->redo();
  else
    err = vw->undo->undo();
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


// Until full Qt includes are used, we need the Qt:: before the keys.
// Or maybe it's the QT_CLEAN_NAMESPACE flag after all

void inputQDefaultKeys(QKeyEvent *event, ImodView *vw)
{
  int keysym = event->key();
  int keypad = event->state() & Qt::Keypad;
  int shifted = event->state() & Qt::ShiftButton;
  int ctrl = event->state() & Qt::ControlButton;
  int bwStep = ImodPrefs->getBwStep();
  int mean, sd;

  if (inputTestMetaKey(event))
    return;
  inputConvertNumLock(keysym, keypad);

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
      vw->zmouse = vw->zsize/2;
      imodDraw(vw, IMOD_DRAW_XYZ);
    } else
      handled = 0;
    break;
  case Qt::Key_Delete:
    if (!keypad)
      inputDeletePoint(vw);
    else
      handled = 0;
    break;
  case Qt::Key_Home:
    if (!keypad) {
      vw->zmouse = vw->zsize - 1;
      imodDraw(vw, IMOD_DRAW_XYZ);
    } else
      handled = 0;
    break;
  case Qt::Key_End:
    if (!keypad) {
      vw->zmouse = 0;
      imodDraw(vw, IMOD_DRAW_XYZ);
    } else
      handled = 0;
    break;

  case Qt::Key_J:
    if (shifted) 
      imodContEditJoin(vw);
    else
      handled = 0;
    break;

  case Qt::Key_Next:
    if (!keypad)
      inputPrevz(vw);
    else
      handled = 0;
    break;

  case Qt::Key_Prior:
    if (!keypad)
      inputNextz(vw);
    else
      handled = 0;
    break;

  case Qt::Key_Up:
    if (!keypad)
      inputNexty(vw);
    else
      handled = 0;
    break;
  case Qt::Key_Down: 
    if (!keypad)
      inputPrevy(vw);
    else
      handled = 0;
    break;
  case Qt::Key_Right: 
    if (!keypad)
      inputNextx(vw);
    else
      handled = 0;
    break;
  case Qt::Key_Left: 
    if (!keypad)
      inputPrevx(vw);
    else
      handled = 0;
    break;

  case Qt::Key_Backslash:
    if (!vw->rawImageStore)
      sslice_open(vw);
    break; 
          
  case Qt::Key_A:
    if (shifted && !vw->fakeImage && !vw->rawImageStore) {
      ImodPrefs->getAutoContrastTargets(mean, sd);
      imodInfoAutoContrast(mean, sd);
    } else
      handled = 0;
    break;

  case Qt::Key_C:
    if (shifted)
      inputNextContour(vw);
    else
      inputPrevContour(vw);
    break;
          
  case Qt::Key_D:
    if (shifted)
      inputDeleteContour(vw);
    else if (ctrl)
      inputTruncateContour(vw);
    else
      handled = 0;
    break;

  case Qt::Key_E: /* erase current contour and/or point */
    if (shifted)
      vw->imod->cindex.contour = -1;
    vw->imod->cindex.point = -1;
    imod_setxyzmouse();
    break;
          
  case Qt::Key_F:
    if (shifted)
      inputFindMaxValue(vw);
    else
      inputFindValue(vw);
    break;


  case Qt::Key_G:
    if (shifted) {
      /* DMN 2/25/01: do not open with fake image */
      if (!vw->rawImageStore && !vw->fakeImage)
        xgraphOpen(vw);
    } else  
      inputGhostmode(vw);
    break;

  case Qt::Key_M:
    if (shifted)
      inputMoveObject(vw);
    else
      imod_set_mmode(IMOD_MM_TOGGLE);
    break;
          
  case Qt::Key_N:
    if (shifted)
      inputNewSurface(vw);
    else
      inputNewContour(vw);
    break;

  case Qt::Key_0:
    inputNewObject(vw);
    break;

  case Qt::Key_1:
    inputPrevTime(vw);
    break;

  case Qt::Key_2:
    inputNextTime(vw);
    break;

  case Qt::Key_3:
    inputMovieTime(vw, -1);
    break;
  case Qt::Key_4:
    inputMovieTime(vw, 1);
    break;

  case Qt::Key_5:
    inputAdjacentContInSurf(vw, -1);
    break;

  case Qt::Key_6:
    inputAdjacentContInSurf(vw, 1);
    break;

  case Qt::Key_7:
    inputAdjacentSurface(vw, -1);
    break;

  case Qt::Key_8:
    inputAdjacentSurface(vw, 1);
    break;

  case Qt::Key_O:
    inputPrevObject(vw);
    break;

  case Qt::Key_P:
    inputNextObject(vw);
    break;
          
  case Qt::Key_BraceLeft:
  case Qt::Key_BracketLeft:
    if (shifted)
      inputFirstPoint(vw);
    else
      inputPrevPoint(vw);
    break;
          
  case Qt::Key_BraceRight:
  case Qt::Key_BracketRight:
    if (shifted)
      inputLastPoint(vw);
    else
      inputNextPoint(vw);
    break;

  case Qt::Key_T:
    if (shifted){
      if (vw->drawcursor)
        vw->drawcursor = FALSE;
      else
        vw->drawcursor = TRUE;
    }else{
      vw->imod->drawmode -= (2 * vw->imod->drawmode);
      imodModelEditUpdate();
    }
    imodDraw(vw, IMOD_DRAW_MOD);
    break;
          
  case Qt::Key_V:
    imodv_open();
    break;

  case Qt::Key_Y:
    if (ctrl)
      inputUndoRedo(vw, true);
    else
      handled = 0;
    break;

  case Qt::Key_Z:
    if (ctrl)
      inputUndoRedo(vw, false);
    else
      imod_zap_open(vw);
    break;
          
  case Qt::Key_R:
    if (ctrl)
      inputRaiseWindows();
    else
      handled = 0;
    break;

  case Qt::Key_Comma: /* slower movie */
    imcSetMovierate(vw, vw->movierate + 1);
    /* vw->movierate++; */
    break;
  case Qt::Key_Period: /* faster movie */
    imcSetMovierate(vw, vw->movierate - 1);
    /*
      vw->movierate--;
      if (vw->movierate < 0)
      vw->movierate = 0;
    */
    break;
          
    /* DNM 3/14/01: took out the DRAW_GL versions for readability */
  case Qt::Key_F1:
    xcramp_level(vw->cramp, -bwStep, 0);
    xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
    imod_info_setbw(vw->black, vw->white);
    break;
  case Qt::Key_F2:
    /* DNM: move in tandem if they're equal; xcramp_ramp takes care
       of it in the F3 case */
    if (vw->black >= vw->white)
      xcramp_level(vw->cramp, bwStep, bwStep);
    else
      xcramp_level(vw->cramp, bwStep, 0);
    xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
    imod_info_setbw(vw->black, vw->white);
    break;
  case Qt::Key_F3:
    xcramp_level(vw->cramp, 0, -bwStep);
    xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
    imod_info_setbw(vw->black, vw->white);
    break;
  case Qt::Key_F4:
    xcramp_level(vw->cramp, 0, bwStep);
    xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
    imod_info_setbw(vw->black, vw->white);
    break;
  case Qt::Key_F5:
    xcramp_level(vw->cramp, -bwStep, bwStep);
    xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
    imod_info_setbw(vw->black, vw->white);
    break;
  case Qt::Key_F6:
    /* DNM 3/14/01: don't move if they are equal */
    if (vw->black + bwStep > vw->white - bwStep)
      break;
    xcramp_level(vw->cramp, bwStep, -bwStep);
    xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
    imod_info_setbw(vw->black, vw->white);
    break;

    /* DNm 2/27/03: it makes more sense to decrease then increase brightness */
  case Qt::Key_F7:
    xcramp_level(vw->cramp, bwStep, bwStep);
    xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
    imod_info_setbw(vw->black, vw->white);
    break;
  case Qt::Key_F8:
    xcramp_level(vw->cramp, -bwStep, -bwStep);
    xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
    imod_info_setbw(vw->black, vw->white);
    break;
  case Qt::Key_F9:
    xcrampSelectIndex(vw->cramp, 0);
    xcramp_ramp(vw->cramp);
    xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
    imod_info_setbw(vw->black, vw->white);

    break;
  case Qt::Key_F10:
    xcrampSelectIndex(vw->cramp, 
                      (vw->cramp->clevel + 1) % vw->cramp->noflevels);
    xcramp_ramp(vw->cramp);
    xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
    imod_info_setbw(vw->black, vw->white);
    break;
  case Qt::Key_F11:
    xcramp_reverse(vw->cramp, !(vw->cramp->reverse));
    if (App->rgba){
      imodDraw(App->cvi, IMOD_DRAW_IMAGE);
    }
    break;
  case Qt::Key_F12:
    xcramp_falsecolor(vw->cramp, !(vw->cramp->falsecolor));
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

/* Convert numeric keypad keys that come through as numbers because NumLock is
   on to the named keys */
/* But also turn off keypad on Mac if they are arrow keys */
static int keypadKeys[10] = {Qt::Key_Delete, Qt::Key_Insert, Qt::Key_End, 
                             Qt::Key_Down, Qt::Key_Next, Qt::Key_Left,
                             Qt::Key_Right, Qt::Key_Home, Qt::Key_Up,
                             Qt::Key_Prior};
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
  if (event->state() & Qt::MetaButton) {
    wprint("\aUse the Apple/Command key instead of Ctrl on the Mac!\n");
    event->ignore();
    return true;
  }
#endif
  return false;
}

/*
$Log$
Revision 4.20  2004/11/01 23:38:06  mast
Changes for multiple selection and deletion of multiple contours

Revision 4.19  2004/09/21 20:28:44  mast
Backed out an erroneous checkin

Revision 4.17  2004/07/11 18:29:52  mast
Consolidated code for new contour or surface and used new function
for getting the contour to add points to

Revision 4.16  2003/12/18 22:44:43  mast
Disable A hot key for fake or raw image

Revision 4.15  2003/11/02 00:07:21  mast
Raise doesnt work in windows, add command that makes it flash

Revision 4.14  2003/10/30 06:19:16  mast
Add A hotkey for autocontrast

Revision 4.13  2003/07/30 00:15:51  mast
Fixed bug that let Z go outside legal limits

Revision 4.12  2003/06/04 23:32:47  mast
Output integer coordinates numbered from one in f and F outputs

Revision 4.11  2003/05/23 02:44:58  mast
Raise windows in order of image then dialog

Revision 4.10  2003/04/18 20:16:39  mast
Rename meta test function

Revision 4.9  2003/04/18 20:08:18  mast
Implement function to reject Ctrl key and give message on Mac

Revision 4.8  2003/04/17 19:27:13  mast
keypad workaround for Mac

Revision 4.7  2003/03/24 17:58:09  mast
Changes for new preferences capability

Revision 4.6  2003/03/13 07:15:33  mast
Make raise window function global

Revision 4.5  2003/03/13 01:17:25  mast
Add function to convert numlocked keypad keys, and add function and
hotkey to raise all windows

Revision 4.4  2003/03/12 06:35:35  mast
Modified inputInsertPoint to work like the xyz window insert in terms of
times and creating a new contour; modified inputModifyPoint to respect time

Revision 4.3  2003/02/27 23:45:42  mast
Add function to truncate contour

Revision 4.2  2003/02/14 01:15:20  mast
Try to prevent bad pageup's

Revision 4.1  2003/02/10 20:29:00  mast
autox.cpp

Revision 1.1.2.13  2003/02/03 05:38:45  mast
fixed problem in finding highest pixel (F)

Revision 1.1.2.12  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.11  2003/01/23 20:05:43  mast
*** empty log message ***

Revision 1.1.2.10  2003/01/18 01:15:25  mast
remove keypad include

Revision 1.1.2.9  2003/01/14 21:52:38  mast
include new movie controller include file

Revision 1.1.2.8  2003/01/13 01:15:42  mast
changes for Qt version of info window

Revision 1.1.2.7  2003/01/10 23:52:54  mast
add new xgraoh include

Revision 1.1.2.6  2003/01/06 15:52:16  mast
changes for Qt version of slicer

Revision 1.1.2.5  2003/01/04 03:47:42  mast
add include of imod_input.h (!) and control.h

Revision 1.1.2.4  2002/12/19 04:37:13  mast
Cleanup of unused global variables and defines

Revision 1.1.2.3  2002/12/17 18:40:24  mast
Changes and new includes with Qt version of imodv

Revision 1.1.2.2  2002/12/13 06:09:09  mast
include file changes

Revision 1.1.2.1  2002/12/09 17:51:07  mast
Conversion to cpp and inclusion of a Qt version for default input keys

Revision 3.2.2.1  2002/12/05 16:23:52  mast
No changes - CVS detected as modified in branch

Revision 3.3  2002/12/03 15:50:15  mast
Before a save, have it test the forbid level, then set forbid level
during the save, to prevent multiple file dialogs

Revision 3.2  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.1  2002/05/20 15:34:47  mast
Made time index modeling be the default for a new object if multiple files
are open

*/
