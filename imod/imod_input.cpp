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
#include "imod_input.h"
#include "control.h"
#include "sslice.h"
#include "imod_moviecon.h"
#include "imod_model_edit.h"
#include "imod_object_edit.h"
#include "preferences.h"

void inputRaiseWindows()
{
  QWidgetList  *list = QApplication::topLevelWidgets();
  QWidgetListIt it( *list );  // iterate over the widgets
  QWidget * w;
  while ( (w=it.current()) != 0 ) {   // for each top level widget...
    ++it;
    if (w->isVisible() )
      w->raise();
  }
  delete list;                // delete the list, not the widgets
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
  Icont *cont = imodContourGet(vw->imod);
  Ipoint point;
  int pt;

  /* create a new contour if there is no current contour */
  if (obj && vw->imod->mousemode == IMOD_MMODEL) {
    if (!cont) {
      vw->imod->cindex.contour = obj->contsize - 1;
      NewContour(vw->imod);
      cont = imodContourGet(vw->imod);
      if (!cont)
	return;
      if (iobjFlagTime(obj)){
	cont->type = vw->ct;
	cont->flags |= ICONT_TYPEISTIME;
      }
    }
    
    /* Set time of empty contour to current time */
    if (!cont->psize && zapTimeMismatch(vw, 0, obj, cont))
      cont->type = vw->ct;

    /* Add point only if at current time */
    if (!zapTimeMismatch(vw, 0, obj, cont)) {
      point.x = vw->xmouse;
      point.y = vw->ymouse;
      point.z = vw->zmouse;
      
      pt = vw->imod->cindex.point;
      if ((cont->psize - 1) == pt)
	imodNewPoint(vw->imod, &point);
      else{
	if (vw->insertmode)
	  InsertPoint(vw->imod, &point, pt);
	else
	  InsertPoint(vw->imod, &point, pt + 1);
      }
    }
  }
  imodDraw(vw, IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
}

void inputDeletePoint(ImodView *vw)
{
  Icont *cont;
  if (vw->imod->mousemode == IMOD_MMODEL){
          
    imodDeletePoint(vw->imod);

    /* If inserting in reverse, advance to next point unless at start
       of contour */
    if (vw->insertmode && vw->imod->cindex.point){
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
     
  if (point && cont && obj && !zapTimeMismatch(vw, 0, obj, cont) &&
      vw->imod->mousemode == IMOD_MMODEL) {
    /* DNM: if z value is changing, need to set contour's wild flag */
    if (point->z != vw->zmouse) {
      Icont *cont = imodContourGet(vw->imod);
      cont->flags |= ICONT_WILD;
    }
    point->x = vw->xmouse;
    point->y = vw->ymouse;
    point->z = vw->zmouse;
  }
  imodDraw(vw, IMOD_DRAW_MOD);
}

void inputNextz(ImodView *vw)
{
  if (vw->zmouse < (vw->zsize - 1)){
    vw->zmouse++;
    imodDraw(vw, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC);
  }
  return;
}

void inputPrevz(ImodView *vw)
{
  if (vw->zmouse > 0){
    vw->zmouse--;
    imodDraw(vw, IMOD_DRAW_XYZ | IMOD_DRAW_NOSYNC);
  }
  return;
}

void inputNexty(ImodView *vw)
{
  if (vw->ymouse < (vw->ysize - 1)){
    vw->ymouse++;
    imodDraw(vw, IMOD_DRAW_XYZ);
  }
  return;
}

void inputPrevy(ImodView *vw)
{
  if (vw->ymouse > 0){
    vw->ymouse--;
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

void inputNewContour(ImodView *vw)
{
  if (vw->imod->mousemode == IMOD_MMOVIE)
    return;
  if (vw->imod->cindex.object >= 0)
    imodNewContour(vw->imod);

  if (vw->nt){
    Iobj *obj = imodObjectGet(vw->imod);
    if ((obj) && (iobjTime(obj->flags))){
      Icont *cont = imodContourGet(vw->imod);
      if (cont){
        cont->type = vw->ct;
        cont->flags |= ICONT_TYPEISTIME;
      }
    }
  }
  imod_info_setocp();
  return;
}

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
  NewContour(vw->imod);
  cont = imodContourGet(vw->imod);
  if (!cont) return;
  *cont = *ocont;
  free(ocont);
  imod_info_setocp();
}

void inputNewSurface(ImodView *vw)
{
  if (vw->imod->mousemode == IMOD_MMOVIE)
    return;
  if (vw->imod->cindex.object >= 0)
    NewContour(vw->imod);
  imodel_contour_newsurf(imodObjectGet(vw->imod),
                         imodContourGet(vw->imod));
  if (vw->nt){
    Iobj *obj = imodObjectGet(vw->imod);
    if ((obj) && (iobjTime(obj->flags))){
      Icont *cont = imodContourGet(vw->imod);
      if (cont){
        cont->type = vw->ct;
        cont->flags |= ICONT_TYPEISTIME;
      }
    }
  }
  imod_info_setocp();
  return;
}

void inputNextObject(ImodView *vw)
{
  imodNextObject(vw->imod);

  /*  Drop tests for object in this and next function, setxyzmouse works OK 
      with no  object */

  imod_setxyzmouse();
}

void inputPrevObject(ImodView *vw)
{
  imodPrevObject(vw->imod);
  inputKeepContourAtSameTime(vw);
  imod_setxyzmouse();
}

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
	  imodSetIndex
	    (vw->imod, objIndex, co, pointIndex);
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
  if (!maxtime) return;
     
  time++;
  /*     inputSetModelTime(vw, time); */
  ivwSetTime(vw, time);
  imodDraw(vw, IMOD_DRAW_ALL);
  return;
}

void inputMovieTime(ImodView *vw, int val)
{
  int time;
  int maxtime = ivwGetTime(vw, &time);
  if (!maxtime) return;
  imodMovieXYZT(vw, MOVIE_DEFAULT, MOVIE_DEFAULT, 
                MOVIE_DEFAULT, val);
  return;
}

void inputPrevTime(ImodView *vw)
{
  int time, maxtime;
     
  maxtime = ivwGetTime(vw, &time);
  if (!maxtime) return;
     
  time--;
  /*     inputSetModelTime(vw, time); */
  ivwSetTime(vw, time);
  imodDraw(vw, IMOD_DRAW_ALL);
  return;
}

void inputFirstPoint(ImodView *vw)
{
  Icont *cont;

  cont = imodContourGet(vw->imod);
  if (!cont)
    return;
  if (!cont->psize)
    return;
  vw->imod->cindex.point = 0;
  imod_setxyzmouse();
  return;
}

void inputLastPoint(ImodView *vw)
{
  Icont *cont;

  cont = imodContourGet(vw->imod);
  if (!cont)
    return;
  if (!cont->psize)
    return;
     
  vw->imod->cindex.point = cont->psize - 1;
  imod_setxyzmouse();
  return;
}

void inputMoveObject(ImodView *vw)
{
  imodContEditMove();
  imod_setxyzmouse();
  return;
}

void inputDeleteContour(ImodView *vw)
{
  /* DNM 3/29/01: have it make the previous contour be the current contour,
     but keep current point undefined to keep image from popping to that
     contour */
  Iobj *obj = imodObjectGet(vw->imod);
  int conew = vw->imod->cindex.contour -1;

  if (imodContourGet(vw->imod)){
    DelContour(vw->imod, vw->imod->cindex.contour);
    if (conew < 0 && obj->contsize > 0)
      conew = 0;
    vw->imod->cindex.contour = conew;
    imod_setxyzmouse();
  }
  return;
}

/* Truncate contour at the current point */
void inputTruncateContour(ImodView *vw)
{
  Icont *cont = imodContourGet(vw->imod);
  if (!cont || vw->imod->cindex.point < 0)
    return;
  cont->psize = vw->imod->cindex.point + 1;
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
         vw->xmouse, vw->ymouse, vw->zmouse, pixval);

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
    if (cont->psize > 1) {
      /* DNM: since z is changing, need to set contour's wild flag */
      obj = imodObjectGet(vw->imod);
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_WILD))
        wprint("\aContour is no longer in one Z plane. "
               "With this contour, you will not get a new"
               " contour automatically when you change Z.");
      cont->flags |= ICONT_WILD;
    }
  }
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
         vw->xmouse, vw->ymouse, vw->zmouse, maxpixval);

  imodDraw(vw, IMOD_DRAW_XYZ);
  return;
}

void inputNewObject(ImodView *vw)
{
  Iobj *obj;
  imodNewObject(vw->imod); 

  obj = imodObjectGet(vw->imod);

  /* DNM: need to find pixel value for new object, but no longer allocate */

  if (!App->rgba && App->depth <= 8)
    obj->fgcolor = App->objbase - vw->imod->cindex.object;
  else if (!App->rgba)
    obj->fgcolor = App->objbase + vw->imod->cindex.object;
     
  /* DNM 5/16/02: if multiple image files, set time flag by default */
  if (vw->nt)
    obj->flags |= IMOD_OBJFLAG_TIME;

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

// Until full Qt includes are used, we need the Qt:: before the keys.
// Or maybe it's the QT_CLEAN_NAMESPACE flag after all

void inputQDefaultKeys(QKeyEvent *event, ImodView *vw)
{
  int keysym = event->key();
  int keypad = event->state() & Qt::Keypad;
  int shifted = event->state() & Qt::ShiftButton;
  int bwStep = ImodPrefs->getBwStep();

  if (keypad)
    keysym = inputConvertNumLock(keysym);

  // Set this to 0 when a case is NOT handling the key
  int handled = 1;

  switch(keysym){

    /* DNM: Make this go to midpoint of stack instead of Inserting 
       a point */
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
  case Qt::Key_Next:
    if (!keypad)
      inputPrevz(vw);
    else
      handled = 0;
    break;

  case Qt::Key_K:
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
          
  case Qt::Key_C:
    if (shifted)
      inputNextContour(vw);
    else
      inputPrevContour(vw);
    break;
          
  case Qt::Key_D:
    if (shifted)
      inputDeleteContour(vw);
    else if (event->state() & Qt::ControlButton)
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

  case Qt::Key_H:
  case Qt::Key_1:
    inputPrevTime(vw);
    break;

  case Qt::Key_L:
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

  case Qt::Key_Z:
    imod_zap_open(vw);
    break;
          
  case Qt::Key_R:
    if (event->state() & Qt::ControlButton)
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
static int keypadKeys[10] = {Qt::Key_Delete, Qt::Key_Insert, Qt::Key_End, 
                             Qt::Key_Down, Qt::Key_Next, Qt::Key_Left,
                             Qt::Key_Right, Qt::Key_Home, Qt::Key_Up,
                             Qt::Key_Prior};
static int numLockKeys[10] = {Qt::Key_Period, Qt::Key_0, Qt::Key_1, Qt::Key_2,
                              Qt::Key_3, Qt::Key_4,
                              Qt::Key_6, Qt::Key_7, Qt::Key_8, Qt::Key_9};
int inputConvertNumLock(int keysym)
{
  for (int i = 0; i < 10; i++)
    if (keysym == numLockKeys[i])
      return keypadKeys[i];
  return keysym;
}

/*
$Log$
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
