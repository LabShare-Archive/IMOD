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

    $Log$
    Revision 3.1  2002/05/20 15:34:47  mast
    Made time index modeling be the default for a new object if multiple files
    are open

*/

#include <stdio.h>
#include <math.h>
#include "keypad.h"
#include "imod.h"
#include "mrcfiles.h"

extern long Typemenu;
     
int Imod_obj_moveto = 0;

void imodv_open_nob(Imod *imod);

/* old function still in use */
int mouse_in_box(int llx, int lly, int urx, int  ury, int mousex, int mousey)
{
     
     if ( (mousex >= llx ) && (mousey >= lly) && 
	 (mousex <= urx ) && (mousey <= ury))
	  return(1);
     else
	  return(0);
}


void inputInsertPoint(ImodView *vw)
{
     Icont *cont = imodContourGet(vw->imod);
     Ipoint point;
     int pt;

     if (!cont)
	  return;
     if (vw->imod->mousemode == IMOD_MMODEL){
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
	  imodDraw(vw, IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
     }
     return;
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
     Ipoint *point = imodPointGet(vw->imod);
     
     if (!point)
	  return;
     if (vw->imod->mousemode == IMOD_MMODEL){
          /* DNM: if z value is changing, need to set contour's wild flag */
          if (point->z != vw->zmouse) {
	       Icont *cont = imodContourGet(vw->imod);
	       cont->flags |= ICONT_WILD;
	  }
	  point->x = vw->xmouse;
	  point->y = vw->ymouse;
	  point->z = vw->zmouse;
	  imodDraw(vw, IMOD_DRAW_MOD);
     }
     return;
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

void inputNextx(ImodView *vw)
{
     if (vw->xmouse < (vw->xsize - 1)){
	  vw->xmouse++;
	  imodDraw(vw, IMOD_DRAW_XYZ);
     }
     return;
}

void inputPrevx(ImodView *vw)
{
     if (vw->xmouse > 0){
	  vw->xmouse--;
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
     Iobj *obj;
     Icont *cont;
     int currentTime, co;
     int objIndex, contIndex, pointIndex;

     ivwGetTime(vw, &currentTime);
     imodNextObject(vw->imod);

     obj  = imodObjectGet(vw->imod);
     if (!obj) return;
     cont = imodContourGet(vw->imod);

     /* DNM: modify so that imod_setxyzmouse always gets called - it's needed
	to keep the info window updated properly */
     if (cont) {
	  imodGetIndex(vw->imod, &objIndex, &contIndex, &pointIndex);

	  if iobjFlagTime(obj){
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

     imod_setxyzmouse();
     return;
}

void inputPrevObject(ImodView *vw)
{
     Iobj *obj;
     Icont *cont;
     int currentTime, co;
     int objIndex, contIndex, pointIndex;

     ivwGetTime(vw, &currentTime);
     imodPrevObject(vw->imod);
     
     obj  = imodObjectGet(vw->imod);
     if (!obj) return;
     cont = imodContourGet(vw->imod);
     if (cont) {
	  imodGetIndex(vw->imod, &objIndex, &contIndex, &pointIndex);

	  if iobjFlagTime(obj){
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
     imod_setxyzmouse();
     return;
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
     Icont *cont;
     
     NextContour(vw->imod);
     
     if (vw->imod->cindex.point == -1){
	  cont = imodContourGet(vw->imod);
	  if (cont)
	       if (cont->psize)
		    vw->imod->cindex.point = 0;
     }

     imod_setxyzmouse();
     return;
}

void inputPrevContour(ImodView *vw)
{
     Icont *cont;
     PrevContour(vw->imod);

     if (vw->imod->cindex.point == -1){
	  cont = imodContourGet(vw->imod);
	  if (cont)
	       if (cont->psize)
		    vw->imod->cindex.point = 0;
     }

     imod_setxyzmouse();
     return;
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
     int ob,co,pt;

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
     inputContourMove();
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

void inputFindMaxValue(ImodView *vw)
{
     int mx, my;
     int  x,  y;
     float maxpixval, pixval;

     mx = (int)(vw->xmouse - 5);
     my = (int)(vw->ymouse - 4);
     maxpixval = ivwGetFileValue(vw, mx, my, (int)vw->zmouse);
     for (x = mx; x < mx + 10; x++)
	  for (y = my - 1; y < my + 9; y++){
	       pixval = ivwGetFileValue(vw, x, y, (int)vw->zmouse);
	       if (pixval > maxpixval){
		    maxpixval = pixval;
		    mx = x; my = y;
	       }
	  }
     vw->xmouse = mx;
     vw->ymouse = my;

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

     /* DNM: need to allocate and find pixel value for new object */
     if (App->rgba)
	  alloc_object_colors(vw->imod, vw->imod->objsize - 1, 
			      vw->imod->objsize - 1);
     else if (App->depth <= 8)
	  obj->fgcolor = App->objbase - vw->imod->cindex.object;
     else
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
     vw->imod->blacklevel = vw->black;
     vw->imod->whitelevel = vw->white;

     /* DNM: the first FlipModel is unnecessary in normal cases and actually 
	messes up the saved model when there's a fakeimage; the second maybe
	restored the in-memory model in that case */
     /*     ivwFlipModel(vw); */
     if (SaveModel(vw->imod));
/*	  show_status("Error Saving Model.");   DNM: it already has messages
     else
	  show_status("Done saving model."); */
     /*     ivwFlipModel(vw); */

     imod_draw_window();
     return;
}

void inputDefaultKeys(XKeyEvent *event, ImodView *vw)
{
     KeySym keysym = XLookupKeysym(event, 0);

     switch(keysym){

	  /* DNM: Make this go to midpoint of stack instead of Inserting 
	     a point */
	case XK_Insert:
	  vw->zmouse = vw->zsize/2;
	  imodDraw(vw, IMOD_DRAW_XYZ);
	  break;
	case XK_Delete:
	  inputDeletePoint(vw);
	  break;
	case XK_Home:
	  vw->zmouse = vw->zsize - 1;
	  imodDraw(vw, IMOD_DRAW_XYZ);
	  break;
	case XK_End:
	  vw->zmouse = 0;
	  imodDraw(vw, IMOD_DRAW_XYZ);
	  break;

	case XK_j:
	case IMOD_XK_Prior:
	  inputPrevz(vw);
	  break;

	case XK_k:
	case IMOD_XK_Next:
	  inputNextz(vw);
	  break;

	case XK_Up:
	  inputNexty(vw);
	  break;
	case XK_Down: 
	  inputPrevy(vw);
	  break;
	case XK_Right: 
	  inputNextx(vw);
	  break;
	case XK_Left: 
	  inputPrevx(vw);
	  break;

	case XK_backslash:
	  if (!vw->rawImageStore)
	       sslice_open(vw);
	  break; 
	  
	case XK_c:
	case XK_C:
	  if (event->state & ShiftMask)
	       inputNextContour(vw);
	  else
	       inputPrevContour(vw);
	  break;
	  
	case XK_d:
	case XK_D:
	  if (event->state & ShiftMask)
	       inputDeleteContour(vw);
	  break;

	case XK_e: /* erase current contour and/or point */
	  if (event->state & ShiftMask)
	       vw->imod->cindex.contour = -1;
	  vw->imod->cindex.point = -1;
	  imod_setxyzmouse();
	  break;
	  
	case XK_f:
	case XK_F:
	  if (event->state & ShiftMask)
	       inputFindMaxValue(vw);
	  else
	       inputFindValue(vw);
	  break;


	case XK_g:
	case XK_G:
	  if (event->state & ShiftMask) {
	       /* DMN 2/25/01: do not open with fake image */
	       if (!vw->rawImageStore && !vw->fakeImage)
		    xgraphOpen(vw);
	  } else  
	       inputGhostmode(vw);
	  break;

	case XK_m:
	case XK_M:
	  if (event->state & ShiftMask)
	       inputMoveObject(vw);
	  else
	       imod_set_mmode(IMOD_MM_TOGGLE);
	  break;
	  
	case XK_n:
	case XK_N:
	  if (event->state & ShiftMask)
	       inputNewSurface(vw);
	  else
	       inputNewContour(vw);
	  break;

	case XK_0:
	  inputNewObject(vw);
	  break;

	case XK_h:
	case XK_1:
	  inputPrevTime(vw);
	  break;

	case XK_l:
	case XK_2:
	  inputNextTime(vw);
	  break;

	case XK_3:
	  inputMovieTime(vw, -1);
	  break;
	case XK_4:
	  inputMovieTime(vw, 1);
	  break;

	case XK_5:
	  inputAdjacentContInSurf(vw, -1);
	  break;

	case XK_6:
	  inputAdjacentContInSurf(vw, 1);
	  break;

	case XK_7:
	  inputAdjacentSurface(vw, -1);
	  break;

	case XK_8:
	  inputAdjacentSurface(vw, 1);
	  break;

	case XK_o:
	case XK_O:
	  inputPrevObject(vw);
	  break;

	case XK_p:
	case XK_P:
	  inputNextObject(vw);
	  break;
	  
	case XK_braceleft:
	case XK_bracketleft:
	  if (event->state & ShiftMask)
	       inputFirstPoint(vw);
	  else
	       inputPrevPoint(vw);
	  break;
	  
	case XK_braceright:
	case XK_bracketright:
	  if (event->state & ShiftMask)
	       inputLastPoint(vw);
	  else
	       inputNextPoint(vw);
	  break;

	  /* DNM: forget this, it doesn't work anymore and image scale does */
	  /* case XK_R:
	     case XK_r:
	     if (event->state & ShiftMask)
	     imod_io_image_reload(vw);
	     break;
	  */
	case XK_t:
	case XK_T:
	  if (event->state & ShiftMask){
	       if (vw->drawcursor)
		    vw->drawcursor = FALSE;
	       else
		    vw->drawcursor = TRUE;
	  }else{
	       vw->imod->drawmode -= (2 * vw->imod->drawmode);
	  }
	  imodDraw(vw, IMOD_DRAW_MOD);
	  break;
	  
	case XK_v:
	  if (event->state & ShiftMask)
	       imodv_open_nob(vw->imod);
	  else
	       imodv_open(vw->imod, App->objbase);
	  break;

	case XK_z:
	  imod_zap_open(vw);
	  break;
	  
	case XK_comma: /* slower movie */
	  imcSetMovierate(vw, vw->movierate + 1);
	  /* vw->movierate++; */
	  break;
	case XK_period: /* faster movie */
	  imcSetMovierate(vw, vw->movierate - 1);
	  /*
	  vw->movierate--;
	  if (vw->movierate < 0)
	       vw->movierate = 0;
	  */
	  break;
	  
	  /* DNM 3/14/01: took out the DRAW_GL versions for readability */
	case XK_F1:
	  xcramp_level(vw->cramp, -1, 0);
	  xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
	  imod_info_setbw(vw->black, vw->white);
	  break;
	case XK_F2:
	  /* DNM: move in tandem if they're equal; xcramp_ramp takes care
	     of it in the F3 case */
	  if (vw->black == vw->white)
	       xcramp_level(vw->cramp, 1, 1);
	  else
	       xcramp_level(vw->cramp, 1, 0);
	  xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
	  imod_info_setbw(vw->black, vw->white);
	  break;
	case XK_F3:
	  xcramp_level(vw->cramp, 0, -1);
	  xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
	  imod_info_setbw(vw->black, vw->white);
	  break;
	case XK_F4:
	  xcramp_level(vw->cramp, 0, 1);
	  xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
	  imod_info_setbw(vw->black, vw->white);
	  break;
	case XK_F5:
	  xcramp_level(vw->cramp, -1, 1);
	  xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
	  imod_info_setbw(vw->black, vw->white);
	  break;
	case XK_F6:
	  /* DNM 3/14/01: don't move if they are equal */
	  if (vw->black == vw->white)
	       break;
	  xcramp_level(vw->cramp, 1, -1);
	  xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
	  imod_info_setbw(vw->black, vw->white);
	  break;
	case XK_F7:
	  xcramp_level(vw->cramp, -1, -1);
	  xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
	  imod_info_setbw(vw->black, vw->white);
	  break;
	case XK_F8:
	  xcramp_level(vw->cramp, 1, 1);
	  xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
	  imod_info_setbw(vw->black, vw->white);
	  break;
	case XK_F9:
	  xcrampSelectIndex(vw->cramp, 0);
	  xcramp_ramp(vw->cramp);
	  xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
	  imod_info_setbw(vw->black, vw->white);

	  break;
	case XK_F10:
	  xcrampSelectIndex(vw->cramp, 
			    (vw->cramp->clevel + 1) % vw->cramp->noflevels);
	  xcramp_ramp(vw->cramp);
	  xcramp_getlevels(vw->cramp, &(vw->black), &(vw->white));
	  imod_info_setbw(vw->black, vw->white);
	  break;
	case XK_F11:
	  xcramp_reverse(vw->cramp, !(vw->cramp->reverse));
	  if (App->rgba){
	      imodDraw(App->cvi, IMOD_DRAW_IMAGE);
	  }
	  break;
	case XK_F12:
	  xcramp_falsecolor(vw->cramp, !(vw->cramp->falsecolor));
	  if (App->rgba){
	      imodDraw(App->cvi, IMOD_DRAW_IMAGE);
	  }
	  break;
	  /* DNM: never knew this was here: now that there's a model clean,
	     take it out */
	  /*
	case XK_q:
	     {
		  int ob;
		  for (ob = vw->imod->objsize - 1; ob >= 0; ob--)
		       if (!vw->imod->obj[ob].contsize)
			    imodFreeObject(vw->imod, ob);
	     }
	     break;
	  */
     }
     return;
}

void defaultKeyInput(Widget w, XEvent *event, String s, Cardinal c)
{
     inputDefaultKeys((XKeyEvent *)event, XYZ_vi);
     return;
}

