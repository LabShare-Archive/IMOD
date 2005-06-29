/*
 *  imod_edit.c -- Functions for handling model structures.
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
#include "imod.h"
#include "imod_display.h"
#include "imod_edit.h"
#include "undoredo.h"

static float imod_distance( float *x, float *y, struct Mod_Point *pnt);

/* DNM 1/23/03: eliminate imod_movepoint */
/* moved point by adding x,y, and z to current model point. */


/* sets the current graphics position to the same as the current model
 * position. 
 *
 * Sets current time also.
 */

int imod_setxyzmouse()
{
  return(imod_redraw(App->cvi));
}

int imod_redraw(ImodView *vw)
{
  Imod *imod = ivwGetModel(vw);
  Iobj  *obj = NULL;
  Icont *cont = NULL;
  int   index;

  if ( (index = imod->cindex.point) < 0){
    imodDraw(vw, IMOD_DRAW_MOD);
    return(1);
  }

  cont = imodContourGet(imod);
  if (cont == NULL){
    imodDraw(vw, IMOD_DRAW_MOD);
    return(1);
  }
  if ((cont->pts == NULL) || (cont->psize <= index)){
    imodDraw(vw, IMOD_DRAW_MOD);
    return(1);
  }

  obj = imodObjectGet(imod);
  if (iobjFlagTime(obj))
    ivwSetTime(vw, cont->type);

  ivwSetLocationPoint(vw, &(cont->pts[index]));

  return(0);
}


/* DNM 1/23/02: deleted  imod_nearest, was used only by xyz window */


/* DNM 6/17/01: pass the selection size as a parameter so that windows can
   make it zoom-dependent */
float imod_obj_nearest(ImodView *vi, struct Mod_Object *obj, 
                       struct Mod_Index *index,
                       struct Mod_Point *pnt,
                       float selsize)
{
    
  struct Mod_Contour *cont;
  int i, pindex;
  float distance = -1.;
  float temp_distance;
  int ctime;
  int cz = (int)floor(pnt->z + 0.5);
  int twod = 0;
  double rad, delz;
  Ipoint scale;
    
  /* Don't report points not in our time. DNM - unless time is 0*/
  ivwGetTime(vi, &ctime);
    
  /* Ignore Z value if 2d image. */
  twod = (!(vi->dim & 4));
    
  scale.x = scale.y = 1.;
  scale.z = ((vi->imod->zscale > 0. ? vi->imod->zscale : 1.) * vi->zbin) /
    vi->xybin;

  for (i = 0; i < obj->contsize; i++){
        
    cont = &(obj->cont[i]);
    if ((ctime) && (obj->flags & IMOD_OBJFLAG_TIME) && (cont->type) &&
        (cont->type != ctime))
      continue;
    if (!cont->psize)
      continue;
        
    if ((obj->pdrawsize || cont->sizes) && !twod) {

      // If there could be 3D points, then allow attachment to any point
      // that should be visible on the plane
      for(pindex = 0; pindex < cont->psize; pindex++){
        
        if ((fabs((double)(pnt->x - cont->pts[pindex].x)) < selsize) &&
            (fabs((double)(pnt->y - cont->pts[pindex].y)) < selsize)) {
            
          // get radius of point and maximum Z difference that will work
          rad = imodPointGetSize(obj, cont, pindex) / vi->xybin;
          delz = 0.5;
          if (rad > 1.)
            delz = sqrt(rad * rad - 1.) / scale.z;

          if (fabs((double)(cont->pts[pindex].z - cz)) <= delz) {

            temp_distance = imodPoint3DScaleDistance(&(cont->pts[pindex]),
                                                     pnt, &scale);
                        
            if (distance < 0. || distance > temp_distance){
              distance = temp_distance;
              index->contour = i;
              index->point   = pindex;
            }
          }
        }
      }    

    } else {

      // Skip contour if not wild and Z does not match
      if (!twod && !(cont->flags & ICONT_WILD) && 
          ( cz != (int)floor(cont->pts->z + 0.5)))
        continue;

      for(pindex = 0; pindex < cont->psize; pindex++){
        
        if ((twod || cz == (int)floor(cont->pts[pindex].z + 0.5)) &&
            (fabs((double)(pnt->x - cont->pts[pindex].x)) < selsize) &&
            (fabs((double)(pnt->y - cont->pts[pindex].y)) < selsize)) {
            
          temp_distance = imod_distance( &(cont->pts[pindex].x),
                                         &(cont->pts[pindex].y),
                                         pnt);
          if (distance < 0. || distance > temp_distance){
            distance = temp_distance;
            index->contour = i;
            index->point   = pindex;
          }
        }
      }
    }
  }
  return(distance);
}


static float imod_distance( float *x, float *y, struct Mod_Point *pnt)
{

  double distance;
  float retval;

  distance = ((*x - pnt->x) * (*x - pnt->x)) + 
    ((*y - pnt->y) * (*y - pnt->y));
     
  distance = sqrt(distance);

  retval = (float)distance ;
     
  return(retval);
}

/* This is called when moving all contours in an object */
void imod_contour_move(int ob)
{
  /* DNM2/13/03: remove oldpt, co, cont */
  int oldob;
  int oldco;
  Iobj *obj;  
  Iobj *olObj;  
  Icont *ocont;
  ImodView *vi = App->cvi;
  Imod *imod = App->cvi->imod;
     
  oldob =  imod->cindex.object;
  oldco =  imod->cindex.contour;

  ocont = imodContourGet(imod);
  if (!ocont)
    return;
     
  if (ob == oldob)
    return;
     
  if (ob > imod->objsize)
    return;
     
  if (ob < 0)
    return;
     
  obj = &(imod->obj[ob]);
  olObj = &(imod->obj[oldob]);
  if (istoreCountContSurfItems(olObj->store, oldco, 0)) {
    vi->undo->objectPropChg(ob);
    istoreCopyContSurfItems(olObj->store, &obj->store, oldco,
                            obj->contsize, 0);
  }

  vi->undo->contourMove(imod->cindex.object, 0, ob, obj->contsize);

  /* DNM: switch to this Add and Remove method to avoid problems with
     labels */
  imodObjectAddContour(obj, ocont);
  
  imodObjectRemoveContour(olObj, oldco);
  /* DNM 3/29/01: delete old code. */
  return;
}


// Add an item to the selection list
void imodSelectionListAdd(ImodView *vi, Iindex newIndex)
{
  int multiObject = 1;    // Placekeeper for possible argument/preference
  Iindex *index;
  int i;

  // Clear list if a different object is given and multiobject not allowed
  if (ilistSize(vi->selectionList) && !multiObject) {
    index = (Iindex *)ilistFirst(vi->selectionList);
    if (index->object != newIndex.object)
      imodSelectionListClear(vi);
  }

  // create list if it does not exist
  if (!vi->selectionList) {
    vi->selectionList = ilistNew(sizeof(Iindex), 4);
    if (!vi->selectionList) 
      return;    // ERROR
  }

  // Look through list to see if contour is already there and update point
  for (i = 0; i < ilistSize(vi->selectionList); i++) {
    index = (Iindex *)ilistItem(vi->selectionList, i);
    if (index->object == newIndex.object && 
        index->contour == newIndex.contour) {
      index->point = newIndex.point;
      /*imodPrintStderr("update %d %d %d\n", newIndex.object, newIndex.contour,
        newIndex.point);*/
      return;
    }
  }

  // Add index to list
  /*imodPrintStderr("adding %d %d %d\n", newIndex.object, newIndex.contour,
    newIndex.point); */
  ilistAppend(vi->selectionList, &newIndex);
  /*index = (Iindex *)ilistFirst(vi->selectionList);
  while (index) {
    imodPrintStderr("%d %d %d\n", index->object, index->contour, index->point);
    index = (Iindex *)ilistNext(vi->selectionList);
    } */

}

// Clear the selection list: returns the number previously on the list
int imodSelectionListClear(ImodView *vi)
{
  int retval = ilistSize(vi->selectionList);
  ilistDelete(vi->selectionList);
  vi->selectionList = NULL;
  return retval;
}

// If object-contour is on selection list, return point number; otherwise -2
int imodSelectionListQuery(ImodView *vi, int ob, int co)
{
  Iindex *index;
  int i;

  for (i = 0; i < ilistSize(vi->selectionList); i++) {
    index = (Iindex *)ilistItem(vi->selectionList, i);
    if (index->object == ob && index->contour == co) {
      //imodPrintStderr("Query returns %d\n", index->point);
      return index->point;
    }
  }
  return -2;
}

void imodSelectionListRemove(ImodView *vi, int ob, int co)
{
  Iindex *index;
  int i;

  for (i = 0; i < ilistSize(vi->selectionList); i++) {
    index = (Iindex *)ilistItem(vi->selectionList, i);
    if (index->object == ob && index->contour == co) {
      ilistRemove(vi->selectionList, i);
      /* imodPrintStderr("Removing item %d, leaves %d\n", i, 
         ilistSize(vi->selectionList)); */
      return;
    }
  }
}

/*
$Log$
Revision 4.6  2005/02/24 22:34:39  mast
Switched to allowing multiple-object selections

Revision 4.5  2004/11/21 05:50:34  mast
Switch from int to float for nearest point distance measurement

Revision 4.4  2004/11/01 23:21:57  mast
Allowed 3D point selection, added selection list functions

Revision 4.3  2003/10/01 05:05:54  mast
change to rationalize location of ivw functions

Revision 4.2  2003/02/14 01:14:30  mast
cleanup unused variables

Revision 4.1  2003/02/10 20:29:00  mast
autox.cpp

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/23 23:06:00  mast
conversion to cpp

Revision 3.2.2.1  2002/12/09 17:42:32  mast
remove include of zap

Revision 3.2  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.1  2002/01/28 16:45:25  mast
Removed imod_nearest function, which was used only by xyz window and did
not work

*/
