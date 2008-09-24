/*
 *  imod_edit.c -- Functions for handling model structures.
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

#include <math.h>
#include "imod.h"
#include "imod_display.h"
#include "imod_edit.h"
#include "undoredo.h"
#include "preferences.h"

static float imod_distance( float *x, float *y, Ipoint *pnt);

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
    ivwSetTime(vw, cont->time);

  ivwSetLocationPoint(vw, &(cont->pts[index]));

  return(0);
}


/* DNM 1/23/02: deleted  imod_nearest, was used only by xyz window */


/* DNM 6/17/01: pass the selection size as a parameter so that windows can
   make it zoom-dependent */
float imod_obj_nearest(ImodView *vi, Iobj *obj, Iindex *index, Ipoint *pnt,
                       float selsize, Imat *mat)
{
    
  Icont *cont;
  int i, pindex;
  float distance = -1.;
  float temp_distance;
  int ctime;
  int cz = (int)floor(pnt->z + 0.5);
  int twod = 0;
  double rad, delz;
  Ipoint scale, pntrot, ptsrot;
    
  /* Don't report points not in our time. DNM - unless time is 0*/
  ivwGetTime(vi, &ctime);
    
  /* Ignore Z value if 2d image. */
  twod = (!(vi->dim & 4));
    
  scale.x = scale.y = 1.;
  scale.z = ((vi->imod->zscale > 0. ? vi->imod->zscale : 1.) * vi->zbin) /
    vi->xybin;

  if (mat)
    imodMatTransform3D(mat, pnt, &pntrot);

  for (i = 0; i < obj->contsize; i++){
        
    cont = &(obj->cont[i]);
    if ((ctime) && (obj->flags & IMOD_OBJFLAG_TIME) && (cont->time) &&
        (cont->time != ctime))
      continue;
    if (!cont->psize)
      continue;
        
    if (mat) {
      
      // If a matrix is supplied, rotate each point and test with rotated pnt
      for (pindex = 0; pindex < cont->psize; pindex++) {
        imodMatTransform3D(mat, &cont->pts[pindex], &ptsrot);
        if ((fabs((double)(pntrot.x - ptsrot.x)) < selsize) &&
            (fabs((double)(pntrot.y - ptsrot.y)) < selsize)) {
          delz = 0.5;

          // get radius of point and maximum Z difference that will work
          // Assume points are visible on too many sections in slicer
          if (obj->pdrawsize || cont->sizes) {
            rad = imodPointGetSize(obj, cont, pindex);
            if (rad > 1.)
              delz = sqrt(rad * rad - 1.);
          }
          if (fabs((double)(ptsrot.z - pntrot.z)) <= delz) {

            temp_distance = imodPoint3DScaleDistance(&pntrot, &ptsrot, &scale);
                        
            if (distance < 0. || distance > temp_distance){
              distance = temp_distance;
              index->contour = i;
              index->point   = pindex;
            }
          }
        }
      }

    } else if ((obj->pdrawsize || cont->sizes) && !twod) {

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

float imodAllObjNearest(ImodView *vi, Iindex *index, Ipoint *pnt,
                          float selsize, Imat *mat)
{
  Imod *imod = vi->imod;
  float temp_distance, distance = -1.;
  int i;
  Ipoint *spnt;
  
  imod->cindex.contour = -1;
  imod->cindex.point = -1;
  for (i = 0; i < imod->objsize; i++) {
    if (ImodPrefs->attachToOnObj() && iobjOff(imod->obj[i].flags))
      continue;
    index->object = i;
    temp_distance = imod_obj_nearest
      (vi, &(imod->obj[i]), index , pnt, selsize, mat);
    if (temp_distance < 0.)
      continue;
    if (distance < 0. || distance > temp_distance) {
      distance      = temp_distance;
      imod->cindex.object  = index->object;
      imod->cindex.contour = index->contour;
      imod->cindex.point   = index->point;
      spnt = imodPointGet(imod);
      if (spnt) {
        vi->xmouse = spnt->x;
        vi->ymouse = spnt->y;
        if (mat)
          vi->zmouse = spnt->z;
      }
    }
  }
  return distance;
}

static float imod_distance( float *x, float *y, Ipoint *pnt)
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

// Move all contours in current object to new object
void imodMoveAllContours(ImodView *vi, int obNew)
{
  int co;
  Imod *imod = vi->imod;
  Iobj *obj = imodObjectGet(imod);

  if (ilistSize(obj->store)) {
    vi->undo->objectPropChg();
    vi->undo->objectPropChg(obNew);
    for (co = 0; co <= obj->surfsize; co++) {
      istoreCopyContSurfItems(obj->store, &imod->obj[obNew].store, co, 
                              co, 1);
      istoreDeleteContSurf(obj->store, co, 1);
    }
  }
  /* DNM: need to set contour inside loop because each deletion
     sets it to -1; and need to not increment counter!  */
  for (co = 0; co < (int)obj->contsize; ) {
    imod->cindex.contour = 0;
    imod_contour_move(obNew);
  }
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
// If co < 0, simply tests whether object is on selection list
int imodSelectionListQuery(ImodView *vi, int ob, int co)
{
  Iindex *index;
  int i;

  for (i = 0; i < ilistSize(vi->selectionList); i++) {
    index = (Iindex *)ilistItem(vi->selectionList, i);
    if (index->object == ob && (index->contour == co || co < 0)) {
      //imodPrintStderr("Query returns %d\n", index->point);
      return index->point;
    }
  }
  return -2;
}

// Returns the number of selected objects, and the minimum and maximum
// selected object number
int imodNumSelectedObjects(ImodView *vi, int &minOb, int &maxOb)
{
  int ob, num;
  num = 0;
  for (ob = 0; ob < vi->imod->objsize; ob++) {
    if (imodSelectionListQuery(vi, ob, -1) > -2 || 
        ob == vi->imod->cindex.object) {
      if (!num)
        minOb = ob;
      num++;
      maxOb = ob;
    }
  }
  return num;
}

// Remove the given obj, cont from the selection list if it is on it
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


// Manage the selection list when there is a new current point selected
void imodSelectionNewCurPoint(ImodView *vi, Imod *imod, Iindex indSave, 
                              int controlDown)
{
  Iindex *indp;

  // If ctrl-select, then manage selection list
  if (controlDown) {
    
    // First add previous point if list is empty
    if (!ilistSize(vi->selectionList) && indSave.contour >= 0)
      imodSelectionListAdd(vi, indSave);
    
    // If point not on list, add it.  If point is on list, then remove
    // it and pop current point back to last item on list
    if (imodSelectionListQuery(vi, imod->cindex.object, 
                               imod->cindex.contour) < -1)
          imodSelectionListAdd(vi, imod->cindex);
    else {
      imodSelectionListRemove(vi, imod->cindex.object, 
                              imod->cindex.contour);
      if (ilistSize(vi->selectionList)) {
        indp = (Iindex *)ilistItem(vi->selectionList,
                                   ilistSize(vi->selectionList) - 1); 
        imod->cindex = *indp;
      }
    }
  } else

    // But if Ctrl not down, clear out the list
    imodSelectionListClear(vi);
}

/*
$Log$
Revision 4.11  2007/07/08 16:45:23  mast
Added functions to count number of selected objects and move all contours in
object to new object

Revision 4.10  2007/06/04 15:03:45  mast
Made nearest point function operate on rotated points if matrix supplied

Revision 4.9  2006/09/12 15:36:33  mast
Handled contour member renames

Revision 4.8  2006/08/31 23:27:44  mast
Changes for stored value display

Revision 4.7  2005/06/29 05:38:40  mast
Changes to manipulate fine grain properties and do undos correctly

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
