/*
 *  imod_edit.cpp -- Functions for handling model structures.
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
#include <set>
#include "imod.h"
#include "xxyz.h"
#include "display.h"
#include "imod_input.h"
#include "imod_edit.h"
#include "undoredo.h"
#include "preferences.h"

static float imod_distance( float *x, float *y, Ipoint *pnt);
static float contourSublength(Icont *cont, int p1, int p2);

/* DNM 1/23/03: eliminate imod_movepoint */
/* moved point by adding x,y, and z to current model point. */

/* Tests whether the contour is in the selection area.  Either it must be 
   entirely within the area, or it must be an open wild contour and have points
   on the current section that are all within the area */
int imodContInSelectArea(Iobj *obj, Icont *cont, Ipoint selmin, Ipoint selmax)
{
  Ipoint pmin, pmax;
  int pt, inRange = 0;
  Ipoint *pnt;

  imodContourGetBBox(cont, &pmin, &pmax);
  if (pmin.x >= selmin.x && pmax.x <= selmax.x &&
      pmin.y >= selmin.y && pmax.y <= selmax.y &&
      pmin.z >= selmin.z && pmax.z <= selmax.z)
    return 1;
  if (!(iobjOpen(obj->flags) && (cont->flags & ICONT_WILD)))
    return 0;

  // Wild open contour is no good if a point in the Z range is outside the
  // X/Y range
  for (pt = 0; pt < cont->psize; pt++) {
    pnt = &cont->pts[pt];
    if (pnt->z >= selmin.z && pnt->z <= selmax.z) {
      inRange = 1;
      if (pnt->x < selmin.x || pnt->x > selmax.x ||
          pnt->y < selmin.y || pnt->y > selmax.y)
        return 0;
    }
  }
  return inRange;
}

/* Tests whether the contour is inside another one, outer.  Either it must be 
   entirely within the outer one, or it must be an open wild contour and have points
   on the current section that are all within the outer one */
int imodContInsideCont(Iobj *obj, Icont *cont, Icont *outer, float zmin, float zmax)
{
  int pt, inRange, inside;
  Ipoint *pnt;
  bool onSec;
  bool wildOpen = iobjOpen(obj->flags) && (cont->flags & ICONT_WILD);

  inRange = wildOpen ? 0 : 1;
  for (pt = 0; pt < cont->psize; pt++) {
    pnt = &cont->pts[pt];
    onSec = pnt->z >= zmin && pnt->z <= zmax;
    inside = imodPointInsideCont(outer, pnt);
    if (!inside || !onSec) {
      // Wild open contour is no good if a point in the Z range is outside the contour
      if (!wildOpen || onSec)
        return 0;
    } else if (wildOpen)
      inRange = 1;
  }
  return inRange;
}


/* DNM 1/23/02: deleted  imod_nearest, was used only by xyz window */


/* DNM 6/17/01: pass the selection size as a parameter so that windows can
   make it zoom-dependent */
float imod_obj_nearest(ImodView *vi, Iobj *obj, Iindex *index, Ipoint *pnt,
                       float selsize, int ctime, Imat *mat)
{
    
  Icont *cont;
  int i, pindex;
  float distance = -1.;
  float temp_distance;
  int cz = (int)floor(pnt->z + 0.5);
  int twod = 0;
  double rad, delz;
  Ipoint scale, pntrot, ptsrot;
    
  /* Ignore Z value if 2d image. */
  twod = (!(vi->dim & 4));
    
  scale.x = scale.y = 1.;
  scale.z = ((vi->imod->zscale > 0. ? vi->imod->zscale : 1.) * vi->zbin) /
    vi->xybin;

  if (mat)
    imodMatTransform3D(mat, pnt, &pntrot);

  for (i = 0; i < obj->contsize; i++){
        
    cont = &(obj->cont[i]);

  /* Don't report points not in our time. DNM - unless time is 0*/
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
                        float selsize, int ctime, Imat *mat)
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
      (vi, &(imod->obj[i]), index , pnt, selsize, ctime, mat);
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

/* This used to be called when moving all contours in an object but not anymore */
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

  // Just record an object property change since there may be max surface number changes
  // as well as the store info
  vi->undo->objectPropChg();
  vi->undo->objectPropChg(obNew);

  // If object has mesh but no contours, transfers the meshes
  if (!obj->contsize && obj->meshsize) {
    for (co = 0; co < obj->meshsize; co++)
      imod->obj[obNew].mesh = imodel_mesh_add(&obj->mesh[co], imod->obj[obNew].mesh,
                                              &imod->obj[obNew].meshsize);
    return;
  }

  // Move all the fine grain information over then delete the store
  if (ilistSize(obj->store)) {
    for (co = 0; co <= obj->surfsize; co++)
      istoreCopyContSurfItems(obj->store, &imod->obj[obNew].store, co, co, 1);
    for (co = 0; co < obj->contsize; co++)
      istoreCopyContSurfItems(obj->store, &imod->obj[obNew].store, co, 
                                    imod->obj[obNew].contsize + co, 0);
    ilistDelete(obj->store);
    obj->store = NULL;
  }

  // Record all contours as moving
  vi->undo->allContourMove(imod->cindex.object, obNew);
  
  /* DNM: need to set contour inside loop because each deletion
     sets it to -1; and need to not increment counter!  */
  for (co = 0; co < (int)obj->contsize; ) {
    imod->cindex.contour = 0;
    imodObjectAddContour(&imod->obj[obNew], &obj->cont[co]);
    imodObjectRemoveContour(obj, co);
  }
}

/*
 * Looks for places where the contour crosses itself and removes the shorter part of the
 * contour in each case (but for an open contour, it always removes the part of the 
 * contour between the crossings even if it is long.)  Uses imodContourBreak so it 
 * should be able to preserve sizes and istores, but has only been tested on lasso 
 * contour.
 */
void imodTrimContourLoops(Icont *cont, int openObj)
{
  int sega, segb, nextb, swap, numSeg, innerLim;
  Icont *newCont;
  Icont tmpCont;
  Ipoint *pts;
  int foundLoop = 1;
  int openCont = (openObj || (cont->flags & ICONT_OPEN)) ? 1 : 0;

  /* Repeat until no more crossings are found */
  while (foundLoop) {
    foundLoop = 0;

    /* Going to loop on pairs of non-adjacent contours, so for closed contour, the first
       time it has to stop before end; thereafter it has to go to end.
    */
    numSeg = cont->psize - (openCont ? 1 : 0);
    innerLim = numSeg - (openCont ? 0 : 1);
    pts = cont->pts;
    for (sega = 0; sega < numSeg - 2; sega++) {
      for (segb = sega + 2; segb < innerLim; segb++) {
        nextb = (segb + 1) % cont->psize;
        if (imodPointIntersect(&pts[sega], &pts[sega + 1], &pts[segb], &pts[nextb])) {

          /* If contour is closed, see if piece to be trimmed out is actually longer
             and retain that piece instead */
          swap = (!openCont && contourSublength(cont, sega+1, segb) > 
                      contourSublength(cont, nextb, sega)) ? 1 : 0;
          newCont = imodContourBreak(cont, sega + 1, segb);
          if (!newCont)
            return;
          if (swap) {
            imodContourCopy(cont, &tmpCont);
            imodContourCopy(newCont, cont);
            imodContourCopy(&tmpCont, newCont);
          }
          imodContourDelete(newCont);
          foundLoop = 1;
          break;
        }
      }
      innerLim = numSeg;
      if (foundLoop)
        break;
    }
  }
}

/*
 * Computes the 2D length of a contour from p1 through p2, going forward through the 
 * points. If p2 is before p1, then it will go around through 0.
 */
static float contourSublength(Icont *cont, int p1, int p2)
{
  int pt, pnext, i;
  double lsum = 0.;
  if (p2 < p1)
    p2 += cont->psize;
  pt = p1;
  for (i = p1; i <= p2; i++) {
    pnext = (pt + 1) % cont->psize;
    lsum += imodPointDistance(&cont->pts[pt], &cont->pts[pnext]);
    pt = pnext;
  }
  return (float)lsum;
}

static void dumpSelectionList(ImodView *vi) 
{
  if (!imodDebug('S'))
    return;
  imodPrintStderr("%d on list:\n", ilistSize(vi->selectionList));
  Iindex *index = (Iindex *)ilistFirst(vi->selectionList);
  while (index) {
    imodPrintStderr("%d %d %d\n", index->object, index->contour, index->point);
    index = (Iindex *)ilistNext(vi->selectionList);
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
      imodTrace('S', "update %d %d %d", newIndex.object, newIndex.contour,
        newIndex.point);
      return;
    }
  }

  // Add index to list
  imodTrace('S', "adding %d %d %d", newIndex.object, newIndex.contour, newIndex.point);
  ilistAppend(vi->selectionList, &newIndex);
  dumpSelectionList(vi);
}

// Clear the selection list: returns the number previously on the list
int imodSelectionListClear(ImodView *vi)
{
  int retval = ilistSize(vi->selectionList);
  ilistDelete(vi->selectionList);
  vi->selectionList = NULL;
  imodTrace('S', "List cleared");
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
      imodTrace('S', "Query returns %d", index->point);
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
      imodTrace('S', "Removing item %d, leaves %d", i, ilistSize(vi->selectionList));
      return;
    }
  }
}


// Manage the selection list when there is a new current point selected
void imodSelectionNewCurPoint(ImodView *vi, Imod *imod, Iindex indSave, 
                              int controlDown)
{
  Iindex *indp;
  bool contourless;

  // If ctrl-select, then manage selection list
  if (controlDown) {
    contourless = indSave.object >= 0 && !vi->imod->obj[indSave.object].contsize && 
      vi->imod->obj[indSave.object].meshsize;
    
    // First add previous point if list is empty
    if (!ilistSize(vi->selectionList) && (indSave.contour >= 0 || contourless))
      imodSelectionListAdd(vi, indSave);
    
    // If point not on list, add it.  If point is on list, then remove
    // it and pop current point back to last item on list
    if (imodSelectionListQuery(vi, imod->cindex.object, imod->cindex.contour) < -1) {
      imodSelectionListAdd(vi, imod->cindex);
    } else {
      imodSelectionListRemove(vi, imod->cindex.object, imod->cindex.contour);
      if (ilistSize(vi->selectionList)) {
        indp = (Iindex *)ilistItem(vi->selectionList, ilistSize(vi->selectionList) - 1); 
        imod->cindex = *indp;
      }
    }
  } else

    // But if Ctrl not down, clear out the list
    imodSelectionListClear(vi);
}

/* 
 * Returns true if a contour has at least 2 points and is coplanar in the given plane
 */
bool imodContourIsPlanar(Icont *cont, int plane)
{
  int first, pt;
  if (cont->psize < 2)
    return false;
  if (plane == Z_SLICE_BOX)
    return !(cont->flags & ICONT_WILD);
  if (plane == X_SLICE_BOX) {
    first = B3DNINT(cont->pts[0].x);
    for (pt = 1; pt < cont->psize; pt++)
      if (B3DNINT(cont->pts[pt].x) != first)
        return false;
  } else {
    first = B3DNINT(cont->pts[0].y);
    for (pt = 1; pt < cont->psize; pt++)
      if (B3DNINT(cont->pts[pt].y) != first)
        return false;
  }
  return true;
}

/*
 * Checks the assigned surface number of an optional contour then checks the surfaces of
 * all contours, working backwards, to find one that has existing planar contours.
 * If none is found, it accepts the current one or returns -1 if a new one is needed.
 */
int imodCheckSurfForNewCont(Iobj *obj, Icont *cont, int time, int plane)
{
  std::set<int> checkedSurfs;
  int coNum;
  int curSurf = cont ? cont->surf : -1;
  for (coNum = obj->contsize; coNum >= 0; coNum--) {

    // Loop backwards through object, but first check supplied contour if any
    if (coNum < obj->contsize)
      cont = &obj->cont[coNum];
    if (!cont || cont->time != time || checkedSurfs.count(cont->surf))
      continue;

    // If contour is planar, add surface to list of checked ones, then see if whole 
    // surface is planar.  If so, that is the surface
    if (imodContourIsPlanar(cont, plane)) {
      checkedSurfs.insert(cont->surf);
      if (imodSurfaceIsPlanar(obj, cont->surf, time, plane) > 0) {
        imodTrace('P', "imodCheckSurfForNewCont found surface %d OK for plane %d",
                  cont->surf, plane);
        return cont->surf;
      }
    }
  }

  // For Z plane, can we just use the 0 surface?
  if (plane == Z_SLICE_BOX && imodSurfaceIsPlanar(obj, 0, time, plane) != 0) {
    imodTrace('P', "imodCheckSurfForNewCont 0 OK for Z plane");
    return 0;
  }

  // For other planes, can we use the existing surface number > 0?
  if (plane != Z_SLICE_BOX && curSurf > 0 && imodSurfaceIsPlanar(obj, curSurf, time,
                                                                 plane) != 0) {
    imodTrace('P', "imodCheckSurfForNewCont %d OK for non-Z plane", curSurf);
    return curSurf;
  }

  // Otherwise, need a new surface
  imodTrace('P', "imodCheckSurfForNewCont -1: need new surf");
  return -1;
}

/* 
 * Check each contour in surface with > 1 point and return 0 if any is non-planar,
 * return 1 if all are planar, or return -1 if there are no non-planar ones
 */
int imodSurfaceIsPlanar(Iobj *obj, int surface, int time, int plane)
{
  Icont *cont;
  int coNum, retval = -1;
  for (coNum = 0; coNum < obj->contsize; coNum++) {
    cont = &obj->cont[coNum];
    if (cont->psize > 1 && cont->surf == surface && cont->time == time) {
      if (!imodContourIsPlanar(cont, plane)) {
        retval = 0;
        break;
      }
      retval = 1;
    }
  }
  imodTrace('P', "surfaceIsPlanar surf %d time %d plane %d return %d", surface, time,
            plane, retval);
  return retval;
}
