/*
 *  iobj.c -- Funtions for handeling model object structures.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 * Log at end
 */

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <float.h>

#include "imodel.h"

/*!
 * Allocates one new object initialized to default values; returns NULL if
 * error.
 */
Iobj *imodObjectNew()
{
  Iobj *obj;
  obj = imodObjectsNew(1);
  if (!obj)
    return(NULL);
  return(obj);
}

/*!
 * Allocates an array of objects of length [size], initialized to default
 * values; returns NULL if error.
 */
Iobj *imodObjectsNew(int size)
{
  Iobj *obj;
  int ob;

  obj = (Iobj *)malloc(size * sizeof(Iobj));
  if (!obj)
    return(NULL);

  for(ob = 0; ob < size; ob++){
    imodObjectDefault(&(obj[ob]));
  }
  return(obj);
}

/*!
 * Initializes the object [obj] to default values.
 */
void imodObjectDefault(Iobj *obj)
{
  int i;

  obj->cont      = NULL;
  obj->mesh      = NULL;
  for(i = 0; i < IOBJ_STRSIZE; i++)
    obj->name[i] = 0x00;
  for(i = 0; i < IOBJ_EXSIZE; i++)
    obj->extra[i] = 0;
  obj->contsize   = 0;
  obj->flags      = 0;
  obj->axis       = 0;
  obj->drawmode   = 1;
  obj->red        = 0.5f;
  obj->green      = 0.5f;
  obj->blue       = 0.5f;
  obj->pdrawsize  = 0;
  obj->symbol     = IOBJ_SYM_NONE;
  obj->symsize    = 3;
  obj->linewidth2 = 1;
  obj->linewidth  = 1;
  obj->linesty    = 0;
  obj->symflags   = 0;
  obj->sympad     = 0;
  obj->trans      = 0;
  obj->meshsize   = 0;
  obj->surfsize   = 0;
  imodClipsInitialize(&obj->clips);
     
  obj->ambient   = 102;
  obj->diffuse   = 255;
  obj->specular  = 127;
  obj->shininess = 4;
  obj->fillred = 0;
  obj->fillgreen = 0;
  obj->fillblue = 0;
  obj->quality = 0;
  obj->mat2 = 0;
  obj->valblack = 0;
  obj->valwhite = 255;
  obj->matflags2 = 0;
  obj->mat3b3 = 0;
  obj->label = NULL;
  obj->meshParam = NULL;
  obj->store = NULL;
  return;
}

/*!
 * Deletes the object [obj], freeing all contour, mesh, and label data and the
 * object structure itself.  Returns -1 if error.
 */
int imodObjectDelete(Iobj *obj)
{
  return(imodObjectsDelete(obj, 1));
}

/*!
 * Deletes the array of objects of length [size] in [obj], freeing all contour,
 * mesh, and label data and the object array itself.  It cannot be used on
 * part of an array.  Returns -1 if error.
 */
int imodObjectsDelete(Iobj *obj, int size)
{
  int ob;

  if (size < 1)
    return(-1);
  if (!obj)
    return(-1);

  for(ob = 0; ob < size; ob++){
    if (obj[ob].contsize)
      imodContoursDelete(obj[ob].cont, obj[ob].contsize);
    if (obj[ob].meshsize)
      imodMeshesDelete(obj[ob].mesh, obj[ob].meshsize);
    imodLabelDelete(obj[ob].label);
    ilistDelete(obj[ob].store);
    imeshParamsDelete(obj[ob].meshParam);
  }
  free(obj);
  return(0);
}

/*!
 * Copies object structure from object [from] to object [to].  Returns -1 if 
 * error.
 */
int imodObjectCopy(Iobj *from, Iobj *to)
{
  if (!from)
    return(-1);
  if (!to)
    return(-1);
  memcpy(to, from, sizeof(Iobj));
  return(0);
}

/*!
 * Duplicates object [obj], including all contour, mesh, and label data, and
 * returns pointer to copy, or NULL if error.
 */
Iobj *imodObjectDup(Iobj *obj)
{
  Iobj *newObj;
  Icont *cont;
  Imesh *mesh;
  int i;

  newObj = imodObjectNew();
  if (!newObj)
    return NULL;

  /* Copy object structure but zero out the count of mesh and contours in case
     we have to free it */
  imodObjectCopy(obj, newObj);
  newObj->contsize = 0;
  newObj->meshsize = 0;
  newObj->label = NULL;
  newObj->store = NULL;
  newObj->meshParam = NULL;

  /* Duplicate contours one at a time and copy into the array */
  if (obj->contsize) {
    newObj->cont = imodContoursNew(obj->contsize);
    if (!newObj->cont) {
      imodObjectDelete(newObj);
      return NULL;
    }
    for (i = 0; i < obj->contsize; i++) {
      cont = imodContourDup(&obj->cont[i]);
      if (!cont) {
        imodObjectDelete(newObj);
        return NULL;
      }
      imodContourCopy(cont, &newObj->cont[i]);
      newObj->contsize++;
      free(cont);
    }
  }

  /* Duplicate meshes similarly */
  if (obj->meshsize) {
    newObj->mesh = imodMeshesNew(obj->meshsize);
    if (!newObj->mesh) {
      imodObjectDelete(newObj);
      return NULL;
    }
    for (i = 0; i < obj->meshsize; i++) {
      mesh = imodMeshDup(&obj->mesh[i]);
      if (!mesh) {
        imodObjectDelete(newObj);
        return NULL;
      }
      imodMeshCopy(mesh, &newObj->mesh[i]);
      newObj->meshsize++;
      free(mesh);
    }
  }
   
  /* Duplicate labels and extra list items and mesh parameters */
  newObj->label = imodLabelDup(obj->label);
  newObj->store = ilistDup(obj->store);
  newObj->meshParam = imeshParamsDup(obj->meshParam);
  return newObj;
}

/* DNM 11/15/04: removed virtual in and out */

/*!
 * Returns pointer to the contour at [inIndex] in object [inObject], or NULL
 * if error.
 */
Icont *imodObjectGetContour(Iobj *inObject, int inIndex)
{
  if (inObject == NULL) return(NULL);
  if (inIndex < 0) return(NULL);
  if (inObject->cont == NULL) return(NULL);
  if (inIndex >= inObject->contsize) return(NULL);
  return(&inObject->cont[inIndex]);
}

/*!
 * Returns pointer to the mesh at [inIndex] in the mesh array of object
 * [inObject], or NULL if error.
 */
Imesh *imodObjectGetMesh(Iobj *inObject, int inIndex)
{
  if (inObject == NULL) return(NULL);
  if (inIndex < 0) return(NULL);
  if (inObject->mesh == NULL) return(NULL);
  if (inIndex >= inObject->meshsize) return(NULL);
  return(&inObject->mesh[inIndex]);
}

/*!
 * Adds the mesh pointed to by [inMesh] to the mesh array of object [inObject].
 * All pointers are transferred to the new mesh array element and no data are
 * duplicated.  If [inMesh] was allocated, such as with 
 * @imesh.html#imodMeshNew , it should be freed with free.
 * Returns index of new array element or -1 if error.
 */
int imodObjectAddMesh(Iobj *inObject, Imesh *inMesh)
{
  if (!inObject) return(-1);
  if (!inMesh) return(-1);
  inObject->mesh = imodel_mesh_add
    (inMesh, inObject->mesh, &(inObject->meshsize));
  return(inObject->meshsize - 1);
  /*    return(-1);*/
}

/*!
 * Sorts the contours in object [obj] by their z values.  If the object
 * contains time-dependent data, then the contours are sorted first by time
 * then by z.  Returns non-zero for error.
 */
/*   Sept 1996. added time value as key to sort.
 *              Empty contours no longer need to be deleted.
 */
int imodObjectSort(Iobj *obj)
{
  Icont *cont;
  Icont  tcont;
  Istore *stp;
  int i, j, sz, z, sindex;
  int st, t;

  if (obj == NULL)
    return(-1);
  if (iobjScat(obj->flags))
    return(-1);
  cont = obj->cont;

  if (cont == NULL)
    return(-1);

  for (i = 0; i < obj->contsize - 1; i++){
    sindex = i;
    if (cont[i].psize)
      sz = B3DNINT(cont[i].pts[0].z);
    else
      sz = INT_MAX;
    st = cont[i].time;

    for (j = i + 1; j < obj->contsize; j++){
      if (cont[j].psize)
        z = B3DNINT(cont[j].pts[0].z);
      else
        z = INT_MAX;
      t = cont[j].time;

      if (iobjFlagTime(obj)){
        if (st == t){
          if (sz > z){
            sz = z;
            sindex = j;
          }
        }else{
          if (st > t){
            st = t;
            sindex = j;
            sz = z;
          }
        }
            
      }else{
        if (sz > z){
          sz = z;
          sindex = j;
        }
      }
    }
    tcont        = cont[i];
    cont[i]      = cont[sindex];
    cont[sindex] = tcont;

    /* Swap any contour general store information in place */
    for (j = 0; j < ilistSize(obj->store); j++) {
      stp = istoreItem(obj->store, j);
      if (stp->flags & (GEN_STORE_NOINDEX | 3))
        break;
      if (stp->flags & GEN_STORE_SURFACE)
        continue;
      if (stp->index.i == i)
        stp->index.i = sindex;
      else if (stp->index.i == sindex)
        stp->index.i = i;
    }
  }

  /* Sort the storage list at the end */
  istoreSort(obj->store);
  return(0);
}


/*!
 * Returns volume of object [obj],computed from contour areas, or zero for a 
 * non-closed contour object
 */
/* Used by 3dmod. */
float imodObjectVolume(Iobj *obj)
{
  float ca = 0.0f;
  int co;

  if (!iobjClose(obj->flags))
    return(0);

  for(co = 0; co < obj->contsize; co++)
    ca += imodContourArea(&(obj->cont[co]));
  return(ca);
}


/*!
 * Returns centroid of object [obj] into the point [rcp].  For closed-contour
 * objects, computes centroid of area enclosed by each contour and forms the
 * overall centroid from that.  For open-contour objects, computes the centroid
 * of each contour line from the segments along the line, then forms an overall
 * centroid from that.  Does not work for scattered point objects.  Does not
 * account for Z-scaling.  Returns 1 for error computing a contour centroid or
 * if there are no contours.
 */
/* Used by Rick's imod-dist program */
int imodel_object_centroid(Iobj *obj, Ipoint *rcp)
{
  Icont *cont;
  Ipoint cpt;
  double weight, tweight = 0;
  int co;

  rcp->x = 0.0f;
  rcp->y = 0.0f;
  rcp->z = 0.0f;

  for(co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    if (!cont->psize)
      continue;
    setOrClearFlags(&cont->flags, ICONT_TEMPUSE, 
                    obj->flags & IMOD_OBJFLAG_OPEN);
    if (imodel_contour_centroid(cont, &cpt, &weight))
      return 1;
    setOrClearFlags(&cont->flags, ICONT_TEMPUSE, 0);
    tweight += weight;
    rcp->x += cpt.x;
    rcp->y += cpt.y;
    rcp->z += cpt.z;  /* z-scale is done outside of function */
  }
  if (!tweight)
    return 1;
  rcp->x /= tweight;
  rcp->y /= tweight;
  rcp->z /= tweight;
  return(0);
}

/*!
 * Adds contour [ncont] to the end of the contour array of object [obj].  
 * Pointers are copied so no data are duplicated.  If [ncont] was allocated, 
 * such as with @icont.html#imodContourNew , it should be freed with free.
 * Returns index of new contour or -1 if error.
 */
int imodObjectAddContour(Iobj *obj, Icont *ncont)
{
  if (!obj)
    return(-1);
  return (imodObjectInsertContour(obj, ncont, obj->contsize));
}

/*!
 * Inserts contour [ncont] at position [index] into the contour array of object
 * [obj].  Pointers are copied so no data are duplicated.  If [ncont] was 
 * allocated, such as with @icont.html#imodContourNew , it should be freed 
 * with free.  Returns index of new contour or -1 if error.
 */
int imodObjectInsertContour(Iobj *obj, Icont *ncont, int index)
{
  Icont *cont;
  int co;

  if (!obj || !ncont)
    return(-1);
  if (index < 0 || index > obj->contsize)
    return(-1);

  if (obj->contsize == 0)
    cont = (Icont *) malloc(sizeof(Icont));
  else
    cont = (Icont *) realloc(obj->cont, sizeof(Icont) * (obj->contsize + 1));

  if (!cont)
    return(-1);

  obj->cont = cont;
  for (co = obj->contsize - 1; co >= index; co--)
      imodContourCopy(&(obj->cont[co]), &(obj->cont[co + 1]));

  obj->contsize++;
  cont = &(obj->cont[index]);
  imodContourCopy(ncont, cont);
  if (index < obj->contsize - 1)
    istoreShiftIndex(obj->store, index, -1, 1);
  if (obj->surfsize < cont->surf)
    obj->surfsize = cont->surf;
     
  return(index);
}

/*!
 * Removes the contour at [index] from object [obj] without deleting the 
 * contour data; this is thus appropriate after adding the contour to another
 * object with @imodObjectAddContour.  Returns 1 if error.
 */
int imodObjectRemoveContour(Iobj *obj, int index)
{
  int co;

  if ((!obj) || (index < 0))
    return(1);
  if (index >= obj->contsize)
    return(1);

  istoreDeleteContSurf(obj->store, index, 0);
  obj->contsize--;
  if (obj->contsize > 0)
    for(co = index; co < obj->contsize; co++){
      imodContourCopy(&(obj->cont[co+1]), &(obj->cont[co]));
    }

  /* DMN 9/20/04: clean out labels for non-existing surfaces */
  imodObjectCleanSurf(obj);
  return(0);
}

/*!
 * Updates surface information for object [obj] by removing labels for surfaces
 * that no longer exist and recomputing the maximum surface number.
 */
void imodObjectCleanSurf(Iobj *obj)
{
  int i, co, found;
  if (!obj)
    return;

  /* Update the maximum surface number while we are at it */
  obj->surfsize = 0;
  for (co = 0; co < obj->contsize; co++)
    if (obj->surfsize < obj->cont[co].surf)
      obj->surfsize = obj->cont[co].surf;

  if (!obj->label)
    return;

  for (i = obj->label->nl - 1; i >= 0; i--) {
    found = 0;
    for (co = 0; co < obj->contsize; co++) {
      if (obj->cont[co].surf == obj->label->label[i].index) {
        found = 1;
        break;
      }
    }
    if (!found)
      imodLabelItemDelete(obj->label, obj->label->label[i].index);
  }
}

/* Unused and suspect, 3/20/05 */
Iobj *imodObjectClip(Iobj *obj, Iplane *plane, int planes)
{
  Iobj *robj = imodObjectNew();
  Icont *cc, *cont;
  Ipoint ipnt;
  int pt, lpt, ppt, tpt;
  int co;
  int laststate;

  if ((planes <= 0) || (!plane) || (!obj))
    return(NULL);

  robj = obj;

  robj->cont = NULL;
  robj->contsize = 0;

  for(co = 0; co < obj->contsize; co++){
    laststate = 0;
    cont = &(obj->cont[co]);
    cc = imodContourNew();
    lpt = cont->psize;
    if (iobjClose(obj->flags))
      lpt++;

    for(pt = 0; pt < lpt; pt++){
      ppt = tpt; /* previous point index gets old */
      tpt = pt;  /* current this point */
      if (tpt == cont->psize)
        tpt = 0;

      if(imodPlanesClip(plane, planes, &(cont->pts[tpt]))){
        imodPointAppend(cc, &(cont->pts[tpt]));
        if (laststate == 2){
          if (cc->psize){
            imodObjectAddContour(robj, cc);
            free(cc);
            cc = imodContourNew();
          }
          imodPointPlaneEdge
            (&ipnt, plane, planes, 
             &(cont->pts[tpt]), &(cont->pts[ppt]));
          imodPointAppend(cc, &ipnt);
        }
        laststate = 1;
        continue;
      }
      if (laststate == 1){
        imodPointPlaneEdge
          (&ipnt, plane, planes, 
           &(cont->pts[tpt]), &(cont->pts[ppt]));
        imodPointAppend(cc, &ipnt);
      }
      laststate = 2;
    }
    imodObjectAddContour(robj, cc);
    free(cc);
  }
  robj->flags |= IMOD_OBJFLAG_OPEN;
  return(robj);
}

/*!
 * Finds the minimum and maximum coordinates of all contours in object [obj]
 * and returns them in points [ll] and [ur], respectively.  If there are no
 * contours but there are meshes, it finds the bounding box from all meshes
 * instead.  Returns -1 for an error (including no contours or meshes), 0 for
 * values from contours, or 1 for values from meshes.
 */
int imodObjectGetBBox(Iobj *obj, Ipoint *ll, Ipoint *ur)
{
  Ipoint min;
  Ipoint max;
  Icont *cont;
  int co;

  min.x = min.y = min.z = FLT_MAX;
  max.x = max.y = max.z = - FLT_MAX;

  if ((!obj) || (!ll) || (!ur))
    return(-1);
  if (!obj->contsize && !obj->meshsize)
    return(-1);

  *ll = min;
  *ur = max;
  for(co = 0; co < obj->contsize; co++){
    if (imodContourGetBBox(&obj->cont[co], &min, &max))
      continue;

    if (min.x < ll->x) ll->x = min.x;
    if (min.y < ll->y) ll->y = min.y;
    if (min.z < ll->z) ll->z = min.z;

    if (max.x > ur->x) ur->x = max.x;
    if (max.y > ur->y) ur->y = max.y;
    if (max.z > ur->z) ur->z = max.z;
      
  }

  if (obj->contsize)
    return(0);

  for(co = 0; co < obj->meshsize; co++){
    if (imodMeshGetBBox(&obj->mesh[co], &min, &max))
      continue;

    if (min.x < ll->x) ll->x = min.x;
    if (min.y < ll->y) ll->y = min.y;
    if (min.z < ll->z) ll->z = min.z;

    if (max.x > ur->x) ur->x = max.x;
    if (max.y > ur->y) ur->y = max.y;
    if (max.z > ur->z) ur->z = max.z;
      
  }
  return(1);
}

/*!
 * Returns color values of object [inObject].
 */
void  imodObjectGetColor(Iobj *inObject,
                         float *outRed, float *outGreen, float *outBlue)
{
  *outRed   = inObject->red;
  *outGreen = inObject->green;
  *outBlue  = inObject->blue;
}

/*!
 * Sets the color of object [inObject] to the given values.
 */
void  imodObjectSetColor(Iobj *inObject,
                         float inRed, float inGreen, float inBlue)
{
  inObject->red = inRed;
  inObject->green = inGreen;
  inObject->blue = inBlue;
}

/*!
 * Returns number of contours in object [inObject] (not the maximum contour 
 * index).
 */
int   imodObjectGetMaxContour(Iobj *inObject)
{
  if (!inObject) return(0);
  return(inObject->contsize);
}

/*!
 * Returns pointer to object name.
 */
char *imodObjectGetName(Iobj *inObject)
{
  if (!inObject) return(0);
  return(inObject->name);
}

/*!
 * Sets name of object [obj] to [inName].  Returns 1 if the string is 
 * truncated to IOBJ_STRSIZE - 1 (63).
 */
int imodObjectSetName(Iobj *obj, char *inName)
{
  int i, len, retval = 0;
  for(i = 0; i < IOBJ_STRSIZE; i++)
    obj->name[i] = 0x00;
  len = strlen(inName);
  if (len > (IOBJ_STRSIZE - 1)){
    len = IOBJ_STRSIZE - 1;
    retval++;
  }
  for(i = 0; i < len; i++)
    obj->name[i] = inName[i];
  return(retval);
}


/*!
 * Returns one value for [inObject]; possible values for [inValueType] are
 * {IobjMaxContour}, {IobjLineWidth}, {IobjLineWidth2}, {IobjPointSize},
 * {IobjMaxMesh}, {IobjMaxSurface}, {IobjFlagClosed}, {IobjFlagConnected}, 
 * {IobjFlagFilled}, {IobjFlagDraw}, {IobjFlagMesh}, {IobjFlagLine},
 * {IobjFlagTime}, and {IobjFlagExtraInModv}.  {IobjFlagConnected} is 0 for 
 * scattered point objects.
 */
int   imodObjectGetValue(Iobj *inObject, int inValueType)
{
  switch(inValueType){

  case IobjMaxContour:
    return(inObject->contsize);
  case IobjLineWidth:
    return(inObject->linewidth);
  case IobjLineWidth2:
    return(inObject->linewidth2);
  case IobjPointSize:
    return(inObject->pdrawsize);
  case IobjMaxMesh:
    return(inObject->meshsize);
  case IobjMaxSurface:
    return(inObject->surfsize);
      

  case IobjFlagClosed:
    return(iobjClose(inObject->flags));

  case IobjFlagConnected:
    return(!(iobjScat(inObject->flags)));

  case IobjFlagFilled:
    return(iobjFill(inObject->flags));

  case IobjFlagDraw:
    return(!(iobjDraw(inObject->flags)));

  case IobjFlagMesh:
    return(iobjMesh(inObject->flags));

  case IobjFlagLine:
    return(!(iobjLine(inObject->flags)));

  case IobjFlagTime:
    return(iobjFlagTime(inObject));

  case IobjFlagExtraInModv:
    return(inObject->flags & IMOD_OBJFLAG_EXTRA_MODV);

  default:
    return(0);
  }
}

static void setObjFlag(Iobj *inObject, b3dUInt32 flag, int state)
{
  if (!inObject) return;
  if (state){
    inObject->flags |= flag;
  }else{
    inObject->flags &= ~flag;
  }
}

/*!
 * Sets one value for [inObject] to [inValue]; possible values for
 * [inValueType] are {IobjLineWidth}, {IobjLineWidth2}, {IobjPointSize}, 
 * {IobjFlagClosed}, {IobjFlagConnected}, {IobjFlagFilled}, {IobjFlagDraw}, 
 * {IobjFlagMesh}, {IobjFlagLine}, and {IobjFlagExtraInModv} (for drawing an 
 * extra object in model view window).
 */
void  imodObjectSetValue(Iobj *inObject, int inValueType, int inValue)
{
  switch(inValueType){
  case IobjLineWidth:
    inObject->linewidth = inValue;
    return;

  case IobjLineWidth2:
    inObject->linewidth2 = inValue;
    return;

  case IobjPointSize:
    inObject->pdrawsize = inValue;
    return;

  case IobjFlagClosed:
    setObjFlag(inObject, IMOD_OBJFLAG_OPEN, !inValue);
    return;

  case IobjFlagConnected:
    setObjFlag(inObject, IMOD_OBJFLAG_SCAT, !inValue);
    return;

  case IobjFlagFilled:
    setObjFlag(inObject, IMOD_OBJFLAG_FILL, inValue);
    return;

  case IobjFlagDraw:
    setObjFlag(inObject, IMOD_OBJFLAG_OFF, !inValue);
    return;

  case IobjFlagMesh:
    setObjFlag(inObject, IMOD_OBJFLAG_MESH, inValue);
    return;

  case IobjFlagLine:
    setObjFlag(inObject, IMOD_OBJFLAG_NOLINE, !inValue);
    return;

  case IobjFlagExtraInModv:
    setObjFlag(inObject, IMOD_OBJFLAG_EXTRA_MODV, inValue);

  }
}

/*

$Log$
Revision 3.22  2009/02/24 18:01:50  mast
Brought object centroid routine up to snuff and workable for open obj

Revision 3.21  2008/12/09 23:26:48  mast
Changed flag from line to noline

Revision 3.20  2008/05/07 04:44:20  mast
Made bounding box function measure meshes if no contours

Revision 3.19  2008/04/24 18:50:22  mast
Added support for plugin to change 2D line width

Revision 3.18  2008/04/04 21:21:04  mast
Free contour after adding to object, clarify documentation

Revision 3.17  2008/03/05 20:08:00  mast
Added ability to set flag for drawing extra object in model view

Revision 3.16  2007/12/06 20:13:21  mast
Allowed sort of open contour objects

Revision 3.15  2007/09/22 00:09:37  mast
rename mat3b2

Revision 3.14  2006/09/12 15:23:14  mast
Handled mesh parameters and member renames

Revision 3.13  2006/08/31 21:11:29  mast
Changed mat1 and mt3 to real names

Revision 3.12  2005/10/06 19:52:10  mast
Maintained surfsize in contour insert function

Revision 3.11  2005/09/11 19:15:26  mast
Managed contour store info when sorting contours

Revision 3.10  2005/06/29 05:35:32  mast
Changed a store function call

Revision 3.9  2005/06/26 19:32:22  mast
Manage storage lists when inserting or removing contours

Revision 3.8  2005/05/27 04:53:33  mast
Make surface cleaning always compute surfsize.

Revision 3.7  2005/04/23 23:36:54  mast
Moved some functions to imodel.c

Revision 3.6  2005/03/20 19:56:43  mast
Documenting and eliminating duplicate functions

Revision 3.5  2004/11/20 04:42:10  mast
Added duplicate and insert contours, functions, removed virtual stuff

Revision 3.4  2004/11/05 18:53:00  mast
Include local files with quotes, not brackets

Revision 3.3  2004/09/21 20:12:48  mast
Added function to maintain surface labels and max surface number

Revision 3.2  2003/06/27 20:16:26  mast
Added functions to get specific contour in object and to set object color,
fixed problem with setting closed property, made sure mat bytes are all zeroed

Revision 3.1  2003/02/21 22:22:02  mast
Use new b3d types

*/
