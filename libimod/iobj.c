/*
 *  iobj.c -- Funtions for handeling model object structures.
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

$Log$
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
  obj->mat1 = 0;
  obj->mat1b1 = 0;
  obj->mat1b2 = 0;
  obj->mat1b3 = 0;
  obj->mat2 = 0;
  obj->mat3 = 0;
  obj->mat3b1 = 0;
  obj->mat3b2 = 0;
  obj->mat3b3 = 0;
  obj->label = NULL;
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
   
  /* Duplicate labels and extra list items */
  newObj->label = imodLabelDup(obj->label);
  newObj->store = ilistDup(obj->store);
  return newObj;
}

/* DNM 11/15/04: removed virtual in and out */

/*!
 * Returns a pointer to the current object in model [imod], or NULL if no legal
 * object is selected.
 */
Iobj *imodObjectGet(Imod  *imod)
{
  if (!imod)
    return(NULL);
  if (imod->cindex.object < 0 || imod->cindex.object >= imod->objsize)
    return( (Iobj *)NULL);
  return( &(imod->obj[imod->cindex.object]));
}

/*!
 * Sets first object in model [imod] as current object and returns pointer to
 * it, or NULL if error.
 */
Iobj *imodObjectGetFirst(Imod *imod)
{
  int ob, co, pt;

  if (!imod) return(NULL);
  imodGetIndex(imod, &ob, &co, &pt);
  imodSetIndex(imod, 0, co, pt);
  return(imodObjectGet(imod));
}

/*!
 * Advances the current object index by one in model [imod] and returns pointer
 * to new current object, or NULL if error or if the existing current object is
 * the last one in the model.
 */
Iobj *imodObjectGetNext(Imod *imod)
{
  int ob, co, pt;

  if (!imod) return(NULL);
  imodGetIndex(imod, &ob, &co, &pt);
  ob++;
  if (ob >= imod->objsize)
    return(NULL);
  imodSetIndex(imod, ob, co, pt);
  return(imodObjectGet(imod));
}

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
 * duplicated.  Returns index of new array element or -1 if error.
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
  int i, j, sz, z, sindex;
  int st, t;

  if (obj == NULL)
    return(-1);
  if (obj->flags & IMOD_OBJFLAG_OPEN)
    return(-1);
  cont = obj->cont;

  if (cont == NULL)
    return(-1);

  /* Delete Empty contours. */
  /*
    for (i = 0; i < obj->contsize; i++){
    if (cont[i].psize)
    continue;
    obj->contsize--;
    sindex = obj->contsize;
    tcont        = cont[i];
    cont[i]      = cont[sindex];
    cont[sindex] = tcont;
    }
  */

  for (i = 0; i < obj->contsize - 1; i++){
    sindex = i;
    if (cont[i].psize)
      sz = cont[i].pts[0].z;
    else
      sz = INT_MAX;
    st = cont[i].type;

    for (j = i + 1; j < obj->contsize; j++){
      if (cont[j].psize)
        z = cont[j].pts[0].z;
      else
        z = INT_MAX;
      t = cont[j].type;

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

  }
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


/* testing, internal only (unused 3/20/05) */
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
    imodel_contour_centroid(cont, &cpt, &weight);
    tweight += weight;
    rcp->x += cpt.x;
    rcp->y += cpt.y;
    rcp->z += cpt.z;  /* z-scale is done outside of function */
  }
  rcp->x /= tweight;
  rcp->y /= tweight;
  rcp->z /= tweight;
  return(0);
}

/*!
 * Adds contour [ncont] to the end of the contour array of object [obj].  
 * Pointers are copied so no data are duplicated.  Returns index of new
 * contour or -1 if error.
 */
int imodObjectAddContour(Iobj *obj, Icont *ncont)
{
  if (!obj)
    return(-1);
  return (imodObjectInsertContour(obj, ncont, obj->contsize));
}

/*!
 * Inserts contour [ncont] at position [index] into the contour array of object
 * [obj].  Pointers are copied so no data are duplicated.  Returns index of new
 * contour or -1 if error.
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
     
  return(index);
}

/*!
 * Removes the contour at [index] from object [obj] without deleting the 
 * contour data.  Returns 1 if error.
 */
int imodObjectRemoveContour(Iobj *obj, int index)
{
  int co;

  if ((!obj) || (index < 0))
    return(1);
  if (index >= obj->contsize)
    return(1);

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
  if (!obj || !obj->label)
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

  /* Update the maximum surface number while we are at it */
  obj->surfsize = 0;
  for (co = 0; co < obj->contsize; co++)
    if (obj->surfsize < obj->cont[co].surf)
      obj->surfsize = obj->cont[co].surf;
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
  }
  robj->flags |= IMOD_OBJFLAG_OPEN;
  return(robj);
}

/*!
 * Finds the minimum and maximum coordinates of all contours in object [obj]
 * and returns them in points [ll] and [ur], respectively.
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
  if (!obj->contsize)
    return(1);

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

  return(0);
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
 * Sets name of object [obj] to [inName].
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
 * {IobjMaxContour}, {IobjLineWidth}, {IobjPointSize}, {IobjMaxMesh}, 
 * {IobjMaxSurface}, {IobjFlagClosed}, {IobjFlagConnected}, {IobjFlagFilled},
 * {IobjFlagDraw}, {IobjFlagMesh}, {IobjFlagLine}, and {IobjFlagTime}.
 */
int   imodObjectGetValue(Iobj *inObject, int inValueType)
{
  switch(inValueType){

  case IobjMaxContour:
    return(inObject->contsize);
  case IobjLineWidth:
    return(inObject->linewidth);
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
 * [inValueType] are {IobjLineWidth}, {IobjPointSize}, {IobjFlagClosed},
 * {IobjFlagConnected}, {IobjFlagFilled}, {IobjFlagDraw}, {IobjFlagMesh},
 * and {IobjFlagLine}.
 */
void  imodObjectSetValue(Iobj *inObject, int inValueType, int inValue)
{
  switch(inValueType){
  case IobjLineWidth:
    inObject->linewidth = inValue;
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
    setObjFlag(inObject, IMOD_OBJFLAG_LINE, !inValue);
    return;

  }
}

