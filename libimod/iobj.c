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
  obj->vertBufCont = NULL;
  obj->vertBufSphere = NULL;
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
 * Computes a checksum from the coordinates, properties, and general storage information
 * for object [obj] and its contours.
 */
double imodObjectChecksum(Iobj *obj, int obNum)
{
  int i, co, pt;
  Icont *cont;
  IclipPlanes *clips;
  double osum, psum;
  osum = obNum;
  psum = 0.;
  osum += obj->red + obj->green + obj->blue;
  osum += obj->flags;
  osum += obj->pdrawsize;
  osum += obj->symbol;
  osum += obj->symsize;
  osum += obj->linewidth2;
  osum += obj->linewidth;
  osum += obj->symflags;
  osum += obj->trans;     
  osum += obj->contsize;      
  osum += obj->ambient + obj->diffuse + obj->specular + obj->shininess;
  clips = &obj->clips;
  osum += clips->count + clips->flags + clips->trans;
  osum += clips->plane + obj->mat2;
  for (i = 0; i < clips->count; i++) {
    osum += clips->normal[i].x + clips->normal[i].y + 
      clips->normal[i].z;
    osum += clips->point[i].x + clips->point[i].y + 
      clips->point[i].z;
  }
  osum += obj->extra[IOBJ_EX_PNT_LIMIT] + obj->extra[IOBJ_EX_2D_TRANS];
  osum += obj->fillred + obj->fillgreen + obj->fillblue + obj->quality;
  osum += obj->valblack + obj->valwhite + obj->matflags2 + obj->mat3b3;
  osum += istoreChecksum(obj->store);
  for(co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    psum += cont->surf;
    psum += cont->psize;
    psum += cont->flags & ~ICONT_WILD & ~ICONT_TEMPUSE;
    psum += cont->time;
    for(pt = 0; pt < cont->psize; pt++){
      psum += cont->pts[pt].x;
      psum += cont->pts[pt].y;
      psum += cont->pts[pt].z;
    }
    psum += istoreChecksum(cont->store);
    if (cont->sizes)
      for(pt = 0; pt < cont->psize; pt++)
        psum += cont->sizes[pt];
  }
  return osum + psum;
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
 * Copies object structure from object [from] to object [to] then sets pointers to NULL
 * and contour and mesh sizes to 0.  Returns -1 if error.
 */
int imodObjectCopyClear(Iobj *from, Iobj *to)
{
  if (imodObjectCopy(from, to))
    return -1;
  to->contsize = 0;
  to->meshsize = 0;
  to->label = NULL;
  to->store = NULL;
  to->meshParam = NULL;
  to->vertBufSphere = NULL;
  to->vertBufCont = NULL;
  return 0;
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

  if (!obj)
    return NULL;

  newObj = imodObjectNew();
  if (!newObj)
    return NULL;

  /* Copy object structure but zero out the count of mesh and contours in case
     we have to free it */
  imodObjectCopyClear(obj, newObj);

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
 * @@imesh.html#imodMeshNew@ , it should be freed with free.
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
 * such as with @@icont.html#imodContourNew@ , it should be freed with free.
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
 * allocated, such as with @@icont.html#imodContourNew@ , it should be freed 
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
 * object with @@imodObjectAddContour@.  Returns 1 if error.
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

/*!
 * Analyzes the mesh of object [obj] to determine which contours are connected by the 
 * mesh and assigns a separate surface number to each set of separately connected 
 * contours.  Returns 1 if there is no mesh information or 2 for memory error.
 */
int imodObjectSortSurf(Iobj *obj)
{
  int npoly, me, i, j, poly, ninpoly, iwork, nwork, scan, found;
  int refvert, work, nsurfs, co, pt, ind, resol, vertBase, normAdd;
  int *listp, *surfs, *meshes, *nverts, *towork, *refp, *scanp, *lincs;
  int **starts;
  float *zmins, *zmaxs;
  Ipoint *vertp;
  float zmin, zmax, ptx, pty, ptz;

  imodMeshNearestRes(obj->mesh, obj->meshsize, 0, &resol);

  /* Count polygons in all meshes based on start flags */

  npoly = 0;
  for (me = 0; me < obj->meshsize; me++) {
    if (imeshResol(obj->mesh[me].flag) == resol) {
      listp = obj->mesh[me].list;
      for (i = 0; i < obj->mesh[me].lsize; i++) {
        if (*listp == IMOD_MESH_BGNPOLYNORM || 
            *listp == IMOD_MESH_BGNPOLYNORM2)
          npoly++;
        listp++;
      }
    }
  }

  if (!npoly)
    return (1);
     
  /* Get arrays for Z values and surface assignment for each polygon, which
     mesh it is in, address of start of polygons, for number of
     vertices in the polygon, and for list of polys to work on */

  zmins = (float *)malloc (npoly * sizeof(float));
  zmaxs = (float *)malloc (npoly * sizeof(float));
  surfs = (int *)malloc (npoly * sizeof(int));
  meshes = (int *)malloc (npoly * sizeof(int));
  starts = (int **)malloc (npoly * sizeof(int *));
  nverts = (int *)malloc (npoly * sizeof(int));
  towork = (int *)malloc (npoly * sizeof(int));
  lincs = (int *)malloc ((npoly + 2) * sizeof(int));
     
  if (!zmins || !zmaxs || !surfs || !meshes || !starts || !nverts || 
      !towork || !lincs) {
    if (zmins) free(zmins);
    if (zmaxs) free(zmaxs);
    if (meshes) free(meshes);
    if (surfs) free(surfs);
    if (starts) free(starts);
    if (nverts) free(nverts);
    if (towork) free(towork);
    if (lincs) free(lincs);

    return (2);
  }

  /* Find min and max Z values of each polygon, and collect other info */

  poly = 0;
  for (me = 0; me < obj->meshsize; me++) {
    if (imeshResol(obj->mesh[me].flag) != resol)
      continue;
    listp = obj->mesh[me].list;
    vertp = obj->mesh[me].vert;
    i = 0;
    while (i < obj->mesh[me].lsize) {
      if (imodMeshPolyNormFactors(listp[i++], &lincs[poly], &vertBase,
                                  &normAdd)) {
        zmin = vertp[listp[i + vertBase]].z;
        zmax = zmin;
        surfs[poly] = 0;
        meshes[poly] = me;
        starts[poly] = listp + i + vertBase; /* point to first vert ind */
        ninpoly = 0;
        while (listp[i] != IMOD_MESH_ENDPOLY) {
          ind = listp[i + vertBase];
          i += lincs[poly];
          if (vertp[ind].z < zmin)
            zmin = vertp[ind].z;
          if (vertp[ind].z > zmax)
            zmax = vertp[ind].z;
          ninpoly++;
        }
        nverts[poly] = ninpoly;
        zmins[poly] = zmin;
        zmaxs[poly++] = zmax;
      }
    }
  }

  /* loop through all polygons, looking for next one that's not assigned
     yet */

  nsurfs = 0;
  for (poly = 0; poly < npoly; poly ++) {
    if (!surfs[poly]) {
      nsurfs++;
      surfs[poly] = nsurfs;
      towork[0] = poly;
      nwork = 1;
      iwork = 0;
      while (iwork < nwork) {

        /* To work on a polygon, scan through all the rest that are not
           assigned yet, find ones that overlap in Z */

        work = towork[iwork];
        for (scan = 0; scan < npoly; scan++) {
          if (!surfs[scan] && zmins[work] <= zmaxs[scan] &&
              zmins[scan] <= zmaxs[work]) {
           
            /* Look for common vertices between the polygons */
           
            refp = starts[work];
            found = 0;
            i = 0;
            while (i < nverts[work] && !found) {
              refvert = *refp;
              refp += lincs[work];
              scanp = starts[scan];
              for (j = 0; j < nverts[scan]; j++) {
                if (*scanp == refvert) {
                  found = 1;
                  break;
                }
                scanp += lincs[scan];
              }
              i++;
            }

            /* If found a match, then add the scan polygon to work list as 
               well as assigning it to this surface */

            if (found) {
              towork[nwork++] = scan;
              surfs[scan] = nsurfs;
            }
          }
        }
        iwork++;
      }
    }
  }

  /* Now go through contours, looking for polygons with a matching vertex */

  obj->surfsize = 0;
  for (co = 0; co < obj->contsize; co++) {
    found = 0;
    obj->cont[co].surf = 0;
    for (pt = 0; pt < obj->cont[co].psize && !found; pt++) {
      ptx = obj->cont[co].pts[pt].x;
      pty = obj->cont[co].pts[pt].y;
      ptz = obj->cont[co].pts[pt].z;
      for (poly = 0; poly < npoly && !found; poly++) {
        if (ptz >= zmins[poly] && ptz <= zmaxs[poly]) {
          vertp = obj->mesh[meshes[poly]].vert;
          scanp = starts[poly];
          for (j = 0; j < nverts[poly]; j++) {
            ind = *scanp;
            if (vertp[ind].x == ptx && vertp[ind].y == pty && 
                vertp[ind].z == ptz) {
              found = 1;
              obj->cont[co].surf = surfs[poly];
              if (surfs[poly] > obj->surfsize)
                obj->surfsize = surfs[poly];
              break;
            }
            scanp += lincs[poly];
          }
        }
      }
    } 
  }
  free(zmins);
  free(zmaxs);
  free(meshes);
  free(surfs);
  free(starts);
  free(nverts);
  free(towork);
  free(lincs);
  imodObjectCleanSurf(obj);
  return (0);
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
 * Returns a pointer to the {label} element of object [obj], or NULL if the label or [obj]
 * is NULL.
 */
Ilabel *imodObjectGetLabel(Iobj *obj)
{
  if (!obj)
    return NULL;
  return obj->label;
}

/*!
 * Makes a new, empty {label} element in ojbect [obj], deleting an existing one if any,
 * and returns a pointer to the new label.  Returns NULL if label allocation failed or
 * [obj] is NULL.
 */
Ilabel *imodObjectNewLabel(Iobj *obj)
{
  if (!obj)
    return NULL;
  imodLabelDelete(obj->label);
  obj->label = imodLabelNew();
  return obj->label;
}

/*!
 * Returns one value for [inObject]; possible values for [inValueType] are
 * {IobjMaxContour}, {IobjLineWidth}, {IobjLineWidth2}, {IobjPointSize},
 * {IobjMaxMesh}, {IobjMaxSurface}, {IobjSymType}, {IobjSymSize}, {IobjSymFlags},
 * {IobjFlagClosed}, {IobjFlagConnected}, {IobjFlagFilled}, {IobjFlagDraw}, 
 * {IobjFlagMesh}, {IobjFlagLine}, {IobjFlagTime}, {IobjFlagExtraInModv}, and 
 * {IobjFlagPntOnSec}.  {IobjFlagConnected} is 0 for scattered point objects.
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
  case IobjSymType:
    return(inObject->symbol);
  case IobjSymSize:
    return(inObject->symsize);
  case IobjSymFlags:
    return(inObject->symflags);

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

  case IobjFlagPntOnSec:
    return(inObject->flags & IMOD_OBJFLAG_PNT_ON_SEC);

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
 * [inValueType] are {IobjLineWidth}, {IobjLineWidth2}, {IobjPointSize}, {IobjSymType},
 * {IobjSymSize}, {IobjSymFlags},
 * {IobjFlagClosed}, {IobjFlagConnected}, {IobjFlagFilled}, {IobjFlagDraw}, 
 * {IobjFlagMesh}, {IobjFlagLine}, {IobjFlagExtraInModv} (for drawing an 
 * extra object in model view window), and {IobjFlagPntOnSec}.
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

  case IobjSymType:
    inObject->symbol = inValue;
    return;

  case IobjSymSize:
    inObject->symsize = inValue;
    return;

  case IobjSymFlags:
    inObject->symflags = inValue;
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
    return;

  case IobjFlagPntOnSec:
    setObjFlag(inObject, IMOD_OBJFLAG_PNT_ON_SEC, inValue);

  }
}

