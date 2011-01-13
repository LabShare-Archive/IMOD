/*
 *  imodmesh.c -- Add mesh data to a model.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 * Log at end
 */

#include <stdio.h>
#include <stdlib.h>
#include "mkmesh.h"

static int ReduceObj(Iobj *obj, float dist);
static void cleanzero(Iobj *obj);
static int resecobj(Iobj *obj, int minz, int maxz, int incz);
static int floatcmp(const void *v1, const void *v2);
static void cleanPrepArrays(Ipoint *bbmin, Ipoint *bbmax, float *volume, 
                            float *volsort, Imat *mat, Imat *inv,
                            Iobj *useObj);
static int extendOpenEnds(Iobj *obj);


/*!
 * Provides a single call for analysis, preparation, and skinning of object 
 * [obj].  Uses the meshing parameters in the {meshParam} member of [obj],
 * a @@MeshParams structure@.
 * Duplicates the contours unless IMESH_MK_IS_COPY is set in {flags}.
 * Analyzes for flatness if {flatCrit} is nonzero and if the contours are
 * not flat enough, finds a separate rotation to flatness for each surface.
 * Calls @imeshPrepContours and @imeshSkinObj .  Returns 1 for error.
 */
int analyzePrepSkinObj(Iobj *obj, int resol, Ipoint *scale, int (*inCB)(int))
{
  int minz, maxz, incz, cap, skipPasses;
  float tol, flatCrit, overlap, tubeDiameter;
  unsigned int flags;
  Ipoint triMin = {-DEFAULT_FLOAT, -DEFAULT_FLOAT, -DEFAULT_FLOAT};
  Ipoint triMax = {DEFAULT_FLOAT, DEFAULT_FLOAT, DEFAULT_FLOAT};
  int subsets = 0;
  int maxsurf = 0;
  float maxZdiff = 0.;
  Iobj *useObj;
  Ipoint *bbmin = NULL, *bbmax = NULL;
  float *volume = NULL;
  float *volsort = NULL;
  Imat *mat = NULL, *inv = NULL;
  Imesh *mesh;
  Icont *cont;
  int co, co2, pt, i, m, surf, ninSurf, numNorm, izdiff, nbad, minbad;
  int makeTubes;
  Ipoint cen, cnorm, normsum, ceninv, sclinv, refNorm, sclSkin;
  double beta, alpha, zrot, z1, z2;
  double dtor = 0.017453293;
  float volsum, volavg, volmed, zofs, minofs, zfactor, dval;
  float smallVal = 1.e-4;
 
  /* Unpack values from the mesh parameter structure */
  if (!obj->meshParam) {
    b3dError(stderr, "ERROR: analyzePrepSkinObj - no meshing parameters "
             "in object");
    return 1;
  }
  flags = obj->meshParam->flags;
  cap = obj->meshParam->cap;
  skipPasses = obj->meshParam->passes;
  if (resol) {
    incz = obj->meshParam->inczLowRes;
    tol = obj->meshParam->tolLowRes;
  } else {
    incz = obj->meshParam->inczHighRes;
    tol = obj->meshParam->tolHighRes;
  }
  minz = obj->meshParam->minz;
  maxz = obj->meshParam->maxz;
  triMin.x = obj->meshParam->xmin;
  triMax.x = obj->meshParam->xmax;
  triMin.y = obj->meshParam->ymin;
  triMax.y = obj->meshParam->ymax;
  overlap = obj->meshParam->overlap;
  tubeDiameter = obj->meshParam->tubeDiameter;
  flatCrit = obj->meshParam->flatCrit;
  imeshSetMinMax(triMin, triMax);
  makeTubes = iobjOpen(obj->flags) && (flags & IMESH_MK_TUBE) ? 1 : 0;
  if (makeTubes && tubeDiameter < 0 && tubeDiameter >= -1.0001)
    obj->flags |= IMOD_OBJFLAG_PNT_NOMODV;
  else 
    obj->flags &= ~IMOD_OBJFLAG_PNT_NOMODV;
  if (flatCrit > 0 && !makeTubes) {
    
    /* Get arrays for bounding box and find biggest Z difference in contours */
    bbmin = (Ipoint *)malloc(obj->contsize * sizeof(Ipoint));
    bbmax = (Ipoint *)malloc(obj->contsize * sizeof(Ipoint));
    volume = (float *)malloc(obj->contsize * sizeof(float));
    volsort = (float *)malloc(obj->contsize * sizeof(float));
    mat = imodMatNew(3);
    if (!bbmin || !bbmax || !volume || !mat || !volsort) {
      b3dError(stderr, "Memory error allocating boundary box arrays\n");
      cleanPrepArrays(bbmin, bbmax, volume, volsort, mat, NULL, NULL);
      return 1;
    }
    for (co = 0; co < obj->contsize; co++) {
      cont = &obj->cont[co];
      if (cont->psize) {
        maxsurf = B3DMAX(maxsurf, cont->surf);
        imodContourGetBBox(cont, &bbmin[co], &bbmax[co]);
        maxZdiff = B3DMAX(maxZdiff, bbmax[co].z - bbmin[co].z);

        /* Add a little thickness to the differences to avoid zero volumes */
        volume[co] = (bbmax[co].x + 1 - bbmin[co].x) * 
          (bbmax[co].y + 1 - bbmin[co].y) * (bbmax[co].z + 1 - bbmin[co].z);
        /* printf("co %d volume %f  maxz %f\n", co, volume[co], maxZdiff); */
      }
    }
    
    if (maxZdiff >= flatCrit)
      subsets = 1;
    else
      cleanPrepArrays(bbmin, bbmax, volume, volsort, mat, inv, NULL);
  }
  
  if (!subsets) {
    
    /* Simple case of no tilted contours */
    if (flags & IMESH_MK_IS_COPY)
      useObj = obj;
    else
      useObj = imeshDupMarkedConts(obj, 0);
    if (!useObj)
      return 1;
    if (!makeTubes && imeshPrepContours(useObj, minz, maxz, incz, tol, 
                                    flags & IMESH_MK_USE_MEAN))
      return 1;
    if (imeshSkinObject(useObj, scale, overlap, cap, 
                        obj->meshParam->capSkipZlist,
                        obj->meshParam->capSkipNz, incz, flags, skipPasses, 
                        tubeDiameter, inCB))
      return 1;
    
    if (useObj != obj) {
      /* printf("Old %d  New %d  contsize %d\n", obj->meshsize, 
         useObj->meshsize, useObj->contsize); */
      if (obj->meshsize)
        imodMeshesDelete(obj->mesh, obj->meshsize);
      obj->mesh = useObj->mesh;
      obj->meshsize = useObj->meshsize;
      useObj->mesh = NULL;
      useObj->meshsize = 0;
      imodObjectDelete(useObj);
    }
    return 0;
  }
  
  /* Tilted contours were found.  Now loop on surfaces */
  if (obj->meshsize)
    imodMeshesDelete(obj->mesh, obj->meshsize);
  obj->mesh = NULL;
  obj->meshsize = 0;

  for (surf = 0; surf <= maxsurf; surf++) {
    
    /* Compute mean volume and centroid of all the bounding boxes */
    volsum = 0.;
    ninSurf = 0;
    cen.x = 0.;
    cen.y = 0.;
    cen.z = 0.;
    for (co = 0; co < obj->contsize; co++) {
      cont = &obj->cont[co];
      if (cont->surf == surf && cont->psize) {
        volsum += volume[co];
        cen.x += volume[co] * (bbmin[co].x + bbmax[co].x) / 2.;
        cen.y += volume[co] * (bbmin[co].y + bbmax[co].y) / 2.;
        cen.z += volume[co] * (bbmin[co].z + bbmax[co].z) / 2.;
        volsort[ninSurf++] = volume[co];
      }
    }

    if (ninSurf < 2)
      continue;
    
    cen.x /= volsum;
    cen.y /= volsum;
    cen.z /= volsum;
    volavg = volsum / ninSurf;

    qsort(volsort, ninSurf, 4, floatcmp);
    volmed = volsort[ninSurf / 2];
    /* printf("Centroid %.2f %.2f %.2f   vol avg %f  median %f\n", cen.x,
       cen.y, cen.z, volavg, volmed);*/

    numNorm = 0;
    normsum.x = normsum.y = normsum.z = 0.;
    for (co = 0; co < obj->contsize; co++) {
      cont = &obj->cont[co];
      if (cont->surf == surf && cont->psize > 2 && volume[co] >= volmed) {
        if (!imodContourFitPlane(cont, scale, &cnorm, &dval, &alpha, &beta)) {

          /* Invert the normal if it does not match the reference */
          if ((numNorm && imodPointDot(&cnorm, &refNorm) < 0) ||
              (!numNorm && cnorm.z < 0.)) {
            cnorm.x = -cnorm.x;
            cnorm.y = -cnorm.y;
            cnorm.z = -cnorm.z;
          }
          normsum.x += cnorm.x;
          normsum.y += cnorm.y;
          normsum.z += cnorm.z;
          if (!numNorm)
            refNorm = cnorm;
          numNorm++;
        }
      }
    }

    /* Get mean of the normals then find the angles to rotate plane flat */
    alpha = 0.;
    beta = 0.;
    if (numNorm) {
      cnorm.x = normsum.x / numNorm;
      cnorm.y = normsum.y / numNorm;
      cnorm.z = normsum.z / numNorm;

      if (fabs((double)cnorm.x) > smallVal || fabs((double)cnorm.z) > smallVal)
        beta = -atan2((double)cnorm.x, (double)cnorm.z);
      zrot = cnorm.z * cos(beta) - cnorm.x * sin(beta);
      alpha = -(atan2(zrot, cnorm.y) - 1.570796);
      /* printf("cnorm %f %f %f  zrot %f alpha %.2f  beta %.2f\n", 
         cnorm.x, cnorm.y, cnorm.z, zrot, alpha/dtor, beta/dtor); */
    }

    /* Figure out how much the normals between planes are being compressed by
       Z-scaling - compute the normal in the original space and transform it */
    sclinv.x = 1. / scale->x;
    sclinv.y = 1. / scale->y;
    sclinv.z = 1. / scale->z;
    sclSkin = *scale;
    cnorm.x *= scale->x;
    cnorm.y *= scale->y;
    cnorm.z *= scale->z;
    imodPointNormalize(&cnorm);
    /*printf("Normal in original space %f %f %f\n", cnorm.x,cnorm.y,cnorm.z);*/

    imodMatId(mat);
    imodMatScale(mat, scale);
    imodMatRot(mat, beta / dtor, b3dY);
    imodMatRot(mat, alpha / dtor, b3dX);
    imodMatScale(mat, &sclinv);
    imodMatTransform(mat, &cnorm, &refNorm);

    /* The Z height of this normal is the distance between planes after 
       flattening so the inverse is the amount that the z scaling needs to 
       increase when flattening the contours, while the skinning Z scale needs
       to change to accommodate */
    sclinv.z /= refNorm.z;;
    sclSkin.z *= refNorm.z;
    /* printf("transformed normal %f %f %f  Z factor %f\n", refNorm.x,
       refNorm.y, refNorm.z, zfactor); */

    /* Compose the transformation */
    ceninv.x = -cen.x;
    ceninv.y = -cen.y;
    ceninv.z = -cen.z;
    imodMatId(mat);
    imodMatTrans(mat, &ceninv);
    imodMatScale(mat, scale);
    imodMatRot(mat, beta / dtor, b3dY);
    imodMatRot(mat, alpha / dtor, b3dX);
    imodMatScale(mat, &sclinv);
    imodMatTrans(mat, &cen);
    
    /* Mark the contours for duplication and get object*/
    for (co = 0; co < obj->contsize; co++) {
      cont = &obj->cont[co];
      if (cont->surf == surf)
        cont->flags |= ICONT_TEMPUSE;
    }
    useObj = imeshDupMarkedConts(obj, ICONT_TEMPUSE);    
    if (!useObj) {
      cleanPrepArrays(bbmin, bbmax, volume, volsort, mat, inv, NULL);
      return 1;
    }

    /* Transform the contours */
    for (co = 0; co < useObj->contsize; co++) {
      float zmin = 1.e10;
      float zmax = -1.e10;
      cont = &useObj->cont[co];
      for (pt = 0; pt < cont->psize; pt++) {
        imodMatTransform(mat, &cont->pts[pt], &cnorm);
        cont->pts[pt] = cnorm;
        /*printf("%d %d %.2f %.2f %.2f\n", co, pt, cnorm.x, cnorm.y,cnorm.z);*/
        zmin=B3DMIN(zmin, cnorm.z);
        zmax=B3DMAX(zmax, cnorm.z);
      }
      /*printf("co %d  min %.3f max %.3f\n", co, zmin, zmax);*/
    }
    if (imeshPrepContours(useObj, minz, maxz, incz, tol, 1)) {
      cleanPrepArrays(bbmin, bbmax, volume, volsort, mat, inv, useObj);
      return 1;
    }

    /* for (co = 0; co < useObj->contsize; co++)
       printf("%d %.2f\n", co, useObj->cont[co].pts[0].z); */
    
    /* Evaluate whether a Z offset is needed to keep contours from rounding
       to inappropriate Z values */
    zofs = 0.;
    minofs = 0.;
    minbad = useObj->contsize * useObj->contsize;
    while (zofs < 0.5 && zofs > -0.5) {
      nbad = 0;
      
      for (co = 0; co < useObj->contsize - 1; co++) {
        z1 = useObj->cont[co].pts[0].z;
        for (co2 = co + 1; co2 < useObj->contsize; co2++) {
          z2 = useObj->cont[co2].pts[0].z;
          if ((int)floor(fabs(z1 - z2) + 0.5) == 1) {
            izdiff = (int)floor(z1 + zofs + 0.5) - (int)floor(z2 + zofs + 0.5);
            if (izdiff != 1 && izdiff != -1)
              nbad++;
          }
        }
      }
      /* printf ("zofs %f  nbad %d\n", zofs, nbad);*/
      if (nbad < minbad) {
        minbad = nbad;
        minofs = zofs;
      }
      if (!nbad)
        break;
      if (zofs <= 0.)
        zofs = -zofs + 0.05;
      else
        zofs = -zofs;
    }

    /* If we found an offset that minimizes or eliminates the problem,
       apply it.  Wait and see if people need anything fancier */
    if (minofs != 0.) {
      /* printf("%d bad rounded intervals at offset %.2f\n", nbad, minofs);*/
      for (co = 0; co < useObj->contsize; co++)
        for (pt = 0; pt < useObj->cont[co].psize; pt++)
          useObj->cont[co].pts[pt].z += minofs;
      
      /*for (co = 0; co < useObj->contsize; co++)
        printf("%d %.2f\n", co, useObj->cont[co].pts[0].z); */

      /* Add shift to transformation so it will be taken out in inverse */
      cnorm.x = 0.;
      cnorm.y = 0.;
      cnorm.z = minofs;
      imodMatTrans(mat, &cnorm);
     }
   
    /* Skin and add to object mesh */
    if (imeshSkinObject(useObj, &sclSkin, overlap, cap, 
                        obj->meshParam->capSkipZlist,
                        obj->meshParam->capSkipNz, incz, flags, skipPasses, 
                        tubeDiameter, inCB)) {
      cleanPrepArrays(bbmin, bbmax, volume, volsort, mat, inv, useObj);
      return 1;
    }

    /* Need a normal transform as well as the point transform */
    inv = imodMatInverse(mat);
    imodMatId(mat);
    imodMatRot(mat, -alpha / dtor, b3dX);
    imodMatRot(mat, -beta / dtor, b3dY);
    
    /* Transform the mesh points and the normals, then add to object */
    for (m = 0; m < useObj->meshsize; m++) {
      mesh = &useObj->mesh[m];
      for (i = 0; i < mesh->vsize; i += 2){
        imodMatTransform(inv, &mesh->vert[i], &cnorm);
        mesh->vert[i] = cnorm;
        imodMatTransform(mat, &mesh->vert[i + 1], &cnorm);
        imodPointNormalize(&cnorm);
        mesh->vert[i + 1] = cnorm;
      }

      obj->mesh = imodel_mesh_add(mesh, obj->mesh, &obj->meshsize);
      if (!obj->mesh) {
        free(useObj->mesh);
        useObj->mesh = NULL;
        useObj->meshsize = 0;
        cleanPrepArrays(bbmin, bbmax, volume, volsort, mat, inv, useObj);
        obj->meshsize = 0;
        return 1;
      }
    }
    imodMatDelete(inv);
    inv = NULL;

    if (useObj->meshsize)
      free(useObj->mesh);
    useObj->mesh = NULL;
    useObj->meshsize = 0;
    imodObjectDelete(useObj);
  }
  cleanPrepArrays(bbmin, bbmax, volume, volsort, mat, inv, NULL);
  return 0;
}

/* Clean up all possible arrays set up for flattening analysis */
static void cleanPrepArrays(Ipoint *bbmin, Ipoint *bbmax, float *volume, 
                            float *volsort, Imat *mat, Imat *inv, Iobj *useObj)
{
  if (bbmin)
    free(bbmin);
  if (bbmin)
    free(bbmax);
  if (volume)
    free(volume);
  if (volsort)
    free(volsort);
  if (mat)
    imodMatDelete(mat);
  if (inv)
    imodMatDelete(inv);
  if (useObj)
    imodObjectDelete(useObj);
}

/* Float comparison function to match requirements of qsort */
static int floatcmp(const void *v1, const void *v2)
{
  float f1 = *((float *)v1);
  float f2 = *((float *)v2);
  if (f1 < f2)
    return -1;
  if (f1 > f2)
    return 1;
  return 0;
}

/*!
 * Deletes meshes pointed to by [meshp] and their {store} elements if their
 * resolution flag matches [resol].  [size] is the size of the mesh; both
 * [meshp] and [size] are updated for the new mesh.  Returns -1 for error.
 */
int imodMeshesDeleteRes(Imesh **meshp, int *size, int resol)
{
  int ms;
  int newsize = 0;
  Imesh *mesh;
  if (!meshp || !(*meshp))
    return(-1);
  mesh = *meshp;

  for(ms = 0; ms < *size; ms++){
    if (imeshResol(mesh[ms].flag) == resol) {
      if (mesh[ms].vert)
        free(mesh[ms].vert);
      if (mesh[ms].list)
        free(mesh[ms].list);
      ilistDelete(mesh[ms].store);
    } else
      mesh[newsize++] = mesh[ms];
  }
  if (!newsize) {
    free(mesh);
    *meshp = NULL;
  }
  *size = newsize;
  return(0);
}

/*!
 * Flattens, resections, reduces resolution, and cleans up small numbers in 
 * the contours of [obj].  Contours are flattened by setting their Z values
 * to the Z of the first point, or to the mean Z value if [useMeanZ] is 
 * nonzero.  Contours with Z not between [minz] and [maxz] and not multiples
 * of [incz] are eliminated ([minz] and [maxz] should both be set to 
 * DEFAULT_VALUE to retain the full Z range).  Point reduction is applied with
 * tolerance [tol] if it is nonzero.  Returns 1 for error.
 */
int imeshPrepContours(Iobj *obj, int minz, int maxz, int incz, float tol,
                      int useMeanZ)
{
  Icont *cont;
  int co, i;
  float zval;

  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    zval = cont->pts[0].z;
    if (useMeanZ) {
      for (i = 1; i < cont->psize; i++)
        zval += cont->pts[i].z;
      zval /= cont->psize;
    }
    for (i = 1; i < cont->psize; i++)
      cont->pts[i].z = zval;
  }

  /* Add phantom ends to open contours if possible */
  if (iobjClose(obj->flags) && extendOpenEnds(obj))
    return 1;

  if (resecobj(obj, minz, maxz, incz))
    return 1;
  if (ReduceObj(obj, tol))
    return 1;
  cleanzero(obj);
  return 0;
}

/*
 * Resection object - remove contours that don't fall on multiple of incz 
 */
static int resecobj(Iobj *obj, int minz, int maxz, int incz)
{
  Icont *cont;
  int co;
  int z = 0;
  int rmcont;

  if (incz <= 0) 
    incz = 1;
  if ((minz == DEFAULT_VALUE) && (maxz == DEFAULT_VALUE) && (incz == 1))
    return 0;

  /* printf("Z section filter from %d to %d, step by %d\n", minz, maxz, incz);
     printf("%d contours\n", obj->contsize); */

  for (co = 0; co < obj->contsize; co++) {
    cont = &(obj->cont[co]);
    rmcont = FALSE;
        
    if (!cont->psize) 
      rmcont = TRUE;

    /*
     * find the zvalue of the contour.
     */
    if (!rmcont)
      z = imodContourZValue(cont);
        
    /*
     * Check the Z value.
     */
    if ((minz != DEFAULT_VALUE && z < minz) ||
        (maxz != DEFAULT_VALUE && z > maxz) || (z % incz))
      rmcont = TRUE;

    if (rmcont) {

      /* printf("removed cont %d : %d\n", co, z); */
      imodContourClear(cont);
      if (imodObjectRemoveContour(obj, co)) {
        b3dError(stderr, "Error removing contour at unneeded slice in Z\n");
        return 1;
      }
      co--;
    }
  }
  return 0;
}

/*
 * Reduce points by removing ones within dist of the remaining lines
 */
static int ReduceObj(Iobj *obj, float dist)
{
  int co, numTest, loopBack, pt;
  Icont *cont;
  Icont *tc;
  float tol;

  if (dist <= 0.0)
    return 0;
  if (!obj)
    return 0;

  for (co = 0; co < obj->contsize; co++){
    cont = &(obj->cont[co]);
    tol = dist;

    /* Check for a loopback contour, end points matching start points */
    numTest = B3DMIN(4, cont->psize / 2 - 1);
    loopBack = 1;
    for (pt = 1; pt <= numTest; pt++) {
      if (fabs(cont->pts[pt].x - cont->pts[cont->psize - pt].x) > 0.001 ||
          fabs(cont->pts[pt].y - cont->pts[cont->psize - pt].y) > 0.001) {
        loopBack = 0;
        break;
      }
    }

    if (cont->psize > 4 && !loopBack)
      while(tol > 0.01 * dist){
        tc = imodContourDup(cont);
        if (!tc) {
          b3dError(stderr, "Failed to get duplicate contour for point "
                   "reduction\n");
          return 1;
        }
        imodContourReduce(tc, tol);
        if (tc->psize < 4){
          tol *= 0.5;
          imodContourDelete(tc);
        } else {
          imodContourClear(cont);
          imodContourCopy(tc, cont);
          free(tc);
          break;
        }
      }
  }
  return 0;
}

static int extendOpenEnds(Iobj *obj)
{
  int co, nco, pt, ptStart, ptEnd, phan, dz, dzmin, retval = 0, numPhant = 0;
  int startGap, endGap;
  int *phantConts;
  int *contz;
  Icont *cont, *nearco;
  float gapLengthRatio = 0.33f;
  Ipoint pnt;
  Istore store;

  phantConts = (int *)malloc(obj->contsize * sizeof(int));
  contz = (int *)malloc(obj->contsize * sizeof(int));
  if (!phantConts || !contz)
    return 1;

  /* Make a list of contours with phantom ends: >= 2 gaps on either end */
  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    contz[co] = imodContourZValue(cont);
    if ((cont->flags & ICONT_OPEN) && cont->psize > 1) {
      startGap = istorePointIsGap(cont->store, 0);
      endGap = istorePointIsGap(cont->store, cont->psize - 2);
      if ((startGap && endGap) || 
          (startGap && istorePointIsGap(cont->store, 1)) ||
          (endGap && istorePointIsGap(cont->store, cont->psize - 3)))
        phantConts[numPhant++] = co;
    }
  }
  if (!numPhant) {
    free(phantConts);
    free(contz);
    return 0;
  }

  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];

    /* Extend a contour if it is open and neither end has phantom points and
       the distance from end to start is greater than a fraction of the total 
       contour length */
    if ((cont->flags & ICONT_OPEN) && cont->psize > 1 && 
        !istorePointIsGap(cont->store, 0) &&
        !istorePointIsGap(cont->store, cont->psize - 2) &&
        imodPointDistance(&cont->pts[0], &cont->pts[cont->psize - 1]) >
        gapLengthRatio * imodContourLength(cont, 0)) {

      nco = -1;
      for (phan = 0; phan < numPhant; phan++) {
        dz = contz[phantConts[phan]] - contz[co];
        if (dz < 0)
          dz = -dz;
        if (nco < 0 || dz < dzmin) {
          dzmin = dz;
          nco = phantConts[phan];
        }
      }

      nearco = &obj->cont[nco];
      /*printf("Extending contour %d at z %d from cont %d at z %d\n", co, 
        contz[co], nco, contz[nco]); */
      
      /* Find last gap point at start and first point after gap at end */
      ptStart = -1;
      while (istorePointIsGap(nearco->store, ptStart + 1) && 
             ptStart < nearco->psize / 2)
        ptStart++;
      ptEnd = nearco->psize;
      while (istorePointIsGap(nearco->store, ptEnd -2) &&
             ptEnd > nearco->psize / 2)
        ptEnd--;

      /* Add points at start and mark as gaps */
      store.flags = GEN_STORE_ONEPOINT;
      store.type = GEN_STORE_GAP;
      store.value.i = 0;
      for (pt = 0; pt <= ptStart; pt++) {
        pnt = nearco->pts[pt];
        pnt.z = contz[co];
        if (!imodPointAdd(cont, &pnt, pt)) {
          retval = 1;
          break;
        }
        store.index.i = pt;
        if (istoreInsert(&cont->store, &store)) {
          retval = 1;
          break;
        }
      }

      /* Add points at end and mark each preceding one as a gap */
      for (pt = ptEnd; pt < nearco->psize; pt++) {
        pnt = nearco->pts[pt];
        pnt.z = contz[co];
        if (!imodPointAppend(cont, &pnt)) {
          retval = 1;
          break;
        }
        store.index.i = cont->psize - 2;
        if (istoreInsert(&cont->store, &store)) {
          retval = 1;
          break;
        }
      }
    }
  }
  free(phantConts);
  free(contz);
  return retval;
}


/*
 * avoid underflow exceptions.  No idea if this is still needed.
*/
static void cleanzero(Iobj *obj)
{
  Icont  *cont;
  Ipoint pnt;
  int co,pt;

  for(co = 0; co < obj->contsize; co++){
    cont = &obj->cont[co];
    for(pt = 0; pt < cont->psize; pt++){
      pnt = cont->pts[pt];
      
      if ((pnt.x < 0.001f) && (pnt.x > -0.001f))
        pnt.x = 0.0f;
      if ((pnt.y < 0.001f) && (pnt.y > -0.001f))
        pnt.y = 0.0f;
      if ((pnt.z < 0.001f) && (pnt.z > -0.001f))
        pnt.z = 0.0f;
      
    }
  }
}

/*!
 * Makes an object with duplicates of the contours in [obj]; either 
 * contours whose flag has a non-zero AND with [flags], or
 * all non-empty contours if [flag] is zero.  Copies store data but not meshes.
 * Returns NULL for error.
 */
Iobj *imeshDupMarkedConts(Iobj *obj, unsigned int flag)
{
  Iobj *newObj;
  Icont *cont;
  int i, err = 0, newInd;
  int maxsurf = 0;

  newObj = imodObjectNew();
  if (!newObj) {
    b3dError(stderr, "Error duplicating contours for meshing\n");
    return NULL;
  }

  /* Copy object structure but zero out the count of mesh and contours in case
     we have to free it */
  imodObjectCopy(obj, newObj);
  newObj->contsize = 0;
  newObj->cont = NULL;
  newObj->store = NULL;
  newObj->label = NULL;
  newObj->meshParam = NULL;
  newObj->meshsize = 0;

  /* Duplicate contours one at a time and add to object */
  
  for (i = 0; i < obj->contsize; i++) {

    /* Skip if there is a flag and this contour is not marked; otherwise
       reset the flag before the copy.  Then skip if empty */
    if (flag && !(flag & obj->cont[i].flags))
      continue;
    if (flag)
      obj->cont[i].flags &= ~flag;
    if (!obj->cont[i].psize)
      continue;

    /* Duplicate the contour and all its data and add to new object */
    err = 1;
    cont = imodContourDup(&obj->cont[i]);
    if (!cont)
      break;

    maxsurf= B3DMAX(maxsurf, cont->surf);
    if ((newInd = imodObjectAddContour(newObj, cont)) < 0)
      break;

    if ((err = istoreCopyContSurfItems(obj->store, &newObj->store, i, newInd,
                                       0)))
      break;
    free(cont);
  }

  /* Copy non-index items and surface items up to the maximum surface copied */
  if (!err)
    err = istoreCopyNonIndex(obj->store, &newObj->store);
  if (!err) {
    for (i = 0; i <= maxsurf; i++)
      if ((err = istoreCopyContSurfItems(obj->store, &newObj->store, i, i, 1)))
        break;
  }

  if (err) {
    imodObjectDelete(newObj);
    b3dError(stderr, "Error duplicating contours for meshing\n");
    return NULL;
  }

  return newObj;
}


/* 
mkmesh.c got the big log from before the split
$Log$
Revision 1.9  2010/04/01 04:11:59  mast
Stop erroneous error when object has no contours

Revision 1.8  2008/09/21 17:59:01  mast
Rationalized value range for using sphere or symbold size for tube diameter

Revision 1.7  2008/06/17 20:13:31  mast
Set or clear new flag based on whether making tubes with point sizes

Revision 1.6  2006/11/02 07:15:03  mast
Rearrange and change documentation

Revision 1.5  2006/10/11 04:06:58  mast
Changed to plane fitting from mean normal routine

Revision 1.4  2006/09/18 19:35:41  mast
Made it not flatten or rotate contours to be meshed as tubes

Revision 1.3  2006/09/13 05:39:07  mast
Increased thickness by 1 to avoid zero volumes when tilted at 90 deg

Revision 1.2  2006/09/13 02:40:20  mast
FIxed a memory leak

Revision 1.1  2006/09/12 14:58:19  mast
Split up and made into new library


*/
