/*  
 *  imesh.c -- library of mesh functions.
 *
 *  Original author: James Kremer
 *  Modified by David Mastronarde  email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$
    
$Date$

$Revision$

$Log$
Revision 3.3  2006/02/25 22:09:04  mast
Documented

Revision 3.2  2005/09/11 19:16:03  mast
Added routine to test for mesh start code and return appropriate factors

Revision 3.1  2004/11/20 04:15:14  mast
Added duplicate function

*/

#include <limits.h>
#include <math.h>
#include <string.h>
#include "imodel.h"
#include "mkmesh.h"


/*#define SKIN_DEBUG     */

/*!
 * Creates a new mesh structure and sets its properties to the default, or
 * returns NULL for an allocation error.
 */
Imesh *imodMeshNew(void)
{
  return(imodMeshesNew(1));
}

/*!
 * Creates an array of [size] mesh structures and sets their properties to the
 * default, or returns NULL for an error.
 */
Imesh *imodMeshesNew(int size)
{
  Imesh *mesh;
  int sh;

  if (size <= 0)
    return(NULL);

  mesh = (Imesh *)malloc(size * sizeof(Imesh));
  if (!mesh)
    return(NULL);
     
  for (sh = 0; sh < size; sh++){
    mesh[sh].vert  = NULL;
    mesh[sh].list  = NULL;
    mesh[sh].vsize = 0;
    mesh[sh].lsize = 0;
    mesh[sh].flag  = 0;
    mesh[sh].time  = 0;
    mesh[sh].surf  = 0;
    mesh[sh].store = NULL;
  }
  return(mesh);
}

/*! Returns the value at [index] in the index list of [mesh], or IMOD_MESH_END
  if [mesh] is NULL or [index] is out of range. */
int imodMeshGetIndex(Imesh *mesh, int index)
{
  if (!mesh) return(IMOD_MESH_END);
  if ((index < 0) || (index >= mesh->lsize))
    return(IMOD_MESH_END);
  return(mesh->list[index]);
}

/*! Returns the size of the index list in [mesh], or 0 if [mesh] is NULL. */
int imodMeshGetMaxIndex(Imesh *mesh)
{
  if (!mesh) return(0);
  return(mesh->lsize);
}

/*! Returns the size of the vertex list in [mesh], or 0 if [mesh] is NULL. */
int imodMeshGetMaxVert(Imesh *mesh)
{
  if (!mesh) return(0);
  return(mesh->vsize);
}

/*! Returns the point at [index] in the vertex list of [mesh], or NULL
  if [mesh] is NULL or [index] is out of range. */
Ipoint *imodMeshGetVert(Imesh *mesh, int index)
{
  if (!mesh) return(NULL);
  if ((index < 0) || (index >= mesh->vsize))
    return(NULL);
  return(&mesh->vert[index]);
}

/*! Returns the vertex array in [mesh], or NULL if there is none. */
Ipoint *imodMeshGetVerts(Imesh *mesh)
{
  if (!mesh) return(NULL);
  if (!mesh->vsize) return(NULL);
  return(mesh->vert);
}


/*!
 * copies a mesh structure from mesh [from] to mesh [to].  Returns -1 for 
 * error.
 */
int imodMeshCopy(Imesh *from, Imesh *to)
{
  if (!from)
    return(-1);
  if (!to)
    return(-1);
  memcpy(to, from, sizeof(Imesh));
  return(0);
}

/*! Returns a duplicate of [mesh], including all data, or NULL for error. */
Imesh *imodMeshDup(Imesh *mesh)
{
  int i;
  Imesh *newMesh = imodMeshNew();
  if (!newMesh)
    return(NULL);
  imodMeshCopy(mesh, newMesh);
  newMesh->vert = (Ipoint *)malloc(mesh->vsize * sizeof(Ipoint));
  newMesh->list = (b3dInt32 *)malloc(mesh->lsize * sizeof(b3dInt32));
  if (!newMesh->vert || !newMesh->list) {
    imodMeshDelete(newMesh);
    return NULL;
  }
  memcpy(newMesh->vert, mesh->vert, mesh->vsize * sizeof(Ipoint));
  memcpy(newMesh->list, mesh->list, mesh->lsize * sizeof(b3dInt32));
  newMesh->store = ilistDup(mesh->store);
  return newMesh;
}

/*!
 * Frees [mesh], including all data that it contains.  Returns -1 if [mesh] is 
 * NULL.
 */
int imodMeshDelete(Imesh *mesh)
{
  return(imodMeshesDelete(mesh, 1));
}

/*!
 * Frees the array of [size] meshes in [mesh], including all data that it 
 * contains.  Returns -1 if [mesh] is NULL.
 */
int imodMeshesDelete(Imesh *mesh, int size)
{
  int ms;
  if (!mesh)
    return(-1);
  for(ms = 0; ms < size; ms++){
    if (mesh[ms].vert)
      free(mesh[ms].vert);
    if (mesh[ms].list)
      free(mesh[ms].list);
    ilistDelete(mesh[ms].store);
  }
  free(mesh);
  return(0);
}

/*!
 * Adds [index] to the end of the index list of [mesh], allocating one more
 * element for the index array.  Returns -1 for allocation error.
 */
int imodMeshAddIndex(Imesh *mesh, int index)
{
  int *tmp;
     
  if ((mesh->lsize > 0) && (mesh->list))
    tmp = (int *)realloc(mesh->list, (mesh->lsize + 1) * sizeof(int));
  else{
    tmp = (int *)malloc(sizeof(int));
    mesh->lsize = 0;
  }
  if (tmp == NULL)
    return(-1);
     
  mesh->list = tmp;
  mesh->list[mesh->lsize] = index;
  mesh->lsize++;
  return(0);
}


/* Unused 7/4/05 */
/*! Deletes the item at [index] from the index lis of [mesh] */
void imodMeshDeleteIndex(Imesh *mesh, int index)
{
  int i;
  if ( (!mesh) || (index < 0) || (index >= mesh->lsize))
    return;

  for(i = index+1; i < mesh->lsize; i++)
    mesh->list[i-1] = mesh->list[i];
  mesh->lsize--;
  return;
}

/*!
 * Adds [vert] to the end of the vertex array of [mesh], allocating one more
 * element for the array.  Returns -1 for allocation error.
 */
int imodMeshAddVert(Imesh *mesh, Ipoint *vert)
{
  Ipoint *tmp;

  if (mesh->vsize)
    tmp = (Ipoint *) realloc
      (mesh->vert, (mesh->vsize + 1) * sizeof(Ipoint));
  else
    tmp = (Ipoint *) malloc(sizeof(Ipoint));
  if (tmp == NULL)
    return(-1);

  mesh->vert = tmp;
  mesh->vert[mesh->vsize].x = vert->x;
  mesh->vert[mesh->vsize].y = vert->y;
  mesh->vert[mesh->vsize].z = vert->z;
  mesh->vsize++;
  return(0);
}

/* Unused 7/4/05 */
int imodMeshAddNormal(Imesh *mesh, Ipoint *normal)
{
  if (!mesh)
    return(-1);
  if (!normal)
    return(-1);
  imodMeshAddVert(mesh, normal);
  imodMeshAddIndex(mesh, IMOD_MESH_NORMAL);
  imodMeshAddIndex(mesh, mesh->vsize - 1);
  return(0);
}


/* Unused 7/4/05 */
/*
 * Inserts an index value [val] into the index list of [mesh] at position 
 * [place] in the array.  Does not check for error.
 */
void imodMeshInsertIndex(Imesh *mesh, int val, int place)
{
  int l;
  imodMeshAddIndex(mesh, val);

  for(l = mesh->lsize - 1; l > place; l--){
    mesh->list[l] = mesh->list[l - 1];
  }
  mesh->list[l] = val;

}

/*!
 * Finds the nearest resolution to [inres] available in the array of meshes
 * of size [size] in [mesh].  Returns the resolution in [outres].  If there
 * are two equally distance resolutions (one below, one above), it returns
 * the resolution that is above [inres].  The return value is 0 if only one
 * resolution exists, 1 if more than one resolution exists, and -1 for error.
 */
int imodMeshNearestRes(Imesh *mesh, int size, int inres, int *outres)
{
  int m, res;
  int oldiff, newdiff, ndiff = 0;
  *outres = inres;
  if (!size || !mesh)
    return -1;
  *outres = imeshResol(mesh->flag);
      
  for (m = 1; m < size; m++) {
    res = imeshResol(mesh[m].flag);
    if (res != *outres) {
      ndiff = 1;
      oldiff = *outres - inres;
      if (oldiff < 0)
        oldiff = -oldiff;
      newdiff = res - inres;
      if (newdiff < 0)
        newdiff = -newdiff;
      /* take the resolution with the smallest difference from the
         desired one; or if there is a tie, take the one that is
         a higher number than desired rather than lower */
      if (newdiff < oldiff || (newdiff > 0 && newdiff == oldiff))
        *outres = res;
    }
  }
  return ndiff;
}

/*!
 * Returns values for indexing the mesh list for an old or new style POLYNORM
 * into [listInc], [vertBase], and [normAdd].  The return value is 1 if 
 * [startCode] is IMOD_MESH_BGNPOLYNORM or IMOD_MESH_BGNPOLYNORM2, otherwise it
 * is 0.  If {i} is the index of the start of a triangle, then: ^
 *    index of vertex {j} is mesh->list\[i + vertBase + j * listInc\] ^
 *    index of normal {j} is mesh->list\[i + j * listInc\] + normAdd
 */
int imodMeshPolyNormFactors(int startCode, int *listInc, int *vertBase,
                            int *normAdd)
{
  if (startCode != IMOD_MESH_BGNPOLYNORM && 
      startCode != IMOD_MESH_BGNPOLYNORM2)
    return 0;
  if (startCode == IMOD_MESH_BGNPOLYNORM) {
    *listInc = 2;
    *vertBase = 1;
    *normAdd = 0;
  } else {
    *listInc = 1;
    *vertBase = 0;
    *normAdd = 1;
  }
  return 1;
}
            
      
/* 2/25/06: deleted imodel_mesh_addlist and imodel_mesh_addvert which were
   duplicates of imodMesh calls */

/*!
 * Adds a new mesh in [nmesh] to the array of meshes in [mray].  [size] 
 * specifies the current number of meshes in [mray] and is returned with the
 * new number of meshes.  Allocated data are transferred from [nmesh] to the 
 * array but [nmesh] is not freed.  Returns NULL or the existing existing array
 * for an error.
 */
Imesh *imodel_mesh_add(Imesh *nmesh,
                                 Imesh *mray, int *size)
{
  Imesh *nmray;

  if (nmesh == NULL){
    return(mray);
  }

  if ((mray == NULL) && (!*size)){
      
    nmray = (Imesh *)malloc(sizeof(Imesh));

  }else{

    nmray = (Imesh *) 
      realloc((Imesh *)mray,
              (*size + 1) * sizeof(Imesh));
  }

  if (nmray == NULL)
    return(mray);
     
  nmray[*size].vert  = nmesh->vert;
  nmray[*size].list  = nmesh->list;
  nmray[*size].vsize = nmesh->vsize;
  nmray[*size].lsize = nmesh->lsize;
  nmray[*size].flag  = nmesh->flag;
  nmray[*size].time  = nmesh->time;
  nmray[*size].surf  = nmesh->surf;
  nmray[*size].store = nmesh->store;
  (*size)++;
  return(nmray);
}


/****************************************************************************/
/* Get mesh info.                                                           */
/****************************************************************************/

float imeshVolume(Imesh *mesh, Ipoint *scale)
{
  return(0.0f);
}

/*! Returns the surface area of [mesh] in square pixels.  If [scale] is 
 * non-NULL, its {z} value specifies the Z scaling to apply. */
float imeshSurfaceArea(Imesh *mesh, Ipoint *scale)
{
  int i;
  float tsa = 0.0f, sa = 0.0f;
  int listInc, vertBase, normAdd;

  Ipoint *p1, *p2, *p3, *p;
  Ipoint n, n1, n2;
  float zs = 1.0f;

  if ((!mesh) || (!mesh->lsize))
    return(0.0f);
  if (scale) zs = scale->z;

  for(i = 0; i < mesh->lsize; i++){
    switch(mesh->list[i]){
    case IMOD_MESH_BGNBIGPOLY:
    case IMOD_MESH_BGNPOLY:
      n.x = n.y = n.z = 0.0f;
      i++;
      if (mesh->list[i] == IMOD_MESH_ENDPOLY)
        break;
      p = p1 = &(mesh->vert[mesh->list[i]]);
      while(mesh->list[++i] != IMOD_MESH_ENDPOLY){
        p2 = &(mesh->vert[mesh->list[i]]);
        n.x += (p1->y * p2->z *zs)
          - (p1->z *zs * p2->y);
        n.y += (p1->z *zs * p2->x)
          - (p1->x * p2->z*zs);
        n.z += (p1->x * p2->y)
          - (p1->y * p2->x);
        p1 = p2;
      }
      p2 = p;
      n.x += (p1->y * p2->z * zs)
        - (p1->z * zs * p2->y);
      n.y += (p1->z * zs * p2->x)
        - (p1->x * p2->z * zs);
      n.z += (p1->x * p2->y)
        - (p1->y * p2->x);
      tsa += (float)(sqrt(n.x*n.x + n.y*n.y + n.z*n.z) * 0.5f); 
      break;

    case IMOD_MESH_BGNPOLYNORM2:
    case IMOD_MESH_BGNPOLYNORM:
      imodMeshPolyNormFactors(mesh->list[i++], &listInc, &vertBase, &normAdd);
      while(mesh->list[i] != IMOD_MESH_ENDPOLY){
        n.x = n.y = n.z = 0.0f;
        p1 = &(mesh->vert[mesh->list[i + vertBase]]);
        i+=listInc;
        p2 = &(mesh->vert[mesh->list[i + vertBase]]);
        i+=listInc;
        p3 = &(mesh->vert[mesh->list[i + vertBase]]);
        i+=listInc;

        n1.x = p1->x - p2->x;
        n1.y = p1->y - p2->y;
        n1.z = (p1->z - p2->z) * zs;
        n2.x = p3->x - p2->x;
        n2.y = p3->y - p2->y;
        n2.z = (p3->z - p2->z) * zs;
        imodPointCross(&n1, &n2, &n);

        tsa += (float)(sqrt(n.x*n.x + n.y*n.y + n.z*n.z) * 0.5);
      }
           
      break;
           
    case IMOD_MESH_END:
      return(tsa);
           
    }
      
  }
  return(tsa);
}

/*******************
 * Meshing parameter functions 
 ******************/

/*!
 * Allocates and returns a new MeshParams structure after setting it to
 * default values.  Returns NULL for error.
 */
MeshParams *imeshParamsNew()
{
  MeshParams *params = (MeshParams *)malloc(sizeof(MeshParams));
  if (!params)
    return NULL;
  imeshParamsDefault(params);
  return params;
}

/*!
 * Sets [params] to default values.
 */
void imeshParamsDefault(MeshParams *params)
{
  params->flags = IMESH_MK_NORM;
  params->cap = IMESH_CAP_OFF;
  params->passes = 1;
  params->capSkipNz = 0;
  params->inczLowRes = 4;
  params->inczHighRes = 1;
  params->minz = DEFAULT_VALUE;
  params->maxz = DEFAULT_VALUE;
  params->xmin = -DEFAULT_FLOAT;
  params->xmax = DEFAULT_FLOAT;
  params->ymin = -DEFAULT_FLOAT;
  params->ymax = DEFAULT_FLOAT;
  params->overlap = 0.;
  params->tubeDiameter = 10.;
  params->tolLowRes = 2.0;
  params->tolHighRes = 0.25;
  params->flatCrit = 1.;
  params->spareInt = 0.;
  params->spareFloat = 0.;
  params->capSkipZlist = NULL;
}

/*!
 * Returns a duplicate of the meshing parameters in [params], including a cap
 * skip list, if any.  Returns NULL for error or if [params] is NULL.
 */
MeshParams *imeshParamsDup(MeshParams *params)
{
  if (!params)
    return NULL;
  MeshParams *newpar = imeshParamsNew();
  if (!newpar)
    return NULL;
  *newpar = *params;
  newpar->capSkipZlist = NULL;
  newpar->capSkipNz = 0;
  if (imeshCopySkipList(params->capSkipZlist, params->capSkipNz, 
                        &newpar->capSkipZlist, &newpar->capSkipNz)) {
    free(newpar);
    return NULL;
  }
  return newpar;
}

/*!
 * Frees the [params] structure and the {capSkipZlist} array if it is nonnull
 * and {capSkipNz} is nonzero.
 */
void imeshParamsDelete(MeshParams *params)
{
  if (!params)
    return;
  if (params->capSkipZlist && params->capSkipNz)
    free(params->capSkipZlist);
  free(params);
}

/*!
 * Copies the integer list in [lfrom] of size [nfrom] to a new list pointed
 * to by [lto], returning the size in [nto].  Frees an existing list in [lto]
 * first if [lto] and [nto] are non-NULL, then allocates a new list with space
 * for three more values than [nfrom].  If [lfrom] is NULL, no new list is
 * allocated.  Returns 1 for memory error.
 */
int imeshCopySkipList(int *lfrom, int nfrom, int **lto, int *nto)
{
  int i;
  *nto = 0;
  if (*lto && *nto) {
    free(*lto);
  }
  if (lfrom) {
    *lto = (b3dInt32 *)malloc((nfrom + 3) * sizeof(b3dInt16));
    if (!*lto) {
      b3dError(stderr, "Error getting memory to copy cap exclusion list");
      return 1;
    }
    for (i = 0; i < nfrom; i++)
      (*lto)[i] = lfrom[i];
    *nto = nfrom;
  }
  return 0;
}

