/*  IMOD VERSION 2.02
 *
 *  imesh.c -- library of mesh functions.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1996 by Boulder Laboratory for 3-Dimensional Fine    *
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

/*
 * Functions                              Descriptions
 * ---------------------------------------------------------------------------
 * Imesh *imodMeshNew(void)               Create a new mesh.
 * Imesh *imodMeshesNew(int size)         Create a new mesh array.
 * int imodMeshCopy(Imesh *from,          Copy mesh structure.
 *                  Imesh *to)
 * Imesh *imodMeshDup(Imesh *mesh);       Duplicate mesh including data
 * int imodMeshDelete(Imesh *mesh)        Delete a mesh.
 * int imodMeshesDelete(Imesh *mesh,      Delete a mesh array.
 *                      int size)
 * int imodMeshAddIndex(Imesh *mesh,      Add index to mesh.
 *                      int index)
 * int imodMeshAddVert(Imesh *mesh,       Add vertex to mesh.
 *                     Ipoint *vert)
 * int imodMeshAddNormal(Imesh *mesh,     Add normal point at end of mesh.
 *                       Ipoint *normal)
 */

/* Utility functions.
 *
 * Surface area and volume in pixel units.
 * ----------------------------------------
 * float imeshSurfaceArea(Imesh *mesh, Ipoint *scale);
 * float imeshVolume(Imesh *mesh, Ipoint *scale);
 *
 * Imesh *imeshContourCap(Icont *cont, int side);
 */

/*  $Author$
    
$Date$

$Revision$

$Log$
*/

#include <limits.h>
#include <math.h>
#include <string.h>
#include "imodel.h"


void imodel_normal(struct Mod_Point *n,
                   struct Mod_Point *p1,
                   struct Mod_Point *p2,
                   struct Mod_Point *p3, double z);

void imesh_normal(Ipoint *n, Ipoint *p1, Ipoint *p2, Ipoint *p3, Ipoint *sp);

/*#define SKIN_DEBUG     */


Imesh *imodMeshNew(void)
{
  return(imodMeshesNew(1));
}

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
    mesh[sh].type  = 0;
    mesh[sh].pad   = 0;
    mesh[sh].store = NULL;
  }
  return(mesh);
}

int imodMeshGetIndex(Imesh *mesh, int index)
{
  if (!mesh) return(IMOD_MESH_END);
  if ((index < 0) || (index >= mesh->lsize))
    return(IMOD_MESH_END);
  return(mesh->list[index]);
}

int imodMeshGetMaxIndex(Imesh *mesh)
{
  if (!mesh) return(0);
  return(mesh->lsize);
}

int imodMeshGetMaxVert(Imesh *mesh)
{
  if (!mesh) return(0);
  return(mesh->vsize);
}

Ipoint *imodMeshGetVert(Imesh *mesh, int index)
{
  if (!mesh) return(NULL);
  if ((index < 0) || (index >= mesh->vsize))
    return(NULL);
  return(&mesh->vert[index]);
}

Ipoint *imodMeshGetVerts(Imesh *mesh)
{
  if (!mesh) return(NULL);
  if (!mesh->vsize) return(NULL);
  return(mesh->vert);
}



int imodMeshCopy(Imesh *from, Imesh *to)
{
  if (!from)
    return(-1);
  if (!to)
    return(-1);
  memcpy(to, from, sizeof(Imesh));
  return(0);
}

/* Duplicate the mesh including all data */
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

int imodMeshDelete(Imesh *mesh)
{
  return(imodMeshesDelete(mesh, 1));
}

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

int imodMeshAddVert(Imesh *mesh, Ipoint *vert)
{
  Ipoint *tmp;

  if (mesh->vsize)
    tmp = (struct Mod_Point *) realloc
      (mesh->vert, (mesh->vsize + 1) * sizeof(struct Mod_Point));
  else
    tmp = (struct Mod_Point *) malloc(sizeof(struct Mod_Point));
  if (tmp == NULL)
    return(-1);

  mesh->vert = tmp;
  mesh->vert[mesh->vsize].x = vert->x;
  mesh->vert[mesh->vsize].y = vert->y;
  mesh->vert[mesh->vsize].z = vert->z;
  mesh->vsize++;
  return(0);
}

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



void imodMeshInsertIndex(Imesh *mesh, int val, int place)
{
  int l;
  imodMeshAddIndex(mesh, val);

  for(l = mesh->lsize - 1; l > place; l--){
    mesh->list[l] = mesh->list[l - 1];
  }
  mesh->list[l] = val;

}

/* find the nearest resolution available to "inres" in the array of meshes;
   return resolution in "outres" and return 1 if more than one resolution 
   exists */
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
            
      
/***************************internal functions********************************/

int imodel_mesh_addlist(struct Mod_Mesh *mesh, int val)
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
  mesh->list[mesh->lsize] = val;
  mesh->lsize++;
  return(0);
}

int imodel_mesh_addvert(struct Mod_Mesh *mesh, struct Mod_Point *pt)
{
  struct Mod_Point *tmp;

  if (mesh->vsize)
    tmp = (struct Mod_Point *) realloc
      (mesh->vert, (mesh->vsize + 1) * sizeof(struct Mod_Point));
  else
    tmp = (struct Mod_Point *) malloc(sizeof(struct Mod_Point));
     
  if (tmp == NULL)
    return(-1);

  mesh->vert = tmp;
  mesh->vert[mesh->vsize].x = pt->x;
  mesh->vert[mesh->vsize].y = pt->y;
  mesh->vert[mesh->vsize].z = pt->z;
  mesh->vsize++;
  return(0);
}


struct Mod_Mesh *imodel_mesh_add(struct Mod_Mesh *nmesh,
                                 struct Mod_Mesh *mray, int *size)
{
  struct Mod_Mesh *nmray;

  if (nmesh == NULL){
    return(mray);
  }

  if ((mray == NULL) && (!*size)){
      
    nmray = (struct Mod_Mesh *)malloc(sizeof(struct Mod_Mesh));

  }else{

    nmray = (struct Mod_Mesh *) 
      realloc((struct Mod_Mesh *)mray,
              (*size + 1) * sizeof(struct Mod_Mesh));
  }

  if (nmray == NULL)
    return(mray);
     
  nmray[*size].vert  = nmesh->vert;
  nmray[*size].list  = nmesh->list;
  nmray[*size].vsize = nmesh->vsize;
  nmray[*size].lsize = nmesh->lsize;
  nmray[*size].flag  = nmesh->flag;
  nmray[*size].type  = nmesh->type;
  nmray[*size].pad   = nmesh->pad;
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

/* Return area of mesh in square pixels. */
float imeshSurfaceArea(Imesh *mesh, Ipoint *scale)
{
  int i;
  float tsa = 0.0f, sa = 0.0f;

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

    case IMOD_MESH_BGNPOLYNORM:
      i++;
      while(mesh->list[i] != IMOD_MESH_ENDPOLY){
        n.x = n.y = n.z = 0.0f;
        p1 = &(mesh->vert[mesh->list[++i]]);
        i+=2;
        p2 = &(mesh->vert[mesh->list[i]]);
        i+=2;
        p3 = &(mesh->vert[mesh->list[i++]]);

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


