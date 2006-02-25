/*
 *  imesh.h -- Image model mesh header.
 *
 *  Author: James Kremer
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */
/*  $Author$

$Date$

$Revision$

$Log$
*/

#ifndef IMESH_H
#define IMESH_H

#include "imodel.h"

/****************************************************************************/
/*
 * Instruction values for mesh list array.  Positive values are indexes into
 the vertex point data. 
 */

/* These definitions are obsolete, but still appear in imodv_ogl.cpp
 */
#define IMOD_MESH_BGNTRI      -11  /* begin triangle strip                 */
#define IMOD_MESH_ENDTRI      -12  /* end triangle strip                   */
#define IMOD_MESH_SWAP        -10  /* swaptmesh();                         */

/* When this value is reached no more drawing is done
 * The end of the list is assumed, even if there is data left.
 */
#define IMOD_MESH_END          -1  /* end of list array                    */

/* Quit drawing the current polygon but keep processing data.
 */
#define IMOD_MESH_ENDPOLY     -22  /* end of polygon data                  */

#define IMOD_MESH_NORMAL      -20  /* next item on list is normal vector.  */
#define IMOD_MESH_BGNPOLY     -21  /* begin polygon                        */
#define IMOD_MESH_BGNBIGPOLY  -24  /* poly not simple, can be concave.     */

/* Most common drawing command used by imodmesh.
 * Data is sets of triangles.
 * Old version: indexes are to Normal, Vertex, Normal, Vertex, Normal, Vertex.
 * New version: indexes are to vertices; each normal assumed to follow vertex.
 */
#define IMOD_MESH_BGNPOLYNORM  -23  /* normal, vertex index pairs           */
#define IMOD_MESH_BGNPOLYNORM2 -25  /* vertex indices only                  */

/****************************************************************************/
/* mesh flags */
#define IMESH_FLAG_NMAG   (1l << 16) /* normals have magnitudes        */
#define IMESH_FLAG_RES_SHIFT 20      /* location of 4 resolution bits    */
#define IMESH_FLAG_RES_BITS  (15l << IMESH_FLAG_RES_SHIFT) /* resolution */

#define imeshResol(flag) (((flag)&IMESH_FLAG_RES_BITS) >> IMESH_FLAG_RES_SHIFT)

#ifdef __cplusplus
extern "C" {
#endif

  /*
   *  Create or delete a mesh that is not part of an object or model.
   *
   *  NOTE: There is no imodMeshGet, imodMeshGetFirst or imodMeshGetNext
   *        functions because the model doesn't store a current mesh 
   *        because mesh data is not editable directly by the user.
   */
  Imesh *imodMeshesNew(int size);
  Imesh *imodMeshNew(void);
  int    imodMeshDelete(Imesh *mesh);
  int    imodMeshesDelete(Imesh *mesh, int size);
  int    imodMeshCopy(Imesh *from, Imesh *to);
  Imesh *imodMeshDup(Imesh *mesh);

  int    imodMeshAddIndex(Imesh *mesh, int index);
  void   imodMeshDeleteIndex(Imesh *mesh, int index);
  int    imodMeshAddVert(Imesh *mesh, Ipoint *vert);
  void   imodMeshInsertIndex(Imesh *mesh, int val, int place);
  Imesh *imodMeshContour(Icont *cont, int side);

  int     imodMeshGetIndex(Imesh *mesh, int index);
  int     imodMeshGetMaxIndex(Imesh *mesh);
  int     imodMeshGetMaxVert(Imesh *mesh);
  Ipoint *imodMeshGetVert(Imesh *mesh, int index);
  Ipoint *imodMeshGetVerts(Imesh *mesh);
  int     imodMeshAddNormal(Imesh *mesh, Ipoint *normal);

  int     imodMeshNearestRes(Imesh *mesh, int size, int inres, int *outres);
  int     imodMeshPolyNormFactors(int startCode, int *listInc, int *vertBase, 
                                  int *normAdd);

  Imesh *imodel_mesh_add(Imesh *nmesh, Imesh *mray, int *size);


  /* get info from meshes. */
  float imeshSurfaceArea(Imesh *mesh, Ipoint *mscale);
  float imeshVolume(Imesh *mesh, Ipoint *mscale);


#ifdef __cplusplus
}
#endif
#endif /* imesh.h */


