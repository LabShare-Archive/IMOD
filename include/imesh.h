/*  IMOD VERSION 2.02
 *
 *  imesh.h -- Image model mesh header.
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

#ifndef IMESH_H
#define IMESH_H

#include <imodel.h>

/****************************************************************************/
/* Mod_Mesh instructions for list arrary. */

/* These definitions are obsolete, they were used in the old Iris GL
 * version of imod.  They were used by similar to the IrisGL funcions
 * bgntmesh(), endtmesh(), and swaptmesh().
 * OpenGL has no Triangle Mesh type calls.
 */
#define IMOD_MESH_BGNTRI      -11  /* bgntmesh();                          */
#define IMOD_MESH_ENDTRI      -12  /* endtmesh();                          */
#define IMOD_MESH_SWAP        -10  /* swaptmesh();                         */

/* When one of these values are reached no more drawing is done
 * The end of the list is assumed, even if there is data left.
 */
#define IMOD_MESH_END          -1  /* end of list array                    */
#define IMOD_MESH_ENDLIST  -1

/* Quit drawing the current polygon but keep processing data.
 */
#define IMOD_MESH_ENDPOLY     -22  /* endpolygon();                        */


#define IMOD_MESH_NORMAL      -20  /* next item on list is normal vector.  */
#define IMOD_MESH_BGNPOLY     -21  /* bngpolygon();                        */
#define IMOD_MESH_BGNBIGPOLY  -24  /* poly not simple, can be concave.     */


/* Most commont drawing command used by imodmesh.
 * Data is sets of triangles.
 * Normal, Vertex, Normal, Vertex, Normal, Vertex.
 * It probably should have been called BGNTRINORM.
 */
#define IMOD_MESH_BGNPOLYNORM -23  /* vertex, normal pairs                 */

/* Not used yet. */
#define IMOD_MESH_BGN_VN   -23 /* triangle list (vertex, normal)       */
#define IMOD_MESH_BGN_VNM  -25 /* triangle list (vertex, normal, map)  */

/****************************************************************************/
/* mesh flags */
#define IMESH_FLAG_NMAG   (1l << 16) /* normals have magnitudes        */
#define IMESH_FLAG_RES_SHIFT 20      /* location of 4 resolution bits    */
#define IMESH_FLAG_RES_BITS  (15l << IMESH_FLAG_RES_SHIFT) /* resolution */

#define imeshResol(flag) (((flag)&IMESH_FLAG_RES_BITS) >> IMESH_FLAG_RES_SHIFT)

#ifdef __cplusplus
extern "C" {
#endif

Imesh *imodMeshesNew(int size);
Imesh *imodMeshNew(void);
int    imodMeshCopy(Imesh *from, Imesh *to);
int    imodMeshDelete(Imesh *mesh);
int    imodMeshesDelete(Imesh *mesh, int size);
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

void   imodel_mesh_skin2draw(Imesh *mesh);
Imesh *imodel_mesh_add(struct Mod_Mesh *nmesh,
				 struct Mod_Mesh *mray, int *size);

/*
Imesh *imodel_mesh_mkskin(struct Mod_Contour *bc, 
				    struct Mod_Contour *tc);
void   imeshObjectSkin(Iobj *obj, int cap, int res);
Imesh *imeshContourMakeMesh(Iobj *obj, Icont **xscan, Icont **yscan,
			    int firstc, int nextc, int lastc);
Imesh *imeshSkinContours(Icont *tc, Icont *bc);
Imesh *imodMeshContours(Icont *bc, Icont *tc, Ipoint *scale);
int imodMeshesNormal(Imesh *meshes, int size, Ipoint *scale);

int imeshSkinObject(Iobj *obj, Ipoint *scale, int cap, int skip);
Imesh *imeshReMeshNormal(Imesh *meshes, int *size, Ipoint *scale, int resol);
Imesh *imodMeshContourCap(Icont *cont, int side);
Imesh *imeshContourCap(Icont *cont, int side);
Imesh *imeshContours();
*/

/* get info from meshes. */
float imeshSurfaceArea(Imesh *mesh, Ipoint *mscale);
float imeshVolume(Imesh *mesh, Ipoint *mscale);

/** internal functions **/
int imodel_mesh_addlist(struct Mod_Mesh *mesh, int val);
int imodel_mesh_addvert(struct Mod_Mesh *mesh, struct Mod_Point *pt);


#ifdef __cplusplus
}
#endif
#endif /* imesh.h */


