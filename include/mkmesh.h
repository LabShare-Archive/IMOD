/*  mkmesh.h - include file for libimesh
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 * 
 * $Id$
 * Log at end
 */

#ifndef MKMESH_H
#define MKMESH_H

#include "imodel.h"

/* make mesh flags. */
#define IMESH_MK_FAST     (1l << 2) /* do things sloppy and fast. */
#define IMESH_MK_SKIP     (1l << 3) /* join data that skips sections.      */
#define IMESH_MK_NORM     (1l << 4) /* calculate normals.                  */
#define IMESH_MK_STRAY    (1l << 5) /* force connection of stray contours. */
#define IMESH_MK_SURF     (1l << 6) /* connect to same surface only        */
#define IMESH_MK_TUBE     (1l << 7) /* open contours are tubes. */
#define IMESH_MK_TIME     (1l << 8) /* connect to same time (type) only. */
#define IMESH_MK_CAP_TUBE (1l << 9) /* cap ends of tubes */
#define IMESH_MK_IS_COPY  (1l << 10) /* Object is already a copy */
#define IMESH_MK_USE_MEAN (1l << 11) /* Use mean Z not starting Z to flatten */
#define IMESH_MK_NO_WARN  (1l << 12) /* Turn off overlap warnings */
#define IMESH_MK_CAP_DOME (1l << 13) /* cap ends of tubes with domes */

#define IMESH_CAP_OFF      0    /* Don't cap ends of surfaces. */
#define IMESH_CAP_END      1    /* Just cap min and max ends.  */
#define IMESH_CAP_ALL      2    /* Cap all stray ends.         */

#ifdef __cplusplus
extern "C" {
#endif

  /* Functions in skinobj.c */
  void skin_report_time(char *string);
  int imeshSkinObject
  (Iobj *obj,            /* The object to skin. */
   Ipoint *scale,        /* scale values.             */
   double overlap,       /* Overlap percentage             */
   int cap,              /* Cap flag.                 */
   int *cap_skip_zlist,  /* List of Z values to not cap */
   int cap_skip_nz,      /* Number of Z values to not cap */
   int incz,             /* Increment in Z values */
   unsigned int  flags,  /* flags.                    */
   int skipPasses,     /* Number of passes for skipped secs */
   double tubeDiameter,  /* Diameter for tube meshing */
   int (*inCB)(int));     /* Callback status function. */

  /* Functions in objprep.c */
  int imodMeshesDeleteRes(Imesh **meshp, int *size, int resol);
  Iobj *imeshDupMarkedConts(Iobj *obj, unsigned int flag);
  int imeshPrepContours(Iobj *obj, int minz, int maxz, int incz, float tol,
                        int useMeanZ);
  int analyzePrepSkinObj(Iobj *obj, int resol, Ipoint *scale, 
                         int (*inCB)(int));


  /* Functions in mkmesh.c */
  Imesh *imeshContoursCost(Iobj *obj, Icont *bc, Icont *tc, Ipoint *scale,
                           int inside, int bco, int tco);
  void imeshSetSkinFlags(int inFlag, int inFast);
  void imeshSetMinMax(Ipoint inMin, Ipoint inMax);
  Imesh *makeCapMesh(Icont *cont, Ipoint *cm, int meshdir, 
                     DrawProps *props, int state, int stateTest);
  int chunkMeshAddIndex(Imesh *mesh, int index, int *maxlist);
  Imesh *joinTubeCont(Icont *c1, Icont *c2, Ipoint *norm, 
                      DrawProps *props1, int state1, DrawProps *props2,
                      int state2);
  int makeTubeCont(Icont *cont, Ipoint *loc, Ipoint *n, Ipoint *scale,
                   float tubeDiameter, int slices);
  
  /* Functions in remesh.c */
  Imesh *imeshReMeshNormal(Imesh *meshes, int *size, Ipoint *scale, int resol);
  void imeshSetNewPolyNorm(int value);
  void imeshNormal(Ipoint *n, Ipoint *p1, Ipoint *p2, Ipoint *p3,
                   Ipoint *sp);

#ifdef __cplusplus
}
#endif

#endif

/*

$Log$
Revision 1.2  2008/05/02 21:52:36  mast
Made imeshNormal a global

Revision 1.1  2006/09/12 15:12:13  mast
Converted to library, split up functions

Revision 3.4  2006/05/08 16:49:32  mast
Removed "FLAT" cap definitions, unused

Revision 3.3  2005/09/11 19:21:07  mast
Added mina dn max arguments

Revision 3.2  2004/11/05 19:05:29  mast
Include local files with quotes, not brackets

Revision 3.1  2003/08/26 03:49:00  mast
Added flag definition for capping ends of tubes

*/
