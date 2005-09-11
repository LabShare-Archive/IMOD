/*
 *  $Id$
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.2  2004/11/05 19:05:29  mast
Include local files with quotes, not brackets

Revision 3.1  2003/08/26 03:49:00  mast
Added flag definition for capping ends of tubes

*/

#ifndef MKMESH_H
#define MKMESH_H

#include "imodel.h"

/* make mesh flags. */
#define IMESH_MK_FAST  (1l << 2) /* do things sloppy and fast. */
#define IMESH_MK_SKIP  (1l << 3) /* join data that skips sections.      */
#define IMESH_MK_NORM  (1l << 4) /* calculate normals.                  */
#define IMESH_MK_STRAY (1l << 5) /* force connection of stray contours. */
#define IMESH_MK_SURF  (1l << 6) /* connect to same surface only        */
#define IMESH_MK_TUBE  (1l << 7) /* open contours are tubes. */
#define IMESH_MK_TIME  (1l << 8) /* connect to same time (type) only. */
#define IMESH_CAP_TUBE (1l << 9) /* cap ends of tubes */

#define IMESH_CAP_OFF      0    /* Don't cap ends of surfaces. */
#define IMESH_CAP_END      1    /* Just cap min and max ends.  */
#define IMESH_CAP_ALL      2    /* Cap all stray ends.         */
#define IMESH_CAP_END_FLAT 3    /* Don't round off ends.       */
#define IMESH_CAP_ALL_FLAT 4    /* Don't round off any caps.   */



int SkinObject(Iobj *obj,            /* The object to skin. */
	       Ipoint *scale,        /* scale values.             */
	       double overlap,       /* not used yet.             */
	       int cap,              /* Cap flag.                 */
	       int *cap_skip_zlist,  /* List of Z values to not cap */
	       int cap_skip_nz,      /* Number of Z values to not cap */
	       int incz,             /* Increment in Z values */
	       unsigned int  flags,  /* flags.                    */
	       int skipPasses,      /* Number of passes for skipped sections */
           Ipoint triMin,       /* Min and max for triangle output */
           Ipoint triMax,  
	       int (*inCB)(int));     /* Callback status function. */

Imesh *imeshReMeshNormal(Imesh *meshes, int *size, Ipoint *scale, int resol);

#endif












