/*  IMOD VERSION 2.50
 *
 *  $Id$
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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
/*  $Author$

$Date$

$Revision$

$Log$
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
	       int (*inCB)(int));     /* Callback status function. */

Imesh *imeshReMeshNormal(Imesh *meshes, int *size, Ipoint *scale, int resol);

#endif












