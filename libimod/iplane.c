/*  IMOD VERSION 2.02
 *
 *  iplane.c -- graphic plane library elements.
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

#include <imodel.h>

Iplane *imodPlanesNew(int size)
{
     Iplane *pl;
     int p;

     if (size <= 0)
	  return(NULL);
     
     pl = (Iplane *)malloc(sizeof(Iplane) * size);
     if (pl){
	  for(p = 0; p < size; p++)
	       imodPlaneInit(&(pl[p]));
     }
     return(pl);
}

void imodPlaneDelete(Iplane *plane)
{
     if (plane)
	  free(plane);
}

void imodPlaneInit(Iplane *plane)
{
     /* default plane z <= 0 */
     plane->a = plane->b = plane->d = 0.0f;
     plane->c = -1.0f;
}

void imodPlaneAxisRotate(Iplane *plane, double angle, int axis)
{
     Ipoint rpt;
     Imat *mat = imodMatNew(3);
     
     imodMatRot(mat, angle, axis);
     imodMatTransform(mat, (Ipoint *)plane, &rpt);
     imodMatDelete(mat);
     plane->a = rpt.x;
     plane->b = rpt.y;
     plane->c = rpt.z;
     return;
}

/* rotate plane by data in point x = alpha, y = beta, z = gamma. */
void imodPlaneRotate(Iplane *plane, double angle, Ipoint *pnt)
{
     Ipoint rpt;
     Imat *mat = imodMatNew(3);

     imodMatRotateVector(mat, angle, pnt);
     imodMatTransform(mat, (Ipoint *)plane, &rpt);
     imodMatDelete(mat);
     plane->a = rpt.x;
     plane->b = rpt.y;
     plane->c = rpt.z;
     return;
}

/* Translate plane by given point (x, y, z) */
void imodPlaneTranslate(Iplane *plane, Ipoint *pnt)
{
     plane->a += pnt->x;
     plane->b += pnt->y;
     plane->c += pnt->z;
     return;
}

/* set plane parameters from a point on a plane and a normal */
void imodPlaneSetPN(Iplane *plane, Ipoint *pnt, Ipoint *nor)
{
     plane->a = nor->x;
     plane->b = nor->y;
     plane->c = nor->z;
     plane->d = (pnt->x * nor->x) + (pnt->y * nor->y) + (pnt->z * nor->z);
     return;
}



/* Returns True if point is in the half space defined by clipping plane. */
/* Returns False if point needs to be clipped.                           */
int imodPlaneClip(Iplane *plane, Ipoint *pnt)
{
     if ( ((plane->a * pnt->x) + (plane->b * pnt->y) + 
	 (plane->c * pnt->z) + plane->d) >= 0)
	  return(1);
     else
	  return(0);
}

/* Check a series of planes */
int imodPlanesClip(Iplane *plane, int nplanes, Ipoint *pnt)
{
     int pn;
     for(pn = 0; pn < nplanes; pn++){
	  if (!imodPlaneClip(&(plane[pn]), pnt))
	       return(0);
     }
     return(1);
}
