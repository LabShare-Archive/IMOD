/*  IMOD VERSION 2.30
 *
 *  ipoint.h -- Image model point header.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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

#ifndef IPOINT_H
#define IPOINT_H

#include "imodel.h"

/*****************************************************************************/
/* ipoint.c functions                                                        */
/*****************************************************************************/

#define IPOINT_MAX 5e29f

#ifdef __cplusplus
extern "C" {
#endif

Ipoint *imodPointGet(Imod *imod);
Ipoint *imodPointGetFirst(Imod *imod);
Ipoint *imodPointGetNext(Imod *imod);

int     imodPointAppend(Icont *cont, Ipoint *pnt);
int     imodPointAdd(Icont *cont, Ipoint *pnt, int index);
int     imodPointDelete(Icont *cont, int index);
void    imodPointSetSize(Icont *cont, int pt, float size);
float   imodPointGetSize(Iobj *obj, Icont *cont, int pt);
void    imodPointCross( Ipoint *v1, Ipoint *v2, Ipoint *rp);
void    imodPointNormalize(Ipoint *n);
float   imodPointDistance(Ipoint *pnt1, Ipoint *pnt2);
float   imodPointDot(Ipoint *pnt1, Ipoint *pnt2);
int     imodPointPlaneEdge(Ipoint *rpt, Iplane *plane, int planes,
			   Ipoint *pt1, Ipoint *pt2);
float   imodPointLineSegDistance(Ipoint *lp1, Ipoint *lp2, Ipoint *p);
float   imodPointLineDistance(Ipoint *ln, Ipoint *p);
double  imodPoint2DAngle(Ipoint *pt);

float   imodPoint3DScaleDistance(Ipoint *p1, Ipoint *p2, Ipoint *scale);
float   imodPointArea(Ipoint *p1, Ipoint *p2, Ipoint *p3);
float   imodPointAreaScale(Ipoint *p1, Ipoint *p2, Ipoint *p3, Ipoint *s);

int     imodPointIsEqual(Ipoint *a, Ipoint *b);     
int imodPointIntersect(Ipoint *a, Ipoint *b, Ipoint *c, Ipoint *d);
		       
double imodel_point_dist(Ipoint *pnt1, Ipoint *pnt2);
int    imodel_point_add(Icont *cont, Ipoint *pnt, int index);
int    imodel_point_delete(Icont *cont, int index);

#ifdef __cplusplus
}
#endif
#endif /* ipoint.h */


