/*
 *  ipoint.h -- Image model point header.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

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

int     imodPointAppend(Icont *cont, Ipoint *pnt);
int     imodPointAppendXYZ(Icont *cont, float x, float y, float z);
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
float   imodPointLineDistance(Ipoint *ln, Ipoint *p);
double  imodPoint2DAngle(Ipoint *pt);

float   imodPoint3DScaleDistance(Ipoint *p1, Ipoint *p2, Ipoint *scale);
float   imodPointArea(Ipoint *p1, Ipoint *p2, Ipoint *p3);
float   imodPointAreaScale(Ipoint *p1, Ipoint *p2, Ipoint *p3, Ipoint *s);

int     imodPointIsEqual(Ipoint *a, Ipoint *b);     
int imodPointIntersect(Ipoint *a, Ipoint *b, Ipoint *c, Ipoint *d);
		       
double imodel_point_dist(Ipoint *pnt1, Ipoint *pnt2);
float imodPointLineSegDistance(Ipoint *lp1, Ipoint *lp2, Ipoint *p,
                               float *tval);
int imodPointInsideCont(Icont *cont, Ipoint *pt);

#ifdef __cplusplus
}
#endif
#endif /* ipoint.h */


