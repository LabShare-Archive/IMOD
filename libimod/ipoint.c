/*
 *  ipoint.c -- Point editing functions for IMOD models.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <math.h>
#include "imodel.h"
#include "istore.h"

/*!
 * Adds point [pnt] to the end of contour [cont].  Manages point
 * sizes and labels correctly.  Returns number of points
 * in contour, or 0 if an error occurs.
 */
int imodPointAppend(Icont *cont, Ipoint *pnt)
{
  return(imodPointAdd(cont, pnt, cont->psize));
}

/*!
 * Adds the point [x], [y], [z] to the end of contour [cont].  Manages point
 * sizes and labels correctly.  Returns number of points
 * in contour, or 0 if an error occurs.
 */
int imodPointAppendXYZ(Icont *cont, float x, float y, float z)
{
  Ipoint pnt;
  pnt.x = x;
  pnt.y = y;
  pnt.z = z;
  return(imodPointAdd(cont, &pnt, cont->psize));
}

/*!
 * Adds point [pnt] to contour [cont] at the given [index].  Manages point
 * sizes, labels, and general storage items correctly.  Returns number of 
 * points in contour, or 0 if an error occurs.
 */
int imodPointAdd(Icont *cont, Ipoint *pnt, int index)
{
  Ipoint ipnt, *tpts;     
  int size;
  int i;
  float startz;

  if (index > cont->psize)
    index = cont->psize;

  if (index < 0)
    return(0);

  if (!pnt)
    return(cont->psize);

  ipnt = *pnt;
  size = cont->psize;

  if (size)
    startz = cont->pts[0].z;
  else
    startz = ipnt.z;

  if (cont->pts){
    tpts = (Ipoint *)realloc((Ipoint *)cont->pts, (size + 1) * sizeof(Ipoint));
  }else{
    tpts = (Ipoint *)malloc(sizeof(Ipoint));
  }
     

  /* DNM: handle sizes appropriately in each area below */
  /* 11/17/04: no longer dump the data if this fails, just return */
  if (tpts == NULL)
    return(0);

  cont->pts = tpts;

  if (cont->sizes)
    cont->sizes = (float *) 
      realloc(cont->sizes, (size + 1) * sizeof(float));

  cont->psize++;
  for(i = cont->psize - 1; i > index; i--){
    cont->pts[i] = cont->pts[i - 1];
    imodLabelItemMove(cont->label, i, i - 1);
  }
  cont->pts[index] = ipnt;

  if (cont->sizes) {
    for(i = cont->psize - 1; i > index; i--)
      cont->sizes[i] = cont->sizes[i - 1];
    cont->sizes[index] = -1;
  }

  /* 6/12/05: Shift general storage indices >= index */
  istoreShiftIndex(cont->store, index, -1, 1);

  /* DNM:  check whether z matches start of contour
     and set wild flag if it doesn't */
  /* DNM 7/22/04: test for equality of nearest int instead of exact equality */
  if ((int)floor(startz + 0.5) != (int)floor(ipnt.z + 0.5))
    cont->flags |= ICONT_WILD;

  return(cont->psize);
}

/*!
 * Deletes the point at [index] from contour [cont].  Frees the point and size
 * arrays if the only point is deleted, and manages point sizes, labels, and general
 * storage items correctly.  Returns the size of the contour or -1 for error.
 */
int imodPointDelete(Icont *cont, int index)
{
  int i;
  
  if (!cont)
    return(-1);
     
  if (index < 0 || index > cont->psize - 1)
    return(-1);
     
  /* DNM: handle sizes appropriately in each area below */

  imodLabelItemDelete(cont->label, index);
  for ( i = index; i < cont->psize - 1; i ++){
    cont->pts[i].x = cont->pts[i + 1].x;
    cont->pts[i].y = cont->pts[i + 1].y;
    cont->pts[i].z = cont->pts[i + 1].z;
    imodLabelItemMove(cont->label, i, i + 1);
  }
  if (cont->sizes)
    for ( i = index; i < cont->psize - 1; i ++)
      cont->sizes[i] = cont->sizes[i+1];

  /* Manage the storage list.  Should catch error, but not clear what to
     do if one occurs */
  istoreDeletePoint(cont->store, index, cont->psize);
  cont->psize--;

  /* 3/26/01: per Lambert Zijp, move this cleanup from top of function
     (where it never happened) to down here, to prevent memory leak.
     Also, set pts to NULL just to be safe */
  if (!cont->psize){
    if (cont->pts)
      free (cont->pts);
    cont->pts = NULL;
    if (cont->sizes)
      free (cont->sizes);
    cont->sizes = NULL;
    return(0);
  }

  /* DNM: if the wild flag is set, recheck contour */
  if (cont->flags & ICONT_WILD)
    imodel_contour_check_wild(cont);

  return(cont->psize);
}

/*!
 * Sets the size of point at [pt] in contour [cont] to [size].  Creates a
 * size array if necessary.
 */
void imodPointSetSize(Icont *cont, int pt, float size)
{
  int i;

  if (!cont->sizes) {
    cont->sizes = (float *)malloc(cont->psize * sizeof(float));
    if (!cont->sizes)
      return;
    for (i = 0; i < cont->psize; i++)
      cont->sizes[i] = -1;
  }
  cont->sizes[pt] = size;
}
           
/*!
 * Returns the size of the point at [pt] in contour [cont] and object [obj];
 * if no size was set for his point it returns the default size for object.
 */
float imodPointGetSize(Iobj *obj, Icont *cont, int pt)
{
  if (!cont->sizes)
    return (obj->pdrawsize);
  if (cont->sizes[pt] < 0)
    return (obj->pdrawsize);
  return (cont->sizes[pt]);
}
           
/*!
 * Returns distance in the X/Y plane between [pnt1] and [pnt2].
 */
double imodel_point_dist(Ipoint *pnt1, Ipoint *pnt2)
{

  float distance;
     
  distance = ((pnt1->x - pnt2->x) * (pnt1->x - pnt2->x) ) +
    ((pnt1->y - pnt2->y) * (pnt1->y - pnt2->y));
     
  return( sqrt(distance));
}

/*!
 * Returns distance in the X/Y plane between [pnt1] and [pnt2].
 */
float imodPointDistance(Ipoint *pnt1, Ipoint *pnt2)
{
  float distance;
     
  distance = ((pnt1->x - pnt2->x) * (pnt1->x - pnt2->x) ) +
    ((pnt1->y - pnt2->y) * (pnt1->y - pnt2->y));
  distance = (float)sqrt((double)distance);
  return(distance);
}

/*!
 * Returns distance in 3D between points [p1] and [p2], with coordinates
 * scaled by the values in [scale].
 */
float imodPoint3DScaleDistance(Ipoint *p1, Ipoint *p2, Ipoint *scale)
{
  float xd   = (p1->x - p2->x) * scale->x; 
  float yd   = (p1->y - p2->y) * scale->y; 
  float zd   = (p1->z - p2->z) * scale->z; 
  double dist = (xd * xd) + (yd * yd) + (zd * zd);
  return((float)sqrt(dist));
}

/* Huh?  Used by icont_alldist which is used by bad principal axis */
float imodPointLineDistance(Ipoint *ln, Ipoint *p)
{
  float l = (ln->x * p->x) + (ln->y * p->y) + ln->z;
  float d = (ln->x*ln->x)+(ln->y*ln->y);

  if (d)
    return((float)sqrt((l*l)/d));
  else{
    printf("ipd00:\n");
    return(0.0);
  }
}

/*!
 * Returns angle of line from origin to X, Y coordinates of [pt], between
 * -pi/2 and pi/2
 */
double imodPoint2DAngle(Ipoint *pt)
{
  double angle;

  if (pt->x != 0.0)
    angle = atan(pt->y / pt->x);
  else if (pt->y != 0.0)
    angle = 1.570796327;
  else
    angle = 0.0;
  return(angle);
}

/*!
 * Returns the {square} of the distance between point [p] and the line segment
 * between [lp1] and [lp2].  In [tval], it returns the parameter specifying 
 * the position along the segment of the point of closest approach (between 0
 * at [lp1] and 1 at [lp2]).
 */
float imodPointLineSegDistance(Ipoint *lp1, Ipoint *lp2, Ipoint *p,
                               float *tval)
{
  float a,b,c,t,d,e,f;

  t = 0.;
  a = lp2->x - lp1->x;
  b = lp2->y - lp1->y;
  c = lp2->z - lp1->z;
  if (a || b || c)
    t = (a * (p->x - lp1->x) + b * (p->y - lp1->y) + c * (p->z - lp1->z)) /
      (a * a + b * b + c * c);
  t = B3DMAX(0., B3DMIN(1., t));
  *tval = t;
  d = a * t + lp1->x - p->x;
  e = b * t + lp1->y - p->y;
  f = c * t + lp1->z - p->z;
  return(d *d + e * e + f * f);
}

/*!
 * Returns the closest distance between the line segments in [cont] and 
 * point [pt], or returns 0 for an empty contour or the distance to the single point in
 * a one-point contour.  Set [open] non-zero for an open contour (i.e., to exclude the 
 * segment connecting end to start).  Set [threeD] non-zero to have the distances measured
 * in 3D instead of in the X/Y plane.  Also returns in [closest] the index of the point 
 * at the beginning of the closest line segment.
 */
float imodPointContDistance(Icont *cont, Ipoint *pt, int open, int threeD, int *closest)
{
  float mindist = 1.e36, t = 0., dist, dx, dy;
  Ipoint *cpts = cont->pts;
  Ipoint scale = {1., 1., 1.};
  int i, ni, numSeg = cont->psize;
  *closest = 0;
  if (!cont->psize)
    return 0;
  if (cont->psize == 1)
    return threeD ? imodPoint3DScaleDistance(pt, cpts, &scale) :
      imodPointDistance(pt, cpts);
  if (open)
    numSeg--;
  for (i = 0; i < numSeg; i++) {
    ni = (i + 1) % cont->psize;
    if (threeD) {
      dist = imodPointLineSegDistance(&cpts[i], &cpts[ni], pt, &t);
    } else {
      dx = cpts[ni].x - cpts[i].x;
      dy = cpts[ni].y - cpts[i].y;
      if (dx || dy)
        t = ((pt->x - cpts[i].x) * dx + (pt->y - cpts[i].y) * dy) / (dx * dx + dy * dy);
      t = B3DMIN(1., B3DMAX(0., t));
      dx = pt->x -(cpts[i].x + t * dx);
      dy = pt->y -(cpts[i].y + t * dy);
      dist = dx * dx + dy * dy;
    }
    if (dist < mindist) {
      *closest = i;
      mindist = dist;
    }
  }
  return (float)sqrt((double)mindist);
}

/*!
 * Returns dot product of [pnt1] and [pnt2].
 */
float imodPointDot(Ipoint *pnt1, Ipoint *pnt2)
{
  return( (pnt1->x * pnt2->x) + (pnt1->y * pnt2->y) + 
          (pnt1->z * pnt2->z));

}

/*!
 * Returns cross product of [pnt1] and [pnt2].
 */
void imodPointCross( Ipoint *v1, Ipoint *v2, Ipoint *rp)
{
  /*find the normal for the plane with the points p1,p2,p3 */
  rp->x = (v1->y * v2->z) - (v1->z * v2->y);
  rp->y = (v1->z * v2->x) - (v1->x * v2->z);
  rp->z = (v1->x * v2->y) - (v1->y * v2->x);
  return;
}

/*!
 * Normalizes vector in [n] to length 1.
 */
void imodPointNormalize(Ipoint *n)
{
  float dist;

  dist = (n->x * n->x) + (n->y * n->y) + (n->z * n->z);
  dist = (float)sqrt((double)dist);
  if (dist == 0.0){
    n->x = 0;
    n->y = 0;
    n->z = 0;
  }
  else{
    dist = 1/dist;
      
    n->x *= dist;
    n->y *= dist;
    n->z *= dist;
  }
  return;
}


/*!
 * Returns 1 if point [a] equals point [b], 0 otherwise.
 */
int imodPointIsEqual(Ipoint *a, Ipoint *b)
{
  if ((a->x == b->x) && (a->y == b->y) && (a->z == b->z))
    return(1);
  return(0);
}

/*!
 * Returns 1 if the line segment between [a] and [b] intersects the line 
 * segment between [c] and [d], 0 otherwise.
 */
int imodPointIntersect(Ipoint *a, Ipoint *b, Ipoint *c, Ipoint *d)
{

  float dx1, dy1, dx2, dy2, dxs, dys;
  double den, tnum, unum, t, u;

  /* First compute parameters t and u for point of intersection along each
     extended line (t and u between 0 and 1 parameterize each line segment) */
  dx1 = b->x - a->x;
  dy1 = b->y - a->y;
  dx2 = d->x - c->x;
  dy2 = d->y - c->y;
  dxs = c->x - a->x;
  dys = c->y - a->y;
  den = dx2 * dy1 - dx1 * dy2;
  tnum = dys * dx2 - dxs * dy2;
  unum = dx1 * dys - dy1 * dxs;

  /* Check for parallel lines */
  if (fabs(den) < 1.e-20 || 
      fabs(den) < 1.e-6 * B3DMAX(fabs(tnum), fabs(unum))) {

    /* For parallel lines, check segment length, then check each endpoint
       against the other segment for being within the segment */
    den = dx1 * dx1 + dy1 * dy1;
    if (fabs(den) < 1.e-20)
      return 0;
    t = (dx1 * (c->x - a->x) + dy1 * (c->y - a->y)) / den;
    if (t >= -0.000001 && t <= 1.000001 && fabs(a->x + t * dx1 - c->x) < 1.e-6
        && fabs(a->y + t * dy1 - c->y) < 1.e-6)
      return 1;
    t = (dx1 * (d->x - a->x) + dy1 * (d->y - a->y)) / den;
    if (t >= -0.000001 && t <= 1.000001 && fabs(a->x + t * dx1 - d->x) < 1.e-6
        && fabs(a->y + t * dy1 - d->y) < 1.e-6)
      return 1;

    den = dx2 * dx2 + dy2 * dy2;
    if (fabs(den) < 1.e-20)
      return 0;
    t = (dx2 * (a->x - c->x) + dy2 * (a->y - c->y)) / den;
    if (t >= -0.000001 && t <= 1.000001 && fabs(c->x + t * dx2 - a->x) < 1.e-6
        && fabs(c->y + t * dy2 - a->y) < 1.e-6)
      return 1;
    t = (dx2 * (b->x - c->x) + dy2 * (b->y - c->y)) / den;
    if (t >= -0.000001 && t <= 1.000001 && fabs(c->x + t * dx2 - b->x) < 1.e-6
        && fabs(c->y + t * dy2 - b->y) < 1.e-6)
      return 1;
    return 0;
  }

  /* Non-parallel lines: test for point of intersection within each line */
  t = tnum / den;
  u = unum / den;
  return (t >= 0. && t <= 1. && u >= 0. && u <= 1.) ? 1 : 0;
}

/* called from uncompiled part of imodinfo and unused and suspect 
   imodObjectClip */
int imodPointPlaneEdge(Ipoint *rpt, Iplane *plane, int planes,
                       Ipoint *pt1, Ipoint *pt2)
{
  Ipoint step;
  Ipoint cpt;
  Ipoint *tpt;
  float pdist = imodPointDistance(pt1, pt2);
  int p1f     = imodPlanesClip(plane, planes, pt1);
  int p2f     = imodPlanesClip(plane, planes, pt2);
  int i, li;

  if (  pdist <= 0.0f )   return(-1);
  if ( (p1f) && (p2f) )   return(1);
  if ( (!p1f) && (!p2f) ) return(2);
     
  step.x = (pt1->x - pt2->x) / pdist;
  step.y = (pt1->y - pt2->y) / pdist;
  step.z = (pt1->z - pt2->z) / pdist;
     
  if (!imodPlanesClip(plane, planes, pt1)){
    step.x *= -1;
    step.y *= -1;
    step.z *= -1;
    tpt = pt2;
    pt2 = pt1;
    pt1 = tpt;
  }

  li = pdist + 0.5f;
  cpt = *pt1;
  *rpt = *pt1;
  for(i = 0; i < li; i++){
    cpt.x += step.x;
    cpt.y += step.y;
    cpt.z += step.z;
    if (imodPlanesClip(plane, planes, &cpt))
      return(0);
    *rpt = cpt;
      
  }
  return(0);
}

/*!
 * Returns the area of the 3D triangle formed by [p1], [p2], and [p3]
 */
float imodPointArea(Ipoint *p1, Ipoint *p2, Ipoint *p3)
{
  Ipoint n,n1,n2;

  n1.x = p1->x - p2->x;
  n1.y = p1->y - p2->y;
  n1.z = p1->z - p2->z;
  n2.x = p3->x - p2->x;
  n2.y = p3->y - p2->y;
  n2.z = p3->z - p2->z;
  imodPointCross(&n1, &n2, &n);
     
  return((float)(sqrt(n.x*n.x + n.y*n.y + n.z*n.z) * 0.5));
}

/*!
 * Returns the area of the 3D triangle formed by [p1], [p2], and [p3], with
 * point coordinates scaled by [s].
 */
float imodPointAreaScale(Ipoint *p1, Ipoint *p2, Ipoint *p3, Ipoint *s)
{
  Ipoint n,n1,n2;
     
  n1.x = (p1->x - p2->x) * s->x;
  n1.y = (p1->y - p2->y) * s->y;
  n1.z = (p1->z - p2->z) * s->z;
  n2.x = (p3->x - p2->x) * s->x;
  n2.y = (p3->y - p2->y) * s->y;
  n2.z = (p3->z - p2->z) * s->z;
  imodPointCross(&n1, &n2, &n);
     
  return((float)(sqrt(n.x*n.x + n.y*n.y + n.z*n.z) * 0.5));
}

/* Based on algorithm in "Computational Geometry in C", Joseph O'Rourke,
   1998, with modifications to speed up search for ray crossings.  First
   written in fortran then translated to C.
*/

/*!
 * Returns 1 if the point [pt] is inside or on the contour [cont], otherwise
 * returns 0.  Uses the same code as in @@cfutils.html#InsideContour@.
 */
int imodPointInsideCont(Icont *cont, Ipoint *pt)
{
  Ipoint *pts = cont->pts;
  int rstrad, lstrad;

  int nrcross=0;
  int nlcross=0;
  int np = cont->psize;
  float x = pt->x;
  float y = pt->y;
  float xp, yp, xc, yc, xcross;
  int j, jl;

  yp = pts[np - 1].y;
  j = 0;
  while(j < np) {
    if (yp < y) {
              
      /* if last point below y, search for first that is not below */
               
      while(j < np && pts[j].y < y) 
        j++;

    } else if (yp > y) {

      /* or if last point above y, search for first that is not 
         above */

      while(j < np && pts[j].y > y)
        j++;
    }

    if (j < np) {
      jl=j-1;
      if (jl < 0)
        jl=np - 1;
      xp=pts[jl].x;
      yp=pts[jl].y;
      xc=pts[j].x;
      yc=pts[j].y;
            
      /*  return if point is a vertex */

      if (x == xc && y == yc)
        return 1;
            
      /* does edge straddle the ray to the right or the left? */

      rstrad=(yc > y) != (yp > y);
      lstrad=(yc < y) != (yp < y);
      if (lstrad || rstrad) {
              
        /* if so, compute the crossing of the ray, add up crossings */

        xcross=xp+(y-yp)*(xc-xp)/(yc-yp);
        if (rstrad && (xcross > x))
          nrcross++;
        if (lstrad && (xcross < x))
          nlcross++;
      }
      yp=yc;
    }
    j++;
  }
  
  /*  if left and right crossings don't match, it's on an edge
      otherwise, inside iff crossings are odd */
  if (nrcross % 2 !=  nlcross % 2)
    return 1;
  return((nrcross % 2) > 0);
}

/*!
 * Tests whether the point [x], [y] is inside any of the contours in object [obj] listed
 * in [list], where [nlist] is the number of entries in [list].  If the point is inside 
 * a contour, it returns the contour number; otherwise it returns -1.
 */
int imodPointInsideArea(Iobj *obj, int *list, int nlist, float x, float y)
{
  int i;
  Ipoint pnt;
  pnt.x = x;
  pnt.y = y;
  pnt.z = 0.;
  for (i = 0; i < nlist; i++) {
    if (imodPointInsideCont(&obj->cont[list[i]], &pnt))
      return list[i];
  }
  return -1;
}

/*!
 * Makes a list of the contours in object [obj] on the Z section closest to the Z value
 * [iz].  The contour numbers are returned in [list] and the number of contours in 
 * [nlist].  [listSize] specifies the size of the [list] array.  Assumes the contours are
 * planar and tests the Z value only of the first point.  Returns 1 if there are no 
 * contours in the object or -1 if there are more than [listSize] contours on the plane.
 */
int makeAreaContList(Iobj *obj, int iz, int *list, int *nlist, int listSize)
{
  int co, dzmin, izmin, zco, dz;
  izmin = -999;
  dzmin = 100000;
  for (co = 0; co < obj->contsize; co++) {
    if (!obj->cont[co].psize)
      continue;
    zco = B3DNINT(obj->cont[co].pts[0].z);
    dz = B3DMAX(iz - zco, zco - iz);
    if (dz < dzmin) {
      dzmin = dz;
      izmin = zco;
    }
  }

  if (izmin == -999)
    return 1;
  *nlist = 0;
  for (co = 0; co < obj->contsize; co++) {
    if (!obj->cont[co].psize)
      continue;
    zco = B3DNINT(obj->cont[co].pts[0].z);
    if (zco == izmin) {
      if (*nlist == listSize - 1)
        return -1;
      list[(*nlist)++] = co;
    }
  }
  return 0;
}

