/*  IMOD VERSION 2.50
 *
 *  ipoint.c -- Point editing functions for imodel models.
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
    Revision 3.3  2004/11/05 18:53:00  mast
    Include local files with quotes, not brackets

    Revision 3.2  2004/09/10 21:33:46  mast
    Eliminated long variables

*/

#include <math.h>
#include "imodel.h"

Ipoint *imodPointGet(struct Mod_Model *imod)
{
  Icont *cont;

  if (imod->cindex.point < 0)
    return((Ipoint *)NULL);

  cont = imodContourGet(imod);
  if (!cont)
    return((Ipoint *)NULL);

  if (imod->cindex.point >= cont->psize)
    imod->cindex.point = cont->psize - 1;
  if (imod->cindex.point < 0)
    return((Ipoint *)NULL);

  return( &(cont->pts[imod->cindex.point]));
}

Ipoint *imodPointGetFirst(Imod *imod)
{
  int ob, co, pt;

  if (!imod) return(NULL);
  imodGetIndex(imod, &ob, &co, &pt);
  imodSetIndex(imod, ob, co, 0);
  return(imodPointGet(imod));
}

Ipoint *imodPointGetNext(Imod *imod)
{
  int ob, co, pt;
  Icont *cont;

  if (!imod) return(NULL);
  imodGetIndex(imod, &ob, &co, &pt);
     
  cont = imodContourGet(imod);
  if (!cont) return(NULL);
  if (!cont->psize) return(NULL);

  pt++;
  if (pt >= cont->psize) return(NULL);
  imodSetIndex(imod, ob, co, pt);
  return(imodPointGet(imod));
}

int imodPointAppend(Icont *cont, Ipoint *pnt)
{
  return(imodel_point_add(cont, pnt, cont->psize));
}

int     imodPointAdd(Icont *cont, Ipoint *pnt, int index)
{
  return(imodel_point_add(cont, pnt, index));
}

/* Add a point to a contour at the given index.  Returns number of points
   in contour, or 0 if an error occurs */
int imodel_point_add(Icont *cont, Ipoint *pnt, int index)
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

  /* DNM:  check whether z matches start of contour
     and set wild flag if it doesn't */
  /* DNM 7/22/04: test for equality of nearest int instead of exact equality */
  if ((int)floor(startz + 0.5) != (int)floor(ipnt.z + 0.5))
    cont->flags |= ICONT_WILD;

  return(cont->psize);
}


int imodPointDelete(Icont *cont, int index)
{
  return(imodel_point_delete(cont, index));
}
int imodel_point_delete(Icont *cont, int index)
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

/* DNM: new functions to set and get size; Set creates size array if it does 
   not exist already, and Get returns either the actual or the default size */

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
           
float imodPointGetSize(Iobj *obj, Icont *cont, int pt)
{
  if (!cont->sizes)
    return (obj->pdrawsize);
  if (cont->sizes[pt] < 0)
    return (obj->pdrawsize);
  return (cont->sizes[pt]);
}
           


double imodel_point_dist(struct Mod_Point *pnt1, struct Mod_Point *pnt2)
{

  float distance;
     
  distance = ((pnt1->x - pnt2->x) * (pnt1->x - pnt2->x) ) +
    ((pnt1->y - pnt2->y) * (pnt1->y - pnt2->y));
     
  return( sqrt(distance));
}

float imodPointDistance(Ipoint *pnt1, Ipoint *pnt2)
{
  float distance;
     
  distance = ((pnt1->x - pnt2->x) * (pnt1->x - pnt2->x) ) +
    ((pnt1->y - pnt2->y) * (pnt1->y - pnt2->y));
  distance = (float)sqrt((double)distance);
  return(distance);
}

float imodPoint3DScaleDistance(Ipoint *p1, Ipoint *p2, Ipoint *scale)
{
  float xd   = (p1->x - p2->x) * scale->x; 
  float yd   = (p1->y - p2->y) * scale->y; 
  float zd   = (p1->z - p2->z) * scale->z; 
  double dist = (xd * xd) + (yd * yd) + (zd * zd);
  return((float)sqrt(dist));
}

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

float imodPointLineSegDistance(Ipoint *lp1, Ipoint *lp2, Ipoint *p)
{
  float dist = 0.0f;
  float a,b,c,l;

  a = lp2->y - lp1->y;
  b = lp1->x - lp2->x;
  c = (-b * lp1->y) - (a +lp1->x);
     
  l = (a * p->x) + (b * p->y) + (c);
  dist = (l * l) / ( (a*a) + (b*b));
  dist = sqrt(dist);
  return(dist);
}

float imodPointDot(Ipoint *pnt1, Ipoint *pnt2)
{
  return( (pnt1->x * pnt2->x) + (pnt1->y * pnt2->y) + 
          (pnt1->z * pnt2->z));

}

void imodPointCross( Ipoint *v1, Ipoint *v2, Ipoint *rp)
{
  /*find the normal for the plane with the points p1,p2,p3 */
  rp->x = (v1->y * v2->z) - (v1->z * v2->y);
  rp->y = (v1->z * v2->x) - (v1->x * v2->z);
  rp->z = (v1->x * v2->y) - (v1->y * v2->x);
  return;
}

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


int imodPointIsEqual(Ipoint *a, Ipoint *b)
{
  if ((a->x == b->x) && (a->y == b->y) && (a->z == b->z))
    return(1);
  return(0);
}

int imodPointIntersect(Ipoint *a, Ipoint *b,
                       Ipoint *c, Ipoint *d)
{
  /* question: does line segment a-b intersect linesegment c-d? */

  float slope[2], intery[2];
  float xd[2], yd[2];
  float xi;  /* xcoord where lines intercept. */

  /* first check for common points. */
  if ((imodPointIsEqual(a,c)) || imodPointIsEqual(a,d) ||
      (imodPointIsEqual(b,c)) || imodPointIsEqual(b,d))
    return(1);     
     
  xd[0] = b->x - a->x;
  xd[1] = d->x - c->x;

  /* slope must be defined for both line segments. */
  if ((xd[0]) && (xd[1])){
    slope[0] = (b->y - a->y) / xd[0];
    slope[1] = (d->y - c->y) / xd[1];
    intery[0] = a->y - (slope[0] * a->x);
    intery[1] = c->y - (slope[1] * c->x);

    if (slope[0] == slope[1]){
      if (intery[0] == intery[1]){
            
      }
      return(0);
    }
      
    xi = (intery[1] - intery[0]) / (slope[0] - slope[1]);

    /* if both lines segments contain xi return true */
    if ( (((a->x > xi)&&(b->x < xi)) || ((a->x < xi)&&(b->x > xi)))  &&
         (((c->x > xi)&&(d->x < xi)) || ((c->x < xi)&&(d->x > xi))))
      return(1);

    return(0);
  }

  if (xd[1]){
    if (a->y > b->y){
      yd[0] = b->y; yd[1] = a->y;
    }else{
      yd[0] = a->y; yd[1] = b->y;
    }
    if ((c->y > yd[0]) && (c->y < yd[1]))
      return(1);
      
    if ((d->y > yd[0]) && (d->y < yd[1]))
      return(1);
  }
     
     

  return(0);
}


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
