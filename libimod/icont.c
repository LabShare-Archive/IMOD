/*  IMOD VERSION 2.50
 *
 *  icont.c -- Library of contour handleing routines.
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
    Revision 3.5  2004/11/05 18:53:00  mast
    Include local files with quotes, not brackets

    Revision 3.4  2004/09/10 21:33:46  mast
    Eliminated long variables

*/

#include <math.h>
#include <string.h>
#include "imodel.h"
#include "b3dutil.h"

/* Library
 * Functions                              Descriptions
 * ---------------------------------------------------------------------------
 * Icont *imodContourNew(void);           Create and init one contour.
 * Icont *imodContoursNew(int size);      Create and init array of contours.
 * int    imodContourDelete(Icont *cont); Delete a contour with all data.
 * int    imodContoursDelete(Icont *cont, Delete an array of contours.
 *                           int   size);
 * int    imodContoursDeleteToEnd         Delete contours to end of array
 *               (Iobj *obj, int keep); 
 * int    imodContourCopy(Icont *from,    Copy contour structure only.
 *                        Icont *to);
 * Icont *imodContourDup(Icont *cont);    Create a duplicate contour.
 *
 * void   imodContourMakeDirection
 *        (Icont *cont, int direction);   Make direction (conter-)clockwise.
 * float  imodContourArea(Icont *cont);   Return area in square pixels.
 * Icont *imodContourJoin(Icont *c1,      Make one contour out of two.
 *                Icont *c2, int fill);
 * void   imodContourFill(Icont *cont);   Fill in data between points.
 * int    imodContourShave(Icont *cont,   Remove points closer then dist apart.
 *                         double dist)
 * void   imodContourReduce(Icont *cont,  Reduce # of points in contour with
 *                           float tol)   maximum error tol
 */


Icont *imodContourNew(void)
{
  struct Mod_Contour *cont;
     
  cont = (struct Mod_Contour *)
    malloc(sizeof(struct Mod_Contour));

  if (cont == NULL)
    return((struct Mod_Contour *)NULL);
  imodContourDefault(cont);
  return(cont);
}

Icont *imodContoursNew(int size)
{
  Icont *cont;
  int co;

  if (size <= 0)
    return(NULL);

  cont = (Icont *)malloc( size * sizeof(Icont));
  if (!cont)
    return(NULL);
     
  for(co = 0; co < size; co++){
    imodContourDefault(&cont[co]);
  }
  return(cont);
}

void imodContourDefault(Icont *cont)
{
  if (!cont) return; 
  cont->pts    = NULL;
  cont->psize  = 0;
  cont->flags  = 0;
  cont->type   = 0;
  cont->surf   = 0;
  cont->label  = NULL;
  cont->sizes  = NULL;
  cont->store  = NULL;
}

int imodContourCopy(Icont *from, Icont *to)
{
  if (!from)
    return(-1);
  if (!to)
    return(-1);
  memcpy(to, from, sizeof(Icont));
  return(0);
}

/* 
 *  Create a new contour containing the same point and label data 
 *  as the input contour.
 */

Icont *imodContourDup(Icont *cont)
{
  Icont *ocont; /* output contour */

  if (cont == NULL)
    return(NULL);
  ocont = imodContourNew();
  if (!ocont)
    return(NULL);

  imodContourCopy(cont, ocont);
  if (cont->psize) {
    ocont->pts = (Ipoint *)malloc(sizeof(Ipoint) * ocont->psize);
    if (!cont->pts){
      free(ocont);
      return(NULL);
    }
    memcpy(ocont->pts, cont->pts, sizeof(Ipoint) * cont->psize);
  }

  if (cont->sizes) {
    ocont->sizes = (float *)malloc(sizeof(float) * ocont->psize);
    if (ocont->sizes)
      memcpy(ocont->sizes, cont->sizes, sizeof(float) * cont->psize);
  }

  /* DNM 11/15/04: make new routine to duplicate label, duplicate store list */
  ocont->label = imodLabelDup(cont->label);
  ocont->store = ilistDup(cont->store);
  return(ocont);
}

int imodContourDelete(Icont *cont)
{
  char *lbl;
  if (cont == NULL)
    return(-1);
  imodel_contour_clear(cont);
  /*     imodLabelDelete(cont->label); DNM: it's already done*/

  free(cont);
  return(0);
}

int imodContoursDelete(Icont *cont, int size)
{
  int co;

  if (cont == NULL)
    return(-1);

  for(co = 0; co < size; co++)
    imodel_contour_clear(&(cont[co]));
  free(cont);
  return(0);
}

int imodContoursDeleteToEnd(Iobj *obj, int keep)
{
  int co;

  if (obj == NULL)
    return(-1);

  if (obj->contsize < keep)
    return(-1);

  if (obj->contsize == keep)
    return(0);

  for(co = keep; co < obj->contsize; co++)
    imodel_contour_clear(&(obj->cont[co]));

  obj->contsize = keep;
  /* Change contour array to new size */
  if (obj->contsize){
    obj->cont = (struct Mod_Contour *)
      realloc(obj->cont, obj->contsize * sizeof(struct Mod_Contour));
    if (!obj->cont)
      return(-1);
     
  }else{
    free(obj->cont);
    obj->cont = NULL;
  }
  return(0);
}

Icont *imodContourGet(struct Mod_Model *imod)
{
  struct Mod_Object *obj;
     
  obj = imodel_object_get(imod);
  if (obj == NULL)
    return((struct Mod_Contour *)NULL);

  /* DNM 3/9/01: need to test for index too high also in case of corrupt
     model */
  if (imod->cindex.contour < 0 || imod->cindex.contour >= obj->contsize)
    return( (struct Mod_Contour *)NULL);
     
  return( &(obj->cont[imod->cindex.contour]));
}

Icont *imodContourGetFirst(Imod *imod)
{
  int ob, co, pt;

  if (!imod) return(NULL);
  imodGetIndex(imod, &ob, &co, &pt);
  imodSetIndex(imod, ob, 0, pt);
  return(imodContourGet(imod));
}

Icont *imodContourGetNext(Imod *imod)
{
  int ob, co, pt;
  Iobj *obj;

  if (!imod) return(NULL);
  imodGetIndex(imod, &ob, &co, &pt);
  obj = imodObjectGet(imod);
  if (!obj) return(NULL);
  if (!obj->contsize) return(NULL);
  co++;
  if (co >= obj->contsize) return(NULL);
  imodSetIndex(imod, ob, co, pt);
  return(imodContourGet(imod));
}


/* imodContourReduce reduces a set of points by selecting a
 * minimal subset of the points.  Each of the original points will be
 * within a tolerance TOL of the segments defined by the new set of points. */
void imodContourReduce(Icont *cont, float tol)
{
  int npts, noutlim, left, irt, nout, ifout, npo, ipo, its;
  int *minseg;
  int *nextpt;
  float tolsq, denom, tmin, distsq, x1, y1, x2, y2, dx, dy, dx0, dy0;
  Ipoint *tpt;

  /*      moving from right to left, look at possible segments from a given
          point going to right.  A segment is possible if all intervening
          points are within TOL of the segment.  From the given point, find
          the possible segment that involves the fewest segments to get to
          the right end.  Keep track of the endpoint of the best segment in
          NEXTPT and the total number of segments in MINSEG  */

  npts = cont->psize;
  minseg = (int *)malloc(npts * sizeof(int) + 1);
  nextpt = (int *)malloc(npts * sizeof(int) + 1);
  if (!minseg || !nextpt)
    return;
  minseg[npts - 1] = 0;
  minseg[npts - 2] = 1;
  nextpt[npts - 1] = npts;
  nextpt[npts - 2] = npts - 1;
  tolsq = tol * tol;

  /*      Stop looking at longer segments after finding NOUTLIM segments that
          are not possible */

  noutlim = 3;
  for (left = npts - 3; left >= 0; left--) {

    /*      set left edge of segment */

    minseg[left] = minseg[left + 1] + 1;
    nextpt[left] = left + 1;
    irt = left + 2;
    x1 = cont->pts[left].x;
    y1 = cont->pts[left].y;
    nout = 0;
    while (irt < npts && nout < noutlim) {
      if (1 + minseg[irt] < minseg[left]) {

        /*    look at a segment only if it will give a better path: set
              right edge */

        ifout=0;
        x2 = cont->pts[irt].x;
        y2 = cont->pts[irt].y;
        dx = x2 - x1;
        dy = y2 - y1;
        denom = dx * dx + dy * dy;
        its = left + 1;
        while (its < irt && ifout == 0) {
         
          /*     check distance of points from segment until one falls out */

          dx0 = cont->pts[its].x - x1;
          dy0 = cont->pts[its].y - y1;
         
          if(denom < 0.00001)
            distsq = dx0 * dx0 + dy0 * dy0;
          else {
            tmin = (dx0 * dx + dy0 * dy) / denom;
            distsq = (tmin * dx - dx0) * (tmin * dx - dx0) +
              (tmin * dy - dy0) * (tmin * dy - dy0);
          }
          if (distsq >= tolsq)
            ifout=1;
          its++;
        }

        /* if its an OK segment, set it up as new minimum */

        if (ifout == 0) {
          minseg[left] = 1 + minseg[irt];
          nextpt[left] = irt;
        } else
          nout++;
      }
      irt++;
    }
  }
      
  /*      when we get to the left edge, the minimal path is available by 
          following the chain of NEXTPT values */
  if (minseg[0] + 1 == npts)
    return;

  tpt = (Ipoint *)malloc(sizeof(Ipoint) * (minseg[0] + 1));

  npo = 1;
  ipo = 0;
  tpt[0] = cont->pts[0];
  while (nextpt[ipo] < npts) {
    ipo = nextpt[ipo];
    tpt[npo++] = cont->pts[ipo];
  }
  if (minseg[0] + 1 != npo)
    printf("OOPS minseg + 1 = %d, npo = %d\n",minseg[0] + 1, npo);
  cont->psize = npo;
  if (cont->pts)
    free(cont->pts);
  cont->pts = tpt;
  /*     printf ("%d to %d\n", npts, npo); */
}


/*****************************************************************************/
/* internal functions                                                        */
/*****************************************************************************/

int imodel_contour_clear(struct Mod_Contour *cont)
{
  if (!cont)
    return(-1);

  if ((cont->pts) && (cont->psize))
    free (cont->pts);
  cont->pts = NULL;
  if (cont->sizes)
    free(cont->sizes);
  cont->sizes = NULL;
  cont->psize = 0;
  cont->flags = 0;
  cont->type = 0;
  imodLabelDelete(cont->label);
  /* DNM: set label to NULL */
  cont->label = NULL;
  ilistDelete(cont->store);
  cont->store = NULL;
  return(0);
}



int imodel_contour_newsurf(struct Mod_Object *obj, struct Mod_Contour *cont)
{
  int co;
  int max;

  if (!obj)
    return(-1);
  if (!cont)
    return(-1);
  if (!obj->contsize)
    return(-1);
  cont->surf = imodel_unused_surface(obj);
  if (obj->surfsize < cont->surf)
    obj->surfsize = cont->surf;
  return(0);
}

/* Find first unused surface number */
int imodel_unused_surface(struct Mod_Object *obj)
{
  int co;
  int max;
  int *bins;

  if (!obj)
    return(0);
  if (!obj->contsize)
    return(0);

  /* find maximum surface number */
  max = obj->cont->surf;     
  for (co = 1; co < obj->contsize; co++)
    if (max < obj->cont[co].surf)
      max = obj->cont[co].surf;

  /* Count number of contours at each surface */
  bins = (int *)malloc(sizeof(int) * (max + 1));
  if (!bins)
    return(0);
  for (co = 0; co <= max; co++)
    bins[co] = 0;
  for (co = 0; co < obj->contsize; co++)
    if (obj->cont[co].surf >= 0)
      bins[obj->cont[co].surf]++;

  /* find first empty bin */
  for (co = 0; co <= max; co++)
    if (!bins[co])
      break;

  free(bins);
  return(co);
}
     

/* returns area of contour in square pixels. */
float imodContourArea(Icont *cont)
{
  Ipoint n;
  int i, next;
  float retval;

  if (!cont)
    return(0);
  if (cont->psize < 3)
    return(0);
  if (!cont->pts)
    return(0);

  n.x = n.y = n.z = 0;

  for(i = 0; i < cont->psize; i++){
    (i == cont->psize - 1) ? (next = 0) : (next = i + 1);
    n.x += (cont->pts[i].y * cont->pts[next].z) 
      - (cont->pts[i].z * cont->pts[next].y);
    n.y += (cont->pts[i].z * cont->pts[next].x)
      - (cont->pts[i].x * cont->pts[next].z);
    n.z += (cont->pts[i].x * cont->pts[next].y)
      - (cont->pts[i].y * cont->pts[next].x);
  }
  retval = (float)(sqrt(n.x*n.x + n.y*n.y + n.z*n.z) * 0.5f);
  return(retval);
}

int imodel_contour_area(struct Mod_Contour *icont)
{
  struct Mod_Contour *cont;
  int pix = 0;
  int i, j;
  int xmin, xmax;
  int bgnpt,endpt;
  int scanline;

  if (!icont)
    return(0);

  if (icont->psize < 3)
    return(0);

  cont = imodel_contour_scan(icont);
     
  for(i = 0, scanline = 0; i < cont->psize - 1; i++, scanline++){
    bgnpt = i;
    while (cont->pts[i].y == cont->pts[i+1].y){
      ++i;
      if (i == cont->psize){
        i--;
        break;
      }
    }
    endpt = i;
      
    /* check for odd amount of scans, shouldn't happen! */
    if (! ( (endpt-bgnpt)% 2)){
      continue;
      /*           printf(" (Error scan line %d,%d)\n", scanline, y);  */
    }
    else{
      for(j = bgnpt; j < endpt; j++){
        xmin = cont->pts[j].x;
        xmax = cont->pts[j+1].x;
        if (xmin >= cont->surf)
          pix += xmax - xmin;
        j++;
      }
    }
  }
  imodContourDelete(cont);
  return(pix);
}





double imodel_contour_length(struct Mod_Contour *cont)
{
  double dist = 0;
  int pt;

  if (!cont)
    return(-1);

  if (cont->psize < 2)
    return(dist);
     
  for(pt = 0; pt < (cont->psize - 1); pt++)
    dist += imodel_point_dist( &(cont->pts[pt]), &(cont->pts[pt + 1]) ); 

  return(dist);

}


double imodContourMoment(Icont *cont, int a, int b)
{
  Icont *scont;
  int scanline;
  int i, j, pt;
  int bline, eline;
  double moment = 0.0;
  double powia;

  /* If contour is already a scan, use it; otherwise get scan contour */
  if (cont->flags & ICONT_SCANLINE)
    scont = cont;
  else {
    scont = imodel_contour_scan(cont);
    if (!scont) 
      return 0.0;
  }

  for(pt = 0; pt < scont->psize; pt+=2){
    i = scont->pts[pt].y + 0.5f;
    bline = scont->pts[pt].x + 0.5f;
    eline = scont->pts[pt + 1].x + 0.5f;
    powia = pow((double)i, (double)a);
    if (b == 0) 
      for(j = bline; j < eline; j++)
        moment += powia;
    else if (b == 1)
      for(j = bline; j < eline; j++)
        moment += powia * j;
    else
      for(j = bline; j < eline; j++){
        moment += powia * pow((double)j, (double)b);
      }
  }

  /* Delete scan contour if made one */
  if (!(cont->flags & ICONT_SCANLINE))
    imodContourDelete(scont);
  return(moment);
}

int imodContourCenterOfMass(Icont *cont, Ipoint *rpt)
{
  double M00, M01, M10;
  Icont *scont;

  rpt->x = rpt->y = rpt->z = 0.0f;
  if (!cont) return(0);
  if (!cont->psize) return(0);

  /* 3/26/01, per Lambert Zijp's suggestion, make scan contour once before
     getting moments */
  scont = imodel_contour_scan(cont);
  if (!scont) return(-1);

  M00 = imodContourMoment(scont, 0, 0);
  M01 = imodContourMoment(scont, 0, 1);
  M10 = imodContourMoment(scont, 1, 0);
  imodContourDelete(scont);
  if (M00 == 0.0) return(-1);

  rpt->x = M01/M00;
  rpt->y = M10/M00;
  rpt->z = cont->pts->z;
  return(0);
}


double imodContourCenterMoment(Icont *cont, Ipoint *org, int a, int b)
{
  Icont *scont = imodel_contour_scan(cont);
  int scanline;
  int i, j, pt;
  int bline, eline;
  double moment = 0.0;
     
  if (!scont) return 0.0;
     
  for(pt = 0; pt < scont->psize; pt+=2){
    i = scont->pts[pt].y + 0.5f - org->x;
    bline = scont->pts[pt].x + 0.5f - org->y;
    eline = scont->pts[pt + 1].x + 0.5f - org->y;
    for(j = bline; j < eline; j++){
      moment += pow(i, a) * pow(j, b);
    }
  }
  imodContourDelete(scont);
  return(moment);
}


/* Measure the circularity of a closed contour.
 * A perfect circle = 1.0, A square = 1.27...
 * A = pi R^2
 * C = 2 pi R = pi D
 * circularity = C^2/ 4 * pi * A
 */
double imodContourCircularity(Icont *cont)
{
  double c = imodel_contour_length(cont);
  double a = imodel_contour_area(cont);
  if (a == 0.0) return(1000.0);
  return( (c*c)/(12.56637062*a));
}

static double icont_alldist(Icont *cont, Ipoint *a, Ipoint *b)
{
  int pt, x, xs, xe;
  double dist = 0.0;
  Ipoint point;
  Ipoint ln;
     
  ln.x = b->y - a->y;
  ln.y = a->x - b->x;
  ln.z = (-ln.y * a->y) - (ln.x + a->x);

  if (cont->flags & ICONT_SCANLINE) {
    for(pt = 0; pt < cont->psize-1; pt+=2){
      point.y = cont->pts[pt].y;
      xs = cont->pts[pt].x;
      xe = cont->pts[pt+1].x;
      for(x = xs; x < xe; x++){
        point.x = x;
        dist += imodPointLineDistance(&ln, &point);
      }
    }
  }else{
    for(pt = 0; pt < cont->psize; pt++)
      dist += imodPointLineDistance(&ln, &cont->pts[pt]);
  }

  return(dist);
}

/* Measure the angle at which the contour is most elongated, to the precision
 * set by "precision", in degrees.
 * Returns the angle to the long axis in radians.
 * Returns the ratio of long to short axis in aspect, and the length of the
 * long axis in longaxis
 */
double imodContourLongAxis(Icont *cont, float precision, float *aspect,
                           float *longaxis)
{
  Ipoint center;
  double minangle, angle;
  int pt, itry;
  float xmin, xmax, ymin, ymax, xrot, yrot, xran, yran, sina, cosa;
  float ratio, minratio, minlong;
  double dtor = 0.017453293;
  int ntrial = 90. / precision;

  *aspect = 1.;
  *longaxis = 0.;
  if (imodContourBad(cont, 1)) return 0.0;
     
  if (cont->psize == 2){
    center.x = cont->pts[1].x - cont->pts[0].x;
    center.y = cont->pts[1].y - cont->pts[0].y;
    *aspect = 1.e6;
    *longaxis = sqrt((double)(center.x*center.x + center.y*center.y));
    return(imodPoint2DAngle(&center));
  }

  /* try every angle at the given precision from 0 to 90 degrees */
  minratio = 1.e30;
  for (itry = 0; itry < ntrial; itry++) {
    angle = itry * precision * dtor;
    cosa = cos(angle);
    sina = sin(angle);
    ymin = xmin = 1.e30;
    ymax = xmax = -1.e30;

    /* rotate points, find min and max */
    for (pt = 0; pt < cont->psize; pt++) {
      xrot = cont->pts[pt].x * cosa - cont->pts[pt].y * sina;
      yrot = cont->pts[pt].x * sina + cont->pts[pt].y * cosa;
      if (xrot > xmax)
        xmax = xrot;
      if (xrot < xmin)
        xmin = xrot;
      if (yrot > ymax)
        ymax = yrot;
      if (yrot < ymin)
        ymin = yrot;
    }
    xran = xmax - xmin;
    yran = ymax - ymin;
    ratio = 1.e6;

    /* if the ratio is a new minimum and height is less than width,
       this angle is a good as is; otherwise it'f off by 90 degrees */
    if (yran < xran) {
      if (xran > 1.e-6 * yran)
        ratio = yran / xran;
      if (ratio < minratio) {
        minratio = ratio;
        minangle = -angle;
        minlong = xran;
      }
    } else {
      if (yran > 1.e-6 * xran)
        ratio = xran / yran;
      if (ratio < minratio) {
        minratio = ratio;
        minangle = 90. * dtor - angle;
        minlong = yran;
      }
    }
  }
  *aspect = 1.e6;
  if (minratio > 1.e-6)
    *aspect = 1. / minratio;
  *longaxis = minlong;
  return (minangle);
}

/* Measure the principal axis.
 * Returns the angle to the principal axis in radians.
 * THIS CODE DOESN'T WORK
 */
double imodContourPrincipalAxis(Icont *cont)
{
  Ipoint center, *pafit;
  Icont *fcont;
  Icont *lcont = imodContourNew();
  double tdist = 0;
  double dist, angle;
  int pt;

  if (imodContourBad(cont, 1)) return 0.0;
     
  if (cont->psize == 2){
    center.x = cont->pts[1].x - cont->pts[0].x;
    center.y = cont->pts[1].y - cont->pts[0].y;
    return(imodPoint2DAngle(&center));
  }

  if (!lcont) return -0.0;

  imodContourCenterOfMass(cont, &center);
     
  /*     fcont = imodContourDup(cont); */

  /*     imodContourFill(fcont); */
  /*     fcont = imodel_contour_scan(cont);  */
  fcont = imodContourFill(cont);
  fcont->type = 0;
  for(pt = 0; pt < fcont->psize; pt++){
    if (fcont->pts[pt].y >= center.y)
      imodPointAppend(lcont, &fcont->pts[pt]);
  }

  /*     imodContourDelete(fcont);  */
  /*     fcont = imodel_contour_scan(cont);  */
  if (!lcont->pts){
    return(0.0);
  }
  tdist  = icont_alldist(fcont, &center, lcont->pts);
  pafit  = lcont->pts;

  for(pt = 1; pt < lcont->psize; pt++){
    dist = icont_alldist(fcont, &center, &lcont->pts[pt]);
    if (dist < tdist){
      tdist = dist;
      pafit = &lcont->pts[pt];
    }
  }
     
  center.x = pafit->x - center.x;
  center.y = pafit->y - center.y;
  imodContourDelete(fcont);
  imodContourDelete(lcont);

  return(imodPoint2DAngle(&center));
}

/* Measure ellipse area.
 * A = pi * (length/2) * (width/2);
 * if A is close to 1.0 the object is highly elliptical.
 *
 * Eccentricity = length/width;
 */
/* int imodContourEllipse(Icont *cont) */


/*****************************************************************************/
/* Old version of centroid calculation
 */
int imodel_contour_centroid(struct Mod_Contour *icont, struct Mod_Point *rcp,
                            double *rtw)
{
  struct Mod_Contour *cont;
  float xval, yval, weight;
  int pix = 0;
  int i, j, y;
  int xmin, xmax;
  int bgnpt,endpt;
  int scanline;

  if (!icont)
    return(-1);

  if (!icont->psize)
    return(-1);

  if (icont->psize == 1){
    rcp->x = icont->pts->x;
    rcp->y = icont->pts->y;
    rcp->z = icont->pts->z;
    return(0);
  }

  rcp->x = 0.0f;
  rcp->y = 0.0f;
  rcp->z = icont->pts[0].z;
  *rtw = 0;
  cont = imodel_contour_scan(icont);
     
  for(i = 0, scanline = 0; i < cont->psize - 1; i++, scanline++){
    y = cont->pts[i].y;
    bgnpt = i;
    /* find all scans for this y line. */
    while (cont->pts[i].y == cont->pts[i+1].y){
      ++i;
      if (i == cont->psize){
        i--;
        break;
      }
    }
    endpt = i;
      
    /* check for odd amount of scans, shouldn't happen! */
    if (! ( (endpt-bgnpt)% 2)){
      printf(" (Error scan line %d,%d)\n", scanline, y);
    }
    else{
      /* add all points in each scan. */
      for(j = bgnpt; j < endpt; j++){
        xmin = cont->pts[j].x;
        xmax = cont->pts[j+1].x;
        xval = (xmin + xmax) * 0.5f;
        yval = cont->pts[j].y;
        weight = xmax - xmin;
        rcp->x += xval * weight;
        rcp->y += yval * weight;
        *rtw += weight;
            
        if (xmin >= cont->surf)
          pix += xmax - xmin;
        j++;
      }
    }
  }
  imodContourDelete(cont);
  rcp->z *= *rtw;
  return(0);
}





/****************************************************************************/
/* FUNCTION imodel_contour_on                                               */
/* returns 0 if point is not on given contour,                              */
/* returns non zero if point is in contour list.                            */
/* list.                                                                    */
/****************************************************************************/
int imodel_contour_on(struct Mod_Contour *cont, int x, int y)
{
  int i;
  int retval = 0;
  int next, prev;

  if (cont == NULL)
    return(0);
     
  for (i = 0; i < cont->psize; i++){
      
    prev = i - 1;
    next = i + 1;
    if (prev < 0)
      prev = cont->psize - 1;
    if (next == cont->psize )
      next = 0;
      
      
    if ((cont->pts[i].x == x) && (cont->pts[i].y == y)){
      retval = 10;
           
           
      if (cont->pts[i].y == cont->pts[next].y)
        retval = 2;
      else
        while (cont->pts[i].y == cont->pts[prev].y){
          prev--;
          if (prev < 0)
            prev = cont->psize - 1;
        }
           
      if(  (cont->pts[i].y < cont->pts[prev].y)
           && (cont->pts[i].y < cont->pts[next].y))
        retval = 1;
           
      if ((cont->pts[i].y > cont->pts[prev].y)
          && (cont->pts[i].y > cont->pts[next].y))
        retval = 1;
           
      return(retval);
    }
  }
  return(0);
}


/*****************************************************************************/
/* FUNCTION: imodel_contour_unique - deletes all duplicate points.           */
/* Returns non-zero for error.                                               */
/* DNM made it remove only sequential duplicate points                       */
/*****************************************************************************/
int imodContourUnique(Icont *cont)
{
  return(imodel_contour_unique(cont));
}
int imodel_contour_unique(struct Mod_Contour *cont)
{
  int i,j;
  struct Mod_Point *pnt;

  if (cont == NULL)
    return(-1);

  i = 0;
  while (i < cont->psize && cont->psize > 1){
    j = (i + 1) % cont->psize;
    pnt = &(cont->pts[i]);
    if ((pnt->x == cont->pts[j].x) &&
        (pnt->y == cont->pts[j].y) &&
        (pnt->z == cont->pts[j].z))
      imodel_point_delete(cont, j);
    else
      i++;
  }

  return(0);
}

int imodel_contour_sorty(struct Mod_Contour *cont, int bgnpt, int endpt)
{
  int i, j;
  int sindex;
  struct Mod_Point point;
  float size;

  if (cont == NULL)
    return(-1);

  if (bgnpt < 0)
    return(-1);

  if (endpt > cont->psize - 1)
    return(-1);
     
  /* sort the y coords */

  for (i = bgnpt; i <= endpt - 1; i++){

    sindex = i;

    for(j = i + 1; j <= endpt; j++){
      if( cont->pts[sindex].y > cont->pts[j].y)
        sindex = j;
    }
    point = cont->pts[i];
    cont->pts[i] = cont->pts[sindex];
    cont->pts[sindex] = point;
    if (cont->sizes) {
      size = cont->sizes[i];
      cont->sizes[i] = cont->sizes[sindex];
      cont->sizes[sindex] = size;
    }
  }
  return(0);
}

int imodel_contour_sortx(struct Mod_Contour *cont, int bgnpt, int endpt)
{
  int i, j;
  int sindex;
  struct Mod_Point point;
  float size;

  if (cont == NULL)
    return(-1);

  if (bgnpt < 0)
    return(-1);

  if (endpt > cont->psize - 1)
    return(-1);
     
  /* sort the x coords */

  for (i = bgnpt; i <= endpt - 1; i++){

    sindex = i;

    for(j = i + 1; j <= endpt; j++){
      if( cont->pts[sindex].x > cont->pts[j].x)
        sindex = j;
    }
    point = cont->pts[i];
    cont->pts[i] = cont->pts[sindex];
    cont->pts[sindex] = point;
    if (cont->sizes) {
      size = cont->sizes[i];
      cont->sizes[i] = cont->sizes[sindex];
      cont->sizes[sindex] = size;
    }
  }
  return(0);
}

int imodel_contour_sortz(struct Mod_Contour *cont, int bgnpt, int endpt)
{
  int i, j;
  int sindex;
  struct Mod_Point point;
  float size;

  if (cont == NULL)
    return(-1);

  if (bgnpt < 0)
    return(-1);

  if (endpt > cont->psize - 1)
    return(-1);
     
  /* sort the z coords */

  for (i = bgnpt; i <= endpt - 1; i++){

    sindex = i;

    for(j = i + 1; j <= endpt; j++){
      if( cont->pts[sindex].z > cont->pts[j].z)
        sindex = j;
    }
    point = cont->pts[i];
    cont->pts[i] = cont->pts[sindex];
    cont->pts[sindex] = point;
    if (cont->sizes) {
      size = cont->sizes[i];
      cont->sizes[i] = cont->sizes[sindex];
      cont->sizes[sindex] = size;
    }
  }
  return(0);
}

int imodel_contour_sort(struct Mod_Contour *cont)
{
  struct Mod_Point point;
  int i, j;
  int sindex;
  double distance;
  double sdist;
  float size;

  if (cont == NULL)
    return(-1);

  imodel_contour_unique(cont);

  for (i = 0; i < cont->psize - 1; i++){
    sindex = i + 1;
    sdist = imodel_point_dist( &( cont->pts[i]), &(cont->pts[i + 1]) );
    for(j = i + 2; j < cont->psize; j++){
      distance = imodel_point_dist(&(cont->pts[i]), &(cont->pts[j]) );
      if (sdist == distance){
        imodel_point_delete(cont, j); 
      }
      else
        if (sdist > distance){
          sdist = distance;
          sindex = j;
        }
    }

    point = cont->pts[i + 1];
    cont->pts[i + 1] = cont->pts[sindex];
    cont->pts[sindex] = point;
    if (cont->sizes) {
      size = cont->sizes[i + 1];
      cont->sizes[i + 1] = cont->sizes[sindex];
      cont->sizes[sindex] = size;
    }
  }
  return(0);
}


/* DNM: invert the direction of a contour */

int imodel_contour_invert(struct Mod_Contour *cont)
{
  Ipoint tpt;
  float tval;
  int i, pmo;

  if (cont == NULL)
    return(-1);
  if (!cont->psize)
    return(-1);

  pmo = cont->psize - 1;
  for (i = 0; i < (cont->psize + 1) / 2; i++) {

    /* swap points */
    tpt = cont->pts[i];
    cont->pts[i] = cont->pts[pmo - i];
    cont->pts[pmo - i] = tpt;

    /* swap point sizes if any */
    if (cont->sizes) {
      tval = cont->sizes[i];
      cont->sizes[i] = cont->sizes[pmo - i];
      cont->sizes[pmo - i] = tval;
    }
  }

  /* If there are labels, invert their indices */
  if (cont->label)
    for (i = 0; i < cont->label->nl; i++)
      cont->label->label[i].index = pmo - cont->label->label[i].index;

  return(0);
}

/* set wild flag if Z is not the same throughout */
/* DNM 7/22/04: switch from perfect equality to same nearest integer */
void imodel_contour_check_wild(Icont *cont)
{
  int pt, cz;
     
  if (cont->psize)
    cz = (int)floor(cont->pts[0].z + 0.5);
  cont->flags &= ~ICONT_WILD;
  for (pt = 1; pt < cont->psize; pt++){
    if ((int)floor(cont->pts[pt].z + 0.5) != cz) {
      cont->flags |= ICONT_WILD;
      break;
    }
  }
}

int imodContourShave(Icont *cont, double dist)
{
  return(imodel_contour_shave(cont,dist));
}
int imodel_contour_shave(struct Mod_Contour *cont, double dist)
{
  int i;
  double pdist, ndist;

  if (cont->psize < 3)
    return(-1);

  for (i = 1; i < cont->psize - 1; i++){
    pdist = imodel_point_dist( &(cont->pts[i - 1]), &(cont->pts[i]));
    ndist = imodel_point_dist( &(cont->pts[i + 1]), &(cont->pts[i]));
    if (dist > 0)
      if ((pdist < dist) && (ndist < dist)){
        imodel_point_delete(cont, i);
        i--;
      }
    if (dist < 0)
      if (pdist > (-dist) && (ndist > (-dist))){
        imodel_point_delete(cont, i);
        i--;
      }
  }
  return(0);
}

/* Calculate the full 3D Bounding box of a contour.
 * ll = lower left, bottom; ur = upper right top;
 */
int imodContourGetBBox(Icont *cont, Ipoint *ll, Ipoint *ur)
{
  int pt;

  if (!cont) return(-1);
  if (!cont->psize) return(-1);

  ll->x = ur->x = cont->pts->x;
  ll->y = ur->y = cont->pts->y;
  ll->z = ur->z = cont->pts->z;

  for(pt = 0; pt < cont->psize; pt++){
    if (cont->pts[pt].x < ll->x) ll->x = cont->pts[pt].x;
    if (cont->pts[pt].y < ll->y) ll->y = cont->pts[pt].y;
    if (cont->pts[pt].z < ll->z) ll->z = cont->pts[pt].z;
    if (cont->pts[pt].x > ur->x) ur->x = cont->pts[pt].x;
    if (cont->pts[pt].y > ur->y) ur->y = cont->pts[pt].y;
    if (cont->pts[pt].z > ur->z) ur->z = cont->pts[pt].z;
  }
  return(0);
}

int imodel_contour_mm(struct Mod_Contour *cont,
                      struct Mod_Point *max,
                      struct Mod_Point *min)
{
  int i;
     
  if (!cont)
    return(-1);
     
  max->x = cont->pts->x;
  max->y = cont->pts->y;
  max->z = cont->pts->z;
  min->x = max->x;
  min->y = max->y;
  min->z = max->z;

  for (i = 1; i < cont->psize; i++){
    if (cont->pts[i].x > max->x)
      max->x = cont->pts[i].x;
    if (cont->pts[i].y > max->y)
      max->y = cont->pts[i].y;
    if (cont->pts[i].z > max->z)
      max->z = cont->pts[i].z;

    if (cont->pts[i].x < min->x)
      min->x = cont->pts[i].x;
    if (cont->pts[i].y < min->y)
      min->y = cont->pts[i].y;
    if (cont->pts[i].z < min->z)
      min->z = cont->pts[i].z;
  }
  return(0);
}


/*
 * FUNCTION: imodContourSplice
 * returns a contour with points 0 to p1 from c1 and
 * points p2 to psize - 1 from c2.
 */
Icont *imodContourSplice(Icont *c1, Icont *c2,
                         int p1, int p2)
{
  Icont *nc;
  int i;

  if ((!c1) || (!c2))
    return(NULL);

  if ((p1 < 0) || (p2 < 0) || (p1 >= c1->psize) || (p2 >= c2->psize))
    return(NULL);

  nc = imodContourNew();
  if (!nc) return(NULL);
  for(i = 0; i <= p1; i++)
    imodPointAppend(nc, &(c1->pts[i]));
  for(i = p2; i < c2->psize; i++)
    imodPointAppend(nc, &(c2->pts[i]));
  if (c1->sizes)
    for(i = 0; i <= p1; i++)
      imodPointSetSize(nc, i, c1->sizes[i]);
  if (c2->sizes)
    for(i = p2; i < c2->psize; i++)
      imodPointSetSize(nc, p1 + 1 + i - p2, c2->sizes[i]);
  return(nc);
}

/*
 * FUNCTION: imodContourBreak
 * returns contour containing points p to psize - 1, and removes
 * those points from the input contour.
 */
Icont *imodContourBreak(Icont *cont, int p)
{
  Icont *nc = imodContourNew();
  Ipoint *tpt;
  int i;

  /* check for bogus input data. */
  if (!nc) return(NULL);
  if (!cont) return(NULL);
  if (!cont->psize) return(NULL);
  if (p < 0) return(NULL);

  for(i = p; i < cont->psize; i++)
    imodPointAppend(nc, &(cont->pts[i]));

  if (cont->sizes)
    for(i = p; i < cont->psize; i++)
      imodPointSetSize(nc, i - p, cont->sizes[i]);

  if (!p){
    cont->psize = 0;
    free(cont->pts);
    if (cont->sizes) {
      free(cont->sizes);
      cont->sizes = NULL;
    }
  }else{
    tpt = (Ipoint *)realloc(cont->pts, p * sizeof(Ipoint));
    cont->psize = p;
    if (!tpt)
      cont->psize = 0;
    cont->pts = tpt;
    if (cont->sizes)
      cont->sizes = (float *)realloc(cont->sizes, 
                                     p * sizeof(float));
  }
  return(nc);
}

/*****************************************************************************/
/* FUNCTION: imodel_contour_move                                             */
/*****************************************************************************/
int imodel_contour_move(void){return(0);}

void imodel_contour_whole(struct Mod_Contour *cont)
{
  int i, x, y;
     
  /* DNM: make it proper nearest integer for negative numbers as well */
  for (i = 0; i < cont->psize; i++){
    x = floor((double)(cont->pts[i].x + 0.5));
    y = floor((double)(cont->pts[i].y + 0.5));
    cont->pts[i].x = x;
    cont->pts[i].y = y;
  }
}

Icont *imodel_contour_scan(Icont *incont)
{
  Ipoint *et; /* edge table & active edge table. */
  Icont *aet; 
  Icont *cont, *ocont;
  Ipoint pmin, pmax, point;
  Ipoint head, tail;
  Ipoint **ptsaty;
  int *numaty;
  int *yvals;
  int ymin, ymax;
  int i,j, ii;
  int pt, npt, ppt;
  int y;
  int endpt, bgnpt;
  int rising, was_rising;
  int allocsize;
  int chunksize = 80;

  if (incont == NULL){
    return((Icont *)NULL);
  }
  if (incont->psize < 1)
    return(NULL);
  if (incont->pts == NULL){
    b3dError(stderr, "imodel_contour_scan: NULL point array\n");
    return((struct Mod_Contour *)NULL);
  }


  /* DNM: move the duplication and unique calls to before the test for
     one point, mark it as scan type if only one point */

  ocont = imodContourDup(incont);
  if (!ocont)
    return((Icont *)NULL);
  imodel_contour_whole(ocont);
  imodel_contour_unique(ocont);

  if (ocont->psize == 1){
    cont = imodel_contour_create();
    if (cont){
      imodPointAppend(cont, ocont->pts);
      imodPointAppend(cont, ocont->pts);
      cont->pts[1].x += 1.0f;
      cont->flags |= ICONT_SCANLINE;
    }
    imodContourDelete(ocont);
    return(cont);
  }

  /* DNM: eliminate getting rid of lines parallel to y scan */

  imodel_contour_mm(ocont, &pmax, &pmin);
  ymin = pmin.y;
  ymax = pmax.y;

  /* DNM: but if there is just one line parallel to y, then make the scan
     contour from starting and ending points */
  if (ymin == ymax) {
    imodel_contour_sortx(ocont, 0, ocont->psize - 1);
    cont = imodel_contour_create();
    if (cont){
      imodPointAppend(cont, &ocont->pts[0]);
      imodPointAppend(cont, &ocont->pts[ocont->psize - 1]);
      cont->flags |= ICONT_SCANLINE;
    }
    imodContourDelete(ocont);
    return(cont);
  }
      
  /* active edge table */
  aet = imodel_contour_create();
  if (aet == NULL){
    imodContourDelete(ocont);
    return((struct Mod_Contour *)NULL);
  }

  /* contour to be returned */
  /* DNM 3/31/01: allocate a whole chuck for points */
  cont = imodel_contour_create();
  if (cont) {
    cont->pts = (Ipoint *)malloc(chunksize * sizeof(Ipoint));
    allocsize = chunksize;
    if (cont->pts == NULL) {
      imodContourDelete(cont);
      cont = NULL;
    }
  }

  if (cont == NULL){
    imodContourDelete(aet);
    imodContourDelete(ocont);
    return((struct Mod_Contour *)NULL);
  }
  cont->surf = pmin.x - 1;
  cont->flags |= ICONT_SCANLINE;

  /* DNM: here and below, take ymin into account to allow negative y's */

  /* DNM: Make et an array of points; allocate aet point space also, and an
     array to keep track of y values of edges */
  et = (Ipoint *)malloc(ocont->psize * sizeof(Ipoint));
  aet->pts = (Ipoint *)malloc(ocont->psize * sizeof(Ipoint));
  yvals = (int *)malloc(ocont->psize * sizeof(int));
     
  /* DNM: get arrays for number of edges at each y, and pointers to et */
  numaty = (int *)malloc((ymax-ymin+2) * sizeof(int));
  ptsaty = (Ipoint **)malloc((ymax-ymin+2) * sizeof(Ipoint *));
  if (!et || !numaty || !ptsaty || !aet->pts || !yvals){
    if (aet->pts) {
      free(aet->pts);
      aet->pts = NULL;
    }
    if (cont->pts)
      free(cont->pts);
    imodContourDelete(ocont);
    imodContourDelete(cont);
    imodContourDelete(aet);
    if (et)
      free(et);
    if (yvals)
      free(yvals);
    if (ptsaty)
      free(ptsaty);
    if (numaty)
      free(numaty);
    return((struct Mod_Contour *)NULL);
  }
  for (i = 0; i <= ymax - ymin + 1; i++){
    numaty[i] = 0;
  }
     
  /****************************************/
  /* Fill edge table                      */
  /* Convert contour to edge format       */
  /* Array of contour from ymin to ymax.  */

  /* first initialize whether last pair of points were rising */
  for (ppt = ocont->psize - 1; ppt > 0; ppt--) {
    if (ocont->pts[ppt].y != ocont->pts[0].y) {
      was_rising = (ocont->pts[ppt].y < ocont->pts[0].y);
      break;
    }
  }

  for (pt = 0; pt < ocont->psize; pt++){

    /* Get point, next point and previous point */
    npt = pt + 1;
    if (npt > ocont->psize - 1)
      npt = 0;
      
    if (ocont->pts[pt].y == ocont->pts[npt].y) 
      continue; 

    /* get start and end points of edge */
    head.x = ocont->pts[npt].x;
    head.y = ocont->pts[npt].y;
    tail.x = ocont->pts[pt].x;
    tail.y = ocont->pts[pt].y;

    /* point.x is min x                     */
    /* point.y is max y                     */
    /* point.z is 1/m where m is slope.     */
    point.x = ocont->pts[pt].x;
    point.y = ocont->pts[pt].y;
    point.z = (ocont->pts[npt].x - ocont->pts[pt].x) /
      (ocont->pts[npt].y - ocont->pts[pt].y);
      
    /* if not local max or min and there are more than 2 points,
       shorten edge by 1 pixel at the tail */
      
    rising = (ocont->pts[npt].y > ocont->pts[pt].y);
    if (ocont->psize > 2 && 
        ((rising  && was_rising) || (!rising && !was_rising))) {
      if (tail.y > head.y){
        y = head.y;
        point.y = tail.y - 1.0;
        point.x = head.x;
      } else {
        point.y = head.y;
        point.x = tail.x + point.z;
        y = tail.y + 1.0;
      }
    } else {
      if (head.y > tail.y){
        point.y = head.y;
        point.x = tail.x;
        y = tail.y;
      } else {
        point.y = tail.y;
        point.x = head.x;
        y = head.y;
      }
    }
    /* Add edge to temporary edge table; keep track of its Y value and
       the number of edges at that y */
    yvals[aet->psize] = y;
    aet->pts[aet->psize++] = point;
    numaty[y - ymin]++;
    was_rising = rising;
  }

  /* Set up pointers into et array */
  j = 0;
  for(i = 0; i <=  ymax - ymin; i++){
    ptsaty[i] = et + j;
    j += numaty[i];
    numaty[i] = 0;
  }

  /* repack edges into et in order */
  for (j = 0; j < aet->psize; j++){
    i = yvals[j] - ymin;
    ptsaty[i][numaty[i]++] = aet->pts[j];
  }

  aet->psize = 0;

  /************************************************************/
  /* Scan convert contour:                                    */
  /* aet stores the active edge table.                        */
  /* cont will contain start and stop points along the y scan */
  for(i = 0; i <=  ymax - ymin; i++){
    point.y = i + ymin;
      
    /* Fill aet */
    if (numaty[i]){
      for(j = 0; j < numaty[i]; j++)
        aet->pts[aet->psize++] = ptsaty[i][j];
    }
    imodel_contour_sortx(aet, 0, aet->psize - 1);
      
    /* Fill cont */
    for (j = 0; j < aet->psize; j++){
      point.x = aet->pts[j].x;

      /* DNM 3/31/01: eliminate calling imodel_point_add with its
         repeated realloc's; just allocate when the chunk is full 
         Sorry, too lazy to clean up here if the realloc fails! */
      if (cont->psize == allocsize) {
        allocsize += chunksize;
        cont->pts = (Ipoint *)realloc(cont->pts,
                                      allocsize * sizeof(Ipoint));
        if (cont->pts == NULL)
          return((struct Mod_Contour *)NULL);
      }
      cont->pts[cont->psize++] = point;

      if (aet->pts[j].y <= i + ymin){

        /* Remove point from aet if it's done 
           DNM 3/26/01: can't use imodel_point_delete after fixing
           it to free the pts array, so do removal here */
        for ( ii = j; ii < aet->psize - 1; ii++)
          aet->pts[ii] = aet->pts[ii + 1];
        aet->psize--;
        --j;
      } else {
      
        /* Or step it for next y */
        aet->pts[j].x += aet->pts[j].z;
      }
    }
  }

  /* Trim the contour down */
  cont->pts = (Ipoint *)realloc(cont->pts, cont->psize * sizeof(Ipoint));
  if (cont->pts == NULL)
    return((struct Mod_Contour *)NULL);

  /* make sure each scanline has an even pair. */
  for(i = 0; i < cont->psize; i++){
    y = cont->pts[i].y;
    bgnpt = i;
    while (i < cont->psize - 1 && 
           cont->pts[i].y == cont->pts[i+1].y)
      ++i;
    endpt = i;

    if (! ( (endpt-bgnpt)% 2)){
      imodel_point_add(cont, &(cont->pts[i]), i);
      i++;
    }
  }

  /*************/
  /* Clean UP  */
  free(aet->pts);
  aet->pts = NULL;
  free(yvals);
  free(ptsaty);
  free(numaty);
  free(et);
  imodContourDelete(aet);
  imodContourDelete(ocont);
  return(cont);
}


int imodContourNearest(Icont *cont, Ipoint *pnt)
{
  int i, index = 0;
  float dist, tdist, dx, dy, dz;

  if (cont == NULL) return(-1);
  if (cont->psize <= 0) return(-1);

  dx = cont->pts->x - pnt->x;
  dy = cont->pts->y - pnt->y;
  dz = cont->pts->z - pnt->z;

  dist = (dx * dx) + (dy * dy) + (dz * dz);

  for (i = 1; i < cont->psize; i++){
    dx = (cont->pts[i].x - pnt->x);
    dy = (cont->pts[i].y - pnt->y);
    dz = (cont->pts[i].z - pnt->z);

    tdist = (dx * dx) + (dy * dy) + (dz * dz);

    if (tdist < dist){
      dist = tdist;
      index = i;
    }
  }
  return(index);
}

/* returns index of point in contour nearest to (x, y) */
int imodel_contour_nearest(struct Mod_Contour *cont, int x, int y)
{
  int i, index = 0;
  double dist, tdist;

  if (cont == NULL)
    return(-1);
  if (cont->psize <= 0) return(-1);
  dist = ((cont->pts[0].x - x) * (cont->pts[0].x - x))
    + ((cont->pts[0].y - y) * (cont->pts[0].y - y));

  for (i = 1; i < cont->psize; i++){
    tdist = ((cont->pts[i].x - x) * (cont->pts[i].x - x))
      + ((cont->pts[i].y - y) * (cont->pts[i].y - y));
    if (tdist < dist){
      dist = tdist;
      index = i;
    }
  }
  return(index);
}


/* returns TRUE if c1 overlapes c2 in a given section. */
int imodel_contour_overlap(struct Mod_Contour *c1, struct Mod_Contour *c2)
{
  Icont *cs1, *cs2;
  Ipoint pmax1, pmin1, pmax2, pmin2;
  int i, j, jstrt;

  /* first check an see if bounding box overlaps. */
  imodel_contour_mm(c1, &pmax1, &pmin1);
  imodel_contour_mm(c2, &pmax2, &pmin2);

  if (pmax1.x < pmin2.x)
    return(0);
  if (pmax2.x < pmin1.x)
    return(0);
  if (pmax1.y < pmin2.y)
    return(0);
  if (pmax2.y < pmin1.y)
    return(0);

  /* then check to see if scanlines overlap */
  cs1 = imodel_contour_scan(c1);
  cs2 = imodel_contour_scan(c2);
     
  if (!cs1) return(0);
  if (!cs2) return(0);

  jstrt = 0;
  for(i = 0; i < cs1->psize - 1; i+=2)
    for(j = jstrt; j < cs2->psize - 1; j+=2){
      if (cs1->pts[i].y == cs2->pts[j].y){
        if ((cs1->pts[i].x >= cs2->pts[j].x) &&
            (cs1->pts[i].x <= cs2->pts[j+1].x)){
          imodContourDelete(cs1);
          imodContourDelete(cs2);
          return(1);
        }
        if ((cs1->pts[i+1].x >= cs2->pts[j].x) &&
            (cs1->pts[i+1].x <= cs2->pts[j+1].x)){
          imodContourDelete(cs1);
          imodContourDelete(cs2);
          return(1);
        }
        if ((cs2->pts[j].x >= cs1->pts[i].x) &&
            (cs2->pts[j].x <= cs1->pts[i+1].x)){
          imodContourDelete(cs1);
          imodContourDelete(cs2);
          return(1);
        }
        if ((cs2->pts[j].x >= cs1->pts[i].x) &&
            (cs2->pts[j].x <= cs1->pts[i+1].x)){
          imodContourDelete(cs1);
          imodContourDelete(cs2);
          return(1);
        }
      } else if (cs1->pts[i].y > cs2->pts[j].y)
        jstrt = j;
      else
        break;
    }

  /* 3/26/01: memory leak fixed by Lambert Zijp */
  imodContourDelete(cs1);
  imodContourDelete(cs2);
  return(0);
}

/* returns TRUE if cs1 overlaps cs2 in a given section. */
int imodel_scans_overlap(Icont *cs1, Ipoint pmin1, Ipoint pmax1,
                         Icont *cs2, Ipoint pmin2, Ipoint pmax2)
{
  int i, j, jstrt;

  if (!cs1) return(0);
  if (!cs2) return(0);

  /* first check and see if bounding box overlaps. */

  if (pmax1.x < pmin2.x)
    return(0);
  if (pmax2.x < pmin1.x)
    return(0);
  if (pmax1.y < pmin2.y)
    return(0);
  if (pmax2.y < pmin1.y)
    return(0);

  /* then check to see if scanlines overlap */

  jstrt = 0;
  for(i = 0; i < cs1->psize - 1; i+=2)
    for(j = jstrt; j < cs2->psize - 1; j+=2){
      if (cs1->pts[i].y == cs2->pts[j].y){
        if ((cs1->pts[i].x >= cs2->pts[j].x) &&
            (cs1->pts[i].x <= cs2->pts[j+1].x)){
          return(1);
        }
        if ((cs1->pts[i+1].x >= cs2->pts[j].x) &&
            (cs1->pts[i+1].x <= cs2->pts[j+1].x)){
          return(1);
        }
        if ((cs2->pts[j].x >= cs1->pts[i].x) &&
            (cs2->pts[j].x <= cs1->pts[i+1].x)){
          return(1);
        }
        if ((cs2->pts[j].x >= cs1->pts[i].x) &&
            (cs2->pts[j].x <= cs1->pts[i+1].x)){
          return(1);
        }
      } else if (cs1->pts[i].y > cs2->pts[j].y)
        jstrt = j;
      else
        break;
    }
  return(0);
}

/* returns TRUE if cs1 overlaps cs2 in a given section, and returns fractions
   of each contour's area that overlaps. */
int imodel_overlap_fractions(Icont *cs1, Ipoint pmin1, Ipoint pmax1,
                             Icont *cs2, Ipoint pmin2, Ipoint pmax2,
                             float *frac1, float *frac2)
{
  int i, j, jstrt;
  float sum1 = 0.0;
  float sum2 = 0.0;
  float sumover = 0.0;
  int didoverlap = 0;
  float ovstart, ovend;

  *frac1 = *frac2 = 0.0;

  if (!cs1) return(0);
  if (!cs2) return(0);

  /* first check and see if bounding box overlaps. */

  if (pmax1.x < pmin2.x)
    return(0);
  if (pmax2.x < pmin1.x)
    return(0);
  if (pmax1.y < pmin2.y)
    return(0);
  if (pmax2.y < pmin1.y)
    return(0);

  /* Now compute total scan line lengths for each */

  for (i = 0; i < cs1->psize - 1; i+=2)
    sum1 += (cs1->pts[i+1].x - cs1->pts[i].x);

  for (i = 0; i < cs2->psize - 1; i+=2)
    sum2 += (cs2->pts[i+1].x - cs2->pts[i].x);

  /* then check to see if scanlines overlap */

  jstrt = 0;
  for(i = 0; i < cs1->psize - 1; i+=2)
    for(j = jstrt; j < cs2->psize - 1; j+=2){
      if (cs1->pts[i].y == cs2->pts[j].y){

        /* the overlap zone starts at the max of the two starting
           points and ends at the min of the two ending points */

        ovstart = cs1->pts[i].x;
        if (ovstart < cs2->pts[j].x)
          ovstart = cs2->pts[j].x;

        ovend = cs1->pts[i+1].x;
        if (ovend > cs2->pts[j+1].x)
          ovend = cs2->pts[j+1].x;

        if (ovend >= ovstart) {
          sumover += (ovend - ovstart);
          didoverlap = 1;
        }

      } else if (cs1->pts[i].y > cs2->pts[j].y)
        jstrt = j;
      else
        break;
    }
     
  if (sum1 > 0.0)
    *frac1 = sumover/sum1;
  else
    *frac1 = didoverlap;
  if (sum2 > 0.0)
    *frac2 = sumover/sum2;
  else
    *frac2 = didoverlap;

  return(didoverlap);
}

/**********************************************************/
/* returns defined flags for clockwise or counter-clockwise.     */
/* DNM: removed imodel_contour_direction; got rid of old imodContZDirection */
/* turned imodContZAreaDirection into this; made it compute area properly */
/* and do so even if not in one Z plane; and return 0 for ambiguous cases */

int imodContZDirection(Icont *cont)
{
  int pt, mpt, nextp;
  double a = 0.0;

  if (!cont) return(0);
  if (cont->psize < 3)
    return(0);

     
  mpt = cont->psize;
  for(pt = 0; pt < mpt; pt++){
    nextp = pt + 1;
    if (nextp == mpt) nextp = 0;
    a += ((cont->pts[pt].y + cont->pts[nextp].y) *
          (cont->pts[nextp].x - cont->pts[pt].x));
        
  }
  if (a < 0)
    return(IMOD_CONTOUR_COUNTER_CLOCKWISE);
  if (a > 0)
    return(IMOD_CONTOUR_CLOCKWISE);
  return(0);
}



struct Mod_Contour *imodel_contour_double(struct Mod_Contour *cont)
{
  int pt, index;
  struct Mod_Contour *fcont;
  struct Mod_Point point;

  fcont = imodel_contour_create();
  if (fcont == NULL)
    return(NULL);

  for (pt = 0, index = 0; pt < (cont->psize - 1); pt ++){
    imodel_point_add(fcont, &(cont->pts[pt]), index++);
    point.x = (cont->pts[pt].x + cont->pts[pt + 1].x) / 2;
    point.y = (cont->pts[pt].y + cont->pts[pt + 1].y) / 2;
    point.z = (cont->pts[pt].z + cont->pts[pt + 1].z) / 2;
    imodel_point_add(fcont, &point, index++);
  }
  imodel_point_add(fcont, &(cont->pts[cont->psize - 1]), index++);
  point.x = (cont->pts[0].x + cont->pts[cont->psize - 1].x) / 2;
  point.y = (cont->pts[0].y + cont->pts[cont->psize - 1].y) / 2;
  point.z = (cont->pts[0].z + cont->pts[cont->psize - 1].z) / 2;
  imodel_point_add(fcont, &point, index++);
  return(fcont);
}

/****************************************************************************
 * ic is a list of points to be connected. All points must be unique and
 * all points must be within one pixel of each other.
 * returns a contour that has connected points 
 *
 * 3|2|1  icts_offset sets the x,y offset according to this grid.
 * -----
 * 4|P|0
 * -----
 * 5|6|7
 *
 *****************************************************************************/
#ifdef ICONT_NEED_TRACER
int icts_offset(int point, int axis)
{
  while(point < 0) point += 8;
  point %= 8;
  if (axis)
    switch(point){
    case 1:
    case 2:
    case 3:
      return(1);
    case 0:
    case 4:
      return(0);
    default:
      return(-1);
    }
  switch(point){
  case 3:
  case 4:
  case 5:
    return(-1);
  case 2:
  case 6:
    return(0);
  default:
    return(1);
  }
}
#endif

/* remove points that are not needed to describe cont. */
int imodContourStrip(Icont *cont)
{
  int pt, npt, nnpt;
  double slope, nslope;
  int is, nis;

  for(pt = 0; pt < cont->psize - 2; pt++){
    npt = pt + 1;
    nnpt = pt + 2;
    is = nis = FALSE;
    if (cont->pts[npt].x == cont->pts[pt].x)
      is = TRUE;
    if (cont->pts[npt].x == cont->pts[nnpt].x)
      nis = TRUE;
    if ( is && nis ){
      imodel_point_delete(cont, npt);
      pt--;
      continue;
    }
    if (is)  continue;
    if (nis) continue;

    slope = (cont->pts[npt].y - cont->pts[pt].y) /
      (cont->pts[npt].x - cont->pts[pt].x);
    nslope = (cont->pts[nnpt].y - cont->pts[npt].y) /
      (cont->pts[nnpt].x - cont->pts[npt].x);

    if (slope == nslope){
      imodel_point_delete(cont, npt);
      pt--;
    }
  }
  return(0);
}

#ifdef ICONT_NEED_TRACER
/* broken, don't need anyway. */
Icont *imodContourTracer(Icont *ic)
{
  Icont  *cont;
  int cp = 0;
  int fp = 0;
  int first = TRUE;
  int search = 6;
  int spi;

  cont = imodContourNew();

  /* select fp such that its 4-neighbor is not in the set */
  for(spi = 0; spi < ic->psize; spi++){
    if (!imodel_contour_on
        (ic,
         ic->pts[cp].x + icts_offset(4, X),
         ic->pts[cp].y + icts_offset(4, Y)))
      break;
  }
  cp = fp = spi;
  imodPointAppend(cont, &(ic->pts[fp]));
  while ((cp != fp ) || (first)){
    for(spi = 0; spi < 3; spi++, search+=2){
           
      if (imodel_contour_on
          (ic, 
           ic->pts[cp].x + icts_offset(search -1,X),
           ic->pts[cp].y + icts_offset(search -1,Y))){
        imodPointAppend(cont, &(ic->pts[cp]));
        search -= 2;
        break;
      }
      if (imodel_contour_on
          (ic,
           ic->pts[cp].x + icts_offset(search, X),
           ic->pts[cp].y + icts_offset(search, Y))){
        imodPointAppend(cont, &(ic->pts[cp]));
        break;
      }
      if (imodel_contour_on
          (ic,
           ic->pts[cp].x + icts_offset(search + 1, X),
           ic->pts[cp].y + icts_offset(search + 1, Y))){
        imodPointAppend(cont, &(ic->pts[cp]));
        break;
      }
    }

    first = FALSE;
  }

  return(cont);
}
#endif

/* scale all point values in contour by pt */
void imodContourScale(Icont *cont, Ipoint *spoint)
{
  int pt;
  if (!cont)
    return;
  if (!spoint)
    return;
  for(pt = 0; pt < cont->psize; pt++){
    cont->pts[pt].x *= spoint->x;
    cont->pts[pt].y *= spoint->y;
    cont->pts[pt].z *= spoint->z;
  }
  return;
}

void imodContourRotateZ(Icont *cont, double rot)
{
  Imat *mat = imodMatNew(2);
  Ipoint rpt;
  int pt;

  imodMatRot(mat, rot, b3dZ);
  for (pt = 0; pt < cont->psize; pt++){
    imodMatTransform2D(mat, &cont->pts[pt], &rpt);
    cont->pts[pt] = rpt;
  }

  imodMatDelete(mat);
  return;
}

void imodel_contour_swapxy(Icont *cont)
{
  int pt;
  float tmp;
  if (!cont)
    return;

  for(pt = 0; pt < cont->psize; pt++){
    tmp = cont->pts[pt].x;
    cont->pts[pt].x = cont->pts[pt].y;
    cont->pts[pt].y = tmp;
  }
  return;
}

/*
 * make the contour either direction = IMOD_CONTOUR_CLOCKWISE or
 * direction = IMOD_CONTOUR_COUNTER_CLOCKWISE
 */
void imodContourMakeDirection(Icont *cont, int direction)
{
  int pt, hpt;
  Ipoint point;
  float size;

  if (!cont)
    return;
  if (cont->psize < 3)
    return;
  if (direction == imodContZDirection(cont))
    return;
  hpt = cont->psize / 2;

  for(pt = 0; pt < hpt; pt++){
    point = cont->pts[pt];
    cont->pts[pt] = cont->pts[cont->psize - pt - 1];
    cont->pts[cont->psize - pt - 1] = point;
  }

  if (cont->sizes)
    for(pt = 0; pt < hpt; pt++){
      size = cont->sizes[pt];
      cont->sizes[pt] = cont->sizes[cont->psize - pt - 1];
      cont->sizes[cont->psize - pt - 1] = size;
    }

  return;
}

/* Add points so that all points are about 1 to sqrt(2) pixels apart. */
Icont *imodContourFill(Icont *cont)
{
  int pt, npt, i, dist;
  float fdist;
  float xstep, ystep, zstep, dx, dy, dz;
  Ipoint point;
  Icont *fcont = imodContourNew();

  if ((!cont) || (!fcont))
    return NULL;

  if (cont->psize < 2)
    return NULL;

  point.z = cont->pts->z;
  for(pt = 0; pt < cont->psize; pt++){
    npt = pt + 1;
    if (npt == cont->psize){
      if (cont->flags & ICONT_OPEN)
        break;
      npt = 0;
    }
    dx = cont->pts[npt].x - cont->pts[pt].x;
    dy = cont->pts[npt].y - cont->pts[pt].y;
    dz = cont->pts[npt].z - cont->pts[pt].z;

    fdist = (float)sqrt((double)(dx*dx)+(dy*dy)+(dz*dz));
    dist = fdist + 0.5f;

    if (!dist) continue;
    xstep = dx / dist;
    ystep = dy / dist;
    zstep = dz / dist;
    point = cont->pts[pt];

    for(i = 0; i < dist; i++){
      imodPointAppend(fcont, &point);
      point.x += xstep;
      point.y += ystep;
      point.z += zstep;
    }
  }
  return(fcont);
}


/*********************************************************/
/* Join two contours at the st1 and st2, or at the       */
/* points of st1 <0 or st2 < 0                           */
/* fill =  1, fill in the line connecting contours up.   */
/* fill = -1, fill in the line connecting contours down. */
/* fill =  0, dont fill.                                 */
Icont *imodContourJoin(Icont *c1, Icont *c2, int st1, int st2, int fill,
                       int counterdir)
{
  Icont *cont;
  Ipoint point;
  double dist, mdist;
  int pt, pt2;
  int dir1, dir2;

  if (!c1)
    if (c2)
      return(imodContourDup(c2));
  if (!c2)
    return(imodContourDup(c1));

  /* If one of the contours is open, make sure it is the first one so
     the opening is preserved */
  if ((c2->flags & ICONT_OPEN) && !(c1->flags & ICONT_OPEN)) {
    cont = c2;
    c2= c1;
    c1 = cont;
    pt = st1;
    st1 = st2;
    st2 = pt;
  }

  dir1 = imodContZDirection(c1);
  dir2 = imodContZDirection(c2);
  if (st1 >= 0 && st2 >= 0) {
    /* adjust points if they will be inverted */
    if (dir1 != IMOD_CONTOUR_CLOCKWISE)
      st1 = c1->psize - 1 - st1;
    if ((!counterdir && dir2 != IMOD_CONTOUR_CLOCKWISE) ||
        (counterdir && dir2 == IMOD_CONTOUR_CLOCKWISE))
      st2 = c2->psize - 1 - st2;
  }

  /* DNM: changed from COUNTER to CLOCKWISE when inverted sense of flags */
  if (dir1 != IMOD_CONTOUR_CLOCKWISE)
    imodContourMakeDirection(c1, IMOD_CONTOUR_CLOCKWISE);
  if (!counterdir && dir2 != IMOD_CONTOUR_CLOCKWISE)
    imodContourMakeDirection(c2, IMOD_CONTOUR_CLOCKWISE);
  if (counterdir && dir2 == IMOD_CONTOUR_CLOCKWISE)
    imodContourMakeDirection(c2, IMOD_CONTOUR_COUNTER_CLOCKWISE);

  if (st1 < 0 || st2 < 0) {
    /* find closest points */
    mdist = imodel_point_dist(c1->pts, c2->pts);
    st1 = st2 = 0;
    for(pt = 0; pt < c1->psize; pt++){
      for(pt2 = 0; pt2 < c2->psize; pt2++){
        dist = imodel_point_dist(&(c1->pts[pt]), &(c2->pts[pt2]));
        if (dist < mdist){
          st1 = pt;
          st2 = pt2;
          mdist = dist;
        }
      }
    }
  }

  /* set up new contour and fill point. */
  cont = imodContourNew();
  if (!cont)
    return(NULL);
  point.x = (c1->pts[st1].x + c2->pts[st2].x) * 0.5f;
  point.y = (c1->pts[st1].y + c2->pts[st2].y) * 0.5f;
  point.z = (c1->pts[st1].z + c2->pts[st2].z) * 0.5f;
  if (fill == 1)
    point.z += 0.75f;
  if (fill == -1)
    point.z -= 0.75f;

  /* add points to new contour */
  for(pt = 0; pt <= st1; pt++)
    imodPointAppend(cont, &(c1->pts[pt]));
  if (fill)
    imodPointAppend(cont, &point);
  for(pt = st2; pt < c2->psize; pt++)
    imodPointAppend(cont, &(c2->pts[pt]));
  for(pt = 0; pt <= st2; pt++)
    imodPointAppend(cont, &(c2->pts[pt]));
  if (fill)
    imodPointAppend(cont, &point);
  for(pt = st1; pt < c1->psize; pt++)
    imodPointAppend(cont, &(c1->pts[pt]));

  pt2 = 0;
  if (c1->sizes)
    for(pt = 0; pt <= st1; pt++)
      imodPointSetSize(cont, pt2++, c1->sizes[pt]);
  else
    pt2 += st1 + 1;

  if (fill)
    pt2++;

  if (c2->sizes) {
    for(pt = st2; pt < c2->psize; pt++)
      imodPointSetSize(cont, pt2++, c2->sizes[pt]);
    for(pt = 0; pt <= st2; pt++)
      imodPointSetSize(cont, pt2++, c2->sizes[pt]);
  } else
    pt2 += c2->psize + 1;

  if (fill)
    pt2++;

  if (c1->sizes)
    for(pt = st1; pt < c1->psize; pt++)
      imodPointSetSize(cont, pt2++, c1->sizes[pt]);

  /* transfer open flag from c1 */
  if (c1->flags & ICONT_OPEN)
    cont->flags |= ICONT_OPEN;

  return(cont);
}

/* DNM 1/17/01: changed to handle edges of image better, added testmask
   argument so imod/autox can use it 
   DNM 1/25/01: implemented new algorithm to walk around edges and build
   contours in order, instead of just putting edge points into a contour and
   trying to sort them out afterwards */

#define RIGHT_EDGE   16
#define TOP_EDGE     (RIGHT_EDGE << 1)
#define LEFT_EDGE    (RIGHT_EDGE << 2)
#define BOTTOM_EDGE  (RIGHT_EDGE << 3)
#define ANY_EDGE    (RIGHT_EDGE | TOP_EDGE | LEFT_EDGE | BOTTOM_EDGE)
Icont *imodContoursFromImagePoints(unsigned char *data, int xsize, int ysize,
                                   int z, unsigned char testmask, 
                                   int diagonal, int *ncont)
{
  struct Mod_Point point;
  Icont *contarr = NULL;
  Icont *cont;
  int i, j, itst, jtst;
  int side;
  int xs, ys;
  int edgemask[4] = {RIGHT_EDGE, TOP_EDGE, LEFT_EDGE, BOTTOM_EDGE};
  int nextx[4] = {0, -1, 0, 1};
  int nexty[4] = {1, 0, -1, 0};
  int cornerx[4] = {1, -1, -1, 1};
  int cornery[4] = {1, 1, -1, -1};
  float delx[4] = {0.95, 0.5, 0.05, 0.5};
  float dely[4] = {0.5, 0.95, 0.5, 0.05};
  int found, iedge, ixst, iyst, iedgest;

  xs = xsize - 1;
  ys = ysize - 1;
  point.z = z;
  *ncont = 0;

  /* Go through all points including the edges of the image area, and 
     mark all the edges of defined area */

  for (j = 0; j < ysize; j++)
    for(i = 0; i < xsize; i++){
      if (data[i + (j * xsize)] & testmask){

        /* Mark a side if on edge of image, or if next pixel over
           is not in the set */
        side = 0;
        if (i == xs)
          side |= RIGHT_EDGE;
        else if (!(data[(i + 1) + (j * xsize)] & testmask))
          side |= RIGHT_EDGE;
        if (j == ys)
          side |= TOP_EDGE;
        else if (!(data[i + ((j + 1) * xsize)] & testmask))
          side |= TOP_EDGE;
        if (!i)
          side |= LEFT_EDGE;
        else if (!(data[(i - 1) + (j * xsize)] & testmask))
          side |= LEFT_EDGE;
        if (!j)
          side |= BOTTOM_EDGE;
        else if (!(data[i + ((j - 1) * xsize)] & testmask))
          side |= BOTTOM_EDGE;
        data[i +(j * xsize)] |= side;
        /* if (side) printf("i %d  j %d  side %d  data %d\n",
           i, j, side, data[i +(j * xsize)]); */
      }
    }

  found = 1;
  while (found) {
    found = 0;
    for (j = 0; j < ysize && !found; j++)
      for(i = 0; i < xsize; i++){
        if (!(data[i + (j * xsize)] & ANY_EDGE))
          continue;
            
        /* find lowest edge */
        for (iedge = 0; iedge < 4; iedge++)
          if (data[i + j * xsize] & edgemask[iedge])
            break;

        /* Start a new contour */
        if (*ncont) {
          cont = (Icont *) realloc(contarr, (*ncont + 1) *
                                   sizeof(Icont));
        } else {
          cont = (Icont *)malloc(sizeof(Icont));
        }
        if (!cont) {
          b3dError(stderr, "imodContoursFromImagePoints: "
                    "failure to allocate memory for contours");
          return (contarr);
        } else {
          (*ncont)++;
          contarr = cont;
        }
        cont = &contarr[*ncont - 1];
        imodContourDefault(cont);

        /* keep track of starting place and stop when reach 
           it again */
        iedgest = iedge;
        ixst = i;
        iyst = j;
        /* printf ("starting %d %d %d\n", ixst, iyst, iedge); */
        while (!cont->psize || i != ixst || j != iyst || 
               iedge != iedgest) {

          if (!diagonal) {
            /* If no diagonals, look for next edge first 
               around corner on same pixel */
            if (data[i + j * xsize] & 
                edgemask[(iedge + 1) % 4]) {
              iedge = (iedge + 1) % 4;
            } else if (data[i + nextx[iedge] + 
                            (j + nexty[iedge]) * xsize] &
                       edgemask[iedge]) {
              /* same edge, next pixel */
              i += nextx[iedge];
              j += nexty[iedge];
            } else {
              /* pixel on an inside corner - it's got to
                 be, but put in check for testing */
              i += cornerx[iedge];
              j += cornery[iedge];
              iedge = (iedge + 3) % 4;
              if (!(data[i + j * xsize] & edgemask[iedge]))
                printf("no edge around corner at i %d, j %d, edge %d\n", 
                       i, j, iedge);
            }
          } else {
            /* If diagonals, look for next edge first on pixel 
               around inside corner if it's legal */
            itst = i + cornerx[iedge];
            jtst = j + cornery[iedge];
            if (itst >= 0 && itst < xsize && jtst >= 0 && 
                jtst < ysize && (data[itst + jtst * xsize] & 
                                 edgemask[(iedge + 3) % 4])){
              i = itst;
              j = jtst;
              iedge = (iedge + 3) % 4;
            } else if (data[i + nextx[iedge] + 
                            (j + nexty[iedge]) * xsize] &
                       edgemask[iedge]) {
              /* then same edge, next pixel */
              i += nextx[iedge];
              j += nexty[iedge];
            } else {
              /* go around corner on this pixel - the
                 edge has to be there, but put in check
                 for testing */
              iedge = (iedge + 1) % 4;
              if (!(data[i + j * xsize] & 
                    edgemask[iedge]))
                printf("no edge around corner at i %d, j %d, edge %d\n", 
                       i, j, iedge);
            }
          }

          /* add the point and clear the edge */
          point.x = i + delx[iedge];
          point.y = j + dely[iedge];
          imodel_point_add(cont, &point, cont->psize);
          data[i + j * xsize] &= ~edgemask[iedge];
          /* printf ("at %d %d %d, adding %f %f\n", 
             i, j, iedge, point.x, point.y); */

        }
        found = 1;
        break;
      }
  }
  return(contarr);
}
  
int imodContourAutoSort(Icont *cont)
{
  struct Mod_Point point;
  float cx, cy;
  int i, j;
  int sindex;
  double distance;
  double sdist;
  float size;
     
  if (cont == NULL)
    return(-1);
     
  /* Loop through points in contour. */
  /* DNM 2/12/01: change test from i < cont->psize - 1 to avoid unsigned 
     int problems */
  for (i = 0; i + 1 < cont->psize; i++){
    cx = cont->pts[i].x;
    cy = cont->pts[i].y;
    sindex = i + 1;
    sdist = imodel_point_dist( &( cont->pts[i]), &(cont->pts[i + 1]) );
      
    /* Loop through remaining points for best match */
    for(j = i + 2; j < cont->psize; j++){
      distance = imodel_point_dist(&(cont->pts[i]), &(cont->pts[j]) );
      if (sdist > distance){
        sdist = distance;
        sindex = j;
      }
    }

    point = cont->pts[i + 1];
    cont->pts[i + 1] = cont->pts[sindex];
    cont->pts[sindex] = point;
    if (cont->sizes) {
      size = cont->sizes[i + 1];
      cont->sizes[i + 1] = cont->sizes[sindex];
      cont->sizes[sindex] = size;
    }
  }
  return(0);
}

void imodContourSwap(Icont *c1, Icont *c2)
{
  Icont tc;

  tc.pts    = c1->pts;
  c1->pts   = c2->pts;
  c2->pts   = tc.pts;
  tc.psize  = c1->psize;
  c1->psize = c2->psize;
  c2->psize = tc.psize;
  tc.flags  = c1->flags;
  c1->flags = c2->flags;
  c2->flags = tc.flags;
  tc.type   = c1->type;
  c1->type  = c2->type;
  c2->type  = tc.type;
  tc.surf   = c1->surf;
  c1->surf  = c2->surf;
  c2->surf  = tc.surf;
  tc.sizes  = c1->sizes;
  c1->sizes = c2->sizes;
  c2->sizes = tc.sizes;
  return;
}

/* returns index of first point found inside of contour that matches point. */
/* returns -1 if no match. */
int imodContourFindPoint(Icont *cont, Ipoint *point, int flag)
{
  int low, high, index = -1;
  unsigned int pt,size;

  if (!cont)
    return(index);
  if (!point)
    return(index);
  size = cont->psize;

  switch(flag){
      
  case ICONT_FIND_NOSORT:
    for(pt = 0; pt < size; pt++)
      if ((point->x == cont->pts[pt].x) &&
          (point->y == cont->pts[pt].y) &&
          (point->z == cont->pts[pt].z))
        return((int)pt);
    break;

  case ICONT_FIND_SORTX:
    low = 0;
    high = size - 1;
    while(low <= high){

      index = (low+high) / 2;
           
      if (point->x < cont->pts[index].x){
        high = index - 1;
        continue;
      }
      if (point->x > cont->pts[index].x){
        low = index + 1;
        continue;
      }
           
      do{
        index--;
        if (index < 0)break;
      }while(cont->pts[index].x == point->x);
      index++;
      if (index >= cont->psize)
        return(-1);
      while(cont->pts[index].x == point->x){
        if ((cont->pts[index].y == point->y) &&
            (cont->pts[index].z == point->z))
          return(index);
        index++;
        if (index >= cont->psize)
          return(-1);
      }
      break;
    }
    break;

  case ICONT_FIND_SORTY:
    break;

  case ICONT_FIND_SORTXY:
    break;

  default:
    return -1;
  }
  return (index);
}

float  imodContourLength(Icont *cont, int closed)
{
  double dist = 0.0f;
  unsigned int pt;

  if (!cont)
    return(-1);

  if (cont->psize < 2)
    return(dist);
     
  for(pt = 1; pt < cont->psize; pt++)
    dist += imodPointDistance( &(cont->pts[pt]), &(cont->pts[pt - 1]) ); 

  if (closed)
    dist += imodPointDistance( cont->pts, &(cont->pts[cont->psize - 1]));

  return((float)dist);
}

/*
  int imodContourPixelLength(Icont *cont, int pstart, int pend)
  {
  if (!cont) return(0);
  if (cont->psize < 2) return(0);
  if (pstart < 0) return(0);
  if (pend >= cont->psize) return(0);

  }
*/



Ilabel *imodContourGetLabel(Icont *inContour)
{ 
  if (!inContour) return(NULL);
  return(inContour->label); 
}

void imodContourSetLabel(Icont *inContour, Ilabel *inLabel)
{
  if (!inContour) return;
  if (!inLabel) return;
  inContour->label = inLabel;
  return;
}

int     imodContourGetMaxPoint(Icont *inContour)
{ 
  if (!inContour) return(0);
  return(inContour->psize); 
}
Ipoint *imodContourGetPoints(Icont *inContour) 
{ 
  if (!inContour) return(NULL);
  if (!inContour->psize) return(NULL);
  return(inContour->pts); 
}
void imodContourSetPointData(Icont *inContour, Ipoint *inPoint, int inMax)
{
  if (!inContour) return;
  inContour->pts = inPoint;
  inContour->psize = inMax;
}
Ipoint *imodContourGetPoint(Icont *inContour, int inIndex)
{
  if (!inContour) return(NULL);
  if (!inContour->psize) return(NULL);
  if ((inIndex < 0) || (inIndex >= inContour->psize))
    return(NULL);
  return(&inContour->pts[inIndex]);
}

int     imodContourGetTimeIndex(Icont *inContour)
{
  if (!inContour) return(0);
  return(inContour->type);
}
void    imodContourSetTimeIndex(Icont *inContour, int inTime)
{
  if (!inContour) return;
  inContour->type = inTime;
}
int     imodContourGetSurface(Icont *inContour)
{
  if (!inContour) return(0);
  return(inContour->surf);
}
void    imodContourSetSurface(Icont *inContour, int inSurface)
{
  if (!inContour) return;
  inContour->surf = inSurface;
}


char *imodContourGetName(Icont *inContour)
{
  static char name = 0x00;

  if (!inContour) return(&name);
  if (!inContour->label) return(&name);
  if (!inContour->label->name) return(&name);
  return(inContour->label->name);
}
