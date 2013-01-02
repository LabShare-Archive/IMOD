/*
 *  icont.c -- Library of contour handling routines.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <math.h>
#include <string.h>
#include "imodel.h"

/*!
 * Creates one new contour and initializes it to default values.  Returns NULL
 * if error. 
 */
Icont *imodContourNew(void)
{
  Icont *cont;
     
  cont = (Icont *)
    malloc(sizeof(Icont));

  if (cont == NULL)
    return((Icont *)NULL);
  imodContourDefault(cont);
  return(cont);
}

/*!
 * Creates an array of [size] contours and initializes them to default values.
 * Returns NULL if error.
 */
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

/*!
 * Initializes [cont] to default values for an empty contour.
 */
void imodContourDefault(Icont *cont)
{
  if (!cont) return; 
  cont->pts    = NULL;
  cont->psize  = 0;
  cont->flags  = 0;
  cont->time   = 0;
  cont->surf   = 0;
  cont->label  = NULL;
  cont->sizes  = NULL;
  cont->store  = NULL;
  cont->tempVal = 0.;
}

/*!
 * Copies the contour structure in [from] to [to] without copying any data.
 * Returns -1 if error.
 */
int imodContourCopy(Icont *from, Icont *to)
{
  if (!from)
    return(-1);
  if (!to)
    return(-1);
  memcpy(to, from, sizeof(Icont));
  return(0);
}

/*!
 * Creates a new contour containing the same point, size, and label data 
 * as [cont].  Returns NULL if error.
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

/*!
 * Deletes [cont], freeing all data and the contour structure.  Returns -1 if
 * error (i.e., [cont] is NULL).
 */
int imodContourDelete(Icont *cont)
{
  if (cont == NULL)
    return(-1);
  imodContourClear(cont);
  /*     imodLabelDelete(cont->label); DNM: it's already done*/

  free(cont);
  return(0);
}

/*!
 * Frees all data in contour [cont] and sets its pointers to NULL.  Returns
 * -1 if error
 */
int imodContourClear(Icont *cont)
{
  if (!cont)
    return(-1);

  imodContourClearPoints(cont);
  if (cont->sizes)
    free(cont->sizes);
  cont->sizes = NULL;
  cont->flags = 0;
  cont->time = 0;
  imodLabelDelete(cont->label);
  /* DNM: set label to NULL */
  cont->label = NULL;
  ilistDelete(cont->store);
  cont->store = NULL;
  return(0);
}

/*!
 * Frees just the point data in contour [cont] and sets the pointer for point 
 * data to NULL and the point count to 0.  Returns -1 if error.
 */
int imodContourClearPoints(Icont *cont)
{
  if (!cont)
    return(-1);

  if ((cont->pts) && (cont->psize))
    free (cont->pts);
  cont->pts = NULL;
  cont->psize = 0;
  return 0;
}

/*!
 * Deletes an array of [size] contours in [cont], freeing all data and the 
 * array itself.  Returns -1 if error (i.e., [cont] is NULL).
 */
int imodContoursDelete(Icont *cont, int size)
{
  int co;

  if (cont == NULL)
    return(-1);

  for(co = 0; co < size; co++)
    imodContourClear(&(cont[co]));
  free(cont);
  return(0);
}

/*!
 * Deletes contours from object [obj], retaining [keep].  The contour data
 * are freed and the contour array is reallocated to the new size.  Returns
 * -1 if error.
 */
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
    imodContourClear(&(obj->cont[co]));

  obj->contsize = keep;
  /* Change contour array to new size */
  if (obj->contsize){
    obj->cont = (Icont *)
      realloc(obj->cont, obj->contsize * sizeof(Icont));
    if (!obj->cont)
      return(-1);
     
  }else{
    free(obj->cont);
    obj->cont = NULL;
  }
  return(0);
}

/*!
 * Assigns a new surface number to contour [cont] and adjusts the maximum
 * surface number for [obj], the object that it is in.  Returns -1 if error.
 */
int imodel_contour_newsurf(Iobj *obj, Icont *cont)
{
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

/*!
 * Finds the first unused surface number in object [obj], and returns this 
 * number or 0 for an error.
 */
int imodel_unused_surface(Iobj *obj)
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
     
/*!
 * Sets the wild flag in contour [cont] (ICONT_FLAG in cont->flags) if Z is not
 * the same for all points when rounded to the nearest integer.
 */
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

/****************************************************************************/
/* INFORMATION FUNCTIONS
 * DOC_SECTION INFORMATION
 */ 

/*!
 * Returns area of contour [cont] in square pixels.  The contour need not be
 * COplanar.  The area is computed as half the magnitude of the sum of 
 * cross-products of adjacent line segments.  
 */
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

/*!
 * Returns area of contour [cont] in the X/Y plane in square pixels, or 0 for
 * an error.  Area is measured by converting to a scan contour and summing the
 * length of the scan lines.  Used (only) by @@imodContourCircularity@.
 */
int imodel_contour_area(Icont *icont)
{
  Icont *cont;
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

/*!
 * returns length of contour [cont] in pixels, including the distance from
 * ending to starting point if [closed] is non-zero.  Returns -1 for error.
 */
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

/*!
 * Returns length of contour [cont], excluding a connection between last and
 * first points, or -1 if error.
 */
double imodel_contour_length(Icont *cont)
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

/*!
 * Returns a moment of the contour [cont] relative to the origin, where the 
 * order of the moment is [a] in X and [b] in Y.  It computes the sum
 * x**a * y**b over all pixels within the contour.  [cont] can be scan contour;
 * if it is not, a temporary scan contour is generated.  Returns 0 for error.
 * Used (only) by @@imodContourCenterOfMass@.
 */
double imodContourMoment(Icont *cont, int a, int b)
{
  Icont *scont;
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

/*!
 * Computes the center of mass of contour [cont] and returns the result in 
 * [rpt].  The contour is assumed to lie in the X/Y plane.  Returns 0 for
 * success, -1 for an error, or 0 if the [cont] is NULL
 * or has no points, in which case [rpt] is 0,0,0.
 */
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

/*!
 * Computes components of the centroid of contour [icont].  If ICONT_TEMPUSE is
 * not set in the {flags} element of [icont], it treats the contour as coming
 * from a closed contour object and assumes it is confined to the X/Y plane,
 * and returns the sum of pixel locations inside the contour in [rcp] and the 
 * pixel sum, or area, in [rtw].  If ICONT_TEMPUSE is set, it treats it as 
 * coming from an open contour object and returns the sum of pixel locations
 * along the contour in [rcp] and the contour length in [rtw].  In either case
 * the centroid is rcp->x/rtw, rcp->y/rtw, rcp->z/rtw.  Returns -1 for error.
 */
int imodel_contour_centroid(Icont *icont, Ipoint *rcp,
                            double *rtw)
{
  Icont *cont;
  float xval, yval, weight;
  int pix = 0;
  int i, j, y;
  int xmin, xmax;
  int bgnpt,endpt;
  int scanline;
  Ipoint scale = {1., 1., 1.};

  if (!icont)
    return(-1);

  if (!icont->psize)
    return(-1);

  if (icont->psize == 1){
    rcp->x = icont->pts->x;
    rcp->y = icont->pts->y;
    rcp->z = icont->pts->z;
    *rtw = 1;
    return(0);
  }

  rcp->x = 0.0f;
  rcp->y = 0.0f;
  *rtw = 0;

  /* Handle contour in open contour object */
  if (icont->flags & ICONT_TEMPUSE) {
    rcp->z = 0;
    for (i = 1; i < icont->psize; i++) {
      weight = imodPoint3DScaleDistance(&icont->pts[i-1], &icont->pts[i],
                                        &scale);
      rcp->x += weight * (icont->pts[i-1].x + icont->pts[i].x) * 0.5f;
      rcp->y += weight * (icont->pts[i-1].y + icont->pts[i].y) * 0.5f;
      rcp->z += weight * (icont->pts[i-1].z + icont->pts[i].z) * 0.5f;
      *rtw += weight;
    }
    return 0;
  }

  /* Use scan contour for closed contour object */
  rcp->z = icont->pts[0].z;
  cont = imodel_contour_scan(icont);
  if (!cont)
    return (-1);
     
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

/*!
 * Returns the moment of contour [cont] of order [a] in X and order [b] in Y,
 * relative to the point [org].  Computes (x-org->x)**a * (y-yorg->y)**b over
 * all pixels inside the contour.  Returns 0 for error.  Unused.
 */
double imodContourCenterMoment(Icont *cont, Ipoint *org, int a, int b)
{
  Icont *scont = imodel_contour_scan(cont);
  int i, j, pt;
  int bline, eline;
  double moment = 0.0;
     
  if (!scont) 
    return 0.0;
     
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


/*!
 * Returns the circularity of a closed contour, based on perimeter squared to
 * area.  A perfect circle = 1.0, a square = 1.27.  Returns 1000. for error or
 * zero area contour. */
/* A = pi R^2
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

/* used by ill-fated Principal Axis */
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

/*!
 * Measures the angle at which the contour is most elongated, to the precision
 * set by [precision], in degrees.  It simply rotates the contour, looking for
 * the bounding box with the greatest ratio of height to width.
 * Returns the angle to the long axis in radians, or 0 for an error.
 * Returns the ratio of long to short axis in [aspect], and the length of the
 * long axis in [longaxis].
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
       this angle is a good as is; otherwise it's off by 90 degrees */
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
  double dist;
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
  fcont->time = 0;
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

/*!
 * Fits a plane to the contour [cont], where points are scaled by [scale].
 * For a plane equation Ax + By + Cz + D = 0, the normal (A,B,C) is returned
 * in [norm], and D is returned in [dval].  The rotations (in radians) around 
 * the Y axis then the X axis that will make the plane be flat are returned 
 * in [beta] and [alpha].  Returns 1 if there are too few points in the
 * contour or if the points are too close to colinear.
 */
int imodContourFitPlane(Icont *cont, Ipoint *scale, Ipoint *norm, float *dval,
                        double *alpha, double *beta)
{
  double sx, sy, sz, sxsq, sysq, szsq, sxy, sxz, syz, xm, ym, zm;
  double cosfac, sinfac, sinb, cosa, cosb, xnorm, ynorm, znorm, zrot;
  double a[3][3];
  double small = 1.e-4;
  int i, n, nrot;
  Ipoint *pts = cont->pts;
  Ipoint pmin, pmax;
  float x, y, z;

  n = cont->psize;
  if (n < 3)
    return 1;

  /* Find axis of smallest extent and setup to rotate data so that axis 
     becomes Z.  If that axis is constant, this moves the all-zero terms
     to the third equation */
  imodContourGetBBox(cont, &pmin, &pmax);
  nrot = 0;
  if (pmax.y - pmin.y < pmax.x - pmin.x && pmax.y - pmin.y < pmax.z - pmin.z)
    nrot = 1;
  if (pmax.x - pmin.x < pmax.y - pmin.y && pmax.x - pmin.x < pmax.z - pmin.z)
    nrot = 2;

  /* Form sums of all kinds */
  sx = sy = sz = sxsq = sysq = szsq = sxy = sxz = syz = 0.;
  for (i = 0; i < n; i++) {
    if (nrot == 0) {
      x = pts[i].x * scale->x;
      y = pts[i].y * scale->y;
      z = pts[i].z * scale->z;
    } else if (nrot == 1) {
      y = pts[i].x * scale->x;
      z = pts[i].y * scale->y;
      x = pts[i].z * scale->z;
    } else {
      z = pts[i].x * scale->x;
      x = pts[i].y * scale->y;
      y = pts[i].z * scale->z;
    }
    sx += x;
    sy += y;
    sz += z;
    sxsq += x * x;
    sysq += y * y;
    szsq += z * z;
    sxy += x * y;
    sxz += x * z;
    syz += y * z;
  }

  /* Compute the terms of the 3 equations in the normal components, which are:
     -ai0*cos(a)*sin(b) + a11*sin(a) + ai2*cos(a)*cos(b) = 0   for i = 0,1,2
   */
  xm = sx / n;
  ym = sy / n;
  zm = sz / n;
  a[0][0] = sxsq - xm * sx;
  a[0][1] = sxy - ym * sx;
  a[0][2] = sxz - zm * sx;
  a[1][0] = sxy - xm * sy;
  a[1][1] = sysq - ym * sy;
  a[1][2] = syz - zm * sy;
  a[2][0] = sxz - xm * sz;
  a[2][1] = syz - ym * sz;
  a[2][2] = szsq - zm * sz;

  /* Combine the first and second equation, or first and third if that fails */
  for (i = 1; i < 3; i++) {
    sinfac = a[0][1] * a[i][0] - a[i][1] * a[0][0];
    cosfac =  a[0][1] * a[i][2] - a[i][1] * a[0][2];
    
    /* If both factors are too small, the equations are redundant due to 
       colinear points on the two dimensions, so try other pair or quit */
    if (fabs(sinfac) < small && fabs(cosfac) < small) {
      if (i == 1)
        continue;
      return 1;
      }
    
    /* get angle between +/- 90 */
    if (sinfac < 0.) {
      sinfac = -sinfac;
      cosfac = -cosfac;
    }
    *beta = atan2(cosfac, sinfac);
    break;
  }

  sinb = sin(*beta);
  cosb = cos(*beta);
  for (i = 0; i < 3; i++) {
    sinfac = a[i][1];
    cosfac = a[i][0] * sinb - a[i][2] * cosb;

    /* If both factors are too small, try the next equation. */
    if (fabs(sinfac) < small && fabs(cosfac) < small) {
      if (i < 2)
        continue;
      return 1;
    }

    /* again get angle between +/- 90 */
    if (sinfac < 0.) {
      sinfac = -sinfac;
      cosfac = -cosfac;
    }
    *alpha = atan2(cosfac, sinfac);
    break;
  }

  cosa = cos(*alpha);

  /* Rotate the normal back to native coordinates and make Z positive */
  if (nrot == 0) {
    xnorm = -cosa * sinb;
    ynorm = sin(*alpha);
    znorm = cosa * cosb;
  } else if (nrot == 1) {
    znorm = -cosa * sinb;
    xnorm = sin(*alpha);
    ynorm = cosa * cosb;
  } else {
    ynorm = -cosa * sinb;
    znorm = sin(*alpha);
    xnorm = cosa * cosb;
  }
  if (znorm < 0.) {
    xnorm = -xnorm;
    ynorm = -ynorm;
    znorm = -znorm;
  }

  norm->x = (float)xnorm;
  norm->y = (float)ynorm;
  norm->z = (float)znorm;
  *dval = -(xm * norm->x + ym * norm->y + zm * norm->z);

  /* Find the angles again */
  *beta = 0.;
  *alpha = 0.;
  if (fabs(xnorm) > small || fabs(znorm) > small)
    *beta = -atan2(xnorm, znorm);
  zrot = znorm * cos(*beta) - xnorm * sin(*beta);
  if (fabs(zrot) > small || fabs(ynorm) > small)
    *alpha = -(atan2(zrot, ynorm) - 1.570796327);

  return 0;
}

/*!
 * Calculates the full 3D bounding box of contour [cont] and returns the 
 * lower left, bottom coordinates in [ll] and the upper right, top coordinates
 * in [ur].  Returns -1 if error.
 */
int imodContourGetBBox(Icont *cont, Ipoint *ll, Ipoint *ur)
{
  int pt;

  if (!cont)
    return(-1);
  if (!cont->psize)
    return(-1);

  ll->x = ur->x = cont->pts->x;
  ll->y = ur->y = cont->pts->y;
  ll->z = ur->z = cont->pts->z;

  for(pt = 0; pt < cont->psize; pt++){
    if (cont->pts[pt].x < ll->x)
      ll->x = cont->pts[pt].x;
    if (cont->pts[pt].y < ll->y)
      ll->y = cont->pts[pt].y;
    if (cont->pts[pt].z < ll->z)
      ll->z = cont->pts[pt].z;
    if (cont->pts[pt].x > ur->x)
      ur->x = cont->pts[pt].x;
    if (cont->pts[pt].y > ur->y)
      ur->y = cont->pts[pt].y;
    if (cont->pts[pt].z > ur->z)
      ur->z = cont->pts[pt].z;
  }
  return(0);
}

/* 3/19/05: removed imodel_contour_mm, same actions as above */

/*!
 * Computes the mean Z value of a contour and rounds to nearest integer,
 * or returns -1 if no contour or no points 
 */
int imodContourZValue(Icont *cont)
{
  int p;
  float z=0.0f;
  if ((!cont) || (!cont->psize))
    return(-1);
  for(p = 0; p < cont->psize; p++)
    z+=cont->pts[p].z;

  p = (int)floor(z / cont->psize + 0.5);
  return(p);
}

/*!
 * Determines whether contour [cont] is clockwise or counter-clockwise and 
 * returns IMOD_CONTOUR_CLOCKWISE or IMOD_CONTOUR_COUNTER_CLOCKWISE, or
 * 0 for an error an a contour with no area.
 */
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

/*!
 * Returns non-zero if ([x], [y]) is in point list of contour [cont], or
 * 0 if it is not.  (unused 4/22/05)
 */
int imodel_contour_on(Icont *cont, int x, int y)
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

/*!
 * Returns the index of the point in contour [cont] that is nearest in 3D to
 * point [pnt], or -1 for an error.
 */
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

/*!
 * Returns the index of the point in contour [cont] that is nearest in X and Y
 * to ([x], [y]), or -1 for an error.
 */
/* Unused 3/29/05 */
int imodel_contour_nearest(Icont *cont, int x, int y)
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

/****************************************************************************/
/* CONTOUR-MODIFYING FUNCTIONS
 * DOC_SECTION MODIFYING
 */ 

/*!
 * Joins two contours [c1] and [c2] by adding a connecting line between them 
 * and arranging points from the two contours into a single path. ^
 *   If [st1] and [st2] are >=0 the contours will be joined at points [st1] 
 * and [st2]; if either one is negative they will be joined at their nearest 
 * points. ^
 *   If [fill] is  1 or -1, a point will be placed in the middle of the 
 * connector at 0.75 pixels up or down in Z, respectively.  The fill point will
 * be given the general storage properties of the preceding point in the 
 * contour. ^
 *   If [counterdir] is nonzero the two contours will be made to go in opposite
 * directions before being joined.
 */
Icont *imodContourJoin(Icont *c1, Icont *c2, int st1, int st2, int fill,
                       int counterdir)
{
  Icont *cont;
  Ipoint point;
  Ilist *nstore = NULL;
  double dist, mdist;
  int pt, pt2;
  int dir1, dir2;
  int lst2, lst3, lst4;
  Istore item;

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

  /* If second contour is open, insert a gap at its end */
  if ((c2->flags & ICONT_OPEN) && !istorePointIsGap(c2->store, c2->psize - 1)){
      item.type = GEN_STORE_GAP;
      item.flags = GEN_STORE_ONEPOINT;
      item.index.i = c2->psize - 1;
      istoreAddOneIndexItem(&c2->store, &item);
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

  /* Take care of joining storage lists first, but keep new list separate
     to avoid having it get renumbered.  Propagate the properties at st1 and
     st2 into the fill point */
  if (ilistSize(c1->store) || ilistSize(c2->store)) {
    lst2 = st1 + 1 + (fill ? 1 : 0);
    lst3 = lst2 + c2->psize - st2;
    lst4 = st1 + c2->psize + 2 + (fill ? 2 : 0);
    if (istoreExtractChanges(c1->store, &nstore, 0, st1, 0, c1->psize) ||
        (fill && istoreExtractChanges(c1->store, &nstore, st1, st1, st1 + 1,
                                      c1->psize)) ||
        istoreExtractChanges(c2->store, &nstore, st2, c2->psize - 1, 
                             lst2, c2->psize) ||
        istoreExtractChanges(c2->store, &nstore, 0, st2, 
                             lst3, c2->psize) ||
        (fill && istoreExtractChanges(c2->store, &nstore, st2, st2,
                                      lst4 - 1, c2->psize)) ||
        istoreExtractChanges(c1->store, &nstore, st1, c1->psize - 1, 
                             lst4, c1->psize) ||
        istoreCopyNonIndex(c1->store, &nstore) ||
        istoreCopyNonIndex(c2->store, &nstore)) {
      ilistDelete(nstore);
      imodContourDelete(cont);
      return(NULL);
    }
  }

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

  istoreCleanEnds(nstore);
  cont->store = nstore;
  return(cont);
}

/*!
 * Returns a contour with points 0 to [p1] from contour [c1] and
 * points from [p2] to the end from contour [c2].
 */
Icont *imodContourSplice(Icont *c1, Icont *c2, int p1, int p2)
{
  Icont *nc;
  Ilist *nstore = NULL;
  int i;

  if ((!c1) || (!c2))
    return(NULL);

  if ((p1 < 0) || (p2 < 0) || (p1 >= c1->psize) || (p2 >= c2->psize))
    return(NULL);

  nc = imodContourNew();
  if (!nc) 
    return(NULL);

  /* Take care of any store items first */
  /* Extract changes from each and copy any non-index items */
  if (ilistSize(c1->store) || ilistSize(c2->store)) {
    if (istoreExtractChanges(c1->store, &nstore, 0, p1, 0, c1->psize) ||
        istoreExtractChanges(c2->store, &nstore, p2, c2->psize - 1, p1 + 1, 
                             c2->psize) ||
        istoreCopyNonIndex(c1->store, &nstore) ||
        istoreCopyNonIndex(c2->store, &nstore)) {
      ilistDelete(nstore);
      imodContourDelete(nc);
      return(NULL);
    }
  }

  for (i = 0; i <= p1; i++)
    imodPointAppend(nc, &(c1->pts[i]));
  for (i = p2; i < c2->psize; i++)
    imodPointAppend(nc, &(c2->pts[i]));
  if (c1->sizes)
    for(i = 0; i <= p1; i++)
      imodPointSetSize(nc, i, c1->sizes[i]);
  if (c2->sizes)
    for (i = p2; i < c2->psize; i++)
      imodPointSetSize(nc, p1 + 1 + i - p2, c2->sizes[i]);

  istoreCleanEnds(nstore);
  nc->store = nstore;
  return(nc);
}

/*!
 * Adds together two scan contours [c1] and [c2], returning all of their segments in order
 * and combining segments where they overlap.  Returns NULL for error.
 */
Icont *imodContourScanAdd(Icont *c1, Icont *c2)
{
  Icont *cont;
  int y1, y2, pt1, pt2, psize = 0;
  if (!c1 || !c2 || !(c1->flags & ICONT_SCANLINE) || !(c2->flags & ICONT_SCANLINE) ||
      !c1->psize || !c2->psize)
    return NULL;
  cont = imodContourNew();
  if (!cont)
    return NULL;
  cont->pts = (Ipoint *)malloc((c1->psize + c2->psize) * sizeof(Ipoint));
  if (!cont->pts)
    return NULL;
  pt1 = pt2 = 0;
  while (pt1 < c1->psize - 1 || pt2 < c2->psize - 1) {
    y1 = y2 = INT_MAX;
    if (pt1 < c1->psize - 1)
      y1 = (int)c1->pts[pt1].y;
    if (pt2 < c2->psize - 1)
      y2 = (int)c2->pts[pt2].y;
    /* Compare the current pair of segments to see which one to add next */
    if (y1 < y2  || (y1 == y2 && c1->pts[pt1+1].x < c2->pts[pt2].x)) {
      cont->pts[psize++] = c1->pts[pt1++];
      cont->pts[psize++] = c1->pts[pt1++];
    } else if (y2 < y1  || (y1 == y2 && c2->pts[pt2+1].x < c1->pts[pt1].x)) {
      cont->pts[psize++] = c2->pts[pt2++];
      cont->pts[psize++] = c2->pts[pt2++];
    } else {

      /* Combine the two segments */
      cont->pts[psize].x = B3DMIN(c1->pts[pt1].x, c2->pts[pt2].x);
      cont->pts[psize++].y = y1;
      cont->pts[psize].x = B3DMAX(c1->pts[pt1+1].x, c2->pts[pt2+1].x);
      cont->pts[psize++].y = y1;
      pt1 += 2;
      pt2 += 2;
    }
  }
  cont->psize = psize;
  cont->flags |= ICONT_SCANLINE;
  return cont;
}

/*!
 * Returns a contour containing points from [p1] to [p2], inclusive, from 
 * contour [cont], and removes those points from [cont].  If [p2] is < 0 all 
 * points from [p1] to the end are transferred to the new contour.  Returns 
 * NULL if error.
 */
Icont *imodContourBreak(Icont *cont, int p1, int p2)
{
  Icont *nc;
  int i, ni;

  /* check for bogus input data. */
  if (p2 < 0)
    p2 = cont->psize - 1;

  if (!cont || !cont->psize || p1 < 0 || p1 >= cont->psize ||
      p2 >= cont->psize || p2 < p1)
    return(NULL);

  /* Get contour and points, copy properties */
  nc = imodContourNew();
  if (!nc) 
    return(NULL);
  nc->flags = cont->flags;
  nc->time = cont->time;
  nc->surf = cont->surf;
  nc->pts = (Ipoint *)malloc((p2 + 1 - p1) * sizeof(Ipoint));
  if (!nc->pts) {
    imodContourDelete(nc);
    return(NULL);
  }
  nc->psize = p2 + 1 - p1;

  /* Copy points then sizes */
  for (i = p1; i <= p2; i++)
    nc->pts[i - p1] = cont->pts[i];

  if (cont->sizes) {
    nc->sizes = (float *)malloc(nc->psize * sizeof(float));
    if (!nc->sizes) {
      imodContourDelete(nc);
      return(NULL);
    }
    for (i = p1; i <= p2; i++)
      nc->sizes[i - p1] = cont->sizes[i];
  }

  if (istoreBreakContour(cont, nc, p1, p2)) {
    imodContourDelete(nc);
    return(NULL);
  }

  /* Copy down remaining points and reset size of old contour */
  ni = p1;
  for (i = p2 + 1; i < cont->psize; i++, ni++) {
    cont->pts[ni] = cont->pts[i];
    if (cont->sizes)
      cont->sizes[ni] = cont->sizes[i];
  }
  cont->psize = ni;

  if (!ni) {
    free(cont->pts);
    if (cont->sizes) {
      free(cont->sizes);
      cont->sizes = NULL;
    }
  }

  /* Do not resize arrays since we are now committed to error-free return */

  return(nc);
}

/*!
 * Sorts points in contour [cont] from index [bgnpt] through index [endpt]
 * by their X coordinates; preserves sizes but not label indexes.
 * Returns -1 for error.  Used (only) by @@imodel_contour_scan@.
 */
int imodel_contour_sortx(Icont *cont, int bgnpt, int endpt)
{
  int i, j;
  int sindex;
  Ipoint point;
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

/*!
 * Sorts points in contour [cont] from index [bgnpt] through index [endpt]
 * by their Y coordinates; preserves sizes but not label indexes.
 * Returns -1 for error.  (Unused 4/22/05)
 */
int imodel_contour_sorty(Icont *cont, int bgnpt, int endpt)
{
  int i, j;
  int sindex;
  Ipoint point;
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

/*!
 * Sorts points in contour [cont] from index [bgnpt] through index [endpt]
 * by their Z coordinates; preserves sizes but not label indexes.
 * Returns -1 for error.  Used in 3dmod.
 */
int imodel_contour_sortz(Icont *cont, int bgnpt, int endpt)
{
  int i, j;
  int sindex;
  Ipoint point;
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

/*!
 * Sorts points in contour [cont] by proximity, using the scale factors in 
 * [scale] to evaluate distances in 3D.  Starting from the first point,
 * it makes the closest point be the next point, then moves on to the next 
 * point and makes the closest among the remaining points be the next point.
 * It then runs a second pass in the opposite direction.  It preserves sizes
 * but not label indexes.  Returns -1 for error.  Used by 3dmod.
 */
int imodContourSort3D(Icont *cont, Ipoint *scale)
{
  Ipoint point;
  int i, j, pass;
  int sindex;
  double distance;
  double sdist;
  float size;

  if (cont == NULL)
    return(-1);

  imodContourUnique(cont);

  for (pass = 0; pass < 2; pass++) {
    for (i = 0; i < cont->psize - 1; i++){

      /* For each point, find the closest point farther along in the contour */
      sindex = i + 1;
      sdist = imodPoint3DScaleDistance(&( cont->pts[i]), &(cont->pts[i + 1]),
                                       scale);
      for (j = i + 2; j < cont->psize; j++) {
        distance = imodPoint3DScaleDistance(&(cont->pts[i]), &(cont->pts[j]), 
                                            scale );
        if (sdist > distance) {
          sdist = distance;
          sindex = j;
        }
      }
      
      /* Swap that point in for the next point */
      point = cont->pts[i + 1];
      cont->pts[i + 1] = cont->pts[sindex];
      cont->pts[sindex] = point;
      if (cont->sizes) {
        size = cont->sizes[i + 1];
        cont->sizes[i + 1] = cont->sizes[sindex];
        cont->sizes[sindex] = size;
      }
    }

    /* Invert the contour after each pass */
    imodel_contour_invert(cont);
  }
  return(0);
}

/*!
 * Sorts points in contour [cont] by proximity in the X/Y plane, by calling
 * @imodContourSort3D with a scale of 1,1,0.  Returns -1 for error.  Maintained
 * for consistency with previous behavior of this sort function.
 */
int imodel_contour_sort(Icont *cont)
{
  Ipoint scale = {1.,1.,0.};
  return imodContourSort3D(cont, &scale);
}

/*!
 * Inverts the order of points in contour [cont]; preserves points sizes and
 * labels.  Returns -1 for error.
 */
int imodel_contour_invert(Icont *cont)
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

  /* Invert the storage items */
  istoreInvert(&cont->store, cont->psize);
  return(0);
}

/*!
 * Reduces the points in contour [cont] by selecting a minimal subset of the
 * points.  Each of the original points will be within a tolerance [tol] of 
 * the segments defined by the remaining points.
 */
void imodContourReduce(Icont *cont, float tol)
{
  int npts, noutlim, left, irt, nout, ifout, npo, ipo, its;
  int *minseg;
  int *nextpt;
  float tolsq, denom, tmin, distsq, x1, y1, x2, y2, dx, dy, dx0, dy0;

  /*      moving from right to left, look at possible segments from a given
          point going to right.  A segment is possible if all intervening
          points are within TOL of the segment.  From the given point, find
          the possible segment that involves the fewest segments to get to
          the right end.  Keep track of the endpoint of the best segment in
          NEXTPT and the total number of segments in MINSEG  */

  npts = cont->psize;
  minseg = (int *)malloc(npts * sizeof(int) + 1);
  nextpt = (int *)malloc(npts * sizeof(int) + 1);
  if (!minseg || !nextpt) {
    if (minseg)
      free(minseg);
    if (nextpt)
      free(nextpt);
    return;
  }
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
          if (istoreRetainPoint(cont->store, its))
            ifout = 1;
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
  if (minseg[0] + 1 != npts) {

    /* npo is number of points already retained and index of points to delete
       ipo is the index of a retained point in original contour */
    npo = 1;
    ipo = 0;
    while (nextpt[ipo] < npts) {
      
      /* Delete any points between current and next point */
      for (its = ipo + 1; its < nextpt[ipo]; its++)
        imodPointDelete(cont, npo);
      ipo = nextpt[ipo];
      npo++;
    }
    /* if (minseg[0] + 1 != npo)
       printf("OOPS minseg + 1 = %d, npo = %d\n",minseg[0] + 1, npo); */
    /*     printf ("%d to %d\n", npts, npo); */
  }
  free(minseg);
  free(nextpt);
}

/*!
 * Removes points from contour [cont] whose distance from both
 * the previous and the next point is less than [dist].  Returns -1 for error.
 * See @imodContourReduce, which will preserve structure better.
 */
int imodContourShave(Icont *cont, double dist)
{
  int i;
  double pdist, ndist;

  if (cont->psize < 3)
    return(-1);

  for (i = 1; i < cont->psize - 1; i++){
    pdist = imodel_point_dist( &(cont->pts[i - 1]), &(cont->pts[i]));
    ndist = imodel_point_dist( &(cont->pts[i + 1]), &(cont->pts[i]));
    if ((pdist < dist) && (ndist < dist) && 
        !istoreRetainPoint(cont->store, i)) {
      imodPointDelete(cont, i);
      i--;
    }

    /* DNM 7/4/05: removed code for negative dist that deleted point if it was
       farther than -dist from both points! */
  }
  return(0);
}

/*!
 * Removes adjacent duplicate points from contour [cont]. 
 * Returns non-zero for error.
 */
int imodContourUnique(Icont *cont)
{
  int i,j;
  Ipoint *pnt;

  if (cont == NULL)
    return(-1);

  i = 0;
  while (i < cont->psize && cont->psize > 1){
    j = (i + 1) % cont->psize;
    pnt = &(cont->pts[i]);
    if ((pnt->x == cont->pts[j].x) &&
        (pnt->y == cont->pts[j].y) &&
        (pnt->z == cont->pts[j].z))
      imodPointDelete(cont, j);
    else
      i++;
  }

  return(0);
}

/*!
 * Removes points that are not needed in contour [cont]; specifically, if three
 * sequential points are colinear it removes the middle one.  Returns 0.
 */
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
      imodPointDelete(cont, npt);
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
      imodPointDelete(cont, npt);
      pt--;
    }
  }
  return(0);
}

/*!
 * Rounds off X/Y point coordinates of contour [cont] to the nearest integer.
 */
void imodel_contour_whole(Icont *cont)
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

/*!
 * Sets the Z values of all points in the contour to the mean Z value rounded
 * to the nearest integer
 */
void imodContourFlatten(Icont *cont)
{
  int i, cz;
  cz = imodContourZValue(cont);
  for (i = 0; i < cont->psize; i++){
    cont->pts[i].z = cz;
  }
}

/* Doubles number of points in contour by interpolation.  Unused 4/22/05 */
Icont *imodel_contour_double(Icont *cont)
{
  int pt, index;
  Icont *fcont;
  Ipoint point;

  fcont = imodContourNew();
  if (fcont == NULL)
    return(NULL);

  for (pt = 0, index = 0; pt < (cont->psize - 1); pt ++){
    imodPointAdd(fcont, &(cont->pts[pt]), index++);
    point.x = (cont->pts[pt].x + cont->pts[pt + 1].x) / 2;
    point.y = (cont->pts[pt].y + cont->pts[pt + 1].y) / 2;
    point.z = (cont->pts[pt].z + cont->pts[pt + 1].z) / 2;
    imodPointAdd(fcont, &point, index++);
  }
  imodPointAdd(fcont, &(cont->pts[cont->psize - 1]), index++);
  point.x = (cont->pts[0].x + cont->pts[cont->psize - 1].x) / 2;
  point.y = (cont->pts[0].y + cont->pts[cont->psize - 1].y) / 2;
  point.z = (cont->pts[0].z + cont->pts[cont->psize - 1].z) / 2;
  imodPointAdd(fcont, &point, index++);
  return(fcont);
}

/*!
 * Scales all point values in contour [cont] by [spoint].  (Unused 4/22/05)
 */
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

/*!
 * Rotates contour [cont] in the X/Y plane about the origin by angle [rot] in
 * radians.
 */
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

/* Swaps X and Y coordinates; unused 4/22/05 */
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

/*!
 * Sets the direction of the contour [cont] to the value of [direction], either
 * IMOD_CONTOUR_CLOCKWISE or IMOD_CONTOUR_COUNTER_CLOCKWISE.
 */
void imodContourMakeDirection(Icont *cont, int direction)
{
  if (!cont)
    return;
  if (cont->psize < 3)
    return;
  if (direction != imodContZDirection(cont))
    imodel_contour_invert(cont);
}

/* Add points so that all points are about 1 to sqrt(2) pixels apart.
  Used only by the broken principal axis routine. */
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

/****************************************************************************/
/* ADVANCED FUNCTIONS - SCAN CONTOURS, OVERLAPS, NESTING, IMAGE
 * DOC_SECTION ADVANCED
 */ 

/*!
 * Creates a scan contour from contour [incont] and returns pointer to it, or 
 * NULL for an error.  A scan contour consists of pairs of points at the starts
 * and ends of horizontal lines.  The pairs are in order by increasing Y, and
 * by increasing X at each Y level.  Z coordinates are ignored.
 */
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
    return((Icont *)NULL);
  }


  /* DNM: move the duplication and unique calls to before the test for
     one point, mark it as scan type if only one point */

  ocont = imodContourDup(incont);
  if (!ocont)
    return((Icont *)NULL);
  imodel_contour_whole(ocont);
  imodContourUnique(ocont);

  if (ocont->psize == 1){
    cont = imodContourNew();
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

  imodContourGetBBox(ocont, &pmin, &pmax);
  ymin = pmin.y;
  ymax = pmax.y;

  /* DNM: but if there is just one line parallel to y, then make the scan
     contour from starting and ending points */
  if (ymin == ymax) {
    imodel_contour_sortx(ocont, 0, ocont->psize - 1);
    cont = imodContourNew();
    if (cont){
      imodPointAppend(cont, &ocont->pts[0]);
      imodPointAppend(cont, &ocont->pts[ocont->psize - 1]);
      cont->flags |= ICONT_SCANLINE;
    }
    imodContourDelete(ocont);
    return(cont);
  }
      
  /* active edge table */
  aet = imodContourNew();
  if (aet == NULL){
    imodContourDelete(ocont);
    return((Icont *)NULL);
  }

  /* contour to be returned */
  /* DNM 3/31/01: allocate a whole chuck for points */
  cont = imodContourNew();
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
    return((Icont *)NULL);
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
    return((Icont *)NULL);
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

      /* DNM 3/31/01: eliminate calling imodPointAdd with its
         repeated realloc's; just allocate when the chunk is full 
         Sorry, too lazy to clean up here if the realloc fails! */
      if (cont->psize == allocsize) {
        allocsize += chunksize;
        cont->pts = (Ipoint *)realloc(cont->pts,
                                      allocsize * sizeof(Ipoint));
        if (cont->pts == NULL)
          return((Icont *)NULL);
      }
      cont->pts[cont->psize++] = point;

      if (aet->pts[j].y <= i + ymin){

        /* Remove point from aet if it's done 
           DNM 3/26/01: can't use imodPointDelete after fixing
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
    return((Icont *)NULL);

  /* make sure each scanline has an even pair. */
  for(i = 0; i < cont->psize; i++){
    y = cont->pts[i].y;
    bgnpt = i;
    while (i < cont->psize - 1 && 
           cont->pts[i].y == cont->pts[i+1].y)
      ++i;
    endpt = i;

    if (! ( (endpt-bgnpt)% 2)){
      imodPointAdd(cont, &(cont->pts[i]), i);
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

/*!
 * Returns 1 if contour [c1] overlaps contour [c2] when projected onto the X/Y 
 * plane.  Returns 0 if not, or if an error occurs.
 */
int imodel_contour_overlap(Icont *c1, Icont *c2)
{
  Icont *cs1, *cs2;
  Ipoint pmax1, pmin1, pmax2, pmin2;
  int i, j, jstrt;

  /* first check and see if bounding box overlaps. */
  imodContourGetBBox(c1, &pmin1, &pmax1);
  imodContourGetBBox(c2, &pmin2, &pmax2);

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

/*!
 * Returns 1 if scan contour [c1] overlaps scan contour [c2].
 * Returns 0 if not, or for an error.
 */
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

/*!
 * Returns 1 if there is overlap between the contours whose addresses are
 * pointed to by [cs1p] and [cs2p], and returns fractions of each contour's
 * area that overlaps in [frac1] and [frac2].  The bounding boxes of the
 * contours must be provided in in [pmin1], [pmax1], [pmin2], and [pmax2].
 * If the contours are already scan contours, they are unchanged.  If they are
 * not scan contours and their bounding boxes do not overlap, the contours
 * are converted to scan contours and the address of the scan contour is
 * placed into [cs1p] and/or [cs2p].  This allows scan conversions to be done 
 * only when needed.  Returns 0 for no overlap or error.
 */
int imodel_overlap_fractions(Icont **cs1p, Ipoint pmin1, Ipoint pmax1,
                             Icont **cs2p, Ipoint pmin2, Ipoint pmax2,
                             float *frac1, float *frac2)
{
  int i, j, jstrt;
  float sum1 = 0.0;
  float sum2 = 0.0;
  float sumover = 0.0;
  int didoverlap = 0;
  float ovstart, ovend;
  Icont *cs1 = *cs1p;
  Icont *cs2 = *cs2p;

  *frac1 = *frac2 = 0.0;

  if (!cs1) 
    return(0);
  if (!cs2)
    return(0);

  /* first check and see if bounding box overlaps. */

  if (pmax1.x < pmin2.x)
    return(0);
  if (pmax2.x < pmin1.x)
    return(0);
  if (pmax1.y < pmin2.y)
    return(0);
  if (pmax2.y < pmin1.y)
    return(0);

  /* Make sure contours are scan contours now */
  if (!(cs1->flags & ICONT_SCANLINE)) {
    cs1 = imodel_contour_scan(cs1);
    if (!cs1)
      return 0;
    *cs1p = cs1;
  }
  
  if (!(cs2->flags & ICONT_SCANLINE)) {
    cs2 = imodel_contour_scan(cs2);
    if (!cs2)
      return 0;
    *cs2p = cs2;
  }


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



/* DNM 4/22/05: removed uncompiled "broken" imodContourTracer and icts_offset
   that it called */

/* Used to be used to sort points from auto contouring */  
int imodContourAutoSort(Icont *cont)
{
  Ipoint point;
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

/*!
 * Swaps the values in two contour structures [c1] and [c2] (Unused 4/22/05)
 */
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
  tc.time   = c1->time;
  c1->time  = c2->time;
  c2->time  = tc.time;
  tc.surf   = c1->surf;
  c1->surf  = c2->surf;
  c2->surf  = tc.surf;
  tc.sizes  = c1->sizes;
  c1->sizes = c2->sizes;
  c2->sizes = tc.sizes;
  return;
}

/* returns index of first point found inside of contour that matches point. */
/* returns -1 if no match. Unused 4/22/05 */
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

/*
 * Consolidated functions for dealing with contours and nesting in imodmesh,
 * imodinfo
 */

/*!
 * Makes tables for object [obj] of contour z values, number and contours at
 * each z value.  Returns -1 if error. ^
 * [incz] = Z increment ^
 * [clearFlag] = a flag to clear in cont->flags ^
 * [contzp] = pointer to array for Z values ^
 * [zlist] = pointer to returned array for list of z values ^
 * [numatzp] = pointer to returned array for number of contours at each Z ^
 * [contatzp] = pointer to returned array of int pointers to list of contours
 * at each Z ^
 * [zminp], [zmaxp] = pointers for return values of minimum and maximum Z ^
 * [zlsizep] = pointer for return value of size of list of Z values ^
 * [nummaxp] = pointer for return value of maximum contours on any section
 */
int imodContourMakeZTables(Iobj *obj, int incz, unsigned int clearFlag, 
                           int **contzp, int **zlistp, int **numatzp, 
                           int ***contatzp, int *zminp, int *zmaxp, 
                           int *zlsizep, int *nummaxp)
{
  int co;
  Icont *cont;
  int zmin, zmax, nummax;
  int i;
  int cz, z;
  int zlsize = 0;
  int *contz = NULL;
  int *numatz = NULL;
  int *zlist = NULL;
  int **contatz = NULL;

  /* Find min and max z values.
   * Clear the type value used to store connection information.
   */
  contz = (int *)malloc(obj->contsize * sizeof(int));
  if (!contz)
    return -1;
  zmin = INT_MAX;
  zmax = INT_MIN;
  for (co = 0; co < obj->contsize; co++) {
    cont = &(obj->cont[co]);
    cz = imodContourZValue(cont);
    contz[co] = cz;
    cont->flags &= ~clearFlag;
    if (cont->psize) {
      if (cz < zmin)
	zmin = cz;
      if (cz > zmax)
	zmax = cz;
    }
  }

  /* get list of z sections to connect for skip */
  zlist = (int *)malloc((zmax - zmin + 2) * sizeof(int));
  numatz = (int *)malloc((zmax - zmin + 2 + incz) * sizeof(int));
  contatz = (int **)malloc((zmax - zmin + 2 + incz) * sizeof(int *));
  if (!zlist || !numatz || !contatz) {
    imodContourFreeZTables(numatz, contatz, contz, zlist, zmin, zmax);
    return -1;
  }
  for (z = 1; z <= incz; z++) {
    numatz[zmax + z - zmin] = 0;
    contatz[zmax + z - zmin] = NULL;
  }
  for (z = zmin; z <= zmax; z++) {
    numatz[z - zmin] = 0;
    contatz[z - zmin] = NULL;
    for (co = 0; co < obj->contsize; co++) {
      cont = &(obj->cont[co]);
      cz = contz[co];
      if (cont->psize && cz == z) {
        zlist[zlsize++] = z;
        break;
      }
    }
  }
  
  /* Get number of contours at each z and tables of each */
  for (co = 0; co < obj->contsize; co++)
    if (obj->cont[co].psize)
      numatz[contz[co] - zmin]++;
  
  nummax =0;
  for (i = 0; i < zmax + 1 - zmin; i++) {
    contatz[i] = (int *)malloc(numatz[i] * sizeof(int));
    if (!contatz[i]) {
      imodContourFreeZTables(numatz, contatz, contz, zlist, zmin, zmax);
      return -1.;
    }
    if (numatz[i] > nummax)
      nummax = numatz[i];
    numatz[i] = 0;
  }
     			     
  for (co = 0; co < obj->contsize; co++) {
    if (obj->cont[co].psize) {
      i = contz[co] - zmin;
      contatz[i][numatz[i]++] = co;
    }
  }

  *nummaxp = nummax;
  *zminp = zmin;
  *zmaxp = zmax;
  *contzp = contz;
  *zlistp = zlist;
  *numatzp = numatz;
  *contatzp = contatz;
  *zlsizep = zlsize;
  return 0;
}

/*!
 * Frees the tables of contour Z values, number and contours at Z created
 * by @imodContourMakeZTables
 */
void imodContourFreeZTables(int *numatz, int **contatz, int *contz, int *zlist,
                            int zmin, int zmax)
{
  int i;
  if (zlist && numatz && contatz) {
    for (i = 0; i < zmax + 1 - zmin; i++) {
      if (numatz[i] && contatz[i])
        free (contatz[i]);
    }
  }

  if(numatz)
    free(numatz);
  if(contatz)
    free(contatz);
  if(contz)
    free(contz);
  if(zlist)
    free(zlist);
}

/*!
 * Checks for one contour inside another and maintains nesting structures ^
 * [co], [eco] = contour numbers, indexes into arrays ^
 * [scancont] = pointer to array of scan contours ^
 * [pmin], [pmax] = arrays of minimum and maximum points (bounding boxes) ^
 * [nests] = pointer to array of nesting structures ^
 * [nestind] = array for index from contours to nests ^
 * [numnests] = pointer to number of nests found ^
 * [numwarn] = pointer to flag for warning on contour overlap: -1 for no
 * warnings, 0 for warnings, becomes 1 after first warning
 */
int imodContourCheckNesting(int co, int eco, Icont **scancont, Ipoint *pmin,
                            Ipoint *pmax, Nesting **nests, int *nestind,
                            int *numnests, int *numwarn)
{
  float frac1, frac2;
  int inco, outco, nind;
  Nesting *nest;
  
  imodel_overlap_fractions(&scancont[co], pmin[co], pmax[co],
                           &scancont[eco], pmin[eco],
                           pmax[eco], &frac1, &frac2);
  if (frac1 > 0.99 || frac2 > 0.99) {
    if (frac2 > frac1) {
      inco = eco;
      outco = co;
    } else {
      inco = co;
      outco = eco;
    }
    /* add outside one to the inside's lists;
       create them new list if necessary */
    nind = nestind[inco];
    if (nind < 0) {
      if (*numnests)
        *nests = (Nesting *)realloc(*nests, (*numnests + 1 )* sizeof(Nesting));
      else
        *nests = (Nesting *)malloc(sizeof(Nesting));
      if (!*nests)
        return -1;
      nind = (*numnests)++;
      nestind[inco] = nind;
      nest = &((*nests)[nind]);
      nest->co = inco;
      nest->level = 0;
      nest->ninside = 0;
      nest->noutside = 0;
      nest->inscan = NULL;
    } else
      nest = &((*nests)[nind]);

    if (nest->noutside)
      nest->outside = (int *)realloc(nest->outside,
                                           (nest->noutside + 1) * sizeof(int));
    else
      nest->outside = (int *)malloc(sizeof(int));
    if (!nest->outside)
      return -1;
    nest->outside[nest->noutside++] = outco;
                         
    /* now add inside one to outside's list */
    nind = nestind[outco];
    if (nind < 0) {
      if (*numnests)
        *nests = (Nesting *)realloc(*nests, (*numnests + 1 )*sizeof(Nesting));
      else
        *nests = (Nesting *)malloc(sizeof(Nesting));
      nind = (*numnests)++;
      nestind[outco] = nind;
      nest = &((*nests)[nind]);
      nest->co = outco;
      nest->level = 0;
      nest->ninside = 0;
      nest->noutside = 0;
      nest->inscan = NULL;
    } else
      nest = &((*nests)[nind]);
    
    if (nest->ninside)
      nest->inside = (int *)realloc(nest->inside,
                                    (nest->ninside + 1) * sizeof(int));
    else
      nest->inside = (int *)malloc(sizeof(int));
    if (!nest->inside)
      return -1;
    nest->inside[nest->ninside++] = inco;
                           
  } else if ((frac1 > 0.1 || frac2 > 0.1) && *numwarn >= 0) {
    printf("WARNING: Contours %d and %d overlap by "
           "%.3f and %.3f\n", co + 1, eco + 1, frac1, frac2);
    if (!*numwarn)
      printf("To find these contours in 3dmod, you may "
             "first have to remove empty contours\n "
             "with the menu command Edit-Object-Clean\n");
    *numwarn = 1;
  }
  return 0;
}

/*!
 * Frees the array of [numnests] nests in [nests] and their internal arrays 
 */
void imodContourFreeNests(Nesting *nests, int numnests)
{
  Nesting *nest;
  int nind;
  for (nind = 0; nind < numnests && nests; nind++) {
    nest = &nests[nind];
    if (nest->ninside > 0 && nest->inscan)
      imodContourDelete(nest->inscan);
    if (nest->ninside && nest->inside)
      free(nest->inside);
    if (nest->noutside && nest->outside)
      free(nest->outside);
  }
  if(numnests && nests)
    free(nests);
}

/*! 
 * Analyzes inside and outside contours to determine level.
 * [nests] is an array of [numnests] nesting structures, [nestind] is an
 * array with indexes from contours to nests.
 */
void imodContourNestLevels(Nesting *nests, int *nestind, int numnests)
{
  int level, more, ready, nind, oind, i;
  level = 1;
  do {
    more = 0;
    for (nind = 0; nind < numnests; nind++) {
      if (nests[nind].level)
        continue;
      ready = 1;
      /* if the only contours outside have level assigned but lower
         than the current level, then this contour can be assigned to
         the current level */
      for (i = 0; i < nests[nind].noutside; i++) {
        oind = nestind[nests[nind].outside[i]];
        if (nests[oind].level == 0 || nests[oind].level >= level) {
          more = 1;
          ready = 0;
          break;
        }
      }
      if (ready) 
        nests[nind].level = level;
    }
    level++;
  } while(more);
}



/****************************************************************************/
/* SIMPLE SET/GET FUNCTIONS
 * DOC_SECTION SET-GET
 */ 

/*! Returns pointer to the label structure of [inContour] or NULL if no 
  contour or label */
Ilabel *imodContourGetLabel(Icont *inContour)
{ 
  if (!inContour) return(NULL);
  return(inContour->label); 
}

/*! Sets the label of [inContour] to [inLabel] */
void imodContourSetLabel(Icont *inContour, Ilabel *inLabel)
{
  if (!inContour) return;
  if (!inLabel) return;
  inContour->label = inLabel;
  return;
}

/*! Returns the number of points in [inContour] */
int     imodContourGetMaxPoint(Icont *inContour)
{ 
  if (!inContour) return(0);
  return(inContour->psize); 
}

/*! Returns pointer to the point array of [inContour], or NULL if none */
Ipoint *imodContourGetPoints(Icont *inContour) 
{ 
  if (!inContour) return(NULL);
  if (!inContour->psize) return(NULL);
  return(inContour->pts); 
}

/*! Sets the point array of [inContour] to [inPoint] and sets the number of
  points to [inMax] */
void imodContourSetPointData(Icont *inContour, Ipoint *inPoint, int inMax)
{
  if (!inContour) return;
  inContour->pts = inPoint;
  inContour->psize = inMax;
}

/*! Returns pointer to point [inIndex] of [inContour] or NULL for error */
Ipoint *imodContourGetPoint(Icont *inContour, int inIndex)
{
  if (!inContour) return(NULL);
  if (!inContour->psize) return(NULL);
  if ((inIndex < 0) || (inIndex >= inContour->psize))
    return(NULL);
  return(&inContour->pts[inIndex]);
}

/*! Returns the time value of [inContour], or 0 if no contour */
int     imodContourGetTimeIndex(Icont *inContour)
{
  if (!inContour) return(0);
  return(inContour->time);
}

/*! Sets the time value of inContour] to [inTime] */
void    imodContourSetTimeIndex(Icont *inContour, int inTime)
{
  if (!inContour) return;
  inContour->time = inTime;
}

/*! Returns the surface number of [inContour], or 0 if no contour */
int     imodContourGetSurface(Icont *inContour)
{
  if (!inContour) return(0);
  return(inContour->surf);
}

/*! Sets the surface number of inContour] to [inSurface] */
void    imodContourSetSurface(Icont *inContour, int inSurface)
{
  if (!inContour) return;
  inContour->surf = inSurface;
}

/*! Sets bit flags defined by [inFlag] in the {flag} element of [inContour]
 to the off if [inState] is 0 or on if [inState] is nonzero. */
void imodContourSetFlag(Icont *inContour, unsigned int inFlag, int inState)
{
  if (inState)
    inContour->flags |= inFlag;
  else
    inContour->flags &= ~inFlag;
}

/*! Returns the value of the bit flags defined by [inFlag] in the {flag} 
   element of [inContour] */
int imodContourGetFlag(Icont *inContour, unsigned int inFlag)
{
  return (inContour->flags & inFlag);
}

/*! Returns pointer to name string of [inContour], or to empty string if 
  none */
char *imodContourGetName(Icont *inContour)
{
  static char name = 0x00;

  if (!inContour) return(&name);
  if (!inContour->label) return(&name);
  if (!inContour->label->name) return(&name);
  return(inContour->label->name);
}

/* END_SECTION */
