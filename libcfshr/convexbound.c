/*
 * convexbound.c - routine for finding a convex boundary with omissions and
 * padding
 *      
 * $Id$
 *      
 * $Log$
 * Revision 1.2  2009/11/29 16:12:23  mast
 * Fixed declarations
 *
 * Revision 1.1  2009/11/28 20:09:32  mast
 * translated to C
 *
 *
 */      

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "imodconfig.h"
#include "b3dutil.h"

#ifdef F77FUNCAP
#define convexbound CONVEXBOUND
#else
#define convexbound convexbound_
#endif

static void cenorder(float *sx, float *sy, int npnts, int *inddist, 
                     float *cendist, float *xcen,float *ycen);
static void angorder(float *sx, float *sy, int nptuse, int *inddist,
                     float xcen, float ycen, int *iangtopt, int *ipttoang,
                     float *angles);

/*!
 * Finds the smallest convex boundary around an arbitrary
 * set of points in [sx], [sy].  [npnts] is the total number of points.
 * If [fracomit] is nonzero, it omits that fraction of points from
 * consideration completely, choosing the points that are farthest from
 * the centroid of all the points.  (If [fracomit] is negative, the Y
 * coordinates of the points will be temporarily scaled to have the 
 * same standard deviation as the X range in order to determine the
 * farthest points; it then eliminates [-fracomit] points.) It returns
 * the centroid of the points under consideration in [xcen], [ycen].  It
 * returns the set of boundary points in [bx], [by] and the number of points in
 * [nvert].  The size of [bx] and [by] is specified by [maxverts].  If [pad]
 * is nonzero, it makes the boundary points be that 
 * distance away from the outermost [sx], [sy] points.  
 * It uses Graham's scan algorithm.  [nvert] is returned with -1 for an error 
 * allocating temporary arrays, or -2 for the boundary arrays not large enough.
 */
void convexBound(float *sx, float *syin, int npnts, float fracomit, float pad,
                float *bx, float *by, int *nvert, float *xcen, float *ycen,
                int maxverts)
{
  float *cendist, *sy, *angtmp;
  int *inddist, *iangtopt;
  unsigned char *vertex;
  int *ipttoang;
  int ind, ind1,ind2,ind3, i, nptuse, itry, indstart;
  float avgx, sdx, semx, avgy, sdy, semy, ymin, xmax, padfrac;

#define nextpt(a) iangtopt[(ipttoang[a] + 1) % nptuse]

  cendist = (float *)malloc(npnts * sizeof(float));
  angtmp = (float *)malloc(npnts * sizeof(float));
  sy = (float *)malloc(npnts * sizeof(float));
  inddist = (int *)malloc(npnts * sizeof(int));
  iangtopt = (int *)malloc(npnts * sizeof(int));
  ipttoang = (int *)malloc(npnts * sizeof(int));
  vertex = (unsigned char *)malloc(npnts * sizeof(int));
  if (!cendist || !angtmp || !sy || !inddist || !iangtopt || !vertex) {
    if (cendist) free(cendist);
    if (angtmp) free(angtmp);
    if (sy) free(sy);
    if (inddist) free(inddist);
    if (iangtopt) free(iangtopt);
    if (ipttoang) free(ipttoang);
    if (vertex) free(vertex);
    printf("convexbound - failure to allocate memory\n");
    *nvert = -1;
    return;
  }

  /* scale y to have same sd as x if fracomit is negative */
  sdx=1.;
  sdy=1.;
  if (fracomit < 0.) {
    avgSD(sx,npnts,&avgx,&sdx,&semx);
    avgSD(syin,npnts,&avgy,&sdy,&semy);
  }
       
  /* start with pointers in numerical order */
  for (i=0; i < npnts; i++) {
    inddist[i]=i;
    sy[i]=sdx*syin[i]/sdy;
  }
       
  /* find distances from center and order by distance */
  cenorder(sx, sy, npnts, inddist, cendist, xcen, ycen);
  nptuse=npnts;

  /* if omitting farthest points, find distances again */
  if (fracomit) {
    nptuse=B3DMAX(npnts-B3DNINT(fabs((double)fracomit)*npnts), 
                  B3DMIN(3,npnts));
    if (fracomit < 0)
      for (i = 0; i < npnts; i++)
        sy[i]=syin[i];
    cenorder(sx,sy,nptuse,inddist,cendist,xcen,ycen);
  }
    
  /* get pointers to points in order by angle and inverse pointers */
  angorder(sx,sy,nptuse,inddist,*xcen,*ycen,iangtopt,ipttoang, angtmp);

  /* find the rightmost lowest point; it must be a vertex */
  ymin=1.e30;
  for (i = 0; i < nptuse; i++) {
    ind=inddist[i];
    if (sy[ind] < ymin || (sy[ind] == ymin && sx[ind] > xmax)) {
      indstart=ind;
      ymin=sy[ind];
      xmax=sx[ind];
    }
    vertex[ind]=1;
  }
       
  /* start the scan at INDSTART and next 2 points */
  ind1=indstart;
  ind2=nextpt(ind1);
  ind3=nextpt(ind2);
  *nvert=nptuse;
  while (ind2 != indstart && *nvert > 2) {

    /* test for left or right turn */
    if (sx[ind1]*(sy[ind2]-sy[ind3])-sx[ind2]*(sy[ind1]-sy[ind3])
        +sx[ind3]*(sy[ind1]-sy[ind2]) > 0.) {
           
      /* left turn; advance the scan */
      ind1=ind2;
      ind2=ind3;
      ind3=nextpt(ind3);
    } else {
           
      /* right turn; mark point 2 as non-vertex */
      vertex[ind2]=0;
      (*nvert)--;
      if (ind1 == indstart) {
             
        /* if still at start, advance points 2 and 3 */
        ind2=ind3;
        ind3=nextpt(ind3);
      } else {
             
        /* otherwise drop points 1 and 2 back; make point 1 be the last
           point that is still eligible as a vertex */
        ind2=ind1;
        do {
          itry=ipttoang[ind1]-1;
          if(itry < 0)
            itry=nptuse - 1;
          ind1=iangtopt[itry];
        } while (!vertex[ind1]);
      }
    }
  }
       
  /* put vertices into bx,by in order by angle relative to
   * centroid, with pad if called for */
  *nvert=0;
  for (i = 0; i < nptuse; i++) {
    ind=iangtopt[i];
    if(vertex[ind]) {
      if (*nvert >= maxverts) {
        printf("convexbound: The contour has too many vertices for the "
               "arrays\n");
        *nvert = -2;
        break;
      }
      padfrac=0.;
      if(pad > 0. && cendist[ind] > 1.e-10)
        padfrac=(float)pad/sqrt((double)cendist[ind]);
      bx[*nvert]=sx[ind]+padfrac*(sx[ind]-*xcen);
      by[*nvert]=sy[ind]+padfrac*(sy[ind]-*ycen);
      (*nvert)++;
    }
  }
  free(cendist);
  free(angtmp);
  free(sy);
  free(inddist);
  free(iangtopt);
  free(ipttoang);
  free(vertex);
  return;
}

/*!
 * Fortran wrapper for @convexBound
 */
void convexbound(float *sx, float *syin, int *npnts, float *fracomit, 
                 float *pad, float *bx, float *by, int *nvert, float *xcen,
                 float *ycen, int *maxverts)
{
  convexBound(sx, syin, *npnts, *fracomit, *pad, bx, by, nvert, xcen, ycen,
              *maxverts);
}

/*       
 * CENORDER finds the centroid XCEN, YCEN of the points in the SX,XY
 * array pointed to by the first NPNTS entries in the pointer INDDIST
 * It also returns distances from the centroid in CENDIST and orders
 * the array INDDIST to point to the points in order by increasing
 * CENDIST
 */
static void cenorder(float *sx, float *sy, int npnts, int *inddist,
                     float *cendist, float *xcen,float *ycen)
{
  double xsum, ysum;
  int i, ind;

  /* find centroid of the points */
  xsum=0.;
  ysum=0.;
  for (i = 0; i < npnts; i++) {
    xsum += sx[inddist[i]];
    ysum += sy[inddist[i]];
  }
  *xcen=xsum/npnts;
  *ycen=ysum/npnts;

  /* get distances from center */
  for (i = 0; i < npnts; i++) {
    ind=inddist[i];
    cendist[ind]=(sx[ind]-*xcen)*(sx[ind]-*xcen) + 
      (sy[ind]-*ycen)*(sy[ind]-*ycen);
  }
     
  /* order pointers by distance */
  rsSortIndexedFloats(cendist, inddist, npnts);
}

       
/* ANGORDER computes the angles of NPTUSE points in the arrays SX, SY
 * (pointed to by INDDIST), relative to a center point XCEN, YCEN,
 * and then orders the points by increasing angle.  IANGTOPT points to
 * the points in order by increasing angle, while IPTTOANG is the
 * inverse pointer, used to get back from point number to angle number.
 */
static void angorder(float *sx, float *sy, int nptuse, int *inddist, 
                     float xcen, float ycen, int *iangtopt, int *ipttoang,
                     float *angle)
{
  int i, ind;
  for (i = 0; i < nptuse; i++) {
    ind=inddist[i];
    angle[ind]=(float)atan2((double)(sy[ind]-ycen),(double)(sx[ind]-xcen));
    iangtopt[i]=ind;
  }
  rsSortIndexedFloats(angle, iangtopt, nptuse);
  for (i = 0; i < nptuse; i++) {
    ipttoang[iangtopt[i]]=i;
  }
}
