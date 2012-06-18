/*
 * circlefit.c  - Functions for fitting circles or spheres to points
 *
 * Copyright (C) 2007 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 */

#include <math.h>
#include "b3dutil.h"
#include "imodconfig.h"

#ifdef F77FUNCAP
#define fitcircle FITCIRCLE
#define fitcirclewgt FITCIRCLEWGT
#else
#define fitcircle fitcircle
#define fitcirclewgt fitcirclewgt_
#endif

static void circleErr(float *y, float *error);
static void sphereErr(float *y, float *error);
static void circleErrWgt(float *y, float *error);
static void sphereErrWgt(float *y, float *error);

/*!
 * Computes the radius [rad] and center ([xc], [yc]) for a circle through the 3
 * given points ([x1], [y1]), ([x2], [y2]), and ([x3], [y3]).  Returns 1 for
 * error (square root of negative number).
 */
int circleThrough3Pts(float x1, float y1, float x2, float y2, float x3, 
                      float y3, float *rad, float *xc, float *yc)
{
  double a, d, e, f, sq1, sq2, sq3;
  double absa, rsq;
  sq1 = x1 * x1 + y1 * y1;
  sq2 = x2 * x2 + y2 * y2;
  sq3 = x3 * x3 + y3 * y3;
  /* printf("x,y,sq: %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f %.2f\n",
     x1, y1, sq1, x2, y2, sq2, x3, y3, sq3); */

  /* Needed to use doubles for the 1.'s to keep them accurate enough */
  a = determ3(x1, y1, 1., x2, y2, 1., x3, y3, 1.);
  d = -determ3(sq1, y1, 1., sq2, y2, 1., sq3, y3, 1.);
  e = determ3(sq1, x1, 1., sq2, x2, 1., sq3, x3, 1.);
  f = -determ3(sq1, x1, y1, sq2, x2, y2, sq3, x3, y3);
  /* printf("a, d, e, f: %f  %f  %f  %f\n", a, d, e, f); */
  absa = fabs((double)a);
  if (absa < 1.e-15 * fabs((double)d) || absa < 1.e-15 * fabs((double)e) ||
      absa < 1.e-15 * fabs((double)f))
    return 1;
  *xc = (float)(-0.5 * d / a);
  *yc = (float)(-0.5 * e / a);
  rsq = (d * d + e * e) / (4. * a * a) - f / a;
  if (rsq < 0.)
    return 1;
  *rad = (float)sqrt(rsq);
  return 0;
}


/* Static variables to make the data accessible to the error function */
static float *xp;
static float *yp;
static float *zp;
static float *wgt;
static int numpt;
#define MAXVAR 5

/*! 
 * Fit a circle or sphere to a set of points using a simplex search with
 * the @amoeba routine. ^
 * Inputs: X and Y coordinates in arrays [xpt], [ypt]; [zpt] is NULL for a 
 * circle fit or has the array of Z coordinate; number of points in [numPts]. ^
 * Inputs/Outputs: radius in [rad], center coordinate in [xcen], [ycen], and [zcen]
 * for a sphere fit; these must be initialized with reasonable starting values for a
 * search. ^
 * Output: RMS error in [rmsErr].  Returns 0.
 */
int fitSphere(float *xpt, float *ypt, float *zpt, int numPts, float *rad, 
              float *xcen, float *ycen, float *zcen, float *rmsErr)
{
  return fitSphereWgt(xpt, ypt, zpt, NULL, numPts, rad, xcen, ycen, zcen,
                      rmsErr);
}

/*! 
 * Fit a circle or sphere to a set of points with weighting of errors using a 
 * simplex search with the @amoeba routine. ^
 * Inputs: X and Y coordinates in arrays [xpt], [ypt]; [zpt] is NULL for a 
 * circle fit or has the array of Z coordinate; [weights] has an array of 
 * weights or is NULL for no weighting; the number of points is in [numPts]. ^
 * Inputs/Outputs: radius in [rad], center coordinate in [xcen], [ycen], and [zcen]
 * for a sphere fit; these must be initialized with reasonable starting values for a
 * search. ^
 * Output: RMS error in [rmsErr].  Returns 0.
 */
int fitSphereWgt(float *xpt, float *ypt, float *zpt, float *weights, 
                 int numPts, float *rad, float *xcen, float *ycen, float *zcen,
                 float *rmsErr)
{
  float pp[MAXVAR + 1][MAXVAR + 1], yy[MAXVAR + 1];
  float ptol[MAXVAR], a[MAXVAR];
  int iter, jmin, i, nvar;
  float errmin, ftol1, ftol2, delfac, ptol1, ptol2;
  float da[MAXVAR] = {2., 2., 2., 2.};
  void (*funk)(float *, float *) = circleErr;
  if (weights) {
    funk = circleErrWgt;
    wgt = weights;
  }
  
  delfac = 2.;
  ftol2 = 5.e-4f;
  ftol1 = 1.e-5f;
  ptol2 = 0.1f;
  ptol1 = .002f;
  nvar = 3;

  xp = xpt;
  yp = ypt;
  numpt = numPts;
  a[0] = *rad;
  a[1] = *xcen;
  a[2] = *ycen;

  if (zpt) {
    zp = zpt;
    a[3] = *zcen;
    funk = sphereErr;
    if (weights)
      funk = sphereErrWgt;
    nvar = 4;
  }

  funk(a, &errmin);
  if (errmin > 0.) {
    amoebaInit(&pp[0][0], yy, MAXVAR + 1, nvar, delfac, ptol2, a, da, funk,
               ptol);
    amoeba(&pp[0][0], yy, MAXVAR + 1, nvar, ftol2, funk, &iter, ptol, &jmin);
    for (i = 0; i < nvar; i++)
      a[i] = pp[i][jmin];

    amoebaInit(&pp[0][0], yy, MAXVAR + 1, nvar, delfac, ptol1, a, da, funk,
               ptol);
    amoeba(&pp[0][0], yy, MAXVAR + 1, nvar, ftol1, funk, &iter, ptol, &jmin);
    for (i = 0; i < nvar; i++)
      a[i] = pp[i][jmin];
    funk(a, &errmin);
  }

  *rad  = a[0];
  *xcen  = a[1];
  *ycen  = a[2];
  if (zpt)
    *zcen = a[3];
  *rmsErr = (float)sqrt((double)errmin);
  return 0;
}

/*! Fortran wrapper to @fitSphere for fitting a circle */
void fitcircle(float *xpt, float *ypt, int *numPts, float *rad, 
                  float *xcen, float *ycen, float *rmsErr)
{
  fitSphereWgt(xpt, ypt, NULL, NULL, *numPts, rad, xcen, ycen, NULL, rmsErr);
}

/*! Fortran wrapper to @fitSphereWgt for fitting a circle with weights */
void fitcirclewgt(float *xpt, float *ypt, float *weights, int *numPts, float *rad, 
                  float *xcen, float *ycen, float *rmsErr)
{
  fitSphereWgt(xpt, ypt, NULL, weights, *numPts, rad, xcen, ycen, NULL, rmsErr);
}

/* Function to compute the error of the circle fit for given vector y */
static void circleErr(float *y, float *error)
{
  float xcen, ycen, rad;
  double delx, dely, delrad, err;
  int i;

  rad = y[0];
  xcen = y[1];
  ycen = y[2];
  err = 0.;
  for (i = 0; i < numpt; i++) {
    delx = xp[i] - xcen;
    dely = yp[i] - ycen;
    delrad = sqrt(delx * delx + dely * dely) - rad;
    err += delrad * delrad;
  }

  *error = (float)(err / numpt);
}

/* Function to compute the error of the circle fit for given vector y with 
   weighting */
static void circleErrWgt(float *y, float *error)
{
  float xcen, ycen, rad;
  double delx, dely, delrad, err;
  int i;

  rad = y[0];
  xcen = y[1];
  ycen = y[2];
  err = 0.;
  for (i = 0; i < numpt; i++) {
    delx = xp[i] - xcen;
    dely = yp[i] - ycen;
    delrad = sqrt(delx * delx + dely * dely) - rad;
    err += delrad * delrad * wgt[i];
  }

  *error = (float)(err / numpt);
}

/* Function to compute the error of the sphere fit for given vector y */
static void sphereErr(float *y, float *error)
{
  float xcen, ycen, zcen, rad;
  double delx, dely, delz, delrad, err;
  int i;

  rad = y[0];
  xcen = y[1];
  ycen = y[2];
  zcen = y[3];
  err = 0.;
  for (i = 0; i < numpt; i++) {
    delx = xp[i] - xcen;
    dely = yp[i] - ycen;
    delz = zp[i] - zcen;
    delrad = sqrt(delx * delx + dely * dely + delz * delz) - rad;
    err += delrad * delrad;
  }

  *error = (float)(err / numpt);
}

/* Function to compute the error of the sphere fit for given vector y with 
   weights*/
static void sphereErrWgt(float *y, float *error)
{
  float xcen, ycen, zcen, rad;
  double delx, dely, delz, delrad, err;
  int i;

  rad = y[0];
  xcen = y[1];
  ycen = y[2];
  zcen = y[3];
  err = 0.;
  for (i = 0; i < numpt; i++) {
    delx = xp[i] - xcen;
    dely = yp[i] - ycen;
    delz = zp[i] - zcen;
    delrad = sqrt(delx * delx + dely * dely + delz * delz) - rad;
    err += delrad * delrad * wgt[i];
  }

  *error = (float)(err / numpt);
}
