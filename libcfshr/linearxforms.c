/*  linearxforms.c - functions for mainpulating linear transformations
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 * Log at end of file
 */

#include <math.h>
#include "imodconfig.h"
#include "b3dutil.h"

#ifdef F77FUNCAP
#define xfunit XFUNIT
#define xfcopy XFCOPY
#define xfmult XFMULT
#define xfinvert XFINVERT
#define xfapply XFAPPLY
#else
#define xfunit xfunit_
#define xfcopy xfcopy_
#define xfmult xfmult_
#define xfinvert xfinvert_
#define xfapply xfapply_
#endif

/*
 * In these functions, the transform can have either 2 or 3 rows of 3 columns, with
 * the rows argument indicating the number of rows.  Existing values in a third row
 * are ignored, and the third row is returned with 0, 0, 1.  The Fortran wrappers call
 * with rows equal to 2.
 *
 * Linear transforms specify the following operation on centered coordinates:
 *   xp = a11 * x + a12 * y + dx
 *   yp = a21 * x + a22 * y + dy
 * Here is a comprehensive list of the locations of these components:
 *       generic  C [9]   C [6]  Fortran (2,3)
 *  a11    0       0       0      (1,1)
 *  a21    1       1       1      (2,1)
 *  a12   rows     3       2      (1,2)
 *  a22   rows+1   4       3      (2,2)
 *  dx    2*rows   6       4      (1,3)
 *  dy    2*rows+1 7       5      (2,3)
 */

/*!
 * Initializes transform [f] with a11 and a22 set equal to [val]; use 1.0 for a unit 
 * transform.
 */
void xfUnit(float *f, float val, int rows)
{
  int i;
  for (i = 0; i < 3 * rows; i++)
    f[i] = 0.;
  f[0] = val;
  f[1 + rows] = val;
  if (rows > 2)
    f[2 * (1 + rows)] = 1.;
    
}

void xfunit(float *f, float *val) {xfUnit(f, *val, 2);}

/*!
 * Copies transform [f1] to [f2], which have [rows1] and [rows2] rows, respectively.
 */
void xfCopy(float *f1, int rows1, float *f2, int rows2)
{
  int row, col;
  if (rows2 > 2)
    xfUnit(f2, 1., rows2);
  for (col = 0; col < 3; col++) 
    for (row = 0; row < 2; row++)
      f2[row + col * rows2] = f1[row + col * rows1]; 
}

void xfcopy(float *f1, float *f2) {xfCopy(f1, 2, f2, 2);}

/*!
 * Multiples transform [f1] (the one applied first) by [f2] (the one applied second)
 * and places the result in [prod], which can be the same as [f1] or [f2].
 */
void xfMult(float *f1, float *f2, float *prod, int rows)
{
  float tmp[9];
  int idx = 2*rows;
  int idy = 2*rows + 1;
  tmp[0] = (f2[0] * f1[0]) + (f2[rows] * f1[1]);
  tmp[1] = (f2[1] * f1[0]) + (f2[rows+1] * f1[1]);
  tmp[rows] = (f2[0] * f1[rows]) + (f2[rows] * f1[rows+1]);
  tmp[rows+1] = (f2[1] * f1[rows]) + (f2[rows+1] * f1[rows+1]);
  tmp[idx] = (f2[0] * f1[idx]) + (f2[rows] * f1[idy]) + f2[idx];
  tmp[idy] = (f2[1] * f1[idx]) + (f2[rows+1] * f1[idy]) + f2[idy];
  if (rows > 2) {
    tmp[2] = tmp[5] = 0.;
    tmp[8] = 1.;
  }
  for (idx = 0; idx < 3 * rows; idx++)
    prod[idx] = tmp[idx];
}

void xfmult(float *f1, float *f2, float *prod) {xfMult(f1, f2, prod, 2);}

/*!
 * Takes the inverse of transform [f] and returns the result in [finv], which can be the 
 * same as [f].
 */
void xfInvert(float *f, float *finv, int rows)
{
  float tmp[9];
  float denom = f[0] * f[rows+1] - f[rows] * f[1];
  int idx = 2*rows;
  int idy = 2*rows + 1;
  tmp[0] = f[rows+1] / denom;
  tmp[rows] = -f[rows] / denom;
  tmp[1] = -f[1] / denom;
  tmp[rows+1] = f[0] / denom;
  tmp[idx] = -(tmp[0] * f[idx] + tmp[rows] * f[idy]);
  tmp[idy] = -(tmp[1] * f[idx] + tmp[rows+1] * f[idy]);
  if (rows > 2) {
    tmp[2] = tmp[5] = 0.;
    tmp[8] = 1.;
  }
  for (idx = 0; idx < 3 * rows; idx++)
    finv[idx] = tmp[idx];
}

void xfinvert(float *f, float *finv) {xfInvert(f, finv, 2);}

/*!
 * Applies transform [f] to the point [x], [y], with the center of transformation at
 * [xcen], [ycen], and returns the result in [xp], [yp], which can be the same as
 * [x], [y].
 */
void xfApply(float *f, float xcen, float ycen, float x, float y, float *xp, float *yp,
             int rows)
{
  float xadj = x - xcen;
  float yadj = y - ycen;
  *xp = f[0] * xadj + f[rows] * yadj + f[2*rows] + xcen;
  *yp = f[1] * xadj + f[rows+1] * yadj + f[2*rows+1] + ycen;
}

void xfapply(float *f, float *xcen, float *ycen, float *x, float *y, float *xp, float *yp)
{
  xfApply(f, *xcen, *ycen, *x, *y, xp, yp, 2);
}

  
/*

$Log$
Revision 1.1  2011/05/24 18:17:31  mast
Initial version


*/
