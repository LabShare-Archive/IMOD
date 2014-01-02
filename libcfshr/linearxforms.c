/*  linearxforms.c - functions for manipulating linear transformations and other matrices
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <math.h>
#include <stdio.h>
#include "imodconfig.h"
#include "b3dutil.h"

#ifdef F77FUNCAP
#define xfunit XFUNIT
#define xfcopy XFCOPY
#define xfmult XFMULT
#define xfinvert XFINVERT
#define xfapply XFAPPLY
#define icalc_matrix ICALC_MATRIX
#else
#define xfunit xfunit_
#define xfcopy xfcopy_
#define xfmult xfmult_
#define xfinvert xfinvert_
#define xfapply xfapply_
#ifdef G77__HACK
#define icalc_matrix icalc_matrix__
#else
#define icalc_matrix icalc_matrix_
#endif
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

/*!
 * Given a set of rotations angles about the X, Y, and Z axes in the first, second, and 
 * third elements of [angles], finds the 3-D rotation matrix and returns it in [matrix].
 * The number of rows in the matrix is specified by [rows], which should be 3 when calling
 * with a packed 3x3 matrix, or 4 when calling with an Imat {data} array.  The order of
 * data elements in the array is r11, r21, r31, etc.  These are the conventions: ^
 *       [R] premultiplies column vector of coordinates: ^
 *       |xnew|       | r11  r12  r13 | |xold|  ^
 *       |ynew|   =   | r21  r22  r23 | |yold|  ^
 *       |znew|       | r31  r32  r33 | |zold|  ^
 *       
 *       rotations applied in the order Z first, X last  ^
 *       ie  [R] = [X][Y][Z]  ^
 *       
 *       rotations are right-handed - i.e., positive angle rotates counterclockwise when
 *       looking down the respective axis ^
 *       
 *       [X] = ^
 *       |   1    0     0   |  ^
 *       |   0   cosa -sina |  ^
 *       |   0   sina  cosa |  ^
 *       
 *       [Y] = ^
 *       |  cosb  0   sinb  |  ^
 *       |   0    1     0   |  ^
 *       | -sinb  0   cosb  |  ^
 *       
 *       [Z] = ^
 *       |  cosg -sinb  0   |  ^
 *       |  sing  cosg  0   |  ^
 *       |   0    0     1   |  ^
 */
void anglesToMatrix(float *angles, float *matrix, int rows)
{
  double cnv = 0.0174532921;
  double ca, cb, cg, sa, sb, sg, alpha, beta, gamma;

 /* This is a translation of code formerly in flib/subrs/imsubs2/icalc_matrix.f */
  alpha = angles[0] * cnv;
  beta  = angles[1] * cnv;
  gamma = angles[2] * cnv;
  ca = cos(alpha);
  cb = cos(beta);
  cg = cos(gamma);
  sa = sin(alpha);
  sb = sin(beta);
  sg = sin(gamma);

  matrix[0] = cb * cg;
  matrix[rows] = -cb * sg;
  matrix[2 * rows] = sb;
  matrix[1] = sa * sb * cg + ca * sg;
  matrix[1 + rows] = -sa * sb * sg + ca * cg;
  matrix[1 + 2 * rows] = -sa * cb;
  matrix[2] = -ca * sb * cg + sa * sg;
  matrix[2 + rows] = ca * sb * sg + sa * cg;
  matrix[2 + 2 * rows] = ca * cb;
}

/*! Fortran wrapper to @anglesToMatrix assuming [matrix] has 3 rows */
void icalc_matrix(float *angles, float *matrix)
{
  anglesToMatrix(angles, matrix, 3);
}

double sDet;

/*!
 * Given a 3D rotation matrix in [matrix], whose number of rows is [rows], it finds 
 * the angles of rotation about the three axes, in the order Z, Y, X, and returns them in
 * [x], [y], and [z].  Returns 1 if the determinant of the matrix is not near zero.  
 * Angles are in degrees.  The conventions of @anglesToMatrix are followed.
 */
int matrixToAngles(float *matrix, double *x, double *y, double *z, int rows)
{
  double r11, r12, r13, r21, r22, r23, r31, r32, r33;
  double crit = 0.01;
  double cnv = 0.017453292;
  double small = 0.0000001;
  double alpha, beta, gamma, cosg, sing, cosb, test1, test2;

 /* This is a translation of code formerly in flib/subrs/imsubs2/icalc_angles.f */
  r11 = matrix[0];
  r12 = matrix[rows];
  r13 = matrix[2 * rows];
  r21 = matrix[1];
  r22 = matrix[1 + rows];
  r23 = matrix[1 + 2 * rows];
  r31 = matrix[2];
  r32 = matrix[2 + rows];
  r33 = matrix[2 + 2 * rows];

  /* first check matrix */
  sDet = r11 * r22 * r33 - r11 * r23 * r32 + r12 * r23 * r31 - r12 * r21 * r33
    + r13 * r21 * r32 - r13 * r22 * r31;

  sDet -= 1.0;
  if (sDet > crit || sDet < -crit)
    return 1;

  test1 = r13 - 1.0;
  if (test1 < 0.0)
    test1 = -test1;
  test2 = r13 + 1.0;
  if (test2 < 0.0)
    test2 = -test2;
  if (test1 < small || test2 < small) {
    beta = asin(r13);
    gamma = atan2(r21, r22);
    alpha = 0.0;

  } else if (r13 <= small && r13 >= -small) {

    beta = 0.0;
    gamma = atan2 (-r12, r11);
    alpha = atan2 (-r23, r33);

  } else {

    alpha = atan2 (-r23, r33);
    gamma = atan2 (-r12, r11);
    cosg = cos(gamma);
    sing = sin(gamma);
    if (cosg > crit || cosg < -crit) 
      cosb = r11 / cosg;
    else
      cosb = -r12 / sing;
    beta = atan2 (r13, cosb);
  }
  *x = alpha / cnv;
  *y = beta / cnv;
  *z = gamma / cnv;
  return 0;
}

/*!
 * Fortran wrapper to @matrixToAngles that assumes [matrix] has three rows and that 
 * places X, Y, and Z rotations into [angles].  If the determinant is not zero, it exits
 * with matrix and determinant output and an error message.  This function is callable
 * from C with this name.
 */
void icalc_angles(float *angles, float *matrix)
{
  double x, y, z;
  if (!matrixToAngles(matrix, &x, &y, &z, 3)) {
    angles[0] = x;
    angles[1] = y;
    angles[2] = z;
    return;
  }

  /* And here is what the old fortran routine did: */
  printf("icalc_angles - matrix %10.6f %10.6f %10.6f\n", matrix[0], matrix[3], matrix[6]);
  printf("                      %10.6f %10.6f %10.6f\n", matrix[1], matrix[4], matrix[7]);
  printf("                      %10.6f %10.6f %10.6f\n", matrix[2], matrix[5], matrix[8]);
  printf("determinant - %f\n", sDet);
  printf("ERROR: icalc_angles - Not a pure rotation matrix\n");
  fflush(stdout);
}
