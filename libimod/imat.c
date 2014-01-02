/*
 *  imat.c : a quick and dirty way to get transforms on none gl machines.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */
/*

$Log$
Revision 3.3  2005/10/19 14:05:16  mast
Added 3D inversion and a print function, documented for sourcedoc

*/

#include <math.h>
#include "imodel.h"

/*!
 * Creates a new matrix structure and sets it to the identity matrix.  The 
 * input dimension [dim] can be 2 or 3.  Returns NULL for error.
 */
Imat *imodMatNew(int dim)
{
  Imat *mat;
     
  mat = (Imat *)malloc(sizeof(Imat));
  if (!mat)
    return(NULL);
  mat->dim = dim;
  switch(dim){
  case 2:
    mat->data = (float *)malloc(9 * sizeof(float));
    mat->size = 9;
    break;
  case 3:
    mat->data = (float *)malloc(16 * sizeof(float));
    mat->size = 16;
    break;
  }
  if (!mat->data){
    free(mat);
    return(NULL);
  }
  imodMatId(mat);
  return(mat);
}            

/*!
 * Frees the matrix [mat] as well as its {data} member.
 */
void imodMatDelete(Imat *mat)
{
  if (mat->data)
    free(mat->data);
  if (mat)
    free(mat);
}

/*!
 *  Sets the matrix [mat] to the identity matrix.
 */
void imodMatId(Imat *mat)
{
  int i;

  for(i = 0; i < mat->size; i++)
    mat->data[i] = 0.0f;
  mat->data[0] = 1.0f;
  if (mat->dim == 2){
    mat->data[4] = 1.0f;
    mat->data[8] = 1.0f;
  }else{
    mat->data[5] = 1.0f;
    mat->data[10] = 1.0f;
    mat->data[15] = 1.0f;
  }
}

/*!
 * Prints matrix in [imat].
 */
void imodMatPrint(Imat *mat)
{
  int i;
  if (mat->dim == 2) {
    for (i = 0; i < 2; i++)
      printf("%13.6f %13.6f %13.3f\n", mat->data[i], mat->data[i + 3],
             mat->data[i + 6]);
  } else {
    for (i = 0; i < 3; i++)
      printf("%13.6f %13.6f %13.6f %13.3f\n", mat->data[i], mat->data[i + 4],
           mat->data[i + 8], mat->data[i + 12]);
  }
}

/*!
 * Copies matrix [fmat] to [tomat], provided they are the same dimension.
 */
void imodMatCopy(Imat *fmat, Imat *tomat)
{
  int i;
     
  if (fmat->dim != tomat->dim)
    return;
     
  for(i = 0; i < fmat->size; i++)
    tomat->data[i] = fmat->data[i];
  tomat->dim = fmat->dim;
  tomat->size = fmat->size;
}

/*!
 * Forms the matrix product [mat1] x [mat2] and places it into [matout]; i.e.,
 * the input arguments are the matrix applied first and the matrix applied 
 * second.
 */
void imodMatMult(Imat *mat2, Imat *mat1, Imat *matout)
{
  float *m1, *m2, *out;
  int n,i,j;
  if (mat2->dim != mat1->dim)
    return;
  m1 = mat1->data;
  m2 = mat2->data;
  out = matout->data;
  if (mat2->dim == 2){
    out[0] = (m1[0] * m2[0]) + (m1[3] * m2[1]) + (m1[6] * m2[2]);
    out[1] = (m1[1] * m2[0]) + (m1[4] * m2[1]) + (m1[7] * m2[2]);
    out[2] = (m1[2] * m2[0]) + (m1[5] * m2[1]) + (m1[8] * m2[2]);
    out[3] = (m1[0] * m2[3]) + (m1[3] * m2[4]) + (m1[6] * m2[5]);
    out[4] = (m1[1] * m2[3]) + (m1[4] * m2[4]) + (m1[7] * m2[5]);
    out[5] = (m1[2] * m2[3]) + (m1[5] * m2[4]) + (m1[8] * m2[5]);
    out[6] = (m1[0] * m2[6]) + (m1[3] * m2[7]) + (m1[6] * m2[8]);
    out[7] = (m1[1] * m2[6]) + (m1[4] * m2[7]) + (m1[7] * m2[8]);
    out[8] = (m1[2] * m2[6]) + (m1[5] * m2[7]) + (m1[8] * m2[8]);
    return;
  }

  for(n = 0, i = 0, j = 0; n < 16; n++, j++){
    out[n] = (m1[j]      * m2[i]  ) +
      (m1[j + 4]  * m2[i+1]) +
      (m1[j + 8]  * m2[i+2]) +
      (m1[j + 12] * m2[i+3]);
    if (j == 3){
      j = -1;
      i += 4;
    }
  }

}

/*!
 * Adds the translation in [pt] to the transformation in [mat].
 */
void imodMatTrans(Imat *mat, Ipoint *pt)
{
  if (mat->dim == 2){
    mat->data[6] += (float)pt->x;
    mat->data[7] += (float)pt->y;
    return;
  }
  mat->data[12] += pt->x;
  mat->data[13] += pt->y;
  mat->data[14] += pt->z;
}

/*!
 * Applies scaling by the factors in [pt] to the transformation in [mat].
 */
int imodMatScale(Imat *mat, Ipoint *pt)
{
  Imat *smat, *omat;

  smat = imodMatNew(mat->dim);
  if (!smat)
    return(-1);
  omat = imodMatNew(mat->dim);
  if (!omat)
    return(-1);

  if (mat->dim == 2){
    smat->data[0] = pt->x;
    smat->data[4] = pt->y;
  }else{
    smat->data[0] = pt->x;
    smat->data[5] = pt->y;
    smat->data[10] = pt->z;
  }

  imodMatMult(mat, smat, omat);
  imodMatCopy(omat, mat);
  imodMatDelete(omat);
  imodMatDelete(smat);
  return(0);
}

/*!
 * Applies rotation by [angle] in degrees around one axis to the transformation
 * in [mat].  For a 3D matrix, [axis] must be one of {b3dX}, {b3dY}, or {b3dZ};
 * for a 2D matrix [axis] is ignored.  Returns 1 for memory error.
 */
int imodMatRot(Imat *mat, double angle, int axis)
{
  Imat *rmat, *omat;
  double cosa, sina;
  angle *= 0.017453293;

  cosa = cos(angle);
  sina = sin(angle);

  rmat = imodMatNew(mat->dim);
  if (!rmat)
    return(-1);
  omat = imodMatNew(mat->dim);
  if (!omat)
    return(-1);
     
  if (mat->dim == 2){
    rmat->data[0] = (float)cosa;
    rmat->data[1] = (float)sina;
    rmat->data[3] = (float)-sina;
    rmat->data[4] = (float)cosa;
  }else{
    switch(axis){
    case b3dX:
      rmat->data[5] = (float)cosa;
      rmat->data[6] = (float)sina;
      rmat->data[9] = (float)-sina;
      rmat->data[10] = (float)cosa;
      break;
    case b3dY:
      rmat->data[0] = (float)cosa;
      rmat->data[2] = (float)-sina;
      rmat->data[8] = (float)sina;
      rmat->data[10] = (float)cosa;
      break;
    case b3dZ:
      rmat->data[0] = (float)cosa;
      rmat->data[1] = (float)sina;
      rmat->data[4] = (float)-sina;
      rmat->data[5] = (float)cosa;
      break;
    default:
      imodMatDelete(omat);
      imodMatDelete(rmat);
      return(-1);
    }


  }
  imodMatMult(mat, rmat, omat);
  imodMatCopy(omat, mat);
  imodMatDelete(omat);
  imodMatDelete(rmat);
  return(0);
}

/*!
 * Applies a rotation by [angle] (in degrees) about the vector [v] to the 
 * matrix in [mat].
 */
/* DNM: implemented correct equations */
int imodMatRotateVector(Imat *mat, double angle, Ipoint *v)
{
  Imat *rmat, *omat;
  double cosa, sina, omca;
  double x, y, z, aval;

  if (mat->dim == 2)
    return(-1);
     
  if ((v->x == 0.0) && (v->y == 0.0) && (v->z == 0.0))
    return(-1);

  angle *= 0.017453293;
  cosa = cos(angle);
  sina = sin(angle);
  omca = 1.0 - cosa;
     
  if (sina - sina) return 0;
     
  aval = (v->x * v->x) + (v->y * v->y) + (v->z * v->z);
  if (aval == 0.0) return 0;
  aval = sqrt(aval);
  x = v->x / aval;
  y = v->y / aval;
  z = v->z / aval;


  rmat = imodMatNew(mat->dim);
  if (!rmat)  return(-1);
  omat = imodMatNew(mat->dim);
  if (!omat)  return(-1);

  rmat->data[0] = x * x * omca + cosa;
  rmat->data[1] = y * x * omca + (sina * z);
  rmat->data[2] = z * x * omca - (sina * y);

  rmat->data[4] = x * y * omca - (sina * z);
  rmat->data[5] = y * y * omca + cosa;
  rmat->data[6] = z * y * omca + (sina * x);

  rmat->data[8] = x * z * omca + (sina * y);
  rmat->data[9] = y * z * omca - (sina * x);
  rmat->data[10] = z * z * omca + cosa;
  imodMatMult(mat, rmat, omat);
  imodMatCopy(omat, mat);
  imodMatDelete(omat);
  imodMatDelete(rmat);
  return(0);
}

/*!
 * Given a rotation matrix [mat], finds a single rotation axis described by
 * vector [v] and the amount of rotation [angle] about that axis, in degrees.
 */
int imodMatFindVector(Imat *mat, double *angle, Ipoint *v)
{
  double xsin, ysin, zsin, sina, cosa;

  xsin = 0.5 * (mat->data[6] - mat->data[9]);
  ysin = 0.5 * (mat->data[8] - mat->data[2]);
  zsin = 0.5 * (mat->data[1] - mat->data[4]);

  sina = sqrt(xsin * xsin + ysin * ysin + zsin * zsin);
  cosa = 0.5 * (mat->data[0] + mat->data[5] + mat->data[10] -1.0);
  *angle = atan2(sina, cosa);

  if (*angle < 1.e-8) {
    v->x = 1.0;
    v->y = 0.0;
    v->z = 0.0;
  } else if (sina < 1.e-8) {
    v->x = sqrt ((mat->data[0] - cosa)/(1.0 - cosa));
    v->y = sqrt ((mat->data[5] - cosa)/(1.0 - cosa));
    v->z = sqrt ((mat->data[10] - cosa)/(1.0 - cosa));
    if (mat->data[1] < 0)
      v->y *= -1.;
    if (mat->data[2] < 0)
      v->z *= -1.;
  } else {
    v->x = xsin / sina;
    v->y = ysin / sina;
    v->z = zsin / sina;
  }

  *angle /= 0.017453293;
  return 0;
}

/*!
 * Given a 3D rotation matrix in [mat], finds the angles of rotation about the
 * three axes, in the order Z, Y, X, and returns them in [x], [y], and [z].
 * Returns 1 if the determinant of the matrix is not near zero.  Angles are in
 * degrees.  Calls @@cfutils.html#matrixToAngles@ then @imodMatUniqueAngles to get a 
 * unique set of angles with restricted range.
 */
int imodMatGetNatAngles(Imat *mat, double *x, double *y, double *z)
{
  if (matrixToAngles(mat->data, x, y, z, 4))
    return 1;
  imodMatUniqueAngles(x, y, z);
  return 0;
}

/*!
 * Converts the three angles [x], [y], and [z] for rotations about the X, Y, 
 * and Z axes into a unique set of angles, with [x] between +/-90
 * and [y] and [z] between +/-180.  Angles are in degrees.  If [x] is out of
 * bounds, it inverts the sign of [z] and takes the complement of [y].
 */
void imodMatUniqueAngles(double *x, double *y, double *z)
{
  while (*x > 180.)
    *x -= 360.;
  while (*x <= -180.)
    *x += 360.;
  while (*y > 180.)
    *y -= 360.;
  while (*y <= -180.)
    *y += 360.;
  while (*z > 180.)
    *z -= 360.;
  while (*z <= -180.)
    *z += 360.;
  if (fabs(*x) > 90.) {
    *x += 180. * ((*x) > 0. ? -1. : 1);
    *y = ((*y) >= 0. ? 1. : -1.) * 180. - (*y);
    *z += 180. * ((*z) > 0. ? -1. : 1);
  }
}

/*!
 * Calls @imodMatUniqueAngles with the X, Y, and Z angles in the three
 * members of [pt].
 */
void imodMatUniqueRotationPt(Ipoint *pt)
{
  double x, y, z;
  x = pt->x;
  y = pt->y;
  z = pt->z;
  imodMatUniqueAngles(&x, &y, &z);
  pt->x = (float)x;
  pt->y = (float)y;
  pt->z = (float)z;
}

/*!
 * Applies the 2D transformation in matrix [mat] to the point [pt] and returns
 * the transformed position in [rpt].
 */
void imodMatTransform2D(Imat *mat, Ipoint *pt, Ipoint *rpt)
{
  rpt->x = (mat->data[0] * pt->x) + (mat->data[3] * pt->y) + mat->data[6];
  rpt->y = (mat->data[1] * pt->x) + (mat->data[4] * pt->y) + mat->data[7];
  rpt->z = pt->z;
}

/*!
 * Applies the 3D transformation in matrix [mat] to the point [pt] and returns
 * the transformed position in [rpt].
 */
void imodMatTransform3D(Imat *mat, Ipoint *pt, Ipoint *rpt)
{
  rpt->x = (mat->data[0] * pt->x) + (mat->data[4] * pt->y) +
    (mat->data[8] * pt->z) + mat->data[12];
  rpt->y = (mat->data[1] * pt->x) + (mat->data[5] * pt->y) +
    (mat->data[9] * pt->z) + mat->data[13];
  rpt->z = (mat->data[2] * pt->x) + (mat->data[6] * pt->y) +
    (mat->data[10] * pt->z) + mat->data[14];
}

/*!
 * Applies the transformation in matrix [mat] to the point [pt] and returns the
 * transformed position in [rpt].  [mat] can be 2D or 3D.
 */
void imodMatTransform(Imat *mat, Ipoint *pt, Ipoint *rpt)
{

  if (mat->dim == 2){
    imodMatTransform2D(mat, pt, rpt);
    return;
  }
  imodMatTransform3D(mat, pt, rpt);
}

/*!
 * Returns the inverse of the matrix [mat], or NULL for memory error.
 */
Imat *imodMatInverse(Imat *mat)
{
  Imat *imat;
  float mval;
  float *mdata = mat->data;
  float *idata;
  int i;

  imat = imodMatNew(mat->dim);
  if (!imat)
    return(NULL);

  idata = imat->data;
  if (mat->dim == 2){
    mval = mdata[8] * ( (mdata[0] * mdata[4]) - (mdata[1] * mdata[3]) );
      
    idata[0] =  mdata[4] * mdata[8]; 
    idata[1] = -mdata[1] * mdata[8];
    idata[2] =  0.0;
    idata[3] = -mdata[3] * mdata[8];
    idata[4] =  mdata[0] * mdata[8];
    idata[5] =  0.0;
    idata[6] = (mdata[3] * mdata[7]) - (mdata[4] * mdata[6]);
    idata[7] = (mdata[1] * mdata[6]) - (mdata[0] * mdata[7]);
    idata[8] = (mdata[0] * mdata[4]) - (mdata[1] * mdata[3]);
      
    for (i = 0; i < 9; i++)
      idata[i] /= mval;

  } else {

    /* 3-D inverse */
    mval = mdata[0]*mdata[5]*mdata[10]+mdata[1]*mdata[6]*mdata[8]+
      mdata[4]*mdata[9]*mdata[2]-mdata[8]*mdata[5]*mdata[2]-mdata[1]*
      mdata[4]*mdata[10]-mdata[0]*mdata[6]*mdata[9];
       
    idata[0] = (mdata[5]*mdata[10]-mdata[6]*mdata[9])/mval;
    idata[1] = (mdata[2]*mdata[9]-mdata[1]*mdata[10])/mval;
    idata[2] = (mdata[1]*mdata[6]-mdata[2]*mdata[5])/mval;
    idata[4] = (mdata[6]*mdata[8]-mdata[4]*mdata[10])/mval;
    idata[5] = (mdata[0]*mdata[10]-mdata[2]*mdata[8])/mval;
    idata[6] = (mdata[2]*mdata[4]-mdata[6]*mdata[0])/mval;
    idata[8] = (mdata[4]*mdata[9]-mdata[5]*mdata[8])/mval;
    idata[9] = (mdata[1]*mdata[8]-mdata[0]*mdata[9])/mval;
    idata[10] = (mdata[5]*mdata[0]-mdata[1]*mdata[4])/mval;
    idata[12] = -(idata[0]*mdata[12]+idata[4]*mdata[13]+idata[8]*mdata[14]);
    idata[13] = -(idata[1]*mdata[12]+idata[5]*mdata[13]+idata[9]*mdata[14]);
    idata[14] = -(idata[2]*mdata[12]+idata[6]*mdata[13]+idata[10]*mdata[14]);
  }
  return(imat);
}

/* 11/17/05: removed a translation of amat_to_rotmagstr.. This is in midas. */
