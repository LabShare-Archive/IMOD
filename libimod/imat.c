/*  IMOD VERSION 2.20
 *
 *  imat.c : a quick and dirty way to get transforms on none gl machines.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1998 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <math.h>
#include "imodel.h"

/*
 * Create a new matrix structure.  Input dimension can be 2 or 3.
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

void imodMatDelete(Imat *mat)
{
     if (mat->data)
	  free(mat->data);
     if (mat)
	  free(mat);
}

/*
 *  Load identidy matrix.
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

/* Applies a rotation by "angle" about the vector "v" to the matrix in "mat" */
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

/* Given a rotation matrix "mat", finds a single rotation axis described by
   vector "v" and the amount of roattion "angle" about that axis  (DNM) */

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

/* DNM: a translation of fortran code in icalc_angles.f */

int imodMatGetNatAngles(Imat *mat, double *x, double *y, double *z)
{
     double r11, r12, r13, r21, r22, r23, r31, r32, r33;
     double crit = 0.01;
     double cnv = 0.017453292;
     double small = 0.0000001;
     double det, alpha, beta, gamma, cosg, sing, cosb, test1, test2;

     r11 = mat->data[0];
     r12 = mat->data[4];
     r13 = mat->data[8];
     r21 = mat->data[1];
     r22 = mat->data[5];
     r23 = mat->data[9];
     r31 = mat->data[2];
     r32 = mat->data[6];
     r33 = mat->data[10];

     /* first check matrix */
     det = r11*r22*r33 - r11*r23*r32 + r12*r23*r31 - r12*r21*r33
       + r13*r21*r32 - r13*r22*r31;

     det -= 1.0;
     if (det > crit || det < -crit)
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
     *x = alpha/cnv;
     *y = beta/cnv;
     *z = gamma/cnv;
     return 0;
}

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

void imodMatTransform2D(Imat *mat, Ipoint *pt, Ipoint *rpt)
{
     rpt->x = (mat->data[0] * pt->x) + (mat->data[3] * pt->y) + mat->data[6];
     rpt->y = (mat->data[1] * pt->x) + (mat->data[4] * pt->y) + mat->data[7];
     rpt->z = pt->z;
}

void imodMatTransform3D(Imat *mat, Ipoint *pt, Ipoint *rpt)
{
     rpt->x = (mat->data[0] * pt->x) + (mat->data[4] * pt->y) +
	  (mat->data[8] * pt->z) + mat->data[12];
     rpt->y = (mat->data[1] * pt->x) + (mat->data[5] * pt->y) +
	  (mat->data[9] * pt->z) + mat->data[13];
     rpt->z = (mat->data[2] * pt->x) + (mat->data[6] * pt->y) +
	  (mat->data[10] * pt->z) + mat->data[14];
}

void imodMatTransform(Imat *mat, Ipoint *pt, Ipoint *rpt)
{

     if (mat->dim == 2){
	  imodMatTransform2D(mat, pt, rpt);
	  return;
     }
     imodMatTransform3D(mat, pt, rpt);
}

Imat *imodMatInverse(Imat *mat)
{
     Imat *imat;
     float mval;
     int i;

     imat = imodMatNew(mat->dim);
     if (!imat)
	  return(NULL);

     if (mat->dim == 2){
	  mval = mat->data[8] * ( (mat->data[0] * mat->data[4]) - 
				 (mat->data[1] * mat->data[3]) );
	  
	  imat->data[0] =  mat->data[4] * mat->data[8]; 
	  imat->data[1] = -mat->data[1] * mat->data[8];
	  imat->data[2] =  0.0;
	  imat->data[3] = -mat->data[3] * mat->data[8];
	  imat->data[4] =  mat->data[0] * mat->data[8];
	  imat->data[5] =  0.0;
	  imat->data[6] = (mat->data[3] * mat->data[7]) - 
	       (mat->data[4] * mat->data[6]);
	  imat->data[7] = (mat->data[1] * mat->data[6]) - 
	       (mat->data[0] * mat->data[7]);
	  imat->data[8] = (mat->data[0] * mat->data[4]) - 
	       (mat->data[1] * mat->data[3]);
	  
	  for(i = 0; i < 9; i++)
	       imat->data[i] /= mval;
     }else{
	  /* 3-D inverse not working yet. */
	  imodMatDelete(imat);
	  return(NULL);
     }
     return(imat);
}

#ifdef imodMatRotMagStr_done
/* 	  AMAT_TO_ROTMAGSTR converts a 2 by 2 transformation matrix AMAT into */
/* 	  four "natural" parameters of image transformation: THETA is overall */
/* 	  rotation, SMAG is overall magnification, STR is a unidirectional */
/* 	  stretch, and PHI is the angle of the stretch axis. */
/* 	  Two equivalent solutions are possible, with the stretch axis in the */
/* 	  first or fourth quadrant.  The program returns the solution that */
/* 	  makes the magnification SMAG nearer to 1.0. */

/* 	  To solve for the variables, the first step is to solve for THETA by */
/* 	  taking the arctangent of a function of the AMAT values.  It is then */
/* 	  possible to compute F1, F2 and F3, intermediate factors whose */
/* 	  equations are listed in ROTMAGSTR_TO_AMAT below. The equations for */
/* 	  F1, F2 and F3 can then be solved explicitly for the square of the */
/* 	  cosine of PHI.  There are two solutions, which are both valid, but */
/* 	  in different quadrants.  The sign of F2, and whether one of the */
/* 	  formulas for STR would yield a value > or < 1, then determines which */
/* 	  solution is valid for the first quadrant.  STR is computed from */
/* 	  one of two different formulas, depending on whether PHI is near 45 */
/* 	  degrees or not, then SMAG is computed. */

/* 	  David Mastronarde 12/29/88, vastly improved 2/5/92 */


void imodMatRotMagStr(Imat *mat, 
		      float *theta, float *smag, float *str, float *phi)
{
  
  /* System generated locals */
  float r__1, r__2;
  
  /* Local variables */
  static float fden;
  static float fnum, cosphisq, sinphisq;
  static float costh, sinth, f1, f2, f3;
  static float a11, a12, a21, a22, factmp, dentmp, afac, bfac;

  a11 = mat->data[0];
  a12 = mat->data[3];
  a21 = mat->data[1];
  a22 = mat->data[4];

  /*   first find the rotation angle theta that gives the same solution for
   *   f2 when derived from a11 and a21 as when derived from a12 and a22
   */
  *theta = (float)0.;
  if (a21 != a12 || a22 != -a11) {
    r__1 = a21 - a12;
    r__2 = a22 + a11;
    *theta = atan2(&r__1, &r__2);
  }
  
  costh = cos(theta);
  sinth = sin(theta);
  f1 = a11 * costh + a21 * sinth;
  f2 = a21 * costh - a11 * sinth;
  f3 = a22 * costh - a12 * sinth;

  /* 	  Next solve for phi */

  if (fabs(f2) < (float)1e-10) {
    /* 	    if f2=0, pick phi=0., set cos phi to 1. */
    cosphisq = (float)1.;
  } else {
    
    /* 	    otherwise, solve quadratic equation, pick the solution that is
     * 	    right for the first quadrant 
     */
    
    /* Computing 2nd power */
    r__1 = f3 - f1;
    afac = r__1 * r__1;
    /* Computing 2nd power */
    r__1 = f2;
    bfac = r__1 * r__1 * (float)4.;
    cosphisq = (sqrt((float)1. - bfac / 
		     (bfac + afac)) + (float)1.) * (float).5;
    sinphisq = (float)1. - cosphisq;
    fnum = (r__1 = f1 * cosphisq - f3 * sinphisq, fabs(r__1));
    fden = (r__1 = f3 * cosphisq - f1 * sinphisq, fabs(r__1));
    if (f2 > (float)0. && fnum < fden || f2 < (float)0. && fnum > fden) {
      cosphisq = (float)1. - cosphisq;
    }
  }
  r__1 = sqrt(cosphisq);
  *phi = acos(&r__1);
  sinphisq = (float)1. - cosphisq;
  
  /* 	    solve for str. */
  
  if ((r__1 = cosphisq - (float).5, dabs(r__1)) > (float).25) {
    
    /* for angles far from 45 deg, use an equation that is good at 0 
     * or 90 deg but blows up at 45 deg. 
     */
    
    *str = (f1 * cosphisq - f3 * sinphisq) / (f3 * cosphisq - f1 * 
					      sinphisq);
    /* 	    write(*,'(a,f12.7)')' outer',str */
  } else {

    /* 	    for angles near 45 deg, use an equation that is good there but
     * 	    blows up at 0. 
     */

    factmp = (f1 + f3) * sqrt(cosphisq * sinphisq);
    *str = (factmp + f2) / (factmp - f2);
    /* 	    write(*,'(a,f12.7)')' inner',str */
  }
  
  /* 	  solve for smag from the equation for f1, or f2 if that would fail */
  /* 	  (which it does with stretch -1 along 45 degree line) */
  dentmp = *str * cosphisq + sinphisq;
  if (dabs(dentmp) > (float)1e-5) {
    *smag = f1 / dentmp;
  } else {
    *smag = (float)1. / ((*str - (float)1.) * sqrt(cosphisq * sinphisq));
  }
  
  /* if it will make smag closer to 1.0, flip stretch axis 90 deg */
  if ((r__1 = *smag - (float)1., fabs(r__1)) > 
      (r__2 = *str * *smag - (float)1., fabs(r__2))) {
    *smag *= *str;
    *str = (float)1. / *str;
    *phi += (float)-90.;
  }
  
  /* 	write(*,'(4f12.7)')str,phi,smag,theta */
  return 0;
} /* amat_to_rotmagstr__ */




#endif
