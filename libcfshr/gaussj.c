/*
 * gaussj.c: Solves linear equations by Gauss-Jordan elimination
 *
 * originally from Cooley and Lohnes - Multivariate Procedures for the
 * Behavioral Sciences, Wiley, 1962 (!).
 * 12/25/90: converted to fortran 77, indented, put in message
 * for singularity, verified basically same program as GAUSSJ from
 * Press et al 1986 for Gauss-Jordan elimination, changed arguments,
 * removed determinant calculation and augmented treatment of B
 * 10/29/99: translated to C.  Failed to keep Fortran ordering of input data.
 *
 * $Id$
 */

#include "imodconfig.h"
#include "b3dutil.h"

#ifdef F77FUNCAP
#define gaussjfw GAUSSJ
#define gaussjdet GAUSSJDET
#else
#define gaussjfw gaussj_
#define gaussjdet gaussjdet_
#endif

#define MSIZ 2000
/*!
 * Solves the linear matrix equation A X = B by Gauss-Jordan elimination.  
 * A is a square matrix of size [n] by [n] in array [a], dimensioned to [np] 
 * columns.  B is a matrix with one row per row of A and [m] columns in array
 * [b], dimensioned to [mp] columns.  The columns of [b] are replaced by
 * the [m] solution vectors while [a] is reduced to a unit matrix.  It is 
 * called the same from C and Fortran, but the matrices must be in row-major 
 * order in both cases.  Specifically, in C, the matrices are indexed as 
 * A[row][column] and in Fortran they are indexed as A(column,row). ^
 * The maximum value of [n] is 2000 but better and faster approaches should be 
 * used long before reaching that point.  The routine returns -1 if [n] exceeds
 * this value and 1 if the A matrix is singular.
 */
int gaussj(float *a, int n, int np, float *b, int m, int mp)
{
  float determ;
  return gaussjDet(a, n, np, b, m, mp, &determ);
}

/*!
 * Version of @gaussj that returns a determinant value
 */
int gaussjDet(float *a, int n, int np, float *b, int m, int mp, float *determ)
{
  short int index[MSIZ][2];
  float pivot[MSIZ];
  short int ipivot[MSIZ];
  int irow, icolum, i, j, k, l, l1;
  float amax, t, abstmp, pivotmp;
  
  *determ = 1.;
  if (n > MSIZ)
    return -1;
  for (j = 0; j < n; j++)
    ipivot[j]=0;
  for (i = 0; i < n; i++) {
    amax=0.;
    for (j = 0; j < n; j++) {
      if(ipivot[j] != 1) {

        for (k = 0; k < n; k++) {
          if(ipivot[k] == 0) {
            abstmp = a[j*np+k];
            if (abstmp < 0)
              abstmp = -abstmp;
            if(amax < abstmp){
              irow=j;
              icolum=k;
              amax=abstmp;
            }

          } else if(ipivot[k] > 1) {
            /* write(*,*) 'Singular matrix' */
            return 1;
          }
        }
      }
    }
    ipivot[icolum]=ipivot[icolum]+1;
    if(irow != icolum) {
      *determ = -(*determ);
      for (l = 0; l < n; l++) {
        t=a[irow*np+l];
        a[irow*np+l]=a[icolum*np+l];
        a[icolum*np+l]=t;
      }
      for (l = 0; l < m; l++) {
        t=b[irow*mp+l];
        b[irow*mp+l]=b[icolum*mp+l];
        b[icolum*mp+l]=t;
      }
    }
    index[i][0]=irow;
    index[i][1]=icolum;
    pivotmp=a[icolum*np+icolum];
    /*    if(abs(pivotmp) < 1.e-30) write(*,*) 'small pivot',pivotmp */
    pivot[i]=pivotmp;
    *determ *= pivotmp;
    a[icolum*np+icolum]=1.;
    /*      worried about that step! */
    for (l = 0; l < n; l++) 
      a[icolum*np+l]=a[icolum*np+l]/pivotmp;
    for (l = 0; l < m; l++)
      b[icolum*mp+l]=b[icolum*mp+l]/pivotmp;
    for (l1 = 0; l1 < n; l1++) {
      t=a[l1*np+icolum];
      if (t != 0. && l1 != icolum) {
        a[l1*np+icolum]=0.;
        for (l = 0; l < n; l++) 
          a[l1*np+l]=a[l1*np+l]-a[icolum*np+l]*t;
        for (l = 0; l < m; l++)
          b[l1*mp+l]=b[l1*mp+l]-b[icolum*mp+l]*t;
      }
    }
  }
  for (i = 0; i < n; i++) {
    l=n-1-i;
    if(index[l][0] != index[l][1]) {
      irow=index[l][0];
      icolum=index[l][1];
      for (k = 0; k < n; k++) {
        t=a[k*np+irow];
        a[k*np+irow]=a[k*np+icolum];
        a[k*np+icolum]=t;
      }
    }
  }
  return 0;
}

int gaussjfw(float *a, int *n, int *np, float *b, int *m, int *mp)
{
  return gaussj(a, *n, *np, b, *m, *mp);
}

int gaussjdet(float *a, int *n, int *np, float *b, int *m, int *mp, float *determ)
{
  return gaussjDet(a, *n, *np, b, *m, *mp, determ);
}
