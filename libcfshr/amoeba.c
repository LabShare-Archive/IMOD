/* amoeba.c: simplex algorithm for minimization of arbitrary function
   Although the approach and method of application are based on
   Press et al, Numerical Recipes in Fortran, this routine was rewritten from 
   scratch following the description of the Nelder-Mead algorithm in 
   Lagarius et al., SIAM J. Optim. Vol. 9, No. 1, pp. 112-147.

   See earlier versions for summary of validation of earlier translation from
   fortran to C.

 * $Id$
*/

#include <math.h>
#include "imodconfig.h"

#ifdef F77FUNCAP
#define amoebafwrap AMOEBA
#define amoebainitfwrap AMOEBAINIT
#else
#define amoebafwrap amoeba_
#define amoebainitfwrap amoebainit_
#endif

#define NMAX  20

/* Reorder the data or a subset of it */
static void simpleSort(float *y, int *index, int npts) 
{
  int ipt, jpt, itmp;
  for (ipt = 0; ipt < npts - 1; ipt++) {
    for (jpt = ipt + 1; jpt < npts; jpt++) {
      if (y[index[ipt]] > y[index[jpt]]) {
        itmp = index[ipt];
        index[ipt] = index[jpt];
        index[jpt] = itmp;
      }
    }
  }
}

/* Insert a new point in the simplex following the ordering rule of Lagarias 
   This business of sorting and maintaining order is silly for a few numbers 
   and the primary motivation was to produce something different from Press's
   code for finding the lowest, highest, and second highest at each step. */
static void acceptPoint(float *p, int mp, int ndim, int *index, float *y,
                        float *pnew, float ynew)
{
  int ipt, jpt, ind, idim;

  /* Find first point that is greater than the new one and insert at that 
     position.  Here is the deal: Lagarius's ordering rule says to insert at
     the position of the LAST point that is greater than the new one but this 
     is just plain wrong.  The minimal change from their formula is to use the
     in instead of the max index value, which means insert at position of the 
     FIRST point that is greater than the new one, at the end of any tied 
     ones. */
  for (ipt = 0; ipt < ndim; ipt++)
    if (ynew < y[index[ipt]])
      break;

  /* Insert the data at the position of former worst point, being discarded */
  ind = index[ndim];
  for (idim = 0; idim < ndim; idim++)
    p[ind + idim * mp] = pnew[idim];
  y[ind] = ynew;

  /* Roll the index values up then insert the new index value*/
  for (jpt = ndim; jpt > ipt; jpt--)
    index[jpt] = index[jpt - 1];
  index[ipt] = ind;
}

/*!
 * Performs a multidimensional search to minimize a function of [ndim] 
 * variables using the Nelder-Mead algorithm as elaborated by Lagarius et al.
 * The function value is computed by [funk],
 * a void function of two variables: a float array containing the values of
 * the [ndim] variables, and a pointer to a float for the function value.
 * [p] is a 2-dimensional array dimensioned to at least one more than the 
 * number of variables in each dimension, and [mp] specifies its fastest 
 * dimension (second in C, first in fortran).  [y] is a one-dimensional array 
 * also dimensioned to at least one more than the number of variables.
 * Both of these should be preloaded by calling amoebaInit.  
 * Termination is controlled by [ftol], which is a limit for the fractional 
 * difference in function value between the lowest and highest point in the 
 * simplex, and the array [ptol], which has limits for the difference of each 
 * variable; each component of each point must be within the respective limit 
 * of the value for the lowest point.  [iterP] is returned with 
 * the number of iterations; [iloP] is returned with the index of the minimum
 * vector in [p] (p\[i\]\[iloP\] in C, p(iloP, i) in fortran).
 * ^ From fortran the subroutine is called as:
 * ^ call amoeba(p, y, mp, ndim, ftol, funk, iter, ptol, ilo)
 * ^ where [funk] is: 
 * ^ subroutine funk(a, value)
 */
void amoeba(float *p, float *y, int mp, int ndim, float ftol, 
            void (*funk)(float *, float *), int *iterP, float *ptol, int *iloP)
{
  /* Reflection, contraction, expansion, and shrinkage coefficients */
  float rho = 1.0, gamma = 0.5, chi = 2.0, sigma = 0.5;
  int index[NMAX];
  float pcen[NMAX], pref[NMAX], pexp[NMAX], yexp, yref;
  int iterMax = 1000;
  int ipt, idim, near, shrink, ind, ilow, ihigh, isecond, iter, indsort;
  int npts = ndim + 1;

  /* Make an index to sorted data */
  for (ipt = 0; ipt < npts; ipt++)
    index[ipt] = ipt;
  simpleSort(y, index, npts);
  
  for (iter = 0; iter < iterMax; iter++) {

    /* Assign indices of lowest, highest, second highest */
    ilow = index[0];
    ihigh = index[npts-1];
    isecond = index[npts-2];

    /* check if each point is within certain distance of lowest */
    near = 1;
    for (ipt = 1; ipt < npts && near; ipt++) {
      for (idim = 0; idim < ndim; idim++) {
        ind = index[ipt];
        if (fabs((double)(p[ind + idim*mp] - p[ilow + idim*mp])) >= ptol[idim]) {
          near = 0;
          break;
        }
      }
    }
    if (near)
      break;
            
    /* Check if overall range of values is small relative to value */
    if (y[ihigh] - y[ilow] <= 0.5 * (fabs((double)y[ihigh]) + 
                                     fabs((double)y[ilow])) * ftol)
      break;
                                         
    /* Get centroid of all but highest point and compute reflection */
    for (idim = 0; idim < ndim; idim++) {
      pcen[idim] = 0.;
      for (ipt = 0; ipt < npts - 1; ipt++)
        pcen[idim] += p[index[ipt] + idim*mp];
      pcen[idim] /= ndim;
      pref[idim] = (1. + rho) * pcen[idim] - rho * p[ihigh + idim*mp];
    }
    funk(pref, &yref);
    
    /* If reflected point is better than second highest but not best, accept */
    if (yref >= y[ilow] && yref < y[isecond]) {
      acceptPoint(p, mp, ndim, index, y, pref, yref);

      /* Or, if reflected point is very best, do an expansion */
    } else if (yref < y[ilow]) {
      for (idim = 0; idim < ndim; idim++)
        pexp[idim] = (1. - chi) * pcen[idim] + chi * pref[idim];
      funk(pexp, &yexp);
      
      /* Take the better of the two points (greedy minimization) */
      if (yexp < yref)
        acceptPoint(p, mp, ndim, index, y, pexp, yexp);
      else
        acceptPoint(p, mp, ndim, index, y, pref, yref);

      /* Or, if reflection is no better than second highest, contract */
    } else {
      shrink = 0;

      /* Outside contraction */
      if (yref <= y[ihigh]) {
        for (idim = 0; idim < ndim; idim++)
          pexp[idim] = (1. + rho * gamma) * pcen[idim] - 
            rho * gamma * p[ihigh + idim*mp];
        funk(pexp, &yexp);
        if (yexp <= yref)
          acceptPoint(p, mp, ndim, index, y, pexp, yexp);
        else
          shrink = 1;

        /* Inside contraction */
      } else {
        for (idim = 0; idim < ndim; idim++)
          pexp[idim] = (1. - gamma) * pcen[idim] + gamma * p[ihigh + idim*mp];
        funk(pexp, &yexp);
        if (yexp < y[ihigh])
          acceptPoint(p, mp, ndim, index, y, pexp, yexp);
        else
          shrink = 1;
      }

      /* Shrink and evaluate, then reorder all the new points*/
      if (shrink) {
        indsort = 1;
        for (ipt = 1; ipt < npts; ipt++) {
          ind = index[ipt];
          for (idim = 0; idim < ndim; idim++) {
            pexp[idim] = (1. - sigma) * p[ilow + idim*mp] +
              sigma * p[ind + idim*mp];
            p[ind + idim*mp] = pexp[idim];
          }
          funk(pexp, &y[ind]);
          if (y[ind] < y[ilow])
            indsort = 0;
        }
        simpleSort(y, &index[indsort], npts - indsort);
      }
    }
  }

  *iloP = ilow;
  *iterP = iter;
}

/*!
 * Initializes the arrays [p], [y], and [ptol] before calling amoeba.
 * [a] is an array with the initial values of the variable.  
 * [da] is an array with
 * factors proportional to the magnitude of the components of [a].
 * The initial step size for each variable is set to [delfac] times [da],
 * while the termination tolerance for each variable is set to [ptolFac] times
 * [da].  Other variables are as just described.  From fortran it is called as:
 * ^ call amoebaInit(p, y, mp, ndim, delfac, ptolFac, a, da, func, ptol)
 */
void amoebaInit(float *p, float *y, int mp, int ndim, float delfac, 
                float ptolFac, float *a, float *da, 
                void (*funk)(float *, float *), float *ptol)
{
  int i, j;
  float ptmp[NMAX];

  for (j = 0; j < ndim + 1; j++) {
    for (i = 0; i < ndim; i++) {
      p[j + i * mp] = a[i];
      if (j && i == j - 1)
        p[j + i * mp] = a[i] + delfac * da[i];
      ptmp[i] = p[j + i * mp];
      ptol[i] = da[i] * ptolFac;
    }
    funk(ptmp, &y[j]);
  }
}

void amoebafwrap(float *p, float *y, int *mp, int *ndim, float *ftol, 
                 void (*funk)(float *, float *), int *iter, float *ptol, 
                 int *ilo)
{
  int iloc;
  amoeba(p, y, *mp, *ndim, *ftol, funk, iter, ptol, &iloc);
  *ilo = iloc + 1;
}

void amoebainitfwrap(float *p, float *y, int *mp, int *ndim, float *delfac, 
                     float *ptolFac, float *a, float *da, 
                     void (*funk)(float *, float *), float *ptol)
{
  amoebaInit(p, y, *mp, *ndim, *delfac, *ptolFac, a, da, funk, ptol);
}
