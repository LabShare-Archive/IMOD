/* ameoba.c: simplex algorithm for minimization of arbitrary function
   from Press et al, Numerical Recipes, P.292-293, then translated from
   amoeba.f 
   In fortran P is dimensioned (MP,NP).  To allow cross-calling, define
   p in C as p[np][mp], convert P(I,J) to p[j][i] and address here as
   p[i + j * mp]
   
   Tested this against fortran version with xfalign -red 0 -bi on uni3a.preali
   max difference in transforms:
   0.0003    0.0003    0.0003    0.0002    0.0300    0.0270
   difference in normalized difference measures is typically < 1.5e-6, up to 
   3e-4 on 4 of 76.
   Tested on uni3b.preali, max transform difference:
   0.0002    0.0005    0.0013    0.0004    0.0080    0.0450
   difference in measure typically < 1.5-e6, up to 4e-4 for 6 of 76.
   
   Also tested on 3 sets with solvematch and single layer of patches
   Coefficients differed by 0.000027 at most.

   Removed print statement on reaching "maximum iterations"; nothing is or was
   being done about limiting the iterations.

 * $Id$
 * Log at end of file
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

/*!
 * Performs a multidimensional search to minimize a function of [ndim] 
 * variables.  The function value is computed by * [funk],
 * a void function of two variables: a float array containing the values of
 * the [ndim] variables, and a pointer to a float for the function value.
 * [p] is a 2-dimensional array dimensioned to at least one more than the 
 * number of variables in each dimension, and [mp] specifies its fastest 
 * dimension (second in C, first in fortran).  [y] is a one-dimensional array 
 * also dimensioned to at least one more than the number of variables.
 * Both of these should be preloaded by calling amoebaInit.  
 * Termination is controlled by [ftol], which is an 
 * limit for the fractional change in function value, and the array [ptol],
 * which has limits for the change of each variable.  [iterP] is returned with 
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
  float alpha = 1.0,beta = 0.5,gamma = 2.0;
  float pr[NMAX], prr[NMAX], pbar[NMAX];
  int iter, ilo, mpts, ihi, inhi, i, ifnear, j;
  float ypr, yprr, rtol;
  int iterMax = 1000;
  
  mpts = ndim + 1;
  iter = 0;
  for (;;) {
   	ilo = 0;
	if (y[0] > y[1]) {
	  ihi = 0;
	  inhi = 1;
    } else {
	  ihi = 1;
	  inhi = 0;
    }
	for (i = 0; i < mpts; i++) {
	  if (y[i] < y[ilo])
        ilo = i;
      if (y[i] > y[ihi]) {
        inhi = ihi;
        ihi = i;
	  } else if (y[i] > y[inhi]) {
	    if (i != ihi) 
          inhi = i;
	  }
    }
	  
    /* check if each point is within certain distance of lowest */
	ifnear = 1;
    for (i = 0; i < mpts; i++) {
      for (j = 0; j < ndim; j++) {
	    if (fabs((double)(p[i + j * mp] - p[ilo + j * mp])) >= ptol[j]) {
          ifnear = 0;
          break;
        }
      }
    }
    if (ifnear)
      break;

    /* DNM 6/17/06: protect against hard zeros by testing for equality first */
    if (y[ihi] == y[ilo])
      break;
	rtol = (float)(2.*fabs((double)(y[ihi] - y[ilo]))/
      (fabs((double)y[ihi])+ fabs((double)y[ilo])));
	if (rtol < ftol)
      break;

	iter++;
    if (iter > iterMax)
      break;

    for (j = 0; j < ndim; j++)
	  pbar[j] = 0.;
    for (i = 0; i < mpts; i++) {
	  if (i != ihi) {
	    for (j = 0; j < ndim; j++)
	      pbar[j] = pbar[j]+p[i + j * mp];
	  }
    }
    for (j = 0; j < ndim; j++) {
	  pbar[j] = pbar[j]/ndim;
	  pr[j] = (1.f+alpha)*pbar[j] - alpha*p[ihi + j * mp];
    }
	funk(pr, &ypr);
    if (ypr <= y[ilo]) {
	  for (j = 0; j < ndim; j++)
	    prr[j] = gamma*pr[j]+(1.f - gamma)*pbar[j];
	  funk(prr, &yprr);
	  if (yprr < y[ilo]) {
	    for (j = 0; j < ndim; j++)
	      p[ihi + j * mp] = prr[j];
	    y[ihi] = yprr;
	  } else {
	    for (j = 0; j < ndim; j++)
	      p[ihi + j * mp] = pr[j];
	    y[ihi] = ypr;
	  }
    } else if (ypr >= y[inhi]) {
      if (ypr < y[ihi]) {
	    for (j = 0; j < ndim; j++)
	      p[ihi + j * mp] = pr[j];
	    y[ihi] = ypr;
	  }
      for (j = 0; j < ndim; j++)
	    prr[j] = beta*p[ihi + j * mp]+(1.f - beta)*pbar[j];
	  funk(prr, &yprr);
      if (yprr < y[ihi]) {
	    for (j = 0; j < ndim; j++)
	      p[ihi + j * mp] = prr[j];
	    y[ihi] = yprr;
      } else {
        for (i = 0; i < mpts; i++) {
	      if (i != ilo) {
            for (j = 0; j < ndim; j++) {
              pr[j] = 0.5f*(p[i + j * mp]+p[ilo + j * mp]);
              p[i + j * mp] = pr[j];
            }
            funk(pr, &y[i]);
	      }
        }
	  }
	} else {
	  for (j = 0; j < ndim; j++)
	    p[ihi + j * mp] = pr[j];
	  y[ihi] = ypr;
	}
  }

  *iloP = ilo;
  *iterP = iter;
  return;
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

/*

$Log$
Revision 1.2  2007/09/20 15:42:32  mast
Documentation fixes

Revision 1.1  2007/09/20 02:43:08  mast
Moved to new library

Revision 3.6  2006/06/18 19:36:46  mast
Changed to take a function that returns the value as an argument because
it is more portable than the return value (float failed on 64-bit)
Also modified fortran wrapper to eliminate useless argument

Revision 3.5  2006/06/08 05:13:05  mast
Added defines on init function fwrap

Revision 3.4  2006/06/08 03:12:02  mast
Added termination on 1000 iterations; probably not needed.

Revision 3.3  2006/06/06 16:22:15  mast
Added initialization func and documentation

  
Revision 3.1  2006/06/05 16:29:38  mast
Added to libimod

*/

