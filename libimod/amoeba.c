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
*/
/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

#include <math.h>
#define NMAX  20
void amoeba(float *p, float *y, int mp, int np, int ndim, float ftol, 
            float (*funk)(float *), int *iterP, float *ptol, int *iloP)
{
  float alpha = 1.0,beta = 0.5,gamma = 2.0;
  float pr[NMAX], prr[NMAX], pbar[NMAX];
  int iter, ilo, mpts, ihi, inhi, i, ifnear, j;
  float ypr, yprr, rtol;
  
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

	rtol = 2.*fabs((double)(y[ihi] - y[ilo]))/
      (fabs((double)y[ihi])+ fabs((double)y[ilo]));
	if (rtol < ftol)
      break;

	iter++;
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
	  pr[j] = (1.+alpha)*pbar[j] - alpha*p[ihi + j * mp];
    }
	ypr = funk(pr);
    if (ypr <= y[ilo]) {
	  for (j = 0; j < ndim; j++)
	    prr[j] = gamma*pr[j]+(1. - gamma)*pbar[j];
	  yprr = funk(prr);
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
	    prr[j] = beta*p[ihi + j * mp]+(1. - beta)*pbar[j];
	  yprr = funk(prr);
      if (yprr < y[ihi]) {
	    for (j = 0; j < ndim; j++)
	      p[ihi + j * mp] = prr[j];
	    y[ihi] = yprr;
      } else {
	    for (i = 0; i < mpts; i++) {
	      if (i != ilo) {
            for (j = 0; j < ndim; j++) {
              pr[j] = 0.5*(p[i + j * mp]+p[ilo + j * mp]);
              p[i + j * mp] = pr[j];
            }
            y[i] = funk(pr);
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

void amoeba_(float *p, float *y, int *mp, int *np, int *ndim, float *ftol, 
            float (*funk)(float *), int *iter, float *ptol, int *ilo)
{
  int iloc;
  amoeba(p, y, *mp, *np, *ndim, *ftol, funk, iter, ptol, &iloc);
  *ilo = iloc + 1;
}
