/*
 * statfuncs.c  - Statistical functions, some from elsewhere
 *
 * Copyright (C) 2009 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 * Log at end of file
 */

#include <math.h>
#include "imodconfig.h"
#include "cfsemshare.h"

#ifdef F77FUNCAP
#define incompbeta INCOMPBETA
#define errfunc ERRFUNC
#define dtvalue DTVALUE
#define dfvalue DFVALUE
#else
#define incompbeta incompbeta_
#define errfunc errfunc_
#define dtvalue dtvalue_
#define dfvalue dfvalue_
#endif


/* Macros for the t and f probabilities */
#define tprob(a,t)   1.-incompBeta(0.5 * a, 0.5, a / (a + t * t))/2.
#define	fprob(a,b,f)  1.-incompBeta(0.5 * b, 0.5 * a, b / (b + a * f))

/*
  NOTE on comparison to old tvalue routine based on Press et al:
  Fractional difference is below 1.e-6 for p <= 0.995, ndf up to 150
  It rises for higher p or, to some extent for higher ndf (> 1000 especially),
  up to values of 3e-5.  Comparison with R indicates Press tends to be a bit
  worse, and values are are relatively close.
*/

/*!
 * Returns the t-value that gives the significance level [signif] with the
 * number of degrees of freedom [ndf], where [signif] should be between 0.5
 * and 1.0.
 */
double tValue(double signif, int ndf)
{
  double iniStep = 0.1;
  int iter, maxIter = 200;
  double maxErr = 1.e-7;
  double maxInterval = 5.e-7;
  double x1, x2, y1, y2, xnew, ynew, err, slope;

  if (signif >= 1.)
    return 1.e10;
  if (signif <= 0.5)
    return 0.;

  /* Take steps and find first X with probability > signif */
  x1 = 0.;
  y1 = tprob(ndf, 0.);
  while (x1 < 1.e4) {
    if (x1 > 1.)
      x2 = x1 + 5. * iniStep;
    else
      x2 = x1 + iniStep;
    y2 = tprob(ndf, x2);
    if (y2 == signif)
      return x2;
    if (y2 > signif)
      break;
    x1 = x2;
    y1 = y2;
  }
  if (y2 < signif)
    return 1.e10;

  /* Iterate by Newton's method to refine the value */
  for (iter = 0; iter < maxIter; iter++) {
    slope = (y2 - y1) / (x2 - x1);
    xnew = x2 - (y2 - signif) / slope;
    ynew = tprob(ndf, xnew);
    err = ynew - signif;
    if (fabs(err) < maxErr)
      return xnew;
    if (ynew < signif) {
      x1 = xnew;
      y1 = ynew;
    } else {
      x2 = xnew;
      y2 = ynew;
    }
    if (x2 - x1 < maxInterval)
      break;
  }
  return xnew;
}

/*!
 * Fortran wrapper for @tValue
 */
double dtvalue(double *signif, int *ndf)
{
  return tValue(*signif, *ndf);
}

/*
  NOTE on comparison to old fvalue routine based on Press et al:
  Fractional difference is below 1.e-6 for p <= ~0.995 as long as one df is 
  fairly modest (say ~15).  With both df's high it can easily differ by 1.e-4.
  And differences grow for higher p.  And differences get huge with two big 
  df's and p > .995.  But R indicates that the new function is correct and
  the Press function is wrong.
*/

/*!
 * Returns the F-value that gives the cumulative probability value [signif] 
 * with the number of degrees of freedom [ndf1] and [ndf2].
 */
double fValue(double signif, int ndf1, int ndf2)
{
  double iniStep = 0.1;
  int iter, maxIter = 200;
  double maxErr = 1.e-7;
  double maxInterval = 5.e-7;
  double x1, x2, y1, y2, xnew, ynew, err, slope;

  if (signif >= 1.)
    return 1.e10;
  if (signif <= 0.)
    return 0.;

  /* Take steps and find first X with probability > signif */
  x1 = 0.;
  y1 = fprob(ndf1, ndf2, 0.);
  while (x1 < 1.e4) {
    if (x1 > 1.)
      x2 = x1 + 5. * iniStep;
    else
      x2 = x1 + iniStep;
    y2 = fprob(ndf1, ndf2, x2);
    if (y2 == signif)
      return x2;
    if (y2 > signif)
      break;
    x1 = x2;
    y1 = y2;
  }
  if (y2 < signif)
    return 1.e10;

  /* Iterate by Newton's method to refine the value */
  for (iter = 0; iter < maxIter; iter++) {
    slope = (y2 - y1) / (x2 - x1);
    xnew = x2 - (y2 - signif) / slope;
    ynew = fprob(ndf1, ndf2, xnew);
    err = ynew - signif;
    if (fabs(err) < maxErr)
      return xnew;
    if (ynew < signif) {
      x1 = xnew;
      y1 = ynew;
    } else {
      x2 = xnew;
      y2 = ynew;
    }
    if (x2 - x1 < maxInterval)
      break;
  }
  return xnew;
}

/*!
 * Fortran wrapper for @fValue
 */
double dfvalue(double *signif, int *ndf1, int *ndf2)
{
  return fValue(*signif, *ndf1, *ndf2);
}

/* The routines errFunc, incompBeta, betaFunc, and gammaFunc are based on the
   modules mincob.for and merror.for from "Computation of Special Functions"
   by Shanjie Zhang and Jianming Jin, John Wiley, New York, 1996.  These 
   modules are copyrighted, but may be incorporated into programs provided that
   the copyright is acknowledged.

   NOTES on accuracy of errFunc for computing the complementary error function:
   The fraction difference from the output of the Press et al. erfcc is at most
   1.e-6 for x up to 3.5, 3.7e-6 at 3.5 and declining again above there until 
   rising above 1.e-5 past 4.9, where the value of erfcc is 1.e-11
   The Press function claimed to be accurate to 2.e-7.
*/   

/*!
 * Returns the value of the error function erf() at [x].
 */
double errFunc(double x)
{
  double eps, pi, x2, er, err, r, c0;
  int k;
  eps = 1.0e-15;
  pi = 3.141592653589793;
  x2 = x * x;
  if (fabs(x) < 3.5) {
    er = 1.0;
    r = 1.0;
    for (k = 1; k <= 50; k++) {
      r = r * x2 / (k + 0.5);
      er = er + r;
      if (fabs(r) <= fabs(er) * eps)
        break;
    }
    c0 = 2.0 / sqrt(pi) * x * exp(-x2);
    err = c0 * er;
  } else {
    er = 1.0;
    r = 1.0;
    for (k = 1; k <= 12; k++) {
      r = -r * (k - 0.5) / x2;
      er = er + r;
    }
    c0 = exp(-x2) / (fabs(x) * sqrt(pi));
    err = 1.0 - c0 * er;
    if (x < 0.0)
      err = -err;
  }
  return err;
}

/*!
 * Fortran wrapper for @errFunc
 */
double errfunc(double *x)
{
  return errFunc(*x);
}

/*
  NOTES on accuracy of incompBeta compared to Press at al. betai:
  Fractional difference from betai is often less than 2e-6, but with relatively
  large a and b, difference up to 4e-5 are seen even for non-extreme returned
  values.  It is more relevant (and easier to characterize) to consider the 
  differences in f and t values.
*/  

/*!
 * Computes and returns the incomplete beta function of [x] for parameters
 * [a] and [b], for 0 <= [x] <= 1, and [a] > 0 and [b] > 0.
 */
double incompBeta(double a, double b, double x)
{
  double dk[51],fk[51];
  double s0, bt, bix, t1, ta, t2, tb;
  int k;
  s0=(a+1.0)/(a+b+2.0);
  bt = betaFunc(a,b);
  if (x <= s0) {
    for (k = 1; k <= 20; k++)
      dk[2*k-1]=k*(b-k)*x/(a+2.0*k-1.0)/(a+2.0*k);
    for (k = 0; k <= 20; k++)
      dk[2*k]=-(a+k)*(a+b+k)*x/(a+2.*k)/(a+2.0*k+1.0);
    t1=0.0;
    for (k = 20; k >= 1; k--)
      t1=dk[k-1]/(1.0+t1);
    ta=1.0/(1.0+t1);
    bix=pow(x, a)*pow(1.0-x,b)/(a*bt)*ta;
  } else {
    for (k = 1; k <= 20; k++)
      fk[2*k-1]=k*(a-k)*(1.0-x)/(b+2.*k-1.0)/(b+2.0*k);
    for (k = 0; k <= 20; k++)
      fk[2*k]=-(b+k)*(a+b+k)*(1.-x)/ (b+2.*k)/(b+2.*k+1.);
    t2=0.0;
    for (k = 20; k >= 1; k--)
      t2=fk[k-1]/(1.0+t2);
    tb=1.0/(1.0+t2);
    bix=1.0-pow(x,a)*pow(1.0-x,b)/(b*bt)*tb;
  }
  return bix;
}

/*!
 * Fortran wrapper for @incompBeta
 */
double incompbeta(double *a, double *b, double *x)
{
  return incompBeta(*a, *b, *x);
}

/*!
 * Computes and returns the beta function of [p] and [q], which must be > 0.
 */
double betaFunc(double p, double q)
{
  double gp, gq, ppq, gpq, bt;
  gp = lnGamma(p);
  gq = lnGamma(q);
  ppq=p+q;
  gpq = lnGamma(ppq);
  bt=exp(gp+ gq - gpq);
  return bt;
}

/*
  NOTE that this was astoundingly close to the results from the gamma function
  provided with mincob.for (fractional differences < 1.e-13) up to the point
  where the latter blew up.
*/

/*!
 * Computes and returns the natural log of the gamma function of [x], which 
 * should not equal 0, -1, -2, etc.
 */
double lnGamma(double x)
{
  double gl, x0,xp,x2,gl0;
  int n, k;
  double a[10] = {8.333333333333333e-02,-2.777777777777778e-03,
                  7.936507936507937e-04,-5.952380952380952e-04,
                  8.417508417508418e-04,-1.917526917526918e-03,
                  6.410256410256410e-03,-2.955065359477124e-02,
                  1.796443723688307e-01,-1.39243221690590};
  x0=x;
  if (x == 1.0 || x == 2.0) {
    return 0.;
  } else if (x <= 7.0) {
    n=(int)(7. - x);
    x0 = x + n;
  }
  x2=1.0/(x0*x0);
  xp=6.283185307179586477;
  gl0=a[9];
  for (k = 9; k >= 1; k--)
    gl0=gl0*x2+a[k-1];
  gl=gl0/x0+0.5*log(xp)+(x0-.5)*log(x0)-x0;
  if (x <= 7.0) {
    for (k = 1; k <= n; k++) {
      gl=gl-log(x0-1.0);
      x0=x0-1.0;
    }
  }
  return gl;
}

/*

$Log$


*/
