/*  simplestat.c - Functions for getting mean and SD and line fits
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
#include "cfsemshare.h"

#ifdef F77FUNCAP
#define avgsd AVGSD
#define sums_to_avgsd SUMS_TO_AVGSD
#define sums_to_avgsd8 SUMS_TO_AVGSD8
#define lsfit LSFIT
#define lsfits LSFITS
#define lsfitpred LSFITPRED
#define lsfit2 LSFIT2
#define lsfit2noc LSFIT2NOC
#define lsfit2pred LSFIT2PRED
#else
#define avgsd avgsd_
#define lsfit lsfit_
#define lsfits lsfits_
#define lsfitpred lsfitpred_
#define lsfit2 lsfit2_
#define lsfit2noc lsfit2noc_
#define lsfit2pred lsfit2pred_
#ifdef G77__HACK
#define sums_to_avgsd sums_to_avgsd__
#define sums_to_avgsd8 sums_to_avgsd8__
#else
#define sums_to_avgsd sums_to_avgsd_
#define sums_to_avgsd8 sums_to_avgsd8_
#endif
#endif

/*!
 * Calculates the mean [avg], standard deviation [sd], and standard
 * error of mean [sem], from the [n] values in array [x].  Callable from
 * Fortran by the same name.
 */      
void avgSD(float *x, int n, float *avg, float *sd, float *sem)
{
  float sx, sxsq, avnew, d;
  int i;
  sx=0.;
  sxsq=0.;
  for (i = 0; i < n; i++)
    sx += x[i];
  *avg = sx / n;
  sx = 0.;
  for (i = 0; i < n; i++) {
    d = x[i] - *avg;
    sx += d;
    sxsq += d * d;
  }
  sumsToAvgSD(sx, sxsq, n, &avnew, sd);
  *avg += avnew;
  *sem = 0.;
  if (n > 0)
    *sem = *sd / (float)sqrt((double)n);
}

/*!
 * Fortran wrapper for @avgSD
 */
void avgsd(float *x, int *n, float *avg, float *sd, float *sem)
{
  avgSD(x, *n, avg, sd, sem);
}

/*!
 * Computes a mean [avg] and standard deviation [sd] from the
 * sum of values [sx], sum of squares [sxsq], and number of values [n].
 * It will not generate any division by 0 errors.  Callable from
 * Fortran by the same name.
 */
void sumsToAvgSD(float sx, float sxsq, int n, float *avg, float *sd)
{
  double den;
  *avg = 0.;
  *sd = 0.;
  if (n <= 0)
    return;
  *avg = sx / n;
  if (n > 1) {
    den = (sxsq - n * (*avg) * (*avg)) / (n - 1.);
    if (den > 0)
      *sd = (float)sqrt(den);
  }
}

/*!
 * Fortran wrapper for @sumsToAvgSD
 */
void sums_to_avgsd(float *sx, float *sxsq, int *n, float *avg, float *sd)
{
  sumsToAvgSD(*sx, *sxsq, *n, avg, sd);
}

/*!
 * Computes a mean [avg] and standard deviation [sd] from the sum of
 * values [sx8], sum of squares [sxsq8], and number of values [n1] * [n2],
 * where the number of values can be greater than 2**31.
 * It will not generate any division by 0 errors.  
 */  
void sumsToAvgSDdbl(double sx8, double sxsq8, int n1, int n2, float *avg,
                    float *sd)
{
  double avg8, dn, den;
  *avg = 0.;
  *sd = 0.;
  dn = n1;
  dn = dn * n2;
  if (dn <= 0)
    return;
  avg8 = sx8 / dn;
  *avg = (float)avg8;
  if (dn > 1.) {
    den = (sxsq8 - dn * avg8 * avg8) / (dn - 1.);
    if (den > 0.)
      *sd = (float)sqrt(den);
  }
}

/*!
 * Fortran wrapper for @sumsToAvgSDdbl; use real*8 for sx8, sxsq8.
 */
void sums_to_avgsd8(double *sx8, double *sxsq8, int *n1, int *n2, float *avg,
                    float *sd)
{
  sumsToAvgSDdbl(*sx8, *sxsq8, *n1, *n2, avg, sd);
}

/*!
 * Fits a straight line to the [n] points in arrays [x] and [y] by the
 * method of least squares, returning [slope], intercept [bint], 
 * correlation coeficient [ro].
 */
void lsFit(float *x, float *y, int num, float *slope, float *intcp, float *ro)
{
  float ypred, prederr, sa, sb, se;
  lsFitPred(x, y, num, slope, intcp, ro, &sa, &sb, &se, 0., &ypred, &prederr);
}

/*!
 * Fortran wrapper for @lsFit
 */
void lsfit(float *x, float *y, int *num, float *slope, float *intcp, float *ro)
{
  lsFit(x, y, *num, slope, intcp, ro);
}

/*!
 * Fits a straight line to the [n] points in arrays [x] and [y] by the
 * method of least squares, returning [slope], intercept [bint], 
 * correlation coeficient [ro], standard errors of the estimate 
 * [se], the slope [sb], and the intercept [sa], and for one X value
 * [xpred], it returns the predicted value [ypred] and the standard
 * error of the prediction [prederr].
 */
void lsFitPred(float *x, float *y, int n, float *slope, float *bint, float *ro,
               float *sa, float *sb, float *se,
               float xpred, float *ypred, float *prederr)
{
  double sx, sy, xbar, ybar,sxpsq, sxyp, sypsq, d, roden;
  double sxy, sysq, rotmp, setmp, xp, yp;
  int i;
  *slope = 1.;
  *bint = *ro = *sa = *sb = *se = *ypred = *prederr = 0.;
  if (n < 2)
    return;
  sx=0.;
  sy=0.;
  for (i=0; i < n; i++) {
    sx=sx+x[i];
    sy=sy+y[i];
  }
  xbar=sx/n;
  ybar=sy/n;
  sxpsq=0.;
  sxyp=0.;
  sypsq=0.;
  for (i=0; i < n; i++) {
    xp=x[i]-xbar;
    yp=y[i]-ybar;
    sxpsq=sxpsq+xp*xp;
    sypsq=sypsq+yp*yp;
    sxyp=sxyp+xp*yp;
  }
  d=n*sxpsq;
  *slope=(float)(sxyp/sxpsq);
  *bint=(float)((ybar*sxpsq-xbar*sxyp)/sxpsq);
  roden=sqrt(sxpsq*sypsq);
  *ro=1.;
  rotmp=sxyp/roden;
  if(roden != 0. && rotmp >= -1. && rotmp <= 1.)
    *ro=(float)rotmp;
  *se=0.;
  sxy=sxyp+n*xbar*ybar;
  sysq=sypsq+n*ybar*ybar;
  setmp = (sysq-*bint*sy-*slope*sxy)/(n-2);
  if(n > 2 && setmp > 0.)
    *se=(float)sqrt(setmp);
  *sa=(float)(*se*sqrt((1./n+(sx*sx/n)/d)));
  *sb=(float)(*se/sqrt((d/n)));
  
  *ypred=*slope*xpred+*bint;
  *prederr=(float)(*se*sqrt((1.+1./n+ n*(xpred-xbar)*(xpred-xbar)/d)));
}

/*!
 * Fortran wrapper for @lsFitPred
 */
void lsfitpred(float *x, float *y, int *n, float *slope, float *bint,
               float *ro, float *sa, float *sb, float *se,
               float *xpred, float *ypred, float *prederr)
{
  lsFitPred(x, y, *n, slope, bint, ro, sa, sb, se, *xpred, ypred, prederr);
}

/*!
 * Fortran wrapper for @lsFitPred that returns the standard errors and omits
 * the prediction
 */
void lsfits(float *x, float *y, int *n, float *slope, float *bint, float *ro,
            float *sa, float *sb, float *se)
{
  float xpred, ypred, prederr;
  xpred = x[1];
  lsFitPred(x, y, *n, slope, bint, ro, sa, sb, se, xpred, &ypred, &prederr);
}

/*!
 * Does a linear regression fit of the [n] values in the array [y] to
 * the values in the arrays [x1] and [x2], namely to the equation
 * ^   y = a * x1 + b * x2 + c   ^
 * It returns the coefficients [a] and [c], and the intercept [c].
 * If [c] is NULL it fits instead to 
 * ^   y = a * x1 + b * x2
*/
void lsFit2(float *x1, float *x2, float *y, int n, float *a, float *b,
            float *c)
{
  float ypred, prederr;
  lsFit2Pred(x1, x2, y, n, a, b, c, 0., 0., &ypred, &prederr);
}

/*!
 * Fortran wrapper for @lsFit2
 */
void lsfit2(float *x1, float *x2, float *y, int *n, float *a, float *b,
            float *c)
{
  lsFit2(x1, x2, y, *n, a, b, c);
}

/*!
 * Fortran wrapper for calling @lsFit2 with [c] NULL.
 */
void lsfit2noc(float *x1, float *x2, float *y, int *n, float *a, float *b)
{
  lsFit2(x1, x2, y, *n, a, b, NULL);
}


/*!
 * Does a linear regression fit of the [n] values in the array [y] to
 * the values in the arrays [x1] and [x2], namely to the equation
 * ^   y = a * x1 + b * x2 + c   ^
 * It returns the coefficients [a] and [c], and the intercept [c], but
 * if [c] is NULL it fits instead to 
 * ^   y = a * x1 + b * x2   ^
 * For one value of x1 and x2 given by [x1pred] and [x2pred], it returns the
 * value predicted by the equation in [ypred] and the standard error of the
 * prediction in [prederr].
 */
void lsFit2Pred(float *x1, float *x2, float *y, int n, float *a, float *b, 
                float *c, float x1pred, float x2pred, float *ypred,
                float *prederr)
{
  float x1s, x2s, ys, x1m, x2m, ym, x1sqs, x2sqs, x1x2s, x1ys, x2ys, ysqs;
  int i;
  float denom, anum, bnum, x1p, x2p, yp, absdenom, absanum, absbnum;
  float c11, c22, c12, predsq, devss;
  x1s=0.;
  x2s=0.;
  ys=0.;
  for (i=0; i < n; i++) {
    x1s=x1s+x1[i];
    x2s=x2s+x2[i];
    ys=ys+y[i];
  }
  
  x1m=x1s/n;
  x2m=x2s/n;
  ym=ys/n;
  x1sqs=0.;
  x2sqs=0.;
  x1x2s=0.;
  x1ys=0.;
  x2ys=0.;
  ysqs = 0.;
  for (i=0; i < n; i++) {
    if (c) {
      x1p=x1[i]-x1m;
      x2p=x2[i]-x2m;
      yp=y[i]-ym;
    } else {
      x1p=x1[i];
      x2p=x2[i];
      yp=y[i];
    }
    x1sqs=x1sqs+x1p*x1p;
    x2sqs=x2sqs+x2p*x2p;
    x1ys=x1ys+x1p*yp;
    x2ys=x2ys+x2p*yp;
    x1x2s=x1x2s+x1p*x2p;
    ysqs=ysqs+yp*yp;
  }
  denom=x1sqs*x2sqs-x1x2s*x1x2s;
  anum=x1ys*x2sqs-x1x2s*x2ys;
  bnum=x1sqs*x2ys-x1ys*x1x2s;
  *a=0.;
  *b=0.;
  if (c)
    *c=ym;
  *ypred=ym;
  *prederr=0.;
  absanum = anum > 0. ? anum : -anum;
  absbnum = bnum > 0. ? bnum : -bnum;
  absdenom = denom > 0. ? denom : -denom;
  if (absanum < absbnum)
    absanum = absbnum;
  if (absdenom < 1.e-20 * absanum)
    return;
  *a=anum/denom;
  *b=bnum/denom;
  if (!c)
    return;
  *c=ym-*a*x1m-*b*x2m;
  *ypred=*a*x1pred+*b*x2pred+*c;
  c11=x2sqs/denom;
  c22=x1sqs/denom;
  c12=-x1x2s/denom;
  devss=ysqs-*a*x1ys-*b*x2ys;
  predsq=(float)(1.+1./n+c11*(x1pred-x1m)*(x1pred-x1m)+
    c22*(x2pred-x2m)*(x2pred-x2m)+2.*c12*(x1pred-x1m)*(x2pred-x2m));
  if(n < 3 || predsq < 0. || devss < 0.)
    return;
  *prederr=(float)sqrt((double)((devss/(n-3))*predsq));
  
}

/*!
 * Fortran wrapper for @lsFit2Pred
 */
void lsfit2pred(float *x1, float *x2, float *y, int *n, float *a, float *b, 
                float *c, float *x1pred, float *x2pred, float *ypred,
                float *prederr)
{
  lsFit2Pred(x1, x2, y, *n, a, b, c, *x1pred, *x2pred, ypred, prederr);
}

/*

$Log$
Revision 1.5  2008/11/14 20:44:53  mast
Fix another call in wrapper, eliminate warnings

Revision 1.4  2008/11/14 20:41:06  mast
Fix it right!

Revision 1.3  2008/11/14 20:40:15  mast
Switched name of call from avgsd

Revision 1.2  2008/11/14 20:38:03  mast
Change to remove b3dutil include

Revision 1.1  2008/11/14 19:58:56  mast
Switched to C versions


*/
