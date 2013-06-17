/*  regression.c - replacements for old Fortran regression routines, and robust regression
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2012 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */
#include <math.h>
#include "imodconfig.h"
#include "b3dutil.h"
#include "string.h"

/* Macro to allow data matrix to be accessed with row, column indices regardless
   of its order */
#define XRC(r, c) (x[(r) * rowStride + (c) * colStride])

#ifdef F77FUNCAP
#define statmatrices STATMATRICES
#define multregress MULTREGRESS
#define robustregress ROBUSTREGRESS
#define polynomialfit POLYNOMIALFIT
#define weightedpolyfit WEIGHTEDPOLYFIT
#else
#define statmatrices statmatrices_
#define multregress multregress_
#define robustregress robustregress_
#define polynomialfit polynomialfit_
#define weightedpolyfit weightedpolyfit_
#endif

/*!
 * Computes basic statistical values and matrices from a data matrix representing a series
 * of measurements of multiple variables.  ^
 * Input parameters:  ^
 * [x]       - Input data matrix  ^
 * [xsize]   - Size of the fastest-progressing dimension of [x]  ^
 * [colFast] - Nonzero if the column dimension is the fastest progressing one  ^
 * [m]       - Number of columns of data, i.e., number of parameters  ^
 * [msize]   - Size of one dimension of the various square output matrices  ^
 * [ndata]   - Number of rows of data; i.e., number of separate measurements ^
 * [ifdisp]  - Value indicating either to skip computation of [d] and [r] (if 0), or to
 * treat the {m+1} column of data as weighting values (if < 0) ^
 * Outputs:
 * [sx]      - Array for the sums of the [m] data columns  ^
 * [ss]      - Array for raw sums of squares and cross-products  ^
 * [ssd]     - Array for sums of deviation squares and cross-products  ^
 * [d]       - Array for variances and covariances  (dispersion matrix) ^
 * [r]       - Array for matrix of correlation coefficients  ^
 * [xm]      - Array for the means of the [m] data columns  ^
 * [sd]      - Array for the standard deviations of the [m] data columns  ^
 * The output matrices will be treated as having leading dimension [msize], so they must 
 * be at least [msize] x [m] in size.  The data element at row i, column j would
 * be accessed as x\[i + j * xsize\] or x\[j\]\[i\] from C, or as x(i,j) from Fortran if
 * [colFast] is 0, or as x\[j + i * xsize\], x\[i\]\[j\], or x(j,i) if [colFast] is 
 * nonzero.  When  weighting is used, the returned values are weighted means, and other 
 * statistics with the weights incorporated.
 */
void statMatrices(float *x, int xsize, int colFast, int m, int msize, int ndata,
                  float *sx, float *ss, float *ssd, float *d, float *r, float *xm,
                  float *sd, int ifdisp)
{
  float fndata, den, weight, wsum;
  int i, j, k;
  int colStride = colFast ? 1 : xsize;
  int rowStride = colFast ? xsize : 1;
  
  fndata = ndata;
  
  for (i = 0; i< m; i++) {
    sx[i] = 0.;
    for (j = 0; j < m; j++) {
      ssd[msize * i + j] = 0.;
      r[msize * i + j] = 0.;
    }
  }
  
  if (ifdisp >= 0) {
    for (i = 0; i< m; i++) {
      for (k = 0; k < ndata; k++)
        sx[i] += XRC(k, i);
      xm[i] = sx[i] / fndata;
    }
  } else {
    wsum = 0.;
    for (k = 0; k < ndata; k++)
      wsum += XRC(k, m);
    for (i = 0; i< m; i++) {
      for (k = 0; k < ndata; k++)
        sx[i] += XRC(k, i) * XRC(k, m);
      xm[i] = sx[i] / wsum;
    }
  }
  
  for (k = 0; k < ndata; k++) {
    weight = 1.;
    if(ifdisp < 0)
      weight = XRC(k, m);
    for (i = 0; i< m; i++)
      for (j = 0; j < m; j++)
        ssd[i * msize + j] += (XRC(k, i) - xm[i]) * (XRC(k, j) - xm[j]) * weight;
  }
  
  for (i = 0; i< m; i++) {
    sd[i] = (float)sqrt((double)(ssd[i * msize + i] / (fndata-1.)));
    for (j = 0; j < m; j++) {
      ss[i * msize + j] = ssd[i * msize + j] + sx[i] * sx[j] / fndata;
      ss[j * msize + i] = ss[i * msize + j];
      ssd[j * msize + i] = ssd[i * msize + j];
    }
  }
  if(ifdisp == 0)
    return;
  for (i = 0; i< m; i++) {
    for (j = 0; j < m; j++) {
      d[i * msize + j] = ssd[i * msize + j] / (fndata-1.);
      d[j * msize + i] = d[i * msize + j];
      den = sd[i] * sd[j];
      r[i * msize + j] = 1.;
      if(den > 1.e-30)
        r[i * msize + j] = d[i * msize + j] / (sd[i] * sd[j]);
      r[j * msize + i] = r[i * msize + j];
    }
  }
}

/*!
 * Fortran wrapper for @statMatrices
 */
void statmatrices(float *x, int *xsize, int *colFast, int *m, int *msize, int *ndata,
                  float *sx, float *ss, float *ssd, float *d, float *r, float *xm,
                  float *sd, int *ifdisp)
{
  statMatrices(x, *xsize, *colFast, *m, *msize, *ndata, sx, ss, ssd, d, r, xm, sd,
               *ifdisp);
}


/*!
 * Computes a multiple linear regression (least-squares fit) for the relationships between
 * one or more dependent variables and a set of independent variables.  ^
 * Input parameters:  ^
 * [x]       - Input data matrix  ^
 * [xsize]   - Size of the fastest-progressing dimension of [x]  ^
 * [colFast] - Nonzero if the column dimension is the fastest progressing one  ^
 * [m]       - Number of columns of data for independent variables  ^
 * [ndata]   - Number of rows of data; i.e., number of separate measurements ^
 * [nbcol]   - Number of columns of data for dependent variables; i.e., number of 
 * relationships to fit  ^
 * [wgtcol]  - Column number with weighting factors if > 0, otherwise no weighting.  
 * Columns are numbered from 0. ^
 * [bsize]   - Size of the fastest-progressing dimension of [b], the array/matrix to
 * receive the solutions; must be at least [m]  ^
 * [work]    - Any array for temporary use whose size must be at least 
 * (m + nbcol) * (m + nbcol)  floating point elements  ^
 * Outputs:  ^
 * [b]       - Matrix to receive the [nbcol] sets of [m] coefficients.  Each set is 
 * placed in a column of [b], where the row dimension is the fastest progressing one ^
 * [c]       - Array to receive the [m] constant terms of the fits  ^
 * [xm]      - Array for the means of the [m] plus [nbcol] data columns  ^
 * [sd]      - Array for the standard deviations of the [m] plus [nbcol] data columns  ^
 * The independent variables should be placed in the first [m] columns of [x] and the
 * the dependent variables in the next [nbcol] columns. The data element at row i,
 * column j would be accessed as x\[i + j * xsize\] or x\[j\]\[i\] from C, or as x(i,j) 
 * from Fortran if [colFast] is 0, or as x\[j + i * xsize\], x\[i\]\[j\], or x(j,i) if 
 * [colFast] is nonzero.  ^
 * The return value is 1 if [wgtcol] has an inappropriate value, or 3 if @gaussj returns
 * with an error.
 */
int multRegress(float *x, int xsize, int colFast, int m, int ndata, int nbcol,
                int wgtcol, float *b, int bsize, float *c, float *xm, float *sd,
                float *work)
{
  float fndata, den;
  double dsum, wsum;
  int i, j, k, mp;
  int colStride = colFast ? 1 : xsize;
  int rowStride = colFast ? xsize : 1;
  mp = m + nbcol;
  fndata = ndata;
  if (wgtcol > 0 && (wgtcol < mp || (!colFast && wgtcol >= xsize)))
    return 1;

  /* Get the unweighted means */
  if (wgtcol <= 0) {
    for (i = 0; i< mp; i++) {
      dsum = 0.;
      for (k = 0; k < ndata; k++)
        dsum += XRC(k, i);
      xm[i] = dsum / fndata;
    }
  } else {

    /* Or add up the weights and get the weighted means */
    wsum = 0.;
    for (k = 0; k < ndata; k++)
      wsum += XRC(k, wgtcol);
    for (i = 0; i< mp; i++) {
      dsum = 0.;
      for (k = 0; k < ndata; k++)
        dsum += XRC(k, i) * XRC(k, wgtcol);
      xm[i] = dsum / wsum;
    }
  }

  /* Get the sums of squares and cross-products of deviations */
  for (i = 0; i < mp; i++) {
    for (j = i; j < mp; j++) {
      if (i >= m && i != j)
        continue;
      dsum = 0.;
      if (wgtcol <= 0) {
        for (k = 0; k < ndata; k++)
          dsum += (XRC(k, i) - xm[i]) * (XRC(k, j) - xm[j]);
      } else {
        for (k = 0; k < ndata; k++)
          dsum += (XRC(k, i) - xm[i]) * (XRC(k, j) - xm[j]) * XRC(k, wgtcol);
      }
      work[j * mp + i] = dsum;
    }
  }
    
  /* Get the SDs */
  for (i = 0; i< mp; i++)
    sd[i] = (float)sqrt((double)(work[i * mp + i] / (fndata-1.)));

  /* Scale it by n -1 to get covariance and by SD's to get the correlation matrix */
  for (i = 0; i < m; i++) {
    for (j = i; j < mp; j++) {
      den = sd[i] * sd[j];
      if (den < 1.e-30) {
        /* printf("sds %d %d %f %f  den %f\n", i, j, sd[i], sd[j], den);
           fflush(stdout); */
        work[j * mp + i] = 1.;
      } else
        work[j * mp + i] /= den * (fndata-1.);
      if (j < m)
        work[i * mp + j] = work[j * mp + i];
    }
  }
  /* for (i = 0; i < m; i++) {
    for (j = 0; j < mp; j++)
      printf("  %.8f", work[j * mp + i]);
    printf("\n");
    } */
  
  /* The matrix to be solved is now completely filled and symmetric so row/column
     doesn't matter, but to call gaussj we need to transpose the final columns into b */
  for (j = 0; j < nbcol; j++) {
    for (i = 0; i < m; i++)
      b[j + i * nbcol] = work[(j + m) * mp + i];
    /*printf("\nb:");
    for (i = 0; i < m; i++)
    printf("    %g", b[j + i * nbcol]); */
  }

  if (gaussj(work, m, mp, b, nbcol, nbcol))
    return 3;
  
  /*for (j = 0; j < nbcol; j++) {
    printf("\nb sol:");
    for (i = 0; i < m; i++)
      printf("    %g", b[j + i * nbcol]);
  }
  printf("\n");
  fflush(stdout);*/

  /* Scale the coefficients and transpose them back; get constant terms */
  memcpy(work, b, m * nbcol * sizeof(float));
  for (j = 0; j < nbcol; j++) {
    c[j] = xm[m + j];
    for (i = 0; i< m; i++) {
      if (sd[i] < 1.e-30)
        b[i + bsize * j] = 0.;
      else
        b[i + bsize * j] = work[j + i * nbcol] * sd[m + j] / sd[i];
      c[j] -= b[i + bsize * j] * xm[i];
    }
  }
  return 0;
}

/* Fortran wrapper */
int multregress(float *x, int *xsize, int *colFast, int *m, int *ndata, int *nbcol,
                int *wgtcol, float *b, int *bsize, float *c, float *xm, float *sd,
                float *work)
{
  return multRegress(x, *xsize, *colFast, *m, *ndata, *nbcol, *wgtcol - 1, b, *bsize, c,
                     xm, sd, work);
}

/* LAPACK NOTE:  To use ssysv, need to add
ssysv.$(O) ssytrf.$(O) ssytrs.$(O) slasyf.$(O) ssytf2.$(O)
slasyf.f  ssysv.f  ssytf2.f  ssytrf.f  ssytrs.f
to lapack, and
sscal.$(O) sger.$(O)  sgemv.$(O) sswap.$(O) scopy.$(O) isamax.$(O)  sgemm.$(O)\
        ssyr.$(O)
scopy.f  sgemm.f  sgemv.f  sger.f  sscal.f  sswap.f  ssyr.f isamax.f
to blas.  To use sspsv, need to add
ssptrs.$(O) ssptrf.$(O) sspsv.$(O)
sspsv.f  ssptrf.f  ssptrs.f  
to lapack and
sspr.$(O)
sspr.f
to blas

But hey, we could just ask for a double work array here.
The impediment is not doubles, it's having a different library.
Also, a bigger work array is needed not just for doubles but also for dsysv.
*/

/*!
 * Uses multiple linear regression to fit a polynomial of order
 * [order] to [ndata] points whose (x,y) coordinates are in the arrays [x] and [y].
 * It returns the coefficient of x to the i power in the array [slopes] and a
 * constant term in [intcpt].  The equation fit is:  ^
 * Y = intcpt + slopes[0] * X + slopes[1] * X**2 + ...  ^
 * [work] is an array whose size must be at least ([order] + 1) * ([order] + 3 + [ndata]).
 * The return value is the value returned by @@multRegress@.  Note that a Fortran 
 * function polyfit in libhvem takes care of allocating [work] to the needed size and 
 * calling the Fortran wrapper to this function.
 */
int polynomialFit(float *x, float *y, int ndata, int order, float *slopes, float *intcpt,
                  float *work)
{
  int wdim = order + 1;
  int i, j;
  float *xm = work + wdim * ndata;
  float *sd = xm + wdim;
  float *mwork = sd + wdim;
  if (!order)
    return 1;
  for (i = 0; i < ndata; i++) {
    for (j = 0; j < order; j++)
      work[i + j * ndata] = (float)pow((double)x[i], j + 1.);
    work[i + order * ndata] = y[i];
  }
  return (multRegress(work, ndata, 0, order, ndata, 1, 0, slopes, ndata, intcpt, xm, sd,
                      mwork));
}

/* Fortran wrapper */
int polynomialfit(float *x, float *y, int *ndata, int *order, float *slopes, 
                  float *intcpt, float *work)
{
  return polynomialFit(x, y, *ndata, *order, slopes, intcpt, work);
}

/*!
 * Uses multiple linear regression to fit a polynomial of order
 * [order] to [ndata] points whose (x,y) coordinates are in the arrays [x] and [y].
 * It returns the coefficient of x to the i power in the array [slopes] and a
 * constant term in [intcpt].  The equation fit is:  ^
 * Y = intcpt + slopes[0] * X + slopes[1] * X**2 + ...  ^
 * [work] is an array whose size must be at least ([order] + 2) * [ndata] +
 * ([order] + 1) * ([order] + 3)).  ^
 * The return value is the value returned by @@multRegress@.  This function is untested.
 */
int weightedPolyFit(float *x, float *y, float *weight, int ndata, int order,
                    float *slopes, float *intcpt, float *work)
{
  int wdim = order + 1;
  int i, j;
  float *xm = work + (order + 2) * ndata;
  float *sd = xm + wdim;
  float *mwork = sd + wdim;
  if (!order)
    return 1;
  for (i = 0; i < ndata; i++) {
    for (j = 0; j < order; j++)
      work[i + j * ndata] = (float)pow((double)x[i], j + 1.);
    work[i + order * ndata] = y[i];
    work[i + (order + 1) * ndata] = weight[i];
  }
  return (multRegress(work, ndata, 0, order, ndata, 1, order + 1, slopes, ndata, intcpt,
                      xm, sd, mwork));
}

/* Fortran wrapper */
int weightedpolyfit(float *x, float *y, float *weight, int *ndata, int *order,
                    float *slopes, float *intcpt, float *work)
{
  return weightedPolyFit(x, y, weight, *ndata, *order, slopes, intcpt, work);
}

/*!
 * Computes a robust least squares fit by iteratively computing a weight from the residual
 * for each data point then doing a weighted regression.  The weight is computed by
 * finding the median and normalized median absolute deviation (MADN) of the residual
 * values.  When there are multiple columns of dependent variables, the square root of the
 * sum of the squares of the residuals for the different variables is used.  Each 
 * residual is standardized by taking (residual - median) / MADN, and the weight is
 * computed from the Tukey bisquare equation using the standardized residual divided by 
 * the specified [kfactor].  However, with multiple dependent variables, all residuals 
 * are positive and ones less than the median are given a weighting of 1.
 * Input parameters:  ^
 * [x]       - Input data matrix  ^
 * [xsize]   - Size of the fastest-progressing dimension of [x]  ^
 * [colFast] - Nonzero if the column dimension is the fastest progressing one  ^
 * [m]       - Number of columns of data for independent variables  ^
 * [ndata]   - Number of rows of data; i.e., number of separate measurements ^
 * [nbcol]   - Number of columns of data for dependent variables; i.e., number of 
 * relationships to fit.   ^
 * [bsize]   - Size of the fastest-progressing dimension of [b], the array/matrix to
 * receive the solutions; must be at least [m]  ^
 * [work]    - Any array for temporary use whose size must be at least the maximum of
 * (m + nbcol) * (m + nbcol) and 2 * ndata floating point elements  ^
 * [kfactor] - Factor by which to divide the standardized residual value in computing the
 * Tukey bisquare weight.  4.68 is the typical value; a different value may be more 
 * appropriate with multiple dependent variables  ^
 * [maxIter] - Maximum number of iterations, or the negative of the maximum to get trace 
 * output on each iteration.  20-50 is probably adequate.  With a negative value, the
 * program outputs the mean and maximum change in weighting and the number of points
 * with weights of 0, between 0 and 0.1, between 0.1 and 0.2, and less than 0.5.   ^
 * [maxZeroWgt] - Maximum number of points allowed to have zero weights.  When this number
 * is exceeded on an iteration, the deviations are sorted and the criterion deviation is 
 * permanently changed to midway 
 * between the deviations for the last point to be retained and first one to be 
 * eliminated.  ^
 * [maxChange]  - Maximum change in weights from one iteration to the next or across two 
 * iterations; the entry should not be smaller than 0.01.  ^
 * [maxOscill]  - Maximum change in weights from one iteration to the next, even if 
 * oscillating. The iterations are terminated when the biggest change in weights between 
 * iterations is less than [maxChange], or when it is less than [maxOscill] and the 
 * biggest change across two iterations is less than [maxChange].
 * Outputs:  ^
 * [b]       - Matrix to receive the [nbcol] sets of [m] coefficients.  Each set is 
 * placed in a column of [b], where the row dimension is the fastest progressing one ^
 * [c]       - Array to receive the [m] constant terms of the fits  ^
 * [xm]      - Array for the means of the [m] plus [nbcol] data columns  ^
 * [sd]      - Array for the standard deviations of the [m] plus [nbcol] data columns  ^
 * [numIter] - Number of iterations  ^
 * The independent variables should be placed in the first [m] columns of [x] and the
 * the dependent variables in the next [nbcol] columns (see @@multRegress@).
 * Final weights are returned in the column of [x] after the dependent variables, and the
 * column after that is used for weights on the previous iteration, so [x]
 * must have at least [m] + [nbcol] + 2 columns.  ^
 * The return value is 1 if there are not enough columns for the weights (detectable 
 * only when [colFast] is nonzero), 3 if @gaussj returns with an error, or -1 if the
 * weights do not converge.
 */
int robustRegress(float *x, int xsize, int colFast, int m, int ndata, int nbcol, float *b,
                  int bsize, float *c, float *xm, float *sd, float *work, float kfactor,
                  int *numIter, int maxIter, int maxZeroWgt, float maxChange,
                  float maxOscill)
{
  int i, j, numOut, ierr, k, iter, num1, num2, num5, report = 0;
  int wgtcol = m + nbcol;
  int prevCol = wgtcol + 1;
  int colStride = colFast ? 1 : xsize;
  int rowStride = colFast ? xsize : 1;
  int splitLastTime = 0, keepCriterion = 0;
  float diff, diffsum, diffmax, median, MADN, dev, weight, ressum, colres;
  float prev, prevmax, criterion;
  if (maxIter < 0) {
    report = 1;
    maxIter = - maxIter;
  }

  if (colFast && prevCol >= xsize)
    return 1;

  /* Initialize weights to 1. */
  for (j = 0; j < ndata; j++)
    XRC(j, wgtcol) = XRC(j, prevCol) = 1.;

  /* Iterate */
  for (iter = 0; iter < maxIter; iter++) {

    /* Get regression solution */
    ierr = multRegress(x, xsize, colFast, m, ndata, nbcol, wgtcol, b, bsize, c, xm, sd,
                       work);
    if (ierr)
      return ierr;

    /* Compute residuals.  Note WORK has to be bigger for this! */
    for (j = 0; j < ndata; j++) {
      if (nbcol == 1) {
        colres = c[0] - XRC(j, m);
        for (i = 0; i < m; i++)
          colres += XRC(j, i) * b[i];
        work[j] = colres;
      } else {
        ressum = 0.;
        for (k = 0; k < nbcol; k++) {
          colres = c[k] - XRC(j, (m + k));
          for (i = 0; i < m; i++)
            colres += XRC(j, i) * b[i + k * bsize];
          /* printf("   %g", colres); */
          ressum += colres * colres;
        }
        work[j] = (float)sqrt(ressum);
        /* printf("   resid  %g\n", work[j]); */
      }
    }

    /* Get the median and MADN */
    rsMedian(work, ndata, &work[ndata], &median);
    rsMADN(work, ndata, median, &work[ndata], &MADN);
    if (!keepCriterion)
      criterion = kfactor * MADN;
    /* printf("Median %g   MADN %g\n", median, MADN); */

    /* Get the new weights and evaluate change from last time */
    diffsum = 0.;
    diffmax = 0.;
    numOut = 0;
    num1 = 0;
    num2 = 0;
    num5 = 0;
    prevmax = 0.;
    
    /* Convert to the deviations that will be compared to criterion and count zero's */
    for (j = 0; j < ndata; j++) {
      work[j] -= median;

      /* For multiple columns, negative deviations are like no deviation */
      if (nbcol > 1)
        work[j] = B3DMAX(0., work[j]);
      else if (work[j] < 0)
        work[j] = -work[j];
      if (work[j] > criterion)
        numOut++;
    }

    /* If there are too many out, need to adjust criterion to limit that number */
    if (numOut > maxZeroWgt) {
      for (j = 0; j < ndata; j++)
        work[ndata + j] = work[j];
      rsSortFloats(&work[ndata], ndata);
      diff = criterion;
      criterion = (work[2 * ndata - maxZeroWgt] + work[2 * ndata - maxZeroWgt - 1]) / 2.;
      if (report)
        printf("%d with zero weight, revising criterion from %g to %g\n", numOut,
               diff, criterion);
      keepCriterion = 1;
    }

    numOut = 0;
    for (j = 0; j < ndata; j++) {

      /* Tests to handle case of MADN zero; avoid division by 0 */
      if (work[j] > criterion) {
        weight = 0.;
        numOut++;
      } else if (work[j] <= 1.e-6 * criterion) {
        weight = 1.;
      } else {
        dev = work[j] / criterion;
        weight = (1 - dev * dev) * (1 - dev * dev);
      }
      if (report) {
        if (weight > 0 && weight <= 0.1)
          num1++;
        if (weight > 0.1 && weight <= 0.2)
          num2++;
        if (weight < 0.5)
          num5++;
      }

      /* Get differences from last time and from time before that */
      diff = fabs(weight - XRC(j, wgtcol));
      diffsum += diff;
      diffmax = B3DMAX(diffmax, diff);
      prev = fabs(weight - XRC(j, prevCol));
      prevmax = B3DMAX(prevmax, prev);
      XRC(j, prevCol) = XRC(j, wgtcol);
      XRC(j, wgtcol) = weight;
    }
    if (report) {
      printf("Iter %3d del mean %.4f max %.4f prev %.4f # 0, 0.1, 0.2, <0.5: %d %d %d "
             "%d\n", iter, diffsum / ndata, diffmax, prevmax, numOut, num1, num2, num5);
      fflush(stdout);
    }

    /* Quit if difference from last time is below limit, or if it is below the oscillation
       limit and difference from the previous time is below limit */
    if (!splitLastTime && (diffmax < maxChange || 
                           (diffmax < maxOscill && prevmax < maxChange)))
      break;

    /* But if we are oscillating, try half-way between and defer testing for 2 rounds */
    if (!splitLastTime && prevmax < maxChange / 2.) {
      splitLastTime = 1;
      if (report) {
        printf("        Oscillation detected: averaging previous weights\n");
        fflush(stdout);
      }
      for (j = 0; j < ndata; j++)
        XRC(j, wgtcol) = (XRC(j, wgtcol) + XRC(j, prevCol)) / 2.;
    } else if (splitLastTime == 1)
      splitLastTime = 2;
    else
      splitLastTime = 0;
  }

  *numIter = iter;
  return (*numIter < maxIter ? 0 : -1);
}

/* Fortran wrapper */
int robustregress(float *x, int *xsize, int *colFast, int *m, int *ndata, int *nbcol, 
                  float *b, int *bsize, float *c, float *xm, float *sd, float *work,
                  float *kfactor, int *numIter, int *maxIter, int *maxZeroWgt, 
                  float *maxChange, float *maxOscillate)
{
  return robustRegress(x, *xsize, *colFast, *m, *ndata, *nbcol, b,
                       *bsize, c, xm, sd, work, *kfactor, numIter, 
                       *maxIter, *maxZeroWgt, *maxChange, *maxOscillate);
}

