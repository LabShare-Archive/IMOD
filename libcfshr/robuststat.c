/*  robuststat.c - Functions for robust statistics
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "imodconfig.h"
#include "cfsemshare.h"

#ifdef F77FUNCAP
#define rssortfloats RSSORTFLOATS
#define rssortindexedfloats RSSORTINDEXEDFLOATS
#define rsmedianofsorted RSMEDIANOFSORTED
#define rsmedian RSMEDIAN
#define rsfastmedian RSFASTMEDIAN
#define rsfastmadn RSFASTMADN
#define rsfastmedianinplace RSFASTMEDIANINPLACE
#define rstrimmedmeanofsorted RSTRIMMEDMEANOFSORTED
#define rstrimmedmean RSTRIMMEDMEAN
#define rsmadn RSMADN
#define rsmadmedianoutliers RSMADMEDIANOUTLIERS
#else
#define rssortfloats rssortfloats_
#define rssortindexedfloats rssortindexedfloats_
#define rsmedianofsorted rsmedianofsorted_
#define rsmedian rsmedian_
#define rsfastmedian rsfastmedian_
#define rsfastmadn rsfastmadn_
#define rsfastmedianinplace rsfastmedianinplace_
#define rstrimmedmeanofsorted rstrimmedmeanofsorted_
#define rstrimmedmean rstrimmedmean_
#define rsmadn rsmadn_
#define rsmadmedianoutliers rsmadmedianoutliers_
#endif

static int intCompar(const void *val1, const void *val2)
{
  int *v1 = (int *)val1;
  int *v2 = (int *)val2;
  if (*v1 < *v2)
    return -1;
  else if (*v1 > *v2)
    return 1;
  return 0;
}

/*!
 * Uses {qsort} to sort the [n] ints in the array [x].
 */
void rsSortInts(int *x, int n)
{
  qsort(x, n, sizeof(int), intCompar);
}

/*!
 * Fortran wrapper for @rsSortInts
 */
void rssortints(int *x, int *n)
{
  rsSortInts(x, *n);
}

static int floatCompar(const void *val1, const void *val2)
{
  float *v1 = (float *)val1;
  float *v2 = (float *)val2;
  if (*v1 < *v2)
    return -1;
  else if (*v1 > *v2)
    return 1;
  return 0;
}

/*!
 * Uses {qsort} to sort the [n] floats in the array [x].
 */
void rsSortFloats(float *x, int n)
{
  qsort(x, n, sizeof(float), floatCompar);
}

/*!
 * Fortran wrapper for @rsSortFloats
 */
void rssortfloats(float *x, int *n)
{
  rsSortFloats(x, *n);
}

static float *valArray;
static int indexOffset = 0;
static int indexedFloatCompar(const void *val1, const void *val2)
{
  int i1 = *((int *)val1) - indexOffset;
  int i2 = *((int *)val2) - indexOffset;
  if (valArray[i1] < valArray[i2])
    return -1;
  else if (valArray[i1] > valArray[i2])
    return 1;
  return 0;
}


/*!
 * Uses {qsort} to sort indexes in [index] to the [n] floats in the array [x].  Indexes 
 * will be used directly when calling from C.  The routine stores a static pointer to 
 * the array so it is not thread-safe.
 */
void rsSortIndexedFloats(float *x, int *index, int n)
{  
  valArray = x;
  qsort(index, n, sizeof(int), indexedFloatCompar);
}

/*!
 * Fortran wrapper for @@rsSortIndexedFloats@.  Indexes will be reduced by one to index 
 * the array in the C comparison routine, so they can still be Fortran indexes.
 */
void rssortindexedfloats(float *x, int *index, int *n)
{
  indexOffset = 1;
  rsSortIndexedFloats(x, index, *n);
  indexOffset = 0;
}


/*!
 * Computes the median of the [n] values in the array [x], which has already 
 * been sorted, and returns the value in [median].
 */
void rsMedianOfSorted(float *x, int n, float *median)
{
  if (n % 2)
    *median = x[n / 2];
  else
    *median = 0.5f * (x[n / 2 - 1] + x[n / 2]);
}

/*!
 * Fortran wrapper for @rsMedianOfSorted
 */
void rsmedianofsorted(float *x, int *n, float *median)
{
  rsMedianOfSorted(x, *n, median);
}

/*!
 * Computes the median of the [n] values in [x].  The value is returned in 
 * [median] and [xsort] is used for sorting the array and returned with the 
 * sorted values.
 */
void rsMedian(float *x, int n, float *xsort, float *median)
{
  memcpy(xsort, x, n * sizeof(float));
  rsSortFloats(xsort, n);
  rsMedianOfSorted(xsort, n, median);
}

/*!
 * Fortran wrapper for @rsMedian
 */
void rsmedian(float *x, int *n, float *xsort, float *median)
{
  rsMedian(x, *n, xsort, median);
}

/*!
 * Computes the median of the [n] values in [x] in linear time.  The value is returned in 
 * [median] and [x] is rearranged by one or two calls to @@percentileFloat@.  This 
 * routine is faster than @rsMedian even for small [n], and much faster for large [n].
 */
void rsFastMedianInPlace(float *x, int n, float *median)
{
  *median = (float)percentileFloat((n + 1) / 2, x, n);
  if (n % 2 == 0)
    *median = (float)((*median + percentileFloat(n / 2 + 1, x, n) )/ 2.);
}

/*! Fortran wrapper for @rsFastMedianInPlace */
void rsfastmedianinplace(float *x, int *n, float *median)
{
  rsFastMedianInPlace(x, *n, median);
}

/*!
 * Computes the median of the [n] values in [x] in linear time.  The value is returned in 
 * [median], and [xjumble] is used for calling @@rsFastMedianInPlace@.
 */
void rsFastMedian(float *x, int n, float *xjumble, float *median)
{
  memcpy(xjumble, x, n * sizeof(float));
  rsFastMedianInPlace(xjumble, n, median);
}

/*! Fortran wrapper for @rsFastMedian */
void rsfastmedian(float *x, int *n, float *xjumble, float *median)
{
  rsFastMedian(x, *n, xjumble, median);
}

/*!
 * Computes the normalized median absolute deviation from the median for the
 * [n] values in [x], using the value already computed for the median in 
 * [median].  The result is returned in [MADN], and [tmp] is returned with
 * sorted values of the absolute deviations.
 */
void rsMADN(float *x, int n, float median, float *tmp, float *MADN)
{
  int i;
  for (i = 0; i < n; i++)
    tmp[i] = (float)fabs((double)(x[i] - median));
  rsSortFloats(tmp, n);
  rsMedianOfSorted(tmp, n, MADN);
  (*MADN) /= 0.6745;
}

/*! Fortran wrapper for @rsMADN */
void rsmadn(float *x, int *n, float *median, float *tmp, float *MADN)
{
  rsMADN(x, *n, *median, tmp, MADN);
}

/*!
 * Computes the normalized median absolute deviation from the median for the
 * [n] values in [x] in linear time, using the value already computed for the median in 
 * [median].  The result is returned in [MADN], and [tmp] is used for storing absolute 
 * deviations.
 */
void rsFastMADN(float *x, int n, float median, float *tmp, float *MADN)
{
  int i;
  for (i = 0; i < n; i++)
    tmp[i] = (float)fabs((double)(x[i] - median));
  rsFastMedianInPlace(tmp, n, MADN);
  (*MADN) /= 0.6745;
}

/*! Fortran wrapper for @rsFastMADN */
void rsfastmadn(float *x, int *n, float *median, float *tmp, float *MADN)
{
  rsFastMADN(x, *n, *median, tmp, MADN);
}

/*!
 * Selects outliers among the [n] values in the array [x] by testing whether
 * the absolute deviation from the median is greater than normalized median
 * absolute deviation by the criterion [kcrit].  [out] is used for temporary
 * storage and is returned with -1 or 1 for outliers in the negative or
 * positive direction from the median, 0 otherwise.
 */
void rsMadMedianOutliers(float *x, int n, float kcrit, float *out)
{
  int i;
  float median, madn;
  rsFastMedian(x, n, out, &median);
  rsFastMADN(x, n, median, out, &madn);
  for (i = 0; i < n; i++) {
    if (fabs((double)(x[i] - median)) / madn > kcrit)
      out[i] = x[i] > median ? 1. : -1.;
    else
      out[i] = 0.;
  }
}

/*!
 * Fortran wrapper for @rsMadMedianOutliers
 */
void rsmadmedianoutliers(float *x, int *n, float *kcrit, float *out)
{
  rsMadMedianOutliers(x, *n, *kcrit, out);
}

/*!
 * Computes a trimmed mean of the [n] already sorted values in [x], trimming 
 * off the fraction [gamma] on each end of the distribution.  The value is 
 * returned in [trmean].
 */
void rsTrimmedMeanOfSorted(float *x, int n, float gamma, float *trmean)
{
  int i;
  int cut = (int)(gamma * n);
  double sum = 0.;
  *trmean = 0.;
  for (i = cut; i < n - cut; i++)
    sum += x[i];
  if (sum != 0.)
    *trmean = sum / (n - 2 * cut);
}

/*!
 * Fortran wrapper for @rsTrimmedMeanOfSorted
 */
void rstrimmedmeanofsorted(float *x, int *n, float *gamma, float *median)
{
  rsTrimmedMeanOfSorted(x, *n, *gamma, median);
}

/*!
 * Computes a trimmed mean of the [n] values in [x], trimming off the fraction 
 * [gamma] on each end of the distribution.  The value is returned in 
 * [trmean] and [xsort] is used for sorting the array and returned with the 
 * sorted values.
 */
void rsTrimmedMean(float *x, int n, float gamma, float *xsort, float *trmean)
{
  memcpy(xsort, x, n * sizeof(float));
  rsSortFloats(xsort, n);
  rsTrimmedMeanOfSorted(xsort, n, gamma, trmean);
}

/*!
 * Fortran wrapper for @rsTrimmedMean
 */
void rstrimmedmean(float *x, int *n, float *gamma, float *xsort, float *median)
{
  rsTrimmedMean(x, *n, *gamma, xsort, median);
}
