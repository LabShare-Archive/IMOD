/*  robuststat.c - Functions for robust statistics
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 * Log at end of file
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
#define rsmadn RSMADN
#define rsmadmedianoutliers RSMADMEDIANOUTLIERS
#else
#define rssortfloats rssortfloats_
#define rssortindexedfloats rssortindexedfloats_
#define rsmedianofsorted rsmedianofsorted_
#define rsmedian rsmedian_
#define rsmadn rsmadn_
#define rsmadmedianoutliers rsmadmedianoutliers_
#endif

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
static int indexedFloatCompar(const void *val1, const void *val2)
{
  int *i1 = (int *)val1;
  int *i2 = (int *)val2;
  if (valArray[*i1] < valArray[*i2])
    return -1;
  else if (valArray[*i1] > valArray[*i2])
    return 1;
  return 0;
}


/*!
 * Uses {qsort} to sort indexes in [index] to the [n] floats in the array [x].
 */
void rsSortIndexedFloats(float *x, int *index, int n)
{  
  valArray = x;
  qsort(index, n, sizeof(int), indexedFloatCompar);
}

/*!
 * Fortran wrapper for @rsSortIndexedFloats
 */
void rssortindexedfloats(float *x, int *index, int *n)
{
  rsSortIndexedFloats(x, index, *n);
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

/*!
 * Fortran wrapper for @rsMADN
 */
void rsmadn(float *x, int *n, float *median, float *tmp, float *MADN)
{
  rsMADN(x, *n, *median, tmp, MADN);
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
  rsMedian(x, n, out, &median);
  rsMADN(x, n, median, out, &madn);
  for (i = 0; i < n; i++) {
    if (out[i] / madn > kcrit)
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

/*

$Log$
Revision 1.1  2009/11/21 21:15:10  mast
Added to package


*/
