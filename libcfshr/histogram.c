/*  histogram.c - Functions for making and analyzing mostly kernel histograms
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <math.h>
#include "imodconfig.h"
#include "b3dutil.h"

#ifdef F77FUNCAP
#define kernelhistogram KERNELHISTOGRAM
#define scanhistogram SCANHISTOGRAM
#define findhistogramdip FINDHISTOGRAMDIP
#else
#define kernelhistogram kernelhistogram_
#define scanhistogram scanhistogram_
#define findhistogramdip findhistogramdip_
#endif

/*!
 * Computes a standard or kernel histogram from the [numVals] values in the 
 * array [values].  The histogram is placed in the array
 * [bins], and occupies [numBins] between [firstVal] and [lastVal].  
 * The kernel half-width is set by [h], where a triweight kernel with shape 
 * (1-(x/h)^2)^3 is added at each point.  Use 0 for a standard binned histogram.
 * Set [verbose] to 1 for a list of values, or 2 for a list of bin values.
 */
void kernelHistogram(float *values, int numVals, float *bins, int numBins,
                     float firstVal, float lastVal, float h, int verbose)
{
  float dxbin, delta, val;
  int i, j, ist, ind;
  dxbin = (lastVal - firstVal) / numBins;
  for (i = 0; i < numBins; i++)
    bins[i] = 0.;
  for (j = 0; j < numVals; j++) {
    val = values[j];

    if (verbose == 1)
      printf("Value: %.4f\n", val);
    if (h) {
      ist = (int)ceil((double)(val - h - firstVal) / dxbin);
      ind = (int)floor((double)(val + h - firstVal) / dxbin);
      ist = B3DMAX(0, ist);
      ind = B3DMIN(numBins - 1, ind);
      for (i = ist; i <= ind; i++) {
        delta = (val - firstVal - i * dxbin) / h;
        bins[i] += (float)pow(1. - delta * delta, 3.);
      }
    } else {
      ist = (int)floor((double)(val - firstVal) / dxbin);
      if (ist >= 0 && ist < numBins)
        bins[ist]++;
      else if (ist == numBins && val - lastVal < 0.001 * dxbin)
        bins[numBins - 1]++;
    }
  }   
  if (verbose == 2)
    for (i = 0; i < numBins; i++)
      printf("bin: %.4f %f\n", firstVal + i * dxbin, bins[i]);
}

/*!
 * Fortran wrapper for @kernelHistogram
 */
void kernelhistogram(float *values, int *numVals, float *bins, int *numBins,
                     float *firstVal, float *lastVal, float *h, int *verbose)
{
  kernelHistogram(values, *numVals, bins, *numBins, *firstVal, *lastVal, *h,
                  *verbose);
}

/*! 
 * Scans a histogram for peaks and a dip.  The histogram is in [numBins] 
 * elements of [bins], extending from [firstVal] to [lastVal].  It will be 
 * scanned between values [scanBot] and [scanTop].  If [findPeaks] is nonzero,
 * it will find the two highest peaks and return their values in [peakBelow] 
 * and [peakAbove], then find the dip between them and returns its value in
 * [dip]; otherwise it just scans the range for the lowest point and returns
 * the value in [dip].  The return value is 1 if it fails to find two peaks.
 */
int scanHistogram(float *bins, int numBins, float firstVal, float lastVal,
                  float scanBot, float scanTop, int findPeaks, float *dip,
                  float *peakBelow, float *peakAbove)
{
  float dxbin, valmin, first, second, lastPeak;
  int indmin, ind, indstr, indend, rising, indFirst, indSecond, lastRank;
  int lastInd;
  float distCrit = 0.005f * (lastVal - firstVal);
  float diffCrit = 0.001f;
  dxbin = (lastVal - firstVal) / numBins;
  indstr = (int)floor((double)(scanTop - firstVal) / dxbin);
  indstr = B3DMIN(numBins - 1, indstr);
  indend = (int)ceil((double)(scanBot - firstVal) / dxbin);
  indend = B3DMAX(0, indend);

  // Seek a peak value first if flag set - this assumes smoothness
  if (findPeaks) {

    first = -1.;
    second = -1.;
    rising = 1;
    lastRank = 0;
    lastInd = numBins;
    lastPeak = 0.;
      
    for (ind = indstr - 1; ind >= indend; ind--) {
      if (rising) {

        // If rising, check for decreasing value; then previous is a peak
        if (bins[ind] < bins[ind+1] || ind == indend) {

          if (lastRank && dxbin * (lastInd - ind - 1) < distCrit &&
              fabs((double)(lastPeak - bins[ind+1])) < diffCrit * lastPeak) {

            // If close to last peak in height and distance, either ignore it
            // or replace with it
            if (lastPeak < bins[ind+1]) {
              lastPeak = bins[ind+1];
              lastInd = ind + 1;
              if (lastRank == 1) {
                first = lastPeak;
                indFirst = ind + 1;
                //printf("Top peak replaced %d  %f\n", indFirst, first);
              } else {
                second = lastPeak;
                indSecond = ind + 1;
                //printf("Lower peak replaced %d  %f\n", indSecond, second);
              }
            }
          } else if (bins[ind+1] > first) {

            // If higher than first peak, roll it down to second one
            second = first;
            indSecond = indFirst;
            first = bins[ind+1];
            indFirst = ind + 1;
            //printf("Top peak at %d  %f\n", indFirst, first);
            lastPeak = first;
            lastInd = indFirst;
            lastRank = 1;

          } else if (bins[ind+1] > second) {

            // Or if higher than second, replace it
            indSecond = ind + 1;
            second = bins[ind+1];
            //printf("Lower peak at %d  %f\n", indSecond, second);
            lastPeak = second;
            lastInd = indSecond;
            lastRank = 2;
          }
          rising = 0.;
        }

        // Otherwise check whether rising yet
      } else if (bins[ind] > bins[ind+1])
        rising = 1;
    }
    
    if (second < 0.)
      return 1;
    indstr = B3DMAX(indFirst, indSecond);
    indend = B3DMIN(indFirst, indSecond);
    *peakAbove = firstVal + indstr * dxbin;
    *peakBelow = firstVal + indend * dxbin;
  }

  // Otherwise, find global minimum in the range
  indmin = indstr;
  valmin = bins[indstr];
  for (ind = indstr; ind >= indend; ind--) {
     if (bins[ind] < valmin) {
       valmin = bins[ind];
       indmin = ind;
     }
  }
  *dip = firstVal + indmin * dxbin;

  return 0;
}

/*!
 * Fortran wrapper for @scanHistogram
 */
int scanhistogram(float *bins, int *numBins, float *firstVal, float *lastVal,
                  float *scanBot, float *scanTop, int *findPeaks, float *dip,
                  float *peakBelow, float *peakAbove)
{
  return scanHistogram(bins, *numBins, *firstVal, *lastVal, *scanBot, *scanTop,
                       *findPeaks, dip, peakBelow, peakAbove);
}

/*!
 * Finds a histogram dip by starting with a high smoothing and dropping to a
 * lower one.  ^
 * [values] - Array of values to form histogram from ^
 * [numVals] - Number of values in the array ^
 * [bins] - Array to build histogram in ^
 * [numBins] - Number of bins to divide histogram into ^
 * [firstVal], [lastVal] - Starting and ending value for histogram range ^
 * [minGuess] - If non-zero, it specifies an estimate of the minimum number of 
 * items above the dip ^
 * [verbose] - Verbose output flag, passed to @kernelHistogram
 * [histDip] - Returned with the location of the dip ^
 * [peakBelow], [peakAbove] - Returned with the peak values below and above 
 * the dip ^
 * The return value is 1 if a dip cannot be found after 4 trial H values.
 */
int findHistogramDip(float *values, int numVals, int minGuess, float *bins,
                     int numBins, float firstVal, float lastVal, 
                     float *histDip, float *peakBelow, float *peakAbove,
                     int verbose)
{
  float range = lastVal - firstVal;
  float coarseH = 0.2f * range;
  float fineH = 0.05f * range;
  int numCut = 4;
  float fracGuess = 0.5f;
  int i, ncum, numCrit;
  float upperLim = 1.0;

  // Build a regular histogram first and use minGuess to find safe upper limit
  kernelHistogram(values, numVals,
                  bins, numBins, firstVal, lastVal, 0., verbose);
  if (minGuess) {
    numCrit = B3DMAX(1, B3DNINT(minGuess * fracGuess));
    ncum = 0;
    for (i = numBins - 1; i > 10; i--) {
      ncum += B3DNINT(bins[i]);
      if (ncum >= numCrit)
        break;
    }
    upperLim = i / (numBins - 1.);
  }
  
  // Seek a kernel width that gives two peaks in histogram
  for (i = 0; i < numCut; i++) {
    kernelHistogram(values, numVals, bins, numBins, firstVal, lastVal, coarseH,
                    verbose);

    // Cut H if it fails or if the top peak is at 1.0
    if (!scanHistogram(bins, numBins, firstVal, lastVal, 0., upperLim, 1,
                       histDip, peakBelow, peakAbove) && 
        *peakAbove < firstVal + 0.999 * range) 
      break;
    coarseH *= 0.707;
  }
  if (i == numCut)
    return 1;
  
  printf("Histogram smoothed with H = %.3f has dip at %g, peaks at %g"
         " and %g\n", coarseH, *histDip, *peakBelow, *peakAbove);
  
  kernelHistogram(values, numVals, bins, numBins, firstVal, lastVal, fineH,
                  verbose);
  scanHistogram(bins, numBins, firstVal, lastVal, 
                0.5 * (*histDip + *peakBelow), 0.5 * (*histDip + *peakAbove),
                0, histDip, peakBelow, peakAbove);
  printf("Histogram smoothed with H = %g has lowest dip at %g\n",
         fineH, *histDip);
  fflush(stdout);
  return 0;
}

/*!
 * Fortran wrapper for @findHistogramDip
 */
int findhistogramdip(float *values, int *numVals, int *minGuess, float *bins,
                     int *numBins, float *firstVal, float *lastVal, 
                     float *histDip, float *peakBelow, float *peakAbove,
                     int *verbose)
{
  return findHistogramDip(values, *numVals, *minGuess, bins, *numBins, 
                          *firstVal, *lastVal, histDip, peakBelow, peakAbove,
                          *verbose);
}
