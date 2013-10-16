/*
 * filtxcorr.c - Functions for filtering and cross-correlation
 *
 * Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 */

#include <math.h>
#include <string.h>
#include "imodconfig.h"
#include "b3dutil.h"

#ifdef F77FUNCAP
#define setctfwsr SETCTFWSR
#define setctfnoscl SETCTFNOSCL
#define filterpart FILTERPART
#define xcorrpeakfind XCORRPEAKFIND
#define xcorrpeakfindwidth XCORRPEAKFINDWIDTH
#define meanzero MEANZERO
#define parabolicfitposition PARABOLICFITPOSITION
#define cccoefficient CCCOEFFICIENT
#define conjugateproduct CONJUGATEPRODUCT
#define slicegaussiankernel SLICEGAUSSIANKERNEL
#define scaledgaussiankernel SCALEDGAUSSIANKERNEL
#define applykernelfilter APPLYKERNELFILTER
#define wrapfftslice WRAPFFTSLICE
#define setpeakfindlimits SETPEAKFINDLIMITS
#else
#define setctfwsr setctfwsr_
#define setctfnoscl setctfnoscl_
#define filterpart filterpart_
#define xcorrpeakfind xcorrpeakfind_
#define xcorrpeakfindwidth xcorrpeakfindwidth_
#define meanzero meanzero_
#define parabolicfitposition parabolicfitposition_
#define cccoefficient cccoefficient_
#define conjugateproduct conjugateproduct_
#define slicegaussiankernel slicegaussiankernel_
#define scaledgaussiankernel scaledgaussiankernel_
#define applykernelfilter applykernelfilter_
#define wrapfftslice wrapfftslice_
#define setpeakfindlimits setpeakfindlimits_
#endif

static float peakHalfWidth(float *array, int baseInd, int cenInd, int stride, int size, 
                           int direction);

/*!
 * Takes the filter parameters [sigma1], [sigma2], [radius1], and [radius2] and
 * sets up a contrast transfer (filter) function in the array [ctf], which 
 * should be dimensioned to at least 8193.  The dimensions of the real space 
 * image being filtered are specified in [nx] and [ny], and the step size 
 * between the values in [ctf] is returned in [delta].  If no filtering
 * is selected ([sigma1] and [sigma2] both 0) then 0 is returned in [delta].
 * The values in [ctf] are scaled so that mean of all but the zero element is 
 * 1.
 */
void XCorrSetCTF(float sigma1, float sigma2, float radius1, float radius2,
                 float *ctf, int nx, int ny, float *delta)
{
  int nsize, j;
  float sum, scl;
  XCorrSetCTFnoScl(sigma1, sigma2, radius1, radius2, ctf, nx, ny, delta,
                   &nsize);
  if (!(*delta))
    return;
  sum = 0.0;
  for (j = 1; j < nsize; j++)
    sum = sum + ctf[j];
  
  scl = (nsize - 1) / sum;
  for (j = 1; j < nsize; j++)
    ctf[j] = ctf[j] * scl;
}

/*!
 * Fortran wrapper for @XCorrSetCTF
 */
void setctfwsr(float *sigma1, float *sigma2, float *radius1, float *radius2,
               float *ctf, int *nx, int *ny, float *delta)
{
  XCorrSetCTF(*sigma1, *sigma2, *radius1, *radius2, ctf, *nx, *ny, delta);
}

/*!
 * Sets up a filter function in [ctf] as described for @XCorrSetCTF, but with
 * no scaling, such that the filter value will be 1 for all unattenuated 
 * frequencies.  The number of elements computed in [ctf] is returned in 
 * [nizeOut].
 */
void XCorrSetCTFnoScl(float sigma1, float sigma2, float radius1, float radius2,
                      float *ctf, int nx,int ny, float *delta, int *nsizeOut) 
{
  double beta1, beta2, alpha;
  float asize, s, ssqrd, radius1p, radius1n, deltmp,delmax;
  int nsize, j;
  
  *delta=0.;
  if(sigma1 == 0. && sigma2 == 0.)
    return;
  
  alpha = 0.0;
  beta1 = 0.0;
  beta2 = 0.0;
  delmax = 0.;
  nsize = 1024;
  if (2 * nx > nsize)
    nsize = 2 * nx;
  if (2 * ny > nsize)
    nsize = 2 * ny;
  if (nsize > 8192)
    nsize = 8192;
  
  asize = (float)nsize;
  nsize = nsize + 1;
  if ((sigma1 >= 0 ? sigma1 : -sigma1) > 1.e-6)
    alpha = -0.5/(sigma1 * sigma1);
  if ((sigma2 >= 0 ? sigma2 : -sigma2) > 1.e-6)
    beta1  = -0.5/(sigma2*sigma2);
  beta2=beta1;

  /* Yes, delta is twice as big as it should be and values are generated
     out to 1.41.  Maybe it should have been 0.71/asize, but since it goes
     past 0.866 it is good for 3D filtering too. */
  *delta = 1.0f/(.71f*asize);
  if (radius1 >= 0.) {
    radius1p = radius1;
    radius1n = 0.;
  } else {
    radius1n = -radius1;
    radius1p = 0.;
  }
  
  /* For negative sigma1, find the maximum to allow scaling to maximum of 1 */
  if (sigma1 < -1.e-6) {
    s=0.;   
    for (j = 0; j < nsize; j++) {
      ssqrd=s*s;
      deltmp=(float)(ssqrd*exp(alpha*ssqrd));
      if (delmax < deltmp)
        delmax = deltmp;
      s = s + *delta;
    }
  }

  s = 0.0;
  for (j = 0; j < nsize; j++) {
    if (s < radius1p) 
      ctf[j] = (float)exp(beta1*(s - radius1p)*(s - radius1p));
    else if (s > radius2)
      ctf[j] = (float)exp(beta2*(s - radius2)*(s - radius2));
    else
      ctf[j] = 1.0;
    
    s = s + *delta;
    if(sigma2 < -1.e-6) ctf[j] = 1.f - ctf[j];
  }
  if (sigma1 > 1.e-6) {
    s=0.;
    for (j = 0; j < nsize; j++) {
      if (s < radius1n)
        ctf[j] = 0.;
      else
        ctf[j] = ctf[j]*(1.0f - (float)exp(alpha*(s-radius1n) * (s-radius1n)));
      s = s + *delta;
    }
  } else if (sigma1 < -1.e-6) {
    s=0.;
    for (j = 0; j < nsize; j++) {
      ssqrd=s*s;
      ctf[j]=(float)(ctf[j]*ssqrd*exp(alpha*ssqrd) / delmax);
      s = s + *delta;
    }
  }

  // Set small numbers to zero to avoid numerical problems and slow inverse
  // FFT's in 3D or 2D
  for (j = 0; j < nsize; j++)
    if (ctf[j] < 1.e-6)
      ctf[j] = 0.;
  *nsizeOut = nsize;
}

/*!
 * Fortran wrapper for @XCorrSetCTFnoScl
 */
void setctfnoscl(float *sigma1, float *sigma2, float *radius1, float *radius2,
                 float *ctf, int *nx, int *ny, float *delta, int *nsize)
{
  XCorrSetCTFnoScl(*sigma1, *sigma2, *radius1, *radius2, ctf, *nx, *ny, delta, 
              nsize);
}



/*!
 * Applies the filter in [ctf] to the 2D Fourier transform in [fft], and 
 * puts result into [array], which can be the same as [ctf].  The dimensions of
 * the real space image are given by [nx] and [ny], and the dimension of the
 * real image array are assumed to be [nx] + 2 by [ny].  [delta] is the
 * interval in reciprocal space (1/pixel) for the function in [ctf].
 */
void XCorrFilterPart(float *fft, float *array, int nx, int ny, float *ctf, 
                     float delta)
{
  float x, delx, dely, y, s;
  double y2;
  int ix, iy, index, ind, indp1, indf, nxo2, nx21, nym1;

  nxo2 = nx/2;
  nx21 = nxo2 + 1;
  nym1 = ny - 1;
  delx = (float)(1.0/nx);
  dely = (float)(1.0/ny);

  /*   apply filter function on fft, put result in array */

  index = 0;
  for (iy = 0; iy <= nym1; iy++) {
    y = iy*dely;
    if (y > 0.5)
      y = 1.0f - y;
    y2 = y*y;
    x = 0.0;
    for (ix = 0; ix <= nxo2; ix++) {
      ind = 2 * (index + ix);
      indp1 = ind + 1;
      s = (float)sqrt(x*x + y2);
      indf = (int)(s/delta + 0.5f);
      array[ind] = fft[ind] * ctf[indf];
      array[indp1] = fft[indp1] * ctf[indf];
      x = x + delx;
    }
    index = index + nx21;
  }
  return;
}

/*!
 * Fortran wrapper to @XCorrFilterPart
 */
void filterpart(float *fft, float *array, int *nx, int *ny, float *ctf, 
                     float *delta)
{
  XCorrFilterPart(fft, array, *nx, *ny, ctf, *delta);
}

/*!
 * Shifts the contents of [array] to have a zero mean.  [nxdim] specifies the 
 * X dimension of [array], and [nx] and [ny] are the size of the data in 
 * [array].
 */
void XCorrMeanZero(float *array, int nxdim, int nx, int ny)
{
  int iy, ix, ixbase;
  float dmean, tsum, sum=0.;
  for (iy = 0; iy < ny; iy++) {
    tsum=0.;
    ixbase = iy * nxdim;
    for (ix = 0; ix < nx; ix++)
      tsum += array[ix + ixbase];
    sum += tsum;
  }
  dmean=sum/(nx*ny);
  
  for (iy = 0; iy < ny; iy++) {
    ixbase = iy * nxdim;
    for (ix = 0; ix < nx; ix++)
         array[ix +ixbase] -= dmean;
  }
}

/*!
 * Fortran wrapper to @XCorrMeanZero
 */
void meanzero(float *array, int *nxdim, int *nx, int *ny)
{
  XCorrMeanZero(array, *nxdim, *nx, *ny);
}

static int sApplyLimits = 0;
static int sLimitXlo, sLimitXhi, sLimitYlo, sLimitYhi;

/*!
 * Finds the coordinates of the [maxpeaks] highest peaks in [array], which is dimensioned
 * to [nxdim] by [ny], and returns the positions in [xpeak], [ypeak], and the peak values
 * in [peak].  In addition, if [width] and [widthSD] are not NULL, the distance from the
 * peak to position at half of the peak height is measured in 4 directions and the mean
 * is returned in [width] and the standard deviation in [widthSD].  The X size of the 
 * image is assumed * to be [nxdim] - 2.  The sub-pixel position is determined by fitting
 * a parabola separately in X and Y to the peak and 2 adjacent points.  Positions
 * are numbered from zero and coordinates bigger than half the image size are
 * shifted to be negative.  The positions are thus the amount to shift a second
 * image in a correlation to align it to the first.  If fewer than [maxpeaks]
 * peaks are found, then the remaining values in [peaks] will be -1.e30.
 */
void XCorrPeakFindWidth(float *array, int nxdim, int ny, float  *xpeak, float *ypeak,
                        float *peak, float *width, float *widthSD, int maxpeaks)
{
  float cx, cy, y1, y2, y3, local, val;
  float widthTemp[4];
  int ixpeak, iypeak, ix, iy, ixBlock, iyBlock, ixStart, ixEnd, iyStart, iyEnd;
  int i, j, ixm, ixp, iyb, iybm, iybp, idx, idy;
  int nx = nxdim - 2;
  int blockSize = 5;
  int nBlocksX = (nx + blockSize - 1) / blockSize;
  int nBlocksY = (ny + blockSize - 1) / blockSize;
  float xLimCen, yLimCen, xLimRadSq, yLimRadSq;

  /* If using elliptical limits, compute center and squares of radii */
  if (sApplyLimits < 0) {
    xLimCen = 0.5 * (sLimitXlo + sLimitXhi);
    cx = B3DMAX(1., (sLimitXhi - sLimitXlo) / 2.);
    xLimRadSq = cx * cx;
    yLimCen = 0.5 * (sLimitYlo + sLimitYhi);
    cy = B3DMAX(1., (sLimitYhi - sLimitYlo) / 2.);
    yLimRadSq = cy * cy;
  }

  /* find peaks */
  for (i = 0; i < maxpeaks; i++) {
    peak[i] = -1.e30f;
    xpeak[i] = 0.;
    ypeak[i] = 0.;
  }

  if (maxpeaks < 2) {

    /* Find one peak within the limits */
    if (sApplyLimits) {
      for (iy = 0; iy  < ny; iy++) {
        idy = (iy > ny / 2) ? iy - ny : iy;
        if (idy < sLimitYlo || idy > sLimitYhi)
          continue;
        for (ix = 0; ix < nx; ix++) {
          idx = (ix > nx / 2) ? ix - nx : ix;
          if (idx >= sLimitXlo && idx <= sLimitXhi && array[ix + iy * nxdim] > *peak) {
            if (sApplyLimits < 0) {
              cx = idx - xLimCen;
              cy = idy - yLimCen;
              if (cx * cx / xLimRadSq + cy * cy / yLimRadSq > 1.)
                continue;
            }
            *peak = array[ix + iy * nxdim];
            ixpeak = ix - iy * nxdim;
            iypeak = iy;
          }
        }
      }
      if (*peak > -0.9e30) {
        *xpeak = (float)ixpeak;
        *ypeak = (float)iypeak;
      }

    } else {

      /* Or just find the one peak in the whole area */
      for (iy = 0; iy  < ny; iy++) {
        for (ix = iy * nxdim; ix < nx + iy * nxdim; ix++) {
          if (array[ix] > *peak) {
            *peak = array[ix];
            ixpeak = ix - iy * nxdim;
            iypeak = iy;
          }
        }
      }
      *xpeak = (float)ixpeak;
      *ypeak = (float)iypeak;
    }
  } else {
    
    // Check for local peaks by looking at the highest point in each local 
    // block
    for (iyBlock = 0 ; iyBlock < nBlocksY; iyBlock++) {

      // Block start and end in Y
      iyStart = iyBlock * blockSize;
      iyEnd = iyStart + blockSize;
      if (iyEnd > ny)
        iyEnd = ny;

      // Test if entire block is outside limits
      if (sApplyLimits && (iyStart > ny / 2 || iyEnd <= ny / 2)) {
        idy = (iyStart > ny / 2) ? iyStart - ny : iyStart;
        if (idy > sLimitYhi)
          continue;
        idy = (iyEnd > ny / 2) ? iyEnd - ny : iyEnd;
        if (idy < sLimitYlo)
          continue;
      }

      // Loop on X blocks, get start and end in Y
      for (ixBlock = 0 ; ixBlock < nBlocksX; ixBlock++) {
        ixStart = ixBlock * blockSize;
        ixEnd = ixStart + blockSize;
        if (ixEnd > nx)
          ixEnd = nx;

        // Test if entire block is outside limits
        if (sApplyLimits && (ixStart > nx / 2 || ixEnd <= nx / 2)) {
          idx = (ixStart > nx / 2) ? ixStart - nx : ixStart;
          if (idx > sLimitXhi)
            continue;
          idx = (ixEnd > nx / 2) ? ixEnd - nx : ixEnd;
          if (idx < sLimitXlo)
            continue;
        }
        
        // Loop on every pixel in the block; have to test each pixel
        local = -1.e30f;
        for (iy = iyStart; iy  < iyEnd; iy++) {
          if (sApplyLimits) {
            idy = (iy > ny / 2) ? iy - ny : iy;
            if (idy < sLimitYlo || idy > sLimitYhi)
              continue;
          }
          for (ix = ixStart; ix < ixEnd; ix++) {
            if (sApplyLimits) {
              idx = (ix > nx / 2) ? ix - nx : ix;
              if (idx < sLimitXlo || idx > sLimitXhi)
                continue;

              // Apply elliptical test
              if (sApplyLimits < 0) {
                cx = idx - xLimCen;
                cy = idy - yLimCen;
                if (cx * cx / xLimRadSq + cy * cy / yLimRadSq > 1.)
                  continue;
              }
            }
            val = array[ix + iy * nxdim];
            if (val > local && val > peak[maxpeaks - 1]) {
              local = val;
              ixpeak = ix;
              iypeak = iy;
            }
          }
        }
        
        // evaluate local peak for truly being local
        if (local > -0.9e30) {
          ixm = (ixpeak + nx - 1) % nx;
          ixp = (ixpeak + 1) % nx;
          iyb = iypeak * nxdim;
          iybp = ((iypeak + 1) % ny) * nxdim;
          iybm = ((iypeak + ny - 1) % ny) * nxdim;

          if (local > array[ixpeak + iybm] && local > array[ixpeak + iybp] &&
              local > array[ixm + iyb] && local > array[ixm + iybm] &&
              local > array[ixm + iybp] && local > array[ixp + iyb] &&
              local > array[ixp + iybm] && local > array[ixp + iybp]){
            
            // Insert peak into the list
            for (i = 0; i < maxpeaks; i++) {
              if (peak[i] < local) {
                for (j = maxpeaks - 1; j > i; j--) {
                  peak[j] = peak[j - 1];
                  xpeak[j] = xpeak[j - 1];
                  ypeak[j] = ypeak[j - 1];
                }
                peak[i] = local;
                xpeak[i] = (float)ixpeak;
                ypeak[i] = (float)iypeak;
                break;
              }
            }
          }
        }
      }
    }
  }
      
  for (i = 0; i < maxpeaks; i++) {
    if (peak[i] < -0.9e30)
      continue;

    // Add 0.2 just in case float was less than int assigned to it
    ixpeak = (int)(xpeak[i] + 0.2);
    iypeak = (int)(ypeak[i] + 0.2);
    
    /* simply fit a parabola to the two adjacent points in X or Y */
    
    y1=array[(ixpeak + nx - 1) % nx + iypeak * nxdim];
    y2=peak[i];
    y3=array[(ixpeak + 1) % nx + iypeak * nxdim];
    cx = (float)parabolicFitPosition(y1, y2, y3);

    y1=array[ixpeak + ((iypeak + ny - 1) % ny) * nxdim];
    y3=array[ixpeak + ((iypeak + 1) % ny) * nxdim];
    cy = (float)parabolicFitPosition(y1, y2, y3);
    
    /*    return adjusted pixel coordinate */
    xpeak[i] = ixpeak + cx;
    ypeak[i] = iypeak + cy;
    if(xpeak[i] > nx/2)
      xpeak[i] = xpeak[i] - nx;
    if(ypeak[i] > ny/2)
      ypeak[i] = ypeak[i] - ny;

    /* Return width if non-NULL */
    if (width && widthSD) {
      width[i] = (widthTemp[0] + widthTemp[0] + widthTemp[0] + widthTemp[3]) / 4.;
      widthTemp[0] = peakHalfWidth(array, iypeak * nxdim, ixpeak, 1, nx, 1) - cx;
      widthTemp[1] = peakHalfWidth(array, iypeak * nxdim, ixpeak, 1, nx, -1) + cx;
      widthTemp[2] = peakHalfWidth(array, ixpeak, iypeak, nxdim, ny, 1) - cy;
      widthTemp[3] = peakHalfWidth(array, ixpeak, iypeak, nxdim, ny, -1) + cy;
      avgSD(widthTemp, 4, &width[i], &widthSD[i], &cy);
    }
  }
  sApplyLimits = 0;
}

/* Find the point in one direction away from the peak pixel where it falls by half */
static float peakHalfWidth(float *array, int baseInd, int cenInd, int stride, int size, 
                           int direction)
{
  float peak = array[baseInd + cenInd * stride];
  int dist;
  float lastVal = peak, val;
  for (dist = 1; dist < size / 4; dist++) {
    val = array[baseInd + ((cenInd + direction * dist + size) % size) * stride];
    if (val < peak / 2.)
      return (float)(dist + (lastVal - peak / 2.) / (lastVal - val) - 1.);
    lastVal = val;
  }
  return (float)dist;
}

/*!
 * Fortran wrapper to @@XCorrPeakFindWidth@.  If [maxpeaks] is 1, then [xpeak], 
 * [ypeak], and [peak] can be single variables instead of arrays.
 */
void xcorrpeakfindwidth(float *array, int *nxdim, int *ny, float  *xpeak, float *ypeak,
                        float *peak, float *width, float *widthSD, int *maxpeaks)
{
  XCorrPeakFindWidth(array, *nxdim, *ny, xpeak, ypeak, peak, width, widthSD, *maxpeaks);
}

/*!
 * Calls @@XCorrPeakFindWidth@ with [width] and [widthSD] NULL.
 */
void XCorrPeakFind(float *array, int nxdim, int ny, float  *xpeak,
                        float *ypeak, float *peak, int maxpeaks)
{
  XCorrPeakFindWidth(array, nxdim, ny, xpeak, ypeak, peak, NULL, NULL, maxpeaks);
}

/*!
 * Fortran wrapper to @@XCorrPeakFind@.  If [maxpeaks] is 1, then [xpeak], 
 * [ypeak], and [peak] can be single variables instead of arrays.
 */
void xcorrpeakfind(float *array, int *nxdim, int *ny, float  *xpeak,
                   float *ypeak, float *peak, int *maxpeaks)
{
  XCorrPeakFindWidth(array, *nxdim, *ny, xpeak, ypeak, peak, NULL, NULL, *maxpeaks);
}

/*!
 * Sets limits for shifts in X and in Y for peaks found in the next call to
 * @@XCorrPeakFindWidth@.  The peak position must be between [limXlo] and [limXhi] in X
 * and between [limYlo] and [limYhi] in Y.  Also, if [useEllipse] is non-zero, the 
 * peaks are constrained to be in the ellipse bounded by these limits (i.e., a circle
 * if the range of limts is the same in X and Y).
 */
void setPeakFindLimits(int limXlo, int limXhi, int limYlo, int limYhi, int useEllipse)
{
  sApplyLimits = useEllipse ? -1 : 1;
  sLimitXlo = limXlo;
  sLimitXhi = limXhi;
  sLimitYlo = limYlo;
  sLimitYhi = limYhi;
}

/*!
 * Fortran wrapper to @@setPeakFindLimits@.
 */
void setpeakfindlimits(int *limXlo, int *limXhi, int *limYlo, int *limYhi, 
                       int *useEllipse)
{
  setPeakFindLimits(*limXlo, *limXhi, *limYlo, *limYhi, *useEllipse);
}

/*!
 * Given values at three successive positions, [y1], [y2], and [y3], where
 * [y2] is the peak value, this fits a parabola to the values and returns the
 * offset of the peak from the center position, a number between -0.5 and 0.5.
 */
double parabolicFitPosition(float y1, float y2, float y3)
{
  double denom, cx = 0.;
  denom=2.f*(y1+y3-2.f*y2);
  
  /* y2 is the highest, so in the absence of numerical error due to numbers
     being very close to each other, the peak must be in the interval.
     This seems better than testing if zero or testing against 1.e-6 */
  if (fabs(denom) > fabs(1.e-2 * (y1-y3)))
    cx=(y1-y3)/denom;
  if (cx > 0.5)
    cx = 0.5;
  if (cx < -0.5)
    cx = -0.5;
  return cx;
}
/*!
 * Fortran wrapper to @parabolicFitPosition
 */
double parabolicfitposition(float *y1, float *y2, float *y3)
{
  return (parabolicFitPosition(*y1, *y2, *y3));
}

/*!
 * Forms the product of complex numbers in [array] and the complex conjugate of
 * numbers in [brray] and puts the result back in [array], where the arrays
 * are Fourier transforms of images with dimensions [nx] by [ny].
 */
void conjugateProduct(float *array, float *brray, int nx, int ny)
{
  int jx, jp1;
  float a, b, c, d;
  for (jx = 0; jx < ny*(nx+2); jx += 2) {
    jp1 = jx + 1;
    a = array[jx];
    b = array[jp1];
    c = brray[jx];
    d = brray[jp1];
    
    array[jx] = a * c + b * d;
    array[jp1] = b * c - a * d;
  }
}

/*!
 * Fortran wrapper to @conjugateProduct
 */
void conjugateproduct(float *array, float *brray, int *nx, int *ny)
{
  conjugateProduct(array, brray, *nx, *ny);
}

/*!
 * Returns the cross-correlation coefficient between the images in [array] and
 * [brray] at a shift between the images given by [xpeak], [ypeak].  The images
 * have sizes [nx] by [ny] and the arrays have X dimension [nxdim].  Areas on
 * each side of [nxpadA] in X and [nypadA] in Y in [array], and of [nxpadB] and [nypadB]
 * in [brray], are excluded from the 
 * correlation.  Otherwise, the correlation will include all the pixels in the 
 * overlap between the images at the given shift.  The number of pixels will
 * be returned in [nsum], but no computation will be done and 0. will be returned if
 * this number is less than [minPixels].
 */
double CCCoefficientTwoPads(float *array, float *brray, int nxdim, int nx, int ny,
                            float xpeak, float ypeak, int nxpadA, int nypadA, int nxpadB, 
                            int nypadB, int minPixels, int *nsum)
{
  double asum, bsum, csum, asumsq, bsumsq, ccc, aval, bval;
  int delx, dely, xstrt, xend, ystrt, yend, ix, iy;
  delx = (int)floor(xpeak + 0.5);
  xstrt = B3DMAX(nxpadA, nxpadB + delx);
  xend = B3DMIN(nx - nxpadA, nx - nxpadB + delx);
  dely = (int)floor(ypeak + 0.5);
  ystrt = B3DMAX(nypadA, nypadB + dely);
  yend = B3DMIN(ny - nypadA, ny - nypadB + dely);
  asum = bsum = csum = asumsq = bsumsq = 0.;
  *nsum = B3DMAX(0, xend + 1 - xstrt) * B3DMAX(0, yend + 1 - ystrt);
  if (xend < xstrt || yend < ystrt || *nsum < minPixels)
    return 0.;
  for (iy = ystrt; iy < yend; iy++) {
    for (ix = xstrt; ix < xend; ix++) {
      aval = array[ix + iy * nxdim];
      bval = brray[ix - delx + (iy - dely) * nxdim];
      asum += aval;
      asumsq += aval * aval;
      bsum += bval;
      bsumsq += bval * bval;
      csum += aval * bval;
    }
  }
  ccc = (*nsum * asumsq - asum * asum) * (*nsum * bsumsq - bsum * bsum);
  if (ccc <= 0.)
    return 0.;
  ccc = (*nsum * csum - asum * bsum) / sqrt(ccc);
  return ccc;
}

/*!
 * Calls @CCCoefficientTwoPads with both excluded areas specified by [nxpad], [nypad]
 * and with a value of 25 for [minPixels].
 */
double XCorrCCCoefficient(float *array, float *brray, int nxdim, int nx, int ny,
                          float xpeak, float ypeak, int nxpad, int nypad, int *nsum)
{
  return CCCoefficientTwoPads(array, brray, nxdim, nx, ny, xpeak, ypeak, nxpad, nypad, 
                             nxpad, nypad, 25, nsum);
}

/*!
 * Fortran wrapper to @XCorrCCCoefficient
 */
double cccoefficient(float *array, float *brray, int *nxdim, int *nx, int *ny,
                     float *xpeak, float *ypeak, int *nxpad, int *nypad,
                     int *nsum)
{
return XCorrCCCoefficient(array, brray, *nxdim, *nx, *ny, *xpeak, *ypeak,
                          *nxpad, *nypad, nsum);
}


/*!
 * Fills [mat] with the coefficients of a [dim] x [dim] Gaussian kernel with 
 * standard deviation [sigma].  The coefficients are scaled to sum to 1.
 */
void sliceGaussianKernel(float *mat, int dim, float sigma)
{
  float sum = 0.;
  int i, j;
  double mid = (dim - 1)/ 2.;
  for (j = 0; j < dim; j++) {
    for (i = 0; i < dim; i++) {
      mat[i + j * dim] = (float)exp(-((i-mid) * (i-mid) + (j-mid) * (j-mid)) / 
                                  (sigma * sigma));
      
      sum += mat[i + j * dim];
    }
  }
  for (j = 0; j < dim; j++)
    for (i = 0; i < dim; i++)
      mat[i + j * dim] /= sum;
}

/*! Fortran wrapper for @sliceGaussianKernel */
void slicegaussiankernel(float *mat, int *dim, float *sigma)
{
  sliceGaussianKernel(mat, *dim, *sigma);
}

/*!
 * Fills [mat] with the coefficients of a Gaussian kernel with standard deviation 
 * [sigma].  The size of the kernel is set to 3 for [sigma] up to 1., 5 for [sigma]
 * up to 2., etc., up to the size given by [limit]. The size is returned in [dim].
 * The coefficients are scaled to sum to 1.  Values are placed sequentially in [mat]; 
 * i.e., it is not treated as a 2D array of size [limit].
 */
void scaledGaussianKernel(float *mat, int *dim, int limit, float sigma)
{
  *dim = 2 * (int)ceil((double)sigma) + 1;
  *dim = B3DMIN(limit, *dim);
  sliceGaussianKernel(mat, *dim, sigma);
}

/*! Fortran wrapper for @scaledGaussianKernel */
void scaledgaussiankernel(float *mat, int *dim, int *limit, float *sigma)
{
  scaledGaussianKernel(mat, dim, *limit, *sigma);
}

/*!
 * Applies the kernel in [mat], with the size given in [kdim], to a whole floating point 
 * image in [array] with X and Y sizes [nx] amd [ny] and X dimension [nxdim], and places
 * the output into [brray] with the same X dimension.  Scaling
 * coefficients should be sequential in [mat] with the X dimension advancing fastest.
 * The filtering is parallelized with OpenMP with the same size-dependent limitation on 
 * the number of threads as in @cubinterp
 */
void applyKernelFilter(float *array, float *brray, int nxdim, int nx, int ny, float *mat,
                      int kdim)
{
  int iyo, ixo, iy, ix;
  int below = kdim / 2, above = kdim - 1 - kdim / 2;
  int nregion, ireg, xstr, xend, aix, aiy, numThreads;
  float sum;

  numThreads = B3DNINT(0.04 * sqrt((double)nx * ny));
  numThreads = numOMPthreads(numThreads);
#pragma omp parallel for num_threads(numThreads) \
  shared(kdim, nx, ny, nxdim, array, brray, mat, below, above)    \
  private(iyo, ixo, iy, ix, nregion, ireg, xstr, xend, aix, aiy, sum)
  for (iyo = 0; iyo < ny; iyo++) {
    xstr = 0;
    xend = below;
    nregion = 2;
    if (iyo < below || iyo >= ny - above) {
      nregion = 1;
      xend = nx;
    } else {
      for (ixo = below; ixo < nx - above; ixo++) {
        sum = 0.;
        for (iy = 0; iy < kdim; iy++) {
          aiy = iy + iyo - below;
          for (ix = 0; ix < kdim; ix++) {
            aix = ix + ixo - below;
            sum += mat[ix + iy * kdim] * array[aix + aiy * nxdim];
          }
        }
        brray[ixo + nxdim * iyo] = sum;
      }
    }
    for (ireg = 0; ireg < nregion; ireg++) {
      for (ixo = xstr; ixo < xend; ixo++) {
        sum = 0.;
        for (iy = 0; iy < kdim; iy++) {
          aiy = iy + iyo - below;
          B3DCLAMP(aiy, 0, ny - 1);
          for (ix = 0; ix < kdim; ix++) {
            aix = ix + ixo - below;
            B3DCLAMP(aix, 0, nx - 1);
            sum += mat[ix + iy * kdim] * array[aix + aiy * nxdim];
          }
        }
        brray[ixo + nxdim * iyo] = sum;
      }
      xstr = nx - above;
      xend = nx;
    }
  }
}

/*! Fortran wrapper for @applyKernelFilter */
void applykernelfilter(float *array, float *brray, int *nxdim, int *nx, int *ny,
                      float *mat, int *kdim)
{
  applyKernelFilter(array, brray, *nxdim, *nx, *ny, mat, *kdim);
}

/*!
 * Converts a 2D FFT or Z slice of a 3D FFT  between the organization used by FFT 
 * routines, with the origin at the lower left, and the organization used in MRC files, 
 * with the origin at the middle left.  The complex data are packed in [array] with [nx] 
 * and [ny] complex elements in X and Y, [direction] is 0 to convert from FFT routine 
 * organization to file organization, and non-zero for the reverse.
 */
void wrapFFTslice(float *array, float *tmpArray, int nx, int ny, int direction)
{
  int lineBytes = 2 * nx * sizeof(float);
  int nyHalf = ny / 2;
  int iy, iyOut, iyLow, iyHigh;
  int incDir = indicesForFFTwrap(ny, direction, &iyOut, &iyLow, &iyHigh);
  if (ny % 2) {
    memcpy(tmpArray, &array[2 * nx * iyOut], lineBytes);
    for (iy = 0; iy < nyHalf; iy++, iyOut += incDir, iyLow += incDir, iyHigh += incDir) {
      memcpy(&array[2 * nx * iyOut], &array[2 * nx * iyHigh], lineBytes);
      memcpy(&array[2 * nx * iyHigh], &array[2 * nx * iyLow], lineBytes);
    }
    memcpy(&array[2 * nx * iyOut], tmpArray, lineBytes);
  } else {
    for (iy = 0; iy < nyHalf; iy++, iyLow++, iyHigh++) {
      memcpy(tmpArray, &array[2 * nx * iyLow], lineBytes);
      memcpy(&array[2 * nx * iyLow], &array[2 * nx * iyHigh], lineBytes);
      memcpy(&array[2 * nx * iyHigh], tmpArray, lineBytes);
    }
  }
}

/*! Fortran wrapper for @wrapFFTslice */
void wrapfftslice(float *array, float *tmpArray, int *nx, int *ny, int *direction)
{
  wrapFFTslice(array, tmpArray, *nx, *ny, *direction);
}

/*!
 * Produces indices needed for converting between FFT routine organization and MRC file
 * organization.  The size of the dimension is in [ny] and the direction is 0 for FFT 
 * routine to file, or non-zero otherwise.  Returns the starting index to copy data from 
 * on the low and high ends of the dimension in [iyLow] and [iyHigh], respectively, and 
 * the starting index to copy data from the high end to in [iyOut].  The return value is
 * the increment for these indices.  In all cases, a loop is run [ny] / 2 times.  For [ny]
 * even, the loop simply swaps each element or line between [iyHigh] to [iyLow].  For [ny]
 * odd, first the element at [iyOut] is saved, then the loop is run to copy from [iyHigh]
 * to [iyOut] and from [iyLow] to [iyHigh], then the saved element is copied to [iyOut].
 */
int indicesForFFTwrap(int ny, int direction, int *iyOut, int *iyLow, int *iyHigh)
{
  int nyHalf = ny / 2;
  *iyOut = 0;
  *iyLow = 0;
  *iyHigh = nyHalf;
  if (! (ny % 2))
    return 1;
  if (direction) {
    *iyOut = ny - 1;
    *iyLow = ny - 2;
    *iyHigh = *iyLow - nyHalf;
    return -1;
  }
  *iyLow = 1;
  *iyHigh = *iyLow + nyHalf;
  return 1;
}
