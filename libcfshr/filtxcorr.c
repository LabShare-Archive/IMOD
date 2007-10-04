/*
 * filtxcorr.c - Functions for filtering and cross-correlation
 *
 * Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
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
#define setctfwsr SETCTFWSR
#define setctfnoscl SETCTFNOSCL
#define filterpart FILTERPART
#define xcorrpeakfind XCORRPEAKFIND
#define meanzero MEANZERO
#define parabolicfitposition PARABOLICFITPOSITION
#else
#define setctfwsr setctfwsr_
#define setctfnoscl setctfnoscl_
#define filterpart filterpart_
#define xcorrpeakfind xcorrpeakfind_
#define meanzero meanzero_
#define parabolicfitposition parabolicfitposition_
#endif

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
 * Fortran wrapper for @XCorrSetCTF.
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
  float asize, s, ssqrd, radius1p, radius1n;
  int nsize, j;
  
  *delta=0.;
  if(sigma1 == 0. && sigma2 == 0.)
    return;
  
  alpha = 0.0;
  beta1 = 0.0;
  beta2 = 0.0;
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
        ctf[j] = ctf[j]*(1.0f - (float)exp(alpha*(s - radius1n)*(s - radius1n)));
      s = s + *delta;
    }
  } else if (sigma1 < -1.e-6) {
    s=0.;
    for (j = 0; j < nsize; j++) {
      ssqrd=s*s;
      ctf[j]=(float)(ctf[j]*ssqrd*exp(alpha*ssqrd));
      s = s + *delta;
    }
  }

  *nsizeOut = nsize;
}

/*!
 * Fortran wrapper for @XCorrSetCTFnoScl.
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

/*!
 * Finds the coordinates of the [maxpeaks] highest peaks in [array], which is
 * dimensioned to [nxdim] by [ny], and returns the positions in [xpeak],
 * [ypeak] and the peak values in [peak].  The X size of the image is assumed 
 * to be [nxdim] - 2.  The sub-pixel position is determined by fitting a
 * parabola separately in X and Y to the peak and 2 adjacent points.  Positions
 * are numbered from zero and coordinates bigger than half the image size are
 * shifted to be negative.  The positions are this the amount to shift a second
 * image in a correlation to a align it to the first.  If fewer than [maxpeaks]
 * peaks are found, then the remaining values in [peaks] will be 1.e-30.
 */
void XCorrPeakFind(float *array, int nxdim, int ny, float  *xpeak,
                   float *ypeak, float *peak, int maxpeaks)
{
  float denom, cx, cy, y1, y2, y3, local, val;
  int ixpeak, iypeak, ix, iy, ixBlock, iyBlock, ixStart, ixEnd, iyStart, iyEnd;
  int i, j, ixm, ixp, iyb, iybm, iybp;
  int nx=nxdim-2;
  int blockSize = 5;
  int nBlocksX = (nx + blockSize - 1) / blockSize;
  int nBlocksY = (ny + blockSize - 1) / blockSize;
    
  /* find peaks */
  for (i = 0; i < maxpeaks; i++) {
    peak[i] = -1.e30f;
    xpeak[i] = 0.;
    ypeak[i] = 0.;
  }

  if (maxpeaks < 2) {
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
  } else {
    
    // Check for local peaks by looking at the highest point in each local 
    // block
    for (iyBlock = 0 ; iyBlock < nBlocksY; iyBlock++) {
      iyStart = iyBlock * blockSize;
      iyEnd = iyStart + blockSize;
      if (iyEnd > ny)
        iyEnd = ny;
      for (ixBlock = 0 ; ixBlock < nBlocksX; ixBlock++) {
        ixStart = ixBlock * blockSize;
        ixEnd = ixStart + blockSize;
        if (ixEnd > nx)
          ixEnd = nx;
        
        local = -1.e30f;
        for (iy = iyStart; iy  < iyEnd; iy++) {
          for (ix = ixStart; ix < ixEnd; ix++) {
            val = array[ix + iy * nxdim];
            if (val > local && val > peak[maxpeaks - 1]) {
              local = val;
              ixpeak = ix;
              iypeak = iy;
            }
          }
        }
        
        // evaluate local peak for truly being local
        if (local > -1.e30) {
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
    if (peak[i] < -1.e29)
      continue;

    // Add 0.2 just in case float was less than int assigned to it
    ixpeak = (int)(xpeak[i] + 0.2);
    iypeak = (int)(ypeak[i] + 0.2);
    
    /* simply fit a parabola to the two adjacent points in X or Y */
    
    y1=array[(ixpeak + nx - 1) % nx + iypeak * nxdim];
    y2=peak[i];
    y3=array[(ixpeak + 1) % nx + iypeak * nxdim];
    cx = parabolicFitPosition(y1, y2, y3);

    y1=array[ixpeak + ((iypeak + ny - 1) % ny) * nxdim];
    y3=array[ixpeak + ((iypeak + 1) % ny) * nxdim];
    cy = parabolicFitPosition(y1, y2, y3);
    
    /*    return adjusted pixel coordinate */
    xpeak[i]=ixpeak+cx;
    ypeak[i]=iypeak+cy;
    if(xpeak[i] > nx/2)
      xpeak[i]=xpeak[i]-nx;
    if(ypeak[i] > ny/2)
      ypeak[i]=ypeak[i]-ny;
  }
}

/*!
 * Fortran wrapper to @XCorrPeakFind.  If [maxpeaks] is 1, then [xpeak], 
 * [ypeak], and [peak] can be single variables instead of arrays.
 */
void xcorrpeakfind(float *array, int *nxdim, int *ny, float  *xpeak,
                   float *ypeak, float *peak, int *maxpeaks)
{
  XCorrPeakFind(array, *nxdim, *ny, xpeak, ypeak, peak, *maxpeaks);
}

/*!
 * Given values at three successive positions, [y1], [y2], and [y3], where
 * [y2] is the peak value, this fits a parabola to the values and returns the
 * offset of the peak from the center position, a number between -0.5 and 0.5.
 */
float parabolicFitPosition(float y1, float y2, float y3)
{
  float denom, cx = 0.;
  denom=2.f*(y1+y3-2.f*y2);
  
  /* y2 is the highest, so in the absence of numerical error due to numbers
     being very close to each other, the peak must be in the interval.
     This seems better than testing if zero or testing against 1.e-6 */
  if (fabs((double)denom) > fabs(1.e-2 * (y1-y3)))
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
float parabolicfitposition(float *y1, float *y2, float *y3)
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

/*  $Log$
/*  Revision 1.1  2007/10/01 15:26:09  mast
/*  *** empty log message ***
/*


*/
