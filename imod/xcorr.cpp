/*   xcorr.cpp  -  Fourier space filtering functions 
 *                    primarily adapted from SerialEM XCorr.cpp
 *
 *   Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 */                                                                           

/*  $Author$
    
$Date$

$Revision$

$Log$
Revision 1.3  2004/11/09 17:53:28  mast
Fixed problems with taperoutpad

Revision 1.2  2004/11/08 05:46:34  mast
Fixed an int32 to be int16

Revision 1.1  2004/11/07 22:59:09  mast
Initial creation

*/

#include <stdlib.h>
#include <math.h>
#include "xcorr.h"
#include "mrcc.h"
#include "cfft.h"
#include "imod.h"
#include "imodview.h"

/*
 * Take an FFT with potential binning, place result back in input slice
 * Desired range of scaled output should be in sin->min, sin->max
 */
float sliceByteBinnedFFT(Islice *sin, int binning, int ix0, int ix1, int iy0,
                         int iy1)
{
  int nxin = sin->xsize;
  int nyin = sin->ysize;
  int nxProc, nyProc, nxDim;
  b3dUByte *indata = sin->data.b;
  b3dUByte *indata2;
  float *fftArray;
  float padFrac = 0.02;
  float taperFrac = 0.02;
  int iyMirror, idir = 0;
  int nPadSize, squareSize, nTaper, nPad, i, iyin, iyout, ixbase, ncopy;
  double logScale = mrcGetComplexScale();
  float val, max, max2, scale;
  
  if (sin->mode != SLICE_MODE_BYTE)
    return -1.;

  nxDim = sin->xsize;

  // If binning, reduce size, get array and bin into it
  if (binning > 1) {
    ix0 /= binning;
    ix1 /= binning;
    iy0 /= binning;
    iy1 /= binning;
    nxDim /= binning;
    indata = (b3dUByte *)malloc(nxDim * (sin->ysize / binning));
    if (!indata)
      return 1;
    ivwBinByN(sin->data.b, sin->xsize, sin->ysize, binning, indata);
  }
  
  // Figure out size of square array and the padding and tapering
  nxProc = ix1 + 1 - ix0;
  nyProc = iy1 + 1 - iy0;

  squareSize = nxProc > nyProc ? nxProc : nyProc;
  nTaper = taperFrac * squareSize;
  nPad = padFrac * squareSize;
  nPadSize = XCorrNiceFrame(squareSize + nPad, 2, 19);
  
  // Get array for FFT
  fftArray = (float *)malloc((nPadSize + 2) * nPadSize * sizeof(float));
  if (!fftArray) {
    if (binning)
      free(indata);
    return -1.;
  }

  // Put into array, take FFT
  XCorrTaperInPad(indata, SLICE_MODE_BYTE, nxDim, ix0, ix1, iy0, iy1,
                  fftArray, nPadSize + 2, nPadSize, nPadSize, nTaper, nTaper);
  if (binning > 1)
    free(indata);

  todfft(fftArray, &nPadSize, &nPadSize, &idir);
  
  // Find top two magnitudes while scaling real parts to magnitudes
  max = 0.;
  for (i = 0; i < (nPadSize + 2) * nPadSize; i += 2) {
    val = (float)sqrt((double)(fftArray[i] * fftArray[i] + 
                               fftArray[i + 1] * fftArray[i + 1]));
    if (max < val) {
      max2 = max;
      max = val;
      iyin = i;
    }
    fftArray[i] = val;
  }

  // Make the maximum 10% of way from second highest to highest and replace max
  max = max2 + 0.1 * (max - max2);
  fftArray[iyin] = max;
  scale = (sin->max - sin->min) / log(logScale * max + 1.);

  // Loop on output lines; set up input line corresponding to first output line
  iyin = nPadSize - nyin / 2;
  for (iyout = 0; iyout < nyin; iyout++) {
    ixbase = iyin * (nPadSize + 2);
    indata = sin->data.b + iyout * nxin + nxin / 2;
    
    // If line is out of range, fill it
    if (iyout < nyin / 2 - nPadSize / 2 || iyout >= nyin / 2 + nPadSize / 2) {
      for (i = 0; i < nxin / 2; i++)
        *indata++ = 0;
      sin->data.b[iyout * nxin] = 0;

    } else {
      
      // Copy as many values as fit or exist
      ncopy = nxin < nPadSize + 2 ? nxin : nPadSize + 2;
      for (i = ixbase; i < ixbase + ncopy; i += 2)
        *indata++ = (b3dUByte)(scale * log(logScale * fftArray[i] + 1.) + 
                               sin->min);
      
      // Mirror something on the left edge if this is a legal line to mirror
      iyMirror = 2 * (nyin / 2) - iyout;
      if (iyMirror < nyin) {

        // If there are more pixels take one; otherwise take 0
        if (ncopy < nPadSize + 2)
          val = (b3dUByte)(scale * log(logScale * fftArray[i] + 1.) + 
                           sin->min);
        else 
          val = 0.;
        sin->data.b[iyMirror * nxin] = val;

        // Take care of orphan pixel in lower left corner
        if (iyMirror && 
            (iyout == nyin - 1 || iyout == nyin / 2 + nPadSize /2 - 1))
          sin->data.b[(iyMirror - 1) * nxin] = val;
      }

      // Fill right edge if necessary
      for (i = ncopy / 2; i < nxin / 2; i++)
        *indata++ = 0;
    }
    
    // Advance iyin and wrap when needed
    iyin++;
    if (iyin == nPadSize)
      iyin = 0;
  }

  // Mirror the lines in Y around the center
  for (iyout = 0; iyout < nyin; iyout++) {
    iyin = 2 * (nyin / 2) - iyout;
    if (iyin == nyin)
      iyin--;
    indata = sin->data.b + iyin * nxin + nxin / 2 + 1;
    indata2 = sin->data.b + iyout * nxin + nxin / 2 - 1;
    for (i = 0; i < nxin / 2 - 1; i++)
      *indata2-- = *indata++;
  }

  return (float)(1. / nPadSize);
}

/*
 * Apply a filter in Fourier space to the input slice, with the usual four
 * filtering parameters.
 * Any input mode (byte, short, ushort, float) is allowed.
 * Desired range of scaled output should be in sin->min, sin->max
 */
int sliceFourierFilter(Islice *sin, float sigma1, float sigma2, float radius1,
                       float radius2)
{
  int nx = sin->xsize;
  int ny = sin->ysize;
  int nxpad, nypad, idir, padx, pady, ixlo, iylo;
  float *brray;
  float ctf[8193];
  float delta, outMin, outMax;

  padx = 0.05 * nx;
  if (padx < 8)
    padx = 8;
  pady = 0.05 * ny;
  if (pady < 8)
    pady = 8;
  nxpad = XCorrNiceFrame(nx + padx, 2, 19);
  nypad = XCorrNiceFrame(ny + pady, 2, 19);
  brray = (float *)malloc((nxpad + 2) * nypad * sizeof(float));
  if (!brray)
    return 1;

  /*for (i = 0; i < nx; i++)
    imodPrintStderr("%d %d %d %d\n", i,sin->data.b[i + nx * (ny / 2 - 1)],
    sin->data.b[i + nx * (ny / 2)],sin->data.b[i + nx * (ny / 2 + 1)]); */
  
  XCorrSetCTF(sigma1, sigma2, radius1, radius2, ctf, nxpad, nypad, &delta);
  XCorrTaperOutPad((void *)sin->data.b, sin->mode, nx, ny, brray, nxpad + 2,
                   nxpad, nypad);
  /* for (i = 0; i < nxpad; i++)
     imodPrintStderr("%d %f %f %f\n", i, brray[i + (nxpad + 2) *
     ( nypad / 2 -1 )], brray[i + (nxpad + 2) * nypad / 2], 
     brray[i + (nxpad + 2) * (nypad / 2 + 1)]); */
  
  idir = 0;
  todfft(brray, &nxpad, &nypad, &idir);
  XCorrFilterPart(brray, brray, nxpad, nypad, ctf, delta);
  idir = 1;
  todfft(brray, &nxpad, &nypad, &idir);

  ixlo = (nxpad - nx) / 2;
  iylo = (nypad - ny) / 2;
  outMin = sin->min;
  outMax = sin->max;
  if (!(outMin || outMax)) {
    if (sin->mode == SLICE_MODE_BYTE) {
      outMin = 0.01;
      outMax = 255.;
    } else if (sin->mode == SLICE_MODE_USHORT) {
      outMin = 0.01;
      outMax = 30000.;
    }
  }
  XCorrExtractConvert(brray, nxpad + 2, ixlo, iylo, (void *)sin->data.b,
                      sin->mode, nx, ny, outMin, outMax);
  free(brray);
  return 0;
}


/*  TAPEROUTPAD pads an image, dimensions NXBOX by NYBOX in ARRAY,
 *  into the center of a larger array.  The padded image in BRRAY will
 *  have size NX by NY while the BRRAY will be dimensioned NXDIM by NY
 *  The values of the image at its edge will be tapered out to the mean at
 *  the pre-existing edge of the image
 */
void XCorrTaperOutPad(void *array, int type, int nxbox, int nybox, 
                      float *brray, int nxdim, int nx, int ny)
{
  b3dUByte *bytein;
  b3dInt16 *intin;
  b3dUInt16 *uintin;
  float *floatin;
  float *out;
  int ixlo, ixhi, iylo, iyhi, ix, iy, nxtop, nytop, iytoplin;
  float sum, dmean, edgel, edger, wedge, wmean, prodmean;

  ixlo = (nx - nxbox) / 2;
  ixhi = ixlo + nxbox;
  iylo = (ny - nybox) / 2;
  iyhi = iylo + nybox;
  for (iy = nybox - 1; iy >= 0; iy--) {
    out = brray + ixhi + (iylo + iy) * nxdim - 1;
    switch (type) {
    case SLICE_MODE_BYTE:
      bytein = (b3dUByte *)array + (iy + 1) * nxbox - 1;
      for (ix = nxbox - 1; ix >= 0; ix--)
        *out-- = *bytein--;
      break;
      
    case SLICE_MODE_SHORT:
      intin = (b3dInt16 *)array + (iy + 1) * nxbox - 1;
      for (ix = nxbox - 1; ix >= 0; ix--)
        *out-- = *intin--;
      break;
      
    case SLICE_MODE_USHORT:
      uintin = (b3dUInt16 *)array + (iy + 1) * nxbox - 1;
      for (ix = nxbox - 1; ix >= 0; ix--)
        *out-- = *uintin--;
      break;
      
    case SLICE_MODE_FLOAT:
      floatin = (float *)array + (iy + 1) * nxbox - 1;
      for (ix = nxbox - 1; ix >= 0; ix--)
        *out-- = *floatin--;
      break;
    }
  }

  /* Do the taper if there is any padding */
  if (nxbox != nx || nybox != ny) {
    sum=0.;
    for (ix = ixlo; ix < ixhi; ix++)
      sum += brray[ix + iylo * nxdim] + brray[ix + (iyhi - 1) * nxdim];

    for (iy = iylo + 1; iy < iyhi - 1; iy++)
      sum += brray[ixlo + iy * nxdim] + brray[ixhi - 1 + iy * nxdim];

    dmean = sum / (2 * (nxbox + nybox - 2));
    nxtop = nx - 1;
    nytop = ny - 1;

    /*  if there is a mismatch between left and right, add a column on 
        right; similarly for bottom versus top, add a row on top */
    if (nx - ixhi > ixlo) {
      nxtop--;
      for (iy = 0; iy < ny; iy++)
        brray[nx - 1 + iy * nxdim] = dmean;
    }
    if (ny - iyhi > iylo) {
      nytop--;
      for (ix = 0; ix < nx; ix++)
        brray[ix + (ny - 1) * nxdim] = dmean;
    }
    
    for (iy = iylo; iy < iyhi; iy++) {
      edgel = brray[ixlo + iy * nxdim];
      edger = brray[ixhi - 1 + iy * nxdim];
      for (ix = 0; ix < ixlo; ix++) {
        wedge = ((float)ix) / ixlo;
        wmean = 1. - wedge;
        brray[ix + iy * nxdim] = wmean * dmean + wedge * edgel;
        brray[nxtop - ix + iy * nxdim] = wmean * dmean + wedge * edger;
      }
    }

    for (iy = 0; iy < iylo; iy++) {
      wedge = ((float)iy) / iylo;
      prodmean = (1. - wedge) * dmean;
      iytoplin = nytop - iy;
      for (ix = 0; ix < nx; ix++) {
        brray[ix + iy * nxdim] = prodmean + wedge * brray[ix + iylo * nxdim];
        brray[ix + iytoplin * nxdim] = prodmean + 
          wedge * brray[ix + (iyhi - 1) * nxdim];
      }
    }
  }
}

/* Extract a subarea from a float ARRAY with X dimension NXDIM, with the 
 * subarea starting at IXLO, IYLO and having size NX by BY.  Place it into
 * BRRAY whose TYPE is one of the SLICE_MODE types, and scale so that the
 * output range is from OUTMIN to OUTMAX 
 */
void XCorrExtractConvert(float *array, int nxdim, int ixlo, int iylo, 
                         void *brray, int type, int nx, int ny,
                         float outMin, float outMax)
{
  b3dUByte *byteout;
  b3dInt16 *intout;
  b3dUInt16 *uintout;
  float *floatout;
  float *in;
  int ix, iy;
  float min, max, scale = 0., base;

  /* Need to scale data?  */
  if (outMin || outMax) {
    min = 1.e30;
    max = -min;
    for (iy = 0; iy < ny; iy++) {
      in = array + ixlo + (iy + iylo) * nxdim;
      for (ix = 0; ix < nx; ix++) {
        if (min > *in)
          min = *in;
        if (max < *in)
          max = *in;
        in++;
      }
    }
    if (max == min)
      max++;
    scale = (outMax - outMin) / (max - min);
    base = outMin - scale * min;
  }

  for (iy = 0; iy < ny; iy++) {
    in = array + ixlo + (iy + iylo) * nxdim;

    switch (type) {
    case SLICE_MODE_BYTE:
      byteout = (b3dUByte *)brray + iy * nx;
      if (scale)
        for (ix = 0; ix < nx; ix++)
          *byteout++ = (b3dUByte)(*in++ * scale + base);
      else
        for (ix = 0; ix < nx; ix++)
          *byteout++ = (b3dUByte)*in++;
      break;

    case SLICE_MODE_SHORT:
      intout = (b3dInt16 *)brray + iy * nx;
      if (scale)
        for (ix = 0; ix < nx; ix++)
          *intout++ = (b3dInt16)(*in++ * scale + base);
      else
        for (ix = 0; ix < nx; ix++)
          *intout++ = (b3dInt16)*in++;
      break;

    case SLICE_MODE_USHORT:
      uintout = (b3dUInt16 *)brray + iy * nx;
      if (scale)
        for (ix = 0; ix < nx; ix++)
          *uintout++ = (b3dUInt16)(*in++ * scale + base);
      else
        for (ix = 0; ix < nx; ix++)
          *uintout++ = (b3dUInt16)*in++;
      break;

    case SLICE_MODE_FLOAT:
      floatout = (float *)brray + iy * nx;
      if (scale)
        for (ix = 0; ix < nx; ix++)
          *floatout++ = *in++;
      else
        for (ix = 0; ix < nx; ix++)
          *floatout++ = *in++ * scale + base;
      break;

    }
  }
}


/*  NICEFRAME returns NUM if it has no prime factor greater
    than LIMIT, or adds IDNUM to NUM until it reaches a number with this
    tractable property */

int XCorrNiceFrame(int num, int idnum, int limit)
{
#ifdef USE_FFTW
  if (limit == 19)
    limit = 13;
#endif
  int numin, numtmp, ifac;
  numin=2 * ((num + 1) / 2);
  do {
    numtmp=numin;
    for (ifac = 2; ifac <= limit; ifac++)
      while (numtmp % ifac == 0)
        numtmp=numtmp/ifac;
    
    if (numtmp > 1)
      numin += idnum;
  } while (numtmp > 1);
  return numin;
}

/*  SETCTF takes the filter parameters SIGMA1,2 and RADIUS1,2 and sets
    up the contrast
    transfer function in the array CTF, which should be dimensioned
    at least 8193.  The real image size is NX by NY, and the step
    size between CTF values is returned in DELTA, or 0 if no filtering
    is selected.
*/

void XCorrSetCTF(float sigma1, float sigma2, float radius1, float radius2,
                 float *ctf, int nx,int ny, float *delta)
{
  double beta1, beta2, alpha;
  float asize, s, ssqrd, sum, scl;
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
       
  asize = nsize;
  nsize = nsize + 1;
  if ((sigma1 >= 0 ? sigma1 : -sigma1) > 1.e-6)
    alpha = -0.5/(sigma1 * sigma1);
  if ((sigma2 >= 0 ? sigma2 : -sigma2) > 1.e-6)
    beta1  = -0.5/(sigma2*sigma2);
  beta2=beta1;
  *delta = 1.0/(.71*asize);
  s = 0.0;
  for (j = 0; j < nsize; j++) {
    if (s < radius1) 
      ctf[j] = exp(beta1*(s - radius1)*(s - radius1));
    else if (s > radius2)
      ctf[j] = exp(beta2*(s - radius2)*(s - radius2));
    else
      ctf[j] = 1.0;

    s = s + *delta;
    if(sigma2 < -1.e-6) ctf[j]=1. - ctf[j];
  }
  if (sigma1 > 1.e-6) {
    s=0.;
    for (j = 0; j < nsize; j++) {
      ctf[j] = ctf[j]*(1.0 - exp(alpha*s*s));
      s = s + *delta;
    }
  } else if (sigma1 < -1.e-6) {
    s=0.;
    for (j = 0; j < nsize; j++) {
      ssqrd=s*s;
      ctf[j]=ctf[j]*ssqrd*exp(alpha*ssqrd);
      s = s + *delta;
    }
  }
  sum = 0.0;
  for (j = 1; j < nsize; j++)
    sum = sum + ctf[j];

  scl = asize/sum;
  for (j = 1; j < nsize; j++)
    ctf[j] = ctf[j]*scl;
  return;
}

/*  FILTERPART applies the filter in CTF to the array in FFT, puts result
    into ARRAY.  NX and NY are real image dimensions, DELTA is the
    interval in reciprocal space for the CTF.
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
  delx = 1.0/nx;
  dely = 1.0/ny;

  /*   apply filter function on fft, put result in array */

  index = 0;
  for (iy = 0; iy <= nym1; iy++) {
    y = iy*dely;
    if (y > 0.5)
      y = 1.0 - y;
    y2 = y*y;
    x = 0.0;
    for (ix = 0; ix <= nxo2; ix++) {
      ind = 2 * (index + ix);
      indp1 = ind + 1;
      s = sqrt(x*x + y2);
      indf = s/delta + 0.5;
      array[ind] = fft[ind] * ctf[indf];
      array[indp1] = fft[indp1] * ctf[indf];
      x = x + delx;
    }
    index = index + nx21;
  }
  return;
}

/* TAPERINPAD pads an image, located at IX0 to IX1 and IY0 to IY1 in 
   ARRAY, into the center of a larger array.  NXDIMIN is the X dimension of
   ARRAY.  The padded image in BRRAY
   will have size NX by NY while the BRRAY will be dimensioned
   NXDIM by NY.  The values of the image at its edge will be
   tapered down to the mean value at the edge, over a width of the
   original image area equal to NXTAP or NYTAP.
*/

void XCorrTaperInPad(void *array, int type, int nxdimin, int ix0, int ix1,
                     int iy0, int iy1, float *brray, int nxdim, int nx, int ny,
                     int nxtap, int nytap)
{
  int lobase, hibase, x1, x2, ixlo, ixhi, iylo, iyhi, ix, iy, ixbase;
  int nsum, nxbox, nybox, ixlim;
  float sum, dmean, fracx, fracy, fmin;
  b3dUByte *bytein;
  b3dInt16 *intin;
  b3dUInt16 *uintin;
  float *floatin;
  float *out;
  
  nxbox = ix1 + 1 - ix0;
  nybox = iy1 + 1 - iy0;
  ixlo=nx/2-nxbox/2 - 1;
  ixhi=ixlo+nxbox;
  iylo=ny/2-nybox/2 - 1;
  iyhi=iylo+nybox;
  
  for (iy = iy0; iy <= iy1; iy++) {
    out = brray + ixlo + 1 + (iylo + 1 + iy - iy0) * nxdim;
    switch (type) {
    case SLICE_MODE_BYTE:
      bytein = (b3dUByte *)array + ix0 + iy * nxdimin;
      for (ix = ix0; ix <= ix1; ix++)
        *out++ = *bytein++;
      break;
      
    case SLICE_MODE_SHORT:
      intin = (b3dInt16 *)array + ix0 + iy * nxdimin;
      for (ix = ix0; ix <= ix1; ix++)
        *out++ = *intin++;
      break;
         
    case SLICE_MODE_USHORT:
      uintin = (b3dUInt16 *)array + ix0 + iy * nxdimin;
      for (ix = ix0; ix <= ix1; ix++)
        *out++ = *uintin++;
      break;
         
    case SLICE_MODE_FLOAT:
      floatin = (float *)array + ix0 + iy * nxdimin;
      for (ix = ix0; ix <= ix1; ix++)
        *out++ = *floatin++;
      break;

    }
  }

  /* get edge mean */
  sum=0.;
  lobase = (iylo + 1) * nxdim;
  hibase = iyhi * nxdim;
  for (ix = ixlo + 1; ix <= ixhi; ix++)
    sum += brray[ix + lobase] + brray[ix + hibase];
  for (iy = iylo + 2; iy < iyhi; iy++) {
    ixbase = iy * nxdim;
    sum += brray[ixlo + 1 + ixbase] + brray[ixhi + ixbase];
  }
  
  nsum=(2*(nxbox+nybox-2));
  dmean=sum/nsum;
    
  /* fill the rest of the array with dmean */
  if (nxbox != nx || nybox != ny) {
    for (iy = iylo + 1; iy <= iyhi; iy++) {
      ixbase = iy * nxdim;
      for (ix = 0; ix <= ixlo; ix++)
        brray[ix + ixbase] = dmean;
      for (ix = ixhi + 1; ix < nx; ix++)
        brray[ix + ixbase] = dmean;
    }
    for (iy = 0; iy <= iylo; iy++) {
      ixbase = iy * nxdim;
      for (ix = 0; ix < nx; ix++)
        brray[ix + ixbase] = dmean;
    }
    for (iy = iyhi + 1; iy < ny; iy++) {
      ixbase = iy * nxdim;
      for (ix = 0; ix < nx; ix++)
        brray[ix + ixbase] = dmean;
    }
  }
  
  /* Taper the edges */
  for (iy = 0; iy < (nybox+1)/2; iy++) {
    fracy=1.;
    ixlim = nxtap;
    if (iy < nytap) {
      fracy=(iy + 1.)/(nytap+1.);
      ixlim = (nxbox+1)/2;
    }
    for (ix = 0; ix < ixlim; ix++) {
      fracx=1.;
      if (ix < nxtap)
        fracx=(ix + 1.)/(nxtap+1.);
      fmin=fracx < fracy ? fracx : fracy;
      if(fmin < 1.) {
        x1 = ix + 1 + ixlo;
        x2 = ixhi - ix;
        lobase = (iy + 1 + iylo) * nxdim;
        hibase = (iyhi - iy) *nxdim;
        brray[x1 + lobase]=fmin*(brray[x1 + lobase]-dmean)+dmean;
        brray[x1 + hibase]=fmin*(brray[x1 + hibase]-dmean)+dmean;
        brray[x2 + lobase]=fmin*(brray[x2 + lobase]-dmean)+dmean;
        brray[x2 + hibase]=fmin*(brray[x2 + hibase]-dmean)+dmean;
      }
    }
  }
}
