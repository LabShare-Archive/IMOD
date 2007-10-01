/*   xcorr.cpp  -  Fourier space filtering functions 
 *                    primarily adapted from SerialEM XCorr.cpp
 *
 *   Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *
 *  $Id$
 *  Log at end
 */

#include <stdlib.h>
#include <math.h>
#include "xcorr.h"
#include "mrcc.h"
#include "cfft.h"
#include "cfsemshare.h"
#include "sliceproc.h"
#include "imod.h"
#include "imodview.h"

/*
 * Take an FFT with potential binning, place result back in input slice
 * Desired range of scaled output should be in sin->min, sin->max
 */
float sliceByteBinnedFFT(Islice *sin, int binning, int ix0, int ix1, int iy0,
                         int iy1, int *xcen, int *ycen)
{
  int nxin = sin->xsize;
  int nyin = sin->ysize;
  int nxProc, nyProc, nxDim;
  b3dUByte *indata = sin->data.b;
  float *fftArray;
  float padFrac = 0.02;
  float taperFrac = 0.02;
  int idir = 0;
  int nPadSize, squareSize, nTaper, nPad, i, iyin, iyout, ixbase, ncopy;
  int nPadPix, ixcen, iycen, ixnd;
  double logScale = mrcGetComplexScale();
  double val, max, max2, sum;
  float scale;
  b3dUByte fillVal;
  
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
      return -1.;
    ivwBinByN(sin->data.b, sin->xsize, sin->ysize, binning, indata);
  }
  
  // Figure out size of square array and the padding and tapering
  nxProc = ix1 + 1 - ix0;
  nyProc = iy1 + 1 - iy0;

  squareSize = B3DMAX(nxProc, nyProc);
  nTaper = (int)(taperFrac * squareSize);
  nPad = (int)(padFrac * squareSize);
  nPadSize = niceFrame(squareSize + nPad, 2, 19);
  nPadPix = (nPadSize + 2) * nPadSize;
  
  // Get array for FFT
  fftArray = (float *)malloc(nPadPix * sizeof(float));
  if (!fftArray) {
    if (binning > 1)
      free(indata);
    return -1.;
  }

  // Put into array, take FFT
  sliceTaperInPad(indata, SLICE_MODE_BYTE, nxDim, ix0, ix1, iy0, iy1,
                  fftArray, nPadSize + 2, nPadSize, nPadSize, nTaper, nTaper);
  if (binning > 1)
    free(indata);

  todfft(fftArray, &nPadSize, &nPadSize, &idir);
  
  // Find top two magnitudes while scaling real parts to magnitudes
  max = 0.;
  sum = 0.;
  for (i = 0; i < nPadPix; i += 2) {
    val = sqrt((double)(fftArray[i] * fftArray[i] + 
                               fftArray[i + 1] * fftArray[i + 1]));
    if (max < val) {
      max2 = max;
      max = (float)val;
      iyin = i;
    }
    sum += val;
    fftArray[i] = (float)val;
  }

  // Make the maximum 10% of way from second highest to highest and replace max
  max = max2 + 0.1 * (max - max2);
  fftArray[iyin] = max;
  scale = (sin->max - sin->min) / log(logScale * max + 1.);
  fillVal = (b3dUByte)(scale * log(logScale * sum / nPadPix + 1.) + sin->min);

  // Loop on output lines
  *xcen = ixcen = binning * (ix1 + 1 + ix0) / 2;
  *ycen = iycen = binning * (iy1 + 1 + iy0) / 2;
  iyin = nPadSize - nyin / 2;
  for (iyout = 0; iyout < nyin; iyout++) {
    iyin = iyout - iycen;

    // If line is out of range, fill it
    if (iyin < - nPadSize / 2 || iyin >= nPadSize / 2) {
      indata = sin->data.b + iyout * nxin;
      for (i = 0; i < nxin; i++)
        *indata++ = fillVal;

    } else {
      
      // Wrap if negative
      if (iyin < 0)
        iyin += nPadSize;

      // Copy as many values as fit or exist
      ixbase = iyin * (nPadSize + 2);
      indata = sin->data.b + iyout * nxin + ixcen;
      ixnd = B3DMIN(ixcen + nPadSize / 2, nxin - 1);
      ncopy = 2 * (ixnd + 1 - ixcen);
      for (i = ixbase; i <  ixbase + ncopy; i += 2)
        *indata++ = (b3dUByte)(scale * log(logScale * fftArray[i] + 1.) + 
                               sin->min);

      // Fill the right side
      for (i = ixcen + nPadSize / 2 + 1; i < nxin; i++)
        *indata++ = fillVal;

      // Get Y line to mirror from, copy data in reverse
      if (iyin) {
        iyin = nPadSize - iyin;
        if (iyin == nPadSize / 2)
          iyin--;
      }
      ixbase = iyin * (nPadSize + 2) + 2;
      indata = sin->data.b + iyout * nxin + ixcen - 1;
      ixnd = B3DMAX(ixcen - nPadSize / 2, 0);
      ncopy = 2 *(ixcen - ixnd);
      for (i = ixbase; i <  ixbase + ncopy; i += 2)
        *indata-- = (b3dUByte)(scale * log(logScale * fftArray[i] + 1.) + 
                               sin->min);
      
      // Fill left side
      for (i = 0; i < ixcen - nPadSize / 2; i++)
        *indata-- = fillVal;
    }
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

  padx = (int)(0.05 * nx);
  if (padx < 8)
    padx = 8;
  pady = (int)(0.05 * ny);
  if (pady < 8)
    pady = 8;
  nxpad = niceFrame(nx + padx, 2, 19);
  nypad = niceFrame(ny + pady, 2, 19);
  brray = (float *)malloc((nxpad + 2) * nypad * sizeof(float));
  if (!brray)
    return 1;

  /*for (i = 0; i < nx; i++)
    imodPrintStderr("%d %d %d %d\n", i,sin->data.b[i + nx * (ny / 2 - 1)],
    sin->data.b[i + nx * (ny / 2)],sin->data.b[i + nx * (ny / 2 + 1)]); */
  
  XCorrSetCTF(sigma1, sigma2, radius1, radius2, ctf, nxpad, nypad, &delta);
  sliceTaperOutPad((void *)sin->data.b, sin->mode, nx, ny, brray, nxpad + 2,
                   nxpad, nypad, 0, 0.);
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


/*
$Log$
Revision 1.10  2007/09/14 05:26:12  mast
moved niceframe to library

Revision 1.9  2007/08/15 00:05:42  mast
Moved XCorrTaperInPad to libiimod and renamed

Revision 1.8  2007/06/09 00:22:37  mast
Fixed bug freeing indata when upon memory error

Revision 1.7  2006/06/24 16:03:33  mast
Added arguments to return center coordinate of FFT display

Revision 1.6  2005/03/08 02:50:48  mast
Fix a return value

Revision 1.5  2005/03/08 02:36:04  mast
Put FFT of subarea into the subarea, filled with mean

Revision 1.4  2004/11/11 15:55:34  mast
Changes to do FFT in a subarea

Revision 1.3  2004/11/09 17:53:28  mast
Fixed problems with taperoutpad

Revision 1.2  2004/11/08 05:46:34  mast
Fixed an int32 to be int16

Revision 1.1  2004/11/07 22:59:09  mast
Initial creation

*/
