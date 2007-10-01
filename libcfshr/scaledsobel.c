/*
 * scaledsobel.c - Scale an image with binning and interpolation, take Sobel
 *                  or Prewitt filter
 *
 * Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 * Log at end of file
 */

#include <stdlib.h>
#include <math.h>
#include "imodconfig.h"
#include "b3dutil.h"
#include "mrcslice.h"

#ifdef F77FUNCAP
#define scaledfwrap SCALEDSOBEL
#else
#define scaledfwrap scaledsobel_
#endif

int scaledSobel(float *inImage, int nxin, int nyin, float scaleFac, 
                float minInterp, int linear, float center, float *outImage,
                int *nxout, int *nyout, float *xOffset, float *yOffset)
{
  float interpScale = scaleFac;
  int binning = 1;
  int nxbin, nybin, nxo, nyo, i, j, ind;
  float scale, amat[2][2];
  double Sr, Sc;
  float *tmpImage, *srcImage, *dest;
  size_t xysize;
  
  // Determine the split between binning and interpolation
  for (i = 2; i < 100; i++) {
    scale = scaleFac / i;
    if (scale < 1. || scale < minInterp)
      break;
    interpScale = scale;
    binning = i;
  }

  // Get binned size, offset from binning (0,0 if no binning)
  nxbin = nxin / binning;
  nybin = nyin / binning;
  *xOffset = (nxin % binning) / 2;
  *yOffset = (nyin % binning) / 2;

  // Then get final size and offset from truncation to integer there
  nxo = (int)(nxbin / interpScale);
  nyo = (int)(nybin / interpScale);
  *xOffset += (nxbin - interpScale * nxo) / 2.;
  *yOffset += (nybin - interpScale * nyo) / 2.;
  *nxout = nxo;
  *nyout = nyo;

  // Return with size information only for null image or negative weighting
  if (!inImage || center < 0.)
    return 0;

  // Get a temporary image of needed size
  xysize = binning > 1 ? nxbin * nybin : nxo * nyo;
  tmpImage = (float *)malloc(xysize * sizeof(float));
  if (tmpImage) 
    return 1;
  if (binning > 1) {
    reduceByBinning(inImage, SLICE_MODE_FLOAT, nxin, nyin, binning, tmpImage,
                    0, &nxbin, &nybin);
    srcImage = tmpImage;
    dest = outImage;
  } else {
    srcImage = inImage;
    dest = tmpImage;
  }

  // Do interpolation
  amat[0][0] = amat[1][1] = 1. / interpScale;
  amat[0][1] = amat[1][0] = 0.;
  cubinterp(srcImage, dest, nxbin, nybin, nxo, nyo, amat, nxbin / 2.,
            nybin / 2., 0., 0., 1., 0., linear);

  // If binned, copy data back to temp
  if (binning > 1) {
    for (i = 0; i < nxo * nyo; i++)
      tmpImage[i] = outImage[i];
  }

  // Do the sobel/prewitt filter on interior pixels
  for (j = 1; j < nyo - 1; j++) {
    for (i = 1; i < nxo - 1; i++) {
      ind = i + j * nxo;
      Sr = (tmpImage[ind-nxo-1] + center * tmpImage[ind-nxo] + 
            tmpImage[ind-nxo+1]) - 
        (tmpImage[ind+nxo-1] + center * tmpImage[ind+nxo] + 
         tmpImage[ind+nxo+1]);
      Sc = (tmpImage[ind+nxo+1] + center * tmpImage[ind+1] + 
            tmpImage[ind-nxo+1]) -
        (tmpImage[ind+nxo-1] + center * tmpImage[ind-1] + 
         tmpImage[ind-nxo-1]);
      outImage[ind] = (float)sqrt(Sr * Sr + Sc * Sc);
    }
  }

  // Copy edge pixels
  for (j = 1; j < nyo - 1; j++) {
    outImage[j*nxo] = outImage[j*nxo + 1];
    outImage[j*nxo + nxo - 1] = outImage[j*nxo + nxo - 2];
  }

  for (i = 0; i < nxo; i++) {
    outImage[i] = outImage[i + nxo];
    outImage[i + nxo * (nyo - 1)] = outImage[i + nxo * (nyo - 2)];
  }
  free(tmpImage);
  return 0;
}

int scaledfwrap(float *inImage, int *nxin, int *nyin, float *scaleFac, 
                float *minInterp, int *linear, float *center, float *outImage, 
                int *nxout, int *nyout, float *xOffset, float *yOffset)
{
  return (scaledSobel(inImage, *nxin, *nyin, *scaleFac, *minInterp, *linear,
                      *center, outImage, nxout, nyout, xOffset, yOffset));
}

/*  $Log$


*/
