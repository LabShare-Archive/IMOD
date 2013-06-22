/*
 * scaledsobel.c - Scale an image with binning and interpolation, take Sobel
 *                  or Prewitt filter
 *
 * Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 */

#include <stdlib.h>
#include <math.h>
#include "imodconfig.h"
#include "b3dutil.h"
#include "mrcslice.h"

#ifdef F77FUNCAP
#define scaledsobel SCALEDSOBEL
#else
#define scaledsobel scaledsobel_
#endif

/*! 
 * Applies a Sobel-type gradient filter to an image after scaling it by a
 * specified amount.  When an image is being scaled down, some of the scaling
 * can be done with binning and the remainder is done with interpolation.
 * ^  [inImage] - input image
 * ^  [nxin], [nyin] - size of image, which is assumed to be contiguous in
 * the image array (X dimension equals [nxin].)
 * ^  [scaleFac] - overall amount by which image is being scaled down (a value 
 * < 1 will scale an image up).
 * ^  [minInterp] - minimum amount of this scaling to be done by interpolation.
 * ^  [linear] - set to 1 to use linear rather than cubic interpolation, or to
 * -1 to use @zoomFiltInterp instead of binning followed by @cubinterp
 * ^  [center] - weighting of center pixel, 2 for Sobel or 1 for Prewitt filter
 * ^  [outImage] - output image
 * ^  [nxout], [nyout] - returned with size of output image
 * ^  [xOffset], [yOffset] - coordinate offset in output image: 
 * ^     input_coord = output_coord * scalingFactor + xOffset ^
 * The return value is 1 for a failure to allocate a temporary array, or the return
 * value from zoomdown if it gives an error.  ^
 * If [inImage] is NULL or [center] < 0, the function will compute the output
 * sizes and offsets and return.  ^
 * If [center] is 0, the scaled image is computed and returned without Sobel 
 * filtering. ^
 * The filtering operation is parallelized with OpenMP with the same limitation on
 * number of threads as used in @@cubinterp@.  ^
 * The call from Fortran is the same as that from C.
 */
int scaledSobel(float *inImage, int nxin, int nyin, float scaleFac, 
                float minInterp, int linear, float center, float *outImage,
                int *nxout, int *nyout, float *xOffset, float *yOffset)
{
  float interpScale = scaleFac;
  int binning = 1;
  int nxbin, nybin, nxo, nyo, i, j, ind, numThreads;
  float scale, edge, amat[2][2];
  double Sr, Sc, wallStart, terpTime;
  float *tmpImage, *srcImage, *dest;
  size_t xysize;
  
  // Determine the split between binning and interpolation
  if (linear >= 0) {
    for (i = 2; i < 100; i++) {
      scale = scaleFac / i;
      if (scale < 1. || scale < minInterp)
        break;
      interpScale = scale;
      binning = i;
    }
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

  if (linear < 0) {
    ind = selectZoomFilter(4, 1. / interpScale, &i);
    if (ind)
      return ind;
  }

  // Get a temporary image of needed size
  wallStart = wallTime();
  xysize = binning > 1 ? nxbin * nybin : nxo * nyo;
  tmpImage = (float *)malloc(xysize * sizeof(float));
  if (!tmpImage) 
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
  edge = (float)sliceEdgeMean(srcImage, nxbin, 0, nxbin - 1, 0, nybin - 1);
  amat[0][0] = amat[1][1] = 1. / interpScale;
  amat[0][1] = amat[1][0] = 0.;
  if (linear >= 0 || scaleFac <= 1.) {
    cubinterp(srcImage, dest, nxbin, nybin, nxo, nyo, amat, nxbin / 2.,
              nybin / 2., 0., 0., 1., edge, linear);
  } else {
    i = zoomFiltInterp(srcImage, dest, nxbin, nybin, nxo, nyo, nxbin / 2.,
                   nybin / 2., 0., 0., edge);
    if (i) {
      free(tmpImage);
      return i;
    }
  }
  terpTime = wallTime() - wallStart;
  wallStart = wallTime();

  // If binned, copy data back to temp
  if (binning > 1) {
    for (i = 0; i < nxo * nyo; i++)
      tmpImage[i] = outImage[i];
  }

  if (!center) {
    for (i = 0; i < nxo * nyo; i++)
      outImage[i] = tmpImage[i];
    free(tmpImage);
    return 0;
  }

  // Do the sobel/prewitt filter on interior pixels
  numThreads = B3DNINT(0.04 * sqrt((double)nxo * nyo));
  numThreads = numOMPthreads(numThreads);
#pragma omp parallel for num_threads(numThreads)                    \
  shared(tmpImage, center, nxo, nyo, outImage)  \
  private(j, i, ind, Sr, Sc)
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

    // Copy edge pixels
    outImage[j*nxo] = outImage[j*nxo + 1];
    outImage[j*nxo + nxo - 1] = outImage[j*nxo + nxo - 2];
  }
  /*printf("interp time = %.1f   sobel time = %.1f\n", 1000. * terpTime, 
    1000. * (wallTime() - wallStart)); */

  // Copy top/bottom edge pixels
  for (i = 0; i < nxo; i++) {
    outImage[i] = outImage[i + nxo];
    outImage[i + nxo * (nyo - 1)] = outImage[i + nxo * (nyo - 2)];
  }
  free(tmpImage);
  return 0;
}

int scaledsobel(float *inImage, int *nxin, int *nyin, float *scaleFac, 
                float *minInterp, int *linear, float *center, float *outImage, 
                int *nxout, int *nyout, float *xOffset, float *yOffset)
{
  return (scaledSobel(inImage, *nxin, *nyin, *scaleFac, *minInterp, *linear,
                      *center, outImage, nxout, nyout, xOffset, yOffset));
}
