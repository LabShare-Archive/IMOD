/*
 * pctstretch.c : estimate percentile limits of an image from sample of pixels
 *
 * $Id$
 */

#include <stdlib.h>
#include "imodconfig.h"
#include "b3dutil.h"
#include "mrcslice.h"

#define BYTE SLICE_MODE_BYTE
#define UNSIGNED_SHORT SLICE_MODE_USHORT
#define SIGNED_SHORT SLICE_MODE_SHORT
#define FLOAT SLICE_MODE_FLOAT

#define SHORT_SHIFT 2

/*!
 * Estimates percentile limits for contrast strecthing an image by building a histogram
 * of a sample. ^
 * [image] = line pointers to image ^
 * [mode] = data type, a SLICE_MODE_ value; BYTE, USHORT, SHORT, or FLOAT are allowed ^
 * [nx], [ny] = X and Y dimensions of image ^
 * [sample] = fraction of pixels to sample ^
 * [ixStart], [iyStart] = starting X and Y coordinates to sample ^
 * [nxUse], [nyUse] = extent of image to sample in X and Y ^
 * [pctLo], [pctHi] = Percentile levels to find on the low and high end of range 
 * (1.0 is 1%) ^
 * [scaleLo], [scaleHi] = returned values of those percentile levels ^
 * Returns 1 for inadequate number of pixels, 2 for invalid mode, or 3 for memory error.
*/

int percentileStretch(unsigned char **image, int type, int nx, int ny, float sample,
                      int ixStart, int iyStart, int nxUse, int nyUse, 
                      float pctLo, float pctHi, float *scaleLo, float *scaleHi)
{
  int nLo, nHi, nSample;
  double nPixUse;
  /* pointers for data of different types */
  unsigned char **bytep;
  b3dUInt16 **ushortp;
  b3dInt16 **shortp;
  float **floatp;
  
  int ixUse, iyUse, dxSample;
  int i, j, val;
  int nbins, cum, base, factor;
  int *hist;
  float minVal, maxVal, fval, fRange, fPad, fFactor, fBase; 
  
  /* get the number of points to sample, and sampling interval */
  nPixUse = ((double)nxUse) * nyUse;
  nSample = (int)(sample * nPixUse);
  if (nSample >= nPixUse) 
    nSample = (int)nPixUse;
  if (nxUse < 2 || nyUse < 2 || nSample < 5)
    return(1);

  dxSample = (int)(nPixUse / nSample);
  if (dxSample == 0)
    dxSample = 1;
  if (dxSample > 5 && nPixUse < 2.e9 && (dxSample % 2 == ((int)nPixUse % 2)))
    dxSample--;
  nSample = (int)(nPixUse / dxSample);
  
  /* get number of extreme values to keep track of */
  nLo = B3DMAX(1, (int)(0.01 * pctLo * nSample));
  nHi = B3DMAX(1, (int)(0.01 * pctHi * nSample));
  
  /* get pointer based on type of data */
  switch (type) {
  case BYTE :
    bytep = (unsigned char **) image;
    nbins = 256;
    base = 0;
    factor = 1;
    break;
    
  case SIGNED_SHORT :
    shortp = (b3dInt16 **)image;
    factor = 1 << SHORT_SHIFT;
    nbins = 65536 / factor;
    base = 32768;
    break;
    
  case UNSIGNED_SHORT :
    ushortp = (b3dUInt16 **)image;
    factor = 1 << SHORT_SHIFT;
    nbins = 65536 / factor;
    base = 0;
    break;
    
  case FLOAT:   
    floatp = (float **)image;
    minVal = 1.e38f;
    maxVal = -1.e38f;

    /* Go through the same pixels to be sampled to find min / max */
    ixUse = 0;
    iyUse = 0;
    for (j = 0; j < nSample; j++) {
      fval = floatp[iyUse + iyStart][ixUse + ixStart];
      minVal = B3DMIN(minVal, fval);
      maxVal = B3DMAX(maxVal, fval);
      ixUse += dxSample;
      while (ixUse >= nxUse) {
        ixUse -= nxUse;
        iyUse++;
      }
    }
    
    nbins = 65536 >> SHORT_SHIFT;
    fPad = 0.01f * (maxVal - minVal);
    if (!fPad)
      fPad = 1.;
    fRange = 2.f * fPad + maxVal - minVal;
    fBase = -(minVal - fPad);
    fFactor = nbins / fRange;
    break;

  default :
    return(2);
  }

  hist = B3DMALLOC(int, nbins);
  if (!hist) 
    return(3);
  for (i = 0; i < nbins; i++)
    hist[i] = 0;
  
  if (dxSample == 1 && ixStart == 0 && iyStart == 0) {
    for (j = 0; j < nyUse; j++) {
      switch (type) {
      case BYTE:
        for (i = 1; i < nxUse; i++) {
          val = bytep[j][i];
          hist[val]++;
        }
        break;

      case SIGNED_SHORT:
        for (i = 1; i < nxUse; i++) {
          val = shortp[j][i];
          hist[(val + 32768) >> SHORT_SHIFT]++;
        } 
        break;
        
      case UNSIGNED_SHORT:
        for (i = 1; i < nxUse; i++) {
          val = ushortp[j][i];
          hist[val >> SHORT_SHIFT]++;
        } 
        break;
        
      case FLOAT:
        for (i = 1; i < nxUse; i++) {
          fval = floatp[j][i];
        val = (int) ((fval + fBase) * fFactor);
        hist[val]++;
        } 
        break;
        
      }
    }
  } else {
    ixUse = 0;
    iyUse = 0;
    
    for (j = 0; j < nSample; j++) {
      
      /* get the value */
      switch (type) {
      case BYTE :
        val = bytep[iyUse + iyStart][ixUse + ixStart];
        hist[val]++;
        break;
        
      case SIGNED_SHORT :
        val = shortp[iyUse + iyStart][ixUse + ixStart];
        hist[(val + 32768) >> SHORT_SHIFT]++;
        break;
        
      case UNSIGNED_SHORT :
        val = ushortp[iyUse + iyStart][ixUse + ixStart];
        hist[val >> SHORT_SHIFT]++;
        break;
        
      case FLOAT:
        fval = floatp[iyUse + iyStart][ixUse + ixStart];
        val = (int) ((fval + fBase) * fFactor);
        hist[val]++;
        break;
      }

      /* move indexes to next spot in use area */
      ixUse += dxSample;
      while (ixUse >= nxUse) {
        ixUse -= nxUse;
        iyUse++;
      }
    }
  }
    
  /* Now we have a histogram, find cumulative sum until get to desired point */
  for (cum = 0, i = 0; i < nbins; i++) {
    cum += hist[i];
    if (cum >= nLo) {
      if (type == FLOAT)
        *scaleLo = ((float)i / fFactor - fBase);
      else
        *scaleLo = (float)(i * factor - base);
      break;
    }
  }
  
  /* At the top end, add back most of the bin width to get the whole bin */
  for (cum = 0, i = nbins - 1; i >= 0; i--) {
    cum += hist[i];
    if (cum >= nHi) {
      if (type == FLOAT)
        *scaleHi = ((float)i / fFactor - fBase);
      else
        *scaleHi = (float)(i * factor - base + (1 << SHORT_SHIFT) - 1);
      break;
    }
  }

  /* It is up to the caller to do something sensible if they end up equal */
  if (*scaleLo >= *scaleHi)
    *scaleLo = *scaleHi = 0.5f * (*scaleLo + *scaleHi);

  free(hist);
  return(0);
}
