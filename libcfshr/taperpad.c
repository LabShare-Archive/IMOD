/*
 * taperpd.c - Taper and pad a slice inside or out
 *
 * Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 * Log at end of file
 */

#include "mrcslice.h"
#include "cfsemshare.h"
#include "imodconfig.h"

#ifdef F77FUNCAP
#define taperoutpad TAPEROUTPAD
#define taperinpad TAPERINPAD
#else
#define taperoutpad taperoutpad_
#define taperinpad taperinpadtst_
#endif

/*!
 * Pads an image in [array] with dimensions [nxbox] by [nybox] into the center
 * of a larger array, [brray], which can be the same as [array].  The 
 * SLICE_MODE is specified by [type], which must be byte, float or short 
 * integer. The size of the padded image is specified by [nx] and [ny] while 
 * [nxdim] specifies the X dimension of the output array.  The values of the 
 * image in the padding area will be tapered from the value of a pixel at the 
 * edge of the input image edge down to a common value.  That value is either
 * the mean at the edge of the input image, if [ifmean] is zero, or the value
 * supplied in [dmeanin], if [ifmean] is nonzero.
 */
void sliceTaperOutPad(void *array, int type, int nxbox, int nybox, 
                      float *brray, int nxdim, int nx, int ny, int ifmean,
                      float dmeanin)
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
    if (ifmean) {
      dmean = dmeanin;
    } else {
      sum=0.;
      for (ix = ixlo; ix < ixhi; ix++)
        sum += brray[ix + iylo * nxdim] + brray[ix + (iyhi - 1) * nxdim];
      
      for (iy = iylo + 1; iy < iyhi - 1; iy++)
        sum += brray[ixlo + iy * nxdim] + brray[ixhi - 1 + iy * nxdim];
      
      dmean = sum / (2 * (nxbox + nybox - 2));
    }
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
        wmean = 1.f - wedge;
        brray[ix + iy * nxdim] = wmean * dmean + wedge * edgel;
        brray[nxtop - ix + iy * nxdim] = wmean * dmean + wedge * edger;
      }
    }

    for (iy = 0; iy < iylo; iy++) {
      wedge = ((float)iy) / iylo;
      prodmean = (1.f - wedge) * dmean;
      iytoplin = nytop - iy;
      for (ix = 0; ix < nx; ix++) {
        brray[ix + iy * nxdim] = prodmean + wedge * brray[ix + iylo * nxdim];
        brray[ix + iytoplin * nxdim] = prodmean + 
          wedge * brray[ix + (iyhi - 1) * nxdim];
      }
    }
  }
}

/*!
 * Fortran wrapper to call @sliceTaperOutPad with a floating point array.
 */
void taperoutpad(void *array, int *nxbox, int *nybox, float *brray, int *nxdim,
                 int *nx, int *ny, int *ifmean, float *dmeanin)
{
  sliceTaperOutPad(array, SLICE_MODE_FLOAT, *nxbox, *nybox,
                   brray, *nxdim, *nx, *ny, *ifmean,
                   *dmeanin);
}

/*!
 * Extracts a subarea of an image, places it into the center of a potentially 
 * larger array with padding, and tapers the image down to the mean value at
 * its edge, tapering pixels inside the extracted image area.  The image data
 * are in [array], their X dimension is [nxdimin], and their SLICE_MODE is 
 * given in [type].  The starting and ending coordinates to extract in X and Y
 * are [ix0] to [ix1] and [iy0] to [iy1].  The output image array is [brray] 
 * and its X dimension is specified by [nxdim].  The padded image size is 
 * specified by [nx] and [ny], and [nxtap] and [nytap] indicate the number of
 * pixels over which to taper in X and Y.  The output array can be the same as
 * the input array if the input image fills the entire array.
 */
void sliceTaperInPad(void *array, int type, int nxdimin, int ix0, int ix1,
                     int iy0, int iy1, float *brray, int nxdim, int nx, int ny,
                     int nxtap, int nytap)
{
  int lobase, hibase, x1, x2, ixlo, ixhi, iylo, iyhi, ix, iy, ixbase;
  int nsum, nxbox, nybox, ixlim, y1, y2;
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
  
  for (iy = iy1; iy >= iy0; iy--) {
    out = brray + ixhi + (iylo + 1 + iy - iy0) * nxdim;
    switch (type) {
    case SLICE_MODE_BYTE:
      bytein = (b3dUByte *)array + ix1 + iy * nxdimin;
      for (ix = ix1; ix >= ix0; ix--)
        *out-- = *bytein--;
      break;
      
    case SLICE_MODE_SHORT:
      intin = (b3dInt16 *)array + ix1 + iy * nxdimin;
      for (ix = ix1; ix >= ix0; ix--)
        *out-- = *intin--;
      break;
         
    case SLICE_MODE_USHORT:
      uintin = (b3dUInt16 *)array + ix1 + iy * nxdimin;
      for (ix = ix1; ix >= ix0; ix--)
        *out-- = *uintin--;
      break;
         
    case SLICE_MODE_FLOAT:
      floatin = (float *)array + ix1 + iy * nxdimin;
      for (ix = ix1; ix >= ix0; ix--)
        *out-- = *floatin--;
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
      fracy=(iy + 1.f)/(nytap+1.f);
      ixlim = (nxbox+1)/2;
    }
    for (ix = 0; ix < ixlim; ix++) {
      fracx=1.;
      if (ix < nxtap)
        fracx=(ix + 1.f)/(nxtap+1.f);
      fmin=fracx < fracy ? fracx : fracy;
      if(fmin < 1.) {
        x1 = ix + 1 + ixlo;
        x2 = ixhi - ix;
        y1 = iy + 1 + iylo;
        y2 = iyhi - iy;

        /*	DNM 4/28/02: for odd box sizes, deflect middle pixel to edge */
        /* to keep it from being attenuated twice */
        if (x1 == x2) 
          x2 = 0;
        if (y1 == y2) 
          y2 = 0;
            
        lobase = y1 * nxdim;
        hibase = y2 *nxdim;
        brray[x1 + lobase]=fmin*(brray[x1 + lobase]-dmean)+dmean;
        brray[x1 + hibase]=fmin*(brray[x1 + hibase]-dmean)+dmean;
        brray[x2 + lobase]=fmin*(brray[x2 + lobase]-dmean)+dmean;
        brray[x2 + hibase]=fmin*(brray[x2 + hibase]-dmean)+dmean;
      }
    }
  }
}

/*!
 * Fortran wrapper to @sliceTaperInPad for padding a whole float image array
 * of size [nxbox] by [nybox].  The output array can be the same as
 * the input array.
 */
void taperinpad(void *array, int *nxbox, int *nybox, float *brray, int *nxdim,
                int *nx, int *ny, int *nxtap, int *nytap)
{
  sliceTaperInPad(array, SLICE_MODE_FLOAT, *nxbox, 0, *nxbox - 1, 0, 
                  *nybox - 1, brray, *nxdim, *nx, *ny, *nxtap, *nytap);
}

/*  $Log$


*/
