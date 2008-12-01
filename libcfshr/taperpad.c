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
#define sliceedgemean SLICEEDGEMEAN
#define splitfill SPLITFILL
#else
#define taperoutpad taperoutpad_
#define taperinpad taperinpad_
#define sliceedgemean sliceedgemean_
#define splitfill splitfill_
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
    if (ifmean)
      dmean = dmeanin;
    else
      dmean = (float)sliceEdgeMean(brray, nxdim, ixlo, ixhi-1,iylo,iyhi-1);
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
  int nxbox, nybox, ixlim, y1, y2;
  float dmean, fracx, fracy, fmin;
  b3dUByte *bytein;
  b3dInt16 *intin;
  b3dUInt16 *uintin;
  float *floatin;
  float *out;
  
  /* ixlo, iylo are last index below image location in output array, 
     ixhi, iyhi are last index within image location */
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
  dmean = (float)sliceEdgeMean(brray, nxdim, ixlo+1, ixhi, iylo+1, iyhi);
    
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


/*!
 * Computes the mean along the edge for the portion of the image in [array]
 * bounded by X indices [ixlo] and [ixhi] and Y indices [iylo] and [iyhi],
 * inclusive.  [nxdim] is the X dimension of the array.  The mean is based
 * solely on the single columns at [ixlo] and [ixhi] and the single rows at 
 * [iylo] and [iyhi].
 */
double sliceEdgeMean(float *array, int nxdim, int ixlo, int ixhi, int iylo,
                    int iyhi)
{
  double dmean, sum = 0.;
  int ix, iy;
  for (ix = ixlo; ix <= ixhi; ix++)
    sum += array[ix + iylo * nxdim] + array[ix + iyhi * nxdim];
      
  for (iy = iylo + 1; iy < iyhi; iy++)
    sum += array[ixlo + iy * nxdim] + array[ixhi + iy * nxdim];
      
  dmean = sum / (2 * (ixhi - ixlo + iyhi - iylo));
  return dmean;
}

/*!
 * Fortran wrapper to @sliceEdgeMean .  The indexes are numbered from 1.
 */
double sliceedgemean(float *array, int *nxdim, int *ixlo, int *ixhi, int *iylo,
                     int *iyhi)
{
  return sliceEdgeMean(array, *nxdim, *ixlo - 1, *ixhi - 1, *iylo - 1, 
                       *iyhi - 1);
} 

/*!
 * Splits a small image in [array] with dimensions [nxbox] by [nybox] into the
 * 4 corners of a potentially larger array.  The padded image is placed in 
 * [brray] with a size of [nx] by [ny], where [nxdim] is the X dimension of 
 * [brray].  The image will be padded with the mean at the edge of the image,
 * or with the value in [fillin] if [iffill] is non-zero.  In either case the
 * values will be shifted so that the mean of the whole array is zero.
 */
void sliceSplitFill(float *array, int nxbox, int nybox, float *brray,
               int nxdim, int nx, int ny, int iffill, float fillin)
{
  int i, ix, iy, ixlo, iylo, ixnew, iynew;
  float sum, fill, bias, dmean, edge;
  sum = 0.;
  for (i = 0; i < nxbox * nybox; i++)
    sum += array[i];
  dmean = (float)(sum / (nxbox * nybox));
  fill=dmean;
  bias=dmean;
  if (nxbox != nx || nybox != ny) {
    fill = fillin;
    if (!iffill) {

      /* find mean of edge of box */
      sum=0.;
      for (ix=0; ix < nxbox; ix++)
        sum += array[ix]+array[ix + (nybox - 1) * nxbox];
      for (iy=1; iy < nybox-1; iy++) 
        sum += array[iy * nxbox] + array[nxbox - 1 + iy * nxbox];
      fill = sum / (2 * nxbox + 2 * (nybox - 2));
    }

    /* Whatever the fill, subtract a bias that would produce a mean of zero */
    bias = fill+(dmean-fill)*((float)(nxbox*nybox))/((float)(nx*ny));

    /* fill whole brray with fill-bias  */
    for (iy = 0; iy < ny; iy++)
      for (ix = 0; ix < nx; ix++)
        brray[ix + iy * nxdim] = fill-bias;
  }

  /* move array into brray, splitting it into the 4 corners of brray */
  ixlo = -nxbox/2;
  iylo = -nybox/2;
  for (iy = 0; iy < nybox; iy++) {
    for (ix = 0; ix < nxbox; ix++) {
      ixnew = (ix + ixlo + nx) % nx;
      iynew = (iy + iylo + ny) % ny;
      brray[ixnew + iynew * nxdim] = array[ix + iy * nxbox] - bias;
    }
  }  
}

/*!
 * Fortran wrapper to @splitFill .
 */
void splitfill(float *array, int *nxbox, int *nybox, float *brray,
               int *nxdim, int *nx, int *ny, int *iffill, float *fillin)
{
  sliceSplitFill(array, *nxbox, *nybox, brray, *nxdim, *nx, *ny, *iffill,
                 *fillin);
}

/*
  $Log$
  Revision 1.5  2007/10/20 20:03:08  mast
  Oops, subtract not add 1 for fortran indices

  Revision 1.4  2007/10/20 03:04:07  mast
  Added fortran wrapper for edgemean

  Revision 1.3  2007/10/12 04:16:34  mast
  Added edge mean function and used it in pad functions
  
  Revision 1.2  2007/10/01 16:22:12  mast
  Fixed wrapper name from tst

  Revision 1.1  2007/10/01 15:26:09  mast
  *** empty log message ***

*/
