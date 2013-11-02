/*
 * taperpd.c - Taper and pad a slice inside or out
 *
 * Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full notice.
 *
 * $Id$
 */

#include "mrcslice.h"
#include "imodconfig.h"
#include "b3dutil.h"

#ifdef F77FUNCAP
#define taperoutpad TAPEROUTPAD
#define smoothoutpad SMOOTHOUTPAD
#define taperinpad TAPERINPAD
#define taperinpadex TAPERINPADEX
#define sliceedgemean SLICEEDGEMEAN
#define splitfill SPLITFILL
#else
#define taperoutpad taperoutpad_
#define smoothoutpad smoothoutpad_
#define taperinpad taperinpad_
#define taperinpadex taperinpadex_
#define sliceedgemean sliceedgemean_
#define splitfill splitfill_
#endif

static void copyToCenter(void *array, int type, int nxbox, int nybox,
                         float *brray, int nxdim, int nx, int ny, int *ixlo,
                         int *ixhi, int *iylo, int *iyhi);

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
  int ixlo, ixhi, iylo, iyhi, ix, iy, nxtop, nytop, iytoplin;
  float dmean, edgel, edger, wedge, wmean, prodmean;

  copyToCenter(array, type, nxbox, nybox, brray, nxdim, nx, ny, &ixlo, &ixhi,
               &iylo, &iyhi);

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

/* Common function for copying the box to the center of the array and returning
   limiting coordinates */
static void copyToCenter(void *array, int type, int nxbox, int nybox,
                         float *brray, int nxdim, int nx, int ny, int *ixlo,
                         int *ixhi, int *iylo, int *iyhi)
{
  b3dUByte *bytein;
  b3dInt16 *intin;
  b3dUInt16 *uintin;
  float *floatin;
  float *out;
  int  ix, iy;

  *ixlo = (nx - nxbox) / 2;
  *ixhi = *ixlo + nxbox;
  *iylo = (ny - nybox) / 2;
  *iyhi = *iylo + nybox;
  for (iy = nybox - 1; iy >= 0; iy--) {
    out = brray + *ixhi + (*iylo + iy) * nxdim - 1;
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
 * given in [type].  The mode can be byte, signed or unsigned short, float, or
 * RGB.  The starting and ending coordinates to extract in X and Y
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

    case SLICE_MODE_RGB:
      bytein = (b3dUByte *)array + 3 * (ix1 + iy * nxdimin);
      for (ix = ix1; ix >= ix0; ix--) {
        *out-- = bytein[0] + bytein[1] + bytein[2];
        bytein -= 3;
      }
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
 * Fortran wrapper to @sliceTaperInPad for padding a subarea from a float image array
 */
void taperinpadex(void *array, int *nxdimin, int *ix0, int *ix1,
                     int *iy0, int *iy1, float *brray, int *nxdim, int *nx, int *ny,
                     int *nxtap, int *nytap)
{
  sliceTaperInPad(array, SLICE_MODE_FLOAT, *nxdimin, *ix0, *ix1, *iy0, 
                  *iy1, brray, *nxdim, *nx, *ny, *nxtap, *nytap);
}

/*!
 * Pads an image in [array] with dimensions [nxbox] by [nybox] into the center
 * of a larger array, [brray], which can be the same as [array].  The 
 * SLICE_MODE is specified by [type], which must be byte, float or short 
 * integer. The size of the padded image is specified by [nx] and [ny] while 
 * [nxdim] specifies the X dimension of the output array.  The padding is done
 * by replicating pixels and smoothing lines progressively more for farther
 * out from the pixels in the box.  This is done in a series of progressively
 * larger rectangles; the each pixel in first rectangle contains the average
 * of the three nearest pixels on the outer edge of the actual data; each pixel
 * in the second rectangle is the average of the 5 nearest pixels in the first
 * rectangle, etc.
 */
void sliceSmoothOutPad(void *array, int type, int nxbox, int nybox, 
                         float *brray, int nxdim, int nx, int ny)
{
  int ixlo, ixhi, iylo, iyhi, ix, iy, x, y, dirx, diry, nsum, numPad, ipad;
  int xmin, xlimlo, xmax, xlimhi, xLineLo, xLineHi, xside, xstart, xend;
  int ymin, ylimlo, ymax, ylimhi, yLineLo, yLineHi, yside, ystart, yend;
  float sum;

  copyToCenter(array, type, nxbox, nybox, brray, nxdim, nx, ny, &ixlo, &ixhi,
               &iylo, &iyhi);
  numPad = b3dIMax(4, ixlo, nx - ixhi, iylo, ny - iyhi);
  /* printf("%d %d %d %d\n", ixlo, ixhi, iylo, iyhi); */
  for (ipad = 1; ipad <= numPad; ipad++) {
    xLineLo = ixlo - ipad;
    xLineHi = ixhi + ipad - 1;
    xmin = B3DMAX(0, xLineLo);
    xlimlo = B3DMAX(0, xLineLo + 1);
    xmax = B3DMIN(nx - 1, xLineHi);
    xlimhi = B3DMIN(nx - 1, xLineHi - 1);

    yLineLo = iylo - ipad;
    yLineHi = iyhi + ipad - 1;
    ymin = B3DMAX(0, yLineLo);
    ylimlo = B3DMAX(0, yLineLo + 1);
    ymax = B3DMIN(ny - 1, yLineHi);
    ylimhi = B3DMIN(ny - 1, yLineHi - 1);
    /* printf("ipad %d X: %d %d %d %d %d %d\n", ipad, xLineLo, xLineHi, xmin,
       xmax, xlimlo, xlimhi);
       printf("ipad %d Y: %d %d %d %d %d %d\n", ipad, yLineLo, yLineHi, ymin,
       ymax, ylimlo, ylimhi); */

    diry = 1;
    for (iy = yLineLo; iy <= yLineHi; iy += yLineHi - yLineLo) {
      if (iy < 0 || iy >= ny) {
        diry = -1;
        continue;
      }
      for (ix = xmin; ix <= xmax; ix++) {
        xside = -1;
        xstart = ix - ipad;
        if (xstart < xlimlo) {
          xstart = xlimlo;
          if (xstart) {
            ystart = iy + 2 * diry;
            yend = ystart + diry * (xstart - (ix - ipad) - 1);
            yend = B3DMAX(ylimlo + 1, B3DMIN(ylimhi - 1, yend));
            if (diry * ystart <= diry * yend)
              xside = xstart;
          }
        }
        xend = ix + ipad;
        if (xend > xlimhi) {
          xend = xlimhi;
          if (xend < nx && xside < 0) {
            ystart = iy + 2 * diry;
            yend = ystart + diry * (ix + ipad - xend - 1);
            yend = B3DMAX(ylimlo + 1, B3DMIN(ylimhi - 1, yend));
            if (diry * ystart <= diry * yend)
              xside = xend;
          }
        }

        sum = 0.;
        nsum = xend + 1 - xstart;
        /* printf("ix %d iy %d xs %d xe %d xside %d diry %d\n", ix, iy, 
           xstart, xend, xside, diry); */
        for (x = xstart; x <= xend; x++)
          sum += brray[x + nxdim * (iy + diry)];
        if (xside >= 0) {
          /* printf("ys %d  ye %d  nsumst %d sumst %.1f", ystart, yend, sum, 
             nsum); */
          for (y = ystart; diry * y <= diry * yend; y+= diry) {
            sum += brray[xside + nxdim * y];
            nsum++;
          }
          /* printf(" %.1f / %d = %.1f\n", sum, nsum, sum / nsum); */
        }
        brray[ix + nxdim * iy] = sum / nsum;
      }
      diry = -1;
    }

    dirx = 1;
    for (ix = xLineLo; ix <= xLineHi; ix += xLineHi - xLineLo) {
      if (ix < 0 || ix >= nx) {
        dirx = -1;
        continue;
      }
      for (iy = ymin; iy <= ymax; iy++) {
        yside = -1;
        ystart = iy - ipad;
        if (ystart < ylimlo) {
          ystart = ylimlo;
          if (ystart) {
            xstart = ix + 2 * dirx;
            xend = xstart + dirx * (ystart - (iy - ipad) - 1);
            xend = B3DMAX(xlimlo + 1, B3DMIN(xlimhi - 1, xend));
            if (dirx * xstart <= dirx * xend)
              yside = ystart;
          }
        }
        yend = iy + ipad;
        if (yend > ylimhi) {
          yend = ylimhi;
          if (yend < ny && yside < 0) {
            xstart = ix + 2 * dirx;
            xend = xstart + dirx * (iy + ipad - yend - 1);
            xend = B3DMAX(xlimlo + 1, B3DMIN(xlimhi - 1, xend));
            if (dirx * xstart <= dirx * xend)
              yside = yend;
          }
        }

        sum = 0.;
        nsum = yend + 1 - ystart;
        /* printf("ix %d iy %d ys %d ye %d yside %d dirx %d\n", ix, iy, 
           ystart, yend, yside, dirx); */
        for (y = ystart; y <= yend; y++)
          sum += brray[ix + dirx + nxdim * y];
        if (yside >= 0) {
          /* printf("xs %d  xe %d  nsumst %d sumst %.1f", xstart, xend, sum, 
             nsum); */
          for (x = xstart; dirx * x <= dirx * xend; x+= dirx) {
            sum += brray[x + nxdim * yside];
            nsum++;
          }
          /* printf(" %.1f / %d = %.1f\n", sum, nsum, sum / nsum); */
        }
        brray[ix + nxdim * iy] = sum / nsum;
      }
      dirx = -1;
    }
  }
}

/*!
 * Fortran wrapper to call @sliceSmoothOutPad with a floating point array.
 */
void smoothoutpad(void *array, int *nxbox, int *nybox, float *brray, 
                  int *nxdim, int *nx, int *ny)
{
  sliceSmoothOutPad(array, SLICE_MODE_FLOAT, *nxbox, *nybox,
                    brray, *nxdim, *nx, *ny);
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
  float sum, fill, bias, dmean;
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
 * Fortran wrapper to @sliceSplitFill .
 */
void splitfill(float *array, int *nxbox, int *nybox, float *brray,
               int *nxdim, int *nx, int *ny, int *iffill, float *fillin)
{
  sliceSplitFill(array, *nxbox, *nybox, brray, *nxdim, *nx, *ny, *iffill,
                 *fillin);
}
