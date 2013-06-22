/* zoomdown.c : reduces images with selectable interpolation filter
 *
 * Author: David Mastronarde   email: mast@colorado.edu
 *
 * Copyright (C) 2011 by Boulder Laboratory for 3-Dimensional Electron
 * Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 * Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * These routines are based heavily on the zoom program by Paul Heckbert, with
 * many function and variable names, some comments and some code copied with appropriate 
 * modifications and simplifications for the tasks needed in IMOD.  Here is the notice
 * from that source:
 *
 * Copyright (c) 1989  Paul S. Heckbert
 * This source may be used for peaceful, nonprofit purposes only, unless
 * under licence from the author. This notice should remain in the source.
 *
 * A version of zoom that builds under Visual Studio and runs on 32-bit Windows (with 
 * one fix needed to write gray-scale tiff files) is available from:
 * http://www.xmission.com/~legalize/zoom.html
 *
 * Some other useful comments from that source:
 *
 * Filters with an arbitrary separable finite impulse response filter.
 * (A filter h(x,y) is called separable if h(x,y)=f(x)*g(y)).
 * The program makes one pass over the image using a moving window of
 * scanlines, so memory usage is proportional to imagewidth*filterheight,
 * not imagewidth*imageheight, as you'd get if the entire image were
 * buffered.  The separability of the filter is also exploited, to get
 * a cpu time proportional to npixels*(filterwidth+filterheight),
 * not npixels*filterwidth*filterheight as with direct 2-D convolution.
 *
 * An important fine point on terminology: there are two kinds of pixel coords:
 *  DISCRETE COORDINATES take on integer values at pixel centers
 *  CONTINUOUS COORDINATES take on integer values halfway between pixels
 * For example, an image with discrete coordinate range [0..511] has a
 * continuous coordinate range of [0..512].  The pixel with
 * discrete coords (x,y) has its center at continuous coords (x+.5,y+.5)
 * and its continuous coord domain is [x..x+1] in X and [y..y+1] in Y.
 * Note: discrete coords are not always stored in ints and continuous coords
 * are not always stored in floats.
 *
 * conversion:
 * if c = continuous coord and d = discrete coord, then
 *  c = d+.5
 *  d = floor(c)
 *
 * Notation: prefix 'a' denotes source coords, 'b' denotes destination coords
 *
 * $Id$
 */

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "b3dutil.h"

#ifdef F77FUNCAP
#define selectzoomfilter SELECTZOOMFILTER
#define selectzoomfilterxy SELECTZOOMFILTERXY
#define zoomwithfilter ZOOMWITHFILTER
#define zoomfiltinterp ZOOMFILTINTERP
#define zoomfiltvalue ZOOMFILTVALUE
#else
#define selectzoomfilter selectzoomfilter_
#define selectzoomfilterxy selectzoomfilterxy_
#define zoomwithfilter zoomwithfilter_
#define zoomfiltinterp zoomfiltinterp_
#define zoomfiltvalue zoomfiltvalue_
#endif

typedef double (*fn_proc)(double x);

typedef struct {/* A 1-D FILTER */
  fn_proc func;  /* filter function */
  double supp; /* radius of nonzero portion */
} Filt;

typedef struct { /* SAMPLED FILTER WEIGHT TABLE */
  int i0, i1;  /* range of samples is [i0..i1-1] */
  union {
    short *s;
    float *f;
  } weight;   /* weight[i] goes with pixel at i0+i */
} Weighttab;

#define PEG(val, t) (t = (val), t < 0 ? 0 : t > 255 ? 255 : t)
#define PI 3.14159265358979323846264338

#define CHANBITS   8
#define WEIGHTBITS  14          /* # bits in filter coefficients */
#define FINALSHIFT  (2*WEIGHTBITS-CHANBITS) /* shift after x&y filter passes */
#define WEIGHTONE  (1<<WEIGHTBITS)  /* filter weight of one */

static void interpLimits(int na, int nb, float cen, float trans, double scale, 
                         float *aOff, int *bSize, int *bOff);
static void scanline_accum(unsigned char *lineb, int dtype, int aXsize, 
                           b3dInt32 *accumBuf, b3dInt16 sweight, float dweight);
static void scanline_filter(b3dInt32 *lineb, int dtype, int aXsize, 
                            unsigned char *obufb, int bXsize, Weighttab *wtab, int shift);
static void scanline_remap(unsigned char *filtBuf, int dtype, int bXsize, 
                           unsigned char *obufb, b3dUInt32 *cindex, 
                           unsigned char *bindex);
static void make_weighttab(int b, double cen, int len, double scale, double support, 
                           int dtype, Weighttab *wtab);
static void mitchell_init(double b, double c);
static double filt_binning(double x);
static double filt_mitchell(double x);
static double filt_triangle(double x);
static double filt_blackman(double x);
static double filt_lanczos2(double x); 
static double filt_lanczos3(double x); 

#define MAX_THREADS 16
#define NUM_FILT 6

static Filt filters[NUM_FILT] = {
  {filt_binning, 0.5},
  {filt_blackman, 1.},
  {filt_triangle, 1.},
  {filt_mitchell, 2.},
  {filt_lanczos2, 2.},
  {filt_lanczos3, 3.}
};

static int zoom_debug = 0;  /* debug level: 0=none, 2=filters */

/* ZOOM-SPECIFIC FILTER PARAMETERS */
static double sXsupport, sYsupport;  /* scaled filter support radius */
static double sXscale, sYscale;    /* filter scale (spacing between centers in a space) */
static int sXwidth, sYwidth;       /* filter width: max number of nonzero samples */
static float sValueScaling = 1.0;  /* Scaling factor for image values */

/* The selected filter function */
static fn_proc sFilt_func = NULL;
static double sMitchP0, sMitchP2, sMitchP3, sMitchQ0, sMitchQ1, sMitchQ2, sMitchQ3;

/*!
 * Selects which filter to use in image reduction with [type] and sets the scaling factor
 * with [zoom], a value that must be less than 1.  The type can be 0 for a box 
 * (equivalent to binning), 1 for a Blackman window, 2 for a triangle filter, 3 for a
 * Mitchell filter, or 4 or 5 for Lanczos 2 or Lanczos 3.  The total 
 * width of the filter in the source image is returned in [outWidth].  Returns 1 for
 * a filter type out of range or 2 for a zoom out of range.
 */
int selectZoomFilter(int type, double zoom, int *outWidth)
{
  sFilt_func = NULL;
  if (type < 0 || type >= NUM_FILT)
    return 1;
  if (zoom >= 1. || zoom <= 0.)
    return 2;
  sFilt_func = filters[type].func;
  sXscale = sYscale = 1. / zoom;
  sXsupport = sYsupport = filters[type].supp * sXscale;
  sXwidth = sYwidth = (int)ceil(2. * sXsupport);
  *outWidth = sXwidth;
  if (type == 3)
    mitchell_init(1./3., 1./3.);
  return 0;
}

/*!
 * Fortran wrapper for @selectZoomFilter
 */
int selectzoomfilter(int *type, float *zoom, int *outWidth)
{
  return selectZoomFilter(*type, (double)(*zoom), outWidth);
}

/*!
 * Like @selectZoomFilter but it allows separate zooms in X and Y given by [xzoom] and
 * [yzoom], with respective filter widths returned in [outWidthX] and [outWidthY].
 */
int selectZoomFilterXY(int type, double xzoom, double yzoom, int *outWidthX, 
                       int *outWidthY)
{
  int err = selectZoomFilter(type, xzoom, outWidthX);
  if (err)
    return err;
  sYscale = 1. / yzoom;
  sYsupport = filters[type].supp * sYscale;
  sYwidth = (int)ceil(2. * sYsupport);
  *outWidthY = sYwidth;
  return 0;
}

/*!
 * Fortran wrapper for @selectZoomFilterXY
 */
int selectzoomfilterXY(int *type, float *xzoom, float *yzoom, int *outWidthX,
                       int *outWidthY)
{
  return selectZoomFilterXY(*type, (double)(*xzoom), (double)(*yzoom), outWidthX, 
                          outWidthY);
}

/*!
 * Sets a factor for scaling the output values from image reduction of data types other
 *  than bytes and RGB.
 */
void setZoomValueScaling(float factor)
{
  sValueScaling = factor;
}

/*!
 * Reduces an image using the interpolation filter and zoom specified in 
 * @@selectZoomFilter@. ^
 * [slines] - array of line pointers for the input image ^
 * [aXsize], [aYsize] - size of input image ^
 * [aXoff], [aYoff] - coordinate in the input image at which the lower left edge of the 
 * lower left pixel of the output starts, where input coordinates are zero at the lower
 * left edge of the first input pixel ^
 * [bXsize], [bYsize] - size of output image to be created, potentially from a subset of 
 * the input in X or Y and into a subset of the output array ^
 * [bXdim] - X dimension of the full output image array ^
 * [bXoff] - Index of first pixel in X to fill in output array ^
 * [dtype] - Type of data, a SLICE_MODE_... value.  BYTE, FLOAT, RGB, SHORT, and USHORT 
 * are allowed ^
 * [outData] - Output array, or address of first line to fill in a larger output array.
 * The array is the same data type as the input unless mapping is being done, in which 
 * case it must be unsigned integers. ^
 * [cindex] - Unsigned integer RGBA values to map byte or short data into, or NULL for
 * no mapping ^
 * [bindex] - Byte values to map RGB data into, or NULL for no mapping.  Each channel is 
 * mapped the same and a fourth channel is added with zero. ^
 * Returns 1 if no filter has been selected, 2 for an unsupported data type, 3 for an
 * attempt to map float data, 4 if needed input coordinates to compose the output image go
 * out of range, or 5 for a memory allocation error. ^
 * This function is parallelized with OpenMP and uses the same formula as @cubinterp for
 * reducing the number of threads for small images.
 */
int zoomWithFilter(unsigned char **slines, int aXsize, int aYsize, float aXoff,
                   float aYoff, int bXsize, int bYsize, int bXdim, int bXoff, int dtype,
                   void *outData, b3dUInt32 *cindex, unsigned char *bindex)
{
  Weighttab *xweights;  /* sampled filter at each dst x pos; for xfilt*/
  b3dInt16 *xweightSbuf, *xwp;  /* big block of memory addressed by xweights */
  Weighttab yweight[MAX_THREADS];       /* a single sampled filter for current y pos */
  b3dFloat *xweightFbuf, *xwfp;
  b3dInt32 *accumBuf[MAX_THREADS];
  unsigned char *filtBuf[MAX_THREADS];
  unsigned char *lineb, *obufb;
  int mapping = 0;
  int numThreads;
  int psizeAccum, psizeWgt, psizeFilt, bx, by, i, ayf, thr;
  float fweight = 0.;
  b3dInt16 sweight = 0;

  if (!sFilt_func)
    return 1;

  psizeAccum = 4;
  psizeWgt = 2;
  switch (dtype) {
  case SLICE_MODE_BYTE:
    if (cindex)
      mapping = 1;
    psizeFilt = 1;
    break;
  case SLICE_MODE_RGB:
    if (bindex)
      mapping = 1;
    psizeAccum = 12;
    psizeFilt = 3;
    break;
  case SLICE_MODE_FLOAT:
    if (cindex || bindex)
      return 3;
    psizeFilt = 4;
    psizeWgt = 4;
    break;
  case SLICE_MODE_SHORT:
  case SLICE_MODE_USHORT:
    if (cindex)
      mapping = 1;
    psizeFilt = 2;
    psizeWgt = 4;
    break;
  default:
    return 2;
  }

  if (aXoff < 0. || aYoff < 0. || (int)(aXoff + bXsize * sXscale) > aXsize ||
      (int)(aYoff + bYsize * sYscale) > aYsize) {
    /* printf("axo %f bxs %d sc %f xe %f axs %d   ayo %f bys %d ye %f ays %d\n", aXoff,
           bXsize, sXscale, aXoff + bXsize * sXscale, aXsize, aYoff, bYsize,
           aYoff + bYsize * sYscale, aYsize); */
    return 4;
  }

  numThreads = B3DNINT(0.04 * sqrt((double)aXsize * aYsize));
  numThreads = numOMPthreads(B3DMIN(numThreads, MAX_THREADS));

  /* Allocate accumulation, filter output, and weight buffers */
  xweights = B3DMALLOC(Weighttab, bXsize);
  xweightSbuf = (b3dInt16 *)malloc(bXsize * sXwidth * psizeWgt);
  if (!xweightSbuf || !xweights) {
    B3DFREE(xweights);
    B3DFREE(xweightSbuf);
    return 5;
  }

  for (i = 0; i < numThreads; i++) {
    filtBuf[i] = NULL;
    accumBuf[i] = (b3dInt32 *)malloc(aXsize * psizeAccum);
    yweight[i].weight.s = (b3dInt16 *)malloc(sYwidth * psizeWgt);
    if (mapping)
      filtBuf[i] = (unsigned char *)malloc(bXsize * psizeFilt);
    if (!accumBuf[i] || !yweight[i].weight.s || (mapping && !filtBuf[i])) {
      for (by = 0; by <= i; by++) {
        B3DFREE(yweight[by].weight.s);
        B3DFREE(accumBuf[by]);
        B3DFREE(filtBuf[by]);
      }
      return 5;
    }
  }
  xweightFbuf = (b3dFloat *)xweightSbuf;

  /*
   * prepare a weighttab (a sampled filter for source pixels) for
   * each dest x position
   */
  xwfp = xweightFbuf;
  xwp = xweightSbuf;
  for (bx = 0; bx < bXsize; bx++, xwp += sXwidth, xwfp += sXwidth) {
    if (psizeWgt == 4)
      xweights[bx].weight.f = xwfp;
    else
      xweights[bx].weight.s = xwp;
    make_weighttab(bx, aXoff + (bx + 0.5) * sXscale, aXsize, sXscale, sXsupport, dtype,
                   &xweights[bx]);
  }

#pragma omp parallel for num_threads(numThreads)                    \
  shared(bYsize, aYoff, sYscale, aYsize, dtype, yweight, psizeAccum, accumBuf, \
         aXsize, slines, outData, psizeFilt, bXsize, mapping, filtBuf, xweights, \
         cindex, bindex, sValueScaling, sYsupport)                     \
  private(by, thr, i, ayf, sweight, fweight, lineb, obufb)
  for (by = 0; by < bYsize; by++) {
    thr = b3dOMPthreadNum();
    
    /* prepare a weighttab for dest y position by */
    make_weighttab(by, aYoff + (by + 0.5) * sYscale, aYsize, sYscale, sYsupport, dtype,
                   &yweight[thr]);
    
    /* Zero the scanline accum buffer */
    for (i = 0; i < aXsize * psizeAccum / 4; i++)
      accumBuf[thr][i] = 0;
    
    /* loop over source scanlines that influence this dest scanline */
    for (ayf = yweight[thr].i0; ayf < yweight[thr].i1; ayf++) {
      if (psizeWgt == 2)
        sweight = yweight[thr].weight.s[ayf-yweight[thr].i0];
      else
        fweight = yweight[thr].weight.f[ayf-yweight[thr].i0] * sValueScaling;
      
      lineb = (unsigned char *)slines[ayf];
      /* add weighted tbuf into accum (these do yfilt) */
      scanline_accum(lineb, dtype, aXsize, accumBuf[thr], sweight, fweight);
    }
    
    /* and filter it into the appropriate line of output or into filtBuf */
    obufb = (unsigned char *)outData + psizeFilt * (by * bXdim + bXoff);
    if (mapping)
      obufb = filtBuf[thr];
    /*for (i = 30; i < 45; i++)
      printf("%.1f ", *((float *)accumBuf[thr] + i));
      printf("\n"); */
    scanline_filter(accumBuf[thr], dtype, aXsize, obufb, bXsize, xweights, FINALSHIFT);
    
    /* Map to RGBA output, always 4 bytes, if index tables provided */
    if (mapping) {
      obufb = (unsigned char *)outData + 4 * (by * bXdim + bXoff);
      scanline_remap(filtBuf[thr], dtype, bXsize, obufb, cindex, bindex);
    }
  }    
  
  B3DFREE(xweights);
  B3DFREE(xweightSbuf);
  for (by = 0; by < numThreads; by++) {
    B3DFREE(yweight[by].weight.s);
    B3DFREE(accumBuf[by]);
    B3DFREE(filtBuf[by]);
  }
  return 0;
}

/*!
 * Fortran wrapper for @zoomWithFilter
 */
int zoomwithfilter(float *array, int *aXsize, int *aYsize, float *aXoff,
                   float *aYoff, int *bXsize, int *bYsize, int *bXdim, int *bXoff,
                   float *outData)
{
  int i;
  unsigned char **linePtrs = makeLinePointers(array, *aXsize, *aYsize, 4);
  if (!linePtrs)
    return 5;
  i = zoomWithFilter(linePtrs, *aXsize, *aYsize, *aXoff, *aYoff, *bXsize, *bYsize, *bXdim,
                     *bXoff, SLICE_MODE_FLOAT, outData, NULL, NULL);
  free(linePtrs);
  return i;
}

/*!
 * Performs image reduction using @zoomWithFilter and can be used the same way as 
 * @cubinterp could be for scaling an image down and shifting it.  The zoom and filter
 * must already be specified with @selectZoomFilter .  ^
 *   [array] - The input image array  ^
 *   [bray] - The output image array  ^
 *   [nxa],[nya] - The dimensions of [array]  ^
 *   [nxb],[nyb] - The dimensions of [bray]  ^
 *   [xc],[yc] - The coordinates of the center of [array]  ^
 *   [xt],[yt] - The translation to add to the final image. The
 * center of the output array is taken as [nxb] / 2., [nyb] / 2.  ^
 *   [dmean] - Mean intensity of image or other value with which to fill empty 
 * image area  ^
 * Returns the same error values as @zoomWithFilter
 */
int zoomFiltInterp(float *array, float *bray, int nxa, int nya, int nxb, int nyb,
                   float xc, float yc, float xt, float yt, float dmean)
{
  int i, bXsize, bYsize, bXoff, bYoff, ix, iy;
  float aXoff, aYoff;
  unsigned char **linePtrs = makeLinePointers(array, nxa, nya, 4);
  if (!linePtrs)
    return 5;
  interpLimits(nxa, nxb, xc, xt, sXscale, &aXoff, &bXsize, &bXoff);
  interpLimits(nya, nyb, yc, yt, sYscale, &aYoff, &bYsize, &bYoff);
  /* printf("%f %d %d  %f %d %d\n", aXoff, bXsize, bXoff, aYoff, bYsize, bYoff); */
  if (bXsize > 0 && bYsize > 0) {
    i = zoomWithFilter(linePtrs, nxa, nya, aXoff, aYoff, bXsize, bYsize, nxb, bXoff,
                       SLICE_MODE_FLOAT, &bray[nxb * bYoff], NULL, NULL);
    if (i) {
      /*printf("ERROR FROM zoomWithFilter: %d\n", i);*/
      free(linePtrs);
      return i;
    }
  }

  /* Fill lines as needed: whole lines or sides */
  for (iy = 0; iy < nyb; iy++) {
    if (iy < bYoff || iy >= bYsize + bYoff) {
      for (ix = 0; ix < nxb; ix++)
        bray[ix + iy * nxb] = dmean;
    } else {
      for (ix = 0; ix < bXoff; ix++) 
        bray[ix + iy * nxb] = dmean;
      for (ix = bXsize + bXoff; ix < nxb; ix++)
        bray[ix + iy * nxb] = dmean;
    }
  }
  free(linePtrs);
  return 0;
}

/*!
 * Fortran wrapper for @zoomFiltInterp
 */
int zoomfiltinterp(float *array, float *bray, int *nxa, int *nya, int *nxb,
                    int *nyb, float *xc, float *yc, float *xt, float *yt, float *dmean)
{
  return zoomFiltInterp(array, bray, *nxa, *nya, *nxb, *nyb, *xc, *yc, *xt, *yt, *dmean);
}

/*!
 * Returns the normalized value of the filter selected by @selectZoomFilter at the 
 * distance [radius] from the center of the filter.  Returns 0 if no filter was selected.
 */
double zoomFiltValue(float radius)
{
  double den = 0.;
  int i, lim;
  if (!sFilt_func)
    return 0.;
  lim = (sXwidth + 1) / 2;
  for (i = -lim; i <= lim; i++)
    den += sFilt_func(i / sXscale);
  return sFilt_func(radius / sXscale) / den;
}

/*!
 * Fortran wrapper for @zoomFiltValue
 */
double zoomfiltvalue(float *radius)
{
  return zoomFiltValue(*radius);
}

/*
 * Compute the limits of usable data in one dimension for calling from the interp function
 */
static void interpLimits(int na, int nb, float cen, float trans, double scale, 
                         float *aOff, int *bSize, int *bOff)
{
  float bcStart, bcEnd;
  int bdStart, bdEnd;

  /* Get starting and ending continuous coordinates in b from corners of a and limit 
     them to edges of first and last pixel */
  bcStart = -cen / scale + nb / 2. + trans;
  bcEnd = (na - cen) / scale + nb / 2. + trans;
  bcStart = B3DMAX(0., bcStart);
  bcEnd = B3DMIN(nb, bcEnd);

  /* Get discrete coordinates of the nearest whole pixel by rounding up on the start and
     down on the end, then get the offset and size from them */
  bdStart = (int)ceil(bcStart - 0.001);
  bdEnd = (int)floor(bcEnd + 0.001) - 1;
  *bOff = bdStart;
  *aOff = scale * (bdStart - nb / 2. - trans) + cen;

  /* Make sure this is not negative; roundoff errors can give negative values */
  *aOff = B3DMAX(0., *aOff);
  *bSize = bdEnd + 1 - bdStart;

  /* Make sure the output size will pass the test below too */
  if (*bSize * scale + *aOff > na)
    (*bSize)--;
}

/*
 * Accumulate lineb from the input image into accumBuf with the given weighting
 */
static void scanline_accum(unsigned char *lineb, int dtype, int aXsize, 
                           b3dInt32 *accumBuf, b3dInt16 sweight, float fweight)
{
  int i;
  b3dFloat *linef, *accumFbuf = (b3dFloat *)accumBuf;
  b3dInt16 *lines;
  b3dUInt16 *lineus;

  switch (dtype) {
  case SLICE_MODE_BYTE:
    for (i = 0; i < aXsize; i++)
      *accumBuf++ += sweight * *lineb++;
    break;
        
  case SLICE_MODE_RGB:
    for (i = 0; i < aXsize; i++) {
      *accumBuf++ += sweight * *lineb++;
      *accumBuf++ += sweight * *lineb++;
      *accumBuf++ += sweight * *lineb++;
    }
    break;
        
  case SLICE_MODE_FLOAT:
    linef = (b3dFloat *)lineb;
    for (i = 0; i < aXsize; i++)
      accumFbuf[i] += fweight * *linef++;
    break;
    
  case SLICE_MODE_SHORT:
    lines = (b3dInt16 *)lineb;
    for (i = 0; i < aXsize; i++)
      accumFbuf[i] += fweight * *lines++;
    break;

  case SLICE_MODE_USHORT:
    lineus = (b3dUInt16 *)lineb;
    for (i = 0; i < aXsize; i++)
      accumFbuf[i] += fweight * *lineus++;
    break;
  }
}

/*
 * scanline_filter - Applies the set of weight tables in wtab to the accumulated line
 * in lineb; applies the shift to scale the output values appropriately when using short 
 * weights
 */
static void scanline_filter(b3dInt32 *lineb, int dtype, int aXsize, 
                            unsigned char *obufb, int bXsize, Weighttab *wtab, int shift)
{
  int b, af, sum, sumr, sumb, sumg, t;
  b3dFloat *linef, *obuff, *wfp, *afp;
  b3dInt16  *obufs, *wp;
  b3dUInt16 *obufus;
  b3dInt32 *ap;
  float rsum;

  linef = (b3dFloat *)lineb;
  switch (dtype) {
  case SLICE_MODE_BYTE:
    for (b = 0; b < bXsize; b++, wtab++) {
      
      /* start sum at 1<<shift-1 for rounding */
      /* Shift the accumulated values by 8 bits to avoid overflow */
      for (sum = 1 << (shift - 1), wp = wtab->weight.s, ap = &lineb[wtab->i0],
             af = wtab->i1 - wtab->i0; af > 0; af--)
        sum += *wp++ * (short)(*ap++ >> CHANBITS);
      *obufb++ = PEG(sum >> shift, t);
    }
    break;
    
  case SLICE_MODE_RGB:
    for (b = 0; b < bXsize; b++, wtab++) {
      sumr = sumg = sumb = 1 << (shift - 1); 
      for (wp = wtab->weight.s, ap = &lineb[3 * wtab->i0],
             af = wtab->i1 - wtab->i0; af > 0; af--, wp++) {
        sumr += *wp * (short)(*ap++ >> CHANBITS);
        sumg += *wp * (short)(*ap++ >> CHANBITS);
        sumb += *wp * (short)(*ap++ >> CHANBITS);
      }
      *obufb++ = PEG(sumr >> shift, t);
      *obufb++ = PEG(sumg >> shift, t);
      *obufb++ = PEG(sumb >> shift, t);
    }
    break;
      
  case SLICE_MODE_FLOAT:
    obuff = (b3dFloat *)obufb;
    for (b = 0; b < bXsize; b++, wtab++) {
      for (rsum = 0., wfp = wtab->weight.f, afp = &linef[wtab->i0],
             af = wtab->i1 - wtab->i0; af > 0; af--)
        rsum += *wfp++ * (*afp++);
      *obuff++ = rsum;
    }
    break;

  case SLICE_MODE_SHORT:
    obufs = (b3dInt16 *)obufb;
    for (b = 0; b < bXsize; b++, wtab++) {
      for (rsum = 0.5, wfp = wtab->weight.f, afp = &linef[wtab->i0],
             af = wtab->i1 - wtab->i0; af > 0; af--)
        rsum += *wfp++ * (*afp++);
      *obufs++ = (b3dInt16)B3DMIN(32767., B3DMAX(-32767., rsum));
    }
    break;
    
  case SLICE_MODE_USHORT:
    obufus = (b3dUInt16 *)obufb;
    for (b = 0; b < bXsize; b++, wtab++) {
      for (rsum = 0.5, wfp = wtab->weight.f, afp = &linef[wtab->i0],
             af = wtab->i1 - wtab->i0; af > 0; af--)
        rsum += *wfp++ * (*afp++);
      *obufus++ = (b3dUInt16)B3DMIN(65535., B3DMAX(0., rsum));
    }
    break;
  }
}

/*
 * scanline_remap - fills a line in the output buffer by mapping from the filter buffer
 * to the color index values in cindex or bindex depending on the data type
 */
static void scanline_remap(unsigned char *filtBuf, int dtype, int bXsize, 
                           unsigned char *obufb, b3dUInt32 *cindex, unsigned char *bindex)
{
  int i;
  int *obufi = (int *)obufb;
  b3dInt16 *filts = (b3dInt16 *)filtBuf;
  b3dUInt16 *filtus = (b3dUInt16 *)filtBuf;
  
  switch (dtype) {
  case SLICE_MODE_BYTE:
    for (i = 0; i < bXsize; i++)
      *obufi++ = cindex[*filtBuf++];
    break;
  case SLICE_MODE_RGB:
    for (i = 0; i < bXsize; i++) {
      *obufb++ = bindex[*filtBuf++];
      *obufb++ = bindex[*filtBuf++];
      *obufb++ = bindex[*filtBuf++];
      *obufb++ = 0;
    }
    break;
  case SLICE_MODE_SHORT:
    for (i = 0; i < bXsize; i++)
      *obufi++ = cindex[*filts++];
    break;
  case SLICE_MODE_USHORT:
    for (i = 0; i < bXsize; i++)
      *obufi++ = cindex[*filtus++];
    break;
  }
}

/*
 * make_weighttab: sample the continuous filter, scaled by scale and
 * positioned at continuous source coordinate cen, for source coordinates in
 * the range [0..len-1], writing the weights into wtab.
 * For byte and RGB data types as given in dtype, scale the weights so they sum to 
 * WEIGHTONE, store as shorts, and trim leading and trailing zeros for.
 * b is the dest coordinate (for diagnostics).
 */
static void make_weighttab(int b, double cen, int len, double scale, double support, 
                           int dtype, Weighttab *wtab)
{
  int i0, i1, i, sum, t, stillzero, lastnonzero;
  short *wp;
  double den, sc, tr, rsum;
  int shortWgts = (dtype == SLICE_MODE_BYTE || dtype == SLICE_MODE_RGB) ? 1 : 0;

  /* find the source coord range of this positioned filter: [i0..i1-1] */
  i0 = (int)(cen - support + .5);
  i1 = (int)(cen + support + .5);
  if (i0 < 0)
    i0 = 0;
  if (i1 > len) 
    i1 = len;
  /*if (i0 >= i1) {
    fprintf(stderr, "make_weighttab: null filter at %d\n", b);
    exit(1);
    }*/

  /* the range of source samples to buffer: */
  wtab->i0 = i0;
  wtab->i1 = i1;

  /* find scale factor sc to normalize the filter */
  for (den = 0, i = i0; i < i1; i++)
    den += sFilt_func((i + .5 - cen) / scale);

  /* set sc so that sum of sc*func() is approximately WEIGHTONE */
  if (shortWgts)
    sc = den==0. ? WEIGHTONE : WEIGHTONE/den;
  else
    sc = den==0. ? 1. : 1./den;
  if (zoom_debug>1) fprintf(stderr,"    b=%d cen=%g scale=%g [%d..%d) sc=%g:  ",
                            b, cen, scale, i0, i1, sc);

  /* compute the discrete, sampled filter coefficients */
  stillzero = shortWgts;
  rsum = 0.;
  for (sum = 0, wp = wtab->weight.s, i=i0; i<i1; i++) {

    /* evaluate the filter function: */
    tr = sc * sFilt_func((i + .5 - cen) / scale);
    rsum += tr;

    /* if (tr<MINSHORT || tr>MAXSHORT) {
      fprintf(stderr, "tr=%g at %d\n", tr, b);
      exit(1);
      } */
    if (shortWgts) {
      t = (int) floor(tr+.5);
      if (stillzero && t==0) 
        i0++;   /* find first nonzero */
      else {
        stillzero = 0;
        *wp++ = t;          /* add weight to table */
        sum += t;
        if (t != 0)
          lastnonzero = i;  /* find last nonzero */
      }
    } else
      wtab->weight.f[i-i0] = tr;
  }

  if ((shortWgts && sum == 0) || rsum == 0.) {
    /* fprintf(stderr, "sum=0 at %d\n", b); */
    wtab->i0 = (wtab->i0+wtab->i1) >> 1;
    wtab->i1 = wtab->i0+1;
    if (shortWgts)
      wtab->weight.s[0] = WEIGHTONE;
    else
      wtab->weight.f[0] = 1.;
  } else if (shortWgts) {
    /* skip leading and trailing zeros */
    /* set wtab->i0 and ->i1 to the nonzero support of the filter */
    wtab->i0 = i0;
    wtab->i1 = i1 = lastnonzero+1;
    if (sum != WEIGHTONE) {
      /*
       * Fudge the center slightly to make sum=WEIGHTONE exactly.
       * Is this the best way to normalize a discretely sampled
       * continuous filter?
       */
      i = (int) (cen+.5);
      if (i < i0) 
        i = i0; 
      else if (i >= i1) 
        i = i1 - 1;
      t = WEIGHTONE - sum;
      if (zoom_debug>1) 
        fprintf(stderr,"[%d]+=%d ", i, t);
      wtab->weight.s[i-i0] += t;    /* fudge center sample */
    }
  }
  if (zoom_debug>1) {
    fprintf(stderr,"\t");
    if (shortWgts) 
      for (wp = wtab->weight.s, i=i0; i<i1; i++, wp++)
        fprintf(stderr,"%5d ", *wp);
    else
      for (i = i0; i < i1; i++)
        fprintf(stderr,"%.4f ", wtab->weight.f[i-i0]);
    fprintf(stderr,"\n");
  }
}

/*
 * The filters
 */
static double filt_binning(double x)
{
  return x >= -0.5 && x < 0.5 ? 1. : 0.;
}

static void mitchell_init(double b, double c)
{
  sMitchP0 = (  6. -  2.*b        ) / 6.;
  sMitchP2 = (-18. + 12.*b +  6.*c) / 6.;
  sMitchP3 = ( 12. -  9.*b -  6.*c) / 6.;
  sMitchQ0 = (       8.*b + 24.*c) / 6.;
  sMitchQ1 = (    - 12.*b - 48.*c) / 6.;
  sMitchQ2 = (       6.*b + 30.*c) / 6.;
  sMitchQ3 = (     -     b -  6.*c) / 6.;
}

static double filt_mitchell(double x)
{
  /*
   * see Mitchell&Netravali, "Reconstruction Filters in Computer Graphics",
   * SIGGRAPH 88
   */
  if (x < -2.) 
    return 0.;
  if (x < -1.)
    return sMitchQ0-x*(sMitchQ1-x*(sMitchQ2-x*sMitchQ3));
  if ( x < 0.) 
    return sMitchP0+x*x*(sMitchP2-x*sMitchP3);
  if (x < 1.) 
    return sMitchP0+x*x*(sMitchP2+x*sMitchP3);
  if (x < 2.) 
    return sMitchQ0+x*(sMitchQ1+x*(sMitchQ2+x*sMitchQ3));
  return 0.;
}

static double filt_blackman(double x)   /* Blackman window */
{
  return .42+.50*cos(PI*x)+.08*cos(2.*PI*x);
}

static double filt_triangle(double x)
{
  if (x <= -1. || x >= 1.)
    return 0.;
  return 1. - fabs(x);
}

static double filt_lanczos2(double x) 
{
  double a = 2.;
  if (x < -a || x > a)
    return 0.;
  if (x < 1.e-6 && x > -1.e-6)
    return 1.;
  return (a * sin(PI*x) * sin(PI*x / a)) / (PI * PI * x * x);
}

static double filt_lanczos3(double x) 
{
  double a = 3.;
  if (x < -a || x > a)
    return 0.;
  if (x < 1.e-6 && x > -1.e-6)
    return 1.;
  return (a * sin(PI*x) * sin(PI*x / a)) / (PI * PI * x * x);
}
