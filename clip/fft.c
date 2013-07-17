/*
 *  fft.c -- calculate fft for clip, also used by filter & correlation
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdlib.h>
#include <string.h>
#include "b3dutil.h"
#include "mrcc.h"
#include "clip.h"
#include "cfft.h"

static int clip_nicesize(int size);
static int mrcODFFT(float *buf, int nx, int ny, int idir);
static int clip_wrapvol(Istack *v, int direction);


/*
 * The entry point from clip for all fft operations, does 2D FFTs
 */
int clip_fft(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  Islice *slice;
  int k, z = 0;

  /* Common tests for parameters */
  if ((hin->mode == MRC_MODE_COMPLEX_FLOAT) ||
      (hin->mode == MRC_MODE_COMPLEX_SHORT)) {

    /* For inverse FFT, it must have full input size;
       default mode is float but others are OK; default X 
       size depends if it is old mirrored size (even) or non-mirrored (odd). */
    if (opt->ix != IP_DEFAULT || opt->iy != IP_DEFAULT || 
        opt->cx != IP_DEFAULT || opt->cy != IP_DEFAULT)
      show_warning("clip inverse fft - input sizes or centers are ignored");
    opt->ix = opt->iy = opt->cx = opt->cy = IP_DEFAULT; 
    if (opt->mode == IP_DEFAULT)
      opt->mode = MRC_MODE_FLOAT;
  } else {

    /* Forward fft must not resize */
    if (opt->ox != IP_DEFAULT || opt->oy != IP_DEFAULT || 
        opt->oz != IP_DEFAULT || opt->mode != IP_DEFAULT)
      show_warning("clip forward fft - output sizes or mode are ignored");
    opt->ox = opt->oy = opt->oz = IP_DEFAULT;
    opt->mode = MRC_MODE_COMPLEX_FLOAT;
    opt->ocanresize = FALSE;
  }

  if (opt->dim == 3)
    return(clip_3dfft(hin, hout, opt));

  /* For inverse FFT,  default X size assumes non-mirrored input; no longer support
     old mirrored size because non-mirrored can be even. */
  if ((hin->mode == MRC_MODE_COMPLEX_FLOAT) ||
      (hin->mode == MRC_MODE_COMPLEX_SHORT)) {
    if (opt->ox == IP_DEFAULT)
      opt->ox = 2 * (hin->nx - 1);
  }

  set_input_options(opt, hin);

  if ((hin->mode != MRC_MODE_COMPLEX_FLOAT) &&
      (hin->mode != MRC_MODE_COMPLEX_SHORT)) {
    if ((!clip_nicesize(opt->ix)) ||
        (!clip_nicesize(opt->iy)) || opt->ix % 2){
      printf("ERROR: clip - fft input size (%d, %d) is odd and/or has factors"
             " greater than 19.\n", opt->ix, opt->iy);
      return(-1);
    }
    opt->ox = opt->ix / 2 + 1;
    opt->oy = opt->iy;
    opt->oz = opt->nofsecs;
    if (opt->add2file && (hout->mode != opt->mode || hout->nx != opt->ox ||
                          hout->ny != opt->oy)) {
      show_error("clip forward 2d fft - cannot append to output file of "
                 "different size or mode");
      return -1;
    }
  }

  z = set_output_options(opt, hout);
  if (z < 0)
    return(z);

  mrc_head_label_cp(hin, hout);
  mrc_head_label(hout, "clip: 2d fft");
  show_status("Doing fast fourier transform...\n");     

  for (k = 0; k < opt->nofsecs; k++) {
    slice = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!slice){
      show_error("fft: Error reading slice.");
      return(-1);
    }
    slice_fft(slice);
    if (clipWriteSlice(slice, hout, opt, k, &z, 1))
      return -1;
  }
  mrc_coord_cp(hout, hin);
  if (hin->nx == hin->mx && hin->ny == hin->my && hin->nz == hin->mz)
    hout->mx = hout->nx;
  return mrc_head_write(hout->fp, hout);  
}

/*
 * Takes the FFT of one slice 
 */ 
int slice_fft(Islice *slice)
{
  float *tbuf;
  int nx2  = slice->xsize + 2;
  int ncs  = (int)slice->xsize * sizeof(float);
  int i, j, newXsize;
  int idir = 1;

  if (slice->mode == MRC_MODE_COMPLEX_FLOAT || 
      slice->mode == MRC_MODE_COMPLEX_SHORT)
    idir = -1;
  
  if (idir == 1){

    /* forward fft.  Convert to float if needed, then pack data into
       wider buffer for FFT and take fft */

    sliceFloat(slice); 
    tbuf = (float *)malloc(sizeof(float) * nx2 * slice->ysize);
    for(i = 0; i < slice->ysize; i++)
      memcpy(&(tbuf[i * nx2]), 
             &(slice->data.f[i * slice->xsize]), 
             ncs);
    free(slice->data.f);
    mrcToDFFT(tbuf, slice->xsize, slice->ysize, 0);

    /* Set slice to new properties and wrap the lines */
    sliceInit(slice, slice->xsize / 2 + 1, slice->ysize, 
              MRC_MODE_COMPLEX_FLOAT,
              tbuf);
    sliceWrapFFTLines(slice, 0);

    /* 7/16/13: deleted the old set of operations, painfully reproduced with mirrored 
       image load in 3dmod */

  } else {

    /* Inverse FFT: first convert to float */
    sliceComplexFloat(slice); 

    /* No longer reduce old-style mirrored */

    /* Unwrap the lines in any case then take the FFT */
    sliceWrapFFTLines(slice, 1);
    /*  fprintf(stderr, "unwrapped\n");
    for (j = 0; j < slice->ysize; j++) {
      for (i = 0; i < 2 * slice->xsize; i++)
        fprintf(stderr, "%8.2f ", slice->data.f[i + j * 2 * slice->xsize]);
      fprintf(stderr, "\n");
      } */
    newXsize = 2 * (slice->xsize - 1);
    mrcToDFFT(slice->data.f, newXsize, slice->ysize, 1);
    
    /* Repack data to eliminate extra elements */
    for (j = 0; j < slice->ysize; j++)
      for (i = 0; i < newXsize; i++)
        slice->data.f[i + j * newXsize] =
          slice->data.f[i + j * (newXsize + 2)];

    slice->xsize = newXsize;
    slice->mode = 2;
  }
  return(0);
}

/*
 * Entry point to process options and do I/O for 3D fft
 */
int clip_3dfft(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  Istack *v;
  int k;

  if (hin->mode != MRC_MODE_COMPLEX_FLOAT){
    if (opt->ix == IP_DEFAULT) opt->ix = hin->nx;
    if (opt->iy == IP_DEFAULT) opt->iy = hin->ny;
    if (opt->iz == IP_DEFAULT) opt->iz = hin->nz;
      
    if ((!clip_nicesize(opt->ix)) ||
        (!clip_nicesize(opt->iy)) ||
        (!clip_nicesize(opt->iz)) || opt->ix % 2){
      printf("ERROR: clip - fft input size %dx%dx%d is odd and/or has "
             "factors greater than 19.\n", opt->ix, opt->iy, opt->iz);
      return(-1);
    }
  }
      
  /* Removed non-functional file-based FFT 7/15/13 */
  /* if (opt->sano){
    return(clip_3dfft_swap(hin, hout, opt));
    } */
      
  v = grap_volume_read(hin, opt);
  if (!v){
    return(-1);
  }

  if (hin->mode != MRC_MODE_COMPLEX_FLOAT)
    for (k = 0; k < v->zsize; k++){
      if (hin->mode == MRC_MODE_COMPLEX_SHORT)
        sliceComplexFloat(v->vol[k]);
      else
        sliceFloat(v->vol[k]);
    }

  show_status("Doing 3d fast fourier transform in core...\n");
  clip_fftvol(v);

  mrc_head_new(hout, v->vol[0]->xsize, v->vol[0]->ysize, 
               v->zsize, v->vol[0]->mode);

  /* DNM 7/31/02: copy labels over, make label specify direction */
  mrc_head_label_cp(hin, hout);
  if (hin->mode != MRC_MODE_COMPLEX_FLOAT)
    mrc_head_label(hout, "Clip: Forward 3D FFT");
  else
    mrc_head_label(hout, "Clip: Inverse 3D FFT");

  if (grap_volume_write(v, hout, opt))
    return(-1);

  /* DNM 7/31/02: Copy cell and sample sizes over just like fftrans so that inverse
     transform will retain pixel spacing. 9/11/11: use function, adjust mx */
  mrc_coord_cp(hout, hin);
  if (hin->nx == hin->mx && hin->ny == hin->my && hin->nz == hin->mz)
    hout->mx = hout->nx;
  if (mrc_head_write(hout->fp, hout))
    return -1;

  grap_volume_free(v);
  return(0);
}

/* 
 * Do 3D FFT in memory on the given volume, must be float or complex float
 */
int clip_fftvol(Istack *v)
{
  Islice *slice;
  int z, i;
  int ncs    = (int)v->vol[0]->xsize * sizeof(float);
  int nx2    = v->vol[0]->xsize + 2;
  float *tbuf;

  if (v->vol[0]->mode == SLICE_MODE_COMPLEX_FLOAT){
    clip_wrapvol(v, 1);
    clip_fftvol3(v, -2);  

    for(z = 0; z < v->zsize; z++){
      /* must use -1 for idir or else phases get messed up */
      mrcToDFFT(v->vol[z]->data.f, (v->vol[z]->xsize - 1)*2,
                v->vol[z]->ysize, -1);
      v->vol[z]->mode = SLICE_MODE_FLOAT;
      v->vol[z]->xsize *= 2;
      sliceBoxIn(v->vol[z], 0, 0,
                 v->vol[z]->xsize - 2, v->vol[z]->ysize);

    }
  }else{
    /* forward fft */
    for(z = 0; z < v->zsize; z++){
      slice = v->vol[z];
      sliceFloat(slice);
      tbuf = (float *)malloc(sizeof(float) * nx2 * slice->ysize);
      for(i = 0; i < slice->ysize; i++)
        memcpy(&(tbuf[i * nx2]),
               &(slice->data.f[i * slice->xsize]),
               ncs);
      free(slice->data.f);
      slice->data.f = tbuf;
      slice->xsize += 2;
      mrcToDFFT(v->vol[z]->data.f, v->vol[z]->xsize - 2,
                v->vol[z]->ysize, 0);
      slice->xsize /= 2;
      slice->mode = SLICE_MODE_COMPLEX_FLOAT;

    }
    clip_fftvol3(v, -1);  
    clip_wrapvol(v, 0);

  }
  return(0);
}

/* 
 * do a complex<->complex fft in the 3rd dimension only 
 */
int clip_fftvol3(Istack *v, int idir)
{
  int x, y, z;
  Islice *slice;
  float *inp, *outp;
  int  vxsize = v->vol[0]->xsize;
  int  vysize = v->vol[0]->ysize;

  slice = sliceCreate(v->zsize, vxsize, SLICE_MODE_COMPLEX_FLOAT);
  for (y = 0; y < vysize; y++) {
    for (z = 0; z < slice->xsize; z++) {
      inp = v->vol[z]->data.f + 2 * y * vxsize;
      outp = slice->data.f + 2 * z;
      for (x = 0; x < slice->ysize; x++) {
        outp[2 * x * slice->xsize] = inp[2 * x];
        outp[2 * x * slice->xsize + 1] = inp[2 * x + 1];
        //sliceGetComplexFloatVal(v->vol[z], x, y, val);
        //slicePutComplexFloatVal(slice, z, x, val);
      }
    }

    mrcODFFT(slice->data.f, slice->xsize, slice->ysize, idir);

    /* put slice back into volume. */
    for (z = 0; z < slice->xsize; z++) {
      outp = v->vol[z]->data.f + 2 * y * vxsize;
      inp = slice->data.f + 2 * z;
      for (x = 0; x < slice->ysize; x++) {
        outp[2 * x] = inp[2 * x * slice->xsize];
        outp[2 * x + 1] = inp[2 * x * slice->xsize + 1];
        //sliceGetComplexFloatVal(slice, z, x, val);
        //slicePutComplexFloatVal(v->vol[z], x, y, val);
      }
    }
  }
  sliceFree(slice);
  return(0);
}

/*
 * Wrap the volume so FFT origin moves from origin to middle in Y/Z or back
 */
int clip_wrapvol(Istack *v, int direction)
{
  Islice *tsl;
  int k, kLow, kHigh, kInc, kOut;
  int nx = v->vol[0]->xsize;
  int ny = v->vol[0]->ysize;
  int hnz = v->zsize / 2;
  float *tmpLine = B3DMALLOC(float, 2 * nx);
  if (!tmpLine) {
    show_error("fft: Memory error getting array for temporary line of data\n");
    return 1;
  }

  for (k = 0; k < v->zsize; k++) {
    wrapFFTslice(v->vol[k]->data.f, tmpLine, nx, ny, direction);
  }
  kInc = indicesForFFTwrap(v->zsize, direction, &kOut, &kLow, &kHigh);
  if (v->zsize % 2) {
    tsl = v->vol[kOut];
    for (k = 0; k < hnz; k++, kOut += kInc, kHigh += kInc, kLow += kInc) {
      v->vol[kOut] = v->vol[kHigh];
      v->vol[kHigh] = v->vol[kLow];
    }
    v->vol[hnz] = tsl;
  } else {
    for (k = 0; k < hnz; k++, kHigh++, kLow++) {
      tsl = v->vol[kLow];
      v->vol[kLow] = v->vol[kHigh];
      v->vol[kHigh] = tsl;
    }
  }
  free(tmpLine);
  return(0);
}

/* Removed non-functional file-based FFT 7/15/13 */


/*******************************************************************/
/* c-stub for formerly fortran SUBROUTINE TODFFT(ARRAY,NX,NY,IDIR) */
/* (now a C-routine with fortran calling convention)               */
/* buf is (nx + 2) by ny floats                                    */
/* idir 0 = forward, 1 = inverse                                   */

/*
 * ODFFT do a series of 1D FFT's on 2D data.
 * idir -1 forward, -2 reverse.
 */

void mrcToDFFT(float buf[], int nx, int ny, int idir)
{
  todfft(buf, &nx, &ny, &idir); 
}


int mrcODFFT(float *buf, int nx, int ny, int idir)
{
  odfft(buf, &nx, &ny, &idir);
  return(0);
}

int clip_nicesize(int size)
{
  int i;
  if (usingFFTW())
    return 1;

  for(i = 2; i < 20; i++)
    while( !(size%i))
      size = size / i;

  if (size > 1)
    return(0);
  else
    return(1);
}

