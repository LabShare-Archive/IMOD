/*
 *  clip_proc.c -- Processing functions for clip.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$

Log at end of file
*/

#include <limits.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include "mrcfiles.h"
#include "mrcslice.h"
#include "mrcspectral.h"
#include "b3dutil.h"
#include "clip.h"

#ifndef FLT_MIN
#define FLT_MIN INT_MIN
#endif
#ifndef FLT_MAX
#define FLT_MAX INT_MAX
#endif

int grap_resize(struct MRCheader *hin, struct MRCheader *hout,
                struct Grap_options *opt)
{
  struct MRCvolume *v;

  v = grap_volume_read(hin, opt);
  if (!v){
    return(-1);
  }

  mrc_head_label_cp(hin, hout);
	  
  mrc_head_label(hout, "clip: resized image.");

  if (grap_volume_write(v, hout, opt)) 
    return(-1); 

  set_mrc_coords(opt);
  
  grap_volume_free(v);
  return(0);
}

/*
 * Common routine for the rescaling options
 */
int clip_scaling(struct MRCheader *hin, struct MRCheader *hout,
                    struct Grap_options *opt, int process)
{
  int k, z;
  Islice *slice;
  struct MRCvolume *v;
  double min, alpha;

  /* Set up new header for 2D */
  if (opt->dim == 2) {
    set_input_options(opt, hin);
    z = set_output_options(opt, hout);
    if (z < 0)
      return(z);
  }

  /* Copy header, give message, add title */
  mrc_head_label_cp(hin, hout);

  switch (process) {
  case IP_BRIGHTNESS:
    show_status("Brightness...\n");
    mrc_head_label(hout, "clip: brightness");
    min = hin->amin;
    break;
  case IP_SHADOW:
    show_status("Shadow...\n");
    mrc_head_label(hout, "clip: shadow");
    min = hin->amax;
    break;
  case IP_CONTRAST:
    show_status("Contrast...\n");
    mrc_head_label(hout, "clip: contrast");
    min = hin->amean;
    break;
  default:
    return(-1);
  }

  if (opt->val == IP_DEFAULT){
    opt->val = 1.0f;
  }
  alpha = opt->val;

  if (opt->dim == 3) {
  
    /* 3D processing based on global min/max/mean */
    v = grap_volume_read(hin, opt);
    if (!v){
      return(-1);
    }
    
    for (k = 0; k < v->zsize; k++){
      mrc_slice_lie(v->vol[k], min, alpha);
    }
    if (grap_volume_write(v, hout, opt)) 
      return(-1); 
    if (set_mrc_coords(opt))
      return (-1);
    grap_volume_free(v);
    return(0);
  }

  /* 2D processing, by slice, with potentially subarea read in */
  for (k = 0; k < opt->nofsecs; k++) {
    slice = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!slice){
      show_error("clip: Error reading slice.");
      return(-1);
    }

    /* Scale based on min.max.mean of individual slice */
    sliceMMM(slice);
     
    switch (process) {
    case IP_BRIGHTNESS:
      min = (double)slice->min;
      break;
    case IP_SHADOW:
      min = (double)slice->max;
      break;
    case IP_CONTRAST:
      min = (double)slice->mean;
      break;
    }
      
    mrc_slice_lie(slice, min, alpha);
    sliceMMM(slice);
    hout->amin = B3DMIN(hout->amin, slice->min);
    hout->amax = B3DMAX(hout->amax, slice->max);
    hout->amean += slice->mean;
    hout->mode  = slice->mode;
    hout->nx    = slice->xsize;
    hout->ny    = slice->ysize;
    if (mrc_write_slice((void *)slice->data.b, hout->fp, hout, k, 'z'))
      return -1;
    sliceFree(slice);
  }

  if (opt->nofsecs)
    hout->amean /= opt->nofsecs;
  return set_mrc_coords(opt);  
}

/*
 * Common routine for the edge filters that are not simple convolutions
 */
int clipEdge(struct MRCheader *hin, struct MRCheader *hout,
             struct Grap_options *opt, int process)
{
  struct MRCslice *s;
  int i, k, z;
  Islice *slice;
  char *message;
  double scale, fixval;

  if (opt->mode == IP_DEFAULT)
    opt->mode = (process == IP_GRADIENT) ? hin->mode : MRC_MODE_BYTE;

  if (hin->mode != MRC_MODE_BYTE && hin->mode != MRC_MODE_SHORT && 
      hin->mode != MRC_MODE_FLOAT) {
    show_error("clip edge: only byte, integer and float modes can be used");
    return -1;
  }

  set_input_options(opt, hin);

  z = set_output_options(opt, hout);
  if (z < 0)
    return(z);

  /* Copy header, give message, add title */
  mrc_head_label_cp(hin, hout);

  switch (process) {
  case IP_GRADIENT:
    message = "Taking gradient of";
    mrc_head_label(hout, "clip: gradient");
    break;
  case IP_PREWITT:
    message = "Applying Prewitt filter to";
    mrc_head_label(hout, "clip: Prewitt filter");
    break;
  case IP_GRAHAM:
    message = "Applying Graham filter to";
    mrc_head_label(hout, "clip: Graham filter");
    break;
  case IP_SOBEL:
    message = "Applying Sobel filter to";
    mrc_head_label(hout, "clip: Sobel filter");
    break;
  default:
    return(-1);
  }

  printf("clip: %s %ld slices...\n", message, opt->nofsecs);
  for (k = 0; k < opt->nofsecs; k++) {
    s = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!s){
      show_error("clip: Error reading slice.");
      return(-1);
    }

    if (process == IP_GRADIENT) {

      /* Do gradient */
      slice = sliceGradient(s);
      sliceFree(s);
    } else {

      /* Otherwise scale slice and convert slice if not bytes */
      slice = s;
      if (slice->mode != SLICE_MODE_BYTE) {
        sliceMinMax(slice);
        scale = 1.;
        if (slice->max > slice->min)
          scale = 255. / (slice->max - slice->min);
        if (scale >= 0.95 && scale <= 1.05)
          scale = 0.95;
        fixval = slice->min * scale / (scale - 1.);
        mrc_slice_lie(slice, fixval, scale);
        sliceNewMode(slice, SLICE_MODE_BYTE);
      }

      /* Do selected filter after getting min/max quickly */
      sliceMinMax(slice);
      if (process == IP_GRAHAM)
        sliceByteGraham(slice);
      else if (process == IP_SOBEL)
        sliceByteEdgeSobel(slice);
      else
        sliceByteEdgePrewitt(slice);
    }

    /* Scale slice to output mode if needed */
    if (slice->mode != opt->mode && sliceNewMode(slice, opt->mode) < 0)
      return -1;
    sliceMMM(slice);
    hout->amin = B3DMIN(hout->amin, slice->min);
    hout->amax = B3DMAX(hout->amax, slice->max);
    hout->amean += slice->mean;
    hout->nx    = slice->xsize;
    hout->ny    = slice->ysize;
    if (mrc_write_slice((void *)slice->data.b, hout->fp, hout, k, 'z'))
      return -1;
    sliceFree(slice);
  }

  if (opt->nofsecs)
    hout->amean /= opt->nofsecs;
  return set_mrc_coords(opt);  
}

/*
 * Common routine for kernel convolution filtering
 */
int clip_convolve(struct MRCheader *hin, 
                 struct MRCheader *hout,
                 struct Grap_options *opt, int process)
{
  struct MRCslice *s;
  float *blur;
  int i, k, z;
  float smoothKernel[] = {
    0.0625f, 0.125f, 0.0625f,
    0.125f,  0.25f,  0.125f, 
    0.0625f, 0.125f, 0.0625f};
  float sharpenKernel[] = {
    -1.,  -1., -1.,
    -1.,  9., -1.,
    -1.,  -1., -1.};
  float laplacianKernel[] = {
    1.,  1., 1.,
    1.,  -4., 1.,
    1.,  1., 1.};
  Islice *slice;
  char *message;

  if (opt->mode == IP_DEFAULT)
    opt->mode = (process == IP_SMOOTH) ? hin->mode : MRC_MODE_FLOAT;

  set_input_options(opt, hin);

  z = set_output_options(opt, hout);
  if (z < 0)
    return(z);

  /* Copy header, give message, add title */
  mrc_head_label_cp(hin, hout);

  switch (process) {
  case IP_SMOOTH:
    message = "Smoothing";
    mrc_head_label(hout, "clip: smooth");
    blur = smoothKernel;
    break;
  case IP_SHARPEN:
    message = "Sharpening";
    mrc_head_label(hout, "clip: sharpen");
    blur = sharpenKernel;
    break;
  case IP_LAPLACIAN:
    message = "Applying Laplacian to";
    mrc_head_label(hout, "clip: Laplacian");
    blur = laplacianKernel;
    break;
  default:
    return(-1);
  }

  printf("clip: %s %ld slices...\n", message, opt->nofsecs);
  for (k = 0; k < opt->nofsecs; k++) {
    s = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!s){
      show_error("clip: Error reading slice.");
      return(-1);
    }

    s->mean = hin->amean;
    slice = slice_mat_filter(s, blur, 3);
    mrc_slice_free(s);

    if (slice->mode != opt->mode && sliceNewMode(slice, opt->mode) < 0)
      return -1;
    sliceMMM(slice);
    hout->amin = B3DMIN(hout->amin, slice->min);
    hout->amax = B3DMAX(hout->amax, slice->max);
    hout->amean += slice->mean;
    hout->nx    = slice->xsize;
    hout->ny    = slice->ysize;
    if (mrc_write_slice((void *)slice->data.b, hout->fp, hout, k, 'z'))
      return -1;
    sliceFree(slice);
  }

  if (opt->nofsecs)
    hout->amean /= opt->nofsecs;
  return set_mrc_coords(opt);  
}

/*
 * 2D or 3D median filtering
 */
int clipMedian(struct MRCheader *hin, struct MRCheader *hout,
             struct Grap_options *opt)
{
  struct MRCslice *s;
  int i, j, k, z;
  Islice *slice;
  struct MRCvolume v;
  char title[40];
  int numInVol, firstInVol, lastInVol, firstNeed, lastNeed, depth, size;

  if (hin->mode != MRC_MODE_BYTE && hin->mode != MRC_MODE_SHORT && 
      hin->mode != MRC_MODE_FLOAT) {
    show_error("clip median: only byte, integer and float modes can be used");
    return -1;
  }

  set_input_options(opt, hin);
  z = set_output_options(opt, hout);
  if (z < 0)
    return(z);

  if (opt->val == IP_DEFAULT)
    opt->val = 3;
  if (opt->mode != MRC_MODE_BYTE && opt->mode != MRC_MODE_SHORT && 
      opt->mode != MRC_MODE_FLOAT)
    opt->mode = hin->mode;
  size = (int)B3DMAX(2, opt->val);

  mrc_head_label_cp(hin, hout);
  sprintf(title, "clip: %dD median filter, size %d", opt->dim, size);
  mrc_head_label(hout, title);
  printf("clip: median filtering %ld slices...\n", opt->nofsecs);

  /* Get slice for output */
  slice = sliceCreate(opt->ix, opt->iy, opt->mode);
  if (!slice)
    return (-1);

  depth = opt->dim == 2 ? 1 : size;
  v.vol = (struct MRCslice **)malloc(depth * sizeof(struct MRCslice *));
  v.zsize = 0;

  for (k = 0; k < opt->nofsecs; k++) {

    /* Set up slice numbers needed in volume */
    if (opt->dim == 2) {
      firstNeed = lastNeed = opt->secs[k];
    } else {
      firstNeed = B3DMAX(0, opt->secs[k] - size / 2);
      lastNeed = B3DMIN(hin->nz - 1, opt->secs[k] + (size - 1) / 2);
    }
    /* printf("k = %d, need %d to %d\n", k, firstNeed, lastNeed); */

      /* Free or copy down existing slices in volume */
    for (i = 0, j = 0; i < v.zsize; i++) {
      if (firstInVol + i < firstNeed || firstInVol + i > lastNeed) {
        /* printf("freeing %d\n", firstInVol + i); */
        sliceFree(v.vol[i]);
      } else {
        /* printf("copying %d: %d to %d\n", firstInVol + i, i, j); */
        v.vol[j++] = v.vol[i];
      }
    }
    v.zsize = j;
    if (j)
      firstInVol = lastInVol + 1 - j;

    /* Load needed slices */
    for (i = firstNeed; i <= lastNeed; i++) {
      if (!v.zsize || i > lastInVol) {
        /* printf("loading %d into %d mode %d\n", i,  v.zsize, hin->mode); */
        s = sliceReadSubm(hin, i, 'z', opt->ix, opt->iy,
                                         (int)opt->cx, (int)opt->cy);
        if (!s)
          return -1;
        if (!v.zsize)
          firstInVol = i;
        v.vol[v.zsize++] = s;
        lastInVol = i;
      }
    }

    /* Filter and maintain mmm and write */
    if (sliceMedianFilter(slice, &v, size))
      return -1;
    sliceMMM(slice);
    hout->amin = B3DMIN(hout->amin, slice->min);
    hout->amax = B3DMAX(hout->amax, slice->max);
    hout->amean += slice->mean;
    hout->nx    = slice->xsize;
    hout->ny    = slice->ysize;
    if (mrc_write_slice((void *)slice->data.b, hout->fp, hout, k, 'z'))
      return -1;
  }
  
  /* clean up */
  sliceFree(slice);
  for (i = 0; i < v.zsize; i++)
    sliceFree(v.vol[i]);

  if (opt->nofsecs)
    hout->amean /= opt->nofsecs;
  return set_mrc_coords(opt);  
}


int grap_flip(struct MRCheader *hin, struct MRCheader *hout,
              struct Grap_options *opt)
{
  int i,j,k;
  Islice *sl, *tsl;
  Ival val;

  hout->mode = hin->mode;

  if ((!strncmp(opt->command, "flipxy",6)) || 
      (!strncmp(opt->command, "flipyx",6))){
    hout->nz = hin->nz;
    hout->nx = hin->ny;
    hout->ny = hin->nx;
    hout->mz = hin->mz;
    hout->mx = hin->my;
    hout->my = hin->mx;
    hout->zlen = hin->zlen;
    hout->xlen = hin->ylen;
    hout->ylen = hin->xlen;
    if (mrc_head_write(hout->fp, hout))
      return -1;
    sl = sliceCreate(hout->nx, hout->nz, hout->mode);
    for(i = 0, j = 0; i < hin->nx; i++, j++){
      if (mrc_read_slice(sl->data.b, hin->fp, hin, i, 'x'))
        return -1;
      if (opt->sano)
        sliceMirror(sl, 'y');
      if (mrc_write_slice(sl->data.b, hout->fp, hout, j, 'y'))
        return -1;
    }
    sliceFree(sl);
    puts(" Done!");
    return(0);
  }

  if ((!strncmp(opt->command, "flipzx",6)) ||
      (!strncmp(opt->command, "flipxz",6))){
    hout->nx = hin->nz;
    hout->mx = hin->mz;
    hout->xlen = hin->zlen;
    hout->ny = hin->ny;
    hout->my = hin->my;
    hout->ylen = hin->ylen;
    hout->nz = hin->nx;
    hout->mz = hin->mx;
    hout->zlen = hin->xlen;
    if (mrc_head_write(hout->fp, hout))
      return -1;
    sl = sliceCreate(hin->ny, hin->nz, hout->mode);

    /* DNM 3/23/01: need to transpose into a new slice because it is
       read in as a y by z slice, not a z by y slice */
    tsl = sliceCreate(hout->nx, hout->ny, hout->mode);
    for(k = 0; k < hin->nx; k++){
      if (mrc_read_slice(sl->data.b, hin->fp, hin, k, 'x'))
        return -1;
      for (i = 0; i < hin->ny; i++)
        for (j = 0; j < hin->nz; j++) {
          sliceGetVal(sl, i, j, val);
          slicePutVal(tsl, j, i, val);
        }
      if (mrc_write_slice(tsl->data.b, hout->fp, hout, k, 'z'))
        return -1;
    }
    sliceFree(sl);
    sliceFree(tsl);
    puts(" Done!");
    return(0);
  }
  if ((!strncmp(opt->command, "flipyz",6)) ||
      (!strncmp(opt->command, "flipzy",6))){
    hout->nx = hin->nx;
    hout->mx = hin->mx;
    hout->xlen = hin->xlen;
    hout->ny = hin->nz;
    hout->my = hin->mz;
    hout->ylen = hin->zlen;
    hout->nz = hin->ny;
    hout->mz = hin->my;
    hout->zlen = hin->ylen;
    if (mrc_head_write(hout->fp, hout))
      return -1;
    sl = sliceCreate(hout->nx, hout->ny, hout->mode);
    for(k = 0, j = 0; k < hin->ny; k++,j++){
      if (mrc_read_slice(sl->data.b, hin->fp, hin, j, 'y'))
        return -1;
      if (mrc_write_slice(sl->data.b, hout->fp, hout, k, 'z'))
        return -1;
    }
    sliceFree(sl);
    puts(" Done!");
    return(0);
  }
  if ((!strncmp(opt->command, "flipx",5)) ||
      (!strncmp(opt->command, "flipy",5))){
    hout->nx = hin->nx;
    hout->ny = hin->ny;
    hout->nz = hin->nz;
    /* DNM 3/21/01: added head_write here and below */
    if (mrc_head_write(hout->fp, hout))
      return -1;
    sl = sliceCreate(hout->nx, hout->ny, hout->mode);
    for (k = 0; k < hin->nz; k++){
      if (mrc_read_slice(sl->data.b, hin->fp, hin, k, 'z'))
        return -1;
      sliceMirror(sl, opt->command[4]);
      if (mrc_write_slice(sl->data.b, hout->fp, hout, k, 'z'))
        return -1;
    }
    sliceFree(sl);
    puts(" Done!");
    return(0);
  }
  if (!strncmp(opt->command, "flipz",5)){
    hout->nx = hin->nx;
    hout->ny = hin->ny;
    hout->nz = hin->nz;
    if (mrc_head_write(hout->fp, hout))
      return -1;
    sl = sliceCreate(hout->nx, hout->ny, hout->mode);
    tsl = sliceCreate(hout->nx, hout->ny, hout->mode);
    for (k = 0; k < hin->nz/2; k++){
      if (mrc_read_slice(sl->data.b, hin->fp, hin, k, 'z') ||
          mrc_read_slice(tsl->data.b, hin->fp, hin, hout->nz-k-1, 'z') ||
          mrc_write_slice(tsl->data.b, hout->fp, hout, k, 'z') ||
          mrc_write_slice(sl->data.b, hout->fp, hout, hout->nz-k-1, 'z'))
        return -1;
    }
    sliceFree(sl);
    sliceFree(tsl);
    puts(" Done!");
    return(0);
  }
	  
  fprintf(stderr, "clip flip: Warning no flipping was done.\n");
  return(-1);
}

/* 3-D color */
int grap_color(struct MRCheader *hin, struct MRCheader *hout, 
               struct Grap_options *opt)
{
  int xysize, k, i;
  unsigned char **idata;
  unsigned char bdata;
  float pixel, pixin;

  if (opt->red == IP_DEFAULT)
    opt->red = 1.0f;
  if (opt->green == IP_DEFAULT)
    opt->green = 1.0f;
  if (opt->blue  == IP_DEFAULT)
    opt->blue = 1.0f;

  if (opt->dim == 2)
    return(clip2d_color(hin,hout,opt));

  if ((opt->ix !=  IP_DEFAULT) || (opt->iy !=  IP_DEFAULT) || 
      (opt->iz !=  IP_DEFAULT) || (opt->ox !=  IP_DEFAULT) ||
      (opt->oy !=  IP_DEFAULT) || (opt->oz !=  IP_DEFAULT))
    fprintf(stderr, "clip 3d color: "
            "Warning input and output sizes ignored.\n");

  printf("clip: color (red, green, blue) = ( %g, %g, %g).\n",
         opt->red, opt->green, opt->blue);
     
  idata  = mrc_read_byte(hin->fp, hin, NULL, NULL);
  xysize = hin->nx * hin->ny;
  hout->nx = hin->nx;
  hout->ny = hin->ny;
  hout->nz = hin->nz;
  hout->mode =  MRC_MODE_RGB;
  if (mrc_head_write(hout->fp, hout))
    return -1;

  for (k = 0; k < hout->nz; k++){
    for (i = 0; i < xysize; i++){

      pixin = idata[k][i];
      pixel = pixin * opt->red;
      if (pixel > 255.0) pixel = 255.0;
      bdata = pixel + 0.5;
      b3dFwrite(&bdata,  sizeof(unsigned char), 1, hout->fp);

      pixel = pixin * opt->green;
      if (pixel > 255.0) pixel = 255.0;
      bdata = pixel + 0.5;
      b3dFwrite(&bdata,  sizeof(unsigned char), 1, hout->fp);
	       
      pixel = pixin * opt->blue;
      if (pixel > 255.0) pixel = 255.0;
      bdata = pixel + 0.5;
      b3dFwrite(&bdata,  sizeof(unsigned char), 1, hout->fp);

    }
  }
  return(0);
}


int clip2d_color(struct MRCheader *hin, struct MRCheader *hout,
                 struct Grap_options *opt)
{
  Islice *slice;
  int k, z, i;
  unsigned char bdata;
  float pixel, pixin;
  int xysize;

  if ((opt->mode != MRC_MODE_RGB) && (opt->mode != IP_DEFAULT)){
    fprintf(stderr, "clip warning: color output mode must be rgb.\n");
  }
  opt->mode = MRC_MODE_RGB;
  set_input_options(opt, hin);
  z = set_output_options(opt, hout);
  if (z < 0)
    return(z);

  show_status("False Color...\n");
  hout->mode = MRC_MODE_RGB;
  mrc_head_label_cp(hin, hout);
  mrc_head_label(hout, "CLIP Color");

  for(k = 0; k < opt->nofsecs; k++, z++){
    slice = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!slice){
      show_error("clip: Error reading slice.");
      return(-1);
    }
    sliceMMM(slice);
    if (slice->min < hout->amin)
      hout->amin = slice->min;
    if (slice->max > hout->amax)
      hout->amax = slice->max;
    hout->amean += slice->mean;
    hout->nx    = slice->xsize;
    hout->ny    = slice->ysize;
    xysize = slice->xsize * slice->ysize;
    for(i = 0 ; i < xysize; i++){
      pixin = slice->data.b[i];
      pixel = pixin * opt->red;
      if (pixel > 255.0) pixel = 255.0;
      bdata = pixel + 0.5;
      b3dFwrite(&bdata,  sizeof(unsigned char), 1, hout->fp);

      pixel = pixin * opt->green;
      if (pixel > 255.0) pixel = 255.0;
      bdata = pixel + 0.5;
      b3dFwrite(&bdata,  sizeof(unsigned char), 1, hout->fp);
	       
      pixel = pixin * opt->blue;
      if (pixel > 255.0) pixel = 255.0;
      bdata = pixel + 0.5;
      b3dFwrite(&bdata,  sizeof(unsigned char), 1, hout->fp);
    }
    sliceFree(slice);
  }
  if (opt->nofsecs)
    hout->amean /= opt->nofsecs;

  /* DNM 1/17/03: remove unneeded rewind */
  return mrc_head_write(hout->fp, hout);
}


int grap_average(struct MRCheader *h1, struct MRCheader *h2, 
                 struct MRCheader *hout, struct Grap_options *opt)
{
  struct MRCslice *s, *so;
  struct MRCheader **hdr;
  int i, j, k, f;
  double valscale;
  Ival val, oval;

  if (opt->dim == 2)
    return(clip2d_average(h1,hout,opt));


  if (opt->infiles < 2){
    fprintf(stderr, "grap average: Need two input files.\n");
    return(-1);
  }

  if (   (h1->nx != h2->nx)
         || (h1->ny != h2->ny)
         || (h1->nz != h2->nz)){
    fprintf(stderr, "grap average: All x,y,z sizes must equal\n");
    return(-1);
  }

  mrc_head_new(hout, h1->nx, h1->ny, h1->nz, 2);
  mrc_head_label(hout, "3D Averaged");
  if (mrc_head_write(hout->fp, hout))
    return -1;

  s = mrc_slice_create(h1->nx, h1->ny, h1->mode);
  if (h1->mode == MRC_MODE_COMPLEX_FLOAT){
    so = mrc_slice_create(h1->nx, h1->ny, 4);
    hout->mode = MRC_MODE_COMPLEX_FLOAT;
  }else if (h1->mode == MRC_MODE_RGB){
    so = mrc_slice_create(h1->nx, h1->ny, MRC_MODE_RGB);
    hout->mode = MRC_MODE_RGB;
  }else{
    so = mrc_slice_create(h1->nx, h1->ny, 2);
  }
  hdr = (struct MRCheader **)
    malloc(opt->infiles * sizeof(struct MRCheader *));
  if (!hdr)
    return(-1);
     
  for (f = 0; f < opt->infiles; f++){
    hdr[f] = (struct MRCheader *)malloc(sizeof(struct MRCheader));
    if (!hdr[f])
      return(-1);
    hdr[f]->fp = fopen(opt->fnames[f], "rb");
    if (!hdr[f]->fp){
      fprintf(stderr, "grap average: error opening %s.\n", 
              opt->fnames[f]);
      return(-1);
    }
    if (mrc_head_read(hdr[f]->fp, hdr[f])){
      fprintf(stderr, "clip average: error reading %s.\n",
              opt->fnames[f]);
      return(-1);
    }
    if ( (h1->nx != hdr[f]->nx) || (h1->ny != hdr[f]->ny) ||
         (h1->nz != hdr[f]->nz) ){
      fprintf(stderr, "clip average: all files must be same size.");
      return(-1);
    }
  }

  for(k = 0; k < h1->nz; k++){
    printf("\r3D - Averaging section %d of %d", k + 1, h1->nz);
    fflush(stdout);
	  
    if (mrc_read_slice((void *)s->data.b, hdr[0]->fp, hdr[0], k, 'z'))
      return -1;
    for (j = 0; j < h1->ny; j++)
      for(i = 0; i < h1->nx; i ++){
        sliceGetVal(s, i, j, val);
        slicePutVal(so, i, j, val);
      }

    for (f = 1; f < opt->infiles; f++){
      if (mrc_read_slice((void *)s->data.b, hdr[f]->fp, hdr[f], k, 'z'))
        return -1;
      for (j = 0; j < h1->ny; j++)
        for(i = 0; i < h1->nx; i ++){
          sliceGetVal(s, i, j,   val);
          sliceGetVal(so, i, j, oval);
          oval[0] += val[0];
          oval[1] += val[1];
          oval[2] += val[2];
			 
          slicePutVal(so, i, j, oval);
        }
    }

    if (f){
      valscale = 1.0 / (double)f;
      mrc_slice_valscale(so, valscale);
    }

    if (mrc_write_slice((void *)so->data.b, hout->fp, hout, k, 'z'))
      return -1;
    mrc_slice_calcmmm(so);
    if (!k){
      hout->amin = so->min;
      hout->amax = so->max;
      hout->amean = so->mean;
      if (mrc_head_write(hout->fp, hout))
        return -1;
    }else{
      if (so->min < hout->amin)
        hout->amin = so->min;
      if (so->max > hout->amax)
        hout->amax = so->max;
      hout->amean += so->mean;
    }
  }
  puts("");
  hout->amean /= k;
  if (mrc_head_write(hout->fp, hout))
    return -1;
  mrc_slice_free(s);
  mrc_slice_free(so);
  return(0);
}

/*
 * Join 3 separate byte files into an RGB file
 */
int clip_joinrgb(struct MRCheader *h1, struct MRCheader *h2,
		 struct MRCheader *hout, struct Grap_options *opt)
{
  int i,j,k,l;
  float val[5];
  struct MRCheader hdr[3];
  struct MRCslice *s, *srgb[3];

  if (opt->red == IP_DEFAULT)
    opt->red = 1.0;
  if (opt->green == IP_DEFAULT)
    opt->green = 1.0;
  if (opt->blue == IP_DEFAULT)
    opt->blue = 1.0;

  if (opt->infiles != 3) {
    fprintf(stderr, "ERROR: clip joinrgb - three input files must be "
            "specified\n");
    return (-1);
  }
  hdr[0] = *h1;
  hdr[1] = *h2;
  hdr[2].fp = fopen(opt->fnames[2], "rb");

  if (!hdr[2].fp){
      fprintf(stderr, "ERROR: clip joinrgb - opening %s.\n", opt->fnames[2]);
      return(-1);
  }
  if (mrc_head_read(hdr[2].fp, &hdr[2])){
    fprintf(stderr, "ERROR: clip joinrgb - reading %s.\n", opt->fnames[2]);
    return(-1);
  }
  
  if ( (h1->nx != hdr[2].nx) || (h1->ny != hdr[2].ny) ||
       (h1->nz != hdr[2].nz) || (h1->nx != h2->nx) || (h1->ny != h2->ny) ||
       (h1->nz != h2->nz)) {
    fprintf(stderr, "ERROR: clip joinrgb - all files must be same size.\n");
    return(-1);
  }

  if (h1->mode != MRC_MODE_BYTE || h2->mode != MRC_MODE_BYTE || 
      hdr[2].mode != MRC_MODE_BYTE) {
    fprintf(stderr, "ERROR: clip joinrgb - all files must be bytes.\n");
    return(-1);
  }

  /* Set up mode and label */
  hout->mode = MRC_MODE_RGB;
  mrc_head_label(hout, "CLIP Join 3 files into RGB");
  if (mrc_head_write(hout->fp, hout))
    return -1;

  s = mrc_slice_create(h1->nx, h1->ny, MRC_MODE_RGB);
  for (i = 0; i < 3; i++) {
    srgb[i] = mrc_slice_create(h1->nx, h1->ny, MRC_MODE_BYTE);
    if (!s || !srgb[i]) {
      fprintf(stderr, "ERROR: CLIP - getting memory for slices\n");
      return (-1);
    }
  }

  /* Loop on slices */
  for (k = 0; k < h1->nz; k++) {
    printf("\rJoining section %d of %d", k + 1, h1->nz);
    fflush(stdout);

    /* Read three slices */
    for (l = 0; l < 3; l++)
      if (mrc_read_slice((void *)srgb[l]->data.b, hdr[l].fp, &hdr[l], k, 'z'))
        return -1;

    /* Get the components, scale, and put into output slice */
    for (j = 0; j < h1->ny; j++) {
      for(i = 0; i < h1->nx; i ++) {
        for (l = 0; l < 3; l++)
          sliceGetVal(srgb[l], i, j, &val[l]);
        val[0] *= opt->red;
        val[1] *= opt->green;
        val[2] *= opt->blue;
        slicePutVal(s, i, j, val);
      }
    }
  
    if (mrc_write_slice((void *)s->data.b, hout->fp, hout, k, 'z'))
      return -1;
  }
  puts("");

  for (i = 0; i < 3; i++)
    mrc_slice_free(srgb[i]);
  mrc_slice_free(s);
  return(0);
}

/*
 * Split an RGB file into 3 separate files
 */
int clip_splitrgb(struct MRCheader *h1, struct Grap_options *opt)
{
  int i,j,k,l;
  float val[5];
  char *ext[3] = {".r", ".g", ".b"};
  struct MRCheader hdr[3];
  char *fname;
  int len = strlen(opt->fnames[1]);
  struct MRCslice *s, *srgb[3];

  if (h1->mode != MRC_MODE_RGB) {
    fprintf(stderr, "ERROR: clip splitrgb - mode is not RGB\n");
    return(-1);
  }
   
  if (opt->infiles != 1) {
    fprintf(stderr, "ERROR: clip splitrgb - only one input file should "
            "be specified\n");
    return (-1);
  }
 
  fname = (char *)malloc(len + 4);
  if (!fname) {
    fprintf(stderr, "ERROR: CLIP - getting memory for filename\n");
    return (-1);
  }

  /* Set up output files and get slices */
  s = mrc_slice_create(h1->nx, h1->ny, MRC_MODE_RGB);
  for (i = 0; i < 3; i++) {
    sprintf(fname, "%s%s", opt->fnames[1], ext[i]);
    imodBackupFile(fname);
    hdr[i] = *h1;
    hdr[i].fp = fopen(fname, "wb+");
    if (!hdr[i].fp){
      fprintf(stderr, "Error opening %s\n", fname);
      return(-1);
    }
    hdr[i].swapped = 0;
    hdr[i].mode = 0;
    hdr[i].amin = 255;
    hdr[i].amean = 0;
    hdr[i].amax = 0;
    hdr[i].headerSize = 1024;
    hdr[i].next = 0;
    mrc_head_label(&hdr[i], "CLIP Split RGB into 3 files");
    if (mrc_head_write(hdr[i].fp, &hdr[i]))
      return -1;
    srgb[i] = mrc_slice_create(h1->nx, h1->ny, MRC_MODE_BYTE);
    if (!s || !srgb[i]) {
      fprintf(stderr, "ERROR: CLIP - getting memory for slices\n");
      return (-1);
    }
  }

  /* Loop on sections */
  for(k = 0; k < h1->nz; k++) {
    printf("\rSplitting section %d of %d", k + 1, h1->nz);
    fflush(stdout);
	  
    /* Read slice and put its 3 components into output slices */
    if (mrc_read_slice((void *)s->data.b, h1->fp, h1, k, 'z'))
      return -1;
    for (j = 0; j < h1->ny; j++) {
      for(i = 0; i < h1->nx; i ++) {
        sliceGetVal(s, i, j, val);
        for (l = 0; l < 3; l++)
          slicePutVal(srgb[l], i, j, &val[l]);
      }
    }

    /* Maintain min/max/mean */
    for (i = 0; i < 3; i++) {
      if (mrc_write_slice((void *)srgb[i]->data.b, hdr[i].fp, &hdr[i], k, 'z'))
        return -1;
      mrc_slice_calcmmm(srgb[i]);
      if (srgb[i]->min < hdr[i].amin)
        hdr[i].amin = srgb[i]->min;
      if (srgb[i]->max > hdr[i].amax)
        hdr[i].amax = srgb[i]->max;
      hdr[i].amean += srgb[i]->mean;
    }
  }

  puts("");

  for (i = 0; i < 3; i++) {
    hdr[i].amean /= k;
    if (mrc_head_write(hdr[i].fp, &hdr[i]))
      return -1;
    mrc_slice_free(srgb[i]);
  }
  mrc_slice_free(s);
  return(0);
}

int clip2d_average(struct MRCheader *hin, struct MRCheader *hout, 
                   struct Grap_options *opt)
{
  Islice *slice;
  Islice *avgs, *cnts;
  Ival val, aval;
  int k, z, i, j;
  int thresh = FALSE;
  float dval,scale;
     
  mrc_head_label_cp(hin, hout);
  mrc_head_label(hout, "CLIP: 2D Average");
  set_input_options(opt, hin);
  z = set_output_options(opt, hout);
  if (z < 0)
    return(z);

  if (opt->val != IP_DEFAULT)
    thresh = TRUE;

  show_status("2D Average...\n");

  avgs = sliceCreate(opt->ix, opt->iy, SLICE_MODE_MAX);
  cnts = sliceCreate(opt->ix, opt->iy, SLICE_MODE_FLOAT);

  aval[0] = aval[1] = aval[2] = 0.0f;
  for(j = 0; j < avgs->ysize; j++)
    for(i = 0; i < avgs->xsize; i++){
      slicePutVal(avgs, i, j, aval);
      cnts->data.f[i + (j * cnts->xsize)] = 0.0f;
    }

  for(k = 0; k < opt->nofsecs; k++){
    slice = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!slice){
      show_error("clip: Error reading slice.");
      return(-1);
    }
    for(j = 0; j < slice->ysize; j++)
      for(i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j,  val);
        sliceGetVal(avgs,  i, j, aval);
        aval[0] += val[0];
        aval[1] += val[1];
        aval[2] += val[2];
		    
        if (thresh){
          dval = mrc_slice_getmagnitude(slice, i, j);
          if (dval > opt->val){
            cnts->data.f[i + (j * cnts->xsize)] += 1.0f;
          }
        }else{
          cnts->data.f[i + (j * cnts->xsize)] += 1.0f;
        }
        slicePutVal(avgs,  i, j, aval);
      }
    sliceFree(slice);
  }

  scale = 1.0;
  if (opt->mode == MRC_MODE_RGB){
    sliceMMM(avgs);
    if (avgs->max > 0)
      scale = opt->nofsecs * 255.0f/avgs->max;
  }

  slice = sliceCreate(opt->ix, opt->iy, opt->mode);
  for(j = 0; j < slice->ysize; j++)
    for(i = 0; i < slice->xsize; i++){
      sliceGetVal(avgs, i, j, aval);
      dval = cnts->data.f[i + (j * cnts->xsize)];
      if (dval){
        dval = scale/dval;
        aval[0] *= dval;
        aval[1] *= dval;
        aval[2] *= dval;
      }
      slicePutVal(slice, i, j, aval);
    }

  sliceMMM(slice);
  hout->amin = slice->min;
  hout->amean = slice->mean;
  hout->amax = slice->max;
  hout->mode = slice->mode;
  if (mrc_write_slice((void *)slice->data.b, hout->fp, hout, z, 'z'))
    return -1;
  hout->nz -= (opt->nofsecs - 1);
  if (mrc_head_write(hout->fp, hout))
    return -1;
  sliceFree(avgs);
  sliceFree(cnts);
  return(0);
}


int clip_parxyz(struct MRCvolume *v, 
                int xmax, int ymax, int zmax,
                float *rx, float *ry, float *rz)
{ 
  float a,b;
  float x,y,z;
  int x1,x2,x3;
  float y1,y2,y3;

  x1 = xmax - 1; x2 = xmax; x3 = xmax + 1;
  if (x1 < 0)
    x1 = v->vol[0]->xsize - 1;
  if (x3 >= v->vol[0]->xsize) 
    x3 = 0;
  y1 = mrc_slice_getmagnitude(v->vol[zmax], x1, ymax);
  y2 = mrc_slice_getmagnitude(v->vol[zmax], x2, ymax);
  y3 = mrc_slice_getmagnitude(v->vol[zmax], x3, ymax);
  x1 = xmax - 1; x3 = xmax + 1;
  a = (y1*(x2-x3)) + (y2*(x3-x1)) + (y3*(x1-x2));
  b = (x1*x1*(y2-y3)) + (x2*x2*(y3-y1)) + (x3*x3*(y1-y2));
  if (a)
    x = -b/(2*a);
  else
    x = xmax;

  x1 = ymax - 1; x2 = ymax; x3 = ymax + 1;
  if (x1 < 0)
    x1 = v->vol[0]->ysize - 1;
  if (x3 >= v->vol[0]->ysize)
    x3 = 0;
  y1 = mrc_slice_getmagnitude(v->vol[zmax], xmax, x1);
  y2 = mrc_slice_getmagnitude(v->vol[zmax], xmax, x2);
  y3 = mrc_slice_getmagnitude(v->vol[zmax], xmax, x3);
  x1 = ymax - 1; x3 = ymax + 1;
  a = (y1*(x2-x3)) + (y2*(x3-x1)) + (y3*(x1-x2));
  b = (x1*x1*(y2-y3)) + (x2*x2*(y3-y1)) + (x3*x3*(y1-y2));
  if (a)
    y = -b/(2*a);
  else
    y = ymax;

  x1 = zmax - 1; x2 = zmax; x3 = zmax + 1;
  if (x1 < 0)
    x1 = v->zsize - 1;
  if (x3 >= v->zsize)
    x3 = 0;
  y1 = mrc_slice_getmagnitude(v->vol[x1], xmax, ymax);
  y2 = mrc_slice_getmagnitude(v->vol[x2], xmax, ymax);
  y3 = mrc_slice_getmagnitude(v->vol[x3], xmax, ymax);
  x1 = zmax - 1; x3 = zmax + 1;
  a = (y1*(x2-x3)) + (y2*(x3-x1)) + (y3*(x1-x2));
  b = (x1*x1*(y2-y3)) + (x2*x2*(y3-y1)) + (x3*x3*(y1-y2));
  if (a)
    z = -b/(2*a);
  else
    z = zmax;

     
  *rx = x;
  *ry = y;
  *rz = z;

  return(0);

}

int clip_stat3d(struct MRCvolume *v)
{
  float min, max, mean;
  float x, y, z;
  int   xmax, ymax, zmax;

  clip_get_stat3d(v, &min, &max, &mean, &xmax, &ymax, &zmax);
  clip_parxyz(v, xmax, ymax, zmax, &x, &y, &z);

  printf("max = %g  min = %g  mean = %g\n",max,min,mean);
  printf("location of max pixel ( %d, %d, %d) is \n", xmax, ymax, zmax);
  printf("( %.2f, %.2f, %.2f)\n", x, y, z);
  return(0);
}


int clip_get_stat3d(struct MRCvolume *v, 
                    float *rmin, float *rmax, float *rmean,
                    int *rx, int *ry, int *rz)
{
  int nx, ny, nz;
  int xmax, ymax, zmax;
  int i, j, k;
  float min, max, mean, m;
  double dmean;
  xmax = ymax = zmax = 0;

  max = FLT_MIN;
  min = 1e36f;
  dmean = 0.0;
  nx = v->vol[0]->xsize;
  ny = v->vol[0]->ysize;
  nz = v->zsize;

  for (k = 0; k < nz; k++){
    for(j = 0; j < ny; j++)
      for(i = 0; i < nx; i++){
        m = mrc_slice_getmagnitude(v->vol[k], i, j);
        if (m > max){
          max = m;
          xmax = i;
          ymax = j;
          zmax = k;
        }
        if (m < min)
          min = m;
        dmean += m;
      }
  }
  mean = (float)(dmean / (double)(nx * ny * nz));

  *rmin = min;
  *rmean = mean;
  *rmax = max;
  *rx = xmax;
  *ry = ymax;
  *rz = zmax;
  return(0);
}

int grap_stat(struct MRCheader *hin, struct Grap_options *opt)
{
  int i, j, k;
  int xmax, ymax;
  struct MRCvolume *v;
  Islice *slice;
  float min, max, mean = 0, std = 0, m;
  float x,y;
  float vmin, vmax, vmean;
  FILE *fout;

  /*     if (opt->dim == 3){
         v = grap_volume_read(hin, opt);
         clip_stat3d(v);
         grap_volume_free(v);
         return(0);
         }
  */

  /* printf("headersize %d\n", hin->headerSize); */

  printf("slice  |   min   |    max  |(      x,      y)|   mean    |   std dev.\n");
  printf("-------|---------|---------------------------|-----------|------------\n");
     
  set_input_options(opt, hin);

  for(k = 0; k < opt->nofsecs; k++){
    if ((opt->secs[k] < 0) || (opt->secs[k] >= hin->nz)){
      show_error("stat: slice out of range.");
      return(-1);
    }
    slice = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!slice){
      show_error("stat: error reading slice.");
      return(-1);
    }
    m = mrc_slice_getmagnitude(slice, 0, 0);
    min = m;
    max = m;
    std = 0;
    mean = 0;
    xmax = 0;
    ymax = 0;
    for(j = 0; j < slice->ysize; j++)
      for(i = 0; i < slice->xsize; i++){
        m = mrc_slice_getmagnitude(slice, i, j);
        if (m > max){
          max = m;
          xmax = i;
          ymax = j;
        }
        if (m < min)
          min = m;
        mean += m;
      }
    mean = mean / (float)(slice->xsize * slice->ysize);
    for(j = 0; j < slice->ysize; j++)
      for(i = 0; i < slice->xsize; i++){
        m = mrc_slice_getmagnitude(slice, i, j);
        std += (m - mean) * (m - mean);
      }
    std = std / (float)(slice->xsize * slice->ysize);
    std = (float)sqrt(std);

    /*	  for (di = 0, j = ymax - 1; j <= ymax + 1; j++)
          for(i = xmax - 1; i <= xmax + 1; i++ , di++)
          data[di] = mrc_slice_getmagnitude(slice, i, j);
    */	
    x = xmax;
    y = ymax;

    corr_getmax(slice, 7, xmax, ymax, &x, &y);

    if (opt->sano){
      x -= (float)slice->xsize/2;
      y -= (float)slice->ysize/2;
    }

    if (k == 0){
      vmin  = min;
      vmax  = max;
    }else{
      if (min < vmin) vmin = min;
      if (max > vmax) vmax = max;
    }
    vmean += mean;

    printf("%3d     %9.4f %9.4f (%7.2f,%7.2f) %9.4f  %9.4f\n", 
           opt->secs[k], min, max, x, y, mean, std);
    mrc_slice_free(slice);
  }
  vmean /= (float)opt->nofsecs;
  printf("all     %9.4f %9.4f ( ---.--, ---.--) %9.4f    --.----\n", 
         vmin, vmax, vmean);
     
  /* DNM: This was inappropriate and apparently never worked because the 
     file was opened as append instead of write */
  /*
    fout = fopen(hin->pathname, "a");
    if (fout != NULL){
    hin->amin  = vmin;
    hin->amax  = vmax;
    hin->amean = vmean;
    mrc_head_write(fout, hin);
    if (ferror(fout))
    perror("update min, max and mean");
    fclose(fout);
    }
  */

  return(0);
}


int write_vol(struct MRCslice **vol, struct MRCheader *hout)
{
  int k;

  for (k = 0; k < hout->nz; k++){
    if (mrc_write_slice((void *)vol[k]->data.b, hout->fp, hout, k, 'z'))
      return -1;
    mrc_slice_calcmmm(vol[k]);
    if (!k){
      hout->amin = vol[k]->min;
      hout->amax = vol[k]->max;
      hout->amean = vol[k]->mean;
    }else{
      if (vol[k]->min < hout->amin)
        hout->amin = vol[k]->min;
      if (vol[k]->max > hout->amax)
        hout->amax = vol[k]->max;
      hout->amean += vol[k]->mean;
    }
  }
  hout->amean /= hout->nz;
  return mrc_head_write(hout->fp, hout);
}

int free_vol(struct MRCslice **vol, int z)
{
  int k;
  for (k = 0; k < z; k++){
    mrc_slice_free(vol[k]);
  }
  free(vol);
  return(0);
}


/*
  clipProject(char **argv, int argc,
  struct MRCheader *hin,
  struct MRCheader *hout,
  struct Grap_options *opt)
  {

  char command[1024];
  char axis = 'Y';
  float scale_add = 0, scale_mult = 1;
  int x1,x2,y1,y2,z1,z2;
  float start,end,inc;
  int width;

  sprintf(command, "echo '%s\n%s\n%d,%d,%d,%d%d,%d\n%c\n%g,%g,%g\n%d\n%d\n%d,%d\n%g\n' | xyzproj",
  argv[argc-2], argv[argc-1],
  x1,x2,y1,y2,z1,z2,
  axis,
  start, end, inc,
  width,
  opt->mode,
  scale_add, scale_mult,
  opt->pad);
  #ifndef __vms
  system(command);
  #endif

  }
*/
/*
$Log$

Revision 3.7  2004/09/21 22:31:13  mast
Added return 0 for split_rgb and join_rgb functions

Revision 3.6  2004/04/22 19:08:45  mast
Added error checks and returns on mrc I/O calls

Revision 3.5  2004/01/17 20:32:33  mast
Remove unneeded rewind

Revision 3.4  2004/01/16 18:09:52  mast
Added functions to split and join rgb images

*/
