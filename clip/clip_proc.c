/*
 *  clip_proc.c -- Processing functions for clip.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end
 */

#include <limits.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include "mrcfiles.h"
#include "mrcslice.h"
#include "sliceproc.h"
#include "b3dutil.h"
#include "clip.h"

#ifndef FLT_MIN
#define FLT_MIN INT_MIN
#endif
#ifndef FLT_MAX
#define FLT_MAX INT_MAX
#endif
#define KERNEL_MAXSIZE 7

/*
 * Common routine for the rescaling options including resize (no scale)
 */
int clip_scaling(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt,
                 int process)
{
  int k, z;
  Islice *slice;
  Istack *v;
  double min, alpha;

  z = set_options(opt, hin, hout);
  if (z < 0)
    return(z);

  /* give message, add title */
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
  case IP_RESIZE:
    mrc_head_label(hout, "clip: resized image");
    break;
  default:
    return(-1);
  }

  if (opt->val == IP_DEFAULT)
    opt->val = 1.0f;
  alpha = opt->val;

  for (k = 0; k < opt->nofsecs; k++) {
    slice = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!slice){
      show_error("clip: Error reading slice.");
      return(-1);
    }

    /* 2D: Scale based on min/max/mean of individual slice */
    if (opt->dim == 2 && process != IP_RESIZE) {
      sliceMMM(slice);
      if (process == IP_BRIGHTNESS)
        min = (double)slice->min;
      else if (process == IP_SHADOW)
        min = (double)slice->max;
      else
        min = (double)slice->mean;
    }
      
    if (process != IP_RESIZE)
      mrc_slice_lie(slice, min, alpha);
    if (clipWriteSlice(slice, hout, opt, k, &z, 1))
      return -1;
  }

  return set_mrc_coords(opt);  
}

/*
 * Common routine for the edge filters that are not simple convolutions
 */
int clipEdge(MrcHeader *hin, MrcHeader *hout,
             ClipOptions *opt, int process)
{
  Islice *s;
  int i, k, z;
  Islice *slice;
  char *message;
  double scale, fixval;

  if (opt->mode == IP_DEFAULT)
    opt->mode = (process == IP_GRADIENT) ? hin->mode : MRC_MODE_BYTE;

  if (hin->mode != MRC_MODE_BYTE && hin->mode != MRC_MODE_SHORT && 
      hin->mode != MRC_MODE_USHORT && hin->mode != MRC_MODE_FLOAT) {
    show_error("clip edge: only byte, integer and float modes can be used");
    return -1;
  }

  z = set_options(opt, hin, hout);
  if (z < 0)
    return(z);

  /* give message, add title */
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

  printf("clip: %s %d slices...\n", message, opt->nofsecs);
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

    if (clipWriteSlice(slice, hout, opt, k, &z, 1))
      return -1;
  }
  return set_mrc_coords(opt);  
}

/*
 * Common routine for kernel convolution filtering
 */
int clip_convolve(MrcHeader *hin, 
                 MrcHeader *hout,
                 ClipOptions *opt, int process)
{
  Islice *s;
  float *blur;
  int i, k, z, iter, dim = 3, niter = 1;
  char title[60];
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
  float gaussianKernel[KERNEL_MAXSIZE * KERNEL_MAXSIZE];
  Islice *slice;
  char *message;

  if (opt->mode == IP_DEFAULT)
    opt->mode = (process == IP_SMOOTH) ? hin->mode : MRC_MODE_FLOAT;

  z = set_options(opt, hin, hout);
  if (z < 0)
    return(z);

  /* give message, add title */
  switch (process) {
  case IP_SMOOTH:
    if (opt->val > 1.)
      niter = B3DNINT(opt->val);
    if (opt->low > 0.) {
      scaledGaussianKernel(&gaussianKernel[0], &dim, KERNEL_MAXSIZE, opt->low);
      message = "Gaussian kernel smoothing";
      sprintf(title, "clip: Gaussian smoothing, sigma %.2f, %d iterations",
              opt->low, niter);
      blur = gaussianKernel;
    } else {
      message = "Smoothing";
      sprintf(title, "clip: Standard smoothing, %d iterations", niter);
      blur = smoothKernel;
    }
    mrc_head_label(hout, title);
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

  printf("clip: %s %d slices...\n", message, opt->nofsecs);
  for (k = 0; k < opt->nofsecs; k++) {
    s = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!s){
      show_error("clip: Error reading slice.");
      return(-1);
    }

    for (iter = 0; iter < niter; iter++) {
      s->mean = hin->amean;
      if (process == IP_SMOOTH)
        sliceMMM(s);
      slice = slice_mat_filter(s, blur, dim);
      if (!slice) {
        show_error("clip: Error getting new slice for filtering.");
        return(-1);
      }
      sliceFree(s);
      s = slice;
    }

    if (clipWriteSlice(slice, hout, opt, k, &z, 1))
      return -1;
  }

  return set_mrc_coords(opt);  
}

/*
 * 2D or 3D median filtering
 */
int clipMedian(MrcHeader *hin, MrcHeader *hout,
             ClipOptions *opt)
{
  Islice *s;
  int i, j, k, z;
  Islice *slice;
  Istack v;
  char title[40];
  int numInVol, firstInVol, lastInVol, firstNeed, lastNeed, depth, size;

  if (hin->mode != MRC_MODE_BYTE && hin->mode != MRC_MODE_SHORT && 
      hin->mode != MRC_MODE_USHORT && hin->mode != MRC_MODE_FLOAT) {
    show_error("clip median: only byte, integer and float modes can be used");
    return -1;
  }

  z = set_options(opt, hin, hout);
  if (z < 0)
    return(z);

  if (opt->val == IP_DEFAULT)
    opt->val = 3;
  if (opt->mode != MRC_MODE_BYTE && opt->mode != MRC_MODE_SHORT && 
      hin->mode != MRC_MODE_USHORT && opt->mode != MRC_MODE_FLOAT)
    opt->mode = hin->mode;
  size = (int)B3DMAX(2, opt->val);

  sprintf(title, "clip: %dD median filter, size %d", opt->dim, size);
  mrc_head_label(hout, title);
  printf("clip: median filtering %d slices...\n", opt->nofsecs);

  /* Get slice for output */
  slice = sliceCreate(opt->ix, opt->iy, opt->mode);
  if (!slice)
    return (-1);

  depth = opt->dim == 2 ? 1 : size;
  v.vol = (Islice **)malloc(depth * sizeof(Islice *));
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
    if (clipWriteSlice(slice, hout, opt, k, &z, 0))
      return -1;
  }
  
  /* clean up */
  sliceFree(slice);
  for (i = 0; i < v.zsize; i++)
    sliceFree(v.vol[i]);
  return set_mrc_coords(opt);  
}

/*
 * Anisotropic diffusion on slices
 */
int clipDiffusion(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  int i, k, z;
  Islice *slice;
  double kk, lambda;
  int iterations, CC;

  if (hin->mode != MRC_MODE_BYTE && hin->mode != MRC_MODE_SHORT && 
      hin->mode != MRC_MODE_USHORT && hin->mode != MRC_MODE_FLOAT) {
    show_error("clip diffusion: only byte, integer and float modes can "
               "be used");
    return -1;
  }

  z = set_options(opt, hin, hout);
  if (z < 0)
    return(z);

  /* Set the parameters supplying weird defaults */
  if (opt->val == IP_DEFAULT)
    opt->val = 5.;
  iterations = B3DMAX(1, (int)opt->val);
  if (opt->thresh == IP_DEFAULT)
    opt->thresh = 2;
  CC = B3DMAX(1, B3DMIN(3, (int)(opt->thresh)));
  if (opt->weight == IP_DEFAULT)
    opt->weight = 2.;
  kk = B3DMAX(0, opt->weight);
  if (opt->low == IP_DEFAULT)
    opt->low = 0.2f;
  lambda = B3DMAX(0.001, opt->low);

  /* give message, add title */
  mrc_head_label(hout, "clip: diffusion");

  printf("clip: anistropic diffusion %d slices...\n", opt->nofsecs);
  for (k = 0; k < opt->nofsecs; k++) {
    slice = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!slice){
      show_error("clip: Error reading slice.");
      return(-1);
    }

    if (sliceAnisoDiff(slice, opt->mode, CC, kk, lambda, iterations, 
                       ANISO_CLEAR_AT_END))
      return -1;

    if (clipWriteSlice(slice, hout, opt, k, &z, 1))
      return -1;
  }

  return set_mrc_coords(opt);  
}

/*
 * All flipping operations
 */
int grap_flip(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  int i,j,k, rotx, numDone, numTodo, maxSlices, yst, ynd, ydir, csize, dsize;
  int err;
  float memlim, memmin = 512000000., memmax = 2048000000.;
  float sizefac = 8.;
  size_t lineOfs;
  float ycen, zcen;
  Islice *sl, *tsl;
  Islice **yslice;
  Ival val;
  IloadInfo li;

  hout->mode = hin->mode;

  if ((!strncmp(opt->command, "flipxy",6)) || 
      (!strncmp(opt->command, "flipyx",6))){
    mrc_head_label(hout, "clip: flipxy");
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
    mrc_head_label(hout, "clip: flipxz");
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
      (!strncmp(opt->command, "flipzy",6)) ||
      (!strncmp(opt->command, "rotx",4))) {
    rotx = strncmp(opt->command, "rotx",4) ? 0 : 1;
    if (rotx)
      mrc_head_label(hout, "clip: rotx - rotation by -90 around X");
    else
      mrc_head_label(hout, "clip: flipyz");
    hout->nx = hin->nx;
    hout->mx = hin->mx;
    hout->xlen = hin->xlen;
    hout->ny = hin->nz;
    hout->my = hin->mz;
    hout->ylen = hin->zlen;
    hout->nz = hin->ny;
    hout->mz = hin->my;
    hout->zlen = hin->ylen;

    /* For rotation try to adjust the header as in rotatevol */
    if (rotx && hin->my && hin->ylen && hin->mz && hin->zlen ) {
      for (i = 0; i < 3; i++)
        hout->tiltangles[i] = hout->tiltangles[i + 3];
      hout->tiltangles[3] -= 90.;
      ycen = hin->ny / 2. - hin->yorg * hin->my / hin->ylen;
      zcen = hin->nz / 2. - hin->zorg * hin->mz / hin->zlen;
      hout->yorg = (hout->ny / 2. - zcen) * hout->ylen / hout->my;
      hout->zorg = (hout->nz / 2. + ycen) * hout->zlen / hout->mz;
    }
      
    if (mrc_head_write(hout->fp, hout))
      return -1;

    /* Get the maximum number of output slices to load, and get input slices
       with that size in Y */
    mrc_getdcsize(hout->mode, &dsize, &csize);
    memlim = (((dsize * hout->nx / sizefac) * hout->ny) * hout->nz);
    memlim = B3DMAX(memmin, B3DMIN(memmax, memlim));
    maxSlices = (int)((memlim / (dsize * hout->nx)) / hout->ny);
    maxSlices = B3DMAX(1, B3DMIN(hout->nz / (2 * dsize), maxSlices));
    yslice = (Islice **)malloc(hin->nz * sizeof(Islice));
    if (!yslice) {
      printf("ERROR: CLIP - getting memory for slice array\n");
      return (-1);
    }
    for (k = 0; k < hin->nz; k++) {
      yslice[k] = sliceCreate(hout->nx, maxSlices, hout->mode);
      if (!yslice[k]) {
        printf("ERROR: CLIP - getting memory for slices\n");
        return (-1);
      }
    }
    /* printf("memlim %.0f  maxslices %d memory %d\n", memlim, maxSlices, hout->nx * maxSlices * hin->nz * dsize); */
    mrc_init_li(&li, NULL);
    mrc_init_li(&li, hin);
    numDone = 0;

    /* Loop on chunks in Z of output */
    while (numDone < hout->nz) {
      numTodo = B3DMIN(maxSlices, hout->nz - numDone);

      /* Set up loading limits and limits for output loop */
      if (rotx) {
        li.ymax = hout->nz - 1 - numDone;
        li.ymin  = li.ymax - (numTodo - 1);
        ydir = -1;
        yst = numTodo - 1;
        ynd = 0;
      } else {
        li.ymin = numDone;
        li.ymax = numDone + numTodo - 1;
        ydir = 1;
        ynd = numTodo - 1;
        yst = 0;
      }

      /* Load the slices within the Y range*/
      for (k = 0; k < hin->nz; k++) {
        if ((err = mrcReadZ(hin, &li, yslice[k]->data.b, k))) {
          printf("ERROR: CLIP - Reading section %d, y %d to %d (error # %d)\n",
                 k, li.ymin, li.ymax, err);
          return -1;
        }
      }
      
      /* Write Z slices in order, line by line */
      for (j = yst; j * ydir <= ynd * ydir; j += ydir) {
        lineOfs = (size_t)(hout->nx * dsize) * (size_t)j;
        for (k = 0; k < hin->nz; k++) {
          if (b3dFwrite(yslice[k]->data.b + lineOfs, dsize, hout->nx, hout->fp)
              != hout->nx) {
            printf("ERROR: CLIP - Writing section %d, line %d\n", numDone + 
                   ydir * (j - yst), k);
            return -1;
          }
        }
      }
      numDone += numTodo;
    }

    /* Clean up */
    for (k = 0; k < hin->nz; k++)
      sliceFree(yslice[k]);
    puts(" Done!");
    return(0);
  }

  if ((!strncmp(opt->command, "flipx",5)) ||
      (!strncmp(opt->command, "flipy",5))){
    if (!strncmp(opt->command, "flipx",5))
      mrc_head_label(hout, "clip: flipx");
    else
      mrc_head_label(hout, "clip: flipy");
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
    mrc_head_label(hout, "clip: flipz");
    hout->nx = hin->nx;
    hout->ny = hin->ny;
    hout->nz = hin->nz;
    if (mrc_head_write(hout->fp, hout))
      return -1;
    tsl = sliceCreate(hout->nx, hout->ny, hout->mode);

    /* DNM 11/14/08: switch to doing file in order, don't pretend it could be
       done on a file in place */
    for (k = 0; k < hin->nz; k++){
      if (mrc_read_slice(tsl->data.b, hin->fp, hin, hout->nz-k-1, 'z') ||
          mrc_write_slice(tsl->data.b, hout->fp, hout, k, 'z'))
        return -1;
    }
    sliceFree(tsl);
    puts(" Done!");
    return(0);
  }
	  
  show_warning("clip flip - no flipping was done.");
  return(-1);
}

/*
 * 3-D color - conversion to shades of one color
 */
int grap_color(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
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
    show_warning("clip 3d color - input and output sizes ignored.");

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

/*
 * 2D conversion to shades of a color
 */
int clip2d_color(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  Islice *slice;
  int k, z, i, j;
  unsigned char bdata;
  float pixel, pixin;
  int xysize;
  Islice *s;
  Ival val;

  if ((opt->mode != MRC_MODE_RGB) && (opt->mode != IP_DEFAULT)){
    show_warning("clip - color output mode must be rgb.");
  }
  opt->mode = MRC_MODE_RGB;
  z = set_options(opt, hin, hout);
  if (z < 0)
    return(z);

  show_status("False Color...\n");
  mrc_head_label(hout, "CLIP Color");

  s = sliceCreate(opt->ix, opt->iy, MRC_MODE_RGB);
  if (!s) {
    show_error("clip - creating slice");
    return -1;
  }

  for (k = 0; k < opt->nofsecs; k++) {
    slice = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!slice){
      show_error("clip: Error reading slice.");
      return(-1);
    }

    for (j = 0; j < opt->iy; j++) {
      for (i = 0; i < opt->ix; i++) {
        sliceGetVal(slice, i, j, val);
        pixin = val[0];
        val[0] = B3DMAX(0., B3DMIN(255., pixin * opt->red + 0.5));
        val[1] = B3DMAX(0., B3DMIN(255., pixin * opt->green + 0.5));
        val[2] = B3DMAX(0., B3DMIN(255., pixin * opt->blue + 0.5));
        slicePutVal(s, i, j, val);
      }
    }

    if (clipWriteSlice(s, hout, opt, k, &z, 0))
      return -1;
    sliceFree(slice);
  }
  sliceFree(s);

  return set_mrc_coords(opt);  
}

/*
 * Join 3 separate byte files into an RGB file
 */
int clip_joinrgb(MrcHeader *h1, MrcHeader *h2,
		 MrcHeader *hout, ClipOptions *opt)
{
  int i,j,k,l;
  float val[5];
  MrcHeader hdr[3];
  Islice *s, *srgb[3];

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

  s = sliceCreate(h1->nx, h1->ny, MRC_MODE_RGB);
  for (i = 0; i < 3; i++) {
    srgb[i] = sliceCreate(h1->nx, h1->ny, MRC_MODE_BYTE);
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
    sliceFree(srgb[i]);
  sliceFree(s);
  return(0);
}

/*
 * Split an RGB file into 3 separate files
 */
int clip_splitrgb(MrcHeader *h1, ClipOptions *opt)
{
  int i,j,k,l;
  float val[5];
  char *ext[3] = {".r", ".g", ".b"};
  MrcHeader hdr[3];
  char *fname;
  int len = strlen(opt->fnames[1]);
  Islice *s, *srgb[3];

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
  s = sliceCreate(h1->nx, h1->ny, MRC_MODE_RGB);
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
    hdr[i].sectionSkip = 0;
    hdr[i].next = 0;
    mrc_head_label(&hdr[i], "CLIP Split RGB into 3 files");
    if (mrc_head_write(hdr[i].fp, &hdr[i]))
      return -1;
    srgb[i] = sliceCreate(h1->nx, h1->ny, MRC_MODE_BYTE);
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
      sliceMMM(srgb[i]);
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
    sliceFree(srgb[i]);
  }
  sliceFree(s);
  return(0);
}

/*
 * 3D averaging of multiple files
 */
int grap_average(MrcHeader *h1, MrcHeader *h2, MrcHeader *hout,
                 ClipOptions *opt)
{
  Islice *s, *so;
  MrcHeader **hdr;
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

  s = sliceCreate(h1->nx, h1->ny, h1->mode);
  if (h1->mode == MRC_MODE_COMPLEX_FLOAT){
    so = sliceCreate(h1->nx, h1->ny, 4);
    hout->mode = MRC_MODE_COMPLEX_FLOAT;
  }else if (h1->mode == MRC_MODE_RGB){
    so = sliceCreate(h1->nx, h1->ny, MRC_MODE_RGB);
    hout->mode = MRC_MODE_RGB;
  }else{
    so = sliceCreate(h1->nx, h1->ny, 2);
  }
  hdr = (MrcHeader **)malloc(opt->infiles * sizeof(MrcHeader *));
  if (!hdr)
    return(-1);
     
  for (f = 0; f < opt->infiles; f++){
    hdr[f] = (MrcHeader *)malloc(sizeof(MrcHeader));
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
    sliceMMM(so);
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
  sliceFree(s);
  sliceFree(so);
  return(0);
}

/*
 * 2D averaging of input slices into one slice 
 */
int clip2d_average(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  Islice *slice;
  Islice *avgs, *cnts;
  Ival val, aval;
  int k, z, i, j;
  int thresh = FALSE;
  float dval,scale;
     
  if (opt->ox != IP_DEFAULT || opt->oy != IP_DEFAULT || 
      opt->oz != IP_DEFAULT)
    show_warning("clip - ox, oy, oz have no effect for 2d average.");
    
  opt->ox = opt->oy = opt->oz = IP_DEFAULT;
  set_input_options(opt, hin);
  opt->oz = 1;
  z = set_output_options(opt, hout);
  if (z < 0)
    return(z);

  mrc_head_label_cp(hin, hout);
  mrc_head_label(hout, "CLIP: 2D Average");

  if (opt->val != IP_DEFAULT)
    thresh = TRUE;

  show_status("2D Average...\n");

  avgs = sliceCreate(opt->ix, opt->iy, SLICE_MODE_MAX);
  cnts = sliceCreate(opt->ix, opt->iy, SLICE_MODE_FLOAT);

  /* Initialize sums */
  aval[0] = aval[1] = aval[2] = 0.0f;
  for(j = 0; j < avgs->ysize; j++)
    for(i = 0; i < avgs->xsize; i++){
      slicePutVal(avgs, i, j, aval);
      cnts->data.f[i + (j * cnts->xsize)] = 0.0f;
    }

  for (k = 0; k < opt->nofsecs; k++) {
    slice = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!slice){
      show_error("clip: Error reading slice.");
      return(-1);
    }

    /* Add each pixel into average */
    for(j = 0; j < slice->ysize; j++)
      for(i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j,  val);
        sliceGetVal(avgs,  i, j, aval);
        aval[0] += val[0];
        aval[1] += val[1];
        aval[2] += val[2];
		    
        /* If thresholding, only put sum back if magnitude is greater than
           threshold.  In any case, add up the number of items contributing 
           to the average */
        if (thresh){
          dval = sliceGetPixelMagnitude(slice, i, j);
          if (dval > opt->val){
            cnts->data.f[i + (j * cnts->xsize)] += 1.0f;
            slicePutVal(avgs,  i, j, aval);
          }
        }else{
          cnts->data.f[i + (j * cnts->xsize)] += 1.0f;
          slicePutVal(avgs,  i, j, aval);
        }
      }
    sliceFree(slice);
  }

  /* Set scaling to keep RGB output in byte range on top end */
  scale = 1.0;
  if (opt->mode == MRC_MODE_RGB){
    sliceMMM(avgs);
    if (avgs->max > 0)
      scale = opt->nofsecs * 255.0f/avgs->max;
  }

  /* create the output slice by dividing each value by counts */
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

  /* Take care of min/max/mean and write the slice */
  sliceMMM(slice);
  hout->amin = B3DMIN(slice->min, hout->amin);
  hout->amax = B3DMAX(slice->max, hout->amax);
  if (opt->add2file != IP_APPEND_OVERWRITE)
    hout->amean += slice->mean / hout->nz;
  if (mrc_write_slice((void *)slice->data.b, hout->fp, hout, z, 'z'))
    return -1;
  if (mrc_head_write(hout->fp, hout))
    return -1;
  sliceFree(avgs);
  sliceFree(cnts);
  return(0);
}


int clip_parxyz(Istack *v, 
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
  y1 = sliceGetPixelMagnitude(v->vol[zmax], x1, ymax);
  y2 = sliceGetPixelMagnitude(v->vol[zmax], x2, ymax);
  y3 = sliceGetPixelMagnitude(v->vol[zmax], x3, ymax);
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
  y1 = sliceGetPixelMagnitude(v->vol[zmax], xmax, x1);
  y2 = sliceGetPixelMagnitude(v->vol[zmax], xmax, x2);
  y3 = sliceGetPixelMagnitude(v->vol[zmax], xmax, x3);
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
  y1 = sliceGetPixelMagnitude(v->vol[x1], xmax, ymax);
  y2 = sliceGetPixelMagnitude(v->vol[x2], xmax, ymax);
  y3 = sliceGetPixelMagnitude(v->vol[x3], xmax, ymax);
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

int clip_stat3d(Istack *v)
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


int clip_get_stat3d(Istack *v, 
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
        m = sliceGetPixelMagnitude(v->vol[k], i, j);
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

int grap_stat(MrcHeader *hin, ClipOptions *opt)
{
  int i, j, k, iz, di, dj;
  int xmax, ymax, xmin, ymin, zmin, zmax;
  Istack *v;
  Islice *slice;
  float min, max, m, ptnum;
  double mean, std, sumsq, vmean, vsumsq, cx, cy, data[3][3];
  float x,y;
  float vmin, vmax;
  FILE *fout;
  b3dInt16 *sdata;

  /*     if (opt->dim == 3){
         v = grap_volume_read(hin, opt);
         clip_stat3d(v);
         grap_volume_free(v);
         return(0);
         }
  */

  /* printf("headersize %d\n", hin->headerSize); */

  printf("slice|   min   |(   x,   y)|    max  |(      x,      y)|   mean    |  std dev.\n");
  printf("-----|---------|-----------|---------|-----------------|-----------|----------\n");
     
  set_input_options(opt, hin);

  for (k = 0; k < opt->nofsecs; k++) {
    iz = opt->secs[k];
    if ((iz < 0) || (iz >= hin->nz)) {
      show_error("stat: slice out of range.");
      return(-1);
    }
    slice = sliceReadSubm(hin, iz, 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!slice){
      show_error("stat: error reading slice.");
      return(-1);
    }
    m = sliceGetPixelMagnitude(slice, 0, 0);
    min = m;
    max = m;
    std = 0;
    sumsq = 0.;
    mean = 0;
    xmax = 0;
    ymax = 0;
    xmin = 0;
    ymin = 0;
    ptnum = (float)(slice->xsize * slice->ysize);
    for (j = 0; j < slice->ysize; j++) {
      switch (slice->mode) {

        /* It only helps 25% so just do it for common data types */
      case SLICE_MODE_SHORT:
        sdata = &slice->data.s[slice->xsize * j];
        for (i = 0; i < slice->xsize; i++) {
          m = *sdata++;
          if (m > max){
            max = m;
            xmax = i;
            ymax = j;
          }
          if (m < min) {
            min = m;
            xmin = i;
            ymin = j;
          }
          mean += m;
          sumsq += m * m;
        }
        break;
        
      default:
        for (i = 0; i < slice->xsize; i++) {
          m = sliceGetPixelMagnitude(slice, i, j);
          if (m > max){
            max = m;
            xmax = i;
            ymax = j;
          }
          if (m < min) {
            min = m;
            xmin = i;
            ymin = j;
          }
          mean += m;
          sumsq += m * m;
        }
        break;
      }
    }
    mean = mean / ptnum;

    /* DNM 5/23/05: switched to computing from differences to using sum of
       squares, which gives identical results with doubles  */

    std = (sumsq - ptnum * mean * mean) / B3DMAX(1.,(ptnum - 1.));
    std = sqrt(B3DMAX(0., std));
  
    /* 3/23/09: switched to parabolic fit, center of gravity is ridiculous */
    for (dj = 0, j = ymax - 1; j <= ymax + 1; j++, dj++)
      for(di = 0,i = xmax - 1; i <= xmax + 1; i++ , di++)
        data[dj][di] = sliceGetPixelMagnitude(slice, i, j);

    parabolic_fit(&cx, &cy, data);
    x = cx + xmax;
    y = cy + ymax;
    
    /* x = xmax;
       y = ymax;
       corr_getmax(slice, 7, xmax, ymax, &x, &y); */

    if (opt->sano){
      x -= (float)slice->xsize/2;
      y -= (float)slice->ysize/2;
    }

    if (k == 0) {
      vmin  = min;
      vmax  = max;
      vmean = 0.;
      zmin = iz;
      zmax = 0;
      vsumsq = 0.;
    } else {
      if (min < vmin) {
        vmin = min;
        zmin = iz;
      }
      if (max > vmax) {
        vmax = max;
        zmax = iz;
      }
    }
    vmean += mean;
    vsumsq += sumsq;

    printf("%4d  %9.4f (%4d,%4d) %9.4f (%7.2f,%7.2f) %9.4f  %9.4f\n", 
           iz, min, xmin, ymin, max, x, y, mean, std);
    sliceFree(slice);
  }
  vmean /= (float)opt->nofsecs;
  ptnum *= opt->nofsecs;
  std = (vsumsq - ptnum * vmean * vmean) / B3DMAX(1.,(ptnum - 1.));
  std = sqrt(B3DMAX(0., std));

  printf(" all  %9.4f (@ z=%5d) %9.4f (@ z=%5d      ) %9.4f  %9.4f\n",
         vmin, zmin, vmax, zmax, vmean, std);
     
  return(0);
}


int write_vol(Islice **vol, MrcHeader *hout)
{
  int k;

  for (k = 0; k < hout->nz; k++){
    if (mrc_write_slice((void *)vol[k]->data.b, hout->fp, hout, k, 'z'))
      return -1;
    sliceMMM(vol[k]);
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

int free_vol(Islice **vol, int z)
{
  int k;
  for (k = 0; k < z; k++){
    sliceFree(vol[k]);
  }
  free(vol);
  return(0);
}


/*
  clipProject(char **argv, int argc,
  MrcHeader *hin,
  MrcHeader *hout,
  ClipOptions *opt)
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
Revision 3.26  2008/12/11 23:51:15  mast
Remove diagnostic output

Revision 3.25  2008/11/15 00:49:56  mast
Fixed bug in flipz of an odd file, made flipyz/rotx use up to 2x memory
for a big file

Revision 3.24  2007/11/23 01:05:58  mast
Added iterations for smoothing

Revision 3.23  2007/11/22 20:48:30  mast
Added gaussian kernel smoothing

Revision 3.22  2007/08/30 20:14:16  mast
Made flipyz/rotx go in chunks, much faster

Revision 3.21  2007/06/13 17:03:25  sueh
bug# 1019 Setting hdr.sectionSkip in clip_splitrgb.

Revision 3.20  2007/02/04 21:21:29  mast
Eliminated mrcspectral includes

Revision 3.19  2007/02/04 21:19:48  mast
Eliminated mrcspectral includes

Revision 3.18  2007/02/04 21:10:15  mast
Function name changes from mrcslice cleanup

Revision 3.17  2006/08/04 21:04:50  mast
Made clip stat a little faster for ints and added min location

Revision 3.16  2006/06/23 17:13:19  mast
Added rotx option and adjusted header as in rotatevol

Revision 3.15  2005/11/15 19:55:28  mast
Fixed initialization of grand sum for stat

Revision 3.14  2005/11/11 22:14:56  mast
Changes for unsigned file mode

Revision 3.13  2005/05/23 23:31:29  mast
Switched mean and SD computation to use doubles

Revision 3.12  2005/02/11 01:42:32  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.11  2005/01/28 05:43:08  mast
Changed defaults for diffusion to match 3dmod

Revision 3.10  2005/01/27 05:55:17  mast
Added anisotropic diffusion option

Revision 3.9  2005/01/17 17:11:02  mast
Changes for new typedefs and 2D processing scheme

Revision 3.8  2005/01/07 20:13:59  mast
Fixed problems with filtering and scaling, added many filtering operations


Revision 3.7  2004/09/21 22:31:13  mast
Added return 0 for split_rgb and join_rgb functions

Revision 3.6  2004/04/22 19:08:45  mast
Added error checks and returns on mrc I/O calls

Revision 3.5  2004/01/17 20:32:33  mast
Remove unneeded rewind

Revision 3.4  2004/01/16 18:09:52  mast
Added functions to split and join rgb images

*/
