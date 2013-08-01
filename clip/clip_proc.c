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

static void writeBytePixel(float pixel, MrcHeader *hout);
static int quadrantSample(Islice *slice, int llx, int lly, int urx, int ury, 
                          double *mean);
static void correctQuadrant(Islice *slice, int llx, int lly, int urx, int ury, double g,
                            double base);


/*
 * Common routine for the rescaling options including resize (no scale)
 */
int clip_scaling(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  int i,j,k, l, z;
  Ival val;
  Islice *slice;
  double min, alpha;
  float hival, loval;
  int truncLo = 0, truncHi = 0, truncToMean = 0;

  z = set_options(opt, hin, hout);
  if (z < 0)
    return(z);

  /* give message, add title */
  switch (opt->process) {
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
  case IP_TRUNCATE:
    show_status("Truncate...\n");
    if (opt->low != IP_DEFAULT)
      truncLo = 1;
    if (opt->high != IP_DEFAULT)
      truncHi = 1;
    if (opt->sano)
      truncToMean = 1;
    if (!truncLo && !truncHi) {
      fprintf(stderr, "clip truncate: You must enter a low or a high limit\n");
      return -1;
    }
    mrc_head_label(hout, "clip: truncated");
    break;
  case IP_UNWRAP:
    show_status("Unwrap...\n");
    if (hin->mode != MRC_MODE_SHORT && hin->mode != MRC_MODE_USHORT) {
      fprintf(stderr, "clip truncate: Mode must be short or unsigned short integers\n");
      return -1;
    }
    if (hin->mode == MRC_MODE_USHORT && opt->val == IP_DEFAULT) {
      fprintf(stderr, "clip truncate: You must enter a value to add with -n for mode "
              "6 input\n");
      return -1;
    }
    if (opt->val == IP_DEFAULT)
      opt->val = 32768.;
    mrc_head_label(hout, "clip: unwrapped integer values");
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
    if ((opt->dim == 2 && opt->process != IP_RESIZE) || 
        (opt->process == IP_TRUNCATE && truncToMean)) {
      sliceMMM(slice);
      if (opt->process == IP_BRIGHTNESS)
        min = (double)slice->min;
      else if (opt->process == IP_SHADOW)
        min = (double)slice->max;
      else
        min = (double)slice->mean;
    }
      
    if (opt->process == IP_TRUNCATE) {
      for (j = 0; j < opt->iy; j++) {
        for (i = 0; i < opt->ix; i++) {
          sliceGetVal(slice, i, j, val);
          for (l = 0; l < slice->csize; l++) {
            if (truncLo && val[l] < opt->low)
              val[l] = truncToMean ? min : opt->low;
            if (truncHi && val[l] > opt->high)
              val[l] = truncToMean ? min : opt->high;
          }
          slicePutVal(slice, i, j, val);
        }
      }

    } else if (opt->process == IP_UNWRAP) {

      /* Unwrap: add a value and wrap values around */
      hival = hin->mode == MRC_MODE_USHORT ? 65535 : 32767;
      loval = hival - 65535.;
      for (j = 0; j < opt->iy; j++) {
        for (i = 0; i < opt->ix; i++) {
          sliceGetVal(slice, i, j, val);
          val[0] += opt->val;
          if (val[0] > hival)
            val[0] -= 65536.;
          else if (val[0] < loval)
            val[0] += 65536.;
          slicePutVal(slice, i, j, val);
        }
      }

    } else if (opt->process != IP_RESIZE)
      mrc_slice_lie(slice, min, alpha);
    if (clipWriteSlice(slice, hout, opt, k, &z, 1))
      return -1;
  }

  return set_mrc_coords(opt);  
}

/*
 * Common routine for the edge filters that are not simple convolutions
 */
int clipEdge(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  Islice *s;
  int k, z;
  Islice *slice;
  char *message;
  double scale, fixval;

  if (opt->mode == IP_DEFAULT)
    opt->mode = (opt->process == IP_GRADIENT) ? hin->mode : MRC_MODE_BYTE;

  if (hin->mode != MRC_MODE_BYTE && hin->mode != MRC_MODE_SHORT && 
      hin->mode != MRC_MODE_USHORT && hin->mode != MRC_MODE_FLOAT) {
    show_error("clip edge: only byte, integer and float modes can be used");
    return -1;
  }

  z = set_options(opt, hin, hout);
  if (z < 0)
    return(z);

  /* give message, add title */
  switch (opt->process) {
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

    if (opt->process == IP_GRADIENT) {

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
      if (opt->process == IP_GRAHAM)
        sliceByteGraham(slice);
      else if (opt->process == IP_SOBEL)
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
int clip_convolve(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  Islice *s;
  float *blur;
  int k, z, iter, dim = 3, niter = 1;
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
    opt->mode = (opt->process == IP_SMOOTH) ? hin->mode : MRC_MODE_FLOAT;

  z = set_options(opt, hin, hout);
  if (z < 0)
    return(z);

  /* give message, add title */
  switch (opt->process) {
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
      if (opt->process == IP_SMOOTH)
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
  int firstInVol, lastInVol, firstNeed, lastNeed, depth, size;

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
  int k, z;
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
int clip_flip(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
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

      /* Load the slices within the Y range and shift bytes for output if needed */
      for (k = 0; k < hin->nz; k++) {
        if ((err = mrcReadZ(hin, &li, yslice[k]->data.b, k))) {
          printf("ERROR: CLIP - Reading section %d, y %d to %d (error # %d)\n",
                 k, li.ymin, li.ymax, err);
          return -1;
        }
        if (!hout->mode && hout->bytesSigned)
          b3dShiftBytes(yslice[k]->data.b, (char *)yslice[k]->data.b, hout->nx, 
                        li.ymax + 1 - li.ymin, 1, 1);
      }
      
      /* Write Z slices in order, line by line */
      for (j = yst; j * ydir <= ynd * ydir; j += ydir) {
        lineOfs = (size_t)(hout->nx * dsize) * (size_t)j;
        for (k = 0; k < hin->nz; k++) {
          if (b3dFwrite(yslice[k]->data.b + lineOfs, dsize, hout->nx, hout->fp) != 
              hout->nx) {
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
 * Quadrant correction based on averaging
 */
int clip_quadrant(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  int width = 20;
  int numGroups, itmp, group, lastOut, newMode = -1;
  int nx = hin->nx, ny = hin->ny, nz = hin->nz;
  int numTodo = nz;
  int groupSize = 1;
  int iz, jz, ind, indStart, indEnd, i, ninGroup;
  double d[8], dt[8], g1, g2, g3, g4, base, c2, c3, c4, qmin, qmax, denom, userBase = 0;
  int vx1, vx2, vx3, vx4, hx1, hx2, hx3, hx4, vy1, vy2, vy3, vy4, hy1, hy2, hy3, hy4;
  Islice *slice;

  if ((opt->ix !=  IP_DEFAULT) || (opt->iy !=  IP_DEFAULT) || (opt->ox !=  IP_DEFAULT) ||
      (opt->oy !=  IP_DEFAULT) || (opt->oz !=  IP_DEFAULT))
    show_warning("clip quadrant - input and output sizes ignored.");
  
  if (opt->nofsecs != IP_DEFAULT) {
    numTodo = opt->nofsecs;

    /* Sort the entries */
    for (iz = 0; iz < numTodo - 1; iz++) {
      for (jz = iz + 1; jz < numTodo; jz++) {
        if (opt->secs[jz] < opt->secs[iz]) {
          itmp = opt->secs[jz];
          opt->secs[jz] = opt->secs[iz];
          opt->secs[iz] = itmp;
        }
      }
    }

    /* Eliminate duplicates */
    jz = 0;
    for (iz = 0; iz < numTodo; iz++)
      if (!jz || opt->secs[jz - 1] != opt->secs[iz])
        opt->secs[jz++] = opt->secs[iz];
    numTodo = jz;

  } else

    /* Or else get the default list */
    set_input_options(opt, hin);
  opt->nofsecs = nz;
  if (opt->val != IP_DEFAULT)
    groupSize = B3DNINT(opt->val);
  if (opt->high != IP_DEFAULT)
    width = B3DNINT(opt->high);
  if (opt->low != IP_DEFAULT)
    userBase = opt->low;
  if (width < 2 || width > nx / 4 || width > ny / 4) {
    show_error("clip: width entry too small or too large.");
    return -1;
  }
  groupSize = B3DMAX(1, B3DMIN(nz, groupSize));
  numGroups = B3DMAX(1, numTodo / groupSize);

  hout->mode = hin->mode;
  if (opt->mode != IP_DEFAULT && opt->mode != hin->mode) {
    newMode = sliceModeIfReal(opt->mode);
    hout->mode = opt->mode;
    if (newMode < 0) {
      show_error("clip: Inappropriate new mode entry.");
      return(-1);
    }
  }

  hout->amean = 0.;
  hout->amin = 1.e37;
  hout->amax = -1.e37;
  if (mrcCopyExtraHeader(hin, hout))
    show_warning("clip warning: failed to copy extra header data");

  /* Get quadrant sample coordinates */
  vx3 = nx / 2 + 1;
  vx4 = vx3 + width;
  vx2 = vx3 - 2;
  vx1 = vx2 - width;
  hx1 = nx / 20;
  hx2 = nx / 2 - 10;
  hx3 = nx / 2 + 10;
  hx4 = 19 * nx / 20;
  hy3 = ny / 2 + 1;
  hy4 = hy3 + width;
  hy2 = hy3 - 2;
  hy1 = hy2 - width;
  vy1 = ny / 20;
  vy2 = ny / 2 - 10;
  vy3 = ny / 2 + 10;
  vy4 = 19 * ny / 20;

  /* Loop on the groups */
  indStart = 0;
  lastOut = -1;
  for (group = 0; group < numGroups; group++) {
    ninGroup = groupSize;
    if (group < numTodo % groupSize)
      ninGroup++;
    
    /* Read each slice for sampling */
    for (i = 0; i < 8; i++)
      d[i] = 0.;
    for (ind = indStart; ind < indStart + ninGroup; ind++) {
      iz = opt->secs[ind];
      slice = sliceReadMRC(hin, iz, 'z');
      if (!slice){
        show_error("clip: Error reading slice.");
        return(-1);
      }

      /* Get samples and accumulate means */
      if (quadrantSample(slice, hx3, hy3, hx4, hy4, &dt[0]) || 
          quadrantSample(slice, vx3, vy3, vx4, vy4, &dt[1]) || 
          quadrantSample(slice, vx1, vy3, vx2, vy4, &dt[3]) || 
          quadrantSample(slice, hx1, hy3, hx2, hy4, &dt[2]) || 
          quadrantSample(slice, hx1, hy1, hx2, hy2, &dt[4]) || 
          quadrantSample(slice, vx1, vy1, vx2, vy2, &dt[5]) || 
          quadrantSample(slice, vx3, vy1, vx4, vy2, &dt[7]) || 
          quadrantSample(slice, hx3, hy1, hx4, hy2, &dt[6]))
        return -1;
      for (i = 0; i < 8; i++)
        d[i] += dt[i];
      sliceFree(slice);
    }

    /* Determine min and max in case a base is needed for logs */
    qmin = 1.e30;
    qmax = -qmin;
    for (i = 0; i < 8; i++) {
      d[i] /= ninGroup;
      qmin = B3DMIN(qmin, d[i]);
      qmax = B3DMAX(qmax, d[i]);
    }

    /* Set or adjust base including the user value */
    base = (userBase ? 0.01 : 0.05) * (qmax - qmin) - qmin - userBase;
    if (base > 0)
      show_warning("clip - intensities being adjusted to avoid taking log of small "
                   "or negative values");
    base = B3DMAX(0., base) + userBase;

    /* printf("%.0f %.0f %.0f %.0f %.0f %.0f %.0f %.0f \n", d[1], d[3], d[2], d[4], d[5],
       d[7], d[6], d[0]); */

    /* Take logs and solve equations for log scaling */
    for (i = 0; i < 8; i++)
      dt[i] = log10(d[i] + base);
    /* printf("%f %f %f %f %f %f %f %f \n", dt[1], dt[3], dt[2], dt[4], dt[5], dt[7], 
       dt[6], dt[0]); */
    c2 = dt[0] - dt[6] + 2. * dt[1] - 2. * dt[3] + dt[4] - dt[2];
    c3 = dt[0] - dt[6] + dt[1] - dt[3] + dt[2] - dt[4] + dt[7] - dt[5];
    c4 = 2. * dt[0] - 2. * dt[6] + dt[1] - dt[3] + dt[5] - dt[7];
    denom = determ3(6., 2., 4., 2., 4., 2., 4., 2., 6.);
    g2 = determ3(c2, 2., 4., c3, 4., 2., c4, 2., 6.) / denom;
    g3 = determ3(6., c2, 4., 2., c3, 2., 4., c4, 6.) / denom;
    g4 = determ3(6., 2., c2, 2., 4., c3, 4., 2., c4) / denom;
    /* printf("c's: %f %f %f  denom %f lg's: %f %f %f\n", c2, c3, c4, denom, g2, 
       g3, g4); */
    g1 = pow(10., -(g2 + g3 + g4));
    g2 = pow(10., g2);
    g3 = pow(10., g3);
    g4 = pow(10., g4);
    if (ninGroup > 1)
      printf("Group from %d to %d:", opt->secs[indStart], iz);
    else
      printf("Section %d:", iz);
    printf(" scale factors %.4f %.4f %.4f %.4f\n", g1, g2, g3, g4);
    printf("Boundary diffs before: %6.1f %6.1f %6.1f %6.1f\n", d[0] - d[6], d[3] - d[1],
           d[4] - d[2], d[7] - d[5]);
    printf("Boundary diffs after: %6.1f %6.1f %6.1f %6.1f\n", 
           g1 * (d[0]+base) - g4 * (d[6]+base), g2 * (d[3]+base) - g1 * (d[1]+base),
           g3 * (d[4]+base) - g2 * (d[2]+base), g4 * (d[7]+base) - g3 * (d[5]+base));

    indEnd = iz;
    if (group == numGroups - 1)
      indEnd = nz - 1;

    /* Loop on all slices in group range and correct or just write */
    for (iz = lastOut + 1; iz <= indEnd; iz++) {
      slice = sliceReadMRC(hin, iz, 'z');
      if (!slice){
        show_error("clip: Error reading slice.");
        return(-1);
      }

      /* Convert if needed */
      if (newMode >= 0 && sliceNewMode(slice, newMode) < 0) {
        show_error("clip: Error converting slice to new mode.");
        return(-1);
      }

      /* Correct if section is in the list */
      for (ind = indStart; ind < indStart + ninGroup; ind++) {
        if (iz == opt->secs[ind]) {
          correctQuadrant(slice, nx / 2, ny / 2, nx, ny, g1, base);
          correctQuadrant(slice, 0, ny / 2, nx / 2, ny, g2, base);
          correctQuadrant(slice, 0, 0, nx / 2, ny / 2, g3, base);
          correctQuadrant(slice, nx / 2, 0, nx, ny / 2, g4, base);
          break;
        }
      }

      /* Maintain MMM and write */
      sliceMMM(slice);
      hout->amin = B3DMIN(hout->amin, slice->min);
      hout->amax = B3DMAX(hout->amax, slice->max);
      hout->amean += slice->mean / nz;
      if (mrc_write_slice((void *)slice->data.b, hout->fp, hout, iz, 'z')) {
        show_error("clip: Error writing slice.");
        return(-1);
      }
      sliceFree(slice);
      lastOut = iz;
    }
    indStart += ninGroup; 
  }
  mrc_head_label(hout, "clip: quadrant correction");
  if (mrc_head_write(hout->fp, hout))
    return -1;
  return 0;
}

static int quadrantSample(Islice *slice, int llx, int lly, int urx, int ury, double *mean)
{
  Islice *box = sliceBox(slice, llx, lly, urx, ury);
  if (!box) {
    show_error("clip: Error extracting subslice.");
    return(-1);
  }
  sliceMMM(box);
  *mean = box->mean;
  sliceFree(box);
  return 0;
}

static void correctQuadrant(Islice *slice, int llx, int lly, int urx, int ury, double g,
                             double base)
{
  Ival val;
  int ix, iy;
  for (iy = lly; iy < ury; iy++) {
    if (slice->mode == SLICE_MODE_FLOAT) {
      for (ix = llx; ix < urx; ix++) {
        sliceGetVal(slice, ix, iy, val);
        val[0] = (float)(g * (val[0] + base) - base);
        slicePutVal(slice, ix, iy, val);
      }
    } else {
      for (ix = llx; ix < urx; ix++) {
        sliceGetVal(slice, ix, iy, val);
        val[0] = (float)floor(g * (val[0] + base) - base + 0.5);
        slicePutVal(slice, ix, iy, val);
      }
    }
  }
}

/*
 * 3-D color - conversion to shades of one color
 */
int clip_color(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  int xysize, k, i;
  unsigned char **idata;
  float pixin;

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
      writeBytePixel((float)(pixin * opt->red), hout);
      writeBytePixel((float)(pixin * opt->green), hout);
      writeBytePixel((float)(pixin * opt->blue), hout);
    }
  }
  return(0);
}

static void writeBytePixel(float pixel, MrcHeader *hout)
{
  unsigned char bdata;
  if (pixel > 255.0)
    pixel = 255.0;
  bdata = pixel + 0.5;
  if (hout->bytesSigned)
    bdata = (unsigned char)(((int)bdata - 128) & 255);
  b3dFwrite(&bdata,  sizeof(unsigned char), 1, hout->fp);
}

/*
 * 2D conversion to shades of a color
 */
int clip2d_color(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  Islice *slice;
  int k, z, i, j;
  float pixin;
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
  int i,j,k,l, zstart;
  float xs, ys, zs;
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
    printf("ERROR: clip joinrgb - three input files must be specified\n");
    return (-1);
  }
  hdr[0] = *h1;
  hdr[1] = *h2;
  hdr[2].fp = fopen(opt->fnames[2], "rb");

  if (!hdr[2].fp){
      printf("ERROR: clip joinrgb - opening %s.\n", opt->fnames[2]);
      return(-1);
  }
  if (mrc_head_read(hdr[2].fp, &hdr[2])){
    printf("ERROR: clip joinrgb - reading %s.\n", opt->fnames[2]);
    return(-1);
  }
  
  if ( (h1->nx != hdr[2].nx) || (h1->ny != hdr[2].ny) ||
       (h1->nz != hdr[2].nz) || (h1->nx != h2->nx) || (h1->ny != h2->ny) ||
       (h1->nz != h2->nz)) {
    printf("ERROR: clip joinrgb - all files must be same size.\n");
    return(-1);
  }

  if (h1->mode != MRC_MODE_BYTE || h2->mode != MRC_MODE_BYTE || 
      hdr[2].mode != MRC_MODE_BYTE) {
    printf("ERROR: clip joinrgb - all files must be bytes.\n");
    return(-1);
  }

  zstart = 0;
  if (opt->add2file) {
    if (opt->add2file == IP_APPEND_OVERWRITE) {
      printf("ERROR: clip joinrgb - Overwriting is not allowed, only appending\n");
      return(-1);
    }
    if (hout->mode != MRC_MODE_RGB) {
      printf("ERROR: clip joinrgb - Mode of file being appended to must be 16\n");
      return(-1);
    }
    if (hout->nx != h1->nx || hout->ny != h1->ny) {
      printf("ERROR: clip joinrgb - File being appended to is not same X/Y size as input"
             " files\n");
      return(-1);
    }

    /* Get starting Z value and maintain the scale */
    mrc_get_scale(hout, &xs, &ys, &zs);
    zstart = hout->nz;
    if (hout->mz == hout->nz)
      hout->mz += h1->nz;
    hout->nz += h1->nz;
    mrc_set_scale(hout, xs, ys, zs);

  } else {

    /* Set up mode and label */
    hout->mode = MRC_MODE_RGB;
    mrc_head_label(hout, "CLIP Join 3 files into RGB");
  }
  hout->amin = 0;
  hout->amax = 255;
  hout->amean = 128;
  if (mrc_head_write(hout->fp, hout))
    return -1;

  s = sliceCreate(h1->nx, h1->ny, MRC_MODE_RGB);
  for (i = 0; i < 3; i++) {
    srgb[i] = sliceCreate(h1->nx, h1->ny, MRC_MODE_BYTE);
    if (!s || !srgb[i]) {
      printf("ERROR: CLIP - getting memory for slices\n");
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
  
    if (mrc_write_slice((void *)s->data.b, hout->fp, hout, k + zstart, 'z'))
      return -1;
  }
  printf("\n");

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
    printf("ERROR: clip splitrgb - mode is not RGB\n");
    return(-1);
  }
   
  if (opt->infiles != 1) {
    printf("ERROR: clip splitrgb - only one input file should "
            "be specified\n");
    return (-1);
  }
 
  opt->ocanresize = FALSE;
  set_input_options(opt, h1);

  fname = (char *)malloc(len + 4);
  if (!fname) {
    printf("ERROR: clip - getting memory for filename\n");
    return (-1);
  }

  /* Set up output files and get slices */
  s = sliceCreate(h1->nx, h1->ny, MRC_MODE_RGB);
  for (i = 0; i < 3; i++) {
    sprintf(fname, "%s%s", opt->fnames[1], ext[i]);
    if (!getenv("IMOD_NO_IMAGE_BACKUP"))
      imodBackupFile(fname);
    hdr[i] = *h1;
    hdr[i].nz = hdr[i].mz = opt->nofsecs;
    mrc_coord_cp(&hdr[i], h1);
    if (hdr[i].mz)
      hdr[i].zorg -= opt->secs[0] * hdr[i].zlen / hdr[i].mz;
      
    hdr[i].fp = fopen(fname, "wb+");
    if (!hdr[i].fp){
      printf("ERROR: clip - opening %s\n", fname);
      return(-1);
    }
    hdr[i].mode = 0;
    mrcInitOutputHeader(&hdr[i]);
    hdr[i].amin = 255;
    hdr[i].amean = 0;
    hdr[i].amax = 0;
    mrc_head_label(&hdr[i], "CLIP Split RGB into 3 files");
    if (mrc_head_write(hdr[i].fp, &hdr[i]))
      return -1;
    srgb[i] = sliceCreate(h1->nx, h1->ny, MRC_MODE_BYTE);
    if (!s || !srgb[i]) {
      printf("ERROR: clip - getting memory for slices\n");
      return (-1);
    }
  }

  /* Loop on sections */
  for (k = 0; k < opt->nofsecs; k++) {
    printf("\rSplitting section %d of %d", k + 1, opt->nofsecs);
    fflush(stdout);
	  
    /* Read slice and put its 3 components into output slices */
    if (mrc_read_slice((void *)s->data.b, h1->fp, h1, opt->secs[k], 'z'))
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

  printf("\n");

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
 * 3D additive operations on multiple files
 */
int clip_average(MrcHeader *h1, MrcHeader *h2, MrcHeader *hout, ClipOptions *opt)
{
  Islice *s, *so, *sso;
  MrcHeader **hdr;
  char *message;
  int i, j, k, z, l, f, variance = 0;
  double valscale = 1., varscale = 1.;
  float factor = 1.;
  Ival val, oval;

  if (opt->infiles == 1 && (opt->process == IP_AVERAGE || opt->process == IP_VARIANCE || 
                            opt->process == IP_STANDEV))
    return(clip2d_average(h1,hout,opt));

  if (opt->add2file != IP_APPEND_FALSE) {
    fprintf(stderr, "clip volume combining: you cannot add to an existing output file");
    return -1;
  }

  if (opt->process == IP_SUBTRACT && opt->infiles != 2 || opt->infiles < 2) {
    fprintf(stderr, "clip %s two input files.\n", opt->process == IP_SUBTRACT ?
            "subtract: needs exactly" : "add: needs at least");
    return(-1);
  }
  z = set_options(opt, h1, hout);
  if (z < 0)
    return(z);

  if (opt->low != IP_DEFAULT) {
    valscale = opt->low;
    varscale = opt->low * opt->low;
  }
  
  switch (opt->process) {
  case IP_AVERAGE:
    valscale /= opt->infiles;
    message = "Averaging";
    mrc_head_label(hout, "clip: 3D Averaged");
    break;    
  case IP_ADD:
    message = "Adding";
    mrc_head_label(hout, "clip: Summed");
    break;
  case IP_VARIANCE:
    variance = 1;
    valscale /= opt->infiles;
    message = "Averaging";
    mrc_head_label(hout, "clip: 3D Variance");
    break;
  case IP_STANDEV:
    variance = 2;
    valscale /= opt->infiles;
    message = "Averaging";
    mrc_head_label(hout, "clip: 3D Standard Deviation");
    break;
  case IP_SUBTRACT:
    message = "Subtracting";
    valscale = 1.;
    mrc_head_label(hout, "clip: Subtract");
    break;
  default:
    return -1;
  }

  if (variance) {
    if (h1->mode == MRC_MODE_COMPLEX_FLOAT) {
      sso = sliceCreate(opt->ix, opt->iy, SLICE_MODE_COMPLEX_FLOAT);
    } else if (h1->mode == MRC_MODE_RGB){
      sso = sliceCreate(opt->ix, opt->iy, SLICE_MODE_MAX);
    } else {
      sso = sliceCreate(opt->ix, opt->iy, SLICE_MODE_FLOAT);
    }
  }
  hdr = (MrcHeader **)malloc(opt->infiles * sizeof(MrcHeader *));
  if (!hdr || (variance && !sso))
    return(-1);
     
  for (f = 0; f < opt->infiles; f++){
    hdr[f] = (MrcHeader *)malloc(sizeof(MrcHeader));
    if (!hdr[f])
      return(-1);
    hdr[f]->fp = fopen(opt->fnames[f], "rb");
    if (!hdr[f]->fp){
      fprintf(stderr, "clip volume combining: error opening %s.\n", opt->fnames[f]);
      return(-1);
    }
    if (mrc_head_read(hdr[f]->fp, hdr[f])){
      fprintf(stderr, "clip volume combining: error reading header of %s.\n", 
              opt->fnames[f]);
      return(-1);
    }
    if ( (h1->nx != hdr[f]->nx) || (h1->ny != hdr[f]->ny) ||
         (h1->nz != hdr[f]->nz) || (h1->mode != hdr[f]->mode )) {
      fprintf(stderr, "clip volume combining: all files must be the same size and "
              "mode.\n");
      return(-1);
    }
  }

  for (k = 0; k < opt->nofsecs; k++) {
    printf("\rclip: %s slice %d of %d\n", message, k + 1, opt->nofsecs);
    fflush(stdout);

    if (h1->mode == MRC_MODE_COMPLEX_FLOAT){
      so = sliceCreate(opt->ix, opt->iy, SLICE_MODE_COMPLEX_FLOAT);
    } else if (h1->mode == MRC_MODE_RGB){
      so = sliceCreate(opt->ix, opt->iy, SLICE_MODE_MAX);
    } else {
      so = sliceCreate(opt->ix, opt->iy, SLICE_MODE_FLOAT);
    }
    if (!so)
      return -1;

    /* Zero the slice(s) */
    factor = 1.;
    val[0] = val[1] = val[2] = 0.;
    for (j = 0; j < opt->iy; j++)
      for(i = 0; i < opt->ix; i ++){
        slicePutVal(so, i, j, val);
        if (variance)
          slicePutVal(sso, i, j, val);
      }

    /* Accumulate sums and sum of squares */
    for (f = 0; f < opt->infiles; f++){
      s = sliceReadSubm( hdr[f], opt->secs[k], 'z', opt->ix, opt->iy,
                        (int)opt->cx, (int)opt->cy);
      if (!s)
        return -1;
      for (j = 0; j < opt->iy; j++)
        for(i = 0; i < opt->ix; i ++){
          sliceGetVal(s, i, j,   val);
          sliceGetVal(so, i, j, oval);
          oval[0] += factor * val[0];
          oval[1] += factor * val[1];
          oval[2] += factor * val[2];
          slicePutVal(so, i, j, oval);
          if (variance) {
            sliceGetVal(sso, i, j, oval);
            oval[0] += val[0] * val[0];
            oval[1] += val[1] * val[1];
            oval[1] += val[2] * val[2];
            slicePutVal(sso, i, j, oval);
          }
        }
      sliceFree(s);
      if (opt->process == IP_SUBTRACT)
        factor = -1.;
    }

    /* Compute mean and variance or SD */
    if (valscale != 1.)
      mrc_slice_valscale(so, valscale);
    if (variance) {
      for (j = 0; j < opt->iy; j++)
        for(i = 0; i < opt->ix; i ++){
          sliceGetVal(sso, i, j,   val);
          sliceGetVal(so, i, j, oval);
          for (l = 0; l < 3; l++) {
            oval[l] = (val[l] * varscale - f * oval[l] * oval[l]) / (f - 1.);
            oval[l] = B3DMAX(0., oval[l]);
            if (variance > 1)
              oval[l] = (float)sqrt(oval[l]);
          }
          slicePutVal(so, i, j, oval);
        }
    }

    if (clipWriteSlice(so, hout, opt, k, &z, 1))
      return -1;
  }
  printf("\n");
  hout->amean /= k;
  if (mrc_head_write(hout->fp, hout))
    return -1;
  if (variance)
    sliceFree(sso);
  for (f = 0; f < opt->infiles; f++)
    free(hdr[f]);
  free(hdr);
  return set_mrc_coords(opt);
}

/*
 * 2D averaging of input slices into one slice 
 */
int clip2d_average(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  Islice *slice;
  Islice *avgs, *cnts, *ssq;
  Ival val, aval;
  int k, z, i, j, l, variance = 0;
  int thresh = FALSE;
  float dval,scale,dscale, sx, sy, sz;
     
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
  switch (opt->process) {
  case IP_AVERAGE:
    mrc_head_label(hout, "clip: 2D Average");
    break;    
    break;
  case IP_VARIANCE:
    variance = 1;
    mrc_head_label(hout, "clip: 2D Variance");
    break;
  case IP_STANDEV:
    variance = 2;
    mrc_head_label(hout, "clip: 2D Standard Deviation");
    break;
  default:
    return -1;
  }
  if (opt->val != IP_DEFAULT)
    thresh = TRUE;

  show_status("2D Averaging...\n");

  avgs = sliceCreate(opt->ix, opt->iy, SLICE_MODE_MAX);
  cnts = sliceCreate(opt->ix, opt->iy, SLICE_MODE_FLOAT);
  if (variance)
    ssq = sliceCreate(opt->ix, opt->iy, SLICE_MODE_MAX);

  /* Initialize sums */
  aval[0] = aval[1] = aval[2] = 0.0f;
  for(j = 0; j < avgs->ysize; j++)
    for(i = 0; i < avgs->xsize; i++){
      slicePutVal(avgs, i, j, aval);
      cnts->data.f[i + (j * cnts->xsize)] = 0.0f;
      if (variance)
        slicePutVal(ssq, i, j, aval);
    }

  for (k = 0; k < opt->nofsecs; k++) {
    slice = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
                          (int)opt->cx, (int)opt->cy);
    if (!slice){
      show_error("clip: Error reading slice.");
      return(-1);
    }

    /* Add each pixel into average */
    for (j = 0; j < slice->ysize; j++) {
      for (i = 0; i < slice->xsize; i++) {

        /* If thresholding, only add to sums if magnitude is greater than threshold. */
        if (thresh) {
          dval = sliceGetPixelMagnitude(slice, i, j);
          if (dval <= opt->val)
            continue;
        }
        sliceGetVal(slice, i, j,  val);
        sliceGetVal(avgs,  i, j, aval);
        aval[0] += val[0];
        aval[1] += val[1];
        aval[2] += val[2];

        /* Add up the number of items contributing to the average */
        cnts->data.f[i + (j * cnts->xsize)] += 1.0f;
        slicePutVal(avgs,  i, j, aval);
        if (variance) {
          sliceGetVal(ssq,  i, j, aval);
          aval[0] += val[0] * val[0];
          aval[1] += val[1] * val[1];
          aval[2] += val[2] * val[2];
          slicePutVal(ssq,  i, j, aval);
        }
      }
    }
    sliceFree(slice);
  }

  /* Set scaling based on -l input (no longer special for RGB) */
  scale = 1.0;
  if (opt->low != IP_DEFAULT)
    scale = opt->low;

  /* create the output slice by dividing each value by counts */
  for (j = 0; j < avgs->ysize; j++) {
    for (i = 0; i < avgs->xsize; i++) {
      sliceGetVal(avgs, i, j, aval);
      dval = cnts->data.f[i + (j * cnts->xsize)];
      if (dval){
        dscale = scale/dval;
        aval[0] *= dscale;
        aval[1] *= dscale;
        aval[2] *= dscale;
        if (variance) {
          if (dval > 1.5) {
            sliceGetVal(ssq, i, j, val);
            for (l = 0; l < 3; l++) {
              aval[l] = (val[l] * scale * scale - dval * aval[l] * aval[l]) / (dval-1.);
              aval[l] = B3DMAX(0., aval[l]);
              if (variance > 1)
                aval[l] = (float)sqrt(aval[l]);
            }
          } else {
            aval[0] = aval[1] = aval[2] = 0.;
          }
        }
      } else {
        aval[0] = aval[1] = aval[2] = 0.;
      }
      slicePutVal(avgs, i, j, aval);
    }
  }

  /* Take care of min/max/mean and write the slice */
  if (avgs->mode != hout->mode && sliceNewMode(avgs, hout->mode) < 0)
    return -1;
  sliceMMM(avgs);
  hout->amin = B3DMIN(avgs->min, hout->amin);
  hout->amax = B3DMAX(avgs->max, hout->amax);
  if (opt->add2file != IP_APPEND_OVERWRITE)
    hout->amean += avgs->mean / hout->nz;
  mrc_get_scale(hin, &sx, &sy, &sz);
  mrc_set_scale(hout, sx, sy, sz);
  if (mrc_write_slice((void *)avgs->data.b, hout->fp, hout, z, 'z'))
    return -1;
  if (mrc_head_write(hout->fp, hout))
    return -1;
  sliceFree(avgs);
  sliceFree(cnts);
  if (variance)
    sliceFree(ssq);
  return(0);
}

/*
 * 3D multiplying or dividing
 */
int clip_multdiv(MrcHeader *h1, MrcHeader *h2, MrcHeader *hout,
                 ClipOptions *opt)
{
  Islice *s, *so;
  char *message;
  int i, j, k, z, dsize, csize1, csize2, divByZero = 0, readOnce;
  Ival val, oval;
  float tmp, denom;

  if (opt->infiles != 2) {
    fprintf(stderr, "clip multiply/divide: Need exactly two input files.\n");
    return(-1);
  }

  z = set_options(opt, h1, hout);
  if (z < 0)
    return(z);

  if (opt->add2file != IP_APPEND_FALSE) {
    fprintf(stderr, "clip multiply/divide: you cannot add to an existing output file");
    return -1;
  }

  mrc_getdcsize(h1->mode, &dsize, &csize1);
  mrc_getdcsize(h2->mode, &dsize, &csize2);
  if (!(csize2 == 1 || (h1->mode == MRC_MODE_COMPLEX_FLOAT && 
                        h2->mode == MRC_MODE_COMPLEX_FLOAT) || 
        hout->mode == MRC_MODE_FLOAT)) {
    fprintf(stderr, "clip multiply/divide: second file must have single-channel data "
            "unless both are FFTs or output mode is float\n");
    return(-1);
  }

  
  if ((h1->nx != h2->nx) || (h1->ny != h2->ny)) {
    fprintf(stderr, "clip  multiply/divide: X and Y sizes must be equal\n");
    return(-1);
  }
  if (h1->nz != h2->nz && h2->nz > 1) {
    fprintf(stderr, "clip  multiply/divide: Z sizes must be the same, or equal to 1 "
            "for second file\n");
    return(-1);
  }

  switch (opt->process) {
  case IP_MULTIPLY:
    message = "Multiplying";
    mrc_head_label(hout, "clip: Multiply");
    break;
  case IP_DIVIDE:
    message = "Dividing";
    mrc_head_label(hout, "clip: Divide");
    break;
  default:
    return -1;
  }

  readOnce = (h2->nz == 1 && h1->nz > 1) ? 1 : 0;
  for (k = 0; k < opt->nofsecs; k++) {
    printf("\rclip: %s slice %d of %d", message, k + 1, opt->nofsecs);
    fflush(stdout);
    so = sliceReadSubm(h1, opt->secs[k], 'z', opt->ix, opt->iy, (int)opt->cx,
                       (int)opt->cy);
    if (!so)
      return -1;
    if (hout->mode != so->mode && sliceNewMode(so, hout->mode) < 0) {
      printf("ERROR: CLIP - getting memory for slice array\n");
      return -1;
    }

    if (readOnce >= 0) {
      s = sliceReadSubm(h2, readOnce ? 0 : opt->secs[k], 'z', opt->ix, opt->iy, 
                        (int)opt->cx, (int)opt->cy);
      if (!s)
        return -1;
      readOnce = -readOnce;
      if (csize2 > 1 && hout->mode == MRC_MODE_FLOAT && sliceFloat(s)) {
        printf("ERROR: CLIP - getting memory for slice array\n");
        return -1;
      }
    }
    for (j = 0; j < opt->iy; j++) {
      for (i = 0; i < opt->ix; i ++) {
        sliceGetVal(s, i, j,   val);
        sliceGetVal(so, i, j, oval);
        if (csize2 == 1 || hout->mode == MRC_MODE_FLOAT) {

          /* Ordinary mult or div, possible multi-channel */
          if (opt->process == IP_MULTIPLY) {
            oval[0] *= val[0];
            oval[1] *= val[0];
            oval[2] *= val[0];
          } else {
            if (val[0] != 0) {
              oval[0] /= val[0];
              oval[1] /= val[0];
              oval[2] /= val[0];
            } else {
              divByZero++;
              oval[0] = oval[1] = oval[2] = 0.;
            }
          }
        } else {
         
          /* Complex mult/div */
          if (opt->process == IP_MULTIPLY) {
            tmp = oval[0] * val[0] - oval[1] * val[1];
            oval[1] = oval[0] * val[1] + val[0] * oval[1];
            oval[0] = tmp;
          } else {
            denom = val[0] * val[0] + val[1] * val[1];
            if (denom != 0) {
              tmp = (oval[0] * val[0] + oval[1] * val[1]) / denom;
              oval[1] = (oval[1] * val[0] - oval[0] * val[1]) / denom;
              oval[0] = tmp;
            } else {
              divByZero++;
              oval[0] = oval[1] = oval[2] = 0.;
            }
          }
        }
        slicePutVal(so, i, j, oval);
      }
    }
    
    if (clipWriteSlice(so, hout, opt, k, &z, 1))
      return -1;

    // Free slice only on last slice if reading once
    if (readOnce == 0 || k == opt->nofsecs - 1)
      sliceFree(s);
  }
  printf("\n");
  if (divByZero)
    printf("WARNING: Division by zero occurred %d times\n", divByZero);
  hout->amean /= k;
  if (mrc_head_write(hout->fp, hout))
    return -1;
  return set_mrc_coords(opt);
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

struct zstats {
  float x, y;
  double mean, std;
  int xmin, ymin, outlier;
};

int clip_stat(MrcHeader *hin, ClipOptions *opt)
{
  int i, j, k, iz, di, dj, nsum, length, kk, outlast;
  int xmax, ymax, xmin, ymin, zmin, zmax, izo, xmaxo, ymaxo;
  int minxpiece, xoverlap, numXpieces, minypiece, yoverlap, numYpieces;
  Islice *slice;
  float min, max, m, ptnum;
  double mean, std, sumsq, vmean, vsumsq, cx, cy, data[3][3];
  float x,y;
  float vmin, vmax, kcrit = 2.24f;
  char starmin, starmax;
  b3dInt16 *sdata;
  IloadInfo li;
  int viewAdd = opt->fromOne ? 1 : 0;
  struct zstats *stats;
  float *allmins, *allmaxes, *ifdrop;
  int outliers = (opt->val != IP_DEFAULT || opt->low != IP_DEFAULT) ? 1 : 0;

  /* Try to get a piece list from file and adjust it for overlaps */
  mrc_init_li(&li, NULL);
  if (opt->plname && mrc_plist_li(&li, hin, opt->plname)) {
    show_error("stat: error reading piece list file");
    return -1;
  }
  if (li.plist) {
    if (li.plist < hin->nz) {
      show_error("stat: not enough piece coordinates in file");
      return -1;
    }
    if (opt->newXoverlap == IP_DEFAULT)
      opt->newXoverlap = 0;
    if (opt->newYoverlap == IP_DEFAULT)
      opt->newYoverlap = 0;
    if (checkPieceList(li.pcoords, 3, li.plist, 1, hin->nx, &minxpiece, &numXpieces,
                       &xoverlap) || 
        checkPieceList(li.pcoords + 1, 3, li.plist, 1, hin->ny, &minypiece, &numYpieces,
                       &yoverlap)) {
      show_error("stat: piece coordinates are not regularly spaced");
      return -1;
    }
    if (numXpieces > 1)
      adjustPieceOverlap(li.pcoords, 3, li.plist, hin->nx, minxpiece, xoverlap, 
                         opt->newXoverlap);
    if (numYpieces > 1)
      adjustPieceOverlap(li.pcoords + 1, 3, li.plist, hin->ny, minypiece, yoverlap,
                         opt->newYoverlap);
    printf("piece|   min   |(   x,  %s)|    max  |(   x,  %s)|   mean\n",
           opt->fromOne ? "y, view" : " y,   z", opt->fromOne ? "y, view" : " y,   z");
    printf("-----|---------|----------------|---------|----------------|---------\n");
  } else {

    printf("%s|   min   |(   x,   y)|    max  |(      x,      y)|   mean    |  "
           "std dev.\n", opt->fromOne ? "view " : "slice");
    printf("-----|---------|-----------|---------|-----------------|-----------|--"
           "--------\n");
  }
     
  set_input_options(opt, hin);
  stats = (struct zstats *)malloc(opt->nofsecs * sizeof(struct zstats));
  allmins = (float *)malloc(opt->nofsecs * sizeof(float));
  allmaxes = (float *)malloc(opt->nofsecs * sizeof(float));
  ifdrop = (float *)malloc(opt->nofsecs * sizeof(float));
  if (!stats || !allmins || !allmaxes || !ifdrop) {
    show_error("stat: error allocating array for statistics.");
    return(-1);
  }
  if (outliers) {
    length = opt->nofsecs;
    if (opt->low != IP_DEFAULT)
      length = B3DNINT(opt->low);
    if (opt->val != IP_DEFAULT)
      kcrit = opt->val;
    length = B3DMIN(opt->nofsecs, B3DMAX(5, length));
    outlast = -1;
  }

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
    } else {

      /* Adjust coordinates for subarea */
      x += (int)opt->cx - opt->ix / 2;
      y += (int)opt->cy - opt->iy / 2;
      xmin += (int)opt->cx - opt->ix / 2;
      ymin += (int)opt->cy - opt->iy / 2;
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

    allmins[k] = min;
    stats[k].xmin = xmin;
    stats[k].ymin = ymin;
    allmaxes[k] = max;
    stats[k].x = x;
    stats[k].y = y;
    stats[k].mean = mean;
    stats[k].std = std;
    if (!outliers) {
      if (li.plist) {
        xmin += li.pcoords[3*iz] + viewAdd;
        ymin += li.pcoords[3*iz+1] + viewAdd;
        xmaxo = B3DNINT(x) + li.pcoords[3*iz] + viewAdd;
        ymaxo = B3DNINT(y) + li.pcoords[3*iz+1] + viewAdd;
        printf("%4d  %9.4f (%4d,%4d,%4d) %9.4f (%4d,%4d,%4d) %9.4f\n", iz + viewAdd, min,
               xmin, ymin, li.pcoords[3*iz+2] + viewAdd, max, xmaxo, ymaxo,
               li.pcoords[3*iz+2] + viewAdd, mean);
      } else
        printf("%4d  %9.4f (%4d,%4d) %9.4f (%7.2f,%7.2f) %9.4f  %9.4f\n", iz + viewAdd,
               min, xmin + viewAdd, ymin + viewAdd, max, x, y, mean, std);
    }
    else if (k >= length - 1) {
      for (kk = outlast + 1; kk <= k; kk++) {
        starmin = ' ';
        starmax = ' ';
        di = B3DMAX(0, kk - length / 2);
        dj = B3DMIN(opt->nofsecs, di + length);
        di = B3DMAX(0, dj - length);
        if (dj > k + 1)
          break;
        //printf("kk %d  di %d  dj %d  kcrit %f\n", kk, di, dj, kcrit);
        rsMadMedianOutliers(&allmins[di], dj-di, kcrit, &ifdrop[di]);
        stats[kk].outlier = 0;
        if (ifdrop[kk] < 0.) {
          starmin = '*';
          stats[kk].outlier = 1;
        }
        rsMadMedianOutliers(&allmaxes[di], dj-di, kcrit, &ifdrop[di]);
        if (ifdrop[kk] > 0.) {
          starmax = '*';
          stats[kk].outlier = 1;
        }

      if (li.plist) {
        xmin = stats[kk].xmin + li.pcoords[3*kk] + viewAdd;
        ymin = stats[kk].ymin + li.pcoords[3*kk+1] + viewAdd;
        xmaxo = B3DNINT(stats[kk].x) + li.pcoords[3*kk] + viewAdd;
        ymaxo = B3DNINT(stats[kk].y) + li.pcoords[3*kk+1] + viewAdd;
        izo = li.pcoords[3*opt->secs[kk] + 2] + viewAdd;
        printf("%4d  %9.4f%c(%4d,%4d,%4d) %9.4f%c(%4d,%4d,%4d) %9.4f  %9.4f\n", 
               opt->secs[kk]+ viewAdd, allmins[kk], starmin, xmin, ymin, izo, 
               allmaxes[kk], starmax, xmaxo, ymaxo, izo, stats[kk].mean, stats[kk].std);
      } else
        printf("%4d  %9.4f%c(%4d,%4d) %9.4f%c(%7d,%7d) %9.4f  %9.4f\n", 
               opt->secs[kk]+ viewAdd, allmins[kk], starmin,
               stats[kk].xmin, stats[kk].ymin, allmaxes[kk], starmax,
               stats[kk].x, stats[kk].y, stats[kk].mean, stats[kk].std);
        outlast = kk;
      }
      
    }
    sliceFree(slice);
  }

  vmean /= (float)opt->nofsecs;
  ptnum *= opt->nofsecs;
  std = (vsumsq - ptnum * vmean * vmean) / B3DMAX(1.,(ptnum - 1.));
  std = sqrt(B3DMAX(0., std));

  if (li.plist)
    printf(" all  %9.4f (@ piece =%5d) %9.4f (@ piece =%5d) %9.4f  %9.4f\n",
           vmin, zmin + 1, vmax, zmax+1, vmean, std);
  else
    printf(" all  %9.4f (@ z=%5d) %9.4f (@ z=%5d      ) %9.4f  %9.4f\n",
           vmin, zmin + viewAdd, vmax, zmax + viewAdd, vmean, std);

  if (outliers) {
    printf("\n%s with %sextreme values:", 
           li.plist ? "Pieces" : (opt->fromOne ? "Views" : "Slices"),
           opt->low != IP_DEFAULT ? "locally " : "");
    nsum = 0;
    length = 28 + (opt->low != IP_DEFAULT ? 8 : 0);
    for (k = 0; k < opt->nofsecs; k++) {
      if (stats[k].outlier) {
        printf(" %3d", opt->secs[k] + viewAdd);
        length += 4;
        if (length > 74) {
          printf("\n");
          length = 0;
        }
        nsum++;
      }
    }
    if (!nsum)
      printf(" None");
    printf("\n");
  }
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
