/*
 *  clip_io.c -- Disk input/output for clip.
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
#include <math.h>
#include "mrcc.h"
#include "b3dutil.h"
#include "clip.h"

/* NOTE:
 * There are two sets of routines here.  The first set is for processing
 * a volume slice by slice, which can be done with either -2d or -3d modes
 * for interpreting the iz input.  This includes set*_options and
 * clipWriteSlice.  The second set reads in the whole volume after interpreting
 * options, and writes out the whole volume based on output options.
 */

static Islice *clipBlankSlice(MrcHeader *hout, ClipOptions *opt);

/*
 * Copy MRC coordinats from input to output header, adjust header for
 * for input and output size and location, and write header
 */
/* DNM 1/5/05: changed to return result of writing header */
int set_mrc_coords(ClipOptions *opt)
{
  float tx, ty, tz, sx, sy, sz;
  MrcHeader *hin = opt->hin;
  MrcHeader *hout = opt->hout;
  int ixScale = (opt->process == IP_UNPACK) ? 2 : 1;

  mrc_coord_cp(hout, hin);
  mrc_get_scale(hout, &sx, &sy, &sz);
  
  /* 1/30/07: In response to a request to preserve the n[xyz]start */
  hout->nxstart = hin->nxstart;
  hout->nystart = hin->nystart;
  hout->nzstart = hin->nzstart;
     
  /* DNM 9/21/01: subtract rather than add tx, ty, tz, because of weird
     definition of origin 
     4/15/12: Argh!  It needs to multiply by the scale */
  tx = (int)(opt->cx * ixScale) - (opt->ix * ixScale) / 2;
  tx += (opt->ix * ixScale - opt->ox)/2;
  if (sx)
    tx *= sx;
  hout->xorg -= tx;
     
  ty = (int)opt->cy - opt->iy / 2;
  ty += (opt->iy - opt->oy)/2;
  if (sy)
    ty *= sy;
  hout->yorg -= ty;
   
  /* DNM 1/5/05: with -2d, -iz is treated like a range list, not a size */
  if (opt->dim == 3) {
    tz = (int)floor(opt->cz - opt->iz / 2.);
    tz += (opt->iz - opt->oz)/2;
    if (sz)
      tz *= sz;
    hout->zorg -= tz;
  }
     
  return (mrc_head_write(hout->fp, hout));
}

/*
 * Sets up variables for defining the input size and center, and section lists
 * for the 2D processing case, and also sets ox, oy,oz and output mode.
 */
void set_input_options(ClipOptions *opt, MrcHeader *hin)
{
  int i, zst, znd, ozst;

  if (opt->nofsecs == IP_DEFAULT){
    opt->nofsecs = hin->nz;
    opt->secs = (int *)malloc(sizeof(int) * hin->nz);
    for (i = 0; i < hin->nz; i++)
      opt->secs[i] = i;
     
  }
  if (opt->dim == 3){
    if (opt->iz == IP_DEFAULT){
      opt->iz = hin->nz;
    }
    if (opt->cz == IP_DEFAULT){
      opt->cz = hin->nz * 0.5f;
    }

    /* If there is no output entry, make it be same as input; but if there is,
       and it is smaller output, trim input to that size */
    if (opt->oz == IP_DEFAULT)
      opt->oz = opt->iz;
    else if (opt->iz > opt->oz)
      opt->iz = opt->oz;

    /* Set up a section list for the case where something is treated as
       2d without -2d being given */
    if (opt->nofsecs != IP_DEFAULT)
      free(opt->secs);
    zst = (int)floor(opt->cz - opt->iz / 2.);
    znd = B3DMAX(B3DMIN(zst + opt->iz - 1, hin->nz - 1), 0);
    zst = B3DMAX(B3DMIN(zst, hin->nz - 1), 0);
    opt->nofsecs = znd + 1 - zst;
    opt->secs = (int *)malloc(sizeof(int) * opt->nofsecs);
    for (i = zst; i <= znd; i++)
      opt->secs[i - zst] = i;

    /* If requested output is larger than this output set, keep track of 
       blank slices to put out before and after */
    if (opt->oz != IP_DEFAULT && opt->oz > opt->nofsecs) {
      ozst = (int)floor(opt->cz - opt->oz / 2.);
      opt->outBefore = B3DMAX(0, zst - ozst);
      opt->outAfter = opt->oz - opt->nofsecs - opt->outBefore;
      if (opt->outAfter < 0) {
        opt->outBefore += opt->outAfter;
        opt->outAfter = 0;
      }
    }
  } else {

    /* 2D case: use section list size for default output size */
    if (opt->oz == IP_DEFAULT)
      opt->oz = opt->nofsecs;
  }

  /* Convert starting, ending X or Y to ix/cx entries - main routine already
     checked for conflict */
  if (opt->x != IP_DEFAULT) {
    opt->x2 = B3DMAX(opt->x, opt->x2);
    opt->cx = (opt->x + opt->x2 + 1) / 2.;
    opt->ix = opt->x2 + 1 - opt->x;
  }
  if (opt->ix == IP_DEFAULT)
    opt->ix = hin->nx;
  if ((int)opt->cx == IP_DEFAULT)
    opt->cx = hin->nx * 0.5f;
  if (opt->ox == IP_DEFAULT) {
    opt->ox = opt->ix;
    if (opt->process == IP_UNPACK)
      opt->ox *= 2;
  } else {
    if (!opt->ocanresize){
      show_warning("clip - Process can't change output size.");
      opt->ox = opt->ix;
    }
  }

  if (opt->y != IP_DEFAULT) {
    opt->y2 = B3DMAX(opt->y, opt->y2);
    opt->cy = (opt->y + opt->y2 + 1) / 2.;
    opt->iy = opt->y2 + 1 - opt->y;
  }
  if (opt->iy == IP_DEFAULT)
    opt->iy = hin->ny;
  if ((int)opt->cy == IP_DEFAULT)
    opt->cy = hin->ny * 0.5f;
  if (opt->oy == IP_DEFAULT)
    opt->oy = opt->iy;
  else{
    if (!opt->ocanresize){
      show_warning("clip - Process can't change output size.");
      opt->oy = opt->iy;
    }
  }

  if (opt->pad == IP_DEFAULT)
    opt->pad = hin->amean;

  if (opt->mode == IP_DEFAULT)
    opt->mode = hin->mode;
}

/*
 * Sets up output size and file header based on output options
 * Returns first z section to write, or negative for error.
*/
int set_output_options(ClipOptions *opt, MrcHeader *hout)
{
  int z = 0;

  /* create a new file */
  if (!opt->add2file){
    mrc_head_new(hout, opt->ox, opt->oy, opt->oz, opt->mode);
    if (mrc_head_write(hout->fp, hout))
      return -1;
  } else {
    if ((opt->ox != hout->nx) || (opt->oy != hout->ny)){
      opt->ox = hout->nx;
      opt->oy = hout->ny;
      show_warning("clip - Appended file can't change size.");
    }
    if (opt->mode != hout->mode){
      opt->mode = hout->mode;
      show_warning("clip - Appended file can't change mode.");
    }
    if (IP_APPEND_ADD == opt->add2file){

      /* If appending, scale mean down so complete mean can be computed
         by adding slice mean / new nz */
      z = hout->nz;
      hout->nz += opt->oz;
      hout->amean = z * hout->amean / hout->nz;
    } else{
      z = opt->isec;
      hout->nz = opt->oz + opt->isec;
    }
      
    if (mrc_head_write(hout->fp, hout))
      return -1;

    /* DNM 1/15/05: removed the calculation of data size and file seek */
  }
  return(z);
}

/* Call both the input and the output option routines and copy header labels */
int set_options(ClipOptions *opt, MrcHeader *hin, MrcHeader *hout)
{
  int z;
  set_input_options(opt, hin);
  z = set_output_options(opt,hout);
  mrc_head_label_cp(hin, hout);
  return z;
}

/* 
 * Write one slice to output file
 * kSec is the index of the sections being processed, 
 * zWrite is address of z value at which to write, maintained here
 * freeSlice 1 indicates to free the slice when done
 */
int clipWriteSlice(Islice *slice, MrcHeader *hout, ClipOptions *opt, int kSec, 
                   int *zWrite, int freeSlice)
{
  Islice *ps = NULL;
  Islice *s;
  int z;
  int blankBefore = 0;
  int blankAfter = 0;

  /* Ignore if output size is smaller than # being processed */
  if (kSec < (opt->nofsecs - opt->oz) / 2 || 
      kSec >= (opt->nofsecs - opt->oz) / 2 + opt->oz) {
    if (freeSlice)
      sliceFree(slice);
    return 0;
  }

  /* determine if blank slices need to be written before or after this slice */
  if (opt->oz > opt->nofsecs) {
    if (!kSec) {
      if (opt->outBefore == IP_DEFAULT)
        blankBefore = (opt->oz - opt->nofsecs) / 2;
      else
        blankBefore = opt->outBefore;
    }
    if (kSec == opt->nofsecs - 1) {
      if (opt->outAfter == IP_DEFAULT)
        blankAfter = opt->oz - opt->nofsecs - (opt->oz - opt->nofsecs) / 2;
      else
        blankAfter = opt->outAfter;
    }
    if (blankBefore || blankAfter) {
      ps = clipBlankSlice(hout, opt);
      if (!ps)
        return -1;
    }
  }

  /* Write blank slices before */
  for (z = 0; z < blankBefore; z++)
    if (mrc_write_slice((void *)ps->data.b, hout->fp, hout, (*zWrite)++, 'z'))
      return -1;
    
  /* convert slice to output mode if needed */
  if (slice->mode != opt->mode && sliceNewMode(slice, opt->mode) < 0)
    return -1;

  /* Resize slice if necessary */
  s = slice;
  if (opt->ox != slice->xsize || opt->oy != slice->ysize) {
    slice->mean = opt->pad;
    s = mrc_slice_resize(slice, opt->ox, opt->oy);
    if (!s) {
      fprintf(stderr, "clipWriteSlice: error resizing slice.\n");
      return(-1);
    }
  }
   
  /*   maintain min & max, add to mean unless overwriting */
  sliceMMM(s);
  hout->amin = B3DMIN(hout->amin, s->min);
  hout->amax = B3DMAX(hout->amax, s->max);
  if (opt->add2file != IP_APPEND_OVERWRITE)
    hout->amean += (s->mean + (blankBefore + blankAfter) * opt->pad) /
      hout->nz;

  /* Write slice, free slices as needed */
  if (mrc_write_slice((void *)s->data.b, hout->fp, hout, (*zWrite)++, 'z'))
    return -1;
  if (opt->ox != slice->xsize || opt->oy != slice->ysize)
    sliceFree(s);
  if (freeSlice)
    sliceFree(slice);

  /* Write blank slices after */
  for (z = 0; z < blankAfter; z++)
    if (mrc_write_slice((void *)ps->data.b, hout->fp, hout, (*zWrite)++, 'z'))
      return -1;
  if (ps)
    sliceFree(ps);
  return 0;
}

/* Read the required data volume */
Istack *grap_volume_read(MrcHeader *hin, ClipOptions *opt)
{
  Istack *v;
  Islice  *s;
  Ival val;
  int i, j, k, x, y, z;

  if (opt->dim == 2){
    if (opt->iz == IP_DEFAULT) opt->iz = 0;
    if (opt->iz2 == IP_DEFAULT) opt->iz2 = hin->nz - 1;
    if (opt->iz2 == 0) opt->iz2 = opt->iz;
    if (opt->iz2 < opt->iz) opt->iz2 = opt->iz;
    opt->cz = ( opt->iz2 + opt->iz) / 2.;
    opt->iz = opt->iz2 - opt->iz + 1;
  }

  if (opt->ix == IP_DEFAULT) opt->ix = hin->nx;
  if (opt->iy == IP_DEFAULT) opt->iy = hin->ny;
  if (opt->iz == IP_DEFAULT) opt->iz = hin->nz;
  if (opt->cx == IP_DEFAULT) opt->cx = hin->nx / 2.;
  if (opt->cy == IP_DEFAULT) opt->cy = hin->ny / 2.;
  if (opt->cz == IP_DEFAULT) opt->cz = hin->nz / 2.;

  /* Do not set opt->pad yet, just pad with current mean */
  val[0] = opt->pad;
  if (opt->pad == IP_DEFAULT) 
    val[0] = hin->amean;
  val[1] = val[0];
  val[2] = val[0];

  /* Create volume and initialize to pad value. */
  v = (Istack *)malloc(sizeof(Istack));
  v->vol = (Islice **)malloc( opt->iz * sizeof(Islice *));
  v->zsize = opt->iz;

  for (k = 0; k < opt->iz; k++) {
    v->vol[k] = sliceCreate(opt->ix, opt->iy, hin->mode);
    if (!v->vol[k])
      return(NULL);
    for( j = 0; j < opt->iy; j++)
      for(i = 0; i < opt->ix; i++)
        slicePutVal(v->vol[k], i, j, val);
    v->vol[k]->mean = hin->amean;
    v->vol[k]->max  = hin->amax;
    v->vol[k]->min  = hin->amin;
  }

  s = sliceCreate(hin->nx, hin->ny, hin->mode);
  if (!s)
    return(NULL);

  /* Read slices in, copying just the part that is needed */
  k = (int)floor(opt->cz - ((float)opt->iz * 0.5f));
  /*     printf("k = %d\n", k); */
  for (z = 0; (k < hin->nz) && (z < opt->iz); k++, z++) {
    if (k >= 0) {
      if (mrc_read_slice((void *)s->data.b, hin->fp, hin, k, 'z'))
        return (NULL);
      j = (int)floor(opt->cy - opt->iy / 2.);
      for (y = 0; (j < hin->ny) && (y < opt->iy); j++, y++) {
        if (j >= 0) {
          i = (int)floor(opt->cx - opt->ix / 2.);
          x = 0;
          if (i < 0) {
            x = -i;
            i = 0;
          }
          for (; (i < hin->nx) && (x < opt->ix); i++, x++) {
            sliceGetVal(s, i, j, val);
            slicePutVal(v->vol[z], x, y, val);
          }
        }
      }
    }
  }

  sliceFree(s);
  return(v);
}

/* Write the data volume with potential padding */
int grap_volume_write(Istack *v,  MrcHeader *hout, ClipOptions *opt)
{
  FILE *fp = hout->fp;
  int zs, ks, z;
  int k;
  float min, max, mean, zscale;
  Islice  *s, *ps = NULL;
  int labels;

  if (opt->mode == IP_DEFAULT)
    opt->mode = v->vol[0]->mode;
  if (opt->ox == IP_DEFAULT)
    opt->ox = v->vol[0]->xsize;
  if (opt->oy == IP_DEFAULT)
    opt->oy = v->vol[0]->ysize;
  if (opt->oz == IP_DEFAULT)
    opt->oz = v->zsize;

  if (v->vol[0]->mode != opt->mode)
    for(k = 0; k < v->zsize; k++){
      sliceNewMode(v->vol[k], opt->mode);
    }

  /* calculate min, max, mean of volume. */
  sliceMMM(v->vol[0]);
  min = v->vol[0]->min;
  max = v->vol[0]->max;
  mean = v->vol[0]->mean;
  for(k = 1; k < v->zsize; k++){
    sliceMMM(v->vol[k]);
    min = B3DMIN(min, v->vol[k]->min);
    max = B3DMAX(max, v->vol[k]->max);
    mean += v->vol[k]->mean;
  }
  mean /= (float)v->zsize;

  /* DNM 6/26/02: do not make a new header if appending or overwriting! */
  switch(opt->add2file){
  case IP_APPEND_OVERWRITE:
    ks = opt->isec;
    hout->nz = ks + opt->oz;
    opt->ox = hout->nx;
    opt->oy = hout->ny;
    if (hout->mode != v->vol[0]->mode){
      fprintf(stderr,
              "overwriting requires data modes to be the same.\n");
      return(-1);
    }
    /* DNM 6/26/02: fix min and max, also fix mz and zlen */
    /* DNM 9/13/02: oops, had xlen instead of zlen */
    /* todo: recalc mmm.  This is a start but not much */
    hout->amin = B3DMIN(min, hout->amin);
    hout->amax = B3DMAX(max, hout->amax);
    zscale = hout->zlen / hout->mz;
    hout->mz = hout->nz;
    hout->zlen = hout->mz * zscale;
    break;

  case IP_APPEND_ADD:
    ks = hout->nz;
    hout->nz = hout->nz + opt->oz;
    opt->ox = hout->nx;
    opt->oy = hout->ny;
    if (hout->mode != v->vol[0]->mode){
      fprintf(stderr, "inserting requires data modes to be the same.\n");
      return(-1);
    }
    hout->amin = B3DMIN(min, hout->amin);
    hout->amax = B3DMAX(max, hout->amax);
    hout->amean = ((hout->amean * ks) + (mean * opt->oz)) / hout->nz; 
    zscale = hout->zlen / hout->mz;
    hout->mz = hout->nz;
    hout->zlen = hout->mz * zscale;
    break;

  case IP_APPEND_FALSE:

    /* New file: reinitialize header but preserve the labels, set 
       min/max/mean */
    labels = hout->nlabl;
    mrc_head_new(hout, opt->ox, opt->oy, opt->oz, v->vol[0]->mode);
    hout->nlabl = labels;
    hout->amin = min;
    hout->amax = max;
    hout->amean = mean;
    ks = 0;

  }

  if (opt->pad == IP_DEFAULT)
    opt->pad = hout->amean;

  if (mrc_head_write(fp, hout))
    return -1;

  zs = (v->zsize - opt->oz) / 2;
  for (k = ks, z = zs; k < hout->nz; k++, z++) {

    /* printf("%d %d\n", k, z);*/
    /* Write blank slice outside z range */
    if ((z < 0) || (z >= v->zsize)) {
      if (!ps) {
        ps = clipBlankSlice(hout, opt);
        if (!ps)
          return -1;
      }
      if (mrc_write_slice((void *)ps->data.b, fp, hout, k, 'z'))
        return (-1);
    } else {

      /* Resize slice only if necessary */
      s = v->vol[z];
      if (opt->ox != v->vol[0]->xsize || opt->oy != v->vol[0]->ysize) {
        v->vol[z]->mean =  opt->pad;
        s = mrc_slice_resize(v->vol[z], opt->ox, opt->oy);
        if (!s){
          fprintf(stderr, "volume_write: error resizing slice.\n");
          return(-1);
        }
      }
      if (mrc_write_slice((void *)s->data.b, fp, hout, k, 'z'))
        return (-1);
      if (opt->ox != v->vol[0]->xsize || opt->oy != v->vol[0]->ysize)
        sliceFree(s);
    }
  }

  if (ps)
    sliceFree(ps);
  return(0);
}

/* Make a blank slice to write to the output volume, using the pad in opt */
static Islice *clipBlankSlice(MrcHeader *hout, ClipOptions *opt)
{
  Ival val;
  int i, j;
  Islice *ps = sliceCreate(hout->nx, hout->ny, hout->mode);
  if (!ps){
    fprintf(stderr, "clipBlankSlice:  error getting slice\n");
    return NULL;
  }
  val[0] = opt->pad;
  val[1] = opt->pad;
  val[2] = opt->pad;

  for(j = 0; j < hout->ny; j++)
    for(i = 0; i < hout->nx; i++)
      slicePutVal(ps, i, j, val);
  return ps;
}


int grap_volume_free(Istack *v)
{
  int k;

  if (!v)
    return(-1);

  for (k = 0; k < v->zsize; k++)
    sliceFree(v->vol[k]);

  free(v->vol);
  free(v);
  return(0);
}


int mrc_head_print(MrcHeader *data)
{
  float  vd1, vd2, nd1, nd2;
  float xscale, yscale, zscale;
  int    i;
  printf("MRC header info:\n");
  switch(data->mode){
  case MRC_MODE_BYTE :
    printf("mode = Byte\n"); break ;
  case MRC_MODE_SHORT :
    printf("mode = Short\n"); break ;
  case MRC_MODE_USHORT :
    printf("mode = Unsigned Short\n"); break ;
  case MRC_MODE_FLOAT :
    printf("mode = Float\n");  break ;
  case MRC_MODE_COMPLEX_SHORT :
    printf("mode = Complex Short\n"); break ;
  case MRC_MODE_COMPLEX_FLOAT :
    printf("mode = Complex Float\n"); break ;
  case MRC_MODE_RGB:
    printf("mode = rgb byte\n"); break;
  default:
    printf("mode is unknown (%d).\n", data->mode);
    break;
  }
  printf("Image size    =  ( %d, %d, %d)\n",
         data->nx, data->ny, data->nz);
  printf("minimum value = %g\n",data->amin) ;
  printf("maximum value = %g\n",data->amax) ;
  printf("mean value    = %g\n",data->amean) ;
  printf("Start reading image at ( %d, %d, %d).\n",
         data->nxstart, data->nystart, data->nzstart) ;
  printf("Read length   =  ( %d, %d, %d).\n",
         data->mx, data->my, data->mz);

  xscale = yscale = zscale = 1.0f;

  /* DNM 9/13/02: invert expressions and change from nx to mx, etc., and
     say Angstrom, not um*/
  if (data->xlen)
    xscale = data->xlen/(float)data->mx;
  if (data->ylen)
    yscale = data->ylen/(float)data->my;
  if (data->zlen)
    zscale = data->zlen/(float)data->mz;
  printf("Scale         =  ( %g x %g x %g ) Angstrom.\n",
         xscale, yscale, zscale);

  printf("Cell Rotation =  ( %g, %g, %g).\n",
         data->alpha, data->beta, data->gamma) ;
  printf("Columns are   = axis %d\n",data->mapc) ;
  printf("Rows are      = axis %d\n",data->mapr) ;
  printf("Sections are  = axis %d\n",data->maps) ;
  if (data->ispg)
    printf("ispg =\t\t%d\n",data->ispg) ;
  if (data->next)
    printf("extra header = \t%d\n", data->next);
  if (data->idtype){
    printf("idtype =\t%d\n", data->idtype);
    nd1 = data->nd1;
    nd2 = data->nd2;
    vd1 = data->vd1;
    vd2 = data->vd2;
    printf("nd1 =\t\t%g\n", (nd1));
    printf("nd2 =\t\t%g\n", (nd2));
    printf("vd1 =\t\t%g\n", (vd1 / 100.0));
    printf("vd2 =\t\t%g\n", (vd2 / 100.0));
  }
  printf("angles = ( %g, %g, %g, %g, %g, %g)\n",
         data->tiltangles[0],
         data->tiltangles[1],
         data->tiltangles[2],
         data->tiltangles[3],
         data->tiltangles[4],
         data->tiltangles[5]);
  printf("orgin  = ( %g, %g, %g)\n",
         data->xorg, data->yorg, data->zorg);
  printf("creator id = %d\n", data->creatid);
  if (data->nlabl > MRC_NLABELS){
    printf("There are to many labels.\n\n");
    return(0);
  }
  printf("Thare are %d labels.\n\n",data->nlabl) ;
  for (i = 0; i < data->nlabl; i++)
    printf("%s\n",data->labels[i]) ;
  return(0);
}
