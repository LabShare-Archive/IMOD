/*
 *  clip_transform.c -- Image transformation functions for clip.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

/*
$Log$
Revision 3.4  2007/02/04 21:19:48  mast
Eliminated mrcspectral includes

Revision 3.3  2007/02/04 21:10:15  mast
Function name changes from mrcslice cleanup

Revision 3.2  2005/01/17 17:07:40  mast
Converted to new typedefs, looked at code in horror and abandoned it

*/

#include <stdlib.h>
#include "mrcfiles.h"
#include "mrcslice.h"
#include "clip.h"


int grap_rotate(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  Islice *sin, *sout, **vol;
  Istack v;
  int i, j, k, ko;

  if (opt->oz == IP_DEFAULT)
    opt->oz = hin->nz;
  if (opt->ox == IP_DEFAULT)
    opt->ox = hin->nx;
  if (opt->oy == IP_DEFAULT)
    opt->oy = hin->ny;
  if (opt->x == IP_DEFAULT)
    opt->x = 0;
  if (opt->y == IP_DEFAULT)
    opt->y = 0;
  if (opt->z == IP_DEFAULT)
    opt->z = 0;
  printf("clip: rotating, gamma = %g, beta = %g, alpha = %g\n",
         opt->z, opt->y, opt->x);

  mrc_head_new(hout, opt->ox, opt->oy, opt->oz, hin->mode);
  mrc_head_label(hout, "Image rotated.");

  opt->cx = hout->nx * 0.5;
  opt->cy = hout->ny * 0.5;
  opt->cz = hout->nz * 0.5;

  sin = sliceCreate(hin->nx, hin->ny, hin->mode);
  vol = (Islice **)malloc( opt->oz * sizeof(Islice *));
  v.vol = vol;
  v.zsize = opt->oz;

  /* rotate z */
  ko = (hin->nz - opt->oz) / 2;

  for(k = 0; k < opt->oz; k++){
    if (((k + ko) >= 0) && ((k + ko) < hin->nz)){
      if (mrc_read_slice((void *)sin->data.b, hin->fp, hin, k + ko, 'z'))
        return -1;
      vol[k] = mrc_slice_rotate(sin, (double)opt->z, 
                                opt->ox, opt->oy,
                                (double)opt->cx,
                                (double)opt->cy);
    }else{
      vol[k] = sliceCreate(opt->ox, opt->oy, hin->mode);
    }
  }
  if ((opt->x == 0) && (opt->y == 0)){
    write_vol(vol, hout);
    free_vol(vol, hin->nz);
    return(0);
  }

  for (j = 0; j < opt->oy; j++){
    sin  = mrc_slice_getvol(&v, j, 'y');
    sout = mrc_slice_rotate(sin, (double)-opt->y,
                            opt->ox, opt->oz,
                            (double)opt->cx,
                            (double)opt->cz);
    mrc_slice_putvol(&v, sout, j, 'y');
    sliceFree(sout);
  }
  if (opt->x == 0){
    write_vol(vol, hout);
    free_vol(vol, hin->nz);
    return(0);
  }

  for (i = 0; i < opt->ox; i++){
    sin  = mrc_slice_getvol(&v, i, 'x');
    sout = mrc_slice_rotate(sin, (double)opt->x,
                            opt->oy, opt->oz,
                            (double)opt->cy,
                            (double)opt->cz);

    mrc_slice_putvol(&v, sout, i, 'x');
    sliceFree(sout);
  }

  write_vol(vol, hout);
  free_vol(vol, hin->nz);
  return(0);
}


int grap_2dtrans(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  if ((hin) && (hout) &&  (opt))
    return(0);
  return(-1);
}

int grap_trans(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  Islice *sin, *sout, **vol;
  Istack v;
  int j, k;

  if (opt->oz == IP_DEFAULT)
    opt->oz = hin->nz;
  if (opt->ox == IP_DEFAULT)
    opt->ox = hin->nx;
  if (opt->oy == IP_DEFAULT)
    opt->oy = hin->ny;
  if (opt->x == IP_DEFAULT)
    opt->x = 0;
  if (opt->y == IP_DEFAULT)
    opt->y = 0;
  if (opt->z == IP_DEFAULT)
    opt->z = 0;
     
  printf("clip: translating, (x, y, z) = ( %g, %g, %g).\n",
         opt->x, opt->y, opt->z);

  mrc_head_new(hout, opt->ox, opt->oy, opt->oz, hin->mode);

  mrc_head_label(hout, "Image translated.");

  sin = sliceCreate(hin->nx, hin->ny, hin->mode);
  vol = (Islice **)malloc( hin->nz * sizeof(Islice *));
  v.vol = vol;
  v.zsize = hin->nz;

  for(k = 0; k < hin->nz; k++){
    if (mrc_read_slice((void *)sin->data.b, hin->fp, hin, k, 'z'))
      return -1;
    vol[k] = mrc_slice_translate(sin, 
                                 (double)opt->x, (double)opt->y,
                                 opt->ox, opt->oy);
  }

  sliceFree(sin);
  if (mrc_head_write(hout->fp, hout))
    return -1;

  if (opt->z == 0){
    /* write vol */
    write_vol(vol, hout);
    free_vol(vol, hin->nz);
    return(0);
  }

  /* mrc_data_new(hout->fp, hout);  This cleared the file out! */
  sin  = sliceCreate(opt->ox, hin->nz, hin->mode);
  sout = sliceCreate(opt->ox, opt->oz, hin->mode);
  for (j = 0; j < vol[0]->ysize; j++){
    sin  = mrc_slice_getvol(&v, j, 'y');
    sout = mrc_slice_translate(sin, (double)0.0, (double)opt->z,
                               opt->ox, opt->oz);
    if (mrc_write_slice((void *)sout->data.b, hout->fp, hout, j, 'y'))
      return -1;
    sliceMMM(sout);
    if (!j){
      hout->amin = sout->min;
      hout->amax = sout->max;
      hout->amean = sout->mean;
    }else{
      if (sout->min < hout->amin)
        hout->amin = sout->min;
      if (sout->max > hout->amax)
        hout->amax = sout->max;
      hout->amean += sout->mean;
    }
  }
  hout->amean /= hout->ny;
  if (mrc_head_write(hout->fp, hout))
    return -1;
  free_vol(vol, hin->nz);
  sliceFree(sout);
  sliceFree(sin);
  return(0);
}


int grap_zoom(MrcHeader *hin, MrcHeader *hout, ClipOptions *opt)
{
  Islice *sin, *sout, **vol;
  Istack v;
  int j, k;

  if (opt->z == IP_DEFAULT)
    opt->z = 1.0;
  if (opt->y == IP_DEFAULT)
    opt->y = 1.0;
  if (opt->x == IP_DEFAULT)
    opt->x = 1.0;

  if (opt->z <= 0) 
    return(-1);
  if (opt->y <= 0)
    return(-1);
  if (opt->x <= 0)
    return(-1);

  printf("clip: zooming, ( x, y, z) = ( %g, %g, %g).\n",
         opt->x, opt->y, opt->z);
     
  if (opt->oz == IP_DEFAULT){
    opt->oz = hin->nz * opt->z;
  }
  if (opt->ox == IP_DEFAULT){
    opt->ox = hin->nx * opt->x;
  }
  if (opt->oy == IP_DEFAULT){
    opt->oy = hin->ny * opt->y;
  }

  if ( (!opt->ox) || (!opt->oy) || (!opt->oz))
    return(-1);

  if (opt->cx == IP_DEFAULT)
    opt->cx = hin->nx * 0.5;
  if (opt->cy == IP_DEFAULT)
    opt->cy = hin->ny * 0.5;
  if (opt->cz == IP_DEFAULT)
    opt->cz = hin->nz * 0.5;

  if (opt->sano){
    opt->oz = hin->nz;
    opt->oy = hin->ny;
    opt->ox = hin->nx;
  }

  mrc_head_new(hout, opt->ox, opt->oy, opt->oz, hin->mode);

  mrc_head_label(hout, "Image zoomed.");

  sin = sliceCreate(hin->nx, hin->ny, hin->mode);
  vol = (Islice **)malloc( hin->nz * sizeof(Islice *));
  v.vol = vol;
  v.zsize = hin->nz;

  for(k = 0; k < hin->nz; k++){
    if (mrc_read_slice((void *)sin->data.b, hin->fp, hin, k, 'z'))
      return -1;
    vol[k] = mrc_slice_zoom(sin, (double)opt->x, (double)opt->y,
                            opt->ox, opt->oy, 
                            (double)opt->cx, (double)opt->cy);
  }

  sliceFree(sin);
  if (mrc_head_write(hout->fp, hout))
    return -1;

  if ((opt->z == 1.0) && (hout->nz == hin->nz)){
    /* write vol */
    write_vol(vol, hout);
    free_vol(vol, hin->nz);
    return(0);
  }

  /* mrc_data_new(hout->fp, hout); */
  sin  = sliceCreate(opt->ox, hin->nz, hin->mode);
  sout = sliceCreate(opt->ox, opt->oz, hin->mode);
  for (j = 0; j < vol[0]->ysize; j++){
    sin  = mrc_slice_getvol(&v, j, 'y');
    sout = mrc_slice_zoom(sin, (double)1.0, (double)opt->z,
                          opt->ox, opt->oz,
                          (double)opt->ox * 0.5, 
                          (double)opt->cz);
    if (mrc_write_slice((void *)sout->data.b, hout->fp, hout, j, 'y'))
      return -1;
    sliceMMM(sout);
    if (!j){
      hout->amin = sout->min;
      hout->amax = sout->max;
      hout->amean = sout->mean;
    }else{
      if (sout->min < hout->amin)
        hout->amin = sout->min;
      if (sout->max > hout->amax)
        hout->amax = sout->max;
      hout->amean += sout->mean;
    }
  }
  hout->amean /= hout->ny;
  if (mrc_head_write(hout->fp, hout))
    return -1;

  /* free vol */
  for (k = 0; k < hin->nz; k++){
    sliceFree(vol[k]);
  }
  free(vol);
  sliceFree(sout);
  sliceFree(sin);
     

  return(0);
}          


