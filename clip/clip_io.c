/*
 *  clip_io.c -- Disk input/output for clip.
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

#include <stdlib.h>
#include "mrcc.h"
#include "b3dutil.h"
#include "clip.h"

/* DNM 1/5/05: changed to return result of writing header */
int set_mrc_coords(struct Grap_options *opt)
{
  float tx, ty, tz;
  struct MRCheader *hin = opt->hin;
  struct MRCheader *hout = opt->hout;

  mrc_coord_cp(hout, hin);
     
  /* DNM 9/21/01: subtract rather than add tx, ty, tz, because of weird
     definition of origin */
  tx = opt->cx - (opt->ix/2);
  tx += (opt->ix - opt->ox)/2;
  hout->xorg -= tx;
     
  ty = opt->cy - (opt->iy/2);
  ty += (opt->iy - opt->oy)/2;
  hout->yorg -= ty;
   
  /* DNM 1/5/05: with -2d, -iz is treated like a range list, not a size */
  if (opt->dim == 3) {
    tz = opt->cz - (opt->iz/2);
    tz += (opt->iz - opt->oz)/2;
    hout->zorg -= tz;
  }
     
  return (mrc_head_write(hout->fp, hout));
}

void set_input_options(struct Grap_options *opt, struct MRCheader *hin)
{
  int i, zst, znd;

  if (opt->nofsecs == IP_DEFAULT){
    opt->nofsecs = hin->nz;
    opt->secs = (int *)malloc(sizeof(int) * hin->nz);
    for(i = 0; i < hin->nz; i++)
      opt->secs[i] = i;
     
  }
  if (opt->dim == 3){
    if (opt->iz == IP_DEFAULT){
      opt->iz = hin->nz;
    }
    if (opt->cz == IP_DEFAULT){
      opt->cz = hin->nz * 0.5f;
    }
    if (opt->oz == IP_DEFAULT)
      opt->oz = opt->iz;

    /* Set up a section list for the case where something is treated as
       2d without -2d being given */
    if (opt->nofsecs != IP_DEFAULT)
      free(opt->secs);
    zst = B3DMAX((int)(opt->cz - opt->iz / 2.), 0);
    znd = B3DMIN(zst + opt->iz - 1, hin->nz - 1);
    opt->nofsecs = znd + 1 - zst;
    opt->secs = (int *)malloc(sizeof(int) * opt->nofsecs);
    for (i = zst; i <= znd; i++)
      opt->secs[i - zst] = i;
  }

  if (opt->ix == IP_DEFAULT)
    opt->ix = hin->nx;
  if ((int)opt->cx == IP_DEFAULT)
    opt->cx = hin->nx * 0.5f;
  if (opt->ox == IP_DEFAULT)
    opt->ox = opt->ix;
  else{
    if (!opt->ocanresize){
      show_error("clip warning: Process can't change output size.");
      opt->ox = opt->ix;
    }
  }

  if (opt->iy == IP_DEFAULT)
    opt->iy = hin->ny;
  if ((int)opt->cy == IP_DEFAULT)
    opt->cy = hin->ny * 0.5f;
  if (opt->oy == IP_DEFAULT)
    opt->oy = opt->iy;
  else{
    if (!opt->ocanresize){
      show_error("clip warning: Process can't change output size.");
      opt->oy = opt->iy;
    }
  }

  if (opt->pad == IP_DEFAULT)
    opt->pad = hin->amean;

  if (opt->mode == IP_DEFAULT)
    opt->mode = hin->mode;
}

/* returns first z section to write, or negative for error. */
int set_output_options(struct Grap_options *opt, struct MRCheader *hout)
{
  int z = 0;
  int dsize;

  /* create a new file */
  if (!opt->add2file){
    mrc_head_new(hout, opt->ox, opt->oy, opt->nofsecs, opt->mode);
    if (mrc_head_write(hout->fp, hout))
      return -1;
  }
  else{
    if ((opt->ox != hout->nx) || (opt->oy != hout->ny)){
      if (opt->ocanresize){
        opt->ox = hout->nx;
        opt->oy = hout->ny;
      }
      else{
        show_error("clip error: Appended file can't change size.");
        return(-1);
      }
    }
    if (opt->mode != hout->mode){
      if (opt->ocanchmode){
        opt->mode = hout->mode;
      }else{
        show_error("clip error: Appended file can't change mode.");
        return(-1);
      }
            
    }
    if (IP_APPEND_ADD == opt->add2file){
      z = hout->nz;
      hout->nz += opt->nofsecs;
    }
    else{
      z = opt->isec;
      hout->nz = opt->nofsecs + opt->isec;
    }
    dsize = 1;
    if (hout->mode == MRC_MODE_SHORT)
      dsize = sizeof(short);
    if (hout->mode == MRC_MODE_FLOAT) 
      dsize = sizeof(float);
    if (hout->mode == MRC_MODE_COMPLEX_SHORT) 
      dsize = 2 * sizeof(short);
    if (hout->mode == MRC_MODE_COMPLEX_FLOAT) 
      dsize = 2 * sizeof(float);
    if (hout->mode == MRC_MODE_RGB)
      dsize = 3;
      
    if (mrc_head_write(hout->fp, hout))
      return -1;
    /* fseek(hout->fp, hout->headerSize + 
       (hout->nx * hout->ny * z * dsize),0); */
    mrc_big_seek(hout->fp, hout->headerSize,
                 hout->nx * hout->ny, z * dsize, 0);
  }
  return(z);
}

int set_options(struct Grap_options *opt, 
                struct MRCheader *hin, 
                struct MRCheader *hout)
{
  set_input_options(opt, hin);
  return(set_output_options(opt,hout));
}


struct MRCvolume *grap_volume_read(struct MRCheader *hin, 
                                   struct Grap_options *opt)
{
  struct MRCvolume *v;
  struct MRCslice  *s;
  Ival val;
  int i, j, k, x, y, z;

  if (opt->dim == 2){
    if (opt->iz == IP_DEFAULT) opt->iz = 0;
    if (opt->iz2 == IP_DEFAULT) opt->iz2 = hin->nz - 1;
    if (opt->iz2 == 0) opt->iz2 = opt->iz;
    if (opt->iz2 < opt->iz) opt->iz2 = opt->iz;
    opt->cz = ( opt->iz2 + opt->iz) / 2;
    opt->iz = opt->iz2 - opt->iz + 1;
  }

  if (opt->ix == IP_DEFAULT) opt->ix = hin->nx;
  if (opt->iy == IP_DEFAULT) opt->iy = hin->ny;
  if (opt->iz == IP_DEFAULT) opt->iz = hin->nz;
  if (opt->cx == IP_DEFAULT) opt->cx = hin->nx / 2;
  if (opt->cy == IP_DEFAULT) opt->cy = hin->ny / 2;
  if (opt->cz == IP_DEFAULT) opt->cz = hin->nz / 2;
  if (opt->pad == IP_DEFAULT) opt->pad = hin->amean;

  val[0] = opt->pad;
  val[1] = opt->pad;
  val[2] = opt->pad;

  /* Create volume and initialize. */
  v = (struct MRCvolume *)malloc(sizeof(struct MRCvolume));
  v->vol = (struct MRCslice **)malloc( opt->iz * sizeof(struct MRCslice *));
  v->zsize = opt->iz;

  for( k = 0; k < opt->iz; k++){
    v->vol[k] = mrc_slice_create(opt->ix, opt->iy, hin->mode);
    if (!v->vol[k])
      return(NULL);
    for( j = 0; j < opt->oy; j++)
      for(i = 0; i < opt->ox; i++)
        slicePutVal(v->vol[k], i, j, val);
    v->vol[k]->mean = hin->amean;
    v->vol[k]->max  = hin->amax;
    v->vol[k]->min  = hin->amin;
  }

  s = mrc_slice_create(hin->nx, hin->ny, hin->mode);
  if (!s)
    return(NULL);
  k = (opt->cz - ((float)opt->iz * 0.5f));
  /*     printf("k = %d\n", k); */
  for(z = 0; (k < hin->nz) && (z < opt->iz); k++, z++){
    if (k >= 0){
      if (mrc_read_slice((void *)s->data.b, hin->fp, hin, k, 'z'))
        return (NULL);
      j = opt->cy - (opt->iy/2);
      for (y = 0; (j < hin->ny) && (y < opt->iy); j++, y++){
        i = opt->cx - (opt->ix / 2);
        for (x = 0; (i < hin->nx) && (x < opt->ix); i++, x++){
          sliceGetVal(s, i, j, val);
          slicePutVal(v->vol[z], x, y, val);
        }
      }
    }
  }

  mrc_slice_free(s);
  return(v);
}


int grap_volume_write(struct MRCvolume *v,  struct MRCheader *hout, 
                      struct Grap_options *opt)
{
  FILE *fp = hout->fp;
  int zs, ks, z;
  int i,j,k;
  float min, max, mean, zscale;
  Ival val;
  struct MRCslice  *s, *ps;
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
  mrc_slice_calcmmm(v->vol[0]);
  min = v->vol[0]->min;
  max = v->vol[0]->max;
  mean = v->vol[0]->mean;
  for(k = 1; k < v->zsize; k++){
    mrc_slice_calcmmm(v->vol[k]);
    if ( v->vol[k]->min < min)
      min = v->vol[k]->min;
    if (v->vol[k]->max > max)
      max = v->vol[k]->max;
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
    if (min < hout->amin)
      hout->amin = min;
    if (max > hout->amax)
      hout->amax = max;
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
      fprintf(stderr,
              "inserting requires data modes to be the same.\n");
      return(-1);
    }
    if (min < hout->amin)
      hout->amin = min;
    if (max > hout->amax)
      hout->amax = max;
    hout->amean = ((hout->amean * ks) + (mean * opt->oz)) / hout->nz; 
    /*  mean = hout->amean;
        min = hout->amin;
        max = hout->amax; */
    zscale = hout->zlen / hout->mz;
    hout->mz = hout->nz;
    hout->zlen = hout->mz * zscale;
    break;
    break;

  case IP_APPEND_FALSE:
    /* Notice in this mode that the output file completely inherits
       the whole input file's min/max/mean! */
    /* hout->nx = opt->ox;
       hout->ny = opt->oy;
       hout->nz = opt->oz;
       hout->mode =  v->vol[0]->mode; */
    labels = hout->nlabl;
    mrc_head_new(hout, opt->ox, opt->oy, opt->oz, v->vol[0]->mode);
    hout->nlabl = labels;
    hout->amin = min;
    hout->amax = max;
    hout->amean = mean;
    ks = 0;

  }

  ps = mrc_slice_create(hout->nx, hout->ny, hout->mode);
  if (!ps){
    fprintf(stderr, "volume_write:  error getting slice\n");
    return(-1);
  }
  if (opt->pad == IP_DEFAULT) opt->pad = hout->amean;
  val[0] = opt->pad;
  val[1] = opt->pad;
  val[2] = opt->pad;

  for(j = 0; j < hout->ny; j++)
    for(i = 0; i < hout->nx; i++)
      slicePutVal(ps, i, j, val);
     
  if (mrc_head_write(fp, hout))
    return -1;

  zs = (v->zsize - opt->oz) / 2;
  for(k = ks, z = zs; k < hout->nz; k++, z++){
    if ((z < 0) || (z >= v->zsize)){
      if (mrc_write_slice((void *)ps->data.b, fp, hout, k, 'z'))
        return (-1);
    }
    else{
      v->vol[z]->mean =  hout->amean;
      s = mrc_slice_resize(v->vol[z], opt->ox, opt->oy);
      if (!s){
        fprintf(stderr, "volume_write: error resizing slice.\n");
        return(-1);
      }
      if (mrc_write_slice((void *)s->data.b, fp, hout, k, 'z'))
        return (-1);
      mrc_slice_free(s);
    }
  }

  mrc_slice_free(ps);
  return(0);
}


int grap_volume_free(struct MRCvolume *v)
{
  int k;

  if (!v)
    return(-1);

  for (k = 0; k < v->zsize; k++)
    mrc_slice_free(v->vol[k]);

  free(v->vol);
  free(v);
  return(0);
}



int mrc_head_print(struct MRCheader *data)
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
  if (data->nsymbt)
    printf("nsymbt =\t%d\n",data->nsymbt) ;
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

/*
$Log$
Revision 3.5  2004/11/05 18:53:16  mast
Include local files with quotes, not brackets

Revision 3.4  2004/04/22 19:08:45  mast
Added error checks and returns on mrc I/O calls

Revision 3.3  2003/11/18 19:29:50  mast
changes to call b3dF* functions for 2GB problem on Windows

Revision 3.2  2002/09/14 01:01:18  mast
Fixed output of scale factors in header output to correspond to proper
usage, and fixed attempt to adjust zlen when overwriting or appending

Revision 3.1  2002/06/26 16:48:52  mast
Fixed problems with header being reinitialized when appending or
overwriting to an output file

*/

