/*  IMOD VERSION 2.02
 *
 *  clip_transform.c -- Image transformation functions for clip.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1996 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/

#include <stdlib.h>
#include "mrcfiles.h"
#include "mrcslice.h"
#include "mrcspectral.h"
#include "clip.h"


int grap_rotate(struct MRCheader *hin, struct MRCheader *hout,
		 struct Grap_options *opt)
{
     struct MRCslice *sin, *sout, **vol;
     struct MRCvolume v;
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
     printf("grap: rotating, gamma = %g, beta = %g, alpha = %g\n",
	    opt->z, opt->y, opt->x);

     mrc_head_new(hout, opt->ox, opt->oy, opt->oz, hin->mode);
     mrc_head_label(hout, "Image rotated.");

     opt->cx = hout->nx * 0.5;
     opt->cy = hout->ny * 0.5;
     opt->cz = hout->nz * 0.5;

     sin = mrc_slice_create(hin->nx, hin->ny, hin->mode);
     vol = (struct MRCslice **)malloc( opt->oz * sizeof(struct MRCslice *));
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
	       vol[k] = mrc_slice_create(opt->ox, opt->oy, hin->mode);
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
	  mrc_slice_free(sout);
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
	  mrc_slice_free(sout);
     }

     write_vol(vol, hout);
     free_vol(vol, hin->nz);
     return(0);
}


int grap_2dtrans(struct MRCheader *hin, struct MRCheader *hout,
		 struct Grap_options *opt)
{
     if ((hin) && (hout) &&  (opt))
	  return(0);
     return(-1);
}

int grap_trans(struct MRCheader *hin, struct MRCheader *hout, 
	       struct Grap_options *opt)
{
     struct MRCslice *sin, *sout, **vol;
     struct MRCvolume v;
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
     
     printf("grap: translating, (x, y, z) = ( %g, %g, %g).\n",
	    opt->x, opt->y, opt->z);

     mrc_head_new(hout, opt->ox, opt->oy, opt->oz, hin->mode);

     mrc_head_label(hout, "Image translated.");

     sin = mrc_slice_create(hin->nx, hin->ny, hin->mode);
     vol = (struct MRCslice **)malloc( hin->nz * sizeof(struct MRCslice *));
     v.vol = vol;
     v.zsize = hin->nz;

     for(k = 0; k < hin->nz; k++){
       if (mrc_read_slice((void *)sin->data.b, hin->fp, hin, k, 'z'))
         return -1;
	  vol[k] = mrc_slice_translate(sin, 
				       (double)opt->x, (double)opt->y,
				       opt->ox, opt->oy);
     }

     mrc_slice_free(sin);
     if (mrc_head_write(hout->fp, hout))
       return -1;

     if (opt->z == 0){
	  /* write vol */
	  write_vol(vol, hout);
	  free_vol(vol, hin->nz);
	  return(0);
     }

     mrc_data_new(hout->fp, hout);
     sin  = mrc_slice_create(opt->ox, hin->nz, hin->mode);
     sout = mrc_slice_create(opt->ox, opt->oz, hin->mode);
     for (j = 0; j < vol[0]->ysize; j++){
	  sin  = mrc_slice_getvol(&v, j, 'y');
	  sout = mrc_slice_translate(sin, (double)0.0, (double)opt->z,
				     opt->ox, opt->oz);
	  if (mrc_write_slice((void *)sout->data.b, hout->fp, hout, j, 'y'))
        return -1;
	  mrc_slice_calcmmm(sout);
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
     mrc_slice_free(sout);
     mrc_slice_free(sin);
     return(0);
}


int grap_zoom(struct MRCheader *hin, struct MRCheader *hout, 
	       struct Grap_options *opt)
{
     struct MRCslice *sin, *sout, **vol;
     struct MRCvolume v;
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

     printf("grap: zooming, ( x, y, z) = ( %g, %g, %g).\n",
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

     sin = mrc_slice_create(hin->nx, hin->ny, hin->mode);
     vol = (struct MRCslice **)malloc( hin->nz * sizeof(struct MRCslice *));
     v.vol = vol;
     v.zsize = hin->nz;

     for(k = 0; k < hin->nz; k++){
       if (mrc_read_slice((void *)sin->data.b, hin->fp, hin, k, 'z'))
         return -1;
	  vol[k] = mrc_slice_zoom(sin, (double)opt->x, (double)opt->y,
				  opt->ox, opt->oy, 
				  (double)opt->cx, (double)opt->cy);
     }

     mrc_slice_free(sin);
     if (mrc_head_write(hout->fp, hout))
       return -1;

     if ((opt->z == 1.0) && (hout->nz == hin->nz)){
	  /* write vol */
	  write_vol(vol, hout);
	  free_vol(vol, hin->nz);
	  return(0);
     }

     mrc_data_new(hout->fp, hout);
     sin  = mrc_slice_create(opt->ox, hin->nz, hin->mode);
     sout = mrc_slice_create(opt->ox, opt->oz, hin->mode);
     for (j = 0; j < vol[0]->ysize; j++){
	  sin  = mrc_slice_getvol(&v, j, 'y');
	  sout = mrc_slice_zoom(sin, (double)1.0, (double)opt->z,
				opt->ox, opt->oz,
				(double)opt->ox * 0.5, 
				(double)opt->cz);
	  if (mrc_write_slice((void *)sout->data.b, hout->fp, hout, j, 'y'))
        return -1;
	  mrc_slice_calcmmm(sout);
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
	  mrc_slice_free(vol[k]);
     }
     free(vol);
     mrc_slice_free(sout);
     mrc_slice_free(sin);
     

     return(0);
}	       


