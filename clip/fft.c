/*  IMOD VERSION 2.42
 *
 *  fft.c -- calculate fft for clip, also used by filter & correlation
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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

#ifndef NOFFTLIB

#include <stdlib.h>
#include <string.h>
#include <mrcc.h>
#include "clip.h"

/**************************************************************/
/* c-stub for fortran SUBROUTINE TODFFT(ARRAY,NX,NY,IDIR)     */
/* buf is (nx + 2)/2 by ny floats                             */
/* idir 0 = forward, 1 = inverse, -1 = inverse w/o conjugate  */

/*
 * ODFFT do a series of 1D FFT's on 2D data.
 * idir -1 forward, -2 reverse.
 */
#ifdef F77FUNCAP
#define odfft_ ODFFT
#define todfft_ TODFFT
#define niceframe_ NICEFRAME
#endif
#ifdef __cplusplus
extern "C" {
#endif
void todfft_(float buf[], int *nx, int *ny, int *idir);
void odfft_ (float *buf, int *nx, int *ny, int *idir);
#ifdef __cplusplus
}
#endif


void mrcToDFFT(float buf[], int nx, int ny, int idir)
{
     todfft_(buf, &nx, &ny, &idir); 
}


int mrcODFFT(float *buf, int nx, int ny, int idir)
{
     odfft_(buf, &nx, &ny, &idir);
     return(0);
}

int clip_nicesize(int size)
{
     int i;

     for(i = 2; i < 20; i++)
	  while( !(size%i))
	       size = size / i;

     if (size > 1)
	  return(0);
     else
	  return(1);

/*
     int nsize = niceframe_(&size, &inc, &fac);
     if (nsize == size)
	  return(1);
     else
	  return(0);
*/
}


int slice_fft(Islice *slice)
{
     float *tbuf, *buf;
     int nx2  = slice->xsize + 2;
     int ncs  = (int)slice->xsize * sizeof(float);
     int y2   = slice->ysize / 2;
/*     int x2   = slice->xsize / 2; */
     int nx2s = nx2 * sizeof(float);
     int nxs  = (int)slice->xsize * sizeof(float);
     int i, j,k;
     int idir = 1;

     if (slice->mode == MRC_MODE_COMPLEX_FLOAT)
	  idir = -1;
     if (slice->mode == MRC_MODE_COMPLEX_SHORT)
	  idir = -1;

     /* forward fft */
     if (idir == 1){
	  sliceFloat(slice); 
	  tbuf = (float *)malloc(sizeof(float) * nx2 * slice->ysize);
	  for(i = 0; i < slice->ysize; i++)
	       memcpy(&(tbuf[i * nx2]), 
		      &(slice->data.f[i * slice->xsize]), 
		      ncs);
	  free(slice->data.f);
	  mrcToDFFT(tbuf, slice->xsize, slice->ysize, 0);
	  buf = (float *)malloc(sizeof(float)*2*slice->xsize*slice->ysize);
	  sliceInit(slice, slice->xsize, slice->ysize, 
		    MRC_MODE_COMPLEX_FLOAT,
		    buf);

	  /* Move top half of FFT to lower right of new array, and store the
	     extra pixel at the left edge of each row */
	  for(j = 0, i = y2; i < slice->ysize; i++, j++){
	       memcpy(&(buf[(2 * j * slice->xsize) + slice->xsize]),
		      &(tbuf[i * nx2]), 
		      nxs);
	       memcpy(&(buf[(2 * j * slice->xsize)]),
		      &(tbuf[(i * nx2)+slice->xsize]),sizeof(float)*2);
	  }
	  /* Now start at the FFT origin and move bottom half of FFT into the
	     upper right of new array */
	  for(i = 0; i < y2; i++, j++){
	       memcpy(&(buf[(2 * j * slice->xsize) + slice->xsize]),
		      &(tbuf[i * nx2]),
		      nxs);
	       memcpy(&(buf[(2 * j * slice->xsize)]),
		      &(tbuf[(i * nx2)+slice->xsize]),sizeof(float)*2);
	       
	  }

	  /* Copy each row after the first point to the mirror row in Y, in
	     inverse order */
	  for(j = 0 ; j < slice->ysize; j++)
	       for(k = slice->xsize - 2,i = 2; i < slice->xsize; i++,k-=2){
		    buf[i+(slice->xsize*2*j)] = 
			 buf[slice->xsize+k + 
			     (slice->xsize*2*(slice->ysize - j - 1))];
		    buf[++i + (slice->xsize*2*j)] = 
			 buf[slice->xsize+ k+1 +
			     (slice->xsize*2*(slice->ysize - j - 1))];
	       }
	  free(tbuf);
     }else{
	  sliceComplexFloat(slice); 

	  tbuf = (float *)malloc(nx2s * slice->ysize);

	  mrc_slice_wrap(slice);
	  for(j = 0; j < slice->ysize; j++){
	       memcpy(&(tbuf[j * nx2]),
		      &(slice->data.f[(j * slice->xsize * 2)]),
		      nx2s);
	  }
	  free(slice->data.f);
	  mrcToDFFT(tbuf, slice->xsize, slice->ysize, 1);
	  for(j = 0; j < slice->ysize; j++)
	       for(i = 0; i < slice->xsize; i++)
		    tbuf[i + (j * slice->xsize)] = 
			 tbuf[i + (j * slice->xsize) + (j * 2)];

	  slice->mode = 2;
	  slice->data.f = tbuf;
     }
     return(0);
}

/* do a complex<->complex fft in the 3-rd dimension only */
int clip_fftvol3(struct MRCvolume *v, int idir)
{
     int x, y, z, i, j;
     Islice *slice;
     Ival val;
     int  vxsize = v->vol[0]->xsize;
     int  vysize = v->vol[0]->ysize;

     slice = sliceCreate(v->zsize, vysize,
			 SLICE_MODE_COMPLEX_FLOAT);
     for(x = 0; x < vxsize; x++){

	  for(j = 0, y = 0; j < slice->ysize; j++, y++)
	       for(i = 0, z = 0; i < slice->xsize; i++, z++){
		    sliceGetComplexFloatVal(v->vol[z], x, y, val);
		    slicePutComplexFloatVal(slice, i, j, val);
	       }

	  mrcODFFT(slice->data.f, slice->xsize, slice->ysize, idir);

	  /* put slice back into volume. */
	  for(j = 0, y = 0; j < slice->ysize; j++, y++)
	       for(i = 0, z = 0; i < slice->xsize; i++, z++){
		    sliceGetComplexFloatVal(slice, i, j, val);
		    slicePutComplexFloatVal(v->vol[z], x, y, val);
	       }
     }
     sliceFree(slice);
     return(0);
}

int clip_fftfile3(struct MRCheader *hdata, int idir)
{
     unsigned int i, j;
     unsigned int nx = hdata->nx;
     unsigned int ny = hdata->nz;
     Islice *slice, *fs;
     Ival val;
     int  y;
     int  vysize = hdata->ny;
     
     if (hdata->mode != SLICE_MODE_COMPLEX_FLOAT)
	  return(-1);

     slice = sliceCreate(hdata->nx, hdata->nz,
			 SLICE_MODE_COMPLEX_FLOAT);
     fs    = sliceCreate(hdata->nz, hdata->nx,
			 SLICE_MODE_COMPLEX_FLOAT);

     for(y = 0; y < vysize; y++){
	  mrc_read_slice(slice->data.f, hdata->fp, hdata, y, 'y');
	  
	  for(j = 0; j < ny; j++)
	       for(i = 0; i < nx; i++){
		    sliceGetComplexFloatVal(slice, i, j, val);
		    slicePutComplexFloatVal(fs,    j, i, val);
	       }
	  
	  mrcODFFT(fs->data.f, fs->xsize, fs->ysize, idir);
	  
	  for(j = 0; j < ny; j++)
	       for(i = 0; i < nx; i++){
		    sliceGetComplexFloatVal(fs,    j, i, val);
		    slicePutComplexFloatVal(slice, i, j, val);
	       }
	  
	  mrc_write_slice(slice->data.f, hdata->fp, hdata, y, 'y');
     }
     sliceFree(slice);
     return(0);
}



int clip_wrapvol(struct MRCvolume *v)
{
     Ival val, tval;
     Islice *tsl;
     int i, j,j2;
     int k,k2;
     int nx = v->vol[0]->xsize;
     int ny = v->vol[0]->ysize;
     int hny = ny / 2;
     int hnz = v->zsize / 2;

     for(k = 0; k < v->zsize; k++){
	  for(i = 0; i < nx; i++){
	       for(j = 0, j2 = hny; j < hny; j++,j2++){
		    sliceGetVal(v->vol[k], i, j, val);
		    sliceGetVal(v->vol[k], i, j2, tval);
		    slicePutVal(v->vol[k], i, j, tval);
		    slicePutVal(v->vol[k], i, j2, val);
	       }
	  }
     }
     for(k = 0, k2 = hnz; k < hnz; k++, k2++){
	  tsl = v->vol[k];
	  v->vol[k] = v->vol[k2];
	  v->vol[k2] = tsl;
     }
     return(0);
}

int clip_wrapfile(struct MRCheader *hdata)
{
     Ival val, tval;
     Islice *s1, *s2;
     int i, j,j2;
     int k,k2;
     int nx = hdata->nx;
     int ny = hdata->ny;
     int hny = ny / 2;
     int hnz = hdata->nz / 2;
     
     s1 = sliceCreate(hdata->nx, hdata->ny, hdata->mode);
     s2 = sliceCreate(hdata->nx, hdata->ny, hdata->mode);

     for(k = 0, k2 = hnz; k < hnz; k++, k2++){
	  mrc_read_slice(s1->data.f, hdata->fp, hdata, k, 'z');
	  mrc_read_slice(s2->data.f, hdata->fp, hdata, k2, 'z');
	  
	  for(i = 0; i < nx; i++){
	       for(j = 0, j2 = hny; j < hny; j++,j2++){
		    sliceGetVal(s1, i, j,  val);
		    sliceGetVal(s1, i, j2, tval);
		    slicePutVal(s1, i, j,  tval);
		    slicePutVal(s1, i, j2, val);
		    sliceGetVal(s2, i, j,  val);
		    sliceGetVal(s2, i, j2, tval);
		    slicePutVal(s2, i, j,  tval);
		    slicePutVal(s2, i, j2, val);
	       }
	  }

	  mrc_write_slice(s1->data.f, hdata->fp, hdata, k2, 'z');
	  mrc_write_slice(s2->data.f, hdata->fp, hdata, k, 'z');
     }
     sliceFree(s1);
     sliceFree(s2);
     return(0);
}

/* volume must be float or complex float. */
int clip_fftvol(struct MRCvolume *v)
{
     Islice *slice;
     int z, i;
     int ncs    = (int)v->vol[0]->xsize * sizeof(float);
     int nx2    = v->vol[0]->xsize + 2;
     float *tbuf;

     if (v->vol[0]->mode == SLICE_MODE_COMPLEX_FLOAT){
	  clip_wrapvol(v); 
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
	  clip_wrapvol(v);

     }
     return(0);
}


int clip_3difft_swap(struct MRCheader *hin, struct MRCheader *hout,
		                         struct Grap_options *opt);
int clip_3dffft_swap(struct MRCheader *hin, struct MRCheader *hout,
		                         struct Grap_options *opt);
     
int clip_3dfft_swap(struct MRCheader *hin, struct MRCheader *hout,
                    struct Grap_options *opt)
{
     show_status("Doing 3d fast fourier transform...\n");
     if (hin->mode == SLICE_MODE_COMPLEX_FLOAT)
	  return(clip_3difft_swap(hin, hout, opt));
     return(clip_3dffft_swap(hin, hout, opt));
}

int clip_3difft_swap(struct MRCheader *hin, struct MRCheader *hout,
		    struct Grap_options *opt)
{
     int k;
     int nz = hin->nz;
     struct MRCslice  *s = sliceCreate
	  (hin->nx * 2, hin->ny, SLICE_MODE_FLOAT);
     struct MRCslice  *sout;

     /* BIG WARNING: input file gets mangled. */
     show_status("Mangeling.\n");
     fclose(hin->fp);
     hin->fp = fopen(opt->fnames[0], "r+");
     clip_wrapfile(hin);
     show_status("Inverse in 3rd dim.\n");
     clip_fftfile3(hin, -2);


     if (opt->mode == IP_DEFAULT) opt->mode = SLICE_MODE_FLOAT;

     mrc_head_new(hout,  s->xsize - 2, hin->ny, hin->nz, opt->mode);
     mrc_head_label(hout, "clip: 3D Inverse FFT");
     mrc_head_write(hout->fp, hout);
     show_status("Inverse 2nd and 1st dim.\n");

     for(k = 0; k < nz; k++){
	  mrc_read_slice(s->data.f, hin->fp, hin, k, 'z');
	  mrcToDFFT(s->data.f, (hin->nx - 1) * 2, hin->ny, -1);
	  sout = sliceBox(s, 0, 0, s->xsize - 2, s->ysize);
	  sliceNewMode(sout, opt->mode);
	  mrc_write_slice(sout->data.b, hout->fp, hout, k, 'z');
	  sliceFree(sout);
     }
     sliceFree(s);

     s = sliceCreate(hout->nx, hout->ny, hout->mode);
     mrc_read_slice((void *)s->data.b, hout->fp, hout, 0, 'z');
     sliceMMM(s);
     hout->amin = s->min;
     hout->amax = s->max;
     hout->amean = s->mean;
     for(k = 1; k < hout->nz; k++){
	  mrc_read_slice((void *)s->data.b, hout->fp, hout, k, 'z');
	  sliceMMM(s);
	  hout->amean += s->mean;
	  if (s->min < hout->amin)
	       hout->amin = s->min;
	  if (s->max > hout->amax)
	       hout->amax = s->max;
     }
     hout->amean /= hout->nz;
     mrc_head_write(hout->fp, hout);
     sliceFree(s);

     return(0);
}

int clip_3dffft_swap(struct MRCheader *hin, struct MRCheader *hout,
		    struct Grap_options *opt)
{
     int k, z;
     struct MRCslice  *s;
     struct MRCslice  *ts;
     int llx, lly, urx, ury;

     llx = opt->cx - ((float)opt->ix * 0.5f);
     urx = llx + opt->ix;
     lly = opt->cy - ((float)opt->iy * 0.5f);
     ury = lly + opt->iy;

     ts = sliceCreate(hin->nx, hin->ny, hin->mode);
     if (!ts)
	  return(-1);

     k = (opt->cz - ((float)opt->iz * 0.5f));
     mrc_head_new(hout,  (opt->ix + 2)/2, opt->iy, opt->iz, 
		  SLICE_MODE_COMPLEX_FLOAT);
     mrc_head_label(hout, "clip: 3D FFT");     
     mrc_head_write(hout->fp, hout);
     ts->mean = opt->pad;

     for(z = 0; z < opt->iz; k++, z++){
	  if (k >= 0){
	       mrc_read_slice((void *)ts->data.b, hin->fp, hin, k, 'z');
	       s = sliceBox(ts, llx, lly, urx + 2, ury);
	       sliceFloat(s);
	  }else{
	       s = sliceCreate(opt->ix + 2, opt->iy, SLICE_MODE_FLOAT);
	       sliceClear(s, &(s->mean));
	  }
	  mrcToDFFT(s->data.f, s->xsize - 2, s->ysize, 0);
	  s->xsize = (s->xsize+2)/2;
	  s->mode = SLICE_MODE_COMPLEX_FLOAT;
	  mrc_write_slice((void *)s->data.b, hout->fp, hout, z, 'z');
	  sliceFree(s);
     }

     sliceFree(ts);

     show_status("Transforming 3rd dim.\n");
     clip_fftfile3(hout, -1); 

     show_status("Demangeling.\n");
     clip_wrapfile(hout);

     s = mrc_slice_create(hout->nx, hout->ny, hout->mode);
     mrc_read_slice((void *)s->data.b, hout->fp, hout, 0, 'z');
     sliceMMM(s);
     hout->amin = s->min;
     hout->amax = s->max;
     hout->amean = s->mean;
     for(z = 1; z < hout->nz; z++){
	  mrc_read_slice((void *)s->data.b, hout->fp, hout, z, 'z');
	  sliceMMM(s);
	  hout->amean += s->mean;
	  if (s->min < hout->amin)
	       hout->amin = s->min;
	  if (s->max > hout->amax)
	       hout->amax = s->max;
     }
     hout->amean /= hout->nz;
     mrc_head_write(hout->fp, hout);
     sliceFree(s);
     return(0);
}




int clip_3dfft(struct MRCheader *hin, struct MRCheader *hout, 
	       struct Grap_options *opt)
{
     Istack *v;
     int k;

     if (hin->mode != MRC_MODE_COMPLEX_FLOAT){
	  if (opt->ix == IP_DEFAULT) opt->ix = hin->nx;
	  if (opt->iy == IP_DEFAULT) opt->iy = hin->ny;
	  if (opt->iz == IP_DEFAULT) opt->iz = hin->nz;
	  if (opt->cx == IP_DEFAULT) opt->cx = hin->nx / 2;
	  if (opt->cy == IP_DEFAULT) opt->cy = hin->ny / 2;
	  if (opt->cz == IP_DEFAULT) opt->cz = hin->nz / 2;
	  if (opt->pad == IP_DEFAULT) opt->pad = hin->amean;
	  
	  if ((!clip_nicesize(opt->ix)) ||
	      (!clip_nicesize(opt->iy)) ||
	      (!clip_nicesize(opt->iz))){
	       fprintf(stderr, 
		       "clip: fft input size (%d, %d, %d)\n has factors ",
		       opt->ix, opt->iy, opt->iz);
	       fprintf(stderr, "greater then 19 and/or has odd size.\n");
	       return(-1);
	  }
     }
	  
     if (opt->sano){
	  return(clip_3dfft_swap(hin, hout, opt));
     }
	  
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
     mrc_head_label(hout, "Clip: 3D FFT");     

     if (grap_volume_write(v, hout, opt))
	  return(-1);

     grap_volume_free(v);
     return(0);
}

int clip_fft(struct MRCheader *hin, struct MRCheader *hout, 
	      struct Grap_options *opt)
{
     Islice *slice;
     int k, z = 0;

     if (opt->dim == 3)
	  return(clip_3dfft(hin, hout, opt));

     set_input_options(opt, hin);
     if ((hin->mode == MRC_MODE_COMPLEX_FLOAT) || 
	 (hin->mode == MRC_MODE_COMPLEX_SHORT))
	  opt->mode = MRC_MODE_FLOAT;
     else
	  opt->mode = MRC_MODE_COMPLEX_FLOAT;
     z = set_output_options(opt, hout);
     if (z < 0)
	  return(z);

     show_status("Doing fast fourier transform...\n");     

     for(k = 0; k < opt->nofsecs; k++, z++){
	  slice = sliceReadSubm(hin, opt->secs[k], 'z', opt->ix, opt->iy,
				(int)opt->cx, (int)opt->cy);
	  if (!slice){
	       show_error("fft: Error reading slice.");
	       return(-1);
	  }
	  slice_fft(slice);
	  sliceMMM(slice);
	  if (slice->min < hout->amin)
	       hout->amin = slice->min;
	  if (slice->max > hout->amax)
	       hout->amax = slice->max;
	  hout->amean += slice->mean;
	  hout->mode  = slice->mode;
	  hout->nx    = slice->xsize;
	  hout->ny    = slice->ysize;
	  mrc_write_slice((void *)slice->data.b, hout->fp, hout, z, 'z');
	  sliceFree(slice);
     }

     if (opt->nofsecs)
	  hout->amean /= opt->nofsecs;
     mrc_head_write(hout->fp, hout);
     return(0);
}




#endif /* NOFFTLIB */
