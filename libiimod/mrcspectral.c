/*  IMOD VERSION 2.50
 *
 *  mrcspectral.c -- Time domain proccessing functions.
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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mrcc.h"
#include "b3dutil.h"

#define REAL  0
#define IMAG  1
#define TWOPI 6.283185307179586476925287
#define PI    3.141592654
#define SWAP(a, b) tempr=(a);(a)=(b);(b)=tempr

/* returns total padding needed for size to be power of 2 */
int mrc_pow2pad(int size)
{
  int p = 1;
  while(p < size){
    p *= 2;
  }
  return(p - size); 
}

int mrc_3dfft(struct MRCvolume *v, int direction)
{
  struct MRCslice *s, *sl;
  struct MRCslice **vol;
  float *ftmp;
  int x, i, j, k;
  int index;

  if (mrc_pow2pad(v->zsize)){
    return(-1);
  }
  sl = v->vol[0];
  if (mrc_pow2pad(sl->xsize)){
    return(-1);
  }
  if (mrc_pow2pad(sl->ysize)){
    return(-1);
  }
  if (sl->mode != MRC_MODE_COMPLEX_FLOAT)
    return(-1);

  /* transform rows */
  for (k = 0; k < v->zsize; k++){
    s = v->vol[k];
    for(j = 0; j < s->ysize; j++){
      mrc_1dfft(&(s->data.f[j * 2  * s->xsize]), s->xsize, direction);
    }
  }

  for (x = 0; x < v->vol[0]->xsize; x++){
    s = mrc_slice_getvol(v, x, 'x');
    for(j = 0; j < s->ysize; j++){
      mrc_1dfft(&(s->data.f[j * 2  * s->xsize]), s->xsize, direction);
    }
    ftmp = (float *)malloc( s->ysize * sizeof(float) * 2);
    for(i = 0; i < s->xsize; i++){
      for(j = 0; j < s->ysize; j++){
        index = 2 * ((j * s->xsize) + i);
        ftmp[2*j]       = s->data.f[index];
        ftmp[(2*j) + 1] = s->data.f[index + 1];
      }
      mrc_1dfft(ftmp, (s->ysize), direction);
      for(j = 0; j < s->ysize; j++){
        index = 2 * ((j * s->xsize) + i);
        s->data.f[index] = ftmp[2*j];
        s->data.f[index + 1] = ftmp[(2*j) + 1];
      }
    }
    free(ftmp);
    mrc_slice_putvol(v, s, x, 'x');
    mrc_slice_free(s);
  }
  return(0);
}





struct MRCslice *mrc_fftx(struct MRCslice *si, int direction, int doy)
{
  struct MRCslice *s;
  int i, j;
  int xoff, yoff, x, y;
  int index, my;
  int xm, ym;
  Ival val;
  float *ftmp;
  int nn[3];


  s = mrc_slice_create(si->xsize + mrc_pow2pad(si->xsize), 
                       si->ysize + mrc_pow2pad(si->ysize), 
                       MRC_MODE_COMPLEX_FLOAT);

  if (!s){
    b3dError(stderr, "mrc_fft: error, not enough memory.\n");
    return(NULL);
  }

  xoff = (s->xsize - si->xsize) / 2;
  yoff = (s->ysize - si->ysize) / 2;

  /* copy to padded array */
  val[0] = 0.0;
  val[1] = 0.0;
  for(j = 0; j < s->ysize; j++)
    for(i = 0; i < s->xsize; i++){
      slicePutVal(s,  i, j, val);
    }

  for(j = 0, y = yoff; j < si->ysize; j++, y++)
    for(i = 0, x = xoff; i < si->xsize; i++, x++){
      sliceGetVal(si, i, j, val);
      if (direction == 1)
        val[1] = 0.0;
      slicePutVal(s,  x, y, val);
    }

  for(j = 0; j < s->ysize; j++){
    mrc_1dfft(&(s->data.f[j * 2 * s->xsize]), s->xsize, direction);
  }

  if (s->ysize == 1)
    return(s);
     
  if (!doy)
    return(s);

  /*     mrc_slice_valscale(s, (1.0/(double)s->xsize));  */
  my = s->ysize;
  ftmp = (float *)malloc( s->ysize * sizeof(float) * 2);
  for(i = 0; i < s->xsize; i++){
    for(j = 0; j < my; j++){
      index = 2 * ((j * s->xsize) + i);
      ftmp[2*j]       = s->data.f[index];
      ftmp[(2*j) + 1] = s->data.f[index + 1];
    }
    mrc_1dfft(ftmp, (s->ysize), direction); 
    for(j = 0; j < my; j++){
      index = 2 * ((j * s->xsize) + i);
      s->data.f[index] = ftmp[2*j];
      s->data.f[index + 1] = ftmp[(2*j) + 1];
    }
  }

  free(ftmp);
  return(s);
}

struct MRCslice *mrc_fft(struct MRCslice *si, int direction)
{
  return((struct MRCslice *)mrc_fftx(si, direction, 1));
}


struct MRCslice *mrc_corr(struct MRCslice *s1, struct MRCslice *s2)
{
  struct MRCslice *sout, *sg, *sh, *stg, *sth, *sc, *ssc, *rsc;
  int xpad, ypad;
  int xsize, ysize, xysize, xoff, yoff, x, y;
  int xm, ym;
  int autocorr = FALSE;
  int i, j;
  Ival val;

  if (!s1)
    return(NULL);
     
  if (!s2)
    s2 = s1;

  if (s1 == s2)
    autocorr = TRUE; /* do autocorrelation. */

  if ((s1->xsize != s2->xsize) || (s1->ysize != s2->ysize)){
    b3dError(stderr, "cor: slices must be same size.\n");
    return(NULL);
  }
     
  if (s1->ysize == 1)
    ysize = 1;
  else
    ysize = 2 * s1->ysize;
  xsize = 2 * s1->xsize;

  sg   = (struct MRCslice *)mrc_slice_create
    (xsize, ysize, s1->mode);
  if (!sg){
    b3dError(stderr, "cor: error, not enough memory.\n");
    return(NULL);
  }

  if (!autocorr)
    sh   = mrc_slice_create(xsize, ysize, s2->mode);
  else
    sh = sg;

  if (!sh){
    b3dError(stderr, "cor: error, not enough memory.\n");
    return(NULL);
  }

  val[0] = s1->mean;
  val[1] = s2->mean;
  xoff = (sg->xsize - s1->xsize) / 2;
  yoff = (sg->ysize - s1->ysize) / 2;

  /* copy data into sg. */
  for(j = 0; j < sg->ysize; j++)
    for(i = 0; i < sg->xsize; i++){
      slicePutVal(sg,  i, j, val);
    }
  for (j = 0, y = yoff; j < s1->xsize; j++, y++)
    for(i = 0, x = xoff; i < s1->ysize; i++, x++){
      sliceGetVal(s1, i, j, val);
      slicePutVal(sg,  x, y, val);
    }

  if (!autocorr){
    for(j = 0; j < sh->ysize; j++)
      for(i = 0; i < sh->xsize; i++){
        slicePutVal(sh,  i, j, val);
      }
    for (j = 0, y = yoff; j < s2->ysize; j++, y++)
      for(i = 0, x = xoff; i < s2->xsize; i++, x++){
        sliceGetVal(s2, i, j, val);
        slicePutVal(sh,  x, y, val);
      }
  }


  stg = mrc_fft(sg, 1);
  if (!autocorr)
    sth = mrc_fft(sh, 1);
  else
    sth = stg;

  corr_conj(stg->data.f, sth->data.f, stg->xsize * stg->ysize);
  sc = mrc_fft(stg, -1);

  mrc_slice_free(sg);
  mrc_slice_free(stg);
  if (!autocorr){
    mrc_slice_free(sh);
    mrc_slice_free(sth);
  }
     
  rsc = mrc_slice_real(sc);
  mrc_slice_free(sc);
  ssc = mrc_slice_create(s1->xsize, s1->ysize, MRC_MODE_FLOAT);

  ym = ssc->ysize / 2;
  xm = ssc->xsize / 2;
     
  /* Move corr to center. */
  /* upper right */
  for(j = 0, y = ym; j < ym; j++, y++)
    for(i = 0, x = xm; i < xm; i++, x++){
      sliceGetVal(rsc, i, j, val);
      slicePutVal(ssc, x, y, val);
    }
  /* upper left */
  for(j = 0, y = ym; j < ym; j++, y++)
    for(i = rsc->xsize - 1, x = xm - 1; x >= 0; i--, x--){
      sliceGetVal(rsc, i, j, val);
      slicePutVal(ssc, x, y, val);
    }
  /* lower left */
  for(j = rsc->ysize - 1, y = ym - 1; y >= 0; j--, y--)
    for(i = rsc->xsize - 1, x = xm - 1; x >= 0; i--, x--){
      sliceGetVal(rsc, i, j, val);
      slicePutVal(ssc, x, y, val);
    }
  /* lower right */
  for(j = rsc->ysize - 1, y = ym - 1; y >= 0; j--, y--)
    for(i = 0, x = xm; i < xm; i++, x++){
      sliceGetVal(rsc, i, j, val);
      slicePutVal(ssc, x, y, val);
    }

  mrc_slice_free(rsc);
  return(ssc);
     
}

/* sin is the slice input, low and high are between (0 - 0.5) */
int mrc_bandpass_filter(struct MRCslice *sin, double low, double high)
{
  int i, j;
  double dist, mval, dx, dy, xscale, xadd, power = 3;
  Ival val;
     
  if (sin->mode != MRC_MODE_COMPLEX_FLOAT){
    b3dError(stderr, " mrc_band_filter: Only complex float mode.\n");
    return(-1);
  }

  /* Set up X coordinate scaling for odd or even (mirrored) FFTs */
  if (sin->xsize % 2) {
    xscale = 0.5 / (sin->xsize - 1.);
    xadd = 0.;
  } else {
    xscale = 1. / sin->xsize;
    xadd = -0.5;
  }

  for (j = 0; j < sin->ysize; j++)
    for(i = 0; i < sin->xsize; i++){
      dx = xscale * i + xadd;

      dy = (float)j / sin->ysize - 0.5;
      dist = sqrt(dx *dx + dy * dy);
      if (low > 0.) {
        if (dist < 0.00001)
          mval = 0;
        else
          mval = 1 / (1 + pow(low / dist, power));
      } else 
        mval = 1.0;
           
      if (high > 0.0)
        mval *= 1 / (1 + pow(dist / high, power));

      sliceGetVal(sin, i, j, val);
      val[0] *= mval;
      val[1] *= mval;
      slicePutVal(sin, i, j, val);
    }
  return(0);
}




/******************************************************/
/* 1D fft four1()                                     */
/* pretty much verbatum from numerical recipes in C   */
/* The only change is support for zero offset arrays. */
/* Press, Flannery, Teukolsky & Vetterling.           */
/* Cambridge University Press.                        */
/* If you want to use this source code you should buy */
/* the book.                                          */
/******************************************************/
void mrc_1dfft(float *data, int nn, int isign)
{
  int n, mmax, m, j, istep, i;
  double wr = 1.0, wi = 0.0;
  double wtemp, wpr, wpi, theta;
  float tempr = 0.0, tempi = 0.0;

  data--;

  n = nn << 1;
  j = 1;
  for ( i = 1; i < n; i+=2){
    if (j > i){
      SWAP(data[j], data[i]);
      SWAP(data[j + 1], data[i + 1]);
    }
    m = n >> 1;
    while (m >= 2 && j > m) {
      j -= m;
      m >>= 1;
    }
    j += m;
  }

  mmax = 2;
  while(n > mmax){
    istep = 2 * mmax;
    theta = TWOPI / (isign*mmax);
    wtemp = sin(0.5*theta);
    wpr = -2.0 * wtemp * wtemp;
    wpi = sin(theta);
    wr=1.0;
    wi=0.0;
    for(m=1;m<mmax;m+=2){
      for(i=m; i<=n; i+=istep){
        j = i + mmax;
        tempr = wr * data[j] - wi * data[j + 1];
        tempi = wr * data[j+1] + wi * data[j];
        data[j] = data[i] - tempr;
        data[j+1] = data[i + 1] - tempi;
        data[i] += tempr;
        data[i+1] += tempi;
      }
      wr = (wtemp=wr) * wpr - wi * wpi + wr;
      wi = wi * wpr + wtemp * wpi + wi;
    }
    mmax = istep;

  }
}


int corr_conj3d(struct MRCvolume *v1, struct MRCvolume *v2)
{
  int k, size;

  if (v1->zsize != v2->zsize)
    return(-1);
  if (v1->vol[0]->mode != MRC_MODE_COMPLEX_FLOAT)
    return(-1);

  size = v1->vol[0]->xsize * v1->vol[0]->ysize;
  for (k = 0; k < v1->zsize; k++)
    corr_conj(v1->vol[k]->data.f, v2->vol[k]->data.f, size);
  return(0);
}

int corr_conj(float *g, float *h, int size)
{
  int i;
  int real, imag;
  float temp, rtmp, itmp;

  for ( i = 0; i < size; i++){
    real = i * 2;
    imag = real + 1;
    temp = g[real];
    rtmp = h[real];
    itmp = h[imag];
    g[real] = rtmp * temp + itmp * g[imag];
    g[imag] = itmp * temp - rtmp * g[imag];
  }
  return(0);
}

