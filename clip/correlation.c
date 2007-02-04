/*
 *  correlation.c -- auto and cross correlation for clip.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 * Log at end
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include "mrcc.h"

#include "clip.h"

double parabolic_fit(double *outX, double *outY, double i[3][3]);
int padfloat_volume(Istack *v, float pad);
int clip_cor_scalevol(Istack *v);


void corr_getmax(Islice *is, int sa, int xm, int ym, float *x, float *y)
{
  unsigned int i, j;
  unsigned int xs, ys;
  Islice *s;
  Ival val;
  float weight, row, ftmp;

  s = sliceBox(is, xm - sa, ym - sa, xm + sa + 1, ym + sa + 1); 
  sliceMMM(s);
  val[0] = -s->min;
  sliceAddConst(s, val);
  sliceMMM(s);
  val[0] = -s->max + (s->max * 0.025f);
  sliceAddConst(s, val); 

  xs = s->xsize;
  ys = s->ysize;
  *x = xm;
  *y = ym;


  row = weight = 0.0f;
  for(i = 0; i < xs; i++)
    for(j = 0; j < ys; j++){
      ftmp = sliceGetPixelMagnitude(s, i, j);

      /*         printf("ftmp %d, %d = %g\n", i, j, ftmp); */
      if (ftmp > 0){
        row += (i + 1) * ftmp;
        weight += ftmp;
      }
    }
  /*     printf("%g / %g = %g\n", row, weight, row/weight); */

  if (weight > 0.0)
    *x = (row/weight) + xm - sa - 1;

  row = weight = 0.0f;
  for(i = 0; i < xs; i++)
    for(j = 0; j < ys; j++){
      ftmp = sliceGetPixelMagnitude(s, i, j);
      if (ftmp > 0){
        row += (j + 1) * ftmp;
        weight += ftmp;
      }
    }
  if (weight > 0.0)
    *y = (row/weight) + ym - sa - 1;



  /* max value */
  /*
    x1 = xmax - 1; x2 = xmax; x3 = xmax + 1;
    y1 = sliceGetPixelMagnitude(v->vol[k], xmax - 1, ymax);
    y2 = sliceGetPixelMagnitude(v->vol[k], xmax, ymax);
    y3 = sliceGetPixelMagnitude(v->vol[k], xmax + 1, ymax);
    a = (y1*(x2-x3)) + (y2*(x3-x1)) + (y3*(x1-x2));
    b = (x1*x1*(y2-y3)) + (x2*x2*(y3-y1)) + (x3*x3*(y1-y2));
    if (a)
    x = -b/(2*a);
    else
    x = xmax;

    x1 = ymax - 1; x2 = ymax; x3 = ymax + 1;
    y1 = sliceGetPixelMagnitude(v->vol[k], xmax, ymax - 1);
    y2 = sliceGetPixelMagnitude(v->vol[k], xmax, ymax);
    y3 = sliceGetPixelMagnitude(v->vol[k], xmax, ymax + 1);
    a = (y1*(x2-x3)) + (y2*(x3-x1)) + (y3*(x1-x2));
    b = (x1*x1*(y2-y3)) + (x2*x2*(y3-y1)) + (x3*x3*(y1-y2));
    if (a)
    y = -b/(2*a);
    else
    y = xmax;
  */  

  return;
}

void clip_padcorr(Islice *s, int pad)
{
  if (!s)
    return;

  if (!pad){
    puts("no padding");
    return;
  }

  if (s->mode == SLICE_MODE_COMPLEX_FLOAT)
    return;

  sliceBoxIn(s, -s->xsize/2, -s->ysize/2, 
             s->xsize + (s->xsize/2) + 2, 
             s->ysize + (s->ysize/2));
  return;
}

/* returns a slice that is the correlation of s1 and s2 */
Islice *clip_slice_corr(Islice *s1, Islice *s2)
{
  Islice *slice;
  Ival val;
  int autocorr = FALSE;

  /*  Check input slices.
   */
  if (!s1)
    return(NULL);
  if (!s2)
    s2 = s1;

  /* Set auto-correlation.
   */
  if (s1 == s2)
    autocorr = TRUE; /* do autocorrelation. */

  /*  If first slice isn't complex compute fft.
   */
  if (s1->mode != SLICE_MODE_COMPLEX_FLOAT){
    sliceFloat(s1);
    sliceMMM(s1);
    val[0] = -s1->mean;
    sliceAddConst(s1, val);
    /* printf("corr: size = %d %d\n", s1->xsize - 2, s1->ysize);*/
    mrcToDFFT(s1->data.f, s1->xsize - 2, s1->ysize, 0);
    s1->xsize /= 2;
  } else if (!(s1->xsize % 2))
    sliceReduceMirroredFFT(s1);
             

  /* Handle the second slice.
   */
  if (!autocorr) {
    if (s2->mode != SLICE_MODE_COMPLEX_FLOAT) {
      sliceFloat(s2);
      mrcToDFFT(s2->data.f, s2->xsize - 2, s2->ysize, 0);
      s2->xsize /= 2;
    } else if (!(s2->xsize % 2))
      sliceReduceMirroredFFT(s2);
  }

  if ((s1->xsize != s2->xsize) || (s1->ysize != s2->ysize)){
    show_error("corr: slices must be same size.\n");
    return(NULL);
  }

  /* create a slice used for returning correlation data.
   */
  slice = sliceCreate(2 * s1->xsize, s1->ysize, SLICE_MODE_FLOAT);
  if (!slice)
    return(NULL);

  /* Calculate the correlation using complex conjugate and
   * calculate the inverse fft.
   */
  corr_conj(s1->data.f, s2->data.f, s1->xsize * s1->ysize);
  mrcToDFFT(s1->data.f, 2*s1->xsize-2, s1->ysize, 1);
  s1->xsize *= 2;

  /* reorder the mangled data. */
  {
    int xm, ym;
    int x, y, i, j;

    xm = slice->xsize / 2;
    ym = slice->ysize / 2;
    for(j = 0, y = ym; j < ym; j++, y++){
      for(i = 0, x = xm; i < xm; i++, x++){
        sliceGetVal(s1, i, j, val);
        slicePutVal(slice, x, y, val);
      }
      for(i = s1->xsize - 3, x = xm - 1; x >= 0; i--, x--){
        sliceGetVal(s1, i, j, val);
        slicePutVal(slice, x, y, val);
      }
    }
      
    for(j = s1->ysize - 1, y = ym - 1; y >= 0; j--, y--){
      i = s1->xsize - 3;
      for(x = xm - 1; x >= 0; i--, x--){
        sliceGetVal(s1, i, j, val);
        slicePutVal(slice, x, y, val);
      }
      for(i = 0, x = xm; i < xm; i++, x++){
        sliceGetVal(s1, i, j, val);
        slicePutVal(slice, x, y, val);
      }
    }
  }

     
  return(slice);
}


/* Does 3d cross-corr. */
int clip_corr3d(MrcHeader *hin1, MrcHeader *hin2,
                MrcHeader *hout, ClipOptions *opt)
{
  FILE *tfp;
  Istack *v2;
  Istack *v1;
  int size,k;
  int xmax, ymax, zmax;
  float min,max,mean;
  float x,y,z;

  ClipOptions opt2;
  memcpy(&opt2, opt, sizeof(ClipOptions));

  if (hin1->mode == MRC_MODE_COMPLEX_FLOAT)
    return(grap_3dcorr(hin1,hin2,hout,opt));
 
  v1 = grap_volume_read(hin1, opt);
  if (!v1)
    return (-1);
  clip_get_stat3d(v1, &min, &max, &mean, &xmax, &ymax, &zmax);
  puts("stats on vol 1:");
  printf("max = %g, min = %g, mean = %g\n", max, min, mean);
  printf("location of max pixel = (%d, %d %d)\n", xmax, ymax, zmax);

  /* Set the type of padding for input. */
  if (opt->val == IP_DEFAULT)
    opt->val = 1;
  if (opt->val == 1)
    padfloat_volume(v1, mean);
  printf("\n");

  if (opt->infiles == 2){
    puts("Cross-Correlation");
    v2 = grap_volume_read(hin2, &opt2);
  if (!v2)
    return (-1);
    clip_get_stat3d(v2, &min, &max, &mean, &xmax, &ymax, &zmax);
    puts("stats on vol 2:");
    printf("max = %g, min = %g, mean = %g\n", max, min, mean);
    printf("location of max pixel = (%d, %d %d)\n", xmax, ymax, zmax);
    if (opt->val == 1)
      padfloat_volume(v2, mean);
    printf("\n");
  }else{
    puts("Auto-Correlation");
    v2 = v1;
  }


  printf("Calculating fft 1");
  fflush(stdout);
  clip_fftvol(v1);
  tfp = hout->fp;
  mrc_head_new(hout, v1->vol[0]->xsize, v1->vol[0]->ysize,
               v1->zsize, v1->vol[0]->mode);
  hout->fp = tfp;

  if (opt->infiles == 2){
    printf("\rCalculating fft 2");
    fflush(stdout);
    clip_fftvol(v2);
  }

  size = v1->vol[0]->xsize * v1->vol[0]->ysize;
  for(k = 0; k < v1->zsize; k++)
    corr_conj(v1->vol[k]->data.f, v2->vol[k]->data.f, size);


  printf("\rCalculating inverse fft");
  fflush(stdout);
  clip_fftvol(v1);
  printf("\n");
  clip_cor_scalevol(v1);

  clip_get_stat3d(v1, &min, &max, &mean, &xmax, &ymax, &zmax);
  clip_parxyz(v1, xmax, ymax, zmax, &x, &y, &z);
  if (x > (float)(v1->vol[0]->xsize/2))
    x -= (float)v1->vol[0]->xsize;
  if (y > (float)(v1->vol[0]->ysize/2))
    y -= (float)v1->vol[0]->ysize;
  if (z > (float)(v1->zsize/2))
    z -= (float)v1->zsize;
  printf("max = %g  min = %g  mean = %g\n",max,min,mean);
  printf("location of max pixel ( %d, %d, %d) is \n", xmax, ymax, zmax);
  printf("( %.2f, %.2f, %.2f)\n", x, y, z);

  mrc_head_new(hout, v1->vol[0]->xsize, v1->vol[0]->ysize,
               v1->zsize, v1->vol[0]->mode);
  mrc_head_label(hout, "Clip: 3D Correlation");
  if (grap_volume_write(v1, hout, opt))
    return (-1);
  grap_volume_free(v1);
  if (opt->infiles == 2)
    grap_volume_free(v2);
  return(0);
}

/*
 * Does a 3D cross or auto correlation on Complex images. 
 * Outputs product.  Inverse fft still needs to be done.
 */
int grap_3dcorr(MrcHeader *hin1, MrcHeader *hin2,
                MrcHeader *hout, ClipOptions *opt)
{
  Islice  *sin1, *sin2;
  int autoc, size;
  int k;

  show_status("Doing 3d correlation...\n");

  if (opt->infiles > 2){
    show_error("3dcorr, Only two input files allowed.\n");
    return(-1);
  }
 
  /* Choose auto or cross correlation. */
  if (opt->infiles == 1)
    autoc = TRUE;
  if (opt->infiles == 2)
    autoc = FALSE;

  if (hin1->mode != MRC_MODE_COMPLEX_FLOAT){
    show_error("corr, Input file must be complex float.\n");
    return(-1);
  }

  if (!autoc){
    if ((hin2->mode != MRC_MODE_COMPLEX_FLOAT) ||
        (hin1->mode != MRC_MODE_COMPLEX_FLOAT)){
      show_error("corr, Both input files must be complex float.\n");
      return(-1);
    }
    if ((hin1->nx != hin2->nx) || (hin1->ny != hin2->ny)){
      show_error("corr, input files must be same size.\n");
      return(-1);
    }
  }

  mrc_head_new(hout, hin1->nx, hin1->ny, hin1->nz, hin1->mode);
  if (autoc)
    mrc_head_label(hout, "clip: Auto  correlation.");
  else
    mrc_head_label(hout, "clip: Cross correlation.");

     
  size = hin1->nx * hin1->ny;
  sin1 = sliceCreate(hin1->nx, hin1->ny, MRC_MODE_COMPLEX_FLOAT);
  if (autoc)
    sin2 = sin1;
  else
    sin2 = sliceCreate(hin1->nx, hin1->ny, MRC_MODE_COMPLEX_FLOAT);

  for(k = 0; k < hin1->nz; k++){
    if (mrc_read_slice((void *)sin1->data.b, hin1->fp, hin1, k, 'z'))
      return -1;
    if (!autoc)
      if (mrc_read_slice((void *)sin2->data.b, hin2->fp, hin2, k, 'z'))
        return -1;
    corr_conj(sin1->data.f, sin2->data.f, size);
    if (mrc_write_slice((void *)sin1->data.b, hout->fp, hout, k, 'z'))
      return -1;
  }


  return mrc_head_write(hout->fp, hout);
}


/*  The main corr driver for clip.
 *
 */

int grap_corr(MrcHeader *hin1, MrcHeader *hin2, 
              MrcHeader *hout, ClipOptions *opt)
{
  Islice  sin1, sin2, *cslice;
  void *buf1, *buf2;
  int autoc;
  int z1 = 0, z2 = 0;
  int llx, lly, urx, ury;

  int i, j;
  int xmax, ymax;
  float x, y, m, max;

  if (opt->dim == 3)
    return(clip_corr3d(hin1, hin2, hout, opt));

  if (opt->infiles > 2){
    show_error("corr, Only two input files are allowed.\n");
    return(-1);
  }

  autoc = TRUE;
  
  if (opt->infiles == 2){
    autoc = FALSE;
  }

  if (opt->nofsecs > 0)
    z1 = opt->secs[0];

  if (opt->nofsecs > 1){
    autoc = FALSE;     
    z2 = opt->secs[1];
  }

  if (opt->val == IP_DEFAULT)
    opt->val = 1;

  if (hin1->mode == MRC_MODE_COMPLEX_FLOAT || 
      hin1->mode == MRC_MODE_COMPLEX_FLOAT) {
    show_error("corr, fourier transform input not allowed");
    return(-1);
  }

  if (opt->add2file){
    if (hout->mode != MRC_MODE_FLOAT){
      show_error("corr, add or append to float file only.\n");
      return(-1);
    }
  }else
    mrc_head_new(hout, hin1->nx, hin1->ny, 1, MRC_MODE_FLOAT);
     
  buf1 = mrc_mread_slice(hin1->fp, hin1, z1, 'z');
  if (!buf1){
    show_error("corr, error getting slice 1.\n");
    return(-1);
  }

  if (!autoc){
    if (opt->infiles == 2)
      buf2 = mrc_mread_slice(hin2->fp, hin2, (int)z2, 'z');
    else
      buf2 = mrc_mread_slice(hin1->fp, hin1, (int)z2, 'z');
    if (!buf2){
      show_error("corr, error getting slice 2.\n");
      return(-1);
    }
    show_status("Clip: Doing 2D correlation...\n");
  }else{
    buf2 = mrc_mread_slice(hin1->fp, hin1, z1, 'z');
    show_status("Clip: Doing 2D auto-correlation...\n");
  }



  if (opt->ix == IP_DEFAULT)
    opt->ix = hin1->nx;
  if (opt->iy == IP_DEFAULT)
    opt->iy = hin1->ny;
  if (opt->cx == IP_DEFAULT)
    opt->cx = hin1->nx / 2;
  if (opt->cy == IP_DEFAULT)
    opt->cy = hin1->ny / 2;
  llx = opt->cx - (opt->ix/2);
  lly = opt->cy - (opt->iy/2);
  urx = llx + opt->ix;
  ury = lly + opt->iy;

  /*     printf(" %d %d %d %d\n", llx, lly, urx, ury); */

  sliceInit(&sin1, hin1->nx, hin1->ny, hin1->mode, buf1);
  sliceMMM(&sin1);
  sliceBoxIn(&sin1, llx, lly, urx, ury);
  sliceMMM(&sin1);

  if (opt->infiles == 2){
    sliceInit(&sin2, hin2->nx, hin2->ny, hin2->mode, buf2);
    sliceMMM(&sin2);
    sliceBoxIn(&sin2, llx, lly, urx, ury);
    sliceMMM(&sin2);
  }else{
    sliceInit(&sin2, hin2->nx, hin2->ny, hin1->mode, buf2);
    sliceMMM(&sin2);
    sliceBoxIn(&sin2, llx, lly, urx, ury);
    sliceMMM(&sin2);  
  }

  /* change padding to given value */
  if (opt->pad != IP_DEFAULT){
    sin1.mean = opt->pad;
    sin2.mean = opt->pad;
  }

  clip_padcorr(&sin1, (int)opt->val);
  clip_padcorr(&sin2, (int)opt->val);

  printf("image 1 size %d by %d\n", sin1.xsize, sin1.ysize);
  printf("image 2 size %d by %d\n", sin2.xsize, sin2.ysize);


  /* todo: add filter option here */

  cslice = clip_slice_corr(&sin1, &sin2);
  llx = (cslice->xsize - 2) / 4;
  lly = cslice->ysize / 4;
  urx = llx + ((cslice->xsize - 2) / 2);
  ury = lly + (cslice->ysize / 2);
  sliceBoxIn(cslice, llx, lly, urx, ury);
  sliceMMM(cslice);
  printf("image c size %d by %d\n", cslice->xsize, cslice->ysize);
     

  if (opt->add2file == IP_APPEND_ADD){
    sliceResizeIn(cslice, hout->nx, hout->ny);
    hout->nz++;
    if (mrc_write_slice((void *)cslice->data.f, hout->fp, hout, hout->nz - 1,
                        'z'))
      return -1;
    if (cslice->min < hout->amin)
      hout->amin = cslice->min;
    if (cslice->max > hout->amax)
      hout->amax = cslice->max;
    hout->amean *= hout->nz - 1;
    hout->amean += cslice->mean;
    hout->amean /= hout->nz;
    if (mrc_head_write(hout->fp, hout))
      return -1;
  }else if (opt->add2file == IP_APPEND_OVERWRITE){
    sliceResizeIn(cslice, hout->nx, hout->ny);
    if (mrc_write_slice((void *)cslice->data.f, hout->fp, hout, hout->nz - 1,
                        'z'))
      return -1;
  }else{
    /* handle ox oy flags */
    hout->nz    = 1;
    hout->amin  = cslice->min;
    hout->amax  = cslice->max;
    hout->amean = cslice->mean;
    if ((opt->ox != IP_DEFAULT) || (opt->oy != IP_DEFAULT)){
      if (opt->ox != IP_DEFAULT)
        opt->ox = cslice->xsize;
      if (opt->oy != IP_DEFAULT)
        opt->oy = cslice->ysize;
      sliceResizeIn(cslice, opt->ox, opt->oy);
      hout->nx = opt->ox;
      hout->ny = opt->oy;
    }else{
      hout->nx = cslice->xsize;
      hout->ny = cslice->ysize;
    }
           
    mrc_head_label(hout, "clip: 2d correlation calculated.");
    if (mrc_head_write(hout->fp, hout))
      return -1;
    if (mrc_write_slice((void *)cslice->data.f, hout->fp, hout, 0, 'z'))
      return -1;
  }


    
  /* now find the exact peak location */
  xmax = ymax = 0;
  max = sliceGetPixelMagnitude(cslice, 0, 0);
  
  for(j = 0; j < cslice->ysize; j++)
    for(i = 0; i < cslice->xsize; i++){
      m = sliceGetPixelMagnitude(cslice, i, j);
      if (m > max){
        max = m;
        xmax = i;
        ymax = j;
      }
    }

  printf("pixel max at ( %d, %d)\n", xmax, ymax);

#define USE_PARABOLIC_FIT
  /*  Center of gravity.
   */
#ifdef USE_CENTER_OF_GRAVITY_FIT
  corr_getmax(cslice, 19, xmax, ymax, &x, &y);
#endif

#ifdef USE_PARABOLIC_FIT
  /*
   * Parabolic fit.
   */
  {
    int di,dj;
    double data[3][3];
    double cx, cy;

    for (dj = 0, j = ymax - 1; j <= ymax + 1; j++, dj++)
      for(di = 0,i = xmax - 1; i <= xmax + 1; i++ , di++)
        data[dj][di] =
          sliceGetPixelMagnitude(cslice, i, j);

    parabolic_fit(&cx, &cy, data);
    x = cx + xmax;
    y = cy + ymax;
  }
#endif

  x -= (float)cslice->xsize * 0.5f;
  x -= 1.0f;
  y -= (float)cslice->ysize * 0.5f;
     
  /* Don't change format: shell scripts need it like this. */
  printf("Maximum at ( %.2f, %.2f), transformation ( %.2f, %.2f)\n", 
         x, y, -x, -y);

  sliceFree(cslice);
  free(sin1.data.b);
  free(sin2.data.b);
  return(0);
}

int padfloat_volume(Istack *v, float pad)
{
  Islice **vol;
  int i,k;
  int zsize, xysize;
  int xsize, ysize;
  int zh,zl;

  zl = v->zsize/2;
  zh = zl + v->zsize;
  zsize = v->zsize*2;
  xsize = v->vol[0]->xsize * 2;
  ysize = v->vol[0]->ysize * 2;
  xysize = xsize * ysize;
  vol = (Islice **)malloc(sizeof(Islice *) * zsize);

  for(k = 0; k < v->zsize; k++){
    v->vol[k]->mean = pad;
    sliceResizeIn(v->vol[k], xsize, ysize);
    sliceFloat(v->vol[k]);
  }
  for(k = 0; k < zl; k++){
    vol[k] =  sliceCreate(xsize, ysize, SLICE_MODE_FLOAT);
    for(i = 0; i < xysize; i++)
      vol[k]->data.f[i] = pad;
  }
  while(k < zh){
    vol[k] = v->vol[k-zl];
    k++;
  }
  while(k < zsize){
    vol[k] =  sliceCreate(xsize, ysize,
                          SLICE_MODE_FLOAT);
    for(i = 0; i < xysize; i++)
      vol[k]->data.f[i] = pad;
    k++;
  }
  free(v->vol);
  v->vol = vol;
  v->zsize = zsize;
  return(0);
}

int clip_cor_scalevol(Istack *v)
{

  float scale;
  int  xysize;
  int  zsize;
  int  i,k;

  xysize = v->vol[0]->xsize * v->vol[0]->ysize;
  zsize = v->zsize;
  scale = xysize * zsize;

  for(k = 0; k < zsize; k++)
    for(i = 0; i < xysize;i++)
      v->vol[k]->data.f[i] /= scale;
  return(0);

}


/*
 * Double parabolic fit in 3-d.
 */
double parabolic_fit(double *outX, double *outY, double i[3][3])
{
  double c, y, yy, x, xy, xx;
  double xc = 0.0, yc = 0.0;
  double p, d;
    
  c  = (26.0*i[0][0] - i[1][0] + 2*i[2][0] - i[0][1] - 19.0*i[1][1]
        -7.0*i[2][1] + 2.0*i[0][2] - 7.0*i[1][2] + 14.0*i[2][2])/9.0;
  y  = (8.0*i[0][0] - 8.0*i[1][0] + 5.0*i[0][1] - 8.0*i[1][1] 
        + 3.0*i[2][1] + 2.0*i[0][2] - 8.0*i[1][2] + 6.0*i[2][2]) / -6.0;
  yy = (i[0][0] - 2.0*i[1][0] + i[2][0] + i[0][1] - 2.0*i[1][1]
        + i[2][1] + i[0][2] - 2.0*i[1][2] + i[2][2]) / 6.0;
  x  = (8.0*i[0][0] + 5.0*i[1][0] + 2.0*i[2][0] - 8.0*i[0][1]
        - 8.0*i[1][1] - 8.0*i[2][1] + 3.0*i[1][2] + 6.0*i[2][2])/-6.0;
  xy = (i[0][0] - i[2][0] - i[0][2] + i[2][2])/4.0;
  xx = (i[0][0] + i[1][0] + i[2][0] - 2.0*i[0][1] - 2.0*i[1][1]
        - 2.0*i[2][1] + i[0][2] + i[1][2] + i[2][2])/6.0;

  /* check it out. */
  /*    printf("coeffs %g %g %g %g %g %g\n",
   *     c, y, yy, x, xy, xx);
   */

  if (0.0 == (d  = 4.0 * yy * xx - xy * xy))
    return(i[1][1]);
  p = (4.0*c*yy*xx - c*xy*xy - y*y*xx + y*x*xy - x*x*yy)/d;
  yc = (x*xy - 2.0*y*xx) / d-2.0;
  xc = (y*xy - 2.0*x*yy) / d-2.0;

  if (yc < -1.0) yc = -1.0;
  if (yc >  1.0) yc =  1.0;
  if (xc < -1.0) xc = -1.0;
  if (xc >  1.0) xc =  1.0;
  *outX = xc;
  *outY = yc;
  return(p);
}

/*
$Log$
Revision 3.6  2005/01/17 17:09:24  mast
Converted to new typedefs

Revision 3.5  2005/01/12 22:34:28  mast
Really fixed it this time

Revision 3.4  2005/01/12 22:09:08  mast
Fixed slice correlation, size was not passed correctly to todfft

*/
