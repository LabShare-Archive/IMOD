/*
 *  mrcslice.c -- Library of image slice functions.
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

Log at end of file */

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "mrcc.h"
#include "b3dutil.h"

/*****************************************************************************
 * Slice library functions:
 *
 * int sliceMode(char *mst); 
 *      returns mode number from mode string.
 * int sliceInit(Islice *s, int xsize, int ysize, int mode, void *data);
 *      initialize a slice to the given values.
 * Islice *sliceCreate(int xsize, int ysize, int mode);
 *      create a slice with the given values.
 * void sliceFree(Islice *s);
 *      free a slice created with sliceCreate().
 * void sliceClear(Islice *sl, Ival val);
 *      clear a slice to a given value.
 * 
 * int slicePutVal(Islice *s, int x, int y, Ival val);
 *      Put a pixel value into a slice. returns non-0 for error.
 * int sliceGetVal(Islice *s, int x, int y, Ival val);
 *      Get a pixel value from a slice. returns non-0 for error.
 * float sliceGetPixelMagnitude(Islice *s, int x, int y);
 * float sliceGetValMagnitude(Ival val, int mode);
 *
 * int sliceNewMode(Islice *sl, int mode);
 *      Convert slice to new data type.
 * int sliceFloat(Islice *slice);
 * int sliceComplexFloat(Islice *slice);
 *      Convert a slice in place to the given mode. returns non-0 for error.
 *
 * void sliceMMM(Islice *slice);
 *      Calculate Min, Max and Mean for a slice.
 *
 * Islice *sliceBox(Islice *sl, int llx, int lly, int urx, int ury);
 *      Box out an image with lower x,y upper x,y coords and return the slice.
 * int sliceBoxIn(Islice *sl, int llx, int lly, int urx, int ury);
 *      Box out an image in place with lower x,y upper x,y coords. return 0 ok.
 * int sliceResizeIn(Islice *sl, int x, int y);
 *      Resize a slice in place, centered.
 * Islice *sliceGradient(Islice *sin);
 *      Return gradient of input slice.
 *
 * Islice *sliceReadMRC(MrcHeader *hin, int sno, char axis);
 * Islice *sliceReadSubm(MrcHeader *hin, 
 *            int sno, char axis,
 *            int s1, int s2, int c1, int c2);
 *      Read in a sub area, sno = section number; axis = x,y or z; 
 *      s1 & s2 = size; c1 & c2 = center.
 *
 *****************************************************************************/

int sliceMode(char *mst)
{
  if (strcmp(mst, "byte") == 0)
    return(SLICE_MODE_BYTE);
  if (strcmp(mst, "0") == 0)
    return(SLICE_MODE_BYTE);
  if (strcmp(mst, "short") == 0)
    return(SLICE_MODE_SHORT);
  if (strcmp(mst, "1") == 0)
    return(SLICE_MODE_SHORT);
  if (strcmp(mst, "float") == 0)
    return(SLICE_MODE_FLOAT);
  if (strcmp(mst, "2") == 0)
    return(SLICE_MODE_FLOAT);
  if (strcmp(mst, "3") == 0)
    return(SLICE_MODE_COMPLEX_SHORT);
  if (strcmp(mst, "complex") == 0)
    return(SLICE_MODE_COMPLEX_FLOAT);
  if (strcmp(mst, "4") == 0)
    return(SLICE_MODE_COMPLEX_FLOAT);
  if (strcmp(mst, "16") == 0)
    return(SLICE_MODE_RGB);
  if (strcmp(mst, "rgb") == 0)
    return(SLICE_MODE_RGB);
  return(SLICE_MODE_UNDEFINED);
}

/* Initialize elements in a slice to input values. */
int mrc_slice_init(Islice *s,  int xsize, int ysize,
                   int mode, void *data)
{ return(sliceInit(s,xsize,ysize,mode,data)); }

int sliceInit(Islice *s, int xsize, int ysize, int mode, void *data)
{
  s->xsize = xsize;
  s->ysize = ysize;
  s->mode  = mode;

  switch(mode){
  case MRC_MODE_BYTE:
    s->csize = 1;
    s->dsize = 1;
    s->data.b = data;
    break;
  case MRC_MODE_SHORT:
    s->dsize = sizeof(short);
    s->csize = 1;
    s->data.s = data;
    break;
  case MRC_MODE_FLOAT:
    s->dsize = sizeof(float);
    s->csize = 1;
    s->data.f = data;
    break;
  case MRC_MODE_COMPLEX_SHORT:
    s->dsize = sizeof(short);
    s->csize = 2;
    s->data.s = data;
    break;
  case MRC_MODE_COMPLEX_FLOAT:
    s->dsize = sizeof(float);
    s->csize = 2;
    s->data.f = data;
    break;
  case MRC_MODE_RGB:
    s->csize = 1;
    s->dsize = 3;
    s->data.b = data;
    break;
  case SLICE_MODE_MAX:
    s->csize = SLICE_MAX_CSIZE;
    s->dsize = SLICE_MAX_DSIZE;
    s->data.f = data;
    break;
  default:
    return(-1);
  }
  return(0);
}

/* creates a slice and returns a pointer to it. */
Islice *sliceCreate(int xsize, int ysize, int mode)
{
  return(mrc_slice_create(xsize,ysize,mode));
}

Islice *mrc_slice_create(int xsize, int ysize, int mode)
{
  Islice *s;

  s = (Islice *)malloc(sizeof(Islice));
  if (!s)
    return(NULL);
     
  s->xsize = xsize;
  s->ysize = ysize;
  s->mode  = mode;
  s->index = -1;

  switch(mode){
      
  case MRC_MODE_BYTE:
    s->dsize = sizeof(unsigned char);
    s->csize = 1;
    s->data.b = (unsigned char *)malloc
      (s->xsize * s->ysize * s->dsize * s->csize);
    break;

  case MRC_MODE_SHORT:
    s->dsize = sizeof(short);
    s->csize = 1;
    s->data.s = (short *)
      malloc(s->xsize * s->ysize * s->dsize * s->csize);
    break;
      
  case MRC_MODE_FLOAT:
    s->dsize = sizeof(float);
    s->csize = 1;
    s->data.f = (float *)
      malloc(s->xsize * s->ysize * s->dsize * s->csize);
    break;

  case MRC_MODE_COMPLEX_SHORT:
    s->dsize = sizeof(short);
    s->csize = 2;
    s->data.s = (short *)
      malloc(s->xsize * s->ysize * s->dsize * s->csize);
    break;

  case MRC_MODE_COMPLEX_FLOAT:
    s->dsize = sizeof(float);
    s->csize = 2;
    s->data.f = (float *)
      malloc(s->xsize * s->ysize * s->dsize * s->csize);
    break;

  case MRC_MODE_RGB:
    s->dsize = sizeof(unsigned char);
    s->csize = 3;
    s->data.b = (unsigned char *)
      malloc(s->xsize * s->ysize * s->dsize * s->csize);
    break;

  case SLICE_MODE_MAX:
    s->csize = SLICE_MAX_CSIZE;
    s->dsize = SLICE_MAX_DSIZE;
    s->data.f = (float *)malloc
      (s->xsize * s->ysize * s->dsize * s->csize);
    break;

  default:
    free(s);
    return(NULL);
  }

  if (!s->data.b){
    b3dError(stderr, "mrc_slice_create: not enough memory.\n");
    free(s);
    return(NULL);
  }

  return(s);
}

void sliceFree(Islice *s)
{
  mrc_slice_free(s);
}

int mrc_slice_free(Islice *s)
{
  if (!s)
    return(-1);
  if (s->data.b)
    free(s->data.b);
  free(s);
  return(0);
}

void sliceClear(Islice *s, Ival val)
{
  unsigned int i, j;
     
  s->min = s->max = s->mean = sliceGetValMagnitude(val, s->mode);
  for (j = 0; j < s->ysize; j++)
    for(i = 0; i < s->xsize; i++)
      slicePutVal(s, i, j, val);
  return;
}


int mrc_slice_putval(Islice *s, int x, int y, float *val)
{ return(slicePutVal(s,x,y,val)); }

int slicePutVal(Islice *s, int x, int y, Ival val)
{
  int index = x + (y * s->xsize);

  if ( (x < 0) || (y < 0) || (x >= s->xsize) || (y >= s->ysize))
    return(-1);

  switch (s->mode)
    {
    case MRC_MODE_BYTE:
      s->data.b[index] = (unsigned char)val[0];
      break;
    case MRC_MODE_SHORT:
      s->data.s[index] = (short)val[0];
      break;
    case MRC_MODE_FLOAT:
      s->data.f[index] = val[0];
      break;
    case MRC_MODE_COMPLEX_SHORT:
      index *= 2;
      s->data.s[index]     = (short)val[0];
      s->data.s[index + 1] = (short)val[1];
      break;
    case MRC_MODE_COMPLEX_FLOAT:
      index *= 2;
      s->data.f[index]     = val[0];
      s->data.f[index + 1] = val[1];
      break;
    case MRC_MODE_RGB:
      index *= 3;
      s->data.b[index++] = val[0];
      s->data.b[index++] = val[1];
      s->data.b[index]   = val[2];
      break;
    case SLICE_MODE_MAX:
      index *= 3;
      s->data.f[index++] = val[0];
      s->data.f[index++] = val[1];
      s->data.f[index]   = val[2];
      break;
    default:
      b3dError(stderr, "slicePutVal: unknown mode.\n");
      return(-1);
    }
  return(0);
}

int mrc_slice_getval(Islice *s, int x, int y, float *val)
{ return(sliceGetVal(s,x,y,val)); }

int sliceGetVal(Islice *s, int x, int y, Ival val)
{
  int index = x + (y * s->xsize);
     
  if ( (x < 0) || (y < 0) || (x >= s->xsize) || (y >= s->ysize)){
    val[0] = s->mean;
    return(-1);
  }

  val[1] = 0;
  switch (s->mode)
    {
    case MRC_MODE_BYTE:
      val[0] = (float)s->data.b[index];
      break;
    case MRC_MODE_SHORT:
      val[0] = (float)s->data.s[index];
      break;
    case MRC_MODE_FLOAT:
      val[0] = s->data.f[index];
      break;
    case MRC_MODE_COMPLEX_SHORT:
      index *= 2;
      val[0] = (float)s->data.s[index];
      val[1] = (float)s->data.s[index + 1];
      break;
    case MRC_MODE_COMPLEX_FLOAT:
      index *= 2;
      val[0] = s->data.f[index];
      val[1] = s->data.f[index + 1];
      break;
    case MRC_MODE_RGB:
      index *= 3;
      val[0] = (float)s->data.b[index++];
      val[1] = (float)s->data.b[index++];;
      val[2] = (float)s->data.b[index];
      break;
    case SLICE_MODE_MAX:
      index *= 3;
      val[0] = s->data.f[index++];
      val[1] = s->data.f[index++];
      val[2] = s->data.f[index];
      break;
    default:
      b3dError(stderr, "sliceGetVal: unknown mode.\n");
      return(-1);
    }
  return(0);
}

Islice *sliceGradient(Islice *sin)
{
  Islice *s;
  int i, j;
  Ival val, nval, gval;

  s = sliceCreate(sin->xsize, sin->ysize, sin->mode);
  if (!s)
    return(NULL);

  for(j = 0; j < sin->ysize; j++){
    for(i = 0; i < sin->xsize - 1; i++){
      sliceGetVal(sin, i, j, val);
      sliceGetVal(sin, i+1, j, nval);
      val[0] = nval[0] - val[0];
      if (val[0] < 0)
        val[0] *= -1;
      slicePutVal(s, i, j, val);
    }
  }
  for(i = 0; i < sin->xsize; i++){
    for(j = 0; j < sin->ysize - 1; j++){
      sliceGetVal(sin, i, j, val);
      sliceGetVal(sin, i, j + 1, nval);
      sliceGetVal(s, i, j, gval);
      val[0] = nval[0] - val[0];
      if (val[0] < 0)
        val[0] *= -1;
      gval[0] = (val[0] + gval[0]) / 2;
      slicePutVal(s, i, j, gval);
    }
    sliceGetVal(s, i, j - 1, val);
    slicePutVal(s, i, j, val);
  }
  mrc_slice_calcmmm(s);
  return(s);
}

/****************************************************************************/



/* reorders data so that fft center is at (datasize/2 <-> 0 )   */
int mrc_slice_wrap(Islice *s)
{
  int i,j;
  int mx, my;
  Ival val, tval;

  mx = s->xsize / 2;
  my = s->ysize / 2;
     
  for (j = 0; j < my; j++){
    for (i = 0; i < mx; i++){
      fflush(stdout);
      sliceGetVal(s, i, j, val);
      sliceGetVal(s, i + mx, j + my, tval);
      slicePutVal(s, i, j, tval);
      slicePutVal(s, i + mx, j + my, val);
    }
    for( i = mx; i < s->xsize; i++){
      sliceGetVal(s, i, j, val);
      sliceGetVal(s, i - mx, j + my, tval);
      slicePutVal(s, i, j, tval);
      slicePutVal(s, i - mx, j + my, val);
    }
  }
  return(0);
}

/* Wraps lines of an FFT to bring origin to center line or back */
int sliceWrapFFTLines(Islice *s)
{
  int pixsize, i, ind1, ind2;
  unsigned char *buf;
  int nx = s->xsize;
  int ny = s->ysize;
    
  if (s->mode == MRC_MODE_COMPLEX_FLOAT)
    pixsize = 8;
  else if (s->mode == MRC_MODE_COMPLEX_SHORT)
    pixsize = 4;
  else
    return 1;

  buf = (unsigned char *)malloc(pixsize * nx);
  if (!buf)
    return 2;

  for (i = 0; i < ny / 2; i++) {
    ind1 = i * nx * 2;
    ind2 = (i + ny / 2) * nx * 2;
    memcpy(buf, &s->data.f[ind1], pixsize * nx);
    memcpy(&s->data.f[ind1], &s->data.f[ind2], pixsize * nx);
    memcpy(&s->data.f[ind2], buf, pixsize * nx);
  }
  free(buf);
  return(0);
}

/* Convert an old mirrored FFT to nx/2 + 1 length */
int sliceReduceMirroredFFT(Islice *s)
{
  int nfloats, i, j;
  int nx = s->xsize;
  float tmp1, tmp2;

  if (s->mode != MRC_MODE_COMPLEX_FLOAT)
    return 1;
  nfloats = nx + 2;
  for (j = 0; j < s->ysize; j++) {
    tmp1 = s->data.f[j * 2 * nx];
    tmp2 = s->data.f[j * 2 * nx + 1];
    for (i = 0; i < nx; i++)
      s->data.f[i + j * nfloats] = s->data.f[i + nx + j * 2 * nx];
    s->data.f[nx +  j * nfloats] = tmp1;
    s->data.f[nx + 1 + j * nfloats] = tmp2;
  }
  s->xsize = nx /2 + 1;
  return 0;
}

/* returns a float slice that is the real part of an image for complex data. */
Islice *mrc_slice_real(Islice *sin)
{
  Islice *sout;
  int i, xysize;
      
  if (sin->mode != MRC_MODE_COMPLEX_FLOAT)
    return(sin);

  sout = mrc_slice_create(sin->xsize, sin->ysize, MRC_MODE_FLOAT);
  xysize = sin->xsize * sin->ysize;
  for(i = 0; i < xysize; i++){
    sout->data.f[i] = sin->data.f[i * 2];
  }
  return(sout);
}

/* multiplies all values in a slice by scale. */
int mrc_slice_valscale(Islice *s, double inScale)
{
  int i, j;
  Ival val;
  float scale = inScale;
  val[0] = val[1] = val[2] = 0.0f;

  for (j = 0; j < s->ysize; j++){
    for (i = 0; i < s->xsize; i++){
      fflush(stdout);
      sliceGetVal(s, i, j, val);
      val[0] *= scale;
      val[1] *= scale;
      val[2] *= scale;
      slicePutVal(s, i, j, val); 

    }
  }
  return(0);
}


void sliceMMM(Islice *slice)
{
  mrc_slice_calcmmm(slice);
}

int mrc_slice_calcmmm(Islice *s)
{
  int i, j;
  Ival val;
  double tsum, sum;
  float temp;

  /* DNM 3/29/01: need to take magnitude for complex, and set mean to val */
  sliceGetVal(s, 0, 0, val);
  if (s->mode == MRC_MODE_COMPLEX_FLOAT){
    temp = (val[0] * val[0]) + (val[1] * val[1]);
    val[0] = (float)sqrt(temp);
  }
  s->max = s->min = val[0];

  /* 5/23/05: Huh?  sum needs to be 0 */
  sum = 0.;

  if ((!s->xsize) || (!s->ysize)){
    b3dError(stderr, 
             "mrc_slice_calcmmm: Warning, empty slice.\n");
    return(-1);
  }

  for (j = 0; j < s->ysize; j++) {
    tsum = 0.;
    for (i = 0; i < s->xsize; i++){
      sliceGetVal(s, i, j, val);
           
      if (s->mode == MRC_MODE_COMPLEX_FLOAT){
        temp = (val[0] * val[0]) + (val[1] * val[1]);
        val[0] = (float)sqrt(temp);
      }

      if (s->min > val[0])
        s->min = val[0];
      if (s->max < val[0])
        s->max = val[0];
      tsum += val[0];
    }
    sum += tsum;
  }
  s->mean = sum / (float)(s->xsize * s->ysize);
  return(0);
}




Islice *mrc_slice_rotate(Islice *slin, double angle,
                                  int xsize, int ysize,
                                  double cx, double cy)
{
  int i, j;
  Islice *sout;
  Ival val;
  double x, y;
  double sino, coso;
  double x2, y2;

  sout = mrc_slice_create(xsize, ysize, slin->mode);
  if (sout == NULL)
    return(NULL);
     
  angle *= 0.017453293;
 
  coso = cos(-angle);
  sino = sin(-angle);
  x2 = (double)xsize * 0.5;
  y2 = (double)ysize * 0.5;

  for(j = 0; j < ysize; j++)
    for (i = 0; i < xsize; i++){
      x = (((double)i - x2) * coso) - (((double)j - y2) * sino) + cx;
      y = (((double)i - x2) * sino) + (((double)j - y2) * coso) + cy;
      mrc_slice_interpolate(slin, x, y, val);
      slicePutVal(sout, i, j, val);
    }
  return(sout);
}

int mrc_slice_rotates(Islice *slin, Islice *sout,
                      double angle, int xsize, int ysize,
                      double cx, double cy)
{
  int i, j;
  Ival val;
  double x, y;
  double sino, coso;
  double x2, y2;

  angle *= 0.017453293;
 
  coso = cos(-angle);
  sino = sin(-angle);
  x2 = (double)xsize * 0.5;
  y2 = (double)ysize * 0.5;

  for(j = 0; j < ysize; j++)
    for (i = 0; i < xsize; i++){
      x = (((double)i - x2) * coso) - (((double)j - y2) * sino) + cx;
      y = (((double)i - x2) * sino) + (((double)j - y2) * coso) + cy;
      mrc_slice_interpolate(slin, x, y, val);
      slicePutVal(sout, i, j, val);
    }
  return(0);
}

int sliceMirror(Islice *s, char axis)
{
  int i, j;
  Ival val1;
  Ival val2;
  int lim;

  if (axis == 'x'){
    lim = s->ysize / 2;
    for(j = 0; j < lim; j++){
      for(i = 0; i < s->xsize; i++){
        sliceGetVal(s, i, j, val1);
        sliceGetVal(s, i, s->ysize - j - 1, val2);
        slicePutVal(s, i, j, val2);
        slicePutVal(s, i, s->ysize - j - 1, val1);
      }
    }
    return(0);
  }
  if (axis == 'y'){
    lim = s->xsize / 2;
    for(i = 0; i < lim; i++)
      for(j = 0; j < s->ysize; j++){
        sliceGetVal(s, i, j, val1);
        sliceGetVal(s, s->xsize - 1 - i, j, val2);
        slicePutVal(s, i, j, val2);
        slicePutVal(s, s->xsize - 1 - i, j, val1);
      }
    return(0);
  }
  return(-1);
}

Islice *mrc_slice_translate(Islice *sin,
                                     double dx, double dy,
                                     int xsize, int ysize)
{
  int i, j;
  Islice *sout;
  Ival val;
  double x, y;

  sout = mrc_slice_create(xsize, ysize, sin->mode);

  if (sout == NULL)
    return(NULL);
     
  for(j = 0; j < ysize; j++){
    y = (double)j + dy;
    for (i = 0; i < xsize; i++){
      x = (double)i + dx;
      mrc_slice_interpolate(sin, x, y, val);
      slicePutVal(sout, i, j, val);
    }
  }
  return(sout);
}

Islice *mrc_slice_zoom(Islice *sin,
                                double xz, double yz, 
                                int xsize, int ysize,
                                double cx,    double cy)
{
  int i, j;
  Islice *sout;
  Ival val;
  double x, y;
  double sbx, sby;

  if ((!xz) || (!yz))
    return(NULL);

  sbx = (xz * cx) - ((double)xsize * 0.5);
  sby = (yz * cy) - ((double)ysize * 0.5);

  sout = sliceCreate(xsize, ysize, sin->mode);
  if (sout == NULL)
    return(NULL);

  for(j = 0; j < ysize; j++){
    y = ((double)j / yz) + sby;
    for (i = 0; i < xsize; i++){
      x = ((double)i / xz) + sbx;
      sliceQuadInterpolate(sin, x, y, val);
      slicePutVal(sout, i, j, val);
    }
  }
  return(sout);
}


int mrc_slice_zooms(Islice *sin, Islice *sout,
                    double xz, double yz, 
                    int xsize, int ysize,
                    double cx,    double cy)
{
  int i, j;
  Ival val;
  double x, y;
  double sbx, sby;

  if ((!xz) || (!yz))
    return(0);

  sbx = (xz * cx) - ((double)xsize * 0.5);
  sby = (yz * cy) - ((double)ysize * 0.5);

  for(j = 0; j < ysize; j++){
    y = ((double)j / yz) + sby;
    for (i = 0; i < xsize; i++){
      x = ((double)i / xz) + sbx;
      sliceQuadInterpolate(sin, x, y, val);
      slicePutVal(sout, i, j, val);
    }
  }
  return(0);
}


Islice *sliceBox(Islice *sl, int llx, int lly, int urx, int ury)
{
  Islice *sout;
  int i, j, x, y;
  int nx, ny;
  Ival val;

  nx = urx-llx;
  ny = ury-lly;

  sout = sliceCreate(nx, ny, sl->mode);
  if (!sout)
    return(NULL);

  for(j = lly, y = 0; y < ny; y++, j++)
    for(i = llx, x = 0; x < nx; x++, i++){
      sliceGetVal(sl, i, j, val);
      slicePutVal(sout, x, y, val);
    }
  return(sout);
}

int sliceBoxIn(Islice *sl, int llx, int lly, int urx, int ury)
{
  Islice *sout;

  sout = sliceBox(sl, llx, lly, urx, ury);
  if (!sout)
    return(-1);
  if (sl->data.b)
    free(sl->data.b);
  memcpy(sl, sout, sizeof(Islice));
  free(sout);
  return(0);
}

int sliceResizeIn(Islice *sl, int x, int y)
{
  int llx, lly, urx, ury;
  int cx, cy;
     
  if ((sl->xsize == x) && (sl->ysize == y))
    return(0);

  cx = sl->xsize / 2;
  cy = sl->ysize / 2;
  llx = cx - (x/2);
  lly = cy - (y/2);
  urx = llx + x;
  ury = lly + y;
  return(sliceBoxIn(sl, llx, lly, urx, ury));
}


Islice *mrc_slice_resize(Islice *slin,
                                  int nx, int ny)
{
  Islice *sout;
  int i, j, x, y;
  int sx, sy;
  Ival pval, val;

  pval[0] = slin->mean;
  pval[1] = slin->mean;
  pval[2] = slin->mean;

  sout = mrc_slice_create(nx, ny, slin->mode);
  if (!sout)
    return(sout);

  if ((nx > slin->xsize) || (ny > slin->ysize))
    for (j = 0; j < ny; j++)
      for(i = 0; i < nx; i++)
        slicePutVal(sout, i, j, val);

  sx = (slin->xsize - nx) / 2;
  sy = (slin->ysize - ny) / 2;

  for(j = 0, y = sy; j < ny; j++, y++)
    for(i = 0, x = sx; i < nx; i++, x++){
           
      if ( (x < 0) || (y < 0) || 
           (x >= slin->xsize) || (y >= slin->ysize )   )
        slicePutVal(sout, i, j, pval);
      else{
        sliceGetVal(slin, x, y, val);
        slicePutVal(sout, i, j, val);
      }
    }
  return(sout);
}



int mrc_slice_interpolate(Islice *sin,
                          double x, double y, float *val)
{
  int channel;
  unsigned char *buf = sin->data.b;

  for( channel = 0; channel < sin->csize; channel++, buf+=sin->dsize)
    mrc_bilinear(&(val[channel]), x, y, buf, 
                 sin->xsize, sin->ysize, sin->mode);
  return(0);
}

void sliceQuadInterpolate(Islice *sl, double x, double y, Ival val)
{
  int xi, yi;    /* nearest integer value to x, y */
  float dx, dy;  /* difference between nearest int val and actual value. */
  float a,b,c,d; /* coeffs for quad. */
  int i = 0;
  double step = 0.5;
  Ival x1,x2,y1,y2;

  xi = x + step;
  yi = y + step;

  dx = x - xi;
  dy = y - yi;
     
  sliceGetVal(sl, xi, yi, val);
  sliceGetVal(sl, xi - 1, yi, x1);
  sliceGetVal(sl, xi + 1, yi, x2);
  sliceGetVal(sl, xi, yi - 1, y1);
  sliceGetVal(sl, xi, yi + 1, y2);

  a = (x1[i] + x2[i]) * 0.5f - (float)val[i];
  b = (y1[i] + y2[i]) * 0.5f - (float)val[i];
  c = (x2[i] - x1[i]) * 0.5f;
  d = (y2[i] - y1[i]) * 0.5f;
  val[i] = (a * dx * dx) + (b * dy * dy) + (c * dx) + (d * dy) + val[i];
  if (sl->csize < 2)
    return;

  while(i++ < sl->csize){
    a = (x1[i] + x2[i]) * 0.5f - (float)val[i];
    b = (y1[i] + y2[i]) * 0.5f - (float)val[i];
    c = (x2[i] - x1[i]) * 0.5f;
    d = (y2[i] - y1[i]) * 0.5f;
    val[i] = (a * dx * dx) + (b * dy * dy) + (c * dx)+(d * dy) + val[i];
  }
  return;
}

/* Extract a dim x dim matrix from the input slice centered around x,y; place
   result into mat */
void mrc_slice_mat_getimat(Islice *sin, int x, int y, int dim, 
                           float *mat)
{
  int xs, ys, xe, ye;
  int i, j;


  xs = x - (dim / 2);
  xe = xs + dim;
  ys = y - (dim / 2);
  ye = ys + dim;

  for(j = ys; j < ye; j++)
    for(i = xs; i < xe; i++)
      mat[(i-xs )+ dim * (j-ys)] = mrc_slice_getmagnitude(sin, i, j);
}

/* Multiply two dim x dim matrices by simple vector product */
float mrc_slice_mat_mult(float *m1, float *m2, int dim)
{
  float rval = 0;
  int i, elements;

  elements = dim * dim;

  for(i = 0; i < elements; i++)
    rval += m1[i] * m2[i];

  return(rval);
}

/* Filter a slice by convolving with a dim x dim matrix; returns float slice */
Islice *slice_mat_filter(Islice *sin, float *mat, int dim)
{
  Islice *sout;
  float *imat;
  Ival val;
  int i,j;

  imat = (float *)malloc(dim * dim * sizeof(float));
  if (!imat)
    return(NULL);
  sout = mrc_slice_create(sin->xsize, sin->ysize, MRC_MODE_FLOAT);
  if (!sout)
    return NULL;

  for(j = 0; j < sin->ysize; j++){
    for(i = 0; i < sin->xsize; i++){
      mrc_slice_mat_getimat(sin, i, j, dim, imat);
      val[0] = mrc_slice_mat_mult(mat, imat, dim);
      slicePutVal(sout, i, j, val);
    }
  }
  free(imat);
  return(sout);
}

int mrc_slice_lie_img(Islice *sin, 
                      Islice *mask, double alpha)
{
  int i, j;
  Ival val1, val2;
  float a, ma;

  a = alpha;
  ma = 1 - alpha;

  for(j = 0; j < sin->ysize; j++)
    for(i = 0; i < sin->xsize; i++){
      sliceGetVal(sin, i, j, val1);
      sliceGetVal(mask, i, j, val2);
      val1[0] = (ma * val1[0]) + (a * val2[0]);

      switch(sin->mode){
      case MRC_MODE_BYTE:
        if (val1[0] > 255)
          val1[0] = 255;
        if (val1[0] < 0)
          val1[0] = 0;
        break;
      case MRC_MODE_SHORT:
        if (val1[0] > 32767)
          val1[0] = 32767;
        if (val1[0] < -32768)
          val1[0] = -32768;
        break;
      }

      if (sin->csize == 3){
        val1[1] = (ma * val1[1]) + (a * val2[1]);
        val1[2] = (ma * val1[2]) + (a * val2[2]);
      }
      slicePutVal(sin, i, j, val1);
    }

  return(0);
}

/* Scales data in the slice by the factor alpha around the value in */
int mrc_slice_lie(Islice *sin, double in, double alpha)
{
  int i, j, c;
  Ival val;
  float scale, offset;

  scale = (float)alpha;
  offset = (1.0f - scale) * (float)in;

  if (sin->csize == 1) {
    for(j = 0; j < sin->ysize; j++)
      for(i = 0; i < sin->xsize; i++) {
        sliceGetVal(sin, i, j, val);
        val[0] = (offset + (scale * val[0]));
        switch(sin->mode) {
        case MRC_MODE_BYTE:
          if (val[0] > 255)
            val[0] = 255;
          if (val[0] < 0)
            val[0] = 0;
          break;
        case MRC_MODE_SHORT:
          if (val[0] > 32767)
            val[0] = 32767;
          if (val[0] < -32768)
            val[0] = -32768;
          break;
        }
        slicePutVal(sin, i, j, val);
      }
  } else {
    for (j = 0; j < sin->ysize; j++)
      for (i = 0; i < sin->xsize; i++) {
        sliceGetVal(sin, i, j, val);
        for(c = 0; c < sin->csize; c++) {
          val[c] = (offset + (scale * val[c]));
          if (sin->mode == MRC_MODE_RGB) {
            if (val[c] > 255)
              val[c] = 255;
            if (val[c] < 0)
              val[c] = 0;
          } else if (sin->mode == MRC_MODE_COMPLEX_SHORT) {
            if (val[c] > 32767)
              val[c] = 32767;
            if (val[c] < -32768)
              val[c] = -32768;
          }
        }
        slicePutVal(sin, i, j, val);
      }
  }
  return(0);
}

float sliceGetValMagnitude(Ival val, int mode)
{
  float m = val[0];
     
  if ((mode == SLICE_MODE_COMPLEX_FLOAT) || 
      (mode == SLICE_MODE_COMPLEX_SHORT)){
    m = (val[0] * val[0]) + (val[1] * val[1]);
    return((float)sqrt(m));
  }

  if (mode == SLICE_MODE_RGB){
    m = val[0] * 0.3f;
    m += val[1] * 0.59f;
    m += val[2] * 0.11f;
  }

  return(m);
}

float sliceGetPixelMagnitude(Islice *s, int x, int y)
{
  return(mrc_slice_getmagnitude(s, x, y));
}

float mrc_slice_getmagnitude(Islice *s, int x, int y)
{
  Ival val;
  float m = 0.0f;
     
  sliceGetVal(s, x, y, val);

  if (s->csize == 1)
    return(val[0]);

  if (s->csize == 2){
    m = (val[0] * val[0]) + (val[1] * val[1]);
    return((float)sqrt(m));
  }

  m = val[0] * 0.3f;
  m += val[1] * 0.59f;
  m += val[2] * 0.11f;
  return(m);
}


Islice *mrc_slice_getvol(struct MRCvolume *v, int sno, char axis)
{
  Islice *sout;
  Ival val;
  int i, j, k;

  switch (axis){
  case 'y': case 'Y':
    sout = mrc_slice_create(v->vol[0]->xsize, v->zsize, v->vol[0]->mode);
    if (!sout)
      return(NULL);
    for (k = 0; k < sout->ysize; k++)
      for(i = 0; i < sout->xsize; i++){
        sliceGetVal(v->vol[k], i, sno, val);
        slicePutVal(sout, i, k, val);
      }
    break;

  case 'x': case 'X':
    sout = mrc_slice_create(v->vol[0]->ysize, v->zsize, v->vol[0]->mode);
    if (!sout)
      return(NULL);
    for (k = 0; k < sout->ysize; k++)
      for (j = 0; j <  sout->xsize; j++){
        sliceGetVal(v->vol[k], sno, j, val);
        slicePutVal(sout, j, k, val);
      }
    break;

  default:
    return(NULL);
  }
  return(sout);
}

int mrc_slice_putvol(struct MRCvolume *v, Islice *s, 
                     int sno, char axis)
{
  Ival val;
  int i, j, k;

  switch (axis){

  case 'z': case 'Z':
    mrc_slice_free(v->vol[sno]);
    v->vol[sno] = s;
    break;

  case 'y': case 'Y':
    for (k = 0; k < s->ysize; k++)
      for(i = 0; i < s->xsize; i++){
        sliceGetVal(s, i, k, val);
        slicePutVal(v->vol[k], i, sno, val);
      }
    break;

  case 'x': case 'X':
    for (k = 0; k < s->ysize; k++)
      for (j = 0; j < s->xsize; j++){
        sliceGetVal(s, j, k, val);
        slicePutVal(v->vol[k], sno, j, val);
      }
    break;
       
  default:
    return(0);
  }
  return(0);
}

int mrc_vol_wrap(struct MRCvolume *v)
{
  int k, z2;
  Islice *s;
  z2 = v->zsize / 2;

  for (k = 0; k < v->zsize; k++){
    mrc_slice_wrap(v->vol[k]);
  }
  for (k = 0; k < z2; k++){
    s = v->vol[k];
    v->vol[k] = v->vol[k + z2];
    v->vol[k + z2] = s;
  }
  return(0);
}


int sliceWriteMRCfile(char *filename, Islice *slice)
{
  MrcHeader hout;
  FILE *fp = fopen(filename, "wb");
  int error;

  if (!fp) return -1;
  mrc_head_new(&hout, slice->xsize, slice->ysize, 1, slice->mode);
  sliceMMM(slice);
  hout.amin  = slice->min;
  hout.amax  = slice->max;
  hout.amean = slice->mean;
  mrc_head_write(fp, &hout);
  b3dFwrite(slice->data.b, slice->dsize, 
            slice->csize * slice->xsize * slice->ysize, fp);
  error = ferror(fp);
  fclose(fp);
  return(error);
}

Islice *sliceReadMRC(MrcHeader *hin, int sno, char axis)
{
  Islice *slice;
  void *buf;
  int nx, ny;

  slice = (Islice *)malloc(sizeof(Islice));
  if (!slice)
    return(NULL);
  slice->mean = hin->amean;
  buf = mrc_mread_slice(hin->fp, hin, sno, axis);
  if (!buf) {
    free(slice);
    return(NULL);
  }
  switch(axis){
  case 'x':
  case 'X':
    nx = hin->nx;
    ny = hin->nz;
    break;
  case 'y':
  case 'Y':
    nx = hin->ny;
    ny = hin->nz;
    break;
  case 'z':
  case 'Z':
    nx = hin->nx;
    ny = hin->ny;
    break;
  default:
    break;
  }
  mrc_slice_init(slice, nx, ny, hin->mode, buf);
  return(slice);
}


Islice *sliceReadSubm(MrcHeader *hin, 
                      int sno, char axis,
                      int s1, int s2, int c1, int c2)
{
  Islice *slice;
  void *buf;
  int nx, ny;
  int llx, lly, urx, ury;

  slice = (Islice *)malloc(sizeof(Islice));
  if (!slice)
    return(NULL);
  slice->mean = hin->amean;

  /* todo: just read in data that is needed. */
  buf = mrc_mread_slice(hin->fp, hin, sno, axis);

  switch(axis){
  case 'x':
  case 'X':
    nx = hin->nx;
    ny = hin->nz;
    break;
  case 'y':
  case 'Y':
    nx = hin->ny;
    ny = hin->nz;
    break;
  case 'z':
  case 'Z':
    nx = hin->nx;
    ny = hin->ny;
    break;
  default:
    break;
  }
  mrc_slice_init(slice, nx, ny, hin->mode, buf);

  llx = c1 - (s1/2);
  lly = c2 - (s2/2);
  urx = llx + s1;
  ury = lly + s2;
  sliceBoxIn(slice, llx, lly, urx, ury);

  return(slice);
}

/* convert a slice to float mode = 2 */
int sliceFloat(Islice *slice)
{
  Islice *tsl;
  Ival val;
  int i, j;

  switch(slice->mode){
  case 0:
  case 1:
    tsl = sliceCreate(slice->xsize, slice->ysize, 2);
    for(j = 0; j < slice->ysize; j++)
      for(i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j, val);
        slicePutVal(tsl,   i, j, val);
      }
    free(slice->data.b);
    slice->data.f = tsl->data.f;
    slice->mode = 2;
    free(tsl);
    break;
  case 2:
    break;
  case 3:
  case 4:
    tsl = sliceCreate(slice->xsize, slice->ysize, 2);
    for(j = 0; j < slice->ysize; j++)
      for(i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] = (val[0] * val[0]) + (val[1] * val[1]);
        val[0] = sqrt(val[0]);
        slicePutVal(tsl,   i, j, val);
      }
    free(slice->data.b);
    slice->data.f = tsl->data.f;
    slice->mode = 2;
    free(tsl);
    break;
  case 16:
    tsl = sliceCreate(slice->xsize, slice->ysize, 2);
    for(j = 0; j < slice->ysize; j++)
      for(i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] = val[0] * 0.3f + val[1] * 0.59f + val[2] * 0.11f;
        slicePutVal(tsl,   i, j, val);
      }
    free(slice->data.b);
    slice->data.f = tsl->data.f;
    slice->mode = 2;
    free(tsl);
    break;
  default:
    return(-1);
  }
  return(0);
}

int sliceComplexFloat(Islice *slice)
{
  Islice *tsl;
  Ival val;
  int i, j;
     
  if (slice->mode > 3)
    return(-1);

  val[1] = 0;
  tsl = sliceCreate(slice->xsize, slice->ysize, MRC_MODE_COMPLEX_FLOAT);
  for(j = 0; j < slice->ysize; j++)
    for(i = 0; i < slice->xsize; i++){
      sliceGetVal(slice, i, j, val);
      slicePutVal(tsl,   i, j, val);
    }
  free(slice->data.b);
  slice->data.f = tsl->data.f;
  slice->mode = 4;
  free(tsl);
  return(0);
}


int sliceGetXSize(Islice *slice)
{
  return(slice->xsize);
}

int sliceGetYSize(Islice *slice)
{
  return(slice->ysize);
}

int sliceMultConst(Islice *slice, Ival c)
{
  int i, j;
  Ival val;
     
  switch(slice->csize){
  case 1:
    for (j = 0; j < slice->ysize; j++){
      for (i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] *= c[0];
        slicePutVal(slice, i, j, val);
      }
    }
    break;
  case 2:
    for (j = 0; j < slice->ysize; j++){
      for (i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] *= c[0];
        val[1] *= c[1];
        slicePutVal(slice, i, j, val);
      }
    }
  case 3:
    for (j = 0; j < slice->ysize; j++){
      for (i = 0; i < slice->xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] *= c[0];
        val[1] *= c[1];
        val[2] *= c[2];
        slicePutVal(slice, i, j, val);
      }
    }
    break;
  }
  return(0);
}

int sliceAddConst(Islice *slice, Ival c)
{
  Ival val;
  unsigned int i, j;
  unsigned int xsize = slice->xsize;
  unsigned int ysize = slice->ysize;

  if (slice->csize == 1 ){
    for (j = 0; j < ysize; j++){
      for (i = 0; i < xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] += c[0];
        slicePutVal(slice, i, j, val);
      }
    }
    return(0);
  }
  if (slice->csize == 2 ){
    for (j = 0; j < ysize; j++){
      for (i = 0; i < xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] += c[0];
        val[1] += c[1];
        slicePutVal(slice, i, j, val);
      }
    }
    return(0);
  }
  if (slice->csize == 3 ){
    for (j = 0; j < ysize; j++){
      for (i = 0; i < xsize; i++){
        sliceGetVal(slice, i, j, val);
        val[0] += c[0];
        val[1] += c[1];
        val[2] += c[2];
        slicePutVal(slice, i, j, val);
      }
    }
    return(0);
  }
  return(0);
}

int sliceNewMode(Islice *s, int mode)
{
  Islice *ns;
  Ival val;
  int i, j;
  int default_copy = 0;
  int limit_val = 0;
  float minval, maxval;

  if (!s)
    return(-1);
     
  if (s->mode == mode)
    return(mode);

  ns = sliceCreate(s->xsize, s->ysize, mode);

  /* Set up limiting values */
  if (mode == MRC_MODE_BYTE || mode == MRC_MODE_RGB) {
    limit_val = 1;
    minval = 0.;
    maxval = 255;
  } else if (mode == MRC_MODE_SHORT) {
    limit_val = 1;
    minval = -32768.;
    maxval = 32767.;
  }

  if (!ns)
    return(-1);

  switch(s->mode){
  case MRC_MODE_BYTE:
  case MRC_MODE_SHORT:
  case MRC_MODE_FLOAT:
    switch(mode){
    case MRC_MODE_BYTE:
    case MRC_MODE_SHORT:
    case MRC_MODE_FLOAT:
      default_copy = 1;
      break;
    case MRC_MODE_COMPLEX_FLOAT:
    case MRC_MODE_COMPLEX_SHORT:
      val[1] = 0;
      default_copy = 1;
      break;
    case MRC_MODE_RGB:
      for(j = 0; j < s->ysize; j++)
        for(i = 0; i < s->xsize; i++){
          sliceGetVal(s,  i, j, val);
          if (limit_val)
            val[0] = B3DMIN(maxval, B3DMAX(minval, val[0]));
          val[2] = val[1] = val[0];
          slicePutVal(ns, i, j, val);
        }
      break;
    default:
      default_copy = 1;
      break;
    }
    break;

  case MRC_MODE_COMPLEX_FLOAT:
  case MRC_MODE_COMPLEX_SHORT:
    switch(mode){
    case MRC_MODE_BYTE:
    case MRC_MODE_SHORT:
    case MRC_MODE_FLOAT:
      for(j = 0; j < s->ysize; j++)
        for(i = 0; i < s->xsize; i++){
          sliceGetVal(s,  i, j, val);
          val[0] *= val[0];
          val[1] *= val[1];
          val[0] += val[1];
          val[0] = (float)sqrt(val[0]);
          if (limit_val)
            val[0] = B3DMIN(maxval, B3DMAX(minval, val[0]));
          slicePutVal(ns, i, j, val);
        }
      break;
    case MRC_MODE_COMPLEX_FLOAT:
    case MRC_MODE_COMPLEX_SHORT:
      default_copy = 1;
      break;
    case MRC_MODE_RGB:
      for(j = 0; j < s->ysize; j++)
        for(i = 0; i < s->xsize; i++){
          sliceGetVal(s,  i, j, val);
          val[0] *= val[0];
          val[1] *= val[1];
          val[0] += val[1];
          val[0] = (float)sqrt(val[0]);
          if (limit_val)
            val[0] = B3DMIN(maxval, B3DMAX(minval, val[0]));
          val[2] = val[1] = val[0];
          slicePutVal(ns, i, j, val);
        }
      break;
    }
    break;

  case MRC_MODE_RGB:
    switch(mode){
    case MRC_MODE_BYTE:
    case MRC_MODE_SHORT:
    case MRC_MODE_FLOAT:
      for(j = 0; j < s->ysize; j++)
        for(i = 0; i < s->xsize; i++){
          sliceGetVal(s,  i, j, val);
          val[0] = (val[0] * 0.3f) +
            (val[1] * 0.59f) + (val[2] * 0.11f);
          if (limit_val)
            val[0] = B3DMIN(maxval, B3DMAX(minval, val[0]));
          slicePutVal(ns, i, j, val);
        }
      break;
           
    case MRC_MODE_COMPLEX_FLOAT:
    case MRC_MODE_COMPLEX_SHORT:
      for(j = 0; j < s->ysize; j++)
        for(i = 0; i < s->xsize; i++){
          sliceGetVal(s,  i, j, val);
          val[0] = (val[0] * 0.3f) +
            (val[1] * 0.59f) + (val[2] * 0.11f);
          val[1] = 0;
          slicePutVal(ns, i, j, val);
        }
      break;
           
    default:
      default_copy = 1;
      break;
    }
    break;

  default:
    default_copy = 1;
    break;

  }

  if (default_copy){
    for(j = 0; j < s->ysize; j++)
      for(i = 0; i < s->xsize; i++){
        sliceGetVal(s,  i, j, val);
        if (limit_val)
          val[0] = B3DMIN(maxval, B3DMAX(minval, val[0]));
        slicePutVal(ns, i, j, val);
      }
  }
     
  free(s->data.b);
  *s = *ns;
  free(ns);
  return(mode);
}

/*
$Log$
Revision 3.10  2005/01/17 17:13:34  mast
Used typedefs for structures, fixed new mode conversion to truncate
bytes and ints

Revision 3.9  2005/01/06 18:15:28  mast
Fixed _lie scaling function, fixed matrix filtering function

Revision 3.8  2004/12/02 21:54:53  mast
Fixed sliceReadMRC to return null upon error

Revision 3.7  2004/11/07 23:06:09  mast
Fixed sliceGradient to not saturate

Revision 3.6  2004/11/05 18:53:04  mast
Include local files with quotes, not brackets

Revision 3.5  2004/11/04 17:10:27  mast
libiimod.def

Revision 3.4  2004/09/10 21:33:53  mast
Eliminated long variables

*/
