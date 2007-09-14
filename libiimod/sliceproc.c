/*
 *  sliceproc.c -- image slice support for 3dmod and clip
 *
 *  Original author: James Kremer
 *  Revised and added to by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end
 */

#include <math.h>
#include <stdlib.h>
#include "b3dutil.h"
#include "mrcc.h"
#include "sliceproc.h"

/* DNM 11/7/04: All routines expect the min and max of the input slice to be
   set to the desired range for scaling the output */

#define RANGE(s, i, j)  (((i)>0)&&((j)>0)&&((i)<(s)->xsize)&&((j)<(s)->ysize))
#define GETVAL(s, i, j) ((s)->data.b[(i) + ((j) * (s)->xsize)])
#define GETSVAL(t, i, j) ((t)->data.s[(i) + ((j) * (t)->xsize)])
#define GETFVAL(s, i, j) ((s)->data.f[(i) + ((j) * (s)->xsize)])

static void sliceScaleAndFree(Islice *sout, Islice *sin);
static float selectFloat(int s, float *r, int num);
static int selectInt(int s, int *r, int num);

static int SmoothKernel[3][3] = 
  { 
    {1, 2, 1},
    {2, 4, 2},
    {1, 2, 1}
  };

static int SharpenKernel[3][3] =
  {
    {-1,  -1, -1},
    {-1,  9, -1},
    {-1,  -1, -1}
  };

static int LaplacianKernel[3][3] =
  {
    {1,  1, 1},
    {1,  -4, 1},
    {1,  1, 1}
  };


/*!
 * Adds [inVal] to all values in the slice [sin], which must contain bytes.
 * Returns 0.
 */
int sliceByteAdd(Islice *sin, int inVal)
{
  int imax = sin->xsize * sin->ysize;
  int aval;
  int i;
     
  unsigned char *image = sin->data.b;

  for(i = 0; i < imax; i++){
    aval = image[i] + inVal;
    if (aval > 255) aval = 255;
    if (aval < 0) aval = 0;
    image[i] = aval;
  }
  return 0;
}

/*!
 * Computes a Prewitt or Sobel edge filter on the slice [sin], depending on 
 * whether the center pixel weighting, [center], is 1 or 2.  The slice must 
 * contain bytes.  Returns 0.
 */
int sliceByteEdgeTwo(Islice *sin, int center)
{
  Islice *sout = sliceCreate(sin->xsize, sin->ysize,  SLICE_MODE_FLOAT);
  float val;
  int imax = sin->xsize - 1;
  int jmax = sin->ysize - 1;
  int i, j, x, y;
  float range;
  float Sr, Sc, k;

  for(i = 1; i < imax; i++) 
    for(j = 1; j < jmax; j++){
      Sr = (GETVAL(sin, i-1, j-1) + 
            (center * GETVAL(sin, i, j-1)) + GETVAL(sin, i+1, j-1)) -
        (GETVAL(sin, i-1, j+1) +
         (center * GETVAL(sin, i, j+1)) + GETVAL(sin, i+1, j+1));
               
      Sc = (GETVAL(sin, i+1, j+1) +  center * GETVAL(sin, i+1, j) +
            GETVAL(sin, i+1, j-1)) -
        (GETVAL(sin, i-1, j+1) +
         (center * GETVAL(sin, i-1, j)) +
         GETVAL(sin, i-1, j-1));
      Sr *= Sr;
      Sc *= Sc;
      val = sqrt(Sr + Sc);

      GETFVAL(sout, i, j) = val;
    }

  for(j = 1; j < jmax; j++){
    GETFVAL(sout, 0, j) =  GETFVAL(sout, 1, j);
    GETFVAL(sout, imax, j) =  GETFVAL(sout, imax-1, j);
  }

  for(i = 0; i <= imax; i++){
    GETFVAL(sout, i, 0) =  GETFVAL(sout, i, 1);
    GETFVAL(sout, i, jmax) =  GETFVAL(sout, i, jmax-1);
  }
  sliceScaleAndFree(sout, sin);
  return(0);
}

/*!
 * Computes a Sobel edge filter on the slice [sin], which must
 * contain bytes.  Returns 0.
 */
int sliceByteEdgeSobel(Islice *sin)
{
  return(sliceByteEdgeTwo(sin, 2));
}

/*!
 * Computes a Prewitt edge filter on the slice [sin], which must
 * contain bytes.  Returns 0.
 */
int sliceByteEdgePrewitt(Islice *sin)
{
  return(sliceByteEdgeTwo(sin, 1));
}

/*!
 * Computes a Laplacian edge filter on the slice [sin], which must
 * contain bytes.  The Laplacian kernel is a -4 surrounded by 1's.  Returns 0.
 */
int sliceByteEdgeLaplacian(Islice *sin)
{
  return(sliceByteConvolve(sin, LaplacianKernel));
}

/*!
 * Computes a Laplacian edge filter on the slice [sin], which must contain 
 * bytes.  The sharpening kernel is a 9 surrounded by -1's.  Returns 0.
 */
int sliceByteSharpen(Islice *sin)
{
  return(sliceByteConvolve(sin, SharpenKernel));
}

/* DNM 11/07/04: Removed smoothing routine that did the same as convolving with
   this kernel */

/*!
 * Smooths the slice [sin], which must contain bytes, by convolving with a 
 * 3x3 smoothing kernel.  Returns 0.
 */
int sliceByteSmooth(Islice *sin)
{
  return(sliceByteConvolve(sin, SmoothKernel));
}

/*!
 * Convolves the slice [sin] with the 3x3 kernel in [mask].  The slice must
 * contain bytes.  Returns 0.
 */
int sliceByteConvolve(Islice *sin, int mask[3][3])
{
  Islice *sout = sliceCreate(sin->xsize, sin->ysize,  SLICE_MODE_SHORT);
  int imax = sin->xsize - 1;
  int jmax = sin->ysize - 1;
  int i, j, m, n;
  int val;

  /* 11/7/04: No longer need to determine mean of mask with new scaling */

  for(i = 1; i < imax; i++) 
    for(j = 1; j < jmax; j++){
      val = (GETVAL(sin, i+1, j+1) * mask[0][0] +
             GETVAL(sin,   i, j+1) * mask[0][1] +
             GETVAL(sin, i-1, j+1) * mask[0][2] +
             GETVAL(sin, i+1, j) * mask[1][0] +
             GETVAL(sin,   i, j) * mask[1][1] +
             GETVAL(sin, i-1, j) * mask[1][2] +
             GETVAL(sin, i+1, j-1) * mask[2][0] +
             GETVAL(sin,   i, j-1) * mask[2][1] +
             GETVAL(sin, i-1, j-1) * mask[2][2]);
      GETSVAL(sout, i, j) = val;
    }

  for(j = 1; j < jmax; j++){
    GETSVAL(sout, 0, j) =  GETSVAL(sout, 1, j);
    GETSVAL(sout, imax, j) =  GETSVAL(sout, imax-1, j);
  }
  for(i = 0; i <= imax; i++){
    GETSVAL(sout, i, 0) =  GETSVAL(sout, i, 1);
    GETSVAL(sout, i, jmax) =  GETSVAL(sout, i, jmax-1);
  }

  sliceScaleAndFree(sout, sin);
  return(0);
}




static int nay8(Islice *sin, int i, int j, int val)
{
  int n, m, k = 0;
  int x, y;

  if ( GETVAL(sin, i, j) != val)
    return(0);
     
  for (n = -1; n <= 1; n++){
    y = n + j;
    for(m = -1; m <= 1 ; m++){
      x = m + i;
      if ((x > 0) && (y > 0) && (x < sin->xsize) && (y < sin->ysize))
        if (GETVAL(sin, x, y) == val) k++; 
    }
  }
  return(k-1);
}

int sliceByteThreshold(Islice *sin, int val)
{
  int imax = sin->xsize * sin->ysize;
  int i;
  unsigned char *image = sin->data.b;
    
  int pmin = (int)sin->min;
  int pmax = (int)sin->max;
  int thresh = pmin + val;
    

  for(i = 0; i < imax; i++){
    if (image[i] < thresh)
      image[i] = pmax;
    else
      image[i] = pmin;
  }
  return 0;
}

int sliceByteGrow(Islice *sin, int val)
{
  int imax = sin->xsize;
  int jmax = sin->ysize;
  int i, j, m, n, x, y;

  for(j = 0; j < jmax; j++)
    for(i = 0; i < imax; i++){
      if ( GETVAL(sin, i, j) != val) continue;
      for(m = -1; m <= 1; m++){
        y = j + m;
        if ((y < 0) || (y >= sin->ysize)) continue;
        for(n = -1; n <= 1; n++){
          x = n + i;
          if ((x == i) && (y == j)) continue;
          if ((x < 0) || (x >= sin->xsize)) continue;
          if (GETVAL(sin, x, y) == sin->min)
            GETVAL(sin, x, y) = (unsigned char)(sin->max - 1);
        }
      }
    }


  for(j = 0; j < jmax; j++)
    for(i = 0; i < imax; i++){
      if ( GETVAL(sin, i, j) == (sin->max - 1)){
        GETVAL(sin, i, j) = val;
      }
    } 
  return(0);
}

int sliceByteShrink(Islice *sin, int val)
{
  Islice *sout = sliceCreate(sin->xsize, sin->ysize,  SLICE_MODE_BYTE);
  int imax = sin->xsize;
  int jmax = sin->ysize;
  int i, j, m, n;
  int tval;
  int white = 0;

  unsigned char pmin = (unsigned char)sin->min;
  unsigned char pmax = (unsigned char)sin->max;

  for(j = 0; j < jmax; j++)
    for(i = 0; i < imax; i++){
      GETVAL(sout, i, j) = 0;
    }


  for(j = 0; j < jmax; j++)
    for(i = 0; i < imax; i++){
      if (nay8(sin, i, j, pmax) < 7)
        GETVAL(sout, i, j)  = 1;
    } 

     
  for(j = 0; j < jmax; j++)
    for(i = 0; i < imax; i++){
      if (GETVAL(sout, i, j))
        GETVAL(sin, i, j)  = pmin;
    }
  return 0;
}

int sliceByteGraham(Islice *sin)
{
  Islice *sout = sliceCreate(sin->xsize, sin->ysize,  SLICE_MODE_FLOAT);
  float val;
  int imax = sin->xsize - 1;
  int jmax = sin->ysize - 1;
  int i, j, x, y;
  float range;
  float Ixx, Iyy, k;
  float ld = 1.0f/6.0f;
  float hd = 1.0f/3.0f;
  float delta = 5;

  for(i = 1; i < imax; i++) 
    for(j = 1; j < jmax; j++){
      Ixx  = GETVAL(sin, i+1, j+1) * ld;
      Ixx -= GETVAL(sin,   i, j+1) * hd;
      Ixx += GETVAL(sin, i-1, j+1) * ld;
      Ixx += GETVAL(sin, i+1,   j) * ld;
      Ixx -= GETVAL(sin,   i,   j) * hd;
      Ixx += GETVAL(sin, i-1,   j) * ld;
      Ixx += GETVAL(sin, i+1, j-1) * ld;
      Ixx -= GETVAL(sin,   i, j-1) * hd;
      Ixx += GETVAL(sin, i-1, j-1) * ld;

      Iyy  = GETVAL(sin, i+1, j+1) * ld;
      Iyy += GETVAL(sin,   i, j+1) * ld;
      Iyy += GETVAL(sin, i-1, j+1) * ld;
      Iyy -= GETVAL(sin, i+1,   j) * hd;
      Iyy -= GETVAL(sin,   i,   j) * hd;
      Iyy -= GETVAL(sin, i-1,   j) * hd;
      Iyy += GETVAL(sin, i+1, j-1) * ld;
      Iyy += GETVAL(sin,   i, j-1) * ld;
      Iyy += GETVAL(sin, i-1, j-1) * ld;


      if (Ixx < delta){
        if (Iyy < delta){
          val = (GETVAL(sin, i+1, j+1) +
                 GETVAL(sin,   i, j+1) + 
                 GETVAL(sin, i-1, j+1) +
                 GETVAL(sin, i+1, j) +
                 GETVAL(sin,   i, j) +
                 GETVAL(sin, i-1, j) +
                 GETVAL(sin, i+1, j-1) +
                 GETVAL(sin,   i, j-1) +
                 GETVAL(sin, i-1, j-1)) / 9.0f;
        }else{
          val = (GETVAL(sin, i+1, j) +
                 GETVAL(sin,   i, j) +
                 GETVAL(sin, i-1, j)) / 3.0f;
        }
      }else{
        if (Iyy < delta){
          val = (GETVAL(sin, i, j+1) +
                 GETVAL(sin, i, j) +
                 GETVAL(sin, 1, j-1)) / 3.0f;
        }else{
          val = GETVAL(sin,   i, j);
        }
      }
      GETFVAL(sout, i, j) = val;
    }

  for(j = 1; j < jmax; j++){
    GETFVAL(sout, 0, j) =  GETFVAL(sout, 1, j);
    GETFVAL(sout, imax, j) =  GETFVAL(sout, imax-1, j);
  }

  for(i = 0; i <= imax; i++){
    GETFVAL(sout, i, 0) =  GETFVAL(sout, i, 1);
    GETFVAL(sout, i, jmax) =  GETFVAL(sout, i, jmax-1);
  }

  sliceScaleAndFree(sout, sin);
  return(0);

}

/*!
 * Applies a 2-D or 3-D median filter to the slices contained in the 
 * @@mrcslice.html#Istack structure@ [v],
 * with the size in X and Y specified by [size] and the size in Z determined
 * by the number of slices in the stack.  Puts the output into [sout], which
 * should already be set up for size and mode, but need not have the
 * same mode as the input slices, and should not have a data array yet.  
 * Input and output modes can be byte, short, or float.  Returns -1 for memory
 * error or -2 for an illegal mode.
 */
int sliceMedianFilter(Islice *sout, Istack *v, int size)
{
  Islice *sl = v->vol[0];
  float *fVals = NULL;
  int *iVals = NULL;
  int isFloat = 0;
  int blockSize = v->zsize * size * size;
  int nVals, x, y, z, xs, ys, ix, iy, xe, ye, dminus, dplus, select;
  int *iout;
  float *fout;
  Ival val;
  b3dFloat *fin;
  b3dUByte *bin;
  b3dInt16 *sin;
  b3dUInt16 *usin;

  /* Get array for data, make sure mode is legal */
  if (sl->mode == SLICE_MODE_FLOAT) {
    fVals = (float *)malloc(blockSize * sizeof(float));
    if (!fVals)
      return (-1);
    isFloat = 1;
  } else if (sl->mode == SLICE_MODE_BYTE || sl->mode == SLICE_MODE_SHORT ||
             sl->mode == SLICE_MODE_USHORT) {
    iVals = (int *)malloc(blockSize * sizeof(int));
    if (!iVals)
      return (-1);
  } else
    return (-2);

  dminus = size / 2;
  dplus = (size + 1) / 2;

  /* Loop on positions */
  for (y = 0; y < sl->ysize; y++) {
    ys = B3DMAX(0, y - dminus);
    ye = B3DMIN(sl->ysize, y + dplus);
    for (x = 0; x < sl->xsize; x++) {
      xs = B3DMAX(0, x - dminus);
      xe = B3DMIN(sl->xsize, x + dplus);
      nVals = v->zsize * (xe - xs) * (ye - ys);
      select = (nVals + 1) / 2;

      /* Loop on slices and subareas to load arrays */
      switch (sl->mode) {
      case SLICE_MODE_FLOAT:
        fout = fVals;
        for (z = 0; z < v->zsize; z++) 
          for (iy = ys; iy < ye; iy++) {
            fin = &v->vol[z]->data.f[xs + iy * sl->xsize];
            for (ix = xs; ix < xe; ix++)
              *fout++ = *fin++;
          }
        val[0] = selectFloat(select, fVals, nVals);
        if (!(nVals % 2))
          val[0] = 0.5 * (val[0] + selectFloat(select + 1, fVals, nVals));
        break;
        
      case SLICE_MODE_SHORT:
        iout = iVals;
        for (z = 0; z < v->zsize; z++) 
          for (iy = ys; iy < ye; iy++) {
            sin = &v->vol[z]->data.s[xs + iy * sl->xsize];
            for (ix = xs; ix < xe; ix++)
              *iout++ = *sin++;
          }
        val[0] =selectInt(select, iVals, nVals);
        if (!(nVals % 2))
          val[0] = 0.5f * (val[0] + selectInt(select + 1, iVals, nVals));
        break;
        
      case SLICE_MODE_USHORT:
        iout = iVals;
        for (z = 0; z < v->zsize; z++) 
          for (iy = ys; iy < ye; iy++) {
            usin = &v->vol[z]->data.us[xs + iy * sl->xsize];
            for (ix = xs; ix < xe; ix++)
              *iout++ = *usin++;
          }
        val[0] =selectInt(select, iVals, nVals);
        if (!(nVals % 2))
          val[0] = 0.5f * (val[0] + selectInt(select + 1, iVals, nVals));
        break;
        
      case SLICE_MODE_BYTE:
        iout = iVals;
        for (z = 0; z < v->zsize; z++) 
          for (iy = ys; iy < ye; iy++) {
            bin = &v->vol[z]->data.b[xs + iy * sl->xsize];
            for (ix = xs; ix < xe; ix++)
              *iout++ = *bin++;
          }
        val[0] =selectInt(select, iVals, nVals);
        if (!(nVals % 2))
          val[0] = 0.5f * (val[0] + selectInt(select + 1, iVals, nVals));
        break;
      }
      slicePutVal(sout, x, y, val);
    }
  }
  if (iVals)
    free(iVals);
  if (fVals)
    free(fVals);
  return 0;
}

/*! 
 * Does anisotropic diffusion on slice [sl] using the Perona and Malik 
 * filtering method, based on a program by Alejandro Canterero.
 * [outMode] specifies the output mode; [CC] is edge stopping type (1, 2, 3),
 * [k] is the threshold parameter, [lambda] is the step size, [iterations] is 
 * the number of iterations to run.  [clearFlag] is one of ANISO_CLEAR_AT_END,
 * ANISO_LEAVE_OPEN, or ANISO_CLEAR_ONLY to allow additional iterations to be 
 * done on the existing data.  Returns -1 for memory error.
 */
int sliceAnisoDiff(Islice *sl,  int outMode, int CC, double k, double lambda,
                   int iterations, int clearFlag)
{
  static float **image, **image2, **imout;
  static int iterDone = 0;
  int i, j;
  int n = sl->xsize;
  int m = sl->ysize;

  /* If just clearing, free arrays if allocated, set iterations to 0 */
  if (clearFlag == ANISO_CLEAR_ONLY) {
    if (iterDone) {
      free(image[0]);
      free(image);
      free(image2[0]);
      free(image2);
      iterDone = 0;
    }
    return 0;
  }

  /* Convert slice to float for copying to/from double */
  if (sl->mode != SLICE_MODE_FLOAT && sliceNewMode(sl, SLICE_MODE_FLOAT) < 0)
    return -1;


  /* If no iterations yet, get double arrays */
  if (!iterDone) {
    image = allocate2D_float(m + 2, n + 2);
    if (!image)
      return -1;
    image2 = allocate2D_float(m + 2, n + 2);
    if (!image2) {
      free(image[0]);
      free(image);
      return -1;
    }

    /* Copy data into array */
    for (j = 0; j < m; j++)
      for (i = 0; i < n; i++)
        image[j + 1][i + 1] = sl->data.f[i + j * sl->xsize];
  }

  /* alternate between two matrices to avoid memcopy */	
  /* printf("m = %d n = %d CC = %d k = %f lambda = %f, iter = %d\n",
     m,n,CC,k,lambda, iterations); */
  for (i = 0; i < iterations; i++, iterDone++) {
	if ( iterDone % 2 == 0 ) {
      updateMatrix(image2,image,m,n,CC,k,lambda);
      imout = image2;
	} else {
      updateMatrix(image,image2,m,n,CC,k,lambda);
      imout = image;
	}
  }
  
  /* Copy data back to slice */
  for (j = 0; j < m; j++)
    for (i = 0; i < n; i++)
      sl->data.f[i + j * sl->xsize] = imout[j + 1][i + 1];

  /* Free data if doing one-shot operation */
  if (clearFlag == ANISO_CLEAR_AT_END) {
    free(image[0]);
    free(image);
    free(image2[0]);
    free(image2);
    iterDone = 0;
  }

  /* convert slice to output mode */
  if (outMode != SLICE_MODE_FLOAT && sliceNewMode(sl, outMode) < 0)
    return -1;

  return 0;
}

/*! 
 * Does anisotropic diffusion on slice [sl], which must contain bytes.
 * [image] and [image2] are float arrays of size {xsize} + 2 by {ysize} + 2 
 * that have already been allocated with @allocate2D_float.
 * [CC] is edge stopping type (1, 2, 3),
 * [k] is the threshold parameter, [lambda] is the step size, [iterations] is 
 * the number of iterations to run.  [iterDone] is maintained with the total
 * number of iterations done.
 */
void sliceByteAnisoDiff(Islice *sl, float **image, float **image2, 
                        int CC, double k, double lambda, int iterations, 
                        int *iterDone)
{
  float **imout;
  int val;
  int i, j;
  int n = sl->xsize;
  int m = sl->ysize;

  /* If no iterations yet, Copy data into array */
  if (!(*iterDone)) {
    for (j = 0; j < m; j++)
      for (i = 0; i < n; i++)
        image[j + 1][i + 1] = sl->data.b[i + j * sl->xsize];
  }

  /* alternate between two matrices to avoid memcopy */	
  /* printf("m = %d n = %d CC = %d k = %f lambda = %f, iter = %d\n",
     m,n,CC,k,lambda, iterations); */
  for (i = 0; i < iterations; i++, (*iterDone)++) {
	if ( (*iterDone) % 2 == 0 ) {
      updateMatrix(image2,image,m,n,CC,k,lambda);
      imout = image2;
	} else {
      updateMatrix(image,image2,m,n,CC,k,lambda);
      imout = image;
	}
  }
  
  /* Copy data back to slice */
  for (j = 0; j < m; j++) {
    for (i = 0; i < n; i++) {
      val = (int)imout[j + 1][i + 1];
      if (val < 0)
        val = 0;
      if (val > 255)
        val = 255;
      sl->data.b[i + j * sl->xsize] = (unsigned char)val;
    }
  }
}

/*!
 * Allocates a  2D array of doubles of size [m] by [n] that is contiguous in 
 * memory.  The array is actually a 1D vector of length [m] x [n], and this
 * routine returns an array of pointers to the rows.
 */
float **allocate2D_float(int m, int n ) 
{
  float  **a;
  float  *fake;
  int i;
  
  if ( (a = (float **)malloc(sizeof(float *)*m)) == NULL )
    return NULL;
  
  if ( (fake = (float *) malloc(m*n*sizeof(float))) == NULL ) {
    free(a);
    return NULL;
  }

  for (i = 0; i < m; i++)
	a[i] = &(fake[i*n]);
  
  return a;
}


/*!
 * Finds the min and max of slice [s] as rapidly as possible for the four
 * basic data modes, byte, float, and signed and unsigned short integer.
 * Returns 1 for other modes.
 */
int sliceMinMax(Islice *s)
{
  int imin, imax, ival, i;
  float fmin, fmax, fval;
  switch (s->mode) {
  case SLICE_MODE_BYTE:
    imin = imax = s->data.b[0];
    for (i = 1; i < s->xsize * s->ysize; i++) {
      ival = s->data.b[i];
      if (imin > ival)
        imin = ival;
      if (imax < ival)
        imax = ival;
    }
    s->min = imin;
    s->max = imax;
    break;

  case SLICE_MODE_SHORT:
    imin = imax = s->data.s[0];
    for (i = 1; i < s->xsize * s->ysize; i++) {
      ival = s->data.s[i];
      if (imin > ival)
        imin = ival;
      if (imax < ival)
        imax = ival;
    }
    s->min = imin;
    s->max = imax;
    break;

  case SLICE_MODE_USHORT:
    imin = imax = s->data.us[0];
    for (i = 1; i < s->xsize * s->ysize; i++) {
      ival = s->data.us[i];
      if (imin > ival)
        imin = ival;
      if (imax < ival)
        imax = ival;
    }
    s->min = imin;
    s->max = imax;
    break;

  case SLICE_MODE_FLOAT:
    fmin = fmax = s->data.f[0];
    for (i = 1; i < s->xsize * s->ysize; i++) {
      fval = s->data.f[i];
      if (fmin > fval)
        fmin = fval;
      if (fmax < fval)
        fmax = fval;
    }
    s->min = fmin;
    s->max = fmax;
    break;

  default:
    return 1;
  }
  return 0;
}

/* Determine min and max of slice sout, determine scaling to match them to the 
   min and max values provided in slice sin, scale data into sin and free sout
*/ 
static void sliceScaleAndFree(Islice *sout, Islice *sin)
{
  float aval, mval;
  int imax, i;

  sliceMinMax(sout);
  imax = sin->xsize * sin->ysize;
  mval = (sin->max - sin->min) / (sout->max - sout->min);
  aval = sin->min - mval * sout->min;
  switch (sout->mode) {
  case SLICE_MODE_FLOAT:
    for(i = 0; i < imax; i++)
      sin->data.b[i] = (unsigned char)(sout->data.f[i] * mval + aval);
    break;
  case SLICE_MODE_SHORT:
    for(i = 0; i < imax; i++)
      sin->data.b[i] = (unsigned char)(sout->data.s[i] * mval + aval);
    break;
  case SLICE_MODE_USHORT:
    for(i = 0; i < imax; i++)
      sin->data.b[i] = (unsigned char)(sout->data.us[i] * mval + aval);
    break;
  case SLICE_MODE_BYTE:
    for(i = 0; i < imax; i++)
      sin->data.b[i] = (unsigned char)(sout->data.b[i] * mval + aval);
    break;
  }
  sliceFree(sout);
}

/* 
 * Routines for selecting item number s (numbered from 1) out of num items
 * selectFloat takes a float array, selectInt takes an int array
 * Based on a pascal program apparently from Handbook of Data Structures and 
 * Algorithms, by Gonnet and Baeza-Yates 
 */
static float selectFloat(int s, float *r, int num)
{
  int lo = 0;
  int up = num - 1;
  int i, j;
  float temp;
  s--;
  while (up >= s && s >= lo) {
    i = lo;
    j = up;
    temp = r[s];
    r[s] = r[lo];
    r[lo] = temp;
    while (i < j) {
      while (r[j] > temp)
        j--;
      r[i] = r[j];
      while (i < j && r[i] <= temp)
        i++;
      r[j] = r[i];
    }
    r[i] = temp;
    if (s < i)
      up = i - 1;
    else
      lo = i + 1;
  }

  return r[s];
}

static int selectInt(int s, int *r, int num)
{
  int lo = 0;
  int up = num - 1;
  int i, j;
  int temp;
  s--;
  while (up >= s && s >= lo) {
    i = lo;
    j = up;
    temp = r[s];
    r[s] = r[lo];
    r[lo] = temp;
    while (i < j) {
      while (r[j] > temp)
        j--;
      r[i] = r[j];
      while (i < j && r[i] <= temp)
        i++;
      r[j] = r[i];
    }
    r[i] = temp;
    if (s < i)
      up = i - 1;
    else
      lo = i + 1;
  }

  return r[s];
}


#define PLIST_CHUNK 1000
/*!
 * Analyzes for fill areas at the edge of the slice [sl], finds the borders 
 * between actual image and fill areas, and tapers the image intensities down 
 * to the fill value over [ntaper] pixels.  If [inside] is 0 the image 
 * intensities at the edge are extended into the fill area, while if [inside] 
 * is 1 then pixels inside the image are attenuated.  Returns -1 for memory
 * allocation errors.
 */
int sliceTaperAtFill(Islice *sl, int ntaper, int inside)
{
  struct pixdist {
    unsigned short int x, y;
    unsigned short dist;
    signed char dx, dy;
  };

  Ival val;
  float fracs[128], fillpart[128];
  float fillval, lastval;
  int len, longest;
  int i, ix, iy, xnext, ynext, xp, yp, lastint, interval, found;
  int dir, tapdir, col, row, ncol, ind, xstart, ystart, ndiv;
  int dist;
  struct pixdist *plist;
  int pllimit = PLIST_CHUNK;
  int plsize = 0;
  unsigned char *bitmap;
  int xsize = sl->xsize;
  int ysize = sl->ysize;
  int bmsize = (xsize * ysize + 7) / 8;
  int dxout[4] = {0, 1, 0, -1};
  int dyout[4] = {-1, 0, 1, 0};
  int dxnext[4] = {1, 0, -1, 0};
  int dynext[4] = {0, 1, 0, -1};

  inside = inside != 0 ? 1 : 0;

  /* find longest string of repeated values along the edge */
  longest = 0;
  for (iy = 0; iy < ysize; iy += ysize - 1) {
    len = 0;
    sliceGetVal(sl, 0, iy, val);
    lastval = val[0];
    for (ix = 1; ix < xsize; ix++) {
      sliceGetVal(sl, ix, iy, val);
      if (val[0] == lastval) {
        /* same as last, add to count, see if this is a new
           best count*/
        len++;
        if (len > longest) {
          longest = len;
          fillval = lastval;
        }
      } else {
        /* different, reset count and set new lastval */
        len = 0;
        lastval = val[0];
      }
    }
  }         
  for (ix = 0; ix < xsize; ix += xsize - 1) {
    len = 0;
    sliceGetVal(sl, ix, 0, val);
    lastval = val[0];
    for (iy = 1; iy < ysize; iy++) {
      sliceGetVal(sl, ix, iy, val);
      if (val[0] == lastval) {
        /* same as last, add to count, see if this is a new
           best count */
        len++;
        if (len > longest) {
          longest = len;
          fillval = lastval;
        }
      } else {
        /* different, reset count and set new lastval */
        len = 0;
        lastval = val[0];
      }
    }
  }         

  /* If length below a criterion (what?) , return without error */
  if (longest < 10)
    return 0;

  /* set the slice mean so that sliceGetValue will return this outside the
     edges */
  sl->mean = fillval;

  /* look from left edge along a series of lines to find the first point
     that is not a fill point */
  lastint = 0;
  found = 0;
  for (ndiv = 2; ndiv <= ysize; ndiv++) {
    interval = (ysize + 2) / ndiv;
    if (interval == lastint)
      continue;
    lastint = interval;
    for (iy = interval; iy < ysize; iy += interval) {
      for (ix = 0; ix < xsize; ix++) {
        sliceGetVal(sl, ix, iy, val);
        if (val[0] != fillval) {
          found = 1;
          break;
        }
      }
      if (found)
        break;
    }
    if (found)
      break;
  }

  if (!found)
    return 0;

  /* Get initial chunk of pixel list and bitmap */
  plist = (struct pixdist *)malloc(PLIST_CHUNK * sizeof(struct pixdist));
  if (plist)
    bitmap = (unsigned char *)malloc(bmsize);
  if (!plist || !bitmap) {
    if (plist)
      free(plist);
    return (-1);
  }

  /* clear bitmap */
  for (i = 0; i < bmsize; i++)
    bitmap[i] = 0;

  dir = 3;
  xstart = ix;
  ystart = iy;
  tapdir = 1 - 2 * inside;

  do {
    ncol = 1;
    xnext = ix + dxout[dir] + dxnext[dir];
    ynext = iy + dyout[dir] + dynext[dir];
    sliceGetVal(sl, xnext, ynext, val);
    if (val[0] != fillval) {
      /* case 1: inside corner */
      ix = xnext;
      iy = ynext;
      dir = (dir + 3) % 4;
      if (inside)
        ncol = ntaper + 1;
    } else {
      xnext = ix + dxnext[dir];
      ynext = iy + dynext[dir];
      sliceGetVal(sl, xnext, ynext, val);
      if (val[0] != fillval) {
        /* case 2: straight edge to next pixel */
        ix = xnext;
        iy = ynext;
      } else {
        /* case 3: outside corner, pixel stays the same */
        dir = (dir + 1) % 4;
        if (!inside)
          ncol = ntaper + 1;
      }
    }
      
    /* If outside pixel is outside the data, nothing to add to lists */
    xp = ix + dxout[dir];
    yp = iy + dyout[dir];
    if (xp < 0 || xp >= xsize || yp < 0 || yp >= ysize)
      continue;

    /* Loop on all the pixels to mark */
    for (col = 0; col < ncol; col++) {
      for (row = 1 - inside; row <= ntaper - inside; row++) {
        xp = ix + tapdir * row * dxout[dir] - col * dxnext[dir];
        yp = iy + tapdir * row * dyout[dir] - col * dynext[dir];

        /* skip if pixel outside area */
        if (xp < 0 || xp >= xsize || yp < 0 || yp >= ysize)
          continue;

        /* skip marking outside pixels for inside taper or
           inside pixels for outside taper */
        sliceGetVal(sl, xp, yp, val);
        if ((inside && val[0] == fillval) || 
            (!inside && val[0] != fillval))
          continue;

        dist = col * col + row * row;
        ind = xsize * yp + xp;
        if (bitmap[ind / 8] & (1 << (ind % 8))) {

          /* If the pixel is already marked, find it on the
             list and see if it's closer this time */
          for (i = plsize - 1; i >= 0; i--) {
            if (plist[i].x == xp && plist[i].y == yp) {
              if (plist[i].dist > dist) {
                plist[i].dist = dist;
                plist[i].dx = (signed char)(ix - xp);
                plist[i].dy = (signed char)(iy - yp);
              }
              break;
            }
          }
        } else {

          /* Otherwise, mark pixel in bitmap and make a new 
             entry on the list */
          bitmap[ind / 8] |= (1 << (ind % 8));
          if (plsize >= pllimit) {
            pllimit += PLIST_CHUNK;
            plist = (struct pixdist *) realloc(plist,
                                               pllimit * sizeof(struct pixdist));
            if (!plist) {
              free (bitmap);
              return (-1);
            }
          }
          plist[plsize].x = xp;
          plist[plsize].y = yp;
          plist[plsize].dist = dist;
          plist[plsize].dx = (signed char)(ix - xp);
          plist[plsize++].dy = (signed char)(iy - yp);
        }
      }
    }

  } while (ix != xstart || iy != ystart || dir != 3);

  /* make tables of fractions and amounts to add of fillval */
  for (i = 1; i <= ntaper; i++) {
    dist = inside ? i : ntaper + 1 - i;
    fracs[i] = (float)dist / (ntaper + 1);
    fillpart[i] = (1. - fracs[i]) * fillval;
  }

  /* Process the pixels on the list */
  for (i = 0; i < plsize; i++) {
    ind = sqrt((double)plist[i].dist) + inside;
    if (ind > ntaper)
      continue;
    ix = plist[i].x;
    iy = plist[i].y;
    if (inside) {
      xp = ix;
      yp = iy;
    } else {
      xp = ix + plist[i].dx;
      yp = iy + plist[i].dy;
    }
    sliceGetVal(sl, xp, yp, val);
    /*  val[0] = fracs[plist[i].dist] * val[0] + fillpart[plist[i].dist]; */
    val[0] = fracs[ind] * val[0] + fillpart[ind];
    slicePutVal(sl, ix, iy, val);
  }

  free(plist);
  free(bitmap);
  return 0;
}


/*!
 * Extracts a subarea of an image, places it into the center of a potentially 
 * larger array with padding, and tapers the image down to the mean value at
 * its edge, tapering pixels inside the extracted image area.  The image data
 * are in [array], their X dimension is [nxdimin], and their SLICE_MODE is 
 * given in [type].  The starting and ending coordinates to extract in X and Y
 * are [ix0] to [ix1] and [iy0] to [iy1].  The output image array is [brray] 
 * and its X dimension is specified by [nxdim].  The padded image size is 
 * specified by [nx] and [ny], and [nxtap] and [nytap] indicate the number of
 * pixels over which to taper in X and Y.
 */
void sliceTaperInPad(void *array, int type, int nxdimin, int ix0, int ix1,
                     int iy0, int iy1, float *brray, int nxdim, int nx, int ny,
                     int nxtap, int nytap)
{
  int lobase, hibase, x1, x2, ixlo, ixhi, iylo, iyhi, ix, iy, ixbase;
  int nsum, nxbox, nybox, ixlim;
  float sum, dmean, fracx, fracy, fmin;
  b3dUByte *bytein;
  b3dInt16 *intin;
  b3dUInt16 *uintin;
  float *floatin;
  float *out;
  
  nxbox = ix1 + 1 - ix0;
  nybox = iy1 + 1 - iy0;
  ixlo=nx/2-nxbox/2 - 1;
  ixhi=ixlo+nxbox;
  iylo=ny/2-nybox/2 - 1;
  iyhi=iylo+nybox;
  
  for (iy = iy0; iy <= iy1; iy++) {
    out = brray + ixlo + 1 + (iylo + 1 + iy - iy0) * nxdim;
    switch (type) {
    case SLICE_MODE_BYTE:
      bytein = (b3dUByte *)array + ix0 + iy * nxdimin;
      for (ix = ix0; ix <= ix1; ix++)
        *out++ = *bytein++;
      break;
      
    case SLICE_MODE_SHORT:
      intin = (b3dInt16 *)array + ix0 + iy * nxdimin;
      for (ix = ix0; ix <= ix1; ix++)
        *out++ = *intin++;
      break;
         
    case SLICE_MODE_USHORT:
      uintin = (b3dUInt16 *)array + ix0 + iy * nxdimin;
      for (ix = ix0; ix <= ix1; ix++)
        *out++ = *uintin++;
      break;
         
    case SLICE_MODE_FLOAT:
      floatin = (float *)array + ix0 + iy * nxdimin;
      for (ix = ix0; ix <= ix1; ix++)
        *out++ = *floatin++;
      break;

    }
  }

  /* get edge mean */
  sum=0.;
  lobase = (iylo + 1) * nxdim;
  hibase = iyhi * nxdim;
  for (ix = ixlo + 1; ix <= ixhi; ix++)
    sum += brray[ix + lobase] + brray[ix + hibase];
  for (iy = iylo + 2; iy < iyhi; iy++) {
    ixbase = iy * nxdim;
    sum += brray[ixlo + 1 + ixbase] + brray[ixhi + ixbase];
  }
  
  nsum=(2*(nxbox+nybox-2));
  dmean=sum/nsum;
    
  /* fill the rest of the array with dmean */
  if (nxbox != nx || nybox != ny) {
    for (iy = iylo + 1; iy <= iyhi; iy++) {
      ixbase = iy * nxdim;
      for (ix = 0; ix <= ixlo; ix++)
        brray[ix + ixbase] = dmean;
      for (ix = ixhi + 1; ix < nx; ix++)
        brray[ix + ixbase] = dmean;
    }
    for (iy = 0; iy <= iylo; iy++) {
      ixbase = iy * nxdim;
      for (ix = 0; ix < nx; ix++)
        brray[ix + ixbase] = dmean;
    }
    for (iy = iyhi + 1; iy < ny; iy++) {
      ixbase = iy * nxdim;
      for (ix = 0; ix < nx; ix++)
        brray[ix + ixbase] = dmean;
    }
  }
  
  /* Taper the edges */
  for (iy = 0; iy < (nybox+1)/2; iy++) {
    fracy=1.;
    ixlim = nxtap;
    if (iy < nytap) {
      fracy=(iy + 1.)/(nytap+1.);
      ixlim = (nxbox+1)/2;
    }
    for (ix = 0; ix < ixlim; ix++) {
      fracx=1.;
      if (ix < nxtap)
        fracx=(ix + 1.)/(nxtap+1.);
      fmin=fracx < fracy ? fracx : fracy;
      if(fmin < 1.) {
        x1 = ix + 1 + ixlo;
        x2 = ixhi - ix;
        lobase = (iy + 1 + iylo) * nxdim;
        hibase = (iyhi - iy) *nxdim;
        brray[x1 + lobase]=fmin*(brray[x1 + lobase]-dmean)+dmean;
        brray[x1 + hibase]=fmin*(brray[x1 + hibase]-dmean)+dmean;
        brray[x2 + lobase]=fmin*(brray[x2 + lobase]-dmean)+dmean;
        brray[x2 + hibase]=fmin*(brray[x2 + hibase]-dmean)+dmean;
      }
    }
  }
}

/*!
 * Returns [num] if it is even and has no prime factor greater than [limit],
 * or makes the number even and adds [idnum] until it reaches a value with this
 * property.  Values of 2 for [idnum] and 19 for [limit] will give a value
 * suitable for taking the FFT with the routines in IMOD.
 */
int niceFrame(int num, int idnum, int limit)
{
  int numin, numtmp, ifac;
  numin=2 * ((num + 1) / 2);
  do {
    numtmp=numin;
    for (ifac = 2; ifac <= limit; ifac++)
      while (numtmp % ifac == 0)
        numtmp=numtmp/ifac;
    
    if (numtmp > 1)
      numin += idnum;
  } while (numtmp > 1);
  return numin;
}


/*
    $Log$
    Revision 3.8  2007/08/15 00:06:21  mast
    Added taperinpad function, renamed from xcorr

    Revision 3.7  2007/05/27 21:17:57  mast
    Added taper routine here, and documented

    Revision 3.6  2005/11/11 22:15:23  mast
    Changes for unsigned file mode

    Revision 3.5  2005/03/09 21:17:15  mast
    Converted diffusion to float, removed processor argument

    Revision 3.4  2005/02/10 22:03:30  mast
    Need to include stdlib.h for mallocs to work in 64-bit land

    Revision 3.3  2005/01/28 05:41:11  mast
    Needed separate byte routine for anisotropic diffusion

    Revision 3.2  2005/01/27 05:56:56  mast
    Added anisotropic diffusion

    Revision 3.1  2005/01/07 20:00:45  mast
    Moved to libiimod to make available to clip, added median filter

    Revision 3.6  2004/12/22 15:21:15  mast
    Fixed problems discovered with Visual C compiler

    Revision 3.5  2004/11/07 23:01:27  mast
    Really fixed scaling, used short slices to prevent loss of resolution

    Revision 3.4  2004/11/05 19:08:12  mast
    Include local files with quotes, not brackets

    Revision 3.3  2004/10/29 22:16:55  mast
    Fixed some scaling problems

    Revision 3.2  2004/09/24 18:15:55  mast
    Fixed bug in Sobel filter

    Revision 3.1  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

*/
/* DNM 11-11-98: changed loops for filling in the edges after processing
   so that the corners would not have spurious data */

