/*  IMOD VERSION 2.20
 *
 *  sliceproc.c -- image slice support for iproc.c
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1998 by Boulder Laboratory for 3-Dimensional Fine    *
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
/*  $Author$

    $Date$

    $Revision$

    $Log$
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

#include <math.h>
#include "mrcc.h"
#include "sliceproc.h"

/* DNM 11/7/04: All routines expect the min and max of the input slice to be
   set to the desired range for scaling the output */

#define RANGE(s, i, j)  (((i)>0)&&((j)>0)&&((i)<(s)->xsize)&&((j)<(s)->ysize))
#define GETVAL(s, i, j) ((s)->data.b[(i) + ((j) * (s)->xsize)])
#define GETSVAL(t, i, j) ((t)->data.s[(i) + ((j) * (t)->xsize)])
#define GETFVAL(s, i, j) ((s)->data.f[(i) + ((j) * (s)->xsize)])

int sliceByteConvolve(Islice *sin, int mask[3][3]);
static void sliceScaleAndFree(Islice *sout, Islice *sin);

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

int sliceByteEdgeSobel(Islice *sin)
{
  return(sliceByteEdgeTwo(sin, 2));
}
int sliceByteEdgePrewitt(Islice *sin)
{
  return(sliceByteEdgeTwo(sin, 1));
}
int sliceByteEdgeLaplacian(Islice *sin)
{
  return(sliceByteConvolve(sin, LaplacianKernel));
}

int sliceByteSharpen(Islice *sin)
{
  return(sliceByteConvolve(sin, SharpenKernel));
}

/* DNM 11/07/04: Removed smoothing routine that did the same as convolving with
   this kernel */

int sliceByteSmooth(Islice *sin)
{
  return(sliceByteConvolve(sin, SmoothKernel));
}

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

/* A routine for finding min and max as rapidly as possible for the three basic
   data modes */
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
  if (sout->mode == SLICE_MODE_FLOAT)
    for(i = 0; i < imax; i++)
      sin->data.b[i] = (unsigned char)(sout->data.f[i] * mval + aval);
  else if (sout->mode == SLICE_MODE_SHORT)
    for(i = 0; i < imax; i++)
      sin->data.b[i] = (unsigned char)(sout->data.s[i] * mval + aval);
  else
    for(i = 0; i < imax; i++)
      sin->data.b[i] = (unsigned char)(sout->data.b[i] * mval + aval);

  sliceFree(sout);
}
