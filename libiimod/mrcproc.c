/*  IMOD VERSION 2.02
 *
 *  mrcproc.c -- Proccessing functions for mrcfiles.
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
#include <stdio.h>
#include <math.h>
#include "mrcfiles.h"
#include "mrcproc.h"
#include "b3dutil.h"


int mrc_getdsize(int mode, int *channel)
{
     int dsize;
     int tchan;

     switch (mode){
	case MRC_MODE_BYTE:
	  dsize = sizeof(unsigned char);
	  tchan = 1;
	  break;
	case MRC_MODE_SHORT:
	  dsize = sizeof(short);
	  tchan = 1;
	  break;
	case MRC_MODE_FLOAT:
	  dsize = sizeof(float);
	  tchan = 1;
	  break;
	case MRC_MODE_COMPLEX_SHORT:
	  dsize = sizeof(short);
	  tchan = 2;
	  break;
	case MRC_MODE_COMPLEX_FLOAT:
	  dsize = sizeof(float);
	  tchan = 2;
	  break;
	case MRC_MODE_RGB:
	  dsize = sizeof(unsigned char);
	  tchan = 3;
	  break;
	default:
	  return(0);
     }
     if (channel)
	  *channel = tchan;

     return(dsize);
}


void *mrc_rotate(void *data, int xsize, int ysize, double angle, 
		 int cx, int cy, int mode)
{
     void *obuf;
     int dsize;
     int i, j;
     double x, y;
     double sino, coso;
     float val;

     angle *= 0.017453293;
     sino = sin(angle);
     coso = sin(angle);

     if (!data)
	  return(NULL);
     
     dsize = mrc_getdsize(mode, NULL);
     
     obuf = (void *)malloc(xsize * ysize * dsize);
     
     if (!obuf)
	  return(NULL);
     
     for(j = 0; j < ysize; j++){
	  for (i = 0; i < xsize; i++){
	       x = cx + ((i - cx) * coso) - ((cy - j) * sino);
	       y = cy + ((i - cx) * sino) - ((cy - j) * coso);
	       mrc_bilinear(&val, x, y, data, xsize, ysize, mode);
	       mrc_putval(&val, obuf, i + (j * xsize), mode, 0);
	  }
     }
     return(obuf);
}

void *mrc_translate(void *data, double xt, double yt, int xsize, int ysize,
		    int xout, int yout, int mode)
{
     void  *obuf;
     int dsize;
     int i, j;
     double x, y;
     float val;

     if (!data)
	  return(NULL);

     dsize = mrc_getdsize(mode, NULL);

     obuf = (void *)malloc(xout * yout * dsize);

     if (!obuf)
	  return(NULL);
     
     for(j = 0; j < yout; j++){
	  y = (double)j + yt;
	  for (i = 0; i < xout; i++){
	       x = (double)i + xt;
	       mrc_bilinear(&val, x, y, data, xsize, ysize, mode);
	       mrc_putval(&val, obuf, i + (j * xout), mode, 0);
	  }
     }
     return(obuf);
}

/*
 * mrc_zoom() - zoom a slice 
 * -------------------------
 * ibuf - image data input.
 * xin, yin - size of ibuf.
 * xout, yout - used for scale factors.
 * xoutsize, youtsize - size of returned output buffer.
 */

void *mrc_zoom(void *ibuf, int xin, int yin, int xout, int yout, int mode,
	       int xoutsize, int youtsize)
{
     void  *obuf;
     float xzoom, yzoom, val;
     double x, y;
     int dsize;
     int i, j;

     if (!ibuf)
	  return(0);

     if ((!xin) || (!xout))
	  return(NULL);
     xzoom = (float)xin / (float)xout;

     if (!yin){
	  yin = 0; yout = 0;
	  yzoom = 0;
     }else{
	  if (!yout)
	       return(NULL);
	  yzoom = (float)yin / (float)yout;
     }
     
     dsize = mrc_getdsize(mode, NULL);

     obuf = (void *)malloc(xoutsize * youtsize * dsize);
     if (!obuf)
	  return(NULL);

     for(j = 0; j < youtsize; j++){
	  y = (double)((float)j * yzoom);
	  for (i = 0; i < xoutsize; i++){
	       x = (double)((float)i * xzoom);
	       mrc_bilinear(&val, x, y, ibuf, xin, yin, mode);
	       mrc_putval(&val, obuf, i + (j * xoutsize), mode, 0);
	  }
     }
     return(obuf);
}

void *mrc_zoom_center(void *ibuf, int xsize, int ysize, int mode,
		      double xzoom, double yzoom)
{
      void  *obuf;
      double x, y;
      int dsize;
      int i, j;
      int cx, cy;
      float val;

      dsize = mrc_getdsize(mode, NULL);
      obuf = (void *)malloc(xsize * ysize * dsize);
      if (!obuf)
	   return(NULL);
      cx = xsize / 2;
      cy = ysize / 2;
      for(j = 0; j < ysize; j++){
	   y = (double)((float)(j - cy) * yzoom) + cy;
	   for (i = 0; i < xsize; i++){
		x = (double)((float)(i - cx) * xzoom) + cx;
		mrc_bilinear(&val, x, y, ibuf, xsize, ysize, mode);
		mrc_putval(&val, obuf, i + (j * xsize), mode, 0);
	   }
      }
      return(obuf);
}

int mrc_bilinear(float *val, double x, double y,
		 void *buf, int xsize, int ysize, int mode)
{

     int x1, x2;
     int y1, y2;

     float v1, v2, v3, v4;
     float tv1, tv2;
     double t, u;

     if ((x < 0) || (y < 0) || (x > (xsize - 1)) ||  (y > (ysize - 1))){
	  *val = 0;
	  return(0);
     }

     x1 = x;
     y1 = y;
     x2 = x1 + 1;
     y2 = y1 + 1;

     if ((x2 >= xsize) || (y2 >= ysize)){
	  mrc_getval(val, buf, x1 + (y1 * xsize), mode, 0);
	  return(0);
     }

     if ( ( (double)y1 == y) && ((double)x1 == x)){
	   mrc_getval(val, buf, x1 + (y1 * xsize), mode, 0);
	   return(0);
      }

     if ( (double)y1 == y){
	  mrc_getval(&tv1, buf, x1 + (y1 * xsize), mode, 0);
	  mrc_getval(&tv2, buf, x2 + (y1 * xsize), mode, 0);
	  if (tv1 == tv2){
	       *val = tv1;
	       return(0);
	  }
	  *val = tv1 + ((y - (double)y1) * (tv2 - tv1));
	  return(0);
     }

     if ( (double)x1 == x){
	  mrc_getval(&tv1, buf, x1 + (y1 * xsize), mode, 0);
	  mrc_getval(&tv2, buf, x1 + (y2 * xsize), mode, 0);
	  if (tv1 == tv2){
	       *val = tv1;
	       return(0);
	  }
	  *val = tv1 + ((y - (double)y1) * (tv2 - tv1));
	  return(0);
     }

     mrc_getval(&v1, buf, x1 + (y1 * xsize), mode, 0);
     mrc_getval(&v2, buf, x2 + (y1 * xsize), mode, 0);
     mrc_getval(&v3, buf, x2 + (y2 * xsize), mode, 0);
     mrc_getval(&v4, buf, x1 + (y2 * xsize), mode, 0);
     
     if (v1 == v2){
	  tv1 = v1;
     }else{
	  tv1 = v1 + ((x - (double)x1) * (v2 - v1));
     }
     
     if (v3 == v4){
	  tv2 = v1;
     }else{
	  tv2 = v4 + ((x - (double)x1) * (v3 - v4));
     }

     
     if (tv1 == tv2){
	  *val = tv1;
	  return(0);
     }

     *val = tv1 + ((y - (double)y1) * (tv2 - tv1));
     return(0);
}

int mrc_biquad(float *val, double x, double y, 
	       void *buf, int xsize, int ysize, int mode)
{
     int x1, y1, x2, y2, x3, y3;
     float v1, v2, v3;

     /* check input */
     if (!buf)          return(-1);
     if (x > xsize - 1) return(-1);
     if (y > ysize -1)  return(-1);
     if (x < 0)         return(-1);
     if (y < 0)         return(-1);

     x2 = x + 0.5;
     y2 = y + 0.5;
     if (x2 >= xsize) x2--;
     if (y2 >= ysize) y2--;
     x1 = x2 - 1;
     y1 = y2 - 1;
     x3 = x2 + 1;
     y3 = y2 + 1;

     
	  
     /* just return closest point for now. */
     mrc_getval(val, buf, x2 + (y2 * xsize), mode, 0); 

     return(0);
}

/*
int mrc_quad(float *retval, double x, float *v1, float *v2, float *v3)
{
     return(0);
}
*/

int mrc_putval(float *val, void *buf, int index, int mode, int channel)
{
     unsigned char *bdat;
     short *sdat;
     float *fdat;

     switch (mode)
	  {
	     case MRC_MODE_BYTE:
	       bdat = (unsigned char *)buf;
	       bdat[index] = ( unsigned char )*val;
	       break;
	     case MRC_MODE_SHORT:
	       sdat = (short *)buf;
	       sdat[index] = (short)*val;
	       break;
	     case MRC_MODE_FLOAT:
	       fdat = (float *)buf;
	       fdat[index] = *val;
	       break;
	     case MRC_MODE_COMPLEX_SHORT:
	       sdat = (short *)buf;
	       sdat[(index * 2) + channel] = (short)*val;
	       break;
	     case MRC_MODE_COMPLEX_FLOAT:		    
	       fdat = (float *)buf;
	       fdat[(index * 2) + channel] = *val;
	       break;
	     case MRC_MODE_RGB:
	       bdat = (unsigned char *)buf;
	       bdat[(index * 3) + channel] = (unsigned char )*val;
	       break;
	     default:
	       b3dError(stderr, "mrc_putval: unknown mode.\n");
	       return(-1);
	  }
     return(0);     
     

}

/* get a float value from an mrc slice */
int mrc_getval(float *val, void *buf, int index, int mode, int channel)
{
     unsigned char *bdat;
     short *sdat;
     float *fdat;

     switch (mode)
	  {
	     case MRC_MODE_BYTE:
	       bdat = (unsigned char *)buf;
	       *val = (float)bdat[index];
	       break;
	     case MRC_MODE_SHORT:
	       sdat = (short *)buf;
	       *val = (float)sdat[index];
	       break;
	     case MRC_MODE_FLOAT:
	       fdat = (float *)buf;
	       *val = fdat[index];
	       break;
	     case MRC_MODE_COMPLEX_SHORT:
	       sdat = (short *)buf;
	       *val = (float)sdat[(index * 2) + channel];
	       break;
	     case MRC_MODE_COMPLEX_FLOAT:		    
	       fdat = (float *)buf;
	       *val = fdat[(index * 2) + channel];
	       break;
	     case MRC_MODE_RGB:
	       bdat = (unsigned char *)buf;
	       *val = (float)bdat[(index * 3) + channel];
	       break;
	     default:
	       b3dError(stderr, "mrc_getval: unknown mode.\n");
	       return(-1);
	  }
     return(0);
}

int mrc_copy_pixel( void *from, int findex, void *to, int tindex, int mode)
{
     unsigned char *bin, *bout;
     short *sin, *sout;
     float *fin, *fout;

     switch (mode)
	  {
	     case MRC_MODE_BYTE:
	       bin  = (unsigned char *)from;
	       bout = (unsigned char *)to;
	       bout[tindex] = bin[findex];
	       break;

	     case MRC_MODE_SHORT:
	       sin  = (short *)from;
	       sout = (short *)to;
	       sout[tindex] = sin[findex];

	     case MRC_MODE_FLOAT:
	       fin  = (float *)from;
	       fout = (float *)to;
	       fout[tindex] = fin[findex];
	       break;

	     case MRC_MODE_COMPLEX_SHORT:
	       sin  = (short *)from;
	       sout = (short *)to;
	       sout[tindex * 2] = sin[findex * 2];
	       sout[(tindex * 2) + 1] = sin[(findex * 2) + 1];
	       break;

	     case MRC_MODE_COMPLEX_FLOAT:
	       fin  = (float *)from;
	       fout = (float *)to;
	       fout[tindex * 2] = fin[findex * 2];
	       fout[(tindex * 2) + 1] = fin[(findex * 2) + 1];
	       break;

	     case MRC_MODE_RGB:
	       bin  = (unsigned char *)from;
	       bout = (unsigned char *)to;
	       bout[tindex * 3] = bin[findex * 3];
	       bout[(tindex * 3) + 1] = bin[(findex * 3) + 1];
	       bout[(tindex * 3) + 2] = bin[(findex * 3) + 2];

	     default:
	       return(-1);
	  }
     return(0);
}



int mrc_slice_mmm(void *data, int xsize, int ysize, int mode, 
		  float *min, float *max, float *mean)
{
     int i;
     int xysize;
     float val;

     xysize = xsize * ysize;
     mrc_getval(min, data, 0, mode, 0);
     *min = *max;

     for (i = 1; i < xysize; i++){
	  mrc_getval(&val, data, i, mode, 0);
	  if (*min > val)
	       *min = val;
	  if (*max < val)
	       *max = val;
	  *mean += val;
     }
     *mean /= (float)xysize;
     return(0);
}
