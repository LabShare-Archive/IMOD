/*  IMOD VERSION 2.02
 *
 *  $Id$
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

#ifndef MRCPROC_H
#define MRCPROC_H

/* prototypes */
#ifdef __cplusplus
extern "C" {
#endif

/* Get data and channel size for image type. */
int   mrc_getdsize(int mode, int *channel);

/* image transformation functions. */
void *mrc_rotate(void *data, int xsize, int ysize, double angle,
		 int cx, int cy, int mode);
void *mrc_translate(void *data, double xt, double yt, int xsize, int ysize,
		    int xout, int yout, int mode);
void *mrc_zoom(void *ibuf, int xin, int yin, int xout, int yout, int mode,
	       int xoutsize, int youtsize);
void *mrc_zoom_center(void *ibuf, int xsize, int ysize, int mode,
		      double xzoom, double yzoom);

/* linear interpolation functions. */
int   mrc_bilinear(float *val, double x, double y,
		 void *buf, int xsize, int ysize, int mode);
int mrc_biquad(float *val, double x, double y,
	       void *buf, int xsize, int ysize, int mode);

/* pixal operations. */
int   mrc_putval(float *val, void *buf, int index, int mode, int channel);
int   mrc_getval(float *val, void *buf, int index, int mode, int channel);
int   mrc_copy_pixel( void *from, int findex, void *to, int tindex, int mode);
int   mrc_slice_mmm(void *data, int xsize, int ysize, int mode,
		    float *min, float *max, float *mean);

#ifdef __cplusplus
}
#endif
#endif
