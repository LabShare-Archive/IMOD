/*****************************************************************************
 *                                                                           *
 *   FILE: xcramp.h                                                          *
 *                                                                           *
 *   PURPOSE: X11 color ramp control.                                        *
 *                                                                           *
 *   HISTORY:                                                                *
 *       Version 1.0  James Kremer  kremer@beagle.colorado.edu               *
 *                                                                           *
 *****************************************************************************
 *   Copyright (C) 1994,1995 by Boulder Laboratory for 3-Dimensional Fine    *
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

#ifndef XCRAMP_H
#define XCRAMP_H

#include <X11/Xlib.h>
#include <X11/Xutil.h>

typedef struct xbldrcoloramp
{
     Colormap      cmap;     /* The X Colormap               */
     Visual        *visual;  /* The X Visual                 */
     unsigned int  depth;    /* The X depth of colormap.     */
     Display       *display; /* The X Display                */
     XColor        *xcolor;  /* Allocated array of X-Colors. */
     
     int           *colors;  /* Array of colors              */
     unsigned long *pixels;  /* Array of pixel values.       */
     float         scale;    /* rampsize/255                 */

     int    rampsize;        /* The number of colors allocated.          */
     int    rampbase;        /* The number of first color.               */
     int    blacklevel;      /* The grey scale black level, default 0    */
     int    whitelevel;      /* The grey scale white level, default 255. */
     int    reverse;         /* Flag for reverse contrast                */
     int    falsecolor;      /* Use color instead of grey scale ramp.    */
     int    noflevels;       /* Number of levels available for storage.  */
     int    clevel;          /* current level index in use.              */
     int    *blacks;         /* Array of black levels.                   */
     int    *whites;         /* Array of white levels.                   */

     unsigned char table[256][3]; 
     unsigned short index[256];
     unsigned int   ramp[256];
} Cramp;

Cramp *xcramp_allinit(Display *display, XVisualInfo *visual, Colormap colormap,
		      unsigned long low, unsigned long high);
Cramp *xcramp_init(Display *display, XVisualInfo *visual, Colormap colormap);

int    xcramp_delete(Cramp *xcramp);
int    xcramp_level(Cramp *xcramp, int black, int white);
int    xcramp_falsecolor(Cramp *xcramp, int flag);
int    xcramp_reverse(Cramp *xcramp, int flag);
int    xcramp_ramp(Cramp *cr);
void   xcramp_mapfalsecolor(int gray, int *red, int *green, int *blue);
void   xcramp_setlevels(Cramp *xcramp, int black, int white);
unsigned short xcramp_getpixel(Cramp *xr, unsigned char val);
unsigned char xcramp_getval(Cramp *xr, unsigned long pixel);
void xcramp_getlevels(Cramp *xcramp, int *black, int *white);

int  xcrampStoreInit(Cramp *cramp, int size);
int  xcrampSelectIndex(Cramp *cramp, int index);
void xcrampNewBase(Cramp *cr, int base);

/* Scale and Ramp Pixels */
#define xcrampSPixel(r,v)  ((r)->pixels[(v) * (r)->scale])
#define xcrampRPixel(r,v) ((v) + (r)->rampbase)

#endif /* XCRAMP_H */



