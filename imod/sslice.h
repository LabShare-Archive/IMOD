/*  IMOD VERSION 2.40
 *
 *  sslice.h -- Include file for slicer.c, slicer window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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

#ifndef SSLICE_H
#define SSLICE_H

#ifndef RADIANS_PER_DEGREE
#define RADIANS_PER_DEGREE 0.017453293
#endif 

#define SLICE_ZSCALE_OFF    0
#define SLICE_ZSCALE_BEFORE 1
#define SLICE_ZSCALE_AFTER  2

/* used to show current slice */
struct imod_showslice_struct
{
     int zx1, zx2;
     int zy1, zy2;  
     int xy1, xy2;
     int xz1, xz2;
     int yx1, yx2;
     int yz1, yz2;     
};


struct Super_slicer{

     XtWorkProcId work_id;
     XtIntervalId iid;
     XtAppContext app;
     Widget       dialog;
     Widget       glw;
     Widget       cube;
     Widget       anglew[3];
     Widget       rowcol;

     Widget       wThick, wImageThick,wModelThick;
     Widget       zoomarrow;

     int          maprowcol;
     Pixmap       showpix, hidepix, lockpix, unlockpix;
     Pixmap       lowrespix, highrespix;
     Pixmap       zscalepix, znormpix;
     int          scalez;
     int          exposed;
     int          mapped;
     int          locked;
     int          expose_draw;

     struct ViewInfo *vi;
     B3dCIImage   *image;
     XID context;
     XID cubegc;

     int    winx, winy;
     float  cx, cy, cz;  /* current x, y, z */
     float  lx, ly, lz;  /* last set point for x, y, z */
     float  zoom;
     float  depth;
     int    ginit;
     int    closing;     /* flag that window is closing */
     int    lastangle;   /* Last angle slider that was used */

     /* coords for plane intersection */
     /* used for show slice location  */
     int zx1, zx2;
     int zy1, zy2;

     /* data for high res slicer */
     int   hq;
     float xs, ys;
     float xsz, ysz, zsz;
     float xo, yo, zo;
     int   yline;
     int   fasthq;

     /* slicer version 3 data. */
     float inangle[3]; /* three user input angles.                      */
     float tang[3];    /* transform angles in order given by order.     */
     float xstep[3];   /* change in x, y, z image for step in x' slicer */
     float ystep[3];   /* change in x, y, z image for step in y' slicer */
     float zstep[3];
     float bcoord[3];  /* beginning point in image to start rendering.  */
     float ccoord[3];  /* current point in rendering for workproc.      */


     float xzoom, yzoom;

     float xshift, yshift;   /* Shifts to apply to raster for low-res draw */
     float zslast;           /* Value of zscale on last draw_cb */
     int pending;            /* Flag that there are pending coords from hit */
     float pendx, pendy, pendz;   /* pending coords */

     short nslice;
     Imat  *mat;
     int   ctrl;
};     

int sslice_open(struct ViewInfo *vi);

#endif


