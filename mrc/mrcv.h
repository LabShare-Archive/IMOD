/*  IMOD VERSION 2.02
 *
 *  mrcv.h
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

#include <gl/gl.h>
#include "imodel.h"

/* Defines for using cmap mode, byte data. */

#define RAMPBASE 256
#define RAMPSIZE 256
#define RAMPSTEP 1
#define RCOLOR(rcolor) ((rcolor) + RAMPBASE)

#define NTSCX 635l     /* NTSC display size. */
#define NTSCY 485l
#define NTSCX_OVER 30  /* Number of pixels in overscan area. */
#define NTSCY_OVER 45
#define NTSCY_OVER_TOP 22
#define NTSCY_CENTER 243

struct ViewInfo{
     unsigned char **idata;
     /*  Colorindex *data; */
     Colorindex *viewdata;
     /*  Colorindex *maxdata; */
     int xsize;
     int ysize;
     int zsize;
     int xysize;
     int xmouse;
     int ymouse;
     int zmouse;
     int xtrans;
     int ytrans;
     int ztrans;
     int rampbase;
     int rampsize;
     int white;
     int black;
     int reverse;
     int falsecolor;
     int movie;
     int frate;
     float zoom;
     float blowup;
     int        xborder;   /* drawing sub regions */
     int        yborder;
     int        xdrawsize;
     int        ydrawsize;
     int        xstart;
     int        ystart;
     Screencoord llx;
     Screencoord lly;
     Screencoord urx;
     Screencoord ury;
     Imod *imod;
};


struct RGBViewInfo{
     
     unsigned long *idata;  /* x * y * z data array in argb format */
     unsigned long *sdata;  /* sub-area data */
     int xsize;
     int ysize;
     int zsize;
     
     int xtrans;
     int ytrans;
     int z;

     int llx, lly, urx, ury;

     int frate;
     float zoom;
};



/* Functions */

/* movie.c  */
int movie_image(struct ViewInfo *vi);
int movieinput(struct ViewInfo *vi);

/* mrcv.c   */
void print_mrcv_inst(void);
void show_image(struct ViewInfo *vi);
void setview(struct ViewInfo *vi);
void adjustcmap_pf(int *low, int *high, int rampbase);
void init_vi(struct ViewInfo *vi);

extern int    Rampbase;
extern int    Verbose;
extern int    Mrcv_onside;
extern int    Mrcv_stereo;
extern int    Mrcv_plax;
extern int    Mrcv_fullwin;
extern int    Mrcv_xoff;
extern int    Mrcv_yoff;
extern int    Mrcv_xmont;
extern int    Mrcv_ymont;
extern int    Mrcv_border;
extern int    Mrcv_background;
extern int    Mrcv_szoom;
extern float  Mrcv_vidscale;

extern struct ViewInfo  *Mrcv_vi;
extern struct ViewInfo  *Mrcv1_vi;
extern struct ViewInfo  *Mrcv2_vi;
extern struct Mod_Model *Mrcv_model;
extern struct Mod_Draw  Mrcv_md;
