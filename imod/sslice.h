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
/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.5  2003/12/18 22:45:56  mast
    changes for movieing

    Revision 3.4  2003/03/03 22:18:41  mast
    Added variable for keeping track of cursor

    Revision 3.3  2003/02/10 20:41:56  mast
    Merge Qt source

    Revision 3.2.2.2  2003/01/10 23:55:34  mast
    moved declaration of cubicFillin to include file

    Revision 3.2.2.1  2003/01/06 15:48:11  mast
    Qt version

    Revision 3.2  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

    Revision 3.1  2002/09/05 16:02:20  mast
    Add a flag for whether the cube is in a doublebuffered visual

*/

#ifndef SSLICE_H
#define SSLICE_H

#ifndef RADIANS_PER_DEGREE
#define RADIANS_PER_DEGREE 0.017453293
#endif 

#define SLICE_ZSCALE_OFF    0
#define SLICE_ZSCALE_BEFORE 1
#define SLICE_ZSCALE_AFTER  2

class SlicerWindow;
class SlicerGL;
class SlicerCube;
class QKeyEvent;
class QMouseEvent;

struct ViewInfo;
typedef struct b3d_ci_image B3dCIImage;
typedef struct imodel_matrix Imat;


typedef struct Super_slicer{

  SlicerWindow *qtWindow;
  SlicerGL     *glw;
  SlicerCube   *cube;
  
  int          maprowcol;
  int          scalez;
  int          mapped;
  int          locked;
  int          imageFilled;
  int          mousemode;    /* value for keeping track of cursor */

  struct ViewInfo *vi;
  B3dCIImage   *image;
  
  int    winx, winy;
  float  cx, cy, cz;  /* current x, y, z */
  float  lx, ly, lz;  /* last set point for x, y, z */
  float  zoom;
  float  depth;
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
  int movieSnapCount;     /* Counter for doing movie snapshots */
  
  short nslice;
  Imat  *mat;
  int   ctrl;
} SlicerStruct;     

void slicerCubicFillin(unsigned short *cidata, int winx, int winy, int izoom,
		  int ilimshort, int jlimshort, int minval, int maxval);
int sslice_open(struct ViewInfo *vi);
void slicerReportAngles();
void slicerHelp();
void slicerStepZoom(SlicerStruct *win, int dir);
void slicerEnteredZoom(SlicerStruct *win, float newZoom);
void slicerShowSlice(SlicerStruct *win);
void slicerStateToggled(SlicerStruct *win, int index, int state);
void slicerAngleChanged(SlicerStruct *sslice, int axis, int value, 
			int dragging);
void slicerZscale(SlicerStruct *sslice, int item);
void slicerImageThickness(SlicerStruct *sslice, int sno);
void slicerModelThickness(SlicerStruct *sslice, float depth);
void slicerResize(SlicerStruct *sslice, int winx, int winy);
void slicerCubeResize(SlicerStruct *sslice, int winx, int winy);
void slicerClosing(SlicerStruct *sslice);
void slicerKeyInput(SlicerStruct *sslice, QKeyEvent *event);
void slicerKeyRelease(SlicerStruct *sslice, QKeyEvent *event);
void slicerMousePress(SlicerStruct *sslice, QMouseEvent *event);
void slicerPaint(SlicerStruct *win);
void slicerCubePaint(SlicerStruct *ss);



#endif


