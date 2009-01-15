//Added by qt3to4:
#include <QMouseEvent>
#include <QKeyEvent>
/*
 *  sslice.h -- Include file for slicer.c, slicer window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#ifndef SSLICE_H
#define SSLICE_H

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
typedef struct Mod_Point Ipoint;

typedef struct Super_slicer{

  SlicerWindow *qtWindow;
  SlicerGL     *glw;
  SlicerCube   *cube;
  
  int          scalez;
  int          locked;
  int          mousemode;    /* value for keeping track of cursor */
  int          classic;
  int          shiftLock;    /* toggle button state for shift key or mouse */

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
  float xsz, ysz, zsz;
  float xo, yo, zo;
  
  float tang[3];    /* transform angles */
  float lang[3];    /* last angles drawn */
  float xstep[3];   /* change in x, y, z image for step in x' slicer */
  float ystep[3];   /* change in x, y, z image for step in y' slicer */
  float zstep[3];
  int timeLock;     /* Time value if time is locked */
  
  float xzoom, yzoom;
  int   imageFilled;
  bool  noPixelZoom;   /* Flag that image data are to be drawn at 1:1 xoom */
  float oneSliceMean, oneSliceSD;   /* mean and SD of single center slice */
  bool  scaleToMeanSD; /* Flag that multi-slice should be scaled to single */

  float xshift, yshift;   /* Shifts to apply to raster for low-res draw */
  float zslast;           /* Value of zscale on last draw_cb */
  int pending;            /* Flag that there are pending coords from hit */
  float pendx, pendy, pendz;   /* pending coords */
  int movieSnapCount;     /* Counter for doing movie snapshots */
  float origZmouse;       /* Original zxmouse when page up/down started */
  float origAngles[3];    /* Original angles at that time */
  float lastXmouse;       /* Last xmouse left by a page up/down */
  float lastYmouse;
  float lastZmouse;
  float cumPageMoves;     /* Cumulative page up/down move since original */
  float drawnXmouse;      /* xmouse, etc when last drawn */
  float drawnYmouse;
  float drawnZmouse;
  int ignoreCurPtChg;     /* Flag to ignore a current point change in draw */
  float scaleBarSize;     /* Actual size of draw scale bar in model units */

  short nslice;       /* Number of slices to draw */
  Imat  *mat;
  int   ctrl;
  int   fftMode;      /* Flag to do FFT of slice */
  int   toolTime;     /* Value of time when label sent to toolbar */
  bool  continuous;   /* Flag for continuous mode in slicer angle update */
  bool  linked;       /* Flag for linked to other slicers */
  bool  needDraw;     /* Flag that window needs a draw regardless of flags*/
  int   drawModView;  /* Draw flag to draw model view on next redraw */
  bool  alreadyDrew;  /* Flag that window was already drawn before draw_cb */
  int   closing;      /* Flag for window closing */
} SlicerStruct;     

void slicerCubicFillin(unsigned short *cidata, int winx, int winy, int izoom,
		  int ilimshort, int jlimshort, int minval, int maxval);
int sslice_open(struct ViewInfo *vi);
void slicerReportAngles();
SlicerStruct *getTopSlicer();
void slicerPixelViewState(bool state);
void slicerHelp();
void slicerStepZoom(SlicerStruct *win, int dir);
void slicerStepTime(SlicerStruct *win, int dir);
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
void slicerMouseRelease(SlicerStruct *sslice, QMouseEvent *event);
void slicerMouseMove(SlicerStruct *sslice, QMouseEvent *event);
void slicerPaint(SlicerStruct *win);
void slicerCubePaint(SlicerStruct *ss);
void slice_trans_step(SlicerStruct *ss);
float slicerGetZScaleBefore(SlicerStruct *ss);
void slicerSetForwardMatrix(SlicerStruct *ss);
int slicerAnglesFromContour(SlicerStruct *ss);
void slicerCheckMovieLimits(SlicerStruct *ss);
int setTopSlicerAngles(float angles[3], Ipoint *center, bool draw);
int getTopSlicerAngles(float angles[3], Ipoint *center, int &time);
int getTopSlicerTime(bool &continuous);
int slicerAnglesOpen();
void slicerAnglesClosing();
void slicerNewTime(bool refresh);
void slicerSetCurrentOrNewRow(SlicerStruct *ss, bool newrow);
void slicerSetAnglesFromRow(SlicerStruct *ss);
int setTopSlicerFromModelView(Ipoint *rot);
#endif

/*
    $Log$
    Revision 3.21  2008/11/29 22:10:29  mast
    Added ability to link slicers

    Revision 3.20  2008/01/25 20:22:58  mast
    Changes for new scale bar

    Revision 3.19  2007/11/30 06:51:50  mast
    Changes for linking slicer to model view

    Revision 3.18  2007/09/20 22:05:36  mast
    Defined RADIANS_PER_DEGREE once

    Revision 3.17  2007/06/15 21:19:23  mast
    Added shift lock

    Revision 3.16  2007/06/04 15:07:31  mast
    A few more variables to keep track of drawn position

    Revision 3.15  2007/05/31 16:32:28  mast
    Changes for slicer angle toolbar, classic setting and warning

    Revision 3.14  2007/05/29 14:52:47  mast
    Changes for new slicer mode

    Revision 3.13  2007/05/25 05:28:16  mast
    Changes for addition of slicer angle storage

    Revision 3.12  2007/03/29 04:55:49  mast
    Fixed crash bug when closing window while focus is in edit/spinbox

    Revision 3.11  2006/10/12 19:02:55  mast
    Added toolbar button for W function

    Revision 3.10  2006/10/06 19:35:50  mast
    Added declarations for the fillin routine to be in slicer_classes

    Revision 3.9  2006/09/17 18:15:59  mast
    Changes to provide mouse position to pixelview

    Revision 3.8  2006/09/12 15:35:39  mast
    Added mouse move slot

    Revision 3.7  2005/03/08 15:48:49  mast
    Added enum for toolbar toggles

    Revision 3.6  2004/08/12 17:05:17  mast
    Added message to get slicer angles

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
