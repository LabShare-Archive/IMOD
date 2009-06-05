/*
 *  xzap.h -- Header file for ZaP Window.
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

#ifndef XZAP_H
#define XZAP_H

#include "control.h"
//Added by qt3to4:
#include <QMouseEvent>
#include <QKeyEvent>
#include <QEvent>

class ZapWindow;
class ZapGL;
class QString;
class QKeyEvent;
class QEvent;
class QMouseEvent;

typedef struct b3d_ci_image B3dCIImage;

typedef struct zapwin
{
  ZapWindow *qtWindow;               /* Zap window widget. */
  ZapGL *gfx;                  /* Image sub window.  */
  int    winx,      winy;      /* Image window size. */
  int    xborder,   yborder;   /* border around image window. */
  int    xstart,    ystart;
  int    xdrawsize, ydrawsize;
  int    xtrans,    ytrans,    ztrans;
  int    lmx,       lmy;

  int    ginit;

  int    hqgfx, hide;
  int    hqgfxsave;           /* Place to save hqgfx when dragging */
  int    drawCurrentOnly;

  int    rubberband;    /* Rubber banding flag and image inside coordinates */
  float  rbImageX0;
  float  rbImageX1;
  float  rbImageY0;
  float  rbImageY1;
  int    rbMouseX0;    /* mouse coords - always derived from image coords */
  int    rbMouseX1;
  int    rbMouseY0;
  int    rbMouseY1;
  int    startingBand;
  int    shiftingCont; /* Flag for shifting contour */
  int    bandChanged;  /* Flag that band changed, draw graph windows */
  Ipoint xformCenter;  /* Center defined by mouse */
  int    centerDefined;  /* Flag that center was defined with mouse */
  int    centerMarked;   /* Flag that center was displayed in extra object */
  int    shiftRegistered;  /* Flag that contour changes have been registered */
  int    shiftObjNum;    /* Extra object number for center marker */
  int    dragAddCount; /* Number of points added and not registered for undo */
  Iindex dragAddIndex; /* Starting obj, cont, point for first such point*/
  int    dragAddEnd;   /* Ending or last point not registered */

  int movieSnapCount; /* Counter if this window is doing movie snapshots */
  int recordSubarea;  /* Record the subarea on the next draw */
  bool   drewExtraCursor;  /* Flag that extra cursor was drawn */
  float scaleBarSize;  /* Size of scale bar that was last drawn */

  float  zoom;
  float  xzoom;    /* Possibly slightly different X zoom */
  char   *data;

  /* The graphic image buffer. */
  B3dCIImage *image;
  B3dCIImage **images;
  int numImages;

  int section;
  int sectionStep; /* auto step image after new model point. */
  int time;
  int overlay;     /* Flag for displaying overlay */
  int lock;
  int keepcentered;
  int mousemode;
  int lastShape;   /* Last shape for cursor */
  int popup;
  int   toolSection;
  int   toolMaxZ;
  float toolZoom;
  int   toolTime;
  int toolSizeX;
  int toolSizeY;

  short insertmode;
  short showslice;   /* Flag that slice should be drawn during paint */
  int showedSlice;   /* Flag that it was drawn last time */

  int numXpanels;    /* Number of panels in X and Y: # in X is flag for */
  int numYpanels;    /* multi Z window type */
  int panelZstep;    /* Step in Z between panels */
  int drawInCenter;  /* Flags to draw model in center or other panels */
  int drawInOthers;
  int panelXborder;  /* Border outside panels in X and Y */
  int panelYborder;  
  int panelGutter;   /* Gutter between panels */
  int panelXsize;    /* Size of panels in X and Y */
  int panelYsize;

  /* Pointer to view and control sturctures. */
  ImodView    *vi;
  int         ctrl;
  int toolstart;

  /* Special, lock time */
  int    timeLock;
  int    twod;

}ZapStruct;

void zapClosing(ZapStruct *zap);
void zapPaint(ZapStruct *zap);
void zapDraw(ZapStruct *zap);
void zapResize(ZapStruct *zap, int winx, int winy);
void zapKeyInput(ZapStruct *zap, QKeyEvent *e);
void zapKeyRelease(ZapStruct *zap, QKeyEvent *e);
void zapMousePress(ZapStruct *zap, QMouseEvent *e);
void zapMouseRelease(ZapStruct *zap, QMouseEvent *e);
void zapMouseMove(ZapStruct *zap, QMouseEvent *e);
void zapGeneralEvent(ZapStruct *zap, QEvent *e);
void zapHelp(ZapStruct *zap);
void zapEnteredZoom(ZapStruct *zap, float newZoom);
void zapEnteredSection(ZapStruct *zap, int section);
void zapStepZoom(ZapStruct *zap, int step);
void zapStateToggled(ZapStruct *zap, int index, int state);
QString zapPrintInfo(ZapStruct *zap, bool toInfoWindow = true);
void zapStepTime(ZapStruct *zap, int step);
int zapSetupPanels(ZapStruct *zap);
void zapReportBiggestMultiZ();
void zapToggleContourShift(ZapStruct *zap);
ZapStruct *getTopZapWindow(bool withBand, int type = ZAP_WINDOW_TYPE);
int getTopZapMouse(Ipoint *imagePt);
int  imod_zap_open(struct ViewInfo *vi, int wintype);
int zapSubsetLimits(ViewInfo *vi, int &ixStart, int &iyStart, int &nxUse, 
                    int &nyUse);
void zapReportRubberband();
void zapSetImageOrBandCenter(float imx, float imy, bool incremental);
int zapRubberbandCoords(float &rbX0, float &rbX1, float &rbY0, float &rbY1);
void zapPixelViewState(bool state);
void zapSetMouseTracking();

/*
$Log$
Revision 3.32  2009/01/15 16:33:18  mast
Qt 4 port

Revision 3.31  2008/08/01 15:37:55  mast
Moved draw routine to global so imodview can draw top zap

Revision 3.30  2008/02/06 16:33:48  sueh
bug# 1065 In zapPrintInfo made printing the info optional.

Revision 3.29  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 3.28  2008/01/14 19:48:23  mast
Added function to return mouse image coords to plugin

Revision 3.27  2008/01/13 22:58:35  mast
Changes for multi-Z window

Revision 3.26  2007/12/04 18:47:27  mast
Changes for moving functions to utilities module, and for mouse tracking
and cursor-like drawing

Revision 3.25  2007/08/13 16:04:50  mast
Changes for locator window

Revision 3.24  2007/05/31 16:23:10  mast
Changes for using hot toolbar

Revision 3.23  2007/03/29 04:55:50  mast
Fixed crash bug when closing window while focus is in edit/spinbox

Revision 3.22  2006/09/17 18:15:59  mast
Changes to provide mouse position to pixelview

Revision 3.21  2006/08/24 21:28:50  mast
Added flag that rubberband changed

Revision 3.20  2006/07/03 04:14:21  mast
Changes for beadfixer overlay mode

Revision 3.19  2006/04/01 23:43:14  mast
Added size output to toolbar

Revision 3.18  2006/03/01 19:13:06  mast
Moved window size/position routines from xzap to dia_qtutils

Revision 3.17  2005/09/12 14:23:43  mast
Added function to get rubber band coordinates

Revision 3.16  2005/03/08 02:28:46  mast
Added flag to set upon resize so that new subarea gets recorded

Revision 3.15  2005/02/19 01:31:05  mast
Added variables for center of rotation

Revision 3.14  2005/02/09 01:19:48  mast
Added flag for keeping track of changes started when shifted contours

Revision 3.13  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 3.12  2004/11/04 17:02:41  mast
Changes for switching to shifting contour as a mode that is turned on

Revision 3.11  2004/11/01 22:58:32  mast
New rubberband image variables

Revision 3.10  2004/05/05 17:33:46  mast
Added call to get rubberband coordinates

Revision 3.9  2003/09/25 21:10:34  mast
Removed unneeded starting geometry member

Revision 3.8  2003/09/18 00:44:13  mast
Added declaration for function to get subset boundaries

Revision 3.7  2003/09/17 04:43:51  mast
Add declarations for window size functions

Revision 3.6  2003/09/16 02:56:21  mast
Added an xzoom variable to keep track of actually displayed fractional zoom

Revision 3.5  2003/03/12 06:38:18  mast
Added time mismatch function

Revision 3.4  2003/03/03 22:13:38  mast
Added variable for starting the rubber band

Revision 3.3  2003/02/10 20:41:56  mast
Merge Qt source

Revision 3.2.2.8  2003/01/30 06:17:47  mast
Add ability to change range of Z slider on image flip

Revision 3.2.2.7  2003/01/30 00:47:35  mast
Cleanup with new timer logic

Revision 3.2.2.6  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 3.2.2.5  2003/01/04 03:42:20  mast
simplified closing logic

Revision 3.2.2.4  2002/12/14 05:23:42  mast
backing out the fancy subclass, adjusting for new visual detection

Revision 3.2.2.3  2002/12/13 06:06:30  mast
using new glmainwindow and mainglwidget classes

Revision 3.2.2.2  2002/12/10 16:57:34  mast
preventing multiple draws, implementing current contour draw while dragging

Revision 3.2.2.1  2002/12/09 17:50:17  mast
Initial changes to get Qt version

Revision 3.2  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.1  2002/09/13 21:04:57  mast
Added resizeSkipDraw to prevent redraws during resize

*/
#endif
