/*
 *  xzap.h -- Header file for ZaP Window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */
/*  $Author$

$Date$

$Revision$
Log at end of file
*/

#ifndef XZAP_H
#define XZAP_H

class ZapWindow;
class ZapGL;
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
  Ipoint xformCenter;  /* Center defined by mouse */
  int    centerDefined;  /* Flag that center was defined with mouse */
  int    centerMarked;   /* Flag that center was displayed in extra object */
  int    shiftRegistered;  /* Flag that contour changes have been registered */
  int    dragAddCount; /* Number of points added and not registered for undo */
  Iindex dragAddIndex; /* Starting obj, cont, point for first such point*/
  int    dragAddEnd;   /* Ending or last point not registered */

  int movieSnapCount; /* Counter if this window is doing movie snapshots */
  int recordSubarea;  /* Record the subarea on the next draw */

  float  zoom;
  float  xzoom;    /* Possibly slightly different X zoom */
  char   *data;

  /* The graphic image buffer. */
  B3dCIImage *image;

  int section;
  int sectionStep; /* auto step image after new model point. */
  int time;
  int lock;
  int keepcentered;
  int mousemode;
  int popup;
  int   toolSection;
  int   toolMaxZ;
  float toolZoom;
  int   toolTime;

  short insertmode;
  short showslice;

  /* Pointer to view and control sturctures. */
  ImodView    *vi;
  int         ctrl;
  int toolstart;

  /* Special, lock time */
  int    timeLock;
  int    twod;

}ZapStruct;

void zapClosing(struct zapwin *zap);
void zapPaint(struct zapwin *zap);
void zapResize(struct zapwin *zap, int winx, int winy);
void zapKeyInput(struct zapwin *zap, QKeyEvent *e);
void zapKeyRelease(struct zapwin *zap, QKeyEvent *e);
void zapMousePress(struct zapwin *zap, QMouseEvent *e);
void zapMouseRelease(struct zapwin *zap, QMouseEvent *e);
void zapMouseMove(struct zapwin *zap, QMouseEvent *e, bool mousePressed);
void zapHelp(void);
void zapEnteredZoom(struct zapwin *zap, float newZoom);
void zapEnteredSection(struct zapwin *zap, int section);
void zapStepZoom(struct zapwin *zap, int step);
void zapStateToggled(struct zapwin *zap, int index, int state);
void zapPrintInfo(struct zapwin *zap);
void zapStepTime(struct zapwin *zap, int step);
void zapDrawSymbol(int mx, int my, unsigned char sym, unsigned char size,
                   unsigned char flags);
void zapCurrentPointSize(Iobj *obj, int *modPtSize, int *backupSize,
                         int *imPtSize);
int  imod_zap_open(struct ViewInfo *vi);
void zapMaximumWindowSize(int &width, int &height);
void zapLimitWindowSize(int &width, int &height);
void zapLimitWindowPos(int neww, int newh, int &newdx, int &newdy);
int zapSubsetLimits(ViewInfo *vi, int &ixStart, int &iyStart, int &nxUse, 
                    int &nyUse);
void zapReportRubberband();
int zapRubberbandCoords(float &rbX0, float &rbX1, float &rbY0, float &rbY1);

/*
$Log$
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
