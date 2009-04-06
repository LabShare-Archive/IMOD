/* 
 *  xxyz.h -- Include file for xyz.c, the XYZ Window.
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

#ifndef XXYZ_H
#define XXYZ_H

#include <qmainwindow.h>
#include <qgl.h>
//Added by qt3to4:
#include <QCloseEvent>
#include <QTimerEvent>
#include <QLabel>
#include <QMouseEvent>
#include <QKeyEvent>
#include "b3dgfx.h"
#include "imodel.h"

/* Forward declarations to minimize includes */
struct ViewInfo;
struct Mod_object;
struct Mod_contour;
class XyzWindow;
class XyzGL;
class HotToolBar;
class QToolBar;
class QLabel;
class QToolButton;
class QSignalMapper;
class ToolEdit;
class QSlider;
class MultiSlider;
class QPushButton;

#define MAX_XYZ_TOGGLES  1
#define NUM_AXIS 3

struct xxyzwin
{
  struct ViewInfo *vi;   /* Image Data information.              */
  XyzWindow *dialog;         /* The top widget of the xyz window     */
  XyzGL *glw;            /* The drawing widget of the xyz window */
  int ctrl;              /* id of control */
     
  unsigned char *fdataxz; /* tmp data storage for xz image       */
  unsigned char *fdatayz; /* tmp data storage for yz image       */
  B3dCIImage *xydata;    /* Draw buffer for Z slices.            */
  B3dCIImage *xzdata;    /* Draw buffer for Y slices.            */
  B3dCIImage *yzdata;    /* Draw buffer for X slices.            */

  int winx, winy;         /* Size of xyz window.                  */
  int exposed;
  float zoom;

  int lx, ly, lz;
  int lastCacheSum;       /* Sum of cache Z values on last draw */

  int xtrans1, ytrans1;   /* translation (pan) in image coords */
  int xtrans2, ytrans2; 
  int xwoffset1, xwoffset2;  /* offset in window coordinates */
  int ywoffset1, ywoffset2;
  int lmx, lmy;           /* last mouse position for panning */
  int hq;                 /* High resolution flag */
  int whichbox;           /* box that left mouse button went down in */
  int project;            /* Flag to project current contour into planes */
  int mousemode;          /* Current mode for cursor */
  float toolZoom;
  int drawCurrentOnly;
  int toolMaxX;
  int toolMaxY;
  int toolMaxZ;
  int winXdim1,winXdim2,winYdim1,winYdim2;
  int xorigin1, xorigin2, yorigin1, yorigin2;
  float xzFraction, yzFraction;
  float scaleBarSize;     /* Actual size of draw scale bar in model units */
};


class XyzWindow : public QMainWindow
{
  Q_OBJECT

    public:
  XyzWindow(struct xxyzwin *xyz, bool rgba, bool doubleBuffer, 
	    bool enableDepth, QWidget * parent = 0, const char * name = 0,
	    Qt::WFlags f = Qt::Window) ;
  ~XyzWindow() {};

  XyzGL *mGLw;
  //HotToolBar *mToolBar;
  QToolBar *mToolBar;
  void Draw();
  int Getxyz(int x, int y, float *mx, float *my, int *mz);
  void B1Press(int x, int y);
  void B2Press(int x, int y);
  void B3Press(int x, int y);
  void B1Drag(int x, int y);
  void B2Drag(int x, int y);
  void B3Drag(int x, int y);
  void DrawAuto();
  void DrawModel();
  void DrawImage();
  void GetCIImages();
  void DrawGhost();
  void DrawContour(struct Mod_Object *obj, int ob, int co);
  void DrawCurrentPoint();
  void DrawCurrentLines();
  void DrawScatSymAllSpheres(Iobj *obj, int ob,  Icont *cont, int co, 
                             DrawProps *contProps,
                             DrawProps *ptProps, int *stateFlags,
                             int handleFlags, int nextChange, 
                             int indx, int indy, int indz,
                             float zmouse, float bx, float by,float zscale);
  void DrawSymProj(Iobj *obj,  Icont *cont, int co, DrawProps *contProps, 
                   DrawProps *ptProps, int *stateFlags, 
                   int handleFlags, int nextChange, int indx, 
                   int indy, int indz, int currentZ, float bx, float by,
                   bool currentCont);
  void keyPressPassedOn ( QKeyEvent * e ) {keyPressEvent(e);};
  void SetCursor(int mode, bool setAnyway = false);
  void setZoomText(float zoom);
  void setSlider(int which, int section);
  void setMaxAxis(int which, int max);
    
  public slots:
    void toggleClicked(int index);
    void toolKeyPress(QKeyEvent *e) {keyPressEvent(e);};
    void toolKeyRelease(QKeyEvent *e) {keyReleaseEvent(e);};
    void zoomUp();
    void zoomDown();
    void newZoom();
    void centerClicked();
    void sliderChanged(int which, int value, bool dragging);
    void help();

 protected:
  void keyPressEvent ( QKeyEvent * e );
  void closeEvent ( QCloseEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );

 private:
  struct xxyzwin *mXyz;
  QToolButton *mToggleButs[MAX_XYZ_TOGGLES];
  int mToggleStates[MAX_XYZ_TOGGLES];
  ToolEdit *mZoomEdit;
  bool mCtrlPressed;
  int mDisplayedAxisLocation[NUM_AXIS];
  MultiSlider *mSliders;
  QPushButton *mHelpButton;
  void StateToggled(int index, int state);
  void stateToggled(int index, int state);
  void enteredZoom(float newZoom);
  void setControlAndLimits();
  void stepZoom(int step);
  void keyRelease(QKeyEvent *event);
  void enteredAxisLocation(int which, int value);
  void allocateDim(int winsize, float zFraction, int &dim1, int &dim2);
  void setFontDependentWidths();
};

class XyzGL : public QGLWidget
{
  Q_OBJECT

    public:
  XyzGL(  struct xxyzwin *xyz, QGLFormat format, XyzWindow * parent = 0);
  ~XyzGL() {};
  bool mClosing;
 
 protected:
  void paintGL();
  void resizeGL( int wdth, int hght );
  void mousePressEvent(QMouseEvent * e );
  void mouseReleaseEvent ( QMouseEvent * e );
  void mouseMoveEvent ( QMouseEvent * e );
  void timerEvent(QTimerEvent *e);

 private:
  struct xxyzwin *mXyz;
  XyzWindow *mWin;
  bool mFirstDraw;
  int mTimerID;
  bool mMousePressed;
  void drawTools();
};

/* Global functions */
int xxyz_open(struct ViewInfo *vi);
void xyzPixelViewState(bool state);
float xyzScaleBarSize();

/*

$Log$
Revision 3.19  2009/01/15 16:33:18  mast
Qt 4 port

Revision 3.18  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 3.17  2007/11/16 23:13:04  mast
New variables for adjusting balance between windows, center button

Revision 3.16  2007/07/13 17:28:50  sueh
bug# 1023 Added mFirstDraw and mTimerID to fix a first draw problem.

Revision 3.15  2007/07/13 14:50:03  mast
cleanup

Revision 3.14  2007/07/13 05:36:05  mast
Added help and functions for generic model drawing

Revision 3.13  2007/07/11 22:45:26  sueh
bug# 1023 Sizing boxes based on window size.  Changed gadgets to slide in visible area only.  Panning the three view independently.

Revision 3.12  2007/06/30 00:42:43  sueh
bug# 1021 Updating the slider ranges and sizes on draw, in case a flip is done.  Labeled the toolbar and limited it docking options.

Revision 3.11  2007/06/29 21:09:24  sueh
bug# 1021 Replacing the Z slider with a multi-slider that shows X, Y, and
Z sliders.

Revision 3.10  2007/06/27 21:53:21  sueh
bug# 1021 Added slider.

Revision 3.9  2007/06/26 21:57:10  sueh
bug# 1021 Removed win_support.  Added functions for zooming and the
zoom edit box.  Removed unnecessary variable from XyzStruct -
recordSubarea.

Revision 3.8  2007/06/26 17:07:11  sueh
bug# 1021 Added a button toolbar with a high-resolution button and zoom
arrows.

Revision 3.7  2006/10/11 20:13:32  mast
Added closing flag

Revision 3.6  2006/09/17 18:15:59  mast
Changes to provide mouse position to pixelview

Revision 3.5  2004/11/01 22:57:24  mast
Changed to floating x and y positions in getxyz

Revision 3.4  2003/03/03 22:18:10  mast
Added variables for keeping tracking of cursor and side-view projection

Revision 3.3  2003/02/10 20:41:56  mast
Merge Qt source

Revision 3.2.2.7  2003/01/03 16:46:30  mast
Simplified closing logic

Revision 3.2.2.6  2003/01/02 15:42:49  mast
add  variable to keep track of sections loaded in cache

Revision 3.2.2.5  2002/12/14 05:23:42  mast
backing out the fancy subclass, adjusting for new visual detection

Revision 3.2.2.4  2002/12/13 07:09:19  mast
GLMainWindow needed different name for mouse event processors

Revision 3.2.2.3  2002/12/13 06:06:29  mast
using new glmainwindow and mainglwidget classes

Revision 3.2.2.2  2002/12/12 02:45:56  mast
*** empty log message ***

Revision 3.2.2.1  2002/12/12 01:22:29  mast
Changes to become Qt window

Revision 3.2  2002/11/25 19:22:16  mast
Added a structure element for control id

Revision 3.1  2002/01/28 16:54:55  mast
Added structure elements for new enhancements

*/

#endif /* xxyz.h */

