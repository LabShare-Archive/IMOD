/*  IMOD VERSION 2.02
 *
 *  xxyz.h -- Include file for xyz.c, the XYZ Window.
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
/*  $Author$

$Date$

$Revision$
Log at end of file
*/

#ifndef XXYZ_H
#define XXYZ_H

#include <qmainwindow.h>
#include <qgl.h>
#include "b3dgfx.h"

/* Forward declarations to minimize includes */
struct ViewInfo;
struct Mod_object;
struct Mod_contour;
class XyzWindow;
class XyzGL;

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

  int xtrans, ytrans;     /* translation (pan) in image coords */
  int xwoffset,ywoffset;  /* offset in window coordinates */
  int lmx, lmy;           /* last mouse position for panning */
  int hq;                 /* High resolution flag */
  int whichbox;           /* box that left mouse button went down in */
  int project;            /* Flag to project current contour into planes */
  int mousemode;          /* Current mode for cursor */
};


class XyzWindow : public QMainWindow
{
  Q_OBJECT

    public:
  XyzWindow(struct xxyzwin *xyz, bool rgba, bool doubleBuffer, 
	    bool enableDepth, QWidget * parent = 0, const char * name = 0,
	    WFlags f = Qt::WDestructiveClose || Qt::WType_TopLevel) ;
  ~XyzWindow() {};

  XyzGL *mGLw;
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
  void SetSubimage(int absStart, int winSize, int imSize, float zoom,
		   int *drawsize, int *woffset, int *dataStart);
  void DrawGhost();
  void DrawContour(struct Mod_Object *obj, int ob, int co);
  void DrawCurrentPoint();
  void DrawCurrentLines();
  void keyPressPassedOn ( QKeyEvent * e ) {keyPressEvent(e);};
  void SetCursor(int mode);

 protected:
  void keyPressEvent ( QKeyEvent * e );
  void closeEvent ( QCloseEvent * e );


 private:

  struct xxyzwin *mXyz;
};

class XyzGL : public QGLWidget
{
  Q_OBJECT

    public:
  XyzGL(  struct xxyzwin *xyz, QGLFormat format, XyzWindow * parent = 0,
        const char * name = 0);
  ~XyzGL() {};
 
 protected:
  void paintGL();
  void resizeGL( int wdth, int hght );
  void mousePressEvent(QMouseEvent * e );
  void mouseReleaseEvent ( QMouseEvent * e );
  void mouseMoveEvent ( QMouseEvent * e );

 private:
  struct xxyzwin *mXyz;
  XyzWindow *mWin;
  bool mMousePressed;
};

/* Global functions */
int xxyz_open(struct ViewInfo *vi);

/*
$Log$
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

