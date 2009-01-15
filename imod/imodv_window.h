/*   imodv_window.h  -  declarations for imodv_window.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#ifndef IMODV_WINDOW_H
#define IMODV_WINDOW_H

#include <qmainwindow.h>
#include <qgl.h>
//Added by qt3to4:
#include <QWheelEvent>
#include <QEvent>
#include <QMouseEvent>
#include <QKeyEvent>
#include <QCloseEvent>

enum {VFILE_MENU_LOAD, VFILE_MENU_SAVE, VFILE_MENU_SAVEAS, VFILE_MENU_SNAPRGB,
      VFILE_MENU_SNAPTIFF, VFILE_MENU_ZEROSNAP, VFILE_MENU_SNAPDIR, 
      VFILE_MENU_MOVIE, VFILE_MENU_QUIT, VEDIT_MENU_OBJECTS,
      VEDIT_MENU_CONTROLS, VEDIT_MENU_OBJLIST, VEDIT_MENU_BKG,
      VEDIT_MENU_MODELS, VEDIT_MENU_VIEWS, VEDIT_MENU_IMAGE,
      VEDIT_MENU_ISOSURFACE, VVIEW_MENU_DB, VVIEW_MENU_BOUNDBOX, 
      VVIEW_MENU_CURPNT,
      VVIEW_MENU_LIGHTING, VVIEW_MENU_WIREFRAME, VVIEW_MENU_LOWRES, 
      VVIEW_MENU_STEREO, VVIEW_MENU_DEPTH, VVIEW_MENU_SCALEBAR,
      VHELP_MENU_MENUS, VHELP_MENU_KEYBOARD, VHELP_MENU_MOUSE,
      VHELP_MENU_ABOUT, LAST_VMENU_ID};

class ImodvGL;
class QStackedWidget;
class QAction;
class QTimer;
typedef struct __imodv_struct ImodvApp;

class ImodvWindow : public QMainWindow
{
  Q_OBJECT

 public:
  ImodvWindow(ImodvApp *a, QWidget * parent = 0, const char * name = 0, 
              Qt::WFlags f = Qt::Window) ;
  ~ImodvWindow();
  void setCheckableItem(int id, bool state);
  int setGLWidget(ImodvApp *a, int db, int stereo);
  ImodvGL *addGLWidgetToStack(QGLFormat *glFormat, bool db, int enableDepth,
                              bool stereo);

  ImodvGL *mDBw;    // Double buffer widget
  ImodvGL *mSBw;    // Single buffer widget
  ImodvGL *mDBstw;    // Double buffer stereo widget
  ImodvGL *mSBstw;    // Single buffer stereo widget
  ImodvGL *mCurGLw; // Current widget
  QTimer  *mTimer;  // Timer for movieing

public slots:
  void fileMenuSlot(int which);
  void editMenuSlot(int which);
  void viewMenuSlot(int which);
  void helpMenuSlot(int which);
  void timeoutSlot();

 protected:
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void closeEvent ( QCloseEvent * e );
  bool event(QEvent *e);

 private:
  QStackedWidget *mStack;    // The stack holding the two widgets
  QAction *mActions[LAST_VMENU_ID];
  bool mMinimized;
};

class ImodvGL : public QGLWidget
{
  Q_OBJECT

 public:
  ImodvGL(QGLFormat format, QWidget * parent = 0);
  ~ImodvGL();
  void setBufferSwapAuto(bool state) { setAutoBufferSwap(state); };
 
protected:
  void initializeGL();
  void paintGL();
  void resizeGL( int wdth, int hght );
  void mousePressEvent(QMouseEvent * e );
  void mouseReleaseEvent ( QMouseEvent * e );
  void mouseMoveEvent ( QMouseEvent * e );
  void wheelEvent ( QWheelEvent * e);

 private:
  bool mMousePressed;
};

#endif

/*

$Log$
Revision 4.11  2008/12/17 17:50:40  mast
Add function to add widget to stack

Revision 4.10  2008/12/15 21:26:28  mast
Changes for stereo/non-stereo widgets

Revision 4.9  2008/11/28 06:48:32  mast
Add more menu items

Revision 4.8  2008/10/02 22:43:04  mast
Add stereo arguments to constructor for requesting stereo visual

Revision 4.7  2008/04/29 18:13:44  xiongq
add isosurface dialog

Revision 4.6  2008/02/03 18:38:00  mast
Added scroll wheel zoom

Revision 4.5  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 4.4  2007/11/10 15:04:51  mast
Fix screwed up comment

Revision 4.3  2007/11/10 04:07:10  mast
Changes for setting snapshot directory

Revision 4.2  2003/04/11 18:56:34  mast
switch to watching event types to manage hide/show events

Revision 4.1  2003/02/10 20:41:56  mast
Merge Qt source

Revision 1.1.2.4  2003/01/01 05:40:21  mast
add timer to workaround iconifying problem

Revision 1.1.2.3  2002/12/30 06:42:09  mast
catch show and hide events

Revision 1.1.2.2  2002/12/23 04:57:47  mast
Add public function to disable autobufferswap

Revision 1.1.2.1  2002/12/17 17:40:37  mast
initial creation

*/

