/*   imodv_window.h  -  declarations for imodv_window.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
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

#ifndef IMODV_WINDOW_H
#define IMODV_WINDOW_H

#include <qmainwindow.h>
#include <qgl.h>

enum {VFILE_MENU_LOAD, VFILE_MENU_SAVE, VFILE_MENU_SAVEAS, VFILE_MENU_SNAPRGB,
      VFILE_MENU_SNAPTIFF, VFILE_MENU_ZEROSNAP, VFILE_MENU_MOVIE, 
      VFILE_MENU_QUIT, 
      VEDIT_MENU_OBJECTS, VEDIT_MENU_CONTROLS, VEDIT_MENU_OBJLIST,
      VEDIT_MENU_BKG, VEDIT_MENU_MODELS, VEDIT_MENU_VIEWS, VEDIT_MENU_IMAGE, 
      VVIEW_MENU_DB, VVIEW_MENU_LIGHTING, VVIEW_MENU_WIREFRAME, 
      VVIEW_MENU_LOWRES, VVIEW_MENU_STEREO, VVIEW_MENU_DEPTH,
      VHELP_MENU_MENUS, VHELP_MENU_KEYBOARD, VHELP_MENU_MOUSE,
      VHELP_MENU_ABOUT};

class ImodvGL;
class QWidgetStack;
class QPopupMenu;
class QTimer;

class ImodvWindow : public QMainWindow
{
  Q_OBJECT

 public:
  ImodvWindow(bool standAlone, int enableDepthDB, int enableDepthSB, 
              bool lighting, bool lowRes, QWidget * parent = 0,
              const char * name = 0, 
              WFlags f = WType_TopLevel | WDestructiveClose) ;
  ~ImodvWindow();
  void setCheckableItem(int id, bool state);
  int setGLWidget(int db);

  ImodvGL *mDBw;    // Double buffer widget
  ImodvGL *mSBw;    // Single buffer widget
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
  QWidgetStack *mStack;    // The stack holding the two widgets
  QPopupMenu *mFileMenu;
  QPopupMenu *mEditMenu;
  QPopupMenu *mViewMenu;
  bool mMinimized;
};

class ImodvGL : public QGLWidget
{
  Q_OBJECT

 public:
  ImodvGL(QGLFormat format, QWidget * parent = 0, const char * name = 0);
  ~ImodvGL();
  void setBufferSwapAuto(bool state) { setAutoBufferSwap(state); };
 
protected:
  void initializeGL();
  void paintGL();
  void resizeGL( int wdth, int hght );
  void mousePressEvent(QMouseEvent * e );
  void mouseReleaseEvent ( QMouseEvent * e );
  void mouseMoveEvent ( QMouseEvent * e );

 private:
  bool mMousePressed;
};

#endif
