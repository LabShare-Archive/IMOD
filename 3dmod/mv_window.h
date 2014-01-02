/*   mv_window.h  -  declarations for mv_window.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
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
      VFILE_MENU_MOVIE, VFILE_MENU_SEQUENCE, VFILE_MENU_QUIT, VEDIT_MENU_OBJECTS,
      VEDIT_MENU_CONTROLS, VEDIT_MENU_ROTATION, VEDIT_MENU_OBJLIST, VEDIT_MENU_BKG,
      VEDIT_MENU_MODELS, VEDIT_MENU_VIEWS, VEDIT_MENU_IMAGE,
      VEDIT_MENU_ISOSURFACE, VVIEW_MENU_DB, VVIEW_MENU_BOUNDBOX, 
      VVIEW_MENU_CURPNT, VVIEW_MENU_INVERTZ, VVIEW_MENU_TRANSBKGD,
      VVIEW_MENU_LIGHTING, VVIEW_MENU_WIREFRAME, VVIEW_MENU_LOWRES, 
      VVIEW_MENU_STEREO, VVIEW_MENU_DEPTH, VVIEW_MENU_SCALEBAR, VVIEW_MENU_RESIZE,
      VHELP_MENU_MENUS, VHELP_MENU_KEYBOARD, VHELP_MENU_MOUSE,
      VHELP_MENU_ABOUT, LAST_VMENU_ID};

class ImodvGL;
class QStackedWidget;
class QAction;
class QTimer;
class RotationTool;
class ResizeTool;
typedef struct __imodv_struct ImodvApp;

class ImodvWindow : public QMainWindow
{
  Q_OBJECT

 public:
  ImodvWindow(ImodvApp *a, QWidget * parent = 0, const char * name = 0, 
              Qt::WFlags f = Qt::Window) ;
  ~ImodvWindow();
  void setCheckableItem(int id, bool state);
  void setEnabledMenuItem(int id, bool state);
  int setGLWidget(ImodvApp *a, int db, int stereo, int alpha);
  ImodvGL *addGLWidgetToStack(QGLFormat *glFormat, bool db, int enableDepth,
                              bool stereo, bool alpha);
  void openRotationTool(ImodvApp *a);
  void openResizeTool(ImodvApp *a);

  ImodvGL *mDBw;    // Double buffer widget
  ImodvGL *mDBalw;    // Double buffer alpha widget
  ImodvGL *mSBw;    // Single buffer widget
  ImodvGL *mDBstw;    // Double buffer stereo widget
  ImodvGL *mDBstAlw;    // Double buffer stereo alpha widget
  ImodvGL *mSBstw;    // Single buffer stereo widget
  ImodvGL *mCurGLw; // Current widget
  QTimer  *mTimer;  // Timer for movieing
  RotationTool *mRotationTool;
  ResizeTool *mResizeTool;

public slots:
  void fileMenuSlot(int which);
  void editMenuSlot(int which);
  void viewMenuSlot(int which);
  void keyMenuSlot(int which);
  void helpMenuSlot(int which);
  void timeoutSlot();
  void rotateClicked(int deltaX, int deltaY, int deltaZ);
  void rotStepChanged(int delta);
  void movieButToggled(bool state);
  void rotationClosing();
  void rotationKeyPress(QKeyEvent *e);
  void resizerKeyPress(QKeyEvent *e);
  void rotationKeyRelease(QKeyEvent *e);
  void resizerClosing();
  void newResizerSize(int sizeX, int sizeY);

 protected:
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void closeEvent ( QCloseEvent * e );
  bool event(QEvent *e);

 private:
  QStackedWidget *mStack;    // The stack holding the two widgets
  QAction *mActions[LAST_VMENU_ID];
  bool mMinimized;
  int mNumKeyEntries;
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

