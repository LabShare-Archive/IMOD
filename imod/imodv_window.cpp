/*  
 *  imodv_window.cpp -- Mainwindow and GLwidget classes for imodv window
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <stdio.h>
#include <qmenubar.h>
#include <qpopupmenu.h>
#include <qwidgetstack.h>
#include <qtimer.h>
#include "imod.h"
#include "imodv_window.h"
#include "imodv.h"
#include "imodv_menu.h"
#include "imodv_gfx.h"
#include "imodv_input.h"
#include "control.h"

extern int Imod_debug;

ImodvWindow::ImodvWindow(bool standAlone, int enableDepthDB, 
                         int enableDepthSB, int stereoDB, int stereoSB, 
                         bool lighting, bool lowRes,
                         QWidget * parent, const char * name, WFlags f)
  : QMainWindow(parent, name, f)
{
  mDBw = mSBw = NULL;
  mMinimized = false;

  // construct file menu
  mFileMenu = new QPopupMenu;
  mFileMenu->insertItem("&Open Model", VFILE_MENU_LOAD);
  mFileMenu->setAccel(CTRL + Key_O, VFILE_MENU_LOAD);
  mFileMenu->setItemEnabled(VFILE_MENU_LOAD, standAlone);

  mFileMenu->insertItem("&Save Model", VFILE_MENU_SAVE);
  mFileMenu->setItemEnabled(VFILE_MENU_SAVE, standAlone);

  mFileMenu->insertItem("Save Model &As...", VFILE_MENU_SAVEAS);
  mFileMenu->setItemEnabled(VFILE_MENU_SAVEAS, standAlone);
  mFileMenu->insertSeparator();

  mFileMenu->insertItem("Snap &Tiff As...", VFILE_MENU_SNAPTIFF);
  mFileMenu->insertItem("Snap &NonT As...", VFILE_MENU_SNAPRGB);
  mFileMenu->insertItem("&Zero Snap File #", VFILE_MENU_ZEROSNAP);
  mFileMenu->insertItem("S&et Snap Dir...", VFILE_MENU_SNAPDIR);
  mFileMenu->insertItem("&Movie...", VFILE_MENU_MOVIE);
  mFileMenu->setAccel(Key_M, VFILE_MENU_MOVIE);

  mFileMenu->insertItem(standAlone ? "&Quit" : "&Close", VFILE_MENU_QUIT);
  mFileMenu->setAccel(CTRL + Key_Q, VFILE_MENU_QUIT);

  connect(mFileMenu, SIGNAL(activated(int)), this, SLOT(fileMenuSlot(int)));

  // Construct Edit menu
  mEditMenu  = new QPopupMenu;
  mEditMenu->insertItem("&Objects...", VEDIT_MENU_OBJECTS);
  mEditMenu->setAccel(SHIFT + Key_O, VEDIT_MENU_OBJECTS);
  mEditMenu->insertItem("&Controls...", VEDIT_MENU_CONTROLS);
  mEditMenu->setAccel(SHIFT + Key_C, VEDIT_MENU_CONTROLS);
  mEditMenu->insertItem("Object &List...", VEDIT_MENU_OBJLIST);
  mEditMenu->setAccel(SHIFT + Key_L, VEDIT_MENU_OBJLIST);
  mEditMenu->insertItem("&Background...", VEDIT_MENU_BKG);
  mEditMenu->setAccel(SHIFT + Key_B, VEDIT_MENU_BKG);
  mEditMenu->insertItem("&Models...", VEDIT_MENU_MODELS);
  mEditMenu->setAccel(SHIFT + Key_M, VEDIT_MENU_MODELS);
  mEditMenu->insertItem("&Views...", VEDIT_MENU_VIEWS);
  mEditMenu->setAccel(SHIFT + Key_V, VEDIT_MENU_VIEWS);
  mEditMenu->insertItem("&Image...", VEDIT_MENU_IMAGE);
  mEditMenu->setAccel(SHIFT + Key_I, VEDIT_MENU_IMAGE);
  mEditMenu->insertItem("Isos&urface...", VEDIT_MENU_ISOSURFACE);
  mEditMenu->setAccel(SHIFT + Key_U, VEDIT_MENU_ISOSURFACE);
  mEditMenu->setItemEnabled(VEDIT_MENU_IMAGE, imodvByteImagesExist() != 0);
  mEditMenu->setItemEnabled(VEDIT_MENU_ISOSURFACE, imodvByteImagesExist() != 0);
  connect(mEditMenu, SIGNAL(activated(int)), this, SLOT(editMenuSlot(int)));

  // View menu
  mViewMenu  = new QPopupMenu;
  mViewMenu->setCheckable(true);
  mViewMenu->insertItem("Low &Res", VVIEW_MENU_LOWRES);
  mViewMenu->setAccel(SHIFT + Key_R, VVIEW_MENU_LOWRES);
  mViewMenu->setItemChecked(VVIEW_MENU_LOWRES, lowRes);
  if (!standAlone) {
    mViewMenu->insertItem("&Current Point", VVIEW_MENU_CURPNT);
    mViewMenu->setAccel(Key_P, VVIEW_MENU_CURPNT);
    mViewMenu->setItemChecked(VVIEW_MENU_CURPNT, false);
  }
  mViewMenu->insertItem("Bounding Bo&x", VVIEW_MENU_BOUNDBOX);
  mViewMenu->setItemChecked(VVIEW_MENU_BOUNDBOX, false);

  mViewMenu->insertItem("&Stereo...", VVIEW_MENU_STEREO);
  mViewMenu->insertItem("&Depth Cue...", VVIEW_MENU_DEPTH);
  mViewMenu->insertItem("Scale &Bar...", VVIEW_MENU_SCALEBAR);

  mViewMenu->insertItem("Do&uble Buffer", VVIEW_MENU_DB);
  mViewMenu->setAccel(Key_D, VVIEW_MENU_DB);
  mViewMenu->setItemChecked(VVIEW_MENU_DB, enableDepthDB >= 0);
  mViewMenu->setItemEnabled(VVIEW_MENU_DB, 
                            enableDepthDB >= 0 && enableDepthSB >= 0);

  mViewMenu->insertItem("&Lighting", VVIEW_MENU_LIGHTING);
  mViewMenu->setItemChecked(VVIEW_MENU_LIGHTING, lighting);

  mViewMenu->insertItem("&Wireframe", VVIEW_MENU_WIREFRAME);
  mViewMenu->setItemChecked(VVIEW_MENU_WIREFRAME, false);
  connect(mViewMenu, SIGNAL(activated(int)), this, SLOT(viewMenuSlot(int)));



  // Help menu
  QPopupMenu *helpMenu  = new QPopupMenu;
  helpMenu->insertItem("&Menus", VHELP_MENU_MENUS);
  helpMenu->insertItem("&Keyboard", VHELP_MENU_KEYBOARD);
  helpMenu->insertItem("M&ouse", VHELP_MENU_MOUSE);
  helpMenu->insertSeparator();
  helpMenu->insertItem("&About", VHELP_MENU_ABOUT);
  connect(helpMenu, SIGNAL(activated(int)), this, SLOT(helpMenuSlot(int)));
  
  // Create and fill menu bar
  QMenuBar *menuBar = new QMenuBar(this);
  menuBar->insertItem("&File", mFileMenu);
  menuBar->insertItem("&Edit", mEditMenu);
  menuBar->insertItem("&View", mViewMenu);
  menuBar->insertSeparator();
  menuBar->insertItem("&Help", helpMenu);

  // Get the widget stack and the GL widgets
  mStack = new QWidgetStack(this);
  setCentralWidget(mStack);
  QGLFormat glFormat;
  glFormat.setRgba(true);

  if (enableDepthDB >= 0) { 
    glFormat.setDoubleBuffer(true);
    glFormat.setDepth(enableDepthDB > 0);
    glFormat.setStereo(stereoDB > 0);
    mDBw = new ImodvGL(glFormat, mStack);
    mStack->addWidget(mDBw);
    mCurGLw = mDBw;
  }

  if (enableDepthSB >= 0) { 
    glFormat.setDoubleBuffer(false);
    glFormat.setDepth(enableDepthSB > 0);
    glFormat.setStereo(stereoSB > 0);
    mSBw = new ImodvGL(glFormat, mStack);
    mStack->addWidget(mSBw);
    if (enableDepthDB < 0)
      mCurGLw = mSBw;
  }

  // Set the topmost widget of the stack
  mStack->raiseWidget(mCurGLw);
  setUpLayout();

  mTimer = new QTimer(this, "imodv timer");
  connect(mTimer, SIGNAL(timeout()), this, SLOT(timeoutSlot()));
}

ImodvWindow::~ImodvWindow()
{
}

// The slots for passing on the menu actions
void ImodvWindow::fileMenuSlot(int which)
{
  if (Imod_debug)
    imodPuts("file menu");
  imodvFileMenu(which);
}

void ImodvWindow::editMenuSlot(int which)
{
  if (Imod_debug)
    imodPuts("edit menu");
  imodvEditMenu(which);
}

// View menu: it is responsibility of receiver to change check state
void ImodvWindow::viewMenuSlot(int which)
{
  imodvViewMenu(which);
}

void ImodvWindow::helpMenuSlot(int which)
{
   imodvHelpMenu(which);
}

void ImodvWindow::setCheckableItem(int id, bool state)
{
  mViewMenu->setItemChecked(id, state);
}

// Bring the selected widget to the top if both exist
int ImodvWindow::setGLWidget(int db)
{
  if (!mDBw || !mSBw)
    return 1;
  if (db)
    mCurGLw = mDBw;
  else
    mCurGLw = mSBw;
  mStack->raiseWidget(mCurGLw);
  return 0;
}

void ImodvWindow::keyPressEvent ( QKeyEvent * e )
{
  imodvKeyPress(e);
}

void ImodvWindow::keyReleaseEvent ( QKeyEvent * e )
{
  imodvKeyRelease(e);
}

void ImodvWindow::closeEvent ( QCloseEvent * e )
{
  imodvQuit();
  e->accept();
}

// Manage the hiding and showing of other windows when this
// window is minimized or brought back
bool ImodvWindow::event(QEvent *e)
{
  if (Imod_debug && e->type() == QEvent::ShowMinimized)
    imodPrintStderr("ShowMinimized\n");
  if (Imod_debug && e->type() == QEvent::Hide)
    imodPrintStderr("Hide\n");
  if (Imod_debug && e->type() == QEvent::ShowNormal)
    imodPrintStderr("ShowNormal\n");
  if (Imod_debug && e->type() == QEvent::Show)
    imodPrintStderr("Show\n");
  if ((e->type() == QEvent::ShowMinimized || e->type() == QEvent::Hide) &&
      !mMinimized) {
    if (Imod_debug)
      imodPuts("minimizing");
    mMinimized = true;
    imodvDialogManager.hide();
  } else if ((e->type() == QEvent::ShowNormal || e->type() == QEvent::Show)
             && mMinimized) {
    if (Imod_debug)
      imodPuts("maximizing");
    mMinimized = false;
    imodvDialogManager.show();
  }
  return QWidget::event(e);
}

void ImodvWindow::timeoutSlot()
{
  imodvMovieTimeout();
}

ImodvGL::ImodvGL(QGLFormat inFormat, QWidget * parent, const char * name)
  : QGLWidget(inFormat, parent, name)
{
  mMousePressed = false;
}

ImodvGL::~ImodvGL()
{
}
 
void ImodvGL::initializeGL()
{
  imodvInitializeGL();
}

void ImodvGL::paintGL()
{
  imodvPaintGL();
}

// Resize event can be for the widget not being displayed, so we need to
// pass on the pointer
void ImodvGL::resizeGL( int wdth, int hght )
{
  imodvResizeGL(this, wdth, hght);
}

void ImodvGL::mousePressEvent(QMouseEvent * e )
{
  mMousePressed = true;
  imodvMousePress(e);
}

void ImodvGL::mouseReleaseEvent ( QMouseEvent * e )
{
  mMousePressed = false;
  imodvMouseRelease(e);
}

void ImodvGL::mouseMoveEvent ( QMouseEvent * e )
{
  if (mMousePressed)
    imodvMouseMove(e);
}

void ImodvGL::wheelEvent ( QWheelEvent * e)
{
  double power = -e->delta() / 120.;
  double zoom = pow(1.05, power);
  imodv_zoomd(Imodv, zoom);
  imodvDraw(Imodv);
}

/*

$Log$
Revision 4.18  2008/10/02 22:43:03  mast
Add stereo arguments to constructor for requesting stereo visual

Revision 4.17  2008/05/27 05:47:45  mast
Capitalize Isosurface

Revision 4.16  2008/04/29 18:13:31  xiongq
add isosurface dialog

Revision 4.15  2008/02/03 18:38:00  mast
Added scroll wheel zoom

Revision 4.14  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 4.13  2007/11/10 04:07:10  mast
Changes for setting snapshot directory

Revision 4.12  2006/10/11 23:53:05  mast
Changed RGB to NonT in menu

Revision 4.11  2006/08/31 23:23:14  mast
Removed hot key for Save As

Revision 4.10  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.9  2003/11/26 18:15:29  mast
Disable image menu entry unless byte images exist

Revision 4.8  2003/11/12 18:52:20  mast
Add include of imodv since quit moved there

Revision 4.7  2003/08/01 00:13:30  mast
Make event reports happen in debug mode

Revision 4.6  2003/05/05 15:08:50  mast
Add accelerator keys, some of which don't show up

Revision 4.5  2003/04/23 17:50:44  mast
no longer need to watch windowActivate event due to fix on Mac

Revision 4.4  2003/04/16 18:46:51  mast
hide/show changes

Revision 4.3  2003/04/11 22:30:29  mast
return value from new event watcher

Revision 4.2  2003/04/11 18:56:34  mast
switch to watching event types to manage hide/show events

Revision 4.1  2003/02/10 20:29:02  mast
autox.cpp

Revision 1.1.2.6  2003/01/13 07:21:38  mast
Changes to use new dialog manager class

Revision 1.1.2.5  2003/01/13 01:15:43  mast
changes for Qt version of info window

Revision 1.1.2.4  2003/01/01 05:40:21  mast
add timer to workaround iconifying problem

Revision 1.1.2.3  2002/12/30 06:42:47  mast
On show and hide events, make calls to show or hide dialogs

Revision 1.1.2.2  2002/12/17 22:04:00  mast
cleanup

Revision 1.1.2.1  2002/12/17 18:46:08  mast
Initial creation

*/
