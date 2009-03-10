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
#include <qmenu.h>
#include <qsignalmapper.h>
#include <qaction.h>
#include <QStackedWidget>
#include <qtimer.h>
//Added by qt3to4:
#include <QCloseEvent>
#include <QWheelEvent>
#include <QMouseEvent>
#include <QKeyEvent>
#include <QEvent>
#include "imod.h"
#include "imodv_window.h"
#include "imodv.h"
#include "imodv_menu.h"
#include "imodv_gfx.h"
#include "imodv_input.h"
#include "control.h"

#define ADD_ACTION(a, b, c) mActions[c] = a##Menu->addAction(b); \
connect(mActions[c], SIGNAL(triggered()), a##Mapper, SLOT(map())); \
a##Mapper->setMapping(mActions[c], c);

#define ADD_ACTION_KEY(a, b, c, d) mActions[c] = a##Menu->addAction(b); \
mActions[c]->setShortcut(QKeySequence(d)); \
connect(mActions[c], SIGNAL(triggered()), a##Mapper, SLOT(map())); \
a##Mapper->setMapping(mActions[c], c);

extern int Imod_debug;

ImodvWindow::ImodvWindow(ImodvApp *a,
                         QWidget * parent, const char * name, Qt::WFlags f)
  : QMainWindow(parent, f)
{
  int numWidg = 0;
  mDBw = mSBw = mDBstw = mSBstw = NULL;
  mMinimized = false;
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);

  QSignalMapper *fileMapper = new QSignalMapper(this);
  QSignalMapper *editMapper = new QSignalMapper(this);
  QSignalMapper *viewMapper = new QSignalMapper(this);
  QSignalMapper *helpMapper = new QSignalMapper(this);

  // construct file menu
  QMenu *fileMenu = menuBar()->addMenu("&File");
  ADD_ACTION_KEY(file, "&Open Model", VFILE_MENU_LOAD, Qt::CTRL + Qt::Key_O);
  mActions[VFILE_MENU_LOAD]->setEnabled(a->standalone);

  ADD_ACTION(file, "&Save Model", VFILE_MENU_SAVE);
  mActions[VFILE_MENU_SAVE]->setEnabled(a->standalone);

  ADD_ACTION(file, "Save Model &As...", VFILE_MENU_SAVEAS);
  mActions[VFILE_MENU_SAVEAS]->setEnabled(a->standalone);
  fileMenu->addSeparator();

  ADD_ACTION(file, "Snap &Tiff As...", VFILE_MENU_SNAPTIFF);
  ADD_ACTION(file, "Snap &NonT As...", VFILE_MENU_SNAPRGB);
  ADD_ACTION(file, "&Zero Snap File #", VFILE_MENU_ZEROSNAP);
  ADD_ACTION(file, "S&et Snap Dir...", VFILE_MENU_SNAPDIR);
  ADD_ACTION_KEY(file, "&Movie...", VFILE_MENU_MOVIE, Qt::Key_M);

  ADD_ACTION_KEY(file, a->standalone ? "&Quit" : "&Close", VFILE_MENU_QUIT,
                 Qt::CTRL + Qt::Key_Q);

  connect(fileMapper, SIGNAL(mapped(int)), this, SLOT(fileMenuSlot(int)));

  // Construct Edit menu
  QMenu *editMenu = menuBar()->addMenu("&Edit");
  ADD_ACTION_KEY(edit, "&Objects...", VEDIT_MENU_OBJECTS, 
                 Qt::SHIFT + Qt::Key_O);
  ADD_ACTION_KEY(edit, "&Controls...", VEDIT_MENU_CONTROLS,
                 Qt::SHIFT + Qt::Key_C);
  ADD_ACTION_KEY(edit, "Object &List...", VEDIT_MENU_OBJLIST,
                 Qt::SHIFT + Qt::Key_L);
  ADD_ACTION_KEY(edit, "&Background...", VEDIT_MENU_BKG,
                 Qt::SHIFT + Qt::Key_B);
  ADD_ACTION_KEY(edit, "&Models...", VEDIT_MENU_MODELS,
                 Qt::SHIFT + Qt::Key_M);
  ADD_ACTION_KEY(edit, "&Views...", VEDIT_MENU_VIEWS,
                 Qt::SHIFT + Qt::Key_V);
  ADD_ACTION_KEY(edit, "&Image...", VEDIT_MENU_IMAGE,
                 Qt::SHIFT + Qt::Key_I);
  ADD_ACTION_KEY(edit, "Isos&urface...", VEDIT_MENU_ISOSURFACE, 
                 Qt::SHIFT + Qt::Key_U);
  mActions[VEDIT_MENU_IMAGE]->setEnabled(imodvByteImagesExist() != 0);
  mActions[VEDIT_MENU_ISOSURFACE]->setEnabled(imodvByteImagesExist() != 0);
  connect(editMapper, SIGNAL(mapped(int)), this, SLOT(editMenuSlot(int)));

  // View menu
  QMenu *viewMenu = menuBar()->addMenu("&View");
  ADD_ACTION_KEY(view, "Low &Res", VVIEW_MENU_LOWRES, Qt::SHIFT + Qt::Key_R);
  mActions[VVIEW_MENU_LOWRES]->setCheckable(true);
  mActions[VVIEW_MENU_LOWRES]->setChecked(a->lowres);
  if (!a->standalone) {
    ADD_ACTION_KEY(view, "&Current Point", VVIEW_MENU_CURPNT, Qt::Key_P);
    mActions[VVIEW_MENU_CURPNT]->setCheckable(true);
  }
  ADD_ACTION(view, "Bounding Bo&x", VVIEW_MENU_BOUNDBOX);
  mActions[VVIEW_MENU_BOUNDBOX]->setCheckable(true);

  ADD_ACTION(view, "&Stereo...", VVIEW_MENU_STEREO);
  ADD_ACTION(view, "&Depth Cue...", VVIEW_MENU_DEPTH);
  ADD_ACTION(view, "Scale &Bar...", VVIEW_MENU_SCALEBAR);

  ADD_ACTION(view, "Do&uble Buffer", VVIEW_MENU_DB);

  // This made it act on a shifted D only and steal it from imodv_input
  //mViewMenu->setAccel(Qt::Key_D, VVIEW_MENU_DB);
  mActions[VVIEW_MENU_DB]->setCheckable(true);
  mActions[VVIEW_MENU_DB]->setChecked(a->db > 0);
  mActions[VVIEW_MENU_DB]->setEnabled(a->db > 0 && a->enableDepthSB >= 0);

  ADD_ACTION(view, "&Lighting", VVIEW_MENU_LIGHTING);
  mActions[VVIEW_MENU_LIGHTING]->setCheckable(true);
  mActions[VVIEW_MENU_LIGHTING]->setChecked(a->lighting);

  ADD_ACTION(view, "&Wireframe", VVIEW_MENU_WIREFRAME);
  mActions[VVIEW_MENU_WIREFRAME]->setCheckable(true);
  connect(viewMapper, SIGNAL(mapped(int)), this, SLOT(viewMenuSlot(int)));

  // Help menu
  menuBar()->addSeparator();
  QMenu *helpMenu = menuBar()->addMenu("&Help");
  ADD_ACTION(help, "&Menus", VHELP_MENU_MENUS);
  ADD_ACTION(help, "&Keyboard", VHELP_MENU_KEYBOARD);
  ADD_ACTION(help, "M&ouse", VHELP_MENU_MOUSE);
  helpMenu->addSeparator();
  ADD_ACTION(help, "&About", VHELP_MENU_ABOUT);
  connect(helpMapper, SIGNAL(mapped(int)), this, SLOT(helpMenuSlot(int)));
  
  // Get the widget stack and the GL widgets
  mStack = new QStackedWidget(this);
  setCentralWidget(mStack);
  QGLFormat glFormat;

  if (a->enableDepthDB >= 0) { 
    mDBw = addGLWidgetToStack(&glFormat, true, a->enableDepthDB, false);
    mCurGLw = mDBw;
    numWidg++;
  }

  if (!numWidg && a->enableDepthDBst >= 0) { 
    mDBstw = addGLWidgetToStack(&glFormat, true, a->enableDepthDBst, true);
    if (!numWidg)
      mCurGLw = mDBstw;
    numWidg++;
  }

  if (a->enableDepthSB >= 0) { 
    mSBw = addGLWidgetToStack(&glFormat, false, a->enableDepthSB, false);
    if (!numWidg)
      mCurGLw = mSBw;
    numWidg++;
  }

  if (!numWidg && a->enableDepthSBst >= 0) { 
    mSBstw = addGLWidgetToStack(&glFormat, true, a->enableDepthSBst, true);
    if (!numWidg)
      mCurGLw = mSBstw;
    numWidg++;
  }

  // Set the topmost widget of the stack
  mStack->setCurrentWidget(mCurGLw);

  mTimer = new QTimer(this);
  connect(mTimer, SIGNAL(timeout()), this, SLOT(timeoutSlot()));
}

ImodvWindow::~ImodvWindow()
{
}

// Makes a GL widget with the goven properties and adds it to the stack
ImodvGL *ImodvWindow::addGLWidgetToStack(QGLFormat *glFormat, bool db,
                                         int enableDepth, bool stereo)
{
  ImodvGL *GLw;
  int id;
  glFormat->setRgba(true);
  glFormat->setDoubleBuffer(db);
  glFormat->setDepth(enableDepth > 0);
  glFormat->setStereo(stereo);
  GLw = new ImodvGL(*glFormat, mStack);
  id = mStack->addWidget(GLw);
  if (Imod_debug)
    imodPrintStderr("Added widget %d  db %d depth %d stereo %d\n", id, db?1:0,
                    enableDepth, stereo ? 1: 0);
  return GLw;
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
  if (id > 0 && id < LAST_VMENU_ID)
    mActions[id]->setChecked(state);
}

// Bring the selected widget to the top if it exists
int ImodvWindow::setGLWidget(ImodvApp *a, int db, int stereo)
{
  QGLFormat glFormat;

  // First create stereo widgets if going into stereo
  if (stereo) {
    if (Imodv->enableDepthDBst >= 0 && !mDBstw)
      mDBstw = addGLWidgetToStack(&glFormat, true, a->enableDepthDBst, true);
    if (Imodv->enableDepthSBst >= 0 && !mSBstw)
      mSBstw = addGLWidgetToStack(&glFormat, false, a->enableDepthSBst, true);
  }

  // Get the desired widget to top of stack
  if (db && !stereo && mDBw)
    mCurGLw = mDBw;
  else if (!db && !stereo && mSBw)
    mCurGLw = mSBw;
  else if (db && stereo && mDBstw)
    mCurGLw = mDBstw;
  else if (!db && stereo && mSBstw)
    mCurGLw = mSBstw;
  else
    return 1;
  mStack->setCurrentWidget(mCurGLw);

  // Now remove and destroy stereo widgets unless needed - won't work
  // all the time if there is no nonstereo DB widget, but that doesn't matter
  if (!stereo) {
    if (mDBw && mDBstw) {
      mStack->removeWidget(mDBstw);
      delete mDBstw;
      mDBstw = NULL;
      if (Imod_debug)
        imodPuts("Deleting DB stereo widget");
    }
    if (mSBstw && (mDBw || mDBstw || mSBw)) {
      mStack->removeWidget(mSBstw);
      delete mSBstw;
      mSBstw = NULL;
      if (Imod_debug)
        imodPuts("Deleting SB stereo widget");
    }
  }
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
  if (ImodvClosed)
    return;
  imodvQuit();
  e->accept();
}

// Manage the hiding and showing of other windows when this
// window is minimized or brought back
bool ImodvWindow::event(QEvent *e)
{
  bool minimized = e->type() == QEvent::WindowStateChange &&
    (windowState() & Qt::WindowMinimized);
  bool normal = e->type() == QEvent::WindowStateChange &&
    (windowState() & (Qt::WindowNoState | Qt::WindowMaximized));
  //imodPrintStderr("event type %d\n", e->type());
  if (Imod_debug && minimized)
    imodPrintStderr("State change to minimized\n");
  if (Imod_debug && e->type() == QEvent::Hide)
    imodPrintStderr("Hide\n");
  if (Imod_debug && normal)
    imodPrintStderr("State change to normal\n");
  if (Imod_debug && e->type() == QEvent::Show)
    imodPrintStderr("Show\n");
  if ((minimized || e->type() == QEvent::Hide) && !mMinimized) {
    if (Imod_debug)
      imodPuts("minimizing");
    mMinimized = true;
    imodvDialogManager.hide();
  } else if ((normal || e->type() == QEvent::Show) && mMinimized) {
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

ImodvGL::ImodvGL(QGLFormat inFormat, QWidget * parent)
  : QGLWidget(inFormat, parent)
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
Revision 4.23  2009/01/15 16:33:18  mast
Qt 4 port

Revision 4.22  2008/12/17 17:49:45  mast
Only create stereo widgets when going into stereo, throw away afterwards

Revision 4.21  2008/12/15 21:27:35  mast
Make stack with nonstereo and stereo db/sb widgets, take Imodv in constructor

Revision 4.20  2008/12/01 15:42:01  mast
Changes for undo/redo and selection in 3dmodv standalone

Revision 4.19  2008/11/28 06:43:32  mast
Added bounding box and current point

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
