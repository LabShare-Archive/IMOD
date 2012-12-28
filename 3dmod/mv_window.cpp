/*  
 *  mv_window.cpp -- Mainwindow and GLwidget classes for imodv window
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
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
#include "mv_window.h"
#include "imodv.h"
#include "mv_menu.h"
#include "mv_gfx.h"
#include "mv_input.h"
#include "control.h"
#include "vertexbuffer.h"

#define ADD_ACTION(a, b, c) mActions[c] = a##Menu->addAction(b); \
connect(mActions[c], SIGNAL(triggered()), a##Mapper, SLOT(map())); \
a##Mapper->setMapping(mActions[c], c);

#define ADD_ACTION_KEY(a, b, c, d) mActions[c] = a##Menu->addAction(b); \
mActions[c]->setShortcut(QKeySequence(d)); \
connect(mActions[c], SIGNAL(triggered()), a##Mapper, SLOT(map())); \
a##Mapper->setMapping(mActions[c], c);

ImodvWindow::ImodvWindow(ImodvApp *a,
                         QWidget * parent, const char * name, Qt::WFlags f)
  : QMainWindow(parent, f)
{
  int numWidg = 0;
  const char *helpStr = "&Help";
  const char *altHelp = ".&Help.";
  const char *useHelp = helpStr;
  mDBw = mSBw = mDBstw = mSBstw = mDBalw = mDBstAlw = NULL;
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
  ADD_ACTION_KEY(file, "&Movie/Montage...", VFILE_MENU_MOVIE, Qt::Key_M);
  ADD_ACTION_KEY(file, "Movie Seque&nce...", VFILE_MENU_SEQUENCE, Qt::Key_N);

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

  ADD_ACTION(view, "&Invert Z", VVIEW_MENU_INVERTZ);
  mActions[VVIEW_MENU_INVERTZ]->setCheckable(true);
  mActions[VVIEW_MENU_INVERTZ]->setChecked(a->invertZ);

  ADD_ACTION(view, "&Lighting", VVIEW_MENU_LIGHTING);
  mActions[VVIEW_MENU_LIGHTING]->setCheckable(true);
  mActions[VVIEW_MENU_LIGHTING]->setChecked(a->lighting);

  ADD_ACTION(view, "&Wireframe", VVIEW_MENU_WIREFRAME);
  mActions[VVIEW_MENU_WIREFRAME]->setCheckable(true);
  connect(viewMapper, SIGNAL(mapped(int)), this, SLOT(viewMenuSlot(int)));

  ADD_ACTION(view, "&Transparent Bkgd", VVIEW_MENU_TRANSBKGD);
  mActions[VVIEW_MENU_TRANSBKGD]->setCheckable(true);
  ADD_ACTION(view, "Do&uble Buffer", VVIEW_MENU_DB);

  // This made it act on a shifted D only and steal it from imodv_input
  //mViewMenu->setAccel(Qt::Key_D, VVIEW_MENU_DB);
  mActions[VVIEW_MENU_DB]->setCheckable(true);

  // Help menu
  // To stabilize the 3dmod menu with model view open, it had to have a 
  // modified menu name PLUS omit the code for the about entry - making it
  // conditional on standalone did not work!  Qt 4.5+ Cocoa Mac. 3/1/10
  // Even this is not perfect, if there is a 3dmodv already up.
#if defined(Q_OS_MACX) && QT_VERSION >= 0x040500
  if (!a->standalone)
    useHelp = altHelp;
#endif
  QMenu *helpMenu = menuBar()->addMenu(useHelp);
  ADD_ACTION(help, "&Menus", VHELP_MENU_MENUS);
  ADD_ACTION(help, "&Keyboard", VHELP_MENU_KEYBOARD);
  ADD_ACTION(help, "M&ouse", VHELP_MENU_MOUSE);
#if !(defined(Q_OS_MACX) && QT_VERSION >= 0x040500)
  helpMenu->addSeparator();
  ADD_ACTION(help, "&About", VHELP_MENU_ABOUT);
#endif
  connect(helpMapper, SIGNAL(mapped(int)), this, SLOT(helpMenuSlot(int)));
  
  // Get the widget stack and the GL widgets
  mStack = new QStackedWidget(this);
  setCentralWidget(mStack);
  QGLFormat glFormat;

  if (a->enableDepthDB >= 0) { 
    mDBw = addGLWidgetToStack(&glFormat, true, a->enableDepthDB, false, false);
    mCurGLw = mDBw;
    numWidg++;
  }

  if (a->enableDepthDBal >= 0) { 
    mDBalw = addGLWidgetToStack(&glFormat, true, a->enableDepthDB, false, true);
    if (!numWidg) {
      mCurGLw = mDBalw;
      a->alphaVisual = 1;
    }
    numWidg++;
  }

  if (!numWidg && a->enableDepthDBst >= 0) { 
    mDBstw = addGLWidgetToStack(&glFormat, true, a->enableDepthDBst, true, false);
    if (!numWidg)
      mCurGLw = mDBstw;
    numWidg++;
  }

  if (!numWidg && a->enableDepthDBstAl >= 0) { 
    mDBstAlw = addGLWidgetToStack(&glFormat, true, a->enableDepthDBstAl, true, true);
    if (!numWidg) {
      mCurGLw = mDBstAlw;
      a->alphaVisual = 1;
    }
    numWidg++;
  }

  if (a->enableDepthSB >= 0) { 
    mSBw = addGLWidgetToStack(&glFormat, false, a->enableDepthSB, false, false);
    if (!numWidg) {
      mCurGLw = mSBw;
      a->db = 0;
    }
    numWidg++;
  }

  if (!numWidg && a->enableDepthSBst >= 0) { 
    mSBstw = addGLWidgetToStack(&glFormat, true, a->enableDepthSBst, true, false);
    if (!numWidg) {
      mCurGLw = mSBstw;
      a->db = 0;
    }
    numWidg++;
  }
  a->dbPossible = a->db;

  if (a->transBkgd) {
    if (a->db && a->enableDepthDBal >= 0) {
      mCurGLw = mDBalw;
      a->alphaVisual = 1;
    } else {
      a->transBkgd = 0;
    }
  }

  // Set the topmost widget of the stack
  mStack->setCurrentWidget(mCurGLw);
  mActions[VVIEW_MENU_DB]->setChecked(a->db > 0);
  mActions[VVIEW_MENU_DB]->setEnabled(a->db > 0 && a->enableDepthSB >= 0 && 
                                      !a->transBkgd);
  mActions[VVIEW_MENU_TRANSBKGD]->setChecked(a->transBkgd > 0);
  mActions[VVIEW_MENU_TRANSBKGD]->setEnabled(a->db > 0 && (a->enableDepthDBal >= 0 || 
                                                           a->enableDepthDBstAl >= 0));

  mTimer = new QTimer(this);
  connect(mTimer, SIGNAL(timeout()), this, SLOT(timeoutSlot()));
}

ImodvWindow::~ImodvWindow()
{
}

// Makes a GL widget with the goven properties and adds it to the stack
ImodvGL *ImodvWindow::addGLWidgetToStack(QGLFormat *glFormat, bool db,
                                         int enableDepth, bool stereo, bool alpha)
{
  ImodvGL *GLw;
  int id;
  glFormat->setRgba(true);
  glFormat->setDoubleBuffer(db);
  glFormat->setDepth(enableDepth > 0);
  glFormat->setStereo(stereo);
  glFormat->setAlpha(alpha);
  if (!imodDebug('M'))
    glFormat->setSampleBuffers(true);
  GLw = new ImodvGL(*glFormat, mStack);
  id = mStack->addWidget(GLw);
  if (Imod_debug)
    imodPrintStderr("Added widget %d  db %d depth %d stereo %d alpha %d\n", id, db?1:0,
                    enableDepth, stereo ? 1: 0, alpha ? 1 : 0);
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

void ImodvWindow::setEnabledMenuItem(int id, bool state)
{
  if (id > 0 && id < LAST_VMENU_ID)
    mActions[id]->setEnabled(state);
}

// Bring the selected widget to the top if it exists
int ImodvWindow::setGLWidget(ImodvApp *a, int db, int stereo, int alpha)
{
  QGLFormat glFormat;

  // First create stereo widgets if going into stereo
  if (stereo) {
    if (Imodv->enableDepthDBst >= 0 && !mDBstw)
      mDBstw = addGLWidgetToStack(&glFormat, true, a->enableDepthDBst, true, false);
    if (Imodv->enableDepthDBstAl >= 0 && !mDBstAlw)
      mDBstAlw = addGLWidgetToStack(&glFormat, true, a->enableDepthDBstAl, true, true);
    if (Imodv->enableDepthSBst >= 0 && !mSBstw)
      mSBstw = addGLWidgetToStack(&glFormat, false, a->enableDepthSBst, true, false);
  }

  // Remove vertex buffer data from all models
  for (int m = 0; m < a->nm; m++)
    vbCleanupVBD(a->mod[m]);

  // Get the desired widget to top of stack
  if (db && !stereo && !alpha && mDBw)
    mCurGLw = mDBw;
  else if (db && !stereo && alpha && mDBalw)
    mCurGLw = mDBalw;
  else if (!db && !stereo && mSBw)
    mCurGLw = mSBw;
  else if (db && stereo && !alpha && mDBstw)
    mCurGLw = mDBstw;
  else if (db && stereo && alpha && mDBstAlw)
    mCurGLw = mDBstAlw;
  else if (!db && stereo && mSBstw)
    mCurGLw = mSBstw;
  else
    return 1;
  mStack->setCurrentWidget(mCurGLw);

  // Now remove and destroy stereo widgets unless needed - won't work
  // all the time if there is no nonstereo DB widget, but that doesn't matter
  if (!stereo) {
    if ((mDBw || mDBalw) && mDBstw) {
      mStack->removeWidget(mDBstw);
      delete mDBstw;
      mDBstw = NULL;
      if (Imod_debug)
        imodPuts("Deleting DB stereo widget");
    }
    if (mSBstw && (mDBw || mDBalw || mDBstw || mSBw)) {
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
  imodvScrollWheel(e);
}

