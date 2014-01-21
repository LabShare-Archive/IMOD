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
#include "dia_qtutils.h"
#include "imod.h"
#include "mv_window.h"
#include "imodv.h"
#include "mv_menu.h"
#include "mv_gfx.h"
#include "mv_input.h"
#include "mv_control.h"
#include "info_cb.h"
#include "control.h"
#include "preferences.h"
#include "rotationtool.h"
#include "resizetool.h"
#include "vertexbuffer.h"

#define ADD_ACTION(a, b, c) mActions[c] = a##Menu->addAction(b); \
connect(mActions[c], SIGNAL(triggered()), a##Mapper, SLOT(map())); \
a##Mapper->setMapping(mActions[c], c);

#define ADD_ACTION_KEY(a, b, c, d) mActions[c] = a##Menu->addAction(b); \
mActions[c]->setShortcut(QKeySequence(d)); \
connect(mActions[c], SIGNAL(triggered()), a##Mapper, SLOT(map())); \
a##Mapper->setMapping(mActions[c], c);

static PopupEntry sPopupTable[] = {
  {"Model Orientation and Zoom", -1, 0, 0, 0},
  {"Show top of model", Qt::Key_T, 0, 0, 0},
  {"Show bottom of model", Qt::Key_B, 0, 0, 0},
  {"Show left side of model", Qt::Key_L, 0, 0, 0},
  {"Show right side of model", Qt::Key_R, 0, 0, 0},
  {"Show front of model", Qt::Key_F, 0, 0, 0},
  {"Show back of model", Qt::Key_K, 0, 0, 0},
  {"", -3, 0, 0, 0},
  {"Decrease zoom by small steps", Qt::Key_Minus, 0, 0, 0},
  {"Increase zoom by small steps", Qt::Key_Equal, 0, 0, 0},
  {"Decrease zoom by large steps", Qt::Key_Underscore, 0, 0, 0},
  {"Increase zoom by large steps", Qt::Key_Plus, 0, 0, 0},
  {"", -3, 0, 0, 0},
  {"Decrease rotation increment and speed", Qt::Key_Comma, 0, 0, 0},
  {"Increase rotation increment and speed", Qt::Key_Period, 0, 0, 0},
  {"Model/Time Selection", -1, 0, 0, 0},

  // On the Mac, setting up shortcuts kills the keypad, so set mainIndex -1 to avoid the
  // shortcut and put key in the text
#ifdef Q_OS_MACX
  {"Toggle displaying all models/one model  8", Qt::Key_8, 0, 0, -1},
  {"Move to previous model time                   1", Qt::Key_1, 0, 0, -1},
  {"Move to next model time                          2", Qt::Key_2, 0, 0, -1},
  {"Switch to previous model                         9", Qt::Key_9, 0, 0, -1},
  {"Switch to next model                               0", Qt::Key_0, 0, 0,-1},
#else
  {"Toggle displaying all models or one model", Qt::Key_8, 0, 0, 0},
  {"Move to previous model time", Qt::Key_1, 0, 0, 0},
  {"Move to next model time", Qt::Key_2, 0, 0, 0},
  {"Switch to previous model", Qt::Key_9, 0, 0, 0},
  {"Switch to next model", Qt::Key_0, 0, 0, 0},
#endif
  {"Display Quality Controls", -1, 0, 0, 0},
  {"Toggle low resolution drawing of meshes", Qt::Key_R, 1, 0, VVIEW_MENU_LOWRES},
  {"Decrease the quality of sphere drawing", Qt::Key_G, 0, 1, 0},
  {"Increase the quality of sphere drawing", Qt::Key_G, 0, 0, 0},
  {"Open Dialogs", -1, 0, 0, 0},
  {"Open movie control window",  Qt::Key_M, 0, 0, VFILE_MENU_MOVIE},
  {"Open Object Edit window", Qt::Key_O, 0, 1, VEDIT_MENU_OBJECTS},
  {"Open controls window", Qt::Key_C, 0, 1, VEDIT_MENU_CONTROLS},
  {"Open Rotation tool", Qt::Key_R, 0, 1, VEDIT_MENU_ROTATION},
  {"Open background color window", Qt::Key_B, 0, 1, VEDIT_MENU_BKG},
  {"Open Object List window", Qt::Key_L, 0, 1, VEDIT_MENU_OBJLIST},
  {"Open model selection window", Qt::Key_M, 0, 1, VEDIT_MENU_MODELS},
  {"Open view editing window", Qt::Key_V, 0, 1, VEDIT_MENU_VIEWS},
  {"Open image overlay control window", Qt::Key_I, 0, 1, VEDIT_MENU_IMAGE},
  {"Open isosurface display control window", Qt::Key_U, 0, 1, VEDIT_MENU_ISOSURFACE},
  {"Stereo Controls", -1, 0, 0, 0},
  {"Toggle stereo mode", Qt::Key_S, 0, 0, 0},
  {"Reduce parallax for stereo viewing", Qt::Key_BracketLeft, 0, 0, 0},
  {"Increase parallax for stereo viewing", Qt::Key_BracketRight, 0, 0, 0},
  {"Invert the parallax angle", Qt::Key_A, 0, 0, 0},
  {"Model Editing", -1, 0, 0, 0},
  {"Delete current scattered point", Qt::Key_Delete, 1, 1, 0},
  {"Select visible contours in current object", Qt::Key_A, 1, 0, 0},
  {"Delete selected contour(s) picked here", Qt::Key_D, 0, 0, 0},
  {"Undo last change to model", Qt::Key_Z, 1, 0, 0},
  {"Redo the last undone change to model", Qt::Key_Y, 1, 0, 0},
  {"", -2, 0, 0, 0},
  {"", -3, 0, 0, 0},
  {"Make TIFF snapshot of window", Qt::Key_S, 1, 0, 0},
  {"Make non-TIFF snapshot of window", Qt::Key_S, 0, 1, 0},
  {"Make 2nd nonTIFF format snapshot of window", Qt::Key_S, 1, 1, 0},
  {"", -3, 0, 0, 0},
  {"Toggle current clipping plane on and off", Qt::Key_C, 1, 0, 0},
  {"Toggle use of vertex buffer objects", Qt::Key_V, 1, 1, 0},
  {"", 0, 0, 0, 0}};

// Statics for rotation tool
static QIcon *sMovieIcon = NULL;
int sRToolLeftPos = -9999;
int sRToolTopPos;
int sRToolWithImodv = -1;

// Constructor for window
ImodvWindow::ImodvWindow(ImodvApp *a,
                         QWidget * parent, const char * name, Qt::WFlags f)
  : QMainWindow(parent, f)
{
  int numWidg = 0;
  const char *helpStr = "&Help";
  const char *useHelp = helpStr;
  double posValues[3];
  int numVals = 0;

  mRotationTool = NULL;
  mResizeTool = NULL;
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
  ADD_ACTION_KEY(edit, "&Objects...", VEDIT_MENU_OBJECTS, Qt::SHIFT + Qt::Key_O);
  ADD_ACTION_KEY(edit, "&Controls...", VEDIT_MENU_CONTROLS, Qt::SHIFT + Qt::Key_C);
  ADD_ACTION_KEY(edit, "&Rotation...", VEDIT_MENU_ROTATION, Qt::SHIFT + Qt::Key_R);
  ADD_ACTION_KEY(edit, "Object &List...", VEDIT_MENU_OBJLIST, Qt::SHIFT + Qt::Key_L);
  ADD_ACTION_KEY(edit, "&Background...", VEDIT_MENU_BKG, Qt::SHIFT + Qt::Key_B);
  ADD_ACTION_KEY(edit, "&Models...", VEDIT_MENU_MODELS, Qt::SHIFT + Qt::Key_M);
  ADD_ACTION_KEY(edit, "&Views...", VEDIT_MENU_VIEWS, Qt::SHIFT + Qt::Key_V);
  ADD_ACTION_KEY(edit, "&Image...", VEDIT_MENU_IMAGE, Qt::SHIFT + Qt::Key_I);
  ADD_ACTION_KEY(edit, "Isos&urface...", VEDIT_MENU_ISOSURFACE, Qt::SHIFT + Qt::Key_U);
  mActions[VEDIT_MENU_IMAGE]->setEnabled(imodvByteImagesExist() != 0);
  mActions[VEDIT_MENU_ISOSURFACE]->setEnabled(imodvByteImagesExist() != 0);
  connect(editMapper, SIGNAL(mapped(int)), this, SLOT(editMenuSlot(int)));

  // View menu
  QMenu *viewMenu = menuBar()->addMenu("&View");
  ADD_ACTION_KEY(view, "Low &Resolution", VVIEW_MENU_LOWRES, Qt::CTRL + Qt::Key_R);
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
  ADD_ACTION(view, "Window Si&ze...", VVIEW_MENU_RESIZE);

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

  QMenu *keyMenu = menuBar()->addMenu("Hot&Keys");
  QSignalMapper *keyMapper = new QSignalMapper(this);
  connect(keyMapper, SIGNAL(mapped(int)), this, SLOT(keyMenuSlot(int)));
  utilBuildPopupMenu(sPopupTable, false, keyMapper, keyMenu, mNumKeyEntries, mActions);

  // Help menu
  // To stabilize the 3dmod menu with model view open, it had to have a 
  // modified menu name PLUS omit the code for the about entry - making it
  // conditional on standalone did not work!  Qt 4.5+ Cocoa Mac. 3/1/10
  // Even this is not perfect, if there is a 3dmodv already up.
#if defined(Q_OS_MACX) && QT_VERSION >= 0x040500
  const char *altHelp = ".&Help.";
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
      a->dblBuf = 0;
    }
    numWidg++;
  }

  if (!numWidg && a->enableDepthSBst >= 0) { 
    mSBstw = addGLWidgetToStack(&glFormat, true, a->enableDepthSBst, true, false);
    if (!numWidg) {
      mCurGLw = mSBstw;
      a->dblBuf = 0;
    }
    numWidg++;
  }
  a->dbPossible = a->dblBuf;

  if (a->transBkgd) {
    if (a->dblBuf && a->enableDepthDBal >= 0) {
      mCurGLw = mDBalw;
      a->alphaVisual = 1;
    } else {
      a->transBkgd = 0;
    }
  }

  // Set the topmost widget of the stack
  mStack->setCurrentWidget(mCurGLw);
  mActions[VVIEW_MENU_DB]->setChecked(a->dblBuf > 0);
  mActions[VVIEW_MENU_DB]->setEnabled(a->dblBuf > 0 && a->enableDepthSB >= 0 && 
                                      !a->transBkgd);
  mActions[VVIEW_MENU_TRANSBKGD]->setChecked(a->transBkgd > 0);
  mActions[VVIEW_MENU_TRANSBKGD]->setEnabled(a->dblBuf > 0 && (a->enableDepthDBal >= 0 || 
                                                           a->enableDepthDBstAl >= 0));

  mTimer = new QTimer(this);
  connect(mTimer, SIGNAL(timeout()), this, SLOT(timeoutSlot()));

  // First time, get the preferences for rotation tool and store in statics
  if (sRToolWithImodv < 0) {
    numVals = ImodPrefs->getGenericSettings("ModvRotationTool", posValues, 3);
    if (numVals > 1) {
      sRToolLeftPos = (int)posValues[0];
      sRToolTopPos = (int)posValues[1];
    }
    if (numVals > 2) 
      sRToolWithImodv = (int)posValues[2];
    else
      sRToolWithImodv = 0;
  }

  // Open tool if it closed with window before
  if (sRToolWithImodv > 0)
    openRotationTool(a);
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

void ImodvWindow::keyMenuSlot(int which)
{
  Qt::KeyboardModifiers modifiers;
  int key = utilLookupPopupHit(which, &sPopupTable[0], mNumKeyEntries, modifiers);
  QKeyEvent *event = new QKeyEvent(QEvent::KeyPress, key, modifiers);
  imodvKeyPress(event);
  delete event;
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
  for (int m = 0; m < a->numMods; m++)
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

void ImodvWindow::rotationKeyPress(QKeyEvent *e)
{
  if (utilCloseKey(e))
    mRotationTool->close();
  else
    imodvKeyPress(e);
}

void ImodvWindow::rotationKeyRelease(QKeyEvent *e)
{
  imodvKeyRelease(e);
}

void ImodvWindow::resizerKeyPress(QKeyEvent *e)
{
  if (utilCloseKey(e))
    mResizeTool->close();
  else
    imodvKeyPress(e);
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

/*
 * Rotation tool creation and slots
 */
void ImodvWindow::openRotationTool(ImodvApp *a)
{
  int width = 90;
  const char *fileList[1][2] =
    {{":/images/movieArrow.png", ":/images/movieArrowOn.png"}};
  if (mRotationTool)
    return;
  if (!sMovieIcon)
    utilFileListsToIcons(fileList, &sMovieIcon, 1);

  mRotationTool = new RotationTool(imodvDialogManager.parent(IMODV_DIALOG), sMovieIcon,
                                   "Start or stop movie", 20, true, a->deltaRot / 10.);
  mRotationTool->setWindowFlags(Qt::Window | Qt::WindowStaysOnTopHint);
  mRotationTool->setAttribute(Qt::WA_DeleteOnClose);
  mRotationTool->setAttribute(Qt::WA_AlwaysShowToolTips);
  connect(mRotationTool, SIGNAL(closing()), this, SLOT(rotationClosing()));
  connect(mRotationTool, SIGNAL(stepChanged(int)), this, SLOT(rotStepChanged(int)));
  connect(mRotationTool, SIGNAL(centerButToggled(bool)), this, 
          SLOT(movieButToggled(bool)));
  connect(mRotationTool, SIGNAL(rotate(int, int, int)), this, 
          SLOT(rotateClicked(int, int, int)));
  connect(mRotationTool, SIGNAL(keyPress(QKeyEvent *)), this, 
          SLOT(rotationKeyPress(QKeyEvent *)));
  connect(mRotationTool, SIGNAL(keyRelease(QKeyEvent *)), this, 
          SLOT(rotationKeyRelease(QKeyEvent *)));
  imodvDialogManager.add((QWidget *)mRotationTool, IMODV_DIALOG);

  // More than minimal width is needed on Linux to be able to drag the window by its 
  // title bar: this needs to be checked elsewhere
  // The resize to a height too small is a way to keep it from opening too tall, the
  // usual adjustSize didn't work.
  mRotationTool->setFixedWidth(width);
  mRotationTool->resize(width, 50);
  if (sRToolLeftPos >= 0) {
    diaLimitWindowPos(mRotationTool->width(), mRotationTool->height(), sRToolLeftPos,
                      sRToolTopPos);
    mRotationTool->move(sRToolLeftPos, sRToolTopPos);
  }
  mRotationTool->show();
}

void ImodvWindow::rotateClicked(int deltaX, int deltaY, int deltaZ)
{
  imodv_rotate_model(Imodv, -Imodv->deltaRot * deltaX, Imodv->deltaRot * deltaY, 
                     Imodv->deltaRot * deltaZ);
}

void ImodvWindow::rotStepChanged(int delta)
{
  imodvControlChangeSteps(Imodv, delta);
}

void ImodvWindow::movieButToggled(bool state)
{
  imodvControlStart();
}

// Window is closing: save the position and whether the model view it is closing with
// model view in statics and preferences
void ImodvWindow::rotationClosing()
{
  double posValues[3];
  QRect pos = ivwRestorableGeometry(mRotationTool);
  sRToolLeftPos = posValues[0] = pos.left();
  sRToolTopPos = posValues[1] = pos.top();
  sRToolWithImodv = posValues[2] = ImodvClosed;
  ImodPrefs->saveGenericSettings("ModvRotationTool", 3, posValues);
  imodvDialogManager.remove((QWidget *)mRotationTool);
  mRotationTool = NULL;
}

/*
 * The resize tool
 */
void ImodvWindow::openResizeTool(ImodvApp *a)
{
  if (mResizeTool) {
    mResizeTool->raise();
    return;
  }
  mResizeTool = new ResizeTool(this, a->winx, a->winy, 10);
  connect(mResizeTool, SIGNAL(closing()), this, SLOT(resizerClosing()));
  connect(mResizeTool, SIGNAL(resize(int, int)), this, SLOT(newResizerSize(int, int)));
  connect(mResizeTool, SIGNAL(keyPress(QKeyEvent *)), this, 
          SLOT(resizerKeyPress(QKeyEvent *)));
  connect(mResizeTool, SIGNAL(keyRelease(QKeyEvent *)), this, 
          SLOT(rotationKeyRelease(QKeyEvent *)));
  imodvDialogManager.add((QWidget *)mResizeTool, IMODV_DIALOG);
  adjustGeometryAndShow((QWidget *)mResizeTool, IMODV_DIALOG);
}

// A new size comes in.  Compute a new rad value to keep scale constant, set window size
// accounting for the menu bar
void ImodvWindow::newResizerSize(int sizeX, int sizeY)
{
  ImodvApp *a = Imodv;
  a->imod->view->rad *= ((float)B3DMIN(sizeX, sizeY)) / B3DMIN(a->winx, a->winy);
  a->mainWin->resize(sizeX, sizeY + a->mainWin->height() - a->winy);
}

// The window is closing, remove its pointers
void ImodvWindow::resizerClosing()
{
  if (!mResizeTool)
    return;
  imodvDialogManager.remove((QWidget *)mResizeTool);
  mResizeTool = NULL;
}

/* 
 * The GL class
 */
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

