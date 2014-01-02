/*
 *  xyz.cpp -- Open the XYZ Window; View the X, Y and Z axis.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <math.h>
#include <qdatetime.h>
#include <qapplication.h>
#include <qcursor.h>
//Added by qt3to4:
#include <QTimerEvent>
#include <QMouseEvent>
#include <QCloseEvent>
#include <QKeyEvent>
#include <QVBoxLayout>
#include <QFrame>
#include "hottoolbar.h"
#include <qlayout.h>
#include <qlabel.h>
#include <qtoolbutton.h>
#include <qsignalmapper.h>
#include <qtooltip.h>
#include <qbitmap.h>
#include <qslider.h>
#include <qpushbutton.h>

#include "imod.h"
#include "xxyz.h"
#include "display.h"
#include "b3dgfx.h"
#include "control.h"
#include "cont_edit.h"
#include "info_cb.h"
#include "imod_input.h"
#include "autox.h"
#include "pixelview.h"
#include "imod_edit.h"
#include "model_edit.h"
#include "workprocs.h"
#include "moviecon.h"
#include "preferences.h"
#include "undoredo.h"
#include "istore.h"
#include "finegrain.h"
#include "arrowbutton.h"
#include "tooledit.h"
#include "model_edit.h"
#include "dia_qtutils.h"
#include "multislider.h"
#include "scalebar.h"
#include "cachefill.h"

#define BM_WIDTH 16
#define BM_HEIGHT 16
#define XYZ_BSIZE 8
#define XYZ_GSIZE 16
#define ALL_BORDER (2 * XYZ_BSIZE + XYZ_GSIZE)
#define GRAB_LENGTH 7
#define GRAB_WIDTH 3
#define XYZ_TOGGLE_RESOL 0
#define XYZ_TOGGLE_LOCKED 1
#define XYZ_TOGGLE_ZSCALE 2
#define MAX_SLIDER_WIDTH 100
#define MIN_SLIDER_WIDTH 20
#define NOTNEW -999999999

static const char *fileList[MAX_XYZ_TOGGLES][2] =
  {{":/images/lowres.png", ":/images/highres.png"},
   {":/images/unlock.png", ":/images/lock.png"},
   {":/images/zscale.png", ":/images/zscaleOn.png"}};
static const char *toggleTips[] = {
  "Toggle between regular and high-resolution (interpolated) image",
  "Keep display at set location and time regardless of global location",
  "Apply model Z scaling to display"};

static QIcon *icons[MAX_XYZ_TOGGLES];
static QIcon *cenIcon = NULL;
static QIcon *fillIcon = NULL;

static const char *sliderLabels[] = {"X", "Y", "Z"};
enum {X_COORD = 0, Y_COORD, Z_COORD};

static PopupEntry sPopupTable[] = {
  {"Toggle projection of current contour on planes", Qt::Key_P, 0, 1, 0},
  {"Report distance from current point to cursor", Qt::Key_Q, 0, 0, 0},
  {"Center current point in all panels", Qt::Key_K, 0, 0, 0},
  {"Toggle high-quality interpolation", Qt::Key_R, 0, 0, 0},
  {"", 0, 0, 0, 0}};

/*************************** internal functions ***************************/
static void xyzKey_cb(ImodView *vi, void *client, int released, QKeyEvent *e);
static void xyzClose_cb(ImodView *vi, void *client, int junk);
static void xyzDraw_cb(ImodView *vi, void *client, int drawflag);

static QTime but1downt;
static int xyzShowSlice = 0;
static bool pixelViewOpen = false;
static int insertDown = 0;
static QTime insertTime;

/* routine for opening or raising the window */
int xxyz_open(ImodView *vi)
{
  XyzWindow *xyz;
  xyz = new XyzWindow(vi, App->rgba, App->doublebuffer, App->qtEnableDepth, 
                      imodDialogManager.parent(IMOD_IMAGE), "xyz window");
  if (!xyz || (!vi->pyrCache && (!xyz->mFdataxz || !xyz->mFdatayz))) {
    wprint("Error:\n\tXYZ window can't open due to low memory\n");
    if (xyz)
      xyz->close();
      delete xyz;
    return(-1);
  }
  return(0);
}

/* The controller calls here to draw the window */
static void xyzDraw_cb(ImodView *vi, void *client, int drawflag)
{
  XyzWindow *xyz = (XyzWindow *)client;
  int pixSize = ivwGetPixelBytes(xyz->mVi->rawImageStore);

  if ((!vi) || (!xyz) || (!drawflag)) 
    return;
     
  if (drawflag & IMOD_DRAW_COLORMAP) {
    xyz->mGLw->setColormap(*(App->qColormap));
    return;
  }

  xyz->SetCursor(vi->imod->mousemode);

  if (drawflag) {
    if (drawflag & IMOD_DRAW_IMAGE) {

      xyz->mLx = xyz->mLy = xyz->mLz = -1;
      b3dFlushImage(xyz->mXydata);
      b3dFlushImage(xyz->mXzdata);
      b3dFlushImage(xyz->mYzdata);

      /* This happens whens a flip occurs: get new image spaces and geometry */
      if (xyz->mToolMaxY != xyz->mVi->ysize || xyz->mToolMaxZ != xyz->mVi->zsize) {
        B3DFREE(xyz->mFdataxz);
        B3DFREE(xyz->mFdatayz);
        xyz->mFdataxz  = (unsigned char *)malloc(vi->xsize * vi->zsize * pixSize);
        xyz->mFdatayz  = (unsigned char *)malloc(vi->ysize * vi->zsize * pixSize);
        xyz->GetCIImages();
      }
    }
    if (drawflag & IMOD_DRAW_SLICE)
      xyzShowSlice = 1;
    xyz->Draw();
  }
  return;
}

// This receives the close signal back from the controller, and tells the
// window to close
static void xyzClose_cb(ImodView *vi, void *client, int junk)
{
  XyzWindow *xyz = (XyzWindow *)client;
  xyz->close();
}

// This receives a key from the controller
static void xyzKey_cb(ImodView *vi, void *client, int released, QKeyEvent *e)
{
  XyzWindow *xyz = (XyzWindow *)client;
  if (!released)
    xyz->keyPressPassedOn(e);
}

// Notification that pixel view is open or closed
void xyzPixelViewState(bool state)
{
  QObjectList objList;
  XyzWindow *xyz;
  pixelViewOpen = state;
  int i;
  imodDialogManager.windowList(&objList, -1, XYZ_WINDOW_TYPE);
  for (i = 0; i < objList.count(); i++) {
    xyz = ((XyzWindow *)objList.at(i));
    xyz->mGLw->setMouseTracking(state || insertDown != 0);
  }
}

float xyzScaleBarSize()
{
  XyzWindow *xyz = getTopXYZ();
  if (!xyz)
    return -1;
  return xyz->mScaleBarSize;
}

// Find the XYZ window that is nearest to the top of the control list
XyzWindow *getTopXYZ()
{
  return (XyzWindow *)imodDialogManager.getTopWindow(XYZ_WINDOW_TYPE);
}

// Implementation of the window class
XyzWindow::XyzWindow(ImodView *vi, bool rgba, bool doubleBuffer, 
                     bool enableDepth, QWidget * parent,
                     const char * name, Qt::WFlags f) 
  : QMainWindow(parent, f)
{
  ArrowButton *upArrow, *downArrow;
  QToolButton *button;
  double newzoom;
  int needWinx, needWiny, maxWinx, maxWiny, xleft, ytop, toolHeight;
  int newHeight, newWidth;
  float zscaled = vi->zsize;
  int pixSize = ivwGetPixelBytes(vi->rawImageStore);

  mXydata = mXzdata = mYzdata = NULL;
  for (ytop = 0; ytop < NUM_AXIS; ytop++)
    mDisplayedAxisLocation[ytop] = -100;

  // For tile cache, side panel arrays will be maintained as needed on the fly
  if (vi->pyrCache) {
    mFdataxz = mFdatayz = NULL;
    mFdataXZsize = mFdataYZsize = 0;

  } else {

    // Otherwise get separate fdata for each side panel
    mFdataxz = (unsigned char *)malloc(vi->xsize * vi->zsize * pixSize);
    mFdatayz = (unsigned char *)malloc(vi->ysize * vi->zsize * pixSize);
    if ((!mFdataxz) || (!mFdatayz)) {
      B3DFREE(mFdataxz);
      B3DFREE(mFdatayz);
      return;
    }
  }

  mVi   = vi;
  mExposed = 0;
  mZoom = 1.0;
  mXtrans1 = 0;
  mYtrans1 = 0;
  mXtrans2 = 0;
  mYtrans2 = 0;
  mHq = ImodPrefs->startInHQ() ? 1 : 0;
  mApplyZscale = ImodPrefs->getXyzApplyZscale();
  mProject = 0;
  mMousemode = IMOD_MMOVIE;
  mToolZoom = -1.0f;
  mToolMaxX = vi->xsize;
  mToolMaxY = vi->ysize;
  mToolMaxZ = vi->zsize;
  if (mApplyZscale)
    zscaled *= vi->imod->zscale;
  mXzFraction = (zscaled) / (vi->xsize + zscaled);
  mYzFraction = (zscaled) / (vi->ysize + zscaled);
  mLock = 0;
  mTimeLock = 0;
  mTimeDrawn = -1;
  mLastTileCacheInd = -1;
  
  mCtrlPressed = false;
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  setAnimated(false);

  if (!cenIcon) {
    cenIcon = new QIcon();
    cenIcon->addFile(":/images/keepCenter.png", QSize(BM_WIDTH, BM_HEIGHT));
    utilFileListsToIcons(fileList, icons, MAX_XYZ_TOGGLES);
  }

  if (mVi->pyrCache && !fillIcon) {
    fillIcon = new QIcon();
    fillIcon->addFile(":/images/fillCache.png", QSize(BM_WIDTH, BM_HEIGHT));
  }

  // Get the toolbar, add zoom arrows
  mToolBar = utilMakeToolBar(this, false, TB_AUTO_RAISE ? 0 : 4, "XYZ Toolbar");

  mZoomEdit = utilTBZoomTools(this, mToolBar, &upArrow, &downArrow);
  connect(upArrow, SIGNAL(clicked()), this, SLOT(zoomUp()));
  connect(downArrow, SIGNAL(clicked()), this, SLOT(zoomDown()));
  connect(mZoomEdit, SIGNAL(editingFinished()), this, SLOT(newZoom()));

  // Make the toggle buttons and their signal mapper
  QSignalMapper *toggleMapper = new QSignalMapper(mToolBar);
  connect(toggleMapper, SIGNAL(mapped(int)), this, SLOT(toggleClicked(int)));
  int j;
  for (j = 0; j < MAX_XYZ_TOGGLES; j++) {
    utilSetupToggleButton(mToolBar, mToolBar, NULL, toggleMapper, icons, 
                          toggleTips, mToggleButs, mToggleStates, j);
    connect(mToggleButs[j], SIGNAL(clicked()), toggleMapper, SLOT(map()));
  }
  mToggleStates[XYZ_TOGGLE_RESOL] = mHq;
  diaSetChecked(mToggleButs[XYZ_TOGGLE_RESOL], mHq != 0);
  mToggleStates[XYZ_TOGGLE_ZSCALE] = mApplyZscale;
  diaSetChecked(mToggleButs[XYZ_TOGGLE_ZSCALE], mApplyZscale != 0);

  // Make simple pushbutton for centering
  utilTBToolButton(this, mToolBar, &button, "Center windows on current image"
                   " or model point (hot key K)");
  button->setIcon(*cenIcon);
  connect(button, SIGNAL(clicked()), this, SLOT(centerClicked()));

  if (mVi->pyrCache) {
    utilTBToolButton(this, mToolBar, &button, "Fill cache for currently displayed area");
    button->setIcon(*fillIcon);
    connect(button, SIGNAL(clicked()), this, SLOT(fillCachePressed()));
  }

  // Make a frame, put a layout in it, and then put multisliders in the layout
  QFrame *sliderFrame = new QFrame();
  mToolBar->addWidget(sliderFrame);
  QVBoxLayout *sliderLayout = new QVBoxLayout(sliderFrame);
  mSliders = new MultiSlider(sliderFrame, NUM_AXIS, sliderLabels, 0,
            255, 0, true);
  sliderLayout->addLayout(mSliders->getLayout());  
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this, 
    SLOT(sliderChanged(int, int, bool)));

  utilTBPushButton("Help", this, mToolBar, &mHelpButton, "Open help window");
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(help()));
  setFontDependentWidths();

  setMaxAxis(X_COORD, mVi->xsize - 1);
  setMaxAxis(Y_COORD, mVi->ysize - 1);
  setMaxAxis(Z_COORD, mVi->zsize - 1);
  
  QGLFormat glFormat;
  glFormat.setRgba(rgba);
  glFormat.setDoubleBuffer(doubleBuffer);
  glFormat.setDepth(enableDepth);
  mGLw = new XyzGL(glFormat, this);
  
  // Set it as main widget, set focus
  setCentralWidget(mGLw);
  setFocusPolicy(Qt::StrongFocus);

  if (!App->rgba)
    mGLw->setColormap(*(App->qColormap));

  mGLw->setMinimumSize(2 * ALL_BORDER, 2 * ALL_BORDER);

  setWindowTitle(imodCaption("3dmod XYZ Window"));

  mLx = mLy = mLz = mLastCacheSum = -1;

  mCtrl = ivwNewControl(vi, xyzDraw_cb, xyzClose_cb, xyzKey_cb, (void *)this);
  imodDialogManager.add((QWidget *)this, IMOD_IMAGE, XYZ_WINDOW_TYPE, mCtrl);
  
  // Determine needed window size, zoom that makes it mostly fit on the screen
  diaMaximumWindowSize(maxWinx, maxWiny);
  needWinx = vi->xsize + (int)zscaled + ALL_BORDER;
  needWiny = vi->ysize + (int)zscaled + ALL_BORDER;
  while (needWinx >  1.05 * maxWinx || needWiny > 1.05 * maxWiny) {
    newzoom = b3dStepPixelZoom(mZoom, -1);
    if (fabs(newzoom - mZoom) < 0.0001)
      break;
    if (vi->pyrCache && vi->pyrCache->zoomRequiresBigLoad
        (newzoom, (int)((1. - mYzFraction) * maxWinx),
         (int)((1. - mXzFraction) * maxWiny)))
      break;
    mZoom = newzoom;
    needWinx = (int)((vi->xsize + zscaled) * mZoom + ALL_BORDER);
    needWiny = (int)((vi->ysize + zscaled) * mZoom + ALL_BORDER);
  }
     
  // Allow input to get the size hints right for toolbars, compute and limit
  // the new window size
  imod_info_input();  
  QSize toolSize = mToolBar->sizeHint();
  //toolHeight = height() - mGLw->height();
  toolHeight = toolSize.height();
  newHeight = needWiny + toolHeight;
  newWidth = B3DMAX(needWinx, toolSize.width());
  diaLimitWindowSize(needWinx, needWiny);

  // Fix the position too then set up the window
  QRect pos = geometry();
  xleft = pos.x();
  ytop = pos.y();
  diaLimitWindowPos(newWidth, newHeight, xleft, ytop);

  resize(newWidth, newHeight);
  move(xleft, ytop);

  show();
  SetCursor(vi->imod->mousemode);
  mGLw->setMouseTracking(pixelViewOpen);
  insertTime.start();
}


void XyzWindow::setFontDependentWidths()
{
  diaSetButtonWidth(mHelpButton, ImodPrefs->getRoundedStyle(), 1.2, "Help");
}

void XyzWindow::help()
{
  imodShowHelpPage("xyz.html#TOP");
}

// Whan a close event comes in, tell control to remove window, clean up
//  and accept
void XyzWindow::closeEvent (QCloseEvent * e )
{
  ivwRemoveControl(mVi, mCtrl);
  imodDialogManager.remove((QWidget *)this);

  /* DNM 11/17/01: stop x and y movies when close window */
  imodMovieXYZT(mVi, 0, 0, MOVIE_DEFAULT, MOVIE_DEFAULT);

  B3DFREE(mFdataxz);
  B3DFREE(mFdatayz);

  b3dFreeCIImage(mXydata);
  b3dFreeCIImage(mXzdata);
  b3dFreeCIImage(mYzdata);
  ImodPrefs->setXyzApplyZscale(mApplyZscale);

  mGLw->mClosing = true;
  e->accept();
}

// Process a right click on the toolbar with a popup menu
void XyzWindow::toolbarMenuEvent(QContextMenuEvent *event)
{
  QSignalMapper mapper(this);
  connect(&mapper, SIGNAL(mapped(int)), this, SLOT(contextMenuHit(int)));
  utilBuildExecPopupMenu(this, &sPopupTable[0], true, &mapper, event);
}

void XyzWindow::contextMenuHit(int index)
{
  Qt::KeyboardModifiers modifiers;
  int key = utilLookupPopupHit(index, &sPopupTable[0], -1, modifiers);
  QKeyEvent *event = new QKeyEvent(QEvent::KeyPress, key, modifiers);
  keyPressEvent(event);
  delete event;
}

void XyzWindow::SetCursor(int mode, bool setAnyway)
{
  if (mMousemode == mode && !setAnyway)
    return;
  if (mode == IMOD_MMODEL)
      mGLw->setCursor(*App->modelCursor);
    else
      mGLw->unsetCursor();
  mMousemode = mode;
}

// Set a slider range and fixed size based on max value
void XyzWindow::setMaxAxis(int which, int max)
{
  mSliders->setRange(which, 0, max - 1);
  int swidth = max < MAX_SLIDER_WIDTH ? max : MAX_SLIDER_WIDTH;
  swidth = swidth > MIN_SLIDER_WIDTH ? swidth : MIN_SLIDER_WIDTH;
  QSlider *slider = mSliders->getSlider(which);
  QSize hint = slider->minimumSizeHint();
  slider->setFixedWidth(swidth + hint.width() + 5);
}

// Allocate a window size between two image dimensions size and zsize
void XyzWindow::allocateDim(int winsize, float zFraction, int &dim1,
                            int &dim2)
{
  dim2 = B3DNINT(zFraction * (winsize - ALL_BORDER));
  dim1 = winsize - ALL_BORDER - dim2;
  if (dim1 < 2) {
    dim2 -= 2 - dim1;
    dim1 = 2;
  }
  else if (dim2 < 2) {
    dim1 -= 2 - dim2;
    dim2 = 2;
  }
  //imodPrintStderr("dim1 %d  dim2 %d\n",dim1, dim2);
}


/* DNM 1/19/02: Add this function to get the CI Images whenever size has
   changed - now that we can do HQ graphics, they need to be the window size.
   SRH 7/13/07: This is also the common place to set the subwindow sizes and 
   origins */
void XyzWindow::GetCIImages()
{
  allocateDim(mWinx, mXzFraction, mWinXdim1, mWinXdim2);
  allocateDim(mWiny, mYzFraction, mWinYdim1, mWinYdim2);
  
  if (mWinXdim2 > mWinYdim2) {
    mWinXdim1 += mWinXdim2 - mWinYdim2;
    mWinXdim2 = mWinYdim2;
  }
  else if (mWinYdim2 > mWinXdim2) {
    mWinYdim1 += mWinYdim2 - mWinXdim2;
    mWinYdim2 = mWinXdim2;
  }
  //mXzFraction = ((float)mWinXdim2) / (mWinXdim1 + mWinXdim2);
  //mYzFraction = ((float)mWinYdim2) / (mWinYdim1 + mWinYdim2);
  
  mXorigin1 = XYZ_BSIZE;
  mXorigin2 = XYZ_BSIZE + XYZ_GSIZE + mWinXdim1;
  mYorigin1 = XYZ_BSIZE;
  mYorigin2 = XYZ_BSIZE + XYZ_GSIZE + mWinYdim1;

  mXydata = (B3dCIImage *)b3dGetNewCIImageSize
    (mXydata, App->depth, mWinXdim1, mWinYdim1);

  mXzdata = (B3dCIImage *)b3dGetNewCIImageSize
    (mXzdata, App->depth,  mWinXdim1, mWinYdim2);

  mYzdata = (B3dCIImage *)b3dGetNewCIImageSize
    (mYzdata, App->depth,  mWinXdim2, mWinYdim1);

  if (!mXydata || !mXzdata || !mYzdata)
    wprint("\aInsufficient memory to run this Xyz window.\n"
            "Try making it smaller or close it.\n");
}

/*
 * Takes a mouse click at x,y and finds which area it is in (the return value)
 * and computes x, y, z image coordinates for that area
 */
int XyzWindow::Getxyz(int x, int y, float &mx, float &my, int &mz)
{
  int cx, cy, cz;
  /* DNM 1/23/02: turn this from float to int to keep calling expressions
     as ints */
  float scale, zscale;
  
  y = mWiny - y;
  
  getLocation(cx, cy, cz);
  mx = cx;
  my = cy;
  mz = cz;

  scale = 1.0 / mZoom;
  zscale = scale / (mApplyZscale ? mVi->imod->zscale : 1.);

  /*imodPrintStderr("mXorigin1 %d, mXorigin2 %d, mYorigin1 %d, mYorigin2 %d\n",
                  mXorigin1, mXorigin2, mYorigin1, mYorigin2);
  imodPrintStderr("mWinXdim1 %d, mWinXdim2 %d, mWinYdim1 %d, mWinYdim2 %d\n",
                  mWinXdim1, mWinXdim2, mWinYdim1, mWinYdim2);
  imodPrintStderr("mXorigin1+mWinXdim1 %d, mXorigin2+mWinXdim2 %d, mYorigin1+mWinYdim1 %d, mYorigin2+mWinYdim2 %d\n",
                  mXorigin1+mWinXdim1, mXorigin2+mWinXdim2,
                  mYorigin1+mYorigin1+mWinYdim1, mYorigin2+mWinYdim2);*/

  /* Click in main image, Z-Section */
  if (mouse_in_box(mXorigin1, mYorigin1, mXorigin1 + mWinXdim1,
                   mYorigin1 + mWinYdim1, x, y)) {
    //imodPrintStderr("found center image: x %d, y %d\n",x, y);
    mx = (x - mXwoffset1) * scale ;
    my = (y - mYwoffset1) * scale;
    //ivwBindMouse(mVi);
    return(Z_SLICE_BOX);
  }

  /* Click in top image, Y-Section */
  if (mouse_in_box(mXorigin1, mYorigin2, mXorigin1 + mWinXdim1,
                   mYorigin2 + mWinYdim2, x, y)) {
    //imodPrintStderr("found top image: x %d, y %d\n",x, y);
    mx = (x - mXwoffset1) * scale;
    mz = (int)((y - mYwoffset2) * zscale);
    //ivwBindMouse(mVi);
    return(Y_SLICE_BOX);
  }

  /* Click in right image X-Section */
  if (mouse_in_box(mXorigin2, mYorigin1, mXorigin2 + mWinXdim2,
                   mYorigin1 + mWinYdim1, x, y)) {
    //imodPrintStderr("found right image: x %d, y %d\n",x, y);
    my = (y - mYwoffset1) * scale;
    mz = (int)((x - mXwoffset2) * zscale);
    return(X_SLICE_BOX);
  }

  /* Z-Section Gadget */
  if (mouse_in_box(mXorigin2 - 1, mYorigin2 - 1,
                   mXorigin2 + mWinXdim2 + 1,
                   mYorigin2 + mWinYdim2 + 1, x, y)) {
    //imodPrintStderr("found z gadget: x %d, y %d\n",x, y);
    mz = (int)(0.5 * ((y - mYwoffset2) + (x - mXwoffset2)) * scale);
    return(Z_GADGET_BOX);
  }
     
  /* X-Section Gadget (top gutter)*/
  if (mouse_in_box(mXorigin1 - 1, mYorigin1 + mWinYdim1,
                   mXorigin1 + mWinXdim1 + 1, mYorigin2, x, y)) {
    //imodPrintStderr("found top gutter: x %d, y %d\n",x, y);
    mx = (x - mXwoffset1) * scale;
    return(X_GADGET_BOX);
  }
     
  /* Y-Section Gadget (right gutter)*/
  if (mouse_in_box(mXorigin1 + mWinXdim1, mYorigin1 -1, mXorigin2,
                   mYorigin1 + mWinYdim1 + 1, x, y)) {
    //imodPrintStderr("found right gutter: x %d, y %d\n",x, y);
    my = (y - mYwoffset1) * scale;
    return(Y_GADGET_BOX);
  }

  // Grab box for readjusting fraction between windows
  if (mouse_in_box(mXorigin1 + mWinXdim1, mYorigin1 + mWinYdim1,
                   mXorigin2, mYorigin2, x, y)) {
    //imodPrintStderr("found fraction grab box: x %d, y %d\n",x, y);
    return(FRACTION_BOX);
  }

  //imodPrintStderr("found nothing: x %d, y %d\n",x, y);
  return(NOT_IN_BOX);
}

// Mouse button 1 click
void XyzWindow::B1Press(int x, int y)
{
  float mx, my;
  int mz;
  ImodView *vi   = mVi;
  Ipoint pnt;
  Iindex index;
  float distance;
  float selsize = IMOD_SELSIZE / mZoom;
  int box = Getxyz(x, y, mx, my, mz);
  Imat *mat = NULL;

  if (box == NOT_IN_BOX)
    return;

  if (box == FRACTION_BOX)
    return;

  if (vi->ax) {
    if (vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      autox_fillmouse(vi, (int)mx, (int)my);
      return;
    }
  }

  /* DNM 1/23/02: Adopt code from Zap window, get nearest point if in the
     main display panel */
  if (vi->imod->mousemode == IMOD_MMODEL && box <= Z_SLICE_BOX) {
    if (box < Z_SLICE_BOX) {
      mat = imodMatNew(3);
      if (mat)
        imodMatRot(mat, 90., box == X_SLICE_BOX ? b3dY : b3dX);
    }
    pnt.x = mx;
    pnt.y = my;
    pnt.z = mz;
    setLocation(mx, my, mz);
    distance = imodAllObjNearest(vi, &index , &pnt, selsize, ivwWindowTime(vi,mTimeLock),
                                 mat);
    B3DFREE(mat);
    if (distance >= 0. || !mLock) {
      vi->xmouse = mx;
      vi->ymouse = my;
      vi->zmouse = mz;
      ivwBindMouse(vi);
      imodDraw(vi, IMOD_DRAW_RETHINK | IMOD_DRAW_XYZ);
    } else
      Draw();
    return;
  }

  setLocation(mx, my, mz);
     
  /* DNM 1/23/02: make it update all windows */
  if (mLock)
    Draw();
  else
    imodDraw(vi, IMOD_DRAW_XYZ);
  return;
}

// Mouse button 2 click
void XyzWindow::B2Press(int x, int y)
{
  float mx, my;
  int mz;
  int plane;
  Iobj  *obj;
  Icont *cont;
  Ipoint point;
  int pt, newSurf, time, isPlanar;
  bool timeMismatch, notInPlane;
  time = ivwWindowTime(mVi, mTimeLock);

  plane = Getxyz(x, y, mx, my, mz);
  if (plane == NOT_IN_BOX)
    return;

  if (mVi->ax) {
    if (mVi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      autox_sethigh(mVi, (int)mx, (int)my);
      return;
    }
  }

  /* DNM 12/18/93: do not start movie in slider areas */
  if (mVi->imod->mousemode == IMOD_MMOVIE) {
    switch(plane) {
    case X_SLICE_BOX:
      imodMovieXYZT(mVi, 1, MOVIE_DEFAULT, MOVIE_DEFAULT, MOVIE_DEFAULT);
      break;
    case Y_SLICE_BOX:
      imodMovieXYZT(mVi, MOVIE_DEFAULT, 1, MOVIE_DEFAULT, MOVIE_DEFAULT);
      break;
    case Z_SLICE_BOX:
      imodMovieXYZT(mVi, MOVIE_DEFAULT, MOVIE_DEFAULT, 1, MOVIE_DEFAULT);
      break;
    default:
      break;
    }
    if (plane <= Z_SLICE_BOX)
      imcSetStarterID(mCtrl);
    return;
  }

  if (plane > Z_SLICE_BOX)
    return;

  obj = imodObjectGet(mVi->imod);
  if (!obj)
    return;

  // DNM 7/10/04: switch to calling function for this
  // 11/17/04: have it take care of modifying time of empty contour
  cont = ivwGetOrMakeContour(mVi, obj, 0);
  if (!cont)
    return;

  if (cont->psize > 0) {
    timeMismatch = ivwTimeMismatch(mVi, mTimeLock, obj, cont);
    notInPlane = iobjPlanar(obj->flags) && newPointOutOfPlane(cont, plane, mx, my, mz);
    if (notInPlane || timeMismatch) {
      newSurf = INCOS_NEW_CONT;
      if (ImodPrefs->slicerNewSurf())
        newSurf = imodCheckSurfForNewCont(obj, cont, time, plane);
      cont = utilAutoNewContour(mVi, cont, notInPlane, timeMismatch, mTimeLock, newSurf,
                                "planes", "plane");
      if (!cont)
        return;
    }
  } else if (iobjPlanar(obj->flags) && ImodPrefs->slicerNewSurf()) {
    
    // For new or empty contour, see if the current surface works or if a surface change 
    // is needed.  The current surface is acceptable as is if it does not have nonplanar
    // contours and is not X or Y plane with surface 0; otherwise do more involved check
    isPlanar = imodSurfaceIsPlanar(obj, cont->surf, time, plane);
    if (isPlanar == 0 || (isPlanar < 0 && plane != Z_SLICE_BOX && !cont->surf)) {
      newSurf = imodCheckSurfForNewCont(obj, cont, time, plane);

      // If this said a new surface is required, get one; in any case assign surf to cont
      utilAssignSurfToCont(mVi, obj, cont, newSurf);
    }
  }

  /* Now if times still don't match refuse the point */
  if (ivwTimeMismatch(mVi, mTimeLock, obj, cont)) {
    wprint("\aContour time does not match current time.\n"
           "Set contour time to 0 to model across times.\n");
    mVi->undo->finishUnit();
    return;
  }

  point.x = mx;
  point.y = my;
  point.z = mz;
  pt = mVi->imod->cindex.point + 1;

  ivwRegisterInsertPoint(mVi, cont, &point, pt);

  /* 11/71/04: deleted test to maintain wild flag, insert takes care of it */
  finishNewModelPoint(mx, my, mz);
}

// Mouse button 3 click
void XyzWindow::B3Press(int x, int y)
{
  float mx, my;
  int mz;
  int plane;
  Icont *cont;
  Iobj *obj;
  int pt;

  plane = Getxyz(x, y, mx, my, mz);
  if (plane == NOT_IN_BOX)
    return;

  if (mVi->ax) {
    if (mVi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      autox_setlow(mVi, (int)mx, (int)my);
      return;
    }
  }
     
  if (mVi->imod->mousemode == IMOD_MMOVIE) {
    switch(plane) {
    case X_SLICE_BOX:
      imodMovieXYZT(mVi, -1, MOVIE_DEFAULT, MOVIE_DEFAULT, MOVIE_DEFAULT);
      break;
    case Y_SLICE_BOX:
      imodMovieXYZT(mVi, MOVIE_DEFAULT, -1, MOVIE_DEFAULT, MOVIE_DEFAULT);
      break;
    case Z_SLICE_BOX:
      imodMovieXYZT(mVi, MOVIE_DEFAULT, MOVIE_DEFAULT, -1, MOVIE_DEFAULT);
      break;
    default:
      break;
    }
    if (plane <= Z_SLICE_BOX)
      imcSetStarterID(mCtrl);
    return;
  }

  if (plane > Z_SLICE_BOX)
    return;
  
  obj = imodObjectGet(mVi->imod);
  cont = imodContourGet(mVi->imod);
  pt   = mVi->imod->cindex.point;
  if (!cont || !obj)
    return;
  if (pt < 0)
    return;
  if ((plane == Z_SLICE_BOX && !ivwPointVisible(mVi, &(cont->pts[pt]))) ||
      (plane == X_SLICE_BOX && B3DNINT(mVi->xmouse) != B3DNINT(cont->pts[pt].x)) ||
      (plane == Y_SLICE_BOX && B3DNINT(mVi->ymouse) != B3DNINT(cont->pts[pt].y)))
    return;

  if (ivwTimeMismatch(mVi, mTimeLock, obj, cont))
    return;

  mVi->undo->pointShift();
  cont->pts[pt].x = mx;
  cont->pts[pt].y = my;
  cont->pts[pt].z = mz;
  mVi->undo->finishUnit();

  finishNewModelPoint(mx, my, mz);
}

// Mouse button 1 drag for panning
void XyzWindow::B1Drag(int x, int y)
{
  int sx, sy, curx, cury, curz;
  int nx, ny, delx, dely, sizeSum;
  int bx2 = mXwoffset2;
  int by2 = mYwoffset2;
  double transFac = mZoom < 1. ? 1. / mZoom : 1.;
  float scale, delz, xfrac, yfrac, delfrac;
  ImodView *vi = mVi;
  int newVal;
  
  //do not drag out of a gutter
  if (mWhichbox == Y_GADGET_BOX) {
    int yinverted = mWiny - y;
    if (yinverted < mYorigin1 + 1) {
      y = mWiny - (mYorigin1 + 1);
    }
    else if (yinverted > mYorigin1 + mWinYdim1) {
      y = mWiny - (mYorigin1 + mWinYdim1);
    }
  }
  else if (mWhichbox == X_GADGET_BOX) {
    if (x < mXorigin1) {
      x = mXorigin1;
    }
    else if (x > mXorigin1 + mWinXdim1 - 1) {
      x = mXorigin1 + mWinXdim1 - 1;
    }
  }
  else if (mWhichbox == Z_GADGET_BOX) {
    if (x < mXorigin2) {
      x = mXorigin2;
    }
    else if (x > mXorigin2 + mWinXdim2 - 1) {
      x = mXorigin2 + mWinXdim2 - 1;
    }
    int yinverted = mWiny - y;
    if (yinverted < mYorigin2 + 1) {
      y = mWiny - (mYorigin2 + 1);
    }
    else if (yinverted > mYorigin2 + mWinYdim2) {
      y = mWiny - (mYorigin2 + mWinYdim2);
    }
  }
  
  sy = mWiny - y - 1;
  sx = x - mXwoffset1;
  sy -= mYwoffset1;

  nx = (int)(vi->xsize * mZoom);
  ny = (int)(vi->ysize * mZoom);

  scale = 1.0/mZoom;
  delx = B3DNINT(transFac * (x - mLmx));
  dely = B3DNINT(transFac * (y - mLmy));
  getLocation(curx, cury, curz);

  switch (mWhichbox) {
  case NOT_IN_BOX:
    return;
  case X_SLICE_BOX:
    mXtrans2 += delx;
    mYtrans1 -= dely;
    mYtrans2 += delx;
    Draw();
    return;
  case Y_SLICE_BOX:
    mXtrans1 += delx;
    mYtrans2 -= dely;
    mXtrans2 -= dely;
    Draw();
    return;
  case Z_SLICE_BOX:
    mXtrans1 += delx;
    mYtrans1 -= dely;
    Draw();
    return;

  case Z_GADGET_BOX:
    newVal = (int)(0.5 * ((mWiny - y - 1 - by2) + (x - bx2)) * scale);
    if (curz == newVal)
      return;
    setLocation(NOTNEW, NOTNEW, newVal);
    break;

  case X_GADGET_BOX:
    newVal = (int)(sx * scale);
    if (curx == newVal)
      return;
    setLocation(newVal, NOTNEW, NOTNEW);
    break;

  case Y_GADGET_BOX:
    newVal = (int)((sy) * scale);
    if (cury == newVal)
      return;
    setLocation(NOTNEW, newVal, NOTNEW);
    break;

  case FRACTION_BOX:

    // Get the average delta Z and the existing window fractions
    delz = -0.5 * (x - mLmx + mLmy - y);
    //imodPrintStderr("lmx %d lmy %d x %d y %d\n", mLmx, mLmy, x, y);
    xfrac = ((float)mWinXdim2) / (mWinXdim1 + mWinXdim2);
    yfrac = ((float)mWinYdim2) / (mWinYdim1 + mWinYdim2);

    // Use the window fraction that is bigger relative to the requested 
    // fractions since that is the unstretched dimension, and get a change
    // in fraction caused by that delta Z in the window
    if (yfrac / mYzFraction > xfrac / mXzFraction)
      delfrac = ((mWinYdim2 + delz) / (mWinYdim1 + mWinYdim2)) / 
        yfrac;
    else
      delfrac = ((mWinXdim2 + delz) / (mWinXdim1 + mWinXdim2)) /
        xfrac;
    
    // Apply that delta to each of the requested fractions and limit
    mXzFraction *= delfrac;
    mYzFraction *= delfrac;
    /*imodPrintStderr("delfrac %f xzf %f yzf %f\n", delfrac, mXzFraction,
      mYzFraction);*/
    sizeSum = mVi->xsize + mVi->zsize;
    mXzFraction = B3DMAX(2. / sizeSum, B3DMIN((sizeSum - 2.) / sizeSum,
                                                 mXzFraction));
    sizeSum = mVi->ysize + mVi->zsize;
    mYzFraction = B3DMAX(2. / sizeSum, B3DMIN((sizeSum - 2.) / sizeSum,
                                                 mYzFraction));

    // Get new images (computes new sizes with these fractions) and draw
    GetCIImages();
    Draw();
    return;
  }
  if (mLock)
    Draw();
  else
    imodDraw(mVi, IMOD_DRAW_XYZ);
}

// Mouse button 2 drag, continuous insert
void XyzWindow::B2Drag(int x, int y)
{
  float mx, my;
  int mz, box;
  Iobj  *obj;
  Icont *cont;
  Ipoint point, scale = {1., 1., 1.};
  double dist;
  int pt;
  scale.z = mApplyZscale ? mVi->imod->zscale : 1.;

  if (mVi->ax) {
    if (mVi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      box = Getxyz(x, y, mx, my, mz);
      if (box != 3)
        return;
      autox_sethigh(mVi, (int)mx, (int)my);
      return;
    }
  }

  if (mVi->imod->mousemode != IMOD_MMODEL)
    return;

  box = Getxyz(x, y, mx, my, mz);
  if (!box || box > Z_SLICE_BOX)
    return;

  obj = imodObjectGet(mVi->imod);
  if (!obj)
    return;
  cont = imodContourGet(mVi->imod);
  if (!cont)
    return;
  pt = mVi->imod->cindex.point;
  if (pt < 0)
    return;

  if (ivwTimeMismatch(mVi, mTimeLock, obj, cont))
    return;

  /* DNM: don't make closed contours wild if they're not */
  if (iobjPlanar(obj->flags) && newPointOutOfPlane(cont, box, mx, my, mz))
    return;

  if (obj->extra[IOBJ_EX_PNT_LIMIT] &&
      cont->psize >= obj->extra[IOBJ_EX_PNT_LIMIT]) {
    B2Press(x, y);
    return;
  }
    
  point.x = mx;
  point.y = my;
  point.z = mz;

  dist = imodPoint3DScaleDistance(&point, &(cont->pts[pt]), &scale);
  if (dist < scaleModelRes(mVi->imod->res, mZoom))
    return;

  ivwRegisterInsertPoint(mVi, cont, &point, pt + 1);
  finishNewModelPoint(mx, my, mz);
}

// Mouse button 3 drag, continuous modification
void XyzWindow::B3Drag(int x, int y)
{
  float mx, my;
  int mz, box;
  Iobj  *obj;
  Icont *cont;
  Ipoint point, scale = {1., 1., 1.};
  double dist;
  int pt;
  scale.z = mApplyZscale ? mVi->imod->zscale : 1.;

  if (mVi->ax) {
    if (mVi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      box = Getxyz(x, y, mx, my, mz);
      if (box != 3)
        return;
      autox_setlow(mVi, (int)mx, (int)my);
      return;
    }
  }

  if (mVi->imod->mousemode != IMOD_MMODEL)
    return;

  box = Getxyz(x, y, mx, my, mz);
  if (!box || box > Z_SLICE_BOX)
    return;

  obj = imodObjectGet(mVi->imod);
  if (!obj)
    return;
  cont = imodContourGet(mVi->imod);
  if (!cont)
    return;
  pt = mVi->imod->cindex.point;
  if (pt < 0)
    return;

  if (ivwTimeMismatch(mVi, mTimeLock, obj, cont))
    return;

  point.x = mx;
  point.y = my;
  point.z = mz;

  dist = imodPoint3DScaleDistance(&point, &(cont->pts[pt]), &scale);
  if (dist < scaleModelRes(mVi->imod->res, mZoom))
    return;

  pt++;
  if (pt >= cont->psize)
    pt = cont->psize - 1;

  mVi->imod->cindex.point = pt;
  mVi->undo->pointShift();
  cont->pts[pt].x = mx;
  cont->pts[pt].y = my;
  cont->pts[pt].z = mz;
  mVi->undo->finishUnit();

  finishNewModelPoint(mx, my, mz);
}

void XyzWindow::finishNewModelPoint(int mx, int my, int mz)
{
  setLocation(mx, my, mz);
  mVi->xmouse  = mx;
  mVi->ymouse  = my;
  mVi->zmouse  = mz;
  ivwBindMouse(mVi);

  /* DNM 1/23/02: make it update all windows */
  imodDraw(mVi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
}

/*
 * Determine if a new point would render a contour nonplanar
 */
bool XyzWindow::newPointOutOfPlane(Icont *cont, int plane, int mx, int my, int mz)
{
  int firstX, firstY, pt;
  bool planarX, planarY, planarZ, onePoint = cont->psize == 1;
  if (!cont->psize)
    return false;
  planarX = planarY = !onePoint;
  planarZ = !(cont->flags & ICONT_WILD) && !onePoint;

  firstX = B3DNINT(cont->pts[0].x);
  for (pt = 1; pt < cont->psize; pt++) {
    if (B3DNINT(cont->pts[pt].x) != firstX) {
      planarX = false;
    }
  }
  firstY = B3DNINT(cont->pts[0].y);
  for (pt = 1; pt < cont->psize; pt++) {
    if (B3DNINT(cont->pts[pt].y) != firstY) {
      planarY = false;
    }
  }
  // imodPrintStderr("planar X %d Y %d Z %d\n", planarX ?1:0, planarY ?1:0, planarZ ?1:0);
  if (!(planarX || planarY || planarZ))
    return false;

  return ((plane == X_SLICE_BOX && (((planarY || planarZ) && !planarX) || 
                                    ((planarX || onePoint) && firstX != mx))) ||
          (plane == Y_SLICE_BOX && (((planarX || planarZ) && !planarY) || 
                                    ((planarY || onePoint) && firstY != my))) ||
          (plane == Z_SLICE_BOX && (((planarX || planarY) && !planarZ) || 
                                    ((planarZ || onePoint) &&
                                     B3DNINT(cont->pts[0].z) != mz))));
}

/*
 * Functions for toolbar elements
 */
void XyzWindow::zoomUp()
{
  stepZoom(1);
}

void XyzWindow::zoomDown()
{
  stepZoom(-1);
}

void XyzWindow::stepZoom(int step)
{
  setControlAndLimits();
  mZoom = b3dStepPixelZoom(mZoom, step);
  Draw();
}

// Set the control priority
void XyzWindow::setControlAndLimits()
{
  ivwControlPriority(mVi, mCtrl);
}

// A new zoom or section was entered
void XyzWindow::newZoom()
{
  QString str = mZoomEdit->text();
  enteredZoom(atof(LATIN1(str)));
}

void XyzWindow::enteredZoom(float newZoom)
{
  setControlAndLimits();
  mZoom = newZoom;
  if (mZoom <= 0.01)
    mZoom = 0.01;
  Draw();
  setFocus();
}

void XyzWindow::setZoomText(float zoom)
{
  QString str;
  str.sprintf("%.4f", zoom);
  if (str.endsWith("00"))
    str.truncate(str.length() - 2);
  mZoomEdit->setText(str);
}

void XyzWindow::sliderChanged(int which, int value, bool dragging)
{
  if (!dragging || ImodPrefs->hotSliderActive(mCtrlPressed))
    enteredAxisLocation(which, value);
  else
    mDisplayedAxisLocation[which] = value;
}

void XyzWindow::setSlider(int which, int section)
{
  if (mDisplayedAxisLocation[which] == section)
    return;
  diaSetSlider(mSliders->getSlider(which), section);
  mDisplayedAxisLocation[which] = section;
}

void XyzWindow::enteredAxisLocation(int which, int value)
{
  setControlAndLimits();
  switch (which) {
  case X_COORD:
    setLocation(value, NOTNEW, NOTNEW);
    break;
  case Y_COORD:
    setLocation(NOTNEW, value, NOTNEW);
    break;
  case Z_COORD:
    setLocation(NOTNEW, NOTNEW, value);
    break;
  }
  if (mLock)
    Draw();
  else
    imodDraw(mVi, IMOD_DRAW_XYZ);
  setFocus();
}


/*
 * Draw all the image panels
 */
void XyzWindow::DrawImage()
{
  int x, y, z, i, time;
  int nx = mVi->xsize;
  int ny = mVi->ysize;
  int nz = mVi->zsize;
  int nzFdataYZ = nz;
  int nzFdataXZ = nz;
  unsigned char **zImage;
  unsigned char *fdata;
  int cyi, cacheInd = -1;
  int cx, cy, cz, cxz, zinv;
  int imdataxsize;
  unsigned char **imdata;
  b3dUInt16 **usim;
  b3dUInt16 *usdata;
  int wx1, wx2, wy1, wy2;
  int xoffset1, xoffset2, yoffset1, yoffset2;
  int width1, height1, width2, height2, cacheSum, xslice, yslice;
  int tileScale, tileZscale, status, zStartYZ, zEndYZ, zStartXZ, zEndXZ;
  int pyrXstart, pyrYstart, curXscaled, curYscaled;
  float pyrXoffset = 0., pyrYoffset = 0., zOffsetYZ = 0., zOffsetXZ = 0., tmpx, tmpy;
  std::vector<FastSegment> segments;
  std::vector<int> startInds;
  bool flipped, needNewXZ, needNewYZ;
  float zscale = mApplyZscale ? mVi->imod->zscale : 1.;
  double zoomZ = mZoom * zscale;
  double xyZoom = mZoom;
  int pixSize = ivwGetPixelBytes(mVi->rawImageStore);

  if (!mExposed)
    return;     /* DNM: avoid crashes if Zap is movieing*/

  /* DNM 3/5/01: changed to avoid very slow ivwGetValue when data are in
     cache; set up image pointer tables */
  // Keep track of a sum of Z values in the cache in order to detect 
  // Changes in available data that will require redisplay of XZ and YZ data
  // Load current Z section first to get it into cacheSum
  getLocation(cx, cy, cz);

  /* Get the time to display, flush images if it doesn't match last drawn */
  time = ivwWindowTime(mVi, mTimeLock);
  if (time != mTimeDrawn) {
    b3dFlushImage(mXydata);
    b3dFlushImage(mYzdata);
    b3dFlushImage(mXzdata);
  }
  mTimeDrawn = time;

  if (!mVi->pyrCache) {
    zImage = ivwGetZSectionTime(mVi, cz, time);
    if (ivwSetupFastAccess(mVi, &imdata, 0, &cacheSum, time))
      return;

    /* Just take the X size, do not allow for possibility of cached data 
       having different X sizes */
    imdataxsize = mVi->xsize;
    usim = (b3dUInt16 **)imdata;
  }

  // HUH?  THIS GIVES UP COMPLETELY ON TRACKING THE CACHE SUM
  /* if (mVi->vmSize) {
    mLx = mLy = -1;
    }*/
          
  utilClearWindow(App->background);

  cyi = cy * nx;
  flipped = !mVi->fakeImage && (!mVi->vmSize || mVi->fullCacheFlipped) && 
    mVi->li->axis == 2;

  /* Now compute drawing parameters for each of the subareas */
  /* Pass the image offset routine the effective image size, including
     de-zoomed borders, and convert a data offset into a negative window
     offset for use in drawing model */
  b3dSetImageOffset(mWinXdim1, nx, mZoom, width1, mXtrans1, wx1, xoffset1, 0);
  b3dSetImageOffset(mWinXdim2, nz, zoomZ, width2, mXtrans2, wx2, xoffset2, 0);
  b3dSetImageOffset(mWinYdim1, ny, mZoom, height1, mYtrans1, wy1, yoffset1, 0);
  b3dSetImageOffset(mWinYdim2, nz, zoomZ, height2, mYtrans2, wy2, yoffset2, 0);

  if (mVi->pyrCache) {

    // Get the Z subarea image: those sizes and offsets should be the same but...
    zImage = mVi->pyrCache->getSectionArea(cz, xoffset1, yoffset1, width1, height1, mZoom,
                                           false, nx, ny, pyrXoffset, pyrYoffset,
                                           tileScale, status);
    if (!zImage || status < 0)
      return;

    // Flush image if offset/size changed
    if (mLastXoffset1 != xoffset1 || mLastYoffset1 != yoffset1 || width1 != mLastWidth1 ||
        height1 != mLastHeight1)
      b3dFlushImage(mXydata);

    // get a cache sum and determine if new YZ or XZ images are needed
    cacheInd = mVi->pyrCache->getBufCacheInd();
    cacheSum = mVi->pyrCache->loadedCacheSum(cacheInd);
    needNewYZ = cx != mLx || cacheInd != mLastTileCacheInd || mLastXoffset2 != xoffset2 ||
      mLastYoffset1 != yoffset1 || 
      width2 != mLastWidth2 || height1 != mLastHeight1 || cacheSum != mLastCacheSum;
    needNewXZ = cy != mLy || cacheInd != mLastTileCacheInd || mLastXoffset1 != xoffset1 ||
      mLastYoffset2 != yoffset2 || 
      width1 != mLastWidth1 || height2 != mLastHeight2 || cacheSum != mLastCacheSum;

    // Get loaded parameters in Z for this cache and Z ranges on top and side
    mVi->pyrCache->scaledRangeInZ(cacheInd, xoffset2, xoffset2 + width2 - 1, zStartYZ,
                                  zEndYZ, tileZscale, zOffsetYZ);
    mVi->pyrCache->scaledRangeInZ(cacheInd, yoffset2, yoffset2 + height2 - 1, zStartXZ,
                                  zEndXZ, tileZscale, zOffsetXZ);

    // Get the X and Y starts and the current x, y in loaded cache cooordinates
    mVi->pyrCache->scaledAreaSize(cacheInd, xoffset1, yoffset1, width1, height1, 
                                  pyrXstart, pyrYstart, x, y, tmpx, tmpy, true);
    mVi->pyrCache->scaledAreaSize(cacheInd, cx, cy, 1, 1, curXscaled, curYscaled, x, y, 
                                  tmpx, tmpy, false);

    // Keep track of this draw in the Last variables
    mLastXoffset1 = xoffset1;
    mLastYoffset1 = yoffset1;
    mLastWidth1 = width1;
    mLastHeight1 = height1;
    mLastXoffset2 = xoffset2;
    mLastYoffset2 = yoffset2;
    mLastWidth2 = width2;
    mLastHeight2 = height2;
    mLastCacheSum = cacheSum;
    mLastTileCacheInd = cacheInd;

    // Adjust most parameters for the drawing calls
    width1 = nx;
    height1 = ny;
    width2 = zEndYZ + 1 - zStartYZ;
    height2 = zEndXZ + 1 - zStartXZ;
    nzFdataYZ = width2;
    nzFdataXZ = height2;
    xyZoom *= tileScale;
    zoomZ *= tileZscale;

    // If a plane needs to be filled, flush it here to be sure and make sure it is
    // big enough
    if (needNewXZ) {
      b3dFlushImage(mXzdata);
      if (mFdataXZsize < height2 * width1) {
        B3DFREE(mFdataxz);
        mFdataxz = (unsigned char *)malloc(height2 * width1 * pixSize);
        if (!mFdataxz) {
          mFdataXZsize = 0;
          return;
        }
        mFdataXZsize = height2 * width1;
      }
    }
    if (needNewYZ) {
      b3dFlushImage(mYzdata);
      if (mFdataYZsize < height1 * width2) {
        B3DFREE(mFdatayz);
        mFdatayz = (unsigned char *)malloc(height1 * width2 * pixSize);
        if (!mFdatayz) {
          mFdataYZsize = 0;
          return;
        }
        mFdataYZsize = height1 * width2;
      }
    }
  }

  // Set all the parameters for drawing position and offsets for getting between window
  // and image coordinates
  wx1 += mXorigin1;
  wx2 += mXorigin2;
  wy1 += mYorigin1;
  wy2 += mYorigin2;
  
  if (xoffset1 + pyrXoffset)
    mXwoffset1 = -B3DNINT((xoffset1 + pyrXoffset) * mZoom + 0.5) + mXorigin1;
  else
    mXwoffset1 = wx1;
  if (xoffset2 + zOffsetYZ)
    mXwoffset2 = -B3DNINT((xoffset2 + zOffsetYZ) * zoomZ + 0.5) + mXorigin2;
  else
    mXwoffset2 = wx2;
  if (yoffset1 + pyrYoffset)
    mYwoffset1 = -B3DNINT((yoffset1 + pyrYoffset) * mZoom + 0.5) + mYorigin1;
  else
    mYwoffset1 = wy1;
  if (yoffset2 + zOffsetXZ)
    mYwoffset2 = -B3DNINT((yoffset2 + zOffsetXZ) * zoomZ + 0.5) + mYorigin2;
  else
    mYwoffset2 = wy2;
  
  // Then zero out the starting offsets for drawing tile images
  if (cacheInd >= 0) {
    xoffset1 = 0;
    yoffset1 = 0;
    xoffset2 = 0;
    yoffset2 = 0;
  }

  //draw XY view
  if (width1 > 0 && height1 > 0) {
    mLz = cz;
    b3dDrawGreyScalePixelsHQ(zImage, nx,ny, xoffset1, yoffset1, wx1, wy1, width1, height1,
                             mXydata, mVi->rampbase, xyZoom, xyZoom, mHq, cz, App->rgba);
  }

  // Load data for YZ view
  // Send out a negative xslice or yslice if the data are being reloaded,
  // this is the best way to signal that they are new to the matching routine
  if (width2 > 0 && height1 > 0) {
    xslice = cx;
    fdata  = mFdatayz;
    usdata = (b3dUInt16 *)fdata;
    if (cacheInd >= 0 && needNewYZ) {
      if (!mVi->pyrCache->fastPlaneAccess(cacheInd, curXscaled, b3dX, segments,
                                          startInds)) {
        imodTrace('t', "curx %d segments %d  inds %d", curXscaled, segments.size(),
                        startInds.size());
        fillArrayFromTiles(fdata, segments, startInds, mVi->ushortStore, true, 
                           pyrYstart, pyrYstart + height1, zStartYZ, zEndYZ + 1);
      }
    } else if (cacheInd < 0 && (cx != mLx || cacheSum != mLastCacheSum)) {
      xslice = -1 - cx;
      mLx = cx;
      if (flipped && !mVi->fakeImage) {
        cxz = cx + (mVi->zsize - 1) * imdataxsize;
        for (y = 0; y < ny; y++) {
          if (imdata[y]) {
            if (mVi->rgbStore) {
              for (z = 0; z < nz; z++) {
                fdata[3 * (z + y * nz)] = imdata[y][3 * (cxz - z * imdataxsize)];
                fdata[3 * (z + y * nz) + 1] = imdata[y][3 * (cxz - z * imdataxsize) + 1];
                fdata[3 * (z + y * nz) + 2] = imdata[y][3 * (cxz - z * imdataxsize) + 2];
              }
            } else if (mVi->ushortStore) {
              for (z = 0; z < nz; z++)
                usdata[z + y * nz] = usim[y][cxz - (z * imdataxsize)];
            } else
              for (z = 0; z < nz; z++) 
                fdata[z + y * nz] = imdata[y][cxz - (z * imdataxsize)];
          } else {
            if (mVi->rgbStore) {
              for (z = 0; z < nz; z++) {
                fdata[3 * (z + y * nz)] = 0;
                fdata[3 * (z + y * nz) + 1] = 0;
                fdata[3 * (z + y * nz) + 2] = 0;
              }
            } else if (mVi->ushortStore) {
              for (z = 0; z < nz; z++) 
                usdata[z + y * nz] = 0;
            } else
              for (z = 0; z < nz; z++) 
                fdata[z + y * nz] = 0;
          }
        }
      } else {
        for(z = 0; z < nz; z++) {
          if (!mVi->fakeImage && imdata[z]) {
            if (mVi->rgbStore) {
              for (i = z, y = 0; y < ny; y++, i += nz) {
                fdata[3 * i] = imdata[z][3 *(cx + (y * imdataxsize))];
                fdata[3 * i + 1] = imdata[z][3 *(cx + (y * imdataxsize))+ 1];
                fdata[3 * i + 2] = imdata[z][3 *(cx + (y * imdataxsize))+ 2];
              }
            } else if (mVi->ushortStore) {
              for (i = z, y = 0; y < ny; y++, i += nz)
                usdata[i] = usim[z][cx + (y * imdataxsize)];
            } else
              for (i = z, y = 0; y < ny; y++, i += nz)
                fdata[i] = imdata[z][cx + (y * imdataxsize)];
          } else {
            if (mVi->rgbStore)
              for (i= z, y = 0; y < ny; y++, i += nz)
                fdata[3 * i] = fdata[3 * i + 1] = fdata[3 * i + 2] = 0;
            else if (mVi->ushortStore)
              for (i= z, y = 0; y < ny; y++, i += nz)
                usdata[i] = 0;
            else
              for (i= z, y = 0; y < ny; y++, i += nz)
                fdata[i] = 0;
          }
        }
      }
    }
    
    //draw yz view
    b3dDrawGreyScalePixelsHQ(ivwMakeLinePointers(mVi, mFdatayz, nzFdataYZ, ny,
                                                 mVi->rawImageStore), 
                             nzFdataYZ, ny, xoffset2, yoffset1, wx2, wy1, width2, height1,
                             mYzdata, mVi->rampbase, zoomZ, xyZoom, mHq, xslice, 
                             App->rgba);
  }

  // Load data for XZ view
  if (width1 > 0 && height2 > 0) {
    yslice = cy;
    fdata  = mFdataxz;
    usdata = (b3dUInt16 *)fdata;
    if (cacheInd >= 0 && needNewXZ) {
      if (!mVi->pyrCache->fastPlaneAccess(cacheInd, curYscaled, b3dY, segments,
                                          startInds)) {
        imodTrace('t', "cury %d segments %d  inds %d", curYscaled, segments.size(),
                        startInds.size());
        fillArrayFromTiles(fdata, segments, startInds, mVi->ushortStore, false, 
                           pyrXstart, pyrXstart + width1, zStartXZ, zEndXZ + 1);
      }
    } else if (cacheInd < 0 && (cy != mLy || cacheSum != mLastCacheSum)) {
      yslice = -1 - cy;
      mLy = cy;
      for (i = 0, z = 0; z < nz; z++) {
        if (flipped && !mVi->fakeImage && imdata[cy]) {
          zinv = mVi->zsize - 1 - z; 
          if (mVi->rgbStore) {
            for (x = 0; x < nx; x++, i++) {
              fdata[3 * i] = imdata[cy][3 * (x + (zinv * imdataxsize))];
              fdata[3 * i + 1] = imdata[cy][3 * (x + (zinv * imdataxsize)) + 1];
              fdata[3 * i + 2] = imdata[cy][3 * (x + (zinv * imdataxsize)) + 2];
            }
          } else if (mVi->ushortStore)
            for(x = 0; x < nx; x++, i++)
              usdata[i] = usim[cy][x + (zinv * imdataxsize)];
          else
            for(x = 0; x < nx; x++, i++)
              fdata[i] = imdata[cy][x + (zinv * imdataxsize)];
        } else if (!flipped && !mVi->fakeImage && imdata[z]) {
          if (mVi->rgbStore) {
            for(x = 0; x < nx; x++, i++) {
              fdata[3 * i] = imdata[z][3 * (x + (cy * imdataxsize))];
              fdata[3 * i + 1] = imdata[z][3 * (x + (cy * imdataxsize)) + 1];
              fdata[3 * i + 2] = imdata[z][3 * (x + (cy * imdataxsize)) + 2];
            }
          } else if (mVi->ushortStore)
            for(x = 0; x < nx; x++, i++)
              usdata[i] = usim[z][x + (cy * imdataxsize)];
          else
            for(x = 0; x < nx; x++, i++)
              fdata[i] = imdata[z][x + (cy * imdataxsize)];
        } else {
          if (mVi->rgbStore)
            for(x = 0; x < nx; x++, i++)
              fdata[3 * i] = fdata[3 * i + 1] = fdata[3 * i + 2] = 0;
          else if (mVi->ushortStore)
            for(x = 0; x < nx; x++, i++)
              usdata[i] = 0;
          else
            for(x = 0; x < nx; x++, i++)
              fdata[i] = 0;
        }
      }
    }

    //draw xz view    
    b3dDrawGreyScalePixelsHQ(ivwMakeLinePointers(mVi, mFdataxz, nx, nzFdataXZ,
                                                 mVi->rawImageStore),
                             nx, nzFdataXZ, xoffset1, yoffset2,
                             wx1, wy2, width1, height2, mXzdata,
                             mVi->rampbase, xyZoom, zoomZ, 
                             mHq, yslice, App->rgba);
  }
  mLastCacheSum = cacheSum;
}

/*
 * Fill one of the XZ or YZ arrays from the list of segments in tiles.
 * This started out very similar to what is in mv_image, including variable names,
 * but then it got messy because of the striding requirements, etc.
 */
void XyzWindow::fillArrayFromTiles(unsigned char *fdataIn,
                                   std::vector<FastSegment> &segments, 
                                   std::vector<int> &startInds, int ushort, bool doingYZ,
                                   int istart, int iend, int jstart, int jend)
{
  int j, i, iseg, segEnd, nextToFill, segStart, npix, fstride = 1;
  FastSegment *segp;
  unsigned char *imdata;
  unsigned short *usimdata; 
  unsigned char *fdata = fdataIn;
  unsigned short *usfdata = (unsigned short *)fdata;
  int pixSize = ushort ? 2 : 1;

  if (doingYZ) {
    fstride = jend - jstart;
    memset(fdata, 0, pixSize * fstride * (iend - istart));
  }

  for (j = jstart; j < jend; j++) {
    if (doingYZ) {
      fdata = fdataIn + pixSize * (j - jstart);
      usfdata = (unsigned short *)fdata;
    }
    nextToFill = istart;
      
    for (iseg = startInds[j]; iseg < startInds[j + 1]; iseg++) {
      segp = &(segments[iseg]);

      // If segment starts past the end, done with segments
      if (segp->XorY >= iend)
        break;

      // If segment ends before the start, skip it
      if (segp->XorY + segp->length <= istart)
        continue;

      // If segment starts past next place to fill, need to fill with 0's
      if (segp->XorY > nextToFill) {
        npix = segp->XorY - nextToFill;
        if (ushort) {
          if (!doingYZ)
            memset(usfdata, 0, 2 * npix);
          usfdata += npix * fstride;
        } else {
          if (!doingYZ)
            memset(fdata, 0, npix);
          fdata += npix * fstride;
        }
      }

      // Fill with segment data
      segEnd = B3DMIN(segp->XorY + segp->length, iend);
      segStart = B3DMAX(segp->XorY, istart);
      imdata = segp->line + segp->stride * (segStart - segp->XorY);
      npix = segEnd - segStart;
      if (ushort) {
        usimdata = (unsigned short *)imdata;
        if (segp->stride == 1) {
          memcpy(usfdata, usimdata, 2 * npix);
          usfdata += npix;
        } else {
          for (i = segStart; i < segEnd; i++) {
            *usfdata = *usimdata;
            usfdata += fstride;
            usimdata += segp->stride;
          }
        }
      } else {
        if (segp->stride == 1) {
          memcpy(fdata, imdata, npix);
          fdata += npix;
        } else {
          for (i = segStart; i < segEnd; i++) {
            *fdata = *imdata;
            fdata += fstride;
            imdata += segp->stride;
          }
        }
      }
      nextToFill = segEnd;
    }

    // Fill past end
    npix = iend - nextToFill;
    if (npix > 0) {
      if (ushort) {
        if (!doingYZ)
          memset(usfdata, 0, 2 * npix);
        usfdata += npix * fstride;
      } else {
        if (!doingYZ)
          memset(fdata, 0, npix);
        fdata += npix * fstride;
      }
    }
  }
}

/*
 * Return the last area drawn in the XY view
 */
void XyzWindow::getSubsetLimits(int &ixStart, int &iyStart, int &nxUse, int &nyUse)
{
  ixStart = mLastXoffset1;
  iyStart = mLastYoffset1;
  nxUse = mLastWidth1;
  nyUse = mLastHeight1;
}

/*
 * Draw the lines around boxes and the position gadgets
 */
void XyzWindow::DrawCurrentLines()
{
  int cx, cy, cz, cenx, ceny, xlim, ylim, cenxlim, cenylim;
  float zoomZ = mZoom * (mApplyZscale ? mVi->imod->zscale : 1);
  int bx = mXwoffset1;
  int by = mYwoffset1;
  int nz = mVi->zsize;
  /*int nx = mVi->xsize;
  int ny = mVi->ysize;
  int xsize = (int)(nx * z + 0.5);
  int ysize = (int)(ny * z + 0.5);
  int zsize = (int)(nz * z + 0.5); */
  int bx2 = mXwoffset2;
  int by2 = mYwoffset2;

  /* DNM 1/23/02: Put the line in the middle now that one can drag it */
  int hlen = GRAB_LENGTH / 2;
  int hwidth = GRAB_WIDTH / 2;
  int xlineY = mYorigin2 - XYZ_GSIZE / 2 - 1;
  int ylineX = mXorigin2 - XYZ_GSIZE / 2 - 1;

  b3dLineWidth(1);

  getLocation(cx, cy, cz);
  b3dColorIndex(App->foreground);

  /* Draw Z location crossed lines and box around X/Y plane */
  cenx = (int)(bx2 + zoomZ * (cz+.5));
  ceny = (int)(by2 + zoomZ * (cz+.5));
  xlim = (int)(bx2 + zoomZ * nz);
  ylim = (int)(by2 + zoomZ * nz);
  cenxlim = B3DMIN(B3DMAX(cenx, mXorigin2 - 1), mXorigin2 + mWinXdim2 + 1);
  cenylim = B3DMIN(B3DMAX(ceny, mYorigin2), mYorigin2 + mWinYdim2);

  //draw Z location X line
  if (ceny == cenylim) {
    b3dDrawLine(mXorigin2, ceny, mXorigin2 + mWinXdim2, ceny);
  }

  //draw Z location Y line
  if (cenx == cenxlim) {
    b3dDrawLine(cenx, mYorigin2, cenx, mYorigin2 + mWinYdim2);
  }

  // Back off from edges and draw grab box
  //draw Z location grab box
  if (cenx == cenxlim && ceny == cenylim) {
    b3dDrawFilledRectangle(cenx - hlen, ceny - hlen, GRAB_LENGTH, GRAB_LENGTH);
  }
  
  //draw XY window box
  b3dDrawRectangle(mXorigin1 - 1, mYorigin1 - 1, mWinXdim1 + 1,
                   mWinYdim1 + 1);
  /*imodPrintStderr("bx - 1 %d  xsize + 1 %d  xsize + 1 %d  ysize + 1 %d\n", 
    bx - 1, xsize + 1, xsize + 1, ysize + 1);*/

       

  /* draw x location line and box around X/Z plane */
  b3dColorIndex(App->bgnpoint);
  cenx = (int)(bx + mZoom * (cx+.5));
  cenxlim = B3DMIN(B3DMAX(cenx, mXorigin1 - 1), mXorigin1 + 
                   mWinXdim1);

  //draw X location line
  b3dDrawLine(cenxlim, xlineY, mXorigin2 + mWinXdim2, xlineY);
  
  //draw grab box for X location
  if (cenx == cenxlim) {
    b3dDrawFilledRectangle(cenx - hwidth, xlineY - hlen, GRAB_WIDTH, GRAB_LENGTH);
  }
  
  //draw YZ window box
  b3dDrawRectangle(mXorigin2 - 1, mYorigin1 - 1, mWinXdim2 + 1, mWinYdim1 + 1);

  /* draw y location line. */
  b3dColorIndex(App->endpoint);
  ceny = (int)(by + mZoom * (cy+.5));
  cenylim = B3DMIN(B3DMAX(ceny, mYorigin1 - 1), mYorigin1 + mWinYdim1);
  b3dDrawLine(ylineX, mYorigin2 + mWinYdim2, ylineX, cenylim);
  
  //draw grab box for Y location
  //only draw box if the current point is displayed
  if (ceny == cenylim){
    b3dDrawFilledRectangle(ylineX - hlen, ceny - hwidth, GRAB_LENGTH, GRAB_WIDTH);
  }
                         
  //draw XZ window box
  b3dDrawRectangle(mXorigin1 - 1, mYorigin2 - 1, mWinXdim1 + 1, mWinYdim2 + 1);

  // Draw a grab box for adjusting allocation between windows
  customGhostColor(64, 192, 255); 
  b3dDrawFilledRectangle(ylineX - hlen - 2, xlineY - hlen - 2, GRAB_LENGTH + 4,
                         GRAB_LENGTH + 4);
  resetGhostColor();
}

// Nonfunctional
void XyzWindow::DrawGhost()
{
  return;
}

/*
 * The contour drawing routine
 */
/* DNM 1/20/02: add argument ob to be able to reset color properly */
/* DNM 5/5/03: replace calls to ivPointVisible with direct Z tests to speed
   things up, now that floor call is needed */
void XyzWindow::DrawContour(Iobj *obj, int ob, int co)
{
  Icont *cont = &(obj->cont[co]);
  bool currentCont = (co == mVi->imod->cindex.contour) &&
    (ob == mVi->imod->cindex.object);
  
  Ipoint *thisPt, *lastPt;
  DrawProps contProps, ptProps;
  int pt;
  bool thisVis, lastVis;
  float zscale;
  int nextChange, stateFlags, changeFlags;
  int handleFlags = HANDLE_LINE_COLOR | HANDLE_2DWIDTH;
  float z = mZoom;
  int bx = mXwoffset1;
  int by = mYwoffset1;
  int bx2 = mXwoffset2;
  int by2 = mYwoffset2;
  int curX, curY, currentZ;

  if (!cont->psize)
    return;
     
  if (ivwTimeMismatch(mVi, mTimeLock, obj, cont))
    return;

  if (ifgGetValueSetupState())
    handleFlags |= HANDLE_VALUE1;

  getLocation(curX, curY, currentZ);
  zscale = ((mVi->imod->zscale ? mVi->imod->zscale : 1.) * mVi->zbin) / mVi->xybin;
  nextChange = ifgHandleContChange(obj, co, &contProps, &ptProps, &stateFlags,
                                   handleFlags, 0);
  if (contProps.gap)
    return;

  utilEnableStipple(mVi, cont);

  if (!iobjScat(obj->flags)) {    

    b3dSubareaViewport(mXorigin1, mYorigin1, mWinXdim1, mWinYdim1);

    /* Open or closed contours, if they are wild or there are any changes
       coming, then need to test every
       point and draw lines between points that are visible */
    lastVis = (int)floor(cont->pts->z + 0.5) == currentZ;
    if ((cont->flags & ICONT_WILD) || nextChange >= 0) {

      /* draw line if this and last point were visible or if this is current
         contour and project is set; draw symbol if visible and one is set */
      for (pt = 0; pt < cont->psize; pt++) {
        thisPt = &(cont->pts[pt]);
        thisVis = (int)floor(thisPt->z + 0.5) == currentZ;
        if (pt && ((lastVis && thisVis) || (currentCont && mProject)) &&
            !ptProps.gap)
          b3dDrawLine((int)(mZoom * thisPt->x + bx),
                      (int)(mZoom * thisPt->y + by),
                      (int)(mZoom * lastPt->x + bx),
                      (int)(mZoom * lastPt->y + by));
        ptProps.gap = 0;
        if (nextChange == pt)
          nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                           &ptProps, &stateFlags, 
                                           &changeFlags, handleFlags, 0);
        if (thisVis && ptProps.symtype != IOBJ_SYM_NONE &&
            !(ptProps.gap && ptProps.valskip))
          utilDrawSymbol((int)(mZoom * thisPt->x + bx), 
                         (int)(mZoom * thisPt->y + by),
                         ptProps.symtype, ptProps.symsize, 
                         ptProps.symflags);
        
        lastVis = thisVis;
        lastPt = thisPt;
      }

      /* Close if all conditions are met */
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN) && !currentCont
          && lastVis && ((int)floor(cont->pts->z + 0.5) == currentZ) && 
          !ptProps.gap)
        b3dDrawLine((int)(mZoom * cont->pts->x + bx),
                    (int)(mZoom * cont->pts->y + by),
                    (int)(mZoom * lastPt->x + bx),
                    (int)(mZoom * lastPt->y + by));

    } else if (lastVis) {

      /* If the contour is not wild or fine grained and it is on this section,
         draw it completely, close if appropriate, and draw symbols in
         separate loop */
      b3dBeginLine();
      for (pt = 0; pt < cont->psize; pt++)
        b3dVertex2i((int)(mZoom * cont->pts[pt].x + bx),  
                    (int)(mZoom * cont->pts[pt].y + by));
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN) && !currentCont)
        b3dVertex2i((int)(bx + mZoom * cont->pts->x), 
                    (int)(by + mZoom * cont->pts->y));
      b3dEndLine();
            
      if (ptProps.symtype != IOBJ_SYM_NONE)
        for (pt = 0; pt < cont->psize; pt++)
          utilDrawSymbol((int)(mZoom * cont->pts[pt].x + bx), 
                        (int)(mZoom * cont->pts[pt].y + by),
                         ptProps.symtype, ptProps.symsize,
                         ptProps.symflags);
    }



    /* Now draw symbols and points in X/Z and Y/Z views */
    b3dSubareaViewport(mXorigin1, mYorigin2, mWinXdim1, mWinYdim2);
    DrawSymProj(obj, cont, co, &contProps, &ptProps, &stateFlags, handleFlags,
                nextChange, 0, 2, 1, curY, (float)bx, by2 + 0.5 * z,
                currentCont);
    b3dSubareaViewport(mXorigin2, mYorigin1, mWinXdim2, mWinYdim1);
    DrawSymProj(obj, cont, co, &contProps, &ptProps, &stateFlags, handleFlags,
                nextChange, 2, 1, 0, curX, bx2 + 0.5 * z, (float)by,
                currentCont);
  }

  utilDisableStipple(mVi, cont);

  // Draw symbols for scattered points, spheres, and end markers in each window
  b3dSubareaViewport(mXorigin1, mYorigin1, mWinXdim1, mWinYdim1);
  DrawScatSymAllSpheres(obj, ob, cont, co, &contProps, &ptProps, &stateFlags, 
                        handleFlags, nextChange, 0, 1, 2, currentZ, 
                        (float)bx, (float)by, zscale);
  b3dSubareaViewport(mXorigin1, mYorigin2, mWinXdim1, mWinYdim2);
  DrawScatSymAllSpheres(obj, ob, cont, co, &contProps, &ptProps, &stateFlags, 
                        handleFlags, nextChange, 0, 2, 1, curY, 
                        (float)bx, by2 + 0.5 * z, 1.);
  b3dSubareaViewport(mXorigin2, mYorigin1, mWinXdim2, mWinYdim1);
  DrawScatSymAllSpheres(obj, ob, cont, co, &contProps, &ptProps, &stateFlags, 
                        handleFlags, nextChange, 2, 1, 0, curX, 
                        bx2 + 0.5 * z, (float)by, 1.);
  b3dResizeViewportXY(mWinx, mWiny);
}

/*
 * Generic routine to draw symbols and projection of current contour on
 * XZ and YZ subwindows
 */
void XyzWindow::DrawSymProj(Iobj *obj, Icont *cont, int co,
                            DrawProps *contProps, DrawProps *ptProps,
                            int *stateFlags, int handleFlags, int nextChange,
                            int indx, int indy, int indz, int currentZ,
                            float bx, float by, bool currentCont)
{
  int pt, next;
  bool thisVis;
  float *point;
  float *nexpt;
  int changeFlags;
  float xzoom, yzoom;
  float scales[3] = {1., 1., 1.};
  if (mApplyZscale)
    scales[2] = mVi->imod->zscale;
  xzoom = scales[indx] * mZoom;
  yzoom = scales[indy] * mZoom;

  if (ilistSize(cont->store))
    nextChange = ifgHandleContChange(obj, co, contProps, ptProps, 
                                       stateFlags, handleFlags, 0);
  for (pt = 0; pt < cont->psize; pt++) {
    ptProps->gap = 0;
    point = (float *)(&cont->pts[pt]);
    if (pt == nextChange)
      nextChange = ifgHandleNextChange(obj, cont->store, contProps, 
                                       ptProps, stateFlags,
                                       &changeFlags, handleFlags, 0);
    next = (pt + 1) % cont->psize;
    thisVis = (int)point[indz] == currentZ;
        
    /* Symbol if in plane */
    if (thisVis && ptProps->symtype != IOBJ_SYM_NONE && 
        !(ptProps->gap && ptProps->valskip))
      utilDrawSymbol((int)(xzoom * point[indx] + bx), 
                     (int)(yzoom * point[indy] + by),
                     ptProps->symtype, ptProps->symsize, 
                     ptProps->symflags);

    /* connecting line if in plane or if projecting current cont, and not 
       last point unless closure is appropriate */
    nexpt = (float *)(&cont->pts[next]);
    if (((thisVis && (int)(nexpt[indz]) == currentZ) || 
         (currentCont && mProject)) && !ptProps->gap && 
        (next || (!currentCont &&
                  (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN)))))
      b3dDrawLine((int)(xzoom * point[indx] + bx),
                  (int)(yzoom * point[indy] + by),
                  (int)(xzoom * nexpt[indx] + bx),
                  (int)(yzoom * nexpt[indy] + by));
    ptProps->gap = 0;
  }
}

/*
 * Generic routine to draw symbols on scattered points, scattered points and
 * other spheres, and contour end markers on one subarea
 */
void XyzWindow::DrawScatSymAllSpheres(Iobj *obj, int ob,  Icont *cont, int co, 
                                      DrawProps *contProps,
                                      DrawProps *ptProps, int *stateFlags,
                                      int handleFlags, int nextChange, 
                                      int indx, int indy, int indz,
                                      int zmouse, float bx, float by,
                                      float zscale)
{
  int pt, testz;
  float *point, *lpoint;
  int changeFlags, radius;
  int currentZ = zmouse;
  float drawsize, delz;
  float xzoom, yzoom;
  float scales[3] = {1., 1., 1.};
  if (mApplyZscale)
    scales[2] = mVi->imod->zscale;
  xzoom = scales[indx] * mZoom;
  yzoom = scales[indy] * mZoom;

  /* scattered contour - symbols in all three planes */
  if (ilistSize(cont->store))
    nextChange = ifgHandleContChange(obj, co, contProps, ptProps, 
                                       stateFlags, handleFlags, 0);
  if (iobjScat(obj->flags) && 
      (contProps->symtype != IOBJ_SYM_NONE || nextChange >= 0)) {
    for (pt = 0; pt < cont->psize; pt++) {
      ptProps->gap = 0;
      point = (float *)(&cont->pts[pt]);
      if (pt == nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, contProps, 
                                         ptProps, stateFlags,
                                         &changeFlags, handleFlags, 0);
      if (ptProps->symtype != IOBJ_SYM_NONE && 
          !(ptProps->gap && ptProps->valskip)) {
        testz = indz == 2 ? B3DNINT(point[indz]) : (int)point[indz];
        if (testz == currentZ)
          utilDrawSymbol((int)(xzoom * point[indx] + bx), 
                         (int)(yzoom * point[indy] + by),
                         ptProps->symtype, ptProps->symsize, 
                         ptProps->symflags);
        
      }
    }
  }

  /* scattered contour or contour with points to be draw */
  if (iobjScat(obj->flags) || obj->pdrawsize || cont->sizes ) {
    if (ilistSize(cont->store))
      nextChange = ifgHandleContChange(obj, co, contProps, ptProps, 
                                       stateFlags, handleFlags, 0);
    for (pt = 0; pt < cont->psize; pt++) {
      ptProps->gap = 0;
      if (pt == nextChange)
        nextChange = ifgHandleNextChange(obj, cont->store, contProps, 
                                         ptProps, stateFlags,
                                         &changeFlags, handleFlags, 0);
      drawsize = imodPointGetSize(obj, cont, pt) / mVi->xybin;
      if (drawsize > 0 && !(ptProps->gap && ptProps->valskip)) {
        point = (float *)(&cont->pts[pt]);
        testz = indz == 2 ? B3DNINT(point[indz]) : (int)point[indz];
        if (testz == currentZ) {

          /* If there's a size, draw a circle and a plus for a
             circle that's big enough */
          b3dDrawCircle((int)(bx + xzoom * point[indx]),
                        (int)(by + yzoom * point[indy]), (int)(mZoom * drawsize));
          if (drawsize > 3)
            b3dDrawPlus((int)(bx + xzoom * point[indx]),
                        (int)(by + yzoom * point[indy]), 3);
        } else if (drawsize > 1 && !(obj->flags & IMOD_OBJFLAG_PNT_ON_SEC)) {

          /* for off-section, compute size of circle and
             draw a smaller circ if further away. */
          delz = (point[indz] - zmouse) * zscale;
          if (delz < 0)
            delz = -delz;
          
          if (delz < drawsize - 0.01) {
            radius = (int)(sqrt((double)(drawsize * drawsize - delz * delz))
                           * mZoom);
            b3dDrawCircle((int)(bx + xzoom * point[indx]),
                          (int)(by + yzoom * point[indy]), radius);
          }
        }
      }
    }
  }

  /* draw end markers if requested */
  b3dLineWidth(contProps->linewidth2);
  if (obj->symflags & (IOBJ_SYMF_ENDS | IOBJ_SYMF_ARROW)) {
    if (!(obj->symflags & IOBJ_SYMF_ARROW))
      b3dColorIndex(App->endpoint);
    point = (float*)(&cont->pts[cont->psize-1]);

    for (pt = 0; pt < 2; pt ++) {
      testz = indz == 2 ? B3DNINT(point[indz]) : (int)point[indz];
      if (testz == currentZ) {
        if (!pt && (obj->symflags & IOBJ_SYMF_ARROW) && cont->psize > 1) {
          lpoint = (float*)(&cont->pts[cont->psize-2]);
          b3dDrawArrow((int)(bx + xzoom * lpoint[indx]), (int)(by + yzoom *lpoint[indy]), 
                       (int)(bx + xzoom * point[indx]), (int)(by + yzoom * point[indy]), 
                       obj->symsize, obj->linewidth2, false);
          
        } else if (obj->symflags & IOBJ_SYMF_ENDS) {
          b3dDrawCross((int)(bx + xzoom * point[indx]), (int)(by + yzoom * point[indy]),
                       obj->symsize/2);
        }
      }
      b3dColorIndex(App->bgnpoint);
      point = (float *)cont->pts;
    }

    /* DNM 1/21/02: need to reset color this way, not wih b3dColorIndex*/
    imodSetObjectColor(ob);

  }
}

/*
 * Draw model by looping through all contours */
void XyzWindow::DrawModel()
{
  Imod *imod = mVi->imod;
  Iobj *obj;
  int ob, co;

  if (imod->drawmode <= 0)
    return;
  if (mVi->ghostmode)
    DrawGhost();
     
  for (ob = 0; ob < imod->objsize; ob++) {
    obj = &(imod->obj[ob]);
    if (iobjOff(obj->flags))
      continue;
    imodSetObjectColor(ob);
    b3dLineWidth(obj->linewidth2);
    ifgSetupValueDrawing(obj, GEN_STORE_MINMAX1);
    for (co = 0; co < imod->obj[ob].contsize; co++)
      DrawContour(obj, ob, co);
  }
}

/*
 * Draw current image or model point and begin/end points of current contour 
*/
void XyzWindow::DrawCurrentPoint()
{
  Iobj *obj = imodObjectGet(mVi->imod);
  Icont *cont = imodContourGet(mVi->imod);
  Ipoint *pnt = imodPointGet(mVi->imod);
  Ipoint *point;
  int psize;
  int cx, cy, cz, pt;
  int bx = mXwoffset1;
  int by = mYwoffset1;
  int bx2 = mXwoffset2;
  int by2 = mYwoffset2;
  int imPtSize, modPtSize, backupSize;
  float zoomZ = mZoom * (mApplyZscale ? mVi->imod->zscale : 1);

  getLocation(cx, cy, cz);
  
  if (!mVi->drawcursor)
    return;

  b3dLineWidth(1);
  utilCurrentPointSize(obj, &modPtSize, &backupSize, &imPtSize);
  psize = modPtSize;
  if (cont && cont->psize > 1 && 
      (pnt == cont->pts || pnt == cont->pts + cont->psize - 1))
    psize = backupSize;

  /* Draw begin and end points of selected contour in model mode. */
  if (mVi->imod->mousemode == IMOD_MMODEL && cont && cont->psize > 1 &&
      !ivwTimeMismatch(mVi, mTimeLock, obj, cont)) {

    b3dLineWidth(obj->linewidth2);
    b3dColorIndex(App->bgnpoint);
    point = cont->pts;

    for (pt = 0; pt < 2; pt ++) {
      if (B3DNINT(point->z) == cz) {
        b3dSubareaViewport(mXorigin1, mYorigin1, mWinXdim1, mWinYdim1);
        b3dDrawCircle((int)(mZoom * point->x+bx),
                      (int)(mZoom * point->y+by), modPtSize);
      }
      if ((int)point->y == cy) {
        b3dSubareaViewport(mXorigin1, mYorigin2, mWinXdim1, mWinYdim2);
        b3dDrawCircle((int)(mZoom * point->x+bx),
                      (int)(zoomZ * (point->z + 0.5) + by2), modPtSize);
      }
      if ((int)point->x == cx) {
        b3dSubareaViewport(mXorigin2, mYorigin1, mWinXdim2, mWinYdim1);
        b3dDrawCircle((int)(zoomZ * (point->z + 0.5) + bx2),
                      (int)(mZoom * point->y+by), modPtSize);
      }
      b3dColorIndex(App->endpoint);
      point = &(cont->pts[cont->psize-1]);
    }
  }
     
  /* Draw location of current point. */
  /* DNM 1/21/02: do it like zap window, draw only if in model mode,
     otherwise draw crosses at current mouse point */
  if (mVi->imod->mousemode == IMOD_MMODEL &&  pnt) {
    b3dLineWidth(obj->linewidth2);
          
    if (B3DNINT(pnt->z) == cz && !ivwTimeMismatch(mVi, mTimeLock, obj, cont)) {
      b3dColorIndex(App->curpoint);
    }else{
      b3dColorIndex(App->shadow);
    }
    b3dSubareaViewport(mXorigin1, mYorigin1, mWinXdim1, mWinYdim1);
    b3dDrawCircle((int)(mZoom * pnt->x + bx), (int)(mZoom * pnt->y + by), psize);
    b3dColorIndex(App->curpoint);
    b3dSubareaViewport(mXorigin1, mYorigin2, mWinXdim1, mWinYdim2);
    b3dDrawPlus((int)(mZoom * pnt->x + bx), (int)(zoomZ * (cz + 0.5) + by2), psize);
    b3dSubareaViewport(mXorigin2, mYorigin1, mWinXdim2, mWinYdim1);
    b3dDrawPlus((int)(zoomZ * (cz + 0.5) + bx2), (int)(by + mZoom * pnt->y), psize);
  } else {
    b3dColorIndex(App->curpoint);
    b3dSubareaViewport(mXorigin1, mYorigin1, mWinXdim1, mWinYdim1);
    b3dDrawPlus((int)(mZoom * (cx + .5) + bx), (int)(mZoom * (cy + .5) + by), imPtSize);
    b3dSubareaViewport(mXorigin1, mYorigin2, mWinXdim1, mWinYdim2);
    b3dDrawPlus((int)(mZoom * (cx + .5) + bx), (int)(zoomZ * (cz + .5) + by2), imPtSize);
    b3dSubareaViewport(mXorigin2, mYorigin1, mWinXdim2, mWinYdim1);
    b3dDrawPlus((int)(bx2 + zoomZ * (cz + .5)), (int)(by + mZoom * (cy + .5)), imPtSize);
  }
  b3dResizeViewportXY(mWinx, mWiny);
}

// send a new value of section, zoom, or time label if it has changed
void XyzWindow::drawTools()
{  
  int cx, cy, cz;
  getLocation(cx, cy, cz);
  if (mToolMaxX != mVi->xsize) {
    mToolMaxX = mVi->xsize;
    setMaxAxis(X_COORD, mToolMaxX);
  }
  
  if (mToolMaxY != mVi->ysize) {
    mToolMaxY = mVi->ysize;
    setMaxAxis(Y_COORD, mToolMaxY);
  }
  
  if (mToolMaxZ != mVi->zsize) {
    mToolMaxZ = mVi->zsize;
    setMaxAxis(Z_COORD, mToolMaxZ);
  }
     
  // Workaround to Qt 4.5.0 cocoa bug, need to load this boxes 3 times
  if (mToolZoom != mZoom){
    if (mToolZoom < 0.)
      mToolZoom -= 1.;
    if (mToolZoom <= -4. || mToolZoom > -0.9)
    mToolZoom = mZoom;
    setZoomText(mZoom);
  }
  
  setSlider(X_COORD, cx);
  setSlider(Y_COORD, cy);
  setSlider(Z_COORD, cz);
}

// Nonfunctional
void XyzWindow::DrawAuto()
{
#ifdef FIX_xyzDrawAuto_BUG
  ImodView *vi = mVi;
  int i, j;
  float vert[2];
  unsigned short cdat;
  int x, y;
  unsigned int pixel;
     

  if (!vi->ax)
    return;
     
  if (!vi->ax->filled)
    return;
     
  if (vi->ax->cz != vi->zmouse)
    return;

  /* Buggy need to fix. */

  cdat = App->endpoint;

  for (j = 0; j < vi->ysize; j++) {
    y = j + XYZ_BSIZE;
    for(i = 0; i < vi->xsize; i++) {
      x = i + XYZ_BSIZE;
      if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_FLOOD) {
        pixel = App->endpoint;
        if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_BLACK) {
          pixel = vi->rampbase;
        }
      }else{
        if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_BLACK) {
          pixel = vi->rampbase;
        }
                    
        if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_WHITE) {
          pixel = vi->rampbase + vi->rampsize;
        }
      }
      b3dColorIndex(pixel);
      b3dDrawFilledRectangle(x, y, 1,1);
    }
  }
#endif
  return;
}


void XyzWindow::Draw()
{
  mGLw->updateGL();
}

void XyzWindow::stateToggled(int index, int state)
{
  setControlAndLimits();
  switch (index) {
  case XYZ_TOGGLE_RESOL:
    mHq = state;
    Draw();
    break;
  case XYZ_TOGGLE_LOCKED:
    mLock = state;
    mTimeLock = 0;
    if (!mLock) {
      Draw();
    } else {
      ivwGetLocation(mVi, &mXlock, &mYlock, &mZlock);
      ivwGetTime(mVi, &mTimeLock);
    }
    break;
  case XYZ_TOGGLE_ZSCALE:
    mApplyZscale = state;
    Draw();
    break;
  }
}


// One of toggle buttons needs to change state
void XyzWindow::toggleClicked(int index)
{
  int state = mToggleButs[index]->isChecked() ? 1 : 0;
  mToggleStates[index] = state; 
  stateToggled(index, state);
}

void XyzWindow::centerClicked()
{
  int curx, cury, curz;
  Ipoint *pt = imodPointGet(mVi->imod);
  getLocation(curx, cury, curz);
  if (!mLock && mVi->imod->mousemode == IMOD_MMODEL && pt) {
    curx = pt->x;
    cury = pt->y;
    curz = pt->z;
  }
  mXtrans1 = mVi->xsize / 2. - curx;
  mYtrans1 = mVi->ysize / 2. - cury;
  mXtrans2 = mVi->zsize / 2. - curz;
  mYtrans2 = mXtrans2;
  Draw();
}

void XyzWindow::fillCachePressed()
{
  ivwControlPriority(mVi, mCtrl);
  imodCacheFill(mVi, 1);
}

void XyzWindow::keyReleaseEvent (QKeyEvent * e )
{
  if (e->key() == hotSliderKey()) {
    mCtrlPressed = false;
    releaseKeyboard();
  }
  keyRelease(e);
}

void XyzWindow::keyRelease(QKeyEvent *event)
{
  /*  imodPrintStderr ("%d down\n", downtime.elapsed()); */
  if (!insertDown || !(event->modifiers() & Qt::KeypadModifier) ||
      (event->key() != Qt::Key_Insert && event->key() != Qt::Key_0))
    return;
  insertDown = 0;
  mGLw->setMouseTracking(pixelViewOpen);
  releaseKeyboard();
  mGLw->releaseMouse();
  if (mDrawCurrentOnly) {
    mDrawCurrentOnly = 0;
    Draw();
  }
}

// Key handler
void XyzWindow::keyPressEvent ( QKeyEvent * event )
{
  Imod *imod = mVi->imod;
  Iobj *obj = imodObjectGet(imod);
  Icont *cont = imodContourGet(imod);
  int keysym = event->key();
  int shifted = event->modifiers() & Qt::ShiftModifier;
  int ctrl = event->modifiers() & Qt::ControlModifier;
  float xMouse, yMouse, dist2d, dx, dy, dz, refx, refy, refz;
  QString str;
  Ipoint *curPnt;
  bool convertKeys = false;
  int keyuse, ix, iy, zMouse, rx, plane;
  int keypad = event->modifiers() & Qt::KeypadModifier;

  if (utilCloseKey(event)) {
    close();
    return;
  }

  if (hotSliderFlag() != NO_HOT_SLIDER && event->key() == hotSliderKey()) {
    mCtrlPressed = true;
    grabKeyboard();
  }

  if (inputTestMetaKey(event))
    return;

  // Start with this at 1: set to 0 if NOT handled
  int handled = 1;

  ivwControlPriority(mVi, mCtrl);

  // This is needed in 3 places, just do it once
  ix = (mGLw->mapFromGlobal(QCursor::pos())).x();
  iy = (mGLw->mapFromGlobal(QCursor::pos())).y();
  plane = Getxyz(ix, iy, xMouse, yMouse, zMouse);

  switch(keysym) {

  case Qt::Key_Minus:
    mZoom = b3dStepPixelZoom(mZoom, -1);
    Draw();
    break;
             
  case Qt::Key_Plus:
  case Qt::Key_Equal:
    mZoom = b3dStepPixelZoom(mZoom, 1);
    Draw();
    break;

  case Qt::Key_S:
    if (shifted || ctrl) {

      // Need to draw the window now (didn't have to in X version)
      Draw();
      b3dKeySnapshot("xyz", shifted, ctrl, NULL);
    } else
      handled = 0;
    break;

  case Qt::Key_R:
    if (!ctrl) {
      mHq = 1 - mHq;
      mToggleStates[XYZ_TOGGLE_RESOL] = mHq;
      diaSetChecked(mToggleButs[XYZ_TOGGLE_RESOL], mHq != 0);
      wprint("\aHigh-resolution mode %s\n", mHq ? "ON" : "OFF");
    } else
      handled = 0;
    break;

  case Qt::Key_K:
    centerClicked();
    break;

  case Qt::Key_P:
    if (!shifted) {
      handled = 0;
      break;
    }
    mProject = 1 - mProject;
    wprint("\aCurrent contour projection into side views %s\n",
           mProject ? "ON" : "OFF");
    Draw();
    break;

    /* DNM: Keypad Insert key, alternative to middle mouse button */
  case Qt::Key_Insert:

    /* But skip out if in movie mode or already active */
    if (!keypad || imod->mousemode == IMOD_MMOVIE || insertDown) {
      handled = 0;
      break;
    }

    // Set a flag, set continuous tracking, grab keyboard and mouse
    insertDown = 1;
    mGLw->setMouseTracking(true);
    grabKeyboard();
    mGLw->grabMouse();

    /* Use time since last event to determine whether to treat like
       single click or drag */
    rx = insertTime.elapsed();
    insertTime.restart();
    if(rx > 250)
      B2Press(ix, iy);
    else
      B2Drag(ix, iy); 

    mLmx = ix;
    mLmy = iy;
    break;

  case Qt::Key_PageUp:
  case Qt::Key_PageDown:
  case Qt::Key_Left:
  case Qt::Key_Right:
  case Qt::Key_Down:
  case Qt::Key_Up:
    keyuse = keysym;
    convertKeys = plane == X_SLICE_BOX || plane == Y_SLICE_BOX;
    if (convertKeys) {
      if (keysym == Qt::Key_PageUp)
        keyuse = plane == X_SLICE_BOX ? Qt::Key_Right : Qt::Key_Up;
      if (keysym == Qt::Key_PageDown)
        keyuse = plane == X_SLICE_BOX ? Qt::Key_Left : Qt::Key_Down;
      if (plane == X_SLICE_BOX && keysym == Qt::Key_Left)
        keyuse = Qt::Key_PageDown;
      if (plane == X_SLICE_BOX && keysym == Qt::Key_Right)
        keyuse = Qt::Key_PageUp;
      if (plane == Y_SLICE_BOX && keysym == Qt::Key_Down)
        keyuse = Qt::Key_PageDown;
      if (plane == Y_SLICE_BOX && keysym == Qt::Key_Up)
        keyuse = Qt::Key_PageUp;
    }
    if (!keypad && (mLock || convertKeys)) {
      if (mLock) {
        if (keyuse == Qt::Key_PageUp || keyuse == Qt::Key_PageDown)
          setLocation(NOTNEW, NOTNEW, mZlock + (keyuse == Qt::Key_PageUp ? 1 : -1));
        else if (keyuse == Qt::Key_Left || keyuse == Qt::Key_Right)
          setLocation(mXlock + (keyuse == Qt::Key_Right ? 1 : -1), NOTNEW, NOTNEW);
        else
          setLocation(NOTNEW, mYlock + (keyuse == Qt::Key_Up ? 1 : -1), NOTNEW);
        Draw();
      } else {
        if (keyuse == Qt::Key_Up)
          inputNexty(mVi);
        else if (keyuse == Qt::Key_Down)
          inputPrevy(mVi);
        else if (keyuse == Qt::Key_Right)
          inputNextx(mVi);
        else if (keyuse == Qt::Key_Left)
          inputPrevx(mVi);
        else
          inputPageUpOrDown(mVi, 0, keyuse == Qt::Key_PageUp ? 1 : -1);
      }
    } else if (keypad && imod->mousemode == IMOD_MMODEL) {
      if (convertKeys && (keysym == Qt::Key_PageUp || keysym == Qt::Key_PageDown) &&
          iobjPlanar(obj->flags) && imodContourIsPlanar(cont, plane))
        wprint("\aContour is no longer in one %s plane. With this contour, you will not"
               " get a new contour automatically when you change %s slice.\n", 
               plane == X_SLICE_BOX ? "YZ" : "XZ", plane == X_SLICE_BOX ? "YZ" : "XZ");
      inputKeyPointMove(mVi, keyuse);
    } else
      handled = 0;
    break;

  case Qt::Key_1:
  case Qt::Key_2:
    if (mTimeLock) {
      mTimeLock += (keysym == Qt::Key_1) ? -1 : 1;
      mTimeLock = B3DMAX(1, B3DMIN(mVi->numTimes, mTimeLock));
      Draw();
    } else 
      handled = 0;
    break;

  case Qt::Key_Q:
    if (plane == NOT_IN_BOX || plane > Z_SLICE_BOX)
      break;

    // Use the lock position or the xyzmouse; or substitute the current point
    refx = mLock ? mXlock : mVi->xmouse;
    refy = mLock ? mYlock : mVi->ymouse;
    refz = mLock ? mZlock : mVi->zmouse;
    curPnt = imodPointGet(imod);
    if (curPnt && imod->mousemode == IMOD_MMODEL) {
      refx = curPnt->x;
      refy = curPnt->y;
      refz = curPnt->z;
    }
    dx = mVi->xybin * (xMouse - refx);
    dy = mVi->xybin * (yMouse - refy);
    dz = mVi->zbin * (zMouse - refz) * imod->zscale;

    // One of these coordinates should match so just get a 3D distance
    dist2d  = (float)sqrt((double)dx * dx + dy * dy + dz * dz);
    wprint("From (%.1f, %.1f, %.1f) to (%.1f, %.1f, %.1f) =\n", refx + 1,
           refy + 1., refz + 1., xMouse + 1., yMouse + 1., zMouse + 1.);
    str.sprintf("  %.1f %spixels", dist2d, mVi->xybin * mVi->zbin > 1 ? "unbinned " : "");
    utilWprintMeasure(str, imod, dist2d);
    break;

  default:
    handled = 0;
    break;
  }

  if (!handled)
    inputQDefaultKeys(event, mVi);
}

void XyzWindow::getLocation(int &cx, int &cy, int &cz)
{
  if (mLock) {
    cx = mXlock;
    cy = mYlock;
    cz = mZlock;
  } else {
    ivwGetLocation(mVi, &cx, &cy, &cz);
  }
}

void XyzWindow::setLocation(int newx, int newy, int newz)
{
  if (newx != NOTNEW) {
    mXlock = B3DMAX(0, B3DMIN(mVi->xsize - 1, newx));
    if (!mLock)
      mVi->xmouse = mXlock;
  }
  if (newy != NOTNEW) {
    mYlock = B3DMAX(0, B3DMIN(mVi->ysize - 1, newy));
    if (!mLock)
      mVi->ymouse = mYlock;
  }
  if (newz != NOTNEW) {
    mZlock = B3DMAX(0, B3DMIN(mVi->zsize - 1, newz));
    if (!mLock)
      mVi->zmouse = mZlock;
  }
}

XyzGL::XyzGL(QGLFormat inFormat, XyzWindow * parent)
  : QGLWidget(inFormat, parent)
{
  mMousePressed = false;
  mWin = parent;
  mClosing = false;
  mFirstDraw = true;
}

// When the timer fires after the first draw, do another draw
void XyzGL::timerEvent(QTimerEvent * e )
{
  killTimer(mTimerID);
  updateGL();
}


// The main drawing routine
/* DNM 1/21/02: eliminate OpenGL scaling of native coordinates, make all
   model drawing routines multiply coordinates by zoom */
/* DNM 1/28/02: moved uses of the elements of xx to after the test for zz */
void XyzGL::paintGL()
{
  float z;
  int bx, by, bx2, by2;

  if (!mWin->mExposed) return;     /* DNM: avoid crashes if Zap is movieing*/

  if (mFirstDraw) {
    mTimerID = startTimer(10);
    mFirstDraw = false;
  }
  
  b3dSetCurSize(mWin->mWinx, mWin->mWiny);
  z = mWin->mZoom;
  bx = mWin->mXwoffset1;
  by = mWin->mYwoffset1;
  bx2 = mWin->mXwoffset2;
  by2 = mWin->mYwoffset2;


  mWin->DrawImage();
  mWin->DrawModel();

  mWin->DrawCurrentLines();
  mWin->DrawCurrentPoint();
  mWin->DrawAuto();

  if (xyzShowSlice) {
    b3dColorIndex(App->foreground);
     
    b3dSubareaViewport(mWin->mXorigin1, mWin->mYorigin1, mWin->mWinXdim1, 
                       mWin->mWinYdim1);     
    b3dDrawLine((int)(bx + (mWin->mVi->slice.zx1 * z)), 
                (int)(by + (mWin->mVi->slice.zy1 * z)),
                (int)(bx + (mWin->mVi->slice.zx2 * z)), 
                (int)(by + (mWin->mVi->slice.zy2 * z)));
                
    b3dSubareaViewport(mWin->mXorigin1, mWin->mYorigin2, mWin->mWinXdim1, 
                       mWin->mWinYdim2);  
    b3dDrawLine((int)(bx + (mWin->mVi->slice.yx1 * z)),
                (int)(by2+ (mWin->mVi->slice.yz1 * z)),
                (int)(bx + (mWin->mVi->slice.yx2 * z)),
                (int)(by2+ (mWin->mVi->slice.yz2 * z)));

    b3dSubareaViewport(mWin->mXorigin2, mWin->mYorigin1, mWin->mWinXdim2, 
                       mWin->mWinYdim1);  
    b3dDrawLine((int)(bx2+ (mWin->mVi->slice.xz1 * z)),
                (int)(by + (mWin->mVi->slice.xy1 * z)),
                (int)(bx2+ (mWin->mVi->slice.xz2 * z)),
                (int)(by + (mWin->mVi->slice.xy2 * z)));

    b3dResizeViewportXY(mWin->mWinx, mWin->mWiny);  
    xyzShowSlice = 0;
  }
  mWin->mScaleBarSize = scaleBarDraw(mWin->mWinx, mWin->mWiny, z, 0);
  mWin->drawTools();
  glFlush();
}


// The routine that initializes or reinitializes upon resize
void XyzGL::resizeGL( int winx, int winy )
{
  ivwControlPriority(mWin->mVi, mWin->mCtrl);

  mWin->mWinx = winx;
  mWin->mWiny = winy;
  b3dSetCurSize(winx, winy);

  b3dResizeViewportXY(winx, winy);
  
  mWin->GetCIImages();
  mWin->mExposed = 1;
}

// Handlers for mouse events
void XyzGL::mousePressEvent(QMouseEvent * event )
{
  int button1, button2, button3;
  float mx, my;
  int mz;
  mMousePressed = true;

  ivwControlPriority(mWin->mVi, mWin->mCtrl);
  
  button1 = event->buttons() & ImodPrefs->actualButton(1) ? 1 : 0;
  button2 = event->buttons() & ImodPrefs->actualButton(2) ? 1 : 0;
  button3 = event->buttons() & ImodPrefs->actualButton(3) ? 1 : 0;
  utilRaiseIfNeeded(mWin, event);
  mWin->SetCursor(mWin->mMousemode, utilNeedToSetCursor());

  if (event->button() == ImodPrefs->actualButton(1) && !button2 && !button3) {
    but1downt.start();
    mWin->mWhichbox = mWin->Getxyz(event->x(), event->y(), mx, my, mz);

  } else if (event->button() == ImodPrefs->actualButton(2) &&
             !button1 && !button3) {
    mWin->B2Press(event->x(), event->y());

  } else if (event->button() == ImodPrefs->actualButton(3) &&
             !button1 && !button2) {
    mWin->B3Press(event->x(), event->y());
  }

  mWin->mLmx = event->x();
  mWin->mLmy = event->y();
  mWin->mFirstMx = mWin->mLmx;
  mWin->mFirstMy = mWin->mLmy;
  //imodPrintStderr("Mouse press at %d %d\n", mWin->mLmx, mWin->mLmy);
}

void XyzGL::mouseReleaseEvent( QMouseEvent * event )
{
  mMousePressed = false;
  if (event->button() == ImodPrefs->actualButton(1)) {
      if (but1downt.elapsed() > 250)
        return;
      mWin->B1Press(event->x(), event->y());
  }
}

void XyzGL::mouseMoveEvent( QMouseEvent * event )
{
  static int button1, button2, button3, ex, ey, processing = 0;
  float mx, my;
  int mz, whichbox, cumdx, cumdy;
  int cumthresh = 6 * 6;

  // Reject event if closing (why is there an event anyway?)
  if (mClosing)
    return;

  // Record state of event and return if processing
  ex = event->x();
  ey = event->y();
  button1 = (event->buttons() & ImodPrefs->actualButton(1)) ? 1 : 0;
  button2 = (event->buttons() & ImodPrefs->actualButton(2)) ? 1 : 0;
  button3 = (event->buttons() & ImodPrefs->actualButton(3)) ? 1 : 0;
  button2 = (button2 || insertDown) ? 1 : 0;
  if (processing) {
    processing++;
    return;
  }

  if (pixelViewOpen) {
    whichbox = mWin->Getxyz(ex, ey, mx, my, mz);
    if (whichbox != NOT_IN_BOX && whichbox <= Z_SLICE_BOX)
      pvNewMousePosition(mWin->mVi, mx, my, mz);
  }

  if(!mMousePressed)
    return;

  // Flush events for panning moves
  if ( (button1) && (!button2) && (!button3)) {
    processing = 1;
    imod_info_input();
    if (imodDebug('m') && processing > 1)
      imodPrintStderr("Flushed %d move events\n", processing - 1);
    processing = 0;
  }

  ivwControlPriority(mWin->mVi, mWin->mCtrl);

  if ( (button1) && (!button2) && (!button3)) {
    cumdx = mWin->mFirstMx - ex;
    cumdy = mWin->mFirstMy - ey;
    if (!mWin->mWhichbox || mWin->mWhichbox > Z_SLICE_BOX || 
        but1downt.elapsed() > 250 || cumdx * cumdx + cumdy * cumdy > cumthresh)
      mWin->B1Drag(ex, ey);
  }
  if ( (!button1) && (button2) && (!button3))
    mWin->B2Drag(ex, ey);
  if ( (!button1) && (!button2) && (button3))
    mWin->B3Drag(ex, ey);
  
  mWin->mLmx = ex;
  mWin->mLmy = ey;
}

void XyzGL::wheelEvent (QWheelEvent *e)
{
  if (iceGetWheelForSize())
    utilWheelChangePointSize(mWin->mVi, mWin->mZoom, e->delta());
}
