/*
 *  xyz.c -- Open the XYZ Window; View the X, Y and Z axis.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
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
#include "imod_display.h"
#include "b3dgfx.h"
#include "control.h"
#include "imod_info_cb.h"
#include "imod_input.h"
#include "autox.h"
#include "pixelview.h"
#include "imod_edit.h"
#include "imod_model_edit.h"
#include "imod_workprocs.h"
#include "imod_moviecon.h"
#include "preferences.h"
#include "undoredo.h"
#include "istore.h"
#include "finegrain.h"
#include "arrowbutton.h"
#include "tooledit.h"
#include "imod_model_edit.h"
#include "dia_qtutils.h"
#include "multislider.h"
#include "scalebar.h"

#include "lowres.bits"
#include "highres.bits"
#include "keepCenter.bits"
#include "unlock.bits"
#include "lock.bits"

#define BM_WIDTH 16
#define BM_HEIGHT 16
#define XYZ_BSIZE 8
#define XYZ_GSIZE 16
#define ALL_BORDER (2 * XYZ_BSIZE + XYZ_GSIZE)
#define GRAB_LENGTH 7
#define GRAB_WIDTH 3
#define XYZ_TOGGLE_RESOL 0
#define XYZ_TOGGLE_LOCKED 1
#define MAX_SLIDER_WIDTH 100
#define MIN_SLIDER_WIDTH 20
#define NOTNEW -999999999

static unsigned char *bitList[MAX_XYZ_TOGGLES][2] =
  {{lowres_bits, highres_bits},
   {unlock_bits, lock_bits}};
static char *toggleTips[] = {
  "Toggle between regular and high-resolution (interpolated) image"};

static QIcon *icons[MAX_XYZ_TOGGLES];
static QIcon *cenIcon = NULL;

static char *sliderLabels[] = {"X", "Y", "Z"};
enum {X_COORD = 0, Y_COORD, Z_COORD};
enum {NOT_IN_BOX = 0, X_SLICE_BOX, Y_SLICE_BOX, Z_SLICE_BOX, Y_GADGET_BOX,
      X_GADGET_BOX, Z_GADGET_BOX, FRACTION_BOX};

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
  if (!xyz || (!xyz->mFdataxz) || (!xyz->mFdatayz)) {
    wprint("Error:\n\tXYZ window can't open due to low memory\n");
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
    xyz->setMouseTracking(state || insertDown != 0);
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
  int pixSize = ivwGetPixelBytes(vi->rawImageStore);

  mXydata = mXzdata = mYzdata = NULL;
  for (ytop = 0; ytop < NUM_AXIS; ytop++)
    mDisplayedAxisLocation[ytop] = -100;

  /* DNM 1/19/02: need separate fdata for each side panel */
  mFdataxz  = (unsigned char *)malloc(vi->xsize * vi->zsize * pixSize);
  mFdatayz  = (unsigned char *)malloc(vi->ysize * vi->zsize * pixSize);
  if ((!mFdataxz) || (!mFdatayz)) {
    B3DFREE(mFdataxz);
    B3DFREE(mFdatayz);
    return;
  }

  mVi   = vi;
  mExposed = 0;
  mZoom = 1.0;
  mXtrans1 = 0;
  mYtrans1 = 0;
  mXtrans2 = 0;
  mYtrans2 = 0;
  mHq = ImodPrefs->startInHQ() ? 1 : 0;
  mProject = 0;
  mMousemode = IMOD_MMOVIE;
  mToolZoom = -1.0f;
  mToolMaxX = vi->xsize;
  mToolMaxY = vi->ysize;
  mToolMaxZ = vi->zsize;
  mXzFraction = ((float)vi->zsize) / (vi->xsize + vi->zsize);
  mYzFraction = ((float)vi->zsize) / (vi->ysize + vi->zsize);
  mLock = 0;
  mTimeLock = 0;
  mTimeDrawn = -1;
  
  mCtrlPressed = false;
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  setAnimated(false);

  if (!cenIcon) {
    cenIcon = new QIcon(QBitmap::fromData(QSize(BM_WIDTH, BM_HEIGHT),
                                          keepCenter_bits));
    utilBitListsToIcons(bitList, icons, MAX_XYZ_TOGGLES);
  }

  // Get the toolbar, add zoom arrows
  mToolBar = new HotToolBar(this);
  addToolBar(mToolBar);
  
  if (!TB_AUTO_RAISE)
    mToolBar->layout()->setSpacing(4);
  connect(mToolBar, SIGNAL(keyPress(QKeyEvent *)), this,
          SLOT(toolKeyPress(QKeyEvent *)));
  connect(mToolBar, SIGNAL(keyRelease(QKeyEvent *)), this,
          SLOT(toolKeyRelease(QKeyEvent *)));

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

  // Make simple pushbutton for centering
  utilTBToolButton(this, mToolBar, &button, "Center windows on current image"
                   " or model point (hot key k)");
  button->setIcon(*cenIcon);
  connect(button, SIGNAL(clicked()), this, SLOT(centerClicked()));

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
  
  mToolBar->setAllowedAreas(Qt::TopToolBarArea);

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
  // THIS DOESN'T WORK
  mToolBar->setWindowTitle(imodCaption("XYZ Toolbar"));

  mLx = mLy = mLz = mLastCacheSum = -1;

  mCtrl = ivwNewControl(vi, xyzDraw_cb, xyzClose_cb, xyzKey_cb, (void *)this);
  imodDialogManager.add((QWidget *)this, IMOD_IMAGE, XYZ_WINDOW_TYPE);
  
  // Determine needed window size, zoom that makes it mostly fit on the screen
  diaMaximumWindowSize(maxWinx, maxWiny);
  needWinx = vi->xsize + vi->zsize + ALL_BORDER;
  needWiny = vi->ysize + vi->zsize + ALL_BORDER;
  while (needWinx >  1.05 * maxWinx || needWiny > 1.05 * maxWiny) {
    newzoom = b3dStepPixelZoom(mZoom, -1);
    if (fabs(newzoom - mZoom) < 0.0001)
      break;
    mZoom = newzoom;
    needWinx = (int)((vi->xsize + vi->zsize) * mZoom + ALL_BORDER);
    needWiny = (int)((vi->ysize + vi->zsize) * mZoom + ALL_BORDER);
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
  imodShowHelpPage("xyz.html");
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

  mGLw->mClosing = true;
  e->accept();
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
int XyzWindow::Getxyz(int x, int y, float *mx, float *my, int *mz)
{
  int nx, ny, nz, cx, cy, cz;
  /* DNM 1/23/02: turn this from float to int to keep calling expressions
     as ints */
  float scale;
  
  y = mWiny - y;

  nx = (int)(mVi->xsize * mZoom);
  ny = (int)(mVi->ysize * mZoom);
  nz = (int)(mVi->zsize * mZoom);
  getLocation(cx, cy, cz);
  *mx = cx;
  *my = cy;
  *mz = cz;

  scale = 1.0/mZoom;
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
    *mx = (x - mXwoffset1) * scale ;
    *my = (y - mYwoffset1) * scale;
    //ivwBindMouse(mVi);
    return(Z_SLICE_BOX);
  }

  /* Click in top image, Y-Section */
  if (mouse_in_box(mXorigin1, mYorigin2, mXorigin1 + mWinXdim1,
                   mYorigin2 + mWinYdim2, x, y)) {
    //imodPrintStderr("found top image: x %d, y %d\n",x, y);
    *mx = (x - mXwoffset1) * scale;
    *mz = (int)((y - mYwoffset2) * scale);
    //ivwBindMouse(mVi);
    return(Y_SLICE_BOX);
  }

  /* Click in right image X-Section */
  if (mouse_in_box(mXorigin2, mYorigin1, mXorigin2 + mWinXdim2,
                   mYorigin1 + mWinYdim1, x, y)) {
    //imodPrintStderr("found right image: x %d, y %d\n",x, y);
    *my = (y - mYwoffset1) * scale;
    *mz = (int)((x - mXwoffset2) * scale);
    return(X_SLICE_BOX);
  }

  /* Z-Section Gadget */
  if (mouse_in_box(mXorigin2 - 1, mYorigin2 - 1,
                   mXorigin2 + mWinXdim2 + 1,
                   mYorigin2 + mWinYdim2 + 1, x, y)) {
    //imodPrintStderr("found z gadget: x %d, y %d\n",x, y);
    *mz = (int)(0.5 * ((y - mYwoffset2) + (x - mXwoffset2)) * scale);
    return(Z_GADGET_BOX);
  }
     
  /* X-Section Gadget (top gutter)*/
  if (mouse_in_box(mXorigin1 - 1, mYorigin1 + mWinYdim1,
                   mXorigin1 + mWinXdim1 + 1, mYorigin2, x, y)) {
    //imodPrintStderr("found top gutter: x %d, y %d\n",x, y);
    *mx = (x - mXwoffset1) * scale;
    return(X_GADGET_BOX);
  }
     
  /* Y-Section Gadget (right gutter)*/
  if (mouse_in_box(mXorigin1 + mWinXdim1, mYorigin1 -1, mXorigin2,
                   mYorigin1 + mWinYdim1 + 1, x, y)) {
    //imodPrintStderr("found right gutter: x %d, y %d\n",x, y);
    *my = (y - mYwoffset1) * scale;
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
  int box = Getxyz(x, y, &mx, &my, &mz);

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
  if (vi->imod->mousemode == IMOD_MMODEL && box == 3) {
    pnt.x = mx;
    pnt.y = my;
    pnt.z = mz;
    setLocation(mx, my, mz);
    distance = imodAllObjNearest(vi, &index , &pnt, selsize);
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
  int movie;
  Iobj  *obj;
  Icont *cont;
  Ipoint point;
  int pt;

  movie = Getxyz(x, y, &mx, &my, &mz);
  if (movie == NOT_IN_BOX)
    return;

  if (mVi->ax) {
    if (mVi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      autox_sethigh(mVi, (int)mx, (int)my);
      return;
    }
  }

  /* DNM 12/18/93: do not start movie in slider areas */
  if (mVi->imod->mousemode == IMOD_MMOVIE) {
    switch(movie) {
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
    if (movie <= Z_SLICE_BOX)
      imcSetStarterID(mCtrl);
    return;
  }

  if (movie != Z_SLICE_BOX)
    return;

  obj = imodObjectGet(mVi->imod);
  if (!obj)
    return;

  // DNM 7/10/04: switch to calling function for this
  // 11/17/04: have it take care of modifying time of empty contour
  cont = ivwGetOrMakeContour(mVi, obj, 0);
  if (!cont)
    return;

  /* Now if times still don't match refuse the point */
  // TODO: Time match
  if (ivwTimeMismatch(mVi, mTimeLock, obj, cont)) {
    wprint("\aContour time does not match current time.\n"
	   "Set contour time to 0 to model across times.\n");
    return;
  }

  /* DNM: don't make closed contours wild if they're not */
  if (cont->psize &&  iobjPlanar(obj->flags) && !(cont->flags & ICONT_WILD)
      && (int)floor(cont->pts[0].z + 0.5) != mz) {
    wprint("\aXYZ will not add a point on a different section to a co-planar"
           " %s contour.\n", iobjClose(obj->flags) ? "closed" : "");
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
  int movie;
  Icont *cont;
  Iobj *obj;
  int pt;

  movie = Getxyz(x, y, &mx, &my, &mz);
  if (movie == NOT_IN_BOX)
    return;

  if (mVi->ax) {
    if (mVi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      autox_setlow(mVi, (int)mx, (int)my);
      return;
    }
  }
     
     
  if (mVi->imod->mousemode == IMOD_MMOVIE) {
    switch(movie) {
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
    if (movie <= Z_SLICE_BOX)
      imcSetStarterID(mCtrl);
    return;
  }

  if (movie != Z_SLICE_BOX)
    return;

  
  obj = imodObjectGet(mVi->imod);
  cont = imodContourGet(mVi->imod);
  pt   = mVi->imod->cindex.point;
  if (!cont || !obj)
    return;
  if (pt < 0)
    return;
  if (!ivwPointVisible(mVi, &(cont->pts[pt])))
    return;

  if (ivwTimeMismatch(mVi, mTimeLock, obj, cont))
    return;

  mVi->undo->pointShift();
  cont->pts[pt].x = mx;
  cont->pts[pt].y = my;
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
  Ipoint point;
  double dist;
  int pt;

  if (mVi->ax) {
    if (mVi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      box = Getxyz(x, y, &mx, &my, &mz);
      if (box != 3)
        return;
      autox_sethigh(mVi, (int)mx, (int)my);
      return;
    }
  }

  if (mVi->imod->mousemode != IMOD_MMODEL)
    return;

  box = Getxyz(x, y, &mx, &my, &mz);
  if (box != 3)
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
  if (cont->psize &&  iobjPlanar(obj->flags) && !(cont->flags & ICONT_WILD)
      && cont->pts[0].z != mz)
    return;

  if (obj->extra[IOBJ_EX_PNT_LIMIT] &&
      cont->psize >= obj->extra[IOBJ_EX_PNT_LIMIT]) {
    B2Press(x, y);
    return;
  }
    
  point.x = mx;
  point.y = my;
  point.z = mz;

  dist = imodel_point_dist(&point, &(cont->pts[pt]));
  if (dist < scaleModelRes(mVi->imod->res, mZoom))
    return;

  ivwRegisterInsertPoint(mVi, cont, &point, pt + 1);
  finishNewModelPoint(mx, my, mz);
}

// Mouse button 2 drag, continuous modification
void XyzWindow::B3Drag(int x, int y)
{
  float mx, my;
  int mz, box;
  Iobj  *obj;
  Icont *cont;
  Ipoint point;
  double dist;
  int pt;

  if (mVi->ax) {
    if (mVi->ax->altmouse == AUTOX_ALTMOUSE_PAINT) {
      box = Getxyz(x, y, &mx, &my, &mz);
      if (box != 3)
        return;
      autox_setlow(mVi, (int)mx, (int)my);
      return;
    }
  }

  if (mVi->imod->mousemode != IMOD_MMODEL)
    return;

  box = Getxyz(x, y, &mx, &my, &mz);
  if (box != 3)
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

  dist = imodel_point_dist(&point, &(cont->pts[pt]));
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
  unsigned char **id;
  unsigned char *fdata;
  int cyi;
  int cx, cy, cz;
  int imdataxsize;
  unsigned char **imdata;
  int wx1, wx2, wy1, wy2;
  int xoffset1, xoffset2, yoffset1, yoffset2;
  int width1, height1, width2, height2, cacheSum, xslice, yslice;
  bool flipped;

  if (!mExposed)
    return;     /* DNM: avoid crashes if Zap is movieing*/

  /* DNM 3/5/01: changed to avoid very slow ivwGetValue when data are in
     cache; set up image pointer tables */
  // Keep track of a sum of Z values in the cache in order to detect 
  // Changes in available data that will require redisplay of XZ and YZ data
  // Load current Z section first to get it into cacheSum
  getLocation(cx, cy, cz);

  /* Get the time to display, flush images if it doesn't match last drawn */
  if (mTimeLock)
    time = mTimeLock;
  else
    ivwGetTime(mVi, &time);
  if (time != mTimeDrawn) {
    b3dFlushImage(mXydata);
    b3dFlushImage(mYzdata);
    b3dFlushImage(mXzdata);
  }
  mTimeDrawn = time;

  id = ivwGetZSectionTime(mVi, cz, time);
  if (ivwSetupFastAccess(mVi, &imdata, 0, &cacheSum, time))
    return;

  /* Just take the X size, do not allow for possibility of cached data 
     having different X sizes */
  imdataxsize = mVi->xsize;

  if (mVi->vmSize) {
    mLx = mLy = -1;
  }
          
  utilClearWindow(App->background);

  cyi = cy * nx;
  flipped = !mVi->fakeImage && (!mVi->vmSize || mVi->fullCacheFlipped) && 
    mVi->li->axis == 2;

  /* Now compute drawing parameters for each of the subareas */
  /* Pass the image offset routine the effective image size, including
     de-zoomed borders, and convert a data offset into a negative window
     offset for use in drawing model */
  b3dSetImageOffset(mWinXdim1, nx, mZoom, width1, mXtrans1, wx1, xoffset1, 0);
  b3dSetImageOffset(mWinXdim2, nz, mZoom, width2, mXtrans2, wx2, xoffset2, 0);
  b3dSetImageOffset(mWinYdim1, ny, mZoom, height1, mYtrans1, wy1, yoffset1, 0);
  b3dSetImageOffset(mWinYdim2, nz, mZoom, height2, mYtrans2, wy2, yoffset2, 0);
  
  wx1 += mXorigin1;
  wx2 += mXorigin2;
  wy1 += mYorigin1;
  wy2 += mYorigin2;
  
  if (xoffset1)
    mXwoffset1 = -B3DNINT(xoffset1 * mZoom + 0.5) + mXorigin1;
  else
    mXwoffset1 = wx1;
  if (xoffset2)
    mXwoffset2 = -B3DNINT(xoffset2 * mZoom + 0.5) + mXorigin2;
  else
    mXwoffset2 = wx2;
  if (yoffset1)
    mYwoffset1 = -B3DNINT(yoffset1 * mZoom + 0.5) + mYorigin1;
  else
    mYwoffset1 = wy1;
  if (yoffset2)
    mYwoffset2 = -B3DNINT(yoffset2 * mZoom + 0.5) + mYorigin2;
  else
    mYwoffset2 = wy2;
  
  //draw XY view
  if (width1 > 0 && height1 > 0) {
    mLz = cz;
    b3dDrawGreyScalePixelsHQ(id, nx,ny, xoffset1, yoffset1, wx1, wy1, width1, height1,
                             mXydata, mVi->rampbase, mZoom, mZoom, mHq, cz, App->rgba);
  }

  // Load data for YZ view
  // Send out a negative xslice or yslice if the data are being reloaded,
  // this is the best way to signal that they are new to the matching routine
  if (width2 > 0 && height1 > 0) {
    xslice = cx;
    fdata  = mFdatayz;
    if (cx != mLx || cacheSum != mLastCacheSum) {
      xslice = -1 - cx;
      mLx = cx;
      if (flipped && !mVi->fakeImage) {
        for (y = 0; y < ny; y++) {
          if (imdata[y]) {
            if (mVi->rawImageStore) {
              for (z = 0; z < nz; z++) {
                fdata[3 * (z + y * nz)] = imdata[y][3 * (cx + (z * imdataxsize))];
                fdata[3 * (z + y * nz) + 1] = imdata[y][3 * (cx + (z * imdataxsize)) + 1];
                fdata[3 * (z + y * nz) + 2] = imdata[y][3 * (cx + (z * imdataxsize)) + 2];
              }
            } else
              for (z = 0; z < nz; z++) 
                fdata[z + y * nz] = imdata[y][cx + (z * imdataxsize)];
          } else {
            if (mVi->rawImageStore) {
              for (z = 0; z < nz; z++) {
                fdata[3 * (z + y * nz)] = 0;
                fdata[3 * (z + y * nz) + 1] = 0;
                fdata[3 * (z + y * nz) + 2] = 0;
              }
            } else
              for (z = 0; z < nz; z++) 
                fdata[z + y * nz] = 0;
          }
        }
      } else {
        for(z = 0; z < nz; z++) {
          if (!mVi->fakeImage && imdata[z]) {
            if (mVi->rawImageStore) {
              for (i = z, y = 0; y < ny; y++, i += nz) {
                fdata[3 * i] = imdata[z][3 *(cx + (y * imdataxsize))];
                fdata[3 * i + 1] = imdata[z][3 *(cx + (y * imdataxsize))+ 1];
                fdata[3 * i + 2] = imdata[z][3 *(cx + (y * imdataxsize))+ 2];
              }
            } else
              for (i = z, y = 0; y < ny; y++, i += nz)
                fdata[i] = imdata[z][cx + (y * imdataxsize)];
          } else {
            if (mVi->rawImageStore)
              for (i= z, y = 0; y < ny; y++, i += nz)
                fdata[3 * i] = fdata[3 * i + 1] = fdata[3 * i + 2] = 0;
            else
              for (i= z, y = 0; y < ny; y++, i += nz)
                fdata[i] = 0;
          }
        }
      }
    }
    
    //draw yz view
    b3dDrawGreyScalePixelsHQ(ivwMakeLinePointers(mVi, mFdatayz, nz, ny, 
                                                 mVi->rawImageStore), 
                             nz, ny, xoffset2, yoffset1, wx2, wy1, width2, height1,
                             mYzdata, mVi->rampbase, mZoom, mZoom, mHq, xslice, 
                             App->rgba);
  }

  // Load data for XZ view
  if (width1 > 0 && height2 > 0) {
    yslice = cy;
    fdata  = mFdataxz;
    if (cy != mLy || cacheSum != mLastCacheSum) {
      yslice = -1 - cy;
      mLy = cy;
      for(i = 0,z = 0; z < nz; z++) {
        if (flipped && !mVi->fakeImage && imdata[cy]) {
          if (mVi->rawImageStore) {
            for(x = 0; x < nx; x++, i++) {
              fdata[3 * i] = imdata[cy][3 * (x + (z * imdataxsize))];
              fdata[3 * i + 1] = imdata[cy][3 * (x + (z * imdataxsize)) + 1];
              fdata[3 * i + 2] = imdata[cy][3 * (x + (z * imdataxsize)) + 2];
            }
          } else
            for(x = 0; x < nx; x++, i++)
              fdata[i] = imdata[cy][x + (z * imdataxsize)];
        } else if (!flipped && !mVi->fakeImage && imdata[z]) {
          if (mVi->rawImageStore) {
            for(x = 0; x < nx; x++, i++) {
              fdata[3 * i] = imdata[z][3 * (x + (cy * imdataxsize))];
              fdata[3 * i + 1] = imdata[z][3 * (x + (cy * imdataxsize)) + 1];
              fdata[3 * i + 2] = imdata[z][3 * (x + (cy * imdataxsize)) + 2];
            }
          } else
            for(x = 0; x < nx; x++, i++)
              fdata[i] = imdata[z][x + (cy * imdataxsize)];
        } else {
          if (mVi->rawImageStore)
            for(x = 0; x < nx; x++, i++)
              fdata[3 * i] = fdata[3 * i + 1] = fdata[3 * i + 2] = 0;
          else
            for(x = 0; x < nx; x++, i++)
              fdata[i] = 0;
        }
      }
    }

    //draw xz view    
    b3dDrawGreyScalePixelsHQ(ivwMakeLinePointers(mVi, mFdataxz, nx, nz,
                                                 mVi->rawImageStore),
                             nx, nz, xoffset1, yoffset2,
                             wx1, wy2, width1, height2, mXzdata,
                             mVi->rampbase, mZoom, mZoom, 
                             mHq, yslice, App->rgba);
  }
  mLastCacheSum = cacheSum;
}

/*
 * Draw the lines around boxes and the position gadgets
 */
void XyzWindow::DrawCurrentLines()
{
  int cx, cy, cz, cenx, ceny, xlim, ylim, cenxlim, cenylim;
  float z = mZoom;
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
  cenx = (int)(bx2 + z * (cz+.5));
  ceny = (int)(by2 + z * (cz+.5));
  xlim = (int)(bx2 + z * nz);
  ylim = (int)(by2 + z * nz);
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
  cenx = (int)(bx + z * (cx+.5));
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
  ceny = (int)(by + z * (cy+.5));
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
          b3dDrawLine((int)(z * thisPt->x + bx),
                      (int)(z * thisPt->y + by),
                      (int)(z * lastPt->x + bx),
                      (int)(z * lastPt->y + by));
        ptProps.gap = 0;
        if (nextChange == pt)
          nextChange = ifgHandleNextChange(obj, cont->store, &contProps, 
                                           &ptProps, &stateFlags, 
                                           &changeFlags, handleFlags, 0);
        if (thisVis && ptProps.symtype != IOBJ_SYM_NONE &&
            !(ptProps.gap && ptProps.valskip))
          utilDrawSymbol((int)(z * thisPt->x + bx), 
                         (int)(z * thisPt->y + by),
                         ptProps.symtype, ptProps.symsize, 
                         ptProps.symflags);
        
        lastVis = thisVis;
        lastPt = thisPt;
      }

      /* Close if all conditions are met */
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN) && !currentCont
          && lastVis && ((int)floor(cont->pts->z + 0.5) == currentZ) && 
          !ptProps.gap)
        b3dDrawLine((int)(z * cont->pts->x + bx),
                    (int)(z * cont->pts->y + by),
                    (int)(z * lastPt->x + bx),
                    (int)(z * lastPt->y + by));

    } else if (lastVis) {

      /* If the contour is not wild or fine grained and it is on this section,
         draw it completely, close if appropriate, and draw symbols in
         separate loop */
      b3dBeginLine();
      for (pt = 0; pt < cont->psize; pt++)
        b3dVertex2i((int)(z * cont->pts[pt].x + bx),  
                    (int)(z * cont->pts[pt].y + by));
      if (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN) && !currentCont)
        b3dVertex2i((int)(bx + z * cont->pts->x), 
                    (int)(by + z * cont->pts->y));
      b3dEndLine();
            
      if (ptProps.symtype != IOBJ_SYM_NONE)
        for (pt = 0; pt < cont->psize; pt++)
          utilDrawSymbol((int)(z * cont->pts[pt].x + bx), 
                        (int)(z * cont->pts[pt].y + by),
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
  float z = mZoom;

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
      utilDrawSymbol((int)(z * point[indx] + bx), 
                     (int)(z * point[indy] + by),
                     ptProps->symtype, ptProps->symsize, 
                     ptProps->symflags);

    /* connecting line if in plane or if projecting current cont, and not 
       last point unless closure is appropriate */
    nexpt = (float *)(&cont->pts[next]);
    if (((thisVis && (int)(nexpt[indz]) == currentZ) || 
         (currentCont && mProject)) && !ptProps->gap && 
        (next || (!currentCont &&
                  (iobjClose(obj->flags) && !(cont->flags & ICONT_OPEN)))))
      b3dDrawLine((int)(z * point[indx] + bx),
                  (int)(z * point[indy] + by),
                  (int)(z * nexpt[indx] + bx),
                  (int)(z * nexpt[indy] + by));
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
  float *point;
  int changeFlags, radius;
  int currentZ = zmouse;
  float z = mZoom;
  float drawsize, delz;

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
          utilDrawSymbol((int)(z * point[indx] + bx), 
                         (int)(z * point[indy] + by),
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
          b3dDrawCircle((int)(bx + z * point[indx]),
                        (int)(by + z * point[indy]), (int)(z * drawsize));
          if (drawsize > 3)
            b3dDrawPlus((int)(bx + z * point[indx]),
                        (int)(by + z * point[indy]), 3);
        } else if (drawsize > 1 && !(obj->flags & IMOD_OBJFLAG_PNT_ON_SEC)) {

          /* for off-section, compute size of circle and
             draw a smaller circ if further away. */
          delz = (point[indz] - zmouse) * zscale;
          if (delz < 0)
            delz = -delz;
          
          if (delz < drawsize - 0.01) {
            radius = (int)(sqrt((double)(drawsize * drawsize - delz * delz))
                           * z);
            b3dDrawCircle((int)(bx + z * point[indx]),
                          (int)(by + z * point[indy]), radius);
          }
        }
      }
    }
  }

  /* draw end markers if requested */
  b3dLineWidth(contProps->linewidth2);
  if (obj->symflags & IOBJ_SYMF_ENDS) {
    b3dColorIndex(App->bgnpoint);
    point = (float *)cont->pts;

    for (pt = 0; pt < 2; pt ++) {
      testz = indz == 2 ? B3DNINT(point[indz]) : (int)point[indz];
      if (testz == currentZ)
        b3dDrawCross((int)(bx + z * point[indx]),
                     (int)(by + z * point[indy]), obj->symsize/2);
      
      b3dColorIndex(App->endpoint);
      point = (float*)(&cont->pts[cont->psize-1]);
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
  float z = mZoom;
  int bx = mXwoffset1;
  int by = mYwoffset1;
  int bx2 = mXwoffset2;
  int by2 = mYwoffset2;
  int imPtSize, modPtSize, backupSize;

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
        b3dSubareaViewport(mXorigin1, mYorigin1, mWinXdim1, 
                           mWinYdim1);
        b3dDrawCircle((int)(z * point->x+bx),
                      (int)(z * point->y+by), modPtSize);
      }
      if ((int)point->y == cy) {
        b3dSubareaViewport(mXorigin1, mYorigin2, mWinXdim1,
                           mWinYdim2);
        b3dDrawCircle((int)(z * point->x+bx),
                      (int)(z * (point->z + 0.5) + by2), modPtSize);
      }
      if ((int)point->x == cx) {
        b3dSubareaViewport(mXorigin2, mYorigin1, mWinXdim2,
                           mWinYdim1);
        b3dDrawCircle((int)(z * (point->z + 0.5) + bx2),
                      (int)(z * point->y+by), modPtSize);
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
    b3dDrawCircle((int)(z * pnt->x+bx), (int)(z * pnt->y+by), psize);
    b3dColorIndex(App->curpoint);
    b3dSubareaViewport(mXorigin1, mYorigin2, mWinXdim1, mWinYdim2);
    b3dDrawPlus((int)(z*pnt->x+bx), (int)(z* (cz + 0.5) + by2), psize);
    b3dSubareaViewport(mXorigin2, mYorigin1, mWinXdim2, mWinYdim1);
    b3dDrawPlus((int)(z * (cz + 0.5) + bx2), (int)(by+z*pnt->y), psize);
  } else {
    b3dColorIndex(App->curpoint);
    b3dSubareaViewport(mXorigin1, mYorigin1, mWinXdim1, mWinYdim1);
    b3dDrawPlus((int)(z*(cx+.5)+bx), (int)(z*(cy+.5)+by), imPtSize);
    b3dSubareaViewport(mXorigin1, mYorigin2, mWinXdim1, mWinYdim2);
    b3dDrawPlus((int)(z*(cx+.5)+bx), (int)(z*(cz+.5)+by2), imPtSize);
    b3dSubareaViewport(mXorigin2, mYorigin1, mWinXdim2, mWinYdim1);
    b3dDrawPlus((int)(bx2+z*(cz+.5)), (int)(by+z*(cy+.5)), imPtSize);
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

  int keysym = event->key();
  int shifted = event->modifiers() & Qt::ShiftModifier;
  int ctrl = event->modifiers() & Qt::ControlModifier;
  
  int ix, iy, rx;
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

    // It wouldn't work going to a QPoint and accessing it, so do it in shot!
    ix = (mGLw->mapFromGlobal(QCursor::pos())).x();
    iy = (mGLw->mapFromGlobal(QCursor::pos())).y();

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
    if (!keypad && mLock) {
      if (keysym == Qt::Key_PageUp || keysym == Qt::Key_PageDown)
        setLocation(NOTNEW, NOTNEW, mZlock + (keysym == Qt::Key_PageUp ? 1 : -1));
      else if (keysym == Qt::Key_Left || keysym == Qt::Key_Right)
        setLocation(mXlock + (keysym == Qt::Key_Right ? 1 : -1), NOTNEW, NOTNEW);
      else
        setLocation(NOTNEW, mYlock + (keysym == Qt::Key_Up ? 1 : -1), NOTNEW);
      Draw();
    } else
      handled = 0;
    break;

  case Qt::Key_1:
  case Qt::Key_2:
    if (mTimeLock) {
      mTimeLock += (keysym == Qt::Key_1) ? -1 : 1;
      mTimeLock = B3DMAX(1, B3DMIN(mVi->nt, mTimeLock));
      Draw();
    } else 
      handled = 0;
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
    mWin->mWhichbox = mWin->Getxyz(event->x(), event->y(), &mx, &my, &mz);

  } else if (event->button() == ImodPrefs->actualButton(2) &&
	     !button1 && !button3) {
    mWin->B2Press(event->x(), event->y());

  } else if (event->button() == ImodPrefs->actualButton(3) &&
	     !button1 && !button2) {
    mWin->B3Press(event->x(), event->y());
  }

  mWin->mLmx = event->x();
  mWin->mLmy = event->y();
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
    whichbox = mWin->Getxyz(ex, ey, &mx, &my, &mz);
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
    cumdx = mWin->mLmx - ex;
    cumdy = mWin->mLmy - ey;
    if (but1downt.elapsed() > 250 || cumdx * cumdx + cumdy * cumdy > cumthresh)
      mWin->B1Drag(ex, ey);
    else if (mWin->mWhichbox == 7)
      return;
  }
  if ( (!button1) && (button2) && (!button3))
    mWin->B2Drag(ex, ey);
  if ( (!button1) && (!button2) && (button3))
    mWin->B3Drag(ex, ey);
  
  mWin->mLmx = ex;
  mWin->mLmy = ey;
}

/*

$Log$
Revision 4.62  2011/02/13 21:33:24  mast
Moved structure into window class, allowed multiple instances, added lock button

Revision 4.61  2011/02/12 05:10:27  mast
Start in HQ mode if preference set

Revision 4.60  2011/01/13 20:26:24  mast
warning cleanup

Revision 4.59  2010/04/01 02:41:48  mast
Called function to test for closing keys, or warning cleanup

Revision 4.58  2010/02/22 21:34:07  mast
Stop drawing points below threshold

Revision 4.57  2009/04/06 19:37:31  mast
Changes to preserve model cursor in Mac Qt 4.5 when raising window

Revision 4.56  2009/03/30 18:26:20  mast
Call function to raise on mouse press if needed

Revision 4.55  2009/03/22 19:42:16  mast
Changes for cocoa/10.5: fill toolbar edit box 3 times

Revision 4.54  2009/03/05 00:59:16  mast
Flush mouse move events to get to most recent one when appropriate

Revision 4.53  2009/01/15 16:33:18  mast
Qt 4 port

Revision 4.52  2008/09/24 02:40:45  mast
Call new attach function; stop drawing objects that are off

Revision 4.51  2008/08/19 20:01:40  mast
Made it zoom with + as well as =

Revision 4.50  2008/07/16 04:29:33  mast
Made drag drawing respect contour limit

Revision 4.49  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 4.48  2008/01/19 22:24:28  mast
Fixed problems when no objects

Revision 4.47  2008/01/13 22:58:35  mast
Changes for multi-Z window

Revision 4.46  2007/12/04 18:43:24  mast
Changes for stippling and using new util functions

Revision 4.45  2007/11/16 23:18:54  mast
Added ability to adjust balance between the panel sizes, added simple
centering button, fixed behavior with pixel view open, fixed panning
to start when mouse has moved far enough.

Revision 4.44  2007/07/13 17:29:47  sueh
bug# 1023 Added mFirstDraw and mTimerID to fix a first draw problem.
In GetCIImages, allocating space based on the size of the views.

Revision 4.43  2007/07/13 14:50:18  mast
Cleanup and commented

Revision 4.42  2007/07/13 14:44:02  mast
cleanup attempt # 1

Revision 4.41  2007/07/13 05:34:55  mast
Got viewports working in all draw actions, fixed insert key modeling,
set geometry of window to fit toolbar, called routine to refigure the
window layout when flip occurs

Revision 4.40  2007/07/12 19:47:48  sueh
bug# 1023 Corrected panning.  Synchronized panning of the top and left windows
by synchronizing the height of the top window with the width of the left 
window.  Fixed the slicer lines.

Revision 4.38  2007/06/30 00:42:53  sueh
bug# 1021 Updating the slider ranges and sizes on draw, in case a flip is done.
Labeled the toolbar and limited it docking options.

Revision 4.37  2007/06/29 21:09:35  sueh
bug# 1021 Replacing the Z slider with a multi-slider that shows X, Y, and
Z sliders.

Revision 4.36  2007/06/27 21:57:40  sueh
bug# 1021 Added slider.

Revision 4.35  2007/06/26 21:57:43  sueh
bug# 1021 Removed win_support.  Added functions for zooming and the
zoom edit box.

Revision 4.34  2007/06/26 17:07:29  sueh
bug# 1021 Added a button toolbar with a high-resolution button and zoom
arrows.

Revision 4.33  2007/06/08 04:52:55  mast
Added protection for planar open contours

Revision 4.32  2006/10/11 20:13:24  mast
Reject mouse event if window closing from escape

Revision 4.31  2006/10/05 15:41:32  mast
Provided for primary and second non-TIFF snapshot format

Revision 4.30  2006/09/17 18:15:59  mast
Changes to provide mouse position to pixelview

Revision 4.29  2006/08/31 23:27:45  mast
Changes for stored value display

Revision 4.28  2006/07/03 04:14:21  mast
Changes for beadfixer overlay mode

Revision 4.27  2006/06/09 20:25:39  mast
Added ability to display spheres on center section only

Revision 4.26  2005/06/26 19:38:10  mast
Added logic for fine-grained changes

Revision 4.25  2005/03/20 19:55:37  mast
Eliminating duplicate functions

Revision 4.24  2004/11/21 05:50:34  mast
Switch from int to float for nearest point distance measurement

Revision 4.23  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 4.22  2004/11/02 20:16:34  mast
Switched to using curpoint color for current point

Revision 4.21  2004/11/01 22:56:51  mast
Kept floating point positions, made res zoom-dependent

Revision 4.20  2004/09/10 02:31:04  mast
replaced long with int

Revision 4.19  2004/07/11 18:27:53  mast
Made it use new function for getting contour to add points

Revision 4.18  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.17  2004/05/03 19:19:10  mast
fixed bug in test for presence of data on one plane

Revision 4.16  2004/01/05 18:35:15  mast
Changes to deal with flipped cache properly and to scale point size by
binning

Revision 4.15  2003/12/18 22:46:26  mast
Register with movie controller when start movie

Revision 4.14  2003/09/16 02:11:30  mast
Changed to access image data using new line pointers

Revision 4.13  2003/05/06 02:18:45  mast
Fixed problem with displaying z = -1 on first section

Revision 4.12  2003/04/25 03:28:33  mast
Changes for name change to 3dmod

Revision 4.11  2003/04/18 20:16:39  mast
Rename meta test function

Revision 4.10  2003/04/18 20:06:28  mast
Reject the Ctrl (meta) key on the Mac

Revision 4.9  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.8  2003/03/25 23:01:43  mast
Take nearest int in checking for current point Z value when adding points

Revision 4.7  2003/03/24 17:56:46  mast
Register with dialogManager so it can be parked with info window

Revision 4.6  2003/03/13 01:14:46  mast
Pass Ctrl R on to default keys

Revision 4.5  2003/03/12 21:35:23  mast
Test if no CIImage is returned and give error message

Revision 4.4  2003/03/12 06:36:18  mast
Fixed problem of adding or modifying contours at the wrong times

Revision 4.3  2003/03/07 15:49:55  mast
Fixed bug in drawing curretn point when no current contour

Revision 4.2  2003/03/03 22:09:49  mast
Added grab bars to the sliders and color coded the sliders and boxes.
Added ability to display spheres for any objects with point sizes.
Made all points and connecting lines display in XZ and YZ windows, with
the projection of the current contour toggleable with P.
Implemented dynamic sizes for current point and end point markers.
Eliminated separate routine for drawing current contour.

Revision 4.1  2003/02/10 20:29:03  mast
autox.cpp

Revision 1.1.2.11  2003/01/29 01:34:23  mast
implement colormaps

Revision 1.1.2.10  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.9  2003/01/23 20:13:33  mast
add include of imod_input

Revision 1.1.2.8  2003/01/13 01:15:43  mast
changes for Qt version of info window

Revision 1.1.2.7  2003/01/06 15:50:47  mast
Use imodCaption and viewport xy routine

Revision 1.1.2.6  2003/01/03 16:46:18  mast
Simplified closing logic

Revision 1.1.2.5  2003/01/02 15:43:37  mast
accept key input from controlled; use a cache sum to detect if xz and yz
data need redrawing

Revision 1.1.2.4  2002/12/14 05:23:42  mast
backing out the fancy subclass, adjusting for new visual detection

Revision 1.1.2.3  2002/12/13 07:09:19  mast
GLMainWindow needed different name for mouse event processors

Revision 1.1.2.2  2002/12/13 06:06:29  mast
using new glmainwindow and mainglwidget classes

Revision 1.1.2.1  2002/12/12 02:41:10  mast
Qt version

Revision 3.6  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

Revision 3.5  2002/11/27 03:22:12  mast
Changed argumnet 3 of xyz_draw_cb from long to int to avoid warnings

Revision 3.4  2002/11/25 19:23:38  mast
Made it add itself to list of controls, and restructured the
structure for closing the window to accomodate that change.

Revision 3.3  2002/01/29 03:11:47  mast
Fixed bug in xxyz_draw from accessing elements of xx before xx existence
test

Revision 3.2  2002/01/28 16:58:52  mast
Major enhancements: Made it use the same image drawing code as
the Zap window so that it would not be slow with fractional zooms; added
ability to display in high-resolution mode and to take snapshots of the
window; added ability to pan the window with the mouse; made the model
display have fixed line widths and symbol sizes independent of zoom; made
attachment to the nearest model point work just like in the Zap window;
added ability to riffle through images by dragging the current point
indicators with the mouse.
Note that the use of the b3dDrawGreyScalePixelsHQ routine is incompatible
with the now-obsolete PIXEL_DRAW_HACK required with Nvidia 0.9.5 drivers.
Removed non OpenGL code for readability.

Revision 3.1  2001/12/17 18:51:49  mast
Removed call to autox_build

*/
