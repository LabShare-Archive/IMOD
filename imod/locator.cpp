/*
 *  locator.cpp: A window for showing and manipulating Zap winodw area
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <qcursor.h>
#include <qtooltip.h>
#include <qlabel.h>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <qpushbutton.h>
//Added by qt3to4:
#include <QCloseEvent>
#include <QTimerEvent>
#include <QMouseEvent>
#include <QKeyEvent>

#include "arrowbutton.h"
#include "dia_qtutils.h"
#include "imod.h"
#include "imod_input.h"
#include "imod_info_cb.h"
#include "b3dgfx.h"
#include "locator.h"
#include "xzap.h"
#include "preferences.h"
#include "control.h"

static void locatorClose_cb(ImodView *vi, void *client, int junk);
static void locatorDraw_cb(ImodView *vi, void *client, int drawflag);

// The resident pointers to the single instance
static LocatorWindow *LocWin = NULL;
static LocatorGL *GLw = NULL;

/*
 * Open the locator window
 */
int locatorOpen(ImodView *vi)
{
  float targetZoom = 6.;
  int minSize = 128;
  int maxSize = 512;
  int winx, winy;
  if (LocWin) {
    LocWin->raise();
    return 0;
  }
  LocWin = new LocatorWindow(App->rgba, App->doublebuffer, App->qtEnableDepth,
                             imodDialogManager.parent(IMOD_IMAGE));
  if (!LocWin) {
    wprint("\aError opening locator window.\n");
    return(-1);
  }
  GLw->mVi = vi;
  if (!App->rgba)
    GLw->setColormap(*(App->qColormap));
  LocWin->setWindowTitle(imodCaption("3dmod Locator"));
  LocWin->mCtrl = ivwNewControl(vi, locatorDraw_cb, locatorClose_cb, NULL,
                                NULL);
  imodDialogManager.add((QWidget *)LocWin, IMOD_IMAGE);

  // Set the window size zoomed down with size between limits
  imod_info_input();
  if (vi->xsize > vi->ysize) {
    winx = B3DMAX(minSize, B3DMIN(maxSize, vi->xsize / targetZoom));
    winx = B3DMIN(vi->xsize, winx);
    winy = (winx * vi->ysize + vi->xsize - 1) / vi->xsize;
  } else {
    winy = B3DMAX(minSize, B3DMIN(maxSize, vi->ysize / targetZoom));
    winy = B3DMIN(vi->ysize, winy);
    winx = (winy * vi->xsize + vi->ysize - 1) / vi->ysize;
  }
  winy += LocWin->height() - GLw->height();
  LocWin->resize(winx, winy);
  adjustGeometryAndShow((QWidget *)LocWin, IMOD_IMAGE, false);
  return 0;
}

/*
 * External command to close
 */
static void locatorClose_cb(ImodView *vi, void *client, int junk)
{
  if (LocWin)
    LocWin->close();
}

/*
 * The callback to draw the window
 */
static void locatorDraw_cb(ImodView *vi, void *client, int drawflag)
{
  if (!LocWin)
    return;
  if (drawflag & IMOD_DRAW_IMAGE)
    b3dFlushImage(GLw->mImage);
  GLw->drawIfNeeded(drawflag);
}

/*
 * External call to schedule a draw if window is open
 */
void locatorScheduleDraw(ImodView *vi)
{
  if (!LocWin)
    return;
  GLw->scheduleDraw();
}


/*
 * Constructor for the locator class: get the GL window, set up widgets
 */
LocatorWindow::LocatorWindow(bool rgba, bool doubleBuffer, bool enableDepth, 
                QWidget * parent, Qt::WFlags f)
  : QMainWindow(parent, f)
{
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  QWidget *central = new QWidget(this);
  QVBoxLayout *cenlay = new QVBoxLayout(central);
  cenlay->setContentsMargins(0,0,0,0);
  setCentralWidget(central);
  QHBoxLayout *topHBox = diaHBoxLayout(cenlay);
  topHBox->setContentsMargins(4,4,4,4);
  topHBox->setSpacing(4);

  ArrowButton *arrow = new ArrowButton(Qt::UpArrow, central, "zoomup button");
  topHBox->addWidget(arrow);
  connect(arrow, SIGNAL(clicked()), this, SLOT(zoomUp()));
  arrow->setToolTip("Increase window size by 1.5 (hot key =)");
  arrow = new ArrowButton(Qt::DownArrow, central, "zoom down button");
  topHBox->addWidget(arrow);
  connect(arrow, SIGNAL(clicked()), this, SLOT(zoomDown()));
  arrow->setToolTip("Decrease window size by 1.5 (hot key -)");
  mZoomLabel = diaLabel("Zoom 1.00", central, topHBox);

  // Help button
  mHelpButton = diaPushButton("Help", central, topHBox);
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(help()));
  setFontDependentWidths();
  topHBox->addStretch();

  QGLFormat glFormat;
  glFormat.setRgba(rgba);
  glFormat.setDoubleBuffer(doubleBuffer);
  glFormat.setDepth(enableDepth);
  GLw = new LocatorGL(glFormat, central);
  cenlay->addWidget(GLw);
  setFocusPolicy(Qt::StrongFocus);
  cenlay->setStretchFactor(GLw, 1);
  adjustSize();
}

void LocatorWindow::setFontDependentWidths()
{
  diaSetButtonWidth(mHelpButton, ImodPrefs->getRoundedStyle(), 1.2, "Help");
}

void LocatorWindow::closeEvent ( QCloseEvent * e )
{
  ivwRemoveControl(GLw->mVi, mCtrl);
  imodDialogManager.remove((QWidget *)LocWin);
  b3dFreeCIImage(GLw->mImage);
  LocWin = NULL;
  e->accept();
}

void LocatorWindow::help()
{
  imodShowHelpPage("locator.html");
}

void LocatorWindow::zoomUp()
{
  GLw->changeSize(1.5);
}
void LocatorWindow::zoomDown()
{
  GLw->changeSize(0.6667);
}

/*
 * Respond to a key input 
 */
void LocatorWindow::keyPressEvent ( QKeyEvent * event )
{
  int handled = 1;
  int key = event->key();
  if (utilCloseKey(event)) {
    close();
    return;
  }

  switch(key){
  case Qt::Key_Minus:
    GLw->changeSize(0.6667);
    break;

  case Qt::Key_Plus:
  case Qt::Key_Equal:
    GLw->changeSize(1.5);
    break;

  default:
    handled = 0;
    break;
  }

  // If key not handled, call the default processor
  if (!handled)
    inputQDefaultKeys(event, GLw->mVi);
}

/*
 * Constructor for GL class: just initialize variables
 */
LocatorGL::LocatorGL(QGLFormat format, QWidget * parent)
  : QGLWidget(format, parent)
{
  mImage = NULL;
  mFirstDraw = true;
  mTimerID = 0;
  mCursorSet = 0;
  mBlack = mWhite = -1;
}

/*
 * The window is resizing - set viewport and get data array
 */
void LocatorGL::resizeGL( int wdth, int hght )
{
  mWinx = wdth;
  mWiny = hght;
  b3dSetCurSize(mWinx, mWiny);
  b3dResizeViewportXY(wdth, hght);
  mImage   = b3dGetNewCIImageSize(mImage, App->depth, wdth, hght);
  if (!mImage) {
    wprint("\aInsufficient memory to run the locator window.\n"
           "Please close it\n");
    return;
  }
}

/*
 * Change window size by a factor - simple-minded zoom
 */
void LocatorGL::changeSize(float factor)
{
  int newx = B3DMAX(16, (int)(factor * mWinx));
  int newy = B3DMAX(16, (int)(factor * mWiny)) + LocWin->height() - height();
  LocWin->resize(newx, newy);
}

/*
 * Do a draw if anything seems to have changed: section, area, contrast, time
 */
void LocatorGL::drawIfNeeded(int drawflag)
{
  int time, section, ix0, iy0, nxUse, nyUse, subret;
  ivwGetTime(mVi, &time);
  section = B3DNINT(mVi->zmouse);
  subret = zapSubsetLimits(mVi, ix0, iy0, nxUse, nyUse);
  if (section != mSection || time != mTime || mVi->li->axis != mLastAxis ||
      subret != mLastSubRet || (drawflag & IMOD_DRAW_IMAGE) ||
      mVi->black != mBlack || mVi->white != mWhite ||
      (!subret && (ix0 != mLastX0 || iy0 != mLastY0 || nxUse != mLastNx || 
                   nyUse != mLastNy)))
    updateGL();
}

/*
 * Start a timer to do a draw - this is called from inside Zap draws
 */
void LocatorGL::scheduleDraw()
{
  if (mTimerID)
    return;
  mTimerID = startTimer(10);
}

/*
 * Timer fires: redo first draw or do draw if needed 
 */
void LocatorGL::timerEvent(QTimerEvent * e )
{
  killTimer(mTimerID);
  mTimerID = 0;
  if (mFirstDraw) {
    mFirstDraw = false;
    updateGL();
  } else
    drawIfNeeded(IMOD_DRAW_XYZ);
}

/*
 * The paint routine called by Qt
 */
void LocatorGL::paintGL()
{
  int time, xdraw, ydraw;
  unsigned char **imageData;

  // Schedule a second draw the first time
  if (mFirstDraw && !mTimerID)
    mTimerID = startTimer(10);

  // Get zoom and drawing limits, figure out whether to flush the image
  b3dSetCurSize(mWinx, mWiny);
  mZoom = B3DMIN((double)mWinx / mVi->xsize, (double)mWiny / mVi->ysize);
  xdraw = (int)(mZoom * mVi->xsize);
  ydraw = (int)(mZoom * mVi->ysize);
  mXborder = (mWinx - xdraw) / 2;
  mYborder = (mWiny - ydraw) / 2;
  mSection = B3DNINT(mVi->zmouse);
  ivwGetTime(mVi, &time);
  if (time != mTime || mVi->black != mBlack || mVi->white != mWhite)
    b3dFlushImage(mImage);
  mTime = time;
  mLastAxis = mVi->li->axis;
  mBlack = mVi->black;
  mWhite = mVi->white;

  // Get image data, draw outside box and image
  imageData = ivwGetZSectionTime(mVi, mSection, time);
  b3dDrawBoxout(mXborder, mYborder, mXborder + xdraw, mYborder + ydraw);
  /* imodPrintStderr("%d %d %d %d %d %d\n", mXborder, mYborder, xdraw, ydraw,
     mWinx, mWiny); */
  b3dDrawGreyScalePixelsHQ(imageData, mVi->xsize, mVi->ysize, 0, 0,
                           mXborder, mYborder, mVi->xsize, mVi->ysize, mImage,
                           mVi->rampbase, mZoom, mZoom, 0, mSection, 
                           App->rgba);

  // Get limits and draw the locator box
  mLastSubRet = zapSubsetLimits(mVi, mLastX0, mLastY0, mLastNx, mLastNy);
  if (mLastSubRet)
    return;
  
  /* imodPrintStderr("%d %d %f %d %d %d %d\n", mXborder, mYborder, mZoom,
     mLastX0, mLastY0, mLastNx, mLastNy); */
  b3dLineWidth(1);
  b3dColorIndex(App->endpoint);
  b3dDrawRectangle(mXborder + (int)(mZoom * mLastX0) - 1,
                   mYborder + (int)(mZoom * mLastY0) - 1,
                   (int)(mZoom * mLastNx) + 1,
                   (int)(mZoom * mLastNy) + 1);

  // Set the zoom label
  QString str;
  str.sprintf("Zoom %.2f", mZoom);
  LocWin->mZoomLabel->setText(str);
}

/*
 * Convert mouse coordinates to image coordinates
 */
void LocatorGL::getImxy(int x, int y, float &imx, float &imy)
{
  imx = (float)((x - mXborder) / mZoom);
  imy = (float)((mWiny - 1 - y - mYborder) / mZoom);
}

/*
 * Mouse press
 */
void LocatorGL::mousePressEvent(QMouseEvent * e )
{
  float imx, imy;
  if (!(mLastNx < mVi->xsize - 1 || mLastNy < mVi->ysize - 1))
    return;

  utilRaiseIfNeeded(LocWin, e);

  // Button 1, non-incremental position setting
  if (e->buttons() & ImodPrefs->actualButton(1)) {
    getImxy(e->x(), e->y(), imx, imy);
    zapSetImageOrBandCenter(imx, imy, false);

    // Button 2: start a shifting from current position, set to move cursor
  } else if (e->buttons() & ImodPrefs->actualButton(2)) {
    setCursor(QCursor(Qt::SizeAllCursor));
    mCursorSet = 1;
    mMouseX = e->x();
    mMouseY = e->y();
  }
}

/*
 * Mouse button 2 up: reset the cursor
 */
void LocatorGL::mouseReleaseEvent ( QMouseEvent * e )
{
  if (mCursorSet)
    unsetCursor();
  mCursorSet = 0;
}

/*
 * Mouse button 2 move: shift the zap window
 */
void LocatorGL::mouseMoveEvent ( QMouseEvent * e )
{
  static int ex, ey, button2, processing = 0;
  float imx, imy;

  // Record event state, return if eating events
  ex = e->x();  
  ey = e->y();  
  button2 = e->buttons() & ImodPrefs->actualButton(2);
  if (processing)
    return;

  if (!(mCursorSet && button2))
    return;

  // Catch up with any pending events
  processing = 1;
  imod_info_input();
  processing = 0;

  imx = (float)((ex - mMouseX) / mZoom);
  imy = (float)((mMouseY - ey) / mZoom);
  mMouseX = ex;
  mMouseY = ey;
  zapSetImageOrBandCenter(imx, imy, true);
}

/*
$Log$
Revision 1.9  2009/03/30 18:26:20  mast
Call function to raise on mouse press if needed

Revision 1.8  2009/03/22 19:54:25  mast
Show with new geometry adjust routine for Mac OS X 10.5/cocoa

Revision 1.7  2009/03/05 00:59:16  mast
Flush mouse move events to get to most recent one when appropriate

Revision 1.6  2009/01/15 16:33:18  mast
Qt 4 port

Revision 1.5  2008/08/19 20:01:40  mast
Made it zoom with + as well as =

Revision 1.4  2008/07/12 03:02:02  mast
Fixed zoom resize bug

Revision 1.3  2008/01/25 20:21:40  mast
Added return value

Revision 1.2  2007/08/13 20:29:51  mast
Added a few tools at the top

Revision 1.1  2007/08/13 16:05:25  mast
Added to program


*/
