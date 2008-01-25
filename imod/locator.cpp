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
#include <qvbox.h>
#include <qhbox.h>
#include <qpushbutton.h>

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
                             imodDialogManager.parent(IMOD_IMAGE), 
                             "locator window");
  if (!LocWin) {
    wprint("\aError opening locator window.\n");
    return(-1);
  }
  GLw->mVi = vi;
  if (!App->rgba)
    GLw->setColormap(*(App->qColormap));
  LocWin->setCaption(imodCaption("3dmod Locator"));
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
  LocWin->show();
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
                QWidget * parent, const char * name, WFlags f)
  : QMainWindow(parent, name, f)
{
  QVBox *central = new QVBox(this, "central");
  setCentralWidget(central);
  QHBox *topHBox = new QHBox(central, "topHBox");
  topHBox->setSpacing(4);

  ArrowButton *arrow = new ArrowButton(Qt::UpArrow, topHBox, "zoomup button");
  connect(arrow, SIGNAL(clicked()), this, SLOT(zoomUp()));
  QToolTip::add(arrow, "Increase window size by 1.5 (hot key =)");
  arrow = new ArrowButton(Qt::DownArrow, topHBox, "zoom down button");
  connect(arrow, SIGNAL(clicked()), this, SLOT(zoomDown()));
  QToolTip::add(arrow, "Decrease window size by 1.5 (hot key -)");
  mZoomLabel = new QLabel("Zoom 1.00", topHBox);

  // Help button
  mHelpButton = new QPushButton("Help", topHBox, "Help button");
  mHelpButton->setFocusPolicy(QWidget::NoFocus);
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(help()));
  setFontDependentWidths();
  QHBox *topSpacer = new QHBox(topHBox);
  topHBox->setStretchFactor(topSpacer, 1);

  QGLFormat glFormat;
  glFormat.setRgba(rgba);
  glFormat.setDoubleBuffer(doubleBuffer);
  glFormat.setDepth(enableDepth);
  GLw = new LocatorGL(glFormat, central);
  setFocusPolicy(QWidget::StrongFocus);
  central->setStretchFactor(GLw, 1);
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

  switch(key){
  case Qt::Key_Escape:
    close();
    return;
    
  case Qt::Key_Minus:
    GLw->changeSize(0.6667);
    break;

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
LocatorGL::LocatorGL(QGLFormat format, QWidget * parent, const char * name)
  : QGLWidget(format, parent, name)
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
  int newy = B3DMAX(16, (int)(factor * mWinx)) + LocWin->height() - height();
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

  // Button 1, non-incremental position setting
  if (e->stateAfter() & ImodPrefs->actualButton(1)) {
    getImxy(e->x(), e->y(), imx, imy);
    zapSetImageOrBandCenter(imx, imy, false);

    // Button 2: start a shifting from current position, set to move cursor
  } else if (e->stateAfter() & ImodPrefs->actualButton(2)) {
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
  float imx, imy;
  if (!(mCursorSet && (e->state() & ImodPrefs->actualButton(2))))
    return;
  imx = (float)((e->x() - mMouseX) / mZoom);
  imy = (float)((mMouseY - e->y()) / mZoom);
  mMouseX = e->x();
  mMouseY = e->y();
  zapSetImageOrBandCenter(imx, imy, true);
}

/*
$Log$
Revision 1.2  2007/08/13 20:29:51  mast
Added a few tools at the top

Revision 1.1  2007/08/13 16:05:25  mast
Added to program


*/
