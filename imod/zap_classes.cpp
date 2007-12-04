/*
 *  zap_classes.cpp -- implementation  ZaP mainwindow and QGLWidget classes.
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

#include <stdlib.h>
#include <qtoolbutton.h>
#include <qlabel.h>
#include <qbitmap.h>
#include <hottoolbar.h>
#include <qtooltip.h>
#include <qsignalmapper.h>
#include <qpushbutton.h>
#include <qlayout.h>
#include <qslider.h>

#include "imod.h"
#include "zap_classes.h"
#include "xzap.h"
#include "tooledit.h"
#include "arrowbutton.h"
#include "dia_qtutils.h"
#include "preferences.h"

#define BM_WIDTH 16
#define BM_HEIGHT 16
#define AUTO_RAISE true
#define MIN_SLIDER_WIDTH 20
#define MAX_SLIDER_WIDTH 100

// Unfinished business: recovering bitmap files
#include "unlock.bits"
#include "lock.bits"
#include "time_unlock.bits"
#include "time_lock.bits"
#include "lowres.bits"
#include "highres.bits"
#include "rubberband.bits"
#include "rubberband2.bits"
#include "smartCenter.bits"
#include "keepCenter.bits"

static unsigned char insert_after_bits[] = {
  0x00, 0x00, 0xc0, 0x03, 0xc0, 0x03, 0xc0, 0x03, 0xc0, 0x03, 0x80, 0x01,
  0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01,
  0x80, 0x01, 0x80, 0x01, 0xff, 0xff, 0xff, 0xff};

static unsigned char insert_before_bits[] = {
  0xff, 0xff, 0xff, 0xff, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01,
  0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0x80, 0x01, 0xc0, 0x03,
  0xc0, 0x03, 0xc0, 0x03, 0xc0, 0x03, 0x00, 0x00};

static unsigned char *bitList[NUM_TOOLBUTTONS][2] =
  { {lowres_bits, highres_bits},
    {unlock_bits, lock_bits},
    {smartCenter_bits, keepCenter_bits},
    {insert_after_bits, insert_before_bits},
    {rubberband_bits, rubberband2_bits},
    {time_unlock_bits, time_lock_bits}};

static QBitmap *bitmaps[NUM_TOOLBUTTONS][2];
static int firstTime = 1;

ZapWindow::ZapWindow(struct zapwin *zap, QString timeLabel, bool rgba, 
                     bool doubleBuffer, bool enableDepth, QWidget * parent,
                     const char * name, WFlags f) 
  : QMainWindow(parent, name, f)
{
  int j;
  ArrowButton *arrow;

  mZap = zap;
  mSecPressed = false;
  mCtrlPressed = false;
  mToolBar2 = NULL;

  // Get the toolbar, add zoom arrows
  mToolBar = new HotToolBar(this, "zap toolbar");
  if (!AUTO_RAISE)
    mToolBar->boxLayout()->setSpacing(4);
  connect(mToolBar, SIGNAL(keyPress(QKeyEvent *)), this,
	  SLOT(toolKeyPress(QKeyEvent *)));
  connect(mToolBar, SIGNAL(keyRelease(QKeyEvent *)), this,
	  SLOT(toolKeyRelease(QKeyEvent *)));

  arrow = new ArrowButton(Qt::UpArrow, mToolBar, "zoomup button");
  arrow->setAutoRaise(AUTO_RAISE);
  connect(arrow, SIGNAL(clicked()), this, SLOT(zoomUp()));
  QToolTip::add(arrow, "Increase zoom factor");
  arrow = new ArrowButton(Qt::DownArrow, mToolBar, "zoom down button");
  arrow->setAutoRaise(AUTO_RAISE);
  connect(arrow, SIGNAL(clicked()), this, SLOT(zoomDown()));
  QToolTip::add(arrow, "Decrease zoom factor");

  mZoomEdit = new ToolEdit(mToolBar, 6, "zoom edit box");
  mZoomEdit->setFocusPolicy(QWidget::ClickFocus);
  mZoomEdit->setAlignment(Qt::AlignRight);
  connect(mZoomEdit, SIGNAL(returnPressed()), this, SLOT(newZoom()));
  connect(mZoomEdit, SIGNAL(focusLost()), this, SLOT(newZoom()));
  QToolTip::add(mZoomEdit, "Enter an arbitrary zoom factor");

  mSizeLabel = new QLabel(mToolBar, " 0000x0000");

// Make the 4 toggle buttons and their signal mapper
  QSignalMapper *toggleMapper = new QSignalMapper(mToolBar);
  connect(toggleMapper, SIGNAL(mapped(int)), this, SLOT(toggleClicked(int)));
  for (j = 0; j < NUM_TOOLBUTTONS - NUM_TIMEBUTTONS; j++)
    setupToggleButton(mToolBar, toggleMapper, j);

  // Section slider
  QLabel *label = new QLabel("Z", mToolBar);
  label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  mSecSlider = new QSlider(1, zap->vi->zsize, 1, 1, Qt::Horizontal, mToolBar,
			  "section slider");

  QSize hint = mSecSlider->minimumSizeHint();
  /* fprintf(stderr, "minimum slider size %d minimum hint size %d\n", 
     mSecSlider->minimumWidth(), hint.width()); */
  int swidth = zap->vi->zsize < MAX_SLIDER_WIDTH ? 
    zap->vi->zsize : MAX_SLIDER_WIDTH;
  swidth = swidth > MIN_SLIDER_WIDTH ? swidth : MIN_SLIDER_WIDTH;
  mSecSlider->setFixedWidth(swidth + hint.width() + 5);
  connect(mSecSlider, SIGNAL(valueChanged(int)), this, 
	  SLOT(sliderChanged(int)));
  connect(mSecSlider, SIGNAL(sliderPressed()), this, SLOT(secPressed()));
  connect(mSecSlider, SIGNAL(sliderReleased()), this, SLOT(secReleased()));
  QToolTip::add(mSecSlider, "Select or riffle through sections");

  // Section edit box
  mSectionEdit = new ToolEdit(mToolBar, 4, "section edit box");
  mSectionEdit->setFocusPolicy(QWidget::ClickFocus);
  mSectionEdit->setAlignment(Qt::AlignRight);
  connect(mSectionEdit, SIGNAL(returnPressed()), this, SLOT(newSection()));
  connect(mSectionEdit, SIGNAL(focusLost()), this, SLOT(newSection()));
  QToolTip::add(mSectionEdit, "Enter section to display");
  
  // Info and help buttons
  mInfoButton = new QPushButton("I", mToolBar, "I button");
  mInfoButton->setFocusPolicy(QWidget::NoFocus);
  connect(mInfoButton, SIGNAL(clicked()), this, SLOT(info()));
  QToolTip::add(mInfoButton, "Bring Info Window to top, get Zap window info");

  mHelpButton = new QPushButton("Help", mToolBar, "Help button");
  mHelpButton->setFocusPolicy(QWidget::NoFocus);
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(help()));
  QToolTip::add(mHelpButton, "Open help window");
  setFontDependentWidths();

  // Optional section if time enabled
  if (!timeLabel.isEmpty()) {
    mToolBar2 = new HotToolBar(this, "time toolbar");
    if (!AUTO_RAISE)
      mToolBar2->boxLayout()->setSpacing(4);
    connect(mToolBar2, SIGNAL(keyPress(QKeyEvent *)), this,
            SLOT(toolKeyPress(QKeyEvent *)));
    connect(mToolBar2, SIGNAL(keyRelease(QKeyEvent *)), this,
            SLOT(toolKeyRelease(QKeyEvent *)));
    setupToggleButton(mToolBar2, toggleMapper, NUM_TOOLBUTTONS - 1);

    label = new QLabel("4th D", mToolBar2);
    label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);

    arrow = new ArrowButton(Qt::LeftArrow, mToolBar2, "time back button");
    connect(arrow, SIGNAL(clicked()), this, SLOT(timeBack()));
    arrow->setAutoRaise(AUTO_RAISE);
    arrow->setAutoRepeat(true);
    QToolTip::add(arrow, "Move back in 4th dimension (time)");

    arrow = new ArrowButton(Qt::RightArrow, mToolBar2, "time forward button");
    connect(arrow, SIGNAL(clicked()), this, SLOT(timeForward()));
    arrow->setAutoRaise(AUTO_RAISE);
    arrow->setAutoRepeat(true);
    QToolTip::add(arrow, "Move forward in 4th dimension (time)");

    mTimeLabel = new QLabel(timeLabel, mToolBar2, "time label");
  }
  firstTime = 0;

  // Need GLwidget next
  QGLFormat glFormat;
  glFormat.setRgba(rgba);
  glFormat.setDoubleBuffer(doubleBuffer);
  glFormat.setDepth(enableDepth);
  mGLw = new ZapGL(zap, glFormat, this);
  
  // Set it as main widget, set focus, dock on top and bottom only
  setCentralWidget(mGLw);
  setFocusPolicy(QWidget::StrongFocus);
  setDockEnabled(mToolBar, Left, FALSE );
  setDockEnabled(mToolBar, Right, FALSE );
  if (mToolBar2) {
    setDockEnabled(mToolBar2, Left, FALSE );
    setDockEnabled(mToolBar2, Right, FALSE );
  }

  // This makes the toolbar give a proper size hint before showing window
  setUpLayout();
}


ZapWindow::~ZapWindow()
{
}

void ZapWindow::setFontDependentWidths()
{
  
  mInfoButton->setFixedWidth
    (10 + diaGetButtonWidth(this, ImodPrefs->getRoundedStyle(), 1., "I"));
  diaSetButtonWidth(mHelpButton, ImodPrefs->getRoundedStyle(), 1.2, "Help");
}

static char *toggleTips[] = {
  "Toggle between regular and high-resolution (interpolated) image",
  "Lock window at current section unless section is changed in this window",
  "Toggle between centering when model point nears edge and keeping model"
  " point centered",
  "Toggle between inserting points after or before current point",
  "Toggle rubberband on or off (resize with first mouse, move with second)",
  "Lock window at current time unless time is changed in this window"};

// Make the two bitmaps, add the toggle button to the tool bar, and add
// it to the signal mapper
void ZapWindow::setupToggleButton(HotToolBar *toolBar, QSignalMapper *mapper, 
                           int ind)
{
  if (firstTime) {
    bitmaps[ind][0] = new QBitmap(BM_WIDTH, BM_HEIGHT, bitList[ind][0], true);
    bitmaps[ind][1] = new QBitmap(BM_WIDTH, BM_HEIGHT, bitList[ind][1], true);
  }
  mToggleButs[ind] = new QToolButton(toolBar, "toolbar toggle");
  mToggleButs[ind]->setPixmap(*bitmaps[ind][0]);
  mToggleButs[ind]->setAutoRaise(AUTO_RAISE);
  mapper->setMapping(mToggleButs[ind],ind);
  connect(mToggleButs[ind], SIGNAL(clicked()), mapper, SLOT(map()));
  mToggleStates[ind] = 0;
  QToolTip::add(mToggleButs[ind], QString(toggleTips[ind]));
}

void ZapWindow::zoomUp()
{
  zapStepZoom(mZap, 1);
}

void ZapWindow::zoomDown()
{
  zapStepZoom(mZap, -1);
}

// A new zoom or section was entered - let zap decide on limits and refresh box
void ZapWindow::newZoom()
{
  QString str = mZoomEdit->text();
  zapEnteredZoom(mZap, atof(str.latin1()));
}

void ZapWindow::newSection()
{
  QString str = mSectionEdit->text();
  zapEnteredSection(mZap, atoi(str.latin1()));
}

void ZapWindow::sliderChanged(int value)
{
    if (!mSecPressed || ImodPrefs->hotSliderActive(mCtrlPressed))
      zapEnteredSection(mZap, value);
    else
      setSectionText(value);
}

void ZapWindow::secPressed()
{
  mSecPressed = true;
}

void ZapWindow::secReleased()
{
  mSecPressed = false;
  zapEnteredSection(mZap, mDisplayedSection);
}

void ZapWindow::help()
{
  zapHelp();
}

void ZapWindow::info()
{
  zapPrintInfo(mZap);
}

void ZapWindow::timeBack()
{
  zapStepTime(mZap, -1);
}

void ZapWindow::timeForward()
{
  zapStepTime(mZap, 1);
}

// One of toggle buttons needs to change state
void ZapWindow::toggleClicked(int index)
{
  int state = 1 - mToggleStates[index];
  mToggleStates[index] = state; 
  mToggleButs[index]->setPixmap(*bitmaps[index][state]);
  zapStateToggled(mZap, index, state);
}

// This allows zap to set one of the buttons
void ZapWindow::setToggleState(int index, int state)
{
  mToggleStates[index] = state ? 1 : 0;
  mToggleButs[index]->setPixmap(*bitmaps[index][state]);
}

void ZapWindow::setZoomText(float zoom)
{
  QString str;
  str.sprintf("%.4f", zoom);
  if (str.endsWith("00"))
    str.truncate(str.length() - 2);
  mZoomEdit->setText(str);
}

void ZapWindow::setSizeText(int winx, int winy)
{
  QString str;
  str.sprintf(" %dx%d", winx, winy);
  mSizeLabel->setText(str);
}

void ZapWindow::setSectionText(int section)
{
  QString str;
  str.sprintf("%d", section);
  mSectionEdit->setText(str);
  diaSetSlider(mSecSlider, section);
  mDisplayedSection = section;
}

void ZapWindow::setMaxZ(int maxZ)
{
  mSecSlider->blockSignals(true);
  mSecSlider->setRange(1, maxZ);
  mSecSlider->blockSignals(false);
}

void ZapWindow::setTimeLabel(QString label)
{
  mTimeLabel->setText(label);
}

void ZapWindow::keyPressEvent ( QKeyEvent * e )
{
  if (hotSliderFlag() != NO_HOT_SLIDER && e->key() == hotSliderKey()) {
    mCtrlPressed = true;
    grabKeyboard();
  }
  zapKeyInput(mZap, e);
}

void ZapWindow::keyReleaseEvent (QKeyEvent * e )
{
  if (e->key() == hotSliderKey()) {
    mCtrlPressed = false;
    releaseKeyboard();
  }
  zapKeyRelease(mZap, e);
}
void ZapWindow::wheelEvent ( QEvent * e)
{
  imodPuts("Zapwin wheelEvent");
  zapGeneralEvent(mZap, e);
}

// Whan a close event comes in, inform zap, and accept
void ZapWindow::closeEvent (QCloseEvent * e )
{
  zapClosing(mZap);
  e->accept();
}

ZapGL::ZapGL(struct zapwin *zap, QGLFormat inFormat, QWidget * parent,
             const char * name)
  : QGLWidget(inFormat, parent, name)
{
  mMousePressed = false;
  mMouseInWindow = -1;
  mZap = zap;
  mFirstDraw = true;
}

void ZapGL::paintGL()
{
  if (mFirstDraw) {
    mTimerID = startTimer(10);
    mFirstDraw = false;
    // DNM 3/25/04: Nvidia driver 5336 does a bad first draw and needs a clear
    // DNM 6/8/04: Suse 9.1 on AMD64 needed more, so just switch to doing 
    // two draws unconditionally
  }
  zapPaint(mZap);
}

// When the timer fires after the first draw, do first real draw
void ZapGL::timerEvent(QTimerEvent * e )
{
  killTimer(mTimerID);
  updateGL();
}

void ZapGL::resizeGL( int wdth, int hght )
{
  zapResize(mZap, wdth, hght);
}

void ZapGL::mousePressEvent(QMouseEvent * e )
{
  mMousePressed = true;
  zapMousePress(mZap, e);
}

void ZapGL::mouseReleaseEvent ( QMouseEvent * e )
{
  mMousePressed = false;
  zapMouseRelease(mZap, e);
}

void ZapGL::mouseMoveEvent ( QMouseEvent * e )
{
  mMouseInWindow = true;
  zapMouseMove(mZap, e, mMousePressed);
}

void ZapGL::enterEvent ( QEvent * e)
{
  mMouseInWindow = 1;
  zapGeneralEvent(mZap, e);
}
void ZapGL::leaveEvent ( QEvent * e)
{
  mMouseInWindow = 0;
  zapGeneralEvent(mZap, e);
}
void ZapGL::wheelEvent ( QEvent * e)
{
  imodPuts("ZapGL wheel");
  zapGeneralEvent(mZap, e);
}

/*
$Log$
Revision 4.23  2007/07/08 16:04:50  mast
Used new hot slider function

Revision 4.22  2007/06/26 21:58:07  sueh
bug# 1021 Removed win_support.

Revision 4.21  2007/06/26 17:08:54  sueh
bug# 1021 Moved BM_HEIGHT and _WIDTH to win_support.

Revision 4.20  2007/05/31 16:23:10  mast
Changes for using hot toolbar

Revision 4.19  2007/05/29 14:49:20  mast
Moved keep center and snart center bits to files

Revision 4.18  2006/04/01 23:43:15  mast
Added size output to toolbar

Revision 4.17  2005/03/29 00:59:25  mast
Moved time to second toolbar

Revision 4.16  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 4.15  2004/06/08 16:24:26  mast
Stopped fooling around and just made it do two draws on starting window

Revision 4.14  2004/05/07 22:15:59  mast
Fixed array dimension problems caused by new toolbutton

Revision 4.13  2004/05/05 17:32:16  mast
Added rubberband tool button

Revision 4.12  2004/03/26 04:58:24  mast
Made it do a clear on first call, because of problems with 53xx Nvidia driver

Revision 4.11  2003/10/01 05:04:19  mast
change include from imodP to imod after eliminating imod.h from imodP.h

Revision 4.10  2003/09/24 00:47:12  mast
Eliminated second setting geometry now that move is used instead

Revision 4.9  2003/09/18 00:48:14  mast
Set the geometry when timer event comes in to do the first real draw

Revision 4.8  2003/09/15 21:04:19  mast
Allow zooms to 4 decimal places

Revision 4.7  2003/04/11 21:47:28  mast
adding tooltips

Revision 4.6  2003/03/26 23:23:15  mast
switched from hotslider.h to preferences.h

Revision 4.5  2003/03/26 06:30:56  mast
adjusting to font changes

Revision 4.4  2003/03/07 15:49:26  mast
Put z section slider under hot slider control

Revision 4.3  2003/03/03 22:28:02  mast
Pass on all mouse move events with the pressed flag to allow tracking

Revision 4.2  2003/02/28 21:40:15  mast
Changing name of tooledit focus signal

Revision 4.1  2003/02/10 20:29:03  mast
autox.cpp

Revision 1.1.2.15  2003/01/30 06:23:18  mast
fiddling with first draw

Revision 1.1.2.14  2003/01/30 06:17:47  mast
Add ability to change range of Z slider on image flip

Revision 1.1.2.13  2003/01/30 00:48:43  mast
New timer logic

Revision 1.1.2.12  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.11  2003/01/13 01:15:43  mast
changes for Qt version of info window

Revision 1.1.2.10  2003/01/02 15:40:27  mast
use dia call to block signals when setting toolbar slider

Revision 1.1.2.9  2002/12/17 17:30:50  mast
Adding timer for redraws, using tooledit with column specifier

Revision 1.1.2.8  2002/12/17 04:45:54  mast
Use new ability to set columns in tooledits

Revision 1.1.2.7  2002/12/14 05:23:42  mast
backing out the fancy subclass, adjusting for new visual detection

Revision 1.1.2.6  2002/12/13 07:09:19  mast
GLMainWindow needed different name for mouse event processors

Revision 1.1.2.5  2002/12/13 06:06:29  mast
using new glmainwindow and mainglwidget classes

Revision 1.1.2.4  2002/12/12 01:25:14  mast
added z slider

Revision 1.1.2.3  2002/12/09 23:24:06  mast
*** empty log message ***

Revision 1.1.2.2  2002/12/09 22:00:29  mast
include stdio and stdlib for atof/atoi calls

Revision 1.1.2.1  2002/12/09 17:47:51  mast
Initial addition to source

*/
