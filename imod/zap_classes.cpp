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
//Added by qt3to4:
#include <QTimerEvent>
#include <QKeyEvent>
#include <QMouseEvent>
#include <QEvent>
#include <QWheelEvent>
#include <QCloseEvent>
#include <hottoolbar.h>
#include <qtooltip.h>
#include <qsignalmapper.h>
#include <qpushbutton.h>
#include <qlayout.h>
#include <qaction.h>
#include <qslider.h>
#include <qcheckbox.h>
#include <qspinbox.h>

#include "imod.h"
#include "zap_classes.h"
#include "xzap.h"
#include "tooledit.h"
#include "arrowbutton.h"
#include "dia_qtutils.h"
#include "preferences.h"

#define BM_WIDTH 16
#define BM_HEIGHT 16
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

static int skipInPanels[NUM_TOOLBUTTONS] = {0, 0, 0, 1, 1, 0};

static QIcon *icons[NUM_TOOLBUTTONS];
static int firstTime = 1;
static char *toggleTips[] = {
  "Toggle between regular and high-resolution (interpolated) image",
  "Lock window at current section unless section is changed in this window",
  "Toggle between centering when model point nears edge and keeping model"
  " point centered",
  "Toggle between inserting points after or before current point",
  "Toggle rubberband on or off (resize with first mouse, move with second)",
  "Lock window at current time unless time is changed in this window"};


ZapWindow::ZapWindow(struct zapwin *zap, QString timeLabel, bool panels,
                     bool rgba, bool doubleBuffer, bool enableDepth, 
                     QWidget * parent, const char * name, Qt::WFlags f) 
  : QMainWindow(parent, f)
{
  int j;
  ArrowButton *upArrow, *downArrow;
  QCheckBox *button;

  mZap = zap;
  mSecPressed = false;
  mCtrlPressed = false;
  mToolBar2 = NULL;
  mPanelBar = NULL;
  mSizeLabel = NULL;
  mSizeAction = NULL;
  mAngleLabel = NULL;
  mAngleAction = NULL;
  mInfoButton = NULL;
  mSizeAngleState = -1;
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  setAnimated(false);
  if (firstTime) 
    utilBitListsToIcons(bitList, icons, NUM_TOOLBUTTONS);
  firstTime = 0;

  // Get the toolbar, add zoom arrows
  mToolBar = new HotToolBar(this);
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

  if (!panels) {
    mSizeLabel = new QLabel(" 0000x0000", this);
    mSizeAction = mToolBar->addWidget(mSizeLabel);
  }

// Make the toggle buttons and their signal mapper
  QSignalMapper *toggleMapper = new QSignalMapper(mToolBar);
  connect(toggleMapper, SIGNAL(mapped(int)), this, SLOT(toggleClicked(int)));
  for (j = 0; j < NUM_TOOLBUTTONS - NUM_TIMEBUTTONS; j++)
    if (!panels || !skipInPanels[j]) {
      mToggleActs[j] = utilSetupToggleButton(mToolBar, mToolBar, NULL,
                                             toggleMapper, icons, toggleTips,
                                             mToggleButs, mToggleStates, j);
      connect(mToggleButs[j], SIGNAL(clicked()), toggleMapper, SLOT(map()));
    }
  
  // Selecting sections
  mLowButtonAction = utilTBPushButton
    ("Lo", this,  mToolBar, &mLowSectionButton, "Set low section of range");
  connect(mLowSectionButton, SIGNAL(clicked()), this, SLOT(setLowSection()));
  mLowEditAction = utilTBToolEdit(4, this,  mToolBar, &mLowSectionEdit,
                                  "Enter low section of range");
  mHighButtonAction = utilTBPushButton
    ("Hi", this,  mToolBar, &mHighSectionButton, "Set high section of range");
  connect(mHighSectionButton, SIGNAL(clicked()), this, SLOT(setHighSection()));
  mHighEditAction = utilTBToolEdit(4, this,  mToolBar, &mHighSectionEdit,
                                  "Enter high section of range");

  // Hide select Z until rubberband is selected
  setLowHighSectionState(0);
  
  // Section slider
  QLabel *label = new QLabel("Z", this);
  mToolBar->addWidget(label);
  label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  mSecSlider = diaSlider(1, zap->vi->zsize, 1, 1, this, NULL);
  mToolBar->addWidget(mSecSlider);

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
  mSecSlider->setToolTip("Select or riffle through sections");

  // Angle label - and show one, hide one of size and angle lables
  if (!panels) {
    mAngleLabel = new QLabel(" 99.0o", this);
    mAngleAction = mToolBar->addWidget(mAngleLabel);
    mAngleLabel->setTextFormat(Qt::RichText);
    setSizeAngleState();
  }

  // Section edit box
  utilTBToolEdit(4, this, mToolBar, &mSectionEdit, "Enter section to display");
  connect(mSectionEdit, SIGNAL(editingFinished()), this, SLOT(newSection()));
  
  // Info and help buttons
  if (!panels) {
    utilTBPushButton("I", this, mToolBar, &mInfoButton,
                  "Bring Info Window to top, get Zap window info");
    connect(mInfoButton, SIGNAL(clicked()), this, SLOT(info()));
  }

  utilTBPushButton("Help", this, mToolBar, &mHelpButton, "Open help window");
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(help()));
  mToolBar->setAllowedAreas(Qt::TopToolBarArea);
  setFontDependentWidths();
  addToolBar(mToolBar);
  
  // Optional section if time enabled
  if (!timeLabel.isEmpty()) {
    mToolBar2 = new HotToolBar(this);
    if (!TB_AUTO_RAISE)
      mToolBar2->layout()->setSpacing(4);
    connect(mToolBar2, SIGNAL(keyPress(QKeyEvent *)), this,
            SLOT(toolKeyPress(QKeyEvent *)));
    connect(mToolBar2, SIGNAL(keyRelease(QKeyEvent *)), this,
            SLOT(toolKeyRelease(QKeyEvent *)));
    j = NUM_TOOLBUTTONS - 1;
    mToggleActs[j] = utilSetupToggleButton(mToolBar2, mToolBar2, NULL,
                                           toggleMapper, icons, toggleTips,
                                           mToggleButs, mToggleStates, j);
    connect(mToggleButs[j], SIGNAL(clicked()), toggleMapper, SLOT(map()));

    label = new QLabel("4th D", this);
    label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    mToolBar2->addWidget(label);

    utilTBArrowButton(Qt::LeftArrow, this, mToolBar2, &downArrow, 
                      "Move back in 4th dimension (time)");
    connect(downArrow, SIGNAL(clicked()), this, SLOT(timeBack()));
    downArrow->setAutoRepeat(true);

    utilTBArrowButton(Qt::RightArrow, this, mToolBar2, &upArrow, 
                      "Move forward in 4th dimension (time)");
    connect(upArrow, SIGNAL(clicked()), this, SLOT(timeForward()));
    upArrow->setAutoRepeat(true);

    mTimeLabel = new QLabel(timeLabel, this);
    mToolBar2->addWidget(mTimeLabel);
    mToolBar2->setAllowedAreas(Qt::TopToolBarArea);
    addToolBar(mToolBar2);
  }

  // Optional panel toolbar
  if (panels) {
    setLowHighSectionState(0);
    mPanelBar =  new HotToolBar(this);
    label = new QLabel("# X");
    mPanelBar->addWidget(label);
    mColumnSpin = (QSpinBox *)diaLabeledSpin(0, 1., (float)MULTIZ_MAX_PANELS,
                                             1., NULL, this, NULL);
    mPanelBar->addWidget(mColumnSpin);
    diaSetSpinBox(mColumnSpin, zap->numXpanels);
    connect(mColumnSpin, SIGNAL(valueChanged(int)), this, 
            SLOT(columnsChanged(int)));
    mColumnSpin->setToolTip("Number of columns of panels");

    label = new QLabel("# Y");
    mPanelBar->addWidget(label);
    mRowSpin = (QSpinBox *)diaLabeledSpin(0, 1., (float)MULTIZ_MAX_PANELS, 
                              1., NULL, this, NULL);
    mPanelBar->addWidget(mRowSpin);
    diaSetSpinBox(mRowSpin, zap->numYpanels);
    connect(mRowSpin, SIGNAL(valueChanged(int)), this, SLOT(rowsChanged(int)));
    mRowSpin->setToolTip("Number of rows of panels");

    label = new QLabel("Z step");
    mPanelBar->addWidget(label);
    mZstepSpin = (QSpinBox *)diaLabeledSpin(0, 1., 100., 1., NULL, this, NULL);
    diaSetSpinBox(mZstepSpin, zap->panelZstep);
    mPanelBar->addWidget(mZstepSpin);
    connect(mZstepSpin, SIGNAL(valueChanged(int)), this, 
            SLOT(zStepChanged(int)));
    mZstepSpin->setToolTip("Interval between panels in Z");

    //label = new QLabel("Show in:", mPanelBar);
    //label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);

    button = diaCheckBox("Middle", this, NULL);
    mPanelBar->addWidget(button);
    diaSetChecked(button, zap->drawInCenter != 0);
    connect(button, SIGNAL(toggled(bool)), this, 
            SLOT(drawCenterToggled(bool)));
    button->setToolTip("Draw model on middle panel in Z");

    button = diaCheckBox("Others", this, NULL);
    mPanelBar->addWidget(button);
    diaSetChecked(button, zap->drawInOthers != 0);
    connect(button, SIGNAL(toggled(bool)), this, 
            SLOT(drawOthersToggled(bool)));
    button->setToolTip("Draw model on panels other than middle one in Z");

    mPanelBar->setAllowedAreas(Qt::TopToolBarArea);
    addToolBar(mPanelBar);
  }

  // Need GLwidget next
  QGLFormat glFormat;
  glFormat.setRgba(rgba);
  glFormat.setDoubleBuffer(doubleBuffer);
  glFormat.setDepth(enableDepth);
  mGLw = new ZapGL(zap, glFormat, this);
  
  // Set it as main widget, set focus
  setCentralWidget(mGLw);
  setFocusPolicy(Qt::StrongFocus);

  // This makes the toolbar give a proper size hint before showing window
  //setUpLayout();
}


ZapWindow::~ZapWindow()
{
}

void ZapWindow::setFontDependentWidths()
{
  
  if (mInfoButton)
    mInfoButton->setFixedWidth
    (10 + diaGetButtonWidth(this, ImodPrefs->getRoundedStyle(), 1., "I"));
  diaSetButtonWidth(mHelpButton, ImodPrefs->getRoundedStyle(), 1.2, "Help");
  if (mLowSectionButton)
    mLowSectionButton->setFixedWidth
    (8 + diaGetButtonWidth(this, ImodPrefs->getRoundedStyle(), 1., "Lo"));
  if (mHighSectionButton)
    mHighSectionButton->setFixedWidth
    (8 + diaGetButtonWidth(this, ImodPrefs->getRoundedStyle(), 1., "Hi"));
  // Unable to affect sizes of panel bar spin boxes in Windows...
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
  zapEnteredZoom(mZap, atof(LATIN1(str)));
}

void ZapWindow::newSection()
{
  QString str = mSectionEdit->text();
  zapEnteredSection(mZap, atoi(LATIN1(str)));
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
  zapHelp(mZap);
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
  int state = mToggleButs[index]->isChecked() ? 1 : 0;
  mToggleStates[index] = state; 
  zapStateToggled(mZap, index, state);
}

/*
 * Shows the low and high section fields and buttons when the rubberbandSelected
 * parameter is not 0.  Hides them when it is 0.  Resets the fields when hiding
 * them.
 */
void ZapWindow::setLowHighSectionState(int rubberbandSelected)
{
  bool show = rubberbandSelected && !mPanelBar;
  if (!show) {
    mLowSectionEdit->setText("");
    mHighSectionEdit->setText("");
  }
  mLowButtonAction->setVisible(show);
  mHighButtonAction->setVisible(show);
  mLowEditAction->setVisible(show);
  mHighEditAction->setVisible(show);
}

// This allows zap to set one of the buttons
void ZapWindow::setToggleState(int index, int state)
{
  mToggleStates[index] = state ? 1 : 0;
  diaSetChecked(mToggleButs[index], state != 0);
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
  int num;
  if (mSizeLabel && !ivwGetTiltAngles(mZap->vi, num)) {
    QString str;
    setSizeAngleState();
    str.sprintf(" %dx%d", winx, winy);
    mSizeLabel->setText(str);
  }
}

void ZapWindow::setSectionText(int section)
{
  int num;
  float *angles = ivwGetTiltAngles(mZap->vi, num);
  QString str;
  str.sprintf("%d", section);
  mSectionEdit->setText(str);
  diaSetSlider(mSecSlider, section);
  mDisplayedSection = section;
  if (angles && mAngleLabel) {
    setSizeAngleState();
    str.sprintf("&nbsp;%.1f&deg;", section > num ? 0. : angles[section - 1]);
    mAngleLabel->setText(str);
  }
}

void ZapWindow::setSizeAngleState()
{
  int num;
  int state = ivwGetTiltAngles(mZap->vi, num) ? 1 : 0;
  if (state != mSizeAngleState) {
    if (mSizeAction)
      mSizeAction->setVisible(state == 0);
    if (mAngleAction)
      mAngleAction->setVisible(state != 0);
    mSizeAngleState = state;
  }
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

void ZapWindow::rowsChanged(int value)
{
  setFocus();
  mZap->numYpanels = value;
  if (!zapSetupPanels(mZap))
    mGLw->updateGL();
}

void ZapWindow::columnsChanged(int value)
{
  setFocus();
  mZap->numXpanels = value;
  if (!zapSetupPanels(mZap))
    mGLw->updateGL();
}

void ZapWindow::zStepChanged(int value)
{
  setFocus();
  mZap->panelZstep = value;
  mGLw->updateGL();
}

QString ZapWindow::lowSection()
{
  return mLowSectionEdit->text();
}

QString ZapWindow::highSection()
{
  return mHighSectionEdit->text();
}

void ZapWindow::setLowSection()
{
  QString str;
  str.sprintf("%d", mZap->section + 1);
  mLowSectionEdit->setText(str);
}

void ZapWindow::setHighSection()
{
  QString str;
  str.sprintf("%d", mZap->section + 1);
  mHighSectionEdit->setText(str);
}

void ZapWindow::drawCenterToggled(bool state)
{
  mZap->drawInCenter = state ? 1 : 0;
  mGLw->updateGL();
}

void ZapWindow::drawOthersToggled(bool state)
{
  mZap->drawInOthers = state ? 1 : 0;
  mGLw->updateGL();
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
void ZapWindow::wheelEvent ( QWheelEvent * e)
{
  zapGeneralEvent(mZap, e);
}

// Whan a close event comes in, inform zap, and accept
void ZapWindow::closeEvent (QCloseEvent * e )
{
  zapClosing(mZap);
  e->accept();
}

ZapGL::ZapGL(struct zapwin *zap, QGLFormat inFormat, QWidget * parent)
  : QGLWidget(inFormat, parent)
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
  zapMouseMove(mZap, e);
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

/*
$Log$
Revision 4.32  2009/01/15 16:33:18  mast
Qt 4 port

Revision 4.31  2008/05/27 22:48:33  mast
Moved angle to separate label after Z slider

Revision 4.30  2008/05/27 05:41:56  mast
Changes for tilt angle display

Revision 4.29  2008/02/06 16:34:41  sueh
bug# 1065 In setLowHighSectionState reset section fields when hiding.

Revision 4.28  2008/02/05 19:59:06  sueh
bug# 1065 Added a low section button and edit field and a high section button
and edit field to the tool bar.  Fields are associated with the rubberband button.

Revision 4.27  2008/01/13 22:58:35  mast
Changes for multi-Z window

Revision 4.26  2008/01/11 18:15:04  mast
Took out message for wheel event

Revision 4.25  2008/01/11 18:12:52  mast
Fixed event handlers for wheel, dropped GL handler as not needed

Revision 4.24  2007/12/04 18:48:01  mast
Passed on some more events to allow cursor-like drawing and wheel (?)

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
