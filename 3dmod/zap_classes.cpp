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
#include <qmenu.h>

#include "imod.h"
#include "zap_classes.h"
#include "xzap.h"
#include "tooledit.h"
#include "arrowbutton.h"
#include "dia_qtutils.h"
#include "preferences.h"

#define MIN_SLIDER_WIDTH 20
#define MAX_SLIDER_WIDTH 100

static const char *fileList[NUM_TOOLBUTTONS][2] =
  { {":/images/lowres.png", ":/images/highres.png"},
    {":/images/unlock.png", ":/images/lock.png"},
    {":/images/smartCenter.png", ":/images/keepCenter.png"},
    {":/images/insertAfter.png", ":/images/insertBefore.png"},
    {":/images/rubberband.png", ":/images/rubberband2.png"},
    {":/images/lasso.png", ":/images/lassoOn.png"},
    {":/images/arrowBlack.png", ":/images/arrowRed.png"},
    {":/images/timeUnlock.png", ":/images/timeLock.png"}};

static int skipInPanels[NUM_TOOLBUTTONS] = {0, 0, 0, 1, 1, 1, 1, 0};

static QIcon *icons[NUM_TOOLBUTTONS];
static int firstTime = 1;
static const char *toggleTips[] = {
  "Toggle between regular and high-resolution (interpolated) image",
  "Lock window at current section unless section is changed in this window",
  "Toggle between centering when model point nears edge and keeping model"
  " point centered\nIn movie mode, toggle on and off to center current image point",
  "Toggle between inserting points after or before current point (hot key I)",
  "Toggle rubberband on or off (resize with first mouse, move with second; hot key "
  "Shift+B)",
  "Toggle lasso tool on or off (draw and move with first or second mouse)",
  "Toggle arrow on or off (draw with first mouse)",
  "Lock window at current time unless time is changed in this window"};

static PopupEntry sPopupTable[] = {
  {"Toggle automatic section advance", Qt::Key_Z, 0, 1, 0},
  {"Toggle modeling direction", Qt::Key_I, 0, 0, 0},
  {"Print area information, raise Info window", Qt::Key_I, 0, 1, 0},
  {"Toggle rubber band", Qt::Key_B, 0, 1, 0},
  {"Report distance from current point to cursor", Qt::Key_Q, 0, 0, 0},
  {"Toggle adjusting contour with mouse", Qt::Key_P, 0, 1, 0},
  {"Add contours on section to selection list", Qt::Key_A, 1, 0, 0},
  {"Add contours from all objects to selection", Qt::Key_A, 1, 1, 0},
  {"Resize window to image or rubber band", Qt::Key_R, 0, 1, 0},
  {"Resize area within rubber band to fit window", Qt::Key_R, 1, 1, 0},
  {"", 0, 0, 0, 0}};

ZapWindow::ZapWindow(ZapFuncs *zap, QString timeLabel, bool panels,
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
    utilFileListsToIcons(fileList, icons, NUM_TOOLBUTTONS);
  firstTime = 0;

  // Get the toolbar, add zoom arrows
  mToolBar = utilMakeToolBar(this, false, TB_AUTO_RAISE ? 0 : 4, "Zap Toolbar");

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
  mSecSlider = diaSlider(1, mZap->mVi->zsize, 1, 1, this, NULL);
  mToolBar->addWidget(mSecSlider);

  QSize hint = mSecSlider->minimumSizeHint();
  /* fprintf(stderr, "minimum slider size %d minimum hint size %d\n", 
     mSecSlider->minimumWidth(), hint.width()); */
  int swidth = mZap->mVi->zsize < MAX_SLIDER_WIDTH ? 
    mZap->mVi->zsize : MAX_SLIDER_WIDTH;
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
                  "Bring Info Window to top, get Zap window info (hot key Ctrl+I)");
    connect(mInfoButton, SIGNAL(clicked()), this, SLOT(info()));
  }

  utilTBPushButton("Help", this, mToolBar, &mHelpButton, "Open help window");
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(help()));
  setFontDependentWidths();
  
  // Optional section if time enabled
  if (!timeLabel.isEmpty()) {
    mToolBar2 = utilMakeToolBar(this, false, TB_AUTO_RAISE ? 0 : 4, "Zap Time Toolbar");

    j = NUM_TOOLBUTTONS - 1;
    mToggleActs[j] = utilSetupToggleButton(mToolBar2, mToolBar2, NULL,
                                           toggleMapper, icons, toggleTips,
                                           mToggleButs, mToggleStates, j);
    connect(mToggleButs[j], SIGNAL(clicked()), toggleMapper, SLOT(map()));

    label = new QLabel("4th D", this);
    label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    mToolBar2->addWidget(label);

    utilTBArrowButton(Qt::LeftArrow, this, mToolBar2, &downArrow, 
                      "Move to previous image file, (hot key 1)");
    connect(downArrow, SIGNAL(clicked()), this, SLOT(timeBack()));
    downArrow->setAutoRepeat(true);

    utilTBArrowButton(Qt::RightArrow, this, mToolBar2, &upArrow, 
                      "Move to next image file (hot key 2)");
    connect(upArrow, SIGNAL(clicked()), this, SLOT(timeForward()));
    upArrow->setAutoRepeat(true);

    mTimeNumLabel = new QLabel(" (999)", this);
    mTimeLabel = new QLabel(timeLabel, this);
    setTimeLabel(ivwGetTime(mZap->mVi, &j), timeLabel);
    mToolBar2->addWidget(mTimeNumLabel);
    mToolBar2->addWidget(mTimeLabel);
  }

  // Optional panel toolbar
  if (panels) {
    setLowHighSectionState(0);
    mPanelBar =  utilMakeToolBar(this, false, 0, "MultiZ Toolbar");
    label = new QLabel("# X");
    mPanelBar->addWidget(label);
    mColumnSpin = (QSpinBox *)diaLabeledSpin(0, 1., (float)MULTIZ_MAX_PANELS,
                                             1., NULL, this, NULL);
    mPanelBar->addWidget(mColumnSpin);
    diaSetSpinBox(mColumnSpin, mZap->mNumXpanels);
    connect(mColumnSpin, SIGNAL(valueChanged(int)), this, 
            SLOT(columnsChanged(int)));
    mColumnSpin->setToolTip("Number of columns of panels");

    label = new QLabel("# Y");
    mPanelBar->addWidget(label);
    mRowSpin = (QSpinBox *)diaLabeledSpin(0, 1., (float)MULTIZ_MAX_PANELS, 
                              1., NULL, this, NULL);
    mPanelBar->addWidget(mRowSpin);
    diaSetSpinBox(mRowSpin, mZap->mNumYpanels);
    connect(mRowSpin, SIGNAL(valueChanged(int)), this, SLOT(rowsChanged(int)));
    mRowSpin->setToolTip("Number of rows of panels");

    label = new QLabel("Z step");
    mPanelBar->addWidget(label);
    mZstepSpin = (QSpinBox *)diaLabeledSpin(0, 1., 100., 1., NULL, this, NULL);
    diaSetSpinBox(mZstepSpin, zap->mPanelZstep);
    mPanelBar->addWidget(mZstepSpin);
    connect(mZstepSpin, SIGNAL(valueChanged(int)), this, 
            SLOT(zStepChanged(int)));
    mZstepSpin->setToolTip("Interval between panels in Z");

    //label = new QLabel("Show in:", mPanelBar);
    //label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);

    button = diaCheckBox("Middle", this, NULL);
    mPanelBar->addWidget(button);
    diaSetChecked(button, zap->mDrawInCenter != 0);
    connect(button, SIGNAL(toggled(bool)), this, 
            SLOT(drawCenterToggled(bool)));
    button->setToolTip("Draw model on middle panel in Z");

    button = diaCheckBox("Others", this, NULL);
    mPanelBar->addWidget(button);
    diaSetChecked(button, mZap->mDrawInOthers != 0);
    connect(button, SIGNAL(toggled(bool)), this, 
            SLOT(drawOthersToggled(bool)));
    button->setToolTip("Draw model on panels other than middle one in Z");

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

void ZapWindow::changeEvent(QEvent *e)
{
  QMainWindow::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    setFontDependentWidths();
}

void ZapWindow::zoomUp()
{
  mZap->stepZoom(1);
}

void ZapWindow::zoomDown()
{
  mZap->stepZoom(-1);
}

// A new zoom or section was entered - let zap decide on limits and refresh box
void ZapWindow::newZoom()
{
  QString str = mZoomEdit->text();
  mZap->enteredZoom(atof(LATIN1(str)));
}

void ZapWindow::newSection()
{
  QString str = mSectionEdit->text();
  mZap->enteredSection(atoi(LATIN1(str)));
}

void ZapWindow::sliderChanged(int value)
{
    if (!mSecPressed || ImodPrefs->hotSliderActive(mCtrlPressed))
      mZap->enteredSection(value);
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
  mZap->enteredSection(mDisplayedSection);
}

void ZapWindow::help()
{
  mZap->help();
}

void ZapWindow::info()
{
  mZap->printInfo();
}

void ZapWindow::timeBack()
{
  mZap->stepTime(-1);
}

void ZapWindow::timeForward()
{
  mZap->stepTime(1);
}

// One of toggle buttons needs to change state
void ZapWindow::toggleClicked(int index)
{
  int state = mToggleButs[index]->isChecked() ? 1 : 0;
  mToggleStates[index] = state; 
  mZap->stateToggled(index, state);
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
  if (mSizeLabel && !ivwGetTiltAngles(mZap->mVi, num)) {
    QString str;
    setSizeAngleState();
    str.sprintf(" %dx%d", winx, winy);
    mSizeLabel->setText(str);
  }
}

void ZapWindow::setSectionText(int section)
{
  int num;
  float *angles = ivwGetTiltAngles(mZap->mVi, num);
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
  int state = ivwGetTiltAngles(mZap->mVi, num) ? 1 : 0;
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

void ZapWindow::setTimeLabel(int time, QString label)
{
  QString str;
  str.sprintf(" (%3d)", time);
  mTimeNumLabel->setText(str);
  mTimeLabel->setText(label);
}

void ZapWindow::rowsChanged(int value)
{
  setFocus();
  mZap->mNumYpanels = value;
  if (!mZap->setupPanels())
    mGLw->updateGL();
}

void ZapWindow::columnsChanged(int value)
{
  setFocus();
  mZap->mNumXpanels = value;
  if (!mZap->setupPanels())
    mGLw->updateGL();
}

void ZapWindow::zStepChanged(int value)
{
  setFocus();
  mZap->mPanelZstep = value;
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
  str.sprintf("%d", mZap->mSection + 1);
  mLowSectionEdit->setText(str);
}

void ZapWindow::setHighSection()
{
  QString str;
  str.sprintf("%d", mZap->mSection + 1);
  mHighSectionEdit->setText(str);
}

void ZapWindow::drawCenterToggled(bool state)
{
  mZap->mDrawInCenter = state ? 1 : 0;
  mGLw->updateGL();
}

void ZapWindow::drawOthersToggled(bool state)
{
  mZap->mDrawInOthers = state ? 1 : 0;
  mGLw->updateGL();
}

void ZapWindow::keyPressEvent ( QKeyEvent * e )
{
  if (hotSliderFlag() != NO_HOT_SLIDER && e->key() == hotSliderKey()) {
    mCtrlPressed = true;
    grabKeyboard();
  }
  mZap->keyInput(e);
}

void ZapWindow::keyReleaseEvent (QKeyEvent * e )
{
  if (e->key() == hotSliderKey()) {
    mCtrlPressed = false;
    releaseKeyboard();
  }
  mZap->keyRelease(e);
}
void ZapWindow::wheelEvent ( QWheelEvent * e)
{
  mZap->generalEvent(e);
}

// Whan a close event comes in, inform zap, and accept
void ZapWindow::closeEvent (QCloseEvent * e )
{
  mZap->closing();
  e->accept();
  delete mZap;
}

// Process a right click on the toolbar with a popup menu
void ZapWindow::toolbarMenuEvent(QContextMenuEvent *event)
{
  QSignalMapper mapper(this);
  connect(&mapper, SIGNAL(mapped(int)), this, SLOT(contextMenuHit(int)));
  utilBuildExecPopupMenu(this, &sPopupTable[0], true, &mapper, event);
}

void ZapWindow::contextMenuHit(int index)
{
  Qt::KeyboardModifiers modifiers;
  int key = utilLookupPopupHit(index, &sPopupTable[0], -1, modifiers);
  QKeyEvent *event = new QKeyEvent(QEvent::KeyPress, key, modifiers);
  mZap->keyInput(event);
  delete event;
}

/*
* The GL class
*/
ZapGL::ZapGL(ZapFuncs *zap, QGLFormat inFormat, QWidget * parent)
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
  mZap->paint();
}

// When the timer fires after the first draw, do first real draw
void ZapGL::timerEvent(QTimerEvent * e )
{
  cancelRedraw();
  updateGL();
}

// Set up to redraw after an interval
void ZapGL::scheduleRedraw(int interval)
{
  mTimerID = startTimer(interval);
}

// Cancel an existing redraw timer
void ZapGL::cancelRedraw()
{
  if (mTimerID)
    killTimer(mTimerID);
  mTimerID = 0;
}

void ZapGL::resizeGL( int wdth, int hght )
{
  mZap->resize(wdth, hght);
}

void ZapGL::mousePressEvent(QMouseEvent * e )
{
  mMousePressed = true;
  mZap->mousePress(e);
}

void ZapGL::mouseReleaseEvent ( QMouseEvent * e )
{
  mMousePressed = false;
  mZap->mouseRelease(e);
}

void ZapGL::mouseMoveEvent ( QMouseEvent * e )
{
  mMouseInWindow = true;
  mZap->mouseMove(e);
}

void ZapGL::enterEvent ( QEvent * e)
{
  mMouseInWindow = 1;
  mZap->generalEvent(e);
}
void ZapGL::leaveEvent ( QEvent * e)
{
  mMouseInWindow = 0;
  mZap->generalEvent(e);
}

