/*
 *  slicer_classes.cpp -- implements slicer mainwindow and QGLWidget classes.
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2013 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */
#include <stdlib.h>
#include <qtoolbutton.h>
#include <qlabel.h>
#include <qbitmap.h>
#include <qtooltip.h>
#include <qsignalmapper.h>
#include <qpushbutton.h>
#include <qlayout.h>
#include <qcheckbox.h>
#include <qcombobox.h>
#include <qspinbox.h>
#include <qaction.h>
#include <QDoubleSpinBox>
#include <qframe.h>
#include <qdatetime.h>
#include <qslider.h>
#include <qvalidator.h>

//Added by qt3to4:
#include <QTimerEvent>
#include <QBoxLayout>
#include <QMouseEvent>
#include <QCloseEvent>
#include <QKeyEvent>
#include <QVBoxLayout>
#include "imod.h"
#include "cont_edit.h"
#include "hottoolbar.h"
#include "slicer_classes.h"
#include "rotationtool.h"
#include "sslice.h"
#include "pyramidcache.h"
#include "xcramp.h"
#include "b3dgfx.h"
#include "xcorr.h"
#include "tooledit.h"
#include "arrowbutton.h"
#include "multislider.h"
#include "dia_qtutils.h"
#include "preferences.h"
#include "sliceproc.h"

#define BM_WIDTH 16
#define BM_HEIGHT 16
#define AUTO_RAISE true
#define TOOLBUT_SIZE 20

static const char *fileList[MAX_SLICER_TOGGLES][2] =
  { {":/images/lowres.png", ":/images/highres.png"},
    {":/images/unlock.png", ":/images/lock.png"},
    {":/images/smartCenter.png", ":/images/keepCenter.png"}, 
    {":/images/rubberband.png", ":/images/rubberband2.png"},
    {":/images/arrowBlack.png", ":/images/arrowRed.png"},
    {":/images/fft.png", ":/images/fftRed.png"},
    {":/images/zscale.png", ":/images/zscaleOn.png"},
    {":/images/timeUnlock.png", ":/images/timeLock.png"},
    {":/images/shiftlockoff.png", ":/images/shiftlockon.png"}};

static QIcon *sIcons[MAX_SLICER_TOGGLES];
static QIcon *sShowIcon;
static QIcon *sContIcon;
static QIcon *sFillIcon;
static bool sFirstTime = true;
static const char *sToggleTips[] = {
  "Toggle between regular and high-resolution (interpolated) image",
  "Lock window at current position",
  "Keep current image or model point centered (classic mode, hot key k)",
  "Toggle rubberband on or off (resize with first mouse, move with second, hot key B)",
  "Toggle arrow on or off (draw with first mouse)",
  "Toggle between showing image and FFT",
  "Toggle applying model Z-scale to image",
  "Lock window at current time",
  "Use keypad and mouse as if Shift key were down to rotate slice"};

static const char *sSliderLabels[] = {"X rotation", "Y rotation", "Z rotation", 
                                      "View axis position"};

SlicerWindow::SlicerWindow(SlicerFuncs *funcs, float maxAngles[], QString timeLabel,
                           bool rgba, bool doubleBuffer, bool enableDepth, float stepSize,
                           QWidget * parent, Qt::WFlags f) 
  : QMainWindow(parent, f)
{
  int j;
  ArrowButton *upArrow, *downArrow;
  QToolButton *button;
  QGLFormat glFormat;
  QLabel *label;
  QCheckBox *check;

  mTimeBar = NULL;
  mBreakBeforeAngBar = 0;

  mFuncs = funcs;
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  setAnimated(false);
  if (sFirstTime) 
    utilFileListsToIcons(fileList, sIcons, MAX_SLICER_TOGGLES);
  
  // Get the toolbar
  mToolBar = makeToolBar(false, TB_AUTO_RAISE ? 0 : 4, "Slicer Toolbar 1");

  // Zoom tools
  mZoomEdit = utilTBZoomTools(this, mToolBar, &upArrow, &downArrow);
  connect(upArrow, SIGNAL(clicked()), this, SLOT(zoomUp()));
  connect(downArrow, SIGNAL(clicked()), this, SLOT(zoomDown()));
  connect(mZoomEdit, SIGNAL(editingFinished()), this, SLOT(newZoom()));

  // Make the toggle buttons and their signal mapper
  QSignalMapper *toggleMapper = new QSignalMapper(mToolBar);
  connect(toggleMapper, SIGNAL(mapped(int)), this, SLOT(toggleClicked(int)));
  for (j = 0; j < MAX_SLICER_TOGGLES - 2; j++) {
    utilSetupToggleButton(mToolBar, mToolBar, NULL, toggleMapper, sIcons, 
                          sToggleTips, mToggleButs, mToggleStates, j);
    connect(mToggleButs[j], SIGNAL(clicked()), toggleMapper, SLOT(map()));
  }
  
  // The showslice button is simpler
  if (sFirstTime) {
    sShowIcon = new QIcon();
    sShowIcon->addFile(QString(":/images/showslice.png"), QSize(BM_WIDTH, BM_HEIGHT));
    sContIcon = new QIcon();
    sContIcon->addFile(QString(":/images/contour.png"), QSize(BM_WIDTH, BM_HEIGHT));
    if (funcs->mVi->pyrCache) {
      sFillIcon = new QIcon();
      sFillIcon->addFile(QString(":/images/fillCache.png"), QSize(BM_WIDTH, BM_HEIGHT));
    }
  }
 
  utilTBToolButton(this, mToolBar, &button, "Show slice cutting lines in"
                   " Xyz and Zap windows (hot key l)");
  button->setIcon(*sShowIcon);
  connect(button, SIGNAL(clicked()), this, SLOT(showslicePressed()));

  utilTBToolButton(this, mToolBar, &button, "Set angles and position to show"
                   " plane of current contour (hot key W)");
  button->setIcon(*sContIcon);
  connect(button, SIGNAL(clicked()), this, SLOT(contourPressed()));

  if (funcs->mVi->pyrCache) {
    utilTBToolButton(this, mToolBar, &button, "Fill cache for currently displayed area");
    button->setIcon(*sFillIcon);
    connect(button, SIGNAL(clicked()), this, SLOT(fillCachePressed()));
  }

  // The low-high buttons
  QSignalMapper *lowHighMapper = new QSignalMapper(mToolBar);
  connect(lowHighMapper, SIGNAL(mapped(int)), this, SLOT(lowHighClicked(int)));
  mLowHighActions[0] = utilTBPushButton("Lo", this, mToolBar, &mLowHighButtons[0],
                                        "Set low limit for range along viewing axis "
                                        "(Shift-click to return to limit)"); 
  mLowHighActions[1] = utilTBPushButton("Hi", this, mToolBar, &mLowHighButtons[1],
                                        "Set high limit for range along viewing axis "
                                        "(Shift-click to return to limit)"); 
  for (j = 0; j < 2; j++) {
    connect(mLowHighButtons[j], SIGNAL(clicked()), lowHighMapper, SLOT(map()));
    lowHighMapper->setMapping(mLowHighButtons[j], j);
    mLowHighActions[j]->setVisible(false);
    mLowHighStates[j] = SLICER_LIMIT_INVALID - 1;
  }

  utilTBPushButton("Help", this, mToolBar, &mHelpButton, "Open help window");
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(help()));


  // THE TIME TOOLBAR
  if (!timeLabel.isEmpty()) {
    mTimeBar = makeToolBar(true, TB_AUTO_RAISE ? 0 : 4, "Slicer Time Toolbar");

    check = diaCheckBox("Link", this, NULL);
    mTimeBar->addWidget(check);
    connect(check, SIGNAL(toggled(bool)), this, SLOT(linkToggled(bool)));
    check->setToolTip("Keep angles and positions same as for other linked "
                "slicers");

    utilSetupToggleButton(mTimeBar, mTimeBar, NULL, toggleMapper, sIcons, sToggleTips,
                          mToggleButs, mToggleStates, SLICER_TOGGLE_TIMELOCK);
    connect(mToggleButs[SLICER_TOGGLE_TIMELOCK], SIGNAL(clicked()), toggleMapper, 
            SLOT(map()));

    label = new QLabel("4th D", this);
    label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
    mTimeBar->addWidget(label);

    utilTBArrowButton(Qt::LeftArrow, this, mTimeBar, &downArrow, 
                      "Move back in 4th dimension (time)");
    connect(downArrow, SIGNAL(clicked()), this, SLOT(timeBack()));
    downArrow->setAutoRepeat(true);

    utilTBArrowButton(Qt::RightArrow, this, mTimeBar, &upArrow, 
                      "Move forward in 4th dimension (time)");
    connect(upArrow, SIGNAL(clicked()), this, SLOT(timeForward()));
    upArrow->setAutoRepeat(true);

    mTimeNumLabel = new QLabel(" (999)", this);
    mTimeLabel = new QLabel(timeLabel, this);
    setTimeLabel(ivwGetTime(funcs->mVi, &j), timeLabel);
    mTimeBar->addWidget(mTimeNumLabel);
    mTimeBar->addWidget(mTimeLabel);
  }

  // SET ANGLE TOOLBAR
  mSaveAngBar = makeToolBar(false, 4, "Slicer Toolbar 3");

  utilTBPushButton("Save", this, mSaveAngBar, &mSaveAngBut, "Save current "
                   "angles and position in slicer angle table");
  connect(mSaveAngBut, SIGNAL(clicked()), this, SLOT(saveAngClicked()));

  utilTBPushButton("New", this, mSaveAngBar, &mNewRowBut, "Save current "
                   "angles and position in new row of slicer angle table");
  connect(mNewRowBut, SIGNAL(clicked()), this, SLOT(newRowClicked()));

  utilTBPushButton("Set", this, mSaveAngBar, &mSetAngBut, "Set angles and "
                   "position from current row in slicer angle table");
  connect(mSetAngBut, SIGNAL(clicked()), this, SLOT(setAngClicked()));

  mAutoBox = diaCheckBox("Auto", this, NULL);
  mSaveAngBar->addWidget(mAutoBox);
  connect(mAutoBox, SIGNAL(toggled(bool)), this,
          SLOT(continuousToggled(bool)));
  mAutoBox->setToolTip("Continuously update table from slicer and slicer "
                "from table");

  // SECOND TOOLBAR
  mToolBar2 = makeToolBar(true, 2, "Slicer Toolbar 2");

  // Make a frame, put a layout in it, and then put multisliders in the layout
  QWidget *sliderFrame = new QWidget(this);
  //  sliderFrame->setFrameStyle(QFrame::NoFrame);
  mToolBar2->addWidget(sliderFrame);
  QVBoxLayout *sliderLayout = new QVBoxLayout(sliderFrame);
  sliderLayout->setContentsMargins(0, 0, 0, 0);
  mSliders = new MultiSlider(sliderFrame, 4, sSliderLabels, -1800, 1800, 1);
  for (j = 0; j < 4; j++) {
    int maxVal = (int)(10. * maxAngles[j] + 0.1);
    mSliders->setRange(j, -maxVal, maxVal);
    mSliders->getSlider(j)->setMinimumWidth(200);
    mSliders->getSlider(j)->setPageStep(j < 3 ? 10 : 1);
  }
  mSliders->setDecimals(3, 0);

  QBoxLayout *multiLayout = mSliders->getLayout();
  multiLayout->setSpacing(0);
  sliderLayout->addLayout(multiLayout);  
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this, 
	  SLOT(angleChanged(int, int, bool)));

  QFrame *line = new QFrame(this);
  line->setFrameShape( QFrame::VLine );
  line->setFrameShadow( QFrame::Sunken );
  mToolBar2->addWidget(line);

  QWidget *rightBox = new QWidget(this);
  mToolBar2->addWidget(rightBox);
  QVBoxLayout *rightVLay = new QVBoxLayout(rightBox);
  QHBoxLayout *topHLay = new QHBoxLayout();
  QHBoxLayout *spinHLay = new QHBoxLayout();
  rightVLay->addLayout(topHLay);
  diaLabel("Thickness:", rightBox, rightVLay);
  rightVLay->addLayout(spinHLay);
  rightVLay->setContentsMargins(0, 0, 0, 0);
  rightVLay->setSpacing(0);
  topHLay->setContentsMargins(0, 0, 0, 0);
  topHLay->setSpacing(0);
  spinHLay->setContentsMargins(0, 0, 0, 0);
  spinHLay->setSpacing(0);

  mRotationTool = new RotationTool(this, sIcons[SLICER_TOGGLE_SHIFTLOCK], 
                                   sToggleTips[SLICER_TOGGLE_SHIFTLOCK], TOOLBUT_SIZE,
                                   true, stepSize);
  topHLay->addWidget(mRotationTool);
  connect(mRotationTool, SIGNAL(stepChanged(int)), this, SLOT(stepSizeChanged(int)));
  connect(mRotationTool, SIGNAL(centerButToggled(bool)), this, SLOT(shiftToggled(bool)));
  connect(mRotationTool, SIGNAL(rotate(int, int, int)), this,
          SLOT(rotationClicked(int, int, int)));

  // A frame for the cube widget; and the cube with the default GL format
  QFrame *cubeFrame = new QFrame(rightBox);
  topHLay->addWidget(cubeFrame);
  cubeFrame->setFixedWidth(85);
  cubeFrame->setFrameShadow(QFrame::Sunken);
  cubeFrame->setFrameShape(QFrame::StyledPanel);
  QVBoxLayout *cubeLayout = new QVBoxLayout(cubeFrame);
  cubeLayout->setContentsMargins(2, 2, 2, 2);
  mCube = new SlicerCube(funcs, glFormat, cubeFrame);
  cubeLayout->addWidget(mCube);

  // Thickness of image spin box
  label = diaLabel("Img", rightBox, spinHLay);
  label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  mImageBox = new QSpinBox(rightBox);
  spinHLay->addWidget(mImageBox);
  mImageBox->setRange(1,1000);
  mImageBox->setSingleStep(1);
  mImageBox->setFocusPolicy(Qt::ClickFocus);
  mImageBox->setKeyboardTracking(false);
  connect(mImageBox, SIGNAL(valueChanged(int)), this, 
	  SLOT(imageThicknessChanged(int)));
  mImageBox->setToolTip("Set number of slices to average (hot keys _  and +)");

  // Thickness of model spin box
  label = diaLabel("Mod", rightBox, spinHLay);
  label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  mModelBox = new QDoubleSpinBox(rightBox);
  spinHLay->addWidget(mModelBox);  
  mModelBox->setDecimals(1);
  mModelBox->setRange(0.1, 1000.);
  mModelBox->setSingleStep(1.0);
  mModelBox->setFocusPolicy(Qt::ClickFocus);
  mModelBox->setKeyboardTracking(false);
  connect(mModelBox, SIGNAL(valueChanged(double)), this, 
	  SLOT(modelThicknessChanged(double)));
  mModelBox->setToolTip("Set thickness of model to project onto image "
                "(hot keys 9 and 0");

  setToggleState(SLICER_TOGGLE_CENTER, funcs->mClassic);
  setFontDependentWidths();
  sFirstTime = false;

  // Need GLwidget next - this gets the defined format
  glFormat.setRgba(rgba);
  glFormat.setDoubleBuffer(doubleBuffer);
  glFormat.setDepth(enableDepth);
  mGLw = new SlicerGL(funcs, glFormat, this);
  
  // Set it as main widget, set focus, dock on top and bottom only
  setCentralWidget(mGLw);
  setFocusPolicy(Qt::StrongFocus);
}

// Does the boilerplate of adding a new toolbar
HotToolBar *SlicerWindow::makeToolBar(bool addBreak, int spacing, const char *caption)
{
  HotToolBar *toolBar = new HotToolBar(this);
  if (addBreak)
    addToolBarBreak();
  addToolBar(toolBar);
  toolBar->layout()->setSpacing(spacing);
  toolBar->setWindowTitle(imodCaption(caption));
  connect(toolBar, SIGNAL(keyPress(QKeyEvent *)), this,
          SLOT(toolKeyPress(QKeyEvent *)));
  connect(toolBar, SIGNAL(keyRelease(QKeyEvent *)), this,
          SLOT(toolKeyRelease(QKeyEvent *)));
  toolBar->setAllowedAreas(Qt::TopToolBarArea);
  return toolBar;
}

void SlicerWindow::setFontDependentWidths()
{
  int width = fontMetrics().width("99.9") + 24;
  int height = fontMetrics().height() + 6;
  mModelBox->setFixedWidth(width);
  //width = fontMetrics().width("999") + 24;
  mImageBox->setFixedWidth(width);
  diaSetButtonWidth(mHelpButton, ImodPrefs->getRoundedStyle(), 1.2, "Help");
  diaSetButtonWidth(mSaveAngBut, ImodPrefs->getRoundedStyle(), 1.2, "Save");
  diaSetButtonWidth(mNewRowBut, ImodPrefs->getRoundedStyle(), 1.3, "New");
  diaSetButtonWidth(mSetAngBut, ImodPrefs->getRoundedStyle(), 1.35, "Set");
  mHelpButton->setFixedHeight(height);
  for (width = 0; width < 2; width++) {
    diaSetButtonWidth(mLowHighButtons[width], ImodPrefs->getRoundedStyle(), 1.5, "Lo");
    mLowHighButtons[width]->setFixedHeight(height);
  }
}

void SlicerWindow::changeEvent(QEvent *e)
{
  QMainWindow::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    setFontDependentWidths();
}

// Show the save angle toolbar after deciding whether a break is needed
// from the toolbar before it by comparing sum of their lengths with window
// width.  But have to use hints - the actual size can be stretched
void SlicerWindow::showSaveAngleToolbar()
{
  QSize angHint = mSaveAngBar->sizeHint();
  HotToolBar *before = mTimeBar ? mTimeBar : mToolBar;
  QSize beforeHint = before->sizeHint();
  int needBreak = angHint.width() + beforeHint.width() >= width() ? 1 : 0;
  /* imodPrintStderr("anghint %d beforehint %d width %d needbreak %d mbreak "
     "%d\n", angHint.width(), beforeHint.width(), width(), needBreak,
     mBreakBeforeAngBar); */
  if (needBreak != mBreakBeforeAngBar) {
    //imodPrintStderr("Inserting or removing break %d\n", needBreak);
    if (needBreak)
      insertToolBarBreak(mSaveAngBar);
    else
      removeToolBarBreak(mSaveAngBar);
  }
  mBreakBeforeAngBar = needBreak;
  mSaveAngBar->show();
}

void SlicerWindow::zoomUp()
{
  mFuncs->stepZoom(1);
}

void SlicerWindow::zoomDown()
{
  mFuncs->stepZoom(-1);
}

void SlicerWindow::timeBack()
{
  mFuncs->stepTime(-1);
}

void SlicerWindow::timeForward()
{
  mFuncs->stepTime(1);
}


// A new zoom or section was entered - let slicer decide on limits and 
// refresh box
void SlicerWindow::newZoom()
{
  QString str = mZoomEdit->text();
  mFuncs->enteredZoom(atof(LATIN1(str)));
  setFocus();
}

// Repond to signals from the rotation tool
void SlicerWindow::rotationClicked(int deltaX, int deltaY, int deltaZ)
{
  mFuncs->rotateOnViewAxis(deltaX, deltaY, deltaZ);
}

void SlicerWindow::stepSizeChanged(int delta)
{
  slicerViewAxisStepChange(delta);
}

void SlicerWindow::shiftToggled(bool state)
{
  mFuncs->stateToggled(SLICER_TOGGLE_SHIFTLOCK, state ? 1 : 0);
}

// Respomd to spin box changes for image and model thickness
void SlicerWindow::imageThicknessChanged(int depth)
{
  mFuncs->imageThickness(depth);
  setFocus();
}

void SlicerWindow::modelThicknessChanged(double depth)
{
  mFuncs->modelThickness((float)depth);
  setFocus();
}

void SlicerWindow::help()
{
  mFuncs->help();
}

void SlicerWindow::angleChanged(int which, int value, bool dragging)
{
  mFuncs->angleChanged(which, value, dragging);
}

// One of toggle buttons needs to change state
void SlicerWindow::toggleClicked(int index)
{
  int state = mToggleButs[index]->isChecked() ? 1 : 0;
  mToggleStates[index] = state; 
  mFuncs->stateToggled(index, state);
}

void SlicerWindow::showslicePressed()
{
  mFuncs->showSlice();
}

void SlicerWindow::contourPressed()
{
  if (!mFuncs->anglesFromContour())
    mFuncs->checkMovieLimits();
}

void SlicerWindow::fillCachePressed()
{
  mFuncs->fillCache();
}

void SlicerWindow::lowHighClicked(int which)
{
  mFuncs->setBandLowHighLimit(which);
}

void SlicerWindow::saveAngClicked()
{
  mFuncs->setCurrentOrNewRow(false);
}

void SlicerWindow::setAngClicked()
{
  mFuncs->setAnglesFromRow();
}

void SlicerWindow::newRowClicked()
{
  mFuncs->setCurrentOrNewRow(true);
}

// Do not synchronize when this is turned on, direction is ambiguous
void SlicerWindow::continuousToggled(bool state)
{
  mFuncs->mContinuous = state;
}

void SlicerWindow::linkToggled(bool state)
{
  mFuncs->mLinked = state;
}

// Functions for setting state of the controls
void SlicerWindow::setAngles(float *angles)
{
  int axis, value;
  for (axis = 0; axis < 3; axis++) {
    value =  (int)floor((double)(angles[axis] * 10.0 + 0.5f));
    mSliders->setValue(axis, value);
  }
}

void SlicerWindow::setViewAxisPosition(int amin, int amax, int current)
{
  mSliders->setMinMaxVal(3, amin, amax, current);
}

void SlicerWindow::setModelThickness(float depth)
{
  diaSetDoubleSpinBox(mModelBox, depth);
}

void SlicerWindow::setImageThickness(int depth)
{
  diaSetSpinBox(mImageBox, depth);
}

// This allows slicer to set one of the buttons
void SlicerWindow::setToggleState(int index, int state)
{
  mToggleStates[index] = state ? 1 : 0;
  if (index == SLICER_TOGGLE_SHIFTLOCK)
    mRotationTool->setCenterState(state != 0);
  else
    diaSetChecked(mToggleButs[index], state != 0);
}

void SlicerWindow::setZoomText(float zoom)
{
  QString str;
  str.sprintf("%.4f", zoom);
  if (str.endsWith("00"))
    str.truncate(str.length() - 2);
  mZoomEdit->setText(str);
}

void SlicerWindow::setTimeLabel(int time, QString label)
{
  QString str;
  str.sprintf(" (%3d)", time);
  mTimeNumLabel->setText(str);
  mTimeLabel->setText(label);
}

void SlicerWindow::setLowHighValidity(int which, int state)
{
  QColor stateColors[3] = {QColor(255, 80, 110), QColor(255, 255, 0), QColor(96, 255,96)};
  if (state != mLowHighStates[which]) {
    B3DCLAMP(state, 0, 2);
    mLowHighStates[which] = state;
    diaSetWidgetColor(mLowHighButtons[which], stateColors[state]);
  }
}

void SlicerWindow::enableLowHighButtons(int enable)
{
  for (int ind = 0; ind < 2; ind++) {
    setLowHighValidity(ind, SLICER_LIMIT_INVALID);
    mLowHighActions[ind]->setVisible(enable != 0);
  }
}

void SlicerWindow::keyPressEvent ( QKeyEvent * e )
{
  mFuncs->keyInput(e);
}
void SlicerWindow::keyReleaseEvent (QKeyEvent * e )
{
  mFuncs->keyRelease(e);
}

// Whan a close event comes in, inform slicer, and accept
void SlicerWindow::closeEvent (QCloseEvent * e )
{
  mFuncs->closing();
  e->accept();
  delete mFuncs;
}

///////////////////////////////////////////////
// The GL widget

SlicerGL::SlicerGL(SlicerFuncs *funcs, QGLFormat inFormat, QWidget * parent)
  : QGLWidget(inFormat, parent)
{
  mMousePressed = false;
  mFuncs = funcs;
  mFirstDraw = true;
}

void SlicerGL::paintGL()
{
  if (mFirstDraw) {
    mTimerID = startTimer(10);
    mFirstDraw = false;
    if (mTimerID)
      return;
  }

  mFuncs->paint();
}

void SlicerGL::timerEvent(QTimerEvent * e )
{
  killTimer(mTimerID);
  updateGL();
}

void SlicerGL::resizeGL( int wdth, int hght )
{
  mFuncs->resize(wdth, hght);
}

void SlicerGL::mousePressEvent(QMouseEvent * e )
{
  mMousePressed = true;
  mFuncs->mousePress(e);
}

void SlicerGL::mouseMoveEvent(QMouseEvent * e )
{
  mFuncs->mouseMove(e);
}

void SlicerGL::mouseReleaseEvent ( QMouseEvent * e )
{
  mMousePressed = false;
  mFuncs->mouseRelease(e);
}

void SlicerGL::wheelEvent (QWheelEvent *e)
{
  if (iceGetWheelForSize())
    utilWheelChangePointSize(mFuncs->mVi, mFuncs->mZoom, e->delta());
  
}

///////////////////////////////////////////////
// The cube class

SlicerCube::SlicerCube(SlicerFuncs *funcs, QGLFormat inFormat, QWidget * parent)
  : QGLWidget(inFormat, parent)
{
  mFuncs = funcs;
}

void SlicerCube::paintGL()
{
  mFuncs->cubePaint();
}

void SlicerCube::resizeGL( int wdth, int hght )
{
  mFuncs->cubeResize(wdth, hght);
}
