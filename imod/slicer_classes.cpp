/*
 *  slicer_classes.cpp -- implements slicer mainwindow and QGLWidget classes.
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */
#include <stdlib.h>
#include <stdio.h>
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
#include <QDoubleSpinBox>
#include <qframe.h>
#include <qdatetime.h>
#include <qslider.h>
#include <qvalidator.h>
#ifdef QT_THREAD_SUPPORT
#include <qmutex.h>
#endif
//Added by qt3to4:
#include <QTimerEvent>
#include <QBoxLayout>
#include <QMouseEvent>
#include <QCloseEvent>
#include <QKeyEvent>
#include <QVBoxLayout>
#include "imod.h"
#include "hottoolbar.h"
#include "slicer_classes.h"
#include "sslice.h"
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

#define MAX_THREADS 16

static void fillArraySegment(int jstart, int jlimit);
static void findIndexLimits(int isize, int xsize, float xo, float xsx,
                            float offset, float *fstart, float *fend);

#include "unlock.bits"
#include "lock.bits"
#include "lowres.bits"
#include "highres.bits"
#include "image.bits"
#include "fft.bits"
#include "contour.bits"
#include "smartCenter.bits"
#include "keepCenter.bits"
#include "time_unlock.bits"
#include "time_lock.bits"
#include "shiftlockon.bits"
#include "shiftlockoff.bits"

static unsigned char showslice_bits[] = {
     0xff, 0x0f, 0xff, 0x0f, 0xff, 0x0f, 0x00, 0x00, 0xff, 0xef, 0xff, 0xef,
     0xff, 0xe7, 0xff, 0xe9, 0xff, 0xee, 0x7f, 0xef, 0x9f, 0xef, 0xef, 0xef,
     0xf7, 0xef, 0xf9, 0xef, 0xfe, 0xef, 0xff, 0xef};


static unsigned char *bitList[MAX_SLICER_TOGGLES][2] =
  { {lowres_bits, highres_bits},
    {unlock_bits, lock_bits},
    {smartCenter_bits, keepCenter_bits}, 
    {shiftlockoff_bits, shiftlockon_bits},
    {image_bits, fft_bits},
    {time_unlock_bits, time_lock_bits}};

static QIcon *icons[MAX_SLICER_TOGGLES];
static QIcon *showIcon;
static QIcon *contIcon;
static int firstTime = 1;
static char *toggleTips[] = {
    "Toggle between regular and high-resolution (interpolated) image",
    "Lock window at current position",
    "Keep current image or model point centered (classic mode, hot key k)",
    "Use keypad and mouse as if Shift key were down to rotate slice",
    "Toggle between showing image and FFT",
    "Lock window at current time" };

static char *sliderLabels[] = {"X rotation", "Y rotation", "Z rotation"};

SlicerWindow::SlicerWindow(SlicerStruct *slicer, float maxAngles[], 
                           QString timeLabel,
			   bool rgba, bool doubleBuffer, bool enableDepth,
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

  mSlicer = slicer;
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  setAnimated(false);
  if (firstTime) 
    utilBitListsToIcons(bitList, icons, MAX_SLICER_TOGGLES);
  
  // Get the toolbar
  mToolBar = new HotToolBar(this);
  addToolBar(mToolBar);
  if (!TB_AUTO_RAISE)
    mToolBar->layout()->setSpacing(4);
  connect(mToolBar, SIGNAL(keyPress(QKeyEvent *)), this,
	  SLOT(toolKeyPress(QKeyEvent *)));
  connect(mToolBar, SIGNAL(keyRelease(QKeyEvent *)), this,
	  SLOT(toolKeyRelease(QKeyEvent *)));

  // Zoom tools
  mZoomEdit = utilTBZoomTools(this, mToolBar, &upArrow, &downArrow);
  connect(upArrow, SIGNAL(clicked()), this, SLOT(zoomUp()));
  connect(downArrow, SIGNAL(clicked()), this, SLOT(zoomDown()));
  connect(mZoomEdit, SIGNAL(editingFinished()), this, SLOT(newZoom()));

  // Make the toggle buttons and their signal mapper
  QSignalMapper *toggleMapper = new QSignalMapper(mToolBar);
  connect(toggleMapper, SIGNAL(mapped(int)), this, SLOT(toggleClicked(int)));
  for (j = 0; j < 5; j++) {
    utilSetupToggleButton(mToolBar, mToolBar, NULL, toggleMapper, icons, 
                          toggleTips, mToggleButs, mToggleStates, j);
    connect(mToggleButs[j], SIGNAL(clicked()), toggleMapper, SLOT(map()));
  }
  
  // The showslice button is simpler
  if (firstTime) {
    showIcon = new QIcon(QBitmap::fromData(QSize(BM_WIDTH, BM_HEIGHT),
                                           showslice_bits));
    contIcon = new QIcon(QBitmap::fromData(QSize(BM_WIDTH, BM_HEIGHT),
                                           contour_bits));
  }
 
  utilTBToolButton(this, mToolBar, &button, "Show slice cutting lines in"
                   " Xyz and Zap windows (hot key l)");
  button->setIcon(*showIcon);
  connect(button, SIGNAL(clicked()), this, SLOT(showslicePressed()));

  utilTBToolButton(this, mToolBar, &button, "Set angles and position to show"
                   " plane of current contour (hot key W)");
  button->setIcon(*contIcon);
  connect(button, SIGNAL(clicked()), this, SLOT(contourPressed()));

  // The Z scale combo box
  mZscaleCombo = new QComboBox(this);
  mToolBar->addWidget(mZscaleCombo);
  QStringList scaleList;
  mZscaleCombo->addItem("Z-Scale Off");
  mZscaleCombo->addItem("Z-Scale Before");

  // Only allow scale after if there is no implicit scale from binning
  if (slicer->vi->xybin == slicer->vi->zbin)
    mZscaleCombo->addItem("Z-Scale After");
  mZscaleCombo->setFocusPolicy(Qt::NoFocus);
  connect(mZscaleCombo, SIGNAL(activated(int)), this, 
	  SLOT(zScaleSelected(int)));
  mZscaleCombo->setToolTip("Select whether to ignore Z scale, or apply it"
                " before or after rotation");
  mToolBar->setAllowedAreas(Qt::TopToolBarArea);

  // THE TIME TOOLBAR
  if (!timeLabel.isEmpty()) {
    mTimeBar = new HotToolBar(this);
    addToolBarBreak();
    addToolBar(mTimeBar);
    if (!TB_AUTO_RAISE)
      mTimeBar->layout()->setSpacing(4);
    connect(mTimeBar, SIGNAL(keyPress(QKeyEvent *)), this,
            SLOT(toolKeyPress(QKeyEvent *)));
    connect(mTimeBar, SIGNAL(keyRelease(QKeyEvent *)), this,
            SLOT(toolKeyRelease(QKeyEvent *)));

    check = diaCheckBox("Link", this, NULL);
    mTimeBar->addWidget(check);
    connect(check, SIGNAL(toggled(bool)), this, SLOT(linkToggled(bool)));
    check->setToolTip("Keep angles and positions same as for other linked "
                "slicers");

    j =  MAX_SLICER_TOGGLES - 1;
    utilSetupToggleButton(mTimeBar, mTimeBar, NULL, toggleMapper, icons, 
                          toggleTips, mToggleButs, mToggleStates, j);
    connect(mToggleButs[j], SIGNAL(clicked()), toggleMapper, SLOT(map()));

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

    mTimeLabel = new QLabel(timeLabel, this);
    mTimeBar->addWidget(mTimeLabel);
    mTimeBar->setAllowedAreas(Qt::TopToolBarArea);
  }

  // SET ANGLE TOOLBAR
  mSaveAngBar = new HotToolBar(this);
  mSaveAngBar->layout()->setSpacing(4);
  connect(mSaveAngBar, SIGNAL(keyPress(QKeyEvent *)), this,
	  SLOT(toolKeyPress(QKeyEvent *)));
  connect(mSaveAngBar, SIGNAL(keyRelease(QKeyEvent *)), this,
	  SLOT(toolKeyRelease(QKeyEvent *)));

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
  mSaveAngBar->setAllowedAreas(Qt::TopToolBarArea);
  addToolBar(mSaveAngBar);

  // SECOND TOOLBAR
  mToolBar2 = new HotToolBar(this);
  addToolBarBreak();
  addToolBar(mToolBar2);
  mToolBar2->layout()->setSpacing(4);
  connect(mToolBar2, SIGNAL(keyPress(QKeyEvent *)), this,
	  SLOT(toolKeyPress(QKeyEvent *)));
  connect(mToolBar2, SIGNAL(keyRelease(QKeyEvent *)), this,
	  SLOT(toolKeyRelease(QKeyEvent *)));

  // Make a frame, put a layout in it, and then put multisliders in the layout
  QWidget *sliderFrame = new QWidget(this);
  //  sliderFrame->setFrameStyle(QFrame::NoFrame);
  mToolBar2->addWidget(sliderFrame);
  QVBoxLayout *sliderLayout = new QVBoxLayout(sliderFrame);
  sliderLayout->setContentsMargins(0, 0, 0, 0);
  mSliders = new MultiSlider(sliderFrame, 3, sliderLabels, -1800,
                             1800, 1);
  for (j = 0; j < 3; j++) {
    int maxVal = (int)(10. * maxAngles[j] + 0.1);
    mSliders->setRange(j, -maxVal, maxVal);
    mSliders->getSlider(j)->setMinimumWidth(200);
    mSliders->getSlider(j)->setPageStep(10);
  }
  sliderLayout->addLayout(mSliders->getLayout());  
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this, 
	  SLOT(angleChanged(int, int, bool)));

  // A frame for the cube widget; and the cube with the default GL format
  QFrame *cubeFrame = new QFrame(this);
  mToolBar2->addWidget(cubeFrame);
  cubeFrame->setFixedWidth(100);
  cubeFrame->setFrameShadow(QFrame::Sunken);
  cubeFrame->setFrameShape(QFrame::StyledPanel);
  QVBoxLayout *cubeLayout = new QVBoxLayout(cubeFrame);
  cubeLayout->setContentsMargins(2, 2, 2, 2);
  mCube = new SlicerCube(slicer, glFormat, cubeFrame);
  cubeLayout->addWidget(mCube);

  // Thickness box and help button
  QWidget *thickBox = new QWidget(this);
  mToolBar2->addWidget(thickBox);
  QVBoxLayout *thickLay = new QVBoxLayout(thickBox);
  thickLay->setContentsMargins(0, 0, 0, 0);
  thickLay->setSpacing(0);
  mHelpButton = diaPushButton("Help", thickBox, thickLay);
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(help()));
  mHelpButton->setToolTip("Open help window");

  // Thickness of image spin box
  label = diaLabel("Image", thickBox, thickLay);
  QSize labelSize = label->sizeHint();
  mImageBox = new QSpinBox(thickBox);
  thickLay->addWidget(mImageBox);
  mImageBox->setRange(1,1000);
  mImageBox->setSingleStep(1);
  mImageBox->setFocusPolicy(Qt::ClickFocus);
  mImageBox->setKeyboardTracking(false);
  mImageBox->setMaximumWidth((int)(labelSize.width() * 1.5));
  connect(mImageBox, SIGNAL(valueChanged(int)), this, 
	  SLOT(imageThicknessChanged(int)));
  mImageBox->setToolTip("Set number of slices to average (hot keys _  and "
                "+)");

  // Thickness of model spin box
  label = diaLabel("Model", thickBox, thickLay);
  mModelBox = new QDoubleSpinBox(thickBox);
  thickLay->addWidget(mModelBox);  
  mModelBox->setDecimals(1);
  mModelBox->setRange(0.1, 1000.);
  mModelBox->setSingleStep(1.0);
  mModelBox->setFocusPolicy(Qt::ClickFocus);
  mModelBox->setKeyboardTracking(false);
  mModelBox->setMaximumWidth((int)(labelSize.width() * 1.5));
  connect(mModelBox, SIGNAL(valueChanged(double)), this, 
	  SLOT(modelThicknessChanged(double)));
  mModelBox->setToolTip("Set thickness of model to project onto image "
                "(hot keys 9 and 0");
  mToolBar2->setAllowedAreas(Qt::TopToolBarArea);

  setToggleState(SLICER_TOGGLE_CENTER, slicer->classic);
  setFontDependentWidths();
  firstTime = 0;

  // Need GLwidget next - this gets the defined format
  glFormat.setRgba(rgba);
  glFormat.setDoubleBuffer(doubleBuffer);
  glFormat.setDepth(enableDepth);
  mGLw = new SlicerGL(slicer, glFormat, this);
  
  // Set it as main widget, set focus, dock on top and bottom only
  setCentralWidget(mGLw);
  setFocusPolicy(Qt::StrongFocus);

}

void SlicerWindow::setFontDependentWidths()
{
  diaSetButtonWidth(mHelpButton, ImodPrefs->getRoundedStyle(), 1.2, "Help");
  diaSetButtonWidth(mSaveAngBut, ImodPrefs->getRoundedStyle(), 1.2, "Save");
  diaSetButtonWidth(mNewRowBut, ImodPrefs->getRoundedStyle(), 1.3, "New");
  diaSetButtonWidth(mSetAngBut, ImodPrefs->getRoundedStyle(), 1.35, "Set");
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
  slicerStepZoom(mSlicer, 1);
}

void SlicerWindow::zoomDown()
{
  slicerStepZoom(mSlicer, -1);
}

void SlicerWindow::timeBack()
{
  slicerStepTime(mSlicer, -1);
}

void SlicerWindow::timeForward()
{
  slicerStepTime(mSlicer, 1);
}


// A new zoom or section was entered - let slicer decide on limits and 
// refresh box
void SlicerWindow::newZoom()
{
  QString str = mZoomEdit->text();
  slicerEnteredZoom(mSlicer, atof(LATIN1(str)));
  setFocus();
}

// Respomd to spin box changes for image and model thickness
void SlicerWindow::imageThicknessChanged(int depth)
{
  slicerImageThickness(mSlicer, depth);
  setFocus();
}

void SlicerWindow::modelThicknessChanged(double depth)
{
  slicerModelThickness(mSlicer, (float)depth);
  setFocus();
}

void SlicerWindow::help()
{
  slicerHelp();
}

void SlicerWindow::angleChanged(int which, int value, bool dragging)
{
  slicerAngleChanged(mSlicer, which, value, dragging);
}

// One of toggle buttons needs to change state
void SlicerWindow::toggleClicked(int index)
{
  int state = mToggleButs[index]->isChecked() ? 1 : 0;
  mToggleStates[index] = state; 
  slicerStateToggled(mSlicer, index, state);
}

void SlicerWindow::showslicePressed()
{
  slicerShowSlice(mSlicer);
}

void SlicerWindow::contourPressed()
{
  if (!slicerAnglesFromContour(mSlicer))
    slicerCheckMovieLimits(mSlicer);
}

void SlicerWindow::zScaleSelected(int item)
{
  slicerZscale(mSlicer, item);
}

void SlicerWindow::saveAngClicked()
{
  slicerSetCurrentOrNewRow(mSlicer, false);
}

void SlicerWindow::setAngClicked()
{
  slicerSetAnglesFromRow(mSlicer);
}

void SlicerWindow::newRowClicked()
{
  slicerSetCurrentOrNewRow(mSlicer, true);
}

// Do not synchronize when this is turned on, direction is ambiguous
void SlicerWindow::continuousToggled(bool state)
{
  mSlicer->continuous = state;
}

void SlicerWindow::linkToggled(bool state)
{
  mSlicer->linked = state;
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

void SlicerWindow::setTimeLabel(QString label)
{
  mTimeLabel->setText(label);
}

void SlicerWindow::keyPressEvent ( QKeyEvent * e )
{
  slicerKeyInput(mSlicer, e);
}
void SlicerWindow::keyReleaseEvent (QKeyEvent * e )
{
  slicerKeyRelease(mSlicer, e);
}

// Whan a close event comes in, inform slicer, and accept
void SlicerWindow::closeEvent (QCloseEvent * e )
{
  slicerClosing(mSlicer);
  e->accept();
}

///////////////////////////////////////////////
// The GL widget

SlicerGL::SlicerGL(SlicerStruct *slicer, QGLFormat inFormat, QWidget * parent)
  : QGLWidget(inFormat, parent)
{
  mMousePressed = false;
  mSlicer = slicer;
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

  slicerPaint(mSlicer);
}

void SlicerGL::timerEvent(QTimerEvent * e )
{
  killTimer(mTimerID);
  updateGL();
}

void SlicerGL::resizeGL( int wdth, int hght )
{
  slicerResize(mSlicer, wdth, hght);
}

void SlicerGL::mousePressEvent(QMouseEvent * e )
{
  mMousePressed = true;
  slicerMousePress(mSlicer, e);
}

void SlicerGL::mouseMoveEvent(QMouseEvent * e )
{
  slicerMouseMove(mSlicer, e);
}

void SlicerGL::mouseReleaseEvent ( QMouseEvent * e )
{
  mMousePressed = false;
  slicerMouseRelease(mSlicer, e);
}

///////////////////////////////////////////////
// The cube class

SlicerCube::SlicerCube(SlicerStruct *slicer, QGLFormat inFormat,
                       QWidget * parent)
  : QGLWidget(inFormat, parent)
{
  mSlicer = slicer;
}

void SlicerCube::paintGL()
{
  slicerCubePaint(mSlicer);
}

void SlicerCube::resizeGL( int wdth, int hght )
{
  slicerCubeResize(mSlicer, wdth, hght);
}

////////////////////////////////////////////////
// Array filling functions and threads.
// Here so the statics won't interfere with the namespace in slicer.cpp

// The thread class saves the limits in Y and run calls the fill with limits
#ifdef QT_THREAD_SUPPORT
SlicerThread::SlicerThread(int jStart, int jLimit)
{
  mJstart = jStart;
  mJlimit = jLimit;
}

void SlicerThread::run()
{
  fillArraySegment(mJstart, mJlimit);
}

static QMutex fillMutex;
#endif

// Statics are read-only data for the threads to use
static float xsx, ysx, zsx; /* steps for moving x in zap. */
static float xsy, ysy, zsy; /* steps for moving y in zap. */
static float xsz, ysz, zsz; /* steps for moving z in zap. */
static int xsize;
static int ysize;
static int zsize;
static float xzost, yzost, zzost;
static int izoom, shortcut, ilimshort, jlimshort;
static int isize, jsize, ksize;
static unsigned char noDataVal;
static b3dUInt16 *cidata;
static int maxval, minval;
static int sshq, sswinx;


// The top-level routine called to fill the image array
void fillImageArray(SlicerStruct *ss, int panning, int meanOnly)
{
  float maxPanPixels = 1000. * ImodPrefs->slicerPanKb();
  int i, j, k;
  unsigned short rbase;
  float xo, yo, zo;  /* coords of the lower left zap origin. */
  float xs = 1.0f, ys = 1.0f, zs = 1.0f;  /* scale factors.  */
  float zoffset, sample, scale, offset, sumSD, sumMean, edge;
  float matt = 0.1;
  int ixStart, iyStart, nxUse, nyUse, ntaper;
  float numPosSD = 4., numNegSD = 3.;

  float xzoom, yzoom, zzoom;
  float zoom = ss->zoom;
  int iz, maxSlice;
  float extrashift, numpix;
  int crossget, crosswant;
  Ipoint pnt, tpnt;
  Imat *mat = ss->mat;

  int cindex, pixsize;
  unsigned int *cmap = App->cvi->cramp->ramp;
  b3dUByte **imdata;
  b3dUByte *bdata;
  b3dUInt32 *idata;
  Islice *slice;
  unsigned char *sclmap = NULL;
  unsigned char *fftmap;
  unsigned char **linePtrs;
  int vmnullvalue;
  int numThreads = 1;
  char *procChar = NULL;
#ifdef QT_THREAD_SUPPORT
  SlicerThread *threads[MAX_THREADS];
#endif  

  QTime fillTime;
  fillTime.start();
  if (!ss->image)
      return;
  cidata = ss->image->id1;
  idata = (b3dUInt32 *)cidata;
  pixsize  = b3dGetImageType(NULL, NULL);

  xsize = ss->vi->xsize;
  ysize = ss->vi->ysize;
  zsize = ss->vi->zsize;
  noDataVal = 0;
  maxval = 255;
  minval = 0;
  sswinx = ss->winx;
  izoom = (int) zoom;

  // Turn off hq drawing if mean only.
  // Set flag for filling each window pixel if HQ or fractional zoom, unless
  // it is FFT at higher zoom
  // Set flag for doing shortcut HQ method
  // Set number of slices to draw
  sshq = meanOnly ? 0 : ss->hq;
  ss->noPixelZoom = (sshq || izoom != ss->zoom) &&
    (!ss->fftMode || ss->zoom < 1.);
  shortcut = (sshq && (zoom == izoom) && (zoom > 1.0) && !ss->fftMode) ? 1 : 0;
  ksize = (ss->vi->colormapImage || meanOnly) ? 1 : ss->nslice;
 
  // Set up number of threads early so pan limit can be modified
  // Start with ideal number of threads and modify with environment variable
  numThreads = QThread::idealThreadCount();
#ifdef QT_THREAD_SUPPORT
  procChar = getenv("IMOD_PROCESSORS");
  if (procChar)
    numThreads = atoi(procChar);
  numThreads = B3DMIN(MAX_THREADS, B3DMAX(1, numThreads));
  maxPanPixels *= numThreads;
#endif

  // When panning, drop HQ and/or reduce number of slices to maintain
  // a maximum number of pixel have to be found
  if (panning) {

    // Get number of pixels to be computed: number in window, divided by
    // zoom**2 if pixel zoom or shortcut
    numpix = ss->winx * ss->winy;
    if (!ss->noPixelZoom || shortcut)
      numpix /= ss->zoom * ss->zoom;

    // If one slice and it is hq, it is 4 times slower unless shortcut, where
    // it may still be 2 times slower.  Cancel hq if too many pixels
    if (ksize == 1 && sshq) {
      if (shortcut)
        numpix *= 2;
      else
        numpix *= 4;
      if (numpix > maxPanPixels) {
        sshq = 0;
        shortcut = 0;
        ss->noPixelZoom = izoom != ss->zoom && (!ss->fftMode || ss->zoom < 1.);
      }
    } else if (ksize > 1) {

      // If multiple slices, assume shortcut is fully effective and increase
      // work just for the HQ interpolation
      if (sshq)
        numpix *= 4;
      maxSlice = B3DMAX(1, (int)(maxPanPixels / numpix));

      // Cancel HQ if too many pixels, or if the maximum slices is dropping
      // by more than 25%
      if (sshq && (maxPanPixels / numpix < 0.75 || 
                   ((float)maxSlice / ksize < 0.75))) {
        sshq = 0;
        shortcut = 0;

        // Re-evaluate flag and number of slices
        ss->noPixelZoom = izoom != ss->zoom && (!ss->fftMode || ss->zoom < 1.);
        numpix = ss->winx * ss->winy;
        if (!ss->noPixelZoom)
          numpix /= ss->zoom * ss->zoom;
        maxSlice = B3DMAX(1, (int)(maxPanPixels / numpix));
      }
      ksize = B3DMIN(ksize, maxSlice);
    }
    if (imodDebug('s'))
      imodPrintStderr("panning HQ %d ksize %d\n", sshq, ksize);
  }
  zoffset = (float)(ksize - 1) * 0.5;

  /* DNM 5/16/02: force a cache load of the current z slice at least */
  iz = B3DNINT(ss->cz);
  ivwGetZSection(ss->vi, iz);

  /* Set up image pointer tables */
  vmnullvalue = (App->cvi->white + App->cvi->black) / 2;
  if (ivwSetupFastAccess(ss->vi, &imdata, vmnullvalue, &i, 
                         ss->timeLock ? ss->timeLock : ss->vi->ct))
    return;

  noDataVal = vmnullvalue;

  slice_trans_step(ss);
  rbase = ss->vi->rampbase;
  if (!App->rgba && App->depth == 8){
    minval = ss->vi->rampbase;
    maxval = minval + ss->vi->rampsize;
    noDataVal = (unsigned char)minval;
  }


  /* DNM 5/5/03: set lx, ly, lz when cx, cy, cz used to fill array */
  xzoom = yzoom = zzoom = ss->zoom;
  xo = ss->lx = ss->cx;
  yo = ss->ly = ss->cy;
  zo = ss->lz = ss->cz;
  ss->lang[0] = ss->tang[0];
  ss->lang[1] = ss->tang[1];
  ss->lang[2] = ss->tang[2];

  xsx = ss->xstep[b3dX];
  ysx = ss->xstep[b3dY];
  zsx = ss->xstep[b3dZ];
  xsy = ss->ystep[b3dX];
  ysy = ss->ystep[b3dY];
  zsy = ss->ystep[b3dZ];
  xsz = ss->zstep[b3dX];
  ysz = ss->zstep[b3dY];
  zsz = ss->zstep[b3dZ];

  zs = 1.0f / slicerGetZScaleBefore(ss);
  // if ((ss->scalez) && (ss->vi->imod->zscale > 0))
  //  zs  = 1.0f/ss->vi->imod->zscale;

  if (ss->scalez == SLICE_ZSCALE_AFTER){
    if (ss->vi->imod->zscale > 0)
      zs = ss->vi->imod->zscale;
    xzoom = zoom * sqrt((double)
                        ((xsx * xsx + ysx * ysx + zsx * zsx * zs * zs)/
                         (xsx * xsx + ysx * ysx + zsx * zsx)));
    yzoom = zoom * sqrt((double)
                        ((xsy * xsy + ysy * ysy + zsy * zsy * zs * zs)/
                         (xsy * xsy + ysy * ysy + zsy * zsy)));
    zzoom = zoom * sqrt((double)
                        ((xsz * xsz + ysz * ysz + zsz * zsz * zs * zs)/
                         (xsz * xsz + ysz * ysz + zsz * zsz)));
          
    xs = zoom / xzoom;
    ys = zoom / yzoom;
    zs = 1.0;
  }

  /* size of 2-D loop for i, j */
  /* DNM: don't use xzoom, yzoom; make pixels be zoom x zoom */
  isize = (int)(ss->winx / zoom + 0.9);
  jsize = (int)(ss->winy / zoom + 0.9);

  if (ss->noPixelZoom) {
    /* high quality image or fractional zoom */
    isize = ss->winx; /* calculate each pixel for zoom unless FFT. */
    jsize = ss->winy;
    xsx /= xzoom;
    ysx /= xzoom;
    zsx /= xzoom / zs; 
    xsy /= yzoom;
    ysy /= yzoom;
    zsy /= yzoom / zs;
    xo -= (isize / 2) * xsx;
    yo -= (isize / 2) * ysx;
    zo -= (isize / 2) * zsx;
    xo -= (jsize / 2) * xsy;
    yo -= (jsize / 2) * ysy;
    zo -= (jsize / 2) * zsy;
    ss->xshift = 0;
    ss->yshift = 0;
  }else{
    xsx *= zoom / xzoom;
    ysx *= zoom / xzoom;
    zsx *= zs * zoom / xzoom;
    xsy *= zoom / yzoom;
    ysy *= zoom / yzoom;
    zsy *= zs * zoom / yzoom;

    /* Take fractional location of data point within a pixel and
       rotate in 3D to find location in display pixel */

    slicerSetForwardMatrix(ss);

    pnt.x = ss->cx - (int)ss->cx;
    pnt.y = ss->cy - (int)ss->cy;
    pnt.z = ss->cz - (int)ss->cz;
    imodMatTransform3D(mat, &pnt, &tpnt);
          
    if (tpnt.x < 0.0)
      tpnt.x += 1.0;
    if (tpnt.y < 0.0)
      tpnt.y += 1.0;

    /* Compute where we want the crosshair to come out in the central
       pixel, and where it will fall with no raster offset, use 
       difference to set raster offset */

    crosswant = (int)(zoom * tpnt.x);  /* don't take nearest int here! */
    if (crosswant >= zoom)
      crosswant -= (int)zoom;
    crossget = (ss->winx / 2) % (int)zoom;
    ss->xshift = crossget - crosswant;
    if (ss->xshift < 0)
      ss->xshift += zoom;

    crosswant = (int)(zoom * tpnt.y);
    if (crosswant >= zoom)
      crosswant -= (int)zoom;
    crossget = (ss->winy / 2) % (int)zoom;
    ss->yshift = crossget - crosswant;
    if (ss->yshift < 0)
      ss->yshift += zoom;

    extrashift = 0.5;      /* Needed for proper sampling */
    if (zoom == 1.0)
      extrashift = 0.0;

    xo -= ((ss->winx / 2 - ss->xshift) / zoom - extrashift) * xsx;
    yo -= ((ss->winx / 2 - ss->xshift) / zoom - extrashift) * ysx;
    zo -= ((ss->winx / 2 - ss->xshift) / zoom - extrashift) * zsx;
    xo -= ((ss->winy / 2 - ss->yshift) / zoom - extrashift) * xsy;
    yo -= ((ss->winy / 2 - ss->yshift) / zoom - extrashift) * ysy;
    zo -= ((ss->winy / 2 - ss->yshift) / zoom - extrashift) * zsy;
  }

  /* steps per step in Z are independent of HQ versus regular */
  xsz *= zoom / zzoom;
  ysz *= zoom / zzoom;
  zsz *= zs * zoom / zzoom;

  /* Save values of starting position */
  ss->xo = xo;
  ss->yo = yo;
  ss->zo = zo;

  /* Adjust for multiple slices */
  xo -= zoffset * xsz;
  yo -= zoffset * ysz;
  zo -= zoffset * zsz;
  xzost = xo; 
  yzost = yo;
  zzost = zo;

  if (shortcut) {
    ilimshort = izoom * ((isize - 1) / izoom - 1);
    jlimshort = izoom * ((jsize - 1) / izoom - 1);
  } else if (!izoom) {
    /* DNM 11/22/01: workaround to Intel compiler bug - it insists on
       doing j % izoom even when shortcut is 0 */ 
    izoom = 1;
  }

  if (imodDebug('s'))
    imodPrintStderr("winx %d winy %d isize %d jsize %d shortcut %d ilimshort"
                    " %d jlimshort %d\n", ss->winx, ss->winy, isize, jsize, 
                    shortcut, ilimshort, ilimshort);
  //  int timeStart = imodv_sys_time();
  /* DNM: don't need to clear array in advance */

#ifdef QT_THREAD_SUPPORT

  // Use threads if image is big enough
  if (numThreads > 1 && jsize > 8 * numThreads && 
      isize * ksize > 200000 / jsize) {
    if (imodDebug('s'))
      imodPrintStderr("%d threads - ", numThreads);
    for (i = 0; i < numThreads; i++) {
      threads[i] = new SlicerThread((i * jsize) / numThreads, 
                                    ((i + 1) * jsize) / numThreads);
      threads[i]->start();
    }

    // Wait for all threads to finish
    for (i = 0; i < numThreads; i++)
      threads[i]->wait();

  } else
    fillArraySegment(0, jsize);

#else
  fillArraySegment(0, jsize);
#endif

  if (shortcut) {

    /* DNM 1/9/03: deleted quadratic interpolation code, turned cubic code
       into a routine that can be used by tumbler */
    slicerCubicFillin(cidata, ss->winx, ss->winy, izoom, ilimshort, jlimshort,
		      minval * ksize, maxval * ksize);
  }

  // If computing mean only or scaling to mean, get the mean and SD of the
  // slice with some edges cut off
  if (meanOnly || (ss->scaleToMeanSD && ksize > 1) || ss->fftMode) {
    linePtrs = ivwMakeLinePointers(ss->vi, (unsigned char *)cidata, ss->winx, 
                                   jsize, MRC_MODE_USHORT);
    sumSD = 0.;
    if (linePtrs) {
      ixStart = matt * isize;
      nxUse = isize - 2 * ixStart;
      iyStart = matt * jsize;
      nyUse = jsize - 2 * iyStart;
      sample = 10000.0/(((double)nxUse) * nyUse);
      if (sample > 1.0)
        sample = 1.0;
      if (sampleMeanSD(linePtrs, 2, isize, jsize, sample, ixStart, iyStart,
                       nxUse, nyUse, &sumMean, &sumSD))
        sumSD = 0.;
      if (imodDebug('s'))
        imodPrintStderr("Array %d x %d mean %f Sd %f\n", isize, jsize, 
                        sumMean, sumSD);
    }

    // If getting mean only, set values and flag and return now
    if (meanOnly) {
      ss->oneSliceMean = sumMean;
      ss->oneSliceSD = sumSD;
      ss->scaleToMeanSD = true;
      if (imodDebug('s'))
        imodPrintStderr("Mean/SD time %d\n", fillTime.elapsed());
      return;
    }
  }

  // imodPrintStderr("%d msec\n", imodv_sys_time() - timeStart);
  cindex = ss->image->width * ss->image->height;
  k = ksize;

  if (k > 1) {
    scale = 1. / k;
    offset = 0.;

    // If scaling to match one slice, set the scaling and adjust the mean and
    // SD to be after the scaling, for FFT scaling
    if (ss->scaleToMeanSD && sumSD > 0.1 && ss->oneSliceSD > 0.1) {
      scale = ss->oneSliceSD / sumSD;
      offset  = ss->oneSliceMean - sumMean * scale;
      sumMean = ss->oneSliceMean;
      sumSD = ss->oneSliceSD;
    } else if (ss->fftMode) {

      // If just doing FFT, divide mean and SD by # of slices
      sumMean /= k;
      sumSD /= k;
    }
    sclmap = get_short_map(scale, offset, 0, 255, MRC_RAMP_LIN, 0, 0);
    if (!sclmap) {
      wprint("\aMemory error getting mapping array.\n");
      return;
    }
  }

  // Take FFT if flag is set
  if (ss->fftMode) {
    slice = sliceCreate(isize, jsize, SLICE_MODE_BYTE);
    if (slice) {

      // Pack data into a byte slice
      bdata = slice->data.b;
      for (j = 0; j < jsize; j++) {
        if (k > 1)
          for (i = j * ss->winx; i < j * ss->winx + isize; i++)
            *bdata++ = sclmap[cidata[i]];
        else
          for (i = j * ss->winx; i < j * ss->winx + isize; i++)
            *bdata++ = (b3dUByte)cidata[i];
      }
      slice->min = minval;
      slice->max = maxval;
      if (imodDebug('s'))
        imodPrintStderr("Fill time %d\n", fillTime.elapsed());
      fillTime.start();

      // Taper the edges 
      ntaper = (int)(0.05 * B3DMAX(isize, jsize));
      if (ntaper)
        sliceTaperAtFill(slice, ntaper, 1);
      if (imodDebug('s'))
        imodPrintStderr("Taper time %d  extent %d\n", fillTime.elapsed(),
                        ntaper);
      fillTime.start();

      // Take the FFT
      if (sliceByteBinnedFFT(slice, 1, 0, isize - 1, 0, jsize - 1, &i, &j) 
          > 0) {

        if (imodDebug('s'))
          imodPrintStderr("FFT time %d\n", fillTime.elapsed());
        fillTime.start();

        // Get the edge mean
        bdata = slice->data.b;
        edge = 0.;
        for (i = 0; i < isize; i++)
          edge += bdata[i] + bdata[i + isize * (jsize - 1)];
        for (j = 0; j < jsize; j++)
          edge += bdata[isize * j] + bdata[isize - 1 + isize * j];
        edge /= 2. * (isize + jsize);

        // Unpack the FFT data into the integer array
        // Map edge to mean - numNeg SD's, 255 to mean + numPos SD's
        scale = 1. / k;
        offset = 0.;
        if (sumSD > 0.1 && edge < 254.5) {
          scale = (numPosSD + numNegSD) * sumSD / (255. - edge);
          offset = sumMean - scale * edge - numNegSD * sumSD;
        }
        if (imodDebug('s'))
          imodPrintStderr("FFT edge %f  scale %f  offset %f\n", edge, scale,
                          offset);
        fftmap = get_byte_map(scale, offset, 0, 255);
        for (j = 0; j < jsize; j++)
          for (i = j * ss->winx; i < j * ss->winx + isize; i++)
            cidata[i] = fftmap[*bdata++];
      }
      sliceFree(slice);
      k = 1;
    }
  }

  /* for 8-bit displays, range is less then 256 gray scales. */
  if (!App->rgba && App->depth == 8){
    int tval;
    int minval = ss->vi->rampbase;
    int maxval = minval + ss->vi->rampsize;
    if (k > 1)
      for (j = 0; j < jsize; j++)
        for(i = j * ss->winx; i < j * ss->winx + isize; i++){
          tval = sclmap[cidata[i]];
          if (tval > maxval) tval = maxval;
          if (tval < minval) tval = minval;
          cidata[i] = tval;
        }
    else
      for (j = 0; j < jsize; j++)
        for(i = j * ss->winx; i < j * ss->winx + isize; i++){
          if (cidata[i] > maxval) cidata[i] = maxval;
          if (cidata[i] < minval) cidata[i] = minval;
        }

  }else{
    switch (pixsize){
    case 1:
      if (k > 1)
        for (j = 0; j < jsize; j++)
          for(i = j * ss->winx; i < j * ss->winx + isize; i++){
            cidata[i] = sclmap[cidata[i]];
          }
      break;
    case 2:
      if (k > 1)
        for (j = 0; j < jsize; j++)
          for(i = j * ss->winx; i < j * ss->winx + isize; i++){
            cidata[i] = sclmap[cidata[i]] + rbase;
          }
      else
        for (j = 0; j < jsize; j++)
          for(i = j * ss->winx; i < j * ss->winx + isize; i++){
            cidata[i] = cidata[i] + rbase;
          }
      break;
    case 4:
      if (k > 1)
        for (j = jsize - 1; j >= 0; j--)
          for(i = j * ss->winx + isize - 1; i >= j * ss->winx; i--){
            idata[i] = cmap[sclmap[cidata[i]]];
          }
      else
	for (j = jsize - 1; j >= 0; j--) {
	  for(i = j * ss->winx + isize - 1; i >= j * ss->winx; i--){
	    idata[i] = cmap[cidata[i]];
	  }
	}
    }
  }
  ss->xzoom = xzoom;
  ss->yzoom = yzoom;
  if (sclmap)
    free(sclmap);
  if (imodDebug('s'))
    imodPrintStderr("Fill time %d\n", fillTime.elapsed());
  return;
}

// Find the limits that need to be filled for a particular line
static void findIndexLimits(int isize, int xsize, float xo, float xsx,
                            float offset, float *fstart, float *fend)
{
  float flower, fupper, ftmp;
  float endCoord = xo + (isize - 1) * xsx + offset;
  float startCoord = xo + offset;
 
  /*if (imodDebug('s')) 
    imodPrintStderr("xo = %f, xsx = %f, start = %f, end = %f\n", xo,xsx,startCoord, endCoord); */
  /* If start and end is all to one side of data, set limits to middle to skip
     the line */
  if ((startCoord < 0 && endCoord < 0) || 
      (startCoord >= xsize && endCoord >= xsize)) {
    *fstart = isize / 2.;
    *fend = *fstart;
 
    /* Otherwise evaluate place where line cuts volume for this coordinate */
  } else if (xsx > 1.e-6 || xsx < -1.e-6) {
    flower = -(startCoord + 1.) / xsx;
    fupper = (xsize - startCoord) / xsx;
    if (xsx < 0) {
      ftmp = flower;
      flower = fupper;
      fupper = ftmp;
    }
    /* if (imodDebug('s')) 
       imodPrintStderr("lower = %f, upper = %f\n", flower, fupper); */
    if (flower > *fstart)
      *fstart = flower;
    if (fupper < *fend)
      *fend = fupper;
  }
}

// Fill a portion of the array from jstart to jlimit - 1
// This routine is called by the threads
static void fillArraySegment(int jstart, int jlimit)
{
  int i, j, k, cindex, ishort;
  int xi, yi, zi;
  float xo, yo, zo;  /* coords of the lower left origin. */
  float x, y, z; /* coords of pixel in 3-D image block. */
  float xzo, yzo,zzo;
  int innerStart, innerEnd, outerStart, outerEnd;
  float fstart, fend;

  /* for 3-D quadratic interpolation */
  float dx, dy, dz;
  float x1, x2, y1, y2, z1, z2;
  float a, b, c, d, e, f;
  float ival;
  int pxi, nxi, pyi, nyi, pzi, nzi;
  unsigned char val;

  xzo = xzost;
  yzo = yzost;
  zzo = zzost;


  for(k = 0; k < ksize; k++){
    xo = xzo;
    yo = yzo;
    zo = zzo;

    // advance coordinates to get to j start
    for (j = 0; j < jstart; j++) {
      xo += xsy;
      yo += ysy;
      zo += zsy;
    }    
          
    /* (i,j) location in zap window data. */
    for (j = jstart; j < jlimit; j++){

#ifdef QT_THREAD_SUPPORT
      // Take the lock for working on first or last line (superstition)
      if (j == jstart || j == jlimit - 1)
        fillMutex.lock();
#endif

      /* Compute starting and ending index that intersects data volume
         for each dimension, and find smallest range of indexes */
      fstart = 0;
      fend = isize;
      findIndexLimits(isize, xsize, xo, xsx, 0., &fstart, &fend);
      findIndexLimits(isize, ysize, yo, ysx, 0., &fstart, &fend);
      findIndexLimits(isize, zsize, zo, zsx, 0.5, &fstart, &fend);

      /* If there is no range, set up for fills to cover the range */
      if (fstart >= fend) {
        outerStart = isize / 2;
        innerStart = innerEnd = outerEnd = outerStart;
      } else {

        /* Otherwise, set outer region safely outside the index limits */
        outerStart = fstart - 2.;
        if (outerStart < 0)
          outerStart = 0;
        outerEnd = fend + 2.;
        if (outerEnd > isize)
          outerEnd = isize;

        /* If not doing HQ, compute inner limits of region that needs no
           testing */
        if (!sshq) {
          innerStart = outerStart + 4;
          innerEnd = outerEnd - 4;
          if (innerStart >= innerEnd)
            innerStart = innerEnd = outerStart;
          
        } else if (shortcut) {
          /* If doing shortcuts, set up for whole line if it is a line to skip
             or make sure start is a multiple of the zoom */
          if (j >= izoom && j < jlimshort && j % izoom) {
            outerStart = 0;
            outerEnd = isize;
          } else
            outerStart = izoom * (outerStart / izoom);
        }

      }

      cindex = j * sswinx;
      
      /* Fill outer regions */
      
      if (k) {
        for (i = 0; i < outerStart; i++)
          cidata[i + cindex] += noDataVal;
        for (i = outerEnd; i < isize; i++)
          cidata[i + cindex] += noDataVal;
      } else {
        for (i = 0; i < outerStart; i++)
          cidata[i + cindex] = noDataVal;
        for (i = outerEnd; i < isize; i++)
          cidata[i + cindex] = noDataVal;
      }

      x = xo + outerStart * xsx;
      y = yo + outerStart * ysx;
      z = zo + outerStart * zsx;

      if (sshq) {
        /* For HQ, do tests all the time since they are minor component */
        for (i = outerStart; i < outerEnd; i++) {

          /* DNM & RJG 2/12/03: remove floor calls - they are dog-slow only
             Pentium 4 below 2.6 GHz... */
          xi = (int)x;
          yi = (int)y;
          zi = (int)(z + 0.5);
                    
          if (xi >= 0 && xi < xsize && yi >= 0 && yi < ysize &&
              z > -0.5 && zi < zsize) {
            val = (*ivwFastGetValue)(xi, yi, zi);

            /* do quadratic interpolation. */
            dx = x - xi - 0.5;
            dy = y - yi - 0.5;
            dz = z - zi;
                              
            pxi = xi - 1;
            nxi = xi + 1;
            pyi = yi - 1;
            nyi = yi + 1;
            pzi = zi - 1;
            nzi = zi + 1;
                              
            if (pxi < 0) pxi = 0;
            if (nxi >= xsize) nxi = xi;
            if (pyi < 0) pyi = 0;
            if (nyi >= ysize) nyi = yi;
            if (pzi < 0) pzi = 0;
            if (nzi >= zsize) nzi = zi;
                            
            x1 = (*ivwFastGetValue)(pxi,  yi,  zi);
            x2 = (*ivwFastGetValue)(nxi,  yi,  zi);
            y1 = (*ivwFastGetValue)( xi, pyi,  zi);
            y2 = (*ivwFastGetValue)( xi, nyi,  zi);
            z1 = (*ivwFastGetValue)( xi,  yi, pzi);
            z2 = (*ivwFastGetValue)( xi,  yi, nzi);
                              
            a = (x1 + x2) * 0.5f - (float)val;
            b = (y1 + y2) * 0.5f - (float)val;
            c = (z1 + z2) * 0.5f - (float)val;
            d = (x2 - x1) * 0.5f;
            e = (y2 - y1) * 0.5f;
            f = (z2 - z1) * 0.5f;
            ival = (a * dx * dx) + 
              (b * dy * dy) + 
              (c * dz * dz) +
              (d * dx) + (e * dy) + 
              (f * dz) + (float)val;
            if (ival > maxval)
              ival = maxval;
            if (ival < minval)
              ival = minval;
            val = (unsigned char)(ival + 0.5f);
                              
          } else
            val = noDataVal;
                    
          if (k)
            cidata[i + cindex] += val;
          else
            cidata[i + cindex] = val;
                    
          x += xsx;
          y += ysx;
          z += zsx;

          if (shortcut != 0 && i >= izoom && i < ilimshort && 
              j >= izoom && j < jlimshort) {
            ishort = izoom - 1;
            if (j % izoom)
              ishort = ilimshort - izoom - 1;
            x += xsx * ishort;
            y += ysx * ishort;
            z += zsx * ishort;
            i += ishort;
          }
        }
      } else {

        /* Non HQ data */
        for (i = outerStart; i < innerStart; i++) {
          xi = (int)x;
          yi = (int)y;
          zi = (int)(z + 0.5);
                    
          if (xi >= 0 && xi < xsize && yi >= 0 && yi < ysize &&
              z > -0.5 && zi < zsize)
            val = (*ivwFastGetValue)(xi, yi, zi);
          else
            val = noDataVal;
                    
          if (k)
            cidata[i + cindex] += val;
          else
            cidata[i + cindex] = val;
                    
          x += xsx;
          y += ysx;
          z += zsx;
        }

        if (k) {
          for (i = innerStart; i < innerEnd; i++) {
            xi = (int)x;
            yi = (int)y;
            zi = (int)(z + 0.5);
            val = (*ivwFastGetValue)(xi, yi, zi);
            cidata[i + cindex] += val;
            x += xsx;
            y += ysx;
            z += zsx;
          }
        } else {
          for (i = innerStart; i < innerEnd; i++) {
            xi = (int)x;
            yi = (int)y;
            zi = (int)(z + 0.5);
            /*if (xi >= 0 && xi < xsize && yi >= 0 && yi < ysize &&
              zi >= 0 && zi < zsize) */
            val = (*ivwFastGetValue)(xi, yi, zi);
            /*else {
              imodPrintStderr("BAD %d %d %d %d %d %f %f %d %d\n", i, j, xi, yi,
                              zi, fstart, fend, innerStart, innerEnd);
              fstart = 0;
              fend = isize;
              findIndexLimits(isize, xsize, xo, xsx, 0., &fstart, &fend);
              imodPrintStderr("X %f %f\n", fstart, fend); 
              findIndexLimits(isize, ysize, yo, ysx, 0., &fstart, &fend,1);
              imodPrintStderr("Y %f %f\n", fstart, fend); 
              findIndexLimits(isize, zsize, zo, zsx, 0.5, &fstart, &fend);
              imodPrintStderr("Z %f %f\n", fstart, fend); 
              } */
            cidata[i + cindex] = val;
            x += xsx;
            y += ysx;
            z += zsx;
          }
        }

        for (i = innerEnd; i < outerEnd; i++) {
          xi = (int)x;
          yi = (int)y;
          zi = (int)(z + 0.5);
                    
          if (xi >= 0 && xi < xsize && yi >= 0 && yi < ysize &&
              z > -0.5 && zi < zsize)
            val = (*ivwFastGetValue)(xi, yi, zi);
          else
            val = noDataVal;
                    
          if (k)
            cidata[i + cindex] += val;
          else
            cidata[i + cindex] = val;
                    
          x += xsx;
          y += ysx;
          z += zsx;
        }

      }
      xo += xsy;
      yo += ysy;
      zo += zsy;

#ifdef QT_THREAD_SUPPORT
      if (j == jstart || j == jlimit - 1)
        fillMutex.unlock();
#endif
    }
    xzo += xsz;
    yzo += ysz;
    zzo += zsz;
  }
}

/*

$Log$
Revision 4.32  2009/02/26 22:39:35  mast
Fix keyboard tracking of image thickness

Revision 4.31  2009/01/24 01:04:49  mast
Turn of keyboard tracking on spin boxes

Revision 4.30  2009/01/16 18:27:03  mast
Commented out debug output on the angle bar break

Revision 4.29  2009/01/15 16:33:18  mast
Qt 4 port

Revision 4.28  2008/11/29 22:10:30  mast
Added ability to link slicers

Revision 4.27  2008/03/06 06:16:27  mast
Fixed artifact on right edge in zoomed HQ image

Revision 4.26  2008/01/28 19:11:31  mast
Fixed hot key in tooltip for show slice

Revision 4.25  2007/11/13 19:11:44  mast
Used the IMOD_PROCESSORS value to increase the max voxels to compute,
made it reduce quality before number of slices if slices are below 75%
of requested number, get voxel limit from settings

Revision 4.24  2007/11/10 17:25:45  mast
Do not sync angles when auto button is turned on

Revision 4.23  2007/08/15 19:50:18  mast
Added 1 pixel margin in computation of index limits to avoid crashes

Revision 4.22  2007/06/26 21:53:42  sueh
bug# 1021 Removed win_support.

Revision 4.21  2007/06/26 17:04:53  sueh
bug# 1021 Moved BM_HEIGHT and _WIDTH to win_support.

Revision 4.20  2007/06/15 21:19:54  mast
Added shift lock toolbar botton

Revision 4.19  2007/06/07 17:39:38  mast
Fixed use of wrong variable in testing for whether to take mean/SD

Revision 4.18  2007/06/04 15:06:34  mast
Added hot key to tooltip

Revision 4.17  2007/05/31 16:32:28  mast
Changes for slicer angle toolbar, classic setting and warning

Revision 4.16  2007/05/29 14:52:35  mast
Changes for new slicer mode and toolbar buttons

Revision 4.15  2007/05/25 05:28:16  mast
Changes for addition of slicer angle storage

Revision 4.14  2006/10/12 19:02:55  mast
Added toolbar button for W function

Revision 4.13  2006/10/06 19:38:08  mast
Made array filling routine multithreaded and moved it here

Revision 4.12  2006/09/12 15:36:09  mast
Added mouse move slot

Revision 4.11  2005/03/08 15:49:23  mast
Added FT/IM toggle button

Revision 4.10  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 4.9  2004/08/12 17:14:43  mast
Left out Z-scale after option when binnings differ

Revision 4.8  2004/01/22 19:12:43  mast
changed from pressed() to clicked() or accomodated change to actionClicked

Revision 4.7  2003/12/16 23:54:13  mast
Move floatspinbox to libdiaqt

Revision 4.6  2003/10/01 05:04:19  mast
change include from imodP to imod after eliminating imod.h from imodP.h

Revision 4.5  2003/09/15 21:04:02  mast
Allow zooms to 4 decimal places

Revision 4.4  2003/04/11 21:47:28  mast
adding tooltips

Revision 4.3  2003/03/26 17:15:30  mast
Adjust sizes for font changes

Revision 4.2  2003/02/28 21:39:32  mast
Changing name of tooledit focus signal

Revision 4.1  2003/02/10 20:29:02  mast
autox.cpp

Revision 1.1.2.4  2003/01/30 00:52:36  mast
new timer logic for getting clean first image

Revision 1.1.2.3  2003/01/29 01:45:29  mast
Make cube be a rgb widget regardless

Revision 1.1.2.2  2003/01/06 18:59:43  mast
fixing problems with float spin box

Revision 1.1.2.1  2003/01/06 15:48:30  mast
initial creation

*/
