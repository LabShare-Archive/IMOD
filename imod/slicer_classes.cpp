/*  IMOD VERSION 2.7.9
 *
 *  slicer_classes.cpp -- implements slicer mainwindow and QGLWidget classes.
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

$Date$

$Revision$

Log at end of file
*/
#include <stdlib.h>
#include <stdio.h>
#include <qtoolbutton.h>
#include <qlabel.h>
#include <qbitmap.h>
#include <qtoolbar.h>
#include <qtooltip.h>
#include <qsignalmapper.h>
#include <qpushbutton.h>
#include <qlayout.h>
#include <qvbox.h>
#include <qcombobox.h>
#include <qspinbox.h>
#include <qframe.h>
#include <qslider.h>
#include <qvalidator.h>

#include "imod.h"
#include "slicer_classes.h"
#include "sslice.h"
#include "tooledit.h"
#include "floatspinbox.h"
#include "arrowbutton.h"
#include "multislider.h"
#include "dia_qtutils.h"
#include "preferences.h"

#define AUTO_RAISE true

#define BM_WIDTH 16
#define BM_HEIGHT 16

#include "unlock.bits"
#include "lock.bits"
#include "lowres.bits"
#include "highres.bits"

static unsigned char showslice_bits[] = {
     0xff, 0x0f, 0xff, 0x0f, 0xff, 0x0f, 0x00, 0x00, 0xff, 0xef, 0xff, 0xef,
     0xff, 0xe7, 0xff, 0xe9, 0xff, 0xee, 0x7f, 0xef, 0x9f, 0xef, 0xef, 0xef,
     0xf7, 0xef, 0xf9, 0xef, 0xfe, 0xef, 0xff, 0xef};


static unsigned char *bitList[MAX_SLICER_TOGGLES][2] =
  { {lowres_bits, highres_bits},
    {unlock_bits, lock_bits}};

static QBitmap *bitmaps[MAX_SLICER_TOGGLES][2];
static QBitmap *showBitmap;
static int firstTime = 1;

static char *sliderLabels[] = {"X rotation", "Y rotation", "Z rotation"};

SlicerWindow::SlicerWindow(SlicerStruct *slicer, float maxAngles[],
			   bool rgba, bool doubleBuffer, bool enableDepth,
			   QWidget * parent, const char * name, WFlags f) 
  : QMainWindow(parent, name, f)
{
  int j;
  ArrowButton *arrow;
  QGLFormat glFormat;

  mSlicer = slicer;
  
  // Get the toolbar
  mToolBar = new HotToolBar(this, "zap toolbar");
  if (!AUTO_RAISE) {
    QBoxLayout *boxLayout = mToolBar->boxLayout();
    boxLayout->setSpacing(4);
  }
  connect(mToolBar, SIGNAL(keyPress(QKeyEvent *)), this,
	  SLOT(toolKeyPress(QKeyEvent *)));
  connect(mToolBar, SIGNAL(keyRelease(QKeyEvent *)), this,
	  SLOT(toolKeyRelease(QKeyEvent *)));

  // Zoom arrows
  arrow = new ArrowButton(Qt::UpArrow, mToolBar, "zoomup button");
  arrow->setAutoRaise(AUTO_RAISE);
  connect(arrow, SIGNAL(clicked()), this, SLOT(zoomUp()));
  QToolTip::add(arrow, "Increase zoom factor");
  arrow = new ArrowButton(Qt::DownArrow, mToolBar, "zoom down button");
  arrow->setAutoRaise(AUTO_RAISE);
  connect(arrow, SIGNAL(clicked()), this, SLOT(zoomDown()));
  QToolTip::add(arrow, "Decrease zoom factor");
  
  // Zoom edit box
  mZoomEdit = new ToolEdit(mToolBar, 6, "zoom edit box");
  mZoomEdit->setFocusPolicy(QWidget::ClickFocus);
  mZoomEdit->setAlignment(Qt::AlignRight);
  connect(mZoomEdit, SIGNAL(returnPressed()), this, SLOT(newZoom()));
  connect(mZoomEdit, SIGNAL(focusLost()), this, SLOT(newZoom()));
  QToolTip::add(mZoomEdit, "Enter an arbitrary zoom factor");
  
  // Make the 2 toggle buttons and their signal mapper
  QSignalMapper *toggleMapper = new QSignalMapper(mToolBar);
  connect(toggleMapper, SIGNAL(mapped(int)), this, SLOT(toggleClicked(int)));
  for (j = 0; j < 2; j++)
    setupToggleButton(mToolBar, toggleMapper, j);
  
  // The showslice button is simpler
  if (firstTime)
    showBitmap = new QBitmap(BM_WIDTH, BM_HEIGHT, showslice_bits, true);
  
  QToolButton *button = new QToolButton(mToolBar, "show slice");
  button->setPixmap(*showBitmap);
  button->setAutoRaise(AUTO_RAISE);
  connect(button, SIGNAL(clicked()), this, SLOT(showslicePressed()));
  
  // The Z scale combo box
  mZscaleCombo = new QComboBox(mToolBar, "zscale combo");
  mZscaleCombo->insertItem("Z-Scale Off", SLICE_ZSCALE_OFF);
  mZscaleCombo->insertItem("Z-Scale Before", SLICE_ZSCALE_BEFORE);

  // Only allow scale after if there is no implicit scale from binning
  if (slicer->vi->xybin == slicer->vi->zbin)
    mZscaleCombo->insertItem("Z-Scale After", SLICE_ZSCALE_AFTER);
  mZscaleCombo->setFocusPolicy(NoFocus);
  connect(mZscaleCombo, SIGNAL(activated(int)), this, 
	  SLOT(zScaleSelected(int)));
  QToolTip::add(mZscaleCombo, "Select whether to ignore Z scale, or apply it"
                " before or after rotation");

  // Help button
  mHelpButton = new QPushButton("Help", mToolBar, "Help button");
  mHelpButton->setFocusPolicy(QWidget::NoFocus);
  connect(mHelpButton, SIGNAL(clicked()), this, SLOT(help()));
  QToolTip::add(mHelpButton, "Open help window");
  setFontDependentWidths();

  // SECOND TOOLBAR
  mToolBar2 = new HotToolBar(this);
  mToolBar2->boxLayout()->setSpacing(4);
  connect(mToolBar2, SIGNAL(keyPress(QKeyEvent *)), this,
	  SLOT(toolKeyPress(QKeyEvent *)));
  connect(mToolBar2, SIGNAL(keyRelease(QKeyEvent *)), this,
	  SLOT(toolKeyRelease(QKeyEvent *)));


  // Make a frame, put a layout in it, and then put multisliders in the layout
  QFrame *sliderFrame = new QFrame(mToolBar2);
  QVBoxLayout *sliderLayout = new QVBoxLayout(sliderFrame);
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
  QFrame *cubeFrame = new QFrame(mToolBar2);
  cubeFrame->setFixedWidth(100);
  cubeFrame->setFrameShadow(QFrame::Sunken);
  cubeFrame->setFrameShape(QFrame::StyledPanel);
  QVBoxLayout *cubeLayout = new QVBoxLayout(cubeFrame);
  cubeLayout->setMargin(3);
  mCube = new SlicerCube(slicer, glFormat, cubeFrame);
  cubeLayout->addWidget(mCube);

  // Thickness label
  QVBox *thickBox = new QVBox(mToolBar2);
  QLabel *label = new QLabel("Thickness", thickBox);
  QSize labelSize = label->sizeHint();

  // Thickness of image spin box
  label = new QLabel("Image", thickBox);
  mImageBox = new QSpinBox(1, 1000, 1, thickBox);
  mImageBox->setFocusPolicy(QWidget::ClickFocus);
  mImageBox->setMaximumWidth(labelSize.width()- 4);
  connect(mImageBox, SIGNAL(valueChanged(int)), this, 
	  SLOT(imageThicknessChanged(int)));
  QToolTip::add(mImageBox, "Set number of slices to average");

  // Thickness of model spin box
  label = new QLabel("Model", thickBox);
  mModelBox = new FloatSpinBox(1, 1, 10000, 10, thickBox);
  mModelBox->setFocusPolicy(QWidget::ClickFocus);
  mModelBox->setMaximumWidth(labelSize.width()- 4);
  connect(mModelBox, SIGNAL(valueChanged(int)), this, 
	  SLOT(modelThicknessChanged(int)));
  QToolTip::add(mModelBox, "Set thickness of model to project onto image");

  firstTime = 0;

  // Need GLwidget next - this gets the defined format
  glFormat.setRgba(rgba);
  glFormat.setDoubleBuffer(doubleBuffer);
  glFormat.setDepth(enableDepth);
  mGLw = new SlicerGL(slicer, glFormat, this);
  
  // Set it as main widget, set focus, dock on top and bottom only
  setCentralWidget(mGLw);
  setFocusPolicy(QWidget::StrongFocus);
  setDockEnabled(mToolBar, Left, FALSE );
  setDockEnabled(mToolBar, Right, FALSE );
  setDockEnabled(mToolBar2, Left, FALSE );
  setDockEnabled(mToolBar2, Right, FALSE );

  // This makes the toolbar give a proper size hint before showing window
  setUpLayout();
}

void SlicerWindow::setFontDependentWidths()
{
  diaSetButtonWidth(mHelpButton, ImodPrefs->getRoundedStyle(), 1.2, "Help");
}

static char *toggleTips[] = {
  "Toggle between regular and high-resolution (interpolated) image",
  "Lock window at current position",
  "Show slice cutting lines in Xyz and Zap windows"};

// Make the two bitmaps, add the toggle button to the tool bar, and add
// it to the signal mapper
void SlicerWindow::setupToggleButton(QToolBar *toolBar, QSignalMapper *mapper, 
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

void SlicerWindow::zoomUp()
{
  slicerStepZoom(mSlicer, 1);
}

void SlicerWindow::zoomDown()
{
  slicerStepZoom(mSlicer, -1);
}

// A new zoom or section was entered - let slicer decide on limits and 
// refresh box
void SlicerWindow::newZoom()
{
  QString str = mZoomEdit->text();
  slicerEnteredZoom(mSlicer, atof(str.latin1()));
  setFocus();
}

// Respomd to spin box changes for image and model thickness
void SlicerWindow::imageThicknessChanged(int depth)
{
  slicerImageThickness(mSlicer, depth);
  setFocus();
}

void SlicerWindow::modelThicknessChanged(int depth)
{
  slicerModelThickness(mSlicer, (float)(depth / 10.));
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
  int state = 1 - mToggleStates[index];
  mToggleStates[index] = state; 
  mToggleButs[index]->setPixmap(*bitmaps[index][state]);
  slicerStateToggled(mSlicer, index, state);
}

void SlicerWindow::showslicePressed()
{
  slicerShowSlice(mSlicer);
}

void SlicerWindow::zScaleSelected(int item)
{
  slicerZscale(mSlicer, item);
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
  // Downcast seems not to be needed, but play it safe
  diaSetSpinBox((QSpinBox *)mModelBox, (int)(10. * depth + 0.5));
}

void SlicerWindow::setImageThickness(int depth)
{
  diaSetSpinBox(mImageBox, depth);
}

// This allows slicer to set one of the buttons
void SlicerWindow::setToggleState(int index, int state)
{
  mToggleStates[index] = state ? 1 : 0;
  mToggleButs[index]->setPixmap(*bitmaps[index][state]);
}

void SlicerWindow::setZoomText(float zoom)
{
  QString str;
  str.sprintf("%.4f", zoom);
  if (str.endsWith("00"))
    str.truncate(str.length() - 2);
  mZoomEdit->setText(str);
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

SlicerGL::SlicerGL(SlicerStruct *slicer, QGLFormat inFormat, QWidget * parent,
             const char * name)
  : QGLWidget(inFormat, parent, name)
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

void SlicerGL::mouseReleaseEvent ( QMouseEvent * e )
{
  mMousePressed = false;
}

///////////////////////////////////////////////
// The cube class

SlicerCube::SlicerCube(SlicerStruct *slicer, QGLFormat inFormat, 
		       QWidget * parent, const char * name)
  : QGLWidget(inFormat, parent, name)
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

 /*
$Log$
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
