/*
 *  form_moviecon.cpp - Class for image movie controller dialog form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "form_moviecon.h"

#include <qvariant.h>
#include <qslider.h>
#include <qimage.h>
#include <qpixmap.h>

//Added by qt3to4:
#include <QCloseEvent>
#include <QKeyEvent>
#include <QVBoxLayout>
#include <qtooltip.h>

#include "imod.h"
#include "control.h"
#include "multislider.h"
#include "moviecon.h"
#include "dia_qtutils.h"
#include "preferences.h"

/*
 *  Constructs a MovieController as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
MovieController::MovieController(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
MovieController::~MovieController()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void MovieController::languageChange()
{
  retranslateUi(this);
}

// Initialize: make the sliders, set radio buttons
void MovieController::init()
{
  const char *labels[] = {"Start", "End", "Increment"};
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
    
  QVBoxLayout *layout = new QVBoxLayout(sliderFrame);
  layout->setContentsMargins(0,0,0,0);
  mSliders = new MultiSlider(sliderFrame, 3, labels, 1, 20);
  layout->addLayout(mSliders->getLayout());

  snapshotGroup = new QButtonGroup(this);
  snapshotGroup->addButton(noneRadioButton, 0);
  snapshotGroup->addButton(tiffRadioButton, 1);
  snapshotGroup->addButton(rgbRadioButton, 2);
  snapshotGroup->addButton(pngRadioButton, 3);
  connect(snapshotGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(snapshotSelected(int)));

  axisGroup = new QButtonGroup(this);
  axisGroup->addButton(xRadioButton, 0);
  axisGroup->addButton(yRadioButton, 1);
  axisGroup->addButton(zRadioButton, 2);
  axisGroup->addButton(timeRadioButton, 3);
  connect(axisGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(axisSelected(int)));

  extentGroup = new QButtonGroup(this);
  extentGroup->addButton(roundTripButton, 0);
  extentGroup->addButton(oneWayTripButton, 1);
  connect(extentGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(extentSelected(int)));

  startHereGroup = new QButtonGroup(this);
  startHereGroup->addButton(endRadioButton, 0);
  startHereGroup->addButton(hereRadioButton, 1);
  connect(startHereGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(startHereSelected(int)));

  diaSetGroup(snapshotGroup, 0);
  diaSetGroup(extentGroup, 0);
  diaSetGroup(startHereGroup, 0);
  diaSetGroup(axisGroup, 2);
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this,
          SLOT(sliderChanged(int, int, bool)));
  mSliders->getSlider(0)->setToolTip("Set starting section of movie range");
  mSliders->getSlider(1)->setToolTip("Set ending section of movie range");
  mSliders->getSlider(2)->setToolTip
    ("Set spacing between sections shown during movie");
  setNonTifLabel();
  bool mont = imcGetSnapMontage(false);
  montageSpinBox->setEnabled(mont && !imcGetSnapWholeMont());
  wholeCheckBox->setEnabled(mont);
  scaleSpinBox->setEnabled(mont && imcGetScaleSizes());
  scaleCheckBox->setEnabled(mont);
  diaSetChecked(montageCheckBox, mont);
  diaSetSpinBox(montageSpinBox, imcGetMontageFactor());
  diaSetChecked(wholeCheckBox, imcGetSnapWholeMont());
  diaSetChecked(scaleCheckBox, imcGetScaleSizes());
  diaSetSpinBox(scaleSpinBox, imcGetSizeScaling());

  // Connections above are in the .ui file, have to add new ones by hand
  connect(slicerMontCheckBox, SIGNAL(toggled(bool)), this, 
          SLOT(slicerMontToggled(bool)));
  connect(slicerScaleCheckBox, SIGNAL(toggled(bool)), this, 
          SLOT(scaleSlicerThickToggled(bool)));
  connect(slicerMontSpinBox, SIGNAL(valueChanged(int)), this, 
          SLOT(newSlicerMontValue(int)));
  connect(slicerScaleSpinBox, SIGNAL(valueChanged(int)), this, 
          SLOT(scaleThickChanged(int)));
  mont = imcGetSlicerMontage(false);
  slicerMontSpinBox->setEnabled(mont);
  slicerScaleSpinBox->setEnabled(mont && imcGetScaleThicks());
  slicerScaleCheckBox->setEnabled(mont);
  diaSetChecked(slicerMontCheckBox, mont);
  diaSetSpinBox(slicerMontSpinBox, imcGetSlicerMontFactor());
  diaSetChecked(slicerScaleCheckBox, imcGetScaleThicks());
  diaSetSpinBox(slicerScaleSpinBox, imcGetThickScaling());
  setFontDependentWidths();
}

void MovieController::setFontDependentWidths()
{
  rateLineEdit->setMaximumWidth(fontMetrics().width("8888.888"));
}

// Set the labels of the non-tiff buttons
void MovieController::setNonTifLabel()
{
  QString str;
  rgbRadioButton->setText(ImodPrefs->snapFormat());
  str = ImodPrefs->snapFormat2();
  pngRadioButton->setEnabled(!str.isEmpty());
    
  // If no second format, make sure selection is in legal range
  if (str.isEmpty()) {
    if (snapshotGroup->checkedId() > 2) {
      diaSetGroup(snapshotGroup, 2);
      imcSnapSelected(2);
    }
  } else
    pngRadioButton->setText(str);
}

// Pass on actions directly to imod_moviecon
void MovieController::axisSelected( int which )
{
  imcAxisSelected(which);
}

void MovieController::sliderChanged( int which, int value, bool dragging )
{
  imcSliderChanged(which, value);
}

void MovieController::upPressed()
{
  imcIncrementRate(-1);
}

void MovieController::downPressed()
{
  imcIncrementRate(1);
}

void MovieController::rateEntered()
{
  float value = rateLineEdit->text().toFloat();
  imcRateEntered(value);
}

void MovieController::snapshotSelected( int which )
{
  imcSnapSelected(which);
}

void MovieController::extentSelected( int which )
{
  imcExtentSelected(which);
}

void MovieController::startHereSelected( int which )
{
  imcStartHereSelected(which);
}

void MovieController::montageToggled(bool state )
{
  montageSpinBox->setEnabled(state && !imcGetSnapWholeMont());
  scaleSpinBox->setEnabled(state && imcGetScaleSizes());
  scaleCheckBox->setEnabled(state);
  wholeCheckBox->setEnabled(state);
  imcSetSnapMontage(state);
}

void MovieController::newMontageValue( int value )
{
  imcSetMontageFactor(value);
}

void MovieController::wholeImageToggled( bool state )
{
  imcSetSnapWholeMont(state);
  montageSpinBox->setEnabled(!state);
}

void MovieController::scaleThickToggled( bool state )
{
  imcSetScaleSizes(state);
  scaleSpinBox->setEnabled(state);
}

void MovieController::scalingChanged( int value )
{
  imcSetSizeScaling(value);
}

void MovieController::slicerMontToggled(bool state )
{
  slicerMontSpinBox->setEnabled(state);
  slicerScaleSpinBox->setEnabled(state && imcGetScaleThicks());
  slicerScaleCheckBox->setEnabled(state);
  imcSetSlicerMontage(state);
}

void MovieController::newSlicerMontValue( int value )
{
  imcSetSlicerMontFactor(value);
}

void MovieController::scaleSlicerThickToggled( bool state )
{
  imcSetScaleThicks(state);
  slicerScaleSpinBox->setEnabled(state);
}

void MovieController::scaleThickChanged( int value )
{
  imcSetThickScaling(value);
}

void MovieController::donePressed()
{
  close();
}

void MovieController::helpPressed()
{
  imcHelp();
}

void MovieController::resetPressed()
{
  imcResetPressed();
}

// Routines for setting the state of the controls
void MovieController::enableTime( int enabled )
{
  timeRadioButton->setEnabled(enabled != 0); 
}

void MovieController::setActualRate( QString str )
{
  actualLabel->setText(str);
}

void MovieController::setRateBox( float value )
{
  QString str;
  if (value < 10.0)
    str.sprintf("%5.2f", value);
  else
    str.sprintf("%5.1f", value);
  rateLineEdit->setText(str);
}

void MovieController::setSliders( int start, int maxStart, int end, int minEnd,
				  int maxEnd, int increment,  int enable )
{
  mSliders->setRange(0, 1, maxStart);
  mSliders->setValue(0, start);
  mSliders->setRange(1, minEnd, maxEnd);
  mSliders->setValue(1, end);
  mSliders->setValue(2, increment);
  for (int i = 0; i < 3; i++) 
    mSliders->getSlider(i)->setEnabled(enable != 0);
}

// Close and key events
void MovieController::closeEvent( QCloseEvent * e )
{
  imcClosing();
  e->accept();
}

void MovieController::keyPressEvent( QKeyEvent * e )
{
  if (utilCloseKey(e))
    close();
  else
    ivwControlKey(0, e);
}

void MovieController::keyReleaseEvent( QKeyEvent * e )
{
  ivwControlKey(1, e);
}

void MovieController::changeEvent(QEvent *e)
{
  QWidget::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    setFontDependentWidths();
}

