/*
 *  formv_controls.cpp - Class for model view controls dialog form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "formv_control.h"

#include <qvariant.h>
#include <stdlib.h>
#include <qimage.h>
#include <qpixmap.h>

//Added by qt3to4:
#include <QCloseEvent>
#include <QKeyEvent>

#include "dia_qtutils.h"
#include "mv_input.h"
#include "mv_control.h"
#include "preferences.h"
#include "imod.h"
#include "imodv.h"

/*
 *  Constructs a imodvControlForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
imodvControlForm::imodvControlForm(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
imodvControlForm::~imodvControlForm()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void imodvControlForm::languageChange()
{
  retranslateUi(this);
}

void imodvControlForm::init()
{
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  mNearPressed = false;
  mFarPressed = false;
  mPerspectivePressed = false;
  mZscalePressed = false;
  mRatePressed = false;
  mCtrlPressed = false;
  if (Imodv->standalone)
    linkSlicerCheckBox->setEnabled(false);
  else
    diaSetChecked(linkSlicerCheckBox, Imodv->linkToSlicer != 0);
  diaSetChecked(linkCentersCheckBox, Imodv->linkSlicerCenter != 0);
  linkCentersCheckBox->setEnabled(!Imodv->standalone && Imodv->linkToSlicer);
  setFontDependentWidths();
}

void imodvControlForm::setFontDependentWidths()
{
  int width;
  width =( (2 * 6 + 3) * scaleLineEdit->fontMetrics().width("888888")) / (2 * 6);
  scaleLineEdit->setFixedWidth(width);
  speedLineEdit->setFixedWidth(width);
  width =( (2 * 7+ 3) * XLineEdit->fontMetrics().width("8888888")) / (2 * 7);
  XLineEdit->setFixedWidth(width);
  YLineEdit->setFixedWidth(width);
  ZLineEdit->setFixedWidth(width);
}


// Routines to display slider labels, and record the value displayed
void imodvControlForm::displayFarLabel( int value )
{
  QString str;
  str.sprintf("%d", value);
  farTextLabel->setText(str);
  mFarDisplayed = value;
}

void imodvControlForm::displayNearLabel( int value )
{
  QString str;
  str.sprintf("%d", value);
  nearTextLabel->setText(str);
  mNearDisplayed = value;
}

void imodvControlForm::displayPerspectiveLabel( int value )
{
  QString str;
  str.sprintf("%d", value);
  perspectiveTextLabel->setText(str);
  mPerspectiveDisplayed = value;
}

void imodvControlForm::displayRateLabel( int value )
{
  QString str;
  str.sprintf("%.1f", value / 10.);
  degreesTextLabel->setText(str);
  mRateDisplayed = value;
}

void imodvControlForm::displayZscaleLabel( int value )
{
  QString str;
  if (value < 200)
    str.sprintf("%.2f", value / 100.);
  else
    str.sprintf("%.1f", value / 100.);
  zScaleTextLabel->setText(str);
  mZscaleDisplayed = value;
}

// ZOOM UP AND DOWN CONTROL BUTTONS
void imodvControlForm::zoomDown()
{
  imodvControlZoom(-1);
}

void imodvControlForm::zoomUp()
{
  imodvControlZoom(1);
}

// A new value is entered in the scale box
void imodvControlForm::newScale()
{
  QString str = scaleLineEdit->text();
  float value = atof(LATIN1(str));
  if  (value < 0.001)
    value = 0.001;
  setScaleText(value);
  setFocus();
  imodvControlScale(value);
}

void imodvControlForm::kickBoxToggled( bool state )
{
  imodvControlKickClips(state);
}

// Changes in the slider positions
void imodvControlForm::nearChanged( int value )
{
  if (!mNearPressed || ImodPrefs->hotSliderActive(mCtrlPressed))
    imodvControlClip(IMODV_CONTROL_NEAR, value, mNearPressed);
}

void imodvControlForm::farChanged( int value )
{
  if (!mFarPressed || ImodPrefs->hotSliderActive(mCtrlPressed))
    imodvControlClip(IMODV_CONTROL_FAR, value, mFarPressed);
}

void imodvControlForm::perspectiveChanged( int value )
{
  if (!mPerspectivePressed || ImodPrefs->hotSliderActive(mCtrlPressed))
    imodvControlClip(IMODV_CONTROL_FOVY, value, mPerspectivePressed);
}

void imodvControlForm::zScaleChanged( int value )
{
  if (!mZscalePressed || ImodPrefs->hotSliderActive(mCtrlPressed))
    imodvControlZscale(value, mZscalePressed);
}

// The rotation buttons
void imodvControlForm::rotateXminus()
{
  imodvControlAxisButton(-IMODV_CONTROL_XAXIS);
}

void imodvControlForm::rotateXplus()
{
  imodvControlAxisButton(IMODV_CONTROL_XAXIS);
}

void imodvControlForm::rotateYminus()
{
  imodvControlAxisButton(-IMODV_CONTROL_YAXIS);
}

void imodvControlForm::rotateYplus()
{
  imodvControlAxisButton(IMODV_CONTROL_YAXIS);
}

void imodvControlForm::rotateZminus()
{
  imodvControlAxisButton(-IMODV_CONTROL_ZAXIS);
}

void imodvControlForm::rotateZplus()
{
  imodvControlAxisButton(IMODV_CONTROL_ZAXIS);
}

// New values entered in the rotation text boxes
void imodvControlForm::newXrotation()
{
  QString str = XLineEdit->text();
  float value = atof(LATIN1(str));
  YLineEdit->setFocus();
  imodvControlAxisText(IMODV_CONTROL_XAXIS, value);
}

void imodvControlForm::newYrotation()
{
  QString str = YLineEdit->text();
  float value = atof(LATIN1(str));
  ZLineEdit->setFocus();
  imodvControlAxisText(IMODV_CONTROL_YAXIS, value);
}

void imodvControlForm::newZrotation()
{
  QString str = ZLineEdit->text();
  float value = atof(LATIN1(str));
  setFocus();
  imodvControlAxisText(IMODV_CONTROL_ZAXIS, value);
}

// Start/top button
void imodvControlForm::startStop()
{
  imodvControlStart();
}

// Link to slicer box
void imodvControlForm::linkBoxToggled(bool state)
{
  Imodv->linkToSlicer = state ? 1 : 0;
  linkCentersCheckBox->setEnabled(state);
}

// Link centers of rotation
void imodvControlForm::linkCenterToggled( bool state )
{
  Imodv->linkSlicerCenter = state ? 1 : 0;
}

// Rate slider
void imodvControlForm::rateChanged( int value )
{
  if (!mRatePressed || ImodPrefs->hotSliderActive(mCtrlPressed))
    imodvControlRate(value);
}

// Speed controls
void imodvControlForm::newSpeed()
{
  QString str = speedLineEdit->text();
  float value = atof(LATIN1(str));
  if  (value < 0.1)
    value = 0.1;
  setSpeedText(value);
  setFocus();
  imodvControlSpeed(value);
}

void imodvControlForm::increaseSpeed()
{
  imodvControlIncSpeed(1);
}

void imodvControlForm::decreaseSpeed()
{
  imodvControlIncSpeed(-1);
}

void imodvControlForm::OKPressed()
{
  imodvControlQuit();
}

void imodvControlForm::helpPressed()
{
  imodvControlHelp();    
}

// Routines for imodv_control to set state of dialog
void imodvControlForm::setAxisText( int axis, float value )
{
  QString str;
  str.sprintf("%6.2f", value);
  if (axis == IMODV_CONTROL_XAXIS)
    diaSetEditText(XLineEdit, str);
  else if (axis == IMODV_CONTROL_YAXIS)
    diaSetEditText(YLineEdit, str);
  else 
    diaSetEditText(ZLineEdit, str);
}

void imodvControlForm::setScaleText( float value )
{
  QString str;
  str.sprintf("%.4g", value);
  scaleLineEdit->setText(str);
}

void imodvControlForm::setKickBox( bool state )
{
  diaSetChecked(kickCheckBox, state);
}

void imodvControlForm::setViewSlider( int which, int value )
{
  switch (which) {
  case IMODV_CONTROL_NEAR:
    diaSetSlider(nearSlider, value);
    displayNearLabel(value);
    break;
  case IMODV_CONTROL_FAR:
    diaSetSlider(farSlider, value);
    displayFarLabel(value);
    break;
  case IMODV_CONTROL_FOVY:
    diaSetSlider(perspectiveSlider, value);
    displayPerspectiveLabel(value);
    break;
  case IMODV_CONTROL_ZSCALE:
    diaSetSlider(zScaleSlider, value);
    displayZscaleLabel(value);
    break;
  }
}

void imodvControlForm::setRotationRate( int value )
{
  diaSetSlider(degreesSlider, value);
  displayRateLabel(value);
}

void imodvControlForm::setSpeedText( float value )
{
  QString str;
  str.sprintf("%.4g", value);
  speedLineEdit->setText(str);
}

void imodvControlForm::closeEvent( QCloseEvent *e )
{
  imodvControlClosing();
  e->accept();
}

// To keep the sliders from being continuously active while dragged, record when they are
// pressed and released
void imodvControlForm::farPressed()
{
  mFarPressed = true;
}

void imodvControlForm::farReleased()
{
  mFarPressed = false;
  farChanged(mFarDisplayed);
}

void imodvControlForm::nearPressed()
{
  mNearPressed = true;
}

void imodvControlForm::nearReleased()
{
  mNearPressed = false;
  nearChanged(mNearDisplayed);
}

void imodvControlForm::zScaleReleased()
{
  mZscalePressed = false;
  zScaleChanged(mZscaleDisplayed);
}

void imodvControlForm::perspectivePressed()
{
  mPerspectivePressed = true;
}

void imodvControlForm::perspectiveReleased()
{
  mPerspectivePressed = false;
  perspectiveChanged(mPerspectiveDisplayed);
}

void imodvControlForm::zScalePressed()
{
  mZscalePressed = true;
}

void imodvControlForm::ratePressed()
{
  mRatePressed = true;
}

void imodvControlForm::rateReleased()
{
  mRatePressed = false;
  rateChanged(mRateDisplayed);
}

// Key event: send quit signal if an escape, keep track of control key, and pass on to imodv_input
void imodvControlForm::keyPressEvent( QKeyEvent * e )
{
  // fprintf(stderr, "keyEvent\n");
  if (utilCloseKey(e)) {
    imodvControlQuit();
  } else {
    
    if (hotSliderFlag() != NO_HOT_SLIDER && e->key() == hotSliderKey()) {
      mCtrlPressed = true;
      grabKeyboard();
    }
    imodvKeyPress(e);
  }
}

void imodvControlForm::keyReleaseEvent( QKeyEvent * e )
{
  if (e->key() == hotSliderKey()) {
    mCtrlPressed = false;
    releaseKeyboard();
  }
  imodvKeyRelease(e);
}

void imodvControlForm::changeEvent(QEvent *e)
{
  QWidget::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    setFontDependentWidths();
}

