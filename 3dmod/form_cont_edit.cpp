/*
 *  formv_cont_edit.cpp - Class for point/contour/surface dialog form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "form_cont_edit.h"

#include <qvariant.h>
#include <math.h>
#include <qimage.h>
#include <qpixmap.h>

//Added by qt3to4:
#include <QCloseEvent>
#include <QKeyEvent>

#include "dia_qtutils.h"
#include "cont_edit.h"
#include "control.h"
#include "preferences.h"
#include "imod.h"

/*
 *  Constructs a ContSurfPoint as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
ContSurfPoint::ContSurfPoint(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
ContSurfPoint::~ContSurfPoint()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void ContSurfPoint::languageChange()
{
  retranslateUi(this);
}

// SET GROUP BOX LAYOUT MARGINS TO 7?

void ContSurfPoint::init()
{
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  mCtrlPressed = false;
  mSliderPressed = false;
  setFontDependentWidths();
  diaSetChecked(mouseSizeBox, iceGetWheelForSize() != 0);
  adjustSize();
}

void ContSurfPoint::setFontDependentWidths()
{
  pointSizeEdit->setMinimumWidth(fontMetrics().width("  888.8-Default "));
}

// Respond to actions by passing them on
void ContSurfPoint::surfaceChanged( int value )
{
  iceSurfGoto(value);
}

void ContSurfPoint::upContPressed()
{
  iceContInSurf(1);
}

void ContSurfPoint::downContPressed()
{
  iceContInSurf(-1);
}

void ContSurfPoint::newSurfPressed()
{
  iceSurfNew();
}

void ContSurfPoint::surfGhostToggled( bool state )
{
  iceGhostToggled(state ? 1 : 0,  IMOD_GHOST_SURFACE);
}

void ContSurfPoint::surfLabelChanged( const QString & str )
{
  iceLabelChanged(LATIN1(str), 2);
}

// Manage the open-closed radio buttons by calling the set function
void ContSurfPoint::closedClicked()
{
  setClosedOpen(0, 1);
  iceClosedOpen(0);
}

void ContSurfPoint::openClicked()
{
  setClosedOpen(1, 1);
  iceClosedOpen(1);
}

void ContSurfPoint::timeChanged( int value )
{
  iceTimeChanged(value);
}

void ContSurfPoint::contLabelChanged( const QString &str )
{
  iceLabelChanged(LATIN1(str), 0);
}

// Pass on the slider if clicked or hot slider enabled
void ContSurfPoint::pointSliderChanged( int value )
{
  displayPointSize(value / 10., 0);
  if (!mSliderPressed || ImodPrefs->hotSliderActive(mCtrlPressed))
    icePointSize(mSizeDisplayed);
}

// Show point size with one decimal, add -Default if it is
void ContSurfPoint::displayPointSize( float value , int defval)
{
  QString str;
  str.sprintf("%.1f", value);
  if (defval)
    str += "-Default";
  pointSizeEdit->setText(str);
  mSizeDisplayed = value;
}

void ContSurfPoint::pointSliderPressed()
{
  mSliderPressed = true;
}

void ContSurfPoint::pointSliderReleased()
{
  mSliderPressed = false;
  icePointSize(mSizeDisplayed);
}

// Limit an entered point size, call the set function to set up slider, and pass it on
void ContSurfPoint::pointSizeEntered()
{
  float value = pointSizeEdit->text().toFloat();
  if (value < 0.)
    value = 0.;
  if (value > MAX_POINT_SIZE)
    value = MAX_POINT_SIZE;
  setPointSize(value, 0);
  icePointSize(value);
}

void ContSurfPoint::mouseSizeToggled( bool state )
{
  iceSetWheelForSize(state ? 1 : 0);
}

void ContSurfPoint::pointLabelChanged( const QString &str )
{
  iceLabelChanged(LATIN1(str), 1);
}

void ContSurfPoint::ghostChanged( int value )
{
  iceGhostInterval(value);
}

// Pass on ghost button changes with the relevant flag
void ContSurfPoint::upGhostToggled( bool state )
{
  iceGhostToggled(state ? 1 : 0,  IMOD_GHOST_NEXTSEC);
}

void ContSurfPoint::downGhostToggled( bool state )
{
  iceGhostToggled(state ? 1 : 0, IMOD_GHOST_PREVSEC);
}

void ContSurfPoint::lighterGhostToggled( bool state )
{
  iceGhostToggled(state ? 1 : 0,  IMOD_GHOST_LIGHTER);
}

void ContSurfPoint::allObjGhostToggled( bool state )
{
  iceGhostToggled(state ? 1 : 0,  IMOD_GHOST_ALLOBJ);
}

void ContSurfPoint::helpPressed()
{
  iceShowHelp();
}

// Set the open/closed radio buttons, or disable if open < 0
void ContSurfPoint::setClosedOpen( int open, int enabled )
{
  closedRadioButton->setEnabled(enabled);
  openRadioButton->setEnabled(enabled);
  closedRadioButton->blockSignals(true);
  closedRadioButton->setChecked(open <= 0);
  closedRadioButton->blockSignals(false);
  openRadioButton->blockSignals(true);
  openRadioButton->setChecked(open > 0);
  openRadioButton->blockSignals(false);
}

// Set the state of all ghost-related controls and save flags for passing with actions
void ContSurfPoint::setGhostState( int interval, int ghostmode)
{
  diaSetSpinBox(ghostSpinBox, interval);
  diaSetChecked(downGhostBox, ghostmode & IMOD_GHOST_PREVSEC);
  diaSetChecked(upGhostBox, ghostmode & IMOD_GHOST_NEXTSEC);
  diaSetChecked(surfGhostBox, ghostmode & IMOD_GHOST_SURFACE);
  diaSetChecked(lighterGhostBox, ghostmode & IMOD_GHOST_LIGHTER);
  diaSetChecked(allObjGhostBox, ghostmode & IMOD_GHOST_ALLOBJ);
}

// Set the point size slider, increasing the range if necessary, and show it in edit box
void ContSurfPoint::setPointSize( float size, int defval )
{
  pointSizeEdit->setEnabled(defval >= 0);
  pointSizeSlider->setEnabled(defval >= 0);
  if (defval < 0) {
    diaSetSlider(pointSizeSlider, 0);
    pointSizeEdit->setText("No Point");
  } else {
    int value = (int)floor(10. * size + 0.5);
    if (value > pointSizeSlider->maximum())
      pointSizeSlider->setMaximum(value);
    diaSetSlider(pointSizeSlider, value);
    displayPointSize(size, defval);
  }
}

// Set the surface number and maximum value
void ContSurfPoint::setSurface( int value, int maxVal )
{
  QString str;
  surfaceSpinBox->setEnabled(value >= 0 && maxVal > 0); 
  if (value < 0) {
    surfaceSpinBox->setSpecialValueText("--x--");
    diaSetSpinBox(surfaceSpinBox, 0);
  } else {
    surfaceSpinBox->setSpecialValueText("");
    diaSetSpinMMVal(surfaceSpinBox, 0, maxVal, value);
  }
  str.sprintf("/ %d", maxVal);
  surfaceLabel->setText(str);
}

// Set the time index value and maximum, or just disable and set to no time
void ContSurfPoint::setTimeIndex( int value, int maxVal )
{
  timeSpinBox->setEnabled(value >= 0 && maxVal > 0);
  if (!maxVal && value > 0)
    maxVal = value;
  if (value < 0) {
    timeSpinBox->setSpecialValueText(value < -1 ? "No Time" : "No Cont");
    timeSpinBox->setValue(0);
  } else {
    timeSpinBox->setSpecialValueText("");
    diaSetSpinMMVal(timeSpinBox, 0, maxVal, value);
  }
}

// Set the labels, or set to no Contour/Point
void ContSurfPoint::setLabels(QString surfLabel, int noSurf,  QString contLabel, int noCont, 
                              QString ptLabel, int noPoint )
{
  // We have to block signals for continuously updating text fields, apparently
  surfaceLabelEdit->setEnabled(!noSurf);
  if (noSurf ) 
    diaSetEditText(surfaceLabelEdit, "No Surface");
  else
    diaSetEditText(surfaceLabelEdit, surfLabel);
    
  contourEdit->setEnabled(!noCont);
  if (noCont) 
    diaSetEditText(contourEdit, "No Contour");
  else
    diaSetEditText(contourEdit, contLabel);
    
  pointLabelEdit->setEnabled(!noPoint);
  if (noPoint) 
    diaSetEditText(pointLabelEdit, "No Point");
  else
    diaSetEditText(pointLabelEdit, ptLabel);
}

// Inform of closing
void ContSurfPoint::closeEvent( QCloseEvent * e )
{
  iceClosing();
  e->accept();
}

// close on Escape, keep track of ctrl, pass on other key events
void ContSurfPoint::keyPressEvent( QKeyEvent * e )
{
  if (utilCloseKey(e))
    close();
  else {
    if (hotSliderFlag() != NO_HOT_SLIDER && e->key() == hotSliderKey()) {
      mCtrlPressed = true;
      grabKeyboard();
    }
    ivwControlKey(0, e);
  }
}

void ContSurfPoint::keyReleaseEvent( QKeyEvent * e )
{
  if (e->key() == hotSliderKey()) {
    mCtrlPressed = false;
    releaseKeyboard();
  }
  ivwControlKey(1, e);
}

void ContSurfPoint::changeEvent(QEvent *e)
{
  QWidget::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    setFontDependentWidths();
}

