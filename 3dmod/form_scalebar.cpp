/*
 *  form_scalebar.cpp - Class for scale bar dialog form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "form_scalebar.h"

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>

//Added by qt3to4:
#include <QTimerEvent>
#include <QKeyEvent>
#include <QCloseEvent>

#include "scalebar.h"
#include "imodv.h"
#include "mv_input.h"
#include "dia_qtutils.h"
#include "control.h"
#include "imod.h"

/*
 *  Constructs a ScaleBarForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
ScaleBarForm::ScaleBarForm(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
ScaleBarForm::~ScaleBarForm()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void ScaleBarForm::languageChange()
{
  retranslateUi(this);
}

void ScaleBarForm::init()
{
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  mTimerID = 0;
  mParams = scaleBarGetParams();
  diaSetChecked(drawCheckBox, mParams->draw);
  diaSetChecked(whiteCheckBox, mParams->white); 
  diaSetChecked(verticalCheckBox, mParams->vertical); 
  diaSetChecked(colorCheckBox, mParams->colorRamp);
  diaSetChecked(invertCheckBox, mParams->invertRamp);
  diaSetSpinBox(lengthSpinBox, mParams->minLength);
  diaSetSpinBox(thicknessSpinBox, mParams->thickness);
  diaSetSpinBox(indentXspinBox, mParams->indentX);
  diaSetSpinBox(indentYspinBox, mParams->indentY);
  positionComboBox->setCurrentIndex(mParams->position);
  diaSetChecked(customCheckBox, mParams->useCustom);
  diaSetSpinBox(customSpinBox, mParams->customVal);
  if (imodvStandalone()) {
    zapLabel->hide();
    zapValue->hide();
    slicerLabel->hide();
    slicerValue->hide();
    multiZLabel->hide();
    multiZValue->hide();
    xyzLabel->hide();
    xyzValue->hide();
  }
  customSpinBox->setEnabled(mParams->useCustom);
}

// Respond to the changes: put value in structure and redraw
void ScaleBarForm::drawToggled( bool state )
{
  mParams->draw = state;
  scaleBarRedraw();
}

void ScaleBarForm::whiteToggled( bool state )
{
  mParams->white = state;
  scaleBarRedraw();
}

void ScaleBarForm::verticalToggled( bool state )
{
  mParams->vertical = state;
  scaleBarRedraw();
}

void ScaleBarForm::colorToggled( bool state )
{
  mParams->colorRamp = state;
  scaleBarRedraw();
}

void ScaleBarForm::invertToggled( bool state )
{
  mParams->invertRamp = state;
  scaleBarRedraw();
}

void ScaleBarForm::lengthChanged( int value )
{
  mParams->minLength = value;
  scaleBarRedraw();
}

void ScaleBarForm::thicknessChanged( int value )
{
  mParams->thickness = value;
  scaleBarRedraw();
}

void ScaleBarForm::positionChanged( int value )
{
  mParams->position = value;
  scaleBarRedraw();
}

void ScaleBarForm::indentXchanged( int value )
{
  mParams->indentX = value;
  scaleBarRedraw();
}

void ScaleBarForm::indentYchanged( int value )
{
  mParams->indentY = value;
  scaleBarRedraw();
}

void ScaleBarForm::customToggled( bool state )
{
  mParams->useCustom = state;
  customSpinBox->setEnabled(state);
  scaleBarRedraw();
}

void ScaleBarForm::customValChanged( int value )
{
  mParams->customVal = value;
  scaleBarRedraw();
}

// Update the value labels if the values are positive
void ScaleBarForm::updateValues( float zapv, float multiZv, float slicerv, 
                                 float xyzv, float modvv, char *units )
{
  QString str;
  if (mTimerID)
    killTimer(mTimerID);
  mTimerID = 0;
  if (!imodvStandalone()) {
    if (zapv > 0)
      str.sprintf("%g %s", zapv, units);
    else
      str = "";
    zapValue->setText(str);
    if (multiZv > 0)
      str.sprintf("%g %s", multiZv, units);
    else
      str = "";
    multiZValue->setText(str);
    if (slicerv > 0)
      str.sprintf("%g %s", slicerv, units);
    else
      str = "";
    slicerValue->setText(str);
    if (xyzv > 0)
      str.sprintf("%g %s", xyzv, units);
    else
      str = "";
    xyzValue->setText(str);
  }
  if (modvv > 0)
    str.sprintf("%g %s", modvv, units);
  else
    str = "";
  modvValue->setText(str);
}

// Start a timer to do an update after window draws a bar
void ScaleBarForm::startUpdateTimer()
{
  if (mTimerID)
    killTimer(mTimerID);
  mTimerID = startTimer(100);
}

// If the timer fires before an update, do an update
void ScaleBarForm::timerEvent( QTimerEvent *e )
{
  mTimerID = 0;
  scaleBarUpdate();
}

void ScaleBarForm::keyPressEvent( QKeyEvent * e )
{
  if (utilCloseKey(e))
    close();
  else if (imodvStandalone())
    imodvKeyPress(e);
  else
    ivwControlKey(0, e);
}

void ScaleBarForm::keyReleaseEvent( QKeyEvent * e )
{
  if (imodvStandalone())
    imodvKeyRelease(e);
  else
    ivwControlKey(1, e);
}

void ScaleBarForm::helpPressed()
{
  imodShowHelpPage("scalebar.html#TOP");
}

void ScaleBarForm::closeEvent( QCloseEvent * e )
{
  scaleBarClosing();
  e->accept();
}

