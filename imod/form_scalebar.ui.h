/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you want to add, delete, or rename functions or slots, use
** Qt Designer to update this file, preserving your code.
**
** You should not define a constructor or destructor in this file.
** Instead, write your code in functions called init() and destroy().
** These will automatically be called by the form's constructor and
** destructor.
*****************************************************************************/


void ScaleBarForm::init()
{
  mTimerID = 0;
  mParams = scaleBarGetParams();
  diaSetChecked(drawCheckBox, mParams->draw);
  diaSetChecked(whiteCheckBox, mParams->white); 
  diaSetChecked(verticalCheckBox, mParams->vertical); 
  diaSetSpinBox(lengthSpinBox, mParams->minLength);
  diaSetSpinBox(thicknessSpinBox, mParams->thickness);
  diaSetSpinBox(indentXspinBox, mParams->indentX);
  diaSetSpinBox(indentYspinBox, mParams->indentY);
  positionComboBox->setCurrentItem(mParams->position);
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
    if (e->key() == Qt::Key_Escape)
	close();
    else
 	ivwControlKey(0, e);
}

void ScaleBarForm::keyReleaseEvent( QKeyEvent * e )
{
  ivwControlKey(1, e);
}

void ScaleBarForm::helpPressed()
{
  imodShowHelpPage("scalebar.html");
}

void ScaleBarForm::closeEvent( QCloseEvent * e )
{
    scaleBarClosing();
    e->accept();
}
