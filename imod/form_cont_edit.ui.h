/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/

// SET ALL LAYOUT MARGINS TO 7

void ContSurfPoint::init()
{
    mCtrlPressed = false;
    mSliderPressed = false;
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
    iceGhostToggled(state ? 1 : 0, mSurfGhostFlag);
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
    iceLabelChanged((char *)str.latin1(), 0);
}

// Pass on the slider if clicked or hot slider enabled
void ContSurfPoint::pointSliderChanged( int value )
{
    displayPointSize(value / 10., 0);
    if (!mSliderPressed || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && mCtrlPressed) ||
	(hotSliderFlag() == HOT_SLIDER_KEYUP && !mCtrlPressed))
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

void ContSurfPoint::pointLabelChanged( const QString &str )
{
    iceLabelChanged((char *)str.latin1(), 1);
}

void ContSurfPoint::ghostChanged( int value )
{
    iceGhostInterval(value);
}

// Pass on ghost button changes with the relevant flag
void ContSurfPoint::upGhostToggled( bool state )
{
    iceGhostToggled(state ? 1 : 0, mNextSecGhostFlag);
}

void ContSurfPoint::downGhostToggled( bool state )
{
    iceGhostToggled(state ? 1 : 0, mPrevSecGhostFlag);
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
void ContSurfPoint::setGhostState( int interval, int ghostmode, int nextFlag, 
				   int prevFlag, int surfFlag )
{
    mSurfGhostFlag = surfFlag;
    mPrevSecGhostFlag = prevFlag;
    mNextSecGhostFlag = nextFlag;
    
    diaSetSpinBox(ghostSpinBox, interval);
    diaSetChecked(downGhostBox, ghostmode & prevFlag);
    diaSetChecked(upGhostBox, ghostmode & nextFlag);
    diaSetChecked(surfGhostBox, ghostmode & surfFlag);
}

// Set the point size slider, increasing the range if necessary, and show it in edit box
void ContSurfPoint::setPointSize( float size, int defval )
{
    pointSizeEdit->setEnabled(defval >= 0);
    pointSizeSlider->setEnabled(defval >= 0);
    if (defval < 0) {
	diaSetSlider(pointSizeSlider, 0.);
	pointSizeEdit->setText("No Point");
    } else {
	int value = (int)floor(10. * size + 0.5);
	if (value > pointSizeSlider->maxValue())
	    pointSizeSlider->setMaxValue(value);
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
	surfaceSpinBox->setValue(0);
    } else {
 	surfaceSpinBox->setSpecialValueText("");
	surfaceSpinBox->blockSignals(true);
	surfaceSpinBox->setMaxValue(maxVal);
	surfaceSpinBox->setValue(value);
	surfaceSpinBox->blockSignals(false);
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
	timeSpinBox->blockSignals(true);
	timeSpinBox->setMaxValue(maxVal);
	timeSpinBox->setValue(value);
	timeSpinBox->blockSignals(false);
    }
}

// Set the labels, or set to no Contour/Point
void ContSurfPoint::setLabels( QString contLabel, int noCont, QString ptLabel, int noPoint )
{
    contourEdit->setEnabled(!noCont);
    if (noCont) 
	contourEdit->setText("No Contour");
    else
	contourEdit->setText(contLabel);
    
    pointLabelEdit->setEnabled(!noPoint);
    if (noPoint) 
	pointLabelEdit->setText("No Point");
    else
	pointLabelEdit->setText(ptLabel);
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
    if (e->key() == Qt::Key_Escape)
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
