/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/

// Initialize the flags and set slider widths
void imodvDepthcueForm::init()
{
    mCtrlPressed = false;
    mStartPressed = false;
    mEndPressed = false;
    int width = DEPTHCUE_MAX - DEPTHCUE_MIN + 16;
    startSlider->setMinimumWidth(width);
    startSlider->setMinValue(DEPTHCUE_MIN);
    startSlider->setMaxValue(DEPTHCUE_MAX);
    endSlider->setMinimumWidth(width);
    endSlider->setMinValue(DEPTHCUE_MIN);
    endSlider->setMaxValue(DEPTHCUE_MAX);
    
}

void imodvDepthcueForm::depthcueToggled( bool state )
{
    imodvDepthcueToggle(state ? 1 : 0);
}

// Display and change functions for the Start slider
void imodvDepthcueForm::displayStartLabel( int value )
{
    mStr.sprintf("%d", value);
    startTextLabel->setText(mStr);
    mStartDisplayed = value;
}

void imodvDepthcueForm::startChanged( int value )
{
    if (!mStartPressed || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && mCtrlPressed) ||
	(hotSliderFlag() == HOT_SLIDER_KEYUP && !mCtrlPressed))
	imodvDepthcueStartEnd(value, false, mStartPressed);
}

void imodvDepthcueForm::startPressed()
{
    mStartPressed = true;
}

void imodvDepthcueForm::startReleased()
{
    mStartPressed = false;
    startChanged(mStartDisplayed);
}

// Display and change functions for the End slider
void imodvDepthcueForm::displayEndLabel( int value )
{
    mStr.sprintf("%d", value);
    endTextLabel->setText(mStr);
    mEndDisplayed = value;
}

void imodvDepthcueForm::endChanged( int value )
{
     if (!mEndPressed || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && mCtrlPressed) ||
	(hotSliderFlag() == HOT_SLIDER_KEYUP && !mCtrlPressed))
	imodvDepthcueStartEnd(value, true, mEndPressed);
}

void imodvDepthcueForm::endPressed()
{
    mEndPressed = true;
}

void imodvDepthcueForm::endReleased()
{
    mEndPressed = false;
    endChanged(mEndDisplayed);
}

void imodvDepthcueForm::donePressed()
{
    imodvDepthcueDone();
}

void imodvDepthcueForm::helpPressed()
{
    imodvDepthcueHelp();
}

// Set the states of the widgets
void imodvDepthcueForm::setStates( int enabled, int start, int end )
{
    diaSetChecked(enableBox, enabled);
    displayStartLabel(start);
    diaSetSlider(startSlider, start);
    displayEndLabel(end);
    diaSetSlider(endSlider, end);
}

// Tell imodv we are closing
void imodvDepthcueForm::closeEvent( QCloseEvent * e )
{
    imodvDepthcueClosing();
    e->accept();
}

// Quit on escape, set flag and grab keyboard if ctrl, pass key on
void imodvDepthcueForm::keyPressEvent( QKeyEvent * e )
{
    if (e->key() == Qt::Key_Escape) {
	imodvDepthcueDone();
    } else {
    
	if (hotSliderFlag() != NO_HOT_SLIDER && e->key() == hotSliderKey()) {
	    mCtrlPressed = true;
	    grabKeyboard();
	}
	imodvKeyPress(e);
    }
}

// release keyboard if ctrl
void imodvDepthcueForm::keyReleaseEvent( QKeyEvent * e )
{
  if (e->key() == hotSliderKey()) {
	mCtrlPressed = false;
        releaseKeyboard();
  }
  imodvKeyRelease(e);
}

