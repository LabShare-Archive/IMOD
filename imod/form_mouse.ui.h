/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/


void MouseForm::init()
{
    mPrefs = ImodPrefs->getDialogPrefs();
    ctrlRadioButton->setText(CTRL_STRING);
    QToolTip::add(ctrlRadioButton, "Make "CTRL_STRING
		  " key control whether sliders are continuously active");
    update();
}

// Set the state of the buttons
void MouseForm::update()
{
    diaSetGroup(hotKeyGroup, mPrefs->hotSliderKey);
    diaSetGroup(activeGroup, mPrefs->hotSliderFlag);
    diaSetGroup(mouseGroup, mPrefs->mouseMapping);
}

// record changes in the buttons
void MouseForm::flagChanged( int value )
{
    mPrefs->hotSliderFlag = value;
}

void MouseForm::keyChanged( int value )
{
    mPrefs->hotSliderKey = value;
}

void MouseForm::mappingChanged( int value )
{
    mPrefs->mouseMapping = value;
}
