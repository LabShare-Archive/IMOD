/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/

// Respond to actions in the box
void imodvModeledForm::modelChanged( int which )
{
    imodvModeledNumber(which);
}

void imodvModeledForm::editClicked( int which )
{
    imodvModeledEdit(which);
}

void imodvModeledForm::moveClicked( int which )
{
    imodvModeledMove(which);
}

void imodvModeledForm::viewSelected( int which )
{
    imodvModeledView(which);
}

// Pass a name on as it changes
void imodvModeledForm::nameChanged( const QString &name )
{
    imodvModeledName(name);
}

// Inform of pixel size change when return pressed
void imodvModeledForm::newPixelSize()
{
    imodvModeledScale(1);
}

void imodvModeledForm::donePressed()
{
    imodvModeledDone();
}

void imodvModeledForm::helpPressed()
{
    imodvModeledHelp();
}

// Return the pixel size string when requested
QString imodvModeledForm::getPixelString()
{
    return pixelSizeEdit->text();
}

// Set states for a new model
void imodvModeledForm::setModel( int curModel, int numModels, QString filename, 
				 QString internalName, QString pixelString )
{
    currentSpinBox->setValue(curModel);
    currentSpinBox->setMaxValue(numModels);
    filenameLabel->setText(filename);
    internalNameEdit->setText(internalName);
    pixelSizeEdit->setText(pixelString);
}

// Initialize radio buttons
void imodvModeledForm::setMoveEdit( int move, int edit )
{
    moveGroup->setButton(move);
    editGroup->setButton(edit);
}

// Change the view selection
void imodvModeledForm::setViewSelection( int which )
{
    viewComboBox->setCurrentItem(which);
}

void imodvModeledForm::closeEvent( QCloseEvent * e )
{
    imodvModeledClosing();
    e->accept();
}

// Pass on keys
void imodvModeledForm::keyPressEvent( QKeyEvent * e )
{
    if (e->key() == Qt::Key_Escape)
	imodvModeledDone();
    else
	imodvKeyPress(e);
}

void imodvModeledForm::keyReleaseEvent( QKeyEvent * e )
{
    imodvKeyRelease(e);
}
