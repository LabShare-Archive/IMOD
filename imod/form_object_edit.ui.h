/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/

// Respond to signals from the widgets
void objectEditForm::helpPressed()
{
    ioew_help();
}

void objectEditForm::nameChanged( const QString & newName )
{
    ioew_nametext(newName.latin1());
}

void objectEditForm::symbolChanged( int value )
{
    ioew_symbol(value);
}

void objectEditForm::OKPressed()
{
    ioew_quit();
}

void objectEditForm::radiusChanged(int value)
{
    ioew_pointsize(value);
}

void objectEditForm::selectedSurface( int value )
{
    ioew_surface(value);
}

void objectEditForm::selectedType( int value )
{
    ioew_open(value);
}

void objectEditForm::sizeChanged( int value )
{
    QString str;
    str.sprintf("%d", value);
    sizeLabel->setText(str);
    ioew_symsize(value);
}

void objectEditForm::toggledDraw( bool state )
{
    ioew_draw(state ? 1 : 0);
}

void objectEditForm::toggledFill( bool state )
{
    ioew_fill(state ? 1 : 0);
}

void objectEditForm::toggledMarkEnds( bool state )
{
    ioew_ends(state ? 1 : 0);
}

void objectEditForm::toggledTime( bool state )
{
    ioew_time(state ? 1 : 0);
}

void objectEditForm::widthChanged( int value )
{
     ioew_linewidth(value);
}	

// Set the state of the widgets initially or when a new object is selected
void objectEditForm::setSymbolProperties( int which, bool fill, bool markEnds, int size )
{
   symbolComboBox->setCurrentItem(which);
   diaSetChecked(fillCheckBox, fill);
   diaSetChecked(markCheckBox, markEnds);
    diaSetSlider(sizeSlider, size);
    QString str;
    str.sprintf("%d", size);
    sizeLabel->setText(str);
}

void objectEditForm::setDrawBox( bool state )
{
    diaSetChecked(drawCheckBox, state);
}

void objectEditForm::setObjectName( char *name )
{
    QString str = name;
    nameEdit->blockSignals(true);
    nameEdit->setText(str);
    nameEdit->blockSignals(false);
}

void objectEditForm::setTimeBox( bool state, bool enabled )
{
    diaSetChecked(timeCheckBox, state);
    timeCheckBox->setEnabled(enabled);
}

void objectEditForm::setPointRadius( int value )
{
    diaSetSpinBox(radiusSpinBox, value);
}

void objectEditForm::setFrontSurface( int value )
{
    diaSetGroup(surfaceButtonGroup, value);
}

void objectEditForm::setObjectType( int value )
{
    diaSetGroup(typeButtonGroup, value);
}

void objectEditForm::setLineWidth( int value )
{
    diaSetSpinBox(widthSpinBox, value);
}

// Handle close event; pass on keypress
void objectEditForm::closeEvent( QCloseEvent *e )
{
    ioew_closing();
    e->accept();
}

void objectEditForm::keyPressEvent( QKeyEvent * e )
{
    if (e->key() == Qt::Key_Escape)
	ioew_quit();
    else
	ivwControlKey(0, e);
    //e->ignore();
}

void objectEditForm::keyReleaseEvent( QKeyEvent * e )
{
    ivwControlKey(1, e);
}
