/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/
// THIS COMPANION CLASS MANAGES ALL OF THE REGULAR WIDGETS IN
// THE IMODV OBJECT EDIT WINDOW, AND CALLS CALLBACK FUNCTIONS
// TO CREATE A STACK OF WIDGETS IN THE PANEL FRAME.  THOSE
// WIDGETS CONTAIN ELEMENTS MANAGED BY THE ImodvObjed CLASS

void imodvObjedForm::init()
{
    int i;
    int width = 0, height = 0;
    ObjectEditField *oef;
    QVBoxLayout *panelFrameLayout = new QVBoxLayout(panelFrame, 5, 6,
						    "panelFrameLayout"); 
    
    mStack = new QWidgetStack(panelFrame);
    // Yes the stack has to be added to the layout too
    panelFrameLayout->addWidget(mStack);
    
    // Now create each widget and determine its size
    for  (i = 0; (objectEditFieldData[i].label); i++) {
	oef = &objectEditFieldData[i];
	
	// Add to the list box
	panelListBox->insertItem(oef->label, i);
	
	// Start with the frame as the parent
	oef->control = new QWidget(panelFrame);
	oef->mkwidget(i);	
	
	// See if we can avoid setting all widgets at start
	// if (oef->setwidget)
	 //   eof->setwidget();
	
	// You have to put it in layout and assess geometry before putting in stack
	panelFrameLayout->addWidget(oef->control);
	QSize size = oef->control->sizeHint();
	if (width < size.width())
	    width = size.width();
	if (height < size.height())
	    height = size.height();
	mStack->addWidget(oef->control, i);
    }
    
    // Set frame size for all widgets
    // 20 needed to be added with margin 11, 8 with margin 5
    panelFrame->setMinimumWidth(width + 8);   
    panelFrame->setMinimumHeight(height + 8);
    mStack->raiseWidget(0);
    
    imodvObjedMakeOnOffs(checkBoxFrame);
    
    // Set size of list box
    panelListBox->setMinimumWidth(panelListBox->maxItemWidth() + 2);
    panelListBox->setMinimumHeight(panelListBox->count() * 
				   panelListBox->itemHeight() + 2);
}

// Pass on changes in the valious controls
void imodvObjedForm::objectSelected( int which )
{
    imodvObjedSelect(which);
}

void imodvObjedForm::editSelected( int item )
{
    imodvObjedEditData(item);
}

void imodvObjedForm::objSliderChanged( int value )
{
    imodvObjedSelect(value);
}

void imodvObjedForm::nameChanged(const QString & str )
{
    imodvObjedName(str.latin1());
}

void imodvObjedForm::typeSelected( int item )
{
    imodvObjedDrawData(item);
}

void imodvObjedForm::styleSelected( int item )
{
    imodvObjedStyleData(item);
}

// A new frame is selected from list - raise it and let objed update it
void imodvObjedForm::frameSelected( int item )
{
    mStack->raiseWidget(item);
    imodvObjedFramePicked(item);
}

// Done and help buttons
void imodvObjedForm::donePressed()
{
    imodvObjedDone();
}

void imodvObjedForm::helpPressed()
{
    imodvObjedHelp();
}

// This provides update information when a new object is set
void imodvObjedForm::updateObject(int ob, int numObj, int drawType, int drawStyle, 
				  QColor color, char *name)
{
    colorFrame->setPaletteBackgroundColor(color);
    objectSpinBox->setMaxValue(numObj);
    objectSpinBox->setValue(ob);
    objectSpinBox->setEnabled(numObj > 1);
    objectSlider->setMaxValue(numObj);
    objectSlider->setValue(ob);
    objectSlider->setEnabled(numObj > 1);
    dataTypeComboBox->setCurrentItem(drawType);
    drawStyleComboBox->setCurrentItem(drawStyle);
    QString str = name;
    nameLineEdit->setText(str);
}

void imodvObjedForm::updateColorBox( QColor color )
{
    colorFrame->setPaletteBackgroundColor(color);
}

// This is a call for final setup of the box 
void imodvObjedForm::setCurrentFrame( int frame, int editData )
{	
    mStack->raiseWidget(frame);
    panelListBox->setCurrentItem(frame);
    oneAllComboBox->setCurrentItem(editData);
}	

// Pass on close events and key events
void imodvObjedForm::closeEvent( QCloseEvent * e )
{
    imodvObjedClosing();
    e->accept();
}

void imodvObjedForm::keyPressEvent( QKeyEvent * e )
{
    if (hotSliderFlag() != NO_HOT_SLIDER && e->key() == hotSliderKey()) {
	imodvObjedCtrlKey(true);
	grabKeyboard();
    }
    if (e->key() == Qt::Key_Escape)
	imodvObjedDone();
    else
	imodvKeyPress(e);
}

void imodvObjedForm::keyReleaseEvent( QKeyEvent * e )
{
    if (e->key() == hotSliderKey()) {
	imodvObjedCtrlKey(false);
	releaseKeyboard();
    }
     imodvKeyRelease(e);
}
