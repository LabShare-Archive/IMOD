/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/

// Set the buttons to fit the font
void imodvViewsForm::init()
{
    setFontDependentWidths();
}

void imodvViewsForm::setFontDependentWidths()
{
    int width = diaGetButtonWidth(this, ImodPrefs->getRoundedStyle(), 1.2,
                                  newButton->text());
    int width2 = diaGetButtonWidth(this, ImodPrefs->getRoundedStyle(), 1.2,
                                  saveButton->text());
    if (width < width2)
	width = width2;
    storeButton->setFixedWidth(width);
    revertButton->setFixedWidth(width);
    newButton->setFixedWidth(width);
    deleteButton->setFixedWidth(width);
    saveButton->setFixedWidth(width);
}

// Store
void imodvViewsForm::storePressed()
{
    int item = viewListBox->currentItem();
    if (item >= 0)
	imodvViewsStore(item);
}

// Reload the stored view of the current item
void imodvViewsForm::revertPressed()
{
    int item = viewListBox->currentItem();
    if (item >= 0)
	imodvViewsGoto(item, true);
}

// New view: find a unique temporary label for it and pass it on
void imodvViewsForm::newPressed()
{
    int i;
    QString label;
    for (i = 1; i < 1000; i++){
	label.sprintf("view %d", i);
	if (!viewListBox->findItem(label)) { 
	    imodvViewsNew(label.latin1());
	    return;
	}
    }
}

// Delete: remove the item, inform imodv of the item and new current item
void imodvViewsForm::deletePressed()
{
    int item = viewListBox->currentItem();
    viewListBox->blockSignals(true);
    viewListBox->removeItem(item);
    viewListBox->blockSignals(false);
    imodvViewsDelete(item, viewListBox->currentItem());
    deleteButton->setEnabled(viewListBox->count() > 1);
}

void imodvViewsForm::savePressed()
{
    imodvViewsSave();
}

void imodvViewsForm::autostoreToggled( bool state )
{
    imodvViewsAutostore(state ? 1 : 0);
}

// This receives a signal when a view is highlighted or selected, so it sets the selection
void imodvViewsForm::viewSelected( int item )
{
    imodvViewsGoto(item, true);
    viewListBox->setCurrentItem(item);
}

void imodvViewsForm::donePressed()
{
    imodvViewsDone();
}

// When a new label is entered, get the most that will fit, send it back out,
// and if there is a current item, change the item and pass label on
void imodvViewsForm::newLabelEntered()
{
    QString newLabel = viewLabelEdit->text().left(VIEW_LABEL_LENGTH - 1);
    viewLabelEdit->setText(newLabel);
    int item = viewListBox->currentItem();
    if (item < 0)
	return;

    // Even changing the item text generates an unwanted signal or two
    viewListBox->blockSignals(true);
    viewListBox->changeItem(newLabel, item);
    viewListBox->blockSignals(false);
    imodvViewsLabel(newLabel.latin1(), item);
}

void imodvViewsForm::helpPressed()
{
    imodvViewsHelp();
}

// Call this after adding items, and it will try to set width 
void imodvViewsForm::setAutostore( int state )
{
    autoStoreBox->setChecked(state);
    viewListBox->setMinimumWidth(viewListBox->maxItemWidth() + 6);
}

// Add an item to the list
void imodvViewsForm::addItem( char * label )
{
    QString str = label;
    viewListBox->insertItem(str);
    deleteButton->setEnabled(viewListBox->count() > 1);
}

// Select an item in the list
void imodvViewsForm::selectItem( int item, bool block )
{
    if (item < 0)
	item = 0;
    if (item >= viewListBox->count())
	item = viewListBox->count();
    viewListBox->blockSignals(block);
    viewListBox->setCurrentItem(item);
    viewListBox->ensureCurrentVisible();
    viewListBox->blockSignals(false);
    viewLabelEdit->setText(viewListBox->currentText());
}

// Remove all items: need to block signals because selection will change
void imodvViewsForm::removeAllItems()
{
    viewListBox->blockSignals(true);
    viewListBox->clearSelection();
    for (int i = viewListBox->count() - 1; i >= 0; i--)
	viewListBox->removeItem(i);
    viewListBox->blockSignals(false);    
}

void imodvViewsForm::closeEvent( QCloseEvent * e )
{
   imodvViewsClosing();
    e->accept();
}

// Use arrow and page keys if not from keypad, and pass event on
// if it not handled
void imodvViewsForm::keyPressEvent( QKeyEvent * e )
{
    int item = viewListBox->currentItem();
    int jump = viewListBox->numItemsVisible() - 1;
    bool handled = !(e->state() & Keypad);
    switch (e->key()) {
    case Key_Escape:
	imodvViewsDone();
	handled = true;
	break;
    case Key_Up:
	if (handled)
	    selectItem(item - 1, false);
	break;
    case Key_Down:
	if (handled)
	    selectItem(item + 1, false);
	break;
    case Key_Prior:
	if (handled)
	    selectItem(item - jump, false);
	break;
    case Key_Next:
	if (handled)
	    selectItem(item + jump, false);
	break;
	
    default:	
	handled = false;
	break;
    }
    if (!handled)
	imodvKeyPress(e);
}

void imodvViewsForm::keyReleaseEvent( QKeyEvent * e )
{
    imodvKeyRelease(e);
}

void imodvViewsForm::fontChange( const QFont & oldFont )
{
    setFontDependentWidths();
}
