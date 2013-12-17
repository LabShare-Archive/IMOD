/*
 *  formv_views.cpp - Class for model views dialog form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "formv_views.h"

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>

//Added by qt3to4:
#include <QKeyEvent>
#include <QCloseEvent>

#include "imod.h"
#include "info_cb.h"
#include "mv_menu.h"
#include "mv_input.h"
#include "mv_views.h"
#include "dia_qtutils.h"
#include "preferences.h"

/*
 *  Constructs a imodvViewsForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
imodvViewsForm::imodvViewsForm(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
imodvViewsForm::~imodvViewsForm()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void imodvViewsForm::languageChange()
{
  retranslateUi(this);
}

// Set the buttons to fit the font
void imodvViewsForm::init()
{
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  connect(viewListBox, SIGNAL(currentRowChanged(int)), this,
          SLOT(viewSelected(int)));
  setFontDependentWidths();
  adjustSize();
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
  width = viewListBox->fontMetrics().width("A very long string fits here");
  viewListBox->setFixedWidth(width);
}

void imodvViewsForm::manageListWidth()
{
  int width, maxwidth = 124;
  for (int i = 0; i < viewListBox->count(); i++) {
    width = viewListBox->fontMetrics().width(viewListBox->item(i)->text());
    maxwidth = B3DMAX(width + 30, maxwidth);
  }
  width =  viewListBox->width();
  if (maxwidth < width - 40 || maxwidth > width) {
    viewListBox->setFixedWidth(maxwidth);
    imod_info_input();
    adjustSize();
  }
}

// Store
void imodvViewsForm::storePressed()
{
  int item = viewListBox->currentRow();
  if (item >= 0)
    imodvViewsStore(item);
}

// Reload the stored view of the current item
void imodvViewsForm::revertPressed()
{
  int item = viewListBox->currentRow();
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
    if (!(viewListBox->findItems(label, Qt::MatchExactly)).count()) {
      imodvViewsNew(LATIN1(label));
      return;
    }
  }
}

// Delete: remove the item, inform imodv of the item and new current item
void imodvViewsForm::deletePressed()
{
  int item = viewListBox->currentRow();
  viewListBox->blockSignals(true);
  QListWidgetItem *label = viewListBox->takeItem(item);
  delete label;
  viewListBox->blockSignals(false);
  imodvViewsDelete(item, viewListBox->currentRow());
  deleteButton->setEnabled(viewListBox->count() > 1);
  manageListWidth();
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
  QListWidgetItem *label = viewListBox->item(item);
  viewListBox->scrollToItem(label);
  //??viewListBox->setCurrentRow(item);
}

void imodvViewsForm::donePressed()
{
  imodvViewsDone();
}

// When a new label is entered, get the most that will fit, send it back out,
// and if there is a current item, change the item and pass label on
void imodvViewsForm::newLabelEntered()
{
  QString newLabel = viewLabelEdit->text().left(VIEW_STRSIZE - 1);
  viewLabelEdit->setText(newLabel);
  int item = viewListBox->currentRow();
  if (item < 0)
    return;

  // Even changing the item text generates an unwanted signal or two
  //viewListBox->blockSignals(true);
  viewListBox->currentItem()->setText(newLabel);
  //viewListBox->blockSignals(false);
  imodvViewsLabel(LATIN1(newLabel), item);
  manageListWidth();
}

void imodvViewsForm::helpPressed()
{
  imodvViewsHelp();
}

// Call this after adding items, and it will try to set width 
void imodvViewsForm::setAutostore( int state )
{
  autoStoreBox->setChecked(state);
  manageListWidth();
  //viewListBox->setMinimumWidth(viewListBox->maxItemWidth() + 6);
}

// Add an item to the list
void imodvViewsForm::addItem( char * label )
{
  QString str = label;
  viewListBox->addItem(str);
  deleteButton->setEnabled(viewListBox->count() > 1);
}

// Select an item in the list
void imodvViewsForm::selectItem( int item, bool block )
{
  if (item < 0)
    item = 0;
  if (item >= viewListBox->count())
    item = viewListBox->count() - 1;
  viewListBox->blockSignals(block);
  viewListBox->setCurrentRow(item);
  viewListBox->scrollToItem(viewListBox->item(item));
  viewListBox->blockSignals(false);
  viewLabelEdit->setText(viewListBox->item(item)->text());
}

// Remove all items: need to block signals because selection will change
void imodvViewsForm::removeAllItems()
{
  viewListBox->blockSignals(true);
  viewListBox->clearSelection();
  viewListBox->clear();
  //for (int i = viewListBox->count() - 1; i >= 0; i--)
  //delete viewListBox->takeItem(i);
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
  int item = viewListBox->currentRow();
  int height =  viewListBox->fontMetrics().height();
  int jump = viewListBox->height() / height - 1;
  bool handled = !(e->modifiers() & Qt::KeypadModifier);
  if (utilCloseKey(e)) {
    imodvViewsDone();
    return;
  }
  switch (e->key()) {
  case Qt::Key_Up:
    if (handled)
      selectItem(item - 1, false);
    break;
  case Qt::Key_Down:
    if (handled)
      selectItem(item + 1, false);
    break;
  case Qt::Key_PageUp:
    if (handled)
      selectItem(item - jump, false);
    break;
  case Qt::Key_PageDown:
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

void imodvViewsForm::changeEvent(QEvent *e)
{
  QWidget::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    setFontDependentWidths();
}

