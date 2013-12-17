/*
 *  form_slicerangle.cpp - Class for slicer angle dialog form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "form_slicerangle.h"

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>

//Added by qt3to4:
#include <QCloseEvent>
#include <QKeyEvent>

#include "imod.h"
#include "preferences.h"
#include "dia_qtutils.h"
#include "string.h"
#include "sslice.h"
#include "ilist.h"
#include "imodview.h"
#include "b3dutil.h"
#include "control.h"
#include "undoredo.h"

#define SLAN_COLS 7

/*
 *  Constructs a SlicerAngleForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
SlicerAngleForm::SlicerAngleForm(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
SlicerAngleForm::~SlicerAngleForm()
{
  destroy();
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void SlicerAngleForm::languageChange()
{
  retranslateUi(this);
}

void SlicerAngleForm::init()
{
  // Determine maximum time allowed in model, and maximum time
  // Set the time increment, so all time indexes start at 1
  SlicerAngles *slanp;
  bool continuous;
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  mTimeInc = 0;
  mIgnoreCurChg = false;
  mMaxImageTime = ivwGetMaxTime(App->cvi);
  if (!mMaxImageTime) {
    mMaxImageTime = 1;
    mTimeInc = 1;
  }
  mMaxModelTime = mMaxImageTime;
  Ilist *angList = App->cvi->imod->slicerAng;
  for (int i = 0; i < ilistSize(angList); i++) {
    slanp = (SlicerAngles *)ilistItem(angList, i);
    mMaxModelTime = B3DMAX(mMaxModelTime, slanp->time);
  }
  copySpin->setMaximum(mMaxModelTime);
  renumberSpin->setMaximum(mMaxModelTime);
  
  connect(table, SIGNAL(itemSelectionChanged()), this, 
          SLOT(selectionChanged()));
  connect(table, SIGNAL(cellChanged(int, int)), this, 
          SLOT(cellChanged(int,int)));

  int volHeight = volumeGroup->height();
  // Get rid of unneeded buttons
  if (mMaxModelTime == 1) {
    removeButton->hide();
    copyButton->hide();
    renumberButton->hide();
    insertButton->hide();
    renumberSpin->hide();
    copySpin->hide();
    volumeGroup->hide();
    QSize winsize = size();
    resize(winsize.width(), winsize.height() - (int)(volHeight + 16));
  }
  if (mMaxImageTime == 1)
    timeLabel->hide();
  mCurRow = new int[mMaxModelTime + 1];

  // Set the current time from the top slicer or fall back to display time
  mCurTime = getTopSlicerTime( continuous);
  if (mCurTime < 0)
    ivwGetTime(App->cvi, &mCurTime);
  mCurTime += mTimeInc;
  setFontDependentWidths();
  loadTable(mCurTime);
  mCurRow[mCurTime] = -1;
  
  // Set the current row
  if (table->rowCount()) {
    table->selectRow(0);
    mCurRow[mCurTime] = 0;
  }
  updateEnables();
  
  // Set time label if there are multiple files
  if (mMaxModelTime > 1)
    setTimeLabel();
  setFontDependentWidths();
}

void SlicerAngleForm::destroy()
{
  delete [] mCurRow;
}

// Enables/disables buttons based on state
void SlicerAngleForm::updateEnables()
{
  int numRows = table->rowCount();
  setButton->setEnabled(numRows > 0);
  deleteButton->setEnabled(numRows > 0);
  renumberButton->setEnabled(renumberSpin->value() != mCurTime);
  copyButton->setEnabled(copySpin->value() != mCurTime);
}

// Slot called when font changes
void SlicerAngleForm::changeEvent(QEvent *e)
{
  QWidget::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    setFontDependentWidths();
}

// Handles width settings with font size changes
void SlicerAngleForm::setFontDependentWidths()
{
  int width, width2, extra = 8;
  QString str;
  bool rounded = ImodPrefs->getRoundedStyle();
  width = diaSetButtonWidth(setButton, rounded, 1.2, "Set Angles");
  deleteButton->setFixedWidth(width);
  diaSetButtonWidth(renumberButton, rounded, 1.2, "Renumber To");
  diaSetButtonWidth(copyButton, rounded, 1.2, "Copy From");
  width = diaSetButtonWidth(removeButton, rounded, 1.2, "Remove");
  insertButton->setFixedWidth(width);
  width = table->fontMetrics().width("-90.00") + extra;
  table->setColumnWidth(0, width);
  width = table->fontMetrics().width("-180.00") + extra;
  table->setColumnWidth(1, width);
  table->setColumnWidth(2, width);
  str.sprintf("%.2f", (float)App->cvi->xsize);
  width = table->fontMetrics().width(str) + extra;
  width2 = B3DNINT(1.2 * table->fontMetrics().width("X cen")) + extra;
  table->setColumnWidth(3, B3DMAX(width, width2));
  str.sprintf("%.2f", (float)App->cvi->ysize);
  width = table->fontMetrics().width(str) + extra;
  table->setColumnWidth(4, B3DMAX(width, width2));
  str.sprintf("%.2f", (float)App->cvi->zsize);
  width = table->fontMetrics().width(str) + extra;
  table->setColumnWidth(5, B3DMAX(width, width2));
  width = table->fontMetrics().width("abcdefghijklmnop") + extra;
  table->setColumnWidth(6, width);
}

// Manage the time label
void SlicerAngleForm::setTimeLabel()
{
  QString str;
  str.sprintf("Time: %d (%s)", mCurTime, ivwGetTimeIndexLabel(App->cvi, mCurTime));
  timeLabel->setText(str);
}

// Call from the slicer to set values into a row
void SlicerAngleForm::setCurrentOrNewRow( int time, bool newrow )
{
  if (time + mTimeInc != mCurTime)
    switchTime(time + mTimeInc, false);
  if (newrow || !table->rowCount())
    newClicked();
  else
    getAngClicked();
}

// Call from a slicer to set slicer angles
void SlicerAngleForm::setAnglesFromRow( int time )
{
  if (time + mTimeInc != mCurTime)
    switchTime(time + mTimeInc, false);
  setAngles(true);
}

// Get angle function, originally a button
void SlicerAngleForm::getAngClicked()
{
  int time, index;
  float angles[3];
  Ipoint center;
  if (getTopSlicerAngles(angles, &center, time))
    return;
  if (time + mTimeInc != mCurTime)
    return;
  SlicerAngles *slanp = findAngles(mCurRow[mCurTime], index);
  if (!slanp)
    return;
  App->cvi->undo->modelChange();
  slanp->angles[0] = angles[0];
  slanp->angles[1] = angles[1];
  slanp->angles[2] = angles[2];
  slanp->center = center;
  App->cvi->undo->finishUnit();
  loadRow(slanp, mCurRow[mCurTime], true);
}

// Set angle button
void SlicerAngleForm::setAngClicked()
{
  setAngles(true);
}

// Common routine to set angles
void SlicerAngleForm::setAngles( bool draw )
{
  int index;
  bool continuous;
  if (getTopSlicerTime(continuous) + mTimeInc != mCurTime)
    return;
  SlicerAngles *slanp = findAngles(mCurRow[mCurTime], index);
  if (!slanp)
    return;
  setTopSlicerAngles(slanp->angles, &slanp->center, draw);
}

// Button to delete one row
void SlicerAngleForm::deleteClicked()
{
  int index;
  SlicerAngles *slanp = findAngles(mCurRow[mCurTime], index);
  if (!slanp)
    return;
  App->cvi->undo->modelChange();
  ilistRemove(App->cvi->imod->slicerAng, index);
  App->cvi->undo->finishUnit();
  loadTable(mCurTime);
  mCurRow[mCurTime] = B3DMIN(table->rowCount() - 1, mCurRow[mCurTime] );
  table->selectRow(mCurRow[mCurTime]);  
  updateEnables();
  updateTopIfContinuous();
}

// Function to create a new row with current angles: originally a button
void SlicerAngleForm::newClicked()
{
  Imod *imod = App->cvi->imod;
  SlicerAngles slang;
  if (getTopSlicerAngles(slang.angles, &slang.center, slang.time))
    return;
  if (!imod->slicerAng) 
    imod->slicerAng = ilistNew(sizeof(SlicerAngles), 4);
  if (!imod->slicerAng)
    return;
  App->cvi->undo->modelChange();
  slang.time += mTimeInc;
  slang.label[0] = 0x00;
  ilistAppend(imod->slicerAng, &slang);
  App->cvi->undo->finishUnit();
  loadTable(slang.time);
  mCurRow[mCurTime] = table->rowCount() - 1;
  table->selectRow(mCurRow[mCurTime]);
  updateEnables();
}

////////////////
// BUTTONS TO OPERATE ON SETS OF ANGLES FOR DIFFERENT TIMES

// Remove an angle set means delete items at the current time and shift all higher times
// down
void SlicerAngleForm::removeClicked()
{
  SlicerAngles *slanp;  
  Ilist *angList = App->cvi->imod->slicerAng;
  bool changed = false;
  App->cvi->undo->modelChange();
  for (int i = 0; i < ilistSize(angList); i++) {
    slanp = (SlicerAngles *)ilistItem(angList, i);
    if (slanp->time == mCurTime) {
      ilistRemove(angList, i--);
      changed = true;
    } else if (slanp->time > mCurTime) {
      slanp->time--;
      changed = true;
    }
  }
  finishOrFlushUnit(changed);
  loadTable(mCurTime);
  updateEnables();
  updateTopIfContinuous();
}

// Insert means shift all items at current or higher time up one
void SlicerAngleForm::insertClicked()
{
  SlicerAngles *slanp;  
  Ilist *angList = App->cvi->imod->slicerAng;
  bool changed = false;
  App->cvi->undo->modelChange();
  for (int i = 0; i < ilistSize(angList); i++) {
    slanp = (SlicerAngles *)ilistItem(angList, i);
    if (slanp->time >= mCurTime) {
      slanp->time++;
      changed = true;
    }
  }
  finishOrFlushUnit(changed);
  loadTable(mCurTime);
  updateEnables();
  updateTopIfContinuous();
}

// Renumber means shift items from FROM + 1 to TO down 1, shift FROM to TO
void SlicerAngleForm::renumberClicked()
{
  int toTime = renumberSpin->value();
  SlicerAngles *slanp;  
  Ilist *angList = App->cvi->imod->slicerAng;
  bool changed = false;
  if (toTime == mCurTime)
    return;
  int shift = toTime > mCurTime ? -1 : 1;
  App->cvi->undo->modelChange();
  for (int i = 0; i < ilistSize(angList); i++) {
    slanp = (SlicerAngles *)ilistItem(angList, i);
    if (slanp->time == mCurTime) {
      slanp->time = toTime;
      changed = true;
    } else if ((slanp->time > mCurTime && slanp->time <= toTime) ||
               (slanp->time < mCurTime && slanp->time >= toTime)) {
      slanp->time += shift;
      changed = true;
    }
  }
  finishOrFlushUnit(changed);
  loadTable(mCurTime);
  updateEnables();
  updateTopIfContinuous();
}

// Copy means remove existing ones at current time and copy from other time
void SlicerAngleForm::copyClicked()
{
  SlicerAngles *slanp;  
  Ilist *angList = App->cvi->imod->slicerAng;
  bool changed = false;
  SlicerAngles slang;
  int toTime = copySpin->value();
  int i;
  if (toTime == mCurTime)
    return;
  
  // Have to remove ones at this time in separate loop
  App->cvi->undo->modelChange();
  for (i = 0; i < ilistSize(angList); i++) {
    slanp = (SlicerAngles *)ilistItem(angList, i);
    if (slanp->time == mCurTime) {
      ilistRemove(angList, i--);
      changed = true;
    }
  }
  for (i = 0; i < ilistSize(angList); i++) {
    slanp = (SlicerAngles *)ilistItem(angList, i);
    if (slanp->time == toTime) {
      slang = *slanp;
      slang.time = mCurTime;
      ilistAppend(angList, &slang);
      changed = true;
    }
  }
  finishOrFlushUnit(changed);
  loadTable(mCurTime);
  updateEnables();
  updateTopIfContinuous();
}

// The renumber or copy spin buttons changed: enable buttons as needed
void SlicerAngleForm::renumberChanged( int value )
{
  setFocus();
  updateEnables();
}

void SlicerAngleForm::copyChanged( int value )
{
  setFocus();
  updateEnables();
}

////////////////////
// HELP
void SlicerAngleForm::helpClicked()
{
  imodShowHelpPage("slicerAngles.html#TOP");
}

// When a cell changes, process it
void SlicerAngleForm::cellChanged( int row, int col )
{
  int index;
  float val;
  QString str = table->item(row, col)->text().trimmed();
  SlicerAngles *slanp = findAngles(row, index);
  if (!slanp)
    return;
  App->cvi->undo->modelChange();
  if (col < 6)
    val = str.toFloat();
  if (!col)
    slanp->angles[0] = B3DMIN(90., B3DMAX(-90., val));
  else if (col < 3)
    slanp->angles[col] = B3DMIN(180., B3DMAX(-180., val));
  else if (col == 3)
    slanp->center.x = str.toFloat() - 1.f;
  else if (col == 4)
    slanp->center.y = str.toFloat() - 1.f;
  else if (col == 5)
    slanp->center.z = str.toFloat() - 1.f;
  else {
    str.truncate(ANGLE_STRSIZE - 1);
    strcpy(slanp->label, LATIN1(str));
  }
  App->cvi->undo->finishUnit();

  // Set the focus back to the table so arrow buttons work
  table->setFocus();
  table->selectRow(row);
  //imodPrintStderr("cell at row %d  current %d\n", row, mCurRow[mCurTime]);
  // Make it ignore the next current cell change since it is on the wrong row
  mIgnoreCurChg = true;
  if (mCurRow[mCurTime] == row)
    updateTopIfContinuous();
}

// When current cell changes, set angle if continuous
void SlicerAngleForm::selectionChanged()
{
  if (mIgnoreCurChg) {
    mIgnoreCurChg = false;
    return;
  }
  //imodPrintStderr("sel changed row %d col %d\n", table->currentRow(), table->currentColumn());
  mCurRow[mCurTime] = table->currentRow();
  updateTopIfContinuous();
}

// Set angles of top slicer if time matches and it is in continuous mode
void SlicerAngleForm::updateTopIfContinuous()
{
  bool continuous;
  int time = getTopSlicerTime(continuous) + mTimeInc;
  if (time == mCurTime && continuous)
    setAngles(true);
}

// Load the whole table and set current time
void SlicerAngleForm::loadTable( int time )
{
  Imod *imod = App->cvi->imod;
  SlicerAngles *slanp;
  int i, j, numRows = 0;
  
  table->blockSignals(true);

  // Load all the items whose time matches
  for (i = 0; i < ilistSize(imod->slicerAng); i++) {
    slanp = (SlicerAngles *)ilistItem(imod->slicerAng, i);
    if (slanp->time != time)
      continue;
    
    // Add a row if needed
    if (numRows + 1 > table->rowCount()) {
      table->insertRow(numRows);
      table->setRowHeight(numRows, table->fontMetrics().height() + 3);
      for (j = 0; j < SLAN_COLS; j++) {
        QTableWidgetItem *item = new QTableWidgetItem();
        if (j < 6)
          item->setTextAlignment(Qt::AlignRight);
        table->setItem(numRows, j, item);
      }
    }
    loadRow(slanp, numRows++, false);
  }
  
  // Get rid of extra rows (hope it deletes the items)
  for (i = table->rowCount() - 1; i >= numRows; i--)
    table->removeRow(i);
  mCurTime = time;
  table->blockSignals(false);
}

// Load one row of the table
void SlicerAngleForm::loadRow( SlicerAngles *slanp, int row , bool block)
{
  QString str;
  if (block)
    table->blockSignals(true);
  str.sprintf("%.2f", slanp->angles[0]);
  table->item(row, 0)->setText(str);
  str.sprintf("%.2f", slanp->angles[1]);
  table->item(row, 1)->setText(str);
  str.sprintf("%.2f", slanp->angles[2]);
  table->item(row, 2)->setText(str);
  str.sprintf("%.2f", slanp->center.x + 1.f);
  table->item(row, 3)->setText(str);
  str.sprintf("%.2f", slanp->center.y + 1.f);
  table->item(row, 4)->setText(str);
  str.sprintf("%.2f", slanp->center.z + 1.f);
  table->item(row, 5)->setText(str);
  table->item(row, 6)->setText(QString(slanp->label));
  if (block)
    table->blockSignals(false);
}

// Switch the table to a new time and manage selection, enables, and label; also set
// angles if in continuous mode and doSet is true
void SlicerAngleForm::switchTime( int newtime, bool doSet )
{
  int row;
  loadTable(newtime);
  updateEnables();
  row = B3DMIN(mCurRow[mCurTime], table->rowCount() - 1);
  mCurRow[mCurTime] = row;
  if (row >= 0) {
    
    // Note that using selections did not do any better than just selectRow
    table->selectRow(row);
    if (doSet)
      updateTopIfContinuous();
  }
  setTimeLabel();
}

// Find slicer angle item at a given row in table and current time, return item index
SlicerAngles * SlicerAngleForm::findAngles( int row, int &index )
{
  Imod *imod = App->cvi->imod;
  int i, numRows = 0;
  SlicerAngles *slanp;  
  
  // Find the angle entry at the current time that matches the given row
  for (i = 0; i < ilistSize(imod->slicerAng); i++) {
    slanp = (SlicerAngles *)ilistItem(imod->slicerAng, i);
    if (slanp->time == mCurTime) {
      if (numRows == row) {
        index = i;
        return slanp;
      }
      numRows++;
    }
  }
  return NULL;
}

// Simple function to finish undo actions depending on whether a change occurred
void SlicerAngleForm::finishOrFlushUnit( bool changed )
{
  if (changed)
    App->cvi->undo->finishUnit();
  else
    App->cvi->undo->flushUnit();
}

// External notification of new time or need to refresh
void SlicerAngleForm::newTime(bool refresh)
{
  bool continuous;
  int newtime = getTopSlicerTime(continuous);
  if (newtime < 0)
    ivwGetTime(App->cvi, &newtime);
  newtime += mTimeInc;
  if (newtime != mCurTime || refresh)
    switchTime(newtime, true);
}

// Notification that the top slicer is drawing
void SlicerAngleForm::topSlicerDrawing( float *angles, float cx, float cy, float cz, 
                                        int time, int dragging, bool continuous )
{
  SlicerAngles *slanp;
  int index;
  static int lastDragging = 0;
  
  // If the active time has changed, need to switch to that time
  if (time + mTimeInc != mCurTime)
    switchTime(time + mTimeInc, false);
  
  // If in continuous mode, check for change in the row
  if (continuous) {
    
    // But make a row if there is not one yet
    if (!table->rowCount())
      newClicked();
      
    //imodPrintStderr("updating row %d\n", mCurRow[mCurTime]);
    slanp = findAngles(mCurRow[mCurTime], index);
    if (slanp && (angles[0] != slanp->angles[0] || angles[1] != slanp->angles[1] ||
                  angles[2] != slanp->angles[2]  || cx != slanp->center.x || cy != slanp->center.y
                  || cz != slanp->center.z)) {
      if (!lastDragging)
        App->cvi->undo->modelChange();
      slanp->angles[0] = angles[0];
      slanp->angles[1] = angles[1];
      slanp->angles[2] = angles[2];
      slanp->center.x = cx;
      slanp->center.y = cy;
      slanp->center.z = cz;
      loadRow(slanp, mCurRow[mCurTime], true);
      if (!dragging)
        App->cvi->undo->finishUnit();
    } else if (lastDragging && !dragging)
      App->cvi->undo->finishUnit();
  }
  lastDragging = dragging;
}

// Close - notify slicer
void SlicerAngleForm::closeEvent( QCloseEvent *e )
{
  slicerAnglesClosing();
  e->accept();
}

// Pass on keys unless they are table navigation keys, which generate an
// event when they do not move the table selection
void SlicerAngleForm::keyPressEvent( QKeyEvent *e )
{
  int key = e->key();
  if (!(e->modifiers() & Qt::KeypadModifier) && 
      (key == Qt::Key_Left || key == Qt::Key_Right || key == Qt::Key_Up || 
       key == Qt::Key_Down || key == Qt::Key_PageUp || key == Qt::Key_PageDown
       || key == Qt::Key_Home || key == Qt::Key_End))
    return;
  if (utilCloseKey(e)) {
    close();
  } else {
    ivwControlKey(0, e);
  }
}
