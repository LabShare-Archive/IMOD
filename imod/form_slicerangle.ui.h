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

void SlicerAngleForm::init()
{
  // Determine maximum time allowed in model, and maximum time
  // Set the time increment, so all time indexes start at 1
  SlicerAngles *slanp;
  mTimeInc = 0;
  mContinuous = false;
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
  copySpin->setMaxValue(mMaxModelTime);
  renumberSpin->setMaxValue(mMaxModelTime);
  
  // Get rid of unneeded buttons
  if (mMaxModelTime == 1) {
    removeButton->hide();
    copyButton->hide();
    renumberButton->hide();
    insertButton->hide();
    renumberSpin->hide();
    copySpin->hide();
    volumeGroup->hide();
  }
  if (mMaxImageTime == 1)
    timeLabel->hide();
  mCurRow = new int[mMaxModelTime + 1];

  // Set the current time from the top slicer or fall back to display time
  mCurTime = getTopSlicerTime();
  if (mCurTime < 0)
    ivwGetTime(App->cvi, &mCurTime);
  mCurTime += mTimeInc;
  setFontDependentWidths();
  loadTable(mCurTime);
  mCurRow[mCurTime] = -1;
  
  // Set the current row
  if (table->numRows()) {
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
  int numRows = table->numRows();
  getButton->setEnabled(numRows > 0);
  setButton->setEnabled(numRows > 0);
  deleteButton->setEnabled(numRows > 0);
  renumberButton->setEnabled(renumberSpin->value() != mCurTime);
  copyButton->setEnabled(copySpin->value() != mCurTime);
}

// Slot called when font changes
void SlicerAngleForm::fontChange( const QFont &oldFont )
{
  setFontDependentWidths();
  QWidget::fontChange(oldFont);
}

// Handles width settings with fint size changes
void SlicerAngleForm::setFontDependentWidths()
{
  int width;
  bool rounded = ImodPrefs->getRoundedStyle();
  width = diaSetButtonWidth(getButton, rounded, 1.2, "Get Angles");
  getButton->setFixedWidth(width);
  deleteButton->setFixedWidth(width);
  newButton->setFixedWidth(width);
  width = diaSetButtonWidth(renumberButton, rounded, 1.2, "Renumber To");
  copyButton->setFixedWidth(width);
  width = diaSetButtonWidth(removeButton, rounded, 1.2, "Remove");
  insertButton->setFixedWidth(width);
  width = (int)(1.1 * table->fontMetrics().width("-90.00") + 0.5);
  table->setColumnWidth(0, width);
  width = (int)(1.1 * table->fontMetrics().width("-180.00") + 0.5);
  table->setColumnWidth(1, width);
  table->setColumnWidth(2, width);
  width = (int)(1.1 * table->fontMetrics().width("-9999.00") + 0.5);
  table->setColumnWidth(3, width);
  table->setColumnWidth(4, width);
  table->setColumnWidth(5, width);
  width = (int)(1.1 * table->fontMetrics().width("abcdefghijklmnop") + 0.5);
  table->setColumnWidth(6, width);
}

// Manage the time label
void SlicerAngleForm::setTimeLabel()
{
  QString str;
  str.sprintf("Time: %d (%s)", mCurTime, ivwGetTimeIndexLabel(App->cvi, mCurTime));
  timeLabel->setText(str);
}

// Get angle button
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
  loadRow(slanp, mCurRow[mCurTime]);
}

// Set angle button
void SlicerAngleForm::setAngClicked()
{
  setAngles(true);
}

// Common routine to set angles
void SlicerAngleForm::setAngles( bool draw )
{
  int time, index;
  if (getTopSlicerTime() + mTimeInc != mCurTime)
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
  mCurRow[mCurTime] = B3DMIN(table->numRows() - 1, mCurRow[mCurTime] );
  table->selectRow(mCurRow[mCurTime]);  
  updateEnables();
}

// Button to create a new row with current angles
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
  mCurRow[mCurTime] = table->numRows() - 1;
  table->selectRow(mCurRow[mCurTime]);
  updateEnables();
}

// Change in continuous update selection
void SlicerAngleForm::updateToggled( bool state )
{
  mContinuous = state;
}

////////////////
// BUTTONS TO OPERATE ON SET S OF ANGLES FOR DIFFERENT TIMES

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
  imodShowHelpPage("slicerAngles.html");    
}

// When a cell changes, process it
void SlicerAngleForm::cellChanged( int row, int col )
{
  int index;
  float val;
  QString str = table->text(row, col).stripWhiteSpace();
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
    slanp->center.x = str.toFloat();
  else if (col == 4)
    slanp->center.y = str.toFloat();
  else if (col == 5)
    slanp->center.z = str.toFloat();
  else {
    str.truncate(ANGLE_STRSIZE - 1);
    strcpy(slanp->label, str.latin1());
  }
  App->cvi->undo->finishUnit();
  setFocus();
  table->selectRow(row);
  //modPrintStderr("cell at row %d  current %d\n", row, mCurRow[mCurTime]);
  // Make it ignore the next current cell change since it is on the wrong row
  mIgnoreCurChg = true;
  if (mContinuous && mCurRow[mCurTime] == row)
    setAngles(true);
}

// When current cell changes and there is a full row selected, start a timer to draw
void SlicerAngleForm::currentChanged( int row, int col )
{
  if (mIgnoreCurChg) {
    mIgnoreCurChg = false;
    return;
  }
  //imodPrintStderr("cur changed args %d %d row %d col %d\n", row, col, table->currentRow(), table->currentColumn());
  mCurRow[mCurTime] = table->currentRow();
  if (table->isRowSelected(mCurRow[mCurTime], true)) {
    mDoSetAng = true;
    mTimerID = startTimer(10);
    //imodPrintStderr("Full row selection %d\n", table->currentRow());
  }

}

// If a cell was pressed instead of button on the left, this comes in, so cancel the draw
void SlicerAngleForm::cellPressed( int row, int col )
{
  //imodPrintStderr("cell pressed %d %d\n", row, col);
  mDoSetAng = false;
}

// When timer is over, if a cell wasn't pressed, do the angle draw 
void SlicerAngleForm::timerEvent( QTimerEvent * e )
{
  killTimer(mTimerID);
  //imodPuts("timer in");
  if (mDoSetAng && table->isRowSelected(mCurRow[mCurTime], true))
    setAngles(true);
}

// Load the whole table and set current time
void SlicerAngleForm::loadTable( int time )
{
  Imod *imod = App->cvi->imod;
  SlicerAngles *slanp;
  int i, numRows = 0;
  
  // Load all the items whose time matches
  for (i = 0; i < ilistSize(imod->slicerAng); i++) {
    slanp = (SlicerAngles *)ilistItem(imod->slicerAng, i);
    if (slanp->time != time)
      continue;
    
    // Add a row if needed
    if (numRows + 1 > table->numRows())
      table->setNumRows(numRows + 1);
    loadRow(slanp, numRows++);
  }
  
  // Get rid of extra rows
  for (i = table->numRows() - 1; i >= numRows; i--)
    table->removeRow(i);
  mCurTime = time;
}

// Load one row of the table
void SlicerAngleForm::loadRow( SlicerAngles *slanp, int row )
{
  QString str;
  str.sprintf("%.2f", slanp->angles[0]);
  table->setText(row, 0, str);
  str.sprintf("%.2f", slanp->angles[1]);
  table->setText(row, 1, str);
  str.sprintf("%.2f", slanp->angles[2]);
  table->setText(row, 2, str);
  str.sprintf("%.2f", slanp->center.x);
  table->setText(row, 3, str);
  str.sprintf("%.2f", slanp->center.y);
  table->setText(row, 4, str);
  str.sprintf("%.2f", slanp->center.z);
  table->setText(row, 5, str);
  table->setText(row, 6, QString(slanp->label));
}

// Switch the table to a new time and manage selection, enables, and label; also set
// angles if in continuous mode and doSet is true
void SlicerAngleForm::switchTime( int newtime, bool doSet )
{
  int row;
  loadTable(newtime);
  updateEnables();
  row = B3DMIN(mCurRow[mCurTime], table->numRows() - 1);
  mCurRow[mCurTime] = row;
  if (row >= 0) {
    
    // Note that using selections did not do any better than just selectRow
    table->selectRow(row);
    if (mContinuous && doSet)
      setAngles(false);
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
  int newtime = getTopSlicerTime() + mTimeInc;
  if (newtime != mCurTime || refresh)
    switchTime(newtime, true);
}

// Notification that the top slicer is drawing
void SlicerAngleForm::topSlicerDrawing( float *angles, float cx, float cy, float cz, 
                                        int time, int dragging )
{
  SlicerAngles *slanp;
  int index;
  static int lastDragging = 0;
  
  // If the active time has changed, need to switch to that time
  if (time + mTimeInc != mCurTime)
    switchTime(time + mTimeInc, false);
  
  // If in continuous mode, check for change in the row
  if (mContinuous) {
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
      loadRow(slanp, mCurRow[mCurTime]);
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

// Pass on keys
void SlicerAngleForm::keyPressEvent( QKeyEvent *e )
{
  if (e->key() == Qt::Key_Escape) {
    close();
  } else {
    ivwControlKey(0, e);
  }
}
