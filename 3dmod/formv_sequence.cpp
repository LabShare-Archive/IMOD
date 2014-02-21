/*
 *  formv_sequence.cpp - Class for movie sequence form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2012 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "formv_sequence.h"
#include "preferences.h"
#include "imod.h"
#include "imodv.h"
#include "info_cb.h"
#include "control.h"
#include "mv_input.h"

#include "dia_qtutils.h"
#include "autodoc.h"

#define MSA_SEGMENT "MovieSegment"
#define MSA_ROTATION "Rotation"
#define MSA_TRANSLATE "Translation"
#define MSA_CLIP_POINT "ClipPoint"
#define MSA_ZOOM "Zoom"
#define MSA_IMG_CEN "ImgCenter"
#define MSA_IMG_SLICES "ImgSlices"
#define MSA_IMG_TRANSP "ImgTransparency"
#define MSA_FRAMES "NumFrames"
#define MSA_VIEW "ViewNum"
#define MSA_FULL_AXIS "FullAxis"
#define MSA_NUM_CLIPS "NumClips"
#define MSA_CLIP_FLAGS "ClipFlags"
#define MSA_CLIP_NORM "ClipNormal"
#define MSA_NUM_ONOFF "NumOnOff"
#define MSA_OBJ_ONOFF "ObjectOnOff"
#define MSA_LABEL "Label"
#define MSA_IMG_SIZE "ImgSize"
#define MSA_IMG_LEVELS "ImgLevels"
#define MSA_IMG_AXIS "ImgAxis"
#define MSA_IMG_FALSE "ImageFalseColor"
#define MSA_NUMOBJ_DELTRANS "NumTransChangeObjs"
#define MSA_OBJ_DELTRANS "TransChangeObjs"
#define MSA_OBJ_TRANSP "ObjTransparency"


/*
 *  Constructs a MovieSequenceForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
MovieSequenceForm::MovieSequenceForm(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
MovieSequenceForm::~MovieSequenceForm()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void MovieSequenceForm::languageChange()
{
  retranslateUi(this);
}

// Slot called when font changes
void MovieSequenceForm::changeEvent(QEvent *e)
{
  QWidget::changeEvent(e);
  if (e->type() == QEvent::FontChange)
    setFontDependentWidths();
}

/*
 * Initialize
 */
void MovieSequenceForm::init()
{
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  mSegments = mvMovieSegmentArray();
  connect(doneButton, SIGNAL(clicked()), this, SLOT(close()));
  connect(addAfterButton, SIGNAL(clicked()), this, SLOT(addAfterClicked()));
  connect(addBeforeButton, SIGNAL(clicked()), this, SLOT(addBeforeClicked()));
  connect(replaceButton, SIGNAL(clicked()), this, SLOT(replaceClicked()));
  connect(deleteButton, SIGNAL(clicked()), this, SLOT(deleteClicked()));
  connect(setMovieButton, SIGNAL(clicked()), this, SLOT(setMovieClicked()));
  connect(setStartButton, SIGNAL(clicked()), this, SLOT(setStartClicked()));
  connect(setEndButton, SIGNAL(clicked()), this, SLOT(setEndClicked()));
  connect(runAllButton, SIGNAL(clicked()), this, SLOT(runAllClicked()));
  connect(saveButton, SIGNAL(clicked()), this, SLOT(saveClicked()));
  connect(loadButton, SIGNAL(clicked()), this, SLOT(loadClicked()));
  connect(helpButton, SIGNAL(clicked()), this, SLOT(helpClicked()));
  connect(table, SIGNAL(cellChanged(int, int)), this, SLOT(entryChanged(int,int)));
  setFontDependentWidths();
  mModified = false;

  // Change window size to give 4 rows plus header
  imod_info_input();
  adjustSize();
  QSize winSize = sizeHint();
  QSize tableSize = table->sizeHint();
  int desired = 5.6 * (table->fontMetrics().height() + 3);
  resize(winSize.width(), winSize.height() + (desired - tableSize.height()));
}

/*
 * Enable buttons based on state of system
 */
void MovieSequenceForm::updateEnables(bool movieEnabled, bool makingMovie)
{
  int size = mSegments->size();
  bool selected = size > 0 && table->currentRow() >= 0;
  addAfterButton->setEnabled(movieEnabled && (!size || table->currentRow() >= 0));
  addBeforeButton->setEnabled(movieEnabled && selected);
  replaceButton->setEnabled(movieEnabled && selected);
  deleteButton->setEnabled(selected);
  setMovieButton->setEnabled(movieEnabled && !makingMovie && selected);
  setStartButton->setEnabled(!makingMovie && selected);
  setEndButton->setEnabled(!makingMovie && selected);
  runAllButton->setEnabled(movieEnabled && !makingMovie);
  saveButton->setEnabled(size > 0);
  mMakingMovie = makingMovie;
  mMovieEnabled = movieEnabled;
}

/*
 * Common function to add ite at given index
 */
void MovieSequenceForm::addAtIndex(int index)
{
  MovieSegment segment;
  mvMovieGetSegment(segment);
  if (index >= mSegments->size())
    mSegments->push_back(segment);
  else
    mSegments->insert(mSegments->begin() + index, segment);
  loadTable();
  table->selectRow(index);
  updateEnables(mMovieEnabled, mMakingMovie);
  mModified = true;
}

/*
 * Add after current item
 */
void MovieSequenceForm::addAfterClicked()
{
  int index = B3DMAX(0, table->currentRow() + 1);
  if (!index && mSegments->size())
    return;
  addAtIndex(index);
}

/*
 * Add before current item
 */
void MovieSequenceForm::addBeforeClicked()
{
  int index = table->currentRow();
  if (index < 0 || !mSegments->size())
    return;
  addAtIndex(index);
}

/*
 * Replace current item
 */
void MovieSequenceForm::replaceClicked()
{
  MovieSegment segment;
  int index = table->currentRow();
  if (index < 0 || index >= mSegments->size())
    return;
  mvMovieGetSegment(segment);
  if (!mSegments->at(index).label.isEmpty())
    segment.label = mSegments->at(index).label;
  mSegments->at(index) = segment;
  loadRow(&segment, index, true);
  mModified = true;
}

/*
 * Delete current item
 */
void MovieSequenceForm::deleteClicked()
{
  int index = table->currentRow();
  if (index < 0 || index >= mSegments->size())
    return;
  mSegments->erase(mSegments->begin() + index);
  loadTable();
  if (index >= mSegments->size())
    index = mSegments->size() - 1;
  if (index >= 0)
    table->selectRow(index);
  updateEnables(mMovieEnabled, mMakingMovie);
  mModified = true;
}

/*
 * Set current segment as the movie parameters
 */
void MovieSequenceForm::setMovieClicked()
{
  int index = table->currentRow();
  if (index < 0 || index >= mSegments->size())
    return;
  mvMovieSetSegment(mSegments->at(index));
}

/*
 * Set the display properties to the start or end of the current segement
 */
void MovieSequenceForm::setStartOrEnd(int startEnd)
{
  int index = table->currentRow();
  if (index < 0 || index >= mSegments->size())
    return;
  mvMovieSetTerminus(startEnd, mSegments->at(index));
}

void MovieSequenceForm::setStartClicked()
{
  setStartOrEnd(IMODV_MOVIE_START_STATE);
}

void MovieSequenceForm::setEndClicked()
{
  setStartOrEnd(IMODV_MOVIE_END_STATE);
}

/*
 * Run all segments
 */
void MovieSequenceForm::runAllClicked()
{
  int i;
  for (i = 0; i < mSegments->size(); i++) {
    mvMovieSetSegment(mSegments->at(i));
    if (mvMovieMake(true))
      break;
  }
}

/*
 * Save the sequence to a file
 */
void MovieSequenceForm::saveClicked()
{
  saveSequence();
}

/*
 * Save sequence with return code of -1 for error, 1 for cancel or failure to back up
 */
int MovieSequenceForm::saveSequence()
{
  QString key, value, oneVal;
  int iseg, err, se, cl;
  char buffer[2 * MAX_OBJ_ONOFF];
  MovieSegment *segp;
  MovieTerminus *term;
  
  if (AdocNew() < 0) {
    dia_err("Failed to create a new autodoc structure");
    AdocDone();
    return -1;
  }
  for (iseg = 0; iseg < mSegments->size(); iseg++) {
    err = 1;
    segp = &(mSegments->at(iseg));
    key.sprintf("%d", iseg + 1);
    if (AdocAddSection(MSA_SEGMENT, LATIN1(key)) < 0)
      break;
    err = 2;

    // General segment items
    if (AdocSetInteger(MSA_SEGMENT, iseg, MSA_FRAMES, segp->numFrames))
      break;
    if (AdocSetInteger(MSA_SEGMENT, iseg, MSA_VIEW, segp->viewNum))
      break;
    if (AdocSetInteger(MSA_SEGMENT, iseg, MSA_FULL_AXIS, segp->fullAxis))
      break;
    if (AdocSetInteger(MSA_SEGMENT, iseg, MSA_NUM_CLIPS, segp->numClips))
      break;
    if (AdocSetInteger(MSA_SEGMENT, iseg, MSA_CLIP_FLAGS, segp->clipFlags))
      break;
    if (AdocSetInteger(MSA_SEGMENT, iseg, MSA_IMG_AXIS, segp->imgAxisFlags))
      break;

    // Segment image drawing state
    if (segp->imgAxisFlags) {
      if (AdocSetThreeIntegers(MSA_SEGMENT, iseg, MSA_IMG_SIZE, segp->imgXsize, 
                               segp->imgYsize, segp->imgZsize))
      break;
      if (AdocSetTwoIntegers(MSA_SEGMENT, iseg, MSA_IMG_LEVELS, segp->imgBlackLevel, 
                               segp->imgWhiteLevel))
      break;
      if (AdocSetInteger(MSA_SEGMENT, iseg, MSA_IMG_FALSE, segp->imgFalseColor))
        break;
    }

    // Segment clip normals
    for (cl = 0; cl < segp->numClips; cl++) {
      key.sprintf("%s%d", MSA_CLIP_NORM, cl + 1);
      if (AdocSetThreeFloats(MSA_SEGMENT, iseg, LATIN1(key), segp->clipNormal[cl].x,
                             segp->clipNormal[cl].y, segp->clipNormal[cl].z))
        break;
    }
    if (cl < segp->numClips)
      break;

    if (!segp->label.isEmpty() && 
        AdocSetKeyValue(MSA_SEGMENT, iseg, MSA_LABEL, LATIN1(segp->label)))
      break;

    // The object states
    if (AdocSetInteger(MSA_SEGMENT, iseg, MSA_NUM_ONOFF, segp->objStates.size()))
      break;
    for (cl = 0; cl < segp->objStates.size(); cl++) {
      buffer[2 * cl] = segp->objStates[cl] ? '1' : '0';
      buffer[2 * cl + 1] = ' ';
    }
    buffer[2 * cl - 1] = 0x00;
    if (AdocSetKeyValue(MSA_SEGMENT, iseg, MSA_OBJ_ONOFF, buffer))
      break;

    // Objects with trans changing
    if (AdocSetInteger(MSA_SEGMENT, iseg, MSA_NUMOBJ_DELTRANS,
                       segp->transChangeObjs.size()))
      break;
    if (segp->transChangeObjs.size()) {
      value = "";
      for (cl = 0; cl < segp->transChangeObjs.size(); cl++) {
        oneVal.sprintf(" %d", segp->transChangeObjs[cl]);
        value += oneVal;
      }
      if (AdocSetKeyValue(MSA_SEGMENT, iseg, MSA_OBJ_DELTRANS, LATIN1(value)))
        break;
    }

    // Now put out start and end data
    term = &segp->start;
    for (se = 0; se < 2; se++) {
      key.sprintf("%s%s", MSA_ROTATION, se ? "End" : "Start");
      if (AdocSetThreeFloats(MSA_SEGMENT, iseg, LATIN1(key), term->rotation.x,
                             term->rotation.y, term->rotation.z))
        break;
      key.sprintf("%s%s", MSA_TRANSLATE, se ? "End" : "Start");
      if (AdocSetThreeFloats(MSA_SEGMENT, iseg, LATIN1(key), term->translate.x,
                             term->translate.y, term->translate.z))
        break;
      key.sprintf("%s%s", MSA_ZOOM, se ? "End" : "Start");
      if (AdocSetFloat(MSA_SEGMENT, iseg, LATIN1(key), term->zoomRad))
        break;
      
      // Terminus image drawing
      if (segp->imgAxisFlags) {
        key.sprintf("%s%s", MSA_IMG_CEN, se ? "End" : "Start");
        if (AdocSetThreeIntegers(MSA_SEGMENT, iseg, LATIN1(key), term->imgXcenter, 
                               term->imgYcenter, term->imgZcenter))
          break;
        key.sprintf("%s%s", MSA_IMG_SLICES, se ? "End" : "Start");
        if (AdocSetInteger(MSA_SEGMENT, iseg, LATIN1(key), term->imgSlices))
          break;
        key.sprintf("%s%s", MSA_IMG_TRANSP, se ? "End" : "Start");
        if (AdocSetInteger(MSA_SEGMENT, iseg, LATIN1(key), term->imgTransparency))
          break;
      }

      // Terminus clip points
      for (cl = 0; cl < segp->numClips; cl++) {
        key.sprintf("%s%s%d", MSA_CLIP_POINT, se ? "End" : "Start", cl + 1);
        if (AdocSetThreeFloats(MSA_SEGMENT, iseg, LATIN1(key), term->clipPoint[cl].x,
                               term->clipPoint[cl].y, term->clipPoint[cl].z))
          break;
      }
      if (cl < segp->numClips)
        break;

      // Terminal transparency values
      if (segp->transChangeObjs.size()) {
        value = "";
        for (cl = 0; cl < segp->transChangeObjs.size(); cl++) {
          oneVal.sprintf(" %d", (int)term->objTrans[cl]);
          value += oneVal;
        }
        key.sprintf("%s%s", MSA_OBJ_TRANSP, se ? "End" : "Start");
        if (AdocSetKeyValue(MSA_SEGMENT, iseg, LATIN1(key), LATIN1(value)))
          break;
      }

      term= &segp->end;
    }
    if (se < 2)
      break;
    err = 0;
  }
  if (err) {
    dia_err(err == 1 ? "Failed to add a section to autodoc" : 
            "Failed to add a key-value pair to autodoc");
    AdocDone();
    return -1;
  }

  key = imodPlugGetSaveName(this, "Select file to save movie sequence into:");
  if (key.isEmpty()) {
    AdocDone();
    return 1;
  }
  err = AdocWrite(LATIN1(key));
  AdocDone();

  if (err)
    dia_err(err == 1 ? "Failed to back up existing copy of file" :
            "Error writing the movie sequence to file");
  mModified = err < 0;
  return err; 
}

/*
 * Load the sequence from a file
 */
void MovieSequenceForm::loadClicked()
{
  int numSeg, iseg, err, se, cl, numObj;
  char *buffer;
  int *intArray = NULL;
  MovieSegment *segp;
  MovieTerminus *term;
  QString key;

  if (mModified) {
    iseg = dia_choice("Save current movie sequence before loading a new one?",
                      "Yes", "No", "Cancel");
    if (iseg == 3)
      return;
    if (iseg == 1 && saveSequence() < 0)
      return;
  }

  key = utilOpenFileName(this, "Select movie sequence file to load", 0, NULL);
  if (key.isEmpty())
    return;
  if (AdocRead(LATIN1(key)) < 0) {
    dia_err("Error reading the movie sequence file as an autodoc");
    AdocDone();
    return;
  }

  mSegments->clear();
  numSeg = AdocGetNumberOfSections(MSA_SEGMENT);
  if (numSeg <= 0) {
    dia_err(numSeg ? "Error getting number of segments from autodoc" : 
            "No segments were found in the file");
    loadTable();
    return;
  }
  mSegments->resize(numSeg);

  for (iseg = 0; iseg < numSeg; iseg++) {
    err = 1;
    segp = &(mSegments->at(iseg));

    // General segment items
    if (AdocGetInteger(MSA_SEGMENT, iseg, MSA_FRAMES, &segp->numFrames))
      break;
    err++;
    if (AdocGetInteger(MSA_SEGMENT, iseg, MSA_VIEW, &segp->viewNum))
      break;
    err++;
    if (AdocGetInteger(MSA_SEGMENT, iseg, MSA_FULL_AXIS, &segp->fullAxis))
      break;
    err++;
    if (AdocGetInteger(MSA_SEGMENT, iseg, MSA_NUM_CLIPS, &segp->numClips))
      break;
    err++;
    if (AdocGetInteger(MSA_SEGMENT, iseg, MSA_CLIP_FLAGS, &segp->clipFlags))
      break;
    err++;
    if (AdocGetInteger(MSA_SEGMENT, iseg, MSA_IMG_AXIS, &segp->imgAxisFlags))
      break;

    // Segment image drawing state
    if (segp->imgAxisFlags) {
      if (AdocGetThreeIntegers(MSA_SEGMENT, iseg, MSA_IMG_SIZE, &segp->imgXsize, 
                               &segp->imgYsize, &segp->imgZsize))
        break;
      if (AdocGetTwoIntegers(MSA_SEGMENT, iseg, MSA_IMG_LEVELS, &segp->imgBlackLevel, 
                               &segp->imgWhiteLevel))
        break;
      if (AdocGetInteger(MSA_SEGMENT, iseg, MSA_IMG_FALSE, &segp->imgFalseColor))
        break;
    }

    // Segment clip normals
    for (cl = 0; cl < segp->numClips; cl++) {
      key.sprintf("%s%d", MSA_CLIP_NORM, cl + 1);
      if (AdocGetThreeFloats(MSA_SEGMENT, iseg, LATIN1(key), &segp->clipNormal[cl].x,
                             &segp->clipNormal[cl].y, &segp->clipNormal[cl].z))
        break;
    }
    if (cl < segp->numClips)
      break;
    err++;

    if (AdocGetString(MSA_SEGMENT, iseg, MSA_LABEL, &buffer) == 0) {
      segp->label = QString(buffer);
      free(buffer);
    }
    err++;

    // The object states
    if (AdocGetInteger(MSA_SEGMENT, iseg, MSA_NUM_ONOFF, &numObj) || !numObj)
      break;
    err++;
    segp->objStates.resize(numObj);
    if (AdocGetString(MSA_SEGMENT, iseg, MSA_OBJ_ONOFF, &buffer))
      break;
    err++;
    if (strlen(buffer) < 2 * numObj - 1) {
      free(buffer);
      break;
    }
    for (cl = 0; cl < numObj; cl++)
      segp->objStates[cl] = buffer[cl * 2] == '0' ? 0 : 1;
    free(buffer);
    err++;

    // Objects with changed transparency
    if (AdocGetInteger(MSA_SEGMENT, iseg, MSA_NUMOBJ_DELTRANS, &numObj))
      break;
    err++;
    if (numObj > 0) {
      intArray = B3DMALLOC(int, numObj);
      if (!intArray)
        break;
      if (AdocGetIntegerArray(MSA_SEGMENT, iseg, MSA_OBJ_DELTRANS, intArray, &numObj,
                              numObj))
        break;
      segp->transChangeObjs.resize(numObj);
      for (cl = 0; cl < numObj; cl++)
        segp->transChangeObjs[cl] = intArray[cl];
    }
    err++;

    // Now start and end data
    term = &segp->start;
    for (se = 0; se < 2; se++) {
      key.sprintf("%s%s", MSA_ROTATION, se ? "End" : "Start");
      if (AdocGetThreeFloats(MSA_SEGMENT, iseg, LATIN1(key), &term->rotation.x,
                             &term->rotation.y, &term->rotation.z))
        break;
    err++;
      key.sprintf("%s%s", MSA_TRANSLATE, se ? "End" : "Start");
      if (AdocGetThreeFloats(MSA_SEGMENT, iseg, LATIN1(key), &term->translate.x, 
                             &term->translate.y, &term->translate.z))
        break;
    err++;
      key.sprintf("%s%s", MSA_ZOOM, se ? "End" : "Start");
      if (AdocGetFloat(MSA_SEGMENT, iseg, LATIN1(key), &term->zoomRad))
        break;
      
      // Terminus image drawing
      if (segp->imgAxisFlags) {
        key.sprintf("%s%s", MSA_IMG_CEN, se ? "End" : "Start");
        if (AdocGetThreeIntegers(MSA_SEGMENT, iseg, LATIN1(key), &term->imgXcenter, 
                               &term->imgYcenter, &term->imgZcenter))
          break;
        key.sprintf("%s%s", MSA_IMG_SLICES, se ? "End" : "Start");
        if (AdocGetInteger(MSA_SEGMENT, iseg, LATIN1(key), &term->imgSlices))
          break;
        key.sprintf("%s%s", MSA_IMG_TRANSP, se ? "End" : "Start");
        if (AdocGetInteger(MSA_SEGMENT, iseg, LATIN1(key), &term->imgTransparency))
          break;
      }

      // Terminus clip points
      for (cl = 0; cl < segp->numClips; cl++) {
        key.sprintf("%s%s%d", MSA_CLIP_POINT, se ? "End" : "Start", cl + 1);
        if (AdocGetThreeFloats(MSA_SEGMENT, iseg, LATIN1(key), &term->clipPoint[cl].x,
                               &term->clipPoint[cl].y, &term->clipPoint[cl].z))
          break;
      }
      if (cl < segp->numClips)
        break;
      err++;
      
      // Terminus transparencies
      if (numObj > 0) {
        key.sprintf("%s%s", MSA_OBJ_TRANSP, se ? "End" : "Start");
        if (AdocGetIntegerArray(MSA_SEGMENT, iseg, LATIN1(key), intArray, &numObj, 
                                 numObj))
          break;
        for (cl = 0; cl < numObj; cl++)
          term->objTrans[cl] = (unsigned char)intArray[cl];
      }

      term= &segp->end;
    }
    if (se < 2)
      break;
    err = 0;
  }
  B3DFREE(intArray);

  if (err) {
    imodPrintStderr("err %d\n", err);
    dia_err("The movie sequence file could not be converted properly or was missing "
            "expected data");
    mSegments->clear();
  }
  loadTable();
  table->selectRow(0);
  updateEnables(mMovieEnabled, mMakingMovie);
  mModified = false;
}

void MovieSequenceForm::helpClicked()
{
  imodShowHelpPage("movieSequence.html#TOP");
}

/*
 * Fill the table with the segments
 */
void MovieSequenceForm::loadTable()
{
  int i, j, row;
  MovieSegment *segmentp;
  
  table->blockSignals(true);
  for (row = 0; row < mSegments->size(); row++) {

    // Insert row if needed
    if (row >= table->rowCount()) {
      table->insertRow(row);
      table->setRowHeight(row, table->fontMetrics().height() + 3);
      for (j = 0; j < 3; j++) {
        QTableWidgetItem *item = new QTableWidgetItem();
        if (j < 2)
          item->setTextAlignment(Qt::AlignRight);
        table->setItem(row, j, item);
      }
    }
    
    // Fill the items in the row
    segmentp = &mSegments->at(row);
    loadRow(segmentp, row, false);
  }

  // remove any extra rows
  for (i = table->rowCount() - 1; i >= row; i--)
    table->removeRow(i);
  table->blockSignals(false);
}

/*
 * Refill one row of the table with a segment
 */
void MovieSequenceForm::loadRow(MovieSegment *segmentp, int row, bool block)
{
  QString str;
  
  if (block)
    table->blockSignals(true);
  str.sprintf("%d", segmentp->numFrames);
  table->item(row, 0)->setText(str);
  str.sprintf("%d", segmentp->viewNum);
  table->item(row, 1)->setText(str);
  table->item(row, 2)->setText(segmentp->label);
  if (block)
    table->blockSignals(false);
}

/*
 * Manage a change in a cell
 */
void MovieSequenceForm::entryChanged(int row, int column)
{
  int val;
  MovieSegment *segmentp;
  QString str = table->item(row, column)->text().trimmed();
  if (row >= mSegments->size())
    return;
  segmentp = &mSegments->at(row);
  if (column > 1) {
    segmentp->label = str;
  } else {
    val = atoi(LATIN1(str));
    if (!column)
      segmentp->numFrames = B3DMAX(2, val);
    else
      segmentp->viewNum = B3DMAX(1, B3DMIN(Imodv->imod->viewsize - 1, val));
  }
  loadRow(segmentp, row, true);
  mModified = true;
}

/*
 * Take care of button widths
 */
void MovieSequenceForm::setFontDependentWidths()
{
  int width, extra = 8;
  QString str;
  bool rounded = ImodPrefs->getRoundedStyle();
  width = diaSetButtonWidth(addBeforeButton, rounded, 1.2, "Add Before");
  addAfterButton->setFixedWidth(width);
  replaceButton->setFixedWidth(width);
  deleteButton->setFixedWidth(width);
  width = diaSetButtonWidth(setMovieButton, rounded, 1.2, "Use as Movie");
  setStartButton->setFixedWidth(width);
  setEndButton->setFixedWidth(width);
  width = diaSetButtonWidth(runAllButton, rounded, 1.2, "Run All");
  saveButton->setFixedWidth(width);
  loadButton->setFixedWidth(width);
  doneButton->setFixedWidth(width);
  helpButton->setFixedWidth(width);
  width = 1.2 * table->fontMetrics().width("# Frames") + extra;
  table->setColumnWidth(0, width);
  width = 1.2 * table->fontMetrics().width("View #") + extra;
  table->setColumnWidth(1, width);
  width = table->fontMetrics().width("3 objects with image half-spin") + extra;
  table->setColumnWidth(2, width);
}

/*
 * Close window: first ask whether to save; cancel if that fails
 */
void MovieSequenceForm::closeEvent( QCloseEvent *e )
{
  int choice;
  if (mModified) {
    choice = dia_choice("Save movie sequence to file before closing?", "Yes", "No",
                        "Cancel");
    if (choice == 3 || (choice == 1 && saveSequence() < 0)) {
      e->ignore();
      return;
    }
  }
  mvMovieSequenceClosing();
  mSegments->clear();
  e->accept();
}

/*
 * Pass on keys unless they are table navigation keys, which generate an
 * event when they do not move the table selection
 */
void MovieSequenceForm::keyPressEvent( QKeyEvent *e )
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
    imodvKeyPress(e);
  }
}

void MovieSequenceForm::keyReleaseEvent( QKeyEvent * e )
{
  imodvKeyRelease(e);
}
