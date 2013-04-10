/*
 *  formv_movie.cpp - Class for model movie dialog form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "formv_movie.h"

#include <qvariant.h>
#include <stdlib.h>
#include <qimage.h>
#include <qpixmap.h>

//Added by qt3to4:
#include <QCloseEvent>
#include <QKeyEvent>

#include "mv_input.h"
#include "mv_movie.h"
#include "imod.h"
#include "imodv.h"
#include "preferences.h"
#include "dia_qtutils.h"

/*
 *  Constructs a imodvMovieForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
imodvMovieForm::imodvMovieForm(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
imodvMovieForm::~imodvMovieForm()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void imodvMovieForm::languageChange()
{
  retranslateUi(this);
}

// Layout notes: set spacing to 2 for the big grid to conserve space

void imodvMovieForm::init()
{
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  startEdits[0] = xRotStart;
  startEdits[1] = yRotStart;
  startEdits[2] = zRotStart;
  startEdits[3] = xTransStart;
  startEdits[4] = yTransStart;
  startEdits[5] = zTransStart;
  startEdits[6] = zoomStart;
  startEdits[7] = xSliceStart;
  startEdits[8] = ySliceStart;
  startEdits[9] = zSliceStart;
  startEdits[10] = transpStart;
  startEdits[11] = thickStart;
  endEdits[0] = xRotEnd;
  endEdits[1] = yRotEnd;
  endEdits[2] = zRotEnd;
  endEdits[3] = xTransEnd;
  endEdits[4] = yTransEnd;
  endEdits[5] = zTransEnd;
  endEdits[6] = zoomEnd;
  endEdits[7] = xSliceEnd;
  endEdits[8] = ySliceEnd;
  endEdits[9] = zSliceEnd;
  endEdits[10] = transpEnd;
  endEdits[11] = thickEnd;
  if (Imodv->standalone) {
    
    // A hide alone was not good enough somewhere (Windows?)
    // removing from layout was good but not available in Qt 3.0, so set height instead
    for (int i = 7; i < 12; i++) {
      startEdits[i]->hide();
      endEdits[i]->hide();
      startEdits[i]->setFixedHeight(1);
      endEdits[i]->setFixedHeight(1);
    }
    xSliceLabel->hide();
    ySliceLabel->hide();
    zSliceLabel->hide();
    transpLabel->hide();
    thickLabel->hide();
    xSliceLabel->setFixedHeight(1);
    ySliceLabel->setFixedHeight(1);
    zSliceLabel->setFixedHeight(1);
    transpLabel->setFixedHeight(1);
    thickLabel->setFixedHeight(1);
  }

  makeGroup = new QButtonGroup(this);
  makeGroup->addButton(movieRadioButton, 0);
  makeGroup->addButton(montageRadioButton, 1);
  connect(makeGroup, SIGNAL(buttonClicked(int)), this, 
          SLOT(movieMontSelected(int)));
  connect(makeGroup, SIGNAL(buttonClicked(int)), this, 
          SLOT(manageSensitivities(int)));
  writeGroup = new QButtonGroup(this);
  writeGroup->addButton(tiffRadioButton, 0);
  writeGroup->addButton(rgbRadioButton, 1);
  writeGroup->addButton(pngRadioButton, 2);
  connect(writeGroup, SIGNAL(buttonClicked(int)), this, 
          SLOT(rgbTiffSelected(int)));
  connect(fpsSpinBox, SIGNAL(valueChanged(int)), this, SLOT(fpsChanged(int)));
  connect(sequenceButton, SIGNAL(clicked()), this, SLOT(sequenceClicked()));
  adjustSize();
  setNonTifLabel();
}

void imodvMovieForm::setNonTifLabel()
{
  QString str;
  rgbRadioButton->setText(ImodPrefs->snapFormat() + "s");
  str = ImodPrefs->snapFormat2();
  pngRadioButton->setEnabled(!str.isEmpty());
    
  // If no second format, make sure selection is in legal range
  if (str.isEmpty()) {
    if (mRgbTiff > 1) {
      diaSetGroup(writeGroup, 1);
      mRgbTiff = 1;
    }
  } else
    pngRadioButton->setText(str + "s");
}

void imodvMovieForm::fullXPressed()
{
  mvMovieFullAxis(IMODV_MOVIE_FULLAXIS_X);
}

void imodvMovieForm::fullYPressed()
{
  mvMovieFullAxis(IMODV_MOVIE_FULLAXIS_Y);
}

void imodvMovieForm::setStartPressed()
{
  mvMovieSetStart();
}

void imodvMovieForm::setEndPressed()
{
  mvMovieSetEnd();
}

// Record the state of toggle buttons
void imodvMovieForm::reverseToggled( bool state )
{
  mReverse = state;
}

void imodvMovieForm::longWayToggled( bool state )
{
  mLongWay = state;
}

void imodvMovieForm::movieMontSelected( int item )
{
  mMovieMont = item;
}

void imodvMovieForm::rgbTiffSelected( int item )
{
  mRgbTiff = item;
}

void imodvMovieForm::writeToggled( bool state )
{
  mWriteFiles = state;
  fpsSpinBox->setEnabled(!state);
  fpsLabel->setEnabled(!state);
}

void imodvMovieForm::fpsChanged(int value)
{
  mFPS = value;
}

void imodvMovieForm::sequenceClicked()
{
  mvMovieSequenceDialog(Imodv, 1);
}

// Respond to botton action buttons
void imodvMovieForm::closePressed()
{
  mvMovieQuit();
}

void imodvMovieForm::makePressed()
{
  mvMovieMake(false);
}

void imodvMovieForm::stopPressed()
{
  mvMovieStop();
}

void imodvMovieForm::helpPressed()
{
  mvMovieHelp();
}

// Read out the start and end edit boxes
void imodvMovieForm::readStartEnd( int item, float &startVal, float &endVal )
{
  mStr = startEdits[item]->text();
  startVal = atof(LATIN1(mStr));
  mStr = endEdits[item]->text();
  endVal = atof(LATIN1(mStr));
}

// Set the contents of start or end edit boxes
void imodvMovieForm::setStart( int item, float value )
{
  mStr =QString("%1").arg((double)value, 0, 'g', 4);
  startEdits[item]->setText(mStr);
}

void imodvMovieForm::setEnd( int item, float value )
{
  mStr =QString("%1").arg((double)value, 0, 'g', 4);
  endEdits[item]->setText(mStr);
}

// Initialize the states of the buttons.  Try to get away with not blocking signals
void imodvMovieForm::setButtonStates( bool longWay, bool reverse, int movieMont,
                                      int rgbTiff, bool writeFiles, int fps)
{
  longWayBox->setChecked(longWay);
  reverseBox->setChecked(reverse);
  writeBox->setChecked(writeFiles);
  diaSetGroup(makeGroup, movieMont);
  diaSetSpinBox(fpsSpinBox, fps);
  fpsSpinBox->setEnabled(!writeFiles);
  fpsLabel->setEnabled(!writeFiles);
  mRgbTiff = rgbTiff;
  if (ImodPrefs->snapFormat2().isEmpty() && rgbTiff > 1)
    mRgbTiff = 1;
  diaSetGroup(writeGroup, mRgbTiff);
  manageSensitivities(movieMont);
  mLongWay = longWay;
  mReverse = reverse;
  mMovieMont = movieMont;
  mWriteFiles = writeFiles;
  mFPS = fps;
}

// Get the button states back
void imodvMovieForm::getButtonStates( int &longWay, int &reverse, int &movieMont,
                                      int &rgbTiff, int &writeFiles, int &fps)
{
  longWay = mLongWay ? 1 : 0;
  reverse = mReverse ? 1 : 0;
  movieMont = mMovieMont;
  rgbTiff = mRgbTiff;
  writeFiles = mWriteFiles ? 1 : 0;
  fps = mFPS;
}

// Get the values in the boxes not in the start-end lists
void imodvMovieForm::getFrameBoxes( int &nMovieFrames, int &nMontFrames)
{
  mStr = framesEdit->text();
  nMovieFrames = atoi(LATIN1(mStr));
  nMontFrames = montageFramesBox->value();
}

// Set the boxes that are not in the start-end lists
void imodvMovieForm::setFrameBoxes( int nMovieFrames, int nMontFrames)
{
  mStr.sprintf("%d", nMovieFrames);
  framesEdit->setText(mStr);
  montageFramesBox->setValue(nMontFrames);
}

// Manage the Sequence button
void imodvMovieForm::sequenceOpen(bool state)
{
  sequenceButton->setEnabled(!state);
}

// Enable the movie or montage part of the form
void imodvMovieForm::manageSensitivities( int movieMont )
{
  // It wouldn't take a for loop for some reason!
  int i = 0;
  bool enable = !movieMont;
  while (i < 12) {
    startEdits[i]->setEnabled(enable);
    endEdits[i++]->setEnabled(enable);
  }
  framesEdit->setEnabled(!movieMont);
  fullXButton->setEnabled(!movieMont);
  fullYButton->setEnabled(!movieMont);
  setStartButton->setEnabled(!movieMont);
  setEndButton->setEnabled(!movieMont);
  longWayBox->setEnabled(!movieMont);
  reverseBox->setEnabled(!movieMont);
  montageFramesBox->setEnabled(movieMont);
  writeBox->setEnabled(!movieMont);
}

void imodvMovieForm::closeEvent( QCloseEvent * e )
{
  mvMovieClosing();
  e->accept();
}

// Send key press and release on to imodv_input, quit on escape
void imodvMovieForm::keyPressEvent( QKeyEvent * e )
{
  if (utilCloseKey(e))
    mvMovieQuit();
  else
    imodvKeyPress(e);
}

void imodvMovieForm::keyReleaseEvent( QKeyEvent * e )
{
  imodvKeyRelease(e);
}

