/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/

void imodvMovieForm::init()
{
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
  if (Imodv->standalone) {
    for (int i = 7; i < 10; i++) {
      startEdits[i]->hide();
      endEdits[i]->hide();
      bigLayout->remove(startEdits[i]);
      bigLayout->remove(endEdits[i]);
    }
    // removing from layout was good but not available in Qt 3.0
    xSliceLabel->hide();
    ySliceLabel->hide();
    zSliceLabel->hide();
    xSliceLabel->setMaximumHeight(1);
    ySliceLabel->setMaximumHeight(1);
    zSliceLabel->setMaximumHeight(1);
  }
  adjustSize();
}  

void imodvMovieForm::fullXPressed()
{
  imodvMovieFullAxis(IMODV_MOVIE_FULLAXIS_X);
}

void imodvMovieForm::fullYPressed()
{
  imodvMovieFullAxis(IMODV_MOVIE_FULLAXIS_Y);
}

void imodvMovieForm::setStartPressed()
{
  imodvMovieSetStart();
}

void imodvMovieForm::setEndPressed()
{
  imodvMovieSetEnd();
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
}

// Respond to botton action buttons
void imodvMovieForm::closePressed()
{
  imodvMovieQuit();
}

void imodvMovieForm::makePressed()
{
  imodvMovieMake();
}

void imodvMovieForm::stopPressed()
{
  imodvMovieStop();
}

void imodvMovieForm::helpPressed()
{
  imodvMovieHelp();
}

// Read out the start and end edit boxes
void imodvMovieForm::readStartEnd( int item, float &startVal, float &endVal )
{
  mStr = startEdits[item]->text();
  startVal = atof(mStr.latin1());
  mStr = endEdits[item]->text();
  endVal = atof(mStr.latin1());
}

// Set the contents of start or end edit boxes
void imodvMovieForm::setStart( int item, float value )
{
  mStr.sprintf("%g", value);
  startEdits[item]->setText(mStr);
}

void imodvMovieForm::setEnd( int item, float value )
{
  mStr.sprintf("%g", value);
  endEdits[item]->setText(mStr);
}

// Initialize the states of the buttons.  Try to get away with not blocking signals
void imodvMovieForm::setButtonStates( bool longWay, bool reverse, int movieMont,
				      int rgbTiff, bool writeFiles )
{
  longWayBox->setChecked(longWay);
  reverseBox->setChecked(reverse);
  writeBox->setChecked(writeFiles);
  makeGroup->setButton(movieMont);
  mRgbTiff = rgbTiff;
  manageSensitivities(movieMont);
  mLongWay = longWay;
  mReverse = reverse;
  mMovieMont = movieMont;
  mWriteFiles = writeFiles;
}

// Get the button states back
void imodvMovieForm::getButtonStates( int &longWay, int &reverse, int &movieMont,
				      int &rgbTiff, int &writeFiles )
{
  longWay = mLongWay ? 1 : 0;
  reverse = mReverse ? 1 : 0;
  movieMont = mMovieMont;
  rgbTiff = mRgbTiff;
  writeFiles = mWriteFiles ? 1 : 0;
}

// Get the values in the boxes not in the start-end lists
void imodvMovieForm::getFrameBoxes( int &nMovieFrames, int &nMontFrames)
{
  mStr = framesEdit->text();
  nMovieFrames = atoi(mStr.latin1());
  nMontFrames = montageFramesBox->value();
}

// Set the boxes that are not in the start-end lists
void imodvMovieForm::setFrameBoxes( int nMovieFrames, int nMontFrames)
{
  mStr.sprintf("%d", nMovieFrames);
  framesEdit->setText(mStr);
  montageFramesBox->setValue(nMontFrames);
}

// Enable the movie or montage part of the form
void imodvMovieForm::manageSensitivities( int movieMont )
{
  // It wouldn't take a for loop for some reason!
  int i = 0;
  bool enable = !movieMont;
  while (i < 10) {
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
  tiffRadioButton->setEnabled(!movieMont);
  rgbRadioButton->setEnabled(!movieMont);
  writeGroup->setEnabled(!movieMont);
  writeGroup->blockSignals(true);
  writeGroup->setButton(movieMont ? 1 : mRgbTiff);
  writeGroup->blockSignals(false);
}

void imodvMovieForm::closeEvent( QCloseEvent * e )
{
  imodvMovieClosing();
  e->accept();
}

// Send key press and release on to imodv_input, quit on escape
void imodvMovieForm::keyPressEvent( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    imodvMovieQuit();
  else
    imodvKeyPress(e);
}

void imodvMovieForm::keyReleaseEvent( QKeyEvent * e )
{
  imodvKeyRelease(e);
}
