/*  IMOD VERSION 2.7.9
 *
 *  colorselector.cpp       Implementation of color selector class
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional         *
 *   Electron Microscopy of Cells ("BL3DEMC") and the Regents of the         *
 *   University of Colorado.                                                 *
 *                                                                           *
 *   BL3DEMC reserves the exclusive rights of preparing derivative works,    *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional EM of Cells.               *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.1.2.1  2003/01/26 20:35:46  mast
adding as library file

Revision 1.1.2.6  2003/01/23 19:55:42  mast
switch from button pressed to clicked

Revision 1.1.2.5  2003/01/18 00:58:37  mast
add tooltips to dialogframe call

Revision 1.1.2.4  2002/12/30 06:53:00  mast
eliminate unused variables

Revision 1.1.2.3  2002/12/30 06:36:21  mast
parameterizing the hot slider

Revision 1.1.2.2  2002/12/29 04:12:58  mast
Recoded to inherit dialog_frame

Revision 1.1.2.1  2002/12/27 01:19:47  mast
Initial creation

*/

/* This class provides a color selector with a smaple color panel, and three
 * sliders for adjusting red, green, and blue.  It manages the color of the
 * panel continuously during changes, and emits a signal for a new color
 * if the slider is clicked.  It will also emit signals during a drag if 
 * hotFlag is nonzero; if the key given by hotKey is up when hotFlag is 1;
 * or when it is down when hotFlag is -1.
 * It also emits signals when done is pressed, when it is closing, and 
 * when keys are pressed or released
 */

#include <qframe.h>
#include <qlayout.h>
#include <qlabel.h>
#include <qcolordialog.h>
#include "multislider.h"
#include "colorselector.h"

#define NO_HOT_SLIDER 0
#define HOT_SLIDER_KEYUP 1
#define HOT_SLIDER_KEYDOWN -1

static char *sliderLabels[] = {"Red", "Green", "Blue"};
static char *buttonLabels[] = {"Done", "Restore", "Qt Selector"};
static char *buttonTips[] = 
  {"Close color selector",
   "Restore to starting color when selector was opened",
   "Select color with Qt color selector box"};

ColorSelector::ColorSelector(QWidget *parent, QString label, int red,
                             int green, int blue, int hotFlag, int hotKey,
                             const char *name, WFlags fl)
  : DialogFrame(parent, 3, buttonLabels, buttonTips, false, "test", "test2",
		name, fl)
{
  QString str;

  mOriginalRGB[0] = mCurrentRGB[0] = red;
  mOriginalRGB[1] = mCurrentRGB[1] = green;
  mOriginalRGB[2] = mCurrentRGB[2] = blue;
  mCtrlPressed = false;
  mHotKey = hotKey;
  mHotFlag = hotFlag;

  // Get the top label
  QLabel *topLabel = new QLabel(label, this);
  mLayout->addWidget(topLabel);
  
  mColorBox = new QFrame(this);
  mColorBox->setFrameStyle(QFrame::Plain);
  mColorBox->setFixedHeight(50);
  mLayout->addWidget(mColorBox);

  // Get the sliders, connect them and initialize them to current color
  mSliders = new MultiSlider(this, 3, sliderLabels);
  mLayout->addLayout(mSliders->getLayout());
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this, 
          SLOT(sliderChanged(int, int, bool)));

  // Connect them: have to connect to release of Qt selector because the modal
  // box keeps the button from coming back up (maybe mixed X problem only)
  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));

  imposeColor(true, false);
}

ColorSelector::~ColorSelector()
{
}

void ColorSelector::buttonPressed(int which)
{
  if (which == 0)
    donePressed();
  else if (which == 1)
    restorePressed();
  else if (which == 2)
    qtSelectorPressed();
}

void ColorSelector::donePressed()
{
  emit done();
}

// Restore: restore the original color and send signal
void ColorSelector::restorePressed()
{
  for (int i = 0; i < 3; i++)
    mCurrentRGB[i] = mOriginalRGB[i];
  imposeColor(true, true);
}

// Open color selector, and take color if it is valid
void ColorSelector::qtSelectorPressed()
{
  QColor retColor = QColorDialog::getColor(QColor(mCurrentRGB[0],
                                                  mCurrentRGB[1],
                                                  mCurrentRGB[2]), this);
  if (!retColor.isValid())
    return;
  retColor.rgb(&mCurrentRGB[0], &mCurrentRGB[1], &mCurrentRGB[2]);
  imposeColor(true, true);
}

// Slider changed: change the current value, update color box,
// and emit new color if not dragging or ctrl pressed
void ColorSelector::sliderChanged(int which, int value, bool dragging)
{
  mCurrentRGB[which] = value;
  imposeColor(false, 
	      !dragging || (mHotFlag == HOT_SLIDER_KEYDOWN && mCtrlPressed) ||
	      (mHotFlag == HOT_SLIDER_KEYUP && !mCtrlPressed));
}

// Act on a new color
void ColorSelector::imposeColor(bool setSliders, bool emitSignal)
{
  if (setSliders)
    for (int i = 0; i < 3; i++)
      mSliders->setValue(i, mCurrentRGB[i]);
  mColorBox->setPaletteBackgroundColor(QColor(mCurrentRGB[0], mCurrentRGB[1],
                                              mCurrentRGB[2]));
  if (emitSignal)
    emit newColor(mCurrentRGB[0], mCurrentRGB[1], mCurrentRGB[2]);
}

void ColorSelector::closeEvent ( QCloseEvent * e )
{
  emit closing();
  e->accept();
}

// watch for ctrl key; emit the key event to pass it on
void ColorSelector::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape) {
    emit done();
  } else {
    
    if (mHotFlag != NO_HOT_SLIDER && e->key() == mHotKey) {
      mCtrlPressed = true;
      grabKeyboard();
    }
    emit (keyPress(e));
  }

}

void ColorSelector::keyReleaseEvent ( QKeyEvent * e )
{
  if (e->key() == mHotKey) {
    mCtrlPressed = false;
    releaseKeyboard();
  }
  emit (keyRelease(e));
}

