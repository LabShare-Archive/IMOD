/*
 *  multislider.cpp     Implementation of class for multiple horizontal sliders
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 *  $Id$
 *  Log at end of file
 */

#include <math.h>
#include <qsignalmapper.h>
#include <qslider.h>
#include <qlayout.h>
#include <qlabel.h>
#include "multislider.h"

#define NON_VALUE -99999999
#define MAX_DECIMALS 6

static char *deciFormats[MAX_DECIMALS + 1] = {"%d", "%.1f", "%.2f", "%.3f",
					      "%.4f", "%.5f", "%.6f"};

/*!
 * This class provides an arbitrary number of horizontal sliders, each with
 * a label and an integer numeric output that is managed as the slider is
 * dragged.  The number of sliders is set in [numSliders], their text labels 
 * in [titles].  Overall minimal and maximum values and number of decimal
 * places can be set with [minVal] (default 0), [maxVal] (default 255), and
 * [decimals] (default 0).  Set [horizontal] flag to true to have the sliders
 * arranged in a QHBoxLayout instead of a QVBoxLayout.  The layout can be
 * obtained with:
 * ^   QBoxLayout *getLayout();
 * ^ As a slider value changes, the class emits a signal:
 * ^   void sliderChanged(int slider, int value, bool dragging);
 * ^ with [slider] equal to the slider number, [value] with the new integer
 * value, and [dragging] true if the slider is being dragged.
 */

MultiSlider::MultiSlider(QWidget *parent, int numSliders, char *titles[], 
                         int minVal, int maxVal, int decimals, bool horizontal)
{
  mHorizontal = horizontal;
  QBoxLayout *layOuter;
  QHBoxLayout *layInner;
  QString str;
  QLabel *titleLabel;
  int i;
  // Get arrays for sliders, labels, and pressed flags
  mSliders = new QSlider* [numSliders];
  mPressed = new bool[numSliders];
  mLabels = new QLabel* [numSliders];
  mTitleLabels = new QLabel* [numSliders];
  mDecimals = new int [numSliders];
  mNumSliders = numSliders;
  QBoxLayout::Direction dir = mHorizontal ? QBoxLayout::LeftToRight :
                              QBoxLayout::TopToBottom;
  int spacing = mHorizontal ? 2 : 0;
  mBigLayout = new QBoxLayout(NULL, dir, 0, 10, "multislider big");

  // Get signal mappers and connect them to slots
  QSignalMapper *activeMapper = new QSignalMapper(parent);
  QSignalMapper *pressedMapper = new QSignalMapper(parent);
  QSignalMapper *releasedMapper = new QSignalMapper(parent);
  connect(activeMapper, SIGNAL(mapped(int)), this, SLOT(sliderActive(int)));
  connect(pressedMapper, SIGNAL(mapped(int)), this, SLOT(sliderPressed(int)));
  connect(releasedMapper, SIGNAL(mapped(int)), this, 
          SLOT(sliderReleased(int)));
  
  // Build the sliders one by one
  for (i = 0; i < numSliders; i++) {

    // Make outer layout and slider, add slider to layout
    layOuter = new QBoxLayout(0, dir, spacing, -1);
    str = titles[i];
    mTitleLabels[i] = new QLabel(str, parent);
    mTitleLabels[i]->setAlignment(Qt::AlignTop | Qt::AlignLeft);
    if (horizontal) {
      layOuter->addWidget(mTitleLabels[i]);
    }
    mSliders[i] = new QSlider(minVal, maxVal, 1, minVal, Qt::Horizontal,
                              parent);
    mSliders[i]->setFocusPolicy(QSlider::NoFocus);
    layOuter->addWidget(mSliders[i]);
    
    if (!horizontal) {
      // Make inner layout, title, and value label
      layInner = new QHBoxLayout(0, 0, 6);
      layInner->addWidget(mTitleLabels[i]);
      mLabels[i] = new QLabel("0", parent);
      mLabels[i]->setAlignment(Qt::AlignTop | Qt::AlignRight);
      layInner->addWidget(mLabels[i]);
      layOuter->addLayout(layInner);
      mDecimals[i] = decimals;
    }
    
    mBigLayout->addLayout(layOuter);
    
    // Connect to the mappers and slot
    connect(mSliders[i], SIGNAL(valueChanged(int)), this, 
            SLOT(valueChanged(int)));
    activeMapper->setMapping(mSliders[i], i);
    pressedMapper->setMapping(mSliders[i], i);
    releasedMapper->setMapping(mSliders[i], i);
    connect(mSliders[i], SIGNAL(valueChanged(int)), activeMapper, SLOT(map()));
    connect(mSliders[i], SIGNAL(sliderPressed()), pressedMapper, SLOT(map()));
    connect(mSliders[i], SIGNAL(sliderReleased()), releasedMapper,
            SLOT(map()));

    mPressed[i] = false;
  }

  mNewValue = NON_VALUE;
  mActiveSlider = -1;
}

MultiSlider::~MultiSlider()
{
  delete [] mPressed;
  delete [] mSliders;
  delete [] mLabels;
  delete [] mTitleLabels;
  delete [] mDecimals;
}

/*! 
 * Sets the number of decimals to display for the given slider number to 
 * [decimals].
 */
void MultiSlider::setDecimals(int slider, int decimals)
{
  /* fprintf(stderr, "setDecimals slider %d decimals %d mHorizontal %d\n", slider,
          decimals, mHorizontal); */
  if (mHorizontal) {
    return;
  }
  if (decimals < 0)
    decimals = 0;
  if (decimals > MAX_DECIMALS)
    decimals = MAX_DECIMALS;
  mDecimals[slider] = decimals;
}

/*!
 * Sets the value of the given slider to [value] with signals blocked.
 */
void MultiSlider::setValue(int slider, int value)
{
  if (slider >=0 && slider < mNumSliders) {
    mSliders[slider]->blockSignals(true);
    mSliders[slider]->setValue(value);
    mSliders[slider]->blockSignals(false);
    displayValue(slider, value);
  }    
}

/*!
 * Sets the range of the given slider to [minVal], [maxVal] with signals
 * blocked.
 */
void MultiSlider::setRange(int slider, int minVal, int maxVal)
{
  if (slider >=0 && slider < mNumSliders) {
    mSliders[slider]->blockSignals(true);
    mSliders[slider]->setRange(minVal, maxVal);
    mSliders[slider]->blockSignals(false);
  }
}

/*!
 * Returns a pointer to the given slider, or NULL if the value is out of range.
 */
QSlider *MultiSlider::getSlider(int slider)
{
  if (slider >=0 && slider < mNumSliders)
    return mSliders[slider];
  return NULL;
}

/*!
 * Sets the enable state of given slider and its associated text fields to 
 * [enabled].
 */
void MultiSlider::setEnabled(int slider, bool enabled)
{
  if (slider >=0 && slider < mNumSliders) {
    mSliders[slider]->setEnabled(enabled);
    mLabels[slider]->setEnabled(enabled);
    mTitleLabels[slider]->setEnabled(enabled);
  }
}


// Process a changed value if both signals are in
void MultiSlider::processChange()
{
  if (mNewValue == NON_VALUE || mActiveSlider < 0)
    return;

  // Namely, display it, emit a signal, and reset the signals
  displayValue(mActiveSlider, mNewValue);
  emit sliderChanged(mActiveSlider, mNewValue, mPressed[mActiveSlider]);
  mNewValue = NON_VALUE;
  mActiveSlider = -1;
}

// Display the slider value
void MultiSlider::displayValue(int slider, int value)
{
  /* fprintf(stderr, "displayValue slider %d value %d mHorizontal %d\n", slider,
          value, mHorizontal); */
  if (mHorizontal) {
    return;
  }
  QString str;
  int dec = mDecimals[slider];
  if (dec)
    str.sprintf(deciFormats[dec], value / pow(10., (double)dec));
  else
    str.sprintf("%d", value);
  mLabels[slider]->setText(str);
}

// When new value comes in directly , record and try to process
void MultiSlider::valueChanged(int value)
{
  mNewValue = value;
  processChange();
}

// When signal about which slider has new value comes in, record and process
void MultiSlider::sliderActive(int which)
{
  mActiveSlider = which;
  processChange();
}

// When a slider is released, emit another signal that it changed
void MultiSlider::sliderReleased(int which)
{
  if (!mPressed[which])
    return;
  mPressed[which] = false;
  emit sliderChanged(which, mSliders[which]->value(), false);
}
/*
$Log$
Revision 1.6  2007/08/26 06:55:59  mast
Documentation changes

Revision 1.5  2007/06/30 00:41:33  sueh
bug# 1021 Fixed the copyright.

Revision 1.4  2007/06/29 21:08:27  sueh
bug# 1021 Allow horizontal, one-line slider lists.

Revision 1.3  2004/11/21 05:52:45  mast
Fixed to prevent two signals on middle mouse click

Revision 1.2  2003/02/10 20:51:22  mast
Merge Qt source

Revision 1.1.2.1  2003/01/26 20:35:41  mast
adding as library file

Revision 1.1.2.7  2003/01/18 20:44:26  mast
resolve some more.  Hate cvs merges!

Revision 1.1.2.6  2003/01/18 20:43:50  mast
resolve merge conflict

Revision 1.1.2.5  2003/01/14 21:45:48  mast
fix format for 2-decimal case, block signals when setting range

Revision 1.1.2.4  2003/01/01 05:39:50  mast
add decimal point capability for value output

Revision 1.1.2.3  2002/12/29 04:21:22  mast
correct delete statements

Revision 1.1.2.2  2002/12/27 17:50:50  mast
change statements for getting pointer arrays to make SGI compiler happy

Revision 1.1.2.1  2002/12/27 01:19:47  mast
Initial creation
*/
