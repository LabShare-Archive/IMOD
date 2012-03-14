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
 */

#include <math.h>
#include <qsignalmapper.h>
#include <qslider.h>
#include <qlayout.h>
#include <qlabel.h>
//Added by qt3to4:
#include <QHBoxLayout>
#include "multislider.h"
#include "dia_qtutils.h"

#define NON_VALUE -99999999
#define MAX_DECIMALS 6

static char const *deciFormats[MAX_DECIMALS + 1] = {"%d", "%.1f", "%.2f", "%.3f",
					      "%.4f", "%.5f", "%.6f"};

/*!
 * This class provides an arbitrary number of horizontal sliders, each with
 * a label and an integer numeric output that is managed as the slider is
 * dragged.  The number of sliders is set in [numSliders], their text labels 
 * in [titles].  Overall minimal and maximum values and number of decimal
 * places can be set with [minVal] (default 0), [maxVal] (default 255), and
 * [decimals] (default 0).  The page step and single step size of each slider
 * is set to 1.  Set [horizontal] flag to true to have the sliders
 * arranged in a QHBoxLayout instead of a QVBoxLayout.  The layout can be
 * obtained with:
 * ^   QBoxLayout *getLayout();
 * ^ As a slider value changes, the class emits a signal:
 * ^   void sliderChanged(int slider, int value, bool dragging);
 * ^ with [slider] equal to the slider number, [value] with the new integer
 * value, and [dragging] true if the slider is being dragged.
 */

MultiSlider::MultiSlider(QWidget *parent, int numSliders, const char *titles[], 
                         int minVal, int maxVal, int decimals, bool horizontal)
{
  mHorizontal = horizontal;
  QBoxLayout *layOuter;
  QHBoxLayout *layInner;
  QString str;
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
  mBigLayout = new QBoxLayout(dir);
  mBigLayout->setSpacing(10);
  mBigLayout->setContentsMargins(0, 0, 0, 0);

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
    layOuter = new QBoxLayout(dir);
    layOuter->setSpacing(spacing);
    str = titles[i];
    mTitleLabels[i] = new QLabel(str, parent);
    mTitleLabels[i]->setAlignment(Qt::AlignTop | Qt::AlignLeft);
    if (horizontal) {
      layOuter->addWidget(mTitleLabels[i]);
    }
    mSliders[i] = new QSlider(Qt::Horizontal, parent);
    mSliders[i]->setRange(minVal, maxVal);
    mSliders[i]->setSingleStep(1);
    mSliders[i]->setPageStep(1);
    mSliders[i]->setValue(minVal);
    mSliders[i]->setFocusPolicy(Qt::NoFocus);
    layOuter->addWidget(mSliders[i]);
    
    if (!horizontal) {
      // Make inner layout, title, and value label
      layInner = new QHBoxLayout();
      layInner->setSpacing(6);
      layInner->setContentsMargins(0, 0, 0, 0);
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
 * Sets the range of the given slider to [minVal], [maxVal] and the value to [value] with
 * signals blocked, thus avoiding any signals from an existing value being outside of a 
 * new range.
 */
void MultiSlider::setMinMaxVal(int slider, int minVal, int maxVal, int value)
{
  if (slider >=0 && slider < mNumSliders) {
    mSliders[slider]->blockSignals(true);
    mSliders[slider]->setRange(minVal, maxVal);
    mSliders[slider]->setValue(value);
    mSliders[slider]->blockSignals(false);
    displayValue(slider, value);
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

/*!
 * Shows or hides a given slider and its associated text fields depending on
 * the value of [show].
 */
void MultiSlider::showWidgets(int slider, bool show)
{
  if (slider >=0 && slider < mNumSliders) {
    diaShowWidget(mSliders[slider], show);
    diaShowWidget(mLabels[slider], show);
    diaShowWidget(mTitleLabels[slider], show);
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
