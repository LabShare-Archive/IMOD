/*  IMOD VERSION 2.7.9
 *
 *  multislider.cpp       Implementation of class multiple horizontal sliders
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

/* This class provides an arbitrary number of horizontal sliders, each with
 * a label and an integer numeric output that is managed as the slider is
 * dragged.  As the value changes, it emits a signal that indicates the
 * slider number, its value, and whether it is being dragged. 
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

MultiSlider::MultiSlider(QWidget *parent, int numSliders, char *titles[], 
                         int minVal, int maxVal, int decimals)
{
  QVBoxLayout *layOuter;
  QHBoxLayout *layInner;
  QString str;
  QLabel *titleLabel;
  int i;

  // Get arrays for sliders, labels, and pressed flags
  mSliders = new QSlider* [numSliders];
  mPressed = new bool[numSliders];
  mLabels = new QLabel* [numSliders];
  mDecimals = new int [numSliders];
  mNumSliders = numSliders;
  mBigLayout = new QVBoxLayout(NULL, 0, 10, "multislider big");

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
    layOuter = new QVBoxLayout(0, 0, -1);
    mSliders[i] = new QSlider(minVal, maxVal, 1, minVal, Qt::Horizontal,
                              parent);
    mSliders[i]->setFocusPolicy(QSlider::NoFocus);
    layOuter->addWidget(mSliders[i]);

    // Make inner layout, title, and value label
    layInner = new QHBoxLayout(0, 0, 6);
    str = titles[i];
    titleLabel = new QLabel(str, parent);
    titleLabel->setAlignment(Qt::AlignTop | Qt::AlignLeft);
    layInner->addWidget(titleLabel);
    mLabels[i] = new QLabel("0", parent);
    mLabels[i]->setAlignment(Qt::AlignTop | Qt::AlignRight);
    layInner->addWidget(mLabels[i]);
    layOuter->addLayout(layInner);
    mBigLayout->addLayout(layOuter);
    mDecimals[i] = decimals;

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
  delete [] mDecimals;
}

// Set the number of decimals to display
void MultiSlider::setDecimals(int slider, int decimals)
{
  if (decimals < 0)
    decimals = 0;
  if (decimals > MAX_DECIMALS)
    decimals = MAX_DECIMALS;
  mDecimals[slider] = decimals;
}

// They emit signals when they are set, which is a trap for the unwary
void MultiSlider::setValue(int slider, int value)
{
  if (slider >=0 && slider < mNumSliders) {
    mSliders[slider]->blockSignals(true);
    mSliders[slider]->setValue(value);
    mSliders[slider]->blockSignals(false);
    displayValue(slider, value);
  }    
}

void MultiSlider::setRange(int slider, int minVal, int maxVal)
{
  if (slider >=0 && slider < mNumSliders) {
    mSliders[slider]->blockSignals(true);
    mSliders[slider]->setRange(minVal, maxVal);
    mSliders[slider]->blockSignals(false);
  }
}

QSlider *MultiSlider::getSlider(int slider)
{
  if (slider >=0 && slider < mNumSliders)
    return mSliders[slider];
  return NULL;
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
