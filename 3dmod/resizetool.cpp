/*
 *  resizetool.cpp -- implements a widget for resizing a window in steps
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2013 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */
#include <qspinbox.h>
#include <qtooltip.h>
#include <qlayout.h>
#include <QCloseEvent>
#include <QKeyEvent>
#include <QEvent>
#include <QHBoxLayout>
#include "dia_qtutils.h"
#include "resizetool.h"
#include "preferences.h"

static const char *buttonLabels[] = {"Done"};
static const char *buttonTips[] = {"Close dialog box"};

// Construct the tool with two spin boxes
ResizeTool::ResizeTool(QWidget *parent, int xSize, int ySize, int step)
  : DialogFrame(parent, 1, 1, buttonLabels, buttonTips, true, 
                ImodPrefs->getRoundedStyle(), "Resizer", "")

{
  int deskWidth, deskHeight;
  diaMaximumWindowSize(deskWidth, deskHeight);
  diaLabel("Set window size:", this, mLayout);
  QHBoxLayout *hBox = diaHBoxLayout(mLayout);
  xSpinBox = (QSpinBox *)diaLabeledSpin(0, 50, deskWidth, step, "Width", this, hBox);
  connect(xSpinBox, SIGNAL(valueChanged(int)), this, SLOT(xSizeChanged(int)));
  xSpinBox->setToolTip("Set size of window in X without changing model zoom");
  hBox = diaHBoxLayout(mLayout);
  ySpinBox = (QSpinBox *)diaLabeledSpin(0, 50, deskHeight, step, "Height", this, hBox);
  connect(ySpinBox, SIGNAL(valueChanged(int)), this, SLOT(ySizeChanged(int)));
  ySpinBox->setToolTip("Set size of window in Y without changing model zoom");
  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonClicked(int)));
  newSize(xSize, ySize);
}

// The user resizes the window
void ResizeTool::newSize(int sizeX, int sizeY)
{
  diaSetSpinBox(xSpinBox, sizeX);
  diaSetSpinBox(ySpinBox, sizeY);
}

// The spin boxes change
void ResizeTool::xSizeChanged(int value)
{
  setFocus();
  emit(resize(value, ySpinBox->value()));
}

void ResizeTool::ySizeChanged(int value)
{
  setFocus();
  emit(resize(xSpinBox->value(), value));
}

// The Done button is pressed
void ResizeTool::buttonClicked(int which)
{
  close();
}

// Closing
void ResizeTool::closeEvent(QCloseEvent *e)
{
  emit(closing());
  e->accept();
}

// Font change
void ResizeTool::changeEvent(QEvent *e)
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::changeEvent(e);
  if (e->type() != QEvent::FontChange)
    return;
}
