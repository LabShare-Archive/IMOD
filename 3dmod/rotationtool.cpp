/*
 *  rotationtool.cpp -- implements a widget for 3D rotation
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2013 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */
#include <stdio.h>
#include <qtooltip.h>
#include <qsignalmapper.h>
#include <qlayout.h>
#include <qlabel.h>
#include <qtoolbutton.h>
#include <qevent.h>
#include "rotationtool.h"
#include "dia_qtutils.h"

static QIcon *sIcons[8];
static const char *sFileList[8] = 
  {":/images/plusSign.png", ":/images/upRotArrow.png",  ":/images/rotCCWarrow.png", 
    ":/images/leftRotArrow.png",  ":/images/rightRotArrow.png", 
   ":/images/minusSign.png", ":/images/downRotArrow.png",  ":/images/rotCWarrow.png"};

static bool sFirstTime = true;
static const char *sButtonTips[] = {
  "Increase step size", "Rotate clockwise around current X axis", 
  "Rotate counterclockwise around current Z axis", 
  "Rotate clockwise around current Y axis", 
  "Rotate counterclockwise around current Y axis", 
  "Decrease step size", "Rotate counterclockwise around current X axis", 
  "Rotate clockwise around current Z axis"};

static int sStepSign[8] = {1, 0, 0, 0, 0, -1, 0, 0};

/*
 * The constructor.  The central button is optional, centerIcon and centerTip are the
 * icon and tooltip for it and can be NULL.  size specifies the size of the buttons.
 * The autoRaise flag is used to set whether the buttons autoraise.  A step size label
 * is optional and provided if stepSize >= 0.
 */
RotationTool::RotationTool(QWidget *parent, QIcon *centerIcon, const char *centerTip, 
                           int size, bool autoRaise, float stepSize)
  : QWidget(parent)
{
  int ind, row, col;
  mStepLabel = NULL;
  mCenterBut = NULL;
  if (sFirstTime) {
    for (ind = 0; ind < 8; ind++) {
      sIcons[ind] = new QIcon();
      sIcons[ind]->addFile(QString(sFileList[ind]), QSize(size - 4, size - 4));
    }
  }

  QHBoxLayout *hLayout = new QHBoxLayout(this);
  hLayout->setContentsMargins(0, 0, 0, 0);
  hLayout->setSpacing(0);
  QVBoxLayout *vLayout = new QVBoxLayout();
  hLayout->addLayout(vLayout);
  vLayout->setContentsMargins(0, 0, 0, 0);
  vLayout->setSpacing(2);
  hLayout->addStretch();

  // Make the grid and the signal mapper
  QGridLayout *grid = new QGridLayout();
  vLayout->addLayout(grid);
  grid->setContentsMargins(0, 0, 0, 0);
  grid->setSpacing(autoRaise ? 0 : 4);

  QSignalMapper *rotMapper = new QSignalMapper(this);
  connect(rotMapper, SIGNAL(mapped(int)), this, SLOT(buttonClicked(int)));

  // Fill the grid
  ind = 0;
  for (row = 0; row < 3; row++) {
    for (col = 0; col < 3; col++) {
      QToolButton *button = new QToolButton(this);
      button->setAutoRaise(autoRaise);
      button->setFixedSize(size, size);
      button->setFocusPolicy(Qt::NoFocus);
      grid->addWidget(button, row, col);
      if (col == 1 && row == 1) {

        // Set center toggle button properties
        button->setCheckable(true);
        button->setChecked(false);
        mCenterBut = button;
        if (centerTip)
          button->setToolTip(centerTip);
        if (centerIcon) 
          button->setIcon(*centerIcon);
        connect(mCenterBut, SIGNAL(toggled(bool)), this, SLOT(centerToggled(bool)));
      } else {

        // Set rotation button properties and mapping, and autorepeat for non-step ones
        button->setToolTip(sButtonTips[ind]);
        button->setIcon(*sIcons[ind]);
        rotMapper->setMapping(button, ind);
        connect(button, SIGNAL(clicked()), rotMapper, SLOT(map()));
        if (!sStepSign[ind]) {
          button->setAutoRepeat(true);
          button->setAutoRepeatDelay(300);
          button->setAutoRepeatInterval(100);
        }
        ind++;
      }
    }
  }

  if (stepSize >= 0) {
    mStepLabel = diaLabel("Step", this, vLayout);
    mStepLabel->setAlignment(Qt::AlignHCenter | Qt::AlignVCenter);
    setStepLabel(stepSize);
  }
  vLayout->addStretch();
}

// Set the state of the center toggle button
void RotationTool::setCenterState(bool state)
{
  if (mCenterBut)
    diaSetChecked(mCenterBut, state);
}

// Set the label for step size
void RotationTool::setStepLabel(float step)
{
  QString str;
  str.sprintf("Step: %g", step);
  if (mStepLabel)
    mStepLabel->setText(str);
}

// The center button is toggled, emit its state
void RotationTool::centerToggled(bool state)
{
  emit centerButToggled(state);
}

// One of the other buttons clicked, send out the step change or rotation signal
void RotationTool::buttonClicked(int which)
{
  int rotSteps[8][3] = 
    { {0, 0, 0}, {1, 0, 0}, {0, 0, 1},
      {0, -1, 0}, {0, 1, 0},
      {0, 0, 0}, {-1, 0, 0}, {0, 0, -1} };
  if (sStepSign[which])
    emit(stepChanged(sStepSign[which]));
  else
    emit(rotate(rotSteps[which][0], rotSteps[which][1], rotSteps[which][2]));
}

void RotationTool::closeEvent(QCloseEvent *e)
{
  emit(closing());
  e->accept();
}
