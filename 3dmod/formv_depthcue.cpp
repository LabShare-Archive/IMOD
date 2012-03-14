/*
 *  formv_depthcue.cpp - Class for depth cue dialog form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "formv_depthcue.h"

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>

//Added by qt3to4:
#include <QKeyEvent>
#include <QCloseEvent>

#include "dia_qtutils.h"
#include "imod.h"
#include "mv_input.h"
#include "mv_depthcue.h"
#include "preferences.h"

/*
 *  Constructs a imodvDepthcueForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
imodvDepthcueForm::imodvDepthcueForm(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
imodvDepthcueForm::~imodvDepthcueForm()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void imodvDepthcueForm::languageChange()
{
  retranslateUi(this);
}

// Initialize the flags and set slider widths
void imodvDepthcueForm::init()
{
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  mCtrlPressed = false;
  mStartPressed = false;
  mEndPressed = false;
  int width = DEPTHCUE_MAX - DEPTHCUE_MIN + 16;
  startSlider->setMinimumWidth(width);
  startSlider->setMinimum(DEPTHCUE_MIN);
  startSlider->setMaximum(DEPTHCUE_MAX);
  endSlider->setMinimumWidth(width);
  endSlider->setMinimum(DEPTHCUE_MIN);
  endSlider->setMaximum(DEPTHCUE_MAX);
    
}

void imodvDepthcueForm::depthcueToggled( bool state )
{
  imodvDepthcueToggle(state ? 1 : 0);
}

// Display and change functions for the Start slider
void imodvDepthcueForm::displayStartLabel( int value )
{
  mStr.sprintf("%d", value);
  startTextLabel->setText(mStr);
  mStartDisplayed = value;
}

void imodvDepthcueForm::startChanged( int value )
{
  if (!mStartPressed || ImodPrefs->hotSliderActive(mCtrlPressed))
    imodvDepthcueStartEnd(value, false, mStartPressed);
}

void imodvDepthcueForm::startPressed()
{
  mStartPressed = true;
}

void imodvDepthcueForm::startReleased()
{
  mStartPressed = false;
  startChanged(mStartDisplayed);
}

// Display and change functions for the End slider
void imodvDepthcueForm::displayEndLabel( int value )
{
  mStr.sprintf("%d", value);
  endTextLabel->setText(mStr);
  mEndDisplayed = value;
}

void imodvDepthcueForm::endChanged( int value )
{
  if (!mEndPressed || ImodPrefs->hotSliderActive(mCtrlPressed))
    imodvDepthcueStartEnd(value, true, mEndPressed);
}

void imodvDepthcueForm::endPressed()
{
  mEndPressed = true;
}

void imodvDepthcueForm::endReleased()
{
  mEndPressed = false;
  endChanged(mEndDisplayed);
}

void imodvDepthcueForm::donePressed()
{
  imodvDepthcueDone();
}

void imodvDepthcueForm::helpPressed()
{
  imodvDepthcueHelp();
}

// Set the states of the widgets
void imodvDepthcueForm::setStates( int enabled, int start, int end )
{
  diaSetChecked(enableBox, enabled);
  displayStartLabel(start);
  diaSetSlider(startSlider, start);
  displayEndLabel(end);
  diaSetSlider(endSlider, end);
}

// Tell imodv we are closing
void imodvDepthcueForm::closeEvent( QCloseEvent * e )
{
  imodvDepthcueClosing();
  e->accept();
}

// Quit on escape, set flag and grab keyboard if ctrl, pass key on
void imodvDepthcueForm::keyPressEvent( QKeyEvent * e )
{
  if (utilCloseKey(e)) {
    imodvDepthcueDone();
  } else {
    
    if (hotSliderFlag() != NO_HOT_SLIDER && e->key() == hotSliderKey()) {
      mCtrlPressed = true;
      grabKeyboard();
    }
    imodvKeyPress(e);
  }
}

// release keyboard if ctrl
void imodvDepthcueForm::keyReleaseEvent( QKeyEvent * e )
{
  if (e->key() == hotSliderKey()) {
    mCtrlPressed = false;
    releaseKeyboard();
  }
  imodvKeyRelease(e);
}

