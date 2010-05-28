/*
 *  form_info.cpp - Class for info window form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 * Log at end
 */

#include "form_info.h"

#include <qvariant.h>
#include <qsignalmapper.h>
#include <qimage.h>
#include <qpixmap.h>

//Added by qt3to4:
#include <QPixmap>
#include <qtooltip.h>

#include "imod.h"
#include "unpegged.xpm"
#include "imod_info.h"
#include "imod_info_cb.h"
#include "dia_qtutils.h"
#include "imod_input.h"
#include "pegged.xpm"
#include "preferences.h"

/*
 *  Constructs a InfoControls as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
InfoControls::InfoControls(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
InfoControls::~InfoControls()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void InfoControls::languageChange()
{
  retranslateUi(this);
}

/* Finicky layouts - here are some spacing and margin values
 * Image - model: spacing 4, margin 0
 * Inner vbox's all spacing 6
 * Outer Hboxes, spacing 6, margin 0
 * Slider grid spacing 5
 * vbox with slider and button, spacing 2
 * Mode group, spacing 2, margin 8 
 * bottom hbox, spacing 10
 * whole widget, spacing 4, margin 3
 * As if that is not enough, editing the form on RH 9.0 screwed it up when compiled 
 * on RH 7.3 and it was necessary to run it through designer on 7.3
 */

void InfoControls::init()
{
  int i, floatOn, subarea;
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  mCtrlPressed = false;
  mBlackPressed = false;
  mWhitePressed = false;
  setShowPoint(true);
  imodInfoGetFloatFlags(floatOn, subarea);
  diaSetChecked(floatCheckBox, floatOn != 0);
  diaSetChecked(subareaCheckBox, subarea != 0);
    
  // Set arrays of spin boxes
  mOCPBox[0] = objectSpinBox;
  mOCPBox[1] = contourSpinBox;
  mOCPBox[2] = pointSpinBox;
  mXYZBox[0] = xSpinBox;
  mXYZBox[1] = ySpinBox;
  mXYZBox[2] = zSpinBox;
  mOCPLabel[0] = maxObjectLabel;
  mOCPLabel[1] = maxContourLabel;
  mOCPLabel[2] = maxPointLabel;
  mXYZLabel[0] = maxXLabel;
  mXYZLabel[1] = maxYLabel;
  mXYZLabel[2] = maxZLabel;
    
  raiseButton->setToolTip("Raise all 3dmod windows above other windows (hot key "
                          CTRL_STRING"-R)");
  undoButton->setToolTip("Undo changes to model (hot key "CTRL_STRING"-Z)");
  redoButton->setToolTip("Redo changes that were undone (hot key "
                         CTRL_STRING"-Y)");
    
  // Get signal mappers for the combo boxes
  QSignalMapper *ocpMapper = new QSignalMapper(this);
  QSignalMapper *xyzMapper = new QSignalMapper(this);
  connect(ocpMapper, SIGNAL(mapped(int)), this, SLOT(ocpChanged(int)));
  connect(xyzMapper, SIGNAL(mapped(int)), this, SLOT(xyzChanged(int)));

  // Set up mode radio button group
  modeGroup = new QButtonGroup(this);
  modeGroup->addButton(movieRadioButton, 0);
  modeGroup->addButton(modelRadioButton, 1);
  connect(modeGroup, SIGNAL(buttonClicked(int)), this, 
          SLOT(movieModelSelected(int)));
    
  // Set up mappings and initialize values to force updates
  for (i = 0; i < 3; i++) {
    ocpMapper->setMapping(mOCPBox[i], i);
    connect(mOCPBox[i], SIGNAL(valueChanged(int)), ocpMapper, SLOT(map()));
    mLastOCPval[i] = -2;
    mLastOCPmax[i] = -2;
    xyzMapper->setMapping(mXYZBox[i], i);
    connect(mXYZBox[i], SIGNAL(valueChanged(int)), xyzMapper, SLOT(map()));
    mLastXYZval[i] = -2;
    mLastXYZmax[i] = -2;
  }
  setFontDependentWidths();
    
  QIcon iconSet;
  iconSet.addPixmap(QPixmap(pegged), QIcon::Normal, QIcon::On);
  iconSet.addPixmap(QPixmap(unpegged), QIcon::Normal, QIcon::Off);
  keepOnTopButton->setIcon(iconSet);
  keepOnTopButton->setChecked(false);
  QSize hint = keepOnTopButton->sizeHint();
  raiseButton->setFixedWidth(hint.width());
  keepOnTopButton->setFixedWidth(hint.width());
  undoButton->setFixedWidth(hint.width());
  redoButton->setFixedWidth(hint.width());
  setUndoRedo(false, false);
  bigModelLabel->hide();
}

// Set a minimum width for spin boxes to keep arrows big
void InfoControls::setFontDependentWidths()
{
  diaSetButtonWidth(autoButton, ImodPrefs->getRoundedStyle(), 1.35, "Auto");
  int minWidth = fontMetrics().width("88888888");
  int minLabelWidth = fontMetrics().width("/ 8888");
  for (int i = 0; i < 3; i++) {
    mOCPBox[i]->setMinimumWidth(minWidth);
    mXYZBox[i]->setMinimumWidth(minWidth);
    mOCPLabel[i]->setMinimumWidth(minLabelWidth);
    mXYZLabel[i]->setMinimumWidth(minLabelWidth);
  }
}

// X, Y, or Z changed: pass on the whole array
void InfoControls::xyzChanged( int item )
{
  mLastXYZval[item] = mXYZBox[item]->value();
  imodInfoNewXYZ(mLastXYZval);
}

// Object, contour or point changed
void InfoControls::ocpChanged( int item )
{
  // The box has apparently already lost focus when the number gets here, so
  // that is not a way to determine if user edited
  // Determine if it was edited simply by whether it changed by more than one
  int value = mOCPBox[item]->value();
  int diff = mLastOCPval[item] - value;
  if (diff < 0)
    diff = -diff;
  // int edited = (diff == 1 || diff == mLastOCPmax[item]) ? 0 : 1;
  int edited = mShowPoint ? 0 : 1;
    
  // Interpret a change to 0 and wrap around as appropriate
  if (!value) {
    if (mLastOCPval[item] == 1)
      value = mLastOCPmax[item];
    else
      value = 1;
    diaSetSpinBox(mOCPBox[item], value);
  }
    
  // If value really has changed, pass it on
  if (mLastOCPval[item] != value)
    imodInfoNewOCP(item, value, edited);
  mLastOCPval[item] = value;
}

// Manage the black slider
void InfoControls::blackChanged( int value )
{
  imodInfoNewBW(0, value, mBlackPressed ? 1 : 0);
}

void InfoControls::blackPressed()
{
  mBlackPressed = true;
}

void InfoControls::blackReleased()
{
  mBlackPressed = false;
  blackChanged(mDisplayedBlack);
}

void InfoControls::displayBlack( int value )
{
  mStr.sprintf("%d", value);
  blackLabel->setText(mStr);
  mDisplayedBlack = value;
}

// Manage the white slider
void InfoControls::whiteChanged( int value )
{
  imodInfoNewBW(1, value, mWhitePressed ? 1 : 0);
}

void InfoControls::whitePressed()
{
  mWhitePressed = true;
}

void InfoControls::whiteReleased()
{
  mWhitePressed = false;
  whiteChanged(mDisplayedWhite);
}

void InfoControls::displayWhite( int value )
{
  mStr.sprintf("%d", value);
  whiteLabel->setText(mStr);
  mDisplayedWhite = value;
}

// The buttons
void InfoControls::movieModelSelected( int item )
{
  imodInfoMMSelected(item);
}

void InfoControls::floatToggled( bool state )
{
  imodInfoFloat(state ? 1 : 0);
}

void InfoControls::raisePressed()
{
  inputRaiseWindows();
}

void InfoControls::showPointToggled( bool state )
{
  mShowPoint = state;
}

void InfoControls::keepOnTopToggled( bool state )
{
  ImodInfoWin->keepOnTop(state);
}

void InfoControls::subareaToggled( bool state )
{
  imodInfoSubset(state ? 1 : 0);
}

void InfoControls::autoClicked()
{
  int mean, sd;
  ImodPrefs->getAutoContrastTargets(mean, sd);
  imodInfoAutoContrast(mean, sd);
}

void InfoControls::undoClicked()
{
  inputUndoRedo(App->cvi, false); 
}

void InfoControls::redoClicked()
{
  inputUndoRedo(App->cvi, true); 
}

// ROUTINES FOR SETTING CONTROLS
// Float checkbox is disabled for state < 0
void InfoControls::setFloat( int state )
{
  floatCheckBox->setEnabled(state >= 0);
  subareaCheckBox->setEnabled(state >= 0);
  autoButton->setEnabled(state >= 0);
  diaSetChecked(floatCheckBox, state > 0);
}

// Enable the undo/ redo buttons
void InfoControls::setUndoRedo( bool undoOn, bool redoOn )
{
  undoButton->setEnabled(undoOn);
  redoButton->setEnabled(redoOn);
}

// Set the sliders with new values
void InfoControls::setBWSliders( int black, int white )
{
  displayBlack(black);
  diaSetSlider(blackSlider, black);
  displayWhite(white);
  diaSetSlider(whiteSlider, white);
}

void InfoControls::setMovieModel( int which )
{
  diaSetGroup(modeGroup, which);
}

// Update the Object, Contour, Point
void InfoControls::updateOCP( int *newVal, int *maxVal )
{
  int i;
  for (i = 0; i < 3; i++) {
    if (mLastOCPmax[i] != maxVal[i]) {
      if (maxVal[i] < 0)
        mStr = "    ";
      else if (maxVal[i] >9999)
        mStr.sprintf("/%d", maxVal[i]);
      else
        mStr.sprintf("/ %d", maxVal[i]);
      mOCPLabel[i]->setText(mStr);
	    
      // Block signals when changing the maximum to avoid signals for value changing
      mOCPBox[i]->blockSignals(true);
      mOCPBox[i]->setMaximum(maxVal[i] > 0 ? maxVal[i] : 0);
      mOCPBox[i]->blockSignals(false);
    }
	 
    // If value changed, set special value text appropriately
    if (mLastOCPval[i] != newVal[i]) {
      mOCPBox[i]->setSpecialValueText(newVal[i] < 0 ? "   " : "--x--");
      diaSetSpinBox(mOCPBox[i], newVal[i] > 0 ? newVal[i] : 0);
    }
    mLastOCPmax[i] = maxVal[i];
    mLastOCPval[i] = newVal[i];
  }
}

// Update the x/y/z boxes and labels.  Only set something if it has changed
void InfoControls::updateXYZ( int *newVal, int *maxVal )
{
  int i;
  for (i = 0; i < 3; i++) {
    if (maxVal[i] != mLastXYZval[i]) {
      if (maxVal[i] >9999)
        mStr.sprintf("/%d", maxVal[i]);
      else
        mStr.sprintf("/ %d", maxVal[i]);
      mXYZLabel[i]->setText(mStr);
	    
      // Block signals when changing the maximum to avoid signals for value changing
      mXYZBox[i]->blockSignals(true);
      mXYZBox[i]->setMaximum(maxVal[i]);
      mXYZBox[i]->blockSignals(false);
    }
    if (newVal[i] != mLastXYZval[i])
      diaSetSpinBox(mXYZBox[i], newVal[i]);
    mLastXYZval[i] = newVal[i];
    mLastXYZmax[i] = maxVal[i];
  }
}

// Set object color into some widgets
void InfoControls::setObjectColor( QColor foreColor, QColor backColor )
{
  diaSetWidgetColor(objectLabel, foreColor, QPalette::WindowText);
  diaSetWidgetColor(objectLabel, backColor);
  diaSetWidgetColor(objectSpinBox, foreColor, QPalette::WindowText);
  diaSetWidgetColor(objectSpinBox, backColor, QPalette::Base);
  diaSetWidgetColor(maxObjectLabel, foreColor, QPalette::WindowText);
  diaSetWidgetColor(maxObjectLabel, backColor);
}

// Set the model name into one or the other label and adjust widget size
void InfoControls::setModelName( char *name )
{
  QSize hint = modelLabel->sizeHint();
  int delHeight = 0;
  mStr = name;

  // The label width is only 1-2 pixels bigger than the string
  if (fontMetrics().width(mStr) + 1 > modelLabel->width()) {
    modelLabel->setText(" ");
    bigModelLabel->setText(mStr);
    if (!bigModelLabel->isVisible()) {
      bigModelLabel->show();
      delHeight = hint.height();
    }
  } else {
    modelLabel->setText(mStr);
    if (bigModelLabel->isVisible()) {
      bigModelLabel->hide();
      delHeight = -hint.height();
    }
  }
  if (delHeight) {
    resize(width(), height() + delHeight);
    ImodInfoWin->setFontDependentWidths();
  }
}

// Set the image file name into the label
void InfoControls::setImageName( char *name )
{
  mStr = name;
  //imageLabel->setText(mStr);
}

// Set the show point toggle
void InfoControls::setShowPoint( int state )
{
  mShowPoint =  state != 0;
  diaSetChecked(showCheckBox, mShowPoint);
}

/*

$Log$
Revision 4.3  2010/05/28 17:14:00  mast
Fixed short labels going into big label line

Revision 4.2  2010/04/01 02:29:56  mast
Put model name on a new line when it is too long

Revision 4.1  2009/01/15 16:33:17  mast
Qt 4 port


*/
