/*
 *  form_mouse.cpp - Class for mouse tab of preferences dialog
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "form_mouse.h"

#include <qvariant.h>
#include <qimage.h>
#include <qpixmap.h>
#include <qtooltip.h>
#include <qbuttongroup.h>

#include "imodconfig.h"
#include "preferences.h"
#include "dia_qtutils.h"

/*
 *  Constructs a MouseForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
MouseForm::MouseForm(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
MouseForm::~MouseForm()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void MouseForm::languageChange()
{
  retranslateUi(this);
}


void MouseForm::init()
{
  mPrefs = ImodPrefs->getDialogPrefs();
  ctrlRadioButton->setText(CTRL_STRING);
  ctrlRadioButton->setToolTip("Make "CTRL_STRING
                              " key control whether sliders are continuously active");
  hotKeyGroup = new QButtonGroup(this);
  hotKeyGroup->addButton(ctrlRadioButton, 0);
  hotKeyGroup->addButton(shiftRadioButton, 1);
  hotKeyGroup->addButton(altRadioButton, 2);
  connect(hotKeyGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(keyChanged(int)));
  activeGroup = new QButtonGroup(this);
  activeGroup->addButton(keyUpRadio, 0);
  activeGroup->addButton(keyDownRadio, 1);
  activeGroup->addButton(neverRadio, 2);
  connect(activeGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(flagChanged(int)));
  mouseGroup = new QButtonGroup(this);
  mouseGroup->addButton(lmrRadioButton, 0);
  mouseGroup->addButton(mrlRadioButton, 1);
  mouseGroup->addButton(rlmRadioButton, 2);
  mouseGroup->addButton(lrmRadioButton, 3);
  mouseGroup->addButton(mlrRadioButton, 4);
  mouseGroup->addButton(rmlRadioButton, 5);
  connect(mouseGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(mappingChanged(int)));
    
  setMouseLabels();
  update();
}

// Set the state of the buttons
void MouseForm::update()
{
  diaSetGroup(hotKeyGroup, mPrefs->hotSliderKey);
  diaSetGroup(activeGroup, mPrefs->hotSliderFlag);
  diaSetGroup(mouseGroup, mPrefs->mouseMapping);
  diaSetChecked(swapModvCheckBox, mPrefs->modvSwapLeftMid);
}

// record changes in the buttons
void MouseForm::flagChanged( int value )
{
  mPrefs->hotSliderFlag = value;
}

void MouseForm::keyChanged( int value )
{
  mPrefs->hotSliderKey = value;
}

void MouseForm::mappingChanged( int value )
{
  mPrefs->mouseMapping = value;
  setMouseLabels();
}

void MouseForm::swapToggled( bool state )
{
  mPrefs->modvSwapLeftMid = state;
}

void  MouseForm::setMouseLabels()
{
  const char *texts[3] = {"Pan, mark\nAttach to pt", "Movie up\nAdd point", 
                    "Movie down\nModify pt"};
  int leftMapping[6] = {0, 2, 1, 0, 1, 2};
  int midMapping[6] = {1, 0, 2, 2, 0, 1};
  int rightMapping[6] = {2, 1, 0, 1, 2, 0};
  leftLabel->setText(texts[leftMapping[mPrefs->mouseMapping]]);
  middleLabel->setText(texts[midMapping[mPrefs->mouseMapping]]);
  rightLabel->setText(texts[rightMapping[mPrefs->mouseMapping]]);
}
