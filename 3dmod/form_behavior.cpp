/*
 *  form_behavior.cpp - Class for behavior tab of preferences form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "form_behavior.h"

#include <qvariant.h>
#include <qtooltip.h>
#include <qdir.h>
#include <stdlib.h>
#include <qimage.h>
#include <qpixmap.h>

#include "dia_qtutils.h"
#include "preferences.h"

/*
 *  Constructs a BehaviorForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
BehaviorForm::BehaviorForm(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
BehaviorForm::~BehaviorForm()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void BehaviorForm::languageChange()
{
  retranslateUi(this);
}


void BehaviorForm::init()
{
  mPrefs = ImodPrefs->getDialogPrefs();
  setFontDependentWidths();
  update();
}

void BehaviorForm::setFontDependentWidths()
{
  int width = (6 * 2 + 3) * fontMetrics().width("999999") / (6 * 2);
  autosaveSpinBox->setMaximumWidth(width);
}

// Update all widgets with the current structure values
void BehaviorForm::update()
{
  diaSetChecked(silenceBox, mPrefs->silentBeep);
  diaSetChecked(classicBox, mPrefs->classicSlicer);
  diaSetChecked(startInHQBox, mPrefs->startInHQ);
  diaSetChecked(arrowsScrollZapBox, mPrefs->arrowsScrollZap);
  //  diaSetChecked(tooltipBox, mPrefs->tooltipsOn);
  diaSetChecked(startAtMidZBox, mPrefs->startAtMidZ);
  diaSetChecked(loadUshortBox, mPrefs->loadUshorts);
  diaSetChecked(selectONcheckBox, mPrefs->attachToOnObj);
  diaSetChecked(slicerNewSurfBox, mPrefs->slicerNewSurf);
  autoConAtStartBox->setCurrentIndex(mPrefs->autoConAtStart);
  diaSetChecked(geomCheckBox, mPrefs->rememberGeom);
  diaSetSpinBox(f1f8StepSpinBox, mPrefs->bwStep);
  diaSetSpinBox(pageStepSpinBox, mPrefs->pageStep);
  diaSetChecked(autosaveEnabledBox, mPrefs->autosaveOn);
  diaSetSpinBox(autosaveSpinBox, mPrefs->autosaveInterval);
  autosaveDirEdit->setText(QDir::convertSeparators(mPrefs->autosaveDir));
  diaSetChecked(imageIconifyBox, mPrefs->iconifyImageWin);
  diaSetChecked(imodDlgIconifyBox, mPrefs->iconifyImodDlg);
  diaSetChecked(imodvDlgIconifyBox, mPrefs->iconifyImodvDlg);
}

// Get state of widgets other than zoom-related and put in structure
void BehaviorForm::unload()
{
  mPrefs->silentBeep = silenceBox->isChecked();
  mPrefs->classicSlicer = classicBox->isChecked();
  mPrefs->startInHQ = startInHQBox->isChecked();
  mPrefs->arrowsScrollZap = arrowsScrollZapBox->isChecked();
  //  mPrefs->tooltipsOn = tooltipBox->isChecked();
  mPrefs->startAtMidZ = startAtMidZBox->isChecked();
  mPrefs->loadUshorts = loadUshortBox->isChecked();
  mPrefs->attachToOnObj = selectONcheckBox->isChecked();
  mPrefs->slicerNewSurf = slicerNewSurfBox->isChecked();
  mPrefs->autoConAtStart = autoConAtStartBox->currentIndex();
  mPrefs->rememberGeom = geomCheckBox->isChecked();
  mPrefs->bwStep = f1f8StepSpinBox->value();
  mPrefs->pageStep = pageStepSpinBox->value();
  mPrefs->autosaveOn = autosaveEnabledBox->isChecked();
  mPrefs->autosaveInterval = autosaveSpinBox->value();
  mPrefs->iconifyImageWin = imageIconifyBox->isChecked();
  mPrefs->iconifyImodDlg = imodDlgIconifyBox->isChecked();
  mPrefs->iconifyImodvDlg = imodvDlgIconifyBox->isChecked();
  QDir *curdir = new QDir();
  mPrefs->autosaveDir = curdir->cleanPath(autosaveDirEdit->text());
  delete curdir;
}
/*
void BehaviorForm::toolTipsToggled( bool state )
{
  mPrefs->tooltipsOn = state;
  //QToolTip::setGloballyEnabled(mPrefs->tooltipsOn);
}
*/
