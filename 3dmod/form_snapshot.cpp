/*
 *  form_snapshot.cpp - Class for snapshot tab of preferences form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2010 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 * Log at end
 */

#include "form_snapshot.h"

#include <qvariant.h>
#include <qtooltip.h>
#include <stdlib.h>
#include <qimage.h>
#include <qpixmap.h>

#include "dia_qtutils.h"
#include "preferences.h"

/*
 *  Constructs a SnapshotForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
SnapshotForm::SnapshotForm(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
SnapshotForm::~SnapshotForm()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void SnapshotForm::languageChange()
{
  retranslateUi(this);
}


void SnapshotForm::init()
{
  mPrefs = ImodPrefs->getDialogPrefs();
  formatComboBox->addItems(ImodPrefs->snapFormatList());
  setFontDependentWidths();
  update();
}

void SnapshotForm::setFontDependentWidths()
{
}

// Update all widgets with the current structure values
void SnapshotForm::update()
{
    
  // Look up the snapshot format in the list of output formats to set combo box
  int item = 0;
  QStringList formats = ImodPrefs->snapFormatList();
  for (int i = 0; i < formats.count(); i++)
    if (formats[i] == mPrefs->snapFormat)
      item = i;
  formatComboBox->setCurrentIndex(item);
  diaSetSpinBox(qualitySpinBox, mPrefs->snapQuality);
  diaSetSpinBox(dpiSpinBox, mPrefs->snapDPI);
  diaSetChecked(scaleDPIcheckBox, mPrefs->scaleSnapDPI);
  showOtherFormats(item);
  connect(formatComboBox, SIGNAL(activated(int)), this, 
          SLOT(showOtherFormats(int)));
}

// Get state of widgets other than zoom-related and put in structure
void SnapshotForm::unload()
{
  mPrefs->snapFormat = formatComboBox->currentText();
  mPrefs->snapQuality = qualitySpinBox->value();
  mPrefs->snapDPI = dpiSpinBox->value();
  mPrefs->scaleSnapDPI = scaleDPIcheckBox->isChecked();
}

void SnapshotForm::showOtherFormats(int item)
{
  QStringList formats = ImodPrefs->snapFormatList();
  QString second = ImodPrefs->snapFormat2(&formats[item]);
  QString label;
  label.sprintf("%s-S gives TIFF, %s-Shift-S ", CTRL_STRING,
                             CTRL_STRING);
  if (second != "")
    label += "gives " + second;
  else
    label += "will not work";
  otherFormatsLabel->setText(label);
}

/*

$Log$

*/
