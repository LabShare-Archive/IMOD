/*
 *  form_rawimage.cpp - Class for raw image dialog form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "form_rawimage.h"

#include <qvariant.h>
#include <qregexp.h>
#include <qimage.h>
#include <qpixmap.h>

#include "dia_qtutils.h"
#include "iimage.h"

/*
 *  Constructs a RawImageForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 *  The dialog will by default be modeless, unless you set 'modal' to
 *  true to construct a modal dialog.
 */
RawImageForm::RawImageForm(QWidget* parent, bool modal, Qt::WindowFlags fl)
    : QDialog(parent, fl)
{
  setupUi(this);
  setModal(modal);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  
  dataTypeGroup = new QButtonGroup(this);
  dataTypeGroup->addButton(sbyteButton, 0);
  dataTypeGroup->addButton(byteButton, 1);
  dataTypeGroup->addButton(intButton, 2);
  dataTypeGroup->addButton(uintButton, 3);
  dataTypeGroup->addButton(floatButton, 4);
  dataTypeGroup->addButton(complexButton, 5);
  dataTypeGroup->addButton(RGBButton, 6);
  connect(dataTypeGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(manageState()));
}

/*
 *  Destroys the object and frees any allocated resources
 */
RawImageForm::~RawImageForm()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void RawImageForm::languageChange()
{
  retranslateUi(this);
}


// Load the values from the info structure into the form
void RawImageForm::load(QString fileName, RawImageInfo *info )
{
  QString str;
  int ind = str.lastIndexOf('/');
  if (ind < 0)
    ind = str.lastIndexOf('\\');
  if (ind >= 0)
    str = str.right(str.length() - 1 - ind);
  str = str;
  fileLabel->setText(QString("File: ") + fileName);
  diaSetGroup(dataTypeGroup, info->type);
  xSizeSpinBox->setValue(info->nx);
  ySizeSpinBox->setValue(info->ny);
  zSizeSpinBox->setValue(info->nz);
  headerSpinBox->setValue(info->headerSize);
  swapCheckBox->setChecked(info->swapBytes);
  invertCheckBox->setChecked(info->yInverted);
  matchCheckBox->setChecked(info->allMatch);
  scanCheckBox->setChecked(info->scanMinMax);
  str.sprintf("%f", info->amin);
  minLineEdit->setText(str);
  str.sprintf("%f", info->amax);
  maxLineEdit->setText(str);
  manageState();
}

// Unload the form into the structure
void RawImageForm::unload( RawImageInfo *info )
{
  int ind = -1;
  ind = dataTypeGroup->checkedId();
  if (ind >= 0)
    info->type = ind;
  info->nx = xSizeSpinBox->value();
  info->ny = ySizeSpinBox->value();
  info->nz = zSizeSpinBox->value();
  info->headerSize = headerSpinBox->value();
  info->swapBytes = swapCheckBox->isChecked();
  info->yInverted = invertCheckBox->isChecked();
  info->allMatch = matchCheckBox->isChecked();
  info->scanMinMax = scanCheckBox->isChecked();
  info->amin = minLineEdit->text().toFloat();
  info->amax = maxLineEdit->text().toFloat();
}

// Manage scale and swap options based on type and scan check box
void RawImageForm::manageState()
{
  int which = -1;
  which = dataTypeGroup->checkedId();
  if (which < 0)
    return;
  bool enab = which !=6 && !scanCheckBox->isChecked();
  swapCheckBox->setEnabled(which && which != 6);
  scanCheckBox->setEnabled(which != 6);
  minLabel->setEnabled(enab);
  minLineEdit->setEnabled(enab);
  maxLabel->setEnabled(enab);
  maxLineEdit->setEnabled(enab);
}
