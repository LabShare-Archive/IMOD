/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you want to add, delete, or rename functions or slots, use
** Qt Designer to update this file, preserving your code.
**
*****************************************************************************/

// Load the values from theinfo structure into the form
void RawImageForm::load(QString fileName, RawImageInfo *info )
{
    QString str;
    int ind = str.findRev('/');
    if (ind < 0)
      ind = str.findRev('\\');
    if (ind >= 0)
	str = str.right(str.length() - 1 - ind);
    str = str;
    fileLabel->setText(QString("File: ") + fileName);
    dataTypeGroup->setButton(info->type);
    xSizeSpinBox->setValue(info->nx);
    ySizeSpinBox->setValue(info->ny);
    zSizeSpinBox->setValue(info->nz);
    headerSpinBox->setValue(info->headerSize);
    swapCheckBox->setChecked(info->swapBytes);
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
    if (dataTypeGroup->selected())
        ind = dataTypeGroup->id(dataTypeGroup->selected());
    if (ind >= 0)
        info->type = ind;
    info->nx = xSizeSpinBox->value();
    info->ny = ySizeSpinBox->value();
    info->nz = zSizeSpinBox->value();
    info->headerSize = headerSpinBox->value();
    info->swapBytes = swapCheckBox->isChecked();
    info->allMatch = matchCheckBox->isChecked();
    info->scanMinMax = scanCheckBox->isChecked();
    info->amin = minLineEdit->text().toFloat();
    info->amax = maxLineEdit->text().toFloat();
}

// Manage scale and swap options based on type and scan check box
void RawImageForm::manageState()
{
    int which = -1;
    if (dataTypeGroup->selected())
        which = dataTypeGroup->id(dataTypeGroup->selected());
    if (which < 0)
        return;
    bool enab = which !=4 && !scanCheckBox->isChecked();
    swapCheckBox->setEnabled(which && which != 4);
    scanCheckBox->setEnabled(which != 4);
    minLabel->setEnabled(enab);
    minLineEdit->setEnabled(enab);
    maxLabel->setEnabled(enab);
    maxLineEdit->setEnabled(enab);
}
