/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/


void BehaviorForm::init()
{
    mPrefs = ImodPrefs->getDialogPrefs();
    formatComboBox->insertStringList(ImodPrefs->snapFormatList());
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
    diaSetChecked(tooltipBox, mPrefs->tooltipsOn);
    diaSetChecked(geomCheckBox, mPrefs->rememberGeom);
    diaSetSpinBox(f1f8StepSpinBox, mPrefs->bwStep);
    diaSetChecked(autosaveEnabledBox, mPrefs->autosaveOn);
    diaSetSpinBox(autosaveSpinBox, mPrefs->autosaveInterval);
    autosaveDirEdit->setText(QDir::convertSeparators(mPrefs->autosaveDir));
    diaSetChecked(imageIconifyBox, mPrefs->iconifyImageWin);
    diaSetChecked(imodDlgIconifyBox, mPrefs->iconifyImodDlg);
    diaSetChecked(imodvDlgIconifyBox, mPrefs->iconifyImodvDlg);
    
    // Look up the snapshot format in the list of output formats to set combo box
    int item = 0;
    QStringList formats = ImodPrefs->snapFormatList();
    for (int i = 0; i < formats.count(); i++)
        if (formats[i] == mPrefs->snapFormat)
            item = i;
    formatComboBox->setCurrentItem(item);
    diaSetSpinBox(qualitySpinBox, mPrefs->snapQuality);
}

// Get state of widgets other than zoom-related and put in structure
void BehaviorForm::unload()
{
    mPrefs->silentBeep = silenceBox->isChecked();
    mPrefs->tooltipsOn = tooltipBox->isChecked();
    mPrefs->rememberGeom = geomCheckBox->isChecked();
    mPrefs->bwStep = f1f8StepSpinBox->value();
    mPrefs->autosaveOn = autosaveEnabledBox->isChecked();
    mPrefs->autosaveInterval = autosaveSpinBox->value();
    mPrefs->iconifyImageWin = imageIconifyBox->isChecked();
    mPrefs->iconifyImodDlg = imodDlgIconifyBox->isChecked();
    mPrefs->iconifyImodvDlg = imodvDlgIconifyBox->isChecked();
    QDir *curdir = new QDir();
    mPrefs->autosaveDir = curdir->cleanDirPath(autosaveDirEdit->text());
    delete curdir;
    mPrefs->snapFormat = formatComboBox->currentText();
    mPrefs->snapQuality = qualitySpinBox->value();
}


void BehaviorForm::toolTipsToggled( bool state )
{
    mPrefs->tooltipsOn = state;
    QToolTip::setGloballyEnabled(mPrefs->tooltipsOn);
}
