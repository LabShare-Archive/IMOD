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
    mZoomIndex = MAXZOOMS / 3;
    zoomIndexSpinBox->setMaxValue(MAXZOOMS);
    diaSetSpinBox(zoomIndexSpinBox, mZoomIndex + 1);
    setFontDependentWidths();
    update();
    
}

void BehaviorForm::setFontDependentWidths()
{
    int width = (6 * 2 + 3) * fontMetrics().width("999999") / (6 * 2);
    autosaveSpinBox->setMaximumWidth(width);
    zoomEdit->setMaximumWidth(width);
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
    displayCurrentZoom();
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
    unloadZoomValue();
    QDir *curdir = new QDir();
    mPrefs->autosaveDir = curdir->cleanDirPath(autosaveDirEdit->text());
    delete curdir;
}

void BehaviorForm::displayCurrentZoom()
{
    QString str;
    double zoom = mPrefs->zooms[mZoomIndex];
    str.sprintf("%.4f", zoom);
    if (str.endsWith("00"))
        str.truncate(str.length() - 2);
    zoomEdit->setText(str);
    str = "Default " + str;
    defaultZoomLabel->setText(str);
    mZoomValChanged = false;
}

void BehaviorForm::newZoomIndex( int value )
{
    unloadZoomValue();
    mZoomIndex = value - 1;
    displayCurrentZoom();
}


void BehaviorForm::unloadZoomValue()
{
    if (!mZoomValChanged)
	return;
    double zoom = zoomEdit->text().toDouble();
    if (zoom < 0.01)
	zoom = 0.01;
    if (zoom > 100.)
	zoom = 100.;
    double roundfac = zoom < 1.0 ? 1000. : 100.;
    mPrefs->zooms[mZoomIndex] = ((int)(roundfac * zoom + 0.5)) / roundfac;
    mPrefs->zoomsChgd = true;
}

void BehaviorForm::newZoomValue()
{
    mZoomValChanged = true;
}

void BehaviorForm::shiftZoomsDown()
{
    unloadZoomValue();
    for (int i = 0; i < MAXZOOMS - 1; i++)
	mPrefs->zooms[i] = mPrefs->zooms[i + 1];
    mPrefs->zoomsChgd = true;
    displayCurrentZoom();
}

void BehaviorForm::restoreDefaultZooms()
{
    for (int i = 0; i < MAXZOOMS; i++)
	mPrefs->zooms[i] = mPrefs->zoomsDflt[i];
    mPrefs->zoomsChgd = true;
    displayCurrentZoom();
}

void BehaviorForm::shiftZoomsUp()
{
    unloadZoomValue();
    for (int i = MAXZOOMS - 1; i > 0; i--)
	mPrefs->zooms[i] = mPrefs->zooms[i - 1];
    mPrefs->zoomsChgd = true;
    displayCurrentZoom();
}



void BehaviorForm::toolTipsToggled( bool state )
{
    mPrefs->tooltipsOn = state;
    QToolTip::setGloballyEnabled(mPrefs->tooltipsOn);
}
