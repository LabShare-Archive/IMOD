/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/

// Initialize
void AppearanceForm::init()
{
    mPrefs = ImodPrefs->getDialogPrefs();
    update();
    
    // Load the style combo box and figure  out which one is the current one
    int ind = 0;
#ifdef EXCLUDE_STYLES    
    
    // If have a list of ones to exclude, get the whole list with the factory and insert
   // ones that are OK, i.e. not on the exclude list
    QStringList keyList = QStyleFactory::keys();
    for ( QStringList::Iterator it = keyList.begin(); it != keyList.end(); ++it ) {
	if (!ImodPrefs->styleOK(*it))
	    continue;
	styleComboBox->insertItem(*it);
	if ((*it).lower() == mPrefs->styleKey.lower())
	    styleComboBox->setCurrentItem(ind);
	ind++;
    }
#else
    
    // If the list is of ones to include, then go through the list and insert ones that
    // can be created
    char **styleList =ImodPrefs->getStyleList();
    int *styleStatus = ImodPrefs->getStyleStatus();
    for (int i = 0;  ; i++) {
	QString str = styleList[i];
	if (str.isEmpty())
	    break;
	if (!styleStatus[i]) {
	    // fprintf(stderr, "FA testing %s\n", str.latin1());
	    if (QStyleFactory::create(str) == NULL)
		styleStatus[i] = -1;
	    else
		styleStatus[i] = 1;
	}
	if (styleStatus[i] < 0)
	    continue;
	styleComboBox->insertItem(str);
	if (str.lower() == mPrefs->styleKey.lower())
	    styleComboBox->setCurrentItem(ind);
	ind++;
    }
#endif
}

// Update the dialog based on current values
void AppearanceForm::update()
{
    diaSetSpinBox(imagePtSpinBox, mPrefs->minImPtSize);
    diaSetSpinBox(modelPtSpinBox, mPrefs->minModPtSize);
}

// New font was selected
void AppearanceForm::fontPressed()
{
    bool ok;
    mPrefs->font = QFontDialog::getFont(&ok, mPrefs->font, this);
    if (!ok) 
	return;
    mPrefs->fontChgd = true;
    ImodPrefs->changeFont(mPrefs->font);
}

// Size of image point marker changed
void AppearanceForm::imagePtChanged( int value )
{
    mPrefs->minImPtSize = value;
    mPrefs->minImPtSizeChgd = true;
    ImodPrefs->pointSizeChanged();
}

// Size of model point marker changed
void AppearanceForm::modelPtChanged( int value )
{
    mPrefs->minModPtSize = value;
    mPrefs->minModPtSizeChgd = true;
    ImodPrefs->pointSizeChanged();
}

// An new style was set  to current item
void AppearanceForm::styleSelected(const QString &key )
{
    if (key.lower() == mPrefs->styleKey.lower())
	return;
    mPrefs->styleChgd = true;
    mPrefs->styleKey = key;
    ImodPrefs->changeStyle(key);
}

// When the window is closing, inform the preference manager
void AppearanceForm::destroy()
{
    ImodPrefs->userCanceled();
}
