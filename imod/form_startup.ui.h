/****************************************************************************
** ui.h extension file, included from the uic-generated form implementation.
**
** If you wish to add, delete or rename slots use Qt Designer which will
** update this file, preserving your code. Create an init() slot in place of
** a constructor, and a destroy() slot in place of a destructor.
*****************************************************************************/


void StartupForm::init()
{
    // Set up radio buttons and other defaults
    mModvMode = false;
    mShowMOntage = false;
    mCacheOption = 0;
    mModvSizeOption = 0;
    startAsGroup->setButton(0);
    cacheGroup->setButton(0);
    windowSizeGroup->setButton(0);
    openZapBox->setChecked(true);
    
    manageForModView();
    
    mFilesChanged = false;
}

void StartupForm::manageForModView()
{
   imageFilesLabel->setText(mModvMode ? "Model file(s)" : "Image file(s):");
   modelFileLabel->setEnabled();
   modelFileEdit->setEnabled(!mModvMode);
   modelSelectButton->setEnabled(!mModvMode);
   pieceFileLabel->setEnabled(!mModvMode);
   pieceFileEdit->setEnabled(!mModvMode);
   pieceSelectButton->setEnabled(!mModvMode);
   subsetsBox->setEnabled(!mModvMode);
   xMinLabel->setEnabled(!mModvMode);
   xFromEdit->setEnabled(!mModvMode);
   xToLabel->setEnabled(!mModvMode);
   xToEdit->setEnabled(!mModvMode);
   yMinLabel->setEnabled(!mModvMode);
   yFromEdit->setEnabled(!mModvMode);
   yToLabel->setEnabled(!mModvMode);
   yToEdit->setEnabled(!mModvMode);
   zMinLabel->setEnabled(!mModvMode);
   zFromEdit->setEnabled(!mModvMode);
   zToLabel->setEnabled(!mModvMode);
   zToEdit->setEnabled(!mModvMode);
   openWhichBox->setEnabled(!mModvMode);
   openZapBox->setEnabled(!mModvMode);
   openXYZBox->setEnabled(!mModvMode);
   openSlicerBox->setEnabled(!mModvMode);
   openModvBox->setEnabled(!mModvMode);
   scaleImageLabel->setEnabled(!mModvMode);
   scaleFromEdit->setEnabled(!mModvMode);
   to0Label->setEnabled(!mModvMode);
   to255Label->setEnabled(!mModvMode);
   scaleToEdit->setEnabled(!mModvMode);
   useCacheLabel->setEnabled(!mModvMode);
   cacheSizeEdit->setEnabled(!mModvMode);
   cacheSectionsButton->setEnabled(!mModvMode);
   megabyteButton->setEnabled(!mModvMode);
   flipCheckBox->setEnabled(!mModvMode);
   fillCacheBox->setEnabled(!mModvMode);
   showRGBGrayBox->setEnabled(!mModvMode);
   loadFramesBox->setEnabled(!mModvMode);
   loadUnscaledBox->setEnabled(!mModvMode);
   showMontageBox->setEnabled(!mModvMode);
   xMontageLabel->setEnabled(!mModvMode);
   yMontageLabel->setEnabled(!mModvMode);
   manageMontage();
   
   windowSizeGroup->setEnabled(mModvMode);
   defaultSizeButton->setEnabled(mModvMode);
   fullScreenButton->setEnabled(mModvMode);
   setSizeButton->setEnabled(mModvMode);
   ySizeLabel->setEnabled(mModvMode);
   manageModvSize();
}



void StartupForm::manageMontage()
{
    bool enable = !mModvMode && mShowMontage;
    xMontageSpinBox->setEnabled(enable);
   xOverlapLabel->setEnabled(enable);
   xOverlapSpinBox->setEnabled(enable);
   yMontageSpinBox->setEnabled(enable);
   yOverlapLabel->setEnabled(enable);
   yOverlapSpinBox->setEnabled(enable);
}

void StartupForm::manageModvSize()
{
    xSizeSpinBox->setEnabled(mModvMode && mModvOption == 2);
    ySizeSpinBox->setEnabled(mModvMode && mModvOption == 2);
}


void StartupForm::modvSizeClicked( int id )
{
    mModvSizeOption = id;
    manageModvSize();
}

void StartupForm::cacheTypeClicked( int id )
{
    mCacheOption = id;
}

void StartupForm::showMontageToggled( bool state )
{
    mShowMontage = state;
    manageMontage();
}

void StartupForm::startAsClicked( int id )
{
    mModvMode = id != 0;
    manageForModView();
}


void StartupForm::imageChanged( const QString & images )
{

}

void StartupForm::modelChanged( const QString & model )
{

}

void StartupForm::pfileChanged( const QString & pfile )
{

}

void StartupForm::imageSelectClicked()
{

}

void StartupForm::modelSelectClicked()
{

}

void StartupForm::pieceSelectClicked()
{

}
