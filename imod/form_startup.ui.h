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
    mCacheOption = 0;
    mModvSizeOption = 0;
    startAsGroup->setButton(0);
    cacheGroup->setButton(0);
    windowSizeGroup->setButton(0);
    openZapBox->setChecked(true);
    
    manageForModView();
    
}

void StartupForm::manageForModView()
{
   imageFilesLabel->setText(mModvMode ? "Model file(s)" : "Image file(s):");
   modelFileLabel->setEnabled(!mModvMode);
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
   xMontageSpinBox->setEnabled(!mModvMode);
   xMontageLabel->setEnabled(!mModvMode);
   xOverlapLabel->setEnabled(!mModvMode);
   xOverlapSpinBox->setEnabled(!mModvMode);
   yMontageSpinBox->setEnabled(!mModvMode);
   yMontageLabel->setEnabled(!mModvMode);
   yOverlapLabel->setEnabled(!mModvMode);
   yOverlapSpinBox->setEnabled(!mModvMode);
   
   windowSizeGroup->setEnabled(mModvMode);
   defaultSizeButton->setEnabled(mModvMode);
   fullScreenButton->setEnabled(mModvMode);
   setSizeButton->setEnabled(mModvMode);
   xSizeSpinBox->setEnabled(mModvMode);
   ySizeSpinBox->setEnabled(mModvMode);
   ySizeLabel->setEnabled(mModvMode);
}


void StartupForm::modelViewSlot()
{

}
