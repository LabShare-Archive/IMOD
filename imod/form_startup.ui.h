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
    mShowMontage = false;
    mCacheOption = 0;
    mModvSizeOption = 0;
    startAsGroup->setButton(0);
    cacheGroup->setButton(0);
    windowSizeGroup->setButton(0);
    openZapBox->setChecked(true);
    binXYSpinBox->setValue(1);
    binZSpinBox->setValue(1);
    
    manageForModView();
    
    mFilesChanged = false;
    mArgv = NULL;
    mArgc = 0;
    mJoinedWithSpace = true;
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
   binByLabel->setEnabled(!mModvMode);
   binByXYLabel->setEnabled(!mModvMode);
   binByZLabel->setEnabled(!mModvMode);
   binXYSpinBox->setEnabled(!mModvMode);
   binZSpinBox->setEnabled(!mModvMode);
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
   loadSepTimesBox->setEnabled(!mModvMode);
   loadUnscaledBox->setEnabled(!mModvMode);
   loadNoMirrorBox->setEnabled(!mModvMode);
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
    xSizeSpinBox->setEnabled(mModvMode && mModvSizeOption == 2);
    ySizeSpinBox->setEnabled(mModvMode && mModvSizeOption == 2);
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

// Track changes in the text boxes for file names
void StartupForm::imageChanged( const QString & images )
{
    mFilesChanged = true;
    mImageFiles = images;
}

void StartupForm::modelChanged( const QString & model )
{
    mModelFile = model;
}

void StartupForm::pfileChanged( const QString & pfile )
{
    mPieceFile = pfile;
}

// Get name(s) from file dialog when Select button pressed
void StartupForm::imageSelectClicked()
{
    enableButtons(false);
    // 5/30/04: need to check if this is still compatible to 3.0.5
    mImageFileList = QFileDialog::getOpenFileNames
                     (mModvMode ? "Model files (*.*mod)" : 
                      "MRC files (*.*st *.*ali *.*rec *.*mrc);;Tiff files (*.tif);;All files (*)",
                      QString::null, 0, 0, 
		      mModvMode ? "Select model file(s) to load" :
		      "Select image file(s) to load");
    enableButtons(true);
    
    // Join the list with ; and see if there are any spaces.  If not rejoin with spaces
    mImageFiles = mImageFileList.join(";");
    mJoinedWithSpace = false;
    if (mImageFiles.find(' ') < 0) {
	mImageFiles = mImageFileList.join(" ");
	mJoinedWithSpace = true;
    }
    mFilesChanged = false;
    imageFilesEdit->setText(mImageFiles);
}

void StartupForm::modelSelectClicked()
{
    char *filter[] = {"Model files (*.*mod *.fid)"};
    enableButtons(false);
    mModelFile = diaOpenFileName(this, "Select model file to load", 1, filter);
    enableButtons(true);
    modelFileEdit->setText(mModelFile);
}

void StartupForm::pieceSelectClicked()
{
    char *filter[] = {"Piece list files (*.pl)"};
    enableButtons(false);
    mPieceFile = diaOpenFileName(this, "Select piece list file to load", 1, filter);
    enableButtons(true);
    pieceFileEdit->setText(mPieceFile);
}

// A workaround to problem in Windows of file dialogs not being modal
void StartupForm::enableButtons( bool enable )
{
    bool block = !enable;
    OKButton->blockSignals(block);
    cancelButton->blockSignals(block);
    imageSelectButton->blockSignals(block);
    modelSelectButton->blockSignals(block);
    pieceSelectButton->blockSignals(block);
}

// Add a single argument, getting memory as needed and bailing out silently
// if the mallocs fail
void StartupForm::addArg( const char *arg )
{
    if (mArgc)
	mArgv = (char **)realloc(mArgv, sizeof(char *) * (mArgc + 1));
    else 
	mArgv = (char **)malloc(sizeof(char *));
    if (!mArgv) {
	mArgc = 0;
	return;
    }
    mArgv[mArgc] = strdup(arg);
    if (mArgv[mArgc])
	mArgc++;
}

// Return all arguments to the program in an argument vector
char ** StartupForm::getArguments( int & argc )
{
    char numstr[32];
    int fromVal, toVal, xybin, zbin;
    if (mModvMode) {
	
	// Model mode: add v for it, then size options
	addArg("3dmodv");
	if (mModvSizeOption == 1)
	    addArg("-f");
	else if (mModvSizeOption == 2) {
	    addArg("-s");
	    sprintf(numstr, "%d,%d", xSizeSpinBox->value(), ySizeSpinBox->value());
	    addArg(numstr);
	}
	
	// Add the "image" files and return options
	addImageFiles();
	argc = mArgc;
	return mArgv;
    }
    
    // Do all the easy ones first
    addArg("3dmod");
    if (openZapBox->isChecked())
	addArg("-Z");
    if (openXYZBox->isChecked())
	addArg("-xyz");
    if (openSlicerBox->isChecked())
	addArg("-S");
    if (openModvBox->isChecked())
	addArg("-V");
    if (flipCheckBox->isChecked())
	addArg("-Y");
    if (fillCacheBox->isChecked())
	addArg("-F");
    if (showRGBGrayBox->isChecked())
	addArg("-G");
    if (loadFramesBox->isChecked())
	addArg("-f");
    if (loadSepTimesBox->isChecked())
	addArg("-T");
    if (loadUnscaledBox->isChecked())
	addArg("-m");
    if (loadNoMirrorBox->isChecked())
	addArg("-M");
    
    // Binning
    xybin = binXYSpinBox->value();
    zbin = binZSpinBox->value();
    if (xybin * zbin != 1) {
        addArg("-b");
        sprintf(numstr, "%d,%d", xybin,zbin);
        addArg(numstr);
    }
    
    // Montage and overlap
    if (showMontageBox->isChecked()) {
	addArg("-P");
	sprintf(numstr, "%d,%d", xMontageSpinBox->value(), yMontageSpinBox->value());
	addArg(numstr);
	addArg("-o");
	sprintf(numstr, "%d,%d", xOverlapSpinBox->value(), yOverlapSpinBox->value());
	addArg(numstr);
    }
    
    // Cache
    if (!cacheSizeEdit->text().isEmpty()) {
	int cacheSize = cacheSizeEdit->text().toInt();
	if (cacheSize > 0) {
	    addArg("-C");
	    sprintf(numstr, "%d%s", cacheSize, mCacheOption ? "M" : "");
	    addArg(numstr);
	}
    }
    
    // Subsets
    if (!xFromEdit->text().isEmpty() || !xToEdit->text().isEmpty()) {
	fromVal = xFromEdit->text().isEmpty() ? -999 : xFromEdit->text().toInt();
	toVal = xToEdit->text().isEmpty() ? 65535 : xToEdit->text().toInt();
	addArg("-x");
	sprintf(numstr, "%d,%d", fromVal, toVal);
	addArg(numstr);
    }
    
    if (!yFromEdit->text().isEmpty() || !yToEdit->text().isEmpty()) {
	fromVal = yFromEdit->text().isEmpty() ? -999 : yFromEdit->text().toInt();
	toVal = yToEdit->text().isEmpty() ? 65535 : yToEdit->text().toInt();
	addArg("-y");
	sprintf(numstr, "%d,%d", fromVal, toVal);
	addArg(numstr); 
    }
 
    if (!zFromEdit->text().isEmpty() || !zToEdit->text().isEmpty()) {
	fromVal = zFromEdit->text().isEmpty() ? -999 : zFromEdit->text().toInt();
	toVal = zToEdit->text().isEmpty() ? 65535 : zToEdit->text().toInt();
	addArg("-z");
	sprintf(numstr, "%d,%d",  fromVal, toVal);
	addArg(numstr);
    }

    // Intensities.  This is the one that requires both for now - need to fix this
    if (!scaleFromEdit->text().isEmpty() && !scaleToEdit->text().isEmpty()) {
	fromVal = scaleFromEdit->text().toInt();
	toVal = scaleToEdit->text().toInt();
	addArg("-s");
	sprintf(numstr, "%d,%d",  fromVal, toVal);
	addArg(numstr);
    }
    
    // Piece list file
    if (!pieceFileEdit->text().isEmpty()) {
	addArg("-p");
	addArg(pieceFileEdit->text().latin1());
    }
    
    // Image files and model file
    addImageFiles();
     if (!modelFileEdit->text().isEmpty())
	addArg(modelFileEdit->text().latin1());
     argc = mArgc;
     return mArgv;
}

void StartupForm::addImageFiles()
{
    // If text box was changed, need to recreate a string list
    // If they were joined with ; or contain a ;, split with that
    if (mFilesChanged) {
	if (mJoinedWithSpace && mImageFiles.find(';') < 0)
	    mImageFileList = QStringList::split(" ", mImageFiles);
 	else 
	    mImageFileList = QStringList::split(";", mImageFiles);
     }
    
    //Process list by the book
    QStringList list = mImageFileList;
    QStringList::Iterator it = list.begin();
    while( it != list.end() ) {
        addArg((*it).latin1());
        ++it;
    }
}


void StartupForm::setValues( ImodView *vi, char * *argv, int firstfile, int argc, 
                             int doImodv, char *plistfname, int xyzwinopen, int sliceropen, 
                             int zapOpen, int modelViewOpen, int fillCache, int ImodTrans, 
                             int mirror, int frames, int nframex, int nframey, 
                             int overx, int overy, int overEntered )
{
    // Imodv mode
    startAsGroup->setButton(doImodv);
    mModvMode = doImodv != 0;
    
    // Files
    if (firstfile && firstfile < argc) {
        mImageFiles = "";
        for (int i = firstfile; i < argc; i++)
            mImageFiles += QString(argv[i]) + " ";
        imageFilesEdit->setText(mImageFiles);
    }
    if (plistfname)
        pieceFileEdit->setText(QString(plistfname));
    
    // X, Y, Z subset
    if (vi->li->xmin != -1 || vi->li->xmax != -1) {
        mStr.sprintf("%d", vi->li->xmin);
        xFromEdit->setText(mStr);
        mStr.sprintf("%d", vi->li->xmax);
        xToEdit->setText(mStr);
    }
    if (vi->li->ymin != -1 || vi->li->ymax != -1) {
        mStr.sprintf("%d", vi->li->ymin);
        yFromEdit->setText(mStr);
        mStr.sprintf("%d", vi->li->ymax);
        yToEdit->setText(mStr);
    }
    if (vi->li->zmin != -1 || vi->li->zmax != -1) {
        mStr.sprintf("%d", vi->li->zmin);
        zFromEdit->setText(mStr);
        mStr.sprintf("%d", vi->li->zmax);
        zToEdit->setText(mStr);
    }
    
    // Binning
    binXYSpinBox->setValue(vi->xybin);
    binZSpinBox->setValue(vi->zbin);
    
    //Intensities
    if (vi->li->smin != vi->li->smax) {
        mStr.sprintf("%g", vi->li->smin);
        scaleFromEdit->setText(mStr);
        mStr.sprintf("%g", vi->li->smax);
        scaleToEdit->setText(mStr);
    }

    //Windows to open
    openZapBox->setChecked(zapOpen > 0);
    openXYZBox->setChecked(xyzwinopen > 0);
    openSlicerBox->setChecked(sliceropen > 0);
    openModvBox->setChecked(modelViewOpen > 0);

    // Caching
    cacheGroup->setButton(vi->vmSize < 0 ? 1 : 0);
    if (vi->vmSize) {
        mStr.sprintf("%d", vi->vmSize > 0 ? vi->vmSize : -vi->vmSize);
        cacheSizeEdit->setText(mStr);
    }
    
    // Miscellaneous flags
    flipCheckBox->setChecked(vi->li->axis == 2);
    fillCacheBox->setChecked(fillCache > 0);
    showRGBGrayBox->setChecked(vi->grayRGBs > 0);
    loadSepTimesBox->setChecked(vi->multiFileZ < 0);
    loadFramesBox->setChecked(frames > 0);
    loadUnscaledBox->setChecked(ImodTrans == 0);
    loadNoMirrorBox->setChecked(mirror < 0);
    
    // Montage stuff
    mShowMontage = nframex > 0;
    if (mShowMontage) {
    showMontageBox->setChecked(true);
        xMontageSpinBox->setValue(nframex);
       yMontageSpinBox->setValue(nframey);
   }
    if (overEntered) {
        xOverlapSpinBox->setValue(overx);
        yOverlapSpinBox->setValue(overy);
    }
    manageForModView();    
}
