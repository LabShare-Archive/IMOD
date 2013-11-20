/*
 *  form_startup.cpp - Class for startup dialog form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "form_startup.h"

#include <qvariant.h>
#include <stdlib.h>
#include <string.h>
#include <qregexp.h>
#include <qstring.h>
#include <qstringlist.h>
#include <qdir.h>
#include <qimage.h>
#include <qpixmap.h>

#include "imod.h"
#include "imodplug.h"
#include "dia_qtutils.h"

/*
 *  Constructs a StartupForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 *  The dialog will by default be modeless, unless you set 'modal' to
 *  true to construct a modal dialog.
 */
StartupForm::StartupForm(QWidget* parent, bool modal, Qt::WindowFlags fl)
  : QDialog(parent, fl)
{
  setupUi(this);

  setModal(modal);
  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
StartupForm::~StartupForm()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void StartupForm::languageChange()
{
  retranslateUi(this);
}


void StartupForm::init()
{
  // Do NOT Delete on close, imod needs to access the arguments
  setAttribute(Qt::WA_AlwaysShowToolTips);

  // Set up radio buttons and other defaults
  startAsGroup = new QButtonGroup(this);
  startAsGroup->addButton(start3dmodRadio, 0);
  startAsGroup->addButton(startModvRadio, 1);
  connect(startAsGroup, SIGNAL(buttonClicked(int)), this, 
          SLOT(startAsClicked(int)));
  diaSetGroup(startAsGroup, 0);
  
  cacheGroup = new QButtonGroup(this);
  cacheGroup->addButton(cacheSectionsButton, 0);
  cacheGroup->addButton(megabyteButton, 1);
  connect(cacheGroup, SIGNAL(buttonClicked(int)), this, 
          SLOT(cacheTypeClicked(int)));
  diaSetGroup(cacheGroup, 0);
  
  windowSizeGroup = new QButtonGroup(this);
  windowSizeGroup->addButton(defaultSizeButton, 0);
  windowSizeGroup->addButton(fullScreenButton, 1);
  windowSizeGroup->addButton(setSizeButton, 2);
  connect(windowSizeGroup, SIGNAL(buttonClicked(int)), this, 
          SLOT(modvSizeClicked(int)));
  diaSetGroup(windowSizeGroup, 0);
  
  mModvMode = false;
  mShowMontage = false;
  mCacheOption = 0;
  mModvSizeOption = 0;
  openZapBox->setChecked(true);
  binXYSpinBox->setValue(1);
  binZSpinBox->setValue(1);
  
  manageForModView();
  
  mFilesChanged = false;
  mPiecesChanged = false;
  mArgv = NULL;
  mArgc = 0;
  mJoinedWithSpace = false;
  mPieceJoinedwSpace = false;
  adjustSize();
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
  angleFileLabel->setEnabled(!mModvMode);
  angleFileEdit->setEnabled(!mModvMode);
  angleSelectButton->setEnabled(!mModvMode);
  useMdocBox->setEnabled(!mModvMode);
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
  tileCacheBox->setEnabled(!mModvMode);
  imagePyramidBox->setEnabled(!mModvMode);
  flipCheckBox->setEnabled(!mModvMode);
  loadUshortBox->setEnabled(!mModvMode);
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
  
  winSizeGroupBox->setEnabled(mModvMode);
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
  mPiecesChanged = true;
  mPieceFiles = pfile;
}

void StartupForm::angleFileChanged( const QString &afile )
{
  mAngleFile = afile;
}

// Get name(s) from file dialog when Select button pressed
void StartupForm::imageSelectClicked()
{

  // Get current directory string for removal; on Windows, set up with both cases
  // of the drive letter (current dir comes through as lower case and the file dialog
  // returns upper case)
  QString curDir = QDir::currentPath() + QString("/");
  QString curDir2;
  bool replace2 = false;

#ifdef _WIN32
  if (curDir.indexOf(':') == 1) {
    replace2 = true;
    QChar start = curDir[0];
    QChar toggle = start.toUpper();
    if (toggle == start)
      toggle = start.toLower();
    curDir2 = curDir;
    curDir2.replace(0, 1, toggle);
  }
#endif

  enableButtons(false);
  mImageFileList = imodPlugGetOpenNames
    (this,  mModvMode ? "Select model file(s) to load" :
     "Select image file(s) to load", QString(),
     mModvMode ? "Model files (*.*mod)" : 
     "MRC files (*.*st *.*ali *.*rec* *.*mrc *.*join);;TIFF files (*.tif);;"
     "JPEG files (*.jpg);;PNG files (*.png);;All files (*)");
  enableButtons(true);
  
  // Join the list with ; and see if there are any spaces.  If not rejoin with spaces
  // Remove current directory to shorten string; the text box is limited to 32767 chars
  mImageFiles = mImageFileList.join(";");
  mJoinedWithSpace = false;
  mImageFiles.remove(curDir);
  if (replace2)
    mImageFiles.remove(curDir2);

  if (mImageFiles.indexOf(' ') < 0) {
    mImageFiles = mImageFileList.join(" ");
    mImageFiles.remove(curDir);
    if (replace2)
      mImageFiles.remove(curDir2);
    mJoinedWithSpace = true;
  }

  if (mImageFiles.length() > 32760)
    dia_err("WARNING: The file list will be truncated in the Image file(s) edit box.\n"
            "All files should load OK if you do not try to edit the list.");

  diaSetEditText(imageFilesEdit, mImageFiles);
  mFilesChanged = false;
}

void StartupForm::modelSelectClicked()
{
  const char *filter[] = {"Model files (*.*mod *.fid)"};
  enableButtons(false);
  mModelFile = utilOpenFileName(this, "Select model file to load", 1, filter);
  enableButtons(true);
  diaSetEditText(modelFileEdit, mModelFile);
}

void StartupForm::pieceSelectClicked()
{
  enableButtons(false);
  mPieceFileList = imodPlugGetOpenNames(this, "Select piece list file(s) to load",
                                        QString(),
                                        "Piece list files (*.pl *.mdoc);;All files (*)");
  enableButtons(true);
  loadFileList(mPieceFileList, mPieceFiles, pieceFileEdit, mPieceJoinedwSpace);
  mPiecesChanged = false;
}

void StartupForm::angleSelectClicked()
{
  const char *filter[] = {"Tilt angle files (*tlt)"};
  enableButtons(false);
  mAngleFile = utilOpenFileName(this, "Select angle file to load", 1, filter);
  enableButtons(true);
  diaSetEditText(angleFileEdit, mAngleFile);
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
  int fromVal, toVal, xybin, zbin, ind;
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
  addArg("-I");
  addArg(loadUshortBox->isChecked() ? "1" : "0");
  if (useMdocBox->isChecked())
    addArg("-pm");
  if (imagePyramidBox->isChecked())
    addArg("-py");
  
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
  if (!cacheSizeEdit->text().isEmpty() || tileCacheBox->isChecked()) {
    int cacheSize = cacheSizeEdit->text().toInt();
    if (tileCacheBox->isChecked())
      addArg("-CT");
    else
      addArg("-C");
    sprintf(numstr, "%d%s", cacheSize, mCacheOption ? "M" : "");
    addArg(numstr);
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
  
  // Piece list files
  if (mPiecesChanged) {
    if (mPieceFiles.indexOf(';') < 0 && mPieceJoinedwSpace)
      mPieceFileList = mPieceFiles.split(" ", QString::SkipEmptyParts);
    else
      mPieceFileList = mPieceFiles.split(";", QString::SkipEmptyParts);
  }
  for (ind = 0; ind < mPieceFileList.count(); ind++) {
    addArg("-p");
    addArg(LATIN1(mPieceFileList[ind]));
  }
  
  // Angle file
  if (!angleFileEdit->text().isEmpty()) {
    addArg("-a");
    addArg(LATIN1(angleFileEdit->text()));
  }
  
  // Image files and model file
  addImageFiles();
  if (!modelFileEdit->text().isEmpty())
    addArg(LATIN1(modelFileEdit->text()));
  argc = mArgc;
  return mArgv;
}

void StartupForm::addImageFiles()
{
  QString path, file;
  int sepInd, i;
  // If text box was changed, need to recreate a string list
  // If the files do not contain a ;, and either were joined with space
  // or contain no directory names, split with space, otherwise split with ;
  if (mFilesChanged) {
    if (mImageFiles.indexOf(';') < 0 && 
        (mJoinedWithSpace || mImageFiles.indexOf(QRegExp("[\\\\/]"))))
      mImageFileList = mImageFiles.split(" ", QString::SkipEmptyParts);
    else 
      mImageFileList = mImageFiles.split(";", QString::SkipEmptyParts);
  }
  
  //Process list by the book
  QStringList list = mImageFileList;
  QStringList::Iterator it = list.begin();
  while( it != list.end() ) {
    
    // Split the file into a path and filename
    sepInd = (*it).lastIndexOf(QRegExp("[\\\\/]"));
    if (sepInd < 0) {
      path = "./";
      file = (*it).trimmed();
    } else {
      path = ((*it).left(sepInd + 1)).trimmed();
      file = ((*it).mid(sepInd + 1)).trimmed();
    }
    /*imodPrintStderr("sepInd %d  path =<%s>   file =<%s>\n", sepInd, LATIN1(path),
      LATIN1(file));*/
    // If there are no wildcards, just add the whole filename
    if (file.indexOf(QRegExp("[\\*\\?]")) < 0) {
      addArg(LATIN1((*it)));
    } else {
      
      // Otherwise get a file list that matches the filename
      QDir dir(path, file);
      for (i = 0; i < dir.count(); i++) {
        file = path +dir[i];
        addArg(LATIN1(file));
      }
    }
    ++it;
  }
}


void StartupForm::setValues(ImodView *vi, char **argv, int firstfile, int argc, 
                            int doImodv, QStringList &plFileNames, char *anglefname,
                            int useMdoc, int xyzwinopen, int sliceropen, int zapOpen,
                            int modelViewOpen, int fillCache, int ImodTrans, int mirror,
                            int frames, int nframex, int nframey, int overx, int overy,
                            int overEntered)
{
  int vms;

  // Imodv mode
  diaSetGroup(startAsGroup, doImodv);
  mModvMode = doImodv != 0;
  
  // Files. Join with ; then if there are no spaces, convert to space join
  if (firstfile && firstfile < argc) {
    for (int i = firstfile; i < argc; i++)
      mImageFileList << QString(argv[i]);
    loadFileList(mImageFileList, mImageFiles, imageFilesEdit, mJoinedWithSpace);
    if (mImageFiles.length() > 32760)
      dia_err("WARNING: The file list will be truncated in the Image file(s) edit box.\n"
              "All files should load OK if you do not try to edit the list.");
  }
  if (plFileNames.count()) {
    mPieceFileList = plFileNames;
    loadFileList(mPieceFileList, mPieceFiles, pieceFileEdit, mPieceJoinedwSpace);
  }
  if (anglefname)
    angleFileEdit->setText(QString(anglefname));
  useMdocBox->setChecked(useMdoc > 0);
  
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
  diaSetGroup(cacheGroup, vi->vmSize < 0 ? 1 : 0);
  vms = vi->vmSize;
  if (vms == HUGE_CACHE)
    vms = 0;
  if (vi->vmSize) {
    mStr.sprintf("%d", vms > 0 ? vms : -vms);
    cacheSizeEdit->setText(mStr);
  }
  
  // Miscellaneous flags
  tileCacheBox->setChecked(vi->stripOrTileCache > 0);
  imagePyramidBox->setChecked(vi->imagePyramid > 0);
  flipCheckBox->setChecked(vi->li->axis == 2);
  fillCacheBox->setChecked(fillCache > 0);
  showRGBGrayBox->setChecked(vi->grayRGBs > 0);
  loadSepTimesBox->setChecked(vi->multiFileZ < 0);
  loadFramesBox->setChecked(frames > 0);
  loadUnscaledBox->setChecked(ImodTrans == 0);
  loadNoMirrorBox->setChecked(mirror < 0);
  loadUshortBox->setChecked(vi->rawImageStore > 0);
  
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
void StartupForm::loadFileList(QStringList &fileList, QString &files,
                               QLineEdit *fileEdit, bool &joinedWithSpace)
{
  files = fileList.join(";");
  if (files.indexOf(' ') < 0) {
    files.replace(';', " ");
    joinedWithSpace = true;
  }
  diaSetEditText(fileEdit, files);
}

void StartupForm::helpClicked()
{
  imodShowHelpPage("startup.html#TOP");
}
