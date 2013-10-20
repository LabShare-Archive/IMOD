/*
 *  form_appearance.h - Class for appearance tab of options window
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "form_appearance.h"

#include <qvariant.h>
#include <qstringlist.h>
#include <qstyle.h>
#include <qfontdialog.h>
#include <qstylefactory.h>
#include <qcolordialog.h>
#include <qimage.h>
#include <qpixmap.h>

#include "preferences.h"
#include "dia_qtutils.h"
#include "imod.h"
#include "info_cb.h"

/*
 *  Constructs a AppearanceForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
AppearanceForm::AppearanceForm(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
AppearanceForm::~AppearanceForm()
{
  destroy();
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void AppearanceForm::languageChange()
{
  retranslateUi(this);
}


// Initialize
void AppearanceForm::init()
{
  mPrefs = ImodPrefs->getDialogPrefs();
  mZoomIndex = MAXZOOMS / 3;
  zoomIndexSpinBox->setMaximum(MAXZOOMS);
  diaSetSpinBox(zoomIndexSpinBox, mZoomIndex + 1);
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
    styleComboBox->addItem(*it);
    if ((*it).toLower() == mPrefs->styleKey.toLower())
      styleComboBox->setCurrentIndex(ind);
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
    styleComboBox->addItem(str);
    if (str.toLower() == mPrefs->styleKey.toLower())
      styleComboBox->setCurrentIndex(ind);
    ind++;
  }
#endif
  
  if (App->cvi->fakeImage || App->cvi->rgbStore) {
    setTargetButton->setEnabled(false);
    autoMeanSpinBox->setEnabled(false);
    autoSDspinBox->setEnabled(false);
  }
  setFontDependentWidths();
}


void AppearanceForm::setFontDependentWidths()
{
  int width = (6 * 2 + 3) * fontMetrics().width("999999") / (6 * 2);
  zoomEdit->setMaximumWidth(width);
}

// Update the dialog based on current values
void AppearanceForm::update()
{
  diaSetSpinBox(imagePtSpinBox, mPrefs->minImPtSize);
  diaSetSpinBox(modelPtSpinBox, mPrefs->minModPtSize);
  diaSetSpinBox(autoMeanSpinBox, mPrefs->autoTargetMean);
  diaSetSpinBox(autoSDspinBox, mPrefs->autoTargetSD);
  diaSetSpinBox(voxLimitSpinBox, mPrefs->slicerPanKb);
  diaSetChecked(limitSliderBox, mPrefs->speedupSlider);
  diaSetChecked(isoHighBox, mPrefs->isoHighThresh);
  diaSetSpinBox(isoBoxLimitSpin, mPrefs->isoBoxLimit);
  diaSetSpinBox(isoBoxInitSpin, mPrefs->isoBoxInitial);
  displayCurrentZoom();
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

// Marker color button has been pressed: get color dialog and set color
void AppearanceForm::markerColorClicked()
{
  int indexes[] = {IMOD_CURPOINT, IMOD_BGNPOINT, IMOD_ENDPOINT, IMOD_FOREGROUND,
                   IMOD_BACKGROUND, IMOD_GHOST, IMOD_SHADOW, IMOD_ARROW};
  int item, which;
  item = indexes[colorComboBox->currentIndex()];
  for (int i = 0; i < MAX_NAMED_COLORS; i++)
    if (mPrefs->namedIndex[i] == item)
      which = i;
  QColor ret = QColorDialog::getColor(mPrefs->namedColor[which]);
  if (!ret.isValid())
    return;
  mPrefs->namedColor[which] = ret.rgb();
  ImodPrefs->pointSizeChanged();
}

// Target Mean or SD for autocontrast has changed: set and display
void AppearanceForm::autoMeanChanged( int value )
{
  mPrefs->autoTargetMean = value;
  imodInfoAutoContrast(mPrefs->autoTargetMean,  mPrefs->autoTargetSD);
}

void AppearanceForm::autoSDChanged( int value )
{
  mPrefs->autoTargetSD = value;
  imodInfoAutoContrast(mPrefs->autoTargetMean,  mPrefs->autoTargetSD);
}

// An new style was set  to current item
void AppearanceForm::styleSelected(const QString &key )
{
  if (key.toLower() == mPrefs->styleKey.toLower())
    return;
  mPrefs->styleChgd = true;
  mPrefs->styleKey = key;
  ImodPrefs->changeStyle(key);
}

// Get the current mean and Sd and use to set the targets
void AppearanceForm::setTargetClicked()
{
  float imageMean, imageSD, scaleLo, scaleHi;
  int targetMean, targetSD;
  int range = App->cvi->white - App->cvi->black;
  
  // Get current mean and SD, compute the target from current B/W settings
  if (imodInfoCurrentMeanSD(imageMean, imageSD, scaleLo, scaleHi))
    return;
  if (range <= 0)
    range = 1;
  targetMean = (int)(255. * (imageMean - App->cvi->black) / range + 0.5);
  targetSD = (int)(255. * imageSD / range - 0.5);
  
  // Set the spin boxes, then unload values back to preferences
  diaSetSpinBox(autoMeanSpinBox, targetMean);
  diaSetSpinBox(autoSDspinBox, targetSD);
  mPrefs->autoTargetMean = autoMeanSpinBox->value();
  mPrefs->autoTargetSD = autoSDspinBox->value();
}

// General unload function
void AppearanceForm::unload()
{
  mPrefs->slicerPanKb = voxLimitSpinBox->value();
  mPrefs->speedupSlider = limitSliderBox->isChecked();
  mPrefs->isoHighThresh = isoHighBox->isChecked();
  mPrefs->isoBoxLimit = isoBoxLimitSpin->value();
  mPrefs->isoBoxInitial = isoBoxInitSpin->value();
  unloadZoomValue();
}

void AppearanceForm::displayCurrentZoom()
{
  QString str;
  double zoom = mPrefs->zooms[mZoomIndex];
  str.sprintf("%.4f", zoom);
  if (str.endsWith("00"))
    str.truncate(str.length() - 2);
  zoomEdit->setText(str);
  str.sprintf("Default %.4f", mPrefs->zoomsDflt[mZoomIndex]);
  if (str.endsWith("00"))
    str.truncate(str.length() - 2);
  defaultZoomLabel->setText(str);
  mZoomValChanged = false;
}

void AppearanceForm::newZoomIndex( int value )
{
  unloadZoomValue();
  mZoomIndex = value - 1;
  displayCurrentZoom();
}

void AppearanceForm::unloadZoomValue()
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

void AppearanceForm::newZoomValue()
{
  mZoomValChanged = true;
}

void AppearanceForm::shiftZoomsDown()
{
  unloadZoomValue();
  for (int i = 0; i < MAXZOOMS - 1; i++)
    mPrefs->zooms[i] = mPrefs->zooms[i + 1];
  mPrefs->zoomsChgd = true;
  displayCurrentZoom();
}

void AppearanceForm::shiftZoomsUp()
{
  unloadZoomValue();
  for (int i = MAXZOOMS - 1; i > 0; i--)
    mPrefs->zooms[i] = mPrefs->zooms[i - 1];
  mPrefs->zoomsChgd = true;
  displayCurrentZoom();
}

void AppearanceForm::restoreDefaultZooms()
{
  for (int i = 0; i < MAXZOOMS; i++)
    mPrefs->zooms[i] = mPrefs->zoomsDflt[i];
  mPrefs->zoomsChgd = true;
  displayCurrentZoom();
}

// When the window is closing, inform the preference manager
void AppearanceForm::destroy()
{
  ImodPrefs->userCanceled();
}

