/*
 *  form_object_edit.cpp - Class for object edit dialog form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "form_object_edit.h"

#include <qvariant.h>
#include <stdlib.h>
#include <qimage.h>
#include <qpixmap.h>

//Added by qt3to4:
#include <QKeyEvent>
#include <QCloseEvent>

#include "imod.h"
#include "control.h"
#include "object_edit.h"
#include "dia_qtutils.h"
#include "preferences.h"

/*
 *  Constructs a objectEditForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
objectEditForm::objectEditForm(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);

  typeButtonGroup = new QButtonGroup(this);
  typeButtonGroup->addButton(closedButton, 0);
  typeButtonGroup->addButton(openButton, 1);
  typeButtonGroup->addButton(scatteredButton, 2);
  connect(typeButtonGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(selectedType(int)));

  surfaceButtonGroup = new QButtonGroup(this);
  surfaceButtonGroup->addButton(outsideButton, 0);
  surfaceButtonGroup->addButton(insideButton, 1);
  connect(surfaceButtonGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(selectedSurface(int)));

  connect(copyObjButton, SIGNAL(clicked()), this, SLOT(copyClicked()));
  connect(setDefaultsButton, SIGNAL(clicked()), this, SLOT(setDefaultsClicked()));
  connect(restoreDefaultsButton, SIGNAL(clicked()), this, SLOT(restoreClicked()));

  connect(transSlider, SIGNAL(valueChanged(int)), this, SLOT(transChanged(int)));
  connect(outlineCheckBox, SIGNAL(toggled(bool)), this, SLOT(toggledOutline(bool)));
  connect(arrowCheckBox, SIGNAL(toggled(bool)), this, SLOT(toggledArrowAtEnd(bool)));
}

/*
 *  Destroys the object and frees any allocated resources
 */
objectEditForm::~objectEditForm()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void objectEditForm::languageChange()
{
  retranslateUi(this);
}

// Respond to signals from the widgets
void objectEditForm::helpPressed()
{
  ioew_help();
}

void objectEditForm::nameChanged( const QString & newName )
{
  ioew_nametext(LATIN1(newName));
}

void objectEditForm::symbolChanged( int value )
{
  ioew_symbol(value);
}

void objectEditForm::OKPressed()
{
  ioew_quit();
}

void objectEditForm::radiusChanged(int value)
{
  ioew_pointsize(value);
}

void objectEditForm::ptLimitChanged( int value )
{
  ioew_pointLimit(value);
}

void objectEditForm::selectedSurface( int value )
{
  ioew_surface(value);
}

void objectEditForm::selectedType( int value )
{
  ioew_open(value);
}

void objectEditForm::sizeChanged( int value )
{
  QString str;
  str.sprintf("%d", value);
  sizeLabel->setText(str);
  ioew_symsize(value);
}

void objectEditForm::toggledDraw( bool state )
{
  ioew_draw(state ? 1 : 0);
}

void objectEditForm::toggledFill( bool state )
{
  ioew_fill(state ? 1 : 0);
}

void objectEditForm::toggledMarkEnds( bool state )
{
  ioew_ends(state ? 1 : 0);
}

void objectEditForm::toggledArrowAtEnd( bool state )
{
  ioew_arrow(state ? 1 : 0);
}

void objectEditForm::toggledTime( bool state )
{
  ioew_time(state ? 1 : 0);
}

void objectEditForm::toggledOnSection( bool state )
{
  ioew_sphere_on_sec(state ? 1 : 0);
}

void objectEditForm::widthChanged( int value )
{
  ioew_linewidth(value);
}	

void objectEditForm::toggledPlanar( bool state )
{
  ioew_planar(state ? 1 : 0);
}

void objectEditForm::transChanged( int value )
{
  ioew_fillTrans(value);
}

void objectEditForm::toggledOutline( bool state )
{
  ioew_outline(state ? 1 : 0);
}

void objectEditForm::copyClicked()
{
  ioewCopyObj(copyObjSpinBox->value());
}

void objectEditForm::setDefaultsClicked()
{
  ImodPrefs->setDefaultObjProps();
}

void objectEditForm::restoreClicked()
{
  ImodPrefs->restoreDefaultObjProps();
}

// Set the state of the widgets initially or when a new object is selected
void objectEditForm::setSymbolProperties( int which, bool fill, bool markEnds, 
                                          bool arrowAtEnd, int size )
{
  symbolComboBox->setCurrentIndex(which);
  diaSetChecked(fillCheckBox, fill);
  diaSetChecked(markCheckBox, markEnds);
  diaSetChecked(arrowCheckBox, arrowAtEnd);
  diaSetSlider(sizeSlider, size);
  QString str;
  str.sprintf("%d", size);
  sizeLabel->setText(str);
}

  
void objectEditForm::setCopyObjLimit( int value )
{
  int curval = copyObjSpinBox->value();
  curval = B3DMIN(curval, value);
  diaSetSpinMMVal(copyObjSpinBox, 1, value, curval);
  copyObjSpinBox->setEnabled(value > 1);
}

void objectEditForm::setDrawBox( bool state )
{
  diaSetChecked(drawCheckBox, state);
}

void objectEditForm::setObjectName( char *name )
{
  QString str = name;
  diaSetEditText(nameEdit, str);
}

void objectEditForm::setObjectNum( int num )
{
  QString str;
  str.sprintf("# %d", num + 1);
  objectNumLabel->setText(str);
}

void objectEditForm::setTimeBox( bool state, bool enabled )
{
  diaSetChecked(timeCheckBox, state);
  timeCheckBox->setEnabled(enabled);
}

void objectEditForm::setOnSecBox( bool state )
{
  diaSetChecked(onSecCheckBox, state);
}

void objectEditForm::setPointRadius( int value )
{
  diaSetSpinBox(radiusSpinBox, value);
}

void objectEditForm::setFrontSurface( int value )
{
  diaSetGroup(surfaceButtonGroup, value);
}

void objectEditForm::setObjectType( int value )
{
  diaSetGroup(typeButtonGroup, value);
}

void objectEditForm::setLineWidth( int value )
{
  diaSetSpinBox(widthSpinBox, value);
}

void objectEditForm::setPlanarBox( bool state, bool enabled )
{
  diaSetChecked(planarCheckBox, state);
  planarCheckBox->setEnabled(enabled);
}

void objectEditForm::setPointLimit( int value )
{
  diaSetSpinBox(ptsPerContSpinBox, value);
}
 
void objectEditForm::setFillTrans(int value, bool state, bool enabled)
{
  diaSetSlider(transSlider, value);
  QString str;
  str.sprintf("%d", value);
  transLabel->setText(str);
  diaSetChecked(outlineCheckBox, state);
  fillLabel->setEnabled(enabled);
  transLabel->setEnabled(enabled);
  transSlider->setEnabled(enabled);
  outlineCheckBox->setEnabled(enabled);
}

// Handle close event; pass on keypress
void objectEditForm::closeEvent( QCloseEvent *e )
{
  ioew_closing();
  e->accept();
}

void objectEditForm::keyPressEvent( QKeyEvent * e )
{
  if (utilCloseKey(e))
    ioew_quit();
  else
    ivwControlKey(0, e);
  //e->ignore();
}

void objectEditForm::keyReleaseEvent( QKeyEvent * e )
{
  ivwControlKey(1, e);
}
