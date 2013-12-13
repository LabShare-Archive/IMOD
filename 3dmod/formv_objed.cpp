/*
 *  formv_objed.cpp - Class for model view object edit dialog form
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2009 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include "formv_objed.h"

#include <qvariant.h>
#include <qapplication.h>

//Added by qt3to4:
#include <QVBoxLayout>
#include <QKeyEvent>
#include <QCloseEvent>
#include <QStackedWidget>

#include "colorselector.h"
#include "preferences.h"
#include "mv_objed.h"
#include "mv_input.h"
#include "dia_qtutils.h"
#include "imod.h"

/*
 *  Constructs a imodvObjedForm as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
imodvObjedForm::imodvObjedForm(QWidget* parent, Qt::WindowFlags fl)
  : QWidget(parent, fl)
{
  setupUi(this);

  init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
imodvObjedForm::~imodvObjedForm()
{
  // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void imodvObjedForm::languageChange()
{
  retranslateUi(this);
}

// THIS COMPANION CLASS MANAGES ALL OF THE REGULAR WIDGETS IN
// THE IMODV OBJECT EDIT WINDOW, AND CALLS CALLBACK FUNCTIONS
// TO CREATE A STACK OF WIDGETS IN THE PANEL FRAME.  THOSE
// WIDGETS CONTAIN ELEMENTS MANAGED BY THE ImodvObjed CLASS

void imodvObjedForm::init()
{
  int i;
  int width = 0, height = 0;
  ObjectEditField *oef;
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
    
  mGLw = new ColorSelectorGL(&mBoxRGB[0], colorFrame);
    
  QVBoxLayout *panelFrameLayout = new QVBoxLayout(panelFrame);
  panelFrameLayout->setContentsMargins(4, 4, 4, 4);
  panelFrameLayout->setSpacing(6);
  mStack = new QStackedWidget(panelFrame);
  // Yes the stack has to be added to the layout too
  panelFrameLayout->addWidget(mStack);
    
  // Now create each widget and determine its size
  for  (i = 0; (objectEditFieldData[i].label); i++) {
    oef = &objectEditFieldData[i];
	
    // Add to the list box
    panelListBox->addItem(oef->label);
	
    // Start with the frame as the parent
    oef->control = new QWidget(panelFrame);
    oef->mkwidget(i);	
	
    // You have to put it in layout and assess geometry before putting in stack
    panelFrameLayout->addWidget(oef->control);
    QSize size = oef->control->sizeHint();
    if (width < size.width())
      width = size.width();
    if (height < size.height())
      height = size.height();
    mStack->addWidget(oef->control);
  }
    
  setFontDependentSizes(width, height);
  mStack->setCurrentIndex(0);
  connect(panelListBox, SIGNAL(currentRowChanged(int)), this,
          SLOT(frameSelected(int)));
    
  imodvObjedMakeOnOffs(checkBoxFrame);
  updateMeshing(-1);
}

void imodvObjedForm::setFontDependentSizes(int width, int height)
{
  // Set frame size for all widgets
  // 20 needed to be added with margin 11, 8 with margin 5
  panelFrame->setMinimumWidth(width + 8);   
  panelFrame->setMinimumHeight(height + 8);
    
  // Set size of list box:
  int wid, maxWidth = 0;
  for (int i = 0; i < panelListBox->count(); i++) {
    wid = panelListBox->fontMetrics().width(panelListBox->item(i)->text());
    if (maxWidth < wid)
      maxWidth = wid;
  }

  // Qt 4: this stretched out, so just set it to a fixed width
  //QSize hint = panelListBox->sizeHint();
  // 8/10/11: Increase both by 2 pixels, need to test on ITS lab computer
  panelListBox->setFixedWidth(maxWidth + 18);
  panelListBox->setFixedHeight(B3DNINT(1.1 * panelListBox->count() * 
                             panelListBox->fontMetrics().height() + 10));
  /*imodPrintStderr("panel max width %d  hint width %d  count *fontheight %d "
    "hint %d\n", maxWidth, hint.width(), panelListBox->count() *
    panelListBox->fontMetrics().height(), hint.height()); */
}

// Need to adjust and measure sizes of widgets on font change
void imodvObjedForm::changeEvent(QEvent *e)
{
  int i;
  int width = 0, height = 0;
  ObjectEditField *oef;
  QWidget::changeEvent(e);
  if (e->type() != QEvent::FontChange)
    return;
  for  (i = 0; (objectEditFieldData[i].label); i++) {
    oef = &objectEditFieldData[i];
    if (oef->fixwidget)
      oef->fixwidget();
	
    // Processing events makes the size hints good
    qApp->processEvents();
    QSize size = oef->control->sizeHint();
    if (width < size.width())
      width = size.width();
    if (height < size.height())
      height = size.height();
  }	
  setFontDependentSizes(width, height);
}

// Pass on changes in the various controls
void imodvObjedForm::objectSelected( int which )
{
  imodvObjedSelect(which);
}

void imodvObjedForm::editSelected( int item )
{
  imodvObjedEditData(item);
}

void imodvObjedForm::objSliderChanged( int value )
{
  imodvObjedSelect(value);
}

void imodvObjedForm::nameChanged(const QString & str )
{
  imodvObjedName(LATIN1(str));
}

void imodvObjedForm::typeSelected( int item )
{
  imodvObjedDrawData(item);
}

void imodvObjedForm::styleSelected( int item )
{
  imodvObjedStyleData(item);
}

// A new frame is selected from list - raise it and let objed update it
void imodvObjedForm::frameSelected( int item )
{
  mStack->setCurrentIndex(item);
  imodvObjedFramePicked(item);
}

// Done and help buttons
void imodvObjedForm::donePressed()
{
  imodvObjedDone();
}

void imodvObjedForm::helpPressed()
{
  imodvObjedHelp();
}

// This provides update information when a new object is set
void imodvObjedForm::updateObject(int ob, int numObj, int drawType, int drawStyle, 
				  QColor color, char *name)
{
  updateColorBox(color);
  diaSetSpinMMVal(objectSpinBox, 1, numObj, ob);
  objectSpinBox->setEnabled(numObj > 1);
  objectSlider->setMaximum(numObj);
  diaSetSlider(objectSlider, ob);
  objectSlider->setEnabled(numObj > 1);
  dataTypeComboBox->setCurrentIndex(drawType);
  drawStyleComboBox->setCurrentIndex(drawStyle);
  QString str = name;
  diaSetEditText(nameLineEdit, str);
}

void imodvObjedForm::updateMeshing(int ob)
{
  QString str;
  if (ob < 0) {
    meshingLabel1->setText("");
    meshingLabel2->setText("");
  } else {
    str.sprintf("Obj. %d", ob + 1);
    meshingLabel1->setText("Meshing");
    meshingLabel2->setText(str);
  }
}

void imodvObjedForm::updateColorBox( QColor color )
{
  mBoxRGB[0] = color.red();
  mBoxRGB[1] = color.green();
  mBoxRGB[2] = color.blue();
  mGLw->updateGL();
}

// This is a call for final setup of the box 
void imodvObjedForm::setCurrentFrame( int frame, int editData )
{	
  mStack->setCurrentIndex(frame);
  panelListBox->setCurrentRow(frame);
  oneAllComboBox->setCurrentIndex(editData);
}	

// Pass on close events (unless meshing) and key events
void imodvObjedForm::closeEvent( QCloseEvent * e )
{
  if (meshingBusy()) {
    e->ignore();
    return;
  }
  imodvObjedClosing();
  e->accept();
}

void imodvObjedForm::keyPressEvent( QKeyEvent * e )
{
  if (hotSliderFlag() != NO_HOT_SLIDER && e->key() == hotSliderKey()) {
    imodvObjedCtrlKey(true);
    grabKeyboard();
  }
  if (utilCloseKey(e))
    imodvObjedDone();
  else
    imodvKeyPress(e);
}

void imodvObjedForm::keyReleaseEvent( QKeyEvent * e )
{
  if (e->key() == hotSliderKey()) {
    imodvObjedCtrlKey(false);
    releaseKeyboard();
  }
  imodvKeyRelease(e);
}

