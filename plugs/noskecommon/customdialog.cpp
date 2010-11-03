#include "customdialog.h"


//############################################################

#include <qstringlist.h>
#include <qvalidator.h>
#include <qcolordialog.h>

using namespace std;



//############################################################

ColorButton::ColorButton(QColor _color, QWidget *parent) : QPushButton(parent)
{
  color = _color;
  this->setStyleSheet( "background-color: " + color.name() );
  this->setMaximumSize(20,20);
  QObject::connect( this, SIGNAL(clicked()), this, SLOT(pickColor()) );  // won't work in Qt 4.3
}

void ColorButton::setColor( QColor _color )
{
  color = _color;
  this->setStyleSheet( "background-color: " + color.name() );
}

void ColorButton::pickColor()
{
  color = QColorDialog::getColor(color, this);
  //QColorDialog dlgColor = new QColorDialor();
  //if( dlgColor.exec() )
  //color = dlgColor.selectedColor();
  this->setStyleSheet( "background-color: " + color.name() );
      // NOTE: QColorDialog is significantly different in Qt version 4.3 versus 4.7
}

//############################################################

//------------------------
//-- Default construtor

CustomDialog::CustomDialog( QString title, QWidget *parent)
  : QDialog(parent)
{
  setWindowTitle(title);
  
  
  QVBoxLayout *vboxLayoutMain = new QVBoxLayout(this);
  vboxLayoutMain->setSpacing(0);
  vboxLayoutMain->setMargin(0);
  
  vboxLayout = new QVBoxLayout();
  vboxLayout->setSpacing(5);
  vboxLayout->setMargin(8);
  vboxLayout->setContentsMargins(9, 6, 9, 9);
  layoutNextElement = vboxLayout;
  
  //## ADD "CANCEL" AND "OKAY" BUTTONS AT BOTTOM:
  
  QHBoxLayout *hboxLayout = new QHBoxLayout();
  hboxLayout->setSpacing(5);
  hboxLayout->setMargin(8);
  
  cancelButton = new QPushButton("Cancel", this);
  connect(cancelButton, SIGNAL(clicked()), this, SLOT(reject()));
  hboxLayout->addWidget(cancelButton);
  
  okButton = new QPushButton("Okay", this);
  connect(okButton, SIGNAL(clicked()), this, SLOT(accept()));
  hboxLayout->addWidget(okButton);
  okButton->setDefault(true);             // if user hits [return] accept() is called
  
  vboxLayoutMain->addLayout( vboxLayout );
  vboxLayoutMain->addLayout( hboxLayout );
  
  this->setLayout( vboxLayoutMain );
}


//------------------------
//-- This function is called at the start of most "DialogElement" functions to
//-- create a new "DialogElement" and add it to the end of the vector.
//-- @ _type   = the type of dialog element to be created (see: DlgType)
//-- @ caption = the caption for the element, which (depending on it's type) will be
//--             the text within the element or a label placed on the left of the element
//-- @ tooltip  = optional tooltip
//-- @ makeLabel = if true: a label will be added to the left of element.

DialogElement& CustomDialog::addNewElement( DlgType _type, QString caption,
                                            QString tooltip, bool makeLabel)
{
  DialogElement e;
  e.type = _type;
  e.extraChkAdded = false;
  
  if(makeLabel)
  {
    e.label = new QLabel( caption, this );
    if( !tooltip.isEmpty() )
      e.label->setToolTip( tooltip );  
    //setDefaultColorAndFont( e.label ); // uncomment if you add special font or stylesheet changes to group boxes  }
  }
  
  e.layout = new QHBoxLayout();
  e.layout->setSpacing(0);
  e.layout->setContentsMargins(0, 0, 0, 0);
  
  elements.push_back( e );
  
  return ( elements.back() );
}

//------------------------
//-- Adds a plain text label to the next row of the dialog
//-- @ caption  = the caption for the string
//-- @ bold     = set to true if you want the text to be bold
//-- @ tooltip  = optional tooltip

int CustomDialog::addLabel( QString caption, bool bold, QString tooltip )
{
  DialogElement &e = addNewElement( DLG_LABEL, caption, tooltip, true );
  
  setBold( e.label, bold );
  e.label->setTextFormat( Qt::PlainText );
  e.layout->addWidget( e.label );
  //setTextColor(e.label,0,0,255);      // makes labels blue
  layoutNextElement->addLayout( e.layout );
  return elements.size();
}


//------------------------
//-- Adds a html/rich text and hyperlink enabled label to the next row of the dialog
//-- @ caption  = the html/rich text caption for the string
//--              (eg: "<i>Made by</i>: <a href='www.andrewnoske.com'>Andrew<a>")
//-- @ tooltip  = optional tooltip

int CustomDialog::addHtmlLabel( QString caption, QString tooltip )
{
  DialogElement &e = addNewElement( DLG_LABEL, caption, tooltip, true );
  
  e.label->setTextFormat( Qt::RichText );
  e.label->setOpenExternalLinks(true);
  e.layout->addWidget( e.label );
  //setTextColor(e.label,0,0,255);      // makes labels blue
  layoutNextElement->addLayout( e.layout );
  return elements.size();
}


//------------------------
//-- Adds a checkbox to the next row of the dialog
//-- @ caption   = the caption for the checkbox (on the right of the box)
//-- @ *checked  = default value + where the checkbox value is updated if "Ok" is clicked
//-- @ tooltip   = optional tooltip

int CustomDialog::addCheckBox( QString caption, bool *checked, QString tooltip )
{
  DialogElement &e = addNewElement( DLG_CHECKBOX, caption, tooltip, false );
  
  e.returnBool = checked;
  e.chkBox = new QCheckBox( caption, this );
  e.chkBox->setChecked( *checked );
  if( !tooltip.isEmpty() )
    e.chkBox->setToolTip( tooltip );
  
  e.layout->addWidget( e.chkBox );
  layoutNextElement->addLayout( e.layout );
    
  return elements.size();
}

//------------------------
//-- Adds a line edit, preceeded by a label, to the next row of the dialog
//-- @ caption       = the caption for the label to the left of the line edit box
//-- @ *stringValue  = the default value for + where the spin box
//--                   value is saved if/when the user clicks "Ok"
//-- @ tooltip       = optional tooltip

int CustomDialog::addLineEdit( QString caption, string *stringValue, QString tooltip )
{
  DialogElement &e = addNewElement( DLG_LINEEDIT, caption, tooltip, true );
  
  e.returnString = stringValue;
  e.lineEdit = new QLineEdit(this);
  e.lineEdit->setText( stringValue->c_str() );
  if( !tooltip.isEmpty() )
    e.lineEdit->setToolTip(tooltip );
  
  e.layout->addWidget( e.label );
  e.layout->addWidget( e.lineEdit );
  layoutNextElement->addLayout( e.layout );
  return elements.size();
}

//------------------------
//-- Adds a special line edit which accepts only floating poing numbers to the next row
//-- of the dialog
//-- @ caption   = the caption for the label to the left of the line edit box
//-- @ min, max  = minimum and maximum limits
//-- @ *value    = the default value for + where user text is saved if user clicks "Ok"
//-- @ decimal   = number of decimal points to show
//-- @ tooltip   = optional tooltip

int CustomDialog::addLineEditF( QString caption, float min, float max, float *value,
                                float decimals, QString tooltip )
{
  DialogElement &e = addNewElement( DLG_FLOATEDIT, caption, tooltip, true );
  
  e.returnFloat = value;
  e.lineEdit = new QLineEdit(this);
  e.lineEdit->setText( QString::number( *value ) );
  e.lineEdit->setMaximumWidth( 100 );
  if( !tooltip.isEmpty() )
    e.lineEdit->setToolTip(tooltip );
  e.lineEdit->setValidator( new QDoubleValidator( min, max,
                            decimals, e.lineEdit ) );
  
  QSpacerItem *sp = new QSpacerItem(40,20,QSizePolicy::Expanding,QSizePolicy::Minimum);
  e.layout->addWidget( e.label );
  e.layout->addItem( sp );
  e.layout->addWidget( e.lineEdit );
  layoutNextElement->addLayout( e.layout );
  return elements.size();
}

//------------------------
//-- Adds a spin box, preceeded by a label, to the next row of the dialog
//-- @ caption   =  the caption for the label to the left of the spin box
//-- @ min, max  =  minimum and maximum (integer) limits for the spin box
//-- @ *value    =  default value + where the spin box value is saved if user clicks "Ok"
//-- @ decimal   =  number of decimal points to show/allow in the spin box
//-- @ tooltip   =  optional tooltip


int CustomDialog::addSpinBox( QString caption, int min, int max, int *value, int step,
                              QString tooltip )
{
  DialogElement &e = addNewElement( DLG_SPINBOX, caption, tooltip, true );
  
  e.returnInt = value;
  e.spnBox = new QSpinBox(this);
  e.spnBox->setRange     ( min, max );
  e.spnBox->setValue     ( *value );
  e.spnBox->setSingleStep( step );
  if( !tooltip.isEmpty() )
    e.spnBox->setToolTip(tooltip );
  
  QSpacerItem *sp = new QSpacerItem(40,20,QSizePolicy::Expanding,QSizePolicy::Minimum);
  e.layout->addWidget( e.label );
  e.layout->addItem( sp );
  e.layout->addWidget( e.spnBox );
  layoutNextElement->addLayout( e.layout );
  
  return elements.size();
}

//------------------------
//-- Adds a floating-pint spin box, preceeded by a label, to the next row of the dialog
//-- @ caption  =  the caption for the label to the left of the spin box
//-- @ min, max =  minimum and maximum (floating point) limits for the spin box
//-- @ *value   =  the default value + the variable which gets updated if user clicks "Ok"
//-- @ decimal  =  number of decimal points to show/allow in the spin box
//-- @ tooltip  =  optional tooltip

int CustomDialog::addDblSpinBoxF( QString caption, float min, float max, float *value,
                                  int decimals, float step, QString tooltip )
{
  DialogElement &e = addNewElement( DLG_DBLSPINBOX, caption, tooltip, true );
  
  e.returnFloat = value;
  
  e.dblSpnBox = new QDoubleSpinBox(this);
  e.dblSpnBox->setRange( min, max );
  e.dblSpnBox->setValue     ( *value );
  e.dblSpnBox->setSingleStep( step );
  e.dblSpnBox->setDecimals  ( decimals );
  if( !tooltip.isEmpty() )
    e.dblSpnBox->setToolTip(tooltip);
  
  QSpacerItem *sp = new QSpacerItem(40,20,QSizePolicy::Expanding,QSizePolicy::Minimum);
  e.layout->addWidget( e.label );
  e.layout->addItem( sp );
  e.layout->addWidget( e.dblSpnBox );
  layoutNextElement->addLayout( e.layout );
  return elements.size();
}

//------------------------
//-- Adds a combo box, preceeded by a label, to the next row of the dialog
//-- @ caption    =  the caption for the label to the left of the combo box
//-- @ barSepList =  sting of items for the combo list. Each item must be seperated by 
//--                 a bar character (eg: "one|two").
//-- @ *selIdx    =  the default selected index + where the selected index is
//--                 updated when/if the user clicks "Ok"
//-- @ tooltip    =  optional tooltip

int CustomDialog::addComboBox( QString caption, QString barSepList, int *selIdx,
                               QString tooltip )
{
  DialogElement &e = addNewElement( DLG_COMBOBOX, caption, tooltip, true );
  
  e.returnInt = selIdx;
  e.cmbBox = new QComboBox(this);
  QStringList items = barSepList.split("|", QString::SkipEmptyParts);
    // break apart string values between each "|"
  
  e.cmbBox->addItems(items);
  if( *selIdx < (int)items.size() )  // set default selection
    e.cmbBox->setCurrentIndex( *selIdx );
  if( !tooltip.isEmpty() )
    e.cmbBox->setToolTip(tooltip);
  
  e.layout->addWidget( e.label );
  e.layout->addWidget( e.cmbBox );
  layoutNextElement->addLayout( e.layout );
  return elements.size();
}

//------------------------
//-- Adds a radio group to the next row of the dialog
//-- @ caption    = the caption/title of the group box around the radio buttons
//-- @ varSepList = sting of items/radio button for the radio box group - whereby
//--                each item must be seperated by bars (eg: "opt 1|opt 2|optn 3").
//-- @ *selIdx    = the default selected index + where the selected index is
//--                updated when/if the user clicks "Ok"
//-- @ tooltip    = optional tooltip applied to whole group
//-- @ tooltipArr = optional string of tool tips where each tool tip must be seperated by
//--                a bar character (eg: "tooltip opt1|tooltip opt2|tooltip opt 3")
//-- @ checkable  = wether or not a checkbox appears in the title of the radio group.
//-- @ *checked   = if "checkable" is true, the default value + where the value of the
//--                checkbox is updated when/if the user clicks "Ok"

int CustomDialog::addRadioGrp( QString caption, QString barSepList, int *selIdx,
                               QString tooltip, QString tooltipArr, bool checkable, bool *checked )
{
  DialogElement &e = addNewElement( DLG_RADIOGRP, caption, tooltip, false );
  
  e.returnInt = selIdx;
  e.grpBox = new QGroupBox(this);
  e.grpBox->setTitle( caption );
  if( checkable )
  {
    e.returnBool = checked;
    e.grpBox->setCheckable( *checked );
  }
  QButtonGroup *butGrp = new QButtonGroup(this);
  QVBoxLayout  *butLay = new QVBoxLayout(e.grpBox);
  butLay->setSpacing(2);
  butLay->setContentsMargins(5, 2, 5, 5);
  QStringList items = barSepList.split(
      "|",QString::SkipEmptyParts);   // break apart string values between each "|"
  
  QStringList tooltips;
  if( !tooltipArr.isEmpty() )
    tooltips = tooltipArr.split("|", QString::SkipEmptyParts);
  
  for (int j=0; j<(int)items.size(); j++)      // add each of item as radio button
  {
    QRadioButton *newRadBtn = new QRadioButton(e.grpBox);
    butLay->addWidget(newRadBtn);
    butGrp->addButton(newRadBtn, j);
    newRadBtn->setText( items[j] );
    if( j == *selIdx )
      newRadBtn->setChecked(true);        // set default selection
    if( j<(int)tooltips.size() )
      newRadBtn->setToolTip(tooltips[j] );
    e.radBtn.push_back( newRadBtn );
  }
  
  if( !tooltip.isEmpty() )
    e.grpBox->setToolTip(tooltip );
  
  e.layout->addWidget( e.grpBox );
  layoutNextElement->addLayout( e.layout );
  return elements.size();
}

//------------------------
//-- Adds a small square color selection button, preceeded by a label, to the dialog.
//-- When this button is clicked, Qt's colour selector dialog appears, and the colour of  
//-- the button changes to the color selected
//-- @ caption    = the caption of the label to the left of the button
//-- @ *color     = the default color + where the color is updated if the user clicks "Ok"
//-- @ tooltip    =  optional tooltip

int CustomDialog::addColorSel( QString caption, QColor *color, QString tooltip )
{
  DialogElement &e = addNewElement( DLG_COLOR, caption, tooltip, true );
  
  e.returnColor = color;
  e.btnColor = new ColorButton(*color,this);
  if( !tooltip.isEmpty() )
    e.btnColor->setToolTip(tooltip );
  
  QSpacerItem *sp = new QSpacerItem(40,20,QSizePolicy::Expanding,QSizePolicy::Minimum);
  e.layout->addWidget( e.label );
  e.layout->addItem( sp );
  e.layout->addWidget( e.btnColor );
  layoutNextElement->addLayout( e.layout );
  
  return elements.size();
}

//------------------------
//-- Begins a new group box to the next row of the dialog. All new elements added after
//-- this will be placed inside this box. "endGroupBox" must be called to end the box.
//-- @ caption    = the caption/title of the group box
//-- @ tooltip    = optional tooltip applied to group box
//-- @ checkable  = wether or not a checkbox appears in the title of the group box.
//-- @ *checked   = if "checkable" is true, the default value + where the value of the
//--                checkbox is updated when/if the user clicks "Ok"

int CustomDialog::beginGroupBox( QString caption, bool flat, QString tooltip,
                                 bool checkable, bool *checked )
{
  DialogElement &e = addNewElement( DLG_GRPBOX, caption, tooltip, false );
  
  e.grpBox = new QGroupBox(this);
  e.grpBox->setTitle( caption );
  e.grpBox->setFlat( flat );
  if( checkable )
  {
    e.returnBool = checked;
    e.grpBox->setCheckable( *checked );
  }
  
  groupBoxLayout = new QVBoxLayout(e.grpBox);
  groupBoxLayout->setSpacing(2);
  groupBoxLayout->setContentsMargins(8, 12, 5, 5);
  if( !tooltip.isEmpty() )
    e.grpBox->setToolTip(tooltip );
  
  e.layout->addWidget( e.grpBox );
  layoutNextElement->addLayout( e.layout );
  
  layoutNextElement = groupBoxLayout;
  
  //e.grpBox->setStyleSheet("background-color: qlineargradient(spread:pad, x1:0, y1:0, x2:0, y2:1, stop:0 rgba(255, 255, 255, 255), stop:1 rgba(220, 233, 255, 255));");  // nice background colour
  
  return elements.size();
}

//------------------------
//-- Must be called AFTER "beginGroupBox" and ends the group box

void CustomDialog::endGroupBox()
{
  layoutNextElement = vboxLayout;
}

//------------------------
//-- Adds an extra checkbox into to the previously added element which (depending on the
//-- value of "chkBehav" can be used to enable, disable, show or hide that element when it
//-- is ticked. For most elements (DLG_SPINBOX, DLG_DBLSPINBOX, DLG_COLOR, DLG_FLOATEDIT)
//-- the checkbox is added in the middle of that row, after the label. For a DLG_RADIOGRP 
//-- or DLG_GRPBOX, the checkbox is added to a row before the group box.
//-- 
//-- @ caption     = the caption for the little checkbox
//-- @ *checked    = the default value + where the value of the little checkbox
//--                is updated when/if the user clicks "Ok"
//-- @ chkbehavior = whether this little checkbox will do nothing (CB_NONE),
//--                  disable (CB_DISABLE), enable (CB_ENABLE), hide (CB_HIDE) or
//--                  show (CB_SHOW) the previous element when checked.
//-- @ removeLabel = will remove the little label added on the left by "addNewElement()"
//-- @ tooltip     = optional tooltip applied to the little checkbox

int CustomDialog::addCheckPrev( QString caption, bool *checked,
                                chkbehaviour chkBehav, bool removeLabel, QString tooltip)
{
  DialogElement &e = elements.back();
  e.extraChkAdded = true;
  
  e.returnChkExtra = checked;
  e.chkExtra = new QCheckBox( caption, this );
  e.chkExtra->setChecked( *checked );
  if( !tooltip.isEmpty() )
    e.chkExtra->setToolTip( tooltip );
  
  //## INSERT CHECK BOX INTO CORRECT POSITION:
  
  int insertPos = 1;
  if( !removeLabel && (e.type==DLG_SPINBOX  || e.type==DLG_DBLSPINBOX
                       || e.type==DLG_COLOR || e.type==DLG_FLOATEDIT ) )
    insertPos = 2;
  if( e.type==DLG_RADIOGRP || e.type==DLG_GRPBOX )
    vboxLayout->insertWidget( vboxLayout->count()-1, e.chkExtra );
  else
    e.layout->insertWidget( insertPos, e.chkExtra );
  
  //## REMOVE LABEL IF SPECIFIED:
  
  if( removeLabel && e.label )
  {
    e.layout->removeWidget( e.label );
    e.label->setParent(0);
  }
  
  //## ADD APPROPRIATE CONNECTOR TO APPROPRIATE WIDGET:
  
  QWidget *wid;
  
  switch( e.type )
  {
  case(DLG_LABEL):      wid = e.label;      break;
  case(DLG_CHECKBOX):   wid = e.chkBox;     break;
  case(DLG_LINEEDIT):   wid = e.lineEdit;   break;
  case(DLG_FLOATEDIT):  wid = e.lineEdit;   break;
  case(DLG_SPINBOX):    wid = e.spnBox;     break;
  case(DLG_DBLSPINBOX): wid = e.dblSpnBox;  break;
  case(DLG_COMBOBOX):   wid = e.cmbBox;     break;
  case(DLG_RADIOGRP):   wid = e.grpBox;     break;
  case(DLG_GRPBOX):     wid = e.grpBox;     break;
  case(DLG_COLOR):      wid = e.btnColor;   break;
  }
  
  switch( chkBehav )
  {
  case(CB_NONE): break;
  case(CB_ENABLE):
    wid->setEnabled( *checked );
    e.chkExtra->connect( e.chkExtra, SIGNAL(clicked(bool)), wid, SLOT(setEnabled(bool)));
    break;
  case(CB_DISABLE):
    wid->setDisabled( *checked );
    e.chkExtra->connect( e.chkExtra, SIGNAL(clicked(bool)), wid, SLOT(setDisabled(bool)));
    break;
  case(CB_SHOW):
    wid->setVisible( *checked );
    e.chkExtra->connect( e.chkExtra, SIGNAL(clicked(bool)), wid, SLOT(setVisible(bool)));
    connect( e.chkExtra, SIGNAL( clicked() ), this, SLOT( resizeMe() ) );
    break;
  case(CB_HIDE):
    wid->setHidden( *checked );
    e.chkExtra->connect( e.chkExtra, SIGNAL(clicked(bool)), wid, SLOT(setHidden(bool)));
    connect( e.chkExtra, SIGNAL( clicked() ), this, SLOT( resizeMe() ) );
    break;
  }
  
  return elements.size();
}

//------------------------
//-- Adds an autocomplete feature to the previous added element, where the previously
//-- added element must be a line edit box.
//-- Will return the index of the element or -1 if the element was not a line edit.
//-- 
//-- @ wordList      = the list of words that comprise the auto complete
//-- @ caseSensitive = if true, the autocomplete is case sensitive

int CustomDialog::addAutoCompletePrev( QStringList wordList, bool caseSensitive )
{
  DialogElement &e = elements.back();
  if (e.type != DLG_LINEEDIT)
  {
    cerr << "ERROR: addAutoCompletePrev() must proceed addLineEdit()" << endl;
    return 01;
  }
  
  QCompleter *completer = new QCompleter(wordList, this);
  if( caseSensitive )
    completer->setCaseSensitivity( Qt::CaseSensitive );
  else
    completer->setCaseSensitivity( Qt::CaseInsensitive );
  e.lineEdit->setCompleter(completer);
  
  return elements.size();
}


//------------------------
//-- Sets the style sheet to the element at the given index in the elements vector
//-- @ idx     = the index of the element to apply the stylesheet to
//-- @ syleStr = the style sheet to apply 
//--             eg: "color: rgb(0, 0, 0); background-color: rgba(255, 255, 255, 0);"
//-- @ bold    = if true: the font for the element will be made bold

bool CustomDialog::setStyleElem( int idx, string styleStr, bool bold )
{
  if( idx >= elements.size() )
    return false;
  
  DialogElement &e = elements[idx];
  QString styleQStr = (QString)styleStr.c_str();
  
  
  if( e.extraChkAdded )
    e.chkExtra->setStyleSheet( styleQStr );
  
  if( e.type == DLG_LABEL || e.type == DLG_LINEEDIT || e.type == DLG_FLOATEDIT 
     || e.type == DLG_SPINBOX || e.type == DLG_DBLSPINBOX || e.type == DLG_COMBOBOX
     || e.type == DLG_COLOR )
    e.label->setStyleSheet( styleQStr );
  
  switch ( e.type )
  {
    case( DLG_CHECKBOX ):   e.chkBox->setStyleSheet( styleQStr );     break;
    case( DLG_LINEEDIT ):
    case( DLG_FLOATEDIT ):  e.lineEdit->setStyleSheet( styleQStr );   break;
    case( DLG_SPINBOX ):    e.spnBox->setStyleSheet( styleQStr );     break;
    case( DLG_DBLSPINBOX ): e.dblSpnBox->setStyleSheet( styleQStr );  break;
    case( DLG_COMBOBOX ):   e.cmbBox->setStyleSheet( styleQStr );     break;
    case( DLG_RADIOGRP ):
    {
      for( int i=0; i<(int)e.radBtn.size(); i++ )
        e.radBtn[i]->setStyleSheet( styleQStr );
    } break;
    case( DLG_COLOR ):      break;
    case( DLG_GRPBOX ):     e.grpBox->setStyleSheet( styleQStr );     break;
  }
  
  return true;
}


//------------------------
//-- Sets the style sheet for the previously added element
//-- @ syleStr = the style sheet to apply 
//--             eg: "color: rgb(0, 0, 0); background-color: rgba(255, 255, 255, 0);"
//-- @ bold    = if true: the font for the element will be made bold

int CustomDialog::setStylePrev( string styleStr, bool bold )
{
  setStyleElem( elements.size()-1, styleStr, bold );
}


//------------------------
//-- Resizes the dialog to the minimum size possible.

void CustomDialog::resizeMe()
{
  this->resize( 20,20 );
  adjustSize();
}

//------------------------
//-- Called when/if the user clicks "Ok" - this funciton takes all appropriate values
//-- from each dialog element and updates the appropriate variable.
//-- For example if you called: "dlg.addLineEdit("your name",&name);"... and your user
//-- types "Bloggs" then clicks okay, the value at the location "&name" changes to "Bloggs".

void CustomDialog::accept()
{
  for( int i = 0; i<(int)elements.size(); i++ )
  {
    DialogElement &e = elements[i];
    
    if( e.extraChkAdded )
      *e.returnChkExtra = e.chkExtra->isChecked();
    
    switch ( e.type )
    {
    case( DLG_LABEL ): break;
    case( DLG_CHECKBOX ):
      *e.returnBool = e.chkBox->isChecked();
      break;
      
    case( DLG_LINEEDIT ):
      *e.returnString = (const char *)e.lineEdit->text().toLatin1();
      break;
      
    case( DLG_FLOATEDIT ):
      *e.returnFloat = e.lineEdit->text().toFloat();
      break;
      
    case( DLG_SPINBOX ):
      *e.returnInt = (int)e.spnBox->value();
      break;
      
    case( DLG_DBLSPINBOX ):
      *e.returnFloat = (float)e.dblSpnBox->value();
      break;
      
    case( DLG_COMBOBOX ):
      *e.returnInt = (double)e.cmbBox->currentIndex();
      break;
      
    case( DLG_RADIOGRP ):
      {
        *e.returnInt = 0;
        for( int j=0; j<(int)e.radBtn.size(); j++ )
          if( e.radBtn[j]->isChecked() )
            *e.returnInt = j;
        if( e.grpBox->isCheckable() )
          *e.returnInt = e.grpBox->isChecked();
      }
      break;
      
    case( DLG_COLOR ):
      *e.returnColor = e.btnColor->color;
      break;
      
    case( DLG_GRPBOX ):
      if( e.grpBox->isCheckable() )
        *e.returnInt = e.grpBox->isChecked();
      break;
    }
  }
  
  QDialog::accept();
}

//------------------------
//-- Begins execution of and shows the dialog you created.

int CustomDialog::exec()
{
  adjustSize();
  return QDialog::exec();
}

//------------------------
//-- Simple accessor which returns true if the user hit "Cancel"
//-- (and thus the input values were not changed) or false if the user hit "Ok"
//-- (and thus some of the input values may have changed).

bool CustomDialog::wasCancelled()
{
  return ( result() == QDialog::Rejected );
}


//############################################################


//----------------------------------------------------------------------------
//
//          GUI FUNCTIONS:
//
//----------------------------------------------------------------------------


//---------
//-- Converts a QString to a standard string

string qStringToString( QString qstr )
{
  string str = "";
  for( int i=0; i<qstr.length(); i++ )
    str +=  qstr.at(i).toLatin1();
  return str;
}

//---------
//-- Display a simple message box

void MsgBox( string str )
{
  QMessageBox::information(0, "...", str.c_str() );
}

//---------
//-- Display a tyes/no dialog box and return "true" if use clicks yes.

bool MsgBoxYesNo( QWidget *parent, string str )
{
  int result = QMessageBox::information( parent, "...", str.c_str(),
                                         QMessageBox::Yes, QMessageBox::No );
  return ( result == QMessageBox::Yes );
}

//---------
//-- Display an input dialog and return the string entered by the user.

string InputBoxString( QWidget *parent, string title, string label, string defaultStr )
{
  return qStringToString( QInputDialog::getText(parent, title.c_str(), label.c_str(),
                                                QLineEdit::Normal, defaultStr.c_str()));
}

//---------
//-- Short function name for converting an integer to a QString.

QString QStr( int number )
{
  return QString::number( number );
}

//---------
//-- Short function name for converting an float to a QString.

QString QStr( float number )
{
  return QString::number( number );
}

//---------
//-- Short function name for taking any widget and making the text in it bold.

void setBold( QWidget *wid, bool bold )
{
  QFont font;
  font.setBold(bold);
  wid->setFont( font );
}

//---------
//-- Short function name for setting the text (forground) color of a widget.

void setTextColor( QWidget *wid, int r, int g, int b )
{
  wid->setStyleSheet( "color: rgb(" + QStr(r) + "," + QStr(g) + "," + QStr(b) + ");" );
}


//---------
//-- Short function which sets the font to default, the foreground/text color to black
//-- and background to transparent. This function is useful for when you might apply a
//-- stylesheet to a container object, but you don't want those changes to apply to 
//-- (heirarchially) to widgets within it.

void setDefaultColorAndFont( QWidget *wid )
{
  wid->setFont( QFont() );
  wid->setStyleSheet("color: rgb(0, 0, 0); background-color: rgba(255, 255, 255, 0);");
}

//############################################################

