#include "qt_dialog_customizable.h"

//############################################################

#include <string>
#include <qstring.h>
#include <qstringlist.h>
#include <qvalidator.h>

using namespace std;

//#include <qtgui.h>
//#include <Qt/QStringList>
//#include

//############################################################


//------------------------
//-- Default construtor

GuiDialogCustomizable::GuiDialogCustomizable(
            CustomDialog *ds, QString title, QWidget *parent) : QDialog(parent)
{
	setDialogElements(ds);
  setCaption(title);
}

//------------------------
//-- Method use to automatically set up dialog

void GuiDialogCustomizable::setDialogElements( CustomDialog *ds_ )
{
	ds = ds_;
	
	int numElements = (int)ds->elVal.size();
	
	if( numElements <= 0 ) {
		cerr << "No elements provided" << endl;
	}
	
	vboxLayout = new QVBoxLayout(this,1,1,"hello");
	vboxLayout->setSpacing(6);
  vboxLayout->setMargin(9);
  
	elements.resize( numElements );
	
	
	//## CREATE ALL FORM ELEMENTS AND ADD THEM TO THE FORM IN VERTICAL LAYOUT:
	
	for( int i = 0; i<numElements; i++ )
	{
		switch ( ds->elVal[i].type )
		{
      case( DLG_LABEL ):
			{
				elements[i].label = new QLabel( ds->elVal[i].caption, this );
				if( ds->elVal[i].tooltip != 0 )
          QToolTip::add( elements[i].label, ds->elVal[i].tooltip );
        
				vboxLayout->addWidget( elements[i].label );
			}
        break;
        
      case( DLG_CHECKBOX ):
			{
				elements[i].chkBox = new QCheckBox( ds->elVal[i].caption, this );
				elements[i].chkBox->setChecked( ds->elVal[i].boolValue );
				if( ds->elVal[i].tooltip != 0 )
					QToolTip::add( elements[i].chkBox, ds->elVal[i].tooltip );
        
				vboxLayout->addWidget( elements[i].chkBox );
			}
        break;
        
      case( DLG_LINEEDIT ):
			{
				elements[i].label = new QLabel( ds->elVal[i].caption, this );
				elements[i].lineEdit = new QLineEdit(this);
				elements[i].lineEdit->setText		( ds->elVal[i].stringValue );
				if( ds->elVal[i].tooltip != 0 )
        {
          QToolTip::add( elements[i].label,    ds->elVal[i].tooltip );
          QToolTip::add( elements[i].lineEdit, ds->elVal[i].tooltip );
        }
        
				QHBoxLayout *hboxLayout = new QHBoxLayout();
				hboxLayout->addWidget( elements[i].label );
				hboxLayout->addWidget( elements[i].lineEdit );
        vboxLayout->addLayout( hboxLayout );
			}
        break;
        
      case( DLG_FLOATEDIT ):
			{
				elements[i].label = new QLabel( ds->elVal[i].caption, this );
				elements[i].lineEdit = new QLineEdit(this);
				elements[i].lineEdit->setText( QString::number( ds->elVal[i].value ) );
				if( ds->elVal[i].tooltip != 0 )
        {
          QToolTip::add( elements[i].label,    ds->elVal[i].tooltip );
          QToolTip::add( elements[i].lineEdit, ds->elVal[i].tooltip );
        }
        
        elements[i].lineEdit->setValidator(                               
          new QDoubleValidator( ds->elVal[i].min, ds->elVal[i].max,
                                ds->elVal[i].decimals, elements[i].lineEdit ) );
        
				QHBoxLayout *hboxLayout = new QHBoxLayout();
				hboxLayout->addWidget( elements[i].label );
				hboxLayout->addWidget( elements[i].lineEdit );
        vboxLayout->addLayout( hboxLayout );
			}
        break;
        
        
      case( DLG_SPINBOX ):
			{
				elements[i].label = new QLabel( ds->elVal[i].caption, this );
				elements[i].spnBox = new QSpinBox(this);
				elements[i].spnBox->setMinValue		( (int)ds->elVal[i].min ); 
				elements[i].spnBox->setMaxValue		( (int)ds->elVal[i].max );
				elements[i].spnBox->setValue		( (int)ds->elVal[i].value );
				elements[i].spnBox->setLineStep	( (int)ds->elVal[i].step );
				if( ds->elVal[i].tooltip != 0 )
        {
          QToolTip::add( elements[i].label,  ds->elVal[i].tooltip );
					QToolTip::add( elements[i].spnBox, ds->elVal[i].tooltip );
        }
        
				QHBoxLayout *hboxLayout = new QHBoxLayout();
				hboxLayout->setSpacing(0);
				hboxLayout->setMargin(0);
				QSpacerItem *spacerItem = new QSpacerItem(40, 20, QSizePolicy::Expanding,
                                                  QSizePolicy::Minimum);
				
				hboxLayout->addWidget( elements[i].label );
				hboxLayout->addItem( spacerItem );
				hboxLayout->addWidget( elements[i].spnBox );
				
				vboxLayout->addLayout( hboxLayout );
			}
        break;
        
      case( DLG_COMBOBOX ):
			{
				elements[i].label = new QLabel( ds->elVal[i].caption, this );
				elements[i].cmbBox = new QComboBox(this);
				
        
        QStringList items = 
          QStringList::split( QString(","), ds->elVal[i].stringValue );				
                            // break apart string values between each ","
        
        for (int j=0; j<(int)items.size(); j++)					// add each items to combo box
          elements[i].cmbBox->insertItem ( items[j] );
        if( (int)ds->elVal[i].value < (int)items.size() )
          elements[i].cmbBox->setCurrentItem( (int)ds->elVal[i].value );	
                                                        // set default selection
        if( ds->elVal[i].tooltip != 0 )
        {
          QToolTip::add( elements[i].label,  ds->elVal[i].tooltip );
          QToolTip::add( elements[i].cmbBox, ds->elVal[i].tooltip );
        }
        
				QHBoxLayout *hboxLayout = new QHBoxLayout();
				hboxLayout->setSpacing(0);
				hboxLayout->setMargin(0);
				hboxLayout->addWidget( elements[i].label );
				hboxLayout->addWidget( elements[i].cmbBox );
				
				vboxLayout->addLayout( hboxLayout );
			}
        break;
        
      case( DLG_RADIOGRP ):
			{
				QVButtonGroup *butGrp = new QVButtonGroup(this);
				butGrp->setTitle( ds->elVal[i].caption );
				butGrp->setInsideSpacing(2);
        QStringList items = 
          QStringList::split( QString(","), ds->elVal[i].stringValue );      
                                  // break apart string values between each ","
        
        
        QStringList tooltips;
        if( ds->elVal[i].tooltipArr != 0 )
          tooltips = QStringList::split( QString(","), ds->elVal[i].tooltipArr );
        
				for (int j=0; j<(int)items.size(); j++)			// add each of item as radio button
				{
					QRadioButton *newRadBtn = new QRadioButton(butGrp);
          newRadBtn->setText( items[j] );
					if( j == (int)ds->elVal[i].value )
						newRadBtn->setChecked(true);				// set default selection
          if( j<(int)tooltips.size() )
            QToolTip::add( newRadBtn,  tooltips[j] );
          elements[i].radBtn.push_back( newRadBtn );
				}
        
				if( ds->elVal[i].tooltip != 0 && ds->elVal[i].tooltip != ""  )
					QToolTip::add( butGrp, ds->elVal[i].tooltip );
        
				vboxLayout->addWidget( butGrp );
			}
        break;
        
		}
	}
	
  //## ADD "CANCEL" AND "OKAY" BUTTONS AT BOTTOM:
  
  QHBoxLayout *hboxLayout = new QHBoxLayout();
  
  cancelButton = new QPushButton("Cancel", this);
  connect(cancelButton, SIGNAL(clicked()), this, SLOT(reject()));
  hboxLayout->addWidget(cancelButton);
  
  okButton = new QPushButton("Okay", this);
  connect(okButton, SIGNAL(clicked()), this, SLOT(accept()));
  hboxLayout->addWidget(okButton);
  okButton->setDefault(true);             // if user hits [return] accept() is called
  
  vboxLayout->addLayout( hboxLayout );
  
  adjustSize();
}

//------------------------
//-- Takes all appropriate values each form element and puts them back
//-- into CustomDialog *ds, so they are availabe when the dialog closes.
//-- NOTE: This method is called when user hits "Okay"

void GuiDialogCustomizable::accept()
{
	for( int i = 0; i<(int)ds->elVal.size(); i++ )
	{
		switch ( ds->elVal[i].type )
		{
      case( DLG_LABEL ):
        ;
        break;
        
      case( DLG_CHECKBOX ):
        *ds->elVal[i].returnBool = elements[i].chkBox->isChecked();
        break;
        
      case( DLG_LINEEDIT ):
      {
        string valueStr = elements[i].lineEdit->text();
        *ds->elVal[i].returnString = valueStr;
      }
        break;
        
      case( DLG_FLOATEDIT ):
      {
        *ds->elVal[i].returnFloat = elements[i].lineEdit->text().toFloat();
      }
        break;
        
      case( DLG_SPINBOX ):
        *ds->elVal[i].returnInt = (double)elements[i].spnBox->value();
        break;
        
      case( DLG_COMBOBOX ):
        *ds->elVal[i].returnInt = (double)elements[i].cmbBox->currentItem();
        break;
        
      case( DLG_RADIOGRP ):
			{
				*ds->elVal[i].returnInt = 0;
				for( int j=0; j<(int)elements[i].radBtn.size(); j++ )
					if( elements[i].radBtn[j]->isChecked() )
						*ds->elVal[i].returnInt = j;
			}
        break;
		}
	}
	
	ds->cancelled = false;
	QDialog::accept();
}

//------------------------
//-- NOTE: This method is called when user hits "Cancel"
//-- Set cancelled to true

void GuiDialogCustomizable::reject()
{
	ds->cancelled = true;
	QDialog::reject();
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
    str +=  qstr.at(i).latin1();
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
  return qStringToString( QInputDialog::getText(title.c_str(), label.c_str(),
                                                QLineEdit::Normal, defaultStr.c_str()));
}

//############################################################

