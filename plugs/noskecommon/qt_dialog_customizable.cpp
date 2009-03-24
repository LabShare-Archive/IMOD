#include "qt_dialog_customizable.h"
//Added by qt3to4:
#include <QHBoxLayout>
#include <QLabel>
#include <QVBoxLayout>

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
  setWindowTitle(title);
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
	
	vboxLayout = new QVBoxLayout(this);
	vboxLayout->setSpacing(6);
  vboxLayout->setMargin(9);
  vboxLayout->setContentsMargins(9, 6, 9, 9);
        
	elements.resize( numElements );
	
	
	//## CREATE ALL FORM ELEMENTS AND ADD THEM TO THE FORM IN VERTICAL LAYOUT:
  // DNM 1/1/09: changed all tooltip comparisons with 0 to tests for ! isEmpty
	for( int i = 0; i<numElements; i++ )
	{
		switch ( ds->elVal[i].type )
		{
      case( DLG_LABEL ):
			{
				elements[i].label = new QLabel( ds->elVal[i].caption, this );
				if( !ds->elVal[i].tooltip.isEmpty() )
           elements[i].label->setToolTip(ds->elVal[i].tooltip );
        
				vboxLayout->addWidget( elements[i].label );
			}
        break;
        
      case( DLG_CHECKBOX ):
			{
				elements[i].chkBox = new QCheckBox( ds->elVal[i].caption, this );
				elements[i].chkBox->setChecked( ds->elVal[i].boolValue );
				if( !ds->elVal[i].tooltip.isEmpty() )
					 elements[i].chkBox->setToolTip(ds->elVal[i].tooltip );
        
				vboxLayout->addWidget( elements[i].chkBox );
			}
        break;
        
      case( DLG_LINEEDIT ):
			{
				elements[i].label = new QLabel( ds->elVal[i].caption, this );
				elements[i].lineEdit = new QLineEdit(this);
				elements[i].lineEdit->setText		( ds->elVal[i].stringValue );
				if( !ds->elVal[i].tooltip.isEmpty() )
        {
           elements[i].label->setToolTip(ds->elVal[i].tooltip );
           elements[i].lineEdit->setToolTip(ds->elVal[i].tooltip );
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
				if( !ds->elVal[i].tooltip.isEmpty() )
        {
           elements[i].label->setToolTip(ds->elVal[i].tooltip );
           elements[i].lineEdit->setToolTip(ds->elVal[i].tooltip );
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
				elements[i].spnBox->setRange(      (int)ds->elVal[i].min, 
                                           (int)ds->elVal[i].max );
				elements[i].spnBox->setValue		 ( (int)ds->elVal[i].value );
				elements[i].spnBox->setSingleStep( (int)ds->elVal[i].step );
				if( !ds->elVal[i].tooltip.isEmpty() )
        {
           elements[i].label->setToolTip(ds->elVal[i].tooltip );
					 elements[i].spnBox->setToolTip(ds->elVal[i].tooltip );
        }
        
				QHBoxLayout *hboxLayout = new QHBoxLayout();
				hboxLayout->setSpacing(0);
				hboxLayout->setContentsMargins(0, 0, 0, 0);
				QSpacerItem *spacerItem = new QSpacerItem(40, 20, QSizePolicy::Expanding,
                                                  QSizePolicy::Minimum);
				
				hboxLayout->addWidget( elements[i].label );
				hboxLayout->addItem( spacerItem );
				hboxLayout->addWidget( elements[i].spnBox );
				
				vboxLayout->addLayout( hboxLayout );
			}
        break;
        
      case( DLG_DBLSPINBOX ):
			{
				elements[i].label = new QLabel( ds->elVal[i].caption, this );
				elements[i].dblSpnBox = new QDoubleSpinBox(this);
				elements[i].dblSpnBox->setMinimum   ( ds->elVal[i].min ); 
        elements[i].dblSpnBox->setMaximum   ( ds->elVal[i].max );
				elements[i].dblSpnBox->setValue	   ( ds->elVal[i].value );
				elements[i].dblSpnBox->setSingleStep( ds->elVal[i].step );
        elements[i].dblSpnBox->setDecimals  ( ds->elVal[i].decimals );
				if( !ds->elVal[i].tooltip.isEmpty() )
        {
          elements[i].label->setToolTip(ds->elVal[i].tooltip );
          elements[i].dblSpnBox->setToolTip(ds->elVal[i].tooltip );
        }
        
				QHBoxLayout *hboxLayout = new QHBoxLayout();
				hboxLayout->setSpacing(0);
				hboxLayout->setContentsMargins(0, 0, 0, 0);
				QSpacerItem *spacerItem = new QSpacerItem(40, 20, QSizePolicy::Expanding,
                                                  QSizePolicy::Minimum);
				
				hboxLayout->addWidget( elements[i].label );
				hboxLayout->addItem( spacerItem );
				hboxLayout->addWidget( elements[i].dblSpnBox );
				
				vboxLayout->addLayout( hboxLayout );
			}
        break;
        
      case( DLG_COMBOBOX ):
			{
				elements[i].label = new QLabel( ds->elVal[i].caption, this );
				elements[i].cmbBox = new QComboBox(this);
				
        
        QStringList items = 
          ds->elVal[i].stringValue.split(",", QString::SkipEmptyParts);
                            // break apart string values between each ","

        // DNM: ANDREW, YOU SHOULD TEST THAT THIS WORKS!
        elements[i].cmbBox->addItems(items );
        if( (int)ds->elVal[i].value < (int)items.size() )
          elements[i].cmbBox->setCurrentIndex( (int)ds->elVal[i].value );	
                                                        // set default selection
        if( !ds->elVal[i].tooltip.isEmpty() )
        {
           elements[i].label->setToolTip(ds->elVal[i].tooltip );
           elements[i].cmbBox->setToolTip(ds->elVal[i].tooltip );
        }
        
				QHBoxLayout *hboxLayout = new QHBoxLayout();
				hboxLayout->setSpacing(0);
				hboxLayout->setContentsMargins(0, 0, 0, 0);
				hboxLayout->addWidget( elements[i].label );
				hboxLayout->addWidget( elements[i].cmbBox );
				
				vboxLayout->addLayout( hboxLayout );
			}
        break;
        
      case( DLG_RADIOGRP ):
			{
        QGroupBox *butGrpBox = new QGroupBox(this);
				QButtonGroup *butGrp = new QButtonGroup(this);
				butGrpBox->setTitle( ds->elVal[i].caption );
        QVBoxLayout *butLay = new QVBoxLayout(butGrpBox);
				butLay->setSpacing(2);
        butLay->setContentsMargins(5, 2, 5, 5);
        QStringList items = 
          ds->elVal[i].stringValue.split(",", QString::SkipEmptyParts);
                                  // break apart string values between each ","
        
        
        QStringList tooltips;
        if( !ds->elVal[i].tooltipArr.isEmpty() )
          tooltips = ds->elVal[i].tooltipArr.split(",", QString::SkipEmptyParts);
        
				for (int j=0; j<(int)items.size(); j++)			// add each of item as radio button
				{
					QRadioButton *newRadBtn = new QRadioButton(butGrpBox);
          butLay->addWidget(newRadBtn);
          butGrp->addButton(newRadBtn, j);
          newRadBtn->setText( items[j] );
					if( j == (int)ds->elVal[i].value )
						newRadBtn->setChecked(true);				// set default selection
          if( j<(int)tooltips.size() )
             newRadBtn->setToolTip(tooltips[j] );
          elements[i].radBtn.push_back( newRadBtn );
				}
        
				if( !ds->elVal[i].tooltip.isEmpty() )
					 butGrpBox->setToolTip(ds->elVal[i].tooltip );
        
				vboxLayout->addWidget( butGrpBox );
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
        // DNM 1/1/09: changed from direct assignment of QString
        string valueStr = (const char *)elements[i].lineEdit->text().toLatin1();
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
        
      case( DLG_DBLSPINBOX ):
        *ds->elVal[i].returnFloat = (float)elements[i].dblSpnBox->value();
        break;
        
      case( DLG_COMBOBOX ):
        *ds->elVal[i].returnInt = (double)elements[i].cmbBox->currentIndex();
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

//############################################################

