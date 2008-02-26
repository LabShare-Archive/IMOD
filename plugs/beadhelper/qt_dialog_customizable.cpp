#include "qt_dialog_customizable.h"

//############################################################

#include <string>
#include <qstring.h>
#include <qstringlist.h>

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
  setModal(false);
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
          QToolTip::add( elements[i].lineEdit, ds->elVal[i].tooltip );
        
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
					QToolTip::add( elements[i].spnBox, ds->elVal[i].tooltip );
        
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
          QToolTip::add( elements[i].cmbBox, ds->elVal[i].tooltip );
        
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
        
				for (int j=0; j<(int)items.size(); j++)			// add each of item as radio button
				{
					QRadioButton *newRadBtn = new QRadioButton(butGrp);
          newRadBtn->setText( items[j] );
					if( j == (int)ds->elVal[i].value )
						newRadBtn->setChecked(true);				// set default selection
          elements[i].radBtn.push_back( newRadBtn );
				}
        
				if( ds->elVal[i].tooltip != 0 )
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
        ds->elVal[i].boolValue = elements[i].chkBox->isChecked();
        break;
        
      case( DLG_LINEEDIT ):
        ds->elVal[i].stringValue = elements[i].lineEdit->text();
        break;
        
      case( DLG_SPINBOX ):
        ds->elVal[i].value = (double)elements[i].spnBox->value();
        break;
        
      case( DLG_COMBOBOX ):
        ds->elVal[i].value = (double)elements[i].cmbBox->currentItem();
        break;
        
      case( DLG_RADIOGRP ):
			{
				ds->elVal[i].value = 0;
				for( int j=0; j<(int)elements[i].radBtn.size(); j++ )
					if( elements[i].radBtn[j]->isChecked() )
						ds->elVal[i].value = j;
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

