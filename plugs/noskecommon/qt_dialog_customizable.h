#ifndef QT_DIALOG_CUSTOMIZABLE_H
#define QT_DIALOG_CUSTOMIZABLE_H

//############################################################
//  
//  This file contains a special customizable input dialog box (GuiDialogCustomizable)
//  in which GUI element's are set up using a special structure.
//  
//  Things to note:
//    
//    > The values entered by the user are only updated when the user hits "Okay"
//      therefore it is not possible to have anything dynamic within the dialog
//      or it's parent until the user closes the dialog.
//
//    > Setting up a dialog dynamically using vectors of structures like this is
//      less efficient (i.e. more computationally expensive) than
//      making a new class to extend QDialog. The advantage is instead that the progammer
//      can reduce code and avoid the tedium of creating a seperate .h and .cpp file
//      everytime he wants to prompt the user for multiple pieces of info in a dialog.
//      
//
//  -----------------
//  
//  An example of how to use this dialog follows:
//  
//    void GetDetailsFromUser()
//    {
//      string  name     = "First Last";
//      bool    student  = true;
//      int     age      = 20;
//      int     sportIdx = 1;
//      
//      CustomDialog ds;
//      ds.addLabel    ( "Please enter your details below ..." );
//      ds.addLineEdit ( "name:  ", &name, "No middle name!" );
//      ds.addCheckBox ( "current student", &student );
//      ds.addSpinBox  ( "your age: ", 1, 120, &age, 1 );
//      ds.addComboBox ( "sport: ", "tennis,soccer,cricket", &sportIdx );
//      
//      GuiDialogCustomizable dlg(&ds, "Registration", this);
//      dlg.exec();                    // execution stops here until user closes dialog
//      
//      if( ds.cancelled ) {
//        cout << "You have cancelled your registration" << endl;
//        return;
//      }
//      
//      cout << "Dear " << name << " thank you for registering..." << endl;
//      
//      ... CODE ..
//    }
//  
//  -----------------
//  
//  And produces a dialog which looks like this:
//  
//    +----------------------------------------+
//    | Registration                       ? X |
//    +----------------------------------------+
//    | Please enter your details below ...    |
//    |        _____________________________   |
//    | name: |__First_Last_________________|  |
//    |                                        |
//    | [X] current student                    |
//    |                                _____   |
//    | your age:                     [_22_^]  |
//    |                       ______________   |
//    | sport:               |_soccer_____[V]  |
//    |                                        |
//    |   +-------------+    +-------------+   |
//    |   |     Ok      |    |   Cancel    |   |
//    |   +-------------+    +-------------+   |
//    +----------------------------------------+
//  
//  
//############################################################


#include <iostream>
#include <vector>
#include <string>
//Added by qt3to4:
#include <QVBoxLayout>

using namespace std;

#include <qdialog.h>
#include <qvariant.h>
#include <qaction.h>
#include <qapplication.h>
#include <qpushbutton.h>
#include <qcheckbox.h>
#include <qlabel.h>
#include <qcombobox.h>
#include <QButtonGroup>
#include <qradiobutton.h>
#include <qdialog.h>
#include <qspinbox.h>
#include <qlayout.h>
#include <qgroupbox.h>
#include <qtooltip.h>
#include <qstringlist.h>
#include <qmessagebox.h>
#include <qinputdialog.h>
//#include <qdialogbuttonbox.h>


//############################################################

//## CONSTANTS:

enum DlgType { DLG_LABEL, DLG_CHECKBOX, DLG_LINEEDIT,
               DLG_SPINBOX, DLG_DBLSPINBOX,
               DLG_COMBOBOX, DLG_RADIOGRP, DLG_FLOATEDIT };

//############################################################

struct DialogElementValue            // stores information to set up each dialog element
{

//## DATA:
  
  string  *returnString;
  int     *returnInt;
  bool    *returnBool;
  float   *returnFloat;
  
  DlgType type;
  QString caption;
  QString tooltip;
  QString tooltipArr;
  double min, max, value, step, decimals;
  bool boolValue;
  QString stringValue;
  
//## METHODS:
  
  DialogElementValue() {
    reset();
  }
  void reset() {
    returnString = NULL;
    returnInt = NULL;
    returnBool = NULL;
    returnFloat = NULL;
    
    caption = "";
    type = DLG_LABEL;
    tooltip = "";
    tooltipArr = "";
    min = 0;  max = 99;  value = 1;  step = 1;  decimals = 1;
    
    boolValue = false;
    stringValue = "";
  }
};

//############################################################

struct CustomDialog                // used to set up a new GuiDialogCustomizable
{
  int cancelled;
  vector<DialogElementValue> elVal;
  
  
  CustomDialog() {
    reset();
  }
  
  void reset()
  {
    cancelled = true;
    elVal.clear();
  }
  
  //** SIMPLE MUTATORS:        (FOR CREATING A GUI ELEMENT)
  
  int addLabel( QString caption, QString tooltip=0  ) {
    DialogElementValue newElement;
    
    newElement.type = DLG_LABEL;
    newElement.caption = caption;
    newElement.tooltip = tooltip;
    
    elVal.push_back( newElement );
    return ((int)elVal.size()-1);
  }
  
  int addCheckBox( QString caption, bool *checked, QString tooltip=0 ) {
    DialogElementValue newElement;
    
    newElement.returnBool = checked;
    
    newElement.type = DLG_CHECKBOX;
    newElement.caption = caption;
    newElement.boolValue = *checked;
    newElement.tooltip = tooltip;
    
    elVal.push_back( newElement );
    return ((int)elVal.size()-1);
  }
  
  int addLineEdit( QString caption, string *stringValue, QString tooltip=0 ) {
    DialogElementValue newElement;
    
    newElement.returnString = stringValue;
    
    newElement.type = DLG_LINEEDIT;
    newElement.caption = caption;
    string strVal = *stringValue;
    newElement.stringValue = strVal.c_str();
    newElement.tooltip = tooltip;
    
    elVal.push_back( newElement );
    return ((int)elVal.size()-1);
  }
  
  int addLineEditF( QString caption, float *value, float min, float max, float decimals,
                    QString tooltip=0 ) {
    DialogElementValue newElement;
    
    newElement.returnFloat = value;
    
    newElement.type = DLG_FLOATEDIT;
    newElement.caption = caption;
    newElement.value = *value;
    newElement.min = min;
    newElement.max = max;
    newElement.decimals = decimals;
    newElement.tooltip = tooltip;
    
    elVal.push_back( newElement );
    return ((int)elVal.size()-1);
  }
  
  int addSpinBox( QString caption, int min, int max, int *value, int step,
                  QString tooltip=0 ) {
    DialogElementValue newElement;
    
    newElement.returnInt = value;
    
    newElement.type = DLG_SPINBOX;
    newElement.caption = caption;
    newElement.min = min;
    newElement.max = max;
    newElement.value = *value;
    newElement.step = step;
    newElement.tooltip = tooltip;
    
    if( *value < min || *value > max ) {
      cerr << "ERROR: Bad value entered into spin box" << elVal.size()+1 << endl;
      newElement.value = min;
    }
    
    elVal.push_back( newElement );
    return ((int)elVal.size()-1);
  }
  
  int addDblSpinBoxF( QString caption, float min, float max, float *value,
                      int decimals, float step=0.1, QString tooltip=0 ) {
    DialogElementValue newElement;
    
    newElement.returnFloat = value;
    
    newElement.type = DLG_DBLSPINBOX;
    newElement.caption = caption;
    newElement.min = min;
    newElement.max = max;
    newElement.value = *value;
    newElement.decimals = decimals;
    newElement.step = step;
    newElement.tooltip = tooltip;
    
    if( *value < min || *value > max ) {
      cerr << "ERROR: Bad value entered into spin box" << elVal.size()+1 << endl;
      newElement.value = min;
    }
    
    elVal.push_back( newElement );
    return ((int)elVal.size()-1);
  }
  
  int addComboBox( QString caption, QString commaSeperatedList, int *selectedIndex,
                   QString tooltip=0 ) {
    DialogElementValue newElement;
    
    newElement.returnInt = selectedIndex;
    
    newElement.type = DLG_COMBOBOX;
    newElement.caption = caption;
    newElement.stringValue = commaSeperatedList;
    newElement.value = *selectedIndex;
    newElement.tooltip = tooltip;
    
    elVal.push_back( newElement );
    return ((int)elVal.size()-1);
  }
  
  int addRadioGrp( QString caption, QString commaSeperatedList, int *selectedIndex,
                   QString tooltip=0, QString tooltipArr=0 ) {
    DialogElementValue newElement;
    
    newElement.returnInt = selectedIndex;
    
    newElement.type = DLG_RADIOGRP;
    newElement.caption = caption;
    newElement.stringValue = commaSeperatedList;
    newElement.value = *selectedIndex;
    newElement.tooltip = tooltip;
    newElement.tooltipArr = tooltipArr;
    
    elVal.push_back( newElement );
    return ((int)elVal.size()-1);
  }
  
  //** SIMPLE ACCESSORS:        (FOR GETTING THE VALUE OF A GUI ELEMENT)
  
  /*
  bool getResultCheckBox( int idx ) {
    if( elVal[idx].type != DLG_CHECKBOX )
      cerr << "WRONG TYPE OF ELEMENT " << idx << endl;
    return elVal[idx].boolValue;
  }
  string getResultLineEdit( int idx ) {
    if( elVal[idx].type != DLG_LINEEDIT )
      cerr << "WRONG TYPE OF ELEMENT " << idx << endl;
    return elVal[idx].stringValue;
  }
  int getResultSpinBox( int idx ) {
    if( elVal[idx].type != DLG_SPINBOX )
      cerr << "WRONG TYPE OF ELEMENT " << idx << endl;
    return (int)elVal[idx].value;
  }
  int getResultComboBox( int idx ) {
    if( elVal[idx].type != DLG_COMBOBOX )
      cerr << "WRONG TYPE OF ELEMENT " << idx << endl;
    return (int)elVal[idx].value;
  }
  int getResultRadioGrp( int idx ) {
    if( elVal[idx].type != DLG_RADIOGRP )
      cerr << "WRONG TYPE OF ELEMENT " << idx << endl;
    return (int)elVal[idx].value;
  }*/
};


//############################################################

                                // used to store gui widgets in the array of widgets
                                // displayed in GuiDialogCustomizable
struct DialogElement                
{
  QLabel         *label;
  QLineEdit      *lineEdit;
  QCheckBox      *chkBox;
  QSpinBox       *spnBox;
  QDoubleSpinBox *dblSpnBox;
  QComboBox      *cmbBox;
  vector<QRadioButton*> radBtn;
  
  DialogElement() {};
};


//############################################################

                                // used to present a customizable gui
                                // dialog and retrieve user input with minimal code!
class GuiDialogCustomizable : public QDialog                                              
{
  //Q_OBJECT
  
public:
  
  GuiDialogCustomizable(CustomDialog *ds, QString title, QWidget *parent = 0);
  ~GuiDialogCustomizable() {};
  void setDialogElements(CustomDialog *ds);
  
private:
  
  CustomDialog *ds;
  vector<DialogElement> elements;
  
  QPushButton *cancelButton;
  QPushButton *okButton;
  
  QVBoxLayout *vboxLayout;
  
public slots:
  void accept();
  void reject();
};


//############################################################

//-------------------------------
//## SMALL GUI FUNCTIONS:

string qStringToString( QString qstr );
void MsgBox( string str );
bool MsgBoxYesNo( QWidget *parent, string str );
string InputBoxString( QWidget *parent, string title, string label, string defaultStr );

//############################################################

#endif

