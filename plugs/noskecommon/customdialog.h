#ifndef CUSTOMDIALOG_H
#define CUSTOMDIALOG_H

//############################################################
//  
//  This file contains a special class, "CustomDialog", for generating
//  a modal dialog box with multiple form elements/inputs including:
//  labels, checkboxes, line edit boxes, number edit boxes, combo boxes,
//  radio buttons, spin boxes (for ints or floats), group boxes (to group elements)
//  and, most recently, "pick color" buttons (letting you chose a color).
//  
//  NOTES:
//    
//    > Each form element is created by a single function only, includes an optional tooltip,
//      and a pointer to the variable you wish to modify.
//      
//    > The value of the variable is updated ONLY if and when hits "Ok", and will change only
//      if the user has changed it from the original/default/starting value in that variable.
//
//    > This class if fantastic for making modal dialogs (i.e. dialogs where you can't click 
//      anything else until you click okay or cancel), but isn't designed for times when you 
//      want/need the parent dialog to dymaically update as the user changes values
//       (i.e. interactive changes).
//    
//    > This method of generating dialog elements on-the-fly by using a vectors of structs is 
//      obviously less versatile than creating a seperate class which extend QDialog for EACH
//      dialog you need. The huge advantage, however, is you can can greatly reduce your code 
//      and avoid the tedium of creating a seperate .h and .cpp file everytime you wants to 
//      prompt the user for multiple pieces of info in a single modal dialog!
//    
//    > This file also includes several "convinience" functions such as "MsgBoxYesNo" and 
//      "setBold" for quickly displaying dialogs/returning results and modifying strings.
//      These convinience functions are at the bottom of this file (and defined in the .cpp).
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
//      CustomDialog d("Registration", this);
//      d.addLabel    ( "Please enter your details below ..." );
//      d.addLineEdit ( "name:  ", &name, "No middle name!" );
//      d.addCheckBox ( "current student", &student );
//      d.addSpinBox  ( "your age: ", 1, 120, &age, 1 );
//      d.addComboBox ( "sport: ", "tennis|soccer|cricket", &sportIdx );
//      
//      d.exec();                    // execution stops here until user closes dialog
//      
//      if( d.wasCancelled() ) {
//        cout << "You have cancelled your registration" << endl;
//        return;
//      }
//      
//      cout << "Dear " << name << " thank you for registering..." << endl;
//      
//      ... CODE ...
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
//############################################################


#include <iostream>
#include <vector>
#include <string>

using namespace std;

#include <qdialog.h>
#include <qvariant.h>
#include <qaction.h>
#include <qapplication.h>
#include <qpushbutton.h>
#include <qcheckbox.h>
#include <qlabel.h>
#include <qcombobox.h>
#include <qbuttongroup>
#include <qradiobutton.h>
#include <qdialog.h>
#include <qspinbox.h>
#include <qlayout.h>
#include <qgroupbox.h>
#include <qtooltip.h>
#include <qstringlist.h>
#include <qmessagebox.h>
#include <qinputdialog.h>
#include <QHBoxLayout>
#include <QLabel>
#include <QVBoxLayout>

//############################################################

//## CONSTANTS:

enum DlgType { DLG_LABEL, DLG_CHECKBOX, DLG_LINEEDIT, DLG_FLOATEDIT,
               DLG_SPINBOX, DLG_DBLSPINBOX,
               DLG_COMBOBOX, DLG_RADIOGRP, DLG_GRPBOX, DLG_COLOR, DGL_ALL };

enum chkbehaviour { CB_NONE, CB_DISABLE, CB_ENABLE,
                    CB_HIDE, CB_SHOW };

//############################################################

//## SMALL CLASSES:

class ColorButton : public QPushButton    // used to create a "pick colour" button
{
  //Q_OBJECT      // NOTE: For Qt version 4.3 I have to comment these out
  
public:
  QColor color;
  ColorButton(QColor _color, QWidget *parent=0);
public slots:
  void pickColor();
};

//############################################################

//************************
//** DialogElement is used to store gui widgets in the array of 
//** widgets displayed in GuiDialogCustomizable

struct DialogElement
{
  string  *returnString;
  int     *returnInt;
  bool    *returnBool;
  float   *returnFloat;
  QColor  *returnColor;
  bool    *returnChkExtra;
  
  DlgType type;
  bool    extraChkAdded;
  
  QLabel         *label;
  QCheckBox      *chkBox;
  QLineEdit      *lineEdit;
  QSpinBox       *spnBox;
  QDoubleSpinBox *dblSpnBox;
  QComboBox      *cmbBox;
  ColorButton    *btnColor;
  vector<QRadioButton*> radBtn;
  QGroupBox      *grpBox;
  
  QCheckBox      *chkExtra;
  QHBoxLayout    *layout;
};

//############################################################

//************************
//** GuiDialogCustomizable is used to present a customizable gui
//** dialog and retrieve user input with minimal code!

class CustomDialog : public QDialog                                              
{
  //Q_OBJECT
  
public:     //## METHODS:
  
  CustomDialog(QString title, QWidget *parent = 0);
  ~CustomDialog() {};
  bool setDialogElements();
  bool wasCancelled();
  
  DialogElement& addNewElement(DlgType _type, QString caption, QString tooltip, bool makeLabel);
  int addLabel( QString caption, bool bold=false, QString tooltip=0 );
  int addCheckBox( QString caption, bool *checked, QString tooltip=0 );
  int addLineEdit( QString caption, string *stringValue, QString tooltip=0 );
  int addLineEditF( QString caption, float min, float max, float *value, float decimals,  QString tooltip=0 );
  int addSpinBox( QString caption, int min, int max, int *value, int step, QString tooltip=0 );
  int addDblSpinBoxF( QString caption, float min, float max, float *value, int decimals, float step=0.1, QString tooltip=0 );
  int addComboBox( QString caption, QString barSepList, int *selIdx, QString tooltip=0 );
  int addRadioGrp( QString caption, QString barSepList, int *selIdx, QString tooltip=0, QString tooltipArr=0, bool checkable=false, bool *checked=0 );
  int addColorSel( QString caption, QColor *color, QString tooltip=0 );
  int beginGroupBox( QString caption, bool flat, QString tooltip=0, bool checkable=false, bool *checked=0 );
  void endGroupBox();
  int addCheckPrev( QString caption, bool *checked, chkbehaviour chkBeh, bool removeLabel, QString tooltip=0 );
  bool setStyleElem( int idx, string styleStr, bool bold=false );
  int setStylePrev( string styleStr, bool bold=false );
  //int setStyleElem( string styleStr, DlgType=DLG_ALL, int startIdx=0, int endIdx=INT_MAX );
  
private:    //## DATA:
  
  vector<DialogElement> elements;
  
  bool nextItemCheck;
  QPushButton *cancelButton;
  QPushButton *okButton;
  QVBoxLayout *vboxLayout;
  
  bool addToGroupBox;
  QVBoxLayout *groupBoxLayout;
  QVBoxLayout *layoutNextElement;
  
public slots:   //## SLOTS:
  
  void resizeMe();
  void accept();
  int exec();
};


//############################################################

//-------------------------------
//## SMALL GUI FUNCTIONS:

string qStringToString( QString qstr );
void MsgBox( string str );
bool MsgBoxYesNo( QWidget *parent, string str );
string InputBoxString( QWidget *parent, string title, string label, string defaultStr );
QString QStr( int number );
QString QStr( float number );
void setBold( QWidget *wid, bool bold );
void setTextColor( QWidget *wid, int r, int g, int b );
void setDefaultColorAndFont( QWidget *wid );

//############################################################

#endif

