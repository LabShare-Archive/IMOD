/*
 *  namewizard.cpp -- Special plugin for naming, colours and modifiying objects
 *
 */

/*****************************************************************************
 *   Copyright (C) 2010 by Andrew Noske - work done while at The National    *
 *   Center for Microscopy and Imaging Research at UCSD                      *
 *****************************************************************************/

/*  $Author$

*/

//############################################################

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

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
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGridLayout>
#include <QWheelEvent>
#include <QMouseEvent>
#include <QCloseEvent>
#include <QScrollArea>
#include <QKeyEvent>
#include <QEvent>
#include <qtoolbutton.h>
#include <QStringList>
#include <QListView>
#include <qfile.h>
#include <QTextStream>
#include <QMenu>

#include <QDesktopServices>
#include <QDir>
#include <QUrl>
#include <QProcess>

#include <qcompleter.h>

#include <qfiledialog.h>
#include <sstream>      // for formatting string output
#include <fstream>      // for input/output of binary files


#include "../../3dmod/pegged.xpm"
#include "../../3dmod/unpegged.xpm"

#include "_common_functions.h"
#include "customdialog.h"
#include "imodplugin.h"
#include "dia_qtutils.h"
#include "namewizard.h"


//############################################################

static NameWizardData plug = { 0, 0 };

//############################################################



//----------------------------------------------------------------------------
//
//          MAPPED FUNCTIONS:
//
//----------------------------------------------------------------------------

//--
//-- NOTE: Neither "imodPlugKeys" or "imodPlugMouse" are implemented here
//--       because this plugin doesn't need to do any special key
//--       interception or drawing.
//--

//------------------------
//-- MAPPED FUNCTION: Called by the imod plugin load function

const char *imodPlugInfo(int *type)
{
  if (type)
    *type = IMOD_PLUG_MENU + IMOD_PLUG_KEYS + IMOD_PLUG_MESSAGE + 
      IMOD_PLUG_MOUSE + IMOD_PLUG_EVENT;
    
  return("Name Wizard");
}


//------------------------
//-- MAPPED FUNCTION: Called when plugin window is started.
//-- Opens the plugin window for user interaction and initilizes data.
//-- See imodplug.h for a list of support functions.

void imodPlugExecute(ImodView *inImodView)
{

  if (plug.window) {      // if already open: bring window to front
    plug.window->raise();
    return;
  }
    
  //## INITIALIZE DATA:
  
  if( !plug.initialized )
  {
    plug.window->initValues();
    plug.window->loadSettings();
    
    plug.initialized = true;
  }
  plug.view = inImodView;
  ivwTrackMouseForPlugs(plug.view, 1);
  
  //## CREATE THE PLUGIN WINDOW:
  
  plug.window  = new NameWizard(imodDialogManager.parent(IMOD_DIALOG),"Name Wizard");
  plug.window->setWindowTitle("Name Wizard (*by SLASH*)");		// to help with our grant
	
  imodDialogManager.add((QWidget *)plug.window, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)plug.window, IMOD_DIALOG );
}


//------------------------
//-- MAPPED FUNCTION: Process wheel events

int imodPlugEvent(ImodView *vw, QEvent *event, float imx, float imy)
{
  return 0;
}



//############################################################

//----------------------------------------------------------------------------
//
//          NameWizard METHODS:
//
//----------------------------------------------------------------------------


//------------------------
//-- Convenience method, allowing you to create a new QAction. The new action
//-- triggers the method "member" (when clicked), is called "text", has the 
//-- status tip "tip" and is added to the specified "menu".
//-- Returns the newly created QAction

QAction *NameWizard::addAction( QMenu *menu, const char *member,
                                   QString text, QString tip )
{
  QAction *newAction = new QAction(text, this);
  newAction->setToolTip( tip );
  newAction->setStatusTip( tip );
  connect( newAction, SIGNAL(triggered()), this, member );
  menu->addAction( newAction );
  
  return newAction;
}


//## WINDOW CLASS CONSTRUCTOR:

static const char *buttonLabels[] = {(char*)"Done", (char*)"Video", (char *)"Help"};
static const char *buttonTips[]   = {(char*)"Close this plugin window",
	                             (char*)"See SLASH help videos showing \n"
	                                    "how to use this plugin",
	                             (char*)"Open help window"};

NameWizard::NameWizard(QWidget *parent, const char *name) :
  DialogFrame(parent, 3, buttonLabels, buttonTips, true, "Name Wizard", "", name)
{
  const int LAY_MARGIN   = 3;
  const int LAY_SPACING  = 3;
  
  QFont smallFont;
  smallFont.setPixelSize(12);
  
	//## RECOLOR MIDDLE "Video" BUTTON:
	
	mButtons[1]->setStyleSheet("color: rgb(150, 180, 255);");
	mButtons[1]->setFlat( true );
	mButtons[1]->setCursor( Qt::PointingHandCursor );
	mButtons[2]->setCursor( Qt::PointingHandCursor );
	
  //## CREATE HEADER:
  
  QWidget *widHeader = new QWidget();
  QHBoxLayout *layHeader = new QHBoxLayout( widHeader );
  layHeader->setSpacing(0);
  layHeader->setContentsMargins(10+2, 0, 0+2, 0);
  
  QLabel *lblColor = new QLabel( "<i>Color<i> &nbsp; " );
  lblColor->setToolTip( "Click the color box to quickly change the color. If you \n"
                        "wish to perform a 'Selection' action (eg: 'Delete') \n"
                        "you can select multiple objects using the check boxes. \n"
                        "If you click 'Match Colors' (below) then all objects \n"
                        "which have a match will change to the standard \n"
                        "color for that name (if one exists).");
  lblColor->setFixedWidth(15+20+20+4+4);
  lblColor->setAlignment(Qt::AlignRight);
  lblColor->setFont( smallFont );
  
  QLabel *lblObjName = new QLabel( "Object Name" );
  lblObjName->setToolTip( "Notice that as you type it should autocomplete with the \n"
                          "full list of names from the standard names and colors CVS \n"
                          "file/table. Whenever a name matches one of these entries a \n"
                          "'match' will appear. Note that if you want to preserve the \n"
                          "match, but add extra information use a full stop (.). This \n"
                          "allows you to (as just one example) to unique identify \n"
                          "individual compartments/surfaces by labeling objects \n"
                          "'Axon.001','Axon.002','Axon.003', etc.\n"
                          "Some groups prefer to separate surfaces into separate \n"
                          "compartments like this, but many find it easier to manage \n"
                          "to segment all axons in a single object (called 'Axon').");
  lblObjName->setMinimumWidth(200+4);
  lblObjName->setFont( smallFont );
  
  QLabel *lblMatch = new QLabel( "Match" );
  lblMatch->setToolTip( "This column is red if you have no match, but changes to \n"
                        "green when you have a match. The 'goal' is to try and have \n"
                        "ALL your objects match. Note that if it's light green then \n"
                        "it matches a standard name, but not the color. You can \n"
                        "correct  the color by holding your mouse over it to see \n"
                        "the red, green and blue values listed for that name (in \n"
                        "the tooltip), then clicking the color box to match these \n"
                        "colors... or you can simply click the 'Match Colors' button \n"
                        "at the bottom. If a hyperlink is provided for that entry, \n"
                        "then you can click 'match' to open a URL matching the \n"
                        "compartment name.");
  lblMatch->setFixedWidth(50+4);
  lblMatch->setFont( smallFont );
  
  lblIdentifier = new QLabel( "<i>UniqueID</i>" );
  lblIdentifier->setFixedWidth(90+5-(16-10));
  lblIdentifier->setFont( smallFont );
  
  btnChangeCol = new QPushButton( "" );
  btnChangeCol->setFixedSize(16,16);
  QMenu *colDropMenu = new QMenu(this);
  addAction( colDropMenu, SLOT(changeColsIdx0()),          "UniqueID",
             "Show the UniqueID for any matching names" );
  addAction( colDropMenu, SLOT(changeColsIdx1()),          "Contours",
             "Show the number and type of contours in each object" );
  btnChangeCol->setMenu( colDropMenu );
  
  widHeader->setLayout( layHeader );
  layHeader->addWidget( lblColor );
  layHeader->addWidget( lblObjName );
  layHeader->addWidget( lblMatch );
  layHeader->addWidget( lblIdentifier );
  
  layHeader->addWidget( btnChangeCol );
  
  //## CREATE SCROLL AREA WHERE LIST OF OBJECTS IS POPULATED:
  
  widList = new QWidget(this);
  layList = new QVBoxLayout(widList);
  layList->setSpacing(LAY_SPACING);
  layList->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
  widList->setLayout( layList );
  
  scrollArea = new QScrollArea( this );
  scrollArea->setBackgroundRole(QPalette::Midlight);
  scrollArea->setWidget( widList );
  //scrollArea->setMaximumSize(200,500);
  //scrollArea->setMaximumSize(250,700);
  
  completer = new QCompleter( plug.wordList, this);
  completer->setCaseSensitivity( Qt::CaseInsensitive );
  
  //## INITIALIZE EXTRA BUTTONS:
  
  QWidget *widgetBtns = new QWidget(this);
  layButtons = new QHBoxLayout(widgetBtns);
  layButtons->setSpacing(LAY_SPACING);
  layButtons->setContentsMargins(LAY_MARGIN, LAY_MARGIN, LAY_MARGIN, LAY_MARGIN);
  widgetBtns->setLayout(layButtons);
  
  refreshListButton = new QPushButton("Refresh List", widgetBtns);
	refreshListButton->setFocusPolicy(Qt::StrongFocus);
  connect(refreshListButton, SIGNAL(clicked()), this, SLOT(refreshObjList()));
  refreshListButton->setToolTip( "Refreshes the list of objects above.... \n"
                                 "useful if you've modified/deleted or added \n"
                                 "objects outside of this plugin");
  layButtons->addWidget(refreshListButton);
  
  selectionButton = new QPushButton("Selection", widgetBtns);
  selectionButton->setToolTip( "Contains several options to manage/modify \n"
                               "the objects which you have selected \n"
                               "by checking the box on the left.");
  layButtons->addWidget(selectionButton); 
  
  matchColorsButton = new QPushButton("Match Colors", widgetBtns);
  connect(matchColorsButton, SIGNAL(clicked()), this, SLOT(matchColors()));
  matchColorsButton->setToolTip( "Will automatically change the color of any object \n"
                                 "which has a name match to the suggested color for \n"
                                 "that name (if one is specified). You can see these \n"
                                 "colors by hovering your mouse over the word 'match'.");
  layButtons->addWidget(matchColorsButton); 
  
  editNameTableButton = new QPushButton("Standards Names", widgetBtns);
  editNameTableButton->setToolTip( "Contains several options to modify or reload the \n"
                                   "'standard names and colors csv file' which this \n"
                                   "plugin uses to autocomplete and match names.");
  layButtons->addWidget(editNameTableButton);
  
  //## INITIALIZE STATUS LABEL AND BOTTOM-BUTTONS:
  
  lblStatusLabel = new QLabel();
  lblStatusLabel->setTextFormat( Qt::RichText );
  lblStatusLabel->setContentsMargins( 5,0,3,0 );
  lblStatusLabel->setOpenExternalLinks(true);
  lblStatusLabel->setFont( smallFont );
  lblStatusLabel->setToolTip( "Shows the number of names and colors which match "
                              "the <b>standard names and colors cvs file</b>.<br><br>"
                              "If you want to change or add more standard names "
                              "and colors click 'Help' to learn how." );
  lblStatusLabel->setVisible( plug.showStatusLabel );
  
  //## ADD THE WIDGES TO THE MAIN LAYOUT, GENERATE THE OBJECT LIST AND RESIZE: 
  
  mLayout->addWidget(lblStatusLabel);
  mLayout->addWidget(widHeader);
  mLayout->addWidget(scrollArea);
  mLayout->addWidget(widgetBtns);
  
  
  refreshObjList();
  updateStatusLabel();
  
  this->adjustSize();
  this->setMinimumWidth(460);
  
  
  //## CREATE "ACTIONS" CONTEXT MENU AND CONNECTIONS:
  
  QMenu *selectionMenu = new QMenu(this);
  addAction( selectionMenu, SLOT(deleteSelected()),           "Delete Selected",
             "Deletes all selected (checked) objects." );
  addAction( selectionMenu, SLOT(mergeSelected()),            "Merge Selected",
             "Merges all the contours from the selected (checked) objects \n"
             "into the first object, and deletes the (now) empty objects." );
  addAction( selectionMenu, SLOT(duplicateSelected()),        "Duplicate Selected",
             "Takes a single selected object, and creates multiple \n"
             "new objects with the same properties (name, color etc) \n"
             "and can also copy contours (if specified). \n\n"
             "NOTE: To split surfaces into new objects use 'imodsortsurf -s'." );
  addAction( selectionMenu, SLOT(moveSelected()),             "Move/Reorder Selected",
             "Used to quickly reorder objects, by moving all selected \n"
             "objects together and down/up to the number you specify." );
  selectionMenu->addSeparator();
  addAction( selectionMenu, SLOT(batchRenameSelected()), "Batch Rename (.001, .002, etc)",
             "Batch renames all selected objects and includes the option \n"
             "to append numbers to the end (NewName.001, NewName.002, etc)." );
  selectionMenu->addSeparator();
  addAction( selectionMenu, SLOT(deselectAll()),             "Deselect/Uncheck All",
             "Deselect/uncheck all objects which have been ticked." );
  addAction( selectionMenu, SLOT(selectRange()),             "Select Range",
             "Select/deselect objects within a given the range of objects." );
  addAction( selectionMenu, SLOT(selectMatching()),          "Find and Select Names",
             "Select/deselect objects matching a string you enter." );
	
	selectionMenu->addSeparator();
	
  addAction( selectionMenu, SLOT(batchRecolorSelected()),    "Recolor Selected Objects",
						 "Allows you to set a color for all selected objects. \n\n"
						 "NOTE: The preferred way to do this is to go into the .cvs \n"
						 "file and change the color for a particular name, so the \n"
						 "correct colors will be applied when you click 'Match Colors'" );
  addAction( selectionMenu, SLOT(randomlyRecolor()),         "Apply Random Colors",
						 "Give random colors to the objects you've selected.\n"
						 "This feature can be useful to see if contours in different \n "
						 "objects belong in the same object in cases where you're \n"
						 "attempting to have only one surface per object." );
	
	selectionMenu->addSeparator();
	
	addAction( selectionMenu, SLOT(batchLabelChange()),        "Change Object Labels",
						 "Provides options which allow you to change the object's labels \n"
						 "to match the object hyperlink or uniqueId. Object labels are usually \n"
						 "hidden from the user, but can be useful in conversion programs such as \n"
						 "'imod2ccdbxml'." );
	
  selectionButton->setMenu( selectionMenu );
  
  
  QMenu *editTableMenu = new QMenu(this);
  addAction( editTableMenu, SLOT(editStandardNamesFile()),  "Edit 'standard_names.csv'",
             "Gives you several options to open the <b>standard names and colors cvs \n"
             "file</b> in Explorer/Finder, Excel or a text editor. After modifying \n"
             "the file don't forget to reload it with the option below." );
  addAction( editTableMenu, SLOT(loadNames()),    "Reload standard names from file",
             "Allows you to reload the <b>standard names and colors cvs file</b> \n"
             "from the default location, or (if specified) a different file location." );
  editTableMenu->addSeparator();
  addAction( editTableMenu, SLOT(helpPluginHelp()), "HELP: How do I add custom names?",
             "Takes you to the help page, where it explains how to load custom names" );
  addAction( editTableMenu, SLOT(helpNamingHelp()), "HELP: Why is this important?",
             "Takes you to a page on 'Naming Objects in IMOD' which explains \n"
             "the importance of consitent names, the conventions you should follow \n"
             "and several other important concepts such as ontologies!" );
  editTableMenu->addSeparator();
  addAction( editTableMenu, SLOT(moreSettings()), "Settings",
             "Lets you modify settings associated with this \n"
             "plugin (such as the nag screen)" );
  
  editNameTableButton->setMenu( editTableMenu );
  
  
  changeCols( plug.rightColIdx );
  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
}


//------------------------
//-- Counts the number of objects in the list which have been checked
//-- In addition to returning the number, it also populates the numbers of
//-- selected objects into "*objList" in the form "3,5,6,10".

int NameWizard::countSelectedObjs( string *objList )
{
  int numSelectedObjs = 0;
  for (int i=0; i<(int)lineItem.size(); i++ )
  {
    if( lineItem[i].chkObj->isChecked() )
    {
      objList->append( (numSelectedObjs) ? "," : "" );
      objList->append( toString(i+1) );
      numSelectedObjs++;
    }
  }
  return (numSelectedObjs); 
}

//------------------------
//-- Finds the first valid file path for the "standard names and colors cvs file"

QString NameWizard::getFirstValidFilePath()
{
  if( QFile::exists( plug.defaultFilePath ) )
    return plug.defaultFilePath;
  else
  {
    cout <<"File '"<< qStringToString(plug.defaultFilePath) <<"' does not exist" <<endl;
    
    return plug.secondaryFilePath;
  }
}

//------------------------
//-- Finds the first valid file path for the "standard names and colors cvs file"

void NameWizard::resizeEvent ( QResizeEvent * event )
{
  if( this->width() > 460 + 20 )
  {
    widList->setGeometry( 0, 0, this->width()-40, widList->height() );
  }
  else
    widList->adjustSize();
  
}





//## SLOTS:




//------------------------
//-- Used to initialize default values into NameWizardData.

void NameWizard::initValues()
{
  plug.rightColIdx         = 0;
  plug.showNagPersitentCsv = true;
  plug.showStatusLabel     = true;
  plug.addIdToObjLabels    = false;
	
  plug.defaultFilePath = QString(getenv("IMOD_CALIB_DIR"))
                       + QString("/standard_names_persistent.csv");
  plug.secondaryFilePath = QString(getenv("IMOD_DIR"))
                       + QString("/lib/imodplug/standard_names.csv");
	
	plug.shiftDown = false;
}

//------------------------
//-- Loads most of the settings for NameWizard from user preferences

void NameWizard::loadSettings()
{
  double savedValues[NUM_SAVED_VALS];
  
  int nvals = prefGetGenericSettings("NameWizard", savedValues, NUM_SAVED_VALS);
  
  if(nvals!=NUM_SAVED_VALS )
  {
    wprint("NameWizard: Could not load saved values");
    int result = QMessageBox::information( this, "-- Documentation --",
                              "If this is your first time using 'Name Wizard' \n"
                              "we HIGHLY recommended you click 'Help' \n"
                              "(at bottom of the plugin) to learn how it works! \n\n"
                              "                                   -- Andrew Noske\n"
															"\n"
															"NOTE: This plugin was possible thanks to\n"
															"the Neuroscience Information Framework",
                              QMessageBox::Yes, QMessageBox::No );
    if( result == QMessageBox::Yes )
      helpPluginHelp();
  }
  else
	{
		plug.rightColIdx         = savedValues[0];
		plug.showNagPersitentCsv = savedValues[1];
		plug.showStatusLabel     = savedValues[2];
		plug.addIdToObjLabels    = savedValues[3];
	}
	
	//## CHECK IF CSV FOUND: IF SO THEN LOAD IT!
  if( plug.showNagPersitentCsv && !QFile::exists( plug.defaultFilePath ) )
    showNagScreenPersistentCsv();
  loadNamesFromFile( getFirstValidFilePath() );
}


//------------------------
//-- Saves most of the settings within NameWizardData in user preferences
//-- so they will load next time the plugin is started.

void NameWizard::saveSettings()
{
  double saveValues[NUM_SAVED_VALS];
  
  saveValues[0]   = plug.rightColIdx;
  saveValues[1]   = plug.showNagPersitentCsv;
  saveValues[2]   = plug.showStatusLabel;
	saveValues[3]   = plug.addIdToObjLabels;
	
  prefSaveGenericSettings("NameWizard",NUM_SAVED_VALS,saveValues);
}


//------------------------
//-- Displays a nag screen asking the user if he wishes to copy a file accross to
//-- "plug.defaultFilePath".

void NameWizard::showNagScreenPersistentCsv()
{
  QString calibDir = QString(getenv("IMOD_CALIB_DIR"));
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int option = 0;
  CustomDialog ds("Missing Default CVS File",this);
  ds.addHtmlLabel( "This plugin works by matching your object's names/colors <br>"
                   "to a \"<b>standard names and colors cvs file</b>\". <br>"
                   "The preferred location for this file is: <br>" );
  ds.addHtmlLabel( "<a href='file://" + calibDir + "'>"
                   + plug.defaultFilePath + "</a><br>");
  ds.addHtmlLabel( "This file is not yet setup.... what would you like to do?" );
  ds.addRadioGrp( "... action:",
                  "use 'standard_names.cvs' (the default cvs file) for now|"
                  "copy 'standard_names.cvs' to this location|"
                  "open this directory in Finder/Explorer|"
                  "show me help!",
                  &option );
  
  ds.addLabel   ( "-----" );
  ds.addCheckBox( "always show this message", &plug.showNagPersitentCsv );
  ds.exec();
  if( ds.wasCancelled() )
    return;
  
  
  //## OPEN HELP OR DO NOTHING:
  
  if( option==0 )   // "use the default cvs file 'standard_names.cvs' instead"
  {
    return;
  }
  if( option==3 )   // "show me help!"
  {
    helpPluginHelp();
    return;
  }
  
  
  //## CHECK "IMOD_CALIB_DIR" EXISTS:
  
  
  if( calibDir.length() == 0 )
  {
    MsgBox( "ERROR: The environmental variable 'IMOD_CALIB_DIR' \n"
            "       has not been set. Cannot complete request." );
    return;
  }
  QDir dir;
  if( !dir.exists( calibDir ) )
  {
    if( MsgBoxYesNo( this, "WARNING: The 'IMOD_CALIB_DIR' directory does not exist. \n("
                     + qStringToString(calibDir)
                     + ")\nTry to create this folder now?" ) )
    {
      bool success = dir.mkpath( calibDir );
      if(success)
        MsgBox( "New folder created at: \n  " + qStringToString(calibDir) );
      else
        MsgBox( "Failed to create new folder at: \n  " + qStringToString(calibDir) 
                + "\nYou may have to create this folder yourself (with permissions)." );
    }
  }
  
  //## TRY TO OPEN FOLDER OR COPY FILE:
  
  if( option==2 )   // "open this directory in Finder/Explorer"
  {
    QStringList dirPathList;
    dirPathList << QString(getenv("IMOD_CALIB_DIR"));
    QProcess *processFinder = new QProcess(NULL);
    processFinder->start("open", dirPathList );
    
    return;
  }
  else if( option==1 )   // "copy 'standard_names.cvs' to this location"
  {
    QFile defaultFile( plug.secondaryFilePath );
    bool success = defaultFile.copy( plug.defaultFilePath );
    if(success)
      MsgBox( "File was successfully copied from: \n  "
              + qStringToString(plug.secondaryFilePath) + "\nto the location: \n  :"
              + qStringToString(plug.defaultFilePath) );
    else
      MsgBox( "ERROR: Was unable to copy the file from: \n  "
              + qStringToString(plug.secondaryFilePath) + "\nto the location: \n  :"
              + qStringToString(plug.defaultFilePath)
              + "\nYou may have to perform this operation yourself (with permissions).");
  }
}


//------------------------
//-- Loads values from a "standard names and colors cvs file" into the
//-- "plugs.nameList" vector (which is used to verify names match)...
//-- and also uses this vector to popuulate the "plugs.wordList" vector
//-- which is used to provide autocomplete on the text fields.
//--
//-- The format of this CSV (comma seperated value) file should be as follows:
//--     (1)Name, (2)Red, (3)Green, (4)Blue, (5)Hyperlink, (6)UniqueID,
//--     (7)Description, (8)Super Category, (9)Synonym(s)
//-- 
//-- For more information refer to "namewizard.html"
//-- 
//-- WARNING: If saved from Excel:Mac may only read one line. Make sure
//--          is saved as "Windows"

int NameWizard::loadNamesFromFile( QString filePath )
{
  //## TRY TO OPEN FILE:
  
  QFile file( filePath );
  if ( !file.open( QIODevice::ReadOnly | QIODevice::Text ) )
  {
    MsgBox("ERROR: Could not find/open file: \n " + qStringToString( filePath ) );
    return 0;
  }
  
  //## CLEAR NAME AND NAME LIST AND POPULAR NEW ENTRIES:
  
  plug.nameList.clear();
  
  int linesRead = 0;
  QString line;
  while ( !file.atEnd() )
  {
    line = file.readLine(); // line of text excluding '\n'
    linesRead++;
    
    if( line.length() > 1 && line[0] != (QChar)'#' )
    {
      QStringList list = line.split(",");
      
      NameEntry nameEntry;
      
      if( (int)list.size() >= 1 )   nameEntry.name          = list[0].trimmed();
      if( (int)list.size() >= 2 )   nameEntry.red           = list[1].trimmed();
      if( (int)list.size() >= 3 )   nameEntry.green         = list[2].trimmed();
      if( (int)list.size() >= 4 )   nameEntry.blue          = list[3].trimmed();
      if( (int)list.size() >= 5 )   nameEntry.hyperlink     = list[4].trimmed();
      if( (int)list.size() >= 6 )   nameEntry.identifier    = list[5].trimmed();
      if( (int)list.size() >= 7 )   nameEntry.description   = list[6].trimmed();
      if( (int)list.size() >= 8 )   nameEntry.superCat      = list[7].trimmed();
      if( (int)list.size() >= 9 )   nameEntry.synonyms      = list[8].trimmed();
      
      plug.nameList.push_back( nameEntry );
    }
  }
  
  //## SHOW OUTCOME:
  
  string filePathStr = qStringToString(filePath);
  wprint("");
  wprint("NAME WIZARD: %d names loaded from", (int)plug.nameList.size() );
  //if( filePath == plug.defaultFilePath )
    wprint(" '%s' ", filePathStr.c_str() );
  
  if( linesRead == 0 )
  {
    MsgBox("WARNING: This file appears to be empty: \n" 
           + qStringToString( filePath ) );
    return 0;
  }
  if( linesRead == 1 )
  {
    MsgBox("WARNING: Could only read one line in the file \n"
           "  You may have to save it as a Windows .csv file" );
  }
  
  //## CLEAR WORD LIST (FOR AUTOCOMPLETE) AND REPOPULATE:
  
  plug.wordList.clear();
  
  for(int i=0; i<(int)plug.nameList.size(); i++)
    plug.wordList << plug.nameList[i].name;
  
  flush(cout);
  
  return (int)plug.nameList.size();
}




//------------------------
//-- Checks if a refresh is needed by comparing the names and number objects
//-- in the model against the names and number of objects in the list.

bool NameWizard::ifRefreshNeeded()
{
  Imod *imod  = ivwGetModel(plug.view);
  
  //## IF MULTIPLE NAMES NEED TO BE UPDATED: ASK USER TO REFRESH
  
  int diffInObjs = osize(imod) - (int)lineItem.size();
  
  int numNamesNeedingUpdate = 0;
  for (int i=0; i<(int)lineItem.size() && i<osize(imod); i++)
  {
    QString currObjName = (QString)imodObjectGetName( getObj(imod,i) );
    if( lineItem[i].prevName != currObjName )
      numNamesNeedingUpdate++;
  }
  
  bool refreshNeeded = ( diffInObjs != 0 || numNamesNeedingUpdate > 0 );
  if(refreshNeeded)
    wprint("\aNAME WIZARD: Needs refresh");
  
  return refreshNeeded;
}

//------------------------
//-- A callback function which calls "checkIfRefreshNeeded" and if a difference 
//-- is detected between the objects in the model and objects in the list
//-- then a message box is displayed advising the user to refresh the list.

void NameWizard::alertIfRefreshNeeded()
{
  static int numWarnings = 0;
  if( ifRefreshNeeded() )
  {
    if(numWarnings > 10)
      return;
		if( !refreshListButton->hasFocus() )
			MsgBox( "Changes to the object list outside of this plugin have "
						  "been detected\n\n"
              "Please hit 'Refresh List' before you continue" );
		refreshListButton->setFocus( Qt::OtherFocusReason );
    numWarnings++;
  }
}

//------------------------
//-- A callback function for whenever an object name in the list is changed.
//-- When this happens, this function looks through all the entries and updates
//-- any object names which have been changed by the user. 

void NameWizard::nameModified()
{
  Imod *imod  = ivwGetModel(plug.view);
  
  //## RENAME ANY OBJECTS WHICH HAVE BEEN MODIFIED BY THE USER:
  
  int numNamesModified = 0;
  for (int i=0; i<(int)lineItem.size(); i++)
  {
    ObjectLineItem &item = lineItem[i];
    if( item.prevName != item.txtObjName->text() )
    {
      if( i>=osize(imod) ) {
				if( !refreshListButton->hasFocus() )
					MsgBox("This object has been deleted outside the plugin \n"
								 "Please click 'Refresh List' and then continue.");
				refreshListButton->setFocus( Qt::OtherFocusReason );
        return;
      }
      refreshObjItem(i);
      Iobj *obj  = getObj(imod,i);
      string newObjStr = qStringToString( item.txtObjName->text() );
      imodObjectSetName( obj, (char *)newObjStr.c_str() );
      numNamesModified++;
    }
  }
  
  if( numNamesModified )
  {
    updateStatusLabel();
    imodUpdateObjectDialogs();
  }
}

//------------------------
//-- A callback function for whenever a checkbox is clicked. This function
//-- records which checkbox was checked/unchecked on the last call, and if the
//-- shift is held down, it will click it will check or uncheck the entire
//-- selection between the previously click and currently clicked checkbox.

void NameWizard::checkClicked()
{
	Imod *imod  = ivwGetModel(plug.view);
	
	static int  prevChkClicked = -1;			// |-- used to record the index of the last 
	static bool prevChkChecked = false;		// |   checkbox clicked and it's value the last 
																				// |   time this function was called
	
	int  currChkClicked = -1;							// |-- will be set to the index of the checkbox
	bool currChkChecked = false;					// |   and it's value during this function call
	
	
	//## DETERMINE WHICH INDEX WAS CLICKED:
	
	for (int i=0; i<(int)lineItem.size(); i++)
		if( QObject::sender() == lineItem[i].chkObj )
		{
			currChkClicked = i;
			currChkChecked = lineItem[i].chkObj->isChecked();
			break;
		}
	if( currChkClicked == -1 )		// if couldn't find valid index: exit early
		return;
	
	//## IF SHIFT KEY IS NOT DOWN: RECORD INDEX, SHOW HELPFUL TOOLTIPS AND EXIT:
	
	if(!plug.shiftDown)
	{
		prevChkClicked = currChkClicked;
		prevChkChecked = currChkChecked;
		
		for (int i=0; i<(int)lineItem.size(); i++)
			if( i<currChkChecked+20 && i>currChkChecked-20 )
			{
				if( currChkChecked )
					lineItem[i].chkObj->setToolTip("Hold [shift] and click to <br>"
																				 "<b>select</b> objects " +
																				 QStr(currChkClicked+1) + " to " + QStr(i+1) );
				else
					lineItem[i].chkObj->setToolTip("Hold [shift] and click to <br>"
																				 "<b>deselect</b> objects<br>" +
																				 QStr(currChkClicked+1) + " to " + QStr(i+1) );
			}
		return;
	}
	
	for (int i=0; i<(int)lineItem.size(); i++)		// remove tooltips from all checkboxes
		if( lineItem[i].chkObj->toolTip() != "" )
			lineItem[i].chkObj->setToolTip( "" );
	
	//## IF PREVIOUS INDEX REGISTERED CHECKE VALUES MATCH:
	//## CHECK ALL BOXES IN BETWEEN AND RESET INDEX
	
	if( prevChkClicked != -1 )
	{
		int minIdx = MAX( MIN( currChkClicked, prevChkClicked ), 0 );
		int maxIdx = MIN( MAX( currChkClicked, prevChkClicked ), (int)lineItem.size()-1 );
		
		for( int i=minIdx; i<=maxIdx; i++ )
			lineItem[i].chkObj->setChecked( prevChkChecked );
		
		prevChkClicked = -1;
		prevChkChecked = false;
	}
}


//------------------------
//-- A callback function to change the value of the right most column.
//-- If the type changes it calls "refreshObjList".

void NameWizard::changeCols( int i )
{
  bool idxChanged = ( plug.rightColIdx != i );
  plug.rightColIdx = i;
  
  if( i==0 )
  {
    lblIdentifier->setText( "<i>UniqueID</i>" );
    lblIdentifier->setToolTip( "If an object name has a match and a UniqueID is given \n"
                               "for that name, then it will appear here. UniqueIDs \n"
                               "are important, as a way to uniquely identify a \n"
                               "compartment/organelle/protein without relying on \n"
                               "the object name (since the names and classification \n"
                               "of organelles change surprisingly often over time).");
  }
  else if( i==1 )
  {
    lblIdentifier->setText( "<i>Contours</i>" );
    lblIdentifier->setToolTip( "Shows the number of contours in each object");
  }
  
  if( idxChanged )
    refreshObjList();
}

//------------------------
//-- Refreshes/regenerates the main list of object names displayed in the plugin.
//-- it achieves this by enlarging/reducing the "lineItem" array (to match the
//-- current number of objects), initializing/setting up widgets in a table
//-- layout, and calling "refreshObjItem" for each lineItem

void NameWizard::refreshObjList()
{
  Imod *imod  = ivwGetModel(plug.view);
  
  int numObjs = osize( imod );
  
  //## HIDE AND REMOVE ANY EXISTING LINES:
  
	int prevSize = (int)lineItem.size();
	
	for( int i=numObjs; i<(int)lineItem.size(); i++ )	// hide and remove superfluous lines
	{
		lineItem[i].widLine->setVisible( false );
		layList->removeWidget( lineItem[i].widLine );
	}
	
  /*for( int i=0; i<(int)lineItem.size(); i++ )
  {
    lineItem[i].widLine->setVisible( false );  
      // had strange artifact where if I added, then deleted then added an object
      // an old line would get left in the background, so I make invisible first
    layList->removeWidget( lineItem[i].widLine );
  }*/
  
  lineItem.resize( numObjs );
  
  QFont smallFont;
  smallFont.setPixelSize(10);
  QFont smallerFont;
  smallerFont.setPixelSize(9);
  
  //## SETUP ANY NEW LINES WHICH HAVE BEEN ADDED BUT NOT YET SETUP:
  
  for(int o=0; (int)o<lineItem.size() && o<osize(imod); o++)
  {
    ObjectLineItem &item = lineItem[o];
    Iobj *obj  = getObj(imod,o);
    
    //## UPDATE VALUES FROM OBJECTS:
    
    item.prevName = (QString)( imodObjectGetName(obj) );
    
    float red, green, blue;
    imodObjectGetColor( obj, &red, &green, &blue );
    item.prevColor.setRgb( red*255, green*255, blue*255 );
    string colorStr = "background-color: rgb(" + toString(red*255) + ","
      + toString(green*255) + "," + toString(blue*255) + ");";
    
    int numConts = csize(obj);
    QString contType = isObjClosed(obj) ? "(closed)" : "(open)";
    item.numContsStr = QStr(numConts) + " " + contType;
    if( numConts==0 )
      item.numContsStr = "EMPTY " + contType;
    
    
    //## IF NOT ALREADY, INITIALIZE GUI ELEMENTS:
    
    if( !item.setup )
    {
      item.widLine       = new QWidget(widList);
      item.layLine       = new QHBoxLayout( item.widLine );
      item.layLine->setSpacing(4);
      item.layLine->setContentsMargins(2, 2, 2, 2);
      
      item.chkObj        = new QCheckBox(item.widLine);
      item.chkObj->setFixedWidth(15);
			item.chkObj->setToolTip("Use [shift] to select a range of points");
      connect( item.chkObj,SIGNAL(clicked()), this,SLOT(checkClicked()) );		// NEW
			
      item.lblObjNum     = new QLabel(item.widLine);
      item.lblObjNum->setFixedWidth(20);
      
      item.btnColor      = new ColorButton( item.prevColor, item.widLine);
      connect( item.btnColor, SIGNAL(released()), this, SLOT(updateColors()) );    
                // CONNECTION
      
      item.txtObjName    = new QLineEdit(item.widLine);
      item.txtObjName->setMinimumWidth( 200 );
      item.txtObjName->setCompleter(completer);
      connect( item.txtObjName,SIGNAL(textEdited(QString)), this,SLOT(nameModified()) );
      connect( item.txtObjName,SIGNAL(editingFinished()), this,SLOT(nameModified()) );
      connect( item.txtObjName,SIGNAL(textEdited(QString)), this,SLOT(alertIfRefreshNeeded()) );
                // CONNECTION
      
      item.lblLink       = new QLabel(item.widLine);
      item.lblLink->setTextFormat( Qt::RichText );
      item.lblLink->setOpenExternalLinks(true);
      item.lblLink->setFont( smallFont );
      item.lblLink->setFixedWidth( 50 );
      item.lblLink->setAlignment( Qt::AlignHCenter | Qt::AlignVCenter );
      
      item.txtIdentifier = new QLineEdit(item.widLine);
      item.txtIdentifier->setFont( smallerFont );
      item.txtIdentifier->setStyleSheet( "color: rgb(100, 100, 100); background-color: rgba(255,255,255,20);" );
      item.txtIdentifier->setReadOnly( true );
      item.txtIdentifier->setFrame( false );
      item.txtIdentifier->setAlignment( Qt::AlignVCenter );
      item.txtIdentifier->setFixedWidth( 90 );
      
      item.widLine->setLayout( item.layLine );
      item.layLine->addWidget( item.chkObj );
      item.layLine->addWidget( item.lblObjNum );
      item.layLine->addWidget( item.btnColor );
      item.layLine->addWidget( item.txtObjName );
      item.layLine->addWidget( item.lblLink );
      item.layLine->addWidget( item.txtIdentifier );
      
      item.setup = true;
    }
    
    //## UPDATE GUI ELEMENTS:
    
    item.lblObjNum->setText( QStr(o+1) );
    item.txtObjName->setText( item.prevName );
    item.btnColor->setColor( item.prevColor );
    item.txtIdentifier->setText( "" );
    
    refreshObjItem(o);
  }
  
	//## MAKE NEW LINES VISIBLE AND RESIZE:
	
	for( int i=prevSize; i<(int)lineItem.size(); i++ )	// show and add all new lines
	{
		lineItem[i].widLine->setVisible( true );
		layList->addWidget( lineItem[i].widLine );
	}
	
	int LINE_HEIGHT = 30;
	widList->setFixedHeight( LINE_HEIGHT * (int)lineItem.size() + 4 );
	
	
  //## REPOPULATE AND MAKE LINES VISIBLE:
  
  /*for(int o=0; (int)o<lineItem.size() && o<osize(imod); o++)
  {
    lineItem[o].widLine->setVisible( true );
    layList->addWidget( lineItem[o].widLine );
  }*/
  
  updateStatusLabel();
  resizeEvent( NULL );
}

//------------------------
//-- Counts the number of unmatched names and colors and uses these values to
//-- update the text and background colors of the status label ("lblStatusLabel")

void NameWizard::updateStatusLabel()
{
  //## COUNT AND DISPLAY NUMBER OF UNMATCHED COLORS AND NAMES:
  
  Imod *imod  = ivwGetModel(plug.view);
  int numMissingNames    = 0;
  int numUnmatchedNames  = 0;
  int numUnmatchedColors = 0;
  
  for(int o=0; (int)o<lineItem.size() && o<osize(imod); o++)
  {
    ObjectLineItem &item = lineItem[o];
    if( item.prevName.length() == 0 )
      numMissingNames++;
    if( item.hasMatch == false )
      numUnmatchedNames++;
    if( item.matchHasColor && item.matchColor != item.btnColor->color )
      numUnmatchedColors++;
  }
  
  //## UPDATE THE STATUS LABEL:
  
  QString text      = "";
  QString styleStr  = "";
  if( numMissingNames )
  {
    text = "<b>(WARNING)</b> empty names: <b>" +QStr(numMissingNames)+ "</b>";
    if( numUnmatchedColors )
      text += ", unmatched colors: <b>" +QStr(numUnmatchedColors)+ "</b>";
    text += " &nbsp; <b>:-(</b>";
    styleStr = "color: rgb(255, 255, 255); background-color: rgb(255, 40, 40);";
  }
  else if( numUnmatchedNames )
  {
    text = "<b>(WARNING)</b> unmatched names: <b>" +QStr(numUnmatchedNames)+ "</b>";
    if( numUnmatchedColors )
      text += ", unmatched colors: <b>" +QStr(numUnmatchedColors)+ "</b>";
    text += " &nbsp; :(";
    styleStr = "color: rgb(255, 255, 255); background-color: rgb(255, 100, 100);";
  }
  else if( numUnmatchedColors )  
  {
    text = "unmatched colors: <b>" + QStr(numUnmatchedColors) + "</b>";
    styleStr = "color: rgb(0, 0, 0); background-color: rgb(180, 255, 180);";    
  }
  else
  {
    text = "<b>(CONGRATS!)</b> All names and colors conform to standards! &nbsp; <b>:)</b>";
    styleStr = "color: rgb(0, 0, 0); background-color: rgb(100, 255, 100);";
  }
  
  lblStatusLabel->setText( text );
  lblStatusLabel->setStyleSheet( styleStr );
}



//------------------------
//-- Refreshes a single line to match the name and color of the object, and checks
//-- if the name and color matches an entries within "plugs.nameList"

void NameWizard::refreshObjItem( int itemIdx )
{
  if( itemIdx < 0 || itemIdx >= (int)lineItem.size() )
    return;
  
  //## UPDATE VALUES FROM LINE EDIT AND COLOR BOX:
  
  ObjectLineItem &item = lineItem[itemIdx];
  item.reset();
  item.prevName = item.txtObjName->text();
  //item.prevColor = item.btnColor->getColor();
  item.txtIdentifier->setText( "" );
  if( plug.rightColIdx == 1)
    item.txtIdentifier->setText( item.numContsStr );
  
  //## GET OBJECT NAME, GET RID OF ANYTHING AFTER DOT OR HASH, AND CHECK IF EMPTY:
  
  QString redObjName = item.txtObjName->text();
  
  if( redObjName.contains(".") )
    redObjName = redObjName.mid( 0, redObjName.indexOf(".") );
  else if( redObjName.contains("#") )
    redObjName = redObjName.mid( 0, redObjName.indexOf("#") );
	
  if( redObjName.length() == 0 )
  {
    item.lblLink->setText( "<b>EMPTY</b>" );
    item.lblLink->setStyleSheet("color: rgb(255, 255, 255); background-color: rgb(255, 0, 0);");
    return;
  }
  
  //## SEARCH FOR A NAME ENTRY MATCHING THE NAME CURRENTY ENTERED FOR THE OBJECT:
  
  item.hasMatch = false;
  item.matchHasColor = false;
  item.colorsMatch = false;
  
  NameEntry match;
  for(int i=0; i<(int)plug.nameList.size(); i++ )
  {
    if( redObjName == plug.nameList[i].name )
    {
      item.hasMatch = true;
      match = plug.nameList[i];
      break;
    }
  }
  
  if( item.hasMatch )
  {
		item.nameEntry = match;
		
    item.lblLink->setText( "match" );
    if( match.hyperlink.length() )
      item.lblLink->setText( "<a href='" + match.hyperlink + "'>match</a>" );
    item.lblLink->setStyleSheet("color: rgb(0, 0, 0); background-color: rgb(100, 255, 100);");
    if( plug.rightColIdx == 0 )
      item.txtIdentifier->setText( match.identifier );
    
    QString description = match.description;
    QString superCat    = match.superCat;
    QString synonyms    = match.synonyms;    
    QString colorStr    = "(" + match.red +","+ match.green + "," + match.blue + ")";
    
    item.matchHasColor = ( match.red.length()>0 && match.green.length()>0 && match.blue.length()>0 );
    
    if( item.matchHasColor )
    {
      item.matchColor.setRgb( match.red.toInt(), match.green.toInt(), match.blue.toInt() );
      colorStr += " <font color='" + item.matchColor.name() + "'><b>|||||</b></font>";
      //cout << qStringToString( colorStr ) << endl;       //%%%%%
      
      item.colorsMatch = (item.matchColor == item.btnColor->color);
      if( item.colorsMatch )
      {
        colorStr += " (maches)";
      }
      else
      {
        item.lblLink->setStyleSheet("color: rgb(0, 0, 0); background-color: rgb(180, 255, 180);");
        colorStr += " (doesn't match current color)";
      }
    }
    
    if( description.length() == 0 )  description = "<i>NONE ENTERED YET</i>";
    if( superCat.length() == 0 )     superCat = "<i>NONE ENTERED YET</i>";
    if( synonyms.length() == 0 )     synonyms = "<i>-</i>";
    if( colorStr.length() == 0 )     colorStr = "<i>NO RECOMMENEDED COLOR YET</i>";
    
    QString info = "<b>Name:</b> " + redObjName + "<br>"
                   "<b>Color:</b> " + colorStr + "<br>"
                   "<b>Description:</b> " + description + "<br><br>"
                   "<b>Super-category:</b> " + superCat + "<br>"
                   "<b>Synonyms:</b> " + synonyms;
    item.txtObjName->setToolTip( info );
    item.lblLink->setToolTip( info );
  }
  else
  {
    item.lblLink->setText( "<b>no match</b>" );
    item.lblLink->setStyleSheet("color: rgb(255, 255, 255); background-color: rgb(255, 100, 100);");
  }
  
}



//------------------------
//-- Presents the user with the options to load or reload the "standard names and
//-- colors cvs file" from the default location, or load their own file.

void NameWizard::loadNames()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static int option = 0;
  CustomDialog ds("Reload / Load Names from File",this);
  ds.addRadioGrp( "Load names from:",
                  "default location|"
                  "let me choose",
                  &option );
  
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  //## LOAD STANDARD NAME AND COLOR ENTRIES FROM FILE:
  
  int entriesLoaded = 0;
  if( option==0 )   // "default location"
  {
    entriesLoaded = loadNamesFromFile( getFirstValidFilePath() );
    MsgBox( toString(entriesLoaded) + " entries loaded" );
  }
  else if( option==1 )   // "let me choose"
  {
    QString filePath =
      QFileDialog::getOpenFileName( this, "Open CVS File", 
                                    QString(getenv("IMOD_DIR")), "CVS Files (*.csv)" );
    if( filePath.length() > 0 )
    {
      entriesLoaded = loadNamesFromFile( filePath );
      MsgBox( toString(entriesLoaded) + " valid entries loaded" );
    }
  }
}



//------------------------
//-- Presents a hyperlink to the default "standard names and colors cvs file" and gives
//-- the user a number of options to load this file in a file explorer, text editor
//-- or spreadsheet program.

void NameWizard::editStandardNamesFile()
{
  static int option = 0;
  QString cvsFilePath = getFirstValidFilePath();
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  CustomDialog ds("Edit Standard Name and Color Table",this);
  ds.addHtmlLabel( "This plugin works by matching your object names/colors <br>"
                  "to a \"<b>standard names and colors cvs file</b>\". <br>"
                  "<i>To change or add names/colors you must edit the file <br>"
                  "below using a text editor or spreadsheet program, save then reload: </i><br>");
  ds.addHtmlLabel( "<a href='file://" + cvsFilePath + "'>"
                   + cvsFilePath + "</a><br>");
  ds.addRadioGrp( "action:",
                  "open in Finder/Explorer|"
                  "edit in Text Editor|"
                  "edit in Excel|"
                  "view in Browser|"
                  "do nothing",
                  &option );
  
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  //## TRY TO OPEN FILE / FOLDER IN CHOSEN APPLICATION TYPE:
  
  QStringList filePathList;
  filePathList << cvsFilePath;
  QStringList possAppPaths;
  
  
  if( option==0 )   // "open in Finder/Explorer"
  {
    QStringList dirPathList;
    if( cvsFilePath == plug.defaultFilePath )
      dirPathList << QString(getenv("IMOD_CALIB_DIR"));
    else
      dirPathList << ( QString(getenv("IMOD_DIR")) + QString("/lib/imodplug/") );                
    
    QProcess *processFinder = new QProcess(NULL);
    processFinder->start("open", dirPathList );
    
    return;
  }
  else if( option==1 )   // "open in Text Editor"
  {
    possAppPaths << "/Applications/TextEdit.app"
                 << "C:\\windows\\Notepad.exe";
  }
  else if( option==2 )   // "open in Excel"
  {
    possAppPaths << "/Applications/Microsoft Office 2008/Microsoft Excel.app"
                 << "/Applications/Microsoft Office 2004/Microsoft Excel.app"
                 << "/Applications/Microsoft Office X/Microsoft Excel.app"
                 << "C:\\Program Files\\Microsoft Office\\Office12\\Excel.exe";
  }
  else if( option == 3)   // "view in Browser"
  {  
    QDesktopServices::openUrl(QUrl( "file://" + cvsFilePath ));   
                  // NOTE: with "file://" added this opens in default progam under OS X
    return;
  }
  else if( option == 4)   // "do nothing"
  {
    return;
  }
  
  QString appPath = "";
  for(int i=0; i<possAppPaths.size(); i++)
    if( QFile::exists( possAppPaths[i] ) )
      appPath = possAppPaths[i];
  
  if( appPath.length()==0 )
  {
    if( MsgBoxYesNo( this, "Could not find path to a matching application. \n\n"
                           "Try loading file in default program?") )
    {
      QProcess *processDefault = new QProcess(NULL);
      processDefault->start("open", filePathList );
    }
  }
  else
  {
    QProcess *process = new QProcess(NULL);
    process->start(appPath, filePathList );
  }
  
}



//------------------------
//-- Iterates through all the color buttons and updates the color of any objects
//-- which don't match. During this process it returns the number of
//-- unmatched colors.

int NameWizard::updateColors()
{
  Imod *imod  = ivwGetModel(plug.view);
  
  int numUnmatchedColors = 0;
  int colorsUpdated = 0;
  
  for(int i=0; i<(int)lineItem.size() && i<(int)osize(imod); i++ )
  {
    ObjectLineItem &item = lineItem[i];
    Iobj *obj  = getObj(imod,i);
    
    QColor currCol = item.btnColor->color;
    
    if( currCol != item.prevColor )
    {
      setObjColor( obj, currCol.red(), currCol.green(), currCol.blue() );
      item.prevColor = currCol;
      refreshObjItem(i);
      colorsUpdated++;
    }
    
    if( item.matchHasColor && item.matchColor != currCol )
      numUnmatchedColors++;
  }
  
  if( colorsUpdated )
  {
    updateStatusLabel();
    imodUpdateObjectDialogs();
    ivwRedraw( plug.view );
  }
  
  return numUnmatchedColors;
}

//------------------------
//-- For any object name that matches an entry in "plug.nameList", this function
//-- will change the object's color to match the suggested color for this entry
//-- if a suggested color matches.
//-- NOTE: Before this happens the function checks to see if mismatches exist, and 
//-- gives the user a Yes/No option to continue.

void NameWizard::matchColors()
{
  int numUnmatchedColors = updateColors();
  
  if( numUnmatchedColors == 0 )
  {
    MsgBox( "All colors match the listed colors!" );
    return;
  }
  
  if( !MsgBoxYesNo(this, "There are " + toString(numUnmatchedColors)
                   + " colors which do not match the listed entries... \n"
                   "Would you like to update these to match the matched color values?" ))
    return;
  
  
  for(int i=0; i<(int)lineItem.size(); i++ )
  {
    ObjectLineItem &item = lineItem[i];
    if( item.matchHasColor && item.matchColor != item.btnColor->color )
      item.btnColor->setColor( item.matchColor );
  }
  updateColors();
}





//------------------------
//-- Gives the option to delete all objects which have been selected
//-- (i.e. their checkboxes ticked)

void NameWizard::deleteSelected()
{
  Imod *imod  = ivwGetModel(plug.view);
  string objListStr = "";
  int numSelectedObjs = countSelectedObjs( &objListStr );
  
  if( numSelectedObjs==0 )
  {
    MsgBox("NOTE: You must use the checkboxes to select \n"
           "   one or more object before you delete them");
  }
  else if( MsgBoxYesNo(this, "Are you sure you want to delete " + toString(numSelectedObjs) 
                       + " objects? \n\n"
                       + "This operation isn't always undoable (so save first)! \n"
                       + "OBJECTS TO DELETE: " + objListStr ) )
  {
    for (int i=(int)lineItem.size()-1; i>=0; i-- )
    {
      if( lineItem[i].chkObj->isChecked() && i<osize(imod) )
      {
        undoObjectRemoval( plug.view, i );   // REGISTER UNDO
        imodDeleteObject( imod, i );
        lineItem[i].chkObj->setChecked(false);
      }
    }
    undoFinishUnit( plug.view );          // FINISH UNDO
  }
  
  refreshObjList();
  imodUpdateObjectDialogs();
  ivwRedraw( plug.view );
}



//------------------------
//-- If the user has checked two or more objects, all of the contours from these
//-- objects are merged into the first object and (if specified by the user)
//-- the now empty objects are deleted.
//-- 
//-- NOTE: To move all contours from the current object to another David uses
//--       "imodMoveAllContours(ImodView, int obNew)" to call "imod_contour_move(obNew)"
//--       See: file://localhost/Users/a.noske/Documents/MACMOD/3dmod/imod_edit.cpp


void NameWizard::mergeSelected()
{
  Imod *imod  = ivwGetModel(plug.view);
  string objListStr = "";
  int numSelectedObjs = countSelectedObjs( &objListStr );
  
  if( numSelectedObjs<=1 )
  {
    MsgBox("NOTE: You must use the checkboxes to select \n"
           "   TWO or more object before you delete them");
    return;
  }
  
  //## FIND FIRST CHECK OBJECT AND DETERMINE IF ALL CHECKED OBJECTS SAME TYPE:
  
  int firstObjIdx = -1;
  bool firstObjClosed; 
  bool objsMatchType  = true;
  int totalConts = 0;
  
  for (int i=0; i<(int)lineItem.size() && i<osize(imod); i++ )
  {
    if( lineItem[i].chkObj->isChecked() )
    {
      Iobj *obj = getObj(imod,i);
      
      if( firstObjIdx == -1 )
      {
        firstObjIdx = i;
        firstObjClosed = isObjClosed(obj);
        continue;
      }
      
      totalConts += csize(obj);
      if( isObjClosed(obj) != firstObjClosed )
        objsMatchType = false;
    }
  }
  
  if( !objsMatchType )
  {
    MsgBox( "Some of these objects are different types \n"
            "(some are 'open' and some 'closed').\n"
            "This operation will only be applied if all \n"
            "objects are of the same time." );
    return;
  }
  
  
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  static bool deleteAfterMerge = true;
  static bool keepContsInOtherObjs = false;
  CustomDialog ds("Merge Objects",this);
  ds.addLabel( "Are you sure you want to merge " + QStr(numSelectedObjs) 
              + " objects into object " + QStr(firstObjIdx+1)
              + " (the first one)? \n\n"
              + "This operation is NOT undoable (so save a copy first)! \n"
              + "OBJECTS SELECTED FOR MERGE: " + QString(objListStr.c_str()) + "\n"
              + "TOTAL CONTOURS TO MOVE: " + QStr(totalConts) );
  ds.addCheckBox( "delete empty objects after merge", &deleteAfterMerge );
  ds.addCheckBox( "copy, instead of move, contours", &keepContsInOtherObjs,
                  "if the above checkbox is off contours will not be removed \n"
                  "from any object (the second selected object, but will instead \n"
                  "get copied into first object." );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  
  //## ITERATE THROUGH LATER CHECKED OBJECTS AND COPY CONTOURS TO FIRST CHECKED OBJECT:
  
  Iobj *objTo = getObj(imod, firstObjIdx);
  int contsCopied = 0;
  
  for (int i=firstObjIdx+1; i<(int)lineItem.size() && i<osize(imod); i++ )
  {
    if( lineItem[i].chkObj->isChecked() )
    {
      Iobj *obj = getObj(imod,i);
      for(int c=0; c<csize(obj); c++ )
      {
        Icont *newCont = imodContourDup( getCont(obj,c) );
        undoContourAddition( plug.view, firstObjIdx, osize(objTo) );   // REGISTER UNDO
        imodObjectAddContour( objTo, newCont );
        contsCopied++;
      }
      
      if(!deleteAfterMerge && keepContsInOtherObjs)
      {
        for(int c=0; c<csize(obj)+1 && csize(obj)>0; c++ )
        {
          undoContourRemoval( plug.view, i, 0 );   // REGISTER UNDO
          imodObjectRemoveContour( obj, 0 );
        }
      }
    }
  }
  
  //## ITERATE BACKWARDS AND DELETE CHECKED OBJECT (EXCEPT FOR FIRST CHECKED ON)
  if(deleteAfterMerge)
  {
    for (int i=(int)lineItem.size()-1; i>firstObjIdx; i-- )
    {
      if( lineItem[i].chkObj->isChecked() )
      {
        undoObjectRemoval( plug.view, i );   // REGISTER UNDO
        imodDeleteObject( imod, i );
        lineItem[i].chkObj->setChecked(false);
      }
    }
  }
  
  MsgBox( toString(contsCopied) + " contours moved to object" + toString(firstObjIdx+1) );
  
  undoFinishUnit( plug.view );          // FINISH UNDO
  
  refreshObjList();
  imodUpdateObjectDialogs();
  ivwRedraw( plug.view );
}


//------------------------
//-- If the user has checked a single object, this function gives options
//-- for the user to duplicate that object. By default only the properties
//-- (i.e. name, color, line thickness etc) are copied... however the user
//-- can also opt to copy contours too (if desired).

void NameWizard::duplicateSelected()
{
  Imod *imod  = ivwGetModel(plug.view);
  string objListStr = "";
  int numSelectedObjs = countSelectedObjs( &objListStr );
  
  if( numSelectedObjs!=1 )
  {
    MsgBox("NOTE: You must use the checkboxes to select \n"
           "   ONLY one object to apply duplication");
    return;
  }
  
  //## FIND FIRST (AND ONLY) CHECKED OBJECT:
  
  int firstObjIdx = -1;
  for (int i=0; i<(int)lineItem.size() && i<osize(imod); i++ ) {
    if( lineItem[i].chkObj->isChecked() ) {
      firstObjIdx = i;
      break;
    }
  }
  Imod *objToDup = getObj(imod, firstObjIdx);
  
  
  //## GET CUSTOM INPUT FROM USER:
  
  string nameString = toString( imodObjectGetName(objToDup) );
  static int numDuplicates = 5;
  static int startingNum = 1;
  static bool addNumbers   = true;
  static bool copyContours = false;
  
  CustomDialog ds("Duplicate Object",this);
  ds.addLabel   ( "Duplicate object " + QStr(firstObjIdx+1) + "... \n" );
  ds.addSpinBox ( "Number of duplicates:", 1, 999, &numDuplicates, 1,
                  "The number of duplicate entries to make" );
  ds.addLineEdit( "set name to: ", &nameString );
  ds.addCheckBox( "add numbers to end (.001, .002 etc)", &addNumbers );
  ds.addSpinBox ( "start numbers at:", 1, 999, &startingNum, 1 );
  ds.addCheckBox( "duplicate contours (not just properties)", &copyContours );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  //## CREATE DUPLICATE OBJECTS:
  
  for( int i=0; i<numDuplicates; i++ )
  {
    undoObjectAddition( plug.view, osize(imod) );                 // REGISTER UNDO
    int error = imodNewObject(imod);
    if(error == 1) {
      wprint("\aError creating new object");
      return;
    }
    
    int objNewIdx = osize(imod)-1;
    Iobj *objNew = getObj(imod, objNewIdx );
    
    // NOTE: I tried using imodObjectDup and imodObjectCopy, but had problems,
    //       so instead I copy *most* of the objects properties individually.
    
    imodObjectSetValue(objNew,IobjLineWidth,imodObjectGetValue(objToDup,IobjLineWidth));
    imodObjectSetValue(objNew,IobjLineWidth2,imodObjectGetValue(objToDup,IobjLineWidth2));
    imodObjectSetValue(objNew,IobjPointSize,imodObjectGetValue(objToDup,IobjPointSize));
    imodObjectSetValue(objNew,IobjFlagClosed,imodObjectGetValue(objToDup,IobjFlagClosed));
    
    float red, green, blue;
    imodObjectGetColor( objToDup, &red, &green, &blue );
    imodObjectSetColor( objNew, red, green, blue );
    
    string newObjName = nameString;
    if( addNumbers )
      newObjName += "." + toStringPadNumber( startingNum+i, 3, '0' );
    
    imodObjectSetName( objNew, (char *)newObjName.c_str() );
    
    if( copyContours )
    {
      for( int c=0; c<csize(objToDup); c++ )
      {
        Icont *cont = getCont(objToDup, c);
        Icont *newCont = imodContourDup( cont );
        imodObjectAddContour( objNew, newCont );
      }
    }
  }
  
  undoFinishUnit( plug.view );                      // FINISH UNDO     
  
  refreshObjList();
  imodUpdateObjectDialogs();
  ivwRedraw( plug.view );
}


//------------------------
//-- Reorders selected objects, by first moving them all together, the moving
//-- them down/up so the first one matches the object number specified by
//-- the user.

void NameWizard::moveSelected()
{
  Imod *imod  = ivwGetModel(plug.view);
  string objListStr = "";
  int numSelectedObjs = countSelectedObjs( &objListStr );
  
  if( numSelectedObjs==0 )
  {
    MsgBox("NOTE: You must use the checkboxes to select \n"
           "   one or more object before you can move them");
    return;
  }
  
  //## GET CUSTOM INPUT FROM USER:
  
  static int objNumStart = 1;
  objNumStart = MIN( objNumStart, osize(imod)+1 );
  CustomDialog ds("Move (Reorder) Objects",this);
  ds.addLabel   ( "Move " + QStr(numSelectedObjs) + " selected object(s)\n"
                  "to the following position:" );
  ds.addSpinBox ( " ....", 1, osize(imod), &objNumStart, 1,
                  "NOTE: The first selected object will get this new number" );  
  ds.addLabel   ( "... (objects selected: " + QString(objListStr.c_str()) + ")" );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  int objIdxStart = objNumStart-1;
  if( objIdxStart >= osize(imod) || objIdxStart < 0 )
    return;
  
  
  //## MARK OBJECTS FOR MOVING USING "IobjFlagFilled":
  
  for (int i=0; i<(int)lineItem.size() && i<osize(imod); i++ )
  {
    int checkedInt = lineItem[i].chkObj->isChecked() ? 1 : 0;
    imodObjectSetValue( getObj(imod,i), IobjFlagFilled, checkedInt );
  }
  
  //## SYSTEMATICALLY MOVE MARKED OBJECTS TO DESIRED POSITION AND RESET "IobjFlagFilled":
  
  int numObjsMoved = 0;
  int numObjsBefore = 0;
  
  for (int i=osize(imod)-1; i>=0; i-- )
  {
    Iobj *obj = getObj(imod,i);
    if( imodObjectGetValue( obj, IobjFlagFilled ) != 0 )
    {
      int newObjIdx = MAX( objIdxStart-numObjsBefore, 0 );
      if(i<objIdxStart)
        numObjsBefore++;
      imodObjectSetValue( obj, IobjFlagFilled, 0 );
      undoObjectMove( plug.view, i, newObjIdx );
      imodMoveObject( imod, i, newObjIdx );
      numObjsMoved++;
      i++;
      if(numObjsMoved>numSelectedObjs) { cerr<<"ERROR: moveSelected()"<<endl; break; }
    }
  }
  undoFinishUnit( plug.view );          // FINISH UNDO
  
  
  //## UPDATE CHECKED BOXES TO THE NEW POSITIONS THEN REFRESH:
  
  int startPos = (numObjsBefore) ? objIdxStart-numObjsBefore+1 : objIdxStart;
  for (int i=0; i<(int)lineItem.size() && i<osize(imod); i++ )
    lineItem[i].chkObj->setChecked( i>=startPos && i<startPos+numSelectedObjs );
  
  
  refreshObjList();
  imodUpdateObjectDialogs();
  ivwRedraw( plug.view );
}



//------------------------
//-- Gives options to batch rename all selected (checked) objects in the list 
//-- by giving them all a fixed name, and also give the option to append 
//-- an incrementing number (.001, .002 etc) to the end.

void NameWizard::batchRenameSelected()
{
  Imod *imod  = ivwGetModel(plug.view);
  string objListStr = "";
  int numSelectedObjs = countSelectedObjs( &objListStr );
  
  if( numSelectedObjs==0 )
  {
    MsgBox("NOTE: You must use the checkboxes to select one or more \n"
           "      object before you can batch rename them");
    return;
  }
  
  //## GET CUSTOM INPUT FROM USER:
  
  static string nameString = "New Name";
  static int startingNum = 1;
  static bool addNumbers   = true;
  
  CustomDialog ds("Batch Rename Objects",this);
  ds.addLineEdit( "set to name: ", &nameString );
  ds.addAutoCompletePrev( plug.wordList, false );
  ds.addCheckBox( "add numbers to end (.001, .002 etc)", &addNumbers );
  ds.addSpinBox ( "start numbers at:", 1, 999, &startingNum, 1 );
  ds.addLabel( "WARNING: you can not undo this operation!" );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  //## RENAME SELECTED OBJECTS:
  
  int objectsRenamed = 0;
  for (int i=0; i<(int)lineItem.size() && i<osize(imod); i++)
  {
    if (lineItem[i].chkObj->isChecked() )
    {
      Iobj *obj = getObj(imod, i);
      undoObjectPropChg( plug.view, i );                 // REGISTER UNDO
      
      string newObjName = nameString;
      if( addNumbers )
        newObjName += "." + toStringPadNumber( startingNum+objectsRenamed, 3, '0' );
      
      imodObjectSetName( obj, (char *)newObjName.c_str() );
      objectsRenamed++;
    }
  }
  MsgBox( toString(objectsRenamed) + " objects renamed" );
  undoFinishUnit( plug.view );          // FINISH UNDO
  
  refreshObjList();
  imodUpdateObjectDialogs();
}



//------------------------
//-- Deselects/unticks any ticked boxes in the list of objects
//-- 

void NameWizard::deselectAll()
{
  for (int i=0; i<(int)lineItem.size(); i++)
    lineItem[i].chkObj->setChecked(false);
}

//------------------------
//-- Prompts the user to specify a range of object numbers and then 
//-- checks/selects or deselects (depending on what user choses) all
//-- objects in this range.

void NameWizard::selectRange()
{
  //## GET CUSTOM INPUT FROM USER:
  
  Imod *imod = ivwGetModel(plug.view);
  int   nObjs       = osize(imod);
  static int objMin = 1;
  static int objMax = nObjs;  
  static int action = 0;
  if (objMin > nObjs) objMin = 1;
  if (objMin > nObjs) objMin = nObjs;
  
	CustomDialog ds("Select Objects",this);
  ds.addComboBox( "action:",
                  "select|"
                  "deselect",
                  &action );
  ds.addLabel   ( "objects in range:" );
  ds.addSpinBox ( "min:", 1, nObjs, &objMin, 1,
                  "Only objects AFTER and including this object will be selected" );
  ds.addSpinBox ( "max:", 1, nObjs, &objMax, 1,
                  "Only objects BEFORE and including this object will be selected" );
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  //## CHECK BOXES IN RANGE  
  for (int i=MAX(objMin-1, 0); i<=objMax && i<(int)lineItem.size(); i++)
    lineItem[i].chkObj->setChecked( (action==0) ? true : false );
}


//------------------------
//-- Allows user to specify a find string and will select any objects which contain
//-- a matching string in their object name.

void NameWizard::selectMatching()
{
  //## GET CUSTOM INPUT FROM USER:
  
  Imod *imod = ivwGetModel(plug.view);
  int   nObjs        = osize(imod);
  int   objMin       = 1;
  int   objMax       = nObjs;
  static int action = 0;
  static string searchString = "";
  static bool caseSensitive = true;
  
	CustomDialog ds("Select Matching",this);
  ds.addComboBox( "action:",
                  "select|"
                  "deselect",
                  &action );
  ds.addLabel   ( "within object range:" );
  ds.addSpinBox ( "min:", 1, nObjs, &objMin, 1,
                  "Only objects AFTER and including this object will be selected" );
  ds.addSpinBox ( "max:", 1, nObjs, &objMax, 1,
                  "Only objects BEFORE and including this object will be selected" );
  ds.addLabel( "-----", false );
  ds.addLineEdit( "objects containing: ", &searchString );
  ds.addAutoCompletePrev( plug.wordList, false );
	ds.addCheckBox( "case sensitive", &caseSensitive);
  ds.exec();
	if( ds.wasCancelled() )
		return;
  
  int objMinIdx = MAX(objMin-1, 0);
  QString searchStr = searchString.c_str();
  
  if( searchStr.length() == 0 )
  {
    MsgBox( "Sorry, you did not entered a search string!" );
    return;
  }
  
  
  //## CHECK BOXES (IN RANGE) WHICH CONTAIN THE SEARCH STRING:
  int matches = 0;
  for (int i=objMinIdx; i<=objMax && i<(int)lineItem.size() && i<osize(imod); i++)
  {
    Iobj *obj = getObj(imod,i);
    QString objStr = (QString)( imodObjectGetName(obj) );
    if( objStr.count( searchStr,
                      (caseSensitive) ? Qt::CaseSensitive : Qt::CaseInsensitive ) > 0 )
    {
      lineItem[i].chkObj->setChecked( (action==0) ? true : false );
      matches++;
    }
  }
  
  MsgBox( toString(matches) + " matches were found" );
}


//------------------------
//-- Allows error free creation of a QColor by taking three float values
//-- for "red", "green" and "blue", then making sure each of these values
//-- is between 0 and 255.

QColor NameWizard::makeQColor( float red, float green, float blue )
{
	keepWithinRange( red,   0.0f, 255.0f );
	keepWithinRange( green, 0.0f, 255.0f );
	keepWithinRange( blue,  0.0f, 255.0f );
	
	return QColor( (int)red, (int)green, (int)blue, 100 );			// %%%% REMOVE ALPHA
}


//------------------------
//-- Brings up a dialog which allows the user to select a color to apply
//-- to all selected objects in Name Wizard's object list. If the user
//-- chooses, he can also tick an option which will allow him to chose
//-- a second colors and "gradient steps" such that each selected object
//-- (in order) will step through this gradient.

void NameWizard::batchRecolorSelected()
{
	//## COUNT THE NUMBER OF OBJECTS SELECTED
	int numSelectedObjs = 0;
	QColor newColor(0,0,0);
	
	for(int i=(int)lineItem.size()-1; i>=0; i-- )
  {
    if( lineItem[i].chkObj->isChecked() )
    {
			numSelectedObjs++;
			newColor = lineItem[i].btnColor->getColor();
		}
  }
	
	static QColor otherColor(100,100,100);
	
	//## PRINT ERROR IF NO OBJECTS SELECTED:
	
  if( numSelectedObjs==0 )
	{
		MsgBox( "You have not selected any objects yet" );
    return;
	}
	
	//## GET CUSTOM INPUT FROM USER:
  
	QString msgStr = "pick a new color for the \n";
	msgStr        += QStr(numSelectedObjs) + " selected objects:";
	
	static bool varyColor = false;
	int colorSteps = numSelectedObjs;
	
	CustomDialog ds("Recolor Selected Objects",this);
  ds.addLabel   ( msgStr );
  ds.addColorSel( "new color:", &newColor, "Click here to select a new color" );
	ds.beginGroupBox("color change:", false );
	ds.addCheckPrev( "add color variation", &varyColor, CB_SHOW, false, "" );
	ds.addColorSel( "other color:", &otherColor, "Click here to select a new color" );
	ds.addSpinBox ( "gradient steps:", 2, 20000, &colorSteps, 1,
								 "The number of colours in between to use... \n"
								 "if smaller than the number of contours it will cycle" );
	ds.endGroupBox();
	
  ds.exec();
	if( ds.wasCancelled() )
		return;
	
	//## RECOLOR ALL OBJECTS TO SELECTED COLOR
	
	float colorChangeSteps = (colorSteps==0) ? 0 : 1.0f / (float)colorSteps;
	float diffR = otherColor.red()   - newColor.red();
	float diffG = otherColor.green() - newColor.green();
	float diffB = otherColor.blue()  - newColor.blue();
	
	Imod *imod = ivwGetModel(plug.view);
  int numColorsSet = 0;
	for(int i=0; i<(int)lineItem.size() && i<osize(imod); i++ )
  {
    if( lineItem[i].chkObj->isChecked() )
		{
			lineItem[i].btnColor->setColor( newColor );
			if(varyColor)
			{
				float vary = fMod(colorChangeSteps * (float)numColorsSet, 1.001f);
				QColor diffColor = makeQColor( newColor.red()  + vary*diffR,
												               newColor.green()+ vary*diffG,
												               newColor.blue() + vary*diffB );
				lineItem[i].btnColor->setColor( diffColor );
				numColorsSet++;
			}
		}
  }
  updateColors();
}


//------------------------
//-- Brings up a small dialog giving the user options to apply random
//-- colors to selected objects or (if chosen) all objects. Within
//-- this dialog the user can also opt to limit the randomly
//-- chosen min-and-max color values for red green and blue.
//-- Applying random colors in this way can be useful to 
//-- see which contours placed in different object may actually belong
//-- to the same surface (and thus object).

void NameWizard::randomlyRecolor()
{
	//## COUNT THE NUMBER OF OBJECTS SELECTED
	int numSelectedObjs = 0;
	for(int i=0; i<(int)lineItem.size(); i++ )
    if( lineItem[i].chkObj->isChecked() )
			numSelectedObjs++;
	
	
	//## GET CUSTOM INPUT FROM USER:
  
	int applyTo = (numSelectedObjs==0) ? 1 : 0;
	static bool limitRand = false;
	static int minR = 0;
	static int maxR = 255;
	static int minG = 0;
	static int maxG = 255;
	static int minB = 0;
	static int maxB = 255;
	
	CustomDialog ds("Randomly Color Objects",this);
	ds.addComboBox( "apply random color to:",
								 "selected objects|"
								 "all objects",
								 &applyTo, "What objects do you want to randomly color?" );
	
	
	ds.beginGroupBox("limit between these values:", false, "", true, &limitRand );
	
	ds.addMinMaxSpinBoxPair("red:   ... min", " max ", -1000, 1255, &minR, &maxR, 5,
													"Set the min and max values for red" );
	ds.addMinMaxSpinBoxPair("green: ... min", " max ", -1000, 1255, &minG, &maxG, 5,
													"Set the min and max values for green" );
	ds.addMinMaxSpinBoxPair("blue:  ... min", " max ", -1000, 1255, &minB, &maxB, 5,
													"Set the min and max values for blue" );
	
	ds.endGroupBox();
	
  ds.exec();
	if( ds.wasCancelled() )
		return;
	
	//## RECOLOR ALL OBJECTS USING RANDOMLY CHOSEN COLOR:
	
	if( applyTo==0 && numSelectedObjs==0 )
	{
		MsgBox( "You must select one or more objects before running this option" );
    return;
	}
	if( applyTo==1 )
	{
		if( !MsgBoxYesNo( this, "Are you sure you want to randomly recolor ALL objects?" ) )
			 return;
	}
	
	//## RECOLOR ALL OBJECTS USING RANDOMLY CHOSEN COLOR:
	
	seedRandom();
	
	float rMin = (limitRand) ? -50.0f : (float)minR;
	float rMax = (limitRand) ? 300.0f : (float)maxR;
	float gMin = (limitRand) ? -50.0f : (float)minG;
	float gMax = (limitRand) ? 300.0f : (float)maxG;
	float bMin = (limitRand) ? -50.0f : (float)minB;
	float bMax = (limitRand) ? 300.0f : (float)maxB;
	
	Imod *imod = ivwGetModel(plug.view);
  for(int i=0; i<(int)lineItem.size() && i<osize(imod); i++ )
  {
		if( applyTo == 0 && !lineItem[i].chkObj->isChecked() )		// if object not selected
			continue;
		
		QColor randColor = makeQColor( randFlt(rMin,rMax),
																	 randFlt(gMin,gMax),
																	 randFlt(bMin,bMax) );
		
    lineItem[i].btnColor->setColor( randColor );
  }
	
  updateColors();
}



//------------------------
//-- Gives options to batch change the labels of objects to
//-- the matching uniqueId and or hyperlinks.

void NameWizard::batchLabelChange()
{
  Imod *imod  = ivwGetModel(plug.view);
  string objListStr = "";
  int numSelectedObjs = countSelectedObjs( &objListStr );
	
  //## GET CUSTOM INPUT FROM USER:
  
	static int applyToOpt   = (numSelectedObjs==0) ? 0 : 1;
  static int changeOpt    = 0;
	static bool makeBlank   = true;
	static bool leaveStereo = true;
	static bool writeOut    = true;
	if(numSelectedObjs==0)
		applyToOpt = 0;
	
  CustomDialog ds("Batch Label Change",this);
  
	ds.addRadioGrp( "change the label of:",
								  "all objects|"
								  "selected objects only",
								  &applyToOpt );
	
	ds.addRadioGrp( "change object's label to:",
								  "any matching uniqueId|"
								  "any matching hyperlink|"
								  "any matching name",
								  &changeOpt );
	
	ds.addCheckBox( "if no name match, make label empty", &makeBlank );
	ds.addCheckBox( "skip any labels with the keyword \n"
								  "'STEREOLOGY'    (recommended)", &leaveStereo );
	ds.addCheckBox( "write out all object labels at end", &writeOut,
									"If true: all object labels will be written out into the \n"
								  "console window after you hit 'ok'.");
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  //## CHANGE LABELS OF SELECTED OBJECTS:
  
  int objectsChanged = 0;
  for (int i=0; i<(int)lineItem.size() && i<osize(imod); i++)
  {
    ObjectLineItem &item = lineItem[i];
		if ( applyToOpt==1 && !item.chkObj->isChecked() )
			continue;
		
		//## DETERMINE DESIRED VALUE FOR LABEL BASED ON OPTIONS:
		
		Iobj *obj = getObj(imod, i);
		QString newLabelVal = "";
		if( item.hasMatch )
		{
			if(changeOpt==0)		newLabelVal = item.nameEntry.identifier;
			if(changeOpt==1)		newLabelVal = item.nameEntry.hyperlink;
			if(changeOpt==2)		newLabelVal = item.nameEntry.name;
		}
		
		if( !makeBlank && !item.hasMatch )
			continue;
		
		QString currLabelVal = getObjLabel(obj);
		
		//## MAKE SURE WE DON'T OVERWRITE STEREOLOGY OBJECTS:
		if(leaveStereo)
		{
			QString objName = (QString)imodObjectGetName(obj);
			if ( currLabelVal.contains( "STEREOLOGY" ) || objName.contains( "STEREOLOGY" ) )
				continue;
		}
		
		//## IF LABEL CHANGE NEEDED: CHANGE LABEL:
		if( currLabelVal != newLabelVal )
		{
			undoObjectPropChg( plug.view, i );                 // REGISTER UNDO
			setObjLabel(obj, newLabelVal);
			objectsChanged++;
		}
  }
	
  MsgBox( toString(objectsChanged) + " objects labels changed" );
  if(objectsChanged > 0)
		undoFinishUnit( plug.view );          // FINISH UNDO
	
	if(writeOut)
	{
		cout << endl << "LIST OF LABELS FOR EACH OBJECT:" << endl;
		for (int i=0; i<osize(imod); i++)
		{
			Iobj *obj = getObj(imod, i);
			cout << "object " << (i+1) << ":\t" << getObjLabel(obj).toStdString() << endl;
		}
		cout << endl;
	}
}

//------------------------
//-- Set's the label of the specified object ("obj") to the
//-- specified string ("newLabelStr"). Returns true if successful
//-- or false if there was a problem assigning the memory.

bool NameWizard::setObjLabel( Iobj *obj, QString newLabelStr )
{	
	Ilabel *newLabel = imodObjectNewLabel( obj );
	return ( imodLabelName( newLabel, (char *)newLabelStr.toStdString().c_str()) != 2 );
}

//------------------------
//-- Returns the the label of the specified object ("obj") as a
//-- string. If the object has no label it returns an empty string instead.

QString NameWizard::getObjLabel( Iobj *obj )
{
	Ilabel *label = imodObjectGetLabel(obj);
	if(label==NULL)
		return "";
	
	return (QString)( imodLabelNameGet(label) );
}



//------------------------
//-- Allows user to change other plugin values/settings.

void NameWizard::moreSettings()
{
  //## GET USER INPUT FROM CUSTOM DIALOG:
  
  int newRightColIdx = plug.rightColIdx;
  
	CustomDialog ds("More Settings", this);
  //ds.addLabel   ( "", false );
  ds.addCheckBox( "show status label", 
                  &plug.showStatusLabel,
                  "Shows/hides the label at the top of the plugin which \n"
                  "displays the number of unmatched names and colors." );
  ds.addCheckBox( "show nag screen about 'standard_names_persistent.cvs'", 
                  &plug.showNagPersitentCsv,
                  "If true, then each time you open this plugin it will check if \n"
                  "a file exists at the location: \n   " + plug.defaultFilePath + " \n" 
                  "and show a nag screen if this file does not exist." );
  ds.addComboBox( "right column:",
                  "UniqueID|"
                  "Contours",
                  &newRightColIdx,
                  "The information to display in the right-most column in the list "
                  "of objects in this plugin.\n"
                  "\n"
                  " > UniqueID - if the object matches an entry in the \n"
                  "              'standards names and colors cvs file' \n"
                  "               then shows the UniqueID for this entry \n"
                  "               (if it exists).\n"
                  " > Contours - shows how many (and what type) of contours \n"
								  "              are in each object" );
	//ds.addCheckBox( "auto-add IDs to object labels", 
	//							 &plug.addIdToObjLabels,
	//							 "If true, then each time you change an object name in NameWizard \n"
	//							 "it will add any uniqueId of the matching name entry into the \n"
	//							 "object's main label. This label is hidden from the user, but can \n"
	//							 "be is useful in file conversion programs like 'imod2ccdbxml'. \n"
	//							 "NOTE: This feature not yet implemented" );
	
	ds.exec();
	if( ds.wasCancelled() )
		return;
  
  lblStatusLabel->setVisible( plug.showStatusLabel );
  
  changeCols( newRightColIdx );
}



//############################################################
//## PROTECTED SLOTS:


//------------------------
//-- Displays a (html) help page with information about the plugin

void NameWizard::helpPluginHelp()
{
  imodShowHelpPage("plughelp/namewizard.html#TOP");
}

//------------------------
//-- Displays a html help page with information about "naming objects in IMOD"

void NameWizard::helpNamingHelp()
{
  imodShowHelpPage("plughelp/naming_help.html#TOP");
}


//------------------------
//-- Callback for the buttons at the bottom

void NameWizard::buttonPressed(int which)
{
  if      (which==0)
    close();
  else if (which==1)
		openUrl( "http://www.slashsegmentation.com/tools/imod/name-wizard-plugin" );
	else if (which==2)
    helpPluginHelp();
}

//------------------------
//-- Window closing event handler - removes this pluging from the imod dialog manager

void NameWizard::closeEvent ( QCloseEvent * e )
{
  imodDialogManager.remove((QWidget *)plug.window);
  ivwTrackMouseForPlugs(plug.view, 0);
  
  plug.window->saveSettings();
  
  ivwDraw( plug.view, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC );
  
  plug.view = NULL;
  plug.window = NULL;
  e->accept();
}


//------------------------
//-- Key press event handler - closes on escape or passes on event to "ivwControlKey"

void NameWizard::keyPressEvent ( QKeyEvent * e )
{
	if( e->key() == Qt::Key_Shift )
		plug.shiftDown = true;
	
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

//------------------------
//-- Key release event hander - passes on event to "ivwControlKey"

void NameWizard::keyReleaseEvent ( QKeyEvent * e )
{
	if( e->key() == Qt::Key_Shift )
		plug.shiftDown = false;
	
  ivwControlKey(1, e);
}


//############################################################


