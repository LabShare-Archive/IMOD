/*
 *  grabslicer.cpp -- Grab With Note
 *  Special plugin for taking snapshot and save with note as an image or input to tomography database
 */

/*****************************************************************************
*   Copyright (C) 2014 by Jane Ding
*   California Institute of Technology   
*****************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <qpushbutton.h>
#include <qcheckbox.h>
#include <qtooltip.h>
#include <qfiledialog.h>
#include <qstringlist.h>

//Added by qt3to4:
#include <QKeyEvent>
#include <QMouseEvent>
#include <QEvent>
#include <QBoxLayout>

#include <qtoolbutton.h>
#include <QButtonGroup>
#include <QGroupBox>
#include <qradiobutton.h>

#include <QTextEdit> 
#include <QLineEdit> 

#include "imodplugin.h"
#include "dia_qtutils.h"
#include "grabslicer.h"

#include <QString>
#include <iostream>
#include <string>
using namespace std;


//Static variables

static QString fullfilename;
static QString realfilename;
static QString qtiltseriesid;
static QString dbini;


/*
 *  Define a structure to contain all local plugin data.
 */
typedef struct
{
  ImodView    *view;
  GrabSlicer *window;

}PlugData;


static PlugData thisPlug = { NULL, NULL };

/*
 * Called by the imod plugin load function. 
 */
const char *imodPlugInfo(int *type)
{
  if (type)
    *type = IMOD_PLUG_MENU + IMOD_PLUG_KEYS;
  return("Grab with Note");
}

/*
 *  Grab hotkey input. return 1 if we handle the key.
 */
int imodPlugKeys(ImodView *vw, QKeyEvent *event)
{
  PlugData *plug = &thisPlug;
  int keysym;
  int    keyhandled = 1;
  int    ctrl;
  int    shift;

  /*
   * Don't grab keys if plug window isn't open.
   */
  if (!plug->view)
    return 0;
    
  /* The keysym values are Key_A ...
   * Key_Space, Key_Comma...
   */
  keysym = event->key();

  /*
   * Modifier key mask.  Set ctrl and shift to true
   * if the coresponding key is pressed.
   */
  ctrl   = event->modifiers() & Qt::ControlModifier;
  shift  = event->modifiers() & Qt::ShiftModifier;
    
    
  switch(keysym){
  case Qt::Key_Space:
    //plug->window->saveDatabase();
    //plug->window->saveImage();
    break;
  case Qt::Key_W:
    if(shift)
      plug->window->saveImage();
    else
      plug->window->saveDatabase();
    break;
  default:
    keyhandled = 0;
    break;
  }
  return keyhandled;
}

/*
 *  Execute any function or program you wish with this function.
 *  Here we open up a window for user interaction.
 *  see imodplug.h for a list of support functions.
 */

void imodPlugExecute(ImodView *inImodView)
{
  PlugData *plug;

  plug = &thisPlug;

  if (plug->window){
    /* 
     * Bring the window to the front if already open.
     */
    plug->window->raise();
    return;
  }

  plug->view = inImodView;
  
  //get path of the image file name, used in saveDatabase()
  fullfilename = ivwCurrentImageFile(inImodView, false);

  /*
   * This creates the plug window.
   */
  plug->window  = new GrabSlicer(imodDialogManager.parent(IMOD_DIALOG),
                                "Grab with Note");

  imodDialogManager.add((QWidget *)plug->window, IMOD_DIALOG);

  // This makes the widget the right size and keeps it on screen and off the
  // info window on the Mac
  adjustGeometryAndShow((QWidget *)plug->window, IMOD_DIALOG);
}



// THE WINDOW CLASS CONSTRUCTOR

static const char *buttonLabels[] = {"Done", "Help"};
static const char *buttonTips[] = {"Close Grab with Note", "Open help window"};

GrabSlicer::GrabSlicer(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, buttonLabels, buttonTips, true, "Grab with Note", "", name)
{
  QPushButton *button;
  QCheckBox *box;
  PlugData *plug = &thisPlug;
  bool hasDBini = false;

  winValue = 0;
  SnapShot_format = SnapShot_JPG;
	
  mLayout->setSpacing(8);
  topBox = new QWidget(this);
  mLayout->addWidget(topBox);
  QHBoxLayout* toplay = new QHBoxLayout(topBox);
  toplay->setSpacing(4);
  toplay->setContentsMargins(0,0,0,0);


  QGroupBox *gbox_win = new QGroupBox("Choose Window to Grab", topBox);
  toplay->addWidget(gbox_win);
  QVBoxLayout *gbLayout = new QVBoxLayout(gbox_win);
  gbLayout->setSpacing(0);
  gbLayout->setContentsMargins(4, 0, 4, 3);

  winGroup = new QButtonGroup(this);
  connect(winGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(winSelected(int)));

  QRadioButton *radio_win = diaRadioButton("Slicer", gbox_win, winGroup, gbLayout, 0, "Take snapshot of Slicer window");
  radio_win = diaRadioButton("Zap", gbox_win, winGroup, gbLayout, 1, "Take snapshot of Zap window");
  diaSetGroup(winGroup, 0);

  QGroupBox *gbox_img = new QGroupBox("Choose Image Type", topBox);
  toplay->addWidget(gbox_img);
  QVBoxLayout *gbLayout1 = new QVBoxLayout(gbox_img);
  gbLayout1->setSpacing(0);
  gbLayout1->setContentsMargins(4, 0, 4, 3);

  imgGroup = new QButtonGroup(this);
  connect(imgGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(imgtypeSelected(int)));  

  QRadioButton *radio_img = diaRadioButton("JPEG", gbox_img, imgGroup, gbLayout1, 0, "Save grabbed image with note as JPEG");
  radio_img = diaRadioButton("PNG", gbox_img, imgGroup, gbLayout1, 1, "Save grabbed image with note as PNG");
  radio_img = diaRadioButton("TIFF", gbox_img, imgGroup, gbLayout1, 2, "Save grabbed image with note as TIFF");
  diaSetGroup(imgGroup, 0);


  lblTilt = new QLabel("Tilt Series Database ID:");
  mLayout->addWidget(lblTilt, 1, 0);

  tilt_id = new QLineEdit;
  mLayout->addWidget(tilt_id);
  tilt_id->setToolTip("Type the tilt series id for grabbed image to be attached in the tomography database"
                          "");

  //get real filename, input tiltseiresid if available
  const char *imod_calob_dir = getenv("IMOD_CALIB_DIR");
  dbini = QString(imod_calob_dir)+"/grabDatabase.ini";

  QFile dbfile(dbini);
  if(dbfile.open(QIODevice::ReadOnly)) {
    hasDBini = true;
    QTextStream in(&dbfile);
    QString qdbpath;
    while ( !in.atEnd() ) {
      QStringList dbline =  in.readLine().split("'");
      if (dbline[0].startsWith("dbpath")) 
	qdbpath = dbline[1].trimmed();;
    } 
    if (fullfilename.startsWith(qdbpath)) {
      int len1 = fullfilename.size();
      int len2 = qdbpath.size(); 
      QString templine = fullfilename.right(len1-len2);
      QStringList  templist = templine.split("/");
      qtiltseriesid = templist[0];
      realfilename = templist[templist.size()-1];
      tilt_id->setText(qtiltseriesid);
    }
    else {
      QStringList  templist = fullfilename.split("/");
      realfilename = templist[templist.size()-1];
    }
  }
  else {
    QStringList templist = fullfilename.split("/");
    if (templist.size() > 1)
      realfilename = templist[templist.size()-1];
    else
      realfilename = fullfilename;
  }
  tilt_id->setEnabled(hasDBini);
  lblTilt->setEnabled(hasDBini);

  tilt_note = new QTextEdit();
  //tilt_note->setText("Input notes here, to be added into Database. Or leave it blank.\n");
  tilt_note->setText("Input notes here, to be added on the image and in the database. Or leave it blank.\n");
  mLayout->addWidget(tilt_note);
  tilt_note->setToolTip("Input notes to be added on grabbed image and database"
                          "");

  saveImgBut = diaPushButton("Save Image", this, mLayout);
  connect(saveImgBut, SIGNAL(clicked()), this, SLOT(saveImage()));
  saveImgBut->setEnabled(true);
  saveImgBut->setToolTip("Save grabbed image with note on local disk "
                          "(hot key Shift+W)");

  saveDatabaseBut = diaPushButton("Save to Database", this, mLayout);
  connect(saveDatabaseBut, SIGNAL(clicked()), this, SLOT(saveDatabase()));
  saveDatabaseBut->setEnabled(hasDBini);
  saveDatabaseBut->setToolTip("Save grabbed image with note to Database "
                          "(hot key W)");

  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));

}

// This illustrates how to show a help page
void GrabSlicer::buttonPressed(int which)
{
  QString str;
  if (!which)
    close();
  else {
    // For a plugin incorporated into IMOD, this is the path in the compiled qhc file
    // Always have an anchor #TOP at the top so Qt Assistant starts at the top
    imodShowHelpPage("../plughelp/grabslicer.html#TOP");

    // Otherwise, use dia_vasmsg to pass a string from within the code (can contain html).
  }
}

// The window is closing, remove from manager
void GrabSlicer::closeEvent ( QCloseEvent * e )
{
  PlugData *plug = &thisPlug;
  imodDialogManager.remove((QWidget *)plug->window);

  plug->view = NULL;
  plug->window = NULL;
  e->accept();
}

// Close on escape, pass on keys
void GrabSlicer::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

void GrabSlicer::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}

void GrabSlicer::winSelected(int value)
{
  //slicer 0, zap 1
  winValue =  value;
}

void GrabSlicer::imgtypeSelected(int value)
{
  //jpeg 0, png 1, tiff 2
  int imgValue =  value;
  switch ( imgValue  ) {
    case 0: 
        SnapShot_format = SnapShot_JPG;
        break;
    case 1: 
        SnapShot_format = SnapShot_PNG;
        break;
    case 2: 
        SnapShot_format = SnapShot_TIF;
        break;
  }  
}

void GrabSlicer::saveImage()
{
  //QString snapname = "/home/hding/snap000.jpg";
    QString snapname = "";
    QString TiltNote = (tilt_note->toPlainText()).trimmed(); 
    QString sangle;
    QString spoint;
    QStringList capLines = (QStringList() << realfilename);
    if (winValue == 0) { // slicer
      float fangle[3];
      Ipoint fpoint;
      int stime;
      int islicer = getTopSlicerAngles(fangle, &fpoint, stime); //time always 0?
      sangle.sprintf ("Slicer angle:  (%f, %f, %f)", fangle[0], fangle[1], fangle[2] );
      spoint.sprintf ("Slicer center point:  (%d, %d, %d)", (int)fpoint.x+1, (int)fpoint.y+1, (int)fpoint.z+1 );
      capLines = capLines << sangle << spoint;       
    }
    if (!TiltNote.isEmpty())
      capLines = capLines << TiltNote;      

    int isnap;
    b3dSetSnapshotCaption(capLines, true);
    if (winValue == 0) { //slicer
      isnap = ivwSnapshotTopSlicer(snapname, SnapShot_format, false);
      if (isnap == -1) {
	dia_err("Snapshot failed: Slicer window is not open.");
      }
    }
    else if  (winValue == 1) { //zap
      isnap = ivwSnapshotTopZap(snapname, SnapShot_format, false);
      if (isnap == -1)
	dia_err("Snapshot failed: Zap window is not open");
    }
    if (isnap == 1)
      dia_err("Snapshot failed.");
    else if (isnap == 0)
      imodPrintStderr("Snapshot saved under Snap Dir\n");

    return;

}

void GrabSlicer::saveDatabase()
{

    QString sangle, spoint;
    if (winValue == 0) { // slicer, put this first in order to check if slicer window is open
      float fangle[3];
      Ipoint fpoint;
      int stime;
      int islicer = getTopSlicerAngles(fangle, &fpoint, stime); //time always 0?
      if (islicer == 1) {
	dia_err("Snapshot failed: Slicer window is not open.");
	return;
      }
      sangle.sprintf ("Slicer angle:  (%f, %f, %f)", fangle[0], fangle[1], fangle[2] );
      spoint.sprintf ("Slicer center point:  (%d, %d, %d)", (int)fpoint.x+1, (int)fpoint.y+1, (int)fpoint.z+1 );
    }

    if (SnapShot_format == SnapShot_TIF) { 
      dia_err("Only JPG or PNG snapshots can be accepted in database for display reason.");
      return;
    }

    //prepare caption text
    QString TiltID = (tilt_id->text()).trimmed();
    QString TiltNote = (tilt_note->toPlainText()).trimmed(); 
    if (TiltID.isEmpty()) { 
      dia_err("Please input tilt series id to which the snapshot will be added");
      return;
    }

    QStringList capLines = (QStringList() << QString("Database id: ")+TiltID);
    capLines = capLines << QString("3D Image: ")+realfilename;
    if (winValue == 0) { // slicer
      float fangle[3];
      Ipoint fpoint;
      int stime;
      int islicer = getTopSlicerAngles(fangle, &fpoint, stime); //time always 0?
      if (islicer == 1) {
	dia_err("Snapshot failed: Slicer window is not open.");
	return;
      }
      capLines = capLines << sangle << spoint;       
      //QStringList capLines = (QStringList() << " " << QString(sangle) << QString(spoint) << TiltID << TiltNote << " ");
    }
    if (!TiltNote.isEmpty())
      capLines = capLines << TiltNote;      

    //get database setting from database.ini file
    QFile dbfile(dbini);
    if(!dbfile.open(QIODevice::ReadOnly)) {
      imodPrintStderr("Error: can not open %s\n", dbini.toStdString().c_str());
      dia_err("Database connection failed: can not open grabDatabase.ini");
      return;
    }
    QTextStream in(&dbfile);
    char dbpath[500];
    QString dbhost, dbname, dbuser, dbpass;
    while ( !in.atEnd() ) {
      QStringList dbline =  in.readLine().split("'");
      if (dbline[0].startsWith("dbpath")) 
	sprintf (dbpath, "%s", dbline[1].toStdString().c_str());
      else if  (dbline[0].startsWith("dbhost")) 
	dbhost = QString(dbline[1]);
      else if  (dbline[0].startsWith("dbname")) 
	dbname = QString(dbline[1]); 
      else if  (dbline[0].startsWith("dbuser")) 
	dbuser = QString(dbline[1]); 
      else if  (dbline[0].startsWith("dbpass")) 
	dbpass = QString(dbline[1]); 
    } 
    dbfile.close();

    //QSqlDatabase db = QSqlDatabase::database("TOMO");
    QSqlDatabase db = QSqlDatabase::addDatabase("QMYSQL");
    QString dbconnection;
    db.setHostName(dbhost);
    db.setDatabaseName(dbname);
    db.setUserName(dbuser);
    db.setPassword(dbpass);
    if(!db.open()) {
      dia_err("Database connection failed.");
      quitDatabase(db);
      return;
    }
    imodPrintStderr("database connected: %s\n", dbhost.toStdString().c_str());

    //verify the tilt series id is valid
    char tiltseriesid[24];
    sprintf (tiltseriesid, "%s", TiltID.toStdString().c_str());
    
    QSqlQuery qry;
    char sqlsql[500];
    sprintf (sqlsql, "SELECT COUNT(1) FROM TiltSeriesData WHERE tiltseriesID='%s' AND status !=2", tiltseriesid);
    qry.prepare(sqlsql);
    if( !qry.exec() ) {
      imodPrintStderr("Error excuting sql=%s\n", sqlsql);
      dia_err("Unexpected error in saving file to database (1)");
      quitDatabase(db);
      return;
    }
    qry.next();
    if (qry.value(0).toInt() == 0) {
      char dbmsg[200];
      sprintf (dbmsg, "%s is not a valid tilt series id in the database!", tiltseriesid);
      dia_err(dbmsg);
      quitDatabase(db);
      return;
    }
    imodPrintStderr("%s is in database\n", tiltseriesid);

    //insert image and get the last insert id
    sprintf (sqlsql, "INSERT INTO tomography.DataFile (`REF|TiltSeriesData|tiltseries`, status, filetype, auto, filename, TXT_notes)  VALUES ( \"%s\", 0, \"2dimage\", 2, \"tempcapsname\", \"%s\")", tiltseriesid, TiltNote.toStdString().c_str() );
    qry.prepare(sqlsql);
    if( !qry.exec() ) {
      imodPrintStderr("Error excuting sql=%s\n", sqlsql);
      dia_err("Unexpected error in saving file to database (2)");
      quitDatabase(db);
      return;
    }
    // get last insert id and generate save path
    char savepath[500], savename[50];
    int last_id;
    if( qry.isActive()) {
	qry.next();
	last_id = qry.lastInsertId().toInt();
	char imgend[4];
	switch ( SnapShot_format  ) {
	case SnapShot_JPG: 
	  strcpy (imgend, "jpg");
	  break;
	case SnapShot_PNG: 
	  strcpy (imgend, "png");
	  break;
	}  
	if (winValue == 0) 
	  sprintf (savename, "%s_slicer%d.%s", tiltseriesid, last_id, imgend);
	else 
	  sprintf (savename, "%s_zap%d.%s", tiltseriesid, last_id, imgend);
    }
    else {
      imodPrintStderr("Error in obtaining lateast inserted id.\n");
      dia_err("Unexpected error in saving file to database (3)");
      quitDatabase(db);
      return;
    }

    //set captions
    b3dSetSnapshotCaption(capLines, true);

    //save grabed image to savepath
    int isnap = 1;
    sprintf (savepath, "%sCaps/%s", dbpath, savename);
    QString snapname = QString(savepath);
    if (winValue == 0) { //slicer
      isnap = ivwSnapshotTopSlicer(snapname, SnapShot_format, false);
      if (isnap == -1) {
	dia_err("Snapshot failed: Slicer window is not open.");
      }
    }
    else if  (winValue == 1) { //zap
      isnap = ivwSnapshotTopZap(snapname, SnapShot_format, false);
      if (isnap == -1)
	dia_err("Snapshot failed: Zap window is not open");
    }
    if (isnap == 0) { // snapshot saved
      //rename the image record in DB with correct name containing file id
      sprintf (sqlsql, "UPDATE DataFile SET filename = \"%s\" WHERE DEF_id = %d ", savename, last_id );
      qry.prepare(sqlsql);
      if( !qry.exec() ) {
	imodPrintStderr("Error excuting sql=%s\n", sqlsql);
	isnap = 1;
      }
      sprintf (sqlsql, "UPDATE TiltSeriesData SET time_modified=NOW() WHERE tiltseriesID = \"%s\"", tiltseriesid );
      qry.prepare(sqlsql);
      if( !qry.exec() ) {
	imodPrintStderr("Warning! Timestamp is not changed. Error excuting sql=%s\n", sqlsql);
      }
      imodPrintStderr("Snapshot saved at %s\n", savepath);
    }
    if (isnap != 0) {  // failed
      dia_err("Snapshot failed.");
      //remove image record from DB
      sprintf (sqlsql, "DELETE FROM TiltSeriesData WHERE tiltseriesID='%d' ", last_id );
      qry.prepare(sqlsql);
    }

    // close db and quit
    qry.clear();
    dbconnection = db.connectionName();
    db.close();
    db = QSqlDatabase();
    db.removeDatabase(dbconnection);
    imodPrintStderr("database connection closed.\n");
    return;

}


void GrabSlicer::quitDatabase(QSqlDatabase db) {
    QString dbconnection = db.connectionName();
    db.close();
    QSqlDatabase::removeDatabase(dbconnection);
    imodPrintStderr("database connection closed.\n");
}


