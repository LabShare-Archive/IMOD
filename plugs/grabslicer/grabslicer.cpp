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
#include <qcheckbox.h>

#include <QTextEdit> 
#include <QLineEdit> 

#include "imodplugin.h"
#include "dia_qtutils.h"
#include "grabslicer.h"

#include <QString>
#include <iostream>
#include <string>
using namespace std;

#define NUM_SAVED_VALS 3

//Static variables

static QString qtiltseriesid;
static QString dbini;
static QString qdbpath;
static bool sHasDBini;
static bool sSaveBoth = false;

/*
 *  Define a structure to contain all local plugin data.
 */
typedef struct
{
  ImodView    *view;
  GrabSlicer *window;
  int slicerZap;
  int snapShot_format;
  bool enableScale;
  bool fullAreaSnap;
}PlugData;


static PlugData sPlug = { NULL, NULL, 0, SnapShot_JPG, true, true};

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
  PlugData *plug = &sPlug;
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
    if(shift && !ctrl)
      plug->window->saveImage();
    else if (sHasDBini) {
      sSaveBoth = shift != 0 && ctrl != 0;
      plug->window->saveDatabase();
    }
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
  double savedValues[NUM_SAVED_VALS];
  int nvals;
  static int firstTime = 1;
  PlugData *plug;
  bool showWelcome = false;
  const char *modelUnits = imodUnits(ivwGetModel(inImodView));

  plug = &sPlug;

  if (plug->window){
    /* 
     * Bring the window to the front if already open.
     */
    plug->window->raise();
    return;
  }

  if (firstTime) {

    // Get values from preferences
    nvals = prefGetGenericSettings("GrabWithNote", savedValues,
                                          NUM_SAVED_VALS);
    if (nvals > 2) { 
      plug->slicerZap = B3DNINT(savedValues[0]);
      plug->snapShot_format = B3DNINT(savedValues[1]);
      plug->fullAreaSnap = savedValues[2] != 0.;
    }
    showWelcome = nvals == 0;

    // And enable scale bar if there appear to be real units
    plug->enableScale = strcmp(modelUnits, "pixels") != 0;
    firstTime = 0;
  }

  plug->view = inImodView;
  
  /*
   * This creates the plug window.
   */
  plug->window  = new GrabSlicer(imodDialogManager.parent(IMOD_DIALOG), showWelcome);

  imodDialogManager.add((QWidget *)plug->window, IMOD_DIALOG);

  // This makes the widget the right size and keeps it on screen and off the
  // info window on the Mac
  adjustGeometryAndShow((QWidget *)plug->window, IMOD_DIALOG);

  if (plug->enableScale) {
    setScaleBarWithoutDialog(true);
    ivwDraw(plug->view, IMOD_DRAW_MOD | IMOD_DRAW_SKIPMODV);
  }
}



// THE WINDOW CLASS CONSTRUCTOR

static const char *buttonLabels[] = {"Done", "Help"};
static const char *buttonTips[] = {"Close Grab with Note", "Open help window"};

GrabSlicer::GrabSlicer(QWidget *parent, bool showWelcome)
  : DialogFrame(parent, 2, buttonLabels, buttonTips, true, "Grab with Note", "")
{
  QPushButton *button;
  QCheckBox *box;
  PlugData *plug = &sPlug;
  sHasDBini = false;
  int snapFormat = 0;

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

  QRadioButton *radio_win = diaRadioButton("Slicer", gbox_win, winGroup, gbLayout, 0,
                                           "Take snapshot of Slicer window");
  radio_win = diaRadioButton("Zap", gbox_win, winGroup, gbLayout, 1,
                             "Take snapshot of Zap window");
  diaSetGroup(winGroup, plug->slicerZap);

  QGroupBox *gbox_img = new QGroupBox("Choose Image Type", topBox);
  toplay->addWidget(gbox_img);
  QVBoxLayout *gbLayout1 = new QVBoxLayout(gbox_img);
  gbLayout1->setSpacing(0);
  gbLayout1->setContentsMargins(4, 0, 4, 3);

  imgGroup = new QButtonGroup(this);
  connect(imgGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(imgtypeSelected(int)));  

  QRadioButton *radio_img = diaRadioButton("JPEG", gbox_img, imgGroup, gbLayout1, 0, 
                                           "Save grabbed image with note as JPEG");
  radio_img = diaRadioButton("PNG", gbox_img, imgGroup, gbLayout1, 1,
                             "Save grabbed image with note as PNG");
  radio_img = diaRadioButton("TIFF", gbox_img, imgGroup, gbLayout1, 2,
                             "Save grabbed image with note as TIFF");

  if (sPlug.snapShot_format == SnapShot_PNG)
    snapFormat = 1;
  if (sPlug.snapShot_format == SnapShot_TIF)
    snapFormat = 2;
  diaSetGroup(imgGroup, snapFormat);

  box = diaCheckBox("Show and record scale bar", this, mLayout);
  box->setChecked(plug->enableScale);
  box->setToolTip("Draw scale bar and add length to note if it is turned on in "
                       "the Scale Bar dialog");
  connect(box, SIGNAL(toggled(bool)), this, SLOT(scaleBarToggled(bool)));

  box = diaCheckBox("Always snapshot whole window", this, mLayout);
  box->setChecked(plug->fullAreaSnap);
  box->setToolTip("Snapshot whole window instead of area inside rubber band if band is"
                  " on");
  connect(box, SIGNAL(toggled(bool)), this, SLOT(fullAreaSnapToggled(bool)));

  QHBoxLayout *hlay = diaHBoxLayout(mLayout);
  button = diaPushButton("Add Arrow", this, hlay);
  button->setToolTip("Turn on arrow drawing in the top window, keeping any existing "
                     "arrows");
  connect(button, SIGNAL(clicked()), this, SLOT(addArrowClicked()));

  button = diaPushButton("Clear Arrows", this, hlay);
  button->setToolTip("Clear out all arrows in windows of the selected type");
  connect(button, SIGNAL(clicked()), this, SLOT(clearArrowsClicked()));

  //get database tiltseires id if available
  const char *imod_calob_dir = getenv("IMOD_CALIB_DIR");
  dbini = QString(imod_calob_dir)+"/grabDatabase.ini";

  QFile dbfile(dbini);
  if(dbfile.open(QIODevice::ReadOnly)) {
    sHasDBini = true;
    lblTilt = new QLabel("Tilt Series Database ID:");
    mLayout->addWidget(lblTilt, 1, 0);
    
    tilt_id = new QLineEdit;
    mLayout->addWidget(tilt_id);
    tilt_id->setToolTip("Type the tilt series id for grabbed image to be attached "
                        "in the tomography database");

    QTextStream in(&dbfile);
    while ( !in.atEnd() ) {
      QStringList dbline =  in.readLine().split("'");
      if (dbline[0].startsWith("dbpath")) 
        qdbpath = dbline[1].trimmed();
    } 

    QString fullfilename = ivwCurrentImageFile(sPlug.view, false);
    if (fullfilename.startsWith(qdbpath)) { //if fullpath starts with dbpath, get tiltseiresid
      int len1 = fullfilename.size();
      int len2 = qdbpath.size(); 
      QString templine = fullfilename.right(len1-len2);
      QStringList  templist = templine.split(QDir::separator());
      qtiltseriesid = templist[0];
      tilt_id->setText(qtiltseriesid);
    }
    else { //if database .id file present in the 3d file directory, get tiltseiresid
      QStringList templist = fullfilename.split(QDir::separator());
      int len1 = fullfilename.size();
      int len2 = templist[templist.size() - 1].size(); 
      QString filedir = fullfilename.left(len1-len2);
      QDir export_folder(filedir);
      export_folder.setNameFilters(QStringList()<<"*.id");
      QStringList fileList = export_folder.entryList();
      if (fileList.size() == 1)  {// if exactly one .id file found
	QStringList  templist1 = fileList[0].split(".");
	qtiltseriesid = templist1[0];
	tilt_id->setText(qtiltseriesid);
      }      
    }
  }

  QLabel *boxlab = new QLabel("Enter note for image snapshot:", this);
  mLayout->addWidget(boxlab, 1, 0);

  tilt_note = new QTextEdit();
  if (showWelcome) {
    QString str = "Welcome to Grab with Note\n"
      "Input notes here, to be added to the snapshot below the image";
    if (sHasDBini)
      str += " and in the database.";
    str += "\nOr leave this box blank.\n";
    tilt_note->setText(str);
  }
  mLayout->addWidget(tilt_note);
  tilt_note->setToolTip("Input notes to be added on grabbed image and in database");

  saveImgBut = diaPushButton("Save Image", this, mLayout);
  connect(saveImgBut, SIGNAL(clicked()), this, SLOT(saveImage()));
  saveImgBut->setToolTip("Save grabbed image with note on local disk "
                           "(hot key Shift+W)");

  if (sHasDBini) {
    saveDatabaseBut = diaPushButton("Save to Database", this, mLayout);
    connect(saveDatabaseBut, SIGNAL(clicked()), this, SLOT(saveDatabase()));
    saveDatabaseBut->setToolTip("Save grabbed image with note to Database "
                                "(hot key W)");
  }

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
  PlugData *plug = &sPlug;
  double saveValues[NUM_SAVED_VALS];

  // Save settings, clear any arrows, restore scale bar
  saveValues[0] = plug->slicerZap;
  saveValues[1] = plug->snapShot_format;
  saveValues[2] = plug->fullAreaSnap ? 1. : 0.;
  prefSaveGenericSettings("GrabWithNote", NUM_SAVED_VALS, saveValues);
  clearAllArrows(ZAP_WINDOW_TYPE, true);
  clearAllArrows(SLICER_WINDOW_TYPE, true);
  if (plug->enableScale) {
    setScaleBarWithoutDialog(false);
    ivwDraw(plug->view, IMOD_DRAW_MOD | IMOD_DRAW_SKIPMODV);
  }
  imodDialogManager.remove((QWidget *)plug->window);

  plug->view = NULL;
  plug->window = NULL;
  e->accept();
}

// Close on escape, pass on keys unless one of the text widgets has focus, modifier keys
// come through here in that case and need to be ignored
void GrabSlicer::keyPressEvent ( QKeyEvent * e )
{
  if (tilt_note->hasFocus() || (sHasDBini && tilt_id->hasFocus()))
    return;
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
  sPlug.slicerZap =  value;
}

void GrabSlicer::imgtypeSelected(int value)
{
  //jpeg 0, png 1, tiff 2
  int imgValue =  value;
  switch ( imgValue  ) {
    case 0: 
        sPlug.snapShot_format = SnapShot_JPG;
        break;
    case 1: 
        sPlug.snapShot_format = SnapShot_PNG;
        break;
    case 2: 
        sPlug.snapShot_format = SnapShot_TIF;
        break;
  }  
}

// The use enables or diables the scale bar: redraw
void GrabSlicer::scaleBarToggled(bool state)
{
  sPlug.enableScale = state;
  setScaleBarWithoutDialog(state);
  ivwDraw(sPlug.view, IMOD_DRAW_MOD | IMOD_DRAW_SKIPMODV);
}

void GrabSlicer::fullAreaSnapToggled(bool state)
{
  sPlug.fullAreaSnap = state;
}

void GrabSlicer::addArrowClicked()
{
  if (!startAddedArrow(sPlug.slicerZap ? ZAP_WINDOW_TYPE : SLICER_WINDOW_TYPE))
    return;
  dia_err(sPlug.slicerZap ? "There are no Zap windows open" : 
          "There are no Slicer windows open");
}

void GrabSlicer::clearArrowsClicked()
{
  clearAllArrows(sPlug.slicerZap ? ZAP_WINDOW_TYPE : SLICER_WINDOW_TYPE, false);
}

// Compose a line with a scale bar length if there is one
QString GrabSlicer::scaleBarLine()
{
  QString str = "", str2 = "";
  float zapLen, slicerLen, xyzLen, multiZlen, modvLen, useLen = -1., zoom;
  int err = 1;
  scaleBarAllLengths(zapLen, slicerLen, xyzLen, multiZlen, modvLen);
  if (sPlug.enableScale)
    useLen = sPlug.slicerZap == 0 ? slicerLen : zapLen;
  if (useLen > 0) {
    str.sprintf("Scale bar: %g %s", useLen, imodUnits(ivwGetModel(sPlug.view)));
    imodPrintStderr("%s\n", (const char *)str.toLatin1());
  }
  if (sPlug.slicerZap == 0)
    err = ivwGetTopSlicerZoom(sPlug.view, &zoom);
  else if (sPlug.slicerZap == 1)
    err = ivwGetTopZapZoom(sPlug.view, &zoom);
  if (!err) {
    str2.sprintf("%sZoom: %g", str.isEmpty() ? "" : "      ", zoom);
    str += str2;
  }
  return str;
}

/*
 * Save image to snapshot 
 */
void GrabSlicer::saveImage()
{
  //QString snapname = "/home/hding/snap000.jpg";
  QString snapname = "";
  QString TiltID;
  QString TiltNote = (tilt_note->toPlainText()).trimmed(); 
  QString sangle;
  QString spoint;
  QString scaleLine = scaleBarLine();
  QStringList templist = ivwCurrentImageFile(sPlug.view, false).split(QDir::separator());
  QString realfilename = templist[templist.size() - 1];
  QStringList capLines;
  if (sHasDBini)
    TiltID = (tilt_id->text()).trimmed();
  if (TiltID.isEmpty()) { 
    capLines = (QStringList() << realfilename);
  }
  else {
    capLines = (QStringList() << QString("Database id: ")+TiltID);
    capLines << QString("3D Image: ")+realfilename;
  }
  if (sPlug.slicerZap == 0) { // slicer
    float fangle[3];
    Ipoint fpoint;
    int stime;
    getTopSlicerAngles(fangle, &fpoint, stime);
    sangle.sprintf ("Slicer angle:  (%.2f, %.2f, %.2f)", fangle[0], fangle[1], fangle[2]);
    spoint.sprintf ("Slicer center point:  (%d, %d, %d)", (int)fpoint.x+1, 
                    (int)fpoint.y+1, (int)fpoint.z+1 );
    capLines << sangle << spoint;       
  } else {
    float imX, imY;
    int imZ;
    ivwGetTopZapCenter(sPlug.view, imX, imY, imZ);
    spoint.sprintf("Zap center point:  (%d, %d, %d)", (int)imX, (int)imY, imZ);
    capLines << spoint;
  }
  if (!scaleLine.isEmpty())
    capLines << scaleLine;
  if (!TiltNote.isEmpty())
    capLines << TiltNote;      

  int isnap;
  b3dSetSnapshotCaption(capLines, true);
  if (sPlug.slicerZap == 0) { //slicer
    isnap = ivwSnapshotTopSlicer(snapname, sPlug.snapShot_format, false, 
                                 sPlug.fullAreaSnap);
    if (isnap == -1) {
      dia_err("Snapshot failed: Slicer window is not open.");
    }
  }
  else if  (sPlug.slicerZap == 1) { //zap
    isnap = ivwSnapshotTopZap(snapname, sPlug.snapShot_format, false, sPlug.fullAreaSnap);
    if (isnap == -1)
      dia_err("Snapshot failed: Zap window is not open");
  }
  if (isnap == 1)
    dia_err("Snapshot failed.");
  else if (isnap == 0) {
    sangle = b3dGetSnapDirectory();
    if (sangle.isEmpty())
      sangle = "current directory";
    imodPrintStderr("Snapshot saved as %s in %s\n", (const char *)snapname.toLatin1(),
                    (const char *)sangle.toLatin1());
  }
  return;

}

/*
 * Save image to database or to both database and snapshot
 */
void GrabSlicer::saveDatabase()
{

  QString sangle, spoint;
  Ipoint fpoint;
  float fangle[3];
  float imX, imY, zoom = 1.;
  int imZ;
  bool saveboth = sSaveBoth;
  sSaveBoth = false;

  if (sPlug.slicerZap == 0) { // check if slicer window is open
    int stime;
    if (getTopSlicerAngles(fangle, &fpoint, stime) == 1) {
      dia_err("Snapshot failed: Slicer window is not open.");
      return;
    }
    sangle.sprintf ("Slicer angle:  (%.2f, %.2f, %.2f)", fangle[0], fangle[1], fangle[2]);
    spoint.sprintf ("Slicer center point:  (%d, %d, %d)", (int)fpoint.x+1, 
                    (int)fpoint.y+1, (int)fpoint.z+1 );
    ivwGetTopSlicerZoom(sPlug.view, &zoom);
  } else {
    if ( ivwGetTopZapCenter(sPlug.view, imX, imY, imZ) == 1) {
      dia_err("Snapshot failed: Zap window is not open.");
      return;
    }
    spoint.sprintf("Zap center point:  (%d, %d, %d)", (int)imX, (int)imY, imZ);
    ivwGetTopZapZoom(sPlug.view, &zoom);
  }

  if (sPlug.snapShot_format == SnapShot_TIF) { 
    dia_err("Only JPG or PNG snapshots can be accepted in database for display reason.");
    return;
  }

  //prepare caption text
  QString TiltID = (tilt_id->text()).trimmed();
  QString TiltNote = (tilt_note->toPlainText()).trimmed(); 
  QString scaleLine = scaleBarLine();
  if (TiltID.isEmpty()) { 
    dia_err("Please input tilt series id to which the snapshot will be added");
    return;
  }

  QStringList capLines = (QStringList() << QString("Database id: ")+TiltID);
  QString fullfilename = ivwCurrentImageFile(sPlug.view, false);
  QStringList templist = fullfilename.split(QDir::separator());
  QString realfilename = templist[templist.size() - 1];
  capLines << QString("3D Image: ")+realfilename;
  if (sPlug.slicerZap == 0) { // slicer
    capLines << sangle << spoint;       
    //QStringList capLines = (QStringList() << " " << QString(sangle) << QString(spoint) << TiltID << TiltNote << " ");
  } else {
    capLines << spoint;
  }
  if (!scaleLine.isEmpty())
    capLines << scaleLine;
  if (!TiltNote.isEmpty())
    capLines << TiltNote;      

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
  QString recordpath;
  if (fullfilename.startsWith(qdbpath)) 
    recordpath = fullfilename;
  else
    recordpath = realfilename;
    
   
  if (sPlug.slicerZap == 0)  // slicer window
    sprintf (sqlsql, "INSERT INTO tomography.DataFile (`REF|TiltSeriesData|tiltseries`,"
             " `REF|ThreeDFile|image`, status, filetype, auto, filename, TXT_notes, grab,"
             " zoom, xcenter, ycenter, zcenter, xangle, yangle, zangle)  VALUES ( \"%s\","
             " \"%s\", 0, \"2dimage\", 2, \"tempcapsname\", \"%s\", 1, %f, %d, %d, %d, "
             "%f,  %f,  %f)", tiltseriesid, recordpath.toStdString().c_str(), 
             TiltNote.toStdString().c_str(), zoom, (int)fpoint.x+1, (int)fpoint.y+1, 
             (int)fpoint.z+1, fangle[0], fangle[1], fangle[2] );
  else
    sprintf (sqlsql, "INSERT INTO tomography.DataFile (`REF|TiltSeriesData|tiltseries`,"
             " `REF|ThreeDFile|image`, status, filetype, auto, filename, TXT_notes, grab,"
             " zoom, xcenter, ycenter, zcenter)  VALUES ( \"%s\", \"%s\", 0, \"2dimage\","
             " 2, \"tempcapsname\", \"%s\", 2, %f, %d, %d, %d)", tiltseriesid,
             recordpath.toStdString().c_str(), TiltNote.toStdString().c_str(), zoom, 
             (int)imX, (int)imY, imZ );
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
    switch ( sPlug.snapShot_format  ) {
    case SnapShot_JPG: 
      strcpy (imgend, "jpg");
      break;
    case SnapShot_PNG: 
      strcpy (imgend, "png");
      break;
    }  
    if (sPlug.slicerZap == 0) 
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
  QString qsavename = QString(savename);
  if (sPlug.slicerZap == 0) { //slicer
    isnap = ivwSnapshotTopSlicer(snapname, sPlug.snapShot_format, false, 
                                 sPlug.fullAreaSnap);
    if (isnap == -1) {
      dia_err("Snapshot failed: Slicer window is not open.");
    }
    if (saveboth) {//sav a copy of image in Snap dir too
      b3dSetSnapshotCaption(capLines, true);
      ivwSnapshotTopSlicer(qsavename, sPlug.snapShot_format, false, 
                                 sPlug.fullAreaSnap);  
    }
  }
  else if  (sPlug.slicerZap == 1) { //zap
    isnap = ivwSnapshotTopZap(snapname, sPlug.snapShot_format, false, sPlug.fullAreaSnap);
    if (isnap == -1)
      dia_err("Snapshot failed: Zap window is not open");
    if (saveboth) {//sav a copy of image in Snap dir too
      b3dSetSnapshotCaption(capLines, true);
      ivwSnapshotTopZap(qsavename, sPlug.snapShot_format, false, sPlug.fullAreaSnap);
    }
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
    if (saveboth) {
      imodPrintStderr("Snapshot saved at %s and Snap dir\n", savepath);
    }
    else
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

