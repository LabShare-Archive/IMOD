/*
 *  imodv_menu.cpp -- menu actions for imodv main window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$
Log at end of file
*/

#include <qfiledialog.h>
#include <qdir.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "colorselector.h"
#include "imodv_window.h"
#include "dia_qtutils.h"
#include "imodv.h"
#include "imod.h"
#include "b3dgfx.h"
#include "imodv_gfx.h"
#include "imodv_menu.h"
#include "imodv_input.h"
#include "imodv_control.h"
#include "imodv_stereo.h"
#include "imodv_depthcue.h"
#include "imodv_views.h"
#include "imodv_modeled.h"
#include "imodv_image.h"
#include "imodv_objed.h"
#include "imodv_movie.h"
#include "preferences.h"
#include "control.h"

static ImodvBkgColor bkgColor;

/* DNM 12/1/02: make this the single path to opening the window, and have
   it keep track of whether window is open or not, and return if it is */
void imodvMenuBgcolor(int state)
{

  // Qt version: raise the window if already open, or open it; or close it
  if (state) {
    if (bkgColor.mSelector) {
      bkgColor.mSelector->raise();
      return;
    }
    bkgColor.openDialog();
  } else {
    bkgColor.doneSlot();
  }
}


/*********************************/
/* Edit menu dispatch function. */
void imodvEditMenu(int item)
{
  switch(item){
  case VEDIT_MENU_OBJECTS:
    objed(Imodv);
    break;
  case VEDIT_MENU_CONTROLS: /* controls */
    imodv_control(Imodv, 1);
    break;
  case VEDIT_MENU_OBJLIST: /* object list */
    imodvObjectListDialog(Imodv, 1);
    break;
  case VEDIT_MENU_BKG: /* background color */
    imodvMenuBgcolor(1);
    break;
  case VEDIT_MENU_MODELS: /* models */
    imodvModelEditDialog(Imodv, 1);
    break;
  case VEDIT_MENU_VIEWS: /* views */
    imodvViewEditDialog(Imodv, 1);
    break;
  case VEDIT_MENU_IMAGE: /* image */
    imodvImageEditDialog(Imodv, 1);
    break;
    
  }
}

/*********************************/
/* help menu dispatch function. */
void imodvHelpMenu(int item)
{
  switch(item) {
  case VHELP_MENU_MENUS:
    dia_vasmsg
      ("3dmodv Help for menus\n",
       "---------------------------------\n",
       "File Menu:\n",
       "     Open Model\t\tLoad a new model to view.\n",
       "     Save Model\t\tSave the current model.\n",
       "     Save Model As...\tSave model under a different name.\n",
       "     Snap RGB As...\tSave a snapshot to a specified RGB file.\n",
       "     Snap TIFF As...\tSave a snapshot a specified TIFF file.\n",
       "     Zero Snap File #\tReset the counter for snapshot files to 0.\n",
       "     Movie...\t\tProgram a sequence of displays and save them.\n",
       "     Close\t\tQuit 3dmodv or close model view window in imod.\n",
                
       "\nEdit Menu:\n",
       "     Objects...\t\tOpen the object edit dialog.\n",
       "     Controls...\t\tOpen the display control dialog.\n",
       "     Object List...\t\tShow objects by name with On/Off buttons.\n",
       "     Background...\tChange the background color.\n",
       "     Models...\t\tControl the display of multiple models.\n",
       "     Views...\t\tOpen a dialog to save and restore views.\n",
       "     Image...\t\tDisplay an image slice on the model.\n",
                
       "\nView - Rendering Options\n",
       "     Double Buffer\tChange between double and single\n",
       "\t\t\t     buffer visual.\n",
       "     Lighting\t\tTurn rendering with a light on or off.\n"
       "     Wireframe\t\tRender data in wireframe only.\n",
       "     Low Res\t\tDisplay low resolution mesh.\n",
       "     Stereo...\t\tOpen stereo control dialog.\n",
       "     Depth Cue...\t\tControl dimming of display with distance.\n",
       NULL);
    break;

  case VHELP_MENU_KEYBOARD:
    dia_vasmsg
      ("3dmodv Help for Keyboard Commands\n",
       "----------------------------------------------------------\n",
       "\nKeys \tCommand \n",
       "----------------------------------------------------------\n",
       "Arrows\tTranslate model in x and y\n",
       "Page  \tUp and Down keys translate model in z\n",
       "Keypad\tRotates model in x, y and z. the '5' key toggles\n",
       "      \tmovie mode on/off\n"
       " Esc/q\tQuit this program\n",
       "  s   \tToggle stereo mode\n",
       "  S   \tSnapshot image as an RGB file to modvnnnn.rgb\n",
       CTRL_STRING"-S\tSnapshot image as a TIFF file to modvnnnn.tif\n",
       "  o   \tOutput transformation information and movie frames/sec\n",
       "  c   \tOutput clipping plane information\n",
       " -/=  \tDecrease/Increase zoom\n",
       " _/+  \tDecrease/Increase zoom by big steps\n"
       "  m   \tOpen movie control window\n",
       "  O   \tOpen Object Edit window\n",
       "  C   \tOpen controls window\n",
       "  B   \tOpen background color window\n",
       "  L   \tOpen Object List window\n",
       "  M   \tOpen model selection window\n",
       "  V   \tOpen view editing window\n",
       "  I   \tOpen image overlay control window\n",
       "  I   \tOpen Z plane image overlay\n",
       "  b   \tToggle double buffering\n",
       "  r   \tToggle low resolution drawing of mesh and spheres\n",
       " g/G  \tIncrease/Decrease the quality of sphere drawing\n",
       " [/]  \tAdjust parallax for stereo viewing\n",
       "  l   \tInvert the parallax angle\n",
       " ,/.  \tDecrease/Increase rotation increment and speed\n",
       " 1/2  \tDecrease/Increase time for 4D models\n",
       "  8   \tToggle displaying all models or one model\n",
       " 9/0  \tPrevious/Next model\n",
       " D    \tDelete current contour if it was picked with mouse\n",
       "----------------------------------------------------------\n",
       NULL);
    break;
  
  case VHELP_MENU_MOUSE:
    dia_vasmsg
      ("3dmodv Help for Mouse Controls\n",
       "----------------------------------------------------------\n",
       "Left Mouse Button Drag\n",
       "\tThe left mouse button moves the model when held down.\n\n",
       "\tWhen the "CTRL_STRING" key is held down the left mouse button "
       "moves the current object clipping plane.\n\n",
       "Middle Mouse Button Drag\n",
       "\tThe middle mouse button rotates the model around an axis "
       "perpendicular to the direction of motion of the mouse.  It can also "
       "be used to \"throw\" the model in a new direction if the model "
       "is already rotating automatically, or if a button or hot key has been "
       "pressed to start rotation.  The model will rotate continuously in the "
       "direction of movement, and the speed of rotation will be proportional "
       "to the distance moved.  (Moving 100 pixels will make it rotate at "
       "the standard speed.)\n\n"
       "\tWhen the Shift key is held down the middle ",
       "mouse button rotates the light source instead.\n\n",
       "\tWhen the "CTRL_STRING" key is held down the middle mouse button "
       "rotates the current object clipping plane instead.\n\n",
       "Right Mouse Button\n",
       "\tWhen running Model View from 3dmod, clicking on a point in the "
       "model with the right mouse button will select the nearest, "
       "frontmost point in the model as the current model point within "
       "3dmod.\n",
       NULL);
    break;

  case VHELP_MENU_ABOUT:
    dia_vasmsg
      ("3dmodv Qt Version ",
       VERSION_NAME,
       ", written by James Kremer and",
       "David Mastronarde\n",
       "Copyright (C)",COPYRIGHT_YEARS,"by",LAB_NAME1,"\n",LAB_NAME2,
       "& Regents of the University of Colorado\n\n",
       NULL);
    break;
  }
}


/****************************************************************************/
/* The FILE MENU */

// load a specified model (only in standalone model)
int imodvLoadModel()
{
  Imod **tmoda;
  Imod *tmod;
  ImodvApp *a = Imodv;
  int i, ob, co;
  QString qname;
  char *filter[] = {"Model files (*.*mod)"};
  
  if (ImodvClosed || !a->standalone)
    return -1;

  // Need to release the keyboard because window grabs it on ctrl
  a->mainWin->releaseKeyboard();
  qname = diaOpenFileName(NULL, "Select Model file to load", 1, filter);

  if (qname.isEmpty())
    return 1;

  tmod = imodRead((char *)qname.latin1());
  if (!tmod)
    return(-1);

  /* DNM 6/20/01: find out max time and set current time */
  tmod->tmax = 0;
  for (ob = 0; ob < tmod->objsize; ob++)
    for (co = 0; co < tmod->obj[ob].contsize; co++)
      if (tmod->tmax < tmod->obj[ob].cont[co].type)
        tmod->tmax = tmod->obj[ob].cont[co].type;
  tmod->ctime = tmod->tmax ? 1 : 0;

  tmoda = (Imod **)malloc(sizeof(Imod *) * (a->nm + 1));
  for (i = 0; i < a->nm; i++)
    tmoda[i] = a->mod[i];
  tmoda[i] = tmod;
  if (a->nm)
    free(a->mod);
  a->mod = tmoda;

  /*     a->cm = a->nm; */
  a->nm++;
  /*     a->imod = tmod; */

  /* DNM: changes for storage of object properties in view and 
     relying on default scaling - switched to new method 6/26/03 */

  imodvViewsInitialize(tmod);

  imodvSelectModel(a, a->nm - 1);
  return(0);
}

// Save to the existing filename
void imodvFileSave()
{
  /* DNM: added rename of existing file to backup.  Also eliminated use of
     Imodv pointer in favor of a->, and the double save (!?!), and added
     error checks */

  int len, error;
  char *nfname1;
  FILE *fout = NULL;
  ImodvApp *a = Imodv;
  char *filename = a->imod->fileName;
  struct stat buf;
  
  /* DNM 8/4/01: store the current view when saving, if appropriate */
  imodvAutoStoreView(a);

  len = strlen(filename)+1;

  nfname1 = (char *)malloc(len + 1);
  sprintf(nfname1, "%s~", filename);

  /* DNM 10/20/03: remove backup and rename only if file already exists */
  if (!stat(filename, &buf)) {
    remove(nfname1);
    rename(filename, nfname1);
  }

  if (a->imod->fileName)
    fout = fopen((QDir::convertSeparators(QString(a->imod->fileName))).
      latin1(), "wb");

  if (fout){
    a->imod->file = fout;
    error = imodWrite(a->imod, a->imod->file);
    /*        error = imodWrite(Imodv->imod, Imodv->imod->file); */
    fflush(fout);
    fclose(fout);
    a->imod->file = NULL;
    if (!error)
      dia_puts("Model file saved.");
  } else
    error = 1;

  if (error) {

    dia_err("File not saved, bad filename or error;"
            " attempting to restore backup file.");
    /* If the backup actually exists, remove regular file first */
    if (!stat(nfname1, &buf))
      remove(filename);
    rename (nfname1, filename);
  }
  free(nfname1);
}

// Save model to new filename
void imodvSaveModelAs()
{
  /* DNM: added rename of existing file and improved error checks */

  ImodvApp *a = Imodv;
  char *filename;
  FILE *fout;
  int len, error;
  char *nfname1;
  QString qname;
  struct stat buf;
  
  a->mainWin->releaseKeyboard();
  qname = QFileDialog::getSaveFileName (QString::null, QString::null, 0, 0, 
                                        "Select file to save model into:");
  if (qname.isEmpty())
    return;
  filename = strdup(qname.latin1());

  /* DNM 8/4/01: store the current view when saving, if appropriate */
  imodvAutoStoreView(a);

  len = strlen(filename)+1;

  nfname1 = (char *)malloc(len + 1);
  sprintf(nfname1, "%s~", filename);
  if (!stat(filename, &buf)) {
    remove(nfname1);
    rename(filename, nfname1);
  }

  fout = fopen((QDir::convertSeparators(QString(filename))).latin1(), "wb");
  if (fout){
    a->imod->file = fout;
    error = imodWrite(Imodv->imod, fout);
    fflush(fout);
    fclose(fout);
    Imodv->imod->file = NULL;

    if (!error) {
      if (a->imod->fileName)
        free(a->imod->fileName);
      a->imod->fileName = (char *)malloc(len);
      if (a->imod->fileName)
        memcpy(a->imod->fileName, filename, len);
          
      if ((strlen(filename)+1) < IMOD_STRSIZE)
        memcpy(a->imod->name, filename, strlen(filename)+1);
      else
        a->imod->name[0] = 0x00;
          
      dia_puts("Model file saved.");
    }
  } else {
    error = 1;
  }
  if (error) {
    dia_err("Error writing model; attempting to restore backup file");
    if (!stat(nfname1, &buf))
      remove(filename);
    rename(nfname1, filename);
  }
  free(nfname1);
  free(filename);
}

// The file menu dispatch function
void imodvFileMenu(int item)
{
  QString qname;

  switch (item) {
  case VFILE_MENU_LOAD:
    if (imodvLoadModel() < 0)
      dia_err("Error reading model file.  No model loaded.");
    break;

  case VFILE_MENU_SAVE:
    imodvFileSave();
    break;

  case VFILE_MENU_SAVEAS:
    imodvSaveModelAs();
    break;

  case VFILE_MENU_SNAPRGB:
  case VFILE_MENU_SNAPTIFF:
    Imodv->mainWin->releaseKeyboard();
    qname = QFileDialog::getSaveFileName (QString::null, QString::null, 0, 0, 
                                          "File to snapshot into:");
    if (qname.isEmpty())
      break;
    imodv_auto_snapshot((char *)qname.latin1(), item == VFILE_MENU_SNAPRGB ? 
                        SnapShot_RGB : SnapShot_TIF);
    break;

  case VFILE_MENU_ZEROSNAP:
    imodvResetSnap();
    break;

  case VFILE_MENU_MOVIE:
    imodvMovieDialog(Imodv, 1);
    break;
 
  case VFILE_MENU_QUIT:
    Imodv->mainWin->close();
    break;
  }
} 

/****************************************************************************/
// The view menu dispatch function
void imodvViewMenu(int item)
{
  ImodvApp *a = Imodv;
  switch (item) {
  case VVIEW_MENU_DB:
    imodv_setbuffer(a);
    break;

  case VVIEW_MENU_LIGHTING:
    imodvRegisterModelChg();
    imodvFinishChgUnit();
    if (a->lighting)
      a->imod->view->world &= ~VIEW_WORLD_LIGHT;
    else
      a->imod->view->world |= VIEW_WORLD_LIGHT;
    a->lighting = 1 - a->lighting;
    a->mainWin->setCheckableItem(VVIEW_MENU_LIGHTING, a->lighting);
    imodvDraw(a);
    break;

  case VVIEW_MENU_WIREFRAME:
    imodvRegisterModelChg();
    imodvFinishChgUnit();
    if (a->wireframe)
      a->imod->view->world &= ~VIEW_WORLD_WIREFRAME;
    else 
      a->imod->view->world |= VIEW_WORLD_WIREFRAME;
    a->wireframe = 1 - a->wireframe;
    a->mainWin->setCheckableItem(VVIEW_MENU_WIREFRAME, a->wireframe);
    imodvDraw(a);
    break;
 
  case VVIEW_MENU_LOWRES:
    imodvRegisterModelChg();
    imodvFinishChgUnit();
    if (a->lowres)
      a->imod->view->world &= ~VIEW_WORLD_LOWRES;
    else 
      a->imod->view->world |= VIEW_WORLD_LOWRES;
    a->lowres = 1 - a->lowres;
    a->mainWin->setCheckableItem(VVIEW_MENU_LOWRES, a->lowres);
    imodvDraw(a);
    break;

  case VVIEW_MENU_STEREO:
    imodvStereoEditDialog(a, 1);
    break;

  case VVIEW_MENU_DEPTH:
    imodvDepthCueEditDialog(a, 1);
    break;
  }
}


/* DNM 1/24/03: REMOVED addImodvViewPlugins
 * imodv plugin menu additions.
 */

// Calls to set the menu items as checked/unchecked
void imodvMenuLight(int value)
{
  Imodv->mainWin->setCheckableItem(VVIEW_MENU_LIGHTING, value);
}

void imodvMenuWireframe(int value)
{
  Imodv->mainWin->setCheckableItem(VVIEW_MENU_WIREFRAME, value);
}

void imodvMenuLowres(int value)
{
  Imodv->mainWin->setCheckableItem(VVIEW_MENU_LOWRES, value);
}

// Initially open selected windows
void imodvOpenSelectedWindows(char *keys)
{
  if (!keys)
    return;
    if (strchr(keys, 'C'))
      imodv_control(Imodv, 1);
    if (strchr(keys, 'O'))
      objed(Imodv);
    if (strchr(keys, 'B'))
      imodvMenuBgcolor(1);
    if (strchr(keys, 'L'))
      imodvObjectListDialog(Imodv, 1);
    if (strchr(keys, 'V'))
      imodvViewEditDialog(Imodv, 1);
    if (strchr(keys, 'M'))
      imodvModelEditDialog(Imodv, 1);
    if (strchr(keys, 'm'))
      imodvMovieDialog(Imodv, 1);
    if (strchr(keys, 'I') && !Imodv->standalone)
      imodvImageEditDialog(Imodv, 1);
    if (strchr(keys, 'S'))
      imodvStereoEditDialog(Imodv, 1);
    if (strchr(keys, 'D'))
      imodvDepthCueEditDialog(Imodv, 1);
}


// Background color class
void ImodvBkgColor::openDialog()
{
  QString qstr;
  char *window_name;

  mSelector = new ColorSelector(imodvDialogManager.parent(IMODV_DIALOG), 
                                "3dmodv background color.",
                                Imodv->rbgcolor->red(),
				Imodv->rbgcolor->green(),
				Imodv->rbgcolor->blue(), hotSliderFlag(), 
				hotSliderKey(), ImodPrefs->getRoundedStyle(), 
                                "selector");
  connect(mSelector, SIGNAL(newColor(int, int, int)), this, 
          SLOT(newColorSlot(int, int, int)));
  connect(mSelector, SIGNAL(done()), this, SLOT(doneSlot()));
  connect(mSelector, SIGNAL(closing()), this, SLOT(closingSlot()));
  connect(mSelector, SIGNAL(keyPress(QKeyEvent *)), this, 
          SLOT(keyPressSlot(QKeyEvent *)));
  connect(mSelector, SIGNAL(keyRelease(QKeyEvent *)), this, 
          SLOT(keyReleaseSlot(QKeyEvent *)));

  window_name = imodwEithername("3dmodv: ", Imodv->imod->fileName, 1);
  qstr = window_name;
  if (window_name)
    free(window_name);
  if (qstr.isEmpty())
    qstr = "3dmodv";
  mSelector->setCaption(qstr);

  imodvDialogManager.add((QWidget *)mSelector, IMODV_DIALOG);
  mSelector->show();
}

ImodvBkgColor::ImodvBkgColor()
  : QObject(0, 0)
{
  mSelector = NULL;
}

void ImodvBkgColor::newColorSlot(int red, int green, int blue)
{
  ImodvApp *a = Imodv;

  a->rbgcolor->setRgb(red, green, blue);
  imodvDraw(a);
}

void ImodvBkgColor::doneSlot()
{
  if (mSelector)
    mSelector->close();
}

void ImodvBkgColor::closingSlot()
{
  imodvDialogManager.remove((QWidget *)mSelector);
  mSelector = NULL;
}

void ImodvBkgColor::keyPressSlot ( QKeyEvent * e )
{
  imodvKeyPress(e);
}
void ImodvBkgColor::keyReleaseSlot ( QKeyEvent * e )
{
  imodvKeyRelease(e);
}

/*
$Log$
Revision 4.15  2004/11/21 06:07:49  mast
Changes for undo/redo

Revision 4.14  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 4.13  2004/05/15 21:24:31  mast
Added hot key Z to help list

Revision 4.12  2004/04/28 05:28:52  mast
Changes for drawing current contour thicker

Revision 4.11  2003/11/04 04:43:16  mast
DOcumentation for new rotation speed and throwing

Revision 4.10  2003/10/24 04:01:57  mast
delete files before renaming, renaming only if necessary for Windows/Intel

Revision 4.9  2003/06/27 20:03:30  mast
Initial views using new scheme when reading in a model

Revision 4.8  2003/05/05 15:07:10  mast
Fix hotkey list

Revision 4.7  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.6  2003/04/17 19:27:48  mast
adding ctrl string

Revision 4.5  2003/04/14 15:31:02  mast
fixing documentation

Revision 4.4  2003/03/26 23:23:15  mast
switched from hotslider.h to preferences.h

Revision 4.3  2003/02/27 17:38:02  mast
Convert filenames with Qt routines

Revision 4.2  2003/02/21 23:20:55  mast
Open model save file in binary mode

Revision 4.1  2003/02/10 20:29:01  mast
autox.cpp

Revision 1.1.2.11  2003/01/29 01:44:29  mast
exit by closing window

Revision 1.1.2.10  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.9  2003/01/18 01:13:44  mast
add include of dia_qtutils

Revision 1.1.2.8  2003/01/13 07:21:38  mast
Changes to use new dialog manager class

Revision 1.1.2.7  2003/01/06 15:53:07  mast
eliminate unused varaiables

Revision 1.1.2.6  2003/01/01 19:12:31  mast
changes to start Qt application in standalone mode

Revision 1.1.2.5  2002/12/30 06:49:50  mast
rationalizing dialogs as widgets and using dialog list

Revision 1.1.2.4  2002/12/27 01:23:56  mast
New background color selector

Revision 1.1.2.3  2002/12/18 04:15:14  mast
new includes for imodv modules

Revision 1.1.2.2  2002/12/17 22:28:21  mast
cleanup of unused variables and SGI errors

Revision 1.1.2.1  2002/12/17 18:43:58  mast
Qt version

Revision 3.2  2002/12/01 15:31:41  mast
Changes to compile with g++; also made only one background color window
be open at any one time.

Revision 3.1  2002/11/05 23:27:46  mast
Changed copyright notice to use defined lab name and years

*/
