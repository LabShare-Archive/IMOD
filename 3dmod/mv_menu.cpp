/*
 *  mv_menu.cpp -- menu actions for imodv main window.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <qdir.h>
//Added by qt3to4:
#include <QKeyEvent>
#include <sys/types.h>
#include <sys/stat.h>
#include "colorselector.h"
#include "mv_window.h"
#include "dia_qtutils.h"
#include "imodv.h"
#include "imod.h"
#include "b3dgfx.h"
#include "mv_gfx.h"
#include "mv_menu.h"
#include "mv_input.h"
#include "mv_control.h"
#include "mv_stereo.h"
#include "mv_depthcue.h"
#include "mv_views.h"
#include "mv_modeled.h"
#include "mv_image.h"
#include "isosurface.h"
#include "mv_objed.h"
#include "mv_listobj.h"
#include "mv_movie.h"
#include "preferences.h"
#include "control.h"
#include "scalebar.h"
#include "utilities.h"

static ImodvBkgColor bkgColor;
static void toggleWorldFlag(int &globalVal, b3dUInt32 mask, int menuID);
static int writeOpenedModelFile(ImodvApp *a, FILE *fout);

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
  case VEDIT_MENU_ROTATION: /* rotation tool */
    Imodv->mainWin->openRotationTool(Imodv);
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
    mvImageEditDialog(Imodv, 1);
    break;
  case VEDIT_MENU_ISOSURFACE: /*isosurface*/
    imodvIsosurfaceEditDialog(Imodv, 1);
    break;
    
  }
}

/*********************************/
/* help menu dispatch function. */
void imodvHelpMenu(int item)
{
  switch(item) {
  case VHELP_MENU_MENUS:
    imodShowHelpPage("modvMenus.html#TOP");
    break;

  case VHELP_MENU_KEYBOARD:
    imodShowHelpPage("modvKeyboard.html#TOP");
    break;
  
  case VHELP_MENU_MOUSE:
    imodShowHelpPage("modvMouse.html#TOP");
    break;

  case VHELP_MENU_ABOUT:
    dia_vasmsg
      ("3dmodv Version ",
       VERSION_NAME,
       ", written by David Mastronarde, ",
       "James Kremer, and Quanren Xiong\n",
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
  const char *filter[] = {"Model files (*.*mod)"};
  
  if (ImodvClosed || !a->standalone)
    return -1;

  // Need to release the keyboard because window grabs it on ctrl
  a->mainWin->releaseKeyboard();
  qname = utilOpenFileName(NULL, "Select Model file to load", 1, filter);

  if (qname.isEmpty())
    return 1;

  tmod = imodRead(LATIN1(qname));
  if (!tmod)
    return(-1);
  utilExchangeFlipRotation(tmod, FLIP_TO_ROTATION);

  /* DNM 6/20/01: find out max time and set current time */
  tmod->tmax = 0;
  for (ob = 0; ob < tmod->objsize; ob++)
    for (co = 0; co < tmod->obj[ob].contsize; co++)
      if (tmod->tmax < tmod->obj[ob].cont[co].time)
        tmod->tmax = tmod->obj[ob].cont[co].time;
  tmod->ctime = tmod->tmax ? 1 : 0;

  tmoda = (Imod **)malloc(sizeof(Imod *) * (a->numMods + 1));
  for (i = 0; i < a->numMods; i++)
    tmoda[i] = a->mod[i];
  tmoda[i] = tmod;
  if (a->numMods)
    free(a->mod);
  a->mod = tmoda;

  /*     a->curMod = a->numMods; */
  a->numMods++;
  /*     a->imod = tmod; */

  /* DNM: changes for storage of object properties in view and 
     relying on default scaling - switched to new method 6/26/03 */

  imodvViewsInitialize(tmod);

  imodvSelectModel(a, a->numMods - 1);
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
    fout = fopen(LATIN1(QDir::convertSeparators(QString(a->imod->fileName))),
                 "wb");

  if (fout){
    error = writeOpenedModelFile(a, fout);
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
  qname = imodPlugGetSaveName(NULL, 
                                       "Select file to save model into:");
  if (qname.isEmpty())
    return;
  filename = strdup(LATIN1(qname));

  /* DNM 8/4/01: store the current view when saving, if appropriate */
  imodvAutoStoreView(a);

  len = strlen(filename)+1;

  nfname1 = (char *)malloc(len + 1);
  sprintf(nfname1, "%s~", filename);
  if (!stat(filename, &buf)) {
    remove(nfname1);
    rename(filename, nfname1);
  }

  fout = fopen(LATIN1(QDir::convertSeparators(QString(filename))), "wb");
  if (fout){
    error = writeOpenedModelFile(a, fout);

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

// Do common functions for writing model file after file opened
// Convert rotation to flip for saving, and back afterwards
static int writeOpenedModelFile(ImodvApp *a, FILE *fout)
{
  int error;
  a->imod->file = fout;
  utilExchangeFlipRotation(a->imod, ROTATION_TO_FLIP);
  error = imodWrite(a->imod, a->imod->file);
  utilExchangeFlipRotation(a->imod, FLIP_TO_ROTATION);
  fflush(fout);
  fclose(fout);
  a->imod->file = NULL;
  if (!error)
    dia_puts("Model file saved.");
  return error;
}

// The file menu dispatch function
void imodvFileMenu(int item)
{
  QString qname, format;

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
    format = (item == VFILE_MENU_SNAPRGB) ? 
      ImodPrefs->snapFormat() : QString("TIFF");
    qname = imodPlugGetSaveName
      (NULL, QString("File to save ") + format + " snapshot into:");
    if (qname.isEmpty())
      break;
    imodv_auto_snapshot(qname, item == VFILE_MENU_SNAPRGB ? 
                        SnapShot_RGB : SnapShot_TIF);
    break;

  case VFILE_MENU_ZEROSNAP:
    imodvResetSnap();
    break;

  case VFILE_MENU_SNAPDIR:
    Imodv->mainWin->releaseKeyboard();
    b3dSetSnapDirectory();
    break;

  case VFILE_MENU_MOVIE:
    mvMovieDialog(Imodv, 1);
    break;
 
  case VFILE_MENU_SEQUENCE:
    mvMovieSequenceDialog(Imodv, 1);
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
  Iobj *xobj;
  Icont *cont;
  bool freeXobj;
  switch (item) {
  case VVIEW_MENU_DB:
    imodv_setbuffer(a, 1 - a->dblBuf, -1, -1);
    a->mainWin->setEnabledMenuItem(VVIEW_MENU_TRANSBKGD, a->dblBuf &&
                                   (a->enableDepthDBal >= 0 ||
                                    a->enableDepthDBstAl >= 0));
    break;

  case VVIEW_MENU_TRANSBKGD:
    imodv_setbuffer(a, -1, -1, 1 - a->transBkgd);
    if (a->transBkgd)
      a->transBkgd = 0;
    else
      a->transBkgd = a->alphaVisual;
    a->mainWin->setCheckableItem(VVIEW_MENU_TRANSBKGD, a->transBkgd);
    a->mainWin->setEnabledMenuItem(VVIEW_MENU_DB, a->dbPossible && a->enableDepthSB >= 0
                                   && !a->transBkgd);
    break;

  case VVIEW_MENU_INVERTZ:
    toggleWorldFlag(a->invertZ, VIEW_WORLD_INVERT_Z, VVIEW_MENU_INVERTZ);
    break;

  case VVIEW_MENU_LIGHTING:
    toggleWorldFlag(a->lighting, VIEW_WORLD_LIGHT, VVIEW_MENU_LIGHTING);
    break;

  case VVIEW_MENU_WIREFRAME:
    toggleWorldFlag(a->wireframe, VIEW_WORLD_WIREFRAME, VVIEW_MENU_WIREFRAME);
    break;
 
  case VVIEW_MENU_LOWRES:
    toggleWorldFlag(a->lowres, VIEW_WORLD_LOWRES, VVIEW_MENU_LOWRES);
    break;

  case VVIEW_MENU_STEREO:
    imodvStereoEditDialog(a, 1);
    break;

  case VVIEW_MENU_DEPTH:
    imodvDepthCueEditDialog(a, 1);
    break;

  case VVIEW_MENU_SCALEBAR:
    scaleBarOpen();
    break;

  case VVIEW_MENU_RESIZE:
    a->mainWin->openResizeTool(a);
    break;

  case VVIEW_MENU_BOUNDBOX:
    freeXobj = true;
    if (a->boundBoxExtraObj <= 0) {
      a->boundBoxExtraObj = ivwGetFreeExtraObjectNumber(a->vi);
      if (a->boundBoxExtraObj <= 0)
        return;
      xobj = ivwGetAnExtraObject(a->vi, a->boundBoxExtraObj);
      if (xobj) {
        imodObjectDefault(xobj);
        strcpy(xobj->name, "Volume bounding box extra object");
        xobj->cont = imodContoursNew(6);
        if (xobj->cont) {
          xobj->contsize = 6;
          xobj->flags |= IMOD_OBJFLAG_OPEN | IMOD_OBJFLAG_WILD | 
            IMOD_OBJFLAG_EXTRA_MODV | IMOD_OBJFLAG_EXTRA_EDIT | 
            IMOD_OBJFLAG_ANTI_ALIAS | IMOD_OBJFLAG_MODV_ONLY;
          xobj->red = 1.;
          xobj->green = 1.;
          xobj->blue = 0.;
          xobj->linewidth = 2;
          if (!imodvAddBoundingBox(a))
            freeXobj = false;
        }
      }
    }
    if (freeXobj) {
      ivwFreeExtraObject(a->vi, a->boundBoxExtraObj);
      a->boundBoxExtraObj = 0;
    }
    a->mainWin->setCheckableItem(VVIEW_MENU_BOUNDBOX, !freeXobj);
    imodvObjedNewView();
    imodvDraw(a);
    break;

  case VVIEW_MENU_CURPNT:
    freeXobj = true;
    if (a->curPointExtraObj <= 0) {
      a->curPointExtraObj = ivwGetFreeExtraObjectNumber(a->vi);
      if (a->curPointExtraObj <= 0)
        return;
      xobj = ivwGetAnExtraObject(a->vi, a->curPointExtraObj);
      if (xobj) {
        imodObjectDefault(xobj);
        strcpy(xobj->name, "Current point extra object");
        xobj->flags |= IMOD_OBJFLAG_SCAT | IMOD_OBJFLAG_MESH |
          IMOD_OBJFLAG_NOLINE | IMOD_OBJFLAG_FILL | IMOD_OBJFLAG_EXTRA_MODV |
          IMOD_OBJFLAG_EXTRA_EDIT | IMOD_OBJFLAG_MODV_ONLY;
        xobj->pdrawsize = 7. * a->vi->xybin;
        xobj->red = 1.;
        xobj->green = 0.;
        xobj->blue = 0.;
        xobj->quality = 4;
        cont = imodContourNew();
        if (cont) {
          imodPointAppendXYZ(cont, 0., 0., 0.);
          imodPointSetSize(cont, 0, 5. * a->vi->xybin);
          if (cont->psize && cont->sizes && imodObjectAddContour(xobj, cont)
              >= 0)
            freeXobj = false;
          free(cont);
        }
      }
    }
    if (freeXobj) {
      ivwFreeExtraObject(a->vi, a->curPointExtraObj);
      a->curPointExtraObj = 0;
    }
    a->mainWin->setCheckableItem(VVIEW_MENU_CURPNT, !freeXobj);
    imodvObjedNewView();
    imodvDraw(a);
    break;
  }
}

// Toggle a flag in the world structure that corresponds to a checkable item
static void toggleWorldFlag(int &globalVal, b3dUInt32 mask, int menuID)
{
  imodvRegisterModelChg();
  imodvFinishChgUnit();
  globalVal = 1 - globalVal;
  setOrClearFlags(&Imodv->imod->view->world, mask, globalVal);
  Imodv->mainWin->setCheckableItem(menuID, globalVal);
  imodvDraw(Imodv);
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

void imodvMenuInvertZ(int value)
{
  Imodv->mainWin->setCheckableItem(VVIEW_MENU_INVERTZ, value);
}

// Add the contours of the bounding box for the current model
int imodvAddBoundingBox(ImodvApp *a)
{
  float xx, yy, zz;
  int i, j, ind;
  if (a->boundBoxExtraObj <= 0)
    return 1;
  Iobj *xobj = ivwGetAnExtraObject(a->vi, a->boundBoxExtraObj);
  if (!xobj)
    return 1;
  for (i =0; i < 6; i++)
    imodContourClearPoints(&xobj->cont[i]);
  for (i = 0; i < 2; i++) {
    zz = i * a->imod->zmax;
    imodPointAppendXYZ(&xobj->cont[i], -1., -1., zz);
    imodPointAppendXYZ(&xobj->cont[i], (float)a->imod->xmax, -1., zz);
    imodPointAppendXYZ(&xobj->cont[i], (float)a->imod->xmax, 
                       (float)a->imod->ymax, zz);
    imodPointAppendXYZ(&xobj->cont[i], -1., (float)a->imod->ymax, zz);
    imodPointAppendXYZ(&xobj->cont[i], -1., -1., zz);
  }
  for (i = 0; i < 2; i++) {
    for (j = 0; j < 2; j++) {
      xx = i * a->imod->xmax;
      yy = j * a->imod->ymax;
      ind = 2 + i + 2 * j;
      imodPointAppendXYZ(&xobj->cont[ind], xx, yy, -1.);
      imodPointAppendXYZ(&xobj->cont[ind], xx, yy, (float)a->imod->zmax);
    }
  }
  return 0;
}

// Initially open selected windows
void imodvOpenSelectedWindows(const char *keys)
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
      mvMovieDialog(Imodv, 1);
    if (strchr(keys, 'N'))
      mvMovieSequenceDialog(Imodv, 1);
    if (strchr(keys, 'I') && !Imodv->standalone)
      mvImageEditDialog(Imodv, 1);
    if (strchr(keys, 'U') && !Imodv->standalone)
      imodvIsosurfaceEditDialog(Imodv, 1);
    if (strchr(keys, 'S'))
      imodvStereoEditDialog(Imodv, 1);
    if (strchr(keys, 'D'))
      imodvDepthCueEditDialog(Imodv, 1);
    if (strchr(keys, 'e') && Imodv->standalone)
      scaleBarOpen();
    if (strchr(keys, 'R'))
      Imodv->mainWin->openRotationTool(Imodv);
}


// Background color class
void ImodvBkgColor::openDialog()
{
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

  setModvDialogTitle(mSelector, "3dmodv: ");

  imodvDialogManager.add((QWidget *)mSelector, IMODV_DIALOG);
  adjustGeometryAndShow((QWidget *)mSelector, IMODV_DIALOG);
}

ImodvBkgColor::ImodvBkgColor()
  : QObject(NULL)
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
