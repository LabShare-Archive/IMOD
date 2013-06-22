/*
 *  mv_modeled.cpp -- Model edit dialog for imodv.
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

#include <math.h>
#include <string.h>
#include "formv_modeled.h"
#include "dia_qtutils.h"
#include "imodv.h"
#include "imod.h"
#include "mv_gfx.h"
#include "mv_menu.h"
#include "mv_input.h"
#include "mv_views.h"
#include "mv_modeled.h"
#include "control.h"
#include "undoredo.h"
#include "model_edit.h"

struct imodv_modeled
{
  imodvModeledForm *dia;
  ImodvApp  *a;
};

/* Static structure and pointer */
static struct imodv_modeled medStruct;
static struct imodv_modeled *med = &medStruct;

/* local function */
static void updateWorkArea(void);

void imodvModeledHelp()
{
  imodShowHelpPage("modvModelEdit.html#TOP");
}

// Done: tell the box to close
void imodvModeledDone()
{
  med->dia->close();
}
  
// Closing: unload the pixel size one last time
void imodvModeledClosing()
{
  imodvDialogManager.remove((QWidget *)med->dia);
  imodvModeledScale(0);
  med->dia = NULL;
}

// Open the dialog box
void imodvModelEditDialog(ImodvApp *a, int state)
{
  static int first = 1;

  if (first){
    med->dia = NULL;
    first = 0;
  }
  if (!state){
    if (med->dia)
      med->dia->close();
    return;
  }
  if (med->dia){
    med->dia->raise();
    return;
  }
  med->a = a;

  med->dia = new imodvModeledForm(imodvDialogManager.parent(IMODV_DIALOG), 
                                  Qt::Window);

  updateWorkArea();
  med->dia->setViewSelection(a->drawall);
  med->dia->setMoveEdit(a->moveall, a->crosset);
  imodvDialogManager.add((QWidget *)med->dia, IMODV_DIALOG);
  adjustGeometryAndShow((QWidget *)med->dia, IMODV_DIALOG);
}

// The program is selecting a new model
int imodvSelectModel(ImodvApp *a, int ncm)
{
  // the update model call will call here again, so keep track of whether
  // this function is active and return if called again
  static int selecting = 0;

  if (selecting) 
    return a->curMod;
  selecting = 1;

  // Before changing model, get current scale text and autostore view
  if (med->dia) {
    imodvModeledScale(0);
  }
  imodvAutoStoreView(a);

  if (ncm < 0)
    ncm = 0;
  if (ncm > a->numMods - 1)
    ncm = a->numMods - 1;

  a->curMod = ncm;

  a->imod = a->mod[a->curMod];
  if (a->objNum >= a->imod->objsize)
    a->objNum = 0;
  a->obj = &(a->imod->obj[a->objNum]);

  // If stansalone, maintain items in the fake vi: clear out undo and selection
  // list
  if (a->standalone) {
    a->vi->imod = a->imod;
    a->vi->undo->clearUnits();
    imodSelectionListClear(a->vi);
  }
     
  if (med->dia){
    
    updateWorkArea();
  }
  imodvSetCaption();
  imodvUpdateModel(a, true);
  imodvAddBoundingBox(a);
  imodvDraw(a);

  selecting = 0;
  return(a->curMod);
}

// User selects a new model number through dialog
void imodvModeledNumber(int which)
{
  imodvSelectModel(Imodv, which - 1);
  updateWorkArea();
}

// User selects a new move radio button
void imodvModeledMove(int item)
{
  Imodv->moveall = item;
}

// User picks a new view item
void imodvModeledView(int item)
{
  Imodv->drawall = item;
  imodvDraw(Imodv);
}

// User selects a new edit radio button
void imodvModeledEdit(int item)
{
  Imodv->crosset = item;
}

// User wants models at same scale
void imodvModeledSameScale()
{
  if (!Imodv->standalone)
    return;
  for (int m = 0; m < Imodv->numMods; m++)
    Imodv->mod[m]->view->rad = Imodv->imod->view->rad;
  imodvDraw(Imodv);
}

// User changes the name
void imodvModeledName(QString nameStr)
{
  int i,mi;
  const char *name = LATIN1(nameStr);

  mi = strlen(name);
  if (mi >= IMOD_STRSIZE)
    mi = IMOD_STRSIZE - 1;
  for(i = 0 ; i<mi; i++)
    Imodv->imod->name[i] = name[i];
  Imodv->imod->name[i] = 0x00;

  updateWorkArea();
}

// General call to read and translate scale and optionally update dialog
void imodvModeledScale(int update)
{
  QString scale = med->dia->getPixelString();

  if (!scale.isEmpty()) {
    imodvRegisterModelChg();
    imodvFinishChgUnit();
    setPixsizeAndUnits(med->a->imod, LATIN1(scale));
  }

  //fprintf(stderr, "string %s gives %f for %s, update %d\n", LATIN1(scale),
  //        med->a->imod->pixsize, med->a->imod->fileName, update);
  if (update)
    updateWorkArea();
  imodModelEditUpdate();
}

// Refresh the dialog box for the current model
static void updateWorkArea(void)
{
  QString scale, fileStr;
  QString internalName = Imodv->imod->name;
  char *filename = imodwGivenName(" ", Imodv->imod->fileName);
  if (filename) {
    fileStr = filename;
    free(filename);
  }

  // Request a units string
  filename = imodUnits(Imodv->imod);
  if (filename)
    scale.sprintf("%g %s", Imodv->imod->pixsize, filename);
  else
    scale.sprintf("%g", Imodv->imod->pixsize);

  med->dia->setModel(med->a->curMod + 1, med->a->numMods, fileStr, internalName, scale);
}

// The drawing selection has been changed elsewhere; change dialog
void imeSetViewData(int wi)
{
  if (med->dia)
    med->dia->setViewSelection(wi);
}

void imodvPixelChanged()
{
  if (med->dia)
    updateWorkArea();
}

// Returns starting and ending models to draw based on value of drawall option
void imodvModelDrawRange(ImodvApp *a, int &mstart, int &mend)
{
  mstart = mend = a->curMod;
  switch (a->drawall) {
  case 2:
    mend = B3DMIN(a->curMod + 1, a->numMods - 1);
    break;
  case 1:
    mstart = B3DMAX(a->curMod - 1, 0);
    break;
  case 3:
    mstart = 0;
    mend = a->numMods - 1;
    break;
  }
}
