/*
 *  imodv_modeled.c -- Model edit dialog for imodv.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

#include <math.h>
#include <string.h>
#include "formv_modeled.h"
#include "dia_qtutils.h"
#include "imodv.h"
#include "imod.h"
#include "imodv_gfx.h"
#include "imodv_input.h"
#include "imodv_views.h"
#include "imodv_modeled.h"
#include "control.h"
#include "imod_model_edit.h"

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
  dia_vasmsg
    ("Model Edit Dialog Help.\n\n",
     "\tMultiple models can be loaded with 3dmodv, either on the command "
     "line or through the File menu.  ",
     "To page through the different models use the Current ",
     "Model # spin button box or the 9 and 0 hot keys.\n\n",
     "\tThe Move radio buttons allow one to choose between ",
     "having actions move just the current model, \"One\", or ",
     "having actions move \"All\" of the models.\n\n",
     "\tThe View selection box allows one to choose between ",
     "viewing just the current model, viewing the ",
     "current ",
     "model and the previous model, viewing the current ",
     "model and the next model, ",
     "or viewing all of the models.  The 8 hot key toggles between viewing "
     "one and viewing all models.\n\n"
     "\tThe Edit radio buttons toggle between editing just ",
     "the current model, \"One\", and editing \"All\" of the ",
     "models for each model edit.  This can include editing done ",
     "in the Edit Objects and Edit Controls menus.\n",
     "\tIn the Edit Objects menu, numerous features can be changed in "
     "tandem for the same object in all models, or even for all objects "
     "in all models.  These include: material properties, "
     "transparency, point size, line width, the drawing data type and "
     "style, object color, and whether objects are on or off.\n"
     "\tIn the Edit Controls menu, the clipping planes and perspective "
     "can be changed together, and typing a value into the [Scale] "
     "box will set all models to the same scale.  If the zoom is "
     "changed with arrow buttons or hot keys, all models will change "
     "their zoom by the same amount.\n\n",
     "\tThe \"Internal name\" text box allows you to enter a name that is "
     "stored in the Imod model, separate from the filename.\n\n"
     "\tThe \"Pixel size\" text box allows you to set the pixel size of "
     "the model, which is needed for extracting quantitative information.  "
     "Enter a number then the units of measurement.  Available units are "
     "km, m, cm, um, nm, A, and pm.\n",
     NULL);
  return;
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

  med->dia = new imodvModeledForm(imodvDialogManager.parent(IMODV_DIALOG), NULL,
                                  Qt::WDestructiveClose | Qt::WType_TopLevel);

  updateWorkArea();
  med->dia->setViewSelection(a->drawall);
  med->dia->setMoveEdit(a->moveall, a->crosset);
  imodvDialogManager.add((QWidget *)med->dia, IMODV_DIALOG);
  med->dia->show();
}

// The program is selecting a new model
int imodvSelectModel(ImodvApp *a, int ncm)
{
  // the update model call will call here again, so keep track of whether
  // this function is active and return if called again
  static int selecting = 0;

  if (selecting) 
    return a->cm;
  selecting = 1;

  // Before changing model, get current scale text and autostore view
  if (med->dia) {
    imodvModeledScale(0);
  }
  imodvAutoStoreView(a);

  if (ncm < 0)
    ncm = 0;
  if (ncm > a->nm - 1)
    ncm = a->nm - 1;

  a->cm = ncm;

  a->imod = a->mod[a->cm];
  if (a->ob >= a->imod->objsize)
    a->ob = 0;
  a->obj = &(a->imod->obj[a->ob]);
     
  if (med->dia){
    
    updateWorkArea();
  }
  imodvSetCaption();
  imodvUpdateModel(a, true);
  imodvDraw(a);

  selecting = 0;
  return(a->cm);
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

// User changes the name
void imodvModeledName(QString nameStr)
{
  int i,mi;
  const char *name = nameStr.latin1();

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
    setPixsizeAndUnits(med->a->imod, (char *)scale.latin1());
  }

  //fprintf(stderr, "string %s gives %f for %s, update %d\n", scale.latin1(),
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

  med->dia->setModel(med->a->cm + 1, med->a->nm, fileStr, internalName, scale);
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
