/*
 *  object_edit.cpp -- Edit how objects are drawn in 2D views
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */
#include "form_object_edit.h"
#include "imod.h"
#include "display.h"
#include "object_edit.h"
#include "preferences.h"
#include "colorselector.h"
#include "control.h"
#include "info_cb.h"
#include "mv_objed.h"
#include "dia_qtutils.h"
#include "undoredo.h"
//Added by qt3to4:
#include <QKeyEvent>

static objectEditForm *Ioew_dialog;

static Iobj *getObjectOrClose(void);
static void setObjectFlag(bool state, int symflag, b3dUInt32 flag);

#define MAX_SYMBOLS  4
static int symTable[MAX_SYMBOLS] = 
  { IOBJ_SYM_NONE, IOBJ_SYM_CIRCLE, IOBJ_SYM_SQUARE, IOBJ_SYM_TRIANGLE };

#define MAX_COLOR_SELECTORS 20
static ImodObjColor *colorObjects[MAX_COLOR_SELECTORS];

void ioew_help(void)
{
  imodShowHelpPage("objectEdit.html#TOP");
}

static void setObjectFlag(bool state, int symflag, b3dUInt32 flag)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;
  App->cvi->undo->objectPropChg();
  utilSetObjFlag(obj, symflag, state, flag);
  App->cvi->undo->finishUnit();
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ioew_draw(int state)
{
  setObjectFlag(!state, 0, IMOD_OBJFLAG_OFF);
  imodvObjedNewView();
}

/* 12/1/02: eliminated unused ioew_trans_cb */

void ioew_fill(int state)
{
  setObjectFlag(state != 0, 1, IOBJ_SYMF_FILL);
}

void ioew_ends(int state)
{
  setObjectFlag(state != 0, 1, IOBJ_SYMF_ENDS);
}

void ioew_arrow(int state)
{
  setObjectFlag(state != 0, 1, IOBJ_SYMF_ARROW);
}

void ioew_linewidth(int value)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;
  App->cvi->undo->objectPropChg();
  obj->linewidth2 = value;
  App->cvi->undo->finishUnit();
  imodDraw(App->cvi, IMOD_DRAW_MOD);
  imodvObjedNewView();
}

void ioew_open(int value)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;

  App->cvi->undo->objectPropChg();
  switch (value){
  case 0:
    obj->flags &= ~(IMOD_OBJFLAG_OPEN | IMOD_OBJFLAG_SCAT);
    break;
  case 1:
    obj->flags |= IMOD_OBJFLAG_OPEN;
    obj->flags &= ~IMOD_OBJFLAG_SCAT;
    break;
  case 2:
    /* scattered */
    obj->flags |= IMOD_OBJFLAG_SCAT | IMOD_OBJFLAG_OPEN;
    break;
  }
  App->cvi->undo->finishUnit();
  imodDraw(App->cvi, IMOD_DRAW_MOD);
  imodvObjedNewView();
}

void ioew_surface(int value)
{
  /* TODO: make sure this is the right polarity */
  setObjectFlag(value != 0, 0, IMOD_OBJFLAG_OUT);
}


void ioew_pointsize(int value)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;
  App->cvi->undo->objectPropChg();
  obj->pdrawsize = value;
  App->cvi->undo->finishUnit();
  imodDraw(App->cvi, IMOD_DRAW_MOD);
  imodvObjedNewView();
}

void ioew_nametext(const char *name)
{
  int i;
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;

  if (name){
    App->cvi->undo->objectPropChg();
    for(i = 0; (i < (IOBJ_STRSIZE - 1))&&(name[i]); i++)
      obj->name[i] = name[i];
    obj->name[i] = 0x00;
    App->cvi->undo->finishUnit();
  }
  imodvObjedNewView();
}

void ioew_symbol(int value)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;
  App->cvi->undo->objectPropChg();
  obj->symbol = symTable[value];
  App->cvi->undo->finishUnit();
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ioew_symsize(int value)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;
  App->cvi->undo->objectPropChg();
  obj->symsize = value;
     
  App->cvi->undo->finishUnit();
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ioew_time(int state)
{
  setObjectFlag(state, 0, IMOD_OBJFLAG_TIME);
}

void ioew_sphere_on_sec(int state)
{
  setObjectFlag(state, 0, IMOD_OBJFLAG_PNT_ON_SEC);
}

void ioew_planar(int state)
{
  setObjectFlag(state, 0, IMOD_OBJFLAG_PLANAR);
  imodvObjedNewView();
}

void ioew_pointLimit(int value)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;
  obj->extra[IOBJ_EX_PNT_LIMIT] = value;
}

void ioew_fillTrans(int value)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;
  App->cvi->undo->objectPropChg();
  obj->extra[IOBJ_EX_2D_TRANS] = value;
  App->cvi->undo->finishUnit();
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ioew_outline(int state)
{
  setObjectFlag(state, 0, IMOD_OBJFLAG_POLY_CONT);
}

void ioewCopyObj(int value)
{
  Iobj *obj = getObjectOrClose();
  Iobj *fromObj;
  if (!obj)
    return;
  if (value < 1 || value > Model->objsize) {
    wprint("\aObject number to copy to is out of range\n");
    return;
  }
  if (value - 1 == Model->cindex.object) {
    wprint("\aSelect an object different from the current one.\n");
    return;
  }
  fromObj = &Model->obj[value-1];
  App->cvi->undo->objectPropChg();
  obj->flags = fromObj->flags;
  obj->symbol = fromObj->symbol;
  obj->symsize = fromObj->symsize;
  obj->symflags = fromObj->symflags;
  obj->pdrawsize = fromObj->pdrawsize;
  obj->linewidth2 = fromObj->linewidth2;
  obj->extra[IOBJ_EX_PNT_LIMIT] = fromObj->extra[IOBJ_EX_PNT_LIMIT];
  obj->extra[IOBJ_EX_2D_TRANS] = fromObj->extra[IOBJ_EX_2D_TRANS];
  App->cvi->undo->finishUnit();
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

/* 
 * Open the object edit dialog
 */
int imod_object_edit()
{
  Iobj *obj = imodObjectGet(Model);
  if (!obj){
    dia_err("No Object selected");
    return(-1);
  }

  if (Ioew_dialog){
    Ioew_dialog->raise();
    return(0);
  }
     
  Ioew_dialog = new objectEditForm(imodDialogManager.parent(IMOD_DIALOG),
				   Qt::Window);

  if (!Ioew_dialog){
    dia_err("Object edit failed.");
    return(-1);
  }

  Ioew_dialog->setWindowTitle(imodCaption("3dmod Object Edit"));
  imodDialogManager.add((QWidget *)Ioew_dialog, IMOD_DIALOG);

  adjustGeometryAndShow((QWidget *)Ioew_dialog, IMOD_DIALOG);

  imod_object_edit_draw();
  return(0);
}


/* 
 * External call to refresh the dialog state
 */
int imod_object_edit_draw(void)
{
  int state = 0;
  int symbol = 0;
  int i;
  // static int update = 0;
  Iobj *obj;
  if (!Ioew_dialog)
    return(-1);
  // imodPrintStderr("updating %d\n", update++);
  obj = getObjectOrClose();
  if (!obj)
    return (-1);

  Ioew_dialog->setCopyObjLimit(Model->objsize);
  Ioew_dialog->setObjectName(obj->name);
  Ioew_dialog->setObjectNum(Model->cindex.object);
  
  Ioew_dialog->setDrawBox(!(obj->flags & IMOD_OBJFLAG_OFF));

  for (i = 0; i < MAX_SYMBOLS; i++) {
    if (obj->symbol == symTable[i]) {
      symbol = i;
      break;
    }
  }
  Ioew_dialog->setSymbolProperties(symbol, 
                                   (obj->symflags & IOBJ_SYMF_FILL) != 0,
                                   (obj->symflags & IOBJ_SYMF_ENDS) != 0, 
                                   (obj->symflags & IOBJ_SYMF_ARROW) != 0, 
                                   (int)obj->symsize);
  Ioew_dialog->setLineWidth((int)obj->linewidth2);
  Ioew_dialog->setTimeBox((obj->flags & IMOD_OBJFLAG_TIME) != 0,
                          App->cvi->numTimes != 0);
  Ioew_dialog->setOnSecBox((obj->flags & IMOD_OBJFLAG_PNT_ON_SEC) != 0);

  if (obj->flags & IMOD_OBJFLAG_SCAT)
    state = 2;
  else if (obj->flags & IMOD_OBJFLAG_OPEN)
    state = 1;
  Ioew_dialog->setObjectType(state);

  Ioew_dialog->setFrontSurface(obj->flags & IMOD_OBJFLAG_OUT ? 1 : 0);
  Ioew_dialog->setPointRadius(obj->pdrawsize);
  Ioew_dialog->setPlanarBox(iobjPlanar(obj->flags), iobjOpen(obj->flags));
  Ioew_dialog->setPointLimit(obj->extra[IOBJ_EX_PNT_LIMIT]);
  Ioew_dialog->setFillTrans(obj->extra[IOBJ_EX_2D_TRANS], 
                            (obj->flags & IMOD_OBJFLAG_POLY_CONT) != 0, 
                            (obj->flags & IMOD_OBJFLAG_SCAT) == 0);
  return(0);
}

/* Get the current object; if it does not exist, close the dialog */
Iobj *getObjectOrClose(void)
{
  Iobj* obj = imodObjectGet(Model);
  if (!obj){
    dia_err("No object! Closing Edit dialog.");
    Ioew_dialog->close();
  }
  return obj;
}

/* But only set the pointer null when the signal comes in that it is closing */
void ioew_closing(void)
{
  imod_cmap(Model);
  imodDialogManager.remove((QWidget *)Ioew_dialog);
  Ioew_dialog = NULL;
}

void ioew_quit(void)
{
  Ioew_dialog->close();
}


/*  OBJECT COLOR ENTRY POINT AND CLASS */

// Entry point for opening or raising an object color dialog for given object
void imod_object_color(int objNum)
{
  static int firstTime = 1;
  int i;
  int freeInd = -1;

  // Initialize list of objects to NULL
  if (firstTime)
    for (i = 0; i < MAX_COLOR_SELECTORS; i++)
      colorObjects[i] = NULL;
  firstTime = 0;

  // Garbage collect: delete objects if selector is null
  for (i = 0; i < MAX_COLOR_SELECTORS; i++) 
    if (colorObjects[i] && !colorObjects[i]->mSelector) {
      if (Imod_debug)
	imodPrintStderr("Deleting selector for object %d from index %d\n", 
		colorObjects[i]->mObjNum + 1, i);
      delete colorObjects[i];
      colorObjects[i] = NULL;
    } 

  // now find first free slot, or raise window if it exists
  for (i = 0; i < MAX_COLOR_SELECTORS; i++) {
    if (freeInd < 0 && !colorObjects[i])
      freeInd = i;

    if (colorObjects[i] && colorObjects[i]->mObjNum == objNum) {
      colorObjects[i]->mSelector->raise();
      return;
    }
  }

  // Open the object if there is a free spot
  if (freeInd >= 0)
    colorObjects[freeInd] = new ImodObjColor(objNum);
  else 
    wprint("\aToo many object color selectors open.\n");
}


// Object color class
ImodObjColor::ImodObjColor(int objNum)
  : QObject(0)
{
  QString qstr;
  
  mObjNum = objNum;
  Iobj *obj = &(Model->obj[objNum]);
  qstr.sprintf("Select color for object %d.", objNum + 1);

  mSelector = new ColorSelector(imodDialogManager.parent(IMOD_DIALOG), 
                                LATIN1(qstr),
                                (int)(obj->red * 255.),
                                (int)(obj->green * 255.),
                                (int)(obj->blue * 255.), hotSliderFlag(), 
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

  mSelector->setWindowTitle(imodCaption("3dmod Color"));
  imodDialogManager.add((QWidget *)mSelector, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)mSelector, IMOD_DIALOG);
}

void ImodObjColor::newColorSlot(int red, int green, int blue)
{
  float fred = red / 255.0;
  float fgreen = green / 255.;
  float fblue = blue / 255.;
  Iobj *obj;

  // If the object number is now illegal, close the selector
  if (mObjNum >= (int)Model->objsize) {
    mSelector->close();
    return;
  }

  obj = &(Model->obj[mObjNum]);
  if (obj->red == fred && obj->green == fgreen && obj->blue == fblue)
    return;

  // Only record an undo state the first time when hot sliding
  if (!mSelector->hotSliding()) {
    App->cvi->undo->objectPropChg(mObjNum);
    App->cvi->undo->finishUnit();
  }

  // Store the new color
  obj->red = fred;
  obj->green = fgreen;
  obj->blue = fblue;

  // This was redraw if rgba, but do it in any case because imodv might be open
  /* DNM 1/23/03: no longer free and allocate object colors */
  imod_cmap(Model);
  imodDraw(App->cvi, IMOD_DRAW_MOD);
  imod_info_setobjcolor();
  imodvObjedNewView();
}

void ImodObjColor::doneSlot()
{
  mSelector->close();
}

void ImodObjColor::closingSlot()
{
  imodDialogManager.remove((QWidget *)mSelector);
  mSelector = NULL;
}

void ImodObjColor::keyPressSlot ( QKeyEvent * e )
{
  ivwControlKey(0, e);
}

void ImodObjColor::keyReleaseSlot ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}
