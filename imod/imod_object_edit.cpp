/*  IMOD VERSION 2.50
 *
 *  imod_object_edit.c -- Edit how objects are drawn in 2D views
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

$Date$

$Revision$
Log at end of file
*/

#include "form_object_edit.h"
#include "imod.h"
#include "imod_display.h"
#include "imod_object_edit.h"
#include "preferences.h"
#include "colorselector.h"
#include "control.h"
#include "imod_info_cb.h"
#include "dia_qtutils.h"

static objectEditForm *Ioew_dialog;

static Iobj *getObjectOrClose(void);

#define MAX_SYMBOLS  4
static int symTable[MAX_SYMBOLS] = 
  { IOBJ_SYM_NONE, IOBJ_SYM_CIRCLE, IOBJ_SYM_SQUARE, IOBJ_SYM_TRIANGLE };

#define MAX_COLOR_SELECTORS 20
static ImodObjColor *colorObjects[MAX_COLOR_SELECTORS];

void ioew_help(void)
{
  dia_vasmsg("Object Type Help\n",
             "-------------------\n",
             "This dialog edits the current object. ",
             "One can leave this dialog open and change the current ",
             "object.\n\n"
             "Object Name:\n",
             "\tEnter a name for the object.\n\n",
             "Draw:\n",
             "\tTurns drawing on/off in image windows.\n\n",
             "Symbols:\n",
             "\tChooses how symbols are drawn.  Open or filled symbols of "
             "three shapes may be drawn at each model point.  Symbol size "
             "is governed by the Size slider.  Independent of whether "
             "symbols are drawn at each point, if Mark Ends is selected, "
             "green and red crosses are drawn over the first and last "
             "points of a contour.\n\n",
             "Sphere radius for points:\n",
             "\tIf this sphere size is nonzero, then spheres will be drawn "
             "in 3-D at every point of the object.  These spheres appear in"
             " the model view window and appear in cross-section on "
             "one or more slices of the image display.\n\n",
             "Line Width:\n",
             "\tSets the width for lines drawn on images, but not for "
             "lines in 3D.\n\n",
             "Open/Closed/Scattered Toggles:\n",
             "\tSet how points in object are connected.  Open and closed "
             "contour objects are drawn with lines between the points; "
             "closed contours have a line connecting the last point back "
             "to the first one; scattered point objects have no "
             "connecting lines.\n\n",
             "Front Face:\n",
             "\tOutside/Inside toggles select which side of the contours "
             "will be brightly lit after the object is meshed.  This "
             "feature can also be used to select an area of interest as "
             "inside or outside the contours, for some programs.\n\n",
             "Time data:\n",
             "\tIf multiple image files are loaded, this toggle button "
             "controls whether time information is encoded "
             "in contours as they are drawn.  If the button is on, then "
             "each new contour that is created will be assigned to the "
             "currently displayed time, and it will appear only over "
             "images at that time.  In addition, the Time Index text box "
             "in the Contour/Surface/Point window can be used to adjust the "
             "time value of a contour.  If the button is off, then new "
             "contours will not be assigned to the current time but "
             "rather will have a time value of 0 and will appear over "
             "images at all times.",
             NULL);
}

void ioew_draw(int state)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;
  if (state)
    obj->flags = obj->flags & ~IMOD_OBJFLAG_OFF;
  else
    obj->flags = obj->flags | IMOD_OBJFLAG_OFF;

  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

/* 12/1/02: eliminated unused ioew_trans_cb */

void ioew_fill(int state)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;
  if (!state)
    obj->symflags = obj->symflags & ~IOBJ_SYMF_FILL;
  else
    obj->symflags = obj->symflags | IOBJ_SYMF_FILL;

  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ioew_ends(int state)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;
  if (!state)
    obj->symflags = obj->symflags & ~IOBJ_SYMF_ENDS;
  else
    obj->symflags = obj->symflags | IOBJ_SYMF_ENDS;
     
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ioew_linewidth(int value)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;
  obj->linewidth2 = value;
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ioew_open(int value)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;

  switch (value){
  case 0:
    obj->flags = obj->flags & (~IMOD_OBJFLAG_OPEN);
    obj->flags = obj->flags & (~IMOD_OBJFLAG_SCAT);
    break;
  case 1:
    obj->flags = obj->flags | IMOD_OBJFLAG_OPEN;
    obj->flags = obj->flags & (~IMOD_OBJFLAG_SCAT);
    break;
  case 2:
    /* scattered */
    obj->flags = obj->flags | IMOD_OBJFLAG_SCAT;
    obj->flags = obj->flags | IMOD_OBJFLAG_OPEN;
    break;
  }
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ioew_surface(int value)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;

  /* TODO: make sure this is the right polarity */
  if (!value) {
    obj->flags &= ~ IMOD_OBJFLAG_OUT;
  } else {
    obj->flags |= IMOD_OBJFLAG_OUT;
  }

  imodDraw(App->cvi, IMOD_DRAW_MOD);
}


void ioew_pointsize(int value)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;
  obj->pdrawsize = value;
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ioew_nametext(const char *name)
{
  int i;
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;

  if (name){
    for(i = 0; (i < (IMOD_STRSIZE - 1))&&(name[i]); i++)
      obj->name[i] = name[i];
    obj->name[i] = 0x00;
  }
}

void ioew_symbol(int value)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;

  obj->symbol = symTable[value];
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ioew_symsize(int value)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;
  obj->symsize = value;
     
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ioew_time(int state)
{
  Iobj *obj = getObjectOrClose();
  if (!obj)
    return;

  if (!state){
    obj->flags &= ~IMOD_OBJFLAG_TIME;
  }else{
    obj->flags |= IMOD_OBJFLAG_TIME;
  }
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
     
  Ioew_dialog = new objectEditForm(imodDialogManager.parent(IMOD_DIALOG), NULL, 
				   Qt::WType_TopLevel | Qt::WDestructiveClose);

  if (!Ioew_dialog){
    dia_err("Object edit failed.");
    return(-1);
  }

  Ioew_dialog->setCaption(imodCaption("3dmod Object Edit"));
  imodDialogManager.add((QWidget *)Ioew_dialog, IMOD_DIALOG);

  Ioew_dialog->show();

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
  // fprintf(stderr, "updating %d\n", update++);
  obj = getObjectOrClose();
  if (!obj)
    return (-1);

  Ioew_dialog->setObjectName(obj->name);
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
                                   (int)obj->symsize);
  Ioew_dialog->setLineWidth((int)obj->linewidth2);
  Ioew_dialog->setTimeBox((obj->flags & IMOD_OBJFLAG_TIME) != 0,
                          App->cvi->nt != 0);

  if (obj->flags & IMOD_OBJFLAG_SCAT)
    state = 2;
  else if (obj->flags & IMOD_OBJFLAG_OPEN)
    state = 1;
  Ioew_dialog->setObjectType(state);

  Ioew_dialog->setFrontSurface(obj->flags & IMOD_OBJFLAG_OUT ? 1 : 0);
  Ioew_dialog->setPointRadius(obj->pdrawsize);

  return(0);
}

/* Get the current object; if it does not exist, close the dialog */
Iobj *getObjectOrClose(void)
{
  Iobj* obj = imodel_object_get(Model);
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
	fprintf(stderr, "Deleting selector for object %d from index %d\n", 
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
  : QObject(0, 0)
{
  QString qstr;
  
  mObjNum = objNum;
  Iobj *obj = &(Model->obj[objNum]);
  qstr.sprintf("Select color for object %d.", objNum + 1);

  mSelector = new ColorSelector(imodDialogManager.parent(IMOD_DIALOG), 
                                qstr.latin1(),
                                (int)(obj->red * 255.),
                                (int)(obj->green * 255.),
                                (int)(obj->blue * 255.), hotSliderFlag(), 
				hotSliderKey(), "selector");
  connect(mSelector, SIGNAL(newColor(int, int, int)), this, 
          SLOT(newColorSlot(int, int, int)));
  connect(mSelector, SIGNAL(done()), this, SLOT(doneSlot()));
  connect(mSelector, SIGNAL(closing()), this, SLOT(closingSlot()));
  connect(mSelector, SIGNAL(keyPress(QKeyEvent *)), this, 
          SLOT(keyPressSlot(QKeyEvent *)));
  connect(mSelector, SIGNAL(keyRelease(QKeyEvent *)), this, 
          SLOT(keyReleaseSlot(QKeyEvent *)));

  mSelector->setCaption(imodCaption("3dmod Color"));
  imodDialogManager.add((QWidget *)mSelector, IMOD_DIALOG);

  mSelector->show();
}

void ImodObjColor::newColorSlot(int red, int green, int blue)
{
  Iobj *obj;
  // If the object number is now illegal, close the selector
  if (mObjNum >= (int)Model->objsize) {
    mSelector->close();
    return;
  }

  // Get the new color
  obj = &(Model->obj[mObjNum]);
  obj->red = red / 255.0;
  obj->green = green / 255.0;
  obj->blue = blue / 255.0;

  // This was redraw if rgba, but do it in any case because imodv might be open
  /* DNM 1/23/03: no longer free and allocate object colors */
  imod_cmap(Model);
  imodDraw(App->cvi, IMOD_DRAW_MOD);
  imod_info_setobjcolor();
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

/*
$Log$
Revision 4.5  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.4  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.3  2003/03/26 23:23:15  mast
switched from hotslider.h to preferences.h

Revision 4.2  2003/02/27 19:40:06  mast
Add parentheses to fix call to set symbol properties

Revision 4.1  2003/02/10 20:29:00  mast
autox.cpp

Revision 1.1.2.10  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.9  2003/01/23 20:08:26  mast
change name of include for form class

Revision 1.1.2.8  2003/01/14 21:51:42  mast
Register with dialog manager

Revision 1.1.2.7  2003/01/13 01:15:43  mast
changes for Qt version of info window

Revision 1.1.2.6  2003/01/06 18:58:59  mast
eliminate warning

Revision 1.1.2.5  2003/01/06 15:45:21  mast
New object color class and code

Revision 1.1.2.4  2002/12/13 06:03:47  mast
moving imod_object_edit declaration to include file and removing argument

Revision 1.1.2.3  2002/12/09 17:49:57  mast
Getting the object type buttons right

Revision 1.1.2.2  2002/12/07 01:22:02  mast
Taking care of window title

Revision 1.1.2.1  2002/12/05 16:30:58  mast
Qt version

Revision 3.1  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

*/
