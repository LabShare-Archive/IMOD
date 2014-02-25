/*
 *  mv_objed.cpp -- The object edit and object list dialogs for imodv
 *                   The main form class is imodvObjedForm in formv_objed.cpp
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

#include <sys/types.h>
#include <math.h>
#include <qlabel.h>
#include <qcheckbox.h>
#include <qspinbox.h>
#include <qapplication.h>
#include <qpushbutton.h>
#include <qbuttongroup.h>
#include <qgroupbox.h>
#include <qradiobutton.h>
#include <qlayout.h>
#include <qframe.h>
#include <qtooltip.h>
#include <qsignalmapper.h>
//Added by qt3to4:
#include <QTimerEvent>
#include <QGridLayout>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QDoubleSpinBox>
#include <qpainter.h>
#include "tooledit.h"
#include "formv_objed.h"
#include "multislider.h"
#include "dia_qtutils.h"
#include "mkmesh.h"

#include "imodv.h"
#include "imod.h"
#include "info_setup.h"
#include "object_edit.h"
#include "mv_gfx.h"
#include "mv_light.h"
#include "mv_objed.h"
#include "mv_listobj.h"
#include "mv_input.h"
#include "mv_menu.h"
#include "mv_window.h"
#include "vertexbuffer.h"
#include "display.h"
#include "xcramp.h"
#include "preferences.h"
#include "control.h"

/*
 *  internal prototypes (first one was public but unused)
 */

static void objset(ImodvApp *a);
static void setObjFlag(int flag, int state, int types = 0);
static void setOnoffButtons(void);
static void addOnoffButton(void);
static void finalSpacer(QWidget *parent, QVBoxLayout *layout);     
static void setLineColor_cb(void);
static void mkLineColor_cb(int index);
static void setFillColor_cb(void);
static void mkFillColor_cb(int index);
static void setMaterial(Iobj *obj, int which, int value);
static void setMaterial_cb(void);
static void mkMaterial_cb (int index);
static void setPoints_cb(void);
static void mkPoints_cb(int index);
static void setLines_cb(void);
static void mkLines_cb(int index);
static void setScalar_cb(void);
static void mkScalar_cb(int index);
static void setClip_cb(void);
static void mkClip_cb(int index);
static void fixClip_cb(void);
static void mkMove_cb(int index);
static void fixMove_cb(void);
static void mkSubsets_cb(int index);
static void mkMakeMesh_cb(int index);
static void fixMakeMesh_cb();
static void setMakeMesh_cb(void);
static MeshParams *makeGetObjectParams(Iobj *obj, int ob);
static void meshObject();
static int finishMesh();
static void optionSetFlags (b3dUInt32 *flag);
static int numEditableObjects(int model);
static Iobj *editableObject(int model, int ob);
static void finishChangeAndDraw(int doObjset, int drawImages);
static void setStartEndModel(int &mst, int &mnd, bool multipleOK = true);
static bool changeModelObject(int m, int ob, bool multipleOK = true);
static void setMat3Flag(int flag, int index, int state);
static bool drawOnSliderChange(bool dragging);
static QVBoxLayout *outerVBoxLayout(QWidget *parent);

/* resident instance of the ImodvObjed class, and pointers to the dialog
   box classes when they are created */
static ImodvObjed imodvObjed;
static imodvObjedForm *objed_dialog = NULL;

static int Imodv_objed_all = 0;  /* edit all objects if 1 */
static int ctrlPressed = false;
static bool switchObjInObjset = false;
static int lastNumEditable, lastModNum;

enum {editOne = 0, editAll, editOns, editGroup};

#define stylePoints 0
#define styleLines  1
#define styleFill   2
#define styleFillOutline 3
#define FIELD_MARGIN 3          // margin and spacing for layouts inside 
#define FIELD_SPACING 5         // the edit fields
#define OBJTYPE_CLOSED 1        // Bit flags for the object types
#define OBJTYPE_OPEN 2
#define OBJTYPE_SCAT 4

/* Defines and variables for the On-off buttons in this dialog and in the
   object list dialog */
#define MAX_ONOFF_BUTTONS  48
#define MAX_ONOFF_COLUMNS   6
static QCheckBox *OnoffButtons[MAX_ONOFF_BUTTONS];
static int numOnoffButtons = 0;
static QGridLayout *OnoffGrid;
static QSignalMapper *OnoffMapper;
static QFrame *OnoffFrame;
static QRadioButton *wSubsetPoint;

static int      CurrentObjectField       = 0;
ObjectEditField objectEditFieldData[]    = {
  {"Line Color", mkLineColor_cb, setLineColor_cb, NULL,       NULL},
  {"Fill Color", mkFillColor_cb, setFillColor_cb, NULL,       NULL},
  {"Material",   mkMaterial_cb,  setMaterial_cb,  NULL,       NULL},
  {"Points",     mkPoints_cb,    setPoints_cb,    NULL,       NULL},
  {"Lines",      mkLines_cb,     setLines_cb,     NULL,       NULL},
  {"Values",     mkScalar_cb,    setScalar_cb,    NULL,       NULL},
  {"Clip",       mkClip_cb,      setClip_cb,      fixClip_cb, NULL},
  {"Move",       mkMove_cb,      NULL,            fixMove_cb, NULL},
  {"Subsets",    mkSubsets_cb,   NULL,            NULL,       NULL},
  {"Meshing",    mkMakeMesh_cb,  setMakeMesh_cb,  fixMakeMesh_cb, NULL},

  {NULL, NULL, NULL, NULL, NULL},
};

/* Constructor for resident class */
ImodvObjed::ImodvObjed(QObject *parent, const char *name)
  : QObject(parent)
{
}


/*****************************************************************************/
/**                 Responses to Main Edit Controls from the Form           **/
/*****************************************************************************/

/****************************************************************************
 * Draw option
 */

static unsigned int onTestFlags, offTestFlags, passSetFlags, passClearFlags;
static unsigned int failSetFlags, failClearFlags;
static void optionSetFlags (b3dUInt32 *flag)
{
  if (( (*flag & onTestFlags) == onTestFlags) && 
      ( !(*flag & offTestFlags) || !offTestFlags)) {
    *flag = (*flag | passSetFlags) & ~passClearFlags;
  }else {
    *flag = (*flag | failSetFlags) & ~failClearFlags;
  }
}          

void imodvObjedDrawData(int option)
{
  int m, mst, mnd, ob;
  Iobj *obj;

  switch(option){
  case 0:
    setObjFlag(IMOD_OBJFLAG_OFF, 1);
    break;

  case 3:
    setObjFlag(IMOD_OBJFLAG_OFF, 0);
    objset(Imodv);
    break;

  case 1:
  case 2:
    if (option == 1) {
      /* If going to line, see if MESH and NOLINE and FILL are all on
         and OFF is not; if so then clear all of these flags;
         otherwise just clear the MESH and OFF flags */
      onTestFlags = IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_NOLINE | 
        IMOD_OBJFLAG_FILL;
      offTestFlags = IMOD_OBJFLAG_OFF;
      passSetFlags = 0;
      passClearFlags = onTestFlags;
      failSetFlags = 0;
      failClearFlags = IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_OFF;
    } else {
      /* If going to mesh, see if not OFF or MESH or NOLINE or FILL:
         if so set MESH and NOLINE and FILL; otherwise just set MESH and
         clear OFF flags */
      onTestFlags = 0;
      offTestFlags = IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_NOLINE |
        IMOD_OBJFLAG_FILL | IMOD_OBJFLAG_OFF;
      passSetFlags = IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_NOLINE |
        IMOD_OBJFLAG_FILL;
      passClearFlags = 0;
      failSetFlags = IMOD_OBJFLAG_MESH;
      failClearFlags = IMOD_OBJFLAG_OFF;
    }

    if (!Imodv->imod) return;

    setStartEndModel(mst, mnd);
    
    for (m = mst; m <= mnd; m++) {
      for (ob = 0; ob < numEditableObjects(m); ob++)
        if (changeModelObject(m, ob)) {
          obj = editableObject(m, ob);
          imodvRegisterObjectChg(ob);
          optionSetFlags(&obj->flags);
        }
    }
    objset(Imodv);
    break;

  default:
    break;
  }
  setOnoffButtons();
  finishChangeAndDraw(0, 1);
}


/******************************************************************
 * Style option
 */
void imodvObjedStyleData(int option)
{
  switch(option){
  case stylePoints:
    setObjFlag(IMOD_OBJFLAG_NOLINE , 1);
    setObjFlag(IMOD_OBJFLAG_FILL, 0);
    break;
  case styleLines:
    setObjFlag(IMOD_OBJFLAG_FILL | IMOD_OBJFLAG_NOLINE, 0);
    break;
  case styleFill:
    setObjFlag(IMOD_OBJFLAG_FILL | IMOD_OBJFLAG_NOLINE, 1);
    break;
  case styleFillOutline:
    setObjFlag(IMOD_OBJFLAG_NOLINE, 0);
    setObjFlag(IMOD_OBJFLAG_FILL, 1);
    break;
  default:
    break;
  }
  finishChangeAndDraw(0, 0);
}

// Edit Each/All option
void imodvObjedEditData(int option)
{
  Imodv_objed_all = option;
}

// User selects a new object through spin box or slider
void imodvObjedSelect(int which)
{
  Imodv->objNum = which - 1;
  objset(Imodv);
  if (Imodv->drawClip)
    imodvDraw(Imodv);
}

// A name is changed
void imodvObjedName(const char *name)
{
  int i, mi;
  Iobj *obj = objedObject();
  if (!obj) return;
  
  imodvRegisterObjectChg(Imodv->objNum); 
  mi = strlen(name);
  if (mi >= IOBJ_STRSIZE)
    mi = IOBJ_STRSIZE - 1;
  for(i = 0 ; i<mi; i++)
    obj->name[i] = name[i];
  obj->name[i] = 0x00;
  setOnoffButtons();
  imodvFinishChgUnit();
  imod_object_edit_draw();
}

// Respond to an On-Off button in either the objed or the object list window
void ImodvObjed::toggleObjSlot(int ob)
{
  objedToggleObj(ob, OnoffButtons[ob]->isChecked());
}

void objedToggleObj(int ob, bool state)
{
  int m, mst, mnd;
  setStartEndModel(mst, mnd);
    
    /* Turn off same object in all other models if editing all and legal ob */
  for (m = mst; m <= mnd; m++) {
    if (Imodv->mod[m]->objsize > ob)
      imodvRegisterObjectChg(ob);
      if (state) {
        Imodv->mod[m]->obj[ob].flags &= ~IMOD_OBJFLAG_OFF;
        Imodv->objNum = ob;
      } else
        Imodv->mod[m]->obj[ob].flags |= IMOD_OBJFLAG_OFF;
  }

  /* If the object is within legal limits for a button list, set that button
     in each list */
  if (ob < numOnoffButtons && ob < Imodv->imod->objsize)
    diaSetChecked(OnoffButtons[ob], state);
  imodvOlistSetChecked(Imodv, ob, state);

  finishChangeAndDraw(1, 1);
}

// User selected a new frame; update the frame
void imodvObjedFramePicked(int item)
{
  CurrentObjectField = item;
  if (objectEditFieldData[CurrentObjectField].setwidget)
    objectEditFieldData[CurrentObjectField].setwidget();
}

// Done or Escape is pressed: close unless busy meshing
void imodvObjedDone()
{
  if (!meshingBusy())
    objed_dialog->close();
}

// Signal from window that it is closing: clear pointer
void imodvObjedClosing()
{
  imodvDialogManager.remove((QWidget *)objed_dialog);
  objed_dialog = NULL;
  numOnoffButtons = 0;
}

// Keep track of the Ctrl key
void imodvObjedCtrlKey(bool pressed)
{
  ctrlPressed = pressed;
}

// HELP!
void imodvObjedHelp()
{
  imodShowHelpPage("modvObjectEdit.html#TOP");
}


/****************************************************************************/
/* Object Edit update functions.                                            */
/****************************************************************************/

/* sets object features in objed window. */
static void objset(ImodvApp *a)
{
  int style, type, ob, dir, diff;
  Iobj *obj;
  unsigned int flag;
  char *namep;
  char tmpname[IOBJ_STRSIZE];
  int numEditable = numEditableObjects(a->curMod);

  // If editable extra objects have just appeared, switch to first one
  if (lastModNum == a->curMod && numEditable > lastNumEditable && 
      lastNumEditable == a->mod[a->curMod]->objsize)
    a->objNum = lastNumEditable;
  lastModNum = a->curMod;
  lastNumEditable = numEditable;

  // Adjust object number and set structure variables
  if (a->objNum >= numEditable)
    a->objNum = 0;

  // If editing Ons or a group, adjust object number to the nearest object that
  // was modified
  if (switchObjInObjset) {
    type = 0;
    for (diff = 0; diff < a->imod->objsize && !type; diff++) {
      for (dir = -1; dir <= 1; dir += 2) {
        ob = a->objNum + dir * diff;
        if (ob < 0 || ob >= a->imod->objsize)
          continue;
        if ((Imodv_objed_all == editOns && !iobjOff(a->imod->obj[ob].flags)) ||
            (Imodv_objed_all == editGroup && imodvOlistObjInGroup(a, ob))) {
          a->objNum = ob;
          type = 1;
          break;
        }
      }
    }
    switchObjInObjset = false;
  }

  obj = objedObject();
  if (obj) {

    flag = obj->flags;
    
    // Find the object style number
    if ( iobjFill(flag) ){
      style = styleFillOutline;
      if (flag & IMOD_OBJFLAG_NOLINE)
        style = styleFill;
    }else{
      if (IMOD_OBJFLAG_NOLINE & flag)
        style = stylePoints;
      else
        style = styleLines;
    }
    
    // Find the object type number
    type = 0;
    if (!iobjOff(flag)){
      if (iobjMesh(flag)){
        type = 2;
      }else{
        type = 1;
      }
    }

    namep = obj->name;
    if (!namep[0] && a->objNum >= a->imod->objsize) {
      namep = &tmpname[0];
      sprintf(tmpname, "Extra object #%d", a->objNum + 1 - a->imod->objsize);
    }

    objed_dialog->updateObject(a->objNum + 1, numEditable, type, style, 
                               QColor((int)(255 * obj->red), 
                                      (int)(255 * obj->green),
                                      (int)(255 * obj->blue)), namep);
    wSubsetPoint->setEnabled(iobjScat(obj->flags) != 0);
  }
  if (objectEditFieldData[CurrentObjectField].setwidget)
    objectEditFieldData[CurrentObjectField].setwidget();
}

// Update the state of all On-Off buttons
static void setOnoffButtons(void)
{
  int ob, num;
  bool state;
  ImodvApp *a = Imodv;
  static int numShown = 0;

  imodvOlistUpdateOnOffs(a);
  if (!objed_dialog)
    return;
  num = B3DMAX(numOnoffButtons, B3DMIN(MAX_ONOFF_BUTTONS, a->imod->objsize));
  
  for (ob = 0; ob < num; ob++) {
    while (ob >= numOnoffButtons)
      addOnoffButton();
    if (ob < a->imod->objsize) {
      state = !(a->imod->obj[ob].flags & IMOD_OBJFLAG_OFF);
      OnoffButtons[ob]->show();
    } else {
      state = false;
      OnoffButtons[ob]->hide();
    }
    diaSetChecked(OnoffButtons[ob], state);
  }
  if (numShown != a->imod->objsize)
    objed_dialog->adjustSize();
  numShown = a->imod->objsize;
}


/*****************************************************************************/
/**                CREATION AND EXTERNALLY CALLED FUNCTIONS                 **/
/*****************************************************************************/

/* DNM 2/7/01: call setOnoffButtons before skipping out if no window; it should
   be safe even if neither list or objed window is open */
void imodvObjedNewView(void)
{
  imodvOlistUpdateGroups(Imodv);
  setOnoffButtons();
  if (!objed_dialog)
    return;
  objset(Imodv);
}

// External close command from other parts of program
int object_edit_kill(void)
{
  if (objed_dialog){
    objed_dialog->close();
    return(1);
  }
  return(0);
}

/*****************************************************************************
 *
 * Create the Object Edit Dialog.
 * 
 *****************************************************************************/
void objed(ImodvApp *a)
{
  if (objed_dialog) {
    objed_dialog->raise();
    return;
  }

  if (!a->imod)
    return;

  objed_dialog = new imodvObjedForm(imodvDialogManager.parent(IMODV_DIALOG),
                                    Qt::Window);

  Imodv_objed_all = 0;  // May want to retain this setting
  setOnoffButtons();

  setModvDialogTitle(objed_dialog, "3dmodv Objects: ");

  ctrlPressed = false;
  objed_dialog->setCurrentFrame(CurrentObjectField, Imodv_objed_all);
  lastNumEditable = a->imod->objsize;
  lastModNum = a->curMod;
  objset(a);
  imodvDialogManager.add((QWidget *)objed_dialog, IMODV_DIALOG);
  adjustGeometryAndShow((QWidget *)objed_dialog, IMODV_DIALOG);
}

// Make an on-off button, add it to the grid, and connect it to the mapper
static void addOnoffButton(void)
{
  QString str;
  int ob = numOnoffButtons++;
  str.sprintf("%d",ob + 1);
  OnoffButtons[ob] = new QCheckBox(str, OnoffFrame);
  OnoffButtons[ob]->setFocusPolicy(Qt::NoFocus);
  OnoffGrid->addWidget(OnoffButtons[ob], ob / MAX_ONOFF_COLUMNS, 
                       ob % MAX_ONOFF_COLUMNS);
  OnoffMapper->setMapping(OnoffButtons[ob], ob);
  QObject::connect(OnoffButtons[ob], SIGNAL(toggled(bool)), OnoffMapper,
                   SLOT(map()));
}
    
// This is called by the form class to put On/Off buttons in a frame
void imodvObjedMakeOnOffs(QFrame *frame)
{
  ImodvApp *a = Imodv;
  int ob, m, num = 0;

  OnoffGrid = new QGridLayout(frame);
  OnoffGrid->setContentsMargins(2,2,2,2);
  OnoffGrid->setSpacing(0);
  OnoffMapper = new QSignalMapper(frame);
  OnoffFrame = frame;
  QObject::connect(OnoffMapper, SIGNAL(mapped(int)), &imodvObjed, 
                   SLOT(toggleObjSlot(int)));

  // Make maximum number of buttons needed for all loaded models
  for (m = 0; m < a->numMods; m++)
    num = B3DMAX(num, B3DMIN(MAX_ONOFF_BUTTONS, a->mod[m]->objsize));
  
  for (ob = 0; ob < num; ob++)
    addOnoffButton();
}


/*****************************************************************************
 *
 * THE EDIT FIELDS.
 * Each one is self-contained, with its static variables, slots for responding
 * to widget signals, an update function and a function to make the field
 * 
 *****************************************************************************/

/*******************************************************************
 * The Line Color Edit Field
 *******************************************************************/
static const char *rgbTitles[] = {"Red", "Green", "Blue", "Transparency"};
static MultiSlider *lineSliders;
static bool multipleColorOK = false;

void ImodvObjed::lineColorSlot(int color, int value, bool dragging)
{
  int m, mst, mnd, ob, numChanged = 0;
  float red, green, blue;
  Iobj *obj = objedObject();
  static bool sliding = false;
  if (!obj)
    return;

  // Compose the entire modified RGB triplet so all objects will change color
  // together if desired
  red = obj->red;
  green = obj->green;
  blue = obj->blue;
  switch(color){
  case 0:
    red = value / 255.0;
    break;
  case 1:
    green = value / 255.0;
    break;
  case 2:
    blue = value / 255.0;
    break;
  case 3:
    break;
  }

  setStartEndModel(mst, mnd, multipleColorOK);
    
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < numEditableObjects(m); ob++)
      if (changeModelObject(m, ob, multipleColorOK)) {
        if (!sliding)
          imodvRegisterObjectChg(ob);
        obj = editableObject(m, ob);
        numChanged++;
        if (color < 3) {
          obj->red = red;
          obj->green = green;
          obj->blue = blue;
        } else
          obj->trans = value;
      }
  }

  // Set sliding flag after first change is registered; it should be reset 
  // when slider is dropped
  if (!sliding)
    imodvFinishChgUnit();
  sliding = dragging;

  obj = Imodv->obj;
  if (drawOnSliderChange(dragging)) {
    objset(Imodv);
    imodvDraw(Imodv);
    imodvDrawImodImages();

    // Update all buttons if more than one changed, otherwise just the one
    if (color < 3) {
      if (numChanged > 1) {
        setOnoffButtons();
        imodvOlistUpdateOnOffs(Imodv);
      } else
        imodvOlistSetColor(Imodv, Imodv->objNum);
    }
  } else if (color < 3)
    objed_dialog->updateColorBox(QColor((int)(255 * obj->red),
                                        (int)(255 * obj->green),
                                        (int)(255 * obj->blue)));
}

void ImodvObjed::multipleColorSlot(bool state)
{
  multipleColorOK = state;
}

static void setLineColor_cb(void)
{
  Iobj *obj = objedObject();
  if (!obj)
    return;
  /* Set red, green and blue values. */
  lineSliders->setValue(0, (int)(obj->red * 255));
  lineSliders->setValue(1, (int)(obj->green * 255));
  lineSliders->setValue(2, (int)(obj->blue * 255));
  lineSliders->setValue(3, obj->trans);
}

static void mkLineColor_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = outerVBoxLayout(oef->control);
  lineSliders = new MultiSlider(oef->control, 4, rgbTitles);
  lineSliders->setRange(3, 0,100);    // Transparency
  layout1->addLayout(lineSliders->getLayout());
  QObject::connect(lineSliders, SIGNAL(sliderChanged(int, int, bool)), 
                   &imodvObjed, SLOT(lineColorSlot(int, int, bool)));
  
  QCheckBox *check = diaCheckBox("Change multiple objects", oef->control,
                                 layout1);
  QObject::connect(check, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(multipleColorSlot(bool)));
  diaSetChecked(check, multipleColorOK);
  check->setToolTip("Apply color changes to all objects, all objects that "
                "are on, or objects in multiple models");

  finalSpacer(oef->control, layout1);

}


/*******************************************************************
 * The Fill Color Edit Field
 *******************************************************************/

static MultiSlider *fillSliders;
static QCheckBox *wFillToggle = 0;
static QCheckBox *wFillPntToggle = 0;

void ImodvObjed::fillToggleSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_FCOLOR, state ? 1 : 0);
  finishChangeAndDraw(1, 0);
}

void ImodvObjed::fillPntToggleSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_FCOLOR_PNT, state ? 1 : 0);
  finishChangeAndDraw(1, 0);
}

void ImodvObjed::fillColorSlot(int color, int value, bool dragging)
{
  Iobj *obj = objedObject();
  unsigned char *colors;
  static bool sliding = false;

  if (!obj) return;
  colors = (unsigned char *)&(obj->fillred);
  colors[color] = value;

  // Register an object change the first time, set flag after that
  if (!sliding) {
    imodvRegisterObjectChg(Imodv->objNum);
    imodvFinishChgUnit();
  }
  sliding = dragging;

  if (drawOnSliderChange(dragging)) {
    objset(Imodv);
    imodvDraw(Imodv);
  }
}

static void setFillColor_cb(void)
{
  Iobj *obj = objedObject();
  if (!obj) return;
  unsigned char *colors = (unsigned char *)&(obj->fillred);

  diaSetChecked(wFillToggle, obj->flags & IMOD_OBJFLAG_FCOLOR);
  diaSetChecked(wFillPntToggle, obj->flags & IMOD_OBJFLAG_FCOLOR_PNT);
  wFillPntToggle->setEnabled(!(obj->flags & IMOD_OBJFLAG_FCOLOR));
  for (int i = 0; i < 3; i++)
    fillSliders->setValue(i, colors[i]);
}

static void mkFillColor_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = outerVBoxLayout(oef->control);
  fillSliders = new MultiSlider(oef->control, 3, rgbTitles);
  layout1->addLayout(fillSliders->getLayout());
  QObject::connect(fillSliders, SIGNAL(sliderChanged(int, int, bool)), 
                   &imodvObjed, SLOT(fillColorSlot(int, int, bool)));

  wFillToggle = diaCheckBox("Use fill color", oef->control, layout1);
  QObject::connect(wFillToggle, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(fillToggleSlot(bool)));
  wFillToggle->setToolTip(
                "Use fill color instead of object color for all filled data");

  wFillPntToggle = diaCheckBox("Use for spheres", oef->control, layout1);
  QObject::connect(wFillPntToggle, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(fillPntToggleSlot(bool)));
  wFillPntToggle->setToolTip(
                "Use fill color instead of object color for spheres");

  finalSpacer(oef->control, layout1);

}


/*******************************************************************
 * The Material Edit Field
 *******************************************************************/

static MultiSlider *matSliders;
static const char *matTitles[] = {"Ambient", "Diffuse", "Specular", "Shininess"};
static QCheckBox *wBothSides  = 0;

// Set the relevant object material property - including normal magnitude
// Black and white levels
static void setMaterial(Iobj *obj, int which, int value)
{
  switch (which) {
  case 0:
    obj->ambient = (unsigned char)value;
    break;
  case 1:
    obj->diffuse = (unsigned char)value;
    break;
  case 2:
    obj->specular = (unsigned char)value;
    break;
  case 3:
    obj->shininess = (unsigned char)value;
    break;
  case 4:
    obj->valblack = (unsigned char)value;
    break;
  case 5:
    obj->valwhite = (unsigned char)value;
    break;
  }
}

void ImodvObjed::materialSlot(int which, int value, bool dragging)
{
  int m, mst, mnd, ob;
  static bool sliding = false;
  Iobj *obj;

  if (!Imodv->imod) return;
     
  setStartEndModel(mst, mnd);
    
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < numEditableObjects(m); ob++)
      if (changeModelObject(m, ob)) {
        obj = editableObject(m, ob);
        if (!sliding)
          imodvRegisterObjectChg(ob);
        setMaterial(obj, which, value);
      }
  }
  /*     imodPrintStderr("set mat %d, offset %d, value%d\n", *item, offset, cbs->value); */
     
  sliding = dragging;
  imodvFinishChgUnit();
  if (drawOnSliderChange(dragging))
    imodvDraw(Imodv);
}

void ImodvObjed::bothSidesSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_TWO_SIDE, state ? 1 : 0);
  finishChangeAndDraw(1, 0);
}

static void setMaterial_cb(void)
{
  Iobj *obj = objedObject();
  if (!obj) return;

  matSliders->setValue(0, (int)obj->ambient);
  matSliders->setValue(1, (int)obj->diffuse);
  matSliders->setValue(2, (int)obj->specular);
  matSliders->setValue(3, (int)obj->shininess);
  diaSetChecked(wBothSides, obj->flags & IMOD_OBJFLAG_TWO_SIDE);
}


static void mkMaterial_cb (int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = outerVBoxLayout(oef->control);
  matSliders = new MultiSlider(oef->control, 4, matTitles);
  layout1->addLayout(matSliders->getLayout());
  matSliders->getSlider(0)->setToolTip(
                "Set non-directional light hitting object");
  matSliders->getSlider(1)->setToolTip(
                "Set light hitting object from light source");
  matSliders->getSlider(2)->setToolTip(
                "Set specular reflection properties of object");
  matSliders->getSlider(3)->setToolTip(
                "Set shininess of object");
  QObject::connect(matSliders, SIGNAL(sliderChanged(int, int, bool)), 
                   &imodvObjed, SLOT(materialSlot(int, int, bool)));

  wBothSides = diaCheckBox("Light both sides", oef->control, layout1);
  QObject::connect(wBothSides, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(bothSidesSlot(bool)));
  wBothSides->setToolTip(
                "Make front and back surface both appear brightly lit"); 
  finalSpacer(oef->control, layout1);
}


/*****************************************************************************
 * The point edit field
 *****************************************************************************/

static QSpinBox *wPointSizeBox;
static QSpinBox *wPointQualityBox;
static QSpinBox *wGlobalQualityBox;
static QCheckBox *wPointNoDrawBox;

void ImodvObjed::pointSizeSlot(int i)
{
  int m, mst, mnd, ob;
  Iobj *obj;

  if (!Imodv->imod) return;

  objed_dialog->setFocus();

  setStartEndModel(mst, mnd);
    
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < numEditableObjects(m); ob++)
      if (changeModelObject(m, ob)) {
        obj = editableObject(m, ob);
        imodvRegisterObjectChg(ob);
        obj->pdrawsize = i;
      }
  }
  finishChangeAndDraw(0, 1);
}

void ImodvObjed::pointQualitySlot(int value)
{
  int m, mst, mnd, ob;
  Iobj *obj;
  value--;
  if (!Imodv->imod) return;

  objed_dialog->setFocus();

  setStartEndModel(mst, mnd);
     
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < numEditableObjects(m); ob++)
      if (changeModelObject(m, ob)) {
        obj = editableObject(m, ob);
        imodvRegisterObjectChg(ob);
        obj->quality = value;
      }
  }
  finishChangeAndDraw(0, 0);
}

void ImodvObjed::globalQualitySlot(int value)
{
  int m, mst, mnd;
  if (!Imodv->imod) return;
  value--;
  objed_dialog->setFocus();

  imodvRegisterModelChg();
  setStartEndModel(mst, mnd);

  for (m = mst; m <= mnd; m++)
    Imodv->mod[m]->view->world = 
      (Imodv->mod[m]->view->world & ~WORLD_QUALITY_BITS) |
      (value << WORLD_QUALITY_SHIFT);
  finishChangeAndDraw(0, 0);
}

void ImodvObjed::pointNoDrawSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_PNT_NOMODV, state ? 1 : 0, 
             OBJTYPE_OPEN | OBJTYPE_CLOSED);
  finishChangeAndDraw(0, 0);
}

static void setPoints_cb(void)
{
  QString str;
  Iobj *obj = objedObject();
  if (!obj)
    return;

  diaSetSpinBox(wPointSizeBox, obj->pdrawsize);
  diaSetSpinBox(wPointQualityBox, obj->quality + 1);
  diaSetSpinBox(wGlobalQualityBox, 
                ((Imodv->imod->view->world & WORLD_QUALITY_BITS) >> 
                WORLD_QUALITY_SHIFT) + 1);
  diaSetChecked(wPointNoDrawBox, obj->flags & IMOD_OBJFLAG_PNT_NOMODV);
  wPointNoDrawBox->setEnabled(!iobjScat(obj->flags));
}

static void mkPoints_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = outerVBoxLayout(oef->control);
  QGridLayout *grid = new QGridLayout();
  layout1->addLayout(grid);
  QLabel *label = new QLabel("Sphere Size", oef->control);
  wPointSizeBox = new QSpinBox(oef->control);
  wPointSizeBox->setRange(0, 255);
  wPointSizeBox->setSingleStep(1);
  wPointSizeBox->setFocusPolicy(Qt::ClickFocus);
  wPointSizeBox->setKeyboardTracking(false);
  grid->addWidget(label, 1, 1);
  grid->addWidget(wPointSizeBox, 1, 2);
  label = new QLabel("Object Quality", oef->control);
  wPointQualityBox = new QSpinBox(oef->control);
  wPointQualityBox->setRange(1, 4);
  wPointQualityBox->setSingleStep(1);
  wPointQualityBox->setFocusPolicy(Qt::ClickFocus);
  wPointQualityBox->setKeyboardTracking(false);
  grid->addWidget(label, 2, 1);
  grid->addWidget(wPointQualityBox, 2, 2);
  label = new QLabel("Global Quality", oef->control);
  wGlobalQualityBox = new QSpinBox(oef->control);
  wGlobalQualityBox->setFocusPolicy(Qt::ClickFocus);
  wGlobalQualityBox->setRange(1, 4);
  wGlobalQualityBox->setSingleStep(1);
  wGlobalQualityBox->setKeyboardTracking(false);
  grid->addWidget(label, 3, 1);
  grid->addWidget(wGlobalQualityBox, 3, 2);
  wPointSizeBox->setToolTip("Set radius of sphere to draw at each point"
                " in pixels");
  wPointQualityBox->setToolTip("Set quality of sphere-drawing for this "
                "object");
  wGlobalQualityBox->setToolTip("Set overall quality of sphere-drawing");
  
  QObject::connect(wPointSizeBox, SIGNAL(valueChanged(int)), &imodvObjed,
          SLOT(pointSizeSlot(int)));
  QObject::connect(wPointQualityBox, SIGNAL(valueChanged(int)), &imodvObjed,
          SLOT(pointQualitySlot(int)));
  QObject::connect(wGlobalQualityBox, SIGNAL(valueChanged(int)), &imodvObjed,
          SLOT(globalQualitySlot(int)));
  wPointNoDrawBox = diaCheckBox("Skip if drawing mesh", oef->control,
                                layout1);
  QObject::connect(wPointNoDrawBox, SIGNAL(toggled(bool)), &imodvObjed,
                   SLOT(pointNoDrawSlot(bool)));
  wPointNoDrawBox->setToolTip("Skip drawing spheres when drawing object "
                "mesh");

  finalSpacer(oef->control, layout1);
}


/*****************************************************************************
 * The line edit field
 *****************************************************************************/

static MultiSlider *widthSlider;
static QCheckBox *wLineAlias;
static QCheckBox *wThickenCont;
static QCheckBox *wOpenObject;
static QCheckBox *wAutoNewCont;
static const char *widthLabel[] = {"2D Line Width", "3D Line Width"};

void ImodvObjed::lineWidthSlot(int which, int value, bool dragging)
{
  int m, mst, mnd, ob;
  static bool sliding = false;
  Iobj *obj;
     
  if (!Imodv->imod)
    return;
     
  setStartEndModel(mst, mnd);
     
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < numEditableObjects(m); ob++)
      if (changeModelObject(m, ob)) {
        if (!sliding)
          imodvRegisterObjectChg(ob);
        obj = editableObject(m, ob);
        if (which)
          obj->linewidth = value;
        else
          obj->linewidth2 = value;
      }
  }

  sliding = dragging;
  imodvFinishChgUnit();
  if (drawOnSliderChange(dragging)) {
    imodvDraw(Imodv);
    if (!which)
      imodvDrawImodImages();
  }
}

void ImodvObjed::lineAliasSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_ANTI_ALIAS, state ? 1 : 0);
  finishChangeAndDraw(1, 0);
}

void ImodvObjed::lineThickenSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_THICK_CONT, state ? 1 : 0);
  finishChangeAndDraw(1, 0);
}

void ImodvObjed::openObjectSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_OPEN, state ? 1 : 0, OBJTYPE_OPEN | OBJTYPE_CLOSED);
  finishChangeAndDraw(1, 1);
}

void ImodvObjed::autoNewContSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_PLANAR, state ? 1 : 0, OBJTYPE_OPEN);
  finishChangeAndDraw(1, 1);
}

static void setLines_cb(void)
{
  Iobj *obj = objedObject();
  if (!obj) 
    return;
  widthSlider->setValue(0, obj->linewidth2);
  widthSlider->setValue(1, obj->linewidth);
  diaSetChecked(wLineAlias, obj->flags & IMOD_OBJFLAG_ANTI_ALIAS );
  diaSetChecked(wThickenCont, obj->flags & IMOD_OBJFLAG_THICK_CONT);
  diaSetChecked(wOpenObject, iobjOpen(obj->flags));
  diaSetChecked(wAutoNewCont, iobjPlanar(obj->flags));
  wOpenObject->setEnabled(!iobjScat(obj->flags));
  wAutoNewCont->setEnabled(iobjOpen(obj->flags));
}

static void mkLines_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = outerVBoxLayout(oef->control);
  widthSlider = new MultiSlider(oef->control, 2, widthLabel, 1, 10);
  widthSlider->setRange(1, 1, 100);
  layout1->addLayout(widthSlider->getLayout());
  QObject::connect(widthSlider, SIGNAL(sliderChanged(int, int, bool)), 
                   &imodvObjed, SLOT(lineWidthSlot(int, int, bool)));
  widthSlider->getSlider(0)->setToolTip(
                "Set line width in pixels for 2-D display on images");
  widthSlider->getSlider(1)->setToolTip(
                "Set line width in pixels for 3-D model view display");

  wLineAlias = diaCheckBox("Anti-alias rendering", oef->control, layout1);
  QObject::connect(wLineAlias, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(lineAliasSlot(bool)));
  wLineAlias->setToolTip("Smooth lines with anti-aliasing");

  wThickenCont = diaCheckBox("Thicken current contour", oef->control, layout1);
  QObject::connect(wThickenCont, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(lineThickenSlot(bool)));
  wThickenCont->setToolTip("Draw current contour thicker");

  wOpenObject = diaCheckBox("Open object type", oef->control, layout1);
  QObject::connect(wOpenObject, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(openObjectSlot(bool)));
  wOpenObject->setToolTip("Select open or closed contour object types");

  wAutoNewCont = diaCheckBox("New contour at new Z", oef->control, layout1);
  QObject::connect(wAutoNewCont, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(autoNewContSlot(bool)));
  wAutoNewCont->setToolTip("Automatically start new contour at new "
                "section");
  finalSpacer(oef->control, layout1);
}


/*****************************************************************************
 * The scalar edit field
 *****************************************************************************/
static QCheckBox *wMeshSkipLo;
static QCheckBox *wMeshSkipHi;
static QCheckBox *wMeshFalse;
static QCheckBox *wMeshConstant;
static MultiSlider *meshSliders;
static QButtonGroup *wMeshGroup;
static MeshColorBar *wMeshColorBar;
static int meshMin, meshMax;
static float meshRmin, meshRmax;

static const char *bwLabels[] = {"Black Level", "White Level"};
#define COLOR_BAR_HEIGHT 6

void ImodvObjed::meshShowSlot(int value)
{
  setObjFlag(IMOD_OBJFLAG_SCALAR, value == 2 ? 1 : 0);
  setObjFlag(IMOD_OBJFLAG_USE_VALUE, value == 1 ? 1 : 0);
  wMeshSkipLo->setEnabled(value == 1);
  wMeshSkipHi->setEnabled(value == 1);
  finishChangeAndDraw(1, 1);
  wMeshColorBar->update();
}

void ImodvObjed::meshFalseSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_MCOLOR, state ? 1 : 0);
  finishChangeAndDraw(1, 1);
  wMeshColorBar->update();
}

void ImodvObjed::meshConstantSlot(bool state)
{
  setMat3Flag(MATFLAGS2_CONSTANT, 0, state ? 1 : 0);
  finishChangeAndDraw(1, 1);
}

void ImodvObjed::meshLevelSlot(int which, int value, bool dragging)
{
  int sclval = (int)floor((255. * (value - meshMin)) / (meshMax - meshMin) + 
                          0.5);
  materialSlot(which + 4, sclval, dragging);
  if (drawOnSliderChange(dragging))
    imodvDrawImodImages();
  wMeshColorBar->update();
}

void ImodvObjed::meshSkipLoSlot(bool state)
{
  setMat3Flag(MATFLAGS2_SKIP_LOW, 0, state ? 1 : 0);
  finishChangeAndDraw(1, 1);
}

void ImodvObjed::meshSkipHiSlot(bool state)
{
  setMat3Flag(MATFLAGS2_SKIP_HIGH, 0, state ? 1 : 0);
  finishChangeAndDraw(1, 1);
}

static void setScalar_cb(void)
{
  unsigned char *ub;
  int which, i, decimals = 0;
  Iobj *obj = objedObject();
  if (!obj) 
    return;
  ub = (unsigned char *)&(obj->valblack);

  which = (IMOD_OBJFLAG_SCALAR & obj->flags) ? 2 : 0;
  meshRmin = 0.;
  meshRmax = 255.;
  if (IMOD_OBJFLAG_USE_VALUE & obj->flags) {
    which = 1;
    istoreGetMinMax(obj->store, obj->contsize, GEN_STORE_MINMAX1, &meshRmin, &meshRmax);
  }

  diaSetGroup(wMeshGroup, which);
  diaSetChecked(wMeshFalse, IMOD_OBJFLAG_MCOLOR & obj->flags);
  diaSetChecked(wMeshSkipLo, (obj->matflags2 & MATFLAGS2_SKIP_LOW) != 0);
  diaSetChecked(wMeshSkipHi, (obj->matflags2 & MATFLAGS2_SKIP_HIGH) != 0);
  diaSetChecked(wMeshConstant, (obj->matflags2 & MATFLAGS2_CONSTANT) != 0);
  wMeshConstant->setEnabled(which == 1);
  wMeshFalse->setEnabled(which != 1 || !(obj->matflags2 & MATFLAGS2_CONSTANT));
  wMeshSkipLo->setEnabled(which == 1);
  wMeshSkipHi->setEnabled(which == 1);
     
  // Set up scaling of slider values
  if (meshRmax > meshRmin)
    decimals = (int)(-log10((meshRmax - meshRmin) / 1500.));
  decimals = B3DMIN(6, B3DMAX(0, decimals));
  meshMin = (int)floor(meshRmin * pow(10., (double)decimals) + 0.5);
  meshMax = (int)floor(meshRmax * pow(10., (double)decimals) + 0.5);
  if (meshMin >= meshMax)
    meshMax = meshMin + 1;

  for (i = 0; i < 2; i++) {
    meshSliders->setDecimals(i, decimals);
    meshSliders->setRange(i, meshMin, meshMax);
    meshSliders->setValue(i, (int)floor(ub[i] * (meshMax - meshMin) / 255. + 
                                        meshMin + 0.5));
  }
  wMeshColorBar->update();
}

static void mkScalar_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = outerVBoxLayout(oef->control);

  wMeshGroup = new QButtonGroup(oef->control);
  QObject::connect(wMeshGroup, SIGNAL(buttonClicked(int)), &imodvObjed,
                   SLOT(meshShowSlot(int)));

  QVBoxLayout *vLayout = diaVBoxLayout(layout1);
  vLayout->setSpacing(1);
  diaRadioButton("No value drawing", oef->control, wMeshGroup, vLayout, 0,
                 NULL);
  diaRadioButton("Show stored values", oef->control, wMeshGroup, vLayout, 1,
                  "Use stored values to modify display color");
  diaRadioButton("Show normal magnitudes", oef->control, wMeshGroup, vLayout, 
                 2, "Make surface intensity proportional to magnitude of "
                 "normal vectors");

  meshSliders = new MultiSlider(oef->control, 2, bwLabels);
  layout1->addLayout(meshSliders->getLayout());
  QObject::connect(meshSliders, SIGNAL(sliderChanged(int, int, bool)),
                   &imodvObjed, SLOT(meshLevelSlot(int, int, bool)));
  meshSliders->getSlider(0)->setToolTip("Set low end of "
                "contrast ramp for displaying values");
  meshSliders->getSlider(1)->setToolTip("Set high end of "
                "contrast ramp for displaying values");

  wMeshColorBar = new MeshColorBar(oef->control);
  layout1->addWidget(wMeshColorBar);
  wMeshColorBar->setFixedHeight(COLOR_BAR_HEIGHT);

  QHBoxLayout *hLayout = diaHBoxLayout(layout1);
  QLabel *label = new QLabel("Color:", oef->control);
  label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  hLayout->addWidget(label);

  wMeshFalse = diaCheckBox("False", oef->control, hLayout);
  QObject::connect(wMeshFalse, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(meshFalseSlot(bool)));
  wMeshFalse->setToolTip("Show values in false color");

  wMeshConstant = diaCheckBox("Fixed", oef->control, hLayout);
  QObject::connect(wMeshConstant, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(meshConstantSlot(bool)));
  wMeshConstant->setToolTip("Keep color constant instead of changing with"
                " value");

  hLayout = diaHBoxLayout(layout1);
  label = new QLabel("Turn off:", oef->control);
  label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  hLayout->addWidget(label);
  wMeshSkipLo = diaCheckBox("Low", oef->control, hLayout);
  QObject::connect(wMeshSkipLo, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(meshSkipLoSlot(bool)));
  wMeshSkipHi = diaCheckBox("High", oef->control, hLayout);
  QObject::connect(wMeshSkipHi, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(meshSkipHiSlot(bool)));
  wMeshSkipLo->setToolTip("Do not draw items with values below low "
                "limit of black/white sliders");
  wMeshSkipHi->setToolTip("Do not draw items with values above high "
                "limit of black/white sliders");

  finalSpacer(oef->control, layout1);
}

/*
 * The value color bar widget constructor and paint function
 */
MeshColorBar::MeshColorBar(QWidget * parent)
  : QWidget(parent)
{
}
void MeshColorBar::paintEvent(QPaintEvent * event)
{
  int ix, index, length = wMeshColorBar->width();
  int red, green, blue;
  float black, white, val, minDiff = (meshRmax - meshRmin) / 255.;
  Iobj *obj = objedObject();

  // It is cleared to background so if don't need the bar, don't draw anything
  if (!obj || !(IMOD_OBJFLAG_MCOLOR & obj->flags))
    return;

  QPainter painter(this);
  unsigned char *ub = (unsigned char *)&(obj->valblack);
  black = B3DMIN(ub[0], ub[1]) * (meshRmax - meshRmin) / 255. + meshRmin;
  white = B3DMAX(ub[0], ub[1]) * (meshRmax - meshRmin) / 255. + meshRmin;
  for (ix = 0; ix < length; ix++) {
    val = meshRmin + (meshRmax - meshRmin) * ix / (length - 1.);
    index = B3DNINT(255. * (val - black) / B3DMAX(minDiff, white - black));
    B3DCLAMP(index, 0, 255);
    xcramp_mapfalsecolor(index, &red, &green, &blue);
    painter.setPen(QColor(red, green, blue));
    painter.drawLine(ix, 0, ix, COLOR_BAR_HEIGHT - 1);
  }
}

/*****************************************************************************
 * The clip edit field
 *****************************************************************************/
static QCheckBox *wClipToggle;
static QCheckBox *wClipSkipGlobal;
static QButtonGroup *wClipGlobalGroup;
static QSpinBox *wClipPlaneSpin;
static QCheckBox *wClipMoveAll;
static QPushButton *wClipButtons[4];

void ImodvObjed::clipShowSlot(bool state)
{
  Imodv->drawClip = state ? 1 : 0;
  imodvDraw(Imodv);
}

void ImodvObjed::clipGlobalSlot(int value)
{
  Imodv->imod->editGlobalClip = value;
  setClip_cb();
}

void ImodvObjed::clipSkipSlot(bool state)
{
  Iobj *obj = objedObject();
  if (!obj)
    return;
  imodvRegisterObjectChg(Imodv->objNum);
  if (state)
    obj->clips.flags |= (1 << 7);
  else
    obj->clips.flags &= ~(1 << 7);
  finishChangeAndDraw(0, 0);
}

void ImodvObjed::clipPlaneSlot(int value)
{
  Iobj *obj = objedObject();     
  objed_dialog->setFocus();
  if (Imodv->imod->editGlobalClip) {
    imodvRegisterModelChg();
    Imodv->imod->view->clips.plane = value - 1;
  } else {
    if (!obj)
      return;
    imodvRegisterObjectChg(Imodv->objNum);
    obj->clips.plane = value - 1;
  }
  imodvFinishChgUnit();
  setClip_cb();
  if (Imodv->drawClip)
    imodvDraw(Imodv);
}

void ImodvObjed::clipResetSlot(int which)
{
  Ipoint min, max, mid;
  int ip;
  IclipPlanes *clips;
  Iobj *obj = objedObject();     

  if (Imodv->imod->editGlobalClip) {
    imodvRegisterModelChg();
    clips = &Imodv->imod->view->clips;
    imodGetBoundingBox(Imodv->imod, &min, &max);
  } else {
    if (!obj)
      return;
    imodvRegisterObjectChg(Imodv->objNum);
    clips = &obj->clips;
    imodObjectGetBBox(obj, &min, &max);
  }

  mid.x = (max.x + min.x) * -0.5f;
  mid.y = (max.y + min.y) * -0.5f;
  mid.z = (max.z + min.z) * -0.5f;
  ip = clips->plane;
  clips->point[ip] = mid;
  clips->normal[ip].x = clips->normal[ip].y = clips->normal[ip].z = 0.0f;
  if (which == 2)
    clips->normal[ip].z = -1.0f;
  else if (which == 1)
    clips->normal[ip].y = -1.0f;
  else
    clips->normal[ip].x = -1.0f;
  finishChangeAndDraw(0, 0);
}

void ImodvObjed::clipInvertSlot()
{
  Iobj *obj = objedObject();     
  IclipPlanes *clips;
  int ip, ipst, ipnd;
  if (!obj && !Imodv->imod->editGlobalClip)
    return;
  clips = Imodv->imod->editGlobalClip ? 
    &Imodv->imod->view->clips : &obj->clips;

  if (Imodv->imod->editGlobalClip)
    imodvRegisterModelChg();
  else
    imodvRegisterObjectChg(Imodv->objNum);
  ipst = ipnd = clips->plane;
  if (Imodv->imod->view->world & WORLD_MOVE_ALL_CLIP) {
    ipst = 0;
    ipnd = clips->count - 1;
  }

  // This did not require it to be on when it was just one, so keep that
  // behavior when there is just one.
  for (ip = ipst; ip <= ipnd; ip++) {
    if (clips->flags & (1 << ip) || ipst == ipnd) {
      clips->normal[ip].x = -clips->normal[ip].x;
      clips->normal[ip].y = -clips->normal[ip].y;
      clips->normal[ip].z = -clips->normal[ip].z;
    }
  }
  finishChangeAndDraw(0, 0);
}

void ImodvObjed::clipToggleSlot(bool state)
{
  Iobj *obj = objedObject();     
  int ip, ipst, ipnd;
  if (!obj && !Imodv->imod->editGlobalClip)
    return;
  IclipPlanes *clips = Imodv->imod->editGlobalClip ? 
    &Imodv->imod->view->clips : &obj->clips;
  ip = clips->plane;
  if (Imodv->imod->editGlobalClip)
    imodvRegisterModelChg();
  else
    imodvRegisterObjectChg(Imodv->objNum);
     
  ipst = ipnd = clips->plane;
  if (Imodv->imod->view->world & WORLD_MOVE_ALL_CLIP) {
    ipst = 0;
    ipnd = B3DMAX(clips->count - 1, ipnd);
  }

  for (ip = ipst; ip <= ipnd; ip++) {
    if (!state)
      clips->flags &= ~(1 << ip);
    else {
      clips->flags |= (1 << ip);
      
    // When a plane is turned on, increment clip count to include it
      if (ip == clips->count) {
        clips->count++;
        setClip_cb();
      }
      /* imodPrintStderr("plane %d  flags %d  count %d\n", ip, clips->flags,
         clips->count); */
      /* DNM: if this is the first time it's been turned on, set to
         middle of object */
      /* imodPrintStderr("%f %f %f %f %f %f\n", clips->point[ip].x,
         clips->point[ip].y, clips->point[ip].z, clips->normal[ip].x,
         clips->normal[ip].y, clips->normal[ip].z); */
      if (clips->point[ip].x == 0.0 && clips->point[ip].y == 0.0 &&
          clips->point[ip].z == 0.0 && clips->normal[ip].x == 0.0 &&
          clips->normal[ip].y == 0.0 && clips->normal[ip].z == -1.0) {
        clipResetSlot(2);
        return;
      }
    }
  }
  finishChangeAndDraw(1, 0);
}

// External call to toggle the clipping plane
void imodvObjedToggleClip(void)
{
  bool state;
  Iobj *obj = objedObject();     
  if (!obj && !Imodv->imod->editGlobalClip)
    return;
  IclipPlanes *clips = Imodv->imod->editGlobalClip ? 
    &Imodv->imod->view->clips : &obj->clips;
  state = (clips->flags & (1 << clips->plane)) == 0;
  imodvObjed.clipToggleSlot(state);
  if (objed_dialog)
    diaSetChecked(wClipToggle, state);
}


void ImodvObjed::clipMoveAllSlot(bool state)
{
  int m, mst, mnd;

  imodvRegisterModelChg();
  setStartEndModel(mst, mnd);

  for (m = mst; m <= mnd; m++) {
    if (state)
      Imodv->mod[m]->view->world |= WORLD_MOVE_ALL_CLIP;
    else
      Imodv->mod[m]->view->world &= ~WORLD_MOVE_ALL_CLIP;
  }
  imodvFinishChgUnit();
}

static void setClip_cb(void)
{
  int max = IMOD_CLIPSIZE;
  bool editGlobal = Imodv->imod->editGlobalClip != 0;
  Iobj *obj = objedObject();
  if (!obj && !editGlobal) 
    return;
  IclipPlanes *clips = editGlobal ? &Imodv->imod->view->clips : &obj->clips;
  
  diaSetGroup(wClipGlobalGroup, editGlobal);
  wClipSkipGlobal->setEnabled(!editGlobal);
  diaSetChecked(wClipSkipGlobal, !editGlobal && (clips->flags & (1 << 7)));

  // Set spin box max to one past number of clip planes up to max size
  if (max > clips->count + 1)
    max = clips->count + 1;
  diaSetSpinMMVal(wClipPlaneSpin, 1, max, clips->plane + 1);

  diaSetChecked(wClipToggle, (clips->flags & (1 << clips->plane)) != 0);
  diaSetChecked(wClipMoveAll, (Imodv->imod->view->world & WORLD_MOVE_ALL_CLIP)
                != 0);
}

static void mkClip_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = outerVBoxLayout(oef->control);
  layout1->setSpacing(B3DMAX(3, FIELD_SPACING-2));

  // Show current plane is a global display property, so do it first
  QCheckBox *cbox = diaCheckBox("Show current plane", oef->control, layout1);
  diaSetChecked(cbox, Imodv->drawClip != 0);
  QObject::connect(cbox, SIGNAL(toggled(bool)), &imodvObjed,
                   SLOT(clipShowSlot(bool)));
  cbox->setToolTip("Draw semi-transparent plane for clipping plane being"
                " edited");

  // Set up the radio button in a group box
  QGroupBox *gbox = new QGroupBox("Planes to edit", oef->control);
  layout1->addWidget(gbox);
  QHBoxLayout *gbLayout = new QHBoxLayout(gbox);
  gbLayout->setContentsMargins(4, 0, 4, 2);  // L T R B
  wClipGlobalGroup = new QButtonGroup(gbox);
  diaRadioButton("Object", gbox, wClipGlobalGroup, gbLayout, 0, "Adjust object"
                 "clip planes (use "CTRL_STRING " Key to adjust)");
  diaRadioButton("Global", gbox, wClipGlobalGroup, gbLayout, 1,
                 "Adjust global clip planes, applied to whole model"
                 " (use "CTRL_STRING" Key to adjust)");
  QObject::connect(wClipGlobalGroup, SIGNAL(buttonClicked(int)), &imodvObjed, 
                   SLOT(clipGlobalSlot(int)));

  // The skip global planes checkbox
  wClipSkipGlobal = diaCheckBox("Skip global planes", oef->control, layout1);
  QObject::connect(wClipSkipGlobal, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(clipSkipSlot(bool)));
  wClipSkipGlobal->setToolTip(
                "Do not apply any global clipping planes to this object");

  // Set up the spin button
  QHBoxLayout *hLayout = diaHBoxLayout(layout1);
  wClipPlaneSpin = (QSpinBox *)diaLabeledSpin(0, 1., 1., 1., "Plane #",
                                              oef->control, hLayout);

  QObject::connect(wClipPlaneSpin, SIGNAL(valueChanged(int)), &imodvObjed, 
                   SLOT(clipPlaneSlot(int)));
  wClipPlaneSpin->setToolTip("Select plane to adjust (use "CTRL_STRING
                " Key to adjust)");
  hLayout->addStretch();

  // The ON/OFF checkbox
  wClipToggle = diaCheckBox("Clipping plane ON", oef->control, layout1);
  QObject::connect(wClipToggle, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(clipToggleSlot(bool)));
  wClipToggle->setToolTip("Toggle selected clipping plane (hot key "
                          CTRL_STRING"-C)");

  // 3 mapped reset buttons
  QSignalMapper *mapper = new QSignalMapper(oef->control);
  QObject::connect(mapper, SIGNAL(mapped(int)), &imodvObjed,
                   SLOT(clipResetSlot(int)));
  hLayout = diaHBoxLayout(layout1);
  diaLabel("Reset:", oef->control, hLayout);

  for (int i = 0; i < 3; i++) {
    wClipButtons[i] = diaPushButton((char *)(i ? (i == 1 ? "Y" : "Z") : "X"),
                           oef->control, hLayout);
    mapper->setMapping(wClipButtons[i], i);
    QObject::connect(wClipButtons[i], SIGNAL(clicked()), mapper, SLOT(map()));
    wClipButtons[i]->setToolTip(QString("Reset clipping plane back to ") +
                  QString(i ? (i == 1 ? "Y/Z" : "X/Z") :"X/Y") +
                  " plane through center of object or model");
  }

  // Invert button
  hLayout->addStretch();
  wClipButtons[3] = diaPushButton("Invert", oef->control, hLayout);
  QObject::connect(wClipButtons[3], SIGNAL(clicked()), &imodvObjed,
                   SLOT(clipInvertSlot()));
  wClipButtons[3]->setToolTip(
                "Make clipped part visible, clip visible part of object");

  wClipMoveAll = diaCheckBox("Adjust all ON planes", oef->control, layout1);
  QObject::connect(wClipMoveAll, SIGNAL(toggled(bool)), &imodvObjed,
                   SLOT(clipMoveAllSlot(bool)));
  wClipMoveAll->setToolTip("Move/rotate all planes that are ON in object "
                "together (use "CTRL_STRING" Key to adjust)");

  //diaLabel("Press "CTRL_STRING" Key to adjust", oef->control, layout1);
  //diaLabel("adjust with mouse or keys", oef->control, layout1);
  //diaLabel("plane with mouse or", oef->control, layout1);
  //diaLabel("keypad & arrow keys", oef->control, layout1); */
  fixClip_cb();
  finalSpacer(oef->control, layout1);
}

static void fixClip_cb() 
{
  for (int i = 0; i < 3; i++)
    diaSetButtonWidth(wClipButtons[i], ImodPrefs->getRoundedStyle(), 1.9, "X");
  diaSetButtonWidth(wClipButtons[3], ImodPrefs->getRoundedStyle(), 1.4,
                    "Invert");
}


/*****************************************************************************
 * The move edit field
 *****************************************************************************/
/* DNM changed to provide enough buttons to do complete rotations about each
   of the three axes */

static const char *moveLabels[] = {"Top", "Front", "Bottom", "Back",
                             "Top", "Left", "Bottom", "Right",
                             "Front", "Left", "Back", "Right"};
static int moveQuarters[] = {0, -1, 2, 1, 0, 1, 2, -1, 2, -1, 0, 1};
static const char *axisLabels[] = {"X", "Y", "Z"};
static QPushButton *moveButtons[12];

void ImodvObjed::moveCenterSlot()
{
  Ipoint min, max;
  Imod *imod = Imodv->imod;
  objedObject();

  if ((!Imodv->obj) || (!imod))
    return;

  if (imodObjectGetBBox(Imodv->obj, &min, &max) < 0)
    return;
  Imodv->imod->view->trans.x = -((max.x + min.x) * 0.5f);
  Imodv->imod->view->trans.y = -((max.y + min.y) * 0.5f);
  Imodv->imod->view->trans.z = -((max.z + min.z) * 0.5f);
  imodvDraw(Imodv);
}

/* DNM: add control of all models */
void ImodvObjed::moveAxisSlot(int which)
{
  imodvObjedMoveToAxis(which);
}

void imodvObjedMoveToAxis(int which)
{
  int quarter = moveQuarters[which];
  int m;
  Ipoint rot;
  Imod *imod = Imodv->imod;
  if (!imod)
    return;

  rot.x = 0.0f;
  rot.y = 0.0f;
  rot.z = 0.0f;

  switch (which / 4) {
  case 0:    // X axis
    rot.x = quarter * 90.0f;
    break;
  case 1:   // Y axis
    rot.y = quarter * 90.0f;
    break;
  case 2:   // Z axis
    rot.x = 90.0f;
    rot.y = 180.0f;
    rot.z = quarter * 90.0f;
    break;
  }
  imod->view->rot = rot;
  imodvNewModelAngles(&rot);

  if (Imodv->moveall)
    for (m = 0; m < Imodv->numMods; m++)
      Imodv->mod[m]->view->rot = imod->view->rot;

  imodvDraw(Imodv);
}

static void mkMove_cb(int index)
{
  int icol;
  QPushButton *button;
  QLabel *label;
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = outerVBoxLayout(oef->control);
  button = diaPushButton("Center on Object", oef->control, layout1);
  QObject::connect(button, SIGNAL(clicked()), &imodvObjed, 
                   SLOT(moveCenterSlot()));

  label = diaLabel("Move by rotating around:", oef->control, layout1);

  // Get a grid and a signal mapper to map the buttons
  QGridLayout *grid = new QGridLayout();
  layout1->addLayout(grid);
  grid->setSpacing(3);
  QSignalMapper *mapper = new QSignalMapper(oef->control);
  QObject::connect(mapper, SIGNAL(mapped(int)), &imodvObjed,
                   SLOT(moveAxisSlot(int)));

  // Make the buttons, put in mapper and grid
  for (int i = 0; i < 12; i++) {
    icol = i / 4;
    if (i % 4 == 0) {
      label = new QLabel(axisLabels[icol], oef->control);
      grid->addWidget(label, 0, icol);
      label->setAlignment(Qt::AlignCenter);
    }

    moveButtons[i] = new QPushButton(moveLabels[i], oef->control);
    moveButtons[i]->setFocusPolicy(Qt::NoFocus);
    grid->addWidget(moveButtons[i], i % 4 + 1, icol);
    mapper->setMapping(moveButtons[i], i);
    QObject::connect(moveButtons[i], SIGNAL(clicked()), mapper, SLOT(map()));
  }    
  fixMove_cb();

  finalSpacer(oef->control, layout1);
}         

static void fixMove_cb() 
{
  int width = diaGetButtonWidth(moveButtons[0], ImodPrefs->getRoundedStyle(), 
                                1.2,"Bottom");
  for (int i = 0; i < 12; i++)
    moveButtons[i]->setFixedWidth(width);
}

/*****************************************************************************
 * The subsets edit field
 *****************************************************************************/

static const char *subsetLabels[7] = {
  "Show all ON objects", 
  "Current object only", 
  "Current surface only", 
  "Surface && other objects", 
  "Current contour only", 
  "Contour && other objects",
  "Point && other objects"                                
};

void ImodvObjed::subsetSlot(int which)
{
  Imodv->current_subset = which;
  imodvDraw(Imodv);
}

static void mkSubsets_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = outerVBoxLayout(oef->control);
  diaLabel("Show subset of model:", oef->control, layout1);

  QButtonGroup *group = new QButtonGroup(oef->control);
  
  QObject::connect(group, SIGNAL(buttonClicked(int)), &imodvObjed, 
		   SLOT(subsetSlot(int)));

  for (int i = 0; i < 7; i++)
    wSubsetPoint = diaRadioButton(subsetLabels[i], oef->control, group, layout1, i, NULL);
  if (!ImodPrefs->getRoundedStyle())
    layout1->setSpacing(2);

  diaSetGroup(group, Imodv->current_subset);
  layout1->addStretch();
  //finalSpacer(oef->control, layout1);
}


/*****************************************************************************
 * The make mesh edit field
 *****************************************************************************/

enum MakeCheckInds {MAKE_MESH_SKIP = 0, MAKE_MESH_TUBE, MAKE_MESH_LOW, 
                    MAKE_MESH_SURF, MAKE_MESH_CAP, MAKE_MESH_DOME};
enum MakeSpinInds {MESH_SPIN_PASS, MESH_SPIN_DIAM, MESH_SPIN_TOL, MESH_SPIN_ZINC,
                   MESH_SPIN_FLAT};
static QCheckBox *wMakeChecks[6];
static QSpinBox *wMakePassSpin;
static QDoubleSpinBox *wMakeDiamSpin;
static QDoubleSpinBox *wMakeTolSpin;
static QSpinBox *wMakeZincSpin;
static QDoubleSpinBox *wMakeFlatSpin;
static ToolEdit *wMakeZedit;
static QPushButton *wMakeDoButton;
static QPushButton *wMakeAllButton;
static int makeLowRes = 0;
static MeshParams *makeParams;
static MeshParams makeDefParams;
static bool meshAllObjects;

void ImodvObjed::makePassSlot(int value)
{
  makeSpinChanged(MESH_SPIN_PASS, (double)value);
}

void ImodvObjed::makeDiamSlot(double value)
{
  makeSpinChanged(MESH_SPIN_DIAM, value);
}

void ImodvObjed::makeTolSlot(double value)
{
  makeSpinChanged(MESH_SPIN_TOL, value);
}

void ImodvObjed::makeZincSlot(int value)
{
  makeSpinChanged(MESH_SPIN_ZINC, (double)value);
}

void ImodvObjed::makeFlatSlot(double value)
{
  makeSpinChanged(MESH_SPIN_FLAT, value);
}

void ImodvObjed::makeSpinChanged(int which, double value)
{
  MeshParams *param;
  Iobj *obj;
  int mst, mnd, ob, m, any = 0;
  objed_dialog->setFocus();
  if (!Imodv->imod || !objedObject())
    return;
  setStartEndModel(mst, mnd);
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < Imodv->mod[m]->objsize; ob++) {
      obj = editableObject(m, ob);
      if (changeModelObject(m, ob) && !iobjScat(obj->flags)) {
        param = makeGetObjectParams(obj, ob);
        any = 1;
        switch (which) {
        case MESH_SPIN_PASS:
          param->passes = B3DNINT(value);
          break;
        case MESH_SPIN_DIAM:
          param->tubeDiameter = value;
          break;
        case MESH_SPIN_TOL:
          if (makeLowRes)
            param->tolLowRes = value;
          else
            param->tolHighRes = value;
          break;
        case MESH_SPIN_ZINC:
          if (makeLowRes)
            param->inczLowRes = B3DNINT(value);
          else
            param->inczHighRes = B3DNINT(value);
          break;
        case MESH_SPIN_FLAT:
          param->flatCrit = value;
          break;
        }
      }
    }
  }
  if (any)
    imodvFinishChgUnit();
}

void ImodvObjed::makeStateSlot(int which)
{
  bool state = wMakeChecks[which]->isChecked();
  Iobj *obj = objedObject();
  int mst, mnd, ob, m, any = 0;
  bool tube;
  MeshParams *param;
  objed_dialog->setFocus();

  if (!Imodv->imod || !objedObject())
    return;

  if (which == MAKE_MESH_LOW) {
    makeLowRes = state ? 1 : 0;
  } else {

    setStartEndModel(mst, mnd);
    for (m = mst; m <= mnd; m++) {
      for (ob = 0; ob < Imodv->mod[m]->objsize; ob++) {
        obj = editableObject(m, ob);
        if (changeModelObject(m, ob) && !iobjScat(obj->flags)) {
          param = makeGetObjectParams(obj, ob);
          tube = (makeParams->flags & IMESH_MK_TUBE) && iobjOpen(obj->flags);
          any = 1;
          switch (which) {
          case MAKE_MESH_SKIP:
            if (!tube)
              setOrClearFlags(&param->flags, IMESH_MK_SKIP, state ? 1 : 0);
            break;
          case MAKE_MESH_SURF:
            if (!tube)
              setOrClearFlags(&param->flags, IMESH_MK_SURF, state ? 1 : 0);
            break;
          case MAKE_MESH_TUBE:
            if (iobjOpen(obj->flags))
              setOrClearFlags(&param->flags, IMESH_MK_TUBE, state ? 1 : 0);
            break;
          case MAKE_MESH_CAP:
            if ((param->flags & IMESH_MK_TUBE) && iobjOpen(obj->flags))
              setOrClearFlags(&param->flags, IMESH_MK_CAP_TUBE, state ? 1 : 0);
            else
              param->cap = state ? IMESH_CAP_ALL : IMESH_CAP_OFF;
            break;
          case MAKE_MESH_DOME:
            if (tube)
              setOrClearFlags(&param->flags, IMESH_MK_CAP_DOME, state ? 1 : 0);
            break;
          }
        }
      }
    }
    if (any)
      imodvFinishChgUnit();
  }
  setMakeMesh_cb();
}

void ImodvObjed::makeZeditSlot()
{
  QString str;
  QStringList strlist;
  Iobj *obj;
  int mst, mnd, minz, maxz, ob, m, any = 0;
  MeshParams *param;
  bool ok;
  objed_dialog->setFocus();

  if (!Imodv->imod || !objedObject())
    return;
  setStartEndModel(mst, mnd);
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < Imodv->mod[m]->objsize; ob++) {
      obj = editableObject(m, ob);
      if (changeModelObject(m, ob) && !iobjScat(obj->flags)) {
        param = makeGetObjectParams(obj, ob);
        any = 1;
        str = wMakeZedit->text();
        param->minz = DEFAULT_VALUE;
        param->maxz = DEFAULT_VALUE;
        if (str.contains(QRegExp("[0-9]"))) {
          strlist = str.split(",", QString::SkipEmptyParts);
          minz = strlist[0].toInt(&ok) - 1;
          if (ok) {
            if (strlist.size() == 1) {
              param->minz = minz;
              param->maxz = minz + 1;
            } else {
              maxz = strlist[1].toInt(&ok) - 1;
              if (ok) {
                param->minz = B3DMIN(minz, maxz);
                param->maxz = B3DMAX(param->minz + 1, B3DMAX(minz, maxz));
              }
            }
          }
        }
      }
    }
    if (any)
      imodvFinishChgUnit();
  }
  setMakeMesh_cb();
}

// Get meshing parameters from the object, make them if they don't exist, and
// return the default params if necessary
static MeshParams *makeGetObjectParams(Iobj *obj, int ob)
{
  if (!obj)
    return &makeDefParams;
  imodvRegisterObjectChg(ob);
  if (!obj->meshParam)
    obj->meshParam = imeshParamsNew();
  if (obj->meshParam) 
    return obj->meshParam;
  wprint("\aError getting memory for mesh parameters\n");
  return &makeDefParams;
}

// Update the mesh making panel
static void setMakeMesh_cb(void)
{
  bool cap, scat, tube;
  Iobj *obj = objedObject();
  QString str = "";
  if (!obj) 
    return;
  if (obj->meshParam)
    makeParams = obj->meshParam;
  else
    makeParams = &makeDefParams;
  scat = iobjScat(obj->flags);
  tube = (makeParams->flags & IMESH_MK_TUBE) && iobjOpen(obj->flags);
  diaSetDoubleSpinBox (wMakeTolSpin, makeLowRes ? makeParams->tolLowRes : 
                       makeParams->tolHighRes);
  diaSetSpinBox(wMakeZincSpin, makeLowRes ? 
                makeParams->inczLowRes : makeParams->inczHighRes);
  diaSetSpinBox(wMakePassSpin, makeParams->passes);
  diaSetDoubleSpinBox(wMakeDiamSpin, makeParams->tubeDiameter);
  diaSetDoubleSpinBox(wMakeFlatSpin, makeParams->flatCrit);
  diaSetChecked(wMakeChecks[MAKE_MESH_SKIP], 
                (makeParams->flags & IMESH_MK_SKIP) != 0);
  diaSetChecked(wMakeChecks[MAKE_MESH_SURF], 
                (makeParams->flags & IMESH_MK_SURF) != 0);
  diaSetChecked(wMakeChecks[MAKE_MESH_TUBE], 
                (makeParams->flags & IMESH_MK_TUBE) != 0);
  if (tube)
    cap = (makeParams->flags & IMESH_MK_CAP_TUBE) != 0;
  else
    cap = makeParams->cap != IMESH_CAP_OFF;
  diaSetChecked(wMakeChecks[MAKE_MESH_CAP], cap); 
  diaSetChecked(wMakeChecks[MAKE_MESH_DOME], tube && 
                (makeParams->flags & IMESH_MK_CAP_DOME));
  wMakeDoButton->setEnabled(!scat && Imodv->objNum < Imodv->imod->objsize);
  wMakeChecks[MAKE_MESH_SKIP]->setEnabled(!tube && !scat);
  wMakeChecks[MAKE_MESH_TUBE]->setEnabled(iobjOpen(obj->flags) && !scat);
  wMakeChecks[MAKE_MESH_LOW]->setEnabled(!scat);
  wMakeChecks[MAKE_MESH_SURF]->setEnabled(!tube && !scat);
  wMakeChecks[MAKE_MESH_CAP]->setEnabled(!scat);
  wMakeChecks[MAKE_MESH_DOME]->setEnabled(tube && !scat);
  wMakePassSpin->setEnabled(!tube && !scat);
  wMakeDiamSpin->setEnabled(tube && !scat);
  wMakeTolSpin->setEnabled(!tube && !scat);
  wMakeZincSpin->setEnabled(!tube && !scat);
  wMakeFlatSpin->setEnabled(!tube && !scat);
  if (makeParams->minz != DEFAULT_VALUE && makeParams->maxz != DEFAULT_VALUE) {
    if (makeParams->minz == makeParams->maxz - 1)
      str.sprintf("%d", makeParams->maxz);
    else
      str.sprintf("%d,%d", makeParams->minz + 1, makeParams->maxz + 1);
  }
  wMakeZedit->setText(str);
}

static const char *makeCheckTips[] = {
  "Connect contours across sections with no contours",
  "Render open contours as tubes",
  "Make this a low-resolution mesh; keep regular mesh",
  "Use surface numbers for connections", 
  "Cap all unconnected ends of object or ends of tubes",
  "Cap ends of tubes with hemispheres"};

static void mkMakeMesh_cb(int index)
{
  const char notAvailable[] = "Mesh object(s) - not available when data "
    "are loaded binned";
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = outerVBoxLayout(oef->control);
  QHBoxLayout *hLayout = diaHBoxLayout(layout1);

  imeshParamsDefault(&makeDefParams);

  wMakeChecks[MAKE_MESH_SKIP] = diaCheckBox("Skip", oef->control, hLayout);

  wMakePassSpin = (QSpinBox *)diaLabeledSpin(0, 1., 9999., 1., "Passes", 
                                             oef->control, hLayout);
  QObject::connect(wMakePassSpin, SIGNAL(valueChanged(int)), &imodvObjed, 
                   SLOT(makePassSlot(int)));
  wMakePassSpin->setToolTip("Set number of passes at increasing distances");

  hLayout = diaHBoxLayout(layout1);
  wMakeChecks[MAKE_MESH_TUBE] = diaCheckBox("Tube", oef->control, hLayout);
  wMakeDiamSpin = (QDoubleSpinBox *)diaLabeledSpin(1, -2., 100., 0.5, "Diam", 
                                                 oef->control, hLayout);
  QObject::connect(wMakeDiamSpin, SIGNAL(valueChanged(double)), &imodvObjed, 
                   SLOT(makeDiamSlot(double)));
  wMakeDiamSpin->setToolTip("Set diameter for tube contours, or 0 to use "
                "3D line width, -1 to use point size, -2 to use symbol size");

  hLayout = diaHBoxLayout(layout1);
  wMakeChecks[MAKE_MESH_LOW] = diaCheckBox("Low res", oef->control, hLayout);
  wMakeTolSpin = (QDoubleSpinBox *)diaLabeledSpin(2,0., 5.0, 0.05f, "Tol", 
                                                oef->control, hLayout);
  QObject::connect(wMakeTolSpin, SIGNAL(valueChanged(double)), &imodvObjed, 
                   SLOT(makeTolSlot(double)));
  wMakeTolSpin->setToolTip("Set tolerance for point reduction");

  hLayout = diaHBoxLayout(layout1);
  wMakeChecks[MAKE_MESH_SURF] = diaCheckBox("Surface", oef->control, hLayout);
  wMakeZincSpin = (QSpinBox *)diaLabeledSpin(0, 1., 10., 1., "Z inc", 
                                             oef->control, hLayout);
  QObject::connect(wMakeZincSpin, SIGNAL(valueChanged(int)), &imodvObjed, 
                   SLOT(makeZincSlot(int)));
  wMakeZincSpin->setToolTip("Set Z increment for coarser meshing");

  hLayout = diaHBoxLayout(layout1);
  wMakeChecks[MAKE_MESH_CAP] = diaCheckBox("Cap", oef->control, hLayout);
  wMakeFlatSpin = (QDoubleSpinBox *)diaLabeledSpin
    (1, 0., 10., 0.5, "Flat crit", oef->control, hLayout);
  QObject::connect(wMakeFlatSpin, SIGNAL(valueChanged(double)), &imodvObjed, 
                   SLOT(makeFlatSlot(double)));
  wMakeFlatSpin->setToolTip("Set criterion Z difference for analyzing for "
                "tilted contours");

  hLayout = diaHBoxLayout(layout1);
  wMakeChecks[MAKE_MESH_DOME] = diaCheckBox("Dome cap", oef->control, hLayout);
  hLayout->addStretch();
  diaLabel("Z", oef->control, hLayout);
  wMakeZedit = new ToolEdit(oef->control, 7);
  hLayout->addWidget(wMakeZedit);
  wMakeZedit->setToolTip("Enter starting,ending Z to mesh, or just starting "
                         "Z for one plane; leave blank to mesh all");
  wMakeZedit->setFocusPolicy(Qt::ClickFocus);
  QObject::connect(wMakeZedit, SIGNAL(editingFinished()), &imodvObjed,
                   SLOT(makeZeditSlot()));

  QSignalMapper *mapper = new QSignalMapper(oef->control);
  QObject::connect(mapper, SIGNAL(mapped(int)), &imodvObjed,
                   SLOT(makeStateSlot(int)));
  for (int i = 0; i < 6; i++) {
    wMakeChecks[i]->setToolTip(makeCheckTips[i]);
    mapper->setMapping(wMakeChecks[i], i);
    QObject::connect(wMakeChecks[i], SIGNAL(toggled(bool)), mapper,
                     SLOT(map()));
  }

  diaSetChecked(wMakeChecks[MAKE_MESH_LOW], makeLowRes != 0);

  //QGridLayout *grid = new QGridLayout(layout1, 1, 2, 0);
  hLayout = diaHBoxLayout(layout1);
  hLayout->setSpacing(3);
  hLayout->setContentsMargins(0,0,0,0);

  wMakeDoButton = diaPushButton("Mesh One", oef->control, hLayout);
  //wMakeDoButton =  new QPushButton("Mesh One", oef->control);
  //grid->addWidget(wMakeDoButton, 0, 0);
  QObject::connect(wMakeDoButton, SIGNAL(clicked()), &imodvObjed, 
                   SLOT(makeDoitSlot()));

  wMakeAllButton = diaPushButton("Mesh All", oef->control, hLayout);
  //wMakeAllButton = new QPushButton("Mesh All", oef->control);
  //grid->addWidget(wMakeAllButton, 0, 1);
  QObject::connect(wMakeAllButton, SIGNAL(clicked()), &imodvObjed, 
                   SLOT(makeDoAllSlot()));
  if (!Imodv->standalone && (Imodv->vi->xybin * Imodv->vi->zbin > 1)) {
    wMakeDoButton->setEnabled(false);    
    wMakeAllButton->setEnabled(false);    
    wMakeDoButton->setToolTip(notAvailable);
    wMakeAllButton->setToolTip(notAvailable);
  } else {
    wMakeDoButton->setToolTip("Mesh the object with these parameters");
    wMakeAllButton->setToolTip("Mesh all objects with their respective"
                  " meshing parameters");
  }

  finalSpacer(oef->control, layout1);
  fixMakeMesh_cb();
  setMakeMesh_cb();
}

static void fixMakeMesh_cb() 
{
  diaSetButtonWidth(wMakeDoButton, ImodPrefs->getRoundedStyle(), 1.3,
                    "Mesh One");
  diaSetButtonWidth(wMakeAllButton, ImodPrefs->getRoundedStyle(), 1.3,
                    "Mesh All");
}


/* Variables for running the thread */
static int meshedObjNum;
static int meshedModNum;
static Iobj *meshDupObj;
static bool makeMeshBusy = false;
static bool meshThreadErr;

// The button to do all objects
void ImodvObjed::makeDoAllSlot()
{
  meshAllObjects = true;
  meshedObjNum = -1;
  meshedModNum = Imodv->curMod;

  // This should keep going unless there is an error or thread used to run mesh
  while (!startMeshingNext()) {};
}

// Find next non-scattered object with contours and start meshing it
// Return -1 for error, -2 no more objs, 1 if started thread, or 0 if finished
// an object synchronously
int ImodvObjed::startMeshingNext()
{
  Iobj *obj;
  while (++meshedObjNum < Imodv->mod[meshedModNum]->objsize) {
    obj = &Imodv->mod[meshedModNum]->obj[meshedObjNum];
    if (obj->contsize && !iobjScat(obj->flags))
      return meshOneObject(obj);
  }
  return -2;
}

// The button to do the current object
void ImodvObjed::makeDoitSlot()
{
  Iobj *obj = objedObject();
  if (!obj) 
    return;
  if (!obj->contsize)
    return;
  meshAllObjects = false;
  meshedObjNum = Imodv->objNum;
  meshedModNum = Imodv->curMod;
  meshOneObject(obj);
}

// Common function to start meshing an object
int ImodvObjed::meshOneObject(Iobj *obj)
{
  QString str;

  // Copy all the contours, save the object and model number
  meshDupObj = imeshDupMarkedConts(obj, 0);
  if (meshDupObj) {
    if (obj->meshParam)
      meshDupObj->meshParam = imeshParamsDup(obj->meshParam);
    else
      meshDupObj->meshParam = imeshParamsDup(&makeDefParams);
  }
  if (!meshDupObj || !meshDupObj->meshParam) {
    dia_err("Failed to copy the contours or data needed for meshing.");
    if (meshDupObj)
      imodObjectDelete(meshDupObj);
    meshDupObj = NULL;
    return -1;
  }
  if (objed_dialog)
    objed_dialog->updateMeshing(meshedObjNum);

#ifdef QT_THREAD_SUPPORT

  // If running in a thread, set flag, disable make button
  // start timer and start thread
  makeMeshBusy = true;
  wMakeDoButton->setEnabled(false);
  wMakeAllButton->setEnabled(false);
  if (!Imodv->standalone)
    ImodInfoWin->manageMenus();
  mTimerID = startTimer(50);
  mMeshThread = new MeshThread;

  // Priorities not available in Qt 3.1
#if QT_VERSION >= 0x030200
  mMeshThread->start(QThread::LowPriority);
#else
  mMeshThread->start();
#endif
  return 1;
#else

  // Otherwise just start the process directly and do finishing tasks
  meshObject();
  return(finishMesh());
#endif
}

void ImodvObjed::timerEvent(QTimerEvent *e)
{
#ifdef QT_THREAD_SUPPORT
  if (mMeshThread->isRunning())
    return;
  killTimer(mTimerID);
  if (objed_dialog) {
    wMakeDoButton->setEnabled(true);
    wMakeAllButton->setEnabled(true);
  }
  delete mMeshThread;
  makeMeshBusy = false;
  finishMesh();
  if (!Imodv->standalone)
    ImodInfoWin->manageMenus();
#endif
}

// Meshing routine that can be run from thread or directly
static void meshObject()
{
  Imod *imod = Imodv->imod;
  Ipoint scale;

  meshDupObj->meshParam->flags |= IMESH_MK_IS_COPY | IMESH_MK_NO_WARN;

  scale.x = imod->xscale;
  scale.y = imod->yscale;
  scale.z = imod->zscale;

  if (analyzePrepSkinObj(meshDupObj, makeLowRes, &scale, NULL)) {
    meshThreadErr = 1;
    return;
  }

  // Set the resolution flag
  for (int m = 0; m < meshDupObj->meshsize; m++)
    meshDupObj->mesh[m].flag |= (makeLowRes << IMESH_FLAG_RES_SHIFT);

  meshThreadErr = 0;
}

// Finish up after a mesh is computed
static int finishMesh()
{
  Iobj *obj;
  QString str;
  int retval = 0;
  int resol = makeLowRes ? 1 : 0;
  if (objed_dialog)
    objed_dialog->updateMeshing(-1);

  // If error, just clean up dup object
  if (meshThreadErr) {
    str = QString("An error occurred meshing the object:\n") + 
      QString(b3dGetError());
    dia_err(LATIN1(str));
    retval = -1;
  } else if (meshedModNum >= Imodv->numMods || 
      meshedObjNum >= Imodv->mod[meshedModNum]->objsize) {
    dia_err("The model changed in a way that prevents the mesh from being "
            "used - try again.");
    retval = -1;
  } else {

    // Clear out this resolution in the existing mesh and transfer new one
    obj = &Imodv->mod[meshedModNum]->obj[meshedObjNum];
    vbCleanupVBD(obj);
    if (obj->meshsize)
      imodMeshesDeleteRes(&obj->mesh, &obj->meshsize, resol);
    for (int m = 0; m < meshDupObj->meshsize; m++) {
      obj->mesh = imodel_mesh_add(&meshDupObj->mesh[m], obj->mesh, 
                                &obj->meshsize);
      
      // In case of error abandon both meshes, i.e. let it leak
      if (!obj->mesh) {
        dia_err("An error occurred transferring new mesh to object.");
        obj->meshsize = 0;
        retval = -1;
        break;
      }
    }

    // Switch between low and high res mode if it is still current model
    meshDupObj->meshsize = 0;
    meshDupObj->mesh = NULL;
    if (meshedModNum == Imodv->curMod && Imodv->lowres != resol && !ImodvClosed)
      imodvViewMenu(VVIEW_MENU_LOWRES);
 
    // Turn on mesh view same way as it is done from draw data selection
    onTestFlags = 0;
    offTestFlags = IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_NOLINE |
      IMOD_OBJFLAG_FILL | IMOD_OBJFLAG_OFF;
    passSetFlags = IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_NOLINE |
      IMOD_OBJFLAG_FILL;
    passClearFlags = 0;
    failSetFlags = IMOD_OBJFLAG_MESH;
    failClearFlags = IMOD_OBJFLAG_OFF;
    int turnon = iobjOff(obj->flags);
    optionSetFlags(&obj->flags);

    // Transfer points off flag
    if (meshDupObj->flags & IMOD_OBJFLAG_PNT_NOMODV)
      obj->flags |= IMOD_OBJFLAG_PNT_NOMODV;
    else 
      obj->flags &= ~IMOD_OBJFLAG_PNT_NOMODV;

    // Update various things
    if (meshedModNum == Imodv->curMod) {
      if (meshedObjNum == Imodv->objNum && objed_dialog)
        objset(Imodv);
      if (turnon) {
        if (meshedObjNum < numOnoffButtons)
          diaSetChecked(OnoffButtons[meshedObjNum], true);
        imodvOlistSetChecked(Imodv, meshedObjNum, true);
        imodvDrawImodImages();
      }
    }
    if (!ImodvClosed)
      imodvDraw(Imodv);
  }

  // Clean up the duplicated object
  imodObjectDelete(meshDupObj);
#ifdef QT_THREAD_SUPPORT
  if (!retval && meshAllObjects && objed_dialog)
    imodvObjed.startMeshingNext();
#endif
  return retval;
}


bool meshingBusy(void)
{
#ifdef QT_THREAD_SUPPORT
  return makeMeshBusy;
#else
  return false;
#endif
}

#ifdef QT_THREAD_SUPPORT
void MeshThread::run()
{
  meshObject();
}
#endif


/*****************************************************************************
 * END OF EDIT FIELDS.
 *****************************************************************************/

/*****************************************************************************/
/************************* internal utility functions. ***********************/
/*****************************************************************************/

Iobj *objedObject(void) 
{
  // Make sure the object number is legal and refresh the object address
  int num = numEditableObjects(Imodv->curMod);
  if (Imodv->objNum > (num - 1))
    Imodv->objNum = B3DMAX(0, num - 1);
  Imodv->obj = editableObject(Imodv->curMod, Imodv->objNum);
  return Imodv->obj;
}

// Return the number of editable objects for a model, including editable extra 
// objects for the current model
static int numEditableObjects(int model)
{
  Iobj *obj;
  int i, num = Imodv->imod->objsize;
  if (model != Imodv->curMod || Imodv->standalone)
    return (Imodv->mod[model]->objsize);
  for (i = 0; i < Imodv->vi->numExtraObj; i++) {
    obj = ivwGetAnExtraObject(Imodv->vi, i);
    if (obj && (obj->flags & IMOD_OBJFLAG_EXTRA_EDIT))
      num++;
  }
  return num;
}

// Return "editable" object ob for a model; editable extra objects count for
// current model
static Iobj *editableObject(int model, int ob)
{
  Iobj *obj;
  int i, num = Imodv->imod->objsize;
  if (model != Imodv->curMod || Imodv->standalone || ob < num) {
    if (ob < 0 || ob >= Imodv->mod[model]->objsize)
      return NULL;
    return &Imodv->mod[model]->obj[ob];
  }
  for (i = 0; i < Imodv->vi->numExtraObj; i++) {
    obj = ivwGetAnExtraObject(Imodv->vi, i);
    if (obj && (obj->flags & IMOD_OBJFLAG_EXTRA_EDIT)) {
      if (num == ob)
        return obj;
      num++;
    }
  }
  return NULL;
}

static void setObjFlag(int flag, int state, int types)
{
  int m, mst, mnd, ob;
  b3dUInt32 obflags;
  Iobj *obj;

  if (!Imodv->imod || !objedObject())
    return;

  setStartEndModel(mst, mnd);
    
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < numEditableObjects(m); ob++) {
      obj = editableObject(m, ob);
      obflags = obj->flags;
      if (changeModelObject(m, ob) && 
          (!types || ((types & OBJTYPE_CLOSED) && iobjClose(obflags)) ||
           ((types & OBJTYPE_OPEN) && iobjOpen(obflags)) ||
           ((types & OBJTYPE_SCAT) && iobjScat(obflags)))) {
        imodvRegisterObjectChg(ob);
        setOrClearFlags(&obj->flags, flag, state);
      }
    }
  }
}

// Sets a flag in byte 2 or 3 (depending on index) of mat3, 
static void setMat3Flag(int flag, int index, int state)
{
  unsigned char *ub;
  int m, mst, mnd, ob;
  Iobj *obj;

  if (!Imodv->imod || !objedObject())
    return;

  setStartEndModel(mst, mnd);
    
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < numEditableObjects(m); ob++)
      if (changeModelObject(m, ob)) {
        obj = editableObject(m, ob);
        imodvRegisterObjectChg(ob);
        ub = (unsigned char *)&(obj->matflags2);
        if (state)
          ub[index] |= (unsigned char)flag;
        else
          ub[index] &= (unsigned char)(~flag);
      }
  }
}

// Finish change unit and draw model after changing a flag
static void finishChangeAndDraw(int doObjset, int drawImages)
{
  if (doObjset && objed_dialog)
    objset(Imodv);
  imodvFinishChgUnit();
  imodvDraw(Imodv);
  if (drawImages)
    imodvDrawImodImages();
}

/* Maintain Imodv->objNum and set the starting and ending model to change based 
   on global flags and possible flag for individual entity*/
static void setStartEndModel(int &mst, int &mnd, bool multipleOK)
{
  int num = numEditableObjects(Imodv->curMod);
  if (Imodv->objNum > (num - 1))
    Imodv->objNum = num - 1;

  mst = 0;
  mnd = Imodv->numMods - 1;
  if (!(Imodv->crosset && multipleOK && 
        !(Imodv_objed_all == editGroup && imodvOlistGrouping())))
    mst = mnd = Imodv->curMod;
}

/* test whether the object ob should be changed in model m based on global
   flags and possible flag for individual entity */
static bool changeModelObject(int m, int ob, bool multipleOK)
{
  bool grouping = Imodv_objed_all == editGroup && imodvOlistGrouping() &&
    multipleOK;
  bool retval = (ob == Imodv->objNum && !grouping && (Imodv_objed_all != editOns ||
                                                  !multipleOK)) ||
    (grouping && imodvOlistObjInGroup(Imodv, ob)) ||
    (multipleOK && Imodv_objed_all == editAll) || 
    (multipleOK && Imodv_objed_all == editOns &&
     !iobjOff(Imodv->mod[m]->obj[ob].flags));

  // If it is the current object and it is not being edited, then set flag for
  // objset to switch to a new object
  if (!retval && ob == Imodv->objNum && m == Imodv->curMod)
    switchObjInObjset = true;
  return retval;
}

/* Central test for whether to draw after a slider change */
static bool drawOnSliderChange(bool dragging)
{
  return(!dragging || ImodPrefs->hotSliderActive(ctrlPressed));
}

/* Utility for adding final spacer.  It used to be a QVBox but this allows
   even the longest panel to have one without making dialog bigger */
static void finalSpacer(QWidget *parent, QVBoxLayout *layout)
{
  layout->addStretch();
}

static QVBoxLayout *outerVBoxLayout(QWidget *parent)
{
  QVBoxLayout *layout1 = new QVBoxLayout(parent);
  layout1->setContentsMargins(FIELD_MARGIN, FIELD_MARGIN, FIELD_MARGIN, 
                              FIELD_MARGIN);
  layout1->setSpacing(FIELD_SPACING);
  return layout1;
}
