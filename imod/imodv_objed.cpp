/*  IMOD VERSION 2.50
 *
 *  imodv_objed.c -- The object edit and object list dialogs for imodv
 *                   The main form class is imodvObjedForm in formv_objed.cpp
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

#include <sys/types.h>
#include <math.h>
#include <qlabel.h>
#include <qcheckbox.h>
#include <qpushbutton.h>
#include <qradiobutton.h>
#include <qlayout.h>
#include <qvbox.h>
#include <qframe.h>
#include <qsignalmapper.h>
#include "tooledit.h"
#include "formv_objed.h"
#include "multislider.h"
#include "dia_qtutils.h"

#include "imodv.h"
#include "imodP.h"
#include "imodv_gfx.h"
#include "imodv_light.h"
#include "imodv_objed.h"
#include "imodv_input.h"
#include "hotslider.h"
#include "control.h"

/*
 *  internal prototypes (first two were public but unused)
 */

/*  returns the current object being edited by objed. */
static Iobj *objedObject(void) { return(Imodv->obj); }

/* returns the current model being used for object editing. */
static Imod *objedModel(void) { return(Imodv->imod); }

static void objset(ImodvApp *a);
static void setObjFlag(long flag, int state);
static void setOnoffButtons(void);
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
static void mkMove_cb(int index);
static void mkSubsets_cb(int index);
static void optionSetFlags (b3dUInt32 *flag);
static void toggleObj(int ob, bool state);


/* resident instance of the IModvObjed class, and pointers to the dialog
   box classes when they are created */
static ImodvObjed imodvObjed;
static imodvObjedForm *objed_dialog = NULL;
static ImodvOlist *Oolist_dialog = NULL;

static int Imodv_objed_all = 0;  /* edit all objects if 1 */
static int ctrlPressed = false;

#define editAll  1

#define stylePoints 0
#define styleLines  1
#define styleFill   2
#define styleFillOutline 3
#define FIELD_MARGIN 3          // margin and spacing for layouts inside 
#define FIELD_SPACING 6         // the edit fields


/* Defines and variables for the On-off buttons in this dislaog and in the
   object list dialog */
#define MAX_ONOFF_BUTTONS  48
#define MAX_ONOFF_COLUMNS   6
static QCheckBox *OnoffButtons[MAX_ONOFF_BUTTONS];
static int numOnoffButtons = 0;

#define MAX_OOLIST_BUTTONS  256
#define MAX_LIST_IN_COL 36
#define MAX_LIST_NAME 48
static int oolist_name_limits[10] = {40, 25, 17, 13, 10, 8, 7, 6, 6, 6};
static QCheckBox *OolistButtons[MAX_OOLIST_BUTTONS];
static int numOolistButtons = 0;
static int olistNcol = 1;

static int      CurrentObjectField       = 0;
ObjectEditField objectEditFieldData[]    = {
  {"Line Color", mkLineColor_cb, setLineColor_cb, NULL},
  {"Fill Color", mkFillColor_cb, setFillColor_cb, NULL},
  {"Points",     mkPoints_cb,    setPoints_cb,    NULL},
  {"Lines",      mkLines_cb,     setLines_cb,     NULL},
  {"Mesh View",  mkScalar_cb,    setScalar_cb,    NULL},
  {"Clip",       mkClip_cb,      setClip_cb,      NULL},
  {"Material",   mkMaterial_cb,  setMaterial_cb,  NULL},
  {"Move",       mkMove_cb,      NULL,            NULL},
  {"Subsets",    mkSubsets_cb,   NULL,            NULL},

  NULL,
};

/* Constructor for resident class */
ImodvObjed::ImodvObjed(QObject *parent, const char *name)
  : QObject(parent, name)
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
  if (( (*flag & onTestFlags) || !onTestFlags) && 
      ( !(*flag & offTestFlags) || !offTestFlags)) {
    *flag = (*flag | passSetFlags) & ~passClearFlags;
  }else {
    *flag = (*flag | failSetFlags) & ~failClearFlags;
  }
}          

void imodvObjedDrawData(int option)
{
  int ob, m;
     
  switch(option){
  case 0:
    setObjFlag(IMOD_OBJFLAG_OFF, 1);
    break;

  case 1:
  case 2:
    if (option == 1) {
      /* If going to line, see if MESH and LINE and FILL are all on
         and OFF is not; if so then clear all of these flags;
         otherwise just clear the MESH and OFF flags */
      onTestFlags = IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_LINE | 
        IMOD_OBJFLAG_FILL;
      offTestFlags = IMOD_OBJFLAG_OFF;
      passSetFlags = 0;
      passClearFlags = onTestFlags;
      failSetFlags = 0;
      failClearFlags = IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_OFF;
    } else {
      /* If going to mesh, see if not OFF or MESH or LINE or FILL:
         if so set MESH and LINE and FILL; otherwise just set MESH and
         clear OFF flags */
      onTestFlags = 0;
      offTestFlags = IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_LINE |
        IMOD_OBJFLAG_FILL | IMOD_OBJFLAG_OFF;
      passSetFlags = IMOD_OBJFLAG_MESH | IMOD_OBJFLAG_LINE |
        IMOD_OBJFLAG_FILL;
      passClearFlags = 0;
      failSetFlags = IMOD_OBJFLAG_MESH;
      failClearFlags = IMOD_OBJFLAG_OFF;
    }

    if (!Imodv->imod) return;

    if (Imodv_objed_all){
      if (Imodv->crosset){
        for(m = 0; m < Imodv->nm; m++)
          for(ob = 0; ob < Imodv->mod[m]->objsize; ob++)
            if (Imodv_objed_all == editAll ||
                !(Imodv->mod[m]->obj[ob].flags & 
                  IMOD_OBJFLAG_OFF))
              optionSetFlags(&Imodv->mod[m]->obj[ob].flags);
      }else{
        for(ob = 0; ob < Imodv->imod->objsize; ob++)
          if (Imodv_objed_all == editAll ||
              !(Imodv->imod->obj[ob].flags & IMOD_OBJFLAG_OFF))
            optionSetFlags(&Imodv->imod->obj[ob].flags);
      }     
            
    }else{
      if (Imodv->crosset){
        for(m = 0; m < Imodv->nm; m++) {
          ob = Imodv->ob;
          if (ob < Imodv->mod[m]->objsize)
            optionSetFlags(&Imodv->mod[m]->obj[ob].flags);
        }
      }else
        optionSetFlags(&Imodv->imod->obj[Imodv->ob].flags);
            
    }
    objset(Imodv);
    break;

  default:
    break;
  }
  setOnoffButtons();
  imodvDraw(Imodv);

}


/******************************************************************
 * Style option
 */
void imodvObjedStyleData(int option)
{
  switch(option){
  case stylePoints:
    setObjFlag(IMOD_OBJFLAG_LINE , 1);
    setObjFlag(IMOD_OBJFLAG_FILL, 0);
    break;
  case styleLines:
    setObjFlag(IMOD_OBJFLAG_FILL | IMOD_OBJFLAG_LINE, 0);
    break;
  case styleFill:
    setObjFlag(IMOD_OBJFLAG_FILL | IMOD_OBJFLAG_LINE, 1);
    break;
  case styleFillOutline:
    setObjFlag(IMOD_OBJFLAG_LINE, 0);
    setObjFlag(IMOD_OBJFLAG_FILL, 1);
    break;
  default:
    break;
  }
  imodvDraw(Imodv);
}

// Edit Each/All option
void imodvObjedEditData(int option)
{
  Imodv_objed_all = option;
}

// User selects a new object through spin box or slider
void imodvObjedSelect(int which)
{
  Imodv->ob = which - 1;
  objset(Imodv);
}

// A name is changed
void imodvObjedName(const char *name)
{
  int i, mi;
  Iobj *obj = objedObject();
   if (!obj) return;
      
  mi = strlen(name);
  if (mi >= IOBJ_STRSIZE)
    mi = IOBJ_STRSIZE - 1;
  for(i = 0 ; i<mi; i++)
    obj->name[i] = name[i];
  obj->name[i] = 0x00;
}

// Respond to an On-Off button in either the objed or the object list window
void ImodvObjed::toggleObjSlot(int ob)
{
  toggleObj(ob, OnoffButtons[ob]->isOn());
}

void ImodvObjed::toggleListSlot(int ob)
{
  toggleObj(ob, OolistButtons[ob]->isOn());
}

static void toggleObj(int ob, bool state)
{
  int  m;
   if (state) {
    Imodv->imod->obj[ob].flags &= ~IMOD_OBJFLAG_OFF;
    Imodv->ob = ob;
  } else
    Imodv->imod->obj[ob].flags |= IMOD_OBJFLAG_OFF;

  /* Turn off same object in all other models if editing all and legal ob */
  if (Imodv->crosset)
    for (m = 0; m < Imodv->nm; m++)
      if (Imodv->mod[m]->objsize > ob)
        if (state)
          Imodv->mod[m]->obj[ob].flags &= ~IMOD_OBJFLAG_OFF;
        else
          Imodv->mod[m]->obj[ob].flags |= IMOD_OBJFLAG_OFF;

  /* If the object is within legal limits for a button list, set that button
     in each list */
  if (ob < numOnoffButtons && ob < Imodv->imod->objsize)
    diaSetChecked(OnoffButtons[ob], state);
  if (ob < numOolistButtons && ob < Imodv->imod->objsize)
    diaSetChecked(OolistButtons[ob], state);

  if (objed_dialog)
    objset(Imodv);
  imodvDraw(Imodv);
}

// User selected a new frame; update the frame
void imodvObjedFramePicked(int item)
{
  CurrentObjectField = item;
  if (objectEditFieldData[CurrentObjectField].setwidget)
    objectEditFieldData[CurrentObjectField].setwidget();
}

// Done or Escape is pressed
void imodvObjedDone()
{
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
  dia_vasmsg
    ("-----------------\n",
     "Object Edit Help.\n",
     "-----------------\n",
     "\tSelect the current object with the Object # arrows or the "
     "slider.  "
     "The Each/All/On's option menu allows one to edit just the current "
     "object, all objects at once, or just the objects that are "
     "currently turned On.  Features that can be edited in tandem for "
     "many objects include: point size, line width, material "
     "properties, and drawing data type and style."
     "\n\n"
     "The name of each object can be edited using the text edit box."
     "\n\n",
     "The numbered check boxes allow one to conveniently turn selected "
     "objects on and off.  When an object is turned on, it becomes the "
     "current object.\n\n",
     "The Draw option selects which type of data to draw. "
     "You can select to draw contour data, mesh data or no data at all."
     "\n",
     "The Style option selects the drawing style. "
     "You can select between drawing points, lines, filled or "
     "filled outline."
     "\n\n",

     "~~~~~~~~~~~~~~~~~~~~~\n"
     "The Edit control box:\n"
     "~~~~~~~~~~~~~~~~~~~~~\n\n",
     "Several different parameters can be edited using the edit "
     "control box.  "
     "The parameter group can be selected by using the left "
     "mouse button on top of the text list in the bottom left corner.  "
     "The larger box in the lower right corner will contain the "
     "controls needed for editing each parameter group.  "
     "The controls in each group are listed below:\n\n\n",

     "\tLine color: Use Red, Green, and Blue sliders to adjust the "
     "foreground line color of the current object.  This also sets the "
     "fill color unless a separate color is selected in the \"Fill "
     "Color\" panel.  The Transparency slider selects object "
     "transparency.  Transparency is only an approximation and can "
     "easily generate artifacts.  To minimize these artifacts, the back "
     "face of the object will not be displayed unless the "
     "\"Light Both Sides\" button is selected in the \"Fill "
     "Color\" panel.  Try it both ways to see which looks best.\n\n",

     "\tFill color: Adjust the fill color of the current object.  "
     "If \"Use Fill Color\" is not selected the line color settings "
     "are used instead.  Select \"Light Both Sides\" to have the object "
     "lit on both its outside and inside surfaces.\n\n",

     "\tPoints: Adjust scattered point rendering.  "
     "The size of scattered points can be entered into the text box."
     "\n\n",
           
     "\tLines: The Width slider changes the line width used to "
     "draw the objects.  The \"Anti-Alias Rendering\" toggle "
     "selects anti-alias rendering for lines.  "
     "Lines will look smoother with this option on; however, "
     "some artifacts may be noticed."
     "\n\n",

     "\tMesh View: This is a special control group used for "
     "viewing meshes that have scalar data, such as to represent "
     "surface density.  The feature is enabled by selecting \"Turn "
     "Normal Magnitide On\".  The sliders adjust the contrast range "
     "of the displayed values, and \"False Color\" will display the "
     "values with a false color rather than gray scale intensity "
     "ramp.\n\n",

     "\tClip: Toggles clip planes on/off for each object.  The "
     "\"Reset\" button moves the plane back to its default location "
     "and orientation through the middle of the object.  The \"Invert\""
     " button inverts the direction  of the clipping plane.\n\n"


     "\tMaterial: "
     "The Ambient slider adjusts ambient, or non-directional, light "
     "hitting the object.  "
     "The Diffuse slider adjusts light hitting the object from the "
     "light source, which then diffuses in a direction-dependent way.  "
     "The Shininess and Specularity sliders together adjust the "
     "shininess or highlights of the object."
     "\n\n",

     "\tMove: Allows one easily to view orthogonal faces of the model.  "
     "Each column of buttons will move by 90 degree rotations about "
     "one of the three axes.  "
     "Right now only center on current object is supported."
     "\n\n",

     "\tSubsets: Allows one to view the current object, surface or "
     "contour of the model, if one is selected.  \"Current Surface "
     "Only\" or \"Current Contour Only\" will show only the current "
     "surface or contour in the current object.  \"Surface & Other "
     "Objects\" or \"Contour & Other Objects\" will show the current "
     "surface or contour in the current object, plus all other objects "
     "that are turned on."
     "\n\n",

     NULL);
  return;
}


/****************************************************************************/
/* Object Edit update functions.                                            */
/****************************************************************************/

/* sets object features in objed window. */
static void objset(ImodvApp *a)
{
  int style, type;
  Iobj *obj;
  unsigned int flag;

  // Adjust object number and set structure variables
  if (a->ob >= a->imod->objsize)
    a->ob = 0;
  a->obj = &(a->imod->obj[a->ob]);
  obj = a->obj;

  flag = obj->flags;

  // Find the object style number
  if ( iobjFill(flag) ){
    style = styleFillOutline;
    if (flag & IMOD_OBJFLAG_LINE)
      style = styleFill;
  }else{
    if (IMOD_OBJFLAG_LINE & flag)
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

  objed_dialog->updateObject(a->ob + 1, a->imod->objsize, type, style, 
                             QColor((int)(255 * obj->red), 
                                    (int)(255 * obj->green),
                                    (int)(255 * obj->blue)), obj->name);

  if (objectEditFieldData[CurrentObjectField].setwidget)
    objectEditFieldData[CurrentObjectField].setwidget();
}

// Update the state of all On-Off buttons
static void setOnoffButtons(void)
{
  int ob;
  bool state;
  QString qstr;
  char obname[MAX_LIST_NAME];
  int len;
  ImodvApp *a = Imodv;
 

  for (ob = 0; ob < numOnoffButtons; ob++) {
    if (ob < a->imod->objsize)
      state = !(a->imod->obj[ob].flags & IMOD_OBJFLAG_OFF);
    else
      state = false;
    OnoffButtons[ob]->setEnabled(ob < a->imod->objsize);
    diaSetChecked(OnoffButtons[ob], state);
  }

  for (ob = 0; ob < numOolistButtons; ob++) {
    if (ob < a->imod->objsize) {
      // Get a truncated name
      len = strlen(a->imod->obj[ob].name);
      if (len > oolist_name_limits[olistNcol - 1])
        len = oolist_name_limits[olistNcol - 1];
      strncpy(obname, a->imod->obj[ob].name, len);
      obname[len] = 0x00;
      qstr.sprintf("%d: %s",ob + 1, obname);
      OolistButtons[ob]->setText(qstr);
      state = !(a->imod->obj[ob].flags & IMOD_OBJFLAG_OFF);
    } else
      state = false;
    OolistButtons[ob]->setEnabled(ob < a->imod->objsize);
    diaSetChecked(OolistButtons[ob], state);
  }
}


/*****************************************************************************/
/**                CREATION AND EXTERNALLY CALLED FUNCTIONS                 **/
/*****************************************************************************/

/* DNM 2/7/01: call setOnoffButtons before skipping out if no window; it should
   be safe even if neither list or objed window is open */
void imodvObjedNewView(void)
{
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
  QString qstr;
  char *window_name;
  if (objed_dialog) {
    objed_dialog->raise();
    return;
  }

  if (!a->imod)
    return;

  objed_dialog = new imodvObjedForm(NULL, NULL, //false,
                                    Qt::WDestructiveClose | Qt::WType_TopLevel);

  Imodv_objed_all = 0;  // May want to retain this setting

  window_name = imodwEithername("Imodv Objects: ", a->imod->fileName, 1);
  if (window_name) {
    qstr = window_name;
    free(window_name);
  }
  if (!qstr.isEmpty())
      objed_dialog->setCaption(qstr);

  ctrlPressed = false;
  objed_dialog->setCurrentFrame(CurrentObjectField, Imodv_objed_all);
  objset(a);
  imodvDialogManager.add((QWidget *)objed_dialog, IMODV_DIALOG);
  objed_dialog->show();
}

// This is called by the form class to put On/Off buttons in a frame
void imodvObjedMakeOnOffs(QFrame *frame)
{
  ImodvApp *a = Imodv;
  int ob, m;
  QString str;

  QGridLayout *grid = new QGridLayout(frame, 1, MAX_ONOFF_COLUMNS, 2, 0,
                                      "onoff grid");
  QSignalMapper *mapper = new QSignalMapper(frame);
  QObject::connect(mapper, SIGNAL(mapped(int)), &imodvObjed, 
                   SLOT(toggleObjSlot(int)));

  // Make maximum number of buttons needed for all loaded models
  for (m = 0; m < a->nm; m++)
    if (numOnoffButtons < a->mod[m]->objsize) 
      numOnoffButtons = a->mod[m]->objsize; 

  if (numOnoffButtons > MAX_ONOFF_BUTTONS)
    numOnoffButtons = MAX_ONOFF_BUTTONS;
  
  // Make the buttons, add them to the grid, and connect them to the mapper
  for (ob = 0; ob < numOnoffButtons; ob++) {
    str.sprintf("%d",ob + 1);
    OnoffButtons[ob] = new QCheckBox(str, frame);
    OnoffButtons[ob]->setFocusPolicy(QWidget::NoFocus);
    grid->addWidget(OnoffButtons[ob], ob / MAX_ONOFF_COLUMNS, 
                    ob % MAX_ONOFF_COLUMNS);
    mapper->setMapping(OnoffButtons[ob], ob);
    QObject::connect(OnoffButtons[ob], SIGNAL(toggled(bool)), mapper,
                     SLOT(map()));
  }
  setOnoffButtons();
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
static char *rgbTitles[] = {"Red", "Green", "Blue", "Transparency"};
static MultiSlider *lineSliders;


void ImodvObjed::lineColorSlot(int color, int value, bool dragging)
{
  Iobj *obj = Imodv->obj;
  
  switch(color){
  case 0:
    obj->red = value / 255.0;
    break;
  case 1:
    obj->green = value / 255.0;
    break;
  case 2:
    obj->blue = value / 255.0;
    break;
  case 3:
    obj->trans = value;
    if (Imodv->crosset)
      for(int m = 0; m < Imodv->nm; m++)
        if (Imodv->mod[m]->objsize > Imodv->ob)
          Imodv->mod[m]->obj[Imodv->ob].trans = value;
    break;
  }
  if (!dragging || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && ctrlPressed) ||
      (hotSliderFlag() == HOT_SLIDER_KEYUP && !ctrlPressed)) {
    objset(Imodv);
    imodvDraw(Imodv);
  } else if (color < 3)
    objed_dialog->updateColorBox(QColor((int)(255 * obj->red),
                                        (int)(255 * obj->green),
                                        (int)(255 * obj->blue)));
}

static void setLineColor_cb(void)
{
  Iobj *obj = objedObject();
  /* Set red, green and blue values. */
  lineSliders->setValue(0, (int)(obj->red * 255));
  lineSliders->setValue(1, (int)(obj->green * 255));
  lineSliders->setValue(2, (int)(obj->blue * 255));
  lineSliders->setValue(3, obj->trans);
}

static void mkLineColor_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = new QVBoxLayout(oef->control, FIELD_MARGIN, 
                                         FIELD_SPACING, "line color layout");
  lineSliders = new MultiSlider(oef->control, 4, rgbTitles);
  lineSliders->setRange(3, 0,100);    // Transparency
  layout1->addLayout(lineSliders->getLayout());
  finalSpacer(oef->control, layout1);

  QObject::connect(lineSliders, SIGNAL(sliderChanged(int, int, bool)), &imodvObjed,
          SLOT(lineColorSlot(int, int, bool)));
}


/*******************************************************************
 * The Fill Color Edit Field
 *******************************************************************/

static MultiSlider *fillSliders;
static QCheckBox *wFillToggle = 0;
static QCheckBox *wBothSides  = 0;

void ImodvObjed::fillToggleSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_FCOLOR, state ? 1 : 0);
  objset(Imodv);
  imodvDraw(Imodv);
}

void ImodvObjed::bothSidesSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_TWO_SIDE, state ? 1 : 0);
  objset(Imodv);
  imodvDraw(Imodv);
}

void ImodvObjed::fillColorSlot(int color, int value, bool dragging)
{
  Iobj *obj = objedObject();
  unsigned char *colors;

  if (!obj) return;
  colors = (unsigned char *)&(obj->mat1);
  colors[color] = value;

  if (!dragging || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && ctrlPressed) ||
      (hotSliderFlag() == HOT_SLIDER_KEYUP && !ctrlPressed)) {
    objset(Imodv);
    imodvDraw(Imodv);
  }
}

static void setFillColor_cb(void)
{
  Iobj *obj = objedObject();
  if (!obj) return;
  unsigned char *colors = (unsigned char *)&(obj->mat1);

  diaSetChecked(wFillToggle, obj->flags & IMOD_OBJFLAG_FCOLOR);
  diaSetChecked(wBothSides, obj->flags & IMOD_OBJFLAG_TWO_SIDE);
  for (int i = 0; i < 3; i++)
    fillSliders->setValue(i, colors[i]);
}

static void mkFillColor_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = new QVBoxLayout(oef->control, FIELD_MARGIN, 
                                         FIELD_SPACING, "fill color layout");
  fillSliders = new MultiSlider(oef->control, 3, rgbTitles);
  layout1->addLayout(fillSliders->getLayout());

  wFillToggle = diaCheckBox("Use Fill Color", oef->control, layout1);
  QObject::connect(wFillToggle, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(fillToggleSlot(bool)));

  wBothSides = diaCheckBox("Light Both Sides", oef->control, layout1);
  QObject::connect(wBothSides, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(bothSidesSlot(bool)));
  
  finalSpacer(oef->control, layout1);

  QObject::connect(fillSliders, SIGNAL(sliderChanged(int, int, bool)), &imodvObjed,
          SLOT(fillColorSlot(int, int, bool)));
}


/*******************************************************************
 * The Material Edit Field
 *******************************************************************/

static MultiSlider *matSliders;
static char *matTitles[] = {"Ambient", "Diffuse", "Specular", "Shininess"};

// Set the relevant object material property - including normal magnitude
// Black and white levels
static void setMaterial(Iobj *obj, int which, int value)
{
  switch (which) {
  case 0:
    obj->diffuse = (unsigned char)value;
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
    obj->mat3 = (unsigned char)value;
    break;
  case 5:
    obj->mat3b1 = (unsigned char)value;
    break;
  }
}

void ImodvObjed::materialSlot(int which, int value, bool dragging)
{
  int ob,m;

  if (!Imodv->imod) return;
     
  if (Imodv_objed_all){
    for(ob = 0; ob < Imodv->imod->objsize; ob++)
      if (Imodv_objed_all == editAll ||
          !(Imodv->imod->obj[ob].flags & IMOD_OBJFLAG_OFF))
        setMaterial(&(Imodv->imod->obj[ob]), which, value);

    if (Imodv->crosset)
      for(m = 0; m < Imodv->nm; m++)
        for(ob = 0; ob < Imodv->mod[m]->objsize; ob++)
          if (Imodv_objed_all == editAll ||
              !(Imodv->mod[m]->obj[ob].flags & IMOD_OBJFLAG_OFF))
            setMaterial(&(Imodv->mod[m]->obj[ob]), which, value);

  } else {
    setMaterial(Imodv->obj, which, value);
    if (Imodv->crosset)
      for(m = 0; m < Imodv->nm; m++)
        if (Imodv->mod[m]->objsize > Imodv->ob)
            setMaterial(&(Imodv->mod[m]->obj[ob]), which, value);

  }
  /*     printf("set mat %d, offset %d, value%d\n", *item, offset, cbs->value); */
     
  if (!dragging || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && ctrlPressed) ||
      (hotSliderFlag() == HOT_SLIDER_KEYUP && !ctrlPressed))
    imodvDraw(Imodv);
}

static void setMaterial_cb(void)
{
  Iobj *obj = objedObject();
  if (!obj) return;

  matSliders->setValue(0, (int)obj->ambient);
  matSliders->setValue(1, (int)obj->diffuse);
  matSliders->setValue(2, (int)obj->specular);
  matSliders->setValue(3, (int)obj->shininess);
}


static void mkMaterial_cb (int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = new QVBoxLayout(oef->control, FIELD_MARGIN, 
                                         FIELD_SPACING, "material layout");
  matSliders = new MultiSlider(oef->control, 4, matTitles);
  layout1->addLayout(matSliders->getLayout());
  finalSpacer(oef->control, layout1);

  QObject::connect(matSliders, SIGNAL(sliderChanged(int, int, bool)), &imodvObjed,
          SLOT(materialSlot(int, int, bool)));
}


/*****************************************************************************
 * The point edit field
 *****************************************************************************/
static ToolEdit *wPointEdit;

void ImodvObjed::pointSizeSlot()
{
  int i,m,ob;

  if (!Imodv->imod) return;

  // Possibly TODO: catch changes as they occur, recording object pointer;
  // then apply them when this signal comes in
  objed_dialog->setFocus();
  QString str = wPointEdit->text();
  i = atoi(str.latin1());
  if (i < 0)
    i = 0;

  if (Imodv->ob > (Imodv->imod->objsize - 1))
    Imodv->ob = Imodv->imod->objsize - 1;

     
  if (Imodv_objed_all){
    for(ob = 0; ob < Imodv->imod->objsize; ob++)
      if (Imodv_objed_all == editAll ||
          !(Imodv->imod->obj[ob].flags & IMOD_OBJFLAG_OFF))
        Imodv->imod->obj[ob].pdrawsize = i;
    if (Imodv->crosset)
      for(m = 0; m < Imodv->nm; m++)
        for(ob = 0; ob < Imodv->mod[m]->objsize; ob++)
          if (Imodv_objed_all == editAll ||
              !(Imodv->mod[m]->obj[ob].flags & 
                IMOD_OBJFLAG_OFF))
            Imodv->mod[m]->obj[ob].pdrawsize = i;
  } else {

    Imodv->imod->obj[Imodv->ob].pdrawsize = i;
    if (Imodv->crosset)
      for(m = 0; m < Imodv->nm; m++)
        if (Imodv->mod[m]->objsize > Imodv->ob)
          Imodv->mod[m]->obj[Imodv->ob].pdrawsize = i;
  }
  imodvDraw(Imodv);     
  return;
}

static void setPoints_cb(void)
{
  QString str;
  Iobj *obj = objedObject();
  if (!obj)
    return;
     
  str.sprintf("%d", obj->pdrawsize);
  wPointEdit->setText(str);
}

static void mkPoints_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = new QVBoxLayout(oef->control, FIELD_MARGIN, 
                                         FIELD_SPACING, "points layout");
  wPointEdit = new ToolEdit(oef->control, 6, "point edit");
  layout1->addWidget(wPointEdit);
  QObject::connect(wPointEdit, SIGNAL(returnPressed()), &imodvObjed,
          SLOT(pointSizeSlot()));
  QObject::connect(wPointEdit, SIGNAL(lostFocus()), &imodvObjed,
          SLOT(pointSizeSlot()));

  diaLabel("Scattered Point Size", oef->control, layout1);
  finalSpacer(oef->control, layout1);
}


/*****************************************************************************
 * The line edit field
 *****************************************************************************/

static MultiSlider *widthSlider;
static QCheckBox *wLineAlias;
static char *widthLabel[] = {"Line Width"};

void ImodvObjed::lineWidthSlot(int which, int value, bool dragging)
{
  int ob,m;
     
  if (!Imodv->imod)
    return;
     
  if (Imodv->ob > (Imodv->imod->objsize - 1))
    Imodv->ob = Imodv->imod->objsize - 1;
     
  if (Imodv_objed_all){
    for(ob = 0; ob < Imodv->imod->objsize; ob++)
      if (Imodv_objed_all == editAll ||
          !(Imodv->imod->obj[ob].flags & IMOD_OBJFLAG_OFF))
        Imodv->imod->obj[ob].linewidth = value;
    if (Imodv->crosset)
      for(m = 0; m < Imodv->nm; m++)
        for(ob = 0; ob < Imodv->mod[m]->objsize; ob++)
          if (Imodv_objed_all == editAll ||
              !(Imodv->mod[m]->obj[ob].flags & 
                IMOD_OBJFLAG_OFF))
            Imodv->mod[m]->obj[ob].linewidth = value;
  } else {

    Imodv->imod->obj[Imodv->ob].linewidth = value;
    if (Imodv->crosset)
      for(m = 0; m < Imodv->nm; m++)
        if (Imodv->mod[m]->objsize > Imodv->ob)
          Imodv->mod[m]->obj[Imodv->ob].linewidth = value;
  }

  if (!dragging || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && ctrlPressed) ||
      (hotSliderFlag() == HOT_SLIDER_KEYUP && !ctrlPressed))
    imodvDraw(Imodv);
}

void ImodvObjed::lineAliasSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_ANTI_ALIAS, state ? 1 : 0);
  objset(Imodv);
  imodvDraw(Imodv);
}


static void setLines_cb(void)
{
  Iobj *obj = objedObject();
  if (!obj) 
    return;
  widthSlider->setValue(0, obj->linewidth);
  diaSetChecked(wLineAlias, obj->flags & IMOD_OBJFLAG_ANTI_ALIAS );
}

static void mkLines_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = new QVBoxLayout(oef->control, FIELD_MARGIN, 
                                         FIELD_SPACING, "lines layout");
  widthSlider = new MultiSlider(oef->control, 1, widthLabel, 1, 10);
  layout1->addLayout(widthSlider->getLayout());
  QObject::connect(widthSlider, SIGNAL(sliderChanged(int, int, bool)), &imodvObjed,
          SLOT(lineWidthSlot(int, int, bool)));

  wLineAlias = diaCheckBox("Anti-alias rendering", oef->control, layout1);
  QObject::connect(wLineAlias, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(lineAliasSlot(bool)));

  layout1->addWidget(wLineAlias);

  finalSpacer(oef->control, layout1);
}


/*****************************************************************************
 * The scalar edit field
 *****************************************************************************/
static QCheckBox *wMeshNormal;
static QCheckBox *wMeshFalse;
static MultiSlider *meshSliders;

static char *bwLabels[] = {"Black Level", "White Level"};

void ImodvObjed::meshNormalSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_SCALAR, state ? 1 : 0);
  objset(Imodv);
  imodvDraw(Imodv);
}

void ImodvObjed::meshFalseSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_MCOLOR, state ? 1 : 0);
  objset(Imodv);
  imodvDraw(Imodv);
}

void ImodvObjed::meshLevelSlot(int which, int value, bool dragging)
{
  materialSlot(which + 4, value, dragging);
}

static void setScalar_cb(void)
{
  unsigned char *ub;
  Iobj *obj = objedObject();
  if (!obj) return;
  ub = (unsigned char *)&(obj->mat3);
  if (!ub[1])
    ub[1] = 255;

  diaSetChecked(wMeshNormal, IMOD_OBJFLAG_SCALAR & obj->flags);
  diaSetChecked(wMeshFalse, IMOD_OBJFLAG_MCOLOR & obj->flags);
     
  meshSliders->setValue(0, (int)ub[0]);
  meshSliders->setValue(1, (int)ub[1]);
}

static void mkScalar_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = new QVBoxLayout(oef->control, FIELD_MARGIN, 
                                         FIELD_SPACING, "mesh layout");
  wMeshNormal = diaCheckBox("Show normal magnitudes", oef->control, layout1);
  QObject::connect(wMeshNormal, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(meshNormalSlot(bool)));

  wMeshFalse = diaCheckBox("False color", oef->control, layout1);
  QObject::connect(wMeshFalse, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(meshFalseSlot(bool)));

  meshSliders = new MultiSlider(oef->control, 2, bwLabels);
  layout1->addLayout(meshSliders->getLayout());
  QObject::connect(meshSliders, SIGNAL(sliderChanged(int, int, bool)),
                   &imodvObjed, SLOT(meshLevelSlot(int, int, bool)));

  finalSpacer(oef->control, layout1);
}


/*****************************************************************************
 * The clip edit field
 *****************************************************************************/
static QCheckBox *wClipToggle;

void ImodvObjed::clipResetSlot()
{
  Iobj *obj = objedObject();     
  Ipoint min, max, mid;

  imodObjectGetBBox(obj, &min, &max);
     
  mid.x = (max.x + min.x) * -0.5f;
  mid.y = (max.y + min.y) * -0.5f;
  mid.z = (max.z + min.z) * -0.5f;
  obj->clip_point = mid;
  obj->clip_normal.x = obj->clip_normal.y = 0.0f;
  obj->clip_normal.z = -1.0f;
  imodvDraw(Imodv);
}

void ImodvObjed::clipInvertSlot()
{
  Iobj *obj = objedObject();     
  obj->clip_normal.x = -obj->clip_normal.x;
  obj->clip_normal.y = -obj->clip_normal.y;
  obj->clip_normal.z = -obj->clip_normal.z;
  imodvDraw(Imodv);
}

void ImodvObjed::clipToggleSlot(bool state)
{
  Iobj *obj = objedObject();     
  if (!obj)
    return;
     
  if (!state)
    obj->clip = 0;
  else {
    obj->clip = 1;
    /* DNM: if this is the first time it's been turned on, set to
       middle of object */
    if (obj->clip_point.x == 0.0 && obj->clip_point.y == 0.0 &&
        obj->clip_point.z == 0.0 && obj->clip_normal.x == 0.0 &&
        obj->clip_normal.y == 0.0 && obj->clip_normal.z == -1.0) {
      clipResetSlot();
      return;
    }
  }
  objset(Imodv);
  imodvDraw(Imodv);
}

static void setClip_cb(void)
{
  Iobj *obj = objedObject();
  if (!obj) return;

  diaSetChecked(wClipToggle, obj->clip != 0);
}

static void mkClip_cb(int index)
{
  QPushButton *button;
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = new QVBoxLayout(oef->control, FIELD_MARGIN, 
                                         FIELD_SPACING, "clip layout");

  wClipToggle = diaCheckBox("Turn clipping plane On", oef->control, layout1);
  QObject::connect(wClipToggle, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(clipToggleSlot(bool)));

  button = diaPushButton("Reset to Center", oef->control, layout1);
  QObject::connect(button, SIGNAL(clicked()), &imodvObjed, 
                   SLOT(clipResetSlot()));

  button = diaPushButton("Invert Polarity", oef->control, layout1);
  QObject::connect(button, SIGNAL(clicked()), &imodvObjed,
                   SLOT(clipInvertSlot()));

  diaLabel("Hold down the Ctrl Key", oef->control, layout1);
  diaLabel("to move & rotate clip", oef->control, layout1);
  diaLabel("plane with mouse or", oef->control, layout1);
  diaLabel("keypad & arrow keys", oef->control, layout1);
  finalSpacer(oef->control, layout1);
}


/*****************************************************************************
 * The move edit field
 *****************************************************************************/
/* DNM changed to provide enough buttons to do complete rotations about each
   of the three axes */

static char *moveLabels[] = {"Top", "Front", "Bottom", "Back",
                             "Top", "Left", "Bottom", "Right",
                             "Front", "Left", "Back", "Right"};
static int moveQuarters[] = {0, -1, 2, 1, 0, 1, 2, -1, 0, 1, 2, -1};
static char *axisLabels[] = {"X", "Y", "Z"};

void ImodvObjed::moveCenterSlot()
{
  Ipoint min, max;
  Imod *imod = Imodv->imod;

  if ((!Imodv->obj) || (!imod))
    return;

  imodObjectGetBBox(Imodv->obj, &min, &max);
  Imodv->imod->view->trans.x = -((max.x + min.x) * 0.5f);
  Imodv->imod->view->trans.y = -((max.y + min.y) * 0.5f);
  Imodv->imod->view->trans.z = -((max.z + min.z) * 0.5f);
  imodvDraw(Imodv);
}

/* DNM: add control of all models */
void ImodvObjed::moveAxisSlot(int which)
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
  case 2:   // Y axis
    rot.z = quarter * 90.0f;
    break;
  }
  imod->view->rot = rot;

  if (Imodv->moveall)
    for (m = 0; m < Imodv->nm; m++)
      Imodv->mod[m]->view->rot = rot;

  imodvDraw(Imodv);
}

static void mkMove_cb(int index)
{
  int width, icol;
  QPushButton *button;
  QLabel *label;
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = new QVBoxLayout(oef->control, FIELD_MARGIN, 
                                         FIELD_SPACING, "move layout");
  button = diaPushButton("Center on Object", oef->control, layout1);
  QObject::connect(button, SIGNAL(clicked()), &imodvObjed, 
                   SLOT(moveCenterSlot()));

  label = diaLabel("Move by rotating around:", oef->control, layout1);

  // Get a grid and a signal mapper to map the buttons
  QGridLayout *grid = new QGridLayout(layout1, 5, 3, 3, "move grid");
  QSignalMapper *mapper = new QSignalMapper(oef->control);
  QObject::connect(mapper, SIGNAL(mapped(int)), &imodvObjed,
                   SLOT(moveAxisSlot(int)));
  width = (int)(1.2 * label->fontMetrics().width("Bottom"));

  // Make the buttons, put in mapper and grid
  for (int i = 0; i < 12; i++) {
    icol = i / 4;
    if (i % 4 == 0) {
      label = new QLabel(axisLabels[icol], oef->control);
      grid->addWidget(label, 0, icol);
      label->setAlignment(Qt::AlignCenter);
    }

    button = new QPushButton(moveLabels[i], oef->control);
    button->setFocusPolicy(QWidget::NoFocus);
    button->setFixedWidth(width);
    grid->addWidget(button, i % 4 + 1, icol);
    mapper->setMapping(button, i);
    QObject::connect(button, SIGNAL(clicked()), mapper, SLOT(map()));
  }    

  finalSpacer(oef->control, layout1);
}         

/*****************************************************************************
 * The subsets edit field
 *****************************************************************************/
// Worried about spacing with a group box, so manage radio behavior manually
QRadioButton *subsetButtons[6];
static char *subsetLabels[6] = {  "Show all ON objects", 
  "Current object only", 
  "Current surface only", 
  "Surface && other objects", 
  "Current contour only", 
  "Contour && other objects"
};

void ImodvObjed::subsetSlot(int which)
{
  subsetButtons[Imodv->current_subset]->blockSignals(true);
  subsetButtons[Imodv->current_subset]->setChecked(false);
  subsetButtons[Imodv->current_subset]->blockSignals(false);
  Imodv->current_subset = which;
  imodvDraw(Imodv);
}

static void mkSubsets_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = new QVBoxLayout(oef->control, FIELD_MARGIN, 
                                         FIELD_SPACING, "move layout");
  diaLabel("Show subset of model:", oef->control, layout1);

  QSignalMapper *mapper = new QSignalMapper(oef->control);
  QObject::connect(mapper, SIGNAL(mapped(int)), &imodvObjed, SLOT(subsetSlot(int)));

  for (int i = 0; i < 6; i++) {
    subsetButtons[i] = new QRadioButton(subsetLabels[i], oef->control);
    layout1->addWidget(subsetButtons[i]);
    subsetButtons[i]->setFocusPolicy(QWidget::NoFocus);
    mapper->setMapping(subsetButtons[i], i);
    QObject::connect(subsetButtons[i], SIGNAL(clicked()), mapper, SLOT(map()));
  }

  subsetButtons[Imodv->current_subset]->setChecked(true);
  finalSpacer(oef->control, layout1);
}


/*****************************************************************************
 *
 * END OF EDIT FIELDS.  START OF OBJECT LIST DIALOG
 *
 * Create the object list dialog
 * 
 *****************************************************************************/

void imodvObjectListDialog(ImodvApp *a, int state)
{
  int ob, m;
  QString qstr;
  int nPerCol;
  char *window_name;

  if (!state){
    if (Oolist_dialog)
      Oolist_dialog->close();
    return;
  }
  if (Oolist_dialog){
    Oolist_dialog->raise();
    return;
  }

  Oolist_dialog = new ImodvOlist(NULL);

  // Get number of buttons, number of columns and number per column
  // Make maximum number of buttons needed for all loaded models
  for (m = 0; m < a->nm; m++)
    if (numOolistButtons < a->mod[m]->objsize) 
      numOolistButtons = a->mod[m]->objsize; 
  if (numOolistButtons > MAX_OOLIST_BUTTONS)
    numOolistButtons = MAX_OOLIST_BUTTONS;
       
  olistNcol = (numOolistButtons + MAX_LIST_IN_COL - 1) / MAX_LIST_IN_COL;
  nPerCol = (numOolistButtons + olistNcol - 1) / olistNcol;

  // Get a signal mapper, connect to the slot for these buttons
  QSignalMapper *mapper = new QSignalMapper(Oolist_dialog);
  QObject::connect(mapper, SIGNAL(mapped(int)), &imodvObjed, 
                   SLOT(toggleListSlot(int)));
  
  // Make the buttons, set properties and map them
  for (ob = 0; ob < numOolistButtons; ob++) {
    qstr.sprintf("%d: ",ob + 1);
    OolistButtons[ob] = new QCheckBox(qstr, Oolist_dialog);
    OolistButtons[ob]->setFocusPolicy(QWidget::NoFocus);
    Oolist_dialog->mGrid->addWidget(OolistButtons[ob], ob % nPerCol, 
                                    ob / nPerCol);
    mapper->setMapping(OolistButtons[ob], ob);
    QObject::connect(OolistButtons[ob], SIGNAL(toggled(bool)), mapper, 
                     SLOT(map()));
  }
  setOnoffButtons();

  window_name = imodwEithername("Imodv Object List: ", a->imod->fileName, 1);
  if (window_name) {
    qstr = window_name;
    free(window_name);
  }
  if (qstr.isEmpty())
    qstr = "Imodv Object List";
  Oolist_dialog->setCaption(qstr);
  imodvDialogManager.add((QWidget *)Oolist_dialog, IMODV_DIALOG);
  Oolist_dialog->show();
}

// Object list class
ImodvOlist::ImodvOlist(QWidget *parent, const char *name, WFlags fl)
  : QWidget(parent, name, fl)
{
  QVBoxLayout *layout = new QVBoxLayout(this, 11, 6, "list layout");
  mGrid = new QGridLayout(layout, 1, 1, 2, "list grid");

  // Make a line
  QFrame *line = new QFrame(this);
  line->setFrameShape( QFrame::HLine );
  line->setFrameShadow( QFrame::Sunken );
  layout->addWidget(line);

  QHBox *box = new QHBox(this);
  QPushButton *button = new QPushButton("Done", box);
  int width = (int)(1.4 * button->fontMetrics().width("Done"));
  button->setFixedWidth(width);
  button->setFocusPolicy(QWidget::NoFocus);
  layout->addWidget(box);
  connect(button, SIGNAL(clicked()), this, SLOT(donePressed()));
}

void ImodvOlist::donePressed()
{
  close();
}

void ImodvOlist::closeEvent ( QCloseEvent * e )
{
  imodvDialogManager.remove((QWidget *)Oolist_dialog);
  Oolist_dialog  = NULL;
  numOolistButtons = 0;
  e->accept();
}

void ImodvOlist::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    imodvKeyPress(e);
}

void ImodvOlist::keyReleaseEvent ( QKeyEvent * e )
{
  imodvKeyRelease(e);
}


/*****************************************************************************/
/************************* internal utility functions. ***********************/
/*****************************************************************************/

static void setObjFlag(long flag, int state)
{
  int ob, m;

  if (!Imodv->imod || !objedObject())
    return;


  if (Imodv_objed_all){
    if (Imodv->crosset){
      for(m = 0; m < Imodv->nm; m++)
        for(ob = 0; ob < Imodv->mod[m]->objsize; ob++)
          if (Imodv_objed_all == editAll ||
              !(Imodv->mod[m]->obj[ob].flags & 
                IMOD_OBJFLAG_OFF))
            if (state){
              Imodv->mod[m]->obj[ob].flags |= flag;
            }else{
              Imodv->mod[m]->obj[ob].flags &= ~flag;
            }
    }else{
      for(ob = 0; ob < Imodv->imod->objsize; ob++)
        if (Imodv_objed_all == editAll ||
            !(Imodv->imod->obj[ob].flags & IMOD_OBJFLAG_OFF))
          if (state){
            Imodv->imod->obj[ob].flags |= flag;
          }else{
            Imodv->imod->obj[ob].flags &= ~flag;
          }     
    }
          
  }else{
    /* DNM: fixed probably bug: the changing for each model was not in the
       for loop, and there was no test for whether the object existed */
    if (Imodv->crosset){
      for(m = 0; m < Imodv->nm; m++) {
        ob = Imodv->ob;
        if (ob < Imodv->mod[m]->objsize) {
          if (state){
            Imodv->mod[m]->obj[ob].flags |= flag;
          }else{
            Imodv->mod[m]->obj[ob].flags &= ~flag;
          }
        }
      }
    }else{
      if (state)
        Imodv->imod->obj[Imodv->ob].flags |= flag;
      else
        Imodv->imod->obj[Imodv->ob].flags &= ~flag;
    }
  }
  return;
}

/* Utility for adding final spacer, hot slider orphans for now */
static void finalSpacer(QWidget *parent, QVBoxLayout *layout)
{
  QVBox *spacer = new QVBox(parent);
  layout->addWidget(spacer);
  layout->setStretchFactor(spacer, 100);
}

int hotSliderFlag()
{
  return HOT_SLIDER_KEYUP;
}

int hotSliderKey() 
{
  return Qt::Key_Control;
}

/*
$Log$
Revision 4.1  2003/02/10 20:29:01  mast
autox.cpp

Revision 1.1.2.10  2003/01/30 00:45:15  mast
Make sliders hot by default

Revision 1.1.2.9  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.8  2003/01/23 20:12:10  mast
switch from button pressed to clicked

Revision 1.1.2.7  2003/01/13 07:21:38  mast
Changes to use new dialog manager class

Revision 1.1.2.6  2002/12/30 06:49:50  mast
rationalizing dialogs as widgets and using dialog list

Revision 1.1.2.5  2002/12/27 17:49:30  mast
Clean up unused variables

Revision 1.1.2.4  2002/12/27 01:21:04  mast
Qt version

Revision 1.1.2.3  2002/12/18 04:15:14  mast
new includes for imodv modules

Revision 1.1.2.2  2002/12/17 18:33:19  mast
using new includes for imodv compoennts

Revision 1.1.2.1  2002/12/15 21:14:02  mast
conversion to cpp

Revision 3.1.2.1  2002/12/07 01:22:49  mast
Added argument to window tilte generator

Revision 3.1  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

*/
