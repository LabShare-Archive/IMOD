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
#include <qspinbox.h>
#include <qpushbutton.h>
#include <qbuttongroup.h>
#include <qradiobutton.h>
#include <qlayout.h>
#include <qvbox.h>
#include <qframe.h>
#include <qtooltip.h>
#include <qsignalmapper.h>
#include "tooledit.h"
#include "formv_objed.h"
#include "multislider.h"
#include "dia_qtutils.h"

#include "imodv.h"
#include "imod.h"
#include "imodv_gfx.h"
#include "imodv_light.h"
#include "imodv_objed.h"
#include "imodv_input.h"
#include "preferences.h"
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
static void fixMove_cb(void);
static void mkSubsets_cb(int index);
static void optionSetFlags (b3dUInt32 *flag);
static void toggleObj(int ob, bool state);
static void setStartEndModel(int &mst, int &mnd);
static bool changeModelObject(int m, int ob);

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
  {"Line Color", mkLineColor_cb, setLineColor_cb, NULL,       NULL},
  {"Fill Color", mkFillColor_cb, setFillColor_cb, NULL,       NULL},
  {"Material",   mkMaterial_cb,  setMaterial_cb,  NULL,       NULL},
  {"Points",     mkPoints_cb,    setPoints_cb,    NULL,       NULL},
  {"Lines",      mkLines_cb,     setLines_cb,     NULL,       NULL},
  {"Mesh View",  mkScalar_cb,    setScalar_cb,    NULL,       NULL},
  {"Clip",       mkClip_cb,      setClip_cb,      NULL,       NULL},
  {"Move",       mkMove_cb,      NULL,            fixMove_cb, NULL},
  {"Subsets",    mkSubsets_cb,   NULL,            NULL,       NULL},

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
  int m, mst, mnd, ob;

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

    setStartEndModel(mst, mnd);
    
    for (m = mst; m <= mnd; m++) {
      for (ob = 0; ob < Imodv->mod[m]->objsize; ob++)
        if (changeModelObject(m, ob)) 
          optionSetFlags(&Imodv->mod[m]->obj[ob].flags);
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
  int m, mst, mnd;
  setStartEndModel(mst, mnd);
    
    /* Turn off same object in all other models if editing all and legal ob */
  for (m = mst; m <= mnd; m++) {
    if (Imodv->mod[m]->objsize > ob)
      if (state) {
        Imodv->mod[m]->obj[ob].flags &= ~IMOD_OBJFLAG_OFF;
        Imodv->ob = ob;
      } else
        Imodv->mod[m]->obj[ob].flags |= IMOD_OBJFLAG_OFF;
  }

  /* If the object is within legal limits for a button list, set that button
     in each list */
  if (ob < numOnoffButtons && ob < Imodv->imod->objsize)
    diaSetChecked(OnoffButtons[ob], state);
  if (ob < numOolistButtons && ob < Imodv->imod->objsize)
    diaSetChecked(OolistButtons[ob], state);

  if (objed_dialog)
    objset(Imodv);
  imodvDraw(Imodv);
  imodvDrawImodImages();
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

     "\tPoints: Adjust rendering of spheres that are displayed at individual "
     "points.  The sphere radius can be incremented, decremented, or typed "
     "in to the spin box.  Note that setting the radius non-zero will cause "
     "points to be displayed at each point, even for open and closed contour "
     "objects.  The quality of the spheres is controlled by the "
     "two quality spin boxes, one to set the quality for the particular "
     "object, and one to set the overall minimum quality for all objects.  "
     "The display quality for an object is the maximum of its own "
     "quality setting and the global setting (range 1 - 4).  The global "
     "setting can also be controlled by the g and G hot keys.\n\n",
           
     "\tLines: The Width slider changes the line width used to "
     "draw the objects.  The \"Anti-Alias Rendering\" check box "
     "selects anti-alias rendering for lines.  "
     "Lines will look smoother with this option on; however, "
     "some artifacts may be noticed.  The \"Thicken current contour\" check "
     "box can be used to highlight the current contour by drawing it thicker "
     "than other contours.  This will "
     "have an effect only for objects whose drawing style is \"Lines\".  "
     "This feature is most useful when using the third mouse button to select "
     "contours."
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

  objed_dialog = new imodvObjedForm(imodvDialogManager.parent(IMODV_DIALOG),
                                    NULL,
                                    Qt::WDestructiveClose | Qt::WType_TopLevel);

  Imodv_objed_all = 0;  // May want to retain this setting

  window_name = imodwEithername("3dmodv Objects: ", a->imod->fileName, 1);
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

void ImodvObjed::fillToggleSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_FCOLOR, state ? 1 : 0);
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
  QToolTip::add(wFillToggle, 
                "Use fill color instead of object color for filled data");

  finalSpacer(oef->control, layout1);

  QObject::connect(fillSliders, SIGNAL(sliderChanged(int, int, bool)), &imodvObjed,
          SLOT(fillColorSlot(int, int, bool)));
}


/*******************************************************************
 * The Material Edit Field
 *******************************************************************/

static MultiSlider *matSliders;
static char *matTitles[] = {"Ambient", "Diffuse", "Specular", "Shininess"};
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
    obj->mat3 = (unsigned char)value;
    break;
  case 5:
    obj->mat3b1 = (unsigned char)value;
    break;
  }
}

void ImodvObjed::materialSlot(int which, int value, bool dragging)
{
  int m, mst, mnd, ob;

  if (!Imodv->imod) return;
     
  setStartEndModel(mst, mnd);
    
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < Imodv->mod[m]->objsize; ob++)
      if (changeModelObject(m, ob)) 
        setMaterial(&(Imodv->mod[m]->obj[ob]), which, value);
  }
  /*     imodPrintStderr("set mat %d, offset %d, value%d\n", *item, offset, cbs->value); */
     
  if (!dragging || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && ctrlPressed) ||
      (hotSliderFlag() == HOT_SLIDER_KEYUP && !ctrlPressed))
    imodvDraw(Imodv);
}

void ImodvObjed::bothSidesSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_TWO_SIDE, state ? 1 : 0);
  objset(Imodv);
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
  diaSetChecked(wBothSides, obj->flags & IMOD_OBJFLAG_TWO_SIDE);
}


static void mkMaterial_cb (int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = new QVBoxLayout(oef->control, FIELD_MARGIN, 
                                         FIELD_SPACING, "material layout");
  matSliders = new MultiSlider(oef->control, 4, matTitles);
  layout1->addLayout(matSliders->getLayout());
  finalSpacer(oef->control, layout1);
  QToolTip::add((QWidget *)matSliders->getSlider(0),
                "Set non-directional light hitting object");
  QToolTip::add((QWidget *)matSliders->getSlider(1), 
                "Set light hitting object from light source");
  QToolTip::add((QWidget *)matSliders->getSlider(2),
                "Set specular reflection properties of object");
  QToolTip::add((QWidget *)matSliders->getSlider(3),
                "Set shininess of object");
  QObject::connect(matSliders, SIGNAL(sliderChanged(int, int, bool)), 
                   &imodvObjed, SLOT(materialSlot(int, int, bool)));

  wBothSides = diaCheckBox("Light Both Sides", oef->control, layout1);
  QObject::connect(wBothSides, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(bothSidesSlot(bool)));
  QToolTip::add(wBothSides, 
                "Make front and back surface both appear brightly lit"); 
}


/*****************************************************************************
 * The point edit field
 *****************************************************************************/

static QSpinBox *wPointSizeBox;
static QSpinBox *wPointQualityBox;
static QSpinBox *wGlobalQualityBox;

void ImodvObjed::pointSizeSlot(int i)
{
  int m, mst, mnd, ob;

  if (!Imodv->imod) return;

  objed_dialog->setFocus();

  setStartEndModel(mst, mnd);
    
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < Imodv->mod[m]->objsize; ob++)
      if (changeModelObject(m, ob)) 
        Imodv->mod[m]->obj[ob].pdrawsize = i;
  }
  imodvDraw(Imodv);     
  imodvDrawImodImages();
  return;
}

void ImodvObjed::pointQualitySlot(int value)
{
  int m, mst, mnd, ob;
  value--;
  if (!Imodv->imod) return;

  objed_dialog->setFocus();

  setStartEndModel(mst, mnd);
     
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < Imodv->mod[m]->objsize; ob++)
      if (changeModelObject(m, ob)) 
        Imodv->mod[m]->obj[ob].mat1b3 = value;
  }
  imodvDraw(Imodv);     
}

void ImodvObjed::globalQualitySlot(int value)
{
  int m, mst, mnd;
  if (!Imodv->imod) return;
  value--;
  objed_dialog->setFocus();

  setStartEndModel(mst, mnd);

  for (m = mst; m <= mnd; m++)
    Imodv->mod[m]->view->world = 
      (Imodv->mod[m]->view->world & ~WORLD_QUALITY_BITS) |
      (value << WORLD_QUALITY_SHIFT);
  imodvDraw(Imodv);     
}

static void setPoints_cb(void)
{
  QString str;
  Iobj *obj = objedObject();
  if (!obj)
    return;

  diaSetSpinBox(wPointSizeBox, obj->pdrawsize);
  diaSetSpinBox(wPointQualityBox, obj->mat1b3 + 1);
  diaSetSpinBox(wGlobalQualityBox, 
                ((Imodv->imod->view->world & WORLD_QUALITY_BITS) >> 
                WORLD_QUALITY_SHIFT) + 1);
}

static void mkPoints_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = new QVBoxLayout(oef->control, FIELD_MARGIN, 
                                         FIELD_SPACING, "points layout");
  QGridLayout *grid = new QGridLayout(layout1, 3, 2);
  QLabel *label = new QLabel("Sphere Size", oef->control);
  wPointSizeBox = new QSpinBox(0, 255, 1, oef->control, "point spin box");
  wPointSizeBox->setFocusPolicy(QWidget::ClickFocus);
  grid->addWidget(label, 1, 1);
  grid->addWidget(wPointSizeBox, 1, 2);
  label = new QLabel("Object Quality", oef->control);
  wPointQualityBox = new QSpinBox(1, 4, 1, oef->control, "quality spin box");
  wPointQualityBox->setFocusPolicy(QWidget::ClickFocus);
  grid->addWidget(label, 2, 1);
  grid->addWidget(wPointQualityBox, 2, 2);
  label = new QLabel("Global Quality", oef->control);
  wGlobalQualityBox = new QSpinBox(1, 4, 1, oef->control, "global quality");
  wGlobalQualityBox->setFocusPolicy(QWidget::ClickFocus);
  grid->addWidget(label, 3, 1);
  grid->addWidget(wGlobalQualityBox, 3, 2);
  QToolTip::add(wPointSizeBox, "Set radius of sphere to draw at each point"
                " in pixels");
  QToolTip::add(wPointQualityBox, "Set quality of sphere-drawing for this "
                "object");
    QToolTip::add(wGlobalQualityBox, "Set overall quality of sphere-drawing");

  QObject::connect(wPointSizeBox, SIGNAL(valueChanged(int)), &imodvObjed,
          SLOT(pointSizeSlot(int)));
  QObject::connect(wPointQualityBox, SIGNAL(valueChanged(int)), &imodvObjed,
          SLOT(pointQualitySlot(int)));
  QObject::connect(wGlobalQualityBox, SIGNAL(valueChanged(int)), &imodvObjed,
          SLOT(globalQualitySlot(int)));

  finalSpacer(oef->control, layout1);
}


/*****************************************************************************
 * The line edit field
 *****************************************************************************/

static MultiSlider *widthSlider;
static QCheckBox *wLineAlias;
static QCheckBox *wThickenCont;
static char *widthLabel[] = {"Line Width"};

void ImodvObjed::lineWidthSlot(int which, int value, bool dragging)
{
  int m, mst, mnd, ob;
     
  if (!Imodv->imod)
    return;
     
  setStartEndModel(mst, mnd);
     
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < Imodv->mod[m]->objsize; ob++)
      if (changeModelObject(m, ob)) 
        Imodv->mod[m]->obj[ob].linewidth = value;
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

void ImodvObjed::lineThickenSlot(bool state)
{
  setObjFlag(IMOD_OBJFLAG_THICK_CONT, state ? 1 : 0);
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
  diaSetChecked(wThickenCont, obj->flags & IMOD_OBJFLAG_THICK_CONT);
}

static void mkLines_cb(int index)
{
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = new QVBoxLayout(oef->control, FIELD_MARGIN, 
                                         FIELD_SPACING, "lines layout");
  widthSlider = new MultiSlider(oef->control, 1, widthLabel, 1, 10);
  layout1->addLayout(widthSlider->getLayout());
  QObject::connect(widthSlider, SIGNAL(sliderChanged(int, int, bool)), 
                   &imodvObjed, SLOT(lineWidthSlot(int, int, bool)));
  QToolTip::add((QWidget *)widthSlider->getSlider(0),
                "Set line width in pixels for 3-D display");

  wLineAlias = diaCheckBox("Anti-alias rendering", oef->control, layout1);
  QObject::connect(wLineAlias, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(lineAliasSlot(bool)));
  QToolTip::add(wLineAlias, "Smooth lines with anti-aliasing");

  layout1->addWidget(wLineAlias);

  wThickenCont = diaCheckBox("Thicken current contour", oef->control, layout1);
  QObject::connect(wThickenCont, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(lineThickenSlot(bool)));
  QToolTip::add(wThickenCont, "Draw current contour thicker");

  layout1->addWidget(wThickenCont);

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
  QToolTip::add(wMeshNormal, "Make surface intensity proportional to "
                "magnitude of normal vectors");

  wMeshFalse = diaCheckBox("False color", oef->control, layout1);
  QObject::connect(wMeshFalse, SIGNAL(toggled(bool)), &imodvObjed, 
          SLOT(meshFalseSlot(bool)));
  QToolTip::add(wMeshFalse, "Show magnitudes in false color");

  meshSliders = new MultiSlider(oef->control, 2, bwLabels);
  layout1->addLayout(meshSliders->getLayout());
  QObject::connect(meshSliders, SIGNAL(sliderChanged(int, int, bool)),
                   &imodvObjed, SLOT(meshLevelSlot(int, int, bool)));
  QToolTip::add((QWidget *)meshSliders->getSlider(0), "Set low end of "
                "contrast ramp for displaying normal magnitudes");
  QToolTip::add((QWidget *)meshSliders->getSlider(1), "Set high end of "
                "contrast ramp for displaying normal magnitudes");

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
  QToolTip::add(wClipToggle, "Toggle clipping plane for this object");

  button = diaPushButton("Reset to Center", oef->control, layout1);
  QObject::connect(button, SIGNAL(clicked()), &imodvObjed, 
                   SLOT(clipResetSlot()));
  QToolTip::add(button, "Shift clipping plane back to center of object");

  button = diaPushButton("Invert Polarity", oef->control, layout1);
  QObject::connect(button, SIGNAL(clicked()), &imodvObjed,
                   SLOT(clipInvertSlot()));
  QToolTip::add(button, 
                "Make clipped part visible, clip visible part of object");

  diaLabel("Hold down the "CTRL_STRING" Key", oef->control, layout1);
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
static QPushButton *moveButtons[12];

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
  int icol;
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

  // Make the buttons, put in mapper and grid
  for (int i = 0; i < 12; i++) {
    icol = i / 4;
    if (i % 4 == 0) {
      label = new QLabel(axisLabels[icol], oef->control);
      grid->addWidget(label, 0, icol);
      label->setAlignment(Qt::AlignCenter);
    }

    moveButtons[i] = new QPushButton(moveLabels[i], oef->control);
    moveButtons[i]->setFocusPolicy(QWidget::NoFocus);
    grid->addWidget(moveButtons[i], i % 4 + 1, icol);
    mapper->setMapping(moveButtons[i], i);
    QObject::connect(moveButtons[i], SIGNAL(clicked()), mapper, SLOT(map()));
  }    
  fixMove_cb();

  finalSpacer(oef->control, layout1);
}         

static void fixMove_cb() 
{
  int width = (int)(1.2 * moveButtons[0]->fontMetrics().width("Bottom"));
  for (int i = 0; i < 12; i++)
    moveButtons[i]->setFixedWidth(width);
}

/*****************************************************************************
 * The subsets edit field
 *****************************************************************************/

static char *subsetLabels[6] = {  "Show all ON objects", 
  "Current object only", 
  "Current surface only", 
  "Surface && other objects", 
  "Current contour only", 
  "Contour && other objects"
};

void ImodvObjed::subsetSlot(int which)
{
  Imodv->current_subset = which;
  imodvDraw(Imodv);
}

static void mkSubsets_cb(int index)
{
  QRadioButton *subsetButton;
  ObjectEditField *oef = &objectEditFieldData[index];

  QVBoxLayout *layout1 = new QVBoxLayout(oef->control, FIELD_MARGIN, 
                                         FIELD_SPACING, "move layout");
  diaLabel("Show subset of model:", oef->control, layout1);

  QButtonGroup *group = new QButtonGroup(1, Qt::Vertical, oef->control);
  group->hide();
  
  QObject::connect(group, SIGNAL(clicked(int)), &imodvObjed, 
		   SLOT(subsetSlot(int)));

  for (int i = 0; i < 6; i++) {
    subsetButton = new QRadioButton(subsetLabels[i], oef->control);
    group->insert(subsetButton);
    layout1->addWidget(subsetButton);
    subsetButton->setFocusPolicy(QWidget::NoFocus);
  }

  diaSetGroup(group, Imodv->current_subset);

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

  Oolist_dialog = new ImodvOlist(imodvDialogManager.parent(IMODV_DIALOG));

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

  window_name = imodwEithername("3dmodv Object List: ", a->imod->fileName, 1);
  if (window_name) {
    qstr = window_name;
    free(window_name);
  }
  if (qstr.isEmpty())
    qstr = "3dmodv Object List";
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
  int m, mst, mnd, ob;

  if (!Imodv->imod || !objedObject())
    return;

  setStartEndModel(mst, mnd);
    
  for (m = mst; m <= mnd; m++) {
    for (ob = 0; ob < Imodv->mod[m]->objsize; ob++)
      if (changeModelObject(m, ob)) {
        if (state)
          Imodv->mod[m]->obj[ob].flags |= flag;
        else
          Imodv->mod[m]->obj[ob].flags &= ~flag;
      }
  }
}

/* Maintain Imodv->ob and set the starting and ending model to change */
static void setStartEndModel(int &mst, int &mnd)
{
  if (Imodv->ob > (Imodv->imod->objsize - 1))
    Imodv->ob = Imodv->imod->objsize - 1;

  mst = 0;
  mnd = Imodv->nm - 1;
  if (!Imodv->crosset)
    mst = mnd = Imodv->cm;
}

/* test whether the object ob should be changed in model m */
static bool changeModelObject(int m, int ob)
{
  return ((ob == Imodv->ob) || (Imodv_objed_all == editAll) || 
      (Imodv_objed_all && 
       !(Imodv->mod[m]->obj[ob].flags & IMOD_OBJFLAG_OFF)));
}


/* Utility for adding final spacer  */
static void finalSpacer(QWidget *parent, QVBoxLayout *layout)
{
  QVBox *spacer = new QVBox(parent);
  layout->addWidget(spacer);
  layout->setStretchFactor(spacer, 100);
}

/*
$Log$
Revision 4.16  2004/04/28 05:28:52  mast
Changes for drawing current contour thicker

Revision 4.15  2003/10/01 05:04:19  mast
change include from imodP to imod after eliminating imod.h from imodP.h

Revision 4.14  2003/06/27 19:42:44  mast
Changes to allow sphere quality tobe controlled

Revision 4.13  2003/06/01 05:43:47  mast
Fixed problem with ambient slider

Revision 4.12  2003/04/25 00:15:00  mast
Moved Light both sides to material panel; implement program name change

Revision 4.11  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.10  2003/04/15 06:07:53  mast
Use invisible button group for subset radios instead of manual management

Revision 4.9  2003/04/15 05:23:47  mast
Added tooltips

Revision 4.8  2003/03/26 23:23:15  mast
switched from hotslider.h to preferences.h

Revision 4.7  2003/03/26 17:15:30  mast
Adjust sizes for font changes

Revision 4.6  2003/03/24 17:58:09  mast
Changes for new preferences capability

Revision 4.5  2003/03/04 21:42:22  mast
Refresh imod image windows when objects turned on/off or point size changes

Revision 4.4  2003/02/28 21:40:32  mast
Changing name of tooledit focus signal

Revision 4.3  2003/02/27 17:35:27  mast
Fixed bug in setting material with multiple models

Revision 4.2  2003/02/21 22:19:54  mast
Use new b3d type

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
