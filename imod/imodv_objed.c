/*  IMOD VERSION 2.50
 *
 *  imodv_objed.c -- The object edit dialog for imodv.
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

#include <sys/types.h>
#include <math.h>

#include <Xm/Xm.h>
#include <Xm/Label.h>
#include <Xm/ToggleB.h>
#include <Xm/ArrowB.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/Protocols.h>
#include <Xm/Frame.h>
#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/List.h>
#include <Xm/AtomMgr.h>
#include <diaP.h>
#include "imodv.h"


/******************************************************************
 * Public Interface function prototypes.
 ******************************************************************/

/* Close up and clean the object edit dialog. */
int object_edit_kill(void);

/* Create and init the object edit dialog. */
void objed(ImodvApp *a);

/* Version 1.3 Functions. */
/*  returns the current object being edited by objed. */
Iobj *objedObject(void) { return(Imodv->obj); }

/* returns the current model being used for object editing. */
Imod *objedModel(void) { return(Imodv->imod); }


char *imodwEithername(char *intro, char *filename);

/*
 *  internal prototypes 
 */

static void setEachAll(void);
static void objset(ImodvApp *a);
static void setObjFlag(long flag, int state);
static void objed_colordrag_cb(Widget w, XtPointer client, XtPointer call);
static void setOnoffButtons(void);


/*
 * global data 
 */
Widget Imodv_dialog = NULL;

static int Imodv_objed_all = FALSE;  /* edit all objects if true. */

/* DNM: actually, Imodv_objed_all is one of these values minus 1, so need
   to place this enum up here so that we can test for editAll-1  */
enum { editOption, editEach, editAll, editOnOnes} ;


/******************************************************************
 * new version of object edit dialog.
 * Uses Object Edit Field for easier expansion.
 ******************************************************************/

typedef struct
{
     char      *label;        /* Label used for list widget.      */
      /* Function used to make edit area. */
     void      (*mkwidget)(Widget, XtPointer, XtPointer);

     XtPointer clientData;    /* Client data                      */
     /* Function to adjust internal data */
     void      (*setwidget)(void);
     Widget    control;       /* Runtime widget storage.          */

}ObjectEditField;

static int      CurrentObjectField       = 0;
static Widget   CurrentObjectFieldWidget = 0;
static Widget Objed_trans;
static void objed_trans_cb
(Widget w, XtPointer client_data, XtPointer call_data);
     

/*******************************************************************
 * The Line Color Edit Field
 *******************************************************************/

static Widget Objed_red   = 0;
static Widget Objed_green = 0;
static Widget Objed_blue  = 0;

static void objed_color_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     int color = (int)client;

     switch(color){
	case 0:
	  Imodv->obj->red = cbs->value / 255.0;
	  break;
	case 1:
	  Imodv->obj->green = cbs->value / 255.0;
	  break;
	case 2:
	  Imodv->obj->blue = cbs->value / 255.0;
	  break;
     }
     objset(Imodv);
     imodvDraw(Imodv);
     return;
}

static void setLineColor_cb(void)
{
     Iobj *obj = objedObject();
     /* Set red, green and blue values. */
     if (Objed_red)
	  XtVaSetValues(Objed_red, XmNvalue, (int)(obj->red * 255), NULL);
     if (Objed_green)
	  XtVaSetValues(Objed_green, XmNvalue, (int)(obj->green * 255), NULL);
     if (Objed_blue)
	  XtVaSetValues(Objed_blue, XmNvalue, (int)(obj->blue * 255), NULL);
     if (Objed_trans)
	 XtVaSetValues(Objed_trans, XmNvalue, obj->trans, NULL);
}

static void mkLineColor_cb(Widget parent, XtPointer client, XtPointer call)
{
     ObjectEditField *oef = (ObjectEditField *)call;
     ImodvApp *a = (ImodvApp *)client;

     static Widget row, red, green, blue;

     oef->control = row = XtVaCreateWidget
	  ("objectEditField", xmRowColumnWidgetClass, parent,
	   XmNleftAttachment, XmATTACH_FORM,   
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM, 
	   XmNrightAttachment, XmATTACH_FORM,
	   XmNmarginHeight, 5,     /* Needed here to avoid losing Trans. */
	   NULL);

/* DNM: made all values show here and in other widgets.  The sliders pack 
better when a label is defines as a separate row instead of included with
the scale widget, so do that here, and for fill color and materials */

     Objed_red = XtVaCreateManagedWidget
	  ("Scale",  xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 255,
	   XmNvalue, 0,
	   XmNshowValue, True,
	   XmNscaleMultiple, 1,
	   NULL);
     XtAddCallback(Objed_red, XmNvalueChangedCallback,
		   objed_color_cb, (XtPointer)0);
     XtAddCallback(Objed_red, XmNdragCallback,
		   objed_colordrag_cb, (XtPointer)0);
     XtVaCreateManagedWidget("Red", xmLabelWidgetClass, row, NULL);

     
     Objed_green = XtVaCreateManagedWidget
	  ("Scale",  xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 255,
	   XmNvalue, 0,
	   XmNshowValue, True,
	   XmNscaleMultiple, 1,
	   NULL);
     XtAddCallback(Objed_green, XmNvalueChangedCallback,
		   objed_color_cb, (XtPointer)1);
     XtAddCallback(Objed_green, XmNdragCallback,
		   objed_colordrag_cb, (XtPointer)1);
     XtVaCreateManagedWidget("Green", xmLabelWidgetClass, row, NULL);
     
     Objed_blue = XtVaCreateManagedWidget
	  ("Scale",  xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 255,
	   XmNvalue, 0,
	   XmNshowValue, True,
	   XmNscaleMultiple, 1,
	   NULL);
     XtAddCallback(Objed_blue, XmNvalueChangedCallback,
		   objed_color_cb, (XtPointer)2);
     XtAddCallback(Objed_blue, XmNdragCallback,
		   objed_colordrag_cb, (XtPointer)2);
     XtVaCreateManagedWidget("Blue", xmLabelWidgetClass, row, NULL);


     Objed_trans =  XtVaCreateManagedWidget
	  ("Trans", xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0, XmNmaximum, 100, XmNvalue, 0,
	   XmNvalue, 0,
	   XmNshowValue, True,
	   XmNscaleMultiple, 1,
	   NULL);
     XtAddCallback(Objed_trans, XmNvalueChangedCallback,
		   objed_trans_cb, NULL);
     XtVaCreateManagedWidget("Transparency", xmLabelWidgetClass, row, NULL);

     setLineColor_cb();
     return;
}


/*******************************************************************
 * The Fill Color Edit Field
 *******************************************************************/
static Widget wFillRed    = 0;
static Widget wFillGreen  = 0;
static Widget wFillBlue   = 0;
static Widget wFillToggle = 0;
static Widget wBothSides  = 0;

static void fillToggle_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     Iobj *obj = objedObject();

     if (!obj) return;

     if (obj->flags & IMOD_OBJFLAG_FCOLOR)
	  setObjFlag(IMOD_OBJFLAG_FCOLOR, False);
     else
	  setObjFlag(IMOD_OBJFLAG_FCOLOR, True);

     objset(Imodv);
     imodvDraw(Imodv);
}

static void bothSides_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     Iobj *obj = objedObject();

     if (!obj) return;

     if (obj->flags & IMOD_OBJFLAG_TWO_SIDE)
	  setObjFlag(IMOD_OBJFLAG_TWO_SIDE, False);
     else
	  setObjFlag(IMOD_OBJFLAG_TWO_SIDE, True);

     objset(Imodv);
     imodvDraw(Imodv);
}

static void fillColor_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     int color = (int)client;
     Iobj *obj = objedObject();
     unsigned char *colors;

     if (!obj) return;
     colors = (unsigned char *)&(obj->mat1);
     colors[color] = cbs->value;

     objset(Imodv);
     imodvDraw(Imodv);
     return;
}

static void setFillColor_cb(void)
{
     Iobj *obj = objedObject();
     unsigned char *colors = (unsigned char *)&(obj->mat1);
     Boolean state, state2;
     if (obj->flags & IMOD_OBJFLAG_FCOLOR)
	  state = True;
     else
	  state = False;
     if (obj->flags & IMOD_OBJFLAG_TWO_SIDE)
	  state2 = True;
     else
	  state2 = False;

     if (wFillRed)
	  XtVaSetValues(wFillRed, XmNvalue, colors[0], NULL);
     if (wFillGreen)
	  XtVaSetValues(wFillGreen, XmNvalue, colors[1], NULL);
     if (wFillBlue)
	  XtVaSetValues(wFillBlue, XmNvalue, colors[2], NULL);
     if (wFillToggle)
	   XmToggleButtonSetState
		(wFillToggle, state, False);
     if (wBothSides)
	   XmToggleButtonSetState
		(wBothSides, state2, False);
}

static void mkFillColor_cb(Widget parent, XtPointer client, XtPointer call)
{

     ObjectEditField *oef = (ObjectEditField *)call;
     ImodvApp *a = (ImodvApp *)client;

     static Widget row, red, green, blue;

     oef->control =  row = XtVaCreateWidget
	  ("objectEditField", xmRowColumnWidgetClass, parent,
	   XmNleftAttachment, XmATTACH_FORM,   
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM, 
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);

     wFillRed = XtVaCreateManagedWidget
	  ("Scale",  xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 255,
	   XmNvalue, 0,
	   XmNshowValue, True,
	   XmNscaleMultiple, 1,
	   NULL);
     XtAddCallback(wFillRed, XmNvalueChangedCallback,
		   fillColor_cb, (XtPointer)0);
     XtVaCreateManagedWidget("Red", xmLabelWidgetClass, row, NULL);
     
     wFillGreen = XtVaCreateManagedWidget
	  ("Scale",  xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 255,
	   XmNvalue, 0,
	   XmNshowValue, True,
	   XmNscaleMultiple, 1,
	   NULL);
     XtAddCallback(wFillGreen, XmNvalueChangedCallback,
		   fillColor_cb, (XtPointer)1);
     XtVaCreateManagedWidget("Green", xmLabelWidgetClass, row, NULL);
     
     wFillBlue = XtVaCreateManagedWidget
	  ("Scale",  xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 255,
	   XmNvalue, 0,
	   XmNshowValue, True,
	   XmNscaleMultiple, 1,
	   NULL);
     XtAddCallback(wFillBlue, XmNvalueChangedCallback,
		   fillColor_cb, (XtPointer)2);
     XtVaCreateManagedWidget("Blue", xmLabelWidgetClass, row, NULL);

     wFillToggle = XtVaCreateManagedWidget
	  ("Use Fill Color", xmToggleButtonWidgetClass, row, NULL);

     XtAddCallback(wFillToggle,
		   XmNvalueChangedCallback, fillToggle_cb, 0);
     
     wBothSides = XtVaCreateManagedWidget
	  ("Light Both Sides", xmToggleButtonWidgetClass, row, NULL);

     XtAddCallback(wBothSides,
		   XmNvalueChangedCallback, bothSides_cb, 0);
     
     setFillColor_cb();
     return;
}


/*******************************************************************
 * The Material Edit Field
 *******************************************************************/
static Widget Objed_specular;
static Widget Objed_shininess;

static Widget Objed_ambient;
static Widget Objed_diffuse;

static void objed_mat_cb(Widget w, XtPointer client_data, XtPointer call_data);

static void setMaterial_cb(void)
{
     Iobj *obj = objedObject();
     if (!obj) return;

     XtVaSetValues(Objed_specular, XmNvalue, (int)obj->specular, NULL);
     XtVaSetValues(Objed_ambient, XmNvalue, (int)obj->ambient, NULL);
     XtVaSetValues(Objed_diffuse, XmNvalue, (int)obj->diffuse, NULL);
     XtVaSetValues(Objed_shininess, XmNvalue, (int)obj->shininess, NULL);
}

static void objed_trans_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     int ob,m;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;

     if (!Imodv->imod)
	  return;

     Imodv->obj->trans = cbs->value;
     if (Imodv->crosset)
	  for(m = 0; m < Imodv->nm; m++)
	       if (Imodv->mod[m]->objsize > Imodv->ob)
		    Imodv->mod[m]->obj[Imodv->ob].trans = cbs->value;

     imodvDraw(Imodv);
     return;
}


static void mkMaterial_cb (Widget parent, XtPointer client, XtPointer call)
{
     ObjectEditField *oef = (ObjectEditField *)call;
     ImodvApp *a = (ImodvApp *)client;
     Widget row;

     oef->control = row =  XtVaCreateWidget
	  ("objectEditField", xmRowColumnWidgetClass, parent,
	   XmNleftAttachment, XmATTACH_FORM,   
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM, 
	   XmNrightAttachment, XmATTACH_FORM,
	   XmNmarginHeight, 5,     /* Needed here to keep sliders in column */
	   NULL);

/*     Objed_trans =  XtVaCreateManagedWidget
	  ("Trans", xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0, XmNmaximum, 100, XmNvalue, 0,
	   XmNscaleMultiple, 1,
	   XtVaTypedArg, XmNtitleString, XmRString, 
	   "Transparency", 13,
	   NULL);
     XtAddCallback(Objed_trans, XmNvalueChangedCallback,
		   objed_trans_cb, NULL);
*/   
     
     Objed_ambient = XtVaCreateManagedWidget
	  ("Scale",  xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0, XmNmaximum, 255,  XmNvalue, 0,   
	   XmNshowValue, True, XmNscaleMultiple, 1,
	   NULL);
     XtAddCallback(Objed_ambient, XmNvalueChangedCallback,
		   objed_mat_cb, (XtPointer)XtOffsetOf(Iobj,ambient));
     XtVaCreateManagedWidget("Ambient", xmLabelWidgetClass, row, NULL);
     
     Objed_diffuse = XtVaCreateManagedWidget
	  ("Scale",  xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,  XmNmaximum, 255,
	   XmNvalue, 0,    XmNshowValue, True,
	   XmNscaleMultiple, 1,
	   NULL);
     XtAddCallback(Objed_diffuse, XmNvalueChangedCallback,
		   objed_mat_cb, (XtPointer)XtOffsetOf(Iobj, diffuse));
     XtVaCreateManagedWidget("Diffuse", xmLabelWidgetClass, row, NULL);


     Objed_specular = XtVaCreateManagedWidget
	  ("Scale",  xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 255,
	   XmNvalue, 0,
	   XmNshowValue, True,
	   XmNscaleMultiple, 1,
	   NULL);
     XtAddCallback(Objed_specular, XmNvalueChangedCallback,
		   objed_mat_cb, (XtPointer)XtOffsetOf(Iobj, specular));
     XtVaCreateManagedWidget("Specular", xmLabelWidgetClass, row, NULL);


     Objed_shininess = XtVaCreateManagedWidget
	 ("Scale",  xmScaleWidgetClass, row,
	  XmNorientation, XmHORIZONTAL,
	  XmNminimum, 0,  XmNmaximum, 255,
	  XmNvalue, 0,    XmNshowValue, True,
	  XmNscaleMultiple, 1,
	  NULL);
     XtAddCallback(Objed_shininess, XmNvalueChangedCallback,
		   objed_mat_cb, (XtPointer)XtOffsetOf(Iobj, shininess));
     XtVaCreateManagedWidget("Shininess", xmLabelWidgetClass, row, NULL);
     
}


/*****************************************************************************
 * The point edit field
 *****************************************************************************/
static Widget Objed_size;

static void objed_size_cb(Widget w, XtPointer client, XtPointer call)
{
     char *name = NULL;
     int i,m,ob;


     if (!Imodv->imod) return;

     setEachAll();
     name = XmTextGetString(w);

     /* DNM: on PC, need to check for empty string or get huge number */
     if (name && name[0] != 0x00) {
	  sscanf(name, "%d", &i);
	  if (i < 0)
	       i = 0;
     }
     XtFree(name);

     if (Imodv->ob > (Imodv->imod->objsize - 1))
	  Imodv->ob = Imodv->imod->objsize - 1;

     
     if (Imodv_objed_all){
	  for(ob = 0; ob < Imodv->imod->objsize; ob++)
	       if (Imodv_objed_all == editAll - 1 ||
		   !(Imodv->imod->obj[ob].flags & IMOD_OBJFLAG_OFF))
		    Imodv->imod->obj[ob].pdrawsize = i;
	  if (Imodv->crosset)
	       for(m = 0; m < Imodv->nm; m++)
		    for(ob = 0; ob < Imodv->mod[m]->objsize; ob++)
		         if (Imodv_objed_all == editAll - 1 ||
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
     XmString objname;
     char nobjst[32];
     Iobj *obj = objedObject();
     if (!obj) return;
     
     sprintf(nobjst, "%d", obj->pdrawsize);
     objname = XmStringCreateSimple(nobjst);
     XtVaSetValues(Objed_size, XmNvalue, nobjst, NULL);
     XmStringFree(objname);  
}

static void mkPoints_cb   (Widget parent, XtPointer client, XtPointer call)
{
     ObjectEditField *oef = (ObjectEditField *)call;
     ImodvApp *a = (ImodvApp *)client;
     Widget row;
     
     oef->control = row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, parent,
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);
     
     Objed_size = XtVaCreateManagedWidget
	  ("Scattered Point Size", xmTextWidgetClass, row, NULL);
     XtAddCallback(Objed_size, XmNactivateCallback,
		   objed_size_cb, NULL);
     XtVaCreateManagedWidget
	  ("Scattered Point Size", xmLabelWidgetClass, row, NULL);
}

/*****************************************************************************
 * The line edit field
 *****************************************************************************/

static Widget Objed_line_scale;

static Widget wLineWidth;
static Widget wLineAlias;

static void obj_line_width_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     int ob,m;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
     
     if (!Imodv->imod)
	  return;
     
     if (Imodv->ob > (Imodv->imod->objsize - 1))
	  Imodv->ob = Imodv->imod->objsize - 1;
     
     if (Imodv_objed_all){
	  for(ob = 0; ob < Imodv->imod->objsize; ob++)
	       if (Imodv_objed_all == editAll - 1 ||
		   !(Imodv->imod->obj[ob].flags & IMOD_OBJFLAG_OFF))
		    Imodv->imod->obj[ob].linewidth = cbs->value;
	  if (Imodv->crosset)
	       for(m = 0; m < Imodv->nm; m++)
		    for(ob = 0; ob < Imodv->mod[m]->objsize; ob++)
		         if (Imodv_objed_all == editAll - 1 ||
			     !(Imodv->mod[m]->obj[ob].flags & 
			       IMOD_OBJFLAG_OFF))
			      Imodv->mod[m]->obj[ob].linewidth = cbs->value;
     } else {

          Imodv->imod->obj[Imodv->ob].linewidth = cbs->value;
	  if (Imodv->crosset)
	    for(m = 0; m < Imodv->nm; m++)
	         if (Imodv->mod[m]->objsize > Imodv->ob)
		      Imodv->mod[m]->obj[Imodv->ob].linewidth = cbs->value;
     }

     imodvDraw(Imodv);
     return;
}

static void lineAlias_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     Iobj *obj = objedObject();

     if (!obj) return;

     if (obj->flags & IMOD_OBJFLAG_ANTI_ALIAS)
	  setObjFlag(IMOD_OBJFLAG_ANTI_ALIAS, False);
     else
	  setObjFlag(IMOD_OBJFLAG_ANTI_ALIAS, True);
     objset(Imodv);
     imodvDraw(Imodv);
}


static void setLines_cb(void)
{
     int alias;
     Iobj *obj = objedObject();
     if (!obj) return;
     XtVaSetValues(wLineWidth, XmNvalue, obj->linewidth, NULL);
     
     if (obj->flags & IMOD_OBJFLAG_ANTI_ALIAS )
	  alias = True;
     else
	  alias = False;

     XmToggleButtonSetState (wLineAlias, alias, False);
}

static void mkLines_cb    (Widget parent, XtPointer client, XtPointer call)
{
     ObjectEditField *oef = (ObjectEditField *)call;
     ImodvApp *a = (ImodvApp *)client;
     Widget row;
     
     oef->control = row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, parent,
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);
      
     wLineWidth = XtVaCreateManagedWidget
	  ("line_scale",
	   xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 1,
	   XmNmaximum, 10,
	   XmNvalue, 1,
	   XmNshowValue, True,
	   XmNscaleMultiple, 1,
	   XtVaTypedArg, XmNtitleString,
	   XmRString, "Line Width", 11,
	   NULL);
     XtAddCallback(wLineWidth, XmNvalueChangedCallback,
		   obj_line_width_cb, NULL);
     
     wLineAlias = XtVaCreateManagedWidget
	  ("Anti-Alias Rendering", xmToggleButtonWidgetClass, row, NULL);

     XtAddCallback(wLineAlias,
		   XmNvalueChangedCallback, lineAlias_cb, 0);

     setLines_cb();
}


/*****************************************************************************
 * The mesh edit field
 *****************************************************************************/
#ifdef OBJ_MKMESH
static int fMeshCap = False;
static Widget wMeshCap = 0;

static void meshCalc_cb(Widget w, XtPointer client, XtPointer call)
{
     Imod *imod = objedModel();
     Iobj *obj = objedObject();

     int cap = 0;
     if (fMeshCap) cap = 2;
     diaBusyCursor(True);
     imeshSkinObject(obj, (Ipoint *)&(imod->xscale), 0.0, cap, 0);
/*     imeshSkinObject(obj, (Ipoint *)&(imod->xscale), 0, 0); */
     imodvDraw(Imodv);
     diaBusyCursor(False);
}

static void meshCap_cb(Widget w, XtPointer client, XtPointer call)
{
     if (fMeshCap)
	  fMeshCap = False;
     else
	  fMeshCap = True;
     objset(Imodv);
}

static void setMesh_cb(void)
{
     if (fMeshCap)
	  XmToggleButtonSetState (wMeshCap, True, False);
     else
	  XmToggleButtonSetState (wMeshCap, False, False);
}

static void mkMesh_cb     (Widget parent, XtPointer client, XtPointer call)
{
     ObjectEditField *oef = (ObjectEditField *)call;
     ImodvApp *a = (ImodvApp *)client;
     Widget row, pb;
     
     oef->control = row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, parent,
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);

     pb = XtVaCreateManagedWidget
	  ("Calculate New Mesh", xmPushButtonWidgetClass, row,
	   NULL);
     XtAddCallback(pb, XmNactivateCallback, meshCalc_cb, 0);

     wMeshCap = XtVaCreateManagedWidget
	  ("Cap ends", xmToggleButtonWidgetClass, row, NULL);
     XtAddCallback(wMeshCap, XmNvalueChangedCallback,
		   meshCap_cb, NULL);

     

}
#endif
/*****************************************************************************
 * The scalar edit field
 *****************************************************************************/
static Widget wMeshNormal = 0;
static Widget wMeshFalse  = 0;
static Widget wMeshBright;
static Widget wMeshContrast;


static void meshNormal_cb(Widget w, XtPointer client, XtPointer call)
{
      XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
      Iobj *obj = objedObject();
      if (!obj) return;

      if (obj->flags & IMOD_OBJFLAG_SCALAR)
	   setObjFlag(IMOD_OBJFLAG_SCALAR, False);
      else
	   setObjFlag(IMOD_OBJFLAG_SCALAR, True);
      
      objset(Imodv);
      imodvDraw(Imodv);
      
}

static void meshFalse_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     Iobj *obj = objedObject();
     
     if (!obj) return;
     
     if (obj->flags & IMOD_OBJFLAG_MCOLOR)
	  setObjFlag(IMOD_OBJFLAG_MCOLOR, False);
     else
	  setObjFlag(IMOD_OBJFLAG_MCOLOR, True);
     
     objset(Imodv);
     imodvDraw(Imodv);
}

static void setScalar_cb(void)
{
     unsigned char *ub;
     Iobj *obj = objedObject();
     if (!obj) return;
     ub = (unsigned char *)&(obj->mat3);

     if (IMOD_OBJFLAG_SCALAR & obj->flags)
	  XmToggleButtonSetState (wMeshNormal, True, False);
     else
	  XmToggleButtonSetState (wMeshNormal, False, False);
     
     if (IMOD_OBJFLAG_MCOLOR & obj->flags)
	  XmToggleButtonSetState (wMeshFalse, True, False);
     else
	  XmToggleButtonSetState (wMeshFalse, False, False);
     
     XtVaSetValues(wMeshBright,   XmNvalue, ub[0], NULL);

     if (!ub[1]) ub[1] = 255;
     XtVaSetValues(wMeshContrast, XmNvalue, ub[1], NULL);
}

static void mkScalar_cb     (Widget parent, XtPointer client, XtPointer call)
{
     ObjectEditField *oef = (ObjectEditField *)call;
     ImodvApp *a = (ImodvApp *)client;
     Widget row, pb;
     
     oef->control = row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, parent,
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);

     wMeshNormal = XtVaCreateManagedWidget
	  ("Show Normal Magnitudes", xmToggleButtonWidgetClass, row, NULL);
     XtAddCallback(wMeshNormal, XmNvalueChangedCallback,
		   meshNormal_cb, NULL);
     
     wMeshFalse = XtVaCreateManagedWidget
	  ("False Color", xmToggleButtonWidgetClass, row, NULL);
     XtAddCallback(wMeshFalse, XmNvalueChangedCallback,
		   meshFalse_cb, NULL);
     
     wMeshBright = XtVaCreateManagedWidget
	  ("Scale",  xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,  XmNmaximum, 255,
	   XmNvalue, 127,    XmNshowValue, True,
	   XmNscaleMultiple, 1,
	   XtVaTypedArg, XmNtitleString, XmRString, "Black Level", 12,
	   NULL);
     XtAddCallback(wMeshBright, XmNvalueChangedCallback,
		   objed_mat_cb, (XtPointer)XtOffsetOf(Iobj, mat3));
     
     wMeshContrast = XtVaCreateManagedWidget
	  ("Scale",  xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 1,  XmNmaximum, 255,
	   XmNvalue, 127,    XmNshowValue, True,
	   XmNscaleMultiple, 1,
	   XtVaTypedArg, XmNtitleString, XmRString, "White Level", 12,
	   NULL);
     XtAddCallback(wMeshContrast, XmNvalueChangedCallback,
		   objed_mat_cb, (XtPointer)(XtOffsetOf(Iobj, mat3) + 1));
}




/*****************************************************************************
 * The clip edit field
 *****************************************************************************/
static Widget wClipToggle;

static void imodv_clip_reset_cb(Widget w, XtPointer client, XtPointer call)
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

static void imodv_clip_invert_cb(Widget w, XtPointer client, XtPointer call)
{
     Iobj *obj = objedObject();     
     obj->clip_normal.x = -obj->clip_normal.x;
     obj->clip_normal.y = -obj->clip_normal.y;
     obj->clip_normal.z = -obj->clip_normal.z;
     imodvDraw(Imodv);
     
}

static void imodv_clip_cb(Widget w, XtPointer client_data, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     
     Iobj *obj = objedObject();     
     if (!obj)
	  return;
     
     if (obj->clip)
	  obj->clip = 0;
     else {
	  obj->clip = 1;
	  /* DNM: if this is the first time it's been turned on, set to
	     middle of object */
	  if (obj->clip_point.x == 0.0 && obj->clip_point.y == 0.0 &&
	      obj->clip_point.z == 0.0 && obj->clip_normal.x == 0.0 &&
	      obj->clip_normal.y == 0.0 && obj->clip_normal.z == -1.0) {
	       imodv_clip_reset_cb(w, client_data, call);
	       return;
	  }
     }
     objset(Imodv);
     imodvDraw(Imodv);
     return;
}

static void setClip_cb(void)
{
     Iobj *obj = objedObject();
     if (!obj) return;

     if (obj->clip)
	  XmToggleButtonSetState(wClipToggle, True, False);
     else
	  XmToggleButtonSetState(wClipToggle, False, False);
     
}

static void mkClip_cb     (Widget parent, XtPointer client, XtPointer call)
{
     ObjectEditField *oef = (ObjectEditField *)call;
     ImodvApp *a = (ImodvApp *)client;
     Widget row, button;
     
     oef->control = row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, parent,
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);

     wClipToggle = XtVaCreateManagedWidget
	  ("Turn Clipping Plane On", xmToggleButtonWidgetClass, row, NULL);
     XtAddCallback(wClipToggle, XmNvalueChangedCallback,
		   imodv_clip_cb, NULL);

     XtVaCreateManagedWidget
	  ("Hold down the Ctrl Key", xmLabelWidgetClass, row, NULL);
     XtVaCreateManagedWidget
	  ("to move & rotate clip", xmLabelWidgetClass, row, NULL);
     XtVaCreateManagedWidget
	  ("plane with mouse or", xmLabelWidgetClass, row, NULL);
     XtVaCreateManagedWidget
	  ("keypad & arrow keys", xmLabelWidgetClass, row, NULL);
     
     button = XtVaCreateManagedWidget
	  ("Reset", xmPushButtonWidgetClass, row, NULL);
     
     XtAddCallback(button, XmNactivateCallback, imodv_clip_reset_cb, 0);

     button = XtVaCreateManagedWidget
	  ("Invert", xmPushButtonWidgetClass, row, NULL);
     
     XtAddCallback(button, XmNactivateCallback, imodv_clip_invert_cb, 0);

}

/*****************************************************************************
 * The light edit field
 *****************************************************************************/
static Widget wLightHoriz;
static Widget wLightVert;
static Widget wLightDist;

static Widget wLightX;
static Widget wLightY;
static Widget wLightZ;
static Widget wLightC;
static Widget wLightL;
static Widget wLightQ;
static Widget wLightD;

void light_getparam(int param, float *outValue);
static void setLight_cb(void)
{
     Iobj *obj = objedObject();
     /*     XtVaSetValues( wLightHoriz, XmNvalue, Imodv->lightx, NULL); */
     /*     XtVaSetValues( wLightVert, XmNvalue,  Imodv->lighty, NULL); */
     char string[32];
     float param;
     
     light_getparam(1, &param);
     sprintf(string, "%g", param);
     XmTextSetString(wLightX,string);
     light_getparam(2, &param);
     sprintf(string, "%g", param);
     XmTextSetString(wLightY,string);
     light_getparam(3, &param);
     sprintf(string, "%g", param);
     XmTextSetString(wLightZ,string);

     light_getparam(4, &param);
     sprintf(string, "%g", param);
     XmTextSetString(wLightC,string);
     light_getparam(5, &param);
     sprintf(string, "%g", param);
     XmTextSetString(wLightL,string);
     light_getparam(6, &param);
     sprintf(string, "%g", param);
     XmTextSetString(wLightQ,string);

     light_getparam(7, &param);
     sprintf(string, "%g", param);
     XmTextSetString(wLightD, string);
}

static void lightHoriz_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     int ob,m;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
     ImodvApp *a = (ImodvApp *)client_data;

     a->lightx = cbs->value;
     light_move(&(a->lightx), &(a->lighty));     
     imodvDraw(a);
     return;
}

static void lightVert_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     int ob,m;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
     ImodvApp *a = (ImodvApp *)client_data;

     a->lighty = cbs->value;
     light_move(&(a->lightx), &(a->lighty));
     imodvDraw(a);
     return;
}

void light_setparam(int param, double value);
static void light_cb(Widget w, XtPointer client, XtPointer call)
{
    int param = (int)client;
    double lval = 0.0;
    char *string;
    
    string = XmTextGetString(w);
    if (string){
	lval = atof(string);
	light_setparam(param, lval);
	free(string);
    }
    imodvDraw(Imodv);
}


static void mkLight_cb(Widget parent, XtPointer client, XtPointer call)
{
     ObjectEditField *oef = (ObjectEditField *)call;
     ImodvApp *a = Imodv; /* (ImodvApp *)client;*/
     Widget srow, row;

     oef->control = row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, parent, 
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);
/*
     wLightHoriz = XtVaCreateManagedWidget
	  ("Scale", xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, -899,  XmNmaximum, 899,
	   XmNvalue, a->lightx,    XmNshowValue, False,
	   XmNscaleMultiple, 1,
	   XmNdecimalPoints, 1,
	   XtVaTypedArg, XmNtitleString, XmRString, "Horizontal", 10,
	   NULL);
     XtAddCallback(wLightHoriz, XmNvalueChangedCallback,
		   lightHoriz_cb, (XtPointer)a);
     
     wLightVert = XtVaCreateManagedWidget
	  ("Scale", xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, -899,  XmNmaximum, 899,
	   XmNvalue, a->lighty,    XmNshowValue, False,
	   XmNscaleMultiple, 1,
	   XmNdecimalPoints, 1,
	   XtVaTypedArg, XmNtitleString, XmRString, "Vertical", 9,
	   NULL);
     XtAddCallback(wLightVert, XmNvalueChangedCallback,
		   lightVert_cb, (XtPointer)a);
*/
     XtVaCreateManagedWidget
          ("Light Normal", xmLabelWidgetClass, row, NULL);
     srow = XtVaCreateManagedWidget
	 ("arrowcol", xmRowColumnWidgetClass, row,
	  XmNorientation, XmHORIZONTAL, NULL);
     wLightX = XtVaCreateManagedWidget
	 ("text", xmTextWidgetClass, srow, 
	  XmNcolumns, 6,
	  NULL);
     XtAddCallback(wLightX, XmNactivateCallback,
                   light_cb, (XtPointer)1);     
     wLightY = XtVaCreateManagedWidget
	 ("text", xmTextWidgetClass, srow, 
	  XmNcolumns, 6,
	  NULL);
     XtAddCallback(wLightY, XmNactivateCallback,
                   light_cb, (XtPointer)2);     
     wLightZ = XtVaCreateManagedWidget
	 ("text", xmTextWidgetClass, srow, 
	  XmNcolumns, 6,
	  NULL);
     XtAddCallback(wLightZ, XmNactivateCallback,
                   light_cb, (XtPointer)3);     
     
     XtVaCreateManagedWidget
          ("Light Attenuation", xmLabelWidgetClass, row, NULL);
     srow = XtVaCreateManagedWidget
	 ("arrowcol", xmRowColumnWidgetClass, row,
	  XmNorientation, XmHORIZONTAL, NULL);
     wLightC = XtVaCreateManagedWidget
	 ("text", xmTextWidgetClass, srow, 
	  XmNcolumns, 6,
	  NULL);
     XtAddCallback(wLightC, XmNactivateCallback,
                   light_cb, (XtPointer)4);     
     wLightL = XtVaCreateManagedWidget
	 ("text", xmTextWidgetClass, srow, 
	  XmNcolumns, 6,
	  NULL);
     XtAddCallback(wLightL, XmNactivateCallback,
                   light_cb, (XtPointer)5);     
     wLightQ = XtVaCreateManagedWidget
	 ("text", xmTextWidgetClass, srow, 
	  XmNcolumns, 6,
	  NULL);
     XtAddCallback(wLightQ, XmNactivateCallback,
                   light_cb, (XtPointer)6);     
     
     srow = XtVaCreateManagedWidget
          ("arrowcol", xmRowColumnWidgetClass, row,
           XmNorientation, XmHORIZONTAL, NULL);
     XtVaCreateManagedWidget
          ("Light Distence", xmLabelWidgetClass, srow, NULL);
     wLightD = XtVaCreateManagedWidget
          ("text", xmTextWidgetClass, srow, 
	  XmNcolumns, 10,
	   NULL);
     XtAddCallback(wLightD, XmNactivateCallback,
                   light_cb, (XtPointer)7);     

     XtVaCreateManagedWidget
	  ("Shift+Mouse2 Moves Light", xmLabelWidgetClass, row, NULL);
}

/*****************************************************************************
 * The move edit field
 *****************************************************************************/
/* DNM changed to provide enough buttons to do complete rotations about each
   of the three axes */

static void moveCenter_cb(Widget w, XtPointer client_data, XtPointer call)
{
     Ipoint min, max;
     Imod *imod = Imodv->imod;

     if ((!Imodv->obj) || (!imod))
	  return;

     imodObjectGetBBox(Imodv->obj, &min, &max);
     Imodv->imod->view->trans.x = -((max.x + min.x) * 0.5f);
     Imodv->imod->view->trans.y = -((max.y + min.y) * 0.5f);
     Imodv->imod->view->trans.z = -((max.z + min.z) * 0.5f);
/*     Imodv->imod->view->trans.x = ((max.x + min.x) * 0.5f); */
/*     Imodv->imod->view->trans.y = ((max.y + min.y) * 0.5f); */
/*     Imodv->imod->view->trans.z = ((max.z + min.z) * 0.5f); */
     imodvDraw(Imodv);
     return;
}

/* DNM: add control of all models */
static void moveX_cb(Widget w, XtPointer client_data, XtPointer call)
{
    int quarter = (int) client_data;
    int m, nm;
    Imod *imod = Imodv->imod;
    if (!imod)
          return;
    imod->view->rot.x = quarter * 90.0f;
    imod->view->rot.y = 0.0f;
    imod->view->rot.z = 0.0f;
    if (Imodv->moveall)
	 for (m = 0; m < Imodv->nm; m++) {
	      imod = Imodv->mod[m];
	      imod->view->rot.x = quarter * 90.0f;
	      imod->view->rot.y = 0.0f;
	      imod->view->rot.z = 0.0f;
	 }

    imodvDraw(Imodv);
}
static void moveY_cb(Widget w, XtPointer client_data, XtPointer call)
{
    int quarter = (int) client_data;
    int m, nm;
    Imod *imod = Imodv->imod;
    if (!imod)
          return;
    imod->view->rot.x = 0.0f;
    imod->view->rot.y = quarter * 90.0f;
    imod->view->rot.z = 0.0f;
    if (Imodv->moveall)
	 for (m = 0; m < Imodv->nm; m++) {
	      imod = Imodv->mod[m];
	      imod->view->rot.x = 0.0f;
	      imod->view->rot.y = quarter * 90.0f;
	      imod->view->rot.z = 0.0f;
	 }

    imodvDraw(Imodv);
}
static void moveZ_cb(Widget w, XtPointer client_data, XtPointer call)
{
    int quarter = (int) client_data;
    int m, nm;
    Imod *imod = Imodv->imod;
    if (!imod)
          return;
    imod->view->rot.x = -90.0f;
    imod->view->rot.y = 0.0f;
    imod->view->rot.z = quarter * 90.0f;
    if (Imodv->moveall)
	 for (m = 0; m < Imodv->nm; m++) {
	      imod = Imodv->mod[m];
	      imod->view->rot.x = -90.0f;
	      imod->view->rot.y = 0.0f;
	      imod->view->rot.z = quarter * 90.0f;
	 }

    imodvDraw(Imodv);
}

static void mkMove_cb(Widget parent, XtPointer client, XtPointer call)
{
     ObjectEditField *oef = (ObjectEditField *)call;
     ImodvApp *a = (ImodvApp *)client;
     Widget srow, row, pb;

     oef->control = row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, parent,
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);
     
     pb = XtVaCreateManagedWidget
	  ("Center on Object", xmPushButtonWidgetClass, row, NULL);
     XtAddCallback(pb, XmNactivateCallback, moveCenter_cb, 0);

     pb = XtVaCreateManagedWidget
	       ("Move by rotating around:", xmLabelWidgetClass, row, NULL);

     srow = XtVaCreateManagedWidget
          ("rowcol", xmRowColumnWidgetClass, row,
           XmNorientation, XmVERTICAL,
	   XmNnumColumns, 3,
	   XmNpacking, XmPACK_COLUMN,
	   XmNentryAlignment, XmALIGNMENT_CENTER,
	   NULL);
     pb = XtVaCreateManagedWidget ("X", xmLabelWidgetClass, srow, NULL);
     pb = XtVaCreateManagedWidget
	 ("Top", xmPushButtonWidgetClass, srow, NULL);
     XtAddCallback(pb, XmNactivateCallback, moveX_cb, (XtPointer)0);
     pb = XtVaCreateManagedWidget
         ("Front", xmPushButtonWidgetClass, srow, NULL);
     XtAddCallback(pb, XmNactivateCallback, moveX_cb, (XtPointer)-1);
     pb = XtVaCreateManagedWidget
         ("Bottom", xmPushButtonWidgetClass, srow, NULL);
     XtAddCallback(pb, XmNactivateCallback, moveX_cb, (XtPointer)2);
     pb = XtVaCreateManagedWidget
         ("Back", xmPushButtonWidgetClass, srow, NULL);
     XtAddCallback(pb, XmNactivateCallback, moveX_cb, (XtPointer)1);

     pb = XtVaCreateManagedWidget ("Y", xmLabelWidgetClass, srow, NULL);
     pb = XtVaCreateManagedWidget
	 ("Top", xmPushButtonWidgetClass, srow, NULL);
     XtAddCallback(pb, XmNactivateCallback, moveY_cb, (XtPointer)0);
     pb = XtVaCreateManagedWidget
         ("Left", xmPushButtonWidgetClass, srow, NULL);
     XtAddCallback(pb, XmNactivateCallback, moveY_cb, (XtPointer)1);
     pb = XtVaCreateManagedWidget
         ("Bottom", xmPushButtonWidgetClass, srow, NULL);
     XtAddCallback(pb, XmNactivateCallback, moveY_cb, (XtPointer)2);
     pb = XtVaCreateManagedWidget
         ("Right", xmPushButtonWidgetClass, srow, NULL);
     XtAddCallback(pb, XmNactivateCallback, moveY_cb, (XtPointer)-1);

     pb = XtVaCreateManagedWidget ("Z", xmLabelWidgetClass, srow, NULL);
     pb = XtVaCreateManagedWidget
         ("Front", xmPushButtonWidgetClass, srow, NULL);
     XtAddCallback(pb, XmNactivateCallback, moveZ_cb, (XtPointer)0);
     pb = XtVaCreateManagedWidget
         ("Left", xmPushButtonWidgetClass, srow, NULL);
     XtAddCallback(pb, XmNactivateCallback, moveZ_cb, (XtPointer)1);
     pb = XtVaCreateManagedWidget
         ("Back", xmPushButtonWidgetClass, srow, NULL);
     XtAddCallback(pb, XmNactivateCallback, moveZ_cb, (XtPointer)2);
     pb = XtVaCreateManagedWidget
         ("Right", xmPushButtonWidgetClass, srow, NULL);
     XtAddCallback(pb, XmNactivateCallback, moveZ_cb, (XtPointer)-1);
}	  

/*****************************************************************************
 * The subsets edit field
 *****************************************************************************/
static void subset_cb(Widget w, XtPointer client_data, XtPointer call)
{
     Imodv->current_subset = (int)client_data;
     imodvDraw(Imodv);
}

static void mkSubsets_cb(Widget parent, XtPointer client, XtPointer call)
{
     ObjectEditField *oef = (ObjectEditField *)call;
     ImodvApp *a = (ImodvApp *)client;
     Widget radio, button, row, pb;

     oef->control = row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, parent,
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNrightAttachment, XmATTACH_FORM,
	   NULL);
     
     pb = XtVaCreateManagedWidget
	       ("Show subset of model:", xmLabelWidgetClass, row, NULL);

     radio = XmCreateRadioBox(row, "radio", NULL, 0);

     button   = XtVaCreateManagedWidget
	  ("Show All ON Objects", xmToggleButtonWidgetClass, radio, NULL);
     XmToggleButtonSetState(button, (Imodv->current_subset == 0), False);
     XtAddCallback(button, XmNvalueChangedCallback, 
		   subset_cb, (XtPointer)0);

     button   = XtVaCreateManagedWidget
	  ("Current Object Only", xmToggleButtonWidgetClass, radio, NULL);
     XmToggleButtonSetState(button, (Imodv->current_subset == 1), False);
     XtAddCallback(button, XmNvalueChangedCallback, 
		   subset_cb, (XtPointer)1);

     button   = XtVaCreateManagedWidget
	  ("Current Surface Only", xmToggleButtonWidgetClass, radio, NULL);
     XmToggleButtonSetState(button, (Imodv->current_subset == 2), False);
     XtAddCallback(button, XmNvalueChangedCallback, 
		   subset_cb, (XtPointer)2);

     button   = XtVaCreateManagedWidget
	  ("Surface & Other Objects", xmToggleButtonWidgetClass, radio, NULL);
     XmToggleButtonSetState(button, (Imodv->current_subset == 3), False);
     XtAddCallback(button, XmNvalueChangedCallback, 
		   subset_cb, (XtPointer)3);

     button   = XtVaCreateManagedWidget
	  ("Current Contour Only", xmToggleButtonWidgetClass, radio, NULL);
     XmToggleButtonSetState(button, (Imodv->current_subset == 4), False);
     XtAddCallback(button, XmNvalueChangedCallback, 
		   subset_cb, (XtPointer)4);

     button   = XtVaCreateManagedWidget
	  ("Contour & Other Objects", xmToggleButtonWidgetClass, radio, NULL);
     XmToggleButtonSetState(button, (Imodv->current_subset == 5), False);
     XtAddCallback(button, XmNvalueChangedCallback, 
		   subset_cb, (XtPointer)5);

     XtManageChild(radio);
}

/*****************************************************************************/
/**                      EDIT FIELD CONTROLS.                               **/
/*****************************************************************************/

ObjectEditField objectEditFieldData[]    = {
     {"Line Color", mkLineColor_cb, NULL, setLineColor_cb, NULL},
     {"Fill Color", mkFillColor_cb, NULL, setFillColor_cb, NULL},
     {"Points",     mkPoints_cb,    NULL, setPoints_cb,    NULL},
     {"Lines",      mkLines_cb,     NULL, setLines_cb,     NULL},
#ifdef OBJ_MKMESH
     {"Mesh Make",  mkMesh_cb,      NULL, setMesh_cb,      NULL},
#endif
     {"Mesh View",  mkScalar_cb,    NULL, setScalar_cb,    NULL},
     {"Clip",       mkClip_cb,      NULL, setClip_cb,      NULL},
     {"Material",   mkMaterial_cb,  NULL, setMaterial_cb,  NULL},
/*     {"Light",      mkLight_cb,     NULL, setLight_cb,     NULL}, */
     {"Move",       mkMove_cb,      NULL, NULL,            NULL},
     {"Subsets",    mkSubsets_cb,      NULL, NULL,            NULL},

     NULL,
};

/* Handles actions on the Edit Field list. */
static void list_cb(Widget w, XtPointer client, XtPointer call)
{
     XmListCallbackStruct *cbs = (XmListCallbackStruct *)call;
     ObjectEditField *oef;
     Dimension width, height, border, diwidth, diheight;
     XtWidgetGeometry size;

     CurrentObjectField =  cbs->item_position - 1;
     oef = &objectEditFieldData[CurrentObjectField];

     /* Get the size of the current field before unmanaging it */
     width = 0;
     if ( CurrentObjectFieldWidget) {
           XtVaGetValues(CurrentObjectFieldWidget, 
			 XmNwidth, &width, XmNheight, &height, NULL);
	   XtUnmanageChild(CurrentObjectFieldWidget);
	 /*  printf("%d %d %d\n", CurrentObjectField, width, height); */
     }

     if (oef->control){
          XtManageChild(oef->control);

	  /* Find out what size the new field would like to be, and if it's
	     bigger than the current size, resize the dialog box by the
	     difference in sizes */
	  size.request_mode = CWHeight | CWWidth | CWBorderWidth;
	  XtQueryGeometry(oef->control, NULL, &size);
	  if (width && (size.width > width || size.height > height)) {
	       XtVaGetValues(Imodv_dialog, 
			     XmNwidth, &diwidth, XmNheight, &diheight, 
			     XmNborderWidth, &border, NULL);
	       if (size.width > width)
		    diwidth += size.width - width;
	       if (size.height > height)
		    diheight += size.height - height;
	       XtResizeWidget(Imodv_dialog, diwidth, diheight, border);
	  }
          XRaiseWindow(XtDisplay(oef->control), XtWindow(oef->control));
     }

     CurrentObjectFieldWidget = 
	  objectEditFieldData[CurrentObjectField].control;

     if (objectEditFieldData[CurrentObjectField].setwidget)
	  objectEditFieldData[CurrentObjectField].setwidget();

     return;
}

/* Create all the edit field stuff. */
static Widget makeEditField(ImodvApp *a, Widget parent, Widget attach)
{
     Arg args[7]; int n = 0;
     int i;
     XmString str;
     Widget form, list, frame;
     Widget frameForm;
     int listCount = 9;

     /* A previous solution to sizing problems was to set listCount 14 for sgi,
	15 for others, to control the lower frame size,
	and provide enough space for all edit fields */

     XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
     XtSetArg(args[n], XmNbottomAttachment, XmATTACH_FORM); n++;
     XtSetArg(args[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
     XtSetArg(args[n], XmNtopWidget, attach); n++;
     XtSetArg(args[n], XmNvisibleItemCount, listCount); n++;
     XtSetArg(args[n], XmNvalue, 1); n++;
     XtSetArg(args[n], XmNselectionPolicy, XmSINGLE_SELECT); n++;
     list = XmCreateScrolledList(parent, "EditList", args, n);

     for(i = 0; (objectEditFieldData[i].label); i++){
          str = XmStringCreateSimple(objectEditFieldData[i].label);
          XmListAddItem(list, str, i+1);
          XmStringFree(str);
     }
     XtAddCallback(list, XmNsingleSelectionCallback, list_cb, (XtPointer)a);
     XtManageChild(list);

     frame = XtVaCreateWidget
	  ("frame", xmFrameWidgetClass, parent,
	   XmNleftAttachment, XmATTACH_WIDGET,
	   XmNleftWidget, list,
	   XmNtopAttachment, XmATTACH_WIDGET,
	   XmNtopWidget, attach,
	   XmNrightAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNshadowType, XmSHADOW_IN,
	   NULL);
     form = XtVaCreateWidget
          ("form", xmFormWidgetClass, frame, NULL);

     for(i = 0; (objectEditFieldData[i].label); i++){
          if (objectEditFieldData[i].mkwidget)
               objectEditFieldData[i].mkwidget
                    (form, objectEditFieldData[i].clientData, 
		     &objectEditFieldData[i]);

          if (objectEditFieldData[i].control){
               XtSetMappedWhenManaged(objectEditFieldData[i].control, True);
               XtUnmanageChild( objectEditFieldData[i].control);
          }
     }

    /* CurrentObjectField = 0; */
     if (objectEditFieldData[CurrentObjectField].control)
	  XtManageChild(objectEditFieldData[CurrentObjectField].control);
     
     CurrentObjectFieldWidget = objectEditFieldData
	  [CurrentObjectField].control;
     

     XtManageChild(form); 
     XtManageChild(frame);

     return(list);
}


/*****************************************************************************/
/**                      DONE WITH EDIT FIELD DATA.                         **/
/*****************************************************************************/
/*****************************************************************************/


/*****************************************************************************/
/*****************************************************************************/
/**                       Default Edit Controls.                            **/
/*****************************************************************************/

/******************************************************************
 * Draw option
 ******************************************************************/

Widget wDrawData[4];

static void setDrawData(void)
{
     Iobj *obj = objedObject();
     int wi = 1;

     if (!obj) return;

     if (!iobjOff(obj->flags)){
	  if (iobjMesh(obj->flags)){
	       wi = 3;
	  }else{
	       wi = 2;
	  }
     }
     XtVaSetValues(wDrawData[0], XmNmenuHistory, wDrawData[wi], NULL);
}

static UINT onTestFlags, offTestFlags, passSetFlags, passClearFlags;
static UINT failSetFlags, failClearFlags;
static void optionSetFlags (UINT *flag)
{
     if (( (*flag & onTestFlags) || !onTestFlags) && 
	 ( !(*flag & offTestFlags) || !offTestFlags)) {
          *flag = (*flag | passSetFlags) & ~passClearFlags;
     }else {
          *flag = (*flag | failSetFlags) & ~failClearFlags;
     }
}          

static void drawdata_cb(Widget w, XtPointer client, XtPointer call)
{
     int ob, m;
     int option = (int)client;
     
     setEachAll();
     switch(option){
	case 1:
	  setObjFlag(IMOD_OBJFLAG_OFF, True);
	  break;
/*	case 2:
	  setObjFlag(IMOD_OBJFLAG_OFF | IMOD_OBJFLAG_MESH, False);
	  break;
	case 3:
	  setObjFlag(IMOD_OBJFLAG_OFF, False);
	  setObjFlag(IMOD_OBJFLAG_MESH, True);
	  break;
*/
	case 2:
	case 3:
	  if (option == 2) {
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
			      if (Imodv_objed_all == editAll - 1 ||
				  !(Imodv->mod[m]->obj[ob].flags & 
				    IMOD_OBJFLAG_OFF))
				 optionSetFlags(&Imodv->mod[m]->obj[ob].flags);
	       }else{
	            for(ob = 0; ob < Imodv->imod->objsize; ob++)
		         if (Imodv_objed_all == editAll - 1 ||
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

static Widget makeDrawData(Widget parent)
{
     Widget menuWidget, optionWidget;
     Arg args[4];
     int n = 0;

     XtSetArg(args[n], XmNvisual, Imodv->visual); n++;
     menuWidget = XmCreatePulldownMenu(parent, "pulldown", args, n);

     XtSetArg(args[n], XmNsubMenuId, menuWidget); n++;

     wDrawData[0] = XmCreateOptionMenu
	  (parent, "option", args, n);
     
     wDrawData[1] = XtVaCreateManagedWidget
	  ("Off", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wDrawData[1], XmNactivateCallback, 
		   drawdata_cb, (XtPointer)1);   

     wDrawData[2] =XtVaCreateManagedWidget
	  ("Contour", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wDrawData[2], XmNactivateCallback, 
		   drawdata_cb, (XtPointer)2);

     wDrawData[3] =XtVaCreateManagedWidget
	  ("Mesh", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wDrawData[3], XmNactivateCallback, 
		   drawdata_cb, (XtPointer)3);
	  
     XtManageChild(wDrawData[0]);
     setDrawData();
     return(wDrawData[0]);
}


/******************************************************************
 * Style option
 ******************************************************************/

Widget wStyleData[6];

enum { styleOption, stylePoints, styleLines, styleFill , styleFillOutline};

static void setStyleData(void)
{
     Iobj *obj = objedObject();
     int wi = 1;
     unsigned int flag;

     if (!obj) return;
     flag = obj->flags;

     if ( iobjFill(flag) ){
	  wi = styleFillOutline;
	  if (flag & IMOD_OBJFLAG_LINE)
	       wi = styleFill;
     }else{
	  if (IMOD_OBJFLAG_LINE & flag)
	       wi = stylePoints;
	  else
	       wi = styleLines;
     }
/*
     if (flag & IMOD_OBJFLAG_3DPOINT)
	  wi = stylePoints;
     else if ( (flag & IMOD_OBJFLAG_LINE)  && (iobjFill(flag)))
	  wi = styleFillOutline;
     else if ( iobjFill(flag) )
	  wi = styleFill;
     else
	  wi = styleFillOutline;
*/
     XtVaSetValues(wStyleData[styleOption], 
		   XmNmenuHistory, wStyleData[wi], NULL);
}

static void styleData_cb(Widget w, XtPointer client, XtPointer call)
{
     int option = (int)client;

     setEachAll();
     switch(option){
	case stylePoints:
	  setObjFlag(IMOD_OBJFLAG_LINE , True);
	  setObjFlag(IMOD_OBJFLAG_FILL, False);
	  break;
	case styleLines:
	  setObjFlag(IMOD_OBJFLAG_FILL | IMOD_OBJFLAG_LINE, False);
	  break;
	case styleFill:
	  setObjFlag(IMOD_OBJFLAG_FILL | IMOD_OBJFLAG_LINE, True);
	  break;
	case styleFillOutline:
	  setObjFlag(IMOD_OBJFLAG_LINE, False);
	  setObjFlag(IMOD_OBJFLAG_FILL, True);
	  break;
	default:
	  break;
     }
     imodvDraw(Imodv);
}

static Widget makeStyleData(Widget parent)
{
     Widget menuWidget, optionWidget;
     Arg args[4];
     int n = 0;

     XtSetArg(args[n], XmNvisual, Imodv->visual); n++;
     menuWidget = XmCreatePulldownMenu(parent, "pulldown", args, n);

     XtSetArg(args[n], XmNsubMenuId, menuWidget); n++;

     wStyleData[styleOption] = XmCreateOptionMenu
	  (parent, "option", args, n);
     
     wStyleData[stylePoints] = XtVaCreateManagedWidget
	  ("Points", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wStyleData[stylePoints], XmNactivateCallback, 
		   styleData_cb, (XtPointer)stylePoints);   

     wStyleData[styleLines] = XtVaCreateManagedWidget
	  ("Lines", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wStyleData[styleLines], XmNactivateCallback, 
		   styleData_cb, (XtPointer)styleLines);   

     wStyleData[styleFill] = XtVaCreateManagedWidget
	  ("Fill", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wStyleData[styleFill], XmNactivateCallback, 
		   styleData_cb, (XtPointer)styleFill);   

     wStyleData[styleFillOutline] = XtVaCreateManagedWidget
	  ("Fill Outline", xmPushButtonWidgetClass, menuWidget, NULL);
     XtAddCallback( wStyleData[styleFillOutline], XmNactivateCallback, 
		   styleData_cb, (XtPointer)styleFillOutline);   
     
	  
     XtManageChild(wStyleData[styleOption]);
     setStyleData();
     return(wStyleData[styleOption]);
}

/******************************************************************
 * Lighting option
 ******************************************************************/

static Widget wLightData[4];

enum { lightOption, lightFlat, lightDepth, lightGo};

static void setLightData(void)
{
     Iobj *obj = objedObject();
     int wi = 1;
     unsigned int flag;

     if (!obj) return;
     flag = obj->flags;

     if (IMOD_OBJFLAG_LIGHT & flag)
	  wi = lightGo;
     else if (IMOD_OBJFLAG_DCUE & flag)
	  wi = lightDepth;
     else
	  wi = lightFlat;

     XtVaSetValues(wLightData[lightOption], 
		   XmNmenuHistory, wLightData[wi], NULL);

     imodvDraw(Imodv);
}

static void lightData_cb(Widget w, XtPointer client, XtPointer call)
{
     int option = (int)client;

     setEachAll();
     switch(option){
	case lightFlat:
	  setObjFlag(IMOD_OBJFLAG_LIGHT | IMOD_OBJFLAG_DCUE, False);
	  break;
	case lightDepth:
	  setObjFlag(IMOD_OBJFLAG_LIGHT, False);
	  setObjFlag(IMOD_OBJFLAG_DCUE,  True);
	  break;
	case lightGo:
	  setObjFlag(IMOD_OBJFLAG_DCUE, False);
	  setObjFlag(IMOD_OBJFLAG_LIGHT,  True);
	  break;

	default:
	  break;
     }
     imodvDraw(Imodv);
}

static Widget makeLightData(Widget parent)
{
     Widget menuWidget, optionWidget;
     Arg args[4];
     int n = 0;

     XtSetArg(args[n], XmNvisual, Imodv->visual); n++;
     menuWidget = XmCreatePulldownMenu(parent, "pulldown", args, n);

     XtSetArg(args[n], XmNsubMenuId, menuWidget); n++;

     wLightData[lightOption] = XmCreateOptionMenu
	  (parent, "option", args, n);
     
     wLightData[lightFlat] = XtVaCreateManagedWidget
	  ("Flat", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wLightData[lightFlat], XmNactivateCallback, 
		   lightData_cb, (XtPointer)lightFlat);   

     wLightData[lightDepth] = XtVaCreateManagedWidget
	  ("Depth Cue", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wLightData[lightDepth], XmNactivateCallback, 
		   lightData_cb, (XtPointer)lightDepth);

     wLightData[lightGo] = XtVaCreateManagedWidget
	  ("Lighting", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wLightData[lightGo], XmNactivateCallback, 
		   lightData_cb, (XtPointer)lightGo);
	  
     XtManageChild(wLightData[lightOption]);
     setLightData();
     return(wLightData[lightOption]);
}


/******************************************************************
 * Edit Each/All option
 ******************************************************************/

static Widget wEditData[4];
static int editDataValue = 1;

static int editData(void)
{
     return(editDataValue - 1);
}

static void editData_cb(Widget w, XtPointer client, XtPointer call)
{
     int option = (int)client;

     editDataValue = option;
     Imodv_objed_all = option - 1;
}

static Widget makeEditData(Widget parent)
{
     Widget menuWidget, optionWidget;
     Arg args[4];
     int n = 0;

     XtSetArg(args[n], XmNvisual, Imodv->visual); n++;
     menuWidget = XmCreatePulldownMenu(parent, "pulldown", args, n);

     XtSetArg(args[n], XmNsubMenuId, menuWidget); n++;

     wEditData[editOption] = XmCreateOptionMenu
	  (parent, "option", args, n);
     
     wEditData[editEach] = XtVaCreateManagedWidget
	  ("Each", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wEditData[editEach], XmNactivateCallback, 
		   editData_cb, (XtPointer)editEach);   

     wEditData[editAll] = XtVaCreateManagedWidget
	  ("All", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wEditData[editAll], XmNactivateCallback, 
		   editData_cb, (XtPointer)editAll);

     wEditData[editOnOnes] = XtVaCreateManagedWidget
	  ("On's", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wEditData[editOnOnes], XmNactivateCallback, 
		   editData_cb, (XtPointer)editOnOnes);

     XtManageChild(wEditData[editOption]);

     XtVaSetValues(wEditData[editOption],
		   XmNmenuHistory, wEditData[editDataValue], NULL);

     return(wEditData[editOption]);
}


/*****************************************************************************/
/**                   DONE WITH DEFAULT EDIT CONTROLS                       **/
/*****************************************************************************/
/*****************************************************************************/


/******************************************************************
 *  Global variables.
 *******************************************************************/

static void imodv_dia_objed(void);
static Widget imodv_objed_material(Widget w);
Display *Objed_display;
Screen  *Objed_screen;
int    Objed_argc;  /* set to argc from main */
char **Objed_argv;  /* set to argv from main */
struct Mod_Model *Objed_model;  /* set to model to edit */
int    Objed_ob = 0;
int    Objed_id = 0;
Widget Objed_toplevel;
Widget Objed_number;
Widget Objed_select;
Widget Objed_toggle_draw;
Widget Objed_toggle_line;
Widget Objed_toggle_fill;
Widget Objed_toggle_mesh;
Widget Objed_toggle_dcue;


XColor Objed_color;
Widget Objed_colorbox;

Widget Objed_objname;
Widget Objed_objnumber;


/* material widgets */
static Widget Objed_material;
static int Objed_managed = 1;
static int Objed_Done = 0;

static int ObjedAllocColor = False;
static Colormap ObjedCmap;
static Dimension Objed_dialogx = 0;
static Dimension Objed_dialogy = 0;

#define MAX_ONOFF_BUTTONS  48
static Widget OnoffButtons[MAX_ONOFF_BUTTONS];
static int numOnoffButtons = 0;

#define MAX_OOLIST_BUTTONS  256
static Widget OolistButtons[MAX_OOLIST_BUTTONS];
static int numOolistButtons = 0;
static diaDialog *Oolist_dialog = NULL;

static void objed_quit(Widget w, XtPointer client, XtPointer call);

int object_edit(struct Mod_Model *mod)
{
     Objed_model = mod;
     objed(Imodv);
     return(0);
}

int object_edit_kill(void)
{
     if (Imodv_dialog){
	  objed_quit(Imodv_dialog, (XtPointer)Imodv_dialog, (XtPointer)0);
	  return(1);
     }
     return(0);
}

static void setOnoffButtons(void)
{
     int ob;
     Boolean state;
     for (ob = 0; ob < numOnoffButtons; ob++) {
          if (ob < Imodv->imod->objsize)
	       state = !(Imodv->imod->obj[ob].flags & IMOD_OBJFLAG_OFF);
	  else
	       state = False;
	  XmToggleButtonSetState(OnoffButtons[ob], state, False);
     }

     for (ob = 0; ob < numOolistButtons; ob++) {
          if (ob < Imodv->imod->objsize)
	       state = !(Imodv->imod->obj[ob].flags & IMOD_OBJFLAG_OFF);
	  else
	       state = False;
	  XmToggleButtonSetState(OolistButtons[ob], state, False);
     }
}

/* DNM 2/7/01: call setOnoffButtons before skipping out if no window; it should
   be safe even if neither list or objed window is open */
void imodvObjedNewView(void)
{
     setOnoffButtons();
     if (!Imodv_dialog)
	  return;
     objset(Imodv);
}




static void objed_map_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     XtVaSetValues(w, XmNx, Objed_dialogx, XmNy, Objed_dialogy, NULL);
     return;
}

static void objed_help(Widget w, XtPointer client_data, XtPointer call_data)
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



/* Picks a new object */ /* old */
static void obj_scale_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
     Imodv->ob = cbs->value - 1;
     objset(Imodv);
     return;
}

static void imodv_allset_cb(Widget w, XtPointer client_data, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     /* int mode = (int)client_data; */
     
    /* if (!cbs->set)
	  return;
     */
/*     if (mode)*/
     if (!Imodv_objed_all)
	  Imodv_objed_all = TRUE;
     else
	  Imodv_objed_all = FALSE;
     return;
}



static void newobj_cb(Widget w, XtPointer client, XtPointer call)
{
     int obo = (int)client;

     if (!Imodv->imod)
	  return;
     if (Imodv->imod->objsize == 1)
          return;


     if (obo == 1){
	  Imodv->ob++;
	  if (Imodv->ob >= Imodv->imod->objsize)
	       Imodv->ob = 0;
     } else {

          Imodv->ob--;
          if (Imodv->ob < 0)
	       Imodv->ob = Imodv->imod->objsize - 1;
     }
     objset(Imodv);
     return;
}



static void objed_mat_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
     int offset = (int)client_data;
     unsigned char *item;

     int ob,m;

     if (!Imodv->imod) return;
     setEachAll();
     
     if (Imodv_objed_all){
	  for(ob = 0; ob < Imodv->imod->objsize; ob++)
	       if (Imodv_objed_all == editAll - 1 ||
		   !(Imodv->imod->obj[ob].flags & IMOD_OBJFLAG_OFF)) {
		    item = (unsigned char *) &(Imodv->imod->obj[ob]);
		    item += offset;
		    *item = (unsigned char)cbs->value;
	       }
	  if (Imodv->crosset)
	       for(m = 0; m < Imodv->nm; m++)
		    for(ob = 0; ob < Imodv->mod[m]->objsize; ob++)
		         if (Imodv_objed_all == editAll - 1 ||
			     !(Imodv->mod[m]->obj[ob].flags & 
			       IMOD_OBJFLAG_OFF)) {
			      item = (unsigned char *) &(Imodv->mod[m]->obj[ob]);
			      item += offset;
			      *item = (unsigned char)cbs->value;
			 }
     }else{
	  item = (unsigned char *) Imodv->obj;
	  item += offset;
	  *item = (unsigned char)cbs->value;
	  if (Imodv->crosset)
	       for(m = 0; m < Imodv->nm; m++)
		    if (Imodv->mod[m]->objsize > Imodv->ob){
			 item = (unsigned char *) &(Imodv->mod[m]->obj[ob]);
			 item += offset;
			 *item = (unsigned char)cbs->value;
		    }
     }
/*     printf("set mat %d, offset %d, value%d\n", *item, offset, cbs->value); */
     
     imodvDraw(Imodv);
     return;
}

static void  obj_colorcb(Widget w, XtPointer client_data, XtPointer call_data)
{
     int red, green, blue, m;
     static int managed = 0;
/*     return; */

     if (!Imodv->imod)
	  return;

     red = Imodv->obj->red * 255;
     green = Imodv->obj->green * 255;
     blue = Imodv->obj->blue * 255;
     dia_color(&red, &green, &blue);
     Imodv->obj->red = red / 255.0;
     Imodv->obj->green = green / 255.0;
     Imodv->obj->blue = blue / 255.0;

     if (Imodv->crosset)
	  for(m = 0; m < Imodv->nm; m++)
	       if (Imodv->mod[m]->objsize > Imodv->ob){
		    Imodv->mod[m]->obj[Imodv->ob].red   = red   / 255.0;
		    Imodv->mod[m]->obj[Imodv->ob].green = green / 255.0;
		    Imodv->mod[m]->obj[Imodv->ob].blue  = blue  / 255.0;
	       }
     imodvDraw(Imodv);
     return;
}

static void obj_all_line(Widget w, XtPointer client_data, XtPointer call_data)
{
     int i;

     if (!Imodv->imod)
	  return;

     for(i = 0; i < Imodv->imod->objsize; i++){
	  if (client_data)
	       Imodv->imod->obj[i].flags &= ~IMOD_OBJFLAG_OFF;
	  else
	        Imodv->imod->obj[i].flags |= IMOD_OBJFLAG_OFF;
     }
     imodvDraw(Imodv);
     return;
}


static void objName_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     int i, mi;
     Iobj *obj = objedObject();
     char *name = NULL;
     if (!obj) return;
     name = XmTextGetString(w);
     
     mi = strlen(name);
     if (mi >= IOBJ_STRSIZE)
	  mi = IOBJ_STRSIZE - 1;
     for(i = 0 ; i<mi; i++)
	  obj->name[i] = name[i];
     obj->name[i] = 0x00;
     XtFree(name);
}

/****************************************************************************/
/* Object Edit creation and update functions.                               */

/* sets object features in objed window. */
static void objset(ImodvApp *a)
{
     unsigned short red, green, blue;
     unsigned long pixel;
     Status status;
/*     Colormap cmap = DefaultColormapOfScreen(XtScreen (Imodv_dialog)); */
     XmString objname;
     Iobj *obj;
     char nobjst[32];
     static int first = 1;


     if (a->ob >= a->imod->objsize)
	  a->ob = 0;
     a->obj = &(a->imod->obj[a->ob]);
     obj = a->obj;

/* DNM: before the top row was made a pane, it got bigger on the SGI unless
   numbers up to 9 were output %5d, 10-99 output %4d, >99 %d. */
     sprintf(nobjst, "Object #%4d  ", a->ob + 1);
     objname = XmStringCreateSimple(nobjst);
     XtVaSetValues(Objed_objnumber, XmNlabelString, objname, NULL);
     XmStringFree(objname);
     if (Objed_select){
	 if (a->imod->objsize <= 1){
	     XtSetSensitive(Objed_select, False);
	 }else{
	     XtVaSetValues(Objed_select, XmNvalue, a->ob+1,
			   XmNmaximum, a->imod->objsize,
			   NULL);
	     XtSetSensitive(Objed_select, True);
	 }
     }
     XtVaSetValues(Objed_objname, XmNvalue, obj->name, NULL);

     setDrawData();
     setStyleData();
/*     setLightData(); */

     if (objectEditFieldData[CurrentObjectField].setwidget)
	  objectEditFieldData[CurrentObjectField].setwidget();

     /* Set object color box */
     red   = 255 * obj->red;
     green = 255 * obj->green;
     blue  = 255 * obj->blue;
     Objed_color.flags = DoRed | DoBlue | DoGreen;

     Objed_color.red   = (red   << 8);
     Objed_color.green = (green << 8);
     Objed_color.blue  = (blue  << 8);

     if (ObjedAllocColor){
	  XFreeColors(XtDisplay(Imodv_dialog), ObjedCmap, 
		      &Objed_color.pixel, 1, 0);  
	  ObjedAllocColor = False;
     }
     if (!XAllocColor(XtDisplay(Imodv_dialog), ObjedCmap, &Objed_color))
	  return;
     ObjedAllocColor = True;
     if (Objed_colorbox)
	  XtVaSetValues(Objed_colorbox, 
			XmNbackground, Objed_color.pixel, 
			NULL);
     return;
}

static void objed_colordrag_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     int color = (int)client;

     switch(color){
	case 0:
          Objed_color.red   = (cbs->value   << 8);
	  break;
	case 1:
          Objed_color.green   = (cbs->value   << 8);
	  break;
	case 2:
          Objed_color.blue   = (cbs->value   << 8);
	  break;
	}

     if (ObjedAllocColor){
	  XFreeColors(XtDisplay(Imodv_dialog), ObjedCmap, 
		      &Objed_color.pixel, 1, 0);  
	  ObjedAllocColor = False;
     }
     if (!XAllocColor(XtDisplay(Imodv_dialog), ObjedCmap, &Objed_color))
	  return;
     ObjedAllocColor = True;
     if (Objed_colorbox)
	  XtVaSetValues(Objed_colorbox, 
			XmNbackground, Objed_color.pixel, 
			NULL);
     return;
}

static void toggleobj_cb(Widget w, XtPointer client_data, XtPointer call)
{
     int  m;
     Boolean objButton = XmToggleButtonGetState(w);
     int ob = (int)client_data;
     if (objButton) {
          Imodv->imod->obj[ob].flags &= ~IMOD_OBJFLAG_OFF;
	  Imodv->ob = ob;
     } else
          Imodv->imod->obj[ob].flags |= IMOD_OBJFLAG_OFF;

     /* Turn off same object in all other models if editing all and legal ob */
     if (Imodv->crosset)
          for (m = 0; m < Imodv->nm; m++)
	       if (Imodv->mod[m]->objsize > ob)
		    if (objButton)
		         Imodv->mod[m]->obj[ob].flags &= ~IMOD_OBJFLAG_OFF;
		    else
		         Imodv->mod[m]->obj[ob].flags |= IMOD_OBJFLAG_OFF;

     /* If the object is within legal limits for a button list, set that button
	in each list */
     if (ob < numOnoffButtons && ob < Imodv->imod->objsize)
	  XmToggleButtonSetState(OnoffButtons[ob], objButton, False);
     if (ob < numOolistButtons && ob < Imodv->imod->objsize)
	  XmToggleButtonSetState(OolistButtons[ob], objButton, False);

     if (Imodv_dialog)
	  objset(Imodv);
     imodvDraw(Imodv);
}

static void objed_select_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     Imodv->ob = cbs->value - 1;
     objset(Imodv);
}

static void objed_quit(Widget w, XtPointer client, XtPointer call)
{
     Widget shell = (Widget) client;
/*     Colormap cmap = DefaultColormapOfScreen(XtScreen (Imodv_dialog)); */

     if (ObjedAllocColor)
	  XFreeColors(XtDisplay(Imodv_dialog), ObjedCmap, 
		      &Objed_color.pixel, 1, 0);  

     XtVaGetValues(Imodv_dialog, 
		   XmNx, &Objed_dialogx, XmNy, &Objed_dialogy, NULL);
     XtDestroyWidget(shell);
     Imodv_dialog = NULL;
     numOnoffButtons = 0;
}

static void kill_object_edit(void)
{
     XtDestroyWidget(Imodv_dialog);
     Imodv_dialog = NULL;
     numOnoffButtons = 0;
}

/*****************************************************************************
 *
 * Create the Object Edit Dialog.
 * 
 *****************************************************************************/
void objed(ImodvApp *a)
{
     Widget dialog, pane, form;
     Widget shell, window;
     Widget obj_scale;
     Widget obj_draw;
     Widget colorbutton, donebutton;
     Widget row, linebutton;
     Widget radio, button;
     Widget arrow;
     Widget editForm, editFrame;
     Widget attach;
     XtAppContext context;
     Atom wmclose;
     char *cname = "Object_Edit";
     XEvent event_return;
     Dimension h;
     char *window_name;
     XmString objname;
     XtWidgetGeometry size;
     /* This provides an alternate way to keep lower frames large enough,
	namely always building with the first item on list then switching
	after popup */
  /*   int currentSave = CurrentObjectField;
     XmListCallbackStruct listcbs; */

     if (Imodv_dialog)
	  return;

     if (!a->imod)
	  return;

     Objed_color.red   = (int)(255 * a->obj->red) << 8;
     Objed_color.green = (int)(255 * a->obj->green) << 8;
     Objed_color.blue  = (int)(255 * a->obj->blue) << 8;

     if (DefaultDepthOfScreen(XtScreen(Dia_toplevel)) < 2)
	  return;

     window_name = imodwEithername("IMODV Objects: ", a->imod->fileName);
     dialog = XtVaCreatePopupShell
	  (Dia_title,  
	   xmDialogShellWidgetClass,   
	   Dia_toplevel,
	   XmNdefaultPosition, True,
	   XmNdeleteResponse, XmDESTROY,
	   XmNvisual,   a->visual,
	   XmNcolormap, a->cmap,
	   XmNdepth,    a->depth,
	   XmNtitle,    window_name,
	   NULL);
     Objed_managed = 1;
     Imodv_objed_all = FALSE;
     XtVaSetValues(dialog, XmNdefaultPosition, True, NULL);
     XtAddCallback(dialog, XmNpopupCallback, objed_map_cb, NULL);

     XtVaGetValues(dialog, XmNcolormap, &ObjedCmap, NULL);
     if (window_name)
          free(window_name);

     pane = XtVaCreateWidget
	  ("pane", xmPanedWindowWidgetClass, dialog,
	   XmNsashWidth,  1,
	   XmNsashHeight, 1,
	   NULL);
     
     /* DNM 3/10/00: made the top row be a separate pane with specified size
	to keep it from increasing in size unaccountably */

     row = XtVaCreateWidget
	  ("arrowcol", xmRowColumnWidgetClass, pane,
	   XmNorientation, XmHORIZONTAL, 
	   NULL);
     {
	  Objed_colorbox = XtVaCreateManagedWidget
	       ("colorwindow",  widgetClass, row,
		XmNheight, 25,
		XmNwidth, 25,
		XmNbackground, Objed_color.pixel,
		NULL);
	  arrow = XtVaCreateManagedWidget
	       ("arrow", xmArrowButtonWidgetClass, row,
		XmNarrowDirection, XmARROW_LEFT, NULL);
	  XtAddCallback(arrow, XmNarmCallback, newobj_cb, (XtPointer)-1);
	  arrow = XtVaCreateManagedWidget
	       ("arrow", xmArrowButtonWidgetClass, row,
		XmNarrowDirection, XmARROW_RIGHT, NULL);
	  XtAddCallback(arrow, XmNarmCallback, newobj_cb, (XtPointer)1);
	  
	  Objed_objnumber = XtVaCreateManagedWidget
	       ("Object Number", xmLabelWidgetClass, row, NULL);
	  objname = XmStringCreateSimple("Object #     8");
	  XtVaSetValues(Objed_objnumber, XmNlabelString, objname, NULL);
	  XmStringFree(objname);

	  makeEditData(row);
     }
     XtManageChild(row);

     /* Set the height of this pane permanently to it's current desired size
	so that it won't grow unaccountably when controls are used */
     size.request_mode = CWHeight;
     XtQueryGeometry(row, NULL, &size);
     XtVaSetValues(row,
		   XmNpaneMinimum, size.height,
		   XmNpaneMaximum, size.height,
		   NULL);

     /* Now make the rest of the workarea into one big form */
     form = XtVaCreateWidget 
	  ("form1", xmFormWidgetClass, pane, 
	   NULL);

     Objed_select =  XtVaCreateManagedWidget
	 ("Object_Select", xmScaleWidgetClass, form,
	  XmNrightAttachment,  XmATTACH_FORM,
	  XmNleftAttachment,  XmATTACH_FORM,
	  XmNtopAttachment, XmATTACH_WIDGET,
	  XmNtopWidget, row,
	  XmNminimum, 1,
	  XmNvalue, 1,
	  XmNmaximum, 2,
	  XmNscaleMultiple, 1,
	  XmNorientation, XmHORIZONTAL,
	  NULL);
     XtAddCallback(Objed_select, XmNdragCallback,
		   objed_select_cb, NULL);
     XtAddCallback(Objed_select, XmNvalueChangedCallback,
		   objed_select_cb, NULL);
     attach = Objed_select;
     if (a->imod->objsize <= 1){
	 XtSetSensitive(Objed_select, False);
     }

     Objed_objname = XtVaCreateManagedWidget
	  ("Object Name", xmTextWidgetClass, form, 
	   XmNrightAttachment,  XmATTACH_FORM,
	   XmNleftAttachment,  XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_WIDGET,
	   XmNtopWidget, attach,
	   NULL);
     XtAddCallback(Objed_objname, XmNactivateCallback,
		   objName_cb, NULL);

     row = XtVaCreateWidget
	  ("toprow", xmRowColumnWidgetClass, form,
	   XmNnumColumns, 6,
	   XmNorientation, XmVERTICAL, 
	   XmNtopAttachment,    XmATTACH_WIDGET,
	   XmNleftAttachment,  XmATTACH_FORM,
	   XmNrightAttachment,  XmATTACH_FORM,
	   XmNtopWidget, Objed_objname,
	   XmNpacking, XmPACK_COLUMN,
	   NULL);
     {
       int ob;
       char obnum[10];
       int state;

       numOnoffButtons = a->imod->objsize;
       if (numOnoffButtons > MAX_ONOFF_BUTTONS)
	    numOnoffButtons = MAX_ONOFF_BUTTONS;
       
       for (ob = 0; ob < numOnoffButtons; ob++) {
	 sprintf(obnum,"%d",ob + 1);
	 state = !(a->imod->obj[ob].flags & IMOD_OBJFLAG_OFF);
	 OnoffButtons[ob] = XtVaCreateManagedWidget
	   (obnum, xmToggleButtonWidgetClass, row, 
	    XmNmarginWidth, 2,
	    XmNmarginHeight, 0,
	    XmNset, state,
	    NULL);

	 XtAddCallback(OnoffButtons[ob], XmNvalueChangedCallback,
		   toggleobj_cb, (XtPointer)ob);
       }
     }
     attach = row;
     XtManageChild(row);


     row = XtVaCreateWidget
	  ("row", xmRowColumnWidgetClass, form, 
	   XmNpacking, XmPACK_COLUMN,
	   XmNnumColumns, 2,
	   XmNorientation, XmVERTICAL,
	   XmNrightAttachment,  XmATTACH_FORM,
	   XmNleftAttachment,  XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_WIDGET,
	   XmNtopWidget, attach,
	   NULL);
     {
	  XtVaCreateManagedWidget
	       ("Draw Data Type", xmLabelWidgetClass, row, NULL);
	  XtVaCreateManagedWidget
	       ("Drawing Style", xmLabelWidgetClass, row, NULL);
/*
	  XtVaCreateManagedWidget
	       ("Render Style", xmLabelWidgetClass, row, NULL);
*/
	  makeDrawData(row);
	  makeStyleData(row);
	/*  makeLightData(row); */
     }

     makeEditField(a, form, row);

     XtManageChild(row);

     XtManageChild (form);  


     form = XtVaCreateWidget ("form2", xmFormWidgetClass, pane,
			      XmNfractionBase,    11,
			      NULL);
     colorbutton = XtVaCreateManagedWidget
	("OK", xmPushButtonWidgetClass, form,
	 XmNtopAttachment,        XmATTACH_FORM,
	 XmNbottomAttachment,     XmATTACH_FORM,
	 XmNleftAttachment,       XmATTACH_POSITION,
	 XmNleftPosition,         1,
	 XmNrightAttachment,      XmATTACH_POSITION,
	 XmNrightPosition,        5,
	 XmNshowAsDefault,        True,
	 XmNdefaultButtonShadowThickness, 1,
	 NULL);
     XtAddCallback(colorbutton, XmNactivateCallback, objed_quit, dialog);     
     
     colorbutton = XtVaCreateManagedWidget
	  ("HELP", xmPushButtonWidgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         6,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        10,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(colorbutton, XmNactivateCallback, objed_help, 0);

     XtManageChild (form);
     XtVaGetValues (colorbutton, XmNheight, &h, NULL);
     XtVaSetValues (form, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
     XtManageChild (pane);
     
     wmclose = XmInternAtom( XtDisplay(dialog), "WM_DELETE_WINDOW", False); 
     XmAddWMProtocolCallback(dialog, wmclose, objed_quit, ( caddr_t )dialog); 
     
     Imodv_dialog = dialog;
     XtPopup (dialog, XtGrabNone);
     
     /* DNM: in the alternate method, after initializing the size management
	with the first item, which should be the biggest, now switch to the 
	last one that was up */
    /* if (currentSave) {
	  listcbs.item_position = currentSave + 1;
	  list_cb(NULL, 0, (XtPointer)&listcbs);
     } */

     ObjedAllocColor = False;
     objset(a);
     return;
}

/*****************************************************************************
 *
 * Create the Object list dialog
 * 
 *****************************************************************************/

#define MAX_LIST_NAME 48
#define MAX_LIST_IN_COL 36
static int oolist_name_limits[10] = {40, 25, 17, 13, 10, 8, 7, 6, 6, 6};
static Widget oolist_make_work_area(ImodvApp *a, Widget top)
{
     Widget frame, row;
     char   modelno[8];
     XmString str, s1, s2, s3, s4;
     int ob;
     char obnum[MAX_LIST_NAME], obname[MAX_LIST_NAME];
     int state;
     int len;
     int ncol;

     frame = XtVaCreateManagedWidget
	  ("frame", xmFrameWidgetClass, top, NULL);

     numOolistButtons = a->imod->objsize;
     if (numOolistButtons > MAX_OOLIST_BUTTONS)
	  numOolistButtons = MAX_OOLIST_BUTTONS;
       
     ncol = (numOolistButtons + MAX_LIST_IN_COL - 1) / MAX_LIST_IN_COL;

     row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, frame,
	   XmNorientation, XmVERTICAL, 
	   XmNpacking, XmPACK_COLUMN,
	   XmNnumColumns, ncol,
	   XmNspacing, 0,
	   NULL);


     for (ob = 0; ob < numOolistButtons; ob++) {
	  len = strlen(a->imod->obj[ob].name);
	  if (len > oolist_name_limits[ncol - 1])
	       len = oolist_name_limits[ncol - 1];
	  strncpy(obname, a->imod->obj[ob].name, len);
	  obname[len] = 0x00;
	  sprintf(obnum,"%d: %s",ob + 1, obname);
	  state = !(a->imod->obj[ob].flags & IMOD_OBJFLAG_OFF);
	  OolistButtons[ob] = XtVaCreateManagedWidget
	       (obnum, xmToggleButtonWidgetClass, row, 
		XmNmarginWidth, 0,
		XmNmarginHeight, 0,
		XmNset, state,
		NULL);

	  XtAddCallback(OolistButtons[ob], XmNvalueChangedCallback,
			toggleobj_cb, (XtPointer)ob);
     }

     return(frame);
}

static void oolist_workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodvApp *a = (ImodvApp *)client;
     XtManageChild(oolist_make_work_area(a, w));
     return;
}


static void oolist_done_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     diaDestroyDialog(dia);
     Oolist_dialog = NULL;
     numOolistButtons = 0;
     return;
}

void imodvObjectListDialog(ImodvApp *a, int state)
{
     if (!state){
	  if (Oolist_dialog)
	       oolist_done_cb(NULL, NULL, (XtPointer)Oolist_dialog);
	  return;
     }
     if (Oolist_dialog){
	  XRaiseWindow(a->display, XtWindow((Widget)Oolist_dialog->dialog));
	  return;
     }

     Oolist_dialog = diaVaCreateDialog
	  ("Imodv: Object List", a->topLevel, a->context,
	   DiaNcontrolButton, "Done", oolist_done_cb, (XtPointer)a,
	   DiaNworkAreaFunc, oolist_workarea_cb, (XtPointer)a,
	   DiaNwindowQuit, oolist_done_cb, (XtPointer)a,
	   0);
     return;
}



/*****************************************************************************/
/************************* internal utility functions. ***********************/
/*****************************************************************************/

static void setEachAll(void)
{
#ifdef SHIFT_EACH_ALL
     Window rootr, childr;
     int rx, ry, mx, my;
     unsigned int maskr;

     if ( editData()){
	  Imodv_objed_all = True;
	  return;
     }
     
     XQueryPointer(Imodv->display,
		   XtWindow(Imodv_dialog), &rootr, &childr,
		   &rx, &ry, &mx, &my, &maskr);

     Imodv_objed_all = (maskr & ShiftMask);
#endif
     return;
}

static void setObjFlag(long flag, int state)
{
     int ob, m;

     if (!Imodv->imod) return;

     if (Imodv_objed_all){
	  if (Imodv->crosset){
	       for(m = 0; m < Imodv->nm; m++)
		    for(ob = 0; ob < Imodv->mod[m]->objsize; ob++)
		         if (Imodv_objed_all == editAll - 1 ||
			     !(Imodv->mod[m]->obj[ob].flags & 
			       IMOD_OBJFLAG_OFF))
			      if (state){
				   Imodv->mod[m]->obj[ob].flags |= flag;
			      }else{
				   Imodv->mod[m]->obj[ob].flags &= ~flag;
			      }
	  }else{
	       for(ob = 0; ob < Imodv->imod->objsize; ob++)
		    if (Imodv_objed_all == editAll - 1 ||
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
