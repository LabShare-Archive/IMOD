/*  IMOD VERSION 2.50
 *
 *  imodv_modeled.c -- Model edit dialog for imodv.
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

#include <math.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/ArrowB.h>
#include <Xm/PushB.h>
#include <dia.h>
#include "imodv.h"

struct imodv_modeled
{
     diaDialog *dia;
     ImodvApp  *a;
     Widget     modelno_widget;
};

static struct imodv_modeled *ModelEdit = NULL;
static void updateWorkArea(void);
static Widget make_work_area(ImodvApp *a, Widget top);

static void help_cb()
{
     dia_vasmsg
	  ("Model Edit Dialog Help.\n\n",
	   "\tMultiple models can be loaded with imodv, either on the command "
	   "line or through the File menu.  ",
	   "To page through the different models use the Current ",
	   "Model # Arrow Gadgets or the 9 and 0 hotkeys.\n",
	   "\tThe Move selection menu allows one to choose between ",
	   "having actions move just the current model, \"Each\", or ",
	   "having actions move \"All\" of the models.\n",
	   "\tThe View selection menu allows one to choose between ",
	   "viewing just the current model, \"One\", viewing the ",
	   "current ",
	   "model and the previous model, \"Prev\", viewing the current ",
	   "model and the next model, \"Next\", ",
	   "or viewing \"All\" of the  models.  The 8 hotkey toggles between "
	   "\"One\" and \"All\".\n"
	   "\tThe Edit selection menu toggles between editing just ",
	   "the current model, \"Each\", and editing \"All\" of the ",
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
	   "changed with arrow gadgets or hotkeys, all models will change "
	   "their zoom by the same amount.\n",
	   NULL);
     return;
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     struct imodv_modeled *med = (struct imodv_modeled *)client;
     Widget parent = w;
     
     XtManageChild(make_work_area(med->a, w));
     return;
}



static void done_cb(Widget w, XtPointer client, XtPointer call)
{
     /* DNM: passing dia in the call spot didn't work on PC, and the
	client data is enough for the job */
     /*  diaDialog *dia = (diaDialog *)call; */
     struct imodv_modeled *med = (struct imodv_modeled *)client;

     ModelEdit =  NULL;
     diaDestroyDialog(med->dia);
     med->dia = NULL;
     return;
}

void imodvModelEditDialog(ImodvApp *a, int state)
{
     static struct imodv_modeled med;
     static int first = 1;

     if (first){
	  med.dia = NULL;
	  first = 0;
     }
     if (!state){
	  /* DNM: make call entry NULL; client entry is enough */
	  if (med.dia)
	       done_cb(NULL, (XtPointer)&med, NULL);
	  return;
     }
     if (med.dia){
	  XRaiseWindow(a->display, XtWindow(med.dia->dialog));
	  return;
     }
     ModelEdit = &med;
     med.a = a;

     med.dia = diaVaCreateDialog
	  ("Imodv: Model Edit", a->topLevel, a->context,
	   DiaNcontrolButton, "Done", done_cb, (XtPointer)&med,
	   DiaNcontrolButton, "Help", help_cb, (XtPointer)&med,
	   DiaNworkAreaFunc, workarea_cb, (XtPointer)&med,
	   DiaNwindowQuit, done_cb, (XtPointer)&med,
	   0);
     return;
}


int imodvSelectModel(ImodvApp *a, int ncm)
{
     XmString xmstr;
     char   modelno[8];

     if (ncm < 0)
	  return(0);
     if (ncm > a->nm - 1)
	  return(a->nm - 1);

     a->cm = ncm;

     a->imod = a->mod[a->cm];
     if (a->ob >= a->imod->objsize)
	  a->ob = 0;
     a->obj = &(a->imod->obj[a->ob]);
     
     if (ModelEdit){
	  sprintf(modelno, "%d", a->cm + 1);
	  xmstr = XmStringCreateSimple(modelno);
	  XtVaSetValues(ModelEdit->modelno_widget, 
			XmNlabelString, xmstr, NULL);
	  XmStringFree(xmstr);
	  updateWorkArea();
     }
     imodvUpdateModel(a);

     imodvDraw(a);

     return(a->cm);
}

static void imodv_modelno_cb(Widget w, XtPointer client, XtPointer call)
{
     imodvSelectModel(Imodv, Imodv->cm + (int)client);
     updateWorkArea();
     return;
}

static void mouse_move_cb(Widget w, XtPointer client, XtPointer call)
{
     int item = (int) client;
     Imodv->mousemove = item;
     return;
}

static void model_move_cb(Widget w, XtPointer client, XtPointer call)
{
     int item = (int) client;
     Imodv->moveall = item;
     
     return;
}
static void model_view_cb(Widget w, XtPointer client, XtPointer call)
{
     int item = (int) client;
     Imodv->drawall = item;
     imodvDraw(Imodv);
     return;
}
static void model_edit_cb(Widget w, XtPointer client, XtPointer call)
{
     int item = (int) client;
     Imodv->crosset = item;
     return;
}

static Widget wModelName;
static void modelName_cb(Widget w, XtPointer client, XtPointer call)
{
     int i,mi;
     char *name = NULL;
     name = XmTextGetString(w);

     mi = strlen(name);
     if (mi >= IMOD_STRSIZE)
	  mi = IMOD_STRSIZE - 1;
     for(i = 0 ; i<mi; i++)
	  Imodv->imod->name[i] = name[i];
     Imodv->imod->name[i] = 0x00;

     XtFree(name);
     updateWorkArea();
}

static Widget wModelScale;
static void modelScale_cb(Widget w, XtPointer client, XtPointer call)
{
     char *name = NULL;
     float fscale;

     name = XmTextGetString(w);

     /* DNM: protect sscanf from empty strings for PC */
     fscale = 0.;
     if (name && name[0] != 0x00)
	  sscanf(name, "%f", &fscale);
     
     if (fscale != 0)
	  Imodv->imod->pixsize = fscale;
     if (strstr(name, "mm"))
	  Imodv->imod->units = IMOD_UNIT_MM;
     if (strstr(name, "um"))
	  Imodv->imod->units = IMOD_UNIT_UM;
     if (strstr(name, "nm"))
	  Imodv->imod->units = IMOD_UNIT_NM;
     
     XtFree(name);
     updateWorkArea();
}

static void updateWorkArea(void)
{
     char scale[64];

     if (wModelName)
	  XmTextSetString(wModelName, Imodv->imod->name);

     switch(Imodv->imod->units){
	case IMOD_UNIT_MM:
	  sprintf(scale, "%g mm", Imodv->imod->pixsize);
	  break;
	case IMOD_UNIT_UM:
	  sprintf(scale, "%g um", Imodv->imod->pixsize);
	  break;
	case IMOD_UNIT_NM:
	  sprintf(scale, "%g nm", Imodv->imod->pixsize);
	  break;
	default:
	  sprintf(scale, "%g", Imodv->imod->pixsize);
	  break;
     }
     if (wModelScale)
	  XtVaSetValues(wModelScale, XmNvalue, scale, NULL);

}

static Widget wViewData[5];

void imeSetViewData(int wi)
{
     if (ModelEdit)
          XtVaSetValues(wViewData[0], XmNmenuHistory, wViewData[wi + 1], NULL);
}


static Widget makeViewData(Widget parent, int viewval)
{
     Widget menuWidget, optionWidget;
     Arg args[4];
     int n = 0;

     XtSetArg(args[n], XmNvisual, Imodv->visual); n++;
     menuWidget = XmCreatePulldownMenu(parent, "pulldown", args, n);

     XtSetArg(args[n], XmNsubMenuId, menuWidget); n++;

     wViewData[0] = XmCreateOptionMenu
	  (parent, "option", args, n);
     
     wViewData[1] = XtVaCreateManagedWidget
	  ("One", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wViewData[1], XmNactivateCallback, 
		   model_view_cb, (XtPointer)0);   

     wViewData[2] =XtVaCreateManagedWidget
	  ("Prev", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wViewData[2], XmNactivateCallback, 
		   model_view_cb, (XtPointer)1);

     wViewData[3] =XtVaCreateManagedWidget
	  ("Next", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wViewData[3], XmNactivateCallback, 
		   model_view_cb, (XtPointer)2);

     wViewData[4] =XtVaCreateManagedWidget
	  ("All", xmPushButtonWidgetClass, menuWidget, NULL);           
     XtAddCallback( wViewData[4], XmNactivateCallback, 
		   model_view_cb, (XtPointer)3);
	  
     XtManageChild(wViewData[0]);
     imeSetViewData(viewval);
     return(wViewData[0]);
}


static Widget make_work_area(ImodvApp *a, Widget top)
{
     Widget frame, col;
     Widget row, arrow, menu;
     char   modelno[8];
     XmString str, s1, s2, s3, s4;

     sprintf(modelno, "%d", a->cm + 1);

     frame = XtVaCreateManagedWidget
	  ("frame", xmFrameWidgetClass, top, NULL);

     col = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, frame,
	   NULL);

     XtVaCreateManagedWidget
	  ("Current Model #",xmLabelWidgetClass, col, NULL);     
     row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, col,
	   XmNorientation, XmHORIZONTAL, NULL);
     arrow = XtVaCreateManagedWidget
	  ("arrow", xmArrowButtonWidgetClass, row,
	   XmNarrowDirection, XmARROW_UP, NULL);
     XtAddCallback(arrow, XmNarmCallback, imodv_modelno_cb, (XtPointer)1);
     arrow = XtVaCreateManagedWidget
	  ("arrow", xmArrowButtonWidgetClass, row,
	   XmNarrowDirection, XmARROW_DOWN, NULL);
     XtAddCallback(arrow, XmNarmCallback, imodv_modelno_cb, (XtPointer)-1);
     ModelEdit->modelno_widget = XtVaCreateManagedWidget
	  (modelno,xmLabelWidgetClass, row, NULL);

     XtVaCreateManagedWidget
	  ("Name:", xmLabelWidgetClass, col, NULL);
     wModelName = XtVaCreateManagedWidget
	  ("Name", xmTextWidgetClass, col, NULL);
     XtAddCallback(wModelName, XmNactivateCallback,
		   modelName_cb, NULL);

     XtVaCreateManagedWidget
	  ("Pixel Size (mm, um, nm):", xmLabelWidgetClass, col, NULL);
     wModelScale = XtVaCreateManagedWidget
	  ("Name", xmTextWidgetClass, col, NULL);
     XtAddCallback(wModelScale, XmNactivateCallback,
		   modelScale_cb, NULL);


     str  = XmStringCreateSimple("Move");
     s1   = XmStringCreateSimple("Each");
     s2   = XmStringCreateSimple("All");
     menu = XmVaCreateSimpleOptionMenu
	  (col, "option_menu",
	   str, 'M', Imodv->moveall, model_move_cb,
	   XmVaPUSHBUTTON, s1, 'E', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'A', NULL, NULL,
	   XmNvisual,   Imodv->visual,
	   NULL);
     XmStringFree(str);
     XmStringFree(s1);
     XmStringFree(s2);
     XtManageChild(menu);


     /* DNM: replace the simple option menu so that it can be set externally
	after keyboard input. */
     /*
     str  = XmStringCreateSimple("View");
     s1   = XmStringCreateSimple("One");
     s2   = XmStringCreateSimple("Prev");
     s3   = XmStringCreateSimple("Next");
     s4   = XmStringCreateSimple("All");
     menu = XmVaCreateSimpleOptionMenu
	  (col, "option_menu",
	   str, 'V', Imodv->drawall, model_view_cb,
	   XmVaPUSHBUTTON, s1, 'O', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'P', NULL, NULL,
	   XmVaPUSHBUTTON, s3, 'N', NULL, NULL,
	   XmVaPUSHBUTTON, s4, 'A', NULL, NULL,
	   XmNvisual,   Imodv->visual,
	   NULL);
     XmStringFree(str);
     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(s4);
     XtManageChild(menu);
     */

     row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, col,
	   XmNorientation, XmHORIZONTAL, NULL);
     XtVaCreateManagedWidget
       ("View", xmLabelWidgetClass, row, NULL);
     makeViewData(row, Imodv->drawall);
     XtManageChild(row);

     str  = XmStringCreateSimple("Edit");
     s1   = XmStringCreateSimple("Each");
     s2   = XmStringCreateSimple("All");
     menu = XmVaCreateSimpleOptionMenu
	  (col, "option_menu",
	   str, 'E', Imodv->drawall, model_edit_cb,
	   XmVaPUSHBUTTON, s1, 'E', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'A', NULL, NULL,
	   XmNvisual,   Imodv->visual,
	   NULL);
     XmStringFree(str);
     XmStringFree(s1);
     XmStringFree(s2);
     XtManageChild(menu);

     /* add move model/light/clip control */
     /*     str = XmStringCreateSimple("Mouse");
     s1  = XmStringCreateSimple("Model");
     s2  = XmStringCreateSimple("Light");
     s3  = XmStringCreateSimple("Clip");
     menu = XmVaCreateSimpleOptionMenu
	  (col, "option_menu",
	   str, 'o', Imodv->mousemove, mouse_move_cb,
	   XmVaPUSHBUTTON, s1, 'M', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'L', NULL, NULL,
	   XmVaPUSHBUTTON, s3, 'C', NULL, NULL,
	   XmNvisual,   Imodv->visual,
	   NULL);
     XmStringFree(str);
     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     */

     



     updateWorkArea();
     return(frame);
}     
