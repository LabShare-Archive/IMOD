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

#include <Xm/DialogS.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/Label.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <diaP.h>
#include "imod.h"

static int    Imod_object_edit_done;
static Widget Ioew_object;
static Widget Ioew_text;
static Widget Ioew_sizetext;
static Widget Ioew_color;
static Widget Ioew_draw;
static Widget Ioew_trans;
static Widget Ioew_fill;
static Widget Ioew_ends;
static Widget Ioew_pts;
static Widget Ioew_lines;
static Widget Ioew_linewidth;
static Widget Ioew_open;
static Widget Ioew_closed;
static Widget Ioew_in;
static Widget Ioew_out;
static Widget Ioew_scat;
static Widget Ioew_dialog = NULL;
static Widget Ioew_symsize;
static Widget Ioew_symbol[IOBJ_SYM_LAST];
static Widget Ioew_symopt;
static Widget Ioew_time;

void ioew_sgicolor_cb(Widget w, XtPointer client, XtPointer call)
{
     DiaColorCallbackStruct *cbs = (DiaColorCallbackStruct *) call;
     int ob = (int)client;
     Iobj *obj;

     switch(cbs->reason){
	case DIA_INIT:
	  break;
	case DIA_APPLY:
	case DIA_OK:
	case DIA_CANCEL:
	case DIA_SLIDER_CHANGED:
	case DIA_SLIDER_DRAG:
	  if (ob >= Model->objsize)
	       return;
	  obj = &(Model->obj[ob]);
	  obj->red = cbs->red / 255.0;
	  obj->green = cbs->green / 255.0;
	  obj->blue = cbs->blue / 255.0;

	  /* DNM: if TrueColor, need to free old color and allocate new one */
	  /* well, maybe not, but in any case, need to redraw unless it's a 
	     drag */
	  if (App->rgba) {
	       if (cbs->reason == DIA_SLIDER_DRAG)
		    break;
	       free_object_colors(Model, ob, ob);
	       alloc_object_colors(Model, ob, ob);
	       imodDraw(App->cvi, IMOD_DRAW_MOD);
	  }
	  break;
	default:
	  break;
     }
     imod_cmap(Model);
     imod_info_setobjcolor();

     return;
}

static void ioew_color_cb(Widget w, XtPointer client, XtPointer call)
{
     Iobj *obj;
     short red, green, blue;
     char prompt[32];
     int ob;


     obj = imodel_object_get(Model);
     if (!obj)
	  return;
     ob = Model->cindex.object + 1;

     red = obj->red * 255;
     blue = obj->blue * 255;
     green = obj->green * 255;
     sprintf(prompt, "Object %d color.", Model->cindex.object + 1);

     dia_cbcolor(red, green, blue, prompt, 
		 (void (*)())ioew_sgicolor_cb, (XtPointer)ob);
     return;
}

static void ioew_help_cb(Widget w, XtPointer client, XtPointer call)
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
		"Line Width:\n",
		"\tSets the width for lines drawn on images, but not for "
		"lines in 3D.\n\n",
		"Time data:\n",
		"\tIf multiple image files are loaded, this toggle button "
		"appears next to control whether time information is encoded "
		"in contours as they are drawn.  If the button is on, then "
		"each new contour that is created will be assigned to the "
		"currently displayed time, and it will appear only over "
		"images at that time.  In addition, the Time Index text box "
		"in the Edit-Contour-Type window can be used to adjust the "
		"time value of a contour.  If the button is off, then new "
		"contours will not be assigned to the current time but "
		"rather will have a time value of 0 and will appear over "
		"images at all times.\n\n"
		"Open/Closed/Scattered Toggles:\n",
		"\tSet how points in object are connected.  Open and closed "
		"contour objects are drawn with lines between the points; "
		"closed contours have a line connecting the last point back "
		"to the first one; scattered point objects have no "
		"connecting lines.\n\n",
		"Front Surface:\n",
		"\tOutside/Inside toggles select which side of the contours "
		"will be brightly lit after the object is meshed.  This "
		"feature can also be used to select an area of interest as "
		"inside or outside the contours, for some programs.\n\n",
		"Scattered Options:\n",
		"\tIf 3D Sphere size is nonzero, then spheres will be drawn "
		"in 3-D.  These spheres appear in"
		" the model view window and appear in cross-section on "
		"one or more slices of the image display.\n",
		NULL);
}

static void ioew_quit_cb(Widget w, XtPointer client, XtPointer call)
{
     imod_cmap(Model);
     XtDestroyWidget(Ioew_dialog);
     Ioew_dialog = NULL;
}

static void ioew_draw_cb(Widget w, XtPointer client_data, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;

     struct Mod_Object *obj;
     obj = imodel_object_get(Model);

     if (!obj){
	  dia_err("No object! Closing Edit dialog.");
	  XtDestroyWidget(Ioew_dialog);
	  Ioew_dialog = NULL;
     }
     if (cbs->set){
	  obj->flags = obj->flags & ~IMOD_OBJFLAG_OFF;
     }
     else{
	  obj->flags = obj->flags | IMOD_OBJFLAG_OFF;
     }
     imodDraw(App->cvi, IMOD_DRAW_MOD);
     return;
}

static void ioew_trans_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Mod_Object *obj;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
     obj = imodel_object_get(Model);
     
     if (!obj){
	  dia_err("No object! Closing Edit dialog.");
	  XtDestroyWidget(Ioew_dialog);
	  Ioew_dialog = NULL;
     }
     obj->trans = cbs->value;
     imodv_draw();
     return;
}

static void ioew_fill_cb(Widget w, XtPointer client_data, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     struct Mod_Object *obj;
     obj = imodel_object_get(Model);
     if (!obj){
	  dia_err("No object! Closing Edit dialog.");
	  XtDestroyWidget(Ioew_dialog);
	  Ioew_dialog = NULL;
     }
     if (!cbs->set)
	  obj->symflags = obj->symflags & ~IOBJ_SYMF_FILL;
     else
	  obj->symflags = obj->symflags | IOBJ_SYMF_FILL;

     imodDraw(App->cvi, IMOD_DRAW_MOD);
     return;
}

static void ends_cb(Widget w, XtPointer client_data, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     struct Mod_Object *obj;
     obj = imodel_object_get(Model);
     if (!obj){
	  dia_err("No object! Closing Edit dialog.");
	  XtDestroyWidget(Ioew_dialog);
	  Ioew_dialog = NULL;
     }
     if (!cbs->set)
	  obj->symflags = obj->symflags & ~IOBJ_SYMF_ENDS;
     else
	  obj->symflags = obj->symflags | IOBJ_SYMF_ENDS;
     
     imodDraw(App->cvi, IMOD_DRAW_MOD);
     return;
}

static void ioew_linewidth_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Mod_Object *obj;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
     obj = imodel_object_get(Model);
     
     if (!obj){
	  dia_err("No object! Closing Edit dialog.");
	  XtDestroyWidget(Ioew_dialog);
	  Ioew_dialog = NULL;
     }
     obj->linewidth2 = cbs->value;
     imodDraw(App->cvi, IMOD_DRAW_MOD);
     return;
}

static void ioew_open_cb(Widget w, XtPointer client_data, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     struct Mod_Object *obj;
     int mode = (int)client_data;

     obj = imodel_object_get(Model);
     if (!obj){
	  dia_err("No object! Closing Edit dialog.");
	  XtDestroyWidget(Ioew_dialog);
	  Ioew_dialog = NULL;
     }

     /* DNM 6/9/01: here and in next callback, workaround for PC now has 
	callbacks only on the disarm of the button that was pushed, so skip 
	this test and unconditionally set the correct buttons */
     /* if (!cbs->set)
	return; */

     switch (mode){
	case 1:
	  obj->flags = obj->flags & (~IMOD_OBJFLAG_OPEN);
	  obj->flags = obj->flags & (~IMOD_OBJFLAG_SCAT);
	  XmToggleButtonSetState(Ioew_closed, True, True);     
	  break;
	case 0:
	  obj->flags = obj->flags | IMOD_OBJFLAG_OPEN;
	  obj->flags = obj->flags & (~IMOD_OBJFLAG_SCAT);
	  XmToggleButtonSetState(Ioew_open, True, True);
	  break;
	case 2:
	  /* scattered */
	  obj->flags = obj->flags | IMOD_OBJFLAG_SCAT;
	  obj->flags = obj->flags | IMOD_OBJFLAG_OPEN;
	  XmToggleButtonSetState(Ioew_scat, True, True);
	  break;
     }
     imodDraw(App->cvi, IMOD_DRAW_MOD);
     return;
}

static void ioew_in_cb(Widget w, XtPointer client_data, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     struct Mod_Object *obj;
     int mode = (int)client_data;

     obj = imodel_object_get(Model);
     if (!obj){
	  dia_err("No object! Closing Edit dialog.");
	  XtDestroyWidget(Ioew_dialog);
	  Ioew_dialog = NULL;
     }
     
     /* if (!cbs->set)
	return; */

     if (!mode) {
	  obj->flags &= ~ IMOD_OBJFLAG_OUT;
	  XmToggleButtonSetState(Ioew_in, True, True);
     } else {
	  obj->flags |= IMOD_OBJFLAG_OUT;
	  XmToggleButtonSetState(Ioew_out, True, True);
     }

     imodDraw(App->cvi, IMOD_DRAW_MOD);
     return;
}


static void ioew_sizetext_cb(Widget w, XtPointer client_data, XtPointer call)
{
     struct Mod_Object *obj = NULL;
     char *name = NULL;
     int i = 0;

     obj = imodel_object_get(Model);
     if (!obj){
	  dia_err("No object! Closing Edit dialog.");
	  XtDestroyWidget(Ioew_dialog);
	  Ioew_dialog = NULL;
     }
     
     name = XmTextGetString(w);

     /* DNM: on PC, need to check for empty string or get huge number */
     if (name && name[0] != 0x00) {
	  sscanf(name, "%d", &i);
	  if (i < 0)
	       i = 0;
     }
     obj->pdrawsize = i;
     XtFree(name);
     imodDraw(App->cvi, IMOD_DRAW_MOD);
     return;
}

static void ioew_text_cb(Widget w, XtPointer client_data, XtPointer call)
{
     struct Mod_Object *obj = NULL;
     char *name = NULL;
     int i;

     obj = imodel_object_get(Model);
     if (!obj){
	  wprint("No object! Closing Edit dialog.");
	  XtDestroyWidget(Ioew_dialog);
	  Ioew_dialog = NULL;
     }
     
     name = XmTextGetString(w);
     
     if (name){
	  for(i = 0; (i < (IMOD_STRSIZE - 1))&&(name[i]); i++)
	       obj->name[i] = name[i];
	  obj->name[i] = 0x00;
	  XtFree(name);
     }
}

static void symbol_cb(Widget w, XtPointer client, XtPointer call)
{
     int item = (int)client;
     Iobj *obj =  imodObjectGet(Model);
     if (!obj)
	  return;

     obj->symbol = item;
     imodDraw(App->cvi, IMOD_DRAW_MOD);
     
     return;
}

static void symsize_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     Iobj *obj = imodel_object_get(Model);
     
     if (!obj){
	  dia_err("No object! Closing Edit dialog.");
	  XtDestroyWidget(Ioew_dialog);
	  Ioew_dialog = NULL;
     }
     obj->symsize = cbs->value;
     
     imodDraw(App->cvi, IMOD_DRAW_MOD);
     return;
}

static void ioew_time_cb(Widget w, XtPointer client, XtPointer call)
{
     Iobj *obj = imodel_object_get(Model);

     if (iobjTime(obj->flags)){
	  obj->flags &= ~IMOD_OBJFLAG_TIME;
     }else{
	  obj->flags |= IMOD_OBJFLAG_TIME;
     }
     imodDraw(App->cvi, IMOD_DRAW_MOD);
}

int imod_object_edit(Widget top)
{
     Widget dialog, pane, form, frame;
     Widget window, rowcol, row, row2, col, radio, pushb;
     Widget pulldown;
     Dimension h;
     Atom wmclose;
     Arg             args[4];
     Cardinal        n = 0;
     char *tstr;
     int i;
     Iobj *obj = imodObjectGet(Model);

     if (!obj){
	  dia_err("No Object selected");
	  return(-1);
     }

     if (Ioew_dialog){
	  XRaiseWindow(App->display, XtWindow(Ioew_dialog));
	  return(0);
     }
     
     dialog = XtVaCreatePopupShell
	  (Dia_title,  xmDialogShellWidgetClass,  Dia_toplevel,
	   XmNvisual, dia_getvisual(),
	   XmNdefaultPosition, True, 
	   XmNdeleteResponse, XmDESTROY, 
	   NULL);
     Ioew_dialog = dialog;
     XtAddCallback(dialog, XmNpopupCallback, dia_map_cb, NULL);

     if (!dialog){
	  dia_err("Object edit failed.");
	  return(-1);
     }

     pane = XtVaCreateWidget
	  ("pane", xmPanedWindowWidgetClass, dialog,
	   XmNsashWidth,  1,
	   XmNsashHeight, 1,
	   NULL);

     form = XtVaCreateWidget
	  ("form1", xmFormWidgetClass, pane, NULL);
     
     rowcol = XtVaCreateManagedWidget
	   ("rowcol", xmRowColumnWidgetClass, form,
	    XmNtopAttachment,    XmATTACH_FORM,
	    XmNleftAttachment,   XmATTACH_FORM,
	    XmNrightAttachment,  XmATTACH_FORM,
	    XmNbottomAttachment, XmATTACH_FORM,
	    NULL);

     /**********************************************************************/
     /*  Work area. */


     /* label */
     XtVaCreateManagedWidget("Object name:", xmLabelWidgetClass, rowcol, NULL);
     Ioew_text = XtVaCreateManagedWidget
	  ( "text", xmTextWidgetClass, rowcol, 
	   XmNvalue, obj->name, NULL);
     XtAddCallback(Ioew_text, XmNvalueChangedCallback, ioew_text_cb, NULL);

     Ioew_draw =  XtVaCreateManagedWidget
	  ("draw", xmToggleButtonWidgetClass, rowcol, NULL);
     XtAddCallback(Ioew_draw, XmNvalueChangedCallback, ioew_draw_cb, NULL);


/*******************/
/*     Ioew_symbol */
     frame = XtVaCreateManagedWidget
	  ("frame", xmFrameWidgetClass, rowcol, NULL);
     col = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, frame, NULL);


     XtVaCreateManagedWidget("Symbols:", xmLabelWidgetClass, col, NULL);

     row = XtVaCreateManagedWidget
	  ("row", xmRowColumnWidgetClass, col, 
	   XmNorientation, XmHORIZONTAL,
	   NULL);

     XtSetArg(args[n], XmNvisual, dia_getvisual());n++;
     pulldown = XmCreatePulldownMenu
	  (row, "option_pd", args, n);

     Ioew_symbol[1] = XtVaCreateManagedWidget
	  ("None", xmPushButtonWidgetClass, pulldown, NULL);
     XtAddCallback(Ioew_symbol[1], XmNactivateCallback, 
		   symbol_cb, (XtPointer)IOBJ_SYM_NONE);
     Ioew_symbol[0] = XtVaCreateManagedWidget
	            ("Circle", xmPushButtonWidgetClass, pulldown, NULL);
     XtAddCallback(Ioew_symbol[0], XmNactivateCallback,
		   symbol_cb, (XtPointer)IOBJ_SYM_CIRCLE);
     Ioew_symbol[2] = XtVaCreateManagedWidget
	  ("Square", xmPushButtonWidgetClass, pulldown, NULL);
     XtAddCallback(Ioew_symbol[2], XmNactivateCallback, 
		   symbol_cb, (XtPointer) IOBJ_SYM_SQUARE);
     Ioew_symbol[3] = XtVaCreateManagedWidget
	  ("Triangle", xmPushButtonWidgetClass, pulldown, NULL);
     XtAddCallback(Ioew_symbol[3], XmNactivateCallback, 
		   symbol_cb, (XtPointer)IOBJ_SYM_TRIANGLE);
     XtSetArg(args[n], XmNsubMenuId, pulldown); n++;
     Ioew_symopt =  XmCreateOptionMenu(row, "option_rc", args, n);
     XtManageChild(Ioew_symopt);

     row2 = XtVaCreateManagedWidget
	  ("row", xmRowColumnWidgetClass, row, NULL);

     Ioew_fill  = XtVaCreateManagedWidget
	  ("fill",  xmToggleButtonWidgetClass, row2,
	   XmNmarginHeight, 0, NULL);
     XtAddCallback(Ioew_fill, XmNvalueChangedCallback, ioew_fill_cb, NULL);

     Ioew_ends = XtVaCreateManagedWidget
	  ("mark ends",  xmToggleButtonWidgetClass, row2, 
	   XmNmarginHeight, 0, NULL);
     XtAddCallback(Ioew_ends, XmNvalueChangedCallback, ends_cb, NULL);
     
     Ioew_symsize = XtVaCreateManagedWidget
	  ("scale", xmScaleWidgetClass, col,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 1, XmNmaximum, 100, XmNvalue, (int)obj->linewidth2,
	   XmNshowValue, True, XmNscaleMultiple, 1,
	   XtVaTypedArg, XmNtitleString, XmRString, "Size", 5,
	   NULL);
     XtAddCallback(Ioew_symsize, XmNvalueChangedCallback,
		   symsize_cb, NULL);

     if (obj->linewidth2 < 1)
	  obj->linewidth2 = 1;
     Ioew_linewidth = XtVaCreateManagedWidget
	  ("line_width", xmScaleWidgetClass, rowcol,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 1, XmNmaximum, 10, XmNvalue, (int)obj->linewidth2,
	   XmNshowValue, True, XmNscaleMultiple, 1,
	   XtVaTypedArg, XmNtitleString, XmRString, "Line Width", 11,
	   NULL);

     XtAddCallback(Ioew_linewidth, XmNvalueChangedCallback,
		   ioew_linewidth_cb, NULL);
     
     if (App->cvi->nt){
	  Ioew_time = XtVaCreateManagedWidget
	       ("Time data", xmToggleButtonWidgetClass, rowcol, NULL);
	  XtAddCallback(Ioew_time, XmNvalueChangedCallback,
			ioew_time_cb, NULL);
     }else
	  Ioew_time = 0;

     row = XtVaCreateManagedWidget
	  ("row", xmRowColumnWidgetClass, rowcol, 
	   XmNorientation, XmHORIZONTAL, NULL);

     radio = XmCreateRadioBox(row, "radio", NULL, 0);

     Ioew_open   = XtVaCreateManagedWidget
	  ("Open", xmToggleButtonWidgetClass, radio, NULL);
     XtAddCallback(Ioew_open, XmNdisarmCallback, 
		   ioew_open_cb, (XtPointer)0);
     Ioew_closed = XtVaCreateManagedWidget
	  ("Closed", xmToggleButtonWidgetClass, radio, NULL);
     XtAddCallback(Ioew_closed, XmNdisarmCallback, 
		   ioew_open_cb, (XtPointer)1);
     Ioew_scat = XtVaCreateManagedWidget
	  ("Scattered", xmToggleButtonWidgetClass, radio, NULL);
     XtAddCallback(Ioew_scat, XmNdisarmCallback, 
		   ioew_open_cb, (XtPointer)2);
     XtManageChild(radio);


     frame = XtVaCreateManagedWidget
	  ("frame", xmFrameWidgetClass, row, NULL);
     col = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, frame, NULL);
     XtVaCreateManagedWidget
	  ("Front Surface:", xmLabelWidgetClass, col, NULL);
     radio = XmCreateRadioBox(col, "radio", NULL, 0);
     Ioew_in  = XtVaCreateManagedWidget
	  ("Outside", xmToggleButtonWidgetClass, radio, NULL);
     XtAddCallback(Ioew_in, XmNdisarmCallback, 
		   ioew_in_cb, (XtPointer)0);
     Ioew_out = XtVaCreateManagedWidget
	  ("Inside", xmToggleButtonWidgetClass, radio, NULL);
     XtAddCallback(Ioew_out, XmNdisarmCallback, 
		   ioew_in_cb, (XtPointer)1);
     XtManageChild(radio);
     

     frame = XtVaCreateManagedWidget
	  ("frame", xmFrameWidgetClass, rowcol, NULL);
     col = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, frame, NULL);
     XtVaCreateManagedWidget
	  ("Scattered Options:", xmLabelWidgetClass, col, NULL);
     row = XtVaCreateManagedWidget
	  ("row", xmRowColumnWidgetClass, col,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     XtVaCreateManagedWidget("3D Sphere Size", xmLabelWidgetClass, row, NULL);
     Ioew_sizetext = XtVaCreateManagedWidget
	  ( "text", xmTextWidgetClass, row, 
	    XmNcolumns, 5,
	    NULL);
     XtAddCallback(Ioew_sizetext, XmNvalueChangedCallback, 
		   ioew_sizetext_cb, NULL);
     XtManageChild(frame);

     XtManageChild(form);


      /* ACTION AREA */
     form = XtVaCreateWidget ("form2", xmFormWidgetClass, pane,
			      XmNfractionBase,    11,
			      NULL);
     

     pushb = XtVaCreateManagedWidget
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
     XtAddCallback(pushb, XmNactivateCallback, ioew_quit_cb, (XtPointer)1);
     

      pushb = XtVaCreateManagedWidget
	    ("HELP", xmPushButtonWidgetClass, form,
	     XmNtopAttachment,        XmATTACH_FORM,
	     XmNbottomAttachment,     XmATTACH_FORM,
	     XmNleftAttachment,       XmATTACH_POSITION,
	     XmNleftPosition,         6,
	     XmNrightAttachment,      XmATTACH_POSITION,
	     XmNrightPosition,        10,
	     XmNdefaultButtonShadowThickness, 1,
	     NULL);
     XtAddCallback(pushb, XmNactivateCallback, ioew_help_cb, NULL);
     

     XtManageChild(form);
     XtVaGetValues (pushb, XmNheight, &h, NULL);
     XtVaSetValues (form, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
     XtManageChild (pane);


     wmclose = XmInternAtom( XtDisplay(dialog), "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(dialog, wmclose, ioew_quit_cb,
			     (caddr_t)-1);

     XtPopup (dialog, XtGrabNone);

     imod_object_edit_draw();
     return(0);
}


int imod_object_edit_draw(void)
{
     struct Mod_Object *obj;
     char sizetext[12];

     if (!Ioew_dialog)
	  return(-1);

     obj = imodel_object_get(Model);

     if (!obj){
	  dia_err("No object! Closing Edit dialog.");
	  XtDestroyWidget(Ioew_dialog);
	  Ioew_dialog = NULL;
	  return(-1);
     }

     XmTextSetString( Ioew_text, obj->name);
     sprintf(sizetext, "%d", obj->pdrawsize);
     XmTextSetString( Ioew_sizetext, sizetext);

     if (obj->flags & IMOD_OBJFLAG_OFF)
	  XmToggleButtonSetState(Ioew_draw, False, False);
     else
	  XmToggleButtonSetState(Ioew_draw, True, False);

     if (obj->symflags & IOBJ_SYMF_FILL)
	  XmToggleButtonSetState(Ioew_fill, True, False);
     else
	  XmToggleButtonSetState(Ioew_fill, False, False);

     if (obj->symflags & IOBJ_SYMF_ENDS)
	  XmToggleButtonSetState(Ioew_ends, True, False);
     else
	  XmToggleButtonSetState(Ioew_ends, False, False);

     XtVaSetValues(Ioew_linewidth, XmNvalue, (int)obj->linewidth2, NULL);

     XtVaSetValues(Ioew_symsize, XmNvalue, (int)obj->symsize, NULL);
	       
     if (obj->flags & IMOD_OBJFLAG_OUT)
	  XmToggleButtonSetState(Ioew_out, True, True);
     else
	  XmToggleButtonSetState(Ioew_in, True, True);

     if (Ioew_time){
	  if (obj->flags & IMOD_OBJFLAG_TIME)
	      XmToggleButtonSetState(Ioew_time, True, False);
	  else
	       XmToggleButtonSetState(Ioew_time, False, False);
     }

     if (obj->flags & IMOD_OBJFLAG_SCAT)
	  XmToggleButtonSetState(Ioew_scat, True, True);
     else
	  if (obj->flags & IMOD_OBJFLAG_OPEN)
	       XmToggleButtonSetState(Ioew_open, True, True);
	  else
	       XmToggleButtonSetState(Ioew_closed, True, True);     

     XtVaSetValues(Ioew_symopt, XmNmenuHistory, 
		   Ioew_symbol[obj->symbol], NULL);

     return(0);
}
