/*  IMOD VERSION 2.50
 *
 *  imodv_control.c -- The imodv control edit dialog.
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
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/ArrowB.h>
#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include "diaP.h"
#include "dia.h"
#include "imodel.h"
#include "imodv.h"


char *imodwEithername(char *intro, char *filename);


static Widget make_second_column(ImodvApp *a, Widget top);
static Widget Imodv_nearscale_widget;
static Widget Imodv_farscale_widget;
static Widget Imodv_modelno_widget;
static Widget Imodv_fovyscale_widget;
static Widget Imodv_zscale_widget;
static Widget Imodv_rotscale_widget;
static Widget wXrot;
static Widget wYrot;
static Widget wZrot;
static Widget wScale;
static Dimension Dialogx;
static Dimension Dialogy;
static Widget dialog = 0;

#define ROTATION_MAX 100

static void map_cb(Widget w, XtPointer client, XtPointer call)
{
     XtVaSetValues(w, XmNx, Dialogx, XmNy, Dialogy, NULL);
     return;
}

static void imodv_control_help_cb(Widget w, XtPointer client, XtPointer call)
{
     dia_vasmsg("Imodv Controls Help\n",
		"-----------------------------------------------------\n",
		"This dialog controls model viewing and movement.\n\n",
		"\tThe Zoom Arrows ",
		"increase or decrease zoom factor.\n\n",
		"\tThe Scale text box shows the number of pixels on the "
		"screen per unit of model coordinates; you can enter a "
		"specific scale to control model zoom.\n\n"
		"\tThe Near and far sliders adjust the Z clipping planes.\n\n",
		"\tThe Z-scale slider adjusts the scale for section "
		"thickness.\n\n",
		"\tThe Rotation box edits model rotation. ",
		"The x, y or z axis arrow",
		"gadgets are equivalent to the hotkeys on the numeric keypad."
		"  If you press the Start/Stop Rotation button then one of "
		"the arrow gadgets, the model will start rotating around the "
		"given axis until you press the Start/Stop Rotation button "
		"again.  Otherwise, each arrow will cause a single step "
		"around the given axis.\n\n",
		"\tThe Degrees slider sets the number of degrees",
		"between views during rotation, as well as the step size for "
		"single steps with the arrows or mumeric pad keys.  The same "
		"step size is used when rotating an object clipping with "
		"Ctrl and the keypad keys.\n",
		NULL);
}



static void imodv_zoom_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     int zoom = (int) client_data;

     if (zoom > 0)
	  imodv_zoomd(Imodv, 1.05);
     else
	  imodv_zoomd(Imodv, 0.95238095);
     imodvDraw(Imodv);
     return;
}

static void imodv_control_clip_cb
(Widget widget, XtPointer client, XtPointer call)
{
     Iview *view, *vwv;
     int m;
     int plane = (int) client;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;

     if (!Imodv->imod) return;
/*     view = &Imodv->imod->view[Imodv->imod->cview];*/
     view = Imodv->imod->view;

     if (plane == 1){
	  Imodv->cfar = cbs->value;
	  if (Imodv->cnear >= Imodv->cfar){
	       Imodv->cnear = Imodv->cfar - 1;
	       XtVaSetValues(Imodv_nearscale_widget, 
			     XmNvalue, Imodv->cnear, NULL);
	  }
	  view->cnear = Imodv->cnear * 0.001;
	  view->cfar  = Imodv->cfar * 0.001;
	  if (Imodv->crosset)
	       for (m = 0; m < Imodv->nm; m++) {
		    vwv = Imodv->mod[m]->view;
		    vwv->cfar = view->cfar;
		    if (vwv->cnear >= vwv->cfar)
		         vwv->cnear = vwv->cfar - 0.001;
	       }
     }

     if (plane == 0){
	  Imodv->cnear = cbs->value;
	  if (Imodv->cfar <= Imodv->cnear){
	       Imodv->cfar = Imodv->cnear + 1;
	       XtVaSetValues(Imodv_farscale_widget, 
			     XmNvalue, Imodv->cfar, NULL);
	  }
	  view->cnear = Imodv->cnear * 0.001;
	  view->cfar  = Imodv->cfar * 0.001;
	  if (Imodv->crosset)
	       for (m = 0; m < Imodv->nm; m++) {
		    vwv = Imodv->mod[m]->view;
		    vwv->cnear = view->cnear;
		    if (vwv->cfar <= vwv->cnear)
		         vwv->cfar = vwv->cnear + 0.001;
	       }
     }

     if (plane == 2) {
	  Imodv->fovy = cbs->value;
	  view->fovy  = Imodv->fovy;
	  if (Imodv->crosset)
	       for (m = 0; m < Imodv->nm; m++) 
		    Imodv->mod[m]->view->fovy = view->fovy;
     }

     imodvDraw(Imodv);
}

void imodvControlSetArot(ImodvApp *a, int newval)
{
     if (newval > ROTATION_MAX)
          newval = ROTATION_MAX;
     a->md->arot = newval;
     if (a->md->xrotm)
          a->md->xrotm = (a->md->xrotm > 0 ? 1 : -1 ) * a->md->arot;
     if (a->md->yrotm)
          a->md->yrotm = (a->md->yrotm > 0 ? 1 : -1 ) * a->md->arot;
     if (a->md->zrotm)
          a->md->zrotm = (a->md->zrotm > 0 ? 1 : -1 ) * a->md->arot;
     if (dialog)
	  XtVaSetValues(Imodv_rotscale_widget,
			XmNvalue, a->md->arot, NULL);
}

void imodvControlSetView(ImodvApp *a)
{
     if (!a->imod) return;
	  
     a->fovy  = a->imod->view->fovy;
     a->cnear = a->imod->view->cnear * 1000.0 + 0.5;
     a->cfar  = a->imod->view->cfar * 1000.0 + 0.5;
     if (dialog){
	  XtVaSetValues(Imodv_nearscale_widget,
			XmNvalue, a->cnear, NULL);
	  XtVaSetValues(Imodv_farscale_widget,
			XmNvalue, a->cfar, NULL);
	  XtVaSetValues(Imodv_fovyscale_widget,
			XmNvalue, a->fovy, NULL);
	  XtVaSetValues(Imodv_zscale_widget,
			XmNvalue,(int) (a->imod->zscale * 10.0 + 0.5), NULL);
     }
     return;
}

static void imodv_control_zscale_cb
(Widget widget, XtPointer client, XtPointer call)
{
     int plane = (int) client;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     int m, nm;
     nm = Imodv->nm;
     Imodv->mod[Imodv->cm]->zscale = cbs->value / 10.0;
     /* DNM: this should not be adjusted in tandem in general */
     /*     if (Imodv->crosset)
	  for(m = 0; m < nm; m++){
	       Imodv->mod[m]->zscale = cbs->value / 10.0;
	  }
     */
     imodvDraw(Imodv);
}

static void imodv_control_scale_cb
(Widget widget, XtPointer client, XtPointer call)
{
     ImodvApp *a = Imodv;
     char *string;
     int m;
     float scale;
     string = XmTextGetString(wScale);
     if (string){
          scale = atof(string);
	  a->imod->view->rad = 
	    0.5 * (a->winx > a->winy ? a->winy : a->winx) / scale;
	  free(string);
	  if (Imodv->crosset)
	       for(m = 0; m < a->nm; m++)
		    a->mod[m]->view->rad = a->imod->view->rad;
	  imodvDraw(Imodv);
     }
}

/* In edit object instead */
/*
static void subset_cb(Widget w, XtPointer client, XtPointer call)
{
     int item = (int) client;
     Imodv->current_subset = item;
     imodvDraw(Imodv);
     return;
}
*/

static void imodv_control_axis_cb
(Widget widget, XtPointer client, XtPointer call)
{
     ImodvApp *a = Imodv;
     int axis = (int) client;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     char *string;
     float rot;
     int m;

     switch(axis){
	case -1:
	  if (Imodv->movie)
	       Imodv->movie = False;
	  else
	       Imodv->movie = True;

	  Imodv->md->xrotm = 0;
	  Imodv->md->yrotm = 0;
	  Imodv->md->zrotm = 0;
	  break;
	case 2:
	  imodv_rotate_model(a, 0, a->md->arot, 0);
	  break;
	case 8:
	  imodv_rotate_model(a, 0, -a->md->arot, 0);
	  break;
	case 6:
	  imodv_rotate_model(a, a->md->arot, 0, 0);
	  break;
	case 4:
	  imodv_rotate_model(a, -a->md->arot, 0, 0);
	  break;
	case 9:
	  imodv_rotate_model(a, 0, 0, a->md->arot);
	  break;
	case 3:
	  imodv_rotate_model(a, 0, 0, -a->md->arot);
	  break;
	case 0:
	  Imodv->md->arot = cbs->value;
	  if (Imodv->md->xrotm)
	       Imodv->md->xrotm = Imodv->md->arot;
	  if (Imodv->md->yrotm)
	       Imodv->md->yrotm = Imodv->md->arot;
	  if (Imodv->md->zrotm)
	       Imodv->md->zrotm = Imodv->md->arot;
	  break;

	case 10:
	  string = XmTextGetString(wXrot);
	  if (string){
	       a->imod->view->rot.x = rot = atof(string);
	       if (a->moveall)
		    for(m = 0; m < a->nm; m++)
			 a->mod[m]->view->rot.x = rot;
	       free(string);
	  }
	  break;
	case 11:
	  string = XmTextGetString(wYrot);
	  if (string){
	       a->imod->view->rot.y = rot = atof(string);
	       if (a->moveall)
		    for(m = 0; m < a->nm; m++)
			 a->mod[m]->view->rot.y = rot;
	       free(string);
	  }
	  break;
	case 12:
	  string = XmTextGetString(wZrot);
	  if (string){
	       a->imod->view->rot.z = rot = atof(string);
	       if (a->moveall)
		    for(m = 0; m < a->nm; m++)
			 a->mod[m]->view->rot.z = rot;
	       free(string);
	  }
	  break;

     }
     imodvDraw(Imodv);
     return;
}

void imodvControlUpdate(ImodvApp *a)
{
     char string[32];
     float scale;
     if (!dialog) return;
     sprintf(string, "%6.2f", a->imod->view->rot.x);
     XmTextSetString(wXrot,string);
     sprintf(string, "%6.2f", a->imod->view->rot.y);
     XmTextSetString(wYrot,string);
     sprintf(string, "%6.2f", a->imod->view->rot.z);
     XmTextSetString(wZrot,string);
     scale = 0.5 * (a->winx > a->winy ? a->winy : a->winx) / 
             a->imod->view->rad;
     sprintf(string, "%.4g", scale);
     XmTextSetString(wScale,string);
     
}

static void imodv_control_quit_cb(Widget w, XtPointer client, XtPointer call)
{
     Widget *shell = (Widget *) client;
     XtVaGetValues(*shell, XmNx, &Dialogx, XmNy, &Dialogy, NULL);
     XtDestroyWidget(*shell);
     *shell = NULL;
}

int imodv_control(ImodvApp *a, int state)
{
     Widget pane, form, topcol;
     Widget label, scale, button, arrow, menu;
     Widget col, row, lcol, frame;
     Dimension h;
     Atom wmclose;
     char *window_name;
     /*     XmString str, s1, s2, s3, s4, s5, s6; */

     if (!state){
	  if (dialog)
	       imodv_control_quit_cb(dialog, (XtPointer)&dialog, (XtPointer)0);
	  return -1;
     }

     if (dialog){
	  XRaiseWindow(a->display, XtWindow(dialog));
	  return -1;
     }
     window_name = imodwEithername("IMODV Controls: ", a->imod->fileName);
     dialog = XtVaCreatePopupShell
	  (Dia_title,  
/*	   topLevelShellWidgetClass, */
	   xmDialogShellWidgetClass,   
	   Dia_toplevel,
	   XmNdefaultPosition, True,
	   XmNdeleteResponse, XmDESTROY,
	   XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
	   XmNvisual,   a->visual,
	   XmNcolormap, a->cmap,
	   XmNdepth,    a->depth,
	   XmNtitle,    window_name,
	   NULL);
     XtVaSetValues(dialog, XmNdefaultPosition, True, NULL);
     XtAddCallback(dialog, XmNpopupCallback, map_cb, NULL);
     if (window_name)
          free(window_name);

     pane = XtVaCreateWidget
	  ("pane", xmPanedWindowWidgetClass, dialog,
	   XmNsashWidth,  1,
	   XmNsashHeight, 1,
	   NULL);

     form = XtVaCreateWidget
	  ("form1", xmFormWidgetClass, pane, 
	   NULL);

     topcol = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, form,
	   XmNtopAttachment,    XmATTACH_FORM,
	   XmNleftAttachment,  XmATTACH_FORM,
	   XmNrightAttachment,  XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
/*	   XmNorientation, XmHORIZONTAL, */
	   NULL);
     col = topcol;

/*     col = XtVaCreateManagedWidget  */
/*	  ("rowcol", xmRowColumnWidgetClass, topcol,  */
/*	   NULL);  */

     /**********************************************************************/
     /*  Work area. */

     row = XtVaCreateManagedWidget
	  ("arrowcol", xmRowColumnWidgetClass, col,
	   XmNorientation, XmHORIZONTAL, NULL);
     arrow = XtVaCreateManagedWidget
	  ("arrow_up", xmArrowButtonWidgetClass, row,
	   XmNarrowDirection, XmARROW_UP, NULL);
     XtAddCallback(arrow, XmNarmCallback, imodv_zoom_cb, (XtPointer)1);
     arrow = XtVaCreateManagedWidget
	  ("arrow_up", xmArrowButtonWidgetClass, row,
	   XmNarrowDirection, XmARROW_DOWN, NULL);
     XtAddCallback(arrow, XmNarmCallback, imodv_zoom_cb, (XtPointer)-1);
     button = XtVaCreateManagedWidget
	  ("Zoom         Scale",xmLabelWidgetClass, row, NULL);
     wScale = XtVaCreateManagedWidget
	  ("text", xmTextWidgetClass, row, 
	   XmNcolumns, 8, 
	   NULL);
     XtAddCallback(wScale, XmNactivateCallback,
		   imodv_control_scale_cb, (XtPointer)0);

     

     Imodv_nearscale_widget = XtVaCreateManagedWidget
	  ("clip_scale_near", xmScaleWidgetClass, col,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 999,
	   XmNshowValue, True,
	   XmNvalue, (int)0,
	   XmNscaleMultiple, 1,
	   /*  XtVaTypedArg, XmNtitleString,
	   XmRString, "Near", 5, */
	   NULL);
     XtAddCallback(Imodv_nearscale_widget, XmNvalueChangedCallback,
		   imodv_control_clip_cb, 0);
     XtVaCreateManagedWidget("Near", xmLabelWidgetClass, col, NULL);

     Imodv_farscale_widget = XtVaCreateManagedWidget
	  ("clip_scale_far", xmScaleWidgetClass, col,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 1,
	   XmNmaximum, 1000,
	   XmNshowValue, True,
	   XmNvalue, (int)1000,
	   XmNscaleMultiple, 1,
	   /*   XtVaTypedArg, XmNtitleString,
	   XmRString, "Far", 4, */
	   NULL);
     XtAddCallback(Imodv_farscale_widget, XmNvalueChangedCallback,
		   imodv_control_clip_cb, (XtPointer)1);
     XtVaCreateManagedWidget("Far", xmLabelWidgetClass, col, NULL);

     Imodv_fovyscale_widget = XtVaCreateManagedWidget
	  ("clip_scale", xmScaleWidgetClass, col,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 89,   
	   XmNshowValue, True,
	   XmNvalue, (int)Imodv->fovy,
	   XmNscaleMultiple, 1,
	   /* XtVaTypedArg, XmNtitleString,
	   XmRString, "Perspective", 12, */
	   NULL);
     XtAddCallback(Imodv_fovyscale_widget, XmNvalueChangedCallback,
		   imodv_control_clip_cb, (XtPointer)2);
     XtVaCreateManagedWidget("Perspective", xmLabelWidgetClass, col, NULL);

     Imodv_zscale_widget = XtVaCreateManagedWidget
	  ("clip_scale", xmScaleWidgetClass, col,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 1,
     	   XmNmaximum, 500,
	   XmNdecimalPoints, 1,
	   XmNshowValue, True,
	   XmNvalue, (int)(a->mod[a->cm]->zscale * 10),
	   XmNscaleMultiple, 1,
	   /* XtVaTypedArg, XmNtitleString,
	   XmRString, "Z-scale", 8, */
	   NULL);

     XtAddCallback(Imodv_zscale_widget, XmNvalueChangedCallback,
		   imodv_control_zscale_cb, NULL);
     XtVaCreateManagedWidget("Z-scale", xmLabelWidgetClass, col, NULL);
   
     /* This is done in Object edit instead */
     /*     str  = XmStringCreateSimple("Show");
     s1   = XmStringCreateSimple("All ON Objects");
     s2   = XmStringCreateSimple("Current Object Only");
     s3   = XmStringCreateSimple("Current Surface Only");
     s5   = XmStringCreateSimple("Surface & Other Objs");
     s4   = XmStringCreateSimple("Current Contour Only");
     s6   = XmStringCreateSimple("Contour & Other Objs");
     menu = XmVaCreateSimpleOptionMenu
	  (col, "option_menu",
	   str, 'S', Imodv->current_subset, subset_cb,
	   XmVaPUSHBUTTON, s1, 'A', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'O', NULL, NULL,
	   XmVaPUSHBUTTON, s3, 'S', NULL, NULL,
	   XmVaPUSHBUTTON, s5, 'U', NULL, NULL,
	   XmVaPUSHBUTTON, s4, 'C', NULL, NULL,
	   XmVaPUSHBUTTON, s6, 'N', NULL, NULL,
	   XmNvisual,   Imodv->visual,
	   NULL);
     XmStringFree(str);
     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(s4);
     XmStringFree(s5);
     XmStringFree(s6);
     XtManageChild(menu);
     */

     frame = XtVaCreateManagedWidget
	  ("frame", xmFrameWidgetClass, col, NULL);
     lcol = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, frame, NULL);
     XtVaCreateManagedWidget
	  ("rotation", xmLabelWidgetClass, lcol, NULL);

     row = XtVaCreateManagedWidget
	  ("arrowcol", xmRowColumnWidgetClass, lcol,
	   XmNorientation, XmHORIZONTAL, NULL);
     arrow = XtVaCreateManagedWidget
	  ("arrow_up", xmArrowButtonWidgetClass, row,
	   XmNarrowDirection, XmARROW_UP, NULL);
     XtAddCallback(arrow, XmNarmCallback, imodv_control_axis_cb, (XtPointer)4);
     arrow = XtVaCreateManagedWidget
	  ("arrow_up", xmArrowButtonWidgetClass, row,
	   XmNarrowDirection, XmARROW_DOWN, NULL);
     XtAddCallback(arrow, XmNarmCallback, imodv_control_axis_cb, (XtPointer)6);
     button = XtVaCreateManagedWidget
	  ("x-axis",xmLabelWidgetClass, row, NULL);     
     wXrot = XtVaCreateManagedWidget
	  ("text", xmTextWidgetClass, row, XmNcolumns, 12, NULL);
     XtAddCallback(wXrot, XmNactivateCallback,
		   imodv_control_axis_cb, (XtPointer)10);

     row = XtVaCreateManagedWidget
	  ("arrowcol", xmRowColumnWidgetClass, lcol,
	   XmNorientation, XmHORIZONTAL, NULL);
     arrow = XtVaCreateManagedWidget
	  ("arrow_up", xmArrowButtonWidgetClass, row,
	   XmNarrowDirection, XmARROW_UP, NULL);
     XtAddCallback(arrow, XmNarmCallback, imodv_control_axis_cb, (XtPointer)8);
     arrow = XtVaCreateManagedWidget
	  ("arrow_up", xmArrowButtonWidgetClass, row,
	   XmNarrowDirection, XmARROW_DOWN, NULL);
     XtAddCallback(arrow, XmNarmCallback, imodv_control_axis_cb, (XtPointer)2);
     button = XtVaCreateManagedWidget
	  ("y-axis",xmLabelWidgetClass, row, NULL);
     wYrot = XtVaCreateManagedWidget
	  ("text", xmTextWidgetClass, row, XmNcolumns, 12, NULL);
     XtAddCallback(wYrot, XmNactivateCallback,
		   imodv_control_axis_cb, (XtPointer)11);

     row = XtVaCreateManagedWidget
	  ("arrowcol", xmRowColumnWidgetClass, lcol,
	   XmNorientation, XmHORIZONTAL, NULL);
     arrow = XtVaCreateManagedWidget
	  ("arrow_up", xmArrowButtonWidgetClass, row,
	   XmNarrowDirection, XmARROW_UP, NULL);
     XtAddCallback(arrow, XmNarmCallback, imodv_control_axis_cb, (XtPointer)9);
     arrow = XtVaCreateManagedWidget
	  ("arrow_up", xmArrowButtonWidgetClass, row,
	   XmNarrowDirection, XmARROW_DOWN, NULL);
     XtAddCallback(arrow, XmNarmCallback, imodv_control_axis_cb, (XtPointer)3);
     button = XtVaCreateManagedWidget
	  ("z-axis",xmLabelWidgetClass, row, NULL);
     wZrot = XtVaCreateManagedWidget
	  ("text", xmTextWidgetClass, row, XmNcolumns, 12, NULL);
     XtAddCallback(wZrot, XmNactivateCallback,
		   imodv_control_axis_cb, (XtPointer)12);

     button = XtVaCreateManagedWidget
     ("Start/Stop Rotation", xmPushButtonWidgetClass, lcol, NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   imodv_control_axis_cb, (XtPointer)-1);

     Imodv_rotscale_widget = XtVaCreateManagedWidget
	  ("clip_scale", xmScaleWidgetClass, lcol,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 1,
	   XmNmaximum, ROTATION_MAX,
	   XmNdecimalPoints, 1,
	   XmNshowValue, True,
	   XmNvalue, a->md->arot,
	   XmNscaleMultiple, 1,
	   /*  XtVaTypedArg, XmNtitleString,
	   XmRString, "Degrees", 8, */
	   NULL);
     XtAddCallback(Imodv_rotscale_widget, XmNvalueChangedCallback,
		   imodv_control_axis_cb, NULL);
     XtVaCreateManagedWidget("Degrees", xmLabelWidgetClass, lcol, NULL);


     XtManageChild (topcol);
     XtManageChild (form);
     imodvControlUpdate(a);

/*****************************************************************************/
/* ACTION AREA                                                               */
     form = XtVaCreateWidget ("form2", xmFormWidgetClass, pane,
			      XmNfractionBase,    7,
			      NULL);
     
     button = XtVaCreateManagedWidget
	  ("OK", xmPushButtonWidgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         1,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        3,
	   XmNshowAsDefault,        True,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   imodv_control_quit_cb, (XtPointer)&dialog);

     button = XtVaCreateManagedWidget
	  ("HELP", xmPushButtonWidgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         4,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        6,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   imodv_control_help_cb, NULL);

     XtManageChild(form);
     XtVaGetValues (button, XmNheight, &h, NULL);
     XtVaSetValues (form, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
     XtManageChild (pane);

     wmclose = XmInternAtom( XtDisplay(dialog), "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(dialog, wmclose, imodv_control_quit_cb,
			     ( caddr_t )&dialog);

     XtPopup (dialog, XtGrabNone);
     return(0);
}
