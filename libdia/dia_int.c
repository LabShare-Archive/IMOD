/*****************************************************************************
 *                                                                           *
 *   FILE: dia_int.c                                                         *
 *                                                                           *
 *   PURPOSE: Get an integer dialog.                                         *
 *                                                                           *
 *   HISTORY:                                                                *
 *       Version 1.0  James Kremer  kremer@beagle.colorado.edu               *
 *                                                                           *
 *****************************************************************************
 *   Copyright (C) 1994,1995 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <stdlib.h>
#include <Xm/DialogS.h>
#include <Xm/PushBG.h>
#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>
#include "diaP.h"


struct Dia_changeint
{
     int oval;
     int *cval;
     Widget w;
};


void dia_int_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     int *value = (int *)client_data;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
     *value = cbs->value;
}

void dia_int_ok_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     int *done = (int *)client_data;
     *done = 1;
}


void dia_int_cancel_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     int *done = (int *)client_data;
     *done = -1;
}

void dia_cint_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Dia_changeint *ci = (struct Dia_changeint *)client_data;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
     *(ci->cval) = cbs->value;
}

void dia_cint_ok_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Dia_changeint *ci = (struct Dia_changeint *)client_data;
     XtDestroyWidget(ci->w);
     free(ci);
}

void dia_cint_cancel_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Dia_changeint *ci = (struct Dia_changeint *)client_data;
     *(ci->cval) = ci->oval;
     XtDestroyWidget(ci->w);
     free(ci);
}


int dia_int(int low, int high, int value, int decimal, char *prompt)
{

     Widget dialog, pane, form;
     Widget label, scale, button;
     Dimension h;
     int done = 0;
     int retval = value;
     int x, y;


     dialog = XtVaCreatePopupShell 
	  (Dia_title,  xmDialogShellWidgetClass,  Dia_toplevel,
	   XmNvisual, dia_getvisual(),
	   XmNdefaultPosition, True,
	   XmNdeleteResponse, XmDESTROY,
	   XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
	   NULL);
     if (!dialog)
	  return(value);
     XtAddCallback(dialog, XmNpopupCallback, dia_map_cb, NULL);
     pane = XtVaCreateWidget
	  ("pane", xmPanedWindowWidgetClass, dialog,
	   XmNsashWidth,  1,
	   XmNsashHeight, 1,
	   NULL);

     form = XtVaCreateWidget
	  ("form1", xmFormWidgetClass, pane, NULL);
     
     
     label = XtVaCreateManagedWidget
	  (prompt, xmLabelWidgetClass, form,
	   XmNleftAttachment,  XmATTACH_FORM,
	   XmNtopAttachment,    XmATTACH_FORM,
	   NULL);
     
     scale = XtVaCreateManagedWidget
	   ("scale", xmScaleWidgetClass, form,
	    XmNorientation, XmHORIZONTAL,
	    XmNprocessingDirection, XmMAX_ON_RIGHT,
	    XmNmaximum, high,
	    XmNminimum, low,
	    XmNvalue, value,
	    XmNdecimalPoints, decimal,
	    XmNshowValue, True,
	    XmNscaleMultiple, 1,
	    XmNtopAttachment, XmATTACH_WIDGET,
	    XmNtopWidget, label,
	    XmNleftAttachment,   XmATTACH_FORM,
	    XmNrightAttachment,  XmATTACH_FORM, 
	    XmNbottomAttachment, XmATTACH_FORM,
	    NULL);
     XtAddCallback(scale, XmNvalueChangedCallback, 
		   dia_int_cb, &retval);   
     XtManageChild (form);

     /* ACTION AREA */
     form = XtVaCreateWidget ("form2", xmFormWidgetClass, pane,
			      XmNfractionBase,    7,
			      NULL);
     
     button = XtVaCreateManagedWidget
	  ("OK", xmPushButtonGadgetClass, form,
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
		   dia_int_ok_cb, &done);

     button = XtVaCreateManagedWidget
	  ("Cancel", xmPushButtonGadgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         4,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        6,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   dia_int_cancel_cb, &done);

     XtManageChild(form);
     XtVaGetValues (button, XmNheight, &h, NULL);
     XtVaSetValues (form, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);

     XtManageChild (pane);

     XtPopup (dialog, XtGrabNone);

     while (!done)
	   XtAppProcessEvent(Dia_context, XtIMAll);
     
     XtDestroyWidget(dialog);

     if (done < 0)
	  retval = value;

     return(retval);

}


void dia_cint(int *val, int low, int high, int decimal, char *prompt)
{
     struct Dia_changeint *ci;
     Widget dialog, pane, form;
     Widget label, scale, button;
     Dimension h;
     int x, y;


     ci = (struct Dia_changeint *)malloc(sizeof(struct Dia_changeint));
     
     dialog = XtVaCreatePopupShell 
	  (Dia_title,  xmDialogShellWidgetClass,  Dia_toplevel,
	   XmNdefaultPosition, True,
	   XmNdeleteResponse, XmDESTROY,
	   XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
	   NULL);
     if (!dialog)
	  return;
     XtAddCallback(dialog, XmNpopupCallback, dia_map_cb, NULL);
     ci->w = dialog;
     ci->oval = *val;
     ci->cval = val;

     pane = XtVaCreateWidget
	  ("pane", xmPanedWindowWidgetClass, dialog,
	   XmNsashWidth,  1,
	   XmNsashHeight, 1,
	   NULL);

     form = XtVaCreateWidget
	  ("form1", xmFormWidgetClass, pane, NULL);
     
     
     label = XtVaCreateManagedWidget
	  (prompt, xmLabelWidgetClass, form,
	   XmNleftAttachment,  XmATTACH_FORM,
	   XmNtopAttachment,    XmATTACH_FORM,
	   NULL);
     
     scale = XtVaCreateManagedWidget
	   ("scale", xmScaleWidgetClass, form,
	    XmNorientation, XmHORIZONTAL,
	    XmNprocessingDirection, XmMAX_ON_RIGHT,
	    XmNmaximum, high,
	    XmNminimum, low,
	    XmNvalue, ci->oval,
	    XmNdecimalPoints, decimal,
	    XmNshowValue, True,
	    XmNscaleMultiple, 1,
	    XmNtopAttachment, XmATTACH_WIDGET,
	    XmNtopWidget, label,
	    XmNleftAttachment,   XmATTACH_FORM,
	    XmNrightAttachment,  XmATTACH_FORM, 
	    XmNbottomAttachment, XmATTACH_FORM,
	    NULL);
     XtAddCallback(scale, XmNvalueChangedCallback, 
		   dia_cint_cb, ci);   
     XtManageChild (form);

     /* ACTION AREA */
     form = XtVaCreateWidget ("form2", xmFormWidgetClass, pane,
			      XmNfractionBase,    7,
			      NULL);
     
     button = XtVaCreateManagedWidget
	  ("OK", xmPushButtonGadgetClass, form,
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
		   dia_cint_ok_cb, ci);

     button = XtVaCreateManagedWidget
	  ("Cancel", xmPushButtonGadgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         4,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        6,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   dia_cint_cancel_cb, ci);

     XtManageChild(form);
     XtVaGetValues (button, XmNheight, &h, NULL);
     XtVaSetValues (form, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);

     XtManageChild (pane);

     dia_map_cb(dialog, NULL, NULL);
     XtPopup (dialog, XtGrabNone);
}



