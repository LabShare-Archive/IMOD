/*****************************************************************************
 *                                                                           *
 *   FILE: dia_file.c                                                        *
 *                                                                           *
 *   PURPOSE: Get a filename dialog.                                         *
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

#include <stdio.h>
#include <Xm/DialogS.h>
#include <Xm/MessageB.h>
#include <Xm/PushBG.h>
#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include "diaP.h"


static int Done;
static int Pipe;

static void cancel_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
    Done = 2;
}

static void ok_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
    Done = 1;
}

static void pipe_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
    if (Pipe)
	Pipe = 0;
    else
	Pipe = 1;
}

FILE *dia_print(char *defaultValue)
{
    FILE *fp;
    Widget label, scale, button;
    Widget window, rowcol;
    char *filename;
    Arg args[1];
    XEvent event_return;
    Widget dialog, pane, form;
    Widget textw;
    Dimension h;
    
    Done = 0;
    Pipe = 1;

     dialog = XtVaCreatePopupShell
          (Dia_title,  xmDialogShellWidgetClass,  Dia_toplevel,
           XmNdefaultPosition, True,
           XmNdeleteResponse, XmDESTROY,
           XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
	   XmNvisual, dia_getvisual(),
           NULL);

     pane = XtVaCreateWidget
          ("pane", xmPanedWindowWidgetClass, dialog,
           XmNsashWidth,  1,
           XmNsashHeight, 1,
           NULL);

     form = XtVaCreateWidget
          ("form1", xmFormWidgetClass, pane, NULL);


    rowcol = XtVaCreateWidget
          ("rowcol", xmRowColumnWidgetClass, form,
           XmNtopAttachment,    XmATTACH_FORM,
           XmNleftAttachment,  XmATTACH_FORM,
           XmNrightAttachment,  XmATTACH_FORM,
           XmNbottomAttachment, XmATTACH_FORM,
           NULL);


     /**********************************************************************/
     /*  Work area. */
     XtVaCreateManagedWidget("Print Command", 
			     xmLabelWidgetClass, rowcol, NULL);
    /*
    button = XtVaCreateManagedWidget
	("Save to File", xmToggleButtonWidgetClass, rowcol, NULL);
	      );
	      XtAddCallback(button, XmNactivateCallback,
	      pipe_cb, &done);

    */
    
    textw = XtVaCreateManagedWidget
	 ("lpr",  xmTextWidgetClass, rowcol, NULL);
    XtAddCallback(textw, XmNactivateCallback,
		  ok_cb, &Done);
    

    XmTextSetString(textw, defaultValue); 
     XtManageChild (rowcol);
     XtManageChild (form);

     /* ACTION AREA */
     form = XtVaCreateWidget ("form2", xmFormWidgetClass, pane,
			      XmNfractionBase,    11,
			      NULL);
     
     button = XtVaCreateManagedWidget
	  ("Print", xmPushButtonGadgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         1,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        5,
	   XmNshowAsDefault,        True,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   ok_cb, &Done);

     button = XtVaCreateManagedWidget
	  ("Cancel", xmPushButtonGadgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         6,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        10,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   cancel_cb, dialog);

     XtManageChild(form);
     XtVaGetValues (button, XmNheight, &h, NULL);
     XtVaSetValues (form, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
     XtManageChild (pane);

     dia_map_cb(dialog, NULL, NULL);
     XtPopup (dialog, XtGrabNone);

     while (!Done)
	   XtAppProcessEvent(Dia_context, XtIMAll);

     XtPopdown(dialog);
     XtDestroyWidget(dialog);
     dia_input();
     XSync(XtDisplay(Dia_toplevel), 0);
     XFlush(XtDisplay(Dia_toplevel));

     while(XtAppPending(Dia_context)){
	  XtAppNextEvent(Dia_context, &event_return);
	  XtDispatchEvent(&event_return);
     }

    if (Done == 2) return NULL;
    filename = XmTextGetString(textw);
    if (!filename) return NULL;

    dialog = 0;
    if (Pipe)
	fp = popen(filename, "w");
    else
	fp = fopen(filename, "w");
    XtFree(filename);
    return(fp);
}
     
