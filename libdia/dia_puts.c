/*****************************************************************************
 *                                                                           *
 *   FILE: dia_puts.c                                                        *
 *                                                                           *
 *   PURPOSE: Put a string to a dialog.                                      *
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

#include <Xm/SelectioB.h>
#include <Xm/MessageB.h>
#include "diaP.h"

int  Dia_error_done;
int dia_post_dialog( Widget parent, int type, int style, char *msg);


int dia_puts(char *msg)
{
    dia_post_dialog(Dia_toplevel, XmDIALOG_MESSAGE,
		    XmDIALOG_MODELESS,
		    msg);
    return(0);
}

int dia_info(char *msg)
{
     dia_post_dialog(Dia_toplevel, XmDIALOG_INFORMATION,
		     XmDIALOG_FULL_APPLICATION_MODAL,
		     msg);
     return(0);

}

int dia_err(char *msg)
{
     dia_post_dialog(Dia_toplevel, XmDIALOG_ERROR,
		     XmDIALOG_FULL_APPLICATION_MODAL,
		     msg);

     Dia_error_done = 0;

     while (!Dia_error_done)
	  XtAppProcessEvent(Dia_context, XtIMAll);

     return(0);

}


void dia_post_cb(Widget w, XtPointer client, XtPointer call)
{
     Widget dialog = (Widget)client;

     Dia_error_done = 1;
     XtDestroyWidget(dialog);
}

int dia_post_dialog( Widget parent, int type, int style, char *msg)
{
     Widget dialog;
     XmString text;
     Arg args[1];

     XtSetArg(args[0], XmNvisual, dia_getvisual());
     dialog = XmCreateMessageDialog(parent, Dia_title, args, 1);
     text = XmStringCreateSimple(msg);
     XtVaSetValues (dialog,
		    XmNmessageString, text,
		    XmNdialogType, type,
		    XmNdialogStyle, style,
		    NULL);
     XmStringFree(text);
     XtUnmanageChild(XmMessageBoxGetChild(dialog, 
					  XmDIALOG_CANCEL_BUTTON));
     XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
/*     XtAddCallback(dialog, XmNokCallback, XtDestroyWidget, NULL); */
     XtAddCallback(dialog, XmNokCallback, dia_post_cb, dialog);
     XtManageChild(dialog);
     return(0);
}



/* The callback function for the "OK" button.  Since this is not a
 * predefined Motif dialog, the "widget" parameter is not the dialog
 * itself.  That is only done by Motif dialog callbacks.  Here in the
 * real world, the callback routine is called directly by the widget
 * that was invoked.  Thus, we must pass the dialog as the client
 * data to get its handle.  (We could get it using GetTopShell(),
 * but this way is quicker, since it's immediately available.)
 */
void
DestroyShell(widget, client_data, call_data)
     Widget widget;
     XtPointer client_data;
     XtPointer call_data;
{
     Widget shell = (Widget) client_data;
     
     XtDestroyWidget(shell);
}







