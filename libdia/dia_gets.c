/*****************************************************************************
 *                                                                           *
 *   FILE: dia_gets.c                                                        *
 *                                                                           *
 *   PURPOSE: Get a string dialog.                                           *
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


int Dia_gets_done;

void dia_read_name(Widget widget, XtPointer client, XtPointer call)
{
     char **str;
     XmSelectionBoxCallbackStruct *cbs = (XmSelectionBoxCallbackStruct *)call;
     
     str = (char **)client;
     
/*     XtVaGetValues(widget, XmNvalue, str, NULL); */
     XmStringGetLtoR( cbs->value, XmSTRING_DEFAULT_CHARSET, str)     ;
     Dia_gets_done = 1;
}




char *dia_gets(char *string, char *prompt)
{
     char *str = NULL;
     Widget dialog;
     XmString xprompt;
     XmString edit;
     extern void xreq_read_name();
     Arg args[5];
     int n = 0;
     
     xprompt = XmStringCreateSimple(prompt);
     edit = XmStringCreateSimple(string);
     XtSetArg(args[n], XmNselectionLabelString, xprompt); n++;
     XtSetArg(args[n], XmNtextString, edit); n++;
     XtSetArg(args[n], XmNautoUnmanage, False); n++;
     XtSetArg(args[n], XmNvisual, dia_getvisual()); n++;
     dialog = XmCreatePromptDialog(Dia_toplevel, Dia_title, args, n);
     XmStringFree(xprompt);
     XmStringFree(edit);

     XtVaSetValues(dialog, XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
		   XmNvalue, string,
		   NULL);

     XtAddCallback (dialog, XmNokCallback, dia_read_name, &str);

     XtAddCallback(dialog, XmNcancelCallback, (XtCallbackProc)XtDestroyWidget,
		   NULL);
     
     XtSetSensitive(XmSelectionBoxGetChild(dialog, XmDIALOG_HELP_BUTTON),
		    False);

     XtManageChild(dialog);

/*     XtPopup(dialog, XtGrabNone);  */

     XtRealizeWidget(dialog);
     
     Dia_gets_done = 0;
     
     while (!Dia_gets_done)
	  XtAppProcessEvent(Dia_context, XtIMAll);

/*     XtPopdown(dialog);  */
     XtUnrealizeWidget(dialog);

     XSync(XtDisplay(dialog), 0);
     XmUpdateDisplay(Dia_toplevel);
     
     XtDestroyWidget(dialog);

     return(str);
}

