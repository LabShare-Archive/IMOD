/*****************************************************************************
 *                                                                           *
 *   FILE: dia_ask.c                                                         *
 *                                                                           *
 *   PURPOSE: Ask a question dialog.                                         *
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

#include <Xm/DialogS.h>
#include <Xm/SelectioB.h>
#include <Xm/MessageB.h>
#include "diaP.h"

void dia_ask_responce(Widget w, XtPointer client, XtPointer call)
{
     int *answer = (int *)client;
     XmAnyCallbackStruct *cbs = (XmAnyCallbackStruct *)call;

     switch (cbs->reason){
	case XmCR_OK:
	  *answer = 2;
	  break;
	  
	case XmCR_CANCEL:
	  *answer = 1;
	  break;

	default:
	  return;
     }
}

int dia_ask(char *question)
{
     static Widget dialog;
     static int done;
     Arg args[1];
     XmString text, yes, no;

     if (!dialog){
	  XtSetArg(args[0], XmNvisual, dia_getvisual());
	  dialog = XmCreateQuestionDialog(Dia_toplevel, Dia_title, args, 1);
	  yes = XmStringCreateSimple("Yes");
	  no  = XmStringCreateSimple("No");
	  XtVaSetValues(dialog,
			XmNdialogStyle,       XmDIALOG_FULL_APPLICATION_MODAL,
			XmNokLabelString,     yes,
			XmNcancelLabelString, no,
			NULL);
	  XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
	  XtAddCallback(dialog, XmNokCallback, dia_ask_responce, &done);
	  XtAddCallback(dialog, XmNcancelCallback, dia_ask_responce, &done);
	  XmStringFree(yes);
	  XmStringFree(no);
     }
     done = 0;

     text = XmStringCreateSimple(question);
     XtVaSetValues(dialog, XmNmessageString, text, NULL);
     XmStringFree(text);
     XtManageChild(dialog);
     XtPopup(XtParent(dialog), XtGrabNone);

     while (!done)
	  XtAppProcessEvent(Dia_context, XtIMAll);

     XtPopdown(XtParent(dialog));
     return(done - 1);
}
