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
 *   Copyright (C) 1994-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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
#include <Xm/FileSB.h>
#include <Xm/Protocols.h>
#include "diaP.h"




struct dia_file{
     char *reason;
     int  *done;
     char *filename;
};


void Dia_filecallback(Widget w, XtPointer client_data, XtPointer call_data)
{
     int len;
     char *text;
     struct dia_file *file = (struct dia_file *)client_data;
     XmFileSelectionBoxCallbackStruct *data =
	  (XmFileSelectionBoxCallbackStruct *) call_data;
     
     switch(data->reason){
	case 1: /* help */
	  dia_puts(file->reason);
	  break;

	case 31:
	  XmStringGetLtoR(data->value, XmSTRING_DEFAULT_CHARSET, 
			  &(file->filename));
	  len = strlen(file->filename);

	  if (file->filename[len - 1] != '/')
	       file->done[0] = 1;
	  else
	       dia_puts("No file selected!");
	  break;

	default:
	  file->done[0] = 2;
	  break;
     }
     return;
}

/* DNM: add this callback for the window delete button */
static void quit_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct dia_file *file = (struct dia_file *)client_data;
     file->done[0] = 3;
}

char *dia_filename(char *reason)
{
     static Widget dialog = 0;
     Widget fsbw;
     static int done;
     struct dia_file file;
     Arg args[1];
     XEvent event_return;
     Atom wmclose;

     if (dialog){
	  dia_err("File selection already open.");
	  return(NULL);
     }

     done = 0;
     file.reason = reason;
     file.done   = &done;
     file.filename = NULL;

     XtSetArg(args[0], XmNvisual, dia_getvisual());
     dialog = XtVaCreatePopupShell
	  ("Filename", topLevelShellWidgetClass, Dia_toplevel, 
	   XmNvisual, dia_getvisual(),
	   NULL);
     fsbw = XmCreateFileSelectionBox
	  (dialog, "FileSelection", args, 1);

/*     dialog = XmCreateFileSelectionDialog(Dia_toplevel, Dia_title, args, 1); */
     
     XtVaSetValues(dialog,
		   XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
		   NULL);
     
     XtAddCallback(fsbw, XmNokCallback, Dia_filecallback, &file);
     XtAddCallback(fsbw, XmNnoMatchCallback, Dia_filecallback, &file);
     XtAddCallback(fsbw, XmNcancelCallback, Dia_filecallback, &file);
     XtAddCallback(fsbw, XmNhelpCallback, Dia_filecallback, &file);
     XtManageChild(fsbw); 

     /* DNM: add a call if user pushed window delete button , otherwise
	window can't be reopened */
     wmclose = XmInternAtom( XtDisplay(dialog), "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(dialog, wmclose, quit_cb, (caddr_t)(&file));

     XtPopup(dialog, XtGrabNone);
/*     XtRealizeWidget(dialog); */
     while (!done)
	   XtAppProcessEvent(Dia_context, XtIMAll);

     /* DNM: if the window delete button is pushed, then window is already
	popped down by time we get here */
     if (done != 3)
	  XtPopdown(dialog);
     XtDestroyWidget(dialog);
     dia_input();
     XSync(XtDisplay(Dia_toplevel), 0);
     XFlush(XtDisplay(Dia_toplevel));

     while(XtAppPending(Dia_context)){
	  XtAppNextEvent(Dia_context, &event_return);
	  XtDispatchEvent(&event_return);
     }
     dialog = 0;
     return(file.filename);
}
     
