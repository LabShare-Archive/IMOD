/*****************************************************************************
 *                                                                           *
 *   FILE: dia_abort.c                                                       *
 *                                                                           *
 *   PURPOSE: Dialog to abort forked processes.                              *
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
#include <unistd.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <signal.h>

#include <Xm/MessageB.h>
#include <Xm/SelectioB.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include "diaP.h"
#include "clock1.symbol"
#include "clock2.symbol"

#include "clock3.symbol"
#include "clock4.symbol"

struct dia_abort_data{
     XtIntervalId   id;
     int            which;
     Pixmap         pix1, pix2, pix3, pix4;
     Widget         dialog;
     XtAppContext   app;
};

Widget Dia_abort_widget;
int    Dia_abort_done;
int    Dia_abort_cpid;



/*****************************************************************************/
/* library functions:
 * abort - gives user a chance to abort a child process.
 * wait  - makes user wait for child process to finish.
 *
 * input:  message - message printed inside dialog.
 *         cpid    - child process id number.
 *
 * returns True if child process was manually aborted.
 * 
 */

int dia_cpid_control(char *msg, int cpid, int abort);

int dia_abort(char *message, int cpid)
{
     return(dia_cpid_control(message, cpid, 1));
}

int dia_wait(char *message, int cpid)
{
     return(dia_cpid_control(message, cpid, 0));
}


/*****************************************************************************/
/*  support functions                                                        */
/*****************************************************************************/
void dia_abort_cb(Widget w, XtPointer client, XtPointer call)
{
     Dia_abort_done = (int)client;

     if (Dia_abort_done)
	   kill(Dia_abort_cpid, 9);
}		       

void dia_abort_help_cb(Widget w, XtPointer client, XtPointer call)
{
     dia_puts("Please wait for child process to finish.");
}
     

void dia_abort_setmsg(Widget w, char *msg, int dots)
{

     XmString text, gauge, label;
     
     text = XmStringCreateSimple(msg);

     switch (dots){
	case 1:
	  gauge = XmStringCreateSimple(" (\\)");
	  break;
	case 2:
	  gauge = XmStringCreateSimple(" (|)");
	  break;
	case 3:
	  gauge = XmStringCreateSimple(" (/)");
	  break;
	case 4:
	  gauge = XmStringCreateSimple(" (-)");
	  break;
	case 0:
	default:
	  gauge = XmStringCreateSimple("");
	  break;
     }
     label = XmStringConcat(text, gauge);
     XtVaSetValues (w,
		    XmNmessageString, label,
		    NULL);
     XmStringFree(text);
     XmStringFree(gauge);
     XmStringFree(label);
}



void dia_abort_cycle(XtPointer client, XtIntervalId *id)
{
     struct dia_abort_data *data = (struct dia_abort_data *)client;
     XEvent event_return;

     data->id = XtAppAddTimeOut (data->app, 1000L, dia_abort_cycle, data);

     switch (data->which){
	case 1:
	  XtVaSetValues (data->dialog, XmNsymbolPixmap, data->pix1, NULL);
	  data->which++;
	  break;
	case 2:
	  XtVaSetValues (data->dialog, XmNsymbolPixmap, data->pix2, NULL);
	  data->which++;
	  break;
	case 3:
	  XtVaSetValues (data->dialog, XmNsymbolPixmap, data->pix3, NULL);
	  data->which++;
	  break;
	case 4:
	  XtVaSetValues (data->dialog, XmNsymbolPixmap, data->pix4, NULL);
	  data->which = 1;
	  break;
     }
     while(XtAppPending(Dia_context)){
	  XtAppNextEvent(Dia_context, &event_return);
	  XtDispatchEvent(&event_return);
     }
}



int dia_cpid_control(char *msg, int cpid, int abort)
{
     int statptr;
     pid_t wstat;
     XEvent event_return;
     Widget dialog;
     int dots = 1;
     XmString text, abort_label;
     struct dia_abort_data *data = XtNew(struct dia_abort_data);
     Display *dpy = XtDisplay(Dia_toplevel);
     Screen  *screen = XtScreen(Dia_toplevel);
     Pixel   fg, bg;
     int depth;

     Arg args[1];


     XmFontList fontlist;
     XFontStruct *font;

     Dia_abort_done = 0;
     Dia_abort_cpid = cpid;
     
/*     font = XLoadQueryFont(XtDisplay(Dia_toplevel), 
			   "-*-courier-*-r-*--*-140-*");

     if (!font)
	  dia_err( "abort dialog: error loading font.\n");
*/
     XtSetArg(args[0], XmNvisual, dia_getvisual());
     dialog = XmCreateMessageDialog(Dia_toplevel, Dia_title, args, 1);
     text = XmStringCreateSimple(msg);
     if (abort)
	  abort_label = XmStringCreateSimple("abort");
     else
	  abort_label = XmStringCreateSimple("wait");

     XtVaGetValues(dialog,
		   XmNforeground, &fg,
		   XmNbackground, &bg,
		   XmNdepth, &depth,
		   NULL);

     data->pix1 = XCreatePixmapFromBitmapData 
	  (dpy, XtWindow (Dia_toplevel),
	   clock1_bits, clock1_width, clock1_height, fg, bg, depth);
     data->pix2 = XCreatePixmapFromBitmapData 
	  (dpy, XtWindow (Dia_toplevel),
	   clock2_bits, clock2_width, clock2_height, fg, bg, depth);
     data->pix3 = XCreatePixmapFromBitmapData 
	  (dpy, XtWindow (Dia_toplevel),
	   clock3_bits, clock3_width, clock3_height, fg, bg, depth);
     data->pix4 = XCreatePixmapFromBitmapData 
	  (dpy, XtWindow (Dia_toplevel),
	   clock4_bits, clock4_width, clock4_height, fg, bg, depth);
     data->dialog = dialog;
     data->app = XtWidgetToApplicationContext(Dia_toplevel);
     data->which = 1;

     XtVaSetValues (dialog,
		    XmNsymbolPixmap, data->pix1,
		    XmNdialogType, XmDIALOG_INFORMATION,
		    XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
		    XmNokLabelString, abort_label,
		    XmNdefaultButton, 
		    XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON),
		    NULL);
     dia_abort_setmsg(dialog, msg, 0);
     XmStringFree(text);
     XmStringFree(abort_label);
     
     if (abort){
	  XtAddCallback(dialog, XmNokCallback, dia_abort_cb, (XtPointer)abort);
	  XtUnmanageChild(XmMessageBoxGetChild(dialog, XmDIALOG_HELP_BUTTON));
     }
     else{
	  XtAddCallback(dialog, XmNhelpCallback, dia_abort_help_cb, 
			(XtPointer)abort);
	  text = XmStringCreateSimple("Wait");
	  XtVaSetValues (dialog,
			 XmNhelpLabelString, text, NULL);
	  XmStringFree(text);
	  XtUnmanageChild(XmMessageBoxGetChild(dialog,
					       XmDIALOG_OK_BUTTON));
     }
     XtUnmanageChild(XmMessageBoxGetChild(dialog,
					  XmDIALOG_CANCEL_BUTTON));

     XtManageChild(dialog);
     data->id = XtAppAddTimeOut (data->app, 1000L, dia_abort_cycle, data);
     while (! Dia_abort_done){

	  while(XtAppPending(Dia_context)){
	       XtAppNextEvent(Dia_context, &event_return);
	       XtDispatchEvent(&event_return);
	  }

	  wstat = waitpid(cpid, (int *)&statptr, WNOHANG); 

	  if (!((wstat != cpid) && (wstat >= 0))  ){
	       Dia_abort_done = 2;
	  }
     }

     XtRemoveTimeOut (data->id);
     XFreePixmap (XtDisplay (data->dialog), data->pix1);
     XFreePixmap (XtDisplay (data->dialog), data->pix2);
     XtFree ((char *)data);
     
     XtDestroyWidget(dialog);     
     return(Dia_abort_done - 1);
}



