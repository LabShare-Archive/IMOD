/*****************************************************************************
 *                                                                           *
 *   FILE: dia_msg.c                                                         *
 *                                                                           *
 *   PURPOSE: Print a message type dialog.                                   *
 *                                                                           *
 *   HISTORY:                                                                *
 *       Version 1.0  James Kremer                                           *
 *       Version 1.1  David Mastronarde  mast@colorado.edu                   *
 *                                                                           *
 *****************************************************************************
 *   Copyright (C) 1994-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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
#include <stdlib.h>
#include <sys/types.h>
#include <ctype.h>
#include <Xm/DialogS.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/PushBG.h>
#include <Xm/LabelG.h>
#include <Xm/PanedW.h>
#include <stdarg.h>
#include "diaP.h"


void dia_destroy_shell(Widget widget, XtPointer client, XtPointer call)
{
     Widget shell = (Widget) client;
     XtDestroyWidget(shell);
}

void dia_vasmsg(char *msg, ...)
{
     char **argv;
     char *emsg;
     char *tmsg;
     int argc = 0;
     va_list ap;

     tmsg = msg;
     va_start(ap, msg);
     while( emsg = va_arg(ap, char *)){
	  argc++;
     }
     va_end(ap);

     argv = (char **)malloc((argc + 2) * sizeof(char *));

     argc = 1;
     va_start(ap, msg);
     argv[0] = tmsg;
     while( emsg = va_arg(ap, char *)){
	  argv[argc] = emsg;
	  argc++;
     }
     argv[argc] = NULL;
     va_end(ap);
     dia_smsg(argv);
     free(argv);
}


/* Scrolled message */

void dia_smsg( char **msg)
{
     Widget help_dialog, pane, text_w, form, sep, widget, label;
     Pixmap pixmap;
     Pixel fg, bg;
     Arg args[10];
     char *p;
     char *buf;
     int maxline, maxrow, linesize;
     long bufsize;
     Dimension h;
     int n = 0;
     int i;
     int lastspace, curpos;

     /* DNM: Make it a topLevelShell instead of a xmDialogShell so that it 
	won't park when the info window is parked */
     help_dialog = XtVaCreatePopupShell 
          (Dia_title, topLevelShellWidgetClass, Dia_toplevel,
	   /*	  (Dia_title, xmDialogShellWidgetClass, Dia_toplevel, */
	   XmNvisual, dia_getvisual(),
	   XmNdefaultPosition, True, 
	   XmNdeleteResponse, XmDESTROY,
	   NULL);

     XtAddCallback(help_dialog, XmNpopupCallback, dia_map_cb, NULL);

     pane = XtVaCreateWidget 
	  ("pane", xmPanedWindowWidgetClass, help_dialog,
	   XmNsashWidth,  1, /* PanedWindow won't let us set these to 0! */
	   XmNsashHeight, 1, /* Make small so user doesn't try to resize */
	   NULL);
     
     form = XtVaCreateWidget 
	  ("form1", xmFormWidgetClass, pane,
	   NULL);

     XtVaGetValues (form,  /* once created, we can get its colors */
		    XmNforeground, &fg,
		    XmNbackground, &bg,
		    NULL);

     label = XtVaCreateManagedWidget 
	  ("label", xmLabelGadgetClass, form,
	   XmNlabelType,        XmPIXMAP,
	   XmNleftAttachment,   XmATTACH_FORM,
	   XmNtopAttachment,    XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,

	   NULL);

     for (i = 0, bufsize = 0; msg[i]; i++){
	  linesize = strlen(msg[i]);
	  bufsize += linesize;
     }

     buf = (char *)malloc(bufsize + i + 1);
     p = buf;
     for (p = buf, i = 0; msg[i]; i++) {
	  p += strlen (strcpy (p, msg[i]));
	  /* DNM: this macro call caused program built on Irix 6.5 to not run
	     on earlier Irix's.  Casting as (int) didn't help - just do 
	     explicit tests */
	  /*if (!isspace (p[-1]))  spaces, tabs and newlines are spaces.. */
	  if (p[-1] != ' ' && p[-1] != '\t' && p[-1] != '\n')
	       *p++ = ' '; /* lines are concatenated, insert a space */
     }
     *--p = 0; /* get rid of trailing space... */

/* DNM: count the actual lines and their lengths to get the right size window*/

     maxline = 0;
     maxrow = 1;
     curpos = 0;
     lastspace = 40;
     for (p = buf; *p; p++) {
       if (*p == '\t')
	      curpos += 8;
       if (*p == ' ') {
	      lastspace = curpos;
	      curpos++;
	    } 
       else if (*p == '\n') {
	      if (curpos >= maxline)
		      maxline = curpos + 1;
	      curpos = 0;
	      lastspace = 40;
	      maxrow++;
	    }
       else if (curpos > 78 ) {
	      if (lastspace >= maxline)
		    maxline = lastspace + 1;
	      curpos -= lastspace;
	      lastspace = 40;
	      maxrow++;
	    }
       else
	 curpos++;
     }

     if (!maxline)
	  maxline = curpos + 1;

     if (maxrow > 50)
	  maxrow = 40;
		  
     XtSetArg (args[n], XmNscrollVertical,        True); n++;
     XtSetArg (args[n], XmNscrollHorizontal,      False); n++;
     XtSetArg (args[n], XmNeditMode,              XmMULTI_LINE_EDIT); n++;
     XtSetArg (args[n], XmNeditable,              False); n++;
     XtSetArg (args[n], XmNcursorPositionVisible, False); n++;
     XtSetArg (args[n], XmNwordWrap,              True); n++;
     XtSetArg (args[n], XmNvalue,                 buf); n++;
     XtSetArg (args[n], XmNrows,                  maxrow); n++;
     XtSetArg (args[n], XmNcolumns,               maxline); n++;
     text_w = XmCreateScrolledText(form, "help_text", args, n);
     /* Attachment values must be set on the Text widget's PARENT,
      * the ScrolledWindow. This is the object that is positioned.
      */
 

     XtVaSetValues (XtParent (text_w),
		    XmNleftAttachment,   XmATTACH_WIDGET,
		    XmNleftWidget,       label,
		    XmNtopAttachment,    XmATTACH_FORM,
		    XmNrightAttachment,  XmATTACH_FORM,
		    XmNbottomAttachment, XmATTACH_FORM,
		    NULL);
     XtManageChild (text_w);
     XtManageChild (form);

     /* Create another form to act as the action area for the dialog */
     form = XtVaCreateWidget ("form2", xmFormWidgetClass, pane,
			      XmNfractionBase,    5,
			      NULL);
     
     /* The OK button is under the pane's separator and is
      * attached to the left edge of the form.  It spreads from
      * position 0 to 1 along the bottom (the form is split into
      * 5 separate grids via XmNfractionBase upon creation).
      */
     widget = XtVaCreateManagedWidget 
	  ("OK",
	   xmPushButtonGadgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         2,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        3,
	   XmNshowAsDefault,        True,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(widget,XmNactivateCallback, dia_destroy_shell, help_dialog);

     XtManageChild (form);

     XtVaGetValues (widget, XmNheight, &h, NULL);
     XtVaSetValues (form, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);

/*     dia_map_cb(help_dialog, NULL, NULL);  */
     XtManageChild (pane);

     
     XtPopup (help_dialog, XtGrabNone);
     free(buf);
}
