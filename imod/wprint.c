/* Written by Dan Heller and Paula Ferguson.  
 * Copyright 1994, O'Reilly & Associates, Inc.
 * Permission to use, copy, and modify this program without
 * restriction is hereby granted, as long as this copyright
 * notice appears in each copy of the program source code.
 * This program is freely distributable without licensing fees and
 * is provided without guarantee or warrantee expressed or implied.
 * This program is -not- in the public domain.
 */

/*
 * Modified for IMOD by James Kremer. 
 * Prints to the imod information window.
 *
 */
/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

/* error_test.c -- test the error handlers and wprint() routine
 */
#include <Xm/Text.h>
#include <Xm/MainW.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <stdio.h>
/*#include <varargs.h>  */
#include <stdarg.h>

extern int Imod_debug;

#ifdef __cplusplus
extern "C" {
#endif
void wprint(char *fmt, ...);
#ifdef __cplusplus
}
#endif

Widget Wprint_text_output;

static int
x_error(Display *dpy, XErrorEvent  *err_event)
{
    char                buf[256];

    XGetErrorText (dpy, err_event->error_code, buf, (sizeof buf));

    wprint("X Error: <%s>\n", buf);
    return 0;
}

static void
xt_error(char *message)
{
     static int  error_in_here = False;
     
     if (error_in_here){
	  fprintf(stderr, "Xt Error: %s\n", message);
     }else{
	  error_in_here = True;
	  wprint ("Xt Error: %s\n", message);
     }
     error_in_here = False;
}


Widget wprintWidget(XtAppContext app, Widget parent)
{
     Arg          args[10];
     int          n;
     Widget text_output;

     /* Create output_text as a ScrolledText window */
     n = 0;
     XtSetArg(args[n], XmNrows,             3); n++; 
/*     XtSetArg(args[n], XmNcolumns,          80); n++; */
     XtSetArg(args[n], XmNeditable,         False); n++;
     XtSetArg(args[n], XmNeditMode,         XmMULTI_LINE_EDIT); n++;
     XtSetArg(args[n], XmNwordWrap,         True); n++;
     XtSetArg(args[n], XmNscrollHorizontal, False); n++;
     XtSetArg(args[n], XmNcursorPositionVisible, False); n++;
     text_output = XmCreateScrolledText(parent, "text_output", args, n);
     XtManageChild (text_output);

     if (!Imod_debug){
	 /* catch Xt errors */
	 XtAppSetErrorHandler (app, xt_error);
	 XtAppSetWarningHandler (app, xt_error);
	 
	 /* and Xlib errors */
	 XSetErrorHandler (x_error);
     }
     
     Wprint_text_output = text_output;
     return(text_output);
}

/*VARARGS*/
void wprint(char *fmt, ...)
{
     char msgbuf[256];
     static XmTextPosition wpr_position;
     va_list args;
     int nopos = False;
     int i, len;

     if (!(XtIsRealized(Wprint_text_output)))
	  return;

     va_start (args, fmt);
/*     fmt = va_arg (args, char *); */

     len = strlen(fmt);
     for(i = 0; i < len; i++)
	  if (fmt[i] == 0x07)
	       XBell(XtDisplay(Wprint_text_output), 100);


     if (fmt[strlen(fmt) - 1] == '\r'){
	  nopos = True;
     }

#ifndef NO_VPRINTF
     (void) vsprintf (msgbuf, fmt, args);
#else /* !NO_VPRINTF */
     {
	  FILE foo;
	  foo._cnt = 256;
	  foo._base = foo._ptr = msgbuf; /* (unsigned char *) ?? */
	  foo._flag = _IOWRT+_IOSTRG;
	  (void) _doprnt (fmt, args, &foo);
	  *foo._ptr = '\0'; /* plant terminating null character */
     }
#endif /* NO_VPRINTF */
     va_end (args);
     
     if (nopos){
	  XmTextSetString(Wprint_text_output, msgbuf);
	  wpr_position = 0;
     }
     else
	  XmTextInsert (Wprint_text_output, wpr_position, msgbuf);
     len = strlen(msgbuf);
     wpr_position = wpr_position + len;
     XtVaSetValues (Wprint_text_output, XmNcursorPosition, wpr_position, NULL);
     XmTextShowPosition (Wprint_text_output, wpr_position);

}


