/*****************************************************************************
 *                                                                           *
 *   FILE: test2.c                                                           *
 *                                                                           *
 *   PURPOSE: Test dia library.                                              *
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

#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>

#include "dia.h"
Window AppWin;

static String Fallback_resources[] = {
     "*fontList: -*-helvetica-bold-r-normal-*-17-*-*-*-*-iso8859-1",
     "*frame*shadowType: SHADOW_IN",
     "*background: blue",
     "*foreground: white",
     NULL,
};

static short colors[] = { 0, 255, 0};

void test_fdact_cb(Widget w, XtPointer client, XtPointer call)
{
     printf("filename = [%s]\n", call);
}

void test_fact_cb(Widget w, XtPointer client, XtPointer call)
{

     DiaFactCallbackStruct *cbs = (DiaFactCallbackStruct *)call;

     if (cbs->outfilename)
	  printf("file 1 is %s\nfile 2 is %s\n",
		 cbs->infilename, cbs->outfilename);
     else
	  printf("file is %s\n", cbs->infilename);
}


static void ok_cb(Widget w, XtPointer client, XtPointer call)
{
     printf("ok : client = %d\n", client);
     diaDestroyDialog((diaDialog *)call);
}

static void cancel_cb(Widget w, XtPointer client, XtPointer call)
{
     printf("cancel : client = %d\n", client);
     diaDestroyDialog((diaDialog *)call);
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     XtVaCreateManagedWidget
	  ("Custom Dialog", xmLabelWidgetClass, 
	   diaGetWorkArea(dia), 
	   NULL);
     return;
}
static void color_cb(Widget w, XtPointer client, XtPointer call)
{
     DiaColorCallbackStruct *cbs = (DiaColorCallbackStruct *)call;

     printf("color %d : %d, %d, %d\n", 
	    cbs->reason, cbs->red, cbs->green, cbs->blue);
     colors[0] = cbs->red;
     colors[1] = cbs->green;
     colors[2] = cbs->blue;
     return;
}

void test2_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia;
     Widget label;
     int butno = (int) client;
     int cpid;
     int i;
     char tmp[128];
     char *filename;
     char *s;

     switch(butno){

	case 1:

	  if (dia_ask("Do you like this program?"))
	       dia_puts("I'm glad you like this program.");
	  else
	       dia_err("Something is wrong, User Error.");
	  break;

	case 2:
	  dia_vasmsg("This is a scrolled message.\n\n",
		     "It has array and varargs versions.",
		     "You can use it for large messages",
		     "or for help",
		     NULL);
	  break;

	case 3:
	  diaBusyCursor(True);
	  printf("10");
	  for(i = 0; i < 10; i++){
	       printf("s");
	       fflush(stdout);
	       sleep(1);
	  }
	  diaBusyCursor(False);

	  if ((cpid = fork()) == 0) {
	       for(i = 0; i < 60; i++){
		    printf(".");
		    fflush(stdout);
		    sleep(1);
	       }
	       exit(0);
	  }else{
	       dia_abort("Printing 60 dots.", cpid);
	  }
	  break;
	  
	case 4: 
	  diaEasyFileAct("Enter a FILE", test_fdact_cb, NULL);

	  dia_fload("Testing: enter two file names.", "junk",
		   test_fact_cb, NULL);
	  break;
	  filename = dia_filename("Enter a file, just for grins.");
	  if (filename){
	       dia_vasmsg("You entered\n", filename, NULL);
	       free(filename);
	  }
	  break;

	case 5:
	  i = dia_int(0, 1000, 500, 1, "Enter a number");
	  sprintf(tmp, "You entered %g\n", (double)i/ 10.0);
	  dia_puts(tmp);
	  break;

	case 6:
#ifdef __sgi_colorreq__noway
	  dia_sgcolor(colors[0],colors[1],colors[2], 
		      "Color for test", color_cb, colors);
#else
	  dia_cbcolor(&(colors[0]),&(colors[1]),&(colors[2]),
		      "Color for test", color_cb, colors);
#endif

/*	  dia_color(&red, &green, &blue);   

	  sprintf(tmp, "Color selected (r, g, b) = ( %d, %d, %d)",
		  red, green, blue);
	  dia_puts(tmp);
*/
	  break;

	case 7:
	  s = dia_gets("Edit me", "String Editor");
	  if (s)
	       printf("String is %s\n", s);
	  else
	       printf("String is NULL\n");
	  break;

	case 8:
	  dia = diaVaCreateDialog
	       ("test", dia_get_toplevel(), dia_get_context(),
		DiaNcontrolButton, "OK", ok_cb, (XtPointer)7,
		DiaNcontrolButton, "Nothing", NULL, NULL,
		DiaNcontrolButton, "Cancel", cancel_cb, (XtPointer)11,
/*		DiaNwidgetClass, topLevelShellWidgetClass,  */
		DiaNworkAreaFunc, workarea_cb, (XtPointer)NULL,
		0);
	  break;

	case -1:
	  dia_busy(1);
	  if (1 == dia_choice("Do You really want to quit?",
			    "Yes", "No", "Cancel"))
	       exit(0);
	  dia_busy(0);
	  break;


	default:
	  return;

     }
}


main(int argc, char **argv)
{
     XtAppContext context;
     Widget topLevel, window, rowColumn, button;
     
	
     topLevel = XtVaAppInitialize
	  (&context, "test2", NULL, 0, &argc, argv, 
	   Fallback_resources, NULL);

     dia_xinit(topLevel, context, "test2");

     window =  XtVaCreateManagedWidget
	  ("test_3", xmMainWindowWidgetClass, topLevel,
/*	   XmNscrollBarDisplayPolicy, XmAS_NEEDED,  */
/*	   XmNscrollingPolicy, XmAUTOMATIC,  */
	   NULL);
     rowColumn = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, window, NULL);

     
     button = XtVaCreateManagedWidget
	  ("Test ask", xmPushButtonWidgetClass, rowColumn, NULL);
     XtAddCallback(button, XmNactivateCallback, test2_cb, (XtPointer)1);


     button = XtVaCreateManagedWidget
	  ("Test message", xmPushButtonWidgetClass, rowColumn, NULL);
     XtAddCallback(button, XmNactivateCallback, test2_cb, (XtPointer)2);


     button = XtVaCreateManagedWidget
	  ("Test abort", xmPushButtonWidgetClass, rowColumn, NULL);
     XtAddCallback(button, XmNactivateCallback, test2_cb, (XtPointer)3);
     

     button = XtVaCreateManagedWidget
	  ("Filename", xmPushButtonWidgetClass, rowColumn, NULL);
     XtAddCallback(button, XmNactivateCallback, test2_cb, (XtPointer)4);


     button = XtVaCreateManagedWidget
	  ("Get int", xmPushButtonWidgetClass, rowColumn, NULL);
     XtAddCallback(button, XmNactivateCallback, test2_cb, (XtPointer)5);

     button = XtVaCreateManagedWidget
	  ("Color", xmPushButtonWidgetClass, rowColumn, NULL);
     XtAddCallback(button, XmNactivateCallback, test2_cb, (XtPointer)6);

     button = XtVaCreateManagedWidget
	  ("Gets", xmPushButtonWidgetClass, rowColumn, NULL);
     XtAddCallback(button, XmNactivateCallback, test2_cb, (XtPointer)7);

     
     button = XtVaCreateManagedWidget
	  ("Dialog", xmPushButtonWidgetClass, rowColumn, NULL);
     XtAddCallback(button, XmNactivateCallback, test2_cb, (XtPointer)8);


     button = XtVaCreateManagedWidget
	  ("Quit", xmPushButtonWidgetClass, rowColumn, NULL);
     XtAddCallback(button, XmNactivateCallback, test2_cb, (XtPointer)-1);


     XtRealizeWidget(topLevel);
     XtAppMainLoop(context);
}     










