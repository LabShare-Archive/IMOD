/*  IMOD VERSION 2.50
 *
 *  midas.c -- Main manual image alignment program.
 *              Renamed from midas because there is already a program 
 *              called midas
 * 
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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

/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.4  2002/11/05 23:54:24  mast
    Changed to get a visual then pass it to GLw.

    Revision 3.3  2002/11/05 23:29:13  mast
    Changed to call imodCopyright

    Revision 3.2  2002/08/19 04:46:10  mast
    Changed number of columns in edge number text box to 4

*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/MainW.h>
#include <Xm/RowColumn.h>
#include <Xm/ArrowB.h>
#include <Xm/ToggleB.h>
#include <Xm/Scale.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/Text.h>
#include <Xm/Frame.h>
#include <Xm/DrawingA.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <mrcc.h>
#include <diaP.h>
#include <imodel.h>
#include "midas.h"

struct Midas_view *VW;
/* DNM: fontlist was -17-, this failed on PC */
static String Fallback_resources[] = {
     "*fontList: -*-helvetica-bold-r-normal-*-14-*-*-*-*-iso8859-1",
     "*menubar*fontList: -*-helvetica-bold-o-normal-*-14-*-*-*-*-iso8859-1",
     "*frame*shadowType: SHADOW_IN",
     /*"*geometry: 640x640",*/
     "*background: grey",
     "*foreground: black",
     "*borderColor: grey",
     NULL,
};


static char defaultTranslations[] =
"<KeyDown>:   glwInput()\n\
 <BtnDown>:   glwInput()\n\
 <BtnMotion>: glwInput()\n\
 <Btn1Up>:    glwInput()\n\
 <EnterNotify>: glwInput()\n\
";

static String KeyTranslations = "<KeyDown>: midasKeyInput()\n";

static XtActionsRec actionsTable[] = {
     {"midasKeyInput",   (XtActionProc)midasKeyInput},
};

static XtTranslations keytrans;

static Widget createMenuBar(Widget window);
static Widget createControlPanel(Widget parent);

/* Attributes for the visuals that would be OK */
static int True24[] =
{
    GLX_DOUBLEBUFFER, 
    GLX_RGBA,
    GLX_RED_SIZE, 8,
    GLX_GREEN_SIZE, 8,
    GLX_BLUE_SIZE, 8,
    None
};
static int True12[] =
{
    GLX_DOUBLEBUFFER, 
    GLX_RGBA,
    GLX_RED_SIZE, 4,
    GLX_GREEN_SIZE, 4,
    GLX_BLUE_SIZE, 4,
    None
};

/* Order of visuals in priority: single then double buffer, then reduced 
   depth */
static int *OpenGLAttribList[] = {
     True24 + 1, True24, True12 + 1, True12,
     NULL
};


static void usage(void)
{
     char *pname = "midas";

     fprintf(stderr, "%s version %s\n", pname, MIDAS_VERSION_STRING);
     imodCopyright();
     fprintf(stderr, "Usage: %s [x opts] [options] <mrc filename> "
	     "[transform filename]\n", pname);
     fprintf(stderr, "options:\n");
     fprintf(stderr, "\t-gl             output global transforms (default"
	     " is local).\n");
     fprintf(stderr, "\t-r <filename>   load reference image file.\n");
     fprintf(stderr, "\t-rz <section>   section # for reference (default 0).\n");
     fprintf(stderr, "\t-p <filename>   load piece list file for fixing montages.\n");
     fprintf(stderr, "\t-C <size>       set cache size to given number of "
	     "sections\n"); 
     fprintf(stderr, "\t-s <min,max>    set intensity scaling; min to 0 and"
	     " max to 255\n");
     fprintf(stderr, "\t-b <size>       set initial size for block copies\n");
     fprintf(stderr, "\t-D              debug mode - do not run in background\n");
     exit(-1);
}

main (int argc, char **argv)
{
     XtAppContext context;
     XtTranslations transTable;
     Widget topLevel, window, menubar, control, frame, row;
     struct Midas_view MidasView, *vw;
     Screen *screen;
     FILE *file;
     Atom wmclose;
     int width, height;
     int command_width = 256;
     int command_height = 800;
     int i;
     int nxfopt = 0;
     XVisualInfo *visualInfo;

#ifdef NO_IMOD_FORK
     int dofork = 0;
#else
     int dofork = 1;
#endif

     vw = VW = &MidasView;
     /* open display */
     topLevel = XtVaAppInitialize
	  (&context, "midas", NULL, 0, &argc, argv, 
	   Fallback_resources, NULL);
     if (!topLevel){
	  fprintf(stderr, "%s: error opening display.\n", argv[0]);
	  exit(-1);
     }
     VW->display = XtDisplay(topLevel);

     if (argc < 2)
	  usage();

     new_view(VW);

     for (i = 1; i < argc ; i++){
	  if (argv[i][0] == '-'){
	       switch (argv[i][1]){
		  case 'r': /* reference image */
		    if (argv[i][2] == 'z'){
			 vw->xsec = atoi(argv[++i]);
		    }else{
			 vw->refname = argv[++i];
		    }
		    break;
		    
		  case 'p': /* piece list */
		    vw->plname = argv[++i];
		    break;

		  case 'g':
		    vw->xtype = XTYPE_XG;
		    break;

		  case 'C':
		    vw->cachein = atoi(argv[++i]);
		    break;

		  case 'b':
		    vw->boxsize = atoi(argv[++i]);
		    break;

		  case 's':
		    sscanf(argv[++i], "%f%*c%f", &(vw->sminin), &(vw->smaxin));
		    break;

		  case 'D':
		    dofork = 0;
		    break;

		  default:
		    usage();
		    break;
	       }
	  }else break;
     }
     
     if (i < argc - 2 || i == argc)
	  usage();
     

     /* If there are two args left, the last one is name of transform file */
     if (i == argc - 2) {
	  /* It gets freed by a save-as, so need to copy to malloc'd space */
	  vw->xname = (char *)malloc(strlen(argv[argc - 1]) + 2);
	  strcpy(vw->xname, argv[argc - 1]);
	  file = fopen(vw->xname, "r");
	  if (file) {
	       /* If file opened, close and mark that need to read it */
	       fclose(file);
	       vw->didsave = -1;
	  } else {
	       /* Otherwise give warning that new file will be used */
	       fprintf(stderr, "Transform file (%s) not found;\n"
		       " transforms will be saved in a "
		       "new file by that name.\n", vw->xname);
	  }
     }
	  
     if (vw->plname) {
	  if (vw->refname) {	
	       fprintf(stderr, "You cannot use both -p and -r options.\n");
	       exit(1);
	  }

	  if (vw->didsave != -1) {
	       fprintf(stderr, "The last entry on the line must be the name of"
		       " an existing edge\n correlation displacement file.\n");
	       exit(1);
	  }

	  if (vw->xtype == XTYPE_XG)
	       fprintf(stderr, "-gl option has no effect when fixing "
		       "montage overlaps.\n");
	  vw->xtype = XTYPE_MONT;
     }	       

     if (vw->refname) {
	  if (vw->xtype == XTYPE_XG)
	       fprintf(stderr, "-gl option has no effect with alignment to "
		       "reference section.\n");
	  vw->xtype = XTYPE_XREF;
     }

     if (load_view(VW, argv[i])){
	  fprintf(stderr, "%s: error opening %s.\n", argv[0], argv[i]);
	  exit(-1);
     }

     if (dofork) {
	  if (fork())
	       exit(0);
     }

     XtAppAddActions(context, actionsTable, XtNumber(actionsTable));
     transTable = XtParseTranslationTable(defaultTranslations);
     keytrans = XtParseTranslationTable(KeyTranslations);

     dia_xinit(topLevel, context, "midas");

     XtVaSetValues(topLevel, XmNdeleteResponse, XmDO_NOTHING, NULL);

     screen =  DefaultScreenOfDisplay(VW->display);
     width = WidthOfScreen(screen) - 40;
     height = HeightOfScreen(screen) - 90;
     if (width > vw->xsize + command_width)
       width = vw->xsize + command_width;
     if (command_height < vw->ysize)
       command_height = vw->ysize;
     if (height > command_height)
       height = command_height;
     /* printf ("%d %d %d %d %d %d\n", vw->xsize, vw->ysize, width, height,
	command_width, command_height); */

     /* Find a visual with desirable attributes */
     for (i = 0; OpenGLAttribList[i] != NULL; i++) {
       visualInfo = NULL;
       visualInfo = glXChooseVisual(VW->display, DefaultScreen(VW->display),
					     OpenGLAttribList[i]);

       /* Insist that the depth be at least 16 */
       if (visualInfo && visualInfo->depth >=16)
	 break;
     }

     if (!visualInfo) {
	  fprintf(stderr, "%s: couldn't get a suitable rendering visual.\n",
		  argv[0]);
	  exit(-1);
     }

     window =  XtVaCreateManagedWidget
	  ("midas", xmMainWindowWidgetClass, topLevel,
	   XmNdeleteResponse, XmDO_NOTHING,
	   NULL);

     menubar = createMenuBar(window);


     row = XtVaCreateManagedWidget ("row", xmFormWidgetClass, window, 
				    XmNwidth, width,
				    XmNheight, height,
				    NULL);
     /* In order to ask for a size for the glw window (does no good) */
     width -= command_width;

     control = createControlPanel(row);


     /*     frame = XtVaCreateManagedWidget
	  ("frame", xmFrameWidgetClass, row, 
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNrightAttachment, XmATTACH_FORM,
	   XmNleftAttachment, XmATTACH_WIDGET,
	   XmNleftWidget, control,
	   XmNshadowType, XmSHADOW_IN,
	   NULL); */

     /* Have it use the supplied visual because GLw is inflexible about
	choosing EITHER single or double buffer if only one is available */
     vw->glw = XtVaCreateManagedWidget
	  ("rgbwidget",  glwMDrawingAreaWidgetClass, row,
	   /*  GLwNrgba, True,
	   GLwNdoublebuffer, False,
	   GLwNgreenSize, 1,
	   GLwNredSize, 1,
	   GLwNblueSize, 1, */
	   GLwNvisualInfo, visualInfo,
	   XmNnavigationType, XmNONE,
	   XmNtraversalOn, True,
	   XmNtranslations, transTable,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNrightAttachment, XmATTACH_FORM,
	   XmNleftAttachment, XmATTACH_WIDGET,
	   XmNleftWidget, control,
	   XmNwidth, width,
	   XmNheight, height,
	   NULL);
     XtAddCallback(vw->glw, GLwNexposeCallback, expose_cb, (XtPointer)vw);
     XtAddCallback(vw->glw, GLwNresizeCallback, expose_cb, (XtPointer)vw);
     XtAddCallback(vw->glw, GLwNginitCallback, expose_cb, (XtPointer)vw);
     XtAddCallback(vw->glw, GLwNinputCallback, input_cb, (XtPointer)vw);


     XtOverrideTranslations(vw->glw, transTable);

#ifdef MWSETWORK
     XtVaSetValues(main_win,XmNworkWindow,frame,NULL);
#endif

     XmMainWindowSetAreas( window, menubar, NULL, NULL, NULL, row); 

     /* Link Window Close to Quit */
     wmclose = XmInternAtom( XtDisplay(topLevel), 
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(topLevel, wmclose, 
			     (XtCallbackProc)midas_quit_cb, NULL);

     XtRealizeWidget(topLevel);

     update_parameters();

     XtAppMainLoop(context);

     return 0;
}

static Widget createMenuBar(Widget window)
{
     Widget menubar, menu, widget;
     XmString s1, s2, s3, s4, s5, s6, s7, s8, help;

     /* Create menu bar. */
     s1 = XmStringCreateSimple("File");
     s2 = XmStringCreateSimple("Edit");
     help  = XmStringCreateSimple("Help");
     menubar = XmVaCreateSimpleMenuBar
	  (window, "menubar",
	   XmVaCASCADEBUTTON, s1, 'F',
	   XmVaCASCADEBUTTON, s2, 'E',
	   XmVaCASCADEBUTTON, help, 'H',
	   NULL);
     XmStringFree(s1);
     XmStringFree(s2);

     /* Set Help */
     if (widget = XtNameToWidget(menubar, "button_2"))
	  XtVaSetValues(menubar, XmNmenuHelpWidget, widget, NULL);

     /* Create File Menu */
     s1 = XmStringCreateSimple("Load  Transforms");
     s2 = XmStringCreateSimple("Save Transforms");
     s3 = XmStringCreateSimple("Save Transforms As...");
     s4 = XmStringCreateSimple("Save Contrast-scaled Image...");
     s5 = XmStringCreateSimple("About");
     s6 = XmStringCreateSimple("Quit");
     s7 = XmStringCreateSimple("s");
     menu = XmVaCreateSimplePulldownMenu
	  (menubar, "file_menu", 0, filemenu_cb,
	   XmVaPUSHBUTTON, s1, 'L', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'S', "<Key>s", s7,
	   XmVaPUSHBUTTON, s3, 'V', NULL, NULL,
	   XmVaPUSHBUTTON, s4, 'C', NULL, NULL,
	   XmVaPUSHBUTTON, s5, 'A', NULL, NULL,
#ifndef MENUBUTTONBUG
	   XmVaSEPARATOR,
#endif
	   XmVaPUSHBUTTON, s6, 'Q', NULL, NULL,
	   NULL);

     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(s4);
     XmStringFree(s5);
     XmStringFree(s6);
     XmStringFree(s7);

     s1 = XmStringCreateSimple("Store Section Transform");
     s2 = XmStringCreateSimple("Reset to Unit Transform");
     s3 = XmStringCreateSimple("Revert to Stored Transform");
     s4 = XmStringCreateSimple("Transform Model...");
     menu = XmVaCreateSimplePulldownMenu
	  (menubar, "edit_menu", 1, editmenu_cb,
	   XmVaPUSHBUTTON, s1, 'S', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'R', NULL, NULL,
	   XmVaPUSHBUTTON, s3, 'v', NULL, NULL,
	   XmVaPUSHBUTTON, s4, 'T', NULL, NULL,
	   NULL);
     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(s4);

     s1 = XmStringCreateSimple("Controls");
     s2 = XmStringCreateSimple("Hotkeys");
     s3 = XmStringCreateSimple("Mouse");
     menu = XmVaCreateSimplePulldownMenu
	  (menubar, "help_menu", 2, helpmenu_cb,
	   XmVaPUSHBUTTON, s1, 'C', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'H', NULL, NULL,
	   XmVaPUSHBUTTON, s3, 'M', NULL, NULL,
	   NULL);
     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(help);
     XtManageChild(menubar);
     return(menubar);
}

static Widget makeTwoArrows(Widget parent, int direction, int value, int pos,
			  void (*callbackFunc)(Widget,XtPointer,XtPointer))
{
     Widget arrow;
     Arg args[10]; 
     int n = 0;
     
     /* select type of arrow */
     if (direction < 0) {
	  XtSetArg(args[n], XmNarrowDirection, XmARROW_LEFT); n++;
     } else {
	  XtSetArg(args[n], XmNarrowDirection, XmARROW_UP); n++;
     }

     /* if inside the big form, set up all the needed attachments */
     if (pos) {
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_FORM); n++;
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_POSITION); n++;
	  XtSetArg(args[n], XmNtopPosition, pos - 1); n++;
	  XtSetArg(args[n], XmNborderWidth, 2); n++;
     }

     arrow = XtCreateManagedWidget
	  ("arrow", xmArrowButtonWidgetClass, parent, args, n);


     XtAddCallback(arrow, XmNarmCallback, (XtCallbackProc)callbackFunc, 
		   (XtPointer)(direction * value));
     XtOverrideTranslations(arrow, keytrans);

     n = 0;
     if (direction < 0) {
	  XtSetArg(args[n], XmNarrowDirection, XmARROW_RIGHT); n++;
     } else {
	  XtSetArg(args[n], XmNarrowDirection, XmARROW_DOWN); n++;
     }

     if (pos) {
	  XtSetArg(args[n], XmNleftAttachment, XmATTACH_WIDGET); n++;
	  XtSetArg(args[n], XmNleftWidget, arrow); n++;
	  XtSetArg(args[n], XmNtopAttachment, XmATTACH_POSITION); n++;
	  XtSetArg(args[n], XmNtopPosition, pos - 1); n++;
	  XtSetArg(args[n], XmNborderWidth, 2); n++;
     }

     arrow = XtCreateManagedWidget
	  ("arrow", xmArrowButtonWidgetClass, parent, args, n);

     XtAddCallback(arrow, XmNarmCallback, (XtCallbackProc)callbackFunc, 
		   (XtPointer)(-direction * value));
     XtOverrideTranslations(arrow, keytrans);
     return(arrow);
}

/* NOTE it does no good to put the row inside its own form attached to the
   position; either with attachments by widget or attachments to horizontal
   positions.  The second label is still unstable in montage mode. Adding
   a vertical separator between control panel and glw didn't help either */
static Widget makeArrowRow(Widget parent, int direction, int cbvalue, int pos,
			   void (*callbackFunc)(Widget,XtPointer,XtPointer),
			   char *textlabel, int decimals, int digits,
			   float value)
{
     char string[32];
     Widget row, label, arrow;
     row = parent;
     arrow = makeTwoArrows(row, direction, cbvalue, pos, callbackFunc);
     arrow = XtVaCreateManagedWidget (textlabel, xmLabelWidgetClass, row, 
				      XmNleftAttachment, XmATTACH_WIDGET,
				      XmNleftWidget, arrow,
				      XmNtopAttachment, XmATTACH_POSITION,
				      XmNtopPosition, pos - 1,
				      XmNtopOffset, 4,
				      NULL);

     sprintf_decimals(string, decimals, digits, value);
     label = XtVaCreateManagedWidget (string, xmLabelWidgetClass, row,
				      XmNalignment, XmALIGNMENT_END,
				      XmNrightAttachment, XmATTACH_FORM,
				      XmNleftAttachment, XmATTACH_WIDGET,
				      XmNleftWidget, arrow,
				      XmNtopAttachment, XmATTACH_POSITION,
				      XmNtopPosition, pos - 1,
				      XmNtopOffset, 4,
				      NULL);
     /*    XtManageChild(row); */
     return (label);
}

static Widget createParameterDisplay(Widget parent)
{
     Widget scale, row, col, form;
     int i;

     /* It has to be one big form to get the label widgets to stick to the
	right side.  But then the only way to get a reasonable vertical layout
	was with positions on the fraction base */

     col = XtVaCreateManagedWidget
	 ("form", xmFormWidgetClass, parent, 
	  XmNfractionBase, VW->xtype == XTYPE_MONT ? 3 : 8,
	  /* XmNtopAttachment, XmATTACH_FORM, */
	  /* XmNbottomAttachment, XmATTACH_FORM, */
	  XmNleftAttachment, XmATTACH_FORM,
	  NULL);
     
     VW->wParameter[3] = makeArrowRow
	  (col, -1, 4, 1, parameter_cb, "X translation",
	   getParamDecimals(3), getParamDigits(3), -1000.0);
     VW->wParameter[4] = makeArrowRow
	  (col, 1, 5, 2, parameter_cb, "Y translation",
	   getParamDecimals(4), getParamDigits(4), -1000.0);
     VW->wIncrement[2] = makeArrowRow
	  (col, 1, 3, 3, increment_cb, "   increment ",
	   getIncDecimals(2),  getIncDigits(2), VW->increment[2]);
     if (VW->xtype != XTYPE_MONT) {
	  XtVaCreateManagedWidget("separator", xmSeparatorWidgetClass, col, 
				  XmNseparatorType, XmSINGLE_LINE,
				  XmNtopAttachment, XmATTACH_POSITION,
				  XmNtopPosition, 3,
				  XmNleftAttachment, XmATTACH_FORM,
				  XmNrightAttachment, XmATTACH_FORM,
				  XmNmargin, 30, NULL); 
	  VW->wParameter[0] = makeArrowRow
	       (col, -1, 1, 4, parameter_cb, "Rotation    ",
		getParamDecimals(0), getParamDigits(0), -179.);
	  VW->wIncrement[0] = makeArrowRow
	       (col, 1, 1, 5, increment_cb, "   increment",
		getIncDecimals(0), getIncDigits(0), VW->increment[0]);
	  XtVaCreateManagedWidget("separator", xmSeparatorWidgetClass, col, 
				  XmNseparatorType, XmSINGLE_LINE,
				  XmNtopAttachment, XmATTACH_POSITION,
				  XmNtopPosition, 5,
				  XmNleftAttachment, XmATTACH_FORM,
				  XmNrightAttachment, XmATTACH_FORM,
				  XmNmargin, 30, NULL);
	  VW->wParameter[1] = makeArrowRow
	       (col, 1, 2, 6, parameter_cb, "Magnification",
		getParamDecimals(1),  getParamDigits(1), 1.0);
	  VW->wParameter[2] = makeArrowRow
	       (col, 1, 3, 7, parameter_cb, "Stretch      ",
		getParamDecimals(2),  getParamDigits(2), 1.0);
	  VW->wIncrement[1] = makeArrowRow
	       (col, 1, 2, 8, increment_cb, "   factor    ", 
		getIncDecimals(1),  getIncDigits(1),VW->increment[1]);

     }     
     XtManageChild(col);

     if (VW->xtype != XTYPE_MONT) {
	  VW->anglescale = XtVaCreateManagedWidget
	       ("scale", xmScaleWidgetClass, parent,
		XmNorientation, XmHORIZONTAL,
		XmNprocessingDirection, XmMAX_ON_LEFT,
		XmNmaximum, 900,
		XmNminimum, -900,
		XmNvalue, VW->sangle,
		XmNdecimalPoints, 1,
		XmNshowValue, True,
		XmNscaleMultiple, 10,
		XtVaTypedArg, XmNtitleString, XmRString, "Stretch Angle", 14, 
		NULL);
	  XtAddCallback(VW->anglescale, XmNvalueChangedCallback, 
			msdia_angle_cb, VW);
	  /* XtOverrideTranslations(scale, keytrans);  Does no good! */
     } else {
	  XtVaCreateManagedWidget("separator", xmSeparatorWidgetClass, parent, 
				  XmNseparatorType, XmDOUBLE_LINE,
				  NULL);

	  /* A rowcol is needed to keep the labels left-justified */
	  col = XtVaCreateManagedWidget ("row", xmRowColumnWidgetClass, parent,
					 XmNorientation, XmVERTICAL,
					 NULL);
	  VW->wMeanerr = XtVaCreateManagedWidget
	       ("Mean error: 100.00",
		xmLabelWidgetClass, col,
		NULL);
	  XtVaCreateManagedWidget("Top 4 errors:",
				  xmLabelWidgetClass, col,
				  XmNleftAttachment, XmATTACH_FORM,
				  NULL);
	  row = XtVaCreateManagedWidget ("row", xmRowColumnWidgetClass, col,
					 XmNorientation, XmHORIZONTAL,
					 XmNnumColumns, 2,
					 XmNpacking, XmPACK_COLUMN,
					 NULL);
	  for (i = 0; i < 4; i++) {
	       /*  VW->wToperr[i] = XtVaCreateManagedWidget("X 199: 50.00  ",
						       xmLabelWidgetClass, row,
						       NULL); */
	       VW->wToperr[i] = XtVaCreateManagedWidget
		    ("X 199: 50.00  ", xmPushButtonWidgetClass, row, NULL);
	       XtAddCallback(VW->wToperr[i], XmNarmCallback, top_error_cb,
			     (XtPointer)i);
	       XtOverrideTranslations(VW->wToperr[i], keytrans);
	  }
	  XtManageChild(row);

	  VW->wCurerr = XtVaCreateManagedWidget
	       ("This edge: -50.00, -50.00",
		xmLabelWidgetClass, col,
		NULL);
	  VW->wLeaverr = XtVaCreateManagedWidget
	       ("Leave-out: -50.00, -50.00",
		xmLabelWidgetClass, col,
		NULL);
	  XtManageChild(col);

	  row = XtVaCreateManagedWidget
	       ("Apply Leave-out Error", xmPushButtonWidgetClass, 
		parent, NULL);
	  XtAddCallback(row, XmNarmCallback, leave_out_cb, (XtPointer)VW);
	  XtOverrideTranslations(row, keytrans);

     }
     return (col);
}

static Widget createSectionControls(Widget parent)
{
     Widget row, lab, minirow;
     Widget col = parent;
     Arg args[3];
     int n = 0;

     row = XtVaCreateManagedWidget ("row", xmRowColumnWidgetClass, col, 
				    XmNorientation, XmHORIZONTAL, 
				    NULL);
     if (VW->xtype != XTYPE_MONT) {
	  XtVaCreateManagedWidget ("Reference Sec.", xmLabelWidgetClass, row,
				   NULL);
	  VW->reftext = XtVaCreateManagedWidget ("reftext", xmTextWidgetClass,
						 row, XmNcolumns, 6, 
						 NULL);
	  XtAddCallback(VW->reftext, XmNactivateCallback, reftext_cb, NULL);
     }

     row = XtVaCreateManagedWidget ("row", xmRowColumnWidgetClass, col, 
				    XmNorientation, XmHORIZONTAL, NULL);
     XtVaCreateManagedWidget ("Current Sec.", xmLabelWidgetClass, row, NULL);
     VW->curtext = XtVaCreateManagedWidget ("curtext", xmTextWidgetClass, row, 
					    XmNcolumns, 6, NULL);
     XtAddCallback(VW->curtext, XmNactivateCallback, curtext_cb, NULL);

     row = XtVaCreateManagedWidget ("arrowrow", xmRowColumnWidgetClass, col, 
				    XmNorientation, XmHORIZONTAL, NULL);
     makeTwoArrows(row, 1, 1, 0, section_cb);
     XtVaCreateManagedWidget ("Section", xmLabelWidgetClass, row, NULL);
     XtManageChild(row);

     if (VW->xtype != XTYPE_XREF && VW->xtype != XTYPE_MONT) {
	  VW->difftoggle  = XtVaCreateManagedWidget
	       ("Keep Curr - Ref diff = 1",  xmToggleButtonWidgetClass, col,
		NULL);
	  XtAddCallback(VW->difftoggle, XmNvalueChangedCallback,
			keepdiff_cb, NULL);
	  XmToggleButtonSetState(VW->difftoggle, True, False);
	  XtOverrideTranslations(VW->difftoggle, keytrans);
	  if (VW->xtype == XTYPE_XF)
	       XtVaCreateManagedWidget ("Local Alignment Mode", 
					xmLabelWidgetClass, col, NULL);
	  else
	       XtVaCreateManagedWidget ("Global Alignment Mode",
					xmLabelWidgetClass, col, NULL);
     } else if (VW->xtype == XTYPE_MONT) { 
	  row = XtVaCreateManagedWidget ("row", xmRowColumnWidgetClass, col, 
				    XmNorientation, XmHORIZONTAL, NULL);
	  XtSetArg(args[n], XmNorientation, XmHORIZONTAL); n++;
	  minirow = XmCreateRadioBox(row, "xory_edge", args, n);

	  VW->wXedge = XtVaCreateManagedWidget
	       ("X", xmToggleButtonWidgetClass, minirow, NULL);
	  XtOverrideTranslations(VW->wXedge, keytrans);
	  XtAddCallback(VW->wXedge, XmNdisarmCallback, 
			xory_cb, (XtPointer)0);
	  
	  VW->wYedge = XtVaCreateManagedWidget
	       ("Y", xmToggleButtonWidgetClass, minirow, NULL);
	  XtOverrideTranslations(VW->wYedge, keytrans);
	  XtAddCallback(VW->wYedge, XmNdisarmCallback, 
			xory_cb, (XtPointer)1);

	  XtManageChild(minirow);

	  XtVaCreateManagedWidget ("Edge", xmLabelWidgetClass, row, NULL);
	  VW->edgetext = XtVaCreateManagedWidget ("curedge", xmTextWidgetClass,
						 row, 
						 XmNcolumns, 4, NULL);
	  XtAddCallback(VW->edgetext, XmNactivateCallback, edgetext_cb, NULL);
	  XtManageChild(row);

	  row = XtVaCreateManagedWidget ("arrowrow", xmRowColumnWidgetClass, 
					 col, 
					 XmNorientation, XmHORIZONTAL, NULL);
	  makeTwoArrows(row, 1, 1, 0, edge_cb);
	  XtVaCreateManagedWidget ("Edge Number", xmLabelWidgetClass, row,
				   NULL);
	  XtManageChild(row);
	  manage_xory(VW);

     } else
	  XtVaCreateManagedWidget ("Reference Alignment Mode",
				   xmLabelWidgetClass, col, NULL);

     return (col);
}

static Widget createZoomBlock(Widget parent)
{
     Widget row;
     char string[32];
     Widget col = parent;
     /*  col = XtVaCreateManagedWidget
	 ("rowcol", xmRowColumnWidgetClass, parent, NULL); */
     row = XtVaCreateManagedWidget ("arrowrow", xmRowColumnWidgetClass, col, 
				    XmNorientation, XmHORIZONTAL, NULL);
     makeTwoArrows(row, 1, 1, 0, zoom_cb);
     VW->zoomlabel = XtVaCreateManagedWidget ("Zoom  1.00",
					      xmLabelWidgetClass, row, NULL);

     sprintf(string, "Block size %2d", VW->boxsize);
     row = XtVaCreateManagedWidget ("arrowrow", xmRowColumnWidgetClass, col, 
				    XmNorientation, XmHORIZONTAL, NULL);
     makeTwoArrows(row, 1, 1, 0, block_cb);
     VW->blocklabel = XtVaCreateManagedWidget (string,
					      xmLabelWidgetClass, row, NULL);

     row  = XtVaCreateManagedWidget
	  ("Interpolate",  xmToggleButtonWidgetClass, col, NULL);
     XtAddCallback(row, XmNvalueChangedCallback, interpolate_cb, NULL);
     XmToggleButtonSetState(row, False, False);
     XtOverrideTranslations(row, keytrans);
     return (col);
}

static Widget createViewToggle(Widget parent)
{
     Widget row;
     Widget col = parent;
     /*  col = XtVaCreateManagedWidget
	 ("rowcol", xmRowColumnWidgetClass, parent, NULL); */

     row  = XtVaCreateManagedWidget
	  ("Overlay view",  xmToggleButtonWidgetClass, col, NULL);
     XtAddCallback(row, XmNvalueChangedCallback, overlay_cb, NULL);
     if (VW->vmode != MIDAS_VIEW_COLOR)
	  XmToggleButtonSetState(row, False, False);
     else
	  XmToggleButtonSetState(row, True, False);
     XtOverrideTranslations(row, keytrans);
     VW->overlaytoggle = row;

     row = XtVaCreateManagedWidget
	       ("Toggle Ref/Cur", xmPushButtonWidgetClass, col, NULL);
     XtAddCallback(row, XmNarmCallback, align_arm_cb, (XtPointer)VW);
     XtAddCallback(row, XmNdisarmCallback, align_disarm_cb, (XtPointer)VW);
     XtOverrideTranslations(row, keytrans);

     return (col);
}

static Widget createContrastControls(Widget parent)
{
     Widget row;
     Widget col = parent;
     /*  col = XtVaCreateManagedWidget
	 ("rowcol", xmRowColumnWidgetClass, parent, NULL); */

     row = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, col, 
	   XmNorientation, XmHORIZONTAL, NULL);
     XtVaCreateManagedWidget
	  ("Black", xmLabelWidgetClass, row, NULL);
     VW->wBlacklevel = XtVaCreateManagedWidget
	  ("Blacklevel", xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 255,
	   XmNvalue, 0,
	   XmNscaleMultiple, 1,
	   NULL);
     VW->wBlackval = XtVaCreateManagedWidget
	  ("000",  xmLabelWidgetClass, row, NULL);
     XtManageChild(row);
     XtOverrideTranslations(VW->wBlacklevel, keytrans);

     XtAddCallback(VW->wBlacklevel, XmNvalueChangedCallback,
		   (XtCallbackProc)blacklevel_cb, NULL);
     XtAddCallback(VW->wBlacklevel, XmNdragCallback,
		   (XtCallbackProc)blacklevel_cb, (XtPointer)1);
     
     row = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, col,
	   XmNorientation, XmHORIZONTAL, NULL);
     XtVaCreateManagedWidget
	  ("White", xmLabelWidgetClass, row, NULL);
     VW->wWhitelevel = XtVaCreateManagedWidget
	  ("Whitelevel", xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 255,
	   XmNvalue, 255,
	   XmNscaleMultiple, 1, 
	   NULL);
     VW->wWhiteval = XtVaCreateManagedWidget
	  ("255", xmLabelWidgetClass, row, NULL);
     XtManageChild(row);
     XtOverrideTranslations(VW->wWhitelevel, keytrans);

     XtAddCallback(VW->wWhitelevel, XmNvalueChangedCallback,
		   (XtCallbackProc)whitelevel_cb, NULL);
     XtAddCallback(VW->wWhitelevel, XmNdragCallback,
		   (XtCallbackProc)whitelevel_cb, (XtPointer)1);

     row  = XtVaCreateManagedWidget
	  ("Apply to only one sec.",  xmToggleButtonWidgetClass, col, NULL);
     XtAddCallback(row, XmNvalueChangedCallback, applyone_cb, NULL);
     XmToggleButtonSetState(row, False, False);
     XtOverrideTranslations(row, keytrans);

     row  = XtVaCreateManagedWidget
	  ("Reverse contrast",  xmToggleButtonWidgetClass, col, NULL);
     XtAddCallback(row, XmNvalueChangedCallback, reverse_cb, NULL);
     XmToggleButtonSetState(row, False, False);
     XtOverrideTranslations(row, keytrans);
     VW->reversetoggle = row;

     return (col);
}


static Widget createControlPanel(Widget parent)
{
     Widget col;

     col = XtVaCreateWidget
	  ("control",  xmRowColumnWidgetClass, parent,
	   XmNorientation, XmVERTICAL,
	   XmNleftAttachment, XmATTACH_FORM,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   NULL);


     createSectionControls(col);
     XtVaCreateManagedWidget("separator", xmSeparatorWidgetClass, col, 
			     XmNseparatorType, XmDOUBLE_LINE,
			     NULL);
     createContrastControls(col);
     XtVaCreateManagedWidget("separator", xmSeparatorWidgetClass, col, 
			     XmNseparatorType, XmDOUBLE_LINE,
			     NULL);
     createZoomBlock(col);
     XtVaCreateManagedWidget("separator", xmSeparatorWidgetClass, col, 
			     XmNseparatorType, XmSINGLE_LINE,
			     XmNmargin, 30, NULL);
     createViewToggle(col);
     XtVaCreateManagedWidget("separator", xmSeparatorWidgetClass, col, 
			     XmNseparatorType, XmDOUBLE_LINE,
			     NULL);
     createParameterDisplay(col);

     XtManageChild(col);

     return(col);
}


void midas_error(char *tmsg, char *bmsg, int retval)
{
     if (Dia_toplevel == NULL)
	  fprintf(stderr, "%s %s\n", tmsg, bmsg);
     else 
	  dia_vasmsg(tmsg, bmsg, NULL);

     if (retval)
	  exit(retval);

     return;
}

