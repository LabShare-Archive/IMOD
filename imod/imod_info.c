/*  IMOD VERSION 2.50
 *
 *  imod_info.c -- open the imod information window.
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
*/
#include <stdio.h>
#include <Xm/Xm.h>
#include <Xm/ArrowBG.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/MainW.h>
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/ToggleBG.h>
#include <Xm/ToggleB.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <dia.h>
#include "imod.h"
#include "imod_info.h"

#if XmVERSION == 1
#if XmREVISION == 1
#define NOKEYTRANS
#endif
#endif

static String KeyTranslations = "<KeyDown>: defaultKeyInput()\n";
void imodPlugMenu(Widget parent, int pos);
static char *truncate_name(char *name, int limit);

XtAppContext Imod_info_context;
int Imod_info_quit = FALSE;
int ImodForbidLevel = FALSE;

Widget Imod_info_top;
Widget Imod_widget_image;
Widget Imod_widget_modelname;
Widget Imod_widget_blacklevel;
Widget Imod_widget_whitelevel;
Widget Imod_widget_object;
Widget Imod_widget_contour;
Widget Imod_widget_point;
Widget Imod_widget_info;
Widget Imod_widget_status;
Widget Imod_widget_blackval;
Widget Imod_widget_whiteval;
Widget Imod_widget_x;
Widget Imod_widget_y;
Widget Imod_widget_z;
Widget Imod_widget_model;
Widget Imod_widget_movie;
Widget ImodGlWidget;
Widget Imod_widget_float;
XColor Imod_object_color;
extern char *Imod_imagefile;
int imod_info_make_levels(Widget col1);
Widget wprintWidget(XtAppContext app, Widget parent);


int imod_info_force(void)
{
     imod_info_forbid();
     imod_info_input();
     imod_info_enable();
     return(0);
}

int imod_info_input(void)
{
     XEvent event_return;
     XFlush(XtDisplay(App->toplevel));
     /* This loop will execute if there is any pending event but will only
	process an X event. It was here originally.  */
     /*     while(XtAppPending(App->context)){
	  XtAppNextEvent(App->context, &event_return);
	  XtDispatchEvent(&event_return);
     } */

     /* This loop will execute only if there is an X event, and process it.
      It would avoid hanging up waiting for input */
     /* while(XtAppPending(App->context) & XtIMXEvent){
	  XtAppNextEvent(App->context, &event_return);
	  XtDispatchEvent(&event_return);
	  } */

     /* This loop will execute if there is any pending event and process all
	kinds of events.  It seems good to process all events... */
     while(XtAppPending(App->context) & XtIMAll)
	   XtAppProcessEvent(App->context, XtIMAll);


     return(0);
}

int imod_info_pending(void)
{
     if (XtAppPending(App->context))
	  return(1);
     else
	  return(0);
}

int imod_info_dispatch(void)
{
     XEvent event_return;
     XtAppNextEvent(App->context, &event_return);
     XtDispatchEvent(&event_return);
     return(0);
}

int imod_xinit(int *argc, char **argv)
{
     Colormap cmap;
     XVisualInfo *vlist;
     Window   window;
     XVisualInfo vistemp;
     Visual  *visual;
     Display *display;
     Screen  *screen;
     char *window_name = imodwfname("IMOD ZaP Window:");
     int depth, screen_num = 0;

     int i, vsize, v = 0;

     Imod_info_top = XtVaAppInitialize
	  (&Imod_info_context, "Imod", NULL, 0, argc, argv, NULL, 
	   XtNtitle,    window_name, NULL);
     if (window_name) free(window_name);
     
     if (!Imod_info_top){
	  fprintf(stderr, "Imod: couldn't open display.\n");
	  exit(-1);
     }

     display = XtDisplay(Imod_info_top);
     screen  = DefaultScreenOfDisplay(display);
     window  = RootWindowOfScreen(screen);
     depth   = DefaultDepthOfScreen(screen);
     visual  = DefaultVisualOfScreen(screen);
     
     dia_xinit(Imod_info_top, Imod_info_context, "Imod");
     return(0);
}

int imod_info_open(int argc, char **argv)
{
     Widget main_win, glw;
     Widget menubar, menu, submenu, subsubmenu;
     Widget scale, frame;
     Widget widget, bigrow, bigcol, textcol, col1, col2, minirow, minicol;
     Widget arrow, arrowcol;
     Widget gl;
     Widget wpr;
     XmString s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, help;
     XmString as1, as2, as3, as4, as5, as6, as7, as8, s14, s15;

/*#ifndef NOKEYTRANS*/
     XtTranslations keytrans = XtParseTranslationTable(KeyTranslations);
/*#endif*/

     Display *display = NULL;
     Atom wmclose;
     char *cname = "Imod_Information";
     char *filename;

     Colormap cmap;
     Visual *visual;
     int    depth;
     int    plugs;
     int    help_pos = 3;

     XtVaGetValues(App->toplevel,
		   XmNdepth, &depth,
		   XmNvisual, &visual,
		   XmNcolormap, &cmap,
		   NULL);
		   
     main_win = XtVaCreateManagedWidget
	  (cname, xmMainWindowWidgetClass, App->toplevel,
	   XmNdeleteResponse, XmDO_NOTHING,
	   XmNdepth,    depth,
	   XmNvisual,   visual,
	   XmNcolormap, cmap,
	   NULL);

     /* Create menu bar. */
     s1 = XmStringCreateSimple("File");
     s2 = XmStringCreateSimple("Edit");
     s3 = XmStringCreateSimple("Image");
     s4 = XmStringCreateSimple("Special");
     help  = XmStringCreateSimple("Help");

     
     plugs   = imodPlugLoaded(IMOD_PLUG_MENU);
     if (!plugs)
	  menubar = XmVaCreateSimpleMenuBar
	       (main_win, "menubar",
		XmNdepth,    depth,
		XmNvisual,   visual,
		XmNcolormap, cmap,
		XmVaCASCADEBUTTON, s1, 'F',
		XmVaCASCADEBUTTON, s2, 'E',
		XmVaCASCADEBUTTON, s3, 'I',
		XmVaCASCADEBUTTON, help, 'H',
		NULL);
     else
	  menubar = XmVaCreateSimpleMenuBar
	       (main_win, "menubar",
		XmNdepth,    depth,
		XmNvisual,   visual,
		XmNcolormap, cmap,
		XmVaCASCADEBUTTON, s1, 'F',
		XmVaCASCADEBUTTON, s2, 'E',
		XmVaCASCADEBUTTON, s3, 'I',
		XmVaCASCADEBUTTON, s4, 'S',
		XmVaCASCADEBUTTON, help, 'H',
		NULL);
     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(s4);


     /* Set Help */
     if (plugs){
	  help_pos++;
	  imodPlugMenu(menubar, 3); /* Install all menu plugs */
	  if (widget = XtNameToWidget(menubar, "button_4"))
	       XtVaSetValues(menubar, XmNmenuHelpWidget, widget, NULL);
     }else{
	  if (widget = XtNameToWidget(menubar, "button_3"))
	       XtVaSetValues(menubar, XmNmenuHelpWidget, widget, NULL);
     }
     
     /* Create File Menu */
     s1 = XmStringCreateSimple("New Model");
     s2 = XmStringCreateSimple("Open Model...");
     s3 = XmStringCreateSimple("Save Model");
     as3 = XmStringCreateSimple("s");
     s4 = XmStringCreateSimple("Save Model As...");
     s5 = XmStringCreateSimple("Write Model As");
     s8 = XmStringCreateSimple("Memory to TIFF...");
     s6 = XmStringCreateSimple("About");
     s7 = XmStringCreateSimple("Quit");

     menu = XmVaCreateSimplePulldownMenu
	  (menubar, "file_menu", 0, (XtCallbackProc)imod_file_cb,
	   XmNvisual,   visual,
	   XmVaPUSHBUTTON, s1, 'N', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'O', NULL, NULL,
	   XmVaPUSHBUTTON, s3, 'S', "<Key>s",  as3,
	   XmVaPUSHBUTTON, s4, 'A', NULL, NULL,
	   XmVaCASCADEBUTTON, s5, 'W',
	   XmVaPUSHBUTTON, s8, 'M', NULL, NULL,
	   XmVaPUSHBUTTON, s6, 'b', NULL, NULL,
#ifndef MENUBUTTONBUG
	   XmVaSEPARATOR,
#endif
	   XmVaPUSHBUTTON, s7, 'Q', NULL, NULL,
	   NULL);

     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(as3);
     XmStringFree(s4);
     XmStringFree(s5);
     XmStringFree(s6);
     XmStringFree(s7);
     XmStringFree(s8);
     s1 = XmStringCreateSimple("Imod");
     s2 = XmStringCreateSimple("Wimp");
     s3 = XmStringCreateSimple("NFF");
     s4 = XmStringCreateSimple("Synu");
     submenu = XmVaCreateSimplePulldownMenu
	  (menu, "format", 4, (XtCallbackProc)imod_file_write_cb,
	   XmNvisual,   visual,
	   XmVaPUSHBUTTON, s1, 'I', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'W', NULL, NULL,
	   XmVaPUSHBUTTON, s3, 'N', NULL, NULL,
	   XmVaPUSHBUTTON, s4, 'S', NULL, NULL,
	   NULL);

     /* Create Edit Menu */
     s1 = XmStringCreateSimple("Model");
     s2 = XmStringCreateSimple("Object");
     s7 = XmStringCreateSimple("Surface");
     s3 = XmStringCreateSimple("Contour");
     s4 = XmStringCreateSimple("Point");
     s5 = XmStringCreateSimple("Image"); 
     s6 = XmStringCreateSimple("Movies..."); 
     menu = XmVaCreateSimplePulldownMenu
	  (menubar, "edit_menu", 1, (XtCallbackProc)imod_edit_cb,
	   XmNvisual,   visual,
	   XmVaCASCADEBUTTON, s1, 'M',
	   XmVaCASCADEBUTTON, s2, 'O',
	   XmVaCASCADEBUTTON, s7, 'S',
	   XmVaCASCADEBUTTON, s3, 'C',
	   XmVaCASCADEBUTTON, s4, 'P',
	   XmVaCASCADEBUTTON, s5, 'I', 
	   XmVaPUSHBUTTON, s6, 'v', NULL, NULL,
	   NULL);
     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(s4);
     XmStringFree(s5); 
     XmStringFree(s6);
     XmStringFree(s7);

     s1 = XmStringCreateSimple("Header...");
     s2 = XmStringCreateSimple("Offsets...");
     s3 = XmStringCreateSimple("Clean");
     submenu = XmVaCreateSimplePulldownMenu
	  (menu, "model_menu", 0, (XtCallbackProc)imod_edit_model_cb,
	   XmNvisual,   visual,
	   XmVaPUSHBUTTON, s1, 'H', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'O', NULL, NULL,
	   XmVaPUSHBUTTON, s3, 'C', NULL, NULL,
	   NULL);
     XmStringFree(s1);
     XmStringFree(s2);

     /* Edit Object Sub Menu. */
     s1 = XmStringCreateSimple("New");
     s2 = XmStringCreateSimple("Delete");
     s3 = XmStringCreateSimple("Color...");
     s4 = XmStringCreateSimple("Type...");
     s5 = XmStringCreateSimple("Info");
     s6 = XmStringCreateSimple("Go To...");
     s7 = XmStringCreateSimple("Move...");
     s8 = XmStringCreateSimple("Clean");
     submenu = XmVaCreateSimplePulldownMenu
	  (menu, "object_menu", 1, (XtCallbackProc)imod_edit_object_cb,
	   XmNvisual,   visual,
	   XmVaPUSHBUTTON, s1, 'N', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'D', NULL, NULL,
#ifndef MENUBUTTONBUG
	   XmVaSEPARATOR,
#endif
	   XmVaPUSHBUTTON, s3, 'C', NULL, NULL,
	   XmVaPUSHBUTTON, s4, 'T', NULL, NULL,
	   XmVaPUSHBUTTON, s6, 'G', NULL, NULL,
	   XmVaPUSHBUTTON, s7, 'M', NULL, NULL,
	   XmVaPUSHBUTTON, s5, 'I', NULL, NULL,
	   XmVaPUSHBUTTON, s8, 'L', NULL, NULL,
	   NULL);
     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(s4);
     XmStringFree(s5);
     XmStringFree(s6);
     XmStringFree(s7);
     XmStringFree(s8);

     /* Edit Surface Sub Menu. */
     s1 = XmStringCreateSimple("New");
     as1 = XmStringCreateSimple("N");
     s2 = XmStringCreateSimple("Go To...");
     s3 = XmStringCreateSimple("Move...");
     submenu = XmVaCreateSimplePulldownMenu
	  (menu, "object_menu", 2, (XtCallbackProc)imod_edit_surface_cb,
	   XmNvisual,   visual,
	   XmVaPUSHBUTTON, s1, 'N', "<Key>N", as1,
	   XmVaPUSHBUTTON, s2, 'G', NULL, NULL,
	   XmVaPUSHBUTTON, s3, 'M', NULL, NULL,
	   NULL);
     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(as1);

     /* Edit Contour Sub Menu */
     s1 = XmStringCreateSimple("New");
     as1 = XmStringCreateSimple("n");
     s2 = XmStringCreateSimple("Delete");
     as2 = XmStringCreateSimple("D");
     s3 = XmStringCreateSimple("Move...");
     s4 = XmStringCreateSimple("Sort");
     s5 = XmStringCreateSimple("Auto...");
     s6 = XmStringCreateSimple("Type...");
     s7 = XmStringCreateSimple("Go To...");
     s8 = XmStringCreateSimple("Info");
     s9 = XmStringCreateSimple("Break...");
     s10 = XmStringCreateSimple("Fix by Z");
     s11 = XmStringCreateSimple("Join...");
     s12 = XmStringCreateSimple("Invert");
     s13 = XmStringCreateSimple("Copy...");
     s14 = XmStringCreateSimple("Loopback");
     as5 = XmStringCreateSimple("a");
     s15 = XmStringCreateSimple("Fill In");
     submenu = XmVaCreateSimplePulldownMenu
	  (menu, "contour_menu", 3, (XtCallbackProc)imod_edit_contour_cb,
	   XmNvisual,   visual,
	   XmVaPUSHBUTTON, s1, 'N', "<Key>n",  as1,
	   XmVaPUSHBUTTON, s2, 'D', "<Key>D",  as2,
	   XmVaPUSHBUTTON, s3, 'M', NULL, NULL,
	   XmVaPUSHBUTTON, s4, 'S', NULL, NULL,
	   XmVaPUSHBUTTON, s5, 'A', NULL, NULL,
	   XmVaPUSHBUTTON, s6, 'T', NULL, NULL,
	   XmVaPUSHBUTTON, s7, 'G', NULL, NULL,
	   XmVaPUSHBUTTON, s8, 'I', NULL, NULL,
	   XmVaPUSHBUTTON, s9, 'B', NULL, NULL,
	   XmVaPUSHBUTTON, s10, 'F', NULL, NULL,
	   XmVaPUSHBUTTON, s11, 'J', NULL, NULL,
	   XmVaPUSHBUTTON, s12, 'V', NULL, NULL,
	   XmVaPUSHBUTTON, s13, 'C', NULL, NULL,
	   XmVaPUSHBUTTON, s14, 'L', NULL, NULL,
	   XmVaPUSHBUTTON, s15, NULL, NULL, NULL,
	   NULL);
     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(s4);
     XmStringFree(s5);
     XmStringFree(s6);
     XmStringFree(s7);
     XmStringFree(s8);
     XmStringFree(s9);
     XmStringFree(s10);
     XmStringFree(s11);
     XmStringFree(s12);
     XmStringFree(s13);
     XmStringFree(s14);
     XmStringFree(s15);
     XmStringFree(as1);
     XmStringFree(as2);
     XmStringFree(as5);

     /* Edit point sub menu. */
     s1 = XmStringCreateSimple("Delete");
     s2 = XmStringCreateSimple("Sort by dist");
     s3 = XmStringCreateSimple("Sort by Z");
     s4 = XmStringCreateSimple("Distance");
     s5 = XmStringCreateSimple("Value");
     s6 = XmStringCreateSimple("Go To...");
     s7 = XmStringCreateSimple("Size...");
     submenu = XmVaCreateSimplePulldownMenu
	  (menu, "point_menu", 4, (XtCallbackProc)imod_edit_point_cb,
	   XmNvisual,   visual,
	   XmVaPUSHBUTTON, s1, 'D', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'o', NULL, NULL,
	   XmVaPUSHBUTTON, s3, 'S', NULL, NULL,
	   XmVaPUSHBUTTON, s4, 'i', NULL, NULL,
	   XmVaPUSHBUTTON, s5, 'V', NULL, NULL,
	   XmVaPUSHBUTTON, s6, 'G', NULL, NULL,
	   XmVaPUSHBUTTON, s7, 'z', NULL, NULL,
	   NULL);
     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(s4);
     XmStringFree(s5);
     XmStringFree(s6);
     XmStringFree(s7);

     s1 = XmStringCreateSimple("Process...");
     s3 = XmStringCreateSimple("Colormap...");
     s4 = XmStringCreateSimple("Reload...");
     s5 = XmStringCreateSimple("Flip");
     s6 = XmStringCreateSimple("Fill Cache");
     s7 = XmStringCreateSimple("Cache Filler...");
     submenu = XmVaCreateSimplePulldownMenu
	  (menu, "image_menu", 5, (XtCallbackProc)imod_edit_image_cb,
	   XmNvisual,   visual,
	   XmVaPUSHBUTTON, s1, 'P', NULL, NULL,
	   XmVaPUSHBUTTON, s3, 'C', NULL, NULL,
	   XmVaPUSHBUTTON, s4, 'R', NULL, NULL,
	   XmVaPUSHBUTTON, s5, 'F', NULL, NULL,
	   XmVaPUSHBUTTON, s6, 'i', NULL, NULL,
	   XmVaPUSHBUTTON, s7, 'M', NULL, NULL,
	   NULL);
     XmStringFree(s1);
     XmStringFree(s3);
     XmStringFree(s4);
     XmStringFree(s5);
     XmStringFree(s6);
     XmStringFree(s7);

     /* Create Window Menu */
     s1 = XmStringCreateSimple("Graph");
     as1 = XmStringCreateSimple("G");
     s2 = XmStringCreateSimple("Slicer");
     as2 = XmStringCreateSimple("\\");
     s3 = XmStringCreateSimple("Tumbler");
     s4 = XmStringCreateSimple("Tilt...");
     s5 = XmStringCreateSimple("Model View");
     as5 = XmStringCreateSimple("v");
     s6 = XmStringCreateSimple("ZaP");
     as6 = XmStringCreateSimple("z");
     s7 = XmStringCreateSimple("XYZ");
     s8 = XmStringCreateSimple("Pixel View");
     menu = XmVaCreateSimplePulldownMenu
	  (menubar, "win_menu", 2, (XtCallbackProc)imod_win_cb,
	   XmNvisual,   visual,
	   XmVaPUSHBUTTON, s1, 'G', "<Key>G", as1,
	   XmVaPUSHBUTTON, s2, 'S', "<Key>\\", as2,
	   XmVaPUSHBUTTON, s3, 'T', NULL, NULL,
	   XmVaPUSHBUTTON, s4, 't', NULL, NULL,
	   XmVaPUSHBUTTON, s5, 'M', "<Key>v",  as5,
	   XmVaPUSHBUTTON, s6, 'Z', "<Key>z",  as6,
	   XmVaPUSHBUTTON, s7, 'X', NULL, NULL,
	   XmVaPUSHBUTTON, s8, 'P', NULL, NULL,
	   NULL);
     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(s4);
     XmStringFree(s5);
     XmStringFree(s6);
     XmStringFree(s7);
     XmStringFree(as1);
     XmStringFree(as2);
     XmStringFree(as5);
     XmStringFree(as6);


     s1 = XmStringCreateSimple("Man Page");
     s2 = XmStringCreateSimple("Menus");
     s3 = XmStringCreateSimple("Hot Keys");
     
     menu = XmVaCreateSimplePulldownMenu
	  (menubar, "help_menu", help_pos, (XtCallbackProc)imod_help_cb,
	   XmNvisual,   visual,
	   XmVaPUSHBUTTON, s1, 'P', NULL, NULL,
	   XmVaPUSHBUTTON, s2, 'M', NULL, NULL,
	   XmVaPUSHBUTTON, s3, 'H', NULL, NULL,
	   NULL);
     XmStringFree(s1);
     XmStringFree(s2);
     XmStringFree(s3);
     XmStringFree(help);

     XtManageChild(menubar);
     

     /************************************************************************/
     /* create window gadgets.                                               */


     bigcol = XtVaCreateWidget
	 ("rowcol",  xmRowColumnWidgetClass, main_win, 
	  XmNorientation, XmVERTICAL, NULL);
   
     bigrow = XtVaCreateWidget
	 ("form",  xmFormWidgetClass, bigcol, 
	  XmNfractionBase, 2,
	  NULL);

     filename = imodwfname("Image:");
     filename = truncate_name(filename, 23);
     Imod_widget_image = XtVaCreateManagedWidget 
	  (filename, xmLabelWidgetClass, bigrow,
	   XmNleftAttachment,  XmATTACH_POSITION,
	   XmNleftPosition, 0,
	   NULL); 
     if (filename)
          free(filename);

     filename = imodwEithername("Model:", Imod_filename);
     filename = truncate_name(filename, 23);
     Imod_widget_modelname = XtVaCreateManagedWidget 
	  (filename, xmLabelWidgetClass, bigrow, 
	   XmNleftAttachment,  XmATTACH_POSITION,
	   XmNleftPosition, 1,
	   NULL); 
     if (filename)
          free(filename);
     XtManageChild(bigrow);

     bigrow = XtVaCreateWidget
	 ("rowcol",  xmRowColumnWidgetClass, bigcol, 
	  XmNpacking, XmPACK_COLUMN,
	  XmNnumColumns, 1, 
	  XmNorientation, XmHORIZONTAL,
	  NULL);

     /* Make obj, cont, pnt box. */
     {
	  frame = XtVaCreateManagedWidget
	       ("frame", xmFrameWidgetClass, bigrow, 
		XmNleftAttachment,  XmATTACH_FORM,
		XmNtopAttachment,  XmATTACH_FORM,
		NULL);
	  minirow = XtVaCreateManagedWidget
	       ("rowcol", xmRowColumnWidgetClass, frame, NULL);
	  arrowcol = XtVaCreateManagedWidget
	       ("arrowcol", xmRowColumnWidgetClass, minirow, 
		XmNorientation, XmHORIZONTAL, NULL);
	  arrow = XtVaCreateManagedWidget
	       ("arrow_up", xmArrowButtonGadgetClass, arrowcol, 
		XmNarrowDirection, XmARROW_UP, NULL);
	  

	  imodOverrideTranslations(arrow, keytrans);

	  XtAddCallback(arrow, XmNarmCallback, 
			(XtCallbackProc)imod_nextobj_cb, NULL);
	  arrow = XtVaCreateManagedWidget
	       ("arrow_up", xmArrowButtonGadgetClass, arrowcol,
		XmNarrowDirection, XmARROW_DOWN, NULL);

	  imodOverrideTranslations(arrow, keytrans);

	  XtAddCallback(arrow, XmNarmCallback, 
			(XtCallbackProc)imod_prevobj_cb, NULL);
	  Imod_widget_object = XtVaCreateManagedWidget
	       ("Object  #  1 /  1 ",
		xmLabelWidgetClass, arrowcol, NULL);
	  arrowcol = XtVaCreateManagedWidget
	       ("arrowcol", xmRowColumnWidgetClass, minirow, 
		XmNorientation, XmHORIZONTAL, NULL);
	  arrow = XtVaCreateManagedWidget
	       ("arrow_up", xmArrowButtonGadgetClass, arrowcol, 
		XmNarrowDirection, XmARROW_UP, NULL);
	  imodOverrideTranslations(arrow, keytrans);

	  XtAddCallback(arrow, XmNarmCallback, 
			(XtCallbackProc)imod_nextcont_cb, NULL);
	  arrow = XtVaCreateManagedWidget
	       ("arrow_up", xmArrowButtonGadgetClass, arrowcol,
		XmNarrowDirection, XmARROW_DOWN, NULL);
	  imodOverrideTranslations(arrow, keytrans);

	  XtAddCallback(arrow, XmNarmCallback, 
			(XtCallbackProc)imod_prevcont_cb, NULL);
	  Imod_widget_contour = XtVaCreateManagedWidget
	       ("Contour #  1 /  1 ",
		xmLabelWidgetClass, arrowcol, NULL);
	  arrowcol = XtVaCreateManagedWidget
	       ("arrowcol", xmRowColumnWidgetClass, minirow, 
		XmNorientation, XmHORIZONTAL, NULL);
	  arrow = XtVaCreateManagedWidget
	       ("arrow_up", xmArrowButtonGadgetClass, arrowcol, 
		XmNarrowDirection, XmARROW_UP, NULL);

	  imodOverrideTranslations(arrow, keytrans);

	  XtAddCallback(arrow, XmNarmCallback, 
			(XtCallbackProc)imod_nextpoint_cb, NULL);
	  arrow = XtVaCreateManagedWidget
	       ("arrow_up", xmArrowButtonGadgetClass, arrowcol,
		XmNarrowDirection, XmARROW_DOWN, NULL);
	  imodOverrideTranslations(arrow, keytrans);

	  XtAddCallback(arrow, XmNarmCallback, 
			(XtCallbackProc)imod_prevpoint_cb, NULL);
	  Imod_widget_point = XtVaCreateManagedWidget
	       ("Point   #  1 /  1 ",
		xmLabelWidgetClass, arrowcol, NULL);
     }


     /* Make box with x, y, & z coords. */
     {
	  frame = XtVaCreateManagedWidget
	       ("frame", xmFrameWidgetClass, bigrow, 
		XmNrightAttachment,  XmATTACH_FORM,
		XmNtopAttachment,  XmATTACH_FORM,
		NULL);
	  minirow = XtVaCreateManagedWidget
	       ("rowcol", xmRowColumnWidgetClass, frame, NULL);
	  arrowcol = XtVaCreateManagedWidget
	       ("arrowcol", xmRowColumnWidgetClass, minirow, 
		XmNorientation, XmHORIZONTAL, NULL);
	  arrow = XtVaCreateManagedWidget
	       ("arrow_left", xmArrowButtonGadgetClass, arrowcol, 
		XmNarrowDirection, XmARROW_LEFT, NULL);
	  XtAddCallback(arrow, XmNarmCallback, 
			(XtCallbackProc)imod_prevx_cb, NULL);
	  arrow = XtVaCreateManagedWidget
	       ("arrow_up", xmArrowButtonGadgetClass, arrowcol,
		XmNarrowDirection, XmARROW_RIGHT, NULL);
	  imodOverrideTranslations(arrow, keytrans);

	  XtAddCallback(arrow, XmNarmCallback, 
			(XtCallbackProc)imod_nextx_cb, NULL);
	  Imod_widget_x = XtVaCreateManagedWidget
	       ("X =  xxx / xxx  ",
		xmLabelWidgetClass, arrowcol, NULL);
	  arrowcol = XtVaCreateManagedWidget
	       ("arrowcol", xmRowColumnWidgetClass, minirow,
		XmNorientation, XmHORIZONTAL, NULL);
	  arrow = XtVaCreateManagedWidget
	       ("arrow_up", xmArrowButtonGadgetClass, arrowcol,
		XmNarrowDirection, XmARROW_UP, NULL);
	  imodOverrideTranslations(arrow, keytrans);

	  XtAddCallback(arrow, XmNarmCallback, 
			(XtCallbackProc)imod_nexty_cb, NULL);
	  arrow = XtVaCreateManagedWidget
	       ("arrow_up", xmArrowButtonGadgetClass, arrowcol,
		XmNarrowDirection, XmARROW_DOWN, NULL);
	  imodOverrideTranslations(arrow, keytrans);

	  XtAddCallback(arrow, XmNarmCallback, 
			(XtCallbackProc)imod_prevy_cb, NULL);
	  Imod_widget_y = XtVaCreateManagedWidget
	       ("Y =  xxx / xxx  ",
		xmLabelWidgetClass, arrowcol, NULL);
	  arrowcol = XtVaCreateManagedWidget
	       ("arrowcol", xmRowColumnWidgetClass, minirow,
		XmNorientation, XmHORIZONTAL, NULL);
	  arrow = XtVaCreateManagedWidget
	       ("arrow_up", xmArrowButtonGadgetClass, arrowcol,
		XmNarrowDirection, XmARROW_UP, NULL);
	  imodOverrideTranslations(arrow, keytrans);

	  XtAddCallback(arrow, XmNarmCallback, 
			(XtCallbackProc)imod_nextz_cb, NULL);
	  arrow = XtVaCreateManagedWidget
	       ("arrow_up", xmArrowButtonGadgetClass, arrowcol,
		XmNarrowDirection, XmARROW_DOWN, NULL);
	  imodOverrideTranslations(arrow, keytrans);

	  XtAddCallback(arrow, XmNarmCallback, 
			(XtCallbackProc)imod_prevz_cb, NULL);
	  Imod_widget_z = XtVaCreateManagedWidget
	       ("Z =  xxx / xxx  ",
		xmLabelWidgetClass, arrowcol, NULL);
     }
     XtManageChild(bigrow);


     bigrow = XtVaCreateWidget
	 ("rowcol", xmRowColumnWidgetClass, bigcol, 
	  XmNorientation, XmHORIZONTAL,
	  NULL);

     imod_info_make_levels(bigrow);

     Imod_widget_float  = XtVaCreateManagedWidget
	  ("Float",  xmToggleButtonWidgetClass, bigrow, NULL);
     XtAddCallback(Imod_widget_float, XmNvalueChangedCallback, 
		   imod_float_cb, NULL);
     XmToggleButtonSetState(Imod_widget_float, False, False);


     /* Make mouse mode radio gadget */
     /* DNM 6/8/01: changed to pass desired modes in "client" */
     {
	 Arg args[3]; 
	 int n = 0;
	 
	 XtSetArg(args[n], XmNrightAttachment,  XmATTACH_FORM); n++;
	 XtSetArg(args[n], XmNbottomAttachment,  XmATTACH_FORM); n++;
	 
	  minirow = XmCreateRadioBox(bigrow, "mouse_mode", args, n);
	  Imod_widget_movie = XtVaCreateManagedWidget
	       ("Movie Mode", xmToggleButtonGadgetClass, minirow, NULL);
	 imodOverrideTranslations(arrow, keytrans);

	  XtAddCallback(Imod_widget_movie, XmNdisarmCallback, 
			imod_mmode_cb, (XtPointer)IMOD_MMOVIE);
	  
	  Imod_widget_model = XtVaCreateManagedWidget
	       ("Model Mode", xmToggleButtonGadgetClass, minirow, NULL);
	 imodOverrideTranslations(arrow, keytrans);
	  XtAddCallback(Imod_widget_model, XmNdisarmCallback, imod_mmode_cb,
			(XtPointer)IMOD_MMODEL);
	  XtManageChild(minirow);
     }
     XtManageChild(bigrow);
     XtManageChild(bigcol);

     /* Make status and info widgets */
     frame = XtVaCreateManagedWidget
	  ("frame", xmFrameWidgetClass, main_win, 
	   NULL);

     wprintWidget(App->context, frame);

#ifdef DRAW_GL
     glxDummy(App->toplevel);
#endif

     /* Link Window Close to Quit */
     wmclose = XmInternAtom( XtDisplay(App->toplevel), 
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback( App->toplevel, wmclose, 
			     (XtCallbackProc)imod_info_quit, NULL);


     XtManageChild(frame);

#ifdef MWSETWORK
     XtVaSetValues(main_win,XmNworkWindow,frame,NULL);
#endif

     XmMainWindowSetAreas (main_win, menubar, bigcol, NULL, NULL, frame);

     XtRealizeWidget(App->toplevel);
     imod_info_forbid();
     imod_info_input();
     imod_info_enable();
#ifdef DRAW_GL
     installColormap(main_win, GlxDummyWidget);
#endif
     return(0);
}


int imod_info_make_levels(Widget parent)
{
     Widget row, col, w;
     Widget label;

/*#ifndef NOKEYTRANS*/
     XtTranslations keytrans = XtParseTranslationTable(KeyTranslations);
/*#endif*/

     col = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, parent, 
	   XmNleftAttachment,  XmATTACH_FORM,
	   XmNbottomAttachment,  XmATTACH_FORM,
	   XmNorientation, XmVERTICAL, NULL);

     row = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, col, 
	   XmNorientation, XmHORIZONTAL, NULL);
     XtVaCreateManagedWidget
	  ("Black", xmLabelWidgetClass, row, NULL);
     Imod_widget_blacklevel = XtVaCreateManagedWidget
	  ("Blacklevel", xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 255,
	   XmNvalue, 0,
	   XmNscaleMultiple, 1,
	   NULL);
     Imod_widget_blackval = XtVaCreateManagedWidget
	  ("000",  xmLabelWidgetClass, row, NULL);
     XtManageChild(row);
     imodOverrideTranslations(Imod_widget_blacklevel, keytrans);

     XtAddCallback(Imod_widget_blacklevel, XmNvalueChangedCallback,
		   (XtCallbackProc)imod_blacklevel_cb, NULL);
     XtAddCallback(Imod_widget_blacklevel, XmNdragCallback,
		   (XtCallbackProc)imod_blacklevel_cb, (XtPointer)1);

     
     row = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, col,
	   XmNorientation, XmHORIZONTAL, NULL);
     XtVaCreateManagedWidget
	  ("White", xmLabelWidgetClass, row, NULL);
     Imod_widget_whitelevel = XtVaCreateManagedWidget
	  ("Whitelevel", xmScaleWidgetClass, row,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 255,
	   XmNvalue, 255,
	   XmNscaleMultiple, 1, 
	   NULL);
     Imod_widget_whiteval = XtVaCreateManagedWidget
	  ("255", xmLabelWidgetClass, row, NULL);
     XtManageChild(row);
     imodOverrideTranslations(Imod_widget_whitelevel, keytrans);

     XtAddCallback(Imod_widget_whitelevel, XmNvalueChangedCallback,
		   (XtCallbackProc)imod_whitelevel_cb, NULL);
     XtAddCallback(Imod_widget_whitelevel, XmNdragCallback,
		   (XtCallbackProc)imod_whitelevel_cb, (XtPointer)1);
     XtManageChild(col);
     return(0);
}


/* Redisplay the model filename after it's changed */

void MaintainModelName(Imod *mod)
{
     XmString filename;
     char *filestr;
     int namelen;
     
     filestr = imodwEithername("Model:", Imod_filename);
     filestr = truncate_name(filestr, 23);
     if(!filestr) {
       filestr = (char *) malloc(1);
       if (filestr)
	   *filestr = 0x00;
     }
     filename = XmStringCreateSimple(filestr);
     XtVaSetValues(Imod_widget_modelname, XmNlabelString, filename, NULL);
     if (filestr)
          free(filestr);
     XmStringFree(filename);


     /* Copy filename into model structure */
     if (mod->fileName)
          free(mod->fileName);
     namelen = strlen(Imod_filename)+1;
     mod->fileName = malloc(namelen);
     if (mod->fileName)
	  memcpy(mod->fileName, Imod_filename, namelen);
}

static char *truncate_name(char *name, int limit)
{
     int len, i;
     char *new;
     if(!name)
          return(name);
     len = strlen(name);
     if (len <= limit)
          return(name);
     new = (char *) malloc(limit + 3);
     for (i = 0; i < limit - 1; i++)
          new[i] = name[i];
     new[limit - 1] = '.';
     new[limit] = '.';
     new[limit + 1] = '.';
     new[limit + 2] = 0x00;
     free(name);
     return(new);
}
