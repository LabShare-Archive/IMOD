/*****************************************************************************
 *                                                                           *
 *   FILE: dia_color.c                                                       *
 *                                                                           *
 *   PURPOSE: Pick a color dialog.                                           *
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
#include <Xm/PushBG.h>
#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>
#include <Xm/RowColumn.h>
#include "diaP.h"

XColor Dia_color;
Widget Dia_colorwindow;
int Dia_red;
int Dia_green;
int Dia_blue;
Widget Dia_color_dialog;

void dia_color_ok_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     int *done = (int *)client_data;
     *done = 1;
}


void dia_color_cancel_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     int *done = (int *)client_data;
     *done = -1;
}

void dia_colorset_cb(Widget scale_w, 
		     XtPointer client_data, XtPointer call_data)
{
     int rgb = (int) client_data;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
/*     Colormap cmap = DefaultColormapOfScreen(XtScreen (Dia_color_dialog)); */
     Colormap cmap;

     cmap = dia_getcolormap();
     
     switch (rgb) {
	case DoRed:
	  Dia_color.red = (cbs->value << 8);
	  Dia_red = cbs->value;
	  break;
	case DoGreen:
	  Dia_color.green = (cbs->value << 8);
	  Dia_green = cbs->value;
	  break;
	case DoBlue:
	  Dia_color.blue = (cbs->value << 8);
	  Dia_blue = cbs->value;
	  break;
     }
     
     XFreeColors (XtDisplay(Dia_color_dialog), cmap, &Dia_color.pixel, 1, 0); 
     if (!XAllocColor(XtDisplay(Dia_color_dialog), cmap, &Dia_color)) 
	  return; 
     
     XtVaSetValues( Dia_colorwindow, XmNbackground, Dia_color.pixel, NULL);
}


int dia_color(int *red, int *green, int *blue)
{

     Widget dialog, pane, form;
     Widget label, scale, button;
     Widget window, rowcol1, rowcol2,  colorwindow;
     int done = 0;
     Colormap cmap;
     Dimension h;
     Dia_red   = *red;
     Dia_green = *green;
     Dia_blue  = *blue;
     Dia_color.red   = *red << 8;
     Dia_color.green = *green << 8;
     Dia_color.blue  = *blue << 8;

     if (DefaultDepthOfScreen(XtScreen(Dia_toplevel)) < 2)
	  return(-1);

     dialog = XtVaCreatePopupShell
	  (Dia_title,  xmDialogShellWidgetClass,  Dia_toplevel,
	   XmNvisual, dia_getvisual(),
	   XmNdefaultPosition, True,
	   XmNdeleteResponse, XmDESTROY,
	   XmNdialogStyle, XmDIALOG_PRIMARY_APPLICATION_MODAL,
	   NULL);

     if (!dialog)
	  return(-1);
     XtAddCallback(dialog, XmNpopupCallback, dia_map_cb, NULL);
     Dia_color_dialog = dialog;

     pane = XtVaCreateWidget
	  ("pane", xmPanedWindowWidgetClass, dialog,
	   XmNsashWidth,  1,
	   XmNsashHeight, 1,
	   NULL);

     form = XtVaCreateWidget
	  ("form1", xmFormWidgetClass, pane, 
	   XtVaTypedArg, XmNbackground, XmRString, "Grey", 5,
	   NULL);


     rowcol1 = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, form,
	   XtVaTypedArg, XmNbackground, XmRString, "Grey", 5,
	   XmNtopAttachment,    XmATTACH_FORM,
	   XmNleftAttachment,  XmATTACH_FORM,
	   XmNrightAttachment,  XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   NULL);
     
     Dia_colorwindow = XtVaCreateManagedWidget
	  ("colorwindow", widgetClass, rowcol1,
	   XmNheight, 100,
	   XmNbackground, Dia_color.pixel,
	   NULL);
     
     rowcol2 = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, rowcol1,
	   XmNorientation, XmHORIZONTAL,
	   XtVaTypedArg, XmNbackground, XmRString, "Grey", 5,
	   NULL);
     
     scale = XtVaCreateManagedWidget
	  ("Red", xmScaleWidgetClass, rowcol2,
	   XmNshowValue, True,
	   XmNmaximum, 255,
	   XmNscaleMultiple, 1,
	   XmNvalue, *red,
	   XtVaTypedArg, XmNtitleString, XmRString,  "Red", 4,
	   XtVaTypedArg, XmNbackground,  XmRString,  "Grey", 5,
	   XtVaTypedArg, XmNforeground,  XmRString,  "Red", 4,
	   NULL);
     XtAddCallback(scale, XmNvalueChangedCallback, dia_colorset_cb, 
		   (XtPointer)DoRed);
     XtAddCallback(scale, XmNdragCallback, dia_colorset_cb, 
		   (XtPointer)DoRed);
     
     scale = XtVaCreateManagedWidget
	  ("Green", xmScaleWidgetClass, rowcol2,
	   XmNshowValue, True,
	   XmNmaximum, 255,
	   XmNscaleMultiple, 1,
	   XmNvalue, *green,
	   XtVaTypedArg, XmNtitleString, XmRString, "Green", 6,
	   XtVaTypedArg, XmNbackground,  XmRString, "Grey", 5,
	   XtVaTypedArg, XmNforeground, XmRString,  "Green", 6,
	   NULL);
     XtAddCallback(scale, XmNvalueChangedCallback, dia_colorset_cb, 
		   (XtPointer)DoGreen);
     XtAddCallback(scale, XmNdragCallback, dia_colorset_cb, 
		   (XtPointer)DoGreen);
     
     scale = XtVaCreateManagedWidget
	  ("Blue", xmScaleWidgetClass, rowcol2,
	   XmNshowValue, True,
	   XmNmaximum, 255,
	   XmNscaleMultiple, 1,
	   XmNvalue, *blue,
	   XtVaTypedArg, XmNtitleString, XmRString, "Blue", 5,
	   XtVaTypedArg, XmNbackground,  XmRString, "Grey", 5,
	   XtVaTypedArg, XmNforeground, XmRString,  "Blue", 5,
	   NULL);
     XtAddCallback(scale, XmNvalueChangedCallback, dia_colorset_cb, 
		   (XtPointer)DoBlue);
     XtAddCallback(scale, XmNdragCallback, dia_colorset_cb, 
		   (XtPointer)DoBlue);
     
     XtManageChild (form);

     /* ACTION AREA */
     form = XtVaCreateWidget ("form2", xmFormWidgetClass, pane,
			      XmNfractionBase,    7,
			      NULL);
     
     button = XtVaCreateManagedWidget
	  ("OK", xmPushButtonGadgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         1,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        3,
	   XmNshowAsDefault,        True,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   dia_color_ok_cb, &done);

     button = XtVaCreateManagedWidget
	  ("Cancel", xmPushButtonGadgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         4,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        6,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   dia_color_cancel_cb, &done);

     XtManageChild(form);
     XtVaGetValues (button, XmNheight, &h, NULL);
     XtVaSetValues (form, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
     XtManageChild (pane);



     XtPopup (dialog, XtGrabNone);
     cmap = DefaultColormapOfScreen(XtScreen (Dia_color_dialog));
     Dia_color.flags = DoRed | DoGreen | DoBlue;
     XAllocColor(XtDisplay(Dia_color_dialog), cmap, &Dia_color);
     XtVaSetValues( Dia_colorwindow, XmNbackground, Dia_color.pixel, NULL);
     while (!done)
	   XtAppProcessEvent(Dia_context, XtIMAll);
     
     XtDestroyWidget(dialog);

     if (done < 0)
	  return(1);

     *red   = Dia_red;
     *green = Dia_green;
     *blue  = Dia_blue;

     return(0);
}


