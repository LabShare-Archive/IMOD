/*****************************************************************************
 *                                                                           *
 *   FILE: dia_sgcolor.c                                                    *
 *                                                                           *
 *   PURPOSE: SGI specific color dialog.                                    *
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

/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

#ifdef __sgi
#ifdef SYSV

#ifdef DRAW_GL
#include <X11/Xlib.h>
#include <Xm/DialogS.h>
#include <Sgm/ColorC.h>
#include "diaP.h"

typedef struct dia_color{
     diaDialog *dialog;
     short dred;
     short dgreen;
     short dblue;
     void  (*cb)(Widget,  XtPointer, XtPointer);
     XtPointer client_data;
     struct dia_color_cbs glc;
     XColor color;
     Colormap cmap;
}DiaColor;


static void cancel_cb(Widget w, XtPointer client_data, XtPointer call_data)
{
     DiaColor *sgic = (DiaColor *)client_data;
     SgColorChooserCallbackStruct *cbs =
	  (SgColorChooserCallbackStruct *)call_data;

     if (sgic->cb){
	  sgic->glc.reason = DIA_CANCEL;
	  sgic->glc.red   = sgic->dred;
	  sgic->glc.green = sgic->dgreen;
	  sgic->glc.blue  = sgic->dblue;
	  sgic->cb(w, (XtPointer)sgic->client_data,(XtPointer)&(sgic->glc));
     }
     diaDestroyDialog(sgic->dialog);
     free(sgic);
}

static void ok_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     DiaColor *sgic = (DiaColor *)client_data;
     SgColorChooserCallbackStruct *cbs =
	  (SgColorChooserCallbackStruct *)call_data;

     sgic->glc.red   = cbs->r;
     sgic->glc.green = cbs->g;
     sgic->glc.blue  = cbs->b;

     if (sgic->cb){
	  sgic->glc.reason = DIA_OK;
	  sgic->cb(w, (XtPointer)sgic->client_data,(XtPointer)&(sgic->glc));
     }
     diaDestroyDialog(sgic->dialog);
     free(sgic);
}

static void help_cb(Widget w, XtPointer client, XtPointer call)
{
     DiaColor *sgic = (DiaColor *)client;
     SgColorChooserCallbackStruct *cbs =
	  (SgColorChooserCallbackStruct *)call;
     
     if (sgic->cb){
	  sgic->glc.red    = cbs->r;
	  sgic->glc.green  = cbs->g;
	  sgic->glc.blue   = cbs->b;
	  sgic->glc.reason = DIA_HELP;
	  sgic->cb(w, (XtPointer)sgic->client_data,(XtPointer)&(sgic->glc));
     }
     return;
     return;
}

static void change_cb(Widget w, XtPointer client, XtPointer call)
{
     DiaColor *sgic = (DiaColor *)client;
     SgColorChooserCallbackStruct *cbs =
	  (SgColorChooserCallbackStruct *)call;
     
     if (sgic->cb){
	  sgic->glc.red    = cbs->r;
	  sgic->glc.green  = cbs->g;
	  sgic->glc.blue   = cbs->b;
	  sgic->glc.reason = DIA_VALUE_CHANGED;
	  sgic->cb(w, (XtPointer)sgic->client_data,(XtPointer)&(sgic->glc));
     }
     return;
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     DiaColor *color = (DiaColor *)client;
     diaDialog *dia = (diaDialog *)call;
     Widget sgcolor;

     sgcolor = XtVaCreateManagedWidget
	  ("SgColorChooser", sgColorChooserWidgetClass, w,
#ifdef DRAW_X11
	   SgNuseGl, False,
#endif	   
	   NULL);

     XtAddCallback(sgcolor, XmNcancelCallback, cancel_cb, client);
     XtAddCallback(sgcolor, XmNokCallback,   ok_cb, client);
     XtAddCallback(sgcolor, XmNapplyCallback,   change_cb, client);
     XtAddCallback(sgcolor, XmNhelpCallback, help_cb, client);
/*     XtAddCallback(sgcolor, XmNvalueChangedCallback, change_cb, client); */

     SgColorChooserSetColor
	  (sgcolor, color->dred, color->dgreen, color->dblue);
     SgColorChooserSetStoredColor
	  (sgcolor, color->dred, color->dgreen, color->dblue);
     return;
}


int dia_sgcolor(short red, short green, short blue, char *prompt,
		void (*cb)(Widget,  XtPointer, XtPointer), void *client_data)
{
     DiaColor *sgicolor;

     sgicolor = (DiaColor *)malloc(sizeof(DiaColor));
     if (!sgicolor)
	 return(-1);
     sgicolor->dred = red;
     sgicolor->dgreen = green;
     sgicolor->dblue = blue;
     sgicolor->cb    = cb;
     sgicolor->client_data = client_data;
     sgicolor->glc.red   = red;
     sgicolor->glc.green = green;
     sgicolor->glc.blue  = blue;

     sgicolor->dialog = diaVaCreateDialog
	  (prompt, dia_get_toplevel(), dia_get_context(),
	   DiaNworkAreaFunc, workarea_cb, (XtPointer)sgicolor,
	   0);

     return(0);
}

#else
int dia_sgcolor(short red, short green, short blue, char *prompt,
		void (*cb)(Widget,  XtPointer, XtPointer), void *client_data)
{
     return(-1);
}

#endif /* DRAW_GL */
#endif
#endif











