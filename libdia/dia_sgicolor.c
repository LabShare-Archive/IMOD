/*****************************************************************************
 *                                                                           *
 *   FILE: dia_sgicolor.c                                                    *
 *                                                                           *
 *   PURPOSE: SGI specific color dialog.                                    *
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

#define DRAW_X11
#ifdef DRAW_GL
#undef DRAW_GL
#endif
#include <stdlib.h>
#include <X11/Xlib.h>
#include <Xm/DialogS.h>
#include <Xm/PushBG.h>
#include <Xm/Label.h>
#include <Xm/PanedW.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>
#include <Xm/RowColumn.h>
#include "diaP.h"

#ifdef DRAW_GL
#include <X11/Xirisw/GlxMDraw.h>
#include <gl/gl.h>
#endif

struct Sgicolor{
     Widget dialog;
     Widget cwindow;
     Widget rwindow;
     Widget gwindow;
     Widget bwindow;
     short dred;
     short dgreen;
     short dblue;
     short red;
     short green;
     short blue;
     void  (*cb)(Widget, XtPointer, XtPointer
);
     XtPointer client_data;
     struct dia_color_cbs glc;
     XColor color;
     Colormap cmap;
};

#ifdef DRAW_GL
static GLXconfig glxSgicolorConfig [] = {
     { GLX_NORMAL, GLX_RGB, TRUE },
     { 0, 0, 0 }
};
#endif

/* DNM: added this to keep track of color allocation, because in some cases
   the pixel number comes back as zero, and testing on it didn't work */
static int AllocColor = False;

static void dia_sgicolor_cancel_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Sgicolor *sgic = (struct Sgicolor *)client_data;
     (sgic->red)   = sgic->dred;
     (sgic->green) = sgic->dgreen;
     (sgic->blue)  = sgic->dblue;

     if (sgic->cb){
	  sgic->glc.reason = DIA_CANCEL;
	  sgic->glc.red   = sgic->dred;
	  sgic->glc.green = sgic->dgreen;
	  sgic->glc.blue  = sgic->dblue;
	  sgic->cb(w, (XtPointer)sgic->client_data,(XtPointer)&(sgic->glc));
     }
     XtDestroyWidget(sgic->dialog);
     free(sgic);
     return;
}

static void dia_sgicolor_apply_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Sgicolor *sgic = (struct Sgicolor *)client_data;

     if (sgic->cb){
	  sgic->glc.reason = DIA_APPLY;

	  sgic->cb(sgic->dialog, 
		   (XtPointer)sgic->client_data,(XtPointer)&(sgic->glc));
     }
     return;
}

static void dia_sgicolor_ok_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Sgicolor *sgic = (struct Sgicolor *)client_data;

     if (sgic->cb){
	  sgic->glc.reason = DIA_OK;
	  sgic->cb(sgic->dialog, 
		   (XtPointer)sgic->client_data,(XtPointer)&(sgic->glc));
     }

     XtDestroyWidget(sgic->dialog);
     free(sgic);
     return;
}


static void dia_sgicolor_draw_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Sgicolor *sgic = (struct Sgicolor *)client_data;
     long winx, winy;
     float llv[2], ulv[2], lrv[2], urv[2];

     llv[0] = 0; llv[1] = 0;
     ulv[0] = 0; lrv[1] = 0;


/* Draw color window */
#ifdef DRAW_GL
     GLXwinset(XtDisplay(w), XtWindow(sgic->cwindow));
     RGBcolor(sgic->glc.red, sgic->glc.green, sgic->glc.blue);
     clear();
#else
     sgic->color.red   = (sgic->red)   << 8;
     sgic->color.green = (sgic->green) << 8;
     sgic->color.blue  = (sgic->blue)  << 8;
     if (AllocColor){
	 XFreeColors (XtDisplay(w), sgic->cmap, &(sgic->color.pixel), 1, 0);
	 AllocColor = False;
     }
     if (!XAllocColor(XtDisplay(w), sgic->cmap, &(sgic->color)))
         return;
     AllocColor = True;
     XtVaSetValues(sgic->cwindow, XmNbackground, sgic->color.pixel, NULL);
#endif

#ifdef DRAW_GL
     GLXwinset(XtDisplay(w), XtWindow(sgic->rwindow));
     getsize((long *)&winx, (long *)&winy);
     ulv[1] = winy;
     urv[1] = winy;
     lrv[0] = winx;
     urv[0] = winx;
     RGBcolor(0, sgic->glc.green, sgic->glc.blue);
     bgnpolygon();
       v2f(ulv);
       v2f(llv);
       RGBcolor(255, (sgic->glc.green), (sgic->glc.blue));
       v2f(urv);
     endpolygon();
     RGBcolor(255, 0, 0);
     bgnpolygon();
       v2f(urv);
       v2f(lrv);
       RGBcolor(0, 0, 0);
       v2f(llv);
     endpolygon();

     GLXwinset(XtDisplay(w), XtWindow(sgic->gwindow));
     RGBcolor((sgic->glc.red), 0, (sgic->glc.blue));
     bgnpolygon();
     v2f(ulv);
     v2f(llv);
     RGBcolor((sgic->glc.red), 255, (sgic->glc.blue));
     v2f(urv);
     endpolygon();

     bgnpolygon();
     RGBcolor(0, 255, 0);
     v2f(urv);
     v2f(lrv);
     RGBcolor(0, 0, 0);
     v2f(llv);
     endpolygon();
     
     
     GLXwinset(XtDisplay(w), XtWindow(sgic->bwindow));
     RGBcolor((sgic->glc.red), (sgic->glc.green), 0);
     bgnpolygon();
     v2f(ulv);
     v2f(llv);
     RGBcolor((sgic->glc.red), (sgic->glc.green), 255);
     v2f(urv);
     endpolygon();

     bgnpolygon();
     RGBcolor(0,0,255);
     v2f(urv);
     v2f(lrv);
     RGBcolor(0, 0, 0);
     v2f(llv);
     endpolygon();
#endif
     return;
}

static void dia_sgicolor_reshape_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
#ifdef DRAW_GL
     GLXwinset(XtDisplay(w), XtWindow(w));
     reshapeviewport();
#endif
     dia_sgicolor_draw_cb(w, client_data, call_data);
     return;
}

static void dia_sgired_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Sgicolor *sgic = (struct Sgicolor *)client_data;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data; 

     if (sgic->cb){
	  sgic->glc.red = cbs->value;
	  sgic->glc.reason = DIA_SLIDER_CHANGED;
	  sgic->cb(w, (XtPointer)sgic->client_data,(XtPointer)&(sgic->glc));
     }

     (sgic->red) = cbs->value;
     dia_sgicolor_draw_cb(w, (XtPointer)sgic, NULL);
     return;
}

static void dia_sgireddrag_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Sgicolor *sgic = (struct Sgicolor *)client_data;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
     
     if (sgic->cb){
	  sgic->glc.red = cbs->value;
	  sgic->glc.reason = DIA_SLIDER_DRAG;
	  sgic->cb(w, (XtPointer)sgic->client_data,(XtPointer)&(sgic->glc));
     }
     
     (sgic->red) = cbs->value;
     dia_sgicolor_draw_cb(w, (XtPointer)sgic, NULL);
     return;
}

static void dia_sgigreen_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Sgicolor *sgic = (struct Sgicolor *)client_data;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
         
     if (sgic->cb){
	  sgic->glc.green = cbs->value;
	  sgic->glc.reason = DIA_SLIDER_CHANGED;
	  sgic->cb(w,(XtPointer)sgic->client_data,(XtPointer)&(sgic->glc));
     }

     (sgic->green) = cbs->value;
     dia_sgicolor_draw_cb(w, (XtPointer)sgic, NULL);
     return;
}

static void dia_sgigreendrag_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Sgicolor *sgic = (struct Sgicolor *)client_data;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
     
     if (sgic->cb){
	  sgic->glc.green = cbs->value;
	  sgic->glc.reason = DIA_SLIDER_DRAG;
	  sgic->cb(w,(XtPointer)sgic->client_data,(XtPointer)&(sgic->glc));
     }
     
     (sgic->green) = cbs->value;
     dia_sgicolor_draw_cb(w, (XtPointer)sgic, NULL);
     return;
}

static void dia_sgiblue_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Sgicolor *sgic = (struct Sgicolor *)client_data;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;

     if (sgic->cb){
	  sgic->glc.blue = cbs->value;
	  sgic->glc.reason = DIA_SLIDER_CHANGED;
	  sgic->cb(w,(XtPointer)sgic->client_data, (XtPointer)&(sgic->glc));
     }
     (sgic->blue) = cbs->value;

     dia_sgicolor_draw_cb(w, (XtPointer)sgic, NULL);
     return;
}

static void dia_sgibluedrag_cb
(Widget w, XtPointer client_data, XtPointer call_data)
{
     struct Sgicolor *sgic = (struct Sgicolor *)client_data;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call_data;
     
     if (sgic->cb){
	  sgic->glc.blue = cbs->value;
	  sgic->glc.reason = DIA_SLIDER_DRAG;
	  sgic->cb(w,(XtPointer)sgic->client_data, (XtPointer)&(sgic->glc));
     }
     (sgic->blue) = cbs->value;
     
     dia_sgicolor_draw_cb(w, (XtPointer)sgic, NULL);
     return;
}


void dia_setcolor(short red, short green, short blue, char *prompt,
		  void (*cb)(), void *client_data)
{
     static short sred, sgreen, sblue;

     sred = red;
     sgreen = green;
     sblue = blue;

     if (!cb)
	  return;

/* seems we can't mix OpenGL and IrisGl, bummer. */
/*     
#ifdef __sgi
     dia_sgcolor(red,green,blue,prompt,cb,client_data);
#else
     dia_sgicolor(&sred, &sgreen, &sblue, prompt,cb,client_data);
#endif
*/
     dia_sgicolor(sred, sgreen, sblue, prompt,cb,client_data);
}

int dia_cbcolor(short red, short green, short blue, char *prompt,
		void (*cb)(), void *client_data)
{
     return(dia_sgicolor(red,green,blue,prompt,cb,client_data));
}


int dia_sgicolor(short red, short green, short blue, char *prompt,
		 void (*cb)(), void *client_data)

/*int dia_sgicolor(short *red, short *green, short *blue, char *prompt) */
{
     struct Sgicolor *sgicolor;
     Widget dialog, pane, form;
     Widget label, scale, button;
     Widget window, rowcol;
     int done = 0;
     Dimension h;
     
     sgicolor = (struct Sgicolor *)malloc(sizeof(struct Sgicolor));
     if (!sgicolor)
	 return(-1);
     sgicolor->red = red;
     sgicolor->blue = blue;
     sgicolor->green = green;
     sgicolor->dred = red;
     sgicolor->dgreen = green;
     sgicolor->dblue = blue;
     sgicolor->cb    = cb;
     sgicolor->client_data = client_data;
     sgicolor->glc.red = red;
     sgicolor->glc.green = green;
     sgicolor->glc.blue  = blue;
     if (cb){
	  sgicolor->red = (sgicolor->dred);
	  sgicolor->green = (sgicolor->dgreen);
	  sgicolor->blue  = (sgicolor->dblue);
     }

#ifndef DRAW_GL
     sgicolor->color.red   = red   << 8;
     sgicolor->color.green = green << 8;
     sgicolor->color.blue  = blue  << 8;
     sgicolor->color.flags = DoRed | DoGreen | DoBlue;
#endif
     
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
     sgicolor->dialog = dialog;
     pane = XtVaCreateWidget
	  ("pane", xmPanedWindowWidgetClass, dialog,
	   XmNsashWidth,  1,
	   XmNsashHeight, 1,
	   NULL);

     form = XtVaCreateWidget
	  ("form1", xmFormWidgetClass, pane, NULL);

     label = XtVaCreateManagedWidget
	  (prompt, xmLabelWidgetClass, form,
	   XmNleftAttachment,  XmATTACH_FORM,
	   XmNtopAttachment,    XmATTACH_FORM,
	   NULL);

     rowcol = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, form,
	   XmNtopAttachment,    XmATTACH_WIDGET,
	   XmNtopWidget, label,
	   XmNleftAttachment,  XmATTACH_FORM,
	   XmNrightAttachment,  XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   NULL);

/*****************************************************************************/
/*  Work area.                                                               */


#ifdef DRAW_GL     
     sgicolor->cwindow = XtVaCreateManagedWidget
	  ("glwidget", glxMDrawWidgetClass, rowcol,
	   GlxNglxConfig, glxSgicolorConfig,
	   XmNheight, 50,
	   NULL);
#else
     sgicolor->cwindow = XtVaCreateManagedWidget
	  ("colorwindow", widgetClass,  rowcol,
	   XmNheight, 50, NULL);
#endif

#ifdef DRAW_GL
     sgicolor->rwindow = XtVaCreateManagedWidget
	  ("glwidget", glxMDrawWidgetClass, rowcol,
	   GlxNglxConfig, glxSgicolorConfig,
	   XmNheight, 20,
	   NULL);
#endif
     scale = XtVaCreateManagedWidget
	  ("Red", xmScaleWidgetClass, rowcol,
	   XmNshowValue, True,
	   XmNorientation, XmHORIZONTAL,
	   XmNmaximum, 255,
	   XmNscaleMultiple, 1,
	   XmNvalue, red,
	   NULL);
     XtAddCallback(scale, XmNvalueChangedCallback, dia_sgired_cb, 
		   sgicolor);
     XtAddCallback(scale, XmNdragCallback, dia_sgireddrag_cb,
		   sgicolor);

#ifndef DRAW_GL
      XtVaCreateManagedWidget("Red", xmLabelWidgetClass, rowcol, NULL);
#endif

#ifdef DRAW_GL
     sgicolor->gwindow = XtVaCreateManagedWidget
	  ("glwidget", glxMDrawWidgetClass, rowcol,
	   GlxNglxConfig, glxSgicolorConfig,
	   XmNheight, 20,
	   NULL);
#endif

     scale = XtVaCreateManagedWidget
	  ("Green", xmScaleWidgetClass, rowcol,
	   XmNshowValue, True,
	   XmNorientation, XmHORIZONTAL,
	   XmNmaximum, 255,
	   XmNscaleMultiple, 1,
	   XmNvalue, green,
	   NULL);
     XtAddCallback(scale, XmNvalueChangedCallback, dia_sgigreen_cb,
		   sgicolor);
     XtAddCallback(scale, XmNdragCallback, dia_sgigreendrag_cb,
		   sgicolor);

#ifndef DRAW_GL
      XtVaCreateManagedWidget("Green", xmLabelWidgetClass, rowcol, NULL);
#endif

#ifdef DRAW_GL
     sgicolor->bwindow = XtVaCreateManagedWidget
	  ("glwidget", glxMDrawWidgetClass, rowcol,
	   GlxNglxConfig, glxSgicolorConfig,
	   XmNwidth, 128,
	   XmNheight, 20,
	   NULL);
#endif
         
     scale = XtVaCreateManagedWidget
	  ("Blue", xmScaleWidgetClass, rowcol,
	   XmNshowValue, True,
	   XmNorientation, XmHORIZONTAL,
	   XmNmaximum, 255,
	   XmNscaleMultiple, 1,
	   XmNvalue, blue,
	   NULL);
     XtAddCallback(scale, XmNvalueChangedCallback, dia_sgiblue_cb,
		   sgicolor);
     XtAddCallback(scale, XmNdragCallback, dia_sgibluedrag_cb,
		   sgicolor);

#ifndef DRAW_GL
      XtVaCreateManagedWidget("Blue", xmLabelWidgetClass, rowcol, NULL);
#endif

#ifdef DRAW_GL
     XtAddCallback(sgicolor->cwindow, GlxNresizeCallback,
		   dia_sgicolor_reshape_cb, sgicolor);
     XtAddCallback(sgicolor->rwindow, GlxNresizeCallback,
		   dia_sgicolor_reshape_cb, sgicolor);
     XtAddCallback(sgicolor->gwindow, GlxNresizeCallback,
		   dia_sgicolor_reshape_cb, sgicolor);
     XtAddCallback(sgicolor->bwindow, GlxNresizeCallback,
		   dia_sgicolor_reshape_cb, sgicolor);

     XtAddCallback(sgicolor->cwindow, GlxNexposeCallback,
		   dia_sgicolor_draw_cb, sgicolor);
     XtAddCallback(sgicolor->rwindow, GlxNexposeCallback,
		   dia_sgicolor_draw_cb, sgicolor);
     XtAddCallback(sgicolor->gwindow, GlxNexposeCallback,
		   dia_sgicolor_draw_cb, sgicolor);
     XtAddCallback(sgicolor->bwindow, GlxNexposeCallback,
		   dia_sgicolor_draw_cb, sgicolor);
#endif


/*  end of work area.                                                        */
/*****************************************************************************/
     XtManageChild (form);


/*****************************************************************************/
/* ACTION AREA                                                               */

     form = XtVaCreateWidget ("form2", xmFormWidgetClass, pane,
			      XmNfractionBase,    16,
			      NULL);
     
     button = XtVaCreateManagedWidget
	  ("OK", xmPushButtonGadgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         1,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        5,
	   XmNshowAsDefault,        True,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   dia_sgicolor_ok_cb, sgicolor);

     button = XtVaCreateManagedWidget
	  ("Apply", xmPushButtonGadgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         6,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        10,
	   XmNshowAsDefault,        True,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(button, XmNactivateCallback,
		   dia_sgicolor_apply_cb, sgicolor);

     button = XtVaCreateManagedWidget
	  ("Cancel", xmPushButtonGadgetClass, form,
	   XmNtopAttachment,        XmATTACH_FORM,
	   XmNbottomAttachment,     XmATTACH_FORM,
	   XmNleftAttachment,       XmATTACH_POSITION,
	   XmNleftPosition,         11,
	   XmNrightAttachment,      XmATTACH_POSITION,
	   XmNrightPosition,        15,
	   XmNdefaultButtonShadowThickness, 1,
	   NULL);
     XtAddCallback(button, XmNactivateCallback, 
		   dia_sgicolor_cancel_cb, sgicolor);

     XtManageChild(form);
     XtVaGetValues (button, XmNheight, &h, NULL);
     XtVaSetValues (form, XmNpaneMaximum, h, XmNpaneMinimum, h, NULL);
     XtManageChild (pane);

     dia_map_cb(dialog, NULL, NULL);

     XtPopup (dialog, XtGrabNone);
     
#ifndef DRAW_GL
     {
	 Status status;
	 XtVaGetValues(sgicolor->cwindow, XmNcolormap, 
		       &(sgicolor->cmap), NULL);
	 status = XAllocColor(XtDisplay(sgicolor->cwindow),
		     sgicolor->cmap, &(sgicolor->color));
	 if (status) {
	      XtVaSetValues(sgicolor->cwindow, XmNbackground,
			    sgicolor->color.pixel, NULL);
	      AllocColor = True;
	 }
     }
#endif

     if (sgicolor->cb){
	  sgicolor->glc.reason = DIA_INIT;
	  sgicolor->cb(sgicolor->dialog,
		       (XtPointer)sgicolor->client_data,
		       (XtPointer)&(sgicolor->glc));
     }
     return(0);
}

