/*  
 *
 *  imodPlugOCP.c - IMOD plugin to control current Object, Contour, Point.
 */

/*****************************************************************************
 *   Copyright (C) 1997 by Boulder Laboratory for 3-Dimensional Fine         *
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

/* include basic X-Window, Motif and OpenGL headers.
 */
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>

#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/Scale.h>
#include <Xm/ArrowB.h>
#include <Xm/Separator.h>

#include <string.h>
#include <mrcslice.h>
#include <imodel.h>
#include "imodplug.h"

/*
 *  Define a structure to contain all local plugin data.
 */
typedef struct
{
     ImodView    *view;
     Widget       window;
     int          control;

     /***** user data ****/

     Widget olabel;       
     Widget oslider;
     Widget odec, oinc;

     Widget clabel;
     Widget cslider;
     Widget cdec, cinc;

     Widget plabel;
     Widget pslider;
     Widget pdec, pinc;

     /* Future movie data. */
     XtWorkProcId id;
     XtIntervalId tid;
     int    delay;

}PlugData;

/*
 * Cheat and call this internal imod function.
 */
void imod_setxyzmouse(void);


static PlugData thisPlug = { 0, 0, 0 };

void plugDraw_cb(ImodView *inImodView, void *client, int drawflag);
void plugClose_cb(ImodView *vi, void *client, int reason);
static Widget makeWorkArea(Widget parent);
static void   setWidgets  (void);

/*
 * Called by the imod plugin load function. 
 */
char *imodPlugInfo(int *type)
{
    if (type)
	*type = IMOD_PLUG_MENU;
     return("4D Model Controls");
}

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     PlugData *plug = (PlugData *)client;
     ivwDeleteControl(plug->view, plug->control);
}

/*
 *  Execute any function or program you wish with this function.
 *  Here we open up a window for user interaction.
 *  see imodplug.h for a list of support functions.
 */

void imodPlugExecute(ImodView *inImodView)
{
     Atom     wmclose;
     Widget   form;
     PlugData *plug;

     plug = &thisPlug;

     if (plug->view){
	 wprint("%s: already open.\n", imodPlugInfo(0));
	 return;
     }

     plug->view = inImodView;

     /* 
      * Bring the window to the front if already open.
      */
     if (plug->window) XtPopup(plug->window, XtGrabNone);

     /* 
      * Initialize user data. 
      */
     plug->id       = 0;
     plug->tid      = 0;
     plug->control  = ivwNewControl
	 (plug->view, plugDraw_cb, plugClose_cb, (XtPointer)plug);

     /*
      * This creates the plug window.
      */
     plug->window  = XtVaCreatePopupShell
	  (imodPlugInfo(0), topLevelShellWidgetClass, imodTopLevel(),
	   XmNvisual, imodVisual(),
	   XtNtitle, imodPlugInfo(0),
	   NULL);

     /* Make window conrols. */
     makeWorkArea(plug->window);

     /* Set up the quit function for the window. */
     wmclose = XmInternAtom( imodDisplay(),
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(plug->window, wmclose, quit_cb,
			     (caddr_t)plug);

     /* Open up the window. */
     XtPopup(plug->window, XtGrabNone);
}

/*********************** Window control commands *****************************/

static void step_cb(Widget w, XtPointer client, XtPointer call)
{
    int step = (int) client;
    PlugData *plug =  &thisPlug;
    Imod *imod = ivwGetModel(plug->view);
    int ob, co, pt;

    imodGetIndex(imod, &ob, &co, &pt);
    
    switch(step){
      case 3: ob++; break;
      case -3: ob--; break;
      case 2: co++; break;
      case -2: co--; break;
      case 1: pt++; break;
      case -1: pt--; break;
    }
    imodSetIndex(imod, ob, co, pt);
    imod_setxyzmouse();
    ivwDraw(plug->view, IMOD_DRAW_MOD|IMOD_DRAW_XYZ);
}

static void slide_cb(Widget w, XtPointer client, XtPointer call)
{
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
    PlugData *plug =  &thisPlug;
    int which = (int)client;
    Imod *imod = ivwGetModel(plug->view);
    int ob, co, pt;

    imodGetIndex(imod, &ob, &co, &pt);
    switch(which){
      case 1:
	pt = cbs->value;
	break;
      case 2:
	co = cbs->value;
	break;
      case 3:
	ob = cbs->value;
	break;
    }
    imodSetIndex(imod, ob, co, pt);
    imod_setxyzmouse();
    ivwDraw(plug->view, IMOD_DRAW_MOD|IMOD_DRAW_XYZ);
}

static void drag_cb(Widget w, XtPointer client, XtPointer call)
{
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
    PlugData *plug =  &thisPlug;
    int which = (int)client;
    Imod *imod = ivwGetModel(plug->view);
    Iobj  *obj     = imodObjectGet(imod);
    Icont *cont    = imodContourGet(imod);
    Ipoint *point  = imodPointGet(imod);
    XmString label;
    char vals[128];
    int ob, co, pt;
    char *plabel;

    switch(which){
      case 1:
	if (!cont) return;
	pt = cbs->value;
	plabel =  imodLabelItemGet(cont->label, pt);
	if (plabel)
	     sprintf(vals, "Point %3d / %3d : %s", 
		     pt+1, cont->psize, plabel);
	else
	     sprintf(vals, "Point %3d / %3d", pt+1, cont->psize);
        label = XmStringCreateSimple(vals);
        XtVaSetValues(plug->plabel, XmNlabelString, label, NULL);
        XmStringFree(label);
        break;
      case 2:
	if (!obj) return;
	co = cbs->value; 
	if (obj->cont[co].type)
	     sprintf(vals, "Contour %3d / %3d : Time (%d)", 
		     co+1, obj->contsize, obj->cont[co].type);
	else
	     sprintf(vals, "Contour %3d / %3d", co+1, obj->contsize);
        label = XmStringCreateSimple(vals);
        XtVaSetValues(plug->clabel, XmNlabelString, label, NULL);
        XmStringFree(label);	
        break;
      case 3:
	if (!imod) return;
	ob = cbs->value;
	sprintf(vals, "Object %3d / %3d : %s", ob+1, imod->objsize,
		imod->obj[ob].name);
	label = XmStringCreateSimple(vals);
	XtVaSetValues(plug->olabel, XmNlabelString, label, NULL);
        XmStringFree(label);	
        break;
    }
}

static Widget makeWorkArea(Widget parent)
{
    PlugData *plug = &thisPlug;

    Widget container;
    Widget row;
    Widget button, arrow;

    
    container = XtVaCreateWidget
	("container", xmRowColumnWidgetClass, parent, NULL);
    
    /*
     * Make object widgets.
     */
    {
	row = XtVaCreateWidget
	    ("row", xmRowColumnWidgetClass, container,
	      XmNorientation, XmHORIZONTAL,
	     NULL);

	plug->odec = arrow = XtVaCreateManagedWidget
	    ("arrow", xmArrowButtonWidgetClass, row,
	     XmNarrowDirection, XmARROW_LEFT, NULL);
	XtAddCallback(arrow, XmNarmCallback, step_cb, (XtPointer)-3);
	
	plug->oinc = arrow = XtVaCreateManagedWidget
	    ("arrow", xmArrowButtonWidgetClass, row,
	     XmNarrowDirection, XmARROW_RIGHT, NULL);
	XtAddCallback(arrow, XmNarmCallback, step_cb, (XtPointer)3);

	plug->olabel = XtVaCreateManagedWidget
	    ("Object 000 / 000", xmLabelWidgetClass, row, NULL);

	XtManageChild(row);

	plug->oslider = XtVaCreateManagedWidget
	    ("Scale",  xmScaleWidgetClass, container,
	     XmNorientation, XmHORIZONTAL,
	     XmNminimum, 0,
	     XmNmaximum, 1,
	     XmNvalue, 0,
	     XmNshowValue, False,
	     XmNscaleMultiple, 1,
	     NULL);
	XtAddCallback(plug->oslider, XmNvalueChangedCallback,
		      slide_cb, (XtPointer)3);
	XtAddCallback(plug->oslider, XmNdragCallback,
		      drag_cb, (XtPointer)3);
    }

    /*
     * Make contour widgets.
     */
    {
	row = XtVaCreateWidget
	    ("row", xmRowColumnWidgetClass, container,
	      XmNorientation, XmHORIZONTAL,
	     NULL);

	plug->cdec = arrow = XtVaCreateManagedWidget
	    ("arrow", xmArrowButtonWidgetClass, row,
	     XmNarrowDirection, XmARROW_LEFT, NULL);
	XtAddCallback(arrow, XmNarmCallback, step_cb, (XtPointer)-2);
	
	plug->cinc = arrow = XtVaCreateManagedWidget
	    ("arrow", xmArrowButtonWidgetClass, row,
	     XmNarrowDirection, XmARROW_RIGHT, NULL);
	XtAddCallback(arrow, XmNarmCallback, step_cb, (XtPointer)2);

	plug->clabel = XtVaCreateManagedWidget
	    ("Contour 000 / 000", xmLabelWidgetClass, row, NULL);

	XtManageChild(row);

	plug->cslider = XtVaCreateManagedWidget
	    ("Scale",  xmScaleWidgetClass, container,
	     XmNorientation, XmHORIZONTAL,
	     XmNminimum, 0,
	     XmNmaximum, 1,
	     XmNvalue, 0,
	     XmNshowValue, False,
	     XmNscaleMultiple, 1,
	     NULL);
	XtAddCallback(plug->cslider, XmNvalueChangedCallback,
		      slide_cb, (XtPointer)2);
	XtAddCallback(plug->cslider, XmNdragCallback,
		      drag_cb, (XtPointer)2);
    }

    /*
     * Make point widgets.
     */
    {
	row = XtVaCreateWidget
	    ("row", xmRowColumnWidgetClass, container,
	      XmNorientation, XmHORIZONTAL,
	     NULL);

	plug->pdec = arrow = XtVaCreateManagedWidget
	    ("arrow", xmArrowButtonWidgetClass, row,
	     XmNarrowDirection, XmARROW_LEFT, NULL);
	XtAddCallback(arrow, XmNarmCallback, step_cb, (XtPointer)-1);
	
	plug->pinc = arrow = XtVaCreateManagedWidget
	    ("arrow", xmArrowButtonWidgetClass, row,
	     XmNarrowDirection, XmARROW_RIGHT, NULL);
	XtAddCallback(arrow, XmNarmCallback, step_cb, (XtPointer)1);

	plug->plabel = XtVaCreateManagedWidget
	    ("Point  000 / 000", xmLabelWidgetClass, row, NULL);

	XtManageChild(row);

	plug->pslider = XtVaCreateManagedWidget
	    ("Scale",  xmScaleWidgetClass, container,
	     XmNorientation, XmHORIZONTAL,
	     XmNminimum, 0,
	     XmNmaximum, 1,
	     XmNvalue, 0,
	     XmNshowValue, False,
	     XmNscaleMultiple, 1,
	     NULL);
	XtAddCallback(plug->pslider, XmNvalueChangedCallback,
		      slide_cb, (XtPointer)1);
	XtAddCallback(plug->pslider, XmNdragCallback,
		      drag_cb, (XtPointer)1);
    }
/*
    XtVaCreateManagedWidget("separator", xmSeparatorWidgetClass, 
			    container, NULL);
    {   
	row = XtVaCreateWidget
	    ("row", xmRowColumnWidgetClass, container,
	     XmNorientation, XmHORIZONTAL,
	     XmNcolumns, 2,
	     NULL);    
	button = XtVaCreateManagedWidget
	    ("Close", xmPushButtonWidgetClass, row, NULL);
	XtAddCallback(button, XmNactivateCallback, quit_cb, plug);

	button = XtVaCreateManagedWidget
	    ("Help", xmPushButtonWidgetClass, row, NULL);
	XtAddCallback(button, XmNactivateCallback, quit_cb, plug);

	XtManageChild(row);
    }
*/
    XtManageChild(container);
    setWidgets();
    return 0;
}

static void blankmod(PlugData *plug)
{
    XmString label;

    XtSetSensitive(plug->oslider, False);
    XtSetSensitive(plug->odec, False);
    XtSetSensitive(plug->oinc, False);
    label = XmStringCreateSimple("Object NONE");
    XtVaSetValues(plug->olabel, XmNlabelString, label, NULL);
    XmStringFree(label);

    XtSetSensitive(plug->cslider, False);
    XtSetSensitive(plug->cdec, False);
    XtSetSensitive(plug->cinc, False);
    label = XmStringCreateSimple("Contour NONE");
    XtVaSetValues(plug->clabel, XmNlabelString, label, NULL);
    XmStringFree(label);

    XtSetSensitive(plug->pslider, False);
    XtSetSensitive(plug->pdec, False);
    XtSetSensitive(plug->pinc, False);
    label = XmStringCreateSimple("Object NONE");
    XtVaSetValues(plug->olabel, XmNlabelString, label, NULL);
    XmStringFree(label);
    return;
}

static void setWidgets(void)
{
    PlugData *plug =  &thisPlug;
    Imod  *imod    = ivwGetModel(plug->view);
    Iobj  *obj     = imodObjectGet(imod);
    Icont *cont    = imodContourGet(imod);
    Ipoint *point  = imodPointGet(imod);
    int ob, co, pt;
    int max, val;
    XmString label;
    char vals[128];

    imodGetIndex(imod, &ob, &co, &pt);

    if (!imod){
	blankmod(plug);
	return;
    }

    if (!obj){
	blankmod(plug);
	XtSetSensitive(plug->oinc, True);
        return;
    }else{
	if (imod->objsize < 2)
	    XtSetSensitive(plug->oslider, False);
	else{
	    XtSetSensitive(plug->oslider, True);
	    XtVaSetValues(plug->oslider, 
			  XmNmaximum, imod->objsize-1,
			  XmNvalue, ob,
			  NULL);
	}
	if (ob < 1)
	    XtSetSensitive(plug->odec, False);
	else
	    XtSetSensitive(plug->odec, True);
	if (ob >= imod->objsize - 1)
	    XtSetSensitive(plug->oinc, False);
        else
            XtSetSensitive(plug->oinc, True);
	sprintf(vals, "Object %3d / %3d : %s", ob+1, imod->objsize,
		imod->obj[ob].name);
	label = XmStringCreateSimple(vals);
	XtVaSetValues(plug->olabel, XmNlabelString, label, NULL);
	XmStringFree(label);
    }

    if (!cont){
	XtSetSensitive(plug->cslider, False);
	XtSetSensitive(plug->cdec, False);
	XtSetSensitive(plug->cinc, True);
	label = XmStringCreateSimple("Contour NONE");
	XtVaSetValues(plug->clabel, XmNlabelString, label, NULL);
	XmStringFree(label);
	XtSetSensitive(plug->pslider, False);
	XtSetSensitive(plug->pdec, False);
	XtSetSensitive(plug->pinc, False);
	label = XmStringCreateSimple("Object NONE");
	XtVaSetValues(plug->olabel, XmNlabelString, label, NULL);
	XmStringFree(label);
	return;
    }else{
	if (co < 1)
            XtSetSensitive(plug->cdec, False);
        else
            XtSetSensitive(plug->cdec, True);
        if (co >= obj->contsize - 1)
            XtSetSensitive(plug->cinc, False);
        else
            XtSetSensitive(plug->cinc, True);
	if (obj->contsize < 2)
            XtSetSensitive(plug->cslider, False);
        else{
            XtSetSensitive(plug->cslider, True);
            XtVaSetValues(plug->cslider,
                          XmNmaximum, obj->contsize-1,
                          XmNvalue, co,
                          NULL);
        }
	if (obj->cont[co].type)
	     sprintf(vals, "Contour %3d / %3d : Time (%d)", 
		     co+1, obj->contsize, obj->cont[co].type);
	else
	     sprintf(vals, "Contour %3d / %3d", co+1, obj->contsize);
	label = XmStringCreateSimple(vals);
	XtVaSetValues(plug->clabel, XmNlabelString, label, NULL);
	XmStringFree(label);
    }

    if (!point){
	XtSetSensitive(plug->pslider, False);
        XtSetSensitive(plug->pdec, False);
        XtSetSensitive(plug->pinc, False);
        label = XmStringCreateSimple("Object NONE");
        XtVaSetValues(plug->olabel, XmNlabelString, label, NULL);
        XmStringFree(label);
    }else{
	 char *plabel;
	if (pt < 1)
            XtSetSensitive(plug->pdec, False);
        else
            XtSetSensitive(plug->pdec, True);
        if (pt >= cont->psize - 1)
            XtSetSensitive(plug->pinc, False);
        else
            XtSetSensitive(plug->pinc, True);
        if (cont->psize < 2)
            XtSetSensitive(plug->pslider, False);
        else{
            XtSetSensitive(plug->pslider, True);
	    XtVaSetValues(plug->pslider, 
			  XmNmaximum, cont->psize-1,
			  XmNvalue, pt,
			  NULL);
	}
	plabel =  imodLabelItemGet(cont->label, pt);
	if (plabel)
	     sprintf(vals, "Point %3d / %3d : %s", 
		     pt+1, cont->psize, plabel);
	else
	     sprintf(vals, "Point %3d / %3d", pt+1, cont->psize);
	label = XmStringCreateSimple(vals);
	XtVaSetValues(plug->plabel, XmNlabelString, label, NULL);
	XmStringFree(label);
    }

}

/*
 * imod callback functions.
 *
 */
void plugDraw_cb(ImodView *inImodView, void *client, int drawflag)
{
    PlugData *plug = (PlugData *)client;

    setWidgets();
}

void plugClose_cb(ImodView *vi, void *client, int reason)
{
     PlugData *plug = (PlugData *)client;

     if (plug->tid)
	 XtRemoveTimeOut(plug->tid);
     if (plug->id)
	 XtRemoveWorkProc(plug->id);

     plug->view = NULL;
     XtPopdown(plug->window);
     XtDestroyWidget(plug->window);
     plug->window = 0;
}

