/*  IN TESTING VERSION 0.2
 *
 *  imodPlugTime.c - IMOD plugin to control position of current time.
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
#include "imodplug.h"


#include <dia.h>
static void help_cb(Widget w, XtPointer client, XtPointer call)
{
    dia_vasmsg("Time Controls Help\n",
	       "The upper arrows step the time forwards or backwards "
	       "one increment.\n",
	       "The slider can be used to select the current time.  "
	       "Only the label will be updated while dragging the slicer.\n",
	       "The lower arrows movie the images forwards or backwards.  "
	       "Use the [Stop] button to stop the movie and the "
	       "[Close] button to close the movie dialog window.\n\n",
	       NULL);

}

/*
 *  Define a structure to contain all local plugin data.
 */
typedef struct
{
     ImodView    *view;
     Widget       window;

     /***** user data ****/

     Widget label;       
     Widget slider;
     Widget lstep, rstep;
     Widget left, right, stop;

     XtWorkProcId id;
     XtIntervalId tid;
     int          control;

     int    ctime;
     int    maxtime;
     int    timestep;
     int    delay;

}PlugData;

static PlugData thisPlug = { 0, 0 };

static Widget makeWorkArea(Widget parent);
static setWidgets(void);
Boolean timeMovie(XtPointer client_data);
void plugDraw_cb(ImodView *inImodView, void *client, int drawflag);
void plugClose_cb(ImodView *vi, void *client, int reason);

/*
 * Called by the imod plugin load function. 
 */
char *imodPlugInfo(int *type)
{
    if (type)
	*type = IMOD_PLUG_MENU;
     return("4D Time Controls");
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

     ivwGetTime(plug->view, &plug->ctime);
     plug->maxtime  = ivwGetMaxTime(plug->view);
     plug->id       = 0;
     plug->tid      = 0;
     plug->timestep = 0;
     plug->delay    = 0;
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
    int timestep = (int) client;
    PlugData *plug =  &thisPlug;

    plug->maxtime = ivwGetTime(plug->view, &plug->ctime);
    if (!plug->maxtime) return;

    plug->ctime += timestep;
    ivwSetTime(plug->view, plug->ctime);
    plug->maxtime = ivwGetTime(plug->view, &plug->ctime);
    setWidgets();
    ivwDraw(plug->view, IMOD_DRAW_ALL);
}

static void move_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug =  &thisPlug;
    plug->timestep = (int)client;

    if (plug->id)
	XtRemoveWorkProc(plug->id);
     if (plug->tid)
	 XtRemoveTimeOut(plug->tid);
    if (plug->timestep){
	plug->id = XtAppAddWorkProc(imodAppContext(), timeMovie, &plug);
	XtSetSensitive(plug->stop, True);
	setWidgets();
	if (plug->timestep > 0){
	    XtSetSensitive(plug->left, True);
	    XtSetSensitive(plug->right, False);
	}else{
	    XtSetSensitive(plug->left, False);
	    XtSetSensitive(plug->right, True);
	}
    }else{
	XtSetSensitive(plug->stop, False);
	setWidgets();
    }
}

static void slide_cb(Widget w, XtPointer client, XtPointer call)
{
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;

    PlugData *plug = (PlugData *)client;
    plug->ctime = cbs->value;
    ivwSetTime(plug->view, plug->ctime);
    plug->maxtime = ivwGetTime(plug->view, &plug->ctime);

    setWidgets();
    ivwDraw(plug->view, IMOD_DRAW_ALL);
}

static void drag_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = (PlugData *)client;
    XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
    XmString tlbl, xs1, xs2;
    char vals[32];
    int tmax, tcur;

    tmax = ivwGetTime(plug->view, &tcur);
    tcur = cbs->value;
    xs1 = XmStringCreateSimple(ivwGetTimeIndexLabel(plug->view, tcur));

    sprintf(vals, "%3d/%3d  ", tcur, tmax);
    xs2 = XmStringCreateSimple(vals);
    tlbl = XmStringConcat(xs2, xs1);
    XtVaSetValues(plug->label, XmNlabelString,
		  tlbl, NULL);
    XmStringFree(xs1);
    XmStringFree(xs2);
    XmStringFree(tlbl);
}

static Widget makeWorkArea(Widget parent)
{
    PlugData *plug = &thisPlug;

    Widget container;
    Widget row;
    Widget button, arrow;

    
    container = XtVaCreateWidget
	("container", xmRowColumnWidgetClass, parent, NULL);
    
    
    {
	row = XtVaCreateWidget
	    ("row", xmRowColumnWidgetClass, container,
	      XmNorientation, XmHORIZONTAL,
	     NULL);

	plug->lstep = XtVaCreateManagedWidget
	    ("arrow", xmArrowButtonWidgetClass, row,
	     XmNarrowDirection, XmARROW_LEFT, NULL);
	XtAddCallback(plug->lstep, XmNarmCallback, step_cb, (XtPointer)-1);
	
	plug->rstep = XtVaCreateManagedWidget
	    ("arrow", xmArrowButtonWidgetClass, row,
	     XmNarrowDirection, XmARROW_RIGHT, NULL);
	XtAddCallback(plug->rstep, XmNarmCallback, step_cb, (XtPointer)1);

	plug->label = XtVaCreateManagedWidget
	    ("000/000 00:00:00 Time Label", xmLabelWidgetClass, row, NULL);


	XtManageChild(row);
    }

    plug->slider = XtVaCreateManagedWidget
	("Scale",  xmScaleWidgetClass, container,
	 XmNorientation, XmHORIZONTAL,
	 XmNminimum, 1,
	 XmNmaximum, plug->maxtime,
	 XmNvalue, plug->ctime,
	 XmNshowValue, False,
	 XmNscaleMultiple, 1,
	 NULL);
    XtAddCallback(plug->slider, XmNvalueChangedCallback,
		  slide_cb, (XtPointer)plug);
    XtAddCallback(plug->slider, XmNdragCallback,
		  drag_cb, (XtPointer)plug);

    {   
	row = XtVaCreateWidget
	    ("row", xmRowColumnWidgetClass, container,
	      XmNorientation, XmHORIZONTAL,
	     NULL);
	plug->left = XtVaCreateManagedWidget
	    ("arrow", xmArrowButtonWidgetClass, row,
	     XmNarrowDirection, XmARROW_LEFT, NULL);
	XtAddCallback(plug->left, XmNarmCallback, move_cb, (XtPointer)-1);

	plug->right = XtVaCreateManagedWidget
	    ("arrow", xmArrowButtonWidgetClass, row,
	     XmNarrowDirection, XmARROW_RIGHT, NULL);
	XtAddCallback(plug->right, XmNarmCallback, move_cb, (XtPointer)1);
	
	plug->stop = XtVaCreateManagedWidget
	    ("Stop", xmPushButtonWidgetClass, row, NULL);
	XtAddCallback(plug->stop, XmNactivateCallback, move_cb, (XtPointer)0);
	XtSetSensitive(plug->stop, False);

	button = XtVaCreateManagedWidget
	    ("Close", xmPushButtonWidgetClass, row, NULL);
	XtAddCallback(button, XmNactivateCallback, quit_cb, plug);

	button = XtVaCreateManagedWidget
	    ("Help", xmPushButtonWidgetClass, row, NULL);
	XtAddCallback(button, XmNactivateCallback, help_cb, plug);


	XtManageChild(row);
    }

    XtManageChild(container);
    setWidgets();
    return 0;
}


/***************** The Low Level Guts of the plugin. ***********************/


static setWidgets(void)
{
    PlugData *plug =  &thisPlug;
    XmString tlbl, xs1, xs2;
    char vals[32];

    plug->maxtime = ivwGetTime(plug->view, &plug->ctime);
    xs1 = XmStringCreateSimple(ivwGetTimeLabel(plug->view));
    sprintf(vals, "%3d/%3d  ", plug->ctime, plug->maxtime);
    xs2 = XmStringCreateSimple(vals);
    tlbl = XmStringConcat(xs2, xs1);
    XtVaSetValues(plug->label, XmNlabelString,
		  tlbl, NULL);
    XmStringFree(xs1);
    XmStringFree(xs2);
    XmStringFree(tlbl);

    XtVaSetValues(plug->slider,
		  XmNvalue, plug->ctime,
		  NULL);

    if (plug->ctime > 1){
	if (plug->timestep >= 0)
	    XtSetSensitive(plug->left, True);
	XtSetSensitive(plug->lstep, True);
    }else{
	XtSetSensitive(plug->left, False);
	XtSetSensitive(plug->lstep, False);
    }

    if (plug->ctime < plug->maxtime){
	if (plug->timestep <= 0)
	    XtSetSensitive(plug->right, True);
	XtSetSensitive(plug->rstep, True);
    }else{
	XtSetSensitive(plug->right, False);
	XtSetSensitive(plug->rstep, False);
    }
    return 0;
}

void timeMovie_to(XtPointer client, XtIntervalId *id)
{
    PlugData *plug =  &thisPlug;
    plug->id = XtAppAddWorkProc(imodAppContext(), timeMovie, &plug);
    plug->tid = 0;
}

/* setup workprocs and timeout functions to movie through time. */
Boolean timeMovie(XtPointer client_data)
{
    PlugData *plug =  &thisPlug;
    int ntime;

    plug->maxtime = ivwGetTime(plug->view, &plug->ctime);
    if (!plug->maxtime) 
	return(True);
    ntime  = plug->timestep + plug->ctime;

    if (ntime <= 1){
	 plug->timestep = 0;
	 plug->ctime = 1;
	 ivwSetTime(plug->view, plug->ctime);
	 plug->maxtime = ivwGetTime(plug->view, &plug->ctime);
	 XtSetSensitive(plug->left, False);
	 XtSetSensitive(plug->right, True);
	 XtSetSensitive(plug->stop, False);
	 ivwDraw(plug->view, IMOD_DRAW_ALL);
	 return(True);
    }
    if (ntime >= (plug->maxtime)){
	 plug->timestep = 0;
	 plug->ctime =  plug->maxtime;
	 ivwSetTime(plug->view, plug->ctime);
         plug->maxtime = ivwGetTime(plug->view, &plug->ctime);
         ivwDraw(plug->view, IMOD_DRAW_ALL);
	 XtSetSensitive(plug->left, True);
	 XtSetSensitive(plug->right, False);
	 XtSetSensitive(plug->stop, False);
         return(True);
    }

    plug->ctime = ntime;
    ivwSetTime(plug->view, plug->ctime);
    plug->maxtime = ivwGetTime(plug->view, &plug->ctime);
    ivwDraw(plug->view, IMOD_DRAW_ALL);

    plug->tid = XtAppAddTimeOut(imodAppContext(), 33L, timeMovie_to, &plug);
    return(True);
}




/*
 * imod callback functions.
 *
 */
void plugDraw_cb(ImodView *inImodView, void *client, int drawflag)
{
    PlugData *plug = (PlugData *)client;
    int ctime;

    /*    ivwGetTime(plug->view, &ctime);

    if (ctime == plug->ctime) return;
    */
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

