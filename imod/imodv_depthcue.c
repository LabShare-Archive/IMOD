/*  IMOD VERSION 2.40
 *
 *  imodv_depthcue.c -- Depth cue dialog for imodv.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/ArrowB.h>
#include <Xm/Scale.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <dia.h>
#include "imodv.h"

#define MINDEPTH 0
#define MAXDEPTH 100

struct{
     diaDialog *dia;
     ImodvApp  *a;
     int       fstart, fend;
     int       fmode;
     Widget    wStart;
     Widget    wEnd;
     Widget    wToggle;
     
}imodvDepthCueData = {0, 0, MINDEPTH, 50, GL_LINEAR, 
		      0, 0, 0};
void imodv_resize_cb(Widget w, XtPointer client, XtPointer call);

static Widget mkWorkArea(ImodvApp *a, Widget top);

static void help_cb()
{
     dia_vasmsg
	  ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
	   "Depth Cue Edit Dialog Help.\n"
	   "~~~~~~~~~~~~~~~~~~~~~~~~"
	   "\n\n",
	   "\tDepth cueing is implemented by having objects blend into the "
	   "background color with increasing distance from the viewer.  "
	   "The range of distances over which this blending occurs is "
	   "controlled by the sliders.\n",
	   "\tThe [Start] slider controls the distance at which the blending "
	   "starts - everything in front of this distance will not be "
	   "fogged.  Typically you would move this slider until the frontmost "
	   "features in your model appear at full intensity.\n",
	   "\tThe [End] slider controls the distance beyond which everything "
	   "will disappear.  Move this slider to change how fast the blending "
	   "into the background occurs.\n",
	   "\tUse the [Depth Cue] checkbox to turn the cueing on and off.\n",
	   NULL);
     return;
}

void imodvDepthCueSetWidgets(void)
{
    int depthcue;
    ImodvApp *a = Imodv;

    a->depthcue = a->imod->view->world & VIEW_WORLD_DEPTH_CUE;
    imodvDepthCueData.fend =  (int)a->imod->view->dcend;
    if (imodvDepthCueData.dia){
	a->depthcue = depthcue = a->imod->view->world & VIEW_WORLD_DEPTH_CUE;
	imodvDepthCueData.fstart = (int)(a->imod->view->dcstart * 100.0f);
	imodvDepthCueData.fend   = (int)(a->imod->view->dcend * 100.0f);
	if (depthcue)
	    XmToggleButtonSetState(imodvDepthCueData.wToggle, True, False);
	else
	    XmToggleButtonSetState(imodvDepthCueData.wToggle, False, False);
	if (imodvDepthCueData.wEnd)
	    XtVaSetValues(imodvDepthCueData.wEnd, 
			  XmNvalue,imodvDepthCueData.fend, NULL); 
	if (imodvDepthCueData.wStart)
	    XtVaSetValues(imodvDepthCueData.wStart, 
			  XmNvalue,imodvDepthCueData.fstart, NULL); 
    }
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     mkWorkArea(imodvDepthCueData.a, w);
     return;
}

static void done_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     diaDestroyDialog(dia);
     imodvDepthCueData.dia = NULL;
     return;
}

#ifdef OLDDC
void imodvAdjustDepthCue(void)
{
    Ipoint maxp;
     float bgcolor[4];
     int depthRange[3];
     float fstart, fend;
     float drange;

     if (Imodv->cindex) return;

     glGetFloatv(GL_COLOR_CLEAR_VALUE, bgcolor);
     glFogfv(GL_FOG_COLOR, bgcolor);

     glGetIntegerv(GL_DEPTH_RANGE, depthRange);
     depthRange[2] = depthRange[1] - depthRange[0];

    imodel_maxpt(Imodv->imod, &maxp);
    maxp.z *= Imodv->imod->zscale;
    drange = (maxp.x * maxp.x) + (maxp.y * maxp.y) + (maxp.z * maxp.z);
    drange = (float)sqrt((double)drange);
     fstart = 0.0f;
     
     fend   = drange;
     fend  -= (((float)imodvDepthCueData.fend)  * 0.01) * drange;

/*
     fprintf(stderr, "Depth Cue start %d : %g       end %d : %g\n",
	     imodvDepthCueData.fstart, fstart,
	     imodvDepthCueData.fend, fend);
*/

     glFogi(GL_FOG_MODE, imodvDepthCueData.fmode);
     glFogf(GL_FOG_DENSITY, 1.0f);


     glFogf(GL_FOG_START, fstart);
     glFogf(GL_FOG_END, fend);
 
    return;
}

void imodvDepthCueSet()
{

    ImodvApp *a = Imodv;
    if (a->depthcue){
	imodvAdjustDepthCue();
	glEnable(GL_FOG);
    }else{
	glDisable(GL_FOG);
    }
}
#endif

void imodvDepthCueEditDialog(ImodvApp *a, int state)
{
     XtPointer cbd = (XtPointer)(&imodvDepthCueData);

     imodvDepthCueData.a = Imodv;
     if (!state){
	  if (imodvDepthCueData.dia)
	       done_cb(NULL, NULL, (XtPointer)imodvDepthCueData.dia);
	  return;
     }
     if (imodvDepthCueData.dia){
	  XRaiseWindow(a->display, 
		       XtWindow(imodvDepthCueData.dia->dialog));
	  return;
     }

     imodvDepthCueData.dia = diaVaCreateDialog
	  ("Imodv: Depth Cue", a->topLevel, a->context,
	   DiaNcontrolButton, "Done", done_cb, cbd,
	   DiaNcontrolButton, "Help", help_cb, cbd,
	   DiaNworkAreaFunc,  workarea_cb,     cbd,
	   DiaNwindowQuit,    done_cb,         cbd,
	   0);
     return;
}

/****************************************************************************/
/* Dialog controls.                                                 */

static void near_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     ImodvApp *a = (ImodvApp *)client;
     imodvDepthCueData.fstart = cbs->value;

     a->imod->view->dcstart = (float)cbs->value * 0.01f;
     imodvDraw(a);
     return;
}
static void far_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     ImodvApp *a = (ImodvApp *)client;

     /*
      * glFogf(GL_FOG_END, (Imodv->imod->view->rad * (float)cbs->value)
      *	                * 0.01);
      */
     
     imodvDepthCueData.fend = cbs->value;
     a->imod->view->dcend = (float)cbs->value * 0.01f;
     imodvDraw(a);
     return;
}

static void toggle_cb(Widget w, XtPointer client, XtPointer call)
{
    XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
    ImodvApp *a = (ImodvApp *)client;
    int depthcue = a->imod->view->world & VIEW_WORLD_DEPTH_CUE;
    
    if (depthcue){
	depthcue = 0;
	XmToggleButtonSetState (w, False, False);
	a->imod->view->world &= ~VIEW_WORLD_DEPTH_CUE;
    }else{
	depthcue = 1;
	XmToggleButtonSetState (w, True, False);
	a->imod->view->world |= VIEW_WORLD_DEPTH_CUE;
    }

    a->depthcue = a->imod->view->world & VIEW_WORLD_DEPTH_CUE;
    imodvDraw(a);
}

static Widget mkWorkArea(ImodvApp *a, Widget top)
{
     Widget frame, col;
     int depthcue;

     depthcue = a->imod->view->world & VIEW_WORLD_DEPTH_CUE;
     imodvDepthCueData.fstart = (int)(a->imod->view->dcstart * 100.0f);
     imodvDepthCueData.fend   = (int)(a->imod->view->dcend * 100.0f);

     frame = XtVaCreateWidget
	  ("frame", xmFrameWidgetClass, top, NULL);

     col = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, frame,
	   NULL);
     {
	 
	 imodvDepthCueData.wToggle = XtVaCreateManagedWidget
	     ("Depth Cue", xmToggleButtonWidgetClass, col,
	      XmNset, depthcue,
	      NULL);

	 XtAddCallback(imodvDepthCueData.wToggle,
		       XmNvalueChangedCallback, toggle_cb, a);	 
	 if (depthcue)
	      XmToggleButtonSetState(imodvDepthCueData.wToggle, True, False);
	 else
	      XmToggleButtonSetState(imodvDepthCueData.wToggle, False, False);

	 /* DNM: add XmNscaleMultiple to get left button click to work right */
	 imodvDepthCueData.wStart = XtVaCreateManagedWidget
	      ("Start",  xmScaleWidgetClass, col,
	       XmNorientation, XmHORIZONTAL,
	       XmNminimum, MINDEPTH,  XmNmaximum, MAXDEPTH,
	       XmNshowValue, True,
	       XmNvalue, imodvDepthCueData.fstart,    
	       XmNscaleMultiple, 1,
	       XtVaTypedArg, XmNtitleString, XmRString, 
	       "Start", 6,
	       NULL);
	  XtAddCallback(imodvDepthCueData.wStart,
			XmNvalueChangedCallback,
			near_cb, (XtPointer)a);

	 imodvDepthCueData.wEnd = XtVaCreateManagedWidget
	      ("End",  xmScaleWidgetClass, col,
	       XmNorientation, XmHORIZONTAL,
	       XmNminimum, MINDEPTH,  XmNmaximum, MAXDEPTH,
	       XmNshowValue, True,
	       XmNvalue, imodvDepthCueData.fend,    
	       XmNscaleMultiple, 1,
	       XtVaTypedArg, XmNtitleString, XmRString, 
	       "End", 4,
	       NULL);
	  XtAddCallback(imodvDepthCueData.wEnd,
			XmNvalueChangedCallback,
			far_cb, (XtPointer)a);

     }
     XtManageChild(col);
     XtManageChild(frame);
     return(frame);
}     


