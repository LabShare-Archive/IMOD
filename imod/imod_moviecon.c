/*  IMOD VERSION 2.50
 *
 *  imod_moviecon.c -- Movie controller dialog
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <limits.h>
#ifndef NO_SYS_TIMES
#include <sys/times.h>
#ifdef FIXED_CLK_TCK
#define USE_CLK_TCK FIXED_CLK_TCK
#else
#define USE_CLK_TCK CLK_TCK
#endif
#endif

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Text.h>
#include <Xm/RowColumn.h>
#include <Xm/ArrowB.h>
#include <Xm/ArrowBG.h>
#include <Xm/Scale.h>
#include <Xm/PushB.h>
#include <Xm/Separator.h>
#include <dia.h>
#include "imod.h"

static int start[4], end[4], increment[4];
static int maxend[4];
static int firsttime = True;
static int axiscon;
static diaDialog *dia = NULL;
static Widget startscale, endscale, incscale, ratetext, actualrate;
static ImodView *view;
static float realrate, realint;
static int looponeway;
static int autosnap;
static int movieCount = 0;
static clock_t movieCurrent, movieStart, movieLast;

#define BASEINT 50.0
#define RATEFAC 1.25992
#define MINRATE 0.2
#define MAXRATE 200.

static Widget mkWorkArea(ImodView *vw, Widget top);
static void set_sliders(void);
static void fill_rate_box(void);
clock_t imodv_sys_time(void);

static void set_sliders()
{
     int newmin, newmax, turnon;
     XtVaSetValues(incscale, XmNvalue, increment[axiscon], NULL);

     newmax = maxend[axiscon];
     
     turnon = (newmax >= 2);
     if (!turnon)
          newmax = 2;
     XtVaSetValues(startscale, XmNvalue, start[axiscon] + 1,
		   XmNmaximum, newmax, NULL);
     newmin = 2;
     newmax = maxend[axiscon] + 1;
     if (!turnon) {
          newmin = newmax;
	  newmax++;
     }
     XtVaSetValues(endscale, XmNvalue, end[axiscon] + 1,
		   XmNmaximum, newmax,
		   XmNminimum, newmin,
		   NULL);
     XtSetSensitive(incscale, turnon);
     XtSetSensitive(startscale, turnon);
     XtSetSensitive(endscale, turnon);

}

void imcResetAll(ImodView *vw)
{
     int i;
     for (i = 0; i < 4; i++) {
          start[i] = 0;
	  increment[i] = 1;
     }
     end[0] = vw->xsize - 1;
     end[1] = vw->ysize - 1;
     end[2] = vw->zsize - 1;
     end[3] = vw->nt - 1;
     for (i = 0; i < 4; i++) {
          if (end[i] < 0) end[i] = 0;
	  maxend[i] = end[i];
     }
     looponeway = 0;
     autosnap = 0;
     firsttime = False;
     if (dia)
          set_sliders();
}

int imcGetIncrement(ImodView *vw, int xyzt)
{
     if (firsttime)
          imcResetAll(vw);
     return increment[xyzt];
}

int imcGetLoopMode(ImodView *vw)
{
     if (firsttime)
          imcResetAll(vw);
     return looponeway;
}

int imcGetSnapshot(ImodView *vw)
{
     if (firsttime)
          imcResetAll(vw);
     return autosnap;
}

void imcGetStartEnd(ImodView *vw, int xyzt, int *stout, int *endout)
{
     if (firsttime)
          imcResetAll(vw);
     *stout = start[xyzt];
     *endout = end[xyzt];
}

float imcGetInterval()
{
     return realint;
}

static void fill_rate_box()
{
     char vals[16];

     if (realrate < 10.0)
          sprintf(vals, "%5.2f", realrate);
     else
          sprintf(vals, "%5.1f", realrate);
     XtVaSetValues(ratetext, XmNvalue, vals, NULL);
}

void imcSetMovierate(ImodView *vw, int newrate)
{
     do {
          vw->movierate = newrate;
	  realint = BASEINT * pow(RATEFAC, (double)newrate);
	  realrate = 1000. / realint;
	  if (realrate > MAXRATE)
	       newrate++;
	  if (realrate < MINRATE)
	       newrate--;
     } while (realrate < MINRATE || realrate > MAXRATE);

     if (dia)
          fill_rate_box();
}

/* DNM 5/21/01: add an actual frame rate output to the box; the variant if
   there is no times function is pretty lame */

void imcStartTimer(void)
{
     movieCount = 0;
     movieStart = imodv_sys_time();
     movieCurrent = movieStart;
}

void imcReadTimer(void)
{
#ifdef NO_SYS_TIMES
     char persec[] = "CPU sec";
#else
     char persec[] = "sec";
#endif

     XmString xstring;
     char cstring[40];
     float elapsed, instrate;

     movieCount++;
     movieLast = movieCurrent;
     movieCurrent = imodv_sys_time();
     if (!dia || movieCurrent == movieLast)
	  return;

#ifdef NO_SYS_TIMES
     elapsed = (float)(movieCurrent - movieStart) / CLOCKS_PER_SEC;
     instrate = CLOCKS_PER_SEC / (float)(movieCurrent - movieLast);
#else
     elapsed = (float)(movieCurrent - movieStart) / USE_CLK_TCK;
     instrate = USE_CLK_TCK / (float)(movieCurrent - movieLast);
#endif
     sprintf(cstring, "Actual /%s: %5.2f (avg)  %5.2f", persec,
	     movieCount / elapsed, instrate);
     xstring = XmStringCreateSimple(cstring);
     XtVaSetValues(actualrate, XmNlabelString, xstring, NULL);
     XmStringFree(xstring);
}

static void help_cb()
{
     dia_vasmsg
	  ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
	   "Imod Movie Controller \n"
	   "~~~~~~~~~~~~~~~~~~~~~~~~"
	   "\n\n",
	   "This window allows you to control the starting and ending "
	   "coordinates within which a movie will run, the increment between "
	   "successive frames, and the frame rate.\n\n"
	   "Use the radio buttons to select whether the sliders will adjust "
	   "the start, end, and increment for X, Y, Z or Time.  The initial "
	   "default setting is for the sliders to adjust movies in Z.\n\n"
	   "The [Reset] button will reset the start, end, and increment to "
	   "their initial settings (the minimum and maximum coordinates and "
	   "an increment of 1) for all four axes.\n\n"
	   "The [Done] button will close the window and reset these values in "
	   "the same way.\n\n"
	   "The Rate text box will display the current frame rate as it is "
	   "changed by the down and up Arrows or by \",\" and \".\" and will "
	   "also allow you to enter a custom value.  You must type Enter to "
	   "have your value take effect.\n\n",
	   "The next line shows two actual frame rates, the first averaged "
	   "over the time since the current movie was started, the second "
	   "an instantaneous rate since the last frame was displayed.  The "
	   "actual rate will be less than the selected rate if the display is "
	   "too slow to keep up, and can also differ from the selected rate "
	   "due to lack of precision of the system clock.\n\n"
	   "If [Forward & Back] is selected, then movies will loop from one "
	   "end to the other then from that end back to the start; "
	   "if [One Way] is selected, then movies will always go in one "
	   "direction and jump back to the start when the end is reached.\n\n",
	   "If one of the Snapshot buttons [RGB] or [TIFF] is selected, "
	   "and a movie is started in a Zap window, then that Zap window "
	   "will automatically take an RGB or Tiff snapshot of each frame for "
	   "one complete cycle of the movie.  The movie will start at the "
	   "starting or the ending section depending on whether it is started "
	   "with the middle or the right mouse button.  It will stop when it "
	   "reaches the opposite end if [One Way] is selected; otherwise it "
	   "will loop back to the end at which it started.\nIf you stop the "
	   "movie before it is done with a mouse click in the same window, "
	   "then another mouse click in that or a different zap window will "
	   "start a completely new movie.  If you stop the movie by "
	   "clicking in a different window, then clicking again in a "
	   "different window will resume the interrupted movie in the "
	   "original window.\n",
	   NULL);
     return;
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     mkWorkArea(view, w);
     return;
}

static void done_cb(Widget w, XtPointer client, XtPointer call)
{
#ifdef __sun
     wprint("\a\nClosing this window crashes Imod on a Sun - so just make "
	    "it small for now.\n");
#else
     diaDestroyDialog(dia);

     dia = NULL;
     imcResetAll(view);
#endif
     return;
}

static void reset_cb(Widget w, XtPointer client, XtPointer call)
{
     imcResetAll(view);
}


void imodMovieConDialog(ImodView *vw)
{
     XtPointer cbd = (XtPointer)vw;
     
     if (dia){
	  XRaiseWindow(App->display, 
		       XtWindow(dia->dialog));
	  return;
     }

     view = vw;
     imcResetAll(view);

     dia = diaVaCreateDialog
	  ("Imod: Movie Controller", App->toplevel, App->context,
	   DiaNcontrolButton, "Done", done_cb,   cbd,
	   DiaNcontrolButton, "Reset", reset_cb, cbd,
	   DiaNcontrolButton, "Help", help_cb,   cbd,
	   DiaNworkAreaFunc,  workarea_cb,       cbd,
	   DiaNwindowQuit,    done_cb,           cbd,
	   0);
     return;
}

/****************************************************************************/
/*  Dialog controls.                                                 */

static void inc_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call; 
     increment[axiscon] = cbs->value;
}

static void start_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call; 
     start[axiscon] = cbs->value - 1;
     if (start[axiscon] >= end[axiscon] && start[axiscon] + 1 <=
	 maxend[axiscon]) {
          end[axiscon] = start[axiscon] + 1;
	  XtVaSetValues(endscale, XmNvalue, end[axiscon] + 1, NULL);
     }
}

static void end_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call; 
     end[axiscon] = cbs->value - 1;
     if (end[axiscon] <= start[axiscon]) {
          start[axiscon] = end[axiscon] - 1;
	  XtVaSetValues(startscale, XmNvalue, start[axiscon] + 1, NULL);
     }
}


static void axis_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;

     if (!cbs->set)
	  return;
     axiscon = (int)client;
     set_sliders();
}

static void loop_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;

     if (!cbs->set)
	  return;
     looponeway = (int)client;
}

static void snap_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;

     if (!cbs->set)
	  return;
     autosnap = (int)client;
}
static void ratetext_cb(Widget w, XtPointer client, XtPointer call)
{
     char *st = NULL;
     st = XmTextGetString(w);
     realrate = atof(st);
     XtFree(st);
     if (realrate <= 0) {
          imcSetMovierate(view, 0);
	  return;
     }

     if (realrate < MINRATE || realrate > MAXRATE) {
          if (realrate < MINRATE)
	       realrate = MINRATE;
          if (realrate > MAXRATE)
	       realrate = MAXRATE;
	  fill_rate_box();
     }

     realint = 1000./realrate;
     view->movierate = log((double)realint / BASEINT) / log(RATEFAC) + 0.5;
}
          
static void rate_arrow_cb(Widget w, XtPointer client, XtPointer call)
{
     imcSetMovierate(view, view->movierate + (int)client);
}

static Widget mkWorkArea(ImodView *vw, Widget top)
{
     XmString xsx, xsy, xsz, xst;
     Widget frame, row, col, radio, arrow;
     int val;

     axiscon = 2;

     frame = XtVaCreateWidget
	  ("frame", xmFrameWidgetClass, top, NULL);

     col = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, frame,
	   NULL);

     XtVaCreateManagedWidget
       ("Axis to Control", xmLabelWidgetClass, col, NULL);

     xsx = XmStringCreateSimple("X");
     xsy = XmStringCreateSimple("Y");
     xsz = XmStringCreateSimple("Z");
     xst = XmStringCreateSimple("Time");

     radio = XmVaCreateSimpleRadioBox (col, "radio_box",
				       2,  /* Initial choice */
				       axis_cb,
				       XmVaRADIOBUTTON, xsx, NULL, NULL, NULL,
				       XmVaRADIOBUTTON, xsy, NULL, NULL, NULL,
				       XmVaRADIOBUTTON, xsz, NULL, NULL, NULL,
				       XmVaRADIOBUTTON, xst, NULL, NULL, NULL,
				       XmNorientation, XmHORIZONTAL, NULL);
     XtManageChild (radio);
     XmStringFree(xsx);
     XmStringFree(xsy);
     XmStringFree(xsz);
     XmStringFree(xst);

     startscale = XtVaCreateManagedWidget
	  ("Start", xmScaleWidgetClass, col,
	   XmNshowValue, True,
	   XmNorientation, XmHORIZONTAL,
	   XmNscaleMultiple, 1,
	   XmNvalue, 1,
	   XmNminimum, 1,
	   /*	   XtVaTypedArg, XmNtitleString, XmRString, "Start", 6, */
	   NULL);
     XtAddCallback(startscale, XmNvalueChangedCallback, start_cb, 0);
     XtVaCreateManagedWidget("Start", xmLabelWidgetClass, col, NULL);
		 
     endscale = XtVaCreateManagedWidget
	  ("End", xmScaleWidgetClass, col,
	   XmNshowValue, True,
	   XmNorientation, XmHORIZONTAL,
	   XmNscaleMultiple, 1,
	   /*	   XtVaTypedArg, XmNtitleString, XmRString, "End", 4, */
	   NULL);
     XtAddCallback(endscale, XmNvalueChangedCallback, end_cb, 0);
     XtVaCreateManagedWidget("End", xmLabelWidgetClass, col, NULL);
		   
     incscale = XtVaCreateManagedWidget
	  ("Increment", xmScaleWidgetClass, col,
	   XmNshowValue, True,
	   XmNorientation, XmHORIZONTAL,
	   XmNscaleMultiple, 1,
	   XmNminimum, 1,
	   XmNmaximum, 20,
	   XmNvalue, 1,
	   /*	   XtVaTypedArg, XmNtitleString, XmRString, "Increment", 10, */
	   NULL);
     XtAddCallback(incscale, XmNvalueChangedCallback, inc_cb, 0);
     XtVaCreateManagedWidget("Increment", xmLabelWidgetClass, col, NULL);
		   
     set_sliders();

     row = XtVaCreateWidget
          ("rowcol", xmRowColumnWidgetClass, col,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     XtVaCreateManagedWidget
          ("Rate: ", xmLabelWidgetClass, row, NULL);
     arrow = XtVaCreateManagedWidget
       ("arrow_up", xmArrowButtonGadgetClass, row, 
	XmNarrowDirection, XmARROW_UP, NULL);
	  
     /*     imodOverrideTranslations(arrow, keytrans); */

     XtAddCallback(arrow, XmNarmCallback, 
		   rate_arrow_cb, (XtPointer)-1);

     arrow = XtVaCreateManagedWidget
       ("arrow_down", xmArrowButtonGadgetClass, row, 
	XmNarrowDirection, XmARROW_DOWN, NULL);
	  
     /*     imodOverrideTranslations(arrow, keytrans); */

     XtAddCallback(arrow, XmNarmCallback, 
		   rate_arrow_cb, (XtPointer)1);

     ratetext = XtVaCreateManagedWidget
	       ("Rate", xmTextWidgetClass, row, 
		XmNcolumns, 5,
		NULL);
     XtAddCallback(ratetext, XmNactivateCallback,
			ratetext_cb, 0);
     XtVaCreateManagedWidget
          ("frames/sec", xmLabelWidgetClass, row, NULL);
     fill_rate_box();

     XtManageChild(row);

     actualrate = XtVaCreateManagedWidget
          ("Actual: ", xmLabelWidgetClass, col, NULL);

     XtVaCreateManagedWidget("separator", xmSeparatorWidgetClass, col, 
			     XmNseparatorType, XmSINGLE_LINE,
			     XmNmargin, 20, NULL);

     xsx = XmStringCreateSimple("Forward & Back");
     xsy = XmStringCreateSimple("One Way");

     radio = XmVaCreateSimpleRadioBox (col, "radio_box",
				       0,  /* Initial choice */
				       loop_cb,
				       XmVaRADIOBUTTON, xsx, NULL, NULL, NULL,
				       XmVaRADIOBUTTON, xsy, NULL, NULL, NULL,
				       XmNorientation, XmHORIZONTAL, NULL);

     XtManageChild (radio);
     XmStringFree(xsx);
     XmStringFree(xsy);

     row = XtVaCreateWidget
          ("rowcol", xmRowColumnWidgetClass, col,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     XtVaCreateManagedWidget
          ("Snapshot:", xmLabelWidgetClass, row, NULL);
     xsx = XmStringCreateSimple("None");
     xsy = XmStringCreateSimple("RGB");
     xsz = XmStringCreateSimple("TIFF");

     radio = XmVaCreateSimpleRadioBox (row, "radio_box",
				       0,  /* Initial choice */
				       snap_cb,
				       XmVaRADIOBUTTON, xsx, NULL, NULL, NULL,
				       XmVaRADIOBUTTON, xsy, NULL, NULL, NULL,
				       XmVaRADIOBUTTON, xsz, NULL, NULL, NULL,
				       XmNorientation, XmHORIZONTAL, NULL);

     XtManageChild (radio);
     XmStringFree(xsx);
     XmStringFree(xsy);
     XmStringFree(xsz);
     XtManageChild(row);

     XtManageChild(col);
     XtManageChild(frame);
     return(frame);
}     
