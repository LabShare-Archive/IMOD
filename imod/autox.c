/*  IMOD VERSION 2.42
 *
 *  autox.c --  Automatic contour generation for imod.
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
    Revision 3.1  2001/12/17 18:41:51  mast
    Add calls for smooth and next section to be done from hotkeys

*/
#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/Label.h>
#include <Xm/ToggleB.h>
#include <Xm/PushB.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <mrcc.h>
#include <string.h>
#include "diaP.h"
#include "imod.h"

/* The current section number and data that is being contoured. */
static int autox_cz = -1;
static unsigned char *autoImage = NULL;

/*
 * Put up a help dialog for the auto contour window.
 */
void autox_help_cb(Widget w, XtPointer client, XtPointer call)
{
     dia_vasmsg
	  ("Auto Contour Help\n",
	   "-----------------\n",
	   "Auto contour creates closed contours ",
	   "that belong to the current object.\n",
	   "\n",
	   "Threshold - controls the grey level ",
	   "that segments the data.\n",
	   "\n",
	   "Resolution - controls point reduction in the ",
	   "generated contour.  Points are originally created at "
	   "intervals of about 1 pixel.  Points are removed if they are "
	   "within this distance of a line segment between the remaining "
	   "points.\n"
	   "\n",
	   "Regular or High Contrast - Select contrast: "
	   "high contrast shows the threshold level, "
	   "regular contrast shows the image data.\n",
	   "\n",
	   "Alt mouse - Use alternate mouse functions:\n"
	   "\tLeft   - Select area by flooding around point.\n",
	   "\tMiddle - Add pixel to flood area (it will appear as yellow).\n",
	   "\tRight  - Delete pixel from flood area (it will appear as dark)"
	   ".\n",
	   "Middle and right buttons may be held down while moving the mouse "
	   "to add or delete a series of pixels.\n"
	   "\n"
	   "Follow diagonals - when flooding an area, add pixels that are "
	   "touching pixels already in the area only on their corners.\n",
	   "\n"
	   "Control Buttons:\n"
	   "Smooth - Smooth a flooded area (Expand followed by Shrink).\n"
	   "Next   - Flood fill next section.\n"
	   "Build  - Build contour around flooded area.\n"
	   "Fill   - Flood fill area at current point.\n"
	   "Clear  - Clear a flooded area.\n"
	   "Done   - Exit auto contour.\n"
	   "Shrink - Shrink a flooded area by one pixel.\n"
	   "Expand - Expand a flooded area by one pixel.\n"
	   "Help   - Gives you this help window.\n",
	   NULL);
}

void autox_threshold_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     Autox *ax = (Autox *)client;

     ax->threshold = cbs->value;

     if (ax->contrast){
#ifdef DRAW_GL
	  cmap_new_levels(ax->threshold, ax->threshold + 1);
#else
	  xcramp_setlevels(ax->vw->cramp, ax->threshold, ax->threshold + 1);
#endif
	  ax->vw->black = ax->threshold;
	  ax->vw->white = ax->threshold + 1;
     }
     imod_info_setbw(ax->vw->black, ax->vw->white);
}

void autox_res_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     Autox *ax = (Autox *)client;

     ax->shave = cbs->value * 0.01;
}
	  
void autox_regc_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     Autox *ax = (Autox *)client;
     
     ax->contrast = FALSE;

#ifdef DRAW_GL
     cmap_index(0,  &(ax->vw->black), &(ax->vw->white));
     adjustcmap_pf( &(ax->vw->black), &(ax->vw->white), Rampbase);
#else
     xcrampSelectIndex(ax->vw->cramp, 0);
     xcramp_ramp(ax->vw->cramp);
     xcramp_getlevels(ax->vw->cramp, &(ax->vw->black), &(ax->vw->white));
#endif

     imod_info_setbw(ax->vw->black, ax->vw->white);
}

void autox_highc_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     Autox *ax = (Autox *)client;

     ax->contrast = TRUE;
#ifdef DRAW_GL
     cmap_index(1, &(ax->vw->black), &(ax->vw->white));
     cmap_new_levels(ax->threshold, ax->threshold + 1);
#else
     xcrampSelectIndex(ax->vw->cramp, 1);
     xcramp_setlevels(ax->vw->cramp, ax->threshold, ax->threshold + 1);
     xcramp_ramp(ax->vw->cramp);
#endif

     ax->vw->black = ax->threshold;
     ax->vw->white = ax->threshold + 1;
     
     imod_info_setbw(ax->vw->black, ax->vw->white);
}

void autox_altmouse_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     Autox *ax = (Autox *)client;
     
     ax->altmouse = cbs->set;
}

void autox_diagonal_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     Autox *ax = (Autox *)client;
     
     ax->diagonal = cbs->set;
}

void autox_quit_cb(Widget w, XtPointer client, XtPointer call)
{
     Autox **pax = (Autox **)client;
     Autox  *ax;
     ax = *pax;

#ifdef DRAW_GL
     cmap_index(0,  &(ax->vw->black), &(ax->vw->white));
     adjustcmap_pf( &(ax->vw->black), &(ax->vw->white), Rampbase);
     cmap_get_levels(&(ax->vw->black), &(ax->vw->white));
#else
     xcrampSelectIndex(ax->vw->cramp, 0);
     xcramp_ramp(ax->vw->cramp);
     xcramp_getlevels(ax->vw->cramp, &(ax->vw->black), &(ax->vw->white));
#endif

     ax->vw->imod->thresh = ax->threshold;
     autox_clear(ax, AUTOX_ALL);
     imod_info_setbw(ax->vw->black, ax->vw->white);

     if (ax->xlist)
	  free(ax->xlist);
     if (ax->ylist)
	  free(ax->ylist);

     imodDraw(ax->vw, IMOD_DRAW_IMAGE);
     XtPopdown(ax->dialog);
     XtDestroyWidget(ax->dialog);
     if (ax->data)
	  free(ax->data);
     free(*pax);
     *pax = NULL;
}


void autox_fill_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodView *vw = (ImodView *)client;

     if (vw->ax->cz == vw->zmouse)
	  autox_clear(vw->ax, AUTOX_FILL);
     else
	  autox_clear(vw->ax, AUTOX_ALL);
     autox_flood(vw->ax);
     auto_patch(vw->ax, vw->xsize, vw->ysize);
     vw->ax->filled = TRUE;
     vw->ax->cz = (int)(vw->zmouse + 0.5);
     imodDraw(vw, IMOD_DRAW_IMAGE);
}

/* DNM 1/25/01: recoded to use new function that finds and returns array of
   contours from the defined points */
void autox_build_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodView *vw = (ImodView *)client;
     Icont *cont;
     Icont *newconts;
     int pt, ncont, i;
     float dist, ndist, ldist;

     if (!vw->ax->filled)
	  return;

     /* First have to turn off any pixels that are marked as black */
     for(i = 0; i < vw->xysize; i++)
	  if (vw->ax->data[i] & AUTOX_BLACK)
	       vw->ax->data[i] &= ~AUTOX_FLOOD;

     /*  get contours from the edges */
     newconts = imodContoursFromImagePoints(vw->ax->data, vw->xsize, vw->ysize,
					   vw->ax->cz, 
					    AUTOX_FLOOD | AUTOX_WHITE, 
					    vw->ax->diagonal, &ncont);

     /* DNM: return if no points found */
     if (!newconts)
	  return;

     /* add each contour to the model */
     for (i = 0; i < ncont; i++) {
	  NewContour( Model );
	  cont = imodContourGet(Model);
	  imodContourCopy(&newconts[i], cont);

	  /* DNM: switch to ContourReduce method, but keep the Strip call as a
	     quick pre-filter - it eliminates points ON the lines */
	  imodContourStrip(cont);
	  if (vw->ax->shave > 0.0) 
	       imodContourReduce(cont, vw->ax->shave);  
     }
     free (newconts);

     /* DNM: better make the clear unconditional, or it will keep adding
	the same points over and over */
     autox_clear(vw->ax, AUTOX_ALL);
     vw->ax->filled = FALSE;
     imodDraw(vw, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
}

void autox_clear_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodView *vw = (ImodView *)client;
     autox_clear(vw->ax, AUTOX_ALL);
     imodDraw(vw, IMOD_DRAW_IMAGE);
}  

void autox_shrink_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodView *vw = (ImodView *)client;
     autox_shrink(vw->ax);
     imodDraw(vw, IMOD_DRAW_IMAGE);
}
void autox_expand_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodView *vw = (ImodView *)client;
     autox_expand(vw->ax);
     auto_patch(vw->ax, vw->xsize, vw->ysize);
     imodDraw(vw, IMOD_DRAW_IMAGE);
}

void autox_smooth_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodView *vw = (ImodView *)client;
     autox_expand(vw->ax);
     autox_shrink(vw->ax);
     auto_patch(vw->ax, vw->xsize, vw->ysize);
     imodDraw(vw, IMOD_DRAW_IMAGE);
}

/* Go to next section. */
void autox_next_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodView *vw = (ImodView *)client;
     if (!vw->insertmode){
	  if (vw->zmouse < (vw->zsize - 1)){
	       vw->zmouse++;
	  }
     }else{
	  if (vw->zmouse > 0){
	       vw->zmouse--;
	  }
     }

     /* Find the new X and Y coords. */
     {
	  int i, j;
	  int mi = vw->xsize;
	  int mj = vw->ysize;
	  float ci = 0.0f, cj = 0.0f, cp = 0.0f;
	  unsigned char *data = vw->ax->data;

	  for(j = 0; j < mj; j++)
	       for(i = 0; i < mi; i++){
		    if (data[i + (j*mi)] & AUTOX_FILL){
			 ci += i;
			 cj += j;
			 cp += 1.0f;
		    }
	       }
	  if (cp){
	       vw->xmouse = ci/cp;
	       vw->ymouse = cj/cp;
	  }else{
	       Ipoint cm;
	       Icont *cont = imodContourGet(vw->imod);
	       if (cont){
		    imodContourCenterOfMass(cont, &cm);
		    vw->xmouse = cm.x;
		    vw->ymouse = cm.y;
	       }
	  }
     }

     autox_fill_cb(w, client, call);
     imodDraw(vw, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ);
}

/* Allocate the array given the current image size */
static int allocate_arrays(ImodView *vw, Autox *ax)
{
     /* data used to store segmentation data. */
     ax->data = (unsigned char *)malloc(vw->xysize * sizeof(unsigned char));
     ax->xysize = vw->xysize;

     /* list of pixels to check: make it twice perimeter size */
     ax->listsize = 4 * (vw->xsize + vw->ysize);
     ax->xlist = (int *)malloc(ax->listsize * sizeof(int));
     ax->ylist = (int *)malloc(ax->listsize * sizeof(int));
     if (!ax->data || !ax->xlist || !ax->ylist){
	  if (ax->data)
	       free(ax->data);
	  if (ax->xlist)
	       free(ax->xlist);
	  if (ax->ylist)
	       free(ax->ylist);
	  return(-1);
     }
     return (0);
}

/* If image is flipped, call this - it dismisses existing memory and gets new
   memory; quits window if failed */
void autox_newsize(ImodView *vw)
{
     Autox *ax = vw->ax;

     if (!ax)
	  return;
     if (ax->data)
	  free(ax->data);
     if (ax->xlist)
	  free(ax->xlist);
     if (ax->ylist)
	  free(ax->ylist);
     if (allocate_arrays(vw, ax)) {
	  ax->data = NULL;
	  ax->xlist = NULL;
	  ax->ylist = NULL;
	  wprint("\aAutoContour failed to get new image memory.");
	  autox_quit_cb(NULL, &(vw->ax), NULL);
     } else
	  autox_clear(ax, AUTOX_ALL);
     ax->filled    = FALSE;
}


int autox_open(ImodView *vw)
{
     Widget row, col, pushb, slider, radio, toggle;
     Autox *ax = vw->ax;
     Atom wmclose;

     if (vw->fakeImage) {
	  wprint("\aAutoContour cannot be used with no image.");
	  return(-1);
     }

     if (ax){
	  XRaiseWindow(App->display, XtWindow(ax->dialog));
	  return(-1);
     }

     ax = (Autox *)malloc(sizeof(Autox));
     if (!ax){
	  wprint("AutoContour Open failed: No memory.");
	  return(-1);
     }

     ax->threshold = vw->imod->thresh;
     ax->contrast  = FALSE;
     ax->reverse   = FALSE;
     ax->shave     = 0.25;
     ax->vw        = vw;
     ax->filled    = FALSE;
     ax->altmouse  = FALSE;
     ax->cz        = (int)(vw->zmouse + 0.5);
     ax->diagonal  = FALSE;

     ax->dialog = XtVaCreatePopupShell
	  ("AutoContour", topLevelShellWidgetClass, App->toplevel,
	   XtDisplay(Dia_toplevel),
	   XtVaTypedArg, XmNtitleString, XmRString,
	   "Imod Auto", 10,
	   XmNvisual, App->visual,
	   NULL);
     
     if (!ax->dialog){
	  free(ax);
	  wprint("AutoContour Open failed: No window available.");
	  return(-1);
     }

     if (allocate_arrays(vw, ax)) {
	  free(ax);
	  wprint("AutoContour Open failed: No memory.");
	  return(-1);
     }


     autox_clear(ax, AUTOX_ALL);
     row = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, ax->dialog, NULL);

     slider = XtVaCreateManagedWidget
	  ("Threshold", xmScaleWidgetClass, row,
	   XmNshowValue, True,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 254,
	   XmNvalue, ax->threshold,
	   XmNscaleMultiple, 1,
	   XtVaTypedArg, XmNtitleString, XmRString, "Threshold", 10,
	   NULL);
     XtAddCallback(slider, XmNvalueChangedCallback, autox_threshold_cb, ax);
     XtAddCallback(slider, XmNdragCallback, autox_threshold_cb, ax);

     slider = XtVaCreateManagedWidget
	  ("Pixel Resolution", xmScaleWidgetClass, row,
	   XmNshowValue, True,
	   XmNorientation, XmHORIZONTAL,
	   XmNminimum, 0,
	   XmNmaximum, 200,
	   XmNdecimalPoints, 2,
	   XmNvalue, (int)(ax->shave * 100),
	   XmNscaleMultiple, 1,
	   XtVaTypedArg, XmNtitleString, XmRString, "Resolution", 11,
	   NULL);
     XtAddCallback(slider, XmNvalueChangedCallback, autox_res_cb, ax);
     
     radio = XmCreateRadioBox(row, "radio", NULL, 0);
     ax->regc = XtVaCreateManagedWidget
	  ("Regular Contrast", xmToggleButtonWidgetClass, radio, NULL);
     XtAddCallback(ax->regc, XmNvalueChangedCallback,
		   autox_regc_cb, (XtPointer)ax);
     ax->highc = XtVaCreateManagedWidget
	  ("High Contrast", xmToggleButtonWidgetClass, radio, NULL);
     XtAddCallback(ax->highc, XmNvalueChangedCallback,
		   autox_highc_cb, (XtPointer)ax);
     XmToggleButtonSetState(ax->regc, True, False);
     XmToggleButtonSetState(ax->highc, False, False);
     XtManageChild(radio);
     
     toggle = XtVaCreateManagedWidget
	  ("Alt mouse", xmToggleButtonWidgetClass, row, 
	   XmNindicatorOn, True,
	   XmNindicatorType, XmN_OF_MANY,
	   XmNset, False,
	   NULL);
     XtAddCallback(toggle, XmNvalueChangedCallback,
		   autox_altmouse_cb, (XtPointer)ax);

     toggle = XtVaCreateManagedWidget
	  ("Follow diagonals", xmToggleButtonWidgetClass, row, 
	   XmNindicatorOn, True,
	   XmNindicatorType, XmN_OF_MANY,
	   XmNset, False,
	   NULL);
     XtAddCallback(toggle, XmNvalueChangedCallback,
		   autox_diagonal_cb, (XtPointer)ax);

     col = XtVaCreateManagedWidget
	  ("rowcol", xmRowColumnWidgetClass, row,
	   XmNpacking, XmPACK_COLUMN,
	   XmNnumColumns, 3,
	   XmNorientation, XmVERTICAL,
	   NULL);


     pushb = XtVaCreateManagedWidget
	  ("Smooth", xmPushButtonWidgetClass, col, NULL);
     XtAddCallback(pushb, XmNactivateCallback, autox_smooth_cb, vw);
     pushb = XtVaCreateManagedWidget
	  ("Next", xmPushButtonWidgetClass, col, NULL);
     XtAddCallback(pushb, XmNactivateCallback, autox_next_cb, vw);
     pushb = XtVaCreateManagedWidget
	  ("Build", xmPushButtonWidgetClass, col, NULL);
     XtAddCallback(pushb, XmNactivateCallback, autox_build_cb, vw);     


     pushb = XtVaCreateManagedWidget
	  ("Fill ", xmPushButtonWidgetClass, col, NULL);
     XtAddCallback(pushb, XmNactivateCallback, autox_fill_cb,  vw);
     pushb = XtVaCreateManagedWidget
	  ("Clear", xmPushButtonWidgetClass, col, NULL);
     XtAddCallback(pushb, XmNactivateCallback, autox_clear_cb, vw);
     pushb = XtVaCreateManagedWidget
	  ("Done ", xmPushButtonWidgetClass, col, NULL);
     XtAddCallback(pushb, XmNactivateCallback, autox_quit_cb, &(vw->ax));


     pushb = XtVaCreateManagedWidget
	  ("Shrink", xmPushButtonWidgetClass, col, NULL);
     XtAddCallback(pushb, XmNactivateCallback, autox_shrink_cb, vw);
     pushb = XtVaCreateManagedWidget
	  ("Expand", xmPushButtonWidgetClass, col, NULL);
     XtAddCallback(pushb, XmNactivateCallback, autox_expand_cb, vw);

     pushb = XtVaCreateManagedWidget
	  ("Help ", xmPushButtonWidgetClass, col, NULL);
     XtAddCallback(pushb, XmNactivateCallback, autox_help_cb, NULL);

#ifdef DRAW_GL
     glxDummy(ax->dialog);
#endif

     wmclose = XmInternAtom( XtDisplay(ax->dialog),
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(ax->dialog, wmclose, autox_quit_cb,
			     (caddr_t)&(vw->ax));
     XtPopup(ax->dialog, XtGrabNone);
     vw->ax = ax;
     return(0);
}

int autox_setlow(struct ViewInfo *vw, int x, int y)
{
     /* DNM 2/1/01: test for within bounds */
     if (x < 0 || y < 0 || x >= vw->xsize || y >= vw->ysize)
	  return 1;
     vw->ax->data[x + (y * vw->xsize)] &= ~AUTOX_WHITE;
     vw->ax->data[x + (y * vw->xsize)] |= AUTOX_BLACK;
     vw->ax->filled = TRUE;
     imodDraw(vw, IMOD_DRAW_IMAGE);
     return(0);
}

int autox_sethigh(struct ViewInfo *vw, int x, int y)
{
     /* DNM 2/1/01: test for within bounds */
     if (x < 0 || y < 0 || x >= vw->xsize || y >= vw->ysize)
	  return 1;
     vw->ax->data[x + (y * vw->xsize)] &= ~AUTOX_BLACK;
     vw->ax->data[x + (y * vw->xsize)] |= AUTOX_WHITE;
     vw->ax->filled = TRUE;
     imodDraw(vw, IMOD_DRAW_IMAGE);
     return(0);
}

int autox_fillmouse(struct ViewInfo *vw, int xm, int ym)
{
     vw->xmouse = xm;
     vw->ymouse = ym;
     autox_fill_cb(NULL, (XtPointer)vw, NULL);
     return(0);
}

int autox_build(Autox *ax)
{
     if (!ax)
	  return(-1);
     autox_build_cb(NULL,(XtPointer)ax->vw, NULL);
     return(0);
}

int autox_next(Autox *ax)
{
     if (!ax)
	  return(-1);
     autox_next_cb(NULL,(XtPointer)ax->vw, NULL);
     return(0);
}

int autox_smooth(Autox *ax)
{
     if (!ax)
	  return(-1);
     autox_smooth_cb(NULL,(XtPointer)ax->vw, NULL);
     return(0);
}

/* DNM 1/17/01: get_contour_edge_points was replaced by call to 
   imodGetImagePoints in library, after adding argument for the flood mask */


/*****************************************************************************
 * FUNCTION: autox_flood
 * Floods an area around the current point
 *
 * RETURNS: 0 if nothing was filled.
 *          1 if somthing was filled.
 *****************************************************************************/
int autox_flood(Autox *ax)
{
     int threshold;
     int x, y;
     int cz = (int)(ax->vw->zmouse + 0.5f);
     unsigned char *data = ax->data;
     int *xlist = ax->xlist;
     int *ylist = ax->ylist;
     int diagonal = ax->diagonal;
     int ringnext = 0;
     int ringfree = 1;
     int pixind;
     int xsize = ax->vw->xsize;
     int ysize = ax->vw->ysize;
     unsigned char neighflag;
     int test;
     
     autoImage = ivwGetCurrentSection(ax->vw);
     if (!autoImage) return(0);

     x = (int)ax->vw->xmouse;
     y = (int)ax->vw->ymouse;

     /* DNM: incrementing threshold by 1, then testing for less than threshold
	or >= threshold, makes areas match what shows up as black and white */
     threshold = ax->threshold + 1;

     /* DNM: exclude rgba visual */
     if (App->depth == 8 && !App->rgba){
	  threshold = (int)((((float)ax->vw->rampsize/256.0f)
			    * threshold) + ax->vw->rampbase);
     }

     if (autoImage[x + (y * ax->vw->xsize)] < threshold)
	  ax->reverse = TRUE;
     else
	  ax->reverse = FALSE;


     autox_cz = (int)(ax->vw->zmouse + 0.5f);

     /* initialize the ring buffer */
     xlist[0] = x;
     ylist[0] = y;
     data[x + y * xsize] |= AUTOX_CHECK;
     neighflag  = AUTOX_FLOOD | AUTOX_CHECK;

     while (ringnext != ringfree) {

	  /* check next point on list */
	  x = xlist[ringnext];
	  y = ylist[ringnext];
	  pixind = x + y * xsize;
	  if (ax->reverse)
	       test = (autoImage[pixind] < threshold) ||
		    (data[pixind] & AUTOX_BLACK);
	  else
	       test = ((autoImage[pixind] >= threshold) ||
		       (data[pixind] & AUTOX_WHITE)) &&
		    (~data[pixind] & AUTOX_BLACK);
	  if (test) {

	       /* If point passes test, mark as flood */ 
	       data[pixind] |= AUTOX_FLOOD;

	       /* add each of four neighbors on list if coordinate is legal
		  and they are not already on list or in flood */
	       if (x > 0 && !(data[pixind - 1] & neighflag)) {
		    xlist[ringfree] = x - 1;
		    ylist[ringfree++] = y;
		    ringfree %= ax->listsize;
		    data[pixind - 1] |= AUTOX_CHECK;
	       }
	       if (x < xsize - 1 && !(data[pixind + 1] & neighflag)) {
		    xlist[ringfree] = x + 1;
		    ylist[ringfree++] = y;
		    ringfree %= ax->listsize;
		    data[pixind + 1] |= AUTOX_CHECK;
	       }
	       if (y > 0 && !(data[pixind - xsize] & neighflag)) {
		    xlist[ringfree] = x;
		    ylist[ringfree++] = y - 1;
		    ringfree %= ax->listsize;
		    data[pixind - xsize] |= AUTOX_CHECK;
	       }
	       if (y < ysize - 1 && !(data[pixind + xsize] & neighflag)) {
		    xlist[ringfree] = x;
		    ylist[ringfree++] = y + 1;
		    ringfree %= ax->listsize;
		    data[pixind + xsize] |= AUTOX_CHECK;
	       }

	       if (diagonal) {
		    if (x > 0 && y > 0 && 
			!(data[pixind - 1 - xsize] & neighflag)) {
			 xlist[ringfree] = x - 1;
			 ylist[ringfree++] = y - 1;
			 ringfree %= ax->listsize;
			 data[pixind - 1 - xsize] |= AUTOX_CHECK;
		    }
		    if (x < xsize - 1 && y > 0 && 
			!(data[pixind + 1 - xsize] & neighflag)) {
			 xlist[ringfree] = x + 1;
			 ylist[ringfree++] = y - 1;
			 ringfree %= ax->listsize;
			 data[pixind + 1 - xsize] |= AUTOX_CHECK;
		    }
		    if (x > 0 && y < ysize - 1 && 
			!(data[pixind - 1 + xsize] & neighflag)) {
			 xlist[ringfree] = x - 1;
			 ylist[ringfree++] = y + 1;
			 ringfree %= ax->listsize;
			 data[pixind - 1 + xsize] |= AUTOX_CHECK;
		    }
		    if (x < xsize - 1 && y < ysize - 1 && 
			!(data[pixind + 1 + xsize] & neighflag)) {
			 xlist[ringfree] = x + 1;
			 ylist[ringfree++] = y + 1;
			 ringfree %= ax->listsize;
			 data[pixind + 1 + xsize] |= AUTOX_CHECK;
		    }
	       }
	  }

	  /* Take point off list, advance next pointer */
	  data[pixind] &= ~AUTOX_CHECK;
	  ringnext++;
	  ringnext %= ax->listsize;
     }

     return(1);
}

/* DNM: removed recursive functions for flood fill and patch fill */

/* auto_patch fills area outside the flood with the patch flag, then makes
   unmarked pixels be part of the flood */
void auto_patch(Autox *ax, int xsize, int ysize)
{
     unsigned char *data = ax->data;
     int i, x, y;
     int xysize;
     int xmax = -1;
     int xmin = xsize;
     int ymax = -1;
     int ymin = ysize;

     /* get min and max of flooded area */
     for (y = 0; y < ysize; y++)
	  for (x = 0; x < xsize; x++)
	       if (data[x + y * xsize] & AUTOX_FLOOD) {
		    if (x < xmin)
			 xmin = x;
		    if (x > xmax)
			 xmax = x;
		    if (y < ymin)
			 ymin = y;
		    if (y > ymax)
			 ymax = y;
	       }

     /* Start a patch from every point along the four sides, because there
	may be isolated patches */
     for(x = xmin ; x <= xmax; x++){
	  auto_patch_fill_outside(ax, xsize, xmin, xmax, ymin, ymax, x, ymin);
	  auto_patch_fill_outside(ax, xsize, xmin, xmax, ymin, ymax, x, ymax);
     }
     for (y = ymin ; y <= ymax; y++){
	  auto_patch_fill_outside(ax, xsize, xmin, xmax, ymin, ymax, xmin, y);
	  auto_patch_fill_outside(ax, xsize, xmin, xmax, ymin, ymax, xmax, y);
     }

     xysize = xsize * ysize;

     /* Mark everything now not in a patch as in the flood */
     for(y = ymin; y <= ymax; y++)
	  for(x = xmin; x <= xmax; x++){
	       i = x + y * xsize;
	       if (!(data[i] & (AUTOX_FLOOD | AUTOX_PATCH)))
		    data[i] |= AUTOX_FLOOD;
	  }
     
     /* Clear the patch flags */
     for ( i = 0; i < xysize; i++)
	  if (data[i] & AUTOX_PATCH)
	       data[i] &= ~AUTOX_PATCH;
}

/* To build a patch from a single point */
void auto_patch_fill_outside(Autox *ax, int xsize, int xmin, int xmax,
			     int ymin, int ymax, int x, int y)
{
     unsigned char *data = ax->data;
     int *xlist = ax->xlist;
     int *ylist = ax->ylist;
     int ringnext = 0;
     int ringfree = 1;
     int pixind;
     unsigned char neighflag;
 
     /* Don't even start if this point is a patch or a flood */
     pixind = x + y * xsize;
     if (data[pixind] & (AUTOX_FLOOD | AUTOX_PATCH))
	 return;

     /* initialize the ring buffer */
     xlist[0] = x;
     ylist[0] = y;
     data[pixind] |= (AUTOX_CHECK | AUTOX_PATCH);
     neighflag  = AUTOX_FLOOD | AUTOX_CHECK | AUTOX_PATCH;

     while (ringnext != ringfree) {

	  /* the next point on list got there by being neither patch nor
	     flood, so it needs no checking or marking */
	  x = xlist[ringnext];
	  y = ylist[ringnext];
	  pixind = x + y * xsize;

	  /* add each of four neighbors on list if coordinate is legal
	     and they are not already on list or in flood or patch. 
	     Mark each as on list and in patch */
	  if (x > xmin && !(data[pixind - 1] & neighflag)) {
	       xlist[ringfree] = x - 1;
	       ylist[ringfree++] = y;
	       ringfree %= ax->listsize;
	       data[pixind - 1] |= (AUTOX_CHECK | AUTOX_PATCH);
	  }
	  if (x < xmax && !(data[pixind + 1] & neighflag)) {
	       xlist[ringfree] = x + 1;
	       ylist[ringfree++] = y;
	       ringfree %= ax->listsize;
	       data[pixind + 1] |= (AUTOX_CHECK | AUTOX_PATCH);
	  }
	  if (y > ymin && !(data[pixind - xsize] & neighflag)) {
	       xlist[ringfree] = x;
	       ylist[ringfree++] = y - 1;
	       ringfree %= ax->listsize;
	       data[pixind - xsize] |= (AUTOX_CHECK | AUTOX_PATCH);
	  }
	  if (y < ymax && !(data[pixind + xsize] & neighflag)) {
	       xlist[ringfree] = x;
	       ylist[ringfree++] = y + 1;
	       ringfree %= ax->listsize;
	       data[pixind + xsize] |= (AUTOX_CHECK | AUTOX_PATCH);
	  }
	  
	  /* Take point off list, advance next pointer */
	  data[pixind] &= ~AUTOX_CHECK;
	  ringnext++;
	  ringnext %= ax->listsize;
     }
}

static int nay8(Autox *ax, int i, int j)
{
     int n, m, k = 0;
     int x, y;
     unsigned char *data = ax->data;
     int xsize = ax->vw->xsize;
     int ysize = ax->vw->ysize;

     if (!(data[i + (j * xsize)] & AUTOX_FLOOD))
	  return(0);
     
     for (n = -1; n <= 1; n++){
	  y = n + j;
	  for(m = -1; m <= 1 ; m++){
	       x = m + i;
	       if ((x >= 0) && (y >= 0) && (x < xsize) && (y < ysize))
		    if (data[x + (y * xsize)] & AUTOX_FLOOD)
			 k++; 
	  }
     }
     return(k-1);
}

void autox_shrink(Autox *ax)
{
     unsigned char *data = ax->data;
     int imax = ax->vw->xsize;
     int jmax = ax->vw->ysize;
     int i, j, m, n;
     int tval;
     int white = 0;
     
     /* DNM: tried testing on fill flag before checking neighbors and it
	didn't work. */
     for(j = 0; j < jmax; j++)
	  for(i = 0; i < imax; i++){
	       if (nay8(ax, i, j) < 7)
		    data[i + (j * imax)] |= AUTOX_CHECK;
	  }

     /* DNM: clear check flag after use, not before */
     for(j = 0; j < jmax; j++)
	  for(i = 0; i < imax; i++){
	       if (data[i + (j * imax)] & AUTOX_CHECK)
		    data[i + (j * imax)] &= ~(AUTOX_FLOOD | AUTOX_CHECK);
	  }

     return;

}

void autox_expand(Autox *ax)
{
     unsigned char *data = ax->data;
     int imax = ax->vw->xsize;
     int jmax = ax->vw->ysize;
     int i, j, m, n, x, y;
     
     for(j = 0; j < jmax; j++)
	  for(i = 0; i < imax; i++){
	       if (!(data[i + (j * imax)] & AUTOX_FILL)) continue;

	       for(m = -1; m <= 1; m++){
		    y = j + m;
		    if ((y < 0) || (y >= jmax)) continue;
		    for(n = -1; n <= 1; n++){
			 x = n + i;
			 if ((x == i) && (y == j)) continue;
			 if ((x < 0) || (x >= imax)) continue;
			 data[x + (y * imax)] |= AUTOX_CHECK;
		    }
	       }
	  }

     /* DNM: clear check flag in this loop, not before use */
     for(i = 0; i < ax->vw->xysize; i++)
	  if (data[i] & AUTOX_CHECK) {
		    data[i] |= AUTOX_FLOOD;
		    data[i] &= ~AUTOX_CHECK;
	  } 
     return;

}

void autox_clear(Autox *ax, unsigned char bit)
{
     int i;
     if (ax->data)
	  for(i = 0; i < ax->vw->xysize; i++)
	       ax->data[i] &= ~bit;
}

int auto_clear(unsigned char *data, int xsize, int ysize)
{
     int i;
     int xysize;

     xysize = xsize * ysize;

     if (data)
	  for(i = 0; i < xysize; i++)
	       data[i] = 0;
     return(0);
}
