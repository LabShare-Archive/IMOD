/*
 *  linegui.c -- Special plugin for line finding
 *
 */

/*****************************************************************************
 *   Copyright (C) 1997-2002 by Boulder Laboratory for 3-Dimensional Fine    *
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


/* include basic X-Window, Motif and OpenGL headers.
 */
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>

#include <Xm/Form.h>
#include <Xm/RowColumn.h>
#include <Xm/PushB.h>
#include <Xm/Text.h>
#include <Xm/Label.h>
#include <Xm/Separator.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <imodel.h>
#include "imodplug.h"

/*
 *  Define a structure to contain all local plugin data.
 */
typedef struct
{
     ImodView    *view;
     Widget       window;
     Widget       undo;

     Iobj  *obj;
     Icont *cont;
     Icont *undoCont, *tmpCont;
     int    ob, co, pt;
     int    csection;
     int    xsize, ysize, zsize;

     unsigned char *idata;
     int            idataSize;

     int   cmax;
     int   ksize;   /* Kernal size   */
     int   knum;    /* Kernal number */
     float sigma; 
     float h;
     int   ifdark;
     float stepsize;
     float redtol;
     int   ifreplace;
     float offset;
     int   closecont;
     int   copytol;
     int   docopy;
     int   copypool;
     int   copyfit;
     int   copiedco;
     int   copysize;

}PlugData;

#ifdef F77FUNCAP
#define linetrack_ LINETRACK
#define conttrack_ CONTTRACK
#endif
#define CONTOUR_POINT_MAX 1000
void linetrack_(unsigned char *image, int *nx, int *ny,
		float *points, int *csize, int *cpnt, int *cmax,
		int   *ksize,
		int   *knum,
		float *sigma,
		float *h,
		int   *ifdark,
		float *stepsize,
		float *redtol,
		int   *ifreplace,
		float *offset,
		int   *closecont,
		int   *iffail);

void conttrack_(unsigned char *image, int *nx, int *ny,
		float *points, int *csize, int *cpnt, float *p_copy, int *cmax,
		int   *ksize,
		int   *knum,
		float *sigma,
		float *h,
		int   *ifdark,
		float *stepsize,
		float *redtol,
		float *offset,
		int   *copytol,
		int   *copypool,
		int   *copyfit);

static void track_cb(Widget w, XtPointer client, XtPointer call);

static void undo_cb(Widget w, XtPointer client, XtPointer call);

static PlugData thisPlug = { 0, 0 };

static void makeWorkArea(Widget parent);

/*
 * Called by the imod plugin load function. 
 */
char *imodPlugInfo(int *type)
{
    if (type)
	*type = IMOD_PLUG_MENU + IMOD_PLUG_KEYS;
    return("Line Track");
}

static void help_cb(Widget w, XtPointer client, XtPointer call)
{
    dia_vasmsg("Line Track Plugin Help\n\n",
	       "Line Track will create points along a line between "
	       "the current selected model point and the previous "
	       "point.  It does this by filtering with a collection "
	       "of elongated kernels at a range of orientations.\n",
	       "\nThis Plugin also has a Contour Copying function which will "
	       "copy the current contour onto an adjacent section (the "
	       "section being displayed) then adjust the position of the "
	       "contour to match the image on that section.  It uses kernels "
	       "that are shaped like the contour.\n\n",
	       "Hot Keys: Space bar to add points between the last and the "
	       "current point\n",
	       "          Ctrl-Space to track from the current point "
	       "and close the contour\n",
	       "          Apostrophe to copy contour to current section\n",
	       "          u or Semicolon to undo last action\n",
	       "\nParameters:\n\n",
	       "0 Light, 1 Dark Lines: Set to 0 or 1 to follow bright "
	       "or dark lines.\n\n",
	       "Offset from line: Amount to offset model points from the "
	       "center of the line.  Set positive for a positive offset "
	       "when modeling to the right.\n\n"
	       "Sigma (Line width): Sigma is actually the half-width of "
	       "the central part of the kernel, but should be set to somewhat "
	       "more than half the width of the lines you are trying to "
	       "track.\n\n",
	       "H (half-length): The half-length of the kernel in its "
	       "elongated direction.\n\n",
	       "Kernel Half-size: Should be at least H and 3 * Sigma "
	       "but could be reduced to run faster.\n\n",
	       "Reduction Tolerance: After finding a path, the program will "
	       "remove any points that are within this distance of lines "
	       "between the remaining points.\n\n",
	       "Copy Tolerance: Maximum distance that the program will search "
	       "for best position when copying a contour to a new section.\n\n"
	       "Copy Pooling: Number of points that will be considered "
	       "together when trying to shift a particular point.  Bigger "
	       "numbers will give more smoothing.  With a positive value, the "
	       " point being shifted will be given the most weight; with a "
	       "negative value, all points will be given equal weight, "
	       "resulting in even more smoothing.\n\n"
	       "Copy Smoothing: Number of points to fit to in the final "
	       "smoothing of the contour (0 for no smoothing, higher values "
	       "for more smoothing).\n\n"
	       "Number of Kernels: Could be reduced to run faster.\n\n",
	       "Step Size: Size of step to move between points; could be "
	       "reduced to run faster.\n",
	       NULL);
}

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     PlugData *plug = (PlugData *)client;

     plug->view = NULL;
     XtPopdown(plug->window);
     XtDestroyWidget(plug->window);
     plug->window = 0;
     if (plug->undoCont)
	 imodContourDelete(plug->undoCont);
     if (plug->idata)
	 free(plug->idata);
}

/*
 *  Grab hotkey input. return 1 if we handle the key.
 */
int imodPlugKeys(ImodView *vw, XKeyEvent *event)
{
    PlugData *plug = &thisPlug;
    KeySym keysym;
    int    keyhandled = 0;
    int    ctrl;
    int    shift;

    /*
     * Don't grab keys if plug window isn't open.
     */
    if (!plug->view)
	return keyhandled;
    
    /* The keysym values are XK_a...XK_z and 
     * XK_space, XK_comma
     */
    keysym = XLookupKeysym(event, 0);

    /*
     * Modifier key mask.  Set ctrl and shift to true
     * if the coresponding key is pressed.
     */
    ctrl   = event->state & ControlMask;
    shift  = event->state & ShiftMask;
    
    
    switch(keysym){
      case XK_apostrophe: 

        plug->docopy = plug->copytol;
	
      case XK_space:
        if(ctrl) plug->closecont = 1;
	track_cb(NULL, NULL, NULL);
	keyhandled = 1;
	break;
      case XK_semicolon: 
      case XK_u:
	undo_cb(NULL, NULL, NULL);
	keyhandled = 1;
	break;
    }
    return keyhandled;
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
	 wprint("Line Track already open.\n");
	 return;
     }

     plug->view = inImodView;
     ivwGetImageSize(inImodView, &plug->xsize, &plug->ysize, &plug->zsize);

     /* 
      * Bring the window to the front if already open.
      */
     if (plug->window) XtPopup(plug->window, XtGrabNone);

     /* 
      * Initialize data. 
      */
     plug->obj       = NULL;
     plug->cont      = NULL;
     plug->undoCont  = NULL;
     plug->tmpCont   = imodContourNew();
     plug->idata     = NULL;
     plug->idataSize = 0;

     plug->cmax     = CONTOUR_POINT_MAX;
     plug->ksize    = 7;
     plug->knum     = 30;
     plug->sigma    = 2.5f;
     plug->h        = 7.0f;
     plug->ifdark   = 1;
     plug->stepsize = 3.0f;
     plug->redtol   = 0.3f;
     plug->ifreplace= 1;
     plug->offset   = 0.0f;
     plug->closecont= 0;
     plug->copytol  = 3;
     plug->docopy   = 0;
     plug->copypool = 5;
     plug->copyfit  = 5;
     plug->copiedco = -1;

     /*
      * This creates the plug window.
      */
     plug->window  = XtVaCreatePopupShell
	  ("Line Track", topLevelShellWidgetClass, imodTopLevel(),
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

static void track_cb(Widget w, XtPointer client, XtPointer call)
{
    unsigned char *image;
    int npoint, maxpoint, curpt, iffail, closecont;
    Ipoint *pts;
    int copytol, curx, cury, curz, i;
    float zdiff;
    Icont *tmpcont;
    float *p_copy;
    
    PlugData *plug = &thisPlug;
    Imod *theModel = ivwGetModel(plug->view);
    ivwGetImageSize(plug->view, &plug->xsize, &plug->ysize, &plug->zsize);

    closecont = plug->closecont;
    plug->closecont = 0;

    copytol = plug->docopy;
    plug->docopy = 0;
    if ((int)client > 1)
	 copytol = plug->copytol;

    plug->cont = imodContourGet(theModel);
    if (!plug->cont){
	wprint("Line Track Error:\n"
	       "  No Contour Selected.\n");
	return;
    }
    maxpoint = imodContourGetMaxPoint(plug->cont);
    if (maxpoint < 2){
	wprint("Line Track Error:\n"
	       "  Contour must have at least 2 points.\n");
	return;
    }
    image = ivwGetCurrentZSection(plug->view);
    if (!image){
	wprint("Line Track Error:\n"
	       "  No current image data.\n");
	return;
    }

    if (copytol != 0){
      ivwGetLocation(plug->view, &curx, &cury, &curz);
      zdiff = plug->cont->pts->z - curz;
      if (zdiff < 0) zdiff = -zdiff;
      if (zdiff > 1.01 || zdiff < 0.99) {
	wprint("Contour Copy Error:\n"
	       "  Copy must be to adjacent section\n");
	return;
      }
      if (maxpoint < 4){
	wprint("Contour Copy Error:\n"
	       "  Contour must have at least 4 points.\n");
	return;
      }
      tmpcont = imodContourDup(plug->cont);
      imodGetIndex(theModel, &plug->ob, &plug->copiedco, &plug->pt);
      imodNewContour(theModel);
      plug->cont = imodContourGet(theModel);
      imodContourCopy(tmpcont, plug->cont);
      free(tmpcont);
      pts = plug->cont->pts;
      for (i = 0; i < maxpoint; i++) pts++->z = curz;
      imodGetIndex(theModel, &plug->ob, &plug->co, &curpt);
      curpt = plug->pt;
    }

    /* Save contour for undo. */
    if (plug->undoCont){
	imodContourDelete(plug->undoCont);
    plug->undoCont = NULL;
    }

    if (copytol == 0) {
      plug->undoCont = imodContourDup(plug->cont);

      /* DNM 2/25/01: make flag for no copied contour be -1, not 0, since
	 0 could be a copied contour */
      plug->copiedco = -1;
      imodGetIndex(theModel, &plug->ob, &plug->co, &plug->pt);
      curpt = plug->pt;
    }

    XtSetSensitive(plug->undo, True);    

    npoint = maxpoint + CONTOUR_POINT_MAX;

    pts = (Ipoint *)malloc(npoint * sizeof(Ipoint));
    if (!pts){
	wprint("LineTrack memory allocation error!\n");
	return;
    }

    memcpy(pts, plug->cont->pts, sizeof(Ipoint) * maxpoint);
    free(plug->cont->pts);
    plug->cont->pts = pts;
    if(copytol == 0) {
      linetrack_(image, &plug->xsize, &plug->ysize,
		 (float *)pts, &maxpoint, &curpt , &npoint,
		 &plug->ksize,
		 &plug->knum,
		 &plug->sigma,
		 &plug->h,
		 &plug->ifdark,
		 &plug->stepsize,
		 &plug->redtol,
		 &plug->ifreplace,
		 &plug->offset,
		 &closecont,
		 &iffail);
      if(iffail != 0){
	wprint("LineTrack failed to find path\n");
	XBell(imodDisplay(), 100);
      }
    } else  {

      p_copy = (float *)malloc(2 * maxpoint * sizeof(Ipoint));
      if (!p_copy){
	   wprint("LineTrack memory allocation error!\n");
	   return;
      }

      conttrack_(image, &plug->xsize, &plug->ysize,
		 (float *)pts, &maxpoint, &curpt, p_copy, &npoint,
		 &plug->ksize,
		 &plug->knum,
		 &plug->sigma,
		 &plug->h,
		 &plug->ifdark,
		 &plug->stepsize,
		 &plug->redtol,
		 &plug->offset,
		 &copytol,
		 &plug->copypool,
		 &plug->copyfit);

      free(p_copy);
    }
    plug->cont->psize = maxpoint;
    plug -> copysize = maxpoint;
    imodSetIndex(theModel, plug->ob, plug->co, curpt);
    ivwDraw(plug->view, IMOD_DRAW_MOD);
    return;
}

static void undo_cb(Widget w, XtPointer client, XtPointer call)
{
    PlugData *plug = &thisPlug;
    Imod *theModel = ivwGetModel(plug->view);
    Icont *cont;
    int x, y, z;
    int ob, co, pt;


    imodGetIndex(theModel, &ob, &co, &pt);
    cont = imodContourGet(theModel);
    
    if (plug->copiedco < 0) {

	 if (!plug->undoCont) return;
    
	 if (cont != 0 && ob == plug->ob && co == plug->co){
	      imodContourCopy(cont, plug->tmpCont);
	      imodContourCopy(plug->undoCont, cont);
	      imodContourCopy(plug->tmpCont, plug->undoCont);
	      imodSetIndex(theModel, plug->ob, plug->co, plug->pt);
	      plug->ob = ob;
	      plug->co = co;
	      plug->pt = pt;
	      ivwDraw(plug->view, IMOD_DRAW_MOD);
	 }
    } else {
	 
	 if (cont != 0 && ob == plug->ob && co == plug->co){
	      if (cont->psize == plug->copysize) {
		   /* DNM 2/25/01: change from imodContourDelete to this */
		   imodDeleteContour(theModel);
		   imodSetIndex(theModel, plug->ob, plug->copiedco, plug->pt);
		   plug->copiedco = -1;
		   ivwDraw(plug->view, IMOD_DRAW_MOD);
	      }
	 }
    }
}

static void float_cb(Widget w, XtPointer client, XtPointer call)
{
    float *val = (float *)client;
    char  stval[32];
    float *mm, tval;
    char *st = XmTextGetString(w);
    tval = atof(st);
    XtVaGetValues(w, XmNuserData, &mm, NULL);
     if (tval > mm[1])
        tval = mm[1];
    if (tval < mm[0])
        tval = mm[0];
    *val = tval;

    XtFree(st);
    sprintf(stval, "%g", tval);
    XmTextSetString(w, stval);
}
static void int_cb(Widget w, XtPointer client, XtPointer call)
{
    char stval[32];
    int *val = (int *)client;
    int *mm;
    char *st = XmTextGetString(w);
    int tval = atoi(st);
    XtVaGetValues(w, XmNuserData, &mm, NULL);
    if (tval > mm[1])
	tval = mm[1];
    if (tval < mm[0])
	tval = mm[0];
    *val = tval;
    XtFree(st);
    sprintf(stval, "%d", tval);
    XmTextSetString(w, stval);
}
static void freemm_cb(Widget w, XtPointer client, XtPointer call)
{
    char *buf = (char *)client;
    if (buf) free(buf);
}

static Widget makeGUIint(Widget parent, char *label,
			 int *val, int min, int max)
{
    Widget lab, text;

    char tval[32];
    int *mm = malloc(sizeof(int) * 2);
    mm[0] = min; mm[1]=max;
    sprintf(tval, "%d", *val);


    lab = XtVaCreateManagedWidget
               (label, xmLabelWidgetClass, parent, NULL);

    text = XtVaCreateManagedWidget
	("Section", xmTextWidgetClass, parent,
	 XmNcolumns, 6,
	 XmNvalue, tval,
	 XmNuserData, mm,
	 NULL);

    XtAddCallback(text, XmNactivateCallback, int_cb, (XtPointer)val);
    XtAddCallback(text, XmNlosingFocusCallback, int_cb, (XtPointer)val);
    XtAddCallback(text, XmNdestroyCallback, freemm_cb, (XtPointer)mm);
    return text;
}

static Widget makeGUIfloat(Widget parent, char *label,
			   float *val, double min, double max)
{
    Widget lab, text;
    
    char tval[32];
    float *mm = malloc(sizeof(float) * 2);
    mm[0] = min; mm[1] = max;
    sprintf(tval, "%g", *val);

    lab = XtVaCreateManagedWidget
               (label, xmLabelWidgetClass, parent, NULL);

    text = XtVaCreateManagedWidget
	("Section", xmTextWidgetClass, parent,
	 XmNcolumns, 6,
	 XmNvalue, tval,
	 XmNuserData, mm,
	 NULL);

    XtAddCallback(text, XmNactivateCallback, float_cb, (XtPointer)val);
    XtAddCallback(text, XmNlosingFocusCallback, float_cb, (XtPointer)val);
    XtAddCallback(text, XmNdestroyCallback, freemm_cb, (XtPointer)mm);
    return text;
}

static void makeWorkArea(Widget parent)
     /* was type Widget but nothing is returned . . . */
{
    PlugData *plug = &thisPlug;

    Widget container;
    Widget rowcol;
    Widget button;

    
    container = XtVaCreateWidget
	("container", xmRowColumnWidgetClass, parent, NULL);
    rowcol = XtVaCreateWidget
	("container", xmRowColumnWidgetClass, container, 
	 XmNpacking, XmPACK_COLUMN,
	 XmNorientation, XmHORIZONTAL,
	 XmNisAligned, True,
	 XmNentryAlignment, XmALIGNMENT_END,
	 XmNnumColumns, 11,
	 NULL);

    makeGUIint(rowcol, "0 Light, 1 Dark Lines",
	       &plug->ifdark, 0, 1);
    makeGUIfloat(rowcol, "Offset from Line",
	       &plug->offset, -5.0, 5.0);
    makeGUIfloat(rowcol, "Sigma (Line width)",
	       &plug->sigma, 0.5, 7.0);
    makeGUIfloat(rowcol, "H (half-length)",
	       &plug->h, 1.0, 20.0);
    makeGUIint(rowcol, "Kernel Half-size",
	       &plug->ksize, 1, 10);
    makeGUIfloat(rowcol, "Reduction Tolerance",
	       &plug->redtol, 0.0, 5.0);
    makeGUIint(rowcol, "Copy Tolerance",
	       &plug->copytol, 0, 10);
    makeGUIint(rowcol, "Copy Pooling",
	       &plug->copypool, -5, 5);
    makeGUIint(rowcol, "Copy Smoothing",
	       &plug->copyfit, 0, 9);
    makeGUIint(rowcol, "Number of Kernels",
	       &plug->knum, 9, 40);
    makeGUIfloat(rowcol, "Step Size",
	       &plug->stepsize, 1., 20.0);
    XtManageChild(rowcol);
    XtVaCreateManagedWidget("separator", xmSeparatorWidgetClass, 
			    container, NULL);

    rowcol = XtVaCreateWidget
	("container", xmRowColumnWidgetClass, container, 
	 XmNpacking, XmPACK_COLUMN,
	 XmNorientation, XmHORIZONTAL,
	 XmNisAligned, True,
	 XmNentryAlignment, XmALIGNMENT_END,
	 XmNnumColumns, 1,
	 NULL);
    button = XtVaCreateManagedWidget
	("Track", xmPushButtonWidgetClass, rowcol, NULL);
    XtAddCallback(button, XmNactivateCallback, track_cb, (XtPointer)1);

    button = XtVaCreateManagedWidget
	("Copy", xmPushButtonWidgetClass, rowcol, NULL);
    XtAddCallback(button, XmNactivateCallback, track_cb, (XtPointer)2);

    plug->undo = XtVaCreateManagedWidget
	("Undo", xmPushButtonWidgetClass, rowcol, NULL);
    XtAddCallback(plug->undo, XmNactivateCallback, undo_cb, plug);
    XtSetSensitive(plug->undo, False);

    button = XtVaCreateManagedWidget
	("Close", xmPushButtonWidgetClass, rowcol, NULL);
    XtAddCallback(button, XmNactivateCallback, quit_cb, plug);

    button = XtVaCreateManagedWidget
	("Help", xmPushButtonWidgetClass, rowcol, NULL);
    XtAddCallback(button, XmNactivateCallback, help_cb, plug);

    XtManageChild(rowcol);
    XtManageChild(container);
}


