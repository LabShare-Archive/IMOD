/*  IMOD VERSION 2.50
 *
 *  slicer.c -- Open the slicer window; Slice 3-D data at any angle.
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
    Revision 3.0  2001/11/29 18:10:49  rickg
    *** empty log message ***

    Revision 1.2  2001/11/23 05:32:30  mast
    Activated faster HQ display method for zoom > 1 using cubic interpolation
     - coded 2 years ago and left unused.  Also added workaround to Intel
    compiler bug.

*/
#include <X11/Intrinsic.h>
#include <Xm/MainW.h>
#include <Xm/DrawingA.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/ArrowB.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>
#include <Xm/RowColumn.h>
#include <Xm/Text.h>
#include <X11/keysym.h>
#include <Xm/VirtKeys.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <math.h>
#include <limits.h>
#include <string.h>

#include "diaP.h"
#include "imod.h"
#include "slicer.h"

/* Where oh where is XtResizeWidget */
void XtResizeWidget(Widget w, Dimension width, Dimension height,
		    Dimension border_width);

/* internal functions. */
static void sslice_ginit_cb(Widget w, XtPointer client, XtPointer call);
static void sslice_clear(struct Super_slicer *win);
static void input_cb(Widget w, XtPointer client, XtPointer call);
static void sslice_cube_draw(struct Super_slicer *win);
static void sslice_draw(struct Super_slicer *win);
Boolean sslice_update(XtPointer client_data);
static void sslice_tupdate(XtPointer client, XtIntervalId *id);
static void sslice_addworkproc(XtPointer client, XtIntervalId *id);
static void sslice_setxyz(struct Super_slicer *win, int x, int y);
static void slice_trans_step(struct Super_slicer *win);
static void slicer_insert_point(struct Super_slicer *win, int x, int y);
static void slicer_modify_point(struct Super_slicer *win, int x, int y);
static Widget createThickControls(struct Super_slicer *win, Widget parent);

static void slicerUpdateImage(struct Super_slicer *win);
static void drawThickControls(struct Super_slicer *win);
static void sslice_draw_model(struct Super_slicer *win);


static String KeyTranslations = "<KeyDown>: sliceKeyInput()\n";
static void slice_keyinput(XKeyEvent *event, struct Super_slicer *slice);

void sliceSetAnglesFromPoints(struct Super_slicer *ss,
			      Ipoint *p1, Ipoint *p2, int axis);

/* DNM: maximum angles for sliders */
static float maxAngle[3] = {90.0, 180.0, 180.0};

/*
 * Open up slicer help dialog.
 */
static void help_cb(Widget w, XtPointer client, XtPointer call)
{
     dia_vasmsg
	  ("Imod Slicer Help\n",
	   "-------------------------------------------------------------\n",
	   "\nThe Tool Bar\n\n",
	   "\tThe Up and Down Arrows step the zoom factor up or down.\n",
	   "\tThe show slice button will cause lines to be draw in the "
	   "XYZ and ZaP windows to show the intersection of the current "
	   "slice.\n",
	   "\tThe lock button will keep the image locked to the current "
	   "position.\n",
	   "\tThe checkerboard button toggles between fast rendering and "
	   "slower but higher quality image rendering.\n",

	   "\tAdditional controls can be selected using the option menu.\n",
	   "\tThe Slice option shows the position of the current slice and "
	   "shows sliders with which you can adjust the rotations about the "
	   "X, Y, and Z axes.  The rotation angles are applied to the volume, "
	   "not to the slicing plane, and they are applied in the order Z, Y, "
	   "X; thus these same angles can be used directly in other programs "
	   "that rotate the volume.  Clicking a slider with the left mouse "
	   "button will change the angle by 1 degree; clicking with the "
	   "middle button moves the slider immediately to the position of "
	   "mouse.  If you drag the slider, the representation of the slice "
	   "in the data volume will change continuously, but the image will "
	   "not be updated until you release the slider.\n",
	   "\tThe Depth option shows controls for adjusting the thickness of "
	   "the image slice and model display.  There is also an option menu "
	   "for controlling whether the volume will be displayed with Z "
	   "scaling.  If you choose \"Scale Before\", then the volume will be "
	   "scaled before it is rotated and sliced, and thus the rotation "
	   "angles will not select the same slice as with no Z-scaling.  If "
	   "you choose \"Scale After\", the volume is scaled after being "
	   "rotated, and the same slice is selected as with no Z-scaling.\n",
	   "\tThe Image option eliminates "
	   "the control panels and just shows the slice image.\n\n",
	   "Mouse Actions\n",
	   "--------\n",
	   "Left button: Pick a new current viewing point in the data volume\n"
	   "Middle button: In model mode, insert a point after the current "
	   "point\n"
	   "Right button: In model mode, move the current point to the "
	   "selected location\n\n",
	   "Hot Keys\n",
	   "--------\n",
	   "-/=\tDecrease/Increase zoom\n",
	   "_/+\tDecrease/Increase displayed image thickness\n",
	   "9/0\tDecrease/Increase displayed model thickness\n",
	   "s\tShow slice in ZaP and XYZ windows\n",
	   "S\tSnapshot to RGB file\n",
	   "Ctrl-S\tSnapshot to TIFF file\n",
	   "x/y/z\tAlign current and previous model points along X, Y or Z "
	   "axis\n",
	   "X/Y/Z\tAlign first and last points of contour along X, Y or Z "
	   "axis\n\n",
	   "Numeric Keypad:\n"
	   "4/6\t(Left/Right) Decrease/Increase last adjusted angle by "
	   "0.1 degree\n",
	   "2/8\t(Down/Up) Decrease/Increase last adjusted angle by "
	   "0.5 degree\n",
	   "0\t(Insert) Set last adjusted angle to 0\n",
	   NULL);
     return;
}


/*
 * Handle keybord input for slicer window.
 */
void sliceKeyInput(Widget w, XEvent *event, String par, Cardinal num)
{
     struct Super_slicer *slice;
     
     XtVaGetValues(w, XmNuserData, &slice, NULL);
     slice_keyinput((XKeyEvent *)event, slice);
     return;
}

/*
 * Toolbar zoom arrow callbacks.
 */
static void sslice_zoomup_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *win = (struct Super_slicer *)client;

     ivwControlPriority(win->vi, win->ctrl);
     win->zoom = b3dStepPixelZoom(win->zoom, 1);
     sslice_draw(win);
     return;
}
static void sslice_zoomdown_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *win = (struct Super_slicer *)client;

     ivwControlPriority(win->vi, win->ctrl);
     win->zoom = b3dStepPixelZoom(win->zoom, -1);
     /*     win->zoom -= 1.0;
     if (win->zoom < 1.0)
     win->zoom = 1.0; */
     /* sslice_clear(win); DNM: not needed */
     sslice_draw(win);
     return;
}


/* 
 * Show the location of the slice in the XYZ and Zap windows. 
 */
static void sslice_show_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *win = (struct Super_slicer *)client;

     slice_trans_step(win);
     sslice_showslice(win);
     return;
}


/*
 * Toolbar: Toggle between fast rendering and highres rendering.
 */
static void sslice_res_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *win = (struct Super_slicer *)client;

     ivwControlPriority(win->vi, win->ctrl);
     if (win->hq){
	  win->hq = FALSE;
	  XtVaSetValues(w, XmNlabelPixmap, win->lowrespix, NULL);
     }else{
	  win->hq = TRUE;
	  XtVaSetValues(w, XmNlabelPixmap, win->highrespix, NULL);
     }
     win->expose_draw = True;
     sslice_draw(win);
     return;
}

/*
 * GfX Event: Redraw graphics window due to window being exposed.
 */
static void expose_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *win = (struct Super_slicer *)client;
     Dimension winx, winy;
     int ox, oy;

#ifdef RESIZE_EXPOSE_DEBUG
          printf("Expose:");
#endif

     if (!win->exposed){
	  XtVaGetValues(w, XmNwidth, &winx, XmNheight, &winy, NULL);
	  win->exposed = True;
	  sslice_ginit_cb(w, client, call);
     }else{
	  /* Window can change size with expose event. */
	  XtVaGetValues(w, XmNwidth, &winx, XmNheight, &winy, NULL);
	  ox = win->winx; oy = win->winy;
	  win->winx = winx;
	  win->winy = winy;
	  b3dWinset(XtDisplay(win->glw), win->glw, win->context);
	  b3dResizeViewport();
	  
	  /* DNM: send 12 rather than App->depth to guarantee shorts */
	  if ((winx != ox) || (winy != oy)){
	       win->image   = b3dGetNewCIImage(win->image, 12);
	  }
     }
     sslice_draw(win);

#ifdef RESIZE_EXPOSE_DEBUG
          printf("\n");
#endif
     return;
}

/*
 * GfX Event: This is for the cube window.
 */
static void sslice_expose_cube_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;
     if (!sslice->cubegc){
	  sslice->cubegc = b3dGetContext(w);
     }
     b3dWinset(XtDisplay(sslice->cube), sslice->cube, sslice->cubegc);
     b3dResizeViewport();
     sslice_cube_draw(sslice);
     return;
}


static void sslice_ginit_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;
     Dimension winx, winy;


     sslice->context = b3dGetContext(w);
     b3dWinset(XtDisplay(sslice->glw), sslice->glw, sslice->context);
     XtVaGetValues(w, XmNwidth, &winx, XmNheight, &winy, NULL);
     sslice->winx = winx;
     sslice->winy = winy;

     b3dResizeViewport();

     /* DNM: send 12 rather than App->depth to guarantee shorts */
     sslice->image   = b3dGetNewCIImage(NULL, 12);
     return;
}


static void resize_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;
     Dimension winx, winy;

     XtVaGetValues(w, XmNwidth, &winx, XmNheight, &winy, NULL);
     sslice->winx = winx;
     sslice->winy = winy;


     if (sslice->exposed){
	  b3dWinset(XtDisplay(sslice->glw), sslice->glw, sslice->context);
	  b3dResizeViewport();
	  /* DNM: send 12 rather than App->depth to guarantee shorts */
	  sslice->image   = b3dGetNewCIImage(sslice->image, 12);

	  ivwControlPriority(sslice->vi, sslice->ctrl);
	  sslice_draw(sslice);
     }
     return;
}

static void sslice_resize_cube_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;

     if (sslice->cubegc){
	  b3dWinset(XtDisplay(sslice->cube), sslice->cube, sslice->cubegc);
	  b3dResizeViewport();
	  sslice_cube_draw(sslice);
     }
     return;
}

/* 
 * Tilt angle controls.
 */
/* DNM: in order to have input focus return to the window, call 
   XmProcessTraversal in each of these, and in the other menu and text box
   call backs.  The translation override that tries to make each one pass
   input to the central key input routine must not work.  Traversal must be
   to a widget where this translation does work.  If problems with Mesa require
   use of GLwDrawingArea instead of GLwMDrawingArea, set the target of
   XmProcessTraversal to zoomarrow rather than glw */
static void sslice_anglevalue_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     int axis = X;
     if (w == sslice->anglew[Y])
	  axis = Y;
     if (w == sslice->anglew[Z])
	  axis = Z;

     ivwControlPriority(sslice->vi, sslice->ctrl);
     sslice->tang[axis] = cbs->value * 0.1f;
     sslice->lastangle = axis;
     /* slice_trans_step(sslice); */
     sslice_draw(sslice);
     sslice_showslice(sslice);
     XmProcessTraversal(sslice->glw, XmTRAVERSE_CURRENT);
     return;
}

static void sslice_angledrag_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     int axis = X;
     if (w == sslice->anglew[Y])
	  axis = Y;
     if (w == sslice->anglew[Z])
	  axis = Z;

     sslice->tang[axis] = cbs->value * 0.1f;
     slice_trans_step(sslice);
     sslice_cube_draw(sslice);

/* only update slice if overlay planes are being used. */
#ifdef DRAW_GL
     sslice_showslice(sslice);
#endif
     return;
}

/* DNM: make it simple */
static void setTiltWidgets(struct Super_slicer *sslice)
{
     int axis, value;
     for (axis = 0; axis < 3; axis++) {
	  value =  (int)floor((double)(sslice->tang[axis] * 10.0 + 0.5f));
	  XmScaleSetValue(sslice->anglew[axis], value);
     }
}

/*
 *  Select the current control view.
 */
static void controlViewSlice_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;
     Dimension width, height, border_width = 0;

     XtVaGetValues(sslice->dialog, XmNwidth, &width, XmNheight, &height, NULL);
     sslice->maprowcol = 1;
     XtUnmanageChild(sslice->wThick);
     XtManageChild(sslice->rowcol);
     XtResizeWidget(sslice->dialog, width, height+2, border_width);
     XmProcessTraversal(sslice->glw, XmTRAVERSE_CURRENT);
}
static void controlViewDepth_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;
     Dimension width, height, border_width = 0;

     XtVaGetValues(sslice->dialog, XmNwidth, &width, XmNheight, &height, NULL);
     sslice->maprowcol = 2;
     XtUnmanageChild(sslice->rowcol);
     XtManageChild(sslice->wThick);
     drawThickControls(sslice);
     XtResizeWidget(sslice->dialog, width, height+2, border_width);
     XmProcessTraversal(sslice->glw, XmTRAVERSE_CURRENT);
}
static void controlViewImage_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;
     Dimension width, height, border_width = 0;

     XtVaGetValues(sslice->dialog, XmNwidth, &width, XmNheight, &height, NULL);
     sslice->maprowcol = 0;
     XtUnmanageChild(sslice->rowcol);
     XtUnmanageChild(sslice->wThick);
     XtResizeWidget(sslice->dialog, width, height-1, border_width);
     XmProcessTraversal(sslice->glw, XmTRAVERSE_CURRENT);
}

/*
 * Lock image position for this window.
 */
static void sslice_lock_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;

     ivwControlPriority(sslice->vi, sslice->ctrl);
     if (sslice->locked){
	  sslice->locked = False;
	  XtVaSetValues(w, XmNlabelPixmap, sslice->unlockpix, NULL);
     }else{
	  sslice->locked = True;
	  XtVaSetValues(w, XmNlabelPixmap, sslice->lockpix, NULL);
     }
     return;
}   

void slicerClose_cb(ImodView *vi, void *client, int junk)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;

     /* DNM: some kind of recursion happens on PC when two windows are on
	top of each other and they are closed, so test if closing already */
     /* 3/29/01: couldn't reproduce problem, this may not be needed, especially
	after removing the dia_input */
     if (sslice->closing)
	  return;
     sslice->closing = 1;
     if (sslice->iid)
	  XtRemoveTimeOut(sslice->iid);
     if (sslice->work_id)
	  XtRemoveWorkProc(sslice->work_id);
     sslice->iid = 0;
     sslice->work_id = 0;

     /* DNM 3/29/01: This caused crashes on PC when double-clicked on destroy 
	icon of a slicer that was behind another window and displayed slowly.
	No other windows use it */
     /* dia_input(); */

    /* b3dWinset(XtDisplay(sslice->glw), 0, 0);*/
     b3dWinset(imodDisplay(), 0, 0);
     XtPopdown(sslice->dialog);

     XtDestroyWidget(sslice->dialog);

     b3dFreeCIImage(sslice->image);
     imodMatDelete(sslice->mat);
     free(sslice);
     return;
}

void slicerDraw_cb(ImodView *vi, void *client, int drawflag)
{
     struct Super_slicer *win = (struct Super_slicer *)client;
     float usex, usey, usez;

     /* DNM: need to make sure window was initialized after first expose */
     if (!win->exposed)
	  return;

     /* DNM: use a value saved in structure in case more than one window */
     if (win->zslast != win->vi->imod->zscale){
	  win->zslast = win->vi->imod->zscale;
	  drawflag |= IMOD_DRAW_ACTIVE;
     }

     b3dWinset(XtDisplay(win->glw), win->glw, (XID)win->context);

     if (drawflag & IMOD_DRAW_XYZ){
	  if (!win->locked){
	       /* DNM: if there is a pending set of values from a mouse hit,
		  use them instead of the mouse values for this */
	       if (win->pending) {
		    usex = win->pendx;
		    usey = win->pendy;
		    usez = win->pendz;
	       } else {
		    usex = win->vi->xmouse;
		    usey = win->vi->ymouse;
		    usez = win->vi->zmouse;
	       }
	       if ((win->lx != usex) ||
		   (win->ly != usey) ||
		   (win->lz != usez)){
		    win->cx = win->lx = usex;
		    win->cy = win->ly = usey;
		    win->cz = win->lz = usez;
		    win->pending = 0;
		    sslice_draw(win);
		    return;
	       }
	  }
     }

     if (drawflag & (IMOD_DRAW_ACTIVE | IMOD_DRAW_IMAGE)){
	  if (win->pending) {
	       win->cx = win->lx = win->pendx;
	       win->cy = win->ly = win->pendy;
	       win->cz = win->lz = win->pendz;
	       win->pending = 0;
	  }
	  sslice_draw(win);
	  return;
     }
     
     if (drawflag & IMOD_DRAW_MOD){
	  slicerUpdateImage(win);
     }

     /* todo: if just the model is changed we already have image
      * buffered.
      * so all we have to do is redraw the model. 
      */

}


/*
 *  Close window, free resources.
 */
static void sslice_quit_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;
     ivwDeleteControl(sslice->vi, sslice->ctrl);      
     return;
}

/*
 * Open new slicer.
 */
int sslice_open(struct ViewInfo *vi)
{
     struct Super_slicer *sslice;
     XtTranslations keytrans = XtParseTranslationTable(KeyTranslations);
     Atom wmclose;
     Widget button;
     Widget command_col, tool_row;
     Widget form, rowcol, col, row, frame, window;
     Widget show, res, label, arrow;
     char *window_name;
     XtTranslations transtable;
     Pixel   fg, bg;
     int depth;
     static int first = True;
     int axis;

     sslice = (struct Super_slicer *)malloc(sizeof(struct Super_slicer));
     if (!sslice)
	  return(-1);

     /* DNM 5/16/02: if the current position is still in the lower left
	corner, move it to middle and draw other windows */
     if (!vi->xmouse && !vi->ymouse) {
	  vi->xmouse = vi->xsize / 2;
	  vi->ymouse = vi->ysize / 2;
	  imodDraw(vi, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ);
     }

     sslice->cx = vi->xmouse;
     sslice->cy = vi->ymouse;
     sslice->cz = vi->zmouse;
     sslice->app    = Dia_context;
     sslice->vi     = vi;
     sslice->locked = 0;
     sslice->expose_draw = 0;
     sslice->ginit  = False;
     sslice->zoom   = 1.0;
     sslice->lx     = vi->xmouse;
     sslice->ly     = vi->ymouse;
     sslice->lz     = vi->zmouse;
     sslice->iid    = 0;
     sslice->work_id = 0;
     sslice->hq     = 0;
     sslice->fasthq     = 1;
     sslice->tang[X] = 0.0f;
     sslice->tang[Y] = 0.0f;
     sslice->tang[Z] = 0.0f;
     sslice->mapped = False;
     sslice->scalez = False;
     sslice->depth = 1.0;
     sslice->image = NULL;
     sslice->exposed = False;
     sslice->cubegc = 0;
     sslice->bcoord[0] = vi->xmouse;
     sslice->bcoord[1] = vi->ymouse;
     sslice->bcoord[2] = vi->zmouse;
     sslice->xstep[0]  = 1.0f; sslice->xstep[1] = sslice->xstep[2] = 0.0f;
     sslice->ystep[1]  = 1.0f; sslice->ystep[0] = sslice->ystep[2] = 0.0f;
     sslice->nslice = 1;
     sslice->mat = imodMatNew(3);
     sslice->lastangle = X;
     sslice->zslast = 1.0;
     sslice->pending = 0;
     sslice->closing = 0;

     slice_trans_step(sslice);
     window_name = imodwfname("IMOD Slicer: ");
     sslice->dialog = XtVaCreatePopupShell
	   ("Slicer", topLevelShellWidgetClass, App->toplevel,
	    XmNuserData, (XtPointer)sslice,
	    XmNvisual, App->visual,
	    XtNtitle, window_name,
	    NULL);
     if (window_name)
	  free(window_name);

     if (!sslice->dialog){
	  free(sslice);
	  return(-1);
     }
     
     window = XtVaCreateManagedWidget
	  ("slicer",  xmMainWindowWidgetClass,  sslice->dialog,
	   NULL);

     command_col = XtVaCreateManagedWidget
	  ("command_col", xmRowColumnWidgetClass, window, NULL);

     frame = XtVaCreateManagedWidget
	  ("frame", xmFrameWidgetClass,  command_col,
	   XmNshadowType, XmSHADOW_OUT,
	   NULL);
     tool_row = XtVaCreateManagedWidget
	  ("tool_row", xmRowColumnWidgetClass, frame,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     {
	  sslice->zoomarrow = XtVaCreateManagedWidget
	       ("arrow_up", xmArrowButtonWidgetClass, tool_row,
		XmNarrowDirection, XmARROW_UP, 
		XmNuserData, (XtPointer)sslice,
		NULL);
	  imodOverrideTranslations(sslice->zoomarrow, keytrans);
	  XtAddCallback(sslice->zoomarrow, XmNarmCallback, sslice_zoomup_cb,
			(XtPointer)sslice);
	  arrow = XtVaCreateManagedWidget
	       ("arrow_down", xmArrowButtonWidgetClass, tool_row,
		XmNarrowDirection, XmARROW_DOWN, 
		XmNuserData, (XtPointer)sslice,
		NULL);
	  imodOverrideTranslations(arrow, keytrans);
	  XtAddCallback(arrow, XmNarmCallback, sslice_zoomdown_cb,
			(XtPointer)sslice);
	  XtVaGetValues(arrow,
			XmNforeground, &fg,
			XmNbackground, &bg,
			XmNdepth, &depth,
			NULL);
	  XtVaSetValues(sslice->dialog, XmNuserData, (XtPointer)sslice, NULL);

	  sslice->showpix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)showslice_bits, 
		slicer_button_width, slicer_button_height,
		fg, bg, depth);

	  sslice->lockpix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)lock_bits,
		slicer_button_width, slicer_button_height,
		fg, bg, depth);
	  sslice->unlockpix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)unlock_bits,
		slicer_button_width, slicer_button_height,
		fg, bg, depth);

	  sslice->lowrespix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)lowres_bits,
		slicer_button_width, slicer_button_height,
		fg, bg, depth);
	  sslice->highrespix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)highres_bits,
		slicer_button_width, slicer_button_height,
		fg, bg, depth);
	  button = XtVaCreateManagedWidget
	       ("Show Slice", xmPushButtonWidgetClass, tool_row, 
		XmNlabelType, XmPIXMAP,
		XmNlabelPixmap, sslice->showpix,
		XmNuserData, (XtPointer)sslice,
		NULL);
	  imodOverrideTranslations(button, keytrans);
	  XtAddCallback(button, XmNactivateCallback, sslice_show_cb,
			(XtPointer)sslice);

	  button = XtVaCreateManagedWidget
	       ("Lock", xmPushButtonWidgetClass, tool_row,
		XmNlabelType, XmPIXMAP,
		XmNlabelPixmap, sslice->unlockpix,
		XmNuserData, (XtPointer)sslice,
		NULL);
	  imodOverrideTranslations(button, keytrans);
	  XtAddCallback(button, XmNactivateCallback, sslice_lock_cb,
			(XtPointer)sslice);
	  button = XtVaCreateManagedWidget
	       ("HQ Graphics", xmPushButtonWidgetClass, tool_row, 
		XmNlabelType, XmPIXMAP,
		XmNlabelPixmap, sslice->lowrespix,
		XmNindicatorOn, False,
		XmNuserData, (XtPointer)sslice,
		NULL);
	  imodOverrideTranslations(button, keytrans);
	  XtAddCallback(button, XmNactivateCallback, sslice_res_cb, 
			(XtPointer)sslice);

	  /*
	   * Create control option menu.
	   */
	  {
	       Widget optmenu, pulldown, pb, pb1;
	       Arg             args[4];
	       Cardinal        n = 0;

	       
	       XtSetArg(args[n], XmNvisual, App->visual); n++;
	       pulldown = XmCreatePulldownMenu
		    (tool_row, "option", args, n);
	       pb1 = XtVaCreateManagedWidget 
		    ("Slice", xmPushButtonWidgetClass, pulldown, NULL);
	       XtAddCallback(pb1, XmNactivateCallback,
			     controlViewSlice_cb, (XtPointer)sslice);
	       pb = XtVaCreateManagedWidget
		    ("Depth", xmPushButtonWidgetClass, pulldown, NULL);
	       XtAddCallback(pb, XmNactivateCallback,
			     controlViewDepth_cb, (XtPointer)sslice);
	       pb = XtVaCreateManagedWidget
		    ("Image", xmPushButtonWidgetClass, pulldown, NULL);
	       XtAddCallback(pb, XmNactivateCallback,
			     controlViewImage_cb, (XtPointer)sslice);
	       XtSetArg(args[n], XmNsubMenuId, pulldown); n++;
	       XtSetArg(args[n], XmNmenuHistory, pb1);    n++;
	       optmenu =  XmCreateOptionMenu(tool_row, "option", args, n);
	       XtManageChild(optmenu);
	  }

	  button = XtVaCreateManagedWidget
	       ("Help", xmPushButtonWidgetClass, tool_row,
		XmNuserData, (XtPointer)sslice,
		NULL);
	  imodOverrideTranslations(button, keytrans);
	  XtAddCallback(button, XmNactivateCallback, help_cb,
			(XtPointer)sslice);
     }
     XtManageChild(tool_row);
     XtManageChild(frame);

     /* DNM: switch from a RowColumn to a Form because a resize on the PC
	put the cube in the middle and shrunk the sliders */
     /*     rowcol = XtVaCreateManagedWidget
	  ("slice_rowcol", xmRowColumnWidgetClass, command_col,
	   XmNorientation, XmHORIZONTAL,
	   NULL); */
     rowcol = XtVaCreateManagedWidget
	  ("slice_form", xmFormWidgetClass, command_col,
	   NULL);
     sslice->rowcol = rowcol;
     sslice->maprowcol = 1;
     
     sslice->wThick = createThickControls(sslice, command_col);

     /* DNM: added the attachment when switched rowcol to a Form */
     col = XtVaCreateWidget
	  ("slice_col", xmRowColumnWidgetClass, rowcol,
	   XmNleftAttachment, XmATTACH_FORM,
	   NULL);
     for (axis = 0; axis < 3; axis++) {
	  sslice->anglew[axis] = XtVaCreateManagedWidget
	       ("slice_scale", xmScaleWidgetClass, col,
		XmNwidth, 218, 
		XmNorientation,   XmHORIZONTAL,
		XmNmaximum,       (int)(10. * maxAngle[axis]),
		XmNminimum,       (int)(-10. * maxAngle[axis]),
		XmNvalue,         0,
		XmNdecimalPoints, 1,
		XmNshowValue,     True,
		XmNscaleMultiple, 10,
		NULL);
	  imodOverrideTranslations(sslice->anglew[axis], keytrans);
	  XtAddCallback(sslice->anglew[axis], XmNvalueChangedCallback, 
			sslice_anglevalue_cb, (XtPointer)sslice);
	  XtAddCallback(sslice->anglew[axis], XmNdragCallback, 
			sslice_angledrag_cb, (XtPointer)sslice);
     }
     XtManageChild(col);

     /* DNM: added the attachments when switched rowcol to a Form */
     frame = XtVaCreateManagedWidget
	  ("frame", xmFrameWidgetClass, rowcol,
	   XmNshadowType, XmSHADOW_IN,
	   XmNtopAttachment, XmATTACH_FORM,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNleftAttachment, XmATTACH_WIDGET,
	   XmNleftWidget, col,
	   NULL);

     /* DNM: doublebuffer needed on PC-Linux in rh 6.x but not 7.0 */
     sslice->cube = XtVaCreateManagedWidget
	  ("glcubewidget", B3dDrawingAreaWidgetClass, frame,
	   GLwNrgba, True,
	   NULL);
     XtAddCallback(sslice->cube, B3dNresizeCallback,
		   sslice_resize_cube_cb, (XtPointer)sslice);
     XtAddCallback(sslice->cube, B3dNexposeCallback,
		   sslice_expose_cube_cb, (XtPointer)sslice);
     XtSetMappedWhenManaged(rowcol,True);
     XtManageChild(rowcol);

     frame = XtVaCreateManagedWidget
	  ("frame", xmFrameWidgetClass, window,
	   XmNshadowType, XmSHADOW_IN,
	   NULL);

     sslice->glw = XtVaCreateManagedWidget
	  ("glwidget", B3dDrawingAreaWidgetClass, frame,
	   XmNnavigationType, XmNONE,
	   XmNtraversalOn, True,
	   XmNheight, 150,
   XmNtranslations, XtParseTranslationTable (B3DGFX_Translations),
	   /*	   XmNvisual, App->visual, */
	   XmNcolormap, App->cmapGL,
	   GLwNvisualInfo, App->visualinfoGL,
	   NULL);
     XtAddCallback
	  (sslice->glw, B3dNexposeCallback, expose_cb, (XtPointer)sslice);
     XtAddCallback
	  (sslice->glw, B3dNresizeCallback, resize_cb, (XtPointer)sslice);
     XtAddCallback
	  (sslice->glw, B3dNinputCallback,  input_cb,  (XtPointer)sslice);
     
     imodOverrideTranslations(sslice->glw,
			    XtParseTranslationTable(B3DGFX_Translations));


     XtManageChild(command_col);

#ifdef MWSETWORK
     XtVaSetValues(window,XmNworkWindow,frame,NULL);
#endif
     XmMainWindowSetAreas( window, NULL, command_col, NULL, NULL, frame);
         
     wmclose = XmInternAtom( XtDisplay(sslice->dialog), 
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(sslice->dialog, wmclose, sslice_quit_cb,
			     (caddr_t)sslice);
     
     XtPopup(sslice->dialog, XtGrabNone);

     sslice->ctrl = ivwNewControl(vi, slicerDraw_cb, slicerClose_cb,
                               (XtPointer)sslice);

     XDefineCursor(App->display, XtWindow(sslice->glw), App->cursor_cross);
     XmProcessTraversal(sslice->glw, XmTRAVERSE_CURRENT);
     return(0);
}

/****************************************************************************/
/* Thickness controls. 
 *
 * Update thickness text widgets.
 */
static void drawThickControls(struct Super_slicer *ss)
{
     char vals[16];
     int i;

     sprintf(vals, "%g", ss->depth);
     XtVaSetValues(ss->wModelThick, XmNvalue, vals, NULL);

     i = ss->nslice;
     sprintf(vals, "%3d", i);
     XtVaSetValues(ss->wImageThick, XmNvalue, vals, NULL);
}

static void imagethick_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;
     char *st = NULL;
     int sno;

     ivwControlPriority(sslice->vi, sslice->ctrl);
     st = XmTextGetString(w);
     sno = atoi(st);
     XtFree(st);
     if (sno < 1)
	  sno = 1;
     sslice->nslice = sno;

     drawThickControls(sslice);
     sslice_draw(sslice);
     XmProcessTraversal(sslice->glw, XmTRAVERSE_CURRENT);
}

static void modelthick_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;
     char *st = NULL;

     ivwControlPriority(sslice->vi, sslice->ctrl);
     st = XmTextGetString(w);
     sslice->depth = atof(st);
     if (sslice->depth <= 0.0)
	  sslice->depth = 1.0;
     XtFree(st);

     drawThickControls(sslice);
     sslice_draw(sslice);
     XmProcessTraversal(sslice->glw, XmTRAVERSE_CURRENT);
}

static void zscale_off_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;

     ivwControlPriority(sslice->vi, sslice->ctrl);
     sslice->scalez = 0;
     sslice_draw(sslice);
     XmProcessTraversal(sslice->glw, XmTRAVERSE_CURRENT);
}
static void zscale_before_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;

     ivwControlPriority(sslice->vi, sslice->ctrl);
     sslice->scalez = SLICE_ZSCALE_BEFORE;
     sslice_draw(sslice);
     XmProcessTraversal(sslice->glw, XmTRAVERSE_CURRENT);
}
static void zscale_after_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;

     ivwControlPriority(sslice->vi, sslice->ctrl);
     sslice->scalez = SLICE_ZSCALE_AFTER;
     sslice_draw(sslice);
     XmProcessTraversal(sslice->glw, XmTRAVERSE_CURRENT);
}

static Widget createThickControls(struct Super_slicer *ss,
				  Widget parent)
{
     Widget w, rowcol, master;
     
     master = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, parent,
	   NULL);
     
     rowcol = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, master,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     XtVaCreateManagedWidget
	  ("Image Thickness", xmLabelWidgetClass, rowcol, NULL);
     ss->wImageThick = w = XtVaCreateManagedWidget
	  ("text", xmTextWidgetClass, rowcol,
	   XmNcolumns, 4,
	   NULL);
     XtAddCallback(w, XmNactivateCallback,
		   imagethick_cb, (XtPointer)ss);
     XtManageChild(rowcol);

     rowcol = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, master,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     XtVaCreateManagedWidget
	  ("Model Thickness", xmLabelWidgetClass, rowcol, NULL);
     ss->wModelThick = w = XtVaCreateManagedWidget
	  ("text", xmTextWidgetClass, rowcol,
	   XmNcolumns, 4,
	   NULL);
     XtAddCallback(w, XmNactivateCallback,
		   modelthick_cb, (XtPointer)ss);
     XtManageChild(rowcol);

     {
	  Widget optmenu, pulldown, pb1, pb;
	  Arg             args[10];
	  Cardinal        n = 0;

	  rowcol = XtVaCreateWidget
	       ("rowcol", xmRowColumnWidgetClass, master,
		XmNorientation, XmHORIZONTAL,
		NULL);
	  XtVaCreateManagedWidget
	       ("Z-Scale:", xmLabelWidgetClass, rowcol, NULL);

	  XtSetArg(args[n], XmNvisual, App->visual); n++;
	  pulldown = XmCreatePulldownMenu
	       (rowcol, "option_pd", args, n);
	  pb1 = XtVaCreateManagedWidget
	       ("Off", xmPushButtonWidgetClass, pulldown, NULL);
	  XtAddCallback(pb1, XmNactivateCallback, 
			zscale_off_cb, (XtPointer)ss);
	  pb = XtVaCreateManagedWidget
	       ("Scale Before", xmPushButtonWidgetClass, pulldown, NULL);
	  XtAddCallback(pb, XmNactivateCallback,
			zscale_before_cb, (XtPointer)ss);
	  pb = XtVaCreateManagedWidget
	       ("Scale After", xmPushButtonWidgetClass, pulldown, NULL);
	  XtAddCallback(pb, XmNactivateCallback,
			zscale_after_cb, (XtPointer)ss);

	  XtSetArg(args[n], XmNsubMenuId, pulldown); n++;
	  XtSetArg(args[n], XmNmenuHistory, pb1);    n++;
	  optmenu =  XmCreateOptionMenu(rowcol, "option_rc", args, n);
	  XtManageChild(optmenu);
	  XtManageChild(rowcol);
     }
     XtSetMappedWhenManaged(master,True);
     return(master);
}

/* Broadcast position of this slice, and let other windows
 * show where this slice intersects with their views.
 */
int sslice_showslice(struct Super_slicer *ss)
{
     int isize, jsize, i, j;
     int xo, yo, zo;
     float xs, ys;
     float xsz, ysz, zsz;
     float x, y, z;
     
     isize = ss->winx / ss->zoom;
     jsize = ss->winy / ss->zoom;

     xo = ss->vi->xmouse;
     yo = ss->vi->ymouse;
     zo = ss->vi->zmouse;

     imodDraw(ss->vi, IMOD_DRAW_SLICE);
     return(0);
}


#ifdef USE_SLICER_TIMER
/* 
 * Timer to check current position.
 */
static void sslice_addworkproc(XtPointer client, XtIntervalId *id)
{
     struct Super_slicer *ss = (struct Super_slicer *)client;
     ss->work_id = XtAppAddWorkProc(ss->app, sslice_update, ss);
     ss->iid = 0;
}

/* Timer update function */
static void sslice_tupdate(XtPointer client, XtIntervalId *id)
{
     struct Super_slicer *ss = (struct Super_slicer *)client;
     static float zs = 1.0f;

     if (!ss->vi)
	  return;
     if (!ss->iid)
	  return;
     if (!ss->locked)
	  if ((ss->lx != ss->vi->xmouse) ||
	      (ss->ly != ss->vi->ymouse) ||
	      (ss->lz != ss->vi->zmouse)){
	       ss->cx = ss->lx = ss->vi->xmouse;
	       ss->cy = ss->ly = ss->vi->ymouse;
	       ss->cz = ss->lz = ss->vi->zmouse;
	       
	       sslice_draw(ss);
     }

     if (zs != ss->vi->imod->zscale){
	  zs = ss->vi->imod->zscale;
	  sslice_draw(ss);
     }

     if (ss->locked)
	  ss->iid = XtAppAddTimeOut (ss->app, 1000L, sslice_tupdate,
				     (XtPointer)ss);
     else
	  ss->iid = XtAppAddTimeOut (ss->app, 100L, sslice_tupdate,
				     (XtPointer)ss);
}
/* Workproc update function */
Boolean sslice_update(XtPointer client_data)
{
     struct Super_slicer *ss = (struct Super_slicer *)client_data;

     if ((ss->lx != ss->vi->xmouse) ||
	 (ss->ly != ss->vi->ymouse) ||
	 (ss->lz != ss->vi->zmouse)){
	  ss->lx = ss->vi->xmouse;
	  ss->ly = ss->vi->ymouse;
	  ss->lz = ss->vi->zmouse;
	  sslice_draw(ss);
	  return(True);
     }

     ss->iid = XtAppAddTimeOut (ss->app, 100L, sslice_addworkproc, 
				 (XtPointer)ss);
     ss->work_id = 0;
     return(True);
}
#endif

static void slice_keyinput(XKeyEvent *event, struct Super_slicer *sslice)
{
     KeySym keysym = XLookupKeysym(event, 0);
     int lang = sslice->lastangle;
     int newang;

     if (imodPlugHandleKey(sslice->vi, event)) return;

     switch(keysym){
	  
	case '=':
	  if (event->state & ShiftMask) {
	       sslice->nslice++;
	       drawThickControls(sslice);
	  }
	  else
	       sslice->zoom = b3dStepPixelZoom(sslice->zoom, 1);
	  break;

	case '-':
	  if (event->state & ShiftMask){
	       sslice->nslice--;
	       if (sslice->nslice < 1) sslice->nslice = 1;
	       drawThickControls(sslice);
	  }else{
	       sslice->zoom = b3dStepPixelZoom(sslice->zoom, -1);
	       /*	       sslice->zoom -= 1.0;
	       if (sslice->zoom < 1.0)
	       sslice->zoom = 1.0; */
	  }
	  break;

	case '9':
	  sslice->depth -= 1.0;
	  if (sslice->depth < 1.0)
	       sslice->depth  = 1.0;
	  drawThickControls(sslice);
	  break;
	case '0':
	  sslice->depth += 1.0;
	  drawThickControls(sslice);
	  break;
	  
	case XK_s:
	case XK_S:
	  if ((event->state & ShiftMask) || (event->state & ControlMask)){
	       b3dWinset(XtDisplay(sslice->glw), 
			 sslice->glw, sslice->context);
	       sslice_draw(sslice);
	       b3dWinset(XtDisplay(sslice->glw),
			 sslice->glw, sslice->context);
	       if (event->state & ShiftMask)
		   b3dAutoSnapshot("slicer", SnapShot_RGB, NULL);
	       else if (event->state & ControlMask)
		   b3dAutoSnapshot("slicer", SnapShot_TIF, NULL);
	  }else
	       sslice_showslice(sslice);
	  break;


	case XK_x:
	case XK_y:
	case XK_z:
	  {
	      Ipoint *p1, *p2;
	      int ob,co,pt, axis;
	      Icont *cont = imodContourGet(sslice->vi->imod);
	      imodGetIndex(sslice->vi->imod, &ob,&co,&pt);

	      p2 = imodPointGet(sslice->vi->imod);
	      if ((cont) && (p2) && (cont->psize > 1)){
		  if (pt)
		       p1 = &cont->pts[pt-1];
		  else
		       p1 = &cont->pts[pt+1];
		  axis = X;
		  if (keysym == XK_y)
		       axis = Y;
		  if (keysym == XK_z)
		       axis = Z;
		  if (event->state & ShiftMask){
		       p2 = &cont->pts[cont->psize - 1];
		       p1 = &cont->pts[0];
		  }
		  sliceSetAnglesFromPoints(sslice, p1, p2, axis);
		  /* slice_trans_step(sslice);
		     sslice_cube_draw(sslice); */
		  sslice_draw(sslice);
	      }
	  }
	  break;

	  /* DNM: add these to adjust last angle, now that input is properly
	     passed to the window */
	case XK_KP_Up:
	case XK_KP_Down:
	case XK_KP_Right:
	case XK_KP_Left:
	case XK_KP_Insert:
	  if (keysym == XK_KP_Down) sslice->tang[lang] -= 0.5;
	  if (keysym == XK_KP_Left) sslice->tang[lang] -= 0.1;
	  if (keysym == XK_KP_Right) sslice->tang[lang] += 0.1;
	  if (keysym == XK_KP_Up) sslice->tang[lang] += 0.5;
	  if (keysym == XK_KP_Insert) sslice->tang[lang] = 0.0;
	  if (sslice->tang[lang] > maxAngle[lang])
	       sslice->tang[lang] = maxAngle[lang];
	  if (sslice->tang[lang] < -maxAngle[lang])
	       sslice->tang[lang] = -maxAngle[lang];

	  setTiltWidgets(sslice);

	  sslice_draw(sslice);
	  sslice_showslice(sslice);
	  break;

	  /* case XK_h:
	     sslice->fasthq = 1 - sslice->fasthq;
	     printf("fasthq = %d\n",sslice->fasthq);
	     break;
	  */

	default:
	  inputDefaultKeys(event, sslice->vi);
	  return;
     }
     sslice_draw(sslice);	       
     return;
}

static void input_cb(Widget w, XtPointer client, XtPointer call)
{
     struct Super_slicer *sslice = (struct Super_slicer *)client;
     B3dDrawingAreaCallbackStruct *cbs = (B3dDrawingAreaCallbackStruct *)call;

     ImodView *vi = sslice->vi;
     char buffer[1];
     int bufsize = 1;
     int charcount;
     KeySym keysym;

     ivwControlPriority(vi, sslice->ctrl);
     switch(cbs->event->type){
	  
	case UnmapNotify:
	  sslice->mapped = False;
	  break;

	case MapNotify:
	  sslice->mapped = True;
	  break;

	case EnterNotify:
	  XmProcessTraversal(sslice->glw, XmTRAVERSE_CURRENT);
	  break;

	case KeyPress:
	  slice_keyinput((XKeyEvent *)cbs->event, sslice);
	  break;
     
	case KeyRelease:
	  break;

	case ButtonPress:
	  XmProcessTraversal(sslice->glw, XmTRAVERSE_CURRENT);
	  if (cbs->event->xbutton.button == 1){
	       sslice_setxyz(sslice, cbs->event->xbutton.x, 
			     cbs->event->xbutton.y);
	       /* DNM: for select hits, do keep cz at an integral value */
	       sslice->pending = 0;
	       imodDraw(sslice->vi, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ);
	  }
	  if (cbs->event->xbutton.button == 2)
	       slicer_insert_point(sslice, cbs->event->xbutton.x,
				  cbs->event->xbutton.y);
	  if (cbs->event->xbutton.button == 3)
	       slicer_modify_point(sslice, cbs->event->xbutton.x,
				  cbs->event->xbutton.y);
	  break;

	case ButtonRelease:
	  break;

	case MotionNotify:
	  break;

	default:
	  break;
     }
     return;
}

static void slicer_insert_point(struct Super_slicer *ss, int x, int y)
{
     sslice_setxyz(ss, x, y);
     inputInsertPoint(ss->vi);
     return;
}

static void slicer_modify_point(struct Super_slicer *ss, int x, int y)
{
     sslice_setxyz(ss, x, y);
     inputModifyPoint(ss->vi);
     return;
}

static void sslice_setxyz(struct Super_slicer *ss, int x, int y)
{
     float xoffset, yoffset;
     float xs, ys, zs;
     float xsz, ysz, zsz;
     float xm, ym, zm;
     int zmouse;

     /* DNM: the xzoom and yzoom are correct for all cases, and the zs
	needs to be applied only for the case of SCALE_BEFORE */

     zs = 1.0f;
     if (ss->scalez == SLICE_ZSCALE_BEFORE && ss->vi->imod->zscale > 0)
	  zs = 1.0 / ss->vi->imod->zscale;

     /* DNM: have to use integer arithmetic on window sizes, and account for
	y going from 0 to winy-1 when inverting it */

     xoffset = ((ss->winx / 2) - x) / ss->xzoom;
     yoffset = ((ss->winy / 2) - (ss->winy - 1 - y)) / ss->yzoom;
     
     /* DNM: always take position relative to current display center,
	regardless of whether the window is locked (it was using [xyz]mouse */
     xm = ss->cx;
     ym = ss->cy;
     zm = ss->cz;

     xm -= (ss->xstep[X] * xoffset) + (ss->ystep[X] * yoffset);
     ym -= (ss->xstep[Y] * xoffset) + (ss->ystep[Y] * yoffset);
     
     zm -= (ss->xstep[Z] * xoffset * zs) + (ss->ystep[Z] * yoffset * zs);

     if (xm < 0)
	  xm = 0;
     if (xm >= ss->vi->xsize)
	  xm = ss->vi->xsize - 1;
     if (ym < 0)
	  ym = 0;
     if (ym >= ss->vi->ysize)
	  ym = ss->vi->ysize - 1;
     if (zm < 0)
	  zm = 0;
     if (zm >= ss->vi->zsize)
	  zm = ss->vi->zsize - 1;

     zmouse = floor((double)(zm + 0.5f));

     /* Set up pending coordinates for next draw to use */
     ss->pendx = xm;
     ss->pendy = ym;
     ss->pendz = zm;
     ss->pending = 1;

     /* DNM: do this regardless of whether locked or not.  Also, set zmouse
	to the integral value but keep pendz at a real value */
     ss->vi->xmouse = xm; 
     ss->vi->ymouse = ym; 
     ss->vi->zmouse = zmouse;
     return;
}

#ifdef NEED_SLICE_CALC
/* DNM: This has not been changed to reflect change in angle signs */
static void slice_calc(struct Super_slicer *ss)
{
     float v1[3], v2[3], v3[3]; /* normal vectors to points in plane */
     float a, b, c;  /* input angles in radians. */
     float pnorm[3]; /* plane normal vector      */
     float *u1, u2[3];
     float cosofy;
     
     a = RADIANS_PER_DEGREE * ss->inangle[0];
     b = RADIANS_PER_DEGREE * ss->inangle[1];
     
     ss->tang[0] = ss->inangle[0];
     ss->tang[2] = ss->inangle[2];
     switch (ss->order[0]){
	case X:
	  v1[X] = 0; v1[Y] =  cos(a); v1[Z] = sin(a);
	  v2[X] = 0; v2[Y] = -sin(b); v2[Z] = cos(b);
	  /*     pnorm = v1 cross v2; */
	  v3[X] = 0; v3[Y] = pnorm[Z], v3[Z] = pnorm[Y];
	  break;
	case Y:
	  v1[X] = sin(a); v1[Y] = 0; v1[Z] =  cos(a);
	  v2[X] = cos(b); v2[Y] = 0; v2[Z] = -sin(b);
	  /*     pnorm = v1 cross v2; */
	  v3[X] = pnorm[Z]; v3[Y] = 0; v3[Z] = pnorm[X];
	  break;
	case Z:
	  v1[X] =  cos(a); v1[Y] = sin(a); v1[Z] = 0;
	  v2[X] = -sin(b); v2[Y] = cos(b); v2[Z] = 0;
	  /*     pnorm = v1 cross v2; */
	  v3[X] = pnorm[Y]; v3[Y] = pnorm[X]; v3[Z] = 0;
	  break;
     }
     
     u1 = v1;
     /*     c = - (u1 dot u2) / |u1| * |u1|;    */
     /*     cosofy = |v2| * |u2| / (v2 dot u2); */
     ss->tang[1] = acos(cosofy);
     
     /* now we can do transformations    */
     
     /* and fill xstep, ystep, and zstep */
     
     /* and then calculate the steps through x,y,z plane intersections. */
     
     
     /* the rest we calculate later. */
     return;
}
#endif

#ifndef NDEBUG
int printmat(Imat *mat)
{
     int i;
     for(i = 0; i < 16; i+=4)
	  wprint ("%5.2f %5.2f %5.2f %5.2f\n",
		  mat->data[i], mat->data[i+1], 
		  mat->data[i+2], mat->data[i+3]);
     return(0);
}
#endif

static double fixangle(double angle)
{
     double rpd = RADIANS_PER_DEGREE;
     if (angle <= -180. * rpd)
	  angle += 360. * rpd;
     if (angle > 180. * rpd)
	  angle -= 360. * rpd;
     return angle;
}


void sliceSetAnglesFromPoints(struct Super_slicer *ss,
			      Ipoint *p1, Ipoint *p2, int axis)
{
    Ipoint n;
    Ipoint a;
    double val;
    float small = 1.e-4;
    double rpd = RADIANS_PER_DEGREE;
    n.x = p2->x - p1->x;
    n.y = p2->y - p1->y;
    n.z = p2->z - p1->z;
    if (ss->scalez == SLICE_ZSCALE_BEFORE)
	 n.z *= ss->vi->imod->zscale;
    if (n.x == 0.0 && n.y == 0.0 && n.z == 0.0)
	 return;
    imodPointNormalize(&n);

    if (axis == X) {
	 a.z = 0.0;
	 if (n.x > small || n.y > small || n.x < -small || n.y < -small)
	      a.z = -atan2((double)n.y, (double)n.x);
	 val = a.z;
	 val = n.x * cos(val) - n.y * sin(val);
	 a.y = fixangle(90. * rpd - atan2(val, (double)n.z));
	 a.x = 0.0;
    } else if (axis == Y) {
       	 a.z = 0.0;
	 if (n.x > small || n.y > small || n.x < -small || n.y < -small)
	      a.z = fixangle(90. * rpd - atan2((double)n.y, (double)n.x));
	 val = a.z;
	 val = n.x * sin(val) + n.y * cos(val);
	 a.x = -atan2((double)n.z, val);
	 a.y = 0.0;
    } else {
	 /* This may be most logical in terms of general angles available */
	 /*	 a.y = 0.0;
	 if (n.x > small || n.z > small || n.x < -small || n.z < -small)
	      a.y = -atan2((double)n.x, (double)n.z);
	 val = a.y;
	 val = n.z * cos(val) - n.x * sin(val);
	 a.x = fixangle(90. * rpd - atan2(val, (double)n.y));
	 a.z = 0.0; */

	 /* But this is useful for doing Z rotation and then X, and for
	    keeping all the angles under +/- 90 degrees. */
       	 a.z = 0.0;
	 if (n.x > small || n.y > small || n.x < -small || n.y < -small)
	      if (n.y >= 0.0)
		   a.z = atan2((double)n.x, (double)n.y);
	      else
		   a.z = -atan2((double)n.x, -(double)n.y);
	 val = a.z;
	 val = n.x * sin(val) + n.y * cos(val);
	 if (n.z >= 0.0)
	      a.x = atan2(val, (double)n.z);
	 else
	      a.x = -atan2(val, -(double)n.z);
	 a.y = 0.0;
    }

    ss->tang[X] = a.x / rpd;
    ss->tang[Y] = a.y / rpd;
    ss->tang[Z] = a.z / rpd;
    setTiltWidgets(ss);
}

/* set up the step factors for a new slice angle. */
static void slice_trans_step(struct Super_slicer *ss)
{
     Ipoint pnt, tpnt;
     Ipoint xn, yn, zn;
     Ipoint normal, xcut, ycut, zcut;
     float xp,yp,zp;
     float x,y,z;
     float isize, jsize, zs;
     Imat *mat = ss->mat;

     /* DNM: made the angles be negative to match angles applied to volume */
  
     imodMatId(mat);
     imodMatRot(mat, (double)(-ss->tang[X]), X); 
     imodMatRot(mat, (double)(-ss->tang[Y]), Y);
     imodMatRot(mat, (double)(-ss->tang[Z]), Z);

     pnt.y = pnt.z = 0.0f;
     pnt.x = 1.0f;
     imodMatTransform3D(mat, &pnt, &xn);
     pnt.x = 0.0f;
     pnt.y = 1.0f;
     imodMatTransform3D(mat, &pnt, &yn);
     pnt.y = 0.0f;
     pnt.z = 1.0f;
     imodMatTransform3D(mat, &pnt, &zn);

     imodPointCross(&xn, &yn, &normal);
     pnt.x = 1.0f;
     pnt.y = pnt.z = 0.0f;
     imodPointCross(&normal, &pnt, &xcut);
     pnt.x = 0.0f;
     pnt.y = 1.0f;
     imodPointCross(&normal, &pnt, &ycut);
     pnt.y = 0.0f;
     pnt.z = 1.0f;
     imodPointCross(&normal, &pnt, &zcut);

     imodPointNormalize(&xcut);
     imodPointNormalize(&ycut);
     imodPointNormalize(&zcut);

     ss->xstep[X] = xn.x;
     ss->xstep[Y] = xn.y;
     ss->xstep[Z] = xn.z;
     ss->ystep[X] = yn.x;
     ss->ystep[Y] = yn.y;
     ss->ystep[Z] = yn.z;
     ss->zstep[X] = zn.x;
     ss->zstep[Y] = zn.y;
     ss->zstep[Z] = zn.z;
     
     /* Set cut points */
     ss->vi->slice.zx1 = ss->vi->slice.zx2 =
	  ss->vi->slice.yx1 = ss->vi->slice.yx2 = ss->vi->xmouse;
     ss->vi->slice.zy1 = ss->vi->slice.zy2 =
	  ss->vi->slice.xy1 = ss->vi->slice.xy2 = ss->vi->ymouse;
     ss->vi->slice.xz1 = ss->vi->slice.xz2 =
	  ss->vi->slice.yz1 = ss->vi->slice.yz2 = ss->vi->zmouse;

     if ((zcut.x) || (zcut.y)){
	  x = ss->vi->xmouse;
	  y = ss->vi->ymouse;
	  do{
	       x += zcut.x;
	       y += zcut.y;
	  }while((x < ss->vi->xsize) && (x > 0) &&
		 (y < ss->vi->ysize) && (y > 0));
	  ss->vi->slice.zx1 = x - zcut.x;
	  ss->vi->slice.zy1 = y - zcut.y;
	  x = ss->vi->xmouse;
	  y = ss->vi->ymouse;
	  do{
	       x -= zcut.x;
	       y -= zcut.y;
	  }while((x < ss->vi->xsize) && (x > 0) &&
		 (y < ss->vi->ysize) && (y > 0));
	  ss->vi->slice.zx2 = x + zcut.x;
	  ss->vi->slice.zy2 = y + zcut.y;
     }else{
	  ss->vi->slice.zx1 = ss->vi->slice.zy1 =
	        ss->vi->slice.zx1 = ss->vi->slice.zx2 = 0;
     }

     /* set yx1, yz1  and yx2, yz2 */
     if ((ycut.x) || (ycut.z)){
	  x = ss->vi->xmouse;
	  z = ss->vi->zmouse;
	  do{
	        x += ycut.x;
		z += ycut.z;
	  }while((x < ss->vi->xsize) && (x > 0) &&
		 (z < ss->vi->zsize) && (z > 0));
	  ss->vi->slice.yx1 = x - ycut.x;
	  ss->vi->slice.yz1 = z - ycut.z;
	  x = ss->vi->xmouse;
	  z = ss->vi->zmouse;
	  do{
	       x -= ycut.x;
	       z -= ycut.z;
	  }while((x < ss->vi->xsize) && (x > 0) &&
		 (z < ss->vi->zsize) && (z > 0));
	  ss->vi->slice.yx2 = x + ycut.x;
	  ss->vi->slice.yz2 = z + ycut.z;
     }else{
	  ss->vi->slice.yx1 = ss->vi->slice.yz1 =
	       ss->vi->slice.yx2 = ss->vi->slice.yz2 = 0;
     }

     /* set xy1, xz1  and xy2, xz2 */
     if ((xcut.y) || (xcut.z)){
	  y = ss->vi->ymouse;
	  z = ss->vi->zmouse;
	  do{
	       y += xcut.y;
	       z += xcut.z;
	  }while((y < ss->vi->ysize) && (y > 0) &&
		 (z < ss->vi->zsize) && (z > 0));
	  ss->vi->slice.xy1 = y - xcut.y;
	  ss->vi->slice.xz1 = z - xcut.z;
	  y = ss->vi->ymouse;
	  z = ss->vi->zmouse;
	  do{
	       y -= xcut.y;
	       z -= xcut.z;
	  }while((y < ss->vi->ysize) && (y > 0) &&
		 (z < ss->vi->zsize) && (z > 0));
	  ss->vi->slice.xy2 = y + xcut.y;
	  ss->vi->slice.xz2 = z + xcut.z;
     }else{
	  ss->vi->slice.xy1 = ss->vi->slice.xz1 =
	       ss->vi->slice.xy2 = ss->vi->slice.xz2 = 0;
     }


     /* set up starting image coord */
     ss->xo = ss->cx;
     ss->yo = ss->cy;
     ss->zo = ss->cz;

     isize = ss->winx / ss->zoom;
     jsize = ss->winy / ss->zoom;

     /* z stretch scale factor.   */
     zs  = ss->vi->imod->zscale; 
     if (zs <= 0.0f)
	  zs = 1.0f;
     zs = 1.0f / zs;
     if (ss->scalez != SLICE_ZSCALE_BEFORE)
	  zs = 1.0f;

     /* DNM: make these correct for HQ case at least */
     ss->xo -= (isize / 2) * ss->xstep[X];
     ss->yo -= (isize / 2) * ss->xstep[Y];
     ss->zo -= (isize / 2) * ss->xstep[Z] * zs;
     ss->zo -= (jsize / 2) * ss->ystep[Z] * zs;
     ss->xo -= (jsize / 2) * ss->ystep[X];
     ss->yo -= (jsize / 2) * ss->ystep[Y];

     return;
}

static void sslice_clear(struct Super_slicer *ss)
{
     b3dWinset(XtDisplay(ss->glw), ss->glw, ss->context);
     b3dClear();
     return;
}


/* DNM: routines to replace ivwGetValue for speedy access */

static int (*best_GetValue)(int x, int y, int z);

static int imdataxsize;
static int *vmdataxsize;
static unsigned char **imdata;
static int vmnullvalue;

static int idata_GetValue(int x, int y, int z)
{
     return(imdata[z][x + (y * imdataxsize)]);
}

static int cache_GetValue(int x, int y, int z)
{
     if (!imdata[z])
	  return(vmnullvalue);
     return(imdata[z][x + (y * vmdataxsize[z])]);
}

static int fake_GetValue(int x, int y, int z)
{
     return(0);
}

static void sslice_draw(struct Super_slicer *ss)
{
     int i, j, k, isize, jsize, ksize;
     int xi, yi, zi;
     unsigned short rbase;
     float xo, yo, zo;  /* coords of the lower left zap origin. */
     float xzo, yzo,zzo;
     float xs = 1.0f, ys = 1.0f, zs = 1.0f;  /* scale factors.  */
     float zoffset, alpha;
     float xsx, ysx, zsx; /* steps for moving x in zap. */
     float xsy, ysy, zsy; /* steps for moving y in zap. */
     float xsz, ysz, zsz; /* steps for moving z in zap. */

     /* for 3-D quadratic interpolation */
     float dx, dy, dz;
     float x1, x2, y1, y2, z1, z2;
     float a, b, c, d, e, f;
     float ival, sval, dval;
     int pxi, nxi, pyi, nyi, pzi, nzi;
     float maxval = 255.0f, minval = 0.0f;
     float xzoom, yzoom, zzoom;
     float x, y, z; /* coords of pixel in 3-D image block. */
     unsigned char val, noDataVal = 0;
     float zoom = ss->zoom;
     int iz, jz;
     float extrashift;
     int crossget, crosswant;
     Ipoint pnt, tpnt;
     Imat *mat = ss->mat;
     int ifill, jfill, deli, delj, joffset, yoffset, nyoffset, pyoffset;
     int izoom, shortcut, ilimshort, jlimshort, ishort;
     float cval;

     int cindex;
     unsigned int *cmap = App->cvi->cramp->ramp;
     B3dCIImage *image = ss->image;
     double avgval;
     unsigned short *cidata = ss->image->id1;
     unsigned char  *bdata = (unsigned char *)cidata;
     unsigned int   *idata = (unsigned int  *)cidata;
     int pixsize  = b3dGetImageType(NULL, NULL);

     if (!ss->exposed)
	  return;

     /* DNM 5/16/02: force a cache load of the current z slice at least */
     iz = floor((double)(ss->cz + 0.5));
     ivwGetZSection(ss->vi, iz);

     /* Set up image pointer tables */
     imdata = (unsigned char **)
	  malloc(sizeof(unsigned char *) * ss->vi->zsize);

     if (!imdata)
	  return;

     vmnullvalue = (App->cvi->white + App->cvi->black) / 2;
     noDataVal = vmnullvalue;
     if (ss->vi->fakeImage) {
	  best_GetValue = fake_GetValue;

     } else if (ss->vi->vmSize) {
	  /* For cached data, get pointers to data that exist at this time */
	  vmdataxsize = (int *)malloc(sizeof(int) * ss->vi->zsize);
	  if (!vmdataxsize)
	       return;
	  best_GetValue = cache_GetValue;
	  for (i = 0; i < ss->vi->zsize; i++)
	       imdata[i] = NULL;
	  for (i = 0; i < ss->vi->vmSize; i++) {
	       iz = ss->vi->vmCache[i].cz;
	       if (iz < ss->vi->zsize && iz >= 0 &&
		   ss->vi->vmCache[i].ct == ss->vi->ct){
		    imdata[iz] = ss->vi->vmCache[i].sec->data.b;
		    vmdataxsize[iz] = ss->vi->vmCache[i].sec->xsize;
	       }
	  }

     } else {
	  /* for loaded data, get pointers from ss->vi */
	  best_GetValue = idata_GetValue;
	  for (i = 0; i < ss->vi->zsize; i++)
	       imdata[i] = ss->vi->idata[i];
	  imdataxsize = ss->vi->xsize;
     }

     b3dWinset(XtDisplay(ss->glw), ss->glw, ss->context);

     slice_trans_step(ss);
     rbase = ss->vi->rampbase;
     if (!App->rgba && App->depth == 8){
	  minval = ss->vi->rampbase;
	  maxval = minval + ss->vi->rampsize;
	  noDataVal = minval;
     }

     ksize = ss->nslice;
     zoffset = (float)(ksize - 1) * 0.5;

     xzoom = yzoom = zzoom = ss->zoom;
     xo = ss->cx;
     yo = ss->cy;
     zo = ss->cz;

     xsx = ss->xstep[X];
     ysx = ss->xstep[Y];
     zsx = ss->xstep[Z];
     xsy = ss->ystep[X];
     ysy = ss->ystep[Y];
     zsy = ss->ystep[Z];
     xsz = ss->zstep[X];
     ysz = ss->zstep[Y];
     zsz = ss->zstep[Z];

     if ((ss->scalez) && (ss->vi->imod->zscale > 0))
          zs  = 1.0f/ss->vi->imod->zscale;

     if (ss->scalez == SLICE_ZSCALE_AFTER){
	  if (ss->vi->imod->zscale > 0)
	       zs = ss->vi->imod->zscale;
	  xzoom = zoom * sqrt((double)
			      ((xsx * xsx + ysx * ysx + zsx * zsx * zs * zs)/
			       (xsx * xsx + ysx * ysx + zsx * zsx)));
	  yzoom = zoom * sqrt((double)
			      ((xsy * xsy + ysy * ysy + zsy * zsy * zs * zs)/
			       (xsy * xsy + ysy * ysy + zsy * zsy)));
	  zzoom = zoom * sqrt((double)
			      ((xsz * xsz + ysz * ysz + zsz * zsz * zs * zs)/
			       (xsz * xsz + ysz * ysz + zsz * zsz)));
	  
	  xs = zoom / xzoom;
	  ys = zoom / yzoom;
	  zs = 1.0;
     }

     /* size of 2-D loop for i, j */
     /* DNM: don't use xzoom, yzoom; make pixels be zoom x zoom */
     isize = ss->winx / zoom + 0.9;
     jsize = ss->winy / zoom + 0.9;

     /* set up high res view workproc - for future development. */
     ss->xs = xs;
     ss->ys = ys;
     ss->yline = 0;

     if (ss->hq || (int)ss->zoom != ss->zoom){ 
	  /* high quality image or fractional zoom */
	  isize = ss->winx; /* calculate each pixel for zoom. */
	  jsize = ss->winy;
	  xsx /= xzoom;
	  ysx /= xzoom;
	  zsx /= xzoom / zs; 
	  xsy /= yzoom;
	  ysy /= yzoom;
	  zsy /= yzoom / zs;
	  xo -= (isize / 2) * xsx;
	  yo -= (isize / 2) * ysx;
	  zo -= (isize / 2) * zsx;
	  xo -= (jsize / 2) * xsy;
	  yo -= (jsize / 2) * ysy;
	  zo -= (jsize / 2) * zsy;
	  ss->xshift = 0;
	  ss->yshift = 0;
     }else{
	  xsx *= zoom / xzoom;
	  ysx *= zoom / xzoom;
	  zsx *= zs * zoom / xzoom;
	  xsy *= zoom / yzoom;
	  ysy *= zoom / yzoom;
	  zsy *= zs * zoom / yzoom;

	  /* Take fractional location of data point within a pixel and
	  rotate in 3D to find location in display pixel */

	  imodMatId(mat);
	  imodMatRot(mat, (double)ss->tang[Z], Z);
	  imodMatRot(mat, (double)ss->tang[Y], Y);
	  imodMatRot(mat, (double)ss->tang[X], X); 

	  pnt.x = ss->cx - (int)ss->cx;
	  pnt.y = ss->cy - (int)ss->cy;
	  pnt.z = ss->cz - (int)ss->cz;
	  imodMatTransform3D(mat, &pnt, &tpnt);
	  
	  if (tpnt.x < 0.0)
	       tpnt.x += 1.0;
	  if (tpnt.y < 0.0)
	       tpnt.y += 1.0;

	  /* Compute where we want the crosshair to come out in the central
	     pixel, and where it will fall with no raster offset, use 
	     difference to set raster offset */

	  crosswant = zoom * tpnt.x;    /* don't take nearest int here! */
	  if (crosswant >= zoom)
	       crosswant -= zoom;
	  crossget = (ss->winx / 2) % (int)zoom;
	  ss->xshift = crossget - crosswant;
	  if (ss->xshift < 0)
	       ss->xshift += zoom;

	  crosswant = zoom * tpnt.y;
	  if (crosswant >= zoom)
	       crosswant -= zoom;
	  crossget = (ss->winy / 2) % (int)zoom;
	  ss->yshift = crossget - crosswant;
	  if (ss->yshift < 0)
	       ss->yshift += zoom;

	  extrashift = 0.5;      /* Needed for proper sampling */
	  if (zoom == 1.0)
	       extrashift = 0.0;

	  xo -= ((ss->winx / 2 - ss->xshift) / zoom - extrashift) * xsx;
	  yo -= ((ss->winx / 2 - ss->xshift) / zoom - extrashift) * ysx;
	  zo -= ((ss->winx / 2 - ss->xshift) / zoom - extrashift) * zsx;
	  xo -= ((ss->winy / 2 - ss->yshift) / zoom - extrashift) * xsy;
	  yo -= ((ss->winy / 2 - ss->yshift) / zoom - extrashift) * ysy;
	  zo -= ((ss->winy / 2 - ss->yshift) / zoom - extrashift) * zsy;
     }

     /* steps per step in Z are independent of HQ versus regular */
     xsz *= zoom / zzoom;
     ysz *= zoom / zzoom;
     zsz *= zs * zoom / zzoom;

     /* Save values of starting position */
     ss->xo = xo;
     ss->yo = yo;
     ss->zo = zo;

     /* Adjust for multiple slices */
     xo -= zoffset * xsz;
     yo -= zoffset * ysz;
     zo -= zoffset * zsz;
     xzo = xo; yzo = yo; zzo = zo;

     shortcut = 0;
     izoom = (int) zoom;
     if ((ss->hq != 0) && (ss->fasthq != 0) && (zoom == izoom) && 
	 (zoom > 1.0)) {
	  shortcut = 1;
	  ilimshort = izoom * ((isize - 1) / izoom - 1);
	  jlimshort = izoom * ((jsize - 1) / izoom - 1);
     } else if (!izoom) {
	  /* DNM 11/22/01: workaround to Intel compiler bug - it insists on
	   doing j % izoom even when shortcut is 0 */ 
	  izoom = 1;
     }

     /* DNM: don't need to clear array in advance */

     for(k = 0; k < ksize; k++){
	  xo = xzo;
	  yo = yzo;
	  zo = zzo;
	  
	  /* (i,j) location in zap window data. */
	  for(j = 0; j < jsize; j++){
	       x = xo;
	       y = yo;
	       z = zo;
	       cindex = j * ss->winx;
	       for(i = 0; i < isize; i++){
		    xi = floor((double)x);
		    yi = floor((double)y);
		    zi = floor((double)(z + 0.5));
		    
		    if ((xi >= 0) && (xi < ss->vi->xsize) &&
			(yi >= 0) && (yi < ss->vi->ysize) &&
			(zi >= 0) && (zi < ss->vi->zsize)){
			 val = (*best_GetValue)(xi, yi, zi);
			 
			 if (ss->hq){ /* do quadratic interpolation. */
			      dx = x - xi - 0.5;
			      dy = y - yi - 0.5;
			      dz = z - zi;
			      
			      pxi = xi - 1;
			      nxi = xi + 1;
			      pyi = yi - 1;
			      nyi = yi + 1;
			      pzi = zi - 1;
			      nzi = zi + 1;
			      
			      if (pxi < 0) pxi = 0;
			      if (nxi >= ss->vi->xsize) nxi = xi;
			      if (pyi < 0) pyi = 0;
			      if (nyi >= ss->vi->ysize) nyi = yi;
			      if (pzi < 0) pzi = 0;
			      if (nzi >= ss->vi->zsize) nzi = zi;
			      
			      x1 = (*best_GetValue)(pxi,  yi,  zi);
			      x2 = (*best_GetValue)(nxi,  yi,  zi);
			      y1 = (*best_GetValue)( xi, pyi,  zi);
			      y2 = (*best_GetValue)( xi, nyi,  zi);
			      z1 = (*best_GetValue)( xi,  yi, pzi);
			      z2 = (*best_GetValue)( xi,  yi, nzi);
			      
			      a = (x1 + x2) * 0.5f - (float)val;
			      b = (y1 + y2) * 0.5f - (float)val;
			      c = (z1 + z2) * 0.5f - (float)val;
			      d = (x2 - x1) * 0.5f;
			      e = (y2 - y1) * 0.5f;
			      f = (z2 - z1) * 0.5f;
			      ival = (a * dx * dx) + 
				     (b * dy * dy) + 
				     (c * dz * dz) +
				     (d * dx) + (e * dy) + 
				     (f * dz) + (float)val;
			      if (ival > maxval)
				   ival = maxval;
			      if (ival < minval)
				   ival = minval;
			      val = ival + 0.5f;
			      
			 }
		    }
		    else
			 val = noDataVal;
		    
		    if (k)
			 cidata[i + cindex] += val;
		    else
			 cidata[i + cindex] = val;
		    
		    x += xsx;
		    y += ysx;
		    z += zsx;

		    if (shortcut != 0 && ((i >= izoom && j % izoom == 0) ||
					  (i >= izoom - 1 && j % izoom != 0))
			&& i < ilimshort && j >= izoom && j < jlimshort) {
			 ishort = izoom - 1;
			 if (j % izoom)
			      ishort = ilimshort - izoom;
			 x += xsx * ishort;
			 y += ysx * ishort;
			 z += zsx * ishort;
			 i += ishort;
		    }
	       }
	       xo += xsy;
	       yo += ysy;
	       zo += zsy;

	  }
	  xzo += xsz;
	  yzo += ysz;
	  zzo += zsz;
     }

     if (shortcut) {

	  minval *= ss->nslice;
	  maxval *= ss->nslice;
	  /* Quadratic interpolation code */
	  /*
	  for (jfill = 0; jfill < izoom; jfill++) {
	       dy = jfill / zoom;
	       delj = -jfill;
	       if (dy > 0.5) {
		    dy = dy - 1.0;
		    delj = izoom - jfill;
	       }
	       ifill = 0;
	       if (jfill == 0)
		    ifill = 1;
	       for (; ifill < izoom; ifill++) {
		    dx = ifill / zoom;
		    deli = -ifill;
		    if (dx > 0.5) {
			 dx = dx - 1.0;
			 deli = izoom - ifill;
		    }

		    for (j = izoom + jfill; j < jlimshort; j += izoom) {
			 joffset = j * ss->winx;
			 yi = j + delj;
			 yoffset = yi * ss->winx;
			 nyoffset = (yi + izoom) * ss->winx;
			 pyoffset = (yi - izoom) * ss->winx;

			 for (i = izoom + ifill; i < ilimshort; i += izoom) {
			      xi = i + deli;
			      cval = cidata[xi + yoffset];
			      x1 = cidata[xi - izoom + yoffset];
			      x2 = cidata[xi + izoom + yoffset];
			      y1 = cidata[xi  + pyoffset];
			      y2 = cidata[xi  + nyoffset];
			 
			      a = (x1 + x2) * 0.5f - cval;
			      b = (y1 + y2) * 0.5f - cval;
			      c = (x2 - x1) * 0.5f;
			      d = (y2 - y1) * 0.5f;
			 
			      ival = (a * dx * dx) + (b * dy * dy) +
				   (c * dx) + (d * dy) + cval;
			      if (ival > maxval)
				   ival = maxval;
			      if (ival < minval)
			      ival = minval;
			      cidata[i + joffset] = ival + 0.5f;

			 }
		    }
	       }
	  }
	  */

	  /* cubic convolution interpolation looks much better at high zoom */

	  for (jfill = 0; jfill < izoom; jfill++) {
	       int xn, xn2, xp, ynoffset, yn2offset, ypoffset;
	       float dysq, dycub, fyp, fy, fyn, fyn2;
	       float dxsq, dxcub, fxp, fx, fxn, fxn2;
	       float yp, y0, yn, yn2;
	       dy = jfill / zoom;
	       delj = -jfill;
	       ifill = 0;
	       dysq = dy * dy;
	       dycub = dy * dysq;
	       fyp = 2 * dysq - dycub - dy;
	       fy = 1 + dycub - 2 * dysq;
	       fyn = dy + dysq - dycub;
	       fyn2 = dycub - dysq;
	       if (jfill == 0)
		    ifill = 1;
	       for (; ifill < izoom; ifill++) {
		    dx = ifill / zoom;
		    deli = -ifill;
		    dxsq = dx * dx;
		    dxcub = dx * dxsq;
		    fxp = 2 * dxsq - dxcub - dx;
		    fx = 1 + dxcub - 2 * dxsq;
		    fxn = dx + dxsq - dxcub;
		    fxn2 = dxcub - dxsq;

		    for (j = izoom + jfill; j < jlimshort; j += izoom) {
			 joffset = j * ss->winx;
			 yi = j + delj;
			 yoffset = yi * ss->winx;
			 ynoffset = (yi + izoom) * ss->winx;
			 yn2offset = (yi + 2 * izoom) * ss->winx;
			 ypoffset = (yi - izoom) * ss->winx;

			 for (i = izoom + ifill; i < ilimshort; i += izoom) {
			      xi = i + deli;
			      xn = xi + izoom;
			      xn2 = xn + izoom;
			      xp = xi - izoom;
			      yp = fxp * cidata[xp + ypoffset] +
				   fx * cidata[xi + ypoffset] +
				   fxn * cidata[xn + ypoffset] +
				   fxn2 * cidata[xn2 + ypoffset];
			      y0 = fxp * cidata[xp + yoffset] +
				   fx * cidata[xi + yoffset] +
				   fxn * cidata[xn + yoffset] +
				   fxn2 * cidata[xn2 + yoffset];
			      yn = fxp * cidata[xp + ynoffset] +
				   fx * cidata[xi + ynoffset] +
				   fxn * cidata[xn + ynoffset] +
				   fxn2 * cidata[xn2 + ynoffset];
			      yn2 = fxp * cidata[xp + yn2offset] +
				   fx * cidata[xi + yn2offset] +
				   fxn * cidata[xn + yn2offset] +
				   fxn2 * cidata[xn2 + yn2offset];
			      ival = fyp * yp + fy * y0 + 
				   fyn * yn + fyn2 * yn2;
			 
			      if (ival > maxval)
				   ival = maxval;
			      if (ival < minval)
				   ival = minval;
			      cidata[i + joffset] = ival + 0.5f;

			 }
		    }
	       }
	  }

     }

     cindex = ss->image->width * ss->image->height;
     k = ss->nslice;

     /* for 8-bit displays, range is less then 256 gray scales. */
     if (!App->rgba && App->depth == 8){
	  int tval;
	  int minval = ss->vi->rampbase;
	  int maxval = minval + ss->vi->rampsize;
	  if (k)
	       for (j = 0; j < jsize; j++)
		    for(i = j * ss->winx; i < j * ss->winx + isize; i++){
			 tval = cidata[i]/k;
			 if (tval > maxval) tval = maxval;
			 if (tval < minval) tval = minval;
			 cidata[i] = tval;
		    }
	  else
	       for (j = 0; j < jsize; j++)
		    for(i = j * ss->winx; i < j * ss->winx + isize; i++){
			 if (cidata[i] > maxval) cidata[i] = maxval;
			 if (cidata[i] < minval) cidata[i] = minval;
		    }

     }else{
	 switch (pixsize){
	   case 1:
	     if (k)
		  for (j = 0; j < jsize; j++)
		       for(i = j * ss->winx; i < j * ss->winx + isize; i++){
			    cidata[i] = (cidata[i]/k);
		       }
	     break;
	   case 2:
	     if (k)
		  for (j = 0; j < jsize; j++)
		       for(i = j * ss->winx; i < j * ss->winx + isize; i++){
			    cidata[i] = (cidata[i]/k)+ rbase;
		       }
	     else
		  for (j = 0; j < jsize; j++)
		       for(i = j * ss->winx; i < j * ss->winx + isize; i++){
			    cidata[i] = cidata[i]+ rbase;
		       }
	     break;
	   case 4:
	     if (k)
		  for (j = jsize - 1; j >= 0; j--)
		       for(i = j * ss->winx + isize - 1; i >= j * ss->winx;
			   i--){
			    idata[i] = cmap[(cidata[i]/k)];
		       }
	     else
		  for (j = jsize - 1; j >= 0; j--)
		       for(i = j * ss->winx + isize - 1; i >= j * ss->winx;
			   i--){
			    idata[i] = cmap[cidata[i]];
		       }
	 }
     }

     ss->xzoom = xzoom;
     ss->yzoom = yzoom;
     slicerUpdateImage(ss);
     free(imdata);
     if (ss->vi->vmSize)
	  free(vmdataxsize);
     return;
}


static void slicerUpdateImage(struct Super_slicer *win)
{
    GLenum format = GL_COLOR_INDEX;
    GLenum type   = GL_UNSIGNED_SHORT;
    GLint unpack = b3dGetImageType(&type, &format);
    if (unpack == 1) {
	 unpack = 2;
	 format = GL_COLOR_INDEX;
	 type   = GL_UNSIGNED_SHORT;
    }

    glPixelStorei(GL_UNPACK_ALIGNMENT, unpack);

    /* Just clear the unused edges if there are shifts */
    if (win->xshift || win->yshift) {
	 b3dColorIndex(App->background);
	 b3dDrawFilledRectangle(0, 0, win->winx, win->yshift);
	 b3dDrawFilledRectangle(0, 0, win->xshift, win->winy);
    }

    /* DNM: one-to-one image for fractional zoom as well as in hq case */
    if (win->hq || win->zoom != (int)win->zoom)
	 glPixelZoom(1.0f, 1.0f);
    else
	 /* DNM: don't make this xzoom, yzoom.  */
	 glPixelZoom(win->zoom, win->zoom);

    glRasterPos2f(win->xshift, win->yshift);
    glDrawPixels(win->winx, win->winy, format, type, win->image->id1);
    

     /* Position of cursor. */
     b3dColorIndex(App->endpoint);
     if (win->vi->drawcursor)
	  b3dDrawPlus((int)win->winx * 0.5f,
		      (int)win->winy * 0.5f, 5);

     sslice_draw_model(win);
     b3dSwapBuffers();
     sslice_cube_draw(win);
     return;

}

static void sslice_draw_model(struct Super_slicer *ss)
{
     float depth = ss->depth;

     depth = depth * ss->zoom;
     depth *= 0.5f;

     glMatrixMode(GL_PROJECTION);
     
     glLoadIdentity();

     glOrtho(0.0 , ss->winx, 0.0, ss->winy, depth, -depth);

     glMatrixMode(GL_MODELVIEW);
     glPushMatrix();


     glTranslatef(ss->winx*0.5f, ss->winy*0.5f, 0.0f);
     /*     if (ss->scalez == SLICE_ZSCALE_AFTER)
	    glScalef(1.0f, 1.0f, ss->vi->imod->zscale); */
     glScalef(ss->xzoom, ss->yzoom, ss->zoom);

     /* DNM: took away minus signs because inverting sense of angles;
	also had to swap X and Y */
     glRotatef(ss->tang[X], 1.0f, 0.0f, 0.0f);
     glRotatef(ss->tang[Y], 0.0f, 1.0f, 0.0f);
     glRotatef(ss->tang[Z], 0.0f, 0.0f, 1.0f);
     if (ss->scalez == SLICE_ZSCALE_BEFORE)
	  glScalef(1.0f, 1.0f, ss->vi->imod->zscale);

     glTranslatef(-ss->cx, -ss->cy, -ss->cz);

     imodDrawModel(ss->vi->imod);
     glPopMatrix();
     return;
}

static void sslice_cube_draw(struct Super_slicer *ss)
{
     double params[4];
     static float v[3], vx[3], vy[3];
     double x, y, z;
     double r;
     float zs  = ss->vi->imod->zscale;
     float zoom = 1.0/ss->zoom;
     int winx, winy;
     float xo, yo, zo;
     if (!ss->cubegc)
	  return;

     b3dWinset(XtDisplay(ss->cube), ss->cube, ss->cubegc);

     glClearColor(0, 0, 0, 0);
     glClear(GL_COLOR_BUFFER_BIT);
     
     x = ss->vi->xsize;
     y = ss->vi->ysize;
     z = ss->vi->zsize;

     /* DNM: take window size * zoom as # of pixels displayed */
     xo = ss->xo; yo = ss->yo, zo = ss->zo;
     winx = ss->winx * zoom;
     winy = ss->winy * zoom;
     r = sqrt( (x * x) + (y * y) + (z * z));

     glMatrixMode(GL_PROJECTION);
     glLoadIdentity();

     /* DNM: was -r, r for near, far!  These ratios seem to work */
     gluPerspective(45.0, 1., .02*r, 10.*r);
     glMatrixMode(GL_MODELVIEW);
     glLoadIdentity();
     gluLookAt(x*0.5,  -r,  r*1.5,
	       x*0.5, y*0.5, z*0.5, 
	       0.0, 0.0, 1.0);


     /* draw the cube */
     glColor4ub(0x00, 0xff, 0xff, 0x00);
     glBegin(GL_LINE_LOOP);
     glVertex3f(0.0, 0.0, 0.0);
     glVertex3f(  x, 0.0, 0.0);
     glVertex3f(  x,   y, 0.0);
     glVertex3f(0.0,   y, 0.0);
     glEnd();

     glBegin(GL_LINE_LOOP);
     glVertex3f(0.0, 0.0, z);
     glVertex3f(  x, 0.0, z);
     glVertex3f(  x,   y, z);
     glVertex3f(0.0,   y, z);
     glEnd();

     glBegin(GL_LINES);
     glVertex3f(0.0, 0.0, 0.0);
     glVertex3f(0.0, 0.0,   z);
     glVertex3f(0.0,   y, 0.0);
     glVertex3f(0.0,   y,   z);
     glVertex3f(  x, 0.0, 0.0);
     glVertex3f(  x, 0.0,   z);
     glVertex3f(  x,   y, 0.0);
     glVertex3f(  x,   y,   z);
     glEnd();

     params[0] = 1.0; params[1] = 0.0; params[2] = 0.0; params[3] = 0.0;
     glClipPlane( GL_CLIP_PLANE0, params);
     params[0] = 0.0; params[1] = 1.0; params[2] = 0.0; params[3] = 0.0;
     glClipPlane( GL_CLIP_PLANE1, params);
     params[0] = 0.0; params[1] = 0.0; params[2] = 1.0; params[3] = 0.0;
     glClipPlane( GL_CLIP_PLANE2, params);
     params[0] = -1.0; params[1] = 0.0; params[2] = 0.0; params[3] = x+1;
     glClipPlane( GL_CLIP_PLANE3, params);
     params[0] = 0.0; params[1] = -1.0; params[2] = 0.0; params[3] = y+1;
     glClipPlane( GL_CLIP_PLANE4, params);
     params[0] = 0.0; params[1] = 0.0; params[2] = -1.0; params[3] = z+1;
     glClipPlane( GL_CLIP_PLANE5, params);
     
     vx[0] = xo + (ss->xstep[0] * winx);
     vx[1] = yo + (ss->xstep[1] * winx);
     vx[2] = zo + (ss->xstep[2] * winx * zs);

     vy[0] = xo + (ss->ystep[0] * winy);
     vy[1] = yo + (ss->ystep[1] * winy);
     vy[2] = zo + (ss->ystep[2] * winy * zs);

     v[0]  = xo + (ss->xstep[0] * winx) 
	  + (ss->ystep[0] * winy);
     v[1]  = yo + (ss->xstep[1] * winx) 
	  + (ss->ystep[1] * winy);
     v[2]  = zo + (ss->xstep[2] * winx * zs) 
	  + (ss->ystep[2] * winy * zs);

     glBlendFunc(GL_SRC_ALPHA,  GL_ONE_MINUS_SRC_ALPHA); 
     glEnable(GL_BLEND); 

     glColor4ub(0xff, 0x00, 0x00, 0x7f);
     glEnable(GL_CLIP_PLANE0);
     glEnable(GL_CLIP_PLANE1);
     glEnable(GL_CLIP_PLANE2);
     glEnable(GL_CLIP_PLANE3);
     glEnable(GL_CLIP_PLANE4);
     glEnable(GL_CLIP_PLANE5);

     glBegin(GL_POLYGON);
     glVertex3f(xo, yo, zo);
     glVertex3fv(vx);
     glVertex3fv(v);
     glVertex3fv(vy);
     glEnd();

     glBlendFunc(GL_ONE,  GL_ZERO);
     glDisable(GL_BLEND);
     glDisable(GL_CLIP_PLANE0);
     glDisable(GL_CLIP_PLANE1);
     glDisable(GL_CLIP_PLANE2);
     glDisable(GL_CLIP_PLANE3);
     glDisable(GL_CLIP_PLANE4);
     glDisable(GL_CLIP_PLANE5);

     glFlush();

     return;
}
