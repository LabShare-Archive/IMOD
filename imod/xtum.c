/*  IMOD VERSION 2.41
 *
 *  xtum.c -- Open the tumbler window.
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

#include <Xm/MainW.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/DrawingA.h>
#include <Xm/Form.h>
#include <Xm/ArrowB.h>
#include <Xm/RowColumn.h>
#include <X11/keysym.h>
#include <Xm/VirtKeys.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <math.h>
#include <string.h>

#include <mrcc.h>
#include "imod.h"

#ifdef DRAW_GL
long Tumble_Window;
long Stum_window;
int imod_stum_input(Device dev, short val){return(0);}
int imod_tumble_input(Device dev, short val){return(0);}
#endif

#define button_width  16
#define button_height 16

static unsigned char lock_bits[] = {
     0xf8, 0x0f, 0x0c, 0x18, 0x06, 0x30, 0x03, 0x60, 0x03, 0x60, 0x03, 0x60,
     0x03, 0x60, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f,
     0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0x7f, 0xff, 0x7f};

static unsigned char unlock_bits[] = {
     0xf8, 0x0f, 0x08, 0x18, 0x00, 0x30, 0x00, 0x60, 0x00, 0x60, 0x00, 0x60,
     0x00, 0x60, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0x7f, 0x7f, 0x7f, 0x7f,
     0x7f, 0x7f, 0x7f, 0x7f, 0xff, 0x7f, 0xff, 0x7f};

static unsigned char lowres_bits[] = {
     0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0x0f, 0x0f, 0x0f, 0x0f,
     0x0f, 0x0f, 0x0f, 0x0f, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0, 0xf0,
     0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f, 0x0f};

static unsigned char highres_bits[] = {
     0xcc, 0xcc, 0xcc, 0xcc, 0x33, 0x33, 0x33, 0x33, 0xcc, 0xcc, 0xcc, 0xcc,
     0x33, 0x33, 0x33, 0x33, 0xcc, 0xcc, 0xcc, 0xcc, 0x33, 0x33, 0x33, 0x33,
     0xcc, 0xcc, 0xcc, 0xcc, 0x33, 0x33, 0x33, 0x33};

struct imod_xtum_struct
{
     struct ViewInfo *vi;

     Widget        dialog;
     Widget        gfx;
     XtWorkProcId  work_id;
     XtIntervalId  iid;
     XtAppContext  app;
     XID           context;
     Pixmap        lockpix;
     Pixmap        unlockpix;
     Pixmap        lowrespix;
     Pixmap        highrespix;
     B3dCIImage   *image;

     float  scale, offset;
     float  scalef, offsetf;
     float  alpha, beta, gamma;
     float  plax;
     Ipoint xstep, ystep, zstep;
     Ipoint sxstep, systep, szstep;

     Islice *slice;
     Islice *stslice;
     Islice *bwslice;
     Islice *count;

     int    exposed;       /* drawing area first exposure flag. */
     int    width, height; /* drawing area size. */
     int    zoom;
     int    cx, cy, cz;
     int    nx, ny, nz;
     int    ms;            /* max size, diameter of binding sphere. */
     int    highres;
     int    locked;
     int    stereo;
     int    bbox;
     int    firstscale;
     int    black, white;   /* black and white levels of last display */
     int    falsecolor;     /* falsecolor state of last display */
};

void xtumNewData(struct imod_xtum_struct *xtum);
void xtumDraw( struct imod_xtum_struct *xtum);

static void xtumSetSlice(struct imod_xtum_struct *xtum);
static void xtumDrawBoxLines(struct imod_xtum_struct *xtum, Imat *mat);
static void xtumFillSlice(struct imod_xtum_struct *xtum);
static void xtumFillASlice(struct imod_xtum_struct *xtum);
static void xtumFillBSlice(struct imod_xtum_struct *xtum);
static void xtumScaleData(struct imod_xtum_struct *xtum);
static void tupdate(XtPointer client, XtIntervalId *id);
static Boolean work_update(XtPointer client);
static void addworkproc(XtPointer client, XtIntervalId *id);


int imod_tumble_open(struct ViewInfo *vi)
{
     xtumOpen(vi);
     return(-1);
}

static void help_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtum_struct *xtum = (struct imod_xtum_struct *)client;
     dia_vasmsg("Tumbler Window Help\n",
		"---------------------------------------------------------\n",
		"\nToolbar Controls:\n",
		"\tThe + and - control the zoom factor.",
		"\tThe checkerboard toggles between low/high resolution ",
		"rendering.\n",
		"\nKeybord Commands:\n",
		"\ts\tToggle stereo mode.\n",
		"\tr\tRescale default contrast/brightness.\n",
		"\t-/=\tDecrease/Increase zoom factor.\n",
		"\tArrows\tTilt cube.\n",
		"\tF5/F6 Decrease/Increase contrast.\n",
		"\tF6/F7 Decrease/Increase brightness.\n",
		NULL);
     XmProcessTraversal(xtum->gfx, XmTRAVERSE_CURRENT);
     return;
}

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtum_struct *xtum = (struct imod_xtum_struct *)client;
     if (!xtum)
	  return;
     sliceFree(xtum->slice);
     sliceFree(xtum->stslice);
     sliceFree(xtum->bwslice);
#ifdef XTUM_DIVSCALE
     sliceFree(xtum->count);
#endif
     b3dFreeCIImage(xtum->image);

     if (xtum->iid)
	  XtRemoveTimeOut(xtum->iid);
     if (xtum->work_id)
	  XtRemoveWorkProc(xtum->work_id);

     b3dWinset(XtDisplay(xtum->gfx), 0, 0);
     XtPopdown(xtum->dialog);
     XtDestroyWidget(xtum->dialog);
     free(xtum);
     return;
}

static void ginit_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtum_struct *xtum = (struct imod_xtum_struct *)client;

     if (xtum->exposed) return;
     xtum->context = b3dGetContext(xtum->gfx);
     b3dWinset(XtDisplay(xtum->gfx), xtum->gfx, xtum->context);
     b3dResizeViewport();
     xtum->image   = b3dGetNewCIImage(xtum->image, App->depth);
     xtumSetSlice(xtum);
     xtum->cx = xtum->vi->xmouse;
     xtum->cy = xtum->vi->ymouse;
     xtum->cz = xtum->vi->zmouse;
     xtum->black = xtum->vi->black;
     xtum->white = xtum->vi->white;
     xtum->falsecolor = xtum->vi->cramp->falsecolor;
     XmProcessTraversal(xtum->gfx, XmTRAVERSE_CURRENT);
     xtum->iid = XtAppAddTimeOut
	    (xtum->app, 3000L, tupdate, (XtPointer)xtum);
     return;
}

static void expose_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtum_struct *xtum = (struct imod_xtum_struct *)client;
     Dimension winx, winy;
     int ow, oh;

     if (!xtum->exposed){
	  XtVaGetValues(w, XmNwidth, &winx, XmNheight, &winy, NULL);
	  xtum->width  = winx;
	  xtum->height = winy;
	  ginit_cb(w, client, call);
     }
     XtVaGetValues(w, XmNwidth, &winx, XmNheight, &winy, NULL);
     ow = xtum->width; oh = xtum->height;
     xtum->width  = winx;
     xtum->height = winy;
     b3dWinset(XtDisplay(xtum->gfx), xtum->gfx, xtum->context);
     b3dResizeViewport();
     if ( (ow != winx) || (oh != winy))
	  xtum->image   = b3dGetNewCIImage(xtum->image, App->depth);
     xtum->exposed = True;
     xtumDraw(xtum);
     return;
}

static void resize_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtum_struct *xtum = (struct imod_xtum_struct *)client;
     Dimension winx, winy;
     
     XtVaGetValues(w, XmNwidth, &winx, XmNheight, &winy, NULL);
     xtum->width  = winx;
     xtum->height = winy;
     if (xtum->exposed){
	  b3dWinset(XtDisplay(xtum->gfx), xtum->gfx, xtum->context);
	  b3dResizeViewport();
	  xtum->image   = b3dGetNewCIImage(xtum->image, App->depth);
	  xtumSetSlice(xtum);
	  xtumDraw(xtum);
     }
     return;
}

static void input_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtum_struct *xtum = (struct imod_xtum_struct *)client;
     B3dDrawingAreaCallbackStruct *cbs = (B3dDrawingAreaCallbackStruct *)call;
     int charcount;
     char buffer[1];
     int bufsize = 1;
     KeySym keysym;
     float tstep = 2;
     switch(cbs->event->type){
	  
	case KeyPress:
	  keysym = XLookupKeysym((XKeyEvent *)cbs->event, 0);
	  switch(keysym){
	     case XK_b:
	       if (xtum->bbox)
		    xtum->bbox = 0;
	       else
		    xtum->bbox = 1;
	       break;
	       
	     case XK_s:
	       if ((cbs->event->xbutton.state & ShiftMask) || 
		   (cbs->event->xbutton.state & ControlMask)){
		   if (cbs->event->xbutton.state & ShiftMask){
		       b3dWinset(XtDisplay(xtum->gfx), 
				 xtum->gfx, xtum->context);
		       xtumDraw(xtum);
		       if (cbs->event->xbutton.state & ShiftMask)
			   b3dAutoSnapshot("tumbler", SnapShot_RGB, NULL);
		       else if (cbs->event->xbutton.state & ControlMask)
			   b3dAutoSnapshot("tumbler", SnapShot_TIF, NULL);
		   }
	       }else{
		   if (xtum->stereo)
		       xtum->stereo = 0;
		    else
			 xtum->stereo = 1;
		    xtumSetSlice(xtum);
	       }
	       break;


	     case XK_r:
	       xtum->scalef = 1.0;
	       xtum->offsetf = 0.0;
	       xtumScaleData(xtum);
	       break;

	     case XK_equal:
	       xtum->zoom += 1.0;
	       break;
	     case XK_minus:
	       xtum->zoom -= 1.0;
	       if (xtum->zoom < 1.0)
		    xtum->zoom = 1.0;
	       break;
	       
	     case XK_Up:
	       xtum->alpha += tstep;
	       xtumNewData(xtum);
	       break;
	     case XK_Down:
	       xtum->alpha -= tstep;
	       xtumNewData(xtum);
	       break;
	     case XK_Right:
	       xtum->beta += tstep;
	       xtumNewData(xtum);
	       break;
	     case XK_Left:
	       xtum->beta -= tstep;
	       xtumNewData(xtum);
	       break;
	       
	     case XK_F5:
	       xtum->scalef /= 1.1f;
	       break;
	     case XK_F6:
	       xtum->scalef *= 1.1f;
	       break;
	     case XK_F7:
	       xtum->offsetf -= 50;
	       break;
	     case XK_F8:
	       xtum->offsetf += 50;
	       break;
	       
	     default:
	       return;
	  }
	  xtumDraw(xtum);
	  
	  break;
	  
	case ButtonPress:
	  XmProcessTraversal(xtum->gfx, XmTRAVERSE_CURRENT);
	  if (cbs->event->xbutton.button == 1)
	       ;
	  break;
	  
     }
     return;
}

static void zoomup_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtum_struct *xtum = (struct imod_xtum_struct *)client;
     xtum->zoom++;
     xtumDraw(xtum);
     XmProcessTraversal(xtum->gfx, XmTRAVERSE_CURRENT);
     return;
}

static void zoomdown_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtum_struct *xtum = (struct imod_xtum_struct *)client;
     xtum->zoom--;
     if (!xtum->zoom)
	  xtum->zoom = 1;
     xtumDraw(xtum);
     XmProcessTraversal(xtum->gfx, XmTRAVERSE_CURRENT);
     return;
}

static void lock_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtum_struct *xtum = (struct imod_xtum_struct *)client;
     
     if (xtum->locked){
	  xtum->locked = False;
	  XtVaSetValues(w, XmNlabelPixmap, xtum->unlockpix, NULL);
     }else{
	  xtum->locked = True;
	  XtVaSetValues(w, XmNlabelPixmap, xtum->lockpix, NULL);
     }
     XmProcessTraversal(xtum->gfx, XmTRAVERSE_CURRENT);
     return;
}

static void xtumSetSlice(struct imod_xtum_struct *xtum)
{
     int xsize, ysize;
     if (xtum->slice)
	  sliceFree(xtum->slice);
     if (xtum->stslice)
	  sliceFree(xtum->stslice);
     if (xtum->bwslice)
	  sliceFree(xtum->bwslice);
#ifdef XTUM_DIVSCALE
     if (xtum->count)
	  sliceFree(xtum->count);
#endif
     if (xtum->highres){
	  xsize = xtum->width;
	  ysize = xtum->height;
	  if (xtum->stereo)
	       xsize /= 2;
     }else{
	  xsize = ysize = xtum->ms;
     }
     xtum->slice   = sliceCreate(xsize, ysize, SLICE_MODE_FLOAT);
     xtum->stslice = sliceCreate(xsize, ysize, SLICE_MODE_FLOAT);
     xtum->bwslice = sliceCreate(xsize, ysize, SLICE_MODE_BYTE);
#ifdef XTUM_DIVSCALE
     xtum->count   = sliceCreate(xsize, ysize, SLICE_MODE_SHORT);
#endif
     xtumNewData(xtum);
     return;
}

static void res_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xtum_struct *xtum = (struct imod_xtum_struct *)client;
     
     if (xtum->highres){
	  xtum->highres = False;
	  XtVaSetValues(w, XmNlabelPixmap, xtum->lowrespix, NULL);
	  xtum->scalef /= 0.71f;
     }else{
	  xtum->highres = True;
	  XtVaSetValues(w, XmNlabelPixmap, xtum->highrespix, NULL);
	  xtum->scalef *= 0.71f;
     }

     xtumSetSlice(xtum);
     xtumDraw(xtum);
     XmProcessTraversal(xtum->gfx, XmTRAVERSE_CURRENT);
     return;
}

int xtumOpen(struct ViewInfo *vi)
{
     struct imod_xtum_struct *xtum;
     char *window_name;
     Widget mw, frame, form, tools;
     Widget row, button;
     Atom            wmclose;
     Pixel           fg, bg;
     int             depth;
     
     
     xtum = (struct imod_xtum_struct *)malloc
	  (sizeof(struct imod_xtum_struct));
     if (!xtum)
	  return(-1);

     xtum->vi      = vi;
     xtum->exposed = False;
     xtum->width   = 256;
     xtum->height  = 256;
     xtum->zoom    = 1;
     xtum->image   = NULL;
     xtum->slice = xtum->stslice = xtum->bwslice = xtum->count = NULL;
     xtum->work_id = 0;
     xtum->iid     = 0;
     xtum->app     = App->context;
     xtum->highres = 0;
     xtum->nx = xtum->ny = xtum->nz = 32;
     xtum->cx = vi->xmouse;
     xtum->cy = vi->ymouse;
     xtum->cz = vi->zmouse;
     xtum->ms = 56;
/*     xtum->scale = 1.0f; */
/*     xtum->offset = 0.0f; */
     xtum->scalef = 0.71;
     xtum->offsetf = 0.0;
     xtum->locked = 0;
     xtum->bbox   = 0;
     xtum->stereo = 0;
     xtum->plax = 4.0f;
     xtum->firstscale = 1;
     xtum->alpha = 0.0;
     xtum->beta = 0.0;
     xtum->gamma = 0.0;
     window_name   = imodwfname("IMOD Tumbler: ");
     
     xtum->dialog = XtVaCreatePopupShell
	  ("Tumbler", topLevelShellWidgetClass, App->toplevel,
	   XmNvisual, App->visual,
	   XtNtitle, window_name,
	   XmNwidth, xtum->width,
	   XmNheight, xtum->height,
	   NULL);
     if (!xtum->dialog){
	  free(xtum);
	  return(-1);
     }
     if (window_name)
	  free(window_name);

     mw = XtVaCreateManagedWidget
	  ("tumbler",  xmMainWindowWidgetClass,  xtum->dialog,
	   NULL);
     
     tools = XtVaCreateManagedWidget
	  ("frame", xmFrameWidgetClass, mw,
	   XmNshadowType, XmSHADOW_OUT,
	   NULL);
     row = XtVaCreateManagedWidget
	  ("toolrow", xmRowColumnWidgetClass, tools,
	   XmNorientation, XmHORIZONTAL,
	   NULL);
     {
	  button = XtVaCreateManagedWidget
	       ("+", xmPushButtonWidgetClass, row, NULL);
	  XtAddCallback(button, XmNactivateCallback, zoomup_cb,
			(XtPointer)xtum);
	  button = XtVaCreateManagedWidget
	       ("-", xmPushButtonWidgetClass, row, NULL);
	  XtAddCallback(button, XmNactivateCallback, zoomdown_cb,
			(XtPointer)xtum);
	  XtVaGetValues(button,
			XmNforeground, &fg,
			XmNbackground, &bg,
			XmNdepth, &depth,
			NULL);
	  xtum->lockpix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)lock_bits,
		button_width, button_height,
		fg, bg, depth);
	  xtum->unlockpix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)unlock_bits,
		button_width, button_height,
		fg, bg, depth);
	  button = XtVaCreateManagedWidget
	       ("Lock", xmPushButtonWidgetClass, row,
		XmNlabelType, XmPIXMAP,
		XmNlabelPixmap, xtum->unlockpix,
		XmNwidth, button_width+4,
		XmNheight, button_height+4,
		NULL);
	  XtAddCallback(button, XmNactivateCallback, lock_cb,
			(XtPointer)xtum);
	  
	  xtum->highrespix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)highres_bits,
		button_width, button_height,
		fg, bg, depth);
	  xtum->lowrespix = XCreatePixmapFromBitmapData
	       (App->display, XtWindow(App->toplevel), (char *)lowres_bits,
		button_width, button_height,
		fg, bg, depth);
	  button = XtVaCreateManagedWidget
	       ("Res", xmPushButtonWidgetClass, row,
		XmNlabelType, XmPIXMAP,
		XmNlabelPixmap, xtum->lowrespix,
		NULL);
	  XtAddCallback(button, XmNactivateCallback, res_cb,
			(XtPointer)xtum);
	  button = XtVaCreateManagedWidget
	       ("?", xmPushButtonWidgetClass, row, NULL);
	  XtAddCallback(button, XmNactivateCallback, help_cb,
			(XtPointer)xtum);

     }
     XtManageChild(row);
     XtManageChild(tools);

     form  = XtVaCreateManagedWidget("form", xmFrameWidgetClass, mw, NULL);
     xtum->gfx = XtVaCreateManagedWidget
	  ("gfx", B3dDrawingAreaWidgetClass, form,
	   XmNnavigationType, XmNONE,
	   XmNtraversalOn, True,
	   XmNtranslations, XtParseTranslationTable (B3DGFX_Translations),
#ifdef DRAW_OpenGL
	   GLwNvisualInfo, App->visualinfoGL,
	   XmNcolormap,         App->cmapGL,
#endif
#ifdef DRAW_GL
	   GlxNglxConfig, B3DGFX_GLXconfig_doublebuffer,
#endif
	   NULL);
     XtAddCallback(xtum->gfx,B3dNexposeCallback, expose_cb, (XtPointer)xtum);
     XtAddCallback(xtum->gfx,B3dNresizeCallback, resize_cb, (XtPointer)xtum);
     XtAddCallback(xtum->gfx,B3dNinputCallback,  input_cb,  (XtPointer)xtum);
     imodOverrideTransTable(xtum->gfx,B3DGFX_Translations);
     XtVaSetValues(xtum->gfx, XmNbackground, App->background, NULL);          

     XtManageChild(form);
#ifdef MWSETWORK
     XtVaSetValues(mw,XmNworkWindow,form,NULL);
#endif
     XmMainWindowSetAreas( mw, NULL, tools, NULL, NULL, form);
     XtManageChild(mw);
     
     wmclose = XmInternAtom( XtDisplay(xtum->dialog),
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(xtum->dialog, wmclose, quit_cb,
			     (caddr_t)xtum);
     XtPopup(xtum->dialog, XtGrabNone);
     
     return(0);
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


static void xtumFillSlice(struct imod_xtum_struct *xtum)
{
     Islice *tsl;
     Ipoint tx,ty,tz;
     int i, iz;

     /* Set up image pointer tables */
     imdata = (unsigned char **)
	  malloc(sizeof(unsigned char *) * xtum->vi->zsize);

     if (!imdata)
	  return;

     vmnullvalue = (App->cvi->white + App->cvi->black) / 2;
     if (xtum->vi->fakeImage) {
	  best_GetValue = fake_GetValue;

     } else if (xtum->vi->vmSize) {
	  /* For cached data, get pointers to data that exist at this time */
	  vmdataxsize = (int *)malloc(sizeof(int) * xtum->vi->zsize);
	  if (!vmdataxsize)
	       return;
	  best_GetValue = cache_GetValue;
	  for (i = 0; i < xtum->vi->zsize; i++)
	       imdata[i] = NULL;
	  for (i = 0; i < xtum->vi->vmSize; i++) {
	       iz = xtum->vi->vmCache[i].cz;
	       if (iz < xtum->vi->zsize && iz >= 0 &&
		   xtum->vi->vmCache[i].ct == xtum->vi->ct){
		    imdata[iz] = xtum->vi->vmCache[i].sec->data.b;
		    vmdataxsize[iz] = xtum->vi->vmCache[i].sec->xsize;
	       }
	  }

     } else {
	  /* for loaded data, get pointers from xtum->vi */
	  best_GetValue = idata_GetValue;
	  for (i = 0; i < xtum->vi->zsize; i++)
	       imdata[i] = xtum->vi->idata[i];
	  imdataxsize = xtum->vi->xsize;
     }

     if (xtum->highres)
	  xtumFillASlice(xtum);
     else
	  xtumFillBSlice(xtum);

     if (xtum->stereo){
	  tx.x = xtum->xstep.x; tx.y = xtum->xstep.y; tx.z = xtum->xstep.z;
	  ty.x = xtum->ystep.x; ty.y = xtum->ystep.y; ty.z = xtum->ystep.z;
	  tz.x = xtum->zstep.x; tz.y = xtum->zstep.y; tz.z = xtum->zstep.z;
	  tsl = xtum->slice;

	  xtum->xstep.x = xtum->sxstep.x;
	  xtum->xstep.y = xtum->sxstep.y;
	  xtum->xstep.z = xtum->sxstep.z;
	  xtum->ystep.x = xtum->systep.x;
	  xtum->ystep.y = xtum->systep.y;
	  xtum->ystep.z = xtum->systep.z;
	  xtum->zstep.x = xtum->szstep.x;
	  xtum->zstep.y = xtum->szstep.y;
	  xtum->zstep.z = xtum->szstep.z;
	  xtum->slice = xtum->stslice;

	  if (xtum->highres)
	       xtumFillASlice(xtum);
	  else
	       xtumFillBSlice(xtum);

	  xtum->stslice = xtum->slice;
	  xtum->slice = tsl;

	  xtum->sxstep.x = xtum->xstep.x;
	  xtum->sxstep.y = xtum->xstep.y;
	  xtum->sxstep.z = xtum->xstep.z;
	  xtum->systep.x = xtum->ystep.x;
	  xtum->systep.y = xtum->ystep.y;
	  xtum->systep.z = xtum->ystep.z;
	  xtum->szstep.x = xtum->zstep.x;
	  xtum->szstep.y = xtum->zstep.y;
	  xtum->szstep.z = xtum->zstep.z;
	  
	  xtum->xstep.x = tx.x;
	  xtum->xstep.y = tx.y;
	  xtum->xstep.z = tx.z;
	  xtum->ystep.x = ty.x;
	  xtum->ystep.y = ty.y;
	  xtum->ystep.z = ty.z;
	  xtum->zstep.x = tz.x;
	  xtum->zstep.y = tz.y;
	  xtum->zstep.z = tz.z;
     }
     free(imdata);
     if (xtum->vi->vmSize)
	  free(vmdataxsize);
     return;
}


static void xtumFillASlice(struct imod_xtum_struct *xtum)
{
     ImodView *iv = xtum->vi;
     float *data=xtum->slice->data.f;
#ifdef XTUM_DIVSCALE
     short *sdata = xtum->count->data.s;
#endif
     unsigned long xysize = xtum->slice->xsize * xtum->slice->ysize;
     unsigned long n;
     float xs, ys, zs, xt, yt, zt;
     float x, y, z;
     float xsx, xsy, xsz;
     float ysx, ysy, ysz;
     float zsx, zsy, zsz;
     float xtz, ytz, ztz;
     unsigned int zfill;
     int isize, jsize;
     unsigned long ksize, xsize, ysize, zsize;
     int xi, yi, zi;
     int i, j, k;
     float zoom;
     int xmin,ymin,zmin,xmax,ymax,zmax;
     unsigned char val;
     float dx, dy, dz;
     float x1, x2, y1, y2, z1, z2;
     float a, b, c, d, e, f, ival;
     int pxi, nxi, pyi, nyi, pzi, nzi;
     float maxval = 255.0f, minval = xtum->vi->black;

     if (xtum->slice->xsize <= 0)
	  return;
     if (xtum->slice->ysize <= 0)
	  return;

     zoom = xtum->zoom;
     xmin = xtum->cx - xtum->nx/2;
     if (xmin < 0) xmin = 0;
     ymin = xtum->cy - xtum->ny/2;
     if (ymin < 0) ymin = 0;
     zmin = xtum->cz - xtum->nz/2;
     if (zmin < 0) zmin = 0;
     xmax = xtum->cx + xtum->nx/2;
     if (xmax >= xtum->vi->xsize)
	  xmax = xtum->vi->xsize - 1;
     ymax = xtum->cy + xtum->ny/2;
     if (ymax >= xtum->vi->ysize)
	  ymax = xtum->vi->ysize - 1;
     zmax = xtum->cz + xtum->nz/2;
     if (zmax >= xtum->vi->zsize)
	  zmax = xtum->vi->zsize - 1;

     isize = xtum->slice->xsize;
     jsize = xtum->slice->ysize;
     ksize = xtum->ms;

     xsize = xtum->vi->xsize;
     ysize = xtum->vi->ysize;
     zsize = xtum->vi->zsize;

     xsx = xtum->xstep.x/zoom;
     xsy = xtum->xstep.y/zoom; 
     xsz = xtum->xstep.z/zoom;
     
     ysx = xtum->ystep.x/zoom; 
     ysy = xtum->ystep.y/zoom; 
     ysz = xtum->ystep.z/zoom;

     zsx = xtum->zstep.x/zoom; 
     zsy = xtum->zstep.y/zoom; 
     zsz = xtum->zstep.z/zoom;

     xs  = xtum->cx; ys = xtum->cy; zs = xtum->cz;
     xs -= xsx * isize * 0.5f;
     ys -= xsy * isize * 0.5f;
     zs -= xsz * isize * 0.5f;
     xs -= ysx * jsize * 0.5f;
     ys -= ysy * jsize * 0.5f;
     zs -= ysz * jsize * 0.5f;
     xs -= zsx * xtum->nz * 0.5f;
     ys -= zsy * xtum->nz * 0.5f;
     zs -= zsz * xtum->nz * 0.5f;

     x = xs; y = ys; z = zs;

     for(n = 0; n < xysize; n++)
	  data[n] = 0.0f;

#ifdef XTUM_DIVSCALE
     for(n = 0; n < xysize; n++)
	  sdata[n] = (short)0;
#endif
     for(k = 0; k < ksize; k++){
	  xtz = x; ytz = y; ztz = z;
	  for(j = 0, n = 0; j < jsize; j++){
	       xt = x; yt = y; zt = z;
	       for(i = 0; i < isize; i++, n++){
		    xi = x + 0.5f; yi = y + 0.5f; zi = z + 0.5f;
		    /* todo: check limits, set flags. */
		    if ((xi >= xmin) && (xi <= xmax) &&
			(yi >= ymin) && (yi <= ymax) &&
			(zi >= zmin) && (zi <= zmax)
			){
			 val = (*best_GetValue)(xi, yi, zi);
			 
			 dx = x - xi;
			 dy = y - yi;
			 dz = z - zi;
			 
			 pxi = xi - 1;
			 nxi = xi + 1;
			 pyi = yi - 1;
			 nyi = yi + 1;
			 pzi = zi - 1;
			 nzi = zi + 1;
			 
			 if (pxi < 0) pxi = 0;
			 if (nxi >= xsize) nxi = xi;
			 if (pyi < 0) pyi = 0;
			 if (nyi >= ysize) nyi = yi;
			 if (pzi < 0) pzi = 0;
			 if (nzi >= zsize) nzi = zi;
			 
			 
			 x1 = (*best_GetValue)(pxi,  yi,  zi);
			 x2 = (*best_GetValue)(nxi,  yi,  zi);
			 y1 = (*best_GetValue)( xi, pyi,  zi);
			 y2 = (*best_GetValue)( xi, nyi,  zi);
			 z1 = (*best_GetValue)( xi,  yi, pzi);
			 z2 = (*best_GetValue)( xi,  yi, nzi);
/*			 
			 x1 = idata[zi][pxi + (yi * xsize)];
			 x2 = idata[zi][nxi + (yi * xsize)];
			 y1 = idata[zi][xi + (pyi * xsize)];
			 y2 = idata[zi][xi + (nyi * xsize)];
			 z1 = idata[pzi][xi + (yi * xsize)];
			 z2 = idata[nzi][xi + (yi * xsize)];
*/
			 a = (x1 + x2) * 0.5f - (float)val;
			 b = (y1 + y2) * 0.5f - (float)val;
			 c = (z1 + z2) * 0.5f - (float)val;
			 d = (x2 - x1) * 0.5f;
			 e = (y2 - y1) * 0.5f;
			 f = (z2 - z1) * 0.5f;
			 ival = (a * dx * dx) + (b * dy * dy) + (c * dz * dz)
			      + (d * dx) + (e * dy) + (f * dz) + (float)val;
			 if (ival > maxval)
			      ival = maxval;
			 if (ival < minval)
			      ival = minval;
			 data[n] += ival + 0.5f;
#ifdef XTUM_DIVSCALE
			 sdata[n]++;
#endif
		    }
		    x += xsx;
		    y += xsy;
		    z += xsz;
	       }
	       x = xt + ysx;
	       y = yt + ysy;
	       z = zt + ysz;
	  }
	  x = xtz + zsx;
	  y = ytz + zsy;
	  z = ztz + zsz;
     }
#ifdef XTUM_DIVSCALE
     for(n = 0; n < xysize; n++)
	  data[n] /= (float)sdata[n];
#endif
     return;
}


static void xtumFillBSlice(struct imod_xtum_struct *xtum)
{
     ImodView *iv = xtum->vi;
     float *data=xtum->slice->data.f;
#ifdef XTUM_DIVSCALE
     short *sdata=xtum->count->data.s;
#endif
     unsigned long xysize = xtum->slice->xsize * xtum->slice->ysize;
     unsigned long n;
     float xs, ys, zs, xt, yt, zt;
     float x, y, z;
     float xsx, xsy, xsz;
     float ysx, ysy, ysz;
     float zsx, zsy, zsz;
     float xtz, ytz, ztz;
     unsigned int zfill;
     int isize, jsize;
     unsigned long ksize, xsize, ysize, zsize;
     int xi, yi, zi;
     int i, j, k;
     int val, minval=0, maxval = 255;
     int xmin,ymin,zmin,xmax,ymax,zmax;
     
     minval = xtum->vi->black;
     xmin = xtum->cx - xtum->nx/2;
     if (xmin < 0) xmin = 0;
     ymin = xtum->cy - xtum->ny/2;
     if (ymin < 0) ymin = 0;
     zmin = xtum->cz - xtum->nz/2;
     if (zmin < 0) zmin = 0;
     xmax = xtum->cx + xtum->nx/2;
     if (xmax >= xtum->vi->xsize)
	  xmax = xtum->vi->xsize - 1;
     ymax = xtum->cy + xtum->ny/2;
     if (ymax >= xtum->vi->ysize)
	  ymax = xtum->vi->ysize - 1;
     zmax = xtum->cz + xtum->nz/2;
     if (zmax >= xtum->vi->zsize)
	  zmax = xtum->vi->zsize - 1;

     isize = xtum->slice->xsize;
     jsize = xtum->slice->ysize;
     ksize = xtum->ms;
     xsize = xtum->vi->xsize;
     ysize = xtum->vi->ysize;
     zsize = xtum->vi->zsize;

     xsx = xtum->xstep.x; xsy = xtum->xstep.y; xsz = xtum->xstep.z;
     ysx = xtum->ystep.x; ysy = xtum->ystep.y; ysz = xtum->ystep.z;
     zsx = xtum->zstep.x; zsy = xtum->zstep.y; zsz = xtum->zstep.z;

     xs  = xtum->cx; ys = xtum->cy; zs = xtum->cz;
     xs -= xsx * isize * 0.5f;
     ys -= xsy * isize * 0.5f;
     zs -= xsz * isize * 0.5f;
     xs -= ysx * jsize * 0.5f;
     ys -= ysy * jsize * 0.5f;
     zs -= ysz * jsize * 0.5f;
     xs -= zsx * xtum->nz * 0.5f;
     ys -= zsy * xtum->nz * 0.5f;
     zs -= zsz * xtum->nz * 0.5f;

     x = xs; y = ys; z = zs;

     for(n = 0; n < xysize; n++)
	  data[n] = 0.0f;

#ifdef XTUM_DIVSCALE
     for(n = 0; n < xysize; n++)
	  sdata[n] = (short)0;
#endif
     for(k = 0; k < ksize; k++){
	  xtz = x; ytz = y; ztz = z;
	  for(j = 0, n = 0; j < jsize; j++){
	       xt = x; yt = y; zt = z;
	       for(i = 0; i < isize; i++, n++){
		    xi = x + 0.5f; yi = y + 0.5f; zi = z + 0.5f;
		    /* todo: check limits, set flags. */
		    if ((xi >= xmin) && (xi <= xmax) &&
			(yi >= ymin) && (yi <= ymax) &&
			(zi >= zmin) && (zi <= zmax)
			){
			 val = (*best_GetValue)(xi, yi, zi);
			 if (val > minval)
			      data[n] += val;
#ifdef XTUM_DIVSCALE
			 sdata[n]++;
#endif
		    }
		    x += xsx;
		    y += xsy;
		    z += xsz;
	       }
	       x = xt + ysx;
	       y = yt + ysy;
	       z = zt + ysz;
	  }
	  x = xtz + zsx;
	  y = ytz + zsy;
	  z = ztz + zsz;
     }

#ifdef XTUM_DIVSCALE
     for(n = 0; n < xysize; n++)
	  data[n] /= (float)sdata[n];
#endif

     return;
}

static void xtumScaleData(struct imod_xtum_struct *xtum)
{
     unsigned long i;
     unsigned long xysize = xtum->slice->xsize * xtum->slice->ysize;
     float scale, offset;
     float *fdata = xtum->slice->data.f;
     float min, max;
     
     min = max = 0.0f;
     for(i = 0; i < xysize; i++){
	  if (fdata[i] > max)
	       max = fdata[i];
	  if (min == 0)
	       min = fdata[i];
	  if (fdata[i] < min)
	       if (fdata[i] != 0)
		    min = fdata[i];
     }
     
     sliceMMM(xtum->slice);
     
     if ((max - min) != 0.0)
	  scale = 256.0f / (max - min);
     else
	  scale = 1.0f;
     
     offset = -min;
     
     xtum->scale = scale;
     xtum->offset = offset;
     return;
}

void xtumDrawImage(struct imod_xtum_struct *xtum)
{
     float scale, offset;
     unsigned long i, xysize;
     unsigned char *data;
     float *fdata;
     float min, max;
     float tf, tmax, tmin;
     int xo, yo;
     int width = xtum->width;
     int zoom = xtum->zoom;
     xysize = xtum->slice->xsize * xtum->slice->ysize;
     min = max = 0.0f;
     fdata = xtum->slice->data.f;
     data = xtum->bwslice->data.b;


     if (xtum->firstscale){
	  xtumScaleData(xtum);
	  xtum->firstscale = False;
     }
#ifdef XTUM_ALWAYS_RESCALE
     else
	  xtumScaleData(xtum);
#endif

     tmax = 255.0f;
     tmin = 0.0f;
     scale  = xtum->scale * xtum->scalef;
     offset = xtum->offset + xtum->offsetf;
     
     /* DNM: scale for depth 8 only if not rgba */
     if (App->depth == 8 && !App->rgba){
	  scale *= xtum->vi->rampsize/256.0f;
	  tmax = xtum->vi->rampbase + xtum->vi->rampsize;
	  tmin = xtum->vi->rampbase;
	  for(i = 0; i < xysize; i++){
	       tf = ((fdata[i] + offset) * scale) + xtum->vi->rampbase;
	       if (tf < tmin) tf = tmin;
	       if (tf > tmax) tf = tmax;
	       data[i] = tf;
	  }
     }else{
     
	  for(i = 0; i < xysize; i++){
	       tf = (fdata[i] + offset) * scale;
	       if (tf < tmin) tf = tmin;
	       if (tf > tmax) tf = tmax;
	       data[i] = tf;
	  }
     }

     if (xtum->stereo)
	  width = xtum->width/2;
     
     if (xtum->highres)
	  zoom = 1;

     /* DNM 1/20/02: add slice argument to graphics calls; make it -1 to 
	prevent image re-use */
     b3dDrawGreyScalePixelsSubArea
	  (xtum->image, data, 
	   xtum->slice->xsize, xtum->slice->ysize,
	   0, 0, 0, 0, width, xtum->height,
	   xtum->vi->rampbase, zoom,
	   &xo, &yo, -1);

     if (xtum->stereo){
	  fdata = xtum->stslice->data.f;
	  /* DNM: scale for depth 8 only if not rgba */
	  if (App->depth == 8 && !App->rgba){
	       for(i = 0; i < xysize; i++){
		    tf = ((fdata[i] + offset) * scale) + xtum->vi->rampbase;
		    if (tf < tmin) tf = tmin;
		    if (tf > tmax) tf = tmax;
		    data[i] = tf;
	       }
	  }else{
	       for(i = 0; i < xysize; i++){
		    tf = (fdata[i] + offset) * scale;
		    if (tf < tmin) tf = tmin;
		    if (tf > tmax) tf = tmax;
		    data[i] = tf;
	       }
	  }
	  b3dDrawGreyScalePixelsSubArea
	       (xtum->image, data,
		xtum->slice->xsize, xtum->slice->ysize,
		0, 0, width, 0, xtum->width, xtum->height,
		xtum->vi->rampbase, zoom,
		&xo, &yo, -1);
     }

     return;
}

static void xtumDrawBBox(struct imod_xtum_struct *xtum)
{
     Imat *mat;
     Ipoint inp, outp1, outp2, transp;
     int x2 = xtum->nx/2;
     int y2 = xtum->ny/2;
     int z2 = xtum->nz/2;
     int width = xtum->width;

     if (!xtum->bbox)
	  return;

     mat = imodMatNew(3);
     b3dColorIndex(App->foreground);     
     if (xtum->stereo)
	  width/=2;

     transp.x = transp.y = transp.z = xtum->zoom;
     imodMatScale(mat, &transp);      
     transp.x = ((float)width *0.5f);
     transp.y = ((float)xtum->height*0.5f);
     transp.z = 0;

     imodMatRot(mat, (double)xtum->gamma, 2);
     imodMatRot(mat, (double)xtum->beta,  1);
     imodMatRot(mat, (double)xtum->alpha, 0);
     imodMatTrans(mat, &transp);

     xtumDrawBoxLines(xtum, mat);

     if (xtum->stereo){
	  imodMatId(mat);
	  transp.x = transp.y = transp.z = xtum->zoom;
	  imodMatScale(mat, &transp);
	  imodMatRot(mat, (double)xtum->gamma, 2);
          imodMatRot(mat, (double)xtum->beta + xtum->plax,  1);
	  imodMatRot(mat, (double)xtum->alpha, 0);
	  transp.x = ((float)width * 1.5f);
	  transp.y = ((float)xtum->height*0.5f);
	  transp.z = 0;
	  imodMatTrans(mat, &transp);

          xtumDrawBoxLines(xtum, mat);
     }
     imodMatDelete(mat);

     return;
}

static void xtumDrawBoxLines(struct imod_xtum_struct *xtum, Imat *mat)
{
     Ipoint inp, outp1, outp2, transp;
     int x2 = xtum->nx/2;
     int y2 = xtum->ny/2;
     int z2 = xtum->nz/2;

     inp.x = x2; inp.y = y2; inp.z = z2;
     imodMatTransform(mat, &inp, &outp1);
     inp.x = -x2;
     imodMatTransform(mat, &inp, &outp2);
     b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
/*     printf("line %d,%d to %d,%d\n",
	    (int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);*/
     inp.x = x2; inp.y = -y2;
     imodMatTransform(mat, &inp, &outp2);
     b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
     inp.y = y2; inp.z = -z2;
     imodMatTransform(mat, &inp, &outp2);
     b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);

     inp.x = -x2; inp.y = -y2; inp.z = -z2;
     imodMatTransform(mat, &inp, &outp1);
     inp.x = x2;
     imodMatTransform(mat, &inp, &outp2);
     b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
     inp.x = -x2; inp.y = y2;
     imodMatTransform(mat, &inp, &outp2);
     b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
     inp.y = -y2; inp.z = z2;
     imodMatTransform(mat, &inp, &outp2);
     b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);

     inp.x = -x2; inp.y = y2; inp.z = -z2;
     imodMatTransform(mat, &inp, &outp1);
     inp.z = z2;
     imodMatTransform(mat, &inp, &outp2);
     b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
     inp.y = -y2;
     imodMatTransform(mat, &inp, &outp1);
     b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
     inp.x = x2;
     imodMatTransform(mat, &inp, &outp2);
     b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);

     inp.x = -x2; inp.y = y2; inp.z = -z2;
     imodMatTransform(mat, &inp, &outp1);
     inp.x = x2;
     imodMatTransform(mat, &inp, &outp2);
     b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
     inp.y = -y2;
     imodMatTransform(mat, &inp, &outp1);
     b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);
     inp.z = z2;
     imodMatTransform(mat, &inp, &outp2);
     b3dDrawLine((int)outp1.x, (int)outp1.y, (int)outp2.x, (int)outp2.y);

     return;
}


void xtumSetSteps(struct imod_xtum_struct *xtum)
{
     Imat *mat;
     Ipoint inp, outp, transp;

     inp.x = inp.y = inp.z = 0.0f;
     mat = imodMatNew(3);

     transp.x = xtum->cx;
     transp.y = xtum->cy;
     transp.z = xtum->cz;
     imodMatTrans(mat, &transp);
     imodMatRot(mat, (double)xtum->alpha, 0);
     imodMatRot(mat, (double)xtum->beta,  1);
     imodMatRot(mat, (double)xtum->gamma, 2);
     transp.x *= -1.0f; transp.y *= -1.0f; transp.z *= -1.0f;
     imodMatTrans(mat, &transp);
     imodMatTransform(mat, &inp, &outp);
     inp.x = 1.0f;
     imodMatTransform(mat, &inp, &(xtum->xstep));
     inp.y = 1.0f; inp.x = 0.0f;
     imodMatTransform(mat, &inp, &(xtum->ystep));
     inp.z = -1.0f; inp.y = 0.0f;
     imodMatTransform(mat, &inp, &(xtum->zstep));
     xtum->xstep.x -= outp.x; xtum->xstep.y -= outp.y; xtum->xstep.z -= outp.z;
     xtum->ystep.x -= outp.x; xtum->ystep.y -= outp.y; xtum->ystep.z -= outp.z;
     xtum->zstep.x -= outp.x; xtum->zstep.y -= outp.y; xtum->zstep.z -= outp.z;

     if (xtum->stereo){
	  imodMatId(mat);
	  transp.x = xtum->cx; transp.y = xtum->cy; transp.z = xtum->cz;
	  imodMatTrans(mat, &transp);
	  imodMatRot(mat, (double)xtum->alpha, 0);
	  imodMatRot(mat, (double)xtum->beta + xtum->plax,  1);
	  imodMatRot(mat, (double)xtum->gamma, 2);
	  transp.x *= -1.0f; transp.y *= -1.0f; transp.z *= -1.0f;
	  imodMatTrans(mat, &transp);
	  inp.x = inp.y = inp.z = 0.0f;
	  imodMatTransform(mat, &inp, &outp);
	  inp.x = 1.0f;
	  imodMatTransform(mat, &inp, &(xtum->sxstep));
	  inp.y = 1.0f; inp.x = 0.0f;
	  imodMatTransform(mat, &inp, &(xtum->systep));
	  inp.z = 1.0f; inp.y = 0.0f;
	  imodMatTransform(mat, &inp, &(xtum->szstep));
	  xtum->sxstep.x -= outp.x; xtum->sxstep.y -= outp.y; 
	  xtum->sxstep.z -= outp.z;
	  xtum->systep.x -= outp.x; xtum->systep.y -= outp.y; 
	  xtum->systep.z -= outp.z;
	  xtum->szstep.x -= outp.x; xtum->szstep.y -= outp.y; 
	  xtum->szstep.z -= outp.z;
     }
     imodMatDelete(mat);
     return;
}

void xtumNewData(struct imod_xtum_struct *xtum)
{
     xtumSetSteps(xtum);
     xtumFillSlice(xtum);
     return;
}

void xtumDrawBorder(struct imod_xtum_struct *xtum)
{
     int cx, cy, gx, gy, sx, sy;
     cx = xtum->width/2;
     gx = (xtum->ms * xtum->zoom)/2;
     cy =  xtum->height/2;
     gy = (xtum->ms * xtum->zoom)/2;
     
     b3dColorIndex(App->background);
     
     
     if (!xtum->stereo){
	  b3dDrawBoxout(cx-gx, cy-gy, cx + gx, cy + gy);
     }else{
	  b3dDrawBoxout((cx/2)-gx, cy-gy, ((cx/2)*3)+gx,cy + gy);
	  sx = (cx/2)-gx;
	  if (sx > 0)
	       b3dDrawFilledRectangle(cx - sx, cy-gy, sx*2,  gy*2);
     }
     return;
}

void xtumDraw( struct imod_xtum_struct *xtum)
{
     if (!xtum->exposed)
	  return;
     
     b3dWinset(XtDisplay(xtum->gfx), xtum->gfx, xtum->context);
     b3dColorIndex(xtum->vi->rampbase);

     xtumDrawImage(xtum);
     xtumDrawBBox(xtum);
     xtumDrawBorder(xtum);
     b3dSwapBuffers();
     return;
}


/*****************************************************************************/
static void addworkproc(XtPointer client, XtIntervalId *id)
{
     struct imod_xtum_struct *xtum = (struct imod_xtum_struct *)client;
     
     xtum->work_id = XtAppAddWorkProc(xtum->app, work_update, xtum);
     xtum->iid = 0;
     return;
}

static void tupdate(XtPointer client, XtIntervalId *id)
{
     struct imod_xtum_struct *xtum = (struct imod_xtum_struct *)client;
     
     if (!xtum->vi)
	  return;
     if (!xtum->iid)
	  return;
     if (!xtum->locked)
	  if ((xtum->cx != xtum->vi->xmouse) ||
	      (xtum->cy != xtum->vi->ymouse) ||
	      (xtum->cz != xtum->vi->zmouse) ||
	      (App->rgba && ((xtum->black != xtum->vi->black) ||
			     (xtum->white != xtum->vi->white) ||
			     (xtum->falsecolor != 
			      xtum->vi->cramp->falsecolor)))){
	       xtum->cx = xtum->vi->xmouse;
	       xtum->cy = xtum->vi->ymouse;
	       xtum->cz = xtum->vi->zmouse;
	       xtum->black = xtum->vi->black;
	       xtum->white = xtum->vi->white;
	       xtum->falsecolor = xtum->vi->cramp->falsecolor;
	       xtumNewData(xtum);
	       xtumDraw(xtum);
	  }
     
     if (xtum->locked)
	  xtum->iid = XtAppAddTimeOut (xtum->app, 1000L, tupdate,
				     (XtPointer)xtum);
     else
	  xtum->iid = XtAppAddTimeOut (xtum->app, 100L, tupdate,
				     (XtPointer)xtum);
     return;
}

/* Workproc update function */
static Boolean work_update(XtPointer client)
{
     struct imod_xtum_struct *xtum = (struct imod_xtum_struct *)client;
     
     if ((xtum->cx != xtum->vi->xmouse) ||
	 (xtum->cy != xtum->vi->ymouse) ||
	 (xtum->cz != xtum->vi->zmouse) ||
	 (App->rgba && ((xtum->black != xtum->vi->black) ||
			(xtum->white != xtum->vi->white)))){
	  
	  xtumDraw(xtum);
	  return(True);
     }
     
     xtum->iid = XtAppAddTimeOut (xtum->app, 100L, addworkproc,
				(XtPointer)xtum);
     xtum->work_id = 0;
     return(True);
}
