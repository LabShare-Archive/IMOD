/*  IMOD VERSION 2.40
 *
 *  xgraph.c -- Imod graph window.
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

#include "imod.h"

/*#include "xgraph.h" */

#ifdef DRAW_GL
long Igraphx_Window;
long Igraphy_Window;
long Igraphz_Window;
int imod_igraph_input(Device dev, short val){return(0);}
void imod_igraph_close(char axis){return;}
int imod_igraph_draw(struct ViewInfo *vi){return(0);}
struct ViewInfo Igraphx_vi;
struct ViewInfo Igraphy_vi;
struct ViewInfo Igraphz_vi;
#endif

#define button_width  16
#define button_height 16
#define XGRAPH_PAXIS_HEIGHT 20

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


struct imod_xgraph_struct
{
     struct ViewInfo *vi;

     Widget        dialog;
     Widget        gfx;
     Widget        paxis;
     Widget        vaxis;
     XtWorkProcId  work_id;
     XtIntervalId  iid;
     XtAppContext  app;
     XID           context;
     GC            vaxis_context;
     GC            paxis_context;
     XFontStruct   *font;
     XmFontList    fontlist;
     Pixmap        lockpix;
     Pixmap        unlockpix;
     Pixmap        lowrespix;
     Pixmap        highrespix;

     int    exposed;
     int    vexposed, pexposed;
     int    width, height;
     int    zoom;
     float *data;
     int    dsize;
     int    cpt;
     float  cx, cy, cz; /* current location. */
     int    co, cc, cp; /* current object, contour, point */
     int    axis;
     int    locked;
     int    highres;
     float  offset;
     float  scale;
     float  min, max;
     int    start;
     int    ctrl;
};

static void    addworkproc(XtPointer client, XtIntervalId *id);
static void    tupdate(XtPointer client, XtIntervalId *id);
static Boolean work_update(XtPointer client_data);
static void setxyz(struct imod_xgraph_struct *xg, int mx, int my);

void xgraphDraw(struct imod_xgraph_struct *xg);
void xgraphDrawAxis(struct imod_xgraph_struct *xg);
void xgraphDrawPlot(struct imod_xgraph_struct *xg);
void xgraphDrawPaxis(struct imod_xgraph_struct *xg);
void xgraphDrawVaxis(struct imod_xgraph_struct *xg);
void xgraphStringDraw(Widget w, GC context, char *string, int x, int y, 
		      int width, unsigned char align);

void graphClose_cb(ImodView *vi, void *client, int junk);
void graphDraw_cb(ImodView *vi, void *client, int drawflag);

/* Make nice ticks. */
/*
void xgraphTicks(float *min, float *max, float *step)
{
}
*/

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xgraph_struct *win = (struct imod_xgraph_struct *)client;
     if (win)
	  ivwDeleteControl(win->vi, win->ctrl);
     return;
}

static void ginit_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;
     
     if (xg->exposed) return;
     
     xg->context = b3dGetContext(w);
     b3dWinset(XtDisplay(xg->gfx), xg->gfx, xg->context);
     b3dResizeViewport();
     return;
}
static void pexpose_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;
     XmFontList fontlist;

     if (!xg->pexposed){
	  xg->pexposed = True;
	  xg->paxis_context = (GC)b3dGetXContext(xg->paxis);
	  return;
     }
     xgraphDrawPaxis(xg);
     return;
}
static void vexpose_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;
     XmFontList fontlist;
     XGCValues val;
     
     if (!xg->vexposed){
	  xg->vexposed = True;
	  xg->vaxis_context = (GC)b3dGetXContext(xg->vaxis);
	  return;
     }
     xgraphDrawVaxis(xg);
     return;
}

static void expose_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;
     Dimension winx, winy;

     if (!xg->exposed){
	  XtVaGetValues(w, XmNwidth, &winx, XmNheight, &winy, NULL);
	  xg->width  = winx;
	  xg->height = winy;
	  ginit_cb(w, client, call);
	  xg->exposed = True;
     }
     b3dWinset(XtDisplay(xg->gfx), xg->gfx, xg->context);
     b3dResizeViewport();
     xgraphDraw(xg);
     return;
}

static void resize_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;
     Dimension winx, winy;
     
     XtVaGetValues(w, XmNwidth, &winx, XmNheight, &winy, NULL);
     xg->width  = winx;
     xg->height = winy;
     if (xg->exposed){
	  b3dWinset(XtDisplay(xg->gfx), xg->gfx, xg->context);
	  b3dResizeViewport();
	  xgraphDraw(xg);
     }
     return;
}

static void input_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;
     B3dDrawingAreaCallbackStruct *cbs = (B3dDrawingAreaCallbackStruct *)call;
     int charcount;
     char buffer[1];
     int bufsize = 1;
     KeySym keysym;

     switch(cbs->event->type){						      
									      
	case KeyPress:							      
	  charcount = XLookupString					      
	       ((XKeyEvent *)cbs->event, buffer, bufsize, &keysym, NULL);     
	  if (charcount){						      
	       switch(buffer[0]){					      
		  case '=':						      
		    xg->zoom += 1.0;					      
		    break;						      
		  case '-':						      
		    xg->zoom -= 1.0;					      
		    if (xg->zoom < 1.0)					      
			 xg->zoom = 1.0;				      
		    break;						      
	       }							      
	  }								      
	  break;							      
	case ButtonPress:						      
	  XmProcessTraversal(xg->gfx, XmTRAVERSE_CURRENT);		      
	  if (cbs->event->xbutton.button == 1)				      
	       setxyz(xg, cbs->event->xbutton.x,			      
		      cbs->event->xbutton.y);				      
	  break;							      
									      
     }									      
     return;								      
}									      
									      
static void zoomup_cb(Widget w, XtPointer client, XtPointer call)	      
{									      
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;     
     xg->zoom++;							      
     xgraphDraw(xg);							      
     return;								      
}									      
static void zoomdown_cb(Widget w, XtPointer client, XtPointer call)	      
{									      
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;     
     xg->zoom--;							      
     if (!xg->zoom)							      
	  xg->zoom = 1;							      
     xgraphDraw(xg);							      
     return;								      
}									      
									      
static void xaxis_cb(Widget w, XtPointer client, XtPointer call)	      
{									      
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;     
     xg->axis = 0;							      
     xgraphDraw(xg);							      
     return;								      
}									      
static void yaxis_cb(Widget w, XtPointer client, XtPointer call)	      
{									      
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;     
     xg->axis = 1;							      
     xgraphDraw(xg);							      
     return;								      
}									      
static void zaxis_cb(Widget w, XtPointer client, XtPointer call)	      
{									      
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;     
     xg->axis = 2;							      
     xgraphDraw(xg);							      
     return;								      
}									      
static void caxis_cb(Widget w, XtPointer client, XtPointer call)	      
{									      
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;     
     xg->axis = 4;							      
     xgraphDraw(xg);							      
     return;								      
}

static void haxis_cb(Widget w, XtPointer client, XtPointer call)
{
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;
     xg->axis = 5;
     xgraphDraw(xg);
     return;
}
									      
static void lock_cb(Widget w, XtPointer client, XtPointer call)		      
{									      
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;     
									      
     if (xg->locked){							      
	  xg->locked = False;						      
	  XtVaSetValues(w, XmNlabelPixmap, xg->unlockpix, NULL);	      
     }else{								      
	  xg->locked = True;						      
	  XtVaSetValues(w, XmNlabelPixmap, xg->lockpix, NULL);		      
     }									      
     return;								      
}									      
									      
static void res_cb(Widget w, XtPointer client, XtPointer call)		      
{									      
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;     
     									      
     if (xg->highres){							      
	  xg->highres = False;						      
	  XtVaSetValues(w, XmNlabelPixmap, xg->lowrespix, NULL);	      
     }else{								      
	  xg->highres = True;						      
	  XtVaSetValues(w, XmNlabelPixmap, xg->highrespix, NULL);	      
     }									      
     xgraphDraw(xg);							      
     return;								      
}									      
									      
/* stub for old igraph.h */						      
int imod_igraph_open(char axis)						      
{									      
     xgraphOpen(XYZ_vi);						      
     return(-1);							      
}									      
									      
int xgraphOpen(struct ViewInfo *vi)					      
{									      
     struct imod_xgraph_struct *xg;					      
     char *window_name;							      
     Widget mw, frame, form, tools;					      
     Widget row, button, menu;						      
     Widget pulldown, option, pb1, pb2, pb3, pb4;			      
									      
     Atom            wmclose;						      
     Arg             args[10];						      
     Cardinal        n = 0;						      
     Pixel           fg, bg;						      
     int             depth;						      
									      
     xg = (struct imod_xgraph_struct *)malloc				      
	  (sizeof(struct imod_xgraph_struct));				      
     if (!xg)								      
	  return(-1);							      
									      
     xg->vi      = vi;							      
     xg->axis    = 0;							      
     xg->exposed = xg->pexposed = xg->vexposed = False;			      
     xg->width   = 300;							      
     xg->height  = 150;							      
     xg->zoom    = 1;							      
     xg->data    = NULL;						      
     xg->dsize   = 0;							      
     xg->locked  = False;						      
     xg->work_id = 0;							      
     xg->iid     = 0;							      
     xg->app     = App->context;					      
     xg->highres = 0;							      
     xg->context = 0;
     xg->paxis_context = xg->vaxis_context = 0;
     xg->paxis = xg->vaxis = 0;
     window_name = imodwfname("IMOD Graph: ");				      
									      
     xg->dialog = XtVaCreatePopupShell					      
	  ("Graph", topLevelShellWidgetClass, App->toplevel,		      
	   XmNvisual, App->visual,					      
	   XtNtitle, window_name,					      
	   XmNwidth, xg->width,						      
	   XmNheight, xg->height,					      
	   NULL);							      
     if (window_name)							      
	  free(window_name);						      
     if (!xg->dialog){							      
	  free(xg);							      
	  return(-1);							      
     }									      
									      
     mw = XtVaCreateManagedWidget					      
	  ("graph",  xmMainWindowWidgetClass,  xg->dialog,		      
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
			(XtPointer)xg);					      
	  button = XtVaCreateManagedWidget				      
	       ("-", xmPushButtonWidgetClass, row, NULL);		      
	  XtAddCallback(button, XmNactivateCallback, zoomdown_cb,	      
			(XtPointer)xg);					      
									      
	  XtVaGetValues(button,						      
			XmNforeground, &fg,				      
			XmNbackground, &bg,				      
			XmNdepth, &depth,				      
			NULL);						      
	  xg->lockpix = XCreatePixmapFromBitmapData			      
	       (App->display, XtWindow(App->toplevel), (char *)lock_bits,     
		button_width, button_height,				      
		fg, bg, depth);						      
	  xg->unlockpix = XCreatePixmapFromBitmapData			      
	       (App->display, XtWindow(App->toplevel), (char *)unlock_bits,   
		button_width, button_height,				      
		fg, bg, depth);						      
	  button = XtVaCreateManagedWidget				      
	       ("Lock", xmPushButtonWidgetClass, row,			      
		XmNlabelType, XmPIXMAP,					      
		XmNlabelPixmap, xg->unlockpix,				      
		NULL);							      
	  XtAddCallback(button, XmNactivateCallback, lock_cb,		      
			(XtPointer)xg);					      
									      
	  xg->highrespix = XCreatePixmapFromBitmapData			      
	       (App->display, XtWindow(App->toplevel), (char *)highres_bits,  
		button_width, button_height,				      
		fg, bg, depth);						      
	  xg->lowrespix = XCreatePixmapFromBitmapData			      
	       (App->display, XtWindow(App->toplevel), (char *)lowres_bits,   
		button_width, button_height,				      
		fg, bg, depth);						      
	  button = XtVaCreateManagedWidget				      
	       ("Res", xmPushButtonWidgetClass, row,			      
		XmNlabelType, XmPIXMAP,					      
		XmNlabelPixmap, xg->lowrespix,				      
		NULL);							      
	  XtAddCallback(button, XmNactivateCallback, res_cb,		      
			(XtPointer)xg);					      
	  								      
	  XtSetArg(args[n], XmNvisual, App->visual); n++;		      
	  pulldown = XmCreatePulldownMenu				      
	       (row, "option_pd", args, n);				      
	  pb1 = XtVaCreateManagedWidget					      
	       ("X-axis", xmPushButtonWidgetClass, pulldown, NULL);	      
	  XtAddCallback(pb1, XmNactivateCallback, xaxis_cb, (XtPointer)xg);   
	  pb2 = XtVaCreateManagedWidget					      
	       ("Y-axis", xmPushButtonWidgetClass, pulldown, NULL);	      
	  XtAddCallback(pb2, XmNactivateCallback, yaxis_cb, (XtPointer)xg);   
	  pb3 = XtVaCreateManagedWidget					      
	       ("Z-axis", xmPushButtonWidgetClass, pulldown, NULL);	      
	  XtAddCallback(pb3, XmNactivateCallback, zaxis_cb, (XtPointer)xg);   
	  pb4 = XtVaCreateManagedWidget					      
	       ("Contour", xmPushButtonWidgetClass, pulldown, NULL);	      
	  XtAddCallback(pb4, XmNactivateCallback, caxis_cb, (XtPointer)xg);
	  pb4 = XtVaCreateManagedWidget		      
	       ("Histo.", xmPushButtonWidgetClass, pulldown, NULL);	      
	  XtAddCallback(pb4, XmNactivateCallback, haxis_cb, (XtPointer)xg);

	  XtSetArg(args[n], XmNsubMenuId, pulldown); n++;		      
	  XtSetArg(args[n], XmNmenuHistory, pb1);    n++;		      
	  option =  XmCreateOptionMenu(row, "option_rc", args, n);	      
									      
	  XtManageChild(option);					      
     }									      
									      
     XtManageChild(row);						      
     XtManageChild(tools);						      
									      
     form  = XtVaCreateManagedWidget("form", xmFormWidgetClass, mw, NULL);    
									      
     xg->vaxis = XtVaCreateManagedWidget				      
	  ("axis", xmDrawingAreaWidgetClass, form,			      
	   XmNwidth,          50,					      
	   XmNleftAttachment, XmATTACH_FORM,				      
	   XmNtopAttachment,    XmATTACH_FORM,				      
	   XmNbottomAttachment, XmATTACH_FORM,				      
	   NULL);							      
     XtAddCallback(xg->vaxis,XmNexposeCallback, vexpose_cb, (XtPointer)xg);   
									      
     xg->paxis = XtVaCreateManagedWidget				      
	  ("axis", xmDrawingAreaWidgetClass, form,			      
	   XmNheight,           XGRAPH_PAXIS_HEIGHT,			      
	   XmNleftAttachment,   XmATTACH_WIDGET,			      
	   XmNleftWidget,       xg->vaxis,				      
	   XmNrightAttachment,  XmATTACH_FORM,				      
	   XmNbottomAttachment, XmATTACH_FORM,				      
	   NULL);							      
     XtAddCallback(xg->paxis,XmNexposeCallback, pexpose_cb, (XtPointer)xg);   
									      
     frame = XtVaCreateManagedWidget					      
	  ("frame", xmFrameWidgetClass, form,				      
	   XmNshadowType, XmSHADOW_IN,					      
	   XmNtopAttachment,    XmATTACH_FORM,				      
	   XmNrightAttachment,  XmATTACH_FORM,				      
	   XmNleftAttachment,   XmATTACH_WIDGET,			      
	   XmNleftWidget,       xg->vaxis,				      
	   XmNbottomAttachment, XmATTACH_WIDGET,			      
	   XmNbottomWidget,     xg->paxis,				      
	   NULL);							      
									      
     xg->gfx = XtVaCreateManagedWidget					      
	  ("gfx", B3dDrawingAreaWidgetClass, frame,			      
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
     XtAddCallback(xg->gfx,B3dNexposeCallback, expose_cb, (XtPointer)xg);     
     XtAddCallback(xg->gfx,B3dNresizeCallback, resize_cb, (XtPointer)xg);     
     XtAddCallback(xg->gfx,B3dNinputCallback,  input_cb,  (XtPointer)xg);     
									      
     XtManageChild(frame);						      
     XtManageChild(form);						      
									      
#ifdef MWSETWORK							      
     XtVaSetValues(mw,XmNworkWindow,form,NULL);				      
#endif									      
     XmMainWindowSetAreas( mw, NULL, tools, NULL, NULL, form);		      
									      
     XtManageChild(mw);							      
									      
     xg->font = b3dGetXFontStruct("9x15");				      
									      
     wmclose = XmInternAtom( XtDisplay(xg->dialog),			      
			    "WM_DELETE_WINDOW", False);			      
     XmAddWMProtocolCallback(xg->dialog, wmclose, quit_cb,		      
			     (caddr_t)xg);				      
     XtPopup(xg->dialog, XtGrabNone);					      

     /*     xg->iid = XtAppAddTimeOut
      *  (xg->app, 3000L, tupdate, (XtPointer)xg);			      
      */
	
     xg->ctrl = ivwNewControl
	  (xg->vi, graphDraw_cb, graphClose_cb,
	   (XtPointer)xg);

     return(0);								      
}									      
									      
									      
void xgraphFillData(struct imod_xgraph_struct *xg)			      
{									      
     int dsize, xsize;							      
/*     unsigned char **idata = xg->vi->idata; */
     unsigned char *image = NULL;
     int cx, cy, cz, i;							      
     int co, cc, cp;							      
     Icont *cont;
     Ipoint *pt1, *pt2;
     float xs, ys, zs;
     int   pmax, pt;
     int   di = 0;
     float x, y, z;
     int curpt;
     float frac, totlen, curint;
     Ipoint scale;
									      
     if (!xg->vi)							      
	  return;							      

     if (! (xg->data))
	  xg->dsize = 0;

     cx = xg->cx = xg->vi->xmouse;
     cy = xg->cy = xg->vi->ymouse;
     cz = xg->cz = xg->vi->zmouse;

     switch(xg->axis){
	case 0: /* X axis */
	  image = ivwGetCurrentZSection(xg->vi);
	  dsize = xg->vi->xsize;
	  if (dsize > xg->dsize){
	       if (xg->data)	free(xg->data);
	       xg->data  = (float *)malloc(dsize * sizeof(float));
	       if (!xg->data) return;
	  }
	  xg->dsize = dsize;
	  for(i = 0; i < dsize; i++) xg->data[i] = 0.0f;
	  cz = xg->vi->zmouse + 0.5f;					      
	  cy = xg->vi->ymouse + 0.5f;					      
	  xsize = cy * xg->vi->xsize;					      
	  xg->cpt = xg->cx;

	  /* DNM: skip out if outside limits */
	  if (cz < 0 || cz >= xg->vi->zsize || cy < 0 || cy >= xg->vi->ysize)
	       break;
	  if (xg->highres)						      
	       for(i = 0; i < dsize; i++)				      
		    xg->data[i] = ivwGetFileValue(xg->vi, i, cy, cz);	      
	  else      
	       for(i = 0; i < dsize; i++)
		    if (image)
			 xg->data[i] = image[i + xsize];
		    else
			xg->data[i] = xg->vi->hdr->amean;
	  break;
	  
	  
	case 1: /* Y axis */
	  image = ivwGetCurrentZSection(xg->vi);
	  dsize = xg->vi->ysize;
	  if (dsize > xg->dsize){
	       if (xg->data)	free(xg->data);
	       xg->data  = (float *)malloc(dsize * sizeof(float));
	       if (!xg->data) return;
	  }
	  xg->dsize = dsize;
	  for(i = 0; i < dsize; i++) xg->data[i] = 0.0f;
	  cz = xg->vi->zmouse + 0.5f;					      
	  xsize = xg->vi->xsize;					      
	  cx = xg->vi->xmouse + 0.5f;					      
	  xg->cpt = xg->cy;

	  /* DNM: skip out if outside limits */
	  if (cx < 0 || cx >= xg->vi->xsize || cz < 0 || cz >= xg->vi->zsize)
	       break;
	  if (xg->highres)						      
	       for(i = 0; i < dsize; i++)				      
		    xg->data[i] = ivwGetFileValue(xg->vi, cx, i, cz);	      
	  else								      
	       for(i = 0; i < dsize; i++){
		    if (image)
			 xg->data[i] = image[(i*xsize) + cx];
		    else
			xg->data[i] = xg->vi->hdr->amean;
	       }
	  break;

	case 2: /* Z axis */
	  dsize = xg->vi->zsize;
	  if (dsize > xg->dsize){
	       if (xg->data)
		    free(xg->data);
	       xg->data  = (float *)malloc(dsize * sizeof(float));
	       if (!xg->data) return;
	  }
	  xg->dsize = dsize;
	  for(i = 0; i < dsize; i++) xg->data[i] = 0.0f;
	  cx = xg->vi->xmouse + 0.5f;					      
	  cy = xg->cy + 0.5f;						      
	  xsize = cx + (cy * xg->vi->xsize);				      
	  xg->cpt = xg->cz;

	  /* DNM: skip out if outside limits */
	  if (cx < 0 || cx >= xg->vi->xsize || cy < 0 || cy >= xg->vi->ysize)
	       break;
	  if (xg->highres)						      
	       for(i = 0; i < dsize; i++)				      
		    xg->data[i] = ivwGetFileValue(xg->vi, cx, cy, i);	      
	  else								      
	       for(i = 0; i < dsize; i++)				      
		    xg->data[i] = ivwGetValue(xg->vi, cx, cy, i);
	  break;

	case 3: /* Rotate */						      
	  break;

	case 4: /* Contour : DNM got this working properly, and in 3D */
	  co = xg->co = xg->vi->imod->cindex.object;			      
	  cc = xg->cc = xg->vi->imod->cindex.contour;			      
	  cp = xg->cp = xg->vi->imod->cindex.point;			      
	  cont = imodContourGet(xg->vi->imod);		      
	  if (!cont) return;
	  if (cont->psize < 2) return;

	  /* Get true 3D length, record where current point falls */

	  totlen = 0;
	  scale.x = 1.0;
	  scale.y = 1.0;
	  scale.z = 1.0;
	  if (cp < 0) cp = 0;
	  if (cp >= cont->psize) cp = cont->psize - 1;
	  xg->cpt = 0;
	  for (i = 1; i < cont->psize; i++) {
	       totlen += imodPoint3DScaleDistance(&cont->pts[i-1],
						  &cont->pts[i], &scale);
	       if (i == cp)
		    xg->cpt = totlen + 0.5;
	  }
	  
	  dsize = totlen + 1.0;
	  if (dsize > xg->dsize){
	       if (xg->data)	free(xg->data);
	       xg->data  = (float *)malloc(dsize * sizeof(float));
	       if (!xg->data) return;
	  }
	  xg->dsize = dsize;
	  for(i = 0; i < dsize; i++) xg->data[i] = 0.0f;
	  xg->cx = cont->pts[cp].x + 0.5f;
	  xg->cy = cont->pts[cp].y + 0.5f;
	  xg->cz = cont->pts[cp].z + 0.5f;
	  cx = cont->pts->x + 0.5f;
	  cy = cont->pts->y + 0.5f;
	  cz = cont->pts->z + 0.5f;
	  if (xg->highres)
	       xg->data[0] = ivwGetFileValue(xg->vi, cx, cy, cz);
	  else if (cx >= 0 && cx < xg->vi->xsize &&
		  cy >= 0 && cy < xg->vi->ysize &&
		  cz >= 0 && cz < xg->vi->zsize)
	       xg->data[0] = ivwGetValue(xg->vi, cx, cy, cz);

	  /* Advance through data points, finding nearest pixel along each line
	     of contour */

	  totlen = 0.0;
	  pt2 = cont->pts;
	  curint = 0.0;
	  curpt = 1;
	  for (i = 1; i < dsize; i++) {

	       /* Advance as needed until i is inside the current interval */

	       while (i > totlen + curint) {
		    totlen += curint;
		    pt1 = pt2;
		    pt2 = &cont->pts[curpt++];
		    curint = imodPoint3DScaleDistance(pt1, pt2, &scale);
	       }
	       frac = 0;
	       if (curint)
		    frac = (i - totlen) / curint;
	       cx = pt1->x + frac * (pt2->x - pt1->x) + 0.5;
	       cy = pt1->y + frac * (pt2->y - pt1->y) + 0.5;
	       cz = pt1->z + frac * (pt2->z - pt1->z) + 0.5;
	       if (xg->highres)
		    xg->data[i] = ivwGetFileValue(xg->vi, cx, cy, cz);
	       else if (cx >= 0 && cx < xg->vi->xsize &&
		       cy >= 0 && cy < xg->vi->ysize &&
		       cz >= 0 && cz < xg->vi->zsize)
		    xg->data[i] = ivwGetValue(xg->vi, cx, cy, cz);
	  }

	  break;

	case 5: /* Generate histogram. */
	  image = ivwGetCurrentZSection(xg->vi);
	  if (image){
	       dsize = 256;
	       if (xg->dsize < dsize){
		    if (xg->data) free(xg->data);
		    xg->data  = (float *)malloc(dsize * sizeof(float));
		    if (!xg->data) return;
	       }
	       xg->dsize = dsize;
	       for(i = 0; i < dsize; i++)
		    xg->data[i] = 0.0f;
	       pmax = xg->vi->xsize * xg->vi->ysize;
	       cz = xg->vi->zmouse + 0.5f;
	       for(i = 0; i < pmax; i++){
		    pt = image[i];
		    xg->data[pt] += 1.0f;
	       }
	       xg->cpt = image[cx + (cy * xg->vi->xsize)];
	  }
	  break;							      
									      
	default:							      
	  break;							      
     }									      
     return;								      
}									      
									     
									     
void xgraphDraw(struct imod_xgraph_struct *xg)				     
{									      
     if (!xg->exposed)							      
	  return;							      
									      
     if (!xg->locked)							      
	  xgraphFillData(xg);						      
									      
     if (!xg->data)							      
	  return;							      
									      
     b3dWinset(XtDisplay(xg->gfx), xg->gfx, xg->context);		      
     b3dColorIndex(App->background);					      
     b3dClear();							      
     xgraphDrawPlot(xg);						      
     b3dSwapBuffers();							      
     xgraphDrawAxis(xg);						      
     return;								      
}									      
									      
									      
static void setxyz(struct imod_xgraph_struct *xg, int mx, int my)	      
{									      
     int ni = (mx/xg->zoom) + xg->start;				      
     int x,y,z;								      
									      
     ivwGetLocation(xg->vi, &x, &y, &z);				      
									      
     switch(xg->axis){							      
	case 0:								      
	  x = ni;							      
	  break;							      
	case 1:								      
	  y = ni;							      
	  break;							      
	case 2:								      
	  z = ni;
	  break;
	case 3:
	  break;
	case 4:
	  break;
	case 5:
	  break;
     }									      
     ivwSetLocation(xg->vi, x, y, z);
     ivwControlPriority(xg->vi, xg->ctrl);
     imodDraw(xg->vi, IMOD_DRAW_XYZ);
     return;								      
}									      
									      
void xgraphDrawPaxis(struct imod_xgraph_struct *xg)			      
{									      
     char pstr[16];							      

     if ((!xg->pexposed)||(!xg->paxis)||(!xg->paxis_context))
	  return;

     /* draw pixel axis */						      
     XClearWindow(XtDisplay(xg->paxis), XtWindow(xg->paxis));		      
									      
     
     b3dXWinset(XtDisplay(xg->paxis), xg->paxis, (XID)xg->paxis_context);     
     b3dXSetCurrentFont(xg->font);					      
     sprintf(pstr, "%d", xg->start);					      
     b3dXDrawString(pstr, 0, 5, XmALIGNMENT_BEGINNING);			      
     sprintf(pstr, "%d", xg->cpt);					      
     b3dXDrawString(pstr, xg->width/2, 5, XmALIGNMENT_CENTER);		      
     sprintf(pstr, "%d", xg->cpt + (xg->cpt-xg->start));		      
     b3dXDrawString(pstr, xg->width, 5, XmALIGNMENT_END);		      
									      
     return;								      
}									      
									      
void xgraphDrawVaxis(struct imod_xgraph_struct *xg)			      
{									      
     char pstr[16];							      
     
     if ((!xg->vexposed)||(!xg->vaxis)||(!xg->vaxis_context))
	  return;

     /* draw value axis */	      
     XClearWindow(XtDisplay(xg->vaxis), XtWindow(xg->vaxis));
     b3dXWinset(XtDisplay(xg->vaxis), xg->vaxis, (XID)xg->vaxis_context);
     b3dXSetCurrentFont(xg->font);
     sprintf(pstr, "%.4f", xg->min);
     b3dXDrawString(pstr, 4, XGRAPH_PAXIS_HEIGHT, XmALIGNMENT_BEGINNING);
     sprintf(pstr, "%.4f", xg->max);
     b3dXDrawString(pstr, 4, xg->height + 10, XmALIGNMENT_BEGINNING);
     return;
}


void xgraphDrawAxis(struct imod_xgraph_struct *xg)
{
     int b = 5;

     b3dColorIndex(App->foreground);

     xgraphDrawVaxis(xg);
     xgraphDrawPaxis(xg);
     return;
}

void xgraphDrawPlot(struct imod_xgraph_struct *xg)
{
     int spnt, epnt, i;
     int cpntx = xg->width/2;
     float min, max, yoffset, yscale;
     int zoom = xg->zoom;

     if (!xg->data)
	  return;

     spnt = xg->cpt - (cpntx / zoom);
     epnt = xg->cpt + (cpntx / zoom);

     b3dColorIndex(App->foreground);
     min = max = xg->data[xg->cpt];
     for(i = spnt; i < epnt; i++){
	  if (i < 0)
	       continue;
	  if (i >= xg->dsize - 2)
	       break;
	  if (xg->data[i] < min)
	       min = xg->data[i];
	  if (xg->data[i] > max)
	       max = xg->data[i];
     }
     xg->min = min;
     xg->max = max;
     yoffset = min;
     if (max-min)
	  yscale = xg->height/(max - min);
     else yscale = 1.0f;

     b3dBeginLine();
     for(i = spnt; (i <= epnt) && (i < xg->dsize); i++){
	  if (i < 0)
	       continue;
	  b3dVertex2i((int)((i - spnt) * zoom),
		      (int)((xg->data[i] - yoffset) * yscale));
     }
     b3dEndLine();

     b3dColorIndex(App->endpoint);
     b3dDrawLine((int)((xg->cpt - spnt) * zoom), 0, 
		 (int)((xg->cpt - spnt) * zoom), xg->height);

     xg->offset = yoffset;
     xg->scale  = yscale;
     xg->start  = spnt;
     return;
}

/*****************************************************************************/
/* to do: use callback routines instead of timer. */

static void addworkproc(XtPointer client, XtIntervalId *id)
{
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;

     xg->work_id = XtAppAddWorkProc(xg->app, work_update, xg);
     xg->iid = 0;
     return;
}

static void tupdate(XtPointer client, XtIntervalId *id)
{
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;
     
     if (!xg->vi)
	  return;
     if (!xg->iid)
	  return;
     if (!xg->locked)
	  if (((int)xg->cx != (int)xg->vi->xmouse) ||
	      ((int)xg->cy != (int)xg->vi->ymouse) ||
	      ((int)xg->cz != (int)xg->vi->zmouse)){
	       xgraphDraw(xg);
	  }
     
     if (xg->locked)
	  xg->iid = XtAppAddTimeOut (xg->app, 1000L, tupdate,
				     (XtPointer)xg);
     else
	  xg->iid = XtAppAddTimeOut (xg->app, 100L, tupdate,
				     (XtPointer)xg);
     return;
}

/* Workproc update function */
static Boolean work_update(XtPointer client)
{
     struct imod_xgraph_struct *xg = (struct imod_xgraph_struct *)client;

     if (((int)xg->cx != (int)xg->vi->xmouse) ||
	 ((int)xg->cy != (int)xg->vi->ymouse) ||
	 ((int)xg->cz != (int)xg->vi->zmouse)){
	  xgraphDraw(xg);
	  return(True);
     }
     
     xg->iid = XtAppAddTimeOut (xg->app, 100L, addworkproc,
				(XtPointer)xg);
     xg->work_id = 0;
     return(True);
}

void graphClose_cb(ImodView *vi, void *client, int junk)
{
     struct imod_xgraph_struct *win = (struct imod_xgraph_struct *)client;

     if (!win) return;

     /*
      *     if (win->iid)XtRemoveTimeOut(win->iid);
      *     if (win->work_id) XtRemoveWorkProc(win->work_id);
      */

     XtPopdown(win->dialog);
     if (win->context){
	  b3dWinset(XtDisplay(win->gfx), win->gfx, win->context);
	  b3dDestroyGFX();
	  win->context = 0;
	  
     }
     if (win->paxis_context){
	  XFreeGC(XtDisplay(win->paxis), win->paxis_context);
	  win->paxis_context = 0;
     }
     if (win->vaxis_context){
	  XFreeGC(XtDisplay(win->vaxis), win->vaxis_context);
			           win->vaxis_context = 0;
     }
     b3dFreeXFontStruct(win->font);
     b3dWinset(XtDisplay(win->gfx), 0, 0);
     XtDestroyWidget(win->dialog);
     free(win);
     return;
}

void graphDraw_cb(ImodView *vi, void *client, int drawflag)
{
     struct imod_xgraph_struct *win = (struct imod_xgraph_struct *)client;

     if (!win) return;
     
     b3dWinset(XtDisplay(win->gfx), win->gfx, (XID)win->context);

     if (drawflag & IMOD_DRAW_XYZ){
	  if (!win->locked){
	       if ((win->cx != win->vi->xmouse) ||
		   (win->cy != win->vi->ymouse) ||
		   (win->cz != win->vi->zmouse)){
		    xgraphDraw(win);
		    return;
	       }
	  }
     }

     if (drawflag & (IMOD_DRAW_ACTIVE | IMOD_DRAW_IMAGE)){
	  xgraphDraw(win);
	  return;
     }
     
     if (drawflag & IMOD_DRAW_MOD){
	  xgraphDraw(win);
     }
     return;
}

