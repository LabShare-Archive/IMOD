/*  VERSION 1.3
 *
 *  plugLineage.c -- Special plugin for viewing lineage type data.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1997-1998 by Boulder Laboratory for 3-Dimensional Fine         *
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
 ****************************************************************************/
/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>

#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/ToggleB.h>
#include <Xm/Separator.h>
#include <Xm/CascadeB.h>
#include <Xm/Label.h>
#include <Xm/ScrolledW.h>
#include <Xm/ScrollBar.h>

#include <GL/gl.h>
#include <GL/glu.h>
#include <GLwMDrawA.h>

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include <math.h>
#include <imodel.h>
#include <dia.h>
#include "imodplug.h"
#include "ps.h"
#include "cellstat.h"

char *lineageVersion = "Lineage Plugin Version 1.30";
static void fill_list(void);

typedef struct
{
     diaDialog *dia;
     ImodView *view;
     int      control;

     int      backgroundColor;
     int      foregroundColor;
     int      hilightColor;

     int      mo, ob;
     Iobj     *obj;

     Widget     gfx;
     Widget     wScroll, wVert, wHoriz;
     Widget     wCellLabel;
     Widget     wCellStat;
     Dimension  width;
     Dimension  height;
     GLXContext context;
     Font       font;
     int        useFont;
     char       *labelBase;
     int        baseSize;
     int        maxTime;
     int        pick, xPick, yPick;
     int        lock, autoupdate;
     int        tmax, *timeval;

     FILE *psfp;
     PS   *ps;

     float pwidth, pheight;
     float dpi;
     float xtrans, ytrans;
     float zoom;

}Plug;

static void linClose(ImodView *view, XtPointer client, int reason);
static void linDraw(ImodView *view, XtPointer client, int reason);
static void linSetView(Plug *win);
static void linOpenWindow(ImodView *view, int ob);
static void linDrawWindow(Plug *lw);
static linSetScrollers(Plug *win);

static void drawObject(Plug *win);
static void drawCurrentPoint(Plug *win);
static void drawPoint(Plug *win,  int time, double inX);
static void drawDeadPoint(Plug *win, int time, double inX);
static void drawTimeLine(Plug *win, int t1, int t2, double inX);
static void drawXLine(Plug *win, int t1, double inX1, double inX2);
static void drawLabel(Plug *win, char *label, int time, double inX);
static void drawTimeLabels(Plug *win);
static void drawBorder(Plug *win);
static void drawLabelr(Plug *win, char *label, int time, double inX, 
		       int rot, int place);

static int checkObject(Plug *win);
static void getObjectInfo(Plug *win);
static void lwPick(Plug *win);
static int  matchLabel(char *lab1, char *lab2);
static int  findLabel(Plug *win, char *inLabel, int inTime, 
	      int *outRelativeRight, int *outRelativeLeft);
static int getSecondsFromLabel(char *label, int dtime);

static void zoomout_cb(Widget w, XtPointer client, XtPointer call);
static void zoomin_cb(Widget w, XtPointer client, XtPointer call);

/****************************************************************************/
/* Interface to IMOD */
char *imodPlugInfo(int *type)
{
    if (type)
	*type = IMOD_PLUG_MENU;
    return("4D Lineage View");
}

/*
 *  Execute any function or program you wish with this function.
 *  Here we open up a window for user interaction.
 *  see imodplug.h for a list of support functions.
 */

void imodPlugExecute(ImodView *inImodView)
{
     wprint("%s\n", lineageVersion);
     linOpenWindow(inImodView, -1);
}

/***************************************************************************/

/* 
 * ImodView control functions
 */
static void linClose(ImodView *view, void *client, int reason)
{
    Plug *plug = (Plug *)client;

    if (plug->dia)
	diaDestroyDialog(plug->dia);

     /* todo: free the font?*/

    if (plug->timeval)
	free(plug->timeval);
    if (plug->labelBase)
	free(plug->labelBase);
    
    free(plug);
}

static void linDraw(ImodView *inImodView, void *client, int drawflag)
{
    Plug *plug = (Plug *)client;
     
    if (!(drawflag & IMOD_DRAW_MOD)) return;
    linDrawWindow(plug);
}


/****************************************************************************/
/*********************lineage Window*****************************************/
/****************************************************************************/

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
    Plug *plug = (Plug *)client;
    ivwDeleteControl(plug->view, plug->control);
    return;
}

static void expose_cb(Widget w, XtPointer client, XtPointer call)
{
    Plug *win = (Plug *)client;

    linSetScrollers(win);
    if (win->context){
	  if (!win->useFont){
	       glXUseXFont(win->font, 32, 95, 32);
	       win->useFont = 1;
	  }
	  linDrawWindow(win);
     }
}

static void ginit_cb(Widget w, XtPointer client, XtPointer call)
{
    Plug *win = (Plug *)client;
    XVisualInfo *vi;
    Dimension width, height;
    
    if (!w) return;
    
    XtVaGetValues(w, GLwNvisualInfo, &vi, NULL);
    XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);

    win->context = glXCreateContext(XtDisplay(w), vi, NULL, GL_TRUE);

    glXMakeCurrent(XtDisplay(w), XtWindow(w), win->context);
    
    linSetView(win);
}

static linSetScrollers(Plug *win)
{
    float imgsize;
    int width, height;
    int max, dval, value;

    imgsize = win->pheight * win->dpi * win->zoom;
    height  = imgsize;

    if ((height < win->height) || (win->height <= 0)){
	XtVaSetValues(win->wVert, XmNvalue, 0, 
		      XmNminimum, 0,
		      XmNmaximum, 1,
		      XmNsliderSize, 1,
		      XmNpageIncrement, 1,
		      NULL);
    }else{
	XtVaGetValues(win->wVert, XmNmaximum, &max,
		      XmNvalue, &dval, NULL);
	if ((max)&&(dval))
	    value = ((height) * dval)/max;
	else
	    value = 0;
/*
	printf("Scroll Vert: image size = %g, win=%d, dval=%d value= %d"
	       "max =  %d/%d\n",
	       imgsize, win->height, dval, value, max, (height - win->height));
*/
	XtVaSetValues(win->wVert,
		      XmNmaximum, height,
		      XmNminimum, 0,
		      XmNincrement, 10,
		      XmNpageIncrement, win->height/4,
		      XmNsliderSize, win->height,
/*		      ((height - win->height) * win->height) / height,*/
		      XmNvalue, value,
		      NULL);

    }
    
    imgsize = win->pwidth * win->dpi * win->zoom;
    width = imgsize;

    if ((width < win->width) || (win->width <= 0)){
	XtVaSetValues(win->wHoriz, XmNvalue, 0, 
		      XmNminimum, 0,
		      XmNmaximum, 1,
		      XmNsliderSize, 1,
		      XmNpageIncrement, 1,
		      NULL);
    }else{
	XtVaGetValues(win->wHoriz, XmNmaximum, &max,
		      XmNvalue, &dval, NULL);
	if ((max)&&(dval))
	    value = (width * dval)/max;
	else
	    value = 0;
	XtVaSetValues(win->wHoriz,
		      XmNmaximum, width,
		      XmNminimum, 0,
		      XmNincrement, 10,
		      XmNpageIncrement, win->width/4, 
		      XmNsliderSize, win->width,
/*		      ((width - win->width) * win->width)/ width,*/
		      XmNvalue, value,
		      NULL);

    }
    return 0;
}

static void scroll_cb(Widget w, XtPointer client, XtPointer call)
{
    Plug *plug = (Plug *)client;
    linSetScrollers(plug);
    linSetView(plug);
    linDrawWindow(plug);
}

static void resize_cb(Widget w, XtPointer client, XtPointer call)
{
    Plug *win = (Plug *)client;
    Dimension width, height;
    XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);
    win->width  = width;
    win->height = height;
    
    linSetScrollers(win);
     if (win->context){
	  glXMakeCurrent(XtDisplay(w), XtWindow(w), win->context);
	  linSetView(win);
	  linDrawWindow(win);
     }
}


static void input_cb(Widget w, XtPointer client, XtPointer call)
{
    Plug *win = (Plug *)client;
    GLwDrawingAreaCallbackStruct *cbs = (GLwDrawingAreaCallbackStruct *)call;
    KeySym keysym;
    
    glXMakeCurrent(XtDisplay(win->gfx), XtWindow(win->gfx), win->context);

     switch(cbs->event->type){
	case KeyPress:
	  keysym = XLookupKeysym((XKeyEvent *)cbs->event, 0);
	  switch(keysym){
	       case XK_minus:
	      zoomout_cb(w, win, NULL);
	      break;
	     case XK_equal:
	      zoomin_cb(w, win, NULL);
	       break;
	  }

	case ButtonPress:
	  switch(cbs->event->xbutton.button){
	     case Button1:
	       lwPick(win);
	       break;

	     case Button2:
	       break;
	       
	     case Button3:
	       break;
  }
	  break;
     }		   

     return;
}

static void lw_workarea_cb(Widget w, XtPointer client, XtPointer call)
{

    Plug *win = (Plug *)client;
    Widget form, col;
    Dimension width  = 256;
    Dimension height = 256;

     win->width = width;
     win->height = height;

     /* todo make scrolled area   */
     /* XmCreateScrolledWindow(); */
     /* XmScrollbar */

    form = XtVaCreateWidget("form", xmFormWidgetClass, w, NULL);


    col = XtVaCreateWidget
	("col", xmRowColumnWidgetClass, form, 
	 XmNleftAttachment, XmATTACH_FORM,
	 XmNrightAttachment, XmATTACH_FORM,
	 XmNbottomAttachment, XmATTACH_FORM,
	 NULL);

    win->wCellLabel = XtVaCreateManagedWidget
	(" ", xmLabelWidgetClass, col, NULL);
    win->wCellStat = XtVaCreateManagedWidget
	(" ", xmLabelWidgetClass, col, NULL);

    XtManageChild(col);

    win->wScroll = XtVaCreateManagedWidget
	("scrollwin", xmScrolledWindowWidgetClass, form,
	 XmNleftAttachment, XmATTACH_FORM,
	 XmNrightAttachment, XmATTACH_FORM,
	 XmNtopAttachment, XmATTACH_FORM,
	 XmNbottomAttachment, XmATTACH_WIDGET,
	 XmNbottomWidget, col,
	 XmNscrollingPolicy, XmAPPLICATION_DEFINED,
	 XmNvisualPolicy, XmVARIABLE,
	 NULL);

/* for 8-bit displays */   
     if (imodDepth() <= 8)
	  win->gfx = XtVaCreateManagedWidget
	       ("OpenGL", glwMDrawingAreaWidgetClass, win->wScroll,
		GLwNvisualInfo, imodVisualInfo(),
		XmNwidth, width,
		XmNheight, height,
		XmNnavigationType, XmNONE,
		XmNtraversalOn, True,
		XmNcolormap, imodColormap(),
		NULL);
     else
	  win->gfx = XtVaCreateManagedWidget
	       ("OpenGL", glwMDrawingAreaWidgetClass, win->wScroll,
		GLwNrgba, True, 
		GLwNdoublebuffer, True, 
		XmNwidth, width,
		XmNheight, height,
		XmNnavigationType, XmNONE,
		XmNtraversalOn, True,
		NULL);

     if (!win->gfx) return;

     XtAddCallback(win->gfx, GLwNresizeCallback,
		   resize_cb, (XtPointer)win);
     XtAddCallback(win->gfx, GLwNexposeCallback,
		   expose_cb, (XtPointer)win);
     XtAddCallback(win->gfx, GLwNginitCallback,
		   ginit_cb, (XtPointer)win);
     XtAddCallback(win->gfx, GLwNinputCallback,
		   input_cb, (XtPointer)win);

    win->wVert = XtVaCreateManagedWidget
	("vsb", xmScrollBarWidgetClass, win->wScroll,
	 XmNorientation, XmVERTICAL,
	 XmNmaximum, 100,
	 XmNpageIncrement, 10,
	 XmNsliderSize, 10,
	 NULL);

    win->wHoriz = XtVaCreateManagedWidget
	("hsb", xmScrollBarWidgetClass, win->wScroll,
	 XmNorientation, XmHORIZONTAL,
	 XmNmaximum, 100,
	 XmNpageIncrement, 10,
	 XmNsliderSize, 10,
	 NULL);
		
    XmScrolledWindowSetAreas(win->wScroll, win->wHoriz, win->wVert, win->gfx);
    XtAddCallback(win->wVert, XmNvalueChangedCallback, scroll_cb, win);
    XtAddCallback(win->wHoriz, XmNvalueChangedCallback, scroll_cb, win);
    

    XtManageChild(form);
     return;
}

FILE *dia_print(char *defaultValue);
static void file_print_cb(Widget w, XtPointer client, XtPointer call)
{
    Plug *win = (Plug *)client;

/*    win->psfp = popen("lpr", "w");*/
    /*    win->psfp = fopen("lineage.ps", "w");*/

#ifdef __sgi
    win->psfp = dia_print("lp"); 
#else
    win->psfp = dia_print("lpr"); 
#endif


    if (!win->psfp) return;

    /* 7.5in x 10in */
    win->ps = PSnew(win->psfp, 300.0, 0.5, 0.5);
    if (!win->ps) return;
    PSsetFont(win->ps, "Courier", 8);
    
    drawObject(win);
    drawTimeLabels(win);

    PSclose(win->ps);
    fclose(win->psfp);
    win->ps = NULL;
}
static void lock_cb(Widget w, XtPointer client, XtPointer call)
{
    Plug *plug = (Plug *)client;
    if (plug->lock)
	plug->lock = 0;
    else
	plug->lock = 1;
}
static void autoupdate_cb(Widget w, XtPointer client, XtPointer call)
{
    Plug *plug = (Plug *)client;
    if (plug->autoupdate)
	plug->autoupdate = 0;
    else
	plug->autoupdate = 1;
}
static void zoomin_cb(Widget w, XtPointer client, XtPointer call)
{
    Plug *win = (Plug *)client;
    win->zoom *= 1.25f;
    linSetScrollers(win);
    linSetView(win);
    linDrawWindow(win);
}
static void zoomout_cb(Widget w, XtPointer client, XtPointer call)
{
    Plug *win = (Plug *)client;
    win->zoom *= 0.8f;
    linSetScrollers(win);
    linSetView(win);
    linDrawWindow(win);
}

static Widget makeItem(char *label, Widget w, char mn,
                       char *acc, char *acct)
{
    Widget item;
    XmString mstr = XmStringCreateSimple(acct);

    item = XtVaCreateManagedWidget
            (label, xmPushButtonWidgetClass, w, 
             XmNmnemonic, mn,
             XmNaccelerator, acc, 
             XmNacceleratorText, mstr,
             NULL);
    XmStringFree(mstr);
    return item;
}


static Widget lw_menubar_cb(Widget w, XtPointer client, XtPointer call)
{
    Plug *win = (Plug *)client;
    Arg args[4];
    int n = 0;
    Widget menubar, menu;
    Widget item, help;
    XmString filestr = XmStringCreateSimple("File");
    XmString viewstr = XmStringCreateSimple("View");

    XtSetArg(args[n], XmNdepth,    imodDepth()); n++;
    XtSetArg(args[n], XmNvisual,   imodVisual()); n++;
    XtSetArg(args[n], XmNcolormap, imodColormap());n++;

    menubar = XmCreateMenuBar(w, "menubar", args, n);

    menu = XmCreatePulldownMenu(menubar, "pulldown", args, n);
    {
	XtVaCreateManagedWidget
	    ("cascade", xmCascadeButtonWidgetClass, menubar,
	     XmNsubMenuId, menu,
	     XmNlabelString, filestr,
	     XmNmnemonic, 'F',
	     NULL);
	item = makeItem("Print", menu, 'P', "Ctrl<Key>P", "Ctrl-P");
	XtAddCallback(item, XmNactivateCallback, file_print_cb, win);
	
	XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, menu, NULL);
	
	item = makeItem("Close Window", menu, 'W',  "Ctrl<Key>W", "Ctrl-W");
	XtAddCallback(item, XmNactivateCallback, quit_cb, win);
    }
    menu = XmCreatePulldownMenu(menubar, "pulldown", args, n);
    {
	XtVaCreateManagedWidget
	    ("cascade", xmCascadeButtonWidgetClass, menubar,
	     XmNsubMenuId, menu,
	     XmNlabelString, viewstr,
	     XmNmnemonic, 'V',
	     NULL);
	item = XtVaCreateManagedWidget
            ("Object Lock", xmToggleButtonWidgetClass, 
	     menu, XmNmnemonic, 'L', NULL);
	XtAddCallback(item, XmNvalueChangedCallback, lock_cb, win);

	XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, menu, NULL);
	
	item = makeItem("Zoom In", menu, 'I', NULL, "=");
        XtAddCallback(item,  XmNactivateCallback, zoomin_cb, win);
        item = makeItem("Zoom Out", menu, 'O', NULL, "-");
        XtAddCallback(item,  XmNactivateCallback, zoomout_cb, win);
    }

    XmStringFree(filestr);
    XmStringFree(viewstr);
    XtManageChild(menubar);
    return menubar;
}


static void linOpenWindow(ImodView *view, int ob)
{
     int       t;
     char      title[64];
     Plug      *win;
     Imod      *imod = ivwGetModel(view);
     int       rampBase, rampSize;

     if (!view){
	 wprint("\a%s cannot open, no time data.\n", imodPlugInfo(NULL));
	 return;
     }
     t  = ivwGetMaxTime(view);
     if (t < 2){
	 wprint("\a%s cannot open, no time data.\n", imodPlugInfo(NULL));
	 return;
     }

     win = (Plug *)malloc(sizeof(Plug));
     if (!win) return;
     win->tmax      = t;
     win->view      = view;
     win->ob        = ob;
     win->mo        = 0;
     win->obj       = &imod->obj[ob];
     win->context   = 0;
     win->labelBase = NULL;
     win->pick      = 0;
     win->useFont   = 0;
     win->lock      = True;
     win->autoupdate      = True;
     win->ps        = NULL;
     win->psfp      = NULL;
     win->zoom      = 0.5f;
     win->pwidth = 7.5f;
     win->pheight = 9.0f;
     win->dpi = 72.0;

     ivwGetRamp(view, &rampBase, &rampSize);

     win->foregroundColor = rampBase;
     win->backgroundColor = rampBase + rampSize;
     win->hilightColor    = rampBase + (rampSize/2);

     /* Todo: check for a valid object. */
     /* if (win->obj) ... */

     if (win->ob == -1)
	  win->lock = False;
     sprintf(title, "Lineage View");

     win->dia = diaVaCreateDialog
	  (title,
	   imodTopLevel(), imodAppContext(),
	   DiaNmenuBarFunc, lw_menubar_cb, (XtPointer)win,
	   quit_cb, (XtPointer)win,
	   DiaNworkAreaFunc, lw_workarea_cb, (XtPointer)win,
	   DiaNwindowQuit, quit_cb, (XtPointer)win,
	   0);

      win->control = ivwNewControl
          (view, linDraw, linClose, (XtPointer)win);


     win->timeval = (int *) malloc( sizeof(int) * 
				   (win->tmax + 2));
     for(t = 1; t <= win->tmax; t++){
	 win->timeval[t] = getSecondsFromLabel
	     (ivwGetTimeIndexLabel(view, t),t);
	 
     }
     win->font = XLoadFont(XtDisplay(imodTopLevel()), "9x15");
     return;
}


static void linDrawWindow(Plug *win)
{
     int co, time, maxtime, t;
     Iobj *obj = win->obj;


     /* check input, clear window. */
     if ((!win)||(!win->context)) return;
     glXMakeCurrent(XtDisplay(win->gfx), XtWindow(win->gfx), win->context);

     glClearIndex(win->backgroundColor);
     glClearColor(1.0f, 1.0f, 1.0f, 0.0f);
     glClear(GL_COLOR_BUFFER_BIT);

     glIndexi(win->foregroundColor);
     glColor3f(0.0f,0.0f,0.0f);
     drawBorder(win);
     drawObject(win);
     drawTimeLabels(win);

     if (!win->pick){
	  glIndexi(win->hilightColor);
	  glColor3f(1.0f,0.0f,0.0f);
	  drawCurrentPoint(win);
	  glXSwapBuffers(XtDisplay(win->gfx), XtWindow(win->gfx));
     }
}

/****************************************************************************/
     
/* left side a,d,l     rightside p,v,r */


static void linSetView(Plug *win)
{
     GLint vp[4];
     int width = win->width;
     int height = win->height;
     int tx,ty;
     float imgsize;
     int isize;


     XtVaGetValues(win->wVert, XmNvalue, &ty, NULL);
     if (ty == 0){
	 imgsize = win->pheight * win->dpi * win->zoom;
	 isize  = imgsize;
	 if (isize < win->height)
	     ty = -(win->height - isize)/2;

     }else{
	 ty *=1;
     }

     XtVaGetValues(win->wHoriz, XmNvalue, &tx, NULL);
     if (tx == 0){
	 imgsize = win->pwidth * win->dpi * win->zoom;
	 isize  = imgsize;
	 if (isize < win->width)
	     tx = (win->width - isize)/2;

     }else{
	 tx *= -1;
     }

     glViewport(0, 0, (GLsizei)width, (GLsizei)height);
     glMatrixMode(GL_PROJECTION);
     glLoadIdentity();

     if (win->pick){
	  glGetIntegerv(GL_VIEWPORT, vp);
	  gluPickMatrix((double)win->xPick, (double)win->yPick, 
			10.0, 10.0, vp);
     }
     
     glOrtho(0.0, width, 0.0, height, 0.5, -0.5);
  
     glMatrixMode(GL_MODELVIEW);
     glLoadIdentity();
     glTranslatef(0.0f, height, 0.0f);
     glTranslatef((float)tx, (float)ty, 0.0f);
     glScalef(win->zoom * win->dpi, -win->zoom * win->dpi, 1.0f);


}

static void drawCurrentPoint(Plug *win)
{
     Imod  *imod;
     Iobj  *obj;
     Icont *cont;
     int   pt;
     char *clabel;
     float Xmin=0.0, Xmax=1.0, Xpos;
     int labelSize, i;

     imod = ivwGetModel(win->view);
     if (win->ob >= 0){
	  if (imod->cindex.object != win->ob)
	       return;
     }
     cont = imodContourGet(imod);
     if (!cont) return;
     pt   = imod->cindex.point;
     clabel = imodLabelItemGet(cont->label, pt);
     if (!clabel) return;
     labelSize = strlen(clabel);
     if (labelSize > win->baseSize)
	  for(i = 0; i < labelSize; i++){
	       switch(clabel[i]){
		  case 'a':
		  case 'd':
		  case 'l':
		    Xmax = (Xmax + Xmin) * 0.5f;
		    break;
		    
		  case 'p':
		  case 'v':
		  case 'r':
		    Xmin = (Xmax + Xmin) * 0.5f;
		    break;
		    
	       }
	  }
     Xpos = (Xmax + Xmin) * 0.5;
     drawDeadPoint(win, cont->type, Xpos);
     return;
}

static void drawObject(Plug *win)
{
     int curTime, maxTime, nextTime;
     int right, left;
     int co, mco, pt, i, labelSize;
     float Xmin=0.0, Xmax=1.0, Xpos;
     Icont *cont;
     char *clabel = NULL;
     int cpt, tval;

     Imod  *imod  = ivwGetModel(win->view);


     CellStatList *list;
     CellStat *cs;

     /* Only draw objects that look like lineages.
      * Check object header.
      */
     if (checkObject(win)){
	 drawLabel(win, win->obj->name, 0, 0.5f);
	 return;
     }

     /* Check contours are sorted.
      * Check label base is current.
      */
     getObjectInfo(win);
     maxTime = win->maxTime;

     list = CellStatNew();
     glPushName(-1);
     mco =  win->obj->contsize;
     for(co = 0; co < mco; co++){
	 cont = &win->obj->cont[co];
	 glLoadName(co);
	 for(pt = 0; pt < cont->psize; pt++){
	     clabel = imodLabelItemGet(cont->label, pt);
	     if (!clabel) continue;
	     Xmin = 0.0f; Xmax =1.0f;
	     labelSize = strlen(clabel);
		    
	     if (labelSize > win->baseSize){
		 for(i = 0; i < labelSize; i++){
		     switch(clabel[i]){
		       case 'a':
		       case 'd':
		       case 'l':
			 Xmax = (Xmax + Xmin) * 0.5f;
			 break;
			 
		       case 'p':
		       case 'v':
		       case 'r':
			 Xmin = (Xmax + Xmin) * 0.5f;
			 break;
			 
		     }
		 }
	    }
	     Xpos = (Xmax + Xmin) * 0.5;
	     CellStatAddLTX(list, clabel, cont->type, Xmax, Xmin);
	     glPushName(pt);
	     drawPoint(win, cont->type, Xpos);
	     glPopName();
	    /* drawTimeLine(win, cont->type, cont->type + 1, Xpos); */
	 }
     }
     glPopName();

     cont  = imodContourGet(imod);
     if (cont)
	 clabel = imodLabelItemGet(cont->label, imod->cindex.point);
     if (clabel)
	 labelSize = strlen(clabel);

     for(cs = (CellStat *)ilistFirst(list); cs;
	 cs = (CellStat *)ilistNext(list)){
	  drawTimeLine(win, cs->bornon, cs->lastseen, (cs->x+cs->y) * 0.5);

	  if (CellStatRelative(list, cs->label)){
	      if (win->ps)
		  drawLabelr(win, cs->label, cs->bornon, 
			     (cs->x+cs->y) * 0.5, 90, PS_VERT_LEFT_JUST);
	    drawXLine(win, cs->lastseen, cs->x, cs->y);
	  }else{
	      if (win->ps)
		  drawLabelr(win, cs->label, cs->lastseen, 
			     (cs->x+cs->y) * 0.5, 90, PS_VERT_RIGHT_JUST);
	    drawDeadPoint(win, cs->lastseen, (cs->x+cs->y) * 0.5);
	    
	  }

	  if ((clabel) && (!win->ps) && ((strcmp(cs->label, clabel) == 0))){
	      XmString xlabel;
	      char *bstr, *lstr;
	      char msg[256];
	      sprintf(msg, "Cell Label: %s", clabel);
	      xlabel = XmStringCreateSimple(msg);
	      XtVaSetValues(win->wCellLabel, XmNlabelString, xlabel, NULL);
	      XmStringFree(xlabel);
	      
	      bstr = ivwGetTimeIndexLabel(win->view, cs->bornon);
	      lstr = ivwGetTimeIndexLabel(win->view, cs->lastseen);
	      if ((bstr)&&(lstr))
		  sprintf(msg, "Lived from %s, to %s", bstr, lstr);
	      else
		  sprintf(msg, "Lived from %d, to %d", 
			  cs->bornon, cs->lastseen);
	      xlabel = XmStringCreateSimple(msg);
	      XtVaSetValues(win->wCellStat, XmNlabelString, xlabel, NULL);
	      XmStringFree(xlabel);
	  }
     }

     CellStatDelete(list);
}

/* returns time index of next occorence of the given label.
 * returns 0 for now exact match.
 * outRelative will contain the time index of the nearest
 * relatives.
 */
static int findLabel(Plug *win, char *inLabel, int inTime, 
	      int *outRelativeRight, int *outRelativeLeft)
{
     Iobj *obj = win->obj;
     Icont *cont;
     char *clabel;
     int curTime;
     int retval   = 0;
     int retleft  = 0;
     int retright = 0;
     int match; 
     int co, pt;

     if (inTime == win->maxTime){
	  *outRelativeRight = 0;
	  *outRelativeLeft = 0;
	  return(0);
     }

     for (curTime = inTime+1; curTime < win->maxTime; curTime++){
	  for(co = 0; co < win->obj->contsize; co++){
	       cont = &win->obj->cont[co];
	       if (cont->type != curTime) continue;
	       for(pt = 0; pt < cont->psize; pt++){
		    clabel = imodLabelItemGet(cont->label, pt);
		    if (!clabel) continue;
		    
		    match = matchLabel(inLabel, clabel);
		    switch(match){
		       case 1:
			 retval = curTime;
/*			 *outRelativeRight = 0;
			 *outRelativeLeft = 0;
			 return(retval);
*/
			 break;
		       case 2:
			 if (!retleft) retleft = curTime;
			 break;
		       case 3:
			 if (!retright) retright = curTime;
			 break;
		       case 4:
		       case 0:
		       default:
			 break;
		    }
	       }
	  }
     }
     *outRelativeLeft = retleft;
     *outRelativeRight = retright;
     return(retval);
}

/* returns 0 if no match between lab1 and lab2.
 * returns 1 if exact match.
 * returns 2 if near left relative.
 * returns 3 if near right relative.
 * returns 4 distant relative.
 */
static int matchLabel(char *lab1, char *lab2)
{
     int len1 = strlen(lab1);
     int len2 = strlen(lab2);
     int i;


     if (len1 > len2) return 0;

     for(i = 0; i < len1; i++)
	  if (lab1[i] != lab2[i])
	       return(0);

     if (len1 == len2)
	  return(1);

     if ((len1 + 1) == len2){
	  switch(lab2[i]){
	     case 'a':
	     case 'd':
	     case 'l':
	       return(2);

	     case 'p':
	     case 'v':
	     case 'r':
	       return(3);
	  }
     }
     return(4);
}

/*****************************drawing stuff********************************/

double getwinx(Plug *win, double inX)
{
    return(inX * win->pwidth);
/*    if (win->ps){
	return(inX * win->pwidth);
    }
    return(inX * (double)win->width);
*/
}

double getwiny(Plug *win, int time)
{
    double y = 0.0;
    float ct, mt;

/* 
    if (time > win->maxTime){
	wprint("Time %d out of range\n", time);
	time = win->maxTime;
    }
*/
    if (time > win->tmax) time = win->tmax;
    ct = win->timeval[time];
    mt = win->timeval[win->tmax];

/*    printf("time %d/%d -> %g/%g\n", time, win->tmax,ct, mt);*/
    if ((mt == 0.0f) ||(win->pheight == 0.0))
	return y;

    y = (ct / mt) * win->pheight;

    if (win->ps)
	y = win->pheight - y;
    
    return(y);
}

static void drawBorder(Plug *win)
{
    float xo = 0.0f;
    float xe = (float)win->pwidth;
    float yo = 0.0;
    float ye = (float)win->pheight;
    if (win->ps){
	return;
    }
    glBegin(GL_LINE_LOOP);
    glVertex2f(xo,yo);
    glVertex2f(xo,ye);
    glVertex2f(xe,ye);
    glVertex2f(xe,yo);
    glEnd();

}

/* 
 *  time is the time index and inX is mapped between 0.0 and 1.0 
 */
static void drawPoint(Plug *win, int time, double inX)
{
     float b = 1.0f/win->dpi;
     float x = getwinx(win, inX);
     float y = getwiny(win, time);

     if (win->ps){
	 PSdrawFilledCircle(win->ps, (double)x, (double)y, .015);
	 return;
     }

     glBegin(GL_LINE_LOOP);
     glVertex2f(x-b,y-b);
     glVertex2f(x+b,y-b);
     glVertex2f(x+b,y+b);
     glVertex2f(x-b,y+b);
     glEnd();
}

static void drawDeadPoint(Plug *win, int time, double inX)
{
     float b = 3.0f/win->dpi;
     float x = getwinx(win, inX);
     float y = getwiny(win, time);

     if (win->ps){
	 b = 0.05;
	 PSsetPoint(win->ps, x-b, y-b);
	 PSdrawVector(win->ps, x+b, y+b);
	 PSsetPoint(win->ps, x+b, y-b);
	 PSdrawVector(win->ps, x-b, y+b);
	 return;
     }

     glBegin(GL_LINES);
     glVertex2f(x-b,y-b);
     glVertex2f(x+b,y+b);
     glVertex2f(x+b,y-b);
     glVertex2f(x-b,y+b);
     glEnd();
}

static void drawTimeLabels(Plug *win)
{
    int t, tmax;
    char label[32];
    int len;
    float tx, ty;

    
    tmax = win->timeval[win->tmax];

    if (tmax < 7200)
	for(t = 0; t < tmax; t+= 300){
	    tx = 0;
	    if (t % 900){
		sprintf(label, "_");
	    }else
		sprintf(label, "%dm", t/60);
	    len = strlen(label);
	    
	    if (win->ps){

		ty = (t * win->pheight) / tmax;

		ty = win->pheight - ty;
		
		PSdrawText(win->ps, label, 0, ty, 0, PS_VERT_LEFT_JUST);
	    }else{
		ty = (t * (float)win->height) / tmax;
		glRasterPos2f(0,ty);
		glCallLists(len, GL_BYTE, label);
	    }
	}

    else
	for(t = 0; t < tmax; t+= 300){
	    tx = 0;
	    if (t % 3600){
		if (t % 900)
		    sprintf(label, ".");
		else
		    sprintf(label, "_");
	    }else
		sprintf(label, "%dh", t/3600);
	    len = strlen(label);

	    if (win->ps){
		ty = (t * win->pheight) / tmax;
		ty = win->pheight - ty;
		PSdrawText(win->ps, label, 0, ty, 0, PS_VERT_RIGHT_JUST);
	    }else{
		ty = (t * (float)win->pheight) / tmax;
		
		tx += 2;
		glRasterPos2f(0,ty);
		glCallLists(len, GL_BYTE, label);
	    }
	}
}

static void drawTimeLine(Plug *win, int t1, int t2, double inX)
{
    float x = getwinx(win, inX);
    float y1 = getwiny(win, t1);
    float y2 = getwiny(win, t2);

    if (win->ps){
	PSsetPoint   (win->ps, x, y1);
	PSdrawVector (win->ps, x, y2);
	return;
    }
    
    glBegin(GL_LINES);
    glVertex2f(x, y1);
    glVertex2f(x, y2);
    glEnd();
}

char *ivwGetTimeIndexLabel();

static void drawXLine(Plug *win, int t1, double inX1, double inX2)
{
    float y     = getwiny(win, t1);
    float xcent = (inX1 + inX2) * 0.5f;
    float x1, x2;
    
    x1 = getwinx(win,(xcent + inX1) * 0.5);
    x2 = getwinx(win,(xcent + inX2) * 0.5);
    
    if (win->ps){
	 char *tlabel;
	 char label[64];
	 int secs, min, hr;
	 PSsetPoint   (win->ps, x1, y);
	 PSdrawVector (win->ps, x2, y);
	 tlabel = ivwGetTimeIndexLabel(win->view, t1);
	 secs = getSecondsFromLabel(tlabel, t1);
	 hr = secs/3600;
	 min = (secs%3600)/60;
	 if (tlabel){
	      if (min < 10)
		   sprintf(label, "(%d:0%d)", hr, min);
	      else
		   sprintf(label, "(%d:%d)", hr, min);
	 }else{
	      sprintf(label, "(%d)", secs);
	 }
	 drawLabelr(win, label, t1, xcent, 
		    90, PS_VERT_RIGHT_JUST);
    }else{
	glBegin(GL_LINES);
	glVertex2f(x1, y);
	glVertex2f(x2, y);
	glEnd();
    } 

}

static void drawLabelr(Plug *win, char *label, int time, double inX, 
		       int rot, int place)
{
    int len;
    float tx, ty;
    char *slabel;
    if (!label) return;

    len = strlen(label);
    tx  = getwinx(win, inX);
    ty  = getwiny(win, time);
    slabel = (char *)malloc(len + 4);
    if (!slabel) return;
    slabel[0] = ' ';
    slabel[1] = 0x00;
    strcat(slabel, label);
    
    slabel[len+1] = ' ';
    slabel[len+2] = 0x00;

    if (win->ps){

	PSdrawText(win->ps, slabel, tx, ty, rot, place);

    }else{
	glRasterPos2f(tx,ty);
	glCallLists(len+1, GL_BYTE, slabel);
    }
    free(slabel);
}


static void drawLabel(Plug *win, char *label, int time, double inX)
{
    int len;
    float tx, ty;
    char *slabel;
    if (!label) return;

    len = strlen(label);
    tx  = getwinx(win, inX);
    ty  = getwiny(win, time);
    slabel = (char *)malloc(len + 4);
    if (!slabel) return;
    slabel[0] = ' ';
    slabel[1] = 0x00;
    strcat(slabel, label);    
    
     if (win->ps){

	 PSdrawText(win->ps, slabel, tx, ty, 0, PS_LEFT_JUST);

     }else{
	 glRasterPos2f(tx,ty);
	 glCallLists(len+1, GL_BYTE, slabel);
     }
    free(slabel);
}

/*************************************************************************/
/* make sure object data is valid. Close window if
 * we got left with a hanging pointer.
 */
static int checkObject(Plug *win)
{
     Imod *imod;
     Iobj *obj;

     imod = ivwGetModel(win->view);

     if (!win->lock){
	  win->obj = imodObjectGet(imod);
	  if (!win->obj)
	      quit_cb(0, (XtPointer)win, 0);
	  win->ob = imod->cindex.object;
     }

     if (win->ob >= imod->objsize)
	  quit_cb(0, (XtPointer)win, 0);
     
     obj = &imod->obj[win->ob];
     if (obj != win->obj)
	  quit_cb(0, (XtPointer)win, 0);

     if (!iobjFlagTime(obj))
	  return(1);

     return(0);
}
int linCompCont(Icont *c1, Icont *c2)
{
    if (c1->type < c2->type)
	return -1;
    if (c1->type > c2->type)
	return 1;
    return 0;
}
/* fill in data needed for drawing object. */
static void getObjectInfo(Plug *win)
{
     Iobj  *obj = win->obj;
     Icont *cont;
     int    co, mco, pt, i;
     char  *clabel;
     int    maxtime = 1;
     int    mintime;
     char  *minlabel = NULL;
     int    baseSize = 0;
     char   base;

     /* We need min and max time.
      * Contours in object should be sorted by time. 
      */
     mco = obj->contsize - 1;
     for(co = 0; co < mco; co++){
	 if (obj->cont[co].type > obj->cont[co +1].type){
	     qsort(obj->cont, obj->contsize, sizeof(Icont),
		   (int(*)(const void *, const void *))linCompCont);
	     break;
	 }
     }
     maxtime = obj->cont[mco].type;
     mintime = obj->cont->type;
     
     /* find common label at mintime */
     for (co = 0; co < mco; co++){
	 cont = &obj->cont[co];
	 if (cont->type > mintime) 
	     break;
	 if (!cont->label) continue;
	 for(pt = 0; pt < cont->psize; pt++){
	     clabel = imodLabelItemGet(cont->label, pt);
	     if (clabel){
		 if (minlabel){
		     if (strlen(clabel) < baseSize){
			 minlabel = clabel;
			 baseSize = strlen(minlabel);
		     }
		 }else{
		     minlabel = clabel;
		     baseSize = strlen(minlabel);
		 }
	     }
	 }
     }

     /* trim the base size to common sub size. */
     for(i = 0; i < baseSize; i++){
	  base = 0;
	  for(co = 0; co < obj->contsize; co++){
	       cont = &obj->cont[co];
	        if (cont->type > mintime) 
		    break;
	       if (!cont->label) continue;
	       for(pt = 0; pt < cont->psize; pt++){
		    clabel = imodLabelItemGet(cont->label, pt);
		    if (!clabel) continue;
		    if (!base) base = clabel[i];
		    if (clabel[i] != base){
			 baseSize = i;
			 co = obj->contsize;
			 break;
		    }
	       }
	  }
     }
     
     win->baseSize = baseSize;
     win->maxTime  = maxtime;
     
     if (win->labelBase)
	  free(win->labelBase);
     win->labelBase = (char *)malloc(baseSize +1);
     memcpy(win->labelBase,clabel,baseSize+1);
}

static void lwProcessHits(Plug *win, 
			  GLint hits, GLuint buffer[])
{
     Imod *imod;
     unsigned int i, j;
     GLuint names, *ptr;
     int z1, z2, name;
     int co = -1, pt = -1;
     int ob = win->ob;

     if (hits <= 0) return;
     ptr = buffer;
     for (i = 0; i < hits; i++) {    /* for each hit */
	  names = *ptr; ptr++;
	  z1 = *ptr; ptr++;
	  z2 = *ptr; ptr++;
	  for (j = 0; j < names; j++) {   /*  for each name */
	       name = *ptr; ptr++;
	       if (names > 1)
		    switch(j){
		       case 0:
			 co = name; 
			 break;
		       case 1:
			 pt = name;
			 break;
			 
		    }
	       
	  }

     }
     imod = ivwGetModel(win->view);
     imodSetIndex(imod, ob, co, pt);
/*     printf("selected contour %d point %d\n", co, pt); */
}

#define SELECT_BUFSIZE 1024
static void lwPick(Plug *win)
{
     static unsigned int buf[SELECT_BUFSIZE];
     GLint hits;
     Icont *cont;
     Window rootr, childr;
     int rx, ry;
     unsigned int maskr;
     int wx, wy;
     XQueryPointer(XtDisplay(win->gfx),
		   XtWindow(win->gfx), &rootr, &childr,
		   &rx, &ry, &wx, &wy, &maskr);

     win->pick = True;
     win->xPick = wx;
     win->yPick = win->height - wy;

     glSelectBuffer(SELECT_BUFSIZE, buf);
     glRenderMode( GL_SELECT);

     linSetView(win);

     glInitNames();
     linDrawWindow(win);
     win->pick = False;
     hits = glRenderMode( GL_RENDER );
     linSetView(win);
     lwProcessHits(win, hits, buf);
     ivwDraw(win->view, IMOD_DRAW_XYZ|IMOD_DRAW_MOD|IMOD_DRAW_RETHINK);
}

static int getSecondsFromLabel(char *label, int dtime)
{
    int hr=0,min=0,sec=dtime;
    if (label){
	hr = atoi(label);
	while(label[0] != ':'){
	    if (!label[0])return dtime;
	    label++;
	}
	label++;
	min = atoi(label);
	while(label[0] != ':'){
	    if (!label[0])return dtime;
	    label++;
	}
	label++;
	sec = atoi(label);
    }
/*    printf("timei %d = %ds\n", dtime, sec + (min * 60) + (hr * 3600));*/
    return (sec + (min * 60) + (hr * 3600));
}

