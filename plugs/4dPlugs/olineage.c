/*  IN TESTING VERSION 0.23
 *
 *  plugLineage.c -- Special plugin for viewing lineage type data.
 *
 *  Author: James Kremer email: kremer@colorado.edu
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
/****************************************************************************/

#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>

#include <Xm/RowColumn.h>
#include <Xm/ScrolledW.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/List.h>
#include <Xm/Separator.h>
#include <Xm/CascadeB.h>

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/GLwMDrawA.h>

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>

#include <imodel.h>
#include <dia.h>
#include "imodplug.h"
#include "ps.h"

char *lineageVersion = "Lineage Plugin Version 1.20";
static void fill_list(void);

static struct
{
     diaDialog *dia;
     ImodView *view;
     int      control;
     Widget   wList;
     int      backgroundColor;
     int      foregroundColor;
     int      hilightColor;

     int      *timeval;
     int      tmax;

}MasterWindow = {NULL, 0, 0, 0, 1, 2};


typedef struct
{
     diaDialog *dia;
     ImodView *view;
     int      mo, ob;
     Iobj     *obj;

     Widget     gfx;
     Dimension  width;
     Dimension  height;
     GLXContext context;
     Font       font;
     int        useFont;
     char       *labelBase;
     int        baseSize;
     int        maxTime;
     int        pick, xPick, yPick;
     int        lock;

     FILE *psfp;
     PS   *ps;

     float pwidth, pheight;
     float xtrans, ytrans;
     float zoom;

}LinageWindow;

Ilist *WindowList = NULL;


static void linOpen(ImodView *view);
static void linClose(ImodView *view, XtPointer client, int reason);
static void linDraw(ImodView *view, XtPointer client, int reason);
static void setLinView(LinageWindow *win);
static void linOpenWindow(ImodView *view, int ob);
static void linCloseWindow(LinageWindow *lw);
static void linDrawWindow(LinageWindow *lw);

static void drawObject(LinageWindow *win);
static void drawCurrentPoint(LinageWindow *win);
static void drawPoint(LinageWindow *win,  int time, double inX);
static void drawDeadPoint(LinageWindow *win, int time, double inX);
static void drawTimeLine(LinageWindow *win, int t1, int t2, double inX);
static void drawXLine(LinageWindow *win, int t1, double inX1, double inX2);
static void drawLabel(LinageWindow *win, char *label, int time, double inX);
static void drawTimeLabels(LinageWindow *win);

static int checkObject(LinageWindow *win);
static void getObjectInfo(LinageWindow *win);
static void lwPick(LinageWindow *win);
static int  matchLabel(char *lab1, char *lab2);
static int  findLabel(LinageWindow *win, char *inLabel, int inTime, 
	      int *outRelativeRight, int *outRelativeLeft);

/****************************************************************************/
/* Interface to IMOD */
char *imodPlugInfo(int *type)
{
    *type = IMOD_PLUG_MENU;
    return("Old Lineage View");
}

/*
 *  Execute any function or program you wish with this function.
 *  Here we open up a window for user interaction.
 *  see imodplug.h for a list of support functions.
 */

void imodPlugExecute(ImodView *inImodView)
{
     wprint("%s\n", lineageVersion);
     linOpen(inImodView);
}

/***************************************************************************/

static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     ivwDeleteControl(MasterWindow.view, MasterWindow.control);
     return;
}

static void listproc_cb(Widget w, XtPointer client, XtPointer call)
{
     XmListCallbackStruct *cbs = (XmListCallbackStruct *)call;
     int ob = cbs->item_position;

     ob -= 2;
     linOpenWindow(MasterWindow.view, ob);
}

static void fill_list(void)
{
     char label[128];
     XmString str;
     Imod *imod = ivwGetModel(MasterWindow.view);
     int ob;

     if (MasterWindow.wList)
	  XmListDeleteAllItems(MasterWindow.wList);

     for(ob = 0; ob <= imod->objsize; ob++){
	  if (ob)
	       sprintf(label, "Object %d: %s", ob,
		       imod->obj[ob-1].name);
	  else{
	       sprintf(label, "Current Object");
	  }
	  str = XmStringCreateSimple(label);
	  XmListAddItem(MasterWindow.wList, str, ob+1);
	  XmStringFree(str);
     }
}

static void lineage_workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     Widget form, list;
     
     form = XtVaCreateWidget
	  ("form", xmFormWidgetClass, w, NULL);
     
     list = MasterWindow.wList = XmCreateScrolledList
	  (form, "object_list", NULL, 0);
     
     /*for(i = 0; (proc_data[i].name); i++){
	  str = XmStringCreateSimple(proc_data[i].name);
	  XmListAddItem(list, str, i+1);
	  XmStringFree(str);
     }*/
     fill_list();

     XtVaSetValues(list,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNrightAttachment, XmATTACH_FORM,
		   XmNtopAttachment, XmATTACH_FORM,
		   XmNbottomAttachment, XmATTACH_FORM,
		   XmNvisibleItemCount, 10,
		   XmNvalue, 1,
		   XmNselectionPolicy, XmSINGLE_SELECT,
		   NULL);
     XtAddCallback(list, XmNsingleSelectionCallback, listproc_cb, client);
     XtManageChild(list);
     XtManageChild(form);
}

static int getSecondsFromLabel(char *label, int dtime)
{
    int hr,min,sec;
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
    return (sec + (min * 60) + (hr * 3600));
}

/*
 * Open master lineage control window, install callback routines.
 */
static void linOpen(ImodView *view)
{
     int t;
     if (MasterWindow.dia){
	  XRaiseWindow(XtDisplay(MasterWindow.dia->dialog),
		       XtWindow(MasterWindow.dia->dialog));
	  return;
     }
     MasterWindow.view = view;

     MasterWindow.dia = diaVaCreateDialog
	  ("Lineage View Select",
	   imodTopLevel(), imodAppContext(),
	   DiaNcontrolButton, "Done",
	   quit_cb, (XtPointer)&MasterWindow,
	   DiaNworkAreaFunc, lineage_workarea_cb, (XtPointer)0,
	   DiaNwindowQuit, quit_cb, (XtPointer)0,
	   0);

     MasterWindow.control = ivwNewControl
          (view, linDraw, linClose, (XtPointer)&MasterWindow);

    MasterWindow.tmax    = ivwGetMaxTime(view);
    MasterWindow.timeval = (int *) malloc( sizeof(int) * 
					  (MasterWindow.tmax + 2));

     for(t = 1; t <= MasterWindow.tmax; t++){
	  MasterWindow.timeval[t] = getSecondsFromLabel
	      (ivwGetTimeIndexLabel(view, t),t);
/*	  printf("%d:%s = %d\n", t, ivwGetTimeIndexLabel(view, t), 
		 MasterWindow.timeval[t]);
*/
      }
}


/* Close all lineage windows. */
static void linClose(ImodView *view, void *client, int reason)
{
     LinageWindow **lw;
     int i;

     if (MasterWindow.dia)
	  diaDestroyDialog(MasterWindow.dia);
     MasterWindow.dia = NULL;
     MasterWindow.wList = 0;
     if (MasterWindow.timeval) 
	 free (MasterWindow.timeval);
     MasterWindow.view = NULL;
/*     puts("linClose"); */

     if (!WindowList) return;
     if (WindowList->size < 1) return;
     wprint("Lineage closing %d sub windows.\n",WindowList->size);
     lw = (LinageWindow **)ilistFirst(WindowList);
     while(lw){
	  /* update all the windows. */
	  linCloseWindow(*lw);
	  lw = (LinageWindow **)ilistNext(WindowList);
     }
     ilistDelete(WindowList);
     WindowList = NULL;

}

static void linDraw(ImodView *inImodView, void *client, int drawflag)
{
     LinageWindow **lw;
     int i;
     
     if (!(drawflag & IMOD_DRAW_MOD)) return;
     
     /* update object list for current model. */
     /* update view in all lineage windows. */
     if ((WindowList) && (WindowList->size > 0)){
	  lw = (LinageWindow **)ilistFirst(WindowList);
	  while(lw){
	       /* update all the windows. */
	       linDrawWindow(*lw);
	       lw = (LinageWindow **)ilistNext(WindowList);
	  }
     }
}


/****************************************************************************/
/*********************lineage Window*****************************************/
/****************************************************************************/

/* Close the lineage window. */
static void linCloseWindow(LinageWindow *lw)
{
     LinageWindow **tval;
   
     int index = 0;
/*     puts("linCloseWindow"); */
     
     if (!lw) return;
     if (lw->dia)
	  diaDestroyDialog(lw->dia);

     tval = (LinageWindow **)ilistFirst(WindowList);
     while(tval){
	  
	  if ((XtPointer)*tval == (XtPointer)lw){
	       ilistRemove(WindowList, index);
	       free(lw);
	       return;
	  }
	  tval = ilistNext(WindowList);
	  index++;
     }

     /* todo: free the font?*/

     if (lw->labelBase)
	  free(lw->labelBase);

     free(lw);
}

static void lw_quit_cb(Widget w, XtPointer client, XtPointer call)
{
     linCloseWindow((LinageWindow *)client);
}

static void expose_cb(Widget w, XtPointer client, XtPointer call)
{
     LinageWindow *win = (LinageWindow *)client;

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
     LinageWindow *win = (LinageWindow *)client;
     XVisualInfo *vi;
     Dimension width, height;
     if (!w) return;

     XtVaGetValues(w, GLwNvisualInfo, &vi, NULL);
     XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);

     win->context = glXCreateContext(XtDisplay(w), vi, NULL, GL_TRUE);

     glXMakeCurrent(XtDisplay(w), XtWindow(w), win->context);

     setLinView(win);
}

static void resize_cb(Widget w, XtPointer client, XtPointer call)
{
     LinageWindow *win = (LinageWindow *)client;
     Dimension width, height;
     XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);
     win->width  = width;
     win->height = height;

     if (win->context){
	  glXMakeCurrent(XtDisplay(w), XtWindow(w), win->context);
	  setLinView(win);
	  linDrawWindow(win);
     }
}


static void input_cb(Widget w, XtPointer client, XtPointer call)
{
     LinageWindow *win = (LinageWindow *)client;
     GLwDrawingAreaCallbackStruct *cbs = (GLwDrawingAreaCallbackStruct *)call;
     KeySym keysym;
     
     glXMakeCurrent(XtDisplay(win->gfx), XtWindow(win->gfx), win->context);

     switch(cbs->event->type){
	case KeyPress:
	  keysym = XLookupKeysym((XKeyEvent *)cbs->event, 0);
	  switch(keysym){
	       /*case XK_minus:
	       break;
	     case XK_equal:
	       break;
	       */
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
     LinageWindow *win = (LinageWindow *)client;
     Dimension width  = 256;
     Dimension height = 256;

     win->width = width;
     win->height = height;

     /* todo make scrolled area   */
     /* XmCreateScrolledWindow(); */
     /* XmScrollbar */

/* for 8-bit displays */   
     if (imodDepth() <= 8)
	  win->gfx = XtVaCreateManagedWidget
	       ("OpenGL", glwMDrawingAreaWidgetClass, w,
		GLwNvisualInfo, imodVisualInfo(),
		XmNwidth, width,
		XmNheight, height,
		XmNnavigationType, XmNONE,
		XmNtraversalOn, True,
		XmNcolormap, imodColormap(),
		NULL);
     else
	  win->gfx = XtVaCreateManagedWidget
	       ("OpenGL", glwMDrawingAreaWidgetClass, w,
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
     return;
}

static void file_print_cb(Widget w, XtPointer client, XtPointer call)
{
    LinageWindow *win = (LinageWindow *)client;

/*    win->psfp = popen("lpr", "w");*/
    win->psfp = fopen("lineage.ps", "w");
    if (!win->psfp) return;

    /* 7.5in x 10in */
    win->ps = PSnew(win->psfp, 300.0, 0.5, 0.5);
    if (!win->ps) return;
    PSsetFont(win->ps, "Courier", 12);
    
    win->pwidth = 7.5f;
    win->pheight = 9.0f;

    drawObject(win);
    drawTimeLabels(win);

    PSclose(win->ps);
    fclose(win->psfp);
    win->ps = NULL;
}

static Widget lw_menubar_cb(Widget w, XtPointer client, XtPointer call)
{
    LinageWindow *win = (LinageWindow *)client;
    Arg args[4];
    int n = 0;
    Widget menubar, fileMenu;
    Widget item, help;
    XmString filestr = XmStringCreateSimple("File");
    
    menubar = XmCreateMenuBar(w, "menubar", NULL, 0);


#ifdef __sgi
    fileMenu = diaGetPulldownMenu(menubar, "File", 'F');
    diaGetMenuItem(fileMenu, "Print", 'P', file_print_cb, win);
    XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass,  fileMenu, NULL);
    diaGetMenuItem(fileMenu, "Close", 'Q', lw_quit_cb, win);
#else
    fileMenu = XmCreatePulldownMenu(menubar, "pulldown", args, n);
    XtVaCreateManagedWidget
	("cascade", xmCascadeButtonWidgetClass, menubar,
         XmNsubMenuId, fileMenu,
         XmNlabelString, filestr,
         XmNmnemonic, 'F',
/*         XmNdepth,    dia_getdepth(),
         XmNvisual,   dia_getvisual(),
         XmNcolormap, dia_getcolormap(),
*/
         NULL);
    item = XtVaCreateManagedWidget
        ("Print", xmPushButtonWidgetClass, fileMenu,
/*         XmNdepth,    dia_getdepth(),
         XmNvisual,   dia_getvisual(),
         XmNcolormap, dia_getcolormap(),
*/
         NULL);
    XtVaSetValues (item, XmNmnemonic, 'P', NULL);
    XtAddCallback(item, XmNactivateCallback, file_print_cb, win);

    XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass,  fileMenu, NULL);

    item = XtVaCreateManagedWidget
        ("Close", xmPushButtonWidgetClass, fileMenu,
/*         XmNdepth,    dia_getdepth(),
         XmNvisual,   dia_getvisual(),
         XmNcolormap, dia_getcolormap(),
*/
         NULL);
    XtVaSetValues (item, XmNmnemonic, 'C', NULL);
    XtAddCallback(item, XmNactivateCallback, lw_quit_cb, win);

#endif

    return menubar;
}


static void linOpenWindow(ImodView *view, int ob)
{
     char title[64];
     LinageWindow *win = (LinageWindow *)malloc(sizeof(LinageWindow));
     XtPointer    tptr = (XtPointer)win;
     Imod        *imod = ivwGetModel(MasterWindow.view);

     if (!win) return;
     win->view   = MasterWindow.view;
     win->ob      = ob;
     win->mo      = 0;
     win->obj     = &imod->obj[ob];
     win->context = 0;
     win->labelBase = NULL;
     win->pick = 0;
     win->useFont = 0;
     win->lock    = True;
     win->ps      = NULL;
     win->psfp    = NULL;
     /* todo: check for a valid object. */
     /* if (win->obj) ... */

     if (!WindowList)
	  WindowList = ilistNew(sizeof(LinageWindow *), 8);

     ilistAppend(WindowList, &tptr);

     if (win->ob == -1)
	  win->lock = False;
     if (win->lock)
	  sprintf(title, "Lineage Obj %d : %s",
		  ob+1, win->obj->name);
     else
	  sprintf(title, "Lineage Current Object");

     win->dia = diaVaCreateDialog
	  (title,
	   imodTopLevel(), imodAppContext(),
/*	   DiaNcontrolButton, "Done",*/
	   DiaNmenuBarFunc, lw_menubar_cb, (XtPointer)win,
	   lw_quit_cb, (XtPointer)win,
	   DiaNworkAreaFunc, lw_workarea_cb, (XtPointer)win,
	   DiaNwindowQuit, lw_quit_cb, (XtPointer)win,
	   0);

     win->font = XLoadFont(XtDisplay(imodTopLevel()), "9x15");
     return;
}


static void linDrawWindow(LinageWindow *win)
{
     int co, time, maxtime, t;
     Iobj *obj = win->obj;


     /* check input, clear window. */
     if ((!win)||(!win->context)) return;
     glXMakeCurrent(XtDisplay(win->gfx), XtWindow(win->gfx), win->context);

     glClearIndex(MasterWindow.backgroundColor);
     glClearColor(1.0f, 1.0f, 1.0f, 0.0f);
     glClear(GL_COLOR_BUFFER_BIT);

     /* Draw the lineage here */
     /* check obj is ok. */

/*     printf("Draw Window Lineage %d ob\n", win->ob); */
/*     glIndexi(win->imodv->cstart+ (win->imodv->cstep * win->ob));*/
     glIndexi(MasterWindow.foregroundColor);
     glColor3f(0.0f,0.0f,0.0f);

     drawObject(win);
     drawTimeLabels(win);

     if (!win->pick){
	  glIndexi(MasterWindow.hilightColor);
	  glColor3f(1.0f,0.0f,0.0f);
	  drawCurrentPoint(win);

	  glXSwapBuffers(XtDisplay(win->gfx), XtWindow(win->gfx));
     }
}

/****************************************************************************/
     
/* left side a,d,l     rightside p,v,r */


static void setLinView(LinageWindow *win)
{
     GLint vp[4];
     int width = win->width;
     int height = win->height;

     glViewport(0, 0, (GLsizei)width, (GLsizei)height);
     glMatrixMode(GL_PROJECTION);
     glLoadIdentity();

     if (win->pick){
	  glGetIntegerv(GL_VIEWPORT, vp);
	  gluPickMatrix((double)win->xPick, (double)win->yPick, 
			10.0, 10.0, vp);
     }
     
     glOrtho(0.0 , width, 0.0, height, 0.5, -0.5);
     glMatrixMode(GL_MODELVIEW);
     glLoadIdentity();

     glScalef(1.0f, -1.0f, 1.0f);
     glTranslatef(0.0f, -height, 0.0f);
}

static void drawCurrentPoint(LinageWindow *win)
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

static void drawObject(LinageWindow *win)
{
     int curTime, maxTime, nextTime;
     int right, left;
     int co, pt, i, labelSize;
     float Xmin=0.0, Xmax=1.0, Xpos;
     Icont *cont;
     char *clabel = NULL;
     int cpt, tval;


     if (checkObject(win)){
	 drawLabel(win, win->obj->name, 0, 0.5f);
	 return;
     }

     getObjectInfo(win);
     maxTime = win->maxTime;

     glPushName(-1);
     for (curTime = 1; curTime < maxTime; curTime++){
	  for(co = 0; co < win->obj->contsize; co++){
	       cont = &win->obj->cont[co];
	       if (cont->type != curTime) continue;

#ifdef PLUGDEBUG
	       printf("time %d contour %d\n", curTime, co);
#endif
	       glLoadName(co);
	       for(pt = 0; pt < cont->psize; pt++){
		    clabel = imodLabelItemGet(cont->label, pt);
		    if (!clabel) continue;
		    Xmin = 0.0f; Xmax =1.0f;
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
		    nextTime = findLabel(win, clabel, curTime,
					 &right, &left);
#ifdef PLUGDEBUG
		    printf("%d %d %d\n", nextTime, right, left); 
#endif
		    if (!nextTime){
			 
			 
			 if ( (right) || (left)){
			      drawTimeLine(win, curTime, right, Xpos);
			      drawXLine(win, right, Xmin, Xmax);
			 }else{
			      if (curTime < maxTime)
				   drawDeadPoint(win, curTime, Xpos);
			 }
			 drawLabel(win, clabel, curTime, Xpos);
			 
		    }else{
			 drawTimeLine(win, curTime, nextTime, Xpos);
		    }
		    glPushName(pt);
		    drawPoint(win, curTime, Xpos);
		    glPopName();

	       }/* point loop */
	  }/* cont loop */
     }/* time loop */
     glPopName();
}

/* returns time index of next occorence of the given label.
 * returns 0 for now exact match.
 * outRelative will contain the time index of the nearest
 * relatives.
 */
static int findLabel(LinageWindow *win, char *inLabel, int inTime, 
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
	       break;
	       
	     case 'p':
	     case 'v':
	     case 'r':
	       return(3);
	       break;
	  }
     }
     return(4);
}

/*****************************drawing stuff********************************/

double getwinx(LinageWindow *win, double inX)
{
    if (win->ps){
	return(inX * win->pwidth);
    }
    return(inX * (double)win->width);
}

double getwiny(LinageWindow *win, int time)
{
    double y;
    float ct, mt;
    
    ct = MasterWindow.timeval[time];
    mt = MasterWindow.timeval[win->maxTime];
    
    if (mt == 0.0f) return 0.0;
    
    if (win->ps){
	if (win->pheight == 0.0) return 0.0;
	y = ct / mt * win->pheight;
	y = win->pheight - y;
	return(y);
    }
    
    if (win->height == 0) return 0.0;
    y = ct * (float)win->height / mt;
    
    return(y);
}

/* 
 *  time is the time index and inX is mapped between 0.0 and 1.0 
 */
static void drawPoint(LinageWindow *win, int time, double inX)
{
     float b = 1.0f;
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

static void drawDeadPoint(LinageWindow *win, int time, double inX)
{
     float b = 4.0f;
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

static void drawTimeLabels(LinageWindow *win)
{
    int t, tmax;
    char label[32];
    int len;
    float tx, ty;

/*
     for(t = 10; t < win->maxTime; t+=10){
	  sprintf(label, "%3d", t);
	  drawLabel(win, label, t, 0.0);
     }
*/

    tmax = MasterWindow.timeval[win->maxTime];
    if (tmax < 7200)
	for(t = 0; t < tmax; t+= 300){
	    tx = 0;
	    if (t % 900){
		sprintf(label, "_");
	    }else
		sprintf(label, "%dm", t/60);
	    len = strlen(label);
	    
	    if (win->ps){
		ty = t / tmax * win->pheight;
		PSdrawText(win->ps, label, 0, ty, 0, PS_LEFT_JUST);
	    }else{
		ty = t * (float)win->height / tmax;
		
		tx += 2;
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
		ty = t / tmax * win->pheight;
		PSdrawText(win->ps, label, 0, ty, 0, PS_LEFT_JUST);
	    }else{
		ty = t * (float)win->height / tmax;
		
		tx += 2;
		glRasterPos2f(0,ty);
		glCallLists(len, GL_BYTE, label);
	    }
	}
/*
    for(t = 0; t < win->maxTime; t++){
	if (MasterWindow.timeval[t] % 900) continue;
	if (MasterWindow.timeval[t] % 3600)
	    if (MasterWindow.timeval[t] % 1800)
		drawLabel(win, "_", t, 0.0);
	    else
		drawLabel(win, "__", t, 0.0);
	else
	    drawLabel(win, ivwGetTimeIndexLabel(win->view, t), t, 0.0);
    }
*/
}

static void drawTimeLine(LinageWindow *win, int t1, int t2, double inX)
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

static void drawXLine(LinageWindow *win, int t1, double inX1, double inX2)
{
    float y     = getwiny(win, t1);
    float xcent = (inX1 + inX2) * 0.5f;
    float x1, x2;
    
    x1 = getwinx(win,(xcent + inX1) * 0.5);
    x2 = getwinx(win,(xcent + inX2) * 0.5);
    
    if (win->ps){
        PSsetPoint   (win->ps, x1, y);
        PSdrawVector (win->ps, x2, y);
    }else{
	glBegin(GL_LINES);
	glVertex2f(x1, y);
	glVertex2f(x2, y);
	glEnd();
    } 
    drawLabel(win, ivwGetTimeIndexLabel(win->view, t1), t1, xcent);
}

static void drawLabel(LinageWindow *win, char *label, int time, double inX)
{
    int len;
    float tx, ty;
    
    if (!label) return;

    len = strlen(label);
    tx  = getwinx(win, inX);
    ty  = getwiny(win, time);

     if (win->ps){
	 PSdrawText(win->ps, label, tx, ty, 0, PS_LEFT_JUST);

     }else{
	 tx += 2;
	 glRasterPos2f(tx,ty);
	 glCallLists(len, GL_BYTE, label);
     }
}

/*************************************************************************/
/* make sure object data is valid. Close window if
 * we got left with a hanging pointer.
 */
static int checkObject(LinageWindow *win)
{
     Imod *imod;
     Iobj *obj;

     imod = ivwGetModel(win->view);

     if (!win->lock){
	  win->obj = imodObjectGet(imod);
	  if (!win->obj)
	       lw_quit_cb(0, (XtPointer)win, 0);
	  win->ob = imod->cindex.object;
     }

     if (win->ob >= imod->objsize)
	  lw_quit_cb(0, (XtPointer)win, 0);
     
     obj = &imod->obj[win->ob];
     if (obj != win->obj)
	  lw_quit_cb(0, (XtPointer)win, 0);

     if (!iobjFlagTime(obj))
	  return(1);

     return(0);
}

/* fill in data needed for drawing object. */
static void getObjectInfo(LinageWindow *win)
{
     Iobj  *obj = win->obj;
     Icont *cont;
     int    co, pt, i;
     char  *clabel;
     int    maxtime = 1;
     char  *minlabel = NULL;
     int    baseSize = 0;
     char   base;

     for(co = 0; co < obj->contsize; co++){
	  cont = &obj->cont[co];
	  if (cont->type > maxtime)
	       maxtime = cont->type;
	  
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

static void lwProcessHits(LinageWindow *win, 
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
static void lwPick(LinageWindow *win)
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

     setLinView(win);

     glInitNames();
     linDrawWindow(win);
     win->pick = False;
     hits = glRenderMode( GL_RENDER );
     setLinView(win);
     lwProcessHits(win, hits, buf);

     ivwDraw(win->view, (1<<13));
}
