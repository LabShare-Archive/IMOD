/*  IMOD VERSION 2.41
 *
 *  imodv_stereo.c -- Stereo view dialog for imodv.
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

/* DNM note: fixed here, and in imod_display.c and imodv.c, so that the program
 * reads and sets resources properly, including defaults and fallbacks, for
 * the resources SGIStereoCommand and SGIResourceCommand.  The resources
 * stereoCommand and restoreCommand still exist and there are settings for
 * them, but they are not as well maintained.  Those values are used to set
 * up stcmd and mocmd, which are used only in the Stereo_FULL_SCREEN_HACK
 * (not compiled), and to set up ImodvStereoData.stereoCommand, which is also
 * never used.  The code could be structured to use alternate stereo commands
 * on other systems, but that still needs doing.  This generally needs to
 * be cleaned up and the various default settings rationalized.
 */

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/RowColumn.h>
#include <Xm/ArrowB.h>
#include <Xm/Scale.h>
#include <Xm/PushB.h>
#include <X11/IntrinsicP.h>
#include <dia.h>
#include "imodv.h"

#ifdef __sgi
#include <X11/extensions/SGIStereo.h>
#endif

static void stereoInitl(void);
static void stereoSetUp(void);

void stereoInit(int usingStereoVisual, 
		char *stereoCmd, char *restoreCmd);
void stereoEnable(void);
void stereoDisable(void);
void stereoDrawBuffer(GLenum mode);
void stereoClear(GLbitfield mask);
void stereoMakeCurrent(Display *dpy, Window win, GLXContext ctx);
void stereoDone(void);
     
     

struct{
     int       init;
     int       hw;
     diaDialog *dia;
     ImodvApp  *a;
     Widget    wlist[5];
     int       cw;
     int       omode; /* the default mode when on */

     Dimension   width, height;
     Position    x, y;
     float       rad;

     Bool        useSGIStereo;
     Display     *currentDisplay;
     Window      currentWindow;
     GLXContext  currentContext;
     GLenum      currentDrawBuffer;
     int         currentStereoBuffer;
     Bool        enabled;
     char        *stereoCommand;
     char        *restoreCommand;
     

}imodvStereoData = {0, 0, 0, 0};

void imodv_resize_cb(Widget w, XtPointer client, XtPointer call);

static Widget mkWorkArea(ImodvApp *a, Widget top);

static void help_cb()
{
     dia_vasmsg
	  ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
	   "Stereo Edit Dialog Help.\n"
	   "~~~~~~~~~~~~~~~~~~~~~~~~"
	   "\n\n",
	   "Manipulate stereo view.",
	   NULL);
     return;
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     mkWorkArea(imodvStereoData.a, w);
     return;
}

static void done_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     diaDestroyDialog(dia);

     imodvStereoData.dia = NULL;
     imodvStereoData.wlist[0] = 0;
     return;
}


/*
 * Stereo lib modified code.      DNM: 5/2/98, switched to use the SGI values
 */
void
stereoEnable(void)
{
/*  printf ("In stereoEnable, command %s\n", Imodv->SGIStereoCommand); */
/*
  if (Imodv->stereoCommand)
      system(Imodv->stereoCommand);
*/
 
  if (Imodv->SGIStereoCommand)
    system(Imodv->SGIStereoCommand);

  else
    system("/usr/gfx/setmon -n STR_TOP");
  
}

/* call to turn off stereo viewing */
void
stereoDisable(void)
{
/*  printf ("In stereoDisable, command %s\n", Imodv->SGIRestoreCommand); */
/*
  if (Imodv->restoreCommand)
    system(Imodv->restoreCommand);
*/

  if (Imodv->SGIRestoreCommand)
    system(Imodv->SGIRestoreCommand);

  else
    system("/usr/gfx/setmon -n 72HZ");
}

void stereoDrawBuffer(GLenum mode)
{

#ifdef __sgi

    imodvStereoData.currentDrawBuffer = mode;
	switch (mode) {
	  case GL_FRONT:
	  case GL_BACK:
	  case GL_FRONT_AND_BACK:
	    /*
	     ** Simultaneous drawing to both left and right buffers isn't
	     ** really possible if we don't have a stereo capable visual.
	     ** For now just fall through and use the left buffer.
	     */
	  case GL_LEFT:
	  case GL_FRONT_LEFT:
	  case GL_BACK_LEFT:
	    imodvStereoData.currentStereoBuffer = STEREO_BUFFER_LEFT;
	    break;
	  case GL_RIGHT:
	  case GL_FRONT_RIGHT:
	    imodvStereoData.currentStereoBuffer = STEREO_BUFFER_RIGHT;
	    mode = GL_FRONT;
	    break;
	  case GL_BACK_RIGHT:
	    imodvStereoData.currentStereoBuffer = STEREO_BUFFER_RIGHT;
	    mode = GL_BACK;
	    break;
	  default:
	    break;
	}
	
	if (Imodv->display && Imodv->cgfx) {
	    /* sync with GL command stream before calling X */
	    glXWaitGL();
	    XSGISetStereoBuffer(Imodv->display,
				XtWindow(Imodv->cgfx),
				imodvStereoData.currentStereoBuffer);
	    /* sync with X command stream before calling GL */
	    glXWaitX();

	}
#endif
    glDrawBuffer(mode);
}

/* call instead of glClear */
void
stereoClear(GLbitfield mask)
{
    if (imodvStereoData.useSGIStereo) {
	GLenum drawBuffer = imodvStereoData.currentDrawBuffer;
	switch (drawBuffer) {
	  case GL_FRONT:
	    stereoDrawBuffer(GL_FRONT_RIGHT);
	    glClear(mask);
	    stereoDrawBuffer(drawBuffer);
	    break;
	  case GL_BACK:
	    stereoDrawBuffer(GL_BACK_RIGHT);
	    glClear(mask);
	    stereoDrawBuffer(drawBuffer);
	    break;
	  case GL_FRONT_AND_BACK:
	    stereoDrawBuffer(GL_RIGHT);
	    glClear(mask);
	    stereoDrawBuffer(drawBuffer);
	    break;
	  case GL_LEFT:
	  case GL_FRONT_LEFT:
	  case GL_BACK_LEFT:
	  case GL_RIGHT:
	  case GL_FRONT_RIGHT:
	  case GL_BACK_RIGHT:
	  default:
	    break;
	}
    }
    glClear(mask);
}


/* call after glXMakeCurrent */
void
stereoMakeCurrent(Display *dpy, Window win, GLXContext ctx)
{
#ifdef __sgi
    if (imodvStereoData.useSGIStereo) {
	if (dpy && (dpy != imodvStereoData.currentDisplay)) {
	    int event, error;
	    /* Make sure new Display supports SGIStereo */
	    if (XSGIStereoQueryExtension(dpy, &event, &error) == False)
		{
		    dpy = NULL;
		}
	}
	if (dpy && win && (win != imodvStereoData.currentWindow)) {
	    /* Make sure new Window supports SGIStereo */
	    if (XSGIQueryStereoMode(dpy, win) == X_STEREO_UNSUPPORTED) {
		win = None;
	    }
	}
	if (ctx && (ctx != imodvStereoData.currentContext)) {
	    GLint drawBuffer;
	    glGetIntegerv(GL_DRAW_BUFFER, &drawBuffer);
	    stereoDrawBuffer((GLenum) drawBuffer);
	}
	imodvStereoData.currentDisplay = dpy;
	imodvStereoData.currentWindow = win;
	imodvStereoData.currentContext = ctx;
    }
#endif
}

#ifndef __sgi
#define  STEREO_BUFFER_NONE 0
#endif
	  
/* call before using stereo */
void stereoInit(int usingStereoVisual, char *stereoCmd, char
	   *restoreCmd)
{

    imodvStereoData.useSGIStereo = !usingStereoVisual;
    imodvStereoData.currentDisplay = NULL;
    imodvStereoData.currentWindow = None;
    imodvStereoData.currentContext = NULL;
    imodvStereoData.currentDrawBuffer = GL_NONE;
    imodvStereoData.currentStereoBuffer = STEREO_BUFFER_NONE;
    imodvStereoData.enabled = False;
    if (imodvStereoData.stereoCommand) {
	free(imodvStereoData.stereoCommand);
    }
    imodvStereoData.stereoCommand = stereoCmd ? strdup(stereoCmd) : NULL;
    if (imodvStereoData.restoreCommand) {
	free(imodvStereoData.restoreCommand);
    }
    imodvStereoData.restoreCommand = restoreCmd ? strdup(restoreCmd) : NULL;
}

/* call when done using stereo */
void
stereoDone(void)
{
    stereoDisable();
    stereoInit(True, NULL, NULL);
}


/**************************************************************************/


static void stereoSetUp(void)
{



     static Dimension width, height, border;
     Dimension sw = 1280, sh = 1024, sb = 0;
     Dimension nx, nwidth, ny , nheight;

     static Position x, y;
     Position sx = 10, sy = 30;
     static Position dx, dy;
     float scalefac;
     int configured = 0;
     
#ifdef __sgi
/*     char stcmd[] = "/usr/gfx/setmon -n STR_RECT"; */
     char *stcmd = "/usr/gfx/setmon -n STR_TOP";
     char *mocmd = "/usr/gfx/setmon -n 72HZ";

     if (Imodv->standalone){
       stcmd = Imodv->stereoCommand;
       mocmd = Imodv->restoreCommand;
/*       puts(stcmd);
       puts(mocmd);
*/
     }else{
       stcmd = ImodRes_SGIStereoCommand();
       mocmd = ImodRes_SGIRestoreCommand();
     }
#else
     char *stcmd = "true";
     char *mocmd = "true";
#endif
     diaBusyCursor(True);

#ifdef Stereo_FULL_SCREEN_HACK     
     if (Imodv->stereo == IMODV_STEREO_HW){
	  if (!imodvStereoData.hw){
	       if (!Imodv->fullscreen){
		    XtVaGetValues(Imodv->topLevel,
				  XmNwidth, &width,
				  XmNheight, &height,
				  XmNx, &x, XmNy, &y,
				  NULL);
		    XtConfigureWidget(Imodv->topLevel,
				      sx, sy,
				      sw, sh, sb);

		    
		    if (imodvStereoData.dia){
			 XtUnmapWidget(imodvStereoData.dia->dialog);
			 XtVaGetValues(imodvStereoData.dia->dialog,
				       XmNwidth, &sw,
				       XmNheight, &sh,
				       XmNx, &dx, XmNy, &dy,
				       NULL);
			 sx += 20; sy += 20;
			 sw += 1; sh += 1;
			 XtConfigureWidget(imodvStereoData.dia->dialog,
					   sx, sy,
					   sw, sh, sb);
			 XtMapWidget(imodvStereoData.dia->dialog);
		    }
		    
	       }
	       system(stcmd);
	       imodvStereoData.hw = 1;
	  }
     }else{
	  if (imodvStereoData.hw){
	       system(mocmd);
	       imodvStereoData.hw = 0;
	       if (!Imodv->fullscreen){
		    
		    XtConfigureWidget(Imodv->topLevel,
				     x, y, width, height, sb);
		    if (imodvStereoData.dia){
			 XtUnmapWidget(imodvStereoData.dia->dialog);
			 XtVaGetValues(imodvStereoData.dia->dialog,
				       XmNwidth, &sw,
				       XmNheight, &sh,
				       NULL);
			 sw -= 1; sh -= 1;
			 XtConfigureWidget(imodvStereoData.dia->dialog,
					   dx, dy,
					   sw, sh, sb);
			 XtMapWidget(imodvStereoData.dia->dialog);
		    }
	       }
	  }
     }

#else
     {
	 /* keep window in upper half of screen. */
	 XtVaGetValues(Imodv->topLevel,
		       XmNwidth, &width,
		       XmNheight, &height,
		       XmNx, &x, XmNy, &y,
		       NULL);
	 if (Imodv->stereo == IMODV_STEREO_HW){
	     imodvStereoData.hw = 1;
	     imodvStereoData.width = width;
	     imodvStereoData.height = height;
	     imodvStereoData.x = x;
	     imodvStereoData.y = y;
	     imodvStereoData.rad = Imodv->imod->view->rad;
	     scalefac = 0.5 * (width > height ? height : width) /
	                  imodvStereoData.rad;
	     ny = y;
	     nheight = height;
	     nx = x;
	     nwidth = width;
	     /* DNM: move any window that will go below edge to the top, and
		cut the width and move windows left if necessary too */
	     if (y + height > 512){  
		 ny = 10;
	     }
	     if (nheight > 484) 
	          nheight = 512;
	     if (width > 1270) {
	          nwidth = 1270;
		  scalefac = (nwidth * scalefac) / width;
	     }
	     
	     /* DNM: set the zoom and enable the stereo before configuring,
		so that the redraw will be useful, then set flag so that
		the resize events can be skipped */

	     Imodv->imod->view->rad = 0.5 * 
	            (nwidth > nheight ? nheight : nwidth) / scalefac;
	     stereoEnable();
	     if (x + nwidth > 1280)
	          nx = 1280 - nwidth;
	     if ( (y != ny) || (height != nheight) || (x != nx) || 
		 (width != nwidth)){
		 
		 XtConfigureWidget(Imodv->topLevel,
				   nx, ny, nwidth, nheight,sb);
		 configured = 1;
	     }
	 }else{
	     if (imodvStereoData.hw){
		 Imodv->imod->view->rad = imodvStereoData.rad;
		 stereoDisable();
		 XtConfigureWidget(Imodv->topLevel,
                                   imodvStereoData.x, 
				   imodvStereoData.y, 
				   imodvStereoData.width, 
				   imodvStereoData.height, sb);
		 configured = 1;
	     }
	     imodvStereoData.hw = 0;
	 }
     }
     
#endif

     diaBusyCursor(False);
     if (!configured) {
	  imodv_resize_cb(Imodv->cgfx, (XtPointer)Imodv, (XtPointer)NULL);
	  imodv_expose_cb(Imodv->cgfx, (XtPointer)Imodv, (XtPointer)NULL);
     }

}

static void stereoInitl(void)
{
/** better stereo **/
#ifdef __sgi
    imodvStereoData.useSGIStereo = 1;
#else
    imodvStereoData.useSGIStereo = 0;
#endif

    stereoInit(1,
	       Imodv->stereoCommand,
	       Imodv->restoreCommand);
    imodvStereoData.currentDisplay = Imodv->display;
    imodvStereoData.currentWindow = XtWindow(Imodv->cgfx);
    atexit(stereoHWOff);
/***/
     if (!imodvStereoData.init){
#ifdef __sgi
/*	 if (Imodv->fullscreen) */
	       imodvStereoData.omode = IMODV_STEREO_HW;
/*	  else */
#else
	       imodvStereoData.omode = IMODV_STEREO_RL;
#endif
	  imodvStereoData.a = Imodv;
	  imodvStereoData.init = True;
     }
}

void stereoHWOff(void)
{
#ifdef __sgi
     
     if (imodvStereoData.hw)
	 system(Imodv->SGIRestoreCommand);
     imodvStereoData.hw = 0;
#endif
     return;
}

void imodvStereoToggle(void)
{
     stereoInitl();

     if (Imodv->stereo != IMODV_STEREO_OFF){
	  Imodv->stereo = IMODV_STEREO_OFF;
     }else{
	  Imodv->stereo = imodvStereoData.omode;
     }

     if (imodvStereoData.dia)
	  if (imodvStereoData.wlist[0])
	       XtVaSetValues(imodvStereoData.wlist[0],
			     XmNmenuHistory, 
			     imodvStereoData.wlist[Imodv->stereo + 1],
			     NULL);
     stereoSetUp();
}

void imodvStereoEditDialog(ImodvApp *a, int state)
{
     XtPointer cbd = (XtPointer)(&imodvStereoData);

     if (!state){
	  if (imodvStereoData.dia)
	       done_cb(NULL, NULL, (XtPointer)imodvStereoData.dia);
	  return;
     }

     stereoInitl();

     if (imodvStereoData.dia){
	  XRaiseWindow(a->display, 
		       XtWindow(imodvStereoData.dia->dialog));
	  return;
     }

     imodvStereoData.dia = diaVaCreateDialog
	  ("Imodv: Stereo View", a->topLevel, a->context,
	   DiaNcontrolButton, "Done", done_cb, cbd,
	   DiaNcontrolButton, "Help", help_cb, cbd,
	   DiaNworkAreaFunc,  workarea_cb,     cbd,
	   DiaNwindowQuit,    done_cb,         cbd,
	   0);
     return;
}

/****************************************************************************/
/*  Stereo Dialog controls.                                                 */

static void stereo_cb(Widget w, XtPointer client, XtPointer call)
{

     int option = (int)client;

     switch (option) {

	case IMODV_STEREO_OFF:
	  Imodv->stereo = IMODV_STEREO_OFF;
	  break;

	case IMODV_STEREO_RL:
	  Imodv->stereo = IMODV_STEREO_RL;
	  imodvStereoData.omode = IMODV_STEREO_RL;
	  break;

	case IMODV_STEREO_TB:
	  imodvStereoData.omode = IMODV_STEREO_TB;
	  Imodv->stereo = IMODV_STEREO_TB;
	  break;

	case IMODV_STEREO_HW:
	  imodvStereoData.omode = IMODV_STEREO_HW;
	  Imodv->stereo = IMODV_STEREO_HW;
	  break;

     }
     stereoSetUp();
     return;
}

static void angle_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     ImodvApp *a = (ImodvApp *)client;

     float newval = cbs->value * 0.1f;
     a->plax = newval;
     imodvDraw(a);
     return;
}

static Widget mkWorkArea(ImodvApp *a, Widget top)
{
     Widget frame, col;

     frame = XtVaCreateWidget
	  ("frame", xmFrameWidgetClass, top, NULL);

     col = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, frame,
	   NULL);
     {
	  {
	       Widget menuWidget;
	       Arg args[4];
	       int n = 0;
	       
	       XtSetArg(args[n], XmNvisual, Imodv->visual); n++;
	       menuWidget = XmCreatePulldownMenu(col, "pulldown", args, n);
	       XtSetArg(args[n], XmNsubMenuId, menuWidget); n++;
	       
	       imodvStereoData.wlist[0] = XmCreateOptionMenu
		    (col, "option", args, n);
	       
	       imodvStereoData.wlist[1] = XtVaCreateManagedWidget
		    ("Off", xmPushButtonWidgetClass, menuWidget, NULL);
	       XtAddCallback( imodvStereoData.wlist[1], XmNactivateCallback,
			     stereo_cb, (XtPointer)IMODV_STEREO_OFF);
	       
	       imodvStereoData.wlist[2] = XtVaCreateManagedWidget
		    ("Side by Side", xmPushButtonWidgetClass, menuWidget, 
		     NULL);
	       XtAddCallback( imodvStereoData.wlist[2], XmNactivateCallback,
			     stereo_cb, (XtPointer)IMODV_STEREO_RL);
	       
	       imodvStereoData.wlist[3] = XtVaCreateManagedWidget
		    ("Top / Bottom", xmPushButtonWidgetClass, menuWidget, 
		     NULL);
	       XtAddCallback( imodvStereoData.wlist[3], XmNactivateCallback,
			     stereo_cb, (XtPointer)IMODV_STEREO_TB);


/* Stereo for SGI machines only right now. */
#ifdef LIMIT_Stereo

#ifdef __sgi

	       if (a->standalone){ 
		    imodvStereoData.wlist[4] = XtVaCreateManagedWidget
			 ("Hardware", xmPushButtonWidgetClass, 
			  menuWidget, NULL);
		    XtAddCallback( imodvStereoData.wlist[4], 
				  XmNactivateCallback,
				  stereo_cb, (XtPointer)IMODV_STEREO_HW);
	       } 
#endif

#else

	       imodvStereoData.wlist[4] = XtVaCreateManagedWidget
		   ("Hardware", xmPushButtonWidgetClass,
		    menuWidget, NULL);
	       XtAddCallback( imodvStereoData.wlist[4],
			     XmNactivateCallback,
			     stereo_cb, (XtPointer)IMODV_STEREO_HW);
	       


#endif
	       
	       XtVaSetValues(imodvStereoData.wlist[0],
			     XmNmenuHistory, 
			     imodvStereoData.wlist[Imodv->stereo+1], 
			     NULL);
	       
	       XtManageChild(imodvStereoData.wlist[0]);
	  }

	  {
	       int angle;
	       Widget wplax;
	       
	       angle = (int)(a->plax * 10.0f);
	       wplax = XtVaCreateManagedWidget
		    ("Separation",  xmScaleWidgetClass, col,
		     XmNorientation, XmHORIZONTAL,
		     XmNminimum, -100,  XmNmaximum, 100,
		     XmNvalue, angle,    
		     XmNshowValue, True,
		     XmNscaleMultiple, 1,
		     XmNdecimalPoints, 1,
		     XtVaTypedArg, XmNtitleString, XmRString, 
		     "Angle", 6,
		     NULL);
	       XtAddCallback(wplax,
			     XmNvalueChangedCallback,
			     angle_cb, (XtPointer)a);
	  }

     }
     XtManageChild(col);
     XtManageChild(frame);
     return(frame);
}     


