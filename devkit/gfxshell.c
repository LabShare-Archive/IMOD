/*
 * Example plugin program with graphics.
 *
 */

/* include basic X-Window, Motif and OpenGL headers.
 */
#include <X11/Xlib.h>
#include <X11/keysym.h>
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <Xm/Form.h>

#include <GL/gl.h>
#include <GL/glu.h>
#include <GLwMDrawA.h>

#include "imodplug.h"

/*
 *  Define a structure to contain all local plugin data.
 */
typedef struct
{
     ImodView  *view;
     int        control; /* imod control id for this plug. */

     Widget     window;
     Widget     drawingArea;
     GLXContext context;
     
}PlugData;

/*
 *   Edit the name of the plugin here.
 */
static char *plugName = "Test graphics plug";


/*
 *  This functions doesn't need to be changed.
 */
char *imodPlugInfo(int *type)
{
     *type = IMOD_PLUG_MENU;
     return(plugName);
}

/*
 *  The main plug Draw function.
 *  Draw whatever you want using OpenGl commands.
 *  Here we draw a model.
 */
static void plugDraw(PlugData *plug)
{
     Dimension width, height;
     GLdouble depth = 5.0; /* show 10 sections worth of model data. */
     Imod *theModel = ivwGetModel(plug->view);
     Ipoint offset;

     if ((!plug) || (!plug->context))
	  return;

     /*
      *  Get drawing area size and location of the current point.
      */
     XtVaGetValues(plug->drawingArea, 
		   XmNwidth, &width, XmNheight, &height, NULL);
     ivwGetLocationPoint(plug->view, &offset);
     
     glXMakeCurrent(XtDisplay(plug->drawingArea), 
		    XtWindow(plug->drawingArea), plug->context);

     /*
      * Set up projection matrix.  This actually only needs to be done 
      * on startup and when the window is resized.
      */
     glMatrixMode(GL_PROJECTION);
     glLoadIdentity();
     glOrtho(0.0 , (GLdouble)width, 0.0, 
	     (GLdouble)height, depth, -depth);

     /*
      * Set up model matrix.  
      * 
      */
     glMatrixMode(GL_MODELVIEW);
     glPushMatrix();
     glTranslatef(width * 0.5f, height * 0.5f, 0.0f);
     glTranslatef(-offset.x, -offset.y, -offset.z);
     glClear(GL_COLOR_BUFFER_BIT);

     /* draw the model */
     imodDrawModel(theModel);

     /* reset the model matrix and show what we drew. */
     glPopMatrix();
     glXSwapBuffers(XtDisplay(plug->drawingArea), 
		    XtWindow(plug->drawingArea));
}


/*
 * imod callback functions.
 *
 */
void plugDraw_cb(ImodView *inImodView, void *client, int drawflag)
{
     PlugData *plug = (PlugData *)client;

     if (drawflag & IMOD_DRAW_MOD)
	  plugDraw(plug);
}

void plugClose_cb(ImodView *vi, void *client, int reason)
{
     PlugData *plug = (PlugData *)client;

     glXMakeCurrent(imodDisplay(), None, NULL);
     if (plug->context)
	  glXDestroyContext(imodDisplay(), plug->context);
     XtDestroyWidget(plug->window);
     free(plug);
}

/*
 *  Motif callback functions.
 *
 */
static void ginit_cb(Widget w, XtPointer client, XtPointer call)
{
     GLwDrawingAreaCallbackStruct *cbs = (GLwDrawingAreaCallbackStruct *)call;
     PlugData *plug = (PlugData *)client;
     XVisualInfo *vi;

     XtVaGetValues(w, GLwNvisualInfo, &vi, NULL);
     plug->context = glXCreateContext(XtDisplay(w), vi, NULL, GL_TRUE);
     glXMakeCurrent(XtDisplay(w), XtWindow(w), plug->context);
     glClearIndex((GLfloat)imodColorValue(COLOR_BACKGROUND));
}

/*  Window needs redrawing.
 */
static void expose_cb(Widget w, XtPointer client, XtPointer call)
{
     GLwDrawingAreaCallbackStruct *cbs = (GLwDrawingAreaCallbackStruct *)call;
     PlugData *plug = (PlugData *)client;

     if (!plug->context) 
	  return;

     plugDraw(plug);
}

/* User has resized the window. */
static void resize_cb(Widget w, XtPointer client, XtPointer call)
{
     GLwDrawingAreaCallbackStruct *cbs = (GLwDrawingAreaCallbackStruct *)call;
     PlugData *plug = (PlugData *)client;
     Dimension width, height;

     if (!plug->context)
	  return;

     XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);
     glXMakeCurrent(XtDisplay(w), XtWindow(w), plug->context);
     glViewport(0, 0, (GLsizei)width, (GLsizei)height);
     plugDraw(plug);
}

/* handle input into the drawing area. */
static void input_cb(Widget w, XtPointer client, XtPointer call)
{
     GLwDrawingAreaCallbackStruct *cbs = (GLwDrawingAreaCallbackStruct *)call;
     PlugData *plug = (PlugData *)client;
     KeySym keysym;

     switch(cbs->event->type){
	case KeyPress:
	  keysym = XLookupKeysym((XKeyEvent *)cbs->event, 0);
	  switch(keysym){

	     case XK_q:
	     case XK_Escape:
	       ivwDeleteControl(plug->view, plug->control);
	       break;

	     default: /* pass keys to imod */
	       ivwControlActive(plug->view, 0);
	       inputDefaultKeys((XKeyEvent *)cbs->event, plug->view);
	       break;
	  }


	case ButtonPress:
	  switch(cbs->event->xbutton.button){
	     case Button1:
	     case Button2:
	     case Button3:
	       break;
	  }
     }
     
}


static void quit_cb(Widget w, XtPointer client, XtPointer call)
{
     PlugData *plug = (PlugData *)client;
     ivwDeleteControl(plug->view, plug->control);
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


     /* setup a plug data structure.
      * it contains everything we need to run the plug.
      * Several windows can be opened at once this way.
      */
     plug = (PlugData *)malloc(sizeof(PlugData));
     
     plug->view    = inImodView;
     plug->context = 0;
     plug->control = ivwNewControl
	  (inImodView, plugDraw_cb, plugClose_cb, (XtPointer)plug);

     /*
      * This creates the plug window.
      */
     plug->window  = XtVaCreatePopupShell
	  (plugName, topLevelShellWidgetClass, imodTopLevel(),
	   XmNvisual, imodVisual(),
	   XtNtitle, plugName,
	   NULL);


     /*
      * Just a container widget to hold the drawing area.
      */
     form = XtVaCreateWidget
	  ("form", xmFormWidgetClass, plug->window, NULL);

     /* 
      * Create the drawing area widget using the default
      * imod visual and colormap.
      *
      */
     plug->drawingArea = XtVaCreateManagedWidget
	  ("gfx",  glwMDrawingAreaWidgetClass, form,
	   XmNbottomAttachment,   XmATTACH_FORM,
	   XmNleftAttachment,     XmATTACH_FORM,
	   XmNrightAttachment,    XmATTACH_FORM,
	   XmNtopAttachment,      XmATTACH_FORM,
	   XmNcolormap,           imodColormap(),
	   XmNdepth,              imodDepth(),
	   GLwNvisualInfo,        imodVisualInfo(),
	   GLwNinstallBackground, False,
	   GLwNinstallColormap,   False,
	   NULL);
     
     /* Setup
      * OpenGL Motif widget callback functions.
      */
     XtAddCallback(plug->drawingArea,
		   GLwNginitCallback,
		   ginit_cb, (XtPointer)plug);
     XtAddCallback(plug->drawingArea,
		   GLwNexposeCallback,
		   expose_cb, (XtPointer)plug);
     XtAddCallback(plug->drawingArea,
		   GLwNresizeCallback,
		   resize_cb, (XtPointer)plug);
     XtAddCallback(plug->drawingArea,
		   GLwNinputCallback,
		   input_cb, (XtPointer)plug);

     XtManageChild(form);


     /* Set up the quit function for the window. */
     wmclose = XmInternAtom( imodDisplay(),
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(plug->window, wmclose, quit_cb,
			     (caddr_t)plug);

     /* Open up the window. */
     XtPopup(plug->window, XtGrabNone);
}




