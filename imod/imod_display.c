/*  IMOD VERSION 2.50
 *
 *  imod_display.c -- Open the display and setup visual and colormaps.
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

#include <X11/Xlib.h>
#include <X11/IntrinsicP.h>
#include <Xm/Xm.h>
#include <GL/glx.h>

#include "imod.h"
#include "b3dgfx.h"

extern int Imodv_window;
extern int Imod_debug;

#define cursor_width 15
#define cursor_height 15
#define cursor_x_hot 7
#define cursor_y_hot 7
/* Cursor with 2-pixel tails */
/* static unsigned char cursor_bits[] = {
   0x00, 0x00, 0xe0, 0x03, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x82, 0x20,
   0x02, 0x20, 0x3e, 0x3e, 0x02, 0x20, 0x82, 0x20, 0x80, 0x00, 0x80, 0x00,
   0x80, 0x00, 0xe0, 0x03, 0x00, 0x00};
static unsigned char cursor_mask_bits[] = {
   0xe0, 0x03, 0xe0, 0x03, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x83, 0x60,
   0x03, 0x60, 0x3f, 0x7e, 0x03, 0x60, 0x83, 0x60, 0x80, 0x00, 0x80, 0x00,
   0x80, 0x00, 0xe0, 0x03, 0xe0, 0x03}; */
/* Cursor with 1-pixel tails */
/*static unsigned char cursor_bits[] = {
   0x00, 0x00, 0xc0, 0x01, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x02, 0x20, 0x3e, 0x3e, 0x02, 0x20, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x80, 0x00, 0xc0, 0x01, 0x00, 0x00};
static unsigned char cursor_mask_bits[] = {
   0xc0, 0x01, 0xc0, 0x01, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x03, 0x60, 0x3f, 0x7e, 0x03, 0x60, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x80, 0x00, 0xc0, 0x01, 0xc0, 0x01}; */

/* Longer red cross with 1-pixel white tails */
static unsigned char cursor_bits[] = {
   0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x00, 0x00, 0x3f, 0x7e, 0x00, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x80, 0x00, 0x80, 0x00, 0x80, 0x00};
static unsigned char cursor_mask_bits[] = {
   0xc0, 0x01, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x01, 0x40, 0x3f, 0x7e, 0x01, 0x40, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
   0x80, 0x00, 0x80, 0x00, 0xc0, 0x01};

/* Cursor with no tails */
/*static unsigned char cursor_bits[] = {
     0x00, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
     0x00, 0x00, 0x3e, 0x3e, 0x00, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
     0x80, 0x00, 0x80, 0x00, 0x00, 0x00};
static unsigned char cursor_mask_bits[] = {
     0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
     0x00, 0x00, 0x3f, 0x7e, 0x00, 0x00, 0x80, 0x00, 0x80, 0x00, 0x80, 0x00,
     0x80, 0x00, 0x80, 0x00, 0x80, 0x00}; */

static int imod_init_cursors(ImodApp *ap);

void zapKeyInput(Widget w, XEvent *event, String par, Cardinal num);
void sliceKeyInput(Widget w, XEvent *event, String par, Cardinal num);
void defaultKeyInput(Widget w, XEvent *event, String par, Cardinal num);
int imod_get_colormap(ImodApp *ap);
void xyzDraw_cb(ImodView *vi, void *client, long drawflag);

static XtActionsRec ActionTable[] = {
{"zapKeyInput",     (XtActionProc)zapKeyInput     },
{"sliceKeyInput",   (XtActionProc)sliceKeyInput   },
{"defaultKeyInput", (XtActionProc)defaultKeyInput },
};


/* DNM: These fallback resources are needed in case there is no app-default
   file */

static String Fallback_resources[] = {
#ifdef __sgi
     "*sgiMode: true", 
     "*useSchemes: all", 
     "*SGIStereoCommand: /usr/gfx/setmon -n STR_TOP",
     "*SGIRestoreCommand: /usr/gfx/setmon -n 72HZ",
#endif
     NULL,
};

/* DNM: The initial values in here SHOULDN'T matter if we get resources */

ImodResourceStruct ImodResource = {NULL, 0
#ifdef __sgi
				   , "/usr/gfx/setmon -n STR_TOP", 
				   "/usr/gfx/setmon -n 72HZ"
#endif
};

static XrmOptionDescRec Imod_options[] = {
    {"-rbg",       "*renderbackground", XrmoptionSepArg, "black"},
};

/* DNM: There are default settings for the the stereo commands in case there
   is an app-default file that doesn't define these resources, since the
   fallback resources don't cover that case */

static XtResource Imod_resources[] = 
{

/*     {"wholeZoom", XtRBool */

    {"renderbackground", "RenderBackground", XmRString, sizeof(XColor),
     XtOffsetOf(ImodResourceStruct, rbgname), XmRImmediate, "black"},

#ifdef __sgi
    { "SGIStereoCommand",
      XtCString, XtRString, sizeof (_XtString),
      XtOffsetOf(ImodResourceStruct, SGIStereoCommand),
      XtRString, "/usr/gfx/setmon -n STR_TOP" },
    { "SGIRestoreCommand",
      XtCString, XtRString, sizeof (_XtString),
      XtOffsetOf(ImodResourceStruct, SGIRestoreCommand),
      XtRString, "/usr/gfx/setmon -n 72HZ" }
#endif
};

char *ImodRes_SGIStereoCommand(void)
{
#ifdef __sgi
  return(ImodResource.SGIStereoCommand);
#else
  return(" ");
#endif
}
char *ImodRes_SGIRestoreCommand(void)
{
#ifdef __sgi
  return(ImodResource.SGIRestoreCommand);
#else
  return(" ");
#endif
}

/* the default graphics rendering attributes for OpenGL */
/* DNM 3/12/01: switch to an arbitrarily large set of attributes; list each
   one with doublebuffer first and then use each one twice, first with then
   without doublebuffering */
static int Pseudo12[] = 
{
     GLX_DOUBLEBUFFER,
     GLX_BUFFER_SIZE, 12,
     GLX_DEPTH_SIZE, 0,
     None
};

static int Pseudo8[] = 
{
     GLX_DOUBLEBUFFER,
     GLX_DEPTH_SIZE, 0,
     GLX_BUFFER_SIZE, 8,
     None
};

/*
 * This visual is for displays that only have TrueColor or DirectColor
 * DNM: force it to be TrueColor on SGI, since the code doesn't handle 
 * DirectColor;
 * eliminate DEPTH_SIZE since it's overriden by RGBA, and put in some minimal
 * R, G, B sizes in the second selection to 
 * force us past an 8-bit RGB visual, which is pretty bad
 */
static int True24[] =
{
    GLX_DOUBLEBUFFER, 
#ifdef __sgi
    GLX_X_VISUAL_TYPE_EXT, GLX_TRUE_COLOR_EXT, 
#endif
    GLX_RGBA,
    GLX_RED_SIZE, 8,
    GLX_GREEN_SIZE, 8,
    GLX_BLUE_SIZE, 8,
    None
};
static int True12[] =
{
    GLX_DOUBLEBUFFER, 
#ifdef __sgi
    GLX_X_VISUAL_TYPE_EXT, GLX_TRUE_COLOR_EXT, 
#endif
    GLX_RGBA,
    GLX_RED_SIZE, 4,
    GLX_GREEN_SIZE, 4,
    GLX_BLUE_SIZE, 4,
    None
};

/* List the attribute lists in order of priority, indicate if they are double
   buffer or truecolor (rgb) type */
static int *OpenGLAttribList[] = {
     Pseudo12, Pseudo12 + 1, True24, True24 + 1,
     Pseudo8, Pseudo8 + 1, True12, True12 + 1,
     NULL
};
static int AttribDB[] = {1, 0, 1, 0, 1, 0, 1, 0};
static int AttribRGB[] = {0, 0, 1, 1, 0, 0, 1, 1};

/* open up the display for imod.                               */
/* check that display has minimum 8-bit Pseudo Color graphics. */

void stereoOff(void)
{
     stereoHardware(0,0);
}

/* Make sure this is not an Octane 2 with broken pseudocolor (?)
   Need to create a graphic context before it will return strings
   correctly.
   Adapted from glxvisuals.c, Copyright (c) Mark J. Kilgard, 1996. */
int pseudo_broken(ImodApp *ap, XVisualInfo *visualToTry)
{
     GLXContext context;
     Window window;
     Colormap colormap;
     XSetWindowAttributes swa;
     Display *dpy = ap->display;
     char *vendor, *renderer;

     context = glXCreateContext(dpy, visualToTry, 0, GL_TRUE);
     colormap = XCreateColormap(dpy, RootWindow(dpy, visualToTry->screen),
				visualToTry->visual, AllocNone);
     swa.colormap = colormap;
     swa.border_pixel = 0;
     window = XCreateWindow(dpy, RootWindow(dpy, visualToTry->screen), 0, 0,
			    100, 100, 0, visualToTry->depth, InputOutput,
			    visualToTry->visual,
			    CWBorderPixel | CWColormap, &swa);
     glXMakeCurrent(dpy, window, context);
     vendor = (char *)glGetString(GL_VENDOR);
     renderer = (char *)glGetString(GL_RENDERER);
     if (!strcmp(vendor, "SGI") && !strncmp(renderer, "VPRO", 4))
	  ap->rgba = 1;
	       
     XDestroyWindow(dpy, window);
     XFreeColormap(dpy, colormap);
     glXDestroyContext(dpy, context);
     return ap->rgba;
}

int imod_display_init(ImodApp *ap, char **argv, int *argc)
{
     Window   window;
     Screen  *screen;
     XColor color;
     int depth, screen_num = 0;

     XVisualInfo *vlist;
     XVisualInfo vistemp;
     int i, vsize, v = 0;
     unsigned long plane[1];
     unsigned long *pixels;
     int maxindex, dindex;
     char *vendor, *renderer;

     ap->toplevel = XtVaAppInitialize
	  (&(ap->context), "Imod", NULL, 0, argc, argv, 
	   Fallback_resources, NULL);
     
     if (!(ap->toplevel)){
	  fprintf(stderr, "%s: couldn't open display.\n", argv[0]);
	  exit(-1);
     }

     XtVaGetApplicationResources
	  (ap->toplevel, (XtPointer)(&ImodResource),
	   Imod_resources, XtNumber(Imod_resources),
	   NULL);

     XtVaSetValues(ap->toplevel, XmNdeleteResponse, XmDO_NOTHING, NULL);

     ap->display = XtDisplay(ap->toplevel);
     screen_num  = DefaultScreen(ap->display);
     screen      = DefaultScreenOfDisplay(ap->display);
     window      = RootWindowOfScreen(screen);
     depth       = DefaultDepthOfScreen(screen);
     ap->visual  = DefaultVisualOfScreen(screen); 
     ap->cmap    = DefaultColormapOfScreen(screen); 

     /* Find visualinfo of default visual */
     vistemp.screen = screen_num;
     vlist = XGetVisualInfo(ap->display, VisualScreenMask, &vistemp, &vsize);
     for (v = 0; v < vsize; v++)
	  if (vlist[v].visual == ap->visual) {
	       ap->visualinfo = &vlist[v];
	       break;
	  }

     ap->wzoom   = 1;

     /* ap->rgba is set by main program, use it to constrain choices */

     ap->visualinfoGL = NULL;
     for (i = 0; OpenGLAttribList[i] != NULL; i++) {

	  /* If want an rgb visual and this is not one, skip */
	  if (ap->rgba && !AttribRGB[i])
	       continue;
	  ap->visualinfoGL = glXChooseVisual(ap->display, screen_num,
					     OpenGLAttribList[i]);

	  /* If it returns one, and it is rgb, make sure depth is at least
	     16 and it is TrueColor, since program can't handle DirectColor */
	  if (ap->visualinfoGL && 
	      ((!AttribRGB[i] && !pseudo_broken(ap, ap->visualinfoGL))
	       || (ap->visualinfoGL->depth >= 16
			     && ap->visualinfoGL->class == TrueColor))) {
	       ap->doublebuffer = AttribDB[i];
	       ap->rgba = AttribRGB[i];
	       break;
	  }
	  ap->visualinfoGL = NULL;
     }
     if (!ap->visualinfoGL) {
	  fprintf(stderr, "%s: couldn't get rendering visual info.\n",
		  argv[0]);
	  exit(-1);
     }

     if (Imod_debug) {
	  printf("default class %d, depth %d, ID 0x%x\n", ap->visualinfo->class, 
		 ap->visualinfo->depth, ap->visualinfo->visualid);
	  printf("GL visual class %d, depth %d, ID 0x%x, rgba %d, doublebuffer %d\n",
		 ap->visualinfoGL->class, ap->visualinfoGL->depth, 
		 ap->visualinfoGL->visualid, ap->rgba,
		 ap->doublebuffer);
     }

     ap->visualGL = ap->visualinfoGL->visual;
     ap->depth = ap->visualinfoGL->depth;
     imod_get_colormap(ap);

     /* If not rgba, set the application to have the same visual and cmap as
	the GL windows need, so color map can be fully shared */
     if (!ap->rgba) {
	  ap->cmap = ap->cmapGL;
	  ap->visualinfo = ap->visualinfoGL;
	  ap->visual = ap->visualGL;
	  XtVaSetValues(ap->toplevel, 
			XmNdepth, ap->depth,
			XmNvisual, ap->visual,
			XmNcolormap, ap->cmap, NULL);
     } 

     dia_xinit(ap->toplevel,ap->context, "Imod");
     XtAppAddActions(ap->context, ActionTable, XtNumber(ActionTable));
     atexit(stereoOff);
     return(0);
}

/* sets the color index of the given object number. */
void imodSetObjectColor(int ob)
{
     Iobj *obj;

     /* check that ob is within range. */
     if (ob < 0)
	  return;
     if (ob >= App->cvi->imod->objsize)
	  return;
     
     obj = &(App->cvi->imod->obj[ob]);

     if (App->rgba){
	  glColor3f(obj->red, obj->green, obj->blue);
	  return;
     }

     if (App->depth <= 8){
	  obj->fgcolor = App->objbase - ob;
	  b3dColorIndex(App->objbase - ob);
     }else{
	  b3dColorIndex(ob + App->objbase);
	  obj->fgcolor = App->objbase + ob;
     }
     return;
}


/* replacement IrisGL function  */
/* changes color of given pixel */
int mapcolor(unsigned long color, 
	     unsigned short red, 
	     unsigned short green, 
	     unsigned short blue)
{
     XColor c;

     if (App->rgba) return 1;
     c.flags    = DoRed | DoGreen | DoBlue;
     c.pixel    = color;
     c.red   = red << 8;
     c.green = green << 8;
     c.blue  = blue << 8;
     XStoreColor(App->display, App->cmap, &c);
     if (App->cmap != App->cmapGL)
	  XStoreColor(App->display, App->cmapGL, &c);

     /* DNM: let's try to skip this forbidden step! */
     /*     XInstallColormap(App->display, App->cmap); */
     return(0);
}

/* A routine to allocate colors in TrueColor instead of mapping */
static int alloc_color(unsigned int *color, 
		     unsigned short red, 
		     unsigned short green, 
		     unsigned short blue)
{
     XColor c;
     c.flags    = DoRed | DoGreen | DoBlue;
     c.red   = red << 8;
     c.green = green << 8;
     c.blue  = blue << 8;
     if (!XAllocColor(App->display, App->cmap, &c))
	  return 1;
     *color = c.pixel;
     return 0;
}

/* Allocate object colors for one or more objects in the model, store pixel
   values properly */
int alloc_object_colors(Imod *m, int obstart, int obend)
{
     int i;
     unsigned short red, green, blue;
     for (i = obstart; i <= obend; i++) {
	  red   = m->obj[i].red * 255.0;
	  green = m->obj[i].green * 255.0;
	  blue  = m->obj[i].blue * 255.0;
	  if (alloc_color(&m->obj[i].fgcolor, red, green,
			  blue))
	       return 1;
     }
     return 0;
}

/* Free object colors for one or more objects in the model */
int free_object_colors(Imod *m, int obstart, int obend)
{
     int i;
     for (i = obstart; i <= obend; i++) {
	  if (!XFreeColors(App->display, App->cmap, 
			   (unsigned long *)&m->obj[i].fgcolor, 1, 0))
	       return 1;
     }
     return 0;
}


/* setup the colors to be used. */
int imod_color_init(ImodApp *ap)
{

     unsigned long plane[1];

     ap->curobj       = IMOD_CUROBJ;
     ap->background   = IMOD_BACKGROUND;
     ap->foreground   = IMOD_FOREGROUND;
     ap->select       = IMOD_SELECT;
     ap->shadow       = IMOD_SHADOW;
     ap->endpoint     = IMOD_ENDPOINT;
     ap->bgnpoint     = IMOD_BGNPOINT;
     ap->curpoint     = IMOD_CURPOINT;
     ap->ghost        = IMOD_GHOST;
     ap->imodvbgcolor = IMOD_VIEWBG;
     ap->objbase      = RAMPMIN - 1;


     if (ap->rgba){

	  /* get pixel values for the various colors */
	  alloc_color(&App->select,     255, 255,   0);
	  alloc_color(&App->shadow,     128, 128,   0);
	  alloc_color(&App->endpoint,   255,   0,   0);
	  alloc_color(&App->bgnpoint,     0, 255,   0);
	  alloc_color(&App->curpoint,   255,   0,   0);
	  alloc_color(&App->foreground, 255, 255, 128);
	  alloc_color(&App->background,  64,  64,  96);
	  alloc_color(&App->ghost,       16, 16, 16);
	  alloc_color(&App->imodvbgcolor,  0,  0,  0);

	  /* Get the pixel values for the model objects */
	  if (ap->cvi->imod){
	       ap->cvi->black = ap->cvi->imod->blacklevel;
	       ap->cvi->white = ap->cvi->imod->whitelevel;
	       alloc_object_colors(ap->cvi->imod, 0, 
				   ap->cvi->imod->objsize - 1);
	       ap->curobj = ap->cvi->imod->obj[0].fgcolor;
	  }

	  /* Get the pixel values for gray scale and set up ramp */
	  ap->cvi->rampsize = 256;
	  ap->cvi->rampbase = 0;
	  ap->cvi->cramp = xcramp_allinit(ap->display, ap->visualinfoGL,
					  ap->cmapGL, 0, 255);
	  /*  imod_info_setbw(ap->cvi->black, ap->cvi->white);  NOT YET */
	  xcramp_setlevels(App->cvi->cramp, App->cvi->black, App->cvi->white);
	  imod_init_cursors(ap);

	  return 0;
     }

     if (ap->depth > 8){
	  unsigned long pixelReturn;
	  ap->objbase    = App->base + 257;
	  XAllocColorCells(ap->display, ap->cmap, False,
			   plane, 0, &pixelReturn, 1);
	  if (App->cmap != App->cmapGL)
	       XAllocColorCells(ap->display, ap->cmapGL, False,
				plane, 0, &pixelReturn, 1);
	  ap->curobj = pixelReturn;
     }

     /* set colors for current view */
     if (ap->cvi->imod){
	  ap->cvi->black = ap->cvi->imod->blacklevel;
	  ap->cvi->white = ap->cvi->imod->whitelevel;
	  imod_cmap(ap->cvi->imod);
     }


     if (ap->depth == 8)
	 ap->cvi->cramp = xcramp_allinit(ap->display, ap->visualinfoGL, 
					 ap->cmapGL, RAMPMIN, RAMPMAX);
     else
	 ap->cvi->cramp = xcramp_allinit(ap->display, 
					 ap->visualinfoGL, 
					 ap->cmapGL, 
					 ap->base, ap->base + 255);

     if (ap->depth == 8){
	  ap->cvi->rampbase = RAMPMIN;
	  ap->cvi->rampsize = RAMPMAX - RAMPMIN;
     }else{
	  ap->cvi->rampsize = 256;
	  ap->cvi->rampbase = ap->base;
     }

     imod_info_setbw(ap->cvi->black, ap->cvi->white);
     xcramp_setlevels(App->cvi->cramp, App->cvi->black, App->cvi->white);

     mapcolor(App->select,     255, 255,   0);
     mapcolor(App->shadow,     128, 128,   0);
     mapcolor(App->endpoint,   255,   0,   0);
     mapcolor(App->bgnpoint,     0, 255,   0);
     mapcolor(App->curpoint,   255,   0,   0);
     mapcolor(App->foreground, 255, 255, 128);
     mapcolor(App->background,  64,  64,  96);
     mapcolor(App->ghost,       16, 16, 16);
     mapcolor(App->imodvbgcolor,  0,  0,  0);

     imod_init_cursors(ap);

     return(0);
}

/* Set the colormap for the given model. */
void imod_cmap(Imod *m)
{
     int i;
     unsigned short red,green,blue;
     if (App->rgba) return;
     for (i = 0; i < m->objsize; i++){
	  red   = m->obj[i].red * 255.0;
	  green = m->obj[i].green * 255.0;
	  blue  = m->obj[i].blue * 255.0;
	  if (App->depth == 8){
	       if ((App->objbase - i) > IMOD_MIN_INDEX)
		    mapcolor(App->objbase - i, red, green, blue);
	       m->obj[i].fgcolor = App->objbase - i;
	  }
	  else{
	       mapcolor(i + App->objbase, red, green, blue);
	       m->obj[i].fgcolor = App->objbase + i;
	  }
     }
     mapcolor(App->select,     255, 255,   0);
     mapcolor(App->shadow,     128, 128,   0);
     mapcolor(App->endpoint,   255,   0,   0);
     mapcolor(App->bgnpoint,     0, 255,   0);
     mapcolor(App->curpoint,   255,   0,   0);
     mapcolor(App->foreground, 255, 255, 128);
     mapcolor(App->background,  64,  64,  96);
     return;
}


static int rethink(ImodView *vw)
{
     Iobj   *obj;
     Icont  *cont;
     Ipoint *point;
     int     index;
     
     if ( (index = Model->cindex.point) < 0){
	  return(IMOD_DRAW_MOD);
     }

     cont = imodContourGet(Model);
     if (cont == NULL){
	  return(IMOD_DRAW_MOD);
     }
     if ((cont->pts == NULL) || (cont->psize <= index)){
	  return(IMOD_DRAW_MOD);
     }

     obj = imodObjectGet(Model);
     if (iobjFlagTime(obj))
	  ivwSetTime(App->cvi, cont->type);

     point = &(cont->pts[index]);
     vw->xmouse = point->x;
     vw->ymouse = point->y;
     vw->zmouse = point->z;
     ivwBindMouse(vw);
     return(IMOD_DRAW_MOD|IMOD_DRAW_XYZ);
}

/* draw all windows in the display */
int imodDraw(ImodView *vw, int flag)
{
     /* todo: set up callback functions.
      * IMOD_DRAW_IMAGE: image data has changed draw all images, 
      *                  clear caches
      * IMOD_DRAW_XYZ:   x,y,z position changed.
      * IMOD_DRAW_MOD:   model has changed.
      * IMOD_DRAW_SLICE: slice has changed.
      */

     if (flag & IMOD_DRAW_RETHINK){
	  flag |= rethink(vw);
     }

/*
     if (flag & IMOD_DRAW_XYZ){
	  imod_info_setxyz();
     }
*/

     if (flag & IMOD_DRAW_MOD){
	  imod_info_setocp();
     }

     xyzDraw_cb(vw, vw->xyz, flag);

#ifdef USE_IMOD_CONTROL
     ivwControlListDraw(vw, flag);
#else
     zapDraw_cb(vw, vw->zap, flag);
#endif


     if (flag & IMOD_DRAW_XYZ){
	  imod_info_setxyz();
     }

     if (flag & IMOD_DRAW_MOD){
	  imodv_draw();
     }



     return(0);
}

static int imod_init_cursors(ImodApp *ap)
{
     Pixmap shape, mask;
     XColor fgc, bgc;
     
     shape = XCreatePixmapFromBitmapData
	  (ap->display, XtWindow(ap->toplevel),
	   (char *)cursor_bits, cursor_width, cursor_height, 1, 0, 1);
     mask = XCreatePixmapFromBitmapData
	  (App->display, XtWindow(ap->toplevel),
	   (char *)cursor_mask_bits, cursor_width, cursor_height, 1, 0, 1);
     XParseColor(ap->display, ap->cmap, "red", &fgc);
     XParseColor(ap->display, ap->cmap, "white", &bgc);
     ap->cursor_cross = XCreatePixmapCursor
	  (ap->display, shape, mask,
	   &fgc, &bgc,
	   cursor_x_hot, cursor_y_hot);
     return(0);
}

void stereoHardware(Widget w, int flag)
{
#ifdef __sgi
     static int hw = 0;
     Position sx = 10, sy = 30;
     Dimension sw = 1280, sh = 1024, sb = 0;

     char stcmd[] = "/usr/gfx/setmon -n STR_RECT";
     char mocmd[] = "/usr/gfx/setmon -n 72HZ";

     if (flag){
	  if (!hw){
	       diaBusyCursor(1);
	       system(stcmd);
	       diaBusyCursor(0);
	  }
	  XtConfigureWidget(w, sx, sy, sw, sh, sb);
	  hw = 1;
	  
     }else{
	  if (hw){
	       diaBusyCursor(1);
	       system(mocmd);
	       diaBusyCursor(0);
	  }
	  hw = 0;
     }
#endif
}


Colormap imod_alloc_colormap(Display *display)
{
     XColor color;
     Colormap cmap;
     Screen *screen = DefaultScreenOfDisplay(display);
     Window window  = RootWindowOfScreen(screen);
     int screen_num = DefaultScreen(display);
     int maxindex   = IMOD_MAX_INDEX + 2;
     int dindex     = 1 << (DefaultDepth(display, screen_num));
     int depth      = App->depth;
     unsigned long *pixels;
     unsigned long plane[1];
     int i;

     /* DNM: TrueColor needs to create a colormap for this visual since
	it may not be the default.  But then exit. */
     cmap = XCreateColormap(display, window, App->visualGL, AllocNone);
     if (Imod_debug)
	  printf("default and created cmap %d %d\n", App->cmap, cmap);
     if (cmap == 0){
	  fprintf(stderr, "IMOD ERROR: Couldn't get ColorMap.\n");
          exit(-1);
     }
     if (App->rgba)
	  return(cmap);

     if (depth > 10){
	  dindex = 256;
	  maxindex = (1 << depth) - 128;
     }

     pixels = (unsigned long *) malloc
	  (sizeof(unsigned long) * maxindex);

     if (!XAllocColorCells(display, cmap, True,
			   plane, 0, pixels, maxindex)){
	  fprintf(stderr, "IMOD Warning: Couldn't get colors\n");
     }
     
     if (maxindex > dindex)
	  maxindex = dindex;
     
     for (i = 0; i < maxindex; i++){
	  color.pixel = i;
	  color.flags = DoRed|DoGreen|DoBlue;
	  XQueryColor (display, DefaultColormap(display,screen_num), &color);
	  XStoreColors(display, cmap, &color, 1);
     }
     free(pixels);
     return(cmap);
}

int imod_get_colormap(ImodApp *ap)
{
     XStandardColormap *scmap, dscmap;
     int screen_num = DefaultScreen(ap->display);
     Atom acmap, atr;
     int count;
     char *dstr;
     Display *display;

     int afr;
     unsigned long bar;
     unsigned char *prop;
     int cpid;
     
     if ( (!ap->rgba) && (ap->depth > 10)){

	  /* Initialize shared colormap */

	  acmap = XInternAtom(ap->display, "bl3dfs_Acmap", 0);
	  if (XGetRGBColormaps
	      (ap->display, RootWindow(ap->display, screen_num),
	       &scmap, &count, acmap) == 0){

	       dstr = DisplayString(ap->display);
	       display = XOpenDisplay(dstr);
	       if (!display){
		    fprintf(stderr, "IMOD ERROR: Display open failed.\n");
		    exit(-1);
	       }
	       ap->cmapGL = imod_alloc_colormap(display);
	       dscmap.colormap = ap->cmapGL;
	       dscmap.killid = ap->cmapGL;
	       XSetRGBColormaps
		    (display, RootWindow(display, screen_num), 
		     &dscmap, 1, acmap);
	       XSetCloseDownMode(display, RetainPermanent);
	       XCloseDisplay(display);
	  }else{
	       ap->cmapGL = scmap->colormap;
	  }

     }else{
	  ap->cmapGL = imod_alloc_colormap(ap->display);
     }

     /*
     XtVaSetValues(ap->toplevel, 
		   XmNdepth, ap->depth,
		   XmNvisual, ap->visual,
		   XmNcolormap, ap->cmap, NULL);
     */
     return(0);
}


#if XmVERSION == 1
#if XmREVISION == 1
#define NOOVERRIDEKEYTRANS
#endif
#endif

       /* Irix4 or
	* IrisGL crashes on this for some reason.
	* We aren't really supports IrisGL fully to just forget translations.
	*/

void imodOverrideTransTable(Widget w, String table)
{
#ifndef NOOVERRIDEKEYTRANS
    XtOverrideTranslations(w,XtParseTranslationTable(table));
#endif
    return;
}
			 

void imodOverrideTranslations(Widget w, XtTranslations translations)
{
#ifndef NOOVERRIDEKEYTRANS
    XtOverrideTranslations(w, translations);
#endif
    return;
}

/* setup pixels from  ivwGetZSectionTime to blast to
 * the screen with glDrawPixels.
 * DNM: this is not consistent with b3dgfx entry but is called by matchPoint
 */     
int ivwGetImageType(ImodView *view, GLenum *otype, GLenum *oformat)
{
     GLint unpack = 1;
     GLenum format = GL_COLOR_INDEX;
     GLenum type   = GL_UNSIGNED_BYTE;

     if (App->rgba){

	 if (App->rgba > 1){
#ifdef __sgi
	     format   = GL_ABGR_EXT;
#else
	     format   = GL_RGBA;
#endif
	     unpack = 4;
	 }else{
	     format = GL_LUMINANCE;
	     /* todo: setup lookup table. */
	 }

     }else{
	 if (App->depth > 8){ 
	      glPixelTransferi(GL_INDEX_OFFSET, imodColorValue(COLOR_MIN));
	      /*unpack = 2; 
	      type = GL_UNSIGNED_SHORT; */
	 }else{
	      /* use default values. */
	      glPixelTransferi(GL_INDEX_OFFSET, 0);
	 }
   }
     if (otype)
	  *otype   = type;
     if (oformat)
	  *oformat = format;
     
     glPixelStorei(GL_UNPACK_ALIGNMENT, unpack);
     return unpack;
}
