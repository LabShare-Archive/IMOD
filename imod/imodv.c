/*  IMOD VERSION 2.50
 *
 *  imodv.c -- The main imodv stand alone program.
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
    Revision 3.3  2002/11/27 03:29:45  mast
    Made it look for both single and double buffer visuals as long as they
    are both RGB or both color index.  Added a true 15 visual, better than
    pseudo 12 (present on O2).

    Revision 3.2  2002/09/04 00:24:48  mast
    Added CVS header.  Changed to getting visuals then passing them to GLw.

*/

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/BulletinB.h>
#include <Xm/MwmUtil.h>
#include <Xm/MainW.h>

#include "imodv.h"
#include "imod.h"

ImodvApp *Imodv;
extern int Imod_debug;

#ifdef DRAW_X11
#define IMODV_VERSION_STRING "1.20x"
#else
#define IMODV_VERSION_STRING "1.20"
#endif

#include <GL/glx.h>

/* the default graphics rendering attributes for OpenGL */

/* These visuals are for displays that only have color index modes */
/* Decided to leave depth out of the requests */
static int Pseudo12[] = 
{
     GLX_DOUBLEBUFFER,
     GLX_BUFFER_SIZE, 12,
     GLX_DEPTH_SIZE, 8,
     None
};

static int Pseudo8[] = 
{
     GLX_DOUBLEBUFFER,
     GLX_BUFFER_SIZE, 8,
     GLX_DEPTH_SIZE, 8,
     None
};

/*
 * These visuals are for displays with TrueColor
 * force it to be TrueColor on SGI, since the code doesn't handle 
 * DirectColor ? - by analogy withy imod visuals;
 * put in some minimal
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
    GLX_DEPTH_SIZE, 8,
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
    GLX_DEPTH_SIZE, 8,
    None
};
static int True15[] =
{
    GLX_DOUBLEBUFFER, 
#ifdef __sgi
    GLX_X_VISUAL_TYPE_EXT, GLX_TRUE_COLOR_EXT, 
#endif
    GLX_RGBA,
    GLX_RED_SIZE, 5,
    GLX_GREEN_SIZE, 5,
    GLX_BLUE_SIZE, 5,
    GLX_DEPTH_SIZE, 8,
    None
};

/* Some visuals with no depth for the truly desperate */
static int True24nodep[] =
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
static int True15nodep[] =
{
    GLX_DOUBLEBUFFER, 
#ifdef __sgi
    GLX_X_VISUAL_TYPE_EXT, GLX_TRUE_COLOR_EXT, 
#endif
    GLX_RGBA,
    GLX_RED_SIZE, 5,
    GLX_GREEN_SIZE, 5,
    GLX_BLUE_SIZE, 5,
    None
};
static int True12nodep[] =
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
static int Pseudo12nodep[] = 
{
     GLX_DOUBLEBUFFER,
     GLX_BUFFER_SIZE, 12,
     None
};

static int Pseudo8nodep[] = 
{
     GLX_DOUBLEBUFFER,
     GLX_BUFFER_SIZE, 8,
     None
};

/* List the attribute lists in order of priority, indicate if they are
   truecolor (rgb) type */
static int *OpenGLAttribList[] = {
     True24, True24 + 1, True15, True15 + 1, Pseudo12, Pseudo12 + 1,
     True12, True12 + 1, Pseudo8, Pseudo8 + 1,
     True24nodep, True24nodep + 1, True15nodep, True15nodep + 1,
     Pseudo12nodep, Pseudo12nodep + 1,
     True12nodep, True12nodep + 1, Pseudo8nodep, Pseudo8nodep + 1,
     NULL
};
static int AttribRGB[] = {1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 
			  1, 1, 0, 0};
static int AttribDepth[] = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
			    0, 0, 0, 0};

/*void __eprintf(void){return;} */

static void usage(char *pname);
static int load_models(int n, char **fname, ImodvApp *a);
static int setmodelview(ImodvApp *a);

static void usage(char *pname)
{
     imodVersion(pname);
     imodCopyright();
     fprintf(stderr, "options: all Xt Toolkit options plus:\n");
     fprintf(stderr, "\t-noborder         Open window with no border.\n");
     fprintf(stderr, "\t-fullscreen       Open window to max size.\n");
     fprintf(stderr, "\t-f                Open window to max size.\n");
     fprintf(stderr, "\t-renderbackground Background color for rendering.\n");
     fprintf(stderr, "\t-rbg              Background color for rendering.\n");
     fprintf(stderr, "\t-cindex           Use color index mode.\n");
     exit(-1);
}


static int load_models(int n, char **fname, ImodvApp *a)
{
     int i, ob, co;

     if (n < 1)
	  return(0);
     a->mod = (Imod **)malloc(sizeof(Imod *) * n);
     a->nm = n;
     a->cm = 0;
     for(i = 0; i < n; i++){
	  a->mod[i] = imodRead(fname[i]);
	  if (!a->mod[i]){
	       fprintf(stderr, "Error loading %s\n", fname[i]);
	       return(-1);
	  }

	  /* DNM 6/20/01: find out max time and set current time */
	  a->mod[i]->tmax = 0;
	  for (ob = 0; ob < a->mod[i]->objsize; ob++)
	       for (co = 0; co < a->mod[i]->obj[ob].contsize; co++)
		    if (a->mod[i]->tmax < a->mod[i]->obj[ob].cont[co].type)
			 a->mod[i]->tmax = a->mod[i]->obj[ob].cont[co].type;
	  a->mod[i]->ctime = a->mod[i]->tmax ? 1 : 0;

	  /* DNM: changes for storage of object properties in view and 
	     relying on default scaling.  Also, make sure every model has
	     the view to use set up */

	  imodViewStore(a->mod[i], 0);
	  if (!a->mod[i]->cview){
	       imodViewModelDefault(a->mod[i], a->mod[i]->view);
	  } else
	       imodViewUse(a->mod[i]);
     }
     a->imod = (a->mod[a->cm]);
     /* DNM 8/3/01: start with current object if defined */
     if (a->imod->cindex.object >= 0 && 
	 a->imod->cindex.object < a->imod->objsize) {
	  a->ob = a->imod->cindex.object;
	  a->obj = &(a->imod->obj[a->ob]);
     }
   
     /* DNM 9/2/02: this did nothing */
     /* setmodelview(a); */
     return(0);
}

/* DNM 9/2/02: eliminate unused setmodelview */

/* Initialize the structure: 
   9/2/02: also called for model view initialization in imod */
int imodv_init(ImodvApp *a, struct Mod_Draw *md)
{
     a->nm = 0;
     a->cm = 0;
     a->mod = NULL;
     a->imod = NULL;
     a->mat  = imodMatNew(3);
     a->rmat  = imodMatNew(3);
     a->dobj = imodObjectNew();
     a->obj = a->dobj;
     a->ob = 0;
     a->md = md;
     a->cnear = 0;
     a->cfar = 1000;
     a->fovy = 0;
     a->gc = a->dgc = 0;
     a->dlist = 0;
     a->update_dlist = 1;
     a->movie = 0;
     a->movieFrames = 0;
     a->wpid = (XtWorkProcId)0;
     a->stereo = IMODV_STEREO_OFF;
     a->plax = 5.0f;
     a->lightx = a->lighty = 0;
     a->wxt = a->wyt = a->wzt = 0;

     a->rbgcolor.red = a->rbgcolor.green = a->rbgcolor.blue = 0;
     md->xorg = md->yorg = md->zorg = 0;
     md->xrot = md->yrot = md->zrot = 0;  /* current rotation. */
     md->zoom = 1.0f;
     md->arot = 10;
     md->atrans = 5.0f;
     md->azoom = 1.05f;
     md->azscale = 0.2;
     md->xrotm = 0; /* rotation movie */
     md->yrotm = 0;
     md->zrotm = 0;

     /* control flags */
     a->fastdraw   = 0;
     a->current_subset = 0;
     a->crosset    = False;
     a->noborder   = False;
     a->fullscreen = False;
     a->drawall    = False;
     a->moveall    = True;
     a->standalone = True;
     a->cindex     = False;
     a->cstart     = 65;
     a->cstep      = 2;
     a->bindex     = 64;
     a->alpha      = 0;
     imodViewDefault(&a->view);
     a->view.cnear = 0.0f;
     a->view.cfar  = 1.0f;
     a->doPick = 0;
     a->wPick = a->hPick = 5;

     a->lighting  = True;
     a->depthcue  = False;
     a->wireframe = False;
     a->lowres = 0;
     a->SGIStereoCommand = NULL;
     a->SGIRestoreCommand = NULL;
     return(0);
}


/* DNM: These fallback resources are needed in case there is no app-default
   file */

static String Imodv_fallback_resources[] = {
     "*frame*shadowType: SHADOW_IN",
     "*sgiMode: True",
     "*useSchemes: all",
     "*stereoCommand: /usr/gfx/setmon -n 640x512_120s",
     "*restoreCommand: /usr/gfx/setmon -n 72HZ",
     "*SGIStereoCommand: /usr/gfx/setmon -n STR_TOP",
     "*SGIRestoreCommand: /usr/gfx/setmon -n 72HZ",
     NULL,
};

/* DNM 9/2/02: add -D argument to get debug mode to work */
static XrmOptionDescRec Imodv_options[] = {
     {"-noborder",     "*noborder",     XrmoptionNoArg, "True"},
     {"-fullscreen",    "*fullscreen",   XrmoptionNoArg, "True"},
     {"-f",             "*fullscreen",   XrmoptionNoArg, "True"},
     {"-fs",            "*fullscreen",   XrmoptionNoArg, "True"},
     {"-renderbackground", "*renderbackground", XrmoptionSepArg, "black"}, 
     {"-rbg",       "*renderbackground", XrmoptionSepArg, "black"},
     {"-cindex",    "*colorindex",       XrmoptionNoArg, "True"},
     {"-D",    "*debug",       XrmoptionNoArg, "True"},
};

/* DNM: There are default settings for the the stereo commands in case there
   is an app-default file that doesn't define these resources, since the
   fallback resources don't cover that case */

static XtResource Imodv_resources[] = {
     {"noborder", "NoBorder", XmRBoolean, sizeof(Boolean),
	   XtOffsetOf(ImodvApp, noborder), XmRImmediate, (XtPointer)False},
     {"fullscreen", "FullScreen", XmRBoolean, sizeof(Boolean),
           XtOffsetOf(ImodvApp, fullscreen), XmRImmediate, (XtPointer)False},
     {"renderbackground", "RenderBackground", XmRString, sizeof(XColor),
	   XtOffsetOf(ImodvApp, rbgname), XmRImmediate, "black"}, 
     {"colorindex", "ColorIndex", XmRBoolean, sizeof(Boolean),
	   XtOffsetOf(ImodvApp, cindex), XmRImmediate, (XtPointer)False},
     { "stereoCommand",
       XtCString, XtRString, sizeof (_XtString),
       XtOffsetOf(ImodvApp, stereoCommand),
       XtRImmediate, (XtPointer) NULL },
     { "restoreCommand",
       XtCString, XtRString, sizeof (_XtString),
       XtOffsetOf(ImodvApp, restoreCommand),
       XtRImmediate, (XtPointer) NULL },
     { "SGIStereoCommand",
       XtCString, XtRString, sizeof (_XtString),
       XtOffsetOf(ImodvApp, SGIStereoCommand),
       XtRString, "/usr/gfx/setmon -n STR_TOP" },
     { "SGIRestoreCommand",
       XtCString, XtRString, sizeof (_XtString),
       XtOffsetOf(ImodvApp, SGIRestoreCommand),
       XtRString, "/usr/gfx/setmon -n 72HZ" }
     
};


int myDebugError(Display *inDisplay, XErrorEvent *inError)
{
    fprintf(stderr, "imodv: xerror\n");
    abort();
    return 0;
}

static int open_display(int *argc, char **argv, ImodvApp *a)
{
     int err;
     Colormap cmap;

     a->topLevel = XtVaAppInitialize
	  (&(a->context), "Imodv",
	   Imodv_options, XtNumber(Imodv_options),
	   argc, argv,
	   Imodv_fallback_resources,
	   NULL);
     if (!a->topLevel){
	  fprintf(stderr, "%s: open display failed.\n",
		  argv[0]);
	  exit(-1);
     }
     a->display = XtDisplay(a->topLevel);

/*
 *   XSetErrorHandler(myDebugError);
 */

     XtVaGetApplicationResources
	  (a->topLevel, (XtPointer)a,
	   Imodv_resources, XtNumber(Imodv_resources),
	   NULL);

     XtVaGetValues(a->topLevel, XmNcolormap, &cmap, NULL);
     a->hidemenu  = False;
     a->db        = True;
     a->dgfx_init = False;
     a->gfx_init  = False;
     if (a->noborder || a->fullscreen){
	  XtVaSetValues(a->topLevel, XmNmwmDecorations, 0, NULL);
	  a->hidemenu = True;
     }
     XParseColor(a->display, cmap, a->rbgname, &(a->rbgcolor));
     a->rbgcolor.red &= 0xff00;
     a->rbgcolor.green &= 0xff00;
     a->rbgcolor.blue &= 0xff00;


     /* DNM 9/2/02: first get these values from the topLevel, then get the
	visuals, which may modify them for color index mode */
     XtVaGetValues(a->topLevel,
		   XmNdepth, &a->depth,
		   XmNvisual, &a->visual,
		   XmNcolormap, &a->cmap,
		   NULL);

     if ((err = imodvGetVisuals(a)) != 0) {
	  if (err > 0)
	       fprintf(stderr, "imodv error: Couldn't get rendering visual.\n");
	  else
	       fprintf(stderr, "imodv error: Couldn't get color map.\n");
	  exit(-1);
     }

     a->gcmap = a->cmap;  /* ?? */
     return(0);
}

static int open_window(ImodvApp *a)
{
     Dimension width, height;
     Widget form,frame;

     a->mainWin = XtVaCreateWidget
	  ("imodv", xmMainWindowWidgetClass, a->topLevel,
	   NULL);

     a->form = XtVaCreateWidget
	  ("form", xmFormWidgetClass, a->mainWin,
	   NULL);

     if (a->fullscreen){
	  width = WidthOfScreen(XtScreen(a->topLevel));
	  height = HeightOfScreen(XtScreen(a->topLevel));
	  XtVaSetValues(a->topLevel, XmNwidth, width,
			XmNheight, height, 
			XmNx, 0, XmNy, 0,
			XtVaTypedArg, XmNgeometry, XmRString, "+0+0", 5,
			NULL);
     }else{
          /* DNM: let's have bigger windows */
	  width = height = 512;
	  if (width > WidthOfScreen(XtScreen(a->topLevel)))
	       width = WidthOfScreen(XtScreen(a->topLevel));
	  if (height > HeightOfScreen(XtScreen(a->topLevel)))
	       height = HeightOfScreen(XtScreen(a->topLevel));
	  XtVaSetValues(a->form, XmNwidth, width,
			XmNheight, height, NULL);
     }

     if (imodv_init_drawing_widget(a, a->form)){
	  fprintf(stderr, "Error opening graphics.\n");
	  exit(-1);
     }

     XtManageChild(a->form);

#ifdef MWSETWORK
     XtVaSetValues(a->mainWin,XmNworkWindow,a->form,NULL);
#endif

     if (a->hidemenu){
	  a->popup   = imodv_create_popup(a);
	  XmMainWindowSetAreas (a->mainWin, NULL, NULL, NULL, NULL, a->form);
	  XtSetMappedWhenManaged(a->popup, True); 
     }else{
	  a->menubar = imodv_create_menu(a);
	  XtManageChild(a->menubar);
	  XmMainWindowSetAreas (a->mainWin, a->menubar, NULL, 
				NULL, NULL, a->form);
     }
     XtManageChild(a->mainWin);
     XtRealizeWidget(a->topLevel);
     ImodvClosed = False;
     return(0);
}

/* DMN 9/2/02: replaced old imodvSetCmap with this, which is accessed from
   both imodv and ximodv (model view startup in imod), and which gets the
   best visuals for double and single buffering then does the old work of
   imodvSetCmap to get color map if needed in color index mode */

int imodvGetVisuals(ImodvApp *a)
{
  XVisualInfo vistemp;
  XVisualInfo *vlist;
  Window   window;
  Screen  *screen;
  XColor color;
  int depth, screen_num = 0;
  int i, vsize, v = 0;
  unsigned long plane[1];
  unsigned long pixels[IMODV_MAX_INDEX];
  int iGot1 = -1;
  int iGot2 = -1;

  a->display = XtDisplay(a->topLevel);
  screen  = DefaultScreenOfDisplay(a->display);
  screen_num  = DefaultScreen(a->display);
  window  = RootWindowOfScreen(screen);

  a->visualInfoSB = NULL;
  a->visualInfoDB = NULL;
  for (i = 0; OpenGLAttribList[i] != NULL; i += 2) {

    /* If want a color index visual and this is not one, skip */
    if (a->cindex && AttribRGB[i])
      continue;

    /* Insist on a RGB visual in model view from imod because of bugs */
    if (!a->standalone && !AttribRGB[i])
      continue;

    /* If got one already, make sure RGB character matches */
    if (iGot1 >= 0 && AttribRGB[i] != AttribRGB[iGot1])
      continue;

    if (!a->visualInfoDB)
      a->visualInfoDB = glXChooseVisual(a->display, screen_num,
					OpenGLAttribList[i]);

    if (!a->visualInfoSB)
      a->visualInfoSB = glXChooseVisual(a->display, screen_num,
				      OpenGLAttribList[i + 1]);

    if (iGot1 < 0 && (a->visualInfoDB || a->visualInfoSB))
      iGot1 = i;

    /* If got both, stop the loop */
    if (a->visualInfoDB && a->visualInfoSB) {
      iGot2 = i;
      break;
    }
  }

  /* error if no visuals */
  if (!a->visualInfoDB && !a->visualInfoSB)
    return 1;

  if (!AttribRGB[iGot1])
    a->cindex = 1;
  if (!AttribDepth[iGot1] || (iGot2 >= 0 && !AttribDepth[iGot2]))
      fprintf(stderr, "Imodv warning: using a visual with"
	      " no depth buffer\n");

  if (Imod_debug) {
    if (a->visualInfoDB)
      printf("DB class %d, depth %d, ID 0x%x\n", a->visualInfoDB->class, 
	     a->visualInfoDB->depth, a->visualInfoDB->visualid);
    if (a->visualInfoSB)
      printf("SB class %d, depth %d, ID 0x%x\n", a->visualInfoSB->class, 
	     a->visualInfoSB->depth, a->visualInfoSB->visualid);
  }

  /* done if RGB */
  if (!a->cindex)
    return 0;

  fprintf(stderr, "Imodv warning: using color index visual\n");

  /* If color index and no DB, set SB into DB */
  if (!a->visualInfoDB) {
    a->visualInfoDB = a->visualInfoSB;
    a->db = False;
  }

  /* get properties from this visualInfo */
  a->visual = a->visualInfoDB->visual;
  a->depth  = a->visualInfoDB->depth;

  /* If not standalone, and the visual matches the App visual, then
     we use the same colormap, otherwise need a new colormap */
  if (a->standalone || a->visual != App->visual) {
    a->gcmap = a->cmap = XCreateColormap
      (a->display, window,  a->visual, AllocNone);
    if (!a->cmap)
      return -1;
  }

  /* set toplevel properties */
  XtVaSetValues(a->topLevel,
		XmNdepth, a->depth,
		XmNvisual, a->visual,
		XmNcolormap, a->cmap,
		NULL);

  /* Get the colors */
  if (a->standalone) { /* || a->visual != App->visual) {*/
    if (!XAllocColorCells(a->display, a->cmap, True,
			  plane, 0, pixels, IMODV_MAX_INDEX)){
      fprintf(stderr, "Imodv Warning: Couldn't get colors\n");
    }
    for (i = 0; i < IMODV_MAX_INDEX; i++){
      color.pixel = i;
      color.flags = DoRed|DoGreen|DoBlue;
      XQueryColor (a->display,
		   DefaultColormap(a->display,screen_num),
		   &color);
      XStoreColors(a->display, a->cmap, &color, 1); 
    }
  }
  return 0;
}

int imodv_main(int argc, char **argv)
{
     ImodvApp imodv_application;
     struct Mod_Draw mdraw;

     Imodv = &imodv_application;

     imodv_init(&imodv_application, &mdraw);

     open_display(&argc, argv, &imodv_application);

     if (argc < 2)
	  usage(argv[0]);

#ifndef NO_IMOD_FORK
     /* put imodv in background if not debug. */
     if (!Imod_debug)
	  if (fork() != 0)
	       exit(0);
#endif

     if (load_models(argc - 1, &(argv[1]), &imodv_application))
	  exit(-1);

     open_window(&imodv_application);

     dia_xinit(imodv_application.topLevel, 
	       imodv_application.context, 
	       "imodv");

     /* DNM: new approach to movie workproc, skip time outs */
     /* imodv_movie(Imodv); */
     imodv_application.standalone = True;
     XtAppMainLoop(imodv_application.context);
     exit(0);
     return 0;
}


