/*  IMOD VERSION 2.50
 *
 *  ximodv.c -- Init and open the model view window in imod.
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

#define USEXIMODV

#ifndef USEXIMODV
#include <imodel.h>

void imodv_open(struct Mod_Model *imod, int cmapbase)
{
     wprint("Model view not yet available for X11 version.\n");
     return;
}

int  imodv_anim(void)
{
     return(0);
}

void imodv_draw()
{
     return;
}

#else

#include <Xm/Xm.h>
#include <X11/keysym.h>
#include <Xm/VirtKeys.h>
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <math.h>

#include "ximodv.h"
#include "imod.h"

ImodvApp  ImodvStruct;
/*ImodvApp *Imodv; */
struct Mod_Draw Imodv_mdraw;

void imodvOpen(ImodView *vw);
#ifdef __sgi
char *ImodRes_SGIStereoCommand(void);
char *ImodRes_SGIRestoreCommand(void);
#endif

static int open_display(int *argc, char **argv, ImodvApp *a);
static int open_window(ImodvApp *a);

static int imodv_nob = 0;
static int imodv_fs  = 0;
/* DNM: make this a static for the whole module, so that it can be used to
   test whether window is open before it's been opened, rather than trusting
   that ImodvStruct.toplevel is initialized at zero */
static int first_open = 1;

void imodv_open_nob(Imod *imod)
{
     imodv_nob = 1;
     Imodv = &ImodvStruct;
     ImodvStruct.md = &Imodv_mdraw;
     imodvOpen(App->cvi);
     imodv_nob = 0;
     return;
     
}

void imodv_open(struct Mod_Model *imod, int cmapbase)
{
     Imodv = &ImodvStruct;
     ImodvStruct.md = &Imodv_mdraw;
     imodvOpen(App->cvi);
     return;
}

static void initstruct(ImodView *vw, ImodvApp *a)
{
     Screen *screen;
     struct Mod_Draw *md;
     a->nm = 1;
     a->cm = 0;
     a->mod = (Imod **)malloc(sizeof(Imod *));
     a->mod[0] = vw->imod;
     a->imod = vw->imod;
     a->mat  = imodMatNew(3);
     a->rmat  = imodMatNew(3);
     
     a->dobj = imodObjectNew();
     /* DNM 8/3/01: start with current object if defined */
     if (a->imod->cindex.object >= 0 && 
	 a->imod->cindex.object < a->imod->objsize) {
	  a->ob = a->imod->cindex.object;
	  a->obj = &(a->imod->obj[a->ob]);
     } else {
	  a->obj = a->dobj;
	  a->ob = 0;
     }
     md = a->md = &Imodv_mdraw;
     a->cnear = 0;
     a->cfar = 1000;
     a->fovy = 0;
     a->gc = a->dgc = 0;
     
     a->dlist = 0;
     a->update_dlist = 1;
     a->movie = 0;
     a->movieFrames = 0;
     a->wpid = (XtWorkProcId)0;
     a->stereo = 0;
     a->plax = 5.0f;
     a->lightx = a->lighty = 0;
     a->wxt = a->wyt = a->wzt = 0;

     a->rbgcolor.red = a->rbgcolor.green = a->rbgcolor.blue = 255;
     md->xorg =  md->yorg = md->zorg = 0;
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
     a->crosset    = False;
     a->noborder   = imodv_nob;
     a->fullscreen = imodv_fs;
     a->drawall    = False;
     a->moveall    = True;
     a->current_subset = 0;

     a->wxt = vw->xsize/2;
     a->wyt = vw->ysize/2;
     a->wzt = vw->zsize/2;
     a->standalone = False;
     a->texMap  = 0;
     a->texTrans = 0;
     a->vi = vw;
     a->doPick = 0;
     a->wPick = a->hPick = 5;

     if (App->depth > 8)
	  a->cindex = False;
     else
	  a->cindex = True;
     a->alpha = 0;

     /* DNM: surely it should be x*x+y*y+z*z, not x*x+y*y+y*y */
     a->r = (a->wxt*a->wxt) + (a->wyt*a->wyt) + (a->wzt*a->wzt);
     a->r = (float)sqrt((double)a->r);
     a->bindex = App->imodvbgcolor;
     a->context = App->context;
     a->display = App->display;
     a->visual = App->visual;
     screen = DefaultScreenOfDisplay(a->display);
     /* If we've changed the main visual, we need to set this depth to be
	the App->depth to match, but otherwise we need to take the default
	depth because App->depth applies to GL windows */
     if (a->visual == DefaultVisualOfScreen(screen))
	  a->depth = DefaultDepthOfScreen(screen);
     else
	  a->depth = App->depth;
     a->gcmap = a->cmap = App->cmap;
     a->db = True;
     a->visualInfo = App->visualinfo;
     a->cstart = App->objbase;
     a->cstep = -1;

     a->lighting  = True;
     a->depthcue  = False;
     a->wireframe = False;
     a->lowres = 0;


     a->SGIStereoCommand  = (_XtString)ImodRes_SGIStereoCommand();
     a->SGIRestoreCommand = (_XtString)ImodRes_SGIRestoreCommand();

     imodViewStore(a->imod, 0);
     if (!a->imod->cview)
	  imodViewModelDefault(a->imod, a->imod->view);
     else 
         imodViewUse(a->imod);

     return;
}

/* DNM: remove the code from here which was quit incomplete, and rely on the
   quit callback in imodv_input */
void ximodv_quit_cb(Widget w, XtPointer client, XtPointer call);

void imodvOpen(ImodView *vw)
{
     Dimension width, height;
     Atom wmclose;
     char *window_name;
     ImodvApp *a = Imodv;
     int ob, co, pt;
     Imod *imod = vw->imod;
     int hasPoints = 0;

     /* mt model ? */
     if (!imod){
	   wprint("Model View didn't open because "
		  "there is no model loaded.\n");
	  return;
     }
     for(ob = 0; ob < imod->objsize; ob++){
	  if (hasPoints > 1) break;
	  for(co = 0; co < imod->obj[ob].contsize; co++){
	       hasPoints += imod->obj[ob].cont[co].psize;
	       if (hasPoints > 1) break;
	  }
     }
     if (hasPoints < 2){
	  wprint("Model View didn't open because model has no points.\n");
	  return;
     }

     /* check for already open? */
     if (first_open){
	  first_open = 0;
	  Imodv->topLevel = 0;
     }
     if (Imodv->topLevel){
	  wprint("Error:\nModel View already open.\n");
	  return;
     }
     initstruct(vw, a);
     window_name = imodwEithername("IMOD Model View: ", Imod_filename);

     Imodv->topLevel = XtVaCreatePopupShell
	  ("Model View", topLevelShellWidgetClass, App->toplevel,
	   /*	   XmNvisual, App->visual,
	   XmNcolormap, App->cmap,
	   XmNdepth, App->depth, */
	   XtNtitle, window_name,
	   NULL);

     if (window_name)
	  free(window_name);
     
     if (!Imodv->topLevel)
	  return;

     a->mainWin = XtVaCreateWidget
	  ("imodv", xmMainWindowWidgetClass, a->topLevel,
	   NULL);


     /* DNM: let's have bigger windows */
     width = height = 512;
     a->form = XtVaCreateWidget
	  ("form", xmFormWidgetClass, a->mainWin,
	   XmNwidth, width,
	   XmNheight, height,
	   NULL);

     imodv_init_drawing_widget(a, a->form);

#ifdef MWSETWORK
          XtVaSetValues(a->mainWin,XmNworkWindow,a->form,NULL);
#endif

     XtManageChild(a->form);

     if (imodv_nob){
	  XmMainWindowSetAreas (a->mainWin, NULL, NULL, NULL, NULL, a->form);
     }else{
	  a->menubar = imodv_create_menu(a);
	  XtManageChild(a->menubar);
	  XmMainWindowSetAreas (a->mainWin, a->menubar, NULL,
				NULL, NULL, a->form);
     }
     XtManageChild(a->mainWin);

     wmclose = XmInternAtom( XtDisplay(a->topLevel),
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(a->topLevel, wmclose, ximodv_quit_cb,
			     (caddr_t)a);

     XtPopup(a->topLevel, XtGrabNone);
     ImodvClosed = False;
     /* DNM: new approach to movie workproc, skip time outs */
     /* imodv_movie(a); */
     return;
}


int  imodv_anim(void)
{
     return(0);
}

void imodv_draw()
{
     if (!first_open && ImodvStruct.topLevel)
	  imodvDraw(Imodv);
     return;
}

/* DNM: a routine for imod to notify imodv of a change in model */
void imodv_new_model(Imod *mod)
{
     if (first_open || !ImodvStruct.topLevel)
	  return;
     Imodv->imod = mod;
     Imodv->mod[0] = mod;

     /* Set up the views and scaling, notify everybody of changes */
     imodViewStore(mod, 0);
     
     if (!mod->cview){
	 imodViewModelDefault(mod, mod->view);
     }else
         imodViewUse(mod);
     imodvSelectModel(Imodv, 0);
}

#endif
