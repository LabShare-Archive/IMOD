/*  IMOD VERSION 2.41
 *
 *  xyz.c -- Open the XYZ Window; View the X, Y and Z axis.
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
    Revision 3.1  2001/12/17 18:51:49  mast
    Removed call to autox_build

*/
#include <Xm/VirtKeys.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <math.h>

#include <diaP.h>
#include "imod.h"
#include "keypad.h"

/*************************** internal functions ***************************/
static void xxyz_quit_cb(Widget w, XtPointer client, XtPointer call);
static void xxyz_resize_cb(Widget w, XtPointer client, XtPointer call);
static void xxyz_expose_cb(Widget w, XtPointer client, XtPointer call);
static void xxyz_ginit(Widget w, XtPointer client, XtPointer call);

static void xxyz_overex_cb(Widget w, XtPointer client, XtPointer call);
static void xxyz_input_cb(Widget w, XtPointer client, XtPointer call);
static int xxyz_getxyz(struct xxyzwin *xx, int x, int y, int *mx, int *my, int *mz);
static void xxyzButton1(struct xxyzwin *xx, int x, int y);
static void xxyzButton2(struct xxyzwin *xx, int x, int y);
static void xxyzButton3(struct xxyzwin *xx, int x, int y);
static void xxyzB1Drag(struct xxyzwin *xx, int x, int y);
static void xxyzB2Drag(struct xxyzwin *xx, int x, int y);
static void xxyzB3Drag(struct xxyzwin *xx, int x, int y);
static int xyz_draw_image(struct ViewInfo *vi);

/*static int xxyz_fillborder(struct xxyzwin *xx); */
static void xyzDrawAuto(struct xxyzwin *xx);
static void xyzDrawModel(struct xxyzwin *xx);
static void xyzDrawShowSlice(struct xxyzwin *xx);
static void xxyz_draw(struct xxyzwin *xx);
static void xyzDrawImage(struct xxyzwin *xx);

void zapDrawSymbol(int mx, int my, unsigned char sym, unsigned char size,
		   unsigned char flags);

int xyz_draw(struct ViewInfo *vi) {xxyz_draw(vi->xyz);return(0);}
static int xyzShowSlice = 0;

int xxyz_open(ImodView *vi)
{
     Screen *screen;
     char *window_name;
     char *filename;
     int i,msize;
     float newzoom;
     struct xxyzwin *xx;
     Atom wmclose;
     xx = vi->xyz;

     if (xx){
	  wprint("Error:\n\tXYZ Window already open.\n\n");
	  return(-1);
     }

     xx = (struct xxyzwin *)malloc(sizeof(struct xxyzwin));
     if (!xx)
	  return(-1);

     xx->xydata = xx->xzdata = xx->yzdata = NULL;

     /* DNM 1/19/02: need separate fdata for each side panel */
     xx->fdataxz  = (unsigned char *)malloc(vi->xsize * vi->zsize);
     xx->fdatayz  = (unsigned char *)malloc(vi->ysize * vi->zsize);

     
     window_name = imodwfname("IMOD XYZ: ");
     xx->dialog = XtVaCreatePopupShell
	  ("XYZ", topLevelShellWidgetClass, App->toplevel,
	   XmNvisual, App->visual,
	   XtNtitle, window_name,
	   XmNdeleteResponse, XmUNMAP,
	   NULL);

     if (window_name)
	  free(window_name);

     if ((!xx->dialog)||
	 (!xx->fdataxz) || (!xx->fdatayz)){
	  wprint("Error:\n\tXYZ window can't open due to low memory\n");
	  if(xx->fdataxz)
	       free(xx->fdataxz);
	  if(xx->fdatayz)
	       free(xx->fdatayz);
	  free(xx);
	  return(-1);
     }

     xx->winx = vi->xsize + vi->zsize + (3 * XYZ_BSIZE);
     xx->winy = vi->ysize + vi->zsize + (3 * XYZ_BSIZE);
     xx->vi   = vi;
     xx->exposed = False;

     xx->zoom = 1.0;
     xx->xtrans = 0;
     xx->ytrans = 0;
     xx->hq = 0;

     screen = XDefaultScreenOfDisplay(App->display);
     while (xx->winx > WidthOfScreen(screen) - 20 ||
	    xx->winy > HeightOfScreen(screen) - 40){
	  newzoom = b3dStepPixelZoom(xx->zoom, -1);
	  if (newzoom == xx->zoom)
	       break;
	  xx->zoom = newzoom;
	  xx->winx = (vi->xsize + vi->zsize) * xx->zoom + (3 * XYZ_BSIZE);
	  xx->winy = (vi->ysize + vi->zsize) * xx->zoom + (3 * XYZ_BSIZE);
     }

     xx->lx = xx->ly = xx->lz = -1;
     vi->xyz  = xx;

     xx->glw = XtVaCreateManagedWidget
	  ("xyz_glw", B3dDrawingAreaWidgetClass, xx->dialog,
	   XmNwidth, xx->winx,
	   XmNheight, xx->winy,
	   XmNnavigationType, XmNONE,
	   XmNtraversalOn, True,
	   XmNtranslations, XtParseTranslationTable (B3DGFX_Translations),
	   GLwNvisualInfo, App->visualinfoGL,
	   XmNcolormap,         App->cmapGL,
	   NULL);
     imodOverrideTransTable(xx->glw,B3DGFX_Translations);
     
     XtAddCallback(xx->glw, B3dNexposeCallback, xxyz_expose_cb, (XtPointer)xx);
     XtAddCallback(xx->glw, B3dNresizeCallback, xxyz_resize_cb, (XtPointer)xx);
     XtAddCallback(xx->glw, B3dNinputCallback,  xxyz_input_cb,  (XtPointer)xx);
     

     wmclose = XmInternAtom( XtDisplay(xx->dialog),
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(xx->dialog, wmclose, xxyz_quit_cb,
			     (caddr_t)&(vi->xyz));

     /*     xyzDrawImage(NULL); */

     imod_info_input();
     XtPopup(xx->dialog, XtGrabNone);

     return(0);
}

static void xxyz_quit_cb(Widget w, XtPointer client, XtPointer call)
{
     struct xxyzwin **pxx = (struct xxyzwin **)client;
     struct xxyzwin *xx;
     
     xx = *pxx;

     /* DNM 11/17/01: stop x and y movies when close window */
     imodMovieXYZT(xx->vi, 0, 0, MOVIE_DEFAULT, MOVIE_DEFAULT);

     if(xx->fdataxz)
	  free(xx->fdataxz);
     if(xx->fdatayz)
	  free(xx->fdatayz);

     b3dFreeCIImage(xx->xydata);
     b3dFreeCIImage(xx->xzdata);
     b3dFreeCIImage(xx->yzdata);

     b3dWinset(XtDisplay(w), 0, 0);

     XtPopdown(xx->dialog);
     XtDestroyWidget(xx->dialog);
     free(*pxx);
     *pxx = NULL;
}

/* DNM 1/19/02: Add this function to get the CI Images whenever size has
   changed - now that we can do HQ graphics, they need to be the window size */
static void xxyz_getCIImages(struct xxyzwin *xx)
{
     int xdim, ydim1, ydim2;

     xdim = xx->winx;
     ydim1 = ydim2 = xx->winy;
     xx->xydata = (B3dCIImage *)b3dGetNewCIImageSize
	  (xx->xydata, App->depth, xdim, ydim1);

     xx->xzdata = (B3dCIImage *)b3dGetNewCIImageSize
	  (xx->xzdata, App->depth, xdim, ydim2);

     xx->yzdata = (B3dCIImage *)b3dGetNewCIImageSize
	  (xx->yzdata, App->depth, xdim, ydim1);
     return;
}

static void xxyz_resize_cb(Widget w, XtPointer client, XtPointer call)
{
     struct xxyzwin *xx = (struct xxyzwin *)client;
     long winx, winy;
     Dimension width, height;

     XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);
     xx->winx = width;
     xx->winy = height;
     if (xx->exposed){
	  b3dWinset(XtDisplay(w), w, xx->context);
	  b3dResizeViewport();
	  xxyz_getCIImages(xx);
	  xxyz_draw(xx);
     }
     return;
}

static void xxyz_expose_cb(Widget w, XtPointer client, XtPointer call)
{
     struct xxyzwin *xx = (struct xxyzwin *)client;

     if (!xx->exposed){
	  xx->exposed = True;
	  xxyz_ginit(xx->glw, (XtPointer)xx, call);
     }
     b3dWinset(XtDisplay(w), w, xx->context);
     b3dResizeViewport();
     xxyz_draw(xx);
     return;
}

static void xxyz_ginit(Widget w, XtPointer client, XtPointer call)
{
     struct xxyzwin *xx = (struct xxyzwin *)client;

     xx->context =  b3dGetContext(w);
     b3dWinset(XtDisplay(w), w, xx->context);
     b3dResizeViewport();

     xxyz_getCIImages(xx);
     return;
}

/* DNM 1/20/02: added ability to pan with the first mouse button, using 
   similar code as the zap window */
static void xxyz_input_cb(Widget w, XtPointer client, XtPointer call)
{
     struct xxyzwin *xx = (struct xxyzwin *)client;
     struct ViewInfo *vi = xx->vi;
     B3dDrawingAreaCallbackStruct *cbs = (B3dDrawingAreaCallbackStruct *)call;

     int button1 = False, button2 = False, button3 = False;
     static Time but1downt = 0;
     KeySym keysym;
     XKeyEvent *event = (XKeyEvent *)cbs->event;
     int mx, my, mz;

     switch(cbs->event->type){
	case KeyPress:
	  keysym = XLookupKeysym((XKeyEvent *)cbs->event, 0);
	  switch(keysym){
	       /* DNM 12/13/01: remove autox calls from here; it makes no
		  sense to call them from xyz window */

	     case XK_minus:
	       xx->zoom = b3dStepPixelZoom(xx->zoom, -1);
	       xxyz_draw(xx);
	       break;
	     
	     case XK_equal:
	       xx->zoom = b3dStepPixelZoom(xx->zoom, 1);
	       xxyz_draw(xx);
	       break;

	     case XK_s:
	       if ((event->state & ShiftMask) || (event->state & ControlMask)){
		    b3dWinset(App->display, xx->glw, xx->context);
		    /* xxyz_draw(xx); Seems not to be needed */
		    if (event->state & ShiftMask)
			 b3dAutoSnapshot("xyz", SnapShot_RGB, NULL);
		    else if (event->state & ControlMask)
			 b3dAutoSnapshot("xyz", SnapShot_TIF, NULL);
	       }
	       break;

	     case XK_r:
	       xx->hq = 1 - xx->hq;
	       if (xx->hq)
		    wprint("\aHigh-resolution mode ON\n");
	       else
		    wprint("\aHigh-resolution mode OFF\n");
	       xxyz_draw(xx);
	       break;

	     default:
	       inputDefaultKeys((XKeyEvent *)cbs->event, vi);
	       break;
	  }
	  break;

	case KeyRelease:
	  keysym = XLookupKeysym((XKeyEvent *)cbs->event, 0);
	  switch(keysym){
	     case XK_Escape:
	       xxyz_quit_cb(xx->dialog, (XtPointer) &(xx->vi->xyz), NULL);
	       break;
	     default:
	       break;
	  }
	  break;

	case ButtonPress:
	  XmProcessTraversal(xx->glw, XmTRAVERSE_CURRENT);
	  if (cbs->event->xbutton.state & Button1Mask)
	       button1 = True;
	  if (cbs->event->xbutton.state & Button2Mask)
	       button2 = True;
	  if (cbs->event->xbutton.state & Button3Mask)
	       button3 = True;

	  switch(cbs->event->xbutton.button){
	     case Button1:
	       if ((button2) || (button3))
		    break;
	       but1downt = cbs->event->xbutton.time;
	       xx->whichbox = xxyz_getxyz(xx,  cbs->event->xbutton.x,
					  cbs->event->xbutton.y,
					  &mx, &my, &mz);
	       break;
	     case Button2:
	       if ((button1) || (button3))
		    break;
	       xxyzButton2(xx, cbs->event->xbutton.x, cbs->event->xbutton.y);
	       break;
	     case Button3:
	       if ((button1) || (button2))
		    break;
	       xxyzButton3(xx, cbs->event->xbutton.x, cbs->event->xbutton.y);
	       break;
	     default:
	       break;
	  }
	  xx->lmx = cbs->event->xmotion.x;
	  xx->lmy = cbs->event->xmotion.y;
	  break;

	case ButtonRelease:
	  if (cbs->event->xbutton.button == Button1){
	       if ((cbs->event->xbutton.time - but1downt) > 250)
		    break;
	       xxyzButton1(xx, cbs->event->xbutton.x, cbs->event->xbutton.y);
	  }
	  break;

	case MotionNotify:
	  if (cbs->event->xmotion.state & Button1Mask)
	       button1 = True;
	  if (cbs->event->xmotion.state & Button2Mask)
	       button2 = True;
	  if (cbs->event->xmotion.state & Button3Mask)
	       button3 = True;
	  if ( (button1) && (!button2) && (!button3) &&
	       (cbs->event->xmotion.time - but1downt) > 250)
	       xxyzB1Drag(xx, cbs->event->xmotion.x, cbs->event->xmotion.y);
	  if ( (!button1) && (button2) && (!button3))
	       xxyzB2Drag(xx, cbs->event->xmotion.x, cbs->event->xmotion.y);
	  if ( (!button1) && (!button2) && (button3))
	       xxyzB3Drag(xx, cbs->event->xmotion.x, cbs->event->xmotion.y);

	  xx->lmx = cbs->event->xmotion.x;
	  xx->lmy = cbs->event->xmotion.y;
	  break;

	default:
	  break;
     }
}

static int xxyz_getxyz(struct xxyzwin *xx, 
		       int x, int y, int *mx, int *my, int *mz)
{
     int nx, ny, nz;
     /* DNM 1/23/02: turn this from float to int to keep calling expressions
	as ints */
     int b2 = XYZ_BSIZE;
     float scale;
     struct ViewInfo *vi = xx->vi;

     y = xx->winy - y - 1;
     x -= XYZ_BSIZE + xx->xwoffset;
     y -= XYZ_BSIZE + xx->ywoffset;

     nx = vi->xsize * xx->zoom;
     ny = vi->ysize * xx->zoom;
     nz = vi->zsize * xx->zoom;

     scale = 1.0/xx->zoom;

     /* Click in main image, Z-Section */
     if (mouse_in_box(0, 0, nx, ny, x, y)){
	  *mx = x * scale;
	  *my = y * scale;
	  *mz = vi->zmouse;
	  ivwBindMouse(vi);
	  return(3);
     }

     /* Click in top image, Y-Section */
     if (mouse_in_box(0, ny + b2,
		      nx,  ny + nz - 1 + b2, x, y)){
	  *mx = x * scale;
	  *my = vi->ymouse;
	  *mz = (y - b2 - ny) * scale;
	  ivwBindMouse(vi);
	  return(2);
     }

     /* Click in right image */
     if (mouse_in_box(nx + b2, 0,
		      nx + b2 + nz - 1,
		      ny, x, y)){
	  *mx = vi->xmouse;
	  *my = y * scale;
	  *mz = (x - b2 - nx) * scale;
	  return(1);
     }

     /* Z-Section Gadget */
     if (mouse_in_box(nx + b2, ny + b2,
		      nx + b2 + nz - 1,
		      ny + b2 + nz - 1, x, y)){
	  *mx = vi->xmouse;
	  *my = vi->ymouse;
	  *mz = 0.5 * ((y - b2 - ny) + (x - b2 - nx)) * scale;
	  return(6);
     }
     
     /* Y-Section Gadget */
     if (mouse_in_box(0, ny, nx, ny + b2, x, y)){
	  *mx = x * scale;
	  *my = vi->ymouse;
	  *mz = vi->zmouse;
	  return(5);
     }
     
     /* X-Section Gadget */
     if (mouse_in_box(nx, 0, nx + b2, ny, x, y)){
	  *mx = vi->xmouse;
	  *my = y * scale;
	  *mz = vi->zmouse;
	  return(4);
     }
     
     return(0);
}


static void xxyzButton1(struct xxyzwin *xx, int x, int y)
{
     int mx, my, mz;
     ImodView *vi   = xx->vi;
     Imod     *imod = vi->imod;
     Ipoint pnt, *spnt;
     Iindex index;
     int i, temp_distance;
     int distance = -1;
     float selsize = IMOD_SELSIZE / xx->zoom;
     int box = xxyz_getxyz(xx, x, y, &mx, &my, &mz);

     if (!box)
	  return;

     if (xx->vi->ax){
	  if (xx->vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
	       autox_fillmouse(xx->vi, mx, my);
	       return;
	  }
     }

     /* DNM 1/23/02: Adopt code from Zap window, get nearest point if in the
	main display panel */
     if (xx->vi->imod->mousemode == IMOD_MMODEL && box == 3){
	  pnt.x = mx;
	  pnt.y = my;
	  pnt.z = mz;
	  vi->xmouse = mx;
	  vi->ymouse = my;
	  vi->zmouse = mz;
	  vi->imod->cindex.contour = -1;
	  vi->imod->cindex.point = -1;

	  for (i = 0; i < imod->objsize; i++){
	       index.object = i;
	       temp_distance = imod_obj_nearest
		   (&(vi->imod->obj[i]), &index , &pnt, selsize);
	       if (temp_distance == -1)
		    continue;
	       if (distance == -1 || distance > temp_distance){
		    distance      = temp_distance;
		    vi->imod->cindex.object  = index.object;
		    vi->imod->cindex.contour = index.contour;
		    vi->imod->cindex.point   = index.point;
		    spnt = imodPointGet(vi->imod);
		    if (spnt){
			 vi->xmouse = spnt->x;
			 vi->ymouse = spnt->y;
		    }
	       }
	  }
	  ivwBindMouse(xx->vi);
	  imodDraw(vi, IMOD_DRAW_RETHINK | IMOD_DRAW_XYZ);
	  return;
     }

     xx->vi->xmouse = mx;
     xx->vi->ymouse = my;
     xx->vi->zmouse = mz;
     ivwBindMouse(xx->vi);
     
     /* DNM 1/23/02: make it update all windows */
     imodDraw(xx->vi, IMOD_DRAW_XYZ);
     return;
}

static void xxyzButton2(struct xxyzwin *xx, int x, int y)
{
     int mx, my, mz;
     int movie;
     struct Mod_Object  *obj;
     struct Mod_Contour *cont;
     struct Mod_Point   point;
     int pt;

     movie = xxyz_getxyz(xx, x, y, &mx, &my, &mz);
     if (!movie)
	  return;

     if (xx->vi->ax){
	  if (xx->vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
	       autox_sethigh(xx->vi, mx, my);
	       return;
	  }
     }


     if (xx->vi->imod->mousemode == IMOD_MMOVIE){
	  switch(movie){
	     case 1:
	     case 4:
	       imodMovieXYZT(xx->vi, 1,
			     MOVIE_DEFAULT, MOVIE_DEFAULT, MOVIE_DEFAULT);
	       break;
	     case 2:
	     case 5:
	       imodMovieXYZT(xx->vi, MOVIE_DEFAULT, 1,
			     MOVIE_DEFAULT, MOVIE_DEFAULT);
	       break;
	     case 3:
	     case 6:
	       imodMovieXYZT(xx->vi, MOVIE_DEFAULT, MOVIE_DEFAULT, 1,
			     MOVIE_DEFAULT);
	       break;
	  }
	  return;
     }

     if (movie != 3)
	  return;

     obj = imodObjectGet(xx->vi->imod);
     if (!obj)
	  return;

     cont = imodContourGet(xx->vi->imod);
     if (!cont){
	  xx->vi->imod->cindex.contour = obj->contsize - 1;
	  NewContour(xx->vi->imod);
	  cont = imodContourGet(xx->vi->imod);
	  if (!cont)
	       return;
     }

     /* DNM: don't make closed contours wild if they're not */
     if (cont->psize &&  iobjClose(obj->flags) && !(cont->flags & ICONT_WILD)
	 && cont->pts[0].z != mz) {
	  wprint("\aXYZ will not add a point on a different section to"
		 " a co-planar closed contour");
	  return;
     }
     point.x = mx;
     point.y = my;
     point.z = mz;
     pt = xx->vi->imod->cindex.point;

     if ((cont->psize - 1) == pt)
	  NewPoint(xx->vi->imod, &point);
     else
	  InsertPoint(xx->vi->imod, &point, pt + 1);

     /* For a non-closed contour, maintain the wild flag */
     if (!iobjClose(obj->flags) && !(cont->flags & ICONT_WILD))
	  imodel_contour_check_wild(cont);
     xx->vi->xmouse  = mx;
     xx->vi->ymouse  = my;

     /* DNM 1/23/02: make it update all windows */
     imodDraw(xx->vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
     return;
}

static void xxyzButton3(struct xxyzwin *xx, int x, int y)
{
     int mx, my, mz;
     int movie;
     struct Mod_Contour *cont;
     int pt;

     movie = xxyz_getxyz(xx, x, y, &mx, &my, &mz);
     if (!movie)
	  return;

     if (xx->vi->ax){
	  if (xx->vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
	       autox_setlow(xx->vi, mx, my);
	       return;
	  }
     }
     
     
     if (xx->vi->imod->mousemode == IMOD_MMOVIE){
	  switch(movie){
	     case 1:
	       imodMovieXYZT(xx->vi, -1, 
			     MOVIE_DEFAULT, MOVIE_DEFAULT, MOVIE_DEFAULT);
	       break;
	     case 2:
	       imodMovieXYZT(xx->vi, MOVIE_DEFAULT, -1,
			     MOVIE_DEFAULT, MOVIE_DEFAULT);
	       break;
	     case 3:
	       imodMovieXYZT(xx->vi, MOVIE_DEFAULT, MOVIE_DEFAULT, -1,
			     MOVIE_DEFAULT);
	       break;
	     default:
	       break;
	  }
	  return;
     }

     if (movie != 3)
	  return;

     cont = imodContourGet(xx->vi->imod);
     pt   = xx->vi->imod->cindex.point;
     if (!cont)
	  return;
     if (pt < 0)
	  return;
     if (!ivwPointVisible(xx->vi, &(cont->pts[pt])))
	 return;
	 
     cont->pts[pt].x = mx;
     cont->pts[pt].y = my;
     xx->vi->xmouse  = mx;
     xx->vi->ymouse  = my;
     ivwBindMouse(xx->vi);

     /* DNM 1/23/02: make it update all windows */
     imodDraw(xx->vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
     return;
}

/* DNM 1/20/02: add statements to implement pan */
static void xxyzB1Drag(struct xxyzwin *xx, int x, int y)
{
     int sx, sy;
     int nx, ny;
     int b2 = XYZ_BSIZE;
     float scale;
     struct ViewInfo *vi = xx->vi;
     int new;

     sy = xx->winy - y - 1;
     sx = x - (XYZ_BSIZE + xx->xwoffset);
     sy -= XYZ_BSIZE + xx->ywoffset;

     nx = vi->xsize * xx->zoom;
     ny = vi->ysize * xx->zoom;

     scale = 1.0/xx->zoom;

     switch (xx->whichbox) {
	case 0:
	case 1:
	case 2:
	case 3:
	  xx->xtrans += (x - xx->lmx);
	  xx->ytrans -= (y - xx->lmy);
	  xxyz_draw(xx);
	  return;

	case 6:
	  new = 0.5 * ((sy - b2 - ny) + (sx - b2 - nx)) * scale;
	  if (xx->vi->zmouse == new)
	       return;
	  xx->vi->zmouse = new;
	  break;

	case 5:
	  new = sx * scale;
	  if (xx->vi->xmouse == new)
	       return;
	  xx->vi->xmouse = new; 
	  break;

	case 4:
	  new = sy * scale;
	  if (xx->vi->ymouse == new)
	       return;
	  xx->vi->ymouse = new;
	  break;

     }
     ivwBindMouse(xx->vi);
     imodDraw(xx->vi, IMOD_DRAW_XYZ);
     return;
}

static void xxyzB2Drag(struct xxyzwin *xx, int x, int y)
{
     int mx, my, mz, box;
     struct Mod_Object  *obj;
     struct Mod_Contour *cont;
     struct Mod_Point   point;
     double dist;
     int pt;

     if (xx->vi->ax){
	  if (xx->vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
	       box = xxyz_getxyz(xx, x, y, &mx, &my, &mz);
	       if (box != 3)
		    return;
	       autox_sethigh(xx->vi, mx, my);
	       return;
	  }
     }

     if (xx->vi->imod->mousemode != IMOD_MMODEL)
	  return;

     box = xxyz_getxyz(xx, x, y, &mx, &my, &mz);
     if (box != 3)
	  return;

     obj = imodel_object_get(xx->vi->imod);
     if (!obj)
	  return;
     cont = imodContourGet(xx->vi->imod);
     if (!cont)
	  return;
     pt = xx->vi->imod->cindex.point;
     if (pt < 0)
	  return;

     /* DNM: don't make closed contours wild if they're not */
     if (cont->psize &&  iobjClose(obj->flags) && !(cont->flags & ICONT_WILD)
	 && cont->pts[0].z != mz) {
	  wprint("\aXYZ will not add a point on a different section to"
		 " a co-planar closed contour");
	  return;
     }
     point.x = mx;
     point.y = my;
     point.z = mz;

     dist = imodel_point_dist(&point, &(cont->pts[pt]));
     if (dist < xx->vi->imod->res)
	  return;

     if ((cont->psize - 1) == pt)
	  NewPoint(xx->vi->imod, &point);
     else
	  InsertPoint(xx->vi->imod, &point, pt + 1);

     /* For a non-closed contour, maintain the wild flag */
     if (!iobjClose(obj->flags) && !(cont->flags & ICONT_WILD))
	  imodel_contour_check_wild(cont);
     xx->vi->xmouse  = mx;
     xx->vi->ymouse  = my;
     ivwBindMouse(xx->vi);

     /* DNM 1/23/02: make it update all windows */
     imodDraw(xx->vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
     return;
}

static void xxyzB3Drag(struct xxyzwin *xx, int x, int y)
{
     int mx, my, mz, box;
     struct Mod_Object  *obj;
     struct Mod_Contour *cont;
     struct Mod_Point   point;
     double dist;
     int pt;

     if (xx->vi->ax){
	  if (xx->vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
	       box = xxyz_getxyz(xx, x, y, &mx, &my, &mz);
	       if (box != 3)
		    return;
	       autox_setlow(xx->vi, mx, my);
	       return;
	  }
     }

     if (xx->vi->imod->mousemode != IMOD_MMODEL)
	  return;

     box = xxyz_getxyz(xx, x, y, &mx, &my, &mz);
     if (box != 3)
	  return;

     obj = imodObjectGet(xx->vi->imod);
     if (!obj)
	  return;
     cont = imodContourGet(xx->vi->imod);
     if (!cont)
	  return;
     pt = xx->vi->imod->cindex.point;
     if (pt < 0)
	  return;

     point.x = mx;
     point.y = my;
     point.z = mz;

     dist = imodel_point_dist(&point, &(cont->pts[pt]));
     if (dist < xx->vi->imod->res)
	  return;

     pt++;
     if (pt >= cont->psize)
	  pt = cont->psize - 1;

     cont->pts[pt].x = mx;
     cont->pts[pt].y = my;
     cont->pts[pt].z = mz;
     xx->vi->imod->cindex.point = pt;
     xx->vi->xmouse  = mx;
     xx->vi->ymouse  = my;
     ivwBindMouse(xx->vi);

     /* DNM 1/23/02: make it update all windows */
     imodDraw(xx->vi, IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
     return;
}


static void xyz_overclear(struct xxyzwin *xx)
{
     return;
}

static void xyzSetSubimage(int absStart, int winSize, int imSize, float zoom,
			   int *drawsize, int *woffset, int *dataStart)
{
     *dataStart = 0;
     *woffset = absStart;

     /* If the absolute starting point is negative, then adjust the data start
	for this; throw away one more pixel and set a small window offset
	if necessary to keep pixels synchronized */
     if (absStart < 0) {
	  *dataStart = -absStart / zoom;
	  *woffset = 0;
	  if (zoom * *dataStart < -absStart) {
	       (*dataStart)++;
	       *woffset = zoom * *dataStart + absStart;
	  }
     }

     /* limit # of pixels to draw if it goes past end of window - this also
      takes care of case where image starts past end of window */
     *drawsize = imSize - *dataStart;
     if (*drawsize * zoom + *woffset > winSize)
	  *drawsize = (winSize - *woffset) / zoom;
}     

static void xyzDrawImage(struct xxyzwin *win)
{

     unsigned int x, y, z, i;
     int nx = win->vi->xsize;
     int ny = win->vi->ysize;
     int nz = win->vi->zsize;
     unsigned char *id;
     unsigned char *fdata;
     unsigned long cyi;
     int cx, cy, cz, iz;
     int imdataxsize;
     unsigned char **imdata;
     int extraImSize;
     int dataOffset;
     int drawsize;
     GLenum type, format;
     GLint  unpack = b3dGetImageType(&type, &format);
     int sx = nx * win->zoom;
     int sy = ny * win->zoom;
     int sz = nz * win->zoom;
     int wx1, wx2, wy1, wy2;
     int xoffset1, xoffset2, yoffset1, yoffset2;
     int width1, height1, width2, height2;
	  
	  
     if (!win) return;

     if (!win->exposed) return;     /* DNM: avoid crashes if Zap is movieing*/

     /* DNM 3/5/01: changed to avoid very slow ivwGetValue when data are in
	cache; set up image pointer tables */
     imdata = (unsigned char **)
	  malloc(sizeof(unsigned char *) * win->vi->zsize);

     if (!imdata)
	  return;

     if (win->vi->vmSize) {
	  /* For cached data, get pointers to data that exist at this time */
	  for (i = 0; i < win->vi->zsize; i++)
	       imdata[i] = NULL;
	  for (i = 0; i < win->vi->vmSize; i++) {
	       iz = win->vi->vmCache[i].cz;
	       if (iz < win->vi->zsize && iz >= 0 &&
		   win->vi->vmCache[i].ct == win->vi->ct){
		    imdata[iz] = win->vi->vmCache[i].sec->data.b;
	       }
	  }

     } else if (!win->vi->fakeImage) {
	  /* for loaded data, get pointers from win->vi */
	  for (i = 0; i < win->vi->zsize; i++)
	       imdata[i] = win->vi->idata[i];
     }
     /* Just take the X size, do not allow for possibility of cached data 
	having different X sizes */
     imdataxsize = win->vi->xsize;

     if (win->vi->vmSize){
	  win->lx = win->ly = -1;
     }
	  
     glClearIndex(App->background);
     /* DNM: need to set clear colors for rgb mode */
     if (App->rgba)
	  glClearColor(64./255., 64./255 , 96./255, 0.);
     
     /* DNM 1/20/02: remove the XYZ_CLEAR_HACK */
     
     glClear(GL_COLOR_BUFFER_BIT);

     ivwGetLocation(win->vi, &cx, &cy, &cz);
     cyi = cy * nx;

     /* Pass the image offset routine the effective image size, including
	de-zoomed borders, and convert a data offset into a negative window
	offset */
     extraImSize = floor((double)(3. * XYZ_BSIZE / win->zoom + 0.5));
     b3dSetImageOffset(win->winx, nx + nz + extraImSize, win->zoom, &drawsize,
		       &win->xtrans, &win->xwoffset, &dataOffset);
     if (dataOffset)
	  win->xwoffset = -floor((double)(dataOffset * win->zoom + 0.5));

     b3dSetImageOffset(win->winy, ny + nz + extraImSize, win->zoom, &drawsize,
		       &win->ytrans, &win->ywoffset, &dataOffset);
     if (dataOffset)
	  win->ywoffset = -floor((double)(dataOffset * win->zoom + 0.5));


     /* Now compute drawing parameters for each of the subareas */
     xyzSetSubimage(win->xwoffset + XYZ_BSIZE, win->winx, nx, win->zoom,
		    &width1, &wx1, &xoffset1);
     xyzSetSubimage(win->xwoffset + sx + 2 * XYZ_BSIZE, win->winx, nz, 
		    win->zoom, &width2, &wx2, &xoffset2);
     xyzSetSubimage(win->ywoffset + XYZ_BSIZE, win->winy, ny, win->zoom,
		    &height1, &wy1, &yoffset1);
     xyzSetSubimage(win->ywoffset + sy + 2 * XYZ_BSIZE, win->winy, nz, 
		    win->zoom, &height2, &wy2, &yoffset2);

     /* printf ("width1 %d  height1 %d  width2 %d  height2 %d\n", width1,
	    height1, width2, height2);
	    printf ("wx1 %d  xoffset1 %d  wy1 %d  yoffset1 %d\n", wx1,
	    xoffset1, wy1, yoffset1); */
     if (width1 > 0 && height1 > 0){
	  win->lz = cz;
	  id = ivwGetCurrentZSection(win->vi);
	  b3dDrawGreyScalePixelsHQ(id, nx,ny, xoffset1, yoffset1, wx1, wy1,
				   width1, height1, win->xydata,
				   win->vi->rampbase, win->zoom, win->zoom, 
				   win->hq, cz);
     }

     if (width2 > 0 && height1 > 0) {
	  fdata  = win->fdatayz;
	  if (cx != win->lx){
	       win->lx = cx;
	       for(z = 0; z < nz; z++) {
		    if (!win->vi->fakeImage && imdata[z]) {
			 for(i = z, y = 0; y < ny; y++, i += nz)
			      fdata[i] = imdata[z][cx + (y * imdataxsize)];
		    } else {
			 for(i= z, y = 0; y < ny; y++, i += nz)
			      fdata[i] = 0;
		    }
	       }
	  }
	  b3dDrawGreyScalePixelsHQ(win->fdatayz, nz, ny, xoffset2, yoffset1,
				   wx2, wy1, width2, height1, win->yzdata,
				   win->vi->rampbase, win->zoom, win->zoom, 
				   win->hq, cx);
     }

     if (width1 > 0 && height2 > 0) {
	  fdata  = win->fdataxz;
	  if (cy != win->ly){
	       win->ly = cy;
	       for(i = 0,z = 0; z < nz; z++) {
		    if (!win->vi->fakeImage && imdata[z]) {
			 for(x = 0; x < nx; x++, i++)
			      fdata[i] = imdata[z][x + (cy * imdataxsize)];
		    } else {
			 for(x = 0; x < nx; x++, i++)
			      fdata[i] = 0;
		    }
	       }
	  }	  
	  b3dDrawGreyScalePixelsHQ(win->fdataxz, nx, nz, xoffset1, yoffset2,
				   wx1, wy2, width1, height2, win->xzdata,
				   win->vi->rampbase, win->zoom, win->zoom, 
				   win->hq, cy);
     }

     free(imdata);
     
     return;
}

static void xyzDrawCurrentLines(struct xxyzwin *xx)
{
     int cx, cy, cz;
     float z = xx->zoom;
     int bx = XYZ_BSIZE + xx->xwoffset;
     int by = XYZ_BSIZE + xx->ywoffset;
     int bx2 = bx + XYZ_BSIZE + floor((double)(xx->vi->xsize * z + 0.5));
     int by2 = by + XYZ_BSIZE + floor((double)(xx->vi->ysize * z + 0.5));

     /* DNM 1/23/02: Put the line in the middle now that one can drag it */
     int bpad = XYZ_BSIZE / 2;

     int nx = xx->vi->xsize;
     int ny = xx->vi->ysize;
     int nz = xx->vi->zsize;

     b3dColorIndex(App->background);
     b3dLineWidth(1);

     ivwGetLocation(xx->vi, &cx, &cy, &cz);
     b3dColorIndex(App->foreground);

     /* draw xz location line. */
     b3dDrawLine(bx2, 
		 (int)(by2 + z * cz),
		 (int)(bx2 + z * (nz - 1)),
		 (int)(by2 + z * cz));

     b3dDrawLine((int)(bx2 + z * cz),
		 by2,
		 (int)(bx2 + z * cz),
		 (int)(by2 + z * (nz - 1)));
     
     b3dDrawLine((int)(bx + z * cx),
		 (int)(by + z * ny + bpad),
		 (int)(bx2 + z * nz),
		 (int)(by + z * ny + bpad));
     b3dDrawLine((int)(bx + z * nx + bpad),
		 (int)(by2 + z * nz),
		 (int)(bx + z * nx + bpad),
		 (int)(by + z * cy));
     return;
}

static void xyzDrawGhost(struct xxyzwin *xx)
{
     return;
}

/* DNM 1/20/02: add argument ob to be able to reset color properly */
static void xyzDrawContour(struct xxyzwin *xx, Iobj *obj, int ob, Icont *cont)
{
     ImodView *vi = xx->vi;
     Ipoint *point;
     int pt, npt = 0, ptsonsec;
     float vert[2];
     float drawsize;
     float z = xx->zoom;
     int bx = XYZ_BSIZE + xx->xwoffset;
     int by = XYZ_BSIZE + xx->ywoffset;
     int bx2 = bx + XYZ_BSIZE + floor((double)(vi->xsize * z + 0.5));
     int by2 = by + XYZ_BSIZE + floor((double)(vi->ysize * z + 0.5));
     
     if (!cont->psize)
	  return;
     
     if (iobjClose(obj->flags)){
	  /* closed  contour: draw all points on visible section, just
	     connecting visible ones if points go out of section */
	  b3dBeginLine();
	  for (pt = 0; pt < cont->psize; pt++){
	       if (!ivwPointVisible(vi, &(cont->pts[pt]))) {
		    /* DNM: If contour is wild, keep going; if not, done with
		       it */
		    if (cont->flags & ICONT_WILD)
			 continue;
		    break;
	       }
	       b3dVertex2i((int)(z * cont->pts[pt].x + bx),  
			   (int)(z * cont->pts[pt].y + by));
	  }
	  if (ivwPointVisible(vi, cont->pts)){
	       b3dVertex2i((int)(bx + z * cont->pts->x), 
			   (int)(by + z * cont->pts->y));
	  }
	  b3dEndLine();
	  
	  /* draw symbols for all points on section */
	  for (pt = 0; pt < cont->psize; pt++){
	       if (!ivwPointVisible(vi, &(cont->pts[pt]))) {
		    /* DNM: If contour is wild, keep going; if not, done with
		       it */
		    if (cont->flags & ICONT_WILD)
			 continue;
		    break;
	       }
	       zapDrawSymbol((int)(z * cont->pts[pt].x + bx), 
			     (int)(z * cont->pts[pt].y + by),
			     obj->symbol, obj->symsize, obj->symflags);
	  }
     }
     
     /* Open contours: draw symbols for all points on section and connecting
	lines just between point pairs on section */
     if (iobjOpen(obj->flags)){
	  for(pt = 0; pt < cont->psize; pt++){
	       point = &(cont->pts[pt]);
	       if (ivwPointVisible(vi, point)){
		    zapDrawSymbol((int)(z * cont->pts[pt].x + bx), 
				  (int)(z * cont->pts[pt].y + by),
				  obj->symbol, obj->symsize, obj->symflags);
		    

			 if (ivwPointVisible(vi, &(cont->pts[pt+1])))
			      b3dDrawLine((int)(z * point->x + bx),
					  (int)(z * point->y + by),
					  (int)(z * cont->pts[pt+1].x + bx),
					  (int)(z * cont->pts[pt+1].y + by));
		    
	       }
	       
	  }	       
     }
     
     /* scattered contour */
     if (iobjScat(obj->flags)){
	  for (pt = 0; pt < cont->psize; pt++){
	       /* draw symbol if point on section */
	       if (ivwPointVisible(vi, &(cont->pts[pt])))
		    zapDrawSymbol((int)(z * cont->pts[pt].x + bx), 
				  (int)(z * cont->pts[pt].y + by),
				  obj->symbol, obj->symsize, obj->symflags);
	       
	       drawsize = imodPointGetSize(obj, cont, pt);
	       if (drawsize > 0)
		    if (ivwPointVisible(vi, &(cont->pts[pt]))){
			 /* If there's a size, draw a circle and a plus for a
			    circle that's big enough */
			 b3dDrawCircle((int)(bx + z * cont->pts[pt].x),
				       (int)(by + z * cont->pts[pt].y),
				       (int)(z * drawsize));
			 if (drawsize > 3)
			      b3dDrawPlus((int)(bx + z * cont->pts[pt].x),
					  (int)(by + z * cont->pts[pt].y), 3);
		    }else{
			 /* for off-section, compute size of circle and draw 
			    that */
			 if (drawsize > 1){
			      /* draw a smaller circ if further away. */
			      vert[0] = (cont->pts[pt].z - vi->zmouse) *
				   App->cvi->imod->zscale;
			      if (vert[0] < 0)
				   vert[0] *= -1.0f;

			      if (vert[0] < drawsize - 0.01){
			           vert[1] =
				     z * sqrt((double)
					  (drawsize * drawsize
					  - vert[0] * vert[0]));
				   
				   b3dDrawCircle((int)(bx + z*cont->pts[pt].x),
						 (int)(by + z*cont->pts[pt].y),
						 (int)vert[1]);
			      }
			 }
		    }
	  }
     }

     /* draw end markers if requested */
     if (obj->symflags & IOBJ_SYMF_ENDS){
	  if (ivwPointVisible(vi, &(cont->pts[cont->psize-1]))){
	       b3dColorIndex(App->endpoint);
	       b3dDrawCross((int)(bx + z * cont->pts[cont->psize-1].x),
			    (int)(by + z * cont->pts[cont->psize-1].y),
			    obj->symsize/2);
	  }
	  if (ivwPointVisible(vi, cont->pts)){
	       b3dColorIndex(App->bgnpoint);
	       b3dDrawCross((int)(bx + z * cont->pts->x),
			    (int)(by + z * cont->pts->y),
			    obj->symsize/2);
	  }
	  /* DNM 1/21/02: need to reset color this way, not wih b3dColorIndex*/
	  imodSetObjectColor(ob);

     }
     return;
}

static void xyzDrawCurrentContour(struct xxyzwin *xx, Iobj *obj, int ob,
				  Icont *cont)
{
     ImodView *vi = xx->vi;
     Ipoint *point;
     int pt;
     int cz = vi->zmouse+0.5f;
     float z = xx->zoom;
     int bx = XYZ_BSIZE + xx->xwoffset;
     int by = XYZ_BSIZE + xx->ywoffset;
     int bx2 = bx + XYZ_BSIZE + floor((double)(vi->xsize * z + 0.5));
     int by2 = by + XYZ_BSIZE + floor((double)(vi->ysize * z + 0.5));
     int drawsize;

     if (!cont->psize)
	  return;
     
     if (iobjClose(obj->flags)){
	  
	  /* closed contours: draw lines, including points on section */
	  b3dBeginLine();
	  for (pt = 0; pt < cont->psize; pt++){
	       if (!ivwPointVisible(vi, &(cont->pts[pt]))) {
		    /* DNM: If contour is wild, keep going; if not, done with
		       it */
		    if (cont->flags & ICONT_WILD)
			 continue;
		    break;
	       }
	       b3dVertex2i((int)(z * cont->pts[pt].x + bx), 
			   (int)(z * cont->pts[pt].y + by));
	  }
	  b3dEndLine();

	  /* draw symbols for points on section */
	  if (obj->symbol != IOBJ_SYM_NONE)
	       for (pt = 0; pt < cont->psize; pt++){
		    if (!ivwPointVisible(vi, &(cont->pts[pt]))) {
			 if (cont->flags & ICONT_WILD)
			      continue;
			 break;
		    }
		    zapDrawSymbol((int)(z * cont->pts[pt].x + bx), 
				  (int)(z * cont->pts[pt].y + by),
				  obj->symbol, obj->symsize, obj->symflags);
	       }

	  /* Draw projection of all lines into x/z view */
	  b3dBeginLine();
	  for (pt = 0; pt < cont->psize; pt++){
	       b3dVertex2i((int)(z * cont->pts[pt].x + bx),  
			   (int)(z * cont->pts[pt].z + by2));
	  }
	  b3dEndLine();


	  /* Draw projection of all lines into y/z view */
	  b3dBeginLine();
	  for (pt = 0; pt < cont->psize; pt++){
	       b3dVertex2i((int)(z * cont->pts[pt].z + bx2), 
			   (int)(z * cont->pts[pt].y + by));
	  }
	  b3dEndLine();
     }

     if (iobjOpen(obj->flags)){
	  /* Open objects: try to draw solid lines that are on-section and
	     dashed lines that are not */
	  if (cont->psize > 1){
	       for(pt = 1; pt < cont->psize; pt++){
		    
		    if ((cont->pts[pt].z == cz) && (cont->pts[pt-1].z == cz)){
			 b3dDrawLine((int)(bx + z * cont->pts[pt].x),
				     (int)(by + z * cont->pts[pt].y),
				     (int)(bx + z * cont->pts[pt-1].x),
				     (int)(by + z * cont->pts[pt-1].y));
			 continue;
		    }
		    b3dLineStyle(B3D_LINESTYLE_DASH);
		    b3dDrawLine((int)(bx + z * cont->pts[pt].x),
				(int)(by + z * cont->pts[pt].y),
				(int)(bx + z * cont->pts[pt-1].x),
				(int)(by + z * cont->pts[pt-1].y));
		    b3dLineStyle(B3D_LINESTYLE_SOLID);
		    
		    /* Draw a circle on an isolated point on-section */
		    if (pt < cont->psize - 1)
			 if ((cont->pts[pt].z == cz) &&
			     (cont->pts[pt-1].z != cz) &&
			     (cont->pts[pt+1].z != cz))
			      b3dDrawCircle((int)(bx + z * cont->pts[pt].x),
					    (int)(by + z *cont->pts[pt].y), 3);
	       }
	       /* Draw circles on first or last points if they are isolated
		  on-section */
	       if ((cont->pts[0].z == cz) && (cont->pts[1].z != cz))
		    b3dDrawCircle((int)(bx + z * cont->pts[0].x),
				  (int)(by + z * cont->pts[0].y), 3);
	       if ((cont->pts[cont->psize - 1].z == cz) &&
		   (cont->pts[cont->psize - 2].z != cz))
		    b3dDrawCircle
			 ((int)(bx + z * cont->pts[cont->psize - 1].x),
			  (int)(by + z * cont->pts[cont->psize - 1].y), 3);

	       /* draw symbols for points on section */
	       if (obj->symbol != IOBJ_SYM_NONE)
		    for (pt = 0; pt < cont->psize; pt++){
			 if (!ivwPointVisible(vi, &(cont->pts[pt]))) {
			      if (cont->flags & ICONT_WILD)
				   continue;
			      break;
			 }
			 zapDrawSymbol((int)(z * cont->pts[pt].x + bx), 
				       (int)(z * cont->pts[pt].y + by),
				       obj->symbol, obj->symsize,
				       obj->symflags);
		    }

	  }else{

	       /* Draw a circle for a single point, regardless of z */
	       b3dDrawCircle((int)(bx + z * cont->pts[0].x),
			     (int)(by + z * cont->pts[0].y), 3);
	  }

	  /* Draw projection of all lines into x/z view */
	  b3dBeginLine();
	  for (pt = 0; pt < cont->psize; pt++){
	       b3dVertex2i((int)(z * cont->pts[pt].x + bx),  
			   (int)(z * cont->pts[pt].z + by2));
	  }
	  b3dEndLine();


	  /* Draw projection of all lines into y/z view */
	  b3dBeginLine();
	  for (pt = 0; pt < cont->psize; pt++){
	       b3dVertex2i((int)(z * cont->pts[pt].z + bx2),
			   (int)(z * cont->pts[pt].y + by));
	  }
	  b3dEndLine();

	  return;
     }

     /* scatterd object */
     if (iobjScat(obj->flags)){
	  /* Just draw the contour with other routines */
	  xyzDrawContour(xx, obj, ob, cont);
	  
	  /* Draw points in other windows if that is where they are */
	  for (pt = 0; pt < cont->psize; pt++){
	       drawsize = z * imodPointGetSize(obj, cont, pt);
	       if ((int)cont->pts[pt].x == vi->xmouse){
		    zapDrawSymbol((int)(z * cont->pts[pt].z + bx2),
				  (int)(z * cont->pts[pt].y + by),
				  obj->symbol, obj->symsize, obj->symflags);
		    if (drawsize > 0)
			 b3dDrawCircle((int)(bx2 + z * cont->pts[pt].z),
				       (int)(by + z * cont->pts[pt].y),
				       drawsize);
	       }
	       if ((int)cont->pts[pt].y == vi->ymouse){
		    zapDrawSymbol((int)(z * cont->pts[pt].x + bx), 
				  (int)(z * cont->pts[pt].z + by2),
				  obj->symbol, obj->symsize, obj->symflags);
		    if (drawsize > 0)
			 b3dDrawCircle((int)(bx + z * cont->pts[pt].x),
				       (int)(by2 + z * cont->pts[pt].z),
				       drawsize);
	       }
	  }
     }

     /* draw end symbols in x/y views */
     if (obj->symflags & IOBJ_SYMF_ENDS){
	  if (ivwPointVisible(vi, &(cont->pts[cont->psize-1]))){
	       b3dColorIndex(App->endpoint);
	       b3dDrawCross((int)(bx + z * cont->pts[cont->psize-1].x),
			    (int)(by + z * cont->pts[cont->psize-1].y),
			    obj->symsize/2);
	  }
	  if (ivwPointVisible(vi, cont->pts)){
	       b3dColorIndex(App->bgnpoint);
	       b3dDrawCross((int)(bx + z * cont->pts->x),
			    (int)(by + z * cont->pts->y),
			    obj->symsize/2);
	  }
	  /* DNM 1/21/02: need to reset color this way, not wih ColorIndex */
	  imodSetObjectColor(ob);
     }
     
     return;
}

static void xyzDrawModel(struct xxyzwin *xx)
{
     Imod *imod = xx->vi->imod;
     Iobj *obj;
     Icont *cont;
     int ob, co;

     if (imod->drawmode <= 0)
	  return;
     if (xx->vi->ghostmode)
	  xyzDrawGhost(xx);
     
     for(ob = 0; ob < imod->objsize; ob++){
	  imodSetObjectColor(ob);
	  obj = &(imod->obj[ob]);
	  b3dLineWidth(obj->linewidth2);
	  for(co = 0; co < imod->obj[ob].contsize; co++){
	       cont = &(obj->cont[co]);
	       if ((co == imod->cindex.contour) &&
		   (ob == imod->cindex.object))
		    xyzDrawCurrentContour(xx, obj, ob, cont);
	       else
		    xyzDrawContour(xx, obj, ob, cont);
	  }
     }

     return;
}

static void xyzDrawCurrentPoint(struct xxyzwin *xx)
{
     Icont *cont = imodContourGet(xx->vi->imod);
     Ipoint *pnt = imodPointGet(xx->vi->imod);
     int psize = 3;
     int cx, cy, cz;
     float z = xx->zoom;
     int bx = XYZ_BSIZE + xx->xwoffset;
     int by = XYZ_BSIZE + xx->ywoffset;
     int bx2 = bx + XYZ_BSIZE + floor((double)(xx->vi->xsize * z + 0.5));
     int by2 = by + XYZ_BSIZE + floor((double)(xx->vi->ysize * z + 0.5));

     ivwGetLocation(xx->vi, &cx, &cy, &cz);

     if (!xx->vi->drawcursor)
	  return;

     /* Draw begin and end points of selected contour. */
     if (cont){
	  if (cont->psize > 1){
	       if ((int)cont->pts->z == cz){
		    b3dColorIndex(App->bgnpoint);
		    b3dDrawCircle((int)(z * cont->pts->x+bx),
				  (int)(z * cont->pts->y+by), 2);
	       }
	       if ((int)cont->pts[cont->psize - 1].z == cz){
		    b3dColorIndex(App->endpoint);
		    b3dDrawCircle((int)(z * cont->pts[cont->psize - 1].x+bx),
				  (int)(z*cont->pts[cont->psize - 1].y+by), 2);
	       }
	  }
     }
     
     /* Draw location of current point. */
     /* DNM 1/21/02: do it like zap window, draw only if in model mode,
	otherwise draw crosses at current mouse point */
     if (xx->vi->imod->mousemode == IMOD_MMODEL &&  pnt){
	  
	  if ((int)(pnt->z) == cz){
	       b3dColorIndex(App->foreground);
	  }else{
	       b3dColorIndex(App->shadow);
	  }
	  b3dDrawCircle((int)(z * pnt->x+bx), (int)(z * pnt->y+by), psize);
	  b3dColorIndex(App->foreground);
	  b3dDrawPlus((int)(z*pnt->x+bx), (int)(z*cz + by2), 3);
	  b3dDrawPlus((int)(z * cz + bx2), (int)(by+z*pnt->y), 3);
	  return;
     }
     b3dColorIndex(App->foreground);
     b3dDrawPlus((int)(z*cx+bx), (int)(z*cy+by), 3);
     b3dDrawPlus((int)(z*cx+bx), (int)(z*cz+by2), 3);
     b3dDrawPlus((int)(bx2+z*cz), (int)(by+z*cy), 3);
     
     return;
}

static void xyzDrawAuto(struct xxyzwin *xx)
{
     ImodView *vi = xx->vi;
     int i, j;
     float vert[2];
     unsigned short cdat;
     int x, y;
     unsigned long pixel;
     

     if (!vi->ax)
	  return;
     
     if (!vi->ax->filled)
	  return;
     
     if (vi->ax->cz != vi->zmouse)
	  return;

     /* Buggy need to fix. */

#ifdef FIX_xyzDrawAuto_BUG
     cdat = App->endpoint;

     for (j = 0; j < vi->ysize; j++){
	  y = j + XYZ_BSIZE;
	  for(i = 0; i < vi->xsize; i++){
	       x = i + XYZ_BSIZE;
	       if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_FLOOD){
		    pixel = App->endpoint;
		    if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_BLACK){
			 pixel = vi->rampbase;
		    }
	       }else{
		    if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_BLACK){
			 pixel = vi->rampbase;
		    }
		    
		    if (vi->ax->data[i + (j * vi->xsize)] & AUTOX_WHITE){
			 pixel = vi->rampbase + vi->rampsize;
		    }
	       }
	       b3dColorIndex(pixel);
	       b3dDrawFilledRectangle(x, y, 1,1);
	  }
     }
#endif
     return;
}

static void xxyz_overclear(struct xxyzwin *xx)
{
     return;
}


/* DNM 1/21/02: eliminate OpenGL scaling of native coordinates, make all
   model drawing routines multiply coordinates by zoom */
static void xxyz_draw(struct xxyzwin *xx)
{
     float z = xx->zoom;
     int bx = XYZ_BSIZE + xx->xwoffset;
     int by = XYZ_BSIZE + xx->ywoffset;
     int bx2 = bx + XYZ_BSIZE + floor((double)(xx->vi->xsize * z + 0.5));
     int by2 = by + XYZ_BSIZE + floor((double)(xx->vi->ysize * z + 0.5));
     if (!xx)
	  return;
     if (!xx->exposed) return;     /* DNM: avoid crashes if Zap is movieing*/

     xxyz_overclear(xx);

     b3dWinset(App->display, xx->glw, xx->context);

     xyzDrawImage(xx);

     xyzDrawModel(xx);
     xyzDrawCurrentLines(xx);
     xyzDrawCurrentPoint(xx);
     xyzDrawAuto(xx);

     if (xyzShowSlice){
	  b3dColorIndex(App->foreground);
	  
	  b3dDrawLine((int)(bx + (xx->vi->slice.zx1 * xx->zoom)), 
		      (int)(by + (xx->vi->slice.zy1 * xx->zoom)),
		      (int)(bx + (xx->vi->slice.zx2 * xx->zoom)), 
		      (int)(by + (xx->vi->slice.zy2 * xx->zoom)));

	  b3dDrawLine((int)(bx + (xx->vi->slice.yx1 * xx->zoom)),
		      (int)(by2+ (xx->vi->slice.yz1 * xx->zoom)),
		      (int)(bx + (xx->vi->slice.yx2 * xx->zoom)),
		      (int)(by2+ (xx->vi->slice.yz2 * xx->zoom)));

	  b3dDrawLine((int)(bx2+ (xx->vi->slice.xz1 * xx->zoom)),
		      (int)(by + (xx->vi->slice.xy1 * xx->zoom)),
		      (int)(bx2+ (xx->vi->slice.xz2 * xx->zoom)),
		      (int)(by + (xx->vi->slice.xy2 * xx->zoom)));

	  xyzShowSlice = 0;
     }
     b3dFlush();

     b3dSwapBuffers();
     return;
}



void xyzDraw_cb(ImodView *vi, void *client, long drawflag)
{
     struct xxyzwin *xx = (struct xxyzwin *)client;

     if ((!vi) || (!xx) || (!drawflag)) return;
     
     if (drawflag){
	  if (drawflag & IMOD_DRAW_IMAGE){

	       /* This happens whens a flip occurs: get new image spaces */
	       xx->lx = xx->ly = xx->lz = -1;
	       b3dFlushImage(xx->xydata);
	       b3dFlushImage(xx->xzdata);
	       b3dFlushImage(xx->yzdata);
	       if(xx->fdataxz)
		    free(xx->fdataxz);
	       if(xx->fdatayz)
		    free(xx->fdatayz);
	       xx->fdataxz  = (unsigned char *)malloc(vi->xsize * vi->zsize);
	       xx->fdatayz  = (unsigned char *)malloc(vi->ysize * vi->zsize);
	  }
	  if (drawflag & IMOD_DRAW_SLICE)
	       xyzShowSlice = 1;
	  xyz_draw(vi);
     }
     return;
}

static void xyzDrawShowSlice(struct xxyzwin *xx)
{
     return;
}

int xyz_draw_showslice(struct ViewInfo *vi)
{
     if (!vi->xyz)
	  return(-1);
     xyzShowSlice = 1;
     xyz_draw(vi);
     return(0);
}
