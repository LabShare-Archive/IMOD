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
     xx->fdata = NULL; 

     msize = vi->xsize * vi->ysize;
     if ((vi->xsize * vi->zsize) > msize)
	  msize = vi->xsize * vi->zsize;
     if ((vi->ysize * vi->zsize) > msize)
	  msize = vi->ysize * vi->zsize;

     xx->fdata  = (unsigned char *)malloc(sizeof(unsigned char) * msize);

     
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
	 (!xx->fdata)){
	  wprint("Error:\n\tXYZ window can't open due to low memory\n");
	  if(xx->fdata)
	       free(xx->fdata);
	  free(xx);
	  return(-1);
     }

     xx->winx = vi->xsize + vi->zsize + (3 * XYZ_BSIZE);
     xx->winy = vi->ysize + vi->zsize + (3 * XYZ_BSIZE);
     xx->vi   = vi;
     xx->exposed = False;

     xx->zoom = 1.0;
#ifdef DRAW_OpenGL
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
#endif

     xx->lx = xx->ly = xx->lz = -1;
     vi->xyz  = xx;

     xx->glw = XtVaCreateManagedWidget
	  ("xyz_glw", B3dDrawingAreaWidgetClass, xx->dialog,
	   XmNwidth, xx->winx,
	   XmNheight, xx->winy,
	   XmNnavigationType, XmNONE,
	   XmNtraversalOn, True,
	   XmNtranslations, XtParseTranslationTable (B3DGFX_Translations),
#ifdef DRAW_OpenGL
	   GLwNvisualInfo, App->visualinfoGL,
	   XmNcolormap,         App->cmapGL,
#endif
#ifdef DRAW_GL
	   GlxNglxConfig, B3DGFX_GLXconfig_dbo,
	   GlxNuseOverlay, TRUE,
#endif
	   NULL);
     imodOverrideTransTable(xx->glw,B3DGFX_Translations);
     
     XtAddCallback(xx->glw, B3dNexposeCallback, xxyz_expose_cb, (XtPointer)xx);
     XtAddCallback(xx->glw, B3dNresizeCallback, xxyz_resize_cb, (XtPointer)xx);
     XtAddCallback(xx->glw, B3dNinputCallback,  xxyz_input_cb,  (XtPointer)xx);
     

#ifdef DRAW_GL

     XtAddCallback(xx->glw, GlxNoverlayExposeCallback, 
		   xxyz_overex_cb, (XtPointer)xx);
#endif

     wmclose = XmInternAtom( XtDisplay(xx->dialog),
			    "WM_DELETE_WINDOW", False);
     XmAddWMProtocolCallback(xx->dialog, wmclose, xxyz_quit_cb,
			     (caddr_t)&(vi->xyz));

     xyzDrawImage(NULL);

     imod_info_input();
     XtPopup(xx->dialog, XtGrabNone);

#ifdef DRAW_GL
     glxInstallColormapWithOverlay(xx->dialog, xx->glw);
#endif
     
     return(0);
}

static void xxyz_quit_cb(Widget w, XtPointer client, XtPointer call)
{
     struct xxyzwin **pxx = (struct xxyzwin **)client;
     struct xxyzwin *xx;
     
     xx = *pxx;

     /* DNM 11/17/01: stop x and y movies when close window */
     imodMovieXYZT(xx->vi, 0, 0, MOVIE_DEFAULT, MOVIE_DEFAULT);

     if(xx->fdata)
	  free(xx->fdata);

     b3dFreeCIImage(xx->xydata);
     b3dFreeCIImage(xx->xzdata);
     b3dFreeCIImage(xx->yzdata);

     b3dWinset(XtDisplay(w), 0, 0);

     XtPopdown(xx->dialog);
     XtDestroyWidget(xx->dialog);
     free(*pxx);
     *pxx = NULL;
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

#ifdef DRAW_GL

     int i,j,k;
     long xzoffset;
     Colorindex *xz;
     Colorindex *yz;


     xx->overclear = TRUE;
     GLXwinset(XtDisplay(w), glxOverlayWindow(w));
     mapcolor(1, 255,   255,   255); 
     mapcolor(2, 255,   255, 0);
     mapcolor(3,   0, 255,   0);
     mapcolor(4,   0,   0, 255);
     mapcolor(5, 128, 128,   0);
     mapcolor(6,   0,   0,   0);
     mapcolor(7, 255, 255, 255);

/*
     for (i = 0; i < xx->vi->xysize; i++){
	  xx->xydata[i] = xx->vi->idata[0][i] + xx->vi->rampbase;
     }

     for (i = 0; i < xx->vi->xysize; i++){
	  xx->xydata[i] = xx->vi->idata[xx->vi->zmouse][i] + xx->vi->rampbase;
     }
     
     xzoffset = xx->vi->ymouse * xx->vi->xsize;
     xz = xx->xzdata;
     yz = xx->yzdata;
     for (k = 0; k < xx->vi->zsize; k++)
	  for (i = 0; i < xx->vi->xsize; i++, xz++)
	       *xz = xx->vi->idata[k][i + xzoffset] + xx->vi->rampbase;
     
     for (j = 0; j < xx->vi->ysize; j++)
	  for (k = 0; k < xx->vi->zsize; k++,yz++)
	       *yz = xx->vi->idata[k][xx->vi->xmouse + (j * xx->vi->xsize)] + 
		    xx->vi->rampbase;
*/
#endif

     xx->context =  b3dGetContext(w);
     b3dWinset(XtDisplay(w), w, xx->context);
     b3dResizeViewport();

     xx->xydata = (B3dCIImage *)b3dGetNewCIImageSize
	  (NULL, App->depth, xx->vi->xsize, xx->vi->ysize);
     xx->xzdata = (B3dCIImage *)b3dGetNewCIImageSize
	  (NULL, App->depth, xx->vi->xsize, xx->vi->zsize);
     xx->yzdata = (B3dCIImage *)b3dGetNewCIImageSize
	  (NULL, App->depth, xx->vi->zsize, xx->vi->ysize);
     return;
}

#ifdef DRAW_GL
static void xxyz_overex_cb(Widget w, XtPointer client, XtPointer call)
{
     struct xxyzwin *xx = (struct xxyzwin *)client;
     GLXwinset(App->display, glxOverlayWindow(w));
     color(0);
     clear();
     gflush();     
     xx->overclear = TRUE;
}
#endif


static void xxyz_input_cb(Widget w, XtPointer client, XtPointer call)
{
     struct xxyzwin *xx = (struct xxyzwin *)client;
     struct ViewInfo *vi = xx->vi;
     B3dDrawingAreaCallbackStruct *cbs = (B3dDrawingAreaCallbackStruct *)call;

     int button1 = False, button2 = False, button3 = False;
     KeySym keysym;

#ifdef DRAW_GL
     glxDummyWinset();
#endif

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
	       xxyzButton1(xx, cbs->event->xbutton.x, cbs->event->xbutton.y);
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
	  break;

	case ButtonRelease:
	  break;

	case MotionNotify:
	  if (cbs->event->xmotion.state & Button1Mask)
	       button1 = True;
	  if (cbs->event->xmotion.state & Button2Mask)
	       button2 = True;
	  if (cbs->event->xmotion.state & Button3Mask)
	       button3 = True;
	  if ( (button1) && (!button2) && (!button3))
	       xxyzB1Drag(xx, cbs->event->xmotion.x, cbs->event->xmotion.y);
	  if ( (!button1) && (button2) && (!button3))
	       xxyzB2Drag(xx, cbs->event->xmotion.x, cbs->event->xmotion.y);
	  if ( (!button1) && (!button2) && (button3))
	       xxyzB3Drag(xx, cbs->event->xmotion.x, cbs->event->xmotion.y);
	  break;

	default:
	  break;
     }
}


static int xxyz_getxyz(struct xxyzwin *xx, 
		       int x, int y, int *mx, int *my, int *mz)
{
     int nx, ny, nz;
     float b2 = BSIZE;
     float scale;
     struct ViewInfo *vi = xx->vi;

     y = xx->winy - y - 1;
     x -= BSIZE;
     y -= BSIZE;

     nx = vi->xsize;
     ny = vi->ysize;
     nz = vi->zsize;
     scale = 1.0f;

#ifdef DRAW_OpenGL
     nx *= xx->zoom;
     ny *= xx->zoom;
     nz *= xx->zoom;
     scale = 1.0/xx->zoom;

#endif

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
	  *mz = (y - b2 - vi->ysize) * scale;
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
     struct Mod_Contour *cont;
     int pt;

     if (!xxyz_getxyz(xx, x, y, &mx, &my, &mz))
	  return;

     if (xx->vi->ax){
	  if (xx->vi->ax->altmouse == AUTOX_ALTMOUSE_PAINT){
	       autox_fillmouse(xx->vi, mx, my);
	       return;
	  }
     }

     if (xx->vi->imod->mousemode == IMOD_MMOVIE){
	  xx->vi->xmouse = mx;
	  xx->vi->ymouse = my;
	  xx->vi->zmouse = mz;
	  ivwBindMouse(xx->vi);
	  xxyz_draw(xx);
	  imod_info_setxyz();
	  return;
     }

     imod_nearest(xx->vi->imod);
     cont = imodContourGet(xx->vi->imod);
     pt = xx->vi->imod->cindex.point;

     if ((cont) && (pt >= 0)){
	  xx->vi->xmouse = cont->pts[pt].x + 0.5;
	  xx->vi->ymouse = cont->pts[pt].y + 0.5;
	  xx->vi->zmouse = cont->pts[pt].z + 0.5;
     }else{
	  xx->vi->xmouse = mx;
	  xx->vi->ymouse = my;
	  xx->vi->zmouse = mz;
     }
     ivwBindMouse(xx->vi);
     imodDraw(xx->vi, IMOD_DRAW_IMAGE | IMOD_DRAW_XYZ | IMOD_DRAW_MOD);
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
     xxyz_draw(xx);
     imod_draw_window();
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
     xxyz_draw(xx);
     imod_draw_window();
     return;
}

static void xxyzB1Drag(struct xxyzwin *xx, int x, int y)
{
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
     xxyz_draw(xx);
     imod_draw_window();
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
     xxyz_draw(xx);
     imod_draw_window();
     return;
}


static void xyz_overclear(struct xxyzwin *xx)
{
#ifdef DRAW_GL
     if (!xx->overclear){
	  xx->overclear = TRUE;
	  GLXwinset(App->display, glxOverlayWindow(xx->glw));
	  color(0);
	  clear();
     }
#endif
     return;
}

static void xyzDrawImage(struct xxyzwin *win)
{

     unsigned int x, y, z, i;
     unsigned int nx, ny, nz;
     unsigned char *id;
     unsigned char *fdata;
     unsigned long cyi;
     int cx, cy, cz, iz;
     int imdataxsize;
     unsigned char **imdata;

	  
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

     fdata  = win->fdata;
     
     if (win->vi->vmSize){
	  win->lx = win->ly = -1;
     }
	  
     ivwGetLocation(win->vi, &cx, &cy, &cz);
     nx = win->vi->xsize;
     ny = win->vi->ysize;
     nz = win->vi->zsize;
     cyi = cy * nx;

     if (cz != win->lz){
	  win->lz = cz;
	  id = ivwGetCurrentZSection(win->vi);
	  if (id)
	       b3dFillGreyScalePixels (id, nx,ny, win->xydata,
				       win->vi->rampbase);
     }
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
	  b3dFillGreyScalePixels(win->fdata, nz, ny, win->yzdata,
				 win->vi->rampbase);
     }
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
	  
	  b3dFillGreyScalePixels(win->fdata, nx, nz, win->xzdata,
				 win->vi->rampbase);
     }

     free(imdata);
     
#ifdef DRAW_OpenGL
     {
	  GLenum type, format;
	  GLint  unpack = b3dGetImageType(&type, &format);
	  int sx = nx * win->zoom;
	  int sy = ny * win->zoom;
	  int sz = nz * win->zoom;
	  
	  glClearIndex(App->background);
	  /* DNM: need to set clear colors for rgb mode */
	  if (App->rgba)
	       glClearColor(64./255., 64./255 , 96./255, 0.);

	  /* Another hack for the PC: need to clear the top of the window
	     after the draws to clear garbage that shows up at top for an
	     image of 572x50x94 and windows bigger than a certain size.
	     So just divide clear into two parts, lower before, upper after */
#ifdef XYZ_CLEAR_HACK
	  b3dColorIndex(App->background);
	  b3dDrawFilledRectangle(0,0, win->winx, BSIZE + BSIZE + sy +sz);
#else
	  glClear(GL_COLOR_BUFFER_BIT);
#endif	  

	  /*	  glPixelStorei( GL_UNPACK_ALIGNMENT, 2); */
	  glPixelZoom(win->zoom, win->zoom);
	  
	  glRasterPos2i(BSIZE, BSIZE);
	  glDrawPixels(nx, ny, 
		       format, type,
		       win->xydata->id1);
	  glRasterPos2i(BSIZE, BSIZE+BSIZE+sy);
	  glDrawPixels(nx, nz, 
		       format, type,
		       win->xzdata->id1);
	  glRasterPos2i(BSIZE+BSIZE+sx, BSIZE);
	  glDrawPixels(nz, ny, 
		       format, type,
		       win->yzdata->id1);

#ifdef XYZ_CLEAR_HACK
	  b3dDrawFilledRectangle(0,BSIZE + BSIZE + sy +sz, win->winx,
				 win->winy);
#endif
     }
#else
     b3dPutCIImage(win->xydata,0,0,BSIZE,BSIZE,nx,ny);
     b3dPutCIImage(win->xzdata,0,0,BSIZE,BSIZE+BSIZE+ny,nx,nz);
     b3dPutCIImage(win->yzdata,0,0,BSIZE+BSIZE+nx,BSIZE,nz,ny);
#endif
     return;
}

static void xyzDrawCurrentLines(struct xxyzwin *xx)
{
     int cx, cy, cz;
     int b  = BSIZE;
     int b2 = BSIZE * 2;
     int ob = BSIZE;
     int nx = xx->vi->xsize;
     int ny = xx->vi->ysize;
     int nz = xx->vi->zsize;
     int wx = xx->winx;
     int wy = xx->winy;

     b3dColorIndex(App->background);
     b3dLineWidth(1);

#ifndef DRAW_OpenGL
     b3dDrawFilledRectangle(0, 0, b, wy);
     b3dDrawFilledRectangle(0, 0, wx, b);
     b3dDrawFilledRectangle(b+nx+b+nz, 0, wx-b+nx+b+nz, wy);
     b3dDrawFilledRectangle(0, b+ny+b+nz, wx, wy-b+ny+b+nz);
     
     b3dDrawFilledRectangle(0, b+ny, wx, b);
     b3dDrawFilledRectangle(b+nx, 0, b, wy);
     b3dDrawFilledRectangle(b+nx, b+ny, wx-b+nx, wy-b+nx);
#else
     ob = (float)ob / xx->zoom;
     b2 = b + ob;
#endif
     ivwGetLocation(xx->vi, &cx, &cy, &cz);
     b3dColorIndex(App->foreground);

     /* draw xz location line. */
     b3dDrawLine(b2 + nx, 
		 b2 + ny + cz,
		 b2 + nx + nz - 1,
		 b2 + ny + cz);

     b3dDrawLine(b2 + nx + cz,
		 b2 + ny,
		 b2 + nx + cz,
		 b2 + ny + nz - 1);
     
     b3dDrawLine(b + cx,
		 b + ny + 1,
		 b2 + nx + nz,
		 b + ny + 1);
     b3dDrawLine(b + nx + 1,
		 b2 + ny + nz,
		 b + nx + 1,
		 b + cy);
     return;
}

static void xyzDrawGhost(struct xxyzwin *xx)
{
     return;
}

static void xyzDrawContour(struct xxyzwin *xx, Iobj *obj, Icont *cont)
{
     ImodView *vi = xx->vi;
     Ipoint *point;
     int pt, npt = 0, ptsonsec;
     float vert[2];
     int co, ob;
     float drawsize;
     int b = BSIZE;
     float gutter = BSIZE;

#ifdef DRAW_OpenGL
     gutter = gutter / xx->zoom;
#endif
     
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
	       b3dVertex2i( cont->pts[pt].x + b,  cont->pts[pt].y + b);
	  }
	  if (ivwPointVisible(vi, cont->pts)){
	       b3dVertex2i(b + cont->pts->x, b + cont->pts->y);
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
	       zapDrawSymbol(cont->pts[pt].x + b, cont->pts[pt].y + b,
			     obj->symbol, obj->symsize, obj->symflags);
	  }
     }
     
     /* Open contours: draw symbols for all points on section and connecting
	lines just between point pairs on section */
     if (iobjOpen(obj->flags)){
	  for(pt = 0; pt < cont->psize; pt++){
	       point = &(cont->pts[pt]);
	       if (ivwPointVisible(vi, point)){
		    zapDrawSymbol(cont->pts[pt].x + b, 
				  cont->pts[pt].y + b,
				  obj->symbol, obj->symsize, obj->symflags);
		    
		    if (pt < (cont->psize - 1))
			 if (ivwPointVisible(vi, &(cont->pts[pt+1])))
			      b3dDrawLine(point->x + b,
					  point->y + b,
					  cont->pts[pt+1].x + b,
					  cont->pts[pt+1].y + b);
		    
	       }
	       
	  }	       
     }
     
     /* scattered contour */
     if (iobjScat(obj->flags)){
	  for (pt = 0; pt < cont->psize; pt++){
	       /* draw symbol if point on section */
	       if (ivwPointVisible(vi, &(cont->pts[pt])))
		    zapDrawSymbol(cont->pts[pt].x + b, 
				  cont->pts[pt].y + b,
				  obj->symbol, obj->symsize, obj->symflags);
	       
	       drawsize = imodPointGetSize(obj, cont, pt);
	       if (drawsize > 0)
		    if (ivwPointVisible(vi, &(cont->pts[pt]))){
			 /* If there's a size, draw a circle and a plus for a
			    circle that's big enough */
			 b3dDrawCircle(b + cont->pts[pt].x,
				       b + cont->pts[pt].y,
				       (int)drawsize);
			 if (drawsize > 3)
			      b3dDrawPlus(b + cont->pts[pt].x,
					  b + cont->pts[pt].y, 3);
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
				     sqrt((double)
					  (drawsize * drawsize
					  - vert[0] * vert[0]));
				   
				   b3dDrawCircle((b +  cont->pts[pt].x),
						 (b + cont->pts[pt].y),
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
	       b3dDrawCross(b + cont->pts[cont->psize-1].x,
			    b + cont->pts[cont->psize-1].y,
			    obj->symsize/2);
	  }
	  if (ivwPointVisible(vi, cont->pts)){
	       b3dColorIndex(App->bgnpoint);
	       b3dDrawCross(b + cont->pts->x,
			    b + cont->pts->y,
			    obj->symsize/2);
	  }
	  b3dColorIndex(obj->fgcolor);
     }
     return;
}

static void xyzDrawCurrentContour(struct xxyzwin *xx, Iobj *obj, Icont *cont)
{
     ImodView *vi = xx->vi;
     Ipoint *point;
     int pt;
     int cz = vi->zmouse+0.5f;
     int b = BSIZE;
     float gutter = BSIZE;

#ifdef DRAW_OpenGL
     gutter = gutter / xx->zoom;
#endif
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
	       b3dVertex2i( cont->pts[pt].x + b,  cont->pts[pt].y + b);
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
		    zapDrawSymbol(cont->pts[pt].x + b, 
				  cont->pts[pt].y + b,
				  obj->symbol, obj->symsize, obj->symflags);
	       }

	  /* Draw projection of all lines into x/z view */
	  b3dBeginLine();
	  for (pt = 0; pt < cont->psize; pt++){
	       b3dVertex2i( cont->pts[pt].x + b,  
			   cont->pts[pt].z + b + gutter + vi->ysize);
	  }
	  b3dEndLine();


	  /* Draw projection of all lines into y/z view */
	  b3dBeginLine();
	  for (pt = 0; pt < cont->psize; pt++){
	       b3dVertex2i(cont->pts[pt].z + b + gutter + vi->xsize, 
			   cont->pts[pt].y + b);
	  }
	  b3dEndLine();
     }

     if (iobjOpen(obj->flags)){
	  /* Open objects: try to draw solid lines that are on-section and
	     dashed lines that are not */
	  if (cont->psize > 1){
	       for(pt = 1; pt < cont->psize; pt++){
		    
		    if ((cont->pts[pt].z == cz) && (cont->pts[pt-1].z == cz)){
			 b3dDrawLine((b +cont->pts[pt].x),
				     (b + cont->pts[pt].y),
				     (b +cont->pts[pt-1].x),
				     (b + cont->pts[pt-1].y));
			 continue;
		    }
		    b3dLineStyle(B3D_LINESTYLE_DASH);
		    b3dDrawLine((b +cont->pts[pt].x),
				(b + cont->pts[pt].y),
				(b +cont->pts[pt-1].x),
				(b + cont->pts[pt-1].y));
		    b3dLineStyle(B3D_LINESTYLE_SOLID);
		    
		    /* Draw a circle on an isolated point on-section */
		    if (pt < cont->psize - 1)
			 if ((cont->pts[pt].z == cz) &&
			     (cont->pts[pt-1].z != cz) &&
			     (cont->pts[pt+1].z != cz))
			      b3dDrawCircle((b + cont->pts[pt].x),
					    (b + cont->pts[pt].y), 3);
	       }
	       /* Draw circles on first or last points if they are isolated
		  on-section */
	       if ((cont->pts[0].z == cz) && (cont->pts[1].z != cz))
		    b3dDrawCircle((b + cont->pts[0].x),
				  (b + cont->pts[0].y), 3);
	       if ((cont->pts[cont->psize - 1].z == cz) &&
		   (cont->pts[cont->psize - 2].z != cz))
		    b3dDrawCircle
			 ((b + cont->pts[cont->psize - 1].x),
			  (b + cont->pts[cont->psize - 1].y), 3);

	       /* draw symbols for points on section */
	       if (obj->symbol != IOBJ_SYM_NONE)
		    for (pt = 0; pt < cont->psize; pt++){
			 if (!ivwPointVisible(vi, &(cont->pts[pt]))) {
			      if (cont->flags & ICONT_WILD)
				   continue;
			      break;
			 }
			 zapDrawSymbol(cont->pts[pt].x + b, 
				       cont->pts[pt].y + b,
				       obj->symbol, obj->symsize,
				       obj->symflags);
		    }

	  }else{

	       /* Draw a circle for a single point, regardless of z */
	       b3dDrawCircle((b + cont->pts[0].x),
			     (b + cont->pts[0].y), 3);
	  }

	  /* Draw projection of all lines into x/z view */
	  b3dBeginLine();
	  for (pt = 0; pt < cont->psize; pt++){
	       b3dVertex2i( cont->pts[pt].x + b,  
			   cont->pts[pt].z + b + gutter + vi->ysize);
	  }
	  b3dEndLine();


	  /* Draw projection of all lines into y/z view */
	  b3dBeginLine();
	  for (pt = 0; pt < cont->psize; pt++){
	       b3dVertex2i(cont->pts[pt].z + b + gutter + vi->xsize, 
			   cont->pts[pt].y + b);
	  }
	  b3dEndLine();

	  return;
     }

     /* scatterd object */
     if (iobjScat(obj->flags)){
	  /* Just draw the contour with other routines */
	  xyzDrawContour(xx, obj, cont);
	  
	  /* Draw points in other windows if that is where they are */
	  for (pt = 0; pt < cont->psize; pt++){
	       if ((int)cont->pts[pt].x == vi->xmouse){
		    zapDrawSymbol(cont->pts[pt].z + b + gutter +
				  vi->xsize, 
				  cont->pts[pt].y + b,
				  obj->symbol, obj->symsize, obj->symflags);
	       }
	       if ((int)cont->pts[pt].y == vi->ymouse){
		    zapDrawSymbol(cont->pts[pt].x + b, 
				  cont->pts[pt].z + b + gutter +
				  vi->ysize,
				  obj->symbol, obj->symsize, obj->symflags);
	       }
	  }
     }

     /* draw end symbols in x/y views */
     if (obj->symflags & IOBJ_SYMF_ENDS){
	  if (ivwPointVisible(vi, &(cont->pts[cont->psize-1]))){
	       b3dColorIndex(App->endpoint);
	       b3dDrawCross(b + cont->pts[cont->psize-1].x,
			    b + cont->pts[cont->psize-1].y,
			    obj->symsize/2);
	  }
	  if (ivwPointVisible(vi, cont->pts)){
	       b3dColorIndex(App->bgnpoint);
	       b3dDrawCross(b + cont->pts->x,
			    b + cont->pts->y,
			    obj->symsize/2);
	  }
	  b3dColorIndex(obj->fgcolor);
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
		    xyzDrawCurrentContour(xx, obj, cont);
	       else
		    xyzDrawContour(xx, obj, cont);
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
     int b = BSIZE;
     float ob = BSIZE;

#ifdef DRAW_OpenGL
     ob = (float)ob / xx->zoom;
#endif

     ivwGetLocation(xx->vi, &cx, &cy, &cz);

     if (!xx->vi->drawcursor)
	  return;

     /* Draw begin and end points of selected contour. */
     if (cont){
	  if (cont->psize > 1){
	       if ((int)cont->pts->z == cz){
		    b3dColorIndex(App->bgnpoint);
		    b3dDrawCircle(cont->pts->x+b,
				  cont->pts->y+b, 2);
	       }
	       if ((int)cont->pts[cont->psize - 1].z == cz){
		    b3dColorIndex(App->endpoint);
		    b3dDrawCircle(cont->pts[cont->psize - 1].x+b,
				  cont->pts[cont->psize - 1].y+b,  2);
	       }
	  }
     }
     
     /* Draw location of current point. */
     if (pnt){
	  
	  if ((int)(pnt->z) == cz){
	       b3dColorIndex(App->foreground);
	  }else{
	       b3dColorIndex(App->shadow);
	  }
	  b3dDrawCircle(cx+b, cy+b, psize);
	  b3dColorIndex(App->foreground);
	  b3dDrawPlus(cx+b, b+xx->vi->ysize+ob+cz, 1);
	  b3dDrawPlus(b+xx->vi->xsize+ob+cz, 
		      b+cy, 1);
	  return;
     }
     b3dColorIndex(App->foreground);
     b3dDrawPlus(cx+b, cy+b, 1);
     b3dDrawPlus(cx+b, b+xx->vi->ysize+ob+cz, 1);
     b3dDrawPlus(b+xx->vi->xsize+ob+cz, 
		 b+cy, 1);
     
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
	  y = j + BSIZE;
	  for(i = 0; i < vi->xsize; i++){
	       x = i + BSIZE;
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

static void xxyz_draw(struct xxyzwin *xx)
{
     int b = BSIZE;
     if (!xx)
	  return;
     if (!xx->exposed) return;     /* DNM: avoid crashes if Zap is movieing*/

     xxyz_overclear(xx);

     b3dWinset(App->display, xx->glw, xx->context);

     xyzDrawImage(xx);

#ifdef DRAW_OpenGL
     glPushMatrix();
     glTranslatef(BSIZE, BSIZE, 0);
     glScalef(xx->zoom, xx->zoom, 1.0);
     glTranslatef(-BSIZE, -BSIZE, 0);
#endif

     xyzDrawModel(xx);
     xyzDrawCurrentLines(xx);
     xyzDrawCurrentPoint(xx);
     xyzDrawAuto(xx);

#ifdef DRAW_OpenGL	     
     glPopMatrix();
     if (xyzShowSlice){
	  int xo = (2*b) + (xx->vi->xsize * xx->zoom);
	  int yo = (2*b) + (xx->vi->ysize * xx->zoom);
	  b3dColorIndex(App->foreground);
	  
	  b3dDrawLine(b + (xx->vi->slice.zx1 * xx->zoom), 
		      b + (xx->vi->slice.zy1 * xx->zoom),
		      b + (xx->vi->slice.zx2 * xx->zoom), 
		      b + (xx->vi->slice.zy2 * xx->zoom));

	  b3dDrawLine(b + (xx->vi->slice.yx1 * xx->zoom),
		      yo+ (xx->vi->slice.yz1 * xx->zoom),
		      b + (xx->vi->slice.yx2 * xx->zoom),
		      yo+ (xx->vi->slice.yz2 * xx->zoom));

	  b3dDrawLine(xo+ (xx->vi->slice.xz1 * xx->zoom),
		      b + (xx->vi->slice.xy1 * xx->zoom),
		      xo+ (xx->vi->slice.xz2 * xx->zoom),
		      b + (xx->vi->slice.xy2 * xx->zoom));

	  xyzShowSlice = 0;
     }
     b3dFlush();

#else
     xyzDrawShowSlice(xx);
#endif

     b3dSwapBuffers();
     return;
}



void xyzDraw_cb(ImodView *vi, void *client, long drawflag)
{
     struct xxyzwin *xx = (struct xxyzwin *)client;

     if ((!vi) || (!xx) || (!drawflag)) return;
     
     if (drawflag){
	  if (drawflag & IMOD_DRAW_IMAGE){
	       xx->lx = xx->ly = xx->lz = -1;
	       b3dFreeCIImage(xx->xydata);
	       b3dFreeCIImage(xx->xzdata);
	       b3dFreeCIImage(xx->yzdata);
	       xx->xydata = (B3dCIImage *)b3dGetNewCIImageSize
		    (NULL, App->depth, xx->vi->xsize, xx->vi->ysize);
	       xx->xzdata = (B3dCIImage *)b3dGetNewCIImageSize
		    (NULL, App->depth, xx->vi->xsize, xx->vi->zsize);
	       xx->yzdata = (B3dCIImage *)b3dGetNewCIImageSize
		    (NULL, App->depth, xx->vi->zsize, xx->vi->ysize);
	  }
	  if (drawflag & IMOD_DRAW_SLICE)
	       xyzShowSlice = 1;
	  xyz_draw(vi);
     }
     return;
}

static void xyzDrawShowSlice(struct xxyzwin *xx)
{
#ifndef DRAW_GL
     xyzShowSlice = 1;
     xyz_draw(xx->vi);
#endif
     return;
}

int xyz_draw_showslice(struct ViewInfo *vi)
{
#ifdef DRAW_GL
     float vert[2];

/* Just draw xyz only, zap to buggy. */
/*     imod_zap_showslice(vi); */

     if (!vi->xyz)
	  return(-1);

     GLXwinset(App->display,
	       glxOverlayWindow(vi->xyz->glw));
     color(0);
     clear();
     vi->xyz->overclear = FALSE;
     color(1);

     if ((vi->slice.zx1 != vi->lslice.zx1) ||
	 (vi->slice.zx2 != vi->lslice.zx2) ||
	 (vi->slice.zy1 != vi->lslice.zy1) ||
	 (vi->slice.zy2 != vi->lslice.zy2) ){
	  
	  vert[0] = vi->slice.zx1 + BSIZE;
	  vert[1] = vi->slice.zy1 + BSIZE;
	  bgnline();
	  v2f(vert);
	  vert[0] = vi->slice.zx2 + BSIZE;
	  vert[1] = vi->slice.zy2 + BSIZE;
	  v2f(vert);
	  endline();
	  vi->slice.zx1 = vi->lslice.zx1;
	  vi->slice.zx2 = vi->lslice.zx2;
	  vi->slice.zy1 = vi->lslice.zy1;
	  vi->slice.zy2 = vi->lslice.zy2;
     }
     
     if ((vi->slice.yx1 != vi->lslice.yx1) ||
	 (vi->slice.yx2 != vi->lslice.yx2) ||
	 (vi->slice.yz1 != vi->lslice.yz1) ||
	 (vi->slice.yz2 != vi->lslice.yz2)){
	  
	  vert[0] = BSIZE + vi->slice.yx1;
	  vert[1] = (BSIZE * 2) + vi->ysize + vi->slice.yz1;
	  bgnline();
	  v2f(vert);
	  vert[0] = BSIZE + vi->slice.yx2;
	  vert[1] = (BSIZE * 2) + vi->ysize + vi->slice.yz2;
	  v2f(vert);
	  endline();
	  vi->slice.yx1 = vi->lslice.yx1;
	  vi->slice.yx2 = vi->lslice.yx2;
	  vi->slice.yz1 = vi->lslice.yz1;
	  vi->slice.yz2 = vi->lslice.yz2;
     }

     if ((vi->slice.xy1 != vi->lslice.xy1) ||
	 (vi->slice.xy2 != vi->lslice.xy2) ||
	 (vi->slice.xz1 != vi->lslice.xz1) ||
	 (vi->slice.xz2 != vi->lslice.xz2)){
	  
	  vert[0] = (BSIZE * 2) + vi->xsize + vi->slice.xz1;
	  vert[1] = BSIZE + vi->slice.xy1;
	  bgnline();
	  v2f(vert);
	  vert[0] = (BSIZE * 2) + vi->xsize + vi->slice.xz2;
	  vert[1] = BSIZE + vi->slice.xy2;
	  v2f(vert);
	  endline();
	  vi->slice.xy1 = vi->lslice.xy1;
	  vi->slice.xy2 = vi->lslice.xy2;
	  vi->slice.xz1 = vi->lslice.xz1;
	  vi->slice.xz2 = vi->lslice.xz2;
     }
     gflush();
#else
     if (!vi->xyz)
	  return(-1);
     xyzShowSlice = 1;
     xyz_draw(vi);
#endif
     return(0);
}
