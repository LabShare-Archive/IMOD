/*  IMOD VERSION 2.50
 *
 *  imodv_gfx.c -- Drawing functions for imodv.
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
    Revision 3.1  2002/06/20 00:26:58  mast
    Force GLw to use the already chosen visual when getting a drawing area

*/

#include <stdlib.h>
#include <math.h>
#include <limits.h>

#include <Xm/Xm.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>

#include <X11/keysym.h>
#include <Xm/VirtKeys.h>
#include <X11/Xutil.h>
#ifdef __vms
#include <Xmu/StdCmap.h>
#else
#include <X11/Xmu/StdCmap.h>
#endif
#include <Xm/DrawingA.h>
#include <mrcc.h>
#include "imod.h"
#include "imodv.h"


void imodv_ginit(Widget w, XtPointer client, XtPointer call);
void imodv_ginit_rgbsb_cb(Widget w, XtPointer client, XtPointer call);
void imodvDraw_models(ImodvApp *a);
void stereoClear(GLbitfield mask);
void stereoDrawBuffer(GLenum mode);
void imodvSetModelTrans(Imod *imod);

/* overide Motif key bindings. */
String ImodvTranslations =
"<KeyDown>: glwInput()\n\
 <BtnDown>: glwInput()\n\
 <BtnMotion>: glwInput()\n\
 <Btn2Up>: glwInput()\n\
";



/* DNM: got this working - although it makes no noticeable difference - see
   note below.*/
int DisplayHasAlpha(Display  *display, int doublebuffer)
{
    XVisualInfo *visualinfo;
    int         OpenGLAttrib[10];
    int         screen_num;
    int         n = 0;


    OpenGLAttrib[n++] = GLX_RGBA;
    if (doublebuffer)
         OpenGLAttrib[n++] = GLX_DOUBLEBUFFER;
    OpenGLAttrib[n++] = GLX_ALPHA_SIZE;
    OpenGLAttrib[n++] = 1;
    OpenGLAttrib[n] = None;

    screen_num  = DefaultScreen(display);
    visualinfo = glXChooseVisual(display, screen_num, OpenGLAttrib);
    
    if (visualinfo){
	 /* fprintf(stderr, "Imodv: using alpha planes db=%d.\n", doublebuffer); */
	 return(1);
    }else {
	 /*  puts("no alpha");  */
	 return(0);
    }

}


/*
 *  Initialize the OpenGL rendering windows.
 *  DNM: to use alpha planes, uncomment the various lines about alpha below.
 *  There is no point in using alpha planes because we do not sort the display
 *  in order by Z before drawing, which would be needed to use alpha properly
 *  for polygon antialiasing.
 */
int imodv_init_drawing_widget(ImodvApp *a, Widget form)
{
     Dimension width, height;
     int altmp = a->alpha;
     XtVaGetValues(form, XmNwidth, &width, XmNheight, &height, NULL);

     a->gc = a->dgc = 0;
     a->first = a->dfirst = 1;
     
     if (a->cindex){
	  a->gfx = XtVaCreateManagedWidget
	       ("rgbwidget", B3dDrawingAreaWidgetClass, form,
		GLwNvisualInfo, Imodv->visualInfo,
		XmNnavigationType, XmNONE,
		XmNtraversalOn, True,
		XmNwidth, width,
		XmNheight, height,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtranslations,
		XtParseTranslationTable(ImodvTranslations),
		XmNcolormap, Imodv->cmap,
		NULL);
	  
	  imodOverrideTransTable(a->gfx,ImodvTranslations);
	  XtAddCallback(a->gfx, GLwNresizeCallback,
			imodv_resize_cb, (XtPointer)a);
	  XtAddCallback(a->gfx, GLwNexposeCallback,
			imodv_expose_cb, (XtPointer)a);
	  XtAddCallback(a->gfx, GLwNinputCallback,
			imodv_input_cb, (XtPointer)a);
	  if (!a->gfx)
	       return(-1);
	  a->cgfx = a->dgfx = a->gfx;

     }else{
	  /* Double buffered widget */
	  /* altmp = DisplayHasAlpha(a->display, 1) */
	  a->dgfx = XtVaCreateManagedWidget
	       ("drgbwidget", B3dDrawingAreaWidgetClass, form,
		GLwNrgba, True, 
		GLwNdoublebuffer, True, 
		GLwNdepthSize, 8,
		GLwNredSize, 1,
		GLwNgreenSize, 1,
		GLwNblueSize, 1,
		GLwNalphaSize, altmp, 
		/*GLwNstereo, True,*/
		XmNnavigationType, XmNONE,
		XmNtraversalOn, True,
		XmNwidth, width,
		XmNheight, height,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtranslations, XtParseTranslationTable(ImodvTranslations),
		NULL);
	  XtManageChild(a->dgfx);
	  
	  imodOverrideTransTable(a->dgfx, ImodvTranslations);
	  XtAddCallback(a->dgfx, GLwNresizeCallback,
			imodv_resize_cb, (XtPointer)a);
	  XtAddCallback(a->dgfx, GLwNexposeCallback,
			imodv_expose_cb, (XtPointer)a);
	  XtAddCallback(a->dgfx, GLwNinputCallback,
			imodv_input_cb, (XtPointer)a);
	  XtSetMappedWhenManaged(a->dgfx, True);
	  a->dgfx_init = 0;
	  if (!a->dgfx){
	      a->db = False;
	      fprintf(stderr, "Imodv warning: double buffer init failed.\n");
	  }
	  /* Single buffered widget */

	  /* altmp = DisplayHasAlpha(a->display, 0) */
	  a->gfx = XtVaCreateManagedWidget
	       ("rgbwidget", B3dDrawingAreaWidgetClass, form,
		GLwNrgba, True,
		GLwNdoublebuffer, False,
		GLwNdepthSize, 8,
		GLwNgreenSize, 1,
		GLwNredSize, 1,
		GLwNblueSize, 1,
		GLwNalphaSize, altmp, 
		XmNnavigationType, XmNONE,
		XmNtraversalOn, True,
		XmNwidth, width,
		XmNheight, height,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNtranslations, XtParseTranslationTable(ImodvTranslations),
		NULL);

	  if (!a->gfx){
	      fprintf(stderr, "Imodv warning: single buffer init failed.\n");
	  }

	  imodOverrideTransTable(a->gfx, ImodvTranslations);
	  XtAddCallback(a->gfx, GLwNresizeCallback,
			imodv_resize_cb, (XtPointer)a);
	  XtAddCallback(a->gfx, GLwNexposeCallback,
			imodv_expose_cb, (XtPointer)a);
	  XtAddCallback(a->gfx, GLwNginitCallback,
			imodv_ginit_rgbsb_cb, (XtPointer)a);
	  XtAddCallback(a->gfx, GLwNinputCallback,
			imodv_input_cb, (XtPointer)a);
	  
	  XtSetMappedWhenManaged(a->gfx, True);
	  
/*	  a->db = False;*/

	  if (a->db){
	       XtManageChild(a->dgfx);
	       a->cgfx = a->dgfx;
	  }else{
	       XtManageChild(a->gfx);
	       a->cgfx = a->gfx;
	  }
	  /* DNM: set alpha based on the current db flag */
	  /*  a->alpha = DisplayHasAlpha(a->display, a->db); */
     }
     return(0);
}

/*
 *  Set the current OpenGL rendering context to the
 *  window being used by Imodv.
 */
int imodv_winset(ImodvApp *a)
{

     if (a->cindex){
	  a->cgfx = a->gfx;
	  if ((XtWindow(a->gfx)) && (a->gc)){
	       glXMakeCurrent(XtDisplay(a->gfx),
			      XtWindow(a->gfx), a->gc);
	   }
	  
     }else{
	  if (a->db){
	      if (!a->dgfx_init)
		  return(0);
	       a->cgfx = a->dgfx;
	       if ((XtWindow(a->dgfx)) && (a->dgc)){
		    glXMakeCurrent(XtDisplay(a->dgfx), 
				   XtWindow(a->dgfx), a->dgc);
	       }else{
		    return(0);
	       }
	  }else{
	       a->cgfx = a->gfx;
	       if ((XtWindow(a->gfx)) && (a->gc)){
		    glXMakeCurrent(XtDisplay(a->gfx), 
				   XtWindow(a->gfx), a->gc);
		} else { 
		    return(0);
		}
	  }
     }
     return(1);
}

/*
 *  imodv version of the IrisGL swapbuffers command.
 */
void imodv_swapbuffers(ImodvApp *a)
{
     if (a->doPick) return;
     if (a->db)
	  GLwDrawingAreaSwapBuffers(a->dgfx);
     glFlush();
}
	  

void imodv_clear(ImodvApp *a)
{
     if (a->cindex){
	  imodvMapColor(a->gfx, a->bindex,
			(a->rbgcolor.red   & 0xf000) / 256,
			(a->rbgcolor.green & 0xf000) / 65535.0,
			(a->rbgcolor.blue  & 0xf000) / 65535.0);
	  glClearIndex(a->bindex);
     }else{
	  if (a->db){
	       GLwDrawingAreaMakeCurrent(a->dgfx, a->dgc);
	       glClearColor((a->rbgcolor.red   & 0xf000) / 65535.0,
			    (a->rbgcolor.green & 0xf000) / 65535.0,
			    (a->rbgcolor.blue  & 0xf000) / 65535.0,
			    1.0);

	  }else{
	       GLwDrawingAreaMakeCurrent(a->gfx, a->gc);
	       
	       glClearColor(a->rbgcolor.red   / 65535.0,
			    a->rbgcolor.green / 65535.0,
			    a->rbgcolor.blue  / 65535.0,
			    1.0);
	  }
     }
     glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
     glFlush();
     return;
}

void imodv_setbuffer(ImodvApp *a)
{
     if (a->cindex)
	  return;

     imodv_clear(a);

     if (a->db){
	  XtUnmanageChild(a->dgfx);
	  XtManageChild(a->gfx);
	  a->db = 0;
	  a->cgfx = a->gfx; 
     }else{
	  XtUnmanageChild(a->gfx);
	  XtManageChild(a->dgfx);
	  a->db = 1;
	  a->cgfx = a->dgfx; 
     }
     /* DNM: set alpha after switching the db flag */
     a->alpha = DisplayHasAlpha(a->display, a->db);
     imodv_winset(a);
     glEnable(GL_DEPTH_TEST);
     glDepthMask(GL_TRUE);
     glFlush();
     glFinish();
     glXWaitX();
     glViewport(0, 0, a->winx, a->winy);
     

     return;
}

void imodv_resize_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodvApp *a = (ImodvApp *)client;
     Dimension width, height;

     if (w == a->dgfx)
	  if (a->dfirst) return;
     if (w == a->gfx)
	  if (a->first) return;

     XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);
     a->winx = width;
     a->winy = height;

     if (w == a->dgfx)
	  if (!a->dgfx_init)
	       return;

     if (w == a->gfx)
	  if (!a->gfx_init)
	       return;

     imodv_winset(a);
     glViewport(0, 0, a->winx, a->winy); 

     /* DNM: if call is NULL, then this is being called by one of our own 
	routines that will be doing a draw, so skip the draw here */
     if (call)
	  imodvDraw(a);

     return;
}

void imodv_expose_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodvApp *a = (ImodvApp *)client;
     Dimension width, height;
     float rad = Imodv->imod->view->rad*1.5f;
     imodv_ginit(w, client, call);
     imodv_winset(a);

     if (w == a->dgfx)
	  if (a->dfirst){
	       glClearColor(1.0, 0.0, 0.0, 0.0);
	       glClearIndex(a->bindex);
	       glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	       glEnable(GL_NORMALIZE);
	       glEnable(GL_DEPTH_TEST);
	       glFogi(GL_FOG_MODE, GL_LINEAR);
	       glFogf(GL_FOG_START,  0.0f);
	       glFogf(GL_FOG_END, rad);
	       light_init();
	       a->dfirst = 0;
	       imodv_resize_cb(w,client,NULL);
	  }

     if (w == a->gfx)
	 if (a->first){
	     glClearColor(1.0, 0.0, 0.0, 0.0);
	     glClearIndex(a->bindex);
	     glEnable(GL_LINE_SMOOTH);
	     glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	     glEnable(GL_NORMALIZE);
	     glEnable(GL_DEPTH_TEST);
	     glFogi(GL_FOG_MODE, GL_LINEAR);
	     glFogf(GL_FOG_START,  0.0f);
	     glFogf(GL_FOG_END, rad);
	     light_init();
	     imodv_resize_cb(w,client,NULL);
	     a->first = 0;
	 }

     XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);
     a->winx = width;
     a->winy = height;
     imodv_winset(a);
     glViewport(0, 0, a->winx, a->winy); 
     imodvDraw(a);
}

void imodv_ginit_rgbsb_cb(Widget w, XtPointer client, XtPointer call)
{
     ImodvApp *a = (ImodvApp *)client;
     XVisualInfo *vi;
     Dimension width, height;
     Arg args[1];

     XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);
     if (width > height) a->wscale = height; else a->wscale = width;
     a->winx = width;
     a->winy = height;

     XtSetArg(args[0], GLwNvisualInfo, &vi);
     XtGetValues(w, args, 1);

     a->gc = glXCreateContext(a->display, vi, NULL, GL_TRUE);
     GLwDrawingAreaMakeCurrent (w, a->gc);
     if (!a->gc){
	 fprintf(stderr, "Error getting graphics context.\n");
	 /* todo: exit/close window */
     }
     a->gfx_init = True;

     return;
}

void imodv_ginit(Widget w, XtPointer client, XtPointer call)
{
     ImodvApp *a = (ImodvApp *)client;
     XVisualInfo *vi;
     Dimension width, height;
     Window   window;
     Screen  *screen;
     int screen_num = 0;
     unsigned long plane[1];
     unsigned long pixels[IMODV_MAX_INDEX];
     XColor color;
     int i;
     Arg args[1];
     

     if (w == a->dgfx){
	 if (a->dgc) return;
     }
     
     if ((w == a->gfx)&&(!a->cindex)){
	 return;
     }

     XtVaGetValues(w, XmNwidth, &width, XmNheight, &height, NULL);
     if (width > height) a->wscale = height; else a->wscale = width;
     a->winx = width;
     a->winy = height;

     XtSetArg(args[0], GLwNvisualInfo, &vi);
     XtGetValues(w, args, 1);

     if (w == a->dgfx){
	  a->dgc = glXCreateContext
	       (a->display, vi, NULL, 
		GL_TRUE);

 	  GLwDrawingAreaMakeCurrent (w, a->dgc);

	  if (!a->dgc){
	       fprintf(stderr, "Error getting db graphics context.\n");
	       /* todo: exit/close window */
	  }
	  a->dgfx_init = True;
     }

     if (w == a->gfx){
	  a->gc = glXCreateContext(a->display, vi, NULL, GL_TRUE);
	  GLwDrawingAreaMakeCurrent (w, a->gc);
	  if (!a->gc){
	       fprintf(stderr, "Error getting graphics context.\n");
	       /* todo: exit/close window */
	  }
	  a->gfx_init = True;
     }

     if (a->cindex){
	  a->gc = a->dgc;

	  XtVaSetValues(w, 
			XmNdepth, a->depth,
			XmNvisual, a->visual,
			XmNcolormap, a->cmap, NULL);
	  imodvMapModel(a, a->imod);
     }
     return;
}



void imodvDraw(ImodvApp *a)
{
     static int first = 1;
     int winx, winy;
     /* static int drawcount = 0;
	printf("drawing %d\n", drawcount++); */

     if (!a->imod)
	  return;

     if (!a->doPick)
	  imodv_winset(a);

     if (a->dlist && a->update_dlist){
	  if (first)
	       first = 0;
	  else
	       glDeleteLists(1,1);
	  glNewList(1, GL_COMPILE);
	  imodvDraw_model(a, a->imod);
	  glEndList();
	  a->update_dlist = False;
     }

     imodv_clear(a);

     switch (a->stereo){
	case IMODV_STEREO_RL:
	  a->winx /= 2;
	  a->stereo *= -1;
	  imodvDraw_models(a);

	  a->stereo *= -1;
	  imodvDraw_models(a); 
	  a->winx *= 2;
	  glViewport(0, 0, a->winx, a->winy);
	  break;

	case IMODV_STEREO_TB:
	  a->winy /= 2;
	  a->stereo *= -1;
	  imodvDraw_models(a);

	  a->stereo *= -1;
	  imodvDraw_models(a);
	  a->winy *= 2;
	  glViewport(0, 0, a->winx, a->winy);
	  break;

	case IMODV_STEREO_HW:
	  a->stereo *= -1;
	  stereoDrawBuffer(GL_BACK_RIGHT);
	  stereoClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  imodvDraw_models(a);

	  a->stereo *= -1;
	  stereoDrawBuffer(GL_BACK_LEFT);
	  stereoClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	  imodvDraw_models(a);
	  break;


	case IMODV_STEREO_OFF:
	  if (a->dlist)
	       glCallList(1);
	  else
	       imodvDraw_models(a);
	  break;
     }

     imodv_swapbuffers(a);
     imodvControlSetView(a);
     imodvControlUpdate(a);
     imodvCallDrawCB(IMODV_DRAWCB_UNKNOWN);
     return;
}


/****************************************************************************/
/****************************************************************************/
/**                        Image Snapshot Utilities                        **/
/****************************************************************************/
/****************************************************************************/

static int snap_fileno = 0;

void imodv_reset_snap_cb(Widget w, XtPointer client, XtPointer call)
{
     snap_fileno = 0;
}

int imodv_auto_snapshot(char *inName, int format_type)
{
     char fname[32];
     FILE *tfp = NULL;
     char *usename = inName;
     char *fext = "rgb";
     Dimension width, height;

     if (format_type == SnapShot_TIF)
	  fext = "tif";

     if (!inName) {
	  usename = fname;
	  if (snap_fileno < 10000)
	       sprintf(fname, "imodv%04d.%s", snap_fileno, fext);
	  else
	       sprintf(fname, "imodv%d.%s", snap_fileno, fext);

	  snap_fileno += 1;
     }

     printf("imodv: Saving image to %s", usename);
     fflush(stdout);


     if (format_type == SnapShot_TIF) {
	  imodvDraw(Imodv);
	  glReadBuffer(GL_FRONT);
	  XtVaGetValues(Imodv->cgfx, XmNwidth, &width, 
		   XmNheight, &height, 
		   NULL);
	  b3dSetCurSize(width, height);
	  b3dSnapshot_TIF(usename, 1, NULL, NULL);
	  printf(".\n");
     } else {
	  if (imodv_snapshot(Imodv, usename))
	       printf(" : error writing file\n");
	  else
	       printf(".\n");
     }

     if (inName)
	  free(inName);
     return(0);
}

void iputbyte(FILE *fout, unsigned char val)
{
     unsigned char buf[1];
     
     buf[0] = val;
     fwrite(buf, 1, 1, fout);
}

void iputshort(FILE *fout, unsigned short val)
{
     unsigned char buf[2];

     buf[0] = (unsigned char)(val >> 8);
     buf[1] = (unsigned char)(val >> 0);
     fwrite(buf, 2, 1, fout);
}

void iputlong(FILE *fout, unsigned long val)
{
     unsigned char buf[4];
     
     buf[0] = (unsigned char)(val >> 24);
     buf[1] = (unsigned char)(val >> 16);
     buf[2] = (unsigned char)(val >>  8);
     buf[3] = (unsigned char)(val >>  0);
     fwrite(buf, 4, 1, fout);
}

int imodv_snapshot(ImodvApp *a, char *fname)
{
     FILE *fout;
     Dimension width, height;
     unsigned int depth;
     Pixmap pixmap;
     static GLXContext gc = 0;
     XVisualInfo *vi;
     unsigned char *pixels;
     unsigned char bdata[3];
     int i, xysize;
     GLint xoffset;
     struct MRCheader hdata;
     short sdata;
     char iname[80];

     fout = fopen(fname, "w");
     if (!fout){
	  perror("imodv: error opening file ");
	  return(-1);
     }

     XtVaGetValues(a->gfx, XmNwidth, &width, 
		   XmNheight, &height, 
		   XmNdepth, &depth, 
		   GLwNvisualInfo, &vi,
		   NULL);
     pixels = (unsigned char *)malloc(width * height * 3);  /* was * 3 * 4 */
     if (!pixels){
	  fclose(fout);
	  return(-1);
     }
     imodvDraw(a);

 /* Width Needs to be multiple of 4 for Octane with unsigned bytes */
     xoffset = (width % 4) / 2;
     width = 4 * (width / 4);  

/*     glReadPixels(0, 0, width, height,  
		  GL_RGB, GL_UNSIGNED_INT, pixels);
*/
     glReadBuffer(GL_FRONT);    /* DNM: have to read from front buffer */
     glReadPixels(xoffset, 0, width, height,  
		  GL_RGB, GL_UNSIGNED_BYTE, pixels);
     glFlush();

 
     /* Create an SGI rgb file */

     iputshort(fout, 474);       /* MAGIC                */
     iputbyte (fout,   0);       /* STORAGE is VERBATIM  */
     iputbyte (fout,   1);       /* BPC is 1             */
     iputshort(fout,   3);       /* DIMENSION is 3       */
     iputshort(fout, width);     /* XSIZE                */
     iputshort(fout, height);    /* YSIZE                */
     iputshort(fout,   3);       /* ZSIZE                */
     iputlong (fout, 0l);        /* PIXMIN is 0          */
     iputlong (fout, 255l);      /* PIXMAX is 255        */
     iputlong (fout, 0);         /* DUMMY 4 bytes        */
     sprintf(iname, "%s, Created by Imodv.", fname);
     fwrite(iname, 80, 1, fout); /* IMAGENAME            */
     iputlong (fout, 0);         /* COLORMAP is 0        */
     for(i=0; i<404; i++)        /* DUMMY 404 bytes      */
	  iputbyte(fout,0);

     /* image data */
/*
     for (i = 0, xysize = width * height; i < xysize; i++)
	  iputbyte (fout, pixels[i*3]/65536);
     for (i = 0; i < xysize; i++)
	  iputbyte (fout, pixels[(i*3)+1]/65536);
     for (i = 0; i < xysize; i++)
	  iputbyte (fout, pixels[(i*3)+2]/65536);
*/
     for (i = 0, xysize = width * height; i < xysize; i++)
	  iputbyte (fout, pixels[i*3]);
     for (i = 0; i < xysize; i++)
	  iputbyte (fout, pixels[(i*3)+1]);
     for (i = 0; i < xysize; i++)
	  iputbyte (fout, pixels[(i*3)+2]);

     free(pixels);
     fclose(fout);
     return(0);
}

/**************imodv movie workproc **************************/

/* DNM 11/5/00: changed logic from using interlocked time-outs and workprocs
   to using just this workproc after starting the movie.
   DNM 5/21/01: eliminated old code */

Boolean imodv_movie_wp(XtPointer client)
{
     ImodvApp *a = (ImodvApp *)client;
     
     Boolean finished = False;
     
     if (a->topLevel && a->movie && 
	 (a->md->xrotm || a->md->yrotm || a->md->zrotm)) {
	  a->movieFrames++;
	  a->movieCurrent = imodv_sys_time();
	  imodv_rotate_model(a,a->md->xrotm, a->md->yrotm, a->md->zrotm);
     } else {
	  a->wpid = (XtWorkProcId)0;
	  finished = True;
     }

     return(finished);
}



void imodvMapModel(ImodvApp *a, Imod *imod)
{
     Iobj *obj;
     unsigned long cindex;
     int ob;
     unsigned short red;
     unsigned short green;
     unsigned short blue;
     int shades = a->cstep;

     if (shades < 0)
	  shades *= -1;
     for(ob = 0, cindex = a->cstart; ob < imod->objsize;
	 ob++, cindex += a->cstep){
	  obj = &(imod->obj[ob]);
	  red = (obj->red   * 255.0f);
	  green = (obj->green * 255.0f);
	  blue = (obj->blue  * 255.0f);
	  imodvMapColor(a->gfx, (unsigned long)cindex, red, green, blue);
	  if (shades > 1)
	       imodvMapColor(a->gfx, (unsigned long)cindex-1, 
			     red/2, green/2, blue/2);
     }
     red = a->rbgcolor.red / 256;
     green = a->rbgcolor.green / 256;
     blue = a->rbgcolor.blue / 256;
     imodvMapColor(a->gfx, a->bindex, red, green, blue);
     return;
}



void imodvMapColor(Widget w,  unsigned int color,
		   unsigned short red,
		   unsigned short green,
		   unsigned short blue)
{
     XColor c;

     c.flags    = DoRed | DoGreen | DoBlue;
     c.pixel    = color;
     c.red   = red * 256;
     c.green = green * 256;
     c.blue  = blue * 256;
     
     if (!Imodv->gc)
	  return;

     XStoreColor(Imodv->display, Imodv->cmap, &c);    
     return;
}
