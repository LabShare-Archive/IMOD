/*  IMOD VERSION 2.50
 *
 *  b3dgfx.c -- Custom graphics routines for IrisGL, OpenGL and X-Windows.
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
    Revision 3.2  2002/01/28 16:40:45  mast
    Also added button 1 up to list of standard translations; xyz window needed.

    Revision 3.1  2002/01/28 16:38:46  mast
    Modified drawing routines to accept a slice number and pass it on to
    routine that checks for identity of temporary buffer, instead of assuming
    current Z.  Modified buffer matching routine so that it would recognize a
    buffer match even when there is only a single buffer in use.  Fixed logic
    for recognizing a match with fractional zooms.  Removed factor of two 
    excess in sizing of temporary buffers.

*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "imod.h"
#include "b3dgfx.h"
#ifdef DRAW_GL
GLXconfig B3DGFX_GLXconfig_dbo[] = {
     { GLX_NORMAL, GLX_DOUBLE, TRUE},
     { GLX_OVERLAY, GLX_BUFSIZE, 2},
     { 0, 0, 0 }
};

GLXconfig B3DGFX_GLXconfig_doublebuffer[] = {
     { GLX_NORMAL, GLX_DOUBLE, TRUE},
     { 0, 0, 0 }
};

String B3DGFX_Translations =
"<KeyDown>:   glxInput()\n\
 <BtnDown>:   glxInput()\n\
 <BtnMotion>: glxInput()\n\
 <Btn1Up>:    glxInput()\n\
 <EnterNotify>: glwInput()\n\
";
#endif

#ifdef DRAW_X11
String B3DGFX_Translations =
"<KeyDown>: DrawingAreaInput()\n\
 <BtnDown>: DrawingAreaInput()\n\
 <Btn1Up>:  DrawingAreaInput()\n\
 <BtnMotion>: DrawingAreaInput()\n\
";
#endif
/* DNM 1/20/02: add button 1 up since xyz window needs it now */
#ifdef DRAW_OpenGL
String B3DGFX_Translations =
"<KeyDown>:   glwInput()\n\
 <BtnDown>:   glwInput()\n\
 <Btn1Up>:    glwInput()\n\
 <BtnMotion>: glwInput()\n\
";
#endif

static XFontStruct *CurFont     = NULL;
static Display     *CurDisplay  = NULL;
static Visual      *CurVisual   = NULL;

static Drawable     CurD        = 0;
static GC           CurGC       = 0;
static Colormap     CurCmap     = 0;
static Widget       CurWidget   = 0;
static int          CurDepth    = 0;
static Dimension    CurWidth;
static Dimension    CurHeight;

/* used for X11 simulation of bgnline() endline() pairs */
static int       CurDrawFlag= B3D_NODRAW;
static int       CurX;
static int       CurY;
static int       CurZ;

#define b3dX11GetY(y) (CurHeight - (y))

#ifdef DRAW_GL
static void glv2i(int x, int y)
{
     static float vert[2];
     vert[0] = x;
     vert[1] = y;
     v2f(vert);
}
#endif

XID b3dGetXContext(Widget w)
{
     if (!w)
	  return(0);
     return((XID)XCreateGC(XtDisplay(w), XtWindow(w), 0, NULL));
}

XID b3dGetContext(Widget w)
{
#ifdef DRAW_OpenGL
     XVisualInfo *vi;
     GLXContext ct;
     Bool direct = GL_TRUE;
     if (!w) return(0);


     XtVaGetValues(w, GLwNvisualInfo, &vi, NULL);

/*     XtVaSetValues(w, XmNcolormap, App->cmap, NULL); */

     ct = glXCreateContext(XtDisplay(w),
			   vi,
			   None,
			   direct);

     glXMakeCurrent(XtDisplay(w), (GLXDrawable)XtWindow(w), (GLXContext)ct);
     if (ct){
	  if (!glXIsDirect(XtDisplay(w),ct)){
	       wprint("Imod Warning :"
		      "Failed to get a direct rendering context.\n"
		      "Using software level rendering.");
	  }
	  glPixelStorei(GL_UNPACK_ALIGNMENT, 2);
     }else{
	  fprintf(stderr,"IMOD Warning : OpenGL context init failed.\n");
     }
     CurGC = (GC)ct;
     return((XID)ct);
#else
     if (!w) return(0);
     return((XID)XCreateGC(XtDisplay(w), XtWindow(w), 0, NULL));
#endif     
}

void b3dDestroyGFX()
{
#ifdef DRAW_OpenGL
     glXDestroyContext(CurDisplay, (GLXContext)CurGC); 
#else
     XFreeGC(CurDisplay, CurGC);
#endif
}

void b3dXWinset(Display *dpy, Widget w, XID context)
{
     CurDisplay  = dpy;
     CurD        = XtWindow(w);
     CurWidget   = w;
     CurDepth    = App->depth;
     CurGC       = (GC)context;
     CurDrawFlag = B3D_NODRAW;
     CurCmap = 0;
     XtVaGetValues(w,XmNwidth, &CurWidth, XmNheight, &CurHeight,
		   XmNvisual, &CurVisual, NULL);
     return;
}

void b3dWinset(Display *dpy, Widget w, XID context)
{
     CurDisplay = dpy;

     /* Set to NULL Window. */
     if ((!w) || (!context)){
	  CurD       = 0;
	  CurGC      = 0;
	  CurWidget  = 0;
	  CurDepth   = 0;
	  
#ifdef DRAW_OpenGL
	  glXMakeCurrent(dpy, None, None);
#endif

	  return;
     }

     CurD        = XtWindow(w);
     CurWidget   = w;
     CurDepth    = App->depth;

#ifdef DRAW_OpenGL
     glXMakeCurrent(dpy, CurD, (GLXContext)context);
     CurGC = (GC)context;
#endif
		    
#ifdef DRAW_GL
     GLXwinset(dpy, CurD);
     CurGC = (GC)context;
#endif
		    
#ifdef DRAW_X11
     CurGC       = (GC)context;
     CurDrawFlag = B3D_NODRAW;
     CurCmap = 0;
#endif

     XtVaGetValues(w, 
		   XmNwidth, &CurWidth, 
		   XmNheight, &CurHeight, 
		   XmNvisual, &CurVisual,
		   NULL);
     return;
}

/* DNM: needed so that imodv can use snapshot functions */
void b3dSetCurSize(int width, int height)
{
     CurWidth = width;
     CurHeight = height;
}

void b3dResizeViewport(void)
{
     double w,h;

     XtVaGetValues(CurWidget, 
		   XmNwidth, &CurWidth, XmNheight, &CurHeight, NULL);

#ifdef DRAW_GL
     reshapeviewport();
     ortho2(-0.5 , CurWidth - 0.5, -0.5, CurHeight - 0.5);
#endif

#ifdef DRAW_OpenGL
     w = CurWidth;
     h = CurHeight;
     glViewport((GLint)0, (GLint)0,
		(GLsizei)CurWidth, (GLsizei)CurHeight);
     glMatrixMode(GL_PROJECTION);
     glLoadIdentity();

     glOrtho(0.0 , w, 0.0, h, 0.5, -0.5);
     glMatrixMode(GL_MODELVIEW);
     glLoadIdentity();
#endif
     return;
}

void b3dSwapBuffers(void)
{
#ifdef DRAW_OpenGL
     glXSwapBuffers(CurDisplay, CurD);
/*     if (!glXIsDirect(CurD, CurGC)) */
     glFinish();
#endif

#ifdef DRAW_GL
     swapbuffers();
#endif

     return;
}

void b3dClear(void)
{
#ifdef DRAW_OpenGL
     glClear(GL_COLOR_BUFFER_BIT);
#endif
#ifdef DRAW_GL
     clear();
#endif
#ifdef DRAW_X11
     XClearArea(CurDisplay, CurD,
		0, 0, CurWidth, CurHeight, False);
#endif
     return;
}

void b3dFlush(void)
{
#ifdef DRAW_OpenGL
     glFlush();
#endif

#ifdef DRAW_GL
     gflush();
#endif
     return;
}


void b3dMapColor(unsigned int color,
		 unsigned short red,
		 unsigned short green,
		 unsigned short blue)
{
#ifndef NOUSEIMODDISPLAYMAPCOLOR
     mapcolor(color, red, green, blue);
#else

#ifdef DRAW_GL
     mapcolor(color, red, green, blue);
#else
     XColor c;
     Colormap cmap;

     wprint("map: %d (%d, %d, %d)\n", color, red, green, blue);
     XtVaGetValues(CurWidget, XmNcolormap, &cmap, NULL);
     c.flags    = DoRed | DoGreen | DoBlue;
     c.pixel    = color;
     c.red   = red << 8;
     c.green = green << 8;
     c.blue  = blue << 8;
     XStoreColors(CurDisplay, cmap, &c, 1);
#endif
#endif
     return;
}

void b3dColorIndex(unsigned int pix)
{
#ifdef DRAW_OpenGL
     glIndexi(pix);
     if (App->rgba){
	 if (pix == App->select)
	     glColor3ub(255, 255,   0);
	 else if (pix == App->shadow)
	     glColor3ub(128, 128,   0);
	 else if (pix == App->endpoint)
	     glColor3ub(255,   0,   0);
	 else if (pix == App->bgnpoint)
	     glColor3ub(  0, 255,   0);
	 else if (pix == App->curpoint)
	     glColor3ub(255,   0,   0);
	 else if (pix == App->foreground)
	     glColor3ub(255, 255, 128);
	 else if (pix == App->background)
	     glColor3ub(64,  64,  96);
	 else if (pix == App->ghost)
	     glColor3ub(16, 16, 16);
	 else if (pix == App->imodvbgcolor)
	     glColor3ub(  0,  0,  0);
     }


#endif
#ifdef DRAW_GL
     color(pix);
#endif
#ifdef DRAW_X11
     XSetForeground(CurDisplay, CurGC, pix);
#endif
     return;
}

void b3dLineStyle(int style)
{
#ifdef DRAW_OpenGL
     GLint factor = 1;
     switch(style){
	case B3D_LINESTYLE_SOLID:
	  glLineStipple(factor, 0xFFFF);
	  break;
	case B3D_LINESTYLE_DASH:
	  glLineStipple(factor, 0x0F0F);
	  break;
	case B3D_LINESTYLE_DDASH:
	  glLineStipple(factor, 0x3333);
	  break;
     }
#endif

#ifdef DRAW_GL
     static int first = 1;
     if (first){
	  deflinestyle(1, 0x0F0F);
	  deflinestyle(2, 0x3333);
	  first = 0;
     }
     setlinestyle(style);
#endif

#ifdef DRAW_X11
     static XGCValues val;
     switch(style){
	case B3D_LINESTYLE_SOLID:
	  val.line_style = LineSolid;
	  break;
	case B3D_LINESTYLE_DASH:
	  val.line_style = LineOnOffDash;
	  break;
	case B3D_LINESTYLE_DDASH:
	  val.line_style = LineDoubleDash;
	  break;
     }
     XChangeGC(CurDisplay, CurGC, GCLineStyle, &val);
#endif
     return;
}

void b3dLineWidth(int width)
{
#ifdef DRAW_OpenGL
     glLineWidth(width);
#endif

#ifdef DRAW_GL
     linewidth(width);
#endif

#ifdef DRAW_X11
     static XGCValues val;
     val.line_width = width;
     XChangeGC(CurDisplay, CurGC, GCLineWidth, &val);
#endif
     return;
}

/***************************************************************/
/* 2D Drawing Functions mapped into window, no transformations */
/* The origin is at the lower-left corner of the window.       */
/* Use OpenGL functions for 3D drawing.                        */

void b3dDrawPoint(int x, int y)
{
#ifdef DRAW_OpenGL
     glBegin(GL_POINTS);
     glVertex2i(x,y);
     glEnd();
#endif

#ifdef DRAW_GL
     bgnpoint();
     glv2i(x, y);
     endpoint();
#endif

#ifdef DRAW_X11
     y = b3dX11GetY(y);
     XDrawPoint(CurDisplay, CurD, CurGC, x, y);
#endif

     return;
}

void b3dDrawCross(int x, int y, int size)
{
#ifdef DRAW_OpenGL
     glBegin(GL_LINES);
     glVertex2i(x - size, y - size);
     glVertex2i(x + size, y + size);
     glEnd();
     glBegin(GL_LINES);
     glVertex2i(x + size, y - size);
     glVertex2i(x - size, y + size);
     glEnd();
#endif

#ifdef DRAW_GL
     bgnline();
     glv2i(x - size, y - size);
     glv2i(x + size, y + size);
     endline();
     bgnline();
     glv2i(x + size, y - size);
     glv2i(x - size, y + size);
     endline();
#endif

#ifdef DRAW_X11
     y = b3dX11GetY(y);
     XDrawLine(CurDisplay, CurD, CurGC,
	       x - size, y - size, x + size, y + size);
     XDrawLine(CurDisplay, CurD, CurGC,
	       x + size, y - size, x - size, y + size);
#endif
	       return;
     }


void b3dDrawPlus(int x, int y, int size)
{
#ifdef DRAW_OpenGL
     glBegin(GL_LINES);
       glVertex2i(x - size, y);
       glVertex2i(x + size, y);
     glEnd();
     glBegin(GL_LINES);
       glVertex2i(x, y - size);
       glVertex2i(x, y + size);
     glEnd();
#endif

#ifdef DRAW_GL
     bgnline();
       glv2i(x - size, y);
       glv2i(x + size, y);
     endline();
     bgnline();
       glv2i(x, y - size);
       glv2i(x, y + size);
     endline();
#endif

#ifdef DRAW_X11
     y = b3dX11GetY(y);
     XDrawLine(CurDisplay, CurD, CurGC,
	       x - size, y, x + size, y);
     XDrawLine(CurDisplay, CurD, CurGC,
	       x, y - size, x, y + size);
#endif
     return;
}

void b3dDrawStar(int x, int y, int size)
{ 
     b3dDrawPlus(x,y,size);
     b3dDrawCross(x,y,size);
     return;
}

void b3dDrawTriangle(int x, int y, int size)
{
#ifdef DRAW_OpenGL
     glBegin(GL_LINE_LOOP);
       glVertex2i(x, y + size);
       glVertex2i(x + size, y - (size/2));
       glVertex2i(x - size, y - (size/2));
     glEnd();
#endif

#ifdef DRAW_GL
     bgnline();
       glv2i(x, y + size);
       glv2i(x + size, y - (size/2));
       glv2i(x - size, y - (size/2));
       glv2i(x, y + size);
     endline();
#endif

#ifdef DRAW_X11
     y = b3dX11GetY(y);
     XDrawLine(CurDisplay, CurD, CurGC,
	       x, y + size, x + size, y - (size/2));
     XDrawLine(CurDisplay, CurD, CurGC,
	       x + size, y - (size/2), x - size, y - (size/2));
     XDrawLine(CurDisplay, CurD, CurGC,
	       x - size, y - (size/2), x, y + size);
#endif
     return;
}

void b3dDrawFilledTriangle(int x, int y, int size)
{
#ifdef DRAW_OpenGL
     glBegin(GL_POLYGON);
       glVertex2i(x, y + size);
       glVertex2i(x + size, y - (size/2));
       glVertex2i(x - size, y - (size/2));
       glVertex2i(x, y + size);
     glEnd();
#endif
#ifdef DRAW_GL
     bgnpolygon();
       glv2i(x, y + size);
       glv2i(x + size, y - (size/2));
       glv2i(x - size, y - (size/2));
       glv2i(x, y + size);
     endpolygon();
#endif

#ifdef DRAW_X11
     y = b3dX11GetY(y);
     XDrawLine(CurDisplay, CurD, CurGC,
	       x, y + size, x + size, y - (size/2));
     XDrawLine(CurDisplay, CurD, CurGC,
	       x + size, y - (size/2), x - size, y - (size/2));
     XDrawLine(CurDisplay, CurD, CurGC,
	       x - size, y - (size/2), x, y + size);
#endif
     return;
}

void b3dDrawCircle(int x, int y, int radius)
{
#ifdef DRAW_OpenGL
     static GLUquadricObj *qobj = NULL;
     int linewidth = 1;
     GLdouble inrad;
     GLdouble dradius = radius;
     
#endif

     if (radius <= 0)
	  return;

#ifdef DRAW_OpenGL
     if (!qobj)
	  qobj = gluNewQuadric();
     /* DNM: by request, make circles have current thickness, which is similar
	to what triangles and squares do */
     /*     glLineWidth(1.0f); */
     glPushMatrix();
     glTranslatef((float)x, (float)y, 0.0f);
     glGetIntegerv(GL_LINE_WIDTH, &linewidth);
     inrad = radius - linewidth;
     if (inrad < 0.)
	  inrad = 0;
     gluDisk(qobj, inrad, dradius, radius+4, 2);
     glPopMatrix();
#endif

#ifdef DRAW_GL
     circ(x, y, radius);
#endif

#ifdef DRAW_X11
     y = b3dX11GetY(y);
     XDrawArc(CurDisplay, CurD, CurGC,
	      x - (radius),
	      y - (radius),
	      (unsigned int)radius*2,
	      (unsigned int)radius*2,
	      0,23039);
#endif

     return;
}


void b3dDrawFilledCircle(int x, int y, int radius)
{
#ifdef DRAW_OpenGL
          static GLUquadricObj *qobj = NULL;
#endif
     if (radius <= 0)
	  return;

#ifdef DRAW_OpenGL
     if (!qobj)
	  qobj = gluNewQuadric();
     glPushMatrix();
     glLineWidth(1.0f);
     glTranslatef((float)x, (float)y, 0.0f);
     gluDisk(qobj, 0, radius, radius+4, 1);
     glPopMatrix();
#endif

#ifdef DRAW_GL
     circf(x, y, radius);
#endif

#ifdef DRAW_X11
     y = b3dX11GetY(y);
     XFillArc(CurDisplay, CurD, CurGC,
	      x - (radius),
	      y - (radius),
	      (unsigned int)radius*2,
	      (unsigned int)radius*2,
	      0,23039);
#endif
     return;
}

void b3dDrawLine(int x1, int y1, int x2, int y2)
{
#ifdef DRAW_OpenGL
     glBegin(GL_LINES);
     glVertex2i(x1, y1);
     glVertex2i(x2, y2);
     glEnd();
#endif

#ifdef DRAW_GL
     static float vert[2];
     bgnline();
     vert[0] = x1; vert[1] = y1;
     v2f(vert);
     vert[0] = x2; vert[1] = y2;
     v2f(vert);
     endline();
#endif

#ifdef DRAW_X11
     XDrawLine(CurDisplay, CurD, CurGC,
	       x1, b3dX11GetY(y1), x2, b3dX11GetY(y2));
#endif
     return;
}

void b3dDrawSquare(int x, int y, int size)
{
     b3dDrawRectangle(x-(size/2), y-(size/2), size, size);
     return;
}

void b3dDrawRectangle(int x, int y, int width, int height)
{
#ifdef DRAW_OpenGL
     glBegin(GL_LINE_STRIP);
     glVertex2i(x,y);
     glVertex2i(x+width,y);
     glVertex2i(x+width,y+height);
     glVertex2i(x,y+height);
     glVertex2i(x,y);
     glEnd();
#endif
     
#ifdef DRAW_GL
     bgnline();
     glv2i(x,y);
     glv2i(x+width,y);
     glv2i(x+width,y+height);
     glv2i(x,y+height);
     glv2i(x,y);
     endline();
#endif
     
#ifdef DRAW_X11
     y = b3dX11GetY(y);
     XDrawRectangle(CurDisplay, CurD, CurGC,
		    x, y-height, width, height);
#endif
     return;
}
void b3dDrawFilledSquare(int x, int y, int size)
{
     b3dDrawFilledRectangle(x-(size/2), y-(size/2), size, size);
     return;
}
void b3dDrawFilledRectangle(int x, int y, int width, int height)
{
#ifdef DRAW_OpenGL
     glBegin(GL_POLYGON);
     glVertex2i(x,y);
     glVertex2i(x+width,y);
     glVertex2i(x+width,y+height);
     glVertex2i(x,y+height);
     glEnd();
#endif

#ifdef DRAW_GL
     rectf(x,y,x+width,y+height);
#endif

#ifdef DRAW_X11
     y = b3dX11GetY(y);
     XFillRectangle(CurDisplay, CurD, CurGC,
		    x, y-height, width, height);
#endif
     return;
}

void b3dBeginLine(void)
{
#ifdef DRAW_OpenGL
     glBegin(GL_LINE_STRIP);
#endif

#ifdef DRAW_GL
     bgnline();
#endif

#ifdef DRAW_X11
     CurDrawFlag = B3D_BGNLINE;
#endif
     return;
}

void b3dEndLine(void)
{
#ifdef DRAW_OpenGL
     glEnd();
#endif

#ifdef DRAW_GL
     endline();
#endif

#ifdef DRAW_X11
     CurDrawFlag = B3D_NODRAW;
#endif
     return;
}


void b3dSetCurPoint(int x, int y, int z)
{
     CurX = x;
     CurY = y;
     CurZ = z;
}

void b3dVertex2i(int x, int y)
{
#ifdef DRAW_OpenGL
     glVertex2i(x, y);
#endif
     
#ifdef DRAW_GL
     float vert[2];
     vert[0] = x; vert[1] = y;
     v2f(vert);
#endif
     
#ifdef DRAW_X11
     switch(CurDrawFlag){

	case B3D_BGNLINE:
	  CurDrawFlag = B3D_DRAWLINE;
	  CurX = x; CurY = b3dX11GetY(y);
	  break;

	case B3D_DRAWLINE:
	  y = b3dX11GetY(y);
	  XDrawLine(CurDisplay, CurD, CurGC,
		    CurX, CurY, x, y);
	  CurX = x; CurY = y;
	  break;

	case B3D_NODRAW:
	  break;
     }
#endif
     return;
}

void b3dDrawBoxout(int llx, int lly, int urx, int ury)
{
#ifdef DRAW_OpenGL
     int cur_color;
     unsigned int back_color;

     glGetIntegerv(GL_CURRENT_INDEX, &cur_color);
     XtVaGetValues(CurWidget, XmNbackground, &back_color, NULL);
     glIndexi(back_color);
     if (App->rgba)
	  b3dColorIndex(App->background);
     
     if (lly > 0)
	  b3dDrawFilledRectangle(0,0,CurWidth,lly);
     if ((CurHeight - ury) > 0)
	  b3dDrawFilledRectangle(0, ury, CurWidth, CurHeight);
     if (llx > 0)
	  b3dDrawFilledRectangle(0, lly, llx, ury);
     if ((CurWidth - urx) > 0)
	  b3dDrawFilledRectangle(urx, lly, CurWidth, ury);

     glIndexi(cur_color);
#endif

#ifdef DRAW_GL
     long cur_color;
     unsigned long back_color;

     cur_color = getcolor();
     XtVaGetValues(CurWidget, XmNbackground, &back_color, NULL);
     color(back_color);
     if (lly > 0)
	  sboxf(0, 0, CurWidth, lly);

     if ((CurHeight - ury) > 0)
	  sboxf(0, ury, CurWidth, CurHeight);

     if (llx > 0)
	  sboxf(0, lly, llx, ury);

     if ((CurWidth - urx) > 0)
	  sboxf(urx, lly, CurWidth, ury);
     color(cur_color);

#endif

#ifdef DRAW_X11
     
     if (lly > 0)
	  XClearArea(CurDisplay, CurD,
		     0, CurHeight-lly, CurWidth, lly, False);

     if ((CurHeight - ury) > 0)
	  XClearArea(CurDisplay, CurD,
		     0, 0,
		     CurWidth, CurHeight-ury, False);

     if (llx > 0)
	  XClearArea(CurDisplay, CurD,
		     0, CurHeight-ury,
		     llx, ury - lly, False);

     if ((CurWidth - urx) > 0)
	  XClearArea(CurDisplay, CurD,
		     urx, CurHeight-ury,
		     CurWidth - urx, 
		     ury - lly, False);
     
#endif


     return;
}


/****************************************************************************/
/* Pixel Operations */


void b3dSetImageOffset(int winsize,     /* window size in wpixels.          */
		       int imsize,      /* image size in ipixels            */
		       double zoom,     /* zoom factor.                     */
		       int *drawsize,   /* size drawn in ipixels            */
		       int *offset,     /* offset from center in ipixels.   */
		       int *woff,       /* window offset in wpixels.        */
		       int *doff)       /* data offset in ipixels           */
{

     /* Fits compleatly inside of window. */
     if ( ((imsize - 1) * zoom) < winsize ){
	  *drawsize = imsize;
	  *woff     = ( winsize - ((imsize - 1) * zoom)) / 2;
	  *doff     = 0;

     }else{
	  /* Draw sub image. */
	  *woff = 0;
	  *drawsize = ((double)winsize) / zoom;
	  *doff = (imsize / 2 ) - (winsize / zoom / 2);
	  *doff -= *offset;

	  /* Offset in lower corner. */
	  if (*doff < 0){

	       /* maxborder. */
	       int maxwoff = winsize/6;
	       *woff = -(*doff) * zoom;
	       if (*woff > maxwoff)
		    *woff = maxwoff;
	       *doff = 0;
	       *drawsize = ((double)(winsize - (*woff))) / zoom;
	       *offset   = (int)((float)imsize*0.5f) - 
		    ((((float)winsize*0.5f) - (float)*woff)/(float)zoom);

/* old way: force offset in lower corner */
/*	       *doff += *offset;
	       *offset = *doff;
	       *doff -= *offset;
*/
	       /* try and fill corners. */
	       if (*drawsize < (imsize-1)) (*drawsize)++;
	       /* printf("ds do offset wo %d %d %d %d\n", *drawsize, *doff, *offset, *woff); */
	       return;
	  }

	  /* Offset in upper corner */
	  if ( (*doff + *drawsize) > (imsize - 1)){

	       /* The minimum drawsize. */
	       int minds = ((winsize * 0.8333333)/zoom);

	       *drawsize = imsize - *doff;

	       if (*drawsize < minds){
		    *drawsize = minds;
		    *doff     = imsize - *drawsize;
		    *offset   = ((float)imsize * 0.5) - (float)(*doff) -
			 (((float)winsize*0.5f)/(float)zoom) - 2.0f;
	       }

/* Old way: */
/*
	       *doff += *offset;
	       *offset = *doff - (imsize - 1 - *drawsize);
	       *doff -= *offset;
	       */
	       return;
	  }
	  if (*drawsize < (imsize-1)) (*drawsize)++;
     }
     return;
}


void b3dFreeCIImage(B3dCIImage *image)
{
#ifdef DRAW_OpenGL
     if (image){
	  if (image->id1)
	       free(image->id1);
	  if (image->id2)
	       free(image->id2);
	  free(image);
     }
#endif

#ifdef DRAW_X11
     if (image)
	  XDestroyImage((XImage *)image);
#endif

#ifdef DRAW_GL
     if (image)
	  free(image);
#endif
     return;
}

B3dCIImage *b3dGetNewCIImage(B3dCIImage *image, int depth)
{
     return(b3dGetNewCIImageSize(image, depth, (int)CurWidth, (int)CurHeight));
}

/* Sets up a second temporary image buffer - used by zap window (11/1/00) */
void b3dBufferImage(B3dCIImage *image)
{
#ifdef DRAW_OpenGL
     int pixsize = 2;
     if (!image) return;
     if (image->bufSize == 2) return;
     if (image->id2) return;
     if (App->rgba)
	  pixsize = 4;
     else if (App->depth == 8)
	  pixsize = 1;
     /* DNM 1/20/02: removed factor of 2 from malloc */
     image->id2 = (unsigned short *)malloc
	  ((image->width + 3) * (image->height + 3) * pixsize);
     image->bufSize = 2;
#else
     return;
#endif
}

void b3dFlushImage(B3dCIImage *image)
{
#ifdef DRAW_OpenGL 
     B3dCIImage *ri = image;
     if (!image) return;
     ri->dw1 = ri->dw2 = ri->dh1 = ri->dh2 = 0;
     ri->xo1 = ri->xo2 = ri->yo1 = ri->yo2 = -1;
     ri->zx1 = ri->zx2 = ri->zy1 = ri->zy2 = 0.0;
     ri->hq1 = ri->hq2 = -1;
     ri->cz1 = ri->cz2 = -1;
#endif
     return;
}

B3dCIImage *b3dGetNewCIImageSize(B3dCIImage *image, int depth, 
			       int width, int height)
{
#ifdef DRAW_OpenGL
     B3dCIImage *ri;
     int pixsize = 2;
     /* If the image is the same, return the same image. */
     if (image)
	  if ((image->width == width) && (image->height == height))
	       return(image);

     if (image)
	   b3dFreeCIImage(image);

     ri = malloc(sizeof(B3dCIImage));

     /* DNM: test on passed depth rather than App->depth so that slicer can
	get a short array even if depth = 8 */
     if (App->rgba)
	  pixsize = 4;
     else if (depth == 8) 
	  pixsize = 1;
     /* DNM 1/20/02: removed factor of 2 from malloc */
     ri->id1 = (unsigned short *)malloc((width+3) * (height+3) * pixsize);
     ri->id2 = NULL;
     ri->width = width;
     ri->height = height;
     ri->buf = 1;
     ri->bufSize = 1;
     b3dFlushImage(ri); 
     return( ri);
     
#endif
     
#ifdef DRAW_X11
     int bpp,bpl;
     char *data;
     B3dCIImage *rimage;

     if (image)
	  XDestroyImage((XImage *)image);

     bpp = depth / 8;
     if (depth % 8)
	  bpp++;
     bpl = bpp * width;
     data = (char *)malloc((bpl+4) * (height+2));
     if (!data){
	  return(NULL);
     }
     rimage = XCreateImage
	  (CurDisplay, CurVisual, depth,
	   ZPixmap, 0, data, width, height,
	   8, bpl);
     
     return(rimage);
#endif
     
#ifdef DRAW_GL
     char *data;
     if (image)
	  free(image);
     data = (char *)malloc((width+3) * (height+3) * sizeof(unsigned short));
     
     return((B3dCIImage *) data);
#endif
}

/* 1/20/02: No longer used by xyz window.  For OpenGL, this routine fills the
   temporary data array - then xyz window used glDrawPixels with OpenGL zoom */
void b3dFillGreyScalePixels(unsigned char *data,      /* input data      */
			    int xsize, int ysize,     /* size of input   */
			    B3dCIImage *image,        /* tmp image data. */
			    int base)                 /* colorindex ramp */
{
     unsigned long x, y;
     unsigned long mx = xsize;
     unsigned long my = ysize;
     unsigned long xysize = xsize * ysize;
     unsigned long i = 0;
     unsigned short b = base;

     int pixmin = 0;
     int rate   = 255;
     int pixmax = 255;

     if (CurDepth == 8 || App->rgba){
	  b = 0;
     }

#ifdef DRAW_OpenGL
     {
	  GLint  unpack = b3dGetImageType(NULL, NULL);
	  unsigned short *ids = image->id1;
	  unsigned char *idb = (unsigned char *)ids;
	  unsigned int *idi = (unsigned int *)ids;
	  unsigned int *cindex = App->cvi->cramp->ramp;

	  /* This would be a wrong thing to do */
	  if (!data){
	       b3dColorIndex(base);
	       glColor3f(0.0f, 0.0f, 0.0f);
	       b3dDrawFilledRectangle(0., 0., xsize, ysize);
	       return;
	  }
	  switch (unpack) {
	     case 1:
	       if (data)
		    for(i = 0; i < xysize; i++)
			 idb[i] = data[i];
	       else
		    for(i = 0; i < xysize; i++)
			 idb[i] = 0;
	       break;
	     case 2:
	       if (data) {
		    if (b)
			 for(i = 0; i < xysize; i++)
			      ids[i] = data[i] + b;
		    else
			 for(i = 0; i < xysize; i++)
			      ids[i] = data[i];
	       } else
		    for(i = 0; i < xysize; i++)
			 ids[i] = b;
	       break;
	     case 4:
	       if (data)
		    for(i = 0; i < xysize; i++)
			 idi[i] = cindex[data[i]];
	       else
		    for(i = 0; i < xysize; i++)
			 idi[i] = 0;
	       break;
	  }
     }
#endif

#ifdef DRAW_GL
	  if (data){
	       for(i = 0; i < xysize; i++)
		    image[i] = data[i] + b;
	  }else{
	       for(i = 0; i < xysize; i++)
		    image[i] = b;
	  }
#endif

#ifdef DRAW_X11
	  if (data){
	       for(y = my; y > 0; --y)
		    for(x = 0; x < mx; x++, i++)
			 XPutPixel((XImage *)image, x, y, data[i]);
	  }else{
	       for(y = my; y > 0; --y)
		    for(x = 0; x < mx; x++, i++)
			 XPutPixel((XImage *)image, x, y, b);
	  }
#endif

     return;
}

/* Unused by any routines with OpenGL */
void b3dPutCIImage(B3dCIImage *image, 
		   int src_x, int src_y,
		   int dest_x, int dest_y,
		   unsigned int width, unsigned int height)
/* width and height must be equal to image width,height */
{
     /* fix later, image src must be 0 now. */
     src_x = src_y = 0;

#ifdef DRAW_OpenGL
     glPixelStorei( GL_UNPACK_ALIGNMENT, 2);
/*     glPixelMap(GL_PIXEL_MAP_I_TO_I, rampsize, *ramp); */
     glPixelZoom(1.0,1.0);
     glRasterPos2i(dest_x, dest_y);
     glDrawPixels(width, height, GL_COLOR_INDEX, GL_UNSIGNED_SHORT, 
		  image->id1);
#endif

#ifdef DRAW_GL
     rectzoom(1.0,1.0);
     rectwrite(dest_x, dest_y, dest_x + width - 1, dest_y + height - 1, image);
#endif

#ifdef DRAW_X11
     XPutImage(CurDisplay, CurD, CurGC, image,
	       src_x, src_y,
	       dest_x, b3dX11GetY(dest_y)-height,
	       width, height);
#endif

     return;
}

/* This is used by the tumbler and tilt windows (11/1/00) - it sets up 
   offsets correctly to call b3dDrawGreyScalePixels, using OpenGl zoom */
/* DNM 1/20/02: added slice argument to govern image re-use */
void b3dDrawGreyScalePixelsSubArea
(B3dCIImage *image,                    /* window image data. */
 unsigned char *data,                  /* input image data. */
 int xsize, int ysize,                 /* size of input image data. */
 int xtrans, int ytrans,               /* offset from data center. */
 int llx, int lly,  int urx, int ury,  /* window coords or sub area. */
 int base, int zoom,                   /* colorindex ramp base, zoom factor. */
 int *xo, int *yo,                     /* return window origin */
 int slice)
{
     int xstart = 0, ystart = 0;
     int xborder = 0, yborder = 0;
     int xdrawsize = 0, ydrawsize = 0;
     int winx = urx - llx - 1;
     int winy = ury - lly - 1;

     if ( ((xsize - 1) * zoom) < winx ){
	  xdrawsize = (xsize);
	  xborder = ( winx - ((xsize - 1) * zoom)) / 2;
     }else{
	  xdrawsize = winx / zoom;
	  xstart = (xsize / 2 ) - (winx / zoom / 2);
	  xstart -= xtrans;
	  if (xstart < 0){
	       xstart += xtrans;
	       xtrans = xstart;
	       xstart -= xtrans;
	  }
	  if ( (xstart + xdrawsize) > (xsize - 1)){
	       xstart += xtrans;
	       xtrans = xstart - (xsize - 1 - xdrawsize);
	       xstart -= xtrans;
	  }
     }
     
     if ( ((ysize - 1) * zoom) < winy ){
	  ydrawsize = (ysize);
	  yborder = ( winy - ((ysize - 1) * zoom)) / 2;
     }else{
	  ydrawsize = winy / zoom;
	  ystart = (ysize / 2 ) - (winy / zoom / 2);
	  ystart -= ytrans;
	  if (ystart < 0){
	       ystart += ytrans;
	       ytrans = ystart;
	       ystart -= ytrans;
	  }
	  if ( (ystart + ydrawsize) > (ysize - 1)){
	       ystart += ytrans;
	       ytrans = ystart - (ysize - 1 - ydrawsize);
	       ystart -= ytrans;
	  }
     }
     *xo = (-(xstart*zoom)+xborder);
     *yo = (-(ystart*zoom)+yborder);

     b3dDrawGreyScalePixels(data, xsize, ysize, xstart, ystart,
			    llx + xborder, lly + yborder, 
			    xdrawsize, ydrawsize,
			    image, base, zoom, zoom, slice);

     return;
}


#ifdef DRAW_OpenGL
/* Tests if the image in either buffer matches the needs for the current
   display, and sets the image in *ri if so */
/* DNM 1/20/02: made it match when single buffering, fixed logic for
 making sure zoom matches under HQ display */
static int b3dImageMatch( B3dCIImage *image, unsigned short **ri,
		  int xo, int yo, int width, int height,
		  double zoom, int hq, int cz)
{
     /* Negative z is never to match */
     if (cz < 0)
	  return(0);

     /* Does it match first buffer? */
     if ((image->dw1 == width) && 
	 (image->dh1 == height) &&
	 (image->xo1 == xo)  &&
	 (image->yo1 == yo) &&
	 (image->cz1 == cz) &&
	 (image->hq1 == hq) && 
	 (!image->hq1 || image->zx1 == zoom)) {
	  *ri = image->id1;
	  image->buf = 1;
	  return(1);
     }

     if (image->bufSize == 1) return(0);

     /* If two in use, does it match second one? */
     if ((image->dw2 == width) &&
	 (image->dh2 == height) &&
	 (image->xo2 == xo)  &&
	 (image->yo2 == yo) &&
	 (image->cz2 == cz) &&
	 (image->hq2 == hq) && 
	 (!image->hq2 || image->zx2 == zoom)) {
	  *ri = image->id2;
	  image->buf = 2;
	  return(1);
     }

     return(0);
}

/* Sets up to use the other temporary image buffer, if two are in use; or
 (1/20/02) sets characteristics for the one image buffer */
static void b3dImageSet(B3dCIImage *image, unsigned short **ri,
		 int xo, int yo, int width, int height,
		 double zoom, int hq, int cz)
{
     /* Do not store a negative Z */
     if (cz < 0)
	  cz = -cz - 1;

     /* Use first buffer if one in use or if last one was # 2 */
     if (image->bufSize == 1 || image->buf == 2){
	  image->dw1 = width;
	  image->dh1 = height;
	  image->xo1 = xo;
	  image->yo1 = yo;
	  image->zx1 = zoom;
	  image->hq1 = hq;
	  *ri = image->id1;
	  image->buf = 1;
	  image->cz1 = cz;
	  return;
     }
     image->dw2 = width;
     image->dh2 = height;
     image->xo2 = xo;
     image->yo2 = yo;
     image->zx2 = zoom;
     image->hq2 = hq;
     *ri = image->id2;
     image->buf = 2;
     image->cz2 = cz;
     return;
}
#endif

int b3dGetImageType(GLenum *otype, GLenum *oformat)
{
     GLint unpack = 1;
     GLenum format = GL_COLOR_INDEX;
     GLenum type   = GL_UNSIGNED_BYTE;

     if (App->rgba){
	 unpack = 4;
	 type = GL_UNSIGNED_BYTE; 
	 /* DNM 1/20/02: eliminated unused conditional on SGI */
	 format   = GL_RGBA;
     }else{
	 if (App->depth > 8){ 
	     unpack = 2; 
	     type = GL_UNSIGNED_SHORT; 
	 }else{
	     /* use default values. */
	 }
     }
     if (otype)
	 *otype   = type;
     if (oformat)
	 *oformat = format;

     glPixelStorei(GL_UNPACK_ALIGNMENT, unpack);
     
     return unpack;
 }

#ifdef CHUNKDRAW_HACK
/* Routine to draw in chunks on the PC (RGBA data).  The largest chunk will
   be the image size divided by CHUNKDRAW_HACK.  The problem seemed to occur
   when a the window was resized to larger than the original window size,
   the first display did not fill the window, then the display was zoomed up
   to a size that filled the window.  This overwrote the cursor and crashed the
   system with the GeForce ULTRA card.  Load a 572 x 378 image, resize window
   to at least 384 in height (404 is good), and zoom to 1.5 or 1.2 with HQ or
   not.  It could also appear with zoom of 1 if initial window had zoom of 
   0.5. */
static void chunkdraw(int xysize, float zoom, int wxdraw, int wy,
		      int drawwidth, int sh,
		      GLenum format, GLenum type, unsigned int *idata)
{
     int chunklines, todraw;

     chunklines = (xysize / CHUNKDRAW_HACK) / drawwidth;
     if (chunklines < 1)
	  chunklines = 1;

     /*  printf ("sh %d  chunklines %d\n", sh, chunklines); */
     while (sh > 0) {
	  todraw = chunklines;
	  if (todraw > sh)
	       todraw = sh;
	  glRasterPos2f((float)wxdraw, (float)wy);
	  glDrawPixels(drawwidth, todraw, format, type, idata);
	  idata += drawwidth * todraw;
	  wy += todraw * zoom;
	  sh -= todraw;
     }
}
#endif

/* This routine will draw pixels from the input image using the OpenGL zoom */
/* DNM 1/20/02: Added slice argument to govern image re-use, rather than
   assuming CurZ */
void b3dDrawGreyScalePixels(unsigned char *data,      /* input data      */
			    int xsize, int ysize,     /* size of input   */
			    int xoffset, int yoffset, /* data offsets    */
			    int wx, int wy,           /* window start    */
			    int width, int height,    /* sub-area size   */
			    B3dCIImage *image,        /* tmp image data. */
			    int base,                 /* colorindex ramp */
			    double xzoom,
			    double yzoom, int slice)  
{

#ifdef DRAW_OpenGL
     double zoom = xzoom;
     int i, j, istart, ilim, di;
     unsigned short *sdata = (unsigned short *)image->id1;
     unsigned char *bdata = (unsigned char *)sdata;
     unsigned int  *idata = (unsigned int *)sdata;
     unsigned int  *rgbadata = (unsigned int *)data;
     unsigned char *bidata;
     unsigned short rbase = base;
     GLenum type, format;
     GLint  unpack = b3dGetImageType(&type, &format);
     unsigned int *cindex = App->cvi->cramp->ramp;

/*     printf("unpack = %d\n", unpack);
     for(i = 0; i < 256; i++)
	 printf("%x ", cindex[i]);
     printf("\n\n");
*/
     if (CurDepth == 8)
	  rbase = 0;

     if (!data){
	  b3dDrawFilledRectangle(wx, wy, width * xzoom, height * yzoom);
	  return;
     }

     if ((zoom >= 1.0) || (unpack == 4)){
	  /* DNM 1/4/01: This will work for zoom < 1 only if
	     the array that is copied into was made large enough to hold the
	     data being zoomed donw, rather than restricted to window size */
	 if (!b3dImageMatch(image, &sdata, xoffset, yoffset,
			    width, height, zoom, 0, slice)){
	     b3dImageSet(image, &sdata, xoffset, yoffset,
			 width, height, zoom, 0, slice);
	     bdata = (unsigned char *)sdata;
	     idata = (unsigned int  *)sdata;
	     for (j = 0, di = 0;  j < height; j++){
		 istart = (((j + yoffset) * xsize) + xoffset);
		 ilim  = istart + width;
		 
		 switch(unpack){
		   case 1:
		     if (App->rgba){
			 for (i = istart; i < ilim; i++, di++)
			     bdata[di] = cindex[data[i]];
		     }else
			 for (i = istart; i < ilim; i++, di++)
			     bdata[di] = data[i];
		     break;
		   case 2:
		     for (i = istart; i < ilim; i++, di++)
			 sdata[di] = data[i] + rbase;
		     break;
		   case 4:
		     switch (App->rgba) {
			case 1:  /* Look up from byte data */
			  for (i = istart; i < ilim; i++,di++)
			       idata[di] = cindex[data[i]];
			  break;
			case 3:  /* copy RGB to RGBA */
			  bidata = (unsigned char *)&(idata[di]);
			  bdata = (unsigned char *)&(data[3 * istart]);
			  for (i = istart; i < ilim; i++) {
			       *bidata++ = *bdata++;
			       *bidata++ = *bdata++;
			       *bidata++ = *bdata++;
			       *bidata++ = 0;
			  }
			  di += ilim - istart;
			  break;
			case 4:  /* untested RGBA data copy */
			  for (i = istart; i < ilim; i++, di++)
			       idata[di] = rgbadata[i];
			  break;
		     }
		     break;
		 }
	     }
	 }

	 /* printf("Draw wx %d wy %d width %d height %d window %d %d\n",
	    wx, wy, width, height, CurWidth, CurHeight); */
	 glPixelZoom((GLfloat)zoom, (GLfloat)zoom);
#ifndef CHUNKDRAW_HACK	 
	 glRasterPos2f((float)wx, (float)wy);
	 glDrawPixels(width, height, format, type, sdata);
#else
	 chunkdraw(xsize * ysize, zoom, wx, wy, width, height, format, type, 
		   (unsigned int *)sdata);
#endif	 
	  /* DNM: eliminated commented out textbook method. */

     }else{
	  /* DNM 1/4/01: eliminated old code for unpack = 4, leaving this
	     code of unknown use or correctness for zoom < 1, CI mode */
	  glRasterPos2f((float)wx, (float)wy);
	  glPixelZoom((GLfloat)zoom, (GLfloat)zoom);
	  glPixelTransferi(GL_INDEX_OFFSET, base);
	  glDrawPixels(xsize, ysize, format, type, sdata);
	  glPixelTransferi(GL_INDEX_OFFSET, 0);
     }
#endif

#ifdef DRAW_GL
/* image should be a pointer to a Colorindex[width * height]. */
     float zoom = xzoom;
     int i, j, istart, ilim, di;
     Colorindex *tdata = (Colorindex *)image;
     unsigned short rbase = base;

     if (!data){
	  b3dDrawFilledRectangle(wx, wy, width * xzoom, height * yzoom);
	  return;
     }
     for (j = 0, di = 0;  j < height; j++){
	  istart = (((j + yoffset) * xsize) + xoffset);
	  ilim  = istart + width;
	  for (i = istart; i < ilim; i++, di++)
	       tdata[di] = data[i] + rbase;
     }
     rectzoom((float)zoom, (float)zoom);
     rectwrite(wx, wy, wx + width - 1, wy + height - 1, image);
#endif

#ifdef DRAW_X11
/* image should be a pointer to an Ximage structure. */
     int xstop, ystop;
     int ystep;
     int i, j, x, y;
     int xz, yz;

     if (!image)
	  return;
     if (!data){
	  b3dDrawFilledRectangle(wx, wy, width * xzoom, height * yzoom);
	  return;
     }
     ystop = yoffset + height;
     ystep = xsize  - width;
     xstop = xoffset + width;
     
     if (zoom > 1)
	  for(j = (height * zoom) - 1, y = yoffset; y < ystop; y++)
	       for(yz = 0; yz < zoom; yz++, j--)
		    for(i = 0, x = xoffset; x < xstop; x++)
			 for(xz = 0; xz < zoom; xz++, i++)
			      XPutPixel((XImage *)image, i, j, 
					data[x + (y * xsize)]);
     else
	  for(j = (height * zoom) - 1, y = yoffset; y < ystop; j--, y++)
	       for(i = 0, x = xoffset; x < xstop; x++, i++)
		    XPutPixel((XImage *)image, i, j, data[x + (y * xsize)]);
     
     XPutImage(CurDisplay, CurD, CurGC, (XImage *)image,
	       0, 0, wx, b3dX11GetY(wy + (height * zoom)), 
	       width * zoom, height * zoom);
#endif

     return;
}

/* This routine is used to draw with rapid 1.5 x zoom, called only from 
   b3dDrawGreyScalePixelsHQ (11/1/00) */
/* DNM 1/20/02: Added slice argument to govern image re-use, rather than
   assuming CurZ */
static void b3dDrawGreyScalePixels15
(unsigned char *data,      /* input data          */
 int xsize, int ysize,     /* size of input data  */
 int xoffset, int yoffset, /* data offsets    */
 int wx, int wy,           /* window start    */
 int width, int height,    /* sub-area size   */
 B3dCIImage *image,        /* tmp image data. */
 int base, int slice
 )
{
     int i, j, istart, ilim, di;
     int iextra;
     unsigned short *sdata = (unsigned short *)image->id1;
     unsigned char  *bdata = (unsigned char *)sdata;
     unsigned int   *idata = (unsigned int *)sdata;
     unsigned int   *indata = (unsigned int *)idata;
     unsigned char  *bindata;
     unsigned short rbase  = base;
     unsigned int *cindex = App->cvi->cramp->ramp;
     int sw, sh; /* scaled width and height. */
     int maxi, maxj;
     unsigned int fillval;
     int sidefill;
     int ibase;
     int drawwidth;
     int wxdraw;
     unsigned char *fillpt = (unsigned char *)(&fillval);
     unsigned int val;
     unsigned char *valptr = (unsigned char *)(&val);

     GLenum type, format;
     GLint unpack = b3dGetImageType(&type, &format);

     if (CurDepth == 8) rbase = 0;
     sw = (int)(((float)width * 1.5f) );
     sh = (int)(((float)height * 1.5f));

     /* DNM: limit the pixels to the existing display */
     if (sh > CurHeight - wy)
	  sh = CurHeight - wy;
     if (sw > CurWidth - wx)
	  sw = CurWidth - wx;

     if (!data){
	  b3dDrawFilledRectangle(wx, wy, sw, sh);
	  return;
     }

     /* DNM: add the hack for the PC, rgb mode only */
     sidefill =0;
#ifdef PIXELDRAW_HACK
     if (unpack == 4)
	  sidefill = 1;
#endif
     if (sidefill) {
	  wxdraw = 0;
	  drawwidth = CurWidth;
	  *fillpt++ = 64;
	  *fillpt++ = 64;
	  *fillpt++ = 96;
	  *fillpt++ = 0;
     } else {
	  wxdraw = wx;
	  drawwidth = sw;
     }

     maxj = height;
     maxi = width;
	  
     if (!b3dImageMatch(image, &sdata, xoffset, yoffset,
			sw, sh, 1.5, 1, slice)){
	 b3dImageSet(image, &sdata, xoffset, yoffset,
		     sw, sh, 1.5, 1, slice);
	 bdata = (unsigned char *)sdata;
         idata = (unsigned int  *)sdata;
	 /*	       if (rbase) */
	 for (j = 0, di = 0;  j < maxj; j++){
	     istart = (((j + yoffset) * xsize) + xoffset);
	     
	     /* DNM: changed this to work with sw trimmed down to pixels
		remaining in the window */
	     ilim  = istart + 2 * (sw / 3);
	     iextra = sw%3;

	     /* DNM: rewrote to use di++ and i++, eliminated unneeded i++
		and iextra-- to make it more readable */
	     switch(unpack){
	       case 1:
		 
		 for (i = istart; i < ilim; i++){
		     bdata[di++] = data[i];
		     bdata[di++] = data[i++];
		     bdata[di++] = data[i];
		 }
		 if (iextra){
		     bdata[di++] = data[i]; iextra--;
		 }
		 if (iextra){
		     bdata[di++] = data[i];
		 }
		 if ( j % 2 ){
		     memcpy(&bdata[di], &bdata[di-sw], sw);
		     di += sw;
		 }

		 break;

	       case 2:
		 for (i = istart; i < ilim; i++){
		     sdata[di++] = data[i] + rbase;
		     sdata[di++] = data[i++] + rbase;
		     sdata[di++] = data[i] + rbase;
		 }
		 if (iextra){
		     sdata[di++] = data[i] + rbase;iextra--;
		 }
		 if (iextra){
		     sdata[di++] = data[i] + rbase;
		 }
		 if ( j % 2 ){
		     memcpy(&sdata[di], &sdata[di-sw], sw*2);
		     di += sw;
		 }
		 break;
		 
	       case 4:
		 /* DNM: add the fill on the sides for PC hack */
		 if (sidefill) 
		      for (i = 0; i < wx; i++)
			   idata[di++] = fillval;
		 switch (App->rgba) {
		    case 1:   /* Look up RGBA value from map with byte */
		      for (i = istart; i < ilim; i++){
			   idata[di++] = cindex[data[i]];
			   idata[di++] = cindex[data[i++]];
			   idata[di++] = cindex[data[i]];
		      }
		      if (iextra){
			   idata[di++] = cindex[data[i]]; iextra--;
		      }
		      if (iextra){
			   idata[di++] = cindex[data[i]];
		      }
		      break;

		    case 3:  /* Copy RGB into RGBA */
		      bindata = &(data[3 * istart]); 
		      valptr[3] = 0;
		      for (i = istart; i < ilim; i += 2){
			   valptr[0] = *bindata++;
			   valptr[1] = *bindata++;
			   valptr[2] = *bindata++;
			   idata[di++] = val;
			   idata[di++] = val;
			   valptr[0] = *bindata++;
			   valptr[1] = *bindata++;
			   valptr[2] = *bindata++;
			   idata[di++] = val;
		      }

		      if (iextra){
			   valptr[0] = *bindata++;
			   valptr[1] = *bindata++;
			   valptr[2] = *bindata++;
			   idata[di++] = val;iextra--;
		      }
		      if (iextra){
			   idata[di++] = val;
		      }
		      break;

		      case 4:  /* Untested: copy RGBA to RGBA */
		      for (i = istart; i < ilim; i++){
			   idata[di++] = indata[i];
			   idata[di++] = indata[i++];
			   idata[di++] = indata[i];
		      }
		      if (iextra){
			   idata[di++] = indata[i];iextra--;
		      }
		      if (iextra){
			   idata[di++] = indata[i];
		      }
		      break;
		 }
		 if (sidefill) 
		      for (i = sw + wx; i < CurWidth; i++)
			   idata[di++] = fillval;
		 if ( j % 2 ){
		     memcpy(&idata[di], &idata[di-drawwidth], drawwidth*4);
		     di += drawwidth;
		 }
		 break;

	     }
	     

	 }
     }
     /* printf("1.5 wx %d wy %d sw %d sh %d window %d %d\n",
	wxdraw, wy, drawwidth, sh, CurWidth, CurHeight);  */

     glPixelZoom((GLfloat)1.0f, (GLfloat)1.0f);

#ifndef CHUNKDRAW_HACK
     glRasterPos2f((float)wxdraw, (float)wy);
     glDrawPixels(drawwidth, sh, format, type, sdata);
#else
     chunkdraw(xsize * ysize, 1., wxdraw, wy, drawwidth, sh, format, type,
	       (unsigned int *)sdata);
#endif

     return;
}

/* This is the display routine called from the zap window, for regular or
   high quality data */
/* DNM 1/20/02: Added slice argument to govern image re-use, rather than
   assuming CurZ */
void b3dDrawGreyScalePixelsHQ(unsigned char *data,      /* input data      */
			      int xsize, int ysize,     /* size of input   */
			      int xoffset, int yoffset, /* data offsets    */
			      int wx, int wy,           /* window start    */
			      int width, int height,    /* sub-area size   */
			      B3dCIImage *image,        /* tmp image data. */
			      int base,                 /* colorindex ramp */
			      double xzoom,
			      double yzoom,
			      int quality, int slice)  
{
#ifdef DRAW_OpenGL
    if (!data){
	b3dColorIndex(base);
	glColor3f(0.0f, 0.0f, 0.0f);
	b3dDrawFilledRectangle(wx, wy, width * xzoom, height * yzoom);
	return;
    }
    
    /* special optimization. DNM: don't take if want quality*/
    if (!quality && (xzoom == 1.50) && (yzoom == 1.50)){
	b3dDrawGreyScalePixels15(data, xsize, ysize, xoffset, yoffset,
				 wx, wy, width, height, image, base, slice);
	return;
	} 

    /* Ignore quality button for small zooms */
    if ((xzoom <= 1.0)&&(yzoom <= 1.0))
	quality = 0;
    
    if (!quality){
	
	if (!App->wzoom){
	     /* If relying on OpenGL zoom completely, just call this */
	     b3dDrawGreyScalePixels(data, xsize, ysize, xoffset, yoffset,
				    wx, wy, width, height, image, base,
				    xzoom, yzoom, slice);
	    return;
	}else{
	    int hz = xzoom * 100.0;
	    if (!(hz%100)){
		 /* If it's an integer zoom (1 or more), then use the OpenGL
		    zoom through this routine */
		b3dDrawGreyScalePixels(data, xsize, ysize, 
				       xoffset, yoffset,
				       wx, wy, width, height, image, base,
				       xzoom, yzoom, slice);
		return;
	    } else {
		 /* DNM: encode fractional zooms as negative quality because
		    they will be stored as single pixels without zoom, but
		    the hq mode needs to be distinguished */
		 quality = -1;
	    }
	}
    }

    {
     float zoom = xzoom;
     unsigned short *sdata = (unsigned short *)image->id1;
     unsigned char  *bdata = (unsigned char *)sdata;
     unsigned int   *idata = (unsigned int *)sdata;
     unsigned int   *rgbadata;
     unsigned char  *bidata, *bptr;
     float ival;
     unsigned char val;

     float a,b,c,d;
     int dwidth = (float)width * zoom;
     int dheight= (float)height * zoom;
     int xi, pxi, nxi, yi, pyi, nyi, x1, x2, y1, y2;
     int i, j;
     
     float cx, cy;                            /* current x,y values          */
     float dx, dy;                            /* pixel  offset.              */
     float zs    = 1.0f/(float)zoom;          /* zoom step for x,y           */
     float trans = -(0.5 - (0.5 * zs));       /* translate offset. */
     float xstop = xoffset + width + trans; /* stop at this x coord.   */
     float ystop = yoffset + height + trans; /* stop at this y coord. */
     unsigned short rbase = base;
     float pixmin = 0.0f, pixmax = 255.0f;
     GLenum type, format;
     GLint unpack = b3dGetImageType(&type, &format);
     unsigned int *cindex = App->cvi->cramp->ramp;
     unsigned int fillval;
     unsigned char *fillpt = (unsigned char *)(&fillval);
     int sidefill;
     int rgbascale = 0;
     int ibase;
     int drawwidth;
     int wxdraw;

     if((unpack == 4) && (App->rgba == 1))
	 rgbascale = 1;

     if (dwidth > CurWidth){
	  dwidth = CurWidth;
     }
     if ( dheight > CurHeight){
	  dheight = CurHeight;
     }


     if (CurDepth == 8){
	  rbase = 0;
	  pixmin = RAMPMIN;
	  pixmax = RAMPMAX;
     }

     /* DNM for PC: need to fill sides of array to work around display bug */
     sidefill =0;
#ifdef PIXELDRAW_HACK
     if (unpack == 4)
	  sidefill = 1;
#endif
     if (sidefill) {
	  wxdraw = 0;
	  drawwidth = CurWidth;
	  *fillpt++ = 64;
	  *fillpt++ = 64;
	  *fillpt++ = 96;
	  *fillpt++ = 0;
     } else {
	  wxdraw = wx;
	  drawwidth = dwidth;
     }

     /* DNM: test and send quality rather than 1 so that it can tell whether
	fractional zooms are truly hq or not */

     if (!b3dImageMatch(image, &sdata, xoffset, yoffset,
			width, height, zoom, quality, slice)){
	  b3dImageSet(image, &sdata, xoffset, yoffset,
	  width, height, zoom , quality, slice); 

	  bdata = (unsigned char *)sdata;
	  idata = (unsigned int  *)sdata;
	  if (quality > 0 && App->rgba < 2){

	       /* For HQ, step to each display pixel and use quadratic 
		  interpolation at each position.  Not for color data */
	       for(j = 0, cy = yoffset + trans; j < dheight; cy += zs, j++) {
		    yi = cy + 0.5;
		    pyi = yi - 1;
		    nyi = yi + 1;
		    if (pyi < 0) pyi = 0;
		    if (nyi >= ysize) nyi = yi;
		    ibase = j * drawwidth;
		    if (sidefill)
			 for (i = 0; i < wx; i++)
			      idata[ibase++] = fillval;

		    for(i = 0, cx = xoffset + trans; 
			i < dwidth; cx += zs, i++){
			 xi = cx + 0.5;
			 val = data[xi + (yi * xsize)];
			 
			 dx = cx - xi;
			 dy = cy - yi;
			 pxi = xi - 1;
			 nxi = xi + 1;
			 
			 if (pxi < 0) pxi = 0;
			 if (nxi >= xsize) nxi = xi;
			 
			 x1 = data[pxi + (yi  * xsize)];
			 x2 = data[nxi + (yi  * xsize)];
			 y1 = data[xi  + (pyi * xsize)];
			 y2 = data[xi  + (nyi * xsize)];
			 
			 a = (x1 + x2) * 0.5f - (float)val;
			 b = (y1 + y2) * 0.5f - (float)val;
			 c = (x2 - x1) * 0.5f;
			 d = (y2 - y1) * 0.5f;
			 
			 ival = (a * dx * dx) + (b * dy * dy) +
			      (c * dx) + (d * dy) + val;
			 if (ival > pixmax)
			      val = pixmax;
			 else if (ival < pixmin)
			      val = pixmin;
			 else 
			      val = ival + 0.5f;

			  switch(unpack){
			     case 1:
			       bdata[i + ibase] = val;
			       break;
			     case 2:
			       sdata[i + ibase] = val + rbase;
			       break;
			     case 4:
			       idata[i + ibase] = cindex[val];
			       break;
			  } 
		    }

		    if (sidefill) {
			 ibase += dwidth;
			 for (i = wx + dwidth; i < CurWidth; i++)
			      idata[ibase++] = fillval;
		    } 
	       }
	  }else{

	       /* For low quality, non-integer zooms, use nearest neighbor
		  interpolation at each pixel */
	       for(j = 0, cy = yoffset + trans; cy < ystop; cy += zs, j++) {
		    yi = cy + 0.5;
		    ibase = j * drawwidth;

		    switch(unpack){
		       case 1:
			 for(i = 0, cx = xoffset + trans; 
			     cx < xstop; cx += zs, i++){
			      xi = cx + 0.5;
			      val = data[xi + (yi * xsize)];
			      bdata[i + ibase] = val;
			 }
			 break;
		       case 2:
			 for(i = 0, cx = xoffset + trans; 
			     cx < xstop; cx += zs, i++){
			      xi = cx + 0.5;
			      val = data[xi + (yi * xsize)];
			      sdata[i + ibase] = val + rbase;
			 }
			 break;
		       case 4:
			 if (sidefill)
			      for (i = 0; i < wx; i++)
				   idata[ibase++] = fillval;
			 switch (App->rgba) {
			    case 1:   /* lookup from bytes */
			      for(i = 0, cx = xoffset + trans; 
				  cx < xstop; cx += zs, i++){
				   xi = cx + 0.5;
				   val = data[xi + (yi * xsize)];
				   idata[i + ibase] = cindex[val];
			      }
			      break;
			    case 3:   /* copy RGB data to RGBA */
			      bidata = (unsigned char *)&(idata[ibase]);
			      bdata = &(data[3 * yi * xsize]);
			      for(i = 0, cx = xoffset + trans; 
				  cx < xstop; cx += zs, i++){
				   xi = cx + 0.5;
				   bptr = bdata + 3 * xi;
				   *bidata++ = *bptr++;
				   *bidata++ = *bptr++;
				   *bidata++ = *bptr++;
				   *bidata++ = 0;
			      }
			      break;
			    case 4:   /* untested placekeeper fro RGBA data */
			      rgbadata = (unsigned int *)data + yi * xsize;
			      for(i = 0, cx = xoffset + trans; 
				  cx < xstop; cx += zs, i++){
				   xi = cx + 0.5;
				   idata[i + ibase] = rgbadata[xi];
			      }
			      break;
			 }
			 if (sidefill){
			 ibase += dwidth;
			 for (i = wx + dwidth; i < CurWidth; i++)
			      idata[ibase++] = fillval;
			 }
			 break;
		    }
	       }
	  }
          }
     /* printf("HQ wx %d wy %d dwidth %d dheight %d window %d %d\n",
	wxdraw, wy, drawwidth, dheight, CurWidth, CurHeight); */

	  glPixelZoom((GLfloat)1.0f, (GLfloat)1.0f); 
#ifndef CHUNKDRAW_HACK
	  glRasterPos2f((float)wxdraw, (float)wy);
	  glDrawPixels(drawwidth, dheight, format, type, sdata); 
#else
	  chunkdraw(xsize * ysize, 1., wxdraw, wy, drawwidth, dheight, format,
		    type, (unsigned int *)sdata);
#endif	  
    }
#endif

#ifdef DRAW_GL
/* image should be a pointer to a Colorindex[width * height]. */
     float zoom = xzoom;
     float xstop, ystop;
     Colorindex *tdata = (Colorindex *)image;
     int dwidth = width * zoom;
     float soffset = - 0.5f + (0.5f / (float)zoom);
     float a,b,c,d;
     float dx, dy, ival;
     float zs, cx, cy;
     unsigned char val;
     int xi, pxi, nxi, yi, pyi, nyi, x1, x2, y1, y2, i, j;
     if (!data){
	  b3dDrawFilledRectangle(wx, wy, width * xzoom, height * yzoom);
	  return;
     }
     if (zoom <= 1){
	  b3dDrawGreyScalePixels
	       (data,xsize,ysize,xoffset,yoffset,wx,wy,
		width, height, image, base, zoom, zoom, slice);
	  return;
     }
     ystop = yoffset + height + soffset;
     xstop = xoffset + width  + soffset;
     
     zs = 1.0f/(float)zoom;
     for(j = 0, cy = yoffset + soffset; cy < ystop; cy += zs, j++)
	  for(i = 0, cx = xoffset + soffset; cx < xstop; cx+=zs, i++){
	       xi = cx + 0.5;
	       yi = cy + 0.5;
	       val = data[xi + (yi * xsize)];
	       
	       dx = cx - xi;
	       dy = cy - yi;
	       pxi = xi - 1;
	       nxi = xi + 1;
	       pyi = yi - 1;
	       nyi = yi + 1;
	       
	       if (pxi < 0) pxi = 0;
	       if (nxi >= xsize) nxi = xi;
	       if (pyi < 0) pyi = 0;
	       if (nyi >= ysize) nyi = yi;
	       
	       x1 = data[pxi + (yi  * xsize)];
	       x2 = data[nxi + (yi  * xsize)];
	       y1 = data[xi  + (pyi * xsize)];
	       y2 = data[xi  + (nyi * xsize)];
	       
	       a = (x1 + x2) * 0.5f - (float)val;
	       b = (y1 + y2) * 0.5f - (float)val;
	       c = (x2 - x1) * 0.5f;
	       d = (y2 - y1) * 0.5f;
	       
	       ival = (a * dx * dx) + (b * dy * dy) +
		    (c * dx) + (d * dy) + val;
	       if (ival > 255)
		    ival = 255;
	       if (ival < 0)
		    ival = 0;
	       val = ival + 0.5f;
	       
	       image[i + (j * dwidth)] = val + base;
	  }

     rectzoom(1.0f, 1.0f);
     rectwrite(wx, wy, wx + (width*zoom) - 1, wy + (height *zoom)- 1, image);
#endif

#ifdef DRAW_X11
/* image should be a pointer to an Ximage structure. */
     float xstop, ystop;
     int ystep;
     int i, j, x, y;
     int xz, yz;
     int pxi, nxi, pyi, nyi, x1, x2, y1, y2;
     float zs;
     float cx, cy;
     unsigned char val, pixmin, pixmax;
     float ival;
     int xi, yi;
     float dx, dy;
     float a,b,c,d;
     float soffset = - 0.5f + (0.5f / (float)zoom);
     int mj, mi;

     if (!image)
	  return;
     if (!data){
	  b3dDrawFilledRectangle(wx, wy, width * xzoom, height * yzoom);
	  return;
     }
     ystop = yoffset + height + soffset;
     ystep = xsize  - width;
     xstop = xoffset + width + soffset;
     mj = image->height;
     mi = image->width;
     pixmin = RAMPMIN;
     pixmax = RAMPMAX;

     j = (height * zoom) - 1;
     if (j > mj) j = mj;

     if (zoom > 1){
	  zs = 1.0f / (float)zoom;
	  for(cy = yoffset + soffset; (j>0)&&(cy < ystop); cy += zs, j--)
	       for(i = 0, cx = xoffset + soffset; cx < xstop; cx+=zs,i++){
		    
		    xi = cx + 0.5;
		    yi = cy + 0.5;
		    val = data[xi + (yi * xsize)];
		    
		    dx = cx - xi;
		    dy = cy - yi;
		    pxi = xi - 1;
		    nxi = xi + 1;
		    pyi = yi - 1;
		    nyi = yi + 1;
		    
		    if (pxi < 0) pxi = 0;
		    if (nxi >= xsize) nxi = xi;
		    if (pyi < 0) pyi = 0;
		    if (nyi >= ysize) nyi = yi;
		    
		    x1 = data[pxi + (yi  * xsize)];
		    x2 = data[nxi + (yi  * xsize)];
		    y1 = data[xi  + (pyi * xsize)];
		    y2 = data[xi  + (nyi * xsize)];
		    
		    a = (x1 + x2) * 0.5f - (float)val;
		    b = (y1 + y2) * 0.5f - (float)val;
		    c = (x2 - x1) * 0.5f;
		    d = (y2 - y1) * 0.5f;
		    
		    ival = (a * dx * dx) + (b * dy * dy) + 
			 (c * dx) + (d * dy) + val + 0.5f;
		    if (ival > pixmax)
			 val = pixmax;
		    else if (ival < pixmin)
			 val = pixmin;
		    else
			 val = ival;
		    
		    XPutPixel((XImage *)image, i, j, val);
	       }
     }else{
	  for(j = (height * zoom) - 1, y = yoffset; y < ystop; j--, y++)
	       for(i = 0, x = xoffset; x < xstop; x++, i++)
		    XPutPixel((XImage *)image, i, j, data[x + (y * xsize)]);
     }
     
     XPutImage(CurDisplay, CurD, CurGC, (XImage *)image,
	       0, 0, wx, b3dX11GetY(wy + (height * zoom)), 
	       width * zoom, height * zoom);
#endif
     
     return;
}


static double wzoomvals[] =
{ 0.25, 0.5, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0, 10.0, 
       12.0, 15.0, 18.0, 20.0, 25.0, 30.0};

/* DNM 7/28/02: this is the one that gets used, but better maintain zoomvals
   to have MAXZOOMI + 1 values too */
static double szoomvals[] =
{ 0.1, 0.16, 0.25, 0.35, 0.5, 0.75, 1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0,
  10.0, 12.0, 15.0, 18.0, 20.0};

static double zoomvals[] =
{ 0.1, 0.16, 0.25, 0.33, 0.5, 0.75, 1.0,
       1.25, 1.5, 1.75, 2.0, 2.5, 3.0, 4.0, 6.0, 8.0, 10.0, 12.0, 15.0};

#define MAXZOOMI 15

double b3dStepPixelZoom(double czoom, int step)
{
     double *zv;
     int i=0;
     int zoomi;

     if (App->wzoom)
	  zv = szoomvals;
     else
	  zv = zoomvals;

     if (step > 0) {
	  /* DNM: need to cast both to floats because czoom was stored as a
	     float and some comparisons can fail on PC */
	  for(i = 0; i < MAXZOOMI; i++)
	       if ((float)zv[i] > (float)czoom) break;
	  zoomi = (i - 1) + step;

     } else {
	  
	  for(i = MAXZOOMI; i >= 0; i--)
	       if ((float)zv[i] < (float)czoom) break;
	  zoomi = (i + 1) + step;
     }

     if (zoomi < 0) zoomi = 0;
     if (zoomi > MAXZOOMI) zoomi = MAXZOOMI;
     /* printf ("%f %d %d %f\n", czoom, step, zoomi, zv[zoomi]); */
     return(zv[zoomi]);
}


/***************************************************************************/
/* fonts                                                                   */
XFontStruct *b3dGetXFontStruct(char *name)
{
     return(XLoadQueryFont(CurDisplay, name));
}

void b3dFreeXFontStruct(XFontStruct *fs)
{
     XFreeFont(CurDisplay, fs);
}

void b3dXSetCurrentFont(XFontStruct *fs)
{
     if (!fs)
	  return;
     CurFont = fs;
     XSetFont(CurDisplay, CurGC, CurFont->fid);
}

void b3dSetCurrentFont(XFontStruct *fs)
{
#ifdef DRAW_OpenGL
     /* todo: use glXUseXFont() */
     glXUseXFont(CurFont->fid, CurFont->min_char_or_byte2,
		 CurFont->max_char_or_byte2 - CurFont->min_char_or_byte2,
		 CurFont->min_char_or_byte2);
#else
     b3dXSetCurrentFont(fs);
#endif
     return;
}

void b3dXDrawString(char *string, int x, int y, int alignment)
{
     int strsize = strlen(string);
     int swidth;
     int xp, yp;
     yp = y;
     switch(alignment){
	  
	case XmALIGNMENT_BEGINNING:
	  xp = x;
	  break;
	  
	case XmALIGNMENT_CENTER:
	  swidth = XTextWidth(CurFont, string, strsize);
	  xp = x - (swidth/2);
	  break;
	  
	case XmALIGNMENT_END:
	  swidth = XTextWidth(CurFont, string, strsize);
	  xp = x - swidth;
	  break;
	  
	default:
	  return;
     }
     yp = b3dX11GetY(yp);
     XDrawString(CurDisplay, CurD, CurGC,
		 xp, yp, string, strsize);
}

 

void b3dDrawString(char *string, int x, int y, int alignment)
{
     int strsize = strlen(string);
     int swidth;
     int xp, yp;
     yp = y;
     switch(alignment){

	case XmALIGNMENT_BEGINNING:
	  xp = x;
	  break;

	case XmALIGNMENT_CENTER:
	  swidth = XTextWidth(CurFont, string, strsize);
	  xp = x - (swidth/2);
	  break;

	case XmALIGNMENT_END:
	  swidth = XTextWidth(CurFont, string, strsize);
	  xp = x - swidth;
	  break;
	  
	default:
	  return;
     }

#ifdef DRAW_OpenGL
     glRasterPos2i(xp,yp);
     glCallLists(strsize, GL_BYTE, string);
#else
     yp = b3dX11GetY(yp);
     XDrawString(CurDisplay, CurD, CurGC,
		 xp, yp, string, strsize);

#endif
     return;
}

/*****************************************************************************/
/* Image Save Functions. */

/***********************************/
/*  Save window image to rgb file. */

static void iputbyte(FILE *fout, unsigned char val)
{
     unsigned char buf[1];
     
     buf[0] = val;
     fwrite(buf, 1, 1, fout);
     return;
}

static void iputshort(FILE *fout, unsigned short val)
{
     unsigned char buf[2];
     
     buf[0] = (unsigned char)(val >> 8);
     buf[1] = (unsigned char)(val >> 0);
     fwrite(buf, 2, 1, fout);
     return;
}

static void iputlong(FILE *fout, unsigned long val)
{
     unsigned char buf[4];
     
     buf[0] = (unsigned char)(val >> 24);
     buf[1] = (unsigned char)(val >> 16);
     buf[2] = (unsigned char)(val >>  8);
     buf[3] = (unsigned char)(val >>  0);
     fwrite(buf, 4, 1, fout);
     return;
}


#ifdef __vms
static int SnapShotFormat = SnapShot_TIF;
#else
static int SnapShotFormat = SnapShot_RGB;
#endif

void b3dAutoSnapshot(char *name, int format_type, int *limits)
{
     char fname[256];
     FILE *tfp = NULL;
     static int fileno;
     char *fext;


     switch(format_type){
       case SnapShot_RGB:
	 fext = "rgb";
	 break;
       case SnapShot_TIF:
	 fext = "tif";
	 break;
       default:
	 fext = "image";
     }


     fileno = 0;
     do{
	  if (tfp){
	       fclose(tfp);
	       tfp = NULL;
	  }
	  if (fileno < 10)
	       sprintf(fname, "%s00%d.%s", name, fileno++, fext);
	  else if (fileno < 100)
	       sprintf(fname, "%s0%d.%s", name, fileno++, fext);
	  else
	       sprintf(fname, "%s%d.%s", name, fileno++, fext);
	  
     }while ((tfp = fopen(fname, "r")));

     if (tfp){
	  fclose(tfp);
	  tfp = NULL;
     }
     wprint("%s: Saving image to %s\n", name, fname);

     switch (format_type){
       case SnapShot_RGB:
	 b3dSnapshot_RGB(fname, App->rgba, limits);
	 break;
       case SnapShot_TIF:
	 b3dSnapshot_TIF(fname, App->rgba, limits, NULL);
	 break;
       default:
	 b3dSnapshot(fname);
     }
     wprint("DONE!\n");
     return;
}

/* DNM 12/24/00 changed long length, long offset to types below, to prevent
   compiler warnings on SGI */
static void puttiffentry(short tag, short type, 
			 int length, unsigned int offset, FILE *fout)
{
     fwrite(&tag, sizeof(short), 1, fout);
     fwrite(&type, sizeof(short), 1, fout);
     fwrite(&length, sizeof(long), 1, fout);

     /* DNM: change __vms to LITTLE_ENDIAN to work on PC */
#ifndef LITTLE_ENDIAN
     if (length == 1)
	  switch(type){
	     case 1:
	       offset = offset << 24;
	       break;
	     case 3:
	       offset = offset << 16;
	       break;
     }
#endif
     fwrite(&offset, sizeof(long), 1, fout);
     return;
}

void b3dSnapshot_RGB(char *fname, int rgbmode, int *limits)
{
     FILE *fout;
     int i;
     unsigned char *pixels = NULL;
     long xysize, xsize, ysize;
     int blank = 1;

#ifdef DRAW_GL
     Screencoord llx, lly, urx, ury;
     long xo, yo;
#endif

#ifdef DRAW_OpenGL
     int mapsize;
     unsigned int *fcmapr, *fcmapg, *fcmapb;
     XColor xcolor;
     unsigned long *cindex, ci;
     unsigned char *pixout, tmp;
     int rpx = 0; 
     int rpy = 0;
     int rpWidth = CurWidth;
     int rpHeight = CurHeight;
#endif

     fout = fopen(fname, "w");
     if (!fout){
	  perror("Snapshot: error opening file ");
	  return;
     }

#ifdef DRAW_OpenGL
     if (limits) {
	  rpx = limits[0];
	  rpy = limits[1];
	  rpWidth = limits[2];
	  rpHeight = limits[3];
     }
     pixels = (unsigned char *)malloc(rpWidth * rpHeight * 4);
     if (!pixels){
	  fclose(fout);
	  return;
     }
     glPixelZoom(1.0,1.0);
     xysize = rpWidth * rpHeight;

     /* DNM: add rgb mode support */
     if (rgbmode) {
	  glReadPixels(rpx, rpy, rpWidth, rpHeight,
		       GL_RGBA, GL_UNSIGNED_BYTE, pixels);
	  glFlush();
	  pixout = (unsigned char *)pixels;
	  /* have to swap bytes and fill in 0 for A, because routine wants
	     AGBR*/
	  for (i = 0; i < xysize; i++) {
	       pixout[3] = pixout[0];
	       pixout[0] = 0;
	       pixout++;
	       tmp = *pixout;
	       *pixout = pixout[1];
	       pixout[1] = tmp;
	       pixout+=3;
	  }

     } else {
	  mapsize = 1 << App->depth;
	  CurCmap = App->cmapGL;
	  xcolor.flags = DoRed | DoGreen | DoBlue; 
	  fcmapr = (unsigned int *)malloc((mapsize+1) * sizeof(unsigned int));
	  if (!fcmapr) return;
	  fcmapg = (unsigned int *)malloc((mapsize+1) * sizeof(unsigned int));
	  if (!fcmapg) return;
	  fcmapb = (unsigned int *)malloc((mapsize+1) * sizeof(unsigned int));
	  if (!fcmapb) return;
	  
	  for(i = 0; i < mapsize; i++){
	       xcolor.pixel = i;
	       XQueryColor(CurDisplay, CurCmap, &xcolor);
	       fcmapr[i] = xcolor.red/256;
	       fcmapg[i] = xcolor.green/256;
	       fcmapb[i] = xcolor.blue/256;
	  }

	  glReadPixels(rpx, rpy, rpWidth, rpHeight,
		       GL_COLOR_INDEX, GL_UNSIGNED_INT, pixels);
	  glFlush();
	  cindex = (unsigned long *)pixels;
	  pixout = (unsigned char *)pixels;
	  for (i = 0; i < xysize; i++, cindex++){
	       ci = *cindex;
	       if (ci > mapsize) ci = 0;
	       /* DNM: switch from using endian-dependent shifts to packing
		  the array as AGBR */
	       *pixout++ = 0;
	       *pixout++ = fcmapb[ci];
	       *pixout++ = fcmapg[ci];
	       *pixout++ = fcmapr[ci];
	  }
	  free(fcmapr);
	  free(fcmapg);
	  free(fcmapb);
     }

     bdRGBWrite(fout, (int)rpWidth, (int)rpHeight, pixels);

     fclose(fout);
     free(pixels);
#endif

#ifdef DRAW_GL
     if (NULL == (pixels = (unsigned char *)malloc
	 ((CurWidth+3) * CurHeight * sizeof(unsigned long)))){
	  fclose(fout);
	  return;
     }
     getsize(&xsize, &ysize);
     getorigin(&xo, &yo);
     llx = xo; lly = yo;
     urx = xo + xsize - 1;
     ury = yo + ysize - 1;
     rectzoom(1.0, 1.0);
     readdisplay(llx, lly, urx, ury, (unsigned long *)pixels, 0);
     bdRGBWrite(fout, CurWidth, CurHeight, pixels);
     fclose(fout);
     free(pixels);
#endif

     return;
}


void b3dSnapshot_TIF(char *fname, int rgbmode, int *limits, 
		     unsigned char *data)
{
     FILE *fout;
     int i, j;
     unsigned char *pixels = NULL;
     int *lpixels;
     long xysize, xsize, ysize;
     int blank = 1;
     unsigned int pixel;
     unsigned int ifd;
     unsigned int colortable;
     int pad;
     unsigned char bpix,rpix,gpix;
     unsigned short tenum;
     unsigned short color[3];
     int depth;

#ifdef DRAW_X11
     XImage *image = NULL;
     XColor xcolor;
#endif

#ifdef DRAW_OpenGL
     int mapsize;
     unsigned int *fcmapr, *fcmapg, *fcmapb;
     XColor xcolor;
     int *cindex, ci;
     int rpx = 0; 
     int rpy = 0;
     int rpWidth = CurWidth;
     int rpHeight = CurHeight;
#endif

     fout = fopen(fname, "w");
     if (!fout){
	  perror("Snapshot: error opening file ");
	  return;
     }

#ifdef DRAW_OpenGL
     if (limits) {
	  rpx = limits[0];
	  rpy = limits[1];
	  rpWidth = limits[2];
	  rpHeight = limits[3];
     }
     xysize = rpWidth * rpHeight;
     xsize = rpWidth;
     ysize = rpHeight;
     if (!data) {
	  pixels = (unsigned char *)malloc(rpWidth * rpHeight * 4);
	  lpixels = (int *)pixels;
	  if (!pixels){
	       fclose(fout);
	       return;
	  }
     }
     glPixelZoom(1.0,1.0);
    
     if (data) {
	  rgbmode = 1;
     } else if (rgbmode) {
	  glReadPixels(rpx, rpy, rpWidth, rpHeight,
		       GL_RGBA, GL_UNSIGNED_BYTE, pixels);
	  glFlush();
	  /* App may not be defined if rgbmode is coming from standalone Imodv,
	     so set up a depth variable that works in later tests regardless */
	  depth = 8;

     } else {
	  depth = App->depth;
	  mapsize = 1 << depth;
	  CurCmap = App->cmapGL;
	  xcolor.flags = DoRed | DoGreen | DoBlue; 
	  fcmapr = (unsigned int *)malloc((mapsize+1) * sizeof(unsigned int));
	  if (!fcmapr) return;
	  fcmapg = (unsigned int *)malloc((mapsize+1) * sizeof(unsigned int));
	  if (!fcmapg) return;
	  fcmapb = (unsigned int *)malloc((mapsize+1) * sizeof(unsigned int));
	  if (!fcmapb) return;

	  for(i = 0; i < mapsize; i++){
	       xcolor.pixel = i;
	       XQueryColor(CurDisplay, CurCmap, &xcolor);
	       fcmapr[i] = xcolor.red/256;
	       fcmapg[i] = xcolor.green/256;
	       fcmapb[i] = xcolor.blue/256;
	  }
	  
	  glReadPixels(rpx, rpy, rpWidth, rpHeight,
		       GL_COLOR_INDEX, GL_UNSIGNED_INT, pixels);
	  glFlush();
     }

     /* DNM: change __vms to LITTLE_ENDIAN to work on PC */
#ifdef LITTLE_ENDIAN
     pixel = 0x002A4949;
#else
     pixel = 0x4D4D002A;
#endif

     if (!fwrite(&pixel, 4, 1, fout)){
	  fclose(fout);
	  if (!rgbmode) {
	       free(fcmapr);
	       free(fcmapg);
	       free(fcmapb);
	  }
	  if (!data)
	       free(pixels);
	  return;
     }

     ifd = xysize;
     if (depth > 8 || rgbmode)
	 ifd *= 3;
     ifd /= 4;
     ifd += 1;
     ifd *= 4;
     pad = -(ifd % 4) + 4;

     if (!fwrite(&ifd, 4, 1, fout)){
	  fclose(fout);
	  if (!rgbmode) {
	       free(fcmapr);
	       free(fcmapg);
	       free(fcmapb);
	  }
	  if (!data) 
	       free(pixels);
	  return;
     }

     if (data) {
	 for(j = ysize - 1; j >= 0; j--)
	      fwrite(&data[3 * j * xsize], 1, 3 * xsize, fout);

     } else if (rgbmode) {
	 for(j = ysize - 1; j >= 0; j--)
	     for(i = 0; i < xsize; i++)
		  fwrite(&lpixels[i + (j * xsize)], 1, 3, fout);

     } else if (depth > 8){
	 for(j = ysize - 1; j >= 0; j--)
	     for(i = 0; i < xsize; i++){
		 ci = lpixels[i + (j * xsize)];
		 rpix = (unsigned char)fcmapr[ci];
		 gpix = (unsigned char)fcmapg[ci];
		 bpix = (unsigned char)fcmapb[ci];
		 fwrite(&rpix, 1, 1, fout);
		 fwrite(&gpix, 1, 1, fout);
		 fwrite(&bpix, 1, 1, fout);
	     }
	 
     }else{
	 for(j = ysize - 1; j >= 0; j--)
	     for(i = 0; i < xsize; i++){
		 ci = lpixels[i + (j * xsize)];
		 bpix = ci;
		 fwrite(&bpix, 1, 1, fout);
	 }
     }
     pixel = 0;
     fwrite(&pixel, 4, 1, fout);
     fseek(fout, (int)ifd, SEEK_SET);
     
     if (depth > 8 || rgbmode){
	 tenum = 11;
	 colortable = ifd + 2 + (tenum * 12) + 7;
	 fwrite(&tenum, 2, 1, fout);
	 puttiffentry(256, 3, 1, xsize, fout);
	 puttiffentry(257, 3, 1, ysize, fout);
	 puttiffentry(258, 3, 3, colortable, fout);
	 puttiffentry(259, 3, 1, 1, fout);
	 puttiffentry(262, 3, 1, 2, fout);
	 puttiffentry(273, 4, 1, 8, fout);
	 puttiffentry(274, 3, 1, 1, fout);
	 puttiffentry(277, 3, 1, 3, fout);
	 puttiffentry(278, 4, 1, ysize*3, fout);
	 puttiffentry(279, 4, 1, xysize * 3, fout);
	 puttiffentry(284, 3, 1, 1, fout);  /* plane config */

	 ifd = 0;
	 fwrite(&ifd, 4, 1, fout);

	 fseek(fout, (int)colortable, SEEK_SET);
	 color[0] = 8;
	 fwrite(color, 2, 1, fout);
	 fwrite(color, 2, 1, fout);
	 fwrite(color, 2, 1, fout);
     }else{
	 tenum = 12;
	 fwrite(&tenum, 2, 1, fout);
	 
	 puttiffentry(256, 3, 1, xsize, fout);
	 puttiffentry(257, 3, 1, ysize, fout);
	 puttiffentry(258, 3, 1, 8, fout);  /* pixel size  1x8  */
	 puttiffentry(259, 3, 1, 1, fout);  /* compression , none*/
	 
	 puttiffentry(262, 3, 1, 3, fout);  /* cmap */
	 puttiffentry(273, 4, 1, 8, fout);  /* image data start */
	 puttiffentry(274, 3, 1, 1, fout);  
	 puttiffentry(277, 3, 1, 1, fout);  /* samples / pixel */
	 puttiffentry(278, 4, 1, ysize, fout); 
	 puttiffentry(279, 4, 1, xysize, fout);
	 puttiffentry(284, 3, 1, 1, fout);  /* plane config */
	 
	 colortable = ifd + 2 + (tenum * 12) + 7;
	 puttiffentry(320, 3, 768, colortable, fout);
	 ifd = 0;
	 fwrite(&ifd, 4, 1, fout);
	 
	 fseek(fout, (int)colortable, SEEK_SET);
	 for(i = 0; i < 256; i++){
	     color[0] = fcmapr[i]*256;
	     fwrite(color, 2, 1, fout);
	 }
	 for(i = 0; i < 256; i++){
	     color[0] = fcmapg[i]*256;
	     fwrite(color, 2, 1, fout);
	 }
	 for(i = 0; i < 256; i++){
	     color[0] = fcmapb[i]*256;
	     fwrite(color, 2, 1, fout);
	 }
     }
     fclose(fout);
     if (!rgbmode) {
	  free(fcmapr);
	  free(fcmapg);
	  free(fcmapb);
     }
     if (!data)
	  free(pixels);

#endif

#ifdef DRAW_X11
     xysize = CurWidth * CurHeight;
     xsize = CurWidth;
     ysize = CurHeight;
     if (NULL == (image = XGetImage(CurDisplay, XtWindow(CurWidget), 0, 0, 
				    xsize, ysize, AllPlanes, ZPixmap))){
	  fclose(fout);
	  return;
     }


#ifdef __vms
     pixel = 0x002A4949;
#else
     pixel = 0x4D4D002A;
#endif

     if (!fwrite(&pixel, 4, 1, fout)){
	  fclose(fout);
	  XDestroyImage(image);
	  return;
     }

     ifd = xysize;
     pad = -(ifd % 4) + 4;
     ifd += pad + 4;
     if (!fwrite(&ifd, 4, 1, fout)){
	  fclose(fout);
	  XDestroyImage(image);
	  return;
     }

     for(j = 0; j < ysize; j++)
	  for(i = 0; i < xsize; i++){
	       pixel = XGetPixel(image, i, j);
	       bpix = pixel;
	       fwrite(&bpix, 1, 1, fout);
	  }
     fwrite(&pixel, 4, 1, fout);
     fseek(fout, ifd, SEEK_SET);
     
     tenum = 12;
     fwrite(&tenum, 2, 1, fout);

     puttiffentry(256, 3, 1, xsize, fout);
     puttiffentry(257, 3, 1, ysize, fout);
     puttiffentry(258, 3, 1, 8, fout);  /* pixel size  1x8  */
     puttiffentry(259, 3, 1, 1, fout);  /* compression , none*/

     puttiffentry(262, 3, 1, 3, fout);  /* cmap */
     puttiffentry(273, 4, 1, 8, fout);  /* image data start */
     puttiffentry(274, 3, 1, 1, fout);  
     puttiffentry(277, 3, 1, 1, fout);  /* samples / pixel */
     puttiffentry(278, 4, 1, ysize, fout); 
     puttiffentry(279, 4, 1, xysize, fout);
     puttiffentry(284, 3, 1, 1, fout);  /* plane config */

     colortable = ifd + 2 + (tenum * 12) + 7;
     puttiffentry(320, 3, 768, colortable, fout);
     ifd = 0;
     fwrite(&ifd, 4, 1, fout);

     fseek(fout, colortable, SEEK_SET);

     xcolor.flags = DoRed | DoGreen | DoBlue;
     XtVaGetValues(CurWidget, XmNcolormap, &CurCmap, NULL);
     for(i = 0; i < 256; i++){
	  xcolor.pixel = i;
	  XQueryColor(CurDisplay, CurCmap, &xcolor);
	  color[0] = xcolor.red;
	  fwrite(color, 2, 1, fout);
     }
     for(i = 0; i < 256; i++){
	  xcolor.pixel = i;
	  XQueryColor(CurDisplay, CurCmap, &xcolor);
	  color[0] = xcolor.green;
	  fwrite(color, 2, 1, fout);
     }
     for(i = 0; i < 256; i++){
	  xcolor.pixel = i;
	  XQueryColor(CurDisplay, CurCmap, &xcolor);
	  color[0] = xcolor.blue;
	  fwrite(color, 2, 1, fout);
     }
     fclose(fout);


     XDestroyImage(image);
#endif
     return;
}



void b3dSnapshot(char *fname)
{
     if (SnapShotFormat == SnapShot_RGB)
	  b3dSnapshot_RGB(fname, App->rgba, NULL);
     else
	  b3dSnapshot_TIF(fname, App->rgba, NULL, NULL);
}


void b3dGetMouseWindowCoord(int *x, int *y)
{
     Window rootr, childr;
     int rx, ry;
     unsigned int maskr;

     XQueryPointer(CurDisplay, XtWindow(CurWidget),
		   &rootr, &childr,
		   &rx, &ry, x, y, &maskr);

     *y = CurHeight - *y;
     return;
}
