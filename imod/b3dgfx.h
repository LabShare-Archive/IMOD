/*  IMOD VERSION 2.42
 *
 *  $Id$
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
*/

#ifndef B3DGFX_H
#define B3DGFX_H

#ifdef  DRAW_OPENGL
#define DRAW_OpenGL
#endif

#include <stdio.h>
#include <X11/Xlib.h>
#include <Xm/Xm.h>

#ifdef DRAW_OpenGL
#include <GL/gl.h>
#include <GL/glu.h>
/* Change glwM to glw to avoid using motif widgets */
#include <X11/GLw/GLwMDrawA.h>
#endif

#ifdef DRAW_GL
#include <gl/gl.h>
#include <X11/Xirisw/GlxMDraw.h>
extern GLXconfig B3DGFX_GLXconfig_dbo[];
extern GLXconfig B3DGFX_GLXconfig_doublebuffer[];
#endif

#ifdef DRAW_X11
#include <Xm/DrawingA.h>
#endif

extern String B3DGFX_Translations;

#define SnapShot_Default 0
#define SnapShot_RGB     1
#define SnapShot_TIF     2


#ifdef DRAW_OpenGL
#define B3dDrawingAreaCallbackStruct GLwDrawingAreaCallbackStruct
/* Change glwM to glw to avoid using motif widgets */
#define B3dDrawingAreaWidgetClass    glwMDrawingAreaWidgetClass
#define B3dNexposeCallback GLwNexposeCallback
#define B3dNresizeCallback GLwNresizeCallback
#define B3dNinputCallback  GLwNinputCallback
#define B3dNginitCallback  GLwNginitCallback
typedef struct
{
     unsigned short *id1;
     unsigned short *id2;
     short width, height;
     short buf, bufSize;

     short dw1, dw2, dh1, dh2;
     short xo1, xo2, yo1, yo2;
     double zx1, zx2, zy1, zy2;
     short  hq1, hq2;
     short cz1, cz2;

}B3dCIImage;
#define FONT_LIST_BASE 10
#endif

#ifdef DRAW_GL
#define B3dDrawingAreaCallbackStruct GlxDrawCallbackStruct
#define B3dDrawingAreaWidgetClass    glxMDrawWidgetClass
#define B3dNexposeCallback GlxNexposeCallback
#define B3dNresizeCallback GlxNresizeCallback
#define B3dNinputCallback  GlxNinputCallback
#define B3dNginitCallback  GlxNginitCallback
typedef unsigned short B3dCIImage;
#endif

#ifdef DRAW_X11
#define B3dDrawingAreaCallbackStruct XmDrawingAreaCallbackStruct
#define B3dDrawingAreaWidgetClass    xmDrawingAreaWidgetClass
#define B3dNexposeCallback XmNexposeCallback
#define B3dNresizeCallback XmNresizeCallback
#define B3dNinputCallback  XmNinputCallback
typedef XImage B3dCIImage;
#endif

#define B3D_NODRAW    0
#define B3D_BGNLINE   1
#define B3D_DRAWLINE  2

/* linestyles */
#define B3D_LINESTYLE_SOLID 0
#define B3D_LINESTYLE_DASH  1
#define B3D_LINESTYLE_DDASH 2

#define b3dGetDrawCoord(x,z,o) (((x) * (z)) + (o))

/* functions */
#ifdef __cplusplus
extern "C" {
#endif

XID  b3dGetContext(Widget w);
XID  b3dGetXContext(Widget w);
void b3dWinset(Display *dpy, Widget w, XID context);
void b3dXWinset(Display *dpy, Widget w, XID context);
     void b3dSetCurSize(int width, int height);
void b3dDestroyGFX(void);
void b3dResizeViewport(void);
void b3dSwapBuffers(void);
void b3dFlush(void);
void b3dMapColor(unsigned int   color,
		 unsigned short red,
		 unsigned short green,
		 unsigned short blue);
void b3dColorIndex(unsigned int pix);
void b3dClear(void);

void b3dDrawPoint(int x, int y);
void b3dDrawPlus(int x, int y, int size);
void b3dDrawCross(int x, int y, int size);
void b3dDrawStar(int x, int y, int size);
void b3dDrawTriangle(int x, int y, int size);
void b3dDrawFilledTriangle(int x, int y, int size);
void b3dDrawCircle(int x, int y, int radius);
void b3dDrawFilledCircle(int x, int y, int radius);
void b3dDrawSquare(int x, int y, int size);
void b3dDrawFilledSquare(int x, int y, int size);
void b3dDrawRectangle(int x, int y, int width, int height);
void b3dDrawFilledRectangle(int x, int y, int width, int height);

void b3dBeginLine(void);
void b3dEndLine(void);
void b3dDrawLine(int x1, int y1, int x2, int y2);
void b3dLineWidth(int width);
void b3dLineStyle(int style);
void b3dVertex2i(int x, int y);
void b3dDrawBoxout(int llx, int lly, int urx, int ury);
void b3dSetCurPoint(int x, int y, int z);

/****************************************************************************/
/* Pixel data functions.                                                    */
/****************************************************************************/

/*
 *   create/delete image functions.
 */

B3dCIImage *b3dGetNewCIImage(B3dCIImage *image, int depth);
B3dCIImage *b3dGetNewCIImageSize
     (B3dCIImage *image, int depth, int width, int height);
void b3dFreeCIImage(B3dCIImage *image);

/* Image may cache data: 
 * the buffer function adds the cache buffer to the image.
 * the flush  function flushes the cache and forces new image
 * data to be drawn using the pixel draw functions below.
 */
void b3dBufferImage(B3dCIImage *image);
void b3dFlushImage(B3dCIImage *image);

/*
 *   Some utility functions.
 */
int b3dGetImageType(GLenum *otype, GLenum *oformat);
double b3dStepPixelZoom(double czoom, int step);
void b3dSetImageOffset(int winsize,     /* window size         */
		       int imsize,      /* image size          */
		       double zoom,     /* zoom factor.        */
		       int *drawsize,   /* size drawn          */
		       int *offset,     /* offset from center. */
		       int *woff,       /* window offset.      */
		       int *doff);      /* data offset         */
void b3dFillGreyScalePixels(unsigned char *data,
			    int xsize, int ysize,
			    B3dCIImage *image,
			    int base);

/*
 *   Pixel drawing.
 */

void b3dPutCIImage(B3dCIImage *image, int src_x, int src_y,
		   int dest_x, int dest_y,
		   unsigned int width, unsigned int height);

void b3dDrawGreyScalePixels
(unsigned char *data, int xsize, int ysize, int xoffset, int yoffset,
int wx, int wy, int width, int height, B3dCIImage *image, int base, 
double xzoom, double yzoom, int slice);

void b3dDrawGreyScalePixelsHQ
(unsigned char *data, int xsize, int ysize, 
 int xoffset, int yoffset,
 int wx, int wy, 
 int width, int height, 
 B3dCIImage *image, int base, 
 double xzoom, double yzoom, int quality, int slice);


void b3dDrawGreyScalePixelsSubArea(B3dCIImage *image,
				   unsigned char *data,
				   int xsize, int ysize,
				   int xtrans, int ytrans,
				   int llx, int lly,  int urx, int ury,
				   int base, int zoom,
				   int *xo, int *yo, int slice);


/*****************************************************************************
 *   String / Font display functions
 ****************************************************************************/

XFontStruct *b3dGetXFontStruct(char *name);
void b3dFreeXFontStruct(XFontStruct *fs);
void b3dSetCurrentFont(XFontStruct *font);
void b3dXSetCurrentFont(XFontStruct *font);
void b3dDrawString(char *string, int x, int y, int alignment);
void b3dXDrawString(char *string, int x, int y, int alignment);
     
void b3dSnapshot(char *fname);

/*void b3dAutoSnapshot(char *name);*/
void b3dAutoSnapshot(char *name, int format_type, int *limits);
void b3dSnapshot_RGB(char *fname, int rgbmode, int *limits);
void b3dSnapshot_TIF(char *fname, int rgbmode, int *limits, 
		     unsigned char *data);

void b3dGetMouseWindowCoord(int *x, int *y);


/* special file io commands. */
int bdRGBWrite(FILE *fout, int xsize, int ysize, unsigned char *pixels);
int bdTIFFWriteImage(FILE *fout, int xsize, int ysize, unsigned char *pixels);
int bdTIFFWriteMap(FILE *fout, int xsize, int ysize,
		   unsigned char *pixels, unsigned short *cmap);

#ifdef __cplusplus
}
#endif

#endif


