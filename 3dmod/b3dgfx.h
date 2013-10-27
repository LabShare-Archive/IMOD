/*  b3dgfx.h - declarations for b3dgfx.cpp 
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#ifndef B3DGFX_H
#define B3DGFX_H

#define GL_GLEXT_PROTOTYPES
#include <qgl.h>
#include <qstring.h>

// Wrapper functions for "extensions" on Windows (past GL 1.2)
#ifdef _WIN32
#include "glext_for_win.h"
void b3dDeleteBuffers(GLsizei n, const GLuint *buffers);
void b3dGenBuffers(GLsizei n, GLuint *buffers);
void b3dBindBuffer(GLenum target, GLuint buffer);
void b3dBufferData(GLenum target, GLsizeiptr size, const GLvoid *data, GLenum usage);
void b3dBufferSubData(GLenum target, GLintptr offset, GLsizeiptr size, 
                        const GLvoid *data);
#else
#define b3dDeleteBuffers glDeleteBuffers
#define b3dGenBuffers glGenBuffers
#define b3dBindBuffer glBindBuffer
#define b3dBufferData glBufferData
#define b3dBufferSubData glBufferSubData
#endif

// Compilation in Fedora 19 apparently needs glu.h - add it for every platform here
#ifdef Q_OS_MACX
#include <glu.h>
#else
#include <GL/glu.h>
#endif

// And wrapper function for primitive restart, needed more widely
void b3dPrimitiveRestartIndex(GLuint index);

// Extension enabled flags in return from b3dInitializeGL
#define B3DGLEXT_VERTBUF (1l << 0)
#define B3DGLEXT_PRIM_RESTART (1l << 1)
#define B3DGLEXT_ANY_SIZE_TEX (1l << 2)


typedef struct b3d_ci_image
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



#define B3D_NODRAW    0
#define B3D_BGNLINE   1
#define B3D_DRAWLINE  2

/* linestyles */
#define B3D_LINESTYLE_SOLID 0
#define B3D_LINESTYLE_DASH  1
#define B3D_LINESTYLE_DDASH 2

/* functions */

int b3dInitializeGL();

void b3dSetCurSize(int width, int height);
void b3dResizeViewportXY(int winx, int winy);
void b3dSubareaViewport(int xstart, int ystart, int xsize, int ysize);
void b3dColorIndex(int pix);
float b3dGetCurXZoom();
void b3dStippleNextLine(bool value);

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
void b3dDrawArrow(int xTail, int yTail, int xHead, int yHead, int tipLength = 16, 
                  int thickness = 4, bool antiAlias = true);

void b3dBeginLine(void);
void b3dEndLine(void);
void b3dDrawLine(int x1, int y1, int x2, int y2);
void b3dLineWidth(int width);
void b3dLineStyle(int style);
void b3dVertex2i(int x, int y);
void b3dDrawBoxout(int llx, int lly, int urx, int ury);

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
float b3dZoomDownCrit();
int b3dGetImageType(GLenum *otype, GLenum *oformat);
double b3dStepPixelZoom(double czoom, int step);
void b3dSetImageOffset(int winsize,     /* window size         */
                       int imsize,      /* image size          */
                       double zoom,     /* zoom factor.        */
                       int &drawsize,   /* size drawn          */
                       int &offset,     /* offset from center. */
                       int &woff,       /* window offset.      */
                       int &doff,       /* data offset in ipixels           */
                       int fillEdge);   /* Fill window to edge, maybe beyond*/

/*
 *   Pixel drawing.
 */

void b3dDrawGreyScalePixels
(unsigned char **data, int xsize, int ysize, int xoffset, int yoffset,
int wx, int wy, int width, int height, B3dCIImage *image, int base, 
 double xzoom, double yzoom, int slice, int rgba);

void b3dDrawGreyScalePixelsHQ
(unsigned char **data, int xsize, int ysize, 
 int xoffset, int yoffset,
 int wx, int wy, 
 int width, int height, 
 B3dCIImage *image, int base, 
 double xzoom, double yzoom, int quality, int slice, int rgba);


void b3dDrawGreyScalePixelsSubArea(B3dCIImage *image,
				   unsigned char **data,
				   int xsize, int ysize,
				   int xtrans, int ytrans,
				   int llx, int lly,  int urx, int ury,
				   int base, int zoom,
                                   int *xo, int *yo, int slice);



void b3dSetSnapDirectory(void);
QString b3dShortSnapName(QString fname);
void b3dSetMovieSnapping(bool snapping);     
void b3dSetDpiScaling(float factor);
int b3dSnapshot(QString fname);
int b3dSetNonTiffSnapFormat(int format);
int b3dNamedSnapshot(QString &fname, const char *prefix, int format, int *limits,
                     bool checkConvert);

QString b3dGetSnapshotName(const char *name, int format_type, int digits,
                           int &fileno);
int b3dAutoSnapshot(const char *name, int format_type, int *limits, 
                    bool checkConvert);
int b3dKeySnapshot(const char *name, int shifted, int ctrl, int *limits, 
                   bool checkConvert = true);
int b3dSnapshot_NonTIF(QString fname, int rgbmode, int *limits,
                       unsigned char **data);
int b3dSnapshot_TIF(QString fname, int rgbmode, int *limits, 
                    unsigned char **data, bool checkConvert);



#endif
