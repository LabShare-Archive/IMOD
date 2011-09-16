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
 *  No more Log
 */

#ifndef B3DGFX_H
#define B3DGFX_H

#include <qgl.h>
#include <qstring.h>

#define SnapShot_Default 0
#define SnapShot_RGB     1
#define SnapShot_TIF     2


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

QString b3dGetSnapshotName(char *name, int format_type, int digits,
                           int &fileno);
int b3dAutoSnapshot(char *name, int format_type, int *limits, 
                    bool checkConvert);
int b3dKeySnapshot(char *name, int shifted, int ctrl, int *limits, 
                   bool checkConvert = true);
int b3dSnapshot_NonTIF(QString fname, int rgbmode, int *limits,
                       unsigned char **data);
int b3dSnapshot_TIF(QString fname, int rgbmode, int *limits, 
                    unsigned char **data, bool checkConvert);



#endif
