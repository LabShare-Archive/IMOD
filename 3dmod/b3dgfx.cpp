/*  b3dgfx.cpp -- Custom graphics routines for OpenGL.
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

#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include <qdir.h>
#include <qfiledialog.h>
#include "b3dgfx.h"
#include <qimage.h>
#include <qpainter.h>
#include <qlibrary.h>
#include <qglcolormap.h>
#include "imod.h"
#include "imodv.h"
#include "b3dfile.h"
#include "xcramp.h"
#include "preferences.h"

static void b3dDrawGreyScalePixels15
(unsigned char **dataPtrs,      /* input data          */
 int xsize, int ysize,     /* size of input data  */
 int xoffset, int yoffset, /* data offsets    */
 int wx, int wy,           /* window start    */
 int width, int height,    /* sub-area size   */
 B3dCIImage *image,        /* tmp image data. */
 int base, int slice, int rgba
 );
static int b3dImageMatch( B3dCIImage *image, unsigned short **ri,
                          int xo, int yo, int width, int height,
                          double xzoom, double yzoom, int hq, int cz);
static void b3dImageSet(B3dCIImage *image, unsigned short **ri,
                        int xo, int yo, int width, int height,
                        double xzoom, double yzoom, int hq, int cz);
static void getCubicFactors(double cx, int xsize, int &pxi, int &xi, int &nxi,
                            int &nxi2, float &fx1, float &fx2, float &fx3,
                            float &fx4);
static int snapshotCommon(QString fname, FILE **fout, bool openFile, int rgbmode,
                          int *limits, int dataPassed, unsigned char **pixelsOut,
                          int &rpWidth, int &rpHeight);

// Structure to hold factors for cubic interpolation
typedef struct cubic_factors
{
  int pxi, xi, nxi, nxi2;
  float fx1, fx2, fx3, fx4;
} cubicFactors;


static int     sCurWidth;
static int     sCurHeight;
static float   sCurXZoom;
static bool    sStippleNextLine = false;
static float   sZoomDownCrit = 0.8f;
static int     sExtFlags = 0;
static float   sDpiScaling = 1.;

// Snapshot statics

// DNM 12/28/03: just define default as TIF (not really used)
static int sSnapShotFormat = SnapShot_TIF;
static QString sSnapDirectory = "";
static QStringList sSnapCaptions;
static bool sWrapLastCaption = false;
static bool  sMovieSnapping = false;

// OpenGL statics

typedef void (*MYRESTARTINDEXPROC) (GLuint index);

// These function typedefs are found in IMOD/include/glext_for_win.h
#ifdef _WIN32
static PFNGLDELETEBUFFERSPROC sGlDeleteBuffers;
static PFNGLGENBUFFERSPROC sGlGenBuffers;
static PFNGLBINDBUFFERPROC sGlBindBuffer;
static PFNGLBUFFERDATAPROC sGlBufferData;
static PFNGLBUFFERSUBDATAPROC sGlBufferSubData;
static PFNGLPRIMITIVERESTARTINDEXPROC sGlPrimitiveRestartIndex;
#else
static MYRESTARTINDEXPROC sGlPrimitiveRestartIndex;
#endif

// "Initialization" function to determine OpenGL version and resolve extension pointers
// on Windows.  Must be called from inside an OpenGL context
int b3dInitializeGL()
{
  float glVersion;
  if (App->glInitialized)
    return sExtFlags;
  
  glVersion = atof((const char *)glGetString(GL_VERSION));
  if (glVersion >= 1.5)
    sExtFlags |= B3DGLEXT_VERTBUF;
  if (glVersion >= 2.0)
    sExtFlags |= B3DGLEXT_ANY_SIZE_TEX;
  if (glVersion >= 3.1)
    sExtFlags |= B3DGLEXT_PRIM_RESTART;
#ifdef _WIN32
  sGlDeleteBuffers = (PFNGLDELETEBUFFERSPROC)wglGetProcAddress("glDeleteBuffers");
  sGlGenBuffers = (PFNGLGENBUFFERSPROC)wglGetProcAddress("glGenBuffers");
  sGlBindBuffer = (PFNGLBINDBUFFERPROC)wglGetProcAddress("glBindBuffer");
  sGlBufferData = (PFNGLBUFFERDATAPROC)wglGetProcAddress("glBufferData");
  sGlBufferSubData = (PFNGLBUFFERSUBDATAPROC)wglGetProcAddress("glBufferSubData");
  sGlPrimitiveRestartIndex = (PFNGLPRIMITIVERESTARTINDEXPROC)wglGetProcAddress
    ("glPrimitiveRestartIndex");
  if (!sGlDeleteBuffers || !sGlGenBuffers || !sGlBindBuffer || !sGlBufferData ||
      !sGlBufferSubData) 
    sExtFlags &= ~B3DGLEXT_VERTBUF;
#else
#ifdef Q_OS_MACX
  QLibrary *gllib = new QLibrary("/System/Library/Frameworks/OpenGL.framework/"
                                 "Libraries/libGL.dylib");
#else
  QLibrary *gllib = new QLibrary("libGL.so");
#endif
  sGlPrimitiveRestartIndex = (MYRESTARTINDEXPROC)gllib->resolve
    ("glPrimitiveRestartIndex");
  if (!gllib->resolve("glDeleteLists"))
    imodPrintStderr("OpenGL library loading failed!\n");
  
  delete gllib;
#endif
  if (!sGlPrimitiveRestartIndex)
    sExtFlags &= ~B3DGLEXT_PRIM_RESTART;    
  App->glInitialized = 1;
  if (Imod_debug)
    imodPrintStderr("GL version %f   flags %d\n", glVersion, sExtFlags);
  return sExtFlags;
}

#ifdef _WIN32
void b3dDeleteBuffers(GLsizei n, const GLuint *buffers)
{
  (sGlDeleteBuffers)(n, buffers);
}

void b3dGenBuffers(GLsizei n, GLuint *buffers)
{
  (sGlGenBuffers)(n, buffers);
}

void b3dBindBuffer(GLenum target, GLuint buffer)
{
  (sGlBindBuffer)(target, buffer);
}

void b3dBufferData(GLenum target, GLsizeiptr size, const GLvoid *data, GLenum usage)
{
  sGlBufferData(target, size, data, usage);
}

void b3dBufferSubData(GLenum target, GLintptr offset, GLsizeiptr size, 
                        const GLvoid *data)
{
  sGlBufferSubData(target, offset, size, data);
}

#endif

void b3dPrimitiveRestartIndex(GLuint index)
{
  sGlPrimitiveRestartIndex(index);
}


/* Set a current window size for the drawing routines */
void b3dSetCurSize(int width, int height)
{
  sCurWidth = width;
  sCurHeight = height;
}

float b3dGetCurXZoom() { return sCurXZoom;}

void b3dStippleNextLine(bool value)
{
  sStippleNextLine = value;
}

/*
 * Set the viewport to the entire window of the given size, with 1:1 
 * transformation in drawing
 */
void b3dResizeViewportXY(int winx, int winy)
{
  // Need to shift the viewport a little bit because the drawing calls use
  // integers which map to the left edge of a pixel and can fall to lower one
  GLdouble eps = 0.05;
  glViewport((GLint)0, (GLint)0, (GLsizei)winx, (GLsizei)winy);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  glOrtho(-eps, (GLdouble)winx-eps, -eps, (GLdouble)winy-eps, 0.5, -0.5);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
}

/*
 * Set the viewport to the subarea of the window defined by the start and
 * size coordinates, still with a 1:1 transformation in drawing but clipping
 * outside the viewport
 */
void b3dSubareaViewport(int xstart, int ystart, int xsize, int ysize)
{
  GLdouble eps = 0.05;
  glViewport((GLint)xstart, (GLint)ystart, (GLsizei)xsize, (GLsizei)ysize);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho((GLdouble)xstart - eps, (GLdouble)(xstart + xsize - eps), 
          (GLdouble)ystart - eps, (GLdouble)(ystart + ysize - eps), 0.5, -0.5);
}

void b3dColorIndex(int pix)
{
  QColor qcol;
  glIndexi(pix);
  if (App->rgba){
    qcol = ImodPrefs->namedColor(pix);
    glColor3ub(qcol.red(), qcol.green(), qcol.blue());
  }
}

void b3dLineStyle(int style)
{
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
}

void b3dLineWidth(int width)
{
  glLineWidth(width);
}

/***************************************************************/
/* 2D Drawing Functions mapped into window, no transformations */
/* The origin is at the lower-left corner of the window.       */
/* Use OpenGL functions for 3D drawing.                        */

void b3dDrawPoint(int x, int y)
{
  glBegin(GL_POINTS);
  glVertex2i(x,y);
  glEnd();
}

void b3dDrawCross(int x, int y, int size)
{
  glBegin(GL_LINES);
  glVertex2i(x - size, y - size);
  glVertex2i(x + size, y + size);
  glEnd();
  glBegin(GL_LINES);
  glVertex2i(x + size, y - size);
  glVertex2i(x - size, y + size);
  glEnd();
}


void b3dDrawPlus(int x, int y, int size)
{
  glBegin(GL_LINES);
  glVertex2i(x - size, y);
  glVertex2i(x + size, y);
  glEnd();
  glBegin(GL_LINES);
  glVertex2i(x, y - size);
  glVertex2i(x, y + size);
  glEnd();
}

void b3dDrawStar(int x, int y, int size)
{ 
  b3dDrawPlus(x,y,size);
  b3dDrawCross(x,y,size);
}

void b3dDrawTriangle(int x, int y, int size)
{
  glBegin(GL_LINE_LOOP);
  glVertex2i(x, y + size);
  glVertex2i(x + size, y - (size/2));
  glVertex2i(x - size, y - (size/2));
  glEnd();
}

void b3dDrawFilledTriangle(int x, int y, int size)
{
  glBegin(GL_POLYGON);
  glVertex2i(x, y + size);
  glVertex2i(x + size, y - (size/2));
  glVertex2i(x - size, y - (size/2));
  glVertex2i(x, y + size);
  glEnd();
}

void b3dDrawCircle(int x, int y, int radius)
{
  int linewidth = 1;
  GLdouble inrad;
  GLdouble dradius = radius;
  if (radius <= 0)
    return;

#ifdef GLU_QUADRIC_HACK
  GLUquadricObj *qobj = gluNewQuadric();
#else
  static GLUquadricObj *qobj = NULL;
  if (!qobj)
    qobj = gluNewQuadric();
#endif

  /* DNM: by request, make circles have current thickness, which is similar
     to what triangles and squares do */
  /*     glLineWidth(1.0f); */
  glPushMatrix();
  glTranslatef((float)x, (float)y, 0.0f);
  glGetIntegerv(GL_LINE_WIDTH, (GLint *)&linewidth);
  inrad = radius - linewidth;
  if (inrad < 0.)
    inrad = 0;
  gluDisk(qobj, inrad, dradius, radius+4, 2);
  glPopMatrix();
#ifdef GLU_QUADRIC_HACK
  gluDeleteQuadric(qobj);
#endif
}


void b3dDrawFilledCircle(int x, int y, int radius)
{
  if (radius <= 0)
    return;

#ifdef GLU_QUADRIC_HACK
  GLUquadricObj *qobj = gluNewQuadric();
#else
  static GLUquadricObj *qobj = NULL;
  if (!qobj)
    qobj = gluNewQuadric();
#endif

  glPushMatrix();
  glLineWidth(1.0f);
  glTranslatef((float)x, (float)y, 0.0f);
  gluDisk(qobj, 0, radius, radius+4, 1);
  glPopMatrix();
#ifdef GLU_QUADRIC_HACK
  gluDeleteQuadric(qobj);
#endif
}

void b3dDrawLine(int x1, int y1, int x2, int y2)
{
  if (sStippleNextLine)
    glEnable(GL_LINE_STIPPLE);
  glBegin(GL_LINES);
  glVertex2i(x1, y1);
  glVertex2i(x2, y2);
  glEnd();
  if (sStippleNextLine)
    glDisable(GL_LINE_STIPPLE);
  sStippleNextLine = false;
}

void b3dDrawSquare(int x, int y, int size)
{
  b3dDrawRectangle(x-(size/2), y-(size/2), size, size);
}

void b3dDrawRectangle(int x, int y, int width, int height)
{
  glBegin(GL_LINE_STRIP);
  glVertex2i(x,y);
  glVertex2i(x+width,y);
  glVertex2i(x+width,y+height);
  glVertex2i(x,y+height);
  glVertex2i(x,y);
  glEnd();
}

void b3dDrawFilledSquare(int x, int y, int size)
{
  b3dDrawFilledRectangle(x-(size/2), y-(size/2), size, size);
}

void b3dDrawFilledRectangle(int x, int y, int width, int height)
{
  glBegin(GL_POLYGON);
  glVertex2i(x,y);
  glVertex2i(x+width,y);
  glVertex2i(x+width,y+height);
  glVertex2i(x,y+height);
  glEnd();
}

/*
 * Draws an arrow with the length of the head lines set to
 * tipLength (default 16) and line width thickness (default 4).  Line width is restored.
 */
void b3dDrawArrow(int xTail, int yTail, int xHead, int yHead, int tipLength, 
                  int thickness, bool antiAlias)
{
  GLfloat curWidth;
  bool smoothEnabled = glIsEnabled(GL_LINE_SMOOTH) == GL_TRUE;
  double angle = atan2((double)yTail - yHead, (double)xTail - xHead);
  int xtip = B3DNINT(tipLength * cos(angle - 45. * RADIANS_PER_DEGREE));
  int ytip = B3DNINT(tipLength * sin(angle - 45. * RADIANS_PER_DEGREE));
  glGetFloatv(GL_LINE_WIDTH, &curWidth);
  if (!smoothEnabled && antiAlias)
    glEnable(GL_LINE_SMOOTH);
  glLineWidth((GLfloat)thickness);
  glBegin(GL_LINE_STRIP);
  glVertex2i(xTail, yTail);
  glVertex2i(xHead, yHead);
  glVertex2i(xHead + xtip, yHead + ytip);
  glVertex2i(xHead, yHead);
  glVertex2i(xHead - ytip, yHead + xtip);
  glEnd();
  glLineWidth(curWidth);
  if (!smoothEnabled && antiAlias)
    glDisable(GL_LINE_SMOOTH);
}

void b3dBeginLine(void)
{
  glBegin(GL_LINE_STRIP);
}

void b3dEndLine(void)
{
  glEnd();
}


void b3dVertex2i(int x, int y)
{
  glVertex2i(x, y);
}

void b3dDrawBoxout(int llx, int lly, int urx, int ury)
{
  int cur_color;

  glGetIntegerv(GL_CURRENT_INDEX, (GLint *)&cur_color);

  b3dColorIndex(App->background);
     
  if (lly > 0)
    b3dDrawFilledRectangle(0,0,sCurWidth,lly);
  if ((sCurHeight - ury) > 0)
    b3dDrawFilledRectangle(0, ury, sCurWidth, sCurHeight);
  if (llx > 0)
    b3dDrawFilledRectangle(0, lly, llx, ury - lly);
  if ((sCurWidth - urx) > 0)
    b3dDrawFilledRectangle(urx, lly, sCurWidth, ury - lly);

  glIndexi(cur_color);
}


/****************************************************************************/
/* Pixel Operations */


void b3dSetImageOffset(int winsize,     /* window size in wpixels.          */
                       int imsize,      /* image size in ipixels            */
                       double zoom,     /* zoom factor.                     */
                       int &drawsize,   /* size drawn          */
                       int &offset,     /* offset from center. */
                       int &woff,       /* window offset.      */
                       int &doff,       /* data offset in ipixels           */
                       int fillEdge)    /* Fill window to edge, maybe beyond*/
{
  int newoffs;
  //imodPrintStderr("winsize %d  imsize %d  zoom %f  offset %d\n", winsize, imsize,
  //zoom, offset);
  /* Fits completely inside of window. (Why test against imsize - 1 ?*/
  if ( (int)((imsize) * zoom) <= winsize ) {
    drawsize = imsize;
    woff     = (int)(( winsize - (int)((imsize) * zoom)) / 2);
    doff     = 0;
    //imodPrintStderr("1 ds do offset wo %d %d %d %d\n", drawsize, doff, offset, woff);

  } else {

    /* Draw sub image. */
    woff = 0;
    drawsize = (int)(winsize / zoom);
    doff = (int)((imsize / 2 ) - (winsize / zoom / 2));
    doff -= offset;

    if (doff < 0){

      /* Offset in lower corner. */
      /* maxborder. */
      int maxwoff = winsize/6;
      woff = (int)(-(doff) * zoom);

      /* DNM 3/8/04: only change offset if woff needs to be limited, to 
         prevent image creep */
      if (woff > maxwoff) {
        woff = maxwoff;
        offset   = (int)(imsize*0.5f - ((winsize*0.5f - woff)/zoom));
      }
      doff = 0;
      drawsize = (int)((winsize - woff) / zoom);

    } else if ( (doff + drawsize) > (imsize - 1)){

      /* Offset in upper corner */
      /* The minimum drawsize. */
      int minds = (int)((winsize * 0.8333333)/zoom);

      drawsize = imsize - doff;

      if (drawsize < minds){
        drawsize = minds;
        doff     = imsize - drawsize;

        // 12/12/07: it was -2, don't even know why it should be -1, but
        // keep the offset from becoming more negative
        newoffs = (int)(imsize * 0.5 - doff - (winsize*0.5f)/zoom - 1.0f);
        offset   = B3DMAX(offset, newoffs);
      }
      //imodPrintStderr("2 ds do offset wo %d %d %d %d\n", drawsize, doff, offset, woff);
      return;
    }
    
    /* try and fill to edge if flag is +, or at least allow draw of one extra
       pixel if flag is 0 */
    if ((fillEdge > 0 && (int)(zoom * drawsize) < winsize - woff) ||
        (!fillEdge && (int)(zoom * (drawsize + 1) < winsize + 1 - woff)))
      drawsize++;
    //imodPrintStderr("3 ds do offset wo %d %d %d %d\n", drawsize, doff, offset, woff);

  }
  return;
}


void b3dFreeCIImage(B3dCIImage *image)
{
  if (image){
    if (image->id1)
      free(image->id1);
    if (image->id2)
      free(image->id2);
    free(image);
  }
  return;
}

B3dCIImage *b3dGetNewCIImage(B3dCIImage *image, int depth)
{
  return(b3dGetNewCIImageSize(image, depth, (int)sCurWidth, (int)sCurHeight));
}

/* Sets up a second temporary image buffer - used by zap window (11/1/00) */
void b3dBufferImage(B3dCIImage *image)
{
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
  /* DNM 3/12/03: only commit to having two buffers if it succeeded */
  if (image->id2)
    image->bufSize = 2;
}

void b3dFlushImage(B3dCIImage *image)
{
  B3dCIImage *ri = image;
  if (!image) return;
  ri->dw1 = ri->dw2 = ri->dh1 = ri->dh2 = 0;
  ri->xo1 = ri->xo2 = ri->yo1 = ri->yo2 = -1;
  ri->zx1 = ri->zx2 = ri->zy1 = ri->zy2 = 0.0;
  ri->hq1 = ri->hq2 = -1;
  ri->cz1 = ri->cz2 = -1;
  return;
}


/* Get the buffer for composing imageto send to GL */
/* DNM 3/12/03: be sure to return NULL if anything fails */
B3dCIImage *b3dGetNewCIImageSize(B3dCIImage *image, int depth, 
                                 int width, int height)
{
  B3dCIImage *ri;
  int pixsize = 2;
  /* If the image is the same, return the same image. */
  if (image)
    if ((image->width == width) && (image->height == height))
      return(image);

  if (image)
    b3dFreeCIImage(image);

  ri = (B3dCIImage *)malloc(sizeof(B3dCIImage));
  if (!ri)
    return NULL;

  /* DNM: test on passed depth rather than App->depth so that slicer can
     get a short array even if depth = 8 */
  if (App->rgba)
    pixsize = 4;
  else if (depth == 8) 
    pixsize = 1;
  /* DNM 1/20/02: removed factor of 2 from malloc */
  ri->id1 = (unsigned short *)malloc((width+3) * (height+3) * pixsize);
  if (!ri->id1) {
    free(ri);
    return NULL;
  }
  ri->id2 = NULL;
  ri->width = width;
  ri->height = height;
  ri->buf = 1;
  ri->bufSize = 1;
  b3dFlushImage(ri); 
  return( ri);
     
}

float b3dZoomDownCrit()
{
  return sZoomDownCrit;
}

/* 1/28/03: eliminated unused b3dPutCIImage and  b3dFillGreyScalePixels */


/* This is used by the tumbler and tilt windows (11/1/00) - it sets up 
   offsets correctly to call b3dDrawGreyScalePixels, using OpenGl zoom */
/* DNM 1/20/02: added slice argument to govern image re-use */
/* DNM 1/10/03: removed "-1" from winx and xsize and zsize expressions */
void b3dDrawGreyScalePixelsSubArea
(B3dCIImage *image,                    /* window image data. */
 unsigned char **data,                  /* input image data. */
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
  int winx = urx - llx;
  int winy = ury - lly;

  if ( ((xsize) * zoom) < winx ){
    xdrawsize = (xsize);
    xborder = ( winx - ((xsize) * zoom)) / 2;
  }else{
    xdrawsize = winx / zoom;
    xstart = (xsize / 2 ) - (winx / zoom / 2);
    xstart -= xtrans;
    if (xstart < 0){
      xstart += xtrans;
      xtrans = xstart;
      xstart -= xtrans;
    }
    if ( (xstart + xdrawsize) > (xsize)){
      xstart += xtrans;
      xtrans = xstart - (xsize - xdrawsize);
      xstart -= xtrans;
    }
  }
     
  if ( ((ysize) * zoom) < winy ){
    ydrawsize = (ysize);
    yborder = ( winy - ((ysize) * zoom)) / 2;
  }else{
    ydrawsize = winy / zoom;
    ystart = (ysize / 2 ) - (winy / zoom / 2);
    ystart -= ytrans;
    if (ystart < 0){
      ystart += ytrans;
      ytrans = ystart;
      ystart -= ytrans;
    }
    if ( (ystart + ydrawsize) > (ysize)){
      ystart += ytrans;
      ytrans = ystart - (ysize - ydrawsize);
      ystart -= ytrans;
    }
  }
  *xo = (-(xstart*zoom)+xborder);
  *yo = (-(ystart*zoom)+yborder);

  b3dDrawGreyScalePixels(data, xsize, ysize, xstart, ystart,
                         llx + xborder, lly + yborder, 
                         xdrawsize, ydrawsize,
                         image, base, zoom, zoom, slice, App->rgba);

  return;
}


/* Tests if the image in either buffer matches the needs for the current
   display, and sets the image in *ri if so */
/* DNM 1/20/02: made it match when single buffering, fixed logic for
   making sure zoom matches under HQ display */
static int b3dImageMatch( B3dCIImage *image, unsigned short **ri,
                          int xo, int yo, int width, int height,
                          double xzoom, double yzoom, int hq, int cz)
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
      (!image->hq1 || 
       (fabs(image->zx1 - xzoom) < 1.e-6 && fabs(image->zy1 - yzoom) < 1.e-6))) {
    *ri = image->id1;
    image->buf = 1;
    return(1);
  }

  if (image->bufSize == 1)
    return(0);

  /* If two in use, does it match second one? */
  if ((image->dw2 == width) &&
      (image->dh2 == height) &&
      (image->xo2 == xo)  &&
      (image->yo2 == yo) &&
      (image->cz2 == cz) &&
      (image->hq2 == hq) && 
      (!image->hq2 || 
       (fabs(image->zx2 - xzoom) < 1.e-6 && fabs(image->zy2 - yzoom) < 1.e-6))) {
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
                        double xzoom, double yzoom, int hq, int cz)
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
    image->zx1 = xzoom;
    image->zy1 = yzoom;
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
  image->zx2 = xzoom;
  image->zy2 = yzoom;
  image->hq2 = hq;
  *ri = image->id2;
  image->buf = 2;
  image->cz2 = cz;
  return;
}

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

// 1/19/13: eliminated CHUNKDRAW_HACK code

/* This routine will draw pixels from the input image using the OpenGL zoom */
/* DNM 1/20/02: Added slice argument to govern image re-use, rather than
   assuming CurZ */
void b3dDrawGreyScalePixels(unsigned char **dataPtrs,  /* input data      */
                            int xsize, int ysize,     /* size of input   */
                            int xoffset, int yoffset, /* data offsets    */
                            int wx, int wy,           /* window start    */
                            int width, int height,    /* sub-area size   */
                            B3dCIImage *image,        /* tmp image data. */
                            int base,                 /* colorindex ramp */
                            double xzoom,
                            double yzoom, int slice, int rgba)  
{

  double zoom = xzoom;
  int i, j, istart, ilim, di;
  if (!image)
    return;
  unsigned short *sdata = (unsigned short *)image->id1;
  unsigned char *bdata = (unsigned char *)sdata;
  unsigned int  *idata = (unsigned int *)sdata;
  unsigned int  *rgbadata;
  unsigned char *bidata;
  unsigned char *data;
  b3dUInt16 *usdata;
  unsigned short rbase = base;
  GLenum type, format;
  GLint  unpack = b3dGetImageType(&type, &format);
  unsigned int *cindex = App->cvi->cramp->ramp;
  unsigned char *bindex = App->cvi->cramp->bramp;
  sCurXZoom = zoom;

  /*     imodPrintStderr("unpack = %d\n", unpack);
         for(i = 0; i < 256; i++)
         imodPrintStderr("%x ", cindex[i]);
         imodPrintStderr("\n\n");
  */
  if (App->depth == 8)
    rbase = 0;

  if (!dataPtrs){
    b3dDrawFilledRectangle(wx, wy, (int)(width * xzoom), (int)(height * yzoom));
    return;
  }

  if ((zoom >= 1.0) || (unpack == 4)){
    /* DNM 1/4/01: This will work for zoom < 1 only if
       the array that is copied into was made large enough to hold the
       data being zoomed donw, rather than restricted to window size */
    if (!b3dImageMatch(image, &sdata, xoffset, yoffset, width, height, zoom, yzoom, 0,
                       slice)) {
      b3dImageSet(image, &sdata, xoffset, yoffset, width, height, zoom, yzoom, 0, slice);
      bdata = (unsigned char *)sdata;
      idata = (unsigned int  *)sdata;
      for (j = 0, di = 0;  j < height; j++){
        data = dataPtrs[j + yoffset];
        usdata = (b3dUInt16 *) data;
        istart = xoffset;
        ilim  = istart + width;
                 
        switch(unpack){
        case 1:
          if (rgba){
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
          switch (rgba) {
          case 1:  /* Look up from byte data */
            for (i = istart; i < ilim; i++,di++)
              idata[di] = cindex[data[i]];
            break;
          case 2:  /* Look up from ushort data */
            for (i = istart; i < ilim; i++,di++)
              idata[di] = cindex[usdata[i]];
            break;
          case 3:  /* copy RGB to RGBA */
            bidata = (unsigned char *)&(idata[di]);
            bdata = (unsigned char *)&(data[3 * istart]);
            for (i = istart; i < ilim; i++) {
              *bidata++ = bindex[*bdata++];
              *bidata++ = bindex[*bdata++];
              *bidata++ = bindex[*bdata++];
              *bidata++ = 0;
            }
            di += ilim - istart;
            break;
          case 4:  /* untested RGBA data copy */
            rgbadata = (unsigned int *)data;
            for (i = istart; i < ilim; i++, di++)
              idata[di] = rgbadata[i];
            break;
          }
          break;
        }
      }
    }

    /*imodPrintStderr("Draw wx %d wy %d width %d height %d window %d %d\n",
      wx, wy, width, height, sCurWidth, sCurHeight); */
    glPixelZoom((GLfloat)zoom, (GLfloat)yzoom);
    glRasterPos2f((float)wx, (float)wy);
    glDrawPixels(width, height, format, type, sdata);
    /* DNM: eliminated commented out textbook method. */

  } else {
    /* DNM 1/4/01: eliminated old code for unpack = 4, leaving this
       code of unknown use or correctness for zoom < 1, CI mode */
    glRasterPos2f((float)wx, (float)wy);
    glPixelZoom((GLfloat)zoom, (GLfloat)yzoom);
    glPixelTransferi(GL_INDEX_OFFSET, base);
    glDrawPixels(xsize, ysize, format, type, sdata);
    glPixelTransferi(GL_INDEX_OFFSET, 0);
  }
}

/* This routine is used to draw with rapid 1.5 x zoom, called only from 
   b3dDrawGreyScalePixelsHQ (11/1/00) */
/* DNM 1/20/02: Added slice argument to govern image re-use, rather than
   assuming CurZ */
static void b3dDrawGreyScalePixels15
(unsigned char **dataPtrs,  /* input data line pointers */
 int xsize, int ysize,     /* size of input data  */
 int xoffset, int yoffset, /* data offsets    */
 int wx, int wy,           /* window start    */
 int width, int height,    /* sub-area size   */
 B3dCIImage *image,        /* tmp image data. */
 int base, int slice, int rgba
 )
{
  int i, j, istart, ilim, di;
  int iextra;
  if (!image)
    return;
  unsigned short *sdata = (unsigned short *)image->id1;
  unsigned char  *bdata = (unsigned char *)sdata;
  unsigned int   *idata = (unsigned int *)sdata;
  unsigned int   *indata;
  unsigned char  *bindata;
  unsigned char  *data;
  b3dUInt16 *usdata;
  unsigned short rbase  = base;
  unsigned int *cindex = App->cvi->cramp->ramp;
  unsigned char *bindex = App->cvi->cramp->bramp;
  int sw, sh; /* scaled width and height. */
  int maxj;
  int drawwidth;
  int wxdraw;
  unsigned int val;
  unsigned char *valptr = (unsigned char *)(&val);

  GLenum type, format;
  GLint unpack = b3dGetImageType(&type, &format);

  if (App->depth == 8) rbase = 0;
  sw = (int)(((float)width * 1.5f) );
  sh = (int)(((float)height * 1.5f));

  /* DNM: limit the pixels to the existing display and to the subarea array*/
  sh = B3DMIN(sCurHeight - wy, B3DMIN(image->height, sh));
  sw = B3DMIN(sCurWidth - wx, B3DMIN(image->width, sw));

  if (!dataPtrs){
    b3dDrawFilledRectangle(wx, wy, sw, sh);
    return;
  }

  /* DNM: 11/26/07: eliminate side filling hack */
  wxdraw = wx;
  drawwidth = sw;

  maxj = height;
          
  if (!b3dImageMatch(image, &sdata, xoffset, yoffset, sw, sh, 1.5, 1.5, 1, slice)) {
    b3dImageSet(image, &sdata, xoffset, yoffset, sw, sh, 1.5, 1.5, 1, slice);
    bdata = (unsigned char *)sdata;
    idata = (unsigned int  *)sdata;
    /*            if (rbase) */
    for (j = 0, di = 0;  j < maxj; j++){
      data = dataPtrs[j + yoffset];
      usdata = (b3dUInt16 *) data;
      istart = xoffset;

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
        switch (rgba) {
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

        case 2:   /* Look up RGBA value from map with int */
          for (i = istart; i < ilim; i++){
            idata[di++] = cindex[usdata[i]];
            idata[di++] = cindex[usdata[i++]];
            idata[di++] = cindex[usdata[i]];
          }
          if (iextra){
            idata[di++] = cindex[usdata[i]]; iextra--;
          }
          if (iextra){
            idata[di++] = cindex[usdata[i]];
          }
          break;

        case 3:  /* Copy RGB into RGBA */
          bindata = &(data[3 * istart]); 
          valptr[3] = 0;
          for (i = istart; i < ilim; i += 2){
            valptr[0] = bindex[*bindata++];
            valptr[1] = bindex[*bindata++];
            valptr[2] = bindex[*bindata++];
            idata[di++] = val;
            idata[di++] = val;
            valptr[0] = bindex[*bindata++];
            valptr[1] = bindex[*bindata++];
            valptr[2] = bindex[*bindata++];
            idata[di++] = val;
          }

          if (iextra){
            valptr[0] = bindex[*bindata++];
            valptr[1] = bindex[*bindata++];
            valptr[2] = bindex[*bindata++];
            idata[di++] = val;iextra--;
          }
          if (iextra){
            idata[di++] = val;
          }
          break;

        case 4:  /* Untested: copy RGBA to RGBA */
          indata = (unsigned int *)data;
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
        if ( j % 2 ){
          memcpy(&idata[di], &idata[di-drawwidth], drawwidth*4);
          di += drawwidth;
        }
        break;

      }
             

    }
  }
  /*imodPrintStderr("1.5 wx %d wy %d sw %d sh %d window %d %d\n",
    wxdraw, wy, drawwidth, sh, sCurWidth, sCurHeight); */

  glPixelZoom((GLfloat)1.0f, (GLfloat)1.0f);

  glRasterPos2f((float)wxdraw, (float)wy);
  glDrawPixels(drawwidth, sh, format, type, sdata);
}

/* This is the display routine called from the zap window, for regular or
   high quality data */
/* DNM 1/20/02: Added slice argument to govern image re-use, rather than
   assuming CurZ */
void b3dDrawGreyScalePixelsHQ(unsigned char **dataPtrs,     // input data lines
                              int imXsize, int imYsize,     // size of input  
                              int imXoffset, int imYoffset, // data offsets into input
                              int winXstart, int winYstart, // window start for drawing
                              int imWidth, int imHeight,    // sub-area size of input data
                              B3dCIImage *image,            // tmp image data.
                              int base,                     // colorindex ramp base
                              double xzoom,
                              double yzoom,
                              int quality, int slice, int rgba)  
{
  float zoom = xzoom;
  if (!image)
    return;
  unsigned short *sdata = (unsigned short *)image->id1;
  unsigned char  *bdata = (unsigned char *)sdata;
  unsigned int   *idata = (unsigned int *)sdata;
  unsigned int   *rgbadata;
  unsigned char  *bidata, *bptr;
  float ival;
  unsigned char val;

  float a,b,c,d;
  int drawWidth = (int)(imWidth * zoom);
  int drawHeight= (int)(imHeight * yzoom);
  int xi, yi, pyi, nyi;
  int i, j, k, ky, izs, xistop, istop, bpxi, bxi, bnxi, bnxi2;
  int *xindex;
     
  double cx, cy;                            // current x,y values
  double zsx    = 1.0 / zoom;                 // zoom step for x
  double zsy    = 1.0 / yzoom;                // zoom step for y          
  float xtrans = -(float)(0.5 - (0.5 * zsx));       // zoom-dependent translation offset
  float ytrans = -(float)(0.5 - (0.5 * zsy));
  float xstop = imXoffset + imWidth + xtrans; // stop at this x coord.  
  float ystop = imYoffset + imHeight + ytrans; // stop at this y coord.
  unsigned short rbase = base;
  float pixmin = 0.0f, pixmax = 255.0f;
  GLenum type, format;
  GLint unpack = b3dGetImageType(&type, &format);
  unsigned int *cindex = App->cvi->cramp->ramp;
  unsigned char *bindex = App->cvi->cramp->bramp;
  int ibase;
  int nyi2;
  sCurXZoom = zoom;
  cubicFactors *cubFacs, *cf;
  cubicFactors nullFacs;
  unsigned char *yptr;
  b3dUInt16 *usptr;
  b3dUInt16 usval;
  float fy1, fy2, fy3, fy4;
  int zoomFilters[] = {5, 4, 1, 0};  // lanczos3, 2, Blackman, box
  int numZoomFilt = sizeof(zoomFilters) / sizeof(int);
  int zoomWidthCrit = 20;
  //double wallstart = wallTime();

  if (!dataPtrs){
    b3dColorIndex(base);
    glColor3f(0.0f, 0.0f, 0.0f);
    b3dDrawFilledRectangle(winXstart, winYstart, drawWidth, drawHeight);
    return;
  }
    
  /* special optimization. DNM: don't take if want quality*/
  if (!quality && (xzoom == 1.50) && (yzoom == 1.50)){
    b3dDrawGreyScalePixels15(dataPtrs, imXsize, imYsize, imXoffset, imYoffset,
                             winXstart, winYstart, imWidth, imHeight, image, base, slice, rgba);
    return;
  } 

  if (!quality){
        
    if (!App->wzoom){
      /* If relying on OpenGL zoom completely, just call this */
      b3dDrawGreyScalePixels(dataPtrs, imXsize, imYsize, imXoffset, imYoffset,
                             winXstart, winYstart, imWidth, imHeight, image, base,
                             xzoom, yzoom, slice, rgba);
      return;
    }else{
      if (xzoom >= 1.0 && (int)xzoom == xzoom){
        /* If it's an integer zoom (1 or more), then use the OpenGL
           zoom through this routine */
        b3dDrawGreyScalePixels(dataPtrs, imXsize, imYsize, 
                               imXoffset, imYoffset,
                               winXstart, winYstart, imWidth, imHeight, image, base,
                               xzoom, yzoom, slice, rgba);
        return;
      } else {
        /* DNM: encode fractional zooms as negative quality because
           they will be stored as single pixels without zoom, but
           the hq mode needs to be distinguished */
        quality = -1;
      }
    }
  }

  drawWidth = B3DMIN(sCurWidth, B3DMIN(image->width, drawWidth));
  drawHeight = B3DMIN(sCurHeight, B3DMIN(image->height, drawHeight));

  if (App->depth == 8){
    rbase = 0;
    pixmin = RAMPMIN;
    pixmax = RAMPMAX;
  }

  // DNM 11/26/07: eliminate side filling hack

  /* DNM: test and send quality rather than 1 so that it can tell whether
     fractional zooms are truly hq or not */

  if (!b3dImageMatch(image, &sdata, imXoffset, imYoffset, imWidth, imHeight, zoom, yzoom,
                     quality, slice)) {
    b3dImageSet(image, &sdata, imXoffset, imYoffset, imWidth, imHeight, zoom, yzoom,
                quality, slice); 

    bdata = (unsigned char *)sdata;
    idata = (unsigned int  *)sdata;
    if (quality > 0 && rgba > 0 && rgba < 4 && 
        ((zoom <= sZoomDownCrit && yzoom <= 1.) ||
         (yzoom <= sZoomDownCrit && zoom <= 1.))) {

      // Here for quality zoom down with antialias filters
      // Select the biggest filter that does not give too many lines, normalized to a
      // 512-pixel image with no threads
      for (i = 0; i < numZoomFilt; i++) {
        j = selectZoomFilterXY(zoomFilters[i], xzoom, yzoom, &k, &ky);
        if (!j && (B3DMAX(k, ky) * drawWidth * drawHeight) / (250000 * numOMPthreads(8))
            <= zoomWidthCrit)
          break;
      }
      if (!j) {
        //imodPrintStderr("Filter selector index %d\n", i);
        i = zoomWithFilter(dataPtrs, imXsize, imYsize, (float)imXoffset, (float)imYoffset,
                           drawWidth, drawHeight, drawWidth, 0,
                           rgba > 2 ? SLICE_MODE_RGB :
                           (rgba > 1 ? SLICE_MODE_USHORT : SLICE_MODE_BYTE), 
                           bdata, cindex, bindex);
        if (i)
          imodPrintStderr("Error from zoomWithFilter %d\n", i);
      }

    } else if (quality > 0 && rgba < 4) {

      /* For regular HQ step to each display pixel and use cubic interpolation at 
         each position. */

      // First set up cubic factors for each position in X
      cubFacs = B3DMALLOC(cubicFactors, drawWidth);
      if (cubFacs) {
        for (i = 0, cx = imXoffset + xtrans; i < drawWidth; cx += zsx, i++) {
          cf = &cubFacs[i];
          getCubicFactors(cx, imXsize, cf->pxi, cf->xi, cf->nxi, cf->nxi2, 
                          cf->fx1, cf->fx2, cf->fx3, cf->fx4);
        } 
      }

      // Loop on lines
#pragma omp parallel for \
  shared(drawHeight, imYoffset, xtrans, ytrans, zsx, zsy, imYsize, drawWidth, rgba, \
         imXoffset, cubFacs,                                            \
         bdata, sdata, idata, cindex, bindex, pixmax, pixmin, rbase, imXsize)   \
  private(j, cy, pyi, yi, nyi, nyi2, fy1, fy2, fy3, fy4, ibase, i, cx, cf, yptr, a, \
          b, c, d, val, ival, bpxi, bxi, bnxi, bnxi2, nullFacs, k, usptr, usval)
      for (j = 0; j < drawHeight; j++) {
        cy = imYoffset + ytrans + j * zsy;

        // Get cubic factors for the position in Y
        getCubicFactors(cy, imYsize, pyi, yi, nyi, nyi2, fy1, fy2, fy3, fy4);

        ibase = j * drawWidth;
        
        if (rgba < 3) {

          // Loop across the line
          for (i = 0, cx = imXoffset + xtrans; i < drawWidth; cx += zsx, i++) {
            
            // Do the cubic interpolation: fall back to computing the factors on
            // the fly
            if (cubFacs) {
              cf = &cubFacs[i];
            } else {
              cf = &nullFacs;
              getCubicFactors(cx, imXsize, cf->pxi, cf->xi, cf->nxi, cf->nxi2, 
                              cf->fx1, cf->fx2, cf->fx3, cf->fx4);
            }
            if (rgba == 2) {
              usptr = (b3dUInt16 *)dataPtrs[pyi];
              a = cf->fx1 * usptr[cf->pxi] + cf->fx2 * usptr[cf->xi] + 
                cf->fx3 * usptr[cf->nxi] + cf->fx4 * usptr[cf->nxi2];
              usptr = (b3dUInt16 *)dataPtrs[yi];
              b = cf->fx1 * usptr[cf->pxi] + cf->fx2 * usptr[cf->xi] + 
                cf->fx3 * usptr[cf->nxi] + cf->fx4 * usptr[cf->nxi2];
              usptr = (b3dUInt16 *)dataPtrs[nyi];
              c = cf->fx1 * usptr[cf->pxi] + cf->fx2 * usptr[cf->xi] + 
                cf->fx3 * usptr[cf->nxi] + cf->fx4 * usptr[cf->nxi2];
              usptr = (b3dUInt16 *)dataPtrs[nyi2];
              d = cf->fx1 * usptr[cf->pxi] + cf->fx2 * usptr[cf->xi] + 
                cf->fx3 * usptr[cf->nxi] + cf->fx4 * usptr[cf->nxi2];
              ival = fy1 * a + fy2 * b + fy3 * c + fy4 * d;

              if (ival > 65535)
                usval = 65535;
              else if (ival < 0)
                usval = 0;
              else 
                usval = (b3dUInt16)(ival + 0.5f);
              idata[i + ibase] = cindex[usval];
              val = (unsigned char)cindex[usval];
              //if (j < 20)printf("%d %d %d %d\n", i, j, usval, val);
              
            } else {
              yptr = dataPtrs[pyi];
              a = cf->fx1 * yptr[cf->pxi] + cf->fx2 * yptr[cf->xi] + 
                cf->fx3 * yptr[cf->nxi] + cf->fx4 * yptr[cf->nxi2];
              yptr = dataPtrs[yi];
              b = cf->fx1 * yptr[cf->pxi] + cf->fx2 * yptr[cf->xi] + 
                cf->fx3 * yptr[cf->nxi] + cf->fx4 * yptr[cf->nxi2];
              yptr = dataPtrs[nyi];
              c = cf->fx1 * yptr[cf->pxi] + cf->fx2 * yptr[cf->xi] + 
                cf->fx3 * yptr[cf->nxi] + cf->fx4 * yptr[cf->nxi2];
              yptr = dataPtrs[nyi2];
              d = cf->fx1 * yptr[cf->pxi] + cf->fx2 * yptr[cf->xi] + 
                cf->fx3 * yptr[cf->nxi] + cf->fx4 * yptr[cf->nxi2];
              ival = fy1 * a + fy2 * b + fy3 * c + fy4 * d;
              
              // Limit the value and put in the array
              if (ival > pixmax)
                val = (unsigned char)pixmax;
              else if (ival < pixmin)
                val = (unsigned char)pixmin;
              else 
                val = (unsigned char)(ival + 0.5f);
              
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
          }
        } else {
          
          // Loop across the line
          for (i = 0, cx = imXoffset + xtrans; i < drawWidth; cx += zsx, i++) {
            if (cubFacs) {
              cf = &cubFacs[i];
            } else {
              cf = &nullFacs;
              getCubicFactors(cx, imXsize, cf->pxi, cf->xi, cf->nxi, cf->nxi2, 
                              cf->fx1, cf->fx2, cf->fx3, cf->fx4);
            }
          
            for (k = 0; k < 3; k++) {
              bpxi = 3*cf->pxi+k;
              bxi = 3*cf->xi+k;
              bnxi = 3*cf->nxi+k;
              bnxi2 = 3*cf->nxi2+k;
              yptr = dataPtrs[pyi];
              a = cf->fx1 * yptr[bpxi] + cf->fx2 * yptr[bxi] +
                cf->fx3 * yptr[bnxi] + cf->fx4 * yptr[bnxi2];
              yptr = dataPtrs[yi];
              b = cf->fx1 * yptr[bpxi] + cf->fx2 * yptr[bxi] + 
                cf->fx3 * yptr[bnxi] + cf->fx4 * yptr[bnxi2];
              yptr = dataPtrs[nyi];
              c = cf->fx1 * yptr[bpxi] + cf->fx2 * yptr[bxi] + 
                cf->fx3 * yptr[bnxi] + cf->fx4 * yptr[bnxi2];
              yptr = dataPtrs[nyi2];
              d = cf->fx1 * yptr[bpxi] + cf->fx2 * yptr[bxi] + 
                cf->fx3 * yptr[bnxi] + cf->fx4 * yptr[bnxi2];
              ival = fy1 * a + fy2 * b + fy3 * c + fy4 * d;
            
              // Limit the value and put in the array
              if (ival > pixmax)
                val = (unsigned char)pixmax;
              else if (ival < pixmin)
                val = (unsigned char)pixmin;
              else 
                val = (unsigned char)(ival + 0.5f);
              bdata[4 * (i + ibase) + k] = bindex[val];
            }
            bdata[4 * (i + ibase) + 3] = 0;
          }
        }
      }
      if (cubFacs)
        free(cubFacs);

    } else {

      /* For low quality, non-integer zooms, use nearest neighbor
         interpolation at each pixel */
      /* DNM 3/4/03: make sure stop values are not too large */
      /* DNM 4/17/03: copy pixels on right if array is not full */
      if (int(xstop + 0.5) >= imXsize)
        xstop = imXsize - 1.5;
      if (int(ystop + 0.5) >= imYsize)
        ystop = imYsize - 1.5;

      /* Get an integer step size and see if it is close enough;
         if so, use integer steps and avoid floating calculation below */
      izs = (int)(zsx + 0.5);
      if (fabs(izs - zsx) > 0.002)
        izs = 0;
      else
        sCurXZoom = 1. / izs;

      /* DNM 7/6/07: Need an integer stopping value for integer steps */
      xistop = (int)(imXoffset + xtrans + 0.5) + drawWidth * izs;
      if (xistop > imXsize)
        xistop = imXsize;

      /* DNM 8/1/08: precompute the X indexes */
      if (!izs) {
        xindex = B3DMALLOC(int, drawWidth + 4);
        if (!xindex)
          return;
        for (i = 0, cx = imXoffset + xtrans; cx < xstop && i < drawWidth; cx += zsx, i++)
          xindex[i] = (int)(cx + 0.5);
        istop = i;
      }

      for (j = 0, cy = imYoffset + ytrans; cy < ystop && j < drawHeight; cy += zsy, j++) {
        yi = (int)(cy + 0.5);
        ibase = j * drawWidth;
        xi = (int)(imXoffset + xtrans + 0.5);
        switch(unpack) {
        case 1:
          if (izs) {
            for (i = 0; xi < xistop; xi += izs, i++)
              bdata[i + ibase] = dataPtrs[yi][xi];
          } else {
            for (i = 0; i < istop; i++) 
              bdata[i + ibase] = dataPtrs[yi][xindex[i]];
          }
          for (; i && i < drawWidth; i++)
            bdata[i + ibase] =  bdata[i + ibase - 1];
          break;

        case 2:
          if (izs) {
            for (i = 0; xi < xistop; xi += izs, i++)
              sdata[i + ibase] = dataPtrs[yi][xi] + rbase;
          } else {
            for (i = 0; i < istop; i++) 
              sdata[i + ibase] = dataPtrs[yi][xindex[i]] + rbase;
          }
          for (; i && i < drawWidth; i++)
            sdata[i + ibase] =  sdata[i + ibase - 1];
          break;

        case 4:
          switch (rgba) {
          case 1:   /* lookup from bytes */
            bdata = dataPtrs[yi];
            if (izs) {
              for (i = 0; xi < xistop; xi += izs, i++)
                idata[i + ibase] = cindex[bdata[xi]];
            }  else {
              for (i = 0; i < istop; i++) 
                idata[i + ibase] = cindex[bdata[xindex[i]]];
            }
            break;

          case 2:   /* lookup from ushorts */
            usptr = (b3dUInt16 *)dataPtrs[yi];
            if (izs) {
              for (i = 0; xi < xistop; xi += izs, i++)
                idata[i + ibase] = cindex[usptr[xi]];
            }  else {
              for (i = 0; i < istop; i++) 
                idata[i + ibase] = cindex[usptr[xindex[i]]];
            }
            break;

          case 3:   /* copy RGB data to RGBA */
            bidata = (unsigned char *)&(idata[ibase]);
            bdata = dataPtrs[yi];
            if (izs) {
              for (i = 0; xi < xistop; xi += izs, i++) {
                bptr = bdata + 3 * xi;
                *bidata++ = bindex[*bptr++];
                *bidata++ = bindex[*bptr++];
                *bidata++ = bindex[*bptr++];
                *bidata++ = 0;
              }
            } else {
              for (i = 0; i < istop; i++) {
                bptr = bdata + 3 * xindex[i];
                *bidata++ = bindex[*bptr++];
                *bidata++ = bindex[*bptr++];
                *bidata++ = bindex[*bptr++];
                *bidata++ = 0;
              }
            }
            break;

          case 4:   /* untested placekeeper fro RGBA data */
            rgbadata = (unsigned int *)dataPtrs[yi];
            if (izs) {
              for (i = 0; xi < xistop; xi += izs, i++)
                idata[i + ibase] = rgbadata[xi];
            } else {
              for (i = 0; i < istop; i++)
                idata[i + ibase] = rgbadata[xindex[i]];
            }
            break;
          }

          for (; i && i < drawWidth; i++)
            idata[i + ibase] =  idata[i + ibase - 1];
          break;
        }
      }
      
      /* Fill the top if necessary */
      for (; j && j < drawHeight; j++) {
        bdata = (unsigned char *)sdata + unpack * j * drawWidth;
        memcpy(bdata, bdata - unpack * drawWidth, unpack * drawWidth);
      }
      if (!izs)
        free(xindex);
    }
    //imodPrintStderr("Array fill time %.4f\n", wallTime() - wallstart);
  }
  /* imodPrintStderr("HQ wx %d wy %d dwidth %d dheight %d window %d %d\n",
    winXstart, winYstart, drawWidth, drawHeight, sCurWidth, sCurHeight);*/

  glPixelZoom((GLfloat)1.0f, (GLfloat)1.0f); 
  glRasterPos2f((float)winXstart, (float)winYstart);
  glDrawPixels(drawWidth, drawHeight, format, type, sdata); 
}

static void getCubicFactors(double cx, int xsize, int &pxi, int &xi, int &nxi,
                            int &nxi2, float &fx1, float &fx2, float &fx3,
                            float &fx4)
{
  float dx, dxm1, dxdxm1;
  xi = (int)cx;
  xi = B3DMIN(xsize - 1, B3DMAX(0, xi));
  pxi = B3DMAX(0, xi - 1);
  nxi = B3DMIN(xsize - 1, xi + 1);
  nxi2 = B3DMIN(xsize - 1, xi + 2);
  dx = (float)(cx - xi);
  dxm1 = dx - 1.f;
  dxdxm1 = dx * dxm1;
  fx1 = -dxm1 * dxdxm1;
  fx4 = dx * dxdxm1;
  fx2 = 1.f + dx * dx * (dx - 2.f);
  fx3 = dx * (1.f - dxdxm1);
}

double b3dStepPixelZoom(double czoom, int step)
{
  double *zv = ImodPrefs->getZooms();
  int i=0;
  int zoomi;

  /* DNM: czoom has been stored as a float by caller.  Change it a bit in the
    desired direction to avoid problems under Windows - instead of former
    approach of casting to floats */
  czoom += step * 0.001;
  if (step > 0) {
    for(i = 0; i < MAXZOOMS - 1; i++)
      if (zv[i] > czoom) break;
    zoomi = (i - 1) + step;

  } else {
          
    for (i = MAXZOOMS - 1; i >= 0; i--)
      if (zv[i] < czoom) break;
    zoomi = (i + 1) + step;
  }

  if (zoomi < 0) zoomi = 0;
  if (zoomi >= MAXZOOMS) zoomi = MAXZOOMS - 1;
  /* imodPrintStderr ("%f %d %d %f\n", czoom, step, zoomi, zv[zoomi]); */
  return(zv[zoomi]);
}



/*****************************************************************************/
/* Image Save Functions. */

/***********************************/
/*  Save window image to rgb file. */

/* DNM 12/15/02 : eliminated unused iput functions, identical to ones in
   mv_gfx.cpp */

// So that movie snapshots can avoid checking file number from 0
void b3dSetMovieSnapping(bool snapping)
{
  sMovieSnapping = snapping;
}

// So that montage snapshots can multiply the resolution value if desired
void b3dSetDpiScaling(float factor)
{
  sDpiScaling = ImodPrefs->scaleSnapDPI() ? factor : 1.;
}

/*
 * Set the snapshot directory, starting in previous one if any
 */
void b3dSetSnapDirectory(void)
{
  QString dir = QDir::currentPath();
  if (!sSnapDirectory.isEmpty())
    dir = sSnapDirectory;
  sSnapDirectory = QFileDialog::getExistingDirectory
    (NULL, "Directory to Save Snapshots to", dir);
}

/*
 * Create a filename in fname with the prefix in name, based on the
 * format_type, and with at least the given number of digits.  The starting
 * candidate file number is in fileno.  If the previous file number exists it
 * will start checking for free file name from this number, but if previous
 * file number does not exist it will start checking from 0.
 */
QString b3dGetSnapshotName(const char *name, int format_type, int digits,
                        int &fileno)
{
  char format[14];
  QString snapFormat, fext, fname;
  QString dir = sSnapDirectory;
  char sep = QDir::separator().toLatin1();
  if (!dir.isEmpty() && (dir.lastIndexOf(sep) != dir.length() - 1))
    dir += sep;
  sprintf(format, "%%s%%s%%0%dd.%%s", digits);
  bool firstCheck = false;

  switch(format_type){
  case SnapShot_RGB:
    snapFormat = ImodPrefs->snapFormat();
    if (snapFormat == "JPEG")
      snapFormat = "JPG";
    fext = snapFormat.toLower();
    break;
  case SnapShot_TIF:
    fext = "tif";
    break;
  default:
    fext = "image";
  }

  // If file number is not zero, the first check is on the previous file
  if (fileno) {
    firstCheck = true;
    fileno--;
  }

  // Loop until a file is found that does not exist
  for (;;) {
    if (fileno < (int)pow(10., double(digits)))
      fname.sprintf(format, LATIN1(dir), name, fileno++, LATIN1(fext));
    else
      fname.sprintf("%s%s%d.%s", LATIN1(dir), name, fileno++, LATIN1(fext));

    // If file does not exist and it is the first check on previous file,
    // set file number to 0 to start from 0.  Otherwise, take this file
    if (!QFile::exists(QDir::convertSeparators(fname))) {
      if (firstCheck) {
        fileno = 0;
        firstCheck = false;
      } else
        break;
    }
    firstCheck = false;
  }
  return fname;
}

QString b3dShortSnapName(QString fname)
{
  QString sname = QDir::convertSeparators(fname);
  char sep = QDir::separator().toLatin1();
  int index = sname.lastIndexOf(sep);
  if (index > 1) {
    index = sname.lastIndexOf(sep, index - 1);
    if (index >= 0)
      sname = sname.replace(0, index, "...");
  }
  return sname;
}

/* Take a snapshot of the current window with prefix in name.  There is
   no default for checking for RGB conversion. */
int b3dAutoSnapshot(const char *name, int format_type, int *limits, 
                    bool checkConvert)
{
  QString fname, sname;
  static int fileno = 0;
  int retval;

  // Reset the file number to 0 unless doing movie snapshots
  if (!sMovieSnapping)
    fileno = 0;

  fname = b3dGetSnapshotName(name, format_type, 3, fileno);
  sname = b3dShortSnapName(fname);

  wprint("%s: Saving image to %s\n", name, LATIN1(sname));

  switch (format_type){
  case SnapShot_RGB:
    retval = b3dSnapshot_NonTIF(fname, App->rgba, limits, NULL);
    break;
  case SnapShot_TIF:
    retval = b3dSnapshot_TIF(fname, App->rgba, limits, NULL, checkConvert);
    break;
  default:
    retval = b3dSnapshot(fname);
  }
  if (retval)
    wprint("Error!\n");
  else
    wprint("DONE!\n");
  return retval;
}

int b3dSnapshot(QString fname)
{
  if (sSnapShotFormat == SnapShot_RGB)
    return(b3dSnapshot_NonTIF(fname, App->rgba, NULL, NULL));
  else
    return(b3dSnapshot_TIF(fname, App->rgba, NULL, NULL, false));
}

/* Take a snapshot of the current window with prefix in name and type selected
   by shift and ctrl key states.  Checking for RGB conversion to gray scale
   is defaulted to TRUE. */
int b3dKeySnapshot(const char *name, int shifted, int ctrl, int *limits,
                   bool checkConvert)
{
  int retval;
  if (shifted) {
    if (ctrl)
      ImodPrefs->set2ndSnapFormat();
    retval = b3dAutoSnapshot(name, SnapShot_RGB, limits, checkConvert);
    if (ctrl)
      ImodPrefs->restoreSnapFormat();
  } else
    retval = b3dAutoSnapshot(name, SnapShot_TIF, limits, checkConvert);
  return retval;
}

/* Takes a snapshot to a file with name supplied in fname, or if fname is empty, it
 * uses the prefix to compose a name and returns that name in fname.  Limits are applied
 * if non-null; there is no default for checking conversion */
int b3dNamedSnapshot(QString &fname, const char *prefix, int format, int *limits,
                     bool checkConvert)
{
  QString sname;
  static int fileno = 0;
  int retval;
  bool isModv = strcmp(prefix, "modv") == 0;

  if (fname.isEmpty())
    fname = b3dGetSnapshotName(prefix, format, isModv ? 4 : 3, fileno);
  sname = b3dShortSnapName(fname);
  
  if (format == SnapShot_RGB)
    retval = b3dSnapshot_NonTIF(fname, App->rgba, limits, NULL);
  else if (format == SnapShot_TIF)
    retval = b3dSnapshot_TIF(fname, App->rgba, limits, NULL, checkConvert);
  else
    return 1;
  if (retval) {
    if (isModv)
      imodPrintStderr("Error saving snapshot!\n");
    else
      wprint("\aError saving snapshot!\n");
  } else {
    if (isModv)
      imodPrintStderr("Saved image to %s\n", LATIN1(sname));
    else
      wprint("Saved image to %s\n", LATIN1(sname));
  }
  return retval;
}

/* Sets the Non-tiff format to a specified type, PNG or JPG.  Returns 0 if it is already
 * the snap format, 1 if it is the second format and needs to be reset when done, or -1
 * if it does not match the first or second format */
int b3dSetNonTiffSnapFormat(int format)
{
  QString snap1 = ImodPrefs->snapFormat();
  QString snap2 = ImodPrefs->snapFormat2();
  if ((format == SnapShot_PNG && snap1 == "PNG") || 
      (format == SnapShot_JPG && (snap1 == "JPEG" || snap1 == "JPG")))
    return 0;
  if ((format == SnapShot_PNG && snap2 == "PNG") || 
      (format == SnapShot_JPG && (snap2 == "JPEG" || snap2 == "JPG"))) {
    ImodPrefs->set2ndSnapFormat();
    return 1;
  }
  return -1;
}

/* DNM 12/24/00 changed long length, long offset to types below, to prevent
   compiler warnings on SGI */
static void puttiffentry(short tag, short type, 
                         int length, unsigned int offset, FILE *fout)
{
  fwrite(&tag, sizeof(b3dInt16), 1, fout);
  fwrite(&type, sizeof(b3dInt16), 1, fout);
  fwrite(&length, sizeof(b3dInt32), 1, fout);

  /* DNM: change __vms to LITTLE_ENDIAN to work on PC */
#ifndef B3D_LITTLE_ENDIAN
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
  fwrite(&offset, sizeof(b3dInt32), 1, fout);
  return;
}

/*
 * Snapshot from current GL window into RGB or other non-TIFF file format
 * Output file is given by fname
 * rgbmode non-zero indicates rgb rather than color index
 * if limits is non-NULL, it contains lower left X, Y and size X,Y for a subset
 * If data is non-NULL, it supplies the data already as line pointers, and
 * rgbmode should be 3 for RGB and 4 for RGBA data.
 */
int b3dSnapshot_NonTIF(QString fname, int rgbmode, int *limits, 
                       unsigned char **data)
{
  FILE *fout = NULL;
  int i;
  unsigned char *pixels = NULL;
  b3dInt32 xysize;

  b3dUInt32 *rgbout;
  unsigned char *pixin;
  int j, loop;
  bool imret = true;
  unsigned char *pixout, tmp;
  bool transBkgd = fname.startsWith("modv") && Imodv->transBkgd;
  int rpWidth;
  int rpHeight;
  QString format = ImodPrefs->snapFormat();
  int resolution = B3DNINT(sDpiScaling * ImodPrefs->snapDPI() / 0.0254);

  if (snapshotCommon(fname, &fout, format == "RGB", rgbmode, limits, data ? 1 : 0,
                     &pixels, rpWidth, rpHeight))
    return 1;

  pixout = (unsigned char *)pixels;
  xysize = rpWidth * rpHeight;

  if (data) {

    // For passed-in line pointers, repack RGB or RGBA
    for (j = 0; j < rpHeight; j++) {
      for (i = 0; i < rpWidth; i++) {
        *pixout++ = data[j][rgbmode*i];
        *pixout++ = data[j][rgbmode*i+1];
        *pixout++ = data[j][rgbmode*i+2];
        *pixout++ = 0;
      }
    }
  }

  // Legacy RGB, call routine
  if (fout) {

    /* have to swap bytes and fill in 0 for A, because RGB routine wants
       ABGR.  Or put in the alpha value for transparent background. */
    for (i = 0; i < xysize; i++) {
      tmp = transBkgd ? pixout[3] : 0;
      pixout[3] = pixout[0];
      pixout[0] = tmp;
      pixout++;
      tmp = *pixout;
      *pixout = pixout[1];
      pixout[1] = tmp;
      pixout+=3;
    }
    bdRGBWrite(fout, (int)rpWidth, (int)rpHeight, pixels);
    fclose(fout);
  } else {

    // Qt image writing: need to reorder lines as well as encode into rgb's
    for (j = 0; j < (rpHeight + 1) / 2; j++) {
      rgbout = (b3dUInt32 *)pixels + (rpHeight - 1 - j) * rpWidth;
      pixin = pixels + 4 * j * rpWidth;
      memcpy(pixels + 4 * xysize, rgbout, 4 * rpWidth);
      for (loop = 0; loop < 2; loop++) {
        if (transBkgd) {
          for (i = 0; i < rpWidth; i++) {
            *rgbout++ = qRgba(pixin[0], pixin[1], pixin[2], pixin[3]);
            pixin += 4;
          }
        } else {
          for (i = 0; i < rpWidth; i++) {
            *rgbout++ = qRgb(pixin[0], pixin[1], pixin[2]);
            pixin += 4;
          }
        }
        pixin = pixels + 4 * xysize;
        rgbout = (b3dUInt32 *)pixels + j * rpWidth;
      }
    } 

    // Save the image with the given format and quality (JPEG only)
    QImage *qim = new QImage(pixels, rpWidth, rpHeight, 
                             transBkgd ? QImage::Format_ARGB32 : QImage::Format_RGB32);
    if (transBkgd)
      imodPrintStderr("   with transparent background");
    qim->setDotsPerMeterX(resolution);
    qim->setDotsPerMeterY(resolution);
    j = format == "JPEG" ? ImodPrefs->snapQuality() : -1;
    imret = qim->save(fname, LATIN1(format), j);
    delete qim;
  }
  free(pixels);

  return imret ? 0 : 1;
}

/*
 * Snapshot from current GL window into a TIFF file given by fname 
 * rgbmode non-zero indicates rgb rather than color index
 * if limits is non-NULL, it contains lower left X, Y and size X,Y for a subset
 * If data is non-NULL, it supplies the data already as line pointers, and
 * rgbmode should be 3 for RGB and 4 for RGBA data.
 */
int b3dSnapshot_TIF(QString fname, int rgbmode, int *limits, 
                     unsigned char **data, bool checkConvert)
{
  FILE *fout;
  int i, j, xsize, ysize;
  unsigned char *pixels = NULL;
  int *lpixels;
  int step, samples, resolution, extra;
  unsigned int pixel;
  unsigned int xysize, ifd;
  unsigned int bitsPerSample, resOffset;
  unsigned char bpix;
  unsigned short tenum;
  unsigned short color[3];

  // 10/16/13: Eliminated depth variable, it was basically wrong
  bool convertRGB = false;

  int rpWidth = sCurWidth;
  int rpHeight = sCurHeight;

  if (snapshotCommon(fname, &fout, true, rgbmode, limits, data ? -1 : 0,
                     &pixels, rpWidth, rpHeight))
    return 1;

  if (checkConvert)
    convertRGB = App->convertSnap;
  resolution = B3DNINT(1000. * sDpiScaling * ImodPrefs->snapDPI());

  xysize = rpWidth * rpHeight;
  xsize = rpWidth;
  ysize = rpHeight;

  ifd = xysize;
  if (ifd < ((double)xsize * ysize) - 20.) {
    imodPrintStderr("The image is too large to save into a TIFF file");
    B3DFREE(pixels);
    return 1;
  }
  if (!convertRGB) {
    ifd *= 3;
    if (ifd < ((double)xsize * ysize) * 3 - 20.) {
      imodPrintStderr("The color image is too large to save into a TIFF file");
      B3DFREE(pixels);
      return 1;
    }
  }

  // 11/1/04: fix to allow 3 more words (2 front of the image, 1 after)
  i = ifd + 12;
  ifd = 4 * ((ifd + 3) / 4 + 3);
  extra = ifd - i;

  /* DNM: change __vms to LITTLE_ENDIAN to work on PC */
#ifdef B3D_LITTLE_ENDIAN
  pixel = 0x002A4949;
#else
  pixel = 0x4D4D002A;
#endif

  if (!fwrite(&pixel, 4, 1, fout)){
    fclose(fout);
    B3DFREE(pixels);
    return 1;
  }

      
  if (!fwrite(&ifd, 4, 1, fout)){
    fclose(fout);
    B3DFREE(pixels);
    return 1;
  }

  if (data) {

    /* Use the line pointers to write each line from data array */
    step = rgbmode == 3 ? 3 : 4;
    for (j = ysize - 1; j >= 0; j--) {
      if (convertRGB) {
        for (i = 0; i < xsize; i++) {
          bpix = (int)(0.3 * data[j][step*i] + 0.59 * data[j][step*i+1] + 
                       0.11 * data[j][step*i + 2] + 0.5);
          fwrite(&bpix, 1, 1, fout);
        }
      } else if (rgbmode == 3) {
        fwrite(data[j], 1, 3 * xsize, fout);
      } else {
        for (i = 0; i < xsize; i++)
          fwrite(&data[j][4*i], 1, 3, fout);
      }
    }
  } else {

    // Write data from the snapshot
    lpixels = (int *)pixels;
    for (j = ysize - 1; j >= 0; j--) {
      if (convertRGB) {
        for (i = 0; i < xsize; i++) {
          bpix = (int)(0.3 * pixels[4*(i+(j*xsize))] + 
                       0.59 * pixels[4*(i+(j*xsize)) + 1] + 
                       0.11 * pixels[4*(i+(j*xsize)) + 2] + 0.5);
          fwrite(&bpix, 1, 1, fout);
        }
      } else {
        for (i = 0; i < xsize; i++)
          fwrite(&lpixels[i + (j * xsize)], 1, 3, fout);
      }
    }
  } 
  
  pixel = 0;
  fwrite(&pixel, 4, 1, fout);

  /* Instead of seeking, which is limited to signed offsets, just get the
     offsets right and keep on writing.  Note that b3dFseek/mrc_big_seek did
     NOT work to seek past the end of the file the way fseek does (at least on
     Windows. */
  if (extra > 0)
    fwrite(&pixel, extra, 1, fout);
  //imodPrintStderr("Tell %d  ifd %u\n", ftell(fout), ifd);
  //fseek(fout, (int)ifd, SEEK_SET);

  tenum = 11;
  if (resolution > 0)
    tenum += 3;
  resOffset = ifd + 2 + (tenum * 12) + 4;   // Was + 7 for unknown reason
  samples = 1;
  bitsPerSample = 8;
  if (!convertRGB) {
    samples = 3;
    bitsPerSample = resOffset;
    resOffset += 6;
  }
  fwrite(&tenum, 2, 1, fout);
  puttiffentry(256, 3, 1, xsize, fout);
  puttiffentry(257, 3, 1, ysize, fout);
  puttiffentry(258, 3, samples, bitsPerSample, fout);
  puttiffentry(259, 3, 1, 1, fout);
  puttiffentry(262, 3, 1, convertRGB ? 1 : 2, fout);
  puttiffentry(273, 4, 1, 8, fout);
  puttiffentry(274, 3, 1, 1, fout);
  puttiffentry(277, 3, 1, samples, fout);
  puttiffentry(278, 4, 1, ysize, fout);
  puttiffentry(279, 4, 1, xysize * samples, fout);
  if (resolution > 0) {
    puttiffentry(282, 5, 1, resOffset, fout);
    puttiffentry(283, 5, 1, resOffset + 8, fout);
  }
  puttiffentry(284, 3, 1, 1, fout);  /* plane config */
  if (resolution > 0)
    puttiffentry(296, 3, 1, 2, fout);   /* Resolution units inches */
  ifd = 0;
  fwrite(&ifd, 4, 1, fout);

  if (!convertRGB) {
    //imodPrintStderr("Tell %d  bits %u\n", ftell(fout), bitsPerSample);
    //fseek(fout, (int)bitsPerSample, SEEK_SET);
    color[0] = 8;
    fwrite(color, 2, 1, fout);
    fwrite(color, 2, 1, fout);
    fwrite(color, 2, 1, fout);
  }

  /// 10/16/13: Got rid of code for writing color map and color indexed file
      
  if (resolution > 0) {
    //imodPrintStderr("Tell %d  res %u\n", ftell(fout), resOffset);
    //fseek(fout, (int)resOffset, SEEK_SET);
    ifd = 1000;
    fwrite(&resolution, 4, 1, fout);
    fwrite(&ifd, 4, 1, fout);
    fwrite(&resolution, 4, 1, fout);
    fwrite(&ifd, 4, 1, fout);
  }
  fclose(fout);
  B3DFREE(pixels);
  return 0;
}

/*
 * Common function for opening a file, allocating an array, and snapshotting the current
 * image.  If openFile is true, provide the filename in fname and the file pointer will 
 * be returned in fout.  If limits is non-null, it will be used to set the limits of a
 * subarea.  If dataPassed is negative, the routine will return after setting limits and
 * opening a file; if it is positive, it will allocate the array and then return.  The
 * data array is returned in pixelsOut and the width and height in rpWidth and rpHeight.
 */
static int snapshotCommon(QString fname, FILE **fout, bool openFile, int rgbmode,
                          int *limits, int dataPassed, unsigned char **pixelsOut,
                          int &rpWidth, int &imHeight)
{
  unsigned char *pixels = NULL;
  b3dInt32 xysize;
  int mapsize;
  b3dUInt32 *fcmapr, *fcmapg, *fcmapb;
  b3dUInt32 *cindex, ci;
  int i, filledSize, filledHeight, singleSize, numSingles, nbLine;
  int rpx = 0; 
  int rpy = 0;
  unsigned char *pixout;
  int minSingleSize = 10;
  int minFilledSize = 14;
  int maxFontSize = 24;
  int optimalLines = 3;
  int rpHeight = sCurHeight;
  int heightSum = 0;
  int textBorder = 5;

  rpWidth = sCurWidth;

  // Open file if desired
  errno = 0;
  if (openFile) {
    *fout = fopen(LATIN1(QDir::convertSeparators(fname)), "wb");
    if (!*fout) {
      QString qerr = "Snapshot: error opening file\n";
      if (errno)
        qerr +=  QString("System error: ") + QString(strerror(errno));
      imodPrintStderr(LATIN1(qerr));
      return 1;
    }
  }

  // Apply limits if any, get pixel array
  if (limits) {
    rpx = limits[0];
    rpy = limits[1];
    rpWidth = limits[2];
    rpHeight = limits[3];
  }

  // If captions have been set, first analyze how much vertical space they need
  if (!dataPassed && sSnapCaptions.size() > 0) {
    QFont font;
    font.setStyleHint(QFont::SansSerif);
    font.setItalic(false);
    numSingles = sSnapCaptions.size() - (sWrapLastCaption ? 1 : 0);

    // For single lines, start at the maximum font size and go down until the length of
    // all strings fits in the width of the image, or minimum size is reached
    for (singleSize = maxFontSize; singleSize >= minSingleSize; singleSize--) {
      font.setPixelSize(singleSize);
      QFontMetrics metric(font);
      bool allOK = true;
      for (i = 0; i < numSingles && allOK; i++)
        allOK = metric.width(sSnapCaptions[i]) <= rpWidth - 2 * textBorder;
      heightSum = numSingles * metric.height();
      if (singleSize == minSingleSize || allOK)
        break;
    }
    if (sWrapLastCaption) {

      // For the filled caption line, start at that size and try sizes until the number
      // of lines does not exceed the optimal number
      for (filledSize = B3DMAX(singleSize, minFilledSize); filledSize >= minFilledSize; 
           filledSize--) {
        font.setPixelSize(filledSize);
        QFontMetrics metric(font);
        QRect bound = metric.boundingRect(0, 0, rpWidth -2 * textBorder, 3000, 
                                          Qt::TextWordWrap, sSnapCaptions[numSingles]);
        filledHeight = bound.height();
        if (filledSize == minFilledSize || 
            filledHeight < (optimalLines + 0.5) * metric.lineSpacing())
          break;
      }
      heightSum += filledHeight;
    }
    heightSum += 3 * textBorder;
  }

  // Now fix the height of the image array and allocate it
  nbLine = rpWidth * 4;
  imHeight = rpHeight + heightSum;
  if (dataPassed < 0)
    return 0;

  pixels = (unsigned char *)malloc(rpWidth * (imHeight + 1) * 4);
  *pixelsOut = pixels;
  if (!pixels) {
    if (openFile)
      fclose(*fout);
    return 1;
  }

  if (dataPassed)
    return 0;

  // For captions, assign this array to a QImage and get a painter for it
  if (heightSum) {
    QImage *qim = new QImage(pixels, rpWidth, heightSum, QImage::Format_RGB32);
    int ypos = textBorder;
    QPainter *painter = new QPainter(qim);
    QFont font;
    font.setStyleHint(QFont::SansSerif);
    font.setItalic(false);

    // Draw black on white background
    painter->setPen(QPen(QColor(0, 0, 0)));
    painter->fillRect(0, 0, rpWidth, heightSum, QColor(255, 255, 255));

    // Draw single-line captions first; use rectangles for consistency in specifying
    // start coordinates, which are at top of rectangle (with just a position given,
    // it specifies the baseline of non-descending letters)
    font.setPixelSize(singleSize);
    painter->setFont(font);
    int spacing = (painter->fontMetrics()).lineSpacing();
    for (i = 0; i < numSingles; i++) {
      painter->drawText(textBorder, ypos, rpWidth - 2 * textBorder, spacing, 0,
                        sSnapCaptions[i]);
      ypos += spacing;
    }

    // Draw the filled rectangle
    if (sWrapLastCaption) {
      font.setPixelSize(filledSize);
      painter->setFont(font);
      ypos += textBorder;
      painter->drawText(textBorder, ypos,// + (painter->fontMetrics()).lineSpacing(), 
                        rpWidth - 2 * textBorder, filledHeight,
                       Qt::TextWordWrap, sSnapCaptions[numSingles]);
    }
    delete painter;
    delete qim;

    // Flip the lines
    for (i = 0; i < (heightSum + 1) / 2; i++) {
      memcpy(pixels + nbLine * heightSum, pixels + nbLine * i, nbLine);
      memcpy(pixels + nbLine * i, pixels + nbLine * (heightSum - i - 1), nbLine);
      memcpy(pixels + nbLine * (heightSum - i - 1), pixels + nbLine * heightSum, nbLine);
    }
    sSnapCaptions.clear();
  }

  pixels += nbLine * heightSum;
  glPixelZoom(1.0, 1.0);
  xysize = rpWidth * rpHeight;
  pixout = (unsigned char *)pixels;
  glFlush();

  // RGB mode snapshot is simple
  if (rgbmode) {
    glReadPixels(rpx, rpy, rpWidth, rpHeight, GL_RGBA, GL_UNSIGNED_BYTE, pixels);
    glFlush();

  } else {

    // Color index mode: get tables and read image as indices
    // Qt color map is/was 256 colors
    mapsize = 256;
    fcmapr = B3DMALLOC(unsigned int, mapsize+1);
    fcmapg = B3DMALLOC(unsigned int, mapsize+1);
    fcmapb = B3DMALLOC(unsigned int, mapsize+1);
    if (!fcmapr || !fcmapg || !fcmapb) {
      free(pixels);
      B3DFREE(fcmapr);
      B3DFREE(fcmapg);
      B3DFREE(fcmapb);
      if (openFile)
        fclose(*fout);
      return 1;
    }
    
    for(i = 0; i < mapsize; i++){
      QColor qcolor = App->qColormap->entryColor(i);
      fcmapr[i] = qcolor.red();
      fcmapg[i] = qcolor.green();
      fcmapb[i] = qcolor.blue();
    }

    glReadPixels(rpx, rpy, rpWidth, rpHeight, GL_COLOR_INDEX, GL_UNSIGNED_INT, pixels);
    glFlush();
    cindex = (b3dUInt32 *)pixels;
    for (i = 0; i < xysize; i++, cindex++){
      ci = *cindex;
      if (ci > mapsize) 
        ci = 0;
      *pixout++ = fcmapr[ci];
      *pixout++ = fcmapg[ci];
      *pixout++ = fcmapb[ci];
      *pixout++ = 0;
    }
    free(fcmapr);
    free(fcmapg);
    free(fcmapb);
  }
  return 0;
}

// Set the captions for the next snapshot
void b3dSetSnapshotCaption(QStringList &captionLines, bool wrapLastLine)
{
  sSnapCaptions = captionLines;
  sWrapLastCaption = wrapLastLine;
}
