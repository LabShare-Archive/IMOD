/*  b3dgfx.c -- Custom graphics routines for OpenGL.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include <qglcolormap.h>
#include <qdir.h>
#include <qfiledialog.h>
#include <qimage.h>
#include "imod.h"
#include "b3dgfx.h"
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
static void getCubicFactors(double cx, int xsize, int &pxi, int &xi, int &nxi,
                            int &nxi2, float &fx1, float &fx2, float &fx3,
                            float &fx4);

// Structure to hold factors for cubic interpolation
typedef struct cubic_factors
{
  int pxi, xi, nxi, nxi2;
  float fx1, fx2, fx3, fx4;
} cubicFactors;


static int          CurWidth;
static int          CurHeight;
static float        CurXZoom;

/* Set a current window size for the drawing routines */
void b3dSetCurSize(int width, int height)
{
  CurWidth = width;
  CurHeight = height;
}

float b3dGetCurXZoom() { return CurXZoom;}

/*
 * Set the viewport to the entire window of the given size, with 1:1 
 * transformation in drawing
 */
void b3dResizeViewportXY(int winx, int winy)
{
  glViewport((GLint)0, (GLint)0, (GLsizei)winx, (GLsizei)winy);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  glOrtho(0.0 , (GLdouble)winx, 0.0, (GLdouble)winy, 0.5, -0.5);
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
  glViewport((GLint)xstart, (GLint)ystart, (GLsizei)xsize, (GLsizei)ysize);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho((GLdouble)xstart, (GLdouble)(xstart + xsize), (GLdouble)ystart,
          (GLdouble)(ystart + ysize), 0.5, -0.5);
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

  return;
}

void b3dLineWidth(int width)
{
  glLineWidth(width);

  return;
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

  return;
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

  return;
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
  glBegin(GL_LINE_LOOP);
  glVertex2i(x, y + size);
  glVertex2i(x + size, y - (size/2));
  glVertex2i(x - size, y - (size/2));
  glEnd();
  return;
}

void b3dDrawFilledTriangle(int x, int y, int size)
{
  glBegin(GL_POLYGON);
  glVertex2i(x, y + size);
  glVertex2i(x + size, y - (size/2));
  glVertex2i(x - size, y - (size/2));
  glVertex2i(x, y + size);
  glEnd();
  return;
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

  return;
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
  return;
}

void b3dDrawLine(int x1, int y1, int x2, int y2)
{
  glBegin(GL_LINES);
  glVertex2i(x1, y1);
  glVertex2i(x2, y2);
  glEnd();
  return;
}

void b3dDrawSquare(int x, int y, int size)
{
  b3dDrawRectangle(x-(size/2), y-(size/2), size, size);
  return;
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
  return;
}
void b3dDrawFilledSquare(int x, int y, int size)
{
  b3dDrawFilledRectangle(x-(size/2), y-(size/2), size, size);
  return;
}
void b3dDrawFilledRectangle(int x, int y, int width, int height)
{
  glBegin(GL_POLYGON);
  glVertex2i(x,y);
  glVertex2i(x+width,y);
  glVertex2i(x+width,y+height);
  glVertex2i(x,y+height);
  glEnd();
  return;
}

void b3dBeginLine(void)
{
  glBegin(GL_LINE_STRIP);
  return;
}

void b3dEndLine(void)
{
  glEnd();
  return;
}


void b3dVertex2i(int x, int y)
{
  glVertex2i(x, y);
  return;
}

void b3dDrawBoxout(int llx, int lly, int urx, int ury)
{
  int cur_color;

  glGetIntegerv(GL_CURRENT_INDEX, (GLint *)&cur_color);

  b3dColorIndex(App->background);
     
  if (lly > 0)
    b3dDrawFilledRectangle(0,0,CurWidth,lly);
  if ((CurHeight - ury) > 0)
    b3dDrawFilledRectangle(0, ury, CurWidth, CurHeight);
  if (llx > 0)
    b3dDrawFilledRectangle(0, lly, llx, ury - lly);
  if ((CurWidth - urx) > 0)
    b3dDrawFilledRectangle(urx, lly, CurWidth, ury - lly);

  glIndexi(cur_color);

  return;
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
  return(b3dGetNewCIImageSize(image, depth, (int)CurWidth, (int)CurHeight));
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

  /*  imodPrintStderr ("sh %d  chunklines %d\n", sh, chunklines); */
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
  unsigned short rbase = base;
  GLenum type, format;
  GLint  unpack = b3dGetImageType(&type, &format);
  unsigned int *cindex = App->cvi->cramp->ramp;
  CurXZoom = zoom;

  /*     imodPrintStderr("unpack = %d\n", unpack);
         for(i = 0; i < 256; i++)
         imodPrintStderr("%x ", cindex[i]);
         imodPrintStderr("\n\n");
  */
  if (App->depth == 8)
    rbase = 0;

  if (!dataPtrs){
    b3dDrawFilledRectangle(wx, wy, (int)(width * xzoom), 
                           (int)(height * yzoom));
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
        data = dataPtrs[j + yoffset];
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
            rgbadata = (unsigned int *)data;
            for (i = istart; i < ilim; i++, di++)
              idata[di] = rgbadata[i];
            break;
          }
          break;
        }
      }
    }

    /* imodPrintStderr("Draw wx %d wy %d width %d height %d window %d %d\n",
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

  return;
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
  unsigned short rbase  = base;
  unsigned int *cindex = App->cvi->cramp->ramp;
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
  sh = B3DMIN(CurHeight - wy, B3DMIN(image->height, sh));
  sw = B3DMIN(CurWidth - wx, B3DMIN(image->width, sw));

  if (!dataPtrs){
    b3dDrawFilledRectangle(wx, wy, sw, sh);
    return;
  }

  /* DNM: 11/26/07: eliminate side filling hack */
  wxdraw = wx;
  drawwidth = sw;

  maxj = height;
          
  if (!b3dImageMatch(image, &sdata, xoffset, yoffset,
                     sw, sh, 1.5, 1, slice)){
    b3dImageSet(image, &sdata, xoffset, yoffset,
                sw, sh, 1.5, 1, slice);
    bdata = (unsigned char *)sdata;
    idata = (unsigned int  *)sdata;
    /*            if (rbase) */
    for (j = 0, di = 0;  j < maxj; j++){
      data = dataPtrs[j + yoffset];
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
    wxdraw, wy, drawwidth, sh, CurWidth, CurHeight); */

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
void b3dDrawGreyScalePixelsHQ(unsigned char **dataPtrs,  /* input data lines */
                              int xsize, int ysize,     /* size of input   */
                              int xoffset, int yoffset, /* data offsets    */
                              int wx, int wy,           /* window start    */
                              int width, int height,    /* sub-area size   */
                              B3dCIImage *image,        /* tmp image data. */
                              int base,                 /* colorindex ramp */
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
  int dwidth = (int)(width * zoom);
  int dheight= (int)(height * zoom);
  int xi, yi, pyi, nyi;
  int i, j, izs, xistop, istop;
  int *xindex;
     
  double cx, cy;                            /* current x,y values          */
  double zs    = 1.0/zoom;                 /* zoom step for x,y           */
  float trans = -(float)(0.5 - (0.5 * zs));       /* translate offset. */
  float xstop = xoffset + width + trans; /* stop at this x coord.   */
  float ystop = yoffset + height + trans; /* stop at this y coord. */
  unsigned short rbase = base;
  float pixmin = 0.0f, pixmax = 255.0f;
  GLenum type, format;
  GLint unpack = b3dGetImageType(&type, &format);
  unsigned int *cindex = App->cvi->cramp->ramp;
  int ibase;
  int drawwidth;
  int wxdraw, nyi2;
  CurXZoom = zoom;
  cubicFactors *cubFacs, *cf;
  cubicFactors nullFacs;
  unsigned char *yptr;
  float fy1, fy2, fy3, fy4;

  if (!dataPtrs){
    b3dColorIndex(base);
    glColor3f(0.0f, 0.0f, 0.0f);
    b3dDrawFilledRectangle(wx, wy, (int)(width * xzoom),
                           (int)(height * yzoom));
    return;
  }
    
  /* special optimization. DNM: don't take if want quality*/
  if (!quality && (xzoom == 1.50) && (yzoom == 1.50)){
    b3dDrawGreyScalePixels15(dataPtrs, xsize, ysize, xoffset, yoffset,
                             wx, wy, width, height, image, base, slice, rgba);
    return;
  } 

  /* Ignore quality button for small zooms */
  if ((xzoom <= 1.0)&&(yzoom <= 1.0))
    quality = 0;
    
  if (!quality){
        
    if (!App->wzoom){
      /* If relying on OpenGL zoom completely, just call this */
      b3dDrawGreyScalePixels(dataPtrs, xsize, ysize, xoffset, yoffset,
                             wx, wy, width, height, image, base,
                             xzoom, yzoom, slice, rgba);
      return;
    }else{
      if (xzoom >= 1.0 && (int)xzoom == xzoom){
        /* If it's an integer zoom (1 or more), then use the OpenGL
           zoom through this routine */
        b3dDrawGreyScalePixels(dataPtrs, xsize, ysize, 
                               xoffset, yoffset,
                               wx, wy, width, height, image, base,
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

  dwidth = B3DMIN(CurWidth, B3DMIN(image->width, dwidth));
  dheight = B3DMIN(CurHeight, B3DMIN(image->height, dheight));

  if (App->depth == 8){
    rbase = 0;
    pixmin = RAMPMIN;
    pixmax = RAMPMAX;
  }

  // DNM 11/26/07: eliminate side filling hack
  wxdraw = wx;
  drawwidth = dwidth;

  /* DNM: test and send quality rather than 1 so that it can tell whether
     fractional zooms are truly hq or not */

  if (!b3dImageMatch(image, &sdata, xoffset, yoffset,
		     width, height, zoom, quality, slice)){
    b3dImageSet(image, &sdata, xoffset, yoffset,
		width, height, zoom , quality, slice); 

    bdata = (unsigned char *)sdata;
    idata = (unsigned int  *)sdata;
    if (quality > 0 && rgba < 2){

      /* For HQ, step to each display pixel and use cubic
	 interpolation at each position.  Not for color data */

      // First set up cubic factors for each position in X
      cubFacs = (cubicFactors *)malloc(dwidth * sizeof(cubicFactors));
      if (cubFacs) {
	for (i = 0, cx = xoffset + trans; i < dwidth; cx += zs, i++) {
          cf = &cubFacs[i];
          getCubicFactors(cx, xsize, cf->pxi, cf->xi, cf->nxi, cf->nxi2, 
                          cf->fx1, cf->fx2, cf->fx3, cf->fx4);
        } 
      }

      // Loop on lines
      for (j = 0, cy = yoffset + trans; j < dheight; cy += zs, j++) {

        // Get cubic factors for the position in Y
        getCubicFactors(cy, ysize, pyi, yi, nyi, nyi2, fy1, fy2, fy3, fy4);

        ibase = j * drawwidth;
        
        // Loop across the line
        for (i = 0, cx = xoffset + trans; i < dwidth; cx += zs, i++) {

          // Do the cubic interpolation: fall back to computing the factors on
          // the fly
          if (cubFacs) {
            cf = &cubFacs[i];
          } else {
            cf = &nullFacs;
            getCubicFactors(cx, xsize, cf->pxi, cf->xi, cf->nxi, cf->nxi2, 
                            cf->fx1, cf->fx2, cf->fx3, cf->fx4);
          }
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
      if (cubFacs)
        free(cubFacs);

    } else {

      /* For low quality, non-integer zooms, use nearest neighbor
	 interpolation at each pixel */
      /* DNM 3/4/03: make sure stop values are not too large */
      /* DNM 4/17/03: copy pixels on right if array is not full */
      if (int(xstop + 0.5) >= xsize)
        xstop = xsize - 1.5;
      if (int(ystop + 0.5) >= ysize)
        ystop = ysize - 1.5;

      /* Get an integer step size and see if it is close enough;
         if so, use integer steps and avoid floating calculation below */
      izs = (int)(zs + 0.5);
      if (izs - zs > 0.002 || izs - zs < -0.002)
        izs = 0;
      else
        CurXZoom = 1. / izs;

      /* DNM 7/6/07: Need an integer stopping value for integer steps */
      xistop = (int)(xoffset + trans + 0.5) + dwidth * izs;
      if (xistop > xsize)
        xistop = xsize;

      /* DNM 8/1/08: precompute the X indexes */
      if (!izs) {
        xindex = (int *)malloc((drawwidth + 4) * sizeof(int));
        if (!xindex)
          return;
        for (i = 0, cx = xoffset + trans; cx < xstop && i < drawwidth;
             cx += zs, i++)
          xindex[i] = (int)(cx + 0.5);
        istop = i;
      }

      for (j = 0, cy = yoffset + trans; cy < ystop && j < dheight;
          cy += zs, j++) {
	yi = (int)(cy + 0.5);
	ibase = j * drawwidth;
        xi = (int)(xoffset + trans + 0.5);
	switch(unpack) {
	case 1:
          if (izs) {
            for (i = 0; xi < xistop; xi += izs, i++)
              bdata[i + ibase] = dataPtrs[yi][xi];
          } else {
            for (i = 0; i < istop; i++) 
              bdata[i + ibase] = dataPtrs[yi][xindex[i]];
          }
	  for (; i && i < drawwidth; i++)
	    bdata[i + ibase] = 	bdata[i + ibase - 1];
	  break;

	case 2:
          if (izs) {
            for (i = 0; xi < xistop; xi += izs, i++)
              sdata[i + ibase] = dataPtrs[yi][xi] + rbase;
          } else {
            for (i = 0; i < istop; i++) 
              sdata[i + ibase] = dataPtrs[yi][xindex[i]] + rbase;
          }
	  for (; i && i < drawwidth; i++)
	    sdata[i + ibase] = 	sdata[i + ibase - 1];
	  break;

	case 4:
	  switch (rgba) {
	  case 1:   /* lookup from bytes */
            if (izs) {
              for (i = 0; xi < xistop; xi += izs, i++)
                idata[i + ibase] = cindex[dataPtrs[yi][xi]];
            }  else {
              for (i = 0; i < istop; i++) 
                idata[i + ibase] = cindex[dataPtrs[yi][xindex[i]]];
            }
	    break;

	  case 3:   /* copy RGB data to RGBA */
	    bidata = (unsigned char *)&(idata[ibase]);
	    bdata = dataPtrs[yi];
            if (izs) {
              for (i = 0; xi < xistop; xi += izs, i++) {
                bptr = bdata + 3 * xi;
                *bidata++ = *bptr++;
                *bidata++ = *bptr++;
                *bidata++ = *bptr++;
                *bidata++ = 0;
              }
            } else {
              for (i = 0; i < istop; i++) {
                bptr = bdata + 3 * xindex[i];
                *bidata++ = *bptr++;
                *bidata++ = *bptr++;
                *bidata++ = *bptr++;
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

	  for (; i && i < drawwidth; i++)
	    idata[i + ibase] = 	idata[i + ibase - 1];
	  break;
	}
      }
      
      /* Fill the top if necessary */
      for (; j && j < dheight; j++) {
	bdata = (unsigned char *)sdata + unpack * j * drawwidth;
	memcpy(bdata, bdata - unpack * drawwidth, unpack * drawwidth);
      }
      if (!izs)
        free(xindex);
    }
  }
  /*imodPrintStderr("HQ wx %d wy %d dwidth %d dheight %d window %d %d\n",
    wxdraw, wy, drawwidth, dheight, CurWidth, CurHeight);*/

  glPixelZoom((GLfloat)1.0f, (GLfloat)1.0f); 
#ifndef CHUNKDRAW_HACK
  glRasterPos2f((float)wxdraw, (float)wy);
  glDrawPixels(drawwidth, dheight, format, type, sdata); 
#else
  chunkdraw(xsize * ysize, 1., wxdraw, wy, drawwidth, dheight, format,
	    type, (unsigned int *)sdata);
#endif

  return;
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
   imodv_gfx.cpp */

/* DNM 12/28/03: just define default as TIF (not really used) */
static int SnapShotFormat = SnapShot_TIF;

static QString snapDirectory = "";

// So that movie snapshots can avoid checking file number from 0
static bool  movieSnapping = false;
void b3dSetMovieSnapping(bool snapping)
{
  movieSnapping = snapping;
}

/*
 * Set the snapshot directory, starting in previous one if any
 */
void b3dSetSnapDirectory(void)
{
  QString dir = QDir::currentPath();
  if (!snapDirectory.isEmpty())
    dir = snapDirectory;
  snapDirectory = QFileDialog::getExistingDirectory
    (NULL, "Directory to Save Snapshots to", dir);
}

/*
 * Create a filename in fname with the prefix in name, based on the
 * format_type, and with at least the given number of digits.  The starting
 * candidate file number is in fileno.  If the previous file number exists it
 * will start checking for free file name from this number, but if previous
 * file number does not exist it will start checking from 0.
 */
QString b3dGetSnapshotName(char *name, int format_type, int digits,
                        int &fileno)
{
  char format[14];
  QString snapFormat, fext, fname;
  QString dir = snapDirectory;
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
int b3dAutoSnapshot(char *name, int format_type, int *limits, 
                    bool checkConvert)
{
  QString fname, sname;
  static int fileno = 0;
  int retval;

  // Reset the file number to 0 unless doing movie snapshots
  if (!movieSnapping)
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

/* Take a snapshot of the current window with prefix in name and type selected
   by shift and ctrl key states.  Checking for RGB conversion to gray scale
   is defaulted to TRUE. */
int b3dKeySnapshot(char *name, int shifted, int ctrl, int *limits,
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

  int mapsize;
  b3dUInt32 *fcmapr, *fcmapg, *fcmapb;
  b3dUInt32 *cindex, ci;
  b3dUInt32 *rgbout;
  unsigned char *pixin;
  int j;
  bool imret = true;
  unsigned char *pixout, tmp;
  int rpx = 0; 
  int rpy = 0;
  int rpWidth = CurWidth;
  int rpHeight = CurHeight;
  QString format = ImodPrefs->snapFormat();

  errno = 0;

  // Open file if RGB mode
  if (format == "RGB") {
    fout = fopen(LATIN1(QDir::convertSeparators(fname)), "wb");
    if (!fout) {
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
  pixels = (unsigned char *)malloc(rpWidth * (rpHeight + 1) * 4);
  if (!pixels){
    if (fout)
      fclose(fout);
    return 1;
  }
  glPixelZoom(1.0,1.0);
  xysize = rpWidth * rpHeight;
  pixout = (unsigned char *)pixels;

  /* DNM: add rgb mode support */
  if (rgbmode && !data) {
    glReadPixels(rpx, rpy, rpWidth, rpHeight,
                 GL_RGBA, GL_UNSIGNED_BYTE, pixels);
    glFlush();

    /* have to swap bytes and fill in 0 for A, because RGB routine wants
       ABGR*/
    for (i = 0; i < xysize; i++) {
      pixout[3] = pixout[0];
      pixout[0] = 0;
      pixout++;
      tmp = *pixout;
      *pixout = pixout[1];
      pixout[1] = tmp;
      pixout+=3;
    }

  } else if (!data) {

    // Color index mode: get tables and read image as indices
    mapsize = 1 << App->depth;
    fcmapr = (unsigned int *)malloc((mapsize+1) * sizeof(unsigned int));
    fcmapg = (unsigned int *)malloc((mapsize+1) * sizeof(unsigned int));
    fcmapb = (unsigned int *)malloc((mapsize+1) * sizeof(unsigned int));
    if (!fcmapr || !fcmapg || !fcmapb) {
      free(pixels);
      if (fcmapr)
        free(fcmapr);
      if (fcmapg)
        free(fcmapg);
      if (fcmapb)
        free(fcmapb);
      if (fout)
        fclose(fout);
      return 1;
    }
    
    for(i = 0; i < mapsize; i++){
      QColor qcolor = App->qColormap->entryColor(i);
      fcmapr[i] = qcolor.red();
      fcmapg[i] = qcolor.green();
      fcmapb[i] = qcolor.blue();
    }

    glReadPixels(rpx, rpy, rpWidth, rpHeight,
                 GL_COLOR_INDEX, GL_UNSIGNED_INT, pixels);
    glFlush();
    cindex = (b3dUInt32 *)pixels;
    for (i = 0; i < xysize; i++, cindex++){
      ci = *cindex;
      if (ci > mapsize) ci = 0;
      /* DNM: switch from using endian-dependent shifts to packing
         the array as ABGR */
      *pixout++ = 0;
      *pixout++ = fcmapb[ci];
      *pixout++ = fcmapg[ci];
      *pixout++ = fcmapr[ci];
    }
    free(fcmapr);
    free(fcmapg);
    free(fcmapb);

  } else {

    // For passed-in line pointers, repack RGB or RGBA as 0BGR
    for (j = 0; j < rpHeight; j++) {
      for (i = 0; i < rpWidth; i++) {
        *pixout++ = 0;
        *pixout++ = data[j][rgbmode*i+2];
        *pixout++ = data[j][rgbmode*i+1];
        *pixout++ = data[j][rgbmode*i];
      }
    }
  }

  // Ordinary RGB, call routine
  if (fout) {
    bdRGBWrite(fout, (int)rpWidth, (int)rpHeight, pixels);
    fclose(fout);
  } else {

    // Qt image writing: need to reorder lines as well as encode into rgb's
    for (j = 0; j < (rpHeight + 1) / 2; j++) {
      rgbout = (b3dUInt32 *)pixels + (rpHeight - 1 - j) * rpWidth;
      pixin = pixels + 4 * j * rpWidth;
      memcpy(pixels + 4 * xysize, rgbout, 4 * rpWidth);
      for (i = 0; i < rpWidth; i++) {
        *rgbout++ = qRgb(pixin[3], pixin[2], pixin[1]);
        pixin += 4;
      }
      pixin = pixels + 4 * xysize;
      rgbout = (b3dUInt32 *)pixels + j * rpWidth;
      for (i = 0; i < rpWidth; i++) {
        *rgbout++ = qRgb(pixin[3], pixin[2], pixin[1]);
        pixin += 4;
      }
    }

    // Save the image with the given format and quality (JPEG only)
    QImage *qim = new QImage(pixels, rpWidth, rpHeight, QImage::Format_RGB32);
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
  int i, j;
  unsigned char *pixels = NULL;
  int *lpixels;
  int xysize, xsize, ysize, step, samples;
  unsigned int pixel;
  unsigned int ifd;
  unsigned int colortable;
  unsigned char bpix,rpix,gpix;
  unsigned short tenum;
  unsigned short color[3];
  int depth;
  bool convertRGB = false;

  int mapsize;
  unsigned int *fcmapr, *fcmapg, *fcmapb;
  int ci;
  int rpx = 0; 
  int rpy = 0;
  int rpWidth = CurWidth;
  int rpHeight = CurHeight;

  errno = 0;
  fout = fopen(LATIN1(QDir::convertSeparators(fname)), "wb");
  if (!fout){

    // DNM 5/31/04: output to standard error in case there are many errors
    QString qerr = "Snapshot: error opening file\n";
    if (errno)
      qerr +=  QString("System error: ") + QString(strerror(errno));
    imodPrintStderr(LATIN1(qerr));
    return 1;
  }

  if (checkConvert && rgbmode)
    convertRGB = App->convertSnap;

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
      return 1;
    }
  }
  glPixelZoom(1.0,1.0);
    
  if (!data && rgbmode) {
    glReadPixels(rpx, rpy, rpWidth, rpHeight,
                 GL_RGBA, GL_UNSIGNED_BYTE, pixels);
    glFlush();
    /* App may not be defined if rgbmode is coming from standalone Imodv,
       so set up a depth variable that works in later tests regardless */
    depth = 8;

  } else if (!data) {
    depth = App->depth;
    mapsize = 1 << depth;
    fcmapr = (unsigned int *)malloc((mapsize+1) * sizeof(unsigned int));
    if (!fcmapr) return 1;
    fcmapg = (unsigned int *)malloc((mapsize+1) * sizeof(unsigned int));
    if (!fcmapg) return 1;
    fcmapb = (unsigned int *)malloc((mapsize+1) * sizeof(unsigned int));
    if (!fcmapb) return 1;

    for(i = 0; i < mapsize; i++){
      QColor qcolor = App->qColormap->entryColor(i);
      fcmapr[i] = qcolor.red();
      fcmapg[i] = qcolor.green();
      fcmapb[i] = qcolor.blue();
    }
          
    glReadPixels(rpx, rpy, rpWidth, rpHeight,
                 GL_COLOR_INDEX, GL_UNSIGNED_INT, pixels);
    glFlush();
  }

  /* DNM: change __vms to LITTLE_ENDIAN to work on PC */
#ifdef B3D_LITTLE_ENDIAN
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
    return 1;
  }


  // 11/1/04: fix to allow 3 more words (2 front of the image, 1 after)
  ifd = xysize;
  if ((depth > 8 || rgbmode) && !convertRGB)
    ifd *= 3;
  ifd = 4 * ((ifd + 3) / 4 + 3);

  if (!fwrite(&ifd, 4, 1, fout)){
    fclose(fout);
    if (!rgbmode) {
      free(fcmapr);
      free(fcmapg);
      free(fcmapb);
    }
    if (!data) 
      free(pixels);
    return 1;
  }

  if (data) {
    /* Use the line pointers to write each line */
    step = rgbmode == 3 ? 3 : 4;
    for(j = ysize - 1; j >= 0; j--)
      if (convertRGB) {
        for(i = 0; i < xsize; i++) {
          bpix = (int)(0.3 * data[j][step*i] + 0.59 * data[j][step*i+1] + 
                       0.11 * data[j][step*i + 2] + 0.5);
          fwrite(&bpix, 1, 1, fout);
        }
      } else if (rgbmode == 3) {
        fwrite(data[j], 1, 3 * xsize, fout);
      } else {
        for(i = 0; i < xsize; i++)
          fwrite(&data[j][4*i], 1, 3, fout);
      }

  } else if (rgbmode) {
    for(j = ysize - 1; j >= 0; j--)
      if (convertRGB) {
        for(i = 0; i < xsize; i++) {
          bpix = (int)(0.3 * pixels[4*(i+(j*xsize))] + 
                       0.59 * pixels[4*(i+(j*xsize)) + 1] + 
                       0.11 * pixels[4*(i+(j*xsize)) + 2] + 0.5);
          fwrite(&bpix, 1, 1, fout);
        }
      } else {
        for(i = 0; i < xsize; i++)
          fwrite(&lpixels[i + (j * xsize)], 1, 3, fout);
      }

  } else if (depth > 8){
    for(j = ysize - 1; j >= 0; j--)
      for(i = 0; i < xsize; i++){
        ci = lpixels[i + (j * xsize)];
        rpix = (unsigned char)fcmapr[ci];
        gpix = (unsigned char)fcmapg[ci];
        bpix = (unsigned char)fcmapb[ci];
        if (convertRGB) {
          bpix = (int)(0.3 * rpix + 0.59 * gpix + 0.11 * bpix);
        } else {
          fwrite(&rpix, 1, 1, fout);
          fwrite(&gpix, 1, 1, fout);
        }
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
    samples = convertRGB ? 1 : 3;
    colortable = convertRGB ? 8 : ifd + 2 + (tenum * 12) + 7;
    fwrite(&tenum, 2, 1, fout);
    puttiffentry(256, 3, 1, xsize, fout);
    puttiffentry(257, 3, 1, ysize, fout);
    puttiffentry(258, 3, samples, colortable, fout);
    puttiffentry(259, 3, 1, 1, fout);
    puttiffentry(262, 3, 1, convertRGB ? 1 : 2, fout);
    puttiffentry(273, 4, 1, 8, fout);
    puttiffentry(274, 3, 1, 1, fout);
    puttiffentry(277, 3, 1, samples, fout);
    puttiffentry(278, 4, 1, ysize, fout);
    puttiffentry(279, 4, 1, xysize * samples, fout);
    puttiffentry(284, 3, 1, 1, fout);  /* plane config */

    ifd = 0;
    fwrite(&ifd, 4, 1, fout);

    if (!convertRGB) {
      fseek(fout, (int)colortable, SEEK_SET);
      color[0] = 8;
      fwrite(color, 2, 1, fout);
      fwrite(color, 2, 1, fout);
      fwrite(color, 2, 1, fout);
    }
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

  return 0;
}



int b3dSnapshot(QString fname)
{
  if (SnapShotFormat == SnapShot_RGB)
    return(b3dSnapshot_NonTIF(fname, App->rgba, NULL, NULL));
  else
    return(b3dSnapshot_TIF(fname, App->rgba, NULL, NULL, false));
}


/*
$Log$
Revision 4.41  2008/12/07 05:22:14  mast
Made images be drawn in window when they fit exactly in window

Revision 4.40  2008/08/01 15:38:47  mast
Fixed rounding bug with large non-integer zooms and nonHQ display

Revision 4.39  2008/05/27 05:37:56  mast
Added ability to snapshot RGB as gray-scale

Revision 4.38  2008/05/23 04:31:21  mast
Changed to allow nontiff montage snapshots

Revision 4.37  2007/12/13 03:20:17  mast
Fixed panning offset getting stuck at negative end

Revision 4.36  2007/11/27 04:56:06  mast
Limit drawing to defined width/height of image array, needed for XYZ subareas

Revision 4.35  2007/11/10 15:21:14  mast
Snap dir fixes for windows: get short name correctly, start in current dir

Revision 4.34  2007/11/10 04:07:10  mast
Changes for setting snapshot directory

Revision 4.33  2007/09/17 04:58:40  mast
Switched from quadratic to cubic for HQ drawing

Revision 4.32  2007/07/12 17:31:47  mast
Added subarea viewport function and added fill flag to offset routine

Revision 4.31  2007/07/07 05:30:20  mast
Fixed limits for filling draw array, which were overrunning array

Revision 4.30  2007/05/06 03:25:35  mast
Do not check snapshot file numbers from 0 when movieing

Revision 4.29  2006/10/05 15:41:31  mast
Provided for primary and second non-TIFF snapshot format

Revision 4.28  2006/07/03 04:14:21  mast
Changes for beadfixer overlay mode

Revision 4.27  2005/10/21 23:58:41  mast
Fixed for gcc 4.0 on Mac

Revision 4.26  2005/03/26 00:39:50  mast
Tested limits in HQ display to prevent crash

Revision 4.25  2004/11/29 19:25:21  mast
Changes to do QImage instead of RGB snapshots

Revision 4.24  2004/11/02 20:14:10  mast
Switch to get named colors from preferences

Revision 4.23  2004/11/02 00:51:59  mast
Fixed tiff writing to allow proper numbmber of bytes after image

Revision 4.22  2004/10/04 18:29:01  mast
Changed snapshot functions to give error returnd

Revision 4.21  2004/09/10 02:31:03  mast
replaced long with int

Revision 4.20  2004/06/01 01:31:42  mast
Add include of errno.h

Revision 4.19  2004/05/31 23:35:26  mast
Switched to new standard error functions for all debug and user output

Revision 4.18  2004/03/09 15:18:02  mast
Fixed problem that was making offset images creep

Revision 4.17  2003/12/30 06:34:24  mast
Make snapshot name in a single routine, and change snapshotting from
an array to take 3 or 4 byte data

Revision 4.16  2003/09/16 05:54:32  mast
Ditto!

Revision 4.15  2003/09/16 05:52:23  mast
Forgot to comment out diagnostic output

Revision 4.14  2003/09/16 03:52:46  mast
Fixed test for integer zoom to not fail on 0.01, and fixed bug in 1.5 zoom

Revision 4.13  2003/09/16 03:01:26  mast
Changed pixel drawing routines to take image data as line pointers
Added faster fractional zoom drawing when fraction is near 1/integer

Revision 4.12  2003/09/12 22:02:20  mast
Fixed float/double problem with zooming under Windows

Revision 4.11  2003/06/04 23:28:50  mast
Simplify auto snapshot numbering code

Revision 4.10  2003/05/05 15:06:21  mast
Copy-fill top and right of array when drawing non-integer nonHQ zooms

Revision 4.9  2003/04/17 19:02:59  mast
adding hack for GL-context dependent gluQuadric

Revision 4.8  2003/03/28 05:08:23  mast
*** empty log message ***

Revision 4.7  2003/03/24 17:58:09  mast
Changes for new preferences capability

Revision 4.6  2003/03/12 21:34:19  mast
Made b3dGetNewCIImageSize return null if there is failure to get memory for
image, and made pixel drawing routines test for null image, all to protect
against memory errors.

Revision 4.5  2003/03/04 23:23:07  mast
Fixed bug in accessing non-existent data with zoomed down images

Revision 4.4  2003/02/27 17:36:58  mast
Convert filenames with Qt routines

Revision 4.3  2003/02/21 23:56:34  mast
open snapshot files in binary mode

Revision 4.2  2003/02/14 01:13:43  mast
cleanup unused variables

Revision 4.1  2003/02/10 20:28:59  mast
autox.cpp

Revision 1.1.2.4  2003/02/07 01:03:23  mast
a little cleanup

Revision 1.1.2.3  2003/01/29 01:42:35  mast
Changes for color index snapshotting

Revision 1.1.2.2  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.1  2003/01/23 23:05:54  mast
conversion to cpp

Revision 3.4.2.6  2003/01/13 01:12:37  mast
Fixed bug in drawboxout with negative coordinates

Revision 3.4.2.5  2003/01/10 23:57:23  mast
questionable change to display routine used by tumbler

Revision 3.4.2.4  2003/01/06 15:40:11  mast
Add call to resize viewport given x and Y arguments

Revision 3.4.2.3  2002/12/17 18:14:26  mast
remove unused iput functions

Revision 3.4.2.2  2002/12/12 01:20:32  mast
When setting fonts, take application display if CurDisplay not defined

Revision 3.4.2.1  2002/12/09 17:49:19  mast
changes to get Zap as a Qt window

Revision 3.4  2002/12/01 15:32:27  mast
Changes to compile under g++; also eliminated all non openGL code

Revision 3.3  2002/07/28 16:18:58  mast
Added three new fractional zooms below 1.0 to make steps be 1.4-1.6

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
