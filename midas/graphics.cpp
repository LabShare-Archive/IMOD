/*  IMOD VERSION 2.50
 *
 *  midas_gl.c -- Graphics for the midas program.
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
Revision 3.5  2003/12/17 21:43:04  mast
Provide control-key dependent mouse action hints

Revision 3.4  2003/11/01 16:43:10  mast
changed to put out virtually all error messages to a window

*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "midas.h"
#include "dia_qtutils.h"

MidasGL::MidasGL(QGLFormat inFormat, QWidget * parent, const char * name)
  : QGLWidget(inFormat, parent, name)
{
  // get format, make sure it is RGB, and compare buffering with requested
  if (!format().rgba())
    midas_error("Midas error:", "No RGB GL format was available", 1);

  if (format().doubleBuffer() && !inFormat.doubleBuffer())
    dia_puts("Midas warning: Double buffering is being used even "
	    "though\n  single buffering was requested\n");
  if (!format().doubleBuffer() && inFormat.doubleBuffer())
    dia_puts("Midas warning: Single buffering is being used even "
	    "though\n  double buffering was requested\n");
  mSkipClearOnDraw = false;
  mMousePressed = false;
}

MidasGL::~MidasGL()
{

}


/* Clear the image display area. */
void MidasGL::midas_clear()
{
  makeCurrent();
  glClear(GL_COLOR_BUFFER_BIT);
}

/* to do a draw, just issue the glDraw and that will make it go through
   glPaint and take care of buffer swapping */
void MidasGL::draw()
{
  mSkipClearOnDraw = true;
  glDraw();
  mSkipClearOnDraw = false;
}

void MidasGL::paintGL()
{
  int xdrawn, ydrawn;
  float xcut, ycut;
  double angle, tcrit;
  int i;
  int numlines = 2;
  float xcen, ycen;
  float zoom = VW->truezoom;
  float censize = 7.;

  if (!mSkipClearOnDraw)
    midas_clear();

  /* DNM 2/5/01: set sdatSize here.  This may be redundant now that the
     expose_cb tests correctly for need for new array */
  if (!VW->sdat) {
    VW->sdat = (b3dUInt32 *)
      malloc( VW->width * VW->height * sizeof(b3dUInt32));
    VW->sdatSize = VW->width * VW->height;
  }

  draw_image(VW, VW->id, 0, 0, VW->width, VW->height, &xdrawn, &ydrawn);
     
  if (VW->paramstate[2] < 1.0001 && VW->paramstate[2] > 0.9999)
    numlines = 1;

  if (VW->showref)
    numlines = 0;
  else {
    /* Draw the center of action */
    xcen = zoom * VW->xcenter + VW->xoffset;
    ycen = zoom * VW->ycenter + VW->yoffset;
    glDisable(GL_LINE_STIPPLE);
    glColor3f(1.0f, 1.0f, 0.0f);
    glBegin(GL_LINES);
    glVertex2f(xcen, ycen + censize);
    glVertex2f(xcen, ycen - censize);
    glEnd();
    glBegin(GL_LINES);
    glVertex2f(xcen + 0.866 * censize, ycen + 0.5 * censize);
    glVertex2f(xcen - 0.866 * censize, ycen - 0.5 * censize);
    glEnd();
    glBegin(GL_LINES);
    glVertex2f(xcen + 0.866 * censize, ycen - 0.5 * censize);
    glVertex2f(xcen - 0.866 * censize, ycen + 0.5 * censize);
    glEnd();
  }
     
  // On the first draw, update parameter display
  // 12/7/03, move up above drawing of stretch line so it is correct
  if (!VW->exposed)
    VW->midasSlots->update_parameters();

  /* First line is for the defined stretch angle */
     
  tcrit = atan(((double)ydrawn)/xdrawn);
  glEnable(GL_LINE_STIPPLE);
  glLineStipple(10,0x5555);
     
  glColor3f(1.0f, 0.0f, 0.0f);
  angle = 0.1 * VW->sangle * RADIANS_PER_DEGREE;

  for (i = 0; i < numlines; i ++) {
    if (angle > tcrit || angle < -tcrit) {
      ycut = ydrawn / 2.;
      xcut = ycut * cos(angle) / sin(angle);
    } else {
      xcut = xdrawn / 2.;
      ycut = xcut * tan(angle);
    }
    glBegin(GL_LINES);
    glVertex2f(VW->width / 2. - xcut, VW->height / 2. - ycut);
    glVertex2f(VW->width / 2. + xcut, VW->height / 2. + ycut);
    glEnd();

    /* now actual stretch axis, conditional on existence of stretch */
    glColor3f(0.0f, 0.4f, 1.0f);
    angle = VW->phi * RADIANS_PER_DEGREE;
  }

  glFlush();

  VW->exposed = 1;
}


/* fill in image data */
/* Copy the data in fbuf to the tobuf.
 */
void MidasGL::fill_rgb(unsigned char *fbuf, b3dUInt32 *tobuf, 
		       int size, int channel, struct Midas_transform *tr)
{
  unsigned char *buf = (unsigned char *)tobuf;
  b3dUInt32 tmp;
  unsigned char cmap[256];
  unsigned int cmapi[256];
  unsigned char *cmap2;
  int rampsize, i;
  float point, slope;

  /* DNM: channels are numbered backwards because it was originally in GL,
     which was ABGR, and OpenGL was
     chosen to be RGBA, since GL_AGBR_EXT was deadly slow on the PC */
  if (channel > 0)
    buf += 3 - channel;

  /* fill with zeros if no image data */
  if (!fbuf){
    tmp = 0;
    if (channel){
      for (i = 0; i < size; i++, buf+=4)
	*buf = (unsigned char)tmp;
      return;
    }else{
      for (i = 0; i < size; i++)
	tobuf[i] = tmp;
      return;
    }
  }

  /* write out saturated values to cmap. */
  for (i = 0; i < tr->black; i++)
    cmap[i] = 0;
  for (i = tr->white; i < 256; i++)
    cmap[i] = 255;

  /* calculate rampsize and slope */
  rampsize = tr->white - tr->black;
  if (rampsize < 1)
    rampsize = 1;
  slope = 256.0 / (float)rampsize;

  /* Make the ramp. */
  for (i = tr->black; i < tr->white; i++){
    point = (float)(i - tr->black) * slope;
    cmap[i] = (unsigned char)point;
  }
     
  /* reverse the color ramp */
  if (VW->reversemap)
    for(i = 0; i < 256; i++)
      cmap[i] = 255 - cmap[i];
     
  /* Positive channel means fill one byte channel of int array */
  if (channel > 0){
    for (i = 0; i < size; i++, buf+=4){
      *buf = cmap[fbuf[i]];
    }
    return;
  }

  /* Negative channel means fill a byte array for writing to file */
  if (channel < 0){
    for (i = 0; i < size; i++){
      *buf++ = cmap[fbuf[i]];
    }
    return;
  }

  /* channel zero means fill r, g, and b: so make an int map to do this */
  cmap2 = (unsigned char *)cmapi;
  for (i = 0; i < 256; i++) {
    *cmap2++ = cmap[i];
    *cmap2++ = cmap[i];
    *cmap2++ = cmap[i];
    *cmap2++ = 0;
  }
     
  for(i = 0; i < size; i++)
    tobuf[i] = cmapi[fbuf[i]];

  return;
}

/* llx, lly, urx, ury are coordinates of area within window to draw to */
void MidasGL::draw_image(struct Midas_view *vw, b3dUInt32 *image,
			 int llx, int lly, int urx, int ury, int *xdrawn, int *ydrawn)
{
  int swinx, swiny;
  int xdrawsize,   ydrawsize;  /* number of pixels being drawn */
  int xborder = 0, yborder = 0;  /* displacement from defined window area */
  int xstart = 0;              /* starting pixels within image to draw */
  int ystart = 0;
  int i, j;
  int xsize, ysize;
  float zoom, glzoom;
  b3dUInt32 *data;
  b3dUInt32 *bdata;
  int di, istart, ilim, maxj, iextra, iskip;

  swinx = urx - llx;  /* size of area to draw to */
  swiny = ury - lly;
  xsize = vw->xsize;  /* full size of image */
  ysize = vw->ysize;

  /* Get actual zoom */
  zoom = vw->truezoom;

  /* compute pixels to draw, starts and borders using actual zoom */
  if ( ((xsize - 1) * zoom) < swinx ){
    xdrawsize = (xsize);
    xborder = (int)(( swinx - ((xsize - 1) * zoom)) / 2);
  }
  else{
    xdrawsize = (int)(swinx / zoom);
    xstart = (int)((xsize / 2 ) - (swinx / zoom / 2));
    xstart -= vw->xtrans;
    if (xstart < 0){
      xstart += vw->xtrans;
      vw->xtrans = xstart;
      xstart -= vw->xtrans;
    }
    if ( (xstart + xdrawsize) > (xsize - 1)){
      xstart += vw->xtrans;
      vw->xtrans = xstart - (xsize - 1 - xdrawsize);
      xstart -= vw->xtrans;
    }
  }
     
  /* same for Y */
  if ( ((ysize - 1) * zoom) < swiny ){
    ydrawsize = (ysize);
    yborder = (int)(( swiny - ((ysize - 1) * zoom)) / 2);
  }
  else{
    ydrawsize = (int)(swiny / zoom);
    ystart = (int)((ysize / 2 ) - (swiny / zoom / 2));
    ystart -= vw->ytrans;
    if (ystart < 0){
      ystart += vw->ytrans;
      vw->ytrans = ystart;
      ystart -= vw->ytrans;
    }
    if ( (ystart + ydrawsize) > (ysize - 1)){
      ystart += vw->ytrans;
      vw->ytrans = ystart - (ysize - 1 - ydrawsize);
      ystart -= vw->ytrans;
    }
  }

  /* save offsets for getting from image to window coordinates */
  vw->xoffset = (int)(xborder - zoom * xstart);
  vw->yoffset = (int)(yborder - zoom * ystart);

  if (vw->zoom < 0) {

    /* fractional zoom: adjust sizes and zooms, copy selected pixels */
    iskip = (int)(-vw->zoom);
    xdrawsize /= iskip;
    ydrawsize /= iskip;
    glzoom = 1;
    image += ystart * xsize;
    image += xstart;
	  
    for (j = 0; j < ydrawsize; j++){
      for(i = 0; i < xdrawsize; i++, image += iskip)
	vw->sdat[(j * xdrawsize) + i] = *image;
      image += xsize * iskip;
      image -= xdrawsize * iskip;
    }
    image = vw->sdat;

  } else if (vw->zoom == 1.5) {

    /* Special zoom 1.5: adjust sizes, set up for extra lines etc */
    glzoom = 1;
    xdrawsize = (int)(1.5 * xdrawsize);
    if (xdrawsize > swinx)
      xdrawsize = swinx;
    ydrawsize = (int)(1.5 * ydrawsize);
    if (ydrawsize > swiny)
      ydrawsize = swiny;
    maxj = (2 * ydrawsize) / 3;
    data = image;
    bdata = vw->sdat;

    for (j = 0, di = 0;  j < maxj; j++){
      istart = (((j + ystart) * xsize) + xstart);
	       
      ilim  = istart + 2 * (xdrawsize / 3);
      iextra = xdrawsize%3;
	       
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
	memcpy(&bdata[di], &bdata[di-xdrawsize], 4 * xdrawsize);
	di += xdrawsize;
      }
    }
    if (ydrawsize % 3)
      ydrawsize--;
    image = vw->sdat;

  } else {

    /* regular zooms use OpenGL zoom, but need to copy pixels if x size
       of image doesn't match size being drawn */
    glzoom = vw->zoom;
    if (ydrawsize != ysize)
      image += ystart * xsize;
    if (xdrawsize != xsize){
      image += xstart;
	  
      for (j = 0; j < ydrawsize; j++){
	for(i = 0; i < xdrawsize; i++, image++)
	  vw->sdat[(j * xdrawsize) + i] = *image;
	image += xsize;
	image -= xdrawsize;
      }
      image = vw->sdat;
    }
  }

  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  glPixelZoom(glzoom, glzoom);
  glRasterPos2i(llx + xborder, lly + yborder);
  glDrawPixels(xdrawsize, ydrawsize,
	       GL_RGBA, GL_UNSIGNED_BYTE, image);
  *xdrawn = (int)(xdrawsize * glzoom);
  *ydrawn = (int)(ydrawsize * glzoom);

  return;
}

/* Refill all image data buffers with refreshed data. */
int MidasGL::fill_viewdata( struct Midas_view *vw)
{
  int refz = vw->refz;

  Islice *prevSlice;
  Islice *curSlice  = midasGetSlice(vw, MIDAS_SLICE_CURRENT);

  unsigned char *currImageData = curSlice->data.b;
  unsigned char *prevImageData = NULL;

  /* set previous image data pointer. */
  switch(vw->xtype){
  case XTYPE_XF:
  case XTYPE_MONT:
    prevSlice = midasGetSlice(vw, vw->rotMode ? MIDAS_SLICE_PREVIOUS : 
                              MIDAS_SLICE_OPREVIOUS);
    if (prevSlice)
      prevImageData = prevSlice->data.b;
    break;
  case XTYPE_XREF:
    if (vw->ref)
      prevImageData = vw->ref->data.b;
    refz = vw->zsize;
    break;
  case XTYPE_XG:
  default:
    prevSlice = midasGetSlice(vw, MIDAS_SLICE_PREVIOUS);
    if (prevSlice)
      prevImageData = prevSlice->data.b;
    break;
  }

  if (vw->vmode == MIDAS_VIEW_COLOR){
	  
    /* fill the current transformed slice image data. */
    fill_rgb(currImageData, vw->id, vw->xysize, 2, &vw->tr[vw->cz]);

    /* fill the previous slice image data. */
    fill_rgb(prevImageData, vw->id, vw->xysize, 3, &vw->tr[refz]);
    fill_rgb(prevImageData, vw->id, vw->xysize, 1, &vw->tr[refz]);
    return(0);
  }	  
     
  if (vw->vmode == MIDAS_VIEW_SINGLE){
    if (vw->showref){
      fill_rgb(prevImageData, vw->id, vw->xysize, 0, &vw->tr[refz]);
    }else{
      fill_rgb(currImageData, vw->id, vw->xysize, 0, &vw->tr[vw->cz]);
    }
  }

  return 0;
}


/* 
 *  update the just transformed slice,
 *  The slice must already be transformed if needed.
 */
int MidasGL::update_slice_view(void)
{
  Islice *curSlice = midasGetSlice(VW, MIDAS_SLICE_CURRENT);

  if (VW->vmode == MIDAS_VIEW_SINGLE)
    fill_viewdata(VW);

  if (VW->vmode == MIDAS_VIEW_COLOR)
    fill_rgb(curSlice->data.b, VW->id, 
	     VW->xysize, 2, &VW->tr[VW->cz]);

  draw();
  return(0);
}


void MidasGL::initializeGL()
{
  glDisable(GL_ALPHA_TEST);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_DITHER);
  glDisable(GL_FOG);
  glClearColor(0.5, 0.5, 0.5, 0.0);
  VW->midasSlots->update_sections();
}

/* Resize event gives the image size */
void MidasGL::resizeGL(int wdth, int hght)
{
  int xysize;

  VW->width  = wdth;
  VW->height = hght;
     
  glViewport(0, 0, wdth, hght);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(0.0 , wdth, 0.0, hght, 0.5, -0.5);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  xysize = wdth * hght;

  /* DNM 2/5/01: change < to >, means it actually happens now */
  if (xysize > VW->sdatSize){
    if (VW->sdat)
      free(VW->sdat);
    VW->sdat = (b3dUInt32 *)malloc( xysize * sizeof(b3dUInt32));
    VW->sdatSize = xysize;
  }

}

// Mouse event processing
void MidasGL::mousePressEvent(QMouseEvent * e )
{
  unsigned int state = e->state();
  unsigned int button = e->button();
  bool button1 = (button == Qt::LeftButton);
  bool button2 = (button == Qt::MidButton);
  bool button3 = (button == Qt::RightButton);

  // Button press: record position (adjust for inverted Y),
  // set new center if Ctrl-middle
  VW->lastmx = e->x();
  VW->lastmy = VW->height - e->y();

  mMousePressed = true;
  if (button2 && (state & Qt::ControlButton) &&
      VW->xtype != XTYPE_MONT) {
    VW->xcenter = (VW->lastmx - VW->xoffset) / VW->truezoom;
    VW->ycenter = (VW->lastmy - VW->yoffset) / VW->truezoom;
    draw();
  } else if (button1 && (state & Qt::ControlButton))
    manageMouseLabel("PANNING IMAGE");
  else if (button1 && ! (state & Qt::ShiftButton))
    manageMouseLabel("TRANSLATING");
  else if (VW->xtype != XTYPE_MONT) {
    if (button2 && !(state & (Qt::ControlButton | Qt::ShiftButton)))
      manageMouseLabel("ROTATING");
    if (button3 && !(state & Qt::ControlButton)) {
      if (state & Qt::ShiftButton)
        manageMouseLabel("CHANGING MAG");
      else
        manageMouseLabel("STRETCHING");
    }
  }
}

// Release: update parameters now
void MidasGL::mouseReleaseEvent ( QMouseEvent * e )
{
  VW->midasSlots->update_parameters();
  mMousePressed = false;
  manageMouseLabel(" ");
}


// Move event with a button down
void MidasGL::mouseMoveEvent ( QMouseEvent * e )
{
  unsigned int state = e->state();
  bool button1 = (state & Qt::LeftButton);
  bool button2 = (state & Qt::MidButton);
  bool button3 = (state & Qt::RightButton);

  // Under windows, there is a spontaneous mouse move event when file dialog is
  // dismissed, so make sure we got the press beforehand
  if (!mMousePressed)
    return;

  // Get current position, invert Y
  VW->mousemoving = 1;
  VW->mx = e->x();
  VW->my = VW->height - e->y();

  if ( button1 && !button2 && !button3){

    // Button 1 is either translation, or panning image
    if (state & Qt::ControlButton)
      VW->midasSlots->mouse_shift_image();
    else if (! (state & Qt::ShiftButton))
      VW->midasSlots->mouse_translate();

    // Button 2 is to rotate
  } else if (VW->xtype != XTYPE_MONT &&
	     !button1 && button2 && !button3  &&
	     !(state & (Qt::ControlButton | Qt::ShiftButton))) {
    VW->midasSlots->mouse_rotate();
    

    // Button 3 is for stretch or mag
  } else if (VW->xtype != XTYPE_MONT &&
	     !button1 && !button2 && button3  && 
	     !(state & Qt::ControlButton)) {
    VW->midasSlots->mouse_stretch(state);
    
    // Only if no action do we update the last position
  } else {
    VW->lastmx = VW->mx;
    VW->lastmy = VW->my;
  }
  VW->mousemoving = 0;
}

void MidasGL::manageMouseLabel(char *string)
{
  if (mMousePressed) {
    VW->mouseLabel->setPaletteForegroundColor(QColor("red"));
    VW->mouseLabel->setText(QString(string));
  } else {
    VW->mouseLabel->setPaletteForegroundColor(QColor("blue"));
    if (VW->ctrlPressed)
      VW->mouseLabel->setText("pan -- new center --     ");
    else if (VW->shiftPressed)
      VW->mouseLabel->setText("        --        -- magnify");
    else
      VW->mouseLabel->setText("shift -- rotate -- stretch");
  }
}
