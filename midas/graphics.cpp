/* 
 *  midas_gl.c -- Graphics for the midas program.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  $Id$
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "midas.h"
#include "dia_qtutils.h"
//Added by qt3to4:
#include <QMouseEvent>

MidasGL::MidasGL(QGLFormat inFormat, QWidget * parent, const char * name)
  : QGLWidget(inFormat, parent)
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
  glDraw();
}

void MidasGL::paintGL()
{
  int xdrawn, ydrawn;
  float xcut, ycut;
  double angle, tcrit;
  int i, nControl;
  int numlines = 2;
  float xcen, ycen, boxXcen, boxYcen;
  bool boxCenSet = false;
  float zoom = VW->truezoom;
  float censize = 7.;
  float *xControl, *yControl, *xVector, *yVector;

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

    if (!(VW->curWarpFile >= 0 && VW->editWarps)) {

      // If not in warp mode, draw the center of action and the possible fixed point
      xcen = zoom * VW->xcenter + VW->xoffset;
      ycen = zoom * VW->ycenter + VW->yoffset;
      glColor3f(1.0f, 1.0f, 0.0f);
      drawStar(xcen, ycen, censize);
      if (VW->useFixed) {
        xcen = zoom * VW->xfixed + VW->xoffset;
        ycen = zoom * VW->yfixed + VW->yoffset;
        glColor3f(1.0f, 0.0f, 0.0f);
        drawStar(xcen, ycen, censize);
      }
    }
  }

  // If there are control points, draw them
  i = VW->numChunks ? VW->curChunk : VW->cz;
  if (VW->curWarpFile >= 0 && i < VW->warpNz && !getNumWarpPoints(i, &nControl)) {
    if (nControl && !getWarpPointArrays(i, &xControl, &yControl, &xVector, &yVector)) {
      for (i = 0; i < nControl; i++) {
        //printf("%d %f %f %f %f\n", i, xControl[i], yControl[i], xVector[i], yVector[i]);
        xcen = zoom * xControl[i] / VW->warpScale + VW->xoffset;
        ycen = zoom * yControl[i] / VW->warpScale + VW->yoffset;
        glColor3f(1.0f, (VW->editWarps && i == VW->curControl) ? 1.f : 0.f, 0.0f);
        drawStar(xcen, ycen, censize);
        if (VW->drawVectors) {
          glBegin(GL_LINES);
          glVertex2f(xcen, ycen);
          glVertex2f(xcen - zoom * xVector[i] / VW->warpScale, 
                     ycen - zoom * yVector[i] / VW->warpScale);
          glEnd();
        }
        if (i == VW->curControl) {
          boxXcen = xcen;
          boxYcen = ycen;
          boxCenSet = true;
        }
      }
    }
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

  if (VW->drawCorrBox) {
    glColor3f(VW->drawCorrBox > 1 ? 1.0f : 0.f, 
              VW->drawCorrBox < 3 ? 1.0f : 0.0f, 0.0f);
    if (!boxCenSet) {
      boxXcen = zoom * VW->xcenter + VW->xoffset;
      boxYcen = zoom * VW->ycenter + VW->yoffset;
    }
    xcut = zoom * VW->corrBoxSize / 2.;
    glBegin(GL_LINE_STRIP);
    glVertex2f(boxXcen - xcut, boxYcen - xcut);
    glVertex2f(boxXcen - xcut, boxYcen + xcut);
    glVertex2f(boxXcen + xcut, boxYcen + xcut);
    glVertex2f(boxXcen + xcut, boxYcen - xcut);
    glVertex2f(boxXcen - xcut, boxYcen - xcut);
    glEnd();
    VW->drawCorrBox = 0;
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
void MidasGL::draw_image(MidasView *vw, b3dUInt32 *image,
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
int MidasGL::fill_viewdata( MidasView *vw)
{
  int i;
  int refz = vw->refz;

  Islice *curSlice  = midasGetSlice(vw, MIDAS_SLICE_CURRENT);

  unsigned char *currImageData;
  unsigned char *prevImageData;
  currImageData = curSlice->data.b;

  /* set previous image data pointer. */
  prevImageData = midasGetPrevImage(vw);

  if (vw->vmode == MIDAS_VIEW_COLOR){

    /* Fill each channel from the indicated image */
    for (i = 0; i < 3; i++) {
      if (vw->imageForChannel[i] == 2)
        fill_rgb(currImageData, vw->id, vw->xysize, i + 1, &vw->tr[vw->cz]);
      else if (vw->imageForChannel[i] == 1)
        fill_rgb(prevImageData, vw->id, vw->xysize, i + 1, &vw->tr[refz]);
      else
        fill_rgb(NULL, vw->id, vw->xysize, i + 1, &vw->tr[refz]);
    }
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
 *  The slice must already be transformed if needed because a slice may be returned
 * that needs shifting
 */
int MidasGL::update_slice_view(void)
{
  int i;
  Islice *curSlice = midasGetSlice(VW, MIDAS_SLICE_CURRENT);

  if (VW->vmode == MIDAS_VIEW_SINGLE)
    fill_viewdata(VW);

  if (VW->vmode == MIDAS_VIEW_COLOR) {
    for (i = 0; i < 3; i++) {
      if (VW->imageForChannel[i] == 2)
        fill_rgb(curSlice->data.b, VW->id, VW->xysize, i + 1, &VW->tr[VW->cz]);
    }
  }
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
  unsigned int state = e->modifiers();
  unsigned int button = e->button();
  bool button1 = (button == Qt::LeftButton);
  bool button2 = (button == Qt::MidButton);
  bool button3 = (button == Qt::RightButton);
  bool control = (state & Qt::ControlModifier) != 0;
  bool shift = (state & Qt::ShiftModifier) != 0;
  float xcen, ycen, mindist, xcont, ycont, xvec, yvec, xit, yit, xst, yst;
  float *xControl, *yControl, *xVector, *yVector;
  int newcur, nControl, nxt, nyt;
  int iz = VW->numChunks ? VW->curChunk : VW->cz;

  // Button press: record position (adjust for inverted Y),
  // set new center if Ctrl-middle
  VW->lastmx = e->x();
  VW->lastmy = VW->height - e->y();
  VW->firstmx = VW->lastmx;
  VW->firstmy = VW->lastmy;
  xcen = (VW->lastmx - VW->xoffset) / VW->truezoom;
  ycen = (VW->lastmy - VW->yoffset) / VW->truezoom;

  // Convert position to warp file coordinates
  if (VW->curWarpFile >= 0) {
    xcont = xcen * VW->warpScale;
    ycont = ycen * VW->warpScale;
  }

  mMousePressed = true;
  if (button2 && control && !VW->editWarps) {

    // Set a new center
    VW->drawCorrBox = 2;
    VW->xcenter = xcen;
    VW->ycenter = ycen;
    draw();

  } else if (button3 && control && VW->xtype != XTYPE_MONT && !VW->editWarps) {

    // Set fixed point
    VW->xfixed = xcen;
    VW->yfixed = ycen;
    VW->useFixed = 1 - VW->useFixed;
    manageMouseLabel(" ");
    draw();
  } else if (button1 && control) {
    manageMouseLabel("PANNING IMAGE");
  } else if (button1 && ! shift) {
    manageMouseLabel(VW->editWarps ? "SHIFTING AT PT" : "TRANSLATING");
    if (VW->editWarps)
      mBut1downt.start();
  
  } else if (VW->xtype != XTYPE_MONT && !VW->editWarps) {
    if (button2 && !(control || shift))
      manageMouseLabel("ROTATING");
    if (button3 && !control) {
      if (shift)
        manageMouseLabel("CHANGING MAG");
      else
        manageMouseLabel(VW->useFixed ? "2 PT STRETCHING" : "STRETCHING");
    }

  } else if (VW->editWarps && button1 && shift) {
    attachControlPoint();

  } else if (VW->editWarps && !shift && !control) {

    if (button3) {

      // Move a warp point
      if (VW->curControl >= 0 && 
          !getWarpPointArrays(iz, &xControl, &yControl, &xVector, &yVector)) {
        VW->drawCorrBox = 2;
        xControl[VW->curControl] = xcont;
        yControl[VW->curControl] = ycont;
        update_slice_view();
        VW->changed = 1;
      }

    } else if (button2) {

      // Find nearest point in order to see if it is too close to existing one
      nControl = 0;
      if ((iz >= VW->warpNz || !getNumWarpPoints(iz, &nControl)) && 
          nearestControlPoint(iz, mindist) > -2) {

        
        // Add a point
        if (mindist > 20. * 20.) {
          
          // need to initialize to current warping there
          xvec = 0.;
          yvec = 0.;
          if (nControl > 3) {
            newcur = fillWarpingGrid(iz, &nxt, &nyt, &xst, &yst, &xit, &yit);
            //printf("gwg %d\n", newcur);
            if (!newcur) 
              interpolateGrid(xcont, ycont, VW->gridDx, VW->gridDy, nxt, nxt, nyt, xst,
                              yst, xit, yit, &xvec, &yvec);
          }
          newcur = addWarpPoint(iz, xcont, ycont, xvec, yvec) - 1;
          //printf("newcur %d %f %f %f %f\n", newcur, xcont, ycont, xvec, yvec);
          if (newcur >= 0) {
            VW->warpNz = B3DMAX(VW->warpNz, iz + 1);
            VW->changed = 1;
            newCurrentControl(newcur, true);
          }
        }
      }        
    }
  }
}

// Release: update parameters now, also do warp point attach
void MidasGL::mouseReleaseEvent ( QMouseEvent * e )
{
  if (VW->editWarps && e->button() == Qt::LeftButton && 
      (e->modifiers() & (Qt::ControlModifier | Qt::ShiftModifier)) == 0 &&
      mBut1downt.elapsed() <= 250)
    attachControlPoint();
  VW->midasSlots->update_parameters();
  mMousePressed = false;
  manageMouseLabel(" ");
}


// Move event with a button down
void MidasGL::mouseMoveEvent ( QMouseEvent * e )
{
  unsigned int state = e->modifiers();
  unsigned int buttons = e->buttons();
  bool button1 = (buttons & Qt::LeftButton);
  bool button2 = (buttons & Qt::MidButton);
  bool button3 = (buttons & Qt::RightButton);
  bool control = (state & Qt::ControlModifier) != 0;
  bool shift = (state & Qt::ShiftModifier) != 0;
  int cumdx, cumdy;
  int cumthresh = 6 * 6;

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
    if (control)
      VW->midasSlots->mouse_shift_image();
    else if (! shift) {
      cumdx = VW->firstmx - VW->mx;
      cumdy = VW->firstmy - VW->my;
      if (!VW->editWarps || mBut1downt.elapsed() > 250 || 
          cumdx * cumdx + cumdy * cumdy > cumthresh)
        VW->midasSlots->mouse_translate();
      else {

        // For delayed shifting of control point, update last mouse to prevent lurch
        VW->lastmx = VW->mx;
        VW->lastmy = VW->my;
      }
    }

    // Button 2 is to rotate
  } else if (VW->xtype != XTYPE_MONT && !VW->editWarps && 
	     !button1 && button2 && !button3  && !(control || shift)) {
    VW->midasSlots->mouse_rotate();
    

    // Button 3 is for stretch or mag
  } else if (VW->xtype != XTYPE_MONT && !VW->editWarps &&
	     !button1 && !button2 && button3  && !control) {
    VW->midasSlots->mouse_stretch(state);
    
    // Only if no action do we update the last position
  } else {
    VW->lastmx = VW->mx;
    VW->lastmy = VW->my;
  }
  VW->mousemoving = 0;
}

/* Find the nearest control point to the stored mouse position */
int MidasGL::nearestControlPoint(int iz, float &mindist)
{
  int newcur = -1, nControl = 0;
  float *xControl, *yControl, *xVector, *yVector;
  float dx, dy, dist;
  float xcont = VW->warpScale * (VW->lastmx - VW->xoffset) / VW->truezoom;
  float ycont = VW->warpScale * (VW->lastmy - VW->yoffset) / VW->truezoom;

  if ((iz >= VW->warpNz || !getNumWarpPoints(iz, &nControl)) && 
      (!nControl || !getWarpPointArrays(iz, &xControl, &yControl, &xVector, &yVector))) {
    mindist = 1.e30;
    for (int i = 0; i < nControl; i++) {
      dx = xControl[i] - xcont;
      dy = yControl[i] - ycont;
      dist = dx * dx + dy * dy;
      if (dist < mindist) {
        newcur = i;
        mindist = dist;
      }
    }
    return newcur;
  }
  return -2;
}

/* Update display for a new current control point */ 
void MidasGL::newCurrentControl(int newcur, bool updateSlice)
{
  VW->curControl = newcur;
  reduceControlPoints(VW);
  VW->midasSlots->updateWarpEdit();
  VW->drawCorrBox = 2;
  if (updateSlice)
    update_slice_view();
  else
    draw();
}

/* Attach to the nearest control point */
void MidasGL::attachControlPoint()
{
  float mindist;
  int iz = VW->numChunks ? VW->curChunk : VW->cz;
  int newcur = nearestControlPoint(iz, mindist);
  if (newcur >= 0)
    newCurrentControl(newcur, false);
}

/* Modify the mouse function labels as appropriate */    
void MidasGL::manageMouseLabel(const char *string)
{
  int i, active = 0;
  QString ctap = QString(CTRL_STRING).left(2);
  QString label;
  if (VW->ctrlPressed)
    active = 2;
  else if (VW->shiftPressed)
    active = 1;
  for (i = 0; i < 3; i++)
    diaSetWidgetColor(VW->mouseLabel[i], QColor(i == active ? "red" : "blue"),
                      QPalette::Foreground);
  if (mMousePressed)
    VW->mouseLabel[active]->setText(QString(string));
  if (!mMousePressed || active != 0) {
    label = VW->editWarps ? "shift/sel -- " : "shift -- ";
    if (VW->xtype == XTYPE_MONT)
      label += "       -- ";
    else
      label += VW->editWarps ? "add pt -- " : "rotate -- ";
    if (VW->xtype != XTYPE_MONT)
      label += VW->editWarps ? "move pt" : (VW->useFixed ? "2 pt stretch" : "stretch");

    VW->mouseLabel[0]->setText(label);
  }
  if (!mMousePressed || active != 1) {
    if (VW->editWarps)
      label = "Sh: Select pt --        --          ";
    else
      label = QString("Sh:        --        -- ") + 
        (VW->xtype != XTYPE_MONT ? "magnify   " : "         ");
    VW->mouseLabel[1]->setText(label);
  }
  if (!mMousePressed || active != 2) {
    label = ctap + ": pan -";
    if (VW->editWarps) 
      label += "-       --        ";
    else {
      label += " new center - ";
      label += VW->xtype == XTYPE_MONT ? "         " : 
        (VW->useFixed ? "no 2 pt" : "fixed pt");
    }
    VW->mouseLabel[2]->setText(label);
  }
}

void MidasGL::drawStar(float xcen, float ycen, float censize)
{
  glDisable(GL_LINE_STIPPLE);
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
