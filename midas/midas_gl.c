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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <Xm/Xm.h>
#include <mrcc.h>
#include "midas.h"


Window glwWindow(w)
     Widget w;
{
     return(XtWindow(w));
}


 


/* Clear the image display area. */
void midas_clear(struct Midas_view *vw)
{
     glXMakeCurrent(XtDisplay(vw->glw), XtWindow(vw->glw), vw->glxcontext);
     glClear(GL_COLOR_BUFFER_BIT);

     return;
}

void midas_draw(struct Midas_view *vw)
{
     int xdrawn, ydrawn;
     float xcut, ycut;
     double angle, tcrit;
     int i;
     int numlines = 2;
     float xcen, ycen;
     float zoom = VW->truezoom;
     float censize = 7.;

     glXMakeCurrent(XtDisplay(vw->gfx), XtWindow(vw->gfx), vw->glxcontext);

     /* DNM 2/5/01: set sdatSize here.  This may be redundant now that the
	expose_cb tests correctly for need for new array */
     if (!vw->sdat) {
	  vw->sdat = (unsigned long *)
	       malloc( vw->width * vw->height * sizeof(unsigned long));
	  vw->sdatSize = vw->width * vw->height;
     }

     draw_image(vw, vw->id, 0, 0, vw->width, vw->height, &xdrawn, &ydrawn);
     
     if (VW->paramstate[2] < 1.0001 && VW->paramstate[2] > 0.9999)
	  numlines = 1;

     if (vw->showref)
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
     
     /* First line is for the defined stretch angle */
     
     tcrit = atan(((double)ydrawn)/xdrawn);
     glEnable(GL_LINE_STIPPLE);
     glLineStipple(10,0x5555);
     
     glColor3f(1.0f, 0.0f, 0.0f);
     angle = 0.1 * vw->sangle * RADIANS_PER_DEGREE;

     for (i = 0; i < numlines; i ++) {
	  if (angle > tcrit || angle < -tcrit) {
	       ycut = ydrawn / 2.;
	       xcut = ycut * cos(angle) / sin(angle);
	  } else {
	       xcut = xdrawn / 2.;
	       ycut = xcut * tan(angle);
	  }
	  glBegin(GL_LINES);
	  glVertex2f(vw->width / 2. - xcut, vw->height / 2. - ycut);
	  glVertex2f(vw->width / 2. + xcut, vw->height / 2. + ycut);
	  glEnd();

	  /* now actual stretch axis, conditional on existence of stretch */
	  glColor3f(0.0f, 0.4f, 1.0f);
	  angle = VW->phi * RADIANS_PER_DEGREE;
     }

     glFlush();
     glXSwapBuffers(XtDisplay(vw->gfx), XtWindow(vw->gfx));
     return;

}


void draw_stretch_angle(void)
{
     struct Midas_view *vw = VW;

     /*  midas_clear(vw); */
     midas_draw(vw);
     return;
}



/* fill in image data */
/* Copy the data in fbuf to the tobuf.
 */
void fill_rgb(unsigned char *fbuf, unsigned long *tobuf, 
	      int size, int channel, struct Midas_transform *tr)
{
     unsigned char *buf = (unsigned char *)tobuf;
     unsigned long tmp, i;
     unsigned char cmap[256];
     unsigned int cmapi[256];
     unsigned char *cmap2;
     int rampsize;
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
	  cmap[i] = point;
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
void draw_image(struct Midas_view *vw, unsigned long *image,
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
     unsigned long *data;
     unsigned long *bdata;
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
	  xborder = ( swinx - ((xsize - 1) * zoom)) / 2;
     }
     else{
	  xdrawsize = swinx / zoom;
	  xstart = (xsize / 2 ) - (swinx / zoom / 2);
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
	  yborder = ( swiny - ((ysize - 1) * zoom)) / 2;
     }
     else{
	  ydrawsize = swiny / zoom;
	  ystart = (ysize / 2 ) - (swiny / zoom / 2);
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
     vw->xoffset = xborder - zoom * xstart;
     vw->yoffset = yborder - zoom * ystart;

     if (vw->zoom < 0) {

	  /* fractional zoom: adjust sizes and zooms, copy selected pixels */
	  iskip = -vw->zoom;
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
	  xdrawsize *= 1.5;
	  if (xdrawsize > swinx)
	       xdrawsize = swinx;
	  ydrawsize *= 1.5;
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
     *xdrawn = xdrawsize * glzoom;
     *ydrawn = ydrawsize * glzoom;

     return;
}


/* Refill all image data buffers with refreshed data. */
int fill_viewdata( struct Midas_view *vw)
{
     int sn = vw->cz;
     int i;
     unsigned long mt = 0x007f7f7f;
     int xfd;
     int refz = vw->refz;

     Islice *prevSlice;
     Islice *curSlice  = midasGetSlice(VW, MIDAS_SLICE_CURRENT, &xfd);

     
     unsigned char *currImageData = curSlice->data.b;
     unsigned char *prevImageData = NULL;

     /* set previous image data pointer. */
     switch(vw->xtype){
	case XTYPE_XF:
	case XTYPE_MONT:
	  prevSlice = midasGetSlice(VW, MIDAS_SLICE_OPREVIOUS, &xfd);
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
	  prevSlice = midasGetSlice(VW, MIDAS_SLICE_PREVIOUS, &xfd);
	  if (prevSlice)
	       prevImageData = prevSlice->data.b;
	  break;
     }


     if (vw->vmode == MIDAS_VIEW_COLOR){
	  
	  /* fill the current transformed slice image data. */
	  fill_rgb(currImageData, vw->id, vw->xysize, 2, &VW->tr[vw->cz]);

	  /* fill the previous slice image data. */
	  fill_rgb(prevImageData, vw->id, vw->xysize, 3, &vw->tr[refz]);
	  fill_rgb(prevImageData, vw->id, vw->xysize, 1, &vw->tr[refz]);
	  return(0);
     }	  
     
     if (vw->vmode == MIDAS_VIEW_SINGLE){
	  if (vw->showref){
	       fill_rgb(prevImageData, vw->id, vw->xysize, 0, &VW->tr[refz]);
	  }else{
	       fill_rgb(currImageData, vw->id, vw->xysize, 0, &VW->tr[vw->cz]);
	  }
     }

     return 0;
}


/* 
 *  update the just transformed slice,
 *  The slice must already be transformed if needed.
 */
int update_slice_view(void)
{
     int xformed;
     Islice *curSlice = midasGetSlice(VW, MIDAS_SLICE_CURRENT, &xformed);

     if (VW->vmode == MIDAS_VIEW_SINGLE)
	  fill_viewdata(VW);

     if (VW->vmode == MIDAS_VIEW_COLOR)
	  fill_rgb(curSlice->data.b, VW->id, 
		   VW->xysize, 2, &VW->tr[VW->cz]);

     midas_draw(VW);
     return(0);
}
