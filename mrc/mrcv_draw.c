/*  IMOD VERSION 2.02
 *
 *  mrcv_draw.c -- drawing routines for mrcv using gl.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1994-1996 by Boulder Laboratory for 3-Dimensional Fine    *
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

/* System include files */
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <gl/image.h>
#include <gl/gl.h>
#include <gl/device.h>
#include <limits.h>
#include <sys/types.h>
#include "imodel.h"

/* MRC include files    */
#include "mrcv.h"


/* functions. */
int mrcv_montage(int winx, int winy, struct ViewInfo *vi);
void mrcv_zoomget(short *buf, int y);
void mrcv_zoomput(short *buf, int y);
void draw_model(int llx, int lly, int urx, int ury, struct ViewInfo *vi,
		struct Mod_Model *imod);
void draw_contour(struct Mod_Model *imod, struct Mod_Object *obj,
		  struct ViewInfo *vi, int co, int ob);

/* software zoom globals. */
Colorindex *Mrcv_szoomdat;
int   Mrcv_sxsize;
int   Mrcv_sysize;
int   Mrcv_xstart;
int   Mrcv_ystart;

/* set Stereo Hardware */
void setsthw(int stmode)
{
     static int sthw = 0;

     if (stmode == 0)
	  if (sthw){
	       system("/usr/gfx/setmon -n 72HZ");
	       sthw = 0;
	  }

     if (stmode == 3)
	  if (!sthw){
	       system("/usr/gfx/setmon STR_RECT");
	       sthw = 1;
	  }
}

void killsthw(void)
{
     setsthw(0);
}

int sthw(struct ViewInfo *vi)
{
     static long lox, loy;
     static long lwx, lwy;

     if (Mrcv_stereo == 3){

	  Mrcv_stereo = 0;
	  prefposition(lox, loy, lox+lwx, loy+lwy);
	  minsize(10 , 10);
	  maxsize(getgdesc(GD_XPMAX), getgdesc(GD_YPMAX));
	  winconstraints();

     }else{
	  getsize(&lwx, &lwy);
	  getorigin(&lox, &loy);

	  prefposition(0l, getgdesc(GD_XPMAX), 0l, getgdesc(GD_YPMAX));
	  winconstraints();
	  Mrcv_stereo = 3;
     }
     setsthw(Mrcv_stereo);
     reshapeviewport();
     setview(vi);
     qreset();
     return 0;
}

void setview(struct ViewInfo *vi)
{
     long winx, winy;
     getsize((long *)&winx, (long *)&winy);

     color(Rampbase + Mrcv_background);
     clear();

     if (Mrcv_stereo){
	  int lz, cz, rz;

	  cz = vi->zmouse;
	  lz = vi->zmouse + Mrcv_plax;
	  if (lz >= vi->zsize) lz = vi->zsize - 1;
	  if (lz < 0) lz = 0;
	  

	  rz = vi->zmouse - Mrcv_plax;
	  if (rz >= vi->zsize) rz = vi->zsize - 1;
	  if (rz < 0) rz = 0;
	  
	  lz = cz;

	  switch(Mrcv_stereo){
	     case 1:
	       vi->zmouse = lz;
	       draw_image(0, 0, (winx / 2) - 5, winy, vi);
	       vi->zmouse = rz;
	       draw_image((winx / 2) + 5,0, winx, winy, vi);
	       break;
	     
	     case 2:
	       vi->zmouse = lz;
	       draw_image(0, 0, winx, (winy/2) - 5, vi);
	       vi->zmouse = rz;
	       draw_image(0, (winy/2) + 5, winx, winy, vi);
	       break;

	     case 3:
	       vi->zmouse = lz;
	       draw_image(0, 0, winx, (winy/2) - 20, vi);
	       vi->zmouse = rz;
	       draw_image(0, (winy/2) + 20, winx, winy, vi);
	       break;
	  }
	  vi->zmouse = cz;
     }else{

	  if ((Mrcv_xmont > 1) || (Mrcv_ymont > 1))
	       mrcv_montage(winx, winy, vi);
	  else{
	       draw_image(0,0,winx,winy,vi);
	       if (Mrcv_model)
		    draw_model(0,0,winx,winy,vi, Mrcv_model);
	  }
     }

     swapbuffers();
     return;
}

int mrcv_montage(int winx, int winy, struct ViewInfo *vi)
{
     int xstep;
     int ystep;
     int x,y,z;
     int xpos = 0, ypos = 0;

     int bd;
     bd = Mrcv_border;

     xstep = winx / Mrcv_xmont;
     ystep = winy / Mrcv_ymont;

     z = vi->zmouse;

     ypos = ystep * (Mrcv_ymont - 1);


     if (Mrcv_onside){
	  ypos = 0;
	  for (x = 0; x < Mrcv_xmont; x++){
	       for(y = 0;  y < Mrcv_ymont; y++, vi->zmouse++){
		    if (vi->zmouse >= vi->zsize)
			 break;
		    draw_image( bd + xpos, bd + ypos,
			       xpos + xstep - bd, ypos + ystep - bd, vi);
		    ypos += ystep;
	       }
	       xpos += xstep;
	       ypos = 0;
	  }
     }else{
	  for(y = 0;  y < Mrcv_ymont; y++){
	       if (vi->zmouse >= vi->zsize)
		    break;
	       for(x = 0; x < Mrcv_xmont; x++, vi->zmouse++)
		    {
			 if (vi->zmouse >= vi->zsize)
			      break;
			 draw_image( bd + xpos, bd + ypos, 
				    xpos + xstep - bd, ypos + ystep - bd, vi);
			 xpos += xstep;
		    }
	       ypos -= ystep;
	       xpos = 0;
	  }
     }
     vi->zmouse = z;
     return(0);
}

int imod_zap_getx(struct ViewInfo *vi, double x)
{
     return( (int)(((x - vi->xstart) * vi->zoom) + vi->xborder));
}

int imod_zap_gety(struct ViewInfo *vi, double y)
{
     return( (int)(((y - vi->ystart) * vi->zoom) + vi->yborder));
}

void draw_contour(struct Mod_Model *imod,
		  struct Mod_Object *obj,
		  struct ViewInfo *vi,
		  int co, int ob)
{
     int pt, obcolor;
     struct Mod_Contour *cont;
     struct Mod_Point *point;
     float vert[3];

     cont = &(obj->cont[co]);
     if ((!cont->pts) || (!cont->psize))
	  return;
     
     /* draw closed contour with lines. */
     if ((!(obj->flags & IMOD_OBJFLAG_OPEN)) &&
	 (!(obj->flags & IMOD_OBJFLAG_SCAT))){
	  if (cont->pts[0].z != vi->zmouse)
	       return;
	  bgnline();
	  for (pt = 0; pt < cont->psize; pt++){
	       point = &(cont->pts[pt]);
	       vert[0] = imod_zap_getx(vi, point->x);
	       vert[1] = imod_zap_gety(vi, point->y);
	       vert[2] = point->z;
	       v2f(vert);
	  }
	  point = &(cont->pts[0]);
	  vert[0] = imod_zap_getx(vi, point->x);
	  vert[1] = imod_zap_gety(vi, point->y);
	  vert[2] = point->z;
	  v2f(vert);

	  
	  endline();
	  return;
     }

     if (( obj->flags & IMOD_OBJFLAG_OPEN) ||
	 ( obj->flags & IMOD_OBJFLAG_SCAT)){
	  for (pt = 0; pt < cont->psize; pt++){
	       if ((int)cont->pts[pt].z == vi->zmouse)
		    circ(imod_zap_getx(vi, cont->pts[pt].x),
			 imod_zap_gety(vi, cont->pts[pt].y),
			 obj->pdrawsize - 1);
	  }
     }

     return;
}


void draw_model(int llx, int lly, int urx, int ury, struct ViewInfo *vi,
	   struct Mod_Model *imod)
{
     long xsize, ysize;
     int ob, co, pt, obcolor;
     short red, green, blue;
     struct Mod_Object *obj;
     struct Mod_Contour *cont;
     

     getsize(&xsize, &ysize);

     ortho2(-0.5, xsize - 0.5, -0.5, ysize - 0.5);

     scrmask(llx, urx, lly, ury);
     pushmatrix();

     translate( llx, lly, 0); 

     
     for (ob = 0; ob < imod->objsize; ob++){
	  obj = &(imod->obj[ob]);
	  if (obj->flags  & IMOD_OBJFLAG_OFF)
	       continue;
	  if (!obj->drawmode)
	       continue;
	  obcolor = (ob + vi->rampbase + 256);
	  red = obj->red * 255.0;
	  green = obj->green * 255.0;
	  blue = obj->blue * 255.0;
	  mapcolor((Colorindex)obcolor, red, green, blue);
	  linewidth (obj->linewidth);
	  color(obcolor);
	  for (co = 0; co < obj->contsize; co++){
	       draw_contour(imod, obj, vi, co, ob);
	  }
     }

     popmatrix();
     scrmask(0, xsize, 0, ysize);
}

int draw_image( int llx, int lly, int urx, int ury, struct ViewInfo *vi)
{
     int i,j;
     int winx, winy;
     int xin, yin, xout, yout;
     unsigned char *pixptr;
     float xzoom = vi->zoom, yzoom = vi->zoom;

     if (Mrcv_stereo == 3)
	  xzoom *= 2.0f;

     vi->xstart = 0;
     vi->ystart = 0;
     vi->xborder = 0;
     vi->yborder = 0;

     if (Mrcv_onside){
	  draw_image_onside(llx,lly,urx,ury,vi);
	  return(0);
     }

     /* Initialize variables. */
     winx = urx - llx;
     winy = ury - lly;

     /* Set up starting x coord. */
     /* If there is a extra room in the window. */
     if ( ((vi->xsize - 1) * xzoom) < winx ){  
	  vi->xdrawsize = (vi->xsize);
	  vi->xborder = ( winx - ((vi->xsize - 1) * xzoom)) / 2;
     }

     else{
	  vi->xdrawsize = winx / xzoom;
	  vi->xstart = (vi->xsize / 2 ) - (winx / xzoom / 2);
	  vi->xstart -= vi->xtrans;
	  if (vi->xstart < 0){
	       vi->xstart += vi->xtrans;
	       vi->xtrans = vi->xstart;
	       vi->xstart -= vi->xtrans;
	  }
	  if ( (vi->xstart + vi->xdrawsize) > (vi->xsize - 1)){
	       vi->xstart += vi->xtrans;
	       vi->xtrans =vi-> xstart - (vi->xsize - 1 - vi->xdrawsize);
	       vi->xstart -= vi->xtrans;
	  }
     }

     if ( ((vi->ysize - 1) * yzoom) < winy ){
	  vi->ydrawsize = (vi->ysize);
	  vi->yborder = ( (float)winy - ((float)(vi->ysize - 1) * yzoom)) 
	       * 0.5f;
     }
     else{
	  vi->ydrawsize = winy / yzoom;
	  vi->ystart = (vi->ysize / 2 ) - (winy / yzoom / 2);
	  vi->ystart -= vi->ytrans;
	  if (vi->ystart < 0){
	       vi->ystart += vi->ytrans;
               vi->ytrans = vi->ystart;
               vi->ystart -= vi->ytrans;
	  }
	  if ( (vi->ystart + vi->ydrawsize) > (vi->ysize - 1)){
	       vi->ystart += vi->ytrans;
	       vi->ytrans = vi->ystart - (vi->ysize - 1 - vi->ydrawsize);
	       vi->ystart -= vi->ytrans;
	  }
     }
     
     /* Load byte data into Colorindex array for viewing. */
     pixptr = &(vi->idata[vi->zmouse][0]);
     pixptr += (vi->ystart * (vi->xsize));
     pixptr += vi->xstart;

     for (j = 0; j < vi->ydrawsize; j++){
	  for (i = 0; i < vi->xdrawsize; i++, pixptr++)
	       vi->viewdata[(j * vi->xdrawsize) + i] = *pixptr + vi->rampbase;
	  pixptr += vi->xsize;
	  pixptr -= vi->xdrawsize;
     }

     /*
     printf("zoom = %g %g : winsize = %d %d\n", 
	    xzoom , yzoom, winx, winy);
     printf("org = %d %d\n", llx, lly);
	 printf("border = %d %d\n", vi->xborder, vi->yborder);
	 printf("drawsi = %d %d\n", vi->xdrawsize, vi->ydrawsize);
	 */

     if ((Mrcv_szoom) && (vi->zoom > 1.0)){
	  
	  Mrcv_sxsize = vi->xdrawsize;
	  Mrcv_xstart = vi->xstart;
	  Mrcv_ystart = vi->ystart;
	  xin = vi->xdrawsize;
	  yin = vi->ydrawsize;
	  xout = xin * xzoom;
	  yout = yin * yzoom;
	  Mrcv_szoomdat = (Colorindex *)malloc
	       (xout * yout * sizeof(Colorindex));
	  if (!Mrcv_szoomdat)
	       return(-1);

	  filterzoom(mrcv_zoomget, mrcv_zoomput,
		     xin, yin, xout, yout,
		     Mrcv_szoom, 1.0);
	  rectzoom(1.0f, 1.0f);
	  rectwrite(llx + vi->xborder, lly + vi->yborder, 
		    llx + vi->xborder + xout - 1,
		    lly + vi->yborder + yout - 1,
		    Mrcv_szoomdat);
	  free(Mrcv_szoomdat);

     }else{

     
	  /* Draw image. */
	  rectzoom(xzoom, yzoom);

	  rectwrite(llx + vi->xborder, lly + vi->yborder, 
		    llx + vi->xborder + vi->xdrawsize - 1,
		    lly + vi->yborder + vi->ydrawsize - 1,
		    vi->viewdata);
     }
     return(0);
}



int draw_image_onside( int llx, int lly, int urx, int ury, struct ViewInfo *vi)
{
     int i,j;
     int winx, winy;
     int xsize, ysize;
     int xdrawsize,   ydrawsize;
     int xborder = 0, yborder = 0;
     int xstart = 0,  ystart  = 0;
     unsigned char *pixptr;
     
     xsize =  vi->xsize;
     ysize =  vi->ysize;
     vi->xsize = ysize;
     vi->ysize = xsize;

     /* Initialize variables. */
     winx = urx - llx;
     winy = ury - lly;

     /* Set up starting x coord. */
     /* If there is a extra room in the window. */
     if ( ((vi->xsize - 1) * vi->zoom) < winx ){
	  xdrawsize = (vi->xsize);
	  xborder = ( winx - ((vi->xsize - 1) * vi->zoom)) / 2;
     }
     
     else{
	  xdrawsize = winx / vi->zoom;
	  xstart = (vi->xsize / 2 ) - (winx / vi->zoom / 2);
	  xstart -= vi->xtrans;
	  if (xstart < 0){
	       xstart += vi->xtrans;
	       vi->xtrans = xstart;
	       xstart -= vi->xtrans;
	  }
	  if ( (xstart + xdrawsize) > (vi->xsize - 1)){
	       xstart += vi->xtrans;
	       vi->xtrans = xstart - (vi->xsize - 1 - xdrawsize);
	       xstart -= vi->xtrans;
	  }
     }
     
     if ( ((vi->ysize - 1) * vi->zoom) < winy ){
	  ydrawsize = (vi->ysize);
	  yborder = ( winy - ((vi->ysize - 1) * vi->zoom)) / 2;
     }

     else{
	  ydrawsize = winy / vi->zoom;
	  ystart = (vi->ysize / 2 ) - (winy / vi->zoom / 2);
	  ystart -= vi->ytrans;
	  if (ystart < 0){
	       ystart += vi->ytrans;
	       vi->ytrans = ystart;
	       ystart -= vi->ytrans;
	  }
	  if ( (ystart + ydrawsize) > (vi->ysize - 1)){
	       ystart += vi->ytrans;
	       vi->ytrans = ystart - (vi->ysize - 1 - ydrawsize);
	       ystart -= vi->ytrans;

	       
	  }
     }
     
     /* Load byte data into Colorindex array for viewing. */
     /* Onside twist. */
     pixptr = &(vi->idata[vi->zmouse][0]);
     pixptr += (xstart * xsize);
     pixptr += xsize - ydrawsize - ystart;
     
     for (i = xdrawsize - 1; i >= 0; i--){
	  for (j = 0; j < ydrawsize; j++, pixptr++)

	       vi->viewdata[(j * xdrawsize) + i ] = *pixptr + vi->rampbase;

	  pixptr += xsize;
	  pixptr -= ydrawsize;
     }

     rectzoom(vi->zoom, vi->zoom);
     rectwrite(llx + xborder, lly + yborder,
	       llx + xborder + xdrawsize - 1,
	       lly + yborder + ydrawsize - 1,
	       vi->viewdata);

     vi->xsize = xsize;
     vi->ysize = ysize;
     return 0;
}

void mrcv_bzoomget(short *buf, int y)
{
     int x;

     unsigned char *ptr;
     
     ptr = &(Mrcv_vi->idata[Mrcv_vi->zmouse][0]);
     ptr += Mrcv_xstart * Mrcv_vi->xsize;
     ptr += Mrcv_vi->xsize + Mrcv_sysize + Mrcv_ystart;

     for(x = 0; x < Mrcv_sysize; x++, ptr++)
	  buf[x] = (short)*ptr;
}

void mrcv_zoomget(short *buf, int y)
{
     int x;
     unsigned char *ptr; 
     

     ptr = &(Mrcv_vi->idata[Mrcv_vi->zmouse][0]); 
     ptr += (Mrcv_ystart + y) * Mrcv_vi->xsize; 
     ptr += Mrcv_xstart; 

     for (x = 0; x < Mrcv_sxsize; x++, ptr++)
	  buf[x] = (short)*ptr;

}

void mrcv_zoomput(short *buf, int y)
{
     int x;
     int xsize;
     
     Colorindex *ptr;
     ptr = Mrcv_szoomdat;

     xsize = Mrcv_sxsize * Mrcv_vi->zoom;
     ptr += y * xsize;

     for(x = 0; x < xsize; x++, ptr++)
	  *ptr = (Colorindex)buf[x] + Mrcv_vi->rampbase; 
}
