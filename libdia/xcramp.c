/*****************************************************************************
 *                                                                           *
 *   FILE: xcramp.c                                                          *
 *                                                                           *
 *   PURPOSE: X11 color ramp control.                                        *
 *                                                                           *
 *   HISTORY:                                                                *
 *       Version 1.0  James Kremer                                           *
 *       Version 1.1  David Mastronarde  mast@colorado.edu                   *
 *                                                                           *
 *****************************************************************************
 *   Copyright (C) 1994-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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
#include "xcramp.h"

#define GSCALE_RAMPSIZE 128

/* Inits a colormap that has all of its colors allocated to current process. */
/* DNM: This also works for TrueColor, but there is no need to allocate
   XColors, because OpenGL does colors separately */
Cramp *xcramp_allinit(Display *display, XVisualInfo *visual, Colormap colormap,
		      unsigned long low, unsigned long high)
{
    Cramp *cramp;
    int value, i;
    int red, green, blue;
    XColor acolor;
    long maxval = 1;
    
    if ((visual->class == PseudoColor) || (visual->class == GrayScale)){
	for(i=0; i < visual->depth;i++)
	    maxval*=2;
	if (high > maxval)
	    high = maxval;
	if (low > high){
	    maxval = (long)low;
	    low = high;
	    high = maxval;
	}
    }

     cramp = (Cramp *)malloc(sizeof(Cramp));
     cramp->visual     = visual->visual;
     cramp->blacklevel = 0;
     cramp->whitelevel = 255;
     cramp->reverse    = False;
     cramp->falsecolor = False;
     cramp->display    = display;
     cramp->cmap       = colormap;
     cramp->depth      = visual->depth;
     cramp->rampbase   = (int)low;
     cramp->rampsize   = (int)high - low + 1;
     cramp->scale      = (cramp->rampsize - 1)/ 255.0f;
     cramp->xcolor     = (XColor *)malloc((cramp->rampsize+1) 
					  * sizeof(XColor));
     cramp->pixels     = NULL;
     if (visual->class == TrueColor) 
	  cramp->rampbase = -1;     /* signal for not a ramp but a look up */

     cramp->clevel     = 0;
     cramp->noflevels = 0;
     xcrampStoreInit(cramp, 4);

     for (i = 0; i < cramp->rampsize; i++){
	  value = ((i * 255) / (cramp->rampsize - 1)) << 8;
	  cramp->xcolor[i].flags = DoRed | DoGreen | DoBlue;
	  cramp->xcolor[i].pixel = low + i;
	  cramp->xcolor[i].red   = value;
	  cramp->xcolor[i].green = value;
	  cramp->xcolor[i].blue  = value;
     }
     if ((cramp->visual->class == PseudoColor) || 
	 (cramp->visual->class == GrayScale)){
	  XStoreColors(cramp->display, cramp->cmap, cramp->xcolor,
		       cramp->rampsize);
     }
     return(cramp);

}

void xcrampNewBase(Cramp *cr, int base)
{
     int i;
     if (!cr) return;
     if (cr->pixels)
	  return;
     cr->rampbase = base;
     for (i = 0; i < cr->rampsize; i++){
	  cr->xcolor[i].pixel = i + base;
     }
     return;
}

/* Inits a colormap that is shared and writeable. */
/* This is used by xmrcv */
Cramp *xcramp_init(Display *display, XVisualInfo *visual, Colormap colormap)
{
     Cramp *Xcramp;

     int value;
     int numofinds, i;
     unsigned long planes[1];

     if (visual->depth < 8)
	  return(0l);

     for(i = 1, numofinds = 2; i < visual->depth; i++)
	  numofinds *= 2;

     Xcramp = (Cramp *)malloc(sizeof(Cramp));
     Xcramp->visual     = visual->visual;
     Xcramp->blacklevel = 0;
     Xcramp->whitelevel = 255;
     Xcramp->reverse    = False;
     Xcramp->falsecolor = False;
     Xcramp->display    = display;
     Xcramp->cmap       = colormap;
     Xcramp->depth      = visual->depth;
     Xcramp->rampbase   = -1; /* use pixel array instead. */
     if (visual->depth == 8){
	  Xcramp->rampsize = GSCALE_RAMPSIZE;
     }else{
	  Xcramp->rampsize = 256;
     }

     Xcramp->xcolor = (XColor *)malloc(Xcramp->rampsize * sizeof(XColor));
     Xcramp->pixels = (unsigned long *)   
	  malloc(Xcramp->rampsize * sizeof(unsigned long));
     
     while(!XAllocColorCells(Xcramp->display,
			       Xcramp->cmap,
			       False,
			       planes, 0,
			       Xcramp->pixels,
			       Xcramp->rampsize)){
	  if (Xcramp->rampsize < 2){
	       return(0l);
	  }
	  Xcramp->rampsize--;
     }

     for (i = 0; i < Xcramp->rampsize; i++){
	  value = ((i * 255) / Xcramp->rampsize) << 8;
	  Xcramp->xcolor[i].flags = DoRed | DoGreen | DoBlue;
	  Xcramp->xcolor[i].pixel = Xcramp->pixels[i];
	  Xcramp->xcolor[i].red   = value;
	  Xcramp->xcolor[i].green = value;
	  Xcramp->xcolor[i].blue  = value;
     }

     if ((Xcramp->visual->class == PseudoColor) || 
	 (Xcramp->visual->class == GrayScale)){
	 XStoreColors(Xcramp->display, Xcramp->cmap, Xcramp->xcolor,
		      Xcramp->rampsize);
     }
     Xcramp->scale = ((float)Xcramp->rampsize/255.0f);
     Xcramp->noflevels = 0;
     xcrampStoreInit(Xcramp, 4);
     return(Xcramp);
}

/* alloc space for storage of extra color index values. */
int xcrampStoreInit(Cramp *cramp, int size)
{
     int i;

     if (size < 1)
	  return(-1);
     if (!cramp) return -1;
     if (cramp->noflevels){
	  if (cramp->blacks)
	       free(cramp->blacks);
	  if (cramp->whites)
	       free(cramp->whites);
     }
	  
     cramp->noflevels = size;
     cramp->clevel = 0;
     cramp->blacks = (int *)malloc(sizeof(int) * size);
     cramp->whites = (int *)malloc(sizeof(int) * size);
     cramp->blacks[0] = cramp->blacklevel;
     cramp->whites[0] = cramp->whitelevel;

     for(i = 1; i < size; i++){
	  cramp->blacks[i] = 0;
	  cramp->whites[i] = 255;
     }

     return(0);
}

int xcrampSelectIndex(Cramp *cramp, int index)
{
    if (!cramp) return -1;
     if (index >= cramp->noflevels)
	  return(-1);
     if (index < 0)
	  return(-1);

     cramp->blacks[cramp->clevel] = cramp->blacklevel;
     cramp->whites[cramp->clevel] = cramp->whitelevel;
     cramp->clevel = index;
     cramp->blacklevel = cramp->blacks[cramp->clevel];
     cramp->whitelevel = cramp->whites[cramp->clevel];
     return(0);
}

/*
int xcrampSetColor(Cramp *cramp, unsigned long color, 
		   unsigned short red,
		   unsigned short green, 
		   unsigned short blue)
{
     Xcolor xcolor;
     if (!xcramp) return -1;
     xcolor.flags = DoRed | DoGreen | DoBlue;
     xcolor.pixel = color;
     xcolor[i].red   = red;
     xcolor[i].green = green;
     xcolor[i].blue  = blue;
     XStoreColors(cramp->display, cramp->cmap, &xcolor, 1);
     return 0;
}
*/

unsigned short xcramp_getpixel(Cramp *xr, unsigned char val)
{
    if (!xr) return 0;
     return( (unsigned short)xr->pixels[(val * xr->rampsize) / 255] );
}

unsigned char xcramp_getval(Cramp *xr, unsigned long pixel)
{
     int i;
     int red, green, blue;
     int val;
     XColor *xcolor;
     XColor acolor;

     if (!xr) return 0;
     if (xr->rampbase == -1){
	  for (i = 0; i < xr->rampsize; i++)
	       if (xr->xcolor[i].pixel == pixel){
		    xcolor = &(xr->xcolor[i]);
		    break;
	       }
     }else{
	  xcolor = &acolor;
	  xcolor->pixel = pixel;
	  xcolor->flags =  DoRed | DoGreen | DoBlue;
	  XQueryColor(xr->display, xr->cmap, xcolor);
     }

     red   = ((xcolor->red   >> 8) * 30) / 100;
     green = ((xcolor->green >> 8) * 59) / 100;
     blue  = ((xcolor->blue  >> 8) * 11) / 100;
     val = red + green + blue;
     if (val > 255)
	  val = 255;
     if (val < 0)
	  val = 0;
     return((unsigned char)val);
}

int xcramp_delete(Cramp *xcramp)
{
     if (!xcramp)
	  return(-1);

     if ((xcramp->visual->class == PseudoColor) || 
	 (xcramp->visual->class == GrayScale)){
	  XFreeColors(xcramp->display, xcramp->cmap, xcramp->pixels,
		      xcramp->rampsize, 0xffffffff);
     }
     if (xcramp->pixels)
	  free(xcramp->pixels);
     if (xcramp->xcolor)
	  free(xcramp->xcolor);
     free(xcramp);
     return(0);
}

int xcramp_level(Cramp *xcramp, int black, int white)
{
     /* DNM 3/14/01: prevent changes from going out of bounds */
     int  minlevel = 0, maxlevel = 255;
     if (!xcramp) return -1;
     xcramp->blacklevel += black;
     if (xcramp->blacklevel < minlevel || xcramp->blacklevel > maxlevel)
	  xcramp->blacklevel -= black;
     xcramp->whitelevel += white;
     if (xcramp->whitelevel < minlevel || xcramp->whitelevel > maxlevel)
	  xcramp->whitelevel -= white;
     return(xcramp_ramp(xcramp));
}

int xcramp_falsecolor(Cramp *xcramp, int flag)
{
    if (!xcramp) return -1;
     xcramp->falsecolor = flag;
     return(xcramp_ramp(xcramp));
}

int xcramp_reverse(Cramp *xcramp, int flag)
{
    if (!xcramp) return -1;
     xcramp->reverse = flag;
     return(xcramp_ramp(xcramp));
}

void xcramp_getlevels(Cramp *xcramp, int *black, int *white)
{
    if (!xcramp) return;
    *black = xcramp->blacklevel;
    *white = xcramp->whitelevel;
}

void xcramp_setlevels(Cramp *xcramp, int black, int white)
{
    if (!xcramp) return;
    xcramp->blacklevel = black;
    xcramp->whitelevel = white;
    xcramp_ramp(xcramp);
    return;
}

int xcramp_ramp(Cramp *cr)
{
     int  i, ival, val;
     int  rampsize;
     unsigned short *cmap;
     int  red, green, blue;
     int  minlevel = 0, maxlevel = 255;
     float slope,   /* The slope of the ramp. */
     point;   /* Temp variable to store colormap index as float. */
     unsigned long *pixtmp;
     unsigned char *ramppt;

     cmap = cr->index;
     if (!cr) return -1;
     /* Keep input variables in bounds. */
     if (cr->blacklevel > cr->whitelevel)
	  cr->blacklevel = cr->whitelevel;
     if (cr->whitelevel < cr->blacklevel)
	  cr->whitelevel = cr->blacklevel;
     if (cr->blacklevel < minlevel)
	  cr->blacklevel = minlevel;
     if (cr->whitelevel > maxlevel)
	  cr->whitelevel = maxlevel;
     
     /* write out saturated values to cmap. */
     for (i = 0; i < cr->blacklevel; i++)
	 cmap[i] = 0;
     for (i = cr->whitelevel; i < 256; i++)
	  cmap[i] = 255;

     /* calculate rampsize and slope */
     rampsize = cr->whitelevel - cr->blacklevel;
     if (rampsize < 1)
	  rampsize = 1;
     slope = 256.0 / (float)rampsize;

     /* Make the ramp. */
     for (i = cr->blacklevel; i < cr->whitelevel; i++){
	  point = (float)(i - cr->blacklevel) * slope;
	  cmap[i] = point;
     }
     
     /* reverse the color ramp (DNM simplified) */
     if (cr->reverse)
	  for(i = 0; i < 256; i++)
	       cmap[i] = 255 - cmap[i];
     
     /* Write to color map */
     if (cr->scale <= 0.0)
	  return(-1);

     /* DNM: for TrueColor, look up the appropriate pixel values and stick in
	the ramp array, conditional on machine type */
     if (cr->visual->class == TrueColor) {
	  switch(cr->falsecolor){
	     case 0:  /* gray scale */

	       for (ival = 0; ival < 256; ival++) {
		    val = cmap[ival];
		    /* DNM: fill the int with a pointer in R - G - B order and 
		       avoid machine-dependent code */
		    ramppt = (unsigned char *)&(cr->ramp[ival]);
		    *ramppt++ = val;
		    *ramppt++ = val;
		    *ramppt++ = val;
		    *ramppt++ = 0;
	       }
	       break;
	  
	     case 1:  /* color ramp */
	       for (ival = 0; ival < 256; ival++) {
		    xcramp_mapfalsecolor( cmap[ival], &red, &green, &blue);
		    ramppt = (unsigned char *)&(cr->ramp[ival]);
		    *ramppt++ = red;
		    *ramppt++ = green;
		    *ramppt++ = blue;
		    *ramppt++ = 0;
	       }
	       break;
	  }
	  return(0);
     }

     /* DNM: take the ramp filling out of here, the loops are not set up to do 
	it correctly */
     switch(cr->falsecolor){
	case 0:  /* gray scale */
	  for (i = 0; i < cr->rampsize; i++){
	       ival = (int)((float)i / cr->scale);
	       val = cmap[ival] << 8;
	       cr->xcolor[i].red   = val;
	       cr->xcolor[i].green = val;
	       cr->xcolor[i].blue  = val;
	  }
	  break;
	  
	case 1:  /* color ramp */
	  for (i = 0; i < cr->rampsize; i++){
	       ival = (int)((float)i / cr->scale);
	       xcramp_mapfalsecolor( cmap[ival], &red, &green, &blue);
	       cr->xcolor[i].red   = red << 8;
	       cr->xcolor[i].green = green << 8;
	       cr->xcolor[i].blue  = blue << 8;
	  }
	  break;

	case 2:  /* color table */
	  for (i = 0; i < cr->rampsize; i++){
	       ival  = (int)((float)i / cr->scale);
	       red   = cr->table[cmap[ival]][0];
	       green = cr->table[cmap[ival]][1];
	       blue  = cr->table[cmap[ival]][2];
	       cr->xcolor[i].red   = red << 8;
	       cr->xcolor[i].green = green << 8;
	       cr->xcolor[i].blue  = blue << 8;
	  }
	  
	  break;
     }
     if ((cr->visual->class == PseudoColor) || 
	 (cr->visual->class == GrayScale)){
	 XStoreColors(cr->display, cr->cmap, cr->xcolor, cr->rampsize);
	 /* XInstallColormap(cr->display, cr->cmap); */
     }
     return(0);
}

static int ImodvRampData[] =
{ 
   15,
  255,    0,  255,  -616,
  179,    0,  255,  -569,
  120,   40,  255,  -528,
   60,   96,  255,  -469,
    0,  175,  177,  -400,
    0,  191,  143,  -383,
    0,  207,   78,  -361,
   90,  255,   60,  -305,
  191,  255,    0,  -259,
  239,  255,    0,  -240,
  255,  255,    0,  -229,
  255,  175,    0,  -162,
  255,  105,    0,   -83,
  255,   45,   55,   -20,
  255,    0,   90,    0,
};

static int load_cmap(unsigned char table[3][256], int *rampData)
{

     FILE *fin;
     char line[256];
     int nline;
     int *inramp;
     int i,l;
     float tabscl,terpfc,tabpos;
     int indtab;
     nline = *rampData;
     rampData++;
     inramp = (int *)malloc(sizeof(int) * nline * 4);

     for(i = 0, l = 0; i < nline; i++, l+=4){
	 inramp[l] = *rampData; rampData++;
	 inramp[l+1] = *rampData; rampData++;
	 inramp[l+2] = *rampData; rampData++;
	 inramp[l+3] = *rampData; rampData++;
      }

     tabscl = (inramp[(nline * 4) - 1] - inramp[3])/255.0;
     indtab = 0;
     for(i = 0; i < 256; i++){
          tabpos = i * tabscl + inramp[3];
          if (tabpos > inramp[((indtab+1) * 4) + 3]){
               indtab++;
               if (indtab > nline - 2)
                    indtab--;
	   }

          terpfc = (tabpos - inramp[(indtab * 4) + 3])/
               (inramp[((indtab+1) * 4) + 3] - inramp[(indtab * 4) + 3]);

          table[0][i] = (1 - terpfc) * inramp[(indtab * 4)] +
               terpfc * inramp [((indtab+1) * 4)];
          table[1][i] = (1 - terpfc) * inramp[(indtab * 4) + 1] +
               terpfc * inramp [((indtab+1) * 4) + 1];
          table[2][i] = (1 - terpfc) * inramp[(indtab * 4) + 2] +
               terpfc * inramp [((indtab+1) * 4) + 2];

      }
     return(0);
 }

void xcramp_mapfalsecolor(int gray, int *red, int *green, int *blue)
{
    static unsigned char cmap[3][256];
    static int first = 1;

    if (first){
	load_cmap(cmap, ImodvRampData);
	first = 0;
    }

    *red = cmap[0][gray];
    *green = cmap[1][gray];
    *blue = cmap[2][gray];
}
