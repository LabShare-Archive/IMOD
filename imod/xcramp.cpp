/*
 *  xcramp.c -- color map control for Qt, rgb and color index  mode
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */
/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 4.2  2004/09/10 02:31:03  mast
    replaced long with int

    Revision 4.1  2003/02/10 20:29:02  mast
    autox.cpp

    Revision 1.1.2.3  2003/01/29 17:52:25  mast
    removed draw command after colormap change, handled in imod_info_cb.cpp

    Revision 1.1.2.2  2003/01/29 01:46:08  mast
    changes for color index mode

    Revision 1.1.2.1  2003/01/26 23:25:26  mast
    Qt version

*/


#include <stdlib.h>
#include <stdio.h>
#include <qglcolormap.h>
#include "xcramp.h"
#include "b3dutil.h"

Cramp *xcramp_allinit(int depth, QGLColormap *qCmapPtr,  int low,  int high)
{
  Cramp *cramp;
  int value, i;
  int red, green, blue;
  int maxval = 1;
    
  cramp = (Cramp *)malloc(sizeof(Cramp));
  cramp->rgba = 1;

  if (qCmapPtr) {
    for (i=0; i < depth;i++)
      maxval*=2;
    if (high > maxval)
      high = maxval;
    if (low > high){
      maxval = low;
      low = high;
      high = maxval;
    }
    cramp->rgba = 0;
  }

  cramp->blacklevel = 0;
  cramp->whitelevel = 255;
  cramp->reverse    = 0;
  cramp->falsecolor = 0;
  cramp->depth      = depth;
  cramp->rampbase   = low;
  cramp->rampsize   = high - low + 1;
  cramp->scale      = (cramp->rampsize - 1)/ 255.0f;
  cramp->qCmapPtr = qCmapPtr;

  cramp->clevel     = 0;
  cramp->noflevels = 0;
  xcrampStoreInit(cramp, 4);

  return(cramp);
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

/* Make the color ramp for current parameters */
int xcramp_ramp(Cramp *cr)
{
  int  i, ival, val;
  int  rampsize;
  int  red, green, blue;
  int  minlevel = 0, maxlevel = 255;
  float slope,   /* The slope of the ramp. */
    point;   /* Temp variable to store colormap index as float. */
  unsigned char *ramppt;
  QRgb rgbtab[256];
  unsigned short cmap[256];

  if (!cr) 
    return -1;

  if (cr->falsecolor < 2) {

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

  } else {

    /* Simple ramp for colormap mode, ignoring all other parameters */
    for(i = 0; i < 256; i++)
      cmap[i] = i;
  }

  /* DNM: for TrueColor, look up the appropriate pixel values and stick in
     the ramp array, conditional on machine type */
  if (cr->rgba) {
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
    case 2:  /* colormap ramp */
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


  switch(cr->falsecolor){
  case 0:  /* gray scale */
    for (i = 0; i < cr->rampsize; i++){
      ival = (int)((float)i / cr->scale);
      val = cmap[ival];
      rgbtab[i] = qRgb(val, val, val);
      cr->qCmapPtr->setEntry(i + cr->rampbase, rgbtab[i]);
      /* fprintf(stderr, "%d %d %d %x\n", i, ival, val, rgbtab[i]); */
    }
    break;
          
  case 1:  /* color ramp */
    for (i = 0; i < cr->rampsize; i++){
      ival = (int)((float)i / cr->scale);
      xcramp_mapfalsecolor( cmap[ival], &red, &green, &blue);
      rgbtab[i] = qRgb(red, green, blue);
      cr->qCmapPtr->setEntry(i + cr->rampbase, rgbtab[i]);
    }
    break;

  }
  /*
  fprintf(stderr, "loading size %d base %d  colormap size %d\n",
	  cr->rampsize, cr->rampbase,  cr->qCmapPtr->size());
  // This seems not to work! 
  cr->qCmapPtr->setEntries(cr->rampsize, rgbtab, cr->rampbase); 
  */
  return(0);
}

// The false color map is now a file-level static so that it can be switched
// with external calls.

static unsigned char cmap[3][256];
static int cmapMade = 0;

void xcramp_mapfalsecolor(int gray, int *red, int *green, int *blue)
{
  int *rampData;

  if (!cmapMade){
    rampData = cmapStandardRamp();
    cmapConvertRamp(rampData, cmap);
    cmapMade = 1;
  }

  *red = cmap[0][gray];
  *green = cmap[1][gray];
  *blue = cmap[2][gray];
}

int xcramp_readfalsemap(char *filename)
{
  int err;
  err = cmapReadConvert(filename, cmap);
  if (!err)
    cmapMade = 1;
  return err;
}

void xcramp_copyfalsemap(unsigned char *inmap)
{
  memcpy(cmap, inmap, 3 * 256);
  cmapMade = 1;
}

void xcramp_restorefalsemap()
{
  int *rampData = cmapStandardRamp();
  cmapConvertRamp(rampData, cmap);
}
