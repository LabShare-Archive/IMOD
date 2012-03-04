/*
 *  xcramp.cpp -- color map control for Qt, rgb and color index  mode
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */


#include <stdlib.h>
#include <stdio.h>
#include <qglcolormap.h>
#include "xcramp.h"
#include "b3dutil.h"

Cramp *xcramp_allinit(int depth, QGLColormap *qCmapPtr, int low, int high, int ushort)
{
  Cramp *cramp;
  int i;
  int maxval = 1;
    
  if (qCmapPtr && ushort)
    return NULL;
  cramp = (Cramp *)malloc(sizeof(Cramp));
  if (!cramp)
    return NULL;
  cramp->rgba = 1;
  cramp->blacklevel = 0;
  cramp->whitelevel = 255;
  cramp->minlevel = 0;
  cramp->maxlevel = 255;
  cramp->mapsize = 256;
  if (ushort) {
    cramp->rgba = 2;
    cramp->whitelevel = 65535;
    cramp->maxlevel = 65535;
    cramp->mapsize = 65536;
  }
  cramp->ramp = B3DMALLOC(unsigned int, cramp->mapsize);
  cramp->cmap = B3DMALLOC(unsigned short, cramp->mapsize);
  if (!cramp->ramp || !cramp->cmap)
    return NULL;

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

  cramp->reverse    = 0;
  cramp->falsecolor = 0;
  cramp->depth      = depth;
  cramp->rampbase   = low;
  cramp->rampsize   = high - low + 1;
  cramp->scale      = (cramp->rampsize - 1)/ 255.0f;
  cramp->qCmapPtr = qCmapPtr;

  cramp->clevel     = 0;
  cramp->noflevels = 0;
  if (xcrampStoreInit(cramp, 4))
    return NULL;

  return(cramp);
}

/* alloc space for storage of extra color index values. */
int xcrampStoreInit(Cramp *cramp, int size)
{
  int i;

  if (size < 1)
    return(-1);
  if (!cramp) 
    return -1;
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
  if (!cramp->blacks || !cramp->whites)
    return -1;
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
  if (!xcramp) return -1;
  xcramp->blacklevel += black;
  if (xcramp->blacklevel < xcramp->minlevel || xcramp->blacklevel > xcramp->maxlevel)
    xcramp->blacklevel -= black;
  xcramp->whitelevel += white;
  if (xcramp->whitelevel < xcramp->minlevel || xcramp->whitelevel > xcramp->maxlevel)
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
  float slope;   /* The slope of the ramp. */
  unsigned char *ramppt;
  QRgb rgbtab[256];

  if (!cr) 
    return -1;

  if (cr->falsecolor < 2) {

    /* Keep input variables in bounds. */
    if (cr->blacklevel < cr->minlevel)
      cr->blacklevel = cr->minlevel;
    if (cr->whitelevel > cr->maxlevel)
      cr->whitelevel = cr->maxlevel;
    if (cr->blacklevel > cr->whitelevel)
      cr->blacklevel = cr->whitelevel;
     
    /* write out saturated values to cmap. */
    for (i = 0; i < cr->blacklevel; i++)
      cr->cmap[i] = 0;
    for (i = cr->whitelevel; i < cr->mapsize; i++)
      cr->cmap[i] = 255;

    /* calculate rampsize and slope */
    rampsize = cr->whitelevel - cr->blacklevel;
    if (rampsize < 1)
      rampsize = 1;
    slope = 256. / (float)rampsize;

    /* Make the ramp. */
    for (i = cr->blacklevel; i < cr->whitelevel; i++)
      cr->cmap[i] = (int)((i - cr->blacklevel) * slope);

    /* reverse the color ramp (DNM simplified) */
    if (cr->reverse)
      for(i = 0; i < cr->mapsize; i++)
        cr->cmap[i] = 255 - cr->cmap[i];
     
    /* Write to color map */
    if (cr->scale <= 0.0)
      return(-1);

  } else {

    /* Simple ramp for colormap mode, ignoring all other parameters */
    for(i = 0; i < 256; i++)
      cr->cmap[i] = i;
  }

  /* Copy to byte ramp for scaling 3 channels of RGB data separately */
  for(i = 0; i < 256; i++)
    cr->bramp[i] = (unsigned char)cr->cmap[i * cr->mapsize / 256];

  /* DNM: for TrueColor, look up the appropriate pixel values and stick in
     the ramp array, conditional on machine type */
  if (cr->rgba) {
    switch(cr->falsecolor){
    case 0:  /* gray scale */

      for (ival = 0; ival < cr->mapsize; ival++) {
        val = cr->cmap[ival];
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
      for (ival = 0; ival < cr->mapsize; ival++) {
        xcramp_mapfalsecolor( cr->cmap[ival], &red, &green, &blue);
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
      val = cr->cmap[ival];
      rgbtab[i] = qRgb(val, val, val);
      cr->qCmapPtr->setEntry(i + cr->rampbase, rgbtab[i]);
      /* fprintf(stderr, "%d %d %d %x\n", i, ival, val, rgbtab[i]); */
    }
    break;
          
  case 1:  /* color ramp */
    for (i = 0; i < cr->rampsize; i++){
      ival = (int)((float)i / cr->scale);
      xcramp_mapfalsecolor( cr->cmap[ival], &red, &green, &blue);
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

