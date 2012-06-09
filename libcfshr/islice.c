/*
 *  islice.c -- Basic image slice functions needed in libcfshr
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "mrcslice.h"
#include "b3dutil.h"

/*!
 * Creates an @@Islice structure@ of the given [mode] and allocates a data
 * array appropriate for the size [xsize], [ysize].  Returns a pointer to the
 * slice or the NULL for error
 */
Islice *sliceCreate(int xsize, int ysize, int mode)
{
  Islice *s;
  size_t xysize = (size_t)xsize * (size_t)ysize;

  if (xysize / xsize != ysize) {
    b3dError(stderr, "ERROR: sliceCreate - slice is too large for a "
             "32-bit computer.\n");
    return(NULL);
  }

  s = (Islice *)malloc(sizeof(Islice));
  if (!s)
    return(NULL);
     
  s->xsize = xsize;
  s->ysize = ysize;
  s->mode  = mode;
  s->index = -1;

  switch(mode){
      
  case MRC_MODE_BYTE:
    s->dsize = sizeof(unsigned char);
    s->csize = 1;
    s->data.b = (unsigned char *)malloc(xysize * s->dsize * s->csize);
    break;

  case MRC_MODE_SHORT:
  case MRC_MODE_USHORT:
    s->dsize = sizeof(b3dInt16);
    s->csize = 1;
    s->data.s = (b3dInt16 *)malloc(xysize * s->dsize * s->csize);
    break;
      
  case MRC_MODE_FLOAT:
    s->dsize = sizeof(float);
    s->csize = 1;
    s->data.f = (float *)malloc(xysize * s->dsize * s->csize);
    break;

  case MRC_MODE_COMPLEX_SHORT:
    s->dsize = sizeof(b3dInt16);
    s->csize = 2;
    s->data.s = (b3dInt16 *)malloc(xysize * s->dsize * s->csize);
    break;

  case MRC_MODE_COMPLEX_FLOAT:
    s->dsize = sizeof(float);
    s->csize = 2;
    s->data.f = (float *)malloc(xysize * s->dsize * s->csize);
    break;

  case MRC_MODE_RGB:
    s->dsize = sizeof(unsigned char);
    s->csize = 3;
    s->data.b = (unsigned char *)malloc(xysize * s->dsize * s->csize);
    break;

  case SLICE_MODE_MAX:
    s->csize = SLICE_MAX_CSIZE;
    s->dsize = SLICE_MAX_DSIZE;
    s->data.f = (float *)malloc(xysize * s->dsize * s->csize);
    break;

  default:
    free(s);
    return(NULL);
  }

  if (!s->data.b){
    b3dError(stderr, "ERROR: sliceCreate - not enough memory.\n");
    free(s);
    return(NULL);
  }

  return(s);
}

/*!
 * Initialize elements in the @@Islice structure@ [s] with the given size
 * [xsize], [ysize] and mode [mode], and sets the data member to [data]. 
 * Returns -1 for an undefined mode.
 */
int sliceInit(Islice *s, int xsize, int ysize, int mode, void *data)
{
  s->xsize = xsize;
  s->ysize = ysize;
  s->mode  = mode;

  switch(mode){
  case MRC_MODE_BYTE:
    s->csize = 1;
    s->dsize = 1;
    s->data.b = data;
    break;
  case MRC_MODE_SHORT:
  case MRC_MODE_USHORT:
    s->dsize = sizeof(b3dInt16);
    s->csize = 1;
    s->data.us = data;
    break;
  case MRC_MODE_FLOAT:
    s->dsize = sizeof(b3dFloat);
    s->csize = 1;
    s->data.f = data;
    break;
  case MRC_MODE_COMPLEX_SHORT:
    s->dsize = sizeof(b3dInt16);
    s->csize = 2;
    s->data.s = data;
    break;
  case MRC_MODE_COMPLEX_FLOAT:
    s->dsize = sizeof(b3dFloat);
    s->csize = 2;
    s->data.f = data;
    break;
  case MRC_MODE_RGB:
    s->csize = 1;
    s->dsize = 3;
    s->data.b = data;
    break;
  case SLICE_MODE_MAX:
    s->csize = SLICE_MAX_CSIZE;
    s->dsize = SLICE_MAX_DSIZE;
    s->data.f = data;
    break;
  default:
    return(-1);
  }
  return(0);
}

/*!
 * Frees the @@Islice structure@ slice and its data array if any.
 */
void sliceFree(Islice *s)
{
  if (!s)
    return;
  if (s->data.b)
    free(s->data.b);
  free(s);
}

/*! 
 * Sets the entire slice [s] to the value given by the @@Ival type@ [val]
 */
void sliceClear(Islice *s, Ival val)
{
  unsigned int i, j;
     
  s->min = s->max = s->mean = sliceGetValMagnitude(val, s->mode);
  for (j = 0; j < s->ysize; j++)
    for(i = 0; i < s->xsize; i++)
      slicePutVal(s, i, j, val);
}

/*!
 * Returns a slice mode corresponding to the string in [mst], which can
 * contain either an MRC mode number or {byte}, {short}, {float}, {complex},
 * {ushort}, or {rgb}.  Returns SLICE_MODE_SBYTE (-2) for {sbyte} or SLICE_MODE_UBYTE 
 * (-3) for {ubyte}.
 */ 
int sliceMode(char *mst)
{
  if (strcmp(mst, "byte") == 0)
    return(SLICE_MODE_BYTE);
  if (strcmp(mst, "sbyte") == 0)
    return(SLICE_MODE_SBYTE);
  if (strcmp(mst, "ubyte") == 0)
    return(SLICE_MODE_UBYTE);
  if (strcmp(mst, "0") == 0)
    return(SLICE_MODE_BYTE);
  if (strcmp(mst, "short") == 0)
    return(SLICE_MODE_SHORT);
  if (strcmp(mst, "1") == 0)
    return(SLICE_MODE_SHORT);
  if (strcmp(mst, "float") == 0)
    return(SLICE_MODE_FLOAT);
  if (strcmp(mst, "2") == 0)
    return(SLICE_MODE_FLOAT);
  if (strcmp(mst, "3") == 0)
    return(SLICE_MODE_COMPLEX_SHORT);
  if (strcmp(mst, "complex") == 0)
    return(SLICE_MODE_COMPLEX_FLOAT);
  if (strcmp(mst, "4") == 0)
    return(SLICE_MODE_COMPLEX_FLOAT);
  if (strcmp(mst, "ushort") == 0)
    return(SLICE_MODE_USHORT);
  if (strcmp(mst, "6") == 0)
    return(SLICE_MODE_USHORT);
  if (strcmp(mst, "16") == 0)
    return(SLICE_MODE_RGB);
  if (strcmp(mst, "rgb") == 0)
    return(SLICE_MODE_RGB);
  return(SLICE_MODE_UNDEFINED);
}

/*!
 * For the MRC_MODE_... type in [mrcMode], returns the corresponding defined
 * SLICE_MODE_... value if the mode is byte, signed or unsigned short integer, 
 * or float, otherwise returns -1.
 */
int sliceModeIfReal(int mrcMode)
{
  if (mrcMode == MRC_MODE_BYTE)
    return SLICE_MODE_BYTE;
  else if (mrcMode == MRC_MODE_SHORT)
    return SLICE_MODE_SHORT;
  else if (mrcMode == MRC_MODE_USHORT)
    return SLICE_MODE_USHORT;
  else if (mrcMode == MRC_MODE_FLOAT)
    return SLICE_MODE_FLOAT;
  return -1;
}

/*! Returns the X size of [slice] */
int sliceGetXSize(Islice *slice)
{
  return(slice->xsize);
}

/*! Returns the Y size of [slice] */
int sliceGetYSize(Islice *slice)
{
  return(slice->ysize);
}

/*!
 * Gets the value of pixel at [x], [y] in slice [s] and puts it into the
 * value array [val].  Returns -1 and puts the slice mean into the first
 * element of [val] for a point out of bounds, and returns -1 if the 
 * slice mode is undefined.
 */
int sliceGetVal(Islice *s, int x, int y, Ival val)
{
  size_t index = x + ((size_t)y * (size_t)s->xsize);
     
  if ( (x < 0) || (y < 0) || (x >= s->xsize) || (y >= s->ysize)){
    val[0] = s->mean;
    return(-1);
  }

  val[1] = 0;
  switch (s->mode)
    {
    case MRC_MODE_BYTE:
      val[0] = (float)s->data.b[index];
      break;
    case MRC_MODE_SHORT:
      val[0] = (float)s->data.s[index];
      break;
    case MRC_MODE_USHORT:
      val[0] = (float)s->data.us[index];
      break;
    case MRC_MODE_FLOAT:
      val[0] = s->data.f[index];
      break;
    case MRC_MODE_COMPLEX_SHORT:
      index *= 2;
      val[0] = (float)s->data.s[index];
      val[1] = (float)s->data.s[index + 1];
      break;
    case MRC_MODE_COMPLEX_FLOAT:
      index *= 2;
      val[0] = s->data.f[index];
      val[1] = s->data.f[index + 1];
      break;
    case MRC_MODE_RGB:
      index *= 3;
      val[0] = (float)s->data.b[index++];
      val[1] = (float)s->data.b[index++];;
      val[2] = (float)s->data.b[index];
      break;
    case SLICE_MODE_MAX:
      index *= 3;
      val[0] = s->data.f[index++];
      val[1] = s->data.f[index++];
      val[2] = s->data.f[index];
      break;
    default:
      b3dError(stderr, "sliceGetVal: unknown mode.\n");
      return(-1);
    }
  return(0);
}

/*!
 * Puts the value(s) in [val] into the pixel at [x], [y] in slice [s].
 * Returns -1 if the point is out of bounds or the slice mode is undefined.
 */
int slicePutVal(Islice *s, int x, int y, Ival val)
{
  size_t index = x + ((size_t)y * (size_t)s->xsize);

  if ( (x < 0) || (y < 0) || (x >= s->xsize) || (y >= s->ysize))
    return(-1);

  switch (s->mode)
    {
    case MRC_MODE_BYTE:
      s->data.b[index] = (unsigned char)val[0];
      break;
    case MRC_MODE_SHORT:
      s->data.s[index] = (b3dInt16)val[0];
      break;
    case MRC_MODE_USHORT:
      s->data.us[index] = (b3dUInt16)val[0];
      break;
    case MRC_MODE_FLOAT:
      s->data.f[index] = val[0];
      break;
    case MRC_MODE_COMPLEX_SHORT:
      index *= 2;
      s->data.s[index]     = (b3dInt16)val[0];
      s->data.s[index + 1] = (b3dInt16)val[1];
      break;
    case MRC_MODE_COMPLEX_FLOAT:
      index *= 2;
      s->data.f[index]     = val[0];
      s->data.f[index + 1] = val[1];
      break;
    case MRC_MODE_RGB:
      index *= 3;
      s->data.b[index++] = val[0];
      s->data.b[index++] = val[1];
      s->data.b[index]   = val[2];
      break;
    case SLICE_MODE_MAX:
      index *= 3;
      s->data.f[index++] = val[0];
      s->data.f[index++] = val[1];
      s->data.f[index]   = val[2];
      break;
    default:
      b3dError(stderr, "slicePutVal: unknown mode.\n");
      return(-1);
    }
  return(0);
}

/*!
 * Returns the magnitude of the pixel at [x], [y] in slice [s]: simply the
 * value for one-channel data, the amplitude of complex data, or a weighted
 * value for RGB data.
 */
float sliceGetPixelMagnitude(Islice *s, int x, int y)
{
  Ival val;
  float m = 0.0f;
     
  sliceGetVal(s, x, y, val);

  if (s->csize == 1)
    return(val[0]);

  if (s->csize == 2){
    m = (val[0] * val[0]) + (val[1] * val[1]);
    return((float)sqrt(m));
  }

  m = val[0] * 0.3f;
  m += val[1] * 0.59f;
  m += val[2] * 0.11f;
  return(m);
}

/*!
 * Returns the magnitude of the value array [val] extracted from a slice of
 * mode [mode]: simply the value for one-channel data, the amplitude of complex
 * data, or a weighted value for RGB data.
 */
float sliceGetValMagnitude(Ival val, int mode)
{
  float m = val[0];
     
  if ((mode == SLICE_MODE_COMPLEX_FLOAT) || 
      (mode == SLICE_MODE_COMPLEX_SHORT)){
    m = (val[0] * val[0]) + (val[1] * val[1]);
    return((float)sqrt(m));
  }

  if (mode == SLICE_MODE_RGB){
    m = val[0] * 0.3f;
    m += val[1] * 0.59f;
    m += val[2] * 0.11f;
  }

  return(m);
}
