/*
 *  mrcsec.c - routines for reading defined parts of sections as byte or raw
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>
#include "b3dutil.h"
#include "iimage.h"

#define MRSA_BYTE 1
#define MRSA_FLOAT 2
#define MRSA_USHORT 3

static int mrcReadSectionAny(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int cz,
                             int readY, int type);
static int mrcWriteSectionAny(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int cz,
                              int mode);
static ImodImageFile *lookupIIfile(MrcHeader *hdata, IloadInfo *li, int axis, 
                                   ImodImageFile *iiSave);
static int callIIorMRSA(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z,
                        int readY, int type, iiSectionFunc func);

/*
 * Routines for accessing particular kinds of data as bytes or raw, Y or Z
 * axis.  They should be able to handle planes > 4 GB on a 64-bit system except
 * for complex data.
 */

/*!
 * Reads one Z slice of raw data at Z value [z] into the buffer [buf] from the 
 * MRC file whose header is in [hdata].  The @@IloadInfo structure@ [li] 
 * controls the subarea loaded through its members {xmin}, {xmax}, {ymin},
 * and {ymax}  If it is not already
 * being called by one of the iiRead functions in @@iimage.html#TOP@, it looks
 * up whether the {fp} member of [hdata] is on the list of opened ImodImageFiles and if 
 * so, redirects the call through an iiRead function.
 * Returns 1 for an illegal request, 2 for a memory error, 3 for an error 
 * reading the file, or if the call is redirected, it can return -1 if no function is 
 * defined for the operation or error codes from I/O functions for other file types.
 */
int mrcReadZ(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  return (callIIorMRSA(hdata, li, buf, z, 0, 0, iiReadSection));
}

/*!
 * Reads one Z slice of data, like @mrcReadZ, and scales it to bytes.
 * Scaling is controlled by [li] members {slope}, {offset}, and {ramp}, but
 * {ramp} should be MRC_RAMP_LIN unless reading in integers or floats.
 */
int mrcReadZByte(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  return (callIIorMRSA(hdata, li, buf, z, 0, MRSA_BYTE, iiReadSectionByte));
}

/*!
 * Reads one Z slice of data, like @mrcReadZ, and scales it to unsigned short integers.
 * Scaling is controlled by [li] members {slope}, {offset}, and {ramp}, but
 * {ramp} should be MRC_RAMP_LIN unless reading in integers or floats.
 */
int mrcReadZUShort(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  return (callIIorMRSA(hdata, li, buf, z, 0, MRSA_USHORT, iiReadSectionUShort));
}

/*!
 * Reads one Z slice of data, like @mrcReadZ, and returns it as floats.
 * Only works for real modes: byte, signed and unsigned short, float.
 */
int mrcReadZFloat(MrcHeader *hdata, IloadInfo *li, b3dFloat *buf, int z)
{
  return (callIIorMRSA(hdata, li, (unsigned char *)buf, z, 0,MRSA_FLOAT, 
                       iiReadSectionFloat));
}

/*!
 * Reads one Y slice of raw data at Y value [z] into the buffer [buf] from the 
 * MRC file whose header is in [hdata].  The IloadInfo structure [li] controls
 * the subarea loaded through its members {xmin}, {xmax}, {zmin}, and {zmax}.
 * Returns 1 for an illegal request, 2 for a memory error, or 3 for an error 
 * reading the file.
 */
int mrcReadY(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  return (callIIorMRSA(hdata, li, buf, z, 1, 0, iiReadSection));
}

/*!
 * Reads one Y slice of data, like @mrcReadY, and scales it to bytes.
 * Scaling is controlled by [li] members {slope}, {offset}, and {ramp}, but
 * {ramp} should be MRC_RAMP_LIN unless reading in integers or floats.
 */
int mrcReadYByte(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  return (callIIorMRSA(hdata, li, buf, z, 1, MRSA_BYTE, iiReadSectionByte));
}

/*!
 * Reads one Y slice of data, like @mrcReadY, and scales it to unsigned short integers.
 * Scaling is controlled by [li] members {slope}, {offset}, and {ramp}, but
 * {ramp} should be MRC_RAMP_LIN unless reading in integers or floats.
 */
int mrcReadYUShort(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  return (callIIorMRSA(hdata, li, buf, z, 1, MRSA_USHORT, iiReadSectionUShort));
}

/*!
 * Reads one Y slice of data, like @mrcReadY, and returns it as floats.
 * Only works for real modes: byte, signed and unsigned short, and float.
 */
int mrcReadYFloat(MrcHeader *hdata, IloadInfo *li, b3dFloat *buf, int z)
{
  return (callIIorMRSA(hdata, li, (unsigned char *)buf, z, 1,MRSA_FLOAT, 
                       iiReadSectionFloat));
}

/*!
 * Reads one slice of raw data into the buffer [buf] from the 
 * MRC file whose header is in [hdata].  A Y slice at Y = [z] is loaded if the
 * {axis} member of [li] is 2; otherwise a Z slice at Z = [z] is loaded.
 * The subarea loaded is determined by [li] members {xmin}, {xmax}, and {ymin}
 * and {ymax} (for a Z slice) or {zmin} and {zmax} (for a Y slice).  If it is not already
 * being called by one of the iiRead functions in @@iimage.html#TOP@, it looks
 * up whether the {fp} member of [hdata] is on the list of opened ImodImageFiles and if 
 * so, redirects the call through an iiRead function.
 * Returns 1 for an illegal request, 2 for a memory error, 3 for an error 
 * reading the file, or if the call is redirected, it can return -1 if no function is 
 * defined for the operation or error codes from I/O functions for other file types.
 */
int mrcReadSection(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  int readY = (li->axis == 2) ? 1 : 0;
  return (callIIorMRSA(hdata, li, buf, z, readY, 0, iiReadSection));
}

/*!
 * Reads one slice of data, like @mrcReadSection, and scales it to bytes.
 * Scaling is controlled by [li] members {slope}, {offset}, and {ramp}, but
 * {ramp} should be MRC_RAMP_LIN unless reading in integers or floats.
 */
int mrcReadSectionByte(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  int readY = (li->axis == 2) ? 1 : 0;
  return (callIIorMRSA(hdata, li, buf, z, readY, MRSA_BYTE, iiReadSectionByte));
}

/*!
 * Reads one slice of data, like @mrcReadSection, and scales it to bytes.
 * Scaling is controlled by [li] members {slope}, {offset}, and {ramp}, but
 * {ramp} should be MRC_RAMP_LIN unless reading in integers or floats.
 */
int mrcReadSectionUShort(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  int readY = (li->axis == 2) ? 1 : 0;
  return (callIIorMRSA(hdata, li, buf, z, readY, MRSA_USHORT, iiReadSectionUShort));
}

/*!
 * Reads one slice of data, like @mrcReadSection, and returns it as floats.
 * Only works for real modes: byte, signed and unsigned short, and float.
 */
int mrcReadSectionFloat(MrcHeader *hdata, IloadInfo *li, b3dFloat *buf, int z)
{
  int readY = (li->axis == 2) ? 1 : 0;
  return (callIIorMRSA(hdata, li, (unsigned char *)buf, z, readY, MRSA_FLOAT,
                       iiReadSectionFloat));
}

/*
 * Looks up an iiFile for the fp and redirects to the indicated function, otherwise
 * proceeds to call the read function
 */
static int callIIorMRSA(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z,
                        int readY, int type, iiSectionFunc func)
{
  ImodImageFile *iiFile, iiSave;
  if ((iiFile = lookupIIfile(hdata, li, readY ? 2 : 3, &iiSave)) != NULL)
    return iiRestoreLoadParams(func(iiFile, (char *)buf, z), iiFile, &iiSave);
  return (mrcReadSectionAny(hdata, li, buf, z, readY, type));
}

/*
 * Generic routine for reading the data line-by-line and scaling it properly
 */
static int mrcReadSectionAny(MrcHeader *hdata, IloadInfo *li,
                              unsigned char *buf, int cz, int readY, int type)
{
  FILE *fin = hdata->fp;
  int  nx = hdata->nx;
  int  ny = hdata->ny;
     
  int  xStart = li->xmin;
  int  xEnd = li->xmax;
  int  yStart = readY ? li->zmin : li->ymin;
  int  yEnd = readY ? li->zmax : li->ymax;
  int xsize = xEnd - xStart + 1;
  int byte = type == MRSA_BYTE ? 1 : 0;
  int toShort = type == MRSA_USHORT ? 1 : 0;
  int mapSbytes = (!hdata->mode && hdata->bytesSigned) ? 1 : 0;
  int convert = byte + toShort;
  int padLeft = B3DMAX(0, li->padLeft);
  int padRight = B3DMAX(0, li->padRight);
  int pixSizeBuf[4] = {0, 1, 4, 2};
     
  float slope  = li->slope;
  float offset = li->offset;
  int   outmin = li->outmin;
  int   outmax = li->outmax;
  int seekLine, seekEndX, seekEndY, seekSkip;
  float kscale = mrcGetComplexScale();
     
  unsigned int pixIndex = 0;
  int deltaYsign = 1;
  int pixel, i, j;
  int pixSize = 1;
  int needData = 0;
  int chunkLines = 1;
  float targetMemory = 65536.;
  float multiLineCrit = 0.5;
  b3dFloat fpixel;
  b3dFloat *fdata;

  /* Buffer to read lines into; may be replaced by temporary buffer for reading */
  unsigned char *bdata = buf;

  /* Buffer to place complex data into; may be temporary for mirroring */
  unsigned char *fft = buf;
  b3dUInt16 *usfft = (b3dUInt16 *)buf;
  unsigned char *map = NULL;
  b3dUInt16 *usmap;

  /* Copies of buffer pointer that can be advanced after each line */
  unsigned char *bufp = buf;
  b3dFloat *fbufp = (b3dFloat *)buf;
  b3dUInt16 *usbufp = (b3dUInt16 *)buf;
  int freeMap = 0;
  unsigned char *inptr;
  unsigned char *tmpData;
  b3dInt16 *sdata;
  b3dUInt16 *usdata;
  int imXsize, imYmin, imYmax, ymin, ymax, llfx, llfy, ulfx, ulfy, urfx, urfy;
  int lrfx, lrfy, x0, x1, x2, x3, cury, toggleY, imNx;
  int xStart2, llfx2, llfy2, ulfx2, ulfy2, ybase;
  int needRead, lineEnd, xDimension;
  size_t dataSize;
  float eps = toShort ? 0.005 / 256. : 0.005;
  int doScale = (offset <= -1.0 || offset >= 1.0 || 
                 slope < 1. - eps || slope > 1. + eps) ? 1 : 0;

  /* Raw bytes need to be treated like a conversion if they need signed->unsigned map */
  if (!type && mapSbytes)
    convert = 1;

  /* printf ("read slope %f  offset %f  doScale %d toshort %d\n", slope, offset, doScale, 
     toShort); */
  if (type == MRSA_FLOAT && sliceModeIfReal(hdata->mode) < 0) {
    b3dError(stderr, "ERROR: mrcReadSectionAny - Only real modes can be read"
             " as floats\n");
    return 1;
  }
  if ((type == MRSA_BYTE && outmax > 255) || (type == MRSA_USHORT && outmax < 256)) {
    b3dError(stderr, "ERROR: mrcReadSectionAny - outmax (%d) is not in right range for"
             " conversion to %s\n", outmax, byte ? "bytes" : "shorts");
    return 1;
  }

  /* Adjust loading parameters if mirroring an FFT 
     This is hopelessly complex because it replicates the mirrored FFT produced
     by clip, with the extra right column placed on the left and the bottom
     left row a duplicate of the row above (except for first pixel) */
  if (li->mirrorFFT && convert) {
    imXsize = xsize;
    imNx = 2 * (nx - 1);

    /* Get source for four corners, including a point in from the left edge
       if necessary, and adjust xmin if all to one side of Y axis */
    imYmin = readY ? cz : li->ymin;
    imYmax = readY ? cz : li->ymax;
    xStart2 = xStart < xEnd ? xStart + 1 : xStart;
    mrcMirrorSource(imNx, ny, xStart, imYmin, &llfx, &llfy);
    mrcMirrorSource(imNx, ny, xStart, imYmax, &ulfx, &ulfy);
    mrcMirrorSource(imNx, ny, xStart2, imYmin, &llfx2, &llfy2);
    mrcMirrorSource(imNx, ny, xStart2, imYmax, &ulfx2, &ulfy2);
    mrcMirrorSource(imNx, ny, xEnd, imYmin, &lrfx, &lrfy);
    mrcMirrorSource(imNx, ny, xEnd, imYmax, &urfx, &urfy);
    xStart = 0;
    if (xEnd < imNx / 2)
      xStart = lrfx;
    if (xStart > imNx / 2)
      xStart = llfx;

    /* Get xmax, ymin, ymax from these corners; get y limits and xsize */
    xEnd = b3dIMax(3, urfx, llfx, llfx2);
    ymin = b3dIMin(6, llfy, ulfy, lrfy, urfy, llfy2, ulfy2);
    ymax = b3dIMax(6, llfy, ulfy, lrfy, urfy, llfy2, ulfy2);
    yStart = readY ? li->zmin : ymin;
    yEnd = readY ? li->zmax : ymax;
    xsize = xEnd - xStart + 1;

    /* Set up flip-flop between two lines if needed when reading in Y */
    toggleY = -1;
    if (readY && ymin < ymax) {
      toggleY = 0;
      cz = ymin;
    }
    /* fprintf(stderr, "xStart %d xEnd %d yStart %d yEnd %d xsize %d ymin %d ymax %d "
            "imYmin %d imYmax "
            "%d\n", xStart, xEnd, yStart, yEnd, xsize, ymin, ymax, imYmin, imYmax);
            fflush(stderr); */
  }

  /* get pixel size based on mode, and prepare scaling and set flags if
     need another data array */
  switch(hdata->mode){
  case MRC_MODE_BYTE:
    pixSize = 1;

    /* Get a scaling map if going to bytes with scaling or if going to shorts, or get a
       signed->unsigned map if signed bytes go to raw or float or unscaled bytes */
    if ((byte && doScale) || toShort)
      map = get_byte_map(slope, offset, outmin, outmax, hdata->bytesSigned);
    else if (mapSbytes)
      map = get_byte_map(1.0, 0., 0, 255, 1);
    needData = (type == MRSA_FLOAT || type == MRSA_USHORT) ? 1 : 0;
    break;

  case MRC_MODE_SHORT:
  case MRC_MODE_USHORT:
    pixSize = 2;
    if ((toShort && (doScale || hdata->mode == MRC_MODE_SHORT)) || byte) {
      map= get_short_map(slope, offset, outmin, outmax, li->ramp, hdata->swapped, 
                         (hdata->mode == MRC_MODE_SHORT) ? 1 : 0);
      /*usmap = (b3dUInt16 *)map;
      for (i = 0; i < 65000; i += 1000)
      printf("%d %d %d\n", i, usmap[i], map[i]); */
      freeMap = 1;
      if (!map)
        return 2;
    }
    needData = (type == MRSA_FLOAT || type == MRSA_BYTE) ? 1 : 0;
    break;

  case MRC_MODE_RGB:
    pixSize = 3;
    needData = convert;
    break;

  case MRC_MODE_FLOAT:
    pixSize = 4;
    needData = convert;
    break;

  case MRC_MODE_COMPLEX_SHORT:
    pixSize = 4;
    if (convert)
      return 1;
    break;

  case MRC_MODE_COMPLEX_FLOAT:
    pixSize = 8;
    needData = convert;
    break;

  default:
    if (convert)
      b3dError(stderr, "ERROR: mrcReadSectionAny - unsupported data type.\n");
      return 1;
    break;
  }
  usmap = (b3dUInt16 *)map;
  dataSize = xsize;
  xDimension = xsize + padLeft + padRight;
  pixSizeBuf[0] = pixSize;

  /* Need to buffer if there is padding in X */
  if (xDimension > xsize)
    needData = 1;

  /* If Y is inverted, adjust the Y load, adjust output pointers to last line and set sign
   for incrementing pointers at end of line, in order to fill output array backwards */
  if (hdata->yInverted) {
    if (li->mirrorFFT) {
      b3dError(stderr, "ERROR: mrcReadSectionAny - cannot mirror FFT with inverted Y "
               "data.\n");
      return 1;
    }
    j = ny - 1 - yStart;
    yStart = ny - 1 - yEnd;
    yEnd = j;
    j = (yEnd - yStart) * xsize;
    bdata += pixSize * j;
    pixIndex += j;
    bufp += pixSizeBuf[type] * j;
    usbufp += j;
    fbufp += j;
    deltaYsign = -1;
  }

  /* Advance pointers by left padding before starting */
  bufp += pixSizeBuf[type] * padLeft;
  usbufp += padLeft;
  fbufp += padLeft;
  pixIndex += padLeft;

  /* Determine if doing multi-line reads: not if reading in Y, inverted Y, or mirroring
     FFT, and only if amount needed is a large enough fraction of the full line.  Only
     do it if it will involve more than one line of buffer, or if there is no buffer */
  if (!readY && !(convert && li->mirrorFFT) && !hdata->yInverted && 
      (float)xsize / nx > multiLineCrit && 
      (B3DNINT(targetMemory / nx) > 1 || (!needData && nx == xsize))) {

    /* Need buffer if it is not full-sized in X; limit the chunk lines by memory usage
       for a buffer but do the whole read otherwise */
    if (nx > xsize)
      needData = 1;
    chunkLines = yEnd + 1 - yStart;
    if (needData) {
      chunkLines = B3DMAX(1, B3DMIN(chunkLines, B3DNINT(targetMemory / nx)));
      dataSize = nx * chunkLines;
    }
  }
  /* printf("ps %d cl %d nd %d ds %d nx %d xsize %d pl %d pr %d\n", pixSize, chunkLines,
     needData, (int)dataSize, nx, xsize, padLeft, padRight);*/

  /* Get the supplemental data array, set all pointers to it */
  if (needData) {
    tmpData = (unsigned char *)malloc(pixSize * dataSize);
    if (!tmpData) {
      if (freeMap)
        free(map);
      b3dError(stderr, "ERROR: mrcReadSectionAny - getting memory for "
               "temporary array.\n");
      return 2;
    }
  }

  /* 12/1/04: no longer fix header size in case it is weird */

  /* Seek distance at start of line (in bytes) and end of line (in pixels) */
  seekLine    = xStart * pixSize;
  seekEndX = B3DMAX(0, nx - xEnd - 1);
  seekSkip = 0;
     
  if (readY) {

    /* If reading Y, seek at end of line gets to next section, first seek
       is small and seek to starting Z is big */
    /* b3dFseek(fin, hdata->headerSize + (cz * nx * pixSize),  SEEK_SET);
    if (yStart)
    mrc_big_seek(fin, 0, yStart, nx * ny * pixSize, SEEK_CUR); */
    seekEndY = ny - 1;
    mrcHugeSeek(fin, hdata->headerSize + hdata->sectionSkip * yStart, 0, cz, yStart,
                nx, ny, pixSize, SEEK_SET);
    seekSkip = hdata->sectionSkip;
  } else {

    /* If reading X, seek at end of line gets to end of line, first seek
       is in Z and big, seek to starting Y is small */
    /* mrc_big_seek(fin, hdata->headerSize, cz, nx * ny * pixSize,  SEEK_SET);
    if (yStart)
    b3dFseek(fin, yStart * nx * pixSize, SEEK_CUR); */
    seekEndY = 0;
    mrcHugeSeek(fin, hdata->headerSize + hdata->sectionSkip * cz, 0, yStart, cz,
                nx, ny, pixSize, SEEK_SET);
  }

  /* Loop on chunks in Y */
  /* printf("chunkLines %d  needData %d mode %d byte %d toShort %d vonvert %d doScale %d"
     " xseek %d\n", chunkLines, needData, hdata->mode, byte, toShort, convert, doScale,
     seekEndX); */
  j = yStart;
  while (j <= yEnd) {

    /* Set up the ending line and amount to read */
    lineEnd = B3DMIN(yEnd, j + chunkLines - 1);
    dataSize = xsize;
    needRead = 1;
    if (chunkLines > 1)
      dataSize = (lineEnd + 1 - j) * (size_t)nx;

    /* Reset the bdata pointer to temp data for each line/chunk, or to the buffer pointer
       if reading directly into buffer.  bufp needs to be maintained for all modes when
       a pointer for another mode is advanced */
    if (needData)
      bdata = tmpData;
    else
      bdata = bufp;
    
    /* Start loop on Y and read a line or chunk of data */
    for (; j <= lineEnd; j++) {
      if (seekLine && chunkLines == 1)
        b3dFseek(fin, seekLine, SEEK_CUR);
      if (needRead && b3dFread(bdata, pixSize, dataSize, fin) != dataSize) {
        b3dError(stderr, "ERROR: mrcReadSectionAny - reading data from file.\n");
        if (needData)
          free(tmpData);
        if (freeMap)
          free(map);
        return 3;
      }
      needRead = 0;

      /* Adjust to start of line for chunk reading, then assign pointers for all modes */
      if (chunkLines > 1)
        bdata += seekLine;
      sdata = (b3dInt16 *)bdata;
      usdata = (b3dUInt16 *)bdata;
      fdata = (b3dFloat *)bdata;

      /* Do data-dependent processing for byte conversions or mappings */
      if (convert) {
        switch (hdata->mode) {
        case MRC_MODE_BYTE:    

          /* Byte to byte: mapping, copying, or data already present */
          if (byte || (!toShort && mapSbytes)) {
            if (doScale || mapSbytes)
              for (i = 0; i < xsize; i++)
                bufp[i] = map[bdata[i]];
            else if (needData)
              memcpy(bufp, bdata, xsize);
            bufp += xDimension * deltaYsign;
          } else {             

            /* Byte to ushort */
            for (i = 0; i < xsize; i++)
              usbufp[i] = usmap[bdata[i]];
            usbufp += xDimension * deltaYsign;
            bufp = (unsigned char *)usbufp;
          }
          break;
        
        case MRC_MODE_SHORT:
        case MRC_MODE_USHORT:
          
          /* Short to byte */
          if (byte) {
            for (i = 0; i < xsize; i++)
              bufp[i] = map[usdata[i]];
            bufp += xDimension * deltaYsign;
          } else {

            /* Short to ushort: mapping, copying, or data already present */
            if (doScale || hdata->mode == MRC_MODE_SHORT)
              for (i = 0; i < xsize; i++)
                usbufp[i] = usmap[usdata[i]];
            else if (needData)
              memcpy(usbufp, usdata, xsize * 2);
            usbufp += xDimension * deltaYsign;
            bufp = (unsigned char *)usbufp;
          }
          break;

        case MRC_MODE_RGB:
          inptr = bdata;
          if (byte) {

            /* RGB to byte */
            for (i = 0; i < xsize; i++) {
              fpixel = 0.3 * *inptr++;
              fpixel += 0.59 * *inptr++;
              fpixel += 0.11 * *inptr++;
              bufp[i] = (int)(fpixel + 0.5f);
            }
            bufp += xDimension * deltaYsign;
          } else {
            
            /* RGB to ushort */
            for (i = 0; i < xsize; i++) {
              fpixel = 255. * 0.3 * *inptr++;
              fpixel += 255. * 0.59 * *inptr++;
              fpixel += 255. * 0.11 * *inptr++;
              usbufp[i] = (int)(fpixel + 0.5f);
            }
            usbufp += xDimension * deltaYsign;
            bufp = (unsigned char *)usbufp;
          }
          break;
        
        case MRC_MODE_FLOAT:
          if (hdata->swapped)
            mrc_swap_floats(fdata, xsize);
          if (byte) {

            /* Float to byte */
            /* Do unused ramps separately to speed up the regular load */
            if (li->ramp == MRC_RAMP_LOG || li->ramp == MRC_RAMP_EXP) {
              for (i = 0; i < xsize; i++){
                if (li->ramp == MRC_RAMP_LOG)
                  fpixel = (float)log((double)fdata[i]) * slope + offset;
                else
                  fpixel = (float)exp((double)fdata[i]) * slope + offset;
                fpixel = B3DMAX(outmin, fpixel);
                bufp[i] = B3DMIN(outmax, fpixel) + 0.5f;
              }
            } else {
              for (i = 0; i < xsize; i++) {
                fpixel = fdata[i] * slope + offset;
                fpixel = B3DMAX(outmin, fpixel);
                bufp[i] = B3DMIN(outmax, fpixel) + 0.5f;
              }
            }
            bufp += xDimension * deltaYsign;
          } else {

            /* Float to ushort */
            if (li->ramp == MRC_RAMP_LOG || li->ramp == MRC_RAMP_EXP) {
              for (i = 0; i < xsize; i++){
                if (li->ramp == MRC_RAMP_LOG)
                  fpixel = (float)log((double)fdata[i]) * slope + offset;
                else
                  fpixel = (float)exp((double)fdata[i]) * slope + offset;
                fpixel = B3DMAX(outmin, fpixel);
                usbufp[i] = B3DMIN(outmax, fpixel) + 0.5f;
              }
            } else {
              for (i = 0; i < xsize; i++) {
                fpixel = fdata[i] * slope + offset;
                fpixel = B3DMAX(outmin, fpixel);
                usbufp[i] = B3DMIN(outmax, fpixel) + 0.5f;
              }
            }
            usbufp += xDimension * deltaYsign;
            bufp = (unsigned char *)usbufp;
          }
          break;

        case MRC_MODE_COMPLEX_FLOAT:

          /* COMPLEX DATA */
          if (hdata->swapped)
            mrc_swap_floats(fdata, xsize * 2);
          if (li->mirrorFFT) {
            fft = bdata;
            usfft = usdata;
            pixIndex = 0;
          }
          for (i = 0; i < xsize; i++, pixIndex++) {
            fpixel = sqrt((double)((fdata[i*2] * fdata[i*2]) + 
                                   (fdata[(i*2)+1] * fdata[(i*2)+1])));
            pixel = log((double)(1.0f + (kscale * fpixel))) * slope + offset;
            pixel = B3DMAX(outmin, pixel);
            pixel = B3DMIN(outmax, pixel);
            if (byte)
              fft[pixIndex] = pixel;
            else
              usfft[pixIndex] = pixel;
          }

          /* MIRRORED FFT DATA */

          if (li->mirrorFFT) {

            /* See if data are needed directly - get intersection with image */
            cury = readY ? cz : j;
            ybase = readY ? j- yStart : cury - imYmin;
            x0 = nx - 1 + xStart;
            x1 = nx - 1 + xEnd;
            if (cury >= imYmin && cury <= imYmax) {
              if (x1 >= li->xmin && x0 <= li->xmax) {
                x2 = x0 > li->xmin ? x0 : li->xmin;
                x3 = x1 < li->xmax ? x1 : li->xmax;
                if (byte)
                  memcpy(&buf[(x2 - li->xmin) + ybase * imXsize], &fft[x2-x0], 
                         x3 + 1 - x2);
                else
                  memcpy(&usbufp[(x2 - li->xmin) + ybase * imXsize], &usfft[x2-x0], 
                         2 * (x3 + 1 - x2));
              }
              /* fprintf(stderr, "direct cury %d x0 %d x1 %d  ", cury, x0, x1);
                 fprintf(stderr, "x2 %d x3 %d\n", x2, x3); */

              /* Is the rightmost pixel needed? */
              if (xEnd == nx - 1 && li->xmin == 0) {
                if (byte)
                  buf[ybase * imXsize] = fft[xsize - 1];
                else
                  usbufp[ybase * imXsize] = usfft[xsize - 1];
              }
            }

            /* See if data are needed for mirror image */
            cury = ny - cury;
            x0 = nx - 1 - xEnd;
            x1 = nx - 1 - xStart;
            if (x1 >= li->xmin && x0 <= li->xmax) {
              x2 = x0 > li->xmin ? x0 : li->xmin;
              x3 = x1 < li->xmax ? x1 : li->xmax;
            
              /* Here the first column and middle column need exclusion 
                 If x2 becomes > x3, the loop will not be executed */
              if (!x2)
                x2 = 1;
              if (x3 >= nx - 1)
                x3 = nx - 2;
              ybase = readY ? j- yStart : cury - imYmin;
              if (cury >= imYmin && cury <= imYmax) {
                /* fprintf(stderr, "mirror cury %d x0 %d x1 %d  ", cury, x0, x1);
                   fprintf(stderr, "x2 %d x3 %d\n", x2, x3); */
                if (byte)
                  for (i = x3 - x2; i >= 0; i--)
                    buf[i + x2 - li->xmin + ybase * imXsize] = fft[x1 - x2 - i];
                else
                  for (i = x3 - x2; i >= 0; i--)
                    usbufp[i + x2 - li->xmin + ybase * imXsize] = usfft[x1 - x2 - i];
              }

              /* Replicate bottom left line if needed */
              ybase = readY ? j- yStart : 0;
              if (cury == 1 && imYmin == 0) {
                if (byte)
                  for (i = x3 - x2; i >= 0; i--)
                    buf[i + x2 - li->xmin + ybase * imXsize] = fft[x1 - x2 - i];
                else
                  for (i = x3 - x2; i >= 0; i--)
                    usbufp[i + x2 - li->xmin + ybase * imXsize] = usfft[x1 - x2 - i];
              }
            }

            /* If toggling between lines, set up seek and next cz value */
            if (toggleY >= 0) {
              toggleY = 1 - toggleY;
              if (toggleY) {
                cz = ymax;
                seekEndY = ymax - ymin - 1;
                j--;
              } else {
                cz = ymin;
                seekEndY = ymin + ny - ymax - 1;
              }
              /* fprintf(stderr, "toggleY %d cz %d seek %d\n",toggleY, cz,
                 seek_endline); */
            }
            /* fflush(stderr); */
          } else {
            pixIndex += xDimension * (deltaYsign - 1);
          }
          break;
        }

      } else if (type == MRSA_FLOAT && hdata->mode != MRC_MODE_FLOAT) {

        /* CONVERSIONS of INPUT DATA TO FLOATS */
        /* Swap, map or copy, advance pointers */
        switch(hdata->mode){
        case MRC_MODE_BYTE:
          if (mapSbytes) 
            for (i = 0; i < xsize; i++)
              fbufp[i] = map[bdata[i]];
          else
            for (i = 0; i < xsize; i++)
              fbufp[i] = bdata[i];
          break;
        
        case MRC_MODE_SHORT:
          if (hdata->swapped)
            mrc_swap_shorts(sdata, xsize * pixSize / 2);
          for (i = 0; i < xsize; i++)
            fbufp[i] = sdata[i];
          break;
        case MRC_MODE_USHORT:
          if (hdata->swapped)
            mrc_swap_shorts(sdata, xsize * pixSize / 2);
          for (i = 0; i < xsize; i++)
            fbufp[i] = usdata[i];
          break;
        }
        fbufp += xDimension * deltaYsign;
        bufp = (unsigned char *)fbufp;
      } else {

        /* RAW DATA - do some swaps, copy if needed, advance buffer pointer */
        if (hdata->swapped) {
          switch(hdata->mode) {
          case MRC_MODE_SHORT:
          case MRC_MODE_USHORT:
          case MRC_MODE_COMPLEX_SHORT:
            mrc_swap_shorts((b3dInt16 *)bdata, xsize * pixSize / 2);
            break;
          case MRC_MODE_FLOAT:
          case MRC_MODE_COMPLEX_FLOAT:
            mrc_swap_floats((b3dFloat *)bdata, xsize * pixSize / 4);
            break;
          default:
            break;
          }
        }
        if (needData)
          memcpy(bufp, bdata, xsize * pixSize);
        bufp += xDimension * deltaYsign * pixSize;
      }

      /* End loop on Y - advance file pointer or data pointer */
      if (chunkLines == 1) {
        if (seekEndX || seekEndY || seekSkip)
          mrcHugeSeek(fin, seekSkip, seekEndX, seekEndY, 0, nx, ny, pixSize, 
                  SEEK_CUR);
      } else {
        bdata += (xsize + seekEndX) * pixSize;
      }
    }
  }

  if (needData)
    free(tmpData);
  if (freeMap)
    free(map);
  return 0;
}

/* WRITING ROUTINES */

/*!
 * Writes one Z slice of raw data at Z value [z] from the buffer [buf] into the 
 * MRC file whose header is in [hdata].  The data must be in the form expected for the 
 * data mode in [hdata], and they are swapped or bytes shifted to signed based on the
 * {swapped} and {bytesSigned} members of [hdata].  The @@IloadInfo structure@ [li] 
 * controls the lines to be written through its members {ymin}
 * and {ymax}; only full lines can be written and {xmin} and {xmax} must be 0 and nx - 1.
 * If it is not already being called by one of the iiWrite functions in 
 * @@iimage.html#TOP@, it looks up whether the {fp} member of [hdata] is on the opened 
 * file list and if so, redirects the call through an iiWrite function.
 * Returns 1 for an illegal request, 2 for a memory error, 3 for an error seeking or
 * writing the file, or if the call is redirected, it can return -1 if no function is 
 * defined for the operation or error codes from I/O functions for other file types.
 */
int mrcWriteZ(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  ImodImageFile *iiFile, iiSave;
  if ((iiFile = lookupIIfile(hdata, li, 3, &iiSave)) != NULL) {
    iiSyncFromMrcHeader(iiFile, hdata);
    if (iiFile->file == IIFILE_MRC && (MrcHeader *)iiFile->header != hdata)
      *((MrcHeader *)iiFile->header) = *hdata;
    return iiRestoreLoadParams(iiWriteSection(iiFile, (char *)buf, z), iiFile, &iiSave);
  }
  return (mrcWriteSectionAny(hdata, li, buf, z, hdata->mode));
}

/*!
 * Writes one Z slice of floating point or complex data, like @mrcWriteZ, but converting 
 * it to the output mode indicated in [hdata].  The input data must be either floats, 
 * complex floats, or complex shorts, and in the latter two cases the output mode must be
 * MRC_MODE_COMPLEX_FLOAT or MRC_MODE_COMPLEX_SHORT, respectively.  The output mode may 
 * not be MRC_MODE_RGB.
 */
int mrcWriteZFloat(MrcHeader *hdata, IloadInfo *li, b3dFloat *buf, int z)
{
  ImodImageFile *iiFile, iiSave;
  if ((iiFile = lookupIIfile(hdata, li, 3, &iiSave)) != NULL) {
    iiSyncFromMrcHeader(iiFile, hdata);
    if (iiFile->file == IIFILE_MRC && (MrcHeader *)iiFile->header != hdata)
      *((MrcHeader *)iiFile->header) = *hdata;
    return iiRestoreLoadParams(iiWriteSectionFloat(iiFile, (char *)buf, z), iiFile, 
                               &iiSave);
  }
  return (mrcWriteSectionAny(hdata, li, (unsigned char *)buf, z, 
                             hdata->mode == MRC_MODE_COMPLEX_FLOAT || 
                             hdata->mode == MRC_MODE_COMPLEX_SHORT ?
                             hdata->mode : MRC_MODE_FLOAT));
}

/* Common writing function, with data conversion if bufMode does not match file mode */
static int mrcWriteSectionAny(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int cz,
                              int bufMode)
{
  FILE *fin = hdata->fp;
  int  nx = hdata->nx;
  int  ny = hdata->ny;
  int  yStart = li->ymin;
  int  yEnd = li->ymax;
  int bytesSigned = (!hdata->mode && hdata->bytesSigned) ? 1 : 0;
  int convert = hdata->mode != bufMode ? 1 : 0;
  int padLeft = B3DMAX(0, li->padLeft);
  int padRight = B3DMAX(0, li->padRight);
  int j, xDimension;
  int pixSizeOut, pixSizeBuf, bytesPerChanOut, numChanOut, bytesPerChanBuf, numChanBuf;
  int needData = 0;
  int chunkLines = 1;
  float targetMemory = 65536.;

  /* Buffer to write lines from; may be replaced by temporary buffer */
  unsigned char *bdata = buf;
  b3dInt16 *sdata;
  char *sbdata;

  /* Copies of buffer pointer that can be advanced after each line */
  unsigned char *bufp = buf;
  b3dFloat *fbufp = (b3dFloat *)buf;
  unsigned char *tmpData = NULL;
  unsigned char *writePtr;
  int lineEnd;
  size_t dataSize;

  if (li->xmin || li->xmax != nx - 1) {
    b3dError(stderr, "ERROR: mrcWriteSectionAny - only full lines can be written\n");
    return 1;
  }
  
  if (mrc_getdcsize(hdata->mode, &bytesPerChanOut, &numChanOut)){
    b3dError(stderr, "ERROR: mrcWriteSectionAny - unknown mode.\n");
    return(-1);
  }
  pixSizeOut = bytesPerChanOut * numChanOut;
  mrc_getdcsize(bufMode, &bytesPerChanBuf, &numChanBuf);
  pixSizeBuf = bytesPerChanBuf * numChanBuf;

  if (convert && sliceModeIfReal(hdata->mode) < 0) {
    b3dError(stderr, "ERROR: mrcWriteSectionAny - floating point data can only be "
             "converted to byte/integer modes\n");
    return 1;
  }

  xDimension = nx + padLeft + padRight;
  bufp += pixSizeBuf * padLeft;
  fbufp += padLeft;

  needData = convert || (hdata->swapped && bytesPerChanOut > 1) || bytesSigned || 
    xDimension > nx;
  chunkLines = yEnd + 1 - yStart;
  if (needData) {
    chunkLines = B3DNINT(targetMemory / nx);
    B3DCLAMP(chunkLines, 1, yEnd + 1 - yStart);
    tmpData = B3DMALLOC(unsigned char, pixSizeOut * nx * chunkLines);
    if (!tmpData) {
      b3dError(stderr, "ERROR: mrcWriteSectionAny - getting memory for temporary "
               "array.\n");
      return 2;
    }
  }
  /*printf("ystart %d yend %d needData %d convert %d chunkLines %d bufmode %d filemode"
     " %d\n", yStart, yEnd, needData, convert, chunkLines, bufMode, hdata->mode);*/
  if (mrcHugeSeek(fin, hdata->headerSize + hdata->sectionSkip * cz, 0, yStart, cz,
                  nx, ny, pixSizeOut, SEEK_SET)) {
    b3dError(stderr, "ERROR: mrcWriteSectionAny - seeking to write location.\n");
    return 3;
  }

  j = yStart;
  while (j <= yEnd) {

    /* Set up the ending line and amount to write */
    lineEnd = B3DMIN(yEnd, j + chunkLines - 1);
    dataSize = (lineEnd + 1 - j) * (size_t)nx;

    /* Reset the bdata pointer to temp data for each line/chunk, or to the buffer pointer
       if reading directly into buffer.  bufp needs to be maintained for all modes when
       a pointer for another mode is advanced */
    if (needData)
      bdata = tmpData;
    else
      bdata = bufp;
    writePtr = bdata;
    
    /* Start loop on Y */
    for (; j <= lineEnd; j++) {
      sdata = (b3dInt16 *)bdata;
      sbdata = (char *)bdata;

      if (convert) {

        /* Convert floats then swap data if needed */
        iiConvertLineOfFloats(fbufp, bdata, nx, hdata->mode, bytesSigned);
        if (hdata->swapped && hdata->mode)
          mrc_swap_shorts(sdata, nx);
        fbufp += xDimension;
        bufp = (unsigned char *)fbufp;

      } else {

        /* Unconverted output, copy and swap or shift bytes if needed */
        if (hdata->swapped && bytesPerChanOut > 1) {
          memcpy(bdata, bufp, pixSizeOut * nx);
          if (bytesPerChanOut == 2)
            mrc_swap_shorts(sdata, nx * numChanOut);
          else
            mrc_swap_floats((b3dFloat *)bdata, nx * numChanOut);
        } else if (bytesSigned) {
          b3dShiftBytes(bufp, sbdata, nx, 1, 1, 1);
        } else if (needData) {
          memcpy(bdata, bufp, pixSizeOut * nx);
        }
        bufp += pixSizeBuf * xDimension;
      }

      /* Advance output data pointer */
      bdata += pixSizeOut * nx;
    }

    /* Write the chunk of data */
    errno = 0;
    if (b3dFwrite(writePtr, pixSizeOut, dataSize, fin) != dataSize) {
      b3dError(stderr, "ERROR: mrcWriteSectionAny - writing data to file (system message:"
               " %s)\n", strerror(errno));
      B3DFREE(tmpData);
      return 3;
    }
  }
  B3DFREE(tmpData);
  return 0;
}

/*
 * If this is not already a call from an II function, look up the iiFile from the file 
 * pointer, and if it is not MRC-like, it fills in the I/O limits for the given axis and
 * returns the iiFile.
 */
static ImodImageFile *lookupIIfile(MrcHeader *hdata, IloadInfo *li, int axis, 
                                   ImodImageFile *iiSave)
{
  ImodImageFile *iiFile;
  if (iiCallingReadOrWrite())
    return NULL;
  iiFile = iiLookupFileFromFP(hdata->fp);
  if (!iiFile || iiFile->file == IIFILE_MRC || iiFile->file == IIFILE_RAW)
    return NULL;

  /* Save original parameters */
  iiSaveLoadParams(iiFile, iiSave);

  /* Set up the load */
  iiFile->llx = li->xmin;
  iiFile->urx = li->xmax;
  if (axis == 3) {
    iiFile->lly = li->ymin;
    iiFile->ury = li->ymax;
  } else {
    iiFile->llz = li->zmin;
    iiFile->urz = li->zmax;
  }
  iiFile->axis = axis;
  iiFile->padLeft = li->padLeft;
  iiFile->padRight = li->padRight;
  iiFile->slope = li->slope;
  iiFile->offset = li->offset;
  return iiFile;
}
