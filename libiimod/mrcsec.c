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
 * Generic routine for reading the data in lines or chunks and processing it line-by-line
 * with proper scaling.
 */
static int mrcReadSectionAny(MrcHeader *hdata, IloadInfo *li,
                              unsigned char *buf, int cz, int readY, int type)
{
  FILE *fin = hdata->fp;
  int pixSizeBuf[4] = {0, 1, 4, 2};
  LineProcData d;
  int  nx = hdata->nx;
  int  ny = hdata->ny;
  int padLeft = B3DMAX(0, li->padLeft);
  int padRight = B3DMAX(0, li->padRight);
  int  yEnd = readY ? li->zmax : li->ymax;
  int seekLine, seekEndX, seekSkip;
  float targetMemory = 65536.;
  float multiLineCrit = 0.5;
  int err, chunkLines = 1;
  int freeMap = 0;
  unsigned char *tmpData;
  int needRead, lineEnd;
  size_t dataSize;

  /* Initialize members of data structure */
  d.type = type;
  d.readY = readY;
  d.cz = cz;
  d.swapped = hdata->swapped;
  err = iiInitReadSectionAny(hdata, li, buf, &d, &freeMap, &yEnd, "mrcReadSectionAny");
  if (err)
    return err;
  
  dataSize = d.xsize;
  d.xDimension = d.xsize + padLeft + padRight;
  pixSizeBuf[0] = d.pixSize;

  /* Need to buffer if there is padding in X */
  if (d.xDimension > d.xsize)
    d.needData = 1;

  /* If Y is inverted, adjust the Y load, adjust output pointers to last line and set sign
   for incrementing pointers at end of line, in order to fill output array backwards */
  if (hdata->yInverted) {
    if (li->mirrorFFT) {
      b3dError(stderr, "ERROR: mrcReadSectionAny - cannot mirror FFT with inverted Y "
               "data.\n");
      return 1;
    }
    d.line = ny - 1 - d.yStart;
    d.yStart = ny - 1 - yEnd;
    yEnd = d.line;
    d.line = (yEnd - d.yStart) * d.xsize;
    d.bdata += d.pixSize * d.line;
    d.pixIndex += d.line;
    d.bufp += pixSizeBuf[type] * d.line;
    d.usbufp += d.line;
    d.fbufp += d.line;
    d.deltaYsign = -1;
  }

  /* Advance pointers by left padding before starting */
  d.bufp += pixSizeBuf[type] * padLeft;
  d.usbufp += padLeft;
  d.fbufp += padLeft;
  d.pixIndex += padLeft;

  /* Determine if doing multi-line reads: not if reading in Y, inverted Y, or mirroring
     FFT, and only if amount needed is a large enough fraction of the full line.  Only
     do it if it will involve more than one line of buffer, or if there is no buffer */
  if (!readY && !(d.convert && li->mirrorFFT) && !hdata->yInverted && 
      (float)d.xsize / nx > multiLineCrit && 
      (B3DNINT(targetMemory / nx) > 1 || (!d.needData && nx == d.xsize))) {

    /* Need buffer if it is not full-sized in X; limit the chunk lines by memory usage
       for a buffer but do the whole read otherwise */
    if (nx > d.xsize)
      d.needData = 1;
    chunkLines = yEnd + 1 - d.yStart;
    if (d.needData) {
      chunkLines = B3DMAX(1, B3DMIN(chunkLines, B3DNINT(targetMemory / nx)));
      dataSize = nx * chunkLines;
    }
  }
  /* printf("ps %d cl %d nd %d ds %d nx %d xsize %d pl %d pr %d\n", d.pixSize, chunkLines,
     d.needData, (int)dataSize, nx, d.xsize, padLeft, padRight);*/

  /* Get the supplemental data array, set all pointers to it */
  if (d.needData) {
    tmpData = (unsigned char *)malloc(d.pixSize * dataSize);
    if (!tmpData) {
      if (freeMap)
        free(d.map);
      b3dError(stderr, "ERROR: mrcReadSectionAny - getting memory for "
               "temporary array.\n");
      return 2;
    }
  }

  /* 12/1/04: no longer fix header size in case it is weird */

  /* Seek distance at start of line (in bytes) and end of line (in pixels) */
  seekLine    = d.xStart * d.pixSize;
  seekEndX = B3DMAX(0, nx - d.xEnd - 1);
  seekSkip = 0;
     
  if (readY) {

    /* If reading Y, seek at end of line gets to next section, first seek
       is small and seek to starting Z is big */
    /* b3dFseek(fin, hdata->headerSize + (d.cz * nx * d.pixSize),  SEEK_SET);
    if (d.yStart)
    mrc_big_seek(fin, 0, d.yStart, nx * ny * d.pixSize, SEEK_CUR); */
    d.seekEndY = ny - 1;
    mrcHugeSeek(fin, hdata->headerSize + hdata->sectionSkip * d.yStart, 0, d.cz, d.yStart,
                nx, ny, d.pixSize, SEEK_SET);
    seekSkip = hdata->sectionSkip;
  } else {

    /* If reading X, seek at end of line gets to end of line, first seek
       is in Z and big, seek to starting Y is small */
    /* mrc_big_seek(fin, hdata->headerSize, d.cz, nx * ny * d.pixSize,  SEEK_SET);
    if (d.yStart)
    b3dFseek(fin, d.yStart * nx * d.pixSize, SEEK_CUR); */
    d.seekEndY = 0;
    mrcHugeSeek(fin, hdata->headerSize + hdata->sectionSkip * d.cz, 0, d.yStart, d.cz,
                nx, ny, d.pixSize, SEEK_SET);
  }

  /* Loop on chunks in Y */
  /*printf("chunkLines %d  needData %d mode %d byte %d toShort %d convert %d doScale %d"
     " xseek %d\n", chunkLines, d.needData, hdata->mode, d.byte, d.toShort, d.convert, 
     d.doScale, seekEndX);*/
  d.line = d.yStart;
  while (d.line <= yEnd) {

    /* Set up the ending line and amount to read */
    lineEnd = B3DMIN(yEnd, d.line + chunkLines - 1);
    dataSize = d.xsize;
    needRead = 1;
    if (chunkLines > 1)
      dataSize = (lineEnd + 1 - d.line) * (size_t)nx;

    /* Reset the bdata pointer to temp data for each line/chunk, or to the buffer pointer
       if reading directly into buffer.  bufp needs to be maintained for all modes when
       a pointer for another mode is advanced */
    if (d.needData)
      d.bdata = tmpData;
    else
      d.bdata = d.bufp;
    
    /* Start loop on Y and read a line or chunk of data */
    for (; d.line <= lineEnd; d.line++) {
      if (seekLine && chunkLines == 1)
        b3dFseek(fin, seekLine, SEEK_CUR);
      if (needRead && b3dFread(d.bdata, d.pixSize, dataSize, fin) != dataSize) {
        b3dError(stderr, "ERROR: mrcReadSectionAny - reading data from file.\n");
        if (d.needData)
          free(tmpData);
        if (freeMap)
          free(d.map);
        return 3;
      }
      needRead = 0;

      /* Adjust to start of line for chunk reading, then assign pointers for all modes */
      if (chunkLines > 1)
        d.bdata += seekLine;
      if (iiProcessReadLine(hdata, li, &d))
        return 1;

      /* End loop on Y - advance file pointer or data pointer */
      if (chunkLines == 1) {
        if (seekEndX || d.seekEndY || seekSkip)
          mrcHugeSeek(fin, seekSkip, seekEndX, d.seekEndY, 0, nx, ny, d.pixSize, 
                  SEEK_CUR);
      } else {
        d.bdata += (d.xsize + seekEndX) * d.pixSize;
      }
    }
  }

  if (d.needData)
    free(tmpData);
  if (freeMap)
    free(d.map);
  return 0;
}

/*!
 * Initialize most variables in the data structure, figure out scaling and need for data 
 * conversion and buffering, set up the dreaded FFT mirroring, get pixel size and mapping
 */
int iiInitReadSectionAny(MrcHeader *hdata, IloadInfo *li, unsigned char *buf,
                         LineProcData *d, int *freeMap, int *yEnd, const char *caller)
{
  float eps;
  int  nx = hdata->nx;
  int  ny = hdata->ny;
  int llfx, llfy, ulfx, ulfy, urfx, urfy;
  int lrfx, lrfy, imNx;
  int xStart2, llfx2, llfy2, ulfx2, ulfy2;

  d->xStart = li->xmin;
  d->xEnd = li->xmax;
  d->yStart = d->readY ? li->zmin : li->ymin;
  d->xsize = d->xEnd - d->xStart + 1;
  d->byte = d->type == MRSA_BYTE ? 1 : 0;
  d->toShort = d->type == MRSA_USHORT ? 1 : 0;
  d->mapSbytes = (!hdata->mode && hdata->bytesSigned) ? 1 : 0;
  d->convert = d->byte + d->toShort;
  d->pixIndex = 0;
  d->deltaYsign = 1;
  d->pixSize = 1;
  d->needData = 0;
  eps = d->toShort ? 0.005 / 256. : 0.005;
  d->doScale = (li->offset <= -1.0 || li->offset >= 1.0 || 
                 li->slope < 1. - eps || li->slope > 1. + eps) ? 1 : 0;

  /* Buffer to read lines into; may be replaced by temporary buffer for reading */
  d->bdata = buf;
  d->buf = buf;

  /* Copies of buffer pointer that can be advanced after each line */
  d->bufp = buf;
  d->fbufp = (b3dFloat *)buf;
  d->usbufp = (b3dUInt16 *)buf;

  /* Buffer to place complex data into; may be temporary for mirroring */
  d->fft = buf;
  d->usfft = (b3dUInt16 *)buf;
  d->map = NULL;

  /* Raw bytes need to be treated like a conversion if they need signed->unsigned map */
  if (!d->type && d->mapSbytes)
    d->convert = 1;

  if (d->type == MRSA_FLOAT && sliceModeIfReal(hdata->mode) < 0) {
    b3dError(stderr, "ERROR: %s - Only real modes can be read"
             " as floats\n", caller);
    return 1;
  }
  if ((d->type == MRSA_BYTE && li->outmax > 255) || 
      (d->type == MRSA_USHORT && li->outmax < 256)) {
    b3dError(stderr, "ERROR: %s - outmax (%d) is not in right range for"
             " conversion to %s\n", caller, li->outmax, d->byte ? "bytes" : "shorts");
    return 1;
  }

  /* Adjust loading parameters if mirroring an FFT 
     This is hopelessly complex because it replicates the mirrored FFT produced
     by clip, with the extra right column placed on the left and the bottom
     left row a duplicate of the row above (except for first pixel) */
  if (li->mirrorFFT && d->convert) {
    d->imXsize = d->xsize;
    imNx = 2 * (nx - 1);

    /* Get source for four corners, including a point in from the left edge
       if necessary, and adjust xmin if all to one side of Y axis */
    d->imYmin = d->readY ? d->cz : li->ymin;
    d->imYmax = d->readY ? d->cz : li->ymax;
    xStart2 = d->xStart < d->xEnd ? d->xStart + 1 : d->xStart;
    mrcMirrorSource(imNx, ny, d->xStart, d->imYmin, &llfx, &llfy);
    mrcMirrorSource(imNx, ny, d->xStart, d->imYmax, &ulfx, &ulfy);
    mrcMirrorSource(imNx, ny, xStart2, d->imYmin, &llfx2, &llfy2);
    mrcMirrorSource(imNx, ny, xStart2, d->imYmax, &ulfx2, &ulfy2);
    mrcMirrorSource(imNx, ny, d->xEnd, d->imYmin, &lrfx, &lrfy);
    mrcMirrorSource(imNx, ny, d->xEnd, d->imYmax, &urfx, &urfy);
    d->xStart = 0;
    if (d->xEnd < imNx / 2)
      d->xStart = lrfx;
    if (d->xStart > imNx / 2)
      d->xStart = llfx;

    /* Get xmax, ymin, ymax from these corners; get y limits and xsize */
    d->xEnd = b3dIMax(3, urfx, llfx, llfx2);
    d->ymin = b3dIMin(6, llfy, ulfy, lrfy, urfy, llfy2, ulfy2);
    d->ymax = b3dIMax(6, llfy, ulfy, lrfy, urfy, llfy2, ulfy2);
    d->yStart = d->readY ? li->zmin : d->ymin;
    *yEnd = d->readY ? li->zmax : d->ymax;
    d->xsize = d->xEnd - d->xStart + 1;
  
    /* Set up flip-flop between two lines if needed when reading in Y */
    d->toggleY = -1;
    if (d->readY && d->ymin < d->ymax) {
      d->toggleY = 0;
      d->cz = d->ymin;
    }
    /*fprintf(stderr, "xStart %d xEnd %d yStart %d yEnd %d xsize %d d->ymin %d ymax %d "
       "imYmin %d imYmax %d\n", d->xStart, d->xEnd, d->yStart, *yEnd, d->xsize, d->ymin,
       d->ymax, d->imYmin, d->imYmax);
       fflush(stderr); */
  }

  if (d->cz < 0 || d->cz >= (d->readY ? hdata->ny : hdata->nz) || 
      d->yStart < 0 || *yEnd >= (d->readY ? hdata->nz : hdata->ny)) {
    b3dError(stderr, "ERROR: %s - Requested area to read is out of range for file size\n",
             caller);
    return 1;
  }

  /* get pixel size based on mode, and prepare scaling and set flags if
     need another data array */
  switch(hdata->mode){
  case MRC_MODE_BYTE:
    d->pixSize = 1;

    /* Get a scaling map if going to bytes with scaling or if going to shorts, or get a
       signed->unsigned map if signed bytes go to raw or float or unscaled bytes */
    if ((d->byte && d->doScale) || d->toShort)
      d->map = get_byte_map(li->slope, li->offset, li->outmin, li->outmax,
                            hdata->bytesSigned);
    else if (d->mapSbytes)
      d->map = get_byte_map(1.0, 0., 0, 255, 1);
    d->needData = (d->type == MRSA_FLOAT || d->type == MRSA_USHORT) ? 1 : 0;
    break;

  case MRC_MODE_SHORT:
  case MRC_MODE_USHORT:
    d->pixSize = 2;
    if ((d->toShort && (d->doScale || hdata->mode == MRC_MODE_SHORT)) || d->byte) {
      d->map= get_short_map(li->slope, li->offset, li->outmin, li->outmax, li->ramp, 
                            d->swapped, (hdata->mode == MRC_MODE_SHORT) ? 1 : 0);
      /*d->usmap = (b3dUInt16 *)d->map;
      for (i = 0; i < 65000; i += 1000)
      printf("%d %d %d\n", i, d->usd->map[i], d->map[i]); */
      *freeMap = 1;
      if (!d->map)
        return 2;
    }
    d->needData = (d->type == MRSA_FLOAT || d->type == MRSA_BYTE) ? 1 : 0;
    break;

  case MRC_MODE_RGB:
    d->pixSize = 3;
    d->needData = d->convert;
    break;

  case MRC_MODE_FLOAT:
    d->pixSize = 4;
    d->needData = d->convert;
    break;

  case MRC_MODE_COMPLEX_SHORT:
    d->pixSize = 4;
    if (d->convert)
      return 1;
    break;

  case MRC_MODE_COMPLEX_FLOAT:
    d->pixSize = 8;
    d->needData = d->convert;
    break;

  default:
    if (d->convert)
      b3dError(stderr, "ERROR: %s - unsupported data type.\n", caller);
      return 1;
    break;
  }
  d->usmap = (b3dUInt16 *)d->map;
  d->bytesSinceCheck = 0;
  return 0;
}

/*!
 * Process one line of data that has been read in according to the conversion needs,
 * and maintain the buffer pointers
 */
int iiProcessReadLine(MrcHeader *hdata, IloadInfo *li, LineProcData *d)
{
  int nx = hdata->nx;
  int ny = hdata->ny;
  int sysExtraRead = 100000;
  int statusInterval = 4000000;
  unsigned char *bdata = d->bdata;
  unsigned char *buf = d->buf;
  unsigned char *fft = d->fft;
  b3dUInt16 *usfft = d->usfft;
  unsigned char *map = d->map;
  b3dUInt16 *usmap = d->usmap;
  b3dInt16 *sdata = (b3dInt16 *)bdata;
  b3dUInt16 *usdata = (b3dUInt16 *)bdata;
  b3dFloat *fdata = (b3dFloat *)bdata;
  float slope  = li->slope;
  float offset = li->offset;
  int   outmin = li->outmin;
  int   outmax = li->outmax;
  float kscale = mrcGetComplexScale();
  int imXsize = d->imXsize;
  unsigned char *inptr;
  int pixel, i, cury, ybase, x0, x1, x2, x3;
  b3dFloat fpixel;

  /* Pointers and indexes that are modified and copied back at the end.  For all modes,
     bufp needs to be maintained when a pointer for another mode is advanced */
  unsigned char *bufp = d->bufp;
  b3dUInt16 *usbufp = d->usbufp;
  b3dFloat *fbufp = d->fbufp;
  unsigned int pixIndex = d->pixIndex;

  /* Do data-dependent processing for byte conversions or mappings */
  if (d->convert) {
    switch (hdata->mode) {
    case MRC_MODE_BYTE:    

      /* Byte to byte: mapping, copying, or data already present */
      if (d->byte || (!d->toShort && d->mapSbytes)) {
        if (d->doScale || d->mapSbytes)
          for (i = 0; i < d->xsize; i++)
            bufp[i] = map[bdata[i]];
        else if (d->needData)
          memcpy(bufp, bdata, d->xsize);
        bufp += d->xDimension * d->deltaYsign;
      } else {             

        /* Byte to ushort */
        for (i = 0; i < d->xsize; i++)
          usbufp[i] = usmap[bdata[i]];
        usbufp += d->xDimension * d->deltaYsign;
        bufp = (unsigned char *)usbufp;
      }
      break;
        
    case MRC_MODE_SHORT:
    case MRC_MODE_USHORT:
          
      /* Short to byte */
      if (d->byte) {
        for (i = 0; i < d->xsize; i++)
          bufp[i] = map[usdata[i]];
        bufp += d->xDimension * d->deltaYsign;
      } else {

        /* Short to ushort: mapping, copying, or data already present */
        if (d->doScale || hdata->mode == MRC_MODE_SHORT)
          for (i = 0; i < d->xsize; i++)
            usbufp[i] = usmap[usdata[i]];
        else if (d->needData)
          memcpy(usbufp, usdata, d->xsize * 2);
        usbufp += d->xDimension * d->deltaYsign;
        bufp = (unsigned char *)usbufp;
      }
      break;

    case MRC_MODE_RGB:
      inptr = bdata;
      if (d->byte) {

        /* RGB to byte */
        for (i = 0; i < d->xsize; i++) {
          fpixel = 0.3 * *inptr++;
          fpixel += 0.59 * *inptr++;
          fpixel += 0.11 * *inptr++;
          bufp[i] = (int)(fpixel + 0.5f);
        }
        bufp += d->xDimension * d->deltaYsign;
      } else {
            
        /* RGB to ushort */
        for (i = 0; i < d->xsize; i++) {
          fpixel = 255. * 0.3 * *inptr++;
          fpixel += 255. * 0.59 * *inptr++;
          fpixel += 255. * 0.11 * *inptr++;
          usbufp[i] = (int)(fpixel + 0.5f);
        }
        usbufp += d->xDimension * d->deltaYsign;
        bufp = (unsigned char *)usbufp;
      }
      break;
        
    case MRC_MODE_FLOAT:
      if (d->swapped)
        mrc_swap_floats(fdata, d->xsize);
      if (d->byte) {

        /* Float to byte */
        /* Do unused ramps separately to speed up the regular load */
        if (li->ramp == MRC_RAMP_LOG || li->ramp == MRC_RAMP_EXP) {
          for (i = 0; i < d->xsize; i++){
            if (li->ramp == MRC_RAMP_LOG)
              fpixel = (float)log((double)fdata[i]) * slope + offset;
            else
              fpixel = (float)exp((double)fdata[i]) * slope + offset;
            fpixel = B3DMAX(outmin, fpixel);
            bufp[i] = B3DMIN(outmax, fpixel) + 0.5f;
          }
        } else {
          for (i = 0; i < d->xsize; i++) {
            fpixel = fdata[i] * slope + offset;
            fpixel = B3DMAX(outmin, fpixel);
            bufp[i] = B3DMIN(outmax, fpixel) + 0.5f;
          }
        }
        bufp += d->xDimension * d->deltaYsign;
      } else {

        /* Float to ushort */
        if (li->ramp == MRC_RAMP_LOG || li->ramp == MRC_RAMP_EXP) {
          for (i = 0; i < d->xsize; i++){
            if (li->ramp == MRC_RAMP_LOG)
              fpixel = (float)log((double)fdata[i]) * slope + offset;
            else
              fpixel = (float)exp((double)fdata[i]) * slope + offset;
            fpixel = B3DMAX(outmin, fpixel);
            usbufp[i] = B3DMIN(outmax, fpixel) + 0.5f;
          }
        } else {
          for (i = 0; i < d->xsize; i++) {
            fpixel = fdata[i] * slope + offset;
            fpixel = B3DMAX(outmin, fpixel);
            usbufp[i] = B3DMIN(outmax, fpixel) + 0.5f;
          }
        }
        usbufp += d->xDimension * d->deltaYsign;
        bufp = (unsigned char *)usbufp;
      }
      break;

    case MRC_MODE_COMPLEX_FLOAT:

      /* COMPLEX DATA */
      if (d->swapped)
        mrc_swap_floats(fdata, d->xsize * 2);
      if (li->mirrorFFT) {
        fft = bdata;
        usfft = usdata;
        pixIndex = 0;
      }
      for (i = 0; i < d->xsize; i++, pixIndex++) {
        fpixel = sqrt((double)((fdata[i*2] * fdata[i*2]) + 
                               (fdata[(i*2)+1] * fdata[(i*2)+1])));
        pixel = log((double)(1.0f + (kscale * fpixel))) * slope + offset;
        pixel = B3DMAX(outmin, pixel);
        pixel = B3DMIN(outmax, pixel);
        if (d->byte)
          fft[pixIndex] = pixel;
        else
          usfft[pixIndex] = pixel;
      }

      /* MIRRORED FFT DATA */

      if (li->mirrorFFT) {

        /* See if data are needed directly - get intersection with image */
        cury = d->readY ? d->cz : d->line;
        ybase = d->readY ? d->line- d->yStart : cury - d->imYmin;
        x0 = nx - 1 + d->xStart;
        x1 = nx - 1 + d->xEnd;
        if (cury >= d->imYmin && cury <= d->imYmax) {
          if (x1 >= li->xmin && x0 <= li->xmax) {
            x2 = x0 > li->xmin ? x0 : li->xmin;
            x3 = x1 < li->xmax ? x1 : li->xmax;
            if (d->byte)
              memcpy(&buf[(x2 - li->xmin) + ybase * imXsize], &fft[x2-x0], x3 + 1 - x2);
            else
              memcpy(&usbufp[(x2 - li->xmin) + ybase * imXsize], &usfft[x2-x0], 
                     2 * (x3 + 1 - x2));
          }
          /* fprintf(stderr, "direct cury %d x0 %d x1 %d  ", cury, x0, x1);
             fprintf(stderr, "x2 %d x3 %d\n", x2, x3); */

          /* Is the rightmost pixel needed? */
          if (d->xEnd == nx - 1 && li->xmin == 0) {
            if (d->byte)
              buf[ybase * imXsize] = fft[d->xsize - 1];
            else
              usbufp[ybase * imXsize] = usfft[d->xsize - 1];
          }
        }

        /* See if data are needed for mirror image */
        cury = ny - cury;
        x0 = nx - 1 - d->xEnd;
        x1 = nx - 1 - d->xStart;
        if (x1 >= li->xmin && x0 <= li->xmax) {
          x2 = x0 > li->xmin ? x0 : li->xmin;
          x3 = x1 < li->xmax ? x1 : li->xmax;
            
          /* Here the first column and middle column need exclusion 
             If x2 becomes > x3, the loop will not be executed */
          if (!x2)
            x2 = 1;
          if (x3 >= nx - 1)
            x3 = nx - 2;
          ybase = d->readY ? d->line- d->yStart : cury - d->imYmin;
          if (cury >= d->imYmin && cury <= d->imYmax) {
            /* fprintf(stderr, "mirror cury %d x0 %d x1 %d  ", cury, x0, x1);
               fprintf(stderr, "x2 %d x3 %d  min %d base %d size %d ind %d\n", x2, x3,
               li->xmin, ybase, imXsize, x2 - li->xmin + ybase * imXsize); */
            if (d->byte)
              for (i = x3 - x2; i >= 0; i--)
                buf[i + x2 - li->xmin + ybase * imXsize] = fft[x1 - x2 - i];
            else
              for (i = x3 - x2; i >= 0; i--)
                usbufp[i + x2 - li->xmin + ybase * imXsize] = usfft[x1 - x2 - i];
          }

          /* Replicate bottom left line if needed */
          ybase = d->readY ? d->line- d->yStart : 0;
          if (cury == 1 && d->imYmin == 0) {
            if (d->byte)
              for (i = x3 - x2; i >= 0; i--)
                buf[i + x2 - li->xmin + ybase * imXsize] = fft[x1 - x2 - i];
            else
              for (i = x3 - x2; i >= 0; i--)
                usbufp[i + x2 - li->xmin + ybase * imXsize] = usfft[x1 - x2 - i];
          }
        }

        /* If toggling between lines, set up seek and next cz value */
        if (d->toggleY >= 0) {
          d->toggleY = 1 - d->toggleY;
          if (d->toggleY) {
            d->cz = d->ymax;
            d->seekEndY = d->ymax - d->ymin - 1;
            d->line--;
          } else {
            d->cz = d->ymin;
            d->seekEndY = d->ymin + ny - d->ymax - 1;
          }
          /*fprintf(stderr, "toggleY %d cz %d j %d seek %d\n",d->toggleY, d->cz, d->line,
            d->seekEndY);*/
        }
        /* fflush(stderr); */
      } else {
        pixIndex += d->xDimension * (d->deltaYsign - 1);
      }
      break;
    }

  } else if (d->type == MRSA_FLOAT && hdata->mode != MRC_MODE_FLOAT) {

    /* CONVERSIONS of INPUT DATA TO FLOATS */
    /* Swap, map or copy, advance pointers */
    switch(hdata->mode){
    case MRC_MODE_BYTE:
      if (d->mapSbytes) 
        for (i = 0; i < d->xsize; i++)
          fbufp[i] = map[bdata[i]];
      else
        for (i = 0; i < d->xsize; i++)
          fbufp[i] = bdata[i];
      break;
        
    case MRC_MODE_SHORT:
      if (d->swapped)
        mrc_swap_shorts(sdata, d->xsize * d->pixSize / 2);
      for (i = 0; i < d->xsize; i++)
        fbufp[i] = sdata[i];
      break;
    case MRC_MODE_USHORT:
      if (d->swapped)
        mrc_swap_shorts(sdata, d->xsize * d->pixSize / 2);
      for (i = 0; i < d->xsize; i++)
        fbufp[i] = usdata[i];
      break;
    }
    fbufp += d->xDimension * d->deltaYsign;
    bufp = (unsigned char *)fbufp;
  } else {

    /* RAW DATA - do some swaps, copy if needed, advance buffer pointer */
    if (d->swapped) {
      switch(hdata->mode) {
      case MRC_MODE_SHORT:
      case MRC_MODE_USHORT:
      case MRC_MODE_COMPLEX_SHORT:
        mrc_swap_shorts((b3dInt16 *)bdata, d->xsize * d->pixSize / 2);
        break;
      case MRC_MODE_FLOAT:
      case MRC_MODE_COMPLEX_FLOAT:
        mrc_swap_floats((b3dFloat *)bdata, d->xsize * d->pixSize / 4);
        break;
      default:
        break;
      }
    }
    if (d->needData)
      memcpy(bufp, bdata, d->xsize * d->pixSize);
    bufp += d->xDimension * d->deltaYsign * d->pixSize;
  }

  /* Pass back the changed pointers and index */
  d->pixIndex = pixIndex;
  d->bufp = bufp;
  d->usbufp = usbufp;
  d->fbufp = fbufp;

  /* Keep track of bytes possibly read by system and check for quit */
  d->bytesSinceCheck += B3DMIN(d->xsize * d->pixSize + sysExtraRead, 
                               hdata->nx * d->pixSize);
  if (d->bytesSinceCheck > statusInterval) {
    d->bytesSinceCheck = 0;
    return iiCheckForQuit(d->cz);
  }
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
