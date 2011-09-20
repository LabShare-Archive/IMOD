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
#include "b3dutil.h"
#include "mrcfiles.h"

#define MRSA_BYTE 1
#define MRSA_FLOAT 2
#define MRSA_USHORT 3

static int mrcReadSectionAny(MrcHeader *hdata, IloadInfo *li,
                             unsigned char *buf, int cz, int readY, int type);

/*
 * Routines for accessing particular kinds of data as bytes or raw, Y or Z
 * axis.  They should be able to handle planes > 4 GB on a 64-bit system except
 * for complex data.
 */

/*!
 * Reads one Z slice of raw data at Z value [z] into the buffer [buf] from the 
 * MRC file whose header is in [hdata].  The @@IloadInfo structure@ [li] 
 * controls the subarea loaded through its members {xmin}, {xmax}, {ymin},
 * and {ymax}.
 * Returns 1 for an illegal request, 2 for a memory error, or 3 for an error 
 * reading the file.
 */
int mrcReadZ(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  return (mrcReadSectionAny(hdata, li, buf, z, 0, 0));
}

/*!
 * Reads one Z slice of data, like @mrcReadZ, and scales it to bytes.
 * Scaling is controlled by [li] members {slope}, {offset}, and {ramp}, but
 * {ramp} should be MRC_RAMP_LIN unless reading in integers or floats.
 */
int mrcReadZByte(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  return (mrcReadSectionAny(hdata, li, buf, z, 0, MRSA_BYTE));
}

/*!
 * Reads one Z slice of data, like @mrcReadZ, and scales it to unsigned short integers.
 * Scaling is controlled by [li] members {slope}, {offset}, and {ramp}, but
 * {ramp} should be MRC_RAMP_LIN unless reading in integers or floats.
 */
int mrcReadZUShort(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  return (mrcReadSectionAny(hdata, li, buf, z, 0, MRSA_USHORT));
}

/*!
 * Reads one Z slice of data, like @mrcReadZ, and returns it as floats.
 * Only works for real modes: byte, signed and unsigned short, float.
 */
int mrcReadZFloat(MrcHeader *hdata, IloadInfo *li, b3dFloat *buf, int z)
{
  return (mrcReadSectionAny(hdata, li, (unsigned char *)buf, z, 0,MRSA_FLOAT));
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
  return (mrcReadSectionAny(hdata, li, buf, z, 1, 0));
}

/*!
 * Reads one Y slice of data, like @mrcReadY, and scales it to bytes.
 * Scaling is controlled by [li] members {slope}, {offset}, and {ramp}, but
 * {ramp} should be MRC_RAMP_LIN unless reading in integers or floats.
 */
int mrcReadYByte(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  return (mrcReadSectionAny(hdata, li, buf, z, 1, MRSA_BYTE));
}

/*!
 * Reads one Y slice of data, like @mrcReadY, and scales it to unsigned short integers.
 * Scaling is controlled by [li] members {slope}, {offset}, and {ramp}, but
 * {ramp} should be MRC_RAMP_LIN unless reading in integers or floats.
 */
int mrcReadYUShort(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  return (mrcReadSectionAny(hdata, li, buf, z, 1, MRSA_USHORT));
}

/*!
 * Reads one Y slice of data, like @mrcReadY, and returns it as floats.
 * Only works for real modes: byte, signed and unsigned short, and float.
 */
int mrcReadYFloat(MrcHeader *hdata, IloadInfo *li, b3dFloat *buf, int z)
{
  return (mrcReadSectionAny(hdata, li, (unsigned char *)buf, z, 1,MRSA_FLOAT));
}

/*!
 * Reads one slice of raw data into the buffer [buf] from the 
 * MRC file whose header is in [hdata].  A Y slice at Y = [z] is loaded if the
 * {axis} member of [li] is 2; otherwise a Z slice at Z = [z] is loaded.
 * The subarea loaded is determined by [li] members {xmin}, {xmax}, and {ymin}
 * and {ymax} (for a Z slice) or {zmin} and {zmax} (for a Y slice).
 * Returns 1 for an illegal request, 2 for a memory error, or 3 for an error 
 * reading the file.
 */
int mrcReadSection(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  int readY = (li->axis == 2) ? 1 : 0;
  return (mrcReadSectionAny(hdata, li, buf, z, readY, 0));
}

/*!
 * Reads one slice of data, like @mrcReadSection, and scales it to bytes.
 * Scaling is controlled by [li] members {slope}, {offset}, and {ramp}, but
 * {ramp} should be MRC_RAMP_LIN unless reading in integers or floats.
 */
int mrcReadSectionByte(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  int readY = (li->axis == 2) ? 1 : 0;
  return (mrcReadSectionAny(hdata, li, buf, z, readY, MRSA_BYTE));
}

/*!
 * Reads one slice of data, like @mrcReadSection, and scales it to bytes.
 * Scaling is controlled by [li] members {slope}, {offset}, and {ramp}, but
 * {ramp} should be MRC_RAMP_LIN unless reading in integers or floats.
 */
int mrcReadSectionUShort(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, int z)
{
  int readY = (li->axis == 2) ? 1 : 0;
  return (mrcReadSectionAny(hdata, li, buf, z, readY, MRSA_USHORT));
}

/*!
 * Reads one slice of data, like @mrcReadSection, and returns it as floats.
 * Only works for real modes: byte, signed and unsigned short, and float.
 */
int mrcReadSectionFloat(MrcHeader *hdata, IloadInfo *li, b3dFloat *buf, int z)
{
  int readY = (li->axis == 2) ? 1 : 0;
  return (mrcReadSectionAny(hdata, li, (unsigned char *)buf, z, readY, 
                            MRSA_FLOAT));
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
     
  int  llx = li->xmin;
  int  urx = li->xmax;
  int  lly = readY ? li->zmin : li->ymin;
  int  ury = readY ? li->zmax : li->ymax;
  int xsize = urx - llx + 1;
  int byte = type == MRSA_BYTE ? 1 : 0;
  int toShort = type == MRSA_USHORT ? 1 : 0;
  int mapSbytes = (!hdata->mode && hdata->bytesSigned) ? 1 : 0;
  int convert = byte + toShort;
     
  float slope  = li->slope;
  float offset = li->offset;
  int   outmin = li->outmin;
  int   outmax = li->outmax;
  int seek_line, seek_endx, seek_endy, seek_skip;
  float kscale = mrcGetComplexScale();
     
  unsigned int pindex = 0;
  int deltaYsign = 1;
  int pixel, i, j;
  int pixSize = 1;
  int needData = 0;
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
  b3dInt16 *sdata;
  b3dUInt16 *usdata;
  int imXsize, imYmin, imYmax, ymin, ymax, llfx, llfy, ulfx, ulfy, urfx, urfy;
  int lrfx, lrfy, x0, x1, x2, x3, cury, toggleY, imNx;
  int llx2, llfx2, llfy2, ulfx2, ulfy2, ybase;
  float eps = toShort ? 0.005 / 256. : 0.005;
  int doscale = (offset <= -1.0 || offset >= 1.0 || 
                 slope < 1. - eps || slope > 1. + eps) ? 1 : 0;

  /* Raw bytes need to be treated like a conversion if they need signed->unsigned map */
  if (!type && mapSbytes)
    convert = 1;

  /* printf ("read slope %f  offset %f  doscale %d toshort %d\n", slope, offset, doscale, 
     toShort); */
  if (type == MRSA_FLOAT && sliceModeIfReal(hdata->mode) < 0) {
    b3dError(stderr, "ERROR: mrcReadSectionAny - Only real modes can be read"
             " as floats");
    return 1;
  }
  if ((type == MRSA_BYTE && outmax > 255) || (type == MRSA_USHORT && outmax < 256)) {
    b3dError(stderr, "ERROR: mrcReadSectionAny - outmax (%d) is not in right range for"
             " conversion to %s", outmax, byte ? "bytes" : "shorts");
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
    llx2 = llx < urx ? llx + 1 : llx;
    mrcMirrorSource(imNx, ny, llx, imYmin, &llfx, &llfy);
    mrcMirrorSource(imNx, ny, llx, imYmax, &ulfx, &ulfy);
    mrcMirrorSource(imNx, ny, llx2, imYmin, &llfx2, &llfy2);
    mrcMirrorSource(imNx, ny, llx2, imYmax, &ulfx2, &ulfy2);
    mrcMirrorSource(imNx, ny, urx, imYmin, &lrfx, &lrfy);
    mrcMirrorSource(imNx, ny, urx, imYmax, &urfx, &urfy);
    llx = 0;
    if (urx < imNx / 2)
      llx = lrfx;
    if (llx > imNx / 2)
      llx = llfx;

    /* Get xmax, ymin, ymax from these corners; get y limits and xsize */
    urx = b3dIMax(3, urfx, llfx, llfx2);
    ymin = b3dIMin(6, llfy, ulfy, lrfy, urfy, llfy2, ulfy2);
    ymax = b3dIMax(6, llfy, ulfy, lrfy, urfy, llfy2, ulfy2);
    lly = readY ? li->zmin : ymin;
    ury = readY ? li->zmax : ymax;
    xsize = urx - llx + 1;

    /* Set up flip-flop between two lines if needed when reading in Y */
    toggleY = -1;
    if (readY && ymin < ymax) {
      toggleY = 0;
      cz = ymin;
    }
    /* fprintf(stderr, "llx %d urx %d lly %d ury %d xsize %d ymin %d ymax %d "
            "imYmin %d imYmax "
            "%d\n", llx, urx, lly, ury, xsize, ymin, ymax, imYmin, imYmax);
            fflush(stderr); */
  }

  /* get pixel size based on mode, and prepare scaling and set flags if
     need another data array */
  switch(hdata->mode){
  case MRC_MODE_BYTE:
    pixSize = 1;

    /* Get a scaling map if going to bytes with scaling or if going to shorts, or get a
       signed->unsigned map if signed bytes go to raw or float or unscaled bytes */
    if ((byte && doscale) || toShort)
      map = get_byte_map(slope, offset, outmin, outmax, hdata->bytesSigned);
    else if (mapSbytes)
      map = get_byte_map(1.0, 0., 0, 255, 1);
    needData = (type == MRSA_FLOAT || type == MRSA_USHORT) ? 1 : 0;
    break;

  case MRC_MODE_SHORT:
  case MRC_MODE_USHORT:
    pixSize = 2;
    if ((toShort && (doscale || hdata->mode == MRC_MODE_SHORT)) || byte) {
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
      b3dError(stderr, "ERROR: mrcReadSectionAny - unsupported data type.");
      return 1;
    break;
  }
  usmap = (b3dUInt16 *)map;

  /* If Y is inverted, adjust the Y load, adjust output pointers to last line and set sign
   for incrementing pointers at end of line, in order to fill output array backwards */
  if (hdata->yInverted) {
    if (li->mirrorFFT) {
      b3dError(stderr, "ERROR: mrcReadSectionAny - cannot mirror FFT with inverted Y "
               "data.");
      return 1;
    }
    j = ny - 1 - lly;
    lly = ny - 1 - ury;
    ury = j;
    j = (ury - lly) * xsize;
    bdata += pixSize * j;
    pindex += j;
    bufp += j;
    usbufp += j;
    fbufp += j;
    deltaYsign = -1;
  }

  /* Get the supplemental data array, set all pointers to it */
  if (needData) {
    bdata = (unsigned char *)malloc(pixSize * xsize);
    sdata = (b3dInt16 *)bdata;
    usdata = (b3dUInt16 *)bdata;
    fdata = (b3dFloat *)bdata;
    if (!bdata) {
      if (freeMap)
        free(map);
      b3dError(stderr, "ERROR: mrcReadSectionAny - getting memory for "
               "temporary array.");
      return 2;
    }
  }

  /* 12/1/04: no longer fix header size in case it is weird */

  /* Seek distance at start and end of line */
  seek_line    = llx * pixSize;
  seek_endx = B3DMAX(0, nx - urx - 1);
  seek_skip = 0;
     
  if (readY) {

    /* If reading Y, seek at end of line gets to next section, first seek
       is small and seek to starting Z is big */
    /* b3dFseek(fin, hdata->headerSize + (cz * nx * pixSize),  SEEK_SET);
    if (lly)
    mrc_big_seek(fin, 0, lly, nx * ny * pixSize, SEEK_CUR); */
    seek_endy = ny - 1;
    mrcHugeSeek(fin, hdata->headerSize + hdata->sectionSkip * lly, 0, cz, lly,
                nx, ny, pixSize, SEEK_SET);
    seek_skip = hdata->sectionSkip;
  } else {

    /* If reading X, seek at end of line gets to end of line, first seek
       is in Z and big, seek to starting Y is small */
    /* mrc_big_seek(fin, hdata->headerSize, cz, nx * ny * pixSize,  SEEK_SET);
    if (lly)
    b3dFseek(fin, lly * nx * pixSize, SEEK_CUR); */
    seek_endy = 0;
    mrcHugeSeek(fin, hdata->headerSize + hdata->sectionSkip * cz, 0, lly, cz,
                nx, ny, pixSize, SEEK_SET);
  }

  /* Start loop on Y and read a line of data */
  for (j = lly; j <= ury; j++){
    if (seek_line)
      b3dFseek(fin, seek_line, SEEK_CUR);
    if (b3dFread(bdata, pixSize, xsize, fin) != xsize) {
      b3dError(stderr, "ERROR: mrcReadSectionAny - reading data from file.");
      if (needData)
        free(sdata);
      if (freeMap)
        free(map);
      return 3;
    }

    /* Do data-dependent processing for byte conversions or mappings */
    if (convert) {
      switch(hdata->mode){
      case MRC_MODE_BYTE:
        if (byte || (!toShort && mapSbytes)) {
          if (doscale || mapSbytes)
            for (i = 0; i < xsize; i++)
              bdata[i] = map[bdata[i]];
          bdata += xsize * deltaYsign;
        } else {
          for (i = 0; i < xsize; i++)
            usbufp[i] = usmap[bdata[i]];
          usbufp += xsize * deltaYsign;
        }
        break;
        
      case MRC_MODE_SHORT:
      case MRC_MODE_USHORT:
        if (byte) {
          for (i = 0; i < xsize; i++)
            bufp[i] = map[usdata[i]];
          bufp += xsize * deltaYsign;
        } else {
          if (doscale || hdata->mode == MRC_MODE_SHORT)
            for (i = 0; i < xsize; i++)
              usbufp[i] = usmap[usbufp[i]];
          usbufp += xsize * deltaYsign;
          bdata = (unsigned char *)usbufp;
        }
        break;

      case MRC_MODE_RGB:
        inptr = bdata;
        if (byte) {
          for (i = 0; i < xsize; i++) {
            fpixel = 0.3 * *inptr++;
            fpixel += 0.59 * *inptr++;
            fpixel += 0.11 * *inptr++;
            bufp[i] = (int)(fpixel + 0.5f);
          }
          bufp += xsize * deltaYsign;
        } else {
          for (i = 0; i < xsize; i++) {
            fpixel = 255. * 0.3 * *inptr++;
            fpixel += 255. * 0.59 * *inptr++;
            fpixel += 255. * 0.11 * *inptr++;
            usbufp[i] = (int)(fpixel + 0.5f);
          }
          usbufp += xsize * deltaYsign;
        }
        break;
        
      case MRC_MODE_FLOAT:
        if (hdata->swapped)
          mrc_swap_floats(fdata, xsize);
        if (byte) {

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
          bufp += xsize * deltaYsign;
        } else {
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
          usbufp += xsize * deltaYsign;
        }
        break;

      case MRC_MODE_COMPLEX_FLOAT:
        if (hdata->swapped)
          mrc_swap_floats(fdata, xsize * 2);
        if (li->mirrorFFT) {
          fft = bdata;
          usfft = usdata;
          pindex = 0;
        }
        for (i = 0; i < xsize; i++, pindex++) {
          fpixel = sqrt((double)((fdata[i*2] * fdata[i*2]) + 
                                 (fdata[(i*2)+1] * fdata[(i*2)+1])));
          pixel = log((double)(1.0f + (kscale * fpixel))) * slope + offset;
          pixel = B3DMAX(outmin, pixel);
          pixel = B3DMIN(outmax, pixel);
          if (byte)
            fft[pindex] = pixel;
          else
            usfft[pindex] = pixel;
        }

        /* MIRRORED FFT DATA */

        if (li->mirrorFFT) {

          /* See if data are needed directly - get intersection with image */
          cury = readY ? cz : j;
          ybase = readY ? j- lly : cury - imYmin;
          x0 = nx - 1 + llx;
          x1 = nx - 1 + urx;
          if (cury >= imYmin && cury <= imYmax) {
            if (x1 >= li->xmin && x0 <= li->xmax) {
              x2 = x0 > li->xmin ? x0 : li->xmin;
              x3 = x1 < li->xmax ? x1 : li->xmax;
              if (byte)
                memcpy(&buf[(x2 - li->xmin) + ybase * imXsize], &fft[x2-x0], x3 + 1 - x2);
              else
                memcpy(&usbufp[(x2 - li->xmin) + ybase * imXsize], &usfft[x2-x0], 
                       2 * (x3 + 1 - x2));
            }
            /* fprintf(stderr, "direct cury %d x0 %d x1 %d  ", cury, x0, x1);
               fprintf(stderr, "x2 %d x3 %d\n", x2, x3); */

            /* Is the rightmost pixel needed? */
            if (urx == nx - 1 && li->xmin == 0) {
              if (byte)
                buf[ybase * imXsize] = fft[xsize - 1];
              else
                usbufp[ybase * imXsize] = usfft[xsize - 1];
            }
          }

          /* See if data are needed for mirror image */
          cury = ny - cury;
          x0 = nx - 1 - urx;
          x1 = nx - 1 - llx;
          if (x1 >= li->xmin && x0 <= li->xmax) {
            x2 = x0 > li->xmin ? x0 : li->xmin;
            x3 = x1 < li->xmax ? x1 : li->xmax;
            
            /* Here the first column and middle column need exclusion 
               If x2 becomes > x3, the loop will not be executed */
            if (!x2)
              x2 = 1;
            if (x3 >= nx - 1)
              x3 = nx - 2;
            ybase = readY ? j- lly : cury - imYmin;
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
            ybase = readY ? j- lly : 0;
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
              seek_endy = ymax - ymin - 1;
              j--;
            } else {
              cz = ymin;
              seek_endy = ymin + ny - ymax - 1;
            }
            /* fprintf(stderr, "toggleY %d cz %d seek %d\n",toggleY, cz,
               seek_endline); */
          }
          /* fflush(stderr); */
        } else {
          pindex += xsize * (deltaYsign - 1);
        }
        break;
      }

    } else if (type == MRSA_FLOAT && hdata->mode != MRC_MODE_FLOAT) {

      /* Float conversions */
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
          mrc_swap_shorts(usdata, xsize * pixSize / 2);
        for (i = 0; i < xsize; i++)
          fbufp[i] = usdata[i];
        break;
      }
      fbufp += xsize * deltaYsign;

    } else {

      /* RAW DATA - do some swaps, advance buffer pointer */

      if (hdata->swapped)
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
      bdata += xsize * deltaYsign * pixSize;
    }

    /* End loop on Y */
    if (seek_endx || seek_endy || seek_skip)
      mrcHugeSeek(fin, seek_skip, seek_endx, seek_endy, 0, nx, ny, pixSize, 
                  SEEK_CUR);
  }

  if (needData)
    free(sdata);
  if (freeMap)
    free(map);
  return 0;
}
