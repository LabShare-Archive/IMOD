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
 *  Log at end
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "b3dutil.h"
#include "mrcfiles.h"

#define MRSA_BYTE 1
#define MRSA_FLOAT 2

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
int mrcReadSectionByte(MrcHeader *hdata, IloadInfo *li, unsigned char *buf, 
                       int z)
{
  int readY = (li->axis == 2) ? 1 : 0;
  return (mrcReadSectionAny(hdata, li, buf, z, readY, MRSA_BYTE));
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
     
  float slope  = li->slope;
  float offset = li->offset;
  int   outmin = li->outmin;
  int   outmax = li->outmax;
  int seek_line, seek_endx, seek_endy, seek_skip;
  float kscale = mrcGetComplexScale();
     
  unsigned int pindex = 0;
  int pixel, i, j;
  int pixSize = 1;
  int needData = 0;
  b3dFloat fpixel;
  b3dFloat *fdata;
  double returned;
  /* Buffer to read lines into; may be replaced by temporary buffer for 
     reading */
  unsigned char *bdata = buf;

  /* Buffer to place complex data into; may be temporary for mirroring */
  unsigned char *fft = buf;
  unsigned char *map = NULL;

  /* Copies of buffer pointer that can be advanced after each line */
  unsigned char *bufp = buf;
  b3dFloat *fbufp = (b3dFloat *)buf;
  int freeMap = 0;
  unsigned char *inptr;
  b3dInt16 *sdata;
  b3dUInt16 *usdata;
  int imXsize, imYmin, imYmax, ymin, ymax, llfx, llfy, ulfx, ulfy, urfx, urfy;
  int lrfx, lrfy, x0, x1, x2, x3, cury, toggleY, imNx;
  int llx2, llfx2, llfy2, ulfx2, ulfy2, ybase;

  int doscale = (offset <= -1.0 || offset >= 1.0 || 
                 slope < 0.995 || slope > 1.005);
  /* printf ("read slope %f  offset %f\n", slope, offset); */
  if (type == MRSA_FLOAT && sliceModeIfReal(hdata->mode) < 0) {
    b3dError(stderr, "ERROR: mrcReadSectionAny - Only real modes can be read"
             " as floats");
    return 1;
  }

  /* Adjust loading parameters if mirroring an FFT 
     This is hopelessly complex because it replicates the mirrored FFT produced
     by clip, with the extra right column placed on the left and the bottom
     left row a duplicate of the row above (except for first pixel) */
  if (li->mirrorFFT && byte) {
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
    if (byte && doscale) {
      map = get_byte_map(slope, offset, outmin, outmax);
      if (!map)
        return 2;
    }
    needData = type == MRSA_FLOAT ? 1 : 0;
    break;

  case MRC_MODE_SHORT:
  case MRC_MODE_USHORT:
    pixSize = 2;
    if (byte) {
      map= get_short_map(slope, offset, outmin, outmax, li->ramp, 
                         hdata->swapped, 
                         (hdata->mode == MRC_MODE_SHORT) ? 1 : 0);
      freeMap = 1;
      if (!map)
        return 2;
    }
    needData = type;
    break;

  case MRC_MODE_RGB:
    pixSize = 3;
    needData = byte;
    break;

  case MRC_MODE_FLOAT:
    pixSize = 4;
    needData = byte;
    break;

  case MRC_MODE_COMPLEX_SHORT:
    pixSize = 4;
    if (byte)
      return 1;
    break;

  case MRC_MODE_COMPLEX_FLOAT:
    pixSize = 8;
    needData = byte;
    break;

  default:
    if (byte)
      b3dError(stderr, "ERROR: mrcReadSectionAny - unsupported data type.");
      return 1;
    break;
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

    /* Do data-dependent processing for byte conversions */
    if (byte) {
      switch(hdata->mode){
      case MRC_MODE_BYTE:
        if (doscale)
          for (i = 0; i < xsize; i++)
            bdata[i] = map[bdata[i]];
        bdata += xsize;
        break;
        
      case MRC_MODE_SHORT:
      case MRC_MODE_USHORT:
        for (i = 0; i < xsize; i++)
          bufp[i] = map[usdata[i]];
        bufp += xsize;
        break;

      case MRC_MODE_RGB:
        inptr = bdata;
        for (i = 0; i < xsize; i++) {
          fpixel = 0.3 * *inptr++;
          fpixel += 0.59 * *inptr++;
          fpixel += 0.11 * *inptr++;
          bufp[i] = (int)(fpixel + 0.5f);
        }
        bufp += xsize;
        break;
        
      case MRC_MODE_FLOAT:
        if (hdata->swapped)
          mrc_swap_floats(fdata, xsize);
        for (i = 0; i < xsize; i++){
          fpixel =  fdata[i];
          if (li->ramp == MRC_RAMP_LOG)
            fpixel = (float)log((double)fpixel);
          if (li->ramp == MRC_RAMP_EXP)
            fpixel = (float)exp((double)fpixel);
          fpixel = fpixel * slope + offset;
          if (fpixel < outmin)
            fpixel = outmin;
          if (fpixel > outmax)
            fpixel = outmax;
          bufp[i] = fpixel + 0.5f;
        }
        bufp += xsize;
        break;

      case MRC_MODE_COMPLEX_FLOAT:
        if (hdata->swapped)
          mrc_swap_floats(fdata, xsize * 2);
        if (li->mirrorFFT) {
          fft = bdata;
          pindex = 0;
        }
        for (i = 0; i < xsize; i++, pindex++) {
          fpixel = sqrt((double)((fdata[i*2] * fdata[i*2]) + 
                                 (fdata[(i*2)+1] * fdata[(i*2)+1])));
          pixel = log((double)(1.0f + (kscale * fpixel))) * slope + offset;
          if (pixel < outmin)
            pixel = outmin;
          if (pixel > outmax)
            pixel = outmax;
          fft[pindex] = pixel;
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
              memcpy(&buf[(x2 - li->xmin) + ybase * imXsize],
                     &fft[x2 - x0], x3 + 1 - x2);
            }
            /* fprintf(stderr, "direct cury %d x0 %d x1 %d  ", cury, x0, x1);
               fprintf(stderr, "x2 %d x3 %d\n", x2, x3); */

            /* Is the rightmost pixel needed? */
            if (urx == nx - 1 && li->xmin == 0) 
              buf[ybase * imXsize] = fft[xsize - 1];
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
              for (i = x3 - x2; i >= 0; i--)
                buf[i + x2 - li->xmin + ybase * imXsize] = fft[x1 - x2 - i];
            }

            /* Replicate bottom left line if needed */
            ybase = readY ? j- lly : 0;
            if (cury == 1 && imYmin == 0)
              for (i = x3 - x2; i >= 0; i--)
                buf[i + x2 - li->xmin + ybase * imXsize] = fft[x1 - x2 - i];
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
        }        
        break;
      }

    } else if (type == MRSA_FLOAT && hdata->mode != MRC_MODE_FLOAT) {

      /* Float conversions */
      switch(hdata->mode){
      case MRC_MODE_BYTE:
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
      fbufp += xsize;

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
      bdata += xsize * pixSize;
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

/*
$Log$
Revision 3.16  2008/05/31 03:48:52  mast
Fixed the log and exp scaling

Revision 3.15  2008/05/31 03:10:19  mast
Added nonlinear ramps and rounding to allow mrcbyte to use this

Revision 3.14  2008/05/23 23:04:04  mast
Switched to NTSC RGB to gray scaling

Revision 3.13  2007/06/13 22:52:16  mast
Modifications for reading with intersection section skip

Revision 3.12  2006/09/28 21:15:02  mast
Changes to work with > 2Gpix and > 4 Gpix images as much as possible

Revision 3.11  2006/08/04 21:04:03  mast
Add documentation

Revision 3.10  2005/11/11 22:15:23  mast
Changes for unsigned file mode

Revision 3.9  2005/02/11 01:42:33  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.8  2004/12/02 21:53:27  mast
Removed setting of header size to min of 1024 so raw reader can use

Revision 3.7  2004/11/12 15:22:36  mast
Changed to use new min/max functions

Revision 3.6  2004/11/04 17:10:27  mast
libiimod.def

Revision 3.5  2004/01/21 00:57:04  mast
Stopped freeing map from byte_map

Revision 3.4  2004/01/17 20:38:07  mast
Convert to calling b3d I/O routines explicitly

Revision 3.3  2004/01/08 06:42:19  mast
Fixed reading of complex data and got scale factor from mrcfiles routines

Revision 3.2  2004/01/05 17:37:12  mast
Rewrote as a single input routine that branches for processing each line

*/
