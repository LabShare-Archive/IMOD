/*  IMOD VERSION 2.42
 *
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
/*  $Author$

$Date$

$Revision$

Log at end of file */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mrcfiles.h"

static int mrcReadSectionAny(struct MRCheader *hdata, struct LoadInfo *li,
                             unsigned char *buf, int cz, int readY, int byte);

/*
 * Routines for accessing particular kinds of data as bytes or raw, Y or Z
 * axis
 */
int mrcReadZByte(struct MRCheader *hdata, struct LoadInfo *li,
              unsigned char *buf, int z)
{
  return (mrcReadSectionAny(hdata, li, buf, z, 0, 1));
}
int mrcReadYByte(struct MRCheader *hdata, struct LoadInfo *li,
              unsigned char *buf, int z)
{
  return (mrcReadSectionAny(hdata, li, buf, z, 1, 1));
}

int mrcReadZ(struct MRCheader *hdata, struct LoadInfo *li,
              unsigned char *buf, int z)
{
  return (mrcReadSectionAny(hdata, li, buf, z, 0, 0));
}
int mrcReadY(struct MRCheader *hdata, struct LoadInfo *li,
              unsigned char *buf, int z)
{
  return (mrcReadSectionAny(hdata, li, buf, z, 1, 0));
}

int mrcReadSection(struct MRCheader *hdata, struct LoadInfo *li,
                    unsigned char *buf, int z)
{
  int readY = (li->axis == 2) ? 1 : 0;
  return (mrcReadSectionAny(hdata, li, buf, z, readY, 0));
}

int mrcReadSectionByte(struct MRCheader *hdata, struct LoadInfo *li,
                    unsigned char *buf, int z)
{
  int readY = (li->axis == 2) ? 1 : 0;
  return (mrcReadSectionAny(hdata, li, buf, z, readY, 1));
}

/*
 * Generic routine for reading the data line-by-line and scaling it properly
 */
static int mrcReadSectionAny(struct MRCheader *hdata, struct LoadInfo *li,
                              unsigned char *buf, int cz, int readY, int byte)
{
  FILE *fin = hdata->fp;
  int  nx = hdata->nx;
  int  ny = hdata->ny;
     
  int  llx = li->xmin;
  int  urx = li->xmax;
  int  lly = readY ? li->zmin : li->ymin;
  int  ury = readY ? li->zmax : li->ymax;
  int xsize = urx - llx + 1;
     
  float slope  = li->slope;
  float offset = li->offset;
  int   outmin = li->outmin;
  int   outmax = li->outmax;
  int seek_line, seek_endline;
  float kscale = mrcGetComplexScale();
     
  int pindex = 0;
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
    b3dMax("iiii", urfx, llfx, llfx2, &urx);
    b3dMin("iiiiiii", llfy, ulfy, lrfy, urfy, llfy2, ulfy2, &ymin);
    b3dMax("iiiiiii", llfy, ulfy, lrfy, urfy, llfy2, ulfy2, &ymax);
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
    break;

  case MRC_MODE_SHORT:
    pixSize = 2;
    if (byte) {
      map= get_short_map(slope, offset, outmin, outmax, MRC_RAMP_LIN,
                         hdata->swapped, 1);
      freeMap = 1;
      if (!map)
        return 2;
      needData = 1;
    }
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

  /* Seek distance at start of line */
  seek_line    = llx * pixSize;
     
  if (readY) {

    /* If reading Y, seek at end of line gets to next section, first seek
       is small and seek to starting Z is big */
    seek_endline = pixSize * (nx - urx - 1 + nx * ny - nx);
    b3dFseek(fin, hdata->headerSize + (cz * nx * pixSize),  SEEK_SET);
    if (lly)
      mrc_big_seek(fin, 0, lly, nx * ny * pixSize, SEEK_CUR);

  } else {

    /* If reading X, seek at end of line gets to end of line, first seek
       is in Z and big, seek to starting Y is small */
    seek_endline = pixSize * (nx - urx - 1);
    mrc_big_seek(fin, hdata->headerSize, cz, nx * ny * pixSize,  SEEK_SET);
    if (lly)
      b3dFseek(fin, lly * nx * pixSize, SEEK_CUR);
  }
  if (seek_endline < 0)
    seek_endline = 0;

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
        for (i = 0; i < xsize; i++, pindex++)
          buf[pindex] = map[usdata[i]];
        break;

      case MRC_MODE_RGB:
        inptr = bdata;
        for (i = 0; i < xsize; i++, pindex++) {
          pixel = *inptr++;
          pixel += *inptr++;
          pixel += *inptr++;
          buf[pindex] = pixel / 3;
        }
        break;
        
      case MRC_MODE_FLOAT:
        if (hdata->swapped)
          mrc_swap_floats(fdata, xsize);
        for (i = 0; i < xsize; i++, pindex++){
          pixel =  fdata[i] * slope + offset;
          if (pixel < outmin)
            pixel = outmin;
          if (pixel > outmax)
            pixel = outmax;
          buf[pindex] = pixel;
        }
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
              seek_endline = pixSize * (nx * (ymax - ymin) - urx - 1);
              j--;
            } else {
              cz = ymin;
              seek_endline = pixSize * (nx * (ymin + ny - ymax) - urx - 1);
            }
            /* fprintf(stderr, "toggleY %d cz %d seek %d\n",toggleY, cz,
               seek_endline); */
          }
          /* fflush(stderr); */
        }        
        break;
      }

    } else {

      /* RAW DATA - do some swaps, advance buffer pointer */

      if (hdata->swapped)
        switch(hdata->mode) {
        case MRC_MODE_SHORT:
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
    if (seek_endline)
      b3dFseek(fin, seek_endline, SEEK_CUR);
  }

  if (needData)
    free(sdata);
  if (freeMap)
    free(map);
  return 0;
}

/*
$Log$
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
