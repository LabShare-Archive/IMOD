/*
 *    iimage.c    - specific routines for tiff-type ImodImageFile's
 *
 *    Authors:  James Kremer and David Mastronarde
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 */

/************************************************************************** 
This software uses the tiff library which has the following copyright:
Copyright (c) 1988-1996 Sam Leffler
Copyright (c) 1991-1996 Silicon Graphics, Inc.

Permission to use, copy, modify, distribute, and sell this software and
its documentation for any purpose is hereby granted without fee, provided
that (i) the above copyright notices and this permission notice appear in
all copies of the software and related documentation, and (ii) the names of
Sam Leffler and Silicon Graphics may not be used in any advertising or
publicity relating to the software without the specific, prior written
permission of Sam Leffler and Silicon Graphics.

THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.

IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF
LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
OF THIS SOFTWARE.

Additional documentation is at <ftp://ftp.sgi.com/graphics/tiff/doc>
****************************************************************************/
/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.3  2004/01/21 00:56:50  mast
    Stopped freeing map from byte_map

    Revision 3.2  2004/01/05 17:51:16  mast
    renamed imin/imax to smin/smax or outmin/outmax as appropriate, changed
    unsigned short to b3dUInt16

    Revision 3.1  2003/02/27 17:08:23  mast
    Set default upper coordinates to -1 rather than 0.

*/


#include "imodconfig.h"
#ifdef NOTIFFLIBS
#include "notiffio.h"
#else
#include <tiffio.h>
#endif

#include "iimage.h"

/* DNM 12/24/00: Got this working for bytes, shorts, and RGBs, for whole
   images or subsets, and using maps for scaling */
/* DNM 11/18/01: Added ability to read tiles, made tiffReadSection and 
   tiffReadSectionByte call a common routine to reduce duplicate code */

static int ReadSection(ImodImageFile *inFile, char *buf, int inSection,
                       int byte)
{
  int nstrip, si;
  int xout, xcopy;
  int xsize = inFile->nx;
  int ysize = inFile->ny;
  int row;
  int i, pixel, xstart, xend, ystart, yend, y, ofsin, ofsout;
  int doscale;
  float slope = inFile->slope;
  float offset = inFile->offset;
  int outmin = 0;
  int outmax = 255;
  int stripsize;
  int xmin, xmax, ymin, ymax;
  int pixsize = 1;
  int movesize = 1;
  unsigned char *obuf;
  unsigned char *tmp = NULL;
  unsigned char *bdata;
  b3dUInt16 *usdata;
  unsigned char *map = NULL;
  int freeMap = 0;
  uint32 rowsperstrip;
  int nread;
  int tilesize, tilewidth, tilelength, xtiles, ytiles, xti, yti;
     
  TIFF* tif = (TIFF *)inFile->header;
  if (inFile->axis == 2)
    return -1;
  if (byte && (inFile->format != IIFORMAT_LUMINANCE))
    return -1;
  if (!tif)
    iiReopen(inFile);
  tif = (TIFF *)inFile->header;
  if (!tif)
    return -1;


  /* set the dimensions to read in */
  /* DNM 2/26/03: replace upper right only if negative */
  xmin   = inFile->llx;
  ymin   = inFile->lly;
  if (inFile->urx < 0)
    xmax = inFile->nx-1;
  else
    xmax = inFile->urx;
  if (inFile->ury < 0)
    ymax = inFile->ny-1;
  else
    ymax = inFile->ury;
  xout = xmax + 1 - xmin;
  doscale = (offset <= -1.0 || offset >= 1.0 || 
             slope < 0.995 || slope > 1.005);
     
  TIFFSetDirectory(tif, inSection);   
  if (byte) {
    if (inFile->type == IITYPE_SHORT) {
      pixsize = 2;
      map = get_short_map(slope, offset, outmin, outmax, MRC_RAMP_LIN, 0, 1);
      freeMap = 1;
    } else if (inFile->type == IITYPE_USHORT) {
      pixsize = 2;
      map = get_short_map(slope, offset, outmin, outmax, MRC_RAMP_LIN, 0, 0);
      freeMap = 1;
    } else if (doscale)
      map = get_byte_map(slope, offset, outmin, outmax);
  } else {
    if (inFile->format == IIFORMAT_RGB)
      pixsize = 3;
    else if (inFile->type == IITYPE_SHORT || 
             inFile->type == IITYPE_USHORT)
      pixsize = 2;
    movesize = pixsize;
  }

  if (freeMap && !map)
    return -1;

  if (TIFFGetField(tif, TIFFTAG_ROWSPERSTRIP, &rowsperstrip)) {

    /* if data are in strips, get strip size and memory for it */
    stripsize = TIFFStripSize(tif);
    tmp = (unsigned char *)malloc(stripsize);
    if (!tmp) {
      if (freeMap)
        free(map);
      return -1;
    }
               
    nstrip = TIFFNumberOfStrips(tif);
    /* printf("%d %d %d %d\n", stripsize, rowsperstrip, nstrip, 
       pixsize); */

    for (si = 0 ; si < nstrip; si++){
               
      /* Compute starting and ending Y values to use in each strip */
      ystart = ysize - 1 - (rowsperstrip * (si + 1) - 1);
      yend = ysize - 1 - (rowsperstrip * si);
      if (ymin > ystart)
        ystart = ymin;
      if (ymax < yend)
        yend = ymax;
      if (ystart > yend)
        continue;
               
      /* Read the strip if necessary */
      nread = TIFFReadEncodedStrip(tif, si, tmp, stripsize);
      /* printf("%d %d %d %d\n", nread, si, ystart, yend); */
      for (y = ystart; y <= yend; y++) {

        /* for each y, compute back to row, and get offsets into
           input and output arrays */
        row = ysize - 1 - y - rowsperstrip * si;
        ofsin = movesize * (row * xsize + xmin);
        ofsout = movesize * (y - ymin) * xout;
        obuf = (unsigned char *)buf + ofsout;
        bdata = tmp + ofsin;
        if (byte) {
          if (pixsize == 1) {
                              
            /* Bytes */
            if (doscale)
              for (i = 0; i < xout; i++)
                *obuf++ = map[*bdata++];
            else
              memcpy(obuf, bdata, xout);
          } else {
                              
            /* Integers */
            usdata = (b3dUInt16 *)tmp + ofsin;
            for (i = 0; i < xout; i++)
              *obuf++ = map[*usdata++];
          }
        } else {
          memcpy(obuf, bdata, xout * pixsize);
        }
      }    
    }
  } else {

    /* Otherwise make sure there are tiles, if not return with error */
    if (TIFFGetField(tif, TIFFTAG_TILEWIDTH, &tilewidth)) {
      tilesize = TIFFTileSize(tif);
      tmp = (unsigned char *)malloc(tilesize);
    }
    if (!tmp) {
      if (freeMap)
        free(map);
      return -1;
    }
    TIFFGetField(tif, TIFFTAG_TILELENGTH, &tilelength);
    xtiles = (xsize + tilewidth - 1) / tilewidth;
    ytiles = (ysize + tilelength - 1) / tilelength;
               
    /* printf("%d %d %d %d %d %d\n", tilesize, tilewidth, tilelength, 
       xtiles, ytiles, pixsize); */

    for (yti = 0; yti < ytiles; yti++) {
      for (xti = 0; xti < xtiles; xti++) {
                    
        /* Compute starting and ending Y then X values to use in 
           this tile */
        ystart = ysize - 1 - (tilelength * (yti + 1) - 1);
        yend = ysize - 1 - (tilelength * yti);
        if (ymin > ystart)
          ystart = ymin;
        if (ymax < yend)
          yend = ymax;
        if (ystart > yend)
          continue;

        xstart = xti * tilewidth;
        xend = xstart + tilewidth - 1;
        if (xmin > xstart)
          xstart = xmin;
        if (xmax < xend)
          xend = xmax;
        if (xstart > xend)
          continue;
                    
        /* Read the tile if necessary */
        si = xti + yti * xtiles;
        nread = TIFFReadEncodedTile(tif, si, tmp, tilesize);
        xcopy = xend + 1 - xstart;
        /* printf("%d %d %d %d\n", nread, si, ystart, yend); */
        for (y = ystart; y <= yend; y++) {
                         
          /* for each y, compute back to row, and get offsets 
             into input and output arrays */
          row = ysize - 1 - y - tilelength * yti;
          ofsin = movesize * 
            (row * tilewidth + xstart - xti * tilewidth);
          ofsout = movesize * 
            ((y - ymin) * xout + xstart - xmin);
          obuf = (unsigned char *)buf + ofsout;
          bdata = tmp + ofsin;
          if (byte) {
            if (pixsize == 1) {
                                   
              /* Bytes */
              if (doscale)
                for (i = 0; i < xcopy; i++)
                  *obuf++ = map[*bdata++];
              else
                memcpy(obuf, bdata, xcopy);
            } else {
                                   
              /* Integers */
              usdata = (b3dUInt16 *)tmp + ofsin;
              for (i = 0; i < xcopy; i++)
                *obuf++ = map[*usdata++];
            }
          } else {
            memcpy(obuf, bdata, xcopy * pixsize);
          }
        }
      }
               
    }
  }
  free (tmp);
  if (freeMap)
    free(map);

  return 0;
}

int tiffReadSectionByte(ImodImageFile *inFile, char *buf, int inSection)
{ 
  return(ReadSection(inFile, buf, inSection, 1));
}

int tiffReadSection(ImodImageFile *inFile, char *buf, int inSection)
{
  return(ReadSection(inFile, buf, inSection, 0));
}

int tiffReopen(ImodImageFile *inFile)
{
  TIFF* tif;
  tif = TIFFOpen(inFile->filename, inFile->fmode);
  if (!tif)
    return 1;
  inFile->headerSize = 8;
  inFile->header = (char *)tif;    
  inFile->fp = (FILE *)tif;    
  return 0;
}

void tiffClose(ImodImageFile *inFile)
{
  TIFF* tif = (TIFF *)inFile->header;
  if (tif)
    TIFFClose(tif);
  inFile->header = NULL;
  inFile->fp = NULL;
}

void tiffDelete(ImodImageFile *inFile)
{
  tiffClose(inFile);
}


int iiTIFFCheck(ImodImageFile *inFile)
{
  TIFF* tif;
  FILE *fp;
  b3dUInt16 buf;
  int dirnum = 1;
  uint32 val;
  uint16 bits, samples, photometric, sampleformat;
  int defined;

  if (!inFile) return -1;
  fp = inFile->fp;
  if (!fp) return 1;

  rewind(fp);
  if (fread(&buf, sizeof(b3dUInt16), 1, fp) < 1)
    return(2);
  if ( (buf != 0x4949) && (buf != 0x4d4d))
    return(3);
  if (fread(&buf, sizeof(b3dUInt16), 1, fp) < 1)
    return(4);
  if ((buf != 0x002a) && (buf != 0x2a00))
    return(5);
  fclose(fp);
  tif = TIFFOpen(inFile->filename, inFile->fmode);
  if (!tif){
    return(-1);
  }
    
  TIFFGetField(tif, TIFFTAG_IMAGEWIDTH, &inFile->nx);
  TIFFGetField(tif, TIFFTAG_IMAGELENGTH, &inFile->ny);
  TIFFGetField(tif, TIFFTAG_BITSPERSAMPLE, &bits);
  TIFFGetField(tif, TIFFTAG_PHOTOMETRIC, &photometric);

  /* DNM 11/18/01: field need not be defined, set a default */
  defined = TIFFGetField(tif, TIFFTAG_SAMPLESPERPIXEL, &samples);
  if (!defined)
    samples = 1;
    
  while(TIFFReadDirectory(tif)) dirnum++;
  TIFFSetDirectory(tif, 0);

  if (!((samples == 1 && (bits == 8 || bits ==16)) ||
        (samples == 3 & photometric == 2 && bits == 8))) {
    TIFFClose(tif);
    return(6);
  }

  inFile->nz     = dirnum;
  inFile->file   = IIFILE_TIFF;
  inFile->format = IIFORMAT_LUMINANCE;
  inFile->readSectionByte = tiffReadSectionByte;

  if (bits == 8) {
    inFile->type   = IITYPE_UBYTE;
    inFile->amean  = 128;
    inFile->amax   = 255;
    inFile->smax   = 255;
    inFile->mode   = MRC_MODE_BYTE;
    if (samples == 3) {
      inFile->format = IIFORMAT_RGB;
      inFile->mode   = MRC_MODE_RGB;
      inFile->readSection = tiffReadSection;
      inFile->readSectionByte = NULL;
    }
  } else {
    /* If there is a field specifying signed numbers, set up for signed;
       otherwise set up for unsigned */
    defined = TIFFGetField(tif, TIFFTAG_SAMPLEFORMAT, &sampleformat);
    if (defined && sampleformat == SAMPLEFORMAT_INT) {
      inFile->type   = IITYPE_SHORT;
      inFile->amean  = 0;
      inFile->amin   = -32767;
      inFile->smin   = -32767;
      inFile->amax   = 32767;
      inFile->smax   = 32767;
    } else {
      inFile->type   = IITYPE_USHORT;
      inFile->amean  = 32767;
      inFile->amin   = 0;
      inFile->smin   = 0;
      inFile->amax   = 65535;
      inFile->smax   = 65535;
    }
    inFile->mode   = MRC_MODE_SHORT;
  }

  inFile->headerSize = 8;
  inFile->header = (char *)tif;

  inFile->cleanUp = tiffDelete;
  inFile->reopen = tiffReopen;
  inFile->close = tiffClose;
  return(0);
}


