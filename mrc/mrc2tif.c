/*
 *  mrc2tif.c -- Convert mrc files to TIFF files.
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
#include <stdlib.h>
#include <string.h>
#include "mrcfiles.h"
#include "b3dtiff.h"
#include "b3dutil.h"
#include "parse_params.h"
static FILE *openEitherWay(ImodImageFile *iifile, char *iname, char *progname,
                           int oldcode);

int main(int argc, char *argv[])
{
  FILE *fin;
  MrcHeader hdata;
  FILE *fpTiff = NULL;
  Islice slice;
  int xsize, ysize, zsize, psize, filenum, outPsize, numChunks, chunk;
  int lines, linesDone, version, linesPerChunk, doChunks = 0;
  size_t xysize, allocSize;
  int i, j, slmode, z, iarg = 1, stack = 0, resolution = 0, initialNum = -1;
  int oldcode = 0, convert = 0, usePixel = 0;
  int compression = 1, black = 0, white = 255, zmin = -1, zmax = -1;
  int quality = -1;
  b3dUInt32 ifdOffset = 0, dataOffset = 0;
  float chunkCriterion = 100.;
  float savecrit, dmin, dmax, smin =0., smax = 0.;
  float val[3];
  float scale, offset, xscale, yscale, zscale;
  char prefix[100];
  IloadInfo li;
  ImodImageFile *iifile = iiNew();
  char *iname;
  unsigned char *buf;
  char *progname = imodProgName(argv[0]);
#define NUM_LEGAL 12
  int legalComp[NUM_LEGAL] = {1, 2, 3, 4, 5, 7, 8, 32771, 32773, 32946, 32845,
                              32845};

  sprintf(prefix, "\nERROR: %s - ", progname);
  setExitPrefix(prefix);

  for (iarg = 1; iarg < argc - 1 ; iarg++){
    if (argv[iarg][0] == '-'){
      switch (argv[iarg][1]){
      case 's':
        stack = 1;
        break;

      case 'o':
        oldcode = 1;
        break;

      case 'c':
        iarg++;
        if (!strcmp(argv[iarg], "zip"))
          compression = 8;
        else if (!strcmp(argv[iarg], "jpeg"))
          compression = 7;
        else if (!strcmp(argv[iarg], "lzw"))
          compression = 5;
        else
          compression = atoi(argv[iarg]);
        for (i = 0; i < NUM_LEGAL; i++)
          if (compression == legalComp[i])
            break;
        if (i == NUM_LEGAL)
          exitError("Compression value %d not allowed", compression);
        break;

      case 'r':
        resolution = atoi(argv[++iarg]);
        break;

      case 'P':
        usePixel = 1;
        break;

      case 'S':
        sscanf(argv[++iarg], "%f%*c%f", &smin, &smax);
        convert = 1;
        break;

      case 'C':
        sscanf(argv[++iarg], "%d%*c%d", &black, &white);
        convert = 1;
        break;

      case 'z':
        sscanf(argv[++iarg], "%d%*c%d", &zmin, &zmax);
        break;

      case 'i':
        initialNum = atoi(argv[++iarg]);
        break;

      case 'q':
        quality = atoi(argv[++iarg]);
        break;

      case 't':
        chunkCriterion = atof(argv[++iarg]);
        break;

      default:
        break;
      }
      
    }
    else
      break;
       
  }

  if (oldcode && compression != 1)
    exitError("Compression not available with old writing code");
  if (oldcode && (resolution > 0 || usePixel))
    exitError("Resolution setting is not available with old writing code");
  if (resolution > 0 && usePixel)
    exitError("You cannot enter both -r and -P");

  if (argc - iarg != 2){
    printf("%s version %s \n", progname, VERSION_NAME);
    imodCopyright();
    printf("%s [options] <mrc file> <tiff name/root>\n\n", progname);
    printf(" Without -s, a series of tiff files will be created "
            "with the\n prefix [tiff root name] and with the suffix nnn.tif, "
           "where nnn is the z number. \n  Options:\n");
    printf("    -s         Stack all images in the mrc file into a single tiff"
           " file\n");
    printf("    -c val     Compress data; val can be lzw, zip, jpeg, or "
           "numbers defined\n\t\t in libtiff\n");
    printf("    -q #       Quality for jpeg compression (0-100) or for zip "
           "compression (1-9)");
    printf("    -S min,max Initial scaling limits for conversion to bytes\n");
    printf("    -C b,w     Contrast black/white values for conversion to "
           "bytes\n");
    printf("    -z min,max Starting and ending Z (from 0) to output\n");
    printf("    -i #       Initial file number (default is starting Z)\n");
    printf("    -r #       Resolution setting in dots per inch\n");
    printf("    -P         Use pixel spacing in MRC header for resolution setting\n");
    printf("    -t #       Criterion image size in megabytes for processing "
           "file in strips\n");
    printf("    -o         Write file with old IMOD code instead of libtiff"
           "\n");
    exit(1);
  }

  if (NULL == (fin = fopen(argv[iarg], "rb")))
    exitError("Couldn't open %s", argv[iarg]);

  if (mrc_head_read(fin, &hdata))
    exitError("Can't Read Input Header from %s", argv[iarg]);
  iarg++;
  if (usePixel) {
    mrc_get_scale(&hdata, &xscale, &yscale, &zscale);
    resolution = 2.54e8 / xscale;
  }

  if (zmin == -1 && zmax == -1) {
    zmin = 0;
    zmax = hdata.nz - 1;
  } else if (zmin < 0 || zmax >= hdata.nz || zmin > zmax)
    exitError("zmin,zmax values are reversed or out of the range 0 to %d\n",
              hdata.nz - 1);
  if (initialNum < 0)
    filenum = zmin;
  else
    filenum = initialNum;

  iifile->format = IIFORMAT_LUMINANCE;
  iifile->file = IIFILE_TIFF;
  iifile->type = IITYPE_UBYTE;
  iifile->amin = iifile->amax = 0;

  switch(hdata.mode){
      
  case MRC_MODE_BYTE:
    psize = 1;
    break;
  case MRC_MODE_SHORT:
    psize = 2;
    iifile->type = IITYPE_SHORT;
    break;
  case MRC_MODE_USHORT:
    psize = 2;
    iifile->type = IITYPE_USHORT;
    break;
  case MRC_MODE_RGB:
    psize = 3;
    iifile->format = IIFORMAT_RGB;
    break;
  case MRC_MODE_FLOAT:
    psize = 4;
    iifile->type = IITYPE_FLOAT;
    break;
  default:
    exitError("Data mode %d not supported.", hdata.mode);
    break;
  }
  xsize = hdata.nx;
  ysize = hdata.ny;
  zsize = hdata.nz;
  xysize = (size_t)xsize * (size_t)ysize;
  allocSize = xysize * psize;
  iifile->nx = xsize;
  iifile->ny = ysize;
  iifile->nz = 1;
  li.xmin = 0;
  li.xmax = xsize - 1;
  li.ymin = 0;
  li.ymax = ysize - 1;
  dmin = hdata.amin;
  dmax = hdata.amax;
  slmode = sliceModeIfReal(hdata.mode);
  outPsize = psize;
  if (convert) {
    mrcContrastScaling(&hdata, smin, smax, black, white, MRC_RAMP_LIN, &scale, 
                       &offset);
    if (slmode > 0) {
      iifile->type = IITYPE_UBYTE;
      outPsize = 1;
    }
  }
  version = tiffVersion(&j);
  /* printf("Version %d.%d\n", version,j); */
  if (version < 4) {
    savecrit =  4.292e9;
    if (!version || oldcode)
      savecrit =  2.146e9;
    if ((double)xysize * outPsize >  savecrit)
      exitError("The image is too large in X/Y to save with TIFF version"
                " %d.%d", version, j);
    if (stack && (zmax + 1 - zmin) * xysize * (double)outPsize >  savecrit)
      exitError("The volume is too large to save in a stack with TIFF version"
                " %d.%d", version, j);
  }

  if (version > 0 && !oldcode && 
      ((double)xsize * ysize) * psize > chunkCriterion * 1024. * 1024.)
    doChunks = 1;

  iname = (char *)malloc(strlen(argv[iarg]) + 20);
  if (!iname)
    exitError("Failed to allocate memory for name");

  /* Open new file for stack */
  if (stack) {
    sprintf(iname, "%s", argv[iarg]);
    iifile->nz = zmax + 1 - zmin;
    fpTiff = openEitherWay(iifile, iname, progname, oldcode);
  }    

  printf("Writing TIFF images. ");
  for (z = zmin; z <= zmax; z++){
    int tiferr;
    printf(".");
    fflush(stdout);

    /* Get filename and open new file for non-stack */
    if (!stack) {
      sprintf(iname, "%s.%3.3d.tif", argv[iarg], filenum++);
      if (zsize == 1)
        sprintf(iname, "%s", argv[iarg]);
      if (z > zmin)
        free(iifile->filename);
      fpTiff = openEitherWay(iifile, iname, progname, oldcode);
      ifdOffset = 0;
      dataOffset = 0;
    }
    if (!stack && slmode > 0 && !convert) {
      iifile->amin = 1.e30;
      iifile->amax = -1.e30;
    }

    /* If doing chunks, set up chunk loop */
    numChunks = 1;
    if (doChunks) {
      tiffWriteSetup(iifile, compression, 0, resolution, quality, 
                     &linesPerChunk, &numChunks);
      allocSize = xsize * linesPerChunk * psize;
      linesDone = 0;
    }
    
    for (chunk = 0; chunk < numChunks; chunk++) {

      /* Allocate memory first time or every time */
      if ((!chunk && z == zmin) || (convert && slmode > 0))
        buf = (unsigned char *)malloc(allocSize);
      if (!buf)
        exitError("Failed to allocate memory for slice");

      lines = ysize;
      if (doChunks) {
        lines = B3DMIN(linesPerChunk, ysize - linesDone);
        li.ymin = ysize - (linesDone + lines);
        li.ymax = li.ymin + lines - 1;
        linesDone += lines;
      }

      if (mrcReadZ(&hdata, &li, buf, z)) {
        perror("mrc2tif ");
        exitError("Reading section %d", z);
      }
      sliceInit(&slice, xsize, lines, hdata.mode, buf);
      
      /* Scale the slice and convert it to bytes */
      if (convert) {
        for (j = 0; j < lines; j++) {
          for (i = 0; i < xsize; i++) {
            sliceGetVal(&slice, i, j, val);
            val[0] = val[0] * scale + offset;
            val[0] = B3DMAX(0., B3DMIN(255., val[0]));
            if (psize == 3) {
              val[1] = val[1] * scale + offset;
              val[1] = B3DMAX(0., B3DMIN(255., val[1]));
              val[2] = val[2] * scale + offset;
              val[2] = B3DMAX(0., B3DMIN(255., val[2]));
            }
            slicePutVal(&slice, i, j, val);
          }
        }
        if (slmode > 0 && sliceNewMode(&slice, SLICE_MODE_BYTE))
          exitError("Converting slice %d to bytes", z);
        buf = slice.data.b;
      }
    
      /* Get min/max for int/float images, those are the only ones the write
         routine will put out min/max for */
      if (!stack && slmode > 0 && !convert) {
        sliceMMM(&slice);
        iifile->amin = B3DMIN(iifile->amin, slice.min);
        iifile->amax = B3DMAX(iifile->amax, slice.max);
      }

      if (fpTiff)
        tiferr = tiff_write_image(fpTiff, xsize, ysize, hdata.mode, buf, 
                                  &ifdOffset, &dataOffset, dmin, dmax);
      else if (!doChunks)
        tiferr = tiffWriteSection(iifile, buf, compression, 0, resolution,
                                  quality);
      else
        tiferr = tiffWriteStrip(iifile, chunk, buf);
      if (tiferr){
        exitError("Error (%d) writing section %d to %s", tiferr, z, iname);
      }
      if (convert && slmode > 0)
        free(slice.data.b);
    }
    if (doChunks)
      tiffWriteFinish(iifile);
    
    if (!stack) {
      if (fpTiff) {
        fclose(fpTiff);
        fpTiff = NULL;
      } else
        iiClose(iifile);
    }
  }

  printf("\r\n");
  if (stack) {
    if (fpTiff)
      fclose(fpTiff);
    else
      iiClose(iifile);
  }
  fclose(fin);
  exit(0);
}

static FILE *openEitherWay(ImodImageFile *iifile, char *iname, char *progname,
                           int oldcode)
{
  static int warned = 0;
  FILE *fpTiff = NULL;
  iifile->filename = strdup(iname);
  
  if (oldcode || tiffOpenNew(iifile)) {
    fpTiff = fopen(iname, "wb");
    if (!fpTiff){
      perror("mrc2tif system message");
      exitError("Opening %s", iname);
    } else if (!oldcode && !warned) {
      warned = 1;
      printf("\nWARNING: %s - Not writing with libtiff, compression not "
             "available", progname);
    }
  }
  return fpTiff;
}

/*

$Log$
Revision 3.15  2010/12/18 18:46:20  mast
Now it will work with up to 4 GB with version 3 libtiff, and above 4 GB
for libtiff 4.  Reads files in chunks corresponding to output file strips
above a certain size.

Revision 3.14  2010/12/17 06:20:51  mast
Maybe it will work with > 2 GB of data

Revision 3.13  2010/12/15 06:21:24  mast
Added options for setting resolution, controlling scaling, setting file
number and doing a subset in Z

Revision 3.12  2009/11/27 16:38:09  mast
Added include to fix warnings

Revision 3.11  2009/04/01 00:20:34  mast
Call new writing routine with libtiff and add compression option

Revision 3.10  2008/05/24 14:52:45  mast
Fixed string length allocation

Revision 3.9  2008/05/23 23:03:31  mast
Added string include

Revision 3.8  2008/05/23 22:57:17  mast
Added float support, option for single stack, and standardized errors

Revision 3.7  2006/06/19 19:29:23  mast
Added ability to write unsigned ints from mode 6

Revision 3.6  2006/02/19 17:53:53  mast
Allocate an array only big enough for the data type, test on array 
allocation, and issue standard error messages (but still to stderr)

Revision 3.5  2005/02/11 01:42:34  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.4  2004/11/05 18:53:10  mast
Include local files with quotes, not brackets

Revision 3.3  2004/07/07 19:25:30  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.2  2003/10/24 02:28:42  mast
strip directory from program name and/or use routine to make backup file

Revision 3.1  2002/11/05 23:48:02  mast
Changed to call imodCopyright

*/

