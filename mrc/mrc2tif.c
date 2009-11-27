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
  int xsize, ysize, zsize, psize, xysize;
  int i, slmode, z, iarg = 1, stack = 0;
  int oldcode = 0;
  int compression = 1;
  b3dUInt32 ifdOffset = 0, dataOffset = 0;
  float dmin, dmax;
  char prefix[100];
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

      default:
        break;
      }
      
    }
    else
      break;
       
  }

  if (oldcode && compression != 1)
    exitError("Compression not available with old writing code");


  if (argc - iarg != 2){
    printf("%s version %s \n", progname, VERSION_NAME);
    imodCopyright();
    printf("%s [options] <mrc file> <tiff name/root>\n\n", progname);
    printf(" Without -s, a series of tiff files will be created "
            "with the\n prefix [tiff root name] and with the suffix nnn.tif, "
           "where nnn is the z number. \n  Options:\n");
    printf("    -s      Stack all images in the mrc file into a single tiff "
           "file\n");
    printf("    -c val  Compress data; val can be lzw, zip, jpeg, or numbers "
           "defined\n\t\t in libtiff\n");
    printf("    -o      Write file with old IMOD code instead of libtiff\n");
    exit(1);
  }

  if (NULL == (fin = fopen(argv[iarg], "rb")))
    exitError("Couldn't open %s", argv[iarg]);

  if (mrc_head_read(fin, &hdata))
    exitError("Can't Read Input Header from %s", argv[iarg]);
  iarg++;
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
  xysize = xsize * ysize;
  dmin = hdata.amin;
  dmax = hdata.amax;
     
  buf = (unsigned char *)malloc(xsize * ysize * psize);
  iname = (char *)malloc(strlen(argv[iarg]) + 20);

  if (!buf || !iname)
    exitError("Failed to allocate memory for slice");

  if (stack) {
    sprintf(iname, "%s", argv[iarg]);
    fpTiff = openEitherWay(iifile, iname, progname, oldcode);
  }    

  printf("Writing TIFF images. ");
  for (z = 0; z < zsize; z++){
    int tiferr;

    printf(".");
    fflush(stdout);

    if (!stack) {
      sprintf(iname, "%s.%3.3d.tif", argv[iarg], z);
      if (zsize == 1)
        sprintf(iname, "%s", argv[iarg]);
      if (z)
        free(iifile->filename);
      fpTiff = openEitherWay(iifile, iname, progname, oldcode);
      ifdOffset = 0;
      dataOffset = 0;
    }

    /* DNM: switch to calling a routine that takes care of swapping and
       big seeks */
    if (mrc_read_slice(buf, fin, &hdata, z, 'Z')) {
      perror("mrc2tif ");
      exitError("Reading section %d", z);
    }

    /* Get min/max for int/float images, those are the only ones the write
       routine will put out min/max for */
    slmode = sliceModeIfReal(hdata.mode);
    if (!stack && slmode > 0) {
      sliceInit(&slice, xsize, ysize, slmode, buf);
      sliceMMM(&slice);
      iifile->amin = slice.min;
      iifile->amax = slice.max;
    }
    iifile->nx = xsize;
    iifile->ny = ysize;
    if (fpTiff)
      tiferr = tiff_write_image(fpTiff, xsize, ysize, hdata.mode, buf, 
                                &ifdOffset, &dataOffset, dmin, dmax);
    else
      tiferr = tiffWriteSection(iifile, buf, compression, 0);
    if (tiferr){
      perror("mrc2tif ");
      exitError("Error (%d) writing section %d to %s", tiferr, z, iname);
    }
    
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
  free(buf);
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

