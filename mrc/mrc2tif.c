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

int main(int argc, char *argv[])
{
  FILE *fin;
  MrcHeader hdata;
  FILE *fpTiff;
  Islice slice;
  int xsize, ysize, zsize, psize, xysize;
  int i, slmode, z, iarg = 1, stack = 0;
  b3dUInt32 ifdOffset = 0, dataOffset = 0;
  float dmin, dmax;

  char *iname;
  unsigned char *buf;
  char *progname = imodProgName(argv[0]);

  if (argc > 1 && !strcmp(argv[1], "-s")) {
    iarg++;
    stack = 1;
  }
  if (argc - iarg != 2){
    printf("%s version %s \n", progname, VERSION_NAME);
    imodCopyright();
    printf("%s [-s] <mrc file> <tiff name/root>\n\n", progname);
    printf(" Without -s, a series of tiff files will be created "
            "with the\n prefix [tiff root name] and with the suffix nnn.tif, "
            "where nnn is the z number.\n With -s, all images in the mrc "
            "file will be stacked into a single tiff file.\n");
    exit(1);
  }

  if (NULL == (fin = fopen(argv[iarg], "rb"))){
    printf("ERROR: %s - Couldn't open %s\n", progname, argv[iarg]);
    exit(3);
  }

  if (mrc_head_read(fin, &hdata)){
    printf("ERROR: %s - Can't Read Input Header from %s.\n",
            progname,argv[iarg]);
    exit(3);
  }
  iarg++;

  switch(hdata.mode){
      
  case MRC_MODE_BYTE:
    psize = 1;
    break;
  case MRC_MODE_SHORT:
    psize = 2;
    break;
  case MRC_MODE_USHORT:
    psize = 2;
    break;
  case MRC_MODE_RGB:
    psize = 3;
    break;
  case MRC_MODE_FLOAT:
    psize = 4;
    break;
  default:
    printf("ERROR: %s - Datatype not supported.\n", progname);
    exit(3);
    break;
  }
  xsize = hdata.nx;
  ysize = hdata.ny;
  zsize = hdata.nz;
  xysize = xsize * ysize;
  dmin = hdata.amin;
  dmax = hdata.amax;
     
  buf = (unsigned char *)malloc(xsize * ysize * psize);
  iname = (char *)malloc(strlen(argv[iarg] + 20));

  if (!buf || !iname) {
    printf("ERROR: %s - Failed to allocate memory for slice\n",
            progname);
    exit(3);
  }

  if (stack) {
    sprintf(iname, "%s", argv[iarg]);
    fpTiff = fopen(iname, "wb");
    if (!fpTiff){
      printf("ERROR: %s - Opening %s\n", progname, iname);
      perror("mrc2tif system message");
    }
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

      fpTiff = fopen(iname, "wb");
      if (!fpTiff){
        printf("\nERROR: %s - Opening %s\n", progname, iname);
        perror("mrc2tif system message");
        exit(1);
      }
      ifdOffset = 0;
      dataOffset = 0;
    }

    /* DNM: switch to calling a routine that takes care of swapping and
       big seeks */
    if (mrc_read_slice(buf, fin, &hdata, z, 'Z')) {
      printf("\nERROR: %s - reading section %d\n", progname, z);
      perror("mrc2tif ");
      exit(1);
    }

    /* Get min/max for int/float images, those are the only ones the write
       routine will put out min/max for */
    slmode = sliceModeIfReal(hdata.mode);
    if (!stack && slmode > 0) {
      sliceInit(&slice, xsize, ysize, slmode, buf);
      sliceMMM(&slice);
      dmin = slice.min;
      dmax = slice.max;
    }

    tiferr = tiff_write_image(fpTiff, xsize, ysize, hdata.mode, buf, 
                              &ifdOffset, &dataOffset, dmin, dmax);
    if (tiferr){
      printf("\nERROR: %s - error (%d) writing section %d to %s\n",
              progname, tiferr, z, iname);
      perror("mrc2tif ");
      exit(1);
    }
    
    if (!stack)
      fclose(fpTiff);
  }

  printf("\r\n");
  if (stack)
    fclose(fpTiff);
  
  fclose(fin);
  free(buf);
  exit(0);
}

/*

$Log$
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

