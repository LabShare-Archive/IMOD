/*
 *  mrc2tif.c -- Convert mrc files to TIFF files.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$

$Log$
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

#include <stdio.h>
#include <stdlib.h>
#include "mrcfiles.h"
#include "b3dtiff.h"
#include "b3dutil.h"

int main(int argc, char *argv[])
{
  FILE *fin;
  struct MRCheader hdata;

  int xsize, ysize, zsize, psize, xysize;
  int i, x, y, z;

  char iname[255];
  unsigned char *buf;
  char *progname = imodProgName(argv[0]);

  if (argc != 3){
    fprintf(stderr, "%s version %s \n", progname, VERSION_NAME);
    imodCopyright();
    fprintf(stderr, "%s [mrc file] [tiff name/root]\n\n", progname);
    fprintf(stderr, "A series of tiff files will be created "
            "with the prefix [tiff root name]\n"
            "and with the suffex nnn.tif, "
            "where nnn is the z number.\n");
    exit(1);
  }

  if (NULL == (fin = fopen(argv[1], "rb"))){
    fprintf(stderr, "ERROR: %s - Couldn't open %s\n", progname, argv[1]);
    exit(3);
  }

  if (mrc_head_read(fin, &hdata)){
    fprintf(stderr, "ERROR: %s - Can't Read Input Header from %s.\n",
            progname,argv[1]);
    exit(3);
  }


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
  default:
    fprintf(stderr, "ERROR: %s - Datatype not supported.\n", progname);
    exit(3);
    break;
  }
  xsize = hdata.nx;
  ysize = hdata.ny;
  zsize = hdata.nz;
  xysize = xsize * ysize;
     
  buf = (unsigned char *)malloc(xsize * ysize * psize);

  if (!buf) {
    fprintf(stderr, "ERROR: %s - Failed to allocate memory for slice\n",
            progname);
    exit(3);
  }

  printf("Writing TIFF images. ");
  for (z = 0; z < zsize; z++){
    FILE *fpTiff;
    int tiferr;

    printf(".");
    fflush(stdout);

    sprintf(iname, "%s.%3.3d.tif", argv[2], z);
    if (zsize == 1)
      sprintf(iname, "%s", argv[2]);

    fpTiff = fopen(iname, "wb");
    if (!fpTiff){
      fprintf(stderr, "ERROR: %s - Opening %s\n", progname, iname);
      perror("mrc2tif system message");
      continue;
    }

    /* DNM: switch to calling a routine that takes care of swapping and
       big seeks */
    mrc_read_slice(buf, fin, &hdata, z, 'Z');

    tiferr = tiff_write_image(fpTiff, xsize, ysize, hdata.mode, buf);
    if (tiferr){
      fprintf(stderr, "\nERROR: %s - error (%d) writing to %s\n",
              progname, tiferr, iname);
      perror("mrc2tif ");
      exit(1);
    }
      
    fclose(fpTiff);
  }
  printf("\r\n");
  fclose(fin);
  free(buf);
  exit(0);
}
