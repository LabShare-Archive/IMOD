/*  
 *  mrctaper.c -- tapers edges of images where they have been filled
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "mrcslice.h"
#include "mrcfiles.h"
#include "b3dutil.h"
#include "sliceproc.h"

#define DEFAULT_TAPER 16

void mrctaper_help(char *name)
{
  fprintf(stderr,"Usage: %s [-i] [-t #] [-z min,max] input_file [output_file]"
          "\n", name);
  puts("Options:");
  puts("\t-i\tTaper inside (default is outside).");
  printf("\t-t #\tTaper over the given # of pixels (default %d or 1%% of size)"
         ".\n", DEFAULT_TAPER);
  puts("\t-z min,max\tDo only sections between min and max.");
  puts("\tWith no output file, images are written back to input file.");
}

int main( int argc, char *argv[] )
{

  int    i = 0;
  FILE   *fin, *fout;
  struct MRCheader hdata, hout;
  struct MRCheader *hptr;
  unsigned char *buf;
  int bsize, csize, dsize;
  int inside = 0;
  int ntaper = DEFAULT_TAPER;
  int taperEntered = 0;
  Islice slice;
  int zmin = -1, zmax = -1;
  int secofs;
  char *progname = imodProgName(argv[0]);

  if (argc < 2){
    fprintf(stderr, 
            "%s version %s\n", progname, VERSION_NAME);
    imodCopyright();
    mrctaper_help(progname);
    exit(3);
  }

  for (i = 1; i < argc; i++)
    if (argv[i][0] == '-')
      switch (argv[i][1]){
          
      case 'i':
        inside = 1;
        break;

      case 't':
        taperEntered = 1;
        if (argv[i][2] != 0x00)
          sscanf(argv[i], "-t%d", &ntaper);
        else
          sscanf(argv[++i], "%d", &ntaper);
        break;

      case 'z':
        if (argv[i][2] != 0x00)
          sscanf(argv[i], "-z%d%*c%d", &zmin, &zmax);
        else
          sscanf(argv[++i], "%d%*c%d", &zmin, &zmax);
        break;

      default:
        printf("ERROR: %s - illegal option\n", progname);
        mrctaper_help(progname);
        exit(1);
        break;
            
      }else break;

     

  if (i < (argc - 2) || i == argc){
    mrctaper_help(progname);
    exit(3);      
  }

  if (ntaper < 1 || ntaper > 127) {
    printf("ERROR: %s - Taper must be between 1 and 127.\n",
            progname);
    exit(3);
  }

  if (i < argc - 1)
    fin = fopen(argv[i++], "rb");
  else
    fin = fopen(argv[i++], "rb+");

  if (fin == NULL){
    printf("ERROR: %s - Opening %s.\n", progname, argv[i - 1]);
    exit(3);
  }
  if (mrc_head_read(fin, &hdata)) {
    printf("ERROR: %s - Can't Read Input File Header.\n", progname);
    exit(3);
  }

  if (sliceModeIfReal(hdata.mode) < 0) {
    printf("ERROR: %s - Can operate only on byte, integer and "
            "real data.\n", progname);
    exit(3);
  }

  if (!taperEntered) {
    ntaper = (hdata.nx + hdata.ny) / 200;
    ntaper = B3DMIN(127, B3DMAX(DEFAULT_TAPER, ntaper));
    printf("Tapering over %d pixels\n", ntaper);
  }
     
  if (zmin == -1 && zmax == -1) {
    zmin = 0;
    zmax = hdata.nz - 1;
  } else {
    if (zmin < 0)
      zmin = 0;
    if (zmax >= hdata.nz)
      zmax = hdata.nz - 1;
  }
     
  if (i < argc){
    fout = fopen(argv[i], "wb");
    if (fout == NULL) {
      printf("ERROR: %s - Opening %s.\n", progname, argv[i]);
      exit(3);
    }
    hout = hdata;
    /* DNM: eliminate extra header info in the output, and mark it as not
       swapped  */
    hout.headerSize = 1024;
    hout.sectionSkip = 0;
    hout.next = 0;
    hout.nint = 0;
    hout.nreal = 0;
    hout.nsymbt = 0;
    hout.swapped = 0;
    hptr = &hout;
    hout.nz = zmax + 1 - zmin;
    hout.mz = hout.nz;
    hout.zlen = hout.nz;
    secofs = zmin;
  }else{
    /* DNM 6/26/02: it it OK now */
    /* if (hdata.swapped) {
       fprintf(stderr, "%s: Cannot write to byte-swapped file.\n", progname);
       exit(3);
       } */
    hptr = &hdata;
    fout = fin;
    secofs = 0;
  }
     
  mrc_getdcsize(hdata.mode, &dsize, &csize);

  bsize = hdata.nx * hdata.ny;
  buf = (unsigned char *)malloc(dsize * csize * bsize);
     
  if (!buf){
    printf("ERROR: %s - Couldn't get memory for slice.\n", progname);
    exit(3);
  }
  sliceInit(&slice, hdata.nx, hdata.ny, hdata.mode, buf);

  for (i = zmin; i <= zmax; i++) {
    printf("\rDoing section #%4d", i);
    fflush(stdout);
    if (mrc_read_slice(buf, fin, &hdata, i, 'Z')) {
      printf("\nERROR: %s - Reading section %d.\n", progname, i);
      exit(3);
    }
      
    if (sliceTaperAtFill(&slice, ntaper, inside)) {
      printf("\nERROR: %s - Can't get memory for taper operation.\n",
              progname);
      exit(3);
    }
          
    if (mrc_write_slice(buf, fout, hptr, i - secofs, 'Z')) {
      printf("\nERROR: %s - Writing section %d.\n", progname, i);
      exit(3);
    }
  }
  puts("\nDone!");

  mrc_head_label(hptr, "mrctaper: Image tapered down to fill value at "
                 "edges");

  mrc_head_write(fout, hptr);

  return(0);
}


/*
$Log$
Revision 3.9  2007/06/13 17:13:42  sueh
bug# 1019 In main, setting hout.sectionSkip to 0.

Revision 3.8  2007/05/27 20:22:36  mast
Added taper routine to library

Revision 3.7  2007/02/04 21:12:20  mast
Function name changes from mrcslice cleanup

Revision 3.6  2005/11/11 21:55:53  mast
Allow unsigned data and standardize ERROR messages

Revision 3.5  2005/02/11 01:42:34  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.4  2004/07/07 19:25:30  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.3  2003/10/24 02:28:42  mast
strip directory from program name and/or use routine to make backup file

Revision 3.2  2002/11/05 23:52:15  mast
Changed to call imodCopyright, fixed bug in outputting usage

Revision 3.1  2002/06/26 16:50:03  mast
Allowed writing back to byte-swapped files

*/
