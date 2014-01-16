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
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "iimage.h"
#include "parse_params.h"
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
  setStandardExitPrefix(progname);

  if (argc < 2){
    fprintf(stderr, 
            "%s version %s\n", progname, VERSION_NAME);
    imodCopyright();
    mrctaper_help(progname);
    exit(3);
  }

  for (i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      switch (argv[i][1]) {
          
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
      }
    } else
      break;
  }

  if (i < (argc - 2) || i == argc){
    mrctaper_help(progname);
    exit(3);      
  }

  if (ntaper < 1 || ntaper > 127)
    exitError("Taper must be between 1 and 127.");

  if (i < argc - 1)
    fin = iiFOpen(argv[i++], "rb");
  else
    fin = iiFOpen(argv[i++], "rb+");

  if (fin == NULL)
    exitError("Opening %s.", argv[i - 1]);
  if (mrc_head_read(fin, &hdata))
    exitError("Can't Read Input File Header.");

  if (sliceModeIfReal(hdata.mode) < 0)
    exitError("Can operate only on byte, integer and real data.");

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
     
  if (i < argc) {
    fout = iiFOpen(argv[i], "wb");
    if (fout == NULL)
      exitError("Opening %s.", argv[i]);
    hout = hdata;
    hout.fp = fout;

    /* DNM: eliminate extra header info in the output, and mark it as not swapped  */
    mrcInitOutputHeader(&hout);
    hptr = &hout;
    hout.nz = zmax + 1 - zmin;
    hout.mz = hout.nz;
    hout.zlen = hout.nz;
    secofs = zmin;
  } else {
    if (b3dOutputFileType() == IIFILE_TIFF)
      exitError("Cannot write to an existing TIFF file.");
      
    hptr = &hdata;
    fout = fin;
    secofs = 0;
  }
     
  mrc_getdcsize(hdata.mode, &dsize, &csize);

  bsize = hdata.nx * hdata.ny;
  buf = (unsigned char *)malloc(dsize * csize * bsize);
     
  if (!buf)
    exitError("Couldn't get memory for slice.");
  sliceInit(&slice, hdata.nx, hdata.ny, hdata.mode, buf);

  for (i = zmin; i <= zmax; i++) {
    printf("\rDoing section #%4d", i);
    fflush(stdout);
    if (mrc_read_slice(buf, fin, &hdata, i, 'Z'))
      exitError("Reading section %d.", i);
      
    if (sliceTaperAtFill(&slice, ntaper, inside))
      exitError("Can't get memory for taper operation.");
          
    if (mrc_write_slice(buf, fout, hptr, i - secofs, 'Z'))
      exitError("Writing section %d.", i);
  }
  puts("\nDone!");

  mrc_head_label(hptr, "mrctaper: Image tapered down to fill value at edges");

  mrc_head_write(fout, hptr);
  iiFClose(fout);

  return(0);
}
