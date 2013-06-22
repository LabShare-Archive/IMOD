/*
 *  dmprops.c - Determine size, data type, and offset to data in 
 *              DigitalMicrograph 3 files
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdio.h>
#include <stdlib.h>
#include "iimage.h"

int main(int argc, char **argv)
{
  int i, dmf;
  FILE *fp;
  RawImageInfo info;
  int  type;

  dmf = atoi(argv[1]);
  if (dmf != 3 && dmf != 4) {
    printf("ERROR: dm3props - First argument must be 3 or 4 for DM format\n");
    exit(1);
  }
  for (i = 2; i < argc; i++){
      
    fp = fopen(argv[i], "rb");
    if (!fp) {
      printf("ERROR: dm3props - Opening %s\n", argv[i]);
      exit(1);
    }

    if (analyzeDM3(fp, argv[i], dmf, &info, &type))
      exit(1);
      
    printf("%d %d %d %d %d %f %f\n", info.nx, info.ny, info.nz, type,
           info.headerSize, info.pixel, info.zPixel);
    fclose(fp);
  }
  exit(0);
}
