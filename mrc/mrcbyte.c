/*
 *  mrcbyte.c -- Converts an mrc file to byte format.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
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
#include "b3dutil.h"
#include "parse_params.h"

void mrcbyte_help(char *name)
{
  printf("Usage: %s -[rRblecsxyz] <infile> <outfile>\n", name);
  printf("Options:\n");
  printf("\t-r\tRequest user input for size and contrast info.\n");
  printf("\t-c blk,wht\tContrast scaling with black and white levels\n");
  printf("\t-s min,min\tInitial intensity scaling (min to 0, max to 255)\n");
  printf("\t-R\tReverse contrast in output.\n");
  printf("\t-l\tUse logarithmic ramp.\n");
  printf("\t-e\tUse exponential ramp.\n");
  printf("\t-x min,max\tWrite subset of image in X\n");
  printf("\t-y min,max\tWrite subset of image in Y\n");
  printf("\t-z min,max\tWrite subset of image in Z\n");
  printf("\t-b\tWrite raw byte data, no header\n");
}

int main( int argc, char *argv[] )
{

  int    i = 0, k;
  FILE   *fin, *fout;
  int resize = FALSE;
  int ramptype = MRC_RAMP_LIN;
  MrcHeader hdata, hout;
  IloadInfo  li;
  Islice slice;
  Islice *slicep;
  char line[128];
  int mode = 0;
  int data_only = FALSE;
  int reverse_video = FALSE;
  float xscl, yscl, zscl;
  size_t xysize, ii;
  unsigned char *buf;
  double meansum = 0.;
  char *progname = imodProgName(argv[0]);
  setStandardExitPrefix(progname);

  if (argc < 2){
    printf("%s version %s\n", progname, VERSION_NAME);
    imodCopyright();
    mrcbyte_help(progname);
    exit(3);
  }

  /* Make library error output to stderr go to stdout */
  b3dSetStoreError(-1);

  mrc_init_li(&li, NULL);

  for (i = 1; i < argc; i++)
    if (argv[i][0] == '-')
      switch (argv[i][1]){
          
      case 'b':
        data_only = TRUE;
        break;

      case 'r':
        resize = TRUE;
        break;

      case 'R':
        reverse_video = TRUE;
        break;

      case 'l':
        ramptype = MRC_RAMP_LOG;
        break;

      case 'e':
        ramptype = MRC_RAMP_EXP;
        break;

      case 'c':
        if (argv[i][2] != 0x00)
          sscanf(argv[i], "-c%d%*c%d", &(li.black), &(li.white));
        else
          sscanf(argv[++i], "%d%*c%d", &(li.black), &(li.white));
        break;

        /* DNM 2/15/01: add scaling option to replicate imod, but
           eliminate short and float options */
      case 's':
        if (argv[i][2] != 0x00)
          sscanf(argv[i], "-s%f%*c%f", &(li.smin), &(li.smax));
        else
          sscanf(argv[++i], "%f%*c%f", &(li.smin), &(li.smax));
        break;

      case 'x':
        if (argv[i][2] != 0x00)
          sscanf(argv[i], "-x%d%*c%d", &(li.xmin), &(li.xmax));
        else
          sscanf(argv[++i], "%d%*c%d", &(li.xmin), &(li.xmax));
        break;
            
      case 'y':
        if (argv[i][2] != 0x00)
          sscanf(argv[i], "-y%d%*c%d", &(li.ymin), &(li.ymax));
        else
          sscanf(argv[++i], "%d%*c%d", &(li.ymin), &(li.ymax));
        break;
            
      case 'z':
        if (argv[i][2] != 0x00)
          sscanf(argv[i], "-z%d%*c%d", &(li.zmin), &(li.zmax));
        else
          sscanf(argv[++i], "%d%*c%d", &(li.zmin), &(li.zmax));
        break;

      default:
        printf("ERROR: %s - Illegal option %s\n", progname, argv[i]);
        mrcbyte_help(progname);
        exit(1);
        break;
            
      }else break;

  if (i < (argc - 1)){
      
    fin = iiFOpen(argv[i], "rb");
    if (fin == NULL)
      exitError("Opening %s.", argv[i]);
 
    i++;
    if (!getenv("IMOD_NO_IMAGE_BACKUP") && imodBackupFile(argv[i])) {
      printf("WARNING: %s - Error making backup file from existing %s\n",
             progname, argv[i]);
    }
    fout = iiFOpen(argv[i], "wb");
    if (fout == NULL)
      exitError("Opening %s.", argv[i]);
  }else{
    mrcbyte_help(progname);
    exit(3);     
  }
     

  if (mrc_head_read(fin, &hdata)) 
    exitError("Reading input file header.");
     
  if (resize) {
    printf("Defaults for %s size = ( %d x %d x %d):\n", 
           argv[i - 1], hdata.nx, hdata.ny, hdata.nz);
    printf("x = ( 0, %d), y = ( 0, %d), z = ( 0, %d), c = ( 0, 255)\n",
           hdata.nx - 1, hdata.ny - 1, hdata.nz - 1);
    resize = get_loadinfo(&hdata, &li);
    printf (" Enter (black level, white level) >");
    fgetline(stdin, line, 127);
    sscanf(line, "%d%*c%d\n", &(li.black), &(li.white));
    printf("\n");
    if ( (!li.black) & (!li.white))
      li.white = 255;     
  }

  li.ramp = ramptype;
  if (ramptype != MRC_RAMP_LIN && hdata.mode != MRC_MODE_SHORT &&
      hdata.mode != MRC_MODE_USHORT && hdata.mode != MRC_MODE_FLOAT)
    exitError("Nonlinear scaling can be used only with integer and float data");

  /* This takes care of fixing the mins and maxes */
  mrc_init_li(&li, &hdata);

  if (!hdata.amin && !hdata.amax) {
    printf("Input file has no min and max; scanning file...");
    hdata.amin = 1.e37;
    hdata.amax = -hdata.amin;
    for (k = 0; k < hdata.nz; k++) {
      slicep = sliceReadMRC(&hdata, k, 'z');
      if (!slicep)
        exitError("Reading slice %d while scanning for min/max", k);
      sliceMMM(slicep);
      hdata.amin = B3DMIN(hdata.amin, slicep->min);
      hdata.amax = B3DMAX(hdata.amax, slicep->max);
      sliceFree(slicep);
      printf(" min %.5g max %.5g\n", hdata.amin, hdata.amax);
    }
  }

  /* This sets the scaling */
  mrcContrastScaling(&hdata, li.smin, li.smax, li.black, li.white, li.ramp, 
                     &li.slope, &li.offset);

  hout = hdata;
  hout.amin = 0.;
  hout.amax = 255.;
  hout.mode = mode;

  /* DNM: eliminate extra header info in the output, and mark it as not
     swapped now that we're done reading data */
  mrcInitOutputHeader(&hout);
  hout.nx = li.xmax + 1 - li.xmin;
  hout.ny = li.ymax + 1 - li.ymin;
  hout.nz = li.zmax + 1 - li.zmin;
  mrc_get_scale(&hdata, &xscl, &yscl, &zscl);
  mrc_set_scale(&hout, (double)xscl, (double)yscl, (double)zscl);

  mrc_head_label(&hout, "mrcbyte: Converted and scaled to byte mode.");

  xysize = (size_t)hout.nx * (size_t)hout.ny;
  buf = (unsigned char *)malloc(xysize);
  if (!buf)
    exitError("Allocating memory to read data into");

  if (!data_only && mrc_head_write(fout, &hout))
    exitError("Writing header to output file");

  for (k = 0; k < hout.nz; k++) {
    printf("Converting Image # %3d\r", k + li.zmin);
    fflush(stdout);
    if (mrcReadZByte(&hdata, &li, buf, k + li.zmin))
      exitError("Reading section %d from file", k+li.zmin);
    if (reverse_video){
      for (ii = 0; ii < xysize; ii++)
        buf[ii] = (unsigned char)(255 - (int)buf[ii]);
    }
    
    sliceInit(&slice, hout.nx, hout.ny, SLICE_MODE_BYTE, buf);
    sliceMMM(&slice);
    if (!k) {
      hout.amin = slice.min;
      hout.amax = slice.max;
    } else {
      hout.amin = B3DMIN(hout.amin, slice.min);
      hout.amax = B3DMAX(hout.amax, slice.max);
    }
    meansum += slice.mean;
    if (mrc_write_slice(buf, fout, &hout, k, 'Z'))
      exitError("Writing section %d to file", k);
      
  }
  hout.amean = meansum / hout.nz;

  if (!data_only && mrc_head_write(fout, &hout))
    exitError("Writing header to output file");
  iiFClose(fout);
  printf("\nDone\n");

  exit(0);
}
