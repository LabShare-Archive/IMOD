/*
 *  mrcbyte.c -- Converts an mrc file to byte format.
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
Revision 3.4  2004/07/07 19:25:30  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.3  2003/11/18 19:29:32  mast
changes to call b3dF* functions for 2GB problem on Windows

Revision 3.2  2003/10/24 02:28:42  mast
strip directory from program name and/or use routine to make backup file

Revision 3.1  2002/11/05 23:49:50  mast
Changed to call imodCopyright

*/

#include <stdio.h>
#include <stdlib.h>
#include "mrcfiles.h"
#include "b3dutil.h"

void mrcbyte_help(char *name)
{
  fprintf(stderr,"Usage: %s -[rRblecsxyz] <infile> <outfile>\n",
          name);
  puts("Options:");
  puts("\t-r\tRequest user input for size and contrast info.");
  puts("\t-c blk,wht\tContrast scaling with black and white levels");
  puts("\t-s min,min\tInitial intensity scaling (min to 0, max to 255)");
  puts("\t-R\tReverse contrast in output.");
  puts("\t-l\tUse logarithmic ramp.");
  puts("\t-e\tUse exponential ramp.");
  puts("\t-x min,max\tWrite subset of image in X");
  puts("\t-y min,max\tWrite subset of image in Y");
  puts("\t-z min,max\tWrite subset of image in Z");
  puts("\t-b\tWrite raw byte data, no header");
}

int main( int argc, char *argv[] )
{

  int    i = 0, k;
  FILE   *fin, *fout;
  int resize = FALSE;
  int cmap   = FALSE;
  int ramptype = MRC_RAMP_LIN;
  struct MRCheader hdata;
  struct LoadInfo  li;
  float  vd1, vd2, nd1, nd2;
  unsigned char **idata;
  char line[128];
  int mode = 0;
  int xysize;
  int data_only = FALSE;
  int reverse_video = FALSE;
  int c;
  unsigned char bdata;
  short sdata;
  float fdata;
  int cdat;
  char *progname = imodProgName(argv[0]);


  if (argc < 2){
    fprintf(stderr, 
            "%s version %s\n", progname, VERSION_NAME);
    imodCopyright();
    mrcbyte_help(progname);
    exit(3);
  }

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
        fprintf(stderr, "%s: illegal option\n", progname);
        mrcbyte_help(progname);
        exit(1);
        break;
            
      }else break;

  if (i < (argc - 1)){
      

    fin = fopen(argv[i], "rb");
    if (fin == NULL)
      {
        fprintf(stderr, "Error opening %s.\n", argv[i]);
        exit(3);
      }

    i++;
    fout = fopen(argv[i], "wb");
    if (fout == NULL)
      {
        fprintf(stderr, "Error opening %s.\n", argv[i]);
        exit(3);
      }
  }else{
    mrcbyte_help(progname);
    exit(3);     
  }
     

  if (mrc_head_read(fin, &hdata))
    {
      fprintf(stderr, "Can't Read Input File Header.\n");
      exit(3);
    }
     
  if (resize){
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

  mrc_init_li(&li, &hdata);

  /* DNM 2/15/01: pass smin and smax - set to header values if zero */
  if (li.smin == li.smax){
    li.smin = hdata.amin;
    li.smax = hdata.amax;
  }


  li.ramp = ramptype;
      


  idata = mrc_read_byte(fin, &hdata, &li, mrc_default_status);
     
  if (!idata)
    {
      fprintf(stderr, "%s: Error reading image data\n", progname);
      exit(3);
    }

     
  printf("\nWriting %s\n", argv[i]);

  mrc_byte_mmm(&hdata, idata);
     
  hdata.mode = mode;

  /* DNM: eliminate extra header info in the output, and mark it as not
     swapped now that we're done reading data */
  hdata.headerSize = 1024;
  hdata.next = 0;
  hdata.nint = 0;
  hdata.nreal = 0;
  hdata.nsymbt = 0;
  hdata.swapped = 0;

  mrc_head_label(&hdata, "mrcbyte: Converted and scaled to byte mode.");


  if (!data_only){
    mrc_head_write(fout, &hdata);
  }

  if (mode == MRC_MODE_BYTE){
    if (reverse_video){
      xysize = hdata.nx * hdata.ny;
      for(k = 0; k < hdata.nz; k++)
        for(i = 0; i < xysize; i++){
          cdat = ((int)idata[k][i] * -1) + 255;
          idata[k][i] = cdat;
        }
    }
    mrc_write_idata(fout, &hdata, (void **)idata);
  }
     
     
  else if (mode == MRC_MODE_SHORT){
    xysize = hdata.nx * hdata.ny;
    for (k = 0; k < hdata.nz; k++)
      for (i = 0; i < (xysize); i++){
        sdata = idata[k][i];
        b3dFwrite(&sdata, 2, 1, fout);
      }
  }
  else if (mode == MRC_MODE_FLOAT){
    xysize = hdata.nx * hdata.ny;
    for (k = 0; k < hdata.nz; k++)
      for (i = 0; i < (xysize); i++){
        fdata = idata[k][i];
        b3dFwrite(&fdata, 4, 1, fout);
      }
  }

  exit(0);
}
