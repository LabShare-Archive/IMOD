/*
 *  mrcbyte.c -- Converts an mrc file to byte format.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

    $Date$

    $Revision$

    $Log$
*/

#include <stdio.h>
#include "mrcfiles.h"

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

main( int argc, char *argv[] )
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


     if (argc < 2){
	  fprintf(stderr, 
		  "%s version %s\n", argv[0], VERSION_NAME);
	  imodCopyright();
	  mrcbyte_help(argv[0]);
	  exit(-1);
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
		    fprintf(stderr, "%s: illegal option\n", argv[0]);
		    mrcbyte_help(argv[0]);
		    exit(1);
		    break;
		    
	       }else break;

     if (i < (argc - 1)){
	  

	  fin = fopen(argv[i], "r");
	  if (fin == NULL)
	       {
		    fprintf(stderr, "Error opening %s.\n", argv[i]);
		    exit(-1);
	       }

	  i++;
	  fout = fopen(argv[i], "w");
	  if (fout == NULL)
	       {
		    fprintf(stderr, "Error opening %s.\n", argv[i]);
		    exit(-1);
	       }
     }else{
	  mrcbyte_help(argv[0]);
	  exit(-1);	  
     }
     

     if (mrc_head_read(fin, &hdata))
	  {
	       fprintf(stderr, "Can't Read Input File Header.\n");
	       exit(-1);
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
	       fprintf(stderr, "%s: Error reading image data\n", argv[0]);
	       exit(-1);
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
		    fwrite(&sdata, 2, 1, fout);
	       }
     }
     else if (mode == MRC_MODE_FLOAT){
	  xysize = hdata.nx * hdata.ny;
	  for (k = 0; k < hdata.nz; k++)
	       for (i = 0; i < (xysize); i++){
		    fdata = idata[k][i];
		    fwrite(&fdata, 4, 1, fout);
	       }
     }

     exit(0);
}










































