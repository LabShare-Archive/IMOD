/*  UNSUPPORTED
 *
 *  mrcshort.c  --  Converts an mrc file to short format.
 *
 *  Author: James Kremer email: kremer@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-1996 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <stdio.h>
#include "mrcfiles.h"


main( int argc, char *argv[] )
{
     int    i = 0, k;
     short sdata;
     FILE   *fin, *fout;
     int resize = FALSE;
     int cmap   = FALSE;
     int ramptype = MRC_RAMP_LIN;
     struct MRCheader hdata;
     struct MRCheader *data;
     struct LoadInfo  li;
     float  vd1, vd2, nd1, nd2;
     unsigned char **idata;
     char line[128];
     data = &hdata;
     
     if (argc < 3)
	  {
	       fprintf(stderr,"Usage: %s [infile] [outfile] [-rlec] \n",
		       argv[0]) ;
	       exit(3) ;
	  }
     
     fin = fopen(argv[1], "rb");
     if (fin == NULL)
	  {
	       fprintf(stderr, "Error opening %s.\n", argv[1]);
	       exit(3);
	  }
     
     fout = fopen(argv[2], "wb");
     if (fin == NULL)
	  {
	       fprintf(stderr, "Error opening %s.\n", argv[2]);
	       exit(3);
	  }
     
     
     if (argc > 3)
          if (argv[3][0] == '-')
	       for (i = 3; i < argc; i++)
		    switch (argv[i][1]){
		       case 'r':
			 resize = TRUE;
			 break;
		       case 'l':
			 ramptype = MRC_RAMP_LOG;
			 break;
		       case 'e':
			 ramptype = MRC_RAMP_EXP;
			 break;
		       case 'c':
			 cmap = TRUE;
		       default:
			 break;
		    }
     

     if (mrc_head_read(fin, &hdata))
	  {
	       fprintf(stderr, "Can't Read Input File Header.\n");
	       exit(3);
	  }
     
     
     
     if (resize)
	  resize = get_loadinfo(&hdata, &li);
     else{
	  li.xmax = hdata.nx;
	  li.ymax = hdata.ny;
	  li.zmax = hdata.nz - 1;
	  li.xmin = 0;
	  li.ymin = 0;
	  li.zmin = 0;
	  li.white = 255;
			           li.black = 0;
     }
     li.ramp = ramptype;
     
     if (cmap){
	  printf (" Enter (black level, white level) >");
	  fgetline(stdin, line, 127);
	  sscanf(line, "%d%*c%d\n", &(li.black), &(li.white));
	  if ( (!li.black) & (!li.white))
	       li.white = 255;
     }
     
     idata = mrc_read_byte(fin, &hdata, &li, mrc_default_status);
     
     if (!idata)
	  {
	       fprintf(stderr, "%s: Error reading image data\n", argv[0]);
	       exit(3);
	  }
     
     
     printf("\nWriting %s\n", argv[2]);
     

     hdata.mode = MRC_MODE_SHORT;
     mrc_byte_mmm(&hdata, idata);
     mrc_head_label(&hdata, "mrcshort: Converted to short mode. ");
     mrc_head_write(fout, &hdata);

     for (k = 0; k < hdata.nz; k++)
	  for (i = 0; i < (hdata.nx * hdata.ny); i++){
	       sdata = idata[k][i];
	       fwrite(&sdata, 2, 1, fout);
	  }


     fclose(fin);
     fclose(fout);

}



