/*  UNSUPPORTED
 *
 *  mrctilt.c -- Modify a mrc header to contain tilt information.
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
#include <math.h>
#include "mrcfiles.h"


main( int argc, char *argv[] )
{
     FILE   *fin, *fout;
     struct MRCheader hdata;
     float first, inc;
     int datasize = 0;

     if (argc != 4){
	  fprintf(stderr, 
		  "%s version 1.0 Copyright (C)1994 Boulder Laboratory for\n", 
		  argv[0]);
	  fprintf(stderr,
		  "3-Dimensional Fine Structure, University of Colorado.\n"); 
	  fprintf(stderr, "%s: Modify a mrc header to contain tilt information.\n", argv[0]) ;
	  fprintf(stderr, "Usage: %s [image file] [first tilt] [tilt increment]\n",  argv[0]) ;
	   exit(3) ;
     }
     

     fin = fopen(argv[1], "rb");
     if (fin == NULL)
	  {
	       fprintf(stderr, "Error opening %s.\n", argv[1]);
	       exit(3);
	  }
     
     fout = fopen(argv[1], "rb+");
     if (fin == NULL)
	  {
	       fprintf(stderr, "Error opening %s.\n", argv[2]);
	       exit(3);
	  }
     
     sscanf(argv[2], "%f", &first);
     sscanf(argv[3], "%f", &inc);


     if (mrc_head_read(fin, &hdata))
	  {
	       fprintf(stderr, "Can't Read Input File Header.\n");
	       exit(3);
	  }

     printf("First tilt = %g, Inc = %g\n", first, inc);

     hdata.idtype = 1;
     hdata.vd1 = first * 100.0;
     hdata.vd2 = inc * 100.0;

     mrc_head_write(fout, &hdata);	  
     fclose(fin);
     fclose(fout);
}
     














