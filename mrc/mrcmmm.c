/*  UNSUPPORTED
 *
 *  mrcmmm.c -- Recalculate min, max and mean in mrc header.
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

#include <math.h>
#include "mrcfiles.h"


main( int argc, char *argv[] )
{
     FILE   *fin, *fout;
     struct MRCheader hdata;
     unsigned char bdata;
     short sdata;
     float fpixel, fadd;
     float min, max, total;
     double dpixel;
     int pixsize;
     int i;

     int datasize = 0;


     if (argc < 2){
	  fprintf(stderr, "Usage: %s [image file]\n",  argv[0]) ;
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
     



     if (mrc_head_read(fin, &hdata))
	  {
	       fprintf(stderr, "Can't Read Input File Header.\n");
	       exit(3);
	  }
     
     datasize = hdata.nx * hdata.ny * hdata.nz;


     if (hdata.mode == MRC_MODE_COMPLEX_SHORT)
	  datasize *= 2;

     if (hdata.mode == MRC_MODE_COMPLEX_FLOAT)
	  datasize *= 2;

     switch(hdata.mode){
	  
	case MRC_MODE_BYTE:
	  max = 0;
	  min = 255;
	  pixsize = sizeof(unsigned char);
	  for(i = 0; i < datasize; i++){
	       fread(&bdata, pixsize, 1, fin);
	       if (bdata > max)
		    max = bdata;
	       if (bdata < min)
		    min = bdata;
	       total += bdata;
	  }
	  break;

	case MRC_MODE_SHORT:
	case MRC_MODE_COMPLEX_SHORT:
	  pixsize = sizeof(short);
	  fread(&sdata, pixsize, 1, fin);
	  min = sdata;
	  max = sdata;
	  total = sdata;
	  for(i = 1; i < datasize; i++){
	       fread(&sdata, pixsize, 1, fin);
	       if (sdata < min)
		    min = sdata;
	       if (sdata > max)
		    max = sdata;
	       total += sdata;
	  }
	  break;

	case MRC_MODE_FLOAT:
	case MRC_MODE_COMPLEX_FLOAT:
	  pixsize = 4;
	  fread(&min, pixsize, 1, fin);
	  max = min;
	  total = min;
	  for ( i = 1; i < datasize; i++){
	       fread(&fpixel, pixsize, 1, fin);
	       if (fpixel < min)
		    min = fpixel;
	       if (fpixel > max)
		    max = fpixel;
	       total += fpixel;
			 
	  }
	  break;

	default:
	  fprintf(stderr, "%s: data type %d unsupported.\n", argv[0], 
		  hdata.mode);
	  break;

     }


     total = total / datasize;
     hdata.amin = min;
     hdata.amax = max;
     hdata.amean = total;
     printf("Min = %g, Max = %g, Mean = %g\n", min, max, total);


     mrc_head_write(fout, &hdata);	  
     fclose(fin);
     fclose(fout);


}
     














