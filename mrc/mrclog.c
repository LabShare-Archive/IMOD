/*  UNSUPPORTED
 *
 *  mrclog.c -- Take log base 10 of image data.
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
     unsigned char bdata;
     short sdata;
     float fpixel, fadd;
     float min, max, total;
     double dpixel;
     int pixsize;
     int i;

     int datasize = 0;


     if (argc < 3){
	  fprintf(stderr, "Usage: %s [infile] [outfile]\n",  argv[0]) ;
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

     mrc_head_label(&hdata, "mrclog: Took log base 10 of data");
     mrc_head_write(fout, &hdata);
     

     switch(hdata.mode){
	  
	case MRC_MODE_BYTE:
	  pixsize = sizeof(unsigned char);
	  for(i = 0; i < datasize; i++){
	       fread(&bdata, pixsize, 1, fin);
	       fpixel = bdata;
	       fpixel = flog(fpixel);
	       bdata = fpixel;
	       fwrite(&bdata,  pixsize, 1, fout);
	       
	  }
	  break;

	case MRC_MODE_SHORT:
	case MRC_MODE_COMPLEX_SHORT:
	  pixsize = sizeof(short);
	  for(i = 0; i < datasize; i++){
	       fread(&sdata, pixsize, 1, fin);
	       fpixel = sdata;
	       fpixel = flog(fpixel);
	       sdata = fpixel;
	       fwrite(&sdata,  pixsize, 1, fout);
	  }
	  break;

	case MRC_MODE_FLOAT:
	case MRC_MODE_COMPLEX_FLOAT:
	  fread(&min, pixsize, 1, fin);
	  for ( i = 1; i < datasize; i++){
	       fread(&fpixel, pixsize, 1, fin);
	       if (fpixel < min)
		    min = fpixel;
	  }
	  fseek(fin, 1024, SEEK_SET);
	  
	  pixsize = sizeof(float);
	  if (min < 1.0)
	       fadd = 1.0 - min;
	  else
	       fadd = 0.0;
	  min = 999999;
	  max = 0;
	  for(i = 0; i < datasize; i++){
	       fread(&fpixel, pixsize, 1, fin);
	       fpixel += fadd;
	       dpixel = fpixel;
	       if (dpixel < 1.0)
		    dpixel = 1.0;
	       fpixel = (float)log(dpixel);
	       if (fpixel < min)
		    min = fpixel;
	       if (max < fpixel)
		    max = fpixel;
	       total += fpixel;
	       fwrite(&fpixel,  pixsize, 1, fout);
	  }
	  total = total / datasize;
	  hdata.amin = min;
	  hdata.amax = max;
	  hdata.amean = total;
	  mrc_head_write(fout, &hdata);	  
	  break;

	default:
	  fprintf(stderr, "%s: data type %d unsupported.\n", argv[0], 
		  hdata.mode);
	  break;

     }

     fclose(fin);
     fclose(fout);


}
     














