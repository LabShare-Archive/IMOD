/*  OBSOLETE IMOD VERSION 2.02
 *
 *  rawmrc.c -- Convert raw image data to an mrc image file.
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

#include <stdlib.h>
#include <stdio.h>
#include "mrcfiles.h"

#define UNSIGNED_SHORT_SWAP 10
#define SIGNED_SHORT_SWAP   11
#define UNSIGNED_SHORT        12


static short getSignedShortSwap(FILE *fin)
{

}

main( int argc, char *argv[] )
{

     struct MRCheader hdata;
     int    i = 0;
     FILE   *fin;
     FILE   *fout;
     int    x, y, z;
     int    type = MRC_MODE_BYTE;
     int    hsize = 0;
     int    start = 0;
     float  angle = 90.0;
     float  xlen, ylen, zlen;
     char   line[128];
     long  lval;
     char   c, c1, c2;
     short  s;
     unsigned short usval;
     int datasize;
     float  f, pixel;
     float  mean, max, min, tmin;
     
     if (argc < 3){
	  fprintf(stderr, "rawmrc version 1.0 %s %s.\n", __DATE__,__TIME__);
	  fprintf(stderr, "Copyright (C)1994 Boulder Laboratory for\n");
	  fprintf(stderr,
		  "3-Dimensional Fine Structure, University of Colorado.\n"); 
	  fprintf(stderr, "Converts raw data into mrc file format.\n");
	  fprintf(stderr, "User will be prompted for input.\n");
	  fprintf(stderr, "Usage: rawmrc infile outfile.\n");
	  exit(3);
     }
     
     fin = fopen(argv[1], "rb");
     fout = fopen(argv[2], "wb");

     fflush(stdout);
     fflush(stdin);

     printf("Enter header size of input file. >");
     fgetline(stdin, line, 127);
     sscanf(line, "%d\n", &hsize);
     printf("%d\n", hsize);

     printf("Enter image size. (x,y,z) >");
     fgetline(stdin, line, 127);
     sscanf(line, "%d%*c%d%*c%d\n", &x, &y, &z);
     printf("(%d %d %d)\n", x, y, z);
     
     printf("Enter image type. \n(0 = BYTE, 1 = SHORT, 2 = FLOAT, 11 = SHORT/SWAP) >");
     fgetline(stdin, line, 127);
     sscanf(line, "%d\n", &type);
     printf("%d\n", type);
     

     fseek(fin, hsize, SEEK_CUR);
     mean = 0.0f;
     max  = -5e29f;
     min  = 5e29f;

     datasize = x * y * z;

     printf("Calculating Min, Max, Mean.\n");
     for (i = 0; i < datasize; i++){
	  switch (type){
	     case MRC_MODE_BYTE:
	       fread(&c, 1, 1, fin);
	       pixel = c;
	       break;
	     case MRC_MODE_SHORT:
	       fread(&s, 2, 1, fin);
	       pixel = s;
	       break;
	     case MRC_MODE_FLOAT:
	       fread(&f, 4, 1, fin);
	       pixel = f;
	       break;
	     case UNSIGNED_SHORT_SWAP:
	       fread(&c1, 1, 1, fin);
	       fread(&c2, 1, 1, fin);
	       s = (c2 << 8) + c1; 
	       s /= 2;
	       if (s < 0)
		    s = 32768 + s;
	       pixel = s;
	       break;
	     case SIGNED_SHORT_SWAP:
	       fread(&c1, 1, 1, fin);
	       fread(&c2, 1, 1, fin);
	       s = (c2 << 8) + c1;
	       pixel = s;
	       break;
	     case UNSIGNED_SHORT:
	       fread(&s, 2, 1, fin);
	       s /= 2;
	       if (s < 0)
		    s = 32768 + s;
	       pixel = s;
	       break;
	     default:
	       pixel = 0;
	       break;
	  }
	  mean += pixel;
	  if (pixel < min)
	       min = pixel;
	  if (pixel > max)
	       max = pixel;

     }

     tmin = min;
     mean /= datasize;

     printf("Min = %g, Max = %g, Mean = %g\n", min, max, mean);

     rewind(fout);
     
     fwrite(&x, 4, 1, fout);
     fwrite(&y, 4, 1, fout);
     fwrite(&z, 4, 1, fout);
     
     if ((type == UNSIGNED_SHORT_SWAP) || (type == SIGNED_SHORT_SWAP)
	 ||(type == UNSIGNED_SHORT)){
	  type = 1;
	  fwrite(&type, 4, 1, fout);
	  type = 10;
     }
     else
	  fwrite(&type, 4, 1, fout);

     fwrite(&start, 4, 1, fout);
     fwrite(&start, 4, 1, fout);
     fwrite(&start, 4, 1, fout);

     fwrite(&x, 4, 1, fout);
     fwrite(&y, 4, 1, fout);
     fwrite(&z, 4, 1, fout);
     
     xlen = x;
     ylen = y;
     zlen = z;

     fwrite(&xlen, 4, 1, fout);
     fwrite(&ylen, 4, 1, fout);
     fwrite(&zlen, 4, 1, fout);

     fwrite(&angle, 4, 1, fout);
     fwrite(&angle, 4, 1, fout);
     fwrite(&angle, 4, 1, fout);

     x = 1;
     y = 2;
     z = 3;

     fwrite(&x, 4, 1, fout);
     fwrite(&y, 4, 1, fout);
     fwrite(&z, 4, 1, fout);
     

     fwrite(&min, 4, 1, fout);
     fwrite(&max, 4, 1, fout);
     fwrite(&mean, 4, 1, fout);

     rewind(fin);
     fseek(fin, hsize, SEEK_CUR);
     rewind(fout);
     fseek(fout, 1024, SEEK_CUR);

     printf("Writing output file\n");

     for (i = 0; i < datasize; i++){
	  switch (type){
	     case MRC_MODE_BYTE:
	       fread(&c, 1, 1, fin);
	       if (tmin < 0)
		    c += tmin;
	       fwrite(&c, 1, 1, fout);
	       break;
	     case MRC_MODE_SHORT:
	       fread(&s, 2, 1, fin);
	       
	       fwrite(&s, 2, 1, fout);
	       break;
	     case MRC_MODE_FLOAT:
	       fread(&f, 4, 1, fin);
	       if (tmin < 0)
		    f += tmin;
	       fwrite(&f, 4, 1, fout);
	       break;

	     case UNSIGNED_SHORT_SWAP:
	       fread(&c1, 1, 1, fin);
	       fread(&c2, 1, 1, fin);
	       s = (c2 << 8) + c1; 
/*	       s /= 2; */
	       if (s < 0)
		    s = 32768 + s;
	       pixel = s;
	       fwrite(&s, 2, 1, fout);
	       break;

	     case SIGNED_SHORT_SWAP:
	       fread(&c1, 1, 1, fin);
	       fread(&c2, 1, 1, fin);
	       s = (c2 << 8) + c1;
	       fwrite(&s, 2, 1, fout);
	       break;

	     case UNSIGNED_SHORT:
	       fread(&s, 2, 1, fin);
/*	       s /= 2; */
	       if (s < 0)
		    s = 32768 + s;
	       fwrite(&s, 2, 1, fout);
	       pixel = s;
	       break;

	     default:
	       pixel = 0;
	       break;
	  }

     }




     fclose(fin);
     fclose(fout);
}
