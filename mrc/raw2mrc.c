/*  IMOD VERSION 2.30
 *
 *  raw2mrc.c -- Convert raw image data to an mrc image file.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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
#include <string.h>
#include "mrcfiles.h"

/* input data types */
#define DTYPE_CHAR   1
#define DTYPE_BYTE   1
#define DTYPE_UCHAR  2
#define DTYPE_SHORT  3
#define DTYPE_USHORT 4
#define DTYPE_LONG   5
#define DTYPE_ULONG  6
#define DTYPE_FLOAT  7
#define DTYPE_SBYTE  10

/* input image types */
#define ITYPE_GREY    1
#define ITYPE_COMPLEX 2
#define ITYPE_RGB     3

/* compression */
#define CTYPE_NONE 0
static void rawswap(unsigned char *indata, int pixsize, int xysize);
static void rawdecompress
(void *indata, int pixsize, int xysize, int compression);

int setintype(char *stype, int *size, int *otype);

void usage(void)
{	  
     fprintf(stderr, "raw2mrc version 2.00 %s %s.\n", __DATE__,__TIME__);
     fprintf(stderr, "Copyright (C)1994 Boulder Laboratory for\n");
     fprintf(stderr, "3-Dimensional Fine Structure, ");
     fprintf(stderr, "Regents of the University of Colorado.\n"); 
     fprintf(stderr, "Converts raw data into mrc file format.\n");
     fprintf(stderr, "Usage raw2mrc [options] <input files...> <output file>\n");
     fprintf(stderr, "Options:\n");
     fprintf(stderr, "\t-x #    Width of input image, default 640\n");
     fprintf(stderr, "\t-y #    Height of input image, default 480\n");
     fprintf(stderr, "\t-z #    Number of sections in input, default 1\n");
     fprintf(stderr, "\t-t type Input data type. \n"
	             "\t        (byte, sbyte, short, ushort, "
	             "long, ulong, float, rgb)\n");
     fprintf(stderr, "\t-s      Swap input bytes.\n");
     fprintf(stderr, "\t-o #    Offset at beginning of file, default 0\n");
     fprintf(stderr, "\t-oz #   Offset between each pair of sections, default 0\n");
     fprintf(stderr, "\t-f      Flip image around X axis\n");
     fprintf(stderr, "\t-d      Divide unsigned shorts by 2 instead of subtracting 32767\n");
     return;
}

main( int argc, char *argv[] )
{
     struct MRCheader hdata;
     int    i, j, k;
     unsigned long xysize;
     FILE   *fin;
     FILE   *fout;
     int    intype, outtype, datatype;
     int    compression = 0;
     int    byteswap = FALSE;
     int    x = 640, y = 480, z = 1;
     int    nfiles, nsecs;
     int    flip = 0;   /* flag whether to flip about X axis */
     int    divide = 0; /* flag to divide unsigned short by 2 */
     int    hsize = 0;  /* amount of data to skip */
     int    csize = 1;
     int    zoffset = 0;
     int    pixsize = 1;
     float  angle = 90.0;
     float  xlen, ylen, zlen;
     char   line[128];
     unsigned char buf[4];
     char   c, c1, c2;
     short  s;
     unsigned short usval;
     int datasize;
     float  f, pixel;
     float  mean = 0.0f, tmean = 0.0f, max = -5e29f, min= 5e29f;
     long start = 0;
     signed char *sbdata, sbval;
     char *bdata, bval;
     short *sdata, sval;
     unsigned short *usdata;
     long *ldata;
     unsigned long *uldata;
     float *fdata, fval;
     void *indata;
     int val;

     if (argc < 3){
	  usage();
	  exit(-1);
     }
     

     for (i = 1; i < argc ; i++){
	  if (argv[i][0] == '-'){
	       switch (argv[i][1]){

		  case 't': /* input file type */
		    intype = setintype(argv[++i], &pixsize, &outtype);
		    printf("pixsize = %d\n", pixsize);
		    break;
		    
		  case 'o':
		    if (argv[i][2] == 'z'){
			 zoffset = atoi(argv[++i]);
		    }else{
			 hsize = atoi(argv[++i]);
		    }
		    break;
		  case 'x':
		    x = atoi(argv[++i]);
		    break;
		  case 'y':
		    y = atoi(argv[++i]);
		    break;
		  case 'z':
		    z = atoi(argv[++i]);
		    break;

		  case 's':
		    byteswap = TRUE;
		    break;
	          case 'f':
		    flip = 1;
		    break;
	          case 'd':
		    divide = 1;
		    break;
	       }
	  }else break;
     }
     
     if ( (argc - 1) < (i + 1)){
	  fprintf(stderr, "%s: argument error.\n", argv[0]);
	  usage();
	  exit(-1);
     }

     nfiles = argc - (i + 1);
     nsecs = z *nfiles;

     printf("nfiles = %d, argc = %d\n",nfiles, argc);

     fout = fopen(argv[argc - 1], "w");
     if (!fout){
	  fprintf(stderr, "%s: error opening %s for output\n", argv[0],
		  argv[argc -1]);
	  exit(-1);
     }

     for (j = 0; j < 1024; j++)
	  fwrite(&start , 1, 1, fout);

     datasize = x * y * z;
     xysize = x * y * csize;

     indata = (void *)malloc(pixsize * csize * xysize);
     if (!indata){
	  fprintf(stderr, "%s: error getting memory.\n", argv[0]);
	  exit(-1);
     }

     for(j = i ; j < argc-1 ; j++) {
       fin = fopen(argv[j], "r");
       if (!fin){
	 fprintf(stderr, "%s: error opening %s for input\n", argv[0],
		 argv[j]);
	 exit(-1);
       }

       fseek(fin, hsize, SEEK_CUR);

       for(k = 0; k < z; k++){
	  tmean = 0.0f;
	  if (k > 0) fseek(fin, zoffset, SEEK_CUR);
	  fread(indata, pixsize, xysize, fin);
	  if (compression){
	       rawdecompress(indata, pixsize, xysize, compression);
	  }
	  if (byteswap)
	       rawswap(indata, pixsize, xysize);

	  switch(intype){

	     case DTYPE_USHORT:
	       usdata = (unsigned short *)indata;
	       sdata = (short *)indata;
	       if (divide) 
		    for(i = 0; i < xysize; i++)
			 sdata[i] = (short)((int)(usdata[i]) / 2);
	       else
		    for(i = 0; i < xysize; i++)
			 sdata[i] = (short)((int)(usdata[i]) - 32767);

	       for(i = 0; i < xysize; i++){
		    pixel = sdata[i];
		    tmean += pixel;
		    if (min > pixel)
			 min = pixel;
		    if (max < pixel)
			 max = pixel;
	       }
	       break;

	     case DTYPE_LONG:
	       ldata = (long *)indata;
	       fdata = (float *)indata;
	       for(i = 0; i < xysize; i++){
		    fval = ldata[i];
		    fdata[i] = fval;
		    pixel = fval;
		    tmean += pixel;
		    if (min > pixel)
			 min = pixel;
		    if (max < pixel)
			 max = pixel;

	       }
	       break;

	     case DTYPE_ULONG:
	       uldata = (unsigned long *)indata;
	       fdata = (float *)indata;
	       for(i = 0; i < xysize; i++){
		    fval = uldata[i];
		    fdata[i] = fval;
		    pixel = fval;
		    tmean += pixel;
		    if (min > pixel)
			 min = pixel;
		    if (max < pixel)
			 max = pixel;

	       }
	       break;

	     case DTYPE_SHORT:
	       sdata = (short *)indata;
	       for(i = 0; i < xysize; i++){
		    pixel = sdata[i];
		    tmean += pixel;
		    if (min > pixel)
			 min = pixel;
		    if (max < pixel)
			 max = pixel;
	       }
	       break;

	     case DTYPE_FLOAT:
	       fdata = (float *)indata;
	       for(i = 0; i < xysize; i++){
		    pixel = fdata[i];
		    tmean += pixel;
		    if (min > pixel)
			 min = pixel;
		    if (max < pixel)
			 max = pixel;
	       }
	       break;

	     case DTYPE_SBYTE:
	       bdata  = (char *)indata;
	       for(i = 0; i < xysize; i++){
		   val = bdata[i];
		   if (val >= 128) val -= 256;
		   val += 127;
		   bdata[i] = val;
		    pixel = bdata[i];
		    tmean += pixel;
		    if (min > pixel)
			 min = pixel;
		    if (max < pixel)
			 max = pixel;
	       }
	       break;

	     case DTYPE_BYTE:
	       bdata = (char *)indata;
	       for(i = 0; i < xysize; i++){
		    pixel = bdata[i];
		    tmean += pixel;
		    if (min > pixel)
			 min = pixel;
		    if (max < pixel)
			 max = pixel;
	       }
	       break;

	     default:
	       break;
	  }

	  if(flip == 0){
	    fwrite(indata, pixsize, xysize, fout);

	  }else{

	    for(i = y - 1; i >= 0 ; i--){
	      bdata = ((char *)indata) + i * pixsize * x;
	      fwrite(bdata, pixsize, x, fout); 
	    }
	  }
	  mean += (tmean / (float)xysize);
       }
       fclose(fin);
     }

     mean  /= (float)nsecs;

     printf("Min = %g, Max = %g, Mean = %g\n", min, max, mean);

     /* write out MRC header */
     rewind(fout);
     fwrite(&x, 4, 1, fout);
     fwrite(&y, 4, 1, fout);
     fwrite(&nsecs, 4, 1, fout);
     fwrite(&outtype, 4, 1, fout);

     fwrite(&start, 4, 1, fout);
     fwrite(&start, 4, 1, fout);
     fwrite(&start, 4, 1, fout);

     fwrite(&x, 4, 1, fout);
     fwrite(&y, 4, 1, fout);
     fwrite(&nsecs, 4, 1, fout);
     
     xlen = x;
     ylen = y;
     zlen = nsecs;

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
  
     fclose(fout);

     exit(0);
}


static void rawdecompress
(void *indata, int pixsize, int xysize, int compression)
{
     if (!compression)
	  return;

     return;
}


static void rawswap(unsigned char *indata, int pixsize, int xysize)
{
     unsigned char u1,u2,u3,u4;
     int i;

     if (pixsize == 4){
	  for (i = 0 ; i < xysize; i++){
	       u1 = indata[i * 2];
	       u2 = indata[(i * 2) + 1];
	       u3 = indata[(i * 2) + 2];
	       u4 = indata[(i * 2) + 3];
	       indata[i * 2] = u4;
	       indata[(i * 2) + 1] = u3;
	       indata[(i * 2) + 2] = u2;
	       indata[(i * 2) + 3] = u1;
	  }
	  return;
     }


     if (pixsize == 2){
	  for (i = 0 ; i < xysize; i++){
	       u1 = indata[i * 2];
	       u2 = indata[(i * 2) + 1];
	       indata[i * 2] = u2;
	       indata[(i * 2) + 1] = u1;
	  }
	  return;
     }
     return;
}

int setintype(char *stype, int *size, int *otype)
{
     if (!strcmp(stype,"byte")){
	  *size = 1;
	  *otype = MRC_MODE_BYTE;
	  return(DTYPE_BYTE);
     }
     
     if (!strcmp(stype,"sbyte")){
	  *size = 1;
	  *otype = MRC_MODE_BYTE;
	  return(DTYPE_SBYTE);
     }

     if (!strcmp(stype,"rgb")){
	  *size = 3;
	  *otype = MRC_MODE_RGB;
	  return(DTYPE_BYTE);
     }

     if (!strcmp(stype, "short")){
	  *size = 2;
	  *otype = MRC_MODE_SHORT;
	  return(DTYPE_SHORT);
     }
     if (!strcmp(stype,"ushort")){
	  *size = 2;
	  *otype = MRC_MODE_SHORT;
	  return(DTYPE_USHORT);
     }

     if (!strcmp(stype,"")){
	  *size = 4;
	  *otype = MRC_MODE_FLOAT;
	  return(DTYPE_LONG);
     }
     if (!strcmp(stype,"ulong")){
	  *size = 4;
	  *otype = MRC_MODE_FLOAT;
	  return(DTYPE_ULONG);
     }
     
     if (!strcmp(stype,"float")){
	  *size = 4;
	  *otype = MRC_MODE_FLOAT;
	  return(DTYPE_FLOAT);
     }

     return(-1);
}

