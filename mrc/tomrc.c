/*  IMOD VERSION 2.02
 *
 *  tomrc.c -- Convert SGI rgb files to an mrc file.
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
/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.1  2002/11/05 23:53:13  mast
    Changed to call imodCopyright

*/

/* To compile:  cc tomrc.c -o readimage -limage */

#include <stdio.h>
#include <stdlib.h>

#ifndef __sgi
main(int argc, char **argv)
{
     fprintf(stderr, "%s only runs on sgi machines.\n", argv[0]);
}
#else

#include <gl/image.h>
#include "mrcfiles.h"
#include "b3dutil.h"

main(argc,argv)
     int argc;
     char **argv;
{
     struct MRCheader hdata;
     FILE *fout;
     IMAGE *image = NULL;
     int i;
     int x, y, z;
     int xsize = 0, ysize = 0, zsize;
     int ox, oy, oxsize, oysize;
     int xborder, yborder;
     int color = FALSE;
     short *rbuf, *gbuf, *bbuf;
     float avg = 0.0;



     if (argc < 3){
	  fprintf(stderr, "tomrc version 1.0 %s, %s\n",__DATE__,__TIME__);
	  imodCopyright();
	  fprintf(stderr, "Converts SGI rgb or bw files to mrc format.\n");
	  fprintf(stderr, "Usage: tomrc [-c] [-x #] [-y #] [infiles.rgb...] [outfile.mrc]\n");
	  fprintf(stderr, "Options:\n");
	  fprintf(stderr, "\t-c will cause a color mrc file to be made.\n");
	  fprintf(stderr, "\t-x xsize of output image. (Default, size of first input image.)\n");
	  fprintf(stderr, "\t-y ysize of output image.\n");
	  exit(1);
     }
     
     for (i = 1; i < argc; i++){
	  if (argv[i][0] == '-'){
	       switch (argv[i][1]){
		  case 'c':
		    color = TRUE;
		    break;

		  case 'x':
		    sscanf(argv[++i], "%d", &xsize);
		    break;

		  case 'y':
		    sscanf(argv[++i], "%d", &ysize);
		    break;

		  default:
		    break;

	       }
	  }else 
	       break;
     }

/*     printf("Opening %s\n", argv[i]); */
     if( (image=iopen(argv[i],"r")) == NULL ) {
	  fprintf(stderr,"tomrc: can't open input file %s\n",argv[i]);
	  exit(1);
     }

     
     fout = fopen(argv[argc - 1], "wb");
     
     if (fout == NULL){
	  fprintf(stderr,"tomrc: Couldn't open output file %s\n", argv[2]);
	  exit(1);
     }
     
     if (xsize <= 0)
	  xsize = image->xsize;
     if (ysize <= 0)
	  ysize = image->ysize;

     zsize = argc - i - 1;
     

     mrc_head_new( &hdata, xsize, ysize, zsize, 0);
     hdata.amin = image->min;
     hdata.amax = image->max;
     hdata.amean = 128;

     if ((color)  && (image->zsize >= 3))
	  hdata.mode = MRC_MODE_RGB;

     mrc_head_write(fout, &hdata);

     iclose(image);

/*     printf("xsize,ysize = %d,%d\n",xsize,ysize); */
     while (i < (argc - 1)){
	  if( (image = iopen(argv[i],"r")) == NULL ) {
	       fprintf(stderr,"tomrc: can't open input file %s\n",argv[i]);
	       exit(1);
	  }
/*	  printf("Opened %s, %d,%d\n", argv[i],image->xsize,image->ysize); */

	  rbuf = (short *)malloc(image->xsize * sizeof(short));
	  gbuf = (short *)malloc(image->xsize * sizeof(short));
	  bbuf = (short *)malloc(image->xsize * sizeof(short));

	  xborder = xsize - image->xsize;
	  yborder = ysize - image->ysize;
	  xborder /= 2;
	  yborder /= 2;

/*	  printf("xyborder = (%d, %d)\n", xborder, yborder);  */

	  oysize = image->ysize;

	  /* fill bottom border */
	  if (yborder >= 0){
	       for(y = 0; y < yborder; y++)
		    for(x = 0; x < xsize; x++){
			 fputc(0x00,fout);
			 if (color){
			      fputc(0x00,fout);
			      fputc(0x00,fout);
			 }
		    }
	       oy = 0;
	  }else{
	       oy = -1 * yborder;
	       oysize = ysize + oy;
	  }

	  for(; oy < oysize; oy++, y++){
/*	       printf("row %d, line %d\n", oy, y); */
	       getrow(image,rbuf,oy,0);
	       if (image->zsize > 2){
		    getrow(image,gbuf,oy,1);
		    getrow(image,bbuf,oy,2);
	       }
	       
	       /* fill left border */
	       if (xborder >= 0){
		    for(x = 0; x < xborder; x++){
			 fputc(0x00,fout);
			 if (color){
			      fputc(0x00,fout);
			      fputc(0x00,fout);
			 }
		    }
		    ox = 0;
		    oxsize = image->xsize;
		    
	       }else{
		    x = -xborder;
		    oxsize = xsize + x;
		    ox = x;

	       }
	       
	       /* fill image */
	       for (; ox < oxsize; ox++, x++){
		    if (color){
			 
			 fputc(rbuf[ox],fout);
			 fputc(gbuf[ox],fout);
			 fputc(bbuf[ox],fout);
			 
		    }
		    if (image->zsize > 2){
			 rbuf[ox] = ((float)rbuf[ox] * 0.3) + 
			      ((float)gbuf[ox] * 0.59) + 
				   ((float)bbuf[ox] * 0.11);
		    }

		    if (!color)
			 fputc(rbuf[ox], fout);

		    avg += rbuf[x];
	       }

	       /* fill right border */
	       for (;x<xsize;x++){
		    fputc(0x00,fout);
		    if (color){
			 fputc(0x00,fout);
			 fputc(0x00,fout);
			      }
	       }
	  }

	  /* fill bottom border */
	  for(;y<ysize;y++)
	       for(x=0;x<xsize;x++){
		    fputc(0x00,fout);
		    if (color){
			 fputc(0x00,fout);
			 fputc(0x00,fout);
		    }
	       }

	  free(rbuf);
	  free(gbuf);
	  free(bbuf);
	  iclose(image);
	  i++;
     }

     avg /= xsize * ysize * zsize;
     hdata.amean = avg;
     mrc_head_write(fout, &hdata);
     
     fclose(fout);
     
}


#endif
