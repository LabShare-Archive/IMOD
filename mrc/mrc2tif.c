/*  IMOD VERSION 2.41
 *
 *  mrc2tif.c -- Convert mrc files to TIFF files.
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
/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.3  2004/07/07 19:25:30  mast
    Changed exit(-1) to exit(3) for Cygwin

    Revision 3.2  2003/10/24 02:28:42  mast
    strip directory from program name and/or use routine to make backup file

    Revision 3.1  2002/11/05 23:48:02  mast
    Changed to call imodCopyright

*/

#include <stdio.h>
#include <stdlib.h>
#include "mrcfiles.h"
#include "b3dtiff.h"
#include "b3dutil.h"

main(int argc, char *argv[])
{
     FILE *fin;
     struct MRCheader hdata;

     int xsize, ysize, zsize, psize, xysize;
     int i, x, y, z;

     char iname[255];
     int *buf;
     char *progname = imodProgName(argv[0]);

     if (argc != 3){
	  fprintf(stderr, "%s version %s \n", progname, VERSION_NAME);
	  imodCopyright();
	  fprintf(stderr, "%s [mrc file] [tiff name/root]\n\n", progname);
	  fprintf(stderr, "A series of tiff files will be created "
		  "with the prefix [tiff root name]\n"
		  "and with the suffex nnn.tif, "
		  "where nnn is the z number.\n");
	  exit(1);
     }

     if (NULL == (fin = fopen(argv[1], "rb"))){
	  fprintf(stderr, "%s: Couldn't open %s\n", progname, argv[1]);
	  exit(3);
     }

     if (mrc_head_read(fin, &hdata)){
	  fprintf(stderr, "%s: Can't Read Input Header from %s.\n",
		  progname,argv[1]);
	  exit(3);
     }


     switch(hdata.mode){
	  
	case MRC_MODE_BYTE:
	  psize = 1;
	  break;
	case MRC_MODE_SHORT:
	  psize = 2;
	  break;
	case MRC_MODE_RGB:
	  psize = 3;
	  break;
	default:
	  fprintf(stderr, "mrc2tif Error: Datatype not supported.\n");
	  exit(3);
	  break;
     }
     xsize = hdata.nx;
     ysize = hdata.ny;
     zsize = hdata.nz;
     xysize = xsize * ysize;
     
     buf = (int *)malloc(xsize * ysize * sizeof(int));

     printf("Writing TIFF images. ");
     for (z = 0; z < zsize; z++){
	  FILE *fpTiff;
	  int tiferr;

	  printf(".");
	  fflush(stdout);

	  sprintf(iname, "%s.%3.3d.tif", argv[2], z);
	  if (zsize == 1)
	       sprintf(iname, "%s", argv[2]);

	  fpTiff = fopen(iname, "wb");
	  if (!fpTiff){
	       fprintf(stderr, "mrc2tif Error: Opening %s\n", iname);
	       perror("mrc2tif system message");
	       continue;
	  }

	  /* DNM: switch to calling a routine that takes care of swapping and
	     big seeks */
	  mrc_read_slice(buf, fin, &hdata, z, 'Z');

	  tiferr = tiff_write_image(fpTiff, xsize, ysize, hdata.mode,
			   (unsigned char *)buf);
	  if (tiferr){
	       fprintf(stderr, "\nmrc2tif: Error (%d) writing to %s\n",
		      tiferr, iname);
	       perror("mrc2tif ");
	       exit(1);
	  }
	  
	  fclose(fpTiff);
     }
     printf("\r\n");
     fclose(fin);
     free(buf);
     exit(0);
}
