/*  IMOD VERSION 2.02
 *
 *  frommrc.c -- Convert mrc files to SGI rgb files.
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
    Revision 3.3  2004/07/07 19:25:30  mast
    Changed exit(-1) to exit(3) for Cygwin

    Revision 3.2  2003/10/24 02:28:42  mast
    strip directory from program name and/or use routine to make backup file

    Revision 3.1  2002/11/05 23:45:56  mast
    Changed to call imodCopyright

*/

#include <stdio.h>
#include <stdlib.h>
#include "b3dutil.h"

#ifndef __sgi
main(int argc, char **argv)
{
     fprintf(stderr, "%s only runs on sgi machines.\n", argv[0]);
}
#else

#include <gl/image.h>
#include "mrcfiles.h"

putbyte(outf,val)
     FILE *outf;
     unsigned char val;
{
     unsigned char buf[1];
     
     buf[0] = val;
     if (!fwrite(buf,1,1,outf))
	  fprintf(stderr, "putbyte: write error.\n");
     return 0;
}

putshort(outf,val)
     FILE *outf;
     unsigned short val;
{
     unsigned char buf[2];
     
     buf[0] = (val>>8);
     buf[1] = (val>>0);
     fwrite(buf,2,1,outf);
     return 0;
}

static int putlong(outf,val)
     FILE *outf;
     unsigned long val;
{
     unsigned char buf[4];
     
     buf[0] = (val>>24);
     buf[1] = (val>>16);
     buf[2] = (val>>8);
     buf[3] = (val>>0);
     return fwrite(buf,4,1,outf);
}


void mrc_rgb_fillbuf(short *buf, unsigned char *idata, int xsize)
{
     int i;

     for (i = 0; i < xsize; i++)
	  buf[i] = (short)idata[i];
}

main(int argc, char *argv[])
{
     IMAGE *image;
     FILE *fin, *of;
     struct MRCheader hdata;
     short xsize, ysize, zsize;
     int xysize;
     char iname[80];
     unsigned char **idata;
     short *buf;
     int i, x, y, z;
     char *progname = imodProgName(argv[0]);
     

     if (argc != 3){
	  fprintf(stderr, 
		  "%s version 1.0\n",  progname);
	  imodCopyright();
	  fprintf(stderr, "%s [mrc file] [rgb name/root]\n\n", progname);
	  fprintf(stderr, "A series of rgb files will be created ");
	  fprintf(stderr, "with the prefix [rgb root name]\n");
	  fprintf(stderr, "and with the suffex nnn.rgb, ");
	  fprintf(stderr, "where nnn is the z number.\n");
	  exit(1);
     }

     if (NULL == (fin = fopen(argv[1], "rb"))){
	  fprintf(stderr, "%s: Couldn't open %s\n", progname, argv[1]);
	  exit(3);
     }

     if (mrc_head_read(fin, &hdata)){
	  fprintf(stderr, "%s: Can't Read Input Header from %s.\n",
		  progname, argv[1]);
	  exit(3);
     }

     if (hdata.mode == MRC_MODE_RGB){
	  mrc_rgb_write(fin, &hdata, argv[2]);
	  fclose(fin);
	  exit(0);
     }

     if (hdata.mode){
	  fprintf(stderr, "mrc2rgb: Only byte mode data supported.\n");
	  exit(3);
     }
     
     xsize = hdata.nx;
     ysize = hdata.ny;
     zsize = hdata.nz;
     xysize = xsize * ysize;

     if (mrc_head_read(fin, &hdata)){
	  fprintf(stderr, "mrc2rgb: Can't Read Input Header from %s.\n",
		  argv[2]);
	  exit(3);
     }

     idata = mrc_read_byte(fin, &hdata, NULL, NULL);
     
     buf = (short *)malloc(xsize * sizeof(short));

     printf("Writing rgb images. ");
     for (z = 0; z < zsize; z++){
	  
	  printf(".");
	  fflush(stdout);

	  sprintf(iname, "%s.%3.3d.rgb", argv[2], z);
	  if (zsize == 1)
	       sprintf(iname, "%s", argv[2]);

	  image = iopen(iname, "w", RLE(1), 2, xsize, ysize, 1);

	  for(y = 0; y < ysize; y++){
	       mrc_rgb_fillbuf(buf, &(idata[z][y * xsize]), xsize);
	       putrow(image, buf, y, 0);
	  }
	  iclose(image);
     }
     printf("\r\n");
}


mrc_rgb_write(FILE *fin, struct MRCheader *hd, char *rgb)
{
     IMAGE *image;
     unsigned char *rdat, *gdat, *bdat;
     short *buf;
     int xysize;
     int xsize, ysize;
     int z, i, y;
     char iname[128];
     FILE *of;

     xsize = hd->nx;
     ysize = hd->ny;
     xysize = hd->nx * hd->ny;

     rdat = (unsigned char *)malloc(xysize);
     gdat = (unsigned char *)malloc(xysize);
     bdat = (unsigned char *)malloc(xysize);
     buf  = (short *)malloc(xsize * sizeof(short));

     if ( (!rdat) || (!gdat) || (!bdat) || (!buf) ){
	  fprintf(stderr, "mrc_rgb_write: memory alloc error.\n");
	  exit(3);
     }

     printf("Writing rgb images. ");
     rewind(fin);
     fseek(fin, 1024, SEEK_SET);

     for(z = 0; z < hd->nz; z++){
	  
	  printf(".");
	  fflush(stdout);
	  
	  sprintf(iname, "%s.%3.3d.rgb", rgb, z);
	  if (hd->nz == 1)
	       sprintf(iname, "%s", rgb);
	  image = iopen(iname, "w", RLE(1), 3, xsize, ysize, 3);

	  for(i = 0; i < xysize; i++){
	       fread(&(rdat[i]), 1, 1, fin);
	       fread(&(gdat[i]), 1, 1, fin);
	       fread(&(bdat[i]), 1, 1, fin);
	  }

	  for(y = 0; y < ysize; y++){
	       mrc_rgb_fillbuf(buf, &(rdat[y * xsize]), xsize);
	       putrow(image, buf, y, 0);
	       mrc_rgb_fillbuf(buf, &(gdat[y * xsize]), xsize);
	       putrow(image, buf, y, 1);
	       mrc_rgb_fillbuf(buf, &(bdat[y * xsize]), xsize);
	       putrow(image, buf, y, 2);
	  }

	  iclose(image);
     }
     printf("\r\n");

     free(rdat);
     free(gdat);
     free(bdat);
     free(buf);
     return 0;
}

#endif
