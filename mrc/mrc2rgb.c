/*  UNSUPPORTED
 *
 *  mrc2rgb.c
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
#include <mrcfiles.h>

putbyte(outf,val)
     FILE *outf;
     unsigned char val;
{
     unsigned char buf[1];
     
     buf[0] = val;
     fwrite(buf,1,1,outf);
}

putshort(outf,val)
     FILE *outf;
     unsigned short val;
{
     unsigned char buf[2];
     
     buf[0] = (val>>8);
     buf[1] = (val>>0);
     fwrite(buf,2,1,outf);
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




main(int argc, char *argv[])
{
     FILE *mrcrf, *mrcgf, *mrcbf, *of;
     struct MRCheader hdata;
     short xsize, ysize, zsize;
     int xysize;
     char iname[80];
     unsigned char *idata;
     int i,z;
     

     if (argc != 5){
	  printf("mrc2rgb: usage,\n");
	  printf("mrc2rgb <red file> <green file> <blue file> <out file>\n");
	  exit(1);
     }

     if (NULL == (mrcrf = fopen(argv[1], "rb"))){
	  fprintf(stderr, "mrc2rgb: Couldn't open %s\n", argv[1]);
	  exit(3);
     }
     if (NULL == (mrcgf = fopen(argv[2], "rb"))){
	  fprintf(stderr, "mrc2rgb: Couldn't open %s\n", argv[2]);
	  exit(3);
     }
     if (NULL == (mrcbf = fopen(argv[3], "rb"))){
	  fprintf(stderr, "mrc2rgb: Couldn't open %s\n", argv[3]);
	  exit(3);
     }


     if (mrc_head_read(mrcrf, &hdata)){
	  fprintf(stderr, "mrc2rgb: Can't Read Input Header from %s.\n",
		  argv[1]);
	  exit(3);
     }
     
     if (hdata.mode){
	  fprintf(stderr, "mrc2rgb: Only byte mode data supported.\n");
	  exit(3);
     }
     
     xsize = hdata.nx;
     ysize = hdata.ny;
     zsize = hdata.nz;

     if (mrc_head_read(mrcgf, &hdata)){
	  fprintf(stderr, "mrc2rgb: Can't Read Input Header from %s.\n",
		  argv[2]);
	  exit(3);
     }

     if (hdata.mode){
	  fprintf(stderr, "mrc2rgb: Only byte mode data supported.\n");
	  exit(3);
     }
     
     if ( (xsize != hdata.nx) || (ysize != hdata.ny) || (zsize != hdata.nz) ){
	  fprintf(stderr, "mrc2rgb: All mrcfiles must be same size.\n");
	  exit(3);
     }


     if (mrc_head_read(mrcbf, &hdata)){
	  fprintf(stderr, "mrc2rgb: Can't Read Input Header from %s.\n",
		  argv[3]);
	  exit(3);
     }
     
     if (hdata.mode){
	  fprintf(stderr, "mrc2rgb: Only byte mode data supported.\n");
	  exit(3);
     }
     
     if ( (xsize != hdata.nx) || (ysize != hdata.ny) || (zsize != hdata.nz) ){
	  fprintf(stderr, "mrc2rgb: All mrcfiles must be same size.\n");
	  exit(3);
     }

     idata = (unsigned char *)malloc(xsize * ysize);
     xysize = xsize * ysize;


     for (z = 0; z < zsize; z++){

	  sprintf(iname, "%s.%3.3d.rgb", argv[4], z);

	  if (NULL == (of  = fopen(iname, "wb"))){
	       fprintf(stderr, "mrc2rgb: Couldn't open %s\n", argv[4]);
	       exit(3);
	  }

	  putshort(of,474);       /* MAGIC                */
	  putbyte(of,0);          /* STORAGE is VERBATIM  */
	  putbyte(of,1);          /* BPC is 1             */
	  putshort(of,3);         /* DIMENSION is 3       */
	  putshort(of,xsize);     /* XSIZE                */
	  putshort(of,ysize);     /* YSIZE                */
	  putshort(of,3);         /* ZSIZE                */
	  putlong(of,0l);          /* PIXMIN is 0          */
	  putlong(of,255l);        /* PIXMAX is 255        */
	  for(i=0; i<4; i++)      /* DUMMY 4 bytes        */
	       putbyte(of,0);
	  strcpy(iname,"Created by mrc2rgb.");
	  fwrite(iname,80,1,of);  /* IMAGENAME            */
	  putlong(of,0l);          /* COLORMAP is 0        */
	  for(i=0; i<404; i++)    /* DUMMY 404 bytes      */
	       putbyte(of,0);
	  
	  fread(idata, 1, xysize, mrcrf);
	  fwrite(idata, 1, xysize, of);
	  fread(idata, 1, xysize, mrcgf);
	  fwrite(idata, 1, xysize, of);
	  fread(idata, 1, xysize, mrcbf);
	  fwrite(idata, 1, xysize, of);
	  
	  fclose(of);
     }

}
	  

