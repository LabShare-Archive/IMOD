/*  IMOD VERSION 2.02
 *
 *  bdfile.c -- read image data from uncompressed image files.
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
#include <stdlib.h>

typedef struct
{
     FILE *fp;
     int hsize;
     int xsize;
     int ysize;
     int zsize;
     int dsize;

}bdIFile;

void bdIFileInit(bdIFile *ifp, FILE *fp,
		 int hsize, int xsize, int ysize, int zsize, int dsize)
{
     tifp->fp = fp;
     tifp->xsize = xsize;
     tifp->ysize = ysize;
     tifp->zsize = zsize;
     tifp->dsize = dsize;
}

bdIFile *bdIFileNew(FILE *fp, int hsize, 
		    int xsize, int ysize, int zsize, int dsize)
{
     bdIFile *tifp;
     tifp = (bdIFile *) malloc(sizeof(bdIFile));
     tifp->fp = fp;
     tifp->xsize = xsize;
     tifp->ysize = ysize;
     tifp->zsize = zsize;
     tifp->dsize = dsize;
     return(tifp);
};

void bdIFileDelete(bdIFile *ifp)
{
     if (ifp)
	  free(ifp);
     return;
}

int bdIFileSeek(bdIFile *if, int x, int y, int z)
{
     fseek(ifp->fp, ifp->hsize + 
	   (ifp->dsize * ((z * ifp->xsize * ifp->ysize) + 
	    (y * ifp->xsize) + x)), SEEK_SET);
     return(0);
}

int bdIFileReadPixel(void *pixel, int x, int y, int z, bdIFile *ifp)
{
     bdIFileSeek(ifp, x, y, z);
     return(fread(ptr, ifp->dsize, 1, ifp->fp));
}

int bdIFileRead(void *ptr, int size, bdIFile *ifp)
{
     return(fread(ptr, ifp->dsize, size, ifp->fp);
}

int bdIFileReadZ(void *ptr, int xsize, int ysize, bdIFile *ifp)
{
     unsigned int row, lastrow = ysize;
     int offset, doffset;

     doffset = ifp->dsize * xsize;
     offset  = (ifp->xsize - xsize) * ifp->dsize;
     if (offset < 0)
	  offset = 0;

     for(row = 0; row < lastrow; row++){
	  fread(ptr, ifp->dsize, xsize, ifp->fp);
	  if (offset)
	       fseek(ifp->fp, offset, SEEK_CUR);
	  prt += doffset;
     }
     
     return(0);
}

int bdIFileReadY(void *ptr, int xsize, int zsize, bdIFile *ifp)
{
     unsigned int row, lastrow = zsize;
     int offset, doffset;

     doffset = ifp->dsize * xsize;
     offset  = ((ifp->xsize * ifp->ysize) - xsize) * ifp->dsize;

     for(row = 0; row < lastrow; row++){
	  fread(ptr, ifp->dsize, xsize, ifp->fp);
	  if (offset)
	       fseek(ifp->fp, offset, SEEK_CUR);
	  prt += doffset;
     }
     
     return(0);
}


int bdIFileReadX(void *ptr, int ysize, int zsize, bdIFile *ifp)
{
     unsigned int k, j;
     int koff, joff;
     
     joff = (ifp->xsize - 1) * ifp->dsize;
     koff = ((ifp->xsize * ifp->ysize) - (ifp->ysize * xsize)) * ifp->dsize;
     koff -= joff;
	  
     for(k = 0; k < zsize; k++){
	  for(j = 0; j < ysize; j++){
	       fread(ptr, ifp->dsize, 1, ifp->fp);
	       fseek(ifp->fp, joff, SEEK_CUR);
	       ptr += ifp->dsize;
	  }
	  fseek(ifp->fp, koff, SEEK_CUR);
     }
     return(0);
}




