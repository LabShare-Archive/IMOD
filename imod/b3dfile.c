/*  IMOD VERSION 2.41
 *
 *  b3dfile.c -- Save graphic port image to file. Supports Tiff and SGI rgb.
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
    Revision 3.5  2004/09/10 02:31:03  mast
    replaced long with int

    Revision 3.4  2003/03/28 05:08:33  mast
    Use new unique little endian flag

    Revision 3.3  2003/02/28 19:45:35  mast
    cast int to short before putting it as short

    Revision 3.2  2003/02/10 20:29:03  mast
    autox.cpp

    Revision 3.1.2.3  2003/02/07 01:03:23  mast
    a little cleanup

    Revision 3.1.2.2  2003/01/27 00:30:07  mast
    Pure Qt version and general cleanup

    Revision 3.1.2.1  2002/12/23 04:53:51  mast
    Make routines for putting bytes, shorts, ints global and rename as iput...

    Revision 3.1  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

*/

#include <stdio.h>
#include "imodconfig.h"

#ifdef __vms
#define B3DFILE_LTLENDIAN
#else
#ifdef B3D_LITTLE_ENDIAN
#define B3DFILE_LTLENDIAN
#else
#define B3DFILE_BIGENDIAN
#endif
#endif

#include "b3dfile.h"

void iputbyte(FILE *fout, unsigned char val)
{
     unsigned char buf[1];
     
     buf[0] = val;
     fwrite(buf, 1, 1, fout);
     return;
}

void iputshort(FILE *fout, b3dUInt16 val)
{
     unsigned char buf[2];
     
     buf[0] = (unsigned char)(val >> 8);
     buf[1] = (unsigned char)(val >> 0);
     fwrite(buf, 2, 1, fout);
     return;
}

void iputlong(FILE *fout, b3dUInt32 val)
{
     unsigned char buf[4];
     
     buf[0] = (unsigned char)(val >> 24);
     buf[1] = (unsigned char)(val >> 16);
     buf[2] = (unsigned char)(val >>  8);
     buf[3] = (unsigned char)(val >>  0);
     fwrite(buf, 4, 1, fout);
     return;
}

int bdRGBWrite(FILE *fout, int xsize, int ysize, 
	       unsigned char *pixels)
{
     char iname[80];
     unsigned int xysize = xsize * ysize;
     unsigned int i;

     /* Create an SGI rgb file */
     iputshort(fout, 474);       /* MAGIC                */
     iputbyte (fout,   0);       /* STORAGE is VERBATIM  */
     iputbyte (fout,   1);       /* BPC is 1             */
     iputshort(fout,   3);       /* DIMENSION is 3       */
     iputshort(fout, (b3dInt16)xsize);     /* XSIZE                */
     iputshort(fout, (b3dInt16)ysize);     /* YSIZE                */
     iputshort(fout,   3);       /* ZSIZE                */
     iputlong (fout, 0l);        /* PIXMIN is 0          */
     iputlong (fout, 255l);      /* PIXMAX is 255        */
     iputlong (fout, 0);         /* DUMMY 4 bytes        */
     fwrite(iname, 80, 1, fout); /* IMAGENAME            */
     iputlong (fout, 0);         /* COLORMAP is 0        */
     for(i=0; i<404; i++)        /* DUMMY 404 bytes      */
	  iputbyte(fout,0);

     for (i = 0; i < xysize; i++)
	  iputbyte (fout, pixels[(i*4)+3]);
     for (i = 0; i < xysize; i++)
	  iputbyte (fout, pixels[(i*4)+2] );
     for (i = 0; i < xysize; i++)
	  iputbyte (fout, pixels[(i*4)+1]);

     return(0);
}

/* tiff stuff  */

/* DNM 12/24/00 changed long length, long offset to types below, to prevent
   compiler warnings on SGI */
static void puttiffentry(short tag, short type, 
			 int length, unsigned int offset, FILE *fout)
{
     fwrite(&tag, sizeof(b3dInt16), 1, fout);
     fwrite(&type, sizeof(b3dInt16), 1, fout);
     fwrite(&length, sizeof(b3dInt32), 1, fout);
#ifdef B3DFILE_BIGENDIAN
     if (length == 1)
	  switch(type){
	     case 1:
	       offset = offset << 24;
	       break;
	     case 3:
	       offset = offset << 16;
	       break;
     }
#endif
     fwrite(&offset, sizeof(b3dInt16), 1, fout);
     return;
}


int bdTIFFWriteMap(FILE *fout, int xsize, int ysize,
		   unsigned char *pixels, unsigned short *cmap)
{
     unsigned int pixel, ifd;
     unsigned int  xysize = xsize * ysize;
     int pad, tenum;

#ifdef B3DFILE_BIGENDIAN
     pixel = 0x4D4D002A;
#else
     pixel = 0x002A4949;
#endif

     fwrite(&pixel, 4, 1, fout);
     ifd = xysize;
     pad = -(ifd % 4) + 4;
     ifd += pad + 4;
     if (!fwrite(&ifd, 4, 1, fout))
	  return(-1);

     fwrite(pixels, 1, xysize, fout);

     fwrite(&pixel, 4, 1, fout);


     

     /* write TIFF entries. */
     fseek(fout, (int)ifd, SEEK_SET);
     tenum = 12; /* set to number of tiff entries. */
     fwrite(&tenum, 2, 1, fout);
     
     puttiffentry(256, 3, 1, xsize, fout);
     puttiffentry(257, 3, 1, ysize, fout);
     puttiffentry(258, 3, 1, 8, fout);  /* pixel size  1x8  */
     puttiffentry(259, 3, 1, 1, fout);  /* compression , none*/
     
     puttiffentry(262, 3, 1, 3, fout);  /* cmap */
     puttiffentry(273, 4, 1, 8, fout);  /* image data start */
     puttiffentry(274, 3, 1, 1, fout);
     puttiffentry(277, 3, 1, 1, fout);  /* samples / pixel */
     puttiffentry(278, 4, 1, ysize, fout);
     puttiffentry(279, 4, 1, xysize, fout);
     puttiffentry(284, 3, 1, 1, fout);  /* plane config */
     
     puttiffentry(320, 3, 768, ifd + 2 + (tenum * 12), fout);
     fwrite(cmap, 2, 768, fout);

     return(0);
}
