/*  
 *  mrctaper.c -- tapers edges of images where they have been filled
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
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
    Revision 3.3  2003/10/24 02:28:42  mast
    strip directory from program name and/or use routine to make backup file

    Revision 3.2  2002/11/05 23:52:15  mast
    Changed to call imodCopyright, fixed bug in outputting usage

    Revision 3.1  2002/06/26 16:50:03  mast
    Allowed writing back to byte-swapped files

*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "mrcslice.h"
#include "mrcfiles.h"
#include "b3dutil.h"

#define DEFAULT_TAPER 16
static int taper_slice(Islice *sl, int ntaper, int inside);

void mrctaper_help(char *name)
{
     fprintf(stderr,"Usage: %s [-i] [-t #] [-z min,max] input_file [output_file]\n",
	     name);
     puts("Options:");
     puts("\t-i\tTaper inside (default is outside).");
     printf("\t-t #\tTaper over the given # of pixels (default %d).\n", 
	    DEFAULT_TAPER);
     puts("\t-z min,max\tDo only sections between min and max.");
     puts("\tWith no output file, images are written back to input file.");
}

main( int argc, char *argv[] )
{

     int    i = 0;
     FILE   *fin, *fout;
     struct MRCheader hdata, hout;
     struct MRCheader *hptr;
     unsigned char *buf;
     int bsize, csize, dsize;
     int inside = 0;
     int ntaper = DEFAULT_TAPER;
     Islice slice;
     int zmin = -1, zmax = -1;
     int secofs;
     char *progname = imodProgName(argv[0]);

     if (argc < 2){
	  fprintf(stderr, 
		  "%s version %s\n", progname, VERSION_NAME);
	  imodCopyright();
	  mrctaper_help(progname);
	  exit(3);
     }

     for (i = 1; i < argc; i++)
	  if (argv[i][0] == '-')
	       switch (argv[i][1]){
		  
		  case 'i':
		    inside = 1;
		    break;

		  case 't':
		    if (argv[i][2] != 0x00)
			 sscanf(argv[i], "-t%d", &ntaper);
		    else
			 sscanf(argv[++i], "%d", &ntaper);
		    break;

		  case 'z':
		    if (argv[i][2] != 0x00)
			 sscanf(argv[i], "-z%d%*c%d", &zmin, &zmax);
		    else
			 sscanf(argv[++i], "%d%*c%d", &zmin, &zmax);
		    break;

		  default:
		    fprintf(stderr, "%s: illegal option\n", progname);
		    mrctaper_help(progname);
		    exit(1);
		    break;
		    
	       }else break;

     

     if (i < (argc - 2) || i == argc){
	  mrctaper_help(progname);
	  exit(3);	  
     }

     if (ntaper < 1 || ntaper > 127) {
	  fprintf(stderr, "%s: Taper must be between 1 and 127.\n", progname);
	  exit(3);
     }

     if (i < argc - 1)
	  fin = fopen(argv[i++], "rb");
     else
	  fin = fopen(argv[i++], "rb+");

     if (fin == NULL){
	  fprintf(stderr, "Error opening %s.\n", argv[i - 1]);
	  exit(3);
     }
     if (mrc_head_read(fin, &hdata)) {
	  fprintf(stderr, "Can't Read Input File Header.\n");
	  exit(3);
     }

     if (hdata.mode != MRC_MODE_BYTE && hdata.mode != MRC_MODE_SHORT &&
	 hdata.mode != MRC_MODE_FLOAT) {
	  fprintf(stderr, "%s: Can operate only on byte, integer and real data.\n", progname);
	  exit(3);
     }
     
     if (zmin == -1 && zmax == -1) {
	  zmin = 0;
	  zmax = hdata.nz - 1;
     } else {
	  if (zmin < 0)
	       zmin = 0;
	  if (zmax >= hdata.nz)
	       zmax = hdata.nz - 1;
     }
     
     if (i < argc){
	  fout = fopen(argv[i], "wb");
	  if (fout == NULL) {
	       fprintf(stderr, "Error opening %s.\n", argv[i]);
	       exit(3);
	  }
	  hout = hdata;
	  /* DNM: eliminate extra header info in the output, and mark it as not
	     swapped  */
	  hout.headerSize = 1024;
	  hout.next = 0;
	  hout.nint = 0;
	  hout.nreal = 0;
	  hout.nsymbt = 0;
	  hout.swapped = 0;
	  hptr = &hout;
	  hout.nz = zmax + 1 - zmin;
	  hout.mz = hout.nz;
	  hout.zlen = hout.nz;
	  secofs = zmin;
     }else{
	  /* DNM 6/26/02: it it OK now */
	  /* if (hdata.swapped) {
	       fprintf(stderr, "%s: Cannot write to byte-swapped file.\n", progname);
	       exit(3);
	       } */
	  hptr = &hdata;
	  fout = fin;
	  secofs = 0;
     }
     
     mrc_getdcsize(hdata.mode, &dsize, &csize);

     bsize = hdata.nx * hdata.ny;
     buf = (unsigned char *)malloc(dsize * csize * bsize);
     
     if (!buf){
	  fprintf(stderr, "%s: Couldn't get memory for slice.\n", progname);
	  exit(3);
     }
     mrc_slice_init(&slice, hdata.nx, hdata.ny, hdata.mode, buf);

     for (i = zmin; i <= zmax; i++) {
	  printf("\rDoing section #%4d", i);
	  fflush(stdout);
	  if (mrc_read_slice(buf, fin, &hdata, i, 'Z')) {
	       fprintf(stderr, "\nError reading section %d.\n", i);
	       exit(3);
	  }
	  
	  if (taper_slice(&slice, ntaper, inside)) {
	       fprintf(stderr, "\nError: Can't get memory for taper operation.\n");
	       exit(3);
	  }
	  	  
	  if (mrc_write_slice(buf, fout, hptr, i - secofs, 'Z')) {
	       fprintf(stderr, "\nError writing section %d.\n", i);
	       exit(3);
	  }
     }
     puts("\nDone!");

     mrc_head_label(hptr, "mrctaper: Image tapered down to fill value at edges");

     mrc_head_write(fout, hptr);

     return(0);
}


#define PLIST_CHUNK 1000

static int taper_slice(Islice *sl, int ntaper, int inside)
{
     struct pixdist {
	  unsigned short int x, y;
	  unsigned short dist;
	  signed char dx, dy;
     };

     Ival val;
     float fracs[128], fillpart[128];
     float fillval, lastval;
     int len, longest;
     int i, ix, iy, xnext, ynext, xp, yp, lastint, interval, found;
     int dir, tapdir, col, row, ncol, ind, xstart, ystart, ndiv;
     int dist;
     struct pixdist *plist;
     int pllimit = PLIST_CHUNK;
     int plsize = 0;
     unsigned char *bitmap;
     int xsize = sl->xsize;
     int ysize = sl->ysize;
     int bmsize = (xsize * ysize + 7) / 8;
     int dxout[4] = {0, 1, 0, -1};
     int dyout[4] = {-1, 0, 1, 0};
     int dxnext[4] = {1, 0, -1, 0};
     int dynext[4] = {0, 1, 0, -1};


     /* find longest string of repeated values along the edge */
     longest = 0;
     for (iy = 0; iy < ysize; iy += ysize - 1) {
	  len = 0;
	  sliceGetVal(sl, 0, iy, val);
	  lastval = val[0];
	  for (ix = 1; ix < xsize; ix++) {
	       sliceGetVal(sl, ix, iy, val);
	       if (val[0] == lastval) {
		    /* same as last, add to count, see if this is a new
		       best count*/
		    len++;
		    if (len > longest) {
			 longest = len;
			 fillval = lastval;
		    }
	       } else {
		    /* different, reset count and set new lastval */
		    len = 0;
		    lastval = val[0];
	       }
	  }
     }		    
     for (ix = 0; ix < xsize; ix += xsize - 1) {
	  len = 0;
	  sliceGetVal(sl, ix, 0, val);
	  lastval = val[0];
	  for (iy = 1; iy < ysize; iy++) {
	       sliceGetVal(sl, ix, iy, val);
	       if (val[0] == lastval) {
		    /* same as last, add to count, see if this is a new
		       best count */
		    len++;
		    if (len > longest) {
			 longest = len;
			 fillval = lastval;
		    }
	       } else {
		    /* different, reset count and set new lastval */
		    len = 0;
		    lastval = val[0];
	       }
	  }
     }		    

     /* If length below a criterion (what?) , return without error */
     if (longest < 10)
	  return 0;

     /* set the slice mean so that sliceGetValue will return this outside the
	edges */
     sl->mean = fillval;

     /* look from left edge along a series of lines to find the first point
	that is not a fill point */
     lastint = 0;
     found = 0;
     for (ndiv = 2; ndiv <= ysize; ndiv++) {
	  interval = (ysize + 2) / ndiv;
	  if (interval == lastint)
	       continue;
	  lastint = interval;
	  for (iy = interval; iy < ysize; iy += interval) {
	       for (ix = 0; ix < xsize; ix++) {
		    sliceGetVal(sl, ix, iy, val);
		    if (val[0] != fillval) {
			 found = 1;
			 break;
		    }
	       }
	       if (found)
		    break;
	  }
	  if (found)
	       break;
     }

     if (!found)
	  return 0;

     /* Get initial chunk of pixel list and bitmap */
     plist = (struct pixdist *)malloc(PLIST_CHUNK * sizeof(struct pixdist));
     if (plist)
	  bitmap = (unsigned char *)malloc(bmsize);
     if (!plist || !bitmap) {
	  if (plist)
	       free(plist);
	  return (-1);
     }

     /* clear bitmap */
     for (i = 0; i < bmsize; i++)
	  bitmap[i] = 0;

     dir = 3;
     xstart = ix;
     ystart = iy;
     tapdir = 1 - 2 * inside;

     do {
	  ncol = 1;
	  xnext = ix + dxout[dir] + dxnext[dir];
	  ynext = iy + dyout[dir] + dynext[dir];
	  sliceGetVal(sl, xnext, ynext, val);
	  if (val[0] != fillval) {
	       /* case 1: inside corner */
	       ix = xnext;
	       iy = ynext;
	       dir = (dir + 3) % 4;
	       if (inside)
		    ncol = ntaper + 1;
	  } else {
	       xnext = ix + dxnext[dir];
	       ynext = iy + dynext[dir];
	       sliceGetVal(sl, xnext, ynext, val);
	       if (val[0] != fillval) {
		    /* case 2: straight edge to next pixel */
		    ix = xnext;
		    iy = ynext;
	       } else {
		    /* case 3: outside corner, pixel stays the same */
		    dir = (dir + 1) % 4;
		    if (!inside)
			 ncol = ntaper + 1;
	       }
	  }
	  
	  /* If outside pixel is outside the data, nothing to add to lists */
	  xp = ix + dxout[dir];
	  yp = iy + dyout[dir];
	  if (xp < 0 || xp >= xsize || yp < 0 || yp >= ysize)
	       continue;

	  /* Loop on all the pixels to mark */
	  for (col = 0; col < ncol; col++) {
	       for (row = 1 - inside; row <= ntaper - inside; row++) {
		    xp = ix + tapdir * row * dxout[dir] - col * dxnext[dir];
		    yp = iy + tapdir * row * dyout[dir] - col * dynext[dir];

		    /* skip if pixel outside area */
		    if (xp < 0 || xp >= xsize || yp < 0 || yp >= ysize)
			 continue;

		    /* skip marking outside pixels for inside taper or
		       inside pixels for outside taper */
		    sliceGetVal(sl, xp, yp, val);
		    if ((inside && val[0] == fillval) || 
			(!inside && val[0] != fillval))
			 continue;

		    dist = col * col + row * row;
		    ind = xsize * yp + xp;
		    if (bitmap[ind / 8] & (1 << (ind % 8))) {

			 /* If the pixel is already marked, find it on the
			    list and see if it's closer this time */
			 for (i = plsize - 1; i >= 0; i--) {
			      if (plist[i].x == xp && plist[i].y == yp) {
				   if (plist[i].dist > dist) {
					plist[i].dist = dist;
					plist[i].dx = (signed char)(ix - xp);
					plist[i].dy = (signed char)(iy - yp);
				   }
				   break;
			      }
			 }
		    } else {

			 /* Otherwise, mark pixel in bitmap and make a new 
			    entry on the list */
			 bitmap[ind / 8] |= (1 << (ind % 8));
			 if (plsize >= pllimit) {
			      pllimit += PLIST_CHUNK;
			      plist = (struct pixdist *) realloc(plist,
					    pllimit * sizeof(struct pixdist));
			      if (!plist) {
				   free (bitmap);
				   return (-1);
			      }
			 }
			 plist[plsize].x = xp;
			 plist[plsize].y = yp;
			 plist[plsize].dist = dist;
			 plist[plsize].dx = (signed char)(ix - xp);
			 plist[plsize++].dy = (signed char)(iy - yp);
		    }
	       }
	  }

     } while (ix != xstart || iy != ystart || dir != 3);

     /* make tables of fractions and amounts to add of fillval */
     for (i = 1; i <= ntaper; i++) {
	  dist = inside ? i : ntaper + 1 - i;
	  fracs[i] = (float)dist / (ntaper + 1);
	  fillpart[i] = (1. - fracs[i]) * fillval;
     }

     /* Process the pixels on the list */
     for (i = 0; i < plsize; i++) {
	  ind = sqrt((double)plist[i].dist) + inside;
	  if (ind > ntaper)
	       continue;
	  ix = plist[i].x;
	  iy = plist[i].y;
	  if (inside) {
	       xp = ix;
	       yp = iy;
	  } else {
	       xp = ix + plist[i].dx;
	       yp = iy + plist[i].dy;
	  }
	  sliceGetVal(sl, xp, yp, val);
	  /*  val[0] = fracs[plist[i].dist] * val[0] + fillpart[plist[i].dist]; */
	  val[0] = fracs[ind] * val[0] + fillpart[ind];
	  slicePutVal(sl, ix, iy, val);
     }

     free(plist);
     free(bitmap);
     return 0;
}
