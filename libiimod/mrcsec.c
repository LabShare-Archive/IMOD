/*  IMOD VERSION 2.42
 *
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
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

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "mrcfiles.h"

/* DNM 12/24/00: Changed the Byte_Byte and Byte_Short routines to call 
   get_byte_map and get_short_map, and use the maps for scaling the raw data */

void mrcReadZByte_Byte(struct MRCheader *hdata, struct LoadInfo *li,
		       unsigned char *buf, int cz)
{
     FILE *fin = hdata->fp;
     int  nx = hdata->nx;
     int  ny = hdata->ny;

     int  llx = li->xmin;
     int  lly = li->ymin;
     int  urx = li->xmax;
     int  ury = li->ymax;

     float slope  = li->slope;
     float offset = li->offset;
     int   imin   = li->imin;
     int   imax   = li->imax;

     int xsize = urx - llx + 1;
     int ysize = ury - lly + 1;
     int seek_line    = llx;
     int seek_endline = nx - urx - 1;
     int pindex = 0;
     int pixel, i, j;
     unsigned char *bdata = buf;
     unsigned char *map;

     /* DNM: add scaling to the loop if it's needed */
     int doscale = (offset <= -1.0 || offset >= 1.0 || 
		    slope < 0.995 || slope > 1.005);
     if (doscale)
	  map = get_byte_map(slope, offset, imin, imax);
     /* printf ("read slope %f  offset %f\n", slope, offset); */

     if (hdata->headerSize < 1024) hdata->headerSize = 1024;
     
     /* fseek(fin, hdata->headerSize + (cz * nx * ny),  SEEK_SET); */
     mrc_big_seek(fin, hdata->headerSize, cz, nx * ny,  SEEK_SET);
     if (seek_endline < 0)
	  seek_endline = 0;
     
     if (lly)
	  fseek(fin, lly * nx, SEEK_CUR);
     
     for (j = lly; j <= ury; j++){
	  if (seek_line)
	       fseek(fin, seek_line, SEEK_CUR);
	  fread(bdata, 1, xsize, fin);
	  if (doscale)
	       for (i = 0; i < xsize; i++)
		    bdata[i] = map[bdata[i]];
	  bdata += xsize;
	  if (seek_endline)
	       fseek(fin, seek_endline, SEEK_CUR);
     }
     return;
}

void mrcReadYByte_Byte(struct MRCheader *hdata, struct LoadInfo *li,
		       unsigned char *buf, int cy)
{
     FILE *fin = hdata->fp;

     int  nx = hdata->nx;
     int  ny = hdata->ny;
     int  nz = hdata->nz;

     int  llx = li->xmin;
     int  lly = li->zmin;
     int  urx = li->xmax;
     int  ury = li->zmax;

     float slope  = li->slope;
     float offset = li->offset;
     int   imin   = li->imin;
     int   imax   = li->imax;

     int xsize = urx - llx + 1;
     int ysize = ury - lly + 1;
     int seek_line    = llx;
     int seek_endline = (nx - urx - 1) + (nx * ny) - (nx);
     int pindex = 0;
     int pixel, i, j;
     unsigned char *bdata = buf;
     unsigned char *map;
     int doscale = (offset <= -1.0 || offset >= 1.0 || 
		    slope < 0.995 || slope > 1.005);
     
     if (doscale)
	  map = get_byte_map(slope, offset, imin, imax);

     if (seek_endline < 0)
	  seek_endline = 0;

     if (hdata->headerSize < 1024) hdata->headerSize = 1024;

     fseek(fin, hdata->headerSize + (cy * nx),  SEEK_SET);
    
     if (lly)
	  /* fseek(fin, lly * (nx * ny), SEEK_CUR); */
	  mrc_big_seek(fin, 0, lly, nx * ny, SEEK_CUR);
     
     for (j = lly; j <= ury; j++){
	  if (seek_line)
	       fseek(fin, seek_line, SEEK_CUR);
	  fread(bdata, 1, xsize, fin);
	  if (doscale)
	       for (i = 0; i < xsize; i++)
		    bdata[i] = map[bdata[i]];
	  bdata += xsize;
	  if (seek_endline)
	       fseek(fin, seek_endline, SEEK_CUR);
     }
     return;
}

void mrcReadZByte_Short(struct MRCheader *hdata, struct LoadInfo *li,
			unsigned char *buf, int cz)
{
     FILE *fin = hdata->fp;
     int  nx = hdata->nx;
     int  ny = hdata->ny;

     int  llx = li->xmin;
     int  lly = li->ymin;
     int  urx = li->xmax;
     int  ury = li->ymax;

     float slope  = li->slope;
     float offset = li->offset;
     int   imin   = li->imin;
     int   imax   = li->imax;
     
     int xsize = urx - llx + 1;
     int ysize = ury - lly + 1;
     int seek_line    = llx * 2;
     int seek_endline = 2 * (nx - urx - 1);
     int pindex = 0;
     int pixel, i, j;
     float fpixel;
     short *sdata;
     unsigned short *usdata;
     /* The map takes care of swapping */
     unsigned char *map= get_short_map(slope, offset, imin, imax, 
				       MRC_RAMP_LIN, hdata->swapped, 1);
     if (seek_endline < 0)
	  seek_endline = 0;
     if (hdata->headerSize < 1024) hdata->headerSize = 1024;
     /* fseek(fin, hdata->headerSize + (cz * nx * ny * 2),  SEEK_SET); */
     mrc_big_seek(fin, hdata->headerSize, cz, nx * ny * 2,  SEEK_SET);
     sdata = (short *)malloc(sizeof(short) * xsize);
     usdata = (unsigned short *)sdata;
     if (lly)
	  fseek(fin, lly * nx * 2, SEEK_CUR);
     
     for (j = lly; j <= ury; j++){
	  if (seek_line)
	       fseek(fin, seek_line, SEEK_CUR);
	  fread(sdata, sizeof(short), xsize, fin);
	  for(i = 0; i < xsize; i++, pindex++)
	       buf[pindex] = map[usdata[i]];
	  if (seek_endline)
	       fseek(fin, seek_endline, SEEK_CUR);
     }
     free(map);
     free(sdata);
}

/* DNM: minor changes to make it work */

void mrcReadYByte_Short(struct MRCheader *hdata, struct LoadInfo *li,
			unsigned char *buf, int cy)
{
     FILE *fin = hdata->fp;
     int  nx = hdata->nx;
     int  ny = hdata->ny;

     int  llx = li->xmin;
     int  lly = li->zmin;
     int  urx = li->xmax;
     int  ury = li->zmax;

     float slope  = li->slope;
     float offset = li->offset;
     int   imin   = li->imin;
     int   imax   = li->imax;
     
     int xsize = urx - llx + 1;
     int ysize = ury - lly + 1;
     int seek_line    = llx * 2;
     int seek_endline = 2 * (nx - urx - 1 + nx * ny - nx);
     int pindex = 0;
     int pixel, i, j;
     float fpixel;
     short *sdata;
     unsigned short *usdata;
     /* The map takes care of swapping */
     unsigned char *map= get_short_map(slope, offset, imin, imax, 
				       MRC_RAMP_LIN, hdata->swapped, 1);
     
     if (seek_endline < 0)
	  seek_endline = 0;

     if (hdata->headerSize < 1024) hdata->headerSize = 1024;
     fseek(fin, hdata->headerSize + (cy * nx * 2),  SEEK_SET);
     sdata = (short *)malloc(sizeof(short) * xsize);
     usdata = (unsigned short *)sdata;
     if (lly)
	  /*  fseek(fin, lly * nx * ny * 2, SEEK_CUR); */
	  mrc_big_seek(fin, 0, lly, nx * ny * 2, SEEK_CUR);
     
     for (j = lly; j <= ury; j++){
	  if (seek_line)
	       fseek(fin, seek_line, SEEK_CUR);
	  fread(sdata, sizeof(short), xsize, fin);
	  for(i = 0; i < xsize; i++, pindex++)
	       buf[pindex] = map[usdata[i]];
	  if (seek_endline)
	       fseek(fin, seek_endline, SEEK_CUR);
     }
     free(map);
     free(sdata);
}

void mrcReadZByte_Float(struct MRCheader *hdata, struct LoadInfo *li,
			       int dsize,
			       unsigned char *buf, int cz)
{
     FILE *fin = hdata->fp;
     int  nx = hdata->nx;
     int  ny = hdata->ny;
     
     int  llx = li->xmin;
     int  lly = li->ymin;
     int  urx = li->xmax;
     int  ury = li->ymax;
     
     float slope  = li->slope;
     float offset = li->offset;
     int   imin   = li->imin;
     int   imax   = li->imax;
     
     int xsize = urx - llx + 1;
     int ysize = ury - lly + 1;
     int seek_line    = llx * 4 * dsize;
     int seek_endline = dsize * 4 * (nx - urx - 1);
     int pindex = 0;
     int pixel, i, j;
     float fpixel;
     float *fdata;
     
     if (seek_endline < 0)
	  seek_endline = 0;
     if (hdata->headerSize < 1024) hdata->headerSize = 1024;
     /* fseek(fin, hdata->headerSize + 
	(cz * nx * ny * dsize * sizeof(float)),  SEEK_SET); */
     mrc_big_seek(fin, hdata->headerSize, 
	   cz, nx * ny * dsize * sizeof(float),  SEEK_SET);
     fdata = (float *)malloc(dsize * sizeof(float) * xsize);
     if (lly)
	  fseek(fin, lly * nx * dsize * sizeof(float), SEEK_CUR);
     
     for (j = lly; j <= ury; j++){
	  if (seek_line)
	       fseek(fin, seek_line, SEEK_CUR);
	  fread(fdata, sizeof(float), dsize * xsize, fin);
	  if (hdata->swapped)
	       mrc_swap_floats(fdata, dsize * xsize);
	  for(i = 0; i < xsize; i++, pindex++){
	       if (dsize == 2){
		    fpixel = sqrt((double)((fdata[i*2] * fdata[i*2]) + 
				  (fdata[(i*2)-1] * fdata[(i*2)-1])));
		    fpixel = log((double)(1.0f + (5.0f * fpixel)));
	       }else
		    fpixel = fdata[i];

	       fpixel *= slope;
	       pixel = fpixel + offset;
	       if (pixel < imin)
		    pixel = imin;
	       if (pixel > imax)
		    pixel = imax;
	       buf[pindex] = pixel;
	       
	  }
	  if (seek_endline)
	       fseek(fin, seek_endline, SEEK_CUR);
     }
     free(fdata);
}

/* DNM: minor changes to make it work; made sure sqrt and log got doubles */
void mrcReadYByte_Float(struct MRCheader *hdata, struct LoadInfo *li,
			       int dsize,
			       unsigned char *buf, int cy)
{
     FILE *fin = hdata->fp;
     int  nx = hdata->nx;
     int  ny = hdata->ny;
     
     int  llx = li->xmin;
     int  lly = li->zmin;
     int  urx = li->xmax;
     int  ury = li->zmax;
     
     float slope  = li->slope;
     float offset = li->offset;
     int   imin   = li->imin;
     int   imax   = li->imax;
     
     int xsize = urx - llx + 1;
     int ysize = ury - lly + 1;
     int seek_line    = dsize * llx * 4;
     int seek_endline = dsize * 4 * (nx - urx - 1 + nx * ny - nx);
     int pindex = 0;
     int pixel, i, j;
     float fpixel;
     float *fdata;

     if (seek_endline < 0)
	  seek_endline = 0;
     if (hdata->headerSize < 1024) hdata->headerSize = 1024;
     fseek(fin, hdata->headerSize + 
	   (cy * nx * dsize * sizeof(float)),  SEEK_SET);
     fdata = (float *)malloc(sizeof(float) * xsize * dsize);
     if (lly)
	  /* fseek(fin, lly * nx * ny * sizeof(float) * dsize, SEEK_CUR); */
	  mrc_big_seek(fin, 0, lly , nx * ny * sizeof(float) * dsize, 
		       SEEK_CUR);
     
     for (j = lly; j <= ury; j++){
	  if (seek_line)
	       fseek(fin, seek_line, SEEK_CUR);
	  fread(fdata, sizeof(float), xsize * dsize, fin);
	  if (hdata->swapped)
	       mrc_swap_floats(fdata, dsize * xsize);
	  for(i = 0; i < xsize; i++, pindex++){
	       if (dsize == 2){
		    fpixel = sqrt((double)((fdata[i*2] * fdata[i*2]) + 
				  (fdata[(i*2)-1] * fdata[(i*2)-1])));
		    fpixel = log((double)(1.0f + (5.0f * fpixel)));
	       }else
		    fpixel = fdata[i];
	       
	       fpixel *= slope;
	       pixel = fpixel + offset;
	       if (pixel < imin)
		    pixel = imin;
	       if (pixel > imax)
		    pixel = imax;
	       buf[pindex] = pixel;
	       
	  }
	  if (seek_endline)
	       fseek(fin, seek_endline, SEEK_CUR);
     }
     free(fdata);
}

/* DNM 8/28/01: add this so that midas can read rgb images and display in
   grayscale */
void mrcReadZByte_RGB(struct MRCheader *hdata, struct LoadInfo *li,
			unsigned char *buf, int cz)
{
     FILE *fin = hdata->fp;
     int  nx = hdata->nx;
     int  ny = hdata->ny;

     int  llx = li->xmin;
     int  lly = li->ymin;
     int  urx = li->xmax;
     int  ury = li->ymax;

     int xsize = urx - llx + 1;
     int ysize = ury - lly + 1;
     int seek_line    = llx * 3;
     int seek_endline = 3 * (nx - urx - 1);
     int pindex = 0;
     int pixel, i, j;
     unsigned char *sdata, *inptr;

     if (seek_endline < 0)
	  seek_endline = 0;
     if (hdata->headerSize < 1024) hdata->headerSize = 1024;

     mrc_big_seek(fin, hdata->headerSize, cz, nx * ny * 3,  SEEK_SET);
     sdata = (unsigned char *)malloc(3 * xsize);

     if (lly)
	  fseek(fin, lly * nx * 3, SEEK_CUR);
     
     for (j = lly; j <= ury; j++){
	  if (seek_line)
	       fseek(fin, seek_line, SEEK_CUR);
	  fread(sdata, 3, xsize, fin);
	  inptr = sdata;
	  for(i = 0; i < xsize; i++, pindex++) {
	       pixel = *inptr++;
	       pixel += *inptr++;
	       pixel += *inptr++;
	       buf[pindex] = pixel / 3;
	  }
	  if (seek_endline)
	       fseek(fin, seek_endline, SEEK_CUR);
     }
     free(sdata);
}


void mrcReadZByte(struct MRCheader *hdata, struct LoadInfo *li,
		  unsigned char *buf, int z)
{
     switch(hdata->mode){
	case MRC_MODE_BYTE:
	  mrcReadZByte_Byte(hdata, li, buf, z);
	  break;
	case MRC_MODE_SHORT:
	  mrcReadZByte_Short(hdata, li, buf, z);
	  break;
	case MRC_MODE_FLOAT:
	  mrcReadZByte_Float(hdata, li, 1, buf, z);
	  break;
	case MRC_MODE_COMPLEX_FLOAT:
	  mrcReadZByte_Float(hdata, li, 2, buf, z);
	  break;
	case MRC_MODE_RGB:
	  mrcReadZByte_RGB(hdata, li, buf, z);
	  break;
	default:
	  break;
     }
     return;
}

void mrcReadYByte(struct MRCheader *hdata, struct LoadInfo *li,
		  unsigned char *buf, int y)

{
     switch(hdata->mode){
	case MRC_MODE_BYTE:
	  mrcReadYByte_Byte(hdata, li, buf, y);
	  break;
	case MRC_MODE_SHORT:
	  mrcReadYByte_Short(hdata, li, buf, y);
	  break;
	case MRC_MODE_FLOAT:
	  mrcReadYByte_Float(hdata, li, 1, buf, y);
	  break;
	case MRC_MODE_COMPLEX_FLOAT:
	  mrcReadYByte_Float(hdata, li, 2, buf, y);
	  break;
	default:
	  break;
     }
     return;
}

void mrcReadSectionByte(struct MRCheader *hdata, struct LoadInfo *li,
			unsigned char *buf, int z)
{
     switch(li->axis){
	case 0:
	case 3:
	  mrcReadZByte(hdata, li, buf, z);
	  break;
	case 2:
	  mrcReadYByte(hdata, li, buf, z);
	  break;
	case 1:
	default:
	  break;
     }
}


/* DNM: added some modes and fixed obvious errors when added byte swapping */
void mrcReadZ(struct MRCheader *hdata, struct LoadInfo *li,
	      unsigned char *buf, int cz)
{
     FILE *fin = hdata->fp;
     int  nx = hdata->nx;
     int  ny = hdata->ny;
     int  pixSize = 1;
     int  llx = li->xmin;
     int  lly = li->ymin;
     int  urx = li->xmax;
     int  ury = li->ymax;
     
     float slope  = li->slope;
     float offset = li->offset;
     int   imin   = li->imin;
     int   imax   = li->imax;
     
     int xsize = urx - llx + 1;
     int ysize = ury - lly + 1;
     int pindex = 0;
     int pixel, i, j;
     int seek_line, seek_endline;


     switch(hdata->mode){
	case MRC_MODE_BYTE:
	  pixSize = 1;
	  break;
	case MRC_MODE_SHORT:
	  pixSize = 2;
	  break;
	case MRC_MODE_RGB:
	  pixSize = 3;
	  break;
	case MRC_MODE_FLOAT:
	case MRC_MODE_COMPLEX_SHORT:
	  pixSize = 4;
	  break;
	case MRC_MODE_COMPLEX_FLOAT:
	  pixSize = 8;
	  break;
	default:
	  break;
     }

     /* DNM : moved these below place where pixSize is set, and added pixSize
	to the initial seek */
     seek_line    = llx * pixSize;
     seek_endline = pixSize * (nx - urx - 1);
     
     if (hdata->headerSize < 1024) hdata->headerSize = 1024;
     fseek(fin, hdata->headerSize + (cz * nx * ny * pixSize),  SEEK_SET);
     if (seek_endline < 0)
	  seek_endline = 0;
     
     if (lly)
	  fseek(fin, lly * nx * pixSize, SEEK_CUR);
     
     for (j = lly; j <= ury; j++){
	  if (seek_line)
	       fseek(fin, seek_line, SEEK_CUR);
	  fread(buf, pixSize, xsize, fin);
	  buf += xsize * pixSize;
	  if (seek_endline)
	       fseek(fin, seek_endline, SEEK_CUR);
	  if (hdata->swapped)
	       switch(hdata->mode){
		  case MRC_MODE_SHORT:
		  case MRC_MODE_COMPLEX_SHORT:
		    mrc_swap_shorts((short *)buf, xsize * pixSize / 2);
		    break;
		  case MRC_MODE_FLOAT:
		  case MRC_MODE_COMPLEX_FLOAT:
		    mrc_swap_floats((float *)buf, xsize * pixSize / 4);
		    break;
		  default:
		    break;
	       }
     }
     return;
}

/* DNM: fixed obvious errors when added byte swapping */
void mrcReadY(struct MRCheader *hdata, struct LoadInfo *li,
			unsigned char *buf, int cy)
{
     FILE *fin = hdata->fp;
     int  nx = hdata->nx;
     int  ny = hdata->ny;
     
     int  llx = li->xmin;
     int  lly = li->ymin;
     int  urx = li->xmax;
     int  ury = li->ymax;
     short pixSize = 1;
     float slope  = li->slope;
     float offset = li->offset;
     int   imin   = li->imin;
     int   imax   = li->imax;
     
     int xsize = urx - llx + 1;
     int ysize = ury - lly + 1;
     int seek_line;
     int seek_endline;
     int pindex = 0;
     int pixel, i, j;
     int cz = 0;

     switch(hdata->mode){
	case MRC_MODE_BYTE:
	  pixSize = 1;
	  break;
	case MRC_MODE_SHORT:
	  pixSize = 2;
	  break;
	case MRC_MODE_RGB:
	  pixSize = 3;
	  break;
	case MRC_MODE_FLOAT:
	case MRC_MODE_COMPLEX_SHORT:
	  pixSize = 4;
	  break;
	case MRC_MODE_COMPLEX_FLOAT:
	  pixSize = 8;
	  break;
	default:
	  break;
     }

     /* DNM : moved these below place where pixSize is set, and added pixSize
	to the initial seek, and made seek_endline skip to next section */
     seek_line    = llx * pixSize;
     seek_endline = pixSize * (nx - urx - 1 + nx * ny - nx);

     /*     return;*/

     if (seek_endline < 0)
	  seek_endline = 0;
     if (hdata->headerSize < 1024) hdata->headerSize = 1024;
     /* fseek(fin, hdata->headerSize + (cz * nx * ny * pixSize),  SEEK_SET); */
     mrc_big_seek(fin, hdata->headerSize, cz, nx * ny * pixSize,  SEEK_SET);

     if (lly)
	  fseek(fin, lly * nx * pixSize, SEEK_CUR);
     
     for (j = lly; j <= ury; j++){
	  if (seek_line)
	       fseek(fin, seek_line, SEEK_CUR);
	  fread(buf, pixSize, xsize, fin);

	  buf += xsize*pixSize;
	  if (seek_endline)
	       fseek(fin, seek_endline, SEEK_CUR);
	  if (hdata->swapped)
	       switch(hdata->mode){
		  case MRC_MODE_SHORT:
		  case MRC_MODE_COMPLEX_SHORT:
		    mrc_swap_shorts((short *)buf, xsize * pixSize / 2);
		    break;
		  case MRC_MODE_FLOAT:
		  case MRC_MODE_COMPLEX_FLOAT:
		    mrc_swap_floats((float *)buf, xsize * pixSize / 4);
		    break;
		  default:
		    break;
	       }
     }
}


void mrcReadSection(struct MRCheader *hdata, struct LoadInfo *li,
		    unsigned char *buf, int z)
{
     switch(li->axis){
	case 0:
	case 3:
	  mrcReadZ(hdata, li, buf, z);
	  break;
	case 2:
	  mrcReadY(hdata, li, buf, z);
	  break;
	case 1:
	default:
	  break;
     }

}
