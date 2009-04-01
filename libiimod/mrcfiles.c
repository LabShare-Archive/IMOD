/*
 *  mrcfiles.c -- Reading and writing mrc files; high level io functions.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <time.h>
#include <math.h>
#include "mrcfiles.h"
#include "b3dutil.h"

/* These defines are OK since all I/O in file is to MRC files */
#if defined(WIN32_BIGFILE) || defined(MAC103_BIGFILE)
#define fseek b3dFseek 
#define fread b3dFread 
#define fwrite b3dFwrite 
#define rewind b3dRewind
#endif

/* 1/16/05: Back off limits for the purposes of this file, forget FLT_MIN */
#ifndef FLT_MAX
#define FLT_MAX 1.E+37F
#endif

/*
 * Header functions: DOC_SECTION HEADER
 */

/*!
 * Reads an MRC header into [hdata] from the file with pointer [fin].  
 * Determines whether byte-swapping is necessary by requiring that {nx}, {ny},
 * and {nz} are positive, that one of them is < 65536, and that {mapc}, {mapr},
 * and {maps} be between 0 and 4.  Leaves the file pointer [fin] in the {fp} 
 * member of [hdata].  Returns -1 for I/O error, or 1 if these requirements are
 * not met or if the mode or number of labels are inappropriate.
 */
int mrc_head_read(FILE *fin, MrcHeader *hdata)
{
  int i;
  int retval = 0;
  int filesize;
  int datasize;

  if (!fin)
    return(-1);
  b3dRewind(fin);
     
  if (fread(hdata, 4, 56, fin) != 56){
    b3dError(stderr, "ERROR: mrc_head_read - reading header data.\n");
    return(-1);
  }
  hdata->swapped = 0;

  /* Test for byte-swapped data with image size and the map numbers and 
     mark data as swapped if it fails */
  if (mrc_test_size(hdata))
    hdata->swapped = 1;

  /* DNM 7/30/02: test for old style header and rearrange origin info */
  if (hdata->cmap[0] != 'M' || hdata->cmap[1] != 'A' || 
      hdata->cmap[2] != 'P') {
    memcpy(&hdata->zorg, &hdata->cmap[0], 4);
    memcpy(&hdata->xorg, &hdata->stamp[0], 4);
    memcpy(&hdata->yorg, &hdata->rms, 4);
    hdata->rms = 0.;
    if (hdata->swapped)
      mrc_swap_floats(&hdata->rms, 1);
    mrc_set_cmap_stamp(hdata);
  }

  if (hdata->swapped) {
    mrc_swap_header(hdata);

    /* Test that this swapping makes values acceptable */
    /* Let calling program issue error message */
    if (mrc_test_size(hdata))
      return(1);
  }
          

  hdata->headerSize = 1024;
  hdata->sectionSkip = 0;
  /*     if (hdata->creatid == -16224){ */
  hdata->headerSize += hdata->next;
     

  for ( i = 0; i < MRC_NLABELS; i ++){
    if (fread(hdata->labels[i], MRC_LABEL_SIZE, 1, fin) == 0){  
      b3dError(stderr, "ERROR: mrc_head_read - reading label %d.\n", i);
      hdata->labels[i][MRC_LABEL_SIZE] = 0;
      return(-1);
    }
    hdata->labels[i][MRC_LABEL_SIZE] = 0;
  }

  if ((hdata->mode > 31) || (hdata->mode < 0)) {
    b3dError(stderr, "ERROR: mrc_head_read - bad file mode %d.\n",
             hdata->mode);
    return(1);
  }
  if (hdata->nlabl > MRC_NLABELS) {
    b3dError(stderr, "ERROR: mrc_head_read - impossible number of "
             "labels, %d.\n", hdata->nlabl);
    return(1);
  }

  /* DNM 7/2/02: This calculation is won't work for big files and is
     a bad idea anyway, so comment out the test below */
  datasize = hdata->nx * hdata->ny * hdata->nz;
  switch(hdata->mode){
  case MRC_MODE_BYTE:
    break;
  case MRC_MODE_SHORT:
  case MRC_MODE_USHORT:
    datasize *= 2;
    break;
  case MRC_MODE_FLOAT:
  case MRC_MODE_COMPLEX_SHORT:
    datasize *= 4;
    break;
  case MRC_MODE_COMPLEX_FLOAT:
    datasize *= 8;
    break;
  case MRC_MODE_RGB:
    datasize *= 3;
    break;
  default:
    b3dError(stderr, "ERROR: mrc_head_read - bad file mode %d.\n",
             hdata->mode);
    return(1);
  }

  /* fseek(fin, 0, 2);
     filesize = ftell(fin); */
  b3dRewind(fin);

  /* if ((filesize - datasize) < 0)
     return(0);
     if ((filesize - datasize) > 512)
     return(0); */

  hdata->fp = fin;

  return(retval);
}

/*!
 * Tests the image size and map entries in MRC header [hdata] to see if they
 * are within allowed ranges: {nx}, {ny}, and {nz} all positive, and at least 
 * one of them less than 65536; and map values between 0 and 3.  Returns 0 if
 * that is the case, 1 if not.
 */
int mrc_test_size(MrcHeader *hdata)
{
  if (hdata->nx <= 0 || hdata->ny <= 0 || hdata->nz <= 0 || 
      (hdata->nx > 65535 && hdata->ny > 65535 && hdata->nz > 65535) ||
      hdata->mapc < 0 || hdata->mapc > 4 ||
      hdata->mapr < 0 || hdata->mapr > 4 ||
      hdata->maps < 0 || hdata->maps > 4)
    return 1;
  return 0;
}

/*!
 * Write the MRC header in [hdata] to the file with pointer [fout].  Returns
 * 1 for error.
 */
int mrc_head_write(FILE *fout, MrcHeader *hdata)
{
  int i;
  MrcHeader hcopy;
  MrcHeader *hptr;
  hptr = hdata;

  if (!fout)
    return(1);

  /* DNM 6/26/02: copy the header and swap if needed */
  if (hdata->swapped) {
    hcopy = *hdata;
    hptr = &hcopy;
    mrc_swap_header(&hcopy);
  }

  rewind(fout);
     
  if (fwrite(hptr, 56, 4, fout) != 4) {
    b3dError(stderr, "ERROR: mrc_head_write - writing header to file\n");
    return 1;
  }
     
  for( i = 0; i < MRC_NLABELS; i++) {
    if (fwrite(hdata->labels[i], MRC_LABEL_SIZE, 1,fout) != 1) {
      b3dError(stderr, "ERROR: mrc_head_write - writing header to file\n");
      return 1;
    }
  }
     
  return(0);
}


/*!
 * Adds [label] to the header [hdata] or replaces the last one if there are
 * already 10 labels.  The label will be truncated at 55 characters or padded
 * with spaces to that length, and a standard date-time stamp will be added.
 * Returns 0.
 */
int mrc_head_label(MrcHeader *hdata, char *label)
{
  struct tm *tmp;
  char *outlab;
  int i, endoflabel = FALSE;
  time_t time_tval;
  int datelen = 25;

  if (hdata->nlabl >= MRC_NLABELS)
    hdata->nlabl--;
  outlab = hdata->labels[hdata->nlabl];
  for(i = 0; i < MRC_LABEL_SIZE - datelen; i++){
    if (label[i] && !endoflabel)
        outlab[i] = label[i];
    else
      endoflabel = TRUE;
    if (endoflabel)
      outlab[i] = ' ';
  }

  time_tval = time(NULL);
  tmp = localtime(&time_tval);
  strftime(&outlab[i], datelen, " %d-%b-%y  %H:%M:%S    ", tmp);
  
  hdata->nlabl++;
  return(0);
}

/*!
 * Copies all labels from header [hin] to header [hout].  Returns 0.
 */
int mrc_head_label_cp(MrcHeader *hin, MrcHeader *hout)
{
  int i, j;
     
  for (i = 0; i < hin->nlabl; i++){
    for(j = 0; j <= MRC_LABEL_SIZE; j++){
      hout->labels[i][j] = hin->labels[i][j];
    }
  }
  hout->nlabl = hin->nlabl;
  return(0);
}

/*!
 * Fills in the header structure [hdata] to default settings for the given
 * file size [x], [y], [z] and the given [mode].  Returns 0.
*/
int mrc_head_new(MrcHeader *hdata,
                 int x, int y, int z, int mode)
{

  hdata->swapped = 0;
  hdata->nx = x;
  hdata->ny = y;
  hdata->nz = z;
  hdata->mode = mode;

  hdata->nxstart = 0;
  hdata->nystart = 0;
  hdata->nzstart = 0;
  hdata->mx = hdata->nx;
  hdata->my = hdata->ny;
  hdata->mz = hdata->nz;
  hdata->xlen = hdata->nx;
  hdata->ylen = hdata->ny;
  hdata->zlen = hdata->nz;
  hdata->alpha = 90;
  hdata->beta  = 90;
  hdata->gamma = 90;
  hdata->mapc  = 1;
  hdata->mapr  = 2;
  hdata->maps  = 3;

  /* 1/16/05: use FLT_MAX instead of INT_MIN/INT_MAX for non-sgi */
  hdata->amin  = FLT_MAX;
  hdata->amax  = -FLT_MAX;
  hdata->amean = 0;
  hdata->ispg  = 0;
  hdata->nsymbt = 0;

  hdata->next    = 0;
  hdata->creatid = 1000;
  hdata->nint    = 0;
  hdata->nreal   = 0;
  hdata->sub     = 1;
  hdata->zfac    = 1;
  hdata->min2    = 0.0f;
  hdata->max2    = 0.0f;
  hdata->min3    = 0.0f;
  hdata->max3    = 0.0f;
  hdata->min4    = 0.0f;
  hdata->max4    = 0.0f;

  hdata->idtype = 0;
  hdata->lens = 0;
  hdata->nd1 = 0;
  hdata->nd2 = 0;
  hdata->vd1 = 0;
  hdata->vd2 = 0;
     
  for(x = 0; x < 6; x++)
    hdata->tiltangles[x] = 0.0f;
#ifdef OLD_STYLE_HEADER
  hdata->nwave = 0;
  hdata->wave1 = 0;
  hdata->wave2 = 0;
  hdata->wave3 = 0;
  hdata->wave4 = 0;
  hdata->wave5 = 0;
#else
  hdata->rms = 0.;
  mrc_set_cmap_stamp(hdata);
#endif
  hdata->zorg = 0.0f;
  hdata->xorg = 0.0f;
  hdata->yorg = 0.0f;
  hdata->nlabl = 0;

  /* We can't clear these values because it crashes clip 
   * and imod image load.
   */
  /*     hdata->fp = 0;    */
  /*     hdata->li = NULL; */

  hdata->headerSize = 1024;
  hdata->sectionSkip = 0;
  hdata->pathname = NULL;
  hdata->filedesc = NULL;
  hdata->userData = NULL;
     
  return(0);

}

/* DNM 12/25/00: Scale is defined as ratio of sample to cell, so change the
   nx, ny, nz below to mx, my, mz.  But also return 0 instead of 1 if cell
   size is zero; so that the mrc_set_scale will fix both cell and sample */
/* DNM 9/13/02: Invert these to correspond to all other usage */
/*!
 * Computes the pixel size or scale values in the MRC header [h] and returns
 * them in [xs], [ys], and [zs].
 */
void mrc_get_scale(MrcHeader *h, float *xs, float *ys, float *zs)
{
  *xs = *ys = *zs = 0.0f;
  if (h->xlen)
    *xs = h->xlen/(float)h->mx;
  if (h->ylen)
    *ys = h->ylen/(float)h->my;
  if (h->zlen)
    *zs = h->zlen/(float)h->mz;
}

/* DNM 12/25/00: change this 1) if 0 scale comes in, set both cell and sample
   sizes to image sizes; 2) compute xlen as mx/x scale, not nx/ x scale, etc */
/*!
 * Sets values in the MRC header [h] so that the pixel size or scale is [x], 
 * [y], and [z].  If [x] is nonzero, it sets the {xlen} element to [h] to be 
 * [x] times the {mx} element; otherwise it sets {xlen} and {mx} equal
 * to {nx}.  [y] and [z] are treated similarly.
 */
void mrc_set_scale(MrcHeader *h,
                   double x, double y, double z)
{
  if (!x) {
    h->xlen = h->nx;
    h->mx = h->nx;
  } else
    h->xlen = h->mx * x;

  if (!y) {
    h->ylen = h->ny;
    h->my = h->ny;
  } else
    h->ylen = h->my * y;

  if (!z) {
    h->zlen = h->nz;
    h->mz = h->nz;
  } else
    h->zlen = h->mz * z;

}

/*!
 * Copies the scale values, current tilt angles, and origin values from the
 * MRC header [hin] to header [hout].
 */
void mrc_coord_cp(MrcHeader *hout, MrcHeader *hin)
{
  float xs, ys, zs;

  mrc_get_scale(hin, &xs, &ys, &zs);
  mrc_set_scale(hout, xs, ys, zs);
  hout->tiltangles[3] = hin->tiltangles[3];
  hout->tiltangles[4] = hin->tiltangles[4];
  hout->tiltangles[5] = hin->tiltangles[5];
  hout->xorg = hin->xorg;
  hout->yorg = hin->yorg;
  hout->zorg = hin->zorg;
}


/*! 
 * Determines the min, max and mean values of the byte data in [idata] and 
 * places them into header [hdata].  [idata] must be an array of pointers to 
 * {hdata->nz} planes of data.  Returns -1 for error.
 */
int mrc_byte_mmm( MrcHeader *hdata, unsigned char **idata)
{

  int i, j, k;

  double min, max, mean;

  mean = 0;
  min = idata[0][0];
  max = idata[0][0];

  if (hdata == NULL)
    return (-1);

  if (idata == NULL)
    return(-1);

  for (k = 0; k < hdata->nz; k++)
    for (j = 0; j < hdata->ny; j++)
      for(i = 0; i < hdata->nx; i++){
        if (idata[k][i + (j * hdata->nx)] > max)
          max = idata[k][i + (j * hdata->nx)];

        if (idata[k][i + (j * hdata->nx)] < min)
          min = idata[k][i + (j * hdata->nx)];

        mean += idata[k][i + (j * hdata->nx)];


      }

  mean = mean / (double)(hdata->nx * hdata->ny * hdata->nz);

  hdata->amin = min;
  hdata->amean = mean;
  hdata->amax = max;
  return(0);
}

/*!
 * Swaps each section of the header [hdata] as appropriate for its data type.
 */
void mrc_swap_header(MrcHeader *hdata)
{
  mrc_swap_longs(&hdata->nx, 10);
  mrc_swap_floats(&hdata->xlen, 6);
  mrc_swap_longs(&hdata->mapc, 3);
  mrc_swap_floats(&hdata->amin, 3);
  mrc_swap_shorts(&hdata->ispg, 2);
  mrc_swap_longs(&hdata->next, 1);
  mrc_swap_shorts(&hdata->creatid, 1);
  mrc_swap_shorts(&hdata->nint, 4);
  mrc_swap_floats(&hdata->min2, 6);
  mrc_swap_shorts(&hdata->idtype, 6);
  mrc_swap_floats(&hdata->tiltangles[0], 6);
#ifdef OLD_STYLE_HEADER
  mrc_swap_shorts(&hdata->nwave, 6);
  mrc_swap_floats(&hdata->zorg, 3);
#else
  mrc_swap_floats(&hdata->xorg, 3);
  mrc_swap_floats(&hdata->rms, 1);
#endif
  mrc_swap_longs(&hdata->nlabl, 1);
}

/* DNM 7/30/02: set cmap and stamp correctly for a new header or
   for converting an old style header on reading in */
void mrc_set_cmap_stamp(MrcHeader *hdata)
{
  hdata->cmap[0] = 'M';
  hdata->cmap[1] = 'A';
  hdata->cmap[2] = 'P';
  hdata->cmap[3] = ' ';
#ifdef B3D_LITTLE_ENDIAN
  hdata->stamp[0] = hdata->swapped ? 17 : 68;
#else
  hdata->stamp[0] = hdata->swapped ? 68 : 17;
#endif
  hdata->stamp[1] = 0;
  hdata->stamp[2] = 0;
  hdata->stamp[3] = 0;
}

/*
 * Reading functions: DOC_SECTION READ_WRITE
 */

/*!
 * Returns the value of a single pixel at [x, y, z] from the file with pointer
 * [fin] given the header properties in [hdata].  Gives the value directly for
 * byte, short, and float data, and the magnitude for complex data.  Returns
 * the file minimum {hdata->amin} for coordinates out of bounds.
 */
float mrc_read_point( FILE *fin, MrcHeader *hdata, int x, int y, int z)
{
  int pixsize = 1;
  unsigned char bdata;
  b3dInt16 sdata;
  b3dInt16 sidata, srdata;
  b3dUInt16 usdata;
  float fdata = hdata->amin;
  float fidata, frdata;
  double rdata;
  int channel = 1;

  if (!fin) return(fdata);
  if (x < 0 || y < 0 || z < 0 || 
      x >= hdata->nx || y >= hdata->ny || z >= hdata->nz)
    return(fdata);

  if ((hdata->mode == MRC_MODE_SHORT) || (hdata->mode == MRC_MODE_USHORT) ||
      (hdata->mode == MRC_MODE_COMPLEX_SHORT))
    pixsize = sizeof(b3dInt16);
     
  if ((hdata->mode == MRC_MODE_FLOAT) || 
      (hdata->mode == MRC_MODE_COMPLEX_FLOAT))
    pixsize = sizeof(b3dFloat);

  if ((hdata->mode == MRC_MODE_COMPLEX_FLOAT) || 
      (hdata->mode == MRC_MODE_COMPLEX_SHORT))
    channel = 2;

  /* rewind(fin);
     fseek(fin, (hdata->headerSize + (channel * pixsize *  
     ( (z * hdata->nx * hdata->ny) + (y * hdata->nx) + (x)))),
     SEEK_CUR); */
  mrcHugeSeek(fin, hdata->headerSize + hdata->sectionSkip * z, x, y, z, 
              hdata->nx, hdata->ny, channel * pixsize, SEEK_SET);
  switch(hdata->mode){
  case MRC_MODE_BYTE:
    fread(&bdata, pixsize, 1, fin);     
    fdata = bdata;
    break;
  case MRC_MODE_SHORT:
    fread(&sdata, pixsize, 1, fin);
    if (hdata->swapped)
      mrc_swap_shorts(&sdata, 1);
    fdata = sdata;
    break;
  case MRC_MODE_USHORT:
    fread(&usdata, pixsize, 1, fin);
    if (hdata->swapped)
      mrc_swap_shorts((b3dInt16 *)&usdata, 1);
    fdata = usdata;
    break;
  case MRC_MODE_FLOAT:
    fread(&fdata, pixsize, 1, fin);
    if (hdata->swapped)
      mrc_swap_floats(&fdata, 1);
    break;
  case MRC_MODE_COMPLEX_SHORT:
    fread(&srdata, pixsize, 1, fin);
    fread(&sidata, pixsize, 1, fin);
    if (hdata->swapped) {
      mrc_swap_shorts(&srdata, 1);
      mrc_swap_shorts(&sidata, 1);
    }
    rdata = ((double)srdata * (double)srdata) + 
      ((double)sidata * (double)sidata);
    fdata = (float)sqrt(rdata);
    break;
  case MRC_MODE_COMPLEX_FLOAT:
    fread(&frdata, pixsize, 1, fin);
    fread(&fidata, pixsize, 1, fin);
    if (hdata->swapped) {
      mrc_swap_floats(&frdata, 1);
      mrc_swap_floats(&fidata, 1);
    }
    rdata = ((double)frdata * (double)frdata) + 
      ((double)fidata * (double)fidata);
    fdata = (float)sqrt(rdata);
    break;
  default:
    break;
  }

  /*     printf("Pixel %d %d %d = %g\n", x, y, z, fdata);    */

  return(fdata);
}

/*!
 * Allocates and returns one plane of data at the coordinate given by [slice]
 * along the axis given by [axis], which must be one of x, X, y, Y, z, or Z.  
 * Reads from the file with pointer [fin] according to the header in [hdata] 
 * and swaps bytes if necessary.  Returns NULL for errors.
 */
void *mrc_mread_slice(FILE *fin, MrcHeader *hdata, int slice, char axis)
{
  unsigned char *buf = NULL;
  int dsize, csize, bsize;

  switch (axis)
    {
    case 'x':
    case 'X':
      bsize = hdata->ny * hdata->nz;
      break;
               
    case 'y':
    case 'Y':
      bsize = hdata->nx * hdata->nz;
      break;
               
    case 'z':
    case 'Z':
      bsize = hdata->nx * hdata->ny;
      break;

    default:
      b3dError(stderr, "ERROR: mrc_mread_slice - axis error.\n");
      return(NULL);
    }

  if (mrc_getdcsize(hdata->mode, &dsize, &csize)){
    b3dError(stderr, "ERROR: mrc_mread_slice - unknown mode.\n");
    return(NULL);
  }
  buf = (unsigned char *)malloc(dsize * csize * bsize);
     
  if (!buf){
    b3dError(stderr, "ERROR: mrc_mread_slice - couldn't get memory.\n");
    return(NULL);
  }

  if (!mrc_read_slice(buf, fin, hdata, slice, axis))
    return((void *)buf);

  free(buf);
  return(NULL);
}

/*!
 * Reads one plane of data into the buffer [buf] at the coordinate given by 
 * [slice] along the axis given by [axis], which must be one of x, X, y, Y, z,
 * or Z.  Reads from the file with pointer [fin] according to the header in 
 * [hdata] and swaps bytes if necessary.  Should work with planes > 4 GB on
 * 64-bit systems.  Returns -1 for errors.
 */
int mrc_read_slice(void *buf, FILE *fin, MrcHeader *hdata, int slice, 
                   char axis)
{
  unsigned char *data = NULL;
  short *sdata = NULL;
  float *fdata = NULL;
  int dsize, csize, sxsize, sysize;
  b3dInt16 *sbuf = (b3dInt16 *)buf;
  b3dFloat *fbuf = (b3dFloat *)buf;

  int dcsize;
  int i,j,k;

  rewind(fin);
  fseek(fin, hdata->headerSize, SEEK_SET);
  data = (unsigned char *)buf;

  if (mrc_getdcsize(hdata->mode, &dsize, &csize)){
    b3dError(stderr, "ERROR: mrc_read_slice - unknown mode.\n");
    return(-1);
  }
  dcsize = dsize * csize;

  switch (axis){
    /* slowest loading  use z or y if possible. */
  case 'x':
  case 'X':
    sxsize = hdata->ny;
    sysize = hdata->nz;
    if (slice >= hdata->nx)
      return(-1);
    fseek( fin, slice * dcsize, SEEK_CUR);
    for(k = 0; k < hdata->nz; k++){
      for (j = 0; j < hdata->ny; j++){
        if (fread(data, dcsize, 1, fin) != 1){
          b3dError(stderr, 
                   "ERROR: mrc_read_slice x - fread error.\n");
          return(-1);
        }
        data += dcsize;
        fseek(fin, dcsize * (hdata->nx - 1), SEEK_CUR);
      }
      if (hdata->sectionSkip)
        fseek(fin, hdata->sectionSkip, SEEK_CUR);
    }
    break;

  case 'y':
  case 'Y':
    sxsize = hdata->nx;
    sysize = hdata->nz;
    if (slice >= hdata->ny)
      return(-1);
    /* fseek( fin, slice * hdata->nx * dcsize, SEEK_CUR); */
    mrcHugeSeek(fin, 0, 0, slice, 0, hdata->nx, hdata->ny, dcsize, SEEK_CUR);
    for(k = 0; k < hdata->nz; k++){
      if (fread(data, dcsize, hdata->nx, fin) != hdata->nx) {
        b3dError(stderr, "ERROR: mrc_read_slice y - fread error.\n");
        return(-1);
      }
      data += dcsize * hdata->nx;
      /*fseek(fin, dcsize * (xysize - hdata->nx), SEEK_CUR);*/
      mrcHugeSeek(fin, hdata->sectionSkip, 0, hdata->ny - 1, 0, hdata->nx,
                  hdata->ny, dcsize, SEEK_CUR);
    }
    break;
          
  case 'z':
  case 'Z':
    sxsize = hdata->nx;
    sysize = hdata->ny;
    if (slice >= hdata->nz)
      return(-1);
    /*  fseek( fin, slice * hdata->nx * hdata->ny * dcsize,
        SEEK_CUR); */
    mrcHugeSeek(fin, slice * hdata->sectionSkip, 0, 0, slice, hdata->nx,
                hdata->ny, dcsize, SEEK_CUR);
    if (fread(data, dcsize * hdata->nx, hdata->ny, fin) != hdata->ny) {
      b3dError(stderr, "ERROR: mrc_read_slice z - fread error.\n");
      return(-1);
    }
    break;
          
  default:
    b3dError(stderr, "ERROR: mrc_read_slice - axis error.\n");
    return(-1);
  }

  /* swap bytes if necessary */
  if (hdata->swapped)
    switch (hdata->mode){
    case MRC_MODE_SHORT:
    case MRC_MODE_USHORT:
    case MRC_MODE_COMPLEX_SHORT:
      for (j = 0; j < sysize; j++) {
        mrc_swap_shorts(sbuf, sxsize * csize);
        sbuf += sxsize * csize;
      }
      break;

    case MRC_MODE_FLOAT:
    case MRC_MODE_COMPLEX_FLOAT:
      for (j = 0; j < sysize; j++) {
        mrc_swap_floats(fbuf, sxsize * csize);
        fbuf += sxsize * csize;
      }
      break;

    default:
      break;
    }

  fflush(fin);
  return(0);
}

/*!
 * Reads a whole Z slice at Z value [slice] from the MRC file whose header is
 * is in [hdata], and returns it into [buf].  Works only for real data (byte,
 * integer, float).  Simply calls @mrcReadZFloat .  Returns 1 for an illegal 
 * request, 2 for a memory error, or 3 for an error reading the file.
 */
int mrcReadFloatSlice(b3dFloat *buf, MrcHeader *hdata, int slice)
{
  IloadInfo li;
  li.xmin = 0;
  li.xmax = hdata->nx - 1;
  li.ymin = 0;
  li.ymax = hdata->ny - 1;
  return (mrcReadZFloat(hdata, &li, buf, slice));
}


/* DNM 5/7/05: eliminated unused 
   void *mrc_read_image(FILE *fin, MrcHeader *hdata, int z)
   which duplicated and was inferior to mrc_mread_slice
*/
void mrc_default_status(const char *string)
{
  printf("%s", string);
  fflush(stdout);
}

/* old function compatibility. sends status to stdout. */
unsigned char **read_mrc_byte(FILE *fin,
                              MrcHeader *hdata,
                              IloadInfo *li)
{
  return(mrc_read_byte(fin, hdata, li, mrc_default_status));

}

/*!
 * Reads an MRC file into an array of unsigned bytes and returns an array of 
 * pointers to the planes of data.  [fin] is the pointer to the
 * file, [hdata] is the MRC header structure, [li] is an @@IloadInfo structure@
 * specifying the limits and scaling of the load, and [func] is a function to
 * receive a status string after each slice.  If [li] is NULL, sensible 
 * defaults are used.  Scaling is set from the {smin}, {smax}, {black}, 
 * {white} and {ramptype} members of [li] using @mrcContrastScaling , where 
 * {ramptype} can be MRC_RAMP_LIN, MRC_RAMP_LOG, or MRC_RAMP_EXP, but the
 * latter two should be used only for integer and float input data.
 * Data memory is allocated with
 * @mrcGetDataMemory and should be freed with @mrcFreeDataMemory .  Should work
 * with planes > 4 GB on 64-bit systems.  The dimensions and mode in [hdata]
 * (as well as the {mx} and {xlen} members, etc.), are modified so as to be 
 * appropriate if the data volume is written.  Returns NULL for error.
 */
unsigned char **mrc_read_byte(FILE *fin, 
                              MrcHeader *hdata, 
                              IloadInfo *li,
                              void (*func)(const char *))
{
  int  i, j, k;
  unsigned int ui;
  int bytesRead;
  size_t xysize;               /* Size of each image.       */
  int xsize, ysize, zsize;  /* Size needed to be loaded. */
  int xoff,  yoff,  zoff;   /* Offsets into image data.  */
  float conscale = 1.0f;
  float fpixel, ipixel, val;
  float kscale = mrcGetComplexScale();  /* scaling value for complex numbers */
  int seek_line, seek_endline, seek_endrow;

  short spixel = 0;
  short pixel;
  int ramptype = MRC_RAMP_LIN;
  float min;
  float max;
  float smin= 0.0f, smax = 0.0f;
  float rscale;
  int range;
  int black  = 0;   /* value of black.                                    */
  int white  = 255; /* value of white. */
  int imag = FALSE;
  int dsize;
  int doscale = 0;
  int contig = 0;
  float slope, offset, total = 0;
  float xscale, yscale, zscale;
  char statstr[128];            /* message sent to callback function. */
  unsigned char **idata;        /* image data to return. */
  unsigned char *idatap;
  unsigned char *bdata = NULL;
  b3dInt16 *sdata;
  b3dFloat *fdata;
  unsigned char *map = NULL;
  int freeMap = 0;
  b3dUInt16 *usdata;

  /* Max # of bytes in full lines before reiterating the status output */
  int statusLimit = 4000000;   

  /* check input */
  if (!fin)
    return(NULL);
  if (!hdata)
    return(NULL);
  if (li){
    xsize = li->xmax - li->xmin + 1;
    ysize = li->ymax - li->ymin + 1;
    zsize = li->zmax - li->zmin + 1;
    xoff  = li->xmin;
    yoff  = li->ymin;
    zoff  = li->zmin;
    ramptype = li->ramp;
    black = li->black;
    white = li->white;
    smin = li->smin;
    smax = li->smax;
    contig = li->contig;
  }else{
    xsize = hdata->nx;
    ysize = hdata->ny;
    zsize = hdata->nz;
    xoff  = 0;
    yoff  = 0;
    zoff  = 0;
    ramptype = MRC_RAMP_LIN;
    black = 0;
    white = 255;
    smin = smax = 0;
  }
  xysize = (size_t)xsize * (size_t)ysize;

  /*************************************/
  /* Calculate color map ramp scaling. */
     
  mrcContrastScaling(hdata, smin, smax, black, white, ramptype, &slope,
                     &offset);

  /* printf("mrc_read_byte: slope = %g offset = %g\n", slope, offset); */

  if (li){
    li->slope = slope;
    li->offset = offset;
  }

  /********************/
  /* Print some info. */
  if (func != ( void (*)(const char *) ) NULL){
    if (zsize > 1)
      sprintf(statstr, "Image size %d x %d, %d sections.\n", 
              xsize, ysize, zsize);
    else{
      sprintf(statstr,"Image size %d x %d.\n",xsize, ysize);
    }
    (*func)(statstr);
  }     

  /********************************/  
  /* Set up the data size, get scaling map if needed. */

  k = 0;
  b3dRewind(fin);
  b3dFseek(fin, hdata->headerSize, SEEK_CUR);
  switch(hdata->mode){
  case MRC_MODE_BYTE:
    dsize = 1;
    doscale = (offset <= -1.0 || offset >= 1.0 ||
               slope < 0.995 || slope > 1.005);
    if (doscale)
      map = get_byte_map(slope, offset, 0, 255);
    break;
  case MRC_MODE_SHORT:
  case MRC_MODE_USHORT:
    dsize = 2;
    map = get_short_map(slope, offset, 0, 255, ramptype, hdata->swapped, 
                        (hdata->mode == MRC_MODE_SHORT) ? 1 : 0);
    freeMap = 1;
    doscale = 1;
    break;
  case MRC_MODE_FLOAT:
    dsize = 4;
    break;
  case MRC_MODE_COMPLEX_SHORT:
    dsize = 4;
    break;
  case MRC_MODE_COMPLEX_FLOAT:
    dsize = 8;
    break;
  case MRC_MODE_RGB:
    dsize = 3;
    break;
  default:
    b3dError(stderr, "ERROR: mrc_read_byte - Unsupported data mode %i\n.",
             hdata->mode);
    return NULL;
    break;
  }
   
  /* Get the data memory */
  idata = mrcGetDataMemory(li, xysize, zsize, 1);
  if (!idata) {
    if (map && freeMap)
      free(map);
    return NULL;
  }
  if (doscale && !map) {
    b3dError(stderr, "ERROR: mrc_read_byte - Could not get memory for "
             "scaling map.\n");
    mrcFreeDataMemory(idata, contig, zsize);
    return NULL;
  }
   
  /* Get the temporary array for a line */
  bdata = (unsigned char *)malloc(dsize * xsize);
  fdata = (b3dFloat *)bdata;
  sdata = (b3dInt16 *)bdata;
  usdata = (b3dUInt16 *)bdata;
  if (!bdata) {
    b3dError(stderr, "ERROR: mrc_read_byte - Could not get memory for reading"
             " a line.");
    mrcFreeDataMemory(idata, contig, zsize);
    if (freeMap)
      free(map);
    return NULL;
  }
  
  /* compute offsets for seeking at lines and sections */
  seek_line = dsize * xoff;
  seek_endline = dsize * (hdata->nx - xoff - xsize);
  seek_endrow  = hdata->ny - yoff - ysize;
  if (zoff)
    mrcHugeSeek(fin, hdata->sectionSkip * zoff, 0, 0, zoff, hdata->nx,
                hdata->ny, dsize, SEEK_CUR);

  if (func != ( void (*)(const char *) ) NULL){
    sprintf(statstr, "\nReading Image # %3.3d",k+1); 
    (*func)(statstr);
  }

  /* Loop on sections */
  for (k = 0; k < zsize; k++){
    if (func != ( void (*)(const char *) ) NULL){
      sprintf(statstr, "\rReading Image # %3.3d",k+1); 
      (*func)(statstr);
    }

    idatap = idata[k];
    bytesRead = 0;
    if (yoff)
      mrcHugeSeek(fin, 0, 0, yoff, 0, hdata->nx, hdata->ny, dsize, SEEK_CUR);

    /* loop on lines */
    for(j = yoff; j < yoff + ysize; j++){
      if (seek_line)
        b3dFseek(fin, seek_line, SEEK_CUR);
      
      /* get a line of data, make sure it is right size */
      if (fread(bdata, dsize, xsize, fin) != xsize) {
        b3dError(stderr, "ERROR: mrc_read_byte - reading from file.");
        mrcFreeDataMemory(idata, contig, zsize);
        free(bdata);
        if (freeMap)
          free(map);
        return NULL;
      }        

      /* Do data-dependent scaling */
      switch(hdata->mode){
      case MRC_MODE_BYTE:
        if (doscale)
          for(i = 0; i < xsize; i++)
            idatap[i] = map[bdata[i]];
        else
          for(i = 0; i < xsize; i++)
            idatap[i] = bdata[i];
        break;

      case MRC_MODE_RGB:
        for(i = 0; i < xsize; i++){
          fpixel = 0.3 * bdata[i * 3];
          fpixel += 0.59 * bdata[(i * 3) + 1];
          fpixel += 0.11 * bdata[(i * 3) + 2];
          pixel = (int)(fpixel + 0.5f);
          idatap[i] = pixel;
        }
        break;
                  
      case MRC_MODE_SHORT:
      case MRC_MODE_USHORT:
        for(i = 0; i < xsize; i++){
          idatap[i] = map[usdata[i]];
        }
        break ;
                  
      case MRC_MODE_FLOAT:

        /* 5/30/08: added ramps and 0.5 for consistency with mrcfiles.c */
        if (hdata->swapped)
          mrc_swap_floats(fdata, xsize);
        for(i = 0; i < xsize; i++){
          fpixel = fdata[i];
          if (ramptype == MRC_RAMP_EXP)
            fpixel = (float)exp(fpixel);
          if (ramptype == MRC_RAMP_LOG)
            fpixel = (float)log(fpixel);
          fpixel = fpixel * slope + offset;
          if (fpixel < 0.0)
            fpixel = 0.0;
          if (fpixel > 255.0)
            fpixel = 255.0;
          idatap[i] = fpixel + 0.5f;
        }
        break ;
          
        
      case MRC_MODE_COMPLEX_SHORT:
        /* DNM 1/7/04: threw away existing scaling of one component and
           made it identical to float scaling of magnitude */
        if (hdata->swapped)
          mrc_swap_shorts(sdata, xsize * 2);
        for(i = 0; i < xsize; i++){
          fpixel = sdata[2 * i];
          ipixel = sdata[2 * i + 1];
          val = (fpixel * fpixel) + (ipixel * ipixel);
          val = (float)sqrt(val);
          val = (float)log((double)(1. + (kscale * val)));
          fpixel = val * slope + offset;
          if (fpixel < 0)
            fpixel = 0;
          if (fpixel > 255)
            fpixel = 255;
          idatap[i] = fpixel;
        }
        break ;
        
      case MRC_MODE_COMPLEX_FLOAT:
        if (hdata->swapped)
          mrc_swap_floats(fdata, xsize * 2);
        for(i = 0; i < xsize; i++){
          fpixel = fdata[2 * i];
          ipixel = fdata[2 * i + 1];
          val = (fpixel * fpixel) + (ipixel * ipixel);
          val = (float)sqrt(val);
          val = (float)log((double)(1. + (kscale * val)));
          /* DNM 2/15/01: add offset */
          fpixel = val * slope + offset;
          if (fpixel < 0)
            fpixel = 0;
          if (fpixel > 255)
            fpixel = 255;
          /* 1/7/04 remove 0.5 for consistency with mrcsec */
          idatap[i] = fpixel;/* + 0.5; */
        }
        break;
      }
      idatap += xsize;

      /* Keep track of total number of bytes on full lines that were probably
         read from disk and report status more frequently for large images so 
         3dmod can process messages */
      if (func != ( void (*)(const char *) ) NULL) {
        bytesRead += dsize * hdata->nx;
        if (bytesRead > statusLimit) {
          (*func)(statstr);
          bytesRead = 0;
        }
      }

      /* Finish line, section, free memory */
      if (seek_endline)
        b3dFseek(fin, seek_endline, SEEK_CUR);
    }
    if (seek_endrow || hdata->sectionSkip)
      mrcHugeSeek(fin, hdata->sectionSkip, 0, seek_endrow, 0, hdata->nx,
                  hdata->ny, dsize, SEEK_CUR);

  }
  free(bdata);
  if (freeMap)
    free(map);

  /* modify header as if data were to be written (?) */
  hdata->nx = xsize;
  hdata->ny = ysize;
  hdata->nz = zsize;
  hdata->mode = MRC_MODE_BYTE; 
  xscale = (hdata->mx && hdata->xlen) ? hdata->xlen / (float)hdata->mx : 1.;
  yscale = (hdata->my && hdata->ylen) ? hdata->ylen / (float)hdata->my : 1.;
  zscale = (hdata->mz && hdata->zlen) ? hdata->zlen / (float)hdata->mz : 1.;
  hdata->mx = hdata->nx;
  hdata->my = hdata->ny;
  hdata->mz = hdata->nz;
  hdata->xlen = hdata->nx * xscale;
  hdata->ylen = hdata->ny * yscale;
  hdata->zlen = hdata->nz * zscale;
     
  sprintf(statstr, "\n");
  if (func != ( void (*)(const char *) ) NULL)
    (*func)(statstr);

  return(idata);
}  

/*!
 * Computes scaling of data to bytes with potentially two levels of scaling.
 * The first level of scaling maps [smin] and [smax] to 0 to 255; if these
 * two values are equal, then the file min and max in [hdata] are used instead.
 * The second level of scaling maps [black] and [white] in these scaled values
 * to 0 to 255 to mimic the effect of black and white sliders in 3dmod.
 * [ramptype] can be MRC_RAMP_LIN, MRC_RAMP_LOG, or MRC_RAMP_EXP.  The factors
 * for scaling by pixel * slope + offset are returned in [slope] and [offset].
 * 
 */
void mrcContrastScaling(MrcHeader *hdata, float smin, float smax, int black,
                        int white, int ramptype, float *slope, float *offset)
{
  float min, max, rscale;
  int range;

  /* DNM 2/16/01: eliminate special treatment of byte mode in which black 
     and white were set to min and max while min and max were set to 0, 255,
     in order to allow double scaling in mrcbyte */

  /* set max and min. */
  max = hdata->amax;
  min = hdata->amin;

  if (smin != smax){
    max = smax;
    min = smin;
  }
  /*printf("min %f  max %f black %d white %d\n", min, max, black, white); */

  if (ramptype == MRC_RAMP_LOG){
    min = (float)log((double)min);
    max = (float)log((double)max);
  }
  if (ramptype == MRC_RAMP_EXP){
    min = (float)exp((double)min);
    max = (float)exp((double)max);
  }
  if (hdata->mode == MRC_MODE_COMPLEX_FLOAT || 
      hdata->mode == MRC_MODE_COMPLEX_SHORT)
    mrcComplexSminSmax(min, max, &min, &max);

  /* range in colormap */
  range = white - black + 1;
  if (!range) range = 1;

  /* range scale */
  rscale = 256.0 / (float)range;
     
  /* calculate slope */
  if ((max - min) != 0)
    *slope = 255.0 / (max - min);
  else
    *slope = 1.0;
     
  *slope *= rscale;
     
  /* calculate offset */
  *offset = -(( ((float)black / 255.0) * (max - min)) + min) * *slope;
}


/*
 * Write image data functions
 */

/* stupid function: check who is using it. JRK  */
/* 2 calls from clip_transform.c.  DNM - which is now abandoned */
int mrc_data_new(FILE *fout, MrcHeader *hdata)
{
  int dsize, i;
  unsigned char cdata = 0;
  short sdata = 0;
  float fdata = 0;

  /* 6/26/01: change from error message to swapping fdata */
  if (hdata->swapped) {
    mrc_swap_floats(&fdata, 1);
  }

  dsize = hdata->nx * hdata->ny * hdata->nz;
  rewind(fout);

  fseek(fout, hdata->headerSize, SEEK_CUR);

  switch(hdata->mode)
    {
    case MRC_MODE_BYTE:
      for (i = 0; i < dsize; i++)
        if (!fwrite(&cdata, 1, 1, fout))
          return(-1);
      break;

    case MRC_MODE_SHORT:
      for (i = 0; i < dsize; i++)
        if (!fwrite(&sdata, 2, 1, fout))
          return(-1);
      break;

    case MRC_MODE_FLOAT:
      for (i = 0; i < dsize; i++)
        if (!fwrite(&fdata, 4, 1, fout))
          return(-1);
      break;

    default:
      return(-1);

    }
  return(0);
}

/*!
 * Writes byte data in [data] to the file with pointer [fout] according to the 
 * dimensions in header [hdata].  [data] must be an array of pointers to 
 * {hdata->nz} planes of data.  Returns 0 (no error checks).  Unused 5/7/05.
 */
int mrc_write_byte(FILE *fout, MrcHeader *hdata, unsigned char **data)
{
  int k;
  int xysize = hdata->nx * hdata->ny;
     
  for (k = 0; k < hdata->nz; k++)
    fwrite(data[k], 1, xysize, fout);

  return(0);
}


/*!
 * Writes byte, short, or float image data to the file with pointer [fout] 
 * according to the dimensions and mode in header [hdata].  [data] must be an
 * array of pointers to {hdata->nz} planes of data.  Should be able to handle
 * planes > 4 GB.  Returns -1 for attempt to write a byte-swapped file, 0 for 
 * improper mode, -2 for a write error, or 1 for success.  Was used by mrcbyte
 * until 5/30/08.
 */
int mrc_write_idata(FILE *fout, MrcHeader *hdata, void *data[])
{
  int k, j=0, nwrote;
  unsigned char **bdata;
  b3dInt16         **sdata;
  b3dFloat         **fdata;

  if (hdata->swapped) {
    b3dError(stderr, "ERROR: mrc_write_idata - cannot write to a"
             " byte-swapped file.\n");
    return(-1);
  }



  for (k = 0; k < hdata->nz; k++) {
    switch (hdata->mode) {
    case MRC_MODE_BYTE:
      bdata = (unsigned char **)data;
      nwrote = fwrite(data[k], hdata->nx, hdata->ny, fout);
      break;
      
    case MRC_MODE_SHORT:
    case MRC_MODE_USHORT:
      sdata = (b3dInt16 **)data;
      nwrote = fwrite(&(sdata[k][j]), hdata->nx * sizeof(b3dInt16),
                      hdata->ny, fout);
      break;
      
    case MRC_MODE_FLOAT:
      fdata = (b3dFloat **)data;
      nwrote = fwrite(&(fdata[k][j]),  hdata->nx * sizeof(b3dFloat),  
                      hdata->ny, fout);
      break;
      
    default:
      b3dError(stderr, "ERROR: mrc_write_idata - unknown mode\n");
      return(0);
    }
    if (nwrote != hdata->ny) {
      b3dError(stderr, "ERROR: mrc_write_idata - Writing data to file\n");
      return(-2);
    }
  }
  return(1);
}

/*!
 * Writes one plane of data from the buffer [buf] at the coordinate given by 
 * [slice] along the axis given by [axis], which must be one of x, X, y, Y, z,
 * or Z.  Writes to the file with pointer [fout] according to the header in 
 * [hdata] and swaps bytes if necessary.  Should handle planes > 4 GB on 64-bit
 * systems.  Returns -1 for errors.
 */
int mrc_write_slice(void *buf, FILE *fout, MrcHeader *hdata, int slice,
                    char axis)
{
  int dsize, csize, retval = 0;
  size_t slicesize, sxsize, sysize;
  int i,j,k, dcsize, nx, ny;
  unsigned char *data = NULL;
  b3dInt16 *sbuf;
  b3dFloat *fbuf;

  if (!buf || slice < 0)
    return(-1);

  rewind(fout);
  fseek(fout, hdata->headerSize, SEEK_SET);
  data = (unsigned char *)buf;
  nx = hdata->nx;
  ny = hdata->ny;

  if (mrc_getdcsize(hdata->mode, &dsize, &csize)){
    b3dError(stderr, "ERROR: mrc_write_slice - unknown mode.\n");
    return(-1);
  }
  dcsize = dsize * csize;

  /* find out the actual size of the data in case swapped, and to get
     some error checks out of the way before getting memory */
  switch (axis){
  case 'x':
  case 'X':
    if (slice >= nx)
      return(-1);
    sxsize = ny;
    sysize = hdata->nz;
    break;
          
  case 'y':
  case 'Y':
    if (slice >= ny)
      return(-1);
    sxsize = nx;
    sysize = hdata->nz;
    break;
          
  case 'z':
  case 'Z':
    if (slice >= hdata->nz)
      return(-1);
    sxsize = nx;
    sysize = ny;
    break;
  default:
    b3dError(stderr, "ERROR: mrc_write_slice - axis error.\n");
    return(-1);
  }
  slicesize = sxsize * sysize;

  /* if swapped,  get memory, copy slice, and swap it in one gulp */
  if (hdata->swapped && dsize > 1) {
    data = malloc(slicesize * dcsize);
    if (!data) {
      b3dError(stderr, "ERROR: mrc_write_slice - "
               "failure to allocate memory.\n");
      return(-1);
    }
    memcpy(data, buf, slicesize * dcsize);
    sbuf = (b3dInt16 *)data;
    fbuf = (b3dFloat *)data;
    for (j = 0; j < sysize; j++) {
      if (dsize == 2) {
        mrc_swap_shorts(sbuf, sxsize * csize);
        sbuf += sxsize * csize;
      } else {
        mrc_swap_floats(fbuf, sxsize * csize);
        fbuf += sxsize * csize;
      }
    }
  }
     
  switch (axis) {
  case 'x':
  case 'X':
    fseek( fout, slice * dcsize, SEEK_CUR);
    for(k = 0; k < hdata->nz && !retval; k++) {
      for (j = 0; j < ny; j++) {
        if (fwrite(data, dcsize, 1, fout) != 1) {
          b3dError(stderr, "ERROR: mrc_write_slice x - fwrite error.\n");
          retval = -1;
          break;
        }
        data += dcsize;
        fseek(fout, dcsize * (nx - 1), SEEK_CUR);
      }
    }
    break;
      
  case 'y':
  case 'Y':
    /* fseek( fout, slice * nx * dcsize, SEEK_CUR);*/
    mrcHugeSeek(fout, 0, 0, slice, 0, nx, ny, dcsize, SEEK_CUR);
    for (k = 0; k < hdata->nz; k++) {
      if (fwrite(data, dcsize, nx, fout) != nx) {
        b3dError(stderr, "ERROR: mrc_write_slice y - fwrite error.\n");
        retval = -1;
        break;
      }
      data += dcsize * nx;
      /* fseek(fout, dcsize * (xysize - nx), SEEK_CUR); */
      mrcHugeSeek(fout, 0, 0, ny - 1, 0, nx, ny, dcsize, SEEK_CUR);
    }
    break;

  case 'z':
  case 'Z':
    /*  fseek( fout, slice * nx * ny * dcsize, SEEK_CUR); */
    mrcHugeSeek(fout, 0, 0, 0, slice, nx, ny, dcsize, SEEK_CUR);
    if (fwrite(data, dcsize * nx, ny, fout) != ny) {
      b3dError(stderr, "ERROR: mrc_write_slice z - fwrite error.\n");
      retval = -1;
    }
    break;
    
  default:
    b3dError(stderr, "ERROR: mrc_write_slice - axis error.\n");
    retval = -1;
  }
  if (hdata->swapped && dsize > 1)
    free(data);
  return(retval);
}

/*!
 * Writes one Z slice of data at Z = [slice] from the buffer [buf] to file
 * [fout] according the header in [hdata].  If parallel writing has been 
 * initialized, lines will be written to a boundary file if appropriate.
 * Returns errors from writing the slice with @mrc_write_slice and also returns
 * other non-zero values from opening the boundary file, writing its header, 
 * or writing to the file.
 */
int parallelWriteSlice(void *buf, FILE *fout, MrcHeader *hdata, int slice)
{
  static MrcHeader hbound;
  static int dsize, csize, linesBound = -1;
  static int sections[2], startLines[2];
  static FILE *fpBound;
  int err, allsec, nfiles, ib;
  char *filename;

  err = mrc_write_slice(buf, fout, hdata, slice, 'Z');
  if (err)
    return err;
  if (linesBound < 0) {
    if (parWrtProperties(&allsec, &linesBound, &nfiles)) {
      linesBound = 0;
      return 0;
    }
    mrc_head_new(&hbound, hdata->nx, linesBound, 2, hdata->mode);
    err = parWrtFindRegion(slice, 0, hdata->ny, &filename, sections, 
                           startLines);
    if (err) {
      b3dError(stdout, "ERROR: sliceWriteParallel - finding parallel writing"
               " region for slice %d (err %d)\n", slice, err);
      return err;
    }
    if (mrc_getdcsize(hdata->mode, &dsize, &csize)){
      b3dError(stdout, "ERROR: sliceWriteParallel - unknown mode.\n");
      return 1;
    }
    imodBackupFile(filename);
    fpBound = fopen(filename, "wb");
    if (!fpBound) {
      b3dError(stdout, "ERROR: sliceWriteParallel - opening boundary file %s"
               "\n", filename);
      return 1;
    }
    if (mrc_head_write(fpBound, &hbound))
      return 1;
  }

  if (!linesBound)
    return 0;
  for (ib = 0; ib < 2; ib++) {
    if (sections[ib] >= 0 && slice == sections[ib]) {
      fseek(fpBound, hbound.headerSize + ib * hbound.nx * linesBound * csize *
            dsize, SEEK_SET);
      filename = (char *)buf;
      filename += hbound.nx * startLines[ib] * csize * dsize;
      err = mrc_write_slice(filename, fpBound, &hbound, ib, 'Z');
      if (err)
        return err;
    }
  }
  return 0;
}


/*
 * Support functions: DOC_SECTION SUPPORT
 */

/*****************************/
/* Get memory for image data */
/* DNM 3/25/03: try to load contiguous if directed, then drop back to
   separate chunks to get the message about where it failed */
/* DNM 1/1/04: turn this into a routine for use in 3dmod alternate loading */
/*!
 * Allocates memory for [zsize] planes of data to contain [xysize] pixels at
 * [pixsize] bytes per pixel, and returns an array of pointers to the planes.
 * It attempts to allocate the data in contigous memory if [li] is non-NULL and
 * the {contig} element of [li] is non-zero.  If this fails it falls back to 
 * allocating planes separately, sets {contig} to 0, and issues a warning with
 * b3dError.  Should be able to allocate planes > 4GB on 64-bit systems.
 * Returns NULL for error.
 */
unsigned char **mrcGetDataMemory(IloadInfo *li, size_t xysize, int zsize,
                                 int pixsize)
{
  int contig = 0;   /* if true: load date into contiguous memory.         */
  unsigned char **idata;        /* image data to return. */
  unsigned char *bdata = NULL;
  int i;

  if (li)
    contig = li->contig;

  idata = (unsigned char **)malloc(zsize * sizeof(unsigned char *)); 
  if (!idata)
    return(NULL); 
  for (i = 0; i < zsize; i++)
    idata[i] = NULL;
     
  if (contig) {
    bdata = (unsigned char *)malloc(xysize * zsize * pixsize * 
                                    sizeof(unsigned char));
    if (!bdata) {
      b3dError(stderr, "WARNING: mrcGetDataMemory - "
               "Not enough contiguous memory to load image data.\n");
      if (li)
        li->contig = 0;
    } else {
      for (i = 0; i < zsize; i++)
        idata[i] = bdata + (xysize * i * pixsize);
      return (idata);
    }
  }

  for (i = 0; i < zsize; i++) {
    idata[i] = (unsigned char *)malloc(xysize * pixsize * 
                                       sizeof(unsigned char));
    if (!idata[i]) {
      b3dError(stderr, "ERROR: mrcGetDataMemory - Not enough memory"
               " for image data after %d sections.\n", i);

      mrcFreeDataMemory(idata, 0, zsize);
      return(NULL);
    }
  }
  return(idata);
}

/*!
 * Frees data memory allocated by @mrcGetDataMemory; [zsize] specifies the 
 * number of Z planes and [contig] indicates if the memory was allocated 
 * contiguously 
 */
void mrcFreeDataMemory(unsigned char **idata, int contig, int zsize)
{
  int i;
  if (contig)
    zsize = 1;
  for (i = 0; i < zsize; i++) {
    if (idata[i])
      free(idata[i]);
  }
  free(idata);
}

/*!
 * Returns a pointer to a lookup table of 256 scaled intensities, given a
 * scaling specified by index * [slope] + [offset].  Output values will be
 * truncated at [outmin] and [outmax] (which should be between 0 and 255).
 */
unsigned char *get_byte_map(float slope, float offset, int outmin, int outmax)
{
  static unsigned char map[256];
  int i, ival;
  float fpixel;

  for (i = 0; i < 256; i++) {
    fpixel = i;
    fpixel *= slope;
    fpixel += offset;
    ival = floor((double)fpixel + 0.5);
    if (ival < outmin)
      ival = outmin;
    if (ival > outmax)
      ival = outmax;
    map[i] = ival;
  }
  return (map);
}

/*!
 * Returns a pointer to a lookup table of scaled intensities for 65536 index
 * values, which can be either unsigned or signed short integers.  The
 * scaling will be index * [slope] + [offset] for [ramptype] = MRC_RAMP_LIN,
 * exp(index) * [slope] + [offset] for [ramptype] = MRC_RAMP_EXP, and
 * log(index) * [slope] + [offset] for [ramptype] = MRC_RAMP_LOG.
 * Output values will be * truncated at [outmin] and [outmax] (which should be
 * between 0 and 255).  Set [swapbytes] nonzero to for a table that swaps 
 * bytes, and [signedint] nonzero for signed integer indices.  The table is
 * allocated with {malloc} and should be freed by the caller.  Returns NULL for
 * error allocating memory.
 */
unsigned char *get_short_map(float slope, float offset, int outmin, int outmax,
                             int ramptype, int swapbytes, int signedint)
{
  int i, ival;
  unsigned short int index;
  float fpixel;

  unsigned char *map = (unsigned char *)malloc(65536);
  if (!map) {
    b3dError(stderr, "ERROR: get_short_map - getting memory");
    return 0;
  }
  for (i = 0; i < 65536; i++) {
    fpixel = i;
    if (i > 32767 && signedint)
      fpixel = i - 65536;
    if (ramptype == MRC_RAMP_EXP)
      fpixel = (float)exp((double)fpixel);
    if (ramptype == MRC_RAMP_LOG)
      fpixel = (float)log((double)fpixel);
    fpixel *= slope;
    fpixel += offset;
    ival = floor((double)fpixel + 0.5);
    if (ival < outmin)
      ival = outmin;
    if (ival > outmax)
      ival = outmax;
    index = i;
    if (swapbytes)
      mrc_swap_shorts((short int *)&index, 1);
    map[index] = ival;
  }
  return (map);
}

/*!
 * Returns a standard scaling factor for taking the log of complex data 
 * by log (1 + scale * value)
 */
float mrcGetComplexScale()
{
  return 5.0;
}

/*!
 * Computes the min and max for log scaling of complex numbers between [inMin] 
 * and [inMax] and returns them in [outMin] and [outMax]
 */
void mrcComplexSminSmax(float inMin, float inMax, float *outMin, 
                         float *outMax)
{
  float minSign = 1.;
  float kscale = mrcGetComplexScale();
  if (inMin < 0.) {
    minSign = -1.;
    inMin = -inMin;
  }
  *outMin = minSign * (float)(log((double)(1.0 + kscale * inMin)));
  *outMax = (float)log((double)(1.0 + kscale * inMax));
}

/*!
 * For a mirrored FFT whose full size is [nx] by [ny], computes where the
 * location [imageX], [imageY] in the image comes from in the file and returns
 * this location in [fileX], [fileY].
 */
void mrcMirrorSource(int nx, int ny, int imageX, int imageY, int *fileX,
                     int *fileY)
{
  *fileY = imageY;
  if (!imageX) {
    *fileX = nx / 2;
  } else if (imageX >= nx / 2) {
    *fileX = imageX - nx / 2;
  } else {
    *fileX = nx / 2 - imageX;
    *fileY = imageY ? ny - imageY : ny - 1;
  }
}



/*
 * Misc std I/O functions.
 */
/*!
 * Initialize the @@IloadInfo structure@ [li] with some sensible default values
 * if [hd] is NULL; otherwise it just calls @mrc_fix_li with the sizes defined
 * in the header [hd].  For full initialization, this function should be called
 * twice, not just once with a header. Returns -1 if [hd] is NULL.
 */
int mrc_init_li(IloadInfo *li, MrcHeader *hd)
{

  if (li == NULL)
    return(-1);

  /* Init li for loading in values. */
  if (hd == NULL){
    li->xmin = -1;
    li->xmax = -1;
    li->ymin = -1;
    li->ymax = -1;
    li->zmin = -1;
    li->zmax = -1;
    li->ramp = 0;
    li->black = 0;
    li->white = 255;
    li->axis = 3;
    li->mirrorFFT = 0;
    li->smin = li->smax = 0.0f;
    li->contig = 0;
    li->outmin = 0;
    li->outmax = 255;
    li->scale = 1.0f;
    li->offset = 0.0f;
    li->plist = 0;
    /* Check li values and change to default for bad data. */
  }else{
    mrc_fix_li(li, hd->nx, hd->ny, hd->nz);

  }
  return(0);
}

/*!
 * Fixes the min and max loading parameters ({xmin}, {xmax}, etc) in the
 * @@IloadInfo structure@ * [li] based on the image dimensions given either in
 * [nx], [ny], [nz] or in the {px}, {py}, and {pz} elements of [li] if there is
 * a piece list.  Undefined parameters are changed to full-range values and 
 * values are fixed to be within 0 to the dimension - 1.
 */
int mrc_fix_li(IloadInfo *li, int nx, int ny, int nz)
{
  int mx, my, mz;
  mx = nx; my = ny; mz = nz;

  /* If piece list, use image size from pxyz and adjust the loading 
   coordinates by the piece list offsets */
  if (li->plist){
    mx = (int)li->px;
    my = (int)li->py;
    mz = (int)li->pz;
    if (li->xmin != -1)
      li->xmin -= (int)li->opx;
    if (li->xmax != -1)
      li->xmax -= (int)li->opx;
    if (li->ymin != -1)
      li->ymin -= (int)li->opy;
    if (li->ymax != -1)
      li->ymax -= (int)li->opy;
    if (li->zmin != -1)
      li->zmin -= (int)li->opz;
    if (li->zmax != -1)
      li->zmax -= (int)li->opz;
  }

  /*        printf("before: x (%d, %d), y (%d, %d), z (%d, %d)\n",
            li->xmin, li->xmax, li->ymin, li->ymax, li->zmin, li->zmax);
  */
  if (li->xmax < 0)
    if ((li->xmin > 0) && (li->xmin < mx)){
      li->xmax = (mx/2) + (li->xmin/2);
      li->xmin = li->xmax - li->xmin + 1;
    }
  if ((li->xmax < 0) || (li->xmax > (mx - 1)))
    li->xmax = mx - 1;
  if ( (li->xmin < 0) || (li->xmin > li->xmax))
    li->xmin = 0;

  if (li->ymax < 0)
    if ((li->ymin > 0) && (li->ymin < my)){
      li->ymax = (my/2) + (li->ymin/2);
      li->ymin = li->ymax - li->ymin + 1;
    }
  if ((li->ymax < 0) || (li->ymax > (my - 1)))
    li->ymax = my - 1;
  if ((li->ymin < 0)  || (li->ymin > li->ymax))
    li->ymin = 0;

  /* don't let z be to big. */
  if (li->zmax >= mz)
    li->zmax = mz - 1;
  if (li->zmin >= mz)
    li->zmin = mz - 1;

  /* don't let zmax be undefined or less than zmin. */
  if ((li->zmax < 0) || (li->zmax < li->zmin)) {
    if (li->zmin >= 0)
      li->zmax = li->zmin;
    else
      li->zmax = mz - 1;
  }

  if ((li->zmin < 0)  || (li->zmin > li->zmax))
    li->zmin = 0;

  /* 7/7/06: Removed zinc entries */

  if ( (li->white > 255) || (li->white < 1))
    li->white = 255;
  if ( (li->black < 0) || (li->black > li->white))
    li->black = 0;
  if ( (li->axis > 3) ||  (li->axis < 1))
    li->axis = 3;

  /*        printf(" x (%d, %d), y (%d, %d), z (%d, %d)\n",
            li->xmin, li->xmax, li->ymin, li->ymax, li->zmin, li->zmax);
  */
  return(0);
}


/* UNused 8/2/06 */
void mrc_liso(MrcHeader *hdata, IloadInfo *li)
{
  float min, max;
  float range, rscale;

  max = hdata->amax;
  min = hdata->amin;
  if (li->ramp == MRC_RAMP_LOG){
    min = (float)log((double)hdata->amin);
    max = (float)log((double)hdata->amax);
  }
  if (li->ramp == MRC_RAMP_EXP){
    min = (float)exp((double)hdata->amin);
    max = (float)exp((double)hdata->amax);
  }
  range = li->white - li->black + 1;
  if (!range)
    range = 1;
  rscale = 256.0 / (float)range;
  if ((max - min) != 0)
    li->slope = 255.0 / (max - min);
  else
    li->slope = 1.0;
     
  li->slope *= rscale;
  li->offset = -(( ((float)li->black / 255.0) * (max - min)) + min) 
    * li->slope;
}


/*!
 * Gets x, y, and z loading limits for a MRC file whose header is in  [hdata]
 * and places them in the  @@IloadInfo structure@ * [li], using interactive
 * input from standard input.  Returns 1 for no errors.
 */
int get_loadinfo(MrcHeader *hdata, IloadInfo *li)
{
  int c;
  char line[128];

  fflush(stdout);
  fflush(stdin);
  printf (" Enter (min x, max x). (return for default) >");
     
  fgetline(stdin, line, 127);
  if (line[0])
    sscanf(line, "%d%*c%d\n", &(li->xmin), &(li->xmax)); 
  else{
    li->xmin = 0;
    li->xmax = hdata->nx - 1;
  }

  printf (" Enter (min y, max y). (return for default)  >");
  fgetline(stdin, line, 127);
  if (line[0])
    sscanf(line, "%d%*c%d\n", &(li->ymin), &(li->ymax));
  else{
    li->ymin = 0;
    li->ymax = hdata->ny - 1;
  }

  printf (" Enter sections (low, high)  >");
  fgetline(stdin, line, 127);
  if (line[0])
    sscanf(line,"%d%*c%d\n", &(li->zmin), &(li->zmax));
  else{
    li->zmin = 0;
    li->zmax = hdata->nz - 1;
  }

  li->scale = 1;
     
  return(TRUE);
}



/*****************************************************************************/
/* Old function, need to auto read tilt data from header                     */
/*****************************************************************************/

int loadtilts(struct TiltInfo *ti, MrcHeader *hdata)
{
  char filename[128];
  int i,c;
  int tiltflag = 0;
  float tiltoff, tslope;
  FILE *fin = NULL;

  while(!tiltflag){
    printf("Do you wish to load a tilt info file? (y/n) >");
    switch (c = getchar())
      {
      case 'y':
      case 'Y':
        tiltflag = 1;
        break;
      case 'n':
      case 'N':
        tiltflag = 2;
        break;
      default:
        break;
      }
    c = getchar();
  }
  ti->tilt = (float *)malloc( hdata->nz * sizeof(float));
  if (!ti->tilt)
    return(0);

  if (tiltflag == 2){
    if (hdata->nz < 2)
      ti->tilt[0] = 0;
    else{
      tiltoff = -60.0;
      tslope = 120.0 / (hdata->nz - 1.0);
      for (i = 0; i < hdata->nz; i++)
        ti->tilt[i] = tiltoff + (i * tslope);
    }
    ti->axis_z = hdata->nz / 2;
    ti->axis_x = hdata->nx / 2;
  }

  if (tiltflag == 1){
    getfilename(filename, "Enter tilt info filename. >");
    fin = fopen(filename, "r");
    if (!fin){
      b3dError(stderr, "ERROR: loadtilts - Couldn't load %s.\n", filename);
      return(0);
    }
    for (i = 0; i < hdata->nz; i++)
      fscanf(fin, "%f", &(ti->tilt[i]));
    fscanf(fin, "%f", &(ti->axis_x));
    fscanf(fin, "%f", &(ti->axis_z));
  }

  return(1);

}  



/*****************************************************************************/
/* Function getfilename - gets a name of a file using stdio.                 */
/*                                                                           */
/* Returns length of filename.                                               */
/*****************************************************************************/

int getfilename(char *name, char *prompt)
{
  int c, i;
  
  printf("%s",prompt);
  fflush(stdout);
     
  for (i = 0; i < 255 && (c = getchar())!= EOF && c!='\n'; i++)
    name[i] = c;
  name[i] = '\0';
  return i;
}


/*!
 * For the given MRC file mode in [mode], returns the number of bytes of the
 * basic data element in [dsize] and the number of data channels in [csize].
 * Returns -1 for an unsupported or undefined mode.
 */
int mrc_getdcsize(int mode, int *dsize, int *csize)
{
  switch (mode){
  case MRC_MODE_BYTE:
    *dsize = sizeof(b3dUByte);
    *csize = 1;
    break;
  case MRC_MODE_SHORT:
  case MRC_MODE_USHORT:
    *dsize = sizeof(b3dInt16);
    *csize = 1;
    break;
  case MRC_MODE_FLOAT:
    *dsize = sizeof(b3dFloat);
    *csize = 1;
    break;
  case MRC_MODE_COMPLEX_SHORT:
    *dsize = sizeof(b3dInt16);
    *csize = 2;
    break;
  case MRC_MODE_COMPLEX_FLOAT:
    *dsize = sizeof(b3dFloat);
    *csize = 2;
    break;
  case MRC_MODE_RGB:
    *dsize = sizeof(b3dUByte);
    *csize = 3;
    break;
  default:
    return(-1);
  }
  return(0);
}

/*!
 * Swaps the bytes in 16-bit integers in [data]; [amt] specifies the number of
 * values to swap.
 */
void mrc_swap_shorts(b3dInt16 *data, int amt)
{
  register unsigned char *ldata = (unsigned char *)data + (amt * 2);
  register unsigned char *ptr = (unsigned char *)data;
  register unsigned char tmp;

  while(ptr < ldata){
    tmp = *ptr;
    *ptr = ptr[1];
    ptr[1] = tmp;
    ptr+=2;
  }

}

/*!
 * Swaps the bytes in 32-bit integers in [data]; [amt] specifies the number of
 * values to swap.
 */
void mrc_swap_longs(b3dInt32 *data, int amt)
{
  register unsigned char *ldata = (unsigned char *)data + (amt * 4);
  register unsigned char *ptr = (unsigned char *)data;
  register unsigned char tmp;
  while(ptr < ldata){
    tmp = ptr[0];
    ptr[0] = ptr[3];
    ptr[3] = tmp;
    ptr++;
    tmp = *ptr;
    *ptr = ptr[1];
    ptr[1] = tmp;
    ptr+=3;
  }
}

#ifdef SWAP_IEEE_FLOATS

/* IEEE: use a copy of swap_longs to swap the bytes  */

/*!
 * Swaps the bytes in 32-bit floats in [data]; [amt] specifies the number of
 * values to swap.
 */
void mrc_swap_floats(b3dFloat *data, int amt)
{
  register unsigned char *ldata = (unsigned char *)data + (amt * 4);
  register unsigned char *ptr = (unsigned char *)data;
  register unsigned char tmp;
  while(ptr < ldata){
    tmp = ptr[0];
    ptr[0] = ptr[3];
    ptr[3] = tmp;
    ptr++;
    tmp = *ptr;
    *ptr = ptr[1];
    ptr[1] = tmp;
    ptr+=3;
  }
}

#else

#ifndef __vms

/* To convert floats from little-endian VMS to big-endian IEEE */

void mrc_swap_floats(b3dFloat *data, int amt)
{
  unsigned char exp, temp;
  int i;
  register unsigned char *ptr = (unsigned char *)data;
  register unsigned char *maxptr = (unsigned char *)data + (amt * 4);
     
  while (ptr < maxptr){

    if ((exp = (ptr[1] << 1) | (ptr[0] >> 7 & 0x01)) > 3 &&
        exp != 0)
      ptr[1] -= 1;
    else if (exp <= 3 && exp != 0)  /*must zero out the mantissa*/
      {
        /*we want manitssa 0 & exponent 1*/
        ptr[0] = 0x80;
        ptr[1] &= 0x80;
        ptr[2] = ptr[3] = 0;
      }
          
    temp = ptr[0];
    ptr[0] = ptr[1];
    ptr[1] = temp;
    temp = ptr[2];
    ptr[2] = ptr[3];
    ptr[3] = temp;
    ptr+=4;
  }
}

#else

/* If VMS: To convert floats from big-endian IEEE to little-endian VMS */

void mrc_swap_floats(fb3dFloat *data, int amt)
{
  unsigned char exp, temp;
  int i;
  register unsigned char *ptr = (unsigned char *)data;
  register unsigned char *maxptr = (unsigned char *)data + (amt * 4);
     
  while (ptr < maxptr){
    if ((exp = (ptr[0] << 1) | (ptr[1] >> 7 & 0x01)) < 253 && exp != 0)
      ptr[0] += 1;
    else if (exp >= 253) /*must also max out the exp & mantissa*/
      {
        /*we want manitssa all 1 & exponent 255*/
        ptr[0] |= 0x7F;
        ptr[1] = 0xFF;
        ptr[2] = ptr[3] = 0xFF;
      }
          
    temp = ptr[0];
    ptr[0] = ptr[1];
    ptr[1] = temp;
    temp = ptr[2];
    ptr[2] = ptr[3];
    ptr[3] = temp;
    ptr+=4;
  }
}

#endif
#endif


/*
$Log$
Revision 3.42  2009/02/16 06:16:58  mast
Add parallel write routine

Revision 3.41  2009/01/02 05:18:43  mast
const char * for Qt 4 port

Revision 3.40  2008/11/02 13:43:08  mast
Added functions for reading float slice

Revision 3.39  2008/05/31 03:09:32  mast
Added scaling routine so callers can use mrcsec with same scaling

Revision 3.38  2008/05/23 23:03:47  mast
Switched to NTSC RGB to gray scaling

Revision 3.37  2008/04/02 02:56:06  mast
Made mrc_head_read and mrc_read_byte use b3d routines to avoid seeking and
rewinding on stdin

Revision 3.36  2008/03/26 20:57:16  mast
Removed debugging statement

Revision 3.35  2008/01/11 17:18:22  mast
Mac warning cleanup

Revision 3.34  2007/09/25 15:21:36  mast
Made mrc_read_byte retain pixel size when it resets the header to new area

Revision 3.33  2007/06/13 22:52:37  mast
Modifications for reading with intersection section skip

Revision 3.32  2007/06/13 17:12:55  sueh
bug# 1019 In mrc_head_new and mrc_head_read, setting hdata->sectionSkip to 0.

Revision 3.31  2007/04/26 19:47:27  mast
Fix doc

Revision 3.30  2006/11/22 18:54:16  mast
Eliminate a warning in VC6

Revision 3.29  2006/09/28 21:15:01  mast
Changes to work with > 2Gpix and > 4 Gpix images as much as possible

Revision 3.28  2006/08/27 23:45:58  mast
Moved fgetline to b3dutil

Revision 3.27  2006/08/04 21:04:03  mast
Add documentation

Revision 3.26  2005/11/11 22:15:23  mast
Changes for unsigned file mode

Revision 3.25  2005/08/19 22:38:41  mast
Added status output to mrc_read_byte periodically during big images

Revision 3.24  2005/05/09 15:16:05  mast
Documentation - not finished

Revision 3.23  2005/02/11 01:42:33  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.22  2005/01/17 17:12:21  mast
Switched to header typedef, fixed initialization of min/max

Revision 3.21  2005/01/06 17:57:54  mast
Made label copy set label number, fixed label adding function to work on
all platforms using only time.h functions and strftime, and to replace the
last label when label list is full

Revision 3.20  2004/12/02 21:53:27  mast
Removed setting of header size to min of 1024 so raw reader can use

Revision 3.19  2004/11/04 17:10:27  mast
libiimod.def

Revision 3.18  2004/09/10 21:33:52  mast
Eliminated long variables

Revision 3.17  2004/04/22 19:14:19  mast
Added error checks when writing header

Revision 3.16  2004/01/21 00:56:57  mast
Stopped freeing map from byte_map

Revision 3.15  2004/01/17 20:37:03  mast
Remove b3d file i/o routines and mrc_big_seek to b3dutil, and add a
define for rewind

Revision 3.14  2004/01/12 17:27:00  mast
Change complex min max routine from float to void

Revision 3.13  2004/01/08 06:40:52  mast
Fixed complex scaling and rewrote mrc_read_byte to split into cases just for
processing each line

Revision 3.12  2004/01/05 17:39:08  mast
Split off memory allocation by mrc_read_byte into a separate routine
that 3dmod could use for alternate loading; moved the shifting of
load-in coordinates by piece list offset to mrc_fix_li, and renamed
imin/imax to outmin/outmax

Revision 3.11  2003/11/18 19:20:18  mast
changes for 2GB problem on Windows

Revision 3.10  2003/11/01 16:42:16  mast
changed to use new error processing routine

Revision 3.9  2003/03/28 05:08:02  mast
Use new unique little endian flag

Revision 3.8  2003/03/26 01:51:27  mast
Do not have mrc_read_byte decide when to load non-contiguous, but have it drop
back to non-contiguous when contiguous fails

Revision 3.7  2003/03/12 03:50:28  mast
Avoid trying to allocate more than 2 GB of contiguous memory, add error
message in contiguous allocation

Revision 3.6  2002/09/27 20:51:25  rickg
Added include of time.h to fix call to ctime.

Revision 3.5  2002/09/14 00:58:41  mast
Invert sense of scale factors in mrc_set_scale and mrc_get_scale to conform
to usage

Revision 3.4  2002/08/02 17:18:19  mast
Fixed bug in reading swapped files, standardized error outputs

Revision 3.3  2002/07/31 17:34:44  mast
Changes to accommodate new header format for origin values

Revision 3.2  2002/06/26 17:06:37  mast
Added type casts to calls to mrc_swap_shorts and _floats

Revision 3.1  2002/06/26 16:52:38  mast
Added ability to write header back to byte-swapped file, and to write
data to byte-swapped file with mrc_data_new and mrc_write_slice

*/
