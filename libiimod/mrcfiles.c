/*  IMOD VERSION 2.50
 *
 *  mrcfiles.c -- Reading and writing mrc files; high level io functions.
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
/*  $Author$

$Date$

$Revision$

Log at end of file */

#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>
#ifdef __unix
#include <sys/time.h>
#endif
#include <math.h>
#include "mrcfiles.h"
#include "b3dutil.h"

/* These defines are OK since all I/O in file is to MRC files */
#ifdef WIN32_BIGFILE
#include <io.h>
#define fseek b3dFseek 
#define fread b3dFread 
#define fwrite b3dFwrite 
#endif

#ifndef FLT_MAX
#define FLT_MAX 3.40282347E+38F
#define FLT_MIN 1.17549435E-38F
#endif

/*
 * Header functions
 */

int mrc_head_read(FILE *fin, struct MRCheader *hdata)
{
  int i;
  int retval = 0;
  long filesize;
  long datasize;

  if (!fin)
    return(-1);
  rewind(fin);
     
     
  if (fread(hdata, 56, 4, fin) == 0){
    b3dError(stderr, "ERROR: mrc_head_read - reading header data.\n");
    return(-1);
  }

  hdata->swapped = 0;

  /* Test for byte-swapped data with image size and the map numbers */
  if (hdata->nx <= 0 || hdata->nx > 60000 ||
      hdata->ny <= 0 || hdata->ny > 60000 ||
      hdata->nz <= 0 || hdata->nz > 60000 ||
      hdata->mapc < 0 || hdata->mapc > 4 ||
      hdata->mapr < 0 || hdata->mapr > 4 ||
      hdata->maps < 0 || hdata->maps > 4) {

    /* Mark data as swapped*/
    hdata->swapped = 1;
  }

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
    if (hdata->nx <= 0 || hdata->nx > 60000 ||
        hdata->ny <= 0 || hdata->ny > 60000 ||
        hdata->nz <= 0 || hdata->nz > 60000 ||
        hdata->mapc < 0 || hdata->mapc > 4 ||
        hdata->mapr < 0 || hdata->mapr > 4 ||
        hdata->maps < 0 || hdata->maps > 4) {
      return(1);
    }
  }
          

  hdata->headerSize = 1024;
  /*     if (hdata->creatid == -16224){ */
  hdata->headerSize += hdata->next;
     

  for ( i = 0; i < MRC_NLABELS; i ++){
    if (fread(hdata->labels[i], MRC_LABEL_SIZE, 1, fin) == 0){  
      b3dError(stderr, "ERROR: mrc_head_read - reading label %d.\n",
               i);
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
  case 0:
    break;
  case 1:
    datasize *= 2;
    break;
  case 2:
  case 3:
    datasize *= 4;
    break;
  case 4:
    datasize *= 8;
    break;
  case 16:
    datasize *= 3;
    break;
  default:
    b3dError(stderr, "ERROR: mrc_head_read - bad file mode %d.\n",
             hdata->mode);
    return(1);
  }
  fseek(fin, 0, 2);
  filesize = ftell(fin);
  rewind(fin);

  /* if ((filesize - datasize) < 0)
     return(0);
     if ((filesize - datasize) > 512)
     return(0); */

  hdata->fp = fin;

  return(retval);
}



int mrc_head_write(FILE *fout, struct MRCheader *hdata)
{
  int i;
  struct MRCheader hcopy;
  struct MRCheader *hptr;
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
     
  fwrite(hptr, 56, 4, fout);
     
  for( i = 0; i < MRC_NLABELS; i++)
    fwrite(hdata->labels[i], MRC_LABEL_SIZE, 1,fout);
     
  return(0);
     
}


/* Add a label to the header */
/* WARNING: doesn't check for string length > 80 */
int mrc_head_label(struct MRCheader *hdata, char *label)
{

#ifdef __unix
  struct timeval tp;
  struct timezone tzp;
  char *time;
  int i, endoflabel = FALSE;

  if (hdata->nlabl < (MRC_NLABELS - 1)){
    gettimeofday(&tp, &tzp);
    for(i = 0; i < MRC_LABEL_SIZE; i++){
      if (label[i])
        hdata->labels[hdata->nlabl][i] = label[i];
      else
        endoflabel = TRUE;
      if (endoflabel)
        hdata->labels[hdata->nlabl][i] = ' ';
    }
    i = 55;
    time = ctime(&(tp.tv_sec));
    hdata->labels[hdata->nlabl][i++] = ' ';
    hdata->labels[hdata->nlabl][i++] = time[8];
    hdata->labels[hdata->nlabl][i++] = time[9];
    hdata->labels[hdata->nlabl][i++] = '-';
    hdata->labels[hdata->nlabl][i++] = time[4];
    hdata->labels[hdata->nlabl][i++] = time[5];
    hdata->labels[hdata->nlabl][i++] = time[6];
    hdata->labels[hdata->nlabl][i++] = '-';
    hdata->labels[hdata->nlabl][i++] = time[22];
    hdata->labels[hdata->nlabl][i++] = time[23];
    hdata->labels[hdata->nlabl][i++] = ' ';
    hdata->labels[hdata->nlabl][i++] = ' ';
    hdata->labels[hdata->nlabl][i++] = time[11];
    hdata->labels[hdata->nlabl][i++] = time[12];
    hdata->labels[hdata->nlabl][i++] = time[13];
    hdata->labels[hdata->nlabl][i++] = time[14];
    hdata->labels[hdata->nlabl][i++] = time[15];
    hdata->labels[hdata->nlabl][i++] = time[16];
    hdata->labels[hdata->nlabl][i++] = time[17];
    hdata->labels[hdata->nlabl][i++] = time[18];
    /*        hdata->labels[hdata->nlabl][i++] = '\n'; */
    hdata->labels[hdata->nlabl][i++] = ' ';
    hdata->labels[hdata->nlabl][i++] = ' ';
    hdata->labels[hdata->nlabl][i++] = ' ';
    hdata->labels[hdata->nlabl][i++] = ' ';
    hdata->labels[hdata->nlabl][i++] = 0x00;
    hdata->nlabl++;
    return(0);
  }
  return(-1);

#else
  return(0);
#endif

}

int mrc_head_label_cp(struct MRCheader *hin, struct MRCheader *hout)
{
  int i, j;
     
  for (i = 0; i < hin->nlabl; i++){
    for(j = 0; j <= MRC_LABEL_SIZE; j++){
      hout->labels[i][j] = hin->labels[i][j];
    }
  }
  return(0);
}

/* Fills in the header structure to default settings. */
int mrc_head_new(struct MRCheader *hdata,
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
#ifdef __sgi
  hdata->amin  = FLT_MAX;
  hdata->amax  = FLT_MIN;
#else
  hdata->amin = INT_MAX;
  hdata->amax = INT_MIN;
#endif
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
  hdata->pathname = NULL;
  hdata->filedesc = NULL;
  hdata->userData = NULL;
     
  return(0);

}


/* fills in min, max and mean structures in header */
int mrc_byte_mmm( struct MRCheader *hdata, unsigned char **idata)
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

/* Swap each section of the header as appropriate for its data type */
void mrc_swap_header(struct MRCheader *hdata)
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
void mrc_set_cmap_stamp(struct MRCheader *hdata)
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
 * Write image data functions
 */

/* stupid function: check who is using it. JRK  */
/* 2 calls from clip_transform.c.  DNM */
int mrc_data_new(FILE *fout, struct MRCheader *hdata)
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

  if (hdata->headerSize < 1024) hdata->headerSize = 1024;
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

/* changed 8-95 to increase speed: JRK */
int mrc_write_byte(FILE *fout, struct MRCheader *hdata, unsigned char **data)
{
  int k;
  int xysize = hdata->nx * hdata->ny;
     
  for (k = 0; k < hdata->nz; k++)
    fwrite(data[k], 1, xysize, fout);

  /*                  for (j = 0 ; j < xysize; j++)
                      fputc(bdata[k][j], fout);*/

  return(0);
}


/* writes image data */
/* changed 8-95 to increase speed: JRK */
mrc_write_idata(FILE *fout, struct MRCheader *hdata, void *data[])
{
  int i,j=0,k;
  long  xysize;
  unsigned char **bdata;
  short         **sdata;
  float         **fdata;

  if (hdata->swapped) {
    b3dError(stderr, "ERROR: mrc_write_idata - cannot write to a"
             " byte-swapped file.\n");
    return(-1);
  }

  xysize = hdata->nx * hdata->ny;


  switch (hdata->mode)
    {
    case MRC_MODE_BYTE:
      bdata = (unsigned char **)data;
      for (k = 0; k < hdata->nz; k++)
        fwrite(data[k], 1, xysize, fout);
      break;

    case MRC_MODE_SHORT:
      sdata = (short **)data;
      for (k = 0; k < hdata->nz; k++)
        fwrite(&(sdata[k][j]), sizeof(short), xysize, fout);
      break;

    case MRC_MODE_FLOAT:
      fdata = (float **)data;
      for (k = 0; k < hdata->nz; k++)
        fwrite(&(fdata[k][j]), sizeof(float), xysize, fout);
      break;

    default:
      b3dError(stderr, "ERROR: mrc_write - unknown mode\n");
      return(0);
    }

  return(xysize * hdata->nz);
}


float mrc_read_point( FILE *fin, struct MRCheader *hdata, int x, int y, int z)
{
  int pixsize = 1;
  unsigned char bdata;
  short sdata;
  short sidata, srdata;
  float fdata = hdata->amin;
  float fidata, frdata;
  double rdata;
  int channel = 1;

  if (!fin) return(fdata);
  if (x < 0)
    return(fdata);
  if (y < 0)
    return(fdata);
  if (x >= hdata->nx)
    return(fdata);
  if (y >= hdata->ny)
    return(fdata);

  if ((hdata->mode == MRC_MODE_SHORT) ||
      (hdata->mode == MRC_MODE_COMPLEX_SHORT))
    pixsize = sizeof(short);
     
  if ((hdata->mode == MRC_MODE_FLOAT) || 
      (hdata->mode == MRC_MODE_COMPLEX_FLOAT))
    pixsize = sizeof(float);

  if ((hdata->mode == MRC_MODE_COMPLEX_FLOAT) || 
      (hdata->mode == MRC_MODE_COMPLEX_SHORT))
    channel = 2;

  rewind(fin);
  if (hdata->headerSize < 1024) hdata->headerSize = 1024;
  /*     fseek(fin, (hdata->headerSize + (channel * pixsize *  
         ( (z * hdata->nx * hdata->ny) + (y * hdata->nx) + (x)))),
         SEEK_CUR); */
  mrc_big_seek(fin, hdata->headerSize + channel * pixsize *  
               (y * hdata->nx + x), channel * pixsize * z, hdata->nx * hdata->ny,
               SEEK_CUR);
  switch(hdata->mode){
  case 0:
    fread(&bdata, pixsize, 1, fin);     
    fdata = bdata;
    break;
  case 1:
    fread(&sdata, pixsize, 1, fin);
    if (hdata->swapped)
      mrc_swap_shorts(&sdata, 1);
    fdata = sdata;
    break;
  case 2:
    fread(&fdata, pixsize, 1, fin);
    if (hdata->swapped)
      mrc_swap_floats(&fdata, 1);
    break;
  case 3:
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
  case 4:
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



void *mrc_mread_slice(FILE *fin, struct MRCheader *hdata,
                      int slice, char axis)
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

int mrc_read_slice(void *buf, FILE *fin, struct MRCheader *hdata, 
                   int slice, char axis)
{
  unsigned char *data = NULL;
  short *sdata = NULL;
  float *fdata = NULL;
  int dsize, csize;

  /* DNM 2/23/01: computed slicesize to make the byte swapping work right */
  int xysize, slicesize;
  int i,j,k;

  rewind(fin);
  if (hdata->headerSize < 1024) hdata->headerSize = 1024;
  fseek(fin, hdata->headerSize, SEEK_SET);
  xysize = hdata->nx * hdata->ny;
  data = (unsigned char *)buf;

  if (mrc_getdcsize(hdata->mode, &dsize, &csize)){
    b3dError(stderr, "ERROR: mrc_read_slice - unknown mode.\n");
    return(-1);
  }

  switch (axis){
    /* slowest loading  use z or y if possible. */
  case 'x':
  case 'X':
    slicesize = hdata->nz * hdata->ny;
    if (slice >= hdata->nx)
      return(-1);
    fseek( fin, slice * dsize * csize, SEEK_CUR);
    for(k = 0; k < hdata->nz; k++){
      for (j = 0; j < hdata->ny; j++){
        if (fread(data, dsize * csize, 1, fin) != 1){
          b3dError(stderr, 
                   "ERROR: mrc_read_slice x - fread error.\n");
          return(-1);
        }
        data += dsize * csize;
        fseek(fin, dsize * csize * (hdata->nx - 1), SEEK_CUR);
      }
    }
    break;

  case 'y':
  case 'Y':
    slicesize = hdata->nx * hdata->nz;
    if (slice >= hdata->ny)
      return(-1);
    fseek( fin, slice * hdata->nx * dsize * csize, SEEK_CUR);
    for(k = 0; k < hdata->nz; k++){
      if (fread(data, dsize * csize, hdata->nx, fin) != 
          hdata->nx){
        b3dError(stderr, 
                 "ERROR: mrc_read_slice y - fread error.\n");
        return(-1);
      }
      data += dsize * csize * hdata->nx;
      fseek(fin, dsize * csize * (xysize - hdata->nx), SEEK_CUR);
    }
    break;
          
  case 'z':
  case 'Z':
    slicesize = xysize;
    if (slice >= hdata->nz)
      return(-1);
    /*  fseek( fin, slice * hdata->nx * hdata->ny * dsize * csize,
        SEEK_CUR); */
    mrc_big_seek( fin, 0, slice, hdata->nx * hdata->ny * dsize * csize,
                  SEEK_CUR);
    if (fread(data, (dsize * csize), xysize, fin) != xysize){
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
    case MRC_MODE_COMPLEX_SHORT:
      mrc_swap_shorts((short int *)buf, slicesize * csize);
      break;

    case MRC_MODE_FLOAT:
    case MRC_MODE_COMPLEX_FLOAT:
      mrc_swap_floats((float *)buf, slicesize * csize);
      break;

    default:
      break;
    }

  fflush(fin);
  return(0);
}


int mrc_write_slice(void *buf, FILE *fout, struct MRCheader *hdata, 
                    int slice, char axis)
{
  int dsize, csize = 1;
  int xysize, slicesize;
  int i,j,k;
  unsigned char *data = NULL;

  if (!buf || slice < 0)
    return(-1);

  rewind(fout);
  if (hdata->headerSize < 1024) hdata->headerSize = 1024;
  fseek(fout, hdata->headerSize, SEEK_SET);
  xysize = hdata->nx * hdata->ny;
  data = (unsigned char *)buf;

  if (mrc_getdcsize(hdata->mode, &dsize, &csize)){
    b3dError(stderr, "ERROR: mrc_write_slice - unknown mode.\n");
    return(-1);
  }

  /* find out the actual size of the data in case swapped, and to get
     some error checks out of the way before getting memory */
  switch (axis){
  case 'x':
  case 'X':
    if (slice >= hdata->nx)
      return(-1);
    slicesize = hdata->nz * hdata->ny;
    break;
          
  case 'y':
  case 'Y':
    if (slice >= hdata->ny)
      return(-1);
    slicesize = hdata->nx * hdata->nz;
    break;
          
  case 'z':
  case 'Z':
    if (slice >= hdata->nz)
      return(-1);
    slicesize = xysize;
    break;
  default:
    b3dError(stderr, "ERROR: mrc_write_slice - axis error.\n");
    return(-1);
  }
     

  /* if swapped,  get memory, copy slice, and swap it in one gulp */
  if (hdata->swapped && dsize > 1) {
    data = malloc(slicesize * dsize * csize);
    if (!data) {
      b3dError(stderr, "ERROR: mrc_write_slice - "
               "failure to allocate memory.\n");
      return(-1);
    }
    memcpy(data, buf, slicesize * dsize * csize);
    if (dsize == 2)
      mrc_swap_shorts((short int *)data, slicesize * csize);
    else
      mrc_swap_floats((float *)data, slicesize * csize);
  }
     
  switch (axis)
    {
    case 'x':
    case 'X':
      fseek( fout, slice * dsize * csize, SEEK_CUR);
      for(k = 0; k < hdata->nz; k++){
        for (j = 0; j < hdata->ny; j++){
          if (fwrite(data, dsize * csize, 1, fout) != 1){
            b3dError(stderr, "ERROR: mrc_write_slice x"
                     " - fwrite error.\n");
            if (hdata->swapped && dsize > 1)
              free(data);
            return(-1);
          }
          data += dsize * csize;
          fseek(fout, csize * dsize * (hdata->nx - 1),  
                SEEK_CUR);
        }
      }
      break;

    case 'y':
    case 'Y':
      fseek( fout, slice * hdata->nx * csize * dsize, SEEK_CUR);
      for(k = 0; k < hdata->nz; k++){
        if (fwrite(data, dsize * csize, hdata->nx, fout) != 
            hdata->nx){
          b3dError(stderr, 
                   "ERROR: mrc_write_slice y - fwrite error.\n");
          return(-1);
          if (hdata->swapped && dsize > 1)
            free(data);
        }
        data += dsize * hdata->nx * csize;
        fseek(fout, csize * dsize * (xysize - hdata->nx), 
              SEEK_CUR);
      }
      break;

    case 'z':
    case 'Z':
      /*  fseek( fout, slice * hdata->nx * hdata->ny * csize * dsize, 
          SEEK_CUR); */
      mrc_big_seek( fout, 0, slice, hdata->nx * hdata->ny * 
                    csize * dsize, SEEK_CUR);
      if (fwrite(data, dsize * csize, xysize, fout) != xysize){
        b3dError(stderr, 
                 "ERROR: mrc_write_slice z - fwrite error.\n");
        if (hdata->swapped && dsize > 1)
          free(data);
        return(-1);
      }
      break;
               
    default:
      b3dError(stderr, "ERROR: mrc_write_slice - axis error.\n");
      return(-1);
    }
  if (hdata->swapped && dsize > 1)
    free(data);
  return(0);
}


void *mrc_read_image(FILE *fin, struct MRCheader *hdata, int z)
{

  unsigned char *cdata;
  short *sdata;
  float *fdata;
  long xysize;
     
  rewind(fin);
  if (hdata->headerSize < 1024) hdata->headerSize = 1024;
  fseek(fin, hdata->headerSize, SEEK_CUR);
  xysize = hdata->nx * hdata->ny;
     
     
  switch (hdata->mode)
    {
    case MRC_MODE_BYTE:
      /* fseek(fin, z * xysize * sizeof(char), SEEK_CUR); */
      mrc_big_seek(fin, 0, z, xysize * sizeof(char), SEEK_CUR);
      cdata = (unsigned char *)malloc(xysize * sizeof(char));
      if (cdata == NULL)
        return(cdata);
      fread(cdata, sizeof(char), xysize, fin);
      return(cdata);
               
    case MRC_MODE_SHORT:
      /* fseek(fin, z * xysize * sizeof(short), SEEK_CUR); */
      mrc_big_seek(fin, 0, z , xysize * sizeof(short), SEEK_CUR);
      sdata = (short *)malloc(xysize * sizeof(short));
      if (sdata == NULL)
        return(sdata);
      fread(sdata, sizeof(short), xysize, fin);
      if (hdata->swapped)
        mrc_swap_shorts(sdata, xysize);
      return(sdata);
               
    case MRC_MODE_FLOAT:
      /* fseek(fin, z * xysize * sizeof(float), SEEK_CUR); */
      mrc_big_seek(fin, 0, z, xysize * sizeof(float), SEEK_CUR);
      fdata = (float *)malloc(xysize * sizeof(float));
      if (fdata == NULL)
        return(fdata);
      fread(fdata, sizeof(float), xysize, fin);
      if (hdata->swapped)
        mrc_swap_floats(fdata, xysize);
      return(fdata);
               
    default:
      return(NULL);
    }
}

/*****************************************************************************/
/* functions get_byte_map and get_short_map: Return pointer to an array of   */
/* unsigned char's that can be used to look up scaled intensities            */
/* Input:  slope, offset  are the scaling factors; out = in * slope + offset */
/*         imin, imax  are the minimum and maximum values for output         */
/*         ramptype (for get_short_map) should be MRC_RAMP_LIN for linear    */
/*         swapbytes (for get_short_map) should indicate whether swapping is */
/*                    needed, and it will be taken care of by swapping map   */
/*                    indexes                                                */
/*         signedint (for get_short_map) should be 0 if ints are unsigned    */
/* The caller to get_short_map should free the map before the next call      */
/*****************************************************************************/

unsigned char *get_byte_map(float slope, float offset, int imin, int imax)
{
  static unsigned char map[256];
  int i, ival;
  float fpixel;

  for (i = 0; i < 256; i++) {
    fpixel = i;
    fpixel *= slope;
    fpixel += offset;
    ival = floor((double)fpixel + 0.5);
    if (ival < imin)
      ival = imin;
    if (ival > imax)
      ival = imax;
    map[i] = ival;
  }
  return (map);
}

unsigned char *get_short_map(float slope, float offset, int imin, int imax,
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
    if (ival < imin)
      ival = imin;
    if (ival > imax)
      ival = imax;
    index = i;
    if (swapbytes)
      mrc_swap_shorts((short int *)&index, 1);
    map[index] = ival;
  }
  return (map);
}

/*****************************************************************************/
/* function read_mrc_byte: reads an mrc file into arrary of unsigned bytes.  */
/* Input:  FILE      *fin - pointer to mrcfile to be read in.                */
/*         MRCheader *hdata - pointer to header data read in from *fin.      */
/*         LoadInfo  *li  -  pointer to data needed for loading instructions.*/
/*                           can be set to NULL for default load settings.   */
/* Returns: (unsigned char **)filled with the image data.                    */
/*****************************************************************************/


void mrc_default_status(char *string)
{
  printf("%s", string);
  fflush(stdout);
}

/* old function compatibility. sends status to stdout. */
unsigned char **read_mrc_byte(FILE *fin,
                              struct MRCheader *hdata,
                              struct LoadInfo *li)
{
  return(mrc_read_byte(fin, hdata, li, mrc_default_status));

}



unsigned char **mrc_read_byte(FILE *fin, 
                              struct MRCheader *hdata, 
                              struct LoadInfo *li,
                              void (*func)(char *))
{
  int  i, j, k;
  unsigned int ui;
  int  pindex;
  unsigned long xysize;               /* Size of each image.       */
  long xsize, ysize, zsize;  /* Size needed to be loaded. */
  long xoff,  yoff,  zoff;   /* Offsets into image data.  */
  float conscale = 1.0f;
  float fpixel, ipixel, val;
  float kscale = 5.0;       /* scaling value for complex numbers */
  int seek_head, seek_line, seek_row, seek_endline, seek_endrow;

  short spixel = 0;
  short pixel;
  int ramptype = MRC_RAMP_LIN;
  float min;
  float max;
  float smin= 0.0f, smax = 0.0f;
  float rscale;
  int contig = 0;   /* if true: load date into contiguous memory.         */
  int range;
  int black  = 0;   /* value of black.                                    */
  int white  = 255; /* value of white. */
  int imag = FALSE;
  int dsize;
  int doscale;
  float slope, offset, total = 0;
  char statstr[128];            /* message sent to callback function. */
  unsigned char **idata;        /* image data to return. */
  unsigned char *bdata = NULL;
  short *sdata;
  float *fdata;
  unsigned char *map;
  unsigned short *usdata;

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
    imag = li->imaginary;
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
  xysize = xsize * ysize;

  /*****************************/
  /* Get memory for image data */
  /* DNM 3/25/03: try to load contiguous if directed, then drop back to
     separate chunks to get the message about where it failed */

  idata = (unsigned char **)malloc(zsize * sizeof(unsigned char *)); 
  if (idata == (unsigned char **)NULL)
    return((unsigned char **)NULL); 
  for (i = 0; i < zsize; i++)
    idata[i] = NULL;
     
  if (contig){
    bdata = malloc(xysize * zsize * sizeof(unsigned char));
    if (!bdata) {
      b3dError(stderr, "WARNING: mrc_read_byte - "
               "Not enough contiguous memory to load image data.\n");
      contig = 0;
      if (li)
        li->contig = 0;
    } else {
      for(ui = 0; ui < zsize; ui++)
        idata[ui] = bdata + (xysize * ui);
    }
  }

  if (!contig) {
    for(i = 0; i < zsize; i++){
      idata[i] = (unsigned char *)malloc(xysize * sizeof(unsigned char));
      if (!idata[i]){
        b3dError(stderr, "ERROR: mrc_read_byte - Not enough memory"
                 " for image data after %d sections.\n", i);

        for (i = 0; i < zsize; i++)
          if (idata[i])
            free(idata[i]);
        free(idata);
        return(NULL);
      }
    }
  }

  /*************************************/
  /* Calculate color map ramp scaling. */
     
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
  if (hdata->mode == MRC_MODE_COMPLEX_FLOAT){
    min = (float)log((double)(1. + kscale * min));  
    max = (float)log((double)(1. + kscale * max));  
  }
  /* range in colormap */
  range = white - black + 1;
  if (!range) range = 1;

  /* range scale */
  rscale = 256.0 / (float)range;
     
  /* calculate slope */
  if ((max - min) != 0)
    slope = 255.0 / (max - min);
  else
    slope = 1.0;
     
  slope *= rscale;
     
  /* calculate offset */
  offset = -(( ((float)black / 255.0) * (max - min)) + min) * slope;

  /* printf("mrc_read_byte: slope = %g offset = %g\n", slope, offset); */

  if (li){
    li->slope = slope;
    li->offset = offset;
  }

  /********************/
  /* Print some info. */
  if (func != ( void (*)() ) NULL){
    if (zsize > 1)
      sprintf(statstr, "Image size %d x %d, %d sections.\n", 
              xsize, ysize, zsize);
    else{
      sprintf(statstr,"Image size %d x %d.\n",xsize, ysize);
    }
    (*func)(statstr);
  }     

  /********************************/  
  /* Read image data into memory. */

  k = 0;
  rewind(fin);
  if (hdata->headerSize < 1024) hdata->headerSize = 1024;
  fseek(fin, hdata->headerSize, SEEK_CUR);
  switch(hdata->mode){
  case MRC_MODE_SHORT:
    dsize = 2;
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
    dsize = 1;
    break;
  }
          
  seek_head = dsize * zoff * hdata->nx * hdata->ny;
  seek_row  = dsize * yoff * hdata->nx;
  seek_line = dsize * xoff;
  seek_endline = dsize * (hdata->nx - xoff - xsize);
  seek_endrow  = dsize * (hdata->ny - yoff - ysize) * hdata->nx;
  /* moved this up from all the individual cases below */
  if (seek_head)
    /* fseek(fin, seek_head ,SEEK_CUR); */
    mrc_big_seek(fin, 0, dsize * zoff, hdata->nx * hdata->ny ,SEEK_CUR);

  switch(hdata->mode){
  case MRC_MODE_RGB:
  case MRC_MODE_BYTE:
    dsize = 1;
    doscale = (offset <= -1.0 || offset >= 1.0 ||
               slope < 0.995 || slope > 1.005);
    if (MRC_MODE_RGB == hdata->mode)
      dsize = 3;
    else if(doscale)
      map = get_byte_map(slope, offset, 0, 255);

    if (func != ( void (*)() ) NULL){
      sprintf(statstr, "\nReading Image # %3.3d",k+1); 
      (*func)(statstr);
    }
    bdata = (unsigned char *)malloc(dsize * xsize);
    for (k = 0; k < zsize; k++){
      if (func != ( void (*)() ) NULL){
        sprintf(statstr, "\rReading Image # %3.3d\0",k+1); 
        (*func)(statstr);
      }
      pindex = 0;
      if (seek_row)
        fseek(fin, seek_row, SEEK_CUR);
      for(j = yoff; j < yoff + ysize; j++){
        if (seek_line)
          fseek(fin, seek_line, SEEK_CUR);

        fread(bdata, 1, (xsize * dsize), fin);
        if (MRC_MODE_RGB == hdata->mode){
          for(i = 0; i < xsize; i++,pindex++){
            fpixel = bdata[i * 3];
            fpixel += bdata[(i * 3) + 1];
            fpixel += bdata[(i * 3) + 2];
            fpixel /= 3;
            pixel = fpixel + 0.5;
            idata[k][pindex] = pixel;
          }
        }else{
          if (doscale)
            for(i = 0; i < xsize; i++,pindex++)
              idata[k][pindex] = map[bdata[i]];
          else
            for(i = 0; i < xsize; i++,pindex++)
              idata[k][pindex] = bdata[i];
        }
        if (seek_endline)
          fseek(fin, seek_endline, SEEK_CUR);
      }
      if (seek_endrow)
        fseek(fin, seek_endrow, SEEK_CUR);
    }
    free(bdata);
    break;
                  
                  
  case MRC_MODE_SHORT:
    sdata = (short *)malloc(sizeof(short) * xsize);
    usdata = (unsigned short *)sdata;
    map = get_short_map(slope, offset, 0, 255, ramptype, hdata->swapped,
                        1);
    for (k = 0; k < zsize; k++){
      if (func != ( void (*)() ) NULL){
        sprintf(statstr, "\rReading Image # %3.3d\0",k+1); 
        (*func)(statstr);
      }
      pindex = 0;                     
      if (seek_row)
        fseek(fin, seek_row, SEEK_CUR);
      for (j = yoff; j < yoff + ysize; j++){
        if (seek_line)
          fseek(fin, seek_line, SEEK_CUR);
        fread(sdata, sizeof(short), xsize, fin);
        for(i = 0; i < xsize; i++, pindex++){
          idata[k][pindex] = map[usdata[i]];
        }
        if (seek_endline)
          fseek(fin, seek_endline, SEEK_CUR);
      }
      if (seek_endrow)
        fseek(fin, seek_endrow, SEEK_CUR);
    }
    free(sdata);
    free(map);
    break ;
          
                  
  case MRC_MODE_FLOAT:
    if (func != ( void (*)() ) NULL){
      sprintf(statstr,"\nReading Image # %3.3d\0",k+1);
      (*func)(statstr);
    }
    fdata = (float *)malloc(sizeof(float) * xsize);
    for (k = 0; k < zsize; k++){
      if (func != ( void (*)() ) NULL){
        sprintf(statstr, "\rReading Image # %3.3d",k+1); 
        (*func)(statstr);
      }
      min *= conscale;
      if (seek_row)
        fseek(fin, seek_row, SEEK_CUR);
      pindex = 0;
      for(j = yoff; j < yoff + ysize; j++){
        if (seek_line)
          fseek(fin, seek_line, SEEK_CUR);
        fread(fdata, sizeof(float), xsize, fin);
        if (hdata->swapped)
          mrc_swap_floats(fdata, xsize);
        for(i = 0; i < xsize; i++, pindex++){
          fpixel = fdata[i];
          if (ramptype == MRC_RAMP_EXP)
            fpixel = (float)exp(fpixel);
          if (ramptype == MRC_RAMP_LOG)
            fpixel = (float)log(fpixel);
          fpixel *= slope;
          fpixel += offset;
          if (fpixel < 0.0)
            fpixel = 0.0;
          if (fpixel > 255.0)
            fpixel = 255.0;
          idata[k][pindex] = fpixel + 0.5;
        }
        if (seek_endline)
          fseek(fin, seek_endline, SEEK_CUR);
      }
      if (seek_endrow)
        fseek(fin, seek_endrow, SEEK_CUR);
    }
    free(fdata);
    break ;
          
                  
  case MRC_MODE_COMPLEX_SHORT:
    if (imag)
      fread( &spixel, 2, 1, fin);
    for (k = 0; k < zsize; k++){
      if (func != ( void (*)() ) NULL){
        sprintf(statstr, "\rReading Image # %3.3d\0",k+1); 
        (*func)(statstr);
      }
      pindex = 0;                     
      if (seek_row)
        fseek(fin, seek_row, SEEK_CUR);
      for (j = yoff; j < yoff + ysize; j++){
        if (seek_line)
          fseek(fin, seek_line, SEEK_CUR);
                    
        for(i = xoff; i < xoff + xsize; i++, pindex++){
          fread(&spixel, 2, 1, fin);
          if (hdata->swapped)
            mrc_swap_shorts(&spixel, 1);
          fpixel = spixel;
          if (ramptype == MRC_RAMP_EXP)
            fpixel = (float)exp((float)fpixel);
          if (ramptype == MRC_RAMP_LOG)
            fpixel = (float)log((float)fpixel);
                         
          fpixel *= slope;
          fpixel += offset;
          if (fpixel < 0)
            fpixel = 0;
          if (fpixel > 255)
            fpixel = 255;
          idata[k][pindex] = fpixel + 0.5;
          fread( &spixel, 2, 1, fin);
        }
        if (seek_endline)
          fseek(fin, seek_endline, SEEK_CUR);
      }
      if (seek_endrow)
        fseek(fin, seek_endrow, SEEK_CUR);
    }
    break ;
          
          
  case MRC_MODE_COMPLEX_FLOAT:
    fdata = (float *)malloc(sizeof(float) * xsize * 2);
    if (func != ( void (*)() ) NULL){
      sprintf(statstr,"\nReading Image # %3.3d\0",k+1);
      (*func)(statstr);
    }
    /* DNM 2/15/01: use scaling from above */
    /*
      kscale = 5;
      slope = (float)log(1 + (kscale * hdata->amax));
      if (!slope)
      slope = 1;
      else
      slope = 255 / slope;
    */
    for (k = 0; k < zsize; k++){
      if (func != ( void (*)() ) NULL){
        sprintf(statstr, "\rReading Image # %3.3d",k+1); 
        (*func)(statstr);
      }
      min *= conscale;
      if (seek_row)
        fseek(fin, seek_row, SEEK_CUR);
      pindex = 0;
      for(j = yoff; j < yoff + ysize; j++){
        if (seek_line)
          fseek(fin, seek_line, SEEK_CUR);

        /* DNM: switched to reading whole line at once and
           processing, when implemented swapping */
        fread(fdata, 4, xsize * 2, fin);
        if (hdata->swapped)
          mrc_swap_floats(fdata, xsize * 2);
        for(i = 0; i < xsize; i++, pindex++){
          fpixel = fdata[2 * i];
          ipixel = fdata[2 * i + 1];
          val = (fpixel * fpixel) + (ipixel * ipixel);
          val = (float)sqrt(val);
          val = (float)log(1 + (kscale * val));
          /* DNM 2/15/01: add offset */
          fpixel = val * slope + offset;
          if (fpixel < 0)
            fpixel = 0;
          if (fpixel > 255)
            fpixel = 255;
          idata[k][pindex] = fpixel + 0.5;
        }
        if (seek_endline)
          fseek(fin, seek_endline, SEEK_CUR);
      }
      if (seek_endrow)
        fseek(fin, seek_endrow, SEEK_CUR);
    }
    free(fdata);
    break ;
          
  default:
    b3dError(stderr, "ERROR: mrc_read - Unknown Data Type\n");
    return(NULL);
  }

  hdata->nx = xsize;
  hdata->ny = ysize;
  hdata->nz = zsize;
  hdata->mode = MRC_MODE_BYTE; 
  hdata->mx = hdata->nx;
  hdata->my = hdata->ny;
  hdata->mz = hdata->nz;
  hdata->xlen = hdata->nx;
  hdata->ylen = hdata->ny;
  hdata->zlen = hdata->nz;
     
  sprintf(statstr, "\n");
  if (func != ( void (*)() ) NULL)
    (*func)(statstr);

  return(idata);
}  



/*
 * Misc std I/O functions.
 */

int mrc_fix_li(struct LoadInfo *li, int nx, int ny, int nz)
{
  int mx, my, mz;
  mx = nx; my = ny; mz = nz;
  if (li->plist){
    mx = li->px;
    my = li->py;
    mz = li->pz;
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
  if ((li->zmax < 0) || (li->zmax < li->zmin))
    if (li->zmin >= 0)
      li->zmax = li->zmin;
    else
      li->zmax = mz - 1;

  if ((li->zmin < 0)  || (li->zmin > li->zmax))
    li->zmin = 0;

  if (li->zinc < 1)
    li->zinc = 1;

  if (li->zinc > li->zmax)
    li->zinc = li->zmax;
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

int mrc_init_li(struct LoadInfo *li, struct MRCheader *hd)
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
    li->zinc = 1;
    li->ramp = 0;
    li->black = 0;
    li->white = 255;
    li->axis = 3;
    li->smin = li->smax = 0.0f;
    li->contig = 0;
    li->imin = 0;
    li->imax = 255;
    li->scale = 1.0f;
    li->offset = 0.0f;
    li->plist = 0;
    /* Check li values and change to default for bad data. */
  }else{
    mrc_fix_li(li, hd->nx, hd->ny, hd->nz);

  }
  return(0);
}


void mrc_liso(struct MRCheader *hdata, struct LoadInfo *li)
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


/*****************************************************************************/
/*  Function get_loadinfo - fills the LoadInfo structure.                    */
/*                                                                           */
/*  Input - pointer to struct MRCheader *hdata used for error checking.      */
/*        - pointer to struct LoadInfo  *li defined in mrcfiles.h            */
/*                                                                           */
/*  Returns 1 if no errors, 0 if LoadInfo wasn't filled.                     */
/*****************************************************************************/

int get_loadinfo(struct MRCheader *hdata, struct LoadInfo *li)
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

int loadtilts(struct TiltInfo *ti, struct MRCheader *hdata)
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








/*****************************************************************************/
/* Function fgetline : get a line length < limit                             */
/* from a file pointer fp   and put it in a null terminated string s         */
/* Returns the length of the line of a non positive value if                 */
/* EOF is reached                                                            */
/*****************************************************************************/

int fgetline(FILE *fp, char s[],int limit)
{
  int c, i, length;
     
  for (i=0; (((c = getc(fp)) != EOF) &&(i < (limit-1)) && (c != '\n')); i++)
    s[i]=c;

  s[i]='\0';
  length = i;
  if (c == EOF) return (length - (2 * length));
  else return length;
}


int mrc_getdcsize(int mode, int *dsize, int *csize)
{
  switch (mode){
  case MRC_MODE_BYTE:
    *dsize = sizeof(unsigned char);
    *csize = 1;
    break;
  case MRC_MODE_SHORT:
    *dsize = sizeof(short);
    *csize = 1;
    break;
  case MRC_MODE_FLOAT:
    *dsize = sizeof(float);
    *csize = 1;
    break;
  case MRC_MODE_COMPLEX_SHORT:
    *dsize = sizeof(short);
    *csize = 2;
    break;
  case MRC_MODE_COMPLEX_FLOAT:
    *dsize = sizeof(float);
    *csize = 2;
    break;
  case MRC_MODE_RGB:
    *dsize = sizeof(unsigned char);
    *csize = 3;
    break;
  default:
    return(-1);
  }
  return(0);
}

/* DNM 12/25/00: Scale is defined as ratio of sample to cell, so change the
   nx, ny, nz below to mx, my, mz.  But also return 0 instead of 1 if cell
   size is zero; so that the mrc_set_scale will fix both cell and sample */
/* DNM 9/13/02: Invert these to correspond to all other usage */
void mrc_get_scale(struct MRCheader *h, float *xs, float *ys, float *zs)
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
void mrc_set_scale(struct MRCheader *h,
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

void mrc_coord_cp(struct MRCheader *hout, struct MRCheader *hin)
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


void mrc_swap_shorts(short int *data, int amt)
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

void mrc_swap_longs(int *data, int amt)
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

void mrc_swap_floats(float *data, int amt)
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

void mrc_swap_floats(float *data, int amt)
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

void mrc_swap_floats(float *data, int amt)
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

#define SEEK_LIMIT 2000000000

int mrc_big_seek(FILE *fp, int base, int size1, int size2, int flag)
{
  int smaller, bigger, ntodo, ndo, abs1, abs2;
  int steplimit, err;

  /* Do the base seek if it is non-zero, or if the rest of the seek is
     zero and we are doing a SEEK_SET */
  if (base || ((!size1 || !size2) && (flag == SEEK_SET))) {
    if (err = fseek(fp, base, flag))
      return err;
    flag = SEEK_CUR;
  }

  if (!size1 || !size2)
    return 0;

  /* Find smaller and larger size */
  abs1 = size1 >= 0 ? size1 : -size1;
  abs2 = size2 >= 0 ? size2 : -size2;
  smaller = abs1 < abs2 ? abs1 : abs2;
  bigger = abs1 < abs2 ? abs2 : abs1;

  /* Step by multiples of the larger size, but not by more than the limit */
  steplimit = SEEK_LIMIT / bigger;
  ntodo = smaller;

  /* If one of the size entries is negative, negate the steps */
  if ((size1 < 0 && size2 >= 0) || (size1 >= 0 && size2 < 0))
    bigger = -bigger;

  while (ntodo > 0) {
    ndo = ntodo <= steplimit ? ntodo : steplimit;
    if (err = fseek(fp, ndo * bigger, flag))
      return err;
    ntodo -= ndo;
    flag = SEEK_CUR;
  }
  return 0;
}

/* These routines will simply call the standard C routine unless under Windows,
   then they will get the matching file handle and call the low-level Windows
   routine */
int b3dFseek(FILE *fp, int offset, int flag)
{
#ifdef WIN32_BIGFILE
  int handle = _fileno(fp);
  __int64 err;
  err = _lseeki64(handle, (_int64)offset, flag);
  return (err == -1 ? -1 : 0);
#else
  return fseek(fp, offset, flag);
#endif
}

size_t b3dFread(void *buf, size_t size, size_t count, FILE *fp)
{
#ifdef WIN32_BIGFILE
  int handle = _fileno(fp);
  return (size_t)(_read(handle, buf, size * count) / size);
#else
  return fread(buf, size, count, fp);
#endif
}
 
size_t b3dFwrite(void *buf, size_t size, size_t count, FILE *fp)
{
#ifdef WIN32_BIGFILE
  int handle = _fileno(fp);
  return (size_t)(_write(handle, buf, size * count) / size);
#else
  return fwrite(buf, size, count, fp);
#endif
}

/*
$Log$
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
