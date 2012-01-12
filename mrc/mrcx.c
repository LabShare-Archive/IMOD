/*
 *  mrcx.c -- Convert between Vax, SGI, and PC data formats
 *
 *  Original author: Ross Darghi, Integrated Crystollagraphy Environment
 *  Revised by: Jim Kremer, David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

/* 
 *
 * PURPOSE....:
 *   To convert an MRC file in VAX format to/from big endian/IEEE format, or
 *   a file in big endian IEEE to/from little endian IEEE format. Byte,
 *   float (4 byte), short int (2 byte), complex float (8 byte), RGB (3 byte),
 *   and complex short (4 byte) types are supported.
 *
 * REVISIONS..:
 *   Ross Dargahi     April 1990
 *
 *   Jim Kremer       Modified July 1993 for HVEM 3D mrc files.
 *                    Modified September 1993 for loading in HVEM extra data.
 *
 *                    Rewrote and renamed program to mrcx.  Program now uses
 *                    the mrcfiles library. Feb 1994
 *                    Jan   1995.  Byte data converted inplace is skipped.
 *                                 Large images don't crash program.
 *                    April 1995   Added bit mode 9-15.
 *                                 Fixed bug that didn't copy byte data.
 *                    Nov   1995   Fixed bug that destroyed header if data 
 *                                 type was unknown.
 *
 *  David Mastronarde Oct   2000   Rewrote to handle either VMS/Unix or
 *                                 big/little IEEE conversions, and to detect
 *                                 which direction to do the conversion in.
 *                                 Eliminated bit modes as hopeless.
 ******************************************************************************/

/******************************************************************************
 *   Include Files
 ******************************************************************************/
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
/*#include <fcntl.h> */
#include <math.h>

/*#include <environ.h> */
#include "mrcfiles.h"
#include "b3dutil.h"

#if defined(WIN32_BIGFILE) || defined(MAC103_BIGFILE)
#define fseek b3dFseek 
#define fread b3dFread 
#define fwrite b3dFwrite 
#define rewind b3dRewind
#endif

/* prototypes */
void convertBytes(FILE  *fp, b3dUByte *buffer, int   noBytes, int direction); 
void convertShorts(FILE  *fp, b3dUByte *buffer, int   noShorts, int direction);
void convertFloats(FILE  *fp, b3dUByte *buffer, int   noFloats, int direction);
void convertLongs(FILE  *fp, b3dUByte *buffer, int   noLongs, int direction); 
void convertBody(void (*convertFunc)(), struct MRCheader *header, int size,
                 int factor, int direction, FILE *infp, FILE *outfp);
void convertHeader(void (*floatFunc)(), int nextra, struct MRCheader  *header,
                   FILE *infp, FILE *outfp, int direction, char *cmap);


int main( int argc, char *argv[])
{
  void  (*convertFunc)();
  void  (*floatFunc)();
  struct MRCheader hdata;
  struct MRCheader hconv;
  struct MRCheader *hout;
  int error;
  int   factor;
  int   size;
  int   tonative;
  int   swapieee;
  FILE  *fin, *fout;
  int   i;
  int   inplace;
  unsigned char bdata;
  int filesize, datasize;
  char *progname = imodProgName(argv[0]);

#ifdef SWAP_IEEE_FLOATS
  swapieee = TRUE;
#else
  swapieee = FALSE;
#endif

#ifndef B3D_LITTLE_ENDIAN
  if (argc < 2){
    fprintf(stderr, "Usage: %s [-vms|-ieee] filename [optional output filename]\n",
            progname);
#ifdef SWAP_IEEE_FLOATS
    fprintf(stderr, "options: choose only one, default is -ieee.\n");
#else
    fprintf(stderr, "options: choose only one, default is -vms.\n");
#endif
    fprintf(stderr, "\t-vms   convert between VMS and IEEE format\n");
    fprintf(stderr, "\t-ieee  swap bytes in IEEE format only\n");
    exit(1);
  }

  i = 1;
  if (argv[1][0] == '-'){
    if (argv[1][1] == 'v'){
      swapieee = FALSE;
      i = 2;
    }else{
      if (argv[1][1] == 'i'){
        swapieee = TRUE;
        i = 2;
      }
    }
  }

  if (i >= argc){
    fprintf(stderr, "%s: Usage [-vms|-ieee] [filename] [optional output filename]\n",
            progname);
    exit(1);
  }
#else
  if (argc < 2 || argc > 3 || argv[1][0] == '-') {
    fprintf(stderr, "%s: Usage [filename] [opt filename]\n",
            progname);
    fprintf(stderr, "There is no option to convert VMS data on this machine.\n");
    exit(1);
  }
  i = 1;
#endif
     
  fin = fopen(argv[i], "rb");
  if (fin == NULL){
    fprintf(stderr, "%s: Couldn't open %s.\n", progname, argv[i]);
    exit(10);
  }
  i++;

  if (i >= argc){
    inplace = TRUE;
    fout = fopen(argv[i-1], "rb+");
  }
  else{
    fout = fopen(argv[i], "wb");
    inplace = FALSE;
  }

  if (fout == NULL){
    fprintf(stderr, "%s: Couldn't open %s.\n", progname, argv[i]);
    exit(10);
  }

  if (swapieee)
    floatFunc = convertLongs;
  else
    floatFunc = convertFloats;

  if (fread(&hdata, 56, 4, fin) == 0){
    fprintf(stderr, "%s: Error Reading header.\n", progname);
    exit(3);
  }

  hdata.swapped = 0;

  /* Test for byte-swapped data with image size and the map numbers */
  if (mrc_test_size(&hdata)) {

    /* Mark data as swapped and do the swaps of critical data */
    hdata.swapped = 1;
    mrc_swap_longs(&hdata.nx, 10);
    mrc_swap_longs(&hdata.mapc, 3);
    mrc_swap_longs(&hdata.next, 1);
    mrc_swap_shorts(&hdata.nint, 4);
    mrc_swap_longs(&hdata.nlabl, 1);

  }
  if (mrc_test_size(&hdata) ||
      hdata.mode > 16 || hdata.mode < 0) {
    fprintf(stderr, "%s: This is not an MRC file, even after "
            "swapping bytes in header.\n", progname);
      
    exit(3);
  }

  hdata.headerSize = 1024;
  hdata.headerSize += hdata.next;
  hdata.sectionSkip = 0;
  tonative = hdata.swapped;
  datasize = hdata.nx * hdata.ny * hdata.nz;
  fseek(fin, 0, SEEK_END);
  filesize = ftell(fin);
  switch(hdata.mode){
  case MRC_MODE_BYTE:
    convertFunc = convertBytes;
    size = 1;
    factor = 1;
    break;
      
  case MRC_MODE_SHORT:
    datasize *= sizeof(b3dInt16);
    convertFunc = convertShorts;
    size = sizeof(b3dInt16);
    factor = 1;
    break;
      
  case MRC_MODE_FLOAT:
    datasize *= sizeof(b3dFloat);
    convertFunc = floatFunc;
    size = sizeof(b3dFloat);
    factor = 1;
    break;
      
  case MRC_MODE_COMPLEX_SHORT:
    datasize *= 2 * sizeof(b3dInt16);
    convertFunc = convertShorts;
    size = sizeof(b3dInt16);
    factor = 2;
    break;
      
  case MRC_MODE_COMPLEX_FLOAT:
    datasize *= 2 * sizeof(b3dFloat);
    convertFunc = floatFunc;
    size = sizeof(b3dFloat);
    factor = 2;
    break;
      
  case MRC_MODE_RGB:
    datasize *= 3;
    convertFunc = convertBytes;
    size = 1;
    factor = 3;
    break;

  default:
    fprintf(stderr, "%s: data type %d unsupported.\n",
            progname, hdata.mode);
    exit(3);
    break;
  }

  datasize += hdata.headerSize;
  if (filesize < datasize || filesize > datasize + 511){
    fprintf(stderr, "mrcx: Warning input file is %d bytes, "
            "expected size is %d.\n", filesize, datasize);
  }
  rewind(fin);

#ifdef B3D_LITTLE_ENDIAN
  if (tonative)
    printf("Converting big-endian IEEE to little-endian IEEE.\n");
  else
    printf("Converting little-endian IEEE to big-endian IEEE.\n");
#else
  if (swapieee) {
    if (tonative)
      printf("Converting little-endian IEEE to big-endian IEEE.\n");
    else
      printf("Converting big-endian IEEE to little-endian IEEE.\n");
  } else {
    if (tonative)
      printf("Converting little-endian VMS to big-endian IEEE.\n");
    else
      printf("Converting big-endian IEEE to little-endian VMS.\n");
  }
#endif


  convertHeader(floatFunc, hdata.next, &hconv, fin, fout, tonative, 
                &hdata.cmap[0]);

  fwrite(&hconv, 56, 4, fout);
     
  for (i = 0; i < MRC_NLABELS; i++)
    fwrite(hconv.labels[i], MRC_LABEL_SIZE, 1,fout);

  if (hdata.next)
    fwrite(hconv.symops, hdata.next, 1, fout);


  if (!inplace || convertFunc != convertBytes)
    convertBody(convertFunc, &hdata, size, factor, tonative, fin, fout);
     
  fclose(fin);
  if (!inplace)
    fclose(fout);
  exit(0);
  return(0);
}



/*****************************************************************************/



void convertBody(void (*convertFunc)(),
                 struct MRCheader *header,
                 int size,
                 int factor,
                 int direction,
                 FILE *infp,
                 FILE *outfp)

     /*sets up the image data conversion and writes the converted data to the 
       output file*/

{
  unsigned int lcv;       
  b3dUByte *scanLine;  /*converted scanline*/
  int   scans = 0;
  b3dUByte  pix;
  int i;

  if (size == 1){
    size = header->nx * header->ny * header->nz;
    for(i = 0; i < size; i++){
      if (! fread(&(pix), 1,1,infp)){
        perror("Error copying data ");
        return;
      }
      fwrite(&(pix), 1, 1, outfp);
    }
    return;
  }

  scanLine = (b3dUByte *)malloc(header->nx * size * factor);
  
  scans = header->ny * header->nz;

  for (lcv = 0; lcv < scans; lcv++)
    {
      (*convertFunc)(infp, scanLine, header->nx * factor, direction);
      fwrite(scanLine, size, header->nx * factor, outfp);
    }

  free(scanLine);

} /*convertBody*/



/*****************************************************************************/
void convertBytes(FILE  *fp,        /*file to read from*/
                  b3dUByte *buffer, /*buffer to write the converted values to*/
                  int   noBytes,    /*number of bytes to convert*/
                  int   direction)  /*conversion direction*/

     /*converts byte data.*/

{
  /* Read in data */

  if (fread(buffer, 1, noBytes, fp) != noBytes)
    {
      fprintf(stderr, "Read Failed While Reading Byte Quantities\n");
    }

} /*convertBytes*/



/*****************************************************************************/
void convertFloats(FILE  *fp,       /*file to read from*/
                   b3dUByte *buffer,/*buffer to write the converted values to*/
                   int   noFloats,  /*number of floats to convert*/
                   int   direction) /*conversion direction*/

     /*
       Converts a floating point number in vax floating point representation to IEEE
       representation and vica versa. A vax floating point no is bias 128 with
       exponents ranging from -127 to 127 the number is stored little endian. An IEEE
       float is point no is bias 127 with exponents ranging form -126 to 128, the
       number is typically stored big endian.
     */

{
  b3dUByte exp;
  b3dUInt32  lcv;
  b3dUInt32  noBytes;
  b3dUByte temp;   /*temporary place holder*/

  noBytes = noFloats * sizeof(b3dFloat);

  if (fread(buffer, 1, noBytes, fp) != noBytes)
    {
      fprintf(stderr, "Read Failed While Reading Float Quantities\n");
    }

  /*When converting from vax floating to IEEE floating, it must be noted 
   *that vax float exponents range from -127 to 127 whilst IEEE exponents
   *range from -126 to 128. We must therefore clamp -127 vax floats to 
   *-126 IEEE floats. This is done by not subtracting when the vax
   *exponent is -127 (1 bias 128). Note that because the vax mantissa is <= 0.5
   *and the IEEE mantissa is <= 1 we subtract 2 from the exponent to make up
   *for this. When converting from IEEE floats to vax floats, we must clamp 128
   *IEEE floats to 127 vax floats. 
   *In either case corrent byte swapping must be done for
   *converting from big endian to little endian and vice versa*/ 

  for (lcv = 0; lcv < noBytes; lcv += sizeof(b3dFloat))
    {
      if (direction) /* FROM_VAX */
        {
          if ((exp = (buffer[lcv+1] << 1) | (buffer[lcv] >> 7 & 0x01)) > 3 && 
              exp != 0)
            buffer[lcv+1] -= 1;
          else if (exp <= 3 && exp != 0)  /*must zero out the mantissa*/
            {
              /*we want manitssa 0 & exponent 1*/

              buffer[lcv] = 0x80;
              buffer[lcv+1] &= 0x80;
              buffer[lcv+2] = buffer[lcv+3] = 0;
            }
       
          temp = buffer[lcv];
          buffer[lcv] = buffer[lcv+1];
          buffer[lcv+1] = temp;
        }
      else  /*TO_VAX*/
        {
          if ((exp = (buffer[lcv] << 1) | (buffer[lcv+1] >> 7 & 0x01)) < 253 &&
              exp != 0)
            buffer[lcv] += 1;
          else if (exp >= 253) /*must also max out the exp & mantissa*/
            {
              /*we want manitssa all 1 & exponent 255*/

              buffer[lcv] |= 0x7F;
              buffer[lcv+1] = 0xFF; 
              buffer[lcv+2] = buffer[lcv+3] = 0xFF;
            }

          temp = buffer[lcv];
          buffer[lcv] = buffer[lcv+1];
          buffer[lcv+1] = temp;
        }
      
      if (((direction  && exp > 3) || 
           (!direction  && exp < 253)) && exp != 0)
        {
          temp = buffer[lcv+2];
          buffer[lcv+2] = buffer[lcv+3];
          buffer[lcv+3] = temp;
        }
    }

} /*convertFloats*/

/******************************************************************************/
void convertHeader(void (*floatFunc)(),
                   int nextra,
                   struct MRCheader  *header,  /*MRC header structure*/
                   FILE         *infp,        /*input file pointer*/
                   FILE         *outfp,       /*input file pointer*/
                   int direction,
                   char *cmap)

{
  int lcv;
  b3dUByte extra;

  convertLongs(infp, (b3dUByte *)&header->nx, 1, direction);
  convertLongs(infp, (b3dUByte *)&header->ny, 1, direction);
  convertLongs(infp, (b3dUByte*)&header->nz, 1, direction);
     
  convertLongs(infp, (b3dUByte*)&header->mode, 1, direction);

  convertLongs(infp, (b3dUByte *)&header->nxstart, 1, direction);
  convertLongs(infp, (b3dUByte *)&header->nystart, 1, direction);
  convertLongs(infp, (b3dUByte *)&header->nzstart, 1, direction);
  convertLongs(infp, (b3dUByte *)&header->mx, 1, direction);
  convertLongs(infp, (b3dUByte *)&header->my, 1, direction);
  convertLongs(infp, (b3dUByte *)&header->mz, 1, direction);
  floatFunc(infp, (b3dUByte *)&header->xlen, 1, direction);
  floatFunc(infp, (b3dUByte *)&header->ylen, 1, direction);
  floatFunc(infp, (b3dUByte *)&header->zlen, 1, direction);
  floatFunc(infp, (b3dUByte *)&header->alpha, 1, direction);
  floatFunc(infp, (b3dUByte *)&header->beta, 1, direction);
  floatFunc(infp, (b3dUByte *)&header->gamma, 1, direction);
  convertLongs(infp, (b3dUByte *)&header->mapc, 1, direction);
  convertLongs(infp, (b3dUByte *)&header->mapr, 1, direction);
  convertLongs(infp, (b3dUByte *)&header->maps, 1, direction);
  floatFunc(infp, (b3dUByte *)&header->amin, 1, direction);
  floatFunc(infp, (b3dUByte *)&header->amax, 1, direction);
  floatFunc(infp, (b3dUByte *)&header->amean, 1, direction);
  convertLongs(infp, (b3dUByte *)&header->ispg, 1, direction);
  /*     convertLongs(infp, (b3dUByte *)header->extra, 16, direction); */
  convertLongs(infp, (b3dUByte *)&header->next, 1, direction);
  convertShorts(infp, (b3dUByte *)&header->creatid, 1, direction);
  convertBytes(infp, (b3dUByte *)&header->blank, 30, direction);
  convertShorts(infp, (b3dUByte *)&header->nint, 4, direction);
  floatFunc(infp, (b3dUByte *)&header->min2, 6, direction);

  convertShorts(infp, (b3dUByte *)&header->idtype, 1, direction);
  convertShorts(infp, (b3dUByte *)&header->lens, 1, direction);

  convertShorts(infp, (b3dUByte *)&header->nd1, 1, direction);
  convertShorts(infp, (b3dUByte *)&header->nd2, 1, direction);
  convertShorts(infp, (b3dUByte *)&header->vd1, 1, direction);
  convertShorts(infp, (b3dUByte *)&header->vd2, 1, direction);

  floatFunc(infp, (b3dUByte *)header->tiltangles, 6, direction);

  if (cmap[0] != 'M' || cmap[1] != 'A' || cmap[2] != 'P') {
    /* Old-style header, swap wavelength then origin data */
    convertShorts(infp, (b3dUByte *)&header->xorg, 6, direction);
    floatFunc(infp, (b3dUByte *)&header->cmap[0], 3, direction);
    printf("Preserving old-style MRC header.\n");
  } else {
    /* new style, swap floats and retain cmap; flip stamp */
    floatFunc(infp, (b3dUByte *)&header->xorg, 1, direction);
    floatFunc(infp, (b3dUByte *)&header->yorg, 1, direction);
    floatFunc(infp, (b3dUByte *)&header->zorg, 1, direction);
    convertBytes(infp, (b3dUByte *)&header->cmap[0], 8, direction);
    floatFunc(infp, (b3dUByte *)&header->rms, 1, direction);
    header->stamp[0] = (header->stamp[0] == 17) ? 68 : 17;
  }
  convertLongs(infp, (b3dUByte *)&header->nlabl, 1, direction);

  for (lcv = 0; lcv < MRC_NLABELS; lcv++){
    convertBytes(infp, (b3dUByte *)header->labels[lcv], MRC_LABEL_SIZE, 
                 direction);  
    header->labels[lcv][MRC_LABEL_SIZE] = '\0';
  }

  /* THIS WILL NOT WORK WITH DATA CONSISTING OF INTEGERS AND REALS */
  if (nextra > 0){
    header->symops = (b3dUByte *)malloc(nextra);
    convertShorts(infp, (b3dUByte *)header->symops, 
                  nextra/2, direction);  
    if (nextra % 2)
      convertBytes(infp, (b3dUByte *)header->symops + nextra - 1, 1, 
                   direction);
  }
     
  header->fp = outfp;
     
} /*convertheader*/


/******************************************************************************/
void convertLongs(FILE  *fp,        /*file to read from*/
                  b3dUByte *buffer, /*buffer to write the converted values to*/
                  int   noLongs,    /*number of longwords to convert*/
                  int   direction)  /*conversion direction*/

     /*converts a vax longword to a "unix" long word*/

{
  b3dUInt32  lcv;
  b3dUInt32  noBytes;  /*no bytes to read*/
  unsigned int lw;        /*long word place holder*/

  /* Read in data */

  noBytes = noLongs * sizeof(b3dInt32);

  if (fread(buffer, 1, noBytes, fp) != noBytes)
    {
      fprintf(stderr, "mrcx: Read Failed While Reading Long Quantities\n");
    }

  /* DNM 3/16/01: discovered that the code here did nothing on a little-endian
     machine!  Switch to call library routine */
  mrc_swap_longs((int *)buffer, noLongs);

} /*convertLong*/

/******************************************************************************/
void convertShorts(FILE  *fp,       /*file to read from*/
                   b3dUByte *buffer,/*buffer to write the converted values to*/
                   int   noShorts,  /*number of shortwords to convert*/
                   int   direction) /*conversion direction*/

     /*converts a vax shortword to a "unix" shortword*/

{
  b3dUInt32   lcv;
  b3dUInt32   noBytes;  /*no bytes to read from the input file*/
  b3dUInt16   s;        /*short word place holder*/

  /* Read in data */

  noBytes = noShorts * sizeof(b3dInt16);

  if (fread(buffer, 1, noBytes, fp) != noBytes)
    {
      fprintf(stderr, "mrcx: Read Failed While Reading Short Quantities\n");
    }

  /* DNM 3/16/01: discovered that the code here did nothing on a little-endian
     machine!  Switch to call library routine */
  mrc_swap_shorts((b3dInt16 *)buffer, noShorts);

} /*convertShorts*/


