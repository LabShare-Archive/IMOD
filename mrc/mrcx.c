/*  IMOD VERSION 2.50
 *
 *  mrcx.c -- Convert between Vax, SGI, and PC data formats
 *
 *  Original author: Ross Darghi, Integrated Crystollagraphy Environment
 *  Revised by: Jim Kremer, David Mastronarde   email: mast@colorado.edu
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
#include <mrcfiles.h>

/* prototypes */
void convertBytes(FILE  *fp, UCHAR *buffer, int   noBytes, int   direction); 
void convertShorts(FILE  *fp, UCHAR *buffer, int   noShorts, int   direction);
void convertFloats(FILE  *fp, UCHAR *buffer, int   noFloats, int   direction);
void convertLongs(FILE  *fp, UCHAR *buffer, int   noLongs, int   direction); 
void convertBody(void (*convertFunc)(), struct MRCheader *header, int size,
		 int factor, int direction, FILE *infp, FILE *outfp);
void convertHeader(void (*floatFunc)(), int nextra, struct MRCheader  *header,
		   FILE *infp, FILE *outfp, int direction);


main( int argc, char *argv[])
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
     long int filesize, datasize;

#ifdef SWAP_IEEE_FLOATS
     swapieee = TRUE;
#else
     swapieee = FALSE;
#endif

#ifndef LITTLE_ENDIAN
     if (argc < 2){
	  fprintf(stderr, "Usage: %s [-vms|-ieee] filename [optional output filename]\n",
		  argv[0]);
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
		  argv[0]);
	  exit(1);
     }
#else
     if (argc < 2 || argc > 3 || argv[1][0] == '-') {
	  fprintf(stderr, "%s: Usage [filename] [opt filename]\n",
		  argv[0]);
	  fprintf(stderr, "There is no option to convert VMS data on this machine.\n");
	  exit(1);
     }
     i = 1;
#endif
     
     fin = fopen(argv[i], "r");
     if (fin == NULL){
	  fprintf(stderr, "%s: Couldn't open %s.\n", argv[0], argv[i]);
	  exit(10);
     }
     i++;

     if (i >= argc){
	  inplace = TRUE;
	  fout = fopen(argv[i-1], "r+");
     }
     else{
	  fout = fopen(argv[i], "w");
	  inplace = FALSE;
     }

     if (fout == NULL){
	  fprintf(stderr, "%s: Couldn't open %s.\n", argv[0], argv[i]);
	  exit(10);
     }

     if (swapieee)
	  floatFunc = convertLongs;
     else
	  floatFunc = convertFloats;

     if (fread(&hdata, 56, 4, fin) == 0){
	  fprintf(stderr, "%s: Error Reading header.\n", argv[0]);
	  exit(-1);
     }

     hdata.swapped = 0;

     /* Test for byte-swapped data with image size and the map numbers */
     if (hdata.nx <= 0 || hdata.nx > 60000 ||
	 hdata.ny <= 0 || hdata.ny > 60000 ||
	 hdata.nz <= 0 || hdata.nz > 60000 ||
	 hdata.mapc < 0 || hdata.mapc > 4 ||
	 hdata.mapr < 0 || hdata.mapr > 4 ||
	 hdata.maps < 0 || hdata.maps > 4) {

	  /* Mark data as swapped and do the swaps of critical data */
	  hdata.swapped = 1;
	  mrc_swap_longs(&hdata.nx, 10);
	  mrc_swap_longs(&hdata.mapc, 3);
	  mrc_swap_longs(&hdata.next, 1);
	  mrc_swap_shorts(&hdata.nint, 4);
	  mrc_swap_longs(&hdata.nlabl, 1);

     }
     if (hdata.nx <= 0 || hdata.nx > 60000 ||
	 hdata.ny <= 0 || hdata.ny > 60000 ||
	 hdata.nz <= 0 || hdata.nz > 60000 ||
	 hdata.mapc < 0 || hdata.mapc > 4 ||
	 hdata.mapr < 0 || hdata.mapr > 4 ||
	 hdata.maps < 0 || hdata.maps > 4 ||
	 hdata.mode > 16 || hdata.mode < 0) {
	  fprintf(stderr, "%s: This is not an MRC file, even after "
		  "swapping bytes in header.\n", argv[0]);
	  
	  exit(-1);
     }

     hdata.headerSize = 1024;
     hdata.headerSize += hdata.next;
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
	  datasize *= sizeof(short);
	  convertFunc = convertShorts;
	  size = sizeof(short);
	  factor = 1;
	  break;
	  
	case MRC_MODE_FLOAT:
	  datasize *= sizeof(float);
	  convertFunc = floatFunc;
	  size = sizeof(float);
	  factor = 1;
	  break;
	  
	case MRC_MODE_COMPLEX_SHORT:
	  datasize *= 2 * sizeof(short);
	  convertFunc = convertShorts;
	  size = sizeof(short);
	  factor = 2;
	  break;
	  
	case MRC_MODE_COMPLEX_FLOAT:
	  datasize *= 2 * sizeof(float);
	  convertFunc = floatFunc;
	  size = sizeof(float);
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
		  argv[0], hdata.mode);
	  exit(-1);
	  break;
     }

     datasize += hdata.headerSize;
     if (filesize < datasize || filesize > datasize + 511){
	  fprintf(stderr, "mrcx: Warning input file is %d bytes, "
		  "expected size is %d.\n", filesize, datasize);
     }
     rewind(fin);

#ifdef LITTLE_ENDIAN
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


     convertHeader(floatFunc, hdata.next, &hconv, fin, fout, tonative);

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
  unsigned long lcv;       
  UCHAR *scanLine;  /*converted scanline*/
  long   scans = 0;
  UCHAR  pix;
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

  scanLine = (UCHAR *)malloc(header->nx * size * factor);
  
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
		  UCHAR *buffer,    /*buffer to write the converted values to*/
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
		   UCHAR *buffer,   /*buffer to write the converted values to*/
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
  UCHAR exp;
  UINT  lcv;
  UINT  noBytes;
  UCHAR temp;   /*temporary place holder*/

  noBytes = noFloats * sizeof(float);

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

  for (lcv = 0; lcv < noBytes; lcv += sizeof(float))
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
		   int direction)

{
     int lcv;
     UCHAR extra;

     convertLongs(infp, (UCHAR *)&header->nx, 1, direction);
     convertLongs(infp, (UCHAR *)&header->ny, 1, direction);
     convertLongs(infp, (UCHAR*)&header->nz, 1, direction);
     
     convertLongs(infp, (UCHAR*)&header->mode, 1, direction);

     convertLongs(infp, (UCHAR *)&header->nxstart, 1, direction);
     convertLongs(infp, (UCHAR *)&header->nystart, 1, direction);
     convertLongs(infp, (UCHAR *)&header->nzstart, 1, direction);
     convertLongs(infp, (UCHAR *)&header->mx, 1, direction);
     convertLongs(infp, (UCHAR *)&header->my, 1, direction);
     convertLongs(infp, (UCHAR *)&header->mz, 1, direction);
     floatFunc(infp, (UCHAR *)&header->xlen, 1, direction);
     floatFunc(infp, (UCHAR *)&header->ylen, 1, direction);
     floatFunc(infp, (UCHAR *)&header->zlen, 1, direction);
     floatFunc(infp, (UCHAR *)&header->alpha, 1, direction);
     floatFunc(infp, (UCHAR *)&header->beta, 1, direction);
     floatFunc(infp, (UCHAR *)&header->gamma, 1, direction);
     convertLongs(infp, (UCHAR *)&header->mapc, 1, direction);
     convertLongs(infp, (UCHAR *)&header->mapr, 1, direction);
     convertLongs(infp, (UCHAR *)&header->maps, 1, direction);
     floatFunc(infp, (UCHAR *)&header->amin, 1, direction);
     floatFunc(infp, (UCHAR *)&header->amax, 1, direction);
     floatFunc(infp, (UCHAR *)&header->amean, 1, direction);
     convertShorts(infp, (UCHAR *)&header->ispg, 1, direction);
     convertShorts(infp, (UCHAR *)&header->nsymbt, 1, direction);
     /*     convertLongs(infp, (UCHAR *)header->extra, 16, direction); */
     convertLongs(infp, (UCHAR *)&header->next, 1, direction);
     convertShorts(infp, (UCHAR *)&header->creatid, 1, direction);
     convertBytes(infp, (UCHAR *)&header->blank, 30, direction);
     convertShorts(infp, (UCHAR *)&header->nint, 4, direction);
     floatFunc(infp, (UCHAR *)&header->min2, 6, direction);

     convertShorts(infp, (UCHAR *)&header->idtype, 1, direction);
     convertShorts(infp, (UCHAR *)&header->lens, 1, direction);

     convertShorts(infp, (UCHAR *)&header->nd1, 1, direction);
     convertShorts(infp, (UCHAR *)&header->nd2, 1, direction);
     convertShorts(infp, (UCHAR *)&header->vd1, 1, direction);
     convertShorts(infp, (UCHAR *)&header->vd2, 1, direction);

     floatFunc(infp, (UCHAR *)header->tiltangles, 6, direction);
     convertShorts(infp, (UCHAR *)&header->nwave, 1, direction);
     convertShorts(infp, (UCHAR *)&header->wave1, 1, direction);
     convertShorts(infp, (UCHAR *)&header->wave2, 1, direction);
     convertShorts(infp, (UCHAR *)&header->wave3, 1, direction);
     convertShorts(infp, (UCHAR *)&header->wave4, 1, direction);
     convertShorts(infp, (UCHAR *)&header->wave5, 1, direction);
     floatFunc(infp, (UCHAR *)&header->zorg, 1, direction);
     floatFunc(infp, (UCHAR *)&header->xorg, 1, direction);
     floatFunc(infp, (UCHAR *)&header->yorg, 1, direction);
     convertLongs(infp, (UCHAR *)&header->nlabl, 1, direction);


     for (lcv = 0; lcv < MRC_NLABELS; lcv++){
	  convertBytes(infp, (UCHAR *)header->labels[lcv], MRC_LABEL_SIZE, 
		       direction);  
	  header->labels[lcv][MRC_LABEL_SIZE] = '\0';
     }

     if (nextra > 0){
	  header->symops = (UCHAR *)malloc(nextra);
	  convertShorts(infp, (UCHAR *)header->symops, 
		       nextra/2, direction);  
	  if (nextra % 2)
	       convertBytes(infp, (UCHAR *)header->symops + nextra - 1, 1, 
			    direction);
     }
     
     header->fp = outfp;
     
} /*convertheader*/


/******************************************************************************/
void convertLongs(FILE  *fp,        /*file to read from*/
		  UCHAR *buffer,    /*buffer to write the converted values to*/
		  int   noLongs,    /*number of longwords to convert*/
		  int   direction)  /*conversion direction*/

/*converts a vax longword to a "unix" long word*/

{
  UINT  lcv;
  UINT  noBytes;  /*no bytes to read*/
  unsigned int lw;        /*long word place holder*/

  /* Read in data */

  noBytes = noLongs * sizeof(long);

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
		   UCHAR *buffer,   /*buffer to write the converted values to*/
		   int   noShorts,  /*number of shortwords to convert*/
		   int   direction) /*conversion direction*/

/*converts a vax shortword to a "unix" shortword*/

{
  UINT   lcv;
  UINT   noBytes;  /*no bytes to read from the input file*/
  USHORT s;        /*short word place holder*/

  /* Read in data */

  noBytes = noShorts * sizeof(short);

  if (fread(buffer, 1, noBytes, fp) != noBytes)
   {
     fprintf(stderr, "mrcx: Read Failed While Reading Short Quantities\n");
   }

  /* DNM 3/16/01: discovered that the code here did nothing on a little-endian
     machine!  Switch to call library routine */
  mrc_swap_shorts((short *)buffer, noShorts);

} /*convertShorts*/


