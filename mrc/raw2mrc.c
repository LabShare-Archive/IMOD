/*
 *  raw2mrc.c -- Convert raw image data to an mrc image file.
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "mrcfiles.h"
#include "b3dutil.h"

/* input data types */
#define DTYPE_CHAR   1
#define DTYPE_BYTE   1
#define DTYPE_UCHAR  2
#define DTYPE_SHORT  3
#define DTYPE_USHORT 4
#define DTYPE_LONG   5
#define DTYPE_ULONG  6
#define DTYPE_FLOAT  7
#define DTYPE_SBYTE  10
#define DTYPE_LONG2SHORT   15
#define DTYPE_ULONG2SHORT  16

/* input image types */
#define ITYPE_GREY    1
#define ITYPE_COMPLEX 2
#define ITYPE_RGB     3

/* compression */
#define CTYPE_NONE 0
static void rawswap(unsigned char *indata, int pixsize, int xysize);
static void rawdecompress
(void *indata, int pixsize, int xysize, int compression);

int setintype(char *stype, int *size, int *otype);

void usage(void)
{     
  fprintf(stderr, "raw2mrc %s %s %s.\n", VERSION_NAME, __DATE__,
          __TIME__);
  imodCopyright();
  fprintf(stderr, "Converts raw data into mrc file format.\n");
  fprintf(stderr, "Usage raw2mrc [options] <input files...> <output file>\n");
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "\t-x #    Width of input image, default 640\n");
  fprintf(stderr, "\t-y #    Height of input image, default 480\n");
  fprintf(stderr, "\t-z #    Number of sections in input, default 1\n");
  fprintf(stderr, "\t-t type Input data type. \n"
          "\t        (byte, sbyte, short, ushort, "
          "long, ulong, float, rgb)\n");
  fprintf(stderr, "\t-s      Swap input bytes.\n");
  fprintf(stderr, "\t-o #    Offset at beginning of file, default 0\n");
  fprintf(stderr, "\t-oz #   Offset between each pair of sections, "
          "default 0\n");
  fprintf(stderr, "\t-f      Flip image around X axis\n");
  fprintf(stderr, "\t-d      Divide unsigned shorts by 2 instead of subtracting 32767\n");
  fprintf(stderr, "\t-u      Save unsigned shorts in unsigned file mode (6) \n");
  fprintf(stderr, "\t-c      Convert long or ulong input to 16-bit"
          " integers, not floats\n");
  return;
}

int main( int argc, char *argv[] )
{
  struct MRCheader hdata;
  int    i, j, k;
  unsigned int xysize;
  FILE   *fin;
  FILE   *fout;
  int    intype = -1;
  int    outtype, datatype;
  int    outPixsize = -1;
  int    compression = 0;
  int    byteswap = FALSE;
  int    x = 640, y = 480, z = 1;
  int    nfiles, nsecs;
  int    flip = 0;   /* flag whether to flip about X axis */
  int    divide = 0; /* flag to divide unsigned short by 2 */
  int    keepUshort = 0;  /* Flag to keep unsigned shorts */
  int    hsize = 0;  /* amount of data to skip */
  int    convert = 0; /* Flag to convert long to short */
  int    csize = 1;
  int    zoffset = 0;
  int    pixsize = 1;
  float  angle = 90.0;
  float  xlen, ylen, zlen;
  char   line[128];
  unsigned char buf[4];
  char   c, c1, c2;
  b3dInt16  s;
  b3dUInt16 usval;
  int datasize;
  float  f, pixel;
  float  mean = 0.0f, tmean = 0.0f, max = -5e29f, min= 5e29f;
  int start = 0;
  b3dByte *sbdata, sbval;
  b3dUByte *bdata, bval;
  b3dInt16 *sdata, sval;
  b3dUInt16 *usdata;
  b3dInt32 *ldata;
  b3dUInt32 *uldata;
  b3dFloat *fdata, fval;
  void *indata;
  int val;
  char *progname = imodProgName(argv[0]);

  if (argc < 3){
    usage();
    exit(3);
  }
     

  for (i = 1; i < argc ; i++){
    if (argv[i][0] == '-' && strlen(argv[i]) <= 2){
      switch (argv[i][1]){

      case 't': /* input file type */
        intype = setintype(argv[++i], &pixsize, &outtype);
        /* printf("pixsize = %d\n", pixsize); */
        break;
            
      case 'o':
        if (argv[i][2] == 'z'){
          zoffset = atoi(argv[++i]);
        }else{
          hsize = atoi(argv[++i]);
        }
        break;
      case 'x':
        x = atoi(argv[++i]);
        break;
      case 'y':
        y = atoi(argv[++i]);
        break;
      case 'z':
        z = atoi(argv[++i]);
        break;

      case 's':
        byteswap = TRUE;
        break;
      case 'f':
        flip = 1;
        break;
      case 'd':
        divide = 1;
        break;
      case 'u':
        keepUshort = 1;
        break;
      case 'c':
        convert = 1;
        break;
      }
    }else break;
  }
     
  if ( (argc - 1) < (i + 1)){
    fprintf(stderr, "ERROR: %s - insufficient arguments\n", progname);
    usage();
    exit(3);
  }

  if (convert) {
    if (intype < 0 || intype == DTYPE_LONG) {
      intype = setintype("long2short", &pixsize, &outtype);
      outPixsize = 2;
    } else if (intype == DTYPE_ULONG) {
      intype = setintype("ulong2short", &pixsize, &outtype);
      outPixsize = 2;
    } else
      fprintf(stderr, "WARNING: %s - conversion option -c ignored with "
             "specified input type\n", progname);
  }

  if (divide && keepUshort) {
    fprintf(stderr, "ERROR: %s - You cannot divide by 2 and keep mode as "
            "unsigned\n", progname);
    exit(3);
  }

  if (intype < 0)
    intype = setintype("byte", &pixsize, &outtype);
  if (outPixsize < 0)
    outPixsize = pixsize;

  if (keepUshort) {
    if (intype == DTYPE_USHORT || intype == DTYPE_ULONG2SHORT)
      outtype = MRC_MODE_USHORT;
    else
      fprintf(stderr, "WARNING: %s - option -u ignored with "
             "specified input type\n", progname);
  }

  nfiles = argc - (i + 1);
  nsecs = z *nfiles;

  /* printf("nfiles = %d, argc = %d\n",nfiles, argc); */

  if (imodBackupFile(argv[argc - 1])) {
    fprintf(stderr, "ERROR: %s - couldn't create backup", progname);
    exit(3);
  }
  fout = fopen(argv[argc - 1], "wb");
  if (!fout){
    fprintf(stderr, "ERROR: %s - opening %s for output\n", progname,
            argv[argc -1]);
    exit(3);
  }

  for (j = 0; j < 1024; j++)
    b3dFwrite(&start , 1, 1, fout);

  datasize = x * y * z;
  xysize = x * y * csize;

  indata = (void *)malloc(pixsize * xysize);
  if (!indata){
    fprintf(stderr, "ERROR: %s - getting memory.\n", progname);
    exit(3);
  }

  for (j = i ; j < argc-1 ; j++) {
    fin = fopen(argv[j], "rb");
    if (!fin){
      fprintf(stderr, "ERROR: %s - opening %s for input\n", progname,
              argv[j]);
      exit(3);
    }

    if (b3dFseek(fin, hsize, SEEK_CUR)) {
      fprintf(stderr, "ERROR: %s - seeking to data in file  %s\n",
              progname, argv[j]);
      exit(3);
    }

    for (k = 0; k < z; k++) {
      tmean = 0.0f;
      if (k > 0) {
        if (b3dFseek(fin, zoffset, SEEK_CUR)) {
          fprintf(stderr, "ERROR: %s - seeking to data at section %d in "
                  "file  %s\n", progname, k, argv[j]);
          exit(3);
        }
      }
      if (b3dFread(indata, pixsize, xysize, fin) != xysize) {
        fprintf(stderr, "ERROR: %s - reading data from file  %s\n", progname,
                argv[j]);
        exit(3);
      }
      if (compression){
        rawdecompress(indata, pixsize, xysize, compression);
      }
      if (byteswap)
        rawswap(indata, pixsize, xysize);


      /* First do any needed conversions of the input types */
      switch (intype) {

      case DTYPE_USHORT:
        usdata = (b3dUInt16 *)indata;
        sdata = (b3dInt16 *)indata;
        if (divide) 
          for (i = 0; i < xysize; i++)
            sdata[i] = (b3dInt16)((int)(usdata[i]) / 2);
        else if (!keepUshort)
          for (i = 0; i < xysize; i++)
            sdata[i] = (b3dInt16)((int)(usdata[i]) - 32767);
        break;

      case DTYPE_LONG:
        ldata = (b3dInt32 *)indata;
        fdata = (b3dFloat *)indata;
        for (i = 0; i < xysize; i++)
          fdata[i] = (b3dFloat)ldata[i];
        break;

      case DTYPE_ULONG:
        uldata = (b3dUInt32 *)indata;
        fdata = (b3dFloat *)indata;
        for (i = 0; i < xysize; i++)
          fdata[i] = (b3dFloat)uldata[i];
        break;

      case DTYPE_ULONG2SHORT:
        uldata = (b3dUInt32 *)indata;
        sdata = (b3dInt16 *)indata;
        usdata = (b3dUInt16 *)indata;
        if (divide) 
          for (i = 0; i < xysize; i++)
            sdata[i] = (b3dInt16)(uldata[i] / 2);
        else if (keepUshort)
          for (i = 0; i < xysize; i++)
            usdata[i] = (b3dUInt16)uldata[i];
        else
          for (i = 0; i < xysize; i++)
            sdata[i] = (b3dInt16)(uldata[i] - 32767);
        break;

      case DTYPE_LONG2SHORT:
        ldata = (b3dInt32 *)indata;
        sdata = (b3dInt16 *)indata;
        if (divide) 
          for (i = 0; i < xysize; i++)
            sdata[i] = (b3dInt16)(ldata[i] / 2);
        else
          for (i = 0; i < xysize; i++)
            sdata[i] = (b3dInt16)ldata[i];
        break;

        /* DNM 12/27/01: fixed these two types on the PC by making the 
           signed and unsigned types explicit */
      case DTYPE_SBYTE:
        sbdata  = (b3dByte *)indata;
        bdata = (b3dUByte *)indata;
        for (i = 0; i < xysize; i++) {
          val = sbdata[i];
          val += 127;
          bdata[i] = val;
        }
        break;

      default:
        break;
      }


      /* Now compute min, mnax, mean based on output type */
      switch (outtype) {
        
      case MRC_MODE_SHORT:
        sdata = (b3dInt16 *)indata;
        for (i = 0; i < xysize; i++) {
          pixel = sdata[i];
          tmean += pixel;
          if (min > pixel)
            min = pixel;
          if (max < pixel)
            max = pixel;
        }
        break;

      case MRC_MODE_USHORT:
        usdata = (b3dUInt16 *)indata;
        for (i = 0; i < xysize; i++) {
          pixel = usdata[i];
          tmean += pixel;
          if (min > pixel)
            min = pixel;
          if (max < pixel)
            max = pixel;
        }
        break;

      case MRC_MODE_FLOAT:
        fdata = (b3dFloat *)indata;
        for (i = 0; i < xysize; i++) {
          pixel = fdata[i];
          tmean += pixel;
          if (min > pixel)
            min = pixel;
          if (max < pixel)
            max = pixel;
        }
        break;

      case MRC_MODE_BYTE:
        bdata = (b3dUByte *)indata;
        for (i = 0; i < xysize; i++) {
          pixel = bdata[i];
          tmean += pixel;
          if (min > pixel)
            min = pixel;
          if (max < pixel)
            max = pixel;
        }
        break;

      default:
        break;
      }

      if (flip == 0) {
        if (b3dFwrite(indata, outPixsize, xysize, fout) != xysize) {
          fprintf(stderr, "ERROR: %s - writing data to file\n", progname);
          exit(3);
        }
      } else {

        for (i = y - 1; i >= 0 ; i--) {
          bdata = ((unsigned char *)indata) + i * outPixsize * x;
          if (b3dFwrite(bdata, outPixsize, x, fout) != x) {
            fprintf(stderr, "ERROR: %s - writing data to file\n", progname);
            exit(3);
          }
        }
      }
      mean += (tmean / (float)xysize);
    }
    fclose(fin);
  }

  mean  /= (float)nsecs;

  printf("Min = %g, Max = %g, Mean = %g\n", min, max, mean);

  /* write out MRC header */
  /* DNM 11/5/02: change from raw writes of each element to calling
     library routines.  Added label.  1/17/04: eliminate unneeded rewind */
  mrc_head_new(&hdata, x, y, nsecs, outtype);
  hdata.amin = min;
  hdata.amax = max;
  hdata.amean = mean;
  mrc_head_label(&hdata, "raw2mrc: Converted to mrc format.");
  mrc_head_write(fout, &hdata);

  fclose(fout);

  exit(0);
}


static void rawdecompress
(void *indata, int pixsize, int xysize, int compression)
{
  if (!compression)
    return;

  return;
}


static void rawswap(b3dUByte *indata, int pixsize, int xysize)
{
  b3dUByte u1,u2,u3,u4;
  int i;

  if (pixsize == 4) {
    for (i = 0 ; i < xysize; i++) {
           
      /* DNM 12/18/01: changed i * 2 to i * 4 */
      u1 = indata[i * 4];
      u2 = indata[(i * 4) + 1];
      u3 = indata[(i * 4) + 2];
      u4 = indata[(i * 4) + 3];
      indata[i * 4] = u4;
      indata[(i * 4) + 1] = u3;
      indata[(i * 4) + 2] = u2;
      indata[(i * 4) + 3] = u1;
    }
    return;
  }


  if (pixsize == 2) {
    for (i = 0 ; i < xysize; i++) {
      u1 = indata[i * 2];
      u2 = indata[(i * 2) + 1];
      indata[i * 2] = u2;
      indata[(i * 2) + 1] = u1;
    }
    return;
  }
  return;
}

int setintype(char *stype, int *size, int *otype)
{
  if (!strcmp(stype,"byte")) {
    *size = 1;
    *otype = MRC_MODE_BYTE;
    return(DTYPE_BYTE);
  }
     
  if (!strcmp(stype,"sbyte")) {
    *size = 1;
    *otype = MRC_MODE_BYTE;
    return(DTYPE_SBYTE);
  }

  if (!strcmp(stype,"rgb")) {
    *size = 3;
    *otype = MRC_MODE_RGB;
    return(DTYPE_BYTE);
  }

  if (!strcmp(stype, "short")) {
    *size = 2;
    *otype = MRC_MODE_SHORT;
    return(DTYPE_SHORT);
  }
  if (!strcmp(stype,"ushort")) {
    *size = 2;
    *otype = MRC_MODE_SHORT;
    return(DTYPE_USHORT);
  }

  /* DNM 12/27/01: long was missing from the quotes here */
  if (!strcmp(stype,"long")) {
    *size = 4;
    *otype = MRC_MODE_FLOAT;
    return(DTYPE_LONG);
  }
  if (!strcmp(stype,"ulong")) {
    *size = 4;
    *otype = MRC_MODE_FLOAT;
    return(DTYPE_ULONG);
  }

  if (!strcmp(stype,"long2short")) {
    *size = 4;
    *otype = MRC_MODE_SHORT;
    return(DTYPE_LONG2SHORT);
  }
  if (!strcmp(stype,"ulong2short")) {
    *size = 4;
    *otype = MRC_MODE_SHORT;
    return(DTYPE_ULONG2SHORT);
  }
     
  if (!strcmp(stype,"float")) {
    *size = 4;
    *otype = MRC_MODE_FLOAT;
    return(DTYPE_FLOAT);
  }

  return(-1);
}

/*
$Log$
Revision 3.16  2005/11/11 21:55:08  mast
Outputs unsigned mode

Revision 3.15  2005/02/11 01:42:34  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.14  2004/09/10 21:33:31  mast
Eliminated long variables

Revision 3.11.2.2  2004/07/08 22:55:03  mast
Backport to 3.3

Revision 3.13  2004/07/08 22:29:33  mast
Had to make prewriting of header bytes us 3dFwrite too

Revision 3.12  2004/07/07 19:25:31  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.11  2004/06/19 22:13:36  mast
Fixed seek error message

Revision 3.10  2004/06/06 00:48:39  mast
Added an option for converting long to short

Revision 3.9  2004/01/17 20:38:45  mast
Eliminate unneeded rewind call

Revision 3.8  2004/01/06 21:20:07  mast
Made it make a backup file of an existing output file

Revision 3.7  2003/11/18 19:29:32  mast
changes to call b3dF* functions for 2GB problem on Windows

Revision 3.6  2003/10/24 02:28:42  mast
strip directory from program name and/or use routine to make backup file

Revision 3.5  2002/11/05 23:22:22  mast
Changed to use library routine for writing header

Revision 3.4  2002/11/05 23:35:08  mast
Changed to call imodCopyright

Revision 3.3  2001/12/29 01:30:23  mast
Fixed byte swapping for 4-byte entities

Revision 3.2  2001/12/29 01:04:09  mast
*** empty log message ***

Revision 3.1  2001/12/29 00:49:24  mast
Made signed long work, made signed and unsigned bytes work on PC,
eliminated two lines of diagnostic output

*/
