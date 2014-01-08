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
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "iimage.h"
#include "b3dutil.h"
#include "parse_params.h"

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

static int setintype(char *stype, int *size, int *otype);

void usage(void)
{     
  printf("raw2mrc %s %s %s.\n", VERSION_NAME, __DATE__,
          __TIME__);
  imodCopyright();
  printf("Converts raw data into mrc file format.\n");
  printf("Usage raw2mrc [options] <input files...> <output file>\n");
  printf("Options:\n");
  printf("\t-x #    Width of input image, default 640\n");
  printf("\t-y #    Height of input image, default 480\n");
  printf("\t-z #    Number of sections in input, default 1\n");
  printf("\t-t type Input data type. \n"
          "\t        (byte, sbyte, short, ushort, "
          "long, ulong, float, rgb)\n");
  printf("\t-s      Swap input bytes.\n");
  printf("\t-o #    Offset at beginning of file, default 0\n");
  printf("\t-oz #   Offset between each pair of sections, "
          "default 0\n");
  printf("\t-f      Flip image around X axis\n");
  printf("\t-i      Stack images in inverted order in Z\n");
  printf("\t-d      Divide unsigned shorts by 2 instead of subtracting 32767\n");
  printf("\t-u      Save unsigned shorts in unsigned file mode (6) \n");
  printf("\t-c      Convert long or ulong input to 16-bit"
          " integers, not floats\n");
  printf("\t-p #    Pixel size in Angstroms to set in file header\n");
  printf("\t-pz #   Pixel size in Z in Angstroms, if different\n");
  return;
}

int main( int argc, char *argv[] )
{
  struct MRCheader hdata;
  int    i, j, k, zread, jread, iarg;
  unsigned int xysize;
  FILE   *fin;
  FILE   *fout;
  int    intype = -1;
  int    outtype, izsec;
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
  int    invertStack = 0;
  int    invertFiles = 0;
  int    zoffset = 0;
  int    pixsize = 1;
  float  pixel;
  double pixSpacing = 0., zPixel = 0.;
  float  mean = 0.0f, tmean = 0.0f, min, max, allmax = -5e29f, allmin= 5e29f;
  b3dByte *sbdata;
  b3dUByte *bdata, *bdata2, *scratch;
  b3dInt16 *sdata;
  b3dUInt16 *usdata;
  b3dInt32 *ldata;
  b3dUInt32 *uldata;
  b3dFloat *fdata;
  void *indata;
  int val;
  char *progname = imodProgName(argv[0]);
  char prefix[100];
  sprintf(prefix, "ERROR: %s - ", progname);
  setExitPrefix(prefix);

  if (argc < 3){
    usage();
    exit(3);
  }
     

  for (iarg = 1; iarg < argc ; iarg++){
    if (argv[iarg][0] == '-' && strlen(argv[iarg]) <= 3){
      switch (argv[iarg][1]){

      case 't': /* input file type */
        intype = setintype(argv[++iarg], &pixsize, &outtype);
        /* printf("pixsize = %d\n", pixsize); */
        break;
            
      case 'o':
        if (argv[iarg][2] == 'z'){
          zoffset = atoi(argv[++iarg]);
        }else{
          hsize = atoi(argv[++iarg]);
        }
        break;
      case 'x':
        x = atoi(argv[++iarg]);
        break;
      case 'y':
        y = atoi(argv[++iarg]);
        break;
      case 'z':
        z = atoi(argv[++iarg]);
        break;

      case 's':
        byteswap = TRUE;
        break;
      case 'f':
        flip = 1;
        break;
      case 'i':
        if (argv[iarg][2] == 'z') {
          invertStack = 1;
        } else if (argv[iarg][2] == 0x00) {
          invertFiles = 1;
          invertStack = 1;
        } else
          exitError("Unrecognized option %s", argv[iarg]);
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
      case 'p':
        if (argv[iarg][2] == 'z') {
          zPixel = atof(argv[++iarg]);
        } else if (argv[iarg][2] == 0x00) {
          pixSpacing = atof(argv[++iarg]);
        } else
          exitError("Unrecognized option %s", argv[iarg]);
        break;
      }
    }else break;
  }
     
  if ( (argc - 1) < (iarg + 1)){
    printf("ERROR: %s - insufficient arguments\n", progname);
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
      printf("WARNING: %s - conversion option -c ignored with "
             "specified input type\n", progname);
  }

  if (divide && keepUshort) 
    exitError("You cannot divide by 2 and keep mode as unsigned");

  if (intype < 0)
    intype = setintype("byte", &pixsize, &outtype);
  if (outPixsize < 0)
    outPixsize = pixsize;

  if (keepUshort) {
    if (intype == DTYPE_USHORT || intype == DTYPE_ULONG2SHORT)
      outtype = MRC_MODE_USHORT;
    else
      printf("WARNING: %s - option -u ignored with specified input type\n", progname);
  }

  nfiles = argc - (iarg + 1);
  nsecs = z *nfiles;

  /* printf("nfiles = %d, argc = %d\n",nfiles, argc); */

  if (!getenv("IMOD_NO_IMAGE_BACKUP") && imodBackupFile(argv[argc - 1])) 
    exitError("Couldn't create backup file");
  fout = iiFOpen(argv[argc - 1], "wb");
  if (!fout)
    exitError("Opening %s for output", argv[argc -1]);

  xysize = x * y;

  indata = (void *)malloc(pixsize * (xysize + (flip ? x : 0)));
  if (!indata)
    exitError("Getting memory");

  /* 7/21/11: make header now so signed byte output is known */
  mrc_head_new(&hdata, x, y, nsecs, outtype);
  if (pixSpacing > 0) {
    if (!zPixel)
      zPixel = pixSpacing;
    mrc_set_scale(&hdata, pixSpacing, pixSpacing, zPixel);
  }
  hdata.fp = fout;
  izsec = 0;

  for (j = iarg ; j < argc-1 ; j++) {
    jread = invertFiles ? iarg + argc - 2 - j : j;
    fin = fopen(argv[jread], "rb");
    if (!fin)
      exitError("Opening %s for input", argv[jread]);

    for (k = 0; k < z; k++) {
      tmean = 0.0f;
      max = -5e29f;
      min= 5e29f;
      zread = invertStack ? z - 1 - k : k;
      if (mrc_big_seek(fin, hsize + zread * zoffset, pixsize * x, y * zread, SEEK_SET))
        exitError("Seeking to data at section %d in file  %s", zread, argv[jread]);
      if (b3dFread(indata, pixsize, xysize, fin) != xysize) 
        exitError("Reading data from file  %s", argv[jread]);
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
          val += 128;
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
          min = B3DMIN(min, pixel);
          max = B3DMAX(max, pixel);
        }
        break;

      case MRC_MODE_USHORT:
        usdata = (b3dUInt16 *)indata;
        for (i = 0; i < xysize; i++) {
          pixel = usdata[i];
          tmean += pixel;
          min = B3DMIN(min, pixel);
          max = B3DMAX(max, pixel);
        }
        break;

      case MRC_MODE_FLOAT:
        fdata = (b3dFloat *)indata;
        for (i = 0; i < xysize; i++) {
          pixel = fdata[i];
          tmean += pixel;
          min = B3DMIN(min, pixel);
          max = B3DMAX(max, pixel);
        }
        break;

      case MRC_MODE_BYTE:
        bdata = (b3dUByte *)indata;
        for (i = 0; i < xysize; i++) {
          pixel = bdata[i];
          tmean += pixel;
          min = B3DMIN(min, pixel);
          max = B3DMAX(max, pixel);
        }
        break;

      default:
        break;
      }

      if (flip) {
        scratch = ((unsigned char *)indata) + y * outPixsize * x;
        for (i = 0; i < y / 2; i++) {
          bdata = ((unsigned char *)indata) + (y - 1 - i) * outPixsize * x;
          bdata2 = ((unsigned char *)indata) + i * outPixsize * x;
          memcpy(scratch, bdata, outPixsize * x);
          memcpy(bdata, bdata2, outPixsize * x);
          memcpy(bdata2, scratch, outPixsize * x);
        }
      }
      hdata.amin = min;
      hdata.amax = max;
      if (mrc_write_slice(indata, fout, &hdata, izsec++ ,'Z'))
        exitError("Writing data to file");
      mean += (tmean / (float)xysize);
      allmin = B3DMIN(min, allmin);
      allmax = B3DMAX(max, allmax);
    }
    fclose(fin);
  }

  hdata.amin = allmin;
  hdata.amax = allmax;
  mean  /= (float)nsecs;
  hdata.amean = mean;

  printf("Min = %g, Max = %g, Mean = %g\n", min, max, mean);

  /* write out MRC header */
  /* DNM 11/5/02: change from raw writes of each element to calling
     library routines.  Added label.  1/17/04: eliminate unneeded rewind */
  mrc_head_label(&hdata, "raw2mrc: Converted to mrc format.");
  mrc_head_write(fout, &hdata);

  iiFClose(fout);

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

static int setintype(char *stype, int *size, int *otype)
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
