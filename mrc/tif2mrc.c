/*
 *  tif2mrc -- Convert TIFF image files to MRC image files.
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

/* tif2mrc [-b file] [tiff files] [mrcfile] */

/* 5-22-95: bugfix, files of different sizes flail. JRK */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "mrcc.h"
#include "b3dtiff.h"
#include "b3dutil.h"

#define XSIZE 512
#define YSIZE 480

int read_cheeze_tiff(FILE *tif_fp, unsigned char *pixels);
static float minmaxmean(unsigned char *tifdata, int mode, int unsign, 
                        int divide, int xsize, int ysize, int *min, int *max);
static void convertrgb(unsigned char *tifdata, int xsize, int ysize);
static void expandIndexToRGB(unsigned char **datap, ImodImageFile *iifile, 
                              int section);
int main( int argc, char *argv[])
{

  FILE *bgfp  = NULL;
  FILE *tiffp = NULL;
  FILE *mrcfp = NULL;
  unsigned char *linePtr;

  Tf_info tiff;
  struct MRCheader hdata;

  int mode = 0, pixSize = 0;
  unsigned char *bgdata;
  unsigned char *tifdata;
  unsigned char *mrcdata;
  int min, max;
  int i,x,y,tmpdata;
  int bg = FALSE;
  int makegray = FALSE;
  int unsign = 0;
  int divide = 0;
  int keepUshort = 0;
  int forceSigned = 0;
  int readFirst = 0;
  int xsize = XSIZE, ysize = YSIZE;
  int mrcxsize = 0, mrcysize = 0, mrcsizeset;
  int bpix;
  float mean, tmean;
  int pixel;
  b3dInt16 *sptr;
  b3dInt16 *bgshort;
  int bgBits, bgxsize, bgysize, xdo, ydo, xoffset, yoffset;
  unsigned char *fillPtr;
  unsigned char byteFill[3];
  b3dInt16 shortFill;
  b3dUInt16 ushortFill;
  char *openmode = "rb";
  char *bgfile;
  char *progname = imodProgName(argv[0]);

  xsize = 0;
  ysize = 0;
  mean = 0;
  min = 100000;
  max = -100000;

  if (argc < 3){
    fprintf(stderr, "Tif2mrc Version %s %s %s\n" , VERSION_NAME,
            __DATE__, __TIME__);
    imodCopyright();
    fprintf(stderr,
            "Usage: %s [options] <tiff files...> <mrcfile>\n"
            , progname);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "\t-g      Convert 24-bit RGB to 8-bit grayscale\n");
    fprintf(stderr, "\t-u      Convert unsigned 16-bit values by "
            "subtracting 32768\n");
    fprintf(stderr, "\t-d      Convert unsigned 16-bit values by "
            "dividing by 2\n");
    fprintf(stderr, "\t-k      Keep unsigned 16-bit values; store in unsigned "
            "integer mode\n");
    fprintf(stderr, "\t-s      Store as signed integers (mode 1) even if data "
            "are unsigned\n");
    fprintf(stderr, "\t-f      Read only first image of multi-page file\n");
    fprintf(stderr, "\t-o x,y  Set output file size in X and Y\n");
    fprintf(stderr, "\t-b file Background subtract image in given file\n");

    exit(3);
  }

  for (i = 1; i < argc - 1 ; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){
         
      case 'h': /* help */
        break;

      case 'g': /* convert rgb to gray scale */
        makegray = TRUE;
        break;
 
      case 'u': /* treat ints as unsigned */
        unsign = 1;
        break;
 
      case 'd': /* treat ints as unsigned and divide by 2*/
        divide = 1;
        break;
 
      case 'k': /* save as unsigned */
        keepUshort = 1;
        break;
 
      case 's': /* save unsigned as signed */
        forceSigned = 1;
        break;
 
      case 'f': /* read only first image */
        readFirst = 1;
        break;
 
      case 'b':
        bgfile = strdup(argv[++i]);
        /*  bgfp = fopen(argv[++i], "rb"); */
        bg = TRUE;
        break;

      case 'o': /* Set output size */
        sscanf(argv[++i], "%d%*c%d", &mrcxsize, &mrcysize);

      default:
        break;
      }
      
    }
    else
      break;
       
  }
  if ( (argc - 1) < (i + 1)){
    fprintf(stderr, "%s: argument error: no output file specified.\n",
            progname);
    exit(3);
  }

  if (divide + unsign + keepUshort + forceSigned > 1) {
    fprintf(stderr, "%s: You must select only one of -u, -d, -k, or -s.\n",
            progname);
    exit(3);
  }
  if (divide)
    unsign = 1;
  if (unsign)
    forceSigned = 1;

  if (i == (argc - 2) && !readFirst){

    /* check for multi-paged tiff file. */
    /* Open the TIFF file. */
    int tiffPages;

    if (tiff_open_file(argv[i], openmode, &tiff)) {
      fprintf(stderr, "tif2mrc: Couldn't open %s.\n", argv[i]);
      exit(3);
    }
       
    tiffp = tiff.fp;
    if (tiff.iifile) {
      tiffPages = tiff.iifile->nz;
    } else {
      tiffPages = tiffIFDNumber(tiffp);
    }
    if (tiffPages > 1){

      int section = 0;
      int xysize;
        
      printf("Reading multi-paged TIFF file.\n");

      if (bg){
        fprintf(stderr, "Background subtraction not supported for "
                "multi-paged images.\n");
        exit(1);
      }

      if (mrcxsize) 
        fprintf(stderr, "Warning: output file size option ignored for "
                "multi-paged file\n");

      if (!tiff.iifile) {
        read_tiffheader(tiffp, &(tiff.header));
        rewind(tiffp);
        fread(&(tiff.header.byteorder), 2, 1, tiffp);
         
        tiff.header.firstIFDoffset = tiffFirstIFD(tiffp);
        rewind(tiffp);
        read_tiffentries(tiffp, &tiff);
      }

      if (imodBackupFile(argv[argc - 1])) {
        fprintf(stderr, "%s: Error, couldn't create backup", progname);
        exit(3);
      }
      mrcfp = fopen(argv[argc - 1], "wb");
      if (!mrcfp){
        perror("tif2mrc");
        fprintf(stderr, "Error opening %s\n", argv[argc - 1]);
        exit(2);
      }

      xsize = tiff.directory[WIDTHINDEX].value;
      ysize = tiff.directory[LENGTHINDEX].value;
      xysize = xsize * ysize;
      mrc_head_new(&hdata, xsize, ysize, tiffPages, mode);
      mrc_head_write(mrcfp, &hdata);
      pixSize = 1;
      if (tiff.BitsPerSample == 16){
        mode = MRC_MODE_SHORT;
        pixSize = 2;
      }

      /* Use unsigned mode either if user requested it or if the file 
         specifies unsigned and the user did not request signed */
      if (mode == MRC_MODE_SHORT && 
          (keepUshort || (!forceSigned && tiff.iifile && 
                          tiff.iifile->type == IITYPE_USHORT)))
        mode = MRC_MODE_USHORT;

      if (tiff.PhotometricInterpretation / 2 == 1 && !makegray){
        mode = MRC_MODE_RGB;
        pixSize = 3;
      }

      printf("Converting %d images size %d x %d\n", 
             tiffPages, xsize, ysize);

      for(section = 0; section < tiffPages; section++){
          
        tifdata = (unsigned char *)tiff_read_section
          (tiffp, &tiff, section);

        if (!tifdata){
          printf("Warning: failed to get image data for "
                 "section %d\n", section);
          continue;
        }

        if (tiff.PhotometricInterpretation == 3)
          expandIndexToRGB(&tifdata, tiff.iifile, section);

        /* convert RGB to gray scale */
        if (tiff.PhotometricInterpretation / 2 == 1 && makegray)
          convertrgb(tifdata, xsize, ysize);

         
        mean += minmaxmean(tifdata, mode, unsign, divide, xsize, 
                           ysize, &min, &max);

        mrc_big_seek( mrcfp, 1024, section, xysize * pixSize, SEEK_SET);
         
        b3dFwrite(tifdata, pixSize, xsize * ysize, mrcfp);
          
        free(tifdata);
      }
      /* write more info to mrc header. 1/17/04 eliminate unneeded rewind */
      hdata.nx = xsize;
      hdata.ny = ysize;
      hdata.mx = hdata.nx;
      hdata.my = hdata.ny;
      hdata.mz = hdata.nz;
      hdata.xlen = hdata.nx;
      hdata.ylen = hdata.ny;
      hdata.zlen = hdata.nz;
      if (mode == MRC_MODE_RGB) {
        hdata.amax = 255;
        hdata.amean = 128.0;
        hdata.amin = 0;
      } else {
        hdata.amax = max;
        hdata.amean = mean / hdata.nz;
        hdata.amin = min;
        printf("Min = %d, Max = %d, Mean = %g\n", min, max, hdata.amean);
      }
      hdata.mode = mode;
      mrc_head_label(&hdata, "tif2mrc: Converted to mrc format.");
      mrc_head_write(mrcfp, &hdata);

      /* cleanup */
      fclose(mrcfp);
      exit(0);

    }
    tiff_close_file(&tiff);
  }
  


  /* read in bg file */
  if (bg){
    if (tiff_open_file(bgfile, openmode, &tiff)) {
      fprintf(stderr, "tif2mrc: Couldn't open %s.\n", bgfile);
      exit(3);
    }
    bgfp = tiff.fp;

    bgdata = (unsigned char *)tiff_read_file(bgfp, &tiff);
    if (!bgdata){
      fprintf(stderr, "tif2mrc: Error reading %s.\n", bgfile);
      exit(3);
    }
    bgBits = tiff.BitsPerSample;
    if ((bgBits != 8 && bgBits !=16) || tiff.PhotometricInterpretation >= 
        2) {
      fprintf(stderr, "tif2mrc: Background file must be 8 or 16-bit "
              "grayscale\n");
      exit(3);
    }

    bgxsize = tiff.directory[WIDTHINDEX].value;
    bgysize = tiff.directory[LENGTHINDEX].value;

    if (bgBits == 8) {
      max = 0;
      min = 255;
        
      for (y = 0; y < bgysize; y++){
        for (x = 0; x < bgxsize; x++){
          tmpdata = bgdata[x + (y * bgxsize)];
          if (tmpdata > max)
            max = tmpdata;
          if (tmpdata < min)
            min = tmpdata;
        }
      }
       
      for (y = 0; y < bgysize; y++){
        for (x = 0; x < bgxsize; x++){
          bgdata[x + (y * bgxsize)] = max - 
            bgdata[x + (y * bgxsize)];
         
        }
      }
    }

    tiff_close_file(&tiff);
  }


  /* Write out mrcheader */
  if (imodBackupFile(argv[argc - 1])) {
    fprintf(stderr, "%s: Error, couldn't create backup", progname);
    exit(3);
  }
  mrcfp = fopen(argv[argc - 1], "wb");
  if (!mrcfp){
    perror("tif2mrc");
    fprintf(stderr, "Error opening %s\n", argv[argc - 1]);
    exit(2);
  }
  mrc_head_new(&hdata, xsize, ysize, argc - i - 1, mode);
  mrc_head_write(mrcfp, &hdata);

  mrcsizeset = i;

  /* Loop through all the tiff files adding them to the MRC stack. */
  for (; i < argc - 1 ; i++){
       
    /* Open the TIFF file. */
    if (tiff_open_file(argv[i], openmode, &tiff)) {
      fprintf(stderr, "tif2mrc: Couldn't open %s.\n", argv[i]);
      exit(3);
    }
    printf("Opening %s for input\n", argv[i]);
    fflush(stdout);
    tiffp = tiff.fp;
       
    /* Read in tiff file */
    tifdata = (unsigned char *)tiff_read_file(tiffp, &tiff);
    if (!tifdata){
      fprintf(stderr, "tif2mrc: Error reading %s.\n", argv[i]);
      exit(3);
    }

    xsize = tiff.directory[WIDTHINDEX].value;
    ysize = tiff.directory[LENGTHINDEX].value;

    if (mrcsizeset == i){
      if (!mrcxsize || !mrcysize) {
        mrcxsize = xsize;
        mrcysize = ysize;
      }
      pixSize = 1;
      if (tiff.BitsPerSample == 16){
        mode = MRC_MODE_SHORT;
        pixSize = 2;
      }

      /* Use unsigned mode either if user requested it or if the file 
         specifies unsigned and the user did not request signed */
      if (mode == MRC_MODE_SHORT && 
          (keepUshort || (!forceSigned && tiff.iifile && 
                          tiff.iifile->type == IITYPE_USHORT)))
        mode = MRC_MODE_USHORT;
      
      if (tiff.PhotometricInterpretation / 2 == 1 && !makegray){
        mode = MRC_MODE_RGB;
        pixSize = 3;
      }
    }

    if ((tiff.BitsPerSample == 16 && mode != MRC_MODE_SHORT && mode != 
         MRC_MODE_USHORT) ||
        (tiff.PhotometricInterpretation / 2 == 1 && !makegray 
        && mode != MRC_MODE_RGB)) {
      fprintf(stderr, "tif2mrc Error: All files must have the same"
              " data type.\n");
      exit(3);
    }

    if (tiff.PhotometricInterpretation == 3)
      expandIndexToRGB(&tifdata, tiff.iifile, 0);

    /* convert RGB to gray scale */
    if (tiff.PhotometricInterpretation / 2 == 1 && makegray)
      convertrgb(tifdata, xsize, ysize);
       
    /* Correct for bg */
    if (bg){
      if ((mode == MRC_MODE_BYTE && bgBits == 16) || 
          (mode == MRC_MODE_SHORT && bgBits == 8)) {
        fprintf(stderr, "tif2mrc Error: Background data must have "
                " the same data type as the image files.\n");
        exit(3);
      }

      xdo = bgxsize < xsize ? bgxsize : xsize;
      ydo = bgysize < ysize ? bgysize : ysize;

      if (mode == MRC_MODE_BYTE) {
        for (y = 0; y < ydo; y++){
          for (x = 0; x < xdo; x++){
            tmpdata = tifdata[x + (y * xdo)] + 
              bgdata[x + (y * xdo)];
            if (tmpdata > 255)
              tmpdata = 255;
            tifdata[x + (y * xdo)] = (unsigned char)tmpdata;
          }
        }
      } else {
        sptr = (b3dInt16 *)tifdata;
        bgshort = (b3dInt16 *)bgdata;
        for (y = 0; y < ydo; y++)
          for (x = 0; x < xdo; x++)
            sptr[x + (y * xdo)] -= bgshort[x + (y * xdo)];
         
      }
    }
       
    tmean = minmaxmean(tifdata, mode, unsign, divide, xsize, ysize, &min,
                       &max);
    mean += tmean;

    if ((xsize == mrcxsize) && (ysize == mrcysize)){
      /* Write out mrc file */    

      b3dFwrite(tifdata , pixSize, xsize*ysize, mrcfp);

    }else{
      fprintf(stderr, "tif2mrc: Warning, file %s not same size.\n", argv[i]);

      /* Unequal sizes: set the fill value and pointer */
      switch (mode) {
      case MRC_MODE_BYTE:
        byteFill[0] = tmean;
        fillPtr = &byteFill[0];
        break;
      case MRC_MODE_RGB:
        byteFill[0] = byteFill[1] = byteFill[2] = 128;
        fillPtr = &byteFill[0];
        break;
      case MRC_MODE_SHORT:
        shortFill = tmean;
        fillPtr = &shortFill;
        break;
      case MRC_MODE_USHORT:
        ushortFill = tmean;
        fillPtr = &ushortFill;
        break;
      }

      /* Output centered data */
      yoffset = (ysize - mrcysize) / 2;
      xoffset = (xsize - mrcxsize) / 2;
      for (y = 0; y < mrcysize; y++) {
        if (y + yoffset < 0 || y + yoffset >= ysize) {

          /* Do fill lines */
          for (x = 0; x < mrcxsize; x++)
            b3dFwrite(fillPtr, pixSize, 1, mrcfp);
        } else {

          /* Fill left edge if necessary, write data, fill right if needed */
          for (x = xoffset; x < 0; x++)
            b3dFwrite(fillPtr, pixSize, 1, mrcfp);
          xdo = pixSize * (B3DMAX(0, xoffset) + (y + yoffset) * xsize);
          b3dFwrite (&tifdata[xdo], pixSize, B3DMIN(xsize, mrcxsize), mrcfp);
          for (x = 0; x < mrcxsize - xsize + xoffset; x++)
            b3dFwrite(fillPtr, pixSize, 1, mrcfp);
        }
      }
    }

    tiff_close_file(&tiff);

    if (tifdata)
      free (tifdata);
    tifdata = NULL;

  }
  
  /* write more info to mrc header. 1/17/04 eliminate unneeded rewind */
  hdata.nx = mrcxsize;
  hdata.ny = mrcysize;
  hdata.mx = hdata.nx;
  hdata.my = hdata.ny;
  hdata.mz = hdata.nz;
  hdata.xlen = hdata.nx;
  hdata.ylen = hdata.ny;
  hdata.zlen = hdata.nz;
  if (mode == MRC_MODE_RGB) {
    hdata.amax = 255;
    hdata.amean = 128.0;
    hdata.amin = 0;
  } else {
    hdata.amax = max;
    hdata.amean = mean / hdata.nz;
    hdata.amin = min;
    printf("Min = %d, Max = %d, Mean = %g\n", min, max, hdata.amean);
  }
  hdata.mode = mode;
  mrc_head_label(&hdata, "tif2mrc: Converted to MRC format.");
  mrc_head_write(mrcfp, &hdata);

  /* cleanup */
  if (mrcfp)
    fclose(mrcfp);

  exit(0);

}

static void convertrgb(unsigned char *tifdata, int xsize, int ysize)
{
  unsigned char *out, *in;
  int x, pixel;
  in = tifdata;
  out = tifdata;
     
  for (x = 0; x < xsize*ysize; x++) {
    pixel = *in++;
    pixel += *in++;
    pixel += *in++;
    *out++ = pixel / 3;
  }
}

static void expandIndexToRGB(unsigned char **datap, ImodImageFile *iifile, 
                              int section)
{
  unsigned char *indata = *datap;
  unsigned char *outdata, *map;
  int xysize, i, ind;
  if (!iifile || !iifile->colormap) {
    fprintf(stderr, "tif2mrc Error: Colormap data not read in properly.\n");
    exit(3);
  }
  xysize = iifile->nx * iifile->ny;
  outdata = (unsigned char *)malloc(3 * xysize);
  if (!outdata) {
    fprintf(stderr, "tif2mrc Error: Insufficient memory.\n");
    exit(3);
  }
  *datap = outdata;

  map = &iifile->colormap[768 * section];
  for (i = 0; i < xysize; i++) {
    ind = indata[i];
    *outdata++ = map[ind];
    *outdata++ = map[256 + ind];
    *outdata++ = map[512 + ind];
  }
}


static float minmaxmean(unsigned char *tifdata, int mode, int unsign, 
                        int divide, int xsize, int ysize, int *min, int *max)
{
  float tmean = 0.;
  int x, y, pixel;
  b3dInt16 *sptr;
  b3dUInt16 *usptr;

  if (mode == MRC_MODE_BYTE)
    for (x = 0; x < xsize * ysize; x++) {
      pixel = tifdata[x];
      if (pixel < *min)
        *min = pixel;
      if(pixel > *max)
        *max = pixel;
      tmean += pixel;
    }

  if (mode == MRC_MODE_SHORT) {
    sptr = (b3dInt16 *)tifdata;

    /* DNM 11/17/01: if unsigned ints, either divide by 2 or 
       subtract 32768 to get into range of signed ints */
    if (unsign) {
      usptr = (b3dUInt16 *)tifdata;
      if (divide) {
        for (x = 0; x < xsize * ysize; x++) {
          pixel = usptr[x];
          pixel /= 2;
          sptr[x] = pixel;
        }
      } else {
        for (x = 0; x < xsize * ysize; x++) {
          pixel = usptr[x];
          sptr[x] = pixel - 32768;
        }
      }
    }
    for (x = 0; x < xsize * ysize; x++) {
      pixel = sptr[x];
      if (pixel < *min)
        *min = pixel;
      if(pixel > *max)
        *max = pixel;
      tmean += pixel;
    }
  }
  if (mode == MRC_MODE_USHORT) {
    usptr = (b3dUInt16 *)tifdata;
    for (x = 0; x < xsize * ysize; x++) {
      pixel = usptr[x];
      if (pixel < *min)
        *min = pixel;
      if(pixel > *max)
        *max = pixel;
      tmean += pixel;
    }
  }
  return (tmean / (xsize * ysize));
}

/* 
   $Log$
   Revision 3.15  2007/10/15 21:36:24  mast
   Fixed output of unequal sized data to use b3dFwrite instead of fputc, made
   it put out centered data and work for all modes, added output size option

   Revision 3.14  2006/08/28 05:26:44  mast
   Added abiity to handle colormapped images
   
   Revision 3.13  2006/01/13 05:00:50  mast
   Added option to suppress reading of multiple pages.
   
   Revision 3.12  2005/11/11 21:55:28  mast
   Outputs unsigned file mode
   
   Revision 3.11  2005/02/11 01:42:34  mast
   Warning cleanup: implicit declarations, main return type, parentheses, etc.
   
   Revision 3.10  2004/11/05 18:53:10  mast
   Include local files with quotes, not brackets
   
   Revision 3.9  2004/09/10 21:33:31  mast
   Eliminated long variables
   
*/

