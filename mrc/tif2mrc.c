/*
 *  tif2mrc -- Convert TIFF image files to MRC image files.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.10  2004/11/05 18:53:10  mast
    Include local files with quotes, not brackets

    Revision 3.9  2004/09/10 21:33:31  mast
    Eliminated long variables

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
  int unsign = FALSE;
  int divide = FALSE;
  int xsize = XSIZE, ysize = YSIZE;
  int mrcxsize, mrcysize, mrcsizeset;
  int bpix;
  float mean, tmean;
  int pixel;
  short int *sptr;
  short int *bgshort;
  int bgBits, bgxsize, bgysize, xdo, ydo;
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
    fprintf(stderr, "\t-b file Background subtract image in given file\n");

    exit(3);
  }




  for (i = 1; i < argc - 1 ; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){
         
      case 'h': /* help */
        break;

      case 'f': /* fill in holes */
        break;

      case 'c': /* float to common mean */
         
        break;

      case 'm': /* float to given mean */
        break;

      case 's': /* stretch data out to min and max */
        break;

      case 'g': /* convert rgb to gray scale */
        makegray = TRUE;
        break;
 
      case 'u': /* treat ints as unsigned */
        unsign = TRUE;
        break;
 
      case 'd': /* treat ints as unsigned and divide by 2*/
        divide = TRUE;
        unsign = TRUE;
        break;
 
      case 'b':
        bgfile = strdup(argv[++i]);
        /*  bgfp = fopen(argv[++i], "rb"); */
        bg = TRUE;
        break;
         
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


  if (i == (argc - 2)){

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
      if (tiff.PhotometricInterpretation == 2 && !makegray){
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

        /* convert RGB to gray scale */
        if (tiff.PhotometricInterpretation == 2 && makegray)
          convertrgb(tifdata, xsize, ysize);

         
        mean += minmaxmean(tifdata, mode, unsign, divide, xsize, 
                           ysize, &min, &max);

        mrc_big_seek( mrcfp, 1024, section, xysize * pixSize, SEEK_SET);
         
        b3dFwrite(tifdata, pixSize, xsize * ysize, mrcfp);
        /* linePtr = tifdata;
           linePtr += xsize*ysize*pixSize;
           linePtr -= xsize*pixSize;
           for (y = 0; y < ysize ; y++){
           fwrite(linePtr, pixSize, xsize, mrcfp);
           linePtr-=xsize*pixSize;
           } */
         
          
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
    if ((bgBits != 8 && bgBits !=16) || tiff.PhotometricInterpretation == 
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

  mrcxsize = mrcysize = 0;
  mrcsizeset = i;



  /* Loop through all the tiff files adding them to the MRC stack. */
  for (; i < argc - 1 ; i++){
       
    /* Open the TIFF file. */
    if (tiff_open_file(argv[i], openmode, &tiff)) {
      fprintf(stderr, "tif2mrc: Couldn't open %s.\n", argv[i]);
      exit(3);
    }
    printf("Opening %s for input\n", argv[i]);
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
      mrcxsize = xsize;
      mrcysize = ysize;
      pixSize = 1;
      if (tiff.BitsPerSample == 16){
        mode = MRC_MODE_SHORT;
        pixSize = 2;
      }
      if (tiff.PhotometricInterpretation == 2 && !makegray){
        mode = MRC_MODE_RGB;
        pixSize = 3;
      }
    }

    if ((tiff.BitsPerSample == 16 && mode != MRC_MODE_SHORT) ||
        (tiff.PhotometricInterpretation == 2 && !makegray 
        && mode != MRC_MODE_RGB)) {
      fprintf(stderr, "tif2mrc Error: All files must have the same"
              " data type.\n");
      exit(3);
    }


    /* convert RGB to gray scale */
    if (tiff.PhotometricInterpretation == 2 && makegray)
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
        sptr = (short int *)tifdata;
        bgshort = (short int *)bgdata;
        for (y = 0; y < ydo; y++)
          for (x = 0; x < xdo; x++)
            sptr[x + (y * xdo)] -= bgshort[x + (y * xdo)];
         
      }
    }
       
    mean += minmaxmean(tifdata, mode, unsign, divide, xsize, ysize, &min,
                       &max);

    if ((xsize == mrcxsize) && (ysize == mrcysize)){
      /* Write out mrc file */    

      b3dFwrite(tifdata , pixSize, xsize*ysize, mrcfp);
      /*        unsigned char *linePtr = tifdata;
                linePtr += xsize*ysize*pixSize;
                linePtr -= xsize*pixSize;
                for (y = 0; y < ysize ; y++){
                fwrite(linePtr, pixSize, xsize, mrcfp);
                linePtr-=xsize*pixSize;
                } */

    }else{
      fprintf(stderr, "tif2mrc: Warning, file %s not same size.\n",
              argv[i]);
      if (pixSize > 1){
        fprintf(stderr, "tif2mrc Error: Non byte data must be"
                " the same size.\n");
        exit(3);
      }
      /* todo: center file if not same size. */
      /*  for(y = mrcysize - 1; y >= 0; y--) */
      for(y = 0; y < mrcysize; y++)
        for(x = 0; x < mrcxsize; x++){
          bpix = 128;
          if ((y < ysize) && (x < xsize))
            bpix = tifdata[x + y * xsize];
          /* bpix = tifdata[x + ((ysize - y - 1) * xsize)]; */
          fputc(bpix, mrcfp);
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

static float minmaxmean(unsigned char *tifdata, int mode, int unsign, 
                        int divide, int xsize, int ysize, int *min, int *max)
{
  float tmean = 0.;
  int x, y, pixel;
  short int *sptr;
  unsigned short int *usptr;

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
    sptr = (short int *)tifdata;

    /* DNM 11/17/01: if unsigned ints, either divide by 2 or 
       subtract 32768 to get into range of signed ints */
    if (unsign) {
      usptr = (unsigned short int *)tifdata;
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
  return (tmean / (xsize * ysize));
}
