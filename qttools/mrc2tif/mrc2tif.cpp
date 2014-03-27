/*
 *  mrc2tif.cpp -- Convert mrc files to TIFF and other files.
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <qimage.h>
#include <qfile.h>
#include <qapplication.h>
#include <qstringlist.h>
#include "b3dtiff.h"
#include "b3dutil.h"
#include "parse_params.h"

static FILE *openEitherWay(ImodImageFile *iifile, char *iname, char *progname,
                           int oldcode);

int main(int argc, char *argv[])
{
  FILE *fin;
  MrcHeader hdata;
  FILE *fpTiff = NULL;
  Islice slice;
  char *plugdir;
  QStringList strList;
  int xsize, ysize, zsize, psize, filenum, outPsize, numChunks, chunk;
  int lines, linesDone, version, linesPerChunk = 0, doChunks = 0;
  size_t xysize, allocSize, lineBytes;
  int i, j, slmode, z, iarg = 1, stack = 0, resolution = 0, initialNum = -1;
  int oldcode = 0, convert = 0, usePixel = 0;
  int compression = 1, black = 0, white = 255, zmin = -1, zmax = -1;
  int quality = -1;
  int makeJPG = 0;
  int makePNG = 0;
  int typeInd, digits = 3;
  bool makeQimage;
  b3dUInt32 ifdOffset = 0, dataOffset = 0;
  float chunkCriterion = 100.;
  float savecrit, dmin, dmax, smin =0., smax = 0.;
  int xTileSize = 0;
  float val[3];
  float scale, offset, xscale, yscale, zscale, allMin, allMax, sliceMin, sliceMax;
  char prefix[100];
  char format[14];
  IloadInfo li;
  ImodImageFile *iifile = iiNew();
  char *iname;
  unsigned char *buf, *qbuf, *qtmp;
  char *progname = imodProgName(argv[0]);
#define NUM_LEGAL 12
  int legalComp[NUM_LEGAL] = {1, 2, 3, 4, 5, 7, 8, 32771, 32773, 32946, 32845,
                              32845};
  const char *extensions[3] = {"tif", "jpg", "png"};
  const char *typeNames[3] = {"TIFF", "JPEG", "PNG"};

  sprintf(prefix, "\nERROR: %s - ", progname);
  setExitPrefix(prefix);

  for (iarg = 1; iarg < argc - 1 ; iarg++){
    if (argv[iarg][0] == '-'){
      switch (argv[iarg][1]){
      case 's':
        stack = 1;
        break;

      case 'o':
        oldcode = 1;
        break;

      case 'c':
        iarg++;
        if (!strcmp(argv[iarg], "zip"))
          compression = 8;
        else if (!strcmp(argv[iarg], "jpeg"))
          compression = 7;
        else if (!strcmp(argv[iarg], "lzw"))
          compression = 5;
        else
          compression = atoi(argv[iarg]);
        for (i = 0; i < NUM_LEGAL; i++)
          if (compression == legalComp[i])
            break;
        if (i == NUM_LEGAL)
          exitError("Compression value %d not allowed", compression);
        break;

      case 'r':
        resolution = atoi(argv[++iarg]);
        break;

      case 'P':
        usePixel = 1;
        break;

      case 'p':
        makePNG = 1;
        break;

      case 'j':
        makeJPG = 1;
        break;

      case 'S':
        sscanf(argv[++iarg], "%f%*c%f", &smin, &smax);
        convert = 1;
        break;

      case 'C':
        sscanf(argv[++iarg], "%d%*c%d", &black, &white);
        convert = 1;
        break;

      case 'z':
        sscanf(argv[++iarg], "%d%*c%d", &zmin, &zmax);
        break;

      case 'i':
        initialNum = atoi(argv[++iarg]);
        break;

      case 'q':
        quality = atoi(argv[++iarg]);
        break;

      case 'T':
        sscanf(argv[++iarg], "%d%*c%d", &xTileSize, &linesPerChunk);
        if (!linesPerChunk)
          linesPerChunk = xTileSize;
        doChunks = 1;
        break;

      case 't':
        chunkCriterion = atof(argv[++iarg]);
        break;

      default:
        break;
      }
      
    }
    else
      break;
       
  }

  if (oldcode && compression != 1)
    exitError("Compression not available with old writing code");
  if (oldcode && (resolution > 0 || usePixel))
    exitError("Resolution setting is not available with old writing code");
  if (oldcode && xTileSize)
    exitError("Tiling not available with old writing code");
  if (oldcode && (makeJPG || makePNG))
    exitError("JPEG and PNG output not available with old writing code");
  if (resolution > 0 && usePixel)
    exitError("You cannot enter both -r and -P");
  makeQimage = makeJPG || makePNG;
  typeInd = makeJPG + 2 * makePNG;
  if (makeQimage && (stack || doChunks || compression != 1 || 
                               xTileSize))
    exitError("You cannot enter -s, -c, or -T with JPEG and PNG output");
  if (makePNG && makeJPG)
    exitError("You cannot enter both -j and -p");
  if ((compression == 8 || makePNG) && (quality < -1 || quality == 0 || quality > 9))
    exitError("Quality for ZIP compression or PNG output must be between 1 and 9");

  if (argc - iarg != 2){
    printf("%s version %s \n", progname, VERSION_NAME);
    imodCopyright();
    printf("%s [options] <mrc file> <tiff name/root>\n\n", progname);
    printf(" Without -s, a series of tiff files will be created "
            "with the\n prefix [tiff root name] and with the suffix nnn.tif, "
           "where nnn is the z number. \n  Options:\n");
    printf("    -s         Stack all images in the mrc file into a single tiff"
           " file\n");
    printf("    -c val     Compress data; val can be lzw, zip, jpeg, or "
           "numbers defined\n\t\t in libtiff\n");
    printf("    -q #       Quality for jpeg compression (0-100) or for zip "
           "compression (1-9)\n");
    printf("    -S min,max Initial scaling limits for conversion to bytes\n");
    printf("    -C b,w     Contrast black/white values for conversion to "
           "bytes\n");
    printf("    -z min,max Starting and ending Z (from 0) to output\n");
    printf("    -i #       Initial file number (default is starting Z)\n");
    printf("    -r #       Resolution setting in dots per inch\n");
    printf("    -P         Use pixel spacing in MRC header for resolution setting\n");
    printf("    -T nx[,ny] Output data in tiles of size nx by ny (nx by nx if "
           "ny omitted)\n");
    
    printf("    -t #       Criterion image size in megabytes for processing "
           "file in strips\n");
    printf("    -o         Write file with old IMOD code instead of libtiff"
           "\n");
    exit(1);
  }

  // Do some initial things for Qimage output
  if (makeQimage) {

    // Put plugin dir on the library path so image plugins can be found
    plugdir = getenv("IMOD_PLUGIN_DIR");
    if (plugdir)
      strList << plugdir;
    plugdir = getenv("IMOD_DIR");
    if (plugdir)
      strList << QString(plugdir) + QString("/lib/imodplug");
    for (i = 0; i < strList.count(); i++)
      if (QFile::exists(strList[i] + "/imageformats"))
        break;
    if (strList.count() && i < strList.count())
      QApplication::setLibraryPaths(strList);

    // Convert the quality for QImage use by inverting and multiplying by 10 
    // Verified that the ZIP default of 6 is equivalent to Qt default of 30
    if (makePNG && quality >= 0)
        quality = 10 * (9 - quality);
  }
    
  // Get input file and header
  if (NULL == (fin = iiFOpen(argv[iarg], "rb")))
    exitError("Couldn't open %s", argv[iarg]);

  if (mrc_head_read(fin, &hdata))
    exitError("Can't Read Input Header from %s", argv[iarg]);
  iarg++;
  if (usePixel) {
    mrc_get_scale(&hdata, &xscale, &yscale, &zscale);
    resolution = 2.54e8 / xscale;
  }

  // Adjust Z limits and file numbering
  if (zmin == -1 && zmax == -1) {
    zmin = 0;
    zmax = hdata.nz - 1;
  } else if (zmin < 0 || zmax >= hdata.nz || zmin > zmax)
    exitError("zmin,zmax values are reversed or out of the range 0 to %d\n",
              hdata.nz - 1);
  if (initialNum < 0)
    filenum = zmin;
  else
    filenum = initialNum;

  // Set up output file structure based on mode
  iifile->format = IIFORMAT_LUMINANCE;
  iifile->file = IIFILE_TIFF;
  iifile->type = IITYPE_UBYTE;
  iifile->amin = iifile->amax = 0;

  switch(hdata.mode){
      
  case MRC_MODE_BYTE:
    psize = 1;
    break;
  case MRC_MODE_SHORT:
    psize = 2;
    iifile->type = IITYPE_SHORT;
    break;
  case MRC_MODE_USHORT:
    psize = 2;
    iifile->type = IITYPE_USHORT;
    break;
  case MRC_MODE_RGB:
    psize = 3;
    iifile->format = IIFORMAT_RGB;
    break;
  case MRC_MODE_FLOAT:
    psize = 4;
    iifile->type = IITYPE_FLOAT;
    break;
  default:
    exitError("Data mode %d not supported.", hdata.mode);
    break;
  }

  // Must convert to bytes if it is not color for PNG/JPEG output
  if (makeQimage && hdata.mode != MRC_MODE_BYTE && hdata.mode != MRC_MODE_RGB)
    convert = 1;
  xsize = hdata.nx;
  ysize = hdata.ny;
  zsize = hdata.nz;
  xysize = (size_t)xsize * (size_t)ysize;
  allocSize = xysize * psize;
  iifile->nx = xsize;
  iifile->ny = ysize;
  iifile->nz = 1;
  mrc_init_li(&li, NULL);
  li.xmin = 0;
  li.xmax = xsize - 1;
  li.ymin = 0;
  li.ymax = ysize - 1;
  dmin = hdata.amin;
  dmax = hdata.amax;
  slmode = sliceModeIfReal(hdata.mode);
  outPsize = psize;
  if (convert) {
    mrcContrastScaling(&hdata, smin, smax, black, white, MRC_RAMP_LIN, &scale, 
                       &offset);
    if (slmode > 0) {
      iifile->type = IITYPE_UBYTE;
      outPsize = 1;
    }
  }

  if (!makeQimage) {

    // Check version and size and set up chunks for TIFF
    version = tiffVersion(&j);
    /* printf("Version %d.%d\n", version,j); */
    if (version < 4) {
      savecrit =  4.292e9;
      if (!version || oldcode)
        savecrit =  2.146e9;
      if ((double)xysize * outPsize >  savecrit)
        exitError("The image is too large in X/Y to save with TIFF version"
                  " %d.%d", version, j);
      if (stack && (zmax + 1 - zmin) * xysize * (double)outPsize >  savecrit)
        exitError("The volume is too large to save in a stack with TIFF version"
                  " %d.%d", version, j);
    }
    if (version > 0 && !oldcode && 
        ((double)xsize * ysize) * psize > chunkCriterion * 1024. * 1024.)
      doChunks = 1;

  } else {

    // Check size for JPEG
    if (makeJPG && (xsize > 65535 || ysize > 65535))
      exitError("The input image is too large in X or Y for JPEG output");

    // Round lines up to 32-bit boundary and get the number of bytes per output line
    // and fix allocated size if necessary, get line buffer
    lineBytes = 4 * ((xsize * outPsize + 3) / 4);
    if (allocSize < lineBytes * ysize)
      allocSize = lineBytes * ysize;
    qtmp = (unsigned char *)malloc(lineBytes);
    if (!qtmp)
      exitError("Failed to allocate memory for line buffer");
  }

  iname = (char *)malloc(strlen(argv[iarg]) + 20);
  if (!iname)
    exitError("Failed to allocate memory for name");

  /* Open new file for stack */
  if (stack) {
    sprintf(iname, "%s", argv[iarg]);
    iifile->nz = zmax + 1 - zmin;
    fpTiff = openEitherWay(iifile, iname, progname, oldcode);
  }    

  // Set up format for numbered filenames
  if (zmax + 1 - zmin >= 10000)
    digits = 5;
  else if (zmax + 1 - zmin >= 1000)
    digits = 4;
  sprintf(format, "%%s.%%0%dd.%%s", digits);

  allMin = 1.e30;
  allMax = -1.e30;

  printf("Writing %s images. ", typeNames[typeInd]);
  for (z = zmin; z <= zmax; z++){
    int tiferr;
    printf(".");
    fflush(stdout);

    /* Get filename and open new file for non-stack */
    if (!stack) {
      sprintf(iname, format, argv[iarg], filenum++, extensions[typeInd]);
      if (zsize == 1)
        sprintf(iname, "%s", argv[iarg]);
      if (!makeQimage) {
        if (z > zmin)
          free(iifile->filename);
        fpTiff = openEitherWay(iifile, iname, progname, oldcode);
        ifdOffset = 0;
        dataOffset = 0;
      }
    }

    // Prepare to get min/max for a slice, but send file min/max to slice initially
    sliceMin = 1.e30;
    sliceMax = -1.e30;
    iifile->amin = dmin;
    iifile->amax = dmax;
    
    /* If doing chunks, set up chunk loop */
    numChunks = 1;
    if (doChunks) {
      tiffWriteSetup(iifile, compression, 0, resolution, quality, 
                     &linesPerChunk, &numChunks, &xTileSize);
      if (xTileSize > xsize + 16 || linesPerChunk > ysize + 16)
        exitError("Entered tile size was too large");
      allocSize = xsize * linesPerChunk * psize;
      linesDone = 0;
    }
    
    for (chunk = 0; chunk < numChunks; chunk++) {

      /* Allocate memory first time or every time */
      if ((!chunk && z == zmin) || (convert && slmode > 0))
        qbuf = buf = (unsigned char *)malloc(allocSize);
      if (!buf)
        exitError("Failed to allocate memory for slice");

      lines = ysize;
      if (doChunks) {
        lines = B3DMIN(linesPerChunk, ysize - linesDone);
        li.ymin = ysize - (linesDone + lines);
        li.ymax = li.ymin + lines - 1;
        linesDone += lines;
      }

      if (mrcReadZ(&hdata, &li, buf, z)) {
        perror("mrc2tif ");
        exitError("Reading section %d", z);
      }
      sliceInit(&slice, xsize, lines, hdata.mode, buf);
      
      /* Scale the slice and convert it to bytes */
      if (convert) {
        for (j = 0; j < lines; j++) {
          for (i = 0; i < xsize; i++) {
            sliceGetVal(&slice, i, j, val);
            val[0] = val[0] * scale + offset;
            val[0] = B3DMAX(0., B3DMIN(255., val[0]));
            if (psize == 3) {
              val[1] = val[1] * scale + offset;
              val[1] = B3DMAX(0., B3DMIN(255., val[1]));
              val[2] = val[2] * scale + offset;
              val[2] = B3DMAX(0., B3DMIN(255., val[2]));
            }
            slicePutVal(&slice, i, j, val);
          }
        }
        if (slmode > 0 && sliceNewMode(&slice, SLICE_MODE_BYTE))
          exitError("Converting slice %d to bytes", z);
        qbuf = buf = slice.data.b;
        if (!buf)
          exitError("Failed to allocate memory for slice");
        if (makeQimage && lineBytes != xsize) {
          qbuf = (unsigned char *)malloc(lineBytes * ysize);
          if (!qbuf)
            exitError("Failed to allocate memory for unpacking image");
        }
      }
    
      // Get min/max for all image types, even bytes
      if (!makeQimage) {
        sliceMMM(&slice);
        sliceMin = B3DMIN(sliceMin, slice.min);
        sliceMax = B3DMAX(sliceMax, slice.max);
        allMin = B3DMIN(allMin, slice.min);
        allMax = B3DMAX(allMax, slice.max);
        if (!doChunks) {
          iifile->amin = sliceMin;
          iifile->amax = sliceMax;
        }
      }

      if (makeQimage) {

        // For Qimage, Spread out the image to give the aligned lines needed
        if (lineBytes != xsize * outPsize) {
          for (j = ysize - 1; j >= 0; j--)
            for (i = xsize * outPsize - 1; i >= 0; i--)
              qbuf[i + lineBytes * j] = buf[i + j * (size_t)xsize * outPsize];
        }

        // Invert the image
        for (j = 0; j < ysize / 2; j++) {
          memcpy(qtmp, &qbuf[lineBytes * j], lineBytes);
          memcpy(&qbuf[lineBytes * j], &qbuf[lineBytes * (ysize - 1 - j)], lineBytes);
          memcpy(&qbuf[lineBytes * (ysize - 1 - j)], qtmp, lineBytes);
        }

        // Create the QImage, set resolution if it exists, set color table for 8-bit
        QImage *qim = new QImage(qbuf, xsize, ysize, (int)lineBytes, outPsize > 1 ? 
                                 QImage::Format_RGB888 : QImage::Format_Indexed8);
        if (resolution) {
          qim->setDotsPerMeterX(resolution / 0.0254);
          qim->setDotsPerMeterY(resolution / 0.0254);
        }
        if (outPsize == 1)
          for (i = 0; i < 256; i++)
            qim->setColor(i, qRgb(i, i, i));
        tiferr = qim->save(iname, typeNames[typeInd], quality) ? 0 : 1;
        delete qim;
        if (qbuf != buf)
          free(qbuf);

        // Back to regular TIFF saving
      } else if (fpTiff)
        tiferr = tiff_write_image(fpTiff, xsize, ysize, hdata.mode, buf, 
                                  &ifdOffset, &dataOffset, dmin, dmax);
      else if (!doChunks)
        tiferr = tiffWriteSection(iifile, buf, compression, 0, resolution,
                                  quality);
      else
        tiferr = tiffWriteStrip(iifile, chunk, buf);
      if (tiferr) {
        exitError("Error (%d) writing section %d to %s", tiferr, z, iname);
      }
      if (convert && slmode > 0)
        free(slice.data.b);
    }

    // Finish up the file; set the min/max for real now
    iifile->amin = sliceMin;
    iifile->amax = sliceMax;
    if (doChunks)
      tiffWriteFinish(iifile);
    
    if (!stack && !makeQimage) {
      if (fpTiff) {
        fclose(fpTiff);
        fpTiff = NULL;
      } else
        iiClose(iifile);
    }
  }

  printf("\r\n");

  // Finish up the stack; give the last image the full min/max
  iifile->amin = allMin;
  iifile->amax = allMax;
  if (stack) {
    if (fpTiff)
      fclose(fpTiff);
    else
      iiClose(iifile);
  }
  iiFClose(fin);
  if (xTileSize)
    printf("Actual tile size = %d x %d\n", xTileSize, linesPerChunk);
  exit(0);
}

static FILE *openEitherWay(ImodImageFile *iifile, char *iname, char *progname,
                           int oldcode)
{
  static int warned = 0;
  FILE *fpTiff = NULL;
  iifile->filename = strdup(iname);
  
  if (oldcode || tiffOpenNew(iifile)) {
    fpTiff = fopen(iname, "wb");
    if (!fpTiff){
      perror("mrc2tif system message");
      exitError("Opening %s", iname);
    } else if (!oldcode && !warned) {
      warned = 1;
      printf("\nWARNING: %s - Not writing with libtiff, compression not "
             "available", progname);
    }
  }
  return fpTiff;
}
