/*
 *    iilikemrc.c - Check for recognizable formats that can be read like MRC
 *
 *    Author:  David Mastronarde
 *
 *   Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *
 *  $Id$
 *  Log at end
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "mrcfiles.h"
#include "iimage.h"
#include "ilist.h"
#include "b3dutil.h"

#define MAX_EM_MACHINES 20
#define MAX_EM_TYPES    20
#define MAX_EM_SIZE     1.6e10

static int checkWinkler(FILE *fp, char *filename, RawImageInfo *info);
static int checkDM3(FILE *fp, char *filename, RawImageInfo *info);
static int checkEM(FILE *fp, char *filename, RawImageInfo *info);
static int checkPif(FILE *fp, char *filename, RawImageInfo *info);
static char *foundDMtag(char *buf, const char *tag, const char *fullTag, int dmind,
                        int dm4Offset);

/* The resident check list */
static Ilist *checkList = NULL;

/* Structure for items to check */
typedef struct check_entry {
  IIRawCheckFunction func;
  char *name;
} CheckEntry;

/* Initialize check list: if it does not exist, allocate it and place resident
   functions on it */
static int initCheckList()
{
  if (checkList)
    return 0;
  checkList = ilistNew(sizeof(CheckEntry), 6);
  if (!checkList)
    return 1;
  iiAddRawCheckFunction(checkEM, "EM");
  iiAddRawCheckFunction(checkDM3, "DM3");
  iiAddRawCheckFunction(checkWinkler, "Winkler");
  iiAddRawCheckFunction(checkPif, "PIF");
  return 0;
}

/*!
 * Add the given raw-type checking function [func] to the front of the checking
 * list; [name] is a name for the format.  The definition of such a function is
 * ^  int checkMyFormat(FILE *fp, char *filename, RawImageInfo *info)
 */
void iiAddRawCheckFunction(IIRawCheckFunction func, const char *name)
{
  CheckEntry item;
  item.func = func;
  item.name = strdup(name);
  if (initCheckList() || !item.name)
    return;
  ilistInsert(checkList, &item, 0);
}

/*!
 * Frees the checking list and all its data to avoid memory leaks
 */
void iiDeleteRawCheckList()
{
  int i;
  CheckEntry *item;
  if (!checkList)
    return;
  for (i = 0; i < ilistSize(checkList); i++) {
    item = (CheckEntry *)ilistItem(checkList, i);
    if (item->name)
      free(item->name);
  }
  ilistDelete(checkList);
  checkList = NULL;
}

/*!
 * Checks the image file in [inFile] for one known MRC-like (raw-type) format
 * after another.
 * Returns IIERR codes for errors.  b3dError is called with a message for all
 * errors that occur during checking, except for IIERR_NOT_FORMAT.
 */
int iiLikeMRCCheck(ImodImageFile *inFile)
{
  FILE *fp; 
  int i;
  RawImageInfo info;
  int err;
  CheckEntry *item;

  info.swapBytes = 0;
  info.sectionSkip = 0;
  info.yInverted = 0;
  info.pixel = 0.;
  info.zPixel = 0.;

  if (!inFile) 
    return IIERR_BAD_CALL;
  fp = inFile->fp;
  if (!fp)
    return IIERR_BAD_CALL;
  if (initCheckList())
    return IIERR_BAD_CALL;

  for (i = 0; i < ilistSize(checkList); i++) {
    item = (CheckEntry *)ilistItem(checkList, i);
    
    if (!(err = (*item->func)(fp, inFile->filename, &info)))
      return(iiSetupRawHeaders(inFile, &info));
      
    if (err != IIERR_NOT_FORMAT) {
      if (err == IIERR_IO_ERROR)
        b3dError(stderr, "ERROR: iiCheckLikeMRC - reading from file %s\n", 
                 inFile->filename);
      else if (err == IIERR_NO_SUPPORT)
        b3dError(stderr, "ERROR: iiCheckLikeMRC - unsupported data mode of "
                 "%s-type file.\n", item->name);
      return err;
    }
  }
  
  return IIERR_NOT_FORMAT;
}

/*!
 * Creates an MRC header and fills it and the items in [inFile] from the
 * information in [info]; specifically the {nx}, {ny}, {nz}, {swapBytes},
 * {headerSize}, {sectionSkip}, {yInverted}, and {type} members.
 * Returns IOERR_MEMORY_ERR for error allocating header.
 */
int iiSetupRawHeaders(ImodImageFile *inFile, RawImageInfo *info)
{
  int modeTable[] = {MRC_MODE_BYTE, MRC_MODE_BYTE, MRC_MODE_SHORT, MRC_MODE_USHORT,
                     MRC_MODE_FLOAT, MRC_MODE_COMPLEX_FLOAT, MRC_MODE_RGB};
  MrcHeader *hdr;

  /* Get an MRC header; set sizes into that header and the iifile header */
  hdr = (MrcHeader *)malloc(sizeof(MrcHeader));
  if (!hdr) {
    b3dError(stderr, "ERROR: iiSetupRawHeaders - Getting memory for header");
    return IIERR_MEMORY_ERR;
  }
  mrc_head_new(hdr, info->nx, info->ny, info->nz, modeTable[info->type]);
  inFile->file = IIFILE_RAW;
  hdr->swapped = info->swapBytes;
  hdr->headerSize = info->headerSize;
  hdr->sectionSkip = info->sectionSkip;
  hdr->yInverted = info->yInverted;
  hdr->bytesSigned = info->type == RAW_MODE_SBYTE ? 1 : 0;
  hdr->fp = inFile->fp;

  /* Pass on a min and max of 0 as a sign that there is no min/max */
  hdr->amin = info->amin;
  hdr->amax = info->amax;
  if (info->pixel)
    mrc_set_scale(hdr, (double)info->pixel, (double)info->pixel, 
                  (double)(info->zPixel ? info->zPixel : info->pixel));
  inFile->header = (char *)hdr;
  iiMRCmodeToFormatType(inFile, hdr->mode, hdr->bytesSigned);
  iiSyncFromMrcHeader(inFile, hdr);

  /* Set the access routines; just use the MRC routines */
  iiMRCsetIOFuncs(inFile, 1);
  inFile->cleanUp = iiLikeMRCDelete;
  return 0;
}

void iiLikeMRCDelete(ImodImageFile *inFile)
{
  if (inFile->header)
    free(inFile->header);
}

/*
 * Check for the Winkler format
 */
static int checkWinkler(FILE *fp, char *filename, RawImageInfo *info)
{
  b3dUInt16 svals[4];
  b3dInt32 ivals[12];

  rewind(fp);
  if (fread(svals, 2, 2, fp) != 2)
    return IIERR_IO_ERROR;

  info->swapBytes = 0;
  if (svals[0] != 18739 || svals[1] != 20480) {
    mrc_swap_shorts((b3dInt16 *)svals, 2);
    if (svals[0] != 18739 || svals[1] != 20480)
      return IIERR_NOT_FORMAT;
    info->swapBytes = 1;
  }

  if (fseek(fp, 16, SEEK_SET))
    return IIERR_IO_ERROR;
  if (fread(svals, 2, 4, fp) != 4)
    return IIERR_IO_ERROR;
  if (info->swapBytes)
    mrc_swap_shorts((b3dInt16 *)svals, 4);
  if (svals[0])
    return IIERR_NO_SUPPORT;
  switch (svals[1]) {
  case 2:
    info->type = RAW_MODE_BYTE;
    break;
  case 3:
    info->type = RAW_MODE_SHORT;
    break;
  case 15:
    info->type = RAW_MODE_USHORT;
    break;
  case 5:
    info->type = RAW_MODE_FLOAT;
    break;
  default:
    return IIERR_NO_SUPPORT;
      break;
  }

  /* Dimension must be 2 or 3 */
  if (svals[3] / 2 != 1)
    return IIERR_NO_SUPPORT;
  if (fseek(fp, 24, SEEK_SET))
    return IIERR_IO_ERROR;

  if (fread(ivals, 4, 1, fp) != 1)
    return IIERR_IO_ERROR;
  if (info->swapBytes)
    mrc_swap_longs(ivals, 1);
  info->headerSize = ivals[0];
  if (fseek(fp, 64, SEEK_SET))
    return IIERR_IO_ERROR;
  if (fread(ivals, 4, svals[3] * 4, fp) != svals[3] * 4)
    return IIERR_IO_ERROR;
  if (info->swapBytes)
    mrc_swap_longs(ivals, 12);
  if (svals[3] == 3) {
    info->nx = ivals[10];
    info->ny = ivals[6];
    info->nz = ivals[2];
  } else {
    info->nx = ivals[6];
    info->ny = ivals[2];
    info->nz = 1;
  }
  
  /* Set these to signal that the range is unknown */
  info->amin = 0.;
  info->amax = 0.;
  return 0;  
}

/*
 * Check for the pif format
 */
static int checkPif(FILE *fp, char *filename, RawImageInfo *info)
{
  b3dInt32 ivals[12];
  b3dByte cvals[6];

  /* is it a pif file (only reading bsoft pif files) */
  if (fseek(fp, 32, SEEK_SET) != 0)
  	return IIERR_IO_ERROR;
  
  if (fread(cvals, 1, 5, fp) != 5)
    return IIERR_IO_ERROR;
  
  /* recognize file type  */
  cvals[5] = 0;
  if (strcmp(cvals,"Bsoft") != 0)
  	return IIERR_NOT_FORMAT;
    
  info->headerSize = 1024;
  info->sectionSkip = 512;
  
  /* set swapBytes */
  if (fseek(fp, 28, SEEK_SET) != 0)
  	return IIERR_IO_ERROR;
  	
  if (fread(ivals, 4, 1, fp) != 1)
    return IIERR_IO_ERROR;
    
  info->swapBytes = 0;
  
#ifdef B3D_LITTLE_ENDIAN
  if (ivals[0] != 0)
    info->swapBytes = 1;
#else
  if (ivals[0] == 0)
    info->swapBytes = 1;
#endif
  
  if (fseek(fp, 24, SEEK_SET) != 0)
    return IIERR_IO_ERROR;
    
  if (fread(ivals, 4, 1, fp) != 1)
    return IIERR_IO_ERROR;
    
  if (info->swapBytes)
    mrc_swap_longs(ivals, 1);
   
  /* set nz from numimages because nz is 1 */
  info->nz = ivals[0];
    
  if (fseek(fp, 64, SEEK_SET) != 0)
    return IIERR_IO_ERROR;
    
  if (fread(ivals, 4, 5, fp) != 5)
    return IIERR_IO_ERROR;
 
  if (info->swapBytes)
    mrc_swap_longs(ivals, 5);
  
  /* If there are images of different sizes and there is more then one image, 
     fail. */
  if (ivals[0] < 1 && info->nz > 1)
    return IIERR_NOT_FORMAT;

  info->nx = ivals[1];
  info->ny = ivals[2];

  switch (ivals[4]) {
  case 0:
  case 6:
    info->type = RAW_MODE_BYTE;
    break;
  case 1:
  case 7:
  case 20:
  case 88:
    info->type = RAW_MODE_SHORT;
    break;
  case 9:
    info->type = RAW_MODE_FLOAT;
    break;
  default:
    return IIERR_NO_SUPPORT;
      break;
  }
  
  /* assume dimensions are 2 or 3 */
  
  /* Set these to signal that the range is unknown */
  info->amin = 0.;
  info->amax = 0.;
  return 0;  
}

/* DocumentObjectList so far seen to end before 208 */
#define DOC_CHECK_BUF 832
/*
 * Check for the DigitalMicrograph format
 */
static int checkDM3(FILE *fp, char *filename, RawImageInfo *info)
{
  unsigned char bvals[DOC_CHECK_BUF];
  int err, dmtype, dmf, i;
  unsigned char bsave;
  char *testString = "DocumentObjectList";
  int testLen = strlen(testString);

  /* Check for 3 or 4 in the fourth byte */
  rewind(fp);
  if (fread(bvals, 1, 4, fp) != 4)
    return IIERR_IO_ERROR;
  if (bvals[3] != 3 && bvals[3] != 4)
    return IIERR_NOT_FORMAT;
  dmf = (int)bvals[3];
  if (fread(bvals, 1, DOC_CHECK_BUF, fp) != DOC_CHECK_BUF) 
    return IIERR_IO_ERROR;

  /* Look for the test string */
  err = 1;
  for (i = 0; i < DOC_CHECK_BUF - testLen - 4; i++) {
    if (bvals[i] == testString[0]) {
      bsave = bvals[i + testLen];
      bvals[i + testLen] = 0x00;
      if (!strcmp((char *)(&bvals[i]), testString)) {
        err = 0;
        break;
      }
      bvals[i + testLen] = bsave;
    }
  }
  if (err)
    return IIERR_NOT_FORMAT;

  if ((err = analyzeDM3(fp, filename, dmf, info, &dmtype)))
    return err;

  switch (dmtype) {
  case 9:
    info->type = RAW_MODE_SBYTE;
    break;
  case 6:
    info->type = RAW_MODE_BYTE;
    break;
  case 1:
    info->type = RAW_MODE_SHORT;
    break;
  case 10:
    info->type = RAW_MODE_USHORT;
    break;
  case 2:
    info->type = RAW_MODE_FLOAT;
    break;
  default:
    return IIERR_NO_SUPPORT;
    break;
  }
  
  info->amin = 0.;
  info->amax = 0.;
  return 0;
}


/* 8/3/09: This was 160000, but a file with Data%%%% at 595098 turned up */
#define BUFSIZE 1000000
#define MAX_TYPES 12
/*!
 * Analyzes a file known to be a DigitalMicrograph version 3 or 4, as indicated in
 * [dmformat]; the file pointer is in [fp] and the filename in [filename].  Returns size,
 * type, and other information in [info]; specifically the {nx}, {ny}, {nz}, {swapBytes},
 * {headerSize}, and {type} members.  Returns the DM data type number in [dmtype].
 * Returns IOERR_IO_ERROR for errors reading the file or IOERR_NO_SUPPORT for other 
 * errors in analyzing the file.
 */
int analyzeDM3(FILE *fp, char *filename, int dmformat, RawImageInfo *info, int *dmtype)
{
  int c, toffset, typeIndex, dmind;
  char buf[BUFSIZE];
  char *found;
  int lowbyte, hibyte;
  int offset, type, xsize, ysize, zsize, gotCal, gotDim, gotScale, gotMeta, gotDimInfo;
  float scale, tmpPixel, pixel = 0., zPixel = 0.;
  off_t typeOffset, maxread, plausibleOff;
  struct stat statbuf;

  /* The type-dependent values that were found after 
     D a t a % % % % 0 0 0 3 0 0 0 24 0 0 0 */
  /*int datacode[MAX_TYPES] = {0, 2, 6, 0, 0, 0, 10, 3, 0, 9, 4, 5}; */
     
  int dataSize[MAX_TYPES] = {1, 2, 4, 1, 1, 1, 1, 4, 1, 1, 2, 4};
  int dimensOff[2] = {15, 27};
  int xsizeOff[2] = {31, 59};
  int ysizeOff[2] = {50, 94};
  int zsizeOff[2] = {69, 129};
  int dtypeOff[2] = {20, 36};
  int dataOff[2] = {24, 48};
  int scaleOff[2] = {17, 33};
  int unitsOff[2] = {25, 49};

  offset = 0;
  xsize = 0;
  ysize = 0;
  type = -1;
  typeOffset = 0;
  dmind = dmformat - 3;
  if (dmind < 0 || dmind > 1) {
    b3dError(stderr, "ERROR: analyzeDM3 - DM format %d not supported\n", dmformat);
    return IIERR_NO_SUPPORT;
  }

  if (stat(filename, &statbuf)) {
      b3dError(stderr, "ERROR: analyzeDM3 - Doing stat of %s\n", filename);
      return IIERR_IO_ERROR;
  }
  maxread = statbuf.st_size - 2;
  if (maxread >= BUFSIZE)
    maxread = BUFSIZE - 1;

  /* Read the end of the file first because we need the data type
     before we can be sure we have the right Data%%%% entry */

  if (fseek(fp, -(maxread+1), SEEK_END)) {
    b3dError(stderr, "ERROR: analyzeDM3 - Seeking to end of %s\n", filename);
    return IIERR_IO_ERROR;
  }
      
  if (!fread(buf, 1, maxread, fp)) {
    b3dError(stderr, "ERROR: analyzeDM3 - Error Reading tail end of %s\n",
             filename);
    return IIERR_IO_ERROR;
  }

  buf[maxread - 1] = 0x00;
  typeIndex = -1;

  /* Look past a DataType enough to see another Dimensions - it is supposed
     to be after it */
  for (c = 0; c < maxread && (xsize == 0 || type < 0 || 
                              c < typeIndex + 64 + (dmind ? 20 : 0)); c++) {

    /* Look for D, then check if it is Dimensions or DataType */
    if (buf[c] == 68) {
      found = strstr(&buf[c], "Dimensions");
      if (found) {
        lowbyte = (unsigned char)buf[c + dimensOff[dmind]];
        if (lowbyte == 3) {
          lowbyte =  (unsigned char)buf[c + zsizeOff[dmind]];
          hibyte =  (unsigned char)buf[c + zsizeOff[dmind] + 1];
          zsize = lowbyte + 256 * hibyte;
        } else if (lowbyte == 2) {
          zsize = 1;
        } else {
          b3dError(stderr, "ERROR: analyzeDM3 - The number of dimensions seems"
                   "to be %d, not 2 or 3, in %s\n", lowbyte, filename);
          return IIERR_NO_SUPPORT;
        }
        lowbyte =  (unsigned char)buf[c + xsizeOff[dmind]];
        hibyte =  (unsigned char)buf[c + xsizeOff[dmind] + 1];
        xsize = lowbyte + 256 * hibyte;
        lowbyte =  (unsigned char)buf[c + ysizeOff[dmind]];
        hibyte =  (unsigned char)buf[c + ysizeOff[dmind] + 1];
        ysize = lowbyte + 256 * hibyte;
      } else {
        found = strstr(&buf[c], "DataType");
        if (found && buf[c + dtypeOff[dmind]] < MAX_TYPES) {
          type = buf[c + dtypeOff[dmind]];
          if (!typeOffset)
            typeOffset = c + statbuf.st_size - (maxread + 1);
          typeIndex = c;
        } 
      }
    }
  }
  if (!xsize || !ysize || type < 0) {
    b3dError(stderr, "ERROR: analyzeDM3 - Dimensions or type not found in "
             "%s\n", filename);
    return IIERR_NO_SUPPORT;
  }
      
  /* Now look for the Data string in the front of the file and pixel size */
  rewind(fp);
  if (!fread(buf, 1, maxread, fp)) {
    b3dError(stderr, "ERROR: analyzeDM3 - Reading beginning of %s\n",
             filename);
    return IIERR_IO_ERROR;
  }

  plausibleOff = typeOffset - (off_t)(xsize * ysize) * zsize * dataSize[type];
  buf[maxread - 1] = 0x00;
  gotCal = gotDim = gotScale = gotMeta = gotDimInfo = 0;
  for (c = 0; c < maxread; c++) {
    if (buf[c] == 68) {
      found = foundDMtag(&buf[c], "Data", "Data%%%%", dmind, 12);
      if (found) {
        toffset = found + dataOff[dmind] - buf;

        /* If this is the first data string, or any data
           string that could still be far enough in front of the datatype
           string, save the offset */
        if (!offset || toffset <= plausibleOff)
          offset = toffset;
             
        /* It used to be done with code types but that turned out to be
           unreliable */
        /* And if the code type is appropriate, save the 
           offset and break out */
        /*if (type <= 11 && buf[c + 19] == datacode[type]) {
          offset = toffset;
          break; 
          } */        
      } else if (gotMeta && strstr(&buf[c], "Dimension info")) {
        gotDimInfo = 1;
      }

      /* Always look for start of the Calibrations sequence */
    } else if (buf[c] == 67) {
      if (strstr(&buf[c], "Calibrations")) {
        gotCal = 1;
        gotDim = gotScale = gotMeta = gotDimInfo = 0;
      }
      
      /* Look for \tDimension if have Calibrations, or always look for Meta Data */
    } else if (buf[c] == '\t') {
      
      if (gotCal && !gotDim && strstr(&buf[c], "\tDimension"))
        gotDim = 1;
      if (strstr(&buf[c], "\tMeta Data")) {
        gotMeta = 1;
        gotCal = gotDim = gotScale = gotDimInfo = 0;
      }

      /* Look for Scale if have Dimension or Dimension info */
    } else if ((gotDim || gotDimInfo) && !gotScale && buf[c] == 'S') {
      found = foundDMtag(&buf[c], "Scale", "Scale%%%%", dmind, 13);
      if (found) {

        /* Always copy a scale over */
        gotScale = 1;
        memcpy(&scale, &buf[c + scaleOff[dmind]], 4);
#ifndef B3D_LITTLE_ENDIAN
        mrc_swap_longs((b3dInt32 *)(&scale), 1);
#endif
      }

      /* If we have a scale, make sure it is valid and at a plausible location and
         if so set the pixel size, overriding an earlier one */
    } else if (gotScale &&  buf[c] == 'U') {
      found = foundDMtag(&buf[c], "Units", "Units%%%%", dmind, 13);
      toffset = found - buf;
      if ((gotDim && !pixel) || (gotDimInfo && !zPixel) || toffset <= plausibleOff) {
        if (buf[c + unitsOff[dmind] + 2] == 'm') {
          if (buf[c + unitsOff[dmind]] == 'n')
            tmpPixel = scale * 10.;
          else if ((unsigned char)buf[c + unitsOff[dmind]] == 181)
            tmpPixel = scale * 10000.;
          if (gotDimInfo)
            zPixel = tmpPixel;
          else
            pixel = tmpPixel;
        }
      }
      gotCal = gotDim = gotScale = gotMeta = gotDimInfo = 0;
    }
  }
  if (!offset) {
    b3dError(stderr, "ERROR: analyzeDM3 - Data string not found in %s\n",
             filename);
    return IIERR_NO_SUPPORT;
  }

  info->nx = xsize;
  info->ny = ysize;
  info->nz = zsize;
  info->headerSize = offset;
  info->yInverted = 1;
  info->pixel = pixel;
  info->zPixel = zPixel;
  *dmtype = type;
#ifdef B3D_LITTLE_ENDIAN
  info->swapBytes = 0;
#else
  info->swapBytes = 1;
#endif
  return 0;
}

static char *foundDMtag(char *buf, const char *tag, const char *fullTag, int dmind,
                        int dm4Offset)
{
  char *found;
  if (dmind)  {
    found = strstr(buf, tag);
    if (found && !strstr(buf + dm4Offset, "%%%%"))
      found = NULL;
  } else
    found = strstr(buf, fullTag);
  return found;
}

/*
 * Check for the EM format
 */
static int checkEM(FILE *fp, char *filename, RawImageInfo *info)
{
  unsigned char bvals[4];
  b3dInt32 ivals[12];

  rewind(fp);
  if (fread(bvals, 1, 4, fp) != 4)
    return IIERR_IO_ERROR;
  if (fread(ivals, 4, 3, fp) != 3)
    return IIERR_IO_ERROR;

  /*printf("bvals %d %d %d %d  ivals %d %d %d\n", bvals[0], bvals[1], bvals[2],
    bvals[3], ivals[0], ivals[1], ivals[2]);*/

  /* Not much magic here, put limits on type values and machine numbers and
     product of putative sizes */
  if (ivals[0] <= 0 || ivals[1] <= 0 || ivals[2] <= 0 ||
      (ivals[0] > 65536 && ivals[1] > 65536 && ivals[2] > 65536) ||
      bvals[0] > MAX_EM_MACHINES || bvals[2] == 1 || 
      bvals[3] > MAX_EM_TYPES || 
      ((float)ivals[0] * ivals[1]) * ivals[2] > MAX_EM_SIZE) {
    mrc_swap_longs(ivals, 3);

    if (ivals[0] <= 0 || ivals[1] <= 0 || ivals[2] <= 0 ||
        (ivals[0] > 65536 && ivals[1] > 65536 && ivals[2] > 65536) ||
        bvals[0] > MAX_EM_MACHINES || bvals[2] == 1 || 
        bvals[3] > MAX_EM_TYPES ||
        ((float)ivals[0] * ivals[1]) * ivals[2] > MAX_EM_SIZE) 
      return IIERR_NOT_FORMAT;
    info->swapBytes = 1;
  }        

  switch (bvals[3]) {
  case 1:
    info->type = RAW_MODE_BYTE;
    break;
  case 2:
    info->type = RAW_MODE_SHORT;
    break;
  case 5:
    info->type = RAW_MODE_FLOAT;
    break;
  default:
    return IIERR_NO_SUPPORT;
    break;
  }

  info->nx = ivals[0];
  info->ny = ivals[1];
  info->nz = ivals[2];
  info->headerSize = 512;
  info->amin = 0.;
  info->amax = 0.;
  return 0;
}
