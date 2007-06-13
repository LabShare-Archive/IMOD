/*
 *    iilikemrc.c - Check for recognizable formats that can be read like MRC
 *
 *    Author:  David Mastronarde
 *
 *   Copyright (C) 2006 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 */
/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.4  2006/09/21 22:25:05  mast
Needed to set the mode in the iifile

Revision 3.3  2006/09/12 19:54:56  mast
Add proper returns to check functions

Revision 3.2  2006/09/03 22:20:14  mast
Reorganized, provided generic check function list and added DM3 support

Revision 3.1  2006/09/03 00:00:56  mast
Initial creation

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
  CheckEntry item;
  if (checkList)
    return 0;
  checkList = ilistNew(sizeof(CheckEntry), 6);
  if (!checkList)
    return 1;
  iiAddRawCheckFunction(checkEM, "EM");
  iiAddRawCheckFunction(checkDM3, "DM3");
  iiAddRawCheckFunction(checkWinkler, "Winkler");
  return 0;
}

/*!
 * Add the given raw-type checking function [func] to the front of the checking
 * list; [name] is a name for the format.  The definition of such a function is
 * ^  int checkMyFormat(FILE *fp, char *filename, RawImageInfo *info)
 */
void iiAddRawCheckFunction(IIRawCheckFunction func, char *name)
{
  CheckEntry item;
  item.func = func;
  item.name = strdup(name);
  if (initCheckList() || !item.name)
    return;
  ilistInsert(checkList, &item, 0);
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
 * {headerSize}, {sectionSkip}, and {type} members.  Returns IOERR_MEMORY_ERR
 * for error allocating header.
 */
int iiSetupRawHeaders(ImodImageFile *inFile, RawImageInfo *info)
{
  MrcHeader *hdr;

  /* Get an MRC header; set sizes into that header and the iifile header */
  hdr = (MrcHeader *)malloc(sizeof(MrcHeader));
  if (!hdr) {
    b3dError(stderr, "ERROR: iiSetupRawHeaders - Getting memory for header");
    return IIERR_MEMORY_ERR;
  }
  inFile->nx   = hdr->nx = info->nx;
  inFile->ny   = hdr->ny = info->ny;
  inFile->nz   = hdr->nz = info->nz;
  inFile->file = IIFILE_RAW;
  hdr->swapped = info->swapBytes;
  hdr->headerSize = info->headerSize;
  hdr->sectionSkip = info->sectionSkip;
  hdr->fp = inFile->fp;

  /* Set flags for type of data */
  switch(info->type) {
  case RAW_MODE_BYTE:
    hdr->mode = MRC_MODE_BYTE;
    inFile->format = IIFORMAT_LUMINANCE;
    inFile->type   = IITYPE_UBYTE;
    break;
  case RAW_MODE_SHORT:
    hdr->mode = MRC_MODE_SHORT;
    inFile->format = IIFORMAT_LUMINANCE;
    inFile->type   = IITYPE_SHORT;
    break;
  case RAW_MODE_USHORT:
    hdr->mode = MRC_MODE_USHORT;
    inFile->format = IIFORMAT_LUMINANCE;
    inFile->type   = IITYPE_USHORT;
    break;
  case RAW_MODE_FLOAT:
    hdr->mode = MRC_MODE_FLOAT;
    inFile->format = IIFORMAT_LUMINANCE;
    inFile->type   = IITYPE_FLOAT;
    break;
  case RAW_MODE_COMPLEX_FLOAT:
    hdr->mode = MRC_MODE_COMPLEX_FLOAT;
    inFile->format = IIFORMAT_COMPLEX;
    inFile->type   = IITYPE_FLOAT;
    break;
  case RAW_MODE_RGB:
    hdr->mode = MRC_MODE_RGB;
    inFile->format = IIFORMAT_RGB;
    inFile->type   = IITYPE_UBYTE;
    break;
  }

  // Set the header and the access routines; just use the MRC routines
  inFile->header = (char *)hdr;
  inFile->readSection = iiMRCreadSection;
  inFile->readSectionByte = iiMRCreadSectionByte;
  inFile->cleanUp = iiLikeMRCDelete;
  inFile->mode = hdr->mode;
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
    mrc_swap_shorts(svals, 2);
    if (svals[0] != 18739 || svals[1] != 20480)
      return IIERR_NOT_FORMAT;
    info->swapBytes = 1;
  }

  if (fseek(fp, 16, SEEK_SET))
    return IIERR_IO_ERROR;
  if (fread(svals, 2, 4, fp) != 4)
    return IIERR_IO_ERROR;
  if (info->swapBytes)
    mrc_swap_shorts(svals, 4);
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
  b3dUInt16 svals[4];
  b3dInt32 ivals[12];
  b3dByte cvals[5];

  //is it a pif file (only reading bsoft pif files)
  if (fseek(fp, 32, SEEK_SET) != 0)
  	return IIERR_IO_ERROR;
  
  if (fread(svals, 1, 5, fp) != 5)
    return IIERR_IO_ERROR;
    
  if (strcmp((char*) cvals,"Bsoft") != 0)
  	return IIERR_NOT_FORMAT;
    
  info->headerSize = 1024;
  info->sectionSkip = 512;
  
  //set swapBytes
  if (fseek(fp, 28, SEEK_SET) != 0)
  	return IIERR_IO_ERROR;
  	
  if (fread(ivals, 1, 1, fp) != 1)
    return IIERR_IO_ERROR;
    
  info->swapBytes = 0;
  
#ifdef B3D_LITTLE_ENDIAN
  if (ivals[0] != 0)
    info->swapBytes = 1;
#else
  if (ivals[0] == 0)
    info->swapBytes = 1;
#endif

  //Bsoft doesn't allow .pif files with different size images (htype < 1),
  //unless there is only 1 image.  Not checking htype.
  
  //set size and mode
  if (fseek(fp, 68, SEEK_SET) != 0)
    return IIERR_IO_ERROR;
    
  if (fread(ivals, 1, 4, fp) != 4)
    return IIERR_IO_ERROR;
  
  if (info->swapBytes)
    mrc_swap_longs(ivals, 4);
      
  info->nx = ivals[0];
  info->ny = ivals[1];
  info->nz = ivals[2];

  switch (ivals[3]) {
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
  
  //assume dimensions are 2 or 3
  
  /* Set these to signal that the range is unknown */
  info->amin = 0.;
  info->amax = 0.;
  return 0;  
}

/*
 * Check for the DigitalMicrograph format
 */
static int checkDM3(FILE *fp, char *filename, RawImageInfo *info)
{
  unsigned char bvals[12];
  int err, dmtype;

  rewind(fp);
  if (fread(bvals, 1, 4, fp) != 4)
    return IIERR_IO_ERROR;
  if (bvals[3] != 3)
    return IIERR_NOT_FORMAT;
  if (fseek(fp, 21, SEEK_SET))
    return IIERR_IO_ERROR;
  if (fread(bvals, 1, 11, fp) != 11) 
    return IIERR_IO_ERROR;

  bvals[11] = 0x00;
  if (strcmp(bvals, "Application"))
    return IIERR_NOT_FORMAT;

  if ((err = analyzeDM3(fp, filename, info, &dmtype)))
    return err;

  switch (dmtype) {
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


#define BUFSIZE 160000
/*!
 * Analyzes a file known to be a DigitalMicrograph version 3; the file pointer
 * is in [fp] and the filename in [filename].  Returns size, type, and other
 * information in [info]; specifically the {nx}, {ny}, {nz}, {swapBytes},
 * {headerSize}, and {type} members.  Returns IOERR_IO_ERROR for errors reading
 * the file or IOERR_NO_SUPPORT for other errors in analyzing the file.
 */
int analyzeDM3(FILE *fp, char *filename, RawImageInfo *info, int *dmtype)
{
  int i,c, toffset, typeOffset, maxread;
  char buf[BUFSIZE];
  char *found;
  int lowbyte, hibyte;
  int offset, type, xsize, ysize;
  struct stat statbuf;

  /* The type-dependent values that were found after 
     D a t a % % % % 0 0 0 3 0 0 0 24 0 0 0 */
  int datacode[12] = {0, 2, 6, 0, 0, 0, 10, 3, 0, 9, 4, 5};
     
  int dataSize[12] = {1, 2, 4, 1, 1, 1, 1, 4, 1, 1, 2, 4};

  offset = 0;
  xsize = 0;
  ysize = 0;
  type = -1;
  typeOffset = 0;

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
  for (c = 0; c < maxread && (xsize == 0 || type < 0); c++) {

    /* Look for D, then check if it is Dimensions or DataType */
    if (buf[c] == 68) {
      found = strstr(&buf[c], "Dimensions");
      if (found) {
        lowbyte =  (unsigned char)buf[c + 31];
        hibyte =  (unsigned char)buf[c + 32];
        xsize = lowbyte + 256 * hibyte;
        lowbyte =  (unsigned char)buf[c + 50];
        hibyte =  (unsigned char)buf[c + 51];
        ysize = lowbyte + 256 * hibyte;
      } else {
        found = strstr(&buf[c], "DataType");
        if (found) {
          type = buf[c + 20];
          if (!typeOffset)
            typeOffset = c + statbuf.st_size - (maxread + 1);
        } 
      }
    }
  }
  if (!xsize || !ysize || type < 0) {
    b3dError(stderr, "ERROR: analyzeDM3 - Dimensions or type not found in "
             "%s\n", filename);
    return IIERR_NO_SUPPORT;
  }
      
  /* Now look for the Data string in the front of the file */
  rewind(fp);
  if (!fread(buf, 1, maxread, fp)) {
    b3dError(stderr, "ERROR: analyzeDM3 - Reading beginning of %s\n",
             filename);
    return IIERR_IO_ERROR;
  }

  buf[maxread - 1] = 0x00;
  for (c = 0; c < maxread; c++) {
    if (buf[c] == 68) {
      found = strstr(&buf[c], "Data%%%%");
      if (found) {
        toffset = found + 24 - buf;

        /* If this is the first data string, or any data
           string that could still be far enough in front of the datatype
           string, save the offset */
        if (!offset || 
            toffset <= typeOffset - xsize * ysize * dataSize[type])
          offset = toffset;
             
        /* It used to be done with code types but that turned out to be
           unreliable */
        /* And if the code type is appropriate, save the 
           offset and break out */
        /*if (type <= 11 && buf[c + 19] == datacode[type]) {
          offset = toffset;
          break; 
          } */        
      }
    }
  }
  if (!offset) {
    b3dError(stderr, "ERROR: analyzeDM3 - Data string not found in %s\n",
             filename);
    return IIERR_NO_SUPPORT;
  }

  info->nx = xsize;
  info->ny = ysize;
  info->nz = 1;
  info->headerSize = offset;
  *dmtype = type;
#ifdef B3D_LITTLE_ENDIAN
  info->swapBytes = 0;
#else
  info->swapBytes = 1;
#endif
  return 0;
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
