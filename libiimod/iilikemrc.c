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
*/

#include <stdio.h>
#include "mrcc.h"
#include "iimage.h"
#include "b3dutil.h"

#define MAX_EM_MACHINES 20
#define MAX_EM_TYPES    20

/*!
 * Checks the image file in [inFile] for one known format after another 
 * Returns -1 for error reading file, 1 for file format not recognized,
 * 2 for unsupported data type of recognized format, or 3 for error
 * allocating header.
 */
int iiLikeMRCCheck(ImodImageFile *inFile)
{
  FILE *fp; 
  b3dUInt16 svals[4];
  unsigned char bvals[4];
  b3dInt32 ivals[12];
  int winkler = 0;
  int emtype = 0;
  RawImageInfo info;
  int err = 0;

  info.swapBytes = 0;

  if (!inFile) 
    return -1;
  fp = inFile->fp;
  if (!fp)
    return 1;

  /* First check Winkler format */
  rewind(fp);
  if (fread(svals, 2, 2, fp) != 2){
    b3dError(stderr, "ERROR: iiCheckLikeMRC - reading first four bytes.\n");
    return(-1);
  }

  if (svals[0] == 18739 && svals[1] == 20480)
    winkler = 1;
  else {
    mrc_swap_shorts(svals, 2);
    if (svals[0] == 18739 && svals[1] == 20480) {
      winkler = 1;
      info.swapBytes = 1;
    }
  }

  if (winkler) {
    fseek(fp, 16, SEEK_SET);
    if (fread(svals, 2, 4, fp) != 4)
      err = -1;
    if (!err) {
      if (info.swapBytes)
        mrc_swap_shorts(svals, 4);
      if (svals[0])
        err = 2;
      switch (svals[1]) {
      case 2:
        info.type = RAW_MODE_BYTE;
        break;
      case 3:
        info.type = RAW_MODE_SHORT;
        break;
      case 15:
        info.type = RAW_MODE_USHORT;
        break;
      case 5:
        info.type = RAW_MODE_FLOAT;
        break;
      default:
        err = 2;
        break;
      }

      /* Dimension must be 2 or 3 */
      if (svals[3] / 2 != 1)
        err = 2;
    }
    if (!err) {
      fseek(fp, 24, SEEK_SET);
      if (fread(ivals, 4, 1, fp) != 1)
        err = -1;
      if (info.swapBytes)
        mrc_swap_longs(ivals, 1);
      info.headerSize = ivals[0];
    }
    if (!err) {
      fseek(fp, 64, SEEK_SET);
      if (fread(ivals, 4, svals[3] * 4, fp) != svals[3] * 4)
        err = -1;
      if (info.swapBytes)
        mrc_swap_longs(ivals, 12);
      info.nx = ivals[10];
      info.ny = ivals[6];
      if (svals[3] == 3)
        info.nz = ivals[2];
      else
        info.nz = 1;
    }

    /* Set these to signal that the range is unknown */
    inFile->amin = 0.;
    inFile->amax = 0.;
    if (!err)
      return(iiSetupRawHeaders(inFile, info));

    if (err == -1)
      b3dError(stderr, "ERROR: iiCheckLikeMRC - reading from file.\n");
    else
      b3dError(stderr, "ERROR: iiCheckLikeMRC - unsupported data mode of "
               "Winkler-type file.\n");
    return err;
  }

  /* EM is pretty easy, do it next */

  rewind(fp);
  if (fread(bvals, 1, 4, fp) != 4)
    err = -1;
  if (fread(ivals, 4, 3, fp) != 3)
    err = -1;
  if (err) {
    b3dError(stderr, "ERROR: iiCheckLikeMRC - reading from file.\n");
    return -1;
  }

  info.swapBytes = 0;
  emtype = 1;
  if (ivals[0] <= 0 || ivals[1] <= 0 || ivals[2] <= 0 ||
      (ivals[0] > 65536 && ivals[1] > 65536 && ivals[2] > 65536)) {
    mrc_swap_longs(ivals, 3);
    info.swapBytes = 1;

    /* Not much magic here, put limits on type values and machine numbers */
    if (ivals[0] <= 0 || ivals[1] <= 0 || ivals[2] <= 0 ||
        (ivals[0] > 65536 && ivals[1] > 65536 && ivals[2] > 65536) ||
        bvals[0] > MAX_EM_MACHINES || bvals[2] == 1 || 
        bvals[3] > MAX_EM_TYPES) {
      emtype = 0;
    }
  }        

  if (emtype) {
    switch (bvals[3]) {
    case 1:
      info.type = RAW_MODE_BYTE;
      break;
    case 2:
      info.type = RAW_MODE_SHORT;
      break;
    case 5:
      info.type = RAW_MODE_FLOAT;
      break;
    default:
      err = 2;
      break;
    }

    info.nx = ivals[0];
    info.ny = ivals[1];
    info.nz = ivals[2];
    info.headerSize = 512;
    inFile->amin = 0.;
    inFile->amax = 0.;
    if (!err)
      return(iiSetupRawHeaders(inFile, info));
    b3dError(stderr, "ERROR: iiCheckLikeMRC - unsupported data mode of "
             "EM-type file.\n");
    return err;
  }

  return 1;
}

/*
 * Creates an MRC header and fills it and the items in [inFile] from the
 * information in [info]; specifically the {nx}, {ny}, {nz}, {swapBytes},
 * {headerSize}, and {type} members.  Returns 3 for error allocating header.
 */
int iiSetupRawHeaders(ImodImageFile *inFile, RawImageInfo info)
{
  MrcHeader *hdr;

  /* Get an MRC header; set sizes into that header and the iifile header */
  hdr = (MrcHeader *)malloc(sizeof(MrcHeader));
  if (!hdr)
    return 3;
  inFile->nx   = hdr->nx = info.nx;
  inFile->ny   = hdr->ny = info.ny;
  inFile->nz   = hdr->nz = info.nz;
  inFile->file = IIFILE_RAW;
  hdr->swapped = info.swapBytes;
  hdr->headerSize = info.headerSize;
  hdr->fp = inFile->fp;

  /* Set flags for type of data */
  switch(info.type) {
  case 0:
    hdr->mode = MRC_MODE_BYTE;
    inFile->format = IIFORMAT_LUMINANCE;
    inFile->type   = IITYPE_UBYTE;
    break;
  case 1:
    hdr->mode = MRC_MODE_SHORT;
    inFile->format = IIFORMAT_LUMINANCE;
    inFile->type   = IITYPE_SHORT;
    break;
  case 2:
    hdr->mode = MRC_MODE_USHORT;
    inFile->format = IIFORMAT_LUMINANCE;
    inFile->type   = IITYPE_USHORT;
    break;
  case 3:
    hdr->mode = MRC_MODE_FLOAT;
    inFile->format = IIFORMAT_LUMINANCE;
    inFile->type   = IITYPE_FLOAT;
    break;
  case 4:
    hdr->mode = MRC_MODE_COMPLEX_FLOAT;
    inFile->format = IIFORMAT_COMPLEX;
    inFile->type   = IITYPE_FLOAT;
    break;
  case 5:
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
  return 0;
}

void iiLikeMRCDelete(ImodImageFile *inFile)
{
  if (inFile->header)
    free(inFile->header);
}
