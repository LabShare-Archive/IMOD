/*
 *    iimrc.c    - specific routines for mrc-type ImodImageFile's
 *
 *    Authors:  James Kremer and David Mastronarde
 *
 *   Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *
 *  $Id$
 */

#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "iimage.h"
#include "b3dutil.h"

static void iiMRCdelete(ImodImageFile *inFile);
static int readSectionScaled(ImodImageFile *inFile, char *buf, int inSection, int outmax);
static int writeSection(ImodImageFile *inFile, char *buf, int inSection, int asFloat);
static int readSectionUnscaled(ImodImageFile *inFile, char *buf, int inSection, 
                               int asFloat);
static int iiMRCreadSection(ImodImageFile *inFile, char *buf, int inSection);
static int iiMRCreadSectionByte(ImodImageFile *inFile, char *buf, int inSection);
static int iiMRCreadSectionUShort(ImodImageFile *inFile, char *buf, int inSection);
static int iiMRCreadSectionFloat(ImodImageFile *inFile, char *buf, int inSection);
static int iiMRCwriteSection(ImodImageFile *inFile, char *buf, int inSection);
static int iiMRCwriteSectionFloat(ImodImageFile *inFile, char *buf, int inSection);

int iiMRCCheck(ImodImageFile *iif)
{
  FILE *fp;
  MrcHeader *hdr;
  int err;

  if (!iif) 
    return IIERR_BAD_CALL;
  fp = iif->fp;
  if (!fp) 
    return IIERR_BAD_CALL;

  hdr = (MrcHeader *)malloc(sizeof(MrcHeader));
  if (!hdr) {
    b3dError(stderr, "ERROR: iiMRCCheck - getting memory for header\n");
    return IIERR_MEMORY_ERR;
  }

  if ((err = mrc_head_read(fp, hdr))){
    free(hdr);
    if (err < 0)
      return IIERR_IO_ERROR;
    return IIERR_NOT_FORMAT;
  }

  iif->header = (char *)hdr;
  iif->file = IIFILE_MRC;
  iiMRCmodeToFormatType(iif, hdr->mode, hdr->bytesSigned);
  iiSyncFromMrcHeader(iif, hdr);
  iif->smin  = iif->amin;
  iif->smax  = iif->amax;

  iif->hasPieceCoords = iiMRCcheckPCoord(hdr);

  iiMRCsetIOFuncs(iif, 0);
  return(0);
}

void iiMRCmodeToFormatType(ImodImageFile *iif, int mode, int bytesSigned)
{
  switch (mode) {
  case MRC_MODE_BYTE:
    iif->format = IIFORMAT_LUMINANCE;
    iif->type   = bytesSigned ? IITYPE_BYTE : IITYPE_UBYTE;
    break;
  case MRC_MODE_SHORT:
    iif->format = IIFORMAT_LUMINANCE;
    iif->type   = IITYPE_SHORT;
    break;
  case MRC_MODE_USHORT:
    iif->format = IIFORMAT_LUMINANCE;
    iif->type   = IITYPE_USHORT;
    break;
  case MRC_MODE_FLOAT:
    iif->format = IIFORMAT_LUMINANCE;
    iif->type   = IITYPE_FLOAT;
    break;
  case MRC_MODE_COMPLEX_SHORT:
    iif->format = IIFORMAT_COMPLEX;
    iif->type   = IITYPE_SHORT;
    break;
  case MRC_MODE_COMPLEX_FLOAT:
    iif->format = IIFORMAT_COMPLEX;
    iif->type   = IITYPE_FLOAT;
    break;
  case MRC_MODE_RGB:
    iif->format = IIFORMAT_RGB;
    iif->type   = IITYPE_UBYTE;
    break;
  }
  iif->mode  = mode;
}

void iiMRCsetIOFuncs(ImodImageFile *inFile, int rawFile)
{
  inFile->readSection = iiMRCreadSection;
  inFile->readSectionByte = iiMRCreadSectionByte;
  inFile->readSectionUShort = iiMRCreadSectionUShort;
  inFile->readSectionFloat = iiMRCreadSectionFloat;
  inFile->fillMrcHeader = iiMRCfillHeader;
  if (rawFile)
    return;
  inFile->cleanUp = iiMRCdelete;
  inFile->writeSection = iiMRCwriteSection;
  inFile->writeSectionFloat = iiMRCwriteSectionFloat;
}

void iiMRCdelete(ImodImageFile *inFile)
{
  if (inFile->header)
    free(inFile->header);
} 

int iiMRCopenNew(ImodImageFile *inFile, const char *mode) 
{
  errno = 0;
  inFile->fp = fopen(inFile->filename, mode);
  if (!inFile->fp) {
    b3dError(stderr, "ERROR: iiMRCopenNew - Could not open %s%s%s\n" , inFile->filename,
             errno ? " - system message: " : "", errno ? strerror(errno) : "");
    return 1;
  }
  inFile->header = (char *)malloc(sizeof(MrcHeader));
  if (!inFile->header) {
    b3dError(stderr, "ERROR: iiMRCopenNew - Allocating MRC header\n");
    return 1;
  }
  mrc_head_new((MrcHeader *)inFile->header, 1, 1, 1, 0);
  iiMRCsetIOFuncs(inFile, 0);
  ((MrcHeader *)(inFile->header))->fp = inFile->fp;
  inFile->file = IIFILE_MRC;
  return 0;
}

int iiMRCfillHeader(ImodImageFile *inFile, MrcHeader *hdata)
{
  if (!inFile || !inFile->header)
    return 1;
  if (hdata != (MrcHeader *)(inFile->header))
    *hdata = *((MrcHeader *)(inFile->header));
  return 0;
}

/*
 * Transfer the subarea loading information from the image file to the loadInfo
 */
void iiMRCsetLoadInfo(ImodImageFile *inFile, IloadInfo *li)
{
  mrc_init_li(li, NULL);
  li->xmin = inFile->llx;
  li->ymin = inFile->lly;
  li->zmin = inFile->llz;

  /* DNM 2/26/03: replace upper right only if negative */
  if (inFile->urx < 0)
    li->xmax = inFile->nx-1;
  else
    li->xmax = inFile->urx;
  if (inFile->ury < 0)
    li->ymax = inFile->ny-1;
  else
    li->ymax = inFile->ury;
  if (inFile->urz < 0)
    li->zmax = inFile->nz-1;
  else
    li->zmax = inFile->urz;

  li->slope = inFile->slope;
  li->offset = inFile->offset;
  li->axis = inFile->axis;
  li->padLeft = inFile->padLeft;
  li->padRight = inFile->padRight;
}

static int iiMRCreadSection(ImodImageFile *inFile, char *buf, int inSection)
{
  return readSectionUnscaled(inFile, buf, inSection, 0);
}

static int iiMRCreadSectionFloat(ImodImageFile *inFile, char *buf, int inSection)
{
  return readSectionUnscaled(inFile, buf, inSection, 1);
}

static int readSectionUnscaled(ImodImageFile *inFile, char *buf, int inSection, 
                               int asFloat)
{
  int err;
  IloadInfo li;
  MrcHeader *h = (MrcHeader *)inFile->header;

  iiMRCsetLoadInfo(inFile, &li);
  li.outmin = inFile->smin;
  li.outmax = inFile->smax;
  li.black = 0;
  li.white = 255;
  li.mirrorFFT = 0;
  h->fp = inFile->fp;
  iiChangeCallCount(1);
  if (asFloat)
    err = mrcReadSectionFloat(h, &li, (b3dFloat *)buf, inSection);
  else
    err = mrcReadSection(h, &li, (unsigned char *)buf, inSection);
  iiChangeCallCount(-1);
  return err;
}

static int iiMRCreadSectionByte(ImodImageFile *inFile, char *buf, int inSection)
{
  return readSectionScaled(inFile, buf, inSection, 255);
}
static int iiMRCreadSectionUShort(ImodImageFile *inFile, char *buf, int inSection)
{
  return readSectionScaled(inFile, buf, inSection, 65535);
}

static int readSectionScaled(ImodImageFile *inFile, char *buf, int inSection, int outmax)
{
  int err;
  IloadInfo li;
  MrcHeader *h = (MrcHeader *)inFile->header;

  iiMRCsetLoadInfo(inFile, &li);
  li.outmin   = 0;
  li.outmax   = outmax;
  li.mirrorFFT = inFile->mirrorFFT;
  h->fp = inFile->fp; 
  iiChangeCallCount(1);
  if (outmax > 255) 
    err = mrcReadSectionUShort(h, &li, (unsigned char *)buf, inSection);
  else
    err = mrcReadSectionByte(h, &li, (unsigned char *)buf, inSection);
  iiChangeCallCount(-1);
  return err;
}

static int iiMRCwriteSection(ImodImageFile *inFile, char *buf, int inSection)
{
  return writeSection(inFile, buf, inSection, 0);
}

static int iiMRCwriteSectionFloat(ImodImageFile *inFile, char *buf, int inSection)
{
  return writeSection(inFile, buf, inSection, 1);
}

static int writeSection(ImodImageFile *inFile, char *buf, int inSection, int asFloat) 
{
  int err;
  IloadInfo li;
  MrcHeader *h = (MrcHeader *)inFile->header;

  iiMRCsetLoadInfo(inFile, &li);
  h->fp = inFile->fp;
  if (inFile->axis != 3) {
    b3dError(stderr, "ERROR: iiMRCwriteSection - attempting to write Y slices\n");
    return 1;
  }
  iiChangeCallCount(1);
  if (asFloat)
    err = mrcWriteZFloat(h, &li, (b3dFloat *)buf, inSection);
  else
    err = mrcWriteZ(h, &li, (unsigned char *)buf, inSection);
  iiChangeCallCount(-1);
  return err;
}


#define TILT_FLAG    1
#define MONTAGE_FLAG 2

/* Return 1 if file has piece coordinates in header, 0 if not */
int iiMRCcheckPCoord(MrcHeader *hdr)
{
  /* 12/31/13: move flags test to function, but still test for Deltavision here */
  if (!hdr->next || !(hdr->nreal & MONTAGE_FLAG) || hdr->creatid == -16224)
    return 0;
  return extraIsNbytesAndFlags(hdr->nint, hdr->nreal);
}


int iiMRCLoadPCoord(ImodImageFile *inFile, IloadInfo *li, int nx, int ny,
                    int nz)
{
  int i;
  b3dUInt16 pcoordxy[2];
  b3dInt16 pcoordz;
  int offset=1024;
  int nread = nz;
  MrcHeader *hdr = (MrcHeader *)inFile->header;     
  int iflag = hdr->nreal;
  int nbytes = hdr->nint;
  int nextra = hdr->next;

  if (!iiMRCcheckPCoord(hdr))
    return 0;

  if (iflag & TILT_FLAG)
    offset += 2;

  if (nbytes * nz > nextra) {
    nread = nextra / nbytes;
    b3dError(stderr, "There are piece coordinates for only %d frames in"
            " the extra header\n", nread);
  }

  li->pcoords = (int *)malloc(sizeof(int) * 3 * nz);

  fseek(inFile->fp, offset, SEEK_SET);

  for (i = 0; i < nread; i++) {
    fread(pcoordxy, 2, 2, inFile->fp);
    fread(&pcoordz, 1, 2, inFile->fp);

    /* add swapping 10/2/00 */
    if (hdr->swapped) {
      mrc_swap_shorts((b3dInt16 *)pcoordxy, 2);
      mrc_swap_shorts(&pcoordz, 1);
    }
    if (ferror(inFile->fp)) {
      nread = i;
      b3dError(stderr, "Error reading piece coordinates from extra"
              " header after %d frames\n", i);
      break;
    }

    li->pcoords[(i*3)]   = pcoordxy[0];
    li->pcoords[(i*3)+1] = pcoordxy[1];
    li->pcoords[(i*3)+2] = pcoordz;

    offset = nbytes - 6;
    if (offset > 0)
      fseek(inFile->fp, offset, SEEK_CUR);
  }
  li->plist = nread;
  return(mrc_plist_proc(li, nx, ny, nz));
}
