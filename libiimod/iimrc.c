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
 *  Log at end
 */

#include <stdlib.h>
#include "mrcc.h"
#include "iimage.h"
#include "b3dutil.h"

static void iiMRCdelete(ImodImageFile *inFile);

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

  iif->nx   = hdr->nx;
  iif->ny   = hdr->ny;
  iif->nz   = hdr->nz;
  iif->file = IIFILE_MRC;

  switch(hdr->mode){
  case MRC_MODE_BYTE:
    iif->format = IIFORMAT_LUMINANCE;
    iif->type   = IITYPE_UBYTE;
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
  iif->mode  = hdr->mode;
  iif->amin  = hdr->amin;
  iif->amax  = hdr->amax;
  iif->smin  = iif->amin;
  iif->smax  = iif->amax;
  iif->amean = hdr->amean;
  iif->xscale = iif->yscale = iif->zscale = 1.;

  /* DNM 11/5/98: inverted these expressions to give proper usage */
  /* DNM 9/13/02: needed to divide by mx, ny, nz, not nx, ny, nz */
  if (hdr->xlen && hdr->mx)
    iif->xscale = hdr->xlen/(float)hdr->mx;
  if (hdr->ylen && hdr->my)
    iif->yscale = hdr->ylen/(float)hdr->my;
  if (hdr->xlen && hdr->mz)
    iif->zscale = hdr->zlen/(float)hdr->mz;
  iif->xtrans = hdr->xorg;
  iif->ytrans = hdr->yorg;
  iif->ztrans = hdr->zorg;
  iif->xrot = hdr->tiltangles[3];
  iif->yrot = hdr->tiltangles[4];
  iif->zrot = hdr->tiltangles[5];

  iif->headerSize = 1024;
  iif->sectionSkip = 0;
  iif->header = (char *)hdr;

  iif->readSection = iiMRCreadSection;
  iif->readSectionByte = iiMRCreadSectionByte;
  iif->cleanUp = iiMRCdelete;

  return(0);
}

void iiMRCdelete(ImodImageFile *inFile)
{
  if (inFile->header)
    free(inFile->header);
} 

int iiMRCreadSection(ImodImageFile *inFile, char *buf, int inSection)
{
  struct LoadInfo li;
  MrcHeader *h = (MrcHeader *)inFile->header;

  li.xmin = inFile->llx;
  li.ymin = inFile->lly;
  li.zmin = inFile->llz;

  /* DNM 2/26/03: replace upper right only if negative */
  if (inFile->urx < 0)
    li.xmax = inFile->nx-1;
  else
    li.xmax = inFile->urx;
  if (inFile->ury < 0)
    li.ymax = inFile->ny-1;
  else
    li.ymax = inFile->ury;
  if (inFile->urz < 0)
    li.zmax = inFile->nz-1;
  else
    li.zmax = inFile->urz;
  li.slope = inFile->slope;
  li.offset = inFile->offset;
  li.outmin = inFile->smin;
  li.outmax = inFile->smax;
  li.black = 0;
  li.white = 255;
  li.axis = inFile->axis;
  li.mirrorFFT = 0;
  h->fp = inFile->fp;
  return (mrcReadSection(h, &li, (unsigned char *)buf, inSection));
}

int iiMRCreadSectionByte(ImodImageFile *inFile, char *buf, int inSection)
{
  struct LoadInfo li;
  MrcHeader *h = (MrcHeader *)inFile->header;
  li.xmin   = inFile->llx;
  li.ymin   = inFile->lly;
  li.zmin = inFile->llz;
  if (inFile->urx < 0)
    li.xmax = inFile->nx-1;
  else
    li.xmax = inFile->urx;
  if (inFile->ury < 0)
    li.ymax = inFile->ny-1;
  else
    li.ymax = inFile->ury;
  if (inFile->urz < 0)
    li.zmax = inFile->nz-1;
  else
    li.zmax = inFile->urz;

  li.slope  = inFile->slope;
  li.offset = inFile->offset;
  li.outmin   = 0;
  li.outmax   = 255;
  li.axis   = inFile->axis;
  li.mirrorFFT = inFile->mirrorFFT;
  h->fp = inFile->fp; 
  return (mrcReadSectionByte(h, &li, (unsigned char *)buf, inSection));
}



#define TILT_FLAG    1
#define MONTAGE_FLAG 2

int iiMRCLoadPCoord(ImodImageFile *inFile, struct LoadInfo *li, int nx, int ny,
                    int nz)
{
  int i;
  b3dUInt16 pcoordxy[2];
  b3dInt16 pcoordz;
  int extra_bytes[32];
  int extratot = 0;
  int offset=1024;
  int nread = nz;
  MrcHeader *hdr = (MrcHeader *)inFile->header;     
  int iflag = hdr->nreal;
  int nbytes = hdr->nint;
  int nextra = hdr->next;
  int flag_count;
  if(!nextra || !(iflag & MONTAGE_FLAG))
    return 0;

   b3dHeaderItemBytes(&flag_count, &extra_bytes[0]);

  /* DNM 12/10/01: as partial protection against mistaking other entries
     for montage information, at least make sure that the total bytes
     implied by the bits in the flag equals the nint entry.  Also
     reject deltavision files */
  /* DNM 2/3/02: make sure nreal also does not have bits beyond the flags */
  for (i = 0; i < flag_count; i++)
    if (iflag & (1 << i))
      extratot += extra_bytes[i];

  if (extratot != nbytes || hdr->creatid == -16224 || 
      iflag >= (1 << flag_count))
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

/*

$Log$
Revision 3.15  2007/06/13 17:11:26  sueh
bug# 1019 In iiMRCCheck, setting iif->sectionSkip to 0.

Revision 3.14  2006/09/03 22:17:59  mast
Reorganized and switched to IIERR codes

Revision 3.13  2005/12/20 01:39:36  mast
Changed piece coordinates to unsigned shorts

Revision 3.12  2005/11/11 22:15:23  mast
Changes for unsigned file mode

Revision 3.11  2005/10/13 20:06:26  mast
Fixed scale setting when mx/my/mz are zero

Revision 3.10  2004/11/05 18:53:04  mast
Include local files with quotes, not brackets

Revision 3.9  2004/11/04 17:10:27  mast
libiimod.def

Revision 3.8  2004/03/18 17:56:43  mast
Changed to calling central routine with extra header byte counts

Revision 3.7  2004/03/17 05:38:19  mast
Corrected byte count for 5th extra header entry (now one short)

Revision 3.6  2004/01/05 17:40:14  mast
Renamed imin/imax to outmin/outmax, and returned errors from mrcRead... calls

Revision 3.5  2003/11/01 16:42:16  mast
changed to use new error processing routine

Revision 3.4  2003/02/27 17:06:50  mast
Changed tests on upper coordinates to respect a value of 0

Revision 3.3  2002/09/14 00:59:38  mast
Fixed computation of scales to use mx indtead of nx

Revision 3.2  2002/02/26 22:56:32  mast
Improved protection against erroneous identification as montage

Revision 3.1  2001/12/17 18:54:45  mast
Added protections against non-Boulder files being recognized as montages

*/
