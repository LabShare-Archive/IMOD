/*
 *    iimrc.c    - specific routines for mrc-type ImodImageFile's
 *
 *    Authors:  James Kremer and David Mastronarde
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 */
/*  $Author$

$Date$

$Revision$

$Log$
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
#include <stdlib.h>
#include "mrcc.h"
#include "iimage.h"
#include "b3dutil.h"

int iiMRCreadSection(ImodImageFile *inFile, char *buf, int inSection)
{
  struct LoadInfo li;
  struct MRCheader *h = (struct MRCheader *)inFile->header;

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
  struct MRCheader *h = (struct MRCheader *)inFile->header;
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

void iiMRCdelete(ImodImageFile *inFile)
{
  if (inFile->header)
    free(inFile->header);
} 

int iiMRCCheck(ImodImageFile *i)
{
  FILE *fp;
  struct MRCheader *hdr;
  if (!i) return -1;
  fp = i->fp;
  if (!fp) return 1;

  hdr = (struct MRCheader *)malloc(sizeof(struct MRCheader));
  if (mrc_head_read(fp, hdr)){
    free(hdr);
    return(1);
  }
  i->nx   = hdr->nx;
  i->ny   = hdr->ny;
  i->nz   = hdr->nz;
  i->file = IIFILE_MRC;

  switch(hdr->mode){
  case MRC_MODE_BYTE:
    i->format = IIFORMAT_LUMINANCE;
    i->type   = IITYPE_UBYTE;
    break;
  case MRC_MODE_SHORT:
    i->format = IIFORMAT_LUMINANCE;
    i->type   = IITYPE_SHORT;
    break;
  case MRC_MODE_FLOAT:
    i->format = IIFORMAT_LUMINANCE;
    i->type   = IITYPE_FLOAT;
    break;
  case MRC_MODE_COMPLEX_SHORT:
    i->format = IIFORMAT_COMPLEX;
    i->type   = IITYPE_SHORT;
    break;
  case MRC_MODE_COMPLEX_FLOAT:
    i->format = IIFORMAT_COMPLEX;
    i->type   = IITYPE_FLOAT;
    break;
  case MRC_MODE_RGB:
    i->format = IIFORMAT_RGB;
    i->type   = IITYPE_UBYTE;
    break;
  }
  i->mode  = hdr->mode;
  i->amin  = hdr->amin;
  i->amax  = hdr->amax;
  i->smin  = i->amin;
  i->smax  = i->amax;
  i->amean = hdr->amean;
  /* DNM 11/5/98: inverted these expressions to give proper usage */
  /* DNM 9/13/02: needed to divide by mx, ny, nz, not nx, ny, nz */
  if (hdr->xlen)
    i->xscale = hdr->xlen/(float)hdr->mx;
  if (hdr->ylen)
    i->yscale = hdr->ylen/(float)hdr->my;
  if (hdr->xlen)
    i->zscale = hdr->zlen/(float)hdr->mz;
  i->xtrans = hdr->xorg;
  i->ytrans = hdr->yorg;
  i->ztrans = hdr->zorg;
  i->xrot = hdr->tiltangles[3];
  i->yrot = hdr->tiltangles[4];
  i->zrot = hdr->tiltangles[5];

  i->headerSize = 1024;
  i->header = (char *)hdr;

  i->readSection = iiMRCreadSection;
  i->readSectionByte = iiMRCreadSectionByte;
  i->cleanUp = iiMRCdelete;

  return(0);
}

#define TILT_FLAG    1
#define MONTAGE_FLAG 2

int iiMRCLoadPCoord(ImodImageFile *inFile, struct LoadInfo *li, int nx, int ny,
                    int nz)
{
  int i;
  short int pcoords[3];
  int extra_bytes[32];
  int extratot = 0;
  int offset=1024;
  int nread = nz;
  struct MRCheader *hdr = (struct MRCheader *)inFile->header;     
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
    fread(pcoords, 3, 2, inFile->fp);

    /* add swapping 10/2/00 */
    if (hdr->swapped)
      mrc_swap_shorts(pcoords, 3);
    if (ferror(inFile->fp)) {
      nread = i;
      b3dError(stderr, "Error reading piece coordinates from extra"
              " header after %d frames\n", i);
      break;
    }

    li->pcoords[(i*3)]   = pcoords[0];
    li->pcoords[(i*3)+1] = pcoords[1];
    li->pcoords[(i*3)+2] = pcoords[2];

    offset = nbytes - 6;
    if (offset > 0)
      fseek(inFile->fp, offset, SEEK_CUR);
  }
  li->plist = nread;
  return(mrc_plist_proc(li, nx, ny, nz));
}
