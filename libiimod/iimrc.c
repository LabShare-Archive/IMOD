/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.2  2002/02/26 22:56:32  mast
    Improved protection against erroneous identification as montage

    Revision 3.1  2001/12/17 18:54:45  mast
    Added protections against non-Boulder files being recognized as montages

*/
#include <stdlib.h>
#include <mrcc.h>
#include "iimage.h"

int iiMRCreadSection(ImodImageFile *inFile, char *buf, int inSection)
{
     struct LoadInfo li;
     struct MRCheader *h = (struct MRCheader *)inFile->header;

     li.xmin = inFile->llx;
     li.ymin = inFile->lly;
     li.zmin = inFile->llz;
     if (!inFile->urx)
	 li.xmax = inFile->nx-1;
     else
	 li.xmax = inFile->urx;
     if (!inFile->ury)
	 li.ymax = inFile->ny-1;
     else
	 li.ymax = inFile->ury;
     if (inFile->urz == 0)
	 li.zmax = inFile->nz-1;
     else
	 li.zmax = inFile->urz;
     li.slope = inFile->slope;
     li.offset = inFile->offset;
     li.imin = inFile->imin;
     li.imax = inFile->imax;
     li.black = 0;
     li.white = 255;
     li.axis = inFile->axis;
     h->fp = inFile->fp;
     mrcReadSection(h, &li, (unsigned char *)buf, inSection);
     return(0);
}

int iiMRCreadSectionByte(ImodImageFile *inFile, char *buf, int inSection)
{
     struct LoadInfo li;
     struct MRCheader *h = (struct MRCheader *)inFile->header;
     li.xmin   = inFile->llx;
     li.ymin   = inFile->lly;
     li.zmin = inFile->llz;
     if (inFile->urx == 0)
	 li.xmax = inFile->nx-1;
     else
	 li.xmax = inFile->urx;
     if (inFile->ury == 0)
	 li.ymax = inFile->ny-1;
     else
	 li.ymax = inFile->ury;
     if (inFile->urz == 0)
	 li.zmax = inFile->nz-1;
     else
	 li.zmax = inFile->urz;

     li.slope  = inFile->slope;
     li.offset = inFile->offset;
     li.imin   = 0;
     li.imax   = 255;
     li.axis   = inFile->axis;
     h->fp = inFile->fp; 
     mrcReadSectionByte(h, &li, (unsigned char *)buf, inSection);
     return(0);
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
    i->imin  = i->amin;
    i->imax  = i->amax;
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
#define FLAG_COUNT 5

int iiMRCLoadPCoord(ImodImageFile *inFile, struct LoadInfo *li, int nx, int ny,
		    int nz)
{
     int i;
     short int pcoords[3];
     int extra_bytes[FLAG_COUNT] = {2, 6, 4, 2, 4};
     int extratot = 0;
     int offset=1024;
     int nread = nz;
     struct MRCheader *hdr = (struct MRCheader *)inFile->header;     
     int iflag = hdr->nreal;
     int nbytes = hdr->nint;
     int nextra = hdr->next;
     if(!nextra || !(iflag & MONTAGE_FLAG))
	  return 0;

     /* DNM 12/10/01: as partial protection against mistaking other entries
	for montage information, at least make sure that the total bytes
	implied by the bits in the flag equals the nint entry.  Also
	reject deltavision files */
     /* DNM 2/3/02: make sure nreal also does not have bits beyond the flags */
     for (i = 0; i < FLAG_COUNT; i++)
	  if (iflag & (1 << i))
	       extratot += extra_bytes[i];

     if (extratot != nbytes || hdr->creatid == -16224 || 
	 iflag >= (1 << FLAG_COUNT))
	  return 0;

     if (iflag & TILT_FLAG)
	  offset += 2;

     if (nbytes * nz > nextra) {
	  nread = nextra / nbytes;
	  fprintf(stderr, "There are piece coordinates for only %d frames in"
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
	       fprintf(stderr, "Error reading piece coordinates from extra"
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
