#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "iimage.h"

int iiTIFFCheck(ImodImageFile *inFile);
int iiMRCCheck(ImodImageFile *inFile);

ImodImageFile *iiNew()
{
     ImodImageFile *ofile = (ImodImageFile *)malloc(sizeof(ImodImageFile));
     
     if (!ofile) 
	  return NULL;
     memset(ofile, 0, sizeof(ImodImageFile));
     ofile->xscale = ofile->yscale = ofile->zscale = 1.0f;
     ofile->slope  = 1.0f;
     ofile->imax   = 255;
     ofile->format = IIFILE_UNKNOWN;
     ofile->fp     = NULL;
     ofile->readSection     = NULL;
     ofile->readSectionByte = NULL;
     ofile->cleanUp         = NULL;
     ofile->reopen          = NULL;
     ofile->close           = NULL;
     ofile->writeSection    = NULL;
     return(ofile);
}

int iiInit(ImodImageFile *i, int xsize, int ysize, int zsize, 
	   int file, int format, int type)
{
    if (!i)     return(-1);
    i->nx     = xsize;
    i->ny     = ysize;
    i->nz     = zsize;
    i->file   = file;
    i->format = format;
    i->type   = type;
    return(0);
}

ImodImageFile *iiOpen(char *filename, char *mode)
{
    ImodImageFile *ofile;
    if ((ofile = iiNew()) == NULL) return NULL;
    ofile->fp = fopen(filename, mode);

    if (ofile->fp == NULL){
	iiDelete(ofile);
	return(NULL);
    }
    ofile->filename = strdup(filename);
    ofile->fmode = mode;
    
    /* Set file format, add new formats here.
     * Formats should fill in rest of sturcture as needed. 
     */
    ofile->format = IIFILE_UNKNOWN;
    if (iiTIFFCheck(ofile))
	if (iiMRCCheck(ofile)){
	    fprintf(stderr, "warning (%s) : unknown format.\n", filename);
	    iiDelete(ofile);
	    return NULL;
	}
    ofile->state = IISTATE_READY;
    return ofile;
}

int  iiReopen(ImodImageFile *inFile)
{
    if (!inFile) return -1;
    if (inFile->fp) return 1;
    if (!inFile->fmode) inFile->fmode = "r";
    if (inFile->reopen) {
	 if ((*inFile->reopen)(inFile))
	      return 2;
	 inFile->state = IISTATE_READY;
	 return 0;
    }

    inFile->fp = fopen(inFile->filename, inFile->fmode);
    if (!inFile->fp) return 2;

    if (inFile->state == IISTATE_NOTINIT){
	inFile->format = IIFILE_UNKNOWN;
	if (iiTIFFCheck(inFile))
	if (iiMRCCheck(inFile)){
	    return -1;
	}
    }

    inFile->state = IISTATE_READY;
    return 0;
}

int  iiSetMM(ImodImageFile *inFile, double inMin, double inMax)
{
    float range;
    int black = 0, white = 255;

    /* DNM: only modify the existing imin, imax if incoming data is useful, and
       set the min and the max to 0, 255 if they are still equal */

    if (inMin != inMax) {
	 inFile->imin = inMin;
	 inFile->imax = inMax;
    }

    if (inFile->imin == inFile->imax){
	 inFile->imin = 0;
	 inFile->imax = 255;
    }

    /* DNM 2/16/01: set scaling properly for complex mode, the same as for
       full-file reads with mrc_read_byte */
    if (inFile->format == IIFORMAT_COMPLEX) {
	 inFile->imin = log(1.0 + 5.0 * inFile->imin);
	 inFile->imax = log(1.0 + 5.0 * inFile->imax);
    }

    range = inFile->imax - inFile->imin;
    inFile->slope = 255.0 / range;

    inFile->offset = -inFile->imin * inFile->slope;

    /* printf("iiSetMM %g %g -> %g %g\n",
       inMin, inMax, inFile->slope, inFile->offset); */

    return(0);
}

void iiClose(ImodImageFile *inFile)
{
     if (inFile->close)
	  (*inFile->close)(inFile);
     else if (inFile->fp != NULL) 
	  fclose(inFile->fp);
     inFile->fp = NULL;
     if (inFile->state != IISTATE_NOTINIT)
	  inFile->state = IISTATE_PARK;
}

void iiDelete(ImodImageFile *inFile)
{
    if (!inFile) 
	return;
    iiClose(inFile);
    if (inFile->filename)
	free(inFile->filename);
    if (inFile->cleanUp)
	 (*inFile->cleanUp)(inFile);
    if (inFile->description)
	free(inFile->description);
    free(inFile);
}

int iiReadSection(ImodImageFile *inFile, char *buf, int inSection)
{
    if (!inFile->readSection) return -1;
    if (!inFile->fp){
	 if (iiReopen(inFile))
	      return -1;
    }
    return( (*inFile->readSection)(inFile, buf, inSection) );
}

int iiReadSectionByte(ImodImageFile *inFile, char *buf, int inSection)
{
     if (!inFile->readSectionByte) return -1;
     if (!inFile->fp){
	  if (iiReopen(inFile))
	       return -1;
     }
     return( (*inFile->readSectionByte)(inFile, buf, inSection) );
}


int iiWriteSection(ImodImageFile *inFile, char *buf, int inSection)
{
    if (!inFile->writeSection) return -1;
    return( (*inFile->writeSection)(inFile, buf, inSection) );
}

int iiLoadPCoord(ImodImageFile *inFile, struct LoadInfo *li, int nx, int ny, 
			       int nz)
{
     if (iiMRCCheck(inFile))
	  return (0);
     return(iiMRCLoadPCoord(inFile, li, nx, ny, nz));
}
