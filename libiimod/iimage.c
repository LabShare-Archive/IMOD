/*
 *    iimage.c    - general routines for manipulating ImodImageFile's
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
Revision 3.6  2004/11/30 03:46:44  mast
Added ability to a caller to put an arbitrary file check and open function
onto a list, after TIFF and MRC are checked

Revision 3.5  2004/11/04 17:10:27  mast
libiimod.def

Revision 3.4  2004/01/08 06:41:07  mast
Fixed complex scaling

Revision 3.3  2004/01/05 17:53:54  mast
Changed imin/imax to smin/smax and initialized axis to 3

Revision 3.2  2003/11/01 16:42:15  mast
changed to use new error processing routine

Revision 3.1  2003/02/27 17:05:37  mast
define coordinate upper limits as -1 initially to avoid confusion with a true
upper limit of 0

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "iimage.h"
#include "ilist.h"
#include "b3dutil.h"

/* The resident check list */
static Ilist *checkList = NULL;

/* Initialize check list: if it does not exist, allocate it and place TIFF and
   MRC functions on the list */
static int initCheckList()
{
  IIFileCheckFunction func;
  if (checkList)
    return 0;
  checkList = ilistNew(sizeof(IIFileCheckFunction), 4);
  if (!checkList)
    return 1;
  func = iiTIFFCheck;
  ilistAppend(checkList, &func);
  func = iiMRCCheck;
  ilistAppend(checkList, &func);
  return 0;
}

/* Add the given function to the check list */
void iiAddCheckFunction(IIFileCheckFunction func)
{
  if (initCheckList())
    return;
  ilistAppend(checkList, &func);
}

/* Create a new file structure and initialize to null values */ 
ImodImageFile *iiNew()
{
  ImodImageFile *ofile = (ImodImageFile *)malloc(sizeof(ImodImageFile));
     
  if (!ofile) 
    return NULL;
  memset(ofile, 0, sizeof(ImodImageFile));
  ofile->xscale = ofile->yscale = ofile->zscale = 1.0f;
  ofile->slope  = 1.0f;
  ofile->smax   = 255;
  ofile->axis   = 3;
  ofile->mirrorFFT = 0;
  ofile->format = IIFILE_UNKNOWN;
  ofile->fp     = NULL;
  ofile->readSection     = NULL;
  ofile->readSectionByte = NULL;
  ofile->cleanUp         = NULL;
  ofile->reopen          = NULL;
  ofile->close           = NULL;
  ofile->writeSection    = NULL;

  /* DNM 2/26/03: set upper right to -1 for later replacement */
  ofile->llx =  0;
  ofile->lly =  0;
  ofile->llz =  0;
  ofile->urx = -1;
  ofile->ury = -1;
  ofile->urz = -1;
  return(ofile);
}

/* Initialize the image file structure for the given size and other
   characteristics */
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

/* Try to open an image file with the given name and with the given mode */
ImodImageFile *iiOpen(char *filename, char *mode)
{
  ImodImageFile *ofile;
  IIFileCheckFunction *checkFunc;
  int i;

  if ((ofile = iiNew()) == NULL) 
    return NULL;
  ofile->fp = fopen(filename, mode);

  if (ofile->fp == NULL || initCheckList()) {
    iiDelete(ofile);
    return(NULL);
  }
  ofile->filename = strdup(filename);
  ofile->fmode = mode;
    
  /* Try to open the file with each of the check functions in turn 
   * until one succeeds
   */
  ofile->format = IIFILE_UNKNOWN;
  for (i = 0; i < ilistSize(checkList); i++) {
    checkFunc = (IIFileCheckFunction *)ilistItem(checkList, i);
    if (!(*checkFunc)(ofile)) {
      ofile->state = IISTATE_READY;
      return ofile;
    }
  }
  b3dError(stderr, "warning (%s) : unknown format.\n", filename);
  iiDelete(ofile);
  return NULL;
}

/* Reopen a file that has already been opened and analyzed */
int  iiReopen(ImodImageFile *inFile)
{
  IIFileCheckFunction *checkFunc;
  int i;

  if (!inFile)
    return -1;
  if (inFile->fp)
    return 1;
  if (!inFile->fmode)
    inFile->fmode = "rb";
  if (inFile->reopen) {
    if ((*inFile->reopen)(inFile))
      return 2;
    inFile->state = IISTATE_READY;
    return 0;
  }

  inFile->fp = fopen(inFile->filename, inFile->fmode);
  if (!inFile->fp)
    return 2;

  if (inFile->state == IISTATE_NOTINIT){
    inFile->format = IIFILE_UNKNOWN;
    for (i = 0; i < ilistSize(checkList); i++) {
      checkFunc = (IIFileCheckFunction *)ilistItem(checkList, i);
      if (!(*checkFunc)(inFile)) {
        inFile->state = IISTATE_READY;
        return 0;
      }
    }
  }
  return -1;
}

/* Set the scaling min/max for this file and compute scaling slope/offset
   Use the input inMin and inMax, or file min and max if they are not set 
   1/3/04: change from double to float for arguments */
int  iiSetMM(ImodImageFile *inFile, float inMin, float inMax)
{
  float range;

  /* DNM: only modify the existing smin, smax if incoming data is useful, and
     set the min and the max to 0, 255 if they are still equal */

  if (inMin != inMax) {
    inFile->smin = inMin;
    inFile->smax = inMax;
  }

  if (inFile->smin == inFile->smax){
    inFile->smin = 0;
    inFile->smax = 255;
  }

  /* DNM 1/7/04: do not modify smin/smax if complex scaling, just get it
     right for the slope and offset 
     Also, use new routine */
  inMin = inFile->smin;
  inMax = inFile->smax;

  /* DNM 2/16/01: set scaling properly for complex mode, the same as for
     full-file reads with mrc_read_byte */
  if (inFile->format == IIFORMAT_COMPLEX)
    mrcComplexSminSmax(inMin, inMax, &inMin, &inMax);

  range = inMax - inMin;
  inFile->slope = 255.0 / range;

  inFile->offset = -inMin * inFile->slope;

  /* printf("iiSetMM %g %g -> %g %g\n",
     inMin, inMax, inFile->slope, inFile->offset); */

  return(0);
}

/* Close an image file */
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

/* Delete an image file; first close and call any cleanup functions */
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

/* Read the given section from the file as raw data into the given buffer */
int iiReadSection(ImodImageFile *inFile, char *buf, int inSection)
{
  if (!inFile->readSection) 
    return -1;
  if (!inFile->fp){
    if (iiReopen(inFile))
      return -1;
  }
  return( (*inFile->readSection)(inFile, buf, inSection) );
}

/* Read the given section from the file as bytes into the given buffer */
int iiReadSectionByte(ImodImageFile *inFile, char *buf, int inSection)
{
  if (!inFile->readSectionByte) return -1;
  if (!inFile->fp){
    if (iiReopen(inFile))
      return -1;
  }
  return( (*inFile->readSectionByte)(inFile, buf, inSection) );
}

/* Write data in the buffer to the given section of the file */
int iiWriteSection(ImodImageFile *inFile, char *buf, int inSection)
{
  if (!inFile->writeSection) return -1;
  return( (*inFile->writeSection)(inFile, buf, inSection) );
}

/* Load piece coordinates from an MRC file */
int iiLoadPCoord(ImodImageFile *inFile, struct LoadInfo *li, int nx, int ny, 
                 int nz)
{
  if (iiMRCCheck(inFile))
    return (0);
  return(iiMRCLoadPCoord(inFile, li, nx, ny, nz));
}
