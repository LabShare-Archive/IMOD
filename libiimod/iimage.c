/*
 *    iimage.c    - general routines for manipulating ImodImageFile's
 *
 *    Authors:  James Kremer and David Mastronarde
 *
 *   Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *
 *  $Id$
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "iimage.h"
#include "ilist.h"
#include "b3dutil.h"

/* The resident check list and list of opened files */
static Ilist *sCheckList = NULL;
static Ilist *sOpenedFiles = NULL;
static int sRWcallCount = 0;

static int readWriteSection(ImodImageFile *inFile, char *buf, int inSection, 
                            iiSectionFunc func, const char *mess);
static int initCheckList();
static int addToOpenedList(ImodImageFile *iiFile);
static void removeFromOpenedList(ImodImageFile *iiFile);
static int findFileInList(ImodImageFile *iiFile, FILE *fp);

/* Initialize check list: if it does not exist, allocate it and place TIFF and
   MRC functions on the list */
static int initCheckList()
{
  IIFileCheckFunction func;
  if (sCheckList)
    return 0;
  sCheckList = ilistNew(sizeof(IIFileCheckFunction), 6);
  if (!sCheckList)
    return 1;
  func = iiTIFFCheck;
  ilistAppend(sCheckList, &func);
  func = iiMRCCheck;
  ilistAppend(sCheckList, &func);
  func = iiLikeMRCCheck;
  ilistAppend(sCheckList, &func);
  return 0;
}

/*!
 * Adds the given function to the end of the list of functions that iiOpen will
 * call to check a file.  The format of the function is
 * ^  int iiFormatCheck(ImodImageFile *inFile)
 */
void iiAddCheckFunction(IIFileCheckFunction func)
{
  if (initCheckList())
    return;
  ilistAppend(sCheckList, &func);
}

/*!
 * Insert the given function into the list of functions that iiOpen will
 * call to check a file at position [index] in the list.  The format of the
 * function is
 * ^  int iiFormatCheck(ImodImageFile *inFile)
 */
void iiInsertCheckFunction(IIFileCheckFunction func, int index)
{
  if (initCheckList())
    return;
  if (index < ilistSize(sCheckList))
    ilistInsert(sCheckList, &func, index);
  else
    ilistAppend(sCheckList, &func);
}

/*!
 * Frees the checking list to avoid memory leaks
 */
void iiDeleteCheckList()
{
  if (!sCheckList)
    return;
  ilistDelete(sCheckList);
  sCheckList = NULL;
}

/*!
 * Creates a new image file structure and initialize it to default or
 * null values.  Returns 1 for error.
*/ 
ImodImageFile *iiNew()
{
  ImodImageFile *ofile = (ImodImageFile *)malloc(sizeof(ImodImageFile));
     
  if (!ofile) {
    b3dError(stderr, "ERROR: iiNew -Allocating new ImodImageFile structure\n");
    return NULL;
  }
  memset(ofile, 0, sizeof(ImodImageFile));
  ofile->xscale = ofile->yscale = ofile->zscale = 1.0f;
  ofile->slope  = 1.0f;
  ofile->smax   = 255;
  ofile->axis   = 3;
  ofile->mirrorFFT = 0;
  ofile->anyTiffPixSize = 0;
  ofile->format = IIFILE_UNKNOWN;
  ofile->fp     = NULL;
  ofile->readSection     = NULL;
  ofile->readSectionByte = NULL;
  ofile->readSectionUShort = NULL;
  ofile->readSectionFloat  = NULL;
  ofile->writeSection      = NULL;
  ofile->writeSectionFloat = NULL;
  ofile->fillMrcHeader = NULL;
  ofile->syncFromMrcHeader = NULL;
  ofile->cleanUp         = NULL;
  ofile->reopen          = NULL;
  ofile->close           = NULL;
  ofile->writeSection    = NULL;
  ofile->colormap        = NULL;

  /* DNM 2/26/03: set upper right to -1 for later replacement */
  ofile->llx =  0;
  ofile->lly =  0;
  ofile->llz =  0;
  ofile->urx = -1;
  ofile->ury = -1;
  ofile->urz = -1;
  ofile->padLeft = 0;
  ofile->padRight = 0;
  ofile->nx = 0;
  ofile->ny = 0;
  ofile->nz = 0;
  ofile->lastWrittenZ = -1;
  return(ofile);
}

/*!
 * Initializes the image file structure [i] for the given size and other
 * characteristics 
 */
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

/*!
 * Tries to open an image file with name [filename] and with the fopen mode 
 * [mode] (e.g. "rb"), using the file format check functions on the list.
 * If [filename] is NULL or an empty string, then it assigns stdin to the file
 * pointer.  Returns NULL for error; it and all checking routines should call 
 * b3dError with their error strings.
 */
ImodImageFile *iiOpen(const char *filename, const char *mode)
{
  ImodImageFile *ofile;
  IIFileCheckFunction *checkFunc;
  int i, err = 0;

  /* If the mode contains w for a new file, call the open routine for a default file */
  if (strstr(mode, "w"))
    return iiOpenNew(filename, mode, IIFILE_DEFAULT);

  if ((ofile = iiNew()) == NULL) 
    return NULL;
  if (filename && filename[0])
    ofile->fp = fopen(filename, mode);
  else
    ofile->fp = stdin;

  if (ofile->fp == NULL || initCheckList()) {
    b3dError(stderr, "ERROR: iiOpen - Opening file %s\n", filename);
    iiDelete(ofile);
    return(NULL);
  }
  ofile->format = IIFILE_UNKNOWN;
  ofile->filename = strdup(filename);
  strncpy(ofile->fmode, mode, 3);
    
  /* Try to open the file with each of the check functions in turn 
   * until one succeeds
   */
  for (i = 0; i < ilistSize(sCheckList); i++) {
    checkFunc = (IIFileCheckFunction *)ilistItem(sCheckList, i);

    /* If file was closed and couldn't reopen, bail out */
    if (ofile->fp == NULL) {
      b3dError(stderr, "ERROR: iiOpen - %s could not be reopened\n", filename);
      break;
    }

    if (!(err = (*checkFunc)(ofile))) {
      ofile->state = IISTATE_READY;
      if (!addToOpenedList(ofile))
        return ofile;
      iiDelete(ofile);
      return NULL;
    }
    
    if (err != IIERR_NOT_FORMAT)
      break;
  }
  
  if (err == IIERR_NOT_FORMAT)
    b3dError(stderr, "ERROR: iiOpen - %s has unknown format.\n", filename);
  iiDelete(ofile);
  return NULL;
}

/*!
 * Opens a new file whose name is in [filename] with the given [mode], which must
 * contain 'w'.  The type of file is specified by [fileKind], which can be either
 * IIFILE_MRC, IIFILE_TIFF, or IIFILE_DEFAULT for the type defined by calling
 * @@b3dutil.html#b3dOutputFileType@.  Adds the file to the list of opened files and
 * returns the opened file pointer.
 */
ImodImageFile *iiOpenNew(const char *filename, const char *mode, int fileKind)
{
  ImodImageFile *ofile;
  int err = 0;
  if (fileKind == IIFILE_DEFAULT)
    fileKind = b3dOutputFileType();
  //printf("fk %d\n", fileKind);
  if (!strstr(mode, "w")) {
    b3dError(stderr, "ERROR: iiOpenNew - File opening mode %s is not appropriate for a "
             "new image file\n");
    return NULL;
  }
      
  if ((ofile = iiNew()) == NULL) 
    return NULL;
  ofile->filename = strdup(filename);
  if (!ofile->filename) {
    err = 1;
  }
   
  if (!err && fileKind == IIFILE_MRC) {
    err = iiMRCopenNew(ofile, mode);
  } else if (!err && fileKind == IIFILE_TIFF) {
    err = tiffOpenNew(ofile);
    if (err)
      b3dError(stderr, "ERROR: iiOpenNew - Opening new TIFF file\n");
  } else if (!err) {
    b3dError(stderr, "ERROR: iiOpenNew - Cannot open new files with file format %d\n", 
             fileKind);
    err = 1;
  }
  
  if (!err) {
    ofile->file = fileKind;
    ofile->newFile = 1;
    strncpy(ofile->fmode, "rb+", 3);
    ofile->state = IISTATE_READY;
    if (!addToOpenedList(ofile))
      return ofile;
  }

  /* Fall through to here on any kind of error requiring deletion of the file */
  iiDelete(ofile);
  return NULL;
}

/*!
 * Reopen a file that has already been opened and analyzed 
 */
int  iiReopen(ImodImageFile *inFile)
{
  IIFileCheckFunction *checkFunc;
  int i;

  if (!inFile)
    return -1;
  if (inFile->fp)
    return 1;
  if (!inFile->fmode[0])
    strncpy(inFile->fmode, "rb+", 3);
  if (inFile->reopen) {
    if ((*inFile->reopen)(inFile))
      return 2;
    inFile->state = IISTATE_READY;
    return 0;
  }

  inFile->fp = fopen(inFile->filename, inFile->fmode);
  if (!inFile->fp)
    return 2;

  /* Add back to opened list: failure is not an error because FP-based programs don't
     use iiReopen */
  addToOpenedList(inFile);

  if (inFile->state != IISTATE_NOTINIT) {
    inFile->state = IISTATE_READY;
    return 0;
  }

  /* If the file is not initted yet, treat it as unknown and deal with from scratch */
  inFile->format = IIFILE_UNKNOWN;
  for (i = 0; i < ilistSize(sCheckList); i++) {
    checkFunc = (IIFileCheckFunction *)ilistItem(sCheckList, i);
    if (!(*checkFunc)(inFile)) {
      inFile->state = IISTATE_READY;
      return 0;
    }
  }
  return -1;
}

/*!
 * Sets the scaling min and max ({smin} and {smax} in the image file structure
 * [inFile] and computes the scaling {slope} and {offset} that will map {smin} to
 * and {smax} to [scaleMax] by scaling with value * slope + offset.
 * Uses the input values [inMin] and [inMax], or the file min and max if these
 * values are equal.  Returns 0.
 */
 /* 1/3/04: change from double to float for arguments */
int  iiSetMM(ImodImageFile *inFile, float inMin, float inMax, float scaleMax)
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
  inFile->slope = scaleMax / range;

  inFile->offset = -inMin * inFile->slope;

  /* printf("iiSetMM %g %g -> %g %g\n",
     inMin, inMax, inFile->slope, inFile->offset); */

  return(0);
}

/*!
 * Closes an image file [inFile] and removes it from the list of opened files
 */
void iiClose(ImodImageFile *inFile)
{
  if (inFile->close)
    (*inFile->close)(inFile);
  else if (inFile->fp != NULL) 
    fclose(inFile->fp);
  inFile->fp = NULL;
  removeFromOpenedList(inFile);
  if (inFile->state != IISTATE_NOTINIT)
    inFile->state = IISTATE_PARK;
}

/*!
 * Deletes an image file [inFile] after closing and calling any cleanup
 * functions 
 */
void iiDelete(ImodImageFile *inFile)
{
  if (!inFile) 
    return;
  iiClose(inFile);
  B3DFREE(inFile->filename);
  if (inFile->cleanUp)
    (*inFile->cleanUp)(inFile);
  B3DFREE(inFile->description);
  B3DFREE(inFile->colormap);
  free(inFile);
}

/*!
 * For the image file [inFile], initializes the MRC header structure [data] and fills in
 * as many fields as possible with file-specific values.  For an MRC file, it simply 
 * copies the already read-in header.  Returns -1 if a header filling function is not 
 * defined or 1 if there is some other problem filling the header in.
 */
int iiFillMrcHeader(ImodImageFile *inFile, MrcHeader *hdata)
{
  if (!inFile || !inFile->fillMrcHeader)
    return 1;
  return inFile->fillMrcHeader(inFile, hdata);
}

/*!
 * For the image file [inFile], synchronizes numerous values in the ImodImageFile 
 * structure from values in [hdata], including size, mode, scale, origin, rotation,
 * and min/max/mean values.
 */
void iiSyncFromMrcHeader(ImodImageFile *inFile, MrcHeader *hdata)
{

  /* Do not write signed bytes to a TIFF file!  It works fine for us and 3dmod can show
   them, but no other image viewers (including photoshop and gimp) read them right */
  int bytesSigned = hdata->bytesSigned && 
    !(inFile->file == IIFILE_TIFF && inFile->newFile) ? 1 : 0;
  if (hdata->mode != inFile->mode || !hdata->mode)
    iiMRCmodeToFormatType(inFile, hdata->mode, bytesSigned);

  /* These all came from the iimrc opening function.  It is not clear if any file format
     will need to have all those values synced after opening, but here they are. */
  inFile->nx   = hdata->nx;
  inFile->ny   = hdata->ny;
  inFile->nz   = hdata->nz;
  inFile->amin  = hdata->amin;
  inFile->amax  = hdata->amax;
  inFile->amean = hdata->amean;
  inFile->xscale = inFile->yscale = inFile->zscale = 1.;

  /* DNM 11/5/98: inverted these expressions to give proper usage */
  /* DNM 9/13/02: needed to divide by mx, ny, nz, not nx, ny, nz */
  if (hdata->xlen && hdata->mx)
    inFile->xscale = hdata->xlen/(float)hdata->mx;
  if (hdata->ylen && hdata->my)
    inFile->yscale = hdata->ylen/(float)hdata->my;
  if (hdata->xlen && hdata->mz)
    inFile->zscale = hdata->zlen/(float)hdata->mz;
  inFile->xtrans = hdata->xorg;
  inFile->ytrans = hdata->yorg;
  inFile->ztrans = hdata->zorg;
  inFile->xrot = hdata->tiltangles[3];
  inFile->yrot = hdata->tiltangles[4];
  inFile->zrot = hdata->tiltangles[5];

  /* And synchronize a few more things */
  inFile->headerSize = hdata->headerSize;
  inFile->sectionSkip = hdata->sectionSkip;
  if (inFile->syncFromMrcHeader)
    inFile->syncFromMrcHeader(inFile, hdata);
}

/*
 * Functions for adding, removing, and looking up a file in the list
 */
static int addToOpenedList(ImodImageFile *iiFile)
{
  if (!sOpenedFiles)
    sOpenedFiles = ilistNew(sizeof(ImodImageFile *), 4);
  if (sOpenedFiles && !ilistAppend(sOpenedFiles, &iiFile))
    return 0;
  b3dError(stderr, "ERROR: iiOpen - Memory error adding new file to master list\n");
    return 1;
}

static void removeFromOpenedList(ImodImageFile *iiFile)
{
  int i = findFileInList(iiFile, NULL);
  if (i >= 0)
    ilistRemove(sOpenedFiles, i);
}

/*
 * Find a file either by its iiFile or its fp entry 
 */
static int findFileInList(ImodImageFile *iiFile, FILE *fp)
{
  int i;
  ImodImageFile **listPtr;
  for (i = 0; i < ilistSize(sOpenedFiles); i++) {
    listPtr = (ImodImageFile **)ilistItem(sOpenedFiles, i);
    if ((iiFile && *listPtr == iiFile) || (fp && (*listPtr)->fp == fp)) {
      return i;
    }
  }
  return -1;
}

/*!
 * Opens an old or new ImodImageFile with the given [filename] and [mode], adds it to the
 * list of opened files, and returns the file pointer.  When this function is used instead
 * of fopen to open an image file, then most MRC file reading and writing functions 
 * listed in @@mrcfiles.html#TOP@, as well as @@mrcfiles.html#mrc_head_read@ and 
 * @@mrcfiles.html#mrc_head_write@, will look up the file pointer in the list of opened
 * files and perform the operation appropriate for the particular file type.
 */
FILE *iiFOpen(const char *filename, const char *mode)
{
  ImodImageFile *iiFile = iiOpen(filename, mode);
  if (!iiFile)
    return NULL;
  return iiFile->fp;
}

/*!
 * Returns the open ImodImageFile that has [fp] as its {fp} member.
 */
ImodImageFile *iiLookupFileFromFP(FILE *fp)
{
  ImodImageFile **listPtr;
  int i = findFileInList(NULL, fp);
  if (i < 0)
    return NULL;
  listPtr = (ImodImageFile **)ilistItem(sOpenedFiles, i);
  return *listPtr;
}

/*!
 * Closes the file with pointer [fp] either by calling @iiDelete for its corresponding 
 * ImodImageFile, or by simple closing it if it is not in the list of opened files.
 * This function must be called for a newly written file if there is a possibility that
 * it is a TIFF file.
 */
void iiFClose(FILE *fp)
{
  ImodImageFile *iiFile = iiLookupFileFromFP(fp);
  if (iiFile)
    iiDelete(iiFile);
  else
    fclose(fp);
}

/* Fuctions for keeping track of whether we are already in an iiRead/Write call */
void iiChangeCallCount(int delta)
{
  sRWcallCount = B3DMAX(0, sRWcallCount + delta);
}

int iiCallingReadOrWrite()
{
  return sRWcallCount;
}

/*!
 * Reads the section [inSection] from the file [inFile] as raw data into buffer
 * [buf].  Returns -1 for undefined reading function or failure to reopen file,
 * otherwise passes along return value of the reading function.
 */
int iiReadSection(ImodImageFile *inFile, char *buf, int inSection)
{
  return( readWriteSection(inFile, buf, inSection, inFile->readSection, "reading from") );
}

/*!
 * Reads the section [inSection] from the file [inFile] as scaled byte data 
 * into buffer [buf].  Returns -1 for undefined reading function or failure 
 * to reopen file, otherwise passes along return value of the reading function.
 */
int iiReadSectionByte(ImodImageFile *inFile, char *buf, int inSection)
{
  return( readWriteSection(inFile, buf, inSection, inFile->readSectionByte,
                           "reading and converting to bytes for") );
}

/*!
 * Reads the section [inSection] from the file [inFile] as scaled unsigned short data 
 * into buffer [buf].  Returns -1 for undefined reading function or failure 
 * to reopen file, otherwise passes along return value of the reading function.
 */
int iiReadSectionUShort(ImodImageFile *inFile, char *buf, int inSection)
{
  return( readWriteSection(inFile, buf, inSection, inFile->readSectionUShort,
                           "reading and converting to shorts for") );
}

/*!
 * Reads the section [inSection] from the file [inFile] as unscaled floating point values
 * into buffer [buf].  Returns -1 for undefined reading function or failure 
 * to reopen file, otherwise passes along return value of the reading function.
 */
int iiReadSectionFloat(ImodImageFile *inFile, char *buf, int inSection)
{
  return( readWriteSection(inFile, buf, inSection, inFile->readSectionFloat,
                           "reading and converting to floats for") );
}

/*!
 * Write data in the buffer [buf] to section [inSection] of the file [inFile].
 * Returns -1 for undefined writing function or failure to reopen file; otherwise passes
 * along return value of the writing function.
 */
int iiWriteSection(ImodImageFile *inFile, char *buf, int inSection)
{
  return( readWriteSection(inFile, buf, inSection, inFile->writeSection, "writing to") );
}

/*!
 * Write floating point or complex data in the buffer [buf] to section [inSection] of the 
 * file [inFile].  Returns -1 for undefined writing function or failure to reopen file;
 * otherwise passes along return value of the writing function.
 */
int iiWriteSectionFloat(ImodImageFile *inFile, char *buf, int inSection)
{
  return( readWriteSection(inFile, buf, inSection, inFile->writeSectionFloat, 
                           "converting floats to write to") );
}

/* The routine that does the work */
static int readWriteSection(ImodImageFile *inFile, char *buf, int inSection, 
                            iiSectionFunc func, const char *mess)
{
  int err;
  if (!func) {
    b3dError(stderr, "ERROR: iiRead/WriteSection - There is no function for %s this "
             "type of file\n", mess);
    return -1;
  }
  if (!inFile->fp){
    if (iiReopen(inFile))
      return -1;
  }
  iiChangeCallCount(1);
  err = func(inFile, buf, inSection);
  iiChangeCallCount(-1);
  return(err);
}

/*!
 * Returns the value of a single pixel at [x, y, z] from the image fioe [inFile].
 * Gives the value directly for byte,
 * short, and float data, (by calling @mrcReadZFloat) and the magnitude for complex data 
 * (after calling @mrcReadZ).  Returns the file minimum {hdata->amin} for coordinates 
 * out of bounds or if there is no function for reading floats for a file type.
 */
float iiReadPoint(ImodImageFile *inFile, int x, int y, int z)
{
  b3dInt16 sdata[2];
  float fdata = inFile->amin;
  float cfdata[2];
  int llx, lly, urx, ury, axis;
  ImodImageFile iiSave;

  if (x < 0 || y < 0 || z < 0 || 
      x >= inFile->nx || y >= inFile->ny || z >= inFile->nz)
    return(fdata);

  iiSaveLoadParams(inFile, &iiSave);
  inFile->llx = inFile->urx = x;
  inFile->lly = inFile->ury = y;
  inFile->axis = 3;
  if (inFile->mode == MRC_MODE_COMPLEX_SHORT) {
    if (!iiReadSection(inFile, (char *)&sdata[0], z))
      fdata = (float)sqrt((double)sdata[0] * sdata[0] + (double)sdata[1] * sdata[1]);
  } else if (inFile->mode == MRC_MODE_COMPLEX_FLOAT) {
    if (!iiReadSection(inFile, (char *)&cfdata[0], z))
      fdata = (float)sqrt((double)cfdata[0] * cfdata[0] + (double)cfdata[1] * cfdata[1]);
  } else {
    iiReadSectionFloat(inFile, (char *)&fdata, z);
  }
  iiRestoreLoadParams(0, inFile, &iiSave);
  return(fdata);
}


/*!
 * Loads piece coordinates from an MRC file [inFile] of size [nx], [ny], [nz]
 * and places them in the LoadInfo structure [li].  If no coordinates are found
 * and [useMdoc] is non-zero, it then tries to load coordinates from a metadata
 * file named as the image filename plus .mdoc.  Returns 0 regardless of
 * whether there are piece coordinates or errors.
 */
int iiLoadPCoord(ImodImageFile *inFile, int useMdoc, IloadInfo *li, int nx,
                 int ny, int nz)
{
  int err;
  if (iiMRCCheck(inFile))
    return (0);
  iiMRCLoadPCoord(inFile, li, nx, ny, nz);
  if (!li->plist && useMdoc)
    err = iiPlistFromMetadata(inFile->filename, 1, li, nx, ny, nz);
  return 0;
}

/*!
 * Converts [nx] floating point values in the array [fbufp] to values in the mode 
 * specified by [mrcMode], with signed bytes produced if [byteSigned] is nonzero.
 * Values are placed in [bdata].
 */
void iiConvertLineOfFloats(float *fbufp, unsigned char *bdata, int nx, int mrcMode, 
                           int bytesSigned)
{
  int i, ival;
  b3dInt16 *sdata = (b3dInt16 *)bdata;
  b3dUInt16 *usdata = (b3dUInt16 *)bdata;
  char *sbdata = (char *)bdata;

  /* Conversions of float to int/byte */
  switch (mrcMode) {
  case MRC_MODE_BYTE:
    if (bytesSigned) {
      
      /* Signed bytes */
      for (i = 0; i < nx; i++) {
        ival = (int)floor(fbufp[i] - 127.5);
        B3DCLAMP(ival, -128, 127);
        sbdata[i] = (char)ival;
      }
    } else {

      /* Unsigned bytes */
      for (i = 0; i < nx; i++) {
        ival = (int)(fbufp[i] + 0.5f);
        B3DCLAMP(ival, 0, 255);
        bdata[i] = (unsigned char)ival;
      }
    }
    break;

    /* Signed shorts */
  case MRC_MODE_SHORT:
    for (i = 0; i < nx; i++) {
      ival = (int)floor(fbufp[i] + 0.5);
      B3DCLAMP(ival, -32768, 32767);
      sdata[i] = (b3dInt16)ival;
    }
    break;

    /* Unsigned shorts */
  case MRC_MODE_USHORT:
    for (i = 0; i < nx; i++) {
      ival = (int)(fbufp[i] + 0.5f);
      B3DCLAMP(ival, 0, 65535);
      usdata[i] = (b3dUInt16)ival;
    }
    break;
  }
}

/*!
 * Copies the parameters with loading information from [iiFile] to [iiSave]: {ll} and
 * {ur} members for x, y, and z, {axis}, {padLeft}, {padRight}, {slope}, and {offset}.
 */
void iiSaveLoadParams(ImodImageFile *iiFile, ImodImageFile *iiSave)
{
  iiSave->llx = iiFile->llx;
  iiSave->urx = iiFile->urx;
  iiSave->lly = iiFile->lly;
  iiSave->ury = iiFile->ury;
  iiSave->llz = iiFile->llz;
  iiSave->urz = iiFile->urz;
  iiSave->axis = iiFile->axis;
  iiSave->padLeft = iiFile->padLeft;
  iiSave->padRight = iiFile->padRight;
  iiSave->slope = iiFile->slope;
  iiSave->offset = iiFile->offset;
}

/*!
 * Restores the parameters copied by @iiSaveLoadParams from [iiSave] back to [iiFile] and
 * returns the value in [retVal] for convenience.
 */
int iiRestoreLoadParams(int retVal, ImodImageFile *iiFile, ImodImageFile *iiSave)
{
  iiFile->llx = iiSave->llx;
  iiFile->urx = iiSave->urx;
  iiFile->lly = iiSave->lly;
  iiFile->ury = iiSave->ury;
  iiFile->llz = iiSave->llz;
  iiFile->urz = iiSave->urz;
  iiFile->axis = iiSave->axis;
  iiFile->padLeft = iiSave->padLeft;
  iiFile->padRight = iiSave->padRight;
  iiFile->slope = iiSave->slope;
  iiFile->offset = iiSave->offset;
  return retVal;
}
