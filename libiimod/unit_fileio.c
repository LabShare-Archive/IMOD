/******************************************************************************
 *  unit_fileio.c - the basic input/output routines for unit-based image file access
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  Adapted from the blockio.c module originally from Baylor
 *
 *  $Id$
 *****************************************************************************/
 
/******************************************************************************
Include Files
******************************************************************************/
#include <stdio.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <errno.h>

#include "b3dutil.h"
#include "iimage.h"
#include "iiunit.h"
#include "ilist.h"

/******************************************************************************
Private Defines
******************************************************************************/

#ifdef F77FUNCAP

#define move       MOVE
#define zero       ZERO
#define iiuopen IIUOPEN
#define imclose IMCLOSE
#define iiuclose IIUCLOSE
#define imposn IMPOSN
#define iiusetposition IIUSETPOSITION
#define iiureadsection IIUREADSECTION
#define iiureadsecpart IIUREADSECPART
#define iiureadlines IIUREADLINES
#define iiuwritesection IIUWRITESECTION
#define iwrsec IWRSEC
#define iwrsecl IWRSECL
#define iiuwritesubarray IIUWRITESUBARRAY
#define iiuwritelines IIUWRITELINES
#define iwrlin IWRLIN
#define iiufileinfo IIUFILEINFO
#define iiualtconvert IIUALTCONVERT

#else

#define move       move_
#define zero       zero_
#define iiuopen iiuopen_
#define imclose imclose_
#define iiuclose iiuclose_
#define imposn imposn_
#define iiusetposition iiusetposition_
#define iiureadsection iiureadsection_
#define iiureadsecpart iiureadsecpart_
#define iiureadlines iiureadlines_
#define iiuwritesection iiuwritesection_
#define iwrsec iwrsec_
#define iwrsecl iwrsecl_
#define iiuwritesubarray iiuwritesubarray_
#define iiuwritelines iiuwritelines_
#define iwrlin iwrlin_
#define iiufileinfo iiufileinfo_
#define iiualtconvert iiualtconvert_

#endif

#define MAX_MODE 17
#define MAX_UNIT 1000

/* JRK: added these for the new attribute in Unit struct. */
#define UNIT_ATBUT_RO      1
#define UNIT_ATBUT_NEW     2
#define UNIT_ATBUT_OLD     3
#define UNIT_ATBUT_SCRATCH 4

/******************************************************************************
Private Data Structures
******************************************************************************/

typedef struct
{
  ImodImageFile *iiFile;
  MrcHeader *header;
  int  currentSec;
  int  currentLine;
  char *tailName;  /* Pointer to filename only in fname */
  int  attribute;  /* JRK: keep track of attributes for files. */
  unsigned char beingUsed;
  unsigned char readOnly;
  unsigned char noConvert; /* Temporary flag filled in whenever unit is looked up */
} Unit;
 
/******************************************************************************
Private Function Prototypes
******************************************************************************/

static Unit *findNewUnit(int iunit);
static void mybcopy(register char *a, register char *b, register int n);
static Unit *lookupUnit(int unit, const char *function, int doExit, int checkRW);
static void *exitOrNULL(int doExit);
static void addUnitToList(Ilist **list, int unit);
static void removeUnitFromList(Ilist *list, int unit);
static int isUnitOnList(Ilist *list, int unit);
static int setupCurrentLines(Unit *u, int numLines);
 
/******************************************************************************
 Static Declarations
******************************************************************************/

static Ilist *sUnitList;
static int *sUnitMap = NULL;
static int sMapSize = 0;
static Ilist *sNoConvList = NULL;
static int sBriefHeader = -1;
static int sPrintHeader = 1;
static int sExitOnError = 1;
static int sStoreError = -1;

#define EXIT_OR_RETURN(a) {if (sExitOnError) exit(1) ; else return(a);}

/******************************************************************************
 ***************************    PUBLIC FUNCTIONS    ****************************
 ******************************************************************************/

/*!
 * Opens a file on unit number [iunit], between 1 and 1000, with filename [name] and
 * [attribute] specified as 'NEW', 'OLD', 'RO' (for read-only), and 'SCRATCH' (deleted 
 * when closed).  Only first letter of [attribute] is checked and it may be either case.
 * If the unit is already in use a warning is printed and the existing file is closed.
 * Fortran wrapper iiuOpen.
 */
int iiuOpen(int iunit, const char *name, const char *attribute)
{

  Unit *u;  
  int mode, errSave;
  const char *modes[4] = {"rb", "rb+", "wb", "wb+"};
  char *tailback;

  u = findNewUnit(iunit);
  iiuMemoryError(u, "ERROR: iiuOpen - Allocating new unit");

  u->beingUsed = 1;
  u->readOnly = 0;
  u->currentSec = 0;
  u->currentLine = 0;

  /* Read-only (RO) file  (believe it or not, the tests were only on the first letter) */
  if (attribute[0] == 'R' || attribute[0] == 'r') {
    mode = 0;
    u->attribute = UNIT_ATBUT_RO;
    u->readOnly = 1;
  }

  /* New file - manage backup */
  if (attribute[0] == 'N' || attribute[0] == 'n') {
    if (!getenv("IMOD_NO_IMAGE_BACKUP")) {
      errno = 0;
      if (imodBackupFile(name)) {
        errSave = errno;
        fprintf(stdout, "\nWARNING: iiuOpen - Could not rename '%s' to '%s~'\n", name, 
                name);
        if (errSave)
          fprintf(stdout, "WARNING: from system - %s\n", strerror(errSave));
      }
    }
    mode = 3;
    u->attribute = UNIT_ATBUT_NEW;
  }

  /* Old and scratch files */
  if (attribute[0] == 'O' || attribute[0] == 'o') {
    mode = 1;
    u->attribute = UNIT_ATBUT_OLD;
  }
  if (attribute[0] == 'S' || attribute[0] == 's') {
    mode = 3;
    u->attribute = UNIT_ATBUT_SCRATCH;
  }

  if (mode == 3) {

    /* Open a new file in default output format */
    u->iiFile = iiOpenNew(name, modes[mode], IIFILE_DEFAULT);
    if (!u->iiFile) {
      b3dError(stdout, "\nERROR: iiuOpen - Opening new output file\n");
      EXIT_OR_RETURN(1);
    }

  } else {

    /* Open an existing file */
    u->iiFile = iiOpen(name, modes[mode]);
    if (!u->iiFile) {
      b3dError(stdout, "\nERROR: iiuOpen - Could not open '%s'\n" , name);
      EXIT_OR_RETURN(1);
    }
    if (!(u->iiFile->writeSection && u->iiFile->writeSectionFloat) && !u->readOnly) {
      b3dError(stdout, "\nERROR: iiuOpen - Non-MRC-type file '%s' with no write function "
              "must be opened read-only\n" , name);
      EXIT_OR_RETURN(1);
    }

    /* For TIFF file, create an MRC header and populate it with defaults */
    if (u->iiFile->file == IIFILE_TIFF) {
      if (u->iiFile->mode < 0) {
        b3dError(stdout, "\nERROR: iiuOpen - TIFF file '%s' has a data type that "
                 "is not supported\n", name);
        EXIT_OR_RETURN(1);
      }
    }
  }

  /* Old or new files */
  /* For a non-MRC file type, create an MRC header and populate it with defaults */
  if (u->iiFile->file != IIFILE_MRC && u->iiFile->file != IIFILE_RAW) {
    u->header = B3DMALLOC(MrcHeader, 1);
    iiuMemoryError(u->header, "ERROR: iiuOpen - Allocating MRC header");
    if (iiFillMrcHeader(u->iiFile, u->header)) {
      b3dError(stdout, "\nERROR: iiuOpen - file '%s' is not a format that provides "
               "an MRC-like header and cannot be read\n", name);
      EXIT_OR_RETURN(1);
    }
  } else {
    
    /* Otherwise copy header pointer */
    u->header = (MrcHeader *)u->iiFile->header;
  }
  
  /* Get the tail of the filename for other error messages */
  u->tailName = strrchr(u->iiFile->filename, '/');
  tailback = strrchr(u->iiFile->filename, '\\');
  if (tailback > u->tailName)
    u->tailName = tailback;
  if (!u->tailName)
    u->tailName = &u->iiFile->filename[0];
  else
    u->tailName++;
  return 0;
}

int iiuopen(int *iunit, char *name, char *attribute, int name_l, int attr_l)
{
  char *cname = f2cString(name, name_l);
  char *cattr = f2cString(attribute, attr_l);
  int err;
  if (!cname || !cattr)
    iiuMemoryError(NULL, "ERROR: iiuopen - Allocating C strings");
  err = iiuOpen(*iunit, cname, cattr);
  free(cname);
  free(cattr);
  return err;
}

/*!
 * Closes the file on unit [iunit].  It ignores a unit number out of range or unit not 
 * open.  Fortran wrapper iiuClose and imclose
 */
void iiuClose(int iunit)
{
  Unit *u; 
  int unit = iunit - 1;
  removeUnitFromList(sNoConvList, unit);
  if (unit >= 0 && unit < sMapSize && sUnitMap[unit] >= 0) {
    u = (Unit *)ilistItem(sUnitList, sUnitMap[unit]);

    if (u->beingUsed) {
      u->beingUsed = 0;

      /* Delete scratch files, Use remove to avoid unistd.h */
      if (u->attribute == UNIT_ATBUT_SCRATCH)
        remove(u->iiFile->filename);

      if (u->iiFile->file == IIFILE_TIFF)
        free(u->header);
      iiDelete(u->iiFile);
    }
    sUnitMap[unit] = -1;
  }
}

void imclose(int *iunit) {iiuClose(*iunit);}
void iiuclose(int *iunit) {iiuClose(*iunit);}

/*!
 * Sets the position for the next read or write on unit [iunit] to the Z value in
 * [section] and the Y value in [line], both numbered from zero.  Fortran wrappers
 * iiuSetPosition and imposn.
 */
void iiuSetPosition(int iunit, int section, int line)
{
  Unit *u = lookupUnit(iunit, "iiuSetPosition", 1, 0);
  u->currentSec = section;
  u->currentLine = line;
}

void imposn(int *iunit, int *section, int *line){iiuSetPosition(*iunit, *section, *line);}
void iiusetposition(int *iunit, int *section, int *line)
{ iiuSetPosition(*iunit, *section, *line);}

/*!
 * Reads the current section from unit [iunit] into [array].  Returns the error from the
 * reading operation regardless of the setting of whether to exit on error.  Fortran 
 * wrapper iiuReadSection.
 */
int iiuReadSection(int iunit, char *array)
{
  Unit *u = lookupUnit(iunit, "iiuReadSection", 0, 1);
  if (!u)
    return -1;
  return iiuReadSecPart(iunit, array, u->iiFile->nx, 0, u->iiFile->nx - 1, 0, 
                        u->iiFile->ny - 1);
}

int iiureadsection(int *iunit, char *array)
{ return iiuReadSection(*iunit, array);}

/*!
 * Reads part of the current section from unit [iunit] into [array].  The portion of the 
 * section from [indX0] to [indX1], inclusive, in X, and from [indY0] to [indY1] in Y will
 * be read, and the current position will be advanced to the start of the next section.
 * The X dimension of [array] is supplied [nxdim]; if this is not equal to the extent 
 * being read, data will be placed into the left part of the array and padded on the 
 * right.  Returns the error from the reading operation regardless of the setting of 
 * whether to exit on error.  Fortran wrapper iiuReadSecPart.
 */
int iiuReadSecPart(int iunit, char *array, int nxdim, int indX0, int indX1,
                   int indY0, int indY1)
{
  int err;
  Unit *u = lookupUnit(iunit, "iiuReadSecPart", 0, 1);
  if (!u)
    return -1;
  u->iiFile->llx = indX0;
  u->iiFile->urx = indX1;
  u->iiFile->lly = indY0;
  u->iiFile->ury = indY1;
  u->iiFile->padLeft = 0;
  u->iiFile->padRight = nxdim - (indX1 + 1 - indX0);
  if (u->noConvert)
    err = iiReadSection(u->iiFile, array, u->currentSec);
  else
    err = iiReadSectionFloat(u->iiFile, array, u->currentSec);
  u->currentSec++;
  u->currentLine = 0;
  return err;
}

int iiureadsecpart(int *iunit, char *array, int *nxdim, int *indX0, int *indX1,
                   int *indY0, int *indY1)
{ return iiuReadSecPart(*iunit, array, *nxdim, *indX0, *indX1, *indY0, *indY1);}

/*!
 * Reads [numLines] lines from unit [iunit] into [array], starting at the current 
 * position.  The current position is advanced to the line after the last one read.
 * Returns the error from the reading operation regardless of the setting of
 * whether to exit on error.  Fortran wrapper iiuReadLines.
 */
int iiuReadLines(int iunit, char *array, int numLines)
{
  int err, iz;
  Unit *u = lookupUnit(iunit, "iiuReadLines", 0, 1);
  if (!u)
    return -1;
  iz = u->currentSec;
  if (setupCurrentLines(u, numLines))
    return -2;
  if (u->noConvert)
    err = iiReadSection(u->iiFile, array, iz);
  else
    err = iiReadSectionFloat(u->iiFile, array, iz);
  return err;
}

int iiureadlines(int *iunit, char *array, int *numLines)
{ return iiuReadLines(*iunit, array, *numLines);}

/*!
 * Writes the data in [array] to the current section of unit [iunit].  If not exiting on
 * error, returns negative values for internal errors and positive values for an
 * error from the writing operation.  Fortran wrappers iiuWriteSection and iwrsec 
 * (a void).
 */
int iiuWriteSection(int iunit, char *array)
{
  Unit *u = lookupUnit(iunit, "iiuWriteSection", sExitOnError, 2);
  int err;
  if (!u)
    return -1;
  err = iiuWriteSubarray(iunit, array, u->iiFile->nx, 0, 0, u->iiFile->ny - 1);
  if (err) {
    b3dError(stdout, "\nERROR: iiuWriteSection - writing section %d to unit %d\n",
           u->currentSec - 1, iunit);
    EXIT_OR_RETURN(err);
  }
  return err;
}

int iiuwritesection(int *iunit, char *array) {return iiuWriteSection(*iunit, array);}
void iwrsec(int *iunit, char *array) {iiuWriteSection(*iunit, array);}


/*!
 * Writes data from a subset of [array] to the current section and line of unit [iunit].
 * The X dimension of [array] is given in [nxdim], the starting index from which to take 
 * data in X in [ixStart], and the starting and ending indices from which to take data in
 * Y in [iyStart] and [iyEnd] (inclusive, numbered from 0).  The current position is
 * advanced to the first line of the next section.  If not exiting on
 * error, returns negative values for internal errors and positive values for an
 * error from the writing operation.  Fortran wrapper iiuWriteSubarray.
 */
int iiuWriteSubarray(int iunit, char *array, int nxdim, int ixStart, 
                     int iyStart, int iyEnd)
{
  int err;
  Unit *u = lookupUnit(iunit, "iiuWriteSubarray", sExitOnError, 2);
  if (!u)
    return -1;

  /* Have to sync header for TIFF file because the write is the time when it matters */
  if (u->iiFile->file == IIFILE_TIFF)
    iiSyncFromMrcHeader(u->iiFile, u->header);
  u->iiFile->llx = 0;
  u->iiFile->urx = u->iiFile->nx - 1;
  u->iiFile->lly = u->currentLine;
  u->iiFile->ury = u->currentLine + iyEnd - iyStart;
  u->iiFile->padLeft = ixStart;
  u->iiFile->padRight = nxdim - u->iiFile->nx - ixStart;
  if (u->iiFile->padRight < 0)
    return -2;
  if (u->iiFile->ury >= u->iiFile->ny)
    return -3;
  if (u->noConvert)
    err = iiWriteSection(u->iiFile, array, u->currentSec);
  else
    err = iiWriteSectionFloat(u->iiFile, array, u->currentSec);
  u->currentSec++;
  u->currentLine = 0;
  return err;
}

int iiuwritesubarray(int *iunit, char *array, int *nxdim, int *ixStart,
                     int *iyStart, int *iyEnd)
{ return iiuWriteSubarray(*iunit, array, *nxdim, *ixStart, *iyStart, *iyEnd);}

/*!
 * Writes [numLines] lines tounit [iunit] from [array], starting at the current 
 * position.  The current position is advanced to the line after the last one written.
 * If not exiting on error, returns negative values for internal errors and positive 
 * values for an error from the writing operation.  Fortran wrappers iiuWriteLines,
 * irwsecl (a void), and irwlin(int *iunit, char *array), a void to write one line.
 */
int iiuWriteLines(int iunit, char *array, int numLines)
{
  int err, iz;
  Unit *u = lookupUnit(iunit, "iiuWriteLine", sExitOnError, 2);
  if (!u)
    return -1;
  if (u->iiFile->file == IIFILE_TIFF)
    iiSyncFromMrcHeader(u->iiFile, u->header);
  iz = u->currentSec;
  if (setupCurrentLines(u, numLines))
    EXIT_OR_RETURN(-3);
  if (u->noConvert)
    err = iiWriteSection(u->iiFile, array, iz);
  else
    err = iiWriteSectionFloat(u->iiFile, array, iz);
  if (err) {
    b3dError(stdout, "\nERROR: iiuWriteLines - writing lines to unit %d.\n", iunit);
    EXIT_OR_RETURN(1);
  }
  return err;
}

int iiuwritelines(int *iunit, char *array, int *numLines)
{ return iiuWriteLines(*iunit, array, *numLines);}
void iwrlin(int *iunit, char *array) {iiuWriteLines(*iunit, array, 1);}
void iwrsecl(int *iunit, char *array, int *numLines) 
{ iiuWriteLines(*iunit, array, *numLines);}

/*
 * Does common tasks for reading or writing a specified number of lines
 */
static int setupCurrentLines(Unit *u, int numLines)
{
  u->iiFile->llx = 0;
  u->iiFile->urx = u->iiFile->nx - 1;
  u->iiFile->lly = u->currentLine;
  u->iiFile->ury = u->currentLine + numLines - 1;
  if (u->iiFile->ury >= u->iiFile->ny) {
    b3dError(stdout, "\nERROR: iiuRead/WriteLines - lines go past end of current "
             "section.\n");
    return(1);
  }
  u->iiFile->padLeft = u->iiFile->padRight = 0;
  u->currentLine += numLines;
  if (u->currentLine == u->iiFile->ny) {
    u->currentSec++;
    u->currentLine = 0;
  }
  return 0;
}

/*!
 * Returns information for unit [iunit]: the file size in KB in [fileSize]; the type of
 * file in [fileType], which can be 1 for TIFF, 2 for MRC, or 4 for other raw-type files;
 * and flags in [flags]: ^
 * bit 0 - bytes are swapped  ^
 * bit 1 - bytes are stored signed  ^
 * bit 2 - old-style header with no MAP and origin wrong place  ^
 * bit 3 - Extra header nint value was corrected to 0  ^
 * bit 4 - map values were bad and were corrected  ^
 * If the unit is not open, [fileSize] is returned with -1.  Fortran wrapper iiuFileInfo.
 */
void iiuFileInfo(int iunit, int *fileSize, int *fileType, int *flags)
{
#ifdef _WIN32
  struct _stat64 buf;
#else
  struct stat buf;
#endif
  Unit *u = lookupUnit(iunit, "iiuFileSize", 0, 0);
  *flags = 0;
  *fileType = IIFILE_MRC;
  *fileSize = -1;
  if (!u)
    return;

  /* generic stat is not good enough on Windows */
#ifdef _WIN32
  _stat64(u->iiFile->filename, &buf);
#else
  stat(u->iiFile->filename, &buf);
#endif
  *fileSize = (int)((double)buf.st_size / 1024.);
  *fileType = u->iiFile->file;
  *flags = u->header->iiuFlags | (u->header->swapped ? IIUNIT_SWAPPED : 0) |
    (u->header->bytesSigned ? IIUNIT_BYTES_SIGNED : 0);
}

void iiufileinfo(int *iunit, int *fileSize, int *fileType, int *flags)
{ iiuFileInfo(*iunit, fileSize, fileType, flags);}

/*!
 * The value in [doExit] sets whether to exit on error when calling most functions with 
 * an error return; the default is 1 and a value of 0 allows the return.  The value in
 * [storeError] will be sent to b3dSetStoreError upon initialization; the default is
 * -1 for printing to stdout, and a value of 1 would allow error messages to be retrieved
 * instead.  Fortran wrapper iiuExitOnError.
 */
void iiuExitOnError(int doExit, int storeError)
{
  sExitOnError = doExit;
  sStoreError = storeError;
}

void iiuexitonerror(int *doExit, int *storeError) {iiuExitOnError(*doExit, *storeError);}

/* Function for header module to discover value */
int iiuGetExitOnError()
{
  return sExitOnError;
}

/*! 
 * Sets whether brief headers should be written, overriding the value of environment
 * variable IMOD_BRIEF_HEADER.  A [val] greater than 0 gives brief output, a value of 0
 * gives full output.  Callable with this name from C or Fortran.
 */
void ialBrief(int *val) 
{
  sBriefHeader = *val;
}

/*!
 * Returns 1 if brief header should be written, 0 otherwise.  Callable with this name 
 * from C or Fortran.
 */
int iiuRetBrief()
{
  if (sBriefHeader >= 0)
    return sBriefHeader;
  return (getenv("IMOD_BRIEF_HEADER") != NULL ? 1 : 0);
}

/*!
 * Sets whether header printing is on if [val] is non-zero. Callable with this name 
 * from C or Fortran.
 */
void iiuAltPrint(int *val) 
{
  sPrintHeader = *val;
}

/*!
 * Returns a non-zero value if header printing is on.  Callable with this name 
 * from C or Fortran. 
 */
int iiuRetPrint()
{
  return sPrintHeader;
}

/*!
 * Sets the option for conversion to/from floats on input and output for unit [iunit];
 * a [val] of 0 turns off conversion. Fortran wrapper iiualtconvert.
 */
void iiuAltConvert(int iunit, int val)
{
  removeUnitFromList(sNoConvList, iunit);
  if (val == 0)
    addUnitToList(&sNoConvList, iunit);
}

void iiualtconvert(int *iunit, int *val) {iiuAltConvert(*iunit, *val);}

/*!
 * Returns the MRC header for unit [iunit].  If this is not an open unit, it prints 
 * a message including the text in [function] and exits if [doExit] is non-zero.  If 
 * checkRW is 1, it tests whether reading is not possible given the current setting
 * of the conversion option; if checkRW is 2 is tests whether writing is possible; in
 * either case it exits or returns depending on the value of [doExit].
 */
MrcHeader *iiuMrcHeader(int iunit, const char *function, int doExit, int checkRW)
{
  Unit *u = lookupUnit(iunit, function, doExit, checkRW);
  if (!u)
    return NULL;
  return u->header;
}

/*!
 * Copies image size from the MRC header to the ImodImageFile structure and sets the 
 * file pointer from the latter to the MRC header.  This should be called after changing 
 * the image size or doing a structure copy to the MRC header.
 */
void iiuSyncWithMrcHeader(int iunit)
{
  Unit *u = lookupUnit(iunit, "iiuSyncWithMrcHeader", 1, 0);
  iiSyncFromMrcHeader(u->iiFile, u->header);
}

/*!
 * Returns the type of file open on unit [iunit]: IIFILE_MRC (2), or IIFILE_TIFF (1).
 */
int iiuFileType(int iunit)
{
  Unit *u = lookupUnit(iunit, "iiuFileType", 1, 0);
  return u->iiFile->file;
}

void move(char *a, char *b, int *n)
{
  mybcopy(a, b, *n);
}
 
void zero(char *a, int *n)
{
  memset(a, 0, *n);
}

void iiuMemoryError(void *ptr, const char *message)
{
  if (ptr != NULL)
    return;
  fprintf(stdout, "\n%s\n", message);
  exit(1);
}
 
/************************************************************************
 ***************************    static FUNCTIONS    ********************
 ************************************************************************/
 
static Unit *findNewUnit(int iunit)
{
  Unit newUnit;
  Unit *u;
  int i, newSize;

  if (!sMapSize)
    b3dSetStoreError(sStoreError);
  if (iunit <= 0 || iunit > MAX_UNIT) {
    b3dError(stdout, "ERROR: iiuOpen - A unit number of %d is out of range\n", iunit);
    exit(1);
  }

  /* Maintain the map from unit numbers to the unitList */
  if (iunit > sMapSize) {
    newSize = iunit + 9;
    if (!sMapSize)
      sUnitMap = B3DMALLOC(int, newSize);
    else
      B3DREALLOC(sUnitMap, int, newSize);
    if (!sUnitMap)
      return NULL;
    for (i = sMapSize; i < newSize; i++)
      sUnitMap[i] = -1;
    sMapSize = newSize;
  }
  if (sUnitMap[iunit - 1] >= 0) {
    b3dError(stdout, "WARNING: iiuOpen - Unit number %d is already in use; closing it\n",
             iunit);
    iiuClose(iunit);
  }

  /* Start the list of units if needed */
  if (!sUnitList)
    sUnitList = ilistNew(sizeof(Unit), 4);
  if (!sUnitList)
    return NULL;

  /* Find free unit if any */
  for (i = 0; i < ilistSize(sUnitList); i++) {
    u = (Unit *)ilistItem(sUnitList, i);
    if (!u->beingUsed) {
      sUnitMap[iunit - 1] = i;
      return u;
    }
  }

  /* Or add a new unit */
  newUnit.beingUsed = 0;
  if (ilistAppend(sUnitList, &newUnit))
    return NULL;
  sUnitMap[iunit - 1] = ilistSize(sUnitList) - 1;
  return (Unit *)ilistItem(sUnitList, ilistSize(sUnitList) - 1);
}

static void mybcopy(register char *a, register char *b, register int n)
{
  while (n--) {
    *a++ = *b++;
  }
}

/* Checks for legal unit number and whether unit is open, gives error
 * message with function name and unit number, exits if doExit set; if checkRW is 1 or
 * it checks for validity of reading or writing */
static Unit *lookupUnit(int unit, const char *function, int doExit, int checkRW)
{
  Unit *u;
  if (unit <= 0 || unit > MAX_UNIT) {
    b3dError(stdout, "\nERROR: %s - %d is not a legal unit number.\n", function,
            unit);
    return exitOrNULL(doExit);
  }
  if (unit <= sMapSize && sUnitMap[unit - 1] >= 0) {
    u = (Unit *)ilistItem(sUnitList, sUnitMap[unit - 1]);
    u->noConvert = isUnitOnList(sNoConvList, unit);
    if (u->iiFile->format == IIFORMAT_COMPLEX)
      u->noConvert = 1;
    if (u->beingUsed) {
      if (checkRW > 1 && u->readOnly) {
        b3dError(stdout, "\nERROR: %s - Trying to write to unit %d, which was opened "
                "read-only.\n", function, unit);
        return exitOrNULL(doExit);
      }   
      if (checkRW == 1 && ((u->noConvert && !u->iiFile->readSection) || 
                           (!u->noConvert && !u->iiFile->readSectionFloat))) {
        b3dError(stdout, "\nERROR: %s - There is no function for reading %s from the "
                "type of file on unit %d.\n", function, 
                u->noConvert ? "raw data" : "floats", unit);
        return exitOrNULL(doExit);
      }   
      if (checkRW > 1 && ((u->noConvert && !u->iiFile->writeSection) || 
                           (!u->noConvert && !u->iiFile->writeSectionFloat))) {
        b3dError(stdout, "\nERROR: %s - There is no function for writing %s to the "
                "type of file on unit %d.\n", function, 
                u->noConvert ? "raw data" : "floats", unit);
        return exitOrNULL(doExit);
      }   
      return u;
    }
  }
  b3dError(stdout, "\nERROR: %s - unit %d is not open.\n", function, unit);
  return exitOrNULL(doExit);
}

static void *exitOrNULL(int doExit)
{
  if (doExit)
    exit(3);
  return NULL;
}

/* Adds a unit to a list of units */
static void addUnitToList(Ilist **list, int unit)
{
  b3dInt16 val = unit;
  if (!*list)
    *list = ilistNew(sizeof(b3dInt16), 4);
  iiuMemoryError(*list, "Allocating a unit list");
  if (ilistAppend(*list, &val))
    iiuMemoryError(NULL, "Appending to a unit list");
}

/* Removes a unit from a list of units */
static void removeUnitFromList(Ilist *list, int unit)
{
  b3dInt16 *valp;
  int i;
  for (i = 0; i < ilistSize(list); i++) {
    valp = (b3dInt16 *)ilistItem(list, i);
    if (*valp == unit) {
      ilistRemove(list, i);
      return;
    }
  }
}

/* Returns 1 if a unit is on the list */
static int isUnitOnList(Ilist *list, int unit)
{
  b3dInt16 *valp;
  int i;
  for (i = 0; i < ilistSize(list); i++) {
    valp = (b3dInt16 *)ilistItem(list, i);
    if (*valp == unit)
      return 1;
  }
  return 0;
}

