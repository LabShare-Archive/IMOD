/*   b3dutil.c   - utility functions for getting version and copyright, 
 *                   trimming program name, system dependent I/O, min/max, etc.
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *
 *   $Id$
 *   Log at end
 */                                                                           

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "imodconfig.h"
#include "b3dutil.h"

#ifdef WIN32_BIGFILE
#include <io.h>
#define fileno _fileno
#define read _read
#define write _write
#define lseek _lseeki64
#define off_t __int64
#endif

#ifdef MAC103_BIGFILE
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#endif

#ifdef F77FUNCAP
#define imodbackupfile IMODBACKUPFILE
#define imodgetenv IMODGETENV
#define b3dheaderitembytes B3DHEADERITEMBYTES
#else
#define imodbackupfile imodbackupfile_
#define imodgetenv imodgetenv_
#define b3dheaderitembytes b3dheaderitembytes_
#endif

/* DNM 2/26/03: These need to be printf instead of fprintf(stderr) to not
   crash imod under Windows */
/*! Prints program name provided in [pname], IMOD version and compilation 
  date */
int imodVersion(char *pname)
{
  if (pname)
    printf("%s Version %s %s %s\n",
           pname, VERSION_NAME, __DATE__, __TIME__);
  return(VERSION);
}

/*! Prints copyright notice */
void imodCopyright(void)
{
  char *uofc =   "the Regents of the University of Colorado";
  printf("Copyright (C) %s by %s\n%s & %s\n", COPYRIGHT_YEARS,
         LAB_NAME1, LAB_NAME2, uofc);
  return;
}

/*! Calls imodVersion and imodCopyright */
void imodUsageHeader(char *pname)
{
  imodVersion(pname);
  imodCopyright();
}

/*! Returns a program name stripped of directories and .exe,
 * given the full name (argv\[0\]) in [fullname].
 * It returns a pointer internal to argv\[0\], unless the name ends in .exe,
 * in which case it tries to return a duplicate copy.  Do not free it. */
char *imodProgName(char *fullname)
{
  char *tail, *tailback, *exe;
  int indexe;
  tail = strrchr(fullname, '/');
  tailback = strrchr(fullname, '\\');
  if (tailback > tail)
    tail = tailback;

  if (!tail)
    return fullname;
  tail++;
  exe = strstr(tail, ".exe");
  indexe = strlen(tail) - 4;
  if (!exe || exe != tail + indexe)
    return tail;
  exe = strdup(tail);
  if (!exe)
    return tail;
  exe[indexe] = 0x00;
  return exe;
}


/*! Renames an existing file named [filename] to filename~ and
   deletes filename~ first if necessary */
int imodBackupFile(char *filename)
{
  struct stat buf;
  int err;
  char *backname;

  /* If file does not exist, return */
  if (stat(filename, &buf))
    return 0;

  /* Get backup name */
  backname = (char *)malloc(strlen(filename) + 3);
  if (!backname)
    return -2;
  sprintf(backname, "%s~", filename);

  /* If the backup file exists, try to remove it first (Windows/Intel) */
  if (!stat(backname, &buf) && remove(backname)) {
    free(backname);
    return -1;
  }

  /* finally, rename file */
  err = rename(filename, backname);
  free(backname);
  return err;
}

/*! A fortran wrapper for imodBackupFile */

int imodbackupfile(char *filename, int strlen)
{
  int err;
  char *cstr = f2cString(filename, strlen);
  if (!cstr)
    return -2;
  err = imodBackupFile(cstr);
  free(cstr);
  return err;
}

/*! A Fortran-callable routine to get an environment variable, [var] and return
  its value in [value].  Returns -1 for error or 1 if the variable is not 
  defined. */
int imodgetenv(char *var, char *value, int varSize, int valueSize)
{
  char *valPtr;
  char *cstr = f2cString(var, varSize);
  if (!cstr)
    return -1;
  valPtr = getenv(cstr);
  free(cstr);
  if (!valPtr)
    return 1;
  return c2fString(valPtr, value, valueSize);
}


/*! Creates a C string with a copy of a Fortran string described by [str] and 
  [strsize], using [malloc]. */
char *f2cString(char *str, int strSize)
{
  int i;
  char *newStr;

  /* find last non-blank character */
  for (i = strSize - 1; i >= 0; i--)
    if (str[i] != ' ')
      break;

  newStr = (char *)malloc(i + 2);
  if (!newStr) {
    return NULL;
  }

  /* copy string if non-null, then put terminator at end */
  if (i >= 0)
    strncpy(newStr, str, i + 1);
  newStr[i + 1] = 0x00;
  return newStr;
}

/*! Converts a C string in [cStr] into a Fortran string [fStr] with size
  [fSize]; returns -1 for error if the string will not fit. */
int c2fString(char *cStr, char *fStr, int fSize)
{
  while (*cStr && fSize > 0) {
    *fStr++ = *cStr++;
    fSize--;
  }

  /* Return error if there is still a non-null character */
  if (*cStr)
    return -1;

  /* Blank-pad */
  while (fSize > 0) {
    *fStr++ = ' ';
    fSize--;
  }
  return 0;
}

/* Simple error processing routines to avoid having libraries print error
   messages themselves */
static int storeError = 0;
static char errorMess[MAX_IMOD_ERROR_STRING] = "";

/*! Stores an error message and may print it as well.  The message is 
  internally printed with vsprintf.  It is printed to [fout] unless 
  b3dSetStoreError has been called with 1. */
void b3dError(FILE *out, char *format, ...)
{
  va_list args;
  va_start(args, format);
  
  vsprintf(errorMess, format, args);
  if (out && !storeError)
    fprintf(out, errorMess);
  va_end(args);
}

/*! Sets flag to print messages passed by [b3dError] if [ival] is 0 (the 
  default) or to store them internally if [ival] is 1. */
void b3dSetStoreError(int ival)
{
  storeError = ival;
}

/*! Returns the current error string */
char *b3dGetError()
{
  return &errorMess[0];
}

/* These routines will simply call the standard C routine under Unix, otherwise
   they will get the file descriptor and call the descriptor-based routine, or 
   its underlying equivalent in Window. */

/*! A substitute for fseek that works for large files on all systems. */
int b3dFseek(FILE *fp, int offset, int flag)
{
#if defined(WIN32_BIGFILE) || defined(MAC103_BIGFILE)
  int handle;
  off_t err;
  if (fp == stdin)
    return 0;
  handle = fileno(fp);
  err = lseek(handle, (off_t)offset, flag);
  return (err == -1 ? -1 : 0);
#else
  if (fp == stdin)
    return 0;
  return fseek(fp, offset, flag);
#endif
}

/*! A substitute for fread that works for large files on all systems. */
size_t b3dFread(void *buf, size_t size, size_t count, FILE *fp)
{
#if defined(WIN32_BIGFILE) || defined(MAC103_BIGFILE)
  int handle = fileno(fp);
  return (size_t)(read(handle, buf, size * count) / size);
#else
  return fread(buf, size, count, fp);
#endif
}
 
/*! A substitute for fwrite that works for large files on all systems. */
size_t b3dFwrite(void *buf, size_t size, size_t count, FILE *fp)
{
#if defined(WIN32_BIGFILE) || defined(MAC103_BIGFILE)
  int handle = fileno(fp);
  return (size_t)(write(handle, buf, size * count) / size);
#else
  return fwrite(buf, size, count, fp);
#endif
}

/*! A substitute for rewind that works for large files on all systems. */
void b3dRewind(FILE *fp)
{
  b3dFseek(fp, 0, SEEK_SET);
}

#define SEEK_LIMIT 2000000000

/*!
 * Does a seek in a large file with pointer [fp].  The amount to seek is
 * given by [base] + [size1] * [size2].  [flag] has the standard meaning for
 * seeks, e.g., SEEK_SET, etc.  Returns the nonzero seek error if error.
 */
int mrc_big_seek(FILE *fp, int base, int size1, int size2, int flag)
{
  int smaller, bigger, ntodo, ndo, abs1, abs2;
  int steplimit, err;

  /* Do the base seek if it is non-zero, or if the rest of the seek is
     zero and we are doing a SEEK_SET */
  if (base || ((!size1 || !size2) && (flag == SEEK_SET))) {
    if ((err = b3dFseek(fp, base, flag)))
      return err;
    flag = SEEK_CUR;
  }

  if (!size1 || !size2)
    return 0;

  /* Find smaller and larger size */
  abs1 = size1 >= 0 ? size1 : -size1;
  abs2 = size2 >= 0 ? size2 : -size2;
  smaller = abs1 < abs2 ? abs1 : abs2;
  bigger = abs1 < abs2 ? abs2 : abs1;

  /* Step by multiples of the larger size, but not by more than the limit */
  steplimit = SEEK_LIMIT / bigger;
  ntodo = smaller;

  /* If one of the size entries is negative, negate the steps */
  if ((size1 < 0 && size2 >= 0) || (size1 >= 0 && size2 < 0))
    bigger = -bigger;

  while (ntodo > 0) {
    ndo = ntodo <= steplimit ? ntodo : steplimit;
    if ((err = b3dFseek(fp, ndo * bigger, flag)))
      return err;
    ntodo -= ndo;
    flag = SEEK_CUR;
  }
  return 0;
}

/*!
 * Does a seek in a large image file with pointer [fp].  The amount to seek is
 * given by [base] plus the change in position indicated by [x], [y], and
 * [z], where [nx] and [ny] are the image dimensions in X and Y and [dsize] is
 * the number of bytes per data element.  Specifically, it seeks by 
 * ^  [base] + [x] * [dsize] + [nx] * [dsize] * ([y] + [ny] * [z]).
 * ^[flag] has the standard meaning for
 * seeks, e.g., SEEK_SET, etc.  Returns the nonzero seek error if error.
 */
int mrcHugeSeek(FILE *fp, int base, int x, int y, int z, int nx, int ny, 
               int dsize, int flag)
{
  int size1, size2;
  double testSize = (double)nx * ny * dsize;

  /* Always add the X offset to the base; then test whether X * Y is OK.
   If so add the Y offset to the base and use X*Y and Z as sizes, otherwise
   use X and Y*Z as the sizes */
  base += x * dsize;
  if (fabs(testSize) < 2.e9) {
    base += nx * y * dsize;
    size1 = nx * ny * dsize;
    size2 = z;
  } else {
    size1 = nx * dsize;
    size2 = y + ny * z;
  }
  return mrc_big_seek(fp, base, size1, size2, flag);
}

/*!
 * Reads a line of characters from the file pointed to by [fp] and places it 
 * into array [s] of size [limit].  Replaces newline with a null or 
 * terminates the string with null if reading stops because the array limit is
 * reached.  Returns the length of the string, or the negative of the length 
 * of the string on error or end of file.
 */
int fgetline(FILE *fp, char s[], int limit)
{
  int c, i, length;

  if (fp == NULL){
    b3dError(stderr, "fgetline: file pointer not valid\n");
    return(0);
  }

  if (limit < 3){
    b3dError(stderr, "fgetline: limit (%d) must be > 2\n", limit);
    return(0);
  }
     
  for (i=0; ( ((c = getc(fp)) != EOF) && (i < (limit-1)) && (c != '\n') ); i++)
    s[i]=c;
     
  if (i == 1){
    if (c == EOF){
      return(0);
    }
    if (c == '\n'){
      s[++i] = '\0';
      return(1);
    }
  }
               

  s[i]='\0';
  length = i;

  if (c == EOF)
    return (-1 * length);
  else
    return (length);
}

/*! Returns the number of possible extra header items encoded as short integers
 * by SerialEM in [nflags], and the number of bytes that each occupies in 
 * [nbytes], an array that should be dimensioned to 32. */
void b3dHeaderItemBytes(int *nflags, int *nbytes)
{

  /* Keep this synced to definitions in SerialEM/EMimageExtra.cpp */
  b3dByte extra_bytes[] = {2, 6, 4, 2, 2, 4, 2, 4, 2, 4, 2};
  int i;
  *nflags = sizeof(extra_bytes);
  for (i = 0; i < *nflags; i++)
    nbytes[i] = extra_bytes[i];
}

/*! A Fortran wrapper for b3dHeaderItemBytes. */
void b3dheaderitembytes(int *nflags, int *nbytes) 
{
  b3dHeaderItemBytes(nflags, nbytes);
}

/*! A variable argument min function for multiple integer arguments.
 * Call as:  b3dIMin(4, ival1, ival2, ival3, ival4); 
 * For only two arguments with acceptable cost of multiple evaluations,
 * use the macro, B3DMIN(val1, val2); */
/* 2/10/05: gave up on fully generic b3dMin/b3dMax as a bad idea */
int b3dIMin(int narg, ...)
{
  va_list args;
  int extreme;
  int i, val;

  va_start(args, narg);
  for (i = 0; i < narg; i++) {
    val = va_arg(args, int);
    if (!i)
      extreme = val;
    else
      extreme = val < extreme ? val : extreme;
  }
  return(extreme);
}

/*! A variable argument max function for multiple integer arguments.
 * Call as:  b3dIMax(4, ival1, ival2, ival3, ival4); 
 * For only two arguments with acceptable cost of multiple evaluations,
 * use the macro, B3DMAX(val1, val2); */
int b3dIMax(int narg, ...)
{
  va_list args;
  int extreme;
  int i, val;

  va_start(args, narg);
  for (i = 0; i < narg; i++) {
    val = va_arg(args, int);
    if (!i)
      extreme = val;
    else
      extreme = val > extreme ? val : extreme;
  }
  return(extreme);
}

/*
$Log$
Revision 1.2  2008/04/02 02:52:05  mast
Made the seek routine a no-op if file is stdin

Revision 1.1  2007/09/20 02:43:08  mast
Moved to new library

Revision 1.17  2007/05/19 00:00:55  mast
Added note about header bytes

Revision 1.16  2006/10/16 16:03:25  mast
Fixed a leak in case removal of backup file fails

Revision 1.15  2006/09/28 21:12:16  mast
Added huge seek routine that can handle images of any size

Revision 1.14  2006/09/20 23:03:01  mast
Added header usage function to be used as callback from PIP

Revision 1.13  2006/09/19 16:38:08  mast
Clean up warnings

Revision 1.12  2006/09/13 02:42:07  mast
Fixed leak in backup filename

Revision 1.11  2006/08/27 23:45:08  mast
Added fgetline

Revision 1.10  2006/06/08 03:13:15  mast
Add va_end to the error function

Revision 1.9  2006/01/23 06:40:23  mast
Documented

Revision 1.8  2005/02/11 01:40:44  mast
Added some includes, switched to simple b3dIMin/Max functions

Revision 1.7  2004/11/12 15:21:56  mast
Added min and max functions with variable arguments

Revision 1.6  2004/06/10 22:47:43  mast
Reserved a bunch of extra header flags to avoid transition problems

Revision 1.5  2004/03/18 17:55:32  mast
Added routine with extra header byte information

Revision 1.4  2004/01/17 20:35:48  mast
Move file I/O and seek routines here, add rewind routine

Revision 1.3  2003/11/01 16:41:56  mast
changed to use new error processing routine

Revision 1.2  2003/10/24 19:53:25  mast
Add stdlib.h for SGI

Revision 1.1  2003/10/24 03:01:34  mast
initial creation, consolidating routines from elsewhere

*/
