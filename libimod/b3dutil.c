/*   b3dutil.c   - utility functions for getting version and copyright, 
 *                      trimming program name
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
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
#endif

#ifdef F77FUNCAP
#define imodbackupfile_ IMODBACKUPFILE
#define imodgetenv_ IMODGETENV
#define b3dheaderitembytes_ B3DHEADERITEMBYTES
#endif

/* DNM 2/26/03: These need to be printf instead of fprintf(stderr) to not
   crash imod under Windows */
int imodVersion(char *pname)
{
  if (pname)
    printf("%s Version %s %s %s\n",
           pname, VERSION_NAME, __DATE__, __TIME__);
  return(VERSION);
}

void imodCopyright(void)
{
  char *uofc =   "the Regents of the University of Colorado";
  printf("Copyright (C) %s by %s\n%s & %s\n", COPYRIGHT_YEARS,
         LAB_NAME1, LAB_NAME2, uofc);
  return;
}

/* imodProgName returns a program name stripped of directories and .exe,
 * given the full name (argv[0]) in fullname
 * It returns a pointer internal to argv[0], unless the name ends in .exe,
 * in which case it tries to return a duplicate copy.  Do not free it */
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


/* imodBackupFile renames an existing file named filename to filename~ and
   deletes filename~ first if necessary */
int imodBackupFile(char *filename)
{
  struct stat buf;
  int len;
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
  if (!stat(backname, &buf) && remove(backname))
    return -1;

  /* finally, rename file */
  return rename(filename, backname);
}

/* A fortran wrapper for the function to make backup file */

int imodbackupfile_(char *filename, int strlen)
{
  int err;
  char *cstr = f2cString(filename, strlen);
  if (!cstr)
    return -2;
  err = imodBackupFile(cstr);
  free(cstr);
  return err;
}

/* Get an environment variable for a fortran routine */
int imodgetenv_(char *var, char *value, int varSize, int valueSize)
{
  char *valPtr;
  char *cstr = f2cString(var, varSize);
  if (!cstr)
    return -1;
  valPtr = getenv(cstr);
  free(cstr);
  if (!valPtr)
    return -1;
  return c2fString(valPtr, value, valueSize);
}


/* Create a C string with a copy of a Fortran string */
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

/* Return a C string into a Fortran string, return error if it won't fit */
int c2fString(char *cStr, char *fStr, int fSize)
{
  int i;
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

/* Set to not print messages */
void b3dSetStoreError(int ival)
{
  storeError = ival;
}

/* Store an error, possibly print it */
void b3dError(FILE *out, char *format, ...)
{
  va_list args;
  va_start(args, format);
  
  vsprintf(errorMess, format, args);
  if (out && !storeError)
    fprintf(out, errorMess);
}

/* Return the error string */
char *b3dGetError()
{
  return &errorMess[0];
}

/* These routines will simply call the standard C routine unless under Windows,
   then they will get the matching file handle and call the low-level Windows
   routine */
int b3dFseek(FILE *fp, int offset, int flag)
{
#if defined(WIN32_BIGFILE) || defined(MAC103_BIGFILE)
  int handle = fileno(fp);
  off_t err;
  err = lseek(handle, (off_t)offset, flag);
  return (err == -1 ? -1 : 0);
#else
  return fseek(fp, offset, flag);
#endif
}

size_t b3dFread(void *buf, size_t size, size_t count, FILE *fp)
{
#if defined(WIN32_BIGFILE) || defined(MAC103_BIGFILE)
  int handle = fileno(fp);
  return (size_t)(read(handle, buf, size * count) / size);
#else
  return fread(buf, size, count, fp);
#endif
}
 
size_t b3dFwrite(void *buf, size_t size, size_t count, FILE *fp)
{
#if defined(WIN32_BIGFILE) || defined(MAC103_BIGFILE)
  int handle = fileno(fp);
  return (size_t)(write(handle, buf, size * count) / size);
#else
  return fwrite(buf, size, count, fp);
#endif
}

void b3dRewind(FILE *fp)
{
  b3dFseek(fp, 0, SEEK_SET);
}

#define SEEK_LIMIT 2000000000

int mrc_big_seek(FILE *fp, int base, int size1, int size2, int flag)
{
  int smaller, bigger, ntodo, ndo, abs1, abs2;
  int steplimit, err;

  /* Do the base seek if it is non-zero, or if the rest of the seek is
     zero and we are doing a SEEK_SET */
  if (base || ((!size1 || !size2) && (flag == SEEK_SET))) {
    if (err = b3dFseek(fp, base, flag))
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
    if (err = b3dFseek(fp, ndo * bigger, flag))
      return err;
    ntodo -= ndo;
    flag = SEEK_CUR;
  }
  return 0;
}

/* A central place to get the list of number of possible extra header items
   and the number of bytes each, including a Fortran wrapper
   nbytes is an array that should be dimensioned to 32 */
void b3dHeaderItemBytes(int *nflags, int *nbytes)
{
  b3dByte extra_bytes[] = {2, 6, 4, 2, 2, 4, 2, 4, 2, 4, 2};
  int i;
  *nflags = sizeof(extra_bytes);
  for (i = 0; i < *nflags; i++)
    nbytes[i] = extra_bytes[i];
}

void b3dheaderitembytes_(int *nflags, int *nbytes) 
{
  b3dHeaderItemBytes(nflags, nbytes);
}

/* Variable argument min and max subroutines
 *  Call as:  b3dMin("ffif", fval1, fval2, ival, &outVal); 
 * For only two arguments with acceptable cost of multiple evaluations,
 * use the macros, B3DMIN(val1, val2);
 * Why two functions?  A wrapper didn't work.  Also double return values were 
 * not correct so the return address is the last argument
 */
int b3dMin(char *types, ...)
{
  va_list args;
  double val, extreme;
  int narg = 0;
  double *doubp;
  float *floatp;
  int *intp;
  b3dInt16 *shortp;

  va_start(args, types);
  while (*(types + 1)) {
    switch (*types++) {
    case 'i':
      val = (double)va_arg(args, int);
      break;
    case 'f':
      val = (double)va_arg(args, float);
      break;
    case 'd':
      val = (double)va_arg(args, double);
      break;
    case 's':
      val = (double)va_arg(args, b3dInt16);
      break;
    default:
      fprintf(stderr, "ERROR: b3dMin - improper code for input argument"
              " type\n");
      return 1;
    }

    if (!narg)
      extreme = val;
    else
      extreme = val < extreme ? val : extreme;
    narg++;
  }

  if (narg < 2) {
    fprintf(stderr, "ERROR: b3dMin - too few arguments\n");
    return 1;
  }

  switch (*types) {
    case 'i':
      intp = (int *)va_arg(args, int *);
      *intp = (int)floor(extreme + 0.5);
      break;
    case 'f':
      floatp = (float *)va_arg(args, float *);
      *floatp = (float)extreme;
      break;
    case 'd':
      doubp = (double *)va_arg(args, double *);
      *doubp = extreme;
      break;
    case 's':
      shortp = (b3dInt16 *)va_arg(args, b3dInt16 *);
      *shortp = (b3dInt16)floor(extreme + 0.5);
      break;
    default:
      fprintf(stderr, "ERROR: b3dMin - improper code for output argument"
              " type\n");
      return 1;
    }

  va_end(args);
  return 0;
}

int b3dMax(char *types, ...)
{
  va_list args;
  double val, extreme;
  int narg = 0;
  double *doubp;
  float *floatp;
  int *intp;
  b3dInt16 *shortp;

  va_start(args, types);
  while (*(types + 1)) {
    switch (*types++) {
    case 'i':
      val = (double)va_arg(args, int);
      break;
    case 'f':
      val = (double)va_arg(args, float);
      break;
    case 'd':
      val = (double)va_arg(args, double);
      break;
    case 's':
      val = (double)va_arg(args, b3dInt16);
      break;
    default:
      fprintf(stderr, "ERROR: b3dMax - improper code for input argument"
              " type\n");
      return 1;
    }

    if (!narg)
      extreme = val;
    else
      extreme = val > extreme ? val : extreme;
    narg++;
  }

  if (narg < 2) {
    fprintf(stderr, "ERROR: b3dMax - too few arguments\n");
    return 1;
  }

  switch (*types) {
    case 'i':
      intp = (int *)va_arg(args, int *);
      *intp = (int)floor(extreme + 0.5);
      break;
    case 'f':
      floatp = (float *)va_arg(args, float *);
      *floatp = (float)extreme;
      break;
    case 'd':
      doubp = (double *)va_arg(args, double *);
      *doubp = extreme;
      break;
    case 's':
      shortp = (b3dInt16 *)va_arg(args, b3dInt16 *);
      *shortp = (b3dInt16)floor(extreme + 0.5);
      break;
    default:
      fprintf(stderr, "ERROR: b3dMax - improper code for output argument"
              " type\n");
      return 1;
    }

  va_end(args);
  return 0;
}

