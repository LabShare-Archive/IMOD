/*   b3dutil.h   - utility functions for getting version and copyright, 
 *                      trimming program name
 *
 *   Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *
 *   $Id$
 *   No more Log
 */                                                                           

#ifndef B3DUTIL_H
#define B3DUTIL_H

#include <stdio.h>

/* Include this since this include file was split off from here */
#include "cfsemshare.h"

#define MAX_IMOD_ERROR_STRING  512

#define B3DMIN(a,b) ((a) < (b) ? (a) : (b))
#define B3DMAX(a,b) ((a) > (b) ? (a) : (b))
#define B3DNINT(a) (int)floor((a) + 0.5)
#define B3DFREE(a) if (a) free(a)
#define B3DMALLOC(a,b) (a *)malloc((b) * sizeof(a))

#ifdef __cplusplus
extern "C" {
#endif

  int imodVersion(char *pname);
  void imodCopyright(void);
  void imodUsageHeader(char *pname);
  char *IMOD_DIR_or_default(int *assumed);
  char *imodProgName(char *fullname);
  int imodBackupFile(char *filename);
  int imodGetpid();
  char *f2cString(char *str, int strSize);
  int c2fString(char *cStr, char *fStr, int fSize);
  void b3dSetStoreError(int ival);
  void b3dError(FILE *stream, char *format, ...);
  char *b3dGetError(void);

  int b3dFseek(FILE *fp, int offset, int flag);
  size_t b3dFread(void *buf, size_t size, size_t count, FILE *fp);
  size_t b3dFwrite(void *buf, size_t size, size_t count, FILE *fp);
  void b3dRewind(FILE *fp);
  int mrc_big_seek(FILE *fp, int base, int size1, int size2, int flag);
  int mrcHugeSeek(FILE *fp, int base, int x, int y, int z, int nx, int ny, 
                  int dsize, int flag);
  int fgetline(FILE *fp, char s[],int limit);

  void b3dHeaderItemBytes(int *nflags, int *nbytes);
  void setOrClearFlags(b3dUInt32 *flags, b3dUInt32 mask, int state);
  unsigned char **makeLinePointers(void *array, int xsize, int ysize, int dsize);

  int b3dIMin(int narg, ...);
  int b3dIMax(int narg, ...);
  double wallTime(void);
  int b3dMilliSleep(int msecs);
  int numOMPthreads(int optimalThreads);
  int b3dOMPthreadNum();

#ifdef __cplusplus
}
#endif


#endif
