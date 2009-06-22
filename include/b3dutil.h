/*   b3dutil.h   - utility functions for getting version and copyright, 
 *                      trimming program name
 *
 *   Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.
 *
 *   $Id$
 *   Log at end
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

#ifdef __cplusplus
extern "C" {
#endif

  int imodVersion(char *pname);
  void imodCopyright(void);
  void imodUsageHeader(char *pname);
  char *imodProgName(char *fullname);
  int imodBackupFile(char *filename);
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

  int b3dIMin(int narg, ...);
  int b3dIMax(int narg, ...);
  double wallTime(void);
  int numOMPthreads(int optimalThreads);

#ifdef __cplusplus
}
#endif


#endif

/*
$Log$
Revision 1.16  2008/11/15 21:51:05  mast
flag function

Revision 1.15  2007/10/01 15:27:41  mast
Split out everything not in b3dutil.c

Revision 1.14  2007/09/20 15:42:12  mast
Added declarations for new routines

Revision 1.13  2007/04/26 19:07:44  mast
Add B3DNINT

Revision 1.12  2006/09/28 21:13:23  mast
Added huge seek routine

Revision 1.11  2006/09/20 23:02:15  mast
Added header usage function

Revision 1.10  2006/09/19 16:33:32  mast
fix prototypes

Revision 1.9  2006/08/27 23:47:00  mast
Moved fgetline from mrcfiles to b3dutil

Revision 1.8  2006/06/26 14:49:09  mast
Moved miscellaneous functions to b3dutil

Revision 1.7  2005/02/11 01:41:04  mast
Swicthed to b3dIMin/Max

Revision 1.6  2004/11/12 15:22:13  mast
Added min and max functions and macros

Revision 1.5  2004/03/18 17:56:57  mast
Added header byte count routine

Revision 1.4  2004/01/17 20:34:03  mast
Add b3d file routines

Revision 1.3  2003/11/04 17:14:21  mast
Add include of stdio so that FILE is defined

Revision 1.2  2003/11/01 16:41:13  mast
Add error routines

Revision 1.1  2003/10/24 02:44:36  mast
Initial creation

*/
