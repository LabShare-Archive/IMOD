/*   b3dutil.h   - utility functions for getting version and copyright, 
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
#ifndef B3DUTIL_H
#define B3DUTIL_H

#include <stdio.h>

#define MAX_IMOD_ERROR_STRING  512

#define B3DMIN(a,b) ((a) < (b) ? (a) : (b))
#define B3DMAX(a,b) ((a) > (b) ? (a) : (b))

#ifdef __cplusplus
extern "C" {
#endif

  int imodVersion(char *pname);
  void imodCopyright(void);
  char *imodProgName(char *fullname);
  int imodBackupFile(char *filename);
  char *f2cString(char *str, int strSize);
  int c2fString(char *cStr, char *fStr, int fSize);
  void b3dSetStoreError(int ival);
  void b3dError(FILE *stream, char *format, ...);
  char *b3dGetError();

  int b3dFseek(FILE *fp, int offset, int flag);
  size_t b3dFread(void *buf, size_t size, size_t count, FILE *fp);
  size_t b3dFwrite(void *buf, size_t size, size_t count, FILE *fp);
  void b3dRewind(FILE *fp);
  int mrc_big_seek(FILE *fp, int base, int size1, int size2, int flag);

  void b3dHeaderItemBytes(int *nflags, int *nbytes);

  int b3dIMin(int narg, ...);
  int b3dIMax(int narg, ...);

#ifdef __cplusplus
}
#endif


#endif
