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
Revision 1.1  2003/10/24 02:44:36  mast
Initial creation

*/
#ifndef B3DUTIL_H
#define B3DUTIL_H

#define MAX_IMOD_ERROR_STRING  512

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

#ifdef __cplusplus
}
#endif


#endif
