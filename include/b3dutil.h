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
*/
#ifndef B3DUTIL_H
#define B3DUTIL_H

#ifdef __cplusplus
extern "C" {
#endif

  int imodVersion(char *pname);
  void imodCopyright(void);
  char *imodProgName(char *fullname);
  int imodBackupFile(char *filename);
  char *f2cString(char *str, int strSize);
  int c2fString(char *cStr, char *fStr, int fSize);

#ifdef __cplusplus
}
#endif


#endif
