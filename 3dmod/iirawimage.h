/*   iirawimage.h  -  declarations for iirawimage.cpp
 *
 *   Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef IIRAWIMAGE_H
#define IIRAWIMAGE_H

typedef struct ImodImageFileStruct ImodImageFile;

extern "C" {
  int iiRawCheck(ImodImageFile *inFile);
}

int iiRawScan(ImodImageFile *inFile);
void iiRawSetSize(int nx, int ny, int nz);
int iiRawSetMode(int mode);
void iiRawSetSwap();
void iiRawSetInverted();
void iiRawSetHeaderSize(int size);
void iiRawSetScale(float smin, float smax);

#endif
