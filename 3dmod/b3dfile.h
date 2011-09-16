/*   b3dfile.h  -  declarations for b3dfile.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 4.1  2003/02/10 20:41:54  mast
Merge Qt source

Revision 1.1.2.2  2003/02/07 01:03:33  mast
a little cleanup

Revision 1.1.2.1  2003/01/26 23:31:09  mast
Initial creation

*/

#ifndef B3DFILE_H
#define B3DFILE_H

#ifdef __cplusplus
extern "C" {
#endif

/* special file io commands. */
int bdRGBWrite(FILE *fout, int xsize, int ysize, unsigned char *pixels);
int bdTIFFWriteMap(FILE *fout, int xsize, int ysize,
		   unsigned char *pixels, b3dUInt16 *cmap);
  void iputbyte(FILE *fout, unsigned char val);
  void iputshort(FILE *fout, b3dUInt16 val);
  void iputlong(FILE *fout, b3dUInt32 val);


#ifdef __cplusplus
}
#endif

#endif
