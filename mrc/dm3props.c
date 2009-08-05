/*
 *  dmprops.c - Determine size, data type, and offset to data in 
 *              DigitalMicrograph 3 files
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.6  2006/09/06 16:14:16  mast
Added include for the exit

Revision 3.5  2006/09/03 23:47:44  mast
Moved analysis routine to library

Revision 3.4  2005/02/11 01:42:34  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.3  2004/06/06 00:49:46  mast
Made to scan through whole starting buffer and take last Data entry
that is not too close to hte DataType entry at the end

Revision 3.2  2003/10/24 02:28:42  mast
strip directory from program name and/or use routine to make backup file

Revision 3.1  2001/12/31 19:19:52  mast
Initial version

*/
#include <stdio.h>
#include <stdlib.h>
#include "iimage.h"

int main(int argc, char **argv)
{
  int i;
  FILE *fp;
  RawImageInfo info;
  int  type;

  for (i = 1; i < argc; i++){
      
    fp = fopen(argv[i], "rb");
    if (!fp) {
      fprintf(stderr, "ERROR: dm3props - Opening %s\n", argv[i]);
      exit(1);
    }

    if (analyzeDM3(fp, argv[i], &info, &type))
      exit(1);
      
    printf("%d %d %d %d %d\n", info.nx, info.ny, info.nz, type,
           info.headerSize);
    fclose(fp);
  }
  exit(0);
}
