/*
 *  $Id$
 *
 *  Author: David Mastronarde  email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.5  2004/11/05 19:05:29  mast
Include local files with quotes, not brackets

Revision 3.4  2004/09/21 22:30:28  mast
Fixed stray ~ in error string

Revision 3.3  2004/07/07 19:25:30  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.2  2003/10/24 03:05:23  mast
open as binary, strip program name and/or use routine for backup file

Revision 3.1  2002/12/23 21:34:22  mast
fixed exit status

*/

#include <stdio.h>
#include <string.h>

#include "imodel.h"


struct Mod_Model *imod_from_patches(FILE *fin, float scale);

int main( int argc, char *argv[])
{
  int i;
  FILE *fin, *fout;
  struct Mod_Model *mod;
  float scale = 10.0;
  int npatch = 0;
  int ix, iy, iz;
  float dx, dy, dz;
  int ob, co;
  Ipoint *pts;

  if (argc != 3){
    if (argc != 1)
      printf("ERROR: imod2patch - wrong # of arguments\n");
    printf("imod2patch version 1.0 usage:\n");
    printf("imod2patch imod_model patch_file\n");
    exit(1);
  }


  i = 1;

  mod = imodRead(argv[i]);
  if (!mod){
    fprintf(stdout, "ERROR: imod2patch - reading model %s\n",
            argv[i]);
    exit(3);
  }
  
  if (imodBackupFile(argv[++i])) {
    fprintf(stdout, "ERROR: imod2patch - renaming existing output file "
            "to %s\n", argv[i]);
    exit(1);
  }
  
  fout = fopen(argv[i], "w");
  if (!fout){
    fprintf(stdout, "ERROR: imod2patch - Couldn't open %s\n", argv[i]);
    exit(3);
  }

  for (ob = 0; ob < mod->objsize; ob++)
    for (co = 0; co < mod->obj[ob].contsize; co++)
      if (mod->obj[ob].cont[co].psize >= 2)
        npatch++;

  fprintf(fout, "%d   edited positions\n", npatch);
  for (ob = 0; ob < mod->objsize; ob++)
    for (co = 0; co < mod->obj[ob].contsize; co++)
      if (mod->obj[ob].cont[co].psize >= 2) {
        pts = mod->obj[ob].cont[co].pts;
        ix = pts[0].x + 0.5;
        iy = pts[0].y + 0.5;
        iz = pts[0].z + 0.5;
        dx = (pts[1].x - pts[0].x) / mod->pixsize;
        dy = (pts[1].y - pts[0].y) / mod->pixsize;
        dz = (pts[1].z - pts[0].z) / mod->pixsize;
        if (mod->flags & IMODF_FLIPYZ)
          fprintf(fout, "%d %d %d %.2f, %.2f, %.2f\n", 
                  ix, iz, iy, dx, dz, dy);
        else
          fprintf(fout, "%d %d %d %.2f, %.2f, %.2f\n", 
                  ix, iy, iz, dx, dy, dz);
      }
  fclose(fout);
  exit(0);
}

