/*  IMOD VERSION 2.50
 *
 *  $Id$
 *
 *  Author: David Mastronarde  email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.1  2002/12/23 21:34:22  mast
fixed exit status

*/

#include <stdio.h>
#include <string.h>

#include <imodel.h>


static int fgetline(FILE *fp, char s[],int limit);
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
    exit(-1);
  }
  
  if (imodBackupFile(argv[++i])) {
    fprintf(stdout, "ERROR: imod2patch - renaming existing output file "
            "to %s\~n", argv[i]);
    exit(1);
  }
  
  fout = fopen(argv[i], "w");
  if (!fout){
    fprintf(stdout, "ERROR: imod2patch - Couldn't open %s\n", argv[i]);
    exit(-1);
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

