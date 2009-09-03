/*
 *  $Id$
 *
 *  Author: David Mastronarde  email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *  Log at end
 */

#include <stdio.h>
#include <string.h>

#include "imodel.h"
#include "istore.h"
#include "parse_params.h"

static int istoreFindValue(Ilist *list, int index, int type, float *value,
                           int *listInd);

int main( int argc, char *argv[])
{
  int i;
  FILE *fin, *fout;
  struct Mod_Model *mod;
  int npatch = 0;
  int nvalue = 0, nvalue2 = 0;
  int ix, iy, iz;
  float dx, dy, dz;
  float value, maxval = -1.e30;
  float value2, maxval2 = -1.e30;
  int ob, co, listInd, listStart;
  Ipoint *pts;
  char format[10] = "%10.4f";
  char format2[10] = "%10.4f";

  setExitPrefix("ERROR: imod2patch - ");


  if (argc != 3){
    if (argc != 1)
      printf("ERROR: imod2patch - wrong # of arguments\n");
    printf("imod2patch usage:\n");
    printf("imod2patch imod_model patch_file\n");
    exit(1);
  }


  i = 1;

  mod = imodRead(argv[i]);
  if (!mod)
    exitError("Reading model %s\n", argv[i]);
  
  if (imodBackupFile(argv[++i]))
    exitError("Renaming existing output file to %s\n", argv[i]);
  
  fout = fopen(argv[i], "w");
  if (!fout)
    exitError("Could not open %s\n", argv[i]);

  /* Count up patches and values and find max of values */
  for (ob = 0; ob < mod->objsize; ob++) {
    listInd = 0;
    for (co = 0; co < mod->obj[ob].contsize; co++) {
      listStart = listInd;
      if (mod->obj[ob].cont[co].psize >= 2) {
        npatch++;
        if (istoreFindValue(mod->obj[ob].store, co, GEN_STORE_VALUE1, &value,
                            &listInd)) {
          maxval = B3DMAX(maxval, value);
          nvalue++;
        }
        listInd = listStart;
        if (istoreFindValue(mod->obj[ob].store, co, GEN_STORE_VALUE2, &value2,
                            &listInd)) {
          maxval2 = B3DMAX(maxval2, value2);
          nvalue2++;
        }
      }
    }
  }

  /* If values are greater than one they are probably residuals and only need
     2 decimal places */
  if (nvalue && maxval > 1.01)
    strcpy(format, "%10.2f");
  if (nvalue2 && maxval2 > 1.01)
    strcpy(format, "%10.2f");

  fprintf(fout, "%d   edited positions\n", npatch);
  for (ob = 0; ob < mod->objsize; ob++) {
    listInd = 0;
    for (co = 0; co < mod->obj[ob].contsize; co++) {
      listStart = listInd;
      if (mod->obj[ob].cont[co].psize >= 2) {
        pts = mod->obj[ob].cont[co].pts;
        ix = pts[0].x + 0.5;
        iy = pts[0].y + 0.5;
        iz = pts[0].z + 0.5;
        dx = (pts[1].x - pts[0].x) / mod->pixsize;
        dy = (pts[1].y - pts[0].y) / mod->pixsize;
        dz = (pts[1].z - pts[0].z) / mod->pixsize;
        if (mod->flags & IMODF_FLIPYZ)
          fprintf(fout, "%6d %5d %5d %8.2f %8.2f %8.2f", 
                  ix, iz, iy, dx, dz, dy);
        else
          fprintf(fout, "%6d %5d %5d %8.2f %8.2f %8.2f", 
                  ix, iy, iz, dx, dy, dz);
        if (nvalue) {
          value = 0.;
          istoreFindValue(mod->obj[ob].store, co, GEN_STORE_VALUE1, &value,
                          &listInd);
          fprintf(fout, format, value);
          
        }
        if (nvalue2) {
          listInd = listStart;
          value2 = 0.;
          istoreFindValue(mod->obj[ob].store, co, GEN_STORE_VALUE2, &value2,
                          &listInd);
          fprintf(fout, format2, value2);
          
        }
        fprintf(fout, "\n");

      }
    }
  }
  fclose(fout);
  exit(0);
}


/* This is meant to be called sequentially for all indexes in the entity,
   not for random access */
static int istoreFindValue(Ilist *list, int index, int type, float *value,
                     int *listInd)
{
  Istore *store;
  while (*listInd < ilistSize(list)) {
    store = istoreItem(list, *listInd);
    if ((store->flags & GEN_STORE_NOINDEX) || store->index.i > index)
      break;
    (*listInd)++;

    if (store->index.i == index && store->type == type) {
      *value = store->value.f;
      return 1;
    }
  }
  return 0;
}

/*
$Log$
Revision 3.8  2006/09/12 15:02:55  mast
add include

Revision 3.7  2006/08/31 20:58:26  mast
Extract values from model and put back in patch file

Revision 3.6  2005/02/11 00:41:40  mast
Removed unneeded declaration

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
