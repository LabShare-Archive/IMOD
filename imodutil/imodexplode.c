/*  IMOD VERSION 2.30
 *
 *  imodexplode: make an "exploded" model by shifting objects by different
 *  amounts
 *
 *  Author: David Mastronarde,  mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <stdlib.h>
#include <stdio.h>

#include "imodel.h"

int *parselist (char *line, int *nlist);

#define SETLIMIT 1000

static int imodexpl_usage(char *prog, int retcode)
{
  fprintf(stderr, "%s usage: %s -o list -x dx -y dy -z dz ... infile"
          " outfile\n",prog,prog);
  fprintf(stderr, "\tShifts the objects in each list by the offsets dx,"
          " dy, and dz.\n");
  fprintf(stderr, "\tA list of objects (following -o) may contain comma-separated ranges.\n");
  fprintf(stderr, "\tOnly one offset, and only non-zero offsets, need to be entered.\n");
  fprintf(stderr, "\tAny number of lists and offsets may be entered in"
          " series.\n");
  if (retcode)
    exit(retcode);
  return(retcode);
}


int main(int argc, char **argv)
{
  Imod *imod;
  Ipoint *pts;
  FILE *fout;
  Icont *cont;
  Imesh *mesh;
  int ob, i, ch, j, pt, co, set, me;

  int nlist = 0;
  int *list;

  int *listp[SETLIMIT];
  int nsets = 0;
  int ninset[SETLIMIT];
  float dx[SETLIMIT], dy[SETLIMIT], dz[SETLIMIT];
  char *progname = imodProgName(argv[0]);
    

  if (argc < 1){
    imodVersion(progname);
    imodCopyright();
    imodexpl_usage(progname, -1);
  }
    
  for (i = 1; i < argc ; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){
            
      case 'o': /* specify objects. */
        list = parselist(argv[++i], &nlist);
        if (!list) {
          fprintf(stderr, "%s: Error parsing object list\n", progname);
          exit(3);
        }
        ninset[nsets] = nlist;
        listp[nsets] = (int *)malloc(nlist * sizeof(int));
        for (i = 0; i < nlist; i++)
          listp[nsets][i] = list[i];
        dx[nsets] = 0.0;
        dy[nsets] = 0.0;
        dz[nsets++] = 0.0;
        break;
            
      case 'x':
        if (!nsets) {
          fprintf(stderr, "%s: Must define object list before offsets\n"
                  , progname);
          exit(3);
        }
        dx[nsets - 1] = atof(argv[++i]);
        break;
            
      case 'y':
        if (!nsets) {
          fprintf(stderr, "%s: Must define object list before offsets\n"
                  , progname);
          exit(3);
        }
        dy[nsets - 1] = atof(argv[++i]);
        break;
            
      case 'z':
        if (!nsets) {
          fprintf(stderr, "%s: Must define object list before offsets\n"
                  , progname);
          exit(3);
        }
        dz[nsets - 1] = atof(argv[++i]);
        break;
            
      case '?':
        imodexpl_usage(progname, -1);
        break;
      default:
        fprintf(stderr, "%s: unknown option %s\n", progname, argv[i]);
        imodexpl_usage(progname, -1);
        break;
               
      }
    } else
      break;
  }    

  if (i != argc - 2)
    imodexpl_usage(progname, -1);
    
        
  imod  = imodRead(argv[i]); /* The model we will edit for skinning. */
        
  if (!imod){
    fprintf(stderr, "%s: Error reading model %s\n",
            progname, argv[i]);
    exit(3);
  }
        

  for (set = 0; set < nsets; set++) {
    for (j = 0; j < ninset[set]; j++) {
      ob = listp[set][j] - 1;
      if (ob < 0 || ob >= imod->objsize)
        fprintf(stderr, "No object # %d\n", ob + 1);
      else {
        /* Shift contour data */
        for (co = 0; co < imod->obj[ob].contsize; co++) {
          cont = &(imod->obj[ob].cont[co]);
          pts = cont->pts;
          for (pt = 0; pt < cont->psize; pt++) {
            pts[pt].x += dx[set];
            pts[pt].y += dy[set];
            pts[pt].z += dz[set];
          }
        }
        /* Shift mesh data */
        for (me = 0; me <imod->obj[ob].meshsize; me++){
          mesh = &(imod->obj[ob].mesh[me]);
          pts = mesh->vert;
          for (pt = 0; pt <mesh->vsize; pt++)
            if (pt % 2 == 0) { 
              pts[pt].x += dx[set];
              pts[pt].y += dy[set];
              pts[pt].z += dz[set];
            }
        }
      }
    }
  }
    
  i = argc - 1;
  /* Save backup of Model to Model~ */
  if (imodBackupFile(argv[i])) {
    fprintf(stderr, "%s: Error, couldn't create backup", progname);
    exit(3);
  }

  fout = fopen(argv[i], "wb");
  if (!fout){
    fprintf(stderr, "%s: Error, couldn't open output.", progname);
    exit(3);
  }

  imodWrite(imod, fout);
  fclose(fout);
  exit(0);
}


