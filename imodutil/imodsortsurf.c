/*  IMOD VERSION 2.30
 *
 *  imodsortsurf: sort contours into surfaces based on their mesh connections
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
/*  $Author$

$Date$

$Revision$

$Log$
Revision 3.3  2004/07/07 19:25:30  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.2  2003/10/24 03:05:24  mast
open as binary, strip program name and/or use routine for backup file

Revision 3.1  2003/02/21 23:16:15  mast
Open output file in binary mode

*/

#include "imodel.h"

static void usage(char *prog)
{
  fprintf(stderr, "Usage: %s [-o list_of_objects] input_model"
          " output_model\n", prog);
  fprintf(stderr, "       The optional list of object numbers can include ranges, e.g. 1-3,6-9,13\n");
  exit(3);
}

int *parselist (char *line, int *nlist);
int imodObjSortSurf(Iobj *obj);

int main(int argc, char **argv)
{
  Imod *inModel;
  int ob, objdo;
  int *list;
  int i, nlist, origsize;
    
  if (argc == 1) usage(argv[0]);

  if (strcmp(argv[1], "-o") == 0) {
    if (argc != 5) usage(argv[0]);
    list = parselist(argv[2], &nlist);
    if (!list) {
      fprintf(stderr, "%s: Error parsing object list\n", argv[0]);
      exit(1);
    } 
  } else {
    if (argc != 3) usage(argv[0]);
    nlist = 0;
  }
    

  inModel = imodRead(argv[argc-2]);
  if (!inModel) {
    fprintf(stderr, "%s: Fatal error reading model\n", argv[0]);
    exit(1);
  }

  for (ob = 0; ob < inModel->objsize; ob++) {
    objdo = 1;
    if (nlist) {
      objdo = 0;
      for (i = 0; i < nlist; i++)
        if (list[i] == ob + 1)
          objdo = 1;
    }
    if (objdo) {
      if (inModel->obj[ob].meshsize) {
        if (inModel->obj[ob].surfsize)
          printf("Object %d already has surface information which will"
                 " be replaced\n", ob + 1);
        if (imodObjSortSurf(&(inModel->obj[ob])) ) 
          printf("Error sorting object %d\n", ob + 1);
        else
          printf("Object %d sorted into %d surfaces\n",ob + 1,
                 inModel->obj[ob].surfsize);
      } else
        printf("Object %d has no mesh data and cannot be sorted\n", ob + 1);
    }
  } 

  imodBackupFile(argv[argc - 1]);
  if (imodOpenFile(argv[argc - 1], "wb", inModel)) {
    fprintf(stderr, "%s: Fatal error opening new model\n", argv[0]);
    exit (1);
  }
  imodWriteFile(inModel);
  exit(0);
}



int imodObjSortSurf(Iobj *obj)
{
  int npoly, me, i, j, poly, ninpoly, iwork, nwork, scan, found;
  int refvert, work, nsurfs, co, pt, ind, resol;
  int *listp, *surfs, *meshes, *nverts, *towork, *refp, *scanp;
  int **starts;
  float *zmins, *zmaxs;
  Ipoint *vertp;
  float zmin, zmax, ptx, pty, ptz;

  imodMeshNearestRes(obj->mesh, obj->meshsize, 0, &resol);

  /* Count polygons in all meshes based on start flags */

  npoly = 0;
  for (me = 0; me < obj->meshsize; me++) {
    if (imeshResol(obj->mesh[me].flag) == resol) {
      listp = obj->mesh[me].list;
      for (i = 0; i < obj->mesh[me].lsize; i++)
        if (*listp++ == IMOD_MESH_BGNPOLYNORM)
          npoly++;
    }
  }

  if (!npoly)
    return (1);
     
  /* Get arrays for Z values and surface assignment for each polygon, which
     mesh it is in, address of start of polygons, for number of
     vertices in the polygon, and for list of polys to work on */

  zmins = (float *)malloc (npoly * sizeof(float));
  zmaxs = (float *)malloc (npoly * sizeof(float));
  surfs = (int *)malloc (npoly * sizeof(int));
  meshes = (int *)malloc (npoly * sizeof(int));
  starts = (int **)malloc (npoly * sizeof(int *));
  nverts = (int *)malloc (npoly * sizeof(int));
  towork = (int *)malloc (npoly * sizeof(int));
     
  if (!zmins || !zmaxs || !surfs || !meshes || !starts || !nverts || 
      !towork) {
    if (zmins) free(zmins);
    if (zmaxs) free(zmaxs);
    if (meshes) free(meshes);
    if (surfs) free(surfs);
    if (starts) free(starts);
    if (nverts) free(nverts);
    if (towork) free(towork);

    return (2);
  }

  /* Find min and max Z values of each polygon, and collect other info */

  poly = 0;
  for (me = 0; me < obj->meshsize; me++) {
    if (imeshResol(obj->mesh[me].flag) != resol)
      continue;
    listp = obj->mesh[me].list;
    vertp = obj->mesh[me].vert;
    i = 0;
    while (i < obj->mesh[me].lsize) {
      if (listp[i++] == IMOD_MESH_BGNPOLYNORM) {
        zmin = vertp[listp[i + 1]].z;
        zmax = zmin;
        surfs[poly] = 0;
        meshes[poly] = me;
        starts[poly] = listp + i + 1;    /* point to first vertex index */
        ninpoly = 0;
        while (listp[i++] != IMOD_MESH_ENDPOLY) {
          ind = listp[i++];
          if (vertp[ind].z < zmin)
            zmin = vertp[ind].z;
          if (vertp[ind].z > zmax)
            zmax = vertp[ind].z;
          ninpoly++;
        }
        nverts[poly] = ninpoly;
        zmins[poly] = zmin;
        zmaxs[poly++] = zmax;
      }
    }
  }

  /* loop through all polygons, looking for next one that's not assigned
     yet */

  nsurfs = 0;
  for (poly = 0; poly < npoly; poly ++) {
    if (!surfs[poly]) {
      nsurfs++;
      surfs[poly] = nsurfs;
      towork[0] = poly;
      nwork = 1;
      iwork = 0;
      while (iwork < nwork) {

        /* To work on a polygon, scan through all the rest that are not
           assigned yet, find ones that overlap in Z */

        work = towork[iwork];
        for (scan = 0; scan < npoly; scan++) {
          if (!surfs[scan] && zmins[work] <= zmaxs[scan] &&
              zmins[scan] <= zmaxs[work]) {
           
            /* Look for common vertices between the polygons */
           
            refp = starts[work];
            found = 0;
            i = 0;
            while (i < nverts[work] && !found) {
              refvert = *refp++;
              refp++;
              scanp = starts[scan];
              for (j = 0; j < nverts[scan]; j++) {
                if (*scanp++ == refvert) {
                  found = 1;
                  break;
                }
                scanp++;
              }
              i++;
            }

            /* If found a match, then add the scan polygon to work list as 
               well as assigning it to this surface */

            if (found) {
              towork[nwork++] = scan;
              surfs[scan] = nsurfs;
            }
          }
        }
        iwork++;
      }
    }
  }

  /* Now go through contours, looking for polygons with a matching vertex */

  for (co = 0; co < obj->contsize; co++) {
    found = 0;
    for (pt = 0; pt < obj->cont[co].psize; pt++) {
      ptx = obj->cont[co].pts[pt].x;
      pty = obj->cont[co].pts[pt].y;
      ptz = obj->cont[co].pts[pt].z;
      for (poly = 0; poly < npoly; poly++) {
        if (ptz >= zmins[poly] && ptz <= zmaxs[poly]) {
          vertp = obj->mesh[meshes[poly]].vert;
          scanp = starts[poly];
          for (j = 0; j < nverts[poly]; j++) {
            ind = *scanp++;
            if (vertp[ind].x == ptx && vertp[ind].y == pty && 
                vertp[ind].z == ptz) {
              found = 1;
              obj->cont[co].surf = surfs[poly];
              if (surfs[poly] > obj->surfsize)
                obj->surfsize = surfs[poly];
              break;
            }
            scanp++;
          }
        }
        if (found)
          break;
      }
      if (found)
        break;
    } 
  }
  free(zmins);
  free(zmaxs);
  free(meshes);
  free(surfs);
  free(starts);
  free(nverts);
  free(towork);
  return (0);
}
