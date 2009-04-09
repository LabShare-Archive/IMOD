/*
 *  imodsortsurf: sort contours into surfaces based on their mesh connections
 *
 *  Author: David Mastronarde,  mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

$Date$

$Revision$

Log at end
*/

#include <string.h>
#include "imodel.h"
#include "b3dutil.h"

int imodObjSortSurf(Iobj *obj);
int imodSplitSurfsToObjs(Imod *mod, int ob, int keepColor, int keepSurf);

static void usage(char *prog)
{
  imodVersion(prog);
  imodCopyright();
  fprintf(stderr, "Usage: %s [options] input_model output_model\n", prog);
  fprintf(stderr, "Options:\n");
  fprintf(stderr, "\t-o list\tList of objects to sort (ranges allowed)\n");
  fprintf(stderr, "\t-s\tSplit surfaces into new objects\n");
  fprintf(stderr, "\t-e\tUse existing surface numbers instead of sorting "
          "from the mesh\n");
  fprintf(stderr, "\t-c\tMake new objects the same color as source object\n");
  fprintf(stderr, "\t-k\tKeep surface numbers after moving to new objects\n");
  exit(3);
}

int main(int argc, char **argv)
{
  Imod *inModel;
  Iobj *obj;
  int ob, objdo;
  int *list;
  int i, nlist, numObj, numBefore;
  char *progname = imodProgName(argv[0]);
  int splitToObj = 0;
  int keepColor = 0;
  int keepSurf = 0;
  int existingSurf = 0;
    
  if (argc < 3){
    usage(progname);
  }
  nlist = 0;
  for (i = 1; i < argc; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){
      case 'o':
        list = parselist(argv[++i], &nlist);
        if (!list) {
          fprintf(stderr, "ERROR: %s - parsing object list\n", progname);
          exit(1);
        } 
        break;

      case 's':
        splitToObj = 1;
        break;
      case 'e':
        existingSurf = 1;
        break;
      case 'c':
        keepColor = 1;
        break;
      case 'k':
        keepSurf = 1;
        break;

      default:
        fprintf(stderr, "ERROR: %s - Invalid option %s\n", progname, argv[i]);
        exit(3);
        break;
      }

    }else{
      break;
    }
  }

  if (i != argc - 2) {
    fprintf(stderr, "ERROR: %s - Command line should end with two non-option "
            "arguments\n", progname);
    usage(progname);
  }

  if (!splitToObj && (existingSurf || keepColor || keepSurf))
    fprintf(stderr, "WARNING: %s - -e, -c, and -k have no effect when not "
            "splitting into objects\n", progname);

  inModel = imodRead(argv[argc-2]);
  if (!inModel) {
    fprintf(stderr, "ERROR: %s - Reading model\n", progname);
    exit(1);
  }

  numObj = inModel->objsize;
  for (ob = 0; ob < numObj; ob++) {
    objdo = 1;
    if (nlist) {
      objdo = 0;
      for (i = 0; i < nlist; i++)
        if (list[i] == ob + 1)
          objdo = 1;
    }
    if (objdo) {
      obj = &inModel->obj[ob];
      if (existingSurf && splitToObj) {
        if (imodSplitSurfsToObjs(inModel, ob, keepColor, keepSurf)) {
          fprintf(stderr, "ERROR: %s - Moving contours in object %d "
                  "to new objects\n", progname, ob + 1);
          exit(1);
        }
      } else {

        if (obj->meshsize) {
          if (obj->surfsize)
            printf("Object %d already has surface information which will"
                   " be replaced\n", ob + 1);
          if (imodObjSortSurf(obj) ) 
            printf("Error sorting object %d\n", ob + 1);
          else {
            if (splitToObj) {
              numBefore = inModel->objsize;
              if (imodSplitSurfsToObjs(inModel, ob, keepColor, keepSurf)) {
                fprintf(stderr, "ERROR: %s - Moving contours in object %d "
                        "to new objects\n", progname, ob + 1);
                exit(1);
              }
              printf("Object %d sorted into %d objects\n",ob + 1,
                     inModel->objsize + 1 - numBefore);
            } else {
              printf("Object %d sorted into %d surfaces\n",ob + 1,
                     obj->surfsize);
            }
          }
        } else
          printf("Object %d has no mesh data and cannot be sorted\n", ob + 1);
      }
    }
  } 

  imodBackupFile(argv[argc - 1]);
  if (imodOpenFile(argv[argc - 1], "wb", inModel)) {
    fprintf(stderr, "ERROR: %s - Cannot open new model file\n", progname);
    exit (1);
  }
  if (imodWriteFile(inModel)) {
    fprintf(stderr, "ERROR: %s - Writing to new model file\n", progname);
    exit (1);
  }
  exit(0);
}



int imodObjSortSurf(Iobj *obj)
{
  int npoly, me, i, j, poly, ninpoly, iwork, nwork, scan, found;
  int refvert, work, nsurfs, co, pt, ind, resol, vertBase, normAdd;
  int *listp, *surfs, *meshes, *nverts, *towork, *refp, *scanp, *lincs;
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
      for (i = 0; i < obj->mesh[me].lsize; i++) {
        if (*listp == IMOD_MESH_BGNPOLYNORM || 
            *listp == IMOD_MESH_BGNPOLYNORM2)
          npoly++;
        listp++;
      }
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
  lincs = (int *)malloc ((npoly + 2) * sizeof(int));
     
  if (!zmins || !zmaxs || !surfs || !meshes || !starts || !nverts || 
      !towork || !lincs) {
    if (zmins) free(zmins);
    if (zmaxs) free(zmaxs);
    if (meshes) free(meshes);
    if (surfs) free(surfs);
    if (starts) free(starts);
    if (nverts) free(nverts);
    if (towork) free(towork);
    if (lincs) free(lincs);

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
      if (imodMeshPolyNormFactors(listp[i++], &lincs[poly], &vertBase,
                                  &normAdd)) {
        zmin = vertp[listp[i + vertBase]].z;
        zmax = zmin;
        surfs[poly] = 0;
        meshes[poly] = me;
        starts[poly] = listp + i + vertBase; /* point to first vert ind */
        ninpoly = 0;
        while (listp[i] != IMOD_MESH_ENDPOLY) {
          ind = listp[i + vertBase];
          i += lincs[poly];
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
              refvert = *refp;
              refp += lincs[work];
              scanp = starts[scan];
              for (j = 0; j < nverts[scan]; j++) {
                if (*scanp == refvert) {
                  found = 1;
                  break;
                }
                scanp += lincs[scan];
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

  obj->surfsize = 0;
  for (co = 0; co < obj->contsize; co++) {
    found = 0;
    obj->cont[co].surf = 0;
    for (pt = 0; pt < obj->cont[co].psize && !found; pt++) {
      ptx = obj->cont[co].pts[pt].x;
      pty = obj->cont[co].pts[pt].y;
      ptz = obj->cont[co].pts[pt].z;
      for (poly = 0; poly < npoly && !found; poly++) {
        if (ptz >= zmins[poly] && ptz <= zmaxs[poly]) {
          vertp = obj->mesh[meshes[poly]].vert;
          scanp = starts[poly];
          for (j = 0; j < nverts[poly]; j++) {
            ind = *scanp;
            if (vertp[ind].x == ptx && vertp[ind].y == pty && 
                vertp[ind].z == ptz) {
              found = 1;
              obj->cont[co].surf = surfs[poly];
              if (surfs[poly] > obj->surfsize)
                obj->surfsize = surfs[poly];
              break;
            }
            scanp += lincs[poly];
          }
        }
      }
    } 
  }
  free(zmins);
  free(zmaxs);
  free(meshes);
  free(surfs);
  free(starts);
  free(nverts);
  free(towork);
  free(lincs);
  imodObjectCleanSurf(obj);
  return (0);
}

/* 
 * Splits the different surfaces in an object into new objects, giving them 
 * the same color as the original object if keepColor is nonzero, and
 * retaining the surface numbers if keepSurf is nonzero.  Returns 1 for error.
 */
int imodSplitSurfsToObjs(Imod *mod, int ob, int keepColor, int keepSurf)
{
  int surf, co, found, first = 0;
  Icont *cont;
  Iobj *newObj;
  Iobj *obj = &mod->obj[ob];

  /* Loop on surface numbers and see if there are any contours at it */
  for (surf = 0; surf <= obj->surfsize; surf++) {
    found = 0;
    for (co = 0; co < obj->contsize; co++) {
      if (obj->cont[co].surf == surf) {
        found = 1;
        break;
      }
    }

    if (found) {
      if (first) {

        /* After the first surface found, get a new object */
        if (imodNewObject(mod))
          return 1;
        newObj = &mod->obj[mod->objsize - 1];
        obj = &mod->obj[ob];
        if (keepColor) {
          newObj->red = obj->red;
          newObj->green = obj->green;
          newObj->blue = obj->blue;
        }

        /* Copy as many things as make sense */
        newObj->flags = obj->flags;
        newObj->pdrawsize = obj->pdrawsize;
        newObj->symbol = obj->symbol;
        newObj->symsize = obj->symsize;
        newObj->linewidth2 = obj->linewidth2;
        newObj->linewidth = obj->linewidth;
        newObj->symflags = obj->symflags;
        newObj->trans = obj->trans;
        memcpy(&newObj->ambient, &obj->ambient, 16);

        /* Loop backwards on contours so they can be removed */
        for (co = obj->contsize - 1; co >= 0; co--) {
          cont = &obj->cont[co];
          if (cont->surf == surf) {
            /* Just add contour to new object then remove from old; this will
               transfer all the pointers to data.  Copy contour store items
               after adding contour.  */
            if (!keepSurf)
              cont->surf = 0;
            if (imodObjectAddContour(newObj, cont) < 0)
              return 1;
            if (istoreCopyContSurfItems(obj->store, &newObj->store, co,
                                        newObj->contsize - 1, 0))
              return 1;
            if (imodObjectRemoveContour(obj, co))
              return 1;
          }
        }

        /* Now copy store items for surface # and remove from object */
        if (istoreCountContSurfItems(obj->store, surf, 1)) {
          if (istoreCopyContSurfItems(obj->store, &newObj->store, 
                                      surf, keepSurf ? surf : 0, 1))
            return 1;
          istoreDeleteContSurf(obj->store, surf, 1);
        }

        imodObjectCleanSurf(newObj);

      } else if (!keepSurf) {

        /* For the first surface, eliminate surface # there too if there are
           no surface store items */
        if (!istoreCountContSurfItems(obj->store, surf, 1))
          for (co = 0; co < obj->contsize; co++)
            if (obj->cont[co].surf == surf)
              obj->cont[co].surf = 0;
      }
      first = 1;
    }
  }
  imodObjectCleanSurf(obj);
  return 0;
}

/*
$Log$
Revision 3.11  2006/06/26 14:48:49  mast
Added b3dutil include for parselist

Revision 3.10  2005/09/11 19:22:54  mast
Changed for new mesh style and managed general store items

Revision 3.9  2005/06/03 20:07:50  mast
Copied many object properties to new objecta

Revision 3.8  2005/05/27 04:54:17  mast
Added clean surf call to end of sorting routine

Revision 3.7  2005/05/27 04:50:37  mast
Added ability to sort into different objects.

Revision 3.6  2005/02/11 01:42:33  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.5  2005/01/28 19:32:41  mast
Make it zero out existing surface information to avoid empty surfaces

Revision 3.4  2004/11/05 19:05:29  mast
Include local files with quotes, not brackets

Revision 3.3  2004/07/07 19:25:30  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.2  2003/10/24 03:05:24  mast
open as binary, strip program name and/or use routine for backup file

Revision 3.1  2003/02/21 23:16:15  mast
Open output file in binary mode

*/
