/*
 *  imodsortsurf: sort contours into surfaces based on their mesh connections
 *
 *  Author: David Mastronarde,  mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <string.h>
#include "imodel.h"
#include "b3dutil.h"

static int imodSplitSurfsToObjs(Imod *mod, int ob, int keepColor, int keepSurf);

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
          if (imodObjectSortSurf(obj) ) 
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

/* 
 * Splits the different surfaces in an object into new objects, giving them 
 * the same color as the original object if keepColor is nonzero, and
 * retaining the surface numbers if keepSurf is nonzero.  Returns 1 for error.
 */
static int imodSplitSurfsToObjs(Imod *mod, int ob, int keepColor, int keepSurf)
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
