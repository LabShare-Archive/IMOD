/*  IMOD VERSION 2.67
 *
 *  imodjoin.c  -  Program to extract a list of objects from a model
 *
 *  Authors: James Kremer and David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Fine    *
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
Revision 3.6  2004/07/07 19:25:30  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.5  2004/06/16 21:06:42  mast
Forgot to delete copies of library functions from debugging

Revision 3.4  2004/06/11 02:29:59  mast
Fixed bug with trying to use the zero-th view when there are no real views

Revision 3.3  2003/10/24 03:05:24  mast
open as binary, strip program name and/or use routine for backup file

Revision 3.2  2003/07/31 21:45:47  mast
Transfer object views appropriately from each model file, add new
views from later files if they have more views than the first file,
and stop big memory leaks by deleting contours and meshes of unused
objects.

Revision 3.1  2003/02/21 23:18:03  mast
Open output file in binary mode

*/

#include "imodel.h"
int *parselist (char *line, int *nlist);

static void usage()
{
  fprintf(stderr, "Usage:\nimodjoin [-o list | -r list] model_1 [-o list]"
          " model_2 [more models] out_model\n");
  fprintf(stderr, "       Each list of objects can include ranges, e.g. 1-3,6,9,13-15\n");
  fprintf(stderr, "       -o will select particular objects from either model\n");
  fprintf(stderr, "       -r will REPLACE listed objects from model 1 with objects from model 2\n");
  exit(3);
}

static void parserr(int mod)
{
  fprintf(stderr, "imodjoin: Error parsing object list before model %d\n", 
          mod);
  usage();
}
static void optionerr(int mod)
{
  fprintf(stderr, "imodjoin: Invalid option before model %d\n", mod);
  usage();
}
static void doublerr(void)
{
  fprintf(stderr, "imodjoin: You cannot use both -o and -r with model 1\n");
  usage();
}
static void readerr(int mod)
{
  fprintf(stderr, "imodjoin: Error reading file for model %d\n", mod);
  exit(1);
}
static void objerr(int ob, int mod)
{
  fprintf(stderr, "imodjoin: Invalid object number %d for model %d\n", ob, 
          mod);
  exit (1);
}

int main(int argc, char **argv)
{
  Imod *inModel;
  Imod *joinModel;
  int ob, nob, i, origsize, iview, onlist;
  int *list1;
  int *list2;
  int nlist1 = 0;
  int nlist2;
  int njoin = 1;
  int replace = 0;
  int iarg = 1;
  char option;
    
  if (argc < 4) usage();

  if (*argv[iarg] == '-') {
      
    if ( (option = *(argv[iarg++] + 1)) == 'o' || option == 'r' ) {
      list1 = parselist(argv[iarg++], &nlist1);
      if (!list1)
        parserr(1);
      if (option == 'r')
        replace = 1;
    } else
      optionerr(1);
  }

  if (*argv[iarg] == '-') {
    if ((option = *(argv[iarg] + 1)) == 'o' || option == 'r' )
      doublerr();
    else
      optionerr(1);
  }

  inModel = imodRead(argv[iarg]); iarg++;
  if (!inModel) readerr(1);

  origsize = inModel->objsize;
  /* If there is a -o list on the first file, reorganize the retained 
     objects */
  if (nlist1 && !replace) {
    /* Make nlist new objects, then shift all existing objects to top */
    for (i = 0; i < nlist1; i++)
      imodNewObject(inModel);
    imodObjviewComplete(inModel);
    for (ob = origsize - 1; ob >= 0; ob--) {
      imodObjectCopy(&inModel->obj[ob], &inModel->obj[ob+nlist1]);
      for (iview = 1; iview < inModel->viewsize; iview++)
        inModel->view[iview].objview[ob + nlist1] = 
          inModel->view[iview].objview[ob];
    }

    /* Now copy all of the selected ones into place */
    for (i = 0; i < nlist1; i++) {
      ob = list1[i] - 1;
      if (ob < 0 || ob >= origsize)
        objerr(ob + 1, 1);

      imodObjectCopy(&inModel->obj[ob + nlist1], &inModel->obj[i]);
      for (iview = 1; iview < inModel->viewsize; iview++)
        inModel->view[iview].objview[i] = 
          inModel->view[iview].objview[ob + nlist1];
    }

    /* Delete extra objects that were not copied - avoid big memory leak */
    for (ob = origsize + nlist1 - 1; ob >= nlist1; ob--) {
      onlist = 0;
      for (i = 0; i < nlist1; i++)
        if (list1[i] - 1 == ob - nlist1)
          onlist = 1;
      if (!onlist)
        imodFreeObject(inModel, ob);
    }

    /* Eliminate extra objects by just setting objsize; leak a little */
    inModel->objsize = nlist1;
    for (iview = 1; iview < inModel->viewsize; iview++)
      inModel->view[iview].objvsize = nlist1;
  }
    
  /* process arguments, read model, add objects to first model for one
     or more models */
  do {
    njoin++;

    if (iarg + 1 >= argc) usage();

    if (njoin > 2 && replace) {
      fprintf(stderr, "imodjoin: You cannot use -r with more than 2 "
              "input models\n");
      exit(1);
    }

    nlist2 = 0;
    if (*argv[iarg] == '-') {
      if (iarg + 3 >= argc) usage();
      if ( (option = *(argv[iarg++] + 1)) == 'o' ) {
        list2 = parselist(argv[iarg++], &nlist2);
        if (!list2)
          parserr(njoin);
      } else
        optionerr(njoin);
    }

    joinModel = imodRead(argv[iarg]); iarg++;
    if (!joinModel) readerr(njoin);

    /* If no list for second model, make simple list of all objects */
    if (!nlist2) {
      nlist2 = joinModel->objsize;
      list2 = (int *)malloc(nlist2 * sizeof(int));
      for (i = 0; i < nlist2; i++)
        list2[i] = i + 1;
    }

    /* If there are more views in this model, add the extra views to the
     output model */
    if (joinModel->viewsize > inModel->viewsize) {
      while (joinModel->viewsize > inModel->viewsize) {
        imodViewModelNew(inModel);
        inModel->view[inModel->viewsize - 1] = 
          joinModel->view[inModel->viewsize - 1];
        inModel->view[inModel->viewsize - 1].objvsize = 0;
        inModel->view[inModel->viewsize - 1].objview = NULL;
      }
      imodObjviewComplete(inModel);
    }

    /* Now go through objects in second file, copying selected ones with 
       or without replacement */
    for (i = 0; i < nlist2; i++) {
      ob = list2[i] - 1;
      if (ob < 0 || ob >= joinModel->objsize)
        objerr(ob+1, njoin);
      if (replace && (i < nlist1)) {
        nob = list1[i] - 1;
        if (nob < 0 || nob >= origsize) 
          objerr(nob+1, 1);

        /* delete contours and meshes of object being replaced */
        if (inModel->obj[nob].contsize)
          imodContoursDelete(inModel->obj[nob].cont, 
                             inModel->obj[nob].contsize);
        if (inModel->obj[nob].meshsize)
          imodMeshesDelete(inModel->obj[nob].mesh, inModel->obj[nob].meshsize);

      } else {
        nob = inModel->objsize;
        imodNewObject(inModel);
      }
      imodObjectCopy(&joinModel->obj[ob], &inModel->obj[nob]);
      imodObjviewComplete(inModel);

      /* For each view, if the view exists in the joining model and the
         object view exists for this object, copy object view; otherwise
         copy current object properties to the object view */
      for (iview = 1; iview < inModel->viewsize; iview++) {
        if (iview < joinModel->viewsize && 
            ob < joinModel->view[iview].objvsize) {
          inModel->view[iview].objview[nob] = 
            joinModel->view[iview].objview[ob];
        } else
          imodObjviewFromObject(&inModel->obj[nob], 
                                &inModel->view[iview].objview[nob]);
      }
    }

    /* We can't free the model because data were transferred from it.
       Delete extra objects that were not copied - avoid big memory leak */
    for (ob = joinModel->objsize - 1; ob >= 0; ob--) {
      onlist = 0;
      for (i = 0; i < nlist2; i++)
        if (list2[i] - 1 == ob)
          onlist = 1;
      if (!onlist)
        imodFreeObject(joinModel, ob);
    }

    free(list2);
  } while (iarg + 1 < argc);

  /* Synchronize object data to current view values if there are any 
     real views*/
  if (!inModel->cview && inModel->viewsize > 1)
    inModel->cview = 1;
  if (inModel->cview)
    imodViewUse(inModel);

  /* set current indexes to -1 to avoid problems */
  inModel->cindex.point  = -1;
  inModel->cindex.contour = -1;
  inModel->cindex.object = 0;

  imodBackupFile(argv[argc - 1]);
  if (imodOpenFile(argv[argc - 1], "wb", inModel)) {
    fprintf(stderr, "imodjoin: Fatal error opening new model\n");
    exit (1);
  }
  imodWriteFile(inModel);
  exit(0);
}
