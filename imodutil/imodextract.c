/*  IMOD VERSION 2.67
 *
 *  imodextract.c  -  Program to extract a list of objects from a model
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
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
Revision 3.5  2004/07/07 19:25:30  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 3.4  2003/10/24 03:05:23  mast
open as binary, strip program name and/or use routine for backup file

Revision 3.3  2003/07/31 21:42:46  mast
Extract object views for the particular objects being extracted

Revision 3.2  2003/02/21 23:18:44  mast
Open output file in binary mode

Revision 3.1  2002/01/28 16:17:27  mast
Fixed report of illegal object number

*/


#include "imodel.h"

static void usage()
{
  fprintf(stderr, "Usage: imodextract <list of objects> <input model> <output model>\n");
  fprintf(stderr, "       The list of objects can include ranges, e.g. 1-3,6,9,13-15\n");
  exit(3);
}

int *parselist (char *line, int *nlist);

int main(int argc, char **argv)
{
  Imod *inModel;
  int ob, nob, iview;
  int *list;
  int i, nlist, origsize;
    
  if (argc != 4) usage();
    
  list = parselist(argv[1], &nlist);
  if (!list) {
    fprintf(stderr, "imodextract: Error parsing object list\n");
    exit(1);
  }

  inModel = imodRead(argv[2]);
  if (!inModel) {
    fprintf(stderr, "imodextract: Fatal error reading model\n");
    exit(1);
  }


  /* Make nlist new objects */
  origsize = inModel->objsize;
  for (i = 0; i < nlist; i++)
    imodNewObject(inModel);

  /* Extent the object views then shift all existing objects and their views
     to top */
  imodObjviewComplete(inModel);
  for (ob = origsize - 1; ob >= 0; ob--) {
    imodObjectCopy(&inModel->obj[ob], &inModel->obj[ob+nlist]);
    for (iview = 1; iview < inModel->viewsize; iview++)
      inModel->view[iview].objview[ob + nlist] = 
        inModel->view[iview].objview[ob];
  }

  /* Now copy all of the selected ones into place, copying the view down 
     too */
  for (i = 0; i < nlist; i++) {
    ob = list[i] - 1;
    if (ob < 0 || ob >= origsize) {
      fprintf(stderr, "imodextract: Invalid object number %d\n", ob + 1);
      exit (1);
    }
    imodObjectCopy(&inModel->obj[ob + nlist], &inModel->obj[i]);
    for (iview = 1; iview < inModel->viewsize; iview++)
      inModel->view[iview].objview[i] = 
        inModel->view[iview].objview[ob + nlist];
  }

  /* Delete extra objects by just setting objsize, and the object view sizes,
     set current indexes to -1 to avoid problems */
  inModel->objsize = nlist;
  inModel->cindex.point  = -1;
  inModel->cindex.contour = -1;
  inModel->cindex.object = 0;
  for (iview = 1; iview < inModel->viewsize; iview++)
    inModel->view[iview].objvsize = nlist;
      

  imodBackupFile(argv[argc - 1]);
  if (imodOpenFile(argv[argc - 1], "wb", inModel)) {
    fprintf(stderr, "imodextract: Fatal error opening new model\n");
    exit (1);
  }
  imodWriteFile(inModel);
  exit(0);
}
