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
    Revision 3.1  2002/01/28 16:17:27  mast
    Fixed report of illegal object number

*/


#include <imodel.h>

static void usage()
{
    fprintf(stderr, "Usage: imodextract <list of objects> <input model> <output model>\n");
    fprintf(stderr, "       The list of objects can include ranges, e.g. 1-3,6,9,13-15\n");
    exit(-1);
}

int *parselist (char *line, int *nlist);

int main(int argc, char **argv)
{
    Imod *inModel;
    int ob, nob;
    int *list;
    int i, nlist, origsize;
    char backname[257];
    
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

/*    for(ob = inModel->objsize - 1; ob >= 0; ob--){
      found = 0;
      for (i = 0; i < nlist; i++) {
	if (list[i] == ob + 1) found = 1;
      }
      if(!found)
	imodDeleteObject(inModel, ob);
    } */

/* Make nlist new objects, then shift all existing objects to top */
    origsize = inModel->objsize;
    for (i = 0; i < nlist; i++)
      imodNewObject(inModel);
    for (ob = origsize - 1; ob >= 0; ob--)
      imodObjectCopy(&inModel->obj[ob], &inModel->obj[ob+nlist]);

    /* Now copy all of the selected ones into place */
    for (i = 0; i < nlist; i++) {
      ob = list[i] - 1;
      if (ob < 0 || ob >= origsize) {
	fprintf(stderr, "imodextract: Invalid object number %d\n", ob + 1);
	exit (1);
      }
      imodObjectCopy(&inModel->obj[ob + nlist], &inModel->obj[i]);
    }

    /* Delete extra objects by just setting objsize, set current indexes to -1
       to avoid problems */
    inModel->objsize = nlist;
    inModel->cindex.point  = -1;
    inModel->cindex.contour = -1;
    inModel->cindex.object = 0;

    sprintf(backname, "%s~", argv[argc - 1]);
    rename (argv[argc - 1], backname);
    if (imodOpenFile(argv[argc - 1], "wb", inModel)) {
      fprintf(stderr, "imodextract: Fatal error opening new model\n");
      exit (1);
    }
    imodWriteFile(inModel);
    exit(0);
}
