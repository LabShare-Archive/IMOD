/* Simple program to extract a list of objects from a model */

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
	fprintf(stderr, "imodextract: Invalid object number %d\n", ob);
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
    if (imodOpenFile(argv[argc - 1], "w", inModel)) {
      fprintf(stderr, "imodextract: Fatal error opening new model\n");
      exit (1);
    }
    imodWriteFile(inModel);
    exit(0);
}
