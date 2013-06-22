/*
 *  imodjoin.c  -  Joins selected objects from multiple models into one model
 *
 *  Authors: James Kremer and David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include "imodel.h"
#include "b3dutil.h"
#include "mrcfiles.h"
#include "parse_params.h"

static void usage()
{
  imodVersion("imodjoin");
  imodCopyright();
  printf("Usage: imodjoin [options] model_1 [-o list]"
          " model_2 [more models] out_model\n");
  printf("Options (before first model):\n");
  printf("\t-o list\tList of objects to take from particular model "
          "(default is all)\n");
  printf("\t-r list\tList of objects in model 1 to REPLACE with "
          "objects from model 2\n");
  printf("\t-c\tChange colors of objects being copied to first model"
          "\n");
  printf("\t-d\tModels are from different volumes\n");
  printf("\t-i file\tTransform all models to match this image file "
          "(implies -d)\n");
  printf("\t-s\tIgnore scale differences between models from "
          "different volumes\n");
  printf("\t-f\tRetain original flip state of each model\n");
  printf("\t-n\tDo not transforms models at all\n");
  /*printf("\t-\t\n");*/
  exit(3);
}

static void parserr(int mod)
{
  printf("ERROR: imodjoin - Error parsing object list before "
          "model %d\n", mod);
  usage();
}
static void optionerr(int mod)
{
  printf("ERROR: imodjoin - Invalid option before model %d\n", mod);
  usage();
}
static void doublerr(void)
{
  printf("ERROR: imodjoin - You cannot use both -o and -r with "
          "model 1\n");
  usage();
}
static void readerr(int mod)
{
  exitError("Error reading file for model %d", mod);
}
static void objerr(int ob, int mod)
{
  exitError("Invalid object number %d for model %d", ob, mod);
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
  int firstOlist = 0;
  int diffVols = 0;
  int iarg, flipState;
  char option;
  int noRef1 = 0;
  int suppress = 0;
  int keepScale = 0;
  int keepFlip = 0;
  int changeColors = 0;
  int setMax = 1;
  float rsave, gsave, bsave;
  IrefImage useRef, outRef, *modRefp;
  FILE *fin;
  MrcHeader hdata;
  Ipoint newmax;
  Ipoint unitPt = {1., 1., 1.};
  Ipoint zeroPt = {0., 0., 0.};
  char *progname = imodProgName(argv[0]);
  char prefix[100];
  sprintf(prefix, "ERROR: %s - ", progname);
  setExitPrefix(prefix);
    
  if (argc < 4) usage();

  for (iarg = 1; iarg < argc ; iarg++) {
    if (argv[iarg][0] == '-') {
      switch (argv[iarg][1]) {

      case 'r':
      case 'o':
        list1 = parselist(argv[++iarg], &nlist1);
        if (!list1)
          parserr(1);
        if (argv[iarg - 1][1] == 'r')
          replace = 1;
        else
          firstOlist = 1;
        break;

      case 'c':
        changeColors = 1;
        break;

      case 'd':
        diffVols = 1;
        break;

      case 's':
        keepScale = 1;
        break;

      case 'n':
        suppress = 1;
        break;

      case 'f':
        keepFlip = 1;
        break;

      case 'i':
        if (NULL == (fin = fopen(argv[++iarg], "rb")))
          exitError("Couldn't open %s", argv[iarg]);

        if (mrc_head_read(fin, &hdata)) 
          exitError("Reading header from %s", argv[iarg]);
        fclose(fin);
        diffVols = 2;
        break;

      default:
        optionerr(1);
        break;

      }
    } else
      break;
  }

  if (replace && firstOlist)
    doublerr();

  if (suppress && diffVols) {
    printf("ERROR: imodjoin - it makes no sense to use -n with -d or -i\n");
    usage();
  }

  inModel = imodRead(argv[iarg++]);
  if (!inModel) 
    readerr(1);

  /* If model has no refimage, set one up with null transformation */
  modRefp = inModel->refImage;
  if (!modRefp) {
    noRef1 = 1;
    inModel->refImage = (IrefImage *) malloc (sizeof(IrefImage));
    if (!inModel->refImage)
      exitError("Getting IrefImage structure for output model");

    printf("WARNING: Model 1 has no image reference data; "
            "transformations may be wrong\n");
    modRefp = inModel->refImage;
    modRefp->otrans = zeroPt;
    modRefp->ctrans = zeroPt;
    modRefp->crot = zeroPt;
    modRefp->cscale = unitPt;
    inModel->flags |= IMODF_OTRANS_ORIGIN;
  }

  /* Set up transformations */
  if (diffVols) {
    if (diffVols > 1) {
      
      /* If reference image file, get the target transformation */
      outRef.ctrans.x = hdata.xorg;
      outRef.ctrans.y = hdata.yorg;
      outRef.ctrans.z = hdata.zorg;
      outRef.crot.x = hdata.tiltangles[3];
      outRef.crot.y = hdata.tiltangles[4];
      outRef.crot.z = hdata.tiltangles[5];
      outRef.cscale = unitPt;
      if (hdata.xlen && hdata.mx)
        outRef.cscale.x = hdata.xlen/(float)hdata.mx;
      if (hdata.ylen && hdata.my)
        outRef.cscale.y = hdata.ylen/(float)hdata.my;
      if (hdata.zlen && hdata.mz)
        outRef.cscale.z = hdata.zlen/(float)hdata.mz;

      /* If model had no refs, copy this into it to prevent transform */
      if (noRef1) {
        modRefp->ctrans = outRef.ctrans;
        modRefp->otrans = outRef.ctrans;
        modRefp->crot = outRef.crot;
        modRefp->cscale = outRef.cscale;
      }
    } else {

      /* No reference image, use model 1 as target with trans set to origin 
         if possible */
      if (inModel->flags | IMODF_OTRANS_ORIGIN)
        outRef.ctrans = modRefp->otrans;
      else
        outRef.ctrans = modRefp->ctrans;
      outRef.crot = modRefp->crot;
      outRef.cscale = modRefp->cscale;
    }

    /* Set up to transform to the reference however it was gotten; namely
       to a full image load coordinates, with rotations ignored */
    useRef.ctrans = zeroPt;
    useRef.cscale = outRef.cscale;
    useRef.crot = zeroPt;
    useRef.orot = zeroPt;
    useRef.oscale = modRefp->cscale;
    useRef.otrans = zeroPt;

    /* Use origin information or give warning */
    if (inModel->flags | IMODF_OTRANS_ORIGIN) {
      useRef.otrans.x = modRefp->ctrans.x - modRefp->otrans.x;
      useRef.otrans.y = modRefp->ctrans.y - modRefp->otrans.y;
      useRef.otrans.z = modRefp->ctrans.z - modRefp->otrans.z;
    } else
      printf("WARNING: Model 1 has no image origin data; "
              "transformations may be wrong\n");

    flipState = keepFlip ? (inModel->flags & IMODF_FLIPYZ) : 0;
    if (keepScale)
      useRef.cscale = useRef.oscale;
    imodTransFromRefImage(inModel, &useRef, unitPt);
    if (flipState) {
      imodFlipYZ(inModel);
      inModel->flags |= IMODF_FLIPYZ;
    }

    /* Set its refimage of the output model, set flags */
    outRef.otrans = outRef.ctrans;
    *modRefp = outRef;
    inModel->flags |= (IMODF_OTRANS_ORIGIN | IMODF_TILTOK);

  } else if (!suppress) {

    /* Models are supposedly from congruent volumes.  Do nothing to model 1, 
       it will be the full reference for current transform */
    useRef = *modRefp;
  }

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
        imodDeleteObject(inModel, ob);
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

    if (njoin > 2 && replace) 
      exitError("You cannot use -r with more than 2 input models");

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

    joinModel = imodRead(argv[iarg++]);
    if (!joinModel) 
      readerr(njoin);

    /* Set desired flip state to original state or model 1 state */
    flipState = (keepFlip ? joinModel->flags : inModel->flags) & IMODF_FLIPYZ;
    modRefp = joinModel->refImage;

    if (diffVols) {

      /* Different volumes: translate to full image load and transform by
         scaling differences only */
      if (modRefp) {
        useRef.otrans = zeroPt;

        /* Use origin information or give warning */
        if (joinModel->flags | IMODF_OTRANS_ORIGIN) {
          useRef.otrans.x = modRefp->ctrans.x - modRefp->otrans.x;
          useRef.otrans.y = modRefp->ctrans.y - modRefp->otrans.y;
          useRef.otrans.z = modRefp->ctrans.z - modRefp->otrans.z;
        } else
          printf("WARNING: Model %d has no image origin data; "
                  "transformations may be wrong\n", njoin);
        useRef.oscale = modRefp->cscale;
        if (keepScale)
          useRef.cscale = useRef.oscale;
        imodTransFromRefImage(joinModel, &useRef, unitPt);
      } else
        printf("WARNING: Model %d has no image reference data and "
                "will not be transformed\n", njoin);
      
    } else if (!suppress) {

      /* Same or congruent volumes: if model has refimage set its current
         values to old in the transformation reference and transform; 
         otherwise issue a warning */
      if (modRefp) {
        useRef.otrans = modRefp->ctrans;
        useRef.orot = modRefp->crot;
        useRef.oscale = modRefp->cscale;
        imodTransFromRefImage(joinModel, &useRef, unitPt);
      } else
        printf("WARNING: Model %d has no image reference data and "
                "will not be transformed\n", njoin);
    }

    /* Set flip to match either the original state or model 1 */
    if (flipState != (joinModel->flags & IMODF_FLIPYZ))
      imodFlipYZ(joinModel);

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
      rsave = inModel->obj[nob].red;
      gsave = inModel->obj[nob].green;
      bsave = inModel->obj[nob].blue;
      imodObjectCopy(&joinModel->obj[ob], &inModel->obj[nob]);

      /* Restore existing or new object color if selected */
      if (changeColors) {
        inModel->obj[nob].red = rsave;
        inModel->obj[nob].green = gsave ;
        inModel->obj[nob].blue = bsave;
      }
      imodObjviewComplete(inModel);

      /* For each view, if the view exists in the joining model and the
         object view exists for this object, copy object view; otherwise
         copy current object properties to the object view */
      for (iview = 1; iview < inModel->viewsize; iview++) {
        if (iview < joinModel->viewsize && 
            ob < joinModel->view[iview].objvsize) {
          inModel->view[iview].objview[nob] = 
            joinModel->view[iview].objview[ob];

          /* If changing colors and color was the same as that of the object,
             then set to restored color */
          if (changeColors && joinModel->view[iview].objview[ob].red == 
              joinModel->obj[ob].red && 
              joinModel->view[iview].objview[ob].green ==
              joinModel->obj[ob].green &&
              joinModel->view[iview].objview[ob].blue == 
              joinModel->obj[ob].blue) {
            inModel->view[iview].objview[nob].red = rsave;
            inModel->view[iview].objview[nob].green = gsave;
            inModel->view[iview].objview[nob].blue = bsave;
          }

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
        imodDeleteObject(joinModel, ob);
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

  /* Set max of model big enough to long the whole thing */
  if (setMax) {
    imodel_maxpt(inModel, &newmax);
    inModel->xmax = B3DMAX(inModel->xmax, newmax.x);
    inModel->ymax = B3DMAX(inModel->ymax, newmax.y);
    inModel->zmax = B3DMAX(inModel->zmax, newmax.z);
  }

  imodBackupFile(argv[argc - 1]);
  if (imodOpenFile(argv[argc - 1], "wb", inModel))
    exitError("Fatal error opening new model");
  imodWriteFile(inModel);
  exit(0);
}
