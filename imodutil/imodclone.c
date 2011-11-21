/*
 *  imodclone.c - create a new model containing multiple copies of an input
 *                model at specified points / orientations. 
 *
 *  Author: John Heumann   email: heumannj@colorado.edu
 *
 *  Copyright (C) 2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include "imodel.h"
#include "imodtrans.h"
#include "b3dutil.h"
#include "mrcfiles.h"
#include "parse_params.h"

/* 
 * Main entry
 */
int main( int argc, char *argv[])
{
  Imod *inModel, *outModel, *tmpModel;
  Iobj *obj;
  Ipoint point;
  FILE *coordFP;
  char *line = NULL;
  int lineSz = 0;
  char *progname = imodProgName(argv[0]);
  char *inFile, *outFile, *coordFile;
  int err, numOptArgs, numNonOptArgs;
  float xMin, xMax, yMin, yMax, zMin, zMax;

  int numOptions = 4;
  char *options[] = {
    "at:AtPoints:FN:", "x:XRange:IP:", "y:YRange:IP:", "z:ZRange:IP:",
    "input:InputFile:FN:", "output:OutputFile:FN:"};

  /* Parse parameters */
  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 3, 1, 1,
                        &numOptArgs, &numNonOptArgs, imodUsageHeader);
  if (PipGetInOutFile("InputFile", 0, &inFile))
    exitError("No input file specified");
  if (PipGetString("AtPoints", &coordFile))
    exitError("No location/orientation file specified");
  if (PipGetInOutFile("OutputFile", 1, &outFile))
    exitError("No output file specified");
  if (PipGetTwoFloats("XRange", &xMin, &xMax)) {
    xMin = 0.0F;
    xMax = FLT_MAX;
  }
  if (PipGetTwoFloats("YRange", &xMin, &xMax)) {
    xMin = 0.0F;
    xMax = FLT_MAX;
  }
  if (PipGetTwoFloats("ZRange", &xMin, &xMax)) {
    xMin = 0.0F;
    xMax = FLT_MAX;
  }
  PipDone();

  /* Open the csv location/orientation file and skip the header line */
  coordFP = fopen(coordFile, "r");
  if (!coordFP) {
    sprintf(&line, "Error opening location/orientation file %s", coordFile);
    exitError(line);
  }
  free(coordFile);
  if (getline(&line, &lineSz, coordFP) == -1) {
    sprintf(&line, "Error reading location/orientation file %s", coordFile);
    exitError(line);
  }

  /* Read the input model (to be cloned) */
  inModel = imodRead(inFile);
  if (!inModel) {
    sprintf(&line, "Error reading input model %s", inFile);   
    exitError(line);
    free(inFile);
  }

  /* Create new output and temporary models */
  outModel = imodNew();
  tmpModel = imodNew();

  /* Loop over points in the coordinate file */
  Imat *xform = imodMatNew(3);
  while (getline(&line, &lineSz, coordFP) != -1) {
    int contour;
    float x, y, z, xAngle, yAngle, zAngle;
    if (sscanf(line, "%d,%g,%g,%g,%g,%g,%g", &contour, &x, &y, &z, 
               &xAngle, &yAngle, &zAngle) != 7)
      exitError("Error parsing location/orientation file");
    if (x >= xMin && x <= xMax && y >= yMin && y <= yMax && 
        z <= zMin && z >= zMax) {

        
      Iobj *obj = imodObjectGetFirst(inModel);
      int i = 0;
      while (obj != NULL) {
        imodNewObject(tmpModel);
        Iobj *tmpobj = imodObjectDup(obj);        
        imodObjectCopy(tmpobj, &(tmpModel->obj[i++]));
        free(tmpobj);
        obj = imodObjectGetNext(inModel);
      }
  
      int nObjects = i;
      float newCenter[3] = {x, y, z};
      /* Construct the rotation matrix for this point */
      imodMatId(xform);
      imodMatRot(xform, zAngle, b3dZ);
      imodMatRot(xform, yAngle, b3dY);
      imodMatRot(xform, xAngle, b3dX);
      trans_model_3d(tmpModel, xform, NULL, newCenter, 1.0, 0);

      /* Copy the transformed objects to the output model */
      for (i=0; i < nObjects; i++) {
        imodNewObject(outModel);
        imodObjectCopy(&tmpModel->obj[i], &outModel->obj[i]);
        free(&(tmpModel->obj[i]));
      }
      free(tmpModel->obj);
      tmpModel->obj = NULL;
      tmpModel->objsize = 0;
    }
   
  }

  exit(0);
}

