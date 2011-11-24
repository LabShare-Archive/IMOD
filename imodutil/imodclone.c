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
#include "b3dutil.h"
#include "mrcfiles.h"
#include "parse_params.h"

/* 
 * Main entry
 */
#define lineSz 1024
int main( int argc, char *argv[])
{
  Imod *inModel, *outModel, *tmpModel;
  FILE *coordFP, *outFP;
  char line[lineSz];
  char msg[256];
  char *progname = imodProgName(argv[0]);
  char *inFile, *outFile, *coordFile;
  int iOutObj = 0, numOptArgs, numNonOptArgs;
  float xMin, xMax, yMin, yMax, zMin, zMax;
  Imat *xform;

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
  if (PipGetTwoFloats("YRange", &yMin, &yMax)) {
    yMin = 0.0F;
    yMax = FLT_MAX;
  }
  if (PipGetTwoFloats("ZRange", &zMin, &zMax)) {
    zMin = 0.0F;
    zMax = FLT_MAX;
  }
  PipDone();

  /* Open the csv location/orientation file and skip the header line */
  coordFP = fopen(coordFile, "r");
  if (!coordFP) {
    sprintf(msg, "Error opening location/orientation file:\n%s", coordFile);
    exitError(msg);
  }
  free(coordFile);
  if (fgets(line, lineSz, coordFP) == NULL) {
    sprintf(msg, "Error reading location/orientation file :\n%s", coordFile);
    exitError(msg);
  }

  /* Read the input model (to be cloned) */
  inModel = imodRead(inFile);
  if (!inModel) {
    sprintf(msg, "Error reading input model:\n%s", inFile);   
    exitError(msg);
    free(inFile);
  }

  /* Create new output and temporary models */
  outModel = imodNew();
  tmpModel = imodNew();
  /* Set tmpModel max coords. Rotation center will be midpoint */
  tmpModel->xmax = inModel->xmax;
  tmpModel->ymax = inModel->ymax;
  tmpModel->zmax = inModel->zmax;

  /* Loop over points in the coordinate file */
  xform = imodMatNew(3);
  while (fgets(line, lineSz, coordFP)) {
    int contour;
    float x, y, z, xAngle, yAngle, zAngle;
    if (sscanf(line, "%d,%g,%g,%g,%g,%g,%g", &contour, &x, &y, &z, 
               &xAngle, &yAngle, &zAngle) != 7)
      exitError("Error parsing location/orientation file");
    if (x >= xMin && x <= xMax && y >= yMin && y <= yMax && 
        z >= zMin && z <= zMax) {

      /* Copy all the objects from the input to the temp model */        
      Iobj *obj = imodObjectGetFirst(inModel);
      Ipoint newCenter;
      int i = 0;
      while (obj != NULL) {
        Iobj *tmpobj = imodObjectDup(obj);
        imodNewObject(tmpModel);
        imodObjectCopy(tmpobj, &(tmpModel->obj[i++]));
        free(tmpobj);
        obj = imodObjectGetNext(inModel);
      }

      newCenter.x = x;
      newCenter.y = y;
      newCenter.z = z;
      /* Construct the rotation matrix for this point */
      imodMatId(xform);
      imodMatRot(xform, zAngle, b3dZ);
      imodMatRot(xform, yAngle, b3dY);
      imodMatRot(xform, xAngle, b3dX);

      /* Transform the temp model */
      imodTransModel3D(tmpModel, xform, NULL, newCenter, 1.0, 0);

      /* Copy the transformed objects to the output model */
      obj = imodObjectGetFirst(tmpModel);
      while (obj != NULL) {
        imodNewObject(outModel);
        imodObjectCopy(obj, &outModel->obj[iOutObj++]);
        obj = imodObjectGetNext(tmpModel);
      }
      /* Prepare temp model for reuse */
      free(tmpModel->obj);
      tmpModel->obj = NULL;
      tmpModel->objsize = 0;
    }
   
  }
  fclose(coordFP);
  imodMatDelete(xform);

  outFP = fopen(outFile, "w");
  if (!outFP) {
    sprintf(msg, "Error writing output file:\n%s", outFile);
    exitError(msg);
  }
  imodWrite(outModel, outFP);
  fclose(outFP);

  exit(0);
}

