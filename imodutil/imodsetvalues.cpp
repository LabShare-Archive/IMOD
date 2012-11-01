/*
 *  imodsetvalues.cpp - set per point or per contour values in a model.
 *
 *  Author: John Heumann   email: heumannj@colorado.edu
 *
 *  Copyright (C) 2012 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */
#include <stdio.h>
#include "b3dutil.h"
#include "imodel.h"
#include "istore.h"
#include "parse_params.h"
#include <set>

using namespace std;

// Forward declarations
int countInputLines(FILE *fp);
int countModelPoints(Imod *model);

// Main entry point
int main( int argc, char *argv[])
{
  const char *progname = imodProgName(argv[0]);
  char *inFile, *outFile, *valueFile, *minMaxFile = NULL;
  FILE *valFP = NULL, *minMaxFP = NULL;
  float f1, f2, f3, f4;
  int iContNum, iErr, iObjNum, iOldObjNum = -1, iPointNum, iView;
  int nContours, nPoints, nOptArgs, nNonOptArgs;
  Icont *contour;
  Iobj *object;
  Iobjview *view;
  Istore store;
  set<int> objectsModified;

  const int nOptions = 4;
    const char *options[] = {"input:InputFile:FN:", 
                             "output:OutputFile:FN:", 
                             "values:ValuesFile:FN:", 
                             "minMax:MinMaxFile:FN:"};

  // Parse parameters 
  PipReadOrParseOptions(argc, argv, options, nOptions, progname, 3, 2, 
                        1, &nOptArgs, &nNonOptArgs, imodUsageHeader);
  if (PipGetInOutFile("ValueFile", 0, &valueFile))
    exitError("No value file specified");
  if (PipGetInOutFile((char *)"InputFile", 1, &inFile))
    exitError("No input file specified");
  if (PipGetInOutFile("OutputFile", 2, &outFile))
    exitError("No output file specified");
  if (!PipGetString("MinMaxFile", &minMaxFile)) {
    minMaxFP = fopen(minMaxFile, "r");
    if (!minMaxFP) {
      exitError("Error opening min/max file %s!", minMaxFile);
    }
  }
  PipDone();

  // Open the value file count the number of values per line
  valFP = fopen(valueFile, "r");
  if (!valFP)
    exitError("Error opening value file %s!", valueFile);

  int nValuesPerLine = fscanf(valFP, "%g, %g, %g, %g \n",
                              &f1, &f2, &f3, &f4);
  rewind(valFP);

  // Open the input model and get number of objects and views
  Imod *model = imodRead(inFile);
  if (!model)
    exitError("Error opening input model %s", inFile);

  const int nObjects = imodGetMaxObject(model);
  const int nViews = model->viewsize;
  
  // We will be setting all views to pseudo-color using VALUE1. This is
  // part of what's required to do that.
  imodObjviewComplete(model);

  // Following store object settings will be re-used for all entries
  store.flags = GEN_STORE_FLOAT << 2;
  store.type = GEN_STORE_VALUE1;
  
  if (nValuesPerLine == 1) {
    // Set values for every point in the model

    // Sanity check: there must be exactly 1 value per model point.
    if (countInputLines(valFP) != countModelPoints(model))
      exitError("Number of values must match the number of model points!");

    // Loop ever all points in the model
    for (iObjNum = 0; iObjNum < nObjects; iObjNum++) {
      objectsModified.insert(iObjNum);
      object = &model->obj[iObjNum];
      nContours = object->contsize;
      for (iContNum = 0; iContNum < nContours; iContNum++) {
        contour = &object->cont[iContNum];
        nPoints = contour->psize;
        for (iPointNum = 0; iPointNum < nPoints; iPointNum++) {
          if (fscanf(valFP, "%g \n", &store.value.f) != 1)
            exitError("Error reading %s!", valueFile);
          store.index.i = iPointNum;
          // Apply the specified value only to a single point
          istoreInsertChange(&contour->store, &store);
          istoreEndChange(contour->store, GEN_STORE_VALUE1, iPointNum + 1);
        }
      }
    }
  }

  else if (nValuesPerLine == 3) {
    // Set values for the specified contours
    while ((iErr = fscanf(valFP, "%d, %d, %g \n", &iObjNum, &iContNum,
			  &store.value.f)) != EOF) {
      if (iErr != 3)
        exitError("Error reading %s!", valueFile);
      iObjNum -= 1;                // 0-based indexing in C/C++ routines
      iContNum -= 1;               // 1-based in values file
      if (iObjNum < 0 || iObjNum >= nObjects)
	exitError("Illegal object number %d in values file", iObjNum + 1);

      object = &model->obj[iObjNum];
      nContours = object->contsize;
      if (iObjNum != iOldObjNum) {
	objectsModified.insert(iObjNum);
	iOldObjNum = iObjNum;
      }
      if (iContNum < 0 || iContNum >= nContours) {
	exitError("Illegal object/contour %d/%d in values file",
		  iObjNum + 1, iContNum + 1);
      }
      store.index.i = iContNum;
      // Apply the specified value only to a single contour
      istoreInsertChange(&object->store, &store);
      istoreEndChange(object->store, GEN_STORE_VALUE1, iContNum + 1);
    }
  }

  else if (nValuesPerLine == 4) {
    // Set values for the specified points
    while ((iErr = fscanf(valFP, "%d, %d, %d, %g \n", &iObjNum, 
			  &iContNum, &iPointNum, &store.value.f)) != EOF) {
      if (iErr != 4)
	exitError("Error reading %s!", valueFile);
      iObjNum -= 1;             // 0-based indexing in C/C++ routines
      iContNum -= 1;            // 1-based in values file
      iPointNum -= 1;
      if (iObjNum < 0 || iObjNum >= nObjects)
	exitError("Illegal object number %d in values file!", iObjNum + 1);

      object = &model->obj[iObjNum];
      nContours = object->contsize;
      if (iObjNum != iOldObjNum) {
	objectsModified.insert(iObjNum);
	iOldObjNum = iObjNum;
      }
      if (iContNum < 0 || iContNum >= nContours) {
	exitError("Illegal object/contour %d/%d in values file!",
		  iObjNum + 1, iContNum + 1);
      }
      contour = &object->cont[iContNum];
      nPoints = contour->psize;
      if (iPointNum < 0 || iPointNum >= nPoints) {
	exitError("Illegal object/contour/point %d/%d/%d in values file!",
		  iObjNum + 1, iContNum + 1, iPointNum + 1);
      }
      store.index.i = iPointNum;
      // Apply the specified value only to a single point
      istoreInsertChange(&contour->store, &store);
      istoreEndChange(contour->store, GEN_STORE_VALUE1, iPointNum + 1);
    }
  }

  else if (nValuesPerLine == EOF) {
    // Values file is empty
    if (minMaxFP)
      ; // Special Case: allow this with user-defined min/max values.
    else
      exitError("Nothing to do... values file is empty!");
  } 

  else
    exitError("Values file must contain 1, 3, or 4 values per line!");

  // Loop over any modified objects updating min/max values and making
  // sure that all views are changed to pseudo-color mode.
  for (set<int>::iterator iObjNumPtr = objectsModified.begin();
       iObjNumPtr != objectsModified.end(); ++iObjNumPtr) {

    iObjNum = *iObjNumPtr;
    object = &model->obj[iObjNum];
    object->flags |= IMOD_OBJFLAG_MCOLOR | IMOD_OBJFLAG_USE_VALUE;
    istoreFindAddMinMax1(object);

    // Fix bad initialization of valwhite to 0 instead of 255 in some
    // earlier versions of IMOD.
    if (!object->valblack && !object->valwhite)
      object->valwhite = 255; 
    for (iView = 1; iView < nViews; iView++) {
      view = &model->view[iView].objview[iObjNum];
      view->flags |= IMOD_OBJFLAG_MCOLOR | IMOD_OBJFLAG_USE_VALUE;
      if (!view->valblack && !view->valwhite)
        view->valwhite = 255;
    }
  }

  // Apply user-supplied min/max values
  if (minMaxFP) {
    int nDone = 0;
    while ((iErr = fscanf(minMaxFP, "%d, %g, %g \n", &iObjNum, &f1, &f2))
	   != EOF) {
      if (iErr != 3)
        exitError("Error reading %s!", minMaxFile);
      iObjNum -= 1;
      if (iObjNum < 0 || iObjNum >= nObjects)
        exitError("Invalid object number %d in min/max file!", iObjNum + 1);
      object = &model->obj[iObjNum];
      istoreAddMinMax(&object->store, GEN_STORE_MINMAX1, f1, f2);
      nDone += 1;
    }
    if (nDone == 0)
      exitError("Nothing to do... values and min/max files are empty!");
  }

  // Finally, write the outupt model, backing up any previous version
  imodBackupFile(outFile);
  if (imodOpenFile(outFile, "wb", model))
    exitError("Error opening new model %s\n", outFile);
  imodWriteFile(model);
  printf("Wrote new model to file %s \n", outFile);

  free(inFile);
  free(outFile);
  if (minMaxFile)
   free(minMaxFile);
  exit(0);
}

/**********************************************************************/

int countInputLines(FILE *fp)
{
  int n = 0;
  while (fscanf(fp, "%*[^\n]\n") != EOF)
    n += 1;
  rewind(fp);

  return n;

}

/**********************************************************************/

int countModelPoints(Imod *model)
{
  int n = 0, iContNum, iObjNum;
  int nContours, nObjects = imodGetMaxObject(model);

  for (iObjNum = 0; iObjNum < nObjects; iObjNum++) {
    Iobj *object = &model->obj[iObjNum];
    nContours = object->contsize;
    for (iContNum = 0; iContNum < nContours; iContNum++) {
      Icont *contour = &object->cont[iContNum];
      n += contour->psize;
    }
  }

  return n;
}

/**********************************************************************/
