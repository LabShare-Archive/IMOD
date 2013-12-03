/*
 *  imodchopcont.c - Chops up contours from patch tracking
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2013 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */

#include <string.h>
#include "imodel.h"
#include "b3dutil.h"
#include "parse_params.h"

/* 
 * Main entry
 */
int main( int argc, char *argv[])
{
  Imod *model;
  Iobj *obj;
  Icont *newCont, *dupCont;
  int lenContour, numContour, numObj = 0;
  int *objList = NULL;
  int minContOverlap = 4;
  int noOverlap = 0;
  int assignSurf = 0;
  int ierr, obNum, surf, numToCut, maxLen, co, lenConts, numCont, lapTotal, lapBase;
  int lapRemainder, ptBase, newCo, lastNewInd, firstInd, afterInd, ind, coInd, ipnt;
  int numBefore, numAfter;
  Istore *storePtr;
  Istore store;
  char *listString;
  char *progname = imodProgName(argv[0]);
  char *filename;
  int numOptArgs, numNonOptArgs;
  
  // Fallbacks from    ../manpages/autodoc2man 2 1 imodchopconts
  int numOptions = 6;
  const char *options[] = {
    "input:InputFile:FN:", "output:OutputFile:FN:", "length:LengthAndOverlap:IP:",
    "number:NumberAndOverlap:IP:", "surfaces:AssignSurfaces:B:",
    "objects:ObjectsToDo:LI:"};

  /* Startup with fallback */
  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 
                        2, 1, 1, &numOptArgs, &numNonOptArgs, imodUsageHeader);

  /* Get input and output files */
  if (PipGetInOutFile("InputFile", 0, &filename))
      exitError("No input file specified");

  model = imodRead(filename);
  if (!model) 
    exitError("Reading model %s", filename);
  free(filename);

  if (PipGetInOutFile("OutputFile", 1, &filename))
      exitError("No output file specified");

  // Set up default contour length as Z size of model
  lenContour = model->zmax;

  // Get object list if any
  if (!PipGetString("ObjectsToDo", &listString)) {
    objList = parselist(listString, &numObj);
    free(listString);
    if (!objList)
      exitError("Bad entry in list of objects to do");
  }

  /* Process other options */
  PipGetBoolean("AssignSurfaces", &assignSurf);
  ierr = PipGetTwoIntegers("LengthAndOverlap", &lenContour, &minContOverlap);
  if (!PipGetTwoIntegers("NumberAndOverlap", &numContour, &minContOverlap)) {
    if (!ierr)
      exitError("You cannot enter both -length and -overlap");
    
    // Determine maximum length of contours
    maxLen = 1;
    for (obNum = 0; obNum < model->objsize; obNum++) {
      if (!numberInList(obNum + 1, objList, numObj, 1))
        continue;
      obj = &model->obj[obNum];
      for (co = 0; co < obj->contsize; co++)
        maxLen = B3DMAX(maxLen, obj->cont[co].psize);
    }

    // Derive length for new contours
    lenContour = B3DNINT((maxLen + (numContour - 1) * B3DMAX(0, minContOverlap)) /
                         numContour);
    printf("Maximum contour length = %d, length for new contours = %d\n", maxLen, 
           lenContour);
  }
  if (minContOverlap == -1) {
    noOverlap = 1;
    minContOverlap = 0;
  }
  if (minContOverlap < -1) 
    exitError("Contour overlap cannot be negative");
  if (lenContour < minContOverlap + 2)
    exitError("Contour length must be greater than the overlap + 1");

  // Loop on objects
  numBefore = 0;
  numAfter = 0;
  for (obNum = 0; obNum < model->objsize; obNum++) {
    if (!numberInList(obNum + 1, objList, numObj, 1))
      continue;
    obj = &model->obj[obNum];
    numToCut = obj->contsize;
    numBefore += numToCut;
    surf = 1;
    maxLen = 0;

    // Find maximum contour length and if it is less than the length to cut, skip cutting
    for (co = 0; co < numToCut; co++)
      maxLen = B3DMAX(maxLen, obj->cont[co].psize);
    if (maxLen <= lenContour)
      numToCut = 0;

    // Loop on contours, each one is the first because it is deleted when done
    for (co = 0; co < numToCut; co++) {

      // Set up the cutting as in tiltxcorr
      ipnt = obj->cont->psize;
      lenConts = B3DMIN(lenContour, ipnt);
      numCont = (ipnt - 1) / (lenConts - minContOverlap) + 1;
      lapTotal = numCont * lenConts - ipnt;
      lapBase = lapTotal / B3DMAX(numCont - 1, 1);
      lapRemainder = lapTotal % B3DMAX(numCont - 1, 1);

      if (noOverlap) {
        lenConts = ipnt / numCont;
        lapRemainder = ipnt % numCont;
        lapBase = 0;
      }

      // If only one contour, duplicate it and add it to the end
      if (numCont == 1) {
        newCont = imodContourDup(obj->cont);
        if (!newCont)
          exitError("Duplicating contour");
        if (assignSurf)
          newCont->surf = surf;
        lastNewInd = imodObjectAddContour(obj, newCont);
        if (lastNewInd < 0)
          exitError("Adding new contour to object");
        free(newCont);
      } else {

        // Otherwise  loop on new contours, make a duplicate and use break function
        // to get the contour and any associated data.  This takes care of fine-grained
        // contour data
        ptBase = 0;
        for (newCo = 0; newCo < numCont; newCo++) {
          dupCont = imodContourDup(obj->cont);
          if (!dupCont)
            exitError("Duplicating contour");
          ind = ptBase + lenConts - 1;
          if (noOverlap && newCo < lapRemainder)
            ind++;
          newCont = imodContourBreak(dupCont, ptBase, ind);
          if (!newCont)
            exitError("Breaking out piece of contour");
          imodContourDelete(dupCont);
          if (assignSurf)
            newCont->surf = surf;
          lastNewInd = imodObjectAddContour(obj, newCont);
          if (lastNewInd < 0)
            exitError("Adding new contour to object");
          free(newCont);

          // Advance the base
          ptBase += lenConts - lapBase;
          if (newCo < lapRemainder)
            ptBase += (noOverlap ? 1 : -1);
        }
      }

      // Copy any fine-grained object data for contour 0 for each new contour
      firstInd = istoreLookup(obj->store, 0, &afterInd);
      if (firstInd >= 0) {
        for (ind = firstInd; ind < afterInd; ind++) {
          storePtr = istoreItem(obj->store, ind);
          store = *storePtr;
          for (coInd = lastNewInd + 1 - numCont; coInd <= lastNewInd; coInd++) {
            store.index.i = coInd;
            if (istoreInsert(&obj->store, &store))
              exitError("Adding fine-grained contour data");
          }
        }
      }

      // Clear out the contour and delete it
      imodContourClear(obj->cont);
      imodObjectRemoveContour(obj, 0);
      surf++;
    }
    obj->flags |= IMOD_OBJFLAG_THICK_CONT;
    numAfter += obj->contsize;
  }
  imodBackupFile(filename);
  if (imodOpenFile(filename, "wb", model))
    exitError("Opening new model %s", filename);
  imodWriteFile(model);
  printf("Number of contours in selected objects changed from %d to %d\n", numBefore, 
         numAfter);
  exit(0);
}
