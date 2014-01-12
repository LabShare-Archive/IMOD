/*
 *  imodchopcont.c - Chops up contours from patch tracking or separation by color
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
#include <map>
#include "imodel.h"
#include "b3dutil.h"
#include "parse_params.h"

// Types for the template items used here
typedef std::map<double,int> DIMap;
typedef std::pair<double,int> DIPair;

// Macro to get one integer from a DrawProps color
#define RGB_VALUE(props) (B3DNINT(255. * props.red) << 16) + \
(B3DNINT(255. * props.green) << 8) + B3DNINT(255. * props.blue)

/*
 * A very modest class, too small to require a .h file yet
 */
class ChopConts
{
public:
  ChopConts();
  void main( int argc, char *argv[]);
 private:

  // Methods
  void transferToSurfOrObj(Iobj *obj, int curColor, int ptBase, int ind, int finish);
  int dupBreakAddCont(Icont *oldCont, Iobj *obj, int indStart, int indEnd, int surf);
  void copyFineGrain(Iobj *oldOld, Iobj *newObj, int lastNewInd, int numCont,
                     int skipColor);
  
  // Member Variables
  Imod *mModel;
  DIMap mColorMap;
  double mSurfFac;
  int mAssignSurf;
  int mNumAfter;
};

/* 
 * Main entry
 */
int main( int argc, char *argv[])
{
  ChopConts chop;
  chop.main(argc, argv);
  exit(0);
}

// Class constructor
ChopConts::ChopConts()
{
  mSurfFac = pow(2., 25.);
  mAssignSurf = 0;
}

/*
 * Class main entry
 */
void ChopConts::main( int argc, char *argv[])
{
  Iobj *obj;
  Icont *contmp;
  int lenContour, numContour, numObj = 0;
  int *objList = NULL;
  int minContOverlap = 4;
  int noOverlap = 0;
  int breakAtColor = 0;
  int ierr, obNum, surf, numToCut, maxLen, co, lenConts, numCont, lapTotal, lapBase;
  int lapRemainder, ptBase, newCo, lastNewInd, ind, ipnt, numObjOrig, numObjTmp;
  int numBefore, curColor, color, coNum, maxSurfOrObj, ptNum, ob;
  Istore store, *storePtr;
  DIMap::iterator iter;
  DrawProps props, dfltProps, contProps;
  int contState, surfState, firstInd, afterInd;
  double mapKey;
  char *listString;
  char *progname = imodProgName(argv[0]);
  char *filename;
  int numOptArgs, numNonOptArgs;
  
  // Fallbacks from    ../manpages/autodoc2man 2 1 imodchopconts
  int numOptions = 7;
  const char *options[] = {
    "input:InputModel:FN:", "output:OutputModel:FN:", "length:LengthOfPieces:I:",
    "overlap:MinimumOverlap:I:", "number:NumberOfPieces:I:",
    "surfaces:AssignSurfaces:B:", "objects:ObjectsToDo:LI:"};

  /* Startup with fallback */
  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 
                        2, 1, 1, &numOptArgs, &numNonOptArgs, imodUsageHeader);

  /* Get input and output files */
  if (PipGetInOutFile("InputModel", 0, &filename))
      exitError("No input model file specified");

  mModel = imodRead(filename);
  if (!mModel) 
    exitError("Reading model %s", filename);
  free(filename);

  if (PipGetInOutFile("OutputModel", 1, &filename))
      exitError("No output model file specified");

  // Get object list if any
  if (!PipGetString("ObjectsToDo", &listString)) {
    objList = parselist(listString, &numObj);
    free(listString);
    if (!objList)
      exitError("Bad entry in list of objects to do");
  }

  PipGetInteger("BreakAtColors", &breakAtColor);
  if (breakAtColor > 0) {
    if (!PipGetBoolean("AssignSurfaces", &mAssignSurf) || 
        !PipGetInteger("LengthOfPieces", &lenContour) ||
        !PipGetInteger("MinimumOverlap", &minContOverlap) ||
        !PipGetInteger("NumberOfPieces", &numContour))
      exitError("You cannot enter -length, -number, -overlap, or -surfaces with -colors");
    mAssignSurf = breakAtColor == 1;
  }

  // Determine maximum length of contours
        maxLen = 1;
  for (obNum = 0; obNum < mModel->objsize; obNum++) {
    if (!numberInList(obNum + 1, objList, numObj, 1))
      continue;
    obj = &mModel->obj[obNum];
    for (co = 0; co < obj->contsize; co++)
      maxLen = B3DMAX(maxLen, obj->cont[co].psize);
  }

  // Set up default contour length as max of Z size of model and max contour length
  lenContour = B3DMAX(mModel->zmax, maxLen);

  PipGetBoolean("AssignSurfaces", &mAssignSurf);

  // Get the length entry
  ierr = PipGetInteger("LengthOfPieces", &lenContour);
  if (lenContour == -1)
    lenContour = B3DMAX(16, mModel->zmax / 5);
  if (lenContour <= 0)
    exitError("New contour length must be a positive number");

  // Set new default of 0 for overlap if length is 1, then get overlap and process it
  if (lenContour == 1)
    minContOverlap = 0;
  ierr = PipGetInteger("MinimumOverlap", &minContOverlap);
  if (minContOverlap == -1) {
    noOverlap = 1;
    minContOverlap = 0;
  }
  if (minContOverlap < -1) 
    exitError("Contour overlap cannot be negative, other than -1 to enforce 0 overlap");

  // Then process number option
  if (!PipGetInteger("NumberOfPieces", &numContour)) {
    if (!ierr)
      exitError("You cannot enter both -length and -number");
    
    // Derive length for new contours
    lenContour = B3DNINT((maxLen + (numContour - 1) * minContOverlap) / numContour);
    printf("Maximum contour length = %d, length for new contours = %d\n", maxLen, 
           lenContour);
  }
  if (lenContour <= minContOverlap || (lenContour > 1 && lenContour < minContOverlap + 2))
    exitError("Contour length must be greater than the overlap%s", 
              lenContour > 1 ? " + 1" : "");

  // Loop on objects
  numBefore = 0;
  mNumAfter = 0;
  numObjOrig = mModel->objsize;
  for (obNum = 0; obNum < numObjOrig; obNum++) {
    if (!numberInList(obNum + 1, objList, numObj, 1))
      continue;
    obj = &mModel->obj[obNum];
    numToCut = obj->contsize;
    numBefore += numToCut;

    if (breakAtColor <= 0) {
      surf = 1;
      maxLen = 0;

      // Find maximum contour length and if it is less than the length to cut, skip 
      // cutting
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
          lastNewInd = dupBreakAddCont(obj->cont, obj, -1, -1, mAssignSurf ? surf : -1);
        } else {

          // Otherwise  loop on new contours, make a duplicate and use break function
          // to get the contour and any associated data.  This takes care of fine-grained
          // contour data
          ptBase = 0;
          for (newCo = 0; newCo < numCont; newCo++) {
            ind = ptBase + lenConts - 1;
            if (noOverlap && newCo < lapRemainder)
              ind++;
            lastNewInd = dupBreakAddCont(obj->cont, obj, ptBase, ind,
                                         mAssignSurf ? surf : -1);

            // Advance the base
            ptBase += lenConts - lapBase;
            if (newCo < lapRemainder)
              ptBase += (noOverlap ? 1 : -1);
          }
        }

        // Copy any fine-grained object data for contour 0 for each new contour
        copyFineGrain(obj, obj, lastNewInd, numCont, 0);

        // Clear out the contour and delete it
        imodContourClear(obj->cont);
        imodObjectRemoveContour(obj, 0);
        surf++;
      }
      obj->flags |= IMOD_OBJFLAG_THICK_CONT;
      mNumAfter += obj->contsize;
    } else {

      istoreDefaultDrawProps(obj, &dfltProps);
      color = RGB_VALUE(dfltProps);

      // Get starting object or surface number for new surfs/objs, and start the map
      // with the index that needs to be used for contours at base color
      mColorMap.clear();
      if (mAssignSurf) {
        imodObjectCleanSurf(obj);
        maxSurfOrObj = obj->surfsize + 1;
        mColorMap.insert(DIPair((double)color, maxSurfOrObj));
        maxSurfOrObj++;
      } else {
        maxSurfOrObj = mModel->objsize;
        mColorMap.insert(DIPair((double)color, obNum));
      }

      // Loop on contours and find all the colors
      for (coNum = 0; coNum < numToCut; coNum++) {
        contmp = &obj->cont[coNum];
        istoreContSurfDrawProps(obj->store, &dfltProps, &contProps, coNum, contmp->surf,
                                &contState, &surfState);
        for (ptNum = 0; ptNum < contmp->psize; ptNum++) {
          istoreListPointProps(contmp->store, &contProps, &props, ptNum);
          color = RGB_VALUE(props);
          mapKey = color;
          if (mAssignSurf)
            mapKey += contmp->surf * mSurfFac;
          if (!mColorMap.count(mapKey)) {
            mColorMap.insert(DIPair(mapKey, maxSurfOrObj));
            maxSurfOrObj++;
          }
        }
      }

      // Add surface colors 
      if (mAssignSurf) {
        for (iter = mColorMap.begin(); iter != mColorMap.end(); iter++) {
          mapKey = iter->first;
          surf = B3DNINT(mapKey) / mSurfFac;
          color = B3DNINT(mapKey - surf * mSurfFac);
          store.type = GEN_STORE_COLOR;
          store.index.i = iter->second;
          store.flags = GEN_STORE_SURFACE;
          store.value.i = 0;
          store.value.b[0] = color >> 16;
          store.value.b[1] = (color >> 8) & 255;
          store.value.b[2] = color & 255;
          if (istoreInsert(&obj->store, &store))
            exitError("Adding fine-grained surface data");

          // Duplicate non-color surface data for this surface into the new surface
          firstInd = istoreLookup(obj->store, surf, &afterInd);
          if (firstInd >= 0) {
            for (ind = firstInd; ind < afterInd; ind++) {
              storePtr = istoreItem(obj->store, ind);
              if ((storePtr->flags & GEN_STORE_SURFACE) && 
                  storePtr->type != GEN_STORE_COLOR) {
                store = *storePtr;
                store.index.i = iter->second;;
                if (istoreInsert(&obj->store, &store))
                  exitError("Adding fine-grained surface data");
              }
            }
          }
        }
      } else {

        // Or add the model objects
        numObjTmp = mModel->objsize;
        for (ob = numObjTmp; ob < maxSurfOrObj; ob++) {
          if (imodNewObject(mModel))
            exitError("Adding object to model");
          imodObjectCopyClear(&mModel->obj[obNum], &mModel->obj[ob]);
        }
        obj = &mModel->obj[obNum];

        // Assign the colors
        for (iter = mColorMap.begin(); iter != mColorMap.end(); iter++) {
          color = B3DNINT(iter->first);
          ob = iter->second;
          if (ob != obNum) {
            mModel->obj[ob].red = (color >> 16) / 255.;
            mModel->obj[ob].green = ((color >> 8) & 255) / 255.;
            mModel->obj[ob].blue = (color & 255) / 255.;
          }
        }

        // Copy any non-color surface data to new objects
        for (ind = 0; ind < ilistSize(obj->store); ind++) {
          storePtr = istoreItem(obj->store, ind);
          if ((storePtr->flags & GEN_STORE_SURFACE) && 
              storePtr->type != GEN_STORE_COLOR) {
            for (ob = numObjTmp; ob < maxSurfOrObj; ob++)
              if (istoreInsert(&mModel->obj[ob].store, storePtr))
                exitError("Transferring fine-grained surface data");
          }
        }
      }

      // Loop on contours to cut them up.  Each time, the contour being cut is contour 0
      // Do not assign a cont pointer because it can change; be careful with obj pointer
      for (coNum = 0; coNum < numToCut; coNum++) {
        istoreContSurfDrawProps(obj->store, &dfltProps, &contProps, 0, 
                                obj->cont->surf, &contState, &surfState);
        istoreListPointProps(obj->cont->store, &contProps, &props, 0);
        curColor = RGB_VALUE(props);
        ptBase = 0;
        for (ptNum = 1; ptNum < obj->cont->psize; ptNum++) {
          istoreListPointProps(obj->cont->store, &contProps, &props, ptNum);
          color = RGB_VALUE(props);
          if (color != curColor) {
            ind = ptNum;
            if (iobjScat(obj->flags))
              ind--;
            transferToSurfOrObj(obj, curColor, ptBase, ind, 0);
            ptBase = ptNum;
            curColor = color;
          }
        }

        // Deal with the points left at the end: move whole contour to end if the final
        // points are the whole thing, otherwise make final segment
        transferToSurfOrObj(obj, curColor, ptBase, ptNum - 1, 1);

        // Clear out the contour and delete it
        imodContourClear(obj->cont);
        imodObjectRemoveContour(obj, 0);
      }        
    }
  }
  imodBackupFile(filename);
  if (imodOpenFile(filename, "wb", mModel))
    exitError("Opening new model %s", filename);
  imodWriteFile(mModel);
  printf("Number of contours in selected objects changed from %d to %d\n", numBefore, 
         mNumAfter);
  exit(0);
}

/*
 * Does the common tasks of transferring a contour segment to a new surface or object
 * for the current color, either an internal segment (finish = 0) or a final segment
 * (finish = 1)
 */
void ChopConts::transferToSurfOrObj(Iobj *obj, int curColor, int ptBase, int ind, 
                                    int finish)
{
  int surf, lastNewInd;
  Iobj *newObj;
  double mapKey = curColor;
  if (mAssignSurf)
    mapKey += mSurfFac * obj->cont->surf;
  surf = mColorMap.find(mapKey)->second;
  newObj = !mAssignSurf ? &mModel->obj[surf] : obj;
  if (finish && !ptBase)
    lastNewInd = dupBreakAddCont(obj->cont, newObj, -1, -1, mAssignSurf ? surf : -1);
  else
    lastNewInd = dupBreakAddCont(obj->cont, newObj, ptBase, ind, mAssignSurf ? surf : -1);
  istoreClearRange(newObj->cont[lastNewInd].store, GEN_STORE_COLOR, 0, 
                   newObj->cont[lastNewInd].psize - 1);

  // Copy non-color fine-grained object data for contour 0 for each new contour
  // one at a time, since they may be in different objects
  copyFineGrain(obj, newObj, lastNewInd, 1, 1);
  mNumAfter++;
}

/*
 * Duplicates a contour, extracts just the segment between indStart and indEnd inclusive
 * if these are both non-negative, and adds the whole or segment to the given object.
 * Assigns the surface as surf if it is non-negative.
 */
int ChopConts::dupBreakAddCont(Icont *oldCont, Iobj *obj, int indStart, int indEnd,
                              int surf)
{
  Icont *newCont, *dupCont;
  int lastNewInd;
  dupCont = imodContourDup(oldCont);
  if (!dupCont)
    exitError("Duplicating contour");
  if (indStart >= 0 && indEnd >= 0) {
    newCont = imodContourBreak(dupCont, indStart, indEnd);
    if (!newCont)
      exitError("Breaking out piece of contour");
    imodContourDelete(dupCont);
  } else {
    newCont = dupCont;
  }
  if (surf >= 0)
    newCont->surf = surf;
  lastNewInd = imodObjectAddContour(obj, newCont);
  if (lastNewInd < 0)
    exitError("Adding new contour to object");
  free(newCont);
  return lastNewInd;
}

/*
 * Copy any fine-grained object data for contour 0 in the old object to each new contour
 * in the new object, numCont contours ending with lastNewInd.  If skipColor is nozero,
 * it does not transfer color data.
 */
void ChopConts::copyFineGrain(Iobj *oldObj, Iobj *newObj, int lastNewInd, int numCont,
                          int skipColor)
{
  int firstInd, afterInd, coInd, ind;
  Istore store, *storePtr;
  
  firstInd = istoreLookup(oldObj->store, 0, &afterInd);
  if (firstInd >= 0) {
    for (ind = firstInd; ind < afterInd; ind++) {
      storePtr = istoreItem(oldObj->store, ind);
      if ((skipColor && storePtr->type == GEN_STORE_COLOR) || 
          (storePtr->flags & GEN_STORE_SURFACE))
        continue;
      store = *storePtr;
      for (coInd = lastNewInd + 1 - numCont; coInd <= lastNewInd; coInd++) {
        store.index.i = coInd;
        if (istoreInsert(&newObj->store, &store))
          exitError("Adding fine-grained contour data");
      }
    }
  }
}
