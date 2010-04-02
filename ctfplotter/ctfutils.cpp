/*
* ctfutils.cpp - Utility functions shared with ctfphaseflip
*
*  Author: David Mastronarde
*
*  Copyright (C) 2010 by Boulder Laboratory for 3-Dimensional Electron
*  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
*  Colorado.  See dist/COPYRIGHT for full copyright notice.
* 
*  $Id$
*  Log at end of file
*/

#include <stdio.h>
#include <math.h>
#include "b3dutil.h"
#include "ilist.h"
#include "ctfutils.h"
#include "parse_params.h"

#define MAX_LINE 100

/*
 * Reads a file of tilt angles whose name is in angleFile, and which must
 * have at least mNzz lines.  mAngleSign should be 1., or -1. to invert the
 * sign of the angles.  Minimum and maximum angles are returned in mMinAngle
 * and mMaxAngle.  The return value is the array of tilt angles.
 */
float *readTiltAngles(const char *angleFile, int mNzz, float mAngleSign,
                      float &mMinAngle, float &mMaxAngle)
{
  char angleStr[MAX_LINE];
  FILE *fpAngle;
  int k, err;
  float currAngle;
  float *mTiltAngles = (float *)malloc(mNzz * sizeof(float));
  mMinAngle = 10000.;
  mMaxAngle = -10000.;
  if (!mTiltAngles)
    exitError("Allocating array for tilt angles");
  if ((fpAngle=fopen(angleFile, "r")) == 0)
    exitError("Opening tilt angle file %s", angleFile);
  for (k = 0; k < mNzz; k++) {
    do {
      err = fgetline(fpAngle, angleStr, MAX_LINE);
    } while (err == 0);
    if (err == -1 || err == -2) 
      exitError("%seading tilt angle file %s\n", 
                err == -1 ? "R" : "End of file while r", angleFile);
    sscanf(angleStr, "%f", &currAngle);
    currAngle *= mAngleSign;
    mMinAngle = B3DMIN(mMinAngle, currAngle);
    mMaxAngle = B3DMAX(mMaxAngle, currAngle);
    mTiltAngles[k] = currAngle;
  }
  fclose(fpAngle);
  return mTiltAngles;
}

/*
 * Reads a defocus file whose name is in mFnDefocus and stores the values
 * in an Ilist of SavedDefocus structures, eliminating duplications if any.
 * The return value is the Ilist, which may be empty if the file does not
 * exist.
 */
Ilist *readDefocusFile(const char *mFnDefocus)
{
  FILE *fp;
  SavedDefocus saved;
  char line[MAX_LINE];
  int nchar;
  float langtmp, hangtmp, defoctmp;
  Ilist *mSaved = ilistNew(sizeof(SavedDefocus), 10);
  if (!mSaved)
    exitError("Allocating list for angles and defocuses");
  fp = fopen(mFnDefocus, "r");
  if (fp) {
    while(1) {
      nchar = fgetline(fp, line, MAX_LINE);
      if (nchar == -2)
        break;
      if (nchar == -1)
        exitError("Error reading defocus file %s", mFnDefocus);
      if (nchar) {
        sscanf(line, "%d %d %f %f %f", &saved.startingSlice, &saved.endingSlice
               , &langtmp, &hangtmp, &defoctmp);
        saved.lAngle = langtmp;
        saved.hAngle = hangtmp;
        saved.defocus = defoctmp / 1000.;
        saved.startingSlice--;
        saved.endingSlice--;
        addItemToDefocusList(mSaved, saved);
      }
      if (nchar < 0)
        break;
    }
    fclose(fp);
  }
  return mSaved;
}

/*
 * Adds one item to the defocus list, keeping the list in order and
 * and avoiding duplicate starting and ending view numbers
 */
void addItemToDefocusList(Ilist *mSaved, SavedDefocus toSave)
{
  SavedDefocus *item;
  int i, matchInd = -1, insertInd = 0;

  // Look for match or place to insert
  for (i = 0; i < ilistSize(mSaved); i++) {
    item = (SavedDefocus *)ilistItem(mSaved, i);
    if (item->startingSlice == toSave.startingSlice && 
        item->endingSlice == toSave.endingSlice) {
      matchInd = i;
      *item = toSave;
      break;
    }
    if (item->lAngle + item->hAngle <= toSave.lAngle + toSave.hAngle)
      insertInd = i + 1;
  }
  
  // If no match, now insert
  if (matchInd < 0 && ilistInsert(mSaved, &toSave, insertInd))
    exitError("Failed to add item to list of angles and defocuses");
}

/*
 * Analyzes the list of angular ranges and defocuses to see if the view
 * numbers are correct, using the nz tilt angles in angles (which can be NULL
 * if there are no tilt angles).  If it detects that the views are low by one,
 * it will adjust them.  If there are other inconsistencies, it returns 1.
 */
int checkAndFixDefocusList(Ilist *list, float *angles, int nz)
{
  SavedDefocus *item;
  int allEqual = 1, allOffByOne = 1;
  int minStart = 10000, maxEnd = -100;
  int i, k, start, end, startAmbig, endAmbig;
  float tol = 0.02f;
  for (k = 0; k < ilistSize(list); k++) {
    item = (SavedDefocus *)ilistItem(list, k);
    minStart = B3DMIN(minStart, item->startingSlice);
    maxEnd = B3DMAX(maxEnd, item->endingSlice);
    startAmbig = 0;
    endAmbig = 0;
    if (nz == 1 || !angles) {
      start = 0;
      end = 0;
      /*printf("item %d  start %d end %d low %f high %f  has to start %d "
             "end %d\n", k, item->startingSlice, item->endingSlice,
             item->lAngle, item->hAngle, start, end); */
    } else {
      start = -1;
      end = -1;

      // Find first and last slice whose angle is within the range
      for (i = 0; i < nz; i++) {

        if (item->lAngle <= angles[i] && angles[i] <= item->hAngle) {
          if (start < 0)
            start = i;
          end = i;
        }

        // But also see if there could be ambiguity about an angle near one
        // end of the range
        if (fabs((double)(item->lAngle - angles[i])) < tol) {
          if (angles[0] <= angles[nz-1])
            startAmbig = 1;
          else
            endAmbig = 1;
          /*printf("Angle %d, %f ambiguous to low angle %f, start %d end %d\n"
            ,i, angles[i], item->lAngle, startAmbig, endAmbig); */
        }
        if (fabs((double)(item->hAngle - angles[i])) < tol) {
          if (angles[0] <= angles[nz-1])
            endAmbig = 1;
          else
            startAmbig = 1;
          /*printf("Angle %d, %f ambiguous to high angle %f, start %d end %d\n"
            ,i, angles[i], item->hAngle, startAmbig, endAmbig);*/
        }
      }
      /*printf("item %d  start %d end %d low %f high %f  implied start %d "
             "end %d\n", k, item->startingSlice, item->endingSlice,
             item->lAngle, item->hAngle, start, end); */

      // If can't find, skip this one and clear both flags
      if (start < 0 || end < 0) {
        allEqual = 0;
        allOffByOne = 0;
        continue;
      }
    }
    if ((start != item->startingSlice && !startAmbig) || 
        (end != item->endingSlice && !endAmbig))
      allEqual = 0;
    if ((start != item->startingSlice + 1 && !startAmbig) || 
        (end != item->endingSlice + 1 && !endAmbig))
      allOffByOne = 0;
  }

  // If we didn't find out anything, see if min and max are at least
  // consistent with the bug, and go ahead and adjust
  if (allEqual && allOffByOne && minStart == -1 && maxEnd < nz - 1)
    allEqual = 0;

  // Adjust them all if all off by one as far as we can tell
  if (!allEqual || allOffByOne) {
    printf("View numbers in defocus file appear to be low by 1;\n"
           "  adding 1 to correct for old Ctfplotter bug\n");
    for (k = 0; k < ilistSize(list); k++) {
      item = (SavedDefocus *)ilistItem(list, k);
      item->startingSlice++;
      item->endingSlice++;
    }
  }  
  if ((!allEqual && allOffByOne) || (allEqual && !allOffByOne))
    return 0;
  return 1;
}

/*

$Log$
Revision 1.1  2010/04/02 00:18:39  mast
Added to provide common functions for adjusting for old bug in ctfplotter


*/
