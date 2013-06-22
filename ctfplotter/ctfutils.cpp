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
*/

#include <stdio.h>
#include <math.h>
#include "b3dutil.h"
#include "ilist.h"
#include "ctfutils.h"
#include "parse_params.h"

#define MAX_LINE 100

/*
 * Ruueads a file of tilt angles whose name is in angleFile, and which must
 * have at least nzz lines.  angleSign should be 1., or -1. to invert the
 * sign of the angles.  Minimum and maximum angles are returned in minAngle
 * and maxAngle.  The return value is the array of tilt angles.
 */
float *readTiltAngles(const char *angleFile, int nzz, float angleSign,
                      float &minAngle, float &maxAngle)
{
  char angleStr[MAX_LINE];
  FILE *fpAngle;
  int k, err;
  float currAngle;
  float *tiltAngles = (float *)malloc(nzz * sizeof(float));
  minAngle = 10000.;
  maxAngle = -10000.;
  if (!tiltAngles)
    exitError("Allocating array for tilt angles");
  if ((fpAngle = fopen(angleFile, "r")) == 0)
    exitError("Opening tilt angle file %s", angleFile);
  for (k = 0; k < nzz; k++) {
    do {
      err = fgetline(fpAngle, angleStr, MAX_LINE);
    } while (err == 0);
    if (err == -1 || err == -2)
      exitError("%seading tilt angle file %s\n",
                err == -1 ? "R" : "End of file while r", angleFile);
    sscanf(angleStr, "%f", &currAngle);
    currAngle *= angleSign;
    minAngle = B3DMIN(minAngle, currAngle);
    maxAngle = B3DMAX(maxAngle, currAngle);
    tiltAngles[k] = currAngle;
  }
  fclose(fpAngle);
  return tiltAngles;
}

/*
 * Reads a defocus file whose name is in fnDefocus and stores the values
 * in an Ilist of SavedDefocus structures, eliminating duplications if any.
 * The return value is the Ilist, which may be empty if the file does not
 * exist.
 */
Ilist *readDefocusFile(const char *fnDefocus, int &defVersion)
{
  FILE *fp;
  SavedDefocus saved;
  char line[MAX_LINE];
  int nchar, versTmp;
  float langtmp, hangtmp, defoctmp;
  Ilist *lstSaved = ilistNew(sizeof(SavedDefocus), 10);
  if (!lstSaved)
    exitError("Allocating list for angles and defocuses");
  fp = fopen(fnDefocus, "r");
  defVersion = 0;
  versTmp = 0;
  if (fp) {
    while (1) {
      nchar = fgetline(fp, line, MAX_LINE);
      if (nchar == -2)
        break;
      if (nchar == -1)
        exitError("Error reading defocus file %s", fnDefocus);
      if (nchar) {
        sscanf(line, "%d %d %f %f %f %d", &saved.startingSlice, &saved.endingSlice
               , &langtmp, &hangtmp, &defoctmp, &versTmp);
        if (!ilistSize(lstSaved))
          defVersion = versTmp;
        saved.lAngle = langtmp;
        saved.hAngle = hangtmp;
        saved.defocus = defoctmp / 1000.;
        saved.startingSlice--;
        saved.endingSlice--;
        addItemToDefocusList(lstSaved, saved);
      }
      if (nchar < 0)
        break;
    }
    fclose(fp);
  }
  return lstSaved;
}

/*
 * Adds one item to the defocus list, keeping the list in order and
 * and avoiding duplicate starting and ending view numbers
 */
void addItemToDefocusList(Ilist *lstSaved, SavedDefocus toSave)
{
  SavedDefocus *item;
  int i, matchInd = -1, insertInd = 0;

  // Look for match or place to insert
  for (i = 0; i < ilistSize(lstSaved); i++) {
    item = (SavedDefocus *)ilistItem(lstSaved, i);
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
  if (matchInd < 0 && ilistInsert(lstSaved, &toSave, insertInd))
    exitError("Failed to add item to list of angles and defocuses");
}

/*
 * Analyzes the list of angular ranges and defocuses to see if the view
 * numbers are correct, using the nz tilt angles in angles (which can be NULL
 * if there are no tilt angles).  If it detects that the views are low by one,
 * it will adjust them.  If there are other inconsistencies, it returns 1.
 */
int checkAndFixDefocusList(Ilist *list, float *angles, int nz, int defVersion)
{
  SavedDefocus *item;
  int allEqual = 1, allOffByOne = 1, numEmpty = 0;
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
             item->lAngle, item->hAngle, start, end);*/
    } else {
      start = -1;
      end = -1;

      // Find first and last slice whose angle is within the range
      for (i = 0; i < nz; i++) {

        if (item->lAngle - tol < angles[i] && angles[i] < item->hAngle + tol) {
          if (start < 0)
            start = i;
          end = i;
        }

        // But also see if there could be ambiguity about an angle near one
        // end of the range, provided this is not a single-image range
        if (!fabs(item->hAngle - item->lAngle) < tol) {
          if (fabs((double)(item->lAngle - angles[i])) < 1.01 * tol) {
            if (angles[0] <= angles[nz - 1])
              startAmbig = 1;
            else
              endAmbig = 1;
            /*printf("Angle %d, %f ambiguous to low angle %f, start %d end %d\n"
               ,i, angles[i], item->lAngle, startAmbig, endAmbig);*/
          }
          if (fabs((double)(item->hAngle - angles[i])) < 1.01 * tol) {
            if (angles[0] <= angles[nz - 1])
              endAmbig = 1;
            else
              startAmbig = 1;
            /*printf("Angle %d, %f ambiguous to high angle %f, start %d end %d\n"
              ,i, angles[i], item->hAngle, startAmbig, endAmbig);*/
          }
        }
      }
      /*printf("item %d  start %d end %d low %f high %f  implied start %d "
             "end %d\n", k, item->startingSlice, item->endingSlice,
             item->lAngle, item->hAngle, start, end);*/

      // If can't find, skip this one, count how many times this happens
      // This used to guarantee it thought they were off by 1!  
      if (start < 0 || end < 0) {
        numEmpty++;
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

  // Adjust them all if all off by one as far as we can tell, for old data
  if ((!allEqual || allOffByOne) && defVersion < 2) {
    printf("View numbers in defocus file appear to be low by 1;\n"
           "  adding 1 to correct for old Ctfplotter bug\n");
    for (k = 0; k < ilistSize(list); k++) {
      item = (SavedDefocus *)ilistItem(list, k);
      item->startingSlice++;
      item->endingSlice++;
    }
  }
  if ((!allEqual && allOffByOne && defVersion < 2) || (allEqual && !allOffByOne))
    return 0;
  return 1;
}
