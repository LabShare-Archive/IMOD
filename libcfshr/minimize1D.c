/*  minimize1D.c - Simple one-dimensional search routine
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2013 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */
#include <math.h>
#include "imodconfig.h"
#include "b3dutil.h"
#ifdef F77FUNCAP
#define minimize1d MINIMIZE1D 
#else
#define minimize1d minimize1d_
#endif

#define CUT_STEP_SET_DIR { \
    step /= 2.; \
    (*numCutsDone)++; \
    direction = values[2] > values[0] ? -1 : 1; }

/*!
 * Guides the search for a minimum value of a one-dimensional function by initially
 * either scanning for a minimum at a given step size for a number of steps, or walking
 * to the local minimum from the starting point, then repeatedly cutting the step size by 
 * a factor of 2 once the minimum is bracketed to an interval. ^
 * [curPosition]  - Currently evaluated position ^
 * [curValue]     - Value at the current position ^
 * [initialStep]  - Initial step size for the search (should be positive) ^
 * [numScanSteps] - Number of steps to scan from initial position, if scanning for the 
 * global minimum with a range, or 0 to walk to the local minimum ^
 * [numCutsDone]  - Number of times that the step size has been cut by a factor of 2 ^
 * [brackets]     - An array of at least 14 elements for storing positions and values ^
 * [nextPosition] - The next position that needs to be evaluated ^
 * To use the routine, initialize [numCutsDone] to -1 and [curPosition] at a starting 
 * value.  For doing a scan, the starting value would be the low end of the range to be
 * scanned; otherwise it should be the most plausible value from which a minimum can
 * be reached.  Then enter a loop to: ^
 * Evaluate the function at [curPosition] to obtain [curValue], ^
 * Call this routine, ^
 * Test whether the return value is 2 if scanning, or test whether [nextPosition] is out 
 * of range if not scanning, and break the loop with failure to find a minimum, ^
 * Test whether [numCuts] has reached a desired limit, and break the loop with 
 * completion, ^
 * Assign [nextPosition] to [curPosition] or pass [curPosition] for that argument.  ^
 * The position that gives the minimum is maintained in brackets[1] and the minimum 
 * value is in brackets[8].  The return value is 1 if brackets[13] is not -1, 0, or 2, 
 * or if the current position is out of range for a scan.
 * The return value is 2 if an initial scan fails to find an absolute minimum.
 */
int minimize1D(float curPosition, float curValue, float initialStep, int numScanSteps,
               int *numCutsDone, float *brackets, float *nextPosition)
{
  int i, stepNum, walking = *numCutsDone ? 0 : 1;
  float step = initialStep;
  float *positions = brackets;
  float *values = brackets + 7;
  int direction = B3DNINT(values[6]);
  if ((direction < -1 || direction > 2) && *numCutsDone >= 0)
    return 1;

  /* If numCutsDone < 0, initialize the search, set position/value into minimum spot */
  if (*numCutsDone < 0) {
    *numCutsDone = 0;
    positions[0] = positions[1] = positions[2] = curPosition;
    values[1] = curValue;
    if (numScanSteps > 0) {

      /* Initialize for full scan, put position/value in top spot */
      values[6] = 2.;
      *nextPosition = curPosition + initialStep;
      positions[5] = curPosition;
      positions[6] = curPosition;
      values[5] = curValue;
      values[0] = curValue - fabs(curValue);
    } else {

      /* Otherwise set up for bidirection walk */
      values[6] = -1.;
      *nextPosition = curPosition - initialStep;
    }
    return 0;
  }

  /* Get the current step size */
  for (i = 0; i < *numCutsDone; i++)
    step /= 2.;

  if (direction == 2) {

    /* INITIAL SCAN FOR DIRECTION = 2 */
    /* figure out which step this is */
    stepNum = B3DNINT((curPosition - positions[6]) / initialStep);
    if (stepNum <= 0 || stepNum > numScanSteps)
      return 1;
    
    /* Roll the positions and decide whether to shift into minimum */
    positions[3] = positions[4];
    positions[4] = positions[5];
    positions[5] = curPosition;
    values[3] = values[4];
    values[4] = values[5];
    values[5] = curValue;
    if (stepNum > 1 && values[4] < values[1]) {
      for (i = 0; i < 3; i++) {
        positions[i] = positions[i + 3];
        values[i] = values[i + 3];
      }
    }

    /* Continue the scan */
    if (stepNum < numScanSteps) {
      *nextPosition = curPosition + initialStep;
      return 0;
    }

    /* Or terminate it.  If the minimum slot is not a true minimum or there is an extreme
     point in the latest piece of the scan that is below the minimum, the scan failed to
     find an absolute minimum */
    if (values[5] < values[1] || values[3] < values[1] || 
        values[2] < values[1] || values[0] < values[1])
      return 2;
    
    /* Cut the step and start in most promising direction */
    CUT_STEP_SET_DIR;

  } else {

    /* WALKING TO MINIMUM OR DOING CUTTING STEPS */

    /* If the new value is higher, replace the bracket on the appropriate side (same 
       dir) */
    if (curValue > values[1]) {
      positions[direction + 1] = curPosition;
      values[direction + 1] = curValue;

      /* It is time to cut the step if already going positive, or if walking and we have 
         moved the bracket on the other side from original minimum */
      if ((!walking && fabs(positions[1 - direction] - positions[1]) < 
           1.1 * fabs(curPosition - positions[1])) || 
          (walking && fabs(positions[1] - positions[2]) > 0.01 * step)) {
        CUT_STEP_SET_DIR;
      } else {

        /* Otherwise, reverse direction */
        direction *= -1;
      }
    } else {
    
      /* If new value is a new minimum or equal to current one, move current minimum to 
         the bracket on the other side and replace the minimum */
      positions[1 - direction] = positions[1];
      values[1 - direction] = values[1];
      positions[1] = curPosition;
      values[1] = curValue;

      /* If walking, just keep going; otherwise cut the step */
      if (!walking) {
        CUT_STEP_SET_DIR;
      }
    }
  }
  *nextPosition = positions[1] + direction * step;
  values[6] = direction;
  return 0;
}

/*! Fortran wrapper for @@minimize1D.@ */
int minimize1d(float *curPosition, float *curValue, float *initialStep, int *numScanSteps,
               int *numCutsDone, float *brackets, float *nextPosition)
{
  return minimize1D(*curPosition, *curValue, *initialStep, *numScanSteps, numCutsDone,
                    brackets, nextPosition);
}
