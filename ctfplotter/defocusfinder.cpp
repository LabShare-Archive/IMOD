/*
* defocusfinder.cpp - routines for finding defocus given a 1D power spectrum.
*
*  Authors: Quanren Xiong and David Mastronarde
*
*  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
*  Microscopy of Cells ("BL3DEMC") and the Regents of the University of
*  Colorado.  See dist/COPYRIGHT for full copyright notice.
*
*  $Id$
*  Log at end of file
*/

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "b3dutil.h"
#include "defocusfinder.h"
#include "myapp.h"

#define DEF_START -1.0
#define DEF_END -20.0
#define ZERO_START 0.2
#define ZERO_END 0.8
#define MY_PI 3.1415926

/*
 * Constructor: set the various constants and expected zero and defocus
 */
DefocusFinder::DefocusFinder(int volt, double pSize,
                             double ampContrast, double inputCs,
                             int dim, double expDef): mVoltage(volt), mPixelSize(pSize),
  mAmpRatio(ampContrast), mCs(inputCs), mDim(dim), mExpDefocus(expDef)
{
  mExpDefocus = mExpDefocus / 1000.0; //convert to microns;
  //wavelength in nm;
  mWavelength = 1.241 / sqrt(mVoltage * (mVoltage +  1022.0));
  mCsOne = sqrt(mCs * mWavelength); // deltaZ=-deltaZ'/mCs1;  In microns
  mCsTwo = sqrt(sqrt(1000000.0 * mCs / mWavelength)); //theta=theta'*mCs2;
  mAmpAngle = 2. * atan(mAmpRatio / sqrt(1. - mAmpRatio * mAmpRatio)) / MY_PI;

  // Compute and set expected zero
  setExpDefocus(mExpDefocus);
  mDefocus = -1000.0;
  mAvgDefocus = -1000.;
}

/*
 * Given the two fitted curves on the left and the right and an interval
 * in which they should intersect, it finds the intersection and returns the
 * zero from that.
 */
int DefocusFinder::findZero(const double *leftRes, const double *rightRes,
                            int x1, int x2, double *zero)
{
  if ((x2 - x1) < 2) {
    printf("findZero() error: range<2\n");
    return -1;
  }
  int dim = x2 - x1 + 1;
  double *diff = (double *)malloc(dim * sizeof(double));
  double minDiff;
  int i, minIndex;
  for (i = x1; i < x1 + dim; i++) {
    diff[i - x1] = leftRes[i] - rightRes[i];
  }

  int middle = dim / 2;

  //find the minimum difference in the first half;
  minDiff = fabs(diff[0]);
  minIndex = 0;
  for (i = 1; i < middle; i++) {
    if (fabs(diff[i]) < minDiff) {
      minDiff = fabs(diff[i]);
      minIndex = i;
    }
  }

  //confirm it is a zerocrossing;
  if ((minIndex - 1) < 0 || (minIndex + 1) > (dim - 1)) {
    printf("findZero() error: the sign of values can not be determined.  \
        zeroIndex=%d\n", minIndex);
    free(diff);
    return -1;
  }

  if (diff[minIndex - 1]*diff[minIndex + 1] < 0.0) { // it is a crossing
    //linear interpolation
    double interpolate = -(diff[minIndex + 1] + diff[minIndex - 1]) /
                         (diff[minIndex + 1] - diff[minIndex - 1]);
    *zero = (double)(x1 + minIndex + interpolate) / (mDim - 1);
    mZeroCrossing = *zero;
    free(diff);
    return 0;
  } else {

    // If that was not a zero crossing, find the minimum in the second half
    minDiff = fabs(diff[middle]);
    minIndex = middle;
    for (i = middle; i < dim; i++) {
      if (fabs(diff[i]) < minDiff) {
        minDiff = fabs(diff[i]);
        minIndex = i;
      }
    }
    if ((minIndex - 1) < 0 || (minIndex + 1) > (dim - 1)) {
      printf("findZero() error: the sign of values can not be determined.  \
          zeroIndex=%d\n", minIndex);
      free(diff);
      return -1;
    }
    if (diff[minIndex - 1]*diff[minIndex + 1] < 0.0) { // it is a crossing
      //linear interpolation
      double interpolate = -(diff[minIndex + 1] + diff[minIndex - 1]) /
                           (diff[minIndex + 1] - diff[minIndex - 1]);
      *zero = (double)(x1 + minIndex + interpolate) / (mDim - 1);
      mZeroCrossing = *zero;
      free(diff);
      return 0;
    }
  }//else

  printf("findZero(): no intersection found\n");
  free(diff);
  return -1;
}

// Removed by DNM 7/7/09, available in revision 1.7
//older version, the same expected defocus may give slightly different defocus
//estimate depending on the current defocus found.
/*void DefocusFinder::setExpDefocus(double expDef) */

/*
 * Sets mExpDefocus and computes mExpZero from the input value expDef
 */
void DefocusFinder::setExpDefocus(double expDef)
{
  double theta;
  double delz = expDef / mCsOne;
  mExpDefocus = expDef;
  theta = sqrt(delz - sqrt(B3DMAX(0, delz * delz + mAmpAngle - 2.)));
  mExpZero = theta * mPixelSize * 2.0 / (mWavelength * mCsTwo);
  //  mExpZero=sqrt(mCsOne/mExpDefocus)*mPixelSize*2.0/(mWavelength*mCsTwo);
}

/*
 * Returns the first and second zero for the given focus
 */
void DefocusFinder::getTwoZeros(double focus, double &firstZero,
                                double &secondZero)
{
  double theta;
  double delz = focus / mCsOne;
  theta = sqrt(delz - sqrt(B3DMAX(0., delz * delz + mAmpAngle - 2.)));
  firstZero = theta * mPixelSize * 2.0 / (mWavelength * mCsTwo);
  theta = sqrt(delz - sqrt(B3DMAX(0., delz * delz + mAmpAngle - 4.)));
  secondZero = theta * mPixelSize * 2.0 / (mWavelength * mCsTwo);
}

/*
 * Find the defocus from the current zero crossing
 */
int DefocusFinder::findDefocus(double *focus)
{
  double theta;
  theta = (mZeroCrossing * mWavelength * mCsTwo) * 0.5 / mPixelSize;
  mDefocus = mCsOne * (pow(theta, 4.) + 2. - mAmpAngle) / (2. * theta * theta);

  if (debugLevel >= 1)
    printf("defocus=%f   \n", mDefocus);
  *focus = mDefocus;
  return 0;
}

/*
 * Find the defocus from the second zero
 */
double DefocusFinder::defocusFromSecondZero(double zero)
{
  double theta;
  theta = (zero * mWavelength * mCsTwo) * 0.5 / mPixelSize;
  return(mCsOne * (pow(theta, 4.) + 4. - mAmpAngle) / (2. * theta * theta));
}

//older non-exact formula, good when defocus>2um but ignoring amp. contrast
/*int DefocusFinder::findDefocus(double *focus)
{
  *focus=mZeroCrossing*mWavelength*mCsTwo/(mPixelSize*2.0);
  *focus=mCsOne/((*focus)*(*focus));
  mDefocus=*focus;
}*/

// Removed by DNM 7/7/09, available in revision 1.7
// older routine trying to solve the exact formula by searching, not reliable;
/*int DefocusFinder::findDefocus(double *focus) */

double DefocusFinder::CTFvalue(double freq, double def)
{
  double theta = (freq * mWavelength * mCsTwo) * 0.5 / mPixelSize;
  double delz = def / mCsOne;
  double phi = 0.5 * MY_PI * (pow(theta, 4.) - 2. * theta * theta * delz);
  return (-2. * (sqrt(1. - mAmpRatio * mAmpRatio) * sin(phi) -
                 mAmpRatio * cos(phi)));
}


/*

$Log$
Revision 1.9  2010/04/02 00:17:12  mast
Cleanup for warnings

Revision 1.8  2009/08/10 22:12:47  mast
Implemented exact equations and added function for second zero

Revision 1.7  2009/01/15 16:31:36  mast
Qt 4 port

Revision 1.6  2008/11/10 18:09:42  xiongq
switch to an approximating formula to find defocus

Revision 1.5  2008/11/07 17:26:24  xiongq
add the copyright heading

*/
