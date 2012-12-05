/*
* myapp.cpp - the QApplication class for ctfplotter.
*
*  Authors: Quanren Xiong and David Mastronarde
*
*  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
*  Microscopy of Cells ("BL3DEMC") and the Regents of the University of
*  Colorado.  See dist/COPYRIGHT for full copyright notice.
*
*  $Id$
*/

#include <QtGui>
#include <qlabel.h>
#include <qfile.h>
#include <qtoolbutton.h>
#include <qcursor.h>
#include <qmessagebox.h>

#include <stdio.h>
#include <math.h>

#include "plotter.h"
#include "fittingdialog.h"
#include "angledialog.h"
#include "myapp.h"
#include "simplexfitting.h"
#include "linearfitting.h"

#include "b3dutil.h"
#include "ilist.h"
#include "mrcfiles.h"
#include "mrcslice.h"
#include "sliceproc.h"
#include "cfft.h"
#include "parse_params.h" //for exitError()

#define MY_PI 3.1415926
#define MIN_ANGLE 1.0e-6  //tilt Angle less than this is treated as 0.0;

int MyApp::mDim = 0;
int MyApp::mTileSize = 0;

MyApp::MyApp(int &argc, char *argv[], int volt, double pSize,
             double ampRatio, float cs, char *defFn, int dim, int hyper,
             double focusTol, int tSize, double tAxisAngle, double lAngle,
             double hAngle, double expDefocus, double leftTol,
             double rightTol, int maxCacheSize, int invertAngles)
  : QApplication(argc, argv),
    defocusFinder(volt, pSize, ampRatio, cs, dim, expDefocus),
    mCache(maxCacheSize)
{
  mSaveModified = false;
  mSaveAndExit = false;
  mDim = dim;
  mHyperRes = hyper;
  mDefocusTol = focusTol;
  mTileSize = tSize;
  mTiltAxisAngle = tAxisAngle;
  mPixelSize = pSize;
  mVoltage = volt;
  mLowAngle = lAngle;
  mHighAngle = hAngle;
  mX1MethodIndex = 1; //0:Linear, 1:Simplex;
  mX2MethodIndex = 1; //0:Linear, 1:Simplex;
  mZeroFitMethod = 0; // 0 CTF, 1 polynomial, 2 intersection
  mVaryCtfPowerInFit = false;
  mPolynomialOrder = 4;
  mDefocusOption = 0;     //use expected defocus;
  mInitialTileOption = 0; //only use central tiles initially;
  mLeftDefTol = leftTol;
  mRightDefTol = rightTol;
  mBackedUp = false;
  mFnDefocus = defFn;
  mTileIncluded = NULL;
  mStackMean = -1000.0;
  mFreqTileCounter = (int *)malloc(mDim * sizeof(int));
  mNumNoiseFiles = 0;
  mNoisePS = (double *)malloc(mDim * sizeof(double));
  mTiltAngles = NULL;
  mSortedAngles = NULL;
  mAngleSign = invertAngles ? -1. : 1.;

  // read in existing defocus data
  mSaved = readDefocusFile(mFnDefocus);
}

MyApp::~MyApp()
{
  delete simplexEngine;
  delete linearEngine;
  //if(mSaveFp) fclose(mSaveFp);
  //for(int k=0;k<MAXSLICENUM;k++) if(slice[k]) sliceFree(slice[k]);
}

//save all PS in text file for plotting in Matlab
//when being called in writeDefocusFile
//The calling of it is commented out in release version.
void MyApp::saveAllPs()
{
  char fnInitPs[40];
  char fnBackPs[40];
  char fnSubtracted[40];
  char fnDim[40];

  sprintf(fnInitPs, "%s.init", mFnDefocus);
  sprintf(fnBackPs, "%s.floor", mFnDefocus);
  sprintf(fnSubtracted, "%s.final", mFnDefocus);
  sprintf(fnDim, "%s.dim", mFnDefocus);

  FILE *fpInit = fopen(fnInitPs, "w");
  FILE *fpFloor = fopen(fnBackPs, "w");
  FILE *fpFinal = fopen(fnSubtracted, "w");
  FILE *fpDim = fopen(fnDim, "w");

  int ii;
  double *ps = (double *)malloc(mDim * sizeof(double));
  setNoiseForMean(mStackMean);
  PlotSettings settings = mPlotter->mZoomStack[mPlotter->mCurZoom];

  for (ii = 0; ii < mDim; ii++) {
    if (settings.minX > ii / 100.0 || settings.maxX < ii / 100.0)
      continue;
    fprintf(fpDim, "%7.4f\n", ii / 100.0);
    fprintf(fpFinal, "%7.4f\n", log(mRAverage[ii])) ;
    fprintf(fpFloor, "%7.4f\n", log(mNoisePS[ii]));
    ps[ii] = mRAverage[ii] * mNoisePS[ii];
    fprintf(fpInit, "%7.4f\n", log(ps[ii]));
  }

  fclose(fpInit);
  fclose(fpFloor);
  fclose(fpFinal);
  fclose(fpDim);
  free(ps);
}

//plot and fit the PS stored in mRAverage, find the defocus and update display
void MyApp::plotFitPS(bool flagSetInitSetting)
{
  double *ps = (double *)malloc(mDim * sizeof(double));
  QVector<QPointF> data;
  int ii;

  // Dividing by the Noise PS happened earlier

  PlotSettings plotSetting = PlotSettings();
  double initialMaxY = -100;
  for (ii = 0; ii < mDim; ii++) {
    ps[ii] = log(mRAverage[ii]);
    data.append(QPointF(ii / (float)(mDim - 1), ps[ii]));
    if (ps[ii] > initialMaxY)
      initialMaxY = ps[ii];
  }
  initialMaxY = ceil(initialMaxY);
  plotSetting.maxY = initialMaxY;

  //adjust initial plot setting to data if needed;
  if (flagSetInitSetting)
    mPlotter->setPlotSettings(plotSetting);
  mPlotter->setCurveData(0, data);

  simplexEngine->setRaw(&ps[0]);
  linearEngine->setRaw(&ps[0]);
  fitPsFindZero();
  free(ps);
}

/*
 * Fits the power spectrum by thechosen method and finds the zero
 */
void MyApp::fitPsFindZero()
{
  QVector<QPointF> data, data1;
  double *resLeftFit = (double *)malloc(mDim * sizeof(double));
  double *resRightFit = (double *)malloc(mDim * sizeof(double));
  double inc = 1.0 / (mDim - 1);
  double zero, defocus, err;

  double model[7] = {0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0};
  int ii, order, error, error2 = 0;

  switch (mZeroFitMethod) {
  case 0:   // CTF like curve
    simplexEngine->setRange(mX1Idx1, mX2Idx2);
    order = mVaryCtfPowerInFit ? 5 : 4;
    // To experiment with restricted fitting when there is one slice
    //if (mNumSlicesDone == 1)
    //order = 2;
    if ((error = simplexEngine->fitCTF(resLeftFit, order, err, defocus)))
      printf("simplexEngine error\n");
    else {
      defocusFinder.getTwoZeros(defocus, zero, err);
      defocusFinder.setZero(zero);
      defocusFinder.setDefocus(defocus);
    }
    mPlotter->clearCurve(2);
    break;

  case 1:   // Polynomial fit
    order = B3DMIN(mPolynomialOrder, mX2Idx2 - mX1Idx1);
    if ((error = linearEngine->computeFitting(resLeftFit, model, order + 1,
					      mX1Idx1, mX2Idx2, zero)))
      printf("linearEngine error\n");
    else {
      defocusFinder.setZero(zero);
      defocusFinder.findDefocus(&defocus);
    }
    mPlotter->clearCurve(2);
    break;
    
  case 2:   // Intersection of two curves
    switch (mX1MethodIndex) {
    case 0: // Linear
      if ((error = linearEngine->computeFitting(resLeftFit, model, 2, mX1Idx1,
						mX1Idx2, zero)))
	printf("linearEngine error\n");
      break;
    case 1: // Gaussian
      simplexEngine->setRange(mX1Idx1, mX1Idx2);
      if ((error = simplexEngine->fitGaussian(resLeftFit, err, 0)))
	printf("simplexEngine error\n");
      break;
    default:
      error = 1;
      printf("unknown fitting method chosen\n");
    }

    switch (mX2MethodIndex) {
    case 0:  // Linear
      if ((error2 = linearEngine->computeFitting(resRightFit, model, 2,
						 mX2Idx1, mX2Idx2, zero)))
	printf("linearEngine error\n");
      break;
    case 1: // Gaussian
      simplexEngine->setRange(mX2Idx1, mX2Idx2);
      if ((error2 = simplexEngine->fitGaussian(resRightFit, err, 1)))
	printf("simplexEngine error\n");
      break;
    default:
      error2 = 1;
      printf("unknown fitting method chosen\n");
    }
    if (!error2) {
      for (ii = 0; ii < mDim; ii++)
	data1.append(QPointF(ii * inc, resRightFit[ii]));
      mPlotter->setCurveData(2, data1);
    }
    break;
    
  default:
    break;
  }

  if (!error) {
    for (ii = 0; ii < mDim; ii++)
      data.append(QPointF(ii * inc, resLeftFit[ii]));
    mPlotter->setCurveData(1, data);
  }


  // calculate defocus and update display;
  if (!(error + error2) && mZeroFitMethod == 2) {
    error = defocusFinder.findZero(resLeftFit, resRightFit, mX1Idx1,
                                   (mX2Idx1 + mX2Idx2) / 2, &zero);
    if (!error)
      defocusFinder.findDefocus(&defocus);
  }
  if (error + error2)
    defocusFinder.setDefocus(-2000.0);
  mPlotter->manageLabels(zero, defocus, 0., 0., error);
  defocusFinder.setAvgDefocus(-1.);
  free(resLeftFit);
  free(resRightFit);
}

/*
 * Initializes cache, opens stack, reads angle file
 * This should only be called once with actual data stack
 */
void MyApp::setSlice(const char *stackFile, char *angleFile)
{

  //init and clear old contents;
  mCache.initCache(stackFile, mDim, mHyperRes, mTileSize, mNxx, mNyy, mNzz);

  // Read in array of tilt angles once from file
  B3DFREE(mTiltAngles);
  mTiltAngles = NULL;
  mMinAngle = 10000.;
  mMaxAngle = -10000.;
  if (angleFile) {
    mTiltAngles = readTiltAngles(angleFile, mNzz, mAngleSign, mMinAngle,
                                 mMaxAngle);
    B3DFREE(mSortedAngles);
    mSortedAngles = B3DMALLOC(float, mNzz);
    if (!mSortedAngles)
      exitError("Allocating array for sorted angles");
    memcpy(mSortedAngles, mTiltAngles, mNzz * sizeof(float));
    rsSortFloats(mSortedAngles, mNzz);
    mAutoFromAngle = mSortedAngles[0];
    mAutoToAngle = mSortedAngles[mNzz - 1];

    // Now that we finally know z size, we can check the starting and ending
    // slices and fix them if they are off by 1 or otherwise inconsistent
    if (ilistSize(mSaved) &&
        checkAndFixDefocusList(mSaved, mTiltAngles, mNzz)) {
      QMessageBox::warning
      (0, "Warning: Inconsistent view numbers",
       "The view numbers in the existing defocus file were not all\n"
       "consistent with the angular ranges.  You should find the defocus\n"
       "again in all of the ranges and save the new data.");
      qApp->processEvents();
    }
  }

  mCache.whatIsNeeded(mLowAngle, mHighAngle, mStartingSlice, mEndingSlice);

  if (mTileIncluded)
    free(mTileIncluded);
  mTileIncluded = (int *)malloc(mNzz * sizeof(int));
}

/*
 * Computes PS from central tiles only
 */
int  MyApp::computeInitPS()
{
  float stripWidthPixels = 0.0;
  int halfSize = mTileSize / 2;
  int psSize = mDim * mHyperRes;
  double *psSum = (double *)malloc(psSize * sizeof(double));
  int counter;
  double localMean, tmpMean;
  int i, j, k, ii;
  double diagonal = sqrt((double)(mNxx * mNxx + mNyy * mNyy));
  double axisXAngle, centerX, centerY, r, alpha, d;

  //tilt axis angle with X coordinate, [0, 2*pi];
  axisXAngle = 90 + mTiltAxisAngle;
  axisXAngle = axisXAngle * MY_PI / 180.0;
  //if(axisXAngle<0) axisXAngle=2.0*M_PI+axisXAngle;

  counter = 0;
  localMean = 0.0;
  for (i = 0; i < psSize; i++)
    psSum[i] = 0.;
  for (i = 0; i < mNzz; i++)
    mTileIncluded[i] = 0;

  std::vector<int> accessOrder = mCache.optimalAccessOrder();
  float *currPS;
  float currAngle;
  int whichSlice;

  mNumSlicesDone = accessOrder.size();
  for (k = 0; k < accessOrder.size(); k++) {
    whichSlice = accessOrder[k];
    currAngle = mCache.getAngle(whichSlice);
    if (fabs(currAngle) > MIN_ANGLE)
      stripWidthPixels = fabs(mDefocusTol / tan(currAngle)) / mPixelSize;
    else
      stripWidthPixels = diagonal;
    if (stripWidthPixels > diagonal)
      stripWidthPixels = diagonal;

    for (i = 0; i < mNxx / halfSize - 1; i++) {
      for (j = 0; j < mNyy / halfSize - 1; j++) {
        centerX = i * halfSize + halfSize - mNxx / 2; //center coordinate of the tile
        centerY = j * halfSize + halfSize - mNyy / 2;
        r = sqrt((double)(centerX * centerX + centerY * centerY));
        alpha = atan2(centerY, centerX);
        //if(alpha<0) alpha=2.0*M_PI+alpha;//convert to [0,2*pi];
        d = r * fabs(sin(alpha - axisXAngle)); // distance to the tilt axis in pixels

        //outside of the strip, continue;
        if (d > stripWidthPixels / 2.0)
          continue;

        counter++;
        mTileIncluded[whichSlice]++;
        currPS = mCache.getHyperPS(i, j, whichSlice, tmpMean);

        for (ii = 0; ii < psSize; ii++)
          psSum[ii] += currPS[ii];
        localMean += tmpMean;
      }
    }
    if (debugLevel >= 1)
      printf("Slice %d has %d central tiles included initially\n", whichSlice,
             mTileIncluded[whichSlice]);
  }//k loop;

  if (debugLevel >= 1)
    printf("computeInitPS() includes %d tiles\n", counter);
  mTotalTileIncluded = counter;

  if (counter) {
    int *freqCounter = mCache.getFreqCount();

    for (i = 0; i < mDim; i++) {
      mRAverage[i] = 0.0;
      mFreqTileCounter[i] = 0;
    }

    mStackMean = localMean / counter; //stack is Not noise, set mStackMean;
    if (mNumNoiseFiles)
      setNoiseForMean(mStackMean);

    // Add the PS sum in to the bins of the average, and add the count of
    // pixels times the tile count into the counter
    // Also make it the divided PS.  RAverage used to be kept as undivided,
    // then needed to be divided in moreTiles and undivided at end
    for (i = 0; i < psSize - mHyperRes / 2; i++) {
      ii = (i + mHyperRes / 2) / mHyperRes;
      if (mNumNoiseFiles)
        mRAverage[ii] += psSum[i] / mNoisePS[ii];
      else
        mRAverage[ii] += psSum[i];
      mFreqTileCounter[ii] += freqCounter[i] * counter;
    }

    //return the PS
    for (i = 0; i < mDim; i++) {
      if (mFreqTileCounter[i])
        mRAverage[i] = mRAverage[i] / mFreqTileCounter[i];
      //printf("%d %d %f  %f\n", i, mFreqTileCounter[i], mRAverage[i], 
      //       mRAverage[i]*mFreqTileCounter[i]);
    }
    free(psSum);
    return -1;
  } else { //need to compute mean;
    exitError("Error: no tile is included, counter=0");
    // DNM removed alternate code here
  }// else
  free(psSum);
  return 0;
}

/*
 * Slot for adding more tiles
 */
void MyApp::moreTileCenterIncluded()
{
  QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));
  moreTile(true);
  QApplication::restoreOverrideCursor();
  plotFitPS(false);
}

/*
 * Computes PS from all available tiles by scaling the frequencies to match
 * the first and second zero of the central tiles; skips central tiles if they
 * are already included
 */
void MyApp::moreTile(bool hasIncludedCentralTiles)
{

  int halfSize = mTileSize / 2;
  int leftCounter, rightCounter;
  int k, ii;
  int psSize = mDim * mHyperRes;
  double *leftPsSum = (double *)malloc(psSize * sizeof(double));
  double *rightPsSum = (double *)malloc(psSize * sizeof(double));
  double deltaZ; // in microns;
  float freqInc = 1.0 / (mDim - 1);
  int *stripCounter = (int *)malloc(mDim * sizeof(int));;
  double *stripAvg = (double *)malloc(mDim * sizeof(double));
  double leftMean, rightMean, tmpMean, effectiveDefocus;

  int tileMax = floor((float)mNxx / (float)halfSize);
  double axisXAngle; //tilt axis angle with X coordinate, [0, 2*pi];
  double diagonal = sqrt((double)(mNxx * mNxx + mNyy * mNyy));
  double stripWidthPixels;
  double centerX, centerY, r, alpha, d, delf1, delf2;
  double centerZero1, centerZero2, shiftedZero1, shiftedZero2;
  double rightScale, rightAdd, leftScale, leftAdd;
  double rightDelFmin, rightDelFmax, leftDelFmin, leftDelFmax;
  double wallStart, procCum = 0., allTime;
  int x, y, scanCounter;
  bool isOnLeft; // is the tile center on the left of the tilt axis;
  float *currPS;

  if (mDefocusOption && defocusFinder.getDefocus() < 0) {
    printf("Warning invalid defocus, computation halts\n");
    free(rightPsSum);
    free(leftPsSum);
    free(stripCounter);
    free(stripAvg);
    return;
  }
  //defocusFinder.setDefocus(defocusFinder.getExpDefocus());

  mPlotter->mTileButton->setEnabled(false);

  tileMax = (tileMax - 1) * (tileMax - 1);

  //tilt axis angle with X coordinate, [0, 2*pi];
  axisXAngle = 90 + mTiltAxisAngle;
  axisXAngle = axisXAngle * MY_PI / 180.0;
  if (axisXAngle < 0)
    axisXAngle = 2.0 * MY_PI + axisXAngle;

  if (mDefocusOption)
    effectiveDefocus = defocusFinder.getDefocus();
  else
    effectiveDefocus = defocusFinder.getExpDefocus();
  defocusFinder.getTwoZeros(effectiveDefocus, centerZero1, centerZero2);

  std::vector<int> accessOrder = mCache.optimalAccessOrder();

  float currAngle;
  int whichSlice;

  allTime = wallTime();
  int mItrNum;
  mNumSlicesDone = accessOrder.size();
  for (k = 0; k < accessOrder.size(); k++) {
    whichSlice = accessOrder[k];
    currAngle = mCache.getAngle(whichSlice);
    mItrNum = 0;
    if (fabs(currAngle) > MIN_ANGLE)
      stripWidthPixels = fabs(mDefocusTol / tan(currAngle)) / mPixelSize;
    else
      stripWidthPixels = diagonal;
    if (stripWidthPixels > diagonal)
      stripWidthPixels = diagonal;

    scanCounter = 0;
    while (scanCounter < tileMax) {

      // Get the delta Z for this strip, then the defocus and the first and
      // second zeros and scaling to center zeros on right and left sides
      // Make deltaZ signed
      deltaZ = (mItrNum + 0.5) * (stripWidthPixels / 2.0) * mPixelSize *
               tan(currAngle) / 1000.0; // in microns;
      tmpMean = effectiveDefocus + deltaZ;
      defocusFinder.getTwoZeros(tmpMean, shiftedZero1, shiftedZero2);
      rightScale = (centerZero2 - centerZero1) / (shiftedZero2 - shiftedZero1);
      rightAdd = centerZero1 - shiftedZero1 * rightScale;
      delf1 = centerZero1 - shiftedZero1;
      delf2 = centerZero2 - shiftedZero2;
      rightDelFmin = B3DMIN(delf1, delf2);
      rightDelFmax = B3DMAX(delf1, delf2);
      if (debugLevel >= 2)
        printf("Right: Z1  %f -> %f   Z2  %f -> %f  scale %f  add %f  "
               "delmin %f  delmax %f\n",
               centerZero1, shiftedZero1, centerZero2, shiftedZero2,
               rightScale, rightAdd, rightDelFmin, rightDelFmax);

      tmpMean = effectiveDefocus - deltaZ;
      defocusFinder.getTwoZeros(tmpMean, shiftedZero1, shiftedZero2);
      leftScale = (centerZero2 - centerZero1) / (shiftedZero2 - shiftedZero1);
      leftAdd = centerZero1 - shiftedZero1 * leftScale;
      delf1 = centerZero1 - shiftedZero1;
      delf2 = centerZero2 - shiftedZero2;
      leftDelFmin = B3DMIN(delf1, delf2);
      leftDelFmax = B3DMAX(delf1, delf2);
      if (debugLevel >= 2)
        printf("Left: Z1  %f -> %f   Z2  %f -> %f  scale %f  add %f  "
               "delmin %f  delmax %f\n",
               centerZero1, shiftedZero1, centerZero2, shiftedZero2,
               leftScale, leftAdd, leftDelFmin, leftDelFmax);

      // Zero out the PS sums on each side and the counters
      for (ii = 0; ii < psSize; ii++) {
        leftPsSum[ii] = 0.0;
        rightPsSum[ii] = 0.0;
      }
      leftCounter = 0;
      rightCounter = 0;
      leftMean = 0.0;
      rightMean = 0.0;

      for (x = 0; x < mNxx / halfSize - 1; x++) {
        for (y = 0; y < mNyy / halfSize - 1; y++) {
          centerX = x * halfSize + halfSize - mNxx / 2; //center coordinate of the tile
          centerY = y * halfSize + halfSize - mNyy / 2;
          r = sqrt((double)(centerX * centerX + centerY * centerY));
          alpha = atan2(centerY, centerX);
          if (alpha < 0)
            alpha = 2.0 * MY_PI + alpha; //convert to [0,2*pi];
          d = r * fabs(sin(alpha - axisXAngle)); //distance to the tilt axis in pixels

          /*if(x==0 && y==0 && mItrNum==1){
            printf("d=%f leftLimit=%f rightLimit=%f \n",
                d, mItrNum*stripWidthPixels/2.0, (mItrNum+1)*stripWidthPixels/2.0);
          }*/

          //outside of the strip, continue;
          if (d < mItrNum * stripWidthPixels / 2.0 ||
              d > (mItrNum + 1)*stripWidthPixels / 2.0)
            continue;

          //printf("mItrNum=%d tile x=%d y=%d is included  \n", mItrNum, x, y);
          scanCounter++;

          //skip central tiles since they already included
          if (hasIncludedCentralTiles && d <= stripWidthPixels / 2.0)
            continue;

          if (axisXAngle <= MY_PI)
            isOnLeft = alpha > axisXAngle && alpha < axisXAngle + MY_PI;
          else
            isOnLeft = alpha > axisXAngle || alpha < axisXAngle - MY_PI;

          d = d * mPixelSize * fabs(tan(currAngle)); //defocus difference in nm;

          if ((isOnLeft && d > mLeftDefTol) || (!isOnLeft && d > mRightDefTol))
            continue;

          // Get the PS for this tile
          wallStart = wallTime();
          currPS = mCache.getHyperPS(x, y, whichSlice, tmpMean);
          procCum += wallTime() - wallStart;

          // Add in to left or right FFT sums
          if (isOnLeft) {
            leftCounter++;
            leftMean += tmpMean;
            for (ii = 0; ii < psSize; ii++)
              leftPsSum[ii] += currPS[ii];

          } else { //right side;
            rightCounter++;
            rightMean += tmpMean;
            for (ii = 0; ii < psSize; ii++)
              rightPsSum[ii] += currPS[ii];
          }
        }
      } //for x-y loop;

      // Add in left side
      scaleAndAddStrip(leftPsSum, stripAvg, stripCounter, leftCounter,
                       leftMean, leftScale, leftAdd,
                       freqInc, leftDelFmin, leftDelFmax);

      //right side
      scaleAndAddStrip(rightPsSum, stripAvg, stripCounter, rightCounter,
                       rightMean, rightScale,
                       rightAdd, freqInc, rightDelFmin, rightDelFmax);
      mTileIncluded[whichSlice] += leftCounter + rightCounter;
      if (debugLevel >= 2)
        printf("scanCounter=%d leftCounter=%d rightCounter=%d mTileIncluded[%d]=%d\n",
           scanCounter, leftCounter, rightCounter, whichSlice, mTileIncluded[whichSlice]);
      mItrNum++;
    }//while loop;

    if (debugLevel >= 1)
      printf("%d tiles of slice %d have been included\n",
             mTileIncluded[whichSlice], whichSlice);

    if (debugLevel >= 2)
      printf("***mItrNum=%d deltaZ=%f(microns) scanCounter=%d \
         mTileIncluded[%d]=%d\n",
             mItrNum, deltaZ, scanCounter, whichSlice,
             mTileIncluded[whichSlice]);
  }// the k-th slice

  if (debugLevel >= 2)
    printf("FFT time %.4f  Total time %.4f\n", procCum, wallTime() - allTime);

  /*  int csum = 0;
  for (ii = 0; ii < mDim; ii++) {
    //printf("%d %d %f  %f\n", ii, mFreqTileCounter[ii], mRAverage[ii],
    //       mRAverage[ii]*mFreqTileCounter[ii]);
      csum+=mFreqTileCounter[ii];
      }*/
  //printf("total %d\n", csum);

  free(leftPsSum);
  free(rightPsSum);
  free(stripCounter);
  free(stripAvg);
}

/*
 * Rotationally average the strip FFT sum with frequency scaling to match
 * zeros
 */
void MyApp::scaleAndAddStrip
(double *psSum, double *stripAvg, int *stripCounter, int counter,
 double mean, double xScale, double xAdd, float freqInc, double delFmin,
 double delFmax)
{
  int ii, jj, npsIndex, stripRIndex, stripLIndex, lnum;
  double freq, delfreq, freqst, freqnd, lfrac;
  double hyperInc = freqInc / mHyperRes;
  int *freqCount = mCache.getFreqCount();

  if (!counter)
    return;

  // Get the mean intensity and noise PS based on this mean
  mean = mean / counter;
  setNoiseForMean(mean);

  // Zero average arrays
  for (ii = 0; ii < mDim; ii++) {
    stripCounter[ii] = 0;
    stripAvg[ii] = 0.0;
  }

  // Loop over average PS, divide by the noise appropriate to the
  // unshifted frequency, then scale/shift the frequency and add into the
  // average
  for (ii = 0; ii < mDim * mHyperRes - mHyperRes / 2; ii++) {

    // Get index for dividing by noise
    npsIndex = (ii + mHyperRes / 2) / mHyperRes;

    // Get starting and ending frequency of hyper bin and adjust them, get
    // index of strip bin each side falls into
    freqst = ii * hyperInc;
    freqnd = freqst + hyperInc;
    delfreq = freqst * xScale + xAdd - freqst;
    delfreq = B3DMIN(delFmax, B3DMAX(delFmin, delfreq));
    freqst += delfreq;
    stripLIndex = B3DNINT(freqst / freqInc);
    delfreq = freqnd * xScale + xAdd - freqnd;
    delfreq = B3DMIN(delFmax, B3DMAX(delFmin, delfreq));
    stripRIndex = B3DNINT((freqnd + delfreq) / freqInc);
    if (stripRIndex >= mDim || stripLIndex < 0)
      continue;

    // If both sides of hyper bin are in the strip bin, then just add it in
    if (stripRIndex == stripLIndex) {
      stripAvg[stripRIndex] += psSum[ii] / mNoisePS[npsIndex];
      stripCounter[stripRIndex] += freqCount[ii] * counter;
    } else if (freqCount[ii]) {

      // Otherwise figure out how to split the hyper bin between two strip bins
      // based on frequency at left edge of upper bin
      freq = (stripRIndex - 0.5) * freqInc;
      lfrac = (freq - freqst) / hyperInc;
      lfrac = B3DMAX(0., B3DMIN(1., lfrac));
      lnum = B3DNINT(lfrac * freqCount[ii]);
      lfrac = (double)lnum / freqCount[ii];
      stripAvg[stripLIndex] += lfrac * psSum[ii] / mNoisePS[npsIndex];
      stripCounter[stripLIndex] += lnum * counter;
      stripAvg[stripRIndex] += (1. - lfrac) * psSum[ii] / mNoisePS[npsIndex];
      stripCounter[stripRIndex] += (freqCount[ii] - lnum) * counter;
    }
  }

  /*printf("xShift=%d mTotalTileIncluded=%d counter=%d\n", xShift,
    mTotalTileIncluded, counter);

    printf("iterNum=%d, before change: mRAverage[0]=%f stripAvg[0]=%f\n",
           mItrNum,  mRAverage[0], stripAvg[0]);
  */

  for (ii = 0; ii < mDim; ii++) {
    jj = stripCounter[ii];
    if (jj) {
      mRAverage[ii] = (mFreqTileCounter[ii] * mRAverage[ii] + stripAvg[ii]) /
                      (mFreqTileCounter[ii] + jj);
      mFreqTileCounter[ii] += jj;
    }
  }

  //printf("iterNum=%d, mRAverage[0]=%f\n",mItrNum,  mRAverage[0]);
  mTotalTileIncluded += counter;
}

/*
 * Slot that is called when the fitting range changes or some other parameters change
 */
void MyApp::rangeChanged(double x1_1, double x1_2, double x2_1, double x2_2)
{
  mX1Idx1 = x1_1 * (mDim - 1);
  mX1Idx2 = x1_2 * (mDim - 1);
  mX2Idx1 = x2_1 * (mDim - 1);
  mX2Idx2 = x2_2 * (mDim - 1);
  fitPsFindZero();
}

/*
 * Slot for responding to angle range change or change in tile definition by
 * computing tile PS if needed and recomputing the PS curves
 */
void MyApp::angleChanged(double lAngle, double hAngle, double expDefocus,
  double defTol, int tSize, double axisAngle, double leftTol, double rightTol)
{
  //If tilt angle range does not change, do not need to reload slices;
  if (lAngle != mLowAngle || hAngle != mHighAngle) {
    setLowAngle(lAngle);
    setHighAngle(hAngle);
    //setSlice();
    mCache.whatIsNeeded(lAngle, hAngle, mStartingSlice, mEndingSlice);
  }
  if (mStartingSlice < 0 || mEndingSlice < 0)
    return;

  QApplication::setOverrideCursor(QCursor(Qt::WaitCursor));

  mDefocusTol = defTol;
  mLeftDefTol = leftTol;
  mRightDefTol = rightTol;
  if (tSize != mTileSize)
    mCache.clearAndSetSize(mDim, mHyperRes, tSize);
  mTileSize = tSize;
  mTiltAxisAngle = axisAngle;
  defocusFinder.setExpDefocus(expDefocus);

  //computeInitPS();

  //plot and fit the new initial PS;
  if (mInitialTileOption) {
    int i;
    //following variables need to be re-set or re-initialized;
    //mStackMean;
    mTotalTileIncluded = 0;
    for (i = 0; i < mNzz; i++)
      mTileIncluded[i] = 0;
    for (i = 0; i < mDim; i++) {
      mRAverage[i] = 0.0;
      mFreqTileCounter[i] = 0;
    }

    moreTile(false); //include all the tiles and plot;
    plotFitPS(false);
  } else {
    computeInitPS();
    mPlotter->mTileButton->setEnabled(true);
    plotFitPS(false); // only plot;
  }

  QApplication::restoreOverrideCursor();
}

/*
 * Does multiple fits to ranges of size rangeSize, at intervals of rangeStep,
 * for all ranges that fit between minAngle and maxAngle.  The fit is iterated
 * numIter times, with the current defocus estimate used after the first iteration.
 * If rangeStep is 0, it does a fit to every individual picture, regardless of of the
 * value of rangeSize.
 */
int MyApp::autoFitToRanges(float minAngle, float maxAngle, float rangeSize,
                           float rangeStep, int numIter)
{
  int tSize, numSteps, i, step, sortInd, indLo, indHi, lastIndLo, lastIndHi, numDel, iter;
  float eps = 0.02f;
  double expDefocus = defocusFinder.getExpDefocus();
  int saveOption = mDefocusOption;
  SavedDefocus *item;
  double defTol, axisAngle, leftTol, rightTol;
  double trueStep, minDel, maxDel, mid, minMid, maxMid, loAngle, hiAngle;
  bool tolOk = mPlotter->mAngleDia->getTileTolerances(defTol, tSize, axisAngle, leftTol,
						      rightTol);

  if (!tolOk)
    return 1;

  // Determine how many ranges to do
  if (rangeStep > 0) {
    numSteps = B3DNINT((maxAngle - minAngle - rangeSize) / rangeStep + 1.);
    trueStep = rangeStep;
    if (numSteps > 1)
      trueStep = (maxAngle - minAngle - rangeSize) / (numSteps - 1);
    else
      rangeSize = maxAngle - minAngle;
    minDel = minAngle + 0.25 * rangeSize;
    maxDel = maxAngle - 0.25 * rangeSize;
  } else {
    numSteps = 0;
    for (i = 0; i < mNzz; i++) {
      if (mSortedAngles[i] >= minAngle - eps && mSortedAngles[i] <= maxAngle + eps) {
        if (!numSteps)
          sortInd = 0;
        numSteps++;
      }
    }
    if (numSteps) {
      minDel = mSortedAngles[sortInd] - eps;
      maxDel = mSortedAngles[sortInd + numSteps - 1] + eps;
    }
  }

  if (!numSteps)
    return 2;

  // Remove existing items from the table - first count them
  numDel = 0;
  minMid = 100000.;
  maxMid = -100000.;
  for (i = 0; i < ilistSize(mSaved); i++) {
    item = (SavedDefocus *)ilistItem(mSaved, i);
    mid = (item->hAngle + item->lAngle) / 2.;
    if (mid >= minDel && mid <= maxDel) {
      numDel++;
      minMid = B3DMIN(minMid, mid);
      maxMid = B3DMAX(maxMid, mid);
    }
  }

  // Confirm and remove
  if (numDel > 0) {
    if (mSaveAndExit) {
      i = QMessageBox::Yes;
    } else {
      QString str;
      str.sprintf("Do you want to remove existing defocus values listed in the \n"
                  "table with midpoint tilt angles from %.2f to %.2f degrees?", minMid,
                  maxMid);
      i = QMessageBox::question(NULL, QString("Ctfplotter"), str,
                                QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel,
                                QMessageBox::Yes);
    }
    if (i == QMessageBox::Cancel)
      return 0;
    if (i == QMessageBox::Yes) {
      for (i = 0; i < ilistSize(mSaved); i++) {
        item = (SavedDefocus *)ilistItem(mSaved, i);
        mid = (item->hAngle + item->lAngle) / 2.;
        if (mid >= minDel && mid <= maxDel)
          ilistRemove(mSaved, i--);
      }
    }
  }

  // Loop on the ranges
  lastIndLo = -1;
  lastIndHi = -1;
  for (step = 0; step < numSteps; step++) {

    // Get the low and high angles, make sure it is nonempty and unique
    if (rangeStep > 0) {
      loAngle = minAngle + step * trueStep;
      hiAngle = loAngle + rangeSize;
      indLo = -1;
      for (i = 0; i < mNzz; i++) {
        if (mSortedAngles[i] >= loAngle - eps && mSortedAngles[i] <= hiAngle + eps) {
          if (indLo < 0)
            indLo = i;
          indHi = i;
        }
      }
      if (indLo < 0 || indLo - lastIndLo && indHi == lastIndHi)
        continue;
      lastIndLo = indLo;
      lastIndHi = indHi;
    } else {
      loAngle = hiAngle = mSortedAngles[sortInd++];
    }

    // Iterate
    for (iter = 0; iter < numIter; iter++) {
      angleChanged(loAngle, hiAngle, expDefocus, defTol, tSize, axisAngle, leftTol,
                   rightTol);
      mDefocusOption = 1;
    }
    mDefocusOption = saveOption;

    // Save in table
    saveCurrentDefocus();
  }
  mPlotter->mAngleDia->setAnglesClicked();
  return 0;
}

void MyApp::setInitTileOption(int index)
{
  mPlotter->mTileButton->setEnabled(false);
  mInitialTileOption = index;
}

/*
 * Determine the noise images to interpolate between and computethe noise PS for
 * the given mean level
 */
void MyApp::setNoiseForMean(double mean)
{
  int i, highInd = mNumNoiseFiles - 1;
  double lowMean, highMean;
  double *lowPs, *highPs;
  for (i = 1; i < mNumNoiseFiles; i++) {
    if (mNoiseMeans[i] > mean) {
      highInd = i;
      break;
    }
  }
  lowMean = mNoiseMeans[highInd - 1];
  highMean = mNoiseMeans[highInd];
  //printf("For mean %f  noise index %d noise means %f %f\n", mean, highInd-1,
  //     lowMean, highMean);
  lowPs = mAllNoisePS + mNoiseIndexes[highInd - 1] * mDim;
  highPs = mAllNoisePS + mNoiseIndexes[highInd] * mDim;
  for (i = 0; i < mDim; i++)
    mNoisePS[i] = lowPs[i] + (highPs[i] - lowPs[i]) * (mean - lowMean) /
                  (highMean - lowMean);
}

/*
 * Save the current defocus and angular range in the list and display in table
 */
void MyApp::saveCurrentDefocus()
{
  SavedDefocus toSave;

  toSave.startingSlice = getStartingSliceNum();
  toSave.endingSlice = getEndingSliceNum();
  toSave.lAngle = getLowAngle();
  toSave.hAngle = getHighAngle();
  toSave.defocus = defocusFinder.getDefocus();
  addItemToDefocusList(mSaved, toSave);
  mSaveModified = true;
  if (mPlotter->mAngleDia)
    mPlotter->mAngleDia->updateTable();
}

/*
 * Write out the defocus file
 */
void MyApp::writeDefocusFile()
{
  SavedDefocus *item;
  FILE *fp;
  int i;
  if (!ilistSize(mSaved))
    return;
  if (!mBackedUp)
    imodBackupFile(mFnDefocus);
  mBackedUp = true;
  fp = fopen(mFnDefocus, "w");
  if (!fp) {
    if (mSaveAndExit)
      exitError("Cannot open output file for saving defocus values");
    QMessageBox::critical(NULL, "Ctfplotter: File save error",
                          "Can not open output file");
    return;
  }

  for (i = 0; i < ilistSize(mSaved); i++) {
    item = (SavedDefocus *)ilistItem(mSaved, i);
    fprintf(fp, "%d\t%d\t%5.2f\t%5.2f\t%6.0f\n", item->startingSlice + 1,
            item->endingSlice + 1, item->lAngle, item->hAngle,
            item->defocus * 1000);
  }
  fclose(fp);
  mSaveModified = false;
}

/*
 * Convenience function.  Note that libdiaqt depends on OpenGL so can't be used
 */
void MyApp::showHideWidget(QWidget *widget, bool state)
{
  if (state)
    widget->show();
  else
    widget->hide();
}
