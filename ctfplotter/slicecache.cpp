/*
* slicecache.cpp - build a slice cache to speed up access to power spectra
*
*  Authors: Quanren Xiong and David Mastronarde
*
*  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
*  Microscopy of Cells ("BL3DEMC") and the Regents of the University of
*  Colorado.  See dist/COPYRIGHT for full copyright notice.
*
*  $Id$
*/

/**************how to use this class  **********************
  (When it was a class for holding slices)
  1) initCache() to init the class;
  2) whatIsNeeded() to tell the class what slices will be needed;
  3) mAccessOrder=optimalAccessOrder(). It needs to be called before
     getSlice() is called.
  4) use getSlice(mAccessOrder[i]), getAngle(mAccessOrder[i])
     to read slices starting from mAccessOrder[0].

  *************************************************************/
#include <math.h>
#include <vector>
#include "parse_params.h"
#include "slicecache.h"
#include "myapp.h"
#include "b3dutil.h"
#include "cfft.h"

#define MY_PI 3.1415926

SliceCache::SliceCache(int cacheSize, MyApp *app)
{
  mApp = app;
  mFpStack = NULL;
  mMaxCacheSize = cacheSize;
  mSliceData = NULL;
  mTile = NULL;
  mPsTmp = NULL;
  mFreqCount = NULL;
  mTileToPsInd = NULL;
}

SliceCache::~SliceCache()
{
  for (int i = 0; i < (int)mCachedPS.size(); i++) {
    free(mCachedPS[i]);
    free(mCachedTileDone[i]);
    free(mCachedMeans[i]);
  }
  if (mTile)
    free(mTile);
  if (mTileToPsInd)
    free(mTileToPsInd);
  if (mFreqCount)
    free(mFreqCount);
  if (mSliceData)
    free(mSliceData);
  if (mPsTmp)
    free(mPsTmp);

  // Clear cache components
  mCachedPS.clear();
  mCachedMeans.clear();
  mCachedTileDone.clear();
  mSliceAngles.clear();
  mCachedSliceMap.clear();
}

//init SliceCache and clear its old contents
//It needs to be called whenever the tomogram it manages changes.
void SliceCache::initCache(const char *fnStack, int dim,
                           int hyper, int tSize, int &nx, int &ny, int &nz)
{
  int dsize, csize;
  mDataOffset = 0.;
  if (mFpStack)
    iiFClose(mFpStack);
  if ((mFpStack = iiFOpen(fnStack, "rb")) == 0)
    exitError("could not open input file %s", fnStack);

  /* read mHeader */
  if (mrc_head_read(mFpStack, &mHeader))
    exitError("could not read header of input file %s",  fnStack);
  mSliceMode = sliceModeIfReal(mHeader.mode);
  if (mSliceMode < 0)
    exitError("File mode is %d; only byte, short, integer allowed\n",
              mHeader.mode);
  mrc_getdcsize(mHeader.mode, &dsize, &csize);
  nx = mHeader.nx;
  ny = mHeader.ny;
  nz = mHeader.nz;
  mPsDim = (dim + 1) * hyper + 1;
  if (mFreqCount)
    free(mFreqCount);
  mFreqCount = (int *)malloc(mPsDim * sizeof(int));
  if (mSliceData)
    free(mSliceData);
  mSliceData = (float *)malloc(nx * ny * dsize);
  if (mPsTmp)
    free(mPsTmp);
  mPsTmp = (double *)malloc(mPsDim * sizeof(double));
  if (!mSliceData || !mFreqCount || !mPsTmp)
    exitError("Allocating memory for image slice, frequency counts, or mPsTmp");
  mCurSlice = -1;
  clearAndSetSize(dim, hyper, tSize);
}

void SliceCache::clearAndSetSize(int dim, int hyper, int tSize)
{
  int fftXdim, i, j, rIndex;
  double freqInc;
  numXtiles = mHeader.nx / (tSize / 2) - 1;
  mNumYtiles = mHeader.ny / (tSize / 2) - 1;
  mPsArraySize = numXtiles * mNumYtiles * mPsDim;
  mMaxSliceNum = mMaxCacheSize * 1024 * 1024 / (mPsArraySize * sizeof(float));
  for (i = 0; i < (int)mCachedPS.size(); i++) {
    free(mCachedPS[i]);
    free(mCachedTileDone[i]);
    free(mCachedMeans[i]);
  }
  if (mTile)
    free(mTile);
  mTile = (float *)malloc(tSize * (tSize + 2) * sizeof(float));
  if (mTileToPsInd)
    free(mTileToPsInd);
  mTileToPsInd = (int *)malloc(tSize * (tSize + 2) * sizeof(int));
  if (!mTile || !mTileToPsInd)
    exitError("Allocating memory for mTile or PS index");

  // Compute the index for each component of FFT and the counts in each bin
  mTileSize = tSize;
  mNDim = dim;
  mHyperRes = hyper;
  fftXdim = (mTileSize + 2) / 2;
  freqInc = 1. / ((mNDim - 1) * hyper);
  for (i = 0; i < mPsDim; i++)
    mFreqCount[i] = 0;
  for (i = 0; i < fftXdim - 1; i++) {
    for (j = 0; j < fftXdim; j++) {

      // Here make bins with integer truncation
      rIndex = (int)(sqrt((double)(i * i + j * j)) / (fftXdim - 1) / freqInc);
      rIndex = B3DMIN(rIndex, mPsDim - 1);
      mTileToPsInd[i * fftXdim + j] = rIndex;
      mFreqCount[rIndex] += 2;
    }
  }

  /*for (i = 0; i < mPsDim; i++)
    printf("%d\n", mFreqCount[i]); */

  // Clear cache components
  mCachedPS.clear();
  mCachedMeans.clear();
  mCachedTileDone.clear();
  mSliceAngles.clear();
  mCachedSliceMap.clear();
  mOldestIdx = -1;

  if (debugLevel >= 2)
    printf("cacheSize is set to %d \n", mMaxSliceNum);
}

// Used to read in angle from file, now get from array
float  SliceCache::readAngle(int whichSlice)
{
  if (whichSlice < 0 || whichSlice >= mHeader.nz)
    exitError("Slice index is out of range");

  float currAngle;
  float *angles = mApp->getTiltAngles();
  if (angles) {
    currAngle = angles[whichSlice];
    if (debugLevel >= 1)
      printf("Slice %d is included, tilt angle is %f degrees. \n", whichSlice,
             currAngle);
    return currAngle * MY_PI / 180.0;
  } else {
    if (debugLevel >= 1)
      printf("No angle is specified, set to 0.0\n");
    return 0.0;
  }
}

//store needed slices in mNeededSlices.
//It needs to be called whenever the angle range changes.
void SliceCache::whatIsNeeded(float lowLimit, float highLimit, int &start,
                              int &end)
{
  mNeededSlices.clear();

  int k;
  float eps = 0.02f;
  float currAngle;
  float *angles = mApp->getTiltAngles();
  for (k = 0; k < mHeader.nz; k++) {
    if (angles) {
      currAngle = angles[k];
    } else {
      currAngle = 0.0;
    }
    if (currAngle < lowLimit - eps || currAngle > highLimit + eps)
      continue;
    mNeededSlices.push_back(k);
  }

  int totalSlice = mNeededSlices.size();
  if (totalSlice > 0) {
    start = mNeededSlices[0];
    end = mNeededSlices[totalSlice - 1];
  } else {
    end = -1;
    start = -1;
  }
}

//return the index of the slice in cache;
//return -1, if the slice is not in cache;
int SliceCache::cacheIndex(int whichSlice)
{
  int currSliceNum = mCachedPS.size();
  for (int i = 0; i < currSliceNum; i++) {
    if (mCachedSliceMap[i] == whichSlice)
      return i;
  }
  return -1;
}

std::vector<int> &SliceCache::optimalAccessOrder()
{
  mAccessOrder.clear();

  int neededSliceNum = mNeededSlices.size();
  for (int i = 0; i < neededSliceNum; i++) {
    if (cacheIndex(mNeededSlices[i]) > -1)
      mAccessOrder.insert(mAccessOrder.begin(), mNeededSlices[i]);
    else
      mAccessOrder.push_back(mNeededSlices[i]);
  }
  return mAccessOrder;
}

float *SliceCache::getHyperPS(int tileX, int tileY, int whichSlice,
                              double &mean)
{
  if (whichSlice < 0 || whichSlice >= mHeader.nz)
    exitError("slice Num is out of range");
  int sliceIdx = cacheIndex(whichSlice);
  int currCacheSize = mCachedPS.size();
  int tileIdx = tileX + tileY * numXtiles;
  float *retPS;
  int halfSize = mTileSize / 2;
  int tileXdim = mTileSize + 2;
  int fftXdim = tileXdim / 2;
  int idir = 0; //FFT direction;
  int ii, jj, ind, ind2, ix0, iy0;

  if (sliceIdx > -1) { // already in cache
    //if (debugLevel >= 2)
    //  printf("Slice %d is in cache and is included\n", whichSlice);

  } else if (currCacheSize < mMaxSliceNum) { //not in cache and cache not full
    float *newPS = (float *)malloc(mPsArraySize * sizeof(float));
    int *newDone = (int *)malloc(numXtiles * mNumYtiles * sizeof(int));
    float *newMeans = (float *)malloc(numXtiles * mNumYtiles * sizeof(double));
    if (!newPS || !newDone || !newMeans)
      exitError("Allocating memory for power spectra");

    if (debugLevel >= 2)
      printf("Slice %d is NOT in cache and is included \n", whichSlice);

    for (ii = 0; ii < numXtiles * mNumYtiles; ii++)
      newDone[ii] = 0;

    mCachedPS.push_back(newPS);
    mCachedMeans.push_back(newMeans);
    mCachedTileDone.push_back(newDone);
    mCachedSliceMap.push_back(whichSlice);
    mSliceAngles.push_back(readAngle(whichSlice));

    if (mOldestIdx == -1)
      mOldestIdx = currCacheSize;
    sliceIdx = currCacheSize;

  } else { // not in cache and cache is full
    if (debugLevel >= 2)
      printf("Slice %d is NOT in cache and replaces slice %d \n", whichSlice,
             mCachedSliceMap[mOldestIdx]);

    mCachedSliceMap[mOldestIdx] = whichSlice;
    mSliceAngles[mOldestIdx] = readAngle(whichSlice);
    sliceIdx = mOldestIdx;
    mOldestIdx = (mOldestIdx + 1) % mMaxSliceNum;
  }

  fflush(stdout);
  retPS = mCachedPS[sliceIdx] + tileIdx * mPsDim;
  if (mCachedTileDone[sliceIdx][tileIdx]) {
    mean = mCachedMeans[sliceIdx][tileIdx];
    return retPS;
  }

  // Have to compute the power spectrum
  // Read in slice if needed
  if (mCurSlice != whichSlice) {
    // Reallocate as floating point when necessary
    if (mDataOffset && mSliceMode != MRC_MODE_FLOAT) {
      free(mSliceData);
      mSliceData = (float *)malloc(mHeader.nx * mHeader.ny * 4);
      if (!mSliceData)
	exitError("Allocating memory for floating point image slice");
      mSliceMode = MRC_MODE_FLOAT;
    }

    if (mrc_read_slice(mSliceData, mFpStack, &mHeader, whichSlice, 'Z'))
      exitError("Reading slice %d", whichSlice);

    // Apply any offset needed. Done as float to avoid over/underflow.
    if (mDataOffset) {
      unsigned char *bdata = (unsigned char *)mSliceData;
      b3dUInt16 *usdata = (b3dUInt16 *)mSliceData;
      b3dInt16 *sdata = (b3dInt16 *)mSliceData;

      switch (mHeader.mode) {
      case MRC_MODE_BYTE:
	for (ii = mHeader.nx * mHeader.ny - 1; ii >= 0; ii--)
	  mSliceData[ii] = bdata[ii] + mDataOffset;
	break;
      case MRC_MODE_USHORT:
	for (ii = mHeader.nx * mHeader.ny - 1; ii >= 0; ii--)
	  mSliceData[ii] = usdata[ii] + mDataOffset;
	break;
      case MRC_MODE_SHORT:
	for (ii = mHeader.nx * mHeader.ny - 1; ii >= 0; ii--)
	  mSliceData[ii] = sdata[ii] + mDataOffset;
	break;
      case MRC_MODE_FLOAT:
	for (ii = 0; ii < mHeader.nx * mHeader.ny; ii++)
	  mSliceData[ii] += mDataOffset;
	break;
      }
    }
    mCurSlice = whichSlice;
  }

  // get the mTile
  ix0 = tileX * halfSize;
  iy0 = tileY * halfSize;
  sliceTaperInPad(mSliceData, mSliceMode, mHeader.nx, ix0, ix0 + mTileSize - 1,
                  iy0, iy0 + mTileSize - 1, mTile, tileXdim, mTileSize, mTileSize,
                  9, 9);

  // Get its mean and save it
  mean = 0.0;
  for (ii = 0; ii < mTileSize; ii++)
    for (jj = 0; jj < mTileSize; jj++)
      mean += mTile[ii * tileXdim + jj];
  mean /= (mTileSize * mTileSize);
  mCachedMeans[sliceIdx][tileIdx] = mean;

  // FFT and sum into the PS curve
  todfft(mTile, &mTileSize, &mTileSize, &idir);

  for (ii = 0; ii < mPsDim; ii++)
    mPsTmp[ii] = 0.;
  for (ii = 0; ii < mTileSize / 2; ii++) {
    for (jj = 0; jj < fftXdim; jj++) {
      ix0 = ii * fftXdim + jj;
      ind = ii * tileXdim + 2 * jj;
      ind2 = (mTileSize - 1 - ii) * tileXdim + 2 * jj;
      mPsTmp[mTileToPsInd[ix0]] +=
        mTile[ind] * mTile[ind] + mTile[ind + 1] * mTile[ind + 1] +
        mTile[ind2] * mTile[ind2] + mTile[ind2 + 1] * mTile[ind2 + 1];
    }
  }

  // Store into floats and return
  for (ii = 0; ii < mPsDim; ii++) {
    retPS[ii] = mPsTmp[ii];
    //printf("%d  %f\n", mFreqCount[ii], retPS[ii]);
  }
  mCachedTileDone[sliceIdx][tileIdx] = 1;
  return retPS;
}

float SliceCache::getAngle(int whichSlice)
{
  int sliceIdx = cacheIndex(whichSlice);

  if (sliceIdx >= 0)
    return mSliceAngles[sliceIdx];
  return readAngle(whichSlice);
}
