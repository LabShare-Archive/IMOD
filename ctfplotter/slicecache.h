/*
 * slicecache.h - Header for SliceCache class
 *
 *  $Id$
 *
 *  $Log$
 *
 */
#ifndef SLICECACHE_H
#define SLICECACHE_H

#include "mrcslice.h"
#include<vector>

class SliceCache
{

  public:
   SliceCache(int cacheSize, int invertAngles);
   void initCache(char *fnStack, char *fnAngle, int dim, int hyper, int tSize,
                  int& nx, int &ny, int &nz);
   float readAngle(int whichSlice);
   void whatIsNeeded(float lowLimit, float highLimit, int &startSliceNum, int&
       endSliceNum); 
   std::vector<int>& optimalAccessOrder();
   float *getHyperPS(int tileX, int tileY, int whichSlice, double &mean);
   float   getAngle(int whichSlice);
   void clearAndSetSize(int dim, int hyper, int tSize);
   int *getFreqCount() {return mFreqCount;};
      
  private:
   int mMaxCacheSize; // in megs;
   int mMaxSliceNum;
   FILE *mFpStack;
   FILE *mFpAngle;
   MrcHeader mHeader;
   int mSliceMode;
   float *mSliceData;
   int numXtiles, mNumYtiles;
   int mCurSlice;
   int mNDim;
   int mHyperRes;
   int mTileSize;
   int mPsDim;
   int mPsArraySize;
   float *mTile;
   int *mTileToPsInd;
   int *mFreqCount;
   double *mPsTmp;
   double mAngleSign;

   std::vector<float *> mCachedPS; 
   std::vector<float *> mCachedMeans; 
   std::vector<int *> mCachedTileDone; 
   std::vector<int>   mCachedSliceMap; // which slices are in the cache;
   std::vector<float>    mSliceAngles; // tilt angles in RADIAN for slices in cache;

   std::vector<int>   mNeededSlices; // slices the user asks;
   std::vector<int>   mAccessOrder;
   int mOldestIdx; //the one that stays in the cache longest will be replaced next;

   int cacheIndex(int whichSlice);

};

#endif
