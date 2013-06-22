/*
 * slicecache.h - Header for SliceCache class
 *
 *  $Id$
 *
 *  $Log$
 *  Revision 1.3  2010/03/09 06:24:52  mast
 *  Change arguments to const char* to take latin1 from QString
 *
 *  Revision 1.2  2009/08/10 22:34:39  mast
 *  General reworking of program
 *
 *
 */
#ifndef SLICECACHE_H
#define SLICECACHE_H

#include "mrcslice.h"
#include<vector>

class MyApp;

class SliceCache
{

  public:
  SliceCache(int cacheSize, MyApp *app);
   void initCache(const char *fnStack, int dim, int hyper,
                  int tSize, int& nx, int &ny, int &nz);
   void setDataOffset(float inval) {mDataOffset = inval;};
   float readAngle(int whichSlice);
   void whatIsNeeded(float lowLimit, float highLimit, int &startSliceNum, int&
       endSliceNum); 
   std::vector<int>& optimalAccessOrder();
   float *getHyperPS(int tileX, int tileY, int whichSlice, double &mean);
   float   getAngle(int whichSlice);
   void clearAndSetSize(int dim, int hyper, int tSize);
   int *getFreqCount() {return mFreqCount;};
   MrcHeader *getHeader() {return &mHeader;};
   ~SliceCache();
      
  private:
   MyApp *mApp;
   int mMaxCacheSize; // in megs;
   int mMaxSliceNum;
   FILE *mFpStack;
   MrcHeader mHeader;
   int mSliceMode;
   float *mSliceData;
   float mDataOffset;
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
