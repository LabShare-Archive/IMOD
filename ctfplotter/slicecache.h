#ifndef SLICECACHE_H
#define SLICECACHE_H

#include "mrcslice.h"
#include<vector>

class SliceCache
{

  public:
   int maxCacheSize; // in megs;
   int maxSliceNum;
   FILE *fpStack;
   FILE *fpAngle;
   MrcHeader header;
   int sliceMode;

   SliceCache(int cacheSize);
   void initCache(char *fnStack, char *fnAngle, int& nx, int &ny, int &nz);
   float readAngle(int whichSlice);
   void whatIsNeeded(float lowLimit, float highLimit, int &startSliceNum, int&
       endSliceNum); 
   std::vector<int>& optimalAccessOrder();
   Islice* getSlice(int whichSlice);
   float   getAngle(int whichSlice);

      
   std::vector<Islice *> cachedSlices; 
   std::vector<int>   cachedSliceMap; // which slices are in the cache;
   std::vector<float>    sliceAngles; // tilt angles in RADIAN for slices in cache;

   std::vector<int>   neededSlices; // slices the user asks;
   std::vector<int>   accessOrder;
   int oldestIdx; //the one that stays in the cache longest will be replaced next;

  private:
   int cacheIndex(int whichSlice);

};

#endif
