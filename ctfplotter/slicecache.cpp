/*
* slicecache.cpp - build a slice cache to speed up slice loading.
*
*  Author: Quanren Xiong
*
*  Copyright (C) 2008 by Boulder Laboratory for 3-Dimensional Electron
*  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
*  Colorado.  See dist/COPYRIGHT for full copyright notice.
* 
*  $Id$
*  Log at end of file
*/

/**************how to use this class  **********************
  1) initCache() to init the class;
  2) whatIsNeeded() to tell the class what slices will be needed;
  3) accessOrder=optimalAccessOrder(). It needs to be called before whenever
     getSlice() is called. 
  4) use getSlice(accessOrder[i]), getAngle(accessOrder[i])
     to read slices starting from accessOrder[0].

  *************************************************************/
#include "mrcslice.h"
#include "parse_params.h"
#include "slicecache.h"
#include<vector>

extern int debugLevel;

#define MY_PI 3.1415926

SliceCache::SliceCache(int cacheSize)
{ 
     fpStack=NULL;
     fpAngle=NULL;
     maxCacheSize=cacheSize;
} 

//init SliceCache and clear its old contents
//It needs to be called whenever the tomogram it manages changes.
void SliceCache::initCache(char *fnStack, char *fnAngle, int &nx, int &ny, int
    &nz)
{
  if(fpStack) fclose(fpStack); 
  if( (fpStack=fopen(fnStack, "rb"))==0 )
    exitError("could not open input file %s\n", fnStack);

  if(fpAngle) fclose(fpAngle);
  if(fnAngle){
    if( (fpAngle=fopen(fnAngle, "r"))==0 ){
      printf("could not open angle file %s, tiltAngle is set to 0.0\n",fnAngle);
    }
  }else fpAngle=NULL;

  /* read header */	
  if (mrc_head_read(fpStack, &header)) 
    exitError("could not read header of input file %s\n",  fnStack);
  /* sliceMode = sliceModeIfReal(header.mode);
    if (sliceMode < 0) 
   printf("ERROR: %s - File mode is %d; only byte, short, integer allowed\n",
        "ctfplotter", header.mode);
  */
  sliceMode =SLICE_MODE_FLOAT;
  maxSliceNum=maxCacheSize*1024*1024/(header.nx*header.ny*sizeof(float));
  nx=header.nx;
  ny=header.ny;
  nz=header.nz;

  for(int i=0;i<cachedSlices.size();i++) sliceFree(cachedSlices[i] );
  cachedSlices.clear();
  sliceAngles.clear();
  cachedSliceMap.clear();
  oldestIdx=-1;

  if(debugLevel>=2)
   printf("cacheSize is set to %d \n", maxSliceNum);
}

//read in the angle for a slice from file 
float  SliceCache::readAngle(int whichSlice)
{
 if( whichSlice<0 || whichSlice>=header.nx )
   exitError("Slice index is out of range");
 
 int k;
 float currAngle;
 if(fpAngle){
   rewind(fpAngle);
   char angleStr[30];
   for(k=0;k<=whichSlice;k++) fgets(angleStr, 30, fpAngle);
   sscanf(angleStr, "%f", &currAngle);
   if( debugLevel>=1)
    printf("Slice %d is included, tilt angle is %f degrees. \n", k-1, currAngle);
   return currAngle*MY_PI/180.0;
 }else{
   if( debugLevel>=1)
    printf("No angle is specified, set to 0.0\n");
   return 0.0;
 }
}

//store needed slices in neededSlices.
//It needs to be called whenever the angle range changes.
void SliceCache::whatIsNeeded(float lowLimit, float highLimit, int &start, int
    &end)
{
  neededSlices.clear();

  int k;
  char angleStr[30];
  float currAngle;
  if(fpAngle) rewind(fpAngle);
  for(k=0;k<header.nz;k++){
    if( fpAngle && fgets(angleStr, 30, fpAngle) ){
      sscanf(angleStr, "%f", &currAngle);
    }else{
      currAngle=0.0;
    }
    if( currAngle<lowLimit || currAngle>highLimit) continue;
    neededSlices.push_back(k);
  }
  
  int totalSlice=neededSlices.size(); 
  if( totalSlice>0){
    start=neededSlices[0];
    end=neededSlices[totalSlice-1];
  }else{
    end=-1;
    start=-1;
  }
}

//return the index of the slice in cache;
//return -1, if the slice is not in cache;
int SliceCache::cacheIndex(int whichSlice)
{
  int currSliceNum=cachedSlices.size();
  for(int i=0;i<currSliceNum;i++)
  {
    if( cachedSliceMap[i]==whichSlice )
      return i;
  }
  return -1;
}

std::vector<int> & SliceCache::optimalAccessOrder()
{
   accessOrder.clear();

   int neededSliceNum=neededSlices.size();
   for(int i=0; i<neededSliceNum;i++)
   {
     if( cacheIndex( neededSlices[i] )>-1 )
       accessOrder.insert( accessOrder.begin(), neededSlices[i] );
     else
       accessOrder.push_back( neededSlices[i] );
   }
   return accessOrder;
}

Islice * SliceCache::getSlice(int whichSlice)
{
  if( whichSlice<0 || whichSlice>=header.nz )
    exitError("slice Num is out of range");
  int sliceIdx=cacheIndex(whichSlice);
  int currCacheSize=cachedSlices.size();
  
  if( sliceIdx>-1){ // already in cache
    if( debugLevel>=2)
       printf("Slice %d is in cache and is included\n", whichSlice);
    return cachedSlices[sliceIdx];
  }else if( currCacheSize<maxSliceNum ){ //not in cache and cache is not full
    Islice *newSlice=sliceCreate(header.nx, header.ny, sliceMode);
    if(!newSlice) exitError("could not create slice for input\n");
    //if( mrc_read_slice(newSlice->data.b, fpStack, &header, whichSlice, 'Z') )
    if( mrcReadFloatSlice(newSlice->data.f, &header, whichSlice) )
      exitError("could not read slice\n");
    if( debugLevel>=2)
       printf("Slice %d is NOT in cache and is included \n", whichSlice);

    /*convert slice to floats
    if(sliceMode !=SLICE_MODE_FLOAT)
      if( sliceNewMode(newSlice, SLICE_MODE_FLOAT)<0 )
        exitError("could not convert slice to float\n");
    */

    cachedSlices.push_back(newSlice);
    cachedSliceMap.push_back(whichSlice);
    sliceAngles.push_back( readAngle(whichSlice) );

    if(oldestIdx==-1)
      oldestIdx=currCacheSize;

   return newSlice;
  }else{ // not in cache and cache is full
      //if( mrc_read_slice(cachedSlices[oldestIdx]->data.b, fpStack, &header, whichSlice, 'Z') )
      if( mrcReadFloatSlice(cachedSlices[oldestIdx]->data.f, &header, whichSlice) )
         exitError("could not read slice\n");
      if( debugLevel>=2)
       printf("Slice %d is NOT in cache and replaces slice %d \n", whichSlice, cachedSliceMap[oldestIdx]);

      cachedSliceMap[oldestIdx]=whichSlice;
      sliceAngles[oldestIdx]=readAngle(whichSlice);

      Islice *retVal=cachedSlices[oldestIdx];
      oldestIdx=(oldestIdx+1)%maxSliceNum; 

      return retVal;

  }//else
}

float SliceCache::getAngle(int whichSlice)
{
  int sliceIdx=cacheIndex(whichSlice);

  if( sliceIdx<0) exitError("the cache is a mess, exit");
  else return sliceAngles[sliceIdx];
}

/*

   $Log$
*/
