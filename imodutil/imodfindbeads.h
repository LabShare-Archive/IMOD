#ifndef IMODFINDBEADS_H

// Structure to hold peaks
typedef struct peak_list_entry {
  float xcen, ycen;
  int iz;
  float ccc;
  float integral;
  float peak;
  float cenmean;
  float annmean;
  float median;
} PeakEntry;

// Some limits for arrays
#define MAX_BINS 10000
#define MAX_GROUPS 10
#define KERNEL_MAXSIZE 7
#define MAX_AREAS  1000
#define MAX_LINE 160

class FindBeads
{
 public:
  FindBeads();
  void main( int argc, char *argv[]);
 private:

  // Methods
  Islice *readSliceAsFloat(int iz);
  float templateCCCoefficient(float *array, int nxdim, int nx, int ny, float *templateIm,
                              int nxtdim, int nxt, int nyt, int xoffset, int yoffset);
  void areaContListCheckErr(int iz);
  int pointInsideBoundary(Iobj *obj, Ipoint *pnt);
  void kernelHistoPL(float *element, int skipZeroCCC, float *select, float selMin,
                     float selMax, float *bins, int numBins, float firstVal,
                     float lastVal, float h, int verbose, FILE *dumpfp);
  int findHistoDipPL(float &histDip, float &peakBelow, float &peakAbove, char *vkeys);
  void printArray(float *filtBead, int nxdim, int nx, int ny);
  void selectedMinMax(float *element, float *select, float selMin, float selMax,
                      float &minVal, float &maxVal, int &ninRange);
  void setupSizeDependentVars();
  void analyzeBackgroundGroups();
  void averageBeads(int izst, int iznd);
  float findStorageThreshold(float &histDip);
  void searchCorrelationPeaks(Islice *sl, int iz);
  float extractDiameter(float *oneBead);
  void profile(const char *format, ...);

  // Member variables
  float mRegHist[MAX_BINS], mKernHist[MAX_BINS];
  MrcHeader mInHead;
  int mAverageFallback;
  int mStorageFallback;
  Islice **mCachedSlices;
  bool mUseSliceCache;
  float *mFiltSlice, *mCorrSlice, *mFullBead, *mOneBead, *mSplitBead;
  PeakEntry *mPeakList;
  int *mZlist;
  int mDumpType;
  float mThreshold;
  float mPeakThresh;
  float mCenterWeight;
  int mLightBeads;
  float mAnnulusPctile;
  float mMinRelativePeak;
  float mMinSpacing;
  float mScaledSize;
  float mBeadSize, mScaleFactor;
  int mBoxSize;
  int mNumPeaks;
  int mNxPad, mNyPad, mNxpDim;
  int mNxOut, mNyOut;
  float mRadCenter, mRadInner, mRadOuter;
  float mMinDist, mMatchCrit;
  int mNumGroups;
  char *mVkeys;
  float mMinInterp;
  int mLinearInterp;
  float *mWriteSlice;
  float mXoffset, mYoffset;
  int mNxIn, mNyIn;
  int mBoxScaled;
  int mBoxScaledOrig;
  float mPeakMax;
  FILE *mDumpFp;
  FILE *mInFp;
  int mMinGuess;
  int mNumObjOrig;
  float *mAlignXshift, *mAlignYshift;
  int mListSize;
  float *mFiltBead;
  Imod *mAreaMod;
  int mAreaConts[MAX_AREAS];
  int mNumAreaCont;
  int mMeasureToUse;
  int mExcludeAreas;
  float mBeadCenOfs;
  int mAdjustSizes;
  double mWallStart;
  double mWallLast;
  bool mProfiling;
  float mMinSizeForAdjust;
  float mMaxAdjustFactor;
  float mMinSizeChangeForRedo;
};


#endif
