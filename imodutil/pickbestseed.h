#define MAX_MODELS 7

// Structure to hold data from one track, indexed by contour number
struct TrackData {
  int model;
  Ipoint midPos;
  int domain;
  int candIndex;
  int topBot;
  float residual;
  float edgeSDmean;
  float edgeSDmedian;
  float edgeSDsd;
  float elongMean;
  float elongMedian;
};

// Structure to hold data about a candidate point
struct Candidate {
  int contours[MAX_MODELS];
  int domain;
  Ipoint pos;
  int topBot;
  float score;
  float meanDeviation; 
  unsigned char clustered;
  unsigned char overlapped;
  unsigned char accepted;
  unsigned char pad;
};

// Structure for data about a grid point
struct GridPoint {
  int domain;
  float x, y;
  float areaFrac;
  float density;
  int exclude;
};

// Structure to hold data about a domain
struct GridDomain {
  int numCandidates;
  int numAccepted;
  int candStartInd;
  int numTracks;
  int trackStartInd;
  int numGridPts;
  int *gridPtInd;
  int numNeighbors;
  int neighbors[9];
};

//#define PRINT1(a,b) printf(#b" = %"#a"\n", b);
#define PRINT1(a) cout << #a << " = " << a << endl
#define PRINT2(a,b) cout << #a << " = " << a << ",  " #b << " = " << b << endl
#define PRINT3(a,b,c) cout << #a << " = " << a << ",  " #b << " = " << b << ",  " #c << " = " << c << endl
#define PRINT4(a,b,c,d) cout << #a << " = " << a << ",  " #b << " = " << b << ",  " #c << " = " << c << ",  " #d << " = " << d << endl

#define MAX_AREAS  1000

class PickSeeds
{
 public:
  PickSeeds();
  void main( int argc, char *argv[]);
  void addBestSpacedPoints(int topBot, int targNum, int numCandidates, float minSpacing);
  void addPointsInGaps(int topBot, int targNum, float termDens, float targSpacing,
                       float clusterThresh, int overlapThresh, float H, int numRings);
  void acceptCandidate(int index, int topBot);
  int domainIndex(float x, float y);
  void computeAreaFracs(float H);
  void getTrackDeviation(int co1, int mod1, int co2, int mod2, float &meanDev,
                         float &fracClose);
  void computeDensities(int topBot, float H);
  void reviseDensities(int acceptNew, float H);
  void outputDensities(int topBot);
  void outputNumAccepted(int twoSurf, int final);
  bool beadsAreClustered(Candidate *cand1, Candidate *cand2);
  
 private:
  Imod *mTrackMods[MAX_MODELS];
  TrackData *mTracks;
  GridDomain *mDomains;
  float mDelXdomain, mDelYdomain;
  int mNumXdomains, mNumYdomains;
  int mNumXgrid, mNumYgrid;
  int mNumAreaCont;
  int mAreaConts[MAX_AREAS];
  Imod *mAreaMod;
  float mAreaXmin, mAreaXmax, mAreaYmin, mAreaYmax;
  std::vector<Candidate> mCandidates;
  int mNumCandidates;
  int mNumAccepted[3];
  GridPoint *mGridPoints;
  int *mCandidList;
  int *mAcceptList;
  int *mRankIndex;
  int mPhase;
  float mBeadSize;
  float mDelXgrid, mDelYgrid;
  int mVerbose;
  float mOverlapTarget;
  char *mDensPlotName;

  float mCloseDiamFrac;
  float mExcludeFac;
  float mRingSpacingFac;
  int mNumRings;
  float mHighestTilt;
  float mCosRot, mSinRot;
  float mCosTilt;
  float mClusterCrit;
};
