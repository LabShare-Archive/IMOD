#ifndef _svlMarkerCorrespondenceLBModel_h_
#define _svlMarkerCorrespondenceLBModel_h_

#include <list>
#include <vector>

//files for Array2D
#include "../external/tnt_i_refvec.h"
#include "../external/tnt_array1d.h"
#include "../external/tnt_array2d.h"

//files from Stair Vision Library for Inference in Graphs
#include "svl/lib/pgm/svlClusterGraph.h"
#include "svl/lib/pgm/svlFactor.h"
#include "svl/lib/pgm/svlMessagePassing.h"


using namespace std;

//typedef from GGL library
typedef TNT::Array2D<double>     gglMatrix;
typedef vector<int>          gglIntVector;
typedef vector< gglIntVector > gglIntVectorVector;
typedef vector<double>       gglVector;
typedef vector< gglVector >    gglVectorVector;
typedef vector<int>          varsVec;
typedef vector<int>          varsVec;
typedef vector<int>          cardVec;



#define SetFunction(name,member,type)\
inline void name(type val) { member = val; _initialized = false; };

/** This class construct an lbModel from an image.
    Unlike the standard lbModel implementation, it also
    creates the variables, cards and graph for itself

    To use:
    1) construct with an image and contour
    2) read options
    3) call BuildModel();
**/

class svlMarkerCorrespondenceLBModel
{
 public:

  static const string usage;


  svlMarkerCorrespondenceLBModel(gglMatrix const&  MarkerLocations1,gglMatrix const& MarkerLocations2, gglMatrix const& MarkerCandidates2,gglMatrix const& SPMatrix, string test_name,int lock_pots_file_exists, istream &  infilename,  istream &  infilename_lock_pots, int  _debug_init_beliefs ,int  _debug_pair_pots ,int  _pair_pots_m1 , int  _pair_pots_m2 , double _dist_diff_intcpt, int _verbose, int _mtime);
  ~svlMarkerCorrespondenceLBModel();

  /** Runs inference and returns the beliefs **/
  gglMatrix GetFinalMarginalBeliefs();
  //gglMatrix GetFinalBeliefs();
  gglMatrix GetInitialBeliefs();

  gglIntVectorVector _PairToClique;
  gglVectorVector _PairDistances;
  //
  /*
  lbVarsList const& Vars() const { return *_lbVars; };
  lbCardsList const& Cards() const { return *_lbCards; };
  lbGraphStruct const& Graph() const { return *_lbGraph; };
  */
  vector<int> const& AllowedVals(int i) const { return _allowVals[i]; };

  //
  void PrintOptions(ostream& Out) const;

  // Important: this method shuold be invoked BEFORE using the class and after reading the options
  void BuildModel(vector< vector<int> >* forcedAllowedValues = NULL,bool replaceExistingValues = false);
  void ReadOptions(int argc, char *argv[]);

  // "translate" contour to full assignment with closest points in the model
  //lbFullAssignment* FullAssignment(gglContour const& C,bool exactMatches = false) const;

  // transfer weights from the model potentials (learned params) to the contours
  //void TransferWeights(gglContour& C) const;

//static svlMessagePassingInference* Inference(svlMarkerCorrespondenceLBModel& model, bool maxProduct, int maxMessages, double smoothParam, int treeK, bool doProdDivide);

double norm_dot(double x1, double y1, double x2, double y2, double x3, double y3, double x4, double y4, double dist_thr_max_a,double dist_thr_min_a , double dist_thr_max_b,double dist_thr_min_b );

double dist_l2(double x1, double y1, double x2, double y2);
double dist_l1(double x1, double y1, double x2, double y2);



 protected:

  /************************************************************
   * Construction of the MRF for the correspondence problem
   *
   * Singleton potentials account for occlusion prior
   * Pairwise potentials account for geodesic distance
   * Triplewise potentials account for angle relations
   *
   * All potentials are neighbors via the singleton potentials
   *
   ***********************************************************/
 private:
  void InitValues();
  void InitBuild(vector< vector<int> >* forcedAllowedValues = NULL,bool replaceExistingValues = false);
  void InitAllowedValues(vector< vector<int> >* forcedAllowedValues = NULL,bool replaceExistingValues = false);
  void Clear();
  void ComputeNearFarThresh();
  void ComputeFarGeoThresh();
  void ChooseRandomEdges();

  // Smart choice of assignment methods
  void ComputeAllowedValues();
  void AllowAllValues(set<int> LMset = set<int>(),bool withDummy = true);
  void AllowValuesByFeatures(bool withSamples = false);
  void AllowValuesByFeaturePairs(bool withSamples = false);
  void AllowValuesByInitialProximity();
  void AllowValuesByForce(vector< vector<int> > const& forcedAllowedValues,bool replaceExistingValues);

  // Build methods
//  int UpdateMeasure(varsVec vvec,int cind,lbFeatureTableMeasure_Sptr meas);
  void BuildCards();

  void BuildSingletonPotentials();

  void BuildFeatureCliques(vector<int>& cliques);
  void BuildFeaturePotentials(vector<int> const& cliques);

  void BuildPairwiseCliques();
  void BuildPairwisePotentials();

  void BuildTriplewiseCliques(vector<int>& cliques);
  void BuildTriplewisePotentials(vector<int> const& cliques);


  typedef pair<int,int> gglMeasAndParamInd;
  static bool IsValidParam(gglMeasAndParamInd const& MP);
  double GetParam( gglMeasAndParamInd const& MP ) const;
  void  ReadConfig(istream & in);
  void  ReadLockPots(istream & in);

  //void ResetCache() { _cachedImageData.clear(); };


 private:

  // fastInf elements
  /*
  lbVarsList_ptr _lbVars;
  lbCardsList_ptr _lbCards;
  lbGraphStruct_ptr _lbGraph;
  lbMeasureDispatcher _lbMD;
  */
  //substitution by svl library elements
  svlClusterGraph _lbGraph;
  vector<int> _lbCards;





  // loopy
  bool _inferenceBuilt;
  bool _maxProduct;
  int _maxMessages;
  double _smooth;
  int _treeK;
//  lbMessageQueueType _MType;
  bool _doProdDivide;



  // "smart": allowed assignment for each variable
  // for each point in the model, keep a list of allowed image edge point assignments
  //gglSmartMethod _SmartM;
  double _proximityThreshold; // for S_INITIAL_PROXIMITY: Only use points within this distance
  unsigned int _min_cands; //minimum number of candidates per marker to enforce
  vector< vector<int> > _allowVals;
  //gglPairwiseFeature _PairwiseFeature;

  // elements of model
  bool _combinePotentials;
  bool _priorLocations;

  gglMatrix _MarkerLocations;
  gglMatrix _MarkerLocations2;
  gglMatrix _MarkerCandidates;
  gglMatrix _SPMatrix;
  string _test_name;
  int		_sing_po_choice;
  int		_pair_po_choice;
  int           _intcpt_b;
  int           _min_pw_cliques;
  double 	_max_mrkr_pair_dist;
  double   *_dist_thr_min;
  double   *_dist_thr_max;
  int   *_lock_pot;
  int   *_locked_pot_val;
  int   **_CandList; //list of candidates per marker
  int   *_CandListSz; //size (occupancy) of list of candidates per marker
  //int   _PWTableSz; //allowed (pruned) size of table per pairwise clique
  svlMessagePassingInference *_lbInfer;
  //gglPairwiseType pw;

  int debug_initial_beliefs; //only for debugging initial beliefs
  int verbose; //print out messages
  int mtime; //measure time
  int  debug_pair_pots ;
  int  pair_pots_m1 ;
  int  pair_pots_m2 ;
  double dist_diff_intcpt; //for distance decay function
  double max_mrkr_pair_dist;
  double 	gcan_potential; //initial potential for garbage can
  double _ppot_scale_factor;
  int  _NumMarkers;
  int  _NumCandidates;
  int  _NumMarkers_for_dist;
  // strengths
  bool _normalizeSingleton;
  bool _useModelWeights;
  vector<double> _pairwiseStrengths;
  vector<double> _featureStrengths;
  double _AnLogStrength;     // strength of angle potentials

  // fractions / default values
  double _occludFraction;    // expected occlusion fraction
  double _LOccludValue;       // potential value for occlusion
  double _overlapValue;      // potential value for point overlap
  double _LMinValue;          // minimum allowed potential value

  // distances
  double _lowVarThresh;      // threshold for low variance edge selection
  double _edgeFraction;      // fraction of near and far edges
  double _geoEdgeFraction;   // fraction of far geodesic distances
  double _nearThresh;        // computed thresholds
  double _farThresh;
  set< pair<int,int> > _randomEdges;    // list of random edges
  double _farGeoThresh;
  double _angleFraction;

  // appearance
  double _Ivar;
  double _CRvar;
  double _CGvar;
  double _CBvar;
  double _InOutThresh;

  // features
  int _FbestK;           // num of candidates

  bool _graphBuilt;
  bool _modelBuilt;
  bool _initialized;
  double _avgLineLength; // keep track of avg line length and count
  bool _optionsRead;
  bool _revisingFeatures;

  // keep track of clique indices
  vector<int> _singleC;
  vector<int> _featureC;
  vector< vector<int> > _pairwiseC;
  vector<int> _angleC;

  //
  //map< pair<int,int> , int > _edgeToClique;
  //map< pair<int,gglPairwiseType> , gglMeasAndParamInd > _edgeWithTypeParam; // param and type to meas and param indice
  //vector<gglMeasAndParamInd> _occlusionParams;
  //vector< vector<gglMeasAndParamInd> > _featureParams;

  // cache measures when Image changes
/*
  struct gglImageCacheElement {
    vector< vector<int> > _allowVals;
    map<int,lbMeasure_Sptr> _measures;
  };
*/
  //map< gglImage* , gglImageCacheElement > _cachedImageData;
};


/** INLINES **/
inline void  svlMarkerCorrespondenceLBModel::ReadConfig(istream & in)
{

    in >>  _NumMarkers_for_dist;
    if(verbose)cerr << "read in num markers = "<<_NumMarkers_for_dist<<"\n";

    in >> _sing_po_choice ;
    if(verbose)cerr << "read in single potential method "<<_sing_po_choice<<"\n";
    in >> _pair_po_choice ;
    if(verbose)cerr << "read in pairwise potential method "<<_pair_po_choice<<"\n";
    in >> _intcpt_b ;
    if(verbose)cerr << "read in rolloff factor k2 of "<<_intcpt_b<<"\n";
    in >> _max_mrkr_pair_dist;
    if(verbose)cerr << "read in maximum marker pair distance for pairwise cliques of "<<_max_mrkr_pair_dist<<"\n";
    in >> gcan_potential ;
    if(verbose)cerr << "read in garbage can potential: "<<gcan_potential<<"\n ";
    in>>_ppot_scale_factor;
    if(verbose)cerr << "read in pairwise potential scale factor: "<<_ppot_scale_factor<<"\n ";
    in >> _min_pw_cliques;
    if(verbose)cerr << "read in minimum number of pairwise cliques: "<<_min_pw_cliques<<"\n ";


    //in >> _PWTableSz;
    //cerr << "read in Max PW Table Size:  "<<_PWTableSz<<"\n ";
    in >> _proximityThreshold ;
    if(verbose)cerr << "read in proximity threshold:  "<<_proximityThreshold <<"\n ";
    in >> _min_cands ;
    if(verbose)cerr << "read in min number of candidates :  "<<_min_cands <<"\n ";

    _dist_thr_min=new double[_NumMarkers_for_dist];
    _dist_thr_max=new double[_NumMarkers_for_dist];
    _lock_pot = new int[_NumMarkers_for_dist];
    _locked_pot_val = new int[_NumMarkers_for_dist];
    for (int i=0;i<_NumMarkers_for_dist;i++){

//initialize lockdown potentials
    _lock_pot[i]=0;
    _locked_pot_val[i]=-1;

    in >> _dist_thr_min[i] ;
    in >> _dist_thr_max[i] ;
    if(verbose)cerr << "read in min distance threshold : "<<i<<": "<<_dist_thr_min[i]<<"\n";
    if(verbose)cerr << "read in max distance threshold : "<<_dist_thr_max[i]<<"\n";
    //return dist_thr_max;
    }
}

inline void  svlMarkerCorrespondenceLBModel::ReadLockPots(istream & in){

int marker,candidate;
if(in){
while (!in.eof()){
    in >> marker;
    in >> candidate;
    _lock_pot[marker]=1;
    _locked_pot_val[marker]=candidate;
    if(verbose)cerr << "locked marker  "<<marker <<" to candidate "<<candidate<<"\n";
    }
}
    if(verbose)cerr << "finished with locked markers\n ";

}



inline

bool svlMarkerCorrespondenceLBModel::IsValidParam(gglMeasAndParamInd const& MP)
{
  if ( MP.first==-1 || MP.second==-1 )
    return false;
  else
    return true;
}


#endif


#ifndef _gglPWPotsList_h_
#define _gglPWPotsList_h_
class PairwiseAssignment{
  public:
    int valA;
    int valB;
    double pot_value;
    inline bool operator<(PairwiseAssignment const& P) const
    {
          return pot_value<P.pot_value;
    }

};

typedef list<PairwiseAssignment> gglPWPotsList;
#endif
