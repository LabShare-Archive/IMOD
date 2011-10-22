#include "dialog_frame.h"
class QPushButton;
class QCheckBox;

class QLabel;
class QSpinner;
class QButtonGroup;
class QRadioButton;
class QGridLayout;
class QVBoxLayout;
class QComboBox;
class QSpinBox;
class QGroupBox;
class QVBoxLayout;
class QSpacerItem;
class QEvent;

#include "imodplugin.h"
#include "dia_qtutils.h"
#include "icontextra.h"
//#include "mkmesh.h"

#include <qstring.h>
#include <vector>
using namespace std;

//############################################################

//-------------------------------
//## CONSTANTS:

enum intmodes			    { INT_NO_INTERPOLATE, INT_LINEAR, INT_SPHERICAL, INT_SMOOTH,
	                      INT_SMOOTH_PTS, INT_TUBULAR, INT_DEW_DROP, INT_NUM_INT_MODES };

enum tilingmethod		  { TM_AUTO, TM_CONSERVE_LENGTH, TM_MIN_SA, TM_FEATURE_RECOG };
enum surfacemethod		{ SR_AUTO, SR_TOUCHING, SR_CENTER_OVERLAP, SR_WITHIN_MIN_DIST,
	                      SR_FRACTION_OVERLAP, SR_MBR_TOUCH, SR_USER_DEFINED };
enum branchingmethod	{ BR_BRANCHING_OFF, BR_MERGE_CONTS, BR_BRIDGE_GAPS, BR_PT_BELOW };

enum ptmethod         { PT_FOUR_PTS, PT_CONVEX_PTS };

const float TENSILE_FRACT = 1.0f;

const int NUM_SAVED_VALS = 13;


//-------------------------------
//## INTERPOLATOR WINDOW:

class Interpolator : public DialogFrame
{
	Q_OBJECT

 public:
	Interpolator(QWidget *parent, const char *name = NULL);
	~Interpolator() {};
	
 public slots:
	
  void loadSettings();
  void saveSettings();
  void keepOnTop(bool state);
  
	void interpolateContour();
	void interpolateContourBtn();
	void interpolateLinearWithLastContour();
  void deleteNearbyInterpolatedContours();
  void findNextIsolatedContour();
  void findNextBiggestHoleBetweenKeyConts();
  
  void toggleInterp();
  bool countContoursCurrObj( int &interpolated, int &key, int &totalNonEmpty );
	void clearInterpolatedContours();
	void regenerateInterpolation();
	void changeInterpContRange();
  void findIsolatedContours();
  void printModelInterpInfo();
  void printObjectSliceInterpInfo();
  void moreActions();
  void moreSettings();
	void test();
	
	void changeType( int value );
  void changeTypeSelected( int newType );
	void changeZBridge( int value );
	void changeLineTracker();
	void changeTilingMethod( int value );
	void changeBranchingMethod( int value );
	void changeSurfaceMethod( int value );
	void changeMinDist( int value );
	void changeOverlap( int value );
	int  getSurfaceResolveMethod();
	int  getTilingMethod();
	void buttonPressed(int);
	
 protected:
	void helpPluginHelp();
	void closeEvent ( QCloseEvent * e );
	void keyPressEvent ( QKeyEvent * e );
	void keyReleaseEvent ( QKeyEvent * e );
	
 private:
	
	QButtonGroup *typeButtonGroup;
	QRadioButton *typeRadio_None;
	QRadioButton *typeRadio_Linear;
	QRadioButton *typeRadio_Spherical;
	QRadioButton *typeRadio_Smooth;
	
	QGroupBox *grpOptions;
	QGridLayout *gridLayout1;
	QLabel *lblZBridge;
	QSpinBox *zBridgeSpinner;
	QCheckBox *applyLineTrackerCheckbox;
	
	QGroupBox *grpActions;
	QVBoxLayout *vboxLayout1;
  QPushButton *applyInterpolationButton;
  QPushButton *clearAllInterpButton;
	QPushButton *regenerateInterpButton;
  
	QGroupBox *grpSurface;
	QGridLayout *gridLayout2;
	QComboBox *surfaceCombo;
	QLabel *lblSurface;
	QLabel *lblBranchMethod;
	QComboBox *branchMethodCombo;
	QComboBox *tilingMethodCombo;
	QLabel *lblTilingMethod;
  
  QWidget     *widget1;
  QGridLayout *gridLayout3;
  QPushButton *moreActionsButton;
  QPushButton *moreSettingsButton;
};


//-------------------------------
//## INTERPOLATOR DATA STRUCTURE:

   // contains all local plugin data


struct InterpolatorData
{
	ImodView    *view;			// NOTE: members are in /include/imodP.h
	Interpolator *window;
	
	int interType;          // the type of interpolation the user is using (see enum "intmodes")
	int tilingMethod;       // the type of tiling method being used (see enum "tilingmethod")
	int branchingMethod;		// the type of branching method being used to generted interpolated contours (see enum "branchingmethod")
	int surfResolveMethod;	// the type of surface resolution method being used to determine which contours are part of the same surface (see enum "surfacemethod")
  int ptResolveMethod;    // the type of point connecting method used when tilingMethod is set to TM_CONSERVE_LENGTH (see enum "ptmethod")
  
	int zBridge;                    // number of slices over which to connect contours for interpolation.
	float interSepDistBetweenConts;	// the X-Y distance contours are allowed to be apart, but still considered part of the same surface (when surfResolveMethod == SR_WITHIN_MIN_DIST).
	float interFractOverlap;        // percentage of smaller contours which must overlap the bigger one to be considered in the same same surface (when surfResolveMethod == SR_FRACTION_OVERLAP).
	bool interLineTracker;          // if 1, then line tracker is applied to generated/interpolated contours ( %%%%%%%%%%% NOT YET IMPLEMENTED %%%%%%%%%%% )
  int minHoleSize;                // minimum hole size to find when [h] is pressed
  int maxGapSize;                 // maximum distance used in "edit_findNextIsolatedContour"
	bool hideSurfSettings;					// if true: will hide surface settings area
	
  //** OTHER:
  
  bool deselectAfterEnter;    // wether the current contour is deselected after [Enter]
  int selectedAction;         // the last selected action under "More Actions"
  
	int contIdxLastInterp;			// the contour index of the last contour interpolated
	int objIdxLastInterp;				// the object  index of the last contour interpolated
	
  bool initialized;           // is set to true after values have been set
  int xsize, ysize, zsize;    // size of the image / tomogram
};



//-------------------------------
//## INTERPOLATION EVENT STRUCTURE:

   // contains information regenerated for each interpolation event and used
   // in determining connected contours etc.


struct ContInfo
{
  int  idx;             // index of the contour within the object
  int  z;               // the z value of the contour
  bool loaded;          // wether the minimum bounding box has been calculated
  
  Ipoint ll;            // lower left  point of the contour's minimum bounding box (mbr)
  Ipoint ur;            // upper right point of the contour's minimum bounding box
  Ipoint centerPt;      // point in the center of the MBR
  
  int checked;          // can be used to see which contour is checked
  
  void resetAll()
  {
    idx = -1;
    setPt( &ll, -1, -1, -1 );
    setPt( &ur, -1, -1, -1 );
    setPt( &centerPt, -1, -1, -1 );
    z = -1;
    loaded  = false;
    checked = 0;
  }
};

struct ZList
{
  vector<int> idxs;     // a vector of contour indexes
};


struct InterpolationEvent
{
//## DATA:
  
  Iobj  *obj;           // pointer to the OBJECT  we are dealing with
	int sContIdx;         // index of the contour we must interpolate (within *obj)
  int sContZ;           // the slice with *cont is on (the starting z value)
  
  vector<ContInfo> conti;     // is populated with contour info (such as MBR) for each
                              // contour matching it's order in *obj
  
  int minZLimit;
  int maxZLimit;
  
  vector<ZList> ztableKey;    // list of indexes of key contours on each slice
  vector<ZList> ztableInt;    // list of indexes of interpolated contours on each slice 
  
	int closestAboveIdx;  // the NEAREST same-surface contour ABOVE our given contour
	int closestBelowIdx;	// the NEAREST same-surface contour BELOW our given contour
	
	vector<int> aboveIdxs;		// list of NEAREST contours ABOVE given contour with
                            //   same z val - (if > 1 then given contour branches)
	vector<int> belowIdxs;		// list of NEAREST contours BELOW given contour with
                            //   same z val - (if > 1 then given contour branches)
  
  
//## METHODS:
  
  
  //** USER CALLED METHODS:
  
  void performInterpolationOnCont( Iobj* _obj, int _contIdx, int interpolationType );
  int  deleteImmediatelyAdjacentInterpolatedConts( Iobj* _obj, int _contId );
  int  findMiddleNextLargeInterpolatedSpan( Iobj *_obj, int _contIdx, int minHoleSize );
  
  //** CONSTRUCTORS:
  
  void resetAll();
  void regenerateContInfoVector( int _minZLimit=0, int _maxZLimit=MAX_INT, int startIdx=0 );
  void regenerateZTables( );
  void regenerateConnections( );
  
  
  //** SURFACE RESOLVING METHODS:
  
  bool contoursSameSurf( int c1Idx, int c2Idx );
  bool contoursSameSurfSlow( Icont *cont1, Icont *cont2 );
  
  vector<int> findIdxsNearestKeyConts( int bcIdx, bool above, int maxZDist, int minZDist=1  );
  int findIdxClosestKeyContInList( int bcIdx, vector<int> conts );
  
  int findIdxNearestKeyCont( int cbIdx, int maxDist, bool above );
  vector<int> findIdxsAllKeyContsNoBranching( int idxBaseCont, int maxDist );
  int deleteAllSameSurfInterpContsInZRange( int minZ, int maxZ, int cbIdx );
  int deleteAllSameSurfInterpContsEitherSideOfCont( int cbIdx );
  int deleteInterpolatedContsBetweenKeyContsAboveAndBelow();
  
  //** POINT FITTING METHODS:
  
  void findCoorrespondingPts( Icont *cont1, Icont *cont2, int *idxCont1, int *idxCont2 );
  void findCoorrespondingPts_FourPtMBR( Icont *cont1, Icont *cont2, int *idxCont1, int *idxCont2 );
  void findCoorrespondingPts_AllConvexPts( Icont *cont1, Icont *cont2, int *idxCont1, int *idxCont2 );      // NEW
  
  void breakBaseContIntoTwoForBranching( Icont* baseCont, Icont* contBranch1, Icont* contBranch2, Icont *newBranch1, Icont *newBranch2 );
  
  void modifyKeyContsForInterp( int cbIdx, int ctIdx, Icont *contLNew, Icont *contUNew, bool closeContours, bool findCoorrespondingPtsAndMakeClockwise );
  void modifyKeyContsForInterp_LineConservation( Icont *contKeyL, Icont *contKeyU, Icont *contL, Icont *contU, bool closeContours, bool findCoorrespondingPtsAndMakeClockwise );
  void modifyKeyContsForInterp_MinimumAreaCost ( int cbIdx, int ctIdx, Icont *contLNew, Icont *contUNew );
  
  
  
  //** INTERPOLATION METHODS:
  
  void interp_SmoothCrude_BetweenConts( int c0, int c1, int c2, int c3 );
  void interp_SmoothPointwise_BetweenTwoConts( int clIdx, int cuIdx );
  vector<IcontPtr> getLinearInterpConts( int c1Idx, int c2Idx );
  int interp_Linear_BetweenTwoConts( int clIdx, int cuIdx );
  
  int  mergeAllTouchingConts( vector<IcontPtr> conts );
  void addAllInterpolatedConstAndMerge( vector< vector<IcontPtr> > newConts );
  void interp_Linear_BetweenContsMerge( int cbIdx, vector<int> branchContIdx );
  int interp_Linear( int baseContIdx, int maxDist );
  void interp_Spherical_OnSingleCont( int cMiddleIdx );
  void interp_Spherical( int baseContIdx, int maxDist );
  void interp_SmoothCrude( int baseContIdx, int maxDist );
  void interp_SmoothCrudeAll( int baseContIdx, int maxDist );
  void interp_SmoothPointwise( int baseContIdx, int maxDist );
  
  
  //** SIMPLE INLINE ACCESSORS:
  
  inline int numKeyContsAtZ(int z)         {  return (int)ztableKey[z].idxs.size(); }
  inline int numIntContsAtZ(int z)         {  return (int)ztableInt[z].idxs.size(); }
  
  inline int idxKeyContZ(int z, int i)     {  return ztableKey[z].idxs[i]; }
  inline int idxIntContZ(int z, int i)     {  return ztableInt[z].idxs[i]; }
  
	inline bool isKeyContAbove()             { return ( closestAboveIdx != -1 );	}
	inline bool isKeyContBelow()             { return ( closestBelowIdx != -1 );	}
  
  inline Icont*  getC(int idx)             { return ( getCont(obj, idx) ); }
  inline int     getCZ(int idx)            { return ( conti[idx].z ); }
  inline Ipoint* getUR(int idx)            { return ( &conti[idx].ur ); }
  inline Ipoint* getLL(int idx)            { return ( &conti[idx].ll ); }
  inline Ipoint* getCenterPt(int idx)      { return ( &conti[idx].centerPt ); }
  inline bool    getClosed(int idx)        { return ( isContClosed(obj,getC(idx)) ); }
};





//############################################################



//-------------------------------
//## SMALL FUNCTIONS:

Iobj *getCurrObj();
Icont *getCurrCont();
Ipoint *getCurrPt();
bool isCurrObjValid();
bool isCurrContValid();
bool isCurrPtValid();

//-------------------------------
//## CONTOUR EVENT FUNCTIONS:

bool addInterpolatedContToObj( Iobj *obj, Icont *cont, int interpolated=1 );
int removeAllDeleteFlaggedContoursFromObj( Iobj *obj );


//-------------------------------
//## POINT INTERPOLATION FUNCTIONS:

float calcCardinalSplineFractAtZ( int zVal, Ipoint p0, Ipoint p1, Ipoint p2,  Ipoint p3 );
vector<float> calcCardinalSplineFractsEachSlice( Ipoint p0, Ipoint p1, Ipoint p2,  Ipoint p3 );



//-------------------------------
//## TESTING FUNCTIONS:                 // DELETE
				// all these functions following are used to test performance,
				// so should be commented out in any actual IMOD releases.
/*
void test_selector();
void test_makeContSetNumPoints();
void test_makeContSetNumPointsCrude( Icont *cont, int targetPts );
void test_makeContSegmentsEqualDistance();
void test_moveCurrContToObjAndInterpolate( bool specifyObj );
void test_showInterpolationBetweenConts();
void test_showInterpolatedContoursNicely();
void test_showCurrContConcaveRegions();
void test_cutSurfThroughZ();
void test_randomlyOffsetContours();
void test_copyContourToEnd();
void test_outputAnalysisOfConcavePoints();
void test_outputAnalysisOfBranches();
void test_outputAnalysisOfTubes();
void test_testTimesTilingMethodsNPoints();
void test_testTimesTilingMethodsDiffConts();
*/
