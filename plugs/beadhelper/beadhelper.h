#include "dialog_frame.h"
#include <QKeyEvent>
#include <QLabel>
#include <QEvent>
#include <QCloseEvent>
class QPushButton;
class QCheckBox;

class QLabel;
class QSpinner;
class QRadioButton;
class QGridLayout;
class QVBoxLayout;
class QComboBox;
class QSpinBox;
class QGroupBox;
class QSpacerItem;
class QEvent;

#include "imodplugin.h"
#include "dia_qtutils.h"
#include "icontextra.h"

#include <qstring.h>
#include <vector>
using namespace std;


//############################################################
//## CONSTANTS:

const int NO_POINT = -1;

enum estimationmethod { EM_BESTTWO, EM_SMARTTWO, EM_NEARESTTWO, EM_PREVTWO,
                        EM_QUADRATIC, EM_LOCALQUADRATIC, EM_LASTTHREE, EM_LASTSIX };

enum contsortcriteria { SORT_YJUMPS, SORT_DEV, SORT_AVG_GREY, SORT_DIST_FROM_MIDDLE,
												SORT_MISSING_PTS, SORT_UNCHECKED, SORT_RANDOM };

enum contdisplay      { LD_OFF, LD_ALL, LD_OBJ, LD_CURRENT, LD_CURRMISSING,
                        LD_RESULTSMOOTH, LD_SLICE_RESID, LD_BEST_FIT };
enum tiltaxisdisplay  { TD_OFF, TD_TILTAXIS, TD_TILTAXISSEED, TD_TILTAXISPT,
                        TD_TILTSEGS, TD_HGRID };
enum expptdisplay     { ED_CROSS, ED_DIAMOND, ED_ARROW };

enum searchcont       { SC_ALL, SC_UNCHECKED, SC_CHECKED };
enum dkeybehavior     { DK_NONE, DK_OPPOSITEMIDDLE, DK_NEARESTEND, DK_SELECTEDRANGE };
enum mkeybehavior     { MK_NORMAL, MK_GOTOMIDDLE, MK_SMOOTHLOCAL, MK_SMOOTHLOCALY };
enum ukeybehavior     { UK_PRINTINFO, UK_TOGGLEPTCHECKED, UK_TOGGLEALLPTSCHECKED };

enum wheelbehavior    { WH_NONE, WH_POINTS, WH_SLICES, WH_SMART };
enum enterbehavior    { EN_NONE, EN_NEXTUNCHECKED, EN_PREVUNCHECKED, EN_NEXTCHECKED,
                        EN_NEXTCONT };

const char DEGREE_SIGN = 0x00B0;      // degree sign
const int NUM_SAVED_VALS = 44;

//############################################################




//-------------------------------
//## INTERPOLATOR WINDOW:

class BeadHelper : public DialogFrame
{
  Q_OBJECT
  
 public:
  BeadHelper(QWidget *parent, const char *name = NULL);
  ~BeadHelper() {};
  
 public slots:
	
  void initValues();
  void loadSettings();
  void saveSettings();
  void keepOnTop(bool state);
  
  bool drawExtraObject( bool redraw );
  void deletePtsInRange();
  void deletePtsCurrContInRange();
  bool deletePtsUsingDAction();
  void reduceContsToSeed();
  void reduceCurrContToSeed();
  void movePtsToEstimatedPosOptions();
  void movePtsToEstimatedPosRange();
  void movePtsToEstimatedPosCurrCont();
  void moveCurrPtToEstimatedPos();
  void fillMissingPts();
  void fillMissingPtsCurrCont( bool fillPastEnds );
  void moreActions();
  void moreSettings();
  void keyboardSettings();
  void reorderContours();
  void moveContour();
  void moveMultipleContours();
  void splitOrMergeContours();
  void correctCurrentObject();
  void toggleStippled();
  void togglePtsChecked();
  bool enterActionIterateConts( bool reverse );
  void markRangeAsStippled();
  void markRangePtsAsChecked();
  
  bool verifyCurrObjectAndUpdateRanges();
  bool verifyAndUpdateEnteredRangeValues();
  bool advanceSelectedPointInCurrCont( int change );
  int  applyMAction();
  void goToSeedView();
  void printContourCheckedInfo();
  bool verifySeedViewIsSeeded();
  bool verifyTiltIncrement( bool printResult, bool showErrorMsgBoxIfBad );
  bool openTiltAngleFile();
  void test();
  
  void changeShowExpectedPos();
  void changeShowSpheres();
  void changeSphereSize( int value );
  void changeLineDisplayType(int value);
  void changeTiltDisplayType(int value);
  void changeWheelBehav(int value);
  void changeEstPosMethod(int value);
  
  void clearExtraObj();
	void buttonPressed(int);
	
 protected:
	void helpPluginHelp();
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  
 private:
    
  QGroupBox   *grpActions;
  QVBoxLayout *vboxLayout1;
  QPushButton *deletePtsButton;
  QPushButton *reduceContsToSeedButton;
  QPushButton *movePtsToEstButton;
  QPushButton *fillMissingPtsButton;
  QPushButton *reorderContsButton;
  
  QGroupBox   *grpDisplay;
  QGridLayout *gridLayout2;
  QCheckBox   *showEstimatedPosCheckbox;
  QCheckBox   *showSpheresCheckbox;
  QSpinBox    *sphereSizeSpinner;
  QLabel      *lblLineDisplay;
  QComboBox   *lineDisplayCombo;
  QLabel      *lblTiltDisplay;
  QComboBox   *tiltDisplayCombo;
  
  QGroupBox   *grpOptions;
  QGridLayout *gridLayout3;
  QLabel      *lblWheelBehav;
  QLabel      *lblEstMethod;
  QComboBox   *estPosMethodCombo;
  QPushButton *moreActionsButton;
  QPushButton *moreSettingsButton;
  QPushButton *keyboardSettingsButton;
};


//-------------------------------
//## DRAWINGTOOLS DATA STRUCTURE:

struct BeadHelperData   // contains all local plugin data
{
  ImodView   *view;
  BeadHelper *window;
  
  //** MAIN OPTIONS:
  
  int viewMinN;               // minimum view in range
  int viewMaxN;               // maximum view in range
  int contMinN;               // minimum contour in range
  int contMaxN;               // maximum contour in range
  
  int viewMin;                // minimum view index in range
  int viewMax;                // maximum view index in range
  int contMin;                // minimum contour index in range
  int contMax;                // maximum contour index in range
  
  int showExpectedPos;        // if true: will show estimated position (est pos) of point on
                              //   the current each view
  bool showSpheres;           // if true: will set the sphere size to "sphereSize"
  int  sphereSize;            // the sphere size of the object, allowing the user
                              //   to see points on adjacent views
  int  lineDisplayType;       // different modes of displaying the trajectory of
                              //   contours (see: contdisplay)
  int  tiltDisplayType;       // different modes of displaying tilt axis
  int  estPosMethod;          // change the method used to estimate the position of pts
                              //   (see: estimationmethod)
  
  //** MORE SETTINGS:
  
  int seedView;               // the view in the stack where seeds are placed
                              //  (usually the middle view in the stack)
  float tiltIncrement;        // approximate tilt increment - the angle which the
                              //   specimen is tilted between subsequent views
  float tiltAxisAngle;        // angle in degrees of the tilt axis relative to vertical
  float tiltOffsetX;          // distance in pixels the tilt axis is offset in X
                              //   from crossing middlePt (usually set to 0)
  float biggestHoleGrid;      // the approximate distance between grid points used to
                              //   find the next biggest hole (bead_goToNextBiggestHole)
  float biggestHoleInset;     // the distance to represent the very edge of the grid
  
  int expPtDisplayType;       // symbol used for expected points (see: expptdisplay)
  int expPtSize;              // the size (in screen pixels) of expected points
  int sizeLineSpheres;        // the size (in screen pixels) of contour line display
  int lineDisplayWidth;       // the thickness of the line used to show contours
  int sizePurpleSpheres;      // the size (in screen pixels) of purple spheres
  float sizeCheckedPts;       // the sphere size of points which have been checked
  
  int selectedAction;         // the last selected action under "More Actions"
  int sortCriteria;           // the last selected sort critria "Reorder Contours"
  bool autoSaveSettings;      // if true: saves all BeadHelperData settings on exit
  
  //** KEYBOARD AND MOUSE OPTIONS:
  
  int  wheelBehav;            // changes the behaviour of the mouse wheel
                              //   (see: wheelbehavior)
  float wheelResistance;      // the higher the value, the slower mouse scrolling works
  bool centerPtOnScroll;      // keeps selected point/estimated point centered on scroll
  
  bool disableHotKeys;        // disables all hotkeys for this plugin window
  bool includeEndsResid;      // include end points and seed when searching for 
                              // residuals with [y], [b] and [w]
  
  bool searchRangeOnly;       // if true: [y] and [b] only search viewMin to viewMax
  int contsToSearch;          // wether [y], [b] and [o] search all contours, checked
                              //   contours or unchecked contours (see: searchcont)
  int dKeyBehav;              // the action when "d" is pressed (see: dkeybehavior)
  int mKeyBehav;              // the action when "m" is pressed (see: mkeybehavior)
  bool wCurrContOnly;         // if true: pressiong "w" searches current contour only
  int  wWeightedDiv;          // weighted_dev = distance_to_expected_pt / 
                              //                (distance_nearest_pts + wWeightedDiv)
  int uKeyBehav;							// the action when "U" is pressed (see: ukeybehavior)
  
  int enterAction;            // the action performed when enter is pressed
  int minPtsEnter;            // the minimum number of points a contour must have to be
                              //   jumped to when enter is pressed
  int maxPtsEnter;            // the maximum number of points a contour must have to be
                              //   jumped to when enter is pressed
  bool enterPrint;            // if true: prints the number of matching contour each time
                              //   enter is pressed
  
  //** SMOOTHING OPTIONS:
  
  int   smoothCurrContOnly;   // if 0: smooths range, if 1: smooths current contour only
  bool  smoothFillGaps;       // fill in missing points (on views with no points)
  bool  smoothBigResidFirst;  // smooths points with greatest residual first
  bool  smoothMoveYOnly;      // only shifts point along y axis
  bool  smoothLeaveSeed;      // will not move the seed point during moving process
  bool  smoothLeaveEnds;      // will not move the start and end point of contours
  bool  smoothLeaveCurrV;     // will not move any point on the current view
  float smoothMoveFract;      // move points by this fraction towards their exp pos
  float smoothMinResid;       // only move points more that this distance from exp pos
  int   smoothIterations;     // how many iterations to run smoothing
  bool  smoothAdjacentV;      // will only smooth views above and below the current view
  int   smoothNumViews;       // how many views above and below the current z to smooth
  
  
  //** OTHER:
  
  bool initialized;           // is set to true after values have been set
  int xsize, ysize, zsize;    // size of the image / tomogram
  
  Ipoint middlePt;            // the point at the middle of the tomogram
  Ipoint mouse;               // the current tomogram coordinates of the mouse
  
  vector<IdxToSort> sortVals; // stores a idx and float for each contour after
                              // "reorder contours" is run
  vector<float> tiltAngles;   // stores tilt angles loaded from a .tlt file
  
  int extraObjExpPos;         //|
  int extraObjContDisp;       //|-- stores reference to extra objects
  int extraObjTiltAxis;       //|
  int extraObjExtra;          //|
  
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
//## EDITING FUNCTIONS:


int edit_getZOfTopZap();
int edit_setZapLocation( float x, int y, int z, bool redraw );
int edit_changeSelectedView( int changeZ, bool redraw );
int edit_addContourToObj( Iobj *obj, Icont *cont, bool enableUndo );
int edit_removeAllDeleteFlaggedContoursFromObj( Iobj *obj, bool enableUndo );

bool bead_focusOnPointCrude( float x, float y, float z );
float bead_getTiltAngleAtZ( int z );

bool bead_isPtChecked( Iobj *obj, Icont *cont, int ptIdx );
bool bead_areDuplicatePtsSameView( Icont *cont );
int bead_removeDuplicatePtsSameView( Icont *cont, bool remove, bool print );

bool bead_ptsAreNotInOrder( Icont *cont );
int bead_orderPtsByAscendingZ( Icont *cont );

bool bead_isPtOnView( Icont *cont, int view );
int bead_getPtIdxOnView( Icont *cont, int view );
int bead_getExpPtIdxForView( Icont *cont, int view );
Ipoint *bead_getPtOnView( Icont *cont, int view );
int bead_getClosestPtIdxToView( Icont *cont, int view );
Ipoint *bead_getClosestPtToView( Icont *cont, int view );

bool bead_getClosestTwoPointsToView( Icont *cont, int view, Ipoint *pt1, Ipoint *pt2 );
bool bead_getPointsEitherSideView( Icont *cont, int view, Ipoint *pt1, Ipoint *pt2 );
bool bead_getSpacedOutPoints( Icont *cont, int view,
                              Icont *ptsByDist, int minZBetweenPts );

Ipoint bead_getPtOnLineWithZ( Ipoint *pt1, Ipoint *pt2, int z );
bool bead_getExpectedPosOfPoint( Icont *cont, int view, Ipoint *pt );
bool bead_getExpectedPosOfPointUsingPtsBefore( Icont *cont, int view, Ipoint *pt,
                                               int numPtsToAvg );

int bead_insertOrOverwritePoint( Icont *cont, Ipoint *pt );
bool bead_insertPtAtEstimatedPos( Icont *cont, int view, bool overwrite );

int bead_movePtTowardsEstimatedPos ( Icont *cont, int z,
                                     float moveFract, float minResid, bool moveYOnly,
                                     bool leaveEnds );
bool bead_movePtsTowardsEstimatedPos ( Icont *cont, int minZ, int maxZ,
                                       float moveFract, float minResid, int iterations,
                                       bool fillGaps, bool moveBigFirst, bool moveYOnly,
                                       bool leaveSeed, bool leaveEnds, bool leaveCurrV,
                                       int &ptsMoved, int &ptsAdded );
bool bead_smoothPtsUsingPlugSettings ( Icont *cont, int &ptsMoved, int &ptsAdded );

int bead_fillMissingPtsOnCont( Icont *cont, int minZ, int maxZ, bool fillPastEnds );
int bead_deletePtsInZRange( Iobj *obj, Icont *cont, int minZ, int maxZ,
                            bool skipCheckedConts, bool skipCheckedPts, bool skipSeedView,
                            bool inclusive );

float bead_calcYJump( Icont *cont, int idx );
float bead_calcAvgYJump( Icont *cont );
float bead_calcDistanceFromExpected( Icont *cont, int idx, bool weighted );
float bead_calcCrudeWeightedDevFromMiddle( Icont *cont );
float bead_getGreyValue( Ipoint *pt );
float bead_avgGreyValueOfPts( Icont *cont );
float bead_distFromMiddle( Icont *cont );

void bead_reorderConts( int sortCriteria, int minCont, int maxCont, 
                        bool calcValsOnly, bool reverse, bool printVals );


bool bead_calcLineOfBestFit( Icont *cont, float *gradient, float *offset, int minPts );
bool bead_calcQuadraticCurve( float x1, float x2, float x3,
                              float y1, float y2, float y3,
                              float *a, float *b, float *c );

bool bead_estimateTurningPointOfCont( Icont *cont, Ipoint *pt,
                                      float minDistRequired, int *idx );

bool bead_goToNextBiggestYJump( bool findNextBiggest );
bool bead_goToNextBiggestDev( bool findNextBiggest );
bool bead_goToNextBiggestWeightedDev( bool findNextBiggest );
bool bead_goToNextBiggestHole( bool findNextBiggest );
bool bead_goToContNextBiggestSortVal( bool findNextBiggest );

float bead_estimateTiltAngle();
bool bead_showBottomContoursInPurple( int minZ, int maxZ );
bool bead_showContourTurningPts();
void bead_showGrid();

//############################################################


