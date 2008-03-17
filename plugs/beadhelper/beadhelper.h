#include "dialog_frame.h"
class QPushButton;
class QCheckBox;

class QLabel;
class QSpinner;
class QVButtonGroup;
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

#include <qstring.h>
#include <vector>
using namespace std;


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
  void buttonPressed(int);
  void loadSettings();
  void saveSettings();
  
  bool drawExtraObject( bool redraw );
  void deletePtsInRange();
  void deletePtsCurrContInRange();
  void deletePtsCurrContToNearestEnd( bool inclusive );
  void reduceContsToSeed();
  void reduceCurrContToSeed();
  void movePtsToExstimatedPos();
  void movePtToExstimatedPosCurrCont();
  void fillMissingPts();
  void fillMissingPtsCurrCont();
  void moreActions();
  void moreSettings();
  void reorderContours();
  void moveContour();
  void moveMultipleContours();
  void correctCurrentObject();
  void toggleStippled();
  bool iterateStippled(int change, bool stippled);
  void markRangeAsStippled();
  
  bool updateAndVerifyRanges();
  bool advanceSelectedPointInCurrCont( int change );
  void test();
    
  void changeShowExpectedPos();
  void changeShowSpheres();
  void changeSphereSize( int value );
  void changeLineDisplayType(int value);
  void changeTiltDisplayType(int value);
  void changeWheelBehav(int value);
  void changeEstPosMethod(int value);
  
  void clearExtraObj();
    
 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  
 private:
  
  QGroupBox   *grpRange;
  QGridLayout *gridLayout1;
  QLabel      *lblSlices;
  QSpinBox    *sliceMinSpinner;
  QLabel      *lblSlicesTo;
  QSpinBox    *sliceMaxSpinner;
  QLabel      *lblContours;
  QSpinBox    *contMinSpinner;
  QLabel      *lblContoursTo;
  QSpinBox    *contMaxSpinner;
  
  QGroupBox   *grpActions;
  QVBoxLayout *vboxLayout1;
  QPushButton *deletePtsButton;
  QPushButton *reduceContsToSeedButton;
  QPushButton *movePtsToEstButton;
  QPushButton *fillMissingPtsButton;
  QPushButton *reorderContsButton;
  
  QGroupBox   *grpDisplay;
  QGridLayout *gridLayout2;
  QCheckBox   *showExpectedPosCheckbox;
  QCheckBox   *showSpheresCheckbox;
  QSpinBox    *sphereSizeSpinner;
  QLabel      *lblLineDisplay;
  QComboBox   *lineDisplayCombo;
  QLabel      *lblTiltDisplay;
  QComboBox   *tiltDisplayCombo;
  
  QGroupBox   *grpOptions;
  QGridLayout *gridLayout3;
  QLabel      *lblWheelBehav;
  QComboBox   *wheelBehavCombo;
  QLabel      *lblEstMethod;
  QComboBox   *estPosMethodCombo;
  QPushButton *moreActionsButton;
  QPushButton *moreSettingsButton;
};


//-------------------------------
//## CONSTANTS:

const int NO_POINT = -1;

enum contsortcriteria  { SORT_YJUMPS, SORT_DEV, SORT_AVG_GREY, SORT_DIST_FROM_MIDDLE,
                          SORT_MISSING_PTS, SORT_RANDOM };

enum contdisplay       { LD_OFF, LD_CURRENT, LD_CURRMISSING,
                          LD_ALL, LD_SLICE_RESID, LD_BEST_FIT, LD_TILTAXIS };
enum tiltaxisdisplay   { TD_OFF, TD_TILTAXIS, TD_TILTAXISSEED, TD_TILTAXISPT };

enum expptdisplay      { ED_CROSS, ED_DIAMOND, ED_ARROW };

enum wheelbehaviour   { WH_NONE, WH_POINTS, WH_SLICES, WH_SMART };
enum estimationmethod { EM_BESTTWO, EM_NEARESTTWO,
                        EM_QUADRATIC, EM_LOCALQUADRATIC, EM_LASTTHREE, EM_LASTSIX };

const int NUM_SAVED_VALS = 17;


//-------------------------------
//## DRAWINGTOOLS DATA STRUCTURE:

struct BeadHelperData   // contains all local plugin data
{
  ImodView    *view;
  BeadHelper *window;
  
  //## DRAWING OPTIONS:
    
  int sliceMin;               // minimum slice in range
  int sliceMax;               // maximum slice in range
  int contMin;                // minimum contour in range
  int contMax;                // maximum contour in range
  
  int showExpectedPos;       // if true: will show estimated position of point on
                              //   the current each slice
  bool showSpheres;           // if true: will set the sphere size to "sphereSize"
  int  sphereSize;            // the sphere size of the object, allowing the user
                              //   to see points on adjacent slices
  int  lineDisplayType;       // different modes of displaying the trajectory of
                              //   contours (see: contdisplay)
  int  tiltDisplayType;       // different modes of displaying tilt axis
  
  int  wheelBehav;            // changes the behaviour of the mouse wheel
                              //   (see: wheelbehaviour)
  int  estPosMethod;          // change the method used to estimate the position of pts
                              //   (see: estimationmethod)
  
  int expPtDisplayType;       // symbol used for expected points (see: expptdisplay)
  int expPtSize;              // the size (in screen pixels) of expected points
  int sizePurpleSpheres;      // the size (in screen pixels) of purple spheres
  
  int selectedAction;         // the last selected action under "More Actions"
  int sortCriteria;           // the last selected sort critria "Reorder Contours"
  
  bool autoSaveSettings;
  
  //## OTHER:
  
  Ipoint middlePt;      // the point at the middle of the tomogram
  
  float tiltAngle;            // angle in degrees of the tilt axis relative to vertical
  float tiltOffsetX;          // distance in pixels the tilt axis is offset in X
                              //  from crossing middlePt (usually set to 0)
  float biggestHoleGrid;      // the approximate distance between grid points used to
                              //  find the next biggest hole (bead_goToNextBiggestHole)
  float biggestHoleOffset;    // the distance to represent the very edge of the grid
  
  
  vector<IdxToSort> sortVals;   // stores a idx and float for each contour after
                                // "reorder contours" is run
  
  Ipoint mouse;               // the current tomogram coordinates of the mouse
  
  float wheelResistance;      // the higher the value, the slower mouse scrolling works
  
  bool initialized;           // is set to true after values have been set
  int xsize, ysize, zsize;    // size of the image / tomogram
  int middleSlice;            // the middle slice of the tomogram (where we expect seed)
  
  int extraObjExpPos;         //|
  int extraObjContDisp;       //|-- stores reference to extra objects
  int extraObjTiltAxis;       //|
  int extraObjExtra;          //|
  
  int redrawControlNum;
};




//############################################################




//-------------------------------
//## GUI FUNCTIONS:

string qStringToString( QString qstr );
void MsgBox( string str );
bool MsgBoxYesNo( QWidget *parent, string str );
string InputBoxString( QWidget *parent, string title, string label, string defaultStr );

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
int edit_changeSelectedSlice( int changeZ, bool redraw );
bool bead_focusOnPointCrude( float x, float y, float z );

bool bead_areDuplicatePtsSameView( Icont *cont );
int bead_removeDuplicatePtsSameView( Icont *cont, bool remove, bool print );

bool bead_isPtOnSlice( Icont *cont, int slice );
int bead_getPtIdxOnSlice( Icont *cont, int slice );
int bead_getExpPtIdxForSlice( Icont *cont, int slice );
Ipoint *bead_getPtOnSlice( Icont *cont, int slice );
int bead_getClosestPtIdxToSlice( Icont *cont, int slice );
Ipoint *bead_getClosestPtToSlice( Icont *cont, int slice );

bool bead_getClosestTwoPointsToSlice( Icont *cont, int slice, Ipoint *pt1, Ipoint *pt2 );
bool bead_getPointsEitherSideSlice( Icont *cont, int slice, Ipoint *pt1, Ipoint *pt2 );
bool bead_getSpacedOutPoints( Icont *cont, int slice,
                              Icont *ptsByDist, int minZBetweenPts );

Ipoint bead_getPtOnLineWithZ( Ipoint *pt1, Ipoint *pt2, int z );
bool bead_getExpectedPosOfPoint( Icont *cont, int slice, Ipoint *pt );
bool bead_getExpectedPosOfPointUsingPtsBefore( Icont *cont, int slice, Ipoint *pt,
                                               int numPtsToAvg );

int bead_insertOrOverwritePoint( Icont *cont, Ipoint *pt );
bool bead_insertPtAtEstimatedPos( Icont *cont, int slice, bool overwrite );

bool bead_movePtsTowardsEstimatedPos ( Icont *cont, int minZ, int maxZ,
                                       float moveFract, float minResid, int iterations,
                                       bool fillGaps, bool moveYOnly, bool leaveSeed,
                                       int &ptsMoved, int &ptsAdded );
int bead_fillMissingPtsOnCont( Icont *cont, int minZ, int maxZ );

float bead_calcYJump( Icont *cont, int idx );
float bead_calcAvgYJump( Icont *cont );
float bead_calcDistanceFromExpected( Icont *cont, int idx );
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
bool bead_goToNextBiggestWeightedDev( bool findNextBiggest, bool weighted );
bool bead_goToNextBiggestHole( bool findNextBiggest );
bool bead_goToContNextBiggestSortVal( bool findNextBiggest );

float bead_estimateTiltAngle();
bool bead_showBottomContoursInPurple( int minZ, int maxZ );
bool bead_showContourTurningPts();

//############################################################


