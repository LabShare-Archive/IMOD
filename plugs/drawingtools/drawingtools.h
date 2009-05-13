#include "dialog_frame.h"
//Added by qt3to4:
#include <QKeyEvent>
#include <QLabel>
#include <QEvent>
#include <QCloseEvent>
class QPushButton;
class QCheckBox;

class QLabel;
class QSpinner;
class QButtonGroup;
class QRadioButton;
class QGridLayout;
class QComboBox;
class QSpinBox;
class QGroupBox;
class QVBoxLayout;
class QSpacerItem;
class QEvent;
class QDoubleSpinBox;

#include "imodplugin.h"
#include "dia_qtutils.h"
#include "icontextra.h"

#include <qstring.h>
#include <vector>
using namespace std;

//############################################################


//-------------------------------
//## INTERPOLATOR WINDOW:

class DrawingTools : public DialogFrame
{
  Q_OBJECT

 public:
  DrawingTools(QWidget *parent, const char *name = NULL);
  ~DrawingTools() {};
  
public slots:
  void buttonPressed(int);
  void initValues();
  void loadSettings();
  void saveSettings();

  bool drawExtraObject( bool redraw );
  void reduceCurrentContour();
  void smoothCurrentContour( bool moveExistingPts );
  void reduceConts();
  void smoothConts();
  bool executeDAction();
  void selectNextOverlappingContour();
  void printModelPointInfo();
  void printObjectDetailedInfo();
  void printContourDetailedInfo();
  void moreActions();
  void moreSettings();
  void keyboardSettings();
  void sortContours();
  void findContours();
  void deleteRangeContours();
  void cropRangeContours();
  void copyOrMoveContourRange();
  void tranformContourRange();
  void movePoint();
  void expandContourRange();
  void cleanModelAndFixContours();
  void test();
  void cut();
  void copy();
  void paste(bool centerOnMouse);
  int  copyCurrContToView(bool smartSize);
  
  void changeType( int value );
  void changeTypeSelected( int newType );
  void changeSculptCircleRadius( float value, bool accel=false );
  void clearExtraObj();
  
 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  
 private:
  
  QButtonGroup *typeButtonGroup;
  QRadioButton *typeRadio_Normal;
  QRadioButton *typeRadio_Sculpt;
  QRadioButton *typeRadio_Join;
  QRadioButton *typeRadio_Transform;
  QRadioButton *typeRadio_Eraser;
  
  QGroupBox    *grpActions;
  QVBoxLayout  *vboxLayout1;
  QPushButton  *reduceContsButton;
  QPushButton  *smoothContsButton;
  
  QWidget      *widget1;
  QGridLayout  *gridLayout2;
  QPushButton  *keyboardSettingsButton;
  QPushButton  *moreActionsButton;
  QPushButton  *moreSettingsButton;
};

//-------------------------------
//## CONSTANTS:

enum drawmodes      { DM_NORMAL, DM_SCULPT, DM_JOIN, DM_TRANSFORM, DM_ERASER,
                      DM_WARP, DM_CIRCLE };
enum smoothmodes    { RD_TOL, RD_MINAREA };
enum wheelbehaviour { WH_NONE, WH_SCULPTCIRCLE, WH_SLICES, WH_CONTS, WH_PTS, WH_PTSIZE };
enum dkeybehavior   { DK_NONE, DK_TOEND, DK_NEARESTEND, DK_DELETEPT, DK_DELETECONT,
                      DK_REMOVEPTSIZE, DK_REMOVEALLPTSIZES, DK_MOVEPT };
enum ekeybehavior   { EK_ADDONLY, EK_MOVEPTS, EK_REDUCEANDMOVE };
enum warpbehavior   { WB_AUTO, WB_LINE, WB_AREA };

enum sculptresize   { SR_STAGGERED, SR_LINEAR, SR_LOG };

enum sortcriteria   { SORT_SURFACENUM,
                      SORT_NUMPTS, SORT_LENGTH, SORT_AREA, SORT_CLOCKWISEAREA,
                      SORT_AVGSEGLEN, SORT_MAXSEGLEN,
                      SORT_AVGPTSIZE, SORT_AVGGRAY, SORT_STIPPLED, SORT_RANDOM, 
                      SORT_AVGX, SORT_AVGY, SORT_AVGZ,
                      SORT_MINX, SORT_MINY, SORT_MINZ,
                      SORT_PTX, SORT_PTY, SORT_PTZ,
                      SORT_PTSIZE, SORT_PTGREY, SORT_NUMOPTIONS };

const int NUM_SAVED_VALS = 29;

//-------------------------------
//## DRAWINGTOOLS DATA STRUCTURE:

struct DrawingToolsData   // contains all local plugin data
{
  ImodView    *view;
  DrawingTools *window;
  
  //## DRAWING OPTIONS:
  
  int drawMode;        // the drawing tool type currently selected (see enum "drawmodes")
  
  bool   reducePts;             // if 1: drawn conts will automatically be reduced
  int    reducePtsOpt;          // if 0: use tolerance, if 1: use min area
  float  reducePtsTol;          // the tolerance setting used in "imodContourReduce"
  float  reducePtsMinArea;      // the minimum area which must be formed by three
                                //  consecutive pts in a contour else the middle
                                //  one be removed - see the 
                                //  "cont_reducePtsMinArea" function
  
  float  smoothMinDist;         // min distance between consecutive points
  float  smoothTensileFract;    // tensile fraction used by the catumull-rom spline
                                //  algorithm in the "cont_addPtsSmooth" function
                                //  NOTE: 0=straight, 1.5=smooth, 2.0>=very bendy
  bool   smoothReduceFirst;     // reduces contour before smoothing when [e] pressed
  int    smoothMoveIts;         // number of iterations points are moved/averaged
  float  smoothMoveFract;       // the fraction of the distance a point is moved
                                //  towards the 'avg pos' of the next and prev point
  float  smoothMoveMinDist;     // the min distance from 'avg pos' a point must be
                                //  if it is to be moved
  
  bool   printSmoothResults;    // prints output each time [e] or [r] is pressed
  
  float  sculptRadius;          //  the radius, in pixels, of the sculpting circle
  float  warpRadius;            //  the radius, in pixels, of the warp circle
  bool   diffWarpSize;          // if false: warpRadius always = sculptRadius
  int    sculptResizeScheme;    // changes way in which sculpt circle is resized
                                //   (see: sculptresize)
  int    warpBehavior;          // changes the behavior of the warp tool
                                //   (see: warpbehavior) 
  bool   scupltBut3Warp;        // if true: in sculpt mode the the third mouse button
                                //   applies warp
  
  int    lineDisplayWidth;      // the thickness of the line used to show contours
  
  //## SETTINGS:
  
  int  wheelBehav;              // changes the behaviour of the mouse wheel
                                //   (see: wheelbehaviour)
  int  dKeyBehav;               // the action when [d] is pressed 
                                //   (see: dkeybehavior)
  int  eKeyBehav;               // the action when [e] is pressed 
                                //   (see: ekeybehavior)       
  int  pgUpDownInc;             // the number of slices to iterate when
                                //   PageUp or PageDown is pressed
  
  bool   useNumKeys;            // intercepts number keys [1]-[5] to change draw mode
  bool   smartPtResizeMode;     // if true: allows "smart point resize mode".
  bool   markTouchedContsAsKey; // if true: any contour modified with sculpt changes
                                //  to unstippled
  int    wheelResistance;       // the higher the value, the slower mouse scrolling works
  bool   showMouseInModelView;  // shows the extra object in the model view
  
  bool   testIntersetAllObjs;   // if true: [a] will test contours for intersection
                                //   with all other contours (not just in the same object)
  bool   testOverlapping;       // if true: [a] will search for contours nested inside
                                //   other contours in the same object
  
  int    selectedAction;        // the last selected action under "More Actions"
  int    sortCriteria;          // the lat sort criteria selected via:
                                // "More Actions >> sort ... " (see: sortcriteria)
  int    findCriteria;          // the lat find criteria selected via:
                                // "More Actions >> sort ... " (see: sortcriteria)
  
  //int    numSavedAction;        // is set to NUM_SAVED_VALS and helps ensure correct
  //                              //  number of values are saved/loaded
  
  //## MOUSE:
  
  Ipoint mouse;         // the current tomogram coordinates of the mouse
  Ipoint mousePrev;     // the mosue coordinates before it's last move
  Ipoint mouseDownPt;   // the mouse coordinates when the mouse button was last pressed
  float changeX;        // the distance (in tomogram pixels) of the last mouse move in X
  float changeY;        // the distance (in tomogram pixels) of the last mouse move in Y
  
  int but1Down;    // used for selecting points & panning (in NORMAL mode)
  int but2Down;    // used for drawing new contours       (in NORMAL mode)
  int but3Down;    // used for modifying points           (in NORMAL mode)
  
  bool but1Pressed;     //|- if true: button was just pressed
  bool but2Pressed;     //|
  bool but3Pressed;     //|
  
  bool but1Released;    //|- if true: button was just released
  bool but2Released;    //|
  bool but3Released;    //|
  
  //## OTHER:
  
  bool shiftDown;       // set to true when the SHIFT button is down
  Ipoint centerPt;      // the center of the currently selected contour
  bool contortInProgress;   // used in DM_WARP when user clicks close to contour edge
  
  vector<IdxToSort> sortVals; // stores a idx and float for each contour after
                              // "sort contours" is run
  vector<IdxToSort> sortPtVals; // stores a idx and float for each contour after
                                // "sort contours" is run
  int sortCriteriaOfVals;     // last sort criteria (see: sortcriteria) used to populate
                              // values in "sortVals" and "sortPtVals"
  
  Icont *copiedCont;      // used to cut/copy and paste contours
  Ipoint copiedCenterPt;  // the center of area of the last contour cut/copied
  
  bool initialized;           // is set to true after values have been set
  int xsize, ysize, zsize;    // size of the image / tomogram
  int extraObjNum;            // stores a reference to the extra object
};




//############################################################



//-------------------------------
//## SMALL FUNCTIONS:

Iobj *getCurrObj();
Icont *getCurrCont();
Ipoint *getCurrPt();
bool isCurrObjValidAndShown();
bool isCurrContValid();
bool isCurrPtValid();

int removeAllDeleteFlaggedContoursFromObj( Iobj *obj, int objIdx );


//-------------------------------
//## EDITING FUNCTIONS:

int edit_getZOfTopZap();
int edit_setZapLocation( float x, int y, int z, bool redraw );
int edit_changeSelectedSlice( int changeZ, bool redraw, bool snapToEnds=true );

int edit_addContourToObj( Iobj *obj, Icont *cont, bool enableUndo );
int edit_removeAllFlaggedContoursFromObj( Iobj *obj );
//bool edit_selectContourPtNearCoordsCurrObj( float x, float y, int z, float distTolerance);
bool edit_selectNearPtInCurrObj(Ipoint *centerPt, float distTol, float distTolCurrCont, bool countInsideCurrCont);
bool edit_selectVisiblePtNearCoords( Ipoint *mouse, float distScreenPix);

bool edit_copiedContIfDiffSlice( bool selectNewCont );

void edit_executeSculptStart();
void edit_executeSculpt();
void edit_executeSculptPush( Ipoint center, float radius );
void edit_executeSculptPinch( Ipoint center, float radius );
void edit_executeSculptEnd();
void edit_executeJoinEnd();

void edit_executeJoinLineStart();
void edit_executeJoinEnd();
void edit_executeJoinRectEnd();

void edit_executeWarpStart();
void edit_executeWarp();
void edit_executeWarpEnd();

void edit_inversePointsInContour(bool reorder);
int  edit_reduceCurrContour();
int  edit_smoothCurrContour();

bool edit_doesBBoxTouchCircle( Ipoint *ll, Ipoint *ur, Ipoint *center, float radius );
int edit_eraseContsInCircle( Ipoint center, float radius);
int edit_erasePointsInCircle( Ipoint center, float radius );
bool edit_breakPointsInCircle( Ipoint center, float radius );

void edit_breakCurrContIntoSimpleContsAndDeleteSmallest();
void edit_makeCurrContSimple();       
void edit_deleteCurrContIfTooSmall();
void edit_joinCurrContWithAnyTouching();

float edit_selectClosestPtInCurrCont( Ipoint *givenPt );
int   edit_addPtsCurrCont( float maxDistBetweenPts );
int   edit_addPtsCurrContInRadius( Ipoint *centerPt, float radius, float maxDistPts );

bool edit_selectNextIntersectingCont();
bool edit_findIntersectingCont( int objIdx, int contIdx,
                               int *pCross, int *objCross, int *contCross, bool *samePts,
                               int minObj, int maxObj, bool skipPrevContsInObj );
int edit_countOverlappingContsCrude( int objIdx, int contIdx );

string edit_getSortValString(int sortCriteria);

float edit_getGreyValue( Ipoint *pt );
float edit_avgGreyValueOfPts( Icont *cont );
float edit_getAvgPtSize( Iobj *obj, Icont *cont );
bool edit_goToContNextBiggestFindVal( bool findNextSmallest, bool recalc,
                                      bool useCurrentValue, float chosenTarget=FLOAT_MIN );
float edit_getSortValue( int sortCriteria, Iobj *obj, Icont *cont, int ptIdx=0 );
void edit_reorderConts( int sortCriteria, int minCont, int maxCont,        
                     bool calcValsOnly, bool reverse, bool printVals );

//############################################################
