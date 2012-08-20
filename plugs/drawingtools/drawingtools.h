#include "dialog_frame.h"
#include <QKeyEvent>
#include <QEvent>
#include <QCloseEvent>
#include <QTime>
class QPushButton;
class QCheckBox;

class QTime;
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
class QColorDialog;

#include "imodplugin.h"
#include "dia_qtutils.h"
#include "icontextra.h"
#include "customdialog.h"
#include "livewire/LivewireCalculator.h"		// jeff's livewire code
#include "livewire/WeightCalculator.h"			// jeff's livewire code for weighting images

#include <qstring.h>
#include <vector>
using namespace std;

//############################################################
//## CONSTANTS:

enum drawmodes      { DM_NORMAL, DM_WARP, DM_SCULPT, DM_JOIN, DM_LIVEWIRE, DM_WAND, 
	                    DM_ERASER, DM_MEASURE, DM_TRANSFORM, DM_CURVE, DM_CIRCLE,
	                    DM_CORRECT };
enum smoothmodes    { RD_TOL, RD_MINAREA };
enum wheelbehaviour { WH_NONE, WH_SCULPTCIRCLE, WH_SLICES, WH_CONTS, WH_PTS, WH_PTSIZE };
enum dkeybehavior   { DK_NONE, DK_TOEND, DK_NEARESTEND, DK_DELETEPT, DK_DELETECONT,
	                    DK_REMOVEPTSIZE, DK_REMOVEALLPTSIZES, DK_MOVEPT };
enum ekeybehavior   { EK_ADDONLY, EK_MOVEPTS, EK_REDUCEANDMOVE };
enum warpbehavior   { WB_AUTO, WB_LINE, WB_AREA };

enum sculptresize   { SR_STAGGERED, SR_LINEAR, SR_LOG, SR_FAST };

enum sortcriteria   { SORT_SURFACENUM,
	                    SORT_NUMPTS, SORT_LENGTH, SORT_AREA, SORT_CLOCKWISEAREA,
	                    SORT_AVGSEGLEN, SORT_MAXSEGLEN,
	                    SORT_AVGPTSIZE, SORT_AVGGRAY, SORT_STIPPLED, SORT_RANDOM, 
	                    SORT_AVGX, SORT_AVGY, SORT_AVGZ,
	                    SORT_MINX, SORT_MINY, SORT_MINZ,
	                    SORT_PTX, SORT_PTY, SORT_PTZ,
	                    SORT_PTSIZE, SORT_PTGREY, SORT_NUMOPTIONS };

enum zhints         { ZH_NONE, ZH_ABOVE, ZH_BELOW, ZH_BOX };

enum livewireopt    { LW_DARK_MEMBRANE, LW_LIGHT_MEMBRANE,
	                    LW_OTHER };		// not yet implemented

enum pixneigh { PX_E, PX_NE, PX_N, PX_NW, PX_W, PX_SW, PX_S, PX_SE, PX_EIGHT };


const float LW_SNAP_DIST  = 10.0f;
const int NUM_TOOLS       = 12;
const int NUM_TOOLS_SHOWN = 9;
const int NUM_SAVED_VALS  = 56;
const int PIX_OFF         = -1;

//############################################################


//-------------------------------
//## DRAWINGTOOLS WINDOW:

class DrawingTools : public DialogFrame
{
  Q_OBJECT
	
public:
  DrawingTools(QWidget *parent, const char *name = NULL);
  ~DrawingTools() {};
  
	public slots:
	void changeRadioOptions();
  void initValues();
  void loadSettings();
  void saveSettings();
	
  bool drawExtraObject( bool redraw );
	bool drawExtraObjectLivewire( bool redraw );
	bool drawExtraObjectWand( bool redraw );
	
  void keepOnTop( bool state );
  void reduceCurrentContour();
  void smoothCurrentContour( bool moveExistingPts );
  void reduceConts();
  void smoothConts();
  bool executeDAction();
  void selectNextOverlappingContour();
	void analyzeTubes();
  void printModelPointInfo();
  void printObjectDetailedInfo();
  void printContourDetailedInfo();
	
  void moreActions();
  void moreSettings();
	void showLivewireOptions();
	void setupLivewireOptions();
	void showWandOptions();
  void keyboardSettings();
  void sortContours();
  void findContours();
  void modifyRangeContours();
  void deleteRangeContours();
  void cropRangeContours();
  void copyOrMoveContourRange();
  void tranformContourRange();
  void movePoint();
  void expandContourRange();
	void roundPoints();
  void cleanModelAndFixContours();
  void checkForNamelessObjects( bool forceMessageBox );
  int  promptRenameObject( int objIdx );
  void test();
  void cut();
  void copy();
  void paste(bool centerOnMouse);
  int  copyCurrContToView(bool allContsOnSlice);
  
	void initLivewire( int w, int h );
	void livewireFinished();
	
	void customizeToolOrder();
  void changeMode( int modeIdx );
  void changeModeSelected( int modeIdx );
	bool changeRadioToMatchMode( int desiredDrawMode );
  void changeSculptCircleRadius( float value, bool slowDown=false );
  void clearExtraObj();
  void buttonPressed(int);
	
protected:
	void helpPluginHelp();
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  
private:
  
  QButtonGroup *typeButtonGroup;
	
	QWidget      *widType [NUM_TOOLS_SHOWN];
	QHBoxLayout  *layType [NUM_TOOLS_SHOWN];
	QRadioButton *radType [NUM_TOOLS_SHOWN];
	QPushButton  *btnType [NUM_TOOLS_SHOWN];
	
	QString typeLabel   [NUM_TOOLS];
	QString typeTooltip [NUM_TOOLS];
	
	
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
  bool   transformBut3Unif;     // if true: in transform mode the the third mouse button
	                              //   applies scale uniformly in X and Y
  
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
  
  bool  useNumKeys;            // intercepts number keys [1]-[5] to change draw mode
  bool  smartPtResizeMode;     // if true: allows "smart point resize mode".
  bool  markTouchedContsAsKey; // if true: any contour modified with sculpt changes
	                             //  to unstippled
  int   wheelResistance;       // the higher the value, the slower mouse scrolling works
  bool  showMouseInModelView;  // shows the extra object in the model view
  
	bool  testIntersetAllObjs;   // if true: [a] will test contours for intersection
	                             //   with all other contours (not just in the same object)
  bool  testOverlapping;       // if true: [a] will search for contours nested inside
	                             //   other contours in the same object
  
	int   selectedAction;        // the last selected action under "More Actions"
  int   sortCriteria;          // the lat sort criteria selected via:
	                             //  "More Actions >> sort ... " (see: sortcriteria)
  int   findCriteria;          // the lat find criteria selected via:
	                             //  "More Actions >> sort ... " (see: sortcriteria)
  
  int   minObjsNameWarning;    // if there are more than this many objects when plugin
	                             //  loads, a warning box may be generated if any of those
	                             //  objects have no names
  int   drawZhint;             // if greater than 1: draws some type of hint
	                             //  to show user what slice he is on
  bool  useArrowKeys;          // if true: up & down arrow keys are intercepted
	                             //  to page up/down
  
	int   modeOrder[NUM_TOOLS_SHOWN];	// specifies which drawing tools appear in the window 
	                                  //  and in what order
	
  int    lwOpt;									// user presets for livewire (see: livewireopt)
	bool   lwSmooth;							// if true: livewire line is smoothed when finished
	int    lwSmoothIts;						// iterations of smoothing is "lwSmooth" is true
	bool   lwUseWrap;							// if true: "livewireF" is used to show the livewire
	                              //  wrapping from the last livewire pt back to the first
	
	int    lwAreaSize;						// list of sizes for a box limit for livewire as:
																//  512x512, 1024x1024, 2048x2048 or 4096x4096 px
	int    lwBinning;							// the amount of binning to use in livewire as either:
																//  0, bin-by-2, bin-by-3, bin-by-4 or bin-by-5
	int    lwNoiseRed;						// the type of noise reduction to use as either:
																//  median, mean or gaussian
	int    lwColor;								// allows the use to choose a few different colors
	                              //  for the livewire line
	bool   lwDontShowAgain;				// if true: the livewire option popup won't appear
	                              //  whenever the "Livewire" radio buttion is clicked
	
	bool   waSmooth;							// if true: livewire line is smoothed when finished
	int    waSmoothIts;						// iterations of smoothing is "lwSmooth" is true
	float  waDistBias;						// a cooefficient used to make points less likely to
																//  be selected the further they are from the middle
																//  of the wand circle
	bool   waDontShowAgain;				// if true: the wand option popup won't appear
																//  whenever the "Want" radio buttion is clicked
	
	//## LIVEWIRE OBJECTS:
	
	Livewire::WeightCalculator::Settings lwSettings;		// current settings used by the
																											//  livewire image "weights" thread
	
	Livewire::WeightCalculator *weights;			// the thread which inputs an area of image
																						//  and creates a filtered version made
	                                          //  for use by a "LivewireCalculator"
	Livewire::LivewireCalculator *livewire;		// livewire thread which runs on "weights" 
																						//  and uses dijkstra to find path of least
																						//  resistance from the previous livewire pt
																						//  to the mouse.
	Livewire::LivewireCalculator *livewireF;	// livewire thread used when "lwUseWrap"
																						//   is use on the first livewire point
																						//   in a contour and shows a path from the
																						//   mouse back to this first pt OR next
																						//   livewire pt if adding intermediates.
	
	bool lwInit;									// set to true after livewire is successfully initialized
	int lwWeightZVal;							// the Z value of the slice currently stored in "weights"
	Icont *lwPts;									// stores the active livewire points (points clicked)
																//  on the current contour, as shown by red dots
	
	float wandAvgGrayVal;					// the average gray value around where the user last
																//  clicked while using the Wand tool
	
	
  //## MOUSE:
  
  Ipoint mouse;         // the current tomogram coordinates of the mouse
  Ipoint mousePrev;     // the mosue coordinates before it's last move
  Ipoint mouseDownPt;   // the mouse coordinates when the mouse button was last pressed
	Ipoint mouseDownPrev; // the mouse coordinates the time before last when button clicked
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
  
  bool shiftDown;						// set to true when the SHIFT button is down
  Ipoint centerPt;					// the center of the currently selected contour
  bool contortInProgress;   // used in DM_WARP when user clicks close to contour edge
  bool newContStarted;			// set to true or false during DM_SCULPT if a new contour 
													  //  was started during "edit_executeSculptStart()"
	
  vector<IdxToSort> sortVals;		// stores a idx and float for each contour after
																//  "sort contours" is run
  vector<IdxToSort> sortPtVals; // stores a idx and float for each contour after
																//  "sort contours" is run
  int sortCriteriaOfVals;				// last sort criteria (see: sortcriteria) used to 
																//  populate values in "sortVals" and "sortPtVals"
  
  Icont *copiedCont;      // used to cut/copy and paste contours
  Ipoint copiedCenterPt;  // the center of area of the last contour cut/copied
  
  bool initialized;           // is set to true after values have been set
  int xsize, ysize, zsize;    // size of the image / tomogram
	
  int extraObjNum;            //| a number reference to an extra object for drawing tools
  int extraObjText;           //| " " " " " for text
	int extraObjLW;							//| " " " " " for a yellow livewire line
	int extraObjLWPts;					//| " " " " " for red livewire points (where clicked)
	int extraObjWPts;						//| " " " " " for red magic wand area
};



//############################################################


//-------------------------------
//## SMALL FUNCTIONS:

Iobj   *getCurrObj();
Icont  *getCurrCont();
Ipoint *getCurrPt();
bool isCurrObjValidAndShown();
bool isCurrContValid();
bool isCurrPtValid();

int removeAllDeleteFlaggedContoursFromObj( Iobj *obj, int objIdx );


//-------------------------------
//## EDITING FUNCTIONS:

int edit_getZOfTopZap();
int edit_setZapLocation(float x, int y, int z, bool redraw);
int edit_changeZTopZap( int changeZ );
int edit_changeSelectedSlice(int changeZ, bool redraw, bool snapToEnds=true);

int edit_addContourToObj(Iobj *obj, Icont *cont, bool enableUndo);
int edit_removeAllFlaggedContoursFromObj(Iobj *obj );
bool edit_selectContourPtNearCoordsCurrObj(float x, float y, int z,
																					 float distTolerance);
bool edit_selectNearPtInCurrObj(Ipoint *centerPt, float distTol,
																float distTolCurrCont, bool countInsideCurrCont);
bool edit_selectVisiblePtNearCoords(Ipoint *mouse, float distScreenPix);

bool edit_copiedContIfDiffSlice(bool selectNewCont);

void edit_executeSculptStart();
void edit_executeSculpt();
void edit_executeSculptPush(Ipoint center, float radius);
void edit_executeSculptPinch(Ipoint center, float radius);
void edit_executeSculptEnd();
void edit_executeMinorCorrectEnd( float radius );

void edit_executeJoinEnd();
void edit_executeJoinRectEnd();

void edit_executeWarpStart();
void edit_executeWarp();
void edit_executeWarpEnd();

void edit_executeCurve(bool forceEndContour);
void edit_executeCircleEnd();

void edit_inversePointsInContour(bool reorder);
int  edit_reduceCurrContour();
int  edit_smoothCurrContour();

bool edit_doesBBoxTouchCircle(Ipoint *ll, Ipoint *ur, Ipoint *center, float radius);
int edit_eraseContsInCircle(Ipoint center, float radius);
int edit_erasePointsInCircle(Ipoint center, float radius);
bool edit_breakPointsInCircle(Ipoint center, float radius);

void edit_breakCurrContIntoSimpleContsAndDeleteSmallest();
void edit_makeCurrContSimple();       
void edit_deleteCurrContIfTooSmall();
void edit_joinCurrContWithAnyTouching();

float edit_selectClosestPtInCurrCont(Ipoint *givenPt);
int   edit_addPtsCurrCont(float maxDistBetweenPts);
int   edit_addPtsCurrContInRadius(Ipoint *centerPt, float radius, float maxDistPts);

bool edit_selectNextIntersectingCont();
bool edit_findIntersectingCont(int objIdx, int contIdx,
                               int *pCross, int *objCross, int *contCross, bool *samePts,
                               int minObj, int maxObj, bool skipPrevContsInObj);
int edit_countOverlappingContsCrude(int objIdx, int contIdx);

string edit_getSortValString(int sortCriteria);

float edit_getGreyValue( Ipoint *pt );
float edit_avgGreyValueOfPts( Icont *cont );
float edit_getAvgPtSize( Iobj *obj, Icont *cont );
bool edit_goToContNextBiggestFindVal(bool findNextSmallest, bool recalc,
                                     bool useCurrentValue, float chosenTarget=FLOAT_MIN);
float edit_getSortValue(int sortCriteria, Iobj *obj, Icont *cont, int ptIdx=0);
void edit_reorderConts(int sortCriteria, int minCont, int maxCont,        
											 bool calcValsOnly, bool reverse, bool printVals);

bool edit_isSimpleWithinCircle( Icont *cont, Ipoint *center, float radius);

void edit_setupLivewire( int w, int h );
void edit_executeLivewireClick();
int edit_addLivewirePtsToCont(Icont *cont, int startIdx,
															Livewire::LivewireCalculator *livewire,
															QPoint qptEnd, int z, bool addUndo=false, bool 
															reverse=true);
bool edit_finishLivewireOnCurrCont(bool deselect = true);
bool edit_startLivewireFromPt(int x, int y, int z, bool useLivewireF);
bool edit_setLivewireImage( int z );
bool edit_executeLivewireSelectPt();
//bool edit_getNextAndPrevLivewirePts(int &startPt, int &endPt);

void edit_executeWandAdd();
int edit_addWandPtsToCont( Icont *cont, Ipoint centerPt, int minDist, 
													 float targetGray, float tolerance,
													 float distBiasCoefficient, bool allPts );
int edit_scanContFill( Icont *cont, Icont *contOut,
											 Ipoint centerPt, int radius, int minPts );
int edit_scanContShrink( Icont *cont, int radius, int iterations, bool erodeAllEdgePts );

int edit_contCorrectCircle( Icont *cont, Ipoint centerPt, int radius,
													  int minPts, Ipoint includePt );




//############################################################

//## INLINE FUNCTIONS:

inline int edit_getPixNeigh( Icont *cont, int pixN[8], int x, int y, int w, int h )
{
	int ptIdx = y*w + x;
	
	pixN[PX_N]  = (getPt( cont, ptIdx+w   )->z == PIX_OFF) ? 0 : 1;
	pixN[PX_S]  = (getPt( cont, ptIdx-w   )->z == PIX_OFF) ? 0 : 1;
	pixN[PX_E]  = (getPt( cont, ptIdx  +1 )->z == PIX_OFF) ? 0 : 1;
	pixN[PX_W]  = (getPt( cont, ptIdx  -1 )->z == PIX_OFF) ? 0 : 1;
	pixN[PX_NE] = (getPt( cont, ptIdx+w+1 )->z == PIX_OFF) ? 0 : 1;
	pixN[PX_SE] = (getPt( cont, ptIdx-w+1 )->z == PIX_OFF) ? 0 : 1;
	pixN[PX_NW] = (getPt( cont, ptIdx+w-1 )->z == PIX_OFF) ? 0 : 1;
	pixN[PX_SW] = (getPt( cont, ptIdx-w-1 )->z == PIX_OFF) ? 0 : 1;
}


//############################################################
