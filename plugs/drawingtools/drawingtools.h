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
class FloatSpinBox;   // Class written by David

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
  void loadSettings();
  void saveSettings();

  bool drawExtraObject( bool redraw );
  void reduceCurrentContour();
  void smoothCurrentContour();
  void reduceConts();
  void smoothConts();
  bool executeDAction();
  void selectNextOverlappingContour();
  void printModelPointInfo();
  void printObjectDetailedInfo();
  void moreActions();
  void moreSettings();
  void sortContours();
  void findContours();
  void test();
  void cut();
  void copy();
  void paste(bool centerOnMouse);
  
  void changeType( int value );
  void changeTypeSelected( int newType );
  void changeSmoothTol( int value );
  void setReducePtsOptionAndChangeDisplay(int value);
  void changeSmoothPtsDist( int value );
  void changeSmoothTensileFract( int value );
  void changeReducePts();
  
  void changeDeformCircleRadius( float value, bool accel=false );
  void clearExtraObj();
  
 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  
 private:
  
  QVButtonGroup *typeButtonGroup;
  QRadioButton *typeRadio_Normal;
  QRadioButton *typeRadio_Deform;
  QRadioButton *typeRadio_Join;
  QRadioButton *typeRadio_Transform;
  QRadioButton *typeRadio_Eraser;
  
  QGroupBox    *grpOptions;
  QGridLayout  *gridLayout1;
  QLabel       *lblMinArea;
  QLabel       *lblTol;
  FloatSpinBox *fSmoothSpinner;
  QCheckBox    *reducePtsCheckbox;
  QLabel       *lblSmoothPtsDist;
  FloatSpinBox *fSmoothPtsDist;
  QLabel       *lblSmoothTensileFract;
  FloatSpinBox *fSmoothTensileFract;
  
  QGroupBox    *grpActions;
  QVBoxLayout  *vboxLayout1;
  QPushButton  *reduceContsButton;
  QPushButton  *smoothContsButton;
  
  QWidget      *widget1;
  QGridLayout  *gridLayout2;
  QPushButton  *moreActionsButton;
  QPushButton  *moreSettingsButton;
};

//-------------------------------
//## CONSTANTS:

enum drawmodes      { DM_NORMAL, DM_DEFORM, DM_JOIN, DM_TRANSFORM, DM_ERASER,
                      DM_RESIZEPT };
enum smoothmodes    { RD_TOL, RD_MINAREA };
enum wheelbehaviour { WH_NONE, WH_DEFORMCIRCLE, WH_SLICES, WH_CONTS, WH_PTSIZE };
enum dkeybehavior   { DK_NONE, DK_TOEND, DK_NEARESTEND, DK_DELETEPT, DK_REMOVEPTSIZE,
                      DK_REMOVEALLPTSIZES };

enum sortcriteria   { SORT_NUMPTS, SORT_LENGTH, SORT_AREA, SORT_CLOCKWISEAREA,
                      SORT_AVGPTSIZE, SORT_AVGGRAY, SORT_STIPPLED, SORT_RANDOM, 
                      SORT_AVGX, SORT_AVGY, SORT_AVGZ,
                      SORT_MINX, SORT_MINY, SORT_MINZ,
                      SORT_PTX, SORT_PTY, SORT_PTZ,
                      SORT_PTSIZE, SORT_PTGREY, SORT_NUMOPTIONS };

const int NUM_SAVED_VALS = 17;

//-------------------------------
//## DRAWINGTOOLS DATA STRUCTURE:

struct DrawingToolsData   // contains all local plugin data
{
  ImodView    *view;
  DrawingTools *window;
  
  //## DRAWING OPTIONS:
  
  int drawMode;        // the drawing tool type currently selected (see enum "drawmodes")
  
  int    draw_reducePts;            // if 1: drawn conts will automatically be reduced
  float  draw_reducePtsTol;         // the tolerance setting used in "imodContourReduce"
  float  draw_reducePtsMinArea;     // the minimum area which must be formed by three
                                    //  consecutive pts in a contour else the middle
                                    //  one be removed - see the 
                                    //  "cont_reducePtsMinArea" function
  int    draw_reducePtsOpt;         // if 0: use tolerance, if 1: use min area
  
  float  draw_smoothMinDist;        // min distance between consecutive points
  float  draw_smoothTensileFract;   // tensile fraction used by the catumull-rom spline
                                    //  algorithm in the "cont_addPtsSmooth" function
                                    //  NOTE: 0=straight, 1.5=smooth, 2.0>=very bendy
  float  draw_deformRadius;         //  the radius, in pixels, of the deformation circle
  
  
  //## SETTINGS:
  
  int  wheelBehav;              // changes the behaviour of the mouse wheel
                                //   (see: wheelbehaviour)
  int  dKeyBehav;               // the action when [d] is pressed 
                                //   (see: dkeybehavior)
  int  pgUpDownInc;             // the number of slices to iterate when
                                //   PageUp or PageDown is pressed
  
  bool   useNumKeys;            // intercepts number keys [1]-[5] to change draw mode
  bool   markTouchedContsAsKey; // if true: any contour modified with deform changes
                                //  to unstippled
  int    wheelResistance;       // the higher the value, the slower mouse scrolling works
  bool   showMouseInModelView;  // shows the extra object in the model view
  
  int    selectedAction;        // the last selected action under "More Actions"
  int    sortCriteria;          // the lat sort criteria selected via:
                                // "More Actions >> sort ... " (see: sortcriteria)
  int    findCriteria;          // the lat find criteria selected via:
                                // "More Actions >> sort ... " (see: sortcriteria)
  
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


//-------------------------------
//## EDITING FUNCTIONS:

int edit_getZOfTopZap();
int edit_setZapLocation( float x, int y, int z, bool redraw );
int edit_changeSelectedSlice( int changeZ, bool redraw, bool snapToEnds=true );

int edit_addContourToObj( Iobj *obj, Icont *cont, bool enableUndo );
int edit_removeAllFlaggedContoursFromObj( Iobj *obj );
bool edit_selectContourPtNearCoords(float x, float y, int z, float distTolerance);

bool edit_copiedContIfDiffSlice( bool selectNewCont );

void edit_executeDeformStart();
void edit_executeDeform();
void edit_executeDeformPush( Ipoint center, float radius );
void edit_executeDeformPinch( Ipoint center, float radius );
void edit_executeDeformEnd();
void edit_executeJoinEnd();

void edit_inversePointsInContour(bool reorder);
int  edit_reduceCurrContour();
int  edit_smoothCurrContour();

bool edit_doesBBoxTouchCircle( Ipoint *ll, Ipoint *ur, Ipoint *center, float radius );
int edit_eraseContsInCircle( Ipoint center, float radius);
int edit_erasePointsInCircle( Ipoint center, float radius );
bool edit_breakPointsInCircle( Ipoint center, float radius );

void edit_breakCurrContIntoSimpleContsAndDeleteSmallest ();    
void edit_makeCurrContSimple ();                
void edit_deleteCurrContIfTooSmall();              
void edit_joinCurrContWithAnyTouching();            

bool edit_selectNextOverlappingCont();


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
