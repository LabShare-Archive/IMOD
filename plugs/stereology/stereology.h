#include "dialog_frame.h"
#include <QKeyEvent>
#include <QLabel>
#include <QEvent>
#include <QTime>
#include <QCloseEvent>
class QPushButton;
class QCheckBox;

class QLabel;
class QComboBox;
class QScrollArea;
class QGridLayout;
class QVBoxLayout;
class QHBoxLayout;
class QSpacerItem;
class QEvent;
class QColorDialog;
class QColor;
class QCompleter;
class QListView;
class QMenu;

#include "imodplugin.h"
#include "dia_qtutils.h"
#include "icontextra.h"
#include "customdialog.h"

#include <qstring.h>
#include <vector>
using namespace std;

//############################################################
//## CONSTANTS:

enum gridtype  { GD_LINES, GD_CROSSHAIRS, GD_ARROWS, GD_OFFPTS, GD_RAND,      SPACE1,
	               GD_VERTLINES, GD_HORZLINES, GD_UPDIAGONAL, GD_DOWNDIAGONAL,
								 GD_LINEPAIRS, GD_CYCLOIDS, GD_CYCLOIDS2, GD_CYCLOIDS3,       SPACE2,
								 GD_RECTS, GD_FORBIDSQ1, GD_FORBIDSQ2, GD_FORBIDSQ3,
	               GD_OFF, NUM_GRID_TYPES };

enum rectdisplay { RD_FORBIDDENSQ, RD_NORMAL, RD_DOTTED, RD_GRID };
enum measuretype { MT_VOL, MT_SA, MT_LEN, MT_NUM };

enum paintmode   { PM_OFF, PM_PAINT, PM_INTERCEPTS };

enum ptchange    { SEL_NA, SEL_ON, SEL_OFF, SEL_TOGGLE };
enum rangeopts   { RG_CURR=0, RG_ALL=1, RG_CUSTOM=2 };

enum gridlabels  { GL_OFF, GL_NUMS, GL_LETTERS_NUMS };
const int NUM_SAVED_VALS = 65;

const int   NUM_MASKS      = 5;					// max number of mask objects in applyMasks()
const float PT_PREC        = 0.001f;		// used by 'ptsEqualXYZ' function
const char  PLUSMINUS_SIGN = 0x00B1;		// plus minus sign
const int   ARCPOINTS      = 12;				// number of points used in cycloid
const float INTERCEPT_DIST = 2.0f;			// pixels distance between intercept points
const float INTERCEPT_SNAP = 5.0f;			// pixels mouse must be to snap to intercept pt

//############################################################


//********************
//** NameEntry is a structure copied from the "Name Wizard"
//** plugin, whereby 'name entries' are loaded from a CSV file.
//** In this plugin, this structure is created only temporarily
//** and used to save or load a default set of category names,
//** colors and sphere sizes from 'default_categories.csv'.

struct NameEntry
{
  QString name;
  QString red;
  QString green;
  QString blue;
  QString hyperlink;
  QString identifier;
  QString description;
	QString superCat;
	QString synonyms;
};


//********************
//** SPoint represents a point within a stereolgy grid.
//** Each point should has an array of "catSel" showing
//** which categories in the grid it belongs to.
//** I've thought about adding a 'interceptPts' vector
//** for cases where the point represents a lines,
//** but for now have worked around this.

class Spoint
{
public:
	//## VARIIABLES:
	
	Ipoint pos;
	
	bool checked;								// says wether this point has been "checked/classified"
	vector<bool> catSel;				// shows which categories this point belongs to
															//  (may switch to int later for better efficiency)
	
	//## METHODS:
	
	Spoint( float xVal, float yVal, int zVal, int numCategories );
	void reset();
	bool isCatOn( int catIdx );
	void setCatOn( int catIdx, bool catOn );
	int  numCatsOn();
	bool hasSameState( Spoint *spt );
};


//********************
//** CategoryObj represents a category which a point can be assigned to.
//** Each category is represented by a object containing the string "(STEREOLOGY)"
//** and into this objects it's points are added. Each category should be associated
//** with a single stereology grid set. The follow examples belong to grid set 1:
//** 
//**    > NOTHING.           #(STEREOLOGY)#id=1#
//**    > Nucleus.           #(STEREOLOGY)#id=1#

class CategoryObj
{
public:
	//## VARIABLES:
	
	int objIdx;							// the index of this object within the IMOD model
													//  and defaults to -1 if not tied to an object
	QString gridSetId;			// identifies which GridSetObj this belongs to
	
	QString objName;				// name of object..   eg: "Nucleus. #(STEREOLOGY)#id=1#"
  QString categoryName;		// name of category.. eg: "Nucleus"    (for the case above)
	QColor color;						// the color of this object
	
	NameEntry nameEntry;		// stores the "NameEntry" values if this was loaded from
													//  a "standard names and colors .csv file"
	QString toolTip;				// stores a string representation of "nameEntry" values
	int sphereSize;					// sphere size for the object - if set to -1 (the default)
													//  the plugin will decide how bit to make spheres
	int lineWidth;					// the line width for the object, set to 1 by default
	
	//** TEMP ANALYSIS VARIABLES:
	
	bool includeInResults;		// temp value used to include/omit category during analysis
	long numPtsOn;						// updated and used during analysis (using "calcResults()")
	float volumeDensity;			// used to store the volume density (Vv)
	float stDev;							// used to store the standard deviation
	long numCounts;						// updated during analysis (using "calcSurfAreaDensity()")
														//  by multiplying "numPtsOn" by calcTotCountUsingName()
	
	long numIntercepts;				// used to record the number of intercept points in 
														//  a category, equal to the number of points in the object
	
	int  changeOpt;						// |-- used in applyMask() and applyRulesToChangePts()
	int  selectOpt;						// |   whereby: 0=ignore,1=on, 2=off, 3=toggle
	
//## METHODS:
	
	CategoryObj();
	void reset();
	void setNameAndTooltip( QString newCatName, QString newToolTip=0 );
	bool catNameStartsWithNumber();
	long calcCountUsingName();
};



//********************
//** GridSetObj represents the settings for a stereology grid set. The
//** settings for each grid set are stored within a special object which
//** starts with "STEREOLOGY.GRID" and stores the other saved values
//** in the rest of it's name. The follow example is for grid set 1:
//** 
//**    > STEREOLOGY.GRID  #id=1#type=pts#spacing=50,50,5#x=0,8000#y=0,6000
//**          #z=10,790#(DONT_RENAME)#
//** 

class GridSetObj
{
public:
	//## VARIABLES:
	
	int objIdx;							// the index of this object within the IMOD model
													//  and defaults to -1 if not tied to an object.
	
	//** VALUES SAVED TO OBJECT NAME:
	
	QString gridSetId;			// should uniquely identify this GridSetObj within the model
	
	int gridType;						// the type of grid used and dictates how 
													//  points are added (see: gridtype).....
													//  note that if equal to "GD_RAND", points are 
													//  added to random positions within the grid using
													//  limits rather than any traditional grid
	
	float xSpacing;					// |-- the spacing between gridlines in X an Y in pixels
	float ySpacing;					// |  
	int   zSpacing;					// spacing between gridlines in Z/sections where a value of 1 
													//  means a grid on every section from zMin to zMax inclusive
	
	int xMin, xMax;					// |-- the limits of the stereology grid along each dimension
	int yMin, yMax;					// |   .... keep in mind stereology points start at 
	int zMin, zMax;					// |        "xMin + xSpacing" along X and Y
	
	bool allowMultipleCats;	// if true: user can assign multiple categories to each point
													//  if false (default): points are limited to one category only 
	
	bool allowIntercepts;		// it true: the user can use the 'Intercepts' tool which 
													//  allows him to place category points along a grid line
	
	bool showSubRects;			// if true: subsample rectangles will be drawn over the grid(s)
	bool subRectsApplied;		// set as true if forbidden squares are applied
													//  will be used during "setupPts()"
	int rXSpan, rYSpan;			// |-- the side length and gap between forbidden squares
	int rXGap,  rYGap;			// |
	
	//** CALCULATED VALUES:
	
	int cols;								// number of vertical   lines in the grid (x)
	int rows;								// number of horizontal lines in the grid (y)
	int grids;							// number of slices grid is applied to as calculated by
													//  equal to [(zMax-zMin+1) / zSpacing]
	
	long ptsPerGrid;				// equal to [cols*rows]
	long maxPts;						// equal to [cols*rows*slices] - the maximum number of
													//  points which the user can iterate through
	long numPtsChecked;			// updated and used during analysis to match number of
													//  point in this object - and thus the number of points
													//  user has iterated through
	
	int ptsPerRowSetup;			// |-- the number of columns and points/grid after setupPts():
	long ptsPerGridSetup;		// |    set to "cols" and "rows" by default, but are smaller
													// |    if "subRectsApplied" is true. These values are used
													// |    only in navigation, so not especailly important
	
	long currPtIdx;					// index of the currently selected point in the "pts" vector
	
	bool countingStarted;		// set to true when grid and categories are finalized and
													//  setup, and thus counting can start
	bool countingFinished;	// set to true when all points have been counted and
													//  is updated by "countCheckedPts()"
	
	
	//** TEMP ANALYSIS VARIABLES:
	
	long totPtsWithCatOn;		// temp value updated by "calcResults()"
	long totCatHits;				// temp value updated by "calcResults()"
	long totCounts;					// used to compute total "counts" by calling
													//  "calcTotCountUsingName()" on each category
	vector<QString> gridTypeStr;		// used to match a string to each
																	//  "gridtype" enum values
	
	
	//** CATEGORY VALUES AND POINTS:
	
	vector<Spoint> pts;						// stores a vector of all stereology points in the grid
																//  ... this vector is empty until "countingStarted" is
																//  set to true (since it can a while to initalize) 
																//  and will contain "maxPts" entries ordered by (x,y,z)
																//  in cases where "allowRandomPts" is left as false.
																//  If "allowRandomPts" is true: points are added in 
																//  random positions, one at a time.
	
	vector<CategoryObj> catObj;		// stores a list of all categories for this grid
																//  the size and order of these categories should 
																//  correspond to the vector of "catSel" for each
																//  stereology point in the "pts" vector.
	
//## METHODS:
	
	GridSetObj();
	long ptsize();
	void reset();
	bool verifyVals();
	bool calcVals();
	
	bool xAlternatesOverRows();
	bool allowRandomPts();
	bool isVolumeGridType();
	bool isSurfaceAreaGridType();
	bool isNumberDensityGridType();
	
	float lineLengthPerPtInPixels();
	void genContourForPt( long ptIdx, Icont *cont );
	float areaPerPtInPixels();
	int  numCatsStartingWithNumbers();
	bool isPtInSubRect( float x, float y, int z, bool includeLine, bool ignoreZ=true );
	bool isSPtContainedInsideSubRect( float x, float y, int z, bool ignoreZ=true );
	Ipoint calcPtPosAtIdx( long ptIdx );
	Spoint *getSPt( long ptIdx );
	Ipoint getPos( long ptIdx );
	long getPtIdx( int xIdx, int yIdx, int zIdx );
	Spoint *getSPtByIdx( int xIdx, int yIdx, int zIdx );
	Spoint *getSPtByPt( Ipoint *pt, bool checkAllPts );
	Spoint *getCurrSPt();
	bool isGridOnSlice( int sliceIdx );
	vector<long> getIdxPtsInRadius( Ipoint *center, float radius );
	bool isPtInRadius( Ipoint *center, float radius, long *closestPtIdx );
	bool getInterceptNearPt( Ipoint pt, Ipoint *ptIntercept, float maxDist );
	bool setupPts( bool useSubRectsIfOn );
	bool areMaxPtsSetup();
	int  getCatObjIdx( int catIdx );
	bool doesPtCatExists( long ptIdx, int catIdx, bool printError=true );
	bool setCurrPt( long newPtIdx, bool wrap );
	bool changeCurrPt( long changeAmount, bool wrap );
	Ipoint genRandomPt( bool useSpacingX, bool useSpacingY, bool useSpacingZ );
	long addRandomPts( long numRandPts, bool useSpacingX, bool useSpacingY, bool useSpacingZ, float minDistFromOtherPts=-1.0f, long totalRetries=0, float biasForSameZAsLastRandPt=0.0f );
	long countCheckedPts( bool earlyExit );
	bool selectFirstUncheckedPt( bool startAtCurrPt, bool backwards=false );
	int  currGrid();
	
	bool validatePtValues( QString *badPtsStr, long *firstBadPtIdx, int *numCheckedPtsNoCat, int *numPtsMultiCat );
	bool validateAllPtValues();
	long calcResults( bool includeAllCats, bool onlyIncludeChecked, long *catHits, int minZVal, int maxZVal, bool tallyCounts=false );
	void calcIntercepts( bool includeAllCats, Imod *imod, int minZVal, int maxZVal );
	QString printResults( bool includeAllCats, bool transpose, QString sepChar, bool includeHeader=true, bool includeStDev=false );
	QString printSurfAreaDensityResults( QString sepChar, long numPtsForCalc, float pixelSize, QString units, bool shortVersion );
	QString printLengthDensityResults( QString sepChar, long numPtsForCalc, float pixelSize, QString units );
	QString printNumberDensityResults( QString sepChar, float pixelSize, QString units, float zScale, int zMinUsed, int zMaxUsed );
	void setupGridTypeStrings();
	int getMatchingGridType( QString gridTypeAsString );
	QString gridTypeAsString();
	QString toString( bool includeCats );
};


//********************
//** StereoLineItem is used to store the form elements which
//** are presented in a single row and represent a single 
//** category, including a button the use can click to
//** assign the selected point to that object/category

struct StereoLineItem
{
	int          objIdx;
	bool         setup;
  
  QWidget      *widLine;
  QHBoxLayout  *layLine;
  QLabel       *lblShortcut;
  ColorButton  *btnColor;			// exists but is disabled
  QPushButton  *btnCat;
	
	QLineEdit    *txtName;
	
  StereoLineItem()
  {
    setup = false;
		objIdx = -1;
  }
};





//-------------------------------
//## STEREOLOGY WINDOW:

class Stereology : public DialogFrame
{
  Q_OBJECT

	
	friend void f(DialogFrame*, Stereology*);
  void g(DialogFrame*);
	
	
public:
  Stereology(QWidget *parent, const char *name = NULL);
  ~Stereology() {};
  QAction *addAction( QMenu *menu, const char *member, QString text, QString tip );
  void resizeEvent ( QResizeEvent * event );
  
public slots:
	
	//** DRAWING METHOD:
	
	bool drawGridObject ( bool redraw );
	bool drawSubsampleRects( bool redraw );
	bool drawSelObject  ( bool redraw );
  bool drawBlackObject( bool redraw );
	bool drawExtraObject( bool redraw );
	
	//** PLUGIN SETTINGS METHODS:
	
  void initValues();
  void loadSettings();
  void saveSettings();
	void keepOnTop( bool state );
  void loadWordList();
	
	//** LOAD/SAVE CATEGORIES NAMES FROM CSV FILE:
	
	void loadDefaultCatNames();
	void loadCatNamesViaDialog();
	void loadCatNamesForCounting();
	void saveCatNamesAsDefault();
	void saveCatNamesViaDialog();
	int  loadCatNamesFromFile( QString filePath, GridSetObj *g );
	bool saveCatNamesToFile( QString filePath, GridSetObj *g );
	
	//** LOAD/SAVE GRIDS FROM IMOD MODEL:
	
	void reloadGridFromImodModel();
	bool loadGridFromImodModel( bool firstLoad );
	int  loadAllGridsAndCatsFromImodObjs( bool printErrors );
	bool loadGridPtsFromImodObjs( GridSetObj *g, bool makeSureGridSetup, bool printResultsConsole, bool showPopupIfErrors, long *numContPtsLoaded, long *numCheckedPts, long *numUnmatchedPts );
	bool setupImodObjsFromGridAndCats( bool skipIfExists, bool overwriteNames, int *numObjAdded, int *numObjChanged );
	
	//** LITTLE METHODS:
	
	GridSetObj *getCurrGridSetObj();
	QString getObjName( int objIdx );
	bool setObjLabel( Iobj *obj, QString newLabelStr );
	QString getObjLabel( Iobj *obj );
	QString getObjLabel( int objIdx );
	QString distToUnitsStr( float distInPixels, int decimals=2 );
	QString formatApproxTime( float totSeconds );
	QString numberToAlphabetChars( int number, bool caps );
	
	//** GRID SETUP METHODS:
	
	void addGrid( bool checkUniqueGridId );
	void updateGridGuiFromGrid( bool setCheckBoxesIntelligently );
	bool updateGridFromGridGui( bool checkCheckboxes, bool updatePtSelSpn, bool jumpToGrid );
	void finalizeGrid();
	void startNewGrid();
	void modifyExistingGridViaObjectLabels();
	void addExtraGridCategoriesToImodModel();
	void makeGridFiner();
	
	void changeTabSelected( int newValue );
	void changeShowGridDisplayChk();
	void changeGridTypeCmb( int newValue );
	void changeGridSpn( int newValue );
	void changeGridDblSpn( double newValue );
	void changeGridChk();
	
	//** CATEGORY SETUP METHODS:
	
	void addCategory();
	void deleteCategory();
	void setupCountingCategories( int maxIntersections, QString labelStr );
	void startCounting();
	void updateGuiToMatchCountingStarted();
	
	void removeAllLineItems();
	void updateItemListGuiFromCats();
	void updateCatsFromItemListGui();
	
	//** POINT SELECTION/CHANGE METHODS:
	
	void addRandomPts();
	void goToRandomPos();
	void goToNextPt();
	void goToPrevPt();
	void goToNextRow();
	void goToPrevRow();
	void goToNextGrid();
	void goToPrevGrid();
	void jumpToPt( int changeAmount );
	void goToNextUncheckedPt();
	void goToPrevUncheckedPt();
	void goToFirstUncheckedPt();
	void goToLastUncheckedPt();
	void jumpToUncheckedPt( bool startAtCurrPt, bool backwards );
	void changeCurrPtSpn( int newPtNum );
	bool updateCurrPtInGui( bool jumpToPt, bool updateSpinner );
	bool updateCatsSelectedGui();
	
	void changePtPaintRadius(float value, bool slowDown=false);
	void changeColorBtn();
	void togglePtChecked();
	void changePtCheckedClicked();
	void toggleCategory( int catIdx );
	void catToggleBtnPushed();
	void updateTimeWorked();
	bool updatePtCat( long ptIdx, int catIdx, bool newVal, bool redraw=true, bool updateGui=true );
	bool changePtCatAndUpdateObj( Spoint *spt, int catIdx, bool newVal );
	bool changePtCheckedAndUpdateObj( Spoint *spt, bool turnCheckedOn );
	bool addOrRemoveSinglePtFromObj( int objIdx, Ipoint newPt, bool turnOn );
	bool isCatObjValid( int catIdx, bool printError=true );
	bool isStereologyObj( int objIdx );
	bool isStereologyObjSelected( bool showErrorIfTrue, const char *customMsg=NULL );
	bool goToFirstInvalidPoint();
	
	void togglePaintBtn();
	void toggleInterceptBtn();
	void executePaint();
	void resetPaint();
	void executeIntercept();
	void deleteIntercept();
	
	//** PLUGIN SETTINGS AND HELP METHODS:
	
	void setSpacingUsingUnits();
	void applyMask();
	void applyMasks();
	void applyIntercepts();
	void applyRulesToChangePts();
	
	void checkProgress();
	void validatePoints();
	void calculateResults();
	
	void gridSettings();
	void displayOptions();
	void buttonPressed(int);
	
protected:
	void helpPluginHelp();
  void helpNamingHelp();
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  
	
	
private:
  
	//** FIRST TAB "(1) Grid Setup":
	
	QTabWidget   *tabWidget;
	QWidget      *widGridSetup_tab0;
	
	QScrollArea  *scrollAreaGridSetup;
	
	QWidget      *widGridScroll;
	QWidget      *widGridSetup;
	
	QComboBox    *cmbGridDisplayOpt;
	
	QCheckBox    *chkSameXYSpacing;
	QCheckBox    *chkShowEverySlice;
	QCheckBox    *chkChangeDefLimits;
	
	QDoubleSpinBox  *spnSpacingX;
	QDoubleSpinBox  *spnSpacingY;
	QSpinBox			  *spnSpacingZ;
	
	QWidget      *widRowZSpace;
	QWidget      *lblGridSpX;
	QWidget      *lblGridSpY;
	
	QWidget      *widPtsRow;
	QLabel       *lblTotalPts;
	
	QGroupBox    *grpLimits;
	QGridLayout  *layLimits;
	QSpinBox     *spnLimitXMin;
	QSpinBox     *spnLimitXMax;
	QSpinBox     *spnLimitYMin;
	QSpinBox     *spnLimitYMax;
	QSpinBox     *spnLimitZMin;
	QSpinBox     *spnLimitZMax;
	
	QCheckBox    *chkSubsampleRect;
	QGroupBox    *grpRects;
	QGridLayout  *layRects;
	QSpinBox     *spnRectXSpan;
	QSpinBox     *spnRectYSpan;
	QSpinBox     *spnRectXGap;
	QSpinBox     *spnRectYGap;
	QCheckBox    *chkLockSqXY;
	
	QGroupBox    *grpGridDisplayOpts;
	QSpinBox     *spnGridSymbolSize;
	QSpinBox     *spnLineWidth;
	ColorButton  *colGridColor;
	QComboBox    *cmbSubRectDisplay;
	QCheckBox    *chkShowDashes;
	
	QCheckBox    *chkShowGridDisplayOpts;
	QPushButton  *btnStartStereology;
	
	
	//** SECOND TAB "(2) Classify Points":
	
	QWidget      *widCategories_tab1;
  
	QScrollArea  *scrollArea;
	QWidget      *widList;
  QVBoxLayout  *layList;
	QWidget      *widCatListOpns;
  QPushButton  *btnAddCat;
	QPushButton  *btnDelCat;
	QPushButton  *btnCatOpts;
	QPushButton  *btnStartCount;
	QCheckBox    *chkPtChecked;
	
  vector<StereoLineItem> lineItem;
  
  QCompleter *completer;
  
	
	//** SHARED BUTTONS (AT BOTTOM):
	
  QHBoxLayout  *layPtBtns;
	QHBoxLayout  *layExBtns;
	QSpinBox     *spnSelPt;
	QLabel       *lblMaxPts;
  QPushButton  *btnNext;
  QPushButton  *btnPrev;
	QLabel       *lblSelGrid;
	QPushButton  *btnRandom;
	QPushButton  *btnAddRandPts;
	QPushButton  *btnPaint;					// toggle botton
	QPushButton  *btnIntercept;			// toggle botton
	QPushButton  *btnOptions;

};


//-------------------------------
//## NAMEWIZARD DATA STRUCTURE:

struct StereologyData   // contains all local plugin data
{
  ImodView    *view;
  Stereology *window;
  
  //## VARIABLES:
  
  QStringList wordList;
	
	vector<GridSetObj> grid;			// an array of stereology grids
  int currGridIdx;							// used to index the currently selected grid
																//  *NOT YET USED*
	
	vector<GridSetObj> gridList;			// an array of grids loaded from the IMOD model
																		//  when loadAllGridsAndCatsFromImodObjs() called
	
  //## SAVED SETTINGS:
  
	bool sameXYSpacing;					// if true: one spin box is used to set X and Y spacing
	bool showGridEverySlice;		// if true: a single grid is project through on all slices
	bool changeDefLimits;				// if true: user can see spin boxes to change grid limits
	float defSpacingX;					// |
	float defSpacingY;					// |-- default spacing in X,Y,Z to use when plugin loads
	int   defSpacingZ;					// |
	int limitDefaultInset;			// the default amount to inset X and Y in "limits" 
															//  when "changeDefLimits" is true
	bool showChangeDefOpt;			// if true: shows a big crosshair when "Intercept" mode
	                            //  is on (paintMode == PM_INTERCEPTS)
	
	bool showGridDisplayOpts;		// if true: grid display options appear in first tab
	bool showGridInModelView;		// if true: the grid is drawn in the model view
	int gridDisplayOpt;					// dictates the appearance of the grid (see: gridtype)
															//  and is not necessarily the same as g->gridType
	int gridSymbolSize;					// the size for the grid display if set to crosshair
	                            //  or arrows
	int gridColorR;							// |
	int gridColorG;							// |-- the rgb values of the grid lines
	int gridColorB;							// |
	int gridLineThickness;			// the thickness of grid lines (by default is 1)
	int gridLabels;							// dictates the appearance of grid labels above the grid
															//  (see: gridlabels)
	int selPtDisplay;						// dictates the appearance of the selected point
	                            //  (see: gridtype)
	bool showDashes;						// if true: small dashes will be drawn to show
															//  point position on otherwise straight lines
	
	bool lockForbiddenSqXY;			// if true: the y span and y gap used for 'forbidden
															//  rectangles' are locked to the same values as x
	int subRectDisplayOpt;			// dictates the appearance of the subsample rectangles
															//  (see: rectdisplay)
	
	bool autoProgress;					// if true, will go to next unchecked point whenever user
															//  checks a category when allowMultipleCats is false
	bool centerOnEachPt;				// if true, will go to next unchecked point whenever user
															//  checks a category when allowMultipleCats is false
	bool blackVisitedPts;				// if true, each time the user checks a point, black
															//  crosshairs are drawn over checked points on the grid
	
	float paintRadius;					// the current radius of the "pt painter" tool
	int paintMode;							// the type of point adding mode which is active
															//  (see: paintmode)...  if 0: "pt painter" is inactive
															//  if 1: the "Pt Painter" tool appears and can be used
															//  to quickly classify points in the paint circle...
															//  if 2: "Intercept" tool is active
	
	bool resultsTranspose;			// if true: the final stereology table/results are printed
															//  with one category per column instead of per row.
	bool resultsSepGrids;				// if true: when results are printed, the count 
															//  for each grid will be shown in a seperate row
	int resultsSepCharIdx;			// options for using a tab, comma, semi-colon, bar or enter
															//  to separate values in the output
	int resultsOutputOpt;				// several options for outputting a copy of results to
															//  the clipboard, console or save to a file
	
	bool resultsAvgAllGrids;		// if true: all grids are used when calculating results,
															//  if false: user can specify a min and max section #
	bool resultsShowStDev;			// if true: will output an extra column/row showing 
															//  standard deviation for each category based on (n)
	
	float secsPerPt;			      // rough estimate of the average number of seconds to 
															//  classify each point - a value used to estimate time 
															//  remaining and it's default value is 3.
	
	
	bool showLoadingInConsole;	// if true: will always display results of trying to load
															//  "STEREOLOGY" objects from the model in the console
															//  (even if there are no errors found).
	float maxContsRend;					// the maximum number of to render in the grid
															//  before showing a performance warning.
	int numRandPtsToAdd;				// the number of random points to add when 'Random Pts+'
															//  button is pressed and confirmed.
	bool jumpToGridOnUpdate;		// if true: when stereology plugin loads, and when changes
															//  are made to the grid setup, ZAP will jump to show
															//  a slice containing a grid/grids
	
	int targetNumPts;					  // used in the 'Start new grid (wizard)' option to setup
															//  a grid automatically to contain ~this many points	
	
	bool randomSymbols;					// if true: a random symbols are assigned to each 
															//  category object added to the model
	bool agreeFinalizeDlg;			// if true: the "I understand" question in the
															//  "finalizeGrid()" dialog will already be checked
	
	bool showBigIntercCH;				// if true: shows a big crosshair when "Intercept" mode
															//  is on (paintMode == PM_INTERCEPTS)
	
															// MASK OPTIONS:  (see "applyMask()" method)
	int maskObjOpt;							// 0=current object only,  1=all, 2=custom
	int maskInverseOpt;					// 0=mask is inside contours, 1=outside
	int maskSectionsOpt;				// 0=current section only, 1=all, 2=custom
	int maskSelPtsOpt;					// 0=all, 1=checked, 2=unchecked only
	int maskChangeToCheckedOpt;	// 0=don't change, 1=checked, 2=unchecked
										// NOTE: Most of these use the enums (see: ptchange & rangeopts)
	
															// BATCH MASK OPTIONS:  (see "applyMasks()" method)
	bool masksDefaultUse;				// if true: will apply default category
	int  masksDefaultCat;				// the default category if isn't in a mask
	int  masksDefaultChecked;		// 0=unchanged, 1=checked, 2=unchecked
	bool masksPrintChanges;			// prints changes to console
	bool masksUse[NUM_MASKS];		// |-- whether to use, the object number and the
	int  masksObj[NUM_MASKS];		// |   category to apply for each mask option
	int  masksCat[NUM_MASKS];		// |
	
	//## OTHER:
	
	QTime timeLastClick;				// used to track time since counting was started
	float numMinutesWorked;			// stores the number of minutes the user has spent
															//  classifying points, and is only incremented if
															//  there was less than 60 seconds between clicks
	
	bool disableGuiUpdates;			// used to temporarily disable certain functions
															//  such as "updateGridGuiFromGrid()" while gui
															//  value are changed.
	
	QString defaultFilePath;		// stores location of the "standard names and colors .csv"
															//  file used to load a default list of categories
	
	int interceptCat;						// references the current category which the "Intercept"
															//  tool will add points to
	
	bool initialized;           // is set to true after values have been set
  int xsize, ysize, zsize;    // size of the image / tomogram

	bool shiftDown;							// set to true when [shift] key is down
	
	Ipoint mouse;               // the current tomogram coordinates of the mouse
	
  int but1Down;					// used for selecting points & panning (in NORMAL mode)
  int but2Down;					// used for drawing new contours       (in NORMAL mode)
  int but3Down;					// used for modifying points           (in NORMAL mode)
  
  bool but1Pressed;     //|- if true: button was just pressed
  bool but2Pressed;     //|
  bool but3Pressed;     //|
  
  bool but1Released;    //|- if true: button was just released
  bool but2Released;    //|
  bool but3Released;    //|
	
	
	int extraObjGrid;			//|
	int extraObjRect1;		//|
	int extraObjRect2;		//|
	int extraObjSel;			//|
	int extraObjBlack;		//|-- stores reference to extra objects
  int extraObjExtra;    //|
};



//############################################################



//-------------------------------
//## EDITING FUNCTIONS:

int edit_getZOfTopZap();
int edit_setZapLocation( float x, int y, int z, bool redraw );
int edit_addContourToObj( Iobj *obj, Icont *cont, bool enableUndo );

//############################################################
