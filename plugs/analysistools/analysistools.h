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

class QMenu;

#include "imodplugin.h"
#include "dia_qtutils.h"
#include "icontextra.h"
#include "ivector.h"
#include "idgrid.h"

#include <qstring.h>
#include <vector>
using namespace std;

//############################################################


//-------------------------------
//## INTERPOLATOR WINDOW:

class AnalysisTools : public DialogFrame
{
  Q_OBJECT

 public:
  AnalysisTools(QWidget *parent, const char *name = NULL);
  ~AnalysisTools() {};
  
public slots:
  
  void buttonPressed(int);
  void initValues();
  void loadSettings();
  void saveSettings();
  
  bool drawExtraObject( bool redraw );
  void moreActions();
  void moreSettings();
  
  
  QAction *addAction( QMenu *menu, const char *member, QString text, QString tip );
  void test();
  void notImplemented();
  void outputString( string bigOutputString, int outputOption );
  
  void estimateZScale();
  void outputConcavePtsAnalysis();
  void outputTubeSizeAnalysis();
  void outputBranchingAnalysis();
  void outputVolumeWithinXAnalysis();
  void findClosestDistanceSurfsTwoObjects();
  
  void generateDefGrid();
  void deformObjectsUsingDefGrid();
  void gridDisplayOptions();
  void analyzeDefPoints();
  void delimitSectionBoundaries();
  void fillInRangeOfPixelValues();
  void deleteSlice();
    
  void printSectionInfo();

  
  
  void clearExtraObj();
  
 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  
 private:
  
  QMenu *actionMenu;  
  /*
  QMenu *analysisMenu;
    QAction *analyzeZScaleAct;
    QAction *analyzeTubesAct;
    QAction *analyzeBranchingAct;
    QAction *analyzeConvexPtsAct;
  QMenu *gridMenu;
    QAction *gridGenerateAct;
    QAction *gridDeformObjsAct;
    QAction *gridViewOptionsAct;
    QAction *gridOpenAct;
    QAction *gridSaveAct;
  QMenu *sectionMenu;
    QAction *sectionMarkBackgroundAct;
    QAction *sectionMarkGreyRangeAct;
  QMenu *displayMenu;
    QAction *displayCutSurfZAct;
    QAction *displayShowInterpolatedContsAct;
    QAction *displayDensityGridAct;
  */
	//QGroupBox    *grpDefGrid;
  //QGridLayout  *gridLayout1;
  //QPushButton  *genDefGridButton;
  //QPushButton  *defObjectsWithDGButton;
  //QPushButton  *moreActionsButton;
    
  QWidget      *widget1;
  QGridLayout  *gridLayout2;

  QPushButton  *moreActionsButton;
  QPushButton  *moreSettingsButton;
};

//-------------------------------
//## CONSTANTS:

const int NUM_SAVED_VALS = 3;

//-------------------------------
//## DRAWINGTOOLS DATA STRUCTURE:

struct AnalysisToolsData   // contains all local plugin data
{
  ImodView    *view;
  AnalysisTools *window;
  
  //## DATA:
  
  DeformationGrid dgrid;        // deformation grid
  
  unsigned char *data;
  unsigned char *temp;
  
  //## SETTINGS:
  
  bool  arrowKeysSections;      // if true: arrow keys jump between sections.
  int   selectedAction;         // the last selected action under "More Actions"
  
  //## DEFORM GRID:
  
  bool  grid_showGrid;            // draws the deformation grid vectors
  bool  grid_showGridBounds;      // draws the deformation grid cell boundaries
  bool  grid_showGridGreen;       // draws the deformation grid origional in green
  bool  grid_showModelView;       // draws the deformation grid in the Model View
  int   grid_displayLineW;
  int   grid_displaySpheres;
  int   grid_displayLineWDot;
  
  int   grid_colsX;
  int   grid_rowsY;
  float grid_power;
  
  
  
  //## MOUSE:
  
  Ipoint mouse;
  
  //## OTHER:
  
  bool shiftDown;               // set to true when the SHIFT button is down
  
  bool initialized;             // is set to true after values have been set
  int xsize, ysize, zsize;      // size of the image / tomogram
  int extraObjG;                // extra object for deformation grid vectors
  int extraObjGB;               // extra object for deformation grid boundaries
  int extraObjGG;               // extra object for defomration grid (green) boundaries
};

//unsigned char *data;
//unsigned char *temp;


//############################################################



//-------------------------------
//## SMALL FUNCTIONS:

Iobj *getCurrObj();
Icont *getCurrCont();
Ipoint *getCurrPt();
bool isCurrObjValidAndShown();
bool isCurrContValid();
bool isCurrPtValid();

int removeAllDeleteFlaggedContoursFromObj( Iobj *obj );

//-------------------------------
//## EDITING FUNCTIONS:

int edit_getZOfTopZap();
int edit_setZapLocation( float x, int y, int z, bool redraw );
int edit_changeSelectedSlice( int changeZ, bool redraw );

int edit_addContourToObj( Iobj *obj, Icont *cont, bool enableUndo );
int edit_removeAllFlaggedContoursFromObj( Iobj *obj );




//-------------------------------
//## ANALYSIS FUNCTIONS:

float analysis_estimateZScale( int objIdx, float currZScale, bool useHemispheres,
                               bool calcSpheres, float maxDeviation,
                               float startChangeZ, float accuracyZScale,
                               int maxItsCenter, float maxZChange,
                               bool showSurfClassif, bool appendToCSV,
                               bool summaryCSV, bool printOnlyAccepted,
                               bool showCenters );
string analysis_outputTubeSizeAnalysis( int minObj, int maxObj, bool printAllConts,
                                        bool convertUnits );

string analysis_outputConcavePtsAnalysis( int minObj, int maxObj, bool printAllConts );
string analysis_outputBranchingAnalysis( int objMainLen, int objBranches,
                                         int  branchDepth, float maxDistBranch,
                                         bool addRadiusToLen, bool convertUnits,
                                         bool showMatches );
vector<int> analysis_findBranches( Iobj *obj, Icont *contRoot, float maxDistBranch,
                                   float zScale );
string analysis_outputVolumeWithinXAnalysis( int objToAnalyze,
                                             int objInIdx, int objOut1Idx, int objOut2Idx,
                                             float distThres,
                                             int  numRandomPts, 
                                             bool useMbr, bool addPtsNewObj );
string analysis_closestDistanceSurfsTwoObjects( int objAIdx, int objBIdx, bool addPtsNewObj, float maxDist );

//-------------------------------
//## DEFORMATION GRID FUNCTIONS:

vector<Ivector> def_generateVectorsFromRefPairs( Iobj *obj, bool allSlices, int sliceNum=-1 );
bool def_generateDefGrid( int objIdx, int colsX, int rowsY, float power, bool applyCumulativeTransforms );
string def_outputDefPointsAnalysis( int objIdxToUse, bool convertUnits );
bool def_deformObjectsUsingDefGrid( int objIdx, bool inverse, bool duplicate );
bool def_createDefGridForSlice( int objIdxToUse, int colsX, int rowsY, int zVal,
                                bool displayGrid, bool displayInDiffObj,
                                int objIdxForDisplay, double powerUsed );


//-------------------------------
//## IMAGE FUNCTIONS:

inline unsigned char &pix(unsigned char *data, int x, int y);
inline int isPixEqual(unsigned char val, unsigned char *data, int x, int y,
                      int maxX, int maxY, int outOfBoundsReturnsVal=0);

float img_fractionOverlap( Icont *cont1, Icont *cont2 );
int  img_noiseReduction( unsigned char ON, unsigned char OFF, int minNeighborsOn,
                         unsigned char *image, int w,int h, int rounds,
                         unsigned char *temp );
int  img_createContourAroundEdge( Icont *cont, unsigned char ON,
                                  int startX, int startY, int z,
                                  unsigned char *image, int w, int h, int maxPts=1000 );
void img_scaleSlice( unsigned char **slice, int w, int h );
int  img_changeSliceData( unsigned char **slice, unsigned char *data,
                          int w, int h, bool scale, bool redraw );
int  img_setAll( unsigned char *data, unsigned char VAL, int w, int h );
long img_countOccurances( unsigned char *data, unsigned char VAL, int w, int h );
int  turnOnRangeOfValues( unsigned char *data, unsigned char ON, unsigned char OFF,
                          int minVal, int maxVal, bool skipPixelsAlreadyOff,
                          unsigned char **slice, int w, int h );
void img_changeValuesInsideContour( Icont *cont, unsigned char ON, unsigned char OFF,
                                    bool toggle, unsigned char *image, int w,int h );
void img_changeValuesInsideObject( Iobj *obj, unsigned char ON, unsigned char OFF,
                                   bool toggle, unsigned char *image, int w,int h,int z);
long img_contoursFromImagePoints( vector<IcontPtr> &contSegs,
                                  unsigned char *data, unsigned char **imdata,
                                  int xsize, int ysize, int z, 
                                  unsigned char testmask, int diagonal,
                                  float threshold, int reverse, long maxConts=10000 );


//############################################################
